use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    path::{Path, PathBuf},
    sync::{atomic::AtomicBool, mpsc, Arc, Mutex},
    time::Duration,
};

use eframe::egui;
use egui::{
    epaint::{ColorMode, PathShape, PathStroke},
    Color32, Pos2, RichText, Shape, StrokeKind,
};
use interpreter::{
    build_prelude, compile, execute_expression, new_parser,
    values::{BuiltinCallableDatabase, LineString, Object, Style, Value},
    ExecutionContext, FsStore, ImString, LogMessage, Parser, RuntimeLog, SourceReference,
    StackScope, StackTrace, Store,
};
use notify::{recommended_watcher, EventKind, RecommendedWatcher, Watcher};
use notify_debouncer_full::{new_debouncer, Debouncer, RecommendedCache};
use tempfile::TempDir;

fn main() {
    let native_options = eframe::NativeOptions::default();
    if let Err(error) = eframe::run_native(
        "Command CAD",
        native_options,
        Box::new(|cc| Ok(Box::new(CommandCAD::new(cc)))),
    ) {
        eprintln!("Failed to run application: {error}");
    }
}

#[derive(Debug)]
struct GuiLogger;

impl RuntimeLog for GuiLogger {
    fn push_message(&self, message: LogMessage) {
        // TODO
    }

    fn collect_syntax_errors<'t>(
        &self,
        input: &str,
        tree: &'t interpreter::compile::RootTree,
        file: &'t Arc<PathBuf>,
        span: SourceReference,
    ) {
        // TODO
    }
}

struct Runtime {
    parser: Parser,
    database: BuiltinCallableDatabase,
    prelude: HashMap<ImString, Value>,
    repl_file: Arc<PathBuf>,
}

struct RuntimeJob {
    expression: String,
    egui_context: egui::Context,
    response: oneshot::Sender<RuntimeOutput>,
    shutdown_signal: Arc<AtomicBool>,
}

struct PendingJob {
    shutdown_signal: Arc<AtomicBool>,
    response: oneshot::Receiver<RuntimeOutput>,
}

enum RuntimeValue {
    TextValue(String),
    LineString(LineString),
    // Polygon(Polygon),
    // PolygonSet(PolygonSet),
    // ManifoldMesh(ManifoldMesh3D),
}

enum RuntimeError {
    Execution(interpreter::Error),
    Parse(String),
    Compile(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Execution(error) => error.fmt(f),
            RuntimeError::Parse(error) => error.fmt(f),
            RuntimeError::Compile(error) => error.fmt(f),
        }
    }
}

struct RuntimeOutput {
    result: Result<RuntimeValue, RuntimeError>,
    files_to_watch: HashSet<Arc<PathBuf>>,
}

fn runtime(expression_rx: mpsc::Receiver<RuntimeJob>) {
    let database = BuiltinCallableDatabase::new();
    let prelude = build_prelude(&database);
    let stack_top = StackScope::top(&prelude);

    let mut parser = new_parser();
    let repl_file = Arc::new(PathBuf::from("repl.ccm"));

    // TODO we should prefer using `.ccad` in the local directory.
    // Also we shouldn't be unwrapping on that.
    let store_directory = TempDir::new().unwrap();
    let store = Store::FsStore(FsStore::new(store_directory.path()));

    let mut run_expression = |shutdown_signal: Arc<AtomicBool>, input: String| -> RuntimeOutput {
        let files = Mutex::new(HashMap::new());
        let tree = match parser
            .parse(&input, None)
            .map_err(|error| RuntimeError::Parse(format!("Failed to parse input: {error:?}")))
        {
            Ok(tree) => tree,
            Err(error) => {
                return RuntimeOutput {
                    result: Err(error),
                    files_to_watch: HashSet::new(),
                }
            }
        };
        let root = match compile(&repl_file, &input, &tree)
            .map_err(|error| RuntimeError::Compile(format!("Failed to compile: {error:?}")))
        {
            Ok(root) => root,
            Err(error) => {
                return RuntimeOutput {
                    result: Err(error),
                    files_to_watch: HashSet::new(),
                }
            }
        };

        let log = GuiLogger;

        let context = ExecutionContext {
            shutdown_singal: &shutdown_signal,
            log: &log as &dyn RuntimeLog,
            stack_trace: &StackTrace::top(root.reference.clone()),
            stack: &stack_top,
            database: &database,
            store: &store,
            file_cache: &files,
            working_directory: Path::new("."),
            import_limit: 100,
        };

        let expression_result = execute_expression(&context, &root);

        let result = match expression_result {
            Ok(Value::LineString(line_string)) => Ok(RuntimeValue::LineString(line_string)),
            Ok(value) => {
                let mut text = String::new();
                value.format(&context, &mut text, Style::Default, None).ok();

                Ok(RuntimeValue::TextValue(text))
            }
            Err(error) => Err(RuntimeError::Execution(error)),
        };

        // TODO we can also use this for better error message formatting.
        let files = files.into_inner().expect("File hashmap was poisoned");
        let files_to_watch: HashSet<Arc<PathBuf>> = files.keys().cloned().collect();

        RuntimeOutput {
            result,
            files_to_watch,
        }
    };

    // An error indicates that there are no more senders and that we should shutdown.
    while let Ok(command) = expression_rx.recv() {
        let result = run_expression(command.shutdown_signal, command.expression);
        command.response.send(result).ok();
        command.egui_context.request_repaint();
    }
}

struct CommandCAD {
    expression: String,
    expression_tx: mpsc::Sender<RuntimeJob>,
    active_job: Option<PendingJob>,
    last_result: Option<Result<RuntimeValue, RuntimeError>>,
    watched_files: HashSet<Arc<PathBuf>>,
    file_watcher: Result<Debouncer<RecommendedWatcher, RecommendedCache>, notify::Error>,
    file_updates_rx: mpsc::Receiver<notify_debouncer_full::DebounceEventResult>,
}

impl CommandCAD {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let (expression_tx, expression_rx) = mpsc::channel();
        std::thread::spawn(|| runtime(expression_rx));

        let (file_updates_tx, file_updates_rx) = mpsc::channel();

        let gui_ctx = cc.egui_ctx.clone();
        let file_watcher = new_debouncer(Duration::from_millis(10), None, move |event| {
            // Notify and refresh the GUI whenever there's an update to one of the project files.
            file_updates_tx.send(event).ok();
            gui_ctx.request_repaint();
        });

        Self {
            expression: String::new(),
            expression_tx,
            active_job: None,
            last_result: None,
            watched_files: HashSet::new(),
            file_watcher,
            file_updates_rx,
        }
    }

    fn spawn_job(&mut self, ctx: &egui::Context) {
        if let Some(pending_job) = self.active_job.take() {
            pending_job
                .shutdown_signal
                .store(true, std::sync::atomic::Ordering::Relaxed);
        }

        let shutdown_signal = Arc::new(AtomicBool::new(false));

        let (response_tx, response_rx) = oneshot::channel();

        let pending_job = PendingJob {
            shutdown_signal: shutdown_signal.clone(),
            response: response_rx,
        };

        let runtime_job = RuntimeJob {
            expression: self.expression.clone(),
            egui_context: ctx.clone(),
            response: response_tx,
            shutdown_signal,
        };

        self.expression_tx
            .send(runtime_job)
            .expect("Runtime thread terminated early");
        self.active_job = Some(pending_job);
    }

    fn check_if_watched_files_changed(&mut self) -> bool {
        match self.file_updates_rx.try_recv() {
            Ok(Ok(event)) => matches!(event.kind, EventKind::Modify(_) | EventKind::Remove(_)),
            // TODO log that or something.
            Ok(Err(error)) => {
                // TODO this can be logged better.
                let notice_me = 0;
                eprintln!("{error}");
                false
            }
            Err(_) => false,
        }
    }

    fn check_job(&mut self) {
        if let Some(active_job) = &self.active_job {
            // This could fail by the thread being closed, but that shouldn't happen and won't
            // cause us to panic.
            if let Ok(output) = active_job.response.try_recv() {
                self.last_result = Some(output.result);
                self.active_job = None;

                // Collect a list of files to watch.
                if let Ok(watcher) = &mut self.file_watcher {
                    // We used to be very picky, only adding and removing files from the wather as
                    // needed. It was eventually discovered that sometimes watchers that were not
                    // removed (and were not supposed to be removed) would fail to trigger on later
                    // edits of files, so now we just remove and re-add everything to make sure
                    // we're in a good known state.

                    let mut paths = watcher.paths_mut();
                    for path in self.watched_files.iter() {
                        // TODO log errors and success here.
                        paths.remove(path).ok();
                    }

                    for path in output.files_to_watch.iter() {
                        // TODO log errors and success here.
                        paths.add(path, notify::RecursiveMode::NonRecursive).ok();
                    }
                }

                self.watched_files = output.files_to_watch;
            }
        }
    }
}

impl eframe::App for CommandCAD {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("Expression: ");
                if ui.text_edit_singleline(&mut self.expression).changed()
                    || self.check_if_watched_files_changed()
                {
                    self.spawn_job(ctx);
                }

                if self.active_job.is_some() {
                    ui.label(RichText::new("Working...").color(Color32::YELLOW));
                } else {
                    ui.label(RichText::new("Ready").color(Color32::GREEN));
                }
            });

            if let Err(error) = &self.file_watcher {
                ui.label(
                    RichText::new(format!("Failed to setup file watching: {error}\nOutput will not update automatically when files are modified"))
                        .color(Color32::YELLOW),
                );
            }

            // Update the job status.
            self.check_job();

            match &self.last_result {
                None => {}
                Some(Ok(RuntimeValue::TextValue(text))) => {
                    ui.label(text);
                }
                Some(Ok(RuntimeValue::LineString(line_string))) => {
                    // TODO this needs a way to scale.
                    let painter = ui.painter();
                    let line_string = &line_string.0;
                    let path = PathShape { points: line_string.coords().map(|coord| Pos2::new(coord.x as f32, coord.y as f32)).collect(), closed: line_string.is_closed(), fill: Color32::TRANSPARENT, stroke: PathStroke { width: 2.0, color: ColorMode::TRANSPARENT, kind: StrokeKind::Middle } };
                    painter.add(Shape::Path(path));
                }
                Some(Err(error)) => {
                    ui.label(RichText::new(format!("{error}")).color(Color32::RED));
                }
            }
        });
    }
}
