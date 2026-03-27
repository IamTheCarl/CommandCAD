/*
 * Copyright 2026 James Carl
 * AGPL-3.0-only or AGPL-3.0-or-later
 *
 * This file is part of Command Cad.
 *
 * Command CAD is free software: you can redistribute it and/or modify it under the terms of
 * the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <https://www.gnu.org/licenses/>.
 */
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    path::{Path, PathBuf},
    sync::{Arc, Mutex, atomic::AtomicBool, mpsc},
    time::Duration,
};

use eframe::egui;
use egui::{Color32, Mesh, Painter, RichText, Sense, StrokeKind, TextEdit};
use interpreter::{
    ExecutionContext, FsStore, LogMessage, RuntimeLog, SourceReference, StackScope, StackTrace,
    Store, build_prelude, compile, execute_expression, new_parser,
    values::{
        BuiltinCallableDatabase, LineString, Object, Polygon, PolygonSet, Style, Value,
        manifold_mesh::ManifoldMesh3D,
    },
};
use notify::{EventKind, RecommendedWatcher, Watcher, recommended_watcher};
use tempfile::TempDir;

use crate::{
    visualize2d::{ViewState2D, build_fill_mesh_from_polygon, paint_linestring, paint_polygon},
    visualize3d::ViewState3D,
};

mod visualize2d;
mod visualize3d;

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
    Polygon {
        polygon: Polygon,
        mesh: Arc<Mesh>,
    },
    PolygonSet {
        polygon_set: PolygonSet,
        meshes: Vec<Arc<Mesh>>,
    },
    ManifoldMesh(ManifoldMeshState),
}

struct ManifoldMeshState {
    manifold: ManifoldMesh3D,
    uploaded_to_gpu: bool,
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
                };
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
                };
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
            Ok(Value::Polygon(polygon)) => {
                let mesh = build_fill_mesh_from_polygon(&polygon.0);

                Ok(RuntimeValue::Polygon { polygon, mesh })
            }
            Ok(Value::PolygonSet(polygon_set)) => {
                let meshes = polygon_set
                    .0
                    .iter()
                    .map(build_fill_mesh_from_polygon)
                    .collect();

                Ok(RuntimeValue::PolygonSet {
                    polygon_set,
                    meshes,
                })
            }
            Ok(Value::ManifoldMesh3D(manifold)) => {
                Ok(RuntimeValue::ManifoldMesh(ManifoldMeshState {
                    manifold,
                    uploaded_to_gpu: false,
                }))
            }
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
    file_watcher: Result<RecommendedWatcher, notify::Error>,
    file_updates_rx: mpsc::Receiver<Result<notify::Event, notify::Error>>,
    view_state: ViewState,
}

impl CommandCAD {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let (mut expression_tx, expression_rx) = mpsc::channel();
        std::thread::spawn(|| runtime(expression_rx));

        let (file_updates_tx, file_updates_rx) = mpsc::channel();

        let gui_ctx = cc.egui_ctx.clone();
        let file_watcher = recommended_watcher(move |event: Result<notify::Event, _>| {
            // Notify and refresh the GUI whenever there's an update to one of the project files.

            // This is a really horrible hack, but I have no idea how to better fix it and it seems
            // to be what the "experts" are doing as well (I looked at a higher-level library for
            // watching for file changes). Basically, sometimes we get this notification before the
            // file is available on disk. We need to check and possibly wait to see if the file is
            // actually ready.
            if let Ok(event) = &event
                && matches!(event.kind, EventKind::Create(_) | EventKind::Modify(_))
            {
                while !event.paths.iter().all(|path| path.exists()) {
                    std::thread::sleep(Duration::from_millis(10));
                }
            }

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
            view_state: ViewState {
                pixels_per_meter: 100.0,
                view_state_2d: ViewState2D::default(),
                view_state_3d: ViewState3D::new(cc),
            },
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
            Ok(Ok(event)) => matches!(event.kind, EventKind::Modify(_) | EventKind::Create(_)),
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
            let expression_editor = TextEdit::multiline(&mut self.expression)
                .code_editor()
                .desired_width(f32::INFINITY)
                .hint_text("expression");
            if ui.add(expression_editor).changed()
                || self.check_if_watched_files_changed()
            {
                self.spawn_job(ctx);
            }
            let mut draw_area = ui.available_rect_before_wrap();
            draw_area.min.y += 30.0; // We don't know how tall the status bar is at this time, so I
                                     // just assume 30 and that seems to be working well enough.
            ui.horizontal(|ui| {
                if self.active_job.is_some() {
                    ui.label(RichText::new("Working...").color(Color32::YELLOW));
                } else {
                    ui.label(RichText::new("Ready").color(Color32::GREEN));
                }

                self.view_state.view_state_2d.draw_interface(&mut self.view_state.pixels_per_meter, ui, &self.last_result, draw_area);
            });

            if let Err(error) = &self.file_watcher {
                ui.label(
                    RichText::new(format!("Failed to setup file watching: {error}\nOutput will not update automatically when files are modified"))
                        .color(Color32::YELLOW),
                );
            }

            // Update the job status.
            self.check_job();

            // TODO Add some kind of scale legend.

            match &mut self.last_result {
                None => {}
                Some(Ok(RuntimeValue::TextValue(text))) => {
                    egui::ScrollArea::vertical().show(ui, |ui| {
                        ui.label(text.as_str());
                    });
                }
                Some(Ok(RuntimeValue::LineString(line_string))) => {
                    let painter = self.view_state.prep_for_painting(ui);
                    painter.add(paint_linestring(draw_area, &self.view_state, StrokeKind::Middle, &line_string.0));
                }
                Some(Ok(RuntimeValue::Polygon { polygon, mesh})) => {
                    let painter = self.view_state.prep_for_painting(ui);
                    paint_polygon(&painter, draw_area, &self.view_state, &polygon.0, mesh.clone());
                }
                Some(Ok(RuntimeValue::PolygonSet { polygon_set, meshes })) => {
                    let painter = self.view_state.prep_for_painting(ui);
                    for (polygon, mesh) in polygon_set.0.iter().zip(meshes.iter()) {
                        paint_polygon(&painter, draw_area, &self.view_state, polygon, mesh.clone());
                    }
                }
                Some(Ok(RuntimeValue::ManifoldMesh(manifold_state))) => {
                    let painter = self.view_state.prep_for_painting(ui);
                    self.view_state.view_state_3d.paint(&self.view_state, manifold_state, painter);
                }
                Some(Err(error)) => {
                    ui.label(RichText::new(format!("{error}")).color(Color32::RED));
                }
            }
        });
    }
}

struct ViewState {
    pixels_per_meter: f32,
    view_state_2d: ViewState2D,
    view_state_3d: ViewState3D,
}

impl ViewState {
    fn prep_for_painting(&mut self, ui: &mut egui::Ui) -> Painter {
        let draw_area = ui.available_rect_before_wrap();
        let response = ui.allocate_rect(draw_area, Sense::DRAG);
        ui.input(|state| {
            self.pixels_per_meter += state.smooth_scroll_delta.y;
            self.pixels_per_meter = self.pixels_per_meter.max(0.0);
        });
        self.view_state_2d.update(ui, &response);
        self.view_state_3d.update(ui, draw_area);

        Painter::new(ui.ctx().clone(), ui.layer_id(), draw_area)
    }
}
