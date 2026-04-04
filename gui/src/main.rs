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

use bevy::{
    pbr::wireframe::WireframePlugin,
    prelude::*,
    winit::{EventLoopProxyWrapper, WinitSettings, WinitUserEvent},
};
use bevy_egui::{EguiContexts, EguiPlugin, EguiPrimaryContextPass};
use egui::{Color32, Mesh, Painter, RichText, Sense, StrokeKind, TextEdit, emath::TSTransform};
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
    visualize2d::{ViewState2d, build_fill_mesh_from_polygon, paint_linestring, paint_polygon},
    visualize3d::{ViewState3d, setup_3d, spawn_meshes, update_projection},
};

mod visualize2d;
mod visualize3d;

fn main() {
    let mut app = App::new();
    app.add_plugins(DefaultPlugins.set(WindowPlugin {
        primary_window: Some(Window {
            // You may want this set to `true` if you need virtual keyboard work in mobile browsers.
            prevent_default_event_handling: false,
            title: String::from("Command CAD"),
            ..default()
        }),
        ..default()
    }))
    .insert_resource(WinitSettings::desktop_app())
    .add_plugins(EguiPlugin::default())
    .add_plugins(WireframePlugin::default())
    .add_systems(Startup, (setup, setup_3d))
    .add_systems(Update, (spawn_meshes, check_job.before(spawn_meshes), update_projection))
    .add_systems(EguiPrimaryContextPass, render_ui);
    app.run();
}

fn setup(
    mut commands: Commands,
    event_loop_proxy: Res<EventLoopProxyWrapper>,
) {
    // Only update when there are events worth updating for.
    commands.insert_resource(WinitSettings::desktop_app());

    let (expression_tx, expression_rx) = mpsc::channel();
    std::thread::spawn(|| job_executor(expression_rx));

    let (response, _) = oneshot::channel();
    expression_tx
        .send(Job {
            expression: "std.import(path = \"examples/modeling/mesh.ccm\")::get(i = 0u)".into(),
            response,
            shutdown_signal: Arc::new(AtomicBool::new(false)),
        })
        .unwrap();

    let (file_updates_tx, file_updates_rx) = mpsc::channel();
    let file_updates_rx = Mutex::new(file_updates_rx);

    let event_loop_proxy = event_loop_proxy.clone();
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
        event_loop_proxy
            .clone()
            .send_event(WinitUserEvent::WakeUp)
            .ok();
    });

    commands.insert_resource(ExpressionField {
        expression: String::new(),
    });

    commands.insert_resource(JobBridge {
        expression_tx,
        active_job: None,
        last_result: None,
        watched_files: HashSet::new(),
        file_watcher,
        file_updates_rx,
    });

    let mut view_state = ViewState {
        zoom: 10.0,
        offset: Vec2::ZERO,  
    };
    view_state.set_pixels_per_meter(100.0);
    commands.insert_resource(view_state);
    commands.insert_resource(ViewState2d);
    commands.insert_resource(ViewState3d);
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

struct Job {
    expression: String,
    response: oneshot::Sender<RuntimeOutput>,
    shutdown_signal: Arc<AtomicBool>,
}

impl std::fmt::Debug for Job {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RuntimeJob")
            .field("expression", &self.expression)
            .field("shutdown_signal", &self.shutdown_signal)
            .finish()
    }
}

struct PendingJob {
    shutdown_signal: Arc<AtomicBool>,
    response: Mutex<oneshot::Receiver<RuntimeOutput>>,
}

enum JobOutput {
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

enum JobError {
    Execution(interpreter::Error),
    Parse(String),
    Compile(String),
}

impl Display for JobError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JobError::Execution(error) => error.fmt(f),
            JobError::Parse(error) => error.fmt(f),
            JobError::Compile(error) => error.fmt(f),
        }
    }
}

struct RuntimeOutput {
    result: Result<JobOutput, JobError>,
    files_to_watch: HashSet<Arc<PathBuf>>,
}

fn job_executor(expression_rx: mpsc::Receiver<Job>) {
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
            .map_err(|error| JobError::Parse(format!("Failed to parse input: {error:?}")))
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
            .map_err(|error| JobError::Compile(format!("Failed to compile: {error:?}")))
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
            Ok(Value::LineString(line_string)) => Ok(JobOutput::LineString(line_string)),
            Ok(Value::Polygon(polygon)) => {
                let mesh = build_fill_mesh_from_polygon(&polygon.0);

                Ok(JobOutput::Polygon { polygon, mesh })
            }
            Ok(Value::PolygonSet(polygon_set)) => {
                let meshes = polygon_set
                    .0
                    .iter()
                    .map(build_fill_mesh_from_polygon)
                    .collect();

                Ok(JobOutput::PolygonSet {
                    polygon_set,
                    meshes,
                })
            }
            Ok(Value::ManifoldMesh3D(manifold)) => Ok(JobOutput::ManifoldMesh(ManifoldMeshState {
                manifold,
                uploaded_to_gpu: false,
            })),
            Ok(value) => {
                let mut text = String::new();
                value.format(&context, &mut text, Style::Default, None).ok();

                Ok(JobOutput::TextValue(text))
            }
            Err(error) => Err(JobError::Execution(error)),
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
    while let Ok(job) = expression_rx.recv() {
        let result = run_expression(job.shutdown_signal, job.expression);
        job.response.send(result).ok();
        let notice_me = 0;
        // TODO wake up Bevy to redraw the scene.
    }
}

#[derive(Resource)]
struct ExpressionField {
    expression: String,
}

#[derive(Resource)]
struct JobBridge {
    expression_tx: mpsc::Sender<Job>,
    active_job: Option<PendingJob>,
    last_result: Option<Result<JobOutput, JobError>>,
    watched_files: HashSet<Arc<PathBuf>>,
    file_watcher: Result<RecommendedWatcher, notify::Error>,
    file_updates_rx: Mutex<mpsc::Receiver<Result<notify::Event, notify::Error>>>,
}

impl JobBridge {
    fn spawn_job(&mut self, job: &str) {
        if let Some(pending_job) = self.active_job.take() {
            pending_job
                .shutdown_signal
                .store(true, std::sync::atomic::Ordering::Relaxed);
        }

        let shutdown_signal = Arc::new(AtomicBool::new(false));

        let (response_tx, response_rx) = oneshot::channel();

        let pending_job = PendingJob {
            shutdown_signal: shutdown_signal.clone(),
            response: Mutex::new(response_rx),
        };

        let job = Job {
            expression: job.into(),
            response: response_tx,
            shutdown_signal,
        };

        self.expression_tx
            .send(job)
            .expect("Runtime thread terminated early");
        self.active_job = Some(pending_job);
    }

    fn check_if_watched_files_changed(&mut self) -> bool {
        match self.file_updates_rx.get_mut().unwrap().try_recv() {
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
}

fn check_job(mut command_cad: ResMut<JobBridge>) {
    let command_cad = &mut *command_cad;
    if let Some(active_job) = &mut command_cad.active_job {
        // This could fail by the thread being closed, but that shouldn't happen and won't
        // cause us to panic.
        if let Ok(output) = active_job.response.get_mut().unwrap().try_recv() {
            command_cad.last_result = Some(output.result);
            command_cad.active_job = None;

            // Collect a list of files to watch.
            if let Ok(watcher) = &mut command_cad.file_watcher {
                // We used to be very picky, only adding and removing files from the wather as
                // needed. It was eventually discovered that sometimes watchers that were not
                // removed (and were not supposed to be removed) would fail to trigger on later
                // edits of files, so now we just remove and re-add everything to make sure
                // we're in a good known state.

                let mut paths = watcher.paths_mut();
                for path in command_cad.watched_files.iter() {
                    // TODO log errors and success here.
                    paths.remove(path).ok();
                }

                for path in output.files_to_watch.iter() {
                    // TODO log errors and success here.
                    paths.add(path, notify::RecursiveMode::NonRecursive).ok();
                }
            }

            command_cad.watched_files = output.files_to_watch;
        }
    }
}
fn render_ui(
    mut job_bridge: ResMut<JobBridge>,
    mut view_state: ResMut<ViewState>,
    mut view_state_2d: ResMut<ViewState2d>,
    mut expression: ResMut<ExpressionField>,
    mut contexts: EguiContexts,
) -> Result {
    let ctx = contexts.ctx_mut()?;

    egui::TopBottomPanel::top("main_interface").show(ctx, |ui| {
        let expression_editor = TextEdit::multiline(&mut expression.expression)
            .code_editor()
            .desired_width(f32::INFINITY)
            .hint_text("expression");
        if ui.add(expression_editor).changed()
            || job_bridge.check_if_watched_files_changed()
        {
            job_bridge.spawn_job(expression.expression.as_str());
        }

        let mut draw_area = ctx.viewport_rect();
        draw_area.max.y -= 110.0;

        ui.horizontal(|ui| {
            if job_bridge.active_job.is_some() {
                ui.label(RichText::new("Working...").color(Color32::YELLOW));
            } else {
                ui.label(RichText::new("Ready").color(Color32::GREEN));
            }

            view_state_2d.draw_interface(&mut view_state, ui, &job_bridge.last_result, draw_area);
        });

        if let Err(error) = &job_bridge.file_watcher {
            ui.label(
                RichText::new(format!("Failed to setup file watching: {error}\nOutput will not update automatically when files are modified"))
                    .color(Color32::YELLOW),
            );
        }

        // TODO Add some kind of scale legend.

    });

    fn draw_thing(ctx: &mut egui::Context, draw: impl FnOnce(&mut egui::Ui, egui::Rect)) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let draw_area = ui.available_rect_before_wrap();
            draw(ui, draw_area)
        });
    }
        
    ctx.input(|state| {
        view_state.zoom += state.smooth_scroll_delta.y;
        view_state.zoom = view_state.zoom.max(0.0);
    });

    match &mut job_bridge.last_result {
        None => {}
        Some(Ok(JobOutput::TextValue(text))) => {
            draw_thing(ctx, |ui, _draw_area| {
                egui::ScrollArea::vertical().show(ui, |ui| {
                    ui.label(text.as_str());
                });
            });
        }
        Some(Ok(JobOutput::LineString(line_string))) => {
            draw_thing(ctx, |ui, draw_area| {
                let painter = view_state.prep_for_painting(ui);
                let pixels_per_meter = view_state.pixels_per_meter();
                let center_offset = draw_area.center().to_vec2();
                let view_offset = egui::Vec2::new(view_state.offset.x, view_state.offset.y);

                let transform = TSTransform {
                    scaling: pixels_per_meter,
                    translation: center_offset + view_offset * pixels_per_meter,
                };
                painter.add(paint_linestring(
                    &transform,
                    StrokeKind::Middle,
                    &line_string.0,
                ));
            });
        }
        Some(Ok(JobOutput::Polygon { polygon, mesh })) => {
            draw_thing(ctx, |ui, draw_area| {
                let painter = view_state.prep_for_painting(ui);
                paint_polygon(
                    &painter,
                    draw_area,
                    &view_state,
                    &polygon.0,
                    mesh.clone(),
                );
            });
        }
        Some(Ok(JobOutput::PolygonSet {
            polygon_set,
            meshes,
        })) => {
            draw_thing(ctx, |ui, draw_area| {
                let painter = view_state.prep_for_painting(ui);
                for (polygon, mesh) in polygon_set.0.iter().zip(meshes.iter()) {
                    paint_polygon(
                        &painter,
                        draw_area,
                        &view_state,
                        polygon,
                        mesh.clone(),
                    );
                }
            });
        }
        Some(Ok(JobOutput::ManifoldMesh(_manifold_state))) => {
            // Rendering for this is done in a different system.
        }
        Some(Err(error)) => {
            draw_thing(ctx, |ui, _draw_area| {
                ui.label(RichText::new(format!("{error}")).color(Color32::RED));
            });
        }
    }

    Ok(())
}

#[derive(Resource)]
struct ViewState {
    zoom: f32,
    offset: Vec2
}

impl ViewState {
    // Percentage of scale per scale factor unit.
    const SCALE_FACTOR: f32 = 1.01;

    fn prep_for_painting(&mut self, ui: &mut egui::Ui) -> Painter {
        let draw_area = ui.available_rect_before_wrap();
        let response = ui.allocate_rect(draw_area, Sense::DRAG);
        let delta = response.drag_delta() / self.pixels_per_meter();
        self.offset += Vec2::new(delta.x, delta.y);

        Painter::new(ui.ctx().clone(), ui.layer_id(), draw_area)
    }

    pub fn pixels_per_meter(&self) -> f32 {
        Self::SCALE_FACTOR.powf(self.zoom)
    }

    pub fn set_pixels_per_meter(&mut self, pixels_per_meter: f32) {
        self.zoom = pixels_per_meter.log(Self::SCALE_FACTOR);
    }
}
