struct VertexInput {
  @location(0) position: vec3<f32>,
}

struct VertexOut {
  @builtin(position) position: vec4<f32>,
}

struct VertexUniform {
  view_projection: mat4x4<f32>,
};
@group(0) @binding(0)
var<uniform> camera: VertexUniform;

struct ColorUniform {
  fill: vec4<f32>,
}
@group(1) @binding(0)
var<uniform> colors: ColorUniform;

@vertex
fn vs_main(model: VertexInput) -> VertexOut {
    var out: VertexOut;

    out.position = camera.view_projection * vec4<f32>(model.position, 1.0);

    return out;
}

@fragment
fn fs_main(in: VertexOut) -> @location(0) vec4<f32> {
    // TODO we should shade these based on normal to view.
    return colors.fill;
}
