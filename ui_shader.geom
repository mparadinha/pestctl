#version 330 core

layout (points) in;
layout (triangle_strip, max_vertices = 6) out;

uniform vec2 screen_size; // in pixels

in VS_Out {
    vec2 bottom_left_pos;
    vec2 top_right_pos;
    vec2 bottom_left_uv;
    vec2 top_right_uv;
    vec4 top_color;
    vec4 bottom_color;
    float corner_roundness;
    float border_thickness;
    vec2 rect_size;
    vec2 clip_rect_min;
    vec2 clip_rect_max;
    uint which_font;

    vec2 rect_pixel_center;
} gs_in[];

out GS_Out {
    vec2 uv;
    vec4 color;
    vec2 quad_coords;
    float quad_size_ratio;
    float corner_roundness;
    float border_thickness;
    vec2 rect_size;
    vec2 clip_rect_min;
    vec2 clip_rect_max;
    flat uint which_font;

    flat vec2 rect_pixel_center;
} gs_out;

void main() {
    vec4 bottom_left_pos  = vec4(gs_in[0].bottom_left_pos, 0, 1);
    vec2 bottom_left_uv   = gs_in[0].bottom_left_uv;
    vec4 top_right_pos    = vec4(gs_in[0].top_right_pos, 0, 1);
    vec2 top_right_uv     = gs_in[0].top_right_uv;
    vec4 bottom_right_pos = vec4(top_right_pos.x, bottom_left_pos.y, 0, 1);
    vec2 bottom_right_uv  = vec2(top_right_uv.x,  bottom_left_uv.y);
    vec4 top_left_pos     = vec4(bottom_left_pos.x, top_right_pos.y, 0, 1);
    vec2 top_left_uv      = vec2(bottom_left_uv.x,  top_right_uv.y);

    vec2 quad_size = gs_in[0].top_right_pos - gs_in[0].bottom_left_pos;

    // some things are the same for all verts of the quad
    gs_out.corner_roundness = gs_in[0].corner_roundness;
    gs_out.border_thickness = gs_in[0].border_thickness;
    gs_out.rect_size = gs_in[0].rect_size;
    gs_out.quad_size_ratio = (quad_size.x / quad_size.y) * (screen_size.x / screen_size.y);
    gs_out.clip_rect_min = gs_in[0].clip_rect_min;
    gs_out.clip_rect_max = gs_in[0].clip_rect_max;
    gs_out.which_font = gs_in[0].which_font;

    gs_out.rect_pixel_center = gs_in[0].rect_pixel_center;

    gl_Position        = bottom_left_pos;
    gs_out.uv          = bottom_left_uv;
    gs_out.color       = gs_in[0].bottom_color;
    gs_out.quad_coords = vec2(0, 0);
    EmitVertex();
    gl_Position        = bottom_right_pos;
    gs_out.uv          = bottom_right_uv;
    gs_out.color       = gs_in[0].bottom_color;
    gs_out.quad_coords = vec2(1, 0);
    EmitVertex();
    gl_Position        = top_right_pos;
    gs_out.uv          = top_right_uv;
    gs_out.color       = gs_in[0].top_color;
    gs_out.quad_coords = vec2(1, 1);
    EmitVertex();
    EndPrimitive();

    gl_Position        = bottom_left_pos;
    gs_out.uv          = bottom_left_uv;
    gs_out.color       = gs_in[0].bottom_color;
    gs_out.quad_coords = vec2(0, 0);
    EmitVertex();
    gl_Position        = top_right_pos;
    gs_out.uv          = top_right_uv;
    gs_out.color       = gs_in[0].top_color;
    gs_out.quad_coords = vec2(1, 1);
    EmitVertex();
    gl_Position        = top_left_pos;
    gs_out.uv          = top_left_uv;
    gs_out.color       = gs_in[0].top_color;
    gs_out.quad_coords = vec2(0, 1);
    EmitVertex();
    EndPrimitive();
}
