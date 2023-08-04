#version 330 core

layout (points) in;
layout (triangle_strip, max_vertices = 6) out;

uniform vec2 screen_size;

in VS_Out {
    vec2 bottom_left_pos;
    vec2 top_right_pos;
    vec2 bottom_left_uv;
    vec2 top_right_uv;
    vec4 top_color;
    vec4 bottom_color;
    float corner_radius;
    float border_thickness;
    vec2 clip_rect_min;
    vec2 clip_rect_max;
    uint which_font;
} gs_in[];

out GS_Out {
    vec2 uv;
    vec4 color;
    flat vec2 rect_size;
    flat vec2 rect_center;
    flat float corner_radius;
    flat float border_thickness;
    flat vec2 clip_rect_min;
    flat vec2 clip_rect_max;
    flat uint which_font;
} gs_out;

void main() {
    vec2 quad_size = gs_in[0].top_right_pos - gs_in[0].bottom_left_pos;
    vec2 quad_btm_left = gs_in[0].bottom_left_pos;
    vec2 quad_btm_right = quad_btm_left + vec2(quad_size.x, 0);
    vec2 quad_top_left = quad_btm_left + vec2(0, quad_size.y);
    vec2 quad_top_right = gs_in[0].top_right_pos;

    vec4 btm_left_coord = vec4((quad_btm_left / screen_size) * 2 - vec2(1), 0, 1);
    vec4 btm_right_coord = vec4((quad_btm_right / screen_size) * 2 - vec2(1), 0, 1);
    vec4 top_left_coord = vec4((quad_top_left / screen_size) * 2 - vec2(1), 0, 1);
    vec4 top_right_coord = vec4((quad_top_right / screen_size) * 2 - vec2(1), 0, 1);

    vec2 uv_range = gs_in[0].top_right_uv - gs_in[0].bottom_left_uv;
    vec2 uv_btm_right = gs_in[0].bottom_left_uv + vec2(uv_range.x, 0);
    vec2 uv_top_left = gs_in[0].bottom_left_uv + vec2(0, uv_range.y);

    // some things are the same for all verts of the quad
    gs_out.rect_size = quad_size;
    gs_out.rect_center = quad_btm_left + quad_size / 2;
    gs_out.corner_radius = gs_in[0].corner_radius;
    gs_out.border_thickness = gs_in[0].border_thickness;
    gs_out.clip_rect_min = gs_in[0].clip_rect_min;
    gs_out.clip_rect_max = gs_in[0].clip_rect_max;
    gs_out.which_font = gs_in[0].which_font;

    gl_Position        = btm_left_coord;
    gs_out.uv          = gs_in[0].bottom_left_uv;
    gs_out.color       = gs_in[0].bottom_color;
    EmitVertex();
    gl_Position        = btm_right_coord;
    gs_out.uv          = uv_btm_right;
    gs_out.color       = gs_in[0].bottom_color;
    EmitVertex();
    gl_Position        = top_right_coord;
    gs_out.uv          = gs_in[0].top_right_uv;
    gs_out.color       = gs_in[0].top_color;
    EmitVertex();
    EndPrimitive();

    gl_Position        = btm_left_coord;
    gs_out.uv          = gs_in[0].bottom_left_uv;
    gs_out.color       = gs_in[0].bottom_color;
    EmitVertex();
    gl_Position        = top_right_coord;
    gs_out.uv          = gs_in[0].top_right_uv;
    gs_out.color       = gs_in[0].top_color;
    EmitVertex();
    gl_Position        = top_left_coord;
    gs_out.uv          = uv_top_left;
    gs_out.color       = gs_in[0].top_color;
    EmitVertex();
    EndPrimitive();
}
