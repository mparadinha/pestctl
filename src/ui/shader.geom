#version 330 core

layout (points) in;
layout (triangle_strip, max_vertices = 6) out;

uniform vec2 screen_size;

in VS_Out {
    vec2 btm_left_pos;
    vec2 top_right_pos;

    vec2 btm_left_uv;
    vec2 top_right_uv;

    vec4 top_left_color;
    vec4 btm_left_color;
    vec4 top_right_color;
    vec4 btm_right_color;

    float top_left_corner_radius;
    float btm_left_corner_radius;
    float top_right_corner_radius;
    float btm_right_corner_radius;

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
    flat float corner_radii[4];
    flat float border_thickness;
    flat vec2 clip_rect_min;
    flat vec2 clip_rect_max;
    flat uint which_font;
} gs_out;

void main() {
    vec2 btm_left_pos = gs_in[0].btm_left_pos;
    vec2 top_right_pos = gs_in[0].top_right_pos;
    vec2 btm_right_pos = vec2(top_right_pos.x, btm_left_pos.y);
    vec2 top_left_pos = vec2(btm_left_pos.x, top_right_pos.y);

    vec2 btm_left_uv = gs_in[0].btm_left_uv;
    vec2 top_right_uv = gs_in[0].top_right_uv;
    vec2 btm_right_uv = vec2(top_right_uv.x, btm_left_uv.y);
    vec2 top_left_uv = vec2(btm_left_uv.x, top_right_uv.y);

    // some things are the same for all verts of the quad
    gs_out.rect_size = top_right_pos - btm_left_pos;
    gs_out.rect_center = (btm_left_pos + top_right_pos) / 2;
    gs_out.corner_radii = float[4](
        gs_in[0].top_left_corner_radius,
        gs_in[0].top_right_corner_radius,
        gs_in[0].btm_left_corner_radius,
        gs_in[0].btm_right_corner_radius
    );
    gs_out.border_thickness = gs_in[0].border_thickness;
    gs_out.clip_rect_min = gs_in[0].clip_rect_min;
    gs_out.clip_rect_max = gs_in[0].clip_rect_max;
    gs_out.which_font = gs_in[0].which_font;

    gl_Position  = vec4((btm_left_pos / screen_size) * 2 - vec2(1), 0, 1);
    gs_out.uv    = btm_left_uv;
    gs_out.color = gs_in[0].btm_left_color;
    EmitVertex();
    gl_Position  = vec4((btm_right_pos / screen_size) * 2 - vec2(1), 0, 1);
    gs_out.uv    = btm_right_uv;
    gs_out.color = gs_in[0].btm_right_color;
    EmitVertex();
    gl_Position  = vec4((top_right_pos / screen_size) * 2 - vec2(1), 0, 1);
    gs_out.uv    = top_right_uv;
    gs_out.color = gs_in[0].top_right_color;
    EmitVertex();
    EndPrimitive();

    gl_Position  = vec4((btm_left_pos / screen_size) * 2 - vec2(1), 0, 1);
    gs_out.uv    = btm_left_uv;
    gs_out.color = gs_in[0].btm_left_color;
    EmitVertex();
    gl_Position  = vec4((top_right_pos / screen_size) * 2 - vec2(1), 0, 1);
    gs_out.uv    = top_right_uv;
    gs_out.color = gs_in[0].top_right_color;
    EmitVertex();
    gl_Position  = vec4((top_left_pos / screen_size) * 2 - vec2(1), 0, 1);
    gs_out.uv    = top_left_uv;
    gs_out.color = gs_in[0].top_left_color;
    EmitVertex();
    EndPrimitive();
}
