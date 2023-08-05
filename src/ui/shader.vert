#version 330 core

layout (location = 0) in vec2 attrib_bottom_left_pos;
layout (location = 1) in vec2 attrib_top_right_pos;
layout (location = 2) in vec2 attrib_bottom_left_uv;
layout (location = 3) in vec2 attrib_top_right_uv;
layout (location = 4) in vec4 attrib_top_color;
layout (location = 5) in vec4 attrib_bottom_color;
layout (location = 6) in float attrib_corner_radius;
layout (location = 7) in float attrib_border_thickness;
layout (location = 8) in vec2 attrib_clip_rect_min;
layout (location = 9) in vec2 attrib_clip_rect_max;
layout (location = 10) in uint attrib_which_font;

out VS_Out {
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
} vs_out;

void main() {
    vs_out.bottom_left_pos = attrib_bottom_left_pos;
    vs_out.top_right_pos = attrib_top_right_pos;
    vs_out.bottom_left_uv = attrib_bottom_left_uv;
    vs_out.top_right_uv = attrib_top_right_uv;
    vs_out.top_color = attrib_top_color;
    vs_out.bottom_color = attrib_bottom_color;
    vs_out.corner_radius = attrib_corner_radius;
    vs_out.border_thickness = attrib_border_thickness;
    vs_out.clip_rect_min = attrib_clip_rect_min;
    vs_out.clip_rect_max = attrib_clip_rect_max;
    vs_out.which_font = attrib_which_font;
}