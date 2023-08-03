#version 330 core

layout (location = 0) in vec2 attrib_bottom_left_pos;
layout (location = 1) in vec2 attrib_top_right_pos;
layout (location = 2) in vec2 attrib_bottom_left_uv;
layout (location = 3) in vec2 attrib_top_right_uv;
layout (location = 4) in vec4 attrib_top_color;
layout (location = 5) in vec4 attrib_bottom_color;
layout (location = 6) in float attrib_corner_roundness;
layout (location = 7) in float attrib_border_thickness;
layout (location = 8) in vec2 attrib_clip_rect_min;
layout (location = 9) in vec2 attrib_clip_rect_max;
layout (location = 10) in uint attrib_which_font;

uniform vec2 screen_size; // in pixels

out VS_Out {
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
} vs_out;

void main() {
    // the input position coordinates come in pixel screen space (which goes from
    // (0, 0) at the bottom left of the screen, to (screen_size.x, screen_size.y) at
    // the top right of the screen) so we need to transform them into NDC (which goes
    // from (-1, -1) to (1, 1))
    vs_out.bottom_left_pos = (attrib_bottom_left_pos / screen_size) * 2 - vec2(1);
    vs_out.top_right_pos = (attrib_top_right_pos / screen_size) * 2 - vec2(1);
    vs_out.bottom_left_uv = attrib_bottom_left_uv;
    vs_out.top_right_uv = attrib_top_right_uv;
    vs_out.top_color = attrib_top_color;
    vs_out.bottom_color = attrib_bottom_color;
    vs_out.corner_roundness = attrib_corner_roundness;
    vs_out.border_thickness = attrib_border_thickness;
    vs_out.rect_size = attrib_top_right_pos - attrib_bottom_left_pos;
    vs_out.clip_rect_min = attrib_clip_rect_min;
    vs_out.clip_rect_max = attrib_clip_rect_max;
    vs_out.which_font = attrib_which_font;

    vs_out.rect_pixel_center = (attrib_bottom_left_pos + attrib_top_right_pos) / 2;
}

