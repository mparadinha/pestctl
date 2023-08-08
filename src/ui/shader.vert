#version 330 core

in vec2 attrib_btm_left_pos;
in vec2 attrib_top_right_pos;
in vec2 attrib_btm_left_uv;
in vec2 attrib_top_right_uv;
in vec4 attrib_top_left_color;
in vec4 attrib_btm_left_color;
in vec4 attrib_top_right_color;
in vec4 attrib_btm_right_color;
in float attrib_top_left_corner_radius;
in float attrib_btm_left_corner_radius;
in float attrib_top_right_corner_radius;
in float attrib_btm_right_corner_radius;
in float attrib_border_thickness;
in vec2 attrib_clip_rect_min;
in vec2 attrib_clip_rect_max;
in uint attrib_which_font;

out VS_Out {
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
} vs_out;

void main() {
    vs_out.btm_left_pos = attrib_btm_left_pos;
    vs_out.top_right_pos = attrib_top_right_pos;
    vs_out.btm_left_uv = attrib_btm_left_uv;
    vs_out.top_right_uv = attrib_top_right_uv;
    vs_out.top_left_color = attrib_top_left_color;
    vs_out.btm_left_color = attrib_btm_left_color;
    vs_out.top_right_color = attrib_top_right_color;
    vs_out.btm_right_color = attrib_btm_right_color;
    vs_out.top_left_corner_radius = attrib_top_left_corner_radius;
    vs_out.btm_left_corner_radius = attrib_btm_left_corner_radius;
    vs_out.top_right_corner_radius = attrib_top_right_corner_radius;
    vs_out.btm_right_corner_radius = attrib_btm_right_corner_radius;
    vs_out.border_thickness = attrib_border_thickness;
    vs_out.clip_rect_min = attrib_clip_rect_min;
    vs_out.clip_rect_max = attrib_clip_rect_max;
    vs_out.which_font = attrib_which_font;
}