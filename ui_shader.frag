#version 330 core

in vec4 gl_FragCoord;

in GS_Out {
    vec2 uv;
    vec4 color;
    vec2 quad_coords;

    float quad_size_ratio;
    float corner_roundness; // 0 is square quad, 1 is full circle

    // these are all in pixels
    float border_thickness;
    vec2 rect_size;
    vec2 clip_rect_min;
    vec2 clip_rect_max;

    flat uint which_font;
} fs_in;

uniform vec2 screen_size; // in pixels
uniform sampler2D text_atlas;
uniform sampler2D text_bold_atlas;
uniform sampler2D icon_atlas;

out vec4 FragColor;

bool rectContains(vec2 rect_min, vec2 rect_max, vec2 point) {
    return (rect_min.x <= point.x && point.x <= rect_max.x) &&
           (rect_min.y <= point.y && point.y <= rect_max.y);
}

bool insideBorder(vec2 quad_coords) {
    if (fs_in.border_thickness == 0) return true;

    vec2 coords = abs((quad_coords * 2) - vec2(1));
    vec2 side_dist = (fs_in.rect_size / 2) * (vec2(1) - coords);
    return side_dist.x <= fs_in.border_thickness || side_dist.y <= fs_in.border_thickness;
}

void main() {
    vec2 pixel_coord = gl_FragCoord.xy - vec2(0.5);
    if (!rectContains(fs_in.clip_rect_min, fs_in.clip_rect_max, pixel_coord)) {
        FragColor = vec4(0);
        return;
    }

    vec2 uv = fs_in.uv;
    vec4 color = fs_in.color;
    vec2 quad_coords = fs_in.quad_coords;

    float alpha = 1;
    switch (fs_in.which_font) {
        case 0u: alpha = texture(text_atlas, uv).r; break;
        case 1u: alpha = texture(text_bold_atlas, uv).r; break;
        case 2u: alpha = texture(icon_atlas, uv).r; break;
    }
    if (uv == vec2(0, 0)) alpha = 1;

    if (!insideBorder(quad_coords)) alpha = 0;

    FragColor = color * vec4(1, 1, 1, alpha);
}

