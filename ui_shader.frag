#version 330 core

layout (pixel_center_integer) in vec4 gl_FragCoord;

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

    flat vec2 rect_pixel_center;
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

float rectSDF(vec2 point, vec2 center, vec2 half_size) {
    point = abs(point - center);
    vec2 dist = point - half_size;
    float corner_dist = length(point - half_size);
    return dist.x > 0 && dist.y > 0 ? corner_dist : max(dist.x, dist.y);
}

float roundedRectSDF(vec2 point, vec2 center, vec2 half_size, float corner_radius) {
    half_size -= vec2(corner_radius);
    float dist = rectSDF(point, center, half_size);
    dist -= corner_radius;
    return dist;
}

void main() {
    // FragColor = fs_in.color;
    // return;

    if (fs_in.rect_size == screen_size) {
        FragColor = vec4(gl_FragCoord.xy / screen_size, 1, 1);
        return;
    }

    vec2 pixel_coord = gl_FragCoord.xy;
    vec4 rect_color = fs_in.color;
    vec2 rect_half_size = fs_in.rect_size / 2;
    vec2 rect_center = fs_in.rect_pixel_center;
    float corner_radius = rect_half_size[0]; // in pixels
    float corner_softness = 1; // TODO: somethings not right about this
    float thickness = 10;

    float rect_dist = roundedRectSDF(pixel_coord, rect_center, rect_half_size, corner_radius);

    FragColor = vec4(
        rect_color.rgb,
        rect_color.a * smoothstep(-corner_softness, corner_softness, -rect_dist)
    );
}

void old_main() {
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