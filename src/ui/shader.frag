#version 330 core

layout (pixel_center_integer) in vec4 gl_FragCoord;


in GS_Out {
    vec2 uv;
    vec4 color;
    flat vec2 rect_size;
    flat vec2 rect_center;
    flat float corner_radii[4];
    flat float border_thickness;
    flat vec2 clip_rect_min;
    flat vec2 clip_rect_max;
    flat uint which_font;
} fs_in;

uniform vec2 screen_size;
uniform sampler2D text_atlas;
uniform sampler2D text_bold_atlas;
uniform sampler2D icon_atlas;

out vec4 FragColor;

bool rectContains(vec2 rect_min, vec2 rect_max, vec2 point) {
    return rect_min.x <= point.x && point.x <= rect_max.x &&
           rect_min.y <= point.y && point.y <= rect_max.y;
}

float rectSDF(vec2 point, vec2 center, vec2 half_size) {
    point = abs(point - center);
    vec2 dist = point - half_size;
    float corner_dist = length(point - half_size);
    return dist.x > 0 && dist.y > 0 ? corner_dist : max(dist.x, dist.y);
}

float roundedRectSDF(vec2 point, vec2 center, vec2 half_size, float corner_radii[4]) {
    float corner_radius = corner_radii[
        (point.x > center.x ? 1 : 0) + (point.y < center.y ? 2 : 0)
    ];
    half_size -= vec2(corner_radius);
    float dist = rectSDF(point, center, half_size);
    dist -= corner_radius;
    return dist;
}

// `center`: distance from the edge to the center of the border
float toBorder(float dist, float center, float border_size) {
    return abs(dist - center) - border_size / 2;
}

void main() {
    vec2 pixel_coord = gl_FragCoord.xy;
    vec4 rect_color = fs_in.color;
    vec2 rect_half_size = fs_in.rect_size / 2;
    vec2 rect_center = fs_in.rect_center;
    float corner_radii[4] = fs_in.corner_radii;
    float corner_softness = 1; // TODO: somethings not right about this
    float thickness = fs_in.border_thickness;
    if (thickness == 0) thickness = max(rect_half_size[0], rect_half_size[1]);

    FragColor = vec4(0);

    // clipping
    if (!rectContains(fs_in.clip_rect_min, fs_in.clip_rect_max, pixel_coord)) {
        return;
    }

    float rect_dist = roundedRectSDF(pixel_coord, rect_center, rect_half_size, corner_radii);
    rect_dist = toBorder(rect_dist, -(thickness / 2), thickness);

    FragColor = rect_color;
    FragColor.a *= smoothstep(-corner_softness, corner_softness, -rect_dist);

    float tex_alpha = 1;
    switch (fs_in.which_font) {
        case 0u: tex_alpha = texture(text_atlas, fs_in.uv).r; break;
        case 1u: tex_alpha = texture(text_bold_atlas, fs_in.uv).r; break;
        case 2u: tex_alpha = texture(icon_atlas, fs_in.uv).r; break;
    }
    if (fs_in.uv != vec2(0, 0)) FragColor.a = tex_alpha;
}