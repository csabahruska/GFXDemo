attribute vec3 position;
attribute vec2 uv;
uniform mat4 worldProjection;

varying vec2 v_Coordinates;

void main()
{
    gl_Position = vec4(position,1.0) * worldProjection;
    v_Coordinates = uv;
}
