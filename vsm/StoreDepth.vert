attribute vec3 position;
uniform mat4 worldProjection;

varying vec4 v_position;

void main()
{
    vec4 p = vec4(position,1.0) * worldProjection;
    gl_Position = p;
    v_position = p;
}