attribute vec3 position;
uniform mat4 worldProjection;
uniform mat4 lightProjection;

// Used for shadow lookup
varying vec4 shadowCoord;
varying vec4 pos;

void main()
{
    vec4 p = vec4(position,1.0);
    shadowCoord = p * lightProjection;
    pos = p;
    gl_Position = p * worldProjection;
    gl_FrontColor = vec4(1.0,0.0,0.0,1.0);
}
