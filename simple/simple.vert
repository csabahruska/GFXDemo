attribute vec3 position;
attribute vec3 normal;
attribute vec2 UVTex;

uniform mat4 cameraMatrix;
uniform mat4 projectionMatrix;
uniform float time;

varying vec3 n;
varying vec2 t;

void main()
{
    mat4 m = cameraMatrix * projectionMatrix;
    n = (vec4(normal.xyz,0.0) * m).xyz;
    t = UVTex + vec2(time*0.0001);

    //gl_Position = vec4(v * vec3(1.0 + 0.4 * sin((time+v.x)*1.0*3.1415)),1.0) * m;
    gl_Position = vec4(position ,1.0) * m + vec4((0.05*sin(position.x+time*2.0))*n,1.0);
}