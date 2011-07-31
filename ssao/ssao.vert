attribute vec2 position;

varying vec2 texCoord;

void main()
{
    gl_Position = vec4(position,0,1.0);
    texCoord = position*vec2(0.5)+vec2(0.5);
}