uniform sampler2D diffuseTexture;

varying vec3 n;
varying vec2 t;

void main()
{
    vec3 dir = normalize(vec3(1,1,-1));
    float lum = dot(dir,n);
    gl_FragColor = lum*texture2D(diffuseTexture, t)+vec4(fract(t)*0.5,0.0,0.0);
}