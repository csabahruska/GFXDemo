uniform sampler2D shadowMap;

varying vec4 shadowCoord;
varying vec4 pos;

vec4 shadowCoordPostW;

float chebyshevUpperBound(float distance)
{
    vec2 moments = texture2D(shadowMap, shadowCoordPostW.xy).rg;

    // Surface is fully lit. as the current fragment is before the light occluder
//    if (distance <= moments.x)
//        return 1.0 ;

    // The fragment is either in shadow or penumbra. We now use chebyshev's upperBound to check
    // How likely this pixel is to be lit (p_max)
//    float variance = moments.y - (moments.x * moments.x);
//    variance = max(variance, 0.002);
    float variance = max(moments.y - (moments.x * moments.x), 0.002);

    float d = distance - moments.x;
    float p_max = variance / (variance + d*d);

    return p_max;
}


void main()
{
    shadowCoordPostW = shadowCoord / shadowCoord.w;

    // This is done via a bias matrix in main.c
    shadowCoordPostW = shadowCoordPostW * 0.5 + 0.5;

    
    //float shadow = chebyshevUpperBound(shadowCoordPostW.z);
    float shadow = chebyshevUpperBound(shadowCoord.z);

    gl_FragColor = vec4(shadow + 0.1) * vec4(fract(pos.x),fract(pos.y),fract(pos.z),1.0);
}
