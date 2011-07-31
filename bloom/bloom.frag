uniform sampler2D renderedTexture;

varying vec2 texCoord;

void main()
{
   vec4 sum = vec4(0);
   int j;
   int i;

   for( i= -4 ;i < 4; i++)
   {
        for (j = -3; j < 3; j++)
        {
            sum += texture2D(renderedTexture, texCoord + vec2(j, i)*0.004) * 0.25;
        }
   }
       if (texture2D(renderedTexture, texCoord).r < 0.3)
    {
       gl_FragColor = sum*sum*0.012 + texture2D(renderedTexture, texCoord);
    }
    else
    {
        if (texture2D(renderedTexture, texCoord).r < 0.5)
        {
            gl_FragColor = sum*sum*0.009 + texture2D(renderedTexture, texCoord);
        }
        else
        {
            gl_FragColor = sum*sum*0.0075 + texture2D(renderedTexture, texCoord);
        }
    }
}
