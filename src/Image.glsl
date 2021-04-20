// Magic Minecraft Cube
// https://github.com/Zherdev/shadertoy-magic-cube/
//
// "Image" module - takes mean result from Buffer B and draws it.

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = fragCoord / iResolution.xy;
    fragColor = texture(iChannel0, uv) / float(iFrame + 1);
}
