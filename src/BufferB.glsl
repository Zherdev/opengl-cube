// Magic Minecraft Cube
// https://github.com/Zherdev/shadertoy-magic-cube/
//
// "Buffer B" module - used for result accumulation.

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = fragCoord / iResolution.xy;
    fragColor = texture(iChannel0, uv) + texture(iChannel1, uv);
}
