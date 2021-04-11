// Magic Minecraft Cube
// https://github.com/Zherdev/shadertoy-magic-cube/

const int   MAX_TRACE_STEPS = 255;
const int   LIGHTS_NUM = 2;
const float MIN_DIST = 0.0;
const float MAX_DIST = 100.0;
const float PRECISION = 0.001;
const float PI = 3.14159265359;
const float SPEED = 1.0 / 15.0;
const float MINECRAFT_CLOUDS_HEIGHT = 18.0;

// Common ======================================================================

vec2 getUV(vec2 fragCoord)
{
    return (fragCoord - iResolution.xy / 2.0) / iResolution.y;
}

float noiseX(vec3 p)
{
    return textureLod(iChannel1, p, 0.0).x;
}

// Noise for generating some randomness by input vector 3.
float noise3(vec3 p)
{
    vec3 size = 1.0 / vec3(textureSize(iChannel1, 0));
    return (
        noiseX(p * size * 1.0 + vec3(0.52, 0.78, 0.43)) * 0.5 +
        noiseX(p * size * 2.0 + vec3(0.33, 0.30, 0.76)) * 0.25 +
        noiseX(p * size * 4.0 + vec3(0.70, 0.25, 0.92)) * 0.125) * 1.14;
}

vec3 phongShading(vec3 lightDir, float lightPower,
        vec3 dir, vec3 normal, vec3 K_s) {
    vec3 K_d = vec3(0.01) * 50.0;
    vec3 K_a = vec3(0.06);
    const float alpha = 50.0;

    float diffuse = clamp(dot(lightDir, normal), 0.0, 1.0);
    float specular =
            pow(
                clamp(
                    dot(
                        reflect(lightDir, normal),
                        -dir),
                    0.0, 1.0),
                alpha
            );

    return lightPower * (K_a + K_d * diffuse + K_s * specular);
}

float calcFresnel(vec3 normal, vec3 dir) {
    const float threshold = 0.0;
    const float power = 5.0;

    return pow(
                clamp(1.0 - dot(normal, dir), threshold, 1.0),
                power
            );
}

struct TraceResult
{
    vec3  vp;
    vec3  p;
    vec3  n;
    float dist;
    bool  hit;
};

struct Light
{
    vec3  pos;
    float power;
};

vec3 calcLightDir(Light light, vec3 pos)
{
    return normalize(light.pos - pos);
}

vec3 calcLight(Light light, vec3 dir, vec3 normal, vec3 pos, vec3 K_s)
{
    vec3 lightDir = calcLightDir(light, pos);
    vec3 col = phongShading(lightDir, light.power, dir, normal, K_s);

    return col;
}

// Common ^=====================================================================

// Minecraft ===================================================================

bool isMinecraftGrassBlock(vec3 vp)
{
    if (vp.y < MINECRAFT_CLOUDS_HEIGHT - 2.0) {
        const float threshold = 0.5;
        return noise3(vp * 0.05) + vp.y * -0.015 > threshold;
    }
    return false;
}

TraceResult traceMinecraftGrass(vec3 orig, vec3 dir)
{
    const float maxDist = float(MAX_TRACE_STEPS);
    float dist = maxDist;

    TraceResult res;
    res.n = -dir;
    res.dist = maxDist;

    vec3 id = 1.0 / dir;
    vec3 sd = sign(dir);
    vec3 nd = max(-sd, 0.0);
    vec3 vp = floor(orig) - nd * vec3(equal(floor(orig), orig));
    vec3 pos = orig;

    for (int i = 0; i < MAX_TRACE_STEPS; i++) {
        if (dist <= 0.0 || orig.y > MINECRAFT_CLOUDS_HEIGHT && dir.y > 0.0) {
        	break;
        }

        if (isMinecraftGrassBlock(vp)) {
			res.vp = vp;
			res.p = pos;
			res.dist = dist;
			res.hit = true;
			return res;
        }

        vec3 n = mix(floor(pos + 1.0), ceil(pos - 1.0), nd);
		vec3 ls = (n - pos) * id;
		float l = min(min(ls.x, ls.y), ls.z);
		vec3 a = vec3(equal(vec3(l), ls));

        pos = mix(pos + dir * l, n, a);
        vp += sd * a;
        res.n = -sd * a;
        dist -= l;
    }

    return res;
}

vec3 minecraftSkyColor()
{
	return vec3(0.25, 0.35, 0.85);
}

vec3 renderMinecraftGrass(TraceResult r, vec3 orig, vec3 dir)
{
    const vec3 grassBaseColor = vec3(0.5, 0.8, 0.25);
    const vec3 dirtColor = vec3(0.8, 0.6, 0.4);
    float texelNoise = textureLod(iChannel0, r.p * 0.5, 0.0).r;

    // Check if block under this is grass (dirt) too.
    float grassMix = 0.0;
    if (!isMinecraftGrassBlock(r.vp + vec3(0, 1, 0))) {
        if (texelNoise * 4.0 + floor(fract(r.p.y) * 16.0) > 15.0) {
            grassMix = 1.0;
        } else {
            grassMix = max(0.0, r.n.y);
        }
    }

    vec3 texel = vec3(texelNoise) * 0.3 + 0.75;
    vec3 grassColor = texel * mix(dirtColor, grassBaseColor, grassMix);
    return grassColor;
}

vec3 renderMinecraftFrag(vec3 orig, vec3 dir)
{
    TraceResult r = traceMinecraftGrass(orig, dir);
    if (r.hit) {
        return renderMinecraftGrass(r, orig, dir);
    }

    return minecraftSkyColor();
}

vec3 minecraftCamera()
{
    const vec3 cameraStart = vec3(-10.0, 15.0, 10.0);
    const vec3 cameraMoveDir = vec3(-50.0 * SPEED, 0.0, 0.0);

    vec3 camera = cameraStart + mod(cameraMoveDir * iTime, 255.0);
    return camera;
}

void mainImageMinecraft(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 orig = vec2(PI * 0.75, -2.0 * PI * iTime * SPEED);
    vec4 cs = cos(vec4(orig.y, orig.x, orig.y - PI * 0.5, orig.x - PI * 0.5));
    vec3 forward = -vec3(cs.x * cs.y, cs.w, cs.z * cs.y);
	vec3 up = vec3(cs.x * cs.w, -cs.y, cs.z * cs.w);
	vec3 left = cross(up, forward);

	vec2 uv = getUV(fragCoord);
    vec3 dir = normalize(vec3(forward + uv.y * up + uv.x * left));
    vec3 camera = minecraftCamera();

    vec3 col = renderMinecraftFrag(camera, dir);
    fragColor = vec4(col, 1.0);
}

bool isMinecraftSubscene(vec2 fragCoord, vec2 center, float radius)
{
    vec2 dist = fragCoord - center;
    vec3 forNoise = vec3(fragCoord, 0);

    return length(dist) / radius + noise3(forNoise * 0.05) < 1.5;
}

float getMinecraftAlpha(vec2 fragCoord, vec2 center, float radius)
{
    vec2 dist = fragCoord - center;
    float normalized = length(dist) / radius;

    float res = 0.85 - normalized;

    if (res < 0.0) {
        return 0.0;
    }
    return res;
}

vec2 toMinecraftCoords(vec2 fragCoord, vec2 center, float minecraftRadius)
{
    vec2 shifted = fragCoord - center + minecraftRadius;
    vec2 normalized = shifted / (2.0 * minecraftRadius);
    return normalized * iResolution.x;
}

// Minecraft ^==================================================================

// Cube figure =================================================================

struct Cube {
    float size;
};

bool isInsideCube(Cube cube, vec3 pos)
{
    return abs(pos.x) < cube.size &&
            abs(pos.y) < cube.size &&
            abs(pos.z) < cube.size;
}

float distToCube(Cube cube, vec3 pos)
{
    float dist = sqrt(
            pow(max(0.0, abs(pos.x) - cube.size), 2.0) +
            pow(max(0.0, abs(pos.y) - cube.size), 2.0) +
            pow(max(0.0, abs(pos.z) - cube.size), 2.0));

    if (isInsideCube(cube, pos)) {
      dist *= -1.0;
    }

    return dist;
}

TraceResult traceCube(Cube cube, vec3 orig, vec3 dir) {
    TraceResult res;
    float depth = MIN_DIST;

    int i = 0;
    for (i = 0; i < MAX_TRACE_STEPS; i++) {
        vec3 pos = orig + depth * dir;
        float dist = distToCube(cube, pos);
        depth += dist;

        if (dist < PRECISION || depth > MAX_DIST) {
            break;
        }
    }

    res.dist = depth;
    res.hit = depth <= MAX_DIST && i < MAX_TRACE_STEPS;
    return res;
}

vec3 cubeNormal(Cube cube, vec3 pos) {
    vec2 e = vec2(1.0, -1.0) * 0.001;
    return normalize(
            e.xyy * distToCube(cube, pos + e.xyy) +
            e.yyx * distToCube(cube, pos + e.yyx) +
            e.yxy * distToCube(cube, pos + e.yxy) +
            e.xxx * distToCube(cube, pos + e.xxx));
}

vec3 renderCube(Cube cube, TraceResult t, vec3 orig, vec3 dir,
        Light lights[LIGHTS_NUM])
{
    vec3 pos = orig + dir * t.dist;
    vec3 normal = cubeNormal(cube, pos);

    vec3 K_s = vec3(2.5);
    vec3 fresnel = K_s + ( 1.0 - K_s ) * calcFresnel(normal, -dir);

    vec3 cubeColor = vec3(0);

    for (int i = 0; i < LIGHTS_NUM; i++) {
        cubeColor += calcLight(lights[i], dir, normal, pos, K_s);
    }

    vec3 cubemapPos = refract(normal, dir, 0.55);
    cubemapPos.y *= -1.0;
    vec3 cubemap = texture(iChannel2,cubemapPos).rgb * fresnel;
    cubeColor += cubemap * 0.7;

    return cubeColor;
}

// Cube figure ^================================================================

// Cylinder figure =============================================================

struct Cylinder {
    float radius;
    float height;
    vec3 center;
};

bool isInsideCylinder(Cylinder cyl, vec3 pos)
{
    return abs(pos.x - cyl.center.x) < cyl.radius &&
            abs(pos.y - cyl.center.y) < cyl.height &&
            abs(pos.z - cyl.center.z) < cyl.radius;
}

float distToCylinder(Cylinder cyl, vec3 pos)
{
    if (abs(pos.y - cyl.center.y) < cyl.height + PRECISION) {
        return length(pos.xz - cyl.center.xz) - cyl.radius;
    }

    float nearestY = 0.0;
    if (pos.y >= cyl.center.y + cyl.height) {
        nearestY = cyl.center.y + cyl.height;
    } else {
        nearestY = cyl.center.y - cyl.height;
    }

    float sig = 1.0;
    if (isInsideCylinder(cyl, pos)) {
         sig = -1.0;
    }

    if (abs(pos.x - cyl.center.x) < cyl.radius &&
            abs(pos.z - cyl.center.z) < cyl.radius) {
        return sig * abs(pos.y - nearestY);
    }

    vec2 nearestXZ = normalize(pos.xz - cyl.center.xz) * cyl.radius + cyl.center.xz;
    vec3 nearest = vec3(nearestXZ.x, nearestY, nearestXZ.y);
    return sig * length(pos - nearest);
}

vec3 cylNormal(Cylinder cyl, vec3 pos) {
    vec2 e = vec2(1.0, -1.0) * 0.001;
    float r = 1.;
    return normalize(
            e.xyy * distToCylinder(cyl, pos + e.xyy) +
            e.yyx * distToCylinder(cyl, pos + e.yyx) +
            e.yxy * distToCylinder(cyl, pos + e.yxy) +
            e.xxx * distToCylinder(cyl, pos + e.xxx));
}

TraceResult traceCylinder(Cylinder cyl, vec3 orig, vec3 dir)
{
    TraceResult res;
    float depth = MIN_DIST;

    int i = 0;
    for (i = 0; i < MAX_TRACE_STEPS; i++) {
        vec3 p = orig + depth * dir;
        float dist = distToCylinder(cyl, p);
        depth += dist;

        if (dist < PRECISION || depth > MAX_DIST) {
            break;
        }
    }

    res.dist = depth;
    res.hit = depth <= MAX_DIST && i < MAX_TRACE_STEPS;
    return res;
}

vec3 renderCylinder(Cylinder cyl, TraceResult t, vec3 orig, vec3 dir,
        Light lights[LIGHTS_NUM])
{
    vec3 pos = orig + dir * t.dist;
    vec3 normal = cylNormal(cyl, pos);

    vec3 K_s = vec3(1.0);
    vec3 fresnel = K_s + ( 1.0 - K_s ) * calcFresnel(normal, -dir);

    vec3 cylColor = vec3(0);

    for (int i = 0; i < LIGHTS_NUM; i++) {
        cylColor += calcLight(lights[i], dir, normal, pos, K_s);
    }

    return cylColor;
}

// Cylinder figure ^============================================================

// Main ========================================================================

mat3 mainCamera(vec3 cameraPos, vec3 lookAtPoint)
{
	vec3 cd = normalize(lookAtPoint - cameraPos);
	vec3 cr = normalize(cross(vec3(0, 1, 0), cd));
	vec3 cu = normalize(cross(cd, cr));

	return mat3(-cr, cu, -cd);
}

mat2 mainRotate(float angle) {
    float s = sin(angle);
    float cs = cos(angle);

    return mat2(
                cs, -s,
                s,  cs
            );
}

vec3 renderMainFrag(vec2 uv, vec2 fragCoord)
{
    vec3 lp = vec3(0);
    vec3 orig = vec3(0, 1.5, 3.0);
    orig.xz *= mainRotate(mix(-PI, PI, iTime * SPEED));

    mat3 camera = mainCamera(orig, lp);
    vec3 dir = camera * normalize(vec3(uv, -1));

    // Global background texture.
    vec3 col = texture(iChannel2, dir).rgb;

    // Light setup =============================================================
    Light lights[LIGHTS_NUM];

    lights[0].pos = vec3(1, 1, 1);
    lights[0].power = 0.65;

    lights[1].pos = vec3(-8, -6, -5);
    lights[1].power = 1.0;
    // Light setup ^============================================================

    // Figures setup ===========================================================
    Cylinder cyl;
    cyl.radius = 0.7;
    cyl.height = 2.25;
    cyl.center = vec3(0.0, -3, 0.0);

    Cube cube;
    cube.size = 0.75;
    // Figures setup ^==========================================================

    // Tracing and rendering figures ===========================================
    TraceResult tCyl = traceCylinder(cyl, orig, dir);
    if (tCyl.hit) {
        vec3 cylColor = renderCylinder(cyl, tCyl, orig, dir, lights);
        col = mix(col, cylColor, 1.0);
    }

    TraceResult tCube = traceCube(cube, orig, dir);
    if (tCube.hit) {
        vec3 cubeColor = renderCube(cube, tCube, orig, dir, lights);
        col = mix(col, cubeColor, 0.85);
    }
    // Rendering figures ^======================================================

    // Minecraft subscene ======================================================
    float minecraftRadius = iResolution.x / 8.0;
    vec2 minecraftPos = vec2(iResolution.x * 0.5, iResolution.y * 0.40);

    if (isMinecraftSubscene(fragCoord, minecraftPos, minecraftRadius)) {
        vec4 minecraftColor = vec4(0.0);
        vec2 minecraftCoord = toMinecraftCoords(
                fragCoord, minecraftPos, minecraftRadius);

        mainImageMinecraft(minecraftColor, minecraftCoord);

        float alpha = getMinecraftAlpha(fragCoord, minecraftPos, minecraftRadius);
        col = mix(vec4(col, 1.0), minecraftColor, alpha).rgb;
    }
    // Minecraft subscene ^=====================================================

    return col;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec3 col = vec3(0);
    vec2 uv = getUV(fragCoord);
    col += renderMainFrag(uv, fragCoord);

    // Anti-aliasing x2.
    vec2 uvh = uv + vec2(1.0, 1.0) / iResolution.x;
    col += renderMainFrag(uvh, fragCoord);
    uvh = uv + vec2(-1.0, -1.0) / iResolution.x;
    col += renderMainFrag(uvh, fragCoord);
    col /= 3.0;

    fragColor = vec4(col, 1.0);
}

// Main ^=======================================================================
