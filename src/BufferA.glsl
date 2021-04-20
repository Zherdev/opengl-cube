// Magic Minecraft Cube
// https://github.com/Zherdev/shadertoy-magic-cube/
//
// "Buffer A" module - renders image for each frame.


// Common ======================================================================

const float INF = 1e10;
const int   LIGHTS_NUM = 2;
const float PRECISION = 0.001;
const float PI = 3.14159265359;

float sqr(float v)
{
    return v * v;
}

vec2 getUV(vec2 fragCoord)
{
    return (fragCoord - iResolution.xy / 2.0) / iResolution.y;
}

float noise(vec3 v)
{
    return textureLod(iChannel0, v, 0.0).x;
}

float noise3(vec3 p)
{
    vec3 size = 1.0 / vec3(textureSize(iChannel0, 0));
    return (
        noise(p * size * 1.0 + vec3(0.52, 0.78, 0.43)) * 0.5 +
        noise(p * size * 2.0 + vec3(0.33, 0.30, 0.76)) * 0.25 +
        noise(p * size * 4.0 + vec3(0.70, 0.25, 0.92)) * 0.125) * 1.14;
}

float rand(float frame)
{
    return fract(
            sin(
                dot(
                    vec3(frame),
                    vec3(12.9899,78.234,45.5433)
                )
            ) * 43758.5452);
}

struct TraceResult {
    vec3  vp;
    vec3  p;
    vec3  n;
    float dist;
    bool  hit;
};

struct TraceSubsceneResult {
    TraceResult globalTrace;
    TraceResult subTrace;
    float       sumDist;
    bool        hit;
};

struct RefractionResult {
    vec3 newDir;
    bool isReflection;
};

RefractionResult calcRefraction(vec3 v, vec3 normal, float n1, float n2) {
    RefractionResult res;

    if (dot(v, normal) < 0.0) {
        normal = -normal;
    }

    float cosA = dot(v, normal);
    float sinA = sqrt(1.0 - cosA * cosA);
    vec3  tang = normalize(v - cosA * normal);
    float sinB = sinA / n2 * n1;

    if (abs(sinB) > 1.0) {
        res.newDir = reflect(v, normal);
        res.isReflection = true;
        return res;
    }

    float cosB = sqrt(1.0 - sinB * sinB);

    res.newDir = sinB * tang + cosB * normal;
    return res;
}


// Common ^=====================================================================

// Lights ======================================================================

struct Light {
    vec3  pos;
    vec3  color;
    float attBase;
};

vec3 calcLight(Light lights[LIGHTS_NUM], vec3 pos, vec3 color, vec3 normal) {
    vec3 sum = vec3(0.0);

    for (int i = 0; i < LIGHTS_NUM; i++) {
        Light light = lights[i];
        vec3 dist = light.pos - pos;
        float distSq = dot(dist, dist);
        float att = light.attBase / distSq;
        sum += max(0.0, dot(normal, normalize(dist))) * light.color * att;
    }

    return color * sum;
}

// Lights ^=====================================================================

// Cube figure =================================================================

struct Cube {
    float size;
    float reflectN;
    float reflectR;
    vec3  color;
};

float traceCubePlane(Cube cube, vec3 normal, vec3 orig, vec3 dir)
{
    // normal * (orig + t * dir) + cube.size = 0
    // t = ?
    float t = -(dot(normal, orig) + cube.size) / dot(normal, dir);
    if (t < 0.0) {
        return INF;
    }

    vec3 pos = orig + t * dir;
    if (abs(pos.x) > cube.size + PRECISION     ||
            abs(pos.y) > cube.size + PRECISION ||
            abs(pos.z) > cube.size + PRECISION) {
        return INF;
    }

    return t;
}

TraceResult traceCube(Cube cube, vec3 orig, vec3 dir)
{
    TraceResult res;

    float dist = INF;
    vec3 normal;

    const int planesNum = 6;
    vec3 planes[planesNum];
    planes[0] = vec3(1, 0, 0);
    planes[1] = vec3(-1, 0, 0);
    planes[2] = vec3(0, 1, 0);
    planes[3] = vec3(0, -1, 0);
    planes[4] = vec3(0, 0, 1);
    planes[5] = vec3(0, 0, -1);

    for (int i = 0; i < planesNum; i++) {
        vec3 plane = planes[i];
        float distToPlane = traceCubePlane(cube, plane, orig, dir);
        if (distToPlane < dist) {
            dist = distToPlane;
            normal = plane;
        }
    }

    res.dist = dist;
    res.n = -normal;
    res.hit = dist >= 0.0 && dist < INF;

    return res;
}

// Cube figure ^================================================================

// Minecraft ===================================================================

struct Minecraft {
    Cube  subscene;
    float size;
    float maxHeight;
};

bool isMinecraftGrassBlock(Minecraft mine, vec3 vp)
{
    if (vp.y < mine.maxHeight - 2.0) {
        const float threshold = 0.75;
        return noise3(vp * 0.03) + vp.y * -0.015 > threshold;
    }

    return false;
}

TraceResult traceMinecraftGrass(Minecraft mine, vec3 orig, vec3 dir)
{
    float maxDist = length(
            vec2(2.0 * mine.size, 2.0 * mine.size));
    float dist = maxDist;

    TraceResult res;
    res.n = -dir;
    res.dist = INF;

    vec3 id = 1.0 / dir;
    vec3 sd = sign(dir);
    vec3 nd = max(-sd, 0.0);
    vec3 vp = floor(orig) - nd * vec3(equal(floor(orig), orig));
    vec3 pos = orig;

    for (int i = 0; i < int(maxDist); i++) {
        if (dist <= 0.0 || orig.y > mine.maxHeight && dir.y > 0.0) {
        	break;
        }

        if (isMinecraftGrassBlock(mine, vp)) {
			res.vp = vp;
			res.p = pos;
			res.dist = maxDist - dist;
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

vec3 renderMinecraftGrass(Minecraft mine, TraceResult r, vec3 orig, vec3 dir)
{
    const vec3 grassBaseColor = vec3(0.5, 0.8, 0.25);
    const vec3 dirtColor = vec3(0.8, 0.6, 0.4);
    float texelNoise = textureLod(iChannel0, r.p * 0.5, 0.0).r;

    // Check if block above this is grass (dirt) too.
    float grassMix = 0.0;
    if (!isMinecraftGrassBlock(mine, r.vp + vec3(0, 1, 0))) {
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

vec3 toMinePos(Minecraft mine, TraceResult tMineCube, vec3 orig, vec3 dir)
{
    vec3 mineGlobalPos = orig + tMineCube.dist * dir;
    vec3 minePos = mineGlobalPos * mine.size / mine.subscene.size;
    minePos.y -= 10.0;
    minePos.x += 45.0;
    return minePos;
}

bool isMinecraftNoise(
        Minecraft mine, TraceSubsceneResult t, vec3 orig, vec3 dir)
{
    vec3 minePos = toMinePos(mine, t.globalTrace, orig, dir);
    vec3 globalPos = orig + t.sumDist * dir;
    return length(globalPos) / mine.subscene.size * 3.5 -
           minePos.y / mine.size +
           noise(globalPos) * 2.0 < 5.0;
}

TraceSubsceneResult traceMinecraftSubscene(Minecraft mine, vec3 orig, vec3 dir)
{
    TraceSubsceneResult res;

    TraceResult tMineCube = traceCube(mine.subscene, orig, dir);
    res.globalTrace = tMineCube;
    if (tMineCube.hit) {
        vec3 minePos = toMinePos(mine, tMineCube, orig, dir);
        res.subTrace = traceMinecraftGrass(mine, minePos, dir);
        res.sumDist = tMineCube.dist + res.subTrace.dist /
                (mine.size / mine.subscene.size);

        res.subTrace.hit = res.subTrace.hit && isMinecraftNoise(mine, res, orig, dir);
    }

    res.hit = res.globalTrace.hit && res.subTrace.hit;

    return res;
}

vec3 renderMinecraftSubscene(
        Minecraft mine, TraceSubsceneResult t, vec3 orig, vec3 dir)
{
    vec3 minePos = toMinePos(mine, t.globalTrace, orig, dir);
    return renderMinecraftGrass(mine, t.subTrace, minePos, dir);
}

// Minecraft ^==================================================================

// Cylinder figure =============================================================

struct Cylinder {
    float radius;
    float height;
    vec3  center;
    vec3  color;
};

float traceCylinderPlane(Cylinder cyl, vec3 normal, vec3 orig, vec3 dir)
{
    float d = cyl.height + cyl.center.y;
    float t = -(dot(normal, orig) + d) / dot(normal, dir);
    if (t < 0.0) {
        return INF;
    }

    vec3 pos = orig + t * dir;
    if (length(pos.xz - cyl.center.xz) > cyl.radius) {
        return INF;
    }

    return t;
}

vec3 calcCylSideNormal(Cylinder cyl, float dist, vec3 orig, vec3 dir)
{
    vec3 pos = orig + dist * dir;
    vec3 normal = normalize(vec3(pos.xz, 0.0));
    return normal;
}

float traceCylinderSide(Cylinder cyl, vec3 orig, vec3 dir)
{
    // length((orig + t * dir).xy) = cyl.radius
    // t = ?

    float a = sqr(length(dir.xz));
    float b = 2.0 * dot(dir.xz, orig.xz);
    float c = sqr(length(orig.xz)) - sqr(cyl.radius);
    float D = sqr(b) - 4.0 * a * c;
    if (D < 0.0) {
        return INF;
    }

    float t1 = (-b + sqrt(D)) / (2.0 * a);
    if (t1 < 0.0) {
        t1 = INF;
    }

    float t2 = (-b - sqrt(D)) / (2.0 * a);
    if (t2 < 0.0) {
        t2 = INF;
    }

    float t = min(t1, t2);
    vec3 pos = orig + t * dir;
    if ((pos.y > cyl.center.y + cyl.height) ||
             (pos.y < cyl.center.y - cyl.height)) {
        return INF;
    }
    return t;
}

TraceResult traceCylinder(Cylinder cyl, vec3 orig, vec3 dir)
{
    TraceResult res;

    float dist = INF;
    vec3 normal;

    vec3 plane = vec3(0, -1, 0);
    float toPlane = traceCylinderPlane(cyl, plane, orig, dir);
    if (toPlane < dist) {
        dist = toPlane;
        normal = -plane;
    }

    float toSide = traceCylinderSide(cyl, orig, dir);
    if (toSide < dist) {
        dist = toSide;
        normal = calcCylSideNormal(cyl, dist, orig, dir);
    }

    res.dist = dist;
    res.n = normal;
    res.hit = dist >= 0.0 && dist < INF;

    return res;
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
    vec3 orig = vec3(0, 1.3, 3.5);
    orig.xz *= mainRotate(PI / 6.0);

    mat3 camera = mainCamera(orig, lp);
    vec3 dir = camera * normalize(vec3(uv, -1));

    vec3 randVals = vec3(
            rand(float(iFrame)),
            rand(float(iFrame + 5)),
            rand(float(iFrame + 15)));

    const int DIFFUSE = 0;
    const int REFLECTION = 1;
    const int REFRACTION = 2;

    const float GLASS_N = 1.5;
    const float AIR_N = 1.0;
    float nPrev = AIR_N;
    float GLASS_R = sqr(AIR_N - GLASS_N) / sqr(AIR_N + GLASS_N);

    // Light setup =============================================================
    Light lights[LIGHTS_NUM];

    lights[0].pos = vec3(1.8, 1.5, 1.8);
    lights[0].color = vec3(1, 1, 1);
    lights[0].attBase = 15.0;

    lights[1].pos = vec3(-3.5, 0.5, -1.0);
    lights[1].color = vec3(1.1, 0.6, 0.6);
    lights[1].attBase = 10.0;
    // Light setup ^============================================================

    // Figures setup ===========================================================
    Cylinder cyl;
    cyl.radius = 1.3;
    cyl.height = 2.15 + PRECISION;
    cyl.center = vec3(0.0, -3, 0.0);
    cyl.color = vec3(0.9, 1.0, 0.9);

    Cube cube;
    cube.size = 0.85;
    cube.reflectN = GLASS_N;
    cube.reflectR = GLASS_R;
    cube.color = vec3(0.9, 0.7, 1.0);
    // Figures setup ^==========================================================

    // Minecraft setup =========================================================
    Cube mineSubscene;
    mineSubscene.size = 0.7;

    Minecraft mine;
    mine.subscene = mineSubscene;
    mine.size = 15.0;
    mine.maxHeight = 18.0;
    // Minecraft setup ^========================================================


    vec3 colorMult = vec3(1, 1, 1);
    vec3 res = vec3(0.0);

    for (int i = 0; i < 10; i++) {
        float t = INF;
        int materialType = 0;
        vec3 color = vec3(0);
        vec3 normal = vec3(0);
        float nNext = AIR_N;

        TraceResult tCube = traceCube(cube, orig, dir);
        if (tCube.hit && tCube.dist < t) {
            t = tCube.dist;
            normal = tCube.n;

            if (randVals.x < cube.reflectR) {
                materialType = REFLECTION;
            } else {
                colorMult *= cube.color;
                materialType = REFRACTION;
                if (dot(dir, normal) > 0.0) {
                    nNext = AIR_N;
                } else {
                    nNext = cube.reflectN;
                }
            }
        }

        TraceResult tCyl = traceCylinder(cyl, orig, dir);
        if (tCyl.hit && tCyl.dist < t) {
            t = tCyl.dist;
            normal = tCyl.n;
            materialType = DIFFUSE;
            color = cyl.color;
        }

        TraceSubsceneResult tMine = traceMinecraftSubscene(mine, orig, dir);
        if (tMine.hit && tMine.sumDist < t) {
            t = tMine.sumDist;
            materialType = DIFFUSE;
            color = renderMinecraftSubscene(mine, tMine, orig, dir);
            normal = tMine.subTrace.n;
        }

        if (t != INF) {
            vec3 gloalPos = orig + t * dir;

            if (materialType == DIFFUSE) {
                res = calcLight(lights, gloalPos, color, normal) * colorMult;
                break;
            } else if (materialType == REFLECTION) {
                dir = reflect(dir, normal);
                orig = gloalPos + dir * PRECISION;
            } else if (materialType == REFRACTION) {
                RefractionResult ref = calcRefraction(dir, normal, nPrev, nNext);
                dir = ref.newDir;
                orig = gloalPos + dir * 2.0 * PRECISION;

                if (!ref.isReflection) {
                    nPrev = nNext;
                }
            }
        } else {
            res = texture(iChannel1, dir).rgb * colorMult;
            break;
        }
    }

    return res;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec3 col = vec3(0);
    vec2 uv = getUV(fragCoord);

    const int antiAliasing = 8;
    vec2 uvs[antiAliasing];
    uvs[0] = vec2(0.0, 0.0);
    uvs[1] = vec2(1.0, 1.0);
    uvs[2] = vec2(-1.0, -1.0);
    uvs[3] = vec2(-1.0, 1.0);
    uvs[4] = vec2(1.0, -1.0);
    uvs[5] = vec2(0.0, -1.0);
    uvs[6] = vec2(1.0, 0.0);
    uvs[7] = vec2(-1.0, 0.0);

    for (int i = 0; i < antiAliasing; i++) {
        vec2 uvh = uv + uvs[i] / iResolution.xy;
        col += renderMainFrag(uvh, fragCoord);
    }
    col /= float(antiAliasing);

    fragColor = vec4(col, 1.0);
}

// Main ^=======================================================================
