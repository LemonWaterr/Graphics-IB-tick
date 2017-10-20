#version 330

uniform vec2 resolution;
uniform float currentTime;
uniform vec3 camPos;
uniform vec3 camDir;
uniform vec3 camUp;
uniform sampler2D tex;
uniform bool showStepDepth;

in vec3 pos;

out vec3 color;

#define PI 3.1415926535897932384626433832795
#define RENDER_DEPTH 800
#define CLOSE_ENOUGH 0.00001

#define BACKGROUND -1
#define BALL 0
#define BASE 1

#define GRADIENT(pt, func) vec3( \
    func(vec3(pt.x + 0.0001, pt.y, pt.z)) - func(vec3(pt.x - 0.0001, pt.y, pt.z)), \
    func(vec3(pt.x, pt.y + 0.0001, pt.z)) - func(vec3(pt.x, pt.y - 0.0001, pt.z)), \
    func(vec3(pt.x, pt.y, pt.z + 0.0001)) - func(vec3(pt.x, pt.y, pt.z - 0.0001)))

#define M_PI 3.1415926535897932384626433832795

const vec3 LIGHT_POS[] = vec3[](vec3(5, 18, 10));

///////////////////////////////////////////////////////////////////////////////

vec3 getBackground(vec3 dir) {
  float u = 0.5 + atan(dir.z, -dir.x) / (2 * PI);
  float v = 0.5 - asin(dir.y) / PI;
  vec4 texColor = texture(tex, vec2(u, v));
  return texColor.rgb;
}

vec3 getRayDir() {
  vec3 xAxis = normalize(cross(camDir, camUp));
  return normalize(pos.x * (resolution.x / resolution.y) * xAxis + pos.y * camUp + 5 * camDir);
}

///////////////////////////////////////////////////////////////////////////////

float sphere(vec3 pt) {
  return length(pt) - 1;
}

float sphereTrans(vec3 p, vec3 trans){
	return sphere(p - trans);
}

float cube(vec3 p) {
	vec3 d = abs(p) - vec3(1); // 1 = radius
	return min(max(d.x, max(d.y, d.z)), 0.0) + length(max(d, 0.0));
}

float cubeTrans(vec3 p, vec3 trans){
	return cube(p - trans);
}

float smin(float a, float b) {
	float k = 0.2;
	float h = clamp(0.5 + 0.5 * (b - a) / k, 0, 1);
	return mix(b, a, h) - k * h * (1 - h);
}

float plane(vec3 p){
	return abs(p.y + 1);
}

float torus(vec3 p, vec2 t) {
  vec2 q = vec2(length(p.xz) - t.x, p.y);
  return length(q) - t.y;
}

float torusTransRotateX(vec3 p, vec2 t, vec3 trans, float angle){
	mat4 T = mat4(
			vec4(1,0,0,trans.x),
			vec4(0,1,0,trans.y),
			vec4(0,0,1,trans.z),
			vec4(0,0,0,1));
	mat4 R = mat4(
			vec4(1,0,0,0),
			vec4(0,cos(angle),-sin(angle),0),
			vec4(0,sin(angle),cos(angle),0),
			vec4(0,0,0,1));
	mat4 invRT = inverse(R * T);
	return torus((vec4(p, 1) * invRT).xyz, t);
}

float torusTransRotateXRepeatXZ(vec3 p, vec2 t, vec3 trans, float angle, int r){
	mat4 T = mat4(
			vec4(1,0,0,trans.x),
			vec4(0,1,0,trans.y),
			vec4(0,0,1,trans.z),
			vec4(0,0,0,1));
	mat4 R = mat4(
			vec4(1,0,0,0),
			vec4(0,cos(angle),-sin(angle),0),
			vec4(0,sin(angle),cos(angle),0),
			vec4(0,0,0,1));
	mat4 invRT = inverse(R * T);
	vec3 final = (vec4(mod(p.x - 4 + r/2, r) - r/2, p.y, mod(p.z - 8, r) - r/2, 1) * invRT).xyz;
	return torus(final, t);
}

float torusTransRotateZRepeatXZ(vec3 p, vec2 t, vec3 trans, float angle, int r){
	mat4 T = mat4(
			vec4(1,0,0,trans.x),
			vec4(0,1,0,trans.y),
			vec4(0,0,1,trans.z),
			vec4(0,0,0,1));
	mat4 R = mat4(
			vec4(cos(angle),-sin(angle),0,0),
			vec4(sin(angle),cos(angle),0,0),
			vec4(0,0,1,0),
			vec4(0,0,0,1));
	mat4 invRT = inverse(R * T);
	vec3 final = (vec4(mod(p.x - 4, r) - r/2, p.y, mod(p.z - 4, r) - r/2, 1) * invRT).xyz;
	return torus(final, t);
}

float getSDF(vec3 p){
	float torus1 = torusTransRotateX(p, vec2(3,1), vec3(0,3,0), 0);
	return torus1;
}

float getSDFWithPlane(vec3 p){
	float plane = plane(p);
	float sdf = getSDF(p);
	return min(plane, sdf);
}

//////////////////////////////////////////////////////////////////////////////

vec3 getNormal(vec3 pt) {
	return ((pt.y + 1 < 0.001) ? vec3(0,1,0) : normalize(GRADIENT(pt, getSDFWithPlane)));
}

vec3 getColor(vec3 pt) {
  vec3 color = vec3(1);
  if(pt.y + 1 < 0.001){
	  if(mod(floor(getSDF(pt)), 5) == 4 && mod(getSDF(pt), 1) >= 0.75){
		  color = vec3(0);
	  }else{
		  color = mix(vec3(0.4, 1, 0.4), vec3(0.4, 0.4, 1), mod(getSDF(pt), 1));
	  }
  }
  return color;
}

///////////////////////////////////////////////////////////////////////////////
float shadow(vec3 pt) {
	vec3 lightDir = normalize(LIGHT_POS[0] - pt);
	float kd = 1;
	int step = 0;
	for (float t = 0.1;t < length(LIGHT_POS[0] - pt) && step < RENDER_DEPTH && kd > 0.001; ){
		float d = abs(getSDF(pt + t * lightDir)); // d = length(current point to an object), t = length(starting point to current point)
		//if hit an object
		if (d < 0.001) {
			kd = 0; //hard shadow
		//if didn't hit an object
		} else {
			kd = min(kd, 16 * d / t);
		}
		t += d;
		step++;
	}
	return kd;
}

float shade(vec3 eye, vec3 pt, vec3 n) {
  float val = 0;
  float spec = 0;

  for (int i = 0; i < LIGHT_POS.length(); i++) {
    vec3 l = LIGHT_POS[i] - pt;
    //diffuse
    val += max(dot(n, normalize(l)), 0);

    //specular
    vec3 r = normalize(n * (2 * dot(n,l)) - l);
    vec3 c = normalize(eye - pt);
    val += pow(max(dot(r,c), 0), 256);
  }

  //val *= shadow(pt);
  val += 0.1; //Ambient
  return val;
}

vec3 illuminate(vec3 camPos, vec3 rayDir, vec3 pt) {
  vec3 c, n;
  n = getNormal(pt);
  c = getColor(pt);
  return shade(camPos, pt, n) * c;
}

///////////////////////////////////////////////////////////////////////////////

vec3 raymarch(vec3 camPos, vec3 rayDir) {
  int step = 0;
  float t = 0;

  for (float d = 1000; step < RENDER_DEPTH && abs(d) > CLOSE_ENOUGH; t += abs(d)) {
	d = getSDFWithPlane(camPos + t * rayDir);
    step++;
  }

  if (step == RENDER_DEPTH) {
    return getBackground(rayDir);
  } else if (showStepDepth) {
    return vec3(float(step) / RENDER_DEPTH);
  } else {
    return illuminate(camPos, rayDir, camPos + t * rayDir);
  }
}

///////////////////////////////////////////////////////////////////////////////

void main() {
  color = raymarch(camPos, getRayDir());
}
