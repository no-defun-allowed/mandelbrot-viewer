/* -*- mode: opencl -*- */

double scale(uint n, uint size, double low, double high) {
  return (double)n / (double)(size) * (high - low) + low;
}

uint pixel(double x0, double y0, unsigned int maximum_iterations) {
  int iterations = 0;
  double xi = 0, yi = 0, xsq = 0, ysq = 0;
  while (xsq + ysq < 4 && iterations < maximum_iterations) {
    yi = 2 * xi * yi + y0;
    xi = xsq - ysq + x0;
    xsq = xi * xi;
    ysq = yi * yi;
    iterations++;
  }
  return iterations;
}

uchar3 colour(unsigned int iterations, unsigned int maximum_iterations) {
  if (iterations == maximum_iterations)
    return (uchar3){0, 0, 0};
  float it = sqrt((float)iterations);
  float r = 128 + sin(it * 0.35) * 127,
        g = 128 + sin(it * 0.70) * 127,
        b = 128 + sin(it * 1.05) * 127;
  return (uchar3){(uchar)r, (uchar)g, (uchar)b};
}

#define SEGMENTS 256
__kernel void mandel(unsigned int width,
                     unsigned int height,
                     unsigned int maximum_iterations,
                     double x1,
                     double x2,
                     double y1,
                     double y2,
                     __global unsigned int* framebuffer) {
  unsigned int y = get_global_id(0) / SEGMENTS;
  unsigned int xpart = get_global_id(0) % SEGMENTS;
  double xoff = ((x2 - x1) / width) / 2;
  double yoff = ((y2 - y1) / height) / 2;
  for (int x = (width * xpart) / SEGMENTS;
       x < (width * (xpart + 1)) / SEGMENTS;
       x++) {
    double x0 = scale(x, width, x1, x2), y0 = scale(y, height, y1, y2);
    uint iterations = pixel(x0, y0, maximum_iterations);
    uchar3 c = colour(iterations, maximum_iterations);
    framebuffer[y * width + x] = 0xFF << 24 | (unsigned int)(c.x) << 16 | (unsigned int)(c.y) << 8 | c.z;
  }
}
