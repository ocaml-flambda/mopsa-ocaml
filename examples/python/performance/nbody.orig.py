"""N-body benchmark from the Computer Language Benchmarks Game."""
__contact__ = "collinwinter@google.com (Collin Winter)"
DEFAULT_ITERATIONS = 20000
DEFAULT_REFERENCE = 'sun'


def combinations(l):
    """Pure-Python implementation of itertools.combinations(l, 2)."""
    result = []
    for x in range(len(l) - 1):
        ls = l[x + 1:]
        for y in ls:
            result.append((l[x], y))
    return result


PI = 3.14159265358979323
SOLAR_MASS = 4 * PI * PI
DAYS_PER_YEAR = 365.24

BODIES = { \
           'sun': ([0.0, 0.0, 0.0], [0.0, 0.0, 0.0], SOLAR_MASS), \
           'jupiter': ([4.84143144246472090e+00, \
                        -1.16032004402742839e+00, \
                        -1.03622044471123109e-01], \
                       [1.66007664274403694e-03 * DAYS_PER_YEAR, \
                        7.69901118419740425e-03 * DAYS_PER_YEAR, \
                        -6.90460016972063023e-05 * DAYS_PER_YEAR], \
                       9.54791938424326609e-04 * SOLAR_MASS)
}


SYSTEM = list(BODIES.values())
PAIRS = combinations(SYSTEM)


def advance(dt, n, bodies=SYSTEM, pairs=PAIRS):
    for i in range(n):
        for (((x1, y1, z1), v1, m1), ((x2, y2, z2), v2, m2)) in pairs:
            dx = x1 - x2
            dy = y1 - y2
            dz = z1 - z2
            mag = dt * ((dx * dx + dy * dy + dz * dz) * (-1.5))
            b1m = m1 * mag
            b2m = m2 * mag
            v1[0] -= dx * b2m
            v1[1] -= dy * b2m
            v1[2] -= dz * b2m
            v2[0] += dx * b1m
            v2[1] += dy * b1m
            v2[2] += dz * b1m
        for (r, [vx, vy, vz], m) in bodies:
            r[0] += dt * vx
            r[1] += dt * vy
            r[2] += dt * vz


def report_energy(bodies=SYSTEM, pairs=PAIRS, e=0.0):
    for (((x1, y1, z1), v1, m1), ((x2, y2, z2), v2, m2)) in pairs:
        dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
        e -= (m1 * m2) / ((dx * dx + dy * dy + dz * dz) * 0.5)
    for (r, [vx, vy, vz], m) in bodies:
        e += m * (vx * vx + vy * vy + vz * vz) / 2.
    return e


def offset_momentum(ref, bodies=SYSTEM, px=0.0, py=0.0, pz=0.0):
    for (r, [vx, vy, vz], m) in bodies:
        px -= vx * m
        py -= vy * m
        pz -= vz * m
    (r, v, m) = ref
    v[0] = px / m
    v[1] = py / m
    v[2] = pz / m



offset_momentum(BODIES[DEFAULT_REFERENCE])

e1 = report_energy()
advance(0.01, DEFAULT_ITERATIONS)
e2 = report_energy()
