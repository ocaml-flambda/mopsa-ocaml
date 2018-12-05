import mopsa

#pi = 3.141592653589793
#e  = 2.718281828459045
#tau = 6.283185307179586
#inf = float('inf')
#nan = float('nan')

def sin(x): return mopsa.random_float(-1, 1)
def cos(x): return mopsa.random_float(-1, 1)

def acos(x): return mopsa.random_float(0, pi) # -1 <= x <= 1
def asin(x): return mopsa.random_float(-pi/2, pi/2) # -1 <= x <= 1

def cosh(x): return mopsa.random_float() # actually, [1; +\infty)
def sinh(x): return mopsa.random_float()

def acosh(x): return mopsa.random_float() # [0; +infty), but x >= 1
def asinh(x): return mopsa.random_float()

def tanh(x): return mopsa.random_float(-1, 1)
def atanh(x): return mopsa.random_float() # -1 < x < 1

def tan(x): return mopsa.random_float() # x != k*pi+pi/2
def atan(x): return mopsa.random_float(-pi/2, pi/2)
def atan2(y, x): return atan(y/x)

def degrees(x): return x * 180 / pi
def radians(x): return x * pi / 180

def ceil(x): pass
def copysign(x, y): pass
def fabs(x): pass
def factorial(x): pass
def floor(x): pass
def erf(x): pass
def erfc(x): pass
def exp(x): pass
def expm1(x): pass
def fmod(x, n): pass
def frexp(x): pass
def fsum(x): pass
def gamma(x): pass
def gcd(x, y): pass
def hypot(x, y): pass
def isclose(a, b): pass
def isfinite(x): pass
def isinf(x): pass
def isnan(x): pass
def ldexp(x, i): pass
def lgamma(x): pass
def log(x): pass
def log10(x): pass
def log1p(x): pass
def log2(x): pass
def modf(x): pass
def pow(x, y): pass
def trunc(x): pass





@mopsa.builtin("math.sqrt")
def sqrt(x): pass
