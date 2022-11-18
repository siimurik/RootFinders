import sympy as sp
import numpy as np
import pylab
"""
Uses Newton's method to find a value x near ""guess
foe which f(x) = 0, to within the tolarance given.
"""
tol = 1.0e-8
dx = 2 * tol
x  = 1

xi = sp.symbols('xi')
expr = sp.exp(xi)*sp.log(xi)-xi*xi
print(sp.diff(expr,xi))

def f(x):
    f = np.exp(x)*np.log(x)-x*x
    return f

def df(x):
    df = -2*x + np.exp(x)*np.log(x) + np.exp(x)/x
    return df

while dx > tol:
    x1 = x - f(x)/df(x)
    dx = abs(x-x1)
    x = x1
print(x)

t = np.linspace(1.2, 2, 100)
x0 = np.zeros(len(t))
pylab.plot(t, f(t))
pylab.plot(t, x0, color="k")
pylab.title('f(x) = exp(x)*log(x)-x*x')
pylab.scatter(x, 0)
pylab.grid()
pylab.show()