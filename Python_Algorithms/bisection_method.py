# Example 2.0.1
"""
This program uses the bisection method to find the root
 of f(x) = exp(x)∗ln(x)−x∗x = 0.
"""
import numpy as np
import pylab

tolerance = 1.0e-8

def f(x):
    f = np.exp(x)*np.log(x)-x*x
    return f
# Get the initial guesses
a, b = 1.0, 2.0

dx = abs(b-a)

while dx > tolerance:
    x = (a+b)/2.0
    if (f(a)*f(x)) < 0:
        b = x
    else:
        a = x
    dx = abs(b-a)

print('Found f(x) = 0 at x = %.8f +/- %0.8f' % (x, tolerance))

t = np.arange(0, 3, 3/100)
pylab.plot(t, f(t))
pylab.scatter(x, 0)
pylab.title("f(x) = exp(x)*log(x)-x*x")
pylab.grid()
pylab.show()