import sympy as sp
t, beta = sp.symbols('t beta', positive=True, real=True)
sp.exp(sp.I*beta*t).expand(complex=True)