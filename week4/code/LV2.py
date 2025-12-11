#!/usr/bin/env python3
"""
Modified Lotka-Volterra Model with Resource Density Dependence
Numerical integration of predator-prey dynamics with carrying capacity
"""

import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt
import sys
import os

# Create results directory if it doesn't exist
if not os.path.exists('results'):
    os.makedirs('results')

# ============================================================================
# Parse command-line arguments
# ============================================================================

if len(sys.argv) != 6:
    print("Usage: python LV2.py r a z e K")
    print("  r: intrinsic growth rate of resource")
    print("  a: per-capita search rate")
    print("  z: consumer mortality rate")
    print("  e: consumer efficiency")
    print("  K: resource carrying capacity")
    sys.exit(1)

r = float(sys.argv[1])
a = float(sys.argv[2])
z = float(sys.argv[3])
e = float(sys.argv[4])
K = float(sys.argv[5])

# ============================================================================
# Define the modified Lotka-Volterra differential equations
# with resource density dependence
# ============================================================================

def dCR_dt(pops, t=0):
    """
    Calculate growth rates of consumer and resource populations
    with logistic growth for the resource (density dependence).
    
    Parameters:
    -----------
    pops : array-like
        Array containing [R, C] where R = resource, C = consumer
    t : float
        Time (required for odeint but not used in equations)
    
    Returns:
    --------
    numpy.array
        Array containing [dR/dt, dC/dt]
    """
    R = pops[0]
    C = pops[1]
    
    # Modified Lotka-Volterra with logistic growth for resource
    dRdt = r * R * (1 - R / K) - a * R * C
    dCdt = -z * C + e * a * R * C
    
    return np.array([dRdt, dCdt])

# ============================================================================
# Define time vector (can be adjusted)
# ============================================================================

t = np.linspace(0, 100, 2000)  # integrate from 0 to 100 with 2000 sub-divisions

# ============================================================================
# Set initial conditions
# ============================================================================

R0 = 5       # initial resource density
C0 = 2       # initial consumer density
RC0 = np.array([R0, C0])

# ============================================================================
# Numerical integration
# ============================================================================

pops, infodict = odeint(dCR_dt, RC0, t, full_output=True)

# ============================================================================
# Print final population values
# ============================================================================

final_R = pops[-1, 0]
final_C = pops[-1, 1]

print(f"=== Modified Lotka-Volterra Model with Density Dependence ===")
print(f"Parameters: r={r}, a={a}, z={z}, e={e}, K={K}")
print(f"Integration successful: {infodict['message']}")
print(f"\nFinal population values at t={t[-1]}:")
print(f"  Resource density (R): {final_R:.6f}")
print(f"  Consumer density (C): {final_C:.6f}")

if final_C > 0.001:
    print(f"\n✓ Both predator and prey persist!")
else:
    print(f"\n✗ Consumer population went extinct")

# ============================================================================
# Create figure with time series plot
# ============================================================================

f1 = plt.figure(figsize=(10, 6))
plt.plot(t, pops[:, 0], 'g-', linewidth=2, label='Resource density')
plt.plot(t, pops[:, 1], 'b-', linewidth=2, label='Consumer density')
plt.grid(True, alpha=0.3)
plt.legend(loc='best', fontsize=11)
plt.xlabel('Time', fontsize=12)
plt.ylabel('Population density', fontsize=12)
plt.title(f'Consumer-Resource population dynamics\n(r={r}, a={a}, z={z}, e={e}, K={K})', fontsize=12)
plt.tight_layout()
plt.savefig('../results/LV2_model.pdf')
print(f"\nFigure saved to results/LV2_model.pdf")
plt.close()