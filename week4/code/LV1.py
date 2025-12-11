#!/usr/bin/env python3
"""
Lotka-Volterra Model Implementation
Numerical integration of predator-prey dynamics
"""

import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt
import os

# Create results directory if it doesn't exist
if not os.path.exists('results'):
    os.makedirs('results')

# ============================================================================
# Define the Lotka-Volterra differential equations
# ============================================================================

def dCR_dt(pops, t=0):
    """
    Calculate growth rates of consumer and resource populations.
    
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
    dRdt = r * R - a * R * C
    dCdt = -z * C + e * a * R * C
    
    return np.array([dRdt, dCdt])

# ============================================================================
# Set parameter values
# ============================================================================

r = 1.0      # intrinsic growth rate of resource
a = 0.1      # per-capita search rate and attack success
z = 1.5      # consumer mortality rate
e = 0.75     # consumer efficiency (fraction converting resource to biomass)

# ============================================================================
# Define time vector
# ============================================================================

t = np.linspace(0, 15, 1000)  # integrate from 0 to 15 with 1000 sub-divisions

# ============================================================================
# Set initial conditions
# ============================================================================

R0 = 10      # initial resource density
C0 = 5       # initial consumer density
RC0 = np.array([R0, C0])

# ============================================================================
# Numerical integration
# ============================================================================

pops, infodict = odeint(dCR_dt, RC0, t, full_output=True)

# Check if integration was successful
print(f"Integration successful: {infodict['message']}")

# ============================================================================
# Figure 1: Time series plot
# ============================================================================

f1 = plt.figure()
plt.plot(t, pops[:, 0], 'g-', label='Resource density')
plt.plot(t, pops[:, 1], 'b-', label='Consumer density')
plt.grid()
plt.legend(loc='best')
plt.xlabel('Time')
plt.ylabel('Population density')
plt.title('Consumer-Resource population dynamics')
plt.tight_layout()
plt.savefig('../results/LV_model.pdf')
print("Figure 1 saved to results/LV_model.pdf")
plt.close()

# ============================================================================
# Figure 2: Phase portrait (Consumer vs Resource)
# ============================================================================

f2 = plt.figure()
plt.plot(pops[:, 0], pops[:, 1], 'r-', linewidth=2)
plt.grid()
plt.xlabel('Resource density')
plt.ylabel('Consumer density')
plt.title('Consumer-Resource population dynamics')
plt.tight_layout()
plt.savefig('../results/LV_phaseplane.pdf')
print("Figure 2 saved to results/LV_phaseplane.pdf")
plt.close()

print("\nAll figures saved to the results directory without displaying on screen.")