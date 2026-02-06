import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp

# Parameters for the consumer–resource model
r = 1.0    # intrinsic growth rate of resource (time^-1)
K = 100    # carrying capacity of resource (abundance)
a = 0.02   # consumption rate
w = 0.1   # conversion efficiency parameter (w = e*a in the linear-response case)
m = 0.2    # mortality rate of consumer (time^-1)

# Time parameters
t_start = 0
t_end = 500
t_eval = np.linspace(t_start, t_end, 1000)

# Initial conditions
initial_conditions = [50, 10]  # [Resource, Consumer]

def resource_consumer_model(t, y):
    """ODE system for a simple consumer–resource model."""
    R, C = y
    dR_dt = r * R * (1 - R / K) - a * R * C
    dC_dt = w * R * C - m * C
    return [dR_dt, dC_dt]

solution = solve_ivp(
    resource_consumer_model,
    [t_start, t_end],
    initial_conditions,
    t_eval=t_eval,
    method='RK45',
)

time = solution.t
resource = solution.y[0]
consumer = solution.y[1]

plt.figure(figsize=(10, 6))
plt.plot(time, resource, 'g-', linewidth=3, label='Resource (R)')
plt.plot(time, consumer, 'b-', linewidth=3, label='Consumer (C)')
plt.xlabel('Time')
plt.ylabel('Population density')
plt.title('Dynamics of a consumer–resource model')
plt.legend()
plt.grid(True, alpha=0.3)
plt.show()