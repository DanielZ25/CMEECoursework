import numpy as np
import matplotlib.pyplot as plt

Z0, A, phi = 50, 20, 0
t = np.linspace(0, 24, 1000)  # months
omega = 2*np.pi/12

Z1 = Z0 + A*np.cos(omega*t + phi)
Z2 = Z0 + A*np.cos(2*omega*t + phi)

plt.figure()
plt.plot(t, Z1, label="omega")
plt.plot(t, Z2, label="2*omega")
plt.xlabel("t (months)")
plt.ylabel("Z(t)")
plt.legend()
plt.show()