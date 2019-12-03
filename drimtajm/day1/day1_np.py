import numpy as np
import time
mydata = np.loadtxt("input1.txt", dtype=np.int32)

def calcFuel(mass):
    fuel = mass / 3 - 2
    return fuel[fuel > 0]

def calcTotalFuel(mass, total):
    fuel = calcFuel(mass)
    if len(fuel) > 0:
        return calcTotalFuel(fuel, total+np.sum(fuel))
    else:
        return total

start_t = time.time()
print calcTotalFuel(mydata, 0)
stop_t = time.time()
print stop_t - start_t
