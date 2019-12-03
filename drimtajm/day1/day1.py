import time

def calcFuel(mass):
    fuel = mass / 3 - 2
    if fuel > 0:
        return fuel
    return 0

def calcTotalFuel():
    with open("input1.txt", "rt") as f:
        total = 0
        for line in f:
            mass = int(line)
            while (mass > 0):
                mass = calcFuel(mass)
                total += mass
    return total

start_t = time.time()
print calcTotalFuel()
stop_t = time.time()
print stop_t - start_t
