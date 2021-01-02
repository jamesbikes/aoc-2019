def total_fuel(mass):
    fuel = max(0, int(mass/3) - 2)
    if fuel == 0: return 0
    return fuel + total_fuel(fuel)

data = [int(x.strip()) for x in open("data/day01.txt", "r")]

# part 1
fuel = [int(x/3) - 2 for x in data]
print(sum(fuel))

# part 2
fuel = [total_fuel(x) for x in data]
print(sum(fuel))
