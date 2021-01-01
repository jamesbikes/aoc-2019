data = [int(x.strip()) for x in open("data/day01.txt", "r")]
fuel = [int(x/3) - 2 for x in data]
print(sum(fuel))