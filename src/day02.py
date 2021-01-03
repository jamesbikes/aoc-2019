def run(mem):
    pos = 0
    while True:
        opcode = mem[pos]
        if opcode == 99:
            return
        elif opcode == 1:
            mem[mem[pos+3]] = mem[mem[pos+1]] + mem[mem[pos+2]]
            pos += 4
        elif opcode == 2:
            mem[mem[pos+3]] = mem[mem[pos+1]] * mem[mem[pos+2]]
            pos += 4
        else:
            raise Exception

data = open("data/day02.txt", 'r').read()
memory = [int(x) for x in data.split(",")]

# part 1
fixed = memory.copy()
fixed[1:3] = 12,2
run(fixed)
print(fixed[0])

# part 2
for i in range(100):
    for j in range(100):
        m = memory.copy()
        m[1:3] = i, j
        run(m)
        if m[0] == 19690720:
            print(i*100 + j)
            break
