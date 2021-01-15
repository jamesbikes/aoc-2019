import sys

WIDTH = 25
HEIGHT = 6

def chunk(data, n):
    for i in range(0, len(data), n):
        yield data[i:(i+n)]

data = open("data/day08.txt", 'r').read()

# part 1
layers = chunk(data, WIDTH*HEIGHT)
min_zero = sys.maxsize
result = 0

for layer in layers:
    zeros = layer.count('0')
    if zeros < min_zero:
        min_zero = zeros
        result = layer.count('1') * layer.count('2') 

print(result)

# part 2
layers = chunk(data, WIDTH*HEIGHT)
image = list(next(layers))

for layer in layers:
    # print(''.join(image))
    for i, pixel in enumerate(layer):
        if image[i] == '2': image[i] = pixel

for r in chunk(image, WIDTH):
    print(''.join(r).replace('1', '#').replace('0', ' '))
