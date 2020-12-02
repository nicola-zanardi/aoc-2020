with open("01.txt") as f:
    inputs = [int(x.strip()) for x in f.readlines()]

print({a + b: a * b for i, a in enumerate(inputs) for b in inputs[i + 1 : -1]}[2020])

print(
    {
        a + b + c: a * b * c
        for i, a in enumerate(inputs)
        for j, b in enumerate(inputs)
        for k, c in enumerate(inputs)
        if i < j and j < k
    }[2020]
)
