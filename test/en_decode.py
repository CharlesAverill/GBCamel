import sys 


def decode(op):
    op = int(op, base=16)
    return ((0b11000000 & op) >> 6, (0b00111000 & op) >> 3, 0b111 & op, (0b110 & ((0b111000 & op) >> 3)) >> 1, 0b1 & ((0b111000 & op) >> 3))


def encode(x, y, z):
    x = int(x) & 0b11
    y = int(y) & 0b111
    z = int(z) & 0b111

    return hex((x << 6) | (y << 3) | z)


if __name__ == "__main__":
    if len(sys.argv) == 2:
        print(decode(sys.argv[1]))
    elif len(sys.argv) == 4:
        print(encode(sys.argv[1], sys.argv[2], sys.argv[3]))
    else:
        exit("Usage: decode.py OPCODE")
