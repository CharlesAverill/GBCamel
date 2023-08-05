import sys 


def info(op):
    op = int(op, base=16)
    return ((0b11000000 & op) >> 6, (0b00111000 & op) >> 3, 0b111 & op, (0b110 & ((0b111000 & op) >> 3)) >> 1, 0b1 & ((0b111000 & op) >> 3))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        exit("Usage: decode.py OPCODE")
    print(info(sys.argv[1]))
