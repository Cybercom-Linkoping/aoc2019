import copy

def compute(op, term1, term2):
    if (op == 1):
        return term1 + term2
    elif (op == 2):
        return term1 * term2

def runintcode(opcodes):
    position = 0
    while opcodes[position] != 99:
        term1 = opcodes[opcodes[position+1]]
        term2 = opcodes[opcodes[position+2]]
        result = compute(opcodes[position], term1, term2)
        opcodes[opcodes[position+3]] = result
        position += 4
    return opcodes

with open("input0.txt") as f:
    opcodes0 = [int(x) for x in f.readline().split(",")]
opcodes = copy.copy(opcodes0)
opcodes[1] = 12
opcodes[2] = 2
print runintcode(opcodes)[0]
noun = 0
verb = -1
targetcode = 19690720
actualcode = 0
while (targetcode != actualcode):
    if (verb == 99):
        noun += 1
        verb = 0
    else:
        verb += 1
    opcodes = copy.copy(opcodes0)
    opcodes[1] = noun
    opcodes[2] = verb
    actualcode = runintcode(opcodes)[0]
print 100*noun+verb
