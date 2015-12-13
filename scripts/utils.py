from math import exp


# convert string consist of channels to float array
def str2floats(string):
    a = list(string)
    lifespans = [exp(-0.1 * int(c)) for c in a[-361:]]
    others = [1.0 if x == '1' else 0.0 for x in a[:-361]]
    others.extend(lifespans)
    return others

