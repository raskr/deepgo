from math import exp


# convert string consist of channels to float array
def str2floats(string):
    lifespans = [exp(-0.1 * float(c)) for c in string[-361:]]
    others = [1.0 if x == '1' else 0.0 for x in string[:-361]]
    others.extend(lifespans)
    return others


def str2floats_simple(string):
    return [1.0 if x == '1' else 0.0 for x in string]
