import numpy as np
from chainer import function
import chainer.functions as F


class MultiAccuracy(function.Function):

    def __init__(self, n):
        self.n = n

    # Do linear() before calling this method
    def forward_cpu(self, inputs):
        x, ts = inputs
        planes = np.hsplit(x, self.n)
        accs = [F.accuracy(plane, t) for plane, t in zip(planes, ts)]

        return sum(accs) / self.n


def accuracy_multi(y, t, n):
    return MultiAccuracy(n)(y, t)
