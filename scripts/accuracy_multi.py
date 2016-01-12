import numpy as np
import chainer
from chainer import function
import chainer.functions as F


class MultiAccuracy(function.Function):

    def __init__(self, n):
        self.n = n

    # Do linear() before calling this method
    def forward_cpu(self, inputs):
        # x.shape => (b_size, 361*n)

        # t.shape => (b_size, n, 1)
        # t.shape => (b_size,    1) (usual)
        x, t = inputs
        reshaped = x.reshape(x.shape[0], self.n, x.shape[1]/self.n)
        planes = np.hsplit(reshaped, self.n)
        targets = np.hsplit(t, self.n)
        accs = [F.accuracy(chainer.Variable(plane.squeeze()), chainer.Variable(t.squeeze()))
                for plane, t in zip(planes, targets)]

        ret = [np.asarray([acc.data]) for acc in accs]
        ret = (reduce(lambda a, b: a + b, ret) / self.n)
        return ret.reshape(()),


def accuracy_multi(y, t, n):
    return MultiAccuracy(n)(y, t)
