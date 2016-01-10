import numpy as np
from chainer import function
import chainer
import chainer.functions as F


class MultiSoftmax(function.Function):

    def __init__(self, use_cudnn, n):
        self.n = n
        self.use_cudnn = use_cudnn

    # Do linear() before calling this method
    def forward_cpu(self, inputs):
        x = inputs
        planes = np.hsplit(x, self.n)
        planes = [F.softmax(plane).data for plane in planes]

        return chainer.Variable(reduce(lambda a, b: np.concatenate(a, b), planes))


def softmax_multi(x, n, use_cudnn=True):
    return MultiSoftmax(use_cudnn, n)(x)
