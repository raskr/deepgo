import numpy as np
from chainer import cuda
from chainer import function
import chainer
import chainer.functions as F


# need not backward
class MultiSoftmax(function.Function):

    softmax = F.Softmax()

    def __init__(self, use_cudnn, n):
        self.n = n
        self.use_cudnn = use_cudnn

    # Do linear() before calling this method
    def forward(self, inputs):
        xp = cuda.get_array_module(*inputs)
        x = inputs[0]
        input_shape = x.shape
        reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
        planes = xp.hsplit(reshaped, self.n)
        planes = [self.softmax.forward((p.squeeze(),)) for p in planes]
        res = reduce(lambda a, b: xp.hstack((a, b)), planes)
        return res.reshape(input_shape),


def softmax_multi(x, n, use_cudnn=True):
    return MultiSoftmax(use_cudnn, n)(x)
