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
        reshaped = x.reshape(x.shape[0], self.n, x.shape[1] / self.n)
        # print('bbbbbbbbbbbbb')
        # print(reshaped.shape)
        planes = xp.hsplit(reshaped, self.n)
        # for p in planes:
        #     print(p.squeeze().shape)
        planes = [self.softmax.forward((p.squeeze(),))[0] for p in planes]
        res = reduce(lambda a, b: xp.hstack((a, b)), planes)
        # print('aaaaaaaaa')
        # print(res.shape)
        return res,


def softmax_multi(x, n, use_cudnn=True):
    return MultiSoftmax(use_cudnn, n)(x)
