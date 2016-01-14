import numpy as np
from chainer import function
import chainer.functions as F
from chainer import cuda
from chainer.utils import type_check


class MultiSoftmaxCrossEntropy(function.Function):

    ignore_label = -1

    def __init__(self, use_cudnn, n, normalize=True):
        self.n = n
        self.id = id(self)
        self.use_cudnn = use_cudnn
        self.normalize = normalize
        self.functions = [F.SoftmaxCrossEntropy(self.use_cudnn, self.normalize) for _ in range(n)]

    def forward(self, inputs):
        xp = cuda.get_array_module(*inputs)
        x, t = inputs
        if getattr(self, 'normalize', True):
            self.count = (t != self.ignore_label).sum()
        else:
            self.count = x.shape[0]

        self.in_shape = x.shape[0], self.n, x.shape[2], x.shape[3]
        reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
        planes = xp.hsplit(reshaped, self.n)
        targets = xp.hsplit(t, self.n)

        losses = [self.functions[i].forward((p.squeeze(), ta.squeeze()))[0]
                  for p, ta, i in zip(planes, targets, range(self.n))]

        return xp.asarray([(sum(losses) / self.n)]).reshape(()),

    def backward(self, inputs, grad_outputs):
        xp = cuda.get_array_module(*inputs)
        x, t = inputs
        reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
        planes = xp.hsplit(reshaped, self.n)
        targets = xp.hsplit(t, self.n)

        # inputs is None in most case
        gxs = [self.functions[i].backward((p.squeeze(), ta.squeeze()), grad_outputs)[0]
               for p, ta, i in zip(planes, targets, range(self.n))]
        # concat arrays and return
        ret = reduce(lambda a, b: xp.hstack((a, b)), gxs)
        # return ret.reshape(self.in_shape), None
        return ret.reshape(self.in_shape), None

    # def forward_cpu(self, inputs):
    #     # x.shape => (b_size, 361*n)
    #     # t.shape => (b_size, n, 1)
    #     # t.shape => (b_size,    1) (usual)
    #     x, t = inputs
    #
    #     if getattr(self, 'normalize', True):
    #         self.count = (t != self.ignore_label).sum()
    #     else:
    #         self.count = x.shape[0]
    #
    #     self.in_shape = x.shape[0], self.n, x.shape[2], x.shape[3]
    #     reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
    #     planes = np.hsplit(reshaped, self.n)
    #     targets = np.hsplit(t, self.n)
    #
    #     losses = [self.functions[i].forward_cpu((p.squeeze(), ta.squeeze()))[0]
    #               for p, ta, i in zip(planes, targets, range(self.n))]
    #
    #     return np.asarray([(sum(losses) / self.n)]).reshape(()),
    #
    # # inputs = return = tuple of array
    # def backward_cpu(self, inputs, grad_outputs):
    #     x, t = inputs
    #     reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
    #     planes = np.hsplit(reshaped, self.n)
    #     targets = np.hsplit(t, self.n)
    #
    #     # inputs is None in most case
    #     gxs = [self.functions[i].backward_cpu((p.squeeze(), ta.squeeze()), grad_outputs)[0]
    #            for p, ta, i in zip(planes, targets, range(self.n))]
    #     # concat arrays and return
    #     ret = reduce(lambda a, b: np.hstack((a, b)), gxs)
    #     # return ret.reshape(self.in_shape), None
    #     return ret.reshape(self.in_shape), None
    #
    # def forward_gpu(self, inputs):
    #     cupy = cuda.cupy
    #     # x.shape => (b_size, 361*n)
    #     # t.shape => (b_size, n, 1)
    #     # t.shape => (b_size,    1) (usual)
    #     x, t = inputs
    #
    #     if getattr(self, 'normalize', True):
    #         self.count = (t != self.ignore_label).sum()
    #     else:
    #         self.count = x.shape[0]
    #
    #     self.in_shape = x.shape[0], self.n, x.shape[2], x.shape[3]
    #     reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
    #     planes = cupy.hsplit(reshaped, self.n)
    #     targets = cupy.hsplit(t, self.n)
    #
    #     losses = [self.functions[i].forward_cpu((p.squeeze(), ta.squeeze()))[0]
    #               for p, ta, i in zip(planes, targets, range(self.n))]
    #
    #     return cupy.asarray([(sum(losses) / self.n)]).reshape(()),
    #
    # def backward_gpu(self, inputs, grad_outputs):
    #     cupy = cuda.cupy
    #     x, t = inputs
    #     reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
    #     planes = cupy.hsplit(reshaped, self.n)
    #     targets = cupy.hsplit(t, self.n)
    #
    #     # inputs is None in most case
    #     gxs = [self.functions[i].backward_cpu((p.squeeze(), ta.squeeze()), grad_outputs)[0]
    #            for p, ta, i in zip(planes, targets, range(self.n))]
    #     # concat arrays and return
    #     ret = reduce(lambda a, b: cupy.hstack((a, b)), gxs)
    #     # return ret.reshape(self.in_shape), None
    #     return ret.reshape(self.in_shape), None


def softmax_cross_entropy_multi(x, t, n, use_cudnn=True, normalize=True):
    return MultiSoftmaxCrossEntropy(use_cudnn, n)(x, t)
