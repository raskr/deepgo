import chainer
import numpy as np
from chainer import function
import chainer.functions as F


class MultiSoftmaxCrossEntropy(function.Function):

    def __init__(self, use_cudnn, n):
        self.n = n
        self.use_cudnn = use_cudnn

    # Do linear() before calling this method
    def forward_cpu(self, inputs):
        x, ts = inputs
        self.losses = [F.softmax_cross_entropy(plane, t)for plane, t in zip(np.hsplit(x, self.n), ts)]
        losses_ret = [float(loss.data) for loss in self.losses]
        return reduce(lambda a, b: a + b, losses_ret) / self.n

    def backward_cpu(self, inputs, grad_outputs):
        gxs = [loss.backward_cpu(inputs, grad_outputs) for loss in self.losses]
        data = [gx.data for gx in gxs]
        return chainer.Variable(reduce(lambda a, b: np.concatenate(a, b), data)), None


def softmax_cross_entropy_multi(x, n, use_cudnn=True):
    """Channelwise softmax function.

    This function computes its softmax along the second axis. Let
    :math:`x = (x_1, x_2, \\dots, x_d)^{\\top}` be the d dimensional index
    array and :math:`f(x)` be the d dimensional input array. For each index
    :math:`x` of the input array :math:`f(x)`, it computes the probability
    :math:`p(x)` defined as
    :math:`p(x) = {\\exp(f(x)) \\over \\sum_{x_2} \\exp(f(x))}`.

    Args:
        x (~chainer.Variable): Input variable.
        use_cudnn (bool): If True and CuDNN is enabled, then this function uses
            CuDNN as the core implementation.

    Returns:
        ~chainer.Variable: Output variable.

    """
    return MultiSoftmaxCrossEntropy(use_cudnn, n)(x)
