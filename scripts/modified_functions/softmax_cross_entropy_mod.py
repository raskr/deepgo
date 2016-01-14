import numpy
import six
from chainer.functions import softmax
from chainer import function


class SoftmaxCrossEntropyMod(function.Function):

    ignore_label = -1

    def __init__(self, use_cudnn, normalize=True):
        # n is the num of channel of input
        self.id = id(self)
        self.use_cudnn = use_cudnn
        self.normalize = normalize

    def forward_cpu(self, inputs):
        x, t = inputs
        self.in_shape = x.shape
        x = x.reshape(x.shape[0], x.shape[1], x.shape[2] * x.shape[3])
        self.y, = softmax.Softmax().forward((x,))
        yd = numpy.rollaxis(self.y, 1)
        yd = yd.reshape(len(yd), -1).T

        p = yd[six.moves.range(t.size), numpy.maximum(t.flat, 0)]
        # deal with the case where the SoftmaxCrossEntropy is
        # unpickled from the old version
        if getattr(self, 'normalize', True):
            count = (t != self.ignore_label).sum()
        else:
            count = x.shape[0]
        self.count = count
        # where is this self from ?

        if count == 0:
            return numpy.zeros((), dtype=x.dtype),

        y = (numpy.log(p) * (t.flat != self.ignore_label)).sum(keepdims=True) * (-1.0 / count)
        # print('aaaa {}'.format(y))
        # print('bbbb {}'.format(y.reshape(())))
        # print(y.reshape(()),)
        return y.reshape(()),

    # grad_outputs may be "xp.array(1.0, dtype=float32)".
    # because loss_variable.backward() initialize error by 1
    def backward_cpu(self, inputs, grad_outputs):
        x, t = inputs
        if self.count == 0:
            return numpy.zeros_like(x), None

        gloss = grad_outputs[0]
        n_unit = t.size // t.shape[0]
        if self.y.ndim == 2:
            gx = self.y.copy()
            gx[six.moves.xrange(len(t)), numpy.maximum(t, 0)] -= 1
            gx *= (t != self.ignore_label).reshape((len(t), 1))
        else:
            # in the case where y.ndim is higher than 2,
            # we think that a current implementation is inefficient
            # because it yields two provisional arrays for indexing.
            gx = self.y.copy().reshape(self.y.shape[0], self.y.shape[1], -1)
            fst_index = numpy.arange(t.size) // n_unit
            trd_index = numpy.arange(t.size) % n_unit
            gx[fst_index, numpy.maximum(t.flat, 0), trd_index] -= 1
            gx *= (t != self.ignore_label).reshape((len(t), 1, -1))
            gx = gx.reshape(self.y.shape)

        gx *= gloss / self.count
        # print(gx)
        return gx.reshape(self.in_shape), None


def softmax_cross_entropy_mod(x, t, use_cudnn=True, normalize=True):
    return SoftmaxCrossEntropyMod(use_cudnn)(x, t)
