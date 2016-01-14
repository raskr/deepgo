from chainer import function
import chainer.functions as F
from chainer import cuda


class MultiSoftmaxCrossEntropy(function.Function):

    ignore_label = -1

    def __init__(self, use_cudnn, n, normalize=True):
        # n is the num of channel of input
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
            # count is the batch size
            self.count = x.shape[0]

        self.in_shape = x.shape[0], self.n, x.shape[2], x.shape[3]
        reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
        planes = xp.hsplit(reshaped, self.n)
        targets = xp.hsplit(t, self.n)

        losses = [self.functions[i].forward((p.squeeze(), ta.squeeze()))[0]
                  for p, ta, i in zip(planes, targets, range(self.n))]

        return xp.asarray([(sum(losses) / self.n)]).reshape(()),

    # grad_outputs may be "xp.array(1.0, dtype=float32)".
    # because loss_variable.backward() initialize error by 1
    def backward(self, inputs, grad_outputs):
        xp = cuda.get_array_module(*inputs)
        x, t = inputs
        reshaped = x.reshape(x.shape[0], self.n, x.shape[2] * x.shape[3])
        planes = xp.hsplit(reshaped, self.n)
        targets = xp.hsplit(t, self.n)

        gxs = [self.functions[i].backward((p.squeeze(), ta.squeeze()), grad_outputs)[0]
               for p, ta, i in zip(planes, targets, range(self.n))]
        # concat arrays
        ret = reduce(lambda a, b: xp.hstack((a, b)), gxs)
        # delta should be the same shape with that of input
        return ret.reshape(self.in_shape), None


def softmax_cross_entropy_multi(x, t, n, use_cudnn=True, normalize=True):
    return MultiSoftmaxCrossEntropy(use_cudnn, n)(x, t)
