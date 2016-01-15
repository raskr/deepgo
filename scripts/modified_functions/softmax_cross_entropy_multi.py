from chainer import function
import chainer.functions as F
from chainer import cuda


class MultiSoftmaxCrossEntropy(function.Function):

    ignore_label = -1

    def __init__(self, use_cudnn, normalize=True):
        # n is the num of channel of input
        self.use_cudnn = use_cudnn
        self.normalize = normalize

    def forward(self, inputs):
        xp = cuda.get_array_module(*inputs)
        x, t = inputs
        n_ch = x.shape[1]
        self.functions = [F.SoftmaxCrossEntropy(self.use_cudnn, self.normalize) for _ in range(n_ch)]

        if getattr(self, 'normalize', True):
            self.count = (t != self.ignore_label).sum()
        else:
            self.count = x.shape[0]

        reshaped = x.reshape(x.shape[0], n_ch, x.shape[2] * x.shape[3])
        planes = xp.hsplit(reshaped, x.shape[1])
        targets = xp.hsplit(t, n_ch)

        losses = [self.functions[i].forward((planes[i].squeeze(), targets[i].squeeze()))[0]
                  for i in range(n_ch)]

        return xp.asarray([(sum(losses) / n_ch)]).reshape(()),

    # grad_outputs may be "xp.array(1.0, dtype=float32)".
    # because loss_variable.backward() initialize error by 1
    def backward(self, inputs, grad_outputs):
        xp = cuda.get_array_module(*inputs)
        x, t = inputs
        n_ch = x.shape[1]
        reshaped = x.reshape(x.shape[0], n_ch, x.shape[2] * x.shape[3])
        planes = xp.hsplit(reshaped, n_ch)
        targets = xp.hsplit(t, n_ch)

        gxs = [self.functions[i].backward((planes[i].squeeze(), targets[i].squeeze()), grad_outputs)[0]
               for i in range(n_ch)]
        # concat arrays
        ret = reduce(lambda a, b: xp.hstack((a, b)), gxs)
        # delta should be the same shape with that of input
        return ret.reshape(x.shape), None


def softmax_cross_entropy_multi(x, t, use_cudnn=True, normalize=True):
    return MultiSoftmaxCrossEntropy(use_cudnn)(x, t)
