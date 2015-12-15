import argparse
import os

import six
import chainer.functions as F
import chainer
from chainer import cuda, optimizers

from data import Data

parser = argparse.ArgumentParser(description='train Go')
parser.add_argument('--gpu', '-g', default=-1, type=int, help='GPU ID (negative value indicates CPU)')
args = parser.parse_args()
use_gpu = args.gpu >= 0

if use_gpu:
    cuda.check_cuda_available()

base = os.path.dirname(os.path.abspath(__file__))
name = os.path.normpath(os.path.join(base, '../deepgo.db'))

# data provider
data = Data(use_gpu=use_gpu,
            db_path=name,
            b_size=128,
            n_ch=24,
            n_train_data=40000,
            n_test_data=8000,
            n_epoch=3)

# Prepare data set
model = chainer.FunctionSet(
    conv1=F.Convolution2D(in_channels=data.n_ch, out_channels=20, ksize=5, pad=2),
    conv2=F.Convolution2D(in_channels=20, out_channels=20, ksize=5, pad=2),
    conv3=F.Convolution2D(in_channels=20, out_channels=1, ksize=5, pad=2),
    l1=F.Linear(361, 361)
)


if use_gpu:
    cuda.get_device(0).use()
    model.to_gpu()


def forward(x_batch, y_batch, invalid_batch=None):
    x, t = chainer.Variable(x_batch), chainer.Variable(y_batch)
    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    h = F.relu(model.conv3(h))
    y = y_test = model.l1(h)
    if invalid_batch is not None:
        y_test = F.softmax(y)
        y_test = (y_test.data - invalid_batch).clip(0)
        y_test = F.softmax(chainer.Variable(y_test))

    return F.softmax_cross_entropy(y, t), F.accuracy(y_test, t)


def train():
    optimizer = optimizers.Adam()
    optimizer.setup(model)
    for epoch in six.moves.range(1, data.n_epoch + 1):
        print('epoch', epoch, '(', data.n_mb_train, 'mini batches )')

        sum_accuracy = 0
        sum_loss = 0
        index = 0
        for i in data.mb_indices(True):
            print('mini batch', index)
            index += 1
            x_batch, y_batch = data(True, i)

            optimizer.zero_grads()
            loss, acc = forward(x_batch, y_batch)
            loss.backward()
            optimizer.update()

            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)

        print('train mean loss={}, accuracy={}'.format(sum_loss / data.n_train_data, sum_accuracy / data.n_train_data))

        # evaluation (test)
        sum_accuracy = 0
        sum_loss = 0
        for i in data.mb_indices(False):
            x_batch, y_batch, invalid_batch = data(False, i)

            loss, acc = forward(x_batch, y_batch, invalid_batch)
            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)

        print('test mean loss={}, accuracy={}'.format(sum_loss / data.n_test_data, sum_accuracy / data.n_test_data))


def save_net(color):
    print("save network...")
    six.moves.cPickle.dump(model.to_cpu(), open("{}.pkl".format("../" + color), "wb"), -1)


train()
save_net('white')
