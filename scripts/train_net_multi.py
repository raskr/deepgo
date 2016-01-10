import argparse
import os

from __init__ import *
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

base_path = os.path.dirname(os.path.abspath(__file__))
db_path = os.path.normpath(os.path.join(base_path, '../deepgo.db'))

# 12481120 -> max
# 10551120 -> omit 1d, 2d
# 10931120 -> not omitd

# data provider (if 39998 sgf => 3898669)
data = Data(use_gpu=use_gpu,
            db_path=db_path,
            b_size=128,
            n_ch=22,
            #n_train_data=12481120,
            #n_test_data=34730,
            # n_train_data=10931120,
            n_train_data=1093,
            n_test_data=3,
            n_epoch=4)

n_out_plane = 1

# Prepare data set
model = chainer.FunctionSet(
        conv1=F.Convolution2D(in_channels=data.n_ch, out_channels=20, ksize=5, pad=2),
        conv2=F.Convolution2D(in_channels=20, out_channels=20, ksize=3, pad=1),
        conv3=F.Convolution2D(in_channels=20, out_channels=n_out_plane, ksize=3, pad=1),
        l1=F.Linear(361*n_out_plane, 361*n_out_plane)
)


if use_gpu:
    cuda.get_device(0).use()
    model.to_gpu()


def forward(x_batch, invalid_batch, *y_batches):

    x = chainer.Variable(x_batch)
    ts = [chainer.Variable(y_batch) for y_batch in y_batches]

    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    h = F.relu(model.conv3(h))

    y = y_test = model.l1(h)

    if invalid_batch is not None:
        y_test = softmax_multi(y_test, n_out_plane)
        y_test = (y_test.data - invalid_batch).clip(0, 1)
        y_test = chainer.Variable(y_test)

    losses = softmax_cross_entropy_multi(y, ts)
    accuracies = accuracy_multi(y_test, ts)

    return losses, accuracies


def train():
    optimizer = optimizers.Adam()
    optimizer.setup(model)
    for epoch in six.moves.range(1, data.n_epoch + 1):
        sum_accuracy = sum_loss = mb_count = 0
        for i in data.mb_indices(True):
            print('epoch: {} mini batch: {} of {}'.format(epoch, mb_count, data.n_mb_train))
            mb_count += 1
            x_batch, y_batch = data(True, i)

            optimizer.zero_grads()
            loss, acc = forward(x_batch, y_batch, None)
            loss.backward()
            optimizer.update()

            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)

        print('train mean loss = {}, accuracy = {}'.format(sum_loss / data.n_train_data, sum_accuracy / data.n_train_data))
        with open('result.txt', 'a+') as f:
            f.write(('train epoch {} train mean loss = {}, accuracy = {}\n'.format(epoch, sum_loss / data.n_train_data, sum_accuracy / data.n_train_data)))

        # evaluation (test)
        sum_accuracy = sum_loss = mb_count = 0
        for i in data.mb_indices(False):
            print('test mini batch: {} of {}'.format(mb_count, data.n_mb_test))
            mb_count += 1
            x_batch, y_batch, invalid_batch = data(False, i)

            loss, acc = forward(x_batch, y_batch, invalid_batch)
            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)

        print('test mean loss = {}, accuracy = {}'.format(sum_loss / data.n_test_data, sum_accuracy / data.n_test_data))
        with open('result.txt', 'a+') as f:
            f.write(('test mean loss = {}, accuracy = {}\n'.format(sum_loss / data.n_test_data, sum_accuracy / data.n_test_data)))

        save_net('white_{}'.format(epoch))


def save_net(name):
    print("save network...")
    six.moves.cPickle.dump(model.to_cpu(), open("{}.pkl".format("../" + name), "wb"), -1)


train()
