import argparse

from datetime import datetime
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

base_path = os.path.dirname(os.path.abspath(__file__))
db_path = os.path.normpath(os.path.join(base_path, '../deepgo_multi_omit_prev_strong.db'))

# 12481120 -> max
# 10551120 -> omit 1d, 2d
# 10931120 -> not omitd

# data provider (if 39998 sgf => 3898669)
data = Data(use_gpu=use_gpu,
            db_path=db_path,
            b_size=200,
            n_ch=5,
            n_train_data=13000000,
            n_test_data=70000,
            n_layer=6,
            n_y=1,
            n_epoch=1)


# Prepare data set
model = chainer.FunctionSet(
    conv1=F.Convolution2D(in_channels=data.n_ch, out_channels=32, ksize=5, pad=2),
    conv2=F.Convolution2D(in_channels=32, out_channels=32, ksize=5, pad=2),
    conv3=F.Convolution2D(in_channels=32, out_channels=32, ksize=5, pad=2),
    conv4=F.Convolution2D(in_channels=32, out_channels=32, ksize=5, pad=2),
    conv5=F.Convolution2D(in_channels=32, out_channels=32, ksize=5, pad=2),
    conv6=F.Convolution2D(in_channels=32, out_channels=1, ksize=3, pad=1),
    l=F.Linear(361, 361)
)


if use_gpu:
    cuda.get_device(0).use()
    model.to_gpu()

start_time = datetime.now().strftime('%Y-%m-%d_%H:%M:%S')


def decline_lr(epoch, optimizer):
    if epoch == 2:
        optimizer.lr = 0.06
    if epoch == 3:
        optimizer.lr = 0.04
    if epoch == 4:
        optimizer.lr = 0.02
    if epoch == 5:
        optimizer.lr = 0.01


def forward(x_batch, y_batch, invalid_batch):
    x, t = chainer.Variable(x_batch), chainer.Variable(y_batch)
    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    h = F.relu(model.conv3(h))
    h = F.relu(model.conv4(h))
    h = F.relu(model.conv5(h))
    y = y_test = model.l(F.relu(model.conv6(h)))

    if invalid_batch is not None:
        y_test = F.softmax(y_test)
        y_test = (y_test.data - invalid_batch).clip(0, 1)
        y_test = F.softmax(chainer.Variable(y_test))

    return F.softmax_cross_entropy(y, t), F.accuracy(y_test, t)


def train():
    optimizer = optimizers.SGD(lr=0.08)
    optimizer.setup(model)
    for epoch in six.moves.range(1, data.n_epoch + 1):
        sum_accuracy = sum_loss = mb_count = 0
        for i in data.mb_indices(True):
            if mb_count % 20 == 0:
                print('epoch: {} mini batch: {} of {}'.format(epoch, mb_count, data.n_mb_train))
            mb_count += 1
            x_batch, y_batch = data(True, i)

            optimizer.zero_grads()
            # loss is result of SoftmaxCrossEntropy#call() (using forward internally)
            loss, acc = forward(x_batch, y_batch, invalid_batch=None)
            loss.backward()
            if epoch == 2:
                optimizer.lr = 0.06
            if epoch == 3:
                optimizer.lr = 0.04
            if epoch == 4:
                optimizer.lr = 0.02
            if epoch == 5:
                optimizer.lr = 0.01
            optimizer.update()

            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)

            if mb_count % (data.n_mb_train / 2) == 0:
                print('state: {}\n train mean loss = {}, accuracy = {}'.format(data.printable(), sum_loss / data.n_train_data, sum_accuracy / data.n_train_data))
                with open('res_{}.txt'.format(start_time), 'a+') as f:
                    f.write(('state: {}\n train epoch {} train loss={}, acc={}\n' .format(data.printable(), epoch, sum_loss / data.n_train_data, sum_accuracy / data.n_train_data)))

        # evaluation (test)
        sum_accuracy = sum_loss = mb_count = 0
        for i in data.mb_indices(False):
            print('test mini batch: {} of {}'.format(mb_count, data.n_mb_test))
            mb_count += 1
            x_batch, y_batch, invalid_batch = data(False, i)

            loss, acc = forward(x_batch, y_batch, invalid_batch)
            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)

        print('state: {}\n test loss={}, acc={}'.format(data.printable(), sum_loss / data.n_test_data, sum_accuracy / data.n_test_data))
        with open('g_size_{}.txt'.format(start_time), 'a+') as f:
            f.write(('state: {}\n test mean loss = {}, accuracy = {}\n'.format(data.printable(), sum_loss / data.n_test_data, sum_accuracy / data.n_test_data)))

    save_net('white_{}'.format(data.printable()))


def save_net(name):
    print("save network...")
    six.moves.cPickle.dump(model.to_cpu(), open("{}.pkl".format("../" + name), "wb"), -1)
    if use_gpu:
        model.to_gpu()


train()
