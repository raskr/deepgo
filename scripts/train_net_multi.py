import argparse
import os

from modified_functions.softmax_cross_entropy_multi import softmax_cross_entropy_multi
import six
import chainer.functions as F
import chainer
from chainer import cuda, optimizers

from data_multi import DataMulti as Data

parser = argparse.ArgumentParser(description='train Go')
parser.add_argument('--gpu', '-g', default=-1, type=int, help='GPU ID (negative value indicates CPU)')
args = parser.parse_args()
use_gpu = args.gpu >= 0

if use_gpu:
    cuda.check_cuda_available()

base_path = os.path.dirname(os.path.abspath(__file__))
db_path = os.path.normpath(os.path.join(base_path, '../deepgo_multi.db'))

# 12481120 -> max
# 10551120 -> omit 1d, 2d
# 10931120 -> not omitd

# data provider (if 39998 sgf => 3898669)
data = Data(use_gpu=use_gpu,
            db_path=db_path,
            b_size=2,
            n_ch=22,
            n_train_data=100,
            n_test_data=40,
            n_y=3,
            n_epoch=4)

n_layer = 3

# Prepare data set
model = chainer.FunctionSet(
        conv1=F.Convolution2D(in_channels=data.n_ch, out_channels=30, ksize=5, pad=2),
        conv2=F.Convolution2D(in_channels=30, out_channels=30, ksize=5, pad=2),
        conv3=F.Convolution2D(in_channels=30, out_channels=data.n_y, ksize=5, pad=2),
)


if use_gpu:
    cuda.get_device(0).use()
    model.to_gpu()

# cache. used when testing.
softmax = F.Softmax()


def forward(x_batch, y_batch, invalid_batch):
    x, t = chainer.Variable(x_batch), chainer.Variable(y_batch)

    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    y = F.relu(model.conv3(h))
    y_reduced = pick_channel_y(y.data, 0)

    if invalid_batch is not None:
        y_reduced = softmax.forward(y_reduced)
        y_reduced = (y_reduced.data - invalid_batch).clip(0, 1)
        y_reduced = softmax.forward(y_reduced)

    return softmax_cross_entropy_multi(y, t),\
           F.accuracy(chainer.Variable(y_reduced), chainer.Variable(pick_channel_t(y_batch, 0)))


# Precisely, not 'channel'
def pick_channel_t(array, idx):
    ret = data.xp.hsplit(array, data.n_y)[idx].squeeze()
    return ret


def pick_channel_y(array, idx):
    reshaped = array.reshape(data.b_size, data.n_y, 361)
    ret = data.xp.hsplit(reshaped, data.n_y)[idx].squeeze()
    return ret


def train():
    optimizer = optimizers.SGD(lr=0.1)
    optimizer.setup(model)
    for epoch in six.moves.range(1, data.n_epoch + 1):
        sum_accuracy = sum_loss = mb_count = 0
        for i in data.mb_indices(True):
            if i % 100 == 0:
                print('epoch: {} mini batch: {} of {}'.format(epoch, mb_count, data.n_mb_train))
            mb_count += 1
            x_batch, y_batch = data(True, i)

            optimizer.zero_grads()
            loss, acc = forward(x_batch, y_batch, None)
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

        current_state = '{}ep_{}data_{}pred_{}layer'.format(epoch, data.n_train_data, data.n_y, n_layer)
        print('state: {}\n train mean loss = {}, accuracy = {}'.format(current_state, sum_loss / data.n_train_data, sum_accuracy / data.n_train_data))
        with open('result.txt', 'a+') as f:
            f.write(('state: {}\n train epoch {} train mean loss = {}, accuracy = {}\n'
                     .format(current_state, epoch, sum_loss / data.n_train_data,
                             sum_accuracy / data.n_train_data)))

        # evaluation (test)
        sum_accuracy = sum_loss = mb_count = 0
        for i in data.mb_indices(False):
            print('test mini batch: {} of {}'.format(mb_count, data.n_mb_test))
            mb_count += 1
            x_batch, y_batch, invalid_batch = data(False, i)

            loss, acc = forward(x_batch, y_batch, invalid_batch)
            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)

        current_state = '{}ep_{}data_{}pred_{}layer'.format(epoch, data.n_test_data, data.n_y, n_layer)
        print('state: {}\n test mean loss = {}, accuracy = {}'.format(current_state, sum_loss / data.n_test_data, sum_accuracy / data.n_test_data))
        with open('result.txt', 'a+') as f:
            f.write(('state: {}\n test mean loss = {}, accuracy = {}\n'.format(current_state, sum_loss / data.n_test_data, sum_accuracy / data.n_test_data)))

        save_net('white_{}'.format(current_state))


def save_net(name):
    print("save network...")
    six.moves.cPickle.dump(model.to_cpu(), open("{}.pkl".format("../" + name), "wb"), -1)


train()
