import argparse
import os
from datetime import datetime

from modified_functions.softmax_cross_entropy_multi import softmax_cross_entropy_multi
import six
import chainer.functions as F
import chainer
from chainer import cuda, optimizers

from data import Data as Data

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
data = Data(feat='plane',
            opt='SGD',
            use_gpu=use_gpu,
            db_path=db_path,
            b_size=2,
            layer_width=32,
            n_ch=24,
            n_train_data=10,
            n_test_data=3,
            n_y=3,
            n_layer=6,
            n_epoch=1)


# Prepare data set
model = chainer.FunctionSet(
        conv1=F.Convolution2D(in_channels=data.n_ch, out_channels=32, ksize=5, pad=2),
        conv2=F.Convolution2D(in_channels=32, out_channels=32, ksize=5, pad=2),
        conv3=F.Convolution2D(in_channels=32, out_channels=32, ksize=5, pad=2),
        conv4=F.Convolution2D(in_channels=32, out_channels=32, ksize=5, pad=2),
        conv5=F.Convolution2D(in_channels=32, out_channels=32, ksize=3, pad=1),
        conv6=F.Convolution2D(in_channels=32, out_channels=data.n_y, ksize=3, pad=1),
)


if use_gpu:
    cuda.get_device(0).use()
    model.to_gpu()


start_time = datetime.now()
start_time_str = start_time.strftime('%Y-%m-%d_%H:%M:%S')

res_filename = '{}.txt'.format(data.printable())
with open(res_filename, 'w+') as f:
    f.write('********** {}\n'.format(data.printable()))

def forward_conv(x):
    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    h = F.relu(model.conv3(h))
    h = F.relu(model.conv4(h))
    h = F.relu(model.conv5(h))
    return F.relu(model.conv6(h))


def forward_test(x_batch, y_batch, invalid_batch):
    x, t = chainer.Variable(x_batch), chainer.Variable(y_batch)
    y = forward_conv(x)
    y_reduced_arr_clip = y_reduced_arr = pick_channel_y(y.data, 0)
    y_reduced_arr_clip = F.softmax(chainer.Variable(y_reduced_arr_clip), use_cudnn=False)
    y_reduced_arr_clip = (y_reduced_arr_clip.data - invalid_batch).clip(0, 1)
    y_reduced_arr_clip = F.softmax(chainer.Variable(y_reduced_arr_clip), use_cudnn=False).data
    ans = chainer.Variable(pick_channel_t(y_batch, 0))
    return softmax_cross_entropy_multi(y, t),\
           F.accuracy(chainer.Variable(y_reduced_arr), ans), \
           F.accuracy(chainer.Variable(y_reduced_arr_clip), ans)


def forward(x_batch, y_batch):
    x, t = chainer.Variable(x_batch), chainer.Variable(y_batch)
    y = forward_conv(x)
    y_reduced_arr = pick_channel_y(y.data, 0)
    return softmax_cross_entropy_multi(y, t),\
           F.accuracy(chainer.Variable(y_reduced_arr), chainer.Variable(pick_channel_t(y_batch, 0)))


# Precisely, not 'channel'
def pick_channel_t(array, idx):
    return data.xp.hsplit(array, data.n_y)[idx].squeeze()


def pick_channel_y(array, idx):
    reshaped = array.reshape(data.b_size, data.n_y, 361)
    return data.xp.hsplit(reshaped, data.n_y)[idx].squeeze()


def decline_lr(epoch, optimizer):
    if epoch == 2:
        optimizer.lr = 0.06
    elif epoch == 3:
        optimizer.lr = 0.04
    elif epoch == 4:
        optimizer.lr = 0.02
    elif epoch == 5:
        optimizer.lr = 0.01


def train():
    optimizer = optimizers.SGD(lr=0.08)
    optimizer.setup(model)
    for epoch in six.moves.range(1, data.n_epoch + 1):
        decline_lr(epoch, optimizer)
        sum_accuracy = sum_loss = mb_count = 0

        # training loop
        for i in data.mb_indices(True):
            if mb_count % 20 == 0:
                print('epoch: {} mini batch: {} of {}'.format(epoch, mb_count, data.n_mb_train))
            mb_count += 1
            x_batch, y_batch = data(True, i)

            optimizer.zero_grads()
            loss, acc = forward(x_batch, y_batch)
            loss.backward()
            optimizer.update()

            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)

            if mb_count % (data.n_mb_train / 2) == 0:
                print('train mean loss = {}, accuracy = {}\n'.format(sum_loss / data.n_train_data, sum_accuracy / data.n_train_data))
                with open(res_filename, 'a+') as f:
                    f.write(('train epoch {} train loss={}, acc={}\n' .format(epoch, sum_loss / data.n_train_data, sum_accuracy / data.n_train_data)))

        # test loop
        sum_accuracy = sum_accuracy_clip = sum_loss = mb_count = 0
        for i in data.mb_indices(False):
            if mb_count % 20 == 0:
                print('epoch{} test mini batch: {} of {}'.format(epoch, mb_count, data.n_mb_test))
            mb_count += 1
            x_batch, y_batch, invalid_batch = data(False, i)

            loss, acc, acc_clip = forward_test(x_batch, y_batch, invalid_batch)
            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)
            sum_accuracy_clip += float(acc_clip.data) * len(y_batch)

        print('test loss={}, acc={}, acc_clip={}'.format(sum_loss / data.n_test_data, sum_accuracy / data.n_test_data, sum_accuracy_clip / data.n_test_data))
        with open(res_filename, 'a+') as f:
            f.write(('test epoch {} loss={}, accuracy={}, accuracy_clip={}\n'.format(epoch, sum_loss / data.n_test_data, sum_accuracy / data.n_test_data, sum_accuracy_clip / data.n_test_data)))

    save_net('white_{}'.format(data.printable()))
    with open(res_filename, 'a+') as f:
        f.write('It took total... {}\n\n'.format(datetime.now() - start_time))


def save_net(name):
    print("save network...")
    six.moves.cPickle.dump(model.to_cpu(), open("{}.pkl".format("../" + name), "wb"), -1)
    if use_gpu:
        model.to_gpu()


train()
