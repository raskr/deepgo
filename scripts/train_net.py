import argparse
from chainer import computational_graph as cg
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

here = os.path.dirname(os.path.abspath(__file__))
db_path = os.path.normpath(os.path.join(here, '../deepgo.db'))

# data provider
data = Data(feat='liber',
            opt='SGD',
            use_gpu=use_gpu,
            db_path=db_path,
            b_size=256,
            layer_width=32,
            n_ch=9,
            n_train_data=13000000,
            n_test_data=100000,
            n_y=1,
            n_layer=3,
            n_epoch=2)

# Prepare data set
model = chainer.FunctionSet(
    conv1=F.Convolution2D(in_channels=data.n_ch, out_channels=32, ksize=5, pad=2),
    conv2=F.Convolution2D(in_channels=32, out_channels=32, ksize=5, pad=2),
    conv3=F.Convolution2D(in_channels=32, out_channels=1, ksize=5, pad=2),
    l=F.Linear(361, 361)
)


if use_gpu:
    cuda.get_device(args.gpu).use()
    model.to_gpu()

start_time = datetime.now()
start_time_str = start_time.strftime('%Y-%m-%d_%H:%M:%S')

res_filename = '{}_{}.txt'.format(data.printable(), start_time_str)
with open(res_filename, 'w+') as f:
    f.write('************* {}\n'.format(data.printable()))


def decline_lr(optimizer):
    lr = optimizer.lr
    optimizer.lr = lr / 2


def forward_conv(x):
    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    return F.relu(model.conv3(h))


def forward(x_batch, y_batch):
    x, t = chainer.Variable(x_batch), chainer.Variable(y_batch)
    y = model.l(forward_conv(x))
    return F.softmax_cross_entropy(y, t), F.accuracy(y, t)


def forward_test(x_batch, y_batch, invalid_batch):
    x, t = chainer.Variable(x_batch), chainer.Variable(y_batch)
    y = y_clip = model.l(forward_conv(x))
    y_clip = F.softmax(y_clip, use_cudnn=False)
    y_clip = (y_clip.data - invalid_batch).clip(0, 1)
    y_clip = F.softmax(chainer.Variable(y_clip), use_cudnn=False)
    return F.softmax_cross_entropy(y, t), F.accuracy(y, t), F.accuracy(y_clip, t)


def train():
    optimizer = optimizers.SGD(lr=0.08)
    optimizer.setup(model)
    for epoch in six.moves.range(1, data.n_epoch + 1):
        sum_accuracy = sum_loss = mb_count = 0
        for i_mb in data.mb_indices(True):
            # print
            if mb_count % 20 == 0: print('epoch: {} mini batch: {} of {}'.format(epoch, mb_count, data.n_mb_train))
            mb_count += 1

            # actual task
            x_batch, y_batch = data(True, i_mb)
            optimizer.zero_grads()
            loss, acc = forward(x_batch, y_batch)
            loss.backward()
            optimizer.update()


	    if epoch == 1 and i_mb == 0:
	        with open('graph.dot',  'w') as o:
	    	    g = cg.build_computational_graph((loss,  ),  remove_split=True)
		    o.write(g.dump())
	        print('graph generated')

	    sum_loss += float(loss.data) * len(y_batch)
	    sum_accuracy += float(acc.data) * len(y_batch)

            # write result
        res = 'train epoch {} train loss={}, acc={}\n'.format(epoch, sum_loss / data.n_train_data, sum_accuracy / data.n_train_data)
        print(res)
        with open(res_filename, 'a+') as f: f.write(res)

        # evaluation (test)
        sum_accuracy = sum_accuracy_clip = sum_loss = mb_count = 0
        for i_mb in data.mb_indices(False):
            print('test mini batch: {} of {}'.format(mb_count, data.n_mb_test))
            mb_count += 1
            x_batch, y_batch, invalid_batch = data(False, i_mb)

            loss, acc, acc_clip = forward_test(x_batch, y_batch, invalid_batch)


            sum_loss += float(loss.data) * len(y_batch)
            sum_accuracy += float(acc.data) * len(y_batch)
            sum_accuracy_clip += float(acc_clip.data) * len(y_batch)

        res = 'test epoch={} loss={}, acc={}, acc_clip={}\n'.format(epoch, sum_loss / data.n_test_data, sum_accuracy / data.n_test_data, sum_accuracy_clip / data.n_test_data)
        print(res)
        with open(res_filename, 'a+') as f: f.write(res)
        decline_lr(optimizer)
    save_net('white_{}'.format(data.printable()))
    with open(res_filename, 'a+') as f:
        f.write('It took total... {}\n\n'.format(datetime.now() - start_time))


def save_net(name):
    print("save network...")
    six.moves.cPickle.dump(model.to_cpu(), open("{}.pkl".format("../" + name), "wb"), -1)
    if use_gpu:
        model.to_gpu()


train()
