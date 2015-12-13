import argparse
from utils import *
import numpy as np
import six
import chainer.functions as F
import chainer


parser = argparse.ArgumentParser(description='eval network')
parser.add_argument('--board', '-b', default='', type=str, help='current board state in 22 ch')
parser.add_argument('--invalids', '-i', default='', type=str, help='invalid positions')
parser.add_argument('--color', '-c', default='', type=str, help='my color')
args = parser.parse_args()

model = six.moves.cPickle.load(open("{}.pkl".format(args.color), "rb"))
n_channel = 24


def forward_once(x, invalid):
    x = chainer.Variable(np.asarray(x, dtype=np.float32).reshape(1, n_channel, 19, 19))
    invalid = np.asarray(invalid, dtype=np.float32)
    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    h = F.relu(model.conv3(h))
    y = model.l1(h)
    y = F.softmax(y)
    y = (y.data - invalid).clip(0)
    return np.argmax(y)


result = forward_once(str2floats(args.board), str2floats(args.invalids))
# return result to stdout
print(str(result))
