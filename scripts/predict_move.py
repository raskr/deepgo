import argparse
from math import exp
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
n_channel = 3


def forward_once(x, invalid):
    x = chainer.Variable(np.asarray(x, dtype=np.float32).reshape(1, n_channel, 19, 19))
    invalid = np.asarray(invalid, dtype=np.float32)
    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    h = F.relu(model.conv3(h))
    h = F.relu(model.conv4(h))
    y = model.l(h)
    y = F.softmax(y)
    return np.argmax(y.data - invalid)


def str2floats(string):
    # print(len(string)/361)
    # full = [1.0 if x == '1' else 0.0 for x in string]
    # others = [1.0 if x == '1' else 0.0 for x in string[361:]]

    board = [1.0 if x == '1' else 0.0 for x in string[:361*3]] # 3

    # border = [1.0 if x == '1' else 0.0 for x in string[361*3:361*4]] # 1
    # lib = [1.0 if x == '1' else 0.0 for x in string[361*4:361*10]] # 6
    # ko = [1.0 if x == '1' else 0.0 for x in string[361*10:361*11]] # 1
    # rank = [1.0 if x == '1' else 0.0 for x in string[361*11:361*20]] # 9
    # prev = [1.0 if x == '1' else 0.0 for x in string[361*20:361*21]] # 1
    # invalid = [1.0 if x == '1' else 0.0  for x in string[361*21:361*22]] # 1
    # g_sizes = [exp(0.01 * int(c)) for c in string[361*22:361*24]] # 2
    # his = [exp(-0.1 * int(c)) for c in string[0:361]] # 1

    # others.extend(his)
    return board


def str2floats_simple(string):
    return [1.0 if x == '1' else 0.0 for x in string]

result = forward_once(str2floats(args.board), str2floats_simple(args.invalids))
# with open('test', 'a+') as f:
#     f.write(str(result))
# return result to stdout
print(str(result))
