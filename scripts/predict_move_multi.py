import argparse
from modified_functions.softmax_cross_entropy_multi import softmax_cross_entropy_multi
import six
import chainer.functions as F
import chainer
import numpy as np


parser = argparse.ArgumentParser(description='eval network')
parser.add_argument('--board', '-b', default='', type=str, help='current board state in 22 ch')
parser.add_argument('--invalids', '-i', default='', type=str, help='invalid positions')
parser.add_argument('--color', '-c', default='', type=str, help='my color')
args = parser.parse_args()

model = six.moves.cPickle.load(open("{}.pkl".format(args.color), "rb"))
n_channel = 3
n_y = 3


def pick_channel_y(array, idx):
    reshaped = array.reshape(1, n_y, 361)
    return np.hsplit(reshaped, n_y)[idx].squeeze()


def forward_once(x, invalid):
    x = chainer.Variable(x)
    invalid = np.asarray(invalid, dtype=np.float32)
    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    h = F.relu(model.conv3(h))
    y = F.relu(model.conv4(h))

    y_reduced_arr_clip = pick_channel_y(y.data, 0)
    y_reduced_arr_clip = F.softmax(chainer.Variable(y_reduced_arr_clip), use_cudnn=False)
    return np.argmax(y_reduced_arr_clip.data - invalid)


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
