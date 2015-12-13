import argparse

import sys
import os
import numpy as np
from math import exp

import six
import chainer.functions as F
import chainer


parser = argparse.ArgumentParser(description='eval network')
parser.add_argument('--board', '-b', default='', type=str, help='current board state in 22 ch')
parser.add_argument('--invalids', '-i', default='', type=str, help='invalid positions')
parser.add_argument('--color', '-c', default='', type=str, help='my color')
args = parser.parse_args()

base = os.path.dirname(os.path.abspath(__file__))
db_name = os.path.normpath(os.path.join(base, '../deepgo.db'))


def forward_once(x, invalid, model):
    x = chainer.Variable(np.asarray(x, dtype=np.float32).reshape(1, 22, 19, 19))
    invalid = np.asarray(invalid, dtype=np.float32)
    h = F.relu(model.conv1(x))
    h = F.relu(model.conv2(h))
    h = F.relu(model.conv3(h))
    y = model.l1(h)
    y = F.softmax(y)
    y = (y.data - invalid).clip(0)
    return np.argmax(y)


def predict(x, invalids, color):
    model = six.moves.cPickle.load(open("{}.pkl".format(color), "rb"))
    return forward_once(x, invalids, model)


def str2floats(string):
    a = list(string)
    lifespans = [exp(-0.1 * int(c)) for c in a[-361:]]
    others = [1.0 if x == '1' else 0.0 for x in a[:-361]]
    others.extend(lifespans)
    return others


result = predict(str2floats(args.board), str2floats(args.invalids), args.color)
print(str(result))
