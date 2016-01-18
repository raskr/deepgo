import sqlite3 as db
from math import exp
import random
import numpy as np
from chainer import cuda

# change the table name O/X
query = "SELECT state, target FROM O WHERE _id BETWEEN {} AND {} ORDER BY RANDOM()"
query_test = "SELECT state, target, invalid FROM O WHERE _id BETWEEN {} AND {} ORDER BY RANDOM()"


class Data:

    def printable(self):
        return '{}ep_{}data_1pred_{}layer'.format(self.n_epoch,
                                                  self.n_train_data,
                                                  self.n_y,
                                                  self.n_layer)

    def __init__(self, use_gpu, n_epoch, n_ch, b_size, n_train_data, n_test_data, n_layer, n_y, db_path):
        self.db_path = db_path
        self.use_gpu = use_gpu
        self.xp = cuda.cupy if use_gpu else np

        self.n_epoch = n_epoch
        self.n_ch = n_ch
        self.n_y = n_y
        self.n_layer = n_layer
        self.n_train_data = n_train_data
        self.n_test_data = n_test_data
        self.b_size = b_size
        self.n_epoch = n_epoch

        self.n_mb_train = int(n_train_data/b_size)
        self.n_mb_test = int(n_test_data/b_size)
        self.mb_indices_train = list(range(self.n_mb_train))
        self.mb_indices_test = list(range(self.n_mb_test + 1, self.n_mb_test + 1 + self.n_mb_test))

        conn = db.connect(db_path)
        self.cur = conn.cursor()

    def __call__(self, train, i):
        xs = self.xp.asarray([], dtype=self.xp.float32)
        ys = self.xp.asarray([], dtype=self.xp.int32)

        if train:
            self.cur.execute(query.format(self.b_size*i+1, self.b_size*i + self.b_size))
            for row in self.cur.fetchall():
                xs = self.xp.concatenate((xs, self.xp.asarray(str2floats(row[0]), self.xp.float32)))
                ys = self.xp.concatenate((ys, self.xp.asarray(split_y(row[1], self.n_y == 1), self.xp.int32)))

            ret_x = xs.reshape(self.b_size, self.n_ch, 19, 19)
            ret_y = ys if self.n_y == 1 else ys.reshape(self.b_size, self.n_y)
            return ret_x, ret_y
        else:
            invalids = self.xp.asarray([], dtype=self.xp.float32)
            self.cur.execute(query_test.format(self.b_size*i+1, self.b_size*i + self.b_size))
            for row in self.cur.fetchall():
                xs = self.xp.concatenate((xs, self.xp.asarray(str2floats(row[0]), self.xp.float32)))
                ys = self.xp.concatenate((ys, self.xp.asarray(split_y(row[1], self.n_y == 1), self.xp.int32)))
                invalids = self.xp.concatenate((invalids, self.xp.asarray(str2floats_simple(row[2]), self.xp.float32)))

            ret_y = ys if self.n_y == 1 else ys.reshape(self.b_size, self.n_y)
            return xs.reshape(self.b_size, self.n_ch, 19, 19), ret_y,\
                   invalids.reshape(self.b_size, 361)

    def mb_indices(self, train):
        if train:
            random.shuffle(self.mb_indices_train)
            return iter(self.mb_indices_train)
        random.shuffle(self.mb_indices_test)
        return iter(self.mb_indices_test)


# return : Array[Int]
def split_y(string, head):
    ret = [int(a) for a in string.split(',')]
    return ret[0:1] if head else ret


# convert string consist of channels to float array
def str2floats(string):
    #lifespans = [exp(-0.1 * int(c)) for c in string[-361:]]
    # g_sizes = [exp(0.01 * int(c)) for c in string[-361*3:-361]]
    #others = [1.0 if x == '1' else 0.0 for x in string[:-361*3]]
    board = [1.0 if x == '1' else 0.0 for x in string[:361*3]]
    gSize = [1.0 if x == '1' else 0.0 for x in string[-361*3:-361*1]]
    # others.extend(g_sizes)
    board.extend(gSize)
    return board


def str2floats_simple(string):
    return [1.0 if x == '1' else 0.0 for x in string]
