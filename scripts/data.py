import sqlite3 as db
from math import exp
import numpy as np
from chainer import cuda

# change the table name O/X
query = "SELECT state, target FROM O WHERE _id BETWEEN {} AND {} ORDER BY RANDOM()"
query_test = "SELECT state, target, invalid FROM O WHERE _id BETWEEN {} AND {} ORDER BY RANDOM()"


class Data:

    def printable(self):
        return '{}_{}ep_{}train_{}test_{}pred_{}layer_{}width_{}'.format(self.feat,
                                                                         self.n_epoch,
                                                                         self.n_train_data,
                                                                         self.n_test_data,
                                                                         self.n_y,
                                                                         self.n_layer,
                                                                         self.layer_width,
                                                                         self.opt)

    def __init__(self, layer_width, feat, use_gpu, n_epoch, n_ch, b_size, n_train_data, n_test_data, n_layer, n_y, db_path, opt):
        self.db_path = db_path
        self.feat = feat
        self.layer_width = layer_width
        self.use_gpu = use_gpu
        self.xp = cuda.cupy if use_gpu else np

        self.n_epoch = n_epoch
        self.n_ch = n_ch
        self.opt = opt
        self.n_y = n_y
        self.n_layer = n_layer
        self.n_train_data = n_train_data
        self.n_test_data = n_test_data
        self.b_size = b_size
        self.n_epoch = n_epoch

        self.n_mb_train = int(n_train_data/b_size)
        self.n_mb_test = int(n_test_data/b_size)
        self.mb_indices_train = np.arange(self.n_mb_train)
        self.mb_indices_test = np.arange(self.n_mb_test + 1, self.n_mb_test * 2 + 1)

        conn = db.connect(db_path)
        self.cur = conn.cursor()

    def __call__(self, train, i):
        xs = self.xp.asarray([], dtype=self.xp.float32)
        ys = self.xp.asarray([], dtype=self.xp.int32)

        if train:
            self.cur.execute(query.format(self.b_size*i+1, self.b_size*i + self.b_size))
            for row in self.cur.fetchall():
                xs = self.xp.concatenate((xs, self.xp.asarray(str2floats(row[0]), self.xp.float32)))
                ys = self.xp.concatenate((ys, self.xp.asarray(split_ys(row[1], self.n_y == 1), self.xp.int32)))

            ret_x = xs.reshape(self.b_size, self.n_ch, 19, 19)
            ret_y = ys if self.n_y == 1 else ys.reshape(self.b_size, self.n_y)
            return ret_x, ret_y
        else:
            invalids = self.xp.asarray([], dtype=self.xp.float32)
            self.cur.execute(query_test.format(self.b_size*i+1, self.b_size*i + self.b_size))
            for row in self.cur.fetchall():
                xs = self.xp.concatenate((xs, self.xp.asarray(str2floats(row[0]), self.xp.float32)))
                ys = self.xp.concatenate((ys, self.xp.asarray(split_ys(row[1], self.n_y == 1), self.xp.int32)))
                invalids = self.xp.concatenate((invalids, self.xp.asarray(str2floats_simple(row[2]), self.xp.float32)))

            ret_y = ys if self.n_y == 1 else ys.reshape(self.b_size, self.n_y)
            return xs.reshape(self.b_size, self.n_ch, 19, 19), ret_y,\
                   invalids.reshape(self.b_size, 361)

    def mb_indices(self, train):
        if train:
            np.random.shuffle(self.mb_indices_train)
            return self.mb_indices_train
        np.random.shuffle(self.mb_indices_test)
        return self.mb_indices_test


# return : Array[Int]
def split_ys(string, head):
    ret = [int(a) for a in string.split(',')]
    return ret[0:1] if head else ret


# total 24
def str2floats(string):
    # print(len(string)/361)
    # full = [1.0 if x == '1' else 0.0 for x in string]

    board = [1.0 if x == '1' else 0.0 for x in string[:361*3]] # 3
    # border = [1.0 if x == '1' else 0.0 for x in string[361*3:361*4]] # 1
    # lib = [1.0 if x == '1' else 0.0 for x in string[361*4:361*10]] # 6
    # ko = [1.0 if x == '1' else 0.0 for x in string[361*10:361*11]] # 1
    # rank = [1.0 if x == '1' else 0.0 for x in string[361*11:361*20]] # 9
    # prev = [1.0 if x == '1' else 0.0 for x in string[361*20:361*21]] # 1
    # invalid = [1.0 if x == '1' else 0.0  for x in string[361*21:361*22]] # 1
    # g_sizes = [exp(0.01 * int(c)) for c in string[361*22:361*24]] # 2
    # his = [exp(-0.1 * int(c)) for c in string[361*24:361*25]] # 1

    # board.extend(rank)
    return board


def str2floats_simple(string):
    return [1.0 if x == '1' else 0.0 for x in string]
