import sqlite3 as db
from utils import *
import random
import numpy as np
from chainer import cuda

# change the table name O/X
query = "SELECT state, target FROM O WHERE _id BETWEEN {} AND {} ORDER BY RANDOM()"
query_test = "SELECT state, target, invalid FROM O WHERE _id BETWEEN {} AND {} ORDER BY RANDOM()"


class Data:

    def __init__(self, use_gpu, n_epoch, n_ch, b_size, n_train_data, n_test_data, db_path):
        self.db_path = db_path
        self.use_gpu = use_gpu
        self.xp = cuda.cupy if use_gpu else np

        self.n_epoch = n_epoch
        self.n_ch = n_ch
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
        xs, ys, invalids = [], [], []

        if train:
            self.cur.execute(query.format(self.b_size*i+1, self.b_size*i + self.b_size))
            for row in self.cur.fetchall():
                xs.extend(str2floats(row[0]))
                ys.append(row[1])
            return self.xp.asarray(xs, dtype=self.xp.float32).reshape(self.b_size, self.n_ch, 19, 19), \
                   self.xp.asarray(ys, dtype=self.xp.int32)
        else:
            self.cur.execute(query_test.format(self.b_size*i+1, self.b_size*i + self.b_size))
            for row in self.cur.fetchall():
                xs.extend(str2floats(row[0]))
                ys.append(row[1])
                invalids.extend(str2floats_simple(row[2]))

            return self.xp.asarray(xs, dtype=self.xp.float32).reshape(self.b_size, self.n_ch, 19, 19), \
                   self.xp.asarray(ys, dtype=self.xp.int32), \
                   self.xp.asarray(invalids, dtype=self.xp.float32).reshape(self.b_size, 361)

    def mb_indices(self, train):
        if train:
            random.shuffle(self.mb_indices_train)
            return iter(self.mb_indices_train)
        random.shuffle(self.mb_indices_test)
        return iter(self.mb_indices_test)

