# deepgo

deepgo is a toolkit to use Convolutional Neural Network (CNN)
for board game Go.
Feature handling is implemented in scala and CNN is done with chainer.
I have also implemented "long term prediction" (suggested by [paper from Facebook AI research](http://arxiv.org/pdf/1511.06410v1.pdf))


### Contains:
 - CNN implementation with Chainer
 - training data (.sgf files) parsing
 - Feature extractor
 - Game Rules
 - GTP commands subset


### Requirement:
 - scala environment
 - [chainer](https://github.com/pfnet/chainer)
 - [sqlite3](https://www.sqlite.org/)

### Usage
##### create features
Create sqlite database holds feature-maps(less than 100GB if 2000 sgf files).

    sbt "run -mode db -color w -d path/to/sgffiles -pred-step 3"

Then `deepgo.db` will be created in ./depgo dir. This is time-consuming process (It takes few hours)

##### Training and save neural network
    python scripts/train_net.py

Then `foo.plk` will be created.
You can use GPU if you set `use_gpu` flag in `train_net.py`.
