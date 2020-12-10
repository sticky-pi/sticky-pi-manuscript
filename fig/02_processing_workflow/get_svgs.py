import os
from sticky_pi.siamese_insect_matcher.trainer import Trainer
from sticky_pi.siamese_insect_matcher.stitcher import Stitcher
from sticky_pi.siamese_insect_matcher.dataset import RemoteDataset, Dataset
import cv2
from sticky_pi.api import StickyPiAPI

from sticky_pi.database import ImageDBRemote, ImageDBFileSystem
import optparse
import dotenv
import logging

import pandas
import tempfile

tmpdir = tempfile.mkdtemp()

db = ImageDBFileSystem('/home/quentin/sticky_pi_root', annotation_algo='sticky-pi-universal-insect-detector')

series = db.select_series('0a5bb6f4','2020-06-29_05-00-00','2020-06-29_12-00-00')

for im in series.image.tolist():
    target = os.path.join(tmpdir, os.path.splitext(im.filename)[0] + '.svg')
    im.to_svg(target)
print(target)