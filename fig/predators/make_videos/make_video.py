import pandas as pd
import logging
import os
from sticky_pi_ml.utils import MLScriptParser
from sticky_pi_api.client import LocalClient, RemoteClient
from sticky_pi_ml.siamese_insect_matcher.ml_bundle import ClientMLBundle
from sticky_pi_ml.siamese_insect_matcher.trainer import Trainer
from sticky_pi_ml.siamese_insect_matcher.predictor import Predictor
from sticky_pi_ml.siamese_insect_matcher.matcher import Matcher
from sticky_pi_ml.siamese_insect_matcher.candidates import make_candidates
from sticky_pi_ml.image import ImageSeries


csv_file = './series.csv'
df = pd.read_csv(csv_file)
assert 'device' in df.columns
assert 'start_datetime' in df.columns
assert 'end_datetime' in df.columns
df = df[['device', 'start_datetime', 'end_datetime']]

parser = MLScriptParser()
opt_dict = parser.get_opt_dict()

cli = RemoteClient(opt_dict['LOCAL_CLIENT_DIR'],
                   opt_dict['API_HOST'],
                   opt_dict['API_USER'],
                   opt_dict['API_PASSWORD'])



for r in df.to_dict('records'):
    s = ImageSeries(**r)
    print(s)
    s.populate_from_client(client=cli, cache_image_dir='/home/quentin/Desktop/spi_cached_img')
