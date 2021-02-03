import pandas as pd
import numpy as np
from sticky_pi.database import ImageDBRemote
from sticky_pi.image import ImageSequence
from sticky_pi.api import StickyPiAPI
import logging
import os
from sticky_pi.ml.mrcnn_detector import RemoteDetector
import optparse
import dotenv
from sticky_pi.image import SVGImage
import pandarallel

METADATA = "./metadata.csv"
SECRET_CONFIG_FILE = '../.secret.env'
CSV_RESULTS="./detection_results.csv"

def detect(row, only_predetected=False):
    im = row['image']
    filename, _ = os.path.splitext(im.filename)
    svg_path = os.path.join(os.path.dirname(im.path),filename + '.svg')
    if not os.path.exists(svg_path):
        if only_predetected:
            return None
        logging.warning('processing %s' % im.filename)
        im = detector.detect(im)
        im.to_svg(svg_path)
    return svg_path

def get_n(row):
    x = detect(row, only_predetected = True)
    if x:
        n = SVGImage(x).n_annotations
        logging.warning((os.path.basename(x), n))
    else:
        n = np.nan
    return n

def annimate(g):
    name = g.series_id.unique()[0] + '.mp4'
    logging.info(name)
    s = ImageSequence(g.annot_image)
    s.to_animation(name, scale=.50)



if __name__ == '__main__':

    metadata = pd.read_csv(METADATA)
    if SECRET_CONFIG_FILE:
        dotenv.load_dotenv(SECRET_CONFIG_FILE)

    configuration = {
        "root_url": os.environ.get("SPI_ROOT_URL"),
        "user": os.environ.get("SPI_USER"),
        "password": os.environ.get("SPI_PASSWORD")
    }


    for k, v in configuration.items():
        if not v:
            raise Exception("No value for bucket credential %s. Check file and/or env variables" % k)

    spi_root = os.environ.get("SPI_ROOT")

    if not os.path.isdir(spi_root):
        raise Exception("No value for spi root `SPI_ROOT`. Check file and/or env variables")

    parser = optparse.OptionParser()
    parser.add_option("-v", "--verbose", dest="verbose", default=False,
                      help="verbose",
                      action="store_true")
    parser.add_option("-p", "--pre", dest="pre", default=False,
                      help="preanalyse: dl files already",
                      action="store_true")
    parser.add_option("-a", "--process", dest="process", default=False,
                      help="process",
                      action="store_true")
    parser.add_option("-c", "--count", dest="count", default=False,
                      help="preanalyse: dl files already",
                      action="store_true")
    parser.add_option("-m", "--make-videos", dest="make_videos", default=False,
                      help="preanalyse: dl files already",
                      action="store_true")
    (options, args) = parser.parse_args()
    option_dict = vars(options)

    if option_dict['verbose']:
        logging.getLogger().setLevel(logging.DEBUG)
        logging.info("DEBUG mode ON")

    logging.warning(str(configuration))
    api = StickyPiAPI(**configuration, protocol='http', sticky_pi_dir=spi_root)
    db = ImageDBRemote(spi_root, api, n_thread=8)
    series = db.select_multiple_series(metadata)

    if option_dict['pre']:
        exit()

    detector = RemoteDetector(spi_root, remote_credentials=SECRET_CONFIG_FILE)

    if option_dict['process']:
        series['annot_image'] = series.apply(detect, axis=1)
        exit()

    pandarallel.pandarallel.initialize()
    if option_dict['count']:
        series['N'] = series.parallel_apply(get_n, axis=1)
        series.to_csv(CSV_RESULTS, index=False)
        exit()

    if option_dict['make_videos']:
        series['annot_image'] = series.parallel_apply(detect, axis=1)
        series.groupby('series_id').parallel_apply(annimate)
        exit()
