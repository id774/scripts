import codecs
import datetime
import json
import os
import readline
import sys
import time
import warnings

print("import sys, os, readline, codecs, time, datetime, json, warnings")

warnings.resetwarnings()

with warnings.catch_warnings():
    warnings.simplefilter('ignore')

    try:
        import numpy as np
        print("import numpy as np")
        print("numpy", np.__version__)
    except ImportError:
        pass

    try:
        import scipy as sp
        print("import scipy as sp")
        print("scipy", sp.__version__)
        from scipy import stats
        print("from scipy import stats")
        from scipy import linalg
        print("from scipy import linalg")
        from scipy import constants as sc
        print("from scipy import constants as sc")
        from scipy.stats import mstats
        print("from scipy.stats import mstats")
    except ImportError:
        pass

    try:
        import matplotlib as mpl
        print("import matplotlib as mpl")
        print("matplotlib", mpl.__version__)
        mpl.use('Agg')
        print("mpl.use('Agg')")
        import matplotlib.pyplot as plt
        print("import matplotlib.pyplot as plt")
    except ImportError:
        pass

    try:
        import pandas as pd
        print("import pandas as pd")
        print("pandas", pd.__version__)
        import pandas.plotting as plotting
        print("import pandas.plotting as plotting")
    except ImportError:
        pass

    try:
        import tensorflow as tf
        print("import tensorflow as tf")
        print("tensorflow", tf.__version__)
    except ImportError:
        pass

    try:
        import keras
        print("import keras")
        print("keras", keras.__version__)
    except ImportError:
        pass

    try:
        import talib as ta
        print("import talib as ta")
        print("talib", ta.__version__)
    except ImportError:
        pass

def set_trace():
    from IPython.core.debugger import Pdb
    Pdb(color_scheme='Linux').set_trace(sys._getframe().f_back)

def debug(f, *args, **kwargs):
    from IPython.core.debugger import Pdb
    pdb = Pdb(color_scheme='Linux')
    return pdb.runcall(f, *args, **kwargs)
