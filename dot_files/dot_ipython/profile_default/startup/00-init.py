import sys
import os
import readline
import codecs
import time
import datetime
import json
print("import sys, os, readline, codecs, time, datetime, json")

try:
    import numpy as np
    print("import numpy as np")
except ImportError:
    print("numpy has not been loaded")

try:
    import scipy as sp
    print("import scipy as sp")
    from scipy import stats
    print("from scipy import stats")
    from scipy import linalg
    print("from scipy import linalg")
    from scipy import constants as sc
    print("from scipy import constants as sc")
    from scipy.stats import mstats
    print("from scipy.stats import mstats")
except ImportError:
    print("scipy has not been loaded")

try:
    import pandas as pd
    print("import pandas as pd")
    import pandas.tools.plotting as plotting
    print("import pandas.tools.plotting as plotting")
except ImportError:
    print("pandas has not been loaded")

try:
    import talib as ta
    print("import talib as ta")
except ImportError:
    print("talib has not been loaded")

def set_trace():
    from IPython.core.debugger import Pdb
    Pdb(color_scheme='Linux').set_trace(sys._getframe().f_back)

def debug(f, *args, **kwargs):
    from IPython.core.debugger import Pdb
    pdb = Pdb(color_scheme='Linux')
    return pdb.runcall(f, *args, **kwargs)
