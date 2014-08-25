
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
except ImportError:
    print("scipy has not been loaded")

try:
    import pandas as pd
    print("import pandas as pd")
except ImportError:
    print("pandas has not been loaded")

try:
    import matplotlib.pyplot as plt
    print("import matplotlib.pyplot as plt")
except ImportError:
    print("matplotlib has not been loaded")

try:
    import pylab
    print("import pylab")
except ImportError:
    print("pylab has not been loaded")
