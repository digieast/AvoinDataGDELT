from IPython.core.display import HTML
styles = open("Style.css").read()
HTML(styles)

import datetime as dt
from collections import defaultdict

import matplotlib.pyplot as plt
import pandas

matplotlib.rcParams['figure.figsize'] = [8,4] # Set default figure size


# Set this variable to the directory where the GDELT data files are
PATH = "/media/baikal/Asiakirjat/data/GDELT/GDELT.1979-2012.reduced/"

!head -n 5 GDELT.1979-2012.reduced/2010.reduced.txt
