import csv
import json
import sys
from urllib.request import urlopen

url = "https://sourceforge.net/p/openflights/code/HEAD/tree/openflights/data/airports.dat?format=raw"

json.dump(list(csv.reader(line.decode() for line in urlopen(url))), sys.stdout)

