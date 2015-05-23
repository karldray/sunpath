import csv
import json
import urllib.request

def convertline(x):
    return [
        int(x[0]), # id
        x[1], x[2], x[3], x[4], x[5],
        float(x[6]), float(x[7]), # lat & long
        int(x[8]), # elevation
        float(x[9]), # UTC offset
        x[10], x[11]
    ]

json.dump([
    x # convertline(x)
    for x in csv.reader(line.decode() for line in urllib.request.urlopen(
        "https://sourceforge.net/p/openflights/code/HEAD/tree/openflights/data/airports.dat?format=raw"
    ))
], open("airports.json", "w"))

