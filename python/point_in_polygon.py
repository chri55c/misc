import shapefile
import shapely
from shapely.geometry import (Polygon, Point)
from rtree import index
import sys
import csv


def pip(poly_shps, point_shps):
    polys = [Polygon(ps) for ps in [p.points for p in poly_shps]]
    point_coords = [p.points[0] for p in point_shps]
    points = [Point(p) for p in point_coords]

    tree = index.Index()
    for i, p in enumerate(poly_shps):
        tree.insert(i, p.bbox)

    matches = []
    for i in range(len(points)):
        match = None
        for j in tree.intersection(point_coords[i]):
            # print "point %d within bbox of poly %d" % (i, j)
            if points[i].within(polys[j]):
                # print "point %d within poly %d" % (i, j)
                match=j
                break
        if (match is not None):
            matches.append((i, match))

    return matches


if __name__ == "__main__":
    poly_sf = sys.argv[1]
    point_sf = sys.argv[2]
    poly_shps = shapefile.Reader(poly_sf).shapes()
    point_shps = shapefile.Reader(point_sf).shapes()

    matches = pip(poly_shps, point_shps)
    with open('pip_out.csv', 'wb') as f:
        writer = csv.writer(f)
        f.writerows(matches)

