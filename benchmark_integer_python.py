#!/usr/bin/env python

import numpy as np

print("Loading integers.txt")
x = np.fromfile("integers.txt", dtype=int, sep=" ")

print("Number of values: {}".format(x.size))

# Get unique elements and their counts
unique, counts = np.unique(x, return_counts=True)

# Get all odd counts
mask = (counts & 1)

# Print number of odd counts
print("Number of values with odd counts: {}".format(mask.sum()))
