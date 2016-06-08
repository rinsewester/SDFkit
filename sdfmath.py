#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
SDF math utilities.

author: Rinse Wester

"""

from fractions import gcd
from functools import reduce


def lcm(numbers):
    return reduce(lambda x, y: (x * y) // gcd(x, y), numbers, 1)
