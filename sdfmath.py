#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
SDF math utilities.

author: Rinse Wester

"""

from math import gcd
from functools import reduce


def lcm(numbers):
    """Retruns the least common multiple of the list numbers."""
    return reduce(lambda x, y: (x * y) // gcd(x, y), numbers, 1)

def is_power2(num):
    """Retruns True when num is a power of two"""
    return num != 0 and ((num & (num - 1)) == 0)
