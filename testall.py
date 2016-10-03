#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Validation of SDFkt using a collection of unittests

author: Rinse Wester

"""

import unittest

from tests.testhsdfgraph import HSDFGraphTestCase
from tests.testsdfgraph import SDFGraphTestCase
from tests.testcsdfgraph import CSDFGraphTestCase

def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(HSDFGraphTestCase))
    suite.addTest(unittest.makeSuite(SDFGraphTestCase))
    # suite.addTest(unittest.makeSuite(CSDFGraphTestCase))
    return suite

if __name__ == '__main__':
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())