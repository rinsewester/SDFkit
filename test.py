#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Validation of the CSDF class usign unittests

author: Rinse Wester

"""

import unittest

from csdfgraph import CSDFGraph

class CSDFGraphTestCase(unittest.TestCase):

    def setUp(self):
        self.prod_cons_hsdf_graph = CSDFGraph()
        # two simple functions for the nodes: one 
        #  just produces numbers while the other just consumes them
        f_prod = 'lambda firecounter, phase: [firecounter]'
        f_cons = 'lambda x, firecounter, phase: []'

        self.prod_cons_hsdf_graph.add_node('P', f_prod, (0, 0))
        self.prod_cons_hsdf_graph.add_node('C', f_cons, (100, 0))

        # connect node P and C using an edge without initial tokens
        self.prod_cons_hsdf_graph.add_edge('P', 'C', 0, 0, [1], [1])


    def test_reset_graph_statecount(self):
        self.prod_cons_hsdf_graph.reset()
        self.assertEqual(self.prod_cons_hsdf_graph.stateCount(), 1)

    def test_data_from_two_firings(self):
        self.prod_cons_hsdf_graph.reset()
        self.prod_cons_hsdf_graph.step()

        # After one firing, data should be on the edge: P -- [0] -> C
        self.assertEqual(self.prod_cons_hsdf_graph.edge['P']['C']['tkns'], [0])

        # do an other step: P should have fired again and C should have consumed the previous token
        self.prod_cons_hsdf_graph.step()
        self.assertEqual(self.prod_cons_hsdf_graph.edge['P']['C']['tkns'], [1])

    def test_edge_state_storing(self):
        self.prod_cons_hsdf_graph.reset()

        # perform 7 steps
        for i in range(7):
            self.prod_cons_hsdf_graph.step()

        self.assertEqual(self.prod_cons_hsdf_graph.edgestates[('P', 'C')], [[], [0], [1], [2], [3], [4], [5], [6]])


if __name__ == '__main__':
    unittest.main()