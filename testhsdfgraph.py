#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Validation of the CSDF class usign unittests

author: Rinse Wester

"""

import unittest

from csdfgraph import CSDFGraph

class HSDFGraphTestCase(unittest.TestCase):

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
        # perform a few steps foloowed by a reset: should be back in initial state
        self.prod_cons_hsdf_graph.reset()
        self.assertEqual(self.prod_cons_hsdf_graph.stateCount(), 1)

    def test_data_from_two_firings(self):
        # After one firing, data should be on the edge: P -- [0] -> C
        self.prod_cons_hsdf_graph.step()
        self.assertEqual(self.prod_cons_hsdf_graph.edge['P']['C']['tkns'], [0])

        # do an other step: P should have fired again and C should have consumed the previous token
        self.prod_cons_hsdf_graph.step()
        self.assertEqual(self.prod_cons_hsdf_graph.edge['P']['C']['tkns'], [1])

    def test_edge_state_storing(self):
        # perform 7 steps
        for i in range(7):
            self.prod_cons_hsdf_graph.step()

        # the log of the edge P --> C should contain an empty element followed by a secuence from 0 to 7
        self.assertEqual(self.prod_cons_hsdf_graph.edgestates[('P', 'C')], [[], [0], [1], [2], [3], [4], [5], [6]])

    def test_node_firings_storing(self):
        # perform a few iterations to fill up the activation log for each node
        for i in range(4):
            self.prod_cons_hsdf_graph.step()

        # P should have firied all iterations while 
        self.assertEqual(self.prod_cons_hsdf_graph.nodefirings['P'], [True, True, True, True])
        self.assertEqual(self.prod_cons_hsdf_graph.nodefirings['C'], [False, True, True, True])

    def test_graph_type_detection(self):
        # initial graph -> HSDF
        self.assertTrue(self.prod_cons_hsdf_graph.isHSDF())

        # change the prodcution and consumption rates such that the graph becomes an SDF graph
        self.prod_cons_hsdf_graph.edge['P']['C']['prates'] = [2]
        self.prod_cons_hsdf_graph.edge['P']['C']['crates'] = [3]
        self.assertTrue(self.prod_cons_hsdf_graph.isSDF() 
            and not self.prod_cons_hsdf_graph.isHSDF())

        # change the prodcution and consumption rates such that the graph becomes a CSDF graph
        self.prod_cons_hsdf_graph.edge['P']['C']['prates'] = [2, 3, 7]
        self.prod_cons_hsdf_graph.edge['P']['C']['crates'] = [3, 11]
        self.assertTrue(self.prod_cons_hsdf_graph.isCSDF() 
            and not self.prod_cons_hsdf_graph.isSDF())

def main():
    unittest.main()

if __name__ == '__main__':
    main()