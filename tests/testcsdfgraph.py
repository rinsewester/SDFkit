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
        # create a chain graph with 4 nodes
        self.cycle_cons_sdf_graph = CSDFGraph()

        # function for n0: accept single token and produce a pair with 0 and 1 added to it
        f_n0 = 'lambda     firecounter, phase: [firecounter]'
        f_n1 = 'lambda xs, firecounter, phase: [xs[0]] if phase == 0 else [xs[0], xs[0]]'
        f_n2 = 'lambda xs, firecounter, phase: [xs[0]] if phase == 0 else [xs[0] + xs[1]]'
        f_n3 = 'lambda xs, firecounter, phase: []'
        self.cycle_cons_sdf_graph.add_node('n0', f_n0, (0, 0))
        self.cycle_cons_sdf_graph.add_node('n1', f_n1, (100, 0))
        self.cycle_cons_sdf_graph.add_node('n2', f_n2, (200, 0))
        self.cycle_cons_sdf_graph.add_node('n3', f_n3, (300, 0))

        # connect the nodes in a chain structure
        self.cycle_cons_sdf_graph.add_edge('n0', 'n1', 0, 0, [1], [1])
        self.cycle_cons_sdf_graph.add_edge('n1', 'n2', 0, 0, [1, 2], [1, 2])
        self.cycle_cons_sdf_graph.add_edge('n2', 'n3', 0, 0, [1], [1])


    def test_data_from_two_firings(self):
        # After one firing, .........
        self.cycle_cons_sdf_graph.step()
        # self.assertEqual(self.cycle_cons_sdf_graph.edge['n0']['n1']['tkns'], [])
        # self.assertEqual(self.cycle_cons_sdf_graph.edge['n1']['n2']['tkns'], [])
        # self.assertEqual(self.cycle_cons_sdf_graph.edge['n2']['n3']['tkns'], [])

        # do an other step: ...........
        self.cycle_cons_sdf_graph.step()
        # self.assertEqual(self.cycle_cons_sdf_graph.edge['n0']['n1']['tkns'], [])
        # self.assertEqual(self.cycle_cons_sdf_graph.edge['n1']['n2']['tkns'], [])
        # self.assertEqual(self.cycle_cons_sdf_graph.edge['n2']['n3']['tkns'], [])


    def test_node_firings_storing(self):
        # perform a few iterations to fill up the activation log for each node
        for i in range(7):
            self.cycle_cons_sdf_graph.step()

        # firing pattern should be .............
        # self.assertEqual(self.cycle_cons_sdf_graph.nodefirings['n0'], [True, False, True, False, True, False, True])
        # self.assertEqual(self.cycle_cons_sdf_graph.nodefirings['n1'], [False, True, False, True, False, True, False])


    def test_graph_type_detection(self):
        # initial graph -> CSDF
        self.assertTrue(not self.cycle_cons_sdf_graph.isSDF())


def main():
    unittest.main()

if __name__ == '__main__':
    main()