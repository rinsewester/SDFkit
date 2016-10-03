#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Validation of the CSDF class usign unittests

author: Rinse Wester

"""

import unittest

from csdfgraph import CSDFGraph

class SDFGraphTestCase(unittest.TestCase):

    def setUp(self):
        # create a graph with a cycle
        self.cycle_cons_sdf_graph = CSDFGraph()

        # function for n0: accept single token and produce a pair with 0 and 1 added to it
        f_n0 = 'lambda xs, firecounter, phase: [xs[0], xs[0] + 1]'
        f_n1 = 'lambda xs, firecounter, phase: [xs[0] + xs[1]]'
        self.cycle_cons_sdf_graph.add_node('n0', f_n0, (0, 0))
        self.cycle_cons_sdf_graph.add_node('n1', f_n1, (100, 0))

        # connect node n0 and n1 using two edges to form a cycle with an initial token
        self.cycle_cons_sdf_graph.add_edge('n0', 'n1', 0, 0, [2], [2])
        self.cycle_cons_sdf_graph.add_edge('n1', 'n0', 0, 0, [1], [1], tkns=[0])


    def test_data_from_two_firings(self):
        # After one firing, the token on the edge from n1 to n0 should be consumed
        #  and two tokens should be produced
        self.cycle_cons_sdf_graph.step()
        self.assertEqual(self.cycle_cons_sdf_graph.edge['n1']['n0']['tkns'], [])
        self.assertEqual(self.cycle_cons_sdf_graph.edge['n0']['n1']['tkns'], [0, 1])

        # do an other step: ...........
        self.cycle_cons_sdf_graph.step()
        self.assertEqual(self.cycle_cons_sdf_graph.edge['n1']['n0']['tkns'], [1])
        self.assertEqual(self.cycle_cons_sdf_graph.edge['n0']['n1']['tkns'], [])


    def test_node_firings_storing(self):
        # perform a few iterations to fill up the activation log for each node
        for i in range(7):
            self.cycle_cons_sdf_graph.step()

        # firing pattern should be n0 n1 n0 n1 n0 n1
        self.assertEqual(self.cycle_cons_sdf_graph.nodefirings['n0'], [True, False, True, False, True, False, True])
        self.assertEqual(self.cycle_cons_sdf_graph.nodefirings['n1'], [False, True, False, True, False, True, False])


    def test_graph_type_detection(self):
        # initial graph -> SDF
        self.assertTrue(self.cycle_cons_sdf_graph.isSDF())


def main():
    unittest.main()

if __name__ == '__main__':
    main()