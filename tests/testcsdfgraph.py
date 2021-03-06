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
        self.chain_csdf_graph = CSDFGraph()

        # function for n0: accept single token and produce a pair with 0 and 1 added to it
        f_n0 = 'lambda     firecounter, phase: [firecounter]'
        f_n1 = 'lambda xs, firecounter, phase: [xs[0]] if phase == 0 else [xs[0], xs[0]]'
        f_n2 = 'lambda xs, firecounter, phase: [xs[0]] if phase == 0 else [xs[0] + xs[1]]'
        f_n3 = 'lambda xs, firecounter, phase: []'
        self.chain_csdf_graph.add_node('n0', f_n0, (0, 0))
        self.chain_csdf_graph.add_node('n1', f_n1, (100, 0))
        self.chain_csdf_graph.add_node('n2', f_n2, (200, 0))
        self.chain_csdf_graph.add_node('n3', f_n3, (300, 0))

        # connect the nodes in a chain structure
        self.chain_csdf_graph.add_edge('n0', 'n1', 0, 0, [1], [1])
        self.chain_csdf_graph.add_edge('n1', 'n2', 0, 0, [1, 2], [1, 2])
        self.chain_csdf_graph.add_edge('n2', 'n3', 0, 0, [1], [1])


    def test_data_from_two_firings(self):
        # After one firing
        self.chain_csdf_graph.step()
        self.assertEqual(self.chain_csdf_graph.edge['n0']['n1']['tkns'], [0])
        self.assertEqual(self.chain_csdf_graph.edge['n1']['n2']['tkns'], [])
        self.assertEqual(self.chain_csdf_graph.edge['n2']['n3']['tkns'], [])

        # do an other step
        self.chain_csdf_graph.step()
        self.assertEqual(self.chain_csdf_graph.edge['n0']['n1']['tkns'], [1])
        self.assertEqual(self.chain_csdf_graph.edge['n1']['n2']['tkns'], [0])
        self.assertEqual(self.chain_csdf_graph.edge['n2']['n3']['tkns'], [])
        

    def test_data_with_phases(self):
        # After four cycles n1 shoulde have prodcued two tokens in during the second phase
        for i in range(4):
            self.chain_csdf_graph.step()

        # The first two cycles, no data is yet produced by n1.
        # thereafter it produces a single value followed by a pair
        self.assertEqual(self.chain_csdf_graph.edgestates[('n1', 'n2')], [[], [], [0], [1, 1], [2]])

        # the last node, n3, is inactive first until it consumes a single value followed by a sum of two tokens
        self.assertEqual(self.chain_csdf_graph.edgestates[('n2', 'n3')], [[], [], [], [0], [2]])


    def test_expecting_exception_tokens_mismatch(self):
        # When the function in the node produces a number of tokens that does not match 
        #  the current production rate, we should expect an exception

        incorrect_node_func = 'lambda xs, firecounter, phase: xs'
        self.chain_csdf_graph.updateNodeFunction('n1', incorrect_node_func)

        # for the first two cycles, all is okay but after the third cycle, n produces 
        #  one token while two are expected based on the current production rate -> expect exception
        for i in range(2):
            self.chain_csdf_graph.step()
        with self.assertRaises(ValueError):
            self.chain_csdf_graph.step()


    def test_node_firings_storing(self):
        # perform a few iterations to fill up the activation log for each node
        for i in range(7):
            self.chain_csdf_graph.step()

        # firing pattern should be .............
        self.assertEqual(self.chain_csdf_graph.nodefirings['n0'], [True, True, True, True, True, True, True])
        self.assertEqual(self.chain_csdf_graph.nodefirings['n3'], [False, False, False, True, True, True, True])


    def test_test_firecount_state_storage(self):

        # perform 5 cycles of simulation
        for i in range(5):
            self.chain_csdf_graph.step()

        # first node is always active so firecount should increase every cycle:
        self.assertEqual(self.chain_csdf_graph.nodestates['n0'], [0, 1, 2, 3, 4, 5])
        self.assertEqual(self.chain_csdf_graph.nodestates['n3'], [0, 0, 0, 0, 1, 2])

    def test_graph_type_detection(self):
        # initial graph -> CSDF
        self.assertTrue(not self.chain_csdf_graph.isSDF())


def main():
    unittest.main()

if __name__ == '__main__':
    main()