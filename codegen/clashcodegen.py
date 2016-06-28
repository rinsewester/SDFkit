#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
class for the CLaSH code generation

author: Rinse Wester

"""


from csdfgraph import *


class ClashCodeGen(object):
    """docstring for ClashCodeGen"""

    def __init__(self, arg):
        super(ClashCodeGen, self).__init__()
        self.arg = arg

    def generateCode(graph, targetdir):
        if not graph.isHSDF():
            raise NotImplementedError(
                'CLaSH codegen only supported for HSDF graphs')
        else:
            print(ClashCodeGen._generateNodeFuncs(graph))

    def _generateNodeFuncs(graph):
        functions = ''
        for n in graph.nodes():
            functions += graph.node[n]['clashcode']
            functions += '\n\n'
        return functions
