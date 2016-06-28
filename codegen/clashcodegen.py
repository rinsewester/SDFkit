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
            print(ClashCodeGen._generateNodeFuncDefs(graph))
            print(ClashCodeGen._generateEdgeDefs(graph))
            print(ClashCodeGen._generateEdgeInstances(graph))

    def _generateEdgeInstances(graph):
        # (e12_dataout, e12_empty, e12_full)      = unbundle $ edge12L $ bundle (e12_datain, e12_rd, e12_wrt)
        edgeInstances = ''
        for src, dst in graph.edges():
            ename = 'e_' + src + '_' + dst
            edgeInstances += "({0}_dataout, {0}_empty, {0}_full) = unbundle $ {0}L $ bundle ({0}_datain, {0}_rd, {0}_wrt)\n".format(ename)
        return edgeInstances

    def _generateNodeFuncDefs(graph):
        functions = ''
        for n in graph.nodes():
            functions += graph.node[n]['clashcode']
            functions += '\n\n'
        return functions

    def _generateEdgeDefs(graph):
        # edge12L = mealy hsdfedge8 (repeat 0 :: Vec8 Cntr, 0 :: RdPtr, 0 :: WrPtr)
        edgedefs = ''
        for src, dst in graph.edges():
            tokens = graph[src][dst]['tkns']
            tokensexp = (tokens + [0,0,0,0,0,0,0,0])[:8]
            tokensstr = ''
            for tkn in tokensexp:
                tokensstr += str(tkn)
                tokensstr += ' <: '
            tokensstr += ' Nil'
            edgedefs += 'e_' + src + '_' + dst +'L = mealy hsdfedge8 (' + tokensstr + ' :: Vec8 Cntr, 0 :: RdPtr, ' + str(len(tokens)) + ' :: WrPtr)\n' 
        return edgedefs
