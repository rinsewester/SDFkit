#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
class for the C code generation

author: Rinse Wester

"""


from csdfgraph import *


class CCodeGen(object):
    """docstring for CCodeGen"""

    def __init__(self, arg):
        super(CCodeGen, self).__init__()
        self.arg = arg

    def generateCode(graph, targetdir):
        raise NotImplementedError(
            'C codegen not implemented yet (target :' + targetdir + ')')
