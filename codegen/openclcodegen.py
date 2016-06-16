#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
class for the OpenCL code generation

author: Rinse Wester

"""


from csdfgraph import *


class OpenCLCodeGen(object):
    """docstring for OpenCLCodeGen"""

    def __init__(self, arg):
        super(OpenCLCodeGen, self).__init__()
        self.arg = arg

    def generateCode(graph, targetdir):
        raise NotImplementedError(
            'OpenCL codegen not implemented yet (target :' + targetdir + ')')
