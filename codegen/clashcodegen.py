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
            nodeFuncDefs = ClashCodeGen._generateNodeFuncDefs(graph)
            nodeDefs = ClashCodeGen._generateNodeDefs(graph)
            edgeDefs = ClashCodeGen._generateEdgeDefs(graph)
            nodeInstances = ClashCodeGen._generateNodeInstances(graph)
            edgeInstances = ClashCodeGen._generateEdgeInstances(graph)
            graphConnections = ClashCodeGen._generateGraphConnections(graph)
            graphOutputs = ClashCodeGen._generateGraphOutputs(graph)

            # Open the template file
            with open('codegen/clash_code/template.hs', 'r') as f:
                templatestr = f.read()

            # Now replace the variables in the code template
            templatestr = templatestr.replace('<NODE_FUNC_DEFS>', nodeFuncDefs)
            templatestr = templatestr.replace('<NODE_DEFS>', nodeDefs)
            templatestr = templatestr.replace('<EDGE_DEFS>', edgeDefs)
            templatestr = templatestr.replace('<NODE_INSTANCES>', nodeInstances)
            templatestr = templatestr.replace('<EDGE_INSTANCES>', edgeInstances)
            templatestr = templatestr.replace('<GRAPH_CONNECTIONS>', graphConnections)
            templatestr = templatestr.replace('<GRAPH_OUTPUTS>', graphOutputs)

            # TODO safe in target directory
            print(templatestr)
            
    def _generateNodeInstances(graph):

        return 'Nodeinstances in raph, still TODO'

    def _generateEdgeInstances(graph):

        edgeInstances = ''

        for src, dst in graph.edges():
            ename = 'e_' + src + '_' + dst
            edgeInstances += "        ({0}_dataout, {0}_empty, {0}_full) = unbundle $ {0}L $ bundle ({0}_datain, {0}_rd, {0}_wrt)\n".format(ename)
        return edgeInstances

    def _generateNodeFuncDefs(graph):

        functions = ''

        for n in graph.nodes():
            functions += graph.node[n]['clashcode']
            functions += '\n\n'
        return functions

    def _generateNodeDefs(graph):

        nodedefs = ''

        for n in graph.nodes():

            nname = 'n_' + n
            inputcount = len(graph.predecessors(n))
            outputcount = len(graph.successors(n))

            # create the type def of the node function
            typenames = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
                'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
                'u', 'v', 'w', 'x', 'y', 'z']
            finptypes = typenames[:inputcount]
            foutptypes = typenames[inputcount:inputcount + outputcount]

            # Create the type definition of the node function
            ftypestr = '('
            for t in finptypes:
                ftypestr += t + ' -> '
            ftypestr += 'Cntr -> Cntr -> ('
            ftypestr += ', '.join(foutptypes)
            ftypestr += '))'

            # create the list of input types
            ninptypes = finptypes
            # add bool types for the 'empty' input
            for i in range(inputcount):
                ninptypes.append('Bool')
            # add bool types for the 'full' input
            for i in range(outputcount):
                ninptypes.append('Bool')
            ninptypesstr = '(' + (', '.join(ninptypes)) + ')'

            # Create the list of output types: types for data on edges and bool for fire
            noutptypes = foutptypes + ['Bool']
            noutptypesstr = '(' + (', '.join(noutptypes)) + ')'

            # And now the final string with the type definition of a node
            ntypedef = nname + ' :: ' + ftypestr + ' -> (Cntr, Cntr) -> ' + ninptypesstr + ' -> ((Cntr, Cntr), ' + noutptypesstr + ')'

            # Create the list of inputs
            ninputs = []
            for i in range(inputcount):
                ninputs.append('datain' + str(i))
            for i in range(inputcount):
                ninputs.append('empty' + str(i))
            for i in range(outputcount):
                ninputs.append('full' + str(i))
            ninputsstr = '(' + (', '.join(ninputs)) + ')'

            # Create the list of outputs
            noutputs = []
            for i in range(outputcount):
                noutputs.append('dataout' + str(i))
            noutputs.append('fire')
            noutputsstr = '(' + (', '.join(noutputs)) + ')'

            nfuncline = nname + ' f (firecounter, phase) ' + ninputsstr + " = ((firecounter', phase'), " + noutputsstr + ')'

            # Create the line expressing the firing condition
            nfireconds = []
            for i in range(inputcount):
                nfireconds.append('not empty' + str(i))
            for i in range(outputcount):
                nfireconds.append('not full' + str(i))
            nfirecondsstr = 'fire = ' + (' && '.join(nfireconds))


            # Create the use of the node function
            if outputcount == 0:
                funcresltsstr = '_'
            else:
                nodefuncresults = []
                for i in range(outputcount):
                    nodefuncresults.append('dataout' + str(i))
                funcresltsstr = '(' + (', '.join(nodefuncresults)) + ')'

            nodefuncargsstr = ''
            for i in range(inputcount):
                nodefuncargsstr += 'datain' + str(i) + ' '

            nodefuncstr = funcresltsstr + ' = f ' + nodefuncargsstr + 'firecounter phase'

            # Bring it all together, create a complete definition of a node:
            nodedefs += ntypedef + '\n'
            nodedefs += nfuncline + '\n'
            nodedefs += '    where\n'
            nodedefs += '        ' + nfirecondsstr + '\n'
            nodedefs += "        firecounter' = if fire then firecounter + 1 else firecounter" + '\n'
            nodedefs += "        phase_max = 0" + '\n'
            nodedefs += "        phase' = if fire then (if phase < phase_max then phase + 1 else 0) else phase_max" + '\n'
            nodedefs += '        ' + nodefuncstr + '\n\n'
            nodedefs += nname + 'L = mealy (' + nname + ' f_' + n + ') (0, 0)\n\n'

        return nodedefs

    def _generateEdgeDefs(graph):
        edgedefs = ''

        for src, dst in graph.edges():
            tokens = graph[src][dst]['tkns'][:]
            tokens.reverse()
            tokensexp = (tokens + [0,0,0,0,0,0,0,0])[:8]
            tokensstr = ''
            for tkn in tokensexp:
                tokensstr += str(tkn)
                tokensstr += ' <: '
            tokensstr += ' Nil'
            edgedefs += 'e_' + src + '_' + dst +'L = mealy hsdfedge8 (' + tokensstr + ' :: Vec8 Cntr, 0 :: RdPtr, ' + str(len(tokens)) + ' :: WrPtr)\n' 

        return edgedefs

    def _generateGraphConnections(graph):
        return ''

    def _generateGraphOutputs(graph):
        return ''        