#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
class for the CLaSH code generation

author: Rinse Wester

"""


from csdfgraph import *
import re

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
            edgeTypes = ClashCodeGen._getEdgeTypes(graph)

            nodeFuncDefs = ClashCodeGen._generateNodeFuncDefs(graph)
            nodeDefs = ClashCodeGen._generateNodeDefs(graph)
            edgeDefs = ClashCodeGen._generateEdgeDefs(graph)
            graphType = ClashCodeGen._generateGraphType(graph, edgeTypes)
            nodeInstances = ClashCodeGen._generateNodeInstances(graph)
            edgeInstances = ClashCodeGen._generateEdgeInstances(graph)
            graphConnections = ClashCodeGen._generateGraphConnections(graph)
            graphOutputs = ClashCodeGen._generateGraphOutputs(graph)

            # Open the template file
            with open('codegen/clash_code/template.hs', 'r') as f:
                templatestr = f.read()

            # Now replace the variables in the code template
            templatestr = templatestr.replace('<GRAPH_NAME>', graph.name)
            templatestr = templatestr.replace('<NODE_FUNC_DEFS>', nodeFuncDefs)
            templatestr = templatestr.replace('<NODE_DEFS>', nodeDefs)
            templatestr = templatestr.replace('<EDGE_DEFS>', edgeDefs)
            templatestr = templatestr.replace('<GRAPH_TYPE>', graphType)
            templatestr = templatestr.replace('<NODE_INSTANCES>', nodeInstances)
            templatestr = templatestr.replace('<EDGE_INSTANCES>', edgeInstances)
            templatestr = templatestr.replace('<GRAPH_CONNECTIONS>', graphConnections)
            templatestr = templatestr.replace('<GRAPH_OUTPUTS>', graphOutputs)

            # write the resulting CLaSH code in a file
            filename = graph.name.lower() + '.hs'
            with open('codegen/clash_code/' + filename, 'w') as f:
                f.write(templatestr)
                print('Generating CLaSH code in ', filename)

    def _getEdgeTypes(graph):
        edgeTypes = {}
        for src, dst in graph.edges():
            nodeoutptypes = ClashCodeGen._getCLasHOutputTypes(graph, src, graph.node[src]['clashcode'])
            outpnumber = graph[src][dst]['res']
            edgeTypes[(src, dst)] = nodeoutptypes[outpnumber]
            
        return edgeTypes


    def _getCLasHOutputTypes(graph, node, code):
        # Get the first line: should be the type definition
        typeline = code.splitlines()[0]
        funcdefline = code.splitlines()[1]

        # Remove whitespaces: makes regex better to read
        typeline = typeline.replace(' ', '')
        typeline = typeline.replace('\t', '')

        # Use regex to validate typestring
        m = re.match(r'^(\w+)\:\:((\w+\-\>)*)(Cntr\-\>){2}(\w+|\(\)|\(\w+(,\w+)+\))$', typeline)
        if m == None:
            raise ValueError('CLASH type definition incorrrect for ' + node)

        # Validate function name
        expname = 'f_' + node
        typefname = m.group(1)
        funcdeffname = funcdefline.split()[0]

        if typefname != expname or funcdeffname != expname:
            raise ValueError('Incorrrect naming of CLaSH function, expecting f_' + node )

        # Get the input types
        inptypesstr = m.group(2)
        if inptypesstr == '':
            inptypes = []
        else:
            inptypes = inptypesstr.replace('->', ' ').split()

        # check wether the number of inputs of the node matches the number of inputs from the CLaSH type
        if len(inptypes) != len(graph.predecessors(node)):
            raise ValueError('Mismatch between number of inputs in  f_' + node + '(found ' + len(inptypes) + ' from CLaSH type while the node has ' + len(graph.predecessors(node)) + ' predecessors)' )

        # Get the output types
        outputtypesstr = m.group(5)
        if outputtypesstr == '()':
            # Empty tuple so no types
            outputtypes = []
        elif outputtypesstr.startswith('('):
            # several outputs in tuple
            outputtypes = outputtypesstr[1:-1].replace(',', ' ').split()
        else:
            # Single output type 
            outputtypes = [outputtypesstr]

        # check wether the number of outputs of the node matches the number of outputs from the CLaSH type
        if len(outputtypes) != len(graph.successors(node)):
            raise ValueError('Mismatch between number of output in  f_' + node + '(found ' + len(outputtypes) + ' from CLaSH type while the node has ' + len(graph.successors(node)) + ' successors)' )

        return outputtypes
            
    def _generateNodeInstances(graph):

        nodeinstancesstr = ''

        for n in graph.nodes():

            nname = 'n_' + n
            inputcount = len(graph.predecessors(n))
            outputcount = len(graph.successors(n))

            # Create the string for the output tuple
            nodeinstanceoutputs = []
            for i in range(outputcount):
                nodeinstanceoutputs.append(nname + '_dataout' + str(i))
            nodeinstanceoutputs.append(nname + '_fire')

            if len(nodeinstanceoutputs) == 1:
                nodeinstanceoutputsstr = nodeinstanceoutputs[0]
            else:
                nodeinstanceoutputsstr = '(' + (', '.join(nodeinstanceoutputs)) + ')'

            # Create the string for the input tuple
            nodeinstanceinputs = []
            for i in range(inputcount):
                nodeinstanceinputs.append(nname + '_datain' + str(i))
            for i in range(inputcount):
                nodeinstanceinputs.append(nname + '_empty' + str(i))
            for i in range(outputcount):
                nodeinstanceinputs.append(nname + '_full' + str(i))

            if len(nodeinstanceinputs) == 1:
                nodeinstanceinputsstr = nodeinstanceinputs[0]
            else:
                nodeinstanceinputsstr = '(' + (', '.join(nodeinstanceinputs)) + ')'

            # Now create th actual string of the instance
            nodeinstancesstr += '        ' + nodeinstanceoutputsstr + ' ='
            nodeinstancesstr += '' if (inputcount == 1 and outputcount == 0) else ' unbundle $'
            nodeinstancesstr += ' ' + nname + 'L'
            nodeinstancesstr += '' if (inputcount == 0 and outputcount == 1) else ' $ bundle'
            nodeinstancesstr += ' ' + nodeinstanceinputsstr + '\n'

        return nodeinstancesstr

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
                tokensstr += ' :> '
            tokensstr += ' Nil'
            # TODO: add proper type of element to vector definition
            edgedefs += 'e_' + src + '_' + dst +'L = mealy hsdfedge8 (' + tokensstr + ' :: Vec8 Cntr, 0 :: RdPtr, ' + str(len(tokens)) + ' :: WrPtr)\n' 

        return edgedefs

    def _generateGraphConnections(graph):
        graphconnectionsstr = ''

        for n in graph.nodes():

            nname = 'n_' + n
            inputcount = len(graph.predecessors(n))
            outputcount = len(graph.successors(n))

            # Create the string connecting the input tuple of a node with other nodes
            nodeinstanceinputs = []
            nodeinstanceinputsources = []
            for i in range(inputcount):
                nodeinstanceinputs.append(nname + '_datain' + str(i))
                # find source node that is connected to this datain(i) port
                for src in graph.predecessors(n):
                    if graph[src][n]['arg'] == i:
                        nodeinstanceinputsources.append('e_' + src + '_' + n + '_dataout')
            for i in range(inputcount):
                nodeinstanceinputs.append(nname + '_empty' + str(i))
                # find source node that is connected to this empty port
                for src in graph.predecessors(n):
                    if graph[src][n]['arg'] == i:
                        nodeinstanceinputsources.append('e_' + src + '_' + n + '_empty')
            for i in range(outputcount):
                nodeinstanceinputs.append(nname + '_full' + str(i))
                # find destination node that is connected to this full port
                for dst in graph.successors(n):
                    if graph[n][dst]['res'] == i:
                        nodeinstanceinputsources.append('e_' + n + '_' + dst + '_full')

            if len(nodeinstanceinputs) == 1:
                nodeinstanceinputsstr = nodeinstanceinputs[0]
            else:
                nodeinstanceinputsstr = '(' + (', '.join(nodeinstanceinputs)) + ')'

            if len(nodeinstanceinputsources) == 1:
                nodeinstanceinputsourcesstr = nodeinstanceinputsources[0]
            else:
                nodeinstanceinputsourcesstr = '(' + (', '.join(nodeinstanceinputsources)) + ')'

            # And now assign the correct other signals to them.....
            graphconnectionsstr += '        ' + nodeinstanceinputsstr + ' = ' + nodeinstanceinputsourcesstr + '\n'

        
        # Create the string for the input tuples of the edges
        for src, dst in graph.edges():

            ename = 'e_' + src + '_' + dst
            edgeinptuplestr = '(' + ename + '_datain, ' + ename + '_rd, ' + ename + '_wrt)'

            resnumber = graph[src][dst]['res']
            edgeinptuplesourcesstr = '(n_' + src + '_dataout' + str(resnumber) + ', n_' + dst + '_fire, n_' + src + '_fire)' 

            graphconnectionsstr += '        ' + edgeinptuplestr + ' = ' + edgeinptuplesourcesstr + '\n'

        return graphconnectionsstr

    def _generateGraphOutputs(graph):
        graphOutputs = []
        for src, dst in graph.edges():
            graphOutputs.append('e_' + src + '_' + dst + '_rd, e_' + src + '_' + dst + '_dataout')
        graphOutputsStr = '(' + (', '.join(graphOutputs)) + ')'
        return graphOutputsStr

    def _generateGraphType(graph, edgetypes):
        outputtupletypes = []
        for src, dst in graph.edges():
            edgetype = edgetypes[(src, dst)]
            outputtupletypes.append('Bool')
            outputtupletypes.append(edgetype)

        outputtuplestr = '(' + (', '.join(outputtupletypes)) + ')'
        return 'graph :: Signal Bool -> Signal ' + outputtuplestr