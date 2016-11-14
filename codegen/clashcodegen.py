#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
class for the CLaSH code generation

author: Rinse Wester

"""

import re
from log import Log
from subprocess import run, PIPE
from sdfmath import is_power2, calc_pointerwidth

class ClashCodeGen(object):
    """Class that generates CLaSH code from a graph object"""

    def __init__(self, arg):
        super(ClashCodeGen, self).__init__()
        self.arg = arg

    def generateCode(graph, targetdir):
        if not graph.isHSDF():
            raise NotImplementedError('Code generation is only supported for HSDF graphs at the moment.')

        edgeTypes = ClashCodeGen._getEdgeTypes(graph)

        predefTypes = ClashCodeGen._generatePredfinedTypes(graph.clashtypes)

        nodeFuncDefs = ClashCodeGen._generateNodeFuncDefs(graph)
        nodeDefs = ClashCodeGen._generateNodeDefs(graph)
        if graph.isHSDF():
            edgeDefs = ClashCodeGen._generateEdgeDefs(graph, edgeTypes)
        else:
            # working on this but first if statement will prevent code from ever reaching this in the online version
            edgeDefs = ClashCodeGen._generateCSDFEdgeDefs(graph, edgeTypes)
        graphType = ClashCodeGen._generateGraphType(graph, edgeTypes)
        nodeInstances = ClashCodeGen._generateNodeInstances(graph)
        edgeInstances = ClashCodeGen._generateEdgeInstances(graph)
        graphConnections = ClashCodeGen._generateGraphConnections(graph)
        graphOutputs = ClashCodeGen._generateGraphOutputs(graph)

        # Open the template file
        with open('codegen/clash_code/template.hs', 'r') as f:
            templatestr = f.read()

        # Now replace the variables in the code template
        templatestr = templatestr.replace('<PREDEFINED_TYPES>', predefTypes)
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
            Log.addLogMessage( Log.INFO, 'CLaSH code generated in ' + filename)

    def _getEdgeTypes(graph):
        edgeTypes = {}
        for src, dst in graph.edges():
            if len(graph.node[src]['clashcode'].splitlines()) < 2:
                raise ValueError('Node ' + src + ' has not enough CLASH code: at least one line for type and one for implementation required')
            _, sourcenodetypes = ClashCodeGen._getCLasHTypes(graph, src, graph.node[src]['clashcode'])
            destnodetypes, _ = ClashCodeGen._getCLasHTypes(graph, dst, graph.node[dst]['clashcode'])
            outpnumber = graph[src][dst]['res']
            inpnumber = graph[src][dst]['arg']

            srctype = sourcenodetypes[outpnumber][0]
            dsttype = destnodetypes[inpnumber][0]
            if srctype != dsttype:
                raise ValueError('Input and output type of edge ' + str((src, dst)) + ' are not equal: src = ' + srctype + ', dst = ' + dsttype)

            srcmaxtokencount = sourcenodetypes[outpnumber][1]
            dstmaxtokencount = destnodetypes[inpnumber][1]

            edgeTypes[(src, dst)] = srctype, (srcmaxtokencount, dstmaxtokencount)
            
        return edgeTypes

    def _generatePredfinedTypes(types):
        resstr = ''
        if types is not None:
            for name, defn in types.items():
                resstr += 'type' + name + ' = ' + defn + '\n'
        else:
            resstr = '--      no predefined types'
        return resstr

    def _typesStr2typetuple(typestr):
        """ Convert a type string to a tuple comtainng the name of the type and number that inidicates the max number of tokens to be sent.

        examples:
            'Byte' -> ('Byte', 1)
            'Vec 4 Nibble' -> ('Nibble', 4)
        """
        typeelements = typestr.split(' ')
        if len(typeelements) > 1:
            return typeelements[2], int(typeelements[1])
        else:
            return typeelements[0], 1

    def _getCLasHTypes(graph, node, code):
        # Get the first line: should be the type definition
        typeline = code.splitlines()[0]
        funcdefline = code.splitlines()[1]

        # regex to match a typename
        if graph.isHSDF():
            typename = r'\w+'
        else:
            typename = r'Vec \d+ \w+'

        # Use regex to validate typestring
        m = re.match(r'^(\w+)\s+\:\:\s+((' + typename + r'\s+\-\>\s+)*)(Cntr\s+\-\>\s+){2}(' + typename + r'|\(\)|\(' + typename + r'(,\s+' + typename + r')+\))$', typeline)
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
            inputtypes = []
        else:
            if graph.isHSDF():
                inputtypes = inptypesstr.replace('->', ' ').split()
            else:
                inputtypes = inptypesstr.split('->')
        # filter and transform the list into a set of tules with name and max tokens to be sent
        inputtypes = filter(lambda s: s != ' ' and s != '', inputtypes)
        inputtypes = list(map(ClashCodeGen._typesStr2typetuple, inputtypes))

        # check wether the number of inputs of the node matches the number of inputs from the CLaSH type
        if len(inputtypes) != len(graph.predecessors(node)):
            raise ValueError('Mismatch between number of inputs in  f_' + node + '(found ' + len(inputtypes) + ' from CLaSH type while the node has ' + len(graph.predecessors(node)) + ' predecessors)' )

        # Get the output types
        outputtypesstr = m.group(5)
        if outputtypesstr == '()':
            # Empty tuple so no types
            outputtypes = []
        elif outputtypesstr.startswith('('):
            # several outputs in tuple
            if graph.isHSDF():
                outputtypes = outputtypesstr[1:-1].replace(',', ' ').split()
            else:
                outputtypes = outputtypesstr[1:-1].split(',')
        else:
            # Single output type 
            outputtypes = [outputtypesstr]
        # filter and transform the list into a set of tules with name and max tokens to be sent
        outputtypes = filter(lambda s: s != ' ' and s != '', outputtypes)
        outputtypes = list(map(ClashCodeGen._typesStr2typetuple, outputtypes))

        # check wether the number of outputs of the node matches the number of outputs from the CLaSH type
        if len(outputtypes) != len(graph.successors(node)):
            raise ValueError('Mismatch between number of output in  f_' + node + '(found ' + len(outputtypes) + ' from CLaSH type while the node has ' + len(graph.successors(node)) + ' successors)' )

        return inputtypes, outputtypes
            
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
                nodeinstanceinputs.append(nname + '_canrd' + str(i))
            for i in range(outputcount):
                nodeinstanceinputs.append(nname + '_canwrt' + str(i))

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
            edgeInstances += "        ({0}_dataout, {0}_canrd, {0}_canwrt) = unbundle $ {0}L $ bundle ({0}_datain, {0}_rd, {0}_wrt)\n".format(ename)
        return edgeInstances

    def _generateCSDFEdgeDefs(graph, edgetypes):
        """Returns a string containing all definitions of the CSDF edges."""
        edgedefs = ''
        for src, dst in graph.edges():

            # Gather the FIFO edge parameters
            edge_datatype = edgetypes[(src, dst)][0]
            edge_capacity = 8 # TODO: make FIFO capacity paramterizable
            edge_prates = graph.edge[src][dst]['prates']
            # A production rate must always be smaller than the capacity: cannot 
            #  not write more elements in the fifo than its capacity.
            if not all(map(lambda prate: prate <= edge_capacity, edge_prates)):
                raise ValueError('Production rate of edge ' + str((src, dst)) + ' exceeds capacity of ' + edge_capacity + ' elements.')
            edge_crates = graph.edge[src][dst]['crates']
            # A consumption rate must always be smaller than the capacity: cannot 
            #  not read more elements from the fifo than its capacity.
            if not all(map(lambda crate: crate <= edge_capacity, edge_crates)):
                raise ValueError('Production rate of edge ' + str((src, dst)) + ' exceeds capacity of ' + edge_capacity + ' elements.')
            edge_tokens = graph.edge[src][dst]['tkns']

            # Construct the types to be used in the generated code
            if not is_power2(edge_capacity):
                raise ValueError('Capacity of edge ' + str((src, dst)) + ' must be a power of two.')
            edge_elements_type = 'Vec {} {}'.format(edge_capacity, edge_datatype)
            edge_token_pointer_type = 'Unsigned {}'.format(calc_pointerwidth(edge_capacity) + 1)
            edge_max_prate = max(edge_prates)
            edge_max_crate = max(edge_crates)
            maxrate = max(edge_max_crate, edge_max_prate)
            edge_prates_max_index = len(edge_prates) - 1
            edge_crates_max_index = len(edge_crates) - 1
            edge_rate_type = 'Unsigned {}'.format(calc_pointerwidth(maxrate + 1))
            productionrates_pointer_type = 'Unsigned {}'.format(calc_pointerwidth(len(edge_prates)))
            consumptionrates_pointer_type = 'Unsigned {}'.format(calc_pointerwidth(len(edge_crates)))

            # TODO: generate the actual code
            print('Code for edge', (src, dst))
            print('---------------------------')
            print('  edge_datatype:', edge_datatype)
            print('  edge_capacity:', edge_capacity)
            print('  edge_prates:', edge_prates)
            print('  edge_crates:', edge_crates)
            print('  edge_tokens:', edge_tokens)
            print('---------------------------')
            print('  edge_elements_type:', edge_elements_type)
            print('  edge_token_pointer_type:', edge_token_pointer_type)
            print('  maxrate:', maxrate)
            print('  edge_rate_type:', edge_rate_type)
            print('  productionrates_pointer_type:', productionrates_pointer_type)
            print('  consumptionrates_pointer_type:', consumptionrates_pointer_type)

            edge_name = 'csdfedge_{}_{}'.format(src, dst)


            # Create the type definition of the current edge
            edgetypedefstr  = "{} :: ({}, {}, {}, {}, {}, {}) ->\n".format(edge_name)
            edgetypedefstr += "    (Vec 3 a, Bool, Bool) ->\n"
            edgetypedefstr += "    ((Vec16 a, RdPtr16, WrtPtr16, TknCntr16, RatesPtr, RatesPtr), (Vec 2 a, Bool, Bool, Bool))\n"

            edgefuncdefstr  = "csdfedge (elms, rptr, wptr, tkncntr, prateptr, crateptr)\n"
            edgefuncdefstr += "    (datain, rd, wrt) = \n"
            edgefuncdefstr += "    ((elms', rptr', wptr', tkncntr', prateptr', crateptr'), (dataout, canread, canwrite, err))\n"

            # TODO: continue here with filling in the body of the edge

        return edgedefs

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
            # add bool types for the 'canrd' input
            for i in range(inputcount):
                ninptypes.append('Bool')
            # add bool types for the 'canwrt' input
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
                ninputs.append('canrd' + str(i))
            for i in range(outputcount):
                ninputs.append('canwrt' + str(i))
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
                nfireconds.append('canrd' + str(i))
            for i in range(outputcount):
                nfireconds.append('canwrt' + str(i))
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

    def _generateEdgeDefs(graph, edgetypes):
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
            tokentype = edgetypes[(src, dst)][0]
            # TODO: Edge capacity now fixed to 8: should come from graph class
            edgedefs += 'e_' + src + '_' + dst +'L = mealy hsdfedge8 (' + tokensstr + ' :: Vec8 ' + tokentype + ', 0 :: RdPtr, ' + str(len(tokens)) + ' :: WrPtr)\n' 

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
                nodeinstanceinputs.append(nname + '_canrd' + str(i))
                # find source node that is connected to this canrd port
                for src in graph.predecessors(n):
                    if graph[src][n]['arg'] == i:
                        nodeinstanceinputsources.append('e_' + src + '_' + n + '_canrd')
            for i in range(outputcount):
                nodeinstanceinputs.append(nname + '_canwrt' + str(i))
                # find destination node that is connected to this canwrt port
                for dst in graph.successors(n):
                    if graph[n][dst]['res'] == i:
                        nodeinstanceinputsources.append('e_' + n + '_' + dst + '_canwrt')

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
            edgetype = edgetypes[(src, dst)][0]
            outputtupletypes.append('Bool')
            outputtupletypes.append(edgetype)

        outputtuplestr = '(' + (', '.join(outputtupletypes)) + ')'
        return 'graph :: Signal Bool -> Signal ' + outputtuplestr


class CLaSHCompiler(object):
    """Class for interaction with the CLaSH compiler to generate VHDL code."""

    VERILOG = 0
    VHDL = 1

    def isavailable():
        try:
            retobj = run(['clash', '--version'], stdout=PIPE, stderr=PIPE)
            return retobj.returncode == 0
        except FileNotFoundError as e:
            return False

    def compile(filename, outputlang):
        """compile the file filename to verilog or VHDL

        outputlang is either CLaSHCompiler.VERILOG or CLaSHCompiler.VHDL"""
        if CLaSHCompiler.isavailable():
            if outputlang == CLaSHCompiler.VHDL:
                lang = 'vhdl'
            else:
                lang = 'verilog'
            retobj = run(['clash', '--' + lang, '-icodegen/clash_code', filename], stderr=PIPE, stdout=PIPE)
            if retobj.returncode != 0:
                raise Exception('An error occurred during ' + lang + ' code generation:\n\n' + str(retobj.stderr))


