#!/usr/bin/python3
# -*- coding: utf-8 -*-
# author: Rinse Wester(r.wester@utwente.nl)

import networkx as nx
import json
import string
import sdfmath
from log import Log
from copy import deepcopy
from collections import OrderedDict

# TODO: add support for change tracking: weather file has changed -> to enable/disable save button in GUI

class CSDFGraph(nx.DiGraph):
    """
    Class for modeling CSDF graphs.

    The CSDF graph class can be used to model HSDF, SDF and CSDF graphs.
    CSDFGraph is construcuted such that every HSDFG graph is an SDF graph
    and every SDF graph is a CSDF graph.

    A CSDFgraph is a dircted graph where the nodes contain a function that
    perfoems the computations of the application.
    The edges are only used to store data in the form of tokens.

    A graph can be constructed by instantiating a CSDF graph object followed 
    by adding nodes and edges. Consider the following simple producer-consumer
    HSDF graph: 

    (Pr) --> (Cr)

    This graph has two nodes, one edge and no initial tokens. Each node 
    contains a lambda expression as its fucntionality. The producer returns 
    a list of one token every cycle and the consumer accepts tokens returning
    an empty list indicating that no tokens are produced. This simple HSDF 
    graph can be constructed as follows:

    >>> from csdfgraph import CSDFGraph
    >>> G = CSDFGraph()
    >>> f_Pr = 'lambda firecounter, phase: [firecounter]'
    >>> f_Cr = 'lambda inp0, firecounter, phase: []'
    >>> G.add_node('Pr', f_Pr, (0,0))
    >>> G.add_node('Cr', f_Cr, (100,0))
    >>> G.add_edge('Pr', 'Cr', 0, 0, [1], [1])

    The graph is now constructed and simulation can be performed by using the
    step function:

    >>> G.step()

    After one cycle, the producer has fired and the edge should therefore contain
    one token. This can be verified by inspecting the edge edge states. Initially,
    the edge was empy but after one cycle a token is produced:

    >>> G.edgestates
    {('Pr', 'Cr'): [[], [0]]}

    Constructing large graphs using this way can become a tedious process. Therefore,
    CSDFgraph can also load graphs from a JSON formatted file which can be written by
    hand or generated by other tools. Creating a graph obejct based on a file is 
    performed as follows:

    >>> G = CSDFGraph()
    >>> G.loadFromFile('examples/HSDF/producer consumer.json')
    """

    DEFAULT_NODE_COLOR = (230, 230, 255)
    DEFAULT_EDGE_COLOR = (180, 180, 180)

    def __init__(self, name=''):
        """
        Create an empty CSDF graph.

        Parameters
        ----------
        name : graph name
            The name of the garph is only used for code generation.
        """
        super().__init__()

        # name of the CSDFGraph
        self.name = name

        # file from which the graph is loaden and to which chenges are saved
        self.filename = ''

        # state storage for backtracking in simulations (tokens and counters)
        # list of clash type definitions 
        self.clashtypes = None

        # state sorage for backtracking in simulations (tokens and counters)
        self.edgestates = {}
        self.nodestates = {}

        # store whether a node has fired
        self.nodefirings = {}

        # counter to count number of clockcycles: accessable from node
        # functions
        self.clockcount = 0

    def add_edge(self, src, dst, resnr, argnr, prates, crates, tkns=[], color=DEFAULT_EDGE_COLOR):
        """
        Add an edge to the graph.

        To prevent inconsistencies later on, first add nodes to the graph before
        adding edges.

        Parameters
        ----------
        src : source node
        dst : destination node
        resnr : result number
            When a node has multiple outputs, the function that is executed in
            the node returns a tuple. In order to distinguish between outputs,
            resnr is used. For example, when resnr equals 1, the second result
            in the output tuple will be sent using the current edge.
        argnr : argument number
            Similar to resnr, this number indicates to which argument of the
            node function the tokens on the current edge will be sent.
        prates : list of production rates
        crates : list of consumption rates
        tkns : list of initial tokens on the edge
        color: tuple formatted as (r, g, b)
        """
        super(CSDFGraph, self).add_edge(src, dst)
        self[src][dst]['res'] = resnr
        self[src][dst]['arg'] = argnr
        self[src][dst]['prates'] = prates
        self[src][dst]['crates'] = crates
        self[src][dst]['tkns'] = tkns
        self[src][dst]['color'] = color

        self[src][dst]['itkns'] = []
        self.edgestates[(src, dst)] = [tkns]

    def add_node(self, n, f, pos, clashcode='', color=DEFAULT_NODE_COLOR):
        """
        Add a node to the graph.

        When constructing a graph, first add nodes using this method before
        adding edges.

        Parameters
        ----------
        n : name of node
        f : node function
        pos : position in shape of (x,y) tuple
        clashcode : string containing the clash codefor code generation.
        color: tuple formatted as (r, g, b)
        """
        super(CSDFGraph, self).add_node(n)
        self.updateNodeFunction(n, f)
        self.node[n]['pos'] = pos
        self.node[n]['clashcode'] = clashcode
        self.node[n]['color'] = color

        self.node[n]['firecount'] = 0
        self.node[n]['hasfired'] = False
        self.nodestates[n] = [0]
        self.nodefirings[n] = [] # not fired yet
        
    def add_nodes_from(self, ns):
        """
        Add a list of nodes to the graph.

        Parameters
        ----------
        ns : list of nodes
            A list of nodes where each element is a (n,f,p) tuple
            where n is the name, f is the node function and p is the
            position respectively.
        """
        for n, f, p in ns:
            self.add_node(n, f, p)

    def add_edges_from(self, es):
        """
        Add a list of edges to the graph.

        Parameters
        ----------
        es : list of edges
            A list of edges where each element is a (src, dst, resnr,
            argnr, prates, crates, tkns) tuple.
        """
        for src, dst, resnr, argnr, prates, crates, tkns in es:
            self.add_edge(src, dst, resnr, argnr, prates, crates, tkns)

    def isHSDF(self):
        """
        Returns True when the graph is an HSDF graph.

        A graph is an HSDF graph only when all edges
        have a production and consumption rate of 1.
        """
        rates = []
        for src, dst in self.edges():
            rates.append(self[src][dst]['prates'])
            rates.append(self[src][dst]['crates'])

        return all(map(lambda rts: rts == [1], rates))

    def isSDF(self):
        """
        Returns True when the graph is an SDF graph.

        A graph is an SDF graph only when all edges
        have only one production and consumption rate.
        """
        rates = []
        for src, dst in self.edges():
            rates.append(self[src][dst]['prates'])
            rates.append(self[src][dst]['crates'])

        return all(map(lambda rts: len(rts) == 1, rates))

    def isCSDF(self):
        """
        Returns True when the graph is an CSDF graph.

        Any graph that is not an HSDF or SDF graph in
        SDFkit is a CSDF graph, i.e., production and
        consumption rates can be a list of rates.
        """
        return not self.isSDF()

    def reset(self):
        """
        Reset the graph to initial state.

        Undo all simulation steps by retoring the state
        (firecopunters and tokens on edges) back to the
        initial state of the graph.
        """
        for (src, dst), states in self.edgestates.items():
            self[src][dst]['tkns'] = states[0]
            del self.edgestates[(src, dst)][1:]

        # reset clcok and firing counters
        for n in self.nodes():
            self.node[n]['firecount'] = 0
            self.nodestates[n] = [0]
            self.nodefirings[n] = []
        self.clockcount = 0

    def _storestate(self):

        for (src, dst), states in self.edgestates.items():
            states.append(self[src][dst]['tkns'])

        for n, states in self.nodestates.items():
            states.append(self.node[n]['firecount'])
            
        for n in self.nodes():
            self.nodefirings[n].append(self.node[n]['hasfired'])

    def stateCount(self):
        """
        Returns the number of states that are stored during sumilation.

        The number of states are the one for the initial state and the 
        number of times the graph has prgressed using the step() method.
        """
        e0 = list(self.edgestates.keys())[0]
        return len(self.edgestates[e0])

    def back(self):
        """
        Go back one simulation step.
        """
        for (src, dst), states in self.edgestates.items():
            self[src][dst]['tkns'] = states[-2]
            states.pop()

        for n, states in self.nodestates.items():
            self.node[n]['firecount'] = states[-2]
            states.pop()

        for n in self.nodes():
            self.node[n]['hasfired'] = self.nodefirings[n][-2]
            self.nodefirings[n].pop()

    def step(self):
        """
        Perform one iteration of the graph.

        During a single iteration step, for all nodes the
        firing codtion is checked. After that all nodes that
        can fire consume the tokens on the inputs and produce
        the results in the form of a list of output tokens 
        which are put on the edges.
        """
        for n in self.nodes():
            canfire = True
            source_nodes = []
            for src in self.predecessors(n):
                source_nodes.append(src)
                phase = self.node[n]['firecount'] % len(self[src][n]['crates'])
                canfire &= len(self[src][n]['tkns']) >= self[
                    src][n]['crates'][phase]
            if canfire:
                # collect arguments
                args = []
                arg_inds = []
                for src in source_nodes:
                    arg_inds.append(self[src][n]['arg'])
                    phase = self.node[n]['firecount'] % len(
                        self[src][n]['crates'])
                    crate = self[src][n]['crates'][phase]
                    if crate > 0:
                        args.append(self[src][n]['tkns'][-crate:])
                        self[src][n]['tkns'] = self[src][n]['tkns'][:-crate]                      
                    else:
                        args.append([])
                # order arguments
                args_sorted = [0] * len(args)
                for i, v in zip(arg_inds, args):
                    args_sorted[i] = v

                # Determine the phase, i.e., lcm of length of all consumption
                # and production rates of current node
                ratelengths = []
                for src in self.predecessors(n):
                    ratelengths.append(len(self[src][n]['crates']))
                for dst in self.successors(n):
                    ratelengths.append(len(self[n][dst]['prates']))
                phase = self.node[n]['firecount'] % sdfmath.lcm(ratelengths)

                # execute node function where clockcount and firecount are
                # variables that can be used inside the function of a node
                res = self.node[n]['func'](
                    *args_sorted, firecounter=self.node[n]['firecount'],
                    phase=phase)
                if type(res) is tuple:
                    results = list(res)
                else:
                    results = [res]

                # add result to intermediate buffers connected to succeeding
                # nodes
                for dest in self.successors(n):
                    phase = self.node[n]['firecount'] % len(self[n][dest]['prates'])
                    prate = self[n][dest]['prates'][phase]
                    resnr = self[n][dest]['res']
                    if len(results[resnr]) != prate:
                        raise ValueError('Mismatch between number of tokens produced and production rate: ', 'edge: ', (n, dest), 'prate: ', prate, 'rescount', len(results[resnr]) )
                    self[n][dest]['itkns'] = results[self[n][dest]['res']]

                # Firing of node complete
                self.node[n]['firecount'] += 1

            # Store whether the node has fired: for activity log of nodes
            self.node[n]['hasfired'] = canfire

        # add all the intermediate buffers to buffers
        for src, dst in self.edges():
            itkns = self[src][dst]['itkns']
            self[src][dst]['tkns'] = itkns + self[src][dst]['tkns']
            self[src][dst]['itkns'] = []

        # store the new state for backtracking
        self._storestate()

        # increase clock for the global clock
        self.clockcount += 1

    def _flattenRateList(lst):
        """
        Returns the a flattened list of rates.

        This function converts a list of production/consumption
        rates to the flat variant: [1, "2*4", 3, "3*2"] becomes:
        [1, 4, 4, 3, 2, 2, 2]
        """
        res = []
        for elm in lst:
            if type(elm) is str:
                replfac, rate = elm.split('*')
                replfac = int(replfac)
                rate = int(rate)
                elms = [rate] * replfac
                res.extend(elms)
            else:
                res.append(elm)
        return res

    def validateGraph(self):
        """Validates the graph.

        This methods checks if the incoming edges match the function in the node.

        Raises
        ------
        ValueError
            When there is a mismatch between incoming edge argument numbers and the
            number of arguments of the node fucntion.
        """
        for n in self.nodes():
            nodefunc = self.node[n]['func']
            nodefuncstr = self.node[n]['funcstr']

            # node must have a lmabda expression as its functionality
            if not nodefuncstr.startswith('lambda'):
                raise ValueError('Node ' + n + ' does not have a valid lambda function.')

            nodefuncargcount = nodefunc.__code__.co_argcount
            srccount = len(list(self.predecessors(n)))

            # for every data argument of the node function, there should be an incoming edge
            if (nodefuncargcount - 2) != srccount:
                raise ValueError('Node ' + n + ' has a function with ' + str(nodefuncargcount - 2) + ' data arguments but has ' + str(srccount) + ' sources')

            argnrs = []
            for p in self.predecessors(n):
                argnrs.append(self[p][n]['arg'])

            if set(argnrs) != set(range(nodefuncargcount - 2)):
                raise ValueError('Every argument should have a corresponding unique source, this is not the case for node ' + n)


    def loadFromFile(self, filename):
        """
        Loads a graph from a JSON file.

        Parameters
        ----------
        filename : string with file path

        Raises
        ------
        ValueError
            When the graph is inconsistent as detected by the validateGraph() method.
        """
        Log.addLogMessage(Log.INFO, 'Opened grap ' + filename)
        self.filename = filename
        with open(filename, 'r') as f:
            jsonstr = f.read()

        jsondata = json.loads(jsonstr)

        # make sure the name becomes camelcase without spaces: required by CLaSH
        namestr = jsondata['name'].strip()
        namestr = string.capwords(namestr)
        namestr = namestr.replace(' ', '')
        self.name = namestr

        # Load the predefined clash types when available
        if 'clashtypes' in jsondata.keys():
            self.clashtypes = jsondata['clashtypes']

        # Load all nodes and their attributes
        for jsnode in jsondata['nodes']:
            nodeName = jsnode['name']
            nodeFunction = jsnode['function']
            nodeClashCode = ''
            if 'clashcode' in jsnode.keys():
                nodeClashCode = jsnode['clashcode']
            nodeColor = self.DEFAULT_NODE_COLOR
            if 'color' in jsnode.keys():
                nodeColor = jsnode['color']
            nodePosition = jsnode['pos'][0], jsnode['pos'][1]
            self.add_node(nodeName, nodeFunction, nodePosition, clashcode=nodeClashCode, color=nodeColor)

        # Load all edges and their attributes
        for jsedge in jsondata['edges']:
            edgeSource = jsedge['src']
            edgeDestination = jsedge['dst']
            edgeResNumber = jsedge['resnr']
            edgeArgNumber = jsedge['argnr']
            edgePRates = CSDFGraph._flattenRateList(jsedge.get('prates', [1]))
            edgeCRates = CSDFGraph._flattenRateList(jsedge.get('crates', [1]))
            edgeTokens = jsedge.get('tkns', [])
            edgeColor = self.DEFAULT_EDGE_COLOR
            if 'color' in jsedge.keys():
                edgeColor = jsedge['color']
            self.add_edge(
                    edgeSource, edgeDestination, edgeResNumber, edgeArgNumber,
                    edgePRates, edgeCRates, edgeTokens, color=edgeColor)

        # Now that the graph is construcuted, validate it:  
        self.validateGraph()
            
    def storeToFile(self, filename=''):
        """
        Stores the current graph in a JSON file.

        Parameters
        ----------
        filename : string with filepath
            filename is an optional argument containing the file in which graph is stored.
            When this argument is not used, the graph is stored in the file from  which it
            was initially read.
        """
        if filename == '':
            # no file name given so use file from which this graph is made
            fname = self.filename
        else:
            fname = filename

        # Put all info into a temporary dictionary which will be transformed into a json string
        graphDict = OrderedDict({})

        # First save graph properties/attributes: name and predefined CLaSH types
        graphDict['name'] = self.name
        if self.clashtypes is not None:
            graphDict['clashtypes'] = self.clashtypes

        # Store all the nodes of the graph in the temporary dictionary
        nodesList = []
        for nname in self.nodes():
            nodedict = OrderedDict({})
            nodedict['name'] = nname
            nodedict['function'] = self.node[nname]['funcstr']
            if self.node[nname]['clashcode'] != '':
                nodedict['clashcode'] = self.node[nname]['clashcode']
            nodedict['pos'] = list(self.node[nname]['pos'])
            nodedict['color'] = self.node[nname]['color']
            nodesList.append(nodedict)

        # add all nodes to temporary dict in form of a list
        graphDict['nodes'] = nodesList

        # Store all the edges of the graph in the temporary dictionary
        edgesList = []
        for srcname, dstname in self.edges():
            edgedict = OrderedDict({})
            edgedict['src'] = srcname
            edgedict['dst'] = dstname
            edgedict['resnr'] = self[srcname][dstname]['res']
            edgedict['argnr'] = self[srcname][dstname]['arg']
            edgedict['prates'] = self[srcname][dstname]['prates']
            edgedict['crates'] = self[srcname][dstname]['crates']
            edgedict['tkns'] = self[srcname][dstname]['tkns']
            edgedict['color'] = self[srcname][dstname]['color']
            edgesList.append(edgedict)

        # add all edges to temporary dict in form of a list
        graphDict['edges'] = edgesList

        # Last but not leat, write the graph to the file
        with open(fname, 'w') as outfile:
            json.dump(graphDict, outfile, indent=4)
            Log.addLogMessage(Log.INFO, 'Saved graph ' + fname)

    def updateNodeFunction(self, nodename, funcstr):
        """
        Updates the function in the node.

        Parameters
        ----------
        nodename : string containing node name
        funcstr : string with lambda
        """
        self.node[nodename]['funcstr'] = funcstr
        self.node[nodename]['func'] = eval(funcstr)

    def updateClashCode(self, nodename, clashcode):
        """
        Changes the CLaSH code of a node.
        """
        self.node[nodename]['clashcode'] = clashcode

    def updateTokens(self, edge, tokenstr):
        """
        Changes the tokens on a particular edge.
        """
        src, dst = edge
        newtokens = eval(tokenstr)
        self[src][dst]['tkns'] = newtokens       

    def updatePRates(self, edge, pratesstr):
        """
        Changes the the list of production rates.
        """
        src, dst = edge
        newprates = eval(pratesstr)
        self[src][dst]['prates'] = newprates

    def updateCRates(self, edge, cratesstr):
        """
        Changes the the list of consumption rates.
        """
        src, dst = edge
        newcrates = eval(cratesstr)
        self[src][dst]['crates'] = newcrates
