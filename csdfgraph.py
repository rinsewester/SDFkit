#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Main class that represents a CSDF graph.

author: Rinse Wester

"""

import networkx as nx
import json
import string
import sdfmath
from copy import deepcopy


class CSDFGraph(nx.DiGraph):

    def __init__(self, name=''):

        super().__init__()

        # name of the CSDFGraph
        self.name = name

        # list of clash type definitions 
        self.clashtypes = None

        # state sorage for backtracking in simulations (tokens and counters)
        self.edgestates = {}
        self.nodestates = {}

        # counter to count number of clockcycles: accessable from node
        # functions
        self.clockcount = 0

    def add_edge(self, src, dst, resnr, argnr, prates, crates, tkns=[]):

        super(CSDFGraph, self).add_edge(src, dst)
        self.edge[src][dst]['res'] = resnr
        self.edge[src][dst]['arg'] = argnr
        self.edge[src][dst]['prates'] = prates
        self.edge[src][dst]['crates'] = crates
        self.edge[src][dst]['tkns'] = tkns
        self.edge[src][dst]['itkns'] = []
        self.edgestates[(src, dst)] = [tkns]

    def add_self_edge(self, n, resnr, argnr, prates, crates, tkns=[], angle=0.6):

        self.add_edge(n, n, resnr, argnr, prates, crates, tkns)
        self.edge[n][n]['angle'] = angle

    def add_node(self, n, f, pos, clashcode=''):

        super(CSDFGraph, self).add_node(n)
        self.updateNodeFunction(n, f)
        self.node[n]['clashcode'] = clashcode
        self.node[n]['firecount'] = 0
        self.nodestates[n] = [0]
        self.node[n]['pos'] = pos

    def add_nodes_from(self, ns):

        for n, f, p in ns:
            self.add_node(n, f, p)

    def add_edges_from(self, es):

        for src, dst, resnr, argnr, prates, crates, tkns in es:
            self.add_edge(src, dst, resnr, argnr, prates, crates, tkns)

    def isHSDF(self):
        rates = []
        for src, dst in self.edges():
            rates.append(self[src][dst]['prates'])
            rates.append(self[src][dst]['crates'])

        return all(map(lambda rts: rts == [1], rates))

    def isSDF(self):
        rates = []
        for src, dst in self.edges():
            rates.append(self[src][dst]['prates'])
            rates.append(self[src][dst]['crates'])

        return all(map(lambda rts: len(rts) == 1, rates))

    def isCSDF(self):
        return not self.isSDF()

    def reset(self):

        for (src, dst), states in self.edgestates.items():
            self[src][dst]['tkns'] = states[0]
            del self.edgestates[(src, dst)][1:]

        # reset clcok and firing counters
        for n in self.nodes():
            self.node[n]['firecount'] = 0
            self.nodestates[n] = [0]
        self.clockcount = 0

    def _storestate(self):

        for (src, dst), states in self.edgestates.items():
            states.append(self[src][dst]['tkns'])

        for n, states in self.nodestates.items():
            states.append(self.node[n]['firecount'])

    def stateCount(self):

        e0 = list(self.edgestates.keys())[0]
        return len(self.edgestates[e0])

    def back(self):

        for (src, dst), states in self.edgestates.items():
            self[src][dst]['tkns'] = states[-2]
            states.pop()

        for n, states in self.nodestates.items():
            self.node[n]['firecount'] = states[-2]
            states.pop()

    def step(self):

        # perform single iteration of DF graph
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

        # add all the intermediate buffers to buffers
        for src, dst in self.edges():
            itkns = self[src][dst]['itkns']
            self[src][dst]['tkns'] = itkns + self[src][dst]['tkns']
            self[src][dst]['itkns'] = []

        # store the new state for backtracking
        self._storestate()

        # increase clock for the global clock
        self.clockcount += 1

    # This function converts a list of production/consumption
    # rates to the flat variant: [1, "2*4", 3, "3*2"] becomes:
    # [1, 4, 4, 3, 2, 2, 2]
    def _flattenRateList(lst):

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

    def loadFromFile(self, filename):
        # TODO add some proper validation here:
        #  at least two nodes and one edge, proper connections
        #  and check wether all edges are connected consistenly
        #  to nodearguments and results
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

        for jsnode in jsondata['nodes']:
            nodeName = jsnode['name']
            nodeFunction = jsnode['function']
            nodeClashCode = ''
            if 'clashcode' in jsnode.keys():
                nodeClashCode = jsnode['clashcode']
            nodePosition = jsnode['pos'][0], jsnode['pos'][1]
            self.add_node(nodeName, nodeFunction, nodePosition, clashcode=nodeClashCode)

        for jsedge in jsondata['edges']:
            edgeSource = jsedge['src']
            edgeDestination = jsedge['dst']
            edgeResNumber = jsedge['resnr']
            edgeArgNumber = jsedge['argnr']
            edgePRates = CSDFGraph._flattenRateList(jsedge['prates'])
            edgeCRates = CSDFGraph._flattenRateList(jsedge['crates'])
            edgeTokens = jsedge['tkns']
            if edgeSource == edgeDestination:
                edgeAngle = jsedge['angle']
                self.add_self_edge(
                    edgeSource, edgeResNumber, edgeArgNumber, edgePRates,
                    edgeCRates, edgeTokens, edgeAngle)
            else:
                self.add_edge(
                    edgeSource, edgeDestination, edgeResNumber, edgeArgNumber,
                    edgePRates, edgeCRates, edgeTokens)

    def updateNodeFunction(self, nodename, funcstr):

        self.node[nodename]['funcstr'] = funcstr
        self.node[nodename]['func'] = eval(funcstr)

    def updateTokens(self, edge, tokenstr):
        src, dst = edge
        newtokens = eval(tokenstr)
        self[src][dst]['tkns'] = newtokens

    def updatePRates(self, edge, pratesstr):
        src, dst = edge
        newprates = eval(pratesstr)
        self[src][dst]['prates'] = newprates

    def updateCRates(self, edge, cratesstr):
        src, dst = edge
        newcrates = eval(cratesstr)
        self[src][dst]['crates'] = newcrates

    def print_state(self):

        for src, dst in self.edges():
            tkns = self[src][dst]['tkns']
            print(src, ' --- ', tkns, ' --> ', dst)

    def test(self):

        if self.isHSDF():
            gtype = 'HSDF'
        elif self.isSDF():
            gtype = 'SDF'
        else:
            gtype = 'CSDF'
        print('Test ' + gtype + ' graph ', self.name)

        # Check wether the edges state store and restore works properly
        initEdgeState = deepcopy(self.edgestates)
        self.step()
        self.step()
        self.reset()
        print(
            'Reset check correct: ', initEdgeState == self.edgestates)

        # Check wether the node state store and restore works properly
        self.step()
        secondCntrState = deepcopy(self.nodestates)
        self.step()
        self.step()
        self.back()
        self.back()
        print(
            'Firecount check correct: ', secondCntrState == self.nodestates)
        self.back()

# The default SDF graph
G0 = CSDFGraph()
G0.loadFromFile('examples/SDF/simple graph.json')
# G0.test()
