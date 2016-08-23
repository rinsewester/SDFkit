#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget
from PyQt5.QtCore import QPoint, QRectF
from node import*
from edge import*
from tokenCluster import*
from csdfgraph import*

class Graph(QWidget):

    def __init__(self, graphWidget, scene, view):
        super().__init__()

        self.graphWidget = graphWidget
        self.scene = scene
        self.view = view

        # Test scenes only work when not overruled by
        # "self.graphWidget.setGraph(self.graph)" in mainwindow.py
        testScene = 1   #0 = empty, 1 = complex, 2 = grid, 3 = simple
      
        self.edgeList = []
        self.nodeList = []
        self.clusterList = []

        #Add edges with: (QGraphicsScene(), x1, y1, x2, y2)
        #self.addEdge(scene, 0, 0, 100, 100)

        #Add edges between nodes with: (QGraphicsScene(), outputNode, inputNode)
        # self.addEdgeToNodes(scene, 0, 1)

        # Add nodes with: (QGraphicsScene(), xpos, ypos, nodeName)
        # self.addNode(scene, 0, 0, 'Node name')
        
        #---Complex test scene---
        if testScene == 1:
            self.addNode(0, 0, '1')
            self.addNode(150, 0, 'n2')
            self.addNode(300, 200, 'node 3')
            self.addNode(-100, 200, 'Node 4 <-')
            self.addNode(-150, 100, 'Node 5 name')
            self.addNode(-300, 0, 'Node 6 name here')
            self.addNode(-150, -100, 'Node 7 name here')
            self.addNode(200, -150, 'Node 8 name here')
            self.addNode(650, -50, 'n9')
            self.addNode(350, 100, 'node 10')
            self.addNode(300, 350, 'node 11')
            self.addNode(-300, 200, 'Node 12')
            
            self.addEdgeToNodes(0, 1, 'right', 'left')
            self.addEdgeToNodes(6, 6, 'left', 'right')
            self.addEdgeToNodes(11, 3, 'right', 'right')
            self.addEdgeToNodes(7, 7, 'right', 'left')
            self.addEdgeToNodes(7, 7, 'right', 'left')
            self.addEdgeToNodes(7, 7, 'right', 'left')
            self.addEdgeToNodes(1, 7, 'right', 'left')
            self.addEdgeToNodes(1, 8, 'right', 'left')
            self.addEdgeToNodes(1, 2, 'right', 'left')
            self.addEdgeToNodes(5, 6, 'right', 'left')
            self.addEdgeToNodes(6, 0, 'right', 'left')
            self.addEdgeToNodes(5, 4, 'right', 'left')
            self.addEdgeToNodes(4, 3, 'right', 'left')
            self.addEdgeToNodes(3, 1, 'right', 'left')
            self.addEdgeToNodes(3, 1, 'right', 'left')
            self.addEdgeToNodes(1, 3, 'left', 'right')
            self.addEdgeToNodes(3, 1, 'right', 'left')
            self.addEdgeToNodes(5, 5, 'right', 'left')
            self.addEdgeToNodes(9, 2, 'left', 'right')
            self.addEdgeToNodes(2, 10, 'right', 'right')
            self.addEdgeToNodes(2, 10, 'left', 'left')
            self.addEdgeToNodes(3, 11, 'left', 'left')


        #---Grid test scene---
        if testScene == 2:
            gridSize = 25
            for i in range(gridSize):
                for j in range(gridSize):
                    name = str(i) + '/' + str(j)
                    #print(name)
                    self.addNode(j*150, i*100, name)

            for i in range(gridSize*gridSize - 1):
                self.addEdgeToNodes(i, i + 1, 'right', 'left')


        #---Simple test scene---
        if testScene == 3:
            self.addNode(0, 0, 'n1')
            self.addNode(200, 100, 'n2')
            self.addNode(200, 300, 'n3')
            self.addNode(-200, 300, 'n4')
            self.addNode(-200, 100, 'n5')

            self.addEdgeToNodes(0, 1, 'right', 'left')
            self.addEdgeToNodes(2, 2, 'right', 'left')
            self.addEdgeToNodes(1, 2, 'right', 'right')
            self.addEdgeToNodes(2, 3, 'left', 'right')
            self.addEdgeToNodes(3, 4, 'left', 'left')
            self.addEdgeToNodes(4, 0, 'right', 'left')


    def clearGraph(self):
        #Delete all items from the scene and clear lists
        for i in range(len(self.nodeList)):
            self.scene.removeItem(self.nodeList[i])

        for i in range(len(self.edgeList)):
            self.scene.removeItem(self.edgeList[i])

        for i in range(len(self.clusterList)):
            self.clusterList[i].deleteTokens()
            self.scene.removeItem(self.clusterList[i])

        self.nodeList.clear()
        self.edgeList.clear()
        self.clusterList.clear()
            
    
    def addNode(self, x, y, name, func = []):
        newNode = Node(self, self.view, name, func)
        newNode.setPos(x, y)
        newNode.setZValue(0)
        
        #Add node to the scene and list
        self.scene.addItem(newNode)
        self.nodeList.append(newNode)


    def addEdge(self, beginPoint, endPoint, beginSide, endSide, edgeSelfLoops, src, dst, tokenValues, pRates, cRates):        
        newEdge = Edge(beginPoint, endPoint, beginSide, endSide, edgeSelfLoops, pRates, cRates)

        #Place edges always behind nodes
        newEdge.setZValue(1)

        #Give edge a cluster of tokens
        tokenCluster = TokenCluster(self, self.scene, self.view, newEdge, src, dst, tokenValues)
        self.scene.addItem(tokenCluster)
        self.clusterList.append(tokenCluster)
        
        #Add edge to the scene and list
        self.scene.addItem(newEdge)
        self.edgeList.append(newEdge)

        return newEdge


    def addEdgeToNodes(self, beginNodeIndex, endNodeIndex, beginSide, endSide, src = '', dst = '', tokenValues = [], pRates = 0, cRates = 0):
        beginNode = self.nodeList[beginNodeIndex]
        endNode = self.nodeList[endNodeIndex]

        edgeSelfLoops = False
        if beginNode == endNode:
            edgeSelfLoops = True


        #Get points on the nodes that the edge can connect to
        beginPoint = beginNode.getIOPointForEdge(beginSide, 2)
        endPoint = endNode.getIOPointForEdge(endSide, 1)

        #Set the beginNode as an output
        beginNode.setIOType(beginSide, 2)
        #Set the endNode as an input
        endNode.setIOType(endSide, 1)

        #Add new IO ports to nodes for future edges
        beginNode.addNewIO(beginSide, 0)
        endNode.addNewIO(endSide, 0)
        
        #Create edge between the 2 nodes
        edge = self.addEdge(beginPoint, endPoint, beginSide, endSide, edgeSelfLoops, src, dst, tokenValues, pRates, cRates)

        #Give both nodes a reference to the created edge
        beginNode.addEdge(edge, 'begin')
        endNode.addEdge(edge, 'end')


    def updateTokens(self, edgeIndex, tokenValues):
        self.clusterList[edgeIndex].newTokenValues(tokenValues)


    def editTokens(self, src, dst, newTokens):
        self.graphWidget.editTokens(src, dst, newTokens)


    def editNodeFunction(self, name, newFunction):
        self.graphWidget.editNodeFunction(name, newFunction)

    
    def editPRates(self, src, dst, newPRates):
        self.graphWidget.editPRates(src, dst, newPRates)


    def editCRates(self, src, dst, newCRates):
        self.graphWidget.editCRates(src, dst, newCRates)