#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtCore import QPoint
from node import*
from edge import*

class Graph():

    def __init__(self, scene, view):
        super().__init__()

        self.scene = scene
        testScene = 1

        
        self.edgeList = []
        self.nodeList = []

        #Add edges with: (QGraphicsScene(), x1, y1, x2, y2)
        #self.addEdge(scene, 0, 0, 100, 100)

        #Add edges between nodes with: (QGraphicsScene(), outputNode, inputNode)
        # self.addEdgeToNodes(scene, 0, 1)

        # Add nodes with: (QGraphicsScene(), xpos, ypos, nodeName)
        # self.addNode(scene, 0, 0, 'Node name')
        
        #---Simple test scene---
        if testScene == 1:
            self.addNode(scene, 0, 0, '1')
            self.addNode(scene, 150, 0, 'n2')
            self.addNode(scene, 300, 200, 'node 3')
            self.addNode(scene, 0, 200, 'Node 4 <-')
            self.addNode(scene, -150, 100, 'Node 5 name')
            self.addNode(scene, -300, 0, 'Node 6 name here')
            self.addNode(scene, -150, -100, 'Node 7 name here')
            self.addNode(scene, 200, -100, 'Node 8 name here')
            self.addNode(scene, 350, 0, 'n9')
            
            self.addEdgeToNodes(scene, 0, 1)
            self.addEdgeToNodes(scene, 1, 7)
            self.addEdgeToNodes(scene, 1, 8)
            self.addEdgeToNodes(scene, 1, 2)
            self.addEdgeToNodes(scene, 5, 6)
            self.addEdgeToNodes(scene, 6, 0)
            self.addEdgeToNodes(scene, 5, 4)
            self.addEdgeToNodes(scene, 4, 3)
            self.addEdgeToNodes(scene, 3, 1)
            self.addEdgeToNodes(scene, 3, 1)
            self.addEdgeToNodes(scene, 3, 1)
            self.addEdgeToNodes(scene, 3, 1)
            self.addEdgeToNodes(scene, 5, 5)



        #---Grid test scene---
        if testScene == 2:
            gridSize = 25
            for i in range(gridSize):
                for j in range(gridSize):
                    name = str(i) + '/' + str(j)
                    #print(name)
                    self.addNode(scene, j*150, i*100, name)

            for i in range(gridSize*gridSize - 1):
                self.addEdgeToNodes(scene, i, i + 1)



    def addNode(self, scene, x, y, name):
        newNode = Node(name)
        newNode.setPos(x, y)
        
        #Add node to the scene and list
        scene.addItem(newNode)
        self.nodeList.append(newNode)


    def addEdge(self, scene, beginPoint, endPoint):
        newEdge = Edge(scene, beginPoint, endPoint)

        #Place edges always behind nodes
        newEdge.setZValue(1)
        
        #Add edge to the scene and list
        scene.addItem(newEdge)
        self.edgeList.append(newEdge)

        return newEdge


    def addEdgeToNodes(self, scene, beginNodeIndex, endNodeIndex):
        beginNode = self.nodeList[beginNodeIndex]
        endNode = self.nodeList[endNodeIndex]

        #Get points on the nodes that the edge can connect to
        beginPoint = beginNode.getIOPointForEdge('right', 2)
        endPoint = endNode.getIOPointForEdge('left', 1)

        #Set the beginNode as an output
        beginNode.setIOType('right', 2)
        #Set the endNode as an input
        endNode.setIOType('left', 1)

        #Add new IO ports to nodes for future edges
        beginNode.addNewIO('right', 0)
        endNode.addNewIO('left', 0)
        
        #Create edge between the 2 nodes
        edge = self.addEdge(scene, beginPoint, endPoint)

        #Give both nodes a reference to the created edge
        beginNode.addEdge(edge, 'begin')
        endNode.addEdge(edge, 'end')