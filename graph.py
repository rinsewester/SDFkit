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

        
        self.edgeList = []
        self.nodeList = []

        #Add edges with: (QGraphicsScene(), x1, y1, x2, y2)
        #self.addEdge(scene, 0, 0, 100, 100)

        #Add edges between nodes with: (QGraphicsScene(), outputNode, inputNode)
        #self.addEdgeToNodes(scene, 0, 1)

        #Add nodes with: (QGraphicsScene(), xpos, ypos, nodeName)
        #self.addNode(scene, 0, 0, 'Node name')
        
        # #---Simple test scene---
        # self.addNode(scene, 0, 0, '1')
        # self.addNode(scene, 150, 0, 'n2')
        # self.addNode(scene, 300, 0, 'node 3')
        # self.addNode(scene, 0, 200, 'Node 4 <-')
        # self.addNode(scene, 150, 200, 'Node 5 name')
        # self.addNode(scene, 300, 200, 'Node 6 name here')
        
        # self.addEdgeToNodes(scene, 0, 1)
        # self.addEdgeToNodes(scene, 1, 2)
        # self.addEdgeToNodes(scene, 5, 4)
        # self.addEdgeToNodes(scene, 4, 3)
        # self.addEdgeToNodes(scene, 3, 1)


        #---Grid test scene---
        gridSize = 50
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


    def addEdge(self, scene, startPoint, endPoint):
        newEdge = Edge(scene, startPoint, endPoint)

        #Place edges always behind nodes
        newEdge.setZValue(-1)
        
        #Add edge to the scene and list
        scene.addItem(newEdge)
        self.edgeList.append(newEdge)


    def addEdgeToNodes(self, scene, startNodeIndex, endNodeIndex):
        startNode = self.nodeList[startNodeIndex]
        endNode = self.nodeList[endNodeIndex]

        #Get points on the nodes that the edge can connect to
        startPoint = startNode.getOutputPointForEdge(0)
        endPoint = endNode.getInputPointForEdge(0)

        self.addEdge(scene, startPoint, endPoint)