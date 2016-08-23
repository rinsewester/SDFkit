#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QDockWidget, QGraphicsView, QGraphicsScene, QApplication, QSplitter, QSlider, QHBoxLayout, QVBoxLayout, QGridLayout, QFrame, QLabel, QToolButton, QButtonGroup, QGraphicsScene, QGraphicsItem
from PyQt5.QtCore import Qt, QSize, QObject, pyqtSignal, QRectF, QPointF
from PyQt5.QtGui import QIcon, QTransform, QColor, QPainter, QBrush, QFont
from graph import*


class GraphicsView(QGraphicsView):
    def __init__(self, widget):
        super().__init__()
        self.widget = widget


    def wheelEvent(self, event):
        #Catch wheelEvent and zoom instead of scroll when Ctrl is pressed
        if event.modifiers() & Qt.ControlModifier:
            if event.angleDelta().y() > 0:
                self.widget.zoomIn(6)
            else:
                self.widget.zoomOut(6)

            event.accept()
        else:
            super().wheelEvent(event)



class GraphicsScene(QGraphicsScene):

    def drawBackground(self, painter, rect):
        painter.setPen(Qt.lightGray)
        #rect = self.sceneRect()

        xGridSize = 512
        yGridSize = 512
        
        for i in range(yGridSize * 2):
            y = i * 20 - (yGridSize) * 20
            painter.drawLine(-xGridSize * 40, y, xGridSize * 40, y)

        for i in range(xGridSize * 2):
            x = i * 40  - (xGridSize) * 40
            painter.drawLine(x, -yGridSize * 20, x, yGridSize * 20)



class GraphWidget(QWidget):

    def __init__(self):
        super().__init__()

        self.initUI()

        
    def initUI(self):

        #setFrameStyle(Sunken | StyledPanel)
        self.graphicsView = GraphicsView(self)
        self.graphicsView.setDragMode(QGraphicsView.RubberBandDrag)
        self.graphicsView.setOptimizationFlags(QGraphicsView.DontSavePainterState)
        self.graphicsView.setViewportUpdateMode(QGraphicsView.SmartViewportUpdate)
        self.graphicsView.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)
        self.graphicsView.setRenderHint(QPainter.Antialiasing, True)

        #Make a graphics scene
        self.scene = GraphicsScene()
        self.graphicsView.setScene(self.scene)

        #Create a graph that can contain nodes and edges
        self.graph = Graph(self, self.scene, self.graphicsView)
        self.tokensInScene = []

        #UI for the graphicsView
        iconSize = QSize(16, 16)

        self.zoomInButton = QToolButton(self)
        self.zoomInButton.setAutoRepeat(True)
        self.zoomInButton.setAutoRepeatInterval(33)
        self.zoomInButton.setAutoRepeatDelay(0);
        self.zoomInButton.setIcon(QIcon('images/zoomin.png'));
        self.zoomInButton.setIconSize(iconSize);
        self.zoomOutButton = QToolButton(self)
        self.zoomOutButton.setAutoRepeat(True);
        self.zoomOutButton.setAutoRepeatInterval(33);
        self.zoomOutButton.setAutoRepeatDelay(0);
        self.zoomOutButton.setIcon(QIcon("images/zoomout.png"));
        self.zoomOutButton.setIconSize(iconSize);

        self.zoomSlider = QSlider(Qt.Vertical, self)
        self.zoomSlider.setRange(25, 350)   #Max zoom out, max zoom in
        self.zoomSlider.setValue(250)

        #Zoom slider layout
        self.resetButton = QToolButton()
        self.resetButton.setText('reset')
        self.resetButton.setEnabled(False)

        zoomSliderLayout = QVBoxLayout()
        zoomSliderLayout.addWidget(self.zoomInButton)
        zoomSliderLayout.addWidget(self.zoomSlider)
        zoomSliderLayout.addWidget(self.zoomOutButton)
        zoomSliderLayout.addWidget(self.resetButton)       

        #Final layout
        topLayout = QGridLayout()
        topLayout.addWidget(self.graphicsView, 1, 0)
        topLayout.addLayout(zoomSliderLayout, 1, 1)
        self.setLayout(topLayout)

        #Connecting
        self.resetButton.clicked.connect(self.resetView)
        self.zoomSlider.valueChanged.connect(self.setupMatrix)
        self.zoomInButton.clicked.connect(self.zoomIn)
        self.zoomOutButton.clicked.connect(self.zoomOut)



    def setGraph(self, graphData):
        if graphData != None:
            self.graphData = graphData

            # set widget size based on min/max positions of nodes
            if not self.graphData is None:
                minX, minY = sys.maxsize, sys.maxsize
                maxX, maxY = 0, 0

                for n in self.graphData.nodes():
                    x, y = self.graphData.node[n]['pos']
                    minX = min(minX, x)
                    minY = min(minY, y)
                    maxX = max(maxX, x)
                    maxY = max(maxY, y)

                self.setMinimumWidth(maxX + 128)
                self.setMinimumHeight(maxY + 128)

                #Determine the center of the graph
                self.centerOfGraph = QPointF((minX + maxX) / 2, (minY + maxY) / 2)

            else:
                self.setMinimumWidth(128)
                self.setMinimumHeight(128)

            self.placeGraphObjects()

            self.update()


    def placeGraphObjects(self):
        #Delete existing objects
        self.graph.clearGraph()
        #Make sure every scene items is deleted
        self.scene.clear()
        self.tokensInScene.clear()

        #Place graph objects based on the graph data
        nodeList = []
        nodePoints = []
        for n in self.graphData.nodes():
            #Place nodes
            x, y = self.graphData.node[n]['pos']
            func = self.graphData.node[n]['funcstr']

            self.graph.addNode(x, y, n, func)
            nPoint = [x, y]
            nodeList.append(n)
            nodePoints.append(nPoint)


        #Check for self-looping edges first
        for src, dst in self.graphData.edges():
            #Place edges and tokens
            node1 = nodeList.index(src)
            node2 = nodeList.index(dst)
            
            if src == dst:
                tokenValues = self.graphData[src][dst]['tkns']
                self.tokensInScene.append((src, dst))    
                    
                self.graph.addEdgeToNodes(node1, node2, 'right', 'left', src, dst, tokenValues)
                
        #Then place the rest of the edges (not self-looping)
        for src, dst in self.graphData.edges():
            #Place edges and tokens
            node1 = nodeList.index(src)
            node2 = nodeList.index(dst)
            
            if src != dst:
                tokenValues = self.graphData[src][dst]['tkns']
                self.tokensInScene.append((src, dst))    

                #If begin node is left of end node
                if nodePoints[node1][0] < nodePoints[node2][0]:
                    self.graph.addEdgeToNodes(node1, node2, 'right', 'left', src, dst, tokenValues)
                elif nodePoints[node1][0] > nodePoints[node2][0]:
                    self.graph.addEdgeToNodes(node1, node2, 'left', 'right', src, dst, tokenValues)
                else:
                    if nodePoints[node1][0] > self.centerOfGraph.x():
                        self.graph.addEdgeToNodes(node1, node2, 'right', 'right', src, dst, tokenValues)
                    else:
                        self.graph.addEdgeToNodes(node1, node2, 'left', 'left', src, dst, tokenValues)
            


    def updateTokensGraph(self):
        #Update tokens after a step
        i = 0
        for src, dst in self.tokensInScene:
            self.graph.updateTokens(i, self.graphData[src][dst]['tkns'])
            i = i + 1


    def editTokens(self, src, dst, newTokens):
        print('Update tokens between: ' + str(src) + '--->' + str(dst) + ' to: ' + str(newTokens))
        self.graphData[src][dst]['tkns'] = newTokens
        newTokens = str(newTokens)
        self.graphData.updateTokens((src, dst), newTokens)

        #Also store the state after the change (Makes it more visual and makes it possible to step back to)
        self.graphData._storestate()
        
        
    def editNodeFunction(self, nodeName, newFunction):
        print('Updated function to: ' + str(newFunction))
        self.graphData.node[nodeName]['funcstr'] = newFunction
        self.graphData.updateNodeFunction(nodeName, newFunction)


    def resetView(self):
        self.zoomSlider.setValue(250)
        self.setupMatrix()
        self.graphicsView.ensureVisible(QRectF(0,0,0,0))

        self.resetButton.setEnabled(False)


    def setResetButtonEnabled(self):
    	self.resetButton.setEnabled(True)


    def setupMatrix(self):
        scale = 2.0 ** ((self.zoomSlider.value() - 250) / 50.0)

        transform = QTransform()
        transform.scale(scale, scale)

        self.graphicsView.setTransform(transform)
        self.setResetButtonEnabled()


    def zoomIn(self, level = 1):
        if not level:
            levelValue = 1
        else:
            levelValue = level

        self.zoomSlider.setValue(self.zoomSlider.value() + levelValue)


    def zoomOut(self, level = 1):
        if not level:
            levelValue = 1
        else:
            levelValue = level

        self.zoomSlider.setValue(self.zoomSlider.value() - levelValue)


