#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsView, QGraphicsScene, QSlider, QToolButton, QVBoxLayout, QGridLayout
from PyQt5.QtCore import Qt, QSize, QRectF, QPointF
from PyQt5.QtGui import QIcon, QTransform, QPainter
from graph import Graph

class GraphicsView(QGraphicsView):
    def __init__(self, widget):
        super().__init__()
        self.widget = widget

    def wheelEvent(self, event):
        #Catch wheelEvent and zoom instead of scroll when Ctrl is pressed
        if event.modifiers() & Qt.ControlModifier:
            if event.angleDelta().y() > 0:
                self.widget.zoomIn()
            else:
                self.widget.zoomOut()

            event.accept()
        else:
            super().wheelEvent(event)

    def keyPressEvent(self, event):
        if event.modifiers() & Qt.ShiftModifier:
            self.setInteractive(False)
            self.setDragMode(True)           

        super().keyPressEvent(event)

    def keyReleaseEvent(self, event):
        self.setInteractive(True)
        self.setDragMode(False)
        self.setDragMode(QGraphicsView.RubberBandDrag)

        super().keyReleaseEvent(event)


class GraphicsScene(QGraphicsScene):

    def __init__(self):
        super().__init__()

        self.drawGrid = True
        self.lockScene = False

        self.changed.connect(self.updateSceneRect)

    def drawBackground(self, painter, rect):
        if self.drawGrid:
            painter.setPen(Qt.lightGray)

            xGridSize = 512
            yGridSize = 512
            
            for i in range(yGridSize * 2):
                y = i * 20 - (yGridSize) * 20
                painter.drawLine(-xGridSize * 40, y, xGridSize * 40, y)

            for i in range(xGridSize * 2):
                x = i * 40  - (xGridSize) * 40
                painter.drawLine(x, -yGridSize * 20, x, yGridSize * 20)

    def updateSceneRect(self):
        #Is called when there is a change in the scene
        #Update scene size to fit the current layout of the graph
        if not self.lockScene:
            rect = self.itemsBoundingRect()
            rect = QRectF(rect.x() - 50, rect.y() - 50, rect.width() + 100, rect.height() + 100)
            self.setSceneRect(rect)
        else:
            self.lockScene = False

class GraphWidget(QWidget):

    def __init__(self):
        super().__init__()

        self.initUI()
        
    def initUI(self):
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
        self.zoomInButton.setAutoRepeat(False)
        self.zoomInButton.setAutoRepeatInterval(33)
        self.zoomInButton.setAutoRepeatDelay(0);
        self.zoomInButton.setIcon(QIcon('images/zoomin.png'));
        self.zoomInButton.setIconSize(iconSize);

        self.zoomOutButton = QToolButton(self)
        self.zoomOutButton.setAutoRepeat(False);
        self.zoomOutButton.setAutoRepeatInterval(33);
        self.zoomOutButton.setAutoRepeatDelay(0);
        self.zoomOutButton.setIcon(QIcon("images/zoomout.png"));
        self.zoomOutButton.setIconSize(iconSize);

        self.zoomSlider = QSlider(Qt.Vertical, self)
        self.zoomSlider.setRange(25, 350)   #Max zoom out, max zoom in
        self.zoomSlider.setValue(250)

        #Zoom slider layout
        zoomSliderLayout = QVBoxLayout()
        zoomSliderLayout.addWidget(self.zoomInButton)
        zoomSliderLayout.addWidget(self.zoomSlider)
        zoomSliderLayout.addWidget(self.zoomOutButton)

        #Final layout
        topLayout = QGridLayout()
        topLayout.addWidget(self.graphicsView, 1, 0)
        topLayout.addLayout(zoomSliderLayout, 1, 1)
        self.setLayout(topLayout)

        #Connecting
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
                    minX = min(minX, x - 50)
                    minY = min(minY, y - 50)
                    maxX = max(maxX, x + 50)
                    maxY = max(maxY, y + 50)

                # Determine the center of the graph
                self.centerOfGraph = QPointF((minX + maxX) / 2, (minY + maxY) / 2)

            self.placeGraphObjects()

            #Resize scene to be slightly larger than the graph
            self.scene.updateSceneRect()

            self.resetView()
            self.update()
     
    def placeGraphObjects(self):
        
        self.graph.clearGraph()
        self.scene.clear()
        self.tokensInScene.clear()

        #Place graph objects based on the graph data
        nodeList = []
        nodePoints = []
        for n in self.graphData.nodes():
            #Place nodes
            x, y = self.graphData.node[n]['pos']
            func = self.graphData.node[n]['funcstr']
            clashCode = self.graphData.node[n]['clashcode']

            self.graph.addNode(x, y, n, func, clashCode)
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
                pRates = self.graphData[src][dst]['prates']
                cRates = self.graphData[src][dst]['crates']
                resnr = self.graphData[src][dst]['res']
                argnr = self.graphData[src][dst]['arg']
                self.tokensInScene.append((src, dst))    
                    
                self.graph.addEdgeToNodes(node1, node2, 'right', 'left', src, dst, tokenValues, pRates, cRates, resnr, argnr)
                
        #Then place the rest of the edges (not self-looping)
        for src, dst in self.graphData.edges():
            #Place edges and tokens
            node1 = nodeList.index(src)
            node2 = nodeList.index(dst)
            
            if src != dst:
                tokenValues = self.graphData[src][dst]['tkns']
                pRates = self.graphData[src][dst]['prates']
                cRates = self.graphData[src][dst]['crates']
                resnr = self.graphData[src][dst]['res']
                argnr = self.graphData[src][dst]['arg']
                self.tokensInScene.append((src, dst))    

                #If begin node is left of end node
                if nodePoints[node1][0] < nodePoints[node2][0]:
                    self.graph.addEdgeToNodes(node1, node2, 'right', 'left', src, dst, tokenValues, pRates, cRates, resnr, argnr)
                elif nodePoints[node1][0] > nodePoints[node2][0]:
                    self.graph.addEdgeToNodes(node1, node2, 'left', 'right', src, dst, tokenValues, pRates, cRates, resnr, argnr)
                else:
                    if nodePoints[node1][0] > self.centerOfGraph.x():
                        self.graph.addEdgeToNodes(node1, node2, 'right', 'right', src, dst, tokenValues, pRates, cRates, resnr, argnr)
                    else:
                        self.graph.addEdgeToNodes(node1, node2, 'left', 'left', src, dst, tokenValues, pRates, cRates, resnr, argnr)

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
        print('Update function to: ' + str(newFunction))
        self.graphData.node[nodeName]['funcstr'] = newFunction
        self.graphData.updateNodeFunction(nodeName, newFunction)

    def editNodePosition(self, nodeName, newPos):
        print('Node', nodeName, 'moved to:', newPos)
        self.graphData.node[nodeName]['pos'] = newPos.x(), newPos.y()

    def editClashCode(self, nodeName, newClashCode):
        print('Update CLaSH code to: ' + str(newClashCode))
        self.graphData.node[nodeName]['clashcode'] = newClashCode
        self.graphData.updateClashCode(nodeName, newClashCode)
    
    def editPRates(self, src, dst, newPRates):
        print('Update pRates to: ' + str(newPRates))
        self.graphData[src][dst]['prates'] = newPRates
        newPRates = str(newPRates)
        self.graphData.updatePRates((src, dst), newPRates)

    def editCRates(self, src, dst, newCRates):
        print('Update cRates to: ' + str(newCRates))
        self.graphData[src][dst]['crates'] = newCRates
        newCRates = str(newCRates)
        self.graphData.updateCRates((src, dst), newCRates)

    def getFireCount(self, src_dst, node):
        return self.graphData.node[src_dst]['firecount']

    def resetView(self):
        self.zoomSlider.setValue(250)
        self.setupMatrix()
        self.graphicsView.ensureVisible(QRectF(0,0,0,0))

    def setupMatrix(self):
        scale = 2.0 ** ((self.zoomSlider.value() - 250) / 50.0)

        transform = QTransform()
        transform.scale(scale, scale)

        self.graphicsView.setTransform(transform)

    def zoomIn(self):
        self.zoomSlider.setValue(self.zoomSlider.value() + 10)

    def zoomOut(self):
        self.zoomSlider.setValue(self.zoomSlider.value() - 10)
