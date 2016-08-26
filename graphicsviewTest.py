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

    def wheelEvent(self, event):
        if event.modifiers() & Qt.ControlModifier:
            if event.angleDelta().y() > 0:
                GraphWidget.zoomIn(6)
            else:
                GraphWidget.zoomOut(6)
        
            event.accept()
        else:
            super().wheelEvent(event)



class GraphWidget(QWidget):

    def __init__(self):
        super().__init__()

        self.initUI()

        
    def initUI(self):

        #setFrameStyle(Sunken | StyledPanel)
        self.graphicsView = QGraphicsView()
        self.graphicsView.setDragMode(QGraphicsView.RubberBandDrag)
        self.graphicsView.setOptimizationFlags(QGraphicsView.DontSavePainterState)
        self.graphicsView.setViewportUpdateMode(QGraphicsView.SmartViewportUpdate)
        self.graphicsView.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)
        self.graphicsView.setRenderHint(QPainter.Antialiasing, True)

        #Make a graphics scene
        scene = QGraphicsScene()
        self.graphicsView.setScene(scene)

        #Create a graph that can contain nodes and edges
        graph = Graph(scene, self.graphicsView)

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
        self.zoomSlider.setRange(0, 500)
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


    def zoomIn(self, level):
        if not level:
            levelValue = 1
        else:
            levelValue = level

        self.zoomSlider.setValue(self.zoomSlider.value() + levelValue)


    def zoomOut(self, level):
        if not level:
            levelValue = 1
        else:
            levelValue = level

        self.zoomSlider.setValue(self.zoomSlider.value() - levelValue)


    def wheelEvent(self, event):
        if event.modifiers() & Qt.ControlModifier:
            if event.angleDelta().y() > 0:
                self.zoomIn(6)
            else:
                self.zoomOut(6)
        
            event.accept()
        else:
            super().wheelEvent(event)


    def addNode(self, scene, x, y, name):
        newNode = Node(name)
        newNode.setPos(x, y)
        newNodeInScene = scene.addItem(newNode)

        self.nodeList.append(newNodeInScene)


    def addEdge(self, scene, x1, y1, x2, y2):
        startPoint = QPoint(x1, y1)
        endPoint = QPoint(x2, y2)
        newEdge = Edge(scene, startPoint, endPoint)
        newEdgeInScene = scene.addItem(newEdge)

        self.edgeList.append(newEdgeInScene)


