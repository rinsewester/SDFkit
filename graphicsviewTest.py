#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QDockWidget, QGraphicsView, QApplication, QSplitter, QSlider, QHBoxLayout, QVBoxLayout, QGridLayout, QFrame, QLabel, QToolButton, QButtonGroup, QGraphicsScene
from PyQt5.QtCore import Qt, QSize, QObject, pyqtSignal, QRectF
from PyQt5.QtGui import QIcon, QTransform

class GraphicsView(QGraphicsView):

        def WheelEvent(self, event):
                if event.modifiers() & Qt.ControlModifier:
                	if event.delta() > 0:
                		view.zoomIn(6)
                	else:
                		view.zoomOut(6)

                	event.accept()
                else:
                	QGraphicsView(self, event)


class GraphWidget(QWidget):

    def __init__(self):
        super().__init__()

        self.initUI()

        
    def initUI(self):

        #setFrameStyle(Sunken | StyledPanel)
        self.graphicsView = GraphicsView()
        self.graphicsView.setDragMode(QGraphicsView.RubberBandDrag)
        self.graphicsView.setOptimizationFlags(QGraphicsView.DontSavePainterState)
        self.graphicsView.setViewportUpdateMode(QGraphicsView.SmartViewportUpdate)
        self.graphicsView.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)

        size = 16
        iconSize = QSize(size, size)

        self.zoomInButton = QToolButton(self)
        self.zoomInButton.setAutoRepeat(True)
        #self.zoomInButton.setAutorepeatInterval(33)
        self.zoomInButton.setAutoRepeatDelay(0);
        self.zoomInButton.setIcon(QIcon('images/zoomin.png'));
        self.zoomInButton.setIconSize(iconSize);
        self.zoomOutButton = QToolButton(self)
        self.zoomOutButton.setAutoRepeat(True);
        #self.zoomOutButton.setAutoRepeatInterval(33);
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

        #Label layout
        labelLayout = QHBoxLayout()
        # label = QLabel('Designer')
        # label2 = QLabel('Pointer Mode')
        # selectModeButton = QToolButton(self)
        # selectModeButton.setText('Select')
        # selectModeButton.setCheckable(True)
        # selectModeButton.setChecked(True)
        # dragModeButton = QToolButton(self)
        # dragModeButton.setText('Drag')
        # dragModeButton.setCheckable(True)
        # dragModeButton.setChecked(False)

        # buttonGroup = QButtonGroup()
        # buttonGroup.setExclusive(True)
        # buttonGroup.addButton(selectModeButton)
        # buttonGroup.addButton(dragModeButton)

        # labelLayout.addWidget(label)
        # labelLayout.addStretch(1)
        # labelLayout.addWidget(label2)
        # labelLayout.addWidget(selectModeButton)
        # labelLayout.addWidget(dragModeButton)
        # labelLayout.addStretch(1)

        #Final layout
        topLayout = QGridLayout()
        topLayout.addLayout(labelLayout, 0, 0)
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
        scale = (2.0 ** (self.zoomSlider.value() - 250) / 50.0)
        scale = scale * scale

        transform = QTransform()
        transform.scale(scale, scale)

        self.graphicsView.setTransform(transform)
        self.setResetButtonEnabled()


    def zoomIn(self, level):
        level = 1
        self.zoomSlider.setValue(self.zoomSlider.value() + level)


    def zoomOut(self, level):
        level = 1
        self.zoomSlider.setValue(self.zoomSlider.value() - level)