#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsItem
from PyQt5.QtCore import QRectF, QPointF
from PyQt5.QtGui import QColor, QPainter, QBrush, QPainterPath

class Node(QGraphicsItem):

    def __init__(self, color, x, y):
        super().__init__()

        self.inputHeigthDifference = 10

        self.color = color
        self.x = x
        self.y = y

        self.inputList = [self.getInputPoint(0), False]
        self.inputList[1] = [self.getInputPoint(len(self.inputList) -1), False]

        self.setFlags(QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemIsMovable)
        print('node created')


    def boundingRect(self):
        return QRectF(0, 0, 100, 60)

    
    def shape(self):
    	path = QPainterPath()
    	path.addRect(0, 0, 100, 60)
    	return path

    
    def paint(self, painter, option, widget):
        #painter = QPainter(self)
        
        self.paintNodeBody(painter)
        self.paintNodeInputs(painter)


    def paintNodeBody(self, painter):
        color = QColor(0, 0, 0)
        painter.setPen(color)
      
        brush = QBrush(QColor(255, 0, 0))
        painter.setBrush(brush)
        painter.drawRoundedRect(0, 0, 100, 60, 10, 10)


    def paintNodeInputs(self, painter):
        color = QColor(0, 0, 0)
        painter.setPen(color)
      
        brush = QBrush(QColor(255, 100, 100))
        painter.setBrush(brush)
        painter.drawRoundedRect(0, 10, 15, 10, 2, 2)


    def mousePressEvent(self, event):
    	super().mousePressEvent(event)
    	self.update()


    def mouseMoveEvent(self, event):
    	#Code for moving nodes goes here
    	self.update()
    	super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event):
        super().mouseReleaseEvent(event)
        self.update()


    def getInputPoint(self, inputIndex):
    	inputPoint = QPointF(0, inputIndex * self.inputHeigthDifference)
    	print(inputPoint.y())

    	return inputPoint


