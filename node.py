#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsItem
from PyQt5.QtCore import QRectF
from PyQt5.QtGui import QColor, QPainter, QBrush, QPainterPath

class Node(QGraphicsItem):

    def __init__(self, color, x, y):
        super().__init__()
        self.color = color
        self.x = x
        self.y = y

        #self.setFlags(QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemIsMovable)
        print(self.flags())
        self.setFlag(QGraphicsItem.ItemIsMovable)
        #self.setAcceptHoverEvents(True)
        print('node created')


    def boundingRect(self):
        return QRectF(0, 0, 100, 60)

    
    def shape(self):
    	path = QPainterPath()
    	path.addRect(0, 0, 100, 60)
    	return path

    
    def paint(self, painter, option, widget):
        #painter = QPainter(self)
        
        color = QColor(0, 0, 0)
        painter.setPen(color)
      
        brush = QBrush(QColor(255, 0, 0))
        painter.setBrush(brush)
        painter.drawRoundedRect(0, 0, 100, 60, 10, 10)


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


