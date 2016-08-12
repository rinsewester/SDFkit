#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsItem
from PyQt5.QtCore import QPoint, QRectF, QEvent, QPointF
from PyQt5.QtGui import QColor, QPainter, QPen, QBrush, QPainterPath

class Edge(QGraphicsItem):

    def __init__(self, scene, beginPoint, endPoint):
        super().__init__()

        self.scene = scene
        self.beginPoint = beginPoint
        self.endPoint = endPoint
        self.midPoint = self.calculateMidPoint(beginPoint, endPoint)
        # print('beginPoint: ' + str(self.beginPoint))
        # print('midPoint: ' + str(self.midPoint))
        # print('endPoint: ' + str(self.endPoint))

        self.edgeColor = QColor(160, 160, 160)
        self.edgeColorSelected = QColor(80, 80, 80)
        self.edgeColorHover = QColor(120, 120, 120)

        #self.setFlags(QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemIsMovable)
        self.setAcceptHoverEvents(True)
        self.hover = False


    def boundingRect(self):
        #Used for collision detection
        rect = QRectF(self.beginPoint, self.endPoint)
        rect = rect.normalized()

        return rect

    
    def shape(self):
        #Determines the paint area
        path = QPainterPath()

        rect = QRectF(self.beginPoint, self.endPoint)
        rect = rect.normalized()

        path.addRect(rect)

        return path


    def paint(self, painter, option, widget):
    	lod = option.levelOfDetailFromTransform(painter.worldTransform())

    	if lod > 0.15:
            pen = QPen(self.edgeColor)
            pen.setWidth(3)

            if self.hover:
        	    pen.setColor(self.edgeColorHover)

            if QGraphicsItem.isSelected(self):
        	    pen.setColor(self.edgeColorSelected)

            painter.setPen(pen)

            #Paint 2 lines that go to a midPoint
            # painter.drawLine(self.beginPoint, self.midPoint)
            # painter.drawLine(self.midPoint, self.endPoint)

            #Paint a line between the beginPoint and endPoint
            painter.drawLine(self.beginPoint, self.endPoint)

            #self.debug(painter)


    def debug(self, painter):
        #Actual paint area
        painter.setBrush(QBrush(QColor(0, 0, 0, 0)))
        pen = QPen(QColor(255, 0, 0, 100))
        pen.setWidth(0.5)
        painter.setPen(pen)

        path = QPainterPath()

        path.addPath(self.shape())

        painter.drawPath(path)


    def calculateMidPoint(self, beginPoint, endPoint):
        x = (beginPoint.x() + endPoint.x()) / 2
        y = (beginPoint.y() + endPoint.y()) / 2
        return QPoint(x, y)


    def hoverEnterEvent(self, event):
        self.hover = True

        super().hoverEnterEvent(event)
        self.update()


    def hoverLeaveEvent(self, event):
        self.hover = False

        super().hoverLeaveEvent(event)
        self.update()


    def mousePressEvent(self, event):
        super().mousePressEvent(event)
        self.update()


    def moveEdge(self, delta, edgeSide):
        if edgeSide == 'begin':
            self.beginPoint += delta
        else:
            self.endPoint += delta

        #Prepare the painter for a geometry change, so it repaints correctly
        self.prepareGeometryChange()
        self.update()
