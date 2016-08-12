#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsItem
from PyQt5.QtCore import Qt, QPoint, QRectF, QEvent, QPointF
from PyQt5.QtGui import QColor, QPainter, QPen, QBrush, QPainterPath, QPainterPathStroker

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
        #Used for collision detection and repaint
        rect = QRectF(self.beginPoint, self.endPoint)
        rect = rect.normalized()

        #Make rect slightly larger in order to include the linecap
        rect.setX(rect.x() - 1)
        rect.setY(rect.y() - 1)
        rect.setWidth(rect.width() + 2)
        rect.setHeight(rect.height() + 2)
        return rect

    
    def shape(self):
        #Determines the paint path

        path = QPainterPath(self.beginPoint)
        path.cubicTo(self.curvePoint1, self.curvePoint2, self.endPoint)

        

        return path


    def paint(self, painter, option, widget):
    	lod = option.levelOfDetailFromTransform(painter.worldTransform())

    	if lod > 0.15:
            pen = QPen(self.edgeColor)
            pen.setWidth(3)
            pen.setCapStyle(Qt.RoundCap)

            if self.hover:
        	    pen.setColor(self.edgeColorHover)

            if QGraphicsItem.isSelected(self):
        	    pen.setColor(self.edgeColorSelected)

            painter.setPen(pen)
            painter.setBrush(QBrush(QColor(0, 0, 0, 0)))

            curvePath = self.shape()
            painter.drawPath(curvePath)

            #self.debug(painter)


    def debug(self, painter):
        #Paint path
        painter.setBrush(QBrush(QColor(0, 0, 0, 25)))
        pen = QPen(QColor(255, 0, 0, 100))
        pen.setWidth(0.5)
        painter.setPen(pen)

        path = QPainterPath()

        path.addPath(self.shape())

        painter.drawPath(path)

        #Curve points
        painter.drawEllipse(self.curvePoint1, 5, 5)
        painter.drawEllipse(self.curvePoint2, 5, 5)


    def calculateMidPoint(self, beginPoint, endPoint):
        x = (beginPoint.x() + endPoint.x()) / 2
        y = (beginPoint.y() + endPoint.y()) / 2

        self.curvePoint1 = QPointF(self.endPoint.x(), self.beginPoint.y())
        self.curvePoint2 = QPointF(self.beginPoint.x(), self.endPoint.y())

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

        #Update midPoint
        self.midPoint = self.calculateMidPoint(self.beginPoint, self.endPoint)

        #Prepare the painter for a geometry change, so it repaints correctly
        self.prepareGeometryChange()
        self.update()
