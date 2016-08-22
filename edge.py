#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsItem
from PyQt5.QtCore import Qt, QPoint, QRectF, QEvent, QPointF
from PyQt5.QtGui import QColor, QPainter, QPen, QBrush, QPainterPath

class Edge(QGraphicsItem):

    def __init__(self, beginPoint, endPoint, beginSide, endSide, edgeSelfLoops):
        super().__init__()

        self.edgeSelfLoops = edgeSelfLoops
        self.penWidth = 4
        self.beginSide = beginSide
        self.endSide = endSide
        self.beginPoint = beginPoint
        self.endPoint = endPoint
        self.calculateCurvePoints(beginPoint, endPoint)
        

        self.edgeColor = QColor(160, 160, 160)
        self.edgeColorSelected = QColor(0, 0, 0)
        self.edgeColorHover = QColor(50, 50, 50)

        #self.setFlags(QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemIsMovable)
        self.setAcceptHoverEvents(True)
        self.hover = False
        self.debugOn = True


    def boundingRect(self):
        #Used for collision detection and repaint
        path = self.getEdgePath()
        rect = path.boundingRect()

        return rect

    
    def shape(self):
        #Determines the collision area
        # path = QPainterPath(self.beginPoint)
        # path.cubicTo(self.curvePoint1, self.curvePoint2, self.endPoint) 

        path = self.getEdgePath()

        return path


    def paint(self, painter, option, widget):
        lod = option.levelOfDetailFromTransform(painter.worldTransform())

        if lod > 0.05:
            self.paintEdge(painter)

        if lod > 0.15 and self.debugOn:
            self.debug(painter) #Uncomment to turn on debug mode


    def paintEdge(self, painter):
        pen = QPen(QColor(0, 0, 0))
        pen.setWidth(1)
        pen.setCapStyle(Qt.RoundCap)
        brush = QBrush(self.edgeColor)

        if self.hover:
            #pen.setColor(self.edgeColorHover)
            brush.setColor(self.edgeColorHover)

        if QGraphicsItem.isSelected(self):
            #pen.setColor(self.edgeColorSelected)
            brush.setColor(self.edgeColorSelected)

        painter.setPen(pen)
        painter.setBrush(brush)

        edgePath = self.getEdgePath()       
        edgePath = edgePath.simplified()



        # edgePath = QPainterPath(self.beginPoint)
        # edgePath.cubicTo(self.curvePoint1, self.curvePoint2, self.endPoint)


        painter.drawPath(edgePath)


    def getEdgePath(self):
        yTranslation = 2

        #Curve 1
        beginPoint = QPointF(self.beginPoint.x(), self.beginPoint.y() + yTranslation)
        curvePoint1 = QPointF(self.curvePoint1.x(), self.curvePoint1.y() + yTranslation)
        curvePoint2 = QPointF(self.curvePoint2.x(), self.curvePoint2.y() + yTranslation)
        endPoint = QPointF(self.endPoint.x(), self.endPoint.y() + yTranslation)
        path = QPainterPath(beginPoint)
        point1 = QPointF(curvePoint1.x(), curvePoint1.y())
        point2 = QPointF(curvePoint2.x(), curvePoint2.y())
        path.cubicTo(point1, point2, endPoint)

        #Arrow
        arrowBeginPoint = QPointF(self.endPoint.x(), self.endPoint.y() + 4)
        path.lineTo(arrowBeginPoint)
        if self.endSide == 'right':
            path.lineTo(QPointF(self.endPoint.x() - 10, self.endPoint.y()))
        else:
            path.lineTo(QPointF(self.endPoint.x() + 10, self.endPoint.y()))
        path.lineTo(QPointF(self.endPoint.x(), self.endPoint.y() - 4))
        path.lineTo(QPointF(self.endPoint.x(), self.endPoint.y() - 2))

        #Curve 2 (back)
        endPoint = QPointF(self.beginPoint.x(), self.beginPoint.y() - yTranslation)
        curvePoint2 = QPointF(self.curvePoint1.x(), self.curvePoint1.y() - yTranslation)
        curvePoint1 = QPointF(self.curvePoint2.x(), self.curvePoint2.y() - yTranslation)
        beginPoint = QPointF(self.endPoint.x(), self.endPoint.y() - yTranslation)
        point1 = QPointF(curvePoint1.x(), curvePoint1.y())
        point2 = QPointF(curvePoint2.x(), curvePoint2.y())
        path.cubicTo(point1, point2, endPoint) 

        #Cap
        # if self.beginSide == 'right':
        #     path.lineTo(QPointF(self.beginPoint.x() - 5, self.beginPoint.y() - 4))
        #     path.lineTo(QPointF(self.beginPoint.x() - 5, self.beginPoint.y() + 4))            
        # else:
        #     path.lineTo(QPointF(self.beginPoint.x() + 5, self.beginPoint.y() - 4))
        #     path.lineTo(QPointF(self.beginPoint.x() + 5, self.beginPoint.y() + 4))
        # path.lineTo(QPointF(self.beginPoint.x(), self.beginPoint.y() + 2))

        if self.beginSide == 'right':
            path.lineTo(QPointF(self.beginPoint.x() - 10, self.beginPoint.y() - 2))
            path.lineTo(QPointF(self.beginPoint.x() - 10, self.beginPoint.y() + 2))            
        else:
            path.lineTo(QPointF(self.beginPoint.x() + 10, self.beginPoint.y() - 2))
            path.lineTo(QPointF(self.beginPoint.x() + 10, self.beginPoint.y() + 2))
        path.lineTo(QPointF(self.beginPoint.x(), self.beginPoint.y() + 2))

        return path


    def debug(self, painter):
        #Paint path
        painter.setBrush(QBrush(QColor(0, 0, 0, 25)))
        pen = QPen(QColor(255, 0, 0, 100))
        pen.setWidth(1)
        painter.setPen(pen)

        #Curve area
        path = QPainterPath()
        path.addPath(self.shape())
        painter.drawPath(path)

        #Curve controll points
        painter.drawEllipse(self.curvePoint1, 2, 2)
        painter.drawEllipse(self.curvePoint2, 2, 2)

        #Draw area
        painter.setPen(QPen(QColor(0, 255, 0, 100)))
        painter.setBrush(QBrush(QColor(0, 0, 0, 15)))
        path2 = QPainterPath()
        rect = self.boundingRect()
        path2.addRect(rect)
        painter.drawPath(path2)

        #Middel point
        painter.setPen(QPen(QColor(0, 0, 255, 100)))
        painter.drawEllipse(self.midPoint, 2, 2)
   


    def calculateCurvePoints(self, beginPoint, endPoint):
        x = (beginPoint.x() + endPoint.x()) / 2
        y = (beginPoint.y() + endPoint.y()) / 2

        #Calculate the point in the middle of beginPoint and endPoint
        self.midPoint = QPointF(x, y)
        xPoint1 = self.midPoint.x()
        xPoint2 = self.midPoint.x()

        #If beginPoint and endPoint are the same, move the endPoint by 1
        if self.beginPoint == self.endPoint:
            if self.beginSide == 'left':
                self.endPoint = QPointF(self.endPoint.x() + 1, self.endPoint.y())       
            else:
                self.endPoint = QPointF(self.endPoint.x() - 1, self.endPoint.y())
            self.calculateCurvePoints(self.beginPoint, self.endPoint)

        #Calculate curvePoints based on the position of the nodes
        self.xDiff = abs(self.beginPoint.x() - self.endPoint.x())
        if  self.xDiff < 400:
             self.xDiff = 400
        self.xDiff = self.xDiff / 4

        #Adjust curve to the different combinations of the node locations
        if self.beginSide == 'right':
            if self.endSide == 'left':
                if abs(self.beginPoint.y() - self.endPoint.y()) < 25:
                    #When the nodes are too close
                    xPoint1 = self.beginPoint.x()
                    xPoint2 = self.endPoint.x()
                else:
                    xPoint1 = self.beginPoint.x() +  self.xDiff
                    xPoint2 = self.endPoint.x() -  self.xDiff

            else:
                xPoint1 = self.beginPoint.x() +  self.xDiff
                xPoint2 = self.endPoint.x() +  self.xDiff
        else:
            if self.endSide == 'right':
                if abs(self.beginPoint.y() - self.endPoint.y()) < 25:
                    #When the nodes are too close
                    xPoint1 = self.beginPoint.x()
                    xPoint2 = self.endPoint.x()
                else:
                    xPoint1 = self.beginPoint.x() - self.xDiff
                    xPoint2 = self.endPoint.x() + self.xDiff

            else:
                xPoint1 = self.beginPoint.x() - self.xDiff
                xPoint2 = self.endPoint.x() - self.xDiff     
        
        #Add a y translation to the curve points when the edge loops to the same node
        #or otherwise crosses straight over itself      
        self.yTranslation = 0

        if self.beginSide == self.endSide:
            #The edge always crosses nodes on the same y level when the IO are on opposite sides
            if abs(self.beginPoint.y() - self.endPoint.y()) < 35:
                self.yTranslation = -45

        else:
            #When the IO is on different sides but the nodes are switched around
            if abs(self.beginPoint.y() - self.endPoint.y()) < 55:
                if self.beginSide == 'right':
                    if self.beginPoint.x() > self.endPoint.x():
                        self.yTranslation = -45
                        xPoint1 = self.beginPoint.x() + 100
                        xPoint2 = self.endPoint.x() - 100
                else:
                    if self.beginPoint.x() < self.endPoint.x():
                        self.yTranslation = -45
                        xPoint1 = self.beginPoint.x() - 100
                        xPoint2 = self.endPoint.x() + 100

        #Adjust for selflooping
        if self.edgeSelfLoops:
            if self.beginSide != self.endSide:
                self.yTranslation = -45
                if self.beginSide == 'right':
                    xPoint1 = self.beginPoint.x() + 100
                    xPoint2 = self.endPoint.x() - 100
                else:
                    xPoint1 = self.beginPoint.x() - 100
                    xPoint2 = self.endPoint.x() + 100


        #Add curvePoints
        self.curvePoint1 = QPointF(xPoint1, self.beginPoint.y() + self.yTranslation)
        self.curvePoint2 = QPointF(xPoint2, self.endPoint.y() + self.yTranslation)


    def hoverEnterEvent(self, event):
        self.hover = True

        super().hoverEnterEvent(event)
        self.update()


    def hoverLeaveEvent(self, event):
        self.hover = False

        super().hoverLeaveEvent(event)
        self.update()


    def mousePressEvent(self, event):
        if event.button() == Qt.RightButton:
            self.tokenCluster.contextMenu(event.scenePos())

        super().mousePressEvent(event)
        self.update()


    def moveEdge(self, delta, edgeSide):
        if edgeSide == 'begin':
            self.beginPoint += delta
        else:
            self.endPoint += delta

        #Update curve
        self.calculateCurvePoints(self.beginPoint, self.endPoint)

        #Update tokens on the edge
        self.tokenCluster.updateTokens()
       
        #Prepare the painter for a geometry change, so it repaints correctly
        self.prepareGeometryChange()
        self.update()


    def setZValueEdge(self, zValue):
        self.setZValue(zValue)
        self.tokenCluster.setZValueTokenCluster(zValue + 1)


    def getPointOnEdge(self, t):
        edgePath = QPainterPath(self.beginPoint)
        edgePath.cubicTo(self.curvePoint1, self.curvePoint2, self.endPoint)

        point = QPointF(edgePath.pointAtPercent(t))

        return point


    def getPointCloseToCenter(self, distance):
        edgePath = QPainterPath(self.beginPoint)
        edgePath.cubicTo(self.curvePoint1, self.curvePoint2, self.endPoint)
        
        if edgePath.length() > 0:
            percent = (edgePath.length() / 2 + distance) / edgePath.length()
        else:
            percent = 0            

        #Snap to begin/end point when the edge is too small
        if percent < 0:
            percent = 0
        elif percent > 1:
            percent = 1

        return self.getPointOnEdge(percent)


    def setTokenCluster(self, tokenCluster):
        self.tokenCluster = tokenCluster
        
