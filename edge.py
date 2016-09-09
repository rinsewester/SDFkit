#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

from PyQt5.QtWidgets import QGraphicsItem, QInputDialog
from PyQt5.QtCore import Qt, QRectF, QPointF
from PyQt5.QtGui import QColor, QPen, QBrush, QPainterPath, QFont

class Edge(QGraphicsItem):

    def __init__(self, beginPoint, endPoint, beginSide, endSide, edgeSelfLoops, pRates, cRates, color):
        super().__init__()

        self.edgeSelfLoops = edgeSelfLoops
        self.penWidth = 4
        self.beginSide = beginSide
        self.endSide = endSide
        self.beginPoint = beginPoint
        self.endPoint = endPoint
        self.calculateCurvePoints(beginPoint, endPoint)
        self.cRates = cRates
        self.pRates = pRates
        self.updatePCRects()
        
        self.calculateEdgeColors(color)

        #self.setFlags(QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemIsMovable)
        self.setAcceptHoverEvents(True)
        self.hover = False
        self.debugOn = False

    def boundingRect(self):
        #Used for collision detection and repaint
        path = self.getEdgePath()
        path.setFillRule(Qt.WindingFill)
        path.addRect(self.cRect)
        path.addRect(self.pRect)
        path.addPath(self.getLargerEdgePath())

        return path.boundingRect()
    
    def shape(self):
        #Determines the collision area
        path = self.getEdgePath()
        path.setFillRule(Qt.WindingFill)
        
        path.addRect(self.cRect)
        path.addRect(self.pRect)

        path.addPath(self.getLargerEdgePath())

        return path

    def paint(self, painter, option, widget):
        lod = option.levelOfDetailFromTransform(painter.worldTransform())

        if lod > 0.05:
            self.paintEdge(painter, lod)

        if lod > 0.15 and self.debugOn:
            self.debug(painter) #Uncomment to turn on debug mode

    def paintEdge(self, painter, lod):
        pen = QPen(Qt.black)
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

        painter.drawPath(edgePath)

        if lod > 0.4:
            self.drawPCRates(painter)

    def drawPCRates(self, painter):
        #Draw production and consumption rates above begin and end of edge
        pen = QPen(QColor(0, 0, 100))
        painter.setPen(pen)
        painter.setFont(QFont("Arial", 8))

        #Only display rates if larger than 1 or more than 1, and max 1 rate at the time
        #Build string for production rate
        pPhase = self.tokenCluster.getFireCount('src') % len(self.pRates)

        if len(self.pRates) == 1:
            if self.pRates == [1]:
                pRateStr = ''
            else:
                pRateStr = str(self.pRates[0])
        elif pPhase == 0:
            pRateStr = str(self.pRates[pPhase]) + ',..'
        elif pPhase == len(self.pRates) - 1:
            pRateStr = '..,' + str(self.pRates[pPhase])
        else:
            pRateStr = '..,' + str(self.pRates[pPhase]) + ',..'

        #Same for consumption string
        cPhase = self.tokenCluster.getFireCount('dst') % len(self.cRates)

        if len(self.cRates) == 1:
            if self.cRates == [1]:
                cRateStr = ''
            else:
                cRateStr = str(self.cRates[0])
        elif cPhase == 0:
            cRateStr = str(self.cRates[cPhase]) + ',..'
        elif cPhase == len(self.cRates) - 1:
            cRateStr = '..,' + str(self.cRates[cPhase])
        else:
            cRateStr = '..,' + str(self.cRates[cPhase]) + ',..'

        if len(self.pRates) > 1 or self.pRates[0] > 1:   
            painter.drawText(self.pRect, Qt.AlignCenter, pRateStr)
        if len(self.cRates) > 1 or self.cRates[0] > 1:        
            painter.drawText(self.cRect, Qt.AlignCenter, cRateStr)

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

        if self.beginSide == 'right':
            path.lineTo(QPointF(self.beginPoint.x() - 10, self.beginPoint.y() - 2))
            path.lineTo(QPointF(self.beginPoint.x() - 10, self.beginPoint.y() + 2))            
        else:
            path.lineTo(QPointF(self.beginPoint.x() + 10, self.beginPoint.y() - 2))
            path.lineTo(QPointF(self.beginPoint.x() + 10, self.beginPoint.y() + 2))
        path.lineTo(QPointF(self.beginPoint.x(), self.beginPoint.y() + 2))

        return path

    def getLargerEdgePath(self):
        #Used to fill in the small areas on the edge
        #This makes it easier to select the edge
        yTranslation = 2

        #Curve 1
        beginPoint = QPointF(self.beginPoint.x(), self.beginPoint.y() + yTranslation)
        curvePoint1 = QPointF(self.curvePoint1.x()+4, self.curvePoint1.y() + yTranslation)
        curvePoint2 = QPointF(self.curvePoint2.x()+4, self.curvePoint2.y() + yTranslation)
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

        #If beginPoint and endPoint are the same, move the endPoint by 0.01
        if self.beginPoint == self.endPoint:
            if self.beginSide == 'left':
                self.endPoint = QPointF(self.endPoint.x() + 0.01, self.endPoint.y())       
            else:
                self.endPoint = QPointF(self.endPoint.x() - 0.01, self.endPoint.y())
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
        self.setCursor(Qt.PointingHandCursor)

        super().hoverEnterEvent(event)
        self.update()

    def hoverLeaveEvent(self, event):
        self.hover = False
        self.setCursor(Qt.ArrowCursor)

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

        #Update P & C rate rects
        self.updatePCRects()
       
        #Prepare the painter for a geometry change, so it repaints correctly
        self.prepareGeometryChange()
        self.update()

    def setZValueEdge(self, zValue):
        self.setZValue(zValue)
        self.tokenCluster.setZValueTokenCluster(zValue + 1)

    def getPointOnEdge(self, t):
        edgePath = QPainterPath(self.beginPoint)
        edgePath.cubicTo(self.curvePoint1, self.curvePoint2, self.endPoint)

        return QPointF(edgePath.pointAtPercent(t))

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

    def updatePCRects(self):
        if self.beginSide == 'left':
            self.pRect = QRectF(self.beginPoint.x() - 20, self.beginPoint.y() - 15, 20, 10)
        else:
            self.pRect = QRectF(self.beginPoint.x(), self.beginPoint.y() - 15, 20, 10)
        
        if self.endSide == 'left':
            self.cRect = QRectF(self.endPoint.x() - 20, self.endPoint.y() - 15, 20, 10)
        else:
            self.cRect = QRectF(self.endPoint.x(), self.endPoint.y() - 15, 20, 10)

    def setPRatesActiontriggered(self):
        pRatesStr = str(self.pRates)
        newPRatesStr, ok = QInputDialog.getText(self.tokenCluster.widget, 'Edit production rates', 'Production rate:', text = pRatesStr)
        
        if ok:
            try:
                newPRates = eval(newPRatesStr)
                self.pRates = newPRates
                self.tokenCluster.widget.editPRates(self.tokenCluster.src, self.tokenCluster.dst, newPRates)
            except:
                print('Invalid pRate entry')

            # newPRates = eval(newPRatesStr)
            # self.pRates = newPRates
            # self.tokenCluster.widget.editPRates(self.tokenCluster.src, self.tokenCluster.dst, newPRates)

    def setCRatesActiontriggered(self):
        cRatesStr = str(self.cRates)
        newCRatesStr, ok = QInputDialog.getText(self.tokenCluster.widget, 'Edit consumption rates', 'Consumption rate:', text = cRatesStr)
        
        if ok:
            try:
                newCRates = eval(newCRatesStr)
                self.cRates = newCRates
                self.tokenCluster.widget.editCRates(self.tokenCluster.src, self.tokenCluster.dst, newCRates)
            except:
                print('Invalid cRate entry')

            # newCRates = eval(newCRatesStr)
            # self.cRates = newCRates
            # self.tokenCluster.widget.editCRates(self.tokenCluster.src, self.tokenCluster.dst, newCRates)

    def calculateEdgeColors(self, color):
        #Calculate all edge colors based on a given color
        r = color.red()
        g = color.green()
        b = color.blue()

        if r < 80:
            r = 80
        if g < 80:
            g = 80
        if b < 80:
            b = 80

        self.edgeColor = QColor(r, g, b)
        self.edgeColorSelected = Qt.black
        self.edgeColorHover = QColor(r - 80, g - 80, b - 80)