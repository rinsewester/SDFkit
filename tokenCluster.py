#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QGraphicsItem
from PyQt5.QtCore import Qt, QPoint, QPointF, QRectF
from PyQt5.QtGui import QColor, QPainter, QPen, QBrush, QPainterPath, QFont

class TokenCluster():
    def __init__(self, scene, edge, clusterPos):
        super().__init__()

        self.edge = edge
        self.scene = scene
        self.clusterPos = clusterPos

        self.tokenList = []

        self.addToken(self.scene, 2666)       


    def addToken(self, scene, value):
        token = Token(2666, self.edge, self.clusterPos)
        token.setZValue(5)
        self.tokenList.append(token)
        scene.addItem(token)



class Token(QGraphicsItem):

    def __init__(self, value, edge, tokenPos):
        super().__init__()

        self.value = value
        self.setPos(tokenPos)
        self.tokenWidth = 15
        self.tokenHeight = 15
        print('Token created')


    def boundingRect(self):
        #Used for collision detection and repaint
        rect = self.getTokenRect()

        return rect

    
    def shape(self):
        #Determines the collision area

        path = QPainterPath()
        path.addEllipse(self.getTokenRect())

        return path


    def paint(self, painter, option, widget):
        lod = option.levelOfDetailFromTransform(painter.worldTransform())

        if lod > 0.15:
            valueSize = 4
            painter.setPen(QColor(0, 0, 0))
            painter.setBrush(QColor(255, 255, 255))
            

            painter.drawEllipse(self.getTokenRect())
            rectValue = self.getTokenRect()

            if valueSize == 1:
                rectValue = QRectF(1, 1, self.tokenWidth, self.tokenHeight)
                painter.setFont(QFont("Lucida Console", 10))
            elif valueSize == 2:
                painter.setFont(QFont("Lucida Console", 8))
            elif valueSize == 3:
                painter.setFont(QFont("Lucida Console", 5))
            elif valueSize == 4:
                painter.setFont(QFont("Lucida Console", 4))
               
            

            painter.drawText(rectValue, Qt.AlignCenter, str(self.value))
            #painter.drawRect(self.getTokenRect())


#------------------
#---Mouse Events---
    def mousePressEvent(self, event):
        print('Token Value: ' + str(self.value))  

        super().mousePressEvent(event)
        self.update()


#------------------
#---Other---
    def getTokenRect(self):
        return QRectF(-self.tokenWidth / 2, -self.tokenHeight / 2, self.tokenWidth, self.tokenHeight)
