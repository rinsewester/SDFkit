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
    def __init__(self, scene, edge):
        super().__init__()

        self.edge = edge
        self.scene = scene
        self.addReferenceToEdge()

        self.tokenList = []

        self.addToken(self.scene, 1)
        self.addToken(self.scene, 2)
        self.addToken(self.scene, 3)
        self.addToken(self.scene, 20)   
        self.addToken(self.scene, 300)   
        
        #Update all tokens once
        self.updateTokens()      
        
        


    def addToken(self, scene, value):
        listLength = len(self.tokenList)
        token = Token(value, self.edge, listLength)

        #Remove last-in-row flag from second last token
        if listLength >= 1:
            self.tokenList[listLength - 1].tokenIsLastInRow = False

        #Add token to scene and list
        token.setZValue(5)
        self.tokenList.append(token)
        scene.addItem(token)


    def addReferenceToEdge(self):
        self.edge.setTokenCluster(self)


    def updateTokens(self):
        for i in range(len(self.tokenList)):
            self.tokenList[i].updatePos()



class Token(QGraphicsItem):

    def __init__(self, value, edge, numberInRow):
        super().__init__()

        self.tokenIsLastInRow = True
        self.value = value
        self.edge = edge
        self.numberInRow = numberInRow
        self.t = 0.5
        #self.updatePos()
        self.tokenWidth = 15
        self.tokenHeight = 15



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
            valueSize = len(str(self.value))
            painter.setPen(QColor(0, 0, 0))
            painter.setBrush(QColor(255, 255, 255))
            

            painter.drawEllipse(self.getTokenRect())
            rectValue = self.getTokenRect()

            if valueSize == 1:
                #rectValue = QRectF(1, 1, self.tokenWidth, self.tokenHeight)
                painter.setFont(QFont("Lucida Console", 10))
            elif valueSize == 2:
                painter.setFont(QFont("Lucida Console", 8))
            elif valueSize == 3:
                painter.setFont(QFont("Lucida Console", 5))
            elif valueSize == 4:
                painter.setFont(QFont("Lucida Console", 4))
               
            
            if lod > 0.45:
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


    def getPointOnEdge(self, t):
        return self.edge.getPointOnEdge(t)


    def getPointCloseToCenter(self, distance):
        return self.edge.getPointCloseToCenter(distance)


    def updatePos(self):          
        #Update postion of the token based on its position in the row
        if self.numberInRow == 0:
        	#First
            self.setPos(self.getPointCloseToCenter(20))
        elif self.tokenIsLastInRow:
        	#Last
            self.setPos(self.getPointCloseToCenter(-20))
        else:
        	#Other
            self.setPos(self.getPointOnEdge(self.t))