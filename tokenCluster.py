#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QGraphicsItem, QMenu, QAction, QInputDialog
from PyQt5.QtCore import Qt, QPoint, QPointF, QRectF
from PyQt5.QtGui import QColor, QPainter, QPen, QBrush, QPainterPath, QFont, QContextMenuEvent, QCursor

class TokenCluster():
    def __init__(self, widget, scene, view, edge, tokenValues = [1, 2, 3, 40, 500, 'token', 789012]):
        super().__init__()

        self.edge = edge
        self.scene = scene
        self.view = view
        self.widget = widget
        self.addReferenceToEdge()
        self.tokenValues = tokenValues
        self.tokensAreClusterd = False

        self.tokenList = []

        self.newTokenValues(self.tokenValues) 
        
        #Update all tokens once
        self.updateTokens()      

        
        self.setTokenAction = QAction('Edit tokens', self.widget)
        self.setTokenAction.triggered.connect(self.setTokenActiontriggered)

        self.tokenMenu = QMenu()
        self.tokenMenu.addAction(self.setTokenAction)
        
        


    def addToken(self, value):
        listLength = len(self.tokenList)
        token = Token(value, self.edge, listLength, self)

        if listLength >= 1:
            #Update the row length for all tokens
            for i in range(listLength):
                self.tokenList[i].updateRowLength(listLength)

        #Add token to scene and list
        token.setZValue(2)
        self.tokenList.append(token)
        self.scene.addItem(token)


    def addReferenceToEdge(self):
        self.edge.setTokenCluster(self)
    

#------------------
#---Changing tokens---
    def updateTokens(self):
        for i in range(len(self.tokenList)):
            self.tokenList[i].updatePos()


    def newTokenValues(self, newTokens):
    	#Replaces all tokens with new tokens
        self.deleteTokens()
        self.tokenList.clear()

        for i in range(len(newTokens)):
            self.addToken(newTokens[i])
        
        self.tokenValues = newTokens
        self.updateTokens()  


    def deleteTokens(self):
        for i in range(len(self.tokenList)):
            self.scene.removeItem(self.tokenList[i])
            #Might also need to remove the QGraphicsItem itself, but 'delete' doesn't work


    def setZValueTokenCluster(self, zValue):
        for i in range(len(self.tokenList)):
            self.tokenList[i].setZValueToken(zValue)


    def contextMenu(self, pos):
    	#Get point from scene
        point = self.view.mapFromScene(pos)

        #Convert point to global point
        point = self.view.mapToGlobal(point)

        #Execute context menu
        self.tokenMenu.exec(point)


    def setTokenActiontriggered(self):
        tokenStr = str(self.tokenValues)
        newTokenStr, ok = QInputDialog.getText(self.widget, 'Edit tokens', 'Tokens:', text = tokenStr)
        
        if ok:
            try:
                newTokens = eval(newTokenStr)
                print('Updated tokens to: ' + str(newTokenStr))
                self.newTokenValues(newTokens)
            except:
                print('Invalid token entry')

 

class Token(QGraphicsItem):

    def __init__(self, value, edge, numberInRow, cluster):
        super().__init__()

        self.value = value
        self.edge = edge
        self.numberInRow = numberInRow
        self.rowLength = numberInRow
        self.cluster = cluster
        self.t = 0.5
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

            #Determine font size based on size of token content
            if valueSize == 1:
                rectValue = QRectF(rectValue.x(), rectValue.x() + 1, rectValue.width(), rectValue.height())
                painter.setFont(QFont("Lucida Console", 9))
            elif valueSize == 2:
                painter.setFont(QFont("Lucida Console", 7))
            elif valueSize == 3:
                painter.setFont(QFont("Lucida Console", 5))
            elif valueSize > 3:
                painter.setFont(QFont("Lucida Console", 4))
               
            
            if lod > 0.4:
                painter.drawText(rectValue, Qt.AlignCenter, str(self.value))
                
                #Add dots to indicate that not the entire contents of the token is displayed
                if valueSize > 5:
                    rect = rectValue                    
                    rect.setY(rect.y() + 5)
                    painter.drawText(rect, Qt.AlignCenter, '..')


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
        self.cluster.tokensAreClusterd = False

        if self.rowLength == 0:
            self.setPos(self.getPointOnEdge(0.5))
        
        elif self.rowLength == 1:
            if self.numberInRow == 0:
                self.setPos(self.getPointCloseToCenter(8))
            else:
                self.setPos(self.getPointCloseToCenter(-8))
        
        elif self.rowLength == 2:
            if self.numberInRow == 0:
                self.setPos(self.getPointCloseToCenter(15))
            elif self.numberInRow == 1:
                self.setPos(self.getPointOnEdge(0.5))
            else:
                self.setPos(self.getPointCloseToCenter(-15))

        else:
            if self.numberInRow == 0:
                self.setPos(self.getPointCloseToCenter(20))
            elif self.numberInRow == self.rowLength:
                self.setPos(self.getPointCloseToCenter(-20))
            else:
                self.cluster.tokensAreClusterd = True
                self.setPos(self.getPointOnEdge(0.5))

    
    def setZValueToken(self, zValue):
        self.setZValue(zValue)


    def updateRowLength(self, length):
        self.rowLength = length


    def contextMenuEvent(self, event):
    	#Gets point of right click and converts it to a position on the scene
        self.cluster.contextMenu(event.scenePos())

