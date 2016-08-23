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

class TokenCluster(QGraphicsItem):
    def __init__(self, widget, scene, view, edge, src, dst, tokenValues = []):
        super().__init__()

        self.edge = edge
        self.scene = scene
        self.view = view
        self.widget = widget
        self.src = src
        self.dst = dst
        self.addReferenceToEdge()
        self.tokenValues = tokenValues
        self.tokensAreClusterd = False
        self.setAcceptHoverEvents(True)
        self.hover = False
        self.setAcceptHoverEvents(True)
        self.clusterWidth = 20
        self.clusterHeight = 20
        self.clusterColor = QColor(240, 240, 240)

        self.tokenList = []

        self.newTokenValues(self.tokenValues) 
        
        #Update all tokens once
        if self.edge.zValue() == 1:
            self.setZValue(2)
        else:
            self.setZValue(6)
        self.updateTokens()      
       
        
        self.setTokenAction = QAction('Edit tokens', self.widget)
        self.setTokenAction.triggered.connect(self.setTokenActiontriggered)
        self.setPRatesAction = QAction('Edit production rates', self.widget)
        self.setPRatesAction.triggered.connect(self.edge.setPRatesActiontriggered)
        self.setCRatesAction = QAction('Edit consumption rates', self.widget)
        self.setCRatesAction.triggered.connect(self.edge.setCRatesActiontriggered)

        self.edgeMenu = QMenu()
        self.edgeMenu.addAction(self.setTokenAction)
        self.edgeMenu.addAction(self.setPRatesAction)
        self.edgeMenu.addAction(self.setCRatesAction)
        


    def boundingRect(self):
        #Used for collision detection and repaint
        rect = self.getClusterRect()
        if self.hover:
            rect = self.getClusterRectHover()

        return rect

    
    def shape(self):
        #Determines the collision area
        path = QPainterPath()
        path.addEllipse(self.boundingRect())

        return path


    def paint(self, painter, option, widget):
        self.lod = option.levelOfDetailFromTransform(painter.worldTransform())

        if self.lod > 0.15 and self.tokensAreClusterd:
            painter.setPen(QColor(0, 0, 0))
            painter.setBrush(self.clusterColor)
            
            rect = self.getClusterRect()
            smallRect1 = QRectF(-self.clusterWidth * 0.5, -self.clusterHeight * 0.25, self.clusterWidth * 0.5, self.clusterHeight * 0.5)
            smallRect2 = QRectF(-self.clusterWidth * 0.25, 0, self.clusterWidth * 0.5, self.clusterHeight * 0.5)
            smallRect3 = QRectF(-self.clusterWidth * 0.25, -self.clusterHeight * 0.5, self.clusterWidth * 0.5, self.clusterHeight * 0.5)
            smallRect4 = QRectF(0, -self.clusterHeight * 0.25, self.clusterWidth * 0.5, self.clusterHeight * 0.5)

            if self.hover:
            	#Make token larger when the mouse hovers over it
                rect = self.getClusterRectHover()
                #smallRect = QRectF(-self.clusterWidth * 0.375, -self.clusterHeight * 0.375, self.clusterWidth * 0.75, self.clusterHeight * 0.75)
                smallRect1 = QRectF(-self.clusterWidth * 0.75, -self.clusterHeight * 0.375, self.clusterWidth * 0.75, self.clusterHeight * 0.75)
                smallRect2 = QRectF(-self.clusterWidth * 0.375, 0, self.clusterWidth * 0.875, self.clusterHeight * 0.75)
                smallRect3 = QRectF(-self.clusterWidth * 0.375, -self.clusterHeight * 0.75, self.clusterWidth * 0.75, self.clusterHeight * 0.75)
                smallRect4 = QRectF(0, -self.clusterHeight * 0.375, self.clusterWidth * 0.75, self.clusterHeight * 0.75)

            #Paint cluster
            painter.drawEllipse(rect)
            painter.drawEllipse(smallRect1)
            painter.drawEllipse(smallRect2)
            painter.drawEllipse(smallRect3)
            painter.drawEllipse(smallRect4)


    def getClusterRect(self):
        if len(self.tokenList) > 0:
            rect = QRectF(-self.clusterWidth * 0.5, -self.clusterHeight * 0.5, self.clusterWidth, self.clusterHeight)
        else:
            rect = QRectF(0, 0, 0, 0)
        return rect


    def getClusterRectHover(self):
        return QRectF(-self.clusterWidth * 0.75, -self.clusterHeight * 0.75, self.clusterWidth * 1.5, self.clusterHeight * 1.5)
        


    def addToken(self, value):
        listLength = len(self.tokenList)
        token = Token(value, self.edge, listLength, self)

        if listLength >= 1:
            #Update the row length for all tokens
            for i in range(listLength):
                self.tokenList[i].updateRowLength(listLength)

        #Add token to scene and list
        if self.edge.zValue() == 1:
            token.setZValue(3)
        else:
            token.setZValue(7)
        
        self.tokenList.append(token)
        self.scene.addItem(token)


    def addReferenceToEdge(self):
        self.edge.setTokenCluster(self)
    

#------------------
#---Changing tokens---
    def updateTokens(self):
        for i in range(len(self.tokenList)):
            self.tokenList[i].updatePos()

        self.prepareGeometryChange()
        self.setPos(self.edge.getPointOnEdge(0.5))


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
        self.setZValue(zValue)
        if self.hover:
            self.setZValue(7)

        for i in range(len(self.tokenList)):
            self.tokenList[i].setZValueToken(zValue + 1)


#------------------
#---Mouse Events---
    def mousePressEvent(self, event):
        print('Token Values: ' + str(self.tokenValues))  

        super().mousePressEvent(event)
        self.update()


    def hoverEnterEvent(self, event):
        self.hover = True
        self.setZValue(self.zValue() + 4)

        super().hoverEnterEvent(event)
        self.update()


    def hoverLeaveEvent(self, event):
        self.hover = False

        if self.zValue() == 7:
            self.setZValue(7)
        else:            
            self.setZValue(self.zValue() - 4)

        self.prepareGeometryChange()

        super().hoverLeaveEvent(event)
        self.update()


    def contextMenuEvent(self, event):
        #Gets point of right click and converts it to a position on the scene
        self.contextMenu(event.scenePos())


    def contextMenu(self, pos):
    	#Get point from scene
        point = self.view.mapFromScene(pos)

        #Convert point to global point
        point = self.view.mapToGlobal(point)

        #Execute context menu
        self.edgeMenu.exec(point)


    def setTokenActiontriggered(self):
        tokenStr = str(self.tokenValues)
        newTokenStr, ok = QInputDialog.getText(self.widget, 'Edit tokens', 'Tokens:', text = tokenStr)
        
        if ok:
            try:
                newTokens = eval(newTokenStr)
                print('Updated tokens to: ' + str(newTokenStr))
                self.newTokenValues(newTokens)
                self.widget.editTokens(self.src, self.dst, newTokens)
            except:
                print('Invalid token entry')
            

            # newTokens = newTokenStr
            # newTokens = eval(newTokenStr)
            # print('Updated tokens to: ' + str(newTokenStr))
            # self.newTokenValues(newTokens)
            # self.widget.editTokens(self.src, self.dst, newTokens)  




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
        self.tokenColor = QColor(255, 255, 255)

        self.setAcceptHoverEvents(True)
        self.hover = False



    def boundingRect(self):
        #Used for collision detection and repaint
        rect = self.getTokenRect()
        if self.hover:
            rect = self.getTokenRectHover()

        return rect

    
    def shape(self):
        #Determines the collision area

        path = QPainterPath()
        path.addEllipse(self.boundingRect())

        return path


    def paint(self, painter, option, widget):
        lod = option.levelOfDetailFromTransform(painter.worldTransform())

        if lod > 0.15:
            if lod > 1 or self.rowLength < 3:
                valueSize = len(str(self.value))
                painter.setPen(QColor(0, 0, 0))
                painter.setBrush(self.tokenColor)
                
                rectValue = self.getTokenRect()
                if self.hover:
                	#Make token larger when the mouse hovers over it
                    rectValue = self.getTokenRectHover()

                painter.drawEllipse(rectValue)
                

                #Determine font size based on size of token content
                if self.hover:
                	#Larger text when hovering
                    if valueSize == 1:
                        rectValue = QRectF(rectValue.x(), rectValue.x() + 1, rectValue.width(), rectValue.height())
                        painter.setFont(QFont("Lucida Console", 13))
                    elif valueSize == 2:
                        painter.setFont(QFont("Lucida Console", 11))
                    elif valueSize == 3:
                        painter.setFont(QFont("Lucida Console", 8))
                    elif valueSize > 3:
                        painter.setFont(QFont("Lucida Console", 6))
                else:
                	#Normal size text when not hovering
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


    def hoverEnterEvent(self, event):
        self.hover = True
        self.setZValue(self.zValue() + 4)   
        
        self.prepareGeometryChange()  

        super().hoverEnterEvent(event)
        self.update()


    def hoverLeaveEvent(self, event):
        self.hover = False

        if self.zValue() == 8:
            self.setZValue(8)
        else:            
            self.setZValue(self.zValue() - 4)

        self.prepareGeometryChange()

        super().hoverLeaveEvent(event)
        self.update()


    def contextMenuEvent(self, event):
        #Gets point of right click and converts it to a position on the scene
        self.cluster.contextMenu(event.scenePos())


#------------------
#---Other---
    def getTokenRect(self):
        return QRectF(-self.tokenWidth * 0.5, -self.tokenHeight * 0.5, self.tokenWidth, self.tokenHeight)


    def getTokenRectHover(self):
        return QRectF(-self.tokenWidth * 0.75 , -self.tokenHeight * 0.75, self.tokenWidth * 1.5, self.tokenHeight * 1.5)


    def getPointOnEdge(self, t):
        return self.edge.getPointOnEdge(t)


    def getPointCloseToCenter(self, distance):
        return self.edge.getPointCloseToCenter(distance)


    def updatePos(self):          
        #Update postion of the token based on its position in the row
        self.cluster.tokensAreClusterd = False
        self.setVisible(True)
        self.prepareGeometryChange()

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
                self.setVisible(False)
            #Draw a cluster instead of all the tokens in the middle
            self.cluster.tokensAreClusterd = True
        
        self.prepareGeometryChange()
        self.update()

    
    def setZValueToken(self, zValue):
        self.setZValue(zValue)

        if self.hover:
            self.setZValue(8)


    def updateRowLength(self, length):
        self.rowLength = length


    

