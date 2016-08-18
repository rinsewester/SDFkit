#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsItem, QPushButton, QVBoxLayout
from PyQt5.QtCore import QRectF, QPointF, QPoint, Qt, QVariant
from PyQt5.QtGui import QColor, QPainter, QBrush, QPainterPath, QLinearGradient, QFont
from collections import Counter

class Node(QGraphicsItem):

    def __init__(self, nodeName):
        super().__init__()
        
        self.ioWidth = 15
        self.ioHeight = 10
        self.ioHeightDifference = 10
        self.nodeBodyWidth = 80
        self.maxNameLength = 6        
        self.nodeBodyColor = QColor(210, 210, 210)
        self.nodeBodyColorGradient = QColor(180, 180, 180)
        self.nodeBodyColorSelected = QColor(150, 150, 150)
        self.nodeBodyColorHover = QColor(180, 180, 180)
        self.nodeInputColor = QColor(230, 230, 230)
        self.nodeInputColorHover = QColor(255, 255, 255)
        self.nodeOutputColor = QColor(120, 120, 120)
        self.nodeOutputColorHover = QColor(80, 80, 80)
        self.nodeNeutralColor = QColor(180, 180, 180, 100)
        self.nodeNeutralColorHover = QColor(180, 180, 180)
        self.lastPos = QPointF(0, 0)
        self.yTranslationLeftIO = 0
        self.yTranslationRightIO = 0
        self.snappingIsOn = True
        self.showNeutralIO = False



        self.nodeText = nodeName
        self.nodeTextDisplayed = ''

        self.edgeList = []
        self.ioList = []
        #Add 2x IO ('left' = left, 'right' = right /,/ 0 = neutral, 1 = input, 2 is output)
        self.addNewIO('left', 0)
        self.addNewIO('right', 0)


        

        self.setYTranslationLeftIO()
        self.setYTranslationRightIO()

        #Set flags for selecting, moving and enabling a position change event
        self.setFlags(QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemIsMovable | QGraphicsItem.ItemSendsGeometryChanges)
        self.setAcceptHoverEvents(True)
        self.hover = False

        #print('node succesfully created: "' + nodeName + '"')


    def boundingRect(self):
        #Used for collision detection and repaint
        return QRectF(0, 0, self.nodeBodyWidth, self.nodeBodyHeight)

    
    def shape(self):
        #Determines the collision area
        path = QPainterPath()
        path.addRect(0, 0, self.nodeBodyWidth, self.nodeBodyHeight)

        return path


#--------------
#---Painting---    
    def paint(self, painter, option, widget):
        lod = option.levelOfDetailFromTransform(painter.worldTransform())
        #print (lod)

        #Paint all elments based on the level of detail
        self.paintNodeBody(painter, lod)
        if lod > 0.2:
            self.paintNodeIO(painter)
        if lod > 0.4:
            self.paintNodeName(painter)


    def paintNodeBody(self, painter, lod):
        painter.setPen(QColor(0, 0, 0))
      
        #Subtle gradient
        if lod > 0.2:
            gradient = QLinearGradient(0, 0, self.nodeBodyWidth, self.nodeBodyHeight)
            gradient.setColorAt(0, self.nodeBodyColor)
            gradient.setColorAt(1, self.nodeBodyColorGradient)
            brush = QBrush(gradient)
        else:
            brush = QBrush(self.nodeBodyColor)

        if self.hover:
            brush = QBrush(self.nodeBodyColorHover)

        if QGraphicsItem.isSelected(self):
            brush = QBrush(self.nodeBodyColorSelected)

        painter.setBrush(brush)

        if lod > 0.1:
            painter.drawRoundedRect(0, 0, self.nodeBodyWidth, self.nodeBodyHeight, 10, 10)
        else:
        	painter.drawRect(0, 0, self.nodeBodyWidth, self.nodeBodyHeight)


    def paintNodeIO(self, painter):
        #Draw all IO
        for i in range(0, len(self.ioList)):
            #Center io if one side contains less io
            yTranslation = 0
            if self.ioList[i][3] == 'left':
                yTranslation = self.yTranslationLeftIO
            else:
                yTranslation = self.yTranslationRightIO

            #Determine io color based on IO TYPE and if mouse is HOVERING
            painter.setPen(QColor(0, 0, 0, 200))
            
            if self.ioList[i][5]:
            	painter.setPen(QColor(0, 0, 0))
            if self.ioList[i][4] == 0:
                #neutral
                if self.ioList[i][5]:
                    painter.setPen(QColor(0, 0, 0))
                    brush = QBrush(self.nodeNeutralColorHover)
                else:
                    painter.setPen(QColor(0, 0, 0, 60))
                    brush = QBrush(self.nodeNeutralColor)
            
            elif self.ioList[i][4] == 1:
                #input
                if self.ioList[i][5]:
                    brush = QBrush(self.nodeInputColorHover)
                else:
                    brush = QBrush(self.nodeInputColor)
            
            else:
                #output
                if self.ioList[i][5]:
                    brush = QBrush(self.nodeOutputColorHover)
                else:
                    brush = QBrush(self.nodeOutputColor)

            #Don't paint neutral IO if disabled
            if self.ioList[i][4] == 0 and self.showNeutralIO:
                painter.setBrush(brush)
                path = self.getRoundedRectPath(i, yTranslation, self.ioList[i][3])
                painter.drawPath(path.simplified())
            elif self.ioList[i][4] != 0:
                painter.setBrush(brush)
                path = self.getRoundedRectPath(i, yTranslation, self.ioList[i][3])
                painter.drawPath(path.simplified())
        
        painter.setPen(QColor(0, 0, 0))

    def paintNodeName(self, painter):
        if self.nodeTextDisplayed == '':
            self.setNodeName()
        
        painter.setFont(QFont("Arial", 8))
        painter.drawText(self.rectNodeName, Qt.AlignCenter, self.nodeTextDisplayed) 


#------------------
#---Mouse Events---
    def mousePressEvent(self, event):
        self.mouseIsOnIO(event.pos(), True)     

        super().mousePressEvent(event)
        self.update()

        #Must be done after super().mousePressEvent(event) in order to
        #flag the node again after clicking on an input/output
        self.setFlag(QGraphicsItem.ItemIsSelectable, True)
        self.setFlag(QGraphicsItem.ItemIsMovable, True)


    def mouseMoveEvent(self, event):
        #Code for ShiftModifier goes here

        self.update()
        super().mouseMoveEvent(event)


    def itemChange(self, change, value):
        #Move selected nodes and edges to the front, untill unselected
        if change == QGraphicsItem.ItemSelectedChange:
 
            if QGraphicsItem.isSelected(self):
            	#Unselected (since the flag is not updated yet)
                self.setZValue(0)
                self.setZValueEdges(1)
            else:
            	#Selected
                self.setZValue(4)
                self.setZValueEdges(5)

        #If the position of the node changes -> calculate position change
        #and move edges with the node
        newPos = value

        if change == QGraphicsItem.ItemPositionChange:
            if self.snappingIsOn:
            	#Snap node to closest grid point
                newPos = self.snapToGrid(newPos)

            posChange = newPos - self.lastPos
            self.moveEdges(posChange)
            self.lastPos = newPos        

        return super(Node, self).itemChange(change, newPos)


    def snapToGrid(self, position):
        #Return position of closest grid point
        gridSize = 50
        curPos = QPoint(position.x(), position.y())
        gridPos = QPoint(round(curPos.x() / gridSize) * gridSize, round(curPos.y() / gridSize) * gridSize)

        return gridPos


    def mouseReleaseEvent(self, event):
        self.setFlag(QGraphicsItem.ItemIsSelectable, True)
        self.setFlag(QGraphicsItem.ItemIsMovable, True)

        super().mouseReleaseEvent(event)
        self.update()


    def hoverMoveEvent(self, event):
        #Don't execute when the nodeBody is selected in order to prevent unselecting the nodeBody
        if not QGraphicsItem.isSelected(self):
            self.mouseIsOnIO(event.pos())

        super().hoverMoveEvent(event)
        self.update()

        #Must be done after super().mousePressEvent(event) in order to
        #flag the node again after clicking on an input/output
        self.setFlag(QGraphicsItem.ItemIsSelectable, True)
        self.setFlag(QGraphicsItem.ItemIsMovable, True)


    def hoverLeaveEvent(self, event):
        self.hover = False
        self.setHoveringToFalse()

        super().hoverLeaveEvent(event)
        self.update()


#------------------
#---In/Outputs-----
    def getIOPoint(self, sideIndex, side):
    	#Gets the point from where the IO rectangle is drawn
        addWidthForRightSide = 0
        if side == 'right':
            addWidthForRightSide = self.nodeBodyWidth - self.ioWidth

        ioPoint = QPointF(addWidthForRightSide, sideIndex * (self.ioHeightDifference + self.ioHeight) + self.ioHeight)

        #Returns the point of a specific io
        return ioPoint


    def getIOPointForEdge(self, side, ioType):
    	#Gets the point where an edge can connect to the IO
        addedX = 0
        addedY = 0

        if side == 'right':
            #Add x translation if the IO lies on the right side of the node
            addedX = self.nodeBodyWidth

            #Add y translation for the exact IO position relative to the node
            ioIndex = self.getLengthRightSide()
            addedY = (ioIndex - 1) * (self.ioHeightDifference + self.ioHeight) + self.ioHeight + (self.ioHeight / 2) + self.yTranslationRightIO
  
        else:
            ioIndex = self.getLengthLeftSide()
            addedY = (ioIndex - 1) * (self.ioHeightDifference + self.ioHeight) + self.ioHeight + (self.ioHeight / 2) + self.yTranslationLeftIO
  
        #Returns the calculated point of the IO
        ioPoint = QPointF(self.pos().x() + addedX, self.pos().y() + addedY)

        return ioPoint


    def addNewIO(self, side, ioType):
        if side == 'left':
            i = self.getLengthLeftSide()
        else:
            i = self.getLengthRightSide()

        #---newIO = (ioPoint.x, ioPoint.y, hasEdge, side, ioType, mouseHover)---
        newIO = (self.getIOPoint(i, side).x(), self.getIOPoint(i, side).y(), False, side, ioType, False)
        self.ioList.append(newIO)

        #Update the nodeBodyHeight
        self.updateNode()

        
    def setIOType(self, side, ioType):    
        #Update the type paramater of the IO
        i = self.getLastIOSide(side)

        self.ioList.insert(i, (self.ioList[i][0], self.ioList[i][1], self.ioList[i][2], self.ioList[i][3], ioType, self.ioList[i][5]))
        del self.ioList[i + 1]


    def mouseIsOnIO(self, mousePos, click = False):    	
    	#Returns the IO that the mouse is on
        for i in range(0, len(self.ioList)):
            #Adjust if IO is centered on a side
            if self.ioList[i][3] == 'left':
                yTranslation = self.yTranslationLeftIO
            else:
                yTranslation = self.yTranslationRightIO

            #Get point of IO
            IOPoint = QPointF(self.ioList[i][0], self.ioList[i][1] + yTranslation)

            #If mouse is over IO -> return IO
            if mousePos.x() > IOPoint.x() and mousePos.x() < IOPoint.x() + self.ioWidth:
                if mousePos.y() > IOPoint.y() and mousePos.y() < IOPoint.y() + self.ioHeight:
                    if click:
                        print('mouse on IO: ' + str(i) + ' (' + str(self.ioList[i][3]) + ', ' + str(self.ioList[i][4]) + ')')
                    
                    #Update the hover paramater of the IO
                    self.ioList.insert(i, (self.ioList[i][0], self.ioList[i][1], self.ioList[i][2], self.ioList[i][3], self.ioList[i][4], True))
                    del self.ioList[i + 1]

                    self.setFlag(QGraphicsItem.ItemIsSelectable, False)
                    self.setFlag(QGraphicsItem.ItemIsMovable, False)
                    self.hover = False
                    return i
        #If no IO is found under the mouse -> make sure hovering is enabled and return -1
        self.hover = True
        self.setHoveringToFalse()
        return -1


    def setHoveringToFalse(self):
        for i in range(0, len(self.ioList)):
        	#Set all hover parameters to false
            self.ioList.insert(i, (self.ioList[i][0], self.ioList[i][1], self.ioList[i][2], self.ioList[i][3], self.ioList[i][4], False))
            del self.ioList[i + 1]


    def setYTranslationLeftIO(self):
        i = 1
        # #YTranslation is used to center the IO on one side if the other side contains more IO
        # yTranslationDifference = self.yTranslationLeftIO

        # leftSideLength = self.getLengthLeftSide()
        # rightSideLength = self.getLengthRightSide()

        # if leftSideLength < rightSideLength:
        #     totalHeightInputs = (leftSideLength * self.ioHeight + (leftSideLength - 1) * self.ioHeightDifference)
        #     totalHeightAvailableForInputs = self.nodeBodyHeight - self.ioHeightDifference * 2
        #     self.yTranslationLeftIO = (totalHeightAvailableForInputs - totalHeightInputs) / 2
        # else:
        #     self.yTranslationLeftIO = 0

        # #Update edge positions when the translation is changed
        # yTranslationDifference += self.yTranslationLeftIO
        # posChange = QPointF(0, yTranslationDifference)
        # self.moveEdges(posChange, 'left')
        # print("info:")
        # print(self.getLengthLeftSide())
        # print(self.getLengthRightSide())
        # print(posChange)
    

    def setYTranslationRightIO(self):
        i = 1
        # #YTranslation is used to center the IO on one side if the other side contains more IO
        # yTranslationDifference = self.yTranslationRightIO

        # leftSideLength = self.getLengthLeftSide()
        # rightSideLength = self.getLengthRightSide()

        # if rightSideLength < leftSideLength:
        #     totalHeightOutputs = (rightSideLength * self.ioHeight + (rightSideLength - 1) * self.ioHeightDifference)
        #     totalHeightAvailableForOutputs = self.nodeBodyHeight - self.ioHeightDifference * 2
        #     self.yTranslationRightIO = (totalHeightAvailableForOutputs - totalHeightOutputs) / 2
        # else:
        #     self.yTranslationRightIO = 0

        # #Update edge positions when the translation is changed
        # yTranslationDifference += self.yTranslationRightIO
        # posChange = QPointF(0, yTranslationDifference)
        # self.moveEdges(posChange, 'right')
        # print("info:")
        # print(self.getLengthLeftSide())
        # print(self.getLengthRightSide())
        # print(posChange)


#------------------
#---Other----------
    def updateNode(self):
    	#Update the dimentional values of the node and its IO
        self.calculateNodeBodyHeight()
        self.setYTranslationLeftIO()
        self.setYTranslationRightIO()


    def calculateNodeBodyHeight(self):
        #Get how many inputs/outputs are on each side
        ioOnLeftSide = self.getLengthLeftSide()
        ioOnRightSide = self.getLengthRightSide()

        #Pick the longest side
        if ioOnLeftSide > ioOnRightSide:
            longestSide = ioOnLeftSide
        else:
            longestSide = ioOnRightSide  

        #Make node smaller when the neutral IO is hidden
        if not self.showNeutralIO:
            longestSide = longestSide - 1
         
        #Set nodeBodyHeight based on longest io side
        self.nodeBodyHeight = (longestSide * (self.ioHeightDifference + self.ioHeight) + self.ioHeight)


    def getNodeBodyHeigth(self):
    	return self.nodeBodyHeight


    def getLengthLeftSide(self):
        countSides = Counter(elem[3] for elem in self.ioList)

        return countSides['left']


    def getLengthRightSide(self):
        countSides = Counter(elem[3] for elem in self.ioList)
        
        return countSides['right']


    def getLastIOSide(self, side):
    	#Returns the index of the last IO on a side
        ioIndex = 0
        for i in reversed(range(len(self.ioList))):
            if side in self.ioList[i]:
                ioIndex = i
                break

        return ioIndex


    def setNodeName(self):
    	#Determine the displayed name of the node and its location once
        self.nodeTextDisplayed = self.nodeText 

        if len(self.nodeText) > self.maxNameLength:
            #Cutoff text if the name is too long
            self.nodeTextDisplayed = self.nodeText[:self.maxNameLength]
            self.nodeTextDisplayed += '..'

        textPoint = QPoint(self.ioWidth + 4, 3)
        textWidth = self.nodeBodyWidth - self.ioWidth * 2 - 8
        textEndPoint = QPoint(textPoint.x() + textWidth, textPoint.y() + 10)
        self.rectNodeName = QRectF(textPoint, textEndPoint)


    def getRoundedRectPath(self, i, yTranslation, side):
        path = QPainterPath();
        path.setFillRule(Qt.WindingFill);
        
        path.addRoundedRect(self.ioList[i][0], self.ioList[i][1] + yTranslation, self.ioWidth, self.ioHeight, 2, 2)
        
        #Remove rounded edges on left or right side
        if side == 'left':
            path.addRect(self.ioList[i][0], self.ioList[i][1] + yTranslation, 2, 2)
            path.addRect(self.ioList[i][0], self.ioList[i][1] + yTranslation + self.ioHeight - 2, 2, 2)
        else:
            path.addRect(self.ioList[i][0] + self.ioWidth - 2, self.ioList[i][1] + yTranslation, 2, 2)
            path.addRect(self.ioList[i][0] + self.ioWidth - 2, self.ioList[i][1] + yTranslation + self.ioHeight - 2, 2, 2)

        return path


    def addEdge(self, edge, edgeSide):
    	#Add new edge with: (reference to edge, 'begin' or 'end')
        newEdge = (edge, edgeSide)
        self.edgeList.append(newEdge)


    def moveEdges(self, posChange, side = 'both'):
        #Move edges connected to node
        if len(self.edgeList) > 0:
            for i in range(len(self.edgeList)):
                if 'begin' in self.edgeList[i]:
                    #Only move edge side if the entire edge is moved or the specified side is moved
                    if side == 'both' or side == self.ioList[i][3]:
                        self.edgeList[i][0].moveEdge(posChange, 'begin')
                else:
                    if side == 'both' or side == self.ioList[i][3]:
                        self.edgeList[i][0].moveEdge(posChange, 'end')


    def setZValueEdges(self, zValue):
        for i in range(len(self.edgeList)):
        	self.edgeList[i][0].setZValueEdge(zValue)