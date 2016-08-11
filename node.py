#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsItem, QPushButton, QVBoxLayout
from PyQt5.QtCore import QRectF, QPointF, QPoint, Qt
from PyQt5.QtGui import QColor, QPainter, QBrush, QPainterPath, QLinearGradient
from collections import Counter

class Node(QGraphicsItem):

    def __init__(self, nodeName):
        super().__init__()
        
        self.ioWidth = 15
        self.ioHeight = 10
        self.ioHeightDifference = 10
        self.nodeBodyWidth = 100
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


        self.nodeText = nodeName
        self.nodeTextDisplayed = ''


        self.ioList = []
        #Add 2x IO ('left' = left, 'right' = right /,/ 0 = neutral, 1 = input, 2 is output)
        self.addNewIO('left', 1)
        self.addNewIO('left', 0)
        self.addNewIO('right', 1)
        self.addNewIO('right', 2)
        self.addNewIO('right', 0)
        #print(self.ioList)


        self.setYTranslationLeftIO()
        self.setYTranslationRightIO()

        self.setFlags(QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemIsMovable)
        self.setAcceptHoverEvents(True)
        self.hover = False

        #print('node succesfully created: "' + nodeName + '"')


    def boundingRect(self):
        #Used for collision detection
        return QRectF(0, 0, self.nodeBodyWidth, self.nodeBodyHeight)

    
    def shape(self):
        #Determines the paint area
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

            painter.setBrush(brush)
            path = self.getRoundedRectPath(i, yTranslation, self.ioList[i][3])
            painter.drawPath(path.simplified())
            #painter.drawRoundedRect(self.ioList[i][0], self.ioList[i][1] + yTranslation, self.ioWidth, self.ioHeight, 2, 2)
        
        painter.setPen(QColor(0, 0, 0))

    def paintNodeName(self, painter):
        if self.nodeTextDisplayed == '':
            self.setNodeName()
        
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
    def getIOPoint(self, sideIndex, leftOrRight):
        addWidthForRightSide = 0
        if leftOrRight == 'right':
            addWidthForRightSide = self.nodeBodyWidth - self.ioWidth

        ioPoint = QPointF(addWidthForRightSide, sideIndex * (self.ioHeightDifference + self.ioHeight) + self.ioHeight)

        #Returns the point of a specific io
        return ioPoint


    def getInputPointForEdge(self, inputIndex):
        inputPoint = QPointF(self.pos().x(), self.pos().y() +  inputIndex * (self.ioHeightDifference + self.ioHeight) + self.ioHeight + (self.ioHeight / 2) + self.yTranslationLeftIO)

        #Returns the point where an edge can connect to a specific input
        return inputPoint


    def getOutputPointForEdge(self, outputIndex):
        outputPoint = QPointF(self.pos().x() + self.nodeBodyWidth, self.pos().y() + outputIndex * (self.ioHeightDifference + self.ioHeight) + self.ioHeight + (self.ioHeight / 2) + self.yTranslationRightIO)

        #Returns the point of a specific output
        return outputPoint


    def addNewIO(self, leftOrRight, neutralInputOrOutput):
        if leftOrRight == 'left':
            i = self.getLenghtLeftSide()
        else:
            i = self.getLenghtRightSide()

        #---newIO = (ioPoint.x, ioPoint.y, hasEdge, leftOrRight, neutralInputOrOutput, mouseHover)---
        newIO = (self.getIOPoint(i, leftOrRight).x(), self.getIOPoint(i, leftOrRight).y(), False, leftOrRight, neutralInputOrOutput, False)
        self.ioList.append(newIO)

        #Update the nodeBodyHeight
        self.updateNode()
        
    def setIOType(self, ioIndex, neutralInputOrOutput):
        self.ioList[ioIndex][4] = neutralInputOrOutput       


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
        #YTranslation is used to center the IO on one side if the other side contains more IO
        leftSideLength = self.getLenghtLeftSide()
        rightSideLength = self.getLenghtRightSide()

        if leftSideLength < rightSideLength:
            totalHeightInputs = (leftSideLength * self.ioHeight + (leftSideLength - 1) * self.ioHeightDifference)
            totalHeightAvailableForInputs = self.nodeBodyHeight - self.ioHeightDifference * 2
            self.yTranslationLeftIO = (totalHeightAvailableForInputs - totalHeightInputs) / 2
        else:
            self.yTranslationLeftIO = 0
    

    def setYTranslationRightIO(self):
        #YTranslation is used to center the IO on one side if the other side contains more IO
        leftSideLength = self.getLenghtLeftSide()
        rightSideLength = self.getLenghtRightSide()

        if rightSideLength < leftSideLength:
            totalHeightOutputs = (rightSideLength * self.ioHeight + (rightSideLength - 1) * self.ioHeightDifference)
            totalHeightAvailableForOutputs = self.nodeBodyHeight - self.ioHeightDifference * 2
            self.yTranslationRightIO = (totalHeightAvailableForOutputs - totalHeightOutputs) / 2
        else:
            self.yTranslationRightIO = 0


#------------------
#---Other----------
    def updateNode(self):
    	#Update the dimentional values of the node and its IO
        self.calculateNodeBodyHeight()
        self.setYTranslationLeftIO()
        self.setYTranslationRightIO()


    def calculateNodeBodyHeight(self):
        #Get how many inputs/outputs are on each side
        ioOnLeftSide = self.getLenghtLeftSide()
        ioOnRightSide = self.getLenghtRightSide()

        #Pick the longest side
        if ioOnLeftSide > ioOnRightSide:
            longestSide = ioOnLeftSide
        else:
            longestSide = ioOnRightSide  
         
        #Set nodeBodyHeight based on longest io side
        self.nodeBodyHeight = (longestSide * (self.ioHeightDifference + self.ioHeight) + self.ioHeight)


    def getLenghtLeftSide(self):
        countSides = Counter(elem[3] for elem in self.ioList)

        return countSides['left']


    def getLenghtRightSide(self):
        countSides = Counter(elem[3] for elem in self.ioList)
        
        return countSides['right']


    def setNodeName(self):
    	#Determine the displayed name of the node and its location once
        self.nodeTextDisplayed = self.nodeText

        maxLength = 9

        if len(self.nodeText) > maxLength:
            #Cutoff text if the name is too long
            self.nodeTextDisplayed = self.nodeText[:maxLength]
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