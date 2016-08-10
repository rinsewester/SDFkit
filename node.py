#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Sander Giesselink

"""

import sys
from PyQt5.QtWidgets import QWidget, QGraphicsItem, QPushButton, QVBoxLayout
from PyQt5.QtCore import QRectF, QPointF, QPoint
from PyQt5.QtGui import QColor, QPainter, QBrush, QPainterPath, QLinearGradient

class Node(QGraphicsItem):

    def __init__(self, nodeName):
        super().__init__()
        
        self.ioWidth = 15
        self.ioHeight = 10
        self.ioHeightDifference = 10
        self.nodeBodyWidth = 100
        self.nodeBodyColor = QColor(210, 210, 210)
        self.nodeBodyColorGradient = QColor(190, 190, 190)
        self.nodeBodyColorSelected = QColor(150, 150, 150)
        self.nodeBodyColorHover = QColor(180, 180, 180)
        self.nodeInputColor = QColor(240, 240, 240)
        self.nodeInputColorSelected = QColor(220, 220, 220)
        self.nodeOutputColor = QColor(120, 120, 120)
        self.nodeOutputColorSelected = QColor(100, 100, 100)


        self.nodeText = nodeName

        self.inputList = []
        self.addNewInput()
        self.addNewInput()
        #self.addNewInput()

        self.outputList = []
        self.addNewOutput()
        #self.addNewOutput()
        #self.addNewOutput()
        #self.addNewOutput()


        self.setYTranslationLeftIO()
        self.setYTranslationRightIO()

        self.setFlags(QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemIsMovable)
        self.setAcceptHoverEvents(True)
        self.hover = False
        print('node succesfully created: "' + nodeName + '"')


    #def get_pos(view, item)


    def boundingRect(self):
        #Used for collision detection
        return QRectF(0, 0, self.nodeBodyWidth, self.getNodeBodyHeight())

    
    def shape(self):
        #Determines the paint area
        path = QPainterPath()
        path.addRect(0, 0, self.nodeBodyWidth, self.getNodeBodyHeight())

        return path


#--------------
#---Painting---    
    def paint(self, painter, option, widget):
        self.paintNodeBody(painter)
        self.paintNodeInputs(painter)
        self.paintNodeOutputs(painter)
        self.paintNodeName(painter)


    def paintNodeBody(self, painter):
        color = QColor(0, 0, 0)
        painter.setPen(color)
      
        #Subtle gradient
        gradient = QLinearGradient(0, 0, self.nodeBodyWidth, self.getNodeBodyHeight())
        gradient.setColorAt(0, self.nodeBodyColor)
        gradient.setColorAt(1, self.nodeBodyColorGradient)
        brush = QBrush(gradient)
        #brush = QBrush(self.nodeBodyColor)

        if self.hover:
        	brush = QBrush(self.nodeBodyColorHover)

        if QGraphicsItem.isSelected(self):
            brush = QBrush(self.nodeBodyColorSelected)

        painter.setBrush(brush)
        painter.drawRoundedRect(0, 0, self.nodeBodyWidth, self.getNodeBodyHeight(), 10, 10)


    def paintNodeInputs(self, painter):
        color = QColor(0, 0, 0)
        painter.setPen(color)
        
        self.setYTranslationLeftIO()

        
        #Draw all inputs
        for i in range(0, len(self.inputList)):
            if self.inputList[i][3]:
                brush = QBrush(self.nodeInputColorSelected)  
            else:
                brush = QBrush(self.nodeInputColor) 
        

            painter.setBrush(brush)
            painter.drawRoundedRect(self.inputList[i][0], self.inputList[i][1] + self.yTranslationLeftIO, self.ioWidth, 10, 2, 2)
         
         
    def paintNodeOutputs(self, painter):
        color = QColor(0, 0, 0)
        painter.setPen(color)
        
        self.setYTranslationRightIO()

        #Draw all inputs
        for i in range(0, len(self.outputList)):
            if self.outputList[i][3]:
                brush = QBrush(self.nodeOutputColorSelected)  
            else:
                brush = QBrush(self.nodeOutputColor) 

            painter.setBrush(brush)
            painter.drawRoundedRect(self.outputList[i][0], self.outputList[i][1] + self.yTranslationRightIO, self.ioWidth, 10, 2, 2)


    def paintNodeName(self, painter):
        nodeTextDisplayed = self.nodeText

        maxLength = 10

        if len(self.nodeText) > maxLength:
        	#Cutoff text if the name is too long
            nodeTextDisplayed = self.nodeText[:maxLength]
            nodeTextDisplayed += '..'
            textPoint = QPoint(self.ioWidth + 2, self.ioHeight + 2)
        else:
        	#Calculate xTranslation to center text in node
            xTranslation = ((self.nodeBodyWidth - 2 * self.ioWidth - 2 * 2) / ((maxLength + 2) * 2)) * (maxLength - len(self.nodeText) + 2)
            textPointX = self.ioWidth + 2 + xTranslation
            textPoint = QPoint(textPointX, self.ioHeight + 2)

        painter.drawText(textPoint, nodeTextDisplayed)    


#------------------
#---Mouse Events---
    def mousePressEvent(self, event):
        if self.mouseIsOnInput(event.pos(), True) < 0:
            self.mouseIsOnOutput(event.pos(), True)

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
        if self.mouseIsOnInput(event.pos()) < 0:
            self.mouseIsOnOutput(event.pos())

        super().hoverMoveEvent(event)
        self.update()

        #Must be done after super().mousePressEvent(event) in order to
        #flag the node again after clicking on an input/output
        self.setFlag(QGraphicsItem.ItemIsSelectable, True)
        self.setFlag(QGraphicsItem.ItemIsMovable, True)


    def hoverLeaveEvent(self, event):
        self.hover = False

        super().hoverLeaveEvent(event)
        self.update()


#------------------
#---In/Outputs-----
    def getInputPoint(self, inputIndex):
        inputPoint = QPointF(0, inputIndex * (self.ioHeightDifference + self.ioHeight) + self.ioHeight)

        #Returns the point of a specific input
        return inputPoint


    def getOutputPoint(self, outputIndex):
        outputPoint = QPointF(self.nodeBodyWidth - self.ioWidth, outputIndex * (self.ioHeightDifference + self.ioHeight) + self.ioHeight)

        #Returns the point of a specific output
        return outputPoint


    def getInputPointForEdge(self, inputIndex):
        inputPoint = QPointF(self.pos().x(), self.pos().y() +  inputIndex * (self.ioHeightDifference + self.ioHeight) + self.ioHeight + (self.ioHeight / 2) + self.yTranslationLeftIO)

        #Returns the point where an edge can connect to a specific input
        return inputPoint


    def getOutputPointForEdge(self, outputIndex):
        outputPoint = QPointF(self.pos().x() + self.nodeBodyWidth, self.pos().y() + outputIndex * (self.ioHeightDifference + self.ioHeight) + self.ioHeight + (self.ioHeight / 2) + self.yTranslationRightIO)

        #Returns the point of a specific output
        return outputPoint


    def addNewInput(self):
        i = len(self.inputList)

        #---newInput = (inputPoint.x, inputPoint.y, hasEdge, mouseHover)---
        newInput = (self.getInputPoint(i).x(), self.getInputPoint(i).y(), False, False)
        self.inputList.append(newInput)


    def addNewOutput(self):
        i = len(self.outputList)

        #---newOutput = (outputPoint.x, outputPoint.y, hasEdge, mouseHover)---
        newOutput = (self.getOutputPoint(i).x(), self.getOutputPoint(i).y(), False, False)
        self.outputList.append(newOutput)

    
    def mouseIsOnInput(self, mousePos, click = False):
    	#for i in self.inputList
        for i in range(0, len(self.inputList)):
            inputPoint = QPointF(self.inputList[i][0], self.inputList[i][1] + self.yTranslationLeftIO)

            #If mouse is over input -> return input
            if mousePos.x() > inputPoint.x() and mousePos.x() < inputPoint.x() + self.ioWidth:
                if mousePos.y() > inputPoint.y() and mousePos.y() < inputPoint.y() + self.ioHeight:
                    if click:
                        print('mouse on input: ' + str(i))
                    self.setFlag(QGraphicsItem.ItemIsSelectable, False)
                    self.setFlag(QGraphicsItem.ItemIsMovable, False)
                    self.hover = False
                    return i

        self.hover = True
        return -1


    def mouseIsOnOutput(self, mousePos, click = False):
        for i in range(0, len(self.outputList)):
            outputPoint = QPointF(self.outputList[i][0], self.outputList[i][1] + self.yTranslationRightIO)

            #If mouse is over input -> return output
            if mousePos.x() > outputPoint.x() and mousePos.x() < outputPoint.x() + self.ioWidth:
                if mousePos.y() > outputPoint.y() and mousePos.y() < outputPoint.y() + self.ioHeight:
                    if click:
                        print('mouse on output: ' + str(i))
                    self.setFlag(QGraphicsItem.ItemIsSelectable, False)
                    self.setFlag(QGraphicsItem.ItemIsMovable, False)
                    self.hover = False
                    return i

        self.hover = True
        return -1


    def setYTranslationLeftIO(self):
        if len(self.inputList) < len(self.outputList):
            totalHeightInputs = (len(self.inputList) * self.ioHeight + (len(self.inputList) - 1) * self.ioHeightDifference)
            totalHeightAvailableForInputs = self.getNodeBodyHeight() - self.ioHeightDifference * 2
            self.yTranslationLeftIO = (totalHeightAvailableForInputs - totalHeightInputs) / 2
        else:
            self.yTranslationLeftIO = 0
    

    def setYTranslationRightIO(self):
        if len(self.outputList) < len(self.inputList):
            totalHeightOutputs = (len(self.outputList) * self.ioHeight + (len(self.outputList) - 1) * self.ioHeightDifference)
            totalHeightAvailableForOutputs = self.getNodeBodyHeight() - self.ioHeightDifference * 2
            self.yTranslationRightIO = (totalHeightAvailableForOutputs - totalHeightOutputs) / 2
        else:
            self.yTranslationRightIO = 0


#------------------
#---Other----------
    def getNodeBodyHeight(self):
        longestList = len(self.inputList)
        if len(self.outputList) > len(self.inputList):
            longestList = len(self.outputList)

        return (longestList * (self.ioHeightDifference + self.ioHeight) + self.ioHeight)

