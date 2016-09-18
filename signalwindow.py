#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of SDF graph.

author: Rinse Wester

"""

from PyQt5.QtWidgets import QApplication, QWidget, QLabel, QScrollArea, QTableWidget, QAbstractItemView, QHeaderView, QPushButton
from PyQt5.QtCore import Qt, QRect, QPoint
from PyQt5.QtGui import QPainter, QFont, QPen, QBrush, QPolygon, QColor
import sys
import random as rdm
from math import floor
from collections import OrderedDict

class SignalTable(QTableWidget):
    """Widget for displaying contents of edges and firing of nodes."""
    def __init__(self):
        super().__init__()

        self.setColumnCount(1)
        self.setGridStyle(Qt.NoPen)
        self.setHorizontalScrollMode(QAbstractItemView.ScrollPerPixel)
        self.setSortingEnabled(True)

        hheader = self.horizontalHeader()
        hheader.setVisible(False)
        hheader.setSectionResizeMode(QHeaderView.ResizeToContents)

        vheader = self.verticalHeader()
        vheader.setSectionResizeMode(QHeaderView.ResizeToContents)
        vheader.setSectionsMovable(True)

        # ordered dictionay to keeps refs to data and signal widget
        self.refDict = OrderedDict({})

    def _signalIndex(self, signalname):
        return list(self.refDict.keys()).index(signalname)

    def updateSignal(self, signalname, signaldata):
        self.refDict[signalname].setTokenData(signaldata)
        self.resizeColumnsToContents()
        self.resizeRowsToContents()

    def addSignal(self, signalname, signaldata):
        if signalname not in self.refDict.keys():
            signalwidget = SignalLogWidget()
            signalwidget.setTokenData(signaldata)
            self.refDict[signalname] = signalwidget
            self.setRowCount(len(self.refDict))
            self.setVerticalHeaderLabels(self.refDict.keys())
            self.setCellWidget(len(self.refDict) - 1, 0, signalwidget)
            self.resizeColumnsToContents()
            self.resizeRowsToContents()

    def removeSignal(self, signalname):
        if signalname in self.refDict.keys():
            index = self._signalIndex(signalname)
            signalwidget = self.refDict[signalname]
            signalwidget.deleteLater()
            del(self.refDict[signalname])
            self.removeRow(index)
            self.resizeColumnsToContents()
            self.resizeRowsToContents()

    def clearSignals(self):
        """Remove all all signals"""
        for signalwidget in self.refDict.values():
            signalwidget.deleteLater()
        self.refDict.clear()
        self.setRowCount(0)
        self.resizeColumnsToContents()
        self.resizeRowsToContents()

    def scrollToEnd(self):
        hScrollBar = self.horizontalScrollBar()
        maxval = hScrollBar.maximum()
        hScrollBar.setValue(maxval)

class SignalLogWidget(QWidget):

    STATE_WIDTH = 100
    TRANSITION_WIDTH = 20

    SIGNAL_COLOR = QColor(50, 50, 54)
    SIGNAL_COLOR_HOVER = QColor(74, 73, 80)
    TEXT_COLOR = QColor(194, 194, 195)

    def __init__(self):

        super().__init__()

        self.setAutoFillBackground(True)
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.white)
        self.setPalette(p)
        self.setMouseTracking(True)
        self.setMinimumHeight(20)

        self.tokenData = [[1, 2, 3], [1, 2]]
        self.zoomFactor = 1.0
        self.hoveringCycle = None

    def _updateSize(self):

        self.setFixedWidth(
            (len(self.tokenData) - 1) *
            (self.STATE_WIDTH * self.zoomFactor + self.TRANSITION_WIDTH))

    def setTokenData(self, tokenData):

        self.tokenData = tokenData[:]
        # add empty list to make sure the the last transtion goes to empty
        self.tokenData.append([])
        self._updateSize()
        self.update()

    def setZoomFactor(self, zoomFactor):

        self.zoomFactor = zoomFactor
        self._updateSize()
        self.update()

    def mouseMoveEvent(self, event):

        xpos = event.pos().x()
        self.hoveringCycle = floor((xpos + self.TRANSITION_WIDTH / 2) / (self.STATE_WIDTH * self.zoomFactor + self.TRANSITION_WIDTH))
        self.update()

    def leaveEvent(self, event):

        self.hoveringCycle = None
        self.update()
        super().leaveEvent(event)

    def paintEvent(self, e):

        curStatewidth = self.STATE_WIDTH * self.zoomFactor

        qp = QPainter()
        qp.begin(self)
        qp.setRenderHint(QPainter.Antialiasing)

        font = QFont('Serif', 10, QFont.Bold)
        qp.setFont(font)

        for i, tokenData in enumerate(self.tokenData[:-1]):

            curhovering = i == self.hoveringCycle
            nxthovering = i + 1 == self.hoveringCycle

            beginCoor = i * (curStatewidth + self.TRANSITION_WIDTH)

            self._drawState(qp, beginCoor, curStatewidth, tokenData, curhovering)

            if tokenData == self.tokenData[i + 1]:
                if tokenData == []:
                    transType = 'empty2empty'
                else:
                    transType = 'sameData'
            else:
                if tokenData != [] and self.tokenData[i + 1] != []:
                    transType = 'dataChange'
                elif tokenData == []:
                    transType = 'empty2data'
                else:
                    transType = 'data2empty'

            self._drawTransition(qp, beginCoor + curStatewidth,
                                 self.TRANSITION_WIDTH, transType, curhovering, nxthovering)

        qp.end()

    def _drawTransition(self, qp, xpos, width, ttype, hoverLeft, hoverRight):

        # Set pen and brush style
        color = SignalLogWidget.SIGNAL_COLOR
        pen = QPen(color)
        pen.setWidth(2)
        brush = QBrush(color)
        qp.setPen(pen)
        qp.setBrush(brush)
        pen.setStyle(Qt.NoPen)
        qp.setPen(pen)

        size = self.size()
        h = size.height()

        if ttype == 'sameData':
            if hoverLeft:
                brush.setColor(self.SIGNAL_COLOR_HOVER)
            else:
                brush.setColor(self.SIGNAL_COLOR)
            qp.setBrush(brush)
            qp.drawRect(xpos, 2, width / 2, h - 4)

            if hoverRight:
                brush.setColor(self.SIGNAL_COLOR_HOVER)
            else:
                brush.setColor(self.SIGNAL_COLOR)
            qp.setBrush(brush)
            qp.drawRect(xpos + width / 2, 2, width / 2, h - 4)

        elif ttype == 'dataChange':
            if hoverLeft:
                brush.setColor(self.SIGNAL_COLOR_HOVER)
            else:
                brush.setColor(self.SIGNAL_COLOR)
            qp.setBrush(brush)
            points = [QPoint(xpos, 2), QPoint(xpos, h - 2),
                      QPoint(xpos + width / 2, h / 2)]
            qp.drawPolygon(QPolygon(points))

            if hoverRight:
                brush.setColor(self.SIGNAL_COLOR_HOVER)
            else:
                brush.setColor(self.SIGNAL_COLOR)
            qp.setBrush(brush)
            points = [QPoint(xpos + width, 2), QPoint(xpos + width, h - 2),
                      QPoint(xpos + width / 2, h / 2)]
            qp.drawPolygon(QPolygon(points))

        elif ttype == 'empty2data':
            if hoverRight:
                brush.setColor(self.SIGNAL_COLOR_HOVER)
            else:
                brush.setColor(self.SIGNAL_COLOR)
            points = [QPoint(xpos + width / 2, h / 2),
                      QPoint(xpos + width, 2), QPoint(xpos + width, h - 2)]
            qp.setBrush(brush)
            qp.drawPolygon(QPolygon(points))

            if hoverLeft:
                pen.setColor(self.SIGNAL_COLOR_HOVER)
            else:
                pen.setColor(self.SIGNAL_COLOR)
            pen.setStyle(Qt.SolidLine)
            qp.setPen(pen)
            qp.drawLine(xpos, h / 2, xpos + width / 2, h / 2)
        elif ttype == 'data2empty':
            if hoverLeft:
                brush.setColor(self.SIGNAL_COLOR_HOVER)
            else:
                brush.setColor(self.SIGNAL_COLOR)
            points = [QPoint(xpos, 2), QPoint(xpos, h - 2), QPoint(xpos + width / 2, h / 2)]
            qp.setBrush(brush)
            qp.drawPolygon(QPolygon(points))

            if hoverRight:
                pen.setColor(self.SIGNAL_COLOR_HOVER)
            else:
                pen.setColor(self.SIGNAL_COLOR)
            pen.setStyle(Qt.SolidLine)
            qp.setPen(pen)
            qp.drawLine(xpos + width / 2, h / 2, xpos + width, h / 2)
        else:
            # 'empty2empty' case
            pen.setStyle(Qt.SolidLine)
            if hoverLeft:
                pen.setColor(self.SIGNAL_COLOR_HOVER)
            else: 
                pen.setColor(self.SIGNAL_COLOR)
            qp.setPen(pen)
            qp.drawLine(xpos, h / 2, xpos + width / 2, h / 2)

            if hoverRight:
                pen.setColor(self.SIGNAL_COLOR_HOVER)
            else: 
                pen.setColor(self.SIGNAL_COLOR)
            qp.setPen(pen)
            qp.drawLine(xpos + width / 2, h / 2, xpos + width, h / 2)

    def _drawState(self, qp, xpos, width, data, hovering=False):

        # Set pen and brush style
        if hovering:
            color = SignalLogWidget.SIGNAL_COLOR_HOVER
        else:
            color = SignalLogWidget.SIGNAL_COLOR

        pen = QPen(color)
        pen.setWidth(2)
        brush = QBrush(color)
        qp.setPen(pen)
        qp.setBrush(brush)

        size = self.size()
        h = size.height()

        # Draw datastripe when data contains tokens
        if data == []:
            qp.drawLine(xpos, h / 2, xpos + width, h / 2)
        else:
            qp.setPen(Qt.NoPen)
            qp.drawRect(xpos, 3, width, h - 6)

            qp.setPen(pen)
            qp.drawLine(xpos, 3, xpos + width, 3)
            qp.drawLine(xpos, h - 3, xpos + width, h - 3)

            pen.setColor(SignalLogWidget.TEXT_COLOR)
            qp.setPen(pen)

            rect = QRect(xpos + 3, 3, width - 3, h - 6)
            qp.drawText(rect, Qt.AlignCenter, str(data)[1:-1])


if __name__ == '__main__':

    app = QApplication(sys.argv)
    sw = SignalTable()
    sw.setGeometry(300, 300, 800, 400)
    sw.show()

    sw.addSignal('Alpha', [[1],[2],[],[2]])
    sw.addSignal('Beta', [[1],[1],[],[]])
    sw.addSignal('Gama', [[],[2],[],[]])

    # for i in range(25):
    #     signalname = 'Signal-' + str(i)
    #     signaldata = []
    #     for i in range(120):
    #         tokens = [1,2,3,4]
    #         rdm.shuffle(tokens)
    #         signaldata.append(tokens)
    #     sw.addSignal(signalname, signaldata)

    app.exec_()
    app.deleteLater()
    sys.exit()
