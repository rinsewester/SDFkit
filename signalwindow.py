#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of SDF graph.

author: Rinse Wester

"""

from PyQt5.QtWidgets import QApplication, QWidget, QLayout, QVBoxLayout, QHBoxLayout, QLabel, QScrollArea, QTableWidget, QAbstractItemView, QHeaderView, QPushButton
from PyQt5.QtCore import Qt, QRect, QPoint
from PyQt5.QtGui import QPainter, QFont, QPen, QBrush, QPolygon
import sys


class SignalWidget(QWidget):

    def __init__(self):
        super().__init__()

        self.lblEdgesHeader = QLabel('<b>Edge</b>', self)
        self.lblEdgesHeader.setMinimumWidth(80)
        self.lblEdgesHeader.setMaximumWidth(80)

        self.lblDataHeader = QLabel('<b>Data</b>', self)

        self.hboxHeader = QHBoxLayout()
        self.hboxHeader.addWidget(self.lblEdgesHeader)
        self.hboxHeader.addWidget(self.lblDataHeader)

        # create an empty vbox layout to hold the edge labels
        self.vboxlabels = QVBoxLayout()
        self.vboxlabels.setContentsMargins(0, 0, 0, 0)
        self.vboxlabels.setSizeConstraint(QLayout.SetMinAndMaxSize)

        self.lblswidget = QWidget(self)
        self.lblswidget.setMinimumWidth(80)
        self.lblswidget.setMaximumWidth(80)
        self.lblswidget.setLayout(self.vboxlabels)

        # create an empty vbox
        self.vboxdata = QVBoxLayout()
        self.vboxdata.setContentsMargins(0, 0, 0, 0)
        self.vboxdata.setSizeConstraint(QLayout.SetMinAndMaxSize)

        self.datascrollwidget = QScrollArea()
        self.datascrollwidget.setAutoFillBackground(True)
        p = self.datascrollwidget.palette()
        p.setColor(self.datascrollwidget.backgroundRole(), Qt.white)
        self.datascrollwidget.setPalette(p)
        self.datawidget = QWidget()
        self.datawidget.setLayout(self.vboxdata)
        self.datascrollwidget.setWidget(self.datawidget)

        self.hboxbody = QHBoxLayout()
        self.hboxbody.addWidget(self.lblswidget)
        self.hboxbody.addWidget(self.datascrollwidget)

        self.vboxmain = QVBoxLayout()
        self.vboxmain.addLayout(self.hboxHeader)
        self.vboxmain.addLayout(self.hboxbody)

        self.setLayout(self.vboxmain)

        self.setEdgeLabels(['Alpha', 'Beta', 'Gamma'])
        self.setEdgeData([
            [[1, 2], [], []],
            [[1, 2], [], [9, 9]],
            [[2, 3], [], []]])

        self.setMinimumHeight(6 * 24 + 64)

    def setEdgeLabels(self, lbls):

        # clean the vboxlayout with the labels
        while self.vboxlabels.count():
            item = self.vboxlabels.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.deleteLater()

        # Create the edge labels and put in layout
        for lbl in lbls:
            lblwidget = QLabel(lbl)
            lblwidget.setMinimumHeight(24)
            self.vboxlabels.addWidget(lblwidget)
        self.vboxlabels.addStretch()

    def setEdgeData(self, data):

        # clean the vboxlayout with the labels
        while self.vboxdata.count():
            item = self.vboxdata.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.deleteLater()

        # Create the edge labels and put in layout
        for edata in data:
            elwidget = EdgeLogWidget()
            elwidget.setTokenData(edata)
            elwidget.setMinimumHeight(24)
            self.vboxdata.addWidget(elwidget)
        self.vboxdata.addStretch()

class SignalTable(QTableWidget):
    """Widget for displaying contents of edges and firing of nodes."""
    def __init__(self):
        super().__init__()

        self.setRowCount(3)
        self.setColumnCount(1)
        self.setVerticalHeaderLabels(['a', 'b', 'c'])
        self.setGridStyle(Qt.NoPen)
        self.setHorizontalScrollMode(QAbstractItemView.ScrollPerPixel)
        self.setSortingEnabled(True)

        hheader = self.horizontalHeader()
        hheader.setVisible(False)
        hheader.setSectionResizeMode(QHeaderView.ResizeToContents)

        vheader = self.verticalHeader()
        vheader.setSectionResizeMode(QHeaderView.ResizeToContents)

        vheader.setSectionsMovable(True)
        # vheader.setDragEnabled(True)
        # vheader.setDragDropMode(QAbstractItemView.InternalMove)

        self.pbButton1 = QPushButton('set width to 100')
        self.pbButton1.setMinimumWidth(80)
        self.pbButton1.setFixedHeight(24)
        self.pbButton1.setStyleSheet('background-color: red;')
        self.pbButton1.clicked.connect(self.btnClicked)
        self.pbButton2 = QPushButton('set width to 200')
        self.pbButton2.setMinimumWidth(80)
        self.pbButton2.setFixedHeight(24)
        self.pbButton2.setStyleSheet('background-color: green;')
        self.pbButton2.clicked.connect(self.btnClicked)
        self.pbButton3 = QPushButton('set width to 300')
        self.pbButton3.setMinimumWidth(80)
        self.pbButton3.setFixedHeight(24)
        self.pbButton3.setStyleSheet('background-color: blue;')
        self.pbButton3.clicked.connect(self.btnClicked)

        self.setCellWidget(0, 0, self.pbButton1)
        self.setCellWidget(1, 0, self.pbButton2)
        self.setCellWidget(2, 0, self.pbButton3)

    def btnClicked(self):
        if self.sender() == self.pbButton1:
            self.pbButton1.setFixedWidth(100)
            self.pbButton2.setFixedWidth(100)
            self.pbButton3.setFixedWidth(100)
        elif self.sender() == self.pbButton2:
            self.pbButton1.setFixedWidth(200)
            self.pbButton2.setFixedWidth(200)
            self.pbButton3.setFixedWidth(200)
        else:
            self.pbButton1.setFixedWidth(300)
            self.pbButton2.setFixedWidth(300)
            self.pbButton3.setFixedWidth(300)
        self.resizeColumnsToContents()
        self.resizeRowsToContents()
            

class EdgeLogWidget(QWidget):

    STATE_WIDTH = 100
    TRANSITION_WIDTH = 20

    def __init__(self):

        super().__init__()

        self.setAutoFillBackground(True)
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.white)
        self.setPalette(p)

        self.setMinimumHeight(24)

        self.tokenData = [[1, 2, 3], [1, 2]]

        self.zoomFactor = 1.0

    def _updateMinWidth(self):

        self.setMinimumWidth(
            (len(self.tokenData) - 1) *
            (self.STATE_WIDTH * self.zoomFactor + self.TRANSITION_WIDTH))
        self.resize(8, 8)  # resize to minimum width and height

    def setTokenData(self, tokenData):

        self.tokenData = tokenData[:]
        # add empty list to make sure the the last transtion goes to empty
        self.tokenData.append([])
        self._updateMinWidth()
        self.update()

    def appendTokenData(self, tokenData):

        self.tokenData = self.tokenData[:-1] + tokenData
        # add empty list to make sure the the last transtion goes to empty
        self.tokenData.append([])
        self._updateMinWidth()
        self.update()

    def setZoomFactor(self, zoomFactor):

        self.zoomFactor = zoomFactor
        self._updateMinWidth()
        self.update()

    def paintEvent(self, e):

        curStatewidth = self.STATE_WIDTH * self.zoomFactor

        qp = QPainter()
        qp.begin(self)
        qp.setRenderHint(QPainter.Antialiasing)

        font = QFont('Serif', 10, QFont.Light)
        qp.setFont(font)

        for i, tokenData in enumerate(self.tokenData[:-1]):

            beginCoor = i * (curStatewidth + self.TRANSITION_WIDTH)

            self._drawState(qp, beginCoor, curStatewidth, tokenData)

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
                                 self.TRANSITION_WIDTH, transType)

        qp.end()

    def _drawTransition(self, qp, xpos, width, ttype):

        # Set pen and brush style
        pen = QPen(Qt.darkGray)
        pen.setWidth(2)
        brush = QBrush(Qt.lightGray)
        qp.setPen(pen)
        qp.setBrush(brush)

        size = self.size()
        h = size.height()

        if ttype == 'sameData':
            pen.setStyle(Qt.NoPen)
            qp.setPen(pen)
            qp.drawRect(xpos, 3, width, h - 6)

            pen.setStyle(Qt.SolidLine)
            qp.setPen(pen)
            qp.drawLine(xpos, 3, xpos + width, 3)
            qp.drawLine(xpos, h - 3, xpos + width, h - 3)
        elif ttype == 'dataChange':
            pen.setStyle(Qt.NoPen)
            qp.setPen(pen)

            points = [QPoint(xpos, 3), QPoint(xpos, h - 3),
                      QPoint(xpos + width, 3), QPoint(xpos + width, h - 3)]
            qp.drawPolygon(QPolygon(points))

            pen.setStyle(Qt.SolidLine)
            qp.setPen(pen)
            qp.drawLine(xpos, 3, xpos + width, h - 3)
            qp.drawLine(xpos, h - 3, xpos + width, 3)
        elif ttype == 'empty2data':
            pen.setStyle(Qt.NoPen)
            qp.setPen(pen)

            points = [QPoint(xpos + width / 2, h / 2),
                      QPoint(xpos + width, 3), QPoint(xpos + width, h - 3)]
            qp.drawPolygon(QPolygon(points))

            pen.setStyle(Qt.SolidLine)
            qp.setPen(pen)
            qp.drawLine(xpos, h / 2, xpos + width / 2, h / 2)
            qp.drawLine(xpos + width / 2, h / 2, xpos + width, 3)
            qp.drawLine(xpos + width / 2, h / 2, xpos + width, h - 3)
        elif ttype == 'data2empty':
            pen.setStyle(Qt.NoPen)
            qp.setPen(pen)

            points = [QPoint(xpos + width / 2, h / 2),
                      QPoint(xpos, 3), QPoint(xpos, h - 3)]
            qp.drawPolygon(QPolygon(points))

            pen.setStyle(Qt.SolidLine)
            qp.setPen(pen)
            qp.drawLine(xpos + width / 2, h / 2, xpos + width, h / 2)
            qp.drawLine(xpos, 3, xpos + width / 2, h / 2)
            qp.drawLine(xpos, h - 3, xpos + width / 2, h / 2)
        else:
            # 'empty2empty' case
            pen.setStyle(Qt.SolidLine)
            qp.setPen(pen)
            qp.drawLine(xpos, h / 2, xpos + width, h / 2)

    def _drawState(self, qp, xpos, width, data):

        # Set pen and brush style
        pen = QPen(Qt.darkGray)
        pen.setWidth(2)
        brush = QBrush(Qt.lightGray)
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

            pen.setColor(Qt.black)
            qp.setPen(pen)

            rect = QRect(xpos + 3, 3, width - 3, h - 6)
            qp.drawText(rect, Qt.AlignCenter, str(data)[1:-1])


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = SignalTable()
    ex.show()
    app.exec_()
    app.deleteLater()
    sys.exit()
