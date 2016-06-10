#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of SDF graph.

author: Rinse Wester

"""

import sys
from PyQt5.QtWidgets import QDockWidget, QApplication, QMainWindow, QAction, QFileDialog, qApp
from PyQt5.QtCore import Qt

from sdfsim import *
from runwindow import *
from logwindow import LogWidget
from sdfsimgui import GraphWidget


class MainWindow(QMainWindow):

    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):

        openAction = QAction('&Open', self)
        openAction.setShortcut('Ctrl+O')
        openAction.setStatusTip('Open graph')
        openAction.triggered.connect(self.openActionTriggered)

        exitAction = QAction('&Exit', self)
        exitAction.setShortcut('Ctrl+Q')
        exitAction.setStatusTip('Exit application')
        exitAction.triggered.connect(qApp.quit)

        menu = self.menuBar().addMenu('&File')
        menu.addAction(openAction)
        menu.addAction(exitAction)

        self.sbar = self.statusBar()


        self.graph = G0


        self.dwRunWindow = QDockWidget('Simulate graph', self)
        self.runWindow = RunWindow()
        self.runWindow.setGraph(self.graph)
        self.dwRunWindow.setAllowedAreas(Qt.LeftDockWidgetArea)
        self.dwRunWindow.setWidget(self.runWindow)
        self.addDockWidget(Qt.LeftDockWidgetArea, self.dwRunWindow)

        self.dwLogWindow = QDockWidget('Edge log', self)
        self.logWindow = LogWidget()
        self.logWindow.setMinimumHeight(250)
        edgeNames = []
        edgeData = []
        if self.graph is not None:
            for (src, dst), edata in self.graph.edgestates.items():
                edgeNames.append(src + ' → ' + dst)
                edgeData.append(edata)
        self.logWindow.setEdgeLabels(edgeNames)
        self.logWindow.setEdgeData(edgeData)
        self.dwLogWindow.setAllowedAreas(Qt.BottomDockWidgetArea)
        self.dwLogWindow.setWidget(self.logWindow)
        self.addDockWidget(Qt.BottomDockWidgetArea, self.dwLogWindow)
        self.runWindow.setLogWidget(self.logWindow)

        self.graphWidget = GraphWidget()
        self.graphWidget.setGraph(self.graph)
        self.runWindow.setGraphWidget(self.graphWidget)

        self.scrlarea = QScrollArea(self)
        self.scrlarea.setAutoFillBackground(True)
        p = self.scrlarea.palette()
        p.setColor(self.scrlarea.backgroundRole(), Qt.white)
        self.scrlarea.setPalette(p)
        self.scrlarea.setWidget(self.graphWidget)
        self.setCentralWidget(self.scrlarea)

        self.setWindowTitle('SDFkit')
        self.setGeometry(300, 300, 1000, 750)
        self.show()

    def openActionTriggered(self):

        graphfile, _ = QFileDialog.getOpenFileName(self, 'Open graph')
        if graphfile != '':
            print('lets open ', graphfile)


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = MainWindow()
    app.exec_()
    app.deleteLater()
    sys.exit()
