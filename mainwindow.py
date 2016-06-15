#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Rinse Wester

"""

import sys
from PyQt5.QtWidgets import QDockWidget, QApplication, QMainWindow, QAction, QFileDialog, QMessageBox, qApp
from PyQt5.QtCore import Qt
from PyQt5.QtGui import QIcon

from csdfgraph import *
from runwindow import *
from logwindow import LogWidget
from graphwidget import GraphWidget


class MainWindow(QMainWindow):

    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):

        openAction = QAction(QIcon('images/open.png'), '&Open', self)
        openAction.setShortcut('Ctrl+O')
        openAction.setStatusTip('Open graph')
        openAction.triggered.connect(self.openActionTriggered)

        exitAction = QAction(QIcon('images/exit.png'),'&Exit', self)
        exitAction.setShortcut('Ctrl+Q')
        exitAction.setStatusTip('Exit application')
        exitAction.triggered.connect(qApp.quit)

        filemenu = self.menuBar().addMenu('&File')
        filemenu.addAction(openAction)
        filemenu.addAction(exitAction)

        clashcodegenAction = QAction(QIcon('images/hardware.png'), '&Generate CLaSH code', self)
        clashcodegenAction.setShortcut('Ctrl+G')
        clashcodegenAction.setStatusTip('Generate CLaSH code')
        clashcodegenAction.triggered.connect(self.clashcodegenActionTriggered)

        softcodegenAction = QAction(QIcon('images/software.png'), 'Generate &software', self)
        softcodegenAction.setShortcut('Ctrl+W')
        softcodegenAction.setStatusTip('Generate C code')
        softcodegenAction.triggered.connect(self.softcodegenActionTriggered)

        gpucodegenAction = QAction(QIcon('images/gpu.png'), '&Generate OpenCL code', self)
        gpucodegenAction.setShortcut('Ctrl+L')
        gpucodegenAction.setStatusTip('Generate OpenCL code')
        gpucodegenAction.triggered.connect(self.gpucodegenActionTriggered)

        codegenmenu = self.menuBar().addMenu('&Code generation')
        codegenmenu.addAction(clashcodegenAction)
        codegenmenu.addAction(softcodegenAction)
        codegenmenu.addAction(gpucodegenAction)


        self.toolbar = self.addToolBar('Exit')
        self.toolbar.addAction(openAction)
        self.toolbar.addAction(clashcodegenAction)
        self.toolbar.addAction(softcodegenAction)
        self.toolbar.addAction(gpucodegenAction)
        self.toolbar.addAction(exitAction)



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
        self._updateLogWindow()
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
        self.setGeometry(300, 300, 1300, 750)
        self.show()

    def _updateLogWindow(self):

        edgeNames = []
        edgeData = []
        if self.graph is not None:
            for (src, dst), edata in self.graph.edgestates.items():
                edgeNames.append(src + ' → ' + dst)
                edgeData.append(edata)
        self.logWindow.setEdgeLabels(edgeNames)
        self.logWindow.setEdgeData(edgeData)


    def openActionTriggered(self):

        graphfile, _ = QFileDialog.getOpenFileName(
            self, 'Open graph', './examples')
        try:
            self.graph = CSDFGraph()
            self.graph.loadFromFile(graphfile)
        except (FileNotFoundError, ValueError, KeyError) as e:
            QMessageBox.critical(
                self, 'Error opening file',
                '<b>Error opening file:</b>' + '\n\n' + str(e))
            self.graph = None

        self.runWindow.setGraph(self.graph)
        self.graphWidget.setGraph(self.graph)
        self._updateLogWindow()

    def clashcodegenActionTriggered(self):
        QMessageBox.warning(
            self, 'Generate CLaSH code',
            'Generation of CLaSH code not yet supported')

    def softcodegenActionTriggered(self):
        QMessageBox.warning(
            self, 'Generate C code',
            'Generation of C code not yet supported')

    def gpucodegenActionTriggered(self):
        QMessageBox.warning(
            self, 'Generate OpenCL code',
            'Generation of OpenCL code not yet supported')


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = MainWindow()
    app.exec_()
    app.deleteLater()
    sys.exit()
