#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Mainwindow of SDFkit showing all widgets.

author: Rinse Wester

"""

import sys
from PyQt5.QtWidgets import QDockWidget, QApplication, QMainWindow, QAction, QFileDialog, QMessageBox, qApp
from PyQt5.QtCore import Qt
from PyQt5.QtGui import QIcon

from csdfgraph import CSDFGraph
from runwindow import RunWindow
from logwidget import LogWidget
from signalwindow import SignalTable, SignalLogWidget
from graphicsview import GraphWidget

from codegen.clashcodegen import ClashCodeGen


class MainWindow(QMainWindow):

    DEFAULT_GRAPH = 'examples/SDF/simple graph.json'

    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):

        openAction = QAction(QIcon('images/open.png'), '&Open', self)
        openAction.setShortcut('Ctrl+O')
        openAction.setStatusTip('Open graph')
        openAction.triggered.connect(self.openActionTriggered)

        saveAction = QAction(QIcon('images/save.png'), '&Save', self)
        saveAction.setShortcut('Ctrl+S')
        saveAction.setStatusTip('Save graph')
        saveAction.triggered.connect(self.saveActionTriggered)

        exitAction = QAction(QIcon('images/exit.png'), '&Exit', self)
        exitAction.setShortcut('Ctrl+Q')
        exitAction.setStatusTip('Exit application')
        exitAction.triggered.connect(qApp.quit)

        graphmenu = self.menuBar().addMenu('&Graph')
        graphmenu.addAction(openAction)
        graphmenu.addAction(saveAction)
        graphmenu.addAction(exitAction)

        clashcodegenAction = QAction(
            QIcon('images/hardware.png'), '&Generate CLaSH code', self)
        clashcodegenAction.setShortcut('Ctrl+G')
        clashcodegenAction.setStatusTip('Generate CLaSH code')
        clashcodegenAction.triggered.connect(self.clashcodegenActionTriggered)

        codegenmenu = self.menuBar().addMenu('&Code generation')
        codegenmenu.addAction(clashcodegenAction)

        resetZoomAction = QAction('&Reset zoom', self)
        resetZoomAction.setShortcut('Ctrl+0')
        resetZoomAction.setStatusTip('Reset zoom')

        zoomInAction = QAction('Zoom &in', self)
        zoomInAction.setShortcut('Ctrl+=')
        zoomInAction.setStatusTip('Zoom in')

        zoomOutAction = QAction('&Zoom out', self)
        zoomOutAction.setShortcut('Ctrl+-')
        zoomOutAction.setStatusTip('Zoom out')

        viewmenu = self.menuBar().addMenu('&View')
        viewmenu.addAction(zoomInAction)
        viewmenu.addAction(zoomOutAction)
        viewmenu.addAction(resetZoomAction)

        self.toolbar = self.addToolBar('toolbar')
        self.toolbar.setMovable(False)
        self.toolbar.addAction(openAction)
        self.toolbar.addAction(saveAction)
        self.toolbar.addAction(clashcodegenAction)

        self.setUnifiedTitleAndToolBarOnMac(True)

        self.graph = CSDFGraph()
        self.graph.loadFromFile(MainWindow.DEFAULT_GRAPH)

        self.graphWidget = GraphWidget()
        self.graphWidget.setGraph(self.graph)

        # connect the view menu actions to the graphwidget
        zoomInAction.triggered.connect(self.graphWidget.zoomIn)
        zoomOutAction.triggered.connect(self.graphWidget.zoomOut)
        resetZoomAction.triggered.connect(self.graphWidget.resetView)
        
        self.dwRunWindow = QDockWidget('Simulate graph', self)
        self.runWindow = RunWindow(self.graphWidget)
        self.runWindow.setGraph(self.graph)
        self.dwRunWindow.setAllowedAreas(Qt.LeftDockWidgetArea)
        self.dwRunWindow.setWidget(self.runWindow)
        self.addDockWidget(Qt.LeftDockWidgetArea, self.dwRunWindow)

        self.dwSignalWindow = QDockWidget('Edge signals', self)
        self.signalWindow = SignalTable()
        self._updateSignalWindow()
        self.dwSignalWindow.setAllowedAreas(Qt.BottomDockWidgetArea)
        self.dwSignalWindow.setWidget(self.signalWindow)
        self.addDockWidget(Qt.BottomDockWidgetArea, self.dwSignalWindow)
        self.runWindow.setSignalWidget(self.signalWindow)

        self.dwLogWindow = QDockWidget('Log', self)
        self.logWindow = LogWidget()
        self.dwLogWindow.setAllowedAreas(Qt.BottomDockWidgetArea)
        self.dwLogWindow.setWidget(self.logWindow)
        self.addDockWidget(Qt.BottomDockWidgetArea, self.dwLogWindow)

        self.tabifyDockWidget(self.dwLogWindow, self.dwSignalWindow)

        self.runWindow.setGraphWidget(self.graphWidget)
        self.setCentralWidget(self.graphWidget)

        self.setWindowTitle('SDFkit')
        self.setGeometry(0, 30, 1300, 750)
        self.show()

    def _updateSignalWindow(self):

        if self.graph is not None:
            self.signalWindow.clearSignals()

            # put all node activity and edge data signals in aphabetical order in the signal widget
            nodes = sorted(self.graph.nodes())
            for n in nodes:
                data = self.graph.nodefirings[n]
                self.signalWindow.addSignal(n, SignalLogWidget.NODE_ACTIVE_SIGNAL, data)
                successors = sorted(self.graph.successors(n))
                for s in successors:
                    data = self.graph.edgestates[(n, s)]
                    name =  n + ' → ' + s
                    self.signalWindow.addSignal(name, SignalLogWidget.EDGE_DATA_SIGNAL, data)

    def openActionTriggered(self):

        graphfile, _ = QFileDialog.getOpenFileName(
            self, 'Open graph', './examples')

        if graphfile != '':
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
            self._updateSignalWindow()

    def saveActionTriggered(self):
        self.graph.storeToFile()

    def clashcodegenActionTriggered(self):
        try:
            ClashCodeGen.generateCode(self.graph, './output/' + self.graph.name)
            QMessageBox.information(self, 'CLaSH code generation', 'CLaSH code generation completed')
        except Exception as e:
            QMessageBox.critical(
                self, 'Generate CLaSH code',
                '<b>Error generating CLaSH code:</b> ' + str(e))

if __name__ == '__main__':

    app = QApplication(sys.argv)
    app.setWindowIcon(QIcon('images/logo.png'))
    ex = MainWindow()
    app.exec_()
    app.deleteLater()
    sys.exit()
