#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display simulation data of a CSDF graph.

author: Rinse Wester

"""

import sys
from PyQt5.QtWidgets import QWidget, QApplication, QVBoxLayout, QHBoxLayout, QPushButton, QLabel, QComboBox, QLineEdit
from log import Log

class RunWindow(QWidget):

    def __init__(self, graphWidget):
        super().__init__()

        self.graphWidget = graphWidget

        self.initUI()

    def initUI(self):

        self.btnReset = QPushButton("Reset graph", self)
        self.btnReset.setEnabled(False)
        self.btnReset.clicked.connect(self.buttonClicked)

        self.lblsingleStep = QLabel('<b>Single step:</b>')

        self.btnBack = QPushButton("Back", self)
        self.btnBack.setEnabled(False)
        self.btnBack.clicked.connect(self.buttonClicked)

        self.lblStateNr = QLabel('State: 0', self)
        self.lblStateNr.setMinimumWidth(85)

        self.btnNext = QPushButton("Step", self)
        self.btnNext.clicked.connect(self.buttonClicked)

        self.hboxSStep = QHBoxLayout()
        self.hboxSStep.addWidget(self.btnBack)
        self.hboxSStep.addWidget(self.lblStateNr)
        self.hboxSStep.addWidget(self.btnNext)

        self.vbox = QVBoxLayout()
        self.vbox.addWidget(self.btnReset)
        self.vbox.addWidget(self.lblsingleStep)
        self.vbox.addLayout(self.hboxSStep)

        self.lblrunTillCond = QLabel('<b>Run untill Condition:</b>')

        self.lbledge = QLabel('Edge:')
        self.cmbEdges = QComboBox()
        self.cmbEdges.addItem('Alpha → Beta')
        self.cmbEdges.addItem('Beta → Gamma')
        self.cmbEdges.addItem('Gamma → Alpha')

        self.lblContains = QLabel('Contains:')

        self.editContains = QLineEdit()
        self.editContains.setText('57')

        self.btnRunUntill = QPushButton('Run until condition')
        self.btnRunUntill.clicked.connect(self.buttonClicked)

        self.vbox.addWidget(self.lblrunTillCond)
        self.vbox.addWidget(self.lbledge)
        self.vbox.addWidget(self.cmbEdges)
        self.vbox.addWidget(self.lblContains)
        self.vbox.addWidget(self.editContains)
        self.vbox.addWidget(self.btnRunUntill)

        self.mainWidget = QWidget()
        self.vbox.addStretch()
        self.setLayout(self.vbox)

        #self.setGraph(G0)

        self.setGeometry(300, 300, 300, 500)
        self.show()

    def buttonClicked(self):

        MAX_ITERATIONS = 128

        if self.sender() == self.btnReset:
            self.graph.reset()
        elif self.sender() == self.btnBack:
            self.graph.back()
        elif self.sender() == self.btnNext:
            self.graph.step()           

        else:
            # btnRunUntill button clicked
            src, dst = self.cmbEdges.currentText().split(' → ')
            n = 0
            while str(self.graph[src][dst]['tkns']) != ('[' + self.editContains.text() + ']') and n < MAX_ITERATIONS:
                self.graph.step()
                self.graphWidget.updateTokensGraph()
                n += 1
            if n == MAX_ITERATIONS:
                Log.addLogMessage(Log.CRITICAL, 'Stop condition never met')


        self.lblStateNr.setText('State: ' + str(self.graph.stateCount() - 1))
        self.btnBack.setEnabled(self.graph.stateCount() > 1)
        self.btnReset.setEnabled(self.graph.stateCount() > 1)

        # Display the new state of the graph in the GraphWidget
        #self.gwid.update()
        self.graphWidget.updateTokensGraph()

        # update the log window such that all tokens of all states are shown
        self.swid.setEdgeData(list(self.graph.edgestates.values()))

    def setGraphWidget(self, gwid):

        self.gwid = gwid

    def setSignalWidget(self, swid):

        self.swid = swid

    def setGraph(self, graph):

        self.graph = graph
        self.cmbEdges.clear()

        if self.graph is not None:
            for src, dst in self.graph.edges():
                self.cmbEdges.addItem(src + ' → ' + dst)

        self.setEnabled(self.graph is not None)


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = RunWindow()
    app.exec_()
    app.deleteLater()
    sys.exit()
