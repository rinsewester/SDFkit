#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Nicely styled tabbar for separating design simulation and code generation.

author: Rinse Wester

"""

import sys
from PyQt5.QtWidgets import QWidget, QApplication, QVBoxLayout, QHBoxLayout, QLabel, QPushButton
from PyQt5.QtGui import QIcon

class MainTabBar(QWidget):

    def __init__(self, designWidget, simulateWidget, gencodeWidget, logWidget):
        super().__init__()

        self.wDesign = designWidget
        self.wSimulate = simulateWidget
        self.wGenCode = gencodeWidget
        self.wLog = logWidget

        self.wTabsWidget = QWidget(self)
        self.wTabsWidget.setObjectName('wTabsWidget')
        self.wTabsWidget.setFixedWidth(200)

        self.pbDesign = QPushButton('Design', self.wTabsWidget)
        self.pbDesign.setCheckable(True)
        self.pbDesign.setIcon(QIcon('images/design.png'))
        self.pbDesign.clicked.connect(self._buttonClicked)

        self.pbSimulate = QPushButton('Simulate', self.wTabsWidget)
        self.pbSimulate.setCheckable(True)
        self.pbSimulate.setIcon(QIcon('images/simulate.png'))
        self.pbSimulate.clicked.connect(self._buttonClicked)

        self.pbGenCode = QPushButton('Generate code', self.wTabsWidget)
        self.pbGenCode.setCheckable(True)
        self.pbGenCode.setIcon(QIcon('images/codegen.png'))
        self.pbGenCode.clicked.connect(self._buttonClicked)

        self.pbLog = QPushButton('Log', self.wTabsWidget)
        self.pbLog.setObjectName('logbtn')
        self.pbLog.setCheckable(True)
        self.pbLog.setIcon(QIcon('images/log.png'))
        self.pbLog.clicked.connect(self._buttonClicked)

        self.vblayout = QVBoxLayout()
        self.vblayout.setContentsMargins(0, 0, 0, 0)
        self.vblayout.setSpacing(0)
        self.vblayout.addWidget(self.pbDesign)
        self.vblayout.addWidget(self.pbSimulate)
        self.vblayout.addWidget(self.pbGenCode)
        self.vblayout.addStretch()
        self.vblayout.addWidget(self.pbLog)
        
        self.wTabsWidget.setLayout(self.vblayout)

        self.hblayout = QHBoxLayout()
        self.hblayout.setContentsMargins(0, 0, 0, 0)
        self.hblayout.setSpacing(0)
        self.hblayout.addWidget(self.wTabsWidget)
        self.hblayout.addWidget(self.wDesign)
        self.hblayout.addWidget(self.wSimulate)
        self.hblayout.addWidget(self.wGenCode)
        self.hblayout.addWidget(self.wLog)
        self.setLayout(self.hblayout)

        self.setStyleSheet("""
            .QWidget#wTabsWidget {
                background-color: rgb(50, 50, 54);
                border: none;
            }

            .QPushButton {
                color: rgb(194, 194, 195);
                text-align: left;
                padding-left: 12px;
                padding-top: 10px;
                padding-bottom: 10px;
                padding-right: 10px;
                background-color: rgb(50, 50, 54);
                border-top: none;
                border-bottom: none;
                border-right: none;
                border-left: 4px solid rgb(50, 50, 54);
                qproperty-iconSize: 24px;
            }

            .QPushButton:hover{
                background-color: rgb(74, 73, 80);
                border-left: 4px solid rgb(194, 194, 195);
            }

            .QPushButton:checked {
                background-color: rgb(74, 73, 80);
                border-left: 4px solid rgb(194, 194, 195);
            }
        """)

        self.swithToTab('design')

        self.setGeometry(200, 200, 600, 400)

    def setLogCount(self, count):
        if count == 0:
            self.pbLog.setText('Log')
        else:
            self.pbLog.setText('Log (' + str(count) + ')')

    def setLogState(self, state):
        if state == 'error':
            self.pbLog.setStyleSheet('border-right: 3px solid darkred;')
        elif state == 'warning':
            self.pbLog.setStyleSheet('border-right: 3px solid orange;')
        elif state == 'success':
            self.pbLog.setStyleSheet('border-right: 3px solid darkgreen;')
        else:
            # state = 'none'
            self.pbLog.setStyleSheet('border-right: none;')

    def swithToTab(self, tab):
        if tab == 'design':
            self.pbDesign.setChecked(True)
            self.pbSimulate.setChecked(False)
            self.pbGenCode.setChecked(False)
            self.pbLog.setChecked(False)
            self.wDesign.show()
            self.wSimulate.hide()
            self.wGenCode.hide()
            self.wLog.hide()
        elif tab == 'simulate':
            self.pbDesign.setChecked(False)
            self.pbSimulate.setChecked(True)
            self.pbGenCode.setChecked(False)
            self.pbLog.setChecked(False)
            self.wDesign.hide()
            self.wSimulate.show()
            self.wGenCode.hide()
            self.wLog.hide()
        elif tab == 'gencode':
            self.pbDesign.setChecked(False)
            self.pbSimulate.setChecked(False)
            self.pbGenCode.setChecked(True)
            self.pbLog.setChecked(False)
            self.wDesign.hide()
            self.wSimulate.hide()
            self.wGenCode.show()
            self.wLog.hide()
        else:
            # tab = 'log'
            self.pbDesign.setChecked(False)
            self.pbSimulate.setChecked(False)
            self.pbGenCode.setChecked(False)
            self.pbLog.setChecked(True)
            self.wDesign.hide()
            self.wSimulate.hide()
            self.wGenCode.hide()
            self.wLog.show()

    def _buttonClicked(self):

        if self.sender() == self.pbDesign:
            self.swithToTab('design')
        elif self.sender() == self.pbSimulate:
            self.swithToTab('simulate')
        elif self.sender() == self.pbGenCode:
            self.swithToTab('gencode')
        else:
            # sender = pbLog
            self.swithToTab('log')

if __name__ == '__main__':

    app = QApplication(sys.argv)

    # Create some labels as dummy widgets
    lblA = QLabel('First')
    lblB = QLabel('Second')
    lblC = QLabel('Third')
    lblD = QLabel('Fourth')
    
    tb = MainTabBar( lblA, lblB, lblC, lblD)
    tb.show()
    app.exec_()
    sys.exit()
