#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Nicely styled tabbar for separating design simulation and code generation.

author: Rinse Wester

"""

import sys
from PyQt5.QtWidgets import QWidget, QApplication, QVBoxLayout, QHBoxLayout, QLabel, QPushButton
from PyQt5.QtGui import QPalette, QColor, QIcon, QPixmap
from PyQt5.QtCore import Qt, QSize

class MainTabBar(QWidget):

    def __init__(self):
        super().__init__()

        self.initUI()

    def initUI(self):
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

        self.lblContents = QLabel('Main contents', self)
        self.lblContents.setStyleSheet("""
            font: bold;
            font-size: 24px;
            color: rgb(50, 50, 54);
            background: solid white;
            qproperty-alignment: AlignCenter;
        """)

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
        self.hblayout.addWidget(self.lblContents)
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
            self.setLogCount(0)
            self.setLogState('error')
        elif tab == 'simulate':
            self.pbDesign.setChecked(False)
            self.pbSimulate.setChecked(True)
            self.pbGenCode.setChecked(False)
            self.pbLog.setChecked(False)
            self.setLogCount(1)
            self.setLogState('warning')
        elif tab == 'gencode':
            self.pbDesign.setChecked(False)
            self.pbSimulate.setChecked(False)
            self.pbGenCode.setChecked(True)
            self.pbLog.setChecked(False)
            self.setLogCount(2)
            self.setLogState('success')
        else:
            # tab = 'log'
            self.pbDesign.setChecked(False)
            self.pbSimulate.setChecked(False)
            self.pbGenCode.setChecked(False)
            self.pbLog.setChecked(True)
            self.setLogCount(3)
            self.setLogState('none')


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
    tb = MainTabBar()
    tb.show()
    app.exec_()
    sys.exit()
