#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display log messages.

author: Rinse Wester

"""

import sys
from PyQt5.QtWidgets import QWidget, QApplication, QVBoxLayout, QHBoxLayout, QPushButton, QListWidget
from log import Log
import random as rdm 

class LogWidget(QWidget):

    def __init__(self):
        super().__init__()

        self.initUI()

        self.show()

    def initUI(self):

        self.btnclear = QPushButton("Clear messages", self)
        self.btnclear.clicked.connect(self._clearClicked)

        self.btnAddM = QPushButton("Add message", self)
        self.btnAddM.clicked.connect(self._addMClicked)
        
        self.lwmessages = QListWidget(self)
        for _, msgtype, msgtext in Log.getLogMessages():
            self.addLogItem(tp, msg)

        self.hboxlayout = QHBoxLayout()
        self.hboxlayout.addWidget(self.btnclear)
        self.hboxlayout.addStretch()
        self.hboxlayout.addWidget(self.btnAddM)

        self.vboxlayout = QVBoxLayout()
        self.vboxlayout.addLayout(self.hboxlayout)
        self.vboxlayout.addWidget(self.lwmessages)

        self.setLayout(self.vboxlayout)

        # Make sure this widget listens to new logevents
        Log.setNewMessageCallBack(self._newMessageAddedToLog)

    def _clearClicked(self):
        self.lwmessages.clear()
        Log.clearMessages()

    def _addMClicked(self):
        msgtype = rdm.choice([Log.INFO, Log.WARNING, Log.ERROR])
        msgtext = 'message ... ' + rdm.choice('ABCDEFGHI')
        Log.addLogMessage(msgtype, msgtext)

    def _newMessageAddedToLog(self, msgtype, msgtext):
        self.addLogItem(msgtype, msgtext)

    def addLogItem(self, msgtype, msgtext):
        self.lwmessages.addItem(str(msgtype) + ' > ' + msgtext)


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = LogWidget()
    app.exec_()
    app.deleteLater()
    sys.exit()
