#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to display log messages.

author: Rinse Wester

"""

import sys
from PyQt5.QtWidgets import QWidget, QApplication, QVBoxLayout, QHBoxLayout, QPushButton, QListWidget, QListWidgetItem
from PyQt5.QtGui import QIcon
from log import Log

class LogWidget(QWidget):

    def __init__(self):
        super().__init__()

        self.initUI()

        self.show()

    def initUI(self):

        self.btnclear = QPushButton("Clear messages", self)
        self.btnclear.clicked.connect(self._clearClicked)
        
        self.lwmessages = QListWidget(self)
        for _, msgtype, msgtext in Log.getLogMessages():
            self.addLogItem(msgtype, msgtext)

        self.hboxlayout = QHBoxLayout()
        self.hboxlayout.addWidget(self.btnclear)
        self.hboxlayout.addStretch()

        self.vboxlayout = QVBoxLayout()
        self.vboxlayout.addLayout(self.hboxlayout)
        self.vboxlayout.addWidget(self.lwmessages)

        self.setLayout(self.vboxlayout)

        # Make sure this widget listens to new logevents
        Log.setNewMessageCallBack(self._newMessageAddedToLog)

    def _clearClicked(self):
        self.lwmessages.clear()
        Log.clearMessages()

    def _newMessageAddedToLog(self, msgtype, msgtext):
        self.addLogItem(msgtype, msgtext)

    def addLogItem(self, msgtype, msgtext):
        if msgtype == Log.INFO:
            icon = QIcon('images/information.png')
        elif msgtype == Log.WARNING:
            icon = QIcon('images/warning.png')
        else:
            icon = QIcon('images/error.png')
        item = QListWidgetItem(icon, msgtext)
        self.lwmessages.addItem(item)


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = LogWidget()
    app.exec_()
    app.deleteLater()
    sys.exit()
