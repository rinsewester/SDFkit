#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Class for logging SDFkit messages

author: Rinse Wester

"""

import datetime as dt

class Log(object):
	"""Log class to store logmessages from SDFkit"""

	ERROR = 3
	WARNING = 2
	INFO = 1

	_newMessageCallback = None

	_msg_list = []

	def addLogMessage(msgtype, msgtext):
		"""Add a message to the list of log messages

		msgtype is the type of message and must be one of: Log.ERROR, Log.WARNING,
		Log.INFO.

		msgtext contains the actual info message and should be human readable."""
		Log._msg_list.append((dt.datetime.now(), msgtype, msgtext))

		if Log._newMessageCallback is not None:
			Log._newMessageCallback(msgtype, msgtext)

	def setNewMessageCallBack(cbfunc):
		Log._newMessageCallback = cbfunc

	def getLogMessages(mintype=INFO):
		"""Get all recorded logmessages

		When mintype is given, only message with a minimum severity are returned.
		For example when mintype = WARNING, only WARNING and ERROR messages are given.
		Similarly, when mintype = INFO, all messages are returned"""
		return list(filter(lambda tpl: tpl[1] >= mintype, Log._msg_list))

	def messageCount():
		"""returns the number of messages in the log."""
		return len(Log._msg_list)

	def lastMessageType():
		"""returns the type of the last message"""
		if Log.messageCount() > 0:
			return Log._msg_list[-1][1]
		else:
			return None

	def clearMessages():
		"""Remove all logmessages."""
		Log._msg_list.clear()

