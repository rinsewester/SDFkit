#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Widget to visualize a CSDF graph

author: Rinse Wester

"""

import sys
import math
from PyQt5.QtWidgets import (
    QWidget, QApplication, QVBoxLayout, QHBoxLayout, QPushButton,
    QLabel, QScrollArea, QMenu, QAction, QInputDialog)
from PyQt5.QtCore import (Qt, QRect, QPoint, pyqtSignal, QObject)
from PyQt5.QtGui import (
    QPainter, QFont, QColor, QPen, QBrush, QPainterPath, QPolygon,
    QMouseEvent, QCursor, QContextMenuEvent)

from csdfgraph import *


class GraphWidget(QWidget):

    NODE_RADIUS = 20
    EDGE_DIST = 26
    EDGE_HEAD_LEN = 32

    node_hovered = None
    edge_hovered = None

    node_right_clicked = None
    edge_right_clicked = None

    tokens_pos = {}

    def __init__(self):
        super().__init__()

        self.setAutoFillBackground(True)
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.white)
        self.setPalette(p)

        self.setMouseTracking(True)

        self.ednodefuncAction = QAction('Edit node function', self)
        self.ednodefuncAction.triggered.connect(self.ednodefuncActionTriggered)

        self.ededgetokensAction = QAction('Edit tokens', self)
        self.ededgetokensAction.triggered.connect(self.ededgetokensActionTriggered)
        self.edpratessAction = QAction('Edit production rates', self)
        self.edpratessAction.triggered.connect(self.edpratessActionTriggered)
        self.edcratessAction = QAction('Edit consumption rates', self)
        self.edcratessAction.triggered.connect(self.edcratessActionTriggered)

        self.nodemenu = QMenu(self)
        self.nodemenu.addAction(self.ednodefuncAction)

        self.edgemenu = QMenu(self)
        self.edgemenu.addAction(self.ededgetokensAction)
        self.edgemenu.addAction(self.edpratessAction)
        self.edgemenu.addAction(self.edcratessAction)
        

    def _rotate(pos, theta):

        x, y = pos
        xn = x * math.cos(theta) - y * math.sin(theta)
        yn = x * math.sin(theta) + y * math.cos(theta)
        return (xn, yn)

    def _inSquare(pm, p1, p2):

        theta = GraphWidget._point2angle(p1, p2)
        pmx, pmy = pm
        p1x, p1y = p1
        p2x, p2y = p2

        # translate to origin
        pmx, pmy = pmx - p1x, pmy - p1y
        p2x, p2y = p2x - p1x, p2y - p1y

        # rotate into flat block
        pmx, pmy = GraphWidget._rotate((pmx, pmy), -theta)
        p2x, p2y = GraphWidget._rotate((p2x, p2y), -theta)

        return (0.0 < pmx < p2x) and (-16.0 < pmy < 16.0)

    def _aboveNode(self, mousePos, nodeName):

        mx, my = mousePos
        nx, ny = self.graph.node[nodeName]['pos']
        inXRange = (nx - self.NODE_RADIUS) < mx < (nx + self.NODE_RADIUS)
        inYRange = (ny - self.NODE_RADIUS) < my < (ny + self.NODE_RADIUS)
        return inXRange and inYRange

    def _aboveEdge(self, mousePos, edge):

        p1, p2 = self.tokens_pos[edge]
        return GraphWidget._inSquare(mousePos, p1, p2)

    def mouseMoveEvent(self, event):

        mpos = event.x(), event.y()
        self.update()

        self.node_hovered = None
        self.edge_hovered = None

        if self.graph is not None:
            for n in self.graph.nodes():
                if self._aboveNode(mpos, n):
                    self.node_hovered = n
                    break

            for e in self.graph.edges():
                if self._aboveEdge(mpos, e):
                    self.edge_hovered = e
                    break

        # Give indication of link with nodes and edges
        if self.node_hovered is not None or self.edge_hovered is not None:
            self.setCursor(Qt.PointingHandCursor)
        else:
            self.setCursor(Qt.ArrowCursor)

    def mouseReleaseEvent(self, event):

        if event.button() == Qt.LeftButton:
            if self.node_hovered is not None:
                print('Node clicked: ', self.node_hovered)

            if self.edge_hovered is not None:
                print('Edge clicked: ', self.edge_hovered)

    def contextMenuEvent(self, event):

        if self.node_hovered is not None:
            self.node_right_clicked = self.node_hovered
            self.nodemenu.exec(event.globalPos())
        if self.edge_hovered is not None:
            self.edge_right_clicked = self.edge_hovered
            self.edgemenu.exec(event.globalPos())

    def ednodefuncActionTriggered(self):

        node = self.node_right_clicked
        codestr = self.graph.node[node]['funcstr']
        newcode, ok = QInputDialog.getText(self, 'Code for ' + node, 'Python code:', text=codestr)
        if ok:
            # TODO add validation of code
            self.graph.updateNodeFunction(node, newcode)


    def ededgetokensActionTriggered(self):

        src, dst = self.edge_right_clicked
        tokensstr = str(self.graph[src][dst]['tkns'])
        newtokensstr, ok = QInputDialog.getText(self, 'Edit tokens', 'Tokens:', text=tokensstr)
        if ok:
            # TODO add validation of token data
            self.graph.updateTokens((src, dst), newtokensstr)
            self.update()
        

    def edpratessActionTriggered(self):

        src, dst = self.edge_right_clicked
        pratesstr = str(self.graph[src][dst]['prates'])
        newpratesstr, ok = QInputDialog.getText(self, 'Edit production rates', 'Production rates:', text=pratesstr)
        if ok:
            # TODO add validation
            self.graph.updatePRates((src, dst), newpratesstr)
            self.update()

    def edcratessActionTriggered(self):

        src, dst = self.edge_right_clicked
        cratesstr = str(self.graph[src][dst]['crates'])
        newcratesstr, ok = QInputDialog.getText(self, 'Edit consumption rates', 'Consumption rates:', text=cratesstr)
        if ok:
            # TODO add validation
            self.graph.updateCRates((src, dst), newcratesstr)
            self.update()


    def setGraph(self, graph):
        self.graph = graph

        # set widget size based on min/max positions of nodes
        if not self.graph is None:
            minX, minY = sys.maxsize, sys.maxsize
            maxX, maxY = 0, 0

            for n in self.graph.nodes():
                x, y = self.graph.node[n]['pos']
                minX = min(minX, x)
                minY = min(minY, y)
                maxX = max(maxX, x)
                maxY = max(maxY, y)

            self.setMinimumWidth(maxX + 128)
            self.setMinimumHeight(maxY + 128)
        else:
            self.setMinimumWidth(128)
            self.setMinimumHeight(128)

        self.update()

    # Determine a point inbetween two points according to ratio
    def _relPoints(p1, p2, rat):
        x1, y1 = p1
        x2, y2 = p2
        xn = x1 + rat * (x2 - x1)
        yn = y1 + rat * (y2 - y1)
        return (xn, yn)

    def _distance(p1, p2):
        x1, y1 = p1
        x2, y2 = p2
        return math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)

    def _point2angle(p1, p2):
        x1, y1 = p1
        x2, y2 = p2
        h = math.hypot(x2 - x1, y2 - y1)
        ang = math.acos((x2 - x1) / h)
        if (y2 - y1) < 0:
            ang = math.pi + (math.pi - ang)
        return ang

    def _angledist2point(p1, ang, dist):
        x, y = p1
        a = dist * math.cos(ang)
        o = dist * math.sin(ang)
        return x + a, y + o

    def _drawNode(self, qp, name, pos, hovering=False):

        # Draw the nodes
        if hovering:
            qp.setPen(QPen(QBrush(Qt.gray), 2))
            qp.setBrush(QColor(220, 220, 255))
        else:
            qp.setPen(QPen(QBrush(Qt.black), 2))
            qp.setBrush(QColor(196, 196, 255))
        x, y = pos
        qp.drawEllipse(
            x - GraphWidget.NODE_RADIUS, y - GraphWidget.NODE_RADIUS,
            2 * GraphWidget.NODE_RADIUS, 2 * GraphWidget.NODE_RADIUS)
        rect = QRect(
            x - GraphWidget.NODE_RADIUS, y - GraphWidget.NODE_RADIUS,
            2 * GraphWidget.NODE_RADIUS, 2 * GraphWidget.NODE_RADIUS)
        qp.setPen(QPen(QBrush(Qt.black), 2))
        qp.drawText(rect, Qt.AlignCenter, name)

    def _draw_edge_attributes(self, qp, pa, pb, pc, pd, prates, crates, tkns, edge):

        src, dst = edge

        # Draw production and consumption rates
        qp.setPen(Qt.black)

        PRPx, PRPy = GraphWidget._relPoints(pa, pb, 0.6)
        prrect = QRect(
            PRPx - 50, PRPy - GraphWidget.NODE_RADIUS,
            100, 2 * GraphWidget.NODE_RADIUS)
        phase = self.graph.node[src]['firecount'] % len(prates)
        if len(prates) == 1:
            if prates == [1]:
                pratestr = ''
            else:
                pratestr = str(prates[0])
        elif phase == 0:
            pratestr = str(prates[phase]) + ',..'
        elif phase == len(prates) - 1:
            pratestr = '..,' + str(prates[phase])
        else:
            pratestr = '..,' + str(prates[phase]) + ',..'
        qp.drawText(prrect, Qt.AlignCenter, pratestr)

        PRPx, PRPy = GraphWidget._relPoints(pd, pc, 0.6)
        crrect = QRect(
            PRPx - 50, PRPy - GraphWidget.NODE_RADIUS,
            100, 2 * GraphWidget.NODE_RADIUS)
        phase = self.graph.node[dst]['firecount'] % len(crates)
        if len(crates) == 1:
            if crates == [1]:
                cratestr = ''
            else:
                cratestr = str(crates[0])
        elif phase == 0:
            cratestr = str(crates[phase]) + ',..'
        elif phase == len(crates) - 1:
            cratestr = '..,' + str(crates[phase])
        else:
            cratestr = '..,' + str(crates[phase]) + ',..'
        qp.drawText(crrect, Qt.AlignCenter, cratestr)

        # Draw tokens on the edge
        tknposx = (pa[0] + 3 * pb[0] + 3 * pc[0] + pd[0]) / 8
        tknposy = (pa[1] + 3 * pb[1] + 3 * pc[1] + pd[1]) / 8

        # TODO fix this way to check if edge is highlighted... (graphicsview)
        theta = GraphWidget._point2angle(pa, pd)
        dist = 0.5 * GraphWidget._distance(pb, pc)
        p1 = GraphWidget._angledist2point((tknposx, tknposy), theta, dist)
        p2 = GraphWidget._angledist2point((tknposx, tknposy), theta, -dist)
        self.tokens_pos[edge] = p1, p2

        if len(tkns) > 0:
            qp.setPen(QPen(
                QBrush(Qt.black), 12, Qt.SolidLine, Qt.RoundCap, Qt.RoundJoin))
            rect = QRect(
                tknposx - 64, tknposy, 128, 2 * GraphWidget.NODE_RADIUS)
            # draw different amount of black dots depending on nr of tokens
            if len(tkns) == 1:
                qp.drawPoint(tknposx, tknposy)
                qp.drawText(rect, Qt.AlignCenter, str(tkns[0]))
            elif len(tkns) == 2:
                qp.drawPoint(tknposx - 3, tknposy - 3)
                qp.drawPoint(tknposx + 3, tknposy + 3)
                qp.drawText(rect, Qt.AlignCenter, str(tkns)[1:-1])
            elif len(tkns) == 3:
                qp.drawPoint(tknposx - 3, tknposy - 3)
                qp.drawPoint(tknposx - 3, tknposy + 3)
                qp.drawPoint(tknposx + 3, tknposy - 3)
                qp.drawText(rect, Qt.AlignCenter, str(tkns)[1:-1])
            else:
                qp.drawPoint(tknposx - 3, tknposy - 3)
                qp.drawPoint(tknposx - 3, tknposy + 3)
                qp.drawPoint(tknposx + 3, tknposy - 3)
                qp.drawPoint(tknposx + 3, tknposy + 3)
                qp.drawText(rect, Qt.AlignCenter, str(tkns)[1:-1])

    def _drawSelfEdge(self, qp, n, prates, crates, tkns, angle, edge, hovering=False):

        pos = self.graph.node[n]['pos']
        pa = GraphWidget._angledist2point(
            pos, angle + math.pi / 6, 1.2 * GraphWidget.NODE_RADIUS)
        pb = GraphWidget._angledist2point(
            pos, angle + math.pi / 6, 5 * GraphWidget.NODE_RADIUS)
        pc = GraphWidget._angledist2point(
            pos, angle - math.pi / 6, 5 * GraphWidget.NODE_RADIUS)
        pd = GraphWidget._angledist2point(
            pos, angle - math.pi / 6, 1.2 * GraphWidget.NODE_RADIUS)

        # Draw the bezier line for the edge
        path = QPainterPath()
        path.moveTo(*pa)
        path.cubicTo(pb[0], pb[1], pc[0], pc[1], pd[0], pd[1])
        if hovering:
            qp.setPen(QPen(
                QBrush(Qt.lightGray), 3, Qt.SolidLine,
                Qt.RoundCap, Qt.RoundJoin))
        else:
            qp.setPen(QPen(
                QBrush(Qt.gray), 3, Qt.SolidLine,
                Qt.RoundCap, Qt.RoundJoin))
        qp.setBrush(Qt.NoBrush)
        qp.drawPath(path)

        # Draw the arrow head
        pl = GraphWidget._angledist2point(
            pd, angle - math.pi / 6 - math.pi / 8, 16)
        pr = GraphWidget._angledist2point(
            pd, angle - math.pi / 6 + math.pi / 8, 16)
        points = [QPoint(*pd), QPoint(*pl), QPoint(*pr)]
        if hovering:
            qp.setBrush(QBrush(Qt.lightGray, Qt.SolidPattern))
        else:
            qp.setBrush(QBrush(Qt.gray, Qt.SolidPattern))

        qp.drawPolygon(QPolygon(points))

        # draw rates and tokens on edge
        self._draw_edge_attributes(
            qp, pa, pb, pc, pd, prates, crates, tkns, edge)

    def _drawEdge(self, qp, src, dst, prates, crates, tkns, edge, hovering=False):

        p1 = self.graph.node[src]['pos']
        p2 = self.graph.node[dst]['pos']

        p12angle = GraphWidget._point2angle(p1, p2)

        pa = GraphWidget._angledist2point(
            p1, p12angle + math.pi / 6, 1.2 * GraphWidget.NODE_RADIUS)
        pb = GraphWidget._angledist2point(
            p1, p12angle + math.pi / 6, 4 * GraphWidget.NODE_RADIUS)
        pc = GraphWidget._angledist2point(
            p2, p12angle + math.pi - math.pi / 6, 4 * GraphWidget.NODE_RADIUS)
        pd = GraphWidget._angledist2point(
            p2, p12angle + math.pi - math.pi / 6, 1.2 * GraphWidget.NODE_RADIUS)

        # Draw the bezier line for the edge
        path = QPainterPath()
        path.moveTo(*pa)
        path.cubicTo(pb[0], pb[1], pc[0], pc[1], pd[0], pd[1])
        if hovering:
            qp.setPen(QPen(QBrush(
                Qt.lightGray), 3, Qt.SolidLine, Qt.RoundCap, Qt.RoundJoin))
        else:
            qp.setPen(QPen(QBrush(
                Qt.gray), 3, Qt.SolidLine, Qt.RoundCap, Qt.RoundJoin))
        qp.setBrush(Qt.NoBrush)
        qp.drawPath(path)

        # Draw the arrow head
        pl = GraphWidget._angledist2point(
            pd, p12angle + math.pi - math.pi / 6 - math.pi / 8, 16)
        pr = GraphWidget._angledist2point(
            pd, p12angle + math.pi - math.pi / 6 + math.pi / 8, 16)
        points = [QPoint(*pd), QPoint(*pl), QPoint(*pr)]
        if hovering:
            qp.setBrush(QBrush(Qt.lightGray, Qt.SolidPattern))
        else:
            qp.setBrush(QBrush(Qt.gray, Qt.SolidPattern))
        qp.drawPolygon(QPolygon(points))

        # draw rates and tokens on edge
        self._draw_edge_attributes(
            qp, pa, pb, pc, pd, prates, crates, tkns, edge)

    def paintEvent(self, e):

        if not self.graph is None:
            qp = QPainter()
            qp.begin(self)
            qp.setRenderHint(QPainter.Antialiasing)
            self.drawGraph(qp)
            qp.end()

    def drawGraph(self, qp):

        font = QFont('Serif', 10, QFont.Light)
        qp.setFont(font)

        # Draw the edges
        for src, dst in self.graph.edges():
            prates = self.graph[src][dst]['prates']
            crates = self.graph[src][dst]['crates']
            tkns = self.graph[src][dst]['tkns']

            if src == dst:
                angle = self.graph[src][dst]['angle']
                self._drawSelfEdge(
                    qp, src, prates, crates, tkns, angle,
                    (src, dst), self.edge_hovered == (src, dst))
            else:
                self._drawEdge(
                    qp, src, dst, prates, crates, tkns, (src, dst),
                    self.edge_hovered == (src, dst))

        # Draw the nodes
        for n in self.graph.nodes():
            pos = self.graph.node[n]['pos']
            self._drawNode(qp, n, pos, self.node_hovered == n)
