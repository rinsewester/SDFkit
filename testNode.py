import sys
from PyQt5.QtWidgets import QWidget, QApplication
from PyQt5.QtGui import QPainter, QBrush, QColor
from PyQt5.QtCore import Qt


class TestNode(QWidget):
    
    def __init__(self):
        super().__init__()
        
        self.initUI()
        
        
    def initUI(self):      

        self.setGeometry(300, 300, 355, 280)
        self.setWindowTitle('Test Node')
        self.show()
        

    def paintEvent(self, e):

        painter = QPainter()
        painter.begin(self)
        self.drawBrushes(painter)
        painter.end()
        
        
    def drawBrushes(self, painter):

        color = QColor(0, 0, 0)
        painter.setPen(color)
      
        brush = QBrush(QColor(255, 0, 0))
        painter.setBrush(brush)
        painter.drawRoundedRect(50, 50, 100, 60, 10, 10)

        
              
                
        
if __name__ == '__main__':
    
    app = QApplication(sys.argv)
    ex = TestNode()
    sys.exit(app.exec_())