from shapes.Polygon import Polygon
from shapes.Rectangle import Rectangle

class Square(Rectangle):
    def __init__(self):
        super(Square, self).__init__()
        print("Square object")
