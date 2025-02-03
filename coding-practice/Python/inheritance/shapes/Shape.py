class Shape(object):
    def __init__(self):
        super(Shape, self).__init__()
        print("Shape object")
        self.__n_sides = 0

    def shape(self):
        print("I am a shape!")
