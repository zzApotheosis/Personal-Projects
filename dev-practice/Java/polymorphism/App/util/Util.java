package App.util;

import java.util.Vector;

public class Util
{
  public static void utilFunction(final String string)
  {
    System.out.println(string);

    Vector<Shape> shapes = new Vector<>();
    shapes.add(new Circle(2.5));
    shapes.add(new Rectangle(5.0, 3.0));
    shapes.add(new Square(4.0));
    shapes.add(new Triangle(3.0, 6.0, 5.0));

    for (final Shape shape : shapes)
      {
        shape.showMe();
        System.out.println();
      }
  }
}

abstract class Shape
{
  private int sideCount;
  private String name;

  Shape()
  {
    this("");
  }

  Shape(final String name)
  {
    this(name, 0);
  }

  Shape(final String name, final int sideCount)
  {
    this.sideCount = sideCount;
    this.name = name;
  }
  
  public abstract double area();
  
  public abstract double perimeter();

  public void showMe()
  {
    System.out.println("Shape: " + this.getName());
    System.out.println("Area: " + String.valueOf(this.area()));
  }

  public int getSideCount()
  {
    return this.sideCount;
  }

  public String getName()
  {
    return this.name;
  }
}

class Circle extends Shape
{
  private double radius;

  Circle()
  {
    this(0.0);
  }

  Circle(final double radius)
  {
    super("Circle");
    this.radius = radius;
  }

  @Override
  public double area()
  {
    return Math.PI * Math.pow(this.radius, 2.0);
  }

  @Override
  public double perimeter()
  {
    return 2 * Math.PI * this.radius;
  }

  @Override
  public void showMe()
  {
    super.showMe();
    System.out.println("Radius: " + String.valueOf(this.radius));
    System.out.println("Circumference: " + String.valueOf(this.circumference()));
  }
  
  public double circumference()
  {
    return this.perimeter();
  }
}

class Rectangle extends Shape
{
  private double width;
  private double height;

  Rectangle()
  {
    this(0.0, 0.0);
  }

  Rectangle(final double width, final double height)
  {
    this("Rectangle", width, height);
  }

  Rectangle(final String name, final double width, final double height)
  {
    super(name, 4);
    this.width = width;
    this.height = height;
  }

  @Override
  public double area()
  {
    return this.width * this.height;
  }

  @Override
  public double perimeter()
  {
    return 2 * this.width + 2 * this.height;
  }

  @Override
  public void showMe()
  {
    super.showMe();
    System.out.println("Width: " + String.valueOf(this.width));
    System.out.println("Height: " + String.valueOf(this.height));
    System.out.println("Perimeter: " + String.valueOf(this.perimeter()));
  }
}

class Square extends Shape
{
  private double side;
  
  /*
   * A square is a rectangle by definition with extra properties.
   *
   * However, we CANNOT extend the Rectangle class because that would violate the Liskov Substitution Principle.
   *
   * All four sides of a square must be equal in length.
   */
  Square()
  {
    this(0.0);
  }

  /*
   * Square can only set one value for its "side" because all 4 sides are expected to be equal in length.
   */
  Square(final double side)
  {
    super("Square", 4);
    this.side = side;
  }

  @Override
  public double area()
  {
    return Math.pow(this.side, 2.0);
  }

  @Override
  public double perimeter()
  {
    return 4 * this.side;
  }

  @Override
  public void showMe()
  {
    super.showMe();
    System.out.println("Side length: " + String.valueOf(this.side));
    System.out.println("Perimeter: " + String.valueOf(this.perimeter()));
  }
}

class Triangle extends Shape
{
  private double base;
  private double legA;
  private double legB;
  
  Triangle()
  {
    this(0.0, 0.0, 0.0);
  }

  Triangle(final double base, final double legA, final double legB)
  {
    super("Triangle", 3);
    this.base = base;
    this.legA = legA;
    this.legB = legB;
  }

  /*
   * Heron's formula
   */
  @Override
  public double area()
  {
    final double p = this.perimeter() / 2.0;
    return Math.sqrt(p *
                     (p - this.base) *
                     (p - this.legA) *
                     (p - this.legB));
  }

  @Override
  public double perimeter()
  {
    return this.base + this.legA + this.legB;
  }

  @Override
  public void showMe()
  {
    super.showMe();
    System.out.println("Base: " + String.valueOf(this.base));
    System.out.println("Leg A: " + String.valueOf(this.legA));
    System.out.println("Leg B: " + String.valueOf(this.legB));
    System.out.println("Perimeter: " + String.valueOf(this.perimeter()));
  }
}
