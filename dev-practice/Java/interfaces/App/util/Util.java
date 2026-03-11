package App.util;

public class Util
{
  private Util()
  {}
  
  public static void utilFunction(final String string)
  {
    System.out.println(string);
    ClassA classa = new ClassA();
    ClassB classb = new ClassB();
    Util.otherFunction(classa);
    Util.otherFunction(classb);

    Succeedable s = classb;
    s.succeed(0);
    //s.work(10); // Succeedable does not know what work() means, even if s refers to a ClassB object that implements work()
    classb.succeed(1);
  }

  /*
   * The basic idea here is that you should be able to pass in any object
   * that implements the interface even if the object is instantiated from different
   * classes.
   */
  private static void otherFunction(final Workable iface)
  {
    iface.work(69);
  }
}

abstract class BaseClass
{
  /*
   * Technically since the amountWorked is in the same package, the `protected` keyword
   * is unneeded. If these classes were defined (and referenced) from different packages,
   * we would need the `protected` keyword to "open" the visibility of this attribute.
   */
  int amountWorked = 0;
  //protected int amountWorked = 0; // This would work too
}

class ClassA extends BaseClass implements Workable
{
  public void work(final int amount)
  {
    this.amountWorked += amount;
    System.err.println(String.valueOf(this.getClass()) + " just did work! Amount worked: " + String.valueOf(this.amountWorked));
  }
}

class ClassB extends BaseClass implements Workable, Succeedable
{
  private int workDivisor;

  ClassB()
  {
    this(10);
  }

  ClassB(final int workDivisor)
  {
    this.workDivisor = workDivisor;
  }
  
  public void work(final int amount)
  {
    this.amountWorked += (amount / this.workDivisor);
    System.err.println(String.valueOf(this.getClass()) + " is barely working tbh... Amount worked: " + String.valueOf(this.amountWorked));
  }

  public void succeed(final int status)
  {
    System.err.println(String.valueOf(this.getClass()) + " has succeeded! (status=" + String.valueOf(status) + ")");
  }
}

abstract interface Workable
{
  abstract public void work(final int amount);
}

abstract interface Succeedable
{
  abstract public void succeed(final int status);
}
