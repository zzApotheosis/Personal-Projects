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
  }

  /*
   * The basic idea here is that you should be able to pass in any object
   * that implements the interface even if the object is instantiated from different
   * classes.
   */
  private static void otherFunction(final Iface iface)
  {
    iface.do_work(69);
  }
}

abstract class BaseClass
{
  /*
   * Technically since the amountWorked is in the same package, the `protected` keyword
   * is unneeded. If these classes were defined (and referenced) from different packages,
   * we would need the `protected` keyword to "open" the visibility of this attribute.
   */
  protected int amountWorked = 0;
  //int amountWorked = 0; // This would work too
}

class ClassA extends BaseClass implements Iface
{
  public void do_work(final int amount)
  {
    this.amountWorked += amount;
    System.err.println("ClassA object just did work! Amount worked: " + String.valueOf(this.amountWorked));
  }
}

class ClassB extends BaseClass implements Iface
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
  
  public void do_work(final int amount)
  {
    this.amountWorked += (amount / this.workDivisor);
    System.err.println("ClassB is barely working tbh... Amount worked: " + String.valueOf(this.amountWorked));
  }
}

abstract interface Iface
{
  abstract public void do_work(final int amount);
}
