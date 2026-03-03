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
  private static void otherFunction(Iface iface)
  {
    iface.do_work();
  }
}

class ClassA implements Iface
{
  ClassA()
  {}

  public void do_work()
  {
    System.err.println("ClassA object just did work!");
  }
}

class ClassB implements Iface
{
  ClassB()
  {}

  public void do_work()
  {
    System.err.println("ClassB is barely working tbh...");
  }
}

abstract interface Iface
{
  public void do_work();
}
