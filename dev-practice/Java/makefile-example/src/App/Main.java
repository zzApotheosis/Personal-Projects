package App;

class Main
{
  public static void main(final String[] args)
  {
    System.out.println("This is the example application!");
    for (final String arg : args)
      System.out.println(arg);
  }

  private Main() throws Exception
  {
    throw new Exception("Do not instantiate me!!!!");
  }
}
