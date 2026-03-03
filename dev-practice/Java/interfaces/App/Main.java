package App;

import App.util.Util;

class Main
{
  public static void main(final String[] args)
  {
    Util.utilFunction("Utility function!");
  }

  private Main() throws Exception
  {
    throw new Exception("Do not instantiate me!!!!");
  }
}
