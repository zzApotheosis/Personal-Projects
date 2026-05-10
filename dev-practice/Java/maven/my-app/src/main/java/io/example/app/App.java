package io.example.app;

/**
 * Hello world!
 */
public class App {
  static java.io.PrintStream outputStream;

  public static void main(final String[] args) {
    App.outputStream = System.err;
    App.outputStream.println("Hello World!");
    App.dumpArgs(args);
  }

  private static void dumpArgs(final String[] args) {
    for (final String arg : args)
      App.outputStream.println(arg);
  }
}
