package App;

import java.util.function.Function;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;

class Main
{
  public static void main(final String[] args)
  {
    // Function<T, R> - takes T, returns R
    Function<Integer, Integer> square = x -> x * x;
    System.out.println(square.apply(5));  // Output: 25
    
    // Consumer<T> - takes T, returns nothing
    Consumer<String> greeter = name -> System.out.println("Hello, " + name + "!");
    greeter.accept("Alice");  // Output: Hello, Alice!
    
    // Predicate<T> - takes T, returns boolean
    Predicate<Integer> isEven = n -> n % 2 == 0;
    System.out.println(isEven.test(4));  // Output: true
    
    // Supplier<T> - takes nothing, returns T
    Supplier<Double> randomSupplier = () -> Math.random();
    System.out.println(randomSupplier.get());  // Output: random double
  }
}
