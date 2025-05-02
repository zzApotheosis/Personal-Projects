# Define class
type Animal = object
  name: string
  age: int

# Implement method for class
proc speak(self: Animal, msg: string) =
  echo self.name & " says: " & msg

# Main
proc main() =
  let sparky = Animal(name: "Sparky", age: 10)
  sparky.speak("Ligma")

# Execute
main()
