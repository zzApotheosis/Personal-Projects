# People
bob = 0.60
alice = 0.76
frank = 0.37
steven = 0.01

# Variables
num_people = 4

# Begin calculating average
total = bob + alice + frank + steven
average = total / num_people

# Print results
print("Class average is: " + str(average))

# Branch based on class average
if average >= 0.8:
    print("Good job! Let's go out for ice cream!")
else:
    print("You guys suck! Git gud noob")
