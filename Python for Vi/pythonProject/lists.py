# People
people = ["bob", "alice", "frank", "steven"]
scores = [0.40, 0.50, 0.51, 0.37]

# Begin calculating average
total = 0
for i in range(len(scores)):
    print(people[i] + " got a score of " + str(scores[i]))
    total = total + scores[i]

# Calculate the class average
average = total / len(scores)

print("The average score (before curve) is: " + str(average))

while average < 0.65:
    for i in range(len(scores)):
        scores[i] += (1 - scores[i]) * 0.2 # Curve the score by giving each student 20% of their missing grade
        print(people[i] + " has a new score of " + str(scores[i]))
    total = 0
    for i in range(len(scores)):
        total += scores[i]
    average = total / len(scores)

print("The average score (after curve) is: " + str(average))
