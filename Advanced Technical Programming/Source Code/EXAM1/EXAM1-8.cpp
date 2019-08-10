// Created by Steven Jennings
// Date: 29 June 2016
//
// Write a program that prompts the user for a positive integer and then determines
// whether the number is semiprime or not. If it is a semiprime the program should print its factors.
// Note: A semiprime is a number formed by the product of exactly two (not necessarily distinct) primes.
// For example: 21 is a semiprime since 3 * 7 = 21. Semiprimes are used by several common encryption
// algorithms as keys. If the number is not a semiprime the program should display a message
// to that effect.
//
// Side Note: My Java experience is certainly helping me out!
//
// Sorry about the messy code... If it's too hard to comprehend, you can find it on my GitHub repo as per usual.
// http://www.github.com/zzApotheosis

#include <iostream>

using namespace std;

void getFactors1(int, int);

void getFactors2(int);

void isSemiPrime1();

void isSemiPrime2();

int primes[2];
int primeConfirmed = 0;

int main() {
    printf("This program will determine whether or not a given number is semiprime.\n");

    int input;
    int counter = 0;

    printf("Enter any integer 0 or greater.\n");
    scanf("%d", &input);

    //Input integer must be 0 or greater.
    if (input < 0) {
        printf("Input value must be 0 or greater. Setting input value to 0.\n");
        input = 0;
    }

    //Determining number of factors in the user input.
    for (int i = 1; i <= input; ++i) {
        if (input % i == 0) {
            ++counter;
        }
    }

    //This giant if statement is the meat of the code. This giant segment of code will determine if the input is a semiprime.
    //This program doubles as a prime detector and composite detector as well.
    if (input == 0 || input == 1) {
        printf("Your number (%d) is neither prime nor composite, and therefore not semiprime.", input);
    } else if (counter == 2) {
        printf("Your number (%d) is prime, but not semiprime.\n", input);
    } else if (counter == 3) {
        getFactors1(input, counter);
        isSemiPrime1();
        if (primeConfirmed == 1) {
            printf("Your number (%d) is semiprime because its only prime factor %d may be squared to get %d.\n", input,
                   primes[0], input);
        } else {
            printf("Your number (%d) contains an odd number of prime factors, but is not semiprime. %d is composite.\n",
                   input, input);
        }
    } else if (counter == 4) {
        getFactors2(input);
        isSemiPrime2();
        if (primeConfirmed == 2) {
            printf("Your number (%d) is semiprime because its factors %d and %d are prime numbers.\n", input, primes[0],
                   primes[1]);
        } else if (primeConfirmed == 1) {
            printf("Your number (%d) has exactly four factors but is not semiprime because either %d or %d is not prime.\n",
                   input, primes[0], primes[1]);
        } else {
            printf("Your number (%d) has exactly four factors but is not semiprime because neither %d nor %d is prime.\n",
                   input, primes[0], primes[1]);
        }
    } else {
        printf("Your number (%d) is composite.\n", input);
    }

    getchar();
    getchar();
}

void getFactors1(int in, int counter) {
    int middle = counter / 2;
    int ctr = 0;
    while (ctr < middle) {
        for (int i = 2; i < in; ++i) {
            if (in % i == 0) {
                ++ctr;
            }
            if (ctr >= middle) {
                primes[0] = i;
                break;
            }
        }
    }
}

void getFactors2(int in) {
    int index = 0;
    for (int i = 2; i < in; ++i) {
        if (in % i == 0) {
            primes[index] = i;
            ++index;
        }
    }
}

void isSemiPrime1() {
    int ctr = 0;
    for (int i = 1; i <= primes[0]; ++i) {
        if (primes[0] % i == 0) {
            ++ctr;
        }
    }

    if (ctr == 2) {
        ++primeConfirmed;
    }
}

void isSemiPrime2() {
    //Determining if the first factor in the input is a prime.
    int ctr = 0;
    for (int i = 1; i <= primes[0]; ++i) {
        if (primes[0] % i == 0) {
            ++ctr;
        }
    }
    if (ctr == 2) {
        ++primeConfirmed;
    }

    //Determining if the second factor in the input is a prime.
    ctr = 0;
    for (int i = 1; i <= primes[1]; ++i) {
        if (primes[1] % i == 0) {
            ++ctr;
        }
    }
    if (ctr == 2) {
        ++primeConfirmed;
    }
}