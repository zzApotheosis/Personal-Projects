#include <iostream>
using namespace std;

int main() {
    // Original variable
    int original = 10;
    
    // Create two references to the same variable
    int & ref1 = original;
    int & ref2 = original;
    
    // Print initial values
    cout << "Original: " << original << endl;
    cout << "Ref1: " << ref1 << endl;
    cout << "Ref2: " << ref2 << endl;
    
    // Modify through ref1
    ref1 = 20;
    
    // Print values after modification through ref1
    cout << "\nAfter modifying ref1:" << endl;
    cout << "Original: " << original << endl;
    cout << "Ref1: " << ref1 << endl;
    cout << "Ref2: " << ref2 << endl;
    
    // Modify through ref2
    ref2 = 30;
    
    // Print values after modification through ref2
    cout << "\nAfter modifying ref2:" << endl;
    cout << "Original: " << original << endl;
    cout << "Ref1: " << ref1 << endl;
    cout << "Ref2: " << ref2 << endl;
}
