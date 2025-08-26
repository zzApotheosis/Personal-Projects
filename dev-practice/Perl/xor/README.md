# XOR Data Splitting

This is an example utility that can cryptographically "split" arbitrary data into different shares. When all shares are combined, they recreate the original plaintext.

Try it with this README!

```
#!/bin/sh
./xor.pl split -c 3 README.md
file README.md.*
./xor.pl join -o derived.md README.md.*
cat derived.md
./xor.pl --help
```
