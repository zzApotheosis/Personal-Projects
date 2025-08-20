# Endianness Example

This is example C code that shows whether the current system is little-endian or big-endian.

You may notice that on modern Linux systems, Linux memory structures are little-endian. This means that the least significant bytes are ordered first and the most significant bytes are ordered last.

Endianness is a big reason why code compiled on a little-endian system will not work with machines that expect memory spaces to be big-endian.
