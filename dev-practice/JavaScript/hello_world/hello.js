#!/bin/node
console.log("Hello, world!");

console.log("This goes to stdout");
console.error("This goes to stderr");

process.stdout.write("This goes to stdout too\n");
process.stderr.write("This goes to stderr too\n");
