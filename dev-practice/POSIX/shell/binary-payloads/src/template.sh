#!/bin/sh

_payload_line=$(awk '/^__BINARY_PAYLOAD__/ { print NR + 1; exit 0; }' "$0")

echo "Dumping hex bytes..."
tail -n +"${_payload_line}" "$0" | hexdump -C

exit 0

__BINARY_PAYLOAD__
