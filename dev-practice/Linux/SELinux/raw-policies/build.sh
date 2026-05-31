#!/bin/sh
checkmodule -M -m -o my-policy.mod my-policy.te
semodule_package -o my-policy.pp -m my-policy.mod
