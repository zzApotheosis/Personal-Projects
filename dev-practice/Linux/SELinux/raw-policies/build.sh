#!/bin/sh
checkmodule -M -m -o my_policy.mod my_policy.te
semodule_package -o my_policy.pp -m my_policy.mod
