# QEMU/KVM CLI

QEMU/KVM is a set of highly versatile technologies for creating and managing virtualized environments known as domains. Even more impressive is that QEMU/KVM is designed to be used and accessed with no hard dependency on a graphical interface; it is perfectly capable of creating and managing domains from a command-line interface (CLI). This document is a small collection of notes to illustrate several different useful commands that can be used to create new domains and/or manage existing ones.

` $ sudo virsh pool-list --all`  
` $ sudo virsh vol-list <pool>`  
` $ sudo virsh vol-create-as --format qcow2 <pool> <volume name> 100GiB`  
` $ sudo virt-install --name archlinux-test --description "Arch Linux Test" --os-type Linux --os-variant archlinux --ram 1024 --vcpus 2 --disk path=<path to QCOW file> --graphics none --network default --cdrom <path to ISO9660 image> --machine q35 --boot uefi,loader_secure=no`  
` $ sudo virsh destroy <domain>`  
` $ sudo virsh undefine --nvram <domain>`
