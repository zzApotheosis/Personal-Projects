# Libvirt

The [virtualization API](https://libvirt.org/).

This is a simple collection of personal notes on how to perform various tasks with libvirt from CLI.
I prefer the CLI approach because I enjoy working in headless (terminal-only) environments over
graphical environments.

## Creating a QCOW2 disk file

`qemu-img create -f qcow2 my-image.qcow2 100G`

## Creating a domain with a given QCOW2 disk file

```
virt-install \
    --name my-domain \
    --ram 8192 \
    --vcpus 8 \
    --disk path=/path/to/my-image.qcow2 \
    --os-variant rhel9-unknown \
    --virt-type kvm \
    --graphics none \
    --boot uefi \
    --location /tmp/rhel-9.7-x86_64-dvd.iso \
    --extra-args "console=ttyS0"
```

The valid arguments for `--os-variant` can be found using the command `osinfo-query os`.

```
virt-install \
    --name f34-uefi \
    --network network=management \
    --ram 2048 \
    --disk size=20 \
    --boot uefi \
    --location https://dl.fedoraproject.org/pub/fedora/linux/releases/34/Server/x86_64/os/ \
    --dry-run \
    --print-xml > vm.xml
```
