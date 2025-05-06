# Custom Docker Image Example

Note that the rootfs directory only exists as a representation of a real root filesystem. This example will only work if rootfs is populated with the appropriate FHS-compliant directories and
files. In my prototyping, I set up a real RHEL 7 virtual machine and used the following commands to synchronize all the necessary files and directories to rootfs.

> $ ssh root@[vm-address] 'tar --create --directory / etc root bin sbin lib lib64 opt usr var tmp' | dd status=progress | tar --extract --directory rootfs && mkdir -pv rootfs/home

After creating a copy of a good working filesystem, we can invoke Podman to create our custom base image. Alternatively, we can use Buildah directly, since Podman uses Buildah underneath to build the OCI-compliant image.

> $ [podman/buildah] build -t rhel7-custom-base -f rhel7-custom-base.docker

Then the rest of the project build is straightforward.

> $ [podman/buildah] build -t project-builder -f project-builder.docker  
> $ [podman/buildah] build -t project-runtime -f project-runtime.docker  
> $ podman run --rm localhost/project-runtime:latest one two three

## A note about ENTRYPOINT and CMD

Basically, ENTRYPOINT is the "entry point" to the container. It's the direct command that is executed when running the container. In a "scratch" base image, this defaults to /bin/sh or some kind of
shell equivalent (probably Bash or Zsh). CMD is the set of arguments that is passed to the ENTRYPOINT.
