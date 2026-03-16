# Meson and Podman Example

> $ podman system reset  
> $ podman build -t my-c-project-builder -f builder.docker --env 'CC=clang'  
> $ podman build -t my-c-project-runtime -f runtime.docker  
> $ podman image list --all  
> $ podman run --rm localhost/my-c-project-runtime

## Exporting and Importing Images

> $ podman image save localhost/my-c-project-runtime > my-c-project-runtime.tar  
> $ podman system reset  
> $ podman image list --all  
> $ cat my-c-project-runtime.tar | podman image load  
> $ podman image list  
> $ podman run --rm localhost/my-c-project-runtime
