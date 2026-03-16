# Meson and Podman Example

> $ podman system reset  
> $ podman build --file builder.docker --tag my-c-project-builder --env 'CC=clang'  
> $ podman build --file runtime.docker --tag my-c-project-runtime  
> $ podman image list --all  
> $ podman run --rm localhost/my-c-project-runtime

## Exporting and Importing Images

> $ podman image save localhost/my-c-project-runtime > my-c-project-runtime.tar  
> $ podman system reset  
> $ podman image list --all  
> $ cat my-c-project-runtime.tar | podman image load  
> $ podman image list  
> $ podman run --rm localhost/my-c-project-runtime
