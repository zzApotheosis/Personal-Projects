# Instructions for aarch64 cross-compilation

Run: `sudo pacman -S podman`

Put this in `Containerfile`:
```
FROM docker.io/library/debian:stable

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      crossbuild-essential-arm64 \
      gcc-aarch64-linux-gnu \
      g++-aarch64-linux-gnu \
      binutils-aarch64-linux-gnu \
      libc6-dev-arm64-cross \
      qemu-user \
      make \
      cmake \
      ninja-build \
      pkg-config \
      ca-certificates \
      file && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /work
```

Build the image: `podman build -t debian-aarch64-cross .`

Run the image in a new container:
```
podman run --rm -it \
  -v "$PWD:/work" \
  -w /work \
  debian-aarch64-cross
```

Create an example C++ program:
```
cat > hello.cpp << 'EOF'
#include <iostream>

int main() {
    std::cout << "Hello from aarch64!\n";
    return 0;
}
EOF
```

Cross-compile: `aarch64-linux-gnu-g++ -o hello-aarch64 hello.cpp`

Emulate the executable: `qemu-aarch64 -L /usr/aarch64-linux-gnu ./hello-aarch64`

If you prefer not to retain a persistent container image on the local machine, creating
an ephemeral container can be achieved with a podman one-liner:
```
podman run --rm -it \
  -v "$PWD:/work" \
  -w /work \
  docker.io/library/debian:stable \
  bash -c '
    apt-get update &&
    apt-get install -y --no-install-recommends \
      crossbuild-essential-arm64 \
      gcc-aarch64-linux-gnu \
      g++-aarch64-linux-gnu \
      binutils-aarch64-linux-gnu \
      libc6-dev-arm64-cross \
      qemu-user \
      make cmake ninja-build pkg-config ca-certificates file &&
    exec bash
  '
```
