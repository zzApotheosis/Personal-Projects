# POSIX Message Queue Implementation
This sample code was generously provided by a user on Stack Overflow:
https://stackoverflow.com/questions/3056307/how-do-i-use-mqueue-in-a-c-program-on-a-linux-based-system

One thing I have learned is that neither the C standard nor the C++ standard provide ANY means of inter-process communication (IPC). Instead, IPC is provided by the operating system itself. It is vastly different on Linux, BSD, macOS, and Windows, more so between *nix systems and Windows systems. The POSIX standard provides a well-defined set of IPC schemes for POSIX-compliant kernels and operating systems to use. This is why this implementation of message queues works; it implements the POSIX-defined <mqueue.h> header to achieve a basic implementation of IPC using message queues.
