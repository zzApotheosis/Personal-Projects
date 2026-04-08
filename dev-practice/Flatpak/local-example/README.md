# Flatpak

## History

In the world of F/LOSS, there is a problem. Bob and Alice both agree that running `my-app` is a good thing. It is easy for them to reach consensus about the importance of running `my-app`.

However, what they didn't discuss is the potential problem that their machines might be running different versions of software. These differences might be so great that software compiled on
one of their machines might not work on the other's machine.

Thus we have re-discovered one of the most common problems in the world of software engineering: Dependency Hell.

The world has made a few attempts to solve this problem. One of the earliest attempts to solve this problem was when the Java programming language was first released. One of its biggest selling points was
"Write Once, Run Anywhere" (WORA), promising to solve this exact problem of dependency hell. So long as the running system has a functional Java Virtual Machine (JVM), that system has everything it needs to
run a Java application that might have been written and compiled in a completely different environment.

The only problem is this is a type of vendor lock-in. People generally don't like to be restricted to specific technologies simply because they solve one problem. Choosing Java as a programming language
for a project is great, until some other tool offers to solve another problem that Java can't solve.

This is where engineers in the industry have attempted to solve this problem in other ways.

- **Virtualization**: Emulating an entire hardware/software stack so the application "thinks" it's on a specific machine.
- **Containers**: Packaging the application together with every single file and library that it might need to run, ensuring the runtime environment is identical regardless of the host machine.
- **Cross-platform Runtimes**: Languages like Java or Python use a "virtual machine" or interpreter. Code written for these languages can be translated to native machine instructions.

## Flatpak's Approach

Flatpak takes a similar approach by building applications so that they can be packaged, distributed, and sandboxed, ensuring that any system that supports Flatpak can execute an application that was packaged with Flatpak.

Run and study the `build.sh` script to see how Flatpak builds a source project and exports the package to a single Flatpak bundle file (`*.flatpak`).
