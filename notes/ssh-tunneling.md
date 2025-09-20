# SSH Tunneling

Simply amazing, to be honest. God bless FOSS and all the volunteers who made this possible.

Use the `-N` flag in either of these commands to prevent a shell from opening on the SSH server.

Use the `-f` flag in either of these commands to fork the process to the background once the SSH authentication succeeds.

## Forward tunneling

In this command, the `listening port` is the port on the local machine to listen on. The `target IP` and `target port` are the IP address and port to forward traffic to from the remote machine's perspective.

> `$ ssh <user>@<host> -L <listening port>:<target IP>:<target port>`

## Reverse tunneling

In this command, the `listening port` is the port on the remote machine to listen on. The `target IP` and `target port` are the IP address and port to forward traffic to from the local machine's perspective.

> `$ ssh <user>@<host> -R <listening port>:<target IP>:<target port>`
