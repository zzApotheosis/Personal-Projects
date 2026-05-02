# DTLS-PSK with OpenSSL

Generate the Pre-Shared Key:

> openssl rand -hex 32 > psk.key
> cat psk.key

In one terminal, run the server:

> openssl s_server -nocert -psk $(cat psk.key) -psk_identity "Server Identity" -dtls1_2 -accept 4433

In another terminal, run the client:

> openssl c_client -psk $(cat psk.key) -psk_identity "Client Identity" -dtls1_2 -connect 127.0.0.1:4433

Weep and be amazed.

