# Personal-Projects
I've decided to merge all of my personal projects into one repository, as I don't expect them to become large enough to warrant their own independent repositories.

## Using my public GnuPG key
The provided GnuPG key file "zzApotheosis.gpg.base64" is base64-encoded data that must be decoded before importing to your keychain.

To do this on GNU/Linux/Unix-like systems, use:
```
$ cat zzApotheosis.gpg.base64 | base64 --decode | gpg --import
```

On Windows PowerShell:
```
certutil -decode .\zzApotheosis.gpg.base64 .\zzApotheosis.gpg
```
and then import it into Kleopatra or whatever GnuPG software is supposed to be used on Windows.

