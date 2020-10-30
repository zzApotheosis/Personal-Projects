# Personal-Projects
I've decided to merge all of my personal projects into one repository, as I don't expect them to become large enough to warrant their own independent repositories.

## Using my public GnuPG key
The provided GnuPG key file "zzApotheosis.gpg.base64" is base64-encoded data that must be decoded before importing to your keychain.

To do this on GNU/Linux/Unix-like systems, use:
```
$ cat zzApotheosis.gpg | gpg --import
```

On Windows, you can import it into Kleopatra or other GnuPG software you might prefer.

## Verifying your downloaded data
If you're like me and you care about digital security, the smart thing to do would be to not just blindly trust that my GnuPG key is actually my public key and not some arbitrary malware. To be certain, use your own trusted and preferred anti-virus method to scan `zzApotheosis.gpg` and verify it is safe.

Additionally, I would recommend verifying the data you receive is indeed the exact same data that you think you downloaded. This goes for pretty much anything you download; it is always recommended to digitally verify the data you receive if you care about digital security. This is done by locally calculating checksums and/or hash values. On GNU/Linux/Unix-like systems, you can use this to verify my GnuPG key:
```
$ cat zzApotheosis.gpg | sha512sum
```
You may visually verify the locally calculated hash value is the same as the one contained in `zzApotheosis.gpg.sha512`:
```
$ cat zzApotheosis.gpg.sha512
```

This is a bit different on Windows. You can calculate SHA512 hash values using PowerShell in recent versions of Windows 10:
```
C:\> certutil -hashfile .\zzApotheosis.gpg SHA512
```
And you may visually verify the locally calculated hash value is the same as the one in `zzApotheosis.gpg.sha512` by opening the text file in your preferred text editor.

