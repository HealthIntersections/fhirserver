openSSL
-------

OpenSSL is bound statically on several platforms. 
This saves from having any version/path dependency issue. 
Also, on OSX, it allows for a hardened runtime

In order to reduce build complexity, the output of the 
standard openSSL build process is included in the git repository. 
These are the files libcrypto.a and libssl.a for the 
various supported platforms (win32+64, linux, and mac + mac/M1).
They are created by the standard openSSL build process on each
of the platforms. See https://github.com/openssl/openssl/blob/master/INSTALL.md
for further information.

OpenSSL Version
---------------

The openSSL version used by the FHIR suite is 1.1.1, so the openSSL
branch to use is OpenSSL_1_1_1-stable. The library files should be 
updated each time that a new openSSL 1.1.1 version is released when 
the release includes security fixes.


Platform Specific Notes
-----------------------

No additional notes.

