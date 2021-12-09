unit IdOpenSSLConsts;

interface

{$i IdCompilerDefines.inc}

const
  CLibCryptoRaw = 'libcrypto';
  CLibSSLRaw = 'libssl';

  SSLDLLVers: array [0..1] of string = ('', '.1.1');

  {$IFDEF OSX}
  CLibCrypto = CLibCryptoRaw+'-1_1.dylib';
  CLibSSL = CLibSSLRaw+'-1_1.dylib';
  {$ENDIF}
  {$IFDEF LINUX}
  CLibCrypto = CLibCryptoRaw+'-1_1.so';
  CLibSSL = CLibSSLRaw+'-1_1.so';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  CLibCrypto =
    {$IFDEF CPU32}CLibCryptoRaw + '-1_1.dll'{$ENDIF}
    {$IFDEF CPU64}CLibCryptoRaw + '-1_1-x64.dll'{$ENDIF}
    ;
  CLibSSL =
    {$IFDEF CPU32}CLibSSLRaw + '-1_1.dll'{$ENDIF}
    {$IFDEF CPU64}CLibSSLRaw + '-1_1-x64.dll'{$ENDIF}
    ;
  {$ENDIF}

implementation

end.
