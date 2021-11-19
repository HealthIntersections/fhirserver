unit IdOpenSSLConsts;

interface

{$i IdCompilerDefines.inc}

const
  CLibCryptoRaw = 'libcrypto';
  CLibSSLRaw = 'libssl';

  {$IFDEF OSX}
  SSLDLLVers: array [0..0] of string = ('.1.1'); // can't try the unversioned ones on OSX
  {$ELSE}
  SSLDLLVers: array [0..1] of string = ('', '.1.1');
  {$ENDIF}
  CLibCrypto =
    {$IFDEF CPU32}CLibCryptoRaw + '-1_1.dll'{$ENDIF}
    {$IFDEF CPU64}CLibCryptoRaw + '-1_1-x64.dll'{$ENDIF}
    ;
  CLibSSL =
    {$IFDEF CPU32}CLibSSLRaw + '-1_1.dll'{$ENDIF}
    {$IFDEF CPU64}CLibSSLRaw + '-1_1-x64.dll'{$ENDIF}
    ;

implementation

end.
