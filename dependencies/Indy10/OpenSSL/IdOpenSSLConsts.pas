unit IdOpenSSLConsts;

interface

{$i IdCompilerDefines.inc}

const
  {$IFDEF MSWINDOWS}
  CLibCrypto =
    {$IFDEF CPU32}'libcrypto-1_1.dll'{$ENDIF}
    {$IFDEF CPU64}'libcrypto-1_1-x64.dll'{$ENDIF}
    ;
  CLibSSL =
    {$IFDEF CPU32}'libssl-1_1.dll'{$ENDIF}
    {$IFDEF CPU64}'libssl-1_1-x64.dll'{$ENDIF}
    ;
  {$ENDIF}

  {$IFDEF LINUX}
  CLibCrypto = 'libcrypto.so';
  CLibSSL = 'libssl.so';
  {$ENDIF}

  {$IFDEF DARWIN}
  CLibCrypto = 'libcrypto.dylib';
  CLibSSL = 'libssl.dylib';
  {$ENDIF}

implementation

end.
