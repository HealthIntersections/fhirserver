unit QlpIReedSolomonGenerator;

{$I QRCodeGenLib.inc}

interface

uses
  QlpQRCodeGenLibTypes;

type
  IReedSolomonGenerator = interface(IInterface)
    ['{589D6EEB-8B78-478C-A316-F9FB02BDFAF2}']

    procedure GetRemainder(const AData: TQRCodeGenLibByteArray;
      ADataOff, ADataLen: Int32; const AResult: TQRCodeGenLibByteArray);
  end;

implementation

end.
