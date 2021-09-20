unit QlpIQrSegment;

{$I QRCodeGenLib.inc}

interface

uses
  QlpQrSegmentMode,
  QlpQRCodeGenLibTypes;

type
  IQrSegment = interface(IInterface)
    ['{2447C529-F7E7-4E6C-B21F-81E2455DA8B6}']

    function GetMode: TQrSegmentMode;
    property Mode: TQrSegmentMode read GetMode;
    function GetNumChars: Int32;
    property NumChars: Int32 read GetNumChars;
    function GetBitLength: Int32;
    property BitLength: Int32 read GetBitLength;
    function GetData: TQRCodeGenLibInt32Array;
    property Data: TQRCodeGenLibInt32Array read GetData;

  end;

implementation

end.
