unit QlpIQrTemplate;

{$I QRCodeGenLib.inc}

interface

uses
  QlpQRCodeGenLibTypes;

type
  IQrTemplate = interface(IInterface)
    ['{BFD8DF9F-5A1E-45ED-B928-388604BF3112}']

    function GetTemplate(): TQRCodeGenLibInt32Array;
    property Template: TQRCodeGenLibInt32Array read GetTemplate;
    function GetDataOutputBitIndexes(): TQRCodeGenLibInt32Array;
    property DataOutputBitIndexes: TQRCodeGenLibInt32Array
      read GetDataOutputBitIndexes;
    function GetMasks(): TQRCodeGenLibMatrixInt32Array;
    property Masks: TQRCodeGenLibMatrixInt32Array read GetMasks;

  end;

implementation

end.
