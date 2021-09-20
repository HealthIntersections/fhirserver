unit QlpIQrCode;

{$I QRCodeGenLib.inc}

interface

uses
  QlpQRCodeGenLibTypes;

type
  IQrCode = interface(IInterface)
    ['{BD801EA2-0A27-46B0-B1CB-0AD8A3EC71AE}']

    function GetVersion: Int32;
    property Version: Int32 read GetVersion;
    function GetSize: Int32;
    property Size: Int32 read GetSize;
    function GetMask: Int32;
    property Mask: Int32 read GetMask;
    function GetModules: TQRCodeGenLibInt32Array;
    property Modules: TQRCodeGenLibInt32Array read GetModules;
    function GetBackgroundColor: TQRCodeGenLibColor;
    procedure SetBackgroundColor(const AColor: TQRCodeGenLibColor);
    property BackgroundColor: TQRCodeGenLibColor read GetBackgroundColor
      write SetBackgroundColor;
    function GetForegroundColor: TQRCodeGenLibColor;
    procedure SetForegroundColor(const AColor: TQRCodeGenLibColor);
    property ForegroundColor: TQRCodeGenLibColor read GetForegroundColor
      write SetForegroundColor;

    /// <summary>
    /// Returns the color of the module (pixel) at the specified coordinates,
    /// which is either false for white or true for black. The top left
    /// corner has the coordinates (x=0, y=0).If the specified coordinates
    /// are out of bounds, then false (white) is returned.
    /// </summary>
    /// <param name="Ax">
    /// the x coordinate, where 0 is the left edge and FSize - 1 is the right
    /// edge
    /// </param>
    /// <param name="Ay">
    /// the y coordinate, where 0 is the top edge and FSize - 1 is the bottom
    /// edge
    /// </param>
    /// <returns>
    /// the module's color, which is either false (white) or true (black)
    /// </returns>
    function GetModule(Ax, Ay: Int32): Boolean;

    /// <summary>
    /// Returns a bitmap image depicting this QR Code, with the specified
    /// module scale and border modules. For example, ToBitmapImage(scale=10,
    /// border=4) means to pad the QR Code with 4 white border modules on all
    /// four sides, and use 10×10 pixels to represent each module. The
    /// resulting image contains the colors specified by the backgroundcolor
    /// (by default = FFFFFF) and foregroundcolor (by default = 000000)
    /// properties.
    /// </summary>
    /// <param name="AScale">
    /// the side length (measured in pixels, must be positive) of each module
    /// </param>
    /// <param name="ABorder">
    /// the number of border modules to add, which must be non-negative
    /// </param>
    /// <returns>
    /// a new bmp image representing this QR Code, with padding and scaling
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the scale or border is out of range, or if <br />{AScale, ABorder,
    /// FSize} cause the image dimensions to exceed System.High(Int32)
    /// </exception>
    /// <remarks>
    /// <b>The caller is responsible for the lifetime of the returned image
    /// object.</b>
    /// </remarks>
    function ToBitmapImage(AScale, ABorder: Int32): TQRCodeGenLibBitmap;

    /// <summary>
    /// Returns a string of SVG code for an image depicting this QR Code,
    /// with the specified number of border modules. The string always uses
    /// Unix newlines Char(#10), regardless of the platform.
    /// </summary>
    /// <param name="ABorder">
    /// the number of border modules to add, which must be non-negative
    /// </param>
    /// <returns>
    /// a string representing this QR Code as an SVG XML document
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the border is negative
    /// </exception>
    function ToSvgString(ABorder: Int32): String;

    /// <summary>
    /// saves a string of SVG code for an image depicting this QR Code, with
    /// the specified number of border modules as an svg file. The string
    /// always uses Unix newlines Char(#10), regardless of the platform.
    /// </summary>
    /// <param name="ABorder">
    /// the number of border modules to add, which must be non-negative
    /// </param>
    /// <param name="AFileName">
    /// the filename to save the output to <b>with file extension</b>
    /// </param>
    /// <returns>
    /// true on success and false on failure.
    /// </returns>
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the border is negative
    /// </exception>
    function ToSvgFile(ABorder: Int32; const AFileName: String): Boolean;
  end;

implementation

end.
