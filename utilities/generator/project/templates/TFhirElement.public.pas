    function noExtensions : TFhirElement;
    property DisallowExtensions : boolean read FDisallowExtensions write FDisallowExtensions;
    function hasExtensions : boolean; override;
    function hasExtension(url : string) : boolean; override;
    function getExtensionString(url : String) : String; override;
    function extensionCount(url : String) : integer; override;
    function extensions(url : String) : TFslList<TFHIRObject>; override;
    procedure addExtension(url : String; value : TFHIRObject); override;
