    property StringValue : String read GetStringValue write SetStringValue;
    function isPrimitive : boolean; override;
    function hasPrimitiveValue : boolean; override;
    function primitiveValue : string; override;
    function setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject; override;
    function ToString : String; override;
