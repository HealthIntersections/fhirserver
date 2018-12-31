unit FHIR.Transformer.Debugger;

interface

uses
  SysUtils,
  FHIR.Support.Base;

const
  DBG_STOPPED = 0;
  DBG_EXECUTE = 1;
  DBG_STEP_OVER = 2;
  DBG_STEP_OUT = 3;
  DBG_STEP_INTO = 4;
  // values 10 or above will abort debugging run
  DBG_STOP = 10;
  DBG_CLOSING = 11;


type
  TFHIRTransformerDebugger = class (TFslObject)
  private
  protected
  public
    // register all the sources
    // set break points

  end;

  TFHIRStructureMapDebugger = class (TFHIRTransformerDebugger)
  private
  protected
  public
  end;

implementation

end.
