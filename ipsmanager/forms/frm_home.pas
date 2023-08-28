unit frm_home;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  laz.VirtualTrees;

type

  { TIPSManagerForm }

  TIPSManagerForm = class(TForm)
    ImageList1: TImageList;
    LazVirtualStringTree1: TLazVirtualStringTree;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
  private

  public

  end;

var
  IPSManagerForm: TIPSManagerForm;

implementation

{$R *.lfm}

end.

