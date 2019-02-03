object TransformerForm: TTransformerForm
  Left = 0
  Top = 0
  Caption = 'FHIR Transformer IDE'
  ClientHeight = 595
  ClientWidth = 960
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    960
    595)
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 27
    Width = 8
    Height = 549
    ExplicitTop = 41
    ExplicitHeight = 554
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 960
    Height = 27
    Align = alTop
    TabOrder = 0
    object ToolBar1: TToolBar
      Left = 1
      Top = 1
      Width = 958
      Height = 29
      Caption = 'ToolBar1'
      Images = ImageList1
      TabOrder = 0
      object tbNew: TToolButton
        Left = 0
        Top = 0
        Hint = 'New File'
        Caption = 'New File'
        ImageIndex = 0
        ParentShowHint = False
        ShowHint = True
        OnClick = tbNewClick
      end
      object tbOpen: TToolButton
        Left = 23
        Top = 0
        Hint = 'Add Existing File'
        Caption = 'Add Existing File'
        ImageIndex = 1
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuOpenClick
      end
      object tbSave: TToolButton
        Left = 46
        Top = 0
        Hint = 'Save'
        Caption = 'Save'
        ImageIndex = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuSaveClick
      end
      object tbSaveAll: TToolButton
        Left = 69
        Top = 0
        Hint = 'Save'
        Caption = 'Save All'
        ImageIndex = 35
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuSaveAllClick
      end
      object tbPrint: TToolButton
        Left = 92
        Top = 0
        Hint = 'Print'
        Caption = 'Print'
        ImageIndex = 24
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton5: TToolButton
        Left = 115
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        ImageIndex = 28
        Style = tbsSeparator
      end
      object tbUndo: TToolButton
        Left = 123
        Top = 0
        Hint = 'Undo'
        Caption = 'Undo'
        ImageIndex = 20
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuUndoClick
      end
      object tbCut: TToolButton
        Left = 146
        Top = 0
        Hint = 'Cut'
        Caption = 'Cut'
        ImageIndex = 5
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuCutClick
      end
      object tbCopy: TToolButton
        Left = 169
        Top = 0
        Hint = 'Copy'
        Caption = 'Copy'
        ImageIndex = 6
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuCopyClick
      end
      object tbPaste: TToolButton
        Left = 192
        Top = 0
        Hint = 'Paste'
        Caption = 'Paste'
        ImageIndex = 7
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuPasteClick
      end
      object tbFind: TToolButton
        Left = 215
        Top = 0
        Hint = 'Find'
        Caption = 'Find'
        ImageIndex = 16
        ParentShowHint = False
        ShowHint = True
        OnClick = FindDialogFind
      end
      object tbCompare: TToolButton
        Left = 238
        Top = 0
        Hint = 'Compare Round Trip'
        Caption = 'Compare Round Trip'
        ImageIndex = 28
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuCompareClick
      end
      object ToolButton2: TToolButton
        Left = 261
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 30
        Style = tbsSeparator
      end
      object tbPackageManager: TToolButton
        Left = 269
        Top = 0
        Hint = 'Package Manager'
        Caption = 'Package Manager'
        ImageIndex = 29
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuPackageManagerClick
      end
      object ToolButton3: TToolButton
        Left = 292
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 28
        Style = tbsSeparator
      end
      object tbHome: TToolButton
        Left = 300
        Top = 0
        Hint = 'Home'
        Caption = 'Home'
        ImageIndex = 8
        ParentShowHint = False
        ShowHint = True
        OnClick = tbHomeClick
      end
      object tbCompile: TToolButton
        Left = 323
        Top = 0
        Hint = 'Check Syntax'
        Caption = 'Check Syntax'
        ImageIndex = 34
        ParentShowHint = False
        ShowHint = True
        OnClick = mnuCompileClick
      end
      object ToolButton1: TToolButton
        Left = 346
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 28
        Style = tbsSeparator
      end
      object tbExecute: TToolButton
        Left = 354
        Top = 0
        Hint = 'Execute'
        Caption = 'Execute'
        ImageIndex = 27
        ParentShowHint = False
        ShowHint = True
        OnClick = btnExecuteClick
      end
      object tbStepInto: TToolButton
        Left = 377
        Top = 0
        Hint = 'Step Into'
        Caption = 'Step Into'
        Enabled = False
        ImageIndex = 36
        ParentShowHint = False
        ShowHint = True
        OnClick = tbStepIntoClick
      end
      object tbStepOver: TToolButton
        Left = 400
        Top = 0
        Hint = 'Step Over'
        Caption = 'Step Over'
        Enabled = False
        ImageIndex = 37
        ParentShowHint = False
        ShowHint = True
        OnClick = tbStepOverClick
      end
      object tbStepOut: TToolButton
        Left = 423
        Top = 0
        Hint = 'Step Out'
        Caption = 'Step Out'
        Enabled = False
        ImageIndex = 38
        ParentShowHint = False
        ShowHint = True
        OnClick = tbStepOutClick
      end
      object tbStop: TToolButton
        Left = 446
        Top = 0
        Hint = 'Stop'
        Caption = 'Stop'
        Enabled = False
        ImageIndex = 39
        ParentShowHint = False
        ShowHint = True
        OnClick = tbStopClick
      end
    end
  end
  object pnlWorkspace: TPanel
    Left = 0
    Top = 27
    Width = 185
    Height = 549
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 1
    object Panel6: TPanel
      Left = 1
      Top = 1
      Width = 183
      Height = 32
      Align = alTop
      TabOrder = 0
      object edtWorkspace: TEdit
        Left = 4
        Top = 4
        Width = 144
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = edtWorkspaceChange
      end
      object btnAddContent: TBitBtn
        Left = 154
        Top = 3
        Width = 23
        Height = 23
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE4F0E69FC8A5559C5E3F
          8E483B8C454C955397C19BE1EDE2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFC3DDC857A0634199507DC28F96D0A696CFA678BE89368D42408D47B9D5
          BBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6DFCB54A06364B478A8DBB587CC9866
          BC7D64BA7C86CB98A5D9B458AA6B35863CB9D5BBFFFFFFFFFFFFFFFFFFE8F3EB
          67AC756AB97DA8DBB260BC775CBA7359B87059B56F58B56F5BB774A5D9B35AAA
          6C418E48E2EDE3FFFFFFFFFFFFAED4B853AB68AADDB464C1795FBE7160BC77FF
          FFFFFFFFFF59B87058B56E5CB774A6DAB4388F4396C19AFFFFFFFFFFFF77B788
          8ACC9889D3966BC67A63C17055AB65FFFFFFFFFFFF59B87059B8705BB97285CC
          977BBE8D4D9654FFFFFFFFFFFF6AB17EA9DDB37DCF8A75CC81FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFF59B87067BE7D9CD4AB3B8C43FFFFFFFFFFFF6EB482
          B6E2BE8BD5977AC986FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF59B87069C1
          7E9DD4AA3F8F48FFFFFFFFFFFF81BF94ACDDB6A6DFAF81CB8C7CC9866EBD79FF
          FFFFFFFFFF5BAC6A60BC775CBA738BD19980C592579D60FFFFFFFFFFFFB8DBC3
          85C797D2EED795D9A08AD3947FC889FFFFFFFFFFFF79CD856BC37C6FC77EACDF
          B5459E57A0C8A6FFFFFFFFFFFFECF5EF7EBF93AADAB7D8F1DC92D89D88CD9384
          CC8E8BD4968AD49583D28EAFE0B76BB97D5AA267E6F0E7FFFFFFFFFFFFFFFFFF
          D1E8D976BB8CAFDCBBDCF2E0B6E4BD9BDBA596D9A0A5DFAFC0E8C579C28A57A2
          65C4DEC9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD1E8D97EBE9294CEA4C3E6CBCF
          EBD4C9E9CEAFDDB86DB97F68AD77C7E0CCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFEBF5EFB9DCC482C09571B6856EB48279B98AB1D5BAE8F3EBFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        PopupMenu = pmAddAsset
        TabOrder = 1
        OnClick = btnAddContentClick
      end
    end
    object Panel7: TPanel
      Left = 1
      Top = 33
      Width = 183
      Height = 515
      Align = alClient
      Anchors = [akLeft, akTop, akBottom]
      BevelOuter = bvNone
      BorderWidth = 4
      Caption = 'pnlWorkspace'
      TabOrder = 1
      object vtWorkspace: TVirtualStringTree
        Left = 4
        Top = 4
        Width = 175
        Height = 507
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Images = ImageList1
        NodeDataSize = 8
        PopupMenu = pmWorkspace
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
        OnAddToSelection = vtWorkspaceAddToSelection
        OnDblClick = vtWorkspaceDblClick
        OnGetText = vtWorkspaceGetText
        OnGetImageIndexEx = vtWorkspaceGetImageIndexEx
        OnInitChildren = vtWorkspaceInitChildren
        OnInitNode = vtWorkspaceInitNode
        OnRemoveFromSelection = vtWorkspaceRemoveFromSelection
        Columns = <>
      end
    end
  end
  object Panel3: TPanel
    Left = 193
    Top = 27
    Width = 767
    Height = 549
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 0
      Top = 280
      Width = 767
      Height = 8
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 381
      ExplicitWidth = 825
    end
    object pnlDebug: TPanel
      Left = 0
      Top = 288
      Width = 767
      Height = 261
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object pgDebug: TPageControl
        Left = 0
        Top = 0
        Width = 767
        Height = 261
        ActivePage = TabSheet3
        Align = alClient
        TabOrder = 0
        object tbConsole: TTabSheet
          Caption = 'Console'
          object Panel5: TPanel
            Left = 668
            Top = 0
            Width = 91
            Height = 233
            Align = alRight
            Alignment = taLeftJustify
            TabOrder = 0
            object btnConsoleClear: TButton
              Left = 5
              Top = 61
              Width = 75
              Height = 25
              Caption = 'Clear'
              TabOrder = 0
              OnClick = btnConsoleClearClick
            end
            object btnConsoleSave: TButton
              Left = 6
              Top = 30
              Width = 75
              Height = 25
              Caption = 'Save'
              TabOrder = 1
              OnClick = btnConsoleSaveClick
            end
            object btnConsoleCopy: TButton
              Left = 6
              Top = -1
              Width = 75
              Height = 25
              Caption = 'Copy'
              TabOrder = 2
              OnClick = btnConsoleCopyClick
            end
          end
          object mConsole: TMemo
            Left = 0
            Top = 0
            Width = 668
            Height = 233
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
        end
        object tbBreakpoints: TTabSheet
          Caption = 'Breakpoints'
          ImageIndex = 1
          object Panel8: TPanel
            Left = 0
            Top = 0
            Width = 759
            Height = 233
            Align = alClient
            BorderWidth = 4
            Caption = 'Panel8'
            TabOrder = 0
            object ListBox1: TListBox
              Left = 5
              Top = 5
              Width = 749
              Height = 223
              Align = alClient
              ItemHeight = 13
              TabOrder = 0
            end
          end
        end
        object tbVariables: TTabSheet
          Caption = 'Debugger'
          ImageIndex = 3
          object Splitter3: TSplitter
            Left = 353
            Top = 0
            Width = 8
            Height = 233
            ExplicitLeft = 514
            ExplicitTop = 3
          end
          object Splitter4: TSplitter
            Left = 617
            Top = 0
            Width = 8
            Height = 233
            ExplicitLeft = 950
            ExplicitTop = -2
          end
          object Panel12: TPanel
            Left = 361
            Top = 0
            Width = 256
            Height = 233
            Align = alLeft
            BorderWidth = 4
            TabOrder = 0
            object vtVars: TVirtualStringTree
              Left = 5
              Top = 25
              Width = 246
              Height = 203
              Align = alClient
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.MainColumn = -1
              TabOrder = 0
              OnAddToSelection = vtVarsAddToSelection
              OnClick = vtVarsClick
              OnDrawText = vtVarsDrawText
              OnGetText = vtVarsGetText
              OnInitNode = vtVarsInitNode
              OnRemoveFromSelection = vtVarsRemoveFromSelection
              Columns = <>
            end
            object Panel11: TPanel
              Left = 5
              Top = 5
              Width = 246
              Height = 20
              Align = alTop
              Alignment = taLeftJustify
              BevelOuter = bvNone
              Caption = ' Variable List'
              TabOrder = 1
            end
          end
          object Panel13: TPanel
            Left = 625
            Top = 0
            Width = 134
            Height = 233
            Align = alClient
            BorderWidth = 4
            TabOrder = 1
            object Panel14: TPanel
              Left = 5
              Top = 5
              Width = 124
              Height = 20
              Align = alTop
              Alignment = taLeftJustify
              BevelOuter = bvNone
              Caption = ' Details for Selected Variable'
              TabOrder = 0
            end
            object vtVarDetails: TVirtualStringTree
              Left = 5
              Top = 25
              Width = 124
              Height = 203
              Align = alClient
              Header.AutoSizeIndex = 2
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
              TabOrder = 1
              OnColumnResize = vtVarDetailsColumnResize
              OnFreeNode = vtVarDetailsFreeNode
              OnGetText = vtVarDetailsGetText
              OnInitChildren = vtVarDetailsInitChildren
              OnInitNode = vtVarDetailsInitNode
              Columns = <
                item
                  Position = 0
                  Width = 100
                  WideText = 'Name'
                end
                item
                  Position = 1
                  Width = 100
                  WideText = 'Type'
                end
                item
                  Position = 2
                  Width = 10
                  WideText = 'Value'
                end>
            end
          end
          object Panel9: TPanel
            Left = 0
            Top = 0
            Width = 353
            Height = 233
            Align = alLeft
            BorderWidth = 4
            Caption = 'Panel8'
            TabOrder = 2
            object vtCallStack: TVirtualStringTree
              Left = 5
              Top = 25
              Width = 343
              Height = 203
              Align = alClient
              Header.AutoSizeIndex = -1
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.Height = 17
              Header.MainColumn = -1
              Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
              NodeDataSize = 8
              TabOrder = 0
              OnAddToSelection = vtCallStackAddToSelection
              OnClick = vtCallStackClick
              OnDrawText = vtCallStackDrawText
              OnGetText = vtCallStackGetText
              OnInitNode = vtCallStackInitNode
              OnRemoveFromSelection = vtCallStackRemoveFromSelection
              Columns = <>
            end
            object Panel15: TPanel
              Left = 5
              Top = 5
              Width = 343
              Height = 20
              Align = alTop
              Alignment = taLeftJustify
              BevelOuter = bvNone
              Caption = ' Call Stack'
              TabOrder = 1
            end
          end
        end
        object TabSheet3: TTabSheet
          Caption = 'FHIRPath Evaluator'
          ImageIndex = 4
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 759
            Height = 28
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              759
              28)
            object Label4: TLabel
              Left = 2
              Top = 4
              Width = 50
              Height = 13
              Caption = 'FHIRPath:'
            end
            object btnPathGo: TBitBtn
              Left = 605
              Top = 2
              Width = 70
              Height = 25
              Anchors = [akTop, akRight]
              Caption = 'Execute'
              Glyph.Data = {
                36030000424D3603000000000000360000002800000010000000100000000100
                18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                185728164F25164F25164F25154C23154C23154C23154C23154C23154C23154C
                23FFFFFFFFFFFFFFFFFFFFFFFF1C672F80B48ECBEFD4B8E9C5B8E9C5B8E9C5B8
                E9C5A1E2B2A1E2B2A1E2B2AFE6BD80B48E154C23FFFFFFFFFFFFFFFFFF1C672F
                F4FBF64DD1774BCF7449CD7147CB6D44C96941C6653EC25F3EC25F3EC25FAFE6
                BD154C23FFFFFFFFFFFFFFFFFF217436E8F8EC4FD37A4DD1774BCF7449CD7147
                CB6D41C66541C6653EC25F3EC25FA1E2B2154C23FFFFFFFFFFFFFFFFFF26873E
                E8F8EC52D57D4FD37A4DD177FFFFFF49CD7144C96944C96941C6653EC25FA1E2
                B2154C23FFFFFFFFFFFFFFFFFF26873EE8F8EC54D68052D57D4FD37AFFFFFFFF
                FFFF47CB6D44C96941C66541C665A1E2B2154C23FFFFFFFFFFFFFFFFFF2CA149
                E8F8EC56D98556D98554D680FFFFFFFFFFFFFFFFFF49CD7147CB6D44C969B8E9
                C5164F25FFFFFFFFFFFFFFFFFF2CA149E8F8EC59DB8956D98556D985FFFFFFFF
                FFFF4DD1774BCF7449CD7147CB6DB8E9C5164F25FFFFFFFFFFFFFFFFFF2FAA4E
                E8F8EC5ADC8A59DB8956D985FFFFFF54D6804FD37A4DD1774BCF7449CD71B8E9
                C5185728FFFFFFFFFFFFFFFFFF2FAA4EE8F8EC5CDE8E5ADC8A59DB8956D98556
                D98552D57D4FD37A4DD1774BCF74B8E9C5185728FFFFFFFFFFFFFFFFFF2FAA4E
                F4FBF65CDE8E5CDE8E5ADC8A59DB8956D98554D68052D57D4FD37A4DD177CBEF
                D41C672FFFFFFFFFFFFFFFFFFF2FAA4E80B48EF4FBF6E8F8ECE8F8ECE8F8ECE8
                F8ECE8F8ECE8F8ECE8F8ECF4FBF6A1E2B2217436FFFFFFFFFFFFFFFFFFFFFFFF
                2FAA4E2FAA4E2FAA4E2FAA4E2FAA4E2FAA4E2CA1492CA1492894442894442687
                3EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              TabOrder = 0
              OnClick = btnPathGoClick
            end
            object btnPathDebug: TBitBtn
              Left = 681
              Top = 2
              Width = 70
              Height = 25
              Anchors = [akTop, akRight]
              Caption = 'Debug'
              Glyph.Data = {
                36030000424D3603000000000000360000002800000010000000100000000100
                18000000000000030000C30E0000C30E00000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFAB6BABFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFED2DED72
                7272FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFEF2FEFD757D7BB3BBBAD2DADFF00FFC142C1FF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA565A577777777777777
                7777995999FF00FF696969FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFA161A1777777777777777777777777777777A868A8747474FF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFE92AE977777777777777777777777777
                7777777777787878717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                CA4ACA777777777777777777777777777777777777777777A262A2FF00FFFF00
                FFFF00FFFF00FFFF00FFA767A7727272AB6BAB77777777777777777777777777
                7777777777777777787878757575945494DF20DFFF00FFFF00FFFF00FFFF00FF
                A929A9965796777777777777777777777777777777777777777777DF20DFE324
                E3AA2BAAFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9C5C9C77777777777777
                77777777777A7A7ACA4ACA848484FF00FFFF00FFFF00FFC242C2FF00FFFF00FF
                C243C26969697979796E6E6E888888787878777777C94AC96666665A5A5A7070
                708383836E6E6ED515D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF71
                7171DD1EDD7C7C7C5A5A5AD515D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FF905090E526E5FF00FF6E6E6EFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDE
                1FDEAC2DACFF00FF7E7E7EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6E6E6EFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFBE3FBED515D5FF00FFFF00FFFF00FFFF00FFFF00FF}
              TabOrder = 1
              OnClick = btnPathDebugClick
            end
            object edtFHIRPath: TEdit
              Left = 58
              Top = 4
              Width = 541
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
            end
          end
          object Panel4: TPanel
            Left = 0
            Top = 28
            Width = 759
            Height = 205
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              759
              205)
            object Label5: TLabel
              Left = 2
              Top = 6
              Width = 43
              Height = 13
              Caption = 'Outcome'
            end
            object lbFHIRPathOutcomes: TListBox
              Left = 58
              Top = 6
              Width = 695
              Height = 187
              Anchors = [akLeft, akTop, akRight, akBottom]
              ItemHeight = 13
              TabOrder = 0
              OnClick = lbFHIRPathOutcomesClick
            end
          end
        end
      end
    end
    object Panel17: TPanel
      Left = 0
      Top = 0
      Width = 767
      Height = 280
      Align = alClient
      Caption = 'Panel17'
      TabOrder = 1
      object pgTabs: TPageControl
        Left = 1
        Top = 1
        Width = 765
        Height = 278
        ActivePage = TabSheet1
        Align = alClient
        Images = ImageList1
        PopupMenu = pmTabs
        TabOrder = 0
        OnChange = pgTabsChange
        object TabSheet1: TTabSheet
          Caption = 'Home'
          ImageIndex = 8
          DesignSize = (
            757
            249)
          object lblExecutionError: TLabel
            Left = 223
            Top = 150
            Width = 509
            Height = 13
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            ExplicitWidth = 810
          end
          object Label1: TLabel
            Left = 3
            Top = 3
            Width = 140
            Height = 13
            Caption = 'Execution Configurations'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object btnExecute: TBitBtn
            Left = 639
            Top = 27
            Width = 109
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Debug'
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              001B7600176B00176B00176B0018660018660018660018660018660018660018
              66FFFFFFFFFFFFFFFFFFFFFFFF00218B5152CDACAFFF8C8EFF8C8EFF8C8EFF8C
              8EFF6568FF6568FF6568FF7D80FF5152CD001866FFFFFFFFFFFFFFFFFF00218B
              EFF0FF0014FF0016FF001BFF0020FF0027FF002AFF0034FF0034FF0034FF7D80
              FF001866FFFFFFFFFFFFFFFFFF00259ADADAFF000FFF0014FF0016FF001BFF00
              20FF002AFF002AFF0034FF0034FF6568FF001866FFFFFFFFFFFFFFFFFF002DB4
              DADAFF000AFF000FFF0014FFFFFFFF001BFF0027FF0027FF002AFF0034FF6568
              FF001866FFFFFFFFFFFFFFFFFF002DB4DADAFF0006FF000AFF000FFFFFFFFFFF
              FFFF0020FF0027FF002AFF002AFF6568FF001866FFFFFFFFFFFFFFFFFF0036D8
              DADAFF0200FF0200FF0006FFFFFFFFFFFFFFFFFFFF001BFF0020FF0027FF8C8E
              FF00176BFFFFFFFFFFFFFFFFFF0036D8DADAFF0700FF0200FF0200FFFFFFFFFF
              FFFF0014FF0016FF001BFF0020FF8C8EFF00176BFFFFFFFFFFFFFFFFFF0038E4
              DADAFF0800FF0700FF0200FFFFFFFF0006FF000FFF0014FF0016FF001BFF8C8E
              FF001B76FFFFFFFFFFFFFFFFFF0038E4DADAFF0D00FF0800FF0700FF0200FF02
              00FF000AFF000FFF0014FF0016FF8C8EFF001B76FFFFFFFFFFFFFFFFFF0038E4
              EFF0FF0D00FF0D00FF0800FF0700FF0200FF0006FF000AFF000FFF0014FFACAF
              FF00218BFFFFFFFFFFFFFFFFFF0038E45152CDEFF0FFDADAFFDADAFFDADAFFDA
              DAFFDADAFFDADAFFDADAFFEFF0FF6568FF00259AFFFFFFFFFFFFFFFFFFFFFFFF
              0038E40038E40038E40038E40038E40038E40036D80036D8002FC7002FC7002D
              B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            TabOrder = 0
            OnClick = btnExecuteClick
          end
          object btnRunNoDebug: TBitBtn
            Left = 639
            Top = 58
            Width = 109
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Run (No Debug)'
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              185728164F25164F25164F25154C23154C23154C23154C23154C23154C23154C
              23FFFFFFFFFFFFFFFFFFFFFFFF1C672F80B48ECBEFD4B8E9C5B8E9C5B8E9C5B8
              E9C5A1E2B2A1E2B2A1E2B2AFE6BD80B48E154C23FFFFFFFFFFFFFFFFFF1C672F
              F4FBF64DD1774BCF7449CD7147CB6D44C96941C6653EC25F3EC25F3EC25FAFE6
              BD154C23FFFFFFFFFFFFFFFFFF217436E8F8EC4FD37A4DD1774BCF7449CD7147
              CB6D41C66541C6653EC25F3EC25FA1E2B2154C23FFFFFFFFFFFFFFFFFF26873E
              E8F8EC52D57D4FD37A4DD177FFFFFF49CD7144C96944C96941C6653EC25FA1E2
              B2154C23FFFFFFFFFFFFFFFFFF26873EE8F8EC54D68052D57D4FD37AFFFFFFFF
              FFFF47CB6D44C96941C66541C665A1E2B2154C23FFFFFFFFFFFFFFFFFF2CA149
              E8F8EC56D98556D98554D680FFFFFFFFFFFFFFFFFF49CD7147CB6D44C969B8E9
              C5164F25FFFFFFFFFFFFFFFFFF2CA149E8F8EC59DB8956D98556D985FFFFFFFF
              FFFF4DD1774BCF7449CD7147CB6DB8E9C5164F25FFFFFFFFFFFFFFFFFF2FAA4E
              E8F8EC5ADC8A59DB8956D985FFFFFF54D6804FD37A4DD1774BCF7449CD71B8E9
              C5185728FFFFFFFFFFFFFFFFFF2FAA4EE8F8EC5CDE8E5ADC8A59DB8956D98556
              D98552D57D4FD37A4DD1774BCF74B8E9C5185728FFFFFFFFFFFFFFFFFF2FAA4E
              F4FBF65CDE8E5CDE8E5ADC8A59DB8956D98554D68052D57D4FD37A4DD177CBEF
              D41C672FFFFFFFFFFFFFFFFFFF2FAA4E80B48EF4FBF6E8F8ECE8F8ECE8F8ECE8
              F8ECE8F8ECE8F8ECE8F8ECF4FBF6A1E2B2217436FFFFFFFFFFFFFFFFFFFFFFFF
              2FAA4E2FAA4E2FAA4E2FAA4E2FAA4E2FAA4E2CA1492CA1492894442894442687
              3EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            TabOrder = 1
            OnClick = mnuRunNoDebugClick
          end
          object VirtualStringTree1: TVirtualStringTree
            Left = 18
            Top = 27
            Width = 615
            Height = 214
            Anchors = [akLeft, akTop, akRight, akBottom]
            Header.AutoSizeIndex = 0
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'Tahoma'
            Header.Font.Style = []
            Header.Height = 17
            Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
            TabOrder = 2
            Columns = <
              item
                Position = 0
                Width = 200
                WideText = 'Javascript Source'
              end
              item
                Position = 1
                Width = 200
                WideText = 'Source Object'
              end
              item
                Position = 2
                Width = 200
                WideText = 'Last Outcome'
              end>
          end
        end
      end
    end
  end
  object pnlStatus: TStatusBar
    Left = 0
    Top = 576
    Width = 960
    Height = 19
    AutoHint = True
    Panels = <
      item
        Text = '0, 0'
        Width = 50
      end
      item
        Text = 'Path'
        Width = 150
      end
      item
        Text = 'INS'
        Width = 50
      end
      item
        Text = 'Status'
        Width = 300
      end>
    SizeGrip = False
  end
  object btnMaximise: TBitBtn
    Left = 933
    Top = 27
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    Glyph.Data = {
      66030000424D6603000000000000360000002800000010000000110000000100
      18000000000030030000C40E0000C40E00000000000000000000EEDFD0ECDDCE
      EBDCCEEBDCCEEBDCCEEBDCCEEBDCCEEBDCCEEBDCCEEBDCCEEBDCCEEBDCCEEBDC
      CEEBDCCEECDDCEEDDECFE6D8CAB2AAA1A29B95A29C95A29C95A29C95A29C95A2
      9C95A29C95A29C95A29C95A29C95A29C95A29B95AEA69EDCCFC1E1D3C6898582
      8080809494949595959595959595959595959595959595959595959595959494
      9484848483807ECFC3B7E1D3C68C8985B0B0B0EDEDEDEFEFEFEFEFEFEFEFEFEF
      EFEFEFEFEFEFEFEFEFEFEFEFEFEFEDEDEDBBBBBB8A8784CFC3B7E1D3C68D8A86
      B9B9B9FCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFD
      FDC5C5C58B8885CFC3B7E1D3C68D8A86B9B9B9FCFCFCFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFDC5C5C58B8885CFC3B7E1D3C68D8A86
      B9B9B9FCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFD
      FDC5C5C58B8885CFC3B7E1D3C68D8A86B9B9B9FCFCFCFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFDC5C5C58B8885CFC3B7E1D4C68D8A86
      B9B9B9FCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFD
      FDC5C5C58B8885CFC3B7E1D4C68D8A86B9B9B9FCFCFCFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFDC5C5C58B8885CFC4B7E1D4C68C8986
      B1B1B1EEEEEEF0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0EEEE
      EEBCBCBC8A8784CFC4B8E1D4C78986828484849B9B9B9C9C9C9C9C9C9C9C9C9C
      9C9C9C9C9C9C9C9C9C9C9C9C9C9C9B9B9B88888884817ECFC4B8E1D4C7898683
      8C8C8CA9A9A9AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA9A9
      A991919185827FCFC4B8E1D4C78C8986B1B1B1EEEEEEF0F0F0F0F0F0F0F0F0F0
      F0F0F0F0F0F0F0F0F0F0F0F0F0F0EEEEEEBBBBBB8A8784CFC4B8E2D5C78D8A86
      8685849898979998989998989998989998989998989998989998989998989898
      97898887888582D1C5B9E8DACBB9B0A7A9A29BA9A29BA9A29BA9A29BA9A29BA9
      A29BA9A29BA9A29BA9A29BA9A29BA9A29BA9A29BB5ACA4DFD1C4EFE0D1ECDCCE
      EADBCDEADBCDEADBCDEADBCDEADBCDEADBCDEADBCDEADBCDEADBCDEADBCDEADB
      CDEADBCDEBDCCDEFDFD0}
    TabOrder = 4
    OnClick = btnMaximiseClick
  end
  object btnNormalSize: TBitBtn
    Left = 933
    Top = 27
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000C40E0000C40E00000000000000000000EFDFD0EFDFD0
      EFDFD0EFDFD0EFDFD0EFDFD0EFDFD0EFDFD0EFDFD0EFDFD0EFDFD0EFDFD0EFDF
      D0EFDFD0EFDFD0EFDFD0EFDFD0EEDECFEADBCCE9DACCE9DACCE9DACCE9DACCE9
      DACCE9DACCEADACCECDDCEEFDFD0EFDFD0EFDFD0EFDFD0EFDFD0EFDFD0E3D4C6
      BCB2A9B1A9A1B1A8A0B1A8A0B1A8A0B1A8A0B1A8A0B3AAA2D2C5BAEEDECFEFDF
      D0EFDFD0EFDFD0EFDFD0EFDFD0D5C8BD85827F83828299989799989799989799
      98979494937E7C7BB3ABA2EDDDCEEFDFD0EFDFD0EFDFD0EFDFD0EFDFD0D4C7BC
      807E7BACACACF4F4F4F4F4F4F4F4F4F4F4F4E6E6E68D8D8CB1A8A0EDDDCEEFDF
      D0EFDFD0EFDFD0EFDFD0EFDFD0D4C7BC807E7BB1B1B1FDFDFDFDFDFDFDFDFDFD
      FDFDEEEEEE8F8F8EB0A8A0EBDCCDEEDECFEEDFD0EFDFD0EFDFD0EFDFD0D4C8BC
      807E7B9A9A9ACFCFCFCFCFCFCFCFCFCFCFCFC5C5C58483839A948FC3B8AEC7BC
      B1E1D3C6EEDFD0EFDFD0EFE0D0D4C8BC807E7B6F6F6F74747474747474747474
      74747373736D6D6D7B7B7A868483807D7BC7BCB1EDDFCFEFE0D0EFE0D1D5C9BC
      82807C6C6B6B6B6B6A6A6A6A6B6B6B6C6C6C6C6C6C717171AAAAAAC5C5C58685
      84C4B9AFEDDED0EFE0D1EFE0D1E0D3C6B4ABA3A8A0999B95907B7B79989898AD
      ADADAEAEAEB2B2B2DADADADEDEDE8C8A89C4B9AFEDDED0EFE0D1EFE0D1EEDFD0
      EADCCDEADBCDD1C5B98B8986B4B4B4D6D6D6D7D7D7D7D7D7DBDBDBC5C5C58685
      84C4B9AFEDDED0EFE0D1EFE0D1EFE0D1EFE0D1EFE0D1D6C9BD807E7C73737378
      7878787878787878787878757575757473C4B9AFEDDED0EFE0D1EFE0D1EFE0D1
      EFE0D1EFE0D1D6CABD7F7D7B6A6A6A6A6A6A6A6A6A6A6A6A6A6A6A6A6A6A7473
      72C4BAB0EDDED0EFE0D1EFE0D1EFE0D1EFE0D1EFE0D1DED1C4A39C96948F8B94
      8F8B948F8B948F8B948F8B948F8B9B9590D2C6BAEEDFD0EFE0D1F0E0D1F0E0D1
      F0E0D1F0E0D1ECDDCEE2D4C6E0D1C4E0D1C4E0D1C4E0D1C4E0D1C4E0D1C4E1D2
      C5EBDBCDF0E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0
      E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0E0D1F0E0D1}
    TabOrder = 5
    Visible = False
    OnClick = btnNormalSizeClick
  end
  object ImageList1: TImageList
    Left = 25
    Top = 90
    Bitmap = {
      494C01012D008800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000C0000000010020000000000000C0
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CFB8B40091605A00814B450088544F0093686100A47C7700C9A7A400F3E8
      E900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009056
      4F006E251D0072231D00934B2F009B5A3C00954946008E373700975458009D77
      6E00768670000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C483F007B21
      1900B0582D00BF6A3300BB5A3300BE59330089221F00453F12001F5D0D003981
      2B005B9C4D00D6B5B50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B9837C008A211900AE3C
      260095622A003B51160048491600554015004D3B14000E750A00135D06001252
      0100277B1E004C4A2900E6CED200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FAF6F50097352A00A72523004D48
      1D00068A0E003D6D1D003D641A004E531F003C5A1A00028807000C8A0A001473
      0A000C770500255D13008C505100F5E9EB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D9B1AA00AC26240052571D00039D
      0E0010770D0061501F006D542A00874F3100BB543A006062240020951100137D
      0D00047F030011890D006D1F2200C79195000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4766B00A930280027711300098C
      0A001B6D0E003B63150078411D00C6703800D6824100F29B4300D0783D00A957
      300071621F0025530E007A1C1C00984C4A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C46E62008B492D00257B1A00239C
      25002D932F00138728007E5C2400D5813500D8A83F00DE9E4100DA8B4100E18A
      3F00E7923C008B1D19007920170098534C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CBA69B006F91580037B841003BB0
      40005BAE5D0042C2610060903800B5622A00FAD44600DBA94000C5833F00AE61
      3700A94128009C281C007B1E1500AE706A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E4D7D00099C88B0088FAA0007BBE
      6F007BA85A0057D4650069AD6C00C0945100FFEE5000E5C04000CC963E00DA8D
      3A00C4722F00B667280091352A00D8B3AE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000093987800DDD9A800B2E0
      B40097CB8C0093C97B00CEE0B400FFFFCC00FCED6100EDC93800F4A23E00B782
      30001A5A12003A3F1100CF767000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8AFA800D2C78F00DDD9
      B000C9EBD100C8E3C300EDF0C000FFFFC700FFF68F00FFBC4800A86427001680
      1D0000E0130014700B00E2DBDC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008E988C00D4E3
      AD00FFFFEB00E1E8C600E2D7A500DCD89800CFC88D009EAA6B0036B83E0012FF
      290011BD0F009EA99C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ACB5
      AD0092A47700C8F1A700C1F9B200A6E19B007BED820075B56A0043E6480034AE
      3500C2C4C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EBECEB0098A89A006D976F00699569005E8F5F008A967A00B4B6AD00F9F9
      F900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000018572800164F
      2500164F2500164F2500154C2300154C2300154C2300154C2300154C2300154C
      2300154C23000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C672F0080B48E00CBEF
      D400B8E9C500B8E9C500B8E9C500B8E9C500A1E2B200A1E2B200A1E2B200AFE6
      BD0080B48E00154C230000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C672F00F4FBF6004DD1
      77004BCF740049CD710047CB6D0044C9690041C665003EC25F003EC25F003EC2
      5F00AFE6BD00154C230000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000021743600E8F8EC004FD3
      7A004DD177004BCF740049CD710047CB6D0041C6650041C665003EC25F003EC2
      5F00A1E2B200154C230000000000000000000000000000000000000000000000
      000000000000F4F4F400C0C0C0008787870086868600BCBCBC00F4F4F4000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F1F0FD00ACA7EE00665CD900645DD100ADA9E000F2F2F9000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F0F8FD00A7D5EE005CACD9005DA9D100A9CCE000F2F7F9000000
      0000000000000000000000000000000000000000000026873E00E8F8EC0052D5
      7D004FD37A004DD177000000000049CD710044C9690044C9690041C665003EC2
      5F00A1E2B200154C230000000000000000000000000000000000000000000000
      000000000000C3C3C30096969600A8A8A800A3A3A30091919100BDBDBD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A5F3006B66E7007D7CEE007273ED006A65DD00AEAAE1000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A5DCF30066BDE7007CCDEE0072CBED0065B6DD00AACDE1000000
      0000000000000000000000000000000000000000000026873E00E8F8EC0054D6
      800052D57D004FD37A00000000000000000047CB6D0044C9690041C6650041C6
      6500A1E2B200154C230000000000000000000000000000000000000000000000
      00000000000093939300BCBCBC00959595008E8E8E00A1A1A100888888000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005A57F5009899F1005A5DED004F50ED007573EB00645DD6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000057C5F50098D9F1005AC6ED004FC1ED0073C7EB005DADD6000000
      000000000000000000000000000000000000000000002CA14900E8F8EC0056D9
      850056D9850054D6800000000000000000000000000049CD710047CB6D0044C9
      6900B8E9C500164F250000000000000000000000000000000000000000000000
      00000000000092929200C7C7C700A2A2A20094949400AAAAAA00898989000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005754F700A9ACF2006A6EF200575AEE00807FEE006059DD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000054C6F700A9E0F2006ACFF20057C6EE007FCEEE0059B1DD000000
      000000000000000000000000000000000000000000002CA14900E8F8EC0059DB
      890056D9850056D9850000000000000000004DD177004BCF740049CD710047CB
      6D00B8E9C500164F250000000000000000000000000000000000000000000000
      000000000000C4C4C4009D9D9D00C7C7C700BFBFBF009B9B9B00C2C2C2000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A4A2FB006C68F400AAABF3009E9EF1006B69EA00AAA6F2000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A2E0FB0068C8F400AADFF3009ED9F10069C3EA00A6D8F2000000
      000000000000000000000000000000000000000000002FAA4E00E8F8EC005ADC
      8A0059DB890056D985000000000054D680004FD37A004DD177004BCF740049CD
      7100B8E9C5001857280000000000000000000000000000000000000000000000
      000000000000F5F5F500C4C4C4009292920094949400C3C3C300F6F6F6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F0F1FE00A4A2FB005754F7005B58F600A6A4F600F2F2FD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F0FBFE00A2E0FB0054C6F70058C6F600A4DDF600F2FAFD000000
      000000000000000000000000000000000000000000002FAA4E00E8F8EC005CDE
      8E005ADC8A0059DB890056D9850056D9850052D57D004FD37A004DD177004BCF
      7400B8E9C5001857280000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002FAA4E00F4FBF6005CDE
      8E005CDE8E005ADC8A0059DB890056D9850054D6800052D57D004FD37A004DD1
      7700CBEFD4001C672F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002FAA4E0080B48E00F4FB
      F600E8F8EC00E8F8EC00E8F8EC00E8F8EC00E8F8EC00E8F8EC00E8F8EC00F4FB
      F600A1E2B2002174360000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002FAA4E002FAA
      4E002FAA4E002FAA4E002FAA4E002FAA4E002CA149002CA14900289444002894
      440026873E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B073370000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000001079F000313A9000418AE000419AE000313A9000108A0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000046464600464646004646460046464600464646004646
      4600464646004646460046464600464646000000000000000000000000000000
      000000000000B0733700B0733700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004545450045454500454545004545450045454500454545004545
      4500454545004545450045454500000000000000000000000000000000000104
      9D00041CB1000730C0000734C4000735C5000735C5000734C3000731C100041F
      B30001069E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000078685800614121006141
      210061412100B0733700B0733700B07337000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000109A100052B
      C3000735C7000733C2000732C2000732C2000732C2000732C2000733C3000735
      C400062DBE00020CA40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000046464600464646004646
      460046464600464646004646460046464600000000006141210000000000EDED
      ED0000000000B0733700B0733700EDEDED000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004545450045454500454545004545
      4500454545004545450045454500000000000000000001049B00052BCA000636
      D8000431CD000027C400032EC1000732C2000732C2000430C1000027BF00042F
      C1000735C400072EBE0001069E00000000000000000000000000000000000000
      000000000000B073370000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000061412100000000000000
      000000000000B073370000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000031ABA000537E7000331
      DD00123DD8006480E0001840CB00002CC100022DC0000F38C4006580D9001B43
      C700052FC1000735C500051FB300000000000000000000000000000000000000
      000000000000B0733700B0733700000000000000000046464600464646004646
      4600464646004646460046464600464646000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000919191003A3A3A003A3A
      3A003A3A3A003A3A3A0000000000000000004545450045454500454545004545
      45004545450045454500454545000000000001049E000430E4000436F100002A
      E4005070E900FFFFFF00B7C4F1000D36CA00042DC300A2B2E800FFFFFF006984
      DA000026BE000733C3000731C1000108A0000000000078685800614121006141
      210061412100B0733700B0733700B07337000000000000000000000000000000
      0000000000000000000000000000000000000000000061412100000000000000
      0000000000000000000046464600464646004646460046464600464646004646
      460046464600464646004646460046464600000000003A3A3A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000020FAF000336FA000335F8000232
      EE000A35E8008CA2F200FFFFFF00B4C2F100A9B8ED00FFFFFF00A7B7E900133A
      C400052FC1000732C2000734C4000313AA00000000006141210000000000EDED
      ED0000000000B0733700B0733700EDEDED000000000046464600464646004646
      4600464646004646460046464600464646000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003A3A3A00000000000000
      0000000000000000000000000000000000004545450045454500454545004545
      4500454545004545450045454500000000000619BC001747FE00093AFC000435
      F8000131F000002BE80091A5F400FFFFFF00FFFFFF00ABBAEF00062FC500022D
      C0000732C2000732C2000736C5000419AE000000000061412100000000000000
      000000000000B073370000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000061412100000000000000
      0000000000000000000000000000000000000000000046464600464646004646
      460046464600464646004646460046464600000000003A3A3A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B1DBE004168FE001C49FC000335
      FB000031F9000531F200A4B5F700FFFFFF00FFFFFF00B9C6F2000D36D000002C
      C6000732C2000732C2000736C5000418AD000000000061412100000000000000
      0000000000000000000000000000000000000000000046464600464646004646
      4600464646004646460046464600464646000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003A3A3A00000000000000
      0000000000004545450045454500454545004545450045454500454545004545
      4500454545004545450045454500000000000613B4005B7CFC00486CFD000133
      FB00113CFB00A1B4FE00FFFFFF00A4B6F80092A7F500FFFFFF00B6C4F2001A41
      D300042FC8000732C4000734C3000212A9000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000061412100000000000000
      0000000000000000000000000000000000000000000046464600464646004646
      460046464600464646004646460046464600000000003A3A3A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000003A0004A6AF3008FA6FF001F46
      FB004C6FFC00FFFFFF00A7B8FE000733F600002AED008CA2F600FFFFFF00627F
      E7000028D0000734CC000730C30000069F000000000061412100000000000000
      0000000000000000000046464600464646004646460046464600464646004646
      4600464646004646460046464600464646000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003A3A3A00000000000000
      0000000000000000000000000000676767000000000000000000000000000000
      000000000000000000000000000000000000000000001A2FCB0099AFFF008BA2
      FE00214DFB004D71FC000E3DFB000030FB000031F7000636F1004C6EF100103C
      E3000432DB000636D700041CB500000000000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000061412100000000000000
      0000000000000000000046464600464646004646460046464600464646004646
      460046464600464646004646460046464600000000003A3A3A00000000000000
      000000000000F1F1F100000000006767670067676700F1F1F100000000000000
      000000000000000000000000000000000000000000000004A000415EEC00B8C7
      FF009CAFFD003A5CFC000A3AFB000335FB000335FB000133F900052FF2000635
      EB000537E900052CCD0000049C00000000000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000919191003A3A3A003A3A
      3A003A3A3A003A3A3A003A3A3A00676767006767670067676700000000000000
      00000000000000000000000000000000000000000000000000000309A5004260
      EC00A9BBFF00BDCAFF008EA5FE006483FD005073FC004A6EFD003961FD001444
      F900042CD7000109A20000000000000000000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000061412100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000676767006767670000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000004
      A0001E32CD005876F600859EFE008BA3FF007994FE005376FC00234AF000051E
      C50001049C000000000000000000000000000000000078685800614121006141
      2100614121006141210061412100614121006141210061412100614121000000
      0000000000000000000000000000000000000000000078685800614121006141
      2100614121006141210061412100614121000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000676767000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000004A0000917B6001022C3000D1FC2000311B40001059F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A4000000000000000000000000000000
      00000000000000000000CBBCB800A6857A00C9BAB400C8B8B000C8B6AC00C7B3
      A800C7B2A400A07E7300B68C7E009C776C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00C2A6A4000000000000000000000000000000
      00000000000000000000BC9B8F00BC938700FFFEFE00A8807400FBF5EF00FCF4
      EA00FBF1E200A8807400DCA49100AA7D700000000000000000002F2F2F002929
      2900242424001E1E1E0019191900141414003737370034343400000000000000
      0000B3B3B300B3B3B300B3B3B3000000000000000000000000003C3C3C003535
      35002F2F2F0029292900242424001E1E1E0019191900141414000F0F0F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00C2A6A4000000000000000000000000000000
      00000000000000000000C5A69B00BD958900000000009C786D00FCFAF700FEFA
      F500FDF6EC00A0776A00DEA79500AE8174000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B3B3B300E4E4E400B3B3B300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000744C4D00744C4D00744C4D00744C
      4D00744C4D00744C4D00744C4D00744C4D00744C4D00FEFBF700FEFBF700FEFB
      F700FEFBF700FEFBF700FEFBF700C2A6A4000000000000000000000000000000
      00000000000000000000C5A79C00BE988C00C9BBB600C9BBB600C9BBB600C9BA
      B400C9B8B10097746800D6A18F00B285780000000000000000003E3E3E003838
      3800323232002C2C2C0026262600202020001B1B1B0016161600282828000000
      0000B4B4B400E5E5E500B3B3B3000000000000000000000000004A4A4A004444
      44003E3E3E0038383800323232002C2C2C0026262600202020001B1B1B001616
      1600111111000C0C0C000000000000000000744C4D00FFFFFF00FDFBFF00C2E8
      C400D1E5D200E6E3E700D8DAD900D6D8D700744C4D00FEF9F400FEF9F400FEF9
      F400FEF9F400FEF9F400FEF9F400C2A6A4000000000000000000000000000000
      00000000000000000000C6A99D00EBBDAE00E9B8A900E6B4A500E4B0A000E1AB
      9B00DDA69600D9A19000D49B8A00B6897C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B6B6B600E6E6E600B3B3B300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000744C4D00E9FBEA008FD68E0019B9
      19005DC65C00DCDBD900DCD6D900D6D5D400744C4D00FEF7F000FEF7F000FEF7
      F000FEF7F000FEF7F000FEF7F000C2A6A400CBBCB800A6857B00C9BAB400C8B8
      B000C8B6AC00C7B4A800C6AA9D00C6A19500C9BAB500C8B9B200C8B7AF00C8B5
      AC00C7B4A800C7B2A500BA8E8100B88D800000000000000000004C4C4C004646
      4600404040003A3A3A00343434002E2E2E004C4C4C0048484800000000000000
      0000B8B8B800E8E8E800B5B5B500000000000000000000000000000000007975
      E9000000000000000000454545003F3F3F0039393900333333002D2D2D002929
      2900232323001E1E1E000000000000000000744C4D0065DB680000B2000000AE
      000005B50500A2D4A000E7DEE500DEDDDD00744C4D00FEF5EC00FEF5EC00FEF5
      EC00FEF5EC00FEF5EC00FEF5EC00C2A6A400BD9B9000BC94880000000000A880
      7400FCF5EF00FDF5EB00C7AB9F00C19F930000000000FFFEFC00FEFBF700FDF8
      F100FCF4EA00FBF1E200B88E8100BB9184000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BABABA00EBEBEB00B7B7B7000000000000000000E0E1F8007873E300817E
      EA00000000000000000066666600616161005C5C5C0057575700525252004D4D
      4D0048484800434343000000000000000000744C4D0063DD670017B917009FDB
      9E002FBF2E0022B72100C9DDC700ECE6EB00744C4D00FEF3E900FEF3E900FEF3
      E900FEF3E900FEF3E900FEF3E900C2A6A400C5A79B00BE978A00000000009C78
      6D00FCF9F700FEFAF400C7AB9F00C2A194000000000000000000FFFEFD00FEFC
      F800FDF9F200FDF5EB00BA918400BE93870000000000000000005A5A5A005454
      54004F4F4F0049494900434343003D3D3D0037373700313131003F3F3F000000
      0000BBBBBB00EDEDED00B9B9B90000000000E2E2F9008985E700938FE9009391
      EC007672E3000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000744C4D00E8FBE900C5E9C400FFF9
      FF00DBEDDB001CB91B0061C96100ECECEC00744C4D00FFF1E500FFF1E500FFF1
      E500FFF1E500FFF1E500FFF1E500C2A6A400C5A89C00BF998D00C9BBB600C9BB
      B600C9BBB600C9BAB400C8ACA100C3A29700000000000000000000000000FFFF
      FE00FEFCFA00FDF9F300BC938700BF968A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BCBCBC00EFEFEF00BBBBBB000000000000000000E3E2F9008A86E9009291
      EC0000000000000000005A5A5A00545454004E4E4E0049494900434343003D3D
      3D0037373700313131000000000000000000744C4D00FFFFFF00FFFEFF00F8F9
      F900FFF9FF00DCEFDB0027BD2800A6E0A600744C4D00FFF0E200FFF0E200DDCF
      C200DDCFC200DDCFC200DDCFC200C2A6A400C6A99D00ECC0B000EABBAC00E7B7
      A700E5B2A200E2AD9E00C8ADA100DCBCAF00C3A59900C2A29600C19F9300C19D
      9100C09B8F00BF998D00C0988B00C0998C000000000000000000666666006161
      61005C5C5C0057575700515151004B4B4B00646464005F5F5F00000000000000
      0000BCBCBC00F0F0F000BBBBBB00000000000000000000000000000000009898
      F000000000000000000075757500717171006C6C6C0067676700626262005D5D
      5D0058585800535353000000000000000000744C4D00FFFFFF00FFFFFF00FFFF
      FF00FEFFFE00FFFFFF00E3F5E400A9E8AB00744C4D00FFEEDE00FFEEDE00C5B5
      A900C3B4A800C2B3A700C1B2A600C2A6A400C7AA9E00C6A39600C9BAB500C9B9
      B200C8B7AF00C8B5AC00BCA39800C5AA9F00C4A89D00C5A99D00C4A89D00C4A8
      9C00C3A79B00C3A69A00C2A59900BAA198000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BCBCBC00F0F0F000BCBCBC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000744C4D00744C4D00744C4D00744C
      4D00744C4D00744C4D00744C4D00744C4D00744C4D00FFECDA00FFECDA00B0A2
      9600B0A29600B0A29600B0A29600C2A6A400C7AB9F00C2A0940000000000FFFE
      FD00FEFBF700FDF8F000FDF4E900FBF1E300B98F8200BC928500000000000000
      0000000000000000000000000000000000000000000000000000707070006C6C
      6C0068686800636363005E5E5E0059595900535353004E4E4E00595959000000
      0000BCBCBC00BCBCBC00BCBCBC00000000000000000000000000767676007474
      7400707070006C6C6C0068686800636363005E5E5E0059595900535353004E4E
      4E00484848004242420000000000000000000000000000000000000000000000
      0000C2A6A400FFEAD700FFEAD700FFEAD700FFEAD700FFEAD700C9B9AC00FBF8
      F400FBF8F400E6DAD900C2A6A40000000000C8ACA000C3A29600000000000000
      0000FFFEFD00FEFCF800FDF8F200FDF5EB00BB928500BE958800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FFE8D300FFE8D300FFE8D300FFE8D300FFE8D300C9B9AC00FBF8
      F400DFCEC700C2A6A4000000000000000000C8ADA100C3A49800000000000000
      000000000000FFFFFE00FEFCFA00FEF9F300BC948800C0978B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FFE6D000FFE6D000FFE6D000FFE6D000FFE6D000C9B9AC00DFCE
      C700C2A6A400000000000000000000000000C8AEA200DDBEB100C4A69A00C3A3
      9800C2A09400C19F9300C09C9000BF9A8E00C0998D00C19A8E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A40000000000000000000000000000000000BCA59D00C5AB9F00C5A99D00C5AA
      9E00C4A99D00C4A89C00C4A79B00C3A69B00C3A59A00BAA19800000000000000
      000000000000000000000000000000000000000000003E3E3E0017171700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000C4DFEF005EA7D30058A2D200BCDAED00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000069696900C9C9C900959595001616
      1600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000DCECF60077B7DC002C8EC8008FCDEB006FB7E200408EC8006BACD700D4E7
      F300000000000000000000000000000000000000000000000000898989008989
      8900898989008989890089898900898989008989890089898900898989008989
      8900898989008989890000000000000000000000000000000000D6820000D682
      0000D6820000D6820000D6820000D6820000D6820000D6820000D6820000D682
      0000D6820000D6820000000000000000000069696900DEDEDE00313131004545
      450015151500FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000EDF6FA0095C9
      E4003E9BCE0082C4E500CCF4FF00C4EFFF008BD2F1008ACEF0005FA4D7002F8B
      C60085BADD00E7F1F80000000000000000000000000089898900ECECEC00EAEA
      EA00E6E6E600E4E4E400E1E1E100DFDFDF00DDDDDD00DBDBDB00D9D9D900D7D7
      D700D6D6D600D4D4D400898989000000000000000000D6820000FFEEDA00FFEB
      D500FEE9CF00FEE6CB00FEE3C500FEE2C100FEDFBC00FFDDB800FEDBB400FEDA
      B100FED8AE00FED7AB00D682000000000000FFFFFF0043434300818181003535
      35003E3E3E0015151500FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FAFCFD00B1D9EB0054AAD4007DC0
      E000C7EEFC00CCF2FF00A8E8FF0094E0FE0041BAE70045B1E4008ACAEF0082C1
      EB005397D0003D92CA00A0C9E400F6FAFC000000000089898900EFEFEF00ECEC
      EC00EAEAEA00E7E7E700E4E4E400E1E1E100DFDFDF00DDDDDD00DBDBDB00D9D9
      D900D7D7D700D5D5D500898989000000000000000000D6820000FFF0E000FEED
      DA00FFEBD500FEE9D000FEE6CA00FEE4C500FEE2C100FEDFBD00FEDDB800FEDB
      B400FEDAB100FED8AD00D682000000000000FFFFFF00FFFFFF00474747008383
      8300373737003F3F3F0015151500FFFFFF00FFFFFF00FFFFFF00BAE9FA00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0072BDDE0077BDDC00BFE5F600DBF6
      FF00C1EEFF00A5E4FF009FE3FF0094E0FE0046C1EA003AB5E60037AAE20056AF
      E50087C6ED0074B3E4004A90CA0057A1D1000000000089898900F1F1F1008888
      8800888888008888880088888800888888008888880088888800888888008888
      880088888800D7D7D700898989000000000000000000D6820000FFF2E4000099
      0000009900000099000000990000009900000099000000990000009900000099
      000000990000FED9B000D682000000000000FFFFFF00FFFFFF00FFFFFF005858
      580084848400373737003F3F3F0015151500FFFFFF00C3EDFB0048C7F400FFFF
      FF00F9FDFE00B7E8FA00FFFFFF00FFFFFF0046AAD400E7FBFE00DDF6FF00C1EE
      FF00B7EBFF00ABE8FF00A4E4FF0096E1FE0048C6EB0040BDE9003DB4E60038A9
      E200329FDE006BB6E60083C4ED002A88C5000000000089898900F4F4F400F1F1
      F100EEEEEE00ECECEC00EAEAEA00E6E6E600E4E4E400E2E2E200DFDFDF00DDDD
      DD00DBDBDB00D9D9D900898989000000000000000000D6820000FEF5EA00FEF2
      E500FEF0DF00FFEEDA00FFEBD500FEE9CF00FEE6CB00FEE3C600FEE1C000FEDF
      BC00FEDDB800FEDBB400D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF004E4E4E00676767003737370040404000161616006CD4F60003B3F100C6EE
      FB0030BFF30096DEF900FFFFFF00FFFFFF004FAFD600E2F6FC00D4F3FF00C9F0
      FF00BEEDFF00B3EAFF00ADE7FF007CD9FE0048C7EF0043C4EA0043BEE8003FB5
      E6003AABE30040A7E10083C4ED00328EC8000000000089898900F6F6F6008888
      88008888880088888800888888008888880088888800E4E4E400E2E2E200DFDF
      DF00DDDDDD00DBDBDB00898989000000000000000000D6820000FFF7EE000099
      0000009900000099000000990000009900000099000000990000009900000099
      000000990000FEDDB800D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF004E4E4E006767670038383800424242004D4D4D004BCCF50019BD
      F2000DB7F200E1F5FD00FFFFFF00FFFFFF0051B1D700E2F6FC00D7F4FF00CEF2
      FF00C8EFFF00BAEBFF0092DBFB0056C1F10048C2F9003BBDF00047C5EC0045BD
      E90042B5E60047B1E60088CAEE003490C9000000000089898900F8F8F800F6F6
      F600F3F3F300F2F2F200EFEFEF00EDEDED00EAEAEA00E7E7E700E4E4E400E2E2
      E200DFDFDF00DDDDDD00898989000000000000000000D6820000FFF9F200FFF7
      EE00FEF5E900FFF3E500FFF0E000FFEEDB00FFEBD500FFE8D000FEE6CA00FEE4
      C600FFE1C000FEDFBC00D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00E3F7FD005BD1F6004F4F4F00676767008C8C8C00A9A9A9004197B40077D9
      F80003B5F10033C3F30080D9F800FFFFFF0053B4D800E2F6FC00DAF4FF00D5F3
      FF00BDEBFF0089D5F70069C9F5004CB4E9008DDAFB008CDCFF0048C4F90038B6
      EC0048BFE8004FBBE8008CD0F0003793CA000000000089898900FBFBFB008888
      8800888888008888880088888800888888008888880088888800888888008888
      880088888800DFDFDF00898989000000000000000000D6820000FFFBF7000099
      0000009900000099000000990000009900000099000000990000009900000099
      000000990000FEE1C000D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00ECF9FE0058D1F6004C4C4C00CACACA00F7F7F700D3D3D3004BA1
      BF0074D9F8000CB8F100C7EEFB00FFFFFF0055B6D900E2F8FD00D4F3FF00B0E4
      FA0086CFF1007FD0F50078D0F5004CB1E400B0E4FA00B6E9FF009BE1FF0078D6
      FE0040BDF5003DB5E90090D5F1003995CB000000000089898900FCFCFC00FAFA
      FA00F9F9F900F6F6F600F3F3F300F1F1F100EFEFEF00ECECEC00E9E9E900E6E6
      E600E4E4E400E2E2E200898989000000000000000000D6820000FFFCFA00FFFB
      F600FFF9F300FFF7ED00FEF4E900FFF2E400FFF0E000FEEEDB00FEEBD500FEE8
      CF00FEE6CB00FEE4C600D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0090E0FA0051CFF6004FA5C200F8F8F800FEFEFE005AB0
      CD006AD6F7002AC2F300E0F6FD00FFFFFF004FB5D800E1F8FE00CDEBF90092D2
      ED0084CCEB006FBFE50056B1DB003B94C800CEECFA00D9F5FF00B9EAFF0095DF
      FE0077D5FF00A5E4FF0084DCFB003293C9000000000089898900FDFDFD008888
      88008888880088888800888888008888880088888800EFEFEF00ECECEC00E9E9
      E900E7E7E700E4E4E400898989000000000000000000D6820000FFFDFC000099
      0000009900000099000000990000009900000099000000990000009900000099
      000000990000FFE6CA00D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00EBF9FE0061D2F70025C1F5004ECEF70083DDF8004EA4C2005BB1CF0068BF
      DC0041CAF60008BCF10068D5F600FFFFFF0091D2E7004DB5D900A5D9ED00D2EB
      F500BEDEED0095C9DE0089C3DB0070B8D60069B9DD0090D7F5007FCFF5009DDB
      F800AAE3FA0084CAEC0051A6D50079B9DD000000000089898900FFFFFF00FEFE
      FE00FCFCFC00FAFAFA00F8F8F800F6F6F600F4F4F400F2F2F200EFEFEF00ECEC
      EC00E9E9E900E7E7E700898989000000000000000000D6820000FFFFFF00FFFE
      FD00FFFDF900FFFBF600FFF9F200FFF7EE00FFF5E900FFF3E500FFEFE000FEEE
      DB00FEEBD500FEE8D000D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0095E1FA005AD0F7006DD7F8007DDCF8004BCD
      F60019BFF300F3FBFE00FFFFFF00FFFFFF0000000000CEEBF40078C6E2007EC6
      E000D1EEF700F6FFFF00F0FEFF00CBEDFB0050ADDA008BD7F700AAE1F90095D6
      F20062B2DB0063B2D800C2E0EF00000000000000000089898900FFFFFF008888
      8800888888008888880088888800888888008888880088888800888888008888
      880088888800EAEAEA00898989000000000000000000D6820000FFFFFF000099
      0000009900000099000000990000009900000099000000990000009900000099
      000000990000FFEBD500D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0053CFF60066D3F70066D3F70050CEF70098E2
      FA0026C2F300ACE7FA00FFFFFF00FFFFFF000000000000000000FBFDFE00B8E2
      EF0065BEDD0092CFE500E6F8FC00E3F6FE00AFDDF200B2E4F70072C0E10056AF
      D600ACD7EB00F8FBFD0000000000000000000000000089898900FFFFFF00FFFF
      FF00FFFFFF00FEFEFE00FCFCFC00FAFAFA00F8F8F800F6F6F600F4F4F400F2F2
      F200EFEFEF00ECECEC00898989000000000000000000D6820000FFFFFF00FFFF
      FF00FFFFFF00FFFDFD00FFFDF900FFFBF600FFF9F200FFF7EE00FFF5E900FFF3
      E500FFF0DF00FEEEDA00D682000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00E7F8FD00A8E6FA00FFFFFF00C1EDFC0052CEF600FFFF
      FF00E2F6FD00A4E6FA00FFFFFF00FFFFFF000000000000000000000000000000
      0000EFF8FB00A2D8EA0056B6D9009CD5EA0088CCE7004DAFD60098D0E700EBF6
      FA00000000000000000000000000000000000000000000000000898989008989
      8900898989008989890089898900898989008989890089898900898989008989
      8900898989008989890000000000000000000000000000000000D6820000D682
      0000D6820000D6820000D6820000D6820000D6820000D6820000D6820000D682
      0000D6820000D68200000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FCFE00ABE7FB00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000E0F2F80083CAE40079C4E100DBEFF700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000868484008684840000000000000000008684840086848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A4000000000076B1E5003E90DB00338B
      D900338BD900338BD900338BD900338BD900338BD900338BD900338BD900338A
      D900388EDA0085B9E800FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008684
      8400BCBABA00B5B3B300868484008684840086848400EBEAEA00868484008684
      8400868484000000000000000000000000000000000000000000000000000000
      0000C2A6A400FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00C2A6A4004799DC00DEF0FA00A7DDF4009EDB
      F40096DAF3008ED8F30086D7F3007FD4F20079D3F20072D2F1006CD0F10069CF
      F100C2EAF8003F94DB00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000086848400E3E2
      E200B5B3B300B5B3B300B5B3B300515050004F4F4F0086878700CDCDCD00E8E9
      E900C7C6C6008684840086848400868484000000000000000000000000000000
      0000C2A6A400FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00C2A6A4003B97DB00EFFAFE00A1E9F90091E5
      F80081E1F70072DEF60063DAF50054D7F40047D3F30039D0F2002ECDF10026CB
      F000CAF2FB003B97DB00FFFFFF00FFFFFF000000000000000000001B76000017
      6B0000176B0000176B0000186600001866000018660000186600001866000018
      6600001866000000000000000000000000000000000086848400DEDDDD00D6D6
      D600A4A3A300A4A3A300A4A3A300565555001615160012121200181818001212
      120093939300CACACA0086848400000000000000000000000000000000000000
      0000C2A6A400FEFBF700FEFBF700FEFBF700FEFBF700FEFBF700FEFBF700FEFB
      F700FEFBF700FEFBF700FEFBF700C2A6A4003B9CDB00F2FAFD00B3EDFA00A4E9
      F90095E6F80085E2F70081E1F7007AE0F7007CE0F70062DAF50054D6F30047D3
      F200E8F9FD003594DA00FFFFFF00FFFFFF000000000000218B005152CD00ACAF
      FF008C8EFF008C8EFF008C8EFF008C8EFF006568FF006568FF006568FF007D80
      FF005152CD0000186600000000000000000086848400D6D6D600D6D6D600A4A3
      A300E0DEDE00D9D7D700CDCBCB00C2C0C000B6B4B4009F9D9D00797677001212
      12001414140013131300868484008F8C8C000000000000000000000000000000
      0000C2A6A400FEF9F400FEF9F4000F76D900ABCBEB00FEF9F400FEF9F400FEF9
      F400FEF9F400FEF9F400FEF9F400C2A6A4003AA3DA00F6FCFE00C8F2FC00B9EF
      FB0094DFEF008CE4F80099CED30091D0D80082E1F7006DDDF60061DAF50057D7
      F400E7F8FD003594DA00FFFFFF00FFFFFF000000000000218B00EFF0FF000014
      FF000016FF00001BFF000020FF000027FF00002AFF000034FF000034FF000034
      FF007D80FF0000186600000000000000000086848400D6D6D600A4A3A300F3F2
      F200FFFEFE00FCFBFB00EAE7E800E6E6E600E6E5E500DAD9D900CCCBCB00BFBD
      BD00A2A0A00073717100939191008E8C8C00000000000F76D9000F76D9000F76
      D9000F76D9000F76D9000F76D900309DF6000F76D900ABCAE800FEF7F000FEF7
      F000FEF7F000FEF7F000FEF7F000C2A6A4003AA7DA00FEFFFF00F8FDFF00F6FD
      FF00F4F4F200E8FAFE00B6D7D800AAC7C50092D8E4007DE0F70072DDF60068DB
      F500E9F9FD003594DA00FFFFFF00FFFFFF000000000000259A00DADAFF00000F
      FF000014FF000016FF00001BFF000020FF00002AFF00002AFF000034FF000034
      FF006568FF0000186600000000000000000086848400A4A3A300FFFFFF00FEFD
      FD00FBFBFB00DFDEDF00ADA7A900B4ADAE00C3BDBE00D1CECF00E0E0E000E1E1
      E100D4D3D300C7C6C600A7A5A50086838300000000000F76D9003ABBFB0046C0
      FB004CC3FB0052C5FC0058C7FD005FCBFD0063CBFC000F76D900ABC9E500FEF5
      EC00FEF5EC00FEF5EC00FEF5EC00C2A6A40038ACDA00E8F6FB007EC5EA004AA3
      DF005E97C2004DA3DE00F2F1ED00F3EFEC00EDE5DF00EDEBE800F1F9FD00F0F9
      FD00FFFFFF003594DA00FFFFFF00FFFFFF0000000000002DB400DADAFF00000A
      FF00000FFF000014FF0000000000001BFF000027FF000027FF00002AFF000034
      FF006568FF00001866000000000000000000000000008684840086848400F1F0
      F000C2B9BA0093898B00A19B9F00ABA8AA00ABA6A700B1ACAD00AFA9AA00B2AD
      AE00CAC9C900DCDCDC00D0D0D00086848400000000000F76D9003DCAFA0047CC
      F9004ECDFA0060DCFC0076F2FE0081F4FF008DF9FF008BEEFD000F76D900ABC7
      E300FEF3E900FEF3E900FEF3E900C2A6A40040AEDB00F1FAFD0094DEF50093DC
      F400ACBFBF00BC9F900064A1CF003594DA003594DA003594DA003594DA003594
      DA003594DA003594DA00FFFFFF00FFFFFF0000000000002DB400DADAFF000006
      FF00000AFF00000FFF0000000000000000000020FF000027FF00002AFF00002A
      FF006568FF000018660000000000000000000000000000000000000000008684
      8400CAA09700BA9E8700A2897E0095817B00897C7F00928C9200A5A2A400BBB6
      B700D7D6D600CFCFCF008684840000000000000000000F76D9002494F4002A97
      F4002C9AF40033A0F60048BCFA005AD2FB0062D5FE000F76D900ABC6E100FFF1
      E500FFF1E500FFF1E500FFF1E500C2A6A40040B3DC00F7FCFE008EE4F80091DE
      F5009FE0F500C5C7C200DFA58300EDC8B300EDCDB800E9BEA300D58E6400EEFB
      FE00FAFDFF0057BCE000FFFFFF00FFFFFF00000000000036D800DADAFF000200
      FF000200FF000006FF00000000000000000000000000001BFF000020FF000027
      FF008C8EFF0000176B0000000000000000000000000000000000000000000000
      0000CC9A9900FFEAC400FFDDB300EEC39900D5AE8C00C9A78600CC9A99009895
      960086848400868484000000000000000000000000000F76D9000F76D9000F76
      D9000F76D9000F76D9000F76D9002B8EF4000F76D900ABC5DF00FFF0E200DDCF
      C200DDCFC200DDCFC200DDCFC200C2A6A4003BB5DB00FDFEFE00FEFFFF00FEFE
      FF00FDFEFF00FEFFFF00E7D6C900E0A98700EBC7B000DDA17C00BCA59500849E
      A60084B4C200A8C3CC00F3F3F300FFFFFF00000000000036D800DADAFF000700
      FF000200FF000200FF0000000000000000000014FF000016FF00001BFF000020
      FF008C8EFF0000176B0000000000000000000000000000000000000000000000
      0000CC9A9900FFE7C800FFDDBA00FFDBB100FFD9A600FFD39F00CC9A99000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FFEEDE00FFEEDE000F76D900ABC4DC00FFEEDE00FFEEDE00C5B5
      A900C3B4A800C2B3A700C1B2A600C2A6A40059C1E00061C3E10063C4E20063C4
      E20063C4E20062C4E20056BFDF00E7D4C500DDAC8F00C4AFA300D5D5D500BBBB
      BB00A6A6A600A0A0A00091919100CECECE00000000000038E400DADAFF000800
      FF000700FF000200FF00000000000006FF00000FFF000014FF000016FF00001B
      FF008C8EFF00001B76000000000000000000000000000000000000000000CE9D
      9B00FFEDDA00FFE7CE00FFE2C300FFDDB800FFDBAE00CC9A9900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FFECDA00FFECDA00FFECDA00FFECDA00FFECDA00FFECDA00B0A2
      9600B0A29600B0A29600B0A29600C2A6A400FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00ACACAC00CBCBCB00D2D2
      D200C9C9C900D2D2D200C6C6C60090909000000000000038E400DADAFF000D00
      FF000800FF000700FF000200FF000200FF00000AFF00000FFF000014FF000016
      FF008C8EFF00001B76000000000000000000000000000000000000000000CC9A
      9900FEF0E100FFECD800FFE6CC00FFE1C200FEDDB700CC9A9900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FFEAD700FFEAD700FFEAD700FFEAD700FFEAD700C9B9AC00FBF8
      F400FBF8F400E6DAD900C2A6A40000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DCDCDC00B2B2B200D6D6
      D600A0A0A00092929200C0C0C00089898900000000000038E400EFF0FF000D00
      FF000D00FF000800FF000700FF000200FF000006FF00000AFF00000FFF000014
      FF00ACAFFF0000218B0000000000000000000000000000000000CC9A9900FFFF
      F500FFFFF500FFF0E100FFEBD600FFE8CC00F6D4BA00CC9A9900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FFE8D300FFE8D300FFE8D300FFE8D300FFE8D300C9B9AC00FBF8
      F400DFCEC700C2A6A4000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DEDEDE00B5B5B500E6E6
      E6009A9A9A00B4B4B400B6B6B600BEBEBE00000000000038E4005152CD00EFF0
      FF00DADAFF00DADAFF00DADAFF00DADAFF00DADAFF00DADAFF00DADAFF00EFF0
      FF006568FF0000259A0000000000000000000000000000000000CC9A9900CC9A
      9900CC9A9900FCF3E900FCEADA00FCE5D000CC9A990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400FFE6D000FFE6D000FFE6D000FFE6D000FFE6D000C9B9AC00DFCE
      C700C2A6A400000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F4F4F400A6A6A600E1E1
      E100D2D2D200B8B8B800FFFFFF00FFFFFF0000000000000000000038E4000038
      E4000038E4000038E4000038E4000038E4000036D8000036D800002FC700002F
      C700002DB4000000000000000000000000000000000000000000000000000000
      000000000000CC9A9900CC9A9900CC9A99000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A40000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D9D9D900A5A5
      A5009D9D9D00C4C4C400FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000854200008240
      0000773A00006D35000069330000693400006934000069340000693400006A34
      00006A3400005A2B000000000000000000000000000000000000854200008240
      0000773A00006D35000069330000693400006934000069340000693400006A34
      00006A3400005A2B000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AB540000CB650000C561
      0000BC5D0000B5590000AF570000AD560000AE560000AE560000AE560000AE56
      0000B3590000954A00005A2C00000000000000000000AB540000CB650000C561
      0000BC5D0000B5590000AF570000AD560000AE560000AE560000AE560000AE56
      0000B3590000954A00005A2C00000000000000000000CB660000CB660000CB66
      0000CB660000CB660000CB660000CB660000CB660000CB660000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C7630000E9730000DD6D
      0000D66A0000D0670000C8630000C1600000BF5F0000BE5E0000BF5E0000BF5E
      0000C4610000B35900006A3400000000000000000000C7630000E9730000DD6D
      0000D66A0000D0670000C8630000C1600000BF5F0000BE5E0000BF5E0000BF5E
      0000C4610000B35900006A34000000000000CB660000FFF9F300FFF7EE00FFF4
      E900FFF1E300FFEEDD00FFEEDD00ADADE000A5B1DE00FFE6CC00CB6600000000
      00000000000000000000000000000000000000000000DD977700DA8F5D00DB90
      6000D98B5C00DE9E8900FAF3F100F3DFDA00D88D7800D37C5100D67E5100D57C
      4D00D4816600F0D8D300000000000000000000000000D1680000ED760200E271
      0200DC6E0200D5690000D98C3F00DA9A5A00D6985B00CF8C4B00C2702000BA5C
      0000BF5E0000AE5600006A3400000000000000000000D1680000ED760200E271
      0200DC6E0200DA7C2000DB934B00DB9A5B00D6975A00CC843F00B95C0000BA5C
      0000BF5E0000AE5600006A34000000000000CB660000FFFAF600FFF7EE00FFF4
      E900FFF1E300FFF0E100EFE6E5001C31D600003AEB0092A3DC00CB6600000000
      00000000000000000000000000000000000000000000FCF8F600F1D4C700E1A4
      7500DC946400F1D5CC000000000000000000FAF1EE00E1A39300DC966B00DB8F
      6000E9BFB500FEFDFD00000000000000000000000000D66A0000F6821100EE7B
      0B00E6750500DE6E0000EFC79F00FEFEFE00FEFEFE00FEFEFE00FEFEFE00D193
      5700BF5E0000AE560000693400000000000000000000D66A0000F6821100EE7B
      0B00EEA45A00FEFEFE00FEFEFE00FEFEFE00FEFEFE00E8C39F00BC5D0000BA5C
      0000BF5E0000AE5600006934000000000000CB660000FFFAF600CB660000CB66
      0000CB660000CB660000FFEFDF00ADADE000A5B1DE00FFE6CC00CB6600000000
      0000000000000000000000000000000000000000000000000000FEFEFE00E4AA
      8A00E2A97C00E0A38700000000000000000000000000DB927400DFA07700DA89
      5A00F5E5E10000000000000000000000000000000000D66A0000F8933000F485
      1900F07C0B00E8740200E1730700DD771400D7771800D2761D00E5B98D00FEFE
      FE00BF5E0000AE560000693400000000000000000000D66A0000F8933000F485
      1900FEFEFE00F4C08E00E37F1D00DD7A1800D6751400CD690700C5620000BD5E
      0000BF5E0000AE5600006934000000000000DF8D3A00FFFBF900FFF9F500FFF7
      F000FFF5EA00FFF3E400FFEFDF00FFECD900FFE9D200FFE6CC00CB6600000066
      000000660000006600000066000000000000000000000000000000000000FAF2
      EE00DD9B6B00E2A77900DE986900DD967500DC937200DB8E6100E3A88100D988
      5900F8EAE70000000000000000000000000000000000D66A0000F9A75600F48E
      2900F3811100F17C0700F7CA9E00E4710000DE6E0000D76A0000D2721400FEFE
      FE00C6620000B15700006A3400000000000000000000D66A0000F9A75600F48E
      2900FEFEFE00F2861A00EB740000E4710000DE6E0000EFC69E00CE660000C863
      0000C6620000B15700006A34000000000000F7BE8200FFFDFB00CB660000CB66
      0000CB660000CB660000CB660000FFEEDD00FFEBD600FFE8D000CB660000C7F1
      D500ADADE000A5B1DE00AAEABF00006600000000000000000000000000000000
      0000EDC9B600E0A06D00E09F7100EFCEBF00F5E3DC00DF9F8100E1A67A00D98C
      5B00FBF5F30000000000000000000000000000000000D66A0000F9B36F00F494
      3500F3841700F9C99900FEFEFE00EE760000E6720000DE6E0000E69F5B00FEFE
      FE00CF660000B85B0000733900000000000000000000D66A0000F9B36F00F494
      3500FEFEFE00F6AB6000F2780000EE760000E6720000FEFEFE00EFC29600D168
      0000CF660000B85B00007339000000000000FFCD9900FFFEFD00FFFCFA00FFFB
      F600FFF8F100FFF5EC00FFF3E700FFF0E100FFEDDA00FFEAD400CB660000EFE6
      E5001C31D600003AEB0092A3DC00006600000000000000000000000000000000
      0000FEFDFC00E4AA8100E0A26E00EBC1AD00FCF8F600E3AA9200E1A67900DC91
      6500FDFAF90000000000000000000000000000000000D66A0000FABA7B00F599
      3F00FBDBBB00FEFEFE00FEFEFE00FCECDC00FBE1C700F9E0C700FEFEFE00EEBD
      8C00D96B0000C2600000824000000000000000000000D66A0000FABA7B00F599
      3F00FACA9A00FEFEFE00FBE1C700FBE1C700FCECDC00FEFEFE00FEFEFE00F3D2
      B100D96B0000C2600000824000000000000000000000FFCD9900FFCD9900FFCD
      9900F7BD8100EBA66000DE8D3A00D3751700CB660000CB660000D1BB8600D5F4
      E000ADADE000A5B1DE00BBEECC00006600000000000000000000000000000000
      000000000000F8ECE400E1A67400E0A37200EFCDBC00E6B39900E2A97C00E19F
      7F000000000000000000000000000000000000000000D66A0000FABF8500F69F
      4B00FBD8B700FEFEFE00FEFEFE00FBDEC300F9CA9C00F9C99A00F2A45800E571
      0000E3700000CD650000904700000000000000000000D66A0000FABF8500F69F
      4B00F4933300F7B37100F9CDA100F9CB9D00FBDEC200FEFEFE00FEFEFE00F5CD
      A500E3700000CD65000090470000000000000000000000000000000000000000
      0000000000002DBF5900FCFEFC00F7FDF900F1FBF500EBFAF000E3F8EA00DDF7
      E500D5F4DE00CBF2D800C3F0D100006600000000000000000000000000000000
      00000000000000000000EAC0A400E2A87400E5AE8200E5AE8900E4AE8200E2A7
      88000000000000000000000000000000000000000000D66A0000FBC79400F7AB
      6100F5973B00FACDA100FEFEFE00F3861B00F3841700F37E0B00F2790200EE76
      0000EB750000D76A00009E4E00000000000000000000D66A0000FBC79400F7AB
      6100F5973B00F4933300F48B2400F3861B00F3841700FEFEFE00F9C18B00EE76
      0000EB750000D76A00009E4E0000000000000000000000000000000000000000
      00000000000033CC6500FFFFFF00006600000066000000660000006600000066
      0000DBF6E400D3F4DE00CBF2D700006600000000000000000000000000000000
      00000000000000000000FAF0E900E6B28300E4AD7A00E2AB7800E1A67500E4AD
      8B00FDF9F80000000000000000000000000000000000D66A0000FBC99800F9C4
      9000F8B77700F7AF6900FAD2AA00F59C4400F4933300F3841700F27C0800F379
      0200F5790000E2700000AB5400000000000000000000D66A0000FBC99800F9C4
      9000F8B77700F7AF6900F6A55600F59C4400F4933300F9C18A00F27C0800F379
      0200F5790000E2700000AB540000000000000000000000000000000000000000
      00000000000033CC6500FFFFFF00FFFFFF00FBFEFB00F5FCF700EFFCF300E9F9
      EF00E2F7E900DAF6E300D2F4DD00006600000000000000000000000000000000
      00000000000000000000F1D6C000E5B27F00E9BC9100E7BA8F00E7B78B00E2A5
      7300E1A27800FCF8F600000000000000000000000000D76A0000FBB97800FBCA
      9A00FBCC9E00FBC79400FABE8300F9B16A00F89E4600F78D2400F6800B00F67B
      0200FB7C0000EA740000B55A00000000000000000000D76A0000FBB97800FBCA
      9A00FBCC9E00FBC79400FABE8300F9B16A00F89E4600F78D2400F6800B00F67B
      0200FB7C0000EA740000B55A0000000000000000000000000000000000000000
      0000000000000000000033CC650033CC65002DC0590024AE4700199933000F85
      1E0006720C000066000000660000000000000000000000000000000000000000
      00000000000000000000F5E0CF00EAC29C00E9BD9500E9BC9500E8B99300E7B4
      8C00E7B39400FCF6F400FEFEFE00000000000000000000000000DA771700DE86
      3000DE883300DE883300DD842C00DC7F2400DA771700D8700B00D76C0400D66B
      0200D76B0000C963000000000000000000000000000000000000DA771700DE86
      3000DE883300DE883300DD842C00DC7F2400DA771700D8700B00D76C0400D66B
      0200D76B0000C963000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A667C00BE9596000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A1300000000000000000
      0000A1300000000000000000000000000000006D0300006D0300000000000000
      0000993300009933000099330000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EBF1F600CBDBE700A4C0
      D6007FA7C4006393B700155D93001D6296000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000A0A56001414
      A5002020B8001515870000000000000000006B9CC3001E89E8004B7AA300C896
      9300000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A1300000000000000000
      0000A130000000000000006D0300006D0300006D030029BA4400006D03000000
      0000993300000000000000000000993300009999990071717100545454005151
      51004F4F4F004C4C4C004A4A4A00474747004545450025679D003274A8003D7C
      AF004784B5004E8ABA003E7EAD00206498009C9C9C009A9A9A00969696009696
      96009696960096969600969696009696960096969600040471000606FE000606
      FF000606FF000606FF000B0BBE00000000004BB4FE0051B5FF002089E9004B7A
      A200C69592000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A1300000A1300000A130
      0000A130000000000000006D030058E0870058E087003CC95F001AAD2C00006D
      030099330000993300009933000099330000000000000000000058585800A2A2
      A200A2A2A200A3A3A300A4A4A400A4A4A400A5A5A5002F6FA50078ABD20078AB
      D30073A7D10069A0CD00407FAE0022669A00A4A4A30000000000F9F9F700F9F9
      F700F9F9F700F9F9F700F9F9F700F9F9F700060640000F0FB000FFFFF2000606
      FF007373FA00FFFFF2002E2EFA000C0C7F000000000051B7FE0051B3FF001D87
      E6004E7AA000CA97920000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A1300000000000000000
      0000A130000000000000006D0300006D0300006D03004CD77600006D03000000
      00009933000000000000000000009933000000000000000000005C5C5C00A0A0
      A0003C734000A2A2A200A3A3A300A3A3A300A4A4A4003674AA007DAFD4005B9A
      C9005495C7005896C8004180AE0026699C009F9F9F0000000000E1E1E000E1E1
      E000E1E1E000E1E1E000E1E1E000E1E1E0003D3D8F001A1AB2008686CB00FFFF
      F200FFFFE1001010DB000E0EDB000000B600000000000000000051B7FE004EB2
      FF001F89E6004E7BA200B9949700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A1300000A130
      000000000000000000000000000000000000006D0300006D0300000000000000
      0000993300009933000099330000000000000000000000000000606060003A77
      3F003D764100A1A1A100A2A2A200A2A2A200A3A3A3003D79B00082B3D700629F
      CC005A9AC9005E9BCA004381AF002B6DA000A0A0A00000000000E4E4E300E4E4
      E300E4E4E300E4E4E300E4E4E300E4E4E300434390003D3DB4003E3EB400FFFF
      F200FFFFF2007979E6003535CE001111AB0000000000000000000000000052B8
      FE004BB1FF002787D9005F6A760000000000B0857F00C09F9400C09F9600BC98
      8E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000039763E004D95
      540049915000286E2D00266A2A002366270021632500457EB40088B7D90067A3
      CF00619ECC00639FCC004583B1003170A400A1A0A00000000000E8E6E600E8E8
      E600E8E8E600E8E6E600E8E8E600E8E8E600252556004E4EB500FFFFF2008A89
      CB002F2EAC00FFFFF2007474D90024246C000000000000000000000000000000
      000055BDFF00B5D6ED00BF9D9200BB9B8C00E7DAC200FFFFE300FFFFE500FDFA
      DA00D8C3B300B58D850000000000000000000000000000000000000000004A66
      7C009C8993000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000059975F00569D5D0080C6
      88007BC3830077C17F0072BE79006FBC7500246728004C84BA008DBBDB006EA8
      D10066A6D1005FB4DF004785B1003675A800A1A1A10000000000EBE9E900EBE9
      E900EBE9E900E9EBE900E9EBE900EBEBE900EBEBE900393984004B4BB4003636
      AB002E2DAC004E4EC20030308F00000000000000000000000000000000000000
      00000000000000000000CEA79500FDEEBE00FFFFD800FFFFDA00FFFFDB00FFFF
      E600FFFFFB00EADDDC00AE837F00000000000000000000000000000000004AB3
      FF002B84D100A18A920000000000000000000000000000000000000000000000
      00000000000000000000000000000000000085B58A005EA566008BCC94007DC5
      860073C07C006EBD770069BB710075BF7C00276C2C005489BF0094BFDD0075AD
      D40063B8E1004BD4FF00428BB8003D79AD00A1A1A10000000000EDEDEC00EDED
      EC00EDEDEC00EDEDEC00EDEDEC00EDEDEC00EDEDEC00EDEFED00363668004746
      9300464692002525630000000000000000000000000000000000000000000000
      00000000000000000000C1A09100FBDCA800FEF7D000FFFFDB00FFFFE300FFFF
      F800FFFFFD00FFFFFD00C6A99C00000000000000000000000000000000000000
      000046B2FF002984D0009186900000000000A8787500D5B6A700D0A59F000000
      0000000000000000000000000000000000000000000060A067005FA667008DCD
      960089CB920084C88D0080C688007BC383002A7030005A8EC40098C3E0007CB3
      D70074AFD6005EC4ED004B88B300447EB100A1A1A10000000000F1F1EF00F1F1
      EF00EFF1EF00F1F1EF00F1F1EF00F1F1EF00F1F1EF00F2F2F10000000000A1A1
      A100000000000000000000000000000000000000000000000000000000000000
      000000000000C1A09100FEE3AC00F1C49100FCF2CA00FFFFDD00FFFFE400FFFF
      F700FFFFF700FFFFE900EEE5CB00B9948C000000000000000000000000000000
      0000000000006AC1FF008F8B9000CAB09A00FEFDDA00FEFDD900FFFFEF00B48D
      890000000000000000000000000000000000000000000000000047894F0060A7
      69005DA4650037823E00347E3B00317937002E7534006092C9009EC7E20083B8
      DA007DB4D7007EB3D7004F89B4004B84B700A0A0A00000000000F4F4F400F4F4
      F200F4F4F200F4F4F200F4F2F200F4F4F200F4F4F200F5F5F40000000000A1A1
      A100000000000000000000000000000000000000000000000000000000000000
      000000000000C2A19100FFE6AE00EEB58100F7DCAE00FEFDD800FFFFDF00FFFF
      E300FFFFE400FFFFE000F3ECD200BB968E000000000000000000000000000000
      000000000000A2ADC200EBC6A800FFF7CA00FFFFDE00FFFFE300FFFFF700FAF9
      E300C79C96000000000000000000000000000000000000000000777777004D90
      54003D8A45009B9B9B009C9C9C009D9D9D009D9D9D006696CC00A2CBE30089BD
      DC0083B9DA0084B9DA00518BB5005288BC009E9E9E0000000000F9F9F900F9F7
      F700F5F7F500F7F5F500F5F5F500F7F7F500F7F7F500F9F9F70000000000A1A1
      A100000000000000000000000000000000000000000000000000000000000000
      000000000000BC978C00FBE7B700F4C79100F2C99400F8E5B900FEFCD800FFFF
      DD00FFFFDC00FFFFE000E2D2BA00B68E86000000000000000000000000000000
      000000000000C6978F00F4D7A800F9E0B300FFFFDD00FFFFEF00FFFFFE00FAF9
      E300C79C960000000000000000000000000000000000000000007A7A7A009899
      9800529159009A9A9A009B9B9B009C9C9C009C9C9C006C9AD000A7CEE5008FC1
      DF0089BDDC008BBDDC00538DB600598EC1009D9D9D0000000000E6E6E600F9F9
      F900FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900FAFAFA0000000000A1A1
      A100000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D9C3A900FFFEE500F7DCB800F2C99400F5D4A500FAE8
      BD00FDF4C900FDFBD600B6908900000000000000000000000000000000000000
      000000000000E4C9B200F6D6A400F2C69100FEFBD300FFFFE500FFFFE700FFFF
      DE00C6AC9A0000000000000000000000000000000000000000007D7D7D009999
      9900999999009A9A9A009A9A9A009B9B9B009B9B9B006F9DD300AAD1E700ABD1
      E70098C7E10091C2DE00568FB7006092C600BABABA00B2B1B200B8B8B800B3B3
      B30000000000FCFDFC00FCFCFC00FCFCFC00FCFCFC00FDFDFD0000000000A1A1
      A100000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B58D8500E8DEDD00FFFEF200F9D8A300F4C48C00F9D4
      9F00FDEAB800D0B49F00B8908600000000000000000000000000000000000000
      000000000000CAA09800F6E2BA00F7D7AA00F5D6A600FCF0C900FFFFD800F8F5
      D100C8A298000000000000000000000000000000000000000000808080007E7E
      7E007C7C7C007A7A7A00777777007575750072727200719ED4006F9ED60087B2
      DC00ABD3E800A9D0E6005890B8006697CA0000000000BFBFBF00FCFCFC00C6C6
      C600000000000000000000000000000000000000000000000000000000009E9E
      9E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AD827F00C9AA9E00EFE0B700EFDFB200E7CE
      AC00B8908600B890860000000000000000000000000000000000000000000000
      00000000000000000000DCC4B400F8F6F200FBDDAC00F8D29B00FBE8B900BE9D
      8F00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000084AB
      DB006D9CD40085B1DA005A91B9006D9BCF000000000000000000BFBFBF00B8B8
      B800000000000000000000000000000000000000000000000000000000009090
      9000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BA968A00BB988C00B791
      8800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CAAD9600DDBFA900DCB8A800CAAD96000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B0C9E7006C9BD200709DD200000000000000000000000000B0B0
      B000909090009090900090909000909090009090900090909000909090009696
      96000000000000000000000000000000000000000000C5AA9400B8967B00AF89
      6B00A77D5C00A0724E0098674100925E3500956139009A694100A3744F00AD82
      5F00A0734F00E8DDD400FFFFFF00FFFFFF00000000005F6A6100636F6400646F
      6400646F6500646F6500646F6600646F6600646F6600646F6600646F6600646F
      6700646F670064706700616C63007E857F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D3BDAB00B8906D00D6BAA300DFC6
      B300E7D4C300EEDFD300F5EAE200FBF4EF00FDFAF600FFFEFD00FBEBDF00FBEF
      E600C19D8000D0B9A600FFFFFF00FFFFFF005F6A6000EBF5EC00D4EDD700D4EE
      D700D4EED800D5EED800D5EED900D5EED900D6EFDA00D6EFDA00D4E2EC00CFE5
      D600D5EDD900D8EFDC00D5EDD900616C63000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C8AB9300C7A48500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFEFC00FEFBF700FEF7F100FEF6F100E4AE8600FAE8
      DB00CEAF9500C7AA9100FFFFFF00FFFFFF00626E6400EEF8EF00A4DBBC008CCA
      A60098D5B10086C9A10074ADC0005687C60083C2B60077ACAF006970EB0081C5
      A3008CD0A60085CAA000D2E9D700646F670000000000000000003C3C3C003535
      35002F2F2F0029292900242424001E1E1E0019191900141414000F0F0F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DDCBBB00A16E3F00B4855900D9A5
      7B00D89E6F00D79B6A00D8966300D6925D00D48F5900D38E5700E29D6900FAE3
      D100D8BBA200C2A28400FFFFFF00FFFFFF00616E6400ECF7EE0096DBAF007FC9
      9A008FD8AA007AC897008AD6A9005FA2AC005588D700485DE00079A8C9007FC9
      990087D0A00080CA9A00D6EEDA00646F66000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7F3EF00C19E7C00D5AE8C00FDF0
      E500F7C7A200F7CFAD00F9D5B500F8DABD00F8DEC200FAE1C600FAE4CC00FDF5
      EC00E2CEBA00BF997600FFFFFF00FFFFFF00616E6300F7FBF8009BDEC40073C3
      930080CF9F006FC18F007ECE9D006EBF8C0070A4BB004858DA004C7CD100518D
      B60073BE9D0074C38F00D7EFDA00646F660000000000000000004A4A4A004444
      44003E3E3E0038383800323232002C2C2C0026262600202020001B1B1B001616
      1600111111000C0C0C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00F1E9E100B7865600FEFE
      FD00FADEC200FADCBF00F9DBC000F9DBC000F9DDC100FADBC000FADCC300FDEB
      DE00ECDCCD00BB916700FFFFFF00FFFFFF00616E6300F8FCF900BCFBFB009DE7
      DF0093E1BB0077C9970089DAA90075C69800717CF3006BAEAD0082E1DD0058A7
      E100568CE600619FBD00D0E9DB00646F66000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E9F5EA0096CA9B004EA056004C99530090B89500E8EEE9000000
      000000000000000000000000000000000000FFFFFF00F5EFE800B9865200FEFC
      F900F9DCBF00F8DBBF00F8DCC000F9DBC200F9DCC100F9DDC200FADDC300FBE7
      D400F5EDE200B9885600FFFFFF00FFFFFF00606D6300F8FCF800A4EBED008DDF
      DF0097EBEB0072CFB70074CA99005683B0006078D70051C3B60090D5CD0050C6
      C6007ED0DD006499DB00CAE4DC00646F66000000000000000000E9AB7B000000
      00000000000000000000454545003F3F3F0039393900333333002D2D2D002929
      2900232323001E1E1E0000000000000000000000000000000000000000000000
      00000000000099D29C0091CB9500A1D4A30099D09B008CC2900091BA96000000
      000000000000000000000000000000000000FFFFFF00FCFAF800B9854B00FEFB
      F700F9DCC100F8DCBF00F8DCBF00F8DBC000F9DDC000F9DDC000F9DDC300FBE2
      CB00FCF9F500B8834900FEFEFE00FFFFFF00606D6200F8FCF800AFFAFA0094EB
      EB00A2F9FA008AEAEB0095EDF300595FEB0087D9E6005CD2D100DCDBC900ACD4
      CD0097DAD40077D1CE00D4EFDB00646F65000000000000000000EBB18300E3A2
      7000F7E6DE000000000066666600616161005C5C5C0057575700525252004D4D
      4D00484848004343430000000000000000000000000000000000000000000000
      00000000000057BD5D00B4DCB5008ACC8B0083CA85009AD09D004D9D55000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00BE8A4F00FCF6
      F000F9DFC700F9DCBD00FADCBF00FADBC100FADDC300FADDC200F9DDC400FBE0
      C900FFFCFA00C18D5300FBF8F400FFFFFF00606D6100F8FCF8009FF1F10081DD
      DF008AEAEB0075DEDE006591EE00557EE20068DCDD0077BFB700E5C8B200DABA
      A300DEBCA600D9B79F00D6EDD800646F640000000000E7B28300EDBD9700E8B4
      8900E3A67600F7E7DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000059C25E00C0E1C00095D2950088CC8900A2D4A40050A557000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00CC9E6700F5E7
      D800FAE5D200F9DABC00F9DBBC00FADBBF00FADDC100FADDC100F9DDC400FBE1
      C800FFFDFB00C8945700FAF7F200FFFFFF005F6D6100F8FCF800A6F9F9008BE9
      EA0099F8FA0078D3EB00656BFF0077DCEA0070DFDE00C1C5B500F1D1BA00E5C1
      A900EAC7AE00E4BDA100D6EDD700636F64000000000000000000EDBB9500E7B0
      8000F8E9E000000000005A5A5A00545454004E4E4E0049494900434343003D3D
      3D00373737003131310000000000000000000000000000000000000000000000
      0000000000009CDC9F0097D69B00C1E2C200BADFBB0092CC950097CD9B000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00D8B08000F0D9
      C100FBEDE100F9DAC000F9DCC200F9DEC500FAE0C700FAE2CA00FAE2CD00FAE5
      D000FFFEFD00CB8F5A00CC995B00F0E2D0005F6D6100F8FCF80090EAEA0078DD
      DE0081E9EA004E6BE200639DEE005ED7D7005BCBC900D4B09700E2BA9F00D6AC
      8F00DBB09100D4A78600D6EDD700636E64000000000000000000F0BF9C000000
      0000000000000000000075757500717171006C6C6C0067676700626262005D5D
      5D00585858005353530000000000000000000000000000000000000000000000
      000000000000EAF7EB009CDC9F0059C25E0058BF5E009AD59D00EAF5EB000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00E0BE9100EDD0
      B200FFF6F000FAE1CA00FBE3CC00FBE3D000FBE6D300FBE9D500FCE9D800FCEA
      DB00FFFFFD00D29D7100EED9C100D4A466005F6D6100F7FCF8009FF9F90085E9
      EA0084D3FA00525AF00087F2F70060DAD70098D5CA00E0B79C00EDC7A900E0B3
      9400E6B89800DEAE8C00D7ECD600636E64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00E6C89F00EBCA
      A500FFFDFB00FDE9D500FDEBD800FDEADB00FDEDDF00FDF0E200FDF1E400FCF0
      E400FFFFFF00E0A07000FFFBF900DFB887005F6D6000F7FCF8008AEAEA0072DD
      DE005665F000569FDF0073E8E80046C1C100BBBBA500D0A48300DEB08E00D19E
      7A00D6A27A00CF987100D7EBD500626E64000000000000000000767676007474
      7400707070006C6C6C0068686800636363005E5E5E0059595900535353004E4E
      4E00484848004242420000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00EBCFAA00EBC6
      9A00FFFFFF00FCEFE200FDF0E700FDF1EB00FDF5EE00FDF8F100FDFAF700FFFC
      FA00FFFFFF00FEFBF700F4DAC000DDAB68005F6D6000F7FCF8009DF9F9006CB4
      ED006271FE0080E7E9008CF4F40052CDCD00ECC2A400DDAD8A00EBBA9700DDA7
      8000E2AB8300DAA07500D9EAD400616E64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00F0D9B900EAC0
      8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDF9F400FBF3EA00F8EBD900F8E6
      D300F5DFC600E9CBA600E0AE6800F2DFC3005C6A5D00FBFCFB00FCFEFC00F7FC
      F800F7FCF800F7FCF800F8FCF800F7FCF900F8FCF800F8FCF800ECF7EE00EDF7
      EE00EFF6ED00EEF4EC00EBF4EB005E6A5F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00F8EDDD00E8BE
      8200EABC8100E8B77700E6B26D00E4B06800E3B06800E5B57000E7BB7D00E7BC
      7F00E9C28900EBC89500F6E6CF00FEFDFB00788078005C6A5D005F6D60005F6D
      60005F6D60005F6D60005F6D61005F6D61005F6D61005F6D6100606D6100606D
      6200606D6200606D63005E6A5F00788079000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008F8E8D009D9C9B009D9C9B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008888
      8A0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00DCFCFF00A4F5FF009CF3FF006FEFFF00ECFDFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00009D9C9B00BDBCBC00EBEBEB00D2D2D1009D9C9B009D9C9B009D9C9B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000AE8A7F00AE8A7F00AE8A7F00AE8A7F00AE8A7F00AE8A7F00AE8A7F006F6F
      71009E9FA000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FCFFFF00A6F4FF00A0F5FF0062EDFF00E2FBFF0096F3FF0009E3FF00D6FA
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000009D9C
      9B00C4C4C400FFFFFF00FAFAFA00EDEDED00F2EDEA0072B17200148314003483
      340080907F009D9C9B000000000000000000A4787400A4787400A4787400A478
      7400A4787400A4787400A4787400A4787400A4787400A4787400A4787400A478
      7400A4787400A4787400A4787400A47874000000000000000000000000000000
      0000AE8A7F00FFFFFF009A99EE004757B900D3D0BC00D5CCD2004757B9004E4E
      4E006B67660088888A000000000000000000FFFFFF00FFFFFF00FFFFFF00D8FC
      FF008EEDFE0036E0FE00ADF2FF0050E6FF00C3F7FF004EE6FE0000D9FE0030E0
      FE00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000009D9C9B00D1D0
      D000FFFFFF00FFFFFF00FBFBFB00F0F0F000F7F1EE0075B47500048204000482
      04009DBC9D00C7C3C1009D9C9B0079797800A4787400FEF4EA00FEF1E400FDEE
      DF00FDEBD900FDE8D300FDE5CD00FDE2C700FDDEC100FDDBBB00FDD9B500FDD6
      B100FCD4AB00FCD2A800FCD0A400A47874000000000000000000000000000000
      0000AE8A7F00F8FBFF004757B900E7D5C7004757B9004757B9004A697B008A88
      AF00BCADA7004757B9000000000000000000FFFFFF00FFFFFF00FFFFFF0078EB
      FE0010C5FB007DE7FE00A8EBFD0008C9FB0099F1FF008FEFFF0003C8FB0022CF
      FC00C4F2FE00FFFFFF00FFFFFF00FFFFFF00000000009D9C9B00E0E0E000FFFF
      FF00FFFFFF00FFFFFF00FDFDFD00F6F6F600FDF7F40078B77800048204000482
      0400A1C1A100DBD8D600C3C3C30077777600A4787400FDF5EE00FDF3E900FEF1
      E300FEEEDE00FDEBD800FDE8D200FDE4CB00FDE1C500FDDEBF00FCDBBA00FCD8
      B400FCD6B000FCD4AB00FCD1A700A47874000000000000000000000000000000
      0000AE8A7F00D7C7D6004757B9004757B900ADA3E3009FB1720062954700908F
      DD00EBD8D7004757B9000000000000000000FFFFFF00FFFFFF00C9F1FE003FCE
      FB001FB0F50066EAFF007FD7FA0048C8F9002EC8FA0072EFFF0008B6F7009ADE
      FB00AFE8FD00FFFFFF00FFFFFF00FFFFFF00000000009D9C9B00FDFDFD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FCFCFC00FFFEFB007BBA7B00048204000482
      0400A6C5A600DEDBDA00C4C4C30077777600A4787400FEF8F200FDF5ED00FEF2
      E800DF993E00DF993E00DF993E00DF993E00DF993E00DF993E00DF993E00FDDB
      B900FCD8B400FCD5AF00FCD3AA00A47874000000000000000000000000000000
      000005446900054469004757B900D3C5E200D7CEA1008ABB6E0086A16C00806F
      C0004757B900AE8A7F000000000000000000FFFFFF00FFFFFF0079CAF8005FBD
      F7002592F0004BF1FF0007A2F300DBEFFD001D9CF10029C4F90010A9F40062B9
      F600FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000009D9C9B00FFFEFF00FFFF
      FF00B2C6B1007FA17F00D2DDD200FFFFFF00FFFFFF007EBD7E00048204000482
      0400ABCAAB00E5E2E100C8C9C8007B787A00A4787400FEF9F500FEF7F100FDF5
      EC00FEF2E700FDF0E200FEEDDB00FEEAD600FDE6D000FDE3CA00FCE0C400FCDD
      BE00FDDAB800FDD7B300FCD5AE00A47874000000000000000000000000000544
      69000544690000537C001C6A8D006D93BB002D6EB400286FBE003869A4005054
      BC00AE8A7F00000000000000000000000000FFFFFF00FFFFFF0085B8F50074AB
      F300286DEA0056E0FD0000C7FB004584ED00AACDF8001373EB000C85EE005E9F
      F200FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000009D9C9B00EBEEEA006790
      65000B711200006B050016601500CBD9CB00FFFFFF00DDEDDD007EBD7E004EA5
      4E00BED5BE00E9E7E600D8D2D8004F674F00A4787400FEFBF800FEF9F500FEF7
      F000DF993E00DF993E00DF993E00DF993E00DF993E00DF993E00DF993E00FDE0
      C300FDDDBC00FDDAB700FCD7B200A47874000000000000000000000000000000
      0000054469000252770009639800227DE6002988F4002980ED002174E2001756
      A70005446900054469000000000000000000FFFFFF00FFFFFF00E0E7FC005D72
      EC006578EC002E7BEE0012E0FF00079AF500999CF100416DEA001448E4007897
      F000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000009D9C9B00256A25001792
      280043DE660037CD5300037307001A5F1900CCD9CD00FFFFFF00FFFFFF00F9F8
      F700EEEFEE00F2F0F200D0D0D00024582400A4787400FEFCFB00FDFBF800FDF9
      F400FDF6EF00FDF4EB00FEF1E500FDEFE000FDEBDA00FDE8D300FDE5CD00FDE2
      C800FDDFC100FCDCBB00FCD9B600A47874000000000000000000000000000000
      000005446900015176002177B8003BA5FF002F99FB002B92F5002B91FC001F7C
      E60000528400000000000000000000000000FFFFFF00FFFFFF00FEFEFF00716B
      EC00D2D0F9001718E2001FB6F90000B6FB002E58EB00AEAEF5000A0BDF00ABAC
      F400FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000217729003BCA5D0054EE
      7E0047DF6A003DD75C002DC2450005710900175E1600CBD8CB00FFFFFF00FDFD
      FD00FFFFFF00C5D0C5002F6D2F0000580000A4787400FEFDFC00FEFCFA00FEFA
      F700FEF8F300FDF6EE00FDF4E900FEF0E400FDEEDE00FDEBD800FDEDDC00B170
      4900BB6C3B00BB693400BB693400A47874000000000000000000000000000000
      000005446900055076003387CC004D9AD7009B908400A3A2A000608BB5005D85
      B8003C5E7B00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00E8E7
      FC007771ED009991F1001442E80002B9F9000156EA00B9B6F6002F24E300D5D3
      FA00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000E7E140065FF970050E8
      790046DE69003CD45A0034CE4E0025BA3700006E02001B5F1A00CEDBCE00FFFF
      FF0099B499000B560B00005C000000000000A4787400FEFDFC00FEFDFC00FEFC
      FA00FEFAF700FEF8F200FEF6ED00FDF3E800FDF0E300FDEEDD00FDEFE0009F71
      540081CEE600DE9C5500DEA27400A47874000000000000000000000000000000
      00000544690008537A00367CB100517FA000D5A16A00F1C99A00896C5400D9A7
      7400D4A67800000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F1F0FD00B3AFF5005F53E900123DE800077AF0006965EC00908BF000FCFC
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000E7E14004AD96E0053ED
      7E0046DE69003CD45A0032CB4C0028C23C001DB12B00006E0300206520005B87
      5B0000510000006300000000000000000000A4787400FEFDFC00FEFDFC00FEFD
      FC00FEFCF900FEF9F600FEF8F100FEF6ED00FDF2E800FEEFE200FDF1E4009E7E
      700099CDFE0099B0C700C7C2C700A47874000000000000000000000000000000
      00000544690004547B003985BC004FBBFF0053A5E000579FD30045A0E8005B9A
      D3003E667E00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F9F8FE003D34E6000B3BE9005150E900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000E7E140047DB
      6C0048E26D003CD45A0033CB4C0037CE530053ED7C0013A91C00007000000053
      000000650000000000000000000000000000A4787400FEFDFC00FEFDFC00FEFD
      FC00FEFDFC00FEFCF900FEFAF500FEF7F000FEF5EC00FDF2E700FDF2E700C7B7
      B300C4C3C800C9C3C700C7B7B400A47874000000000000000000000000000000
      000005446900036B9B0019628B00499DDA004DBCFF0049B9FF004AB7FF002B92
      E30000548200000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00EAE9FC001A20E4007584F100FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000E7E
      14003FD25E003ED85C0036CF510053ED7C0053ED7C000BA21000057E08000000
      000000000000000000000000000000000000A4787400A4787400A4787400A478
      7400A4787400A4787400A4787400A4787400A4787400A4787400A4787400A478
      7400A4787400A4787400A4787400A47874000000000000000000000000000000
      000000000000054469000480B500166D9800337CAE003F94D2003287C5000977
      A80005446900000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FDFDFF005F55EB00A0B5F600FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000E7E140035C8500035CE4F000E7E14000B971000068708000E7E14000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000005446900054469000544690005446900054469000544
      690000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FEFEFF009B95F200DCE8FD00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000E7E14000E7E140000000000035E04000E7E1400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FEFEFF00E6E5FC00FAFCFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A4000000000000000000000000000000
      00000000000000000000A5420800A7440700A23F080000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A4787400A4787400A4787400A4787400A4787400A4787400A478
      7400A4787400A47874008C5D5C0000000000000000000000000000669A000066
      9A0000669A00A4787400A4787400A4787400A4787400A4787400A4787400A478
      7400A4787400A47874008C5D5C00000000000000000000000000000000000000
      0000C2A6A400FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00C2A6A4000000000000000000000000000000
      000000000000A23F0800A5420800A23F0800A542080000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A87C7500FEE5CB00FFE2C400FFDFBE00FFDCB800FFD9B100FED6
      AC00FFD4A600FFD1A2008C5D5C00000000000000000000669A003CBEE30036BA
      E10030B6DF00B7818300FCEDDD00FAF2E400F8F1E000F7EEDD00F7EEDB00F7ED
      DB00F7ECDA00F8EDD9008C5D5C000000000000000000000000000016C5000016
      C5000016C5000016C5001019B1007F7EC900FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00C2A6A4000000000000000000000000000000
      000000000000A74407000000000000000000A23F0800A23F080000000000A23F
      0800A74407000000000000000000000000000000000000000000000000000000
      000000000000AD807800FFEAD400E5A65700E5A65700E5A65700E5A65700E5A6
      5700E5A65700FFD4A8008C5D5C00000000000000000000669A0045C4E6003FC0
      E40038BCE200B7818300F6E4DA00E4A85C00E4A75900E3A75900E3A75900E3A6
      5800E3A75900F0E3D0008C5D5C0000000000000000000016C5001D37D8002550
      FF000133FF000133FF001140FF002039D8003032B000FEFBF700FEFBF700FEFB
      F700FEFBF700FEFBF700FEFBF700C2A6A4000000000000000000000000000000
      000000000000A74407000000000000000000A23F080000000000A23F0800A23F
      0800A23F0800A542080000000000000000000000000000000000000000000000
      000000000000B4867A00FEEEDD00FFEBD600FFE8CF00FFE4C900FEE1C200FEDD
      BB00FFDBB500FFD8AF008C5D5C00000000000000000000669A004DC9E90047C4
      E70041C0E500B9848400F8E8DF00F9E4CE00F9DBBD00F9DBBD00F9DBBD00F8D9
      B800F5DDC200F0E4D2008C5D5C0000000000000000000016C500D7DFFF00F0F3
      FF00375FFF001241FF00CFD9FF00EFF2FF002039D8007F7CC500FEF9F400FEF9
      F400FEF9F400FEF9F400FEF9F400C2A6A4000000000000000000000000000000
      000000000000A5420800A23F0800A23F0800A23F080000000000A23F08000000
      000000000000A64307000000000000000000A4787400A4787400A4787400A478
      7400A4787400BA8D7D00FEF2E500E5A65700E5A65700E5A65700E5A65700E5A6
      5700E5A65700FEDCB7008C5D5C00000000000000000000669A0056CDED0050C9
      EA004AC5E800BC878500F9EBE400E4A85C00E4A75800E4A75800E4A75800E4A6
      5700E3A75900F2E6D6008C5D5C00000000000016C5005377FF00BECBFF00FFFF
      FF00F0F3FF00E1E7FF00FFFFFF00EFF2FF00214DFF001019B000FEF7F000FEF7
      F000FEF7F000FEF7F000FEF7F000C2A6A4000000000000000000000000000000
      00000000000000000000A23F0800A7440700A5420800A23F0800A54208000000
      000000000000A23F08000000000000000000A87C7500FEE5CB00FFE2C400FFDF
      BE00FFDCB800C2958100FEF6EC00FEF3E600FEEFE100FFEDDA00FEE9D400FEE6
      CC00FFE2C600FEDFBF008C5D5C00000000000000000000669A005ED2F00058CF
      ED0052CBEB00C08B8500FAEFE900FAEDDE00FAE5D000F9E5CF00F9E3CD00F8E1
      CA00F5E4D000F3E8DB008C5D5C00000000000016C5007592FF006988FF00C3CF
      FF00FFFFFF00FFFFFF00F1F4FF00365EFF000133FF000016C500FEF5EC00FEF5
      EC00FEF5EC00FEF5EC00FEF5EC00C2A6A4000000000000000000000000000000
      00000000000000000000000000009E410D00A14E2200A23F0800A7440700A542
      0800A5420800A23F08000000000000000000AD807800FFEAD400E5A65700E5A6
      5700E5A65700CA9B8300FFF9F300E5A65700E5A65700E5A65700E5A65700E5A6
      5700E5A65700FEE3C8008C5D5C00000000000000000000669A0066D7F30060D4
      F1005AD0EE00C4908600FCF4F000E5A95C00E5A65700E5A65700E4A65700E4A6
      5600E4A75A00F7EEE3008C5D5C00000000000016C5009AAFFF008DA5FF00E4E9
      FF00FFFFFF00FFFFFF00F1F4FF00456BFF000B3CFF000117C500FEF3E900FEF3
      E900FEF3E900FEF3E900FEF3E900C2A6A4000000000000000000000000000000
      0000000000000000000000000000957D75008D766C00A55E3900A23F0800A23F
      0800A23F0800000000000000000000000000B4867A00FEEEDD00FFEBD600FFE8
      CF00FFE4C900D1A28600FEFBF900FEF9F400FEF7EF00FEF5EA00FEF1E400FEEE
      DE00FEEBD700FEE8D0008C5D5C00000000000000000000669A006DDBF60067D8
      F30062D4F200C8948800FEF7F400FEF7F000FBF0E500FBEFE300FAEDE000FAEE
      E100F9F0E500E8E3DD008C5D5C00000000000016C500ABBDFF00EDF1FF00FFFF
      FF00F7F9FF00D2DCFF00FFFFFF00F2F4FF003E65FF00141CB000FFF1E500FFF1
      E500FFF1E500FFF1E500FFF1E500C2A6A4000000000000000000000000000000
      000000000000000000008E7C7200AC928E008E7C72008E7C7200000000000000
      000000000000000000000000000000000000BA8D7D00FEF2E500E5A65700E5A6
      5700E5A65700D8A98A00FEFEFD00FEFCFA00FEFAF600FEF8F100FEF5EC00EBDF
      DB00D3C2C000BAA9AA008C5D5C00000000000000000000669A0074DFF8006FDC
      F6006ADAF400CC998900FEF8F500FFFFFF00FEFFFE00FCFAF900FBFBF900B481
      7600B4817600B4817600B481760000000000000000000016C500E3E9FF00FAFB
      FF00ABBCFF00819CFF00CCD7FF00E0E6FF00344AD8007F78BC00FFF0E200DDCF
      C200DDCFC200DDCFC200DDCFC200C2A6A4000000000000000000000000000000
      0000000000008E7C7200D3BDBD008E7C7200AD938F008E7C7200000000000000
      000000000000000000000000000000000000C2958100FEF6EC00FEF3E600FEEF
      E100FFEDDA00DFB08D00FEFEFE00FEFEFE00FEFCFB00FEFBF700FEF8F200B481
      7600B4817600B4817600B17F7400000000000000000000669A007AE3FA0076E1
      F80070DDF600D09C8900FFFAF800FFFFFF00FFFFFF00FFFFFF00FFFFFF00B481
      7600E0A87000F7935B00FF00FE0000000000000000000016C500747ED800B4C4
      FF00ABBDFF009EB2FF008AA2FF00485AD8003635AB00FFEEDE00FFEEDE00C5B5
      A900C3B4A800C2B3A700C1B2A600C2A6A4000000000000000000000000000000
      00008E7C7200E9DEDE008E7C7200A9958F00D2BABA008E7C7200000000000000
      000000000000000000000000000000000000CA9B8300FFF9F300E5A65700E5A6
      5700E5A65700E4B58E00FEFEFE00FEFEFE00FEFEFE00FEFDFC00FEFBF800B481
      7600EBB56F00E49B420000000000000000000000000000669A007FE6FC007BE4
      FA0077E1F900D29F8A00DCA88700DCA88700DCA88700DCA88700DCA88700B481
      7600C7AF890000669A00000000000000000000000000000000000016C5000016
      C5004F56C5004C53C5002E31AF007F76B800FFECDA00FFECDA00FFECDA00B0A2
      9600B0A29600B0A29600B0A29600C2A6A4000000000000000000000000008E7C
      7200FCFAFA008E7C7200000000008E7C7200DFCECE008E7C7200000000000000
      000000000000000000000000000000000000D1A28600FEFBF900FEF9F400FEF7
      EF00FEF5EA00E8B89000DCA88700DCA88700DCA88700DCA88700DCA88700B481
      7600F0B25E000000000000000000000000000000000000669A0083E8FE0080E6
      FC007DE5FC007DE5FC0078E2FA0072DFF8006BDAF50064D5F2005DD0EF0056CD
      ED0052CAEB0000669A0000000000000000000000000000000000000000000000
      0000C2A6A400FFEAD700FFEAD700FFEAD700FFEAD700FFEAD700C9B9AC00FBF8
      F400FBF8F400E6DAD900C2A6A400000000000000000000000000000000008E7C
      72008E7C720000000000000000008E7C7200ECE2E2008E7C7200000000000000
      000000000000000000000000000000000000D8A98A00FEFEFD00FEFCFA00FEFA
      F600FEF8F100FEF5EC00EBDFDB00D3C2C000BAA9AA008C5D5C00000000000000
      0000000000000000000000000000000000000000000000669A0084E9FE0084E9
      FE007373730073737300737373007373730073737300737373007373730060D4
      F0005ACFEE0000669A0000000000000000000000000000000000000000000000
      0000C2A6A400FFE8D300FFE8D300FFE8D300FFE8D300FFE8D300C9B9AC00FBF8
      F400DFCEC700C2A6A40000000000000000000000000000000000000000008E7C
      72000000000000000000000000008E7C7200F9F5F5008E7C7200000000000000
      000000000000000000000000000000000000DFB08D00FEFEFE00FEFEFE00FEFC
      FB00FEFBF700FEF8F200B4817600B4817600B4817600B17F7400000000000000
      0000000000000000000000000000000000000000000000669A0084E9FE0084E9
      FE0073737300CFC1BC00CFC1BB00CFC1BB00CFC1BB00CEBEB7007373730068D8
      F40062D4F10000669A0000000000000000000000000000000000000000000000
      0000C2A6A400FFE6D000FFE6D000FFE6D000FFE6D000FFE6D000C9B9AC00DFCE
      C700C2A6A4000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008E7C72008E7C720000000000000000000000
      000000000000000000000000000000000000E4B58E00FEFEFE00FEFEFE00FEFE
      FE00FEFDFC00FEFBF800B4817600EBB56F00E49B420000000000000000000000
      000000000000000000000000000000000000000000000000000000669A000066
      9A0073737300E2D8D300FAF9F800F9F8F700F9F8F700D0C5BF00737373000066
      9A0000669A000000000000000000000000000000000000000000000000000000
      0000C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008E7C72000000000000000000000000000000
      000000000000000000000000000000000000E8B89000DCA88700DCA88700DCA8
      8700DCA88700DCA88700B4817600F0B25E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007373730073737300737373007373730073737300000000000000
      0000000000000000000000000000000000000000000000000000B7818300B781
      8300B7818300B7818300B7818300B7818300B7818300B7818300B7818300B781
      8300B7818300B7818300B78183000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7A79C00FEEE
      D400F7E3C500F6DFBC00F5DBB400F3D7AB00F3D3A200F1CF9A00F0CF9700F0CF
      9800F0CF9800F5D49A00B781830000000000078DBE0025A1D10072C7E70085D7
      FA0066CDF90065CDF90065CDF90065CDF90065CDF80065CDF90065CDF80066CE
      F90039ADD800078DBE000000000000000000000000000000000097433F009743
      3F00B59A9B00B59A9B00B59A9B00B59A9B00B59A9B00B59A9B00B59A9B009330
      300097433F000000000000000000000000000000000000000000000000000000
      0000E4E7F6009EA9E100546BC7003E59C0003953BF004B66C20097A7DB00E1E5
      F400000000000000000000000000000000000000000000000000C7A79E00FDEF
      D900F6E3CB00F5DFC200F4DBBA00F2D7B200F1D4A900F1D0A200EECC9900EECC
      9700EECC9700F3D19900B781830000000000078DBE004CBCE70039A8D100A0E2
      FB006FD4FA006FD4F9006ED4FA006FD4F9006FD4FA006FD4FA006FD4FA006ED4
      F9003EB1D90084D7EB00078DBE00000000000000000097433F00D6686800C660
      6000E5DEDF0092292A0092292A00E4E7E700E0E3E600D9DFE000CCC9CC008F20
      1F00AF46460097433F000000000000000000000000000000000000000000C2C8
      ED005465CB003C52CC00757AE8008F92EE008F92EE007178E400334DC1003F5B
      BE00B9C4E6000000000000000000000000000000000000000000C7A9A100FEF3
      E300F8E7D300F5E3CB00F5DFC300F3DBBB00F2D7B200F1D4AB00F0D0A300EECC
      9A00EECC9700F3D19900B781830000000000078DBE0072D6FA00078DBE00AEEA
      FC0079DCFB0079DCFB0079DCFB0079DCFB0079DCFB007ADCFB0079DCFA0079DC
      FA0044B5D900AEF1F900078DBE00000000000000000097433F00D0656600C25F
      5F00E9E2E20092292A0092292A00E2E1E300E2E6E800DDE2E400CFCCCF008F22
      2200AD46460097433F0000000000000000000000000000000000C5C9EE005060
      CD005C65E000A1A6F5007E86EF005B63E900595DE7007D84EE009EA0F400515D
      D7003451BA00B9C4E60000000000000000000000000000000000C9ACA500FFF7
      EB00F9EBDA00F7E7D200F6E3CA00F5DFC200F4DBB900F2D7B200F1D4AA00F0D0
      A100EFCD9900F3D19800B781830000000000078DBE0079DDFB001899C7009ADF
      F30092E7FB0084E4FB0083E4FC0083E4FC0084E4FC0083E4FC0083E4FB0084E5
      FC0048B9DA00B3F4F900078DBE00000000000000000097433F00D0656500C15D
      5D00ECE4E40092292A0092292A00DFDDDF00E1E6E800E0E5E700D3D0D2008A1E
      1E00AB44440097433F00000000000000000000000000E8E9F9006471D300616B
      E300A1ACF500545FEC00505CEA004D59E9004E59E6004C56E6005056E6009EA2
      F4005460D6003F5CBF00E2E7F500000000000000000000000000CEB2AA00FFFC
      F400FAEFE400F8EADA00F7E7D300F5E2CB00F5DFC200F4DBBB00F1D7B200F1D3
      AA00F0D0A100F3D29B00B781830000000000078DBE0082E3FC0043B7DC0065C3
      E000ACF0FD008DEBFC008DEBFC008DEBFD008DEBFD008DEBFC008DEBFD000C85
      18004CBBDA00B6F7F9006DCAE000078DBE000000000097433F00D0656500C15B
      5C00EFE6E600EDE5E500E5DEDF00E0DDDF00DFE0E200E0E1E300D6D0D200962A
      2A00B24A4A0097433F00000000000000000000000000ACB0E9004B56DB00A2AB
      F6005664F0005266EE004D59E9004D59E9004D59E9004D59E9004C58E600525A
      E6009FA3F5003450C40095A5DB00000000000000000000000000D3B7AF00FFFF
      FD00FBF4EC00FAEFE300F9EBDA00F7E7D200F5E3C900F5DFC200F4DBBA00F2D7
      B100F0D4A900F5D5A300B781830000000000078DBE008AEAFC0077DCF300229C
      C600FDFFFF00C8F7FE00C9F7FE00C9F7FE00C9F7FE00C8F7FE000C8518003CBC
      5D000C851800DEF9FB00D6F6F900078DBE000000000097433F00CD626300C860
      6000C9676700CC727200CA727100C6696900C4646400CC6D6C00CA666700C55D
      5D00CD65650097433F000000000000000000000000007378DC00818CEE007E91
      F7005D73F3004D59E9004D59E9004D59E9004D59E9004D59E9004D59E9004F5B
      E9007B83F000757BE2004B64C300000000000000000000000000D7BBB200FFFF
      FF00FEF9F500FBF3EC00FAEFE200F9EADA00F8E7D200F5E3CA00F5DEC200F4DB
      BA00F2D8B200F6D9AC00B781830000000000078DBE0093F0FE0093F0FD001697
      C500078DBE00078DBE00078DBE00078DBE00078DBE000C85180052D97F0062ED
      970041C465000C851800078DBE00078DBE000000000097433F00B6555300C27B
      7800D39D9C00D7A7A500D8A7A600D8A6A500D7A09F00D5A09F00D7A9A700D8AB
      AB00CC66670097433F000000000000000000000000006468DB00A1ABF7007086
      F8006882F6000000000000000000000000000000000000000000000000004D59
      E9005C66EA00969CF1003956BE00000000000000000000000000DABEB300FFFF
      FF00FFFEFD00FDF8F400FBF3EC00F9EFE300F8EADA00F7E7D200F6E2CA00F6DE
      C100F4DBB900F8DDB400B781830000000000078DBE009BF5FE009AF6FE009AF6
      FE009BF5FD009BF6FE009AF6FE009BF5FE000C85180046CE6C0059E4880058E1
      880061EB940040C165000C851800000000000000000097433F00CC666700F9F9
      F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9
      F900CC66670097433F00000000000000000000000000696EDC00AFB9F9007F93
      FA007085F0000000000000000000000000000000000000000000000000004D59
      E9005E6AEE00969DF1003D55C000000000000000000000000000DEC1B500FFFF
      FF00FFFFFF00FFFEFD00FEF9F400FBF3EB00FAEFE200F9EBD900F8E6D100F6E2
      C800F7E1C200F0DAB700B781830000000000078DBE00FEFEFE00A0FBFF00A0FB
      FE00A0FBFE00A1FAFE00A1FBFE000C8518000C8518000C8518000C85180056E1
      840047CD6E000C8518000C8518000C8518000000000097433F00CC666700F9F9
      F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9
      F900CC66670097433F000000000000000000000000007C7FE200A5AFF5009DAB
      FA00778CF000545FEC00545FEC00545FEC00545FEC00545FEC00545FEC006377
      F200818EF400787FE900556BC800000000000000000000000000E2C5B500FFFF
      FF00FFFFFF00FFFFFF00FFFEFD00FDF9F400FBF3EB00FAEEE200FAEDDC00FCEF
      D900E6D9C400C6BCA900B78183000000000000000000078DBE00FEFEFE00A5FE
      FF00A5FEFF00A5FEFF00078CB60043B7DC0043B7DC0043B7DC000C8518004EDD
      790036BA54000C85180000000000000000000000000097433F00CC666700F9F9
      F900CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00F9F9
      F900CC66670097433F00000000000000000000000000B5B4EF007D83EA00CDD4
      FC008B9DFA007E93F700758AEE006C84F6006C84F6006C84F6006C84F6006379
      F300A4AFF8003E4FD0009FAAE000000000000000000000000000E5C7B700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFEFD00FDF8F300FDF6EC00F1E1D500C6A1
      9400B5948900B08F8100B7818300000000000000000000000000078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000000000000C85180040D0
      65000C8518000000000000000000000000000000000097433F00CC666700F9F9
      F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9
      F900CC66670097433F00000000000000000000000000EBEBFB007978E300A3A7
      F300D4DBFD00879AFA007F91F0007A8EF1007F94F8007E92F900768CF800A8B6
      F800636EE3005768CC00E5E8F700000000000000000000000000E9CBB800FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFEFC00FFFEF900E3CFC900BF8C
      7600E8B27000ECA54A00C5876800000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C8518002AB743002DBA
      49000C8518000000000000000000000000000000000097433F00CC666700F9F9
      F900CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00F9F9
      F900CC66670097433F0000000000000000000000000000000000CFCFF500706F
      E100AAADF200D8DCFD00AEBAFA0091A3FA008B9DFA009CA9FB00BAC7FC00707B
      E9005362CE00C3C8ED0000000000000000000000000000000000ECCDBA00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E4D4D200C89A
      7F00FAC57700CD93770000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C85180021B538000C85
      1800000000000000000000000000000000000000000097433F00CC666700F9F9
      F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9
      F900CC66670097433F000000000000000000000000000000000000000000CFCF
      F5007878E2008E93ED00BEC3F800CCD3F900C4CBF900AAB4F4006670E200636D
      D500C6CAEF000000000000000000000000000000000000000000EACAB600FCF7
      F400FCF7F300FBF6F300FBF6F300FAF5F300F9F5F300F9F5F300E1D0CE00C797
      7C00CF9B86000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C8518000C8518000C8518000C8518000000
      000000000000000000000000000000000000000000000000000097433F00F9F9
      F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9
      F90097433F000000000000000000000000000000000000000000000000000000
      0000EAEAFB00B6B5F0007D7FE1006A6BDD00686BDB007379DE00AEB2EA00E8E9
      F900000000000000000000000000000000000000000000000000E9C6B100EBCC
      B800EBCCB800EBCCB800EBCBB800EACBB800EACBB800EACCB900DABBB000B885
      7A00000000000000000000000000000000000000000000000000000000000000
      0000000000000C8518000C8518000C8518000C85180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000C00000000100010000000000000600000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000F00F000000000000
      E007000000000000C00300000000000080010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000080010000000000008001000000000000C003000000000000
      E007000000000000F00F000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      C007FFFFFFFFFFFF8003FFFFFFFFFFFF8003FFFFFFFFFFFF8003F81FF81FF81F
      8203F81FF81FF81F8303F81FF81FF81F8383F81FF81FF81F8303F81FF81FF81F
      8203F81FF81FF81F8003FFFFFFFFFFFF8003FFFFFFFFFFFF8003FFFFFFFFFFFF
      C007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFFFFFFF81FFC00F9FFF801E007
      FFFF80FFFFFFC003FF80A8FFFF018001FBFFBBFFFFFF8001F980BFFF83010000
      80FFBC00BFFF0000A880BFFFBF010000BBFFBF80BFFF0000BF80BFFFB8010000
      BFFFBF80BFFF0000BC00BFFFBEFF8001BFFFBC00BA3F8001BFFFBFFF803FC003
      BFFFBFFFFE7FE007801F80FFFEFFF81FFFFFFFFFF000FC00FFFFFFFFF000FC00
      C031C01FF000FC80FFF1FFFF0000FC00C011C0030000FC00FFF1FFFF00000000
      C031EC0300002080FFF18C03000020C0C01107FF000000E0FFF18C0300000000
      C031EC0300000000FFF1FFFF0000203FC011C003F001303FFFFFFFFFF003383F
      FFFFFFFFF007003FFFFFFFFFF00F003F8000FC3FFFFFFFFF0000F00FC003C003
      0000C00380018001000000008001800100000000800180010000000080018001
      0000000080018001000000008001800100000000800180010000000080018001
      0000000080018001000000008001800100008001800180010000C00380018001
      0000F00FC003C0030000FC3FFFFFFFFFF33FF0008000FFFFE007F0000000FFFF
      C000F0000000C0078001F000000080030000F000000080030000800000008003
      00008000000082038000800000008303E001800000008383F003800000008303
      F01FF00000008203E03FF00000008003E03FF00100008003C03FF00300008003
      C07FF0070000C007F8FFF00F0000FFFFFFFFFFFFFFFFFFFFC003C003FFFFFFFF
      80018001803FFFFF80018001001F800380018001001F830380018001001FC387
      800180010001E007800180010000F007800180010000F007800180018000F80F
      80018001F800FC0F80018001F800FC0780018001F800FC0380018001FC01FC01
      C003C003FFFFFFFFFFFFFFFFFFFFFFFF9FFFB731FF80FFC30FFFB41600000001
      07FF8400C000400083FFB416C0004000C1FFCF31C0004000E10FFFFFC0004000
      F003E7FF80004001FC01E3FF00004003FC01F11F8000402FF800F80FC000402F
      F800F807C000402FF800F807C000402FFC01F807C000082FFC01F807C0008FEF
      FE03FC0FFFE0CFEFFF8FFE1FFFF8E00F80008000FFFFFFFF00000000FFFFFFFF
      00000000C01FFFFF00000000FFFFFFFF00000000C003FFFF00000000FFFFF81F
      00000000DC03F81F00000000C403F81F0000000083FFF81F00000000C403F81F
      00000000DC03F81F00000000FFFFFFFF00000000C003FFFF00000000FFFFFFFF
      00000000FFFFFFFF00000000FFFFFFFFF8FFFFFFFFEF0000F01FFFFFF0070000
      E0030000F0030000C0000000F003000080000000F003000080000000F0030000
      80000000E007000080000000F003000080000000F007000080000000F0070000
      80010000F007000080030000F0070000C0070000F0070000E01F0000F8070000
      F01FFFFFFC0F0000F93FFFFFFFFF0000F000FC7FF801C001F000F87FF8018001
      C000FB27F80180018000FB43F80180018000F85B000180010000FC1B00018001
      0000FE03000180010000FE07000180010000FC3F000180018000F83F00018001
      8000F03F00038003C000E23F00078003F001E63F003F8003F003EE3F003F8003
      F007FE7F007FC007F00FFEFF00FFF83FC0018003FFFFFFFFC0010003C007F00F
      C00100018003E007C00100018003C003C001000180038001C001000080038001
      C001000080038001C0010000800387E1C0010001800387E1C001000080038001
      C001800380038001C001C3C780038001C001FF878003C003C003FF8F8003E007
      C007FE1FC007F00FC00FF87FFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    Left = 25
    Top = 138
    object File1: TMenuItem
      Caption = '&File'
      OnClick = File1Click
      object New1: TMenuItem
        Caption = '&New'
        ImageIndex = 0
        object mnuNewMessage: TMenuItem
          Caption = '&V2 Message'
          OnClick = mnuNewMessageClick
        end
        object mnuNewDocument: TMenuItem
          Caption = '&CDA Document'
          OnClick = mnuNewDocumentClick
        end
        object mnuNewResourceJson: TMenuItem
          Caption = '&Resource (JSON)'
          OnClick = mnuNewResourceJsonClick
        end
        object mnuNewResourceXml: TMenuItem
          Caption = 'Resource (&XML)'
          OnClick = mnuNewResourceXmlClick
        end
        object mnuNewScript: TMenuItem
          Caption = '&Javascript'
          OnClick = mnuNewScriptClick
        end
        object mnuNewMap: TMenuItem
          Caption = '&Map'
          OnClick = mnuNewMapClick
        end
        object mnuNewTemplate: TMenuItem
          Caption = '&Template'
          OnClick = mnuNewTemplateClick
        end
        object mnuNewMarkdown: TMenuItem
          Caption = 'Mar&kdown '
          OnClick = NewMarkdown1Click
        end
      end
      object mnuOpen: TMenuItem
        Caption = '&Open'
        ImageIndex = 1
        ShortCut = 16463
        OnClick = mnuOpenClick
      end
      object mnuSave: TMenuItem
        Caption = '&Save'
        ImageIndex = 2
        ShortCut = 16467
        OnClick = mnuSaveClick
      end
      object mnuSaveAll: TMenuItem
        Caption = 'Save &All'
        ShortCut = 49235
        OnClick = mnuSaveAllClick
      end
      object mnuDuplicate: TMenuItem
        Caption = '&Duplicate'
        ImageIndex = 22
        OnClick = mnuDuplicateClick
      end
      object mnuRename: TMenuItem
        Caption = '&Rename'
        ImageIndex = 25
        OnClick = mnuRenameClick
      end
      object mnuClose: TMenuItem
        Caption = '&Close'
        ImageIndex = 4
        ShortCut = 16499
        OnClick = pmCloseThisClick
      end
      object mnuDrop: TMenuItem
        Caption = 'Drop'
        ImageIndex = 3
        OnClick = mnuDropClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print'
        ImageIndex = 24
        ShortCut = 16464
        OnClick = Print1Click
      end
      object mnuWorkspace: TMenuItem
        Caption = '&Workspace'
        ImageIndex = 26
        object mnuChooseWorkspace: TMenuItem
          Caption = '&Choose'
          OnClick = mnuChooseWorkspaceClick
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 32883
        OnClick = Exit1Click
      end
    end
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      object mnuUndo: TMenuItem
        Caption = '&Undo'
        ImageIndex = 20
        ShortCut = 16474
        OnClick = mnuUndoClick
      end
      object mnuRedo: TMenuItem
        Caption = '&Redo'
        ImageIndex = 21
        ShortCut = 49242
        OnClick = mnuRedoClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuCut: TMenuItem
        Caption = 'Cu&t'
        ImageIndex = 5
        ShortCut = 16472
        OnClick = mnuCutClick
      end
      object mnuCopy: TMenuItem
        Caption = '&Copy'
        ImageIndex = 6
        ShortCut = 16451
        OnClick = mnuCopyClick
      end
      object mnuPaste: TMenuItem
        Caption = '&Paste'
        ImageIndex = 7
        ShortCut = 16470
        OnClick = mnuPasteClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuSearch: TMenuItem
        Caption = '&Find...'
        ImageIndex = 16
        ShortCut = 16454
        OnClick = mnuSearchClick
      end
      object mnuFindNext: TMenuItem
        Caption = 'Find &Next'
        ShortCut = 114
        OnClick = mnuFindNextClick
      end
      object mnuReplace: TMenuItem
        Caption = 'R&eplace...'
        ImageIndex = 17
        ShortCut = 16456
        OnClick = mnuReplaceClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object mnuGoto: TMenuItem
        Caption = '&Go to Line...'
        ShortCut = 16455
        OnClick = mnuGotoClick
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object mnuClipboard: TMenuItem
        Caption = 'Clip&board'
        object mnuCopyFileName: TMenuItem
          Caption = 'Copy &File Name'
          OnClick = mnuCopyFileNameClick
        end
        object mnuCopyDirectory: TMenuItem
          Caption = 'Copy &Directory'
          OnClick = mnuCopyDirectoryClick
        end
        object mnuCopyContents: TMenuItem
          Caption = 'Copy &Contents'
          OnClick = mnuCopyContentsClick
        end
      end
    end
    object Settings1: TMenuItem
      Caption = '&View'
      object mnuFont: TMenuItem
        Caption = '&Font'
        ImageIndex = 23
        OnClick = mnuFontClick
      end
      object mnuPackageManager: TMenuItem
        Caption = '&Package Manager'
        ImageIndex = 29
        OnClick = mnuPackageManagerClick
      end
    end
    object Execute1: TMenuItem
      Caption = '&Execute'
      object mnuCompile: TMenuItem
        Caption = '&Compile / Check Syntax'
        ImageIndex = 34
        ShortCut = 121
        OnClick = mnuCompileClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object mnuExecute: TMenuItem
        Caption = '&Go'
        ImageIndex = 27
        ShortCut = 116
        OnClick = btnExecuteClick
      end
      object mnuStepInto: TMenuItem
        Caption = 'Step &Into'
        Enabled = False
        ImageIndex = 36
        ShortCut = 118
        OnClick = tbStepIntoClick
      end
      object mnuStepOver: TMenuItem
        Caption = '&Step Over'
        Enabled = False
        ImageIndex = 37
        ShortCut = 119
        OnClick = tbStepOverClick
      end
      object mnuStepOut: TMenuItem
        Caption = 'Step &Out'
        Enabled = False
        ImageIndex = 38
        ShortCut = 120
        OnClick = tbStepOutClick
      end
      object mnuEndDebugging: TMenuItem
        Caption = '&End Debugging'
        Enabled = False
        ImageIndex = 39
        ShortCut = 113
        OnClick = tbStopClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object mnuRunNoDebug: TMenuItem
        Caption = '&Run (No Debug)'
        ImageIndex = 40
        ShortCut = 117
        OnClick = mnuRunNoDebugClick
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object mnuRecompileAll: TMenuItem
        Caption = 'Recompile &All'
        OnClick = mnuRecompileAllClick
      end
    end
    object ools1: TMenuItem
      Caption = '&Tools'
      object mnuCompare: TMenuItem
        Caption = 'Round Trip &Diff'
        ImageIndex = 28
        OnClick = mnuCompareClick
      end
      object mnuPretty: TMenuItem
        Caption = 'Make &Pretty'
        ImageIndex = 30
        OnClick = mnuPrettyClick
      end
      object mnuDense: TMenuItem
        Caption = 'Make &Dense'
        ImageIndex = 31
        OnClick = mnuDenseClick
      end
      object mnuEOL: TMenuItem
        Caption = 'Standardize EO&L'
        ImageIndex = 32
        object mnuWindowsEOL: TMenuItem
          Caption = '&Windows'
          OnClick = mnuWindowsEOLClick
        end
        object mnuUnixEOL: TMenuItem
          Caption = '&Unix'
          OnClick = mnuUnixEOLClick
        end
      end
      object mnuRemoveEmptyLines: TMenuItem
        Caption = '&Remove Empty Lines'
        ImageIndex = 33
        OnClick = mnuRemoveEmptyLinesClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mnuOptions: TMenuItem
        Caption = '&Options'
        OnClick = mnuOptionsClick
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object estException1: TMenuItem
        Caption = '&Test Exception'
        OnClick = estException1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About'
      end
    end
  end
  object pmAddAsset: TPopupMenu
    Left = 25
    Top = 193
    object pmAddExisting: TMenuItem
      Caption = '&Add Existing File'
      OnClick = mnuOpenClick
    end
    object NweV2Message1: TMenuItem
      Caption = 'New &V2 Message'
      OnClick = mnuNewMessageClick
    end
    object NewCDADocument1: TMenuItem
      Caption = 'New &CDA Document'
      OnClick = mnuNewDocumentClick
    end
    object NewResourceJSON1: TMenuItem
      Caption = 'New &Resource (JSON)'
      OnClick = mnuNewResourceJsonClick
    end
    object newResourceXML1: TMenuItem
      Caption = 'New Resource (&XML)'
      OnClick = mnuNewResourceXmlClick
    end
    object NewJavascript1: TMenuItem
      Caption = 'New &Javascript'
      OnClick = mnuNewScriptClick
    end
    object NewMap1: TMenuItem
      Caption = 'New &Map'
      OnClick = mnuNewMapClick
    end
    object NewTemplate1: TMenuItem
      Caption = 'New &Template'
      OnClick = mnuNewTemplateClick
    end
    object NewMarkdown1: TMenuItem
      Caption = 'New Mar&kdown'
      OnClick = NewMarkdown1Click
    end
  end
  object pmWorkspace: TPopupMenu
    OnPopup = pmWorkspacePopup
    Left = 25
    Top = 241
    object pmNewItem: TMenuItem
      Caption = '&New Item'
      OnClick = pmNewItemClick
    end
    object pmDuplicate: TMenuItem
      Caption = '&Duplicate'
      OnClick = mnuDuplicateClick
    end
    object pmRename: TMenuItem
      Caption = '&Rename'
      OnClick = mnuRenameClick
    end
    object pmDrop: TMenuItem
      Caption = 'Drop'
      OnClick = mnuDropClick
    end
  end
  object sdNew: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 8
    Top = 300
  end
  object odImport: TOpenDialog
    Filter = 
      'Known Files|*.msg;*.hl7;*.cda;*.xml;*.json;*.js;*.map;*.liquid;*' +
      '.md|Messages (*.msg, *.hl7)|*.msg;*.hl7|CDA Documents (*.cda, *.' +
      'xml)|*.cda;*.xml|Resources (*.json, *.xml)|*.json;*.xml|Scripts ' +
      '(*.js)|*.js|Maps (*.map)|*.map|Templates (*.liquid)|*.liquid|Mar' +
      'kdowns (*.md)|*.md|All Files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 300
  end
  object fd: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Left = 41
    Top = 377
  end
  object fontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 33
    Top = 441
  end
  object sdText: TSaveDialog
    DefaultExt = 'txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save Console'
    Left = 113
    Top = 465
  end
  object pmTabs: TPopupMenu
    Images = ImageList1
    OnPopup = pmTabsPopup
    Left = 97
    Top = 209
    object pmCloseThis: TMenuItem
      Caption = 'Close This'
      ImageIndex = 4
      ShortCut = 16499
      OnClick = pmCloseThisClick
    end
    object pmCloseOthers: TMenuItem
      Caption = 'Close Others'
      ImageIndex = 19
      OnClick = pmCloseOthersClick
    end
  end
  object pmEditor: TPopupMenu
    Images = ImageList1
    OnPopup = pmEditorPopup
    Left = 97
    Top = 161
    object pmnuCut: TMenuItem
      Caption = 'Cu&t'
      ImageIndex = 5
      ShortCut = 16472
      OnClick = mnuCutClick
    end
    object pmnuCopy: TMenuItem
      Caption = '&Copy'
      ImageIndex = 6
      ShortCut = 16451
      OnClick = mnuCopyClick
    end
    object pmnuPaste: TMenuItem
      Caption = '&Paste'
      ImageIndex = 7
      RadioItem = True
      ShortCut = 16470
      OnClick = mnuPasteClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Search2: TMenuItem
      Caption = '&Find...'
      ImageIndex = 16
      ShortCut = 16454
      OnClick = mnuSearchClick
    end
    object FindNext2: TMenuItem
      Caption = 'Find &Next'
      ShortCut = 114
      OnClick = mnuFindNextClick
    end
    object Replace1: TMenuItem
      Caption = 'R&eplace...'
      ImageIndex = 17
      ShortCut = 16456
      OnClick = mnuReplaceClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
  end
  object ReplaceDialog: TReplaceDialog
    OnFind = FindDialogFind
    OnReplace = ReplaceDialogReplace
    Left = 88
    Top = 512
  end
  object FindDialog: TFindDialog
    OnFind = FindDialogFind
    Left = 24
    Top = 496
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 200
    OnTimer = Timer1Timer
    Left = 97
    Top = 100
  end
end
