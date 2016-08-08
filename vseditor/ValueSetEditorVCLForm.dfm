object ValueSetEditorForm: TValueSetEditorForm
  Left = 0
  Top = 0
  Caption = 'FHIR ValueSet Editor'
  ClientHeight = 600
  ClientWidth = 1084
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 1100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 201
    Top = 76
    Width = 10
    Height = 524
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitHeight = 486
  end
  object pnlToolbar: TPanel
    Left = 0
    Top = 0
    Width = 1084
    Height = 39
    Align = alTop
    Alignment = taLeftJustify
    Caption = 'Toolbar todo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object ToolBar1: TToolBar
      Left = 1
      Top = 1
      Width = 1082
      Height = 48
      ButtonHeight = 38
      ButtonWidth = 39
      Caption = 'ToolBar1'
      DisabledImages = imgDisabled
      Images = imgEnabled
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object tbNew: TToolButton
        Left = 0
        Top = 0
        Hint = 'New Valueset'
        Caption = 'New Valueset'
        ImageIndex = 0
        OnClick = btnNewClick
      end
      object tbOpenFile: TToolButton
        Left = 39
        Top = 0
        Hint = 'Open From a File'
        Caption = 'Open From a File'
        ImageIndex = 2
        OnClick = btnOpenFileClick
      end
      object tbOpenServer: TToolButton
        Left = 78
        Top = 0
        Hint = 'Browse the Server'
        Caption = 'Open a value set from the server'
        ImageIndex = 4
        OnClick = btnOpenServerClick
      end
      object tbSave: TToolButton
        Left = 117
        Top = 0
        Hint = 'Save'
        Caption = 'Save'
        ImageIndex = 6
        OnClick = btnSaveClick
      end
      object tbSaveAs: TToolButton
        Left = 156
        Top = 0
        Hint = 'Save As...'
        Caption = 'Save As...'
        ImageIndex = 8
        OnClick = btnSaveAsClick
      end
      object ToolButton6: TToolButton
        Left = 195
        Top = 0
        Width = 8
        Caption = 'ToolButton6'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object tbUndo: TToolButton
        Left = 203
        Top = 0
        Hint = 'Undo'
        Caption = 'Undo'
        ImageIndex = 10
        OnClick = btnUndoClick
      end
      object tbRedo: TToolButton
        Left = 242
        Top = 0
        Hint = 'Redo'
        Caption = 'Redo'
        ImageIndex = 12
        OnClick = btnRedoClick
      end
      object ToolButton9: TToolButton
        Left = 281
        Top = 0
        Width = 8
        Caption = 'ToolButton9'
        ImageIndex = 7
        Style = tbsSeparator
      end
      object tbCut: TToolButton
        Left = 289
        Top = 0
        Hint = 'Cut'
        Caption = 'Cut'
        ImageIndex = 14
      end
      object tbCopy: TToolButton
        Left = 328
        Top = 0
        Hint = 'Copy'
        Caption = 'Copy'
        ImageIndex = 16
      end
      object tbPaste: TToolButton
        Left = 367
        Top = 0
        Hint = 'Paste'
        Caption = 'Paste'
        ImageIndex = 18
      end
      object ToolButton13: TToolButton
        Left = 406
        Top = 0
        Width = 8
        Caption = 'ToolButton13'
        ImageIndex = 10
        Style = tbsSeparator
      end
      object tbFind: TToolButton
        Left = 414
        Top = 0
        Hint = 'Find'
        Caption = 'Find'
        Enabled = False
        ImageIndex = 20
      end
      object ToolButton15: TToolButton
        Left = 453
        Top = 0
        Width = 8
        Caption = 'ToolButton15'
        ImageIndex = 11
        Style = tbsSeparator
      end
      object tbInsert: TToolButton
        Left = 461
        Top = 0
        Hint = 'Insert'
        Caption = 'Insert'
        ImageIndex = 22
      end
      object tbDelete: TToolButton
        Left = 500
        Top = 0
        Hint = 'Delete'
        Caption = 'Delete'
        ImageIndex = 24
      end
      object tbUp: TToolButton
        Left = 539
        Top = 0
        Hint = 'Up'
        Caption = 'Up'
        ImageIndex = 26
      end
      object tbDown: TToolButton
        Left = 578
        Top = 0
        Hint = 'Down'
        Caption = 'Down'
        ImageIndex = 28
      end
      object tbIn: TToolButton
        Left = 617
        Top = 0
        Hint = 'In'
        Caption = 'In'
        ImageIndex = 30
      end
      object tbOut: TToolButton
        Left = 656
        Top = 0
        Hint = 'Out'
        Caption = 'Out'
        ImageIndex = 32
      end
      object ToolButton22: TToolButton
        Left = 695
        Top = 0
        Width = 8
        Caption = 'ToolButton22'
        ImageIndex = 17
        Style = tbsSeparator
      end
      object tbImport: TToolButton
        Left = 703
        Top = 0
        Hint = 'Import'
        Caption = 'Import'
        Enabled = False
        ImageIndex = 34
      end
      object tbExport: TToolButton
        Left = 742
        Top = 0
        Hint = 'Export'
        Caption = 'Export'
        Enabled = False
        ImageIndex = 36
      end
      object ToolButton25: TToolButton
        Left = 781
        Top = 0
        Width = 8
        Caption = 'ToolButton25'
        ImageIndex = 19
        Style = tbsSeparator
      end
      object tbHelp: TToolButton
        Left = 789
        Top = 0
        Hint = 'Show Documentation'
        Caption = 'Show Documentation'
        ImageIndex = 38
        OnClick = miDocoClick
      end
    end
  end
  object PageControl1: TPageControl
    Left = 211
    Top = 76
    Width = 573
    Height = 524
    ActivePage = tabStart
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MultiLine = True
    ParentFont = False
    TabHeight = 1
    TabOrder = 2
    OnChange = LocationChange
    object tabInformation: TTabSheet
      Caption = 'Valueset Information'
      TabVisible = False
      object Panel6: TPanel
        Left = 0
        Top = 300
        Width = 565
        Height = 64
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object Label8: TLabel
          Left = 18
          Top = 6
          Width = 38
          Height = 13
          Caption = 'Phone:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label14: TLabel
          Left = 266
          Top = 6
          Width = 33
          Height = 13
          Caption = 'Email:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label15: TLabel
          Left = 520
          Top = 6
          Width = 28
          Height = 13
          Caption = 'Web:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtPhone: TEdit
          Left = 18
          Top = 28
          Width = 215
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = edtPhoneChange
        end
        object edtEmail: TEdit
          Left = 266
          Top = 28
          Width = 215
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnChange = edtEmailChange
        end
        object edtWeb: TEdit
          Left = 520
          Top = 28
          Width = 229
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnChange = edtWebChange
        end
      end
      object Panel8: TPanel
        Left = 0
        Top = 364
        Width = 565
        Height = 150
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          565
          150)
        object Label9: TLabel
          Left = 16
          Top = 6
          Width = 67
          Height = 13
          Caption = 'Description:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object txtDescription: TMemo
          Left = 18
          Top = 32
          Width = 520
          Height = 95
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = txtDescriptionChange
        end
      end
      object Panel9: TPanel
        Left = 0
        Top = 96
        Width = 565
        Height = 51
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 2
        DesignSize = (
          565
          51)
        object Label10: TLabel
          Left = 16
          Top = 22
          Width = 56
          Height = 13
          Caption = 'Identifier:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtIdentifier: TEdit
          Left = 92
          Top = 16
          Width = 446
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = edtIdentifierChange
        end
      end
      object Panel10: TPanel
        Left = 0
        Top = 147
        Width = 565
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 3
        DesignSize = (
          565
          49)
        object Label11: TLabel
          Left = 16
          Top = 22
          Width = 45
          Height = 13
          Caption = 'Version:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtVersion: TEdit
          Left = 92
          Top = 16
          Width = 446
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = edtVersionChange
        end
      end
      object Panel11: TPanel
        Left = 0
        Top = 196
        Width = 565
        Height = 52
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 4
        DesignSize = (
          565
          52)
        object Label12: TLabel
          Left = 16
          Top = 22
          Width = 55
          Height = 13
          Caption = 'Publisher:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtPublisher: TEdit
          Left = 92
          Top = 16
          Width = 446
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = edtPublisherChange
        end
      end
      object Panel12: TPanel
        Left = 0
        Top = 248
        Width = 565
        Height = 52
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 5
        DesignSize = (
          565
          52)
        object Label13: TLabel
          Left = 16
          Top = 22
          Width = 58
          Height = 13
          Caption = 'Copyright:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtCopyright: TEdit
          Left = 92
          Top = 16
          Width = 446
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = edtCopyrightChange
        end
      end
      object Panel13: TPanel
        Left = 0
        Top = 45
        Width = 565
        Height = 51
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 6
        DesignSize = (
          565
          51)
        object Label16: TLabel
          Left = 16
          Top = 22
          Width = 40
          Height = 13
          Caption = 'Status:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbExperimental: TCheckBox
          Left = 401
          Top = 20
          Width = 185
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Not for production usage'
          TabOrder = 0
          OnClick = cbExperimentalClick
        end
        object cbxStatus: TComboBox
          Left = 92
          Top = 16
          Width = 294
          Height = 24
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 1
          Text = 'draft - still being designed'
          OnChange = cbxStatusChange
          Items.Strings = (
            'draft - still being designed'
            'active - ready for normal use'
            'retired - no longer expected to be used')
        end
      end
      object Panel14: TPanel
        Left = 0
        Top = 0
        Width = 565
        Height = 45
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 7
        DesignSize = (
          565
          45)
        object Label17: TLabel
          Left = 16
          Top = 22
          Width = 35
          Height = 13
          Caption = 'Name:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtName: TEdit
          Left = 92
          Top = 16
          Width = 446
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = edtNameChange
        end
      end
    end
    object tabDefined: TTabSheet
      Caption = 'Codes Defined Here'
      ImageIndex = 1
      TabVisible = False
      object tvCodeSystem: TVirtualStringTree
        Left = 0
        Top = 97
        Width = 565
        Height = 417
        Hint = 'test'
        AccessibleName = 'tvDefine'
        Align = alClient
        Header.AutoSizeIndex = 3
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = [fsBold]
        Header.Height = 20
        Header.Options = [hoColumnResize, hoDrag, hoShowImages, hoVisible]
        HintAnimation = hatFade
        HintMode = hmHint
        ParentShowHint = False
        PopupMenu = pmTreeView
        ShowHint = True
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick, toEditOnDblClick]
        TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
        OnBeforeCellPaint = tvCodeSystemBeforeCellPaint
        OnColumnResize = tvCodeSystemColumnResize
        OnCreateEditor = tvCodeSystemCreateEditor
        OnEditCancelled = tvCodeSystemEditCancelled
        OnEdited = tvCodeSystemEdited
        OnEditing = tvCodeSystemEditing
        OnEnter = tvCodeSystemEnter
        OnExit = tvCodeSystemExit
        OnFocusChanged = tvCodeSystemFocusChanged
        OnGetText = tvCodeSystemGetText
        OnGetHintKind = tvAllGetHintKind
        OnGetHint = tvAllGetHint
        OnInitChildren = tvCodeSystemInitChildren
        OnInitNode = tvCodeSystemInitNode
        OnKeyDown = tvCodeSystemKeyDown
        OnNewText = tvCodeSystemNewText
        Columns = <
          item
            Position = 0
            Width = 100
            WideText = 'Code'
          end
          item
            Position = 1
            Width = 70
            WideText = 'Abstract'
          end
          item
            Position = 2
            Width = 200
            WideText = 'Display'
          end
          item
            Position = 3
            Width = 200
            WideText = 'Definition'
          end>
      end
      object Panel17: TPanel
        Left = 0
        Top = 0
        Width = 565
        Height = 97
        Align = alTop
        TabOrder = 1
        object Panel18: TPanel
          Left = 1
          Top = 46
          Width = 563
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 0
          DesignSize = (
            563
            45)
          object Label18: TLabel
            Left = 12
            Top = 22
            Width = 45
            Height = 13
            Caption = 'Version:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edtDefineVersion: TEdit
            Left = 74
            Top = 16
            Width = 360
            Height = 24
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnChange = edtDefineVersionChange
            OnEnter = tvCodeSystemEnter
            OnExit = tvCodeSystemExit
          end
          object cbDefineCase: TCheckBox
            Left = 449
            Top = 20
            Width = 89
            Height = 17
            Anchors = [akTop, akRight]
            Caption = 'Case Sensitive'
            TabOrder = 1
            OnEnter = tvCodeSystemEnter
            OnExit = tvCodeSystemExit
          end
        end
        object Panel19: TPanel
          Left = 1
          Top = 1
          Width = 563
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 1
          DesignSize = (
            563
            45)
          object Label19: TLabel
            Left = 12
            Top = 18
            Width = 56
            Height = 13
            Caption = 'Identifier:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edtDefineURI: TEdit
            Left = 74
            Top = 16
            Width = 464
            Height = 24
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnChange = edtDefineURIChange
            OnEnter = tvCodeSystemEnter
            OnExit = tvCodeSystemExit
          end
        end
      end
    end
    object tabComposition: TTabSheet
      Caption = 'Codes From Elsewhere'
      ImageIndex = 2
      TabVisible = False
      object Notebook2: TNotebook
        Left = 0
        Top = 0
        Width = 565
        Height = 514
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnPageChanged = LocationChange
        object TPage
          Left = 0
          Top = 0
          Caption = 'Null'
          object Label1: TLabel
            Left = 27
            Top = 33
            Width = 259
            Height = 19
            Caption = 'A value set can define it'#39's own codes'
          end
          object Label2: TLabel
            Left = 27
            Top = 128
            Width = 369
            Height = 19
            Caption = 'A value set can import codes from another value set'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label3: TLabel
            Left = 28
            Top = 240
            Width = 421
            Height = 19
            Caption = 'A value set can include a set of a codes from a code system'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label4: TLabel
            Left = 28
            Top = 344
            Width = 411
            Height = 19
            Caption = 'A value set can exclude a set of codes from a code system'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label5: TLabel
            Left = 176
            Top = 72
            Width = 238
            Height = 16
            Caption = 'When the codes aren'#39't defined elsewhere'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label6: TLabel
            Left = 176
            Top = 168
            Width = 324
            Height = 16
            Caption = 'When you want to include the codes in another value set'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label7: TLabel
            Left = 176
            Top = 280
            Width = 257
            Height = 16
            Caption = 'Include by listing them, or by their properties'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label25: TLabel
            Left = 176
            Top = 392
            Width = 336
            Height = 16
            Caption = 'To remove codes from import value sets or included codes'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object btnEditCodes: TButton
            Left = 56
            Top = 64
            Width = 97
            Height = 33
            Caption = 'Edit Codes'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = btnEditCodesClick
          end
          object btnNewImport: TButton
            Left = 56
            Top = 160
            Width = 97
            Height = 33
            Caption = 'Import'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = btnNewImportClick
          end
          object btnNewInclude: TButton
            Left = 56
            Top = 272
            Width = 97
            Height = 33
            Caption = 'Include'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = btnNewIncludeClick
          end
          object btnNewExclude: TButton
            Left = 56
            Top = 384
            Width = 97
            Height = 33
            Caption = 'Exclude'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnClick = btnNewExcludeClick
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'Import'
          object Panel22: TPanel
            Left = 0
            Top = 0
            Width = 565
            Height = 514
            Align = alClient
            Caption = 'Panel22'
            TabOrder = 0
            object Panel25: TPanel
              Left = 1
              Top = 1
              Width = 563
              Height = 48
              Align = alTop
              BevelOuter = bvNone
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentColor = True
              ParentFont = False
              TabOrder = 0
              DesignSize = (
                563
                48)
              object Label20: TLabel
                Left = 10
                Top = 16
                Width = 56
                Height = 13
                Caption = 'Value Set:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object edtImportUri: TLookAheadEdit
                Left = 72
                Top = 13
                Width = 471
                Height = 21
                AutoComplete = False
                Anchors = [akLeft, akTop, akRight]
                DropDownCount = 24
                TabOrder = 0
                Text = 'edtImportUri'
                OnChange = edtImportUriChange
                OnEnter = edtImportUriEnter
              end
            end
            object Panel23: TPanel
              Left = 1
              Top = 49
              Width = 563
              Height = 36
              Align = alTop
              Alignment = taLeftJustify
              Caption = '   Current content of this value set:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
            end
            object tvPreview: TVirtualStringTree
              Left = 1
              Top = 85
              Width = 563
              Height = 428
              Align = alClient
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              Header.AutoSizeIndex = 2
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
              NodeDataSize = 4
              ParentFont = False
              TabOrder = 2
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toFullRowSelect]
              OnColumnResize = tvPreviewColumnResize
              OnCompareNodes = tvPreviewCompareNodes
              OnGetText = tvPreviewGetText
              OnInitChildren = tvPreviewInitChildren
              OnInitNode = tvPreviewInitNode
              Columns = <
                item
                  Position = 0
                  Width = 250
                  WideText = 'System'
                end
                item
                  Position = 1
                  Width = 200
                  WideText = 'Code'
                end
                item
                  Position = 2
                  Width = 109
                  WideText = 'Display'
                end>
            end
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'IncludeExclude'
          object Panel26: TPanel
            Left = 0
            Top = 0
            Width = 565
            Height = 514
            Align = alClient
            TabOrder = 0
            object Splitter2: TSplitter
              Left = 1
              Top = 401
              Width = 563
              Height = 12
              Cursor = crVSplit
              Align = alBottom
              ExplicitTop = 557
              ExplicitWidth = 774
            end
            object Panel24: TPanel
              Left = 1
              Top = 413
              Width = 563
              Height = 100
              Align = alBottom
              BevelOuter = bvNone
              Constraints.MinHeight = 100
              ParentColor = True
              TabOrder = 0
              DesignSize = (
                563
                100)
              object Label21: TLabel
                Left = 6
                Top = 14
                Width = 38
                Height = 13
                Caption = 'Filters:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object tvFilters: TVirtualStringTree
                Left = 66
                Top = 18
                Width = 476
                Height = 67
                Anchors = [akLeft, akTop, akRight, akBottom]
                DefaultNodeHeight = 19
                Header.AutoSizeIndex = 2
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
                Header.PopupMenu = pmTreeView
                HintMode = hmHint
                ParentShowHint = False
                PopupMenu = pmTreeView
                ShowHint = True
                TabOrder = 0
                TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
                OnColumnResize = tvFiltersColumnResize
                OnCreateEditor = tvFiltersCreateEditor
                OnEnter = tvFiltersEnter
                OnExit = tvFiltersExit
                OnFocusChanged = tvFiltersFocusChanged
                OnGetText = tvFiltersGetText
                OnGetHintKind = tvAllGetHintKind
                OnGetHint = tvAllGetHint
                OnInitNode = tvFiltersInitNode
                OnKeyDown = tvFiltersKeyDown
                OnNewText = tvFiltersNewText
                Columns = <
                  item
                    Position = 0
                    Width = 150
                    WideText = 'Property'
                  end
                  item
                    Position = 1
                    Width = 70
                    WideText = 'Op'
                  end
                  item
                    Position = 2
                    Width = 252
                    WideText = 'Value'
                  end>
              end
            end
            object Panel27: TPanel
              Left = 1
              Top = 1
              Width = 563
              Height = 52
              Align = alTop
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 1
              DesignSize = (
                563
                52)
              object Label22: TLabel
                Left = 6
                Top = 22
                Width = 46
                Height = 13
                Caption = 'System:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object edtSystemReference: TLookAheadEdit
                Left = 66
                Top = 17
                Width = 475
                Height = 27
                AutoComplete = False
                Anchors = [akLeft, akTop, akRight]
                DropDownCount = 24
                TabOrder = 0
                OnChange = edtSystemReferenceChange
                OnEnter = edtSystemReferenceEnter
              end
            end
            object Panel28: TPanel
              Left = 1
              Top = 101
              Width = 563
              Height = 300
              Align = alClient
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 2
              DesignSize = (
                563
                300)
              object Label23: TLabel
                Left = 5
                Top = 10
                Width = 37
                Height = 13
                Caption = 'Codes:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object tvCodes: TVirtualStringTree
                Left = 66
                Top = 6
                Width = 476
                Height = 276
                Anchors = [akLeft, akTop, akRight, akBottom]
                DefaultNodeHeight = 19
                EditDelay = 200
                Header.AutoSizeIndex = 2
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
                HintMode = hmHint
                NodeDataSize = 4
                ParentShowHint = False
                PopupMenu = pmTreeView
                ShowHint = True
                TabOrder = 0
                TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick, toEditOnDblClick]
                TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
                OnBeforeCellPaint = tvCodesBeforeCellPaint
                OnColumnResize = tvCodesColumnResize
                OnCreateEditor = tvCodesCreateEditor
                OnDrawText = tvCodesDrawText
                OnEnter = tvCodesEnter
                OnExit = tvCodesExit
                OnFocusChanged = tvCodesFocusChanged
                OnGetText = tvCodesGetText
                OnGetHintKind = tvAllGetHintKind
                OnGetHint = tvAllGetHint
                OnInitNode = tvCodesInitNode
                OnKeyDown = tvCodesKeyDown
                OnNewText = tvCodesNewText
                Columns = <
                  item
                    Position = 0
                    Width = 273
                    WideText = 'Code'
                  end
                  item
                    Position = 1
                    Width = 200
                    WideText = 'Display'
                  end
                  item
                    Position = 2
                    Width = 10
                    WideText = 'Comments'
                  end>
              end
            end
            object Panel29: TPanel
              Left = 1
              Top = 53
              Width = 563
              Height = 48
              Align = alTop
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 3
              DesignSize = (
                563
                48)
              object Label24: TLabel
                Left = 8
                Top = 14
                Width = 45
                Height = 13
                Caption = 'Version:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object edtSystemRefVersion: TEdit
                Left = 66
                Top = 6
                Width = 476
                Height = 24
                Anchors = [akLeft, akTop, akRight]
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -13
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                OnChange = edtSystemRefVersionChange
              end
              object CheckBox1: TCheckBox
                Left = 636
                Top = 20
                Width = 113
                Height = 17
                Anchors = [akTop, akRight]
                Caption = 'Case Sensitive'
                TabOrder = 1
              end
            end
          end
        end
      end
    end
    object tabExpansion: TTabSheet
      Caption = 'Evaluate Current Content'
      ImageIndex = 3
      TabVisible = False
      object pnlExpansion: TPanel
        Left = 0
        Top = 0
        Width = 565
        Height = 65
        Align = alTop
        Alignment = taLeftJustify
        TabOrder = 0
        DesignSize = (
          565
          65)
        object lblExpansion: TLabel
          Left = 16
          Top = 43
          Width = 56
          Height = 13
          Caption = 'Status note'
        end
        object Label26: TLabel
          Left = 160
          Top = 12
          Width = 53
          Height = 13
          Caption = 'Text Filter:'
        end
        object Button2: TButton
          Left = 0
          Top = 0
          Width = 145
          Height = 37
          Caption = 'Evaluate'
          ImageIndex = 13
          TabOrder = 0
          OnClick = Button2Click
        end
        object edtFilter: TEdit
          Left = 235
          Top = 6
          Width = 315
          Height = 27
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnChange = edtFilterChange
        end
      end
      object tvExpansion: TVirtualStringTree
        Left = 0
        Top = 65
        Width = 565
        Height = 449
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Header.AutoSizeIndex = 2
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
        Header.SortColumn = 0
        NodeDataSize = 4
        ParentFont = False
        TabOrder = 1
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        OnColumnResize = tvExpansionColumnResize
        OnCompareNodes = tvExpansionCompareNodes
        OnGetText = tvExpansionGetText
        OnInitChildren = tvExpansionInitChildren
        OnInitNode = tvExpansionInitNode
        Columns = <
          item
            Position = 0
            Width = 250
            WideText = 'System'
          end
          item
            Position = 1
            Width = 200
            WideText = 'Code'
          end
          item
            Position = 2
            Width = 111
            WideText = 'Display'
          end>
      end
    end
    object tabStart: TTabSheet
      Caption = 'Welcome and Choose Value Set'
      ImageIndex = 4
      TabVisible = False
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 565
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label27: TLabel
          Left = 21
          Top = 12
          Width = 157
          Height = 16
          Cursor = crHandPoint
          Caption = 'Create a new Value Set'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'Verdana'
          Font.Style = [fsUnderline]
          ParentFont = False
          OnClick = btnNewClick
        end
        object Label29: TLabel
          Left = 21
          Top = 38
          Width = 397
          Height = 16
          Cursor = crArrow
          Caption = 'Open a value set from a File, the current server, or any URL'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          OnClick = btnNewClick
        end
        object Label30: TLabel
          Left = 187
          Top = 38
          Width = 22
          Height = 16
          Cursor = crHandPoint
          Caption = 'File'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'Verdana'
          Font.Style = [fsUnderline]
          ParentColor = False
          ParentFont = False
          Transparent = False
          OnClick = btnOpenFileClick
        end
        object Label31: TLabel
          Left = 245
          Top = 38
          Width = 94
          Height = 16
          Cursor = crHandPoint
          Caption = 'current server'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'Verdana'
          Font.Style = [fsUnderline]
          ParentColor = False
          ParentFont = False
          Transparent = False
          OnClick = btnOpenServerClick
        end
        object Label32: TLabel
          Left = 364
          Top = 38
          Width = 53
          Height = 16
          Cursor = crHandPoint
          Caption = 'any URL'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'Verdana'
          Font.Style = [fsUnderline]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 473
        Width = 565
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
      end
      object Panel3: TPanel
        Left = 0
        Top = 65
        Width = 565
        Height = 408
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 2
        DesignSize = (
          565
          408)
        object Panel4: TPanel
          Left = 10
          Top = 10
          Width = 545
          Height = 26
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = '  Past Value Sets:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object webMRU: TWebBrowser
          Left = 21
          Top = 42
          Width = 494
          Height = 360
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 1
          ExplicitWidth = 676
          ControlData = {
            4C0000000E330000352500000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
  end
  object pnlDocumentation: TPanel
    Left = 784
    Top = 76
    Width = 300
    Height = 524
    Align = alRight
    BorderWidth = 6
    TabOrder = 1
    object webDoco: TWebBrowser
      Left = 7
      Top = 7
      Width = 286
      Height = 510
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 184
      ExplicitHeight = 788
      ControlData = {
        4C0000008F1D0000B63400000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E12620B000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
  object Panel20: TPanel
    Left = 0
    Top = 76
    Width = 201
    Height = 524
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 3
    object Panel21: TPanel
      Left = 0
      Top = 0
      Width = 201
      Height = 37
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Value Set Structure'
      TabOrder = 0
    end
    object tvStructure: TVirtualStringTree
      Left = 0
      Top = 37
      Width = 201
      Height = 487
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.MainColumn = -1
      NodeDataSize = 4
      ParentFont = False
      PopupMenu = pmtvStructure
      TabOrder = 1
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
      OnClick = tvStructureClick
      OnFocusChanged = tvStructureFocusChanged
      OnGetText = tvStructureGetText
      OnPaintText = tvStructurePaintText
      OnInitChildren = tvStructureInitChildren
      OnInitNode = tvStructureInitNode
      Columns = <>
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 39
    Width = 1084
    Height = 37
    Align = alTop
    TabOrder = 4
    DesignSize = (
      1084
      37)
    object Label33: TLabel
      Left = 10
      Top = 10
      Width = 72
      Height = 13
      Caption = 'Current Server'
    end
    object cbxServer: TComboBox
      Left = 92
      Top = 6
      Width = 949
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'cbxServer'
    end
    object btnManageServers: TBitBtn
      Left = 1048
      Top = 4
      Width = 30
      Height = 25
      Hint = 'Manage Servers'
      Anchors = [akTop, akRight]
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000F8E6FE393838
        393838393838393838F8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6
        FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00C77B479A542A633B2439383839383839
        3838F8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00
        DCA878EBB788C77B479A542A633B24393838393838393838F8E6FEF8E6FEF8E6
        FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00E5B996FFF0DBFEE2C3FDDBB7F3BA83CA
        87499A542A533625393838F8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00
        E5C0A9FEEAD4FEE2C3FDDBB7FDE7B9FFD186FBAD5D773814393838F8E6FEF8E6
        FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00E6C9B7FFF0DBCCD7CC0098CBFDDBB7FF
        C25BF2B777633B24333333F8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00
        FDDEC0D9E4DC0098CB0098CBBABDA7919175D5976581491A333333333333F8E6
        FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00FEE6D10098CBBCD7D9ABD1D40098CBA7
        9C81EBB788F9CA93CA874995673B333333333333F8E6FEF8E6FEF8E6FEC45C00
        F4EBE5F7F6EEFEF1E3D9E4DC0098CBFFEBCAE4AC72CA8749F9CA93F9CA93BD96
        6A5B5C83333333F8E6FEF8E6FEC45C00FEF6F0FEF9F5FEF5EAFEF1E30098CBF6
        ECDBE5B99666381A333333DCA878AC9E9C3059D14C57B1F8E6FEF8E6FEC45C00
        FEFEFDFEFEFDFEF9F5FFF9ECABD1D4C9E2E2F4BD93533625333333F8E6FEF8E6
        FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00FEFEFDFEFEFDFEFEFDFEFEFDD4EAECC9
        E2E2F3BA83533625333333F8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEC45C00
        C67339D59765D9B69BE8CEC1F4EBE5FEFEFDE5B9965A4029333333F8E6FEF8E6
        FEF8E6FEF8E6FEF8E6FEF8E6FEE4892ED26C09C45C00C45C00B85301C05D10C7
        7B47C673395A4837333333F8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FE
        F8E6FE656565656565CA8749D88736D47618D26C09715F4EF8E6FEF8E6FEF8E6
        FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8
        E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FEF8E6FE}
      TabOrder = 1
      OnClick = btnManageServersClick
    end
  end
  object opnValueSet: TFileOpenDialog
    DefaultExtension = 'xml'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'XML File'
        FileMask = '*.xml'
      end
      item
        DisplayName = 'JSON file'
        FileMask = '*.json'
      end
      item
        DisplayName = 'Any File'
        FileMask = '*.*'
      end>
    Options = [fdoFileMustExist]
    Title = 'Open ValueSet'
    Left = 856
    Top = 108
  end
  object mnuSaveAs: TPopupMenu
    Left = 860
    Top = 168
    object File1: TMenuItem
      Caption = 'File'
      OnClick = File1Click
    end
    object OSer1: TMenuItem
      Caption = 'To Server As New'
      OnClick = OSer1Click
    end
    object oServerasExisting1: TMenuItem
      Caption = 'To Server as Existing'
      OnClick = oServerasExisting1Click
    end
  end
  object svValueSet: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'XML Format'
        FileMask = '*.xml'
      end
      item
        DisplayName = 'JSON Format'
        FileMask = '*.json'
      end>
    Options = [fdoPathMustExist]
    Left = 812
    Top = 108
  end
  object MainMenu1: TMainMenu
    Left = 900
    Top = 168
    object File2: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
        OnClick = btnNewClick
      end
      object Open1: TMenuItem
        Caption = '&Open from File...'
        ShortCut = 16463
        OnClick = btnOpenFileClick
      end
      object OpenfromUrl1: TMenuItem
        Caption = 'Open from &Server'
        ShortCut = 49235
        OnClick = OpenfromUrl1Click
      end
      object OpenfromURL2: TMenuItem
        Caption = 'Open from &URL'
        ShortCut = 16469
        OnClick = OpenfromURL2Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = btnSaveClick
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
        ShortCut = 49235
        OnClick = btnSaveAsClick
      end
      object OpenFromServer1: TMenuItem
        Caption = '&Close'
        ShortCut = 16499
        OnClick = btnCloseClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print...'
        ShortCut = 16464
      end
      object PrintSetup1: TMenuItem
        Caption = 'P&rint Setup...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object miUndo: TMenuItem
        Caption = '&Undo'
        ShortCut = 16474
        OnClick = btnUndoClick
      end
      object miRedo: TMenuItem
        Caption = '&Redo'
        ShortCut = 16473
        OnClick = btnRedoClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miCut: TMenuItem
        Caption = 'Cu&t'
        ShortCut = 16472
        OnClick = miCutClick
      end
      object miCopy: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = miCopyClick
      end
      object miPaste: TMenuItem
        Caption = '&Paste'
        ShortCut = 16470
        OnClick = miPasteClick
      end
      object PasteSpecial1: TMenuItem
        Caption = 'Paste &Special...'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Caption = '&Find...'
      end
    end
    object ools1: TMenuItem
      Caption = '&Tools'
      object Import2: TMenuItem
        Caption = 'Import'
      end
      object Export1: TMenuItem
        Caption = 'Export'
      end
      object Servers1: TMenuItem
        Caption = 'Manage &Servers'
        ShortCut = 16461
        OnClick = Servers1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object miDoco: TMenuItem
        Caption = '&Documentation'
        OnClick = miDocoClick
      end
      object WelcomeScreen1: TMenuItem
        Caption = 'Welcome &Screen'
        OnClick = WelcomeScreen1Click
      end
      object miWebPage: TMenuItem
        Caption = '&Web Page'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miAbout: TMenuItem
        Caption = '&About...'
        OnClick = miAboutClick
      end
    end
  end
  object imgEnabled: TImageList
    Height = 32
    Width = 32
    Left = 960
    Top = 112
    Bitmap = {
      494C010128006C00A40020002000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      00000000000036000000280000008000000060010000010020000000000000C0
      0200000000000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000085AD4B0085AD4B0085AD
      4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD
      4B0085AD4B0085AD4B0085AD4B0085AD4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000085AD4B007FA942007BA6
      3C007BA63C007BA63C007EA8410085AD4B0085AD4B0085AD4B0085AD4B0080A9
      44007BA63C007BA63C0080A9440085AD4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BBBBBB00B8B8B800B5B5
      B500B5B5B500B5B5B500B7B7B700BBBBBB00BBBBBB00BBBBBB00BBBBBB00B8B8
      B800B5B5B500B5B5B500B8B8B800BBBBBB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000816863007E5751008C584F008D574D008A574E007D5751007D6460008A80
      7F00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A7A7A7009F9F9F009F9F9F009E9E9E009E9E9E009E9E9E00A6A6A600B5B5
      B500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3783E00B378
      3E00B3783E00000000000000000000000000000000007FA94200D9E6C800FFFF
      FF00FFFFFF00FFFFFF00D5E2C1007AA53A0085AD4B0085AD4B007DA73F00BCD2
      9C00FFFFFF00FFFFFF00C4D7A80080A944000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AAAAAA00AAAA
      AA00AAAAAA0000000000000000000000000000000000B8B8B800EAEAEA00FFFF
      FF00FFFFFF00FFFFFF00E7E7E700B5B5B500BBBBBB00BBBBBB00B7B7B700D9D9
      D900FFFFFF00FFFFFF00DDDDDD00B8B8B8000000000000000000000000000000
      0000000000000000000000000000000000000000000094716A0092554700AC6F
      5C00C48B7300D69F8300E0AA8D00E2AC8F00DFA98D00D59C8200C3887200AA6C
      5A00915346008D6E680000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ADADAD009D9D9D00ACAC
      AC00BBBBBB00C5C5C500CBCBCB00CDCDCD00CBCBCB00C4C4C400B9B9B900AAAA
      AA009C9C9C00ABABAB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AF71
      3400AE6F3100B3783E0000000000000000000000000084AC4A007BA63D00EDF3
      E500FFFFFF00FFFFFF00FFFFFF00BBD29B007DA73F0080A94300A2C17700FFFF
      FF00FFFFFF00DCE7CD0079A53A0085AD4B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A5A5
      A500A4A4A400AAAAAA00000000000000000000000000BBBBBB00B5B5B500F5F5
      F500FFFFFF00FFFFFF00FFFFFF00D9D9D900B7B7B700B8B8B800CBCBCB00FFFF
      FF00FFFFFF00EBEBEB00B4B4B400BBBBBB000000000000000000000000000000
      00000000000000000000000000009676700099594A00C88E7700ECB79800EFB6
      9500EEAE8C00ECA78400ECA68200ECA68200ECA68200EDA98500EEAF8C00EFB7
      9600EAB59600C48972009757490090726C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B0009F9F9F00BCBCBC00D2D2D200D2D2
      D200CECECE00CACACA00C9C9C900C9C9C900C9C9C900CACACA00CECECE00D2D2
      D200D1D1D100B9B9B9009F9F9F00ADADAD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000AF713400AE6F3100B3783E00000000000000000085AD4B0083AC490082AB
      4700FCFDFC00FFFFFF00FFFFFF00FFFFFF00A0C074008CB15500FFFFFF00FFFF
      FF00F1F5EA007BA73E0084AC4A0085AD4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A5A500A4A4A400AAAAAA000000000000000000BBBBBB00BABABA00B9B9
      B900FDFDFD00FFFFFF00FFFFFF00FFFFFF00CACACA00BFBFBF00FFFFFF00FFFF
      FF00F6F6F600B6B6B600BBBBBB00BBBBBB000000000000000000000000000000
      0000000000000000000090594E00BC826C00EDB89900EEB08E00EBA27D00EB9F
      7900EB9F7900E4987300DA8F6A00DA8F6C00E4987300EB9F7900EB9F7900EB9F
      7900EBA37E00EEB18F00EBB69700B77B66008E574D0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A0A0A000B6B6B600D3D3D300CECECE00C7C7C700C5C5
      C500C5C5C500C1C1C100BCBCBC00BCBCBC00C1C1C100C5C5C500C5C5C500C5C5
      C500C7C7C700CFCFCF00D2D2D200B2B2B2009F9F9F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AE703200AE6F3100B3783E000000000085AD4B0085AD4B0082AB
      46008FB45B00FFFFFF00FFFFFF00FFFFFF00FDFDFF00F5F7F000FFFFFF00FFFF
      FF0085AD4C0083AC480085AD4B0085AD4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A4A4A400A4A4A400AAAAAA0000000000BBBBBB00BBBBBB00B9B9
      B900C1C1C100FFFFFF00FFFFFF00FFFFFF00FDFDFD00F8F8F800FFFFFF00FFFF
      FF00BBBBBB00BABABA00BBBBBB00BBBBBB000000000000000000000000000000
      00000000000097574A00D7A08400EFB79600EBA37E00EB9F7900EB9F7900E99C
      7700CE835F00C5836500D7A79100D7A69100C5836500CD835F00E99C7700EB9F
      7900EB9F7900EB9F7900ECA48000F1B89700D2987E0093564900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009E9E9E00C6C6C600D2D2D200C7C7C700C5C5C500C5C5C500C3C3
      C300B5B5B500B5B5B500CACACA00CACACA00B5B5B500B5B5B500C3C3C300C5C5
      C500C5C5C500C5C5C500C8C8C800D3D3D300C2C2C2009E9E9E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C1906100B0743800AE6F
      3100AE6F3100AE6F3100AE6F3100AE6F3100AF77420085AD4B0085AD4B0085AD
      4B0080A94300A3C27900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0094B8
      620081AA460085AD4B0085AD4B0085AD4B000000000000000000000000000000
      00000000000000000000000000000000000000000000B9B9B900A7A7A700A4A4
      A400A4A4A400A4A4A400A4A4A400A4A4A400AAAAAA00BBBBBB00BBBBBB00BBBB
      BB00B8B8B800CCCCCC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C4C4
      C400B9B9B900BBBBBB00BBBBBB00BBBBBB000000000000000000000000000000
      000098594B00DFA98C00EEB39000EB9F7900EB9F7900EB9F7900EB9F7900D085
      6200D6A58F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D6A58F00D0856200EB9F
      7900EB9F7900EB9F7900EB9F7900EBA07A00EFB49200D9A28600945749000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009F9F9F00CBCBCB00D0D0D000C5C5C500C5C5C500C5C5C500C5C5C500B7B7
      B700CACACA00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CACACA00B7B7B700C5C5
      C500C5C5C500C5C5C500C5C5C500C6C6C600D1D1D100C7C7C7009E9E9E000000
      000000000000000000000000000000000000919191006B6B6B006B6B6B006B6B
      6B00868686000000000000000000BA844F00AE6F3100AE6F3100AE6F3100AE6F
      3100AE6F3100AE6F3100AE6F3100AE6F3100AF77430085AD4B0085AD4B0085AD
      4B0085AD4B007DA73F00B9D09800FFFFFF00FFFFFF00FFFFFF00EAF0E00078A4
      370084AC4A0085AD4B0085AD4B0085AD4B00BFBFBF00A9A9A900A9A9A900A9A9
      A900B8B8B8000000000000000000B1B1B100A4A4A400A4A4A400A4A4A400A4A4
      A400A4A4A400A4A4A400A4A4A400A4A4A400ABABAB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00B7B7B700D8D8D800FFFFFF00FFFFFF00FFFFFF00F2F2F200B4B4
      B400BBBBBB00BBBBBB00BBBBBB00BBBBBB00000000000000000000000000975E
      4F00D9A28600EEB19000EB9F7900EB9F7900EB9F7900EB9F7900E99C7800C27A
      5900FFFBF800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFBF800C2795900E99C
      7800EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EFB49200D2997E00915A
      4D0000000000000000000000000000000000000000000000000000000000A2A2
      A200C7C7C700D0D0D000C5C5C500C5C5C500C5C5C500C5C5C500C4C4C400B0B0
      B000FCFCFC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FCFCFC00B0B0B000C4C4
      C400C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500D1D1D100C2C2C200A0A0
      A000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF000000000000000000B2773D00AE6F3100AF713500CBA47C00000000000000
      000000000000AE703200AE6F3100B3783D000000000085AD4B0085AD4B0085AD
      4B0085AD4B007DA73F00B7CE9600FFFFFF00FFFFFF00FFFFFF00FFFFFF00D4E3
      C2007AA53A0085AD4B0085AD4B0085AD4B00ABABAB00FFFFFF00FFFFFF00FFFF
      FF000000000000000000A9A9A900A4A4A400A5A5A500C5C5C500000000000000
      000000000000A4A4A400A4A4A400A9A9A90000000000BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00B7B7B700D6D6D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7E7
      E700B5B5B500BBBBBB00BBBBBB00BBBBBB000000000000000000B5887C00C387
      6F00EFB69500EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900E4987300C887
      6A00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6856800E699
      7300EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EBA07A00F1B89700BA7E
      670093746D000000000000000000000000000000000000000000BABABA00B8B8
      B800D2D2D200C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C1C1C100B8B8
      B800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B7B7B700C2C2
      C200C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C6C6C600D3D3D300B3B3
      B300AFAFAF000000000000000000000000006F6F6F00FFFFFF00FFFFFF000000
      000000000000B9834E00AE6F3100B57D45000000000000000000000000000000
      0000B47B4200AE6F3100B3783D00000000000000000085AD4B0085AD4B0085AD
      4B0080A94300A2C17700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00BAD099007DA73F0085AD4B0085AD4B00ABABAB00FFFFFF00FFFFFF000000
      000000000000B1B1B100A4A4A400ACACAC000000000000000000000000000000
      0000ABABAB00A4A4A400A9A9A9000000000000000000BBBBBB00BBBBBB00BBBB
      BB00B8B8B800CBCBCB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00D8D8D800B7B7B700BBBBBB00BBBBBB000000000000000000A2624E00EEB9
      9900EBA27D00EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EA9D7800C279
      5800FFF9F600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEF8F500C1785800EA9D
      7800EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900ECA48000ECB7
      97009D5D4A000000000000000000000000000000000000000000A4A4A400D4D4
      D400C7C7C700C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C4C4C400B0B0
      B000FAFAFA00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FAFAFA00AFAFAF00C4C4
      C400C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C8C8C800D2D2
      D200A2A2A2000000000000000000000000006F6F6F00FFFFFF00FFFFFF000000
      000000000000AE6F3100AF71350000000000000000000000000000000000B67D
      4600AE6F3100B2773D0000000000000000000000000085AD4B0085AD4B0082AB
      46008FB45B00FFFFFF00FFFFFF00F3F6EE0081AA4500FDFDFD00FFFFFF00FFFF
      FF00FFFFFF00A1BF740080A9440085AD4B00ABABAB00FFFFFF00FFFFFF000000
      000000000000A4A4A400A5A5A50000000000000000000000000000000000ADAD
      AD00A4A4A400A9A9A900000000000000000000000000BBBBBB00BBBBBB00B9B9
      B900C1C1C100FFFFFF00FFFFFF00F8F8F800B9B9B900FDFDFD00FFFFFF00FFFF
      FF00FFFFFF00CACACA00B8B8B800BBBBBB0000000000B3847700CF967A00EEAE
      8B00EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900D287
      6400D39F8700FFFDFB00FFFFFF00FFFFFF00FFFCF900D39D8600D3876500EB9F
      7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EEB0
      8E00C98E730093726900000000000000000000000000B8B8B800C0C0C000CDCD
      CD00C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500B8B8
      B800C6C6C600FDFDFD00FFFFFF00FFFFFF00FCFCFC00C5C5C500B8B8B800C5C5
      C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500CECE
      CE00BCBCBC00ADADAD0000000000000000006F6F6F00FFFFFF00FFFFFF000000
      0000C08F5F00AE6F3100CBA47C0000000000FFFFFF0000000000B3783E00B378
      3E00B3783E000000000000000000FFFFFF000000000085AD4B0083AC490081AB
      4700FCFDFB00FFFFFF00FFFFFF0087AE4F0080A9440090B55E00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008DB3570082AB4700ABABAB00FFFFFF00FFFFFF000000
      0000B8B8B800A4A4A400C5C5C50000000000FFFFFF0000000000AAAAAA00AAAA
      AA00AAAAAA000000000000000000FFFFFF0000000000BBBBBB00BABABA00B9B9
      B900FDFDFD00FFFFFF00FFFFFF00BCBCBC00B8B8B800C2C2C200FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000B9B9B90000000000A05E4900EEB99900EBA3
      7D00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EAA0
      7A00D0876400C37E5F00D29D8500D29C8500C37E5F00D0886500EAA07A00EBA1
      7B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA5
      8000EBB696009C5B4700000000000000000000000000A2A2A200D4D4D400C7C7
      C700C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600B7B7B700B3B3B300C5C5C500C5C5C500B3B3B300B8B8B800C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C8C8
      C800D2D2D200A0A0A00000000000000000006F6F6F00FFFFFF00FFFFFF000000
      0000B0723600AE6F31000000000000000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000084AC4A0081AA4600F6F9
      F300FFFFFF00FFFFFF0098BA690081AA450085AD4B007FA94300A6C27C00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0087AE4F00ABABAB00FFFFFF00FFFFFF000000
      0000A6A6A600A4A4A4000000000000000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0000000000BBBBBB00B9B9B900F9F9
      F900FFFFFF00FFFFFF00C6C6C600B9B9B900BBBBBB00B8B8B800CDCDCD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00BCBCBC0000000000BA7D6400EEB49100EBA5
      7E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA5
      7E00E39D7800D9956F00D08C6700D28C6700D9956F00E09A7400EBA57E00EBA5
      7E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA5
      7E00EFB69300B4765C00000000000000000000000000B3B3B300D1D1D100C8C8
      C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8
      C800C3C3C300BFBFBF00B9B9B900B9B9B900BFBFBF00C2C2C200C8C8C800C8C8
      C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8
      C800D2D2D200AEAEAE0000000000000000006F6F6F00FFFFFF00FFFFFF000000
      0000D4B39300D4B3930000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000085AD4A0084AC4B0080AA
      44007EA840007DA7400082AB470085AD4B0085AD4B0085AD4B0081AA45007DA7
      41007EA840007EA840007FA9420085AC4A00ABABAB00FFFFFF00FFFFFF000000
      0000CFCFCF00CFCFCF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000BBBBBB00BBBBBB00B8B8
      B800B7B7B700B7B7B700B9B9B900BBBBBB00BBBBBB00BBBBBB00B9B9B900B7B7
      B700B7B7B700B7B7B700B8B8B800BBBBBB00C3998B00D2977A00ECB08B00EBAB
      8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300E4A4
      7D00C4866600D4A48D00D4A48D00D4A48D00D4A48D00C88D7000DD9B7600EBAB
      8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB
      8300EDB18D00CA8F72009A80770000000000C4C4C400C1C1C100CECECE00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00C7C7
      C700B7B7B700C9C9C900C9C9C900C9C9C900C9C9C900BBBBBB00C2C2C200CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CFCFCF00BCBCBC00B5B5B500000000006F6F6F00FFFFFF00FFFFFF000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000085AD4B0085AD4A0084AC
      4A0084AC4A0084AC4A0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0084AC
      4A0084AC4A0084AC4A0084AC4A0084AD4A00ABABAB00FFFFFF00FFFFFF000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00B07B6700E0A78700EBB18900EBAF
      8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700DFA2
      7B00D2A28900FFFFFF00FFFFFF00FFFFFF00FFFFFF00DEBBA900D6987300EBAF
      8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF
      8700ECB18C00D9A08100966C5E0000000000B1B1B100C9C9C900CECECE00CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00C6C6
      C600C7C7C700FFFFFF00FFFFFF00FFFFFF00FFFFFF00D6D6D600C0C0C000CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCD
      CD00CECECE00C6C6C600AAAAAA00000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5674F00E9B39100EBB58D00EBB4
      8D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00E3AB
      8400CC997D00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F3E3DA00C4876600E6AD
      8600EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB4
      8D00ECB58E00E2AB8900A46C580000000000A5A5A500D0D0D000D0D0D000D0D0
      D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000CACA
      CA00C2C2C200FFFFFF00FFFFFF00FFFFFF00FFFFFF00EEEEEE00B7B7B700CBCB
      CB00D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000D0D0D000CBCBCB00A9A9A900000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFE
      FE006C6C6C000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFE
      FE00A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A4634900EDB89600EFBA9300EFBA
      9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFB9
      9200C98C6A00F3E2D900FFFFFF00FFFFFF00FFFFFF00FFFFFF00E9D2C300C88C
      6900E9B38C00EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA
      9300EFBA9300E6AF8E00A56C540000000000A3A3A300D2D2D200D3D3D300D3D3
      D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D2D2
      D200B9B9B900EDEDED00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E3E3E300B9B9
      B900CFCFCF00D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3
      D300D3D3D300CECECE00A9A9A900000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D00FFFFFF00FEFE
      FE006C6C6C000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00FFFFFF00FEFE
      FE00A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A9694F00ECB79300F5C09900F5C0
      9900F5C09900F5C09900F5C09900F5C09900F5C09900F5C09900F5C09900F5C0
      9900E8B18C00CB937400F9F1EB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EDD7
      CB00CB906E00EDB79100F5C09900F5C09900F5C09900F5C09900F5C09900F5C0
      9900F5C09900E6AE8D00A770590000000000A7A7A700D2D2D200D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600CECECE00BEBEBE00F6F6F600FFFFFF00FFFFFF00FFFFFF00FFFFFF00E6E6
      E600BCBCBC00D1D1D100D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600CDCDCD00ABABAB00000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFE
      FE006C6C6C000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFE
      FE00A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B37B6300E4AE8C00F5C29C00F6C2
      9C00F6C29C00F6C29C00F6C29C00F6C29C00F6C29C00F6C29C00F6C29C00F6C2
      9C00F6C29C00E6AF8C00CD967A00FCF3EE00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00EED8CD00CC907000F3BF9900F6C29C00F6C29C00F6C29C00F6C29C00F6C2
      9C00F6C29D00DEA585009C73610000000000B1B1B100CDCDCD00D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800CDCDCD00C0C0C000F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00E7E7E700BCBCBC00D6D6D600D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800C8C8C800ACACAC00000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D00FFFFFF00FEFE
      FE006B6B6B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00FFFFFF00FEFE
      FE00A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C89D8900D79D7D00F6C3A000F6C3
      9F00F6C39F00F6C39F00F6C39F00F6C39F00F6C39F00F6C39F00F6C39F00F6C3
      9F00F6C39F00F6C39F00E4AE8B00CE9A7E00FFFBF800FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E1BFAC00D9A28000F6C39F00F6C39F00F6C39F00F6C39F00F6C3
      9F00F5C39F00D0967600A2877B0000000000C5C5C500C4C4C400D9D9D900D9D9
      D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9
      D900D9D9D900D9D9D900CDCDCD00C2C2C200FCFCFC00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00D8D8D800C6C6C600D9D9D900D9D9D900D9D9D900D9D9D900D9D9
      D900D8D8D800C0C0C000B8B8B800000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD006B6B6B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C4876600F5C4A200F5C4
      A000F5C4A000F5C4A000F5C4A000F5C4A000F5C4A000F5C3A000F1BF9B00F5C4
      A000F5C4A000F5C4A000F5C4A000DAA48300DEBAA600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFBF800C9907100F4C29F00F5C4A000F5C4A000F5C4A000F5C4
      A000F5C4A200BF805F00000000000000000000000000B7B7B700D9D9D900D9D9
      D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D6D6D600D9D9
      D900D9D9D900D9D9D900D9D9D900C7C7C700D5D5D500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FCFCFC00BCBCBC00D8D8D800D9D9D900D9D9D900D9D9D900D9D9
      D900D9D9D900B3B3B30000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D00FFFFFF00FDFD
      FD006B6B6B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00FFFFFF00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AE6B4B00F3C1A000F5C5
      A300F4C4A200F4C4A200F4C4A200F4C4A200F4C4A200D39C7C00C9937700D29A
      7A00E0AC8C00EAB89700ECBA9900E1AD8C00D2A38B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00D2A48C00EAB89700F4C4A200F4C4A200F4C4A200F5C5
      A400EEBC9B00AD6B4C00000000000000000000000000A7A7A700D8D8D800DADA
      DA00D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900C3C3C300BEBEBE00C2C2
      C200CCCCCC00D2D2D200D4D4D400CDCDCD00C8C8C800FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C9C9C900D2D2D200D9D9D900D9D9D900D9D9D900DADA
      DA00D5D5D500A7A7A70000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD006B6B6B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C2907800D9A28000F5C8
      A600F4C4A300F4C4A300F4C4A300F4C4A300EEBF9D00CA967A00FFFFFE00EFDF
      D500DEBBAA00D2A58D00CE9F8600D6AD9700F4E6DF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00D2A68F00E9B89700F4C4A300F4C4A300F4C4A300F5C8
      A600D49A7900A7847200000000000000000000000000BDBDBD00C6C6C600DBDB
      DB00D9D9D900D9D9D900D9D9D900D9D9D900D6D6D600C0C0C000FEFEFE00EBEB
      EB00D6D6D600C9C9C900C5C5C500CECECE00EFEFEF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CACACA00D2D2D200D9D9D900D9D9D900D9D9D900DBDB
      DB00C2C2C200B6B6B60000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD006B6B6B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B7745100F3C5
      A400F5C6A600F4C5A400F4C5A400F4C5A400DEAB8C00DEBCAB00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C9937700F1C1A000F4C5A400F4C5A400F5C8A700F1C2
      A100B3704C000000000000000000000000000000000000000000ACACAC00DADA
      DA00DBDBDB00DADADA00DADADA00DADADA00CBCBCB00D6D6D600FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BEBEBE00D8D8D800DADADA00DADADA00DCDCDC00D8D8
      D800A9A9A9000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FCFC
      FC006B6B6B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7997F00D399
      7700F7CBAC00F4C5A400F4C5A400F4C5A400CE987900F4E6DF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00EBD5C900D09B7C00F4C5A400F4C5A400F4C5A500F7CBAC00CC91
      6E00B18F7C000000000000000000000000000000000000000000C2C2C200C1C1
      C100DDDDDD00DADADA00DADADA00DADADA00C0C0C000EFEFEF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E5E5E500C2C2C200DADADA00DADADA00DADADA00DDDDDD00BCBC
      BC00BCBCBC000000000000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D00FFFFFF00FFFFFF00949494006B6B6B006B6B6B006B6B6B006B6B6B006C6C
      6C006B6B6B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00FFFFFF00FFFFFF00C0C0C000A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A900A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B576
      5400E7B59300F8CEAF00F4C5A400F4C5A400CB957600E3C6B700FFFBF800FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F1E0D700C9927600ECBB9B00F4C5A400F4C5A400F8CDAF00E0AD8B00AB72
      530000000000000000000000000000000000000000000000000000000000ADAD
      AD00D1D1D100DFDFDF00DADADA00DADADA00BFBFBF00DCDCDC00FCFCFC00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00ECECEC00BEBEBE00D5D5D500DADADA00DADADA00DFDFDF00CCCCCC00AAAA
      AA00000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006B6B6B00FEFEFE00FFFFFF00FFFFFF00FEFEFE00ABAB
      AB00848484000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A9A9A900FEFEFE00FFFFFF00FFFFFF00FEFEFE00CECE
      CE00B7B7B7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B6744F00EDBF9F00F9CFB000F4C5A400EDBE9D00D7A28300C8917400DAB6
      A200EBD6CB00F9F1EC00FFFDFC00FFFFFF00FFFDFC00F9F1EC00ECD7CD00D6AD
      9800CB937600E9B89800F4C5A400F4C5A500F9CFB100E9B89800B3734E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000ACACAC00D6D6D600E0E0E000DADADA00D6D6D600C6C6C600BDBDBD00D3D3
      D300E6E6E600F6F6F600FDFDFD00FFFFFF00FDFDFD00F6F6F600E7E7E700CECE
      CE00BEBEBE00D3D3D300DADADA00DADADA00E0E0E000D3D3D300ABABAB000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D00FFFFFF00FFFFFF006B6B6B00FEFEFE00FFFFFF00FEFEFE00AEAEAE007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00FFFFFF00FFFFFF00A9A9A900FEFEFE00FFFFFF00FEFEFE00D0D0D000B2B2
      B200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B7754F00EABA9A00FCD3B600F5C9A700F4C5A400F1C1A000E1AF
      8F00D5A18200CB957600C68E7000C48C6E00C68E7000CB957600D5A08100E3B1
      9100F2C3A200F4C5A400F6C9A900FCD3B600E6B49300B6744E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000ACACAC00D4D4D400E3E3E300DCDCDC00DADADA00D8D8D800CECE
      CE00C6C6C600BFBFBF00BBBBBB00BABABA00BBBBBB00BFBFBF00C5C5C500CFCF
      CF00D9D9D900DADADA00DCDCDC00E3E3E300D0D0D000ACACAC00000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006B6B6B00FEFEFE00FFFFFF00B2B2B200787878000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A9A9A900FEFEFE00FFFFFF00D2D2D200B1B1B1000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B97A5500D8A17E00FDD5B800FBD2B400F5C8A700F4C5
      A400F4C5A400F4C5A400F4C5A400F4C5A400F4C5A400F4C5A400F4C5A400F4C5
      A400F5C9A900FCD3B500FBD3B600D49B7800BC7E5B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AFAFAF00C5C5C500E3E3E300E2E2E200DCDCDC00DADA
      DA00DADADA00DADADA00DADADA00DADADA00DADADA00DADADA00DADADA00DADA
      DA00DCDCDC00E2E2E200E2E2E200C2C2C200B1B1B10000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006B6B6B00FEFEFE00B7B7B70074747400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A9A9A900FEFEFE00D5D5D500AEAEAE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CEA08400BE7B5400E2AF8E00FED7BB00FED7
      BB00FBD2B400F8CDAD00F7CBAC00F7CBAB00F7CBAC00F8CDAE00FCD2B400FFD7
      BB00FDD6BA00DFAB8800BA785000D2A78F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C5C5C500AFAFAF00CDCDCD00E5E5E500E5E5
      E500E2E2E200DFDFDF00DDDDDD00DDDDDD00DDDDDD00DFDFDF00E2E2E200E5E5
      E500E4E4E400CBCBCB00ADADAD00CACACA000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006C6C6C00B9B9B9007373730000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A9A9A900D6D6D600ADADAD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CC9C7F00B9764E00CE95
      6F00E2B08F00EFC3A500F7CEB300FBD3B800F7CEB100EEC2A300E0AD8C00CD91
      6D00B8764E00CFA2870000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C300ACACAC00BEBE
      BE00CECECE00D9D9D900E0E0E000E3E3E300E0E0E000D8D8D800CCCCCC00BCBC
      BC00ACACAC00C6C6C60000000000000000000000000000000000000000000000
      000000000000000000000000000000000000919191006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B00787878000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BFBFBF00A9A9A900A9A9A900A9A9
      A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A900A9A9A900A9A9A900A9A9A900B1B1B1000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6AE9600C9937400BF815D00BC7C5700BE815D00CA967600D9B29B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CECECE00BEBEBE00B3B3B300B0B0B000B3B3B300BFBFBF00D1D1D1000000
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
      0000000000000000000000000000000000000000000000000000000000009191
      91006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B00929292000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BFBF
      BF00A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A900A9A9A900A9A9A900A9A9A900BFBFBF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000053705D00326446001F6F48001A7248001E6E4700316346004E6C59007D85
      7D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00A9A9A9000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00A9A9A9000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00A9A9A9000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FFFFFF006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FFFFFF00A9A9A9000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE00A9A9A9000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FEFEFE006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FEFEFE00A9A9A9000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB003ED2AD0044D3AF003AD0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      500000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D4D4D400D5D5D500D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE00A9A9A900000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB005AD8B90074ADA0006ADDC0003CD0AC0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200DADADA00C9C9C900DEDEDE00D3D3D300D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FEFEFE006C6C6C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FEFEFE00A9A9A9000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0035C6A30000000000458171006FDEC2003FD2AD0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C600179266000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200CECECE006B6B6B00AEAEAE00DFDFDF00D4D4D400D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE006C6C6C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE00A9A9A9000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0033C19D0000000000000000003162550073DEC30045D3
      AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200CBCBCB006B6B6B006B6B6B009E9E9E00DFDFDF00D5D5
      D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FEFEFE006C6C6C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FEFEFE00A9A9A90000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003BD2AD003CD2AD003CD2AD003CD2AD003CD2AD003CD2
      AD003CD2AD003CD2AD0037C5A20000010000000000000000000022463D0076D9
      C1004BD6B4003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400CDCDCD006B6B6B006B6B6B006B6B6B008F8F8F00DDDD
      DD00D7D7D700D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE006C6C6C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE00A9A9A9000000000031B78C0054E6C0003CD3
      AF003CD3AF0056D9BA008DDECB009BE0D0009BE0D0009BE0D0009BE0D0009BE0
      D0009BE0D0009BE0D00076D6BF00000603000000000000000000000000001830
      290073D0BA0055D9BA003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500DADADA00E3E3E300E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500DCDCDC006D6D6D006B6B6B006B6B6B006B6B6B008484
      8400D9D9D900DADADA00D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FEFEFE006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FEFEFE00A9A9A9006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B30040D0B00008151100010000000100000001000000010000000100
      0000010000000100000001000000010000000100000001000000010000000100
      000013201C006EC3AF005FDDC00040D5B3003ED5B3003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D4D4D400757575006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B007B7B7B00D2D2D200DDDDDD00D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFDFD006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFDFD00A9A9A900429A780058E8C20047DCB90041D7
      B70041D7B7003CCAAB000C181500080604000806040008060400080604000806
      0400080604000806040008060400080604000806040008060400080604000806
      0400080604001116140066B1A1006AE1C80044D8B80041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D1D1D100777777006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E0076767600C9C9C900E0E0E000D8D8D800D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFDFD006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFDFD00A9A9A90028916A0058E9C30047DCBB0045D9
      BA0045D9BA003FCCAE00121E1A000D0B0A000D0B0A000D0B0A000D0B0A000D0B
      0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B
      0A000D0B0A000D0B0A0011121000599B8D0069E2C90047DABB0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900D2D2D2007A7A7A00707070007070700070707000707070007070
      7000707070007070700070707000707070007070700070707000707070007070
      7000707070007070700074747400BEBEBE00E1E1E100D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FCFCFC006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FDFDFD00A9A9A90020936A0058E8C40049DDBF0048DD
      BF0048DDBF0043CEB30018252000141210001412100014121000141210001412
      1000141210001412100014121000141210001412100014121000141210001412
      1000141210001412100014121000141311003AA48F0049DCBE0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DCDCDC00D4D4D4007E7E7E00747474007474740074747400747474007474
      7400747474007474740074747400747474007474740074747400747474007474
      740074747400747474007474740075757500BFBFBF00DBDBDB00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008D8D8D008D8D8D008D8D8D008D8D
      8D000000000000000000B3783E00B3783E000000000000000000A0A0A0006B6B
      6B006B6B6B006B6B6B006C6C6C006B6B6B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BCBCBC00BCBCBC00BCBCBC00BCBC
      BC000000000000000000AAAAAA00AAAAAA000000000000000000C7C7C700A9A9
      A900A9A9A900A9A9A900A9A9A900A9A9A90028956D0056E8C6004DE0C4004BDF
      C4004BDFC40046D0B7001F2A26001B1816001B1816001B1816001B1816001B18
      16001B1816001B1816001B1816001B1816001B1816001B1816001B1816001B18
      16001B1816001B1816001B1B17002F77680043C9B0004BDEC3004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00D5D5D50081818100787878007878780078787800787878007878
      7800787878007878780078787800787878007878780078787800787878007878
      7800787878007878780079797900A8A8A800D2D2D200DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF000000000085AD4B0085AD4B0085AD4B0085AD
      4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD
      4B0085AD4B0085AD4B0085AD4B0000000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000B3783D00AE6F3100AE6F3100B3783E000000000000000000FEFE
      FE00FFFFFF00FEFEFE00ABABAB0084848400BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB0000000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000A9A9A900A4A4A400A4A4A400AAAAAA000000000000000000FEFE
      FE00FFFFFF00FEFEFE00CECECE00B7B7B70042A2800055E6C50052E3CA004FE2
      C9004FE2C90048D3BB0025302B00211E1C00211E1C00211E1C00211E1C00211E
      1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E
      1C00211E1C0022272300368F800047CEB7004FE0C8004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00D7D7D700858585007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B0080808000B5B5B500D5D5D500DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE000000000085AD4B007FA942007BA63C007BA6
      3C007BA63C007EA8410085AD4B0085AD4B0085AD4B0085AD4B0080A944007BA6
      3C007BA63C0080A9440085AD4B00000000008D8D8D008D8D8D00000000000000
      0000B3783D00AE6F3100AE6F3100AE6F3100AE6F3100B3783E00000000000000
      0000FFFFFF00AEAEAE007B7B7B0000000000BBBBBB00B8B8B800B5B5B500B5B5
      B500B5B5B500B7B7B700BBBBBB00BBBBBB00BBBBBB00BBBBBB00B8B8B800B5B5
      B500B5B5B500B8B8B800BBBBBB0000000000BCBCBC00BCBCBC00000000000000
      0000A9A9A900A4A4A400A4A4A400A4A4A400A4A4A400AAAAAA00000000000000
      0000FFFFFF00D0D0D000B2B2B200000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0049CFB9002B3B3500292B2800292B2800292B2800292B2800292B
      2800292B2800292B2800292B2800272521002723210027232100272321002723
      21002A3632003DA492004BD5BF0052E3CC0052E4CC0052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B3006893800000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100D6D6D6008B8B8B00828282008282820082828200828282008282
      82008282820082828200828282007F7F7F007E7E7E007E7E7E007E7E7E007E7E
      7E0088888800BFBFBF00D9D9D900E0E0E000E1E1E100E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA00000000007FA94200D9E6C800FFFFFF00FFFF
      FF00FFFFFF00D5E2C1007AA53A0085AD4B0085AD4B007DA73F00BCD29C00FFFF
      FF00FFFFFF00C4D7A80080A9440000000000000000000000000000000000B277
      3D00AE6F3100AE703200AE6F3100AE6F3100AE703200AE6F3100B3783E000000
      000000000000787878000000000000000000B8B8B800EAEAEA00FFFFFF00FFFF
      FF00FFFFFF00E7E7E700B5B5B500BBBBBB00BBBBBB00B7B7B700D9D9D900FFFF
      FF00FFFFFF00DDDDDD00B8B8B80000000000000000000000000000000000A9A9
      A900A4A4A400A4A4A400A4A4A400A4A4A400A4A4A400A4A4A400AAAAAA000000
      000000000000B1B1B1000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00051DDC80049CAB70047C6B40047C6B40047C6B40047C6B40047C6
      B40047C6B40047C6B40045C0AE002F322F002D2927002D2927002D2927003249
      440044B5A30051DCC60054E6D00055E7D00055E7D00055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300DDDDDD00D4D4D400D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200CECECE00868686008282820082828200828282009292
      9200C9C9C900DDDDDD00E2E2E200E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C700000000000000000084AC4A007BA63D00EDF3E500FFFF
      FF00FFFFFF00FFFFFF00BBD29B007DA73F0080A94300A2C17700FFFFFF00FFFF
      FF00DCE7CD0079A53A0085AD4B00000000000000000000000000B3783E00AE6F
      3100B47B420000000000AE6F3100AE6F310000000000AF713400AE6F3100B378
      3E0000000000000000000000000000000000BBBBBB00B5B5B500F5F5F500FFFF
      FF00FFFFFF00FFFFFF00D9D9D900B7B7B700B8B8B800CBCBCB00FFFFFF00FFFF
      FF00EBEBEB00B4B4B400BBBBBB00000000000000000000000000AAAAAA00A4A4
      A400ABABAB0000000000A4A4A400A4A4A40000000000A5A5A500A4A4A400AAAA
      AA000000000000000000000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8
      D40058E8D40058E8D40050D7C30034393400322F2B00322F2B00395E560048C2
      B00054E1CD0058E8D40058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E4E4E400DBDBDB008A8A8A0085858500858585009D9D9D00D0D0
      D000E0E0E000E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B500000000000000000085AD4B0083AC490082AB4700FCFD
      FC00FFFFFF00FFFFFF00FFFFFF00A0C074008CB15500FFFFFF00FFFFFF00F1F5
      EA007BA73E0084AC4A0085AD4B00000000000000000000000000B3783E00B67D
      46000000000000000000AE6F3100AE6F31000000000000000000AF713400B378
      3E0000000000000000000000000000000000BBBBBB00BABABA00B9B9B900FDFD
      FD00FFFFFF00FFFFFF00FFFFFF00CACACA00BFBFBF00FFFFFF00FFFFFF00F6F6
      F600B6B6B600BBBBBB00BBBBBB00000000000000000000000000AAAAAA00ADAD
      AD000000000000000000A4A4A400A4A4A4000000000000000000A5A5A500AAAA
      AA0000000000000000000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEA
      D7005AEAD7005AEAD70052D9C800393C3900373330003F766D004DCBBA0058E6
      D3005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500DCDCDC008C8C8C0088888800AAAAAA00D5D5D500E3E3
      E300E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B700000000000000000085AD4B0085AD4B0082AB46008FB4
      5B00FFFFFF00FFFFFF00FFFFFF00FDFDFF00F5F7F000FFFFFF00FFFFFF0085AD
      4C0083AC480085AD4B0085AD4B00000000000000000000000000B3783E000000
      00000000000000000000AE6F3100B0743800000000000000000000000000B378
      3E0000000000000000000000000000000000BBBBBB00BBBBBB00B9B9B900C1C1
      C100FFFFFF00FFFFFF00FFFFFF00FDFDFD00F8F8F800FFFFFF00FFFFFF00BBBB
      BB00BABABA00BBBBBB00BBBBBB00000000000000000000000000AAAAAA000000
      00000000000000000000A4A4A400A7A7A700000000000000000000000000AAAA
      AA0000000000000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80052D7C5003C433E00448D820050D2C1005AE9D6005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600DBDBDB0090909000B5B5B500D9D9D900E5E5E500E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA0000000000000000000000000085AD4B0085AD4B0085AD4B0080A9
      4300A3C27900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0094B8620081AA
      460085AD4B0085AD4B0085AD4B00000000000000000000000000000000000000
      000000000000CBA47C00AE6F3100C19061000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00B8B8
      B800CCCCCC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C4C4C400B9B9
      B900BBBBBB00BBBBBB00BBBBBB00000000000000000000000000000000000000
      000000000000C5C5C500A4A4A400B9B9B9000000000000000000000000000000
      000000000000000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80052D7C50048A99B0052D7C6005AEAD7005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600DBDBDB00C3C3C300DBDBDB00E5E5E500E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C30000000000000000000000000085AD4B0085AD4B0085AD4B0085AD
      4B007DA73F00B9D09800FFFFFF00FFFFFF00FFFFFF00EAF0E00078A4370084AC
      4A0085AD4B0085AD4B0085AD4B00000000000000000000000000000000000000
      000000000000AF713500AE6F3100000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00B7B7B700D8D8D800FFFFFF00FFFFFF00FFFFFF00F2F2F200B4B4B400BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00000000000000000000000000000000000000
      000000000000A5A5A500A4A4A400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005AE9D60059E8D5005AEAD7005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B0000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E5E5E500E4E4E400E5E5E500E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B7000000000000000000000000000000000085AD4B0085AD4B0085AD4B0085AD
      4B007DA73F00B7CE9600FFFFFF00FFFFFF00FFFFFF00FFFFFF00D4E3C2007AA5
      3A0085AD4B0085AD4B0085AD4B00000000000000000000000000000000000000
      0000B57D4500AE6F3100BA844F00000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00B7B7B700D6D6D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7E7E700B5B5
      B500BBBBBB00BBBBBB00BBBBBB00000000000000000000000000000000000000
      0000ACACAC00A4A4A400B1B1B100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      00000000000000000000000000000000000085AD4B0085AD4B0085AD4B0080A9
      4300A2C17700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BAD0
      99007DA73F0085AD4B0085AD4B00000000000000000000000000CBA47C00AF71
      3500AE6F3100B2773D0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00B8B8
      B800CBCBCB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D8D8
      D800B7B7B700BBBBBB00BBBBBB00000000000000000000000000C5C5C500A5A5
      A500A4A4A400A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      00000000000000000000000000000000000085AD4B0085AD4B0082AB46008FB4
      5B00FFFFFF00FFFFFF00F3F6EE0081AA4500FDFDFD00FFFFFF00FFFFFF00FFFF
      FF00A1BF740080A9440085AD4B000000000000000000AE6F3100AE6F3100AE6F
      3100B9834E000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00B9B9B900C1C1
      C100FFFFFF00FFFFFF00F8F8F800B9B9B900FDFDFD00FFFFFF00FFFFFF00FFFF
      FF00CACACA00B8B8B800BBBBBB000000000000000000A4A4A400A4A4A400A4A4
      A400B1B1B1000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      00000000000000000000000000000000000085AD4B0083AC490081AB4700FCFD
      FB00FFFFFF00FFFFFF0087AE4F0080A9440090B55E00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008DB3570082AB47000000000000000000B0723600C08F5F000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BABABA00B9B9B900FDFD
      FD00FFFFFF00FFFFFF00BCBCBC00B8B8B800C2C2C200FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C0C0C000B9B9B9000000000000000000A6A6A600B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      00000000000000000000000000000000000084AC4A0081AA4600F6F9F300FFFF
      FF00FFFFFF0098BA690081AA450085AD4B007FA94300A6C27C00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0087AE4F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00B9B9B900F9F9F900FFFF
      FF00FFFFFF00C6C6C600B9B9B900BBBBBB00B8B8B800CDCDCD00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BCBCBC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000085AD4A0084AC4B0080AA44007EA8
      40007DA7400082AB470085AD4B0085AD4B0085AD4B0081AA45007DA741007EA8
      40007EA840007FA9420085AC4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00B8B8B800B7B7
      B700B7B7B700B9B9B900BBBBBB00BBBBBB00BBBBBB00B9B9B900B7B7B700B7B7
      B700B7B7B700B8B8B800BBBBBB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000085AD4B0085AD4A0084AC4A0084AC
      4A0084AC4A0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0084AC4A0084AC
      4A0084AC4A0084AC4A0084AD4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00000000000000000000000000000000000000
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
      000053705D00326446001F6F48001A7248001E6E4700316346004E6C59007D85
      7D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000053705D00326446001F6F48001A7248001E6E4700316346004E6C59007D85
      7D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB003BD0AC003FD2AD003AD0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D3D3D300D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB003DD2AC0073DFC40079B8A90063DABC003AD0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      500000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D4D4D400E0E0E000CECECE00DCDCDC00D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A300000000000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB003AD0AB0044D3AF003ED2AD0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      500000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D5D5D500D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A30000000000000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003BD0AB0060DABB00307060000000000049A08B0056D8B80039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200DBDBDB00A4A4A4006B6B6B00BEBEBE00D9D9D900D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE00000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003CD0AC006ADDC00074ADA0005AD8B90039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D3D3D300DEDEDE00C9C9C900DADADA00D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB005AD8B9003F917D0000000000000000000000000054B69D0050D6B50039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C600179266000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200DADADA00B5B5B5006B6B6B006B6B6B006B6B6B00C9C9C900D8D8D800D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD2
      AD006FDEC200458171000000000035C6A30039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C600179266000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4
      D400DFDFDF00AEAEAE006B6B6B00CECECE00D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF000000000000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0052D7
      B6004FAE96000000000000000000000000000000000001110C005DC8AE004AD5
      B30039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D8D8
      D800C5C5C5006B6B6B006B6B6B006B6B6B006B6B6B0072727200D3D3D300D6D6
      D600D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD0000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0045D3AF0073DE
      C30031625500000000000000000033C19D0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D5D5D500DFDF
      DF009E9E9E006B6B6B006B6B6B00CBCBCB00D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD00000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC004ED6B5005BC3
      AA00020D0A0000000000000000000000000000000000000000000E28210064D5
      B90046D5B1003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D8D8D800D0D0
      D000707070006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B007E7E7E00D9D9
      D900D6D6D600D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB00000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC004BD6B40076D9C1002245
      3D0000000000000000000001000037C5A2003CD2AD003CD2AD003CD2AD003CD2
      AD003CD2AD003CD2AD003CD2AD003BD2AD003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D7D7D700DDDDDD008F8F
      8F006B6B6B006B6B6B006B6B6B00CDCDCD00D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB0000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF0048D6B50064D4B9001026
      1F00000000000000000000000000000000000000000000000000000000001D45
      3A0068DDC10043D5B3003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D7D7D700D9D9D9007E7E
      7E006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B008E8E
      8E00DEDEDE00D6D6D600D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C30000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF0055D9BA0073D0BA00183029000000
      000000000000000000000006030076D6BF009BE0D0009BE0D0009BE0D0009BE0
      D0009BE0D0009BE0D0009BE0D0008DDECB0056D9BA003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500DADADA00D9D9D900848484006B6B
      6B006B6B6B006B6B6B006D6D6D00DCDCDC00E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500E5E5E500E3E3E300DADADA00D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C30000000000000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B3003ED5B3003ED5B3003ED5B30046D7B60069DDC3001F433A000100
      0000010000000100000001000000010000000100000001000000010000000100
      00002F64580068DFC40043D6B4003ED5B3003ED5B3003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D8D8D800DFDFDF008D8D8D006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B009F9F9F00DFDFDF00D7D7D700D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B400000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B3003ED5B30040D5B3005FDDC0006EC3AF0013201C00010000000100
      0000010000000100000001000000010000000100000001000000010000000100
      00000100000001000000010000000815110040D0B0003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D6D6D600D6D6D600DDDDDD00D2D2D2007B7B7B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B0075757500D4D4D400D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B40000000000429A780058E8C20047DCB90041D7
      B70041D7B70041D7B70041D7B70045D8B8006AE0C80031655900070604000706
      0400070604000706040007060400070604000706040007060400070604000706
      0400070604004086770067E0C50043D7B70041D7B70041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800E0E0E0009F9F9F006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E00B1B1B100E0E0E000D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA0000000000429A780058E8C20047DCB90041D7
      B70041D7B70044D8B8006AE1C80066B1A1001116140008060400080604000806
      0400080604000806040008060400080604000806040008060400080604000806
      04000806040008060400080604000C1815003CCAAB0041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D8D8D800E0E0E000C9C9C900767676006E6E6E006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E0077777700D1D1D100D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA000000000028916A0058E9C30047DCBB0045D9
      BA0045D9BA0045D9BA0046DABB0066E1C800448879000D0B09000D0B09000D0B
      09000D0B09000D0B09000D0B09000D0B09000D0B09000D0B09000D0B09000D0B
      09000D0B09000E0D0C0051A3910062E0C50046D9BA0045D9BA0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900D9D9D900D9D9D900E0E0E000B2B2B20070707000707070007070
      7000707070007070700070707000707070007070700070707000707070007070
      70007070700072727200C0C0C000DFDFDF00D9D9D900D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC000000000028916A0058E9C30047DCBB0045D9
      BA0047DABB0069E2C900599B8D00111210000D0B0A000D0B0A000D0B0A000D0B
      0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B
      0A000D0B0A000D0B0A000D0B0A00121E1A003FCCAE0045D9BA0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900E1E1E100BEBEBE00747474007070700070707000707070007070
      7000707070007070700070707000707070007070700070707000707070007070
      70007070700070707000707070007A7A7A00D2D2D200D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC000000000020936A0058E8C40049DDBF0048DD
      BF0048DDBF0048DDBF004BDCC0003A9F89001313110013110E0013110E001311
      0E0013110E0013110E0013110E0013110E0013110E0013110E0013110E001311
      0E0013110E0013110E00151A16003FB19A004ADDBF0048DDBF0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DCDCDC00DCDCDC00DCDCDC00BCBCBC007575750074747400747474007474
      7400747474007474740074747400747474007474740074747400747474007474
      7400747474007474740079797900C6C6C600DCDCDC00DCDCDC00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD000000000020936A0058E8C40049DDBF0048DD
      BF0049DCBE003AA48F0014131100141210001412100014121000141210001412
      1000141210001412100014121000141210001412100014121000141210001412
      10001412100014121000141210001825200043CEB30048DDBF0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DBDBDB00BFBFBF0075757500747474007474740074747400747474007474
      7400747474007474740074747400747474007474740074747400747474007474
      74007474740074747400747474007E7E7E00D4D4D400DCDCDC00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD000000000028956D0056E8C6004DE0C4004BDF
      C4004BDFC4004BDFC4004ADDC20043C8AF003EBFA7003EBEA6003EBEA6003BB4
      9F001816140018161400181614001816140018161400181614001A1D1B003DB9
      A3003EBEA6003EBEA6003FBFA70044CAB1004ADEC3004BDFC4004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DCDCDC00D1D1D100CDCDCD00CCCCCC00CCCCCC00C7C7
      C7007777770077777700777777007777770077777700777777007B7B7B00CACA
      CA00CCCCCC00CCCCCC00CDCDCD00D2D2D200DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF000000000028956D0056E8C6004DE0C4004BDF
      C4004BDEC30043C9B0002F7769001B1B17001B1816001B1816001B1816001B18
      16001B1816001B1816001B1816001B1816001B1816001B1816001B1816001B18
      16001B1816001B1816001B1816001F2A260046D0B7004BDFC4004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00D2D2D200A8A8A800797979007878780078787800787878007878
      7800787878007878780078787800787878007878780078787800787878007878
      780078787800787878007878780081818100D5D5D500DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE1C90046CA
      B4001E1C1A001E1C1A001E1C1A001E1C1A001E1C1A001E1C1A002022200048D0
      B9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00D3D3
      D3007A7A7A007A7A7A007A7A7A007A7A7A007A7A7A007A7A7A007E7E7E00D6D6
      D600DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C9004FE0C80047CEB700368F800022272300211E1C00211E1C00211E
      1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E
      1C00211E1C00211E1C00211E1C0025302B0048D3BB004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00DFDFDF00D5D5D500B5B5B500808080007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B0085858500D7D7D700DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE00000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E3CC0049CD
      B70023201E0023201E0023201E0023201E0023201E0023201E00262825004BD4
      BE0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B3006893800000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E0E0E000D5D5
      D5007D7D7D007D7D7D007D7D7D007D7D7D007D7D7D007D7D7D0081818100D8D8
      D800E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA00000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0052E4CC0052E3CC004BD5BF003DA492002A363200272321002723
      2100272321002723210027252100292B2800292B2800292B2800292B2800292B
      2800292B2800292B2800292B28002B3B350049CFB90052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B3006893800000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100E1E1E100E0E0E000D9D9D900BFBFBF00888888007E7E7E007E7E
      7E007E7E7E007E7E7E007F7F7F00828282008282820082828200828282008282
      82008282820082828200828282008B8B8B00D6D6D600E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA0000000000000000002CBB98005BE9D00055E7
      D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00054E6D0004DCF
      BB002926220029262200292622002926220029262200292622002A2D29004ED6
      C20055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E2E2E200D6D6
      D60080808000808080008080800080808000808080008080800084848400DADA
      DA00E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C7000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00055E7D00055E7D00054E6D00051DCC60044B5A300324944002D29
      27002D2927002D2927002F322F0045C0AE0047C6B40047C6B40047C6B40047C6
      B40047C6B40047C6B40047C6B40049CAB70051DDC80055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300E3E3E300E3E3E300E2E2E200DDDDDD00C9C9C900929292008282
      8200828282008282820086868600CECECE00D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DDDDDD00E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C700000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D3004FD2
      BF002F2A28002F2A28002F2A28002F2A28002F2A28002F2A280030322F0050D7
      C40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400D8D8
      D80082828200828282008282820082828200828282008282820086868600DBDB
      DB00E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B500000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40054E1CD0048C2B000395E
      5600322F2B00322F2B003439340050D7C30058E8D40058E8D40058E8D40058E8
      D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E0E0E000D0D0D0009D9D
      9D0085858500858585008A8A8A00DBDBDB00E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B5000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70059E9D60050D3
      C100332F2B00332F2B00332F2B00332F2B00332F2B00332F2B003436320052D9
      C8005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E4E4E400D9D9
      D90085858500858585008585850085858500858585008585850089898900DCDC
      DC00E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B7000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70058E6D3004DCB
      BA003F766D0037333000393C390052D9C8005AEAD7005AEAD7005AEAD7005AEA
      D7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E3E3E300D5D5
      D500AAAAAA00888888008C8C8C00DCDCDC00E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B7000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005AEAD70051D4
      C200373330003733300037333000373330003733300037333000393A360053DA
      C9005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E5E5E500D9D9
      D9008888880088888800888888008888880088888800888888008B8B8B00DDDD
      DD00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA00000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005AE9
      D60050D2C100448D82003C433E0052D7C5005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E5E5
      E500D9D9D900B5B5B50090909000DBDBDB00E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA00000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005AEAD70051D4
      C2003B3633003B3633003B3633003B3633003B3633003B3633003C3D390053D9
      C8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E5E5E500D9D9
      D9008989890089898900898989008989890089898900898989008D8D8D00DCDC
      DC00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C300000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005AEAD70052D7C60048A99B0052D7C5005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E5E5E500DBDBDB00C3C3C300DBDBDB00E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C30000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEAD7004FCD
      BC003E403C003E403C003E403C003E403C003E403C003E403C003E46400050D4
      C2005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B0000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600D6D6
      D6008F8F8F008F8F8F008F8F8F008F8F8F008F8F8F008F8F8F0091919100D9D9
      D900E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B7000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005AEAD70059E8D5005AE9D6005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B0000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E5E5E500E4E4E400E5E5E500E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B700000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEAD70055DE
      CC004ECCBB004DCAB9004DCAB9004DCAB9004DCAB9004DCAB9004ECDBB0055E0
      CD005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600DFDF
      DF00D5D5D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D6D6D600E0E0
      E000E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
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
      000056567B0038386F002C2C79002A2A7C002B2B780037376F00515176007E7E
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A3A3A3009494940091919100919191009090900094949400A0A0A000B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000053705D00326446001F6F48001A7248001E6E4700316346004E6C59007D85
      7D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A5A870027278A003F3F
      B1005757D1006666E6006A6AEB006A6AEB006A6AEB006666E6005757D1003F3F
      B100272789005454820000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A6A6A60092929200A3A3
      A300B2B2B200BCBCBC00BEBEBE00BEBEBE00BEBEBE00BCBCBC00B2B2B200A3A3
      A30092929200A3A3A30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000062628D002A2A94005959D4006A6AEB006363
      E5005959DE005252D8004F4FD6004F4FD6004F4FD6005252D8005959DE006363
      E5006A6AEB005959D4002A2A9400595985000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000ABABAB0094949400B3B3B300BEBEBE00BABA
      BA00B5B5B500B1B1B100B0B0B000B0B0B000B0B0B000B1B1B100B5B5B500BABA
      BA00BEBEBE00B3B3B30094949400A6A6A6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002E2E80005050C8006969EA005D5DE1004D4DD4004747
      D0004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      D0004D4DD4005D5DE1006969EA005050C8002B2B7E0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000093939300ADADAD00BEBEBE00B7B7B700AEAEAE00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00AEAEAE00B7B7B700BEBEBE00ADADAD009191910000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000025258A006161E0006464E6004E4ED5004747D0004747D0004747
      D0004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      D0004747D0004747D0004E4ED5006464E6006161DF0024248800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000091919100B8B8B800BBBBBB00AFAFAF00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00AFAFAF00BBBBBB00B8B8B80091919100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000026268C006565E4005F5FE2004848D1004747D0004747D0004747D0004747
      D0004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      D0004747D0004747D0004747D0004848D1005F5FE3006565E400252589000000
      0000000000000000000000000000000000000000000000000000000000000000
      000092929200BBBBBB00B8B8B800ACACAC00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ACACAC00B8B8B800BBBBBB00919191000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000002E2E
      84006161E0005F5FE2004747D0004747D0004747D0004747D0004747D0004747
      D0004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      D0004747D0004747D0004747D0004747D0004747D0005F5FE3006161DF002B2B
      8100000000000000000000000000000000000000000000000000000000009494
      9400B8B8B800B8B8B800ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00B8B8B800B8B8B8009292
      9200000000000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0058D8
      B8008EE6D00099E9D50099E9D50099E9D50099E9D50099E9D5008CE6CF0054D7
      B70039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      500000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D9D9
      D900E6E6E600E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E6E6E600D9D9
      D900D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A3000000000000000000000000000000000000000000000000006C6CAC005050
      CA006464E6004848D0004747D0004747D0004747D0004747D0004242B8004646
      C3004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      C3004242B9004747D0004747D0004747D0004747D0004848D1006464E6004F4F
      C9005A5A8A000000000000000000000000000000000000000000B4B4B400AEAE
      AE00BBBBBB00ACACAC00ABABAB00ABABAB00ABABAB00ABABAB00A5A5A500A9A9
      A900ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00A9A9
      A900A5A5A500ABABAB00ABABAB00ABABAB00ABABAB00ACACAC00BBBBBB00ADAD
      AD00A7A7A700000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003CC8
      A400000000000000000000000000000000000000000000000000000100003DCD
      AA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200CFCF
      CF006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE0000000000000000000000000000000000000000002B2B9B006969
      EA004D4DD4004747D0004747D0004747D0004747D0004544B700AFABCD007C7A
      B5004B4AC5004747D0004747D0004747D0004747D0004747D0004B4BC5008685
      BA00ACA9CC004544B9004747D0004747D0004747D0004747D0004D4DD5006969
      EA0029299900000000000000000000000000000000000000000096969600BEBE
      BE00AEAEAE00ABABAB00ABABAB00ABABAB00ABABAB00A6A6A600D4D4D400BBBB
      BB00AAAAAA00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00C0C0
      C000D2D2D200A6A6A600ABABAB00ABABAB00ABABAB00ABABAB00AEAEAE00BEBE
      BE00959595000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0037CFAA0031BA
      98000000000000000000000000000000000000000000000000000000000033C1
      9F0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C600179266000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200C7C7
      C7006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00CBCB
      CB00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF00000000000000000000000000000000006565AC005A5AD7005B5B
      DF004747D0004747D0004747D0004747D0004544B700AFABCE00FFFAF800FFFA
      F8007C7AB6004B4BC5004747D0004747D0004747D0004B4BC5008685BA00FFFA
      F800FFFAF800ACAACD004545B8004747D0004747D0004747D0004747D0005B5B
      E0005858D60055558800000000000000000000000000B1B1B100B4B4B400B6B6
      B600ABABAB00ABABAB00ABABAB00ABABAB00A6A6A600D4D4D400FBFBFB00FBFB
      FB00BCBCBC00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00C0C0C000FBFB
      FB00FBFBFB00D3D3D300A6A6A600ABABAB00ABABAB00ABABAB00ABABAB00B6B6
      B600B3B3B300A4A4A40000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0037CFAA0031B9
      98000000000000000000000000000000000000000000000000000000000033C1
      9D0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200C7C7
      C7006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00CBCB
      CB00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD00000000000000000000000000262694006868EA004C4E
      D400484AD100484AD100484AD1004243B700AFACCE00FFFBF900FFFBF900FFFB
      F900FFFBF9007C7BB6004B4DC600484AD1004B4CC5008785BA00FFFBF900FFFB
      F900FFFBF900FFFBF900ADABCD004546BB00484AD100484AD100484AD1004D4E
      D4006868EA002525940000000000000000000000000094949400BDBDBD00AEAE
      AE00ADADAD00ADADAD00ADADAD00A5A5A500D4D4D400FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00BCBCBC00ACACAC00ADADAD00ACACAC00C1C1C100FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00D4D4D400A7A7A700ADADAD00ADADAD00ADADAD00AEAE
      AE00BDBDBD0093939300000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD0AC0032BB
      99000000000000000000000000000000000000000000000000000000000034C2
      A0003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D3D3D300C8C8
      C8006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00CBCB
      CB00D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB000000000000000000000000004040B9006061E3004A4F
      D2004A4FD2004A4FD2004A4FD2004346B600B1AFCF00FFFBF900FFFBF900FFFB
      F900FFFBF900FFFBF9007C7CB600474AB8008786BB00FFFBF900FFFBF900FFFB
      F900FFFBF900FFFBF9008987BB004B4EC5004A4FD2004A4FD2004A4FD2004A4F
      D2006061E3003F3FB800000000000000000000000000A5A5A500B9B9B900AEAE
      AE00AEAEAE00AEAEAE00AEAEAE00A6A6A600D5D5D500FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00BCBCBC00A8A8A800C1C1C100FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00C2C2C200ACACAC00AEAEAE00AEAEAE00AEAEAE00AEAE
      AE00B9B9B900A4A4A40000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF0034BC
      9C000000000000000000000000000000000000000000000000000001000036C3
      A2003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500C9C9
      C9006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00CDCD
      CD00D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C30000000000000000007979BA005657D500575CDC004C55
      D4004C55D4004C55D4004C55D4004C55D400494EBA00B1AFD000FFFBFA00FFFB
      FA00FFFBFA00FFFBFA00FFFBFA00CDCADF00FFFBFA00FFFBFA00FFFBFA00FFFB
      FA00FFFBFA008988BC005057C9004C55D4004C55D4004C55D4004C55D4004C55
      D400575DDD005656D4006767920000000000BCBCBC00B3B3B300B5B5B500B1B1
      B100B1B1B100B1B1B100B1B1B100B1B1B100AAAAAA00D6D6D600FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00E3E3E300FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00C2C2C200B0B0B000B1B1B100B1B1B100B1B1B100B1B1B100B1B1
      B100B6B6B600B2B2B200AEAEAE00000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B30036BF
      A0000000000000000000000000000000000000000000000000000107040039C5
      A5003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600CBCB
      CB006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006E6E6E00CECE
      CE00D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B400000000004C4CA1006365E500545EDA004F5B
      D6004F5BD6004F5BD6004F5BD6004F5BD6004F5BD6004B53BB00B1B1D100FFFC
      FA00FFFCFA00FFFCFA00FFFCFA00FFFCFA00FFFCFA00FFFCFA00FFFCFA00FFFC
      FA00898ABC00535CCA004F5BD6004F5BD6004F5BD6004F5BD6004F5BD6004F5B
      D600545EDA006365E5004040860000000000A5A5A500BBBBBB00B5B5B500B4B4
      B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400ACACAC00D6D6D600FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00C3C3C300B2B2B200B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4
      B400B5B5B500BBBBBB009C9C9C0000000000429A780058E8C20047DCB90041D7
      B70041D7B70041D7B70041D7B70041D7B70041D7B70041D7B70041D7B7003AC1
      A300040201000402010004020100040201000402010004020100070B09003CC8
      AA0041D7B70041D7B70041D7B70041D7B70041D7B70041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800CCCC
      CC006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B0070707000D0D0
      D000D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA0000000000343598006368E6005463DA005263
      D8005263D8005263D8005263D8005263D8005263D8005263D8004D58BC00B2B2
      D200FFFCFB00FFFCFB00FFFCFB00FFFCFB00FFFCFB00FFFCFB00FFFCFB008A8B
      BD005563CC005263D8005263D8005263D8005263D8005263D8005263D8005263
      D8005463DA006368E60030319100000000009A9A9A00BCBCBC00B7B7B700B7B7
      B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700AEAEAE00D7D7
      D700FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00C3C3
      C300B5B5B500B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7
      B700B7B7B700BCBCBC00979797000000000028916A0058E9C30047DCBB0045D9
      BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA003DC4
      A7000A0807000A0807000A0807000A0807000A0807000A0807000C110D0040CC
      AE0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900CECE
      CE006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F0073737300D2D2
      D200D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC00000000002F309900636AE500566ADB00556A
      DB00556ADB00556ADB00556ADB00556ADB00556ADB00556ADB00556ADB004854
      B300F0EFF300FFFCFB00FFFCFB00FFFCFB00FFFCFB00FFFCFB00CDCDE1004F5D
      BF00556ADB00556ADB00556ADB00556ADB00556ADB00556ADB00556ADB00556A
      DB00566ADB00626AE5002D2D95000000000098989800BCBCBC00BABABA00BABA
      BA00BABABA00BABABA00BABABA00BABABA00BABABA00BABABA00BABABA00ABAB
      AB00F6F6F600FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00E4E4E400B0B0
      B000BABABA00BABABA00BABABA00BABABA00BABABA00BABABA00BABABA00BABA
      BA00BABABA00BCBCBC00969696000000000020936A0058E8C40049DDBF0048DD
      BF0048DDBF0048DDBF004FDEC1007DE8D300A4EFE100A6F1E100A6F1E10077DF
      CA00100D0C00100D0C00100D0C00100D0C00100D0C00100D0C001316140080E7
      D300A6F1E100A6F1E100A3EFE00077E6CF004DDDC00048DDBF0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DCDCDC00DCDCDC00DDDDDD00E6E6E600EEEEEE00EEEEEE00EEEEEE00E1E1
      E10072727200727272007272720072727200727272007272720077777700E6E6
      E600EEEEEE00EEEEEE00EDEDED00E4E4E400DCDCDC00DCDCDC00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD000000000034359B00626DE5005971DE005872
      DD005872DD005872DD005872DD005872DD005872DD005872DD00586DCF00898D
      BE00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC007F84
      B900596ED0005872DD005872DD005872DD005872DD005872DD005872DD005872
      DD005971DE00626DE50031329500000000009B9B9B00BEBEBE00BDBDBD00BEBE
      BE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00B9B9B900C5C5
      C500FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00C0C0
      C000B9B9B900BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBE
      BE00BDBDBD00BEBEBE00989898000000000028956D0056E8C6004DE0C4004BDF
      C4004BDFC4004BDFC4004ADDC200369684001615130015131200151312001513
      1200151312001513120015131200151312001513120015131200151312001513
      12001513120015131200171B18003BA995004BDEC3004BDFC4004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DCDCDC00B8B8B8007676760075757500757575007575
      7500757575007575750075757500757575007575750075757500757575007575
      7500757575007575750079797900C2C2C200DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF00000000004B4CA700616FE3005E79E1005B79
      E0005B79E0005B79E0005B79E0005B79E0005B79E0005B73D2008A8EBF00FFFD
      FC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFD
      FC008085BA005B74D3005B79E0005B79E0005B79E0005B79E0005B79E0005B79
      E0005E79E100606FE30041418C0000000000A6A6A600BEBEBE00C1C1C100C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000BCBCBC00C5C5C500FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00C0C0C000BCBCBC00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C1C1C100BEBEBE009D9D9D000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C9004FE2C9004FE1C90047CDB700307769001C1816001C1816001C18
      16001C1816001C1816001C1816001C1816001C1816001C1816001C1816001C18
      16001C1816001C1B1800348B7B0048D2BA004FE1C9004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00D5D5D500A8A8A80078787800787878007878
      7800787878007878780078787800787878007878780078787800787878007878
      7800787878007A7A7A00B3B3B300D7D7D700DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE00000000007879C0005262D400617EE3005E80
      E2005E80E2005E80E2005E80E2005E80E2005E79D4008A90BF00FFFDFD00FFFD
      FD00FFFDFD00FFFDFD00FFFDFD00F2F2F600FFFDFD00FFFDFD00FFFDFD00FFFD
      FD00FFFDFD008087BB005E7AD5005E80E2005E80E2005E80E2005E80E2005E80
      E200617EE3005161D30069699A0000000000BDBDBD00B6B6B600C3C3C300C3C3
      C300C3C3C300C3C3C300C3C3C300C3C3C300BFBFBF00C6C6C600FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00F8F8F800FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00C1C1C100BFBFBF00C3C3C300C3C3C300C3C3C300C3C3C300C3C3
      C300C3C3C300B5B5B500B0B0B000000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0052E4CC0052E4CC0051E2CB0047CBB5002F635900211E1C00211E
      1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E
      1C00211E1C00347A6E0049CFB90052E3CB0052E4CC0052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B3006893800000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100E1E1E100E1E1E100E0E0E000D4D4D4009F9F9F007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B00AAAAAA00D6D6D600E0E0E000E1E1E100E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA0000000000000000003C4ABD006684E6006187
      E4006187E4006187E4006187E4005D7CD3008B92C000FFFEFD00FFFEFD00FFFE
      FD00FFFEFD00FFFEFD008D93C1005066BA00B3B6D400FFFEFD00FFFEFD00FFFE
      FD00FFFEFD00FFFEFD008189BB005E7DD5006187E4006187E4006187E4006187
      E4006683E6003B49BC00000000000000000000000000A8A8A800C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600BFBFBF00C6C6C600FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00C7C7C700B3B3B300D9D9D900FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00C2C2C200C0C0C000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600A8A8A8000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00055E7D00055E7D00055E7D00054E3CE0048C5B4002F514A002723
      2000272320002723200027232000272320002723200027232000272320002723
      200033685E004ACDB90054E4CF0055E7D00055E7D00055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E1E1E100D1D1D100969696007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E00A1A1A100D5D5D500E2E2E200E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C700000000000000000000000000232AA1006486E600678E
      E600638DE500638DE500638DE5004E67B800DADBEA00FFFEFE00FFFEFE00FFFE
      FE00FFFEFE008D94C1006386D700638DE5005975C600B3B7D500FFFEFE00FFFE
      FE00FFFEFE00FFFEFE00B3B7D5005876C800638DE500638DE500638DE500678E
      E6006486E6002329A10000000000000000000000000096969600C6C6C600C9C9
      C900C9C9C900C9C9C900C9C9C900B3B3B300ECECEC00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00C7C7C700C4C4C400C9C9C900BBBBBB00D9D9D900FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00D9D9D900BBBBBB00C9C9C900C9C9C900C9C9C900C9C9
      C900C6C6C60096969600000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40055E3CF0047BFAD003145
      3F002C2826002C2826002C2826002C2826002C2826002C2826002C2826003459
      51004AC9B70056E6D20058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E1E1E100CECECE009090
      9000818181008181810081818100818181008181810081818100818181009A9A
      9A00D4D4D400E3E3E300E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B5000000000000000000000000006467B9005675D8006B92
      E8006591E7006591E7006591E7006591E600526AB700DADCEA00FFFEFE00FFFE
      FE008D94C1006589D9006591E7006591E7006591E7005A78C700B4B8D500FFFE
      FE00FFFEFE00B4B8D5005B78C8006591E7006591E7006591E7006591E7006B92
      E8005474D7005A5A9900000000000000000000000000B4B4B400BEBEBE00CBCB
      CB00CACACA00CACACA00CACACA00CACACA00B4B4B400ECECEC00FEFEFE00FEFE
      FE00C7C7C700C5C5C500CACACA00CACACA00CACACA00BCBCBC00DADADA00FEFE
      FE00FEFEFE00DADADA00BCBCBC00CACACA00CACACA00CACACA00CACACA00CBCB
      CB00BCBCBC00A9A9A9000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70055E1CF0047B4
      A500333C3700312C2A00312C2A00312C2A00312C2A00312C2A00354D47004AC2
      B30058E6D3005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E0E0E000C9C9
      C9008C8C8C00848484008484840084848400848484008484840094949400D0D0
      D000E3E3E300E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B700000000000000000000000000000000002736AA006893
      E8006C95E9006694E8006694E8006694E8006694E800546EBB00D9DBEA008E96
      C200668BD9006694E8006694E8006694E8006694E8006694E8005B7AC800B4B9
      D600B4B9D6005B7BC8006694E8006694E8006694E8006694E8006C95E9006893
      E8002634A90000000000000000000000000000000000000000009C9C9C00CBCB
      CB00CDCDCD00CBCBCB00CBCBCB00CBCBCB00CBCBCB00B6B6B600ECECEC00C9C9
      C900C6C6C600CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00BCBCBC00DBDB
      DB00DBDBDB00BDBDBD00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CDCDCD00CBCB
      CB009B9B9B00000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEAD70055DF
      CD0045A698003637330035312D0035312D0035312D0037443E0048B9AA0058E3
      D2005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600DFDF
      DF00C2C2C2008989890086868600868686008686860090909000CBCBCB00E2E2
      E200E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA0000000000000000000000000000000000000000006C6FBE004F6E
      D0007397EA006894E8006694E8006694E8006694E8006694E8005471C0006288
      D7006694E8006694E8006694E8006694E8006694E8006694E8006694E8005878
      C7005878C7006694E8006694E8006694E8006694E8006894E8007397EA004E6D
      CF006668AD000000000000000000000000000000000000000000B8B8B800B9B9
      B900CECECE00CCCCCC00CBCBCB00CBCBCB00CBCBCB00CBCBCB00B8B8B800C5C5
      C500CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00BBBB
      BB00BBBBBB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CCCCCC00CECECE00B8B8
      B800B2B2B200000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005AEA
      D70053D9C9004496890039353200393431003A3E3A0047AD9F0055E0CE005BEA
      D7005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E5E5
      E500DCDCDC00B9B9B90089898900888888008D8D8D00C6C6C600E0E0E000E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C3000000000000000000000000000000000000000000000000002E34
      A600688AE2007A99EC006794E8006694E8006694E8006694E8006694E8006694
      E8006694E8006694E8006694E8006694E8006694E8006694E8006694E8006694
      E8006694E8006694E8006694E8006694E8006794E8007A99EC00678AE2002D33
      A400000000000000000000000000000000000000000000000000000000009C9C
      9C00C7C7C700D0D0D000CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00D0D0D000C7C7C7009A9A
      9A000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005AEAD70051D5C300438379003D3B370046A0920054DCCA005AEAD7005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B0000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E5E5E500DADADA00B0B0B0008C8C8C00BFBFBF00DEDEDE00E5E5E500E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B700000000000000000000000000000000000000000000000000000000000000
      0000252CA6007593E8007E9AED006894E8006694E8006694E8006694E8006694
      E8006694E8006694E8006694E8006694E8006694E8006694E8006694E8006694
      E8006694E8006694E8006694E8006894E8007E9AED007592E800242BA5000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900CCCCCC00D0D0D000CCCCCC00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CCCCCC00D0D0D000CCCCCC00989898000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005AE9D60050D2C10048AB9D0052D8C6005AEAD7005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E5E5E500D9D9D900C5C5C500DCDCDC00E5E5E500E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000262DA700788FE600879DEF007097EA006694E8006694E8006694
      E8006694E8006694E8006694E8006694E8006694E8006694E8006694E8006694
      E8006694E8006694E8007097EA00879DEF00778EE600262CA600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000099999900CBCBCB00D2D2D200CECECE00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CECECE00D2D2D200CACACA0098989800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005AEAD7005AE8D5005AEAD7005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E5E5E500E4E4E400E5E5E500E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003234AA006575D6008F9FF100869DEF007096EA006694
      E8006694E8006694E8006694E8006694E8006694E8006694E8006694E8006694
      E8007096EA00869DEF008F9FF1006574D6003234AA0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009D9D9D00BEBEBE00D4D4D400D2D2D200CDCDCD00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CDCDCD00D2D2D200D4D4D400BEBEBE009D9D9D0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007071C3003339B2007A86E20098A2F30092A0
      F100859CEE007A99EC007598EB007498EB007598EB007A99EC00859CEE0092A0
      F10098A2F3007A85E1003339B2006F71C3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BABABA009F9F9F00C7C7C700D6D6D600D4D4
      D400D1D1D100D0D0D000CECECE00CECECE00CECECE00D0D0D000D1D1D100D4D4
      D400D6D6D600C7C7C7009F9F9F00BABABA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000696AC2002D30AD00575D
      C9007E84E100989EF1009EA4F4009FA4F4009EA4F400989EF1007E84E100575D
      C9002D30AD006A6BC20000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B7B7B7009C9C9C00B3B3
      B300C7C7C700D4D4D400D7D7D700D7D7D700D7D7D700D4D4D400C7C7C700B3B3
      B3009C9C9C00B8B8B80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080CB005555BA003838AD003435AC003838AD005555BA008081CB000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2C2C200ADADAD009F9F9F009E9E9E009F9F9F00ADADAD00C2C2C2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
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
      000000000000000000000000000000000000000000000000000098635A00A765
      5900937B77000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A6A6A600A8A8
      A800B3B3B3000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000526F5C00326446001F6F48001A7248001E6E4700316346004E6C59007A82
      7A00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B4B4
      B400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5665900D2917900E7AE
      9100C27B6600A273690000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A700BFBFBF00CECE
      CE00B3B3B300AFAFAF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A66A5C00D3917800F2BE9C00EEB4
      9000EEB99600C6806700A98C8500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A9A9A900BFBFBF00D6D6D600D0D0
      D000D3D3D300B5B5B500BDBDBD00000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A76E5D00D4917600F2BE9D00EDAE8B00ECB1
      8900F4C29D00EEBC9C00BB6F5900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000ABABAB00BEBEBE00D6D6D600CDCDCD00CECE
      CE00D8D8D800D5D5D500ACACAC00000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A7725F00D5917300F1BC9C00EDAF8C00ECB18900F5C3
      9F00FFDAC000F9CFB400C2765A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000ACACAC00BEBEBE00D5D5D500CECECE00CECECE00D8D8
      D800E7E7E700E0E0E000AFAFAF00000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AB796500D6917100F1BC9C00EEB08D00ECB18900F5C29F00FFDA
      BF00FCD3B800CC80610000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B0B0B000BEBEBE00D5D5D500CECECE00CECECE00D8D8D800E7E7
      E700E3E3E300B4B4B40000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C8B8A007C7A
      7900827F7D0085818000827F7D007F7D7B008B89890000000000000000000000
      000000000000C47B5900ECB79700EEB39000ECB18900F5C29D00FFD9BE00FDD5
      B900D28663000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BBBBBB00B1B1
      B100B4B4B400B5B5B500B4B4B400B2B2B200BABABA0000000000000000000000
      000000000000B1B1B100D2D2D200D0D0D000CECECE00D8D8D800E6E6E600E4E4
      E400B7B7B7000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0044C5A40045C1A20045C1A20045C1A20045C1A20048C5A60044C5A40039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      500000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200CECECE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CFCFCF00CECECE00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B797700A2A09F00CBC9C900E8E7
      E600F2EFEE00F3F1EF00F2EFEE00E9E7E600CCCAC900A4A2A0007B7776000000
      000079777600A46E5200CD886300EAB59200F4C09C00FFD9BC00FFD7BB00D78D
      6500000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B0B0B000C7C7C700DFDFDF00F0F0
      F000F5F5F500F6F6F600F5F5F500F0F0F000DFDFDF00C8C8C800AFAFAF000000
      0000B0B0B000A9A9A900B7B7B700D1D1D100D7D7D700E6E6E600E5E5E500BABA
      BA0000000000000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0043C4A400F8FDF800FCFDF900FCFDF900FCFDF900ECF8F30045C1A20039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200CECECE00FCFCFC00FCFCFC00FCFCFC00FCFCFC00F9F9F900CDCDCD00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE000000000000000000000000000000000000000000000000000000
      00000000000000000000807E7C00A9A6A400E7E4E300F3F1F100D5D3D000B5B3
      B100A09C9B009A979600A09C9B00B6B3B100D6D3D200F4F2F100E8E6E400ABA9
      A7006D6967006D6A6900AB745900CF8D6700EFC0A100FFD8BC00DC916800EAC3
      AE00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B3B3B300CACACA00EFEFEF00F6F6F600E4E4E400D2D2
      D200C5C5C500C2C2C200C5C5C500D2D2D200E5E5E500F6F6F600EFEFEF00CCCC
      CC00A7A7A700A8A8A800ADADAD00BABABA00D8D8D800E6E6E600BCBCBC00DBDB
      DB00000000000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0045C1A200FFFDFB00FFFFFD00FFFFFD00FFFFFD00F5FBF60045C1A20039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C600179266000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200CDCDCD00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FAFAFA00CDCDCD00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF000000000000000000000000000000000000000000000000000000
      000000000000817D7C00CFCECD00F1EDEC00C1BEBB00918C8900BDB6AE00E2DC
      D100F6EFE400FBF5EB00F6F0E700E3DDD500BEB7B200928E8A00C2BFBE00F1ED
      EC00D2D0CF007D7A7900A4A1A000B17B5C00D5926A00DD936900E4BDA7000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B3B3B300E2E2E200F4F4F400D8D8D800BBBBBB00D3D3D300E8E8
      E800F4F4F400F7F7F700F4F4F400EAEAEA00D4D4D400BCBCBC00D9D9D900F4F4
      F400E3E3E300B1B1B100C8C8C800B0B0B000BCBCBC00BDBDBD00D7D7D7000000
      0000000000000000000000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0045C1A200FFFEFC00FFFFFE00FFFFFE00FFFFFE00F5FBF70045C1A20039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200CDCDCD00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FBFBFB00CDCDCD00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD0000000000000000000000000000000000000000000000
      0000827D7D00DAD8D700E6E1E00094908E00C3BDB100FBF0DE00FFF5DE00FFF4
      DF00FFF7E500FFF9EB00FFFBEE00FFFBEF00FFF9EE00FFF6EA00C7C0B8009591
      8E00E7E2E000DDDAD900807C7B00726E6B00A2806C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B3B3B300E7E7E700EDEDED00BDBDBD00D6D6D600F3F3F300F5F5F500F5F5
      F500F7F7F700F9F9F900F9F9F900FAFAFA00F9F9F900F7F7F700D9D9D900BEBE
      BE00EDEDED00E9E9E900B3B3B300A9A9A900B3B3B30000000000000000000000
      00000000000000000000000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2
      AC0045C1A200FFFEFC00FFFFFE00FFFFFE00FFFFFE00F5FCF70045C1A2003AD2
      AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400CDCDCD00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FBFBFB00CDCDCD00D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB0000000000000000000000000000000000000000008784
      8300D0CFCE00E1DEDA00908A8700DDD4C400FFF0D400FFF0D000FFF1D300FFF4
      D900FFF6E200FFF9EB00FFFCF200FFFDF400FFFCF000FFF9EC00FFF8E900E5DE
      D300908B8800E2DFDC00D4D2D0007A7775000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B7B7
      B700E2E2E200EBEBEB00BABABA00E3E3E300F2F2F200F1F1F100F2F2F200F4F4
      F400F6F6F600F9F9F900FBFBFB00FCFCFC00FAFAFA00F9F9F900F8F8F800EAEA
      EA00BABABA00EBEBEB00E4E4E400AFAFAF000000000000000000000000000000
      0000000000000000000000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0045C1A200FFFEFD00FFFFFE00FFFFFE00FFFFFE00F5FCF70043C4A4003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500CDCDCD00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FBFBFB00CECECE00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C3000000000000000000000000000000000000000000ABA7
      A600E9E3E100928E8A00DBD0BD00FEEDCB00FFEEC600FFEFC900FFF0CF00FFF1
      D500FFF6DF00FFF9EA00FFFEF400FFFFF800FFFFF800FFFDF400FFF9EC00FFF7
      E800E5DED30096908D00E9E3E100AEABAA000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CBCB
      CB00EEEEEE00BCBCBC00E0E0E000EFEFEF00EFEFEF00F0F0F000F1F1F100F2F2
      F200F5F5F500F9F9F900FCFCFC00FDFDFD00FDFDFD00FCFCFC00F9F9F900F7F7
      F700EAEAEA00BDBDBD00EEEEEE00CECECE000000000000000000000000000000
      0000000000000000000000000000000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B3003ED5B30045C5A60045C1A20045C1A20045C1A20045C1A2004AC8
      A90047C6A700FFFFFE00FFFFFF00FFFFFF00FFFFFF00F5FCF80047C6A70045C1
      A20045C1A20045C1A20045C1A20049C6A70045C5A6003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D6D6D600CFCFCF00CDCDCD00CDCDCD00CDCDCD00CDCDCD00D1D1
      D100D0D0D000FEFEFE00FFFFFF00FFFFFF00FFFFFF00FCFCFC00D0D0D000CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00D0D0D000CFCFCF00D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B40000000000000000000000000086838100E4E1
      DF00B4AEAB00BDB5A500F8E8C300FFEABC00FFEDC000FFEDC600FFEFCA00FFF1
      D000FFF4DB00FFF8E600FFFDF200FFFFFB00FFFFFD00FFFFF900FFFDF400FFF9
      EC00FFF8E800C8C1B900B7B0AD00E6E2E000827D7C0000000000000000000000
      0000000000000000000000000000000000000000000000000000B6B6B600EDED
      ED00CFCFCF00D1D1D100ECECEC00ECECEC00EDEDED00EEEEEE00F0F0F000F2F2
      F200F4F4F400F7F7F700FBFBFB00FDFDFD00FEFEFE00FDFDFD00FCFCFC00F9F9
      F900F7F7F700DADADA00D0D0D000EDEDED00B2B2B20000000000000000000000
      000000000000000000000000000000000000429A780058E8C20047DCB90041D7
      B70041D7B70041D7B70045C5A600F9FFFB00FCFFFC00FCFFFC00FCFFFC00FCFF
      FC00FCFFFC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FCFFFC00FCFF
      FC00FCFFFC00FCFFFC00FCFFFC00E8FBF40045C1A20041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D8D8D800CFCFCF00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00F9F9F900CDCDCD00D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA00000000000000000000000000A7A4A200E3DD
      D9008F898400EADDBE00FEE7B400FFEAB700FFECBB00FFEDC000FFEDC600FFEF
      CB00FFF1D400FFF6DF00FFFBED00FFFFF700FFFFFE00FFFFFD00FFFFF800FFFD
      F200FFF9EB00FFF6EA0094908C00E4DEDA00ABA7A60000000000000000000000
      0000000000000000000000000000000000000000000000000000C9C9C900EAEA
      EA00BABABA00E6E6E600EAEAEA00EAEAEA00ECECEC00EDEDED00EEEEEE00EFEF
      EF00F2F2F200F5F5F500F9F9F900FDFDFD00FEFEFE00FEFEFE00FDFDFD00FBFB
      FB00F9F9F900F7F7F700BDBDBD00EBEBEB00CBCBCB0000000000000000000000
      00000000000000000000000000000000000028916A0058E9C30047DCBB0045D9
      BA0045D9BA0045D9BA0045C1A200FFFFFE00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00EFFCF70045C1A20045D9BA0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900D9D9D900CDCDCD00FEFEFE00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FBFBFB00CDCDCD00D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC00000000000000000000000000CCC8C500C4BE
      BA00B1A89A00F2DFB200FFE7AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEE
      C600FFF0CE00FFF3D700FFF7E400FFFDEF00FFFFF800FFFFFB00FFFFF800FFFE
      F500FFFBEE00FFFAEC00C2BDB500C6BFBC00CECAC90095939200000000000000
      0000000000000000000000000000000000000000000000000000DEDEDE00D8D8
      D800CACACA00E5E5E500E7E7E700E9E9E900EAEAEA00ECECEC00EDEDED00EFEF
      EF00F1F1F100F3F3F300F7F7F700FAFAFA00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00F9F9F900F9F9F900D7D7D700D9D9D900E0E0E000C0C0C000000000000000
      00000000000000000000000000000000000020936A0058E8C40049DDBF0048DD
      BF0048DDBF0048DDBF0045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00F1FCF70045C1A20048DDBF0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DCDCDC00DCDCDC00CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FBFBFB00CDCDCD00DCDCDC00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD0000000000000000008B878700E3DEDC00AAA4
      A200C5BAA200F6DDA700FFE6A800FFE7AC00FFE8B200FFEAB700FFECBB00FFED
      C000FFEEC700FFF0CF00FFF4D800FFF7E400FFFBED00FFFDF300FFFEF500FFFD
      F300FFFBEF00FFF9EC00E6E0D600ACA6A400E6E0DE0089878500000000000000
      00000000000000000000000000000000000000000000B9B9B900EBEBEB00CACA
      CA00D3D3D300E3E3E300E7E7E700E7E7E700E9E9E900EAEAEA00ECECEC00EDED
      ED00EFEFEF00F1F1F100F4F4F400F7F7F700F9F9F900FBFBFB00FCFCFC00FBFB
      FB00FAFAFA00F9F9F900EBEBEB00CBCBCB00EDEDED00B8B8B800000000000000
      00000000000000000000000000000000000028956D0056E8C6004DE0C4004BDF
      C4004BDFC4004BDFC40045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00F1FDF90045C1A2004BDFC4004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FCFCFC00CDCDCD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF00000000000000000097929100E9E2E0009B97
      9500CABDA000F7DB9F00FFE4A200FFE6A800FFE7AC00FFE8B200FFEAB700FFEC
      BB00FFEDC100FFEEC800FFF0CF00FFF3D800FFF6DF00FFF8E700FFF9EB00FFF9
      ED00FFF9EC00FFF9EA00F7F0E6009F999700E9E2E000918C8A00000000000000
      00000000000000000000000000000000000000000000BFBFBF00EEEEEE00C2C2
      C200D4D4D400E1E1E100E5E5E500E7E7E700E7E7E700E9E9E900EAEAEA00ECEC
      EC00EDEDED00EFEFEF00F1F1F100F3F3F300F5F5F500F7F7F700F9F9F900F9F9
      F900F9F9F900F9F9F900F4F4F400C3C3C300EEEEEE00BBBBBB00000000000000
      00000000000000000000000000000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C9004FE2C90045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00EFFCF80047C6AA004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00DFDFDF00CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FBFBFB00D0D0D000DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE00000000000000000098949300EAE3E0009993
      9100C7BA9B00F2D69600FFE29D00FFE5A200FFE6A800FFE7AC00FFE8B200FFEA
      B700FFECBB00FFEDC000FFEEC700FFF0CE00FFF2D500FFF4DB00FFF6E000FFF6
      E300FFF7E500FFF7E600FCF5E8009A969300EAE3E00095908E00000000000000
      00000000000000000000000000000000000000000000C0C0C000EEEEEE00C0C0
      C000D2D2D200DDDDDD00E4E4E400E5E5E500E7E7E700E7E7E700E9E9E900EAEA
      EA00ECECEC00EDEDED00EFEFEF00F1F1F100F2F2F200F4F4F400F6F6F600F6F6
      F600F7F7F700F7F7F700F6F6F600C1C1C100EEEEEE00BEBEBE00000000000000
      0000000000000000000000000000000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0052E4CC0048C8AB0047C6A90045C1A20045C1A20045C1A20045C1
      A20046C5A700FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6FEFD0045C2A30045C1
      A20045C1A20045C1A20045C1A20045C1A20048C9AB0052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B3006893800000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100E1E1E100D1D1D100D0D0D000CDCDCD00CDCDCD00CDCDCD00CDCD
      CD00CFCFCF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00CDCDCD00CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00D1D1D100E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA0000000000000000009A979300EBE4E1009F9A
      9800C2B49600E6CA8B00FFE19800FFE39D00FFE5A200FFE6A800FFE7AC00FFE8
      B200FFEAB700FFECBB00FFEDC000FFEEC600FFEFCB00FFF1D100FFF2D700FFF4
      DA00FFF4DD00FFF5DF00F4ECDF00A19C9A00EBE4E100938F8D00000000000000
      00000000000000000000000000000000000000000000C1C1C100EEEEEE00C3C3
      C300CECECE00D6D6D600E2E2E200E4E4E400E5E5E500E7E7E700E7E7E700E9E9
      E900EAEAEA00ECECEC00EDEDED00EFEFEF00EFEFEF00F2F2F200F3F3F300F4F4
      F400F4F4F400F5F5F500F2F2F200C5C5C500EEEEEE00BDBDBD00000000000000
      000000000000000000000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7
      D00048C8AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6FFFE0045C1A20055E7
      D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300D1D1D100FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00CDCDCD00E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C700000000000000000000000000B0ADAB00E6DFDC00B0AC
      AA00B5AA9100D6BD8400FADA9100FFE29900FFE39D00FFE5A200FFE6A800FFE7
      AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEDC600FFEFCA00FFF0CF00FFF1
      D300FFF2D700FFF2D900E3DBCD00B3AEAC00E8E1DE0089858400000000000000
      00000000000000000000000000000000000000000000CECECE00EBEBEB00CECE
      CE00C9C9C900CFCFCF00DEDEDE00E3E3E300E4E4E400E5E5E500E7E7E700E7E7
      E700E9E9E900EAEAEA00ECECEC00EDEDED00EEEEEE00F0F0F000F1F1F100F2F2
      F200F2F2F200F3F3F300E8E8E800CFCFCF00EDEDED00B8B8B800000000000000
      00000000000000000000000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8
      D40045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF0045C1A20058E8
      D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00CDCDCD00E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B50000000000000000000000000000000000D0CAC600CCC9
      C600A29A8900CBB68500E7C88100FFDF9300FFE29900FFE39D00FFE5A200FFE6
      A800FFE7AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEDC600FFEFC900FFF0
      CF00FFF0D200FCEED600C2BCB200CECAC800D3CCC90096939200000000000000
      0000000000000000000000000000000000000000000000000000DFDFDF00DFDF
      DF00C2C2C200CCCCCC00D4D4D400E1E1E100E3E3E300E4E4E400E5E5E500E7E7
      E700E7E7E700E9E9E900EAEAEA00ECECEC00EDEDED00EEEEEE00F0F0F000F1F1
      F100F1F1F100F1F1F100D6D6D600DFDFDF00E0E0E000C0C0C000000000000000
      000000000000000000000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEA
      D70045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF0045C1A2005AEA
      D7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00CDCDCD00E5E5
      E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B70000000000000000000000000000000000B0AAA600ECE8
      E600948F8900BEAF8B00D5B97900F1D18500FFE19400FFE29900FFE39D00FFE5
      A200FFE6A800FFE7AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEDC600FFEF
      C900FFEDCC00F2E6D1009D989300EDE9E700B3ACA90000000000000000000000
      0000000000000000000000000000000000000000000000000000CDCDCD00F1F1
      F100BDBDBD00CBCBCB00CCCCCC00D8D8D800E1E1E100E3E3E300E4E4E400E5E5
      E500E7E7E700E7E7E700E9E9E900EAEAEA00ECECEC00EDEDED00EEEEEE00F0F0
      F000F0F0F000EDEDED00C1C1C100F1F1F100CECECE0000000000000000000000
      000000000000000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6FFFF0045C1A2005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00CDCDCD00E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA000000000000000000000000000000000000000000A39F9D00EBE4
      E000BFBBBA00A59D8A00C4B08100D9BC7700F2D18700FFE09400FFE29900FFE3
      9D00FFE5A200FFE6A800FFE7AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEA
      C300F5E6C800C4BDB000C1BEBC00ECE6E200928F8C0000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600EEEE
      EE00D7D7D700C3C3C300C9C9C900CDCDCD00D8D8D800E1E1E100E3E3E300E4E4
      E400E5E5E500E7E7E700E7E7E700E9E9E900EAEAEA00ECECEC00EDEDED00EDED
      ED00EBEBEB00D6D6D600D8D8D800EFEFEF00BDBDBD0000000000000000000000
      000000000000000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6FFFF0045C1A2005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00CDCDCD00E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C300000000000000000000000000000000000000000000000000B9B3
      AF00F1ECEB009E999700B1A68C00C6AF7C00D8BB7600EACB8200FDDB9100FFE1
      9900FFE39D00FFE5A200FFE6A800FFE7AC00FFE8B200FFE9B500FCE6B800F1DF
      BC00D2C7B400A29D9A00F2EDEB00BBB6B3000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D2D2
      D200F3F3F300C2C2C200C6C6C600C9C9C900CCCCCC00D6D6D600DFDFDF00E2E2
      E200E4E4E400E5E5E500E7E7E700E7E7E700E9E9E900EAEAEA00E9E9E900E7E7
      E700DBDBDB00C5C5C500F3F3F300D4D4D4000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80046C3A40045C1A20045C1A20045C1A20045C1A20045C1A20046C3A4005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B0000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600CECECE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CECECE00E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B70000000000000000000000000000000000000000000000000000000000B4B1
      AE00DED8D500E7E3E2009A948F00B1A68A00C2AD7E00D1B67600DDBF7A00E9CB
      8600F2D58F00F7D99700F8DC9C00F6DBA000F1D8A200E7D1A100D9C8A400C1B7
      A3009C979300E8E4E300E0DAD80092908E000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D0D0
      D000E7E7E700EEEEEE00C0C0C000C6C6C600C8C8C800CACACA00CFCFCF00D6D6
      D600DCDCDC00DFDFDF00E1E1E100E1E1E100E0E0E000DDDDDD00DADADA00D2D2
      D200C1C1C100EFEFEF00E9E9E900BEBEBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A09E00EAE6E300E7E4E300A09B9800A69D8A00BAAB8600C4AE7D00CDB4
      7A00D4B97D00D6BC8000D7BE8600D5BE8B00D0BD9200C7BA9B00AFA69600A19D
      9A00E8E6E400ECE7E40098949200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C8C8C800EFEFEF00EFEFEF00C3C3C300C3C3C300C8C8C800C8C8C800CBCB
      CB00CDCDCD00CFCFCF00D1D1D100D2D2D200D1D1D100D2D2D200C8C8C800C5C5
      C500EFEFEF00F0F0F000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A7A1A000E3E0DE00F4F2EF00C1BEBC009A948F00A49B8900B2A6
      8B00BAAD8C00BEAF8D00BDAF9100B6AB9100A9A090009B959000C2BFBE00F5F2
      F100E6E1DF009B96940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C8C8C800ECECEC00F6F6F600D8D8D800C0C0C000C2C2C200C7C7
      C700CACACA00CBCBCB00CBCBCB00CACACA00C5C5C500C1C1C100D9D9D900F7F7
      F700EDEDED00C1C1C10000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B7B3B100C2BFBC00F7F4F300F3F1EF00D0CECC00B7B4
      B100A9A4A200A39F9C00A9A4A200B7B4B300D2CECD00F4F2F100F8F5F400C4C1
      BF00A9A6A5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D2D2D200D9D9D900F8F8F800F6F6F600E2E2E200D2D2
      D200C9C9C900C6C6C600C9C9C900D3D3D300E2E2E200F6F6F600F9F9F900DADA
      DA00CBCBCB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABA6A500C0BBBA00E3E1DF00F9F7
      F600FDFBF900FCFBF900FDFBF900FBF8F700E4E1E000C1BEBB00A9A4A3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CBCBCB00D7D7D700EDEDED00F9F9
      F900FCFCFC00FCFCFC00FCFCFC00FAFAFA00EDEDED00D8D8D800CACACA000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B8B4
      B200A9A6A300AAA5A200A9A6A200B7B3B100D1CECD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D2D2
      D200CACACA00CACACA00CACACA00D2D2D200E2E2E20000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
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
      0000000000000000000000000000000000000000000000000000B4B4B4009191
      9100888888008282820082828200828282008282820082828200828282008282
      8200828282008282820082828200828282008282820082828200828282008282
      8200828282009393930000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D3D3D300BEBE
      BE00BABABA00B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000929292006F6F
      6F00707070007171710072727200727272007272720072727200727272007272
      7200727272007272720072727200727272007272720072727200727272007272
      7200727272008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF00ABAB
      AB00ACACAC00ACACAC00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00ADADAD00B7B7B70000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9A9A00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB008686860000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00B8B8B80000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000070707000FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB007676760000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ACACAC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9A9A00FBFB
      FB00FDFDFD00FCFCFC00FCFCFC00FCFBFB00FBFBFB00FBFBFA00FAFAFA00FAFA
      F900F9F9F900F9F9F800F8F8F800F8F7F700F7F7F700F7F6F600F6F6F500F6F5
      F500FBFBFB008686860000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFB
      FB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9
      F900FCFCFC00B8B8B80000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000070707000FBFB
      FB00FDFDFD00FCFCFC00FCFCFC00FCFBFB00FBFBFB00FBFBFA00FAFAFA00FAFA
      F900F9F9F900F9F9F800F8F8F800F8F7F700F7F7F700F7F6F600F6F6F500F6F5
      F500FBFBFB007676760000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ACACAC00FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFB
      FB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9
      F900FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9A9A00FBFB
      FB00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FBFAFA00FAFAFA00FAF9F900F9F9
      F900F9F8F800F8F8F700F7F7F700F7F7F600F6F6F600F6F5F500F5F5F500F5F4
      F400FBFBFB008686860000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFB
      FB00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8
      F800FCFCFC00B8B8B8000000000000000000000000000000000038587A002A4D
      71002A4D71002A4D71002A4D71002A4D71002A4D71002B4E710070707000FBFB
      FB00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FBFAFA00FAFAFA00FAF9F900F9F9
      F900F9F8F800F8F8F700F7F7F700F7F7F600F6F6F600F6F5F500F5F5F500F5F4
      F400FBFBFB007676760000000000000000000000000000000000A1A1A1009B9B
      9B009B9B9B009B9B9B009B9B9B009B9B9B009B9B9B009B9B9B00ACACAC00FCFC
      FC00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFB
      FB00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8
      F800FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9A9A00FBFB
      FB00FCFBFB00FBFBFB00FBFBFA00FAFAFA00FAFAF900F9F9F900F9F9F800F8F8
      F800F8F7F700F7F7F700F7F6F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3
      F300FBFBFB008686860000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FAFAFA00FAFA
      FA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8
      F800FCFCFC00B8B8B8000000000000000000000000003C5D7E005084B1005286
      B3005286B3005185B3005185B1005083B0004F82AE004E80AA0070707000FBFB
      FB00FCFBFB00FBFBFB00FBFBFA00FAFAFA00FAFAF900F9F9F900F9F9F800F8F8
      F800F8F7F700F7F7F700F7F6F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3
      F300FBFBFB0076767600000000000000000000000000A3A3A300BBBBBB00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BBBBBB00BABABA00B8B8B800ACACAC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FAFAFA00FAFA
      FA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8
      F800FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000099999900FBFB
      FB00FBFBFB00FBF9F800F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B47100F4F3F300F3F3
      F200FBFBFB008686860000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FCFCFC00FAFAFA00CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00CACACA00CACACA00CACACA00CACACA00CACACA00CBCBCB00F8F8F800F7F7
      F700FCFCFC00B8B8B8000000000000000000000000002D51740077A8D0005390
      C300528EC200518EC100518DBF00508BBD004E88B9004D85B40070707000FBFB
      FB00FBFBFB00FBF9F800F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B47100F4F3F300F3F3
      F200FBFBFB00767676000000000000000000000000009D9D9D00D0D0D000C3C3
      C300C2C2C200C2C2C200C1C1C100C0C0C000BEBEBE00BCBCBC00ACACAC00FCFC
      FC00FCFCFC00FAFAFA00CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00CACACA00CACACA00CACACA00CACACA00CACACA00CBCBCB00F8F8F800F7F7
      F700FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000094949400FBFB
      FB00FAFAFA00FAFAF900F9F9F900F9F9F800F8F8F800F8F7F700F7F7F700F7F6
      F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3F300F3F3F200F3F2F200F2F2
      F100FBFBFB008686860000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C1C1C100FCFC
      FC00FCFCFC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9
      F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6
      F600FCFCFC00B8B8B8000000000000000000000000002C5074007BABD3005490
      C3005490C300538FC200528EC000518CBD004F89BA004E86B40070707000FBFB
      FB00FAFAFA00FAFAF900F9F9F900F9F9F800F8F8F800F8F7F700F7F7F700F7F6
      F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3F300F3F3F200F3F2F200F2F2
      F100FBFBFB00767676000000000000000000000000009C9C9C00D1D1D100C3C3
      C300C3C3C300C2C2C200C2C2C200C0C0C000BFBFBF00BCBCBC00ACACAC00FCFC
      FC00FCFCFC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9
      F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6
      F600FCFCFC00AFAFAF000000000000000000000000000000000000000000FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FAFAFA009A9A9A00FBFB
      FB00FAF9F900F9F9F900F9F8F800F8F8F700F7F7F700F7F7F600F6F6F600F6F5
      F500F5F5F500F5F4F400F4F4F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1
      F000FBFBFB00868686000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FCFCFC00C4C4C400FCFC
      FC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9
      F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6
      F600FCFCFC00B8B8B8000000000000000000000000002D5176007CACD3005692
      C5005591C4005590C300548FC100538DBF00518ABB004F86B50070707000FBFB
      FB00FAF9F900F9F9F900F9F8F800F8F8F700F7F7F700F7F7F600F6F6F600F6F5
      F500F5F5F500F5F4F400F4F4F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1
      F000FBFBFB00767676000000000000000000000000009D9D9D00D2D2D200C4C4
      C400C3C3C300C3C3C300C2C2C200C1C1C100BFBFBF00BCBCBC00ACACAC00FCFC
      FC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9
      F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6
      F600FCFCFC00AFAFAF000000000000000000000000000000000000000000FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FDFCFC00FCFCFC00FAFAFA009A9A9A00FBFB
      FB00F9F9F800F8F7F600F7B57400F7B57400F7B57300F7B57300F7B57300F7B5
      7300F6B57300F6B57300F6B57300F6B57300F6B57300F6B77700F1F0F000F0F0
      EF00FBFBFB00868686000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FBFBFB00C4C4C400FCFC
      FC00FAFAFA00F9F9F900CDCDCD00CDCDCD00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CECECE00F6F6F600F5F5
      F500FCFCFC00B8B8B8000000000000000000000000002E5277007DADD4005894
      C6005793C5005793C4005691C200558FC000538CBC005188B60070707000FBFB
      FB00F9F9F800F8F7F600F7B57400F7B57400F7B57300F7B57300F7B57300F7B5
      7300F6B57300F6B57300F6B57300F6B57300F6B57300F6B77700F1F0F000F0F0
      EF00FBFBFB00767676000000000000000000000000009E9E9E00D2D2D200C5C5
      C500C5C5C500C4C4C400C3C3C300C2C2C200C0C0C000BEBEBE00ACACAC00FCFC
      FC00FAFAFA00F9F9F900CDCDCD00CDCDCD00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CECECE00F6F6F600F5F5
      F500FCFCFC00AFAFAF000000000000000000000000000000000000000000FCFC
      FC00FDFDFD00FCFCFC00FBFBFB00F8F8F800F3F3F300EBEBEB0099999900FBFB
      FB00F8F8F700F7F7F700F7F7F600F6F6F600F6F5F500F5F5F500F5F4F400F4F4
      F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1F000F1F0EF00F0EFEF00F0EF
      EE00FBFBFB00868686000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FDFDFD00FCFCFC00FBFBFB00F7F7F700F3F3F300C3C3C300FCFC
      FC00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8F800F8F8
      F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5
      F500FCFCFC00B8B8B8000000000000000000000000002F5479007EAED5005A95
      C7005995C6005994C5005892C3005791C100558EBD005389B70070707000FBFB
      FB00F8F8F700F7F7F700F7F7F600F6F6F600F6F5F500F5F5F500F5F4F400F4F4
      F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1F000F1F0EF00F0EFEF00F0EF
      EE00FBFBFB00767676000000000000000000000000009F9F9F00D3D3D300C6C6
      C600C5C5C500C5C5C500C4C4C400C3C3C300C1C1C100BEBEBE00ACACAC00FCFC
      FC00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8F800F8F8
      F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5
      F500FCFCFC00AFAFAF000000000000000000000000000000000000000000FCFC
      FC00FDFCFC00FCFBFB00FAFAF900F5F5F500EEEEEE00E3E3E20097979700FBFB
      FB00F7F7F700F7F6F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3F300F3F3
      F200F3F2F200F2F2F100F2F1F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEE
      ED00FBFBFB00858585000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FCFCFC00FBFBFB00F9F9F900F5F5F500EEEEEE00C2C2C200FCFC
      FC00FAFAFA00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7
      F700F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4
      F400FCFCFC00B7B7B70000000000000000000000000030557A0080AFD6005C97
      C9005C97C8005B96C7005A94C5005992C200578FBE00558BB90070707000FBFB
      FB00F7F7F700F7F6F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3F300F3F3
      F200F3F2F200F2F2F100F2F1F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEE
      ED00FBFBFB00757575000000000000000000000000009F9F9F00D4D4D400C7C7
      C700C6C6C600C6C6C600C5C5C500C3C3C300C2C2C200BFBFBF00ACACAC00FCFC
      FC00FAFAFA00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7
      F700F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4
      F400FCFCFC00AEAEAE000000000000000000000000000000000000000000FCFC
      FC00FCFCFC00FBFBFB00F9F9F900F5F5F500EDEDED00E1E1E00097979700FBFB
      FB00F6F6F600F6F5F300F7B57300F7B57300F6B57300F6B57300F6B57300F6B5
      7300F6B57300F6B57300F6B57300F6B57300F6B57300F6B67700EEEEED00EEED
      EC00FBFBFB00848484000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FCFCFC00FBFBFB00F8F8F800F4F4F400EDEDED00C2C2C200FCFC
      FC00F9F9F900F8F8F800CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CDCDCD00F4F4F400F3F3
      F300FCFCFC00B7B7B70000000000000000000000000031567C0081B0D7005E99
      CA005E99C9005D98C8005C96C6005B94C3005991C000578DBA0070707000FBFB
      FB00F6F6F600F6F5F300F7B57300F7B57300F6B57300F6B57300F6B57300F6B5
      7300F6B57300F6B57300F6B57300F6B57300F6B57300F6B67700EEEEED00EEED
      EC00FBFBFB0074747400000000000000000000000000A1A1A100D4D4D400C7C7
      C700C7C7C700C7C7C700C6C6C600C5C5C500C3C3C300C0C0C000ACACAC00FCFC
      FC00F9F9F900F8F8F800CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CDCDCD00F4F4F400F3F3
      F300FCFCFC00AEAEAE000000000000000000000000000000000000000000FCFC
      FC00FCFCFC00FBFAFA00F7D2AD00F3CEAA00EBC8A500DFBF9D0097949000FBFB
      FB00F6F5F500F5F5F400F4F4F400F4F3F300F3F3F200F3F2F200F2F2F100F2F1
      F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEEED00EEEDED00EEEDEC00EDEC
      EC00FBFBFB00848484000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FBFBFB00E1E1E100DEDEDE00DBDBDB00D5D5D500C0C0C000FCFC
      FC00F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6
      F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3
      F300FCFCFC00B7B7B70000000000000000000000000032587E0083B1D700619B
      CC00619BCB00609ACA005F98C8005E96C5005C93C100598FBB0070707000FBFB
      FB00F6F5F500F5F5F400F4F4F400F4F3F300F3F3F200F3F2F200F2F2F100F2F1
      F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEEED00EEEDED00EEEDEC00EDEC
      EC00FBFBFB0074747400000000000000000000000000A2A2A200D5D5D500C9C9
      C900C9C9C900C8C8C800C7C7C700C6C6C600C4C4C400C2C2C200ACACAC00FCFC
      FC00F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6
      F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3
      F300FCFCFC00AEAEAE000000000000000000000000000000000000000000FCFC
      FC00FCFBFB00FBFAFA00F8F8F800F3F3F300ECECEB00DFDFDF0097979700FBFB
      FB00F5F4F400F4F4F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1F000F1F0
      EF00F0EFEF00F0EFEE00EFEEEE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECEC
      EB00FBFBFB00848484000000000000000000000000000000000000000000FDFD
      FD00FCFCFC00FCFCFC00FBFBFB00F8F8F800F3F3F300ECECEC00C2C2C200FCFC
      FC00F8F8F800F8F8F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6F600F5F5
      F500F5F5F500F5F5F500F5F5F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3
      F300FCFCFC00B7B7B700000000000000000000000000345A800083B2D800639D
      CE00639DCD00629CCC00619ACA006098C7005E95C3005C91BC0070707000FBFB
      FB00F5F4F400F4F4F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1F000F1F0
      EF00F0EFEF00F0EFEE00EFEEEE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECEC
      EB00FBFBFB0074747400000000000000000000000000A3A3A300D5D5D500CACA
      CA00CACACA00C9C9C900C8C8C800C7C7C700C5C5C500C2C2C200ACACAC00FCFC
      FC00F8F8F800F8F8F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6F600F5F5
      F500F5F5F500F5F5F500F5F5F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3
      F300FCFCFC00AEAEAE000000000000000000000000000000000000000000FCFC
      FC00FBFBFB00FAFAFA00F8F7F700F3F3F300EBEBEB00DFDFDF0097979700FBFB
      FB00F4F3F300F3F2F000F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00F0E7DF00EFEEED00EEEDED00EEEDEC00EDECEC00EDECEB00ECEBEB00ECEB
      EA00FBFBFB00848484000000000000000000000000000000000000000000FDFD
      FD00FCFCFC00FBFBFB00FAFAFA00F7F7F700F3F3F300ECECEC00C2C2C200FCFC
      FC00F8F8F800F6F6F600CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00EFEFEF00F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2
      F200FCFCFC00B7B7B700000000000000000000000000365C830086B4DA0066A0
      CF00669FCE00659ECD00649CCB00629AC8006097C4005E93BE0070707000FBFB
      FB00F4F3F300F3F2F000F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00F0E7DF00EFEEED00EEEDED00EEEDEC00EDECEC00EDECEB00ECEBEB00ECEB
      EA00FBFBFB0074747400000000000000000000000000A4A4A400D6D6D600CBCB
      CB00CBCBCB00CACACA00C9C9C900C8C8C800C6C6C600C3C3C300ACACAC00FCFC
      FC00F8F8F800F6F6F600CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00EFEFEF00F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2
      F200FCFCFC00AEAEAE000000000000000000000000000000000000000000FCFC
      FC00FBFBFB00FAF9F900F7D4B100F3D1AE00EBCAA800DFC0A00097949100FBFB
      FB00F3F3F200F2F2F100F2F1F100F1F1F000F1F0EF00F0EFEF00F0EFEE00EFEE
      EE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECECEB00ECEBEA00ECEBEA00EBEA
      E900FBFBFB00848484000000000000000000000000000000000000000000FDFD
      FD00FCFCFC00FBFBFB00E2E2E200E0E0E000DCDCDC00D6D6D600C0C0C000FCFC
      FC00F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2
      F200FCFCFC00B7B7B700000000000000000000000000375E840087B6DB0069A2
      D10068A1D00067A0CF00679FCD00659DCA006399C6006195BF0070707000FBFB
      FB00F3F3F200F2F2F100F2F1F100F1F1F000F1F0EF00F0EFEF00F0EFEE00EFEE
      EE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECECEB00ECEBEA00ECEBEA00EBEA
      E900FBFBFB0074747400000000000000000000000000A5A5A500D7D7D700CDCD
      CD00CCCCCC00CBCBCB00CBCBCB00CACACA00C7C7C700C5C5C500ACACAC00FCFC
      FC00F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2
      F200FCFCFC00AEAEAE000000000000000000000000000000000000000000FCFC
      FC00FAFAFA00F9F9F900F7F7F600F2F2F200EBEAEA00DFDEDE0097979700FBFB
      FB00F2F2F100F2F1F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEEED00EEED
      ED00EEEDEC00EDECEC00EDECEB00F7F7F600FAF9F900F9F9F900F9F9F900F9F9
      F900FCFCFC00848484000000000000000000000000000000000000000000FDFD
      FD00FCFCFC00FBFBFB00FAFAFA00F7F7F700F3F3F300ECECEC00C2C2C200FCFC
      FC00F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4F400F4F4
      F400F3F3F300F3F3F300F3F3F300F9F9F900FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FDFDFD00B7B7B700000000000000000000000000385F860089B7DC006BA4
      D3006BA3D2006AA3D10069A1CF00689FCC00659CC7006397C10070707000FBFB
      FB00F2F2F100F2F1F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEEED00EEED
      ED00EEEDEC00EDECEC00EDECEB00F7F7F600FAF9F900F9F9F900F9F9F900F9F9
      F900FCFCFC0074747400000000000000000000000000A6A6A600D8D8D800CECE
      CE00CDCDCD00CDCDCD00CCCCCC00CBCBCB00C9C9C900C6C6C600ACACAC00FCFC
      FC00F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4F400F4F4
      F400F3F3F300F3F3F300F3F3F300F9F9F900FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FDFDFD00AEAEAE000000000000000000000000000000000000000000FCFC
      FC00FAFAFA00F9F9F900F6F6F600F2F2F200EAE9E900DEDEDD0097979700FBFB
      FB00F1F1F000F1EFED00F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00EEE5DC00ECECEB00ECEBEA00F9F8F800A7A7A5009595930095959300BBBB
      BA00CFCFCE008E8E8D000000000000000000000000000000000000000000FDFD
      FD00FBFBFB00FBFBFB00F9F9F900F7F7F700F2F2F200EBEBEB00C2C2C200FCFC
      FC00F6F6F600F5F5F500CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00EEEEEE00F3F3F300F2F2F200FAFAFA00CBCBCB00C0C0C000C0C0C000D6D6
      D600E2E2E200BCBCBC000000000000000000000000003A6188008BB8DD006EA6
      D4006EA6D3006DA5D2006BA3D0006AA1CD00689EC9006699C30070707000FBFB
      FB00F1F1F000F1EFED00F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00EEE5DC00ECECEB00ECEBEA00F9F8F800A7A7A5009595930095959300BBBB
      BA00F3F3F30086868500000000000000000000000000A7A7A700D8D8D800CFCF
      CF00CFCFCF00CECECE00CDCDCD00CBCBCB00CACACA00C7C7C700ACACAC00FCFC
      FC00F6F6F600F5F5F500CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00EEEEEE00F3F3F300F2F2F200FAFAFA00CBCBCB00C0C0C000C0C0C000D6D6
      D600F8F8F800B8B8B8000000000000000000000000000000000000000000FCFC
      FC00FAF9F900F9F7F700F7D4B100F3D0AE00EBCAA900DFC0A00097949100FBFB
      FB00F0F0EF00F0EFEF00EFEFEE00EFEEED00EEEDED00EEEDEC00EDECEC00EDEC
      EB00ECEBEB00ECEBEA00EBEAEA00F8F8F7009C9C9A00F6F6F600FAF9F900D8D8
      D70093939100000000000000000000000000000000000000000000000000FDFD
      FD00FBFBFB00FAFAFA00E2E2E200E0E0E000DCDCDC00D6D6D600C0C0C000FCFC
      FC00F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3
      F300F3F3F300F2F2F200F2F2F200FAFAFA00C5C5C500F9F9F900FBFBFB00E7E7
      E700C0C0C000000000000000000000000000000000003B638A008CB9DE0071A8
      D60070A8D50070A7D4006EA5D2006DA3CF006BA0CB00699CC50070707000FBFB
      FB00F0F0EF00F0EFEF00EFEFEE00EFEEED00EEEDED00EEEDEC00EDECEC00EDEC
      EB00ECEBEB00ECEBEA00EBEAEA00F8F8F7009C9C9A00F6F6F600FAF9F900F3F3
      F3009191900000000000000000000000000000000000A8A8A800D9D9D900D0D0
      D000D0D0D000D0D0D000CECECE00CDCDCD00CBCBCB00C9C9C900ACACAC00FCFC
      FC00F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3
      F300F3F3F300F2F2F200F2F2F200FAFAFA00C5C5C500F9F9F900FBFBFB00F8F8
      F800BEBEBE00000000000000000000000000000000000000000000000000FCFC
      FC00F9F9F900F8F8F700F6F5F500F1F1F100EAEAEA00DEDDDD0097979700FBFB
      FB00F0EFEE00EFEEEE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECECEB00ECEB
      EA00ECEBEA00EBEAE900EBEAE900F8F8F7009C9C9A00F2F2F100D9D9D8009494
      920000000000000000000000000000000000000000000000000000000000FDFD
      FD00FBFBFB00FAFAFA00F9F9F900F7F7F700F2F2F200EBEBEB00C2C2C200FCFC
      FC00F5F5F500F5F5F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2
      F200F2F2F200F2F2F200F2F2F200FAFAFA00C5C5C500F6F6F600E8E8E800C0C0
      C00000000000000000000000000000000000000000003C648C008EBBDE0073AB
      D80073AAD70072AAD50071A8D40070A6D1006EA4CE006C9FC80070707000FBFB
      FB00F0EFEE00EFEEEE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECECEB00ECEB
      EA00ECEBEA00EBEAE900EBEAE900F8F8F7009C9C9A00F2F2F100F3F3F3009292
      90000000000000000000000000000000000000000000A9A9A900DADADA00D2D2
      D200D1D1D100D1D1D100D0D0D000CECECE00CDCDCD00CACACA00ACACAC00FCFC
      FC00F5F5F500F5F5F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2
      F200F2F2F200F2F2F200F2F2F200FAFAFA00C5C5C500F6F6F600F8F8F800BFBF
      BF0000000000000000000000000000000000000000000000000000000000FCFC
      FC00F9F9F800F7F7F700F5F5F500F1F1F100EAEAEA00DFDFDF0097979700FBFB
      FB00EFEEED00EEEDED00EEEDEC00EDECEC00EDECEB00ECEBEB00ECEBEA00EBEA
      EA00EBEAE900EBEAE900EAE9E800F8F7F700BEBEBD00DADAD900979795000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FBFBFB00FAFAFA00F9F9F900F6F6F600F2F2F200ECECEC00C2C2C200FCFC
      FC00F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2
      F200F2F2F200F2F2F200F1F1F100FAFAFA00D8D8D800E8E8E800C1C1C1000000
      000000000000000000000000000000000000000000003D668E008FBCE00076AD
      D90076ACD80075ACD70074AAD60073A9D40071A7D1006FA3CC0070707000FBFB
      FB00EFEEED00EEEDED00EEEDEC00EDECEC00EDECEB00ECEBEB00ECEBEA00EBEA
      EA00EBEAE900EBEAE900EAE9E800F8F7F700BEBEBD00F3F3F300919190000000
      00000000000000000000000000000000000000000000AAAAAA00DBDBDB00D2D2
      D200D2D2D200D2D2D200D1D1D100D0D0D000CFCFCF00CDCDCD00ACACAC00FCFC
      FC00F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2
      F200F2F2F200F2F2F200F1F1F100FAFAFA00D8D8D800F8F8F800BFBFBF000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F8F8F700F7F6F500F7D3AE00F4D0AC00EECBA700E5C3A10098949100FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FCFCFC00D8D8D70098989600000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FAFAFA00F9F9F900E1E1E100DFDFDF00DCDCDC00D8D8D800C0C0C000FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FDFDFD00E7E7E700C2C2C200000000000000
      000000000000000000000000000000000000000000003F67900090BCE00078AF
      DB0078AEDA0078AEDA0077ADD80076ABD70075A9D40073A6D00070707000FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FCFCFC00F3F3F30093939100000000000000
      00000000000000000000000000000000000000000000AAAAAA00DBDBDB00D4D4
      D400D4D4D400D4D4D400D2D2D200D2D2D200D0D0D000CECECE00ACACAC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FDFDFD00F8F8F800C0C0C000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F8F7F700F7F6F600F5F5F500F2F2F200EDEDED00E7E7E600AFAFAF009696
      9600959595009595950095959500959595009595950095959500959595009696
      9600999999009393930099999900999999009B9B9A0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FAFAFA00FAFAFA00F9F9F900F7F7F700F4F4F400F0F0F000D0D0D000C1C1
      C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1
      C100C3C3C300C0C0C000C3C3C300C4C4C400C4C4C40000000000000000000000
      00000000000000000000000000000000000000000000416A920092BEE1007BB1
      DC007BB1DC007AAFDA0078ADD70076AAD40075A8D10074A7CF006F7D89006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E007E80800000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00DCDCDC00D5D5
      D500D5D5D500D4D4D400D2D2D200D1D1D100D0D0D000CFCFCF00B4B4B400AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00B4B4B40000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F7F7F700F7F6F600F5F5F500F3F3F300F0F0EF00EBEAEA00E5E5E500E1E1
      E000DDDDDD00DBDBDB00DBDAD900DFDFDF00E1E0E000E1E1E100E3E3E300EBEB
      EB00FDFDFD000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FAFAFA00F9F9F900F9F9F900F7F7F700F5F5F500F3F3F300EFEFEF00EDED
      ED00EBEBEB00E9E9E900E9E9E900ECECEC00ECECEC00EDEDED00EEEEEE00F3F3
      F300FDFDFD000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000426B930093C0E2007DB3
      DE007CB3DD007AAFD90076A9D10071A2C9006E9EC4006C9CC1006C9BC0006B9A
      BF006B99BE006B99BD006A99BD006A98BD006A98BD006B99BE006E9DC30072A4
      CB0076A9D20078ACD50078ACD5006998C0003F68910000000000000000000000
      00000000000000000000000000000000000000000000ADADAD00DDDDDD00D6D6
      D600D6D6D600D4D4D400D0D0D000CCCCCC00C9C9C900C8C8C800C7C7C700C7C7
      C700C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C9C9C900CDCD
      CD00D0D0D000D2D2D200D2D2D200C6C6C600ABABAB0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F7F6F600F6F6F500F9D4AF00F8D3AE00F6D2AD00F3CFAA00F0CCA900EEC9
      A600E7E3DE00E5E5E400E4E4E400EBEBEB00C2C2C100B9B9B800BABAB900D3D3
      D200E5E5E4000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FAFAFA00F9F9F900E2E2E200E1E1E100E0E0E000DEDEDE00DDDDDD00DBDB
      DB00EEEEEE00EFEFEF00EFEFEF00F3F3F300DBDBDB00D6D6D600D6D6D600E4E4
      E400EFEFEF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000436C950095C0E3007FB5
      DF007FB4DE007BAFD80074A3C9006A94B600648DAD006289A9006188A7006087
      A6006087A6006086A5006086A5006087A5006188A700638BAB006993B50072A2
      C80079ACD5007CB0DA007CB0DA006B9AC200406A930000000000000000000000
      00000000000000000000000000000000000000000000ADADAD00DDDDDD00D7D7
      D700D6D6D600D4D4D400CDCDCD00C3C3C300BFBFBF00BDBDBD00BCBCBC00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BCBCBC00BEBEBE00C3C3C300CBCB
      CB00D2D2D200D4D4D400D4D4D400C7C7C700ACACAC0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F6F6F600F6F6F500F5F5F500F4F4F400F3F3F200F1F1F100F0F0EF00EFEE
      ED00EDEDEC00ECECEB00ECECEB00F2F2F200C3C3C200F2F2F200F5F5F500E4E4
      E300C5C5C4000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00F9F9F900F9F9F900F9F9F900F8F8F800F7F7F700F6F6F600F5F5F500F5F5
      F500F3F3F300F3F3F300F3F3F300F7F7F700DBDBDB00F7F7F700F8F8F800EFEF
      EF00DDDDDD000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000446D960096C1E40081B6
      E00080B6E0007DB1DA005A636A005A5A5A00595A5A0059595900595959005959
      5900595959005959590059595900595959005959590059595900585858006073
      81007CB0D9007FB4DD007FB5DE006D9DC400416B940000000000000000000000
      00000000000000000000000000000000000000000000AEAEAE00DDDDDD00D8D8
      D800D8D8D800D5D5D500A5A5A5009F9F9F009E9E9E009E9E9E009E9E9E009E9E
      9E009E9E9E009E9E9E009E9E9E009E9E9E009E9E9E009E9E9E009E9E9E00AEAE
      AE00D4D4D400D6D6D600D7D7D700C9C9C900ADADAD0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F6F6F500F6F5F500F5F5F500F5F4F400F4F3F300F3F3F200F3F2F200F1F1
      F000F1F1F000F0F0F000F0F0EF00F7F7F700C6C6C500F4F4F400E7E7E700C5C5
      C400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00F9F9F900F9F9F900F9F9F900F8F8F800F7F7F700F7F7F700F7F7F700F6F6
      F600F6F6F600F6F6F600F5F5F500FAFAFA00DDDDDD00F8F8F800F0F0F000DDDD
      DD00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000456E970097C3E50082B8
      E20082B8E10080B6DF005D5F6000C2C2C200DCDCDC00DCDCDC00DBDBDB00DBDB
      DB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00ABABAB00626E
      760080B5DE0081B7E00082B8E1006E9EC600426C950000000000000000000000
      00000000000000000000000000000000000000000000AEAEAE00DFDFDF00D9D9
      D900D9D9D900D8D8D800A2A2A200DBDBDB00EAEAEA00EAEAEA00EAEAEA00EAEA
      EA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00CECECE00ABAB
      AB00D7D7D700D8D8D800D9D9D900CACACA00ADADAD0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F6F5F500F5F5F400F5F5F400F5F4F400F4F4F400F4F4F300F3F3F200F3F3
      F200F3F2F200F2F2F100F2F2F100F9F9F900DBDBDA00E9E9E900C6C6C5000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00F9F9F900F8F8F800F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F7F7
      F700F7F7F700F6F6F600F6F6F600FBFBFB00E9E9E900F2F2F200DDDDDD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000456F9800A3C7E400A0C7
      E600A0C7E6009FC6E5008AA5B9006C7276007E7F7F00ADADAD00E0E0E000D6D6
      D600E4E4E400E3E3E300D6D6D600DBDBDB00A4A4A400787878006F767C0090AE
      C6009FC6E5009FC7E600A0C7E6007AA9D0004B739B0000000000000000000000
      00000000000000000000000000000000000000000000AFAFAF00E0E0E000E0E0
      E000E0E0E000E0E0E000CBCBCB00ADADAD00B4B4B400CFCFCF00EDEDED00E7E7
      E700EFEFEF00EEEEEE00E7E7E700EAEAEA00CACACA00B0B0B000AFAFAF00D1D1
      D100E0E0E000E0E0E000E0E0E000D0D0D000B1B1B10000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FDFDFD00E9E9E900C7C7C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00F2F2F200DEDEDE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000097AFC60048729A004872
      9B0048729B0048729B0048729B0048729A004E6E8D00606E7B009B9C9C00F0F0
      F0008282820097989800F0F0F000868686005B6D7F004C6E900048729B004872
      9B0048729B0048729B0048729B0049729A00A0B5C80000000000000000000000
      00000000000000000000000000000000000000000000D2D2D200B1B1B100B1B1
      B100B1B1B100B1B1B100B1B1B100B1B1B100ADADAD00ACACAC00C5C5C500F6F6
      F600B6B6B600C2C2C200F6F6F600B8B8B800ACACAC00AEAEAE00B1B1B100B1B1
      B100B1B1B100B1B1B100B1B1B100B0B0B000D5D5D50000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008A8A8A00EDED
      ED00A0A0A000B4B4B400E5E5E500828282000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BBBBBB00F4F4
      F400C7C7C700D3D3D300EFEFEF00B6B6B6000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000BCBDBD008888
      880085858500858585008A8A8A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D8D8D800B9B9
      B900B8B8B800B8B8B800BBBBBB00000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004343
      7C0032327C0058587F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009B9B
      9B0094949400A3A3A30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007C5B
      4A0079422500856E630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009E9E
      9E008F8F8F00A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000555599003333B5005555
      E1006464EC004848D0002B2B8E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A8009E9E9E00B4B4
      B400BCBCBC00ACACAC0094949400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000956C5800AC67
      4400E1967200A25D3A009C847700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A9A9A900A5A5
      A500C0C0C0009F9F9F00B6B6B600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006C6CB3004141C6006262E6006B6B
      D7006161CA006464E2004D4DDA0042428B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B6B6B600A7A7A700BABABA00BBBB
      BB00B5B5B500BABABA00B0B0B0009D9D9D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000966D5700AF694400E69C
      7600E9A17C00E0987300924E2A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A800A5A5A500C3C3
      C300C6C6C600C1C1C10096969600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002E2EAB004E4EDF003636B3005F5F
      AB00000000002424A7004D4DE1002727A2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009B9B9B00B1B1B1009F9F9F00AEAE
      AE000000000096969600B1B1B100969696000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C766100AF684300E79C7600EAA3
      7E00ECAC8B00AA674300BD988400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ADADAD00A5A5A500C3C3C300C7C7
      C700CDCDCD00A4A4A400C2C2C200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000004848A3005151E0003232C4004B4B8C000000
      00000000000024249F004C4CE1002D2DAE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A4A4A400B2B2B200A1A1A100A1A1A1000000
      00000000000094949400B0B0B0009B9B9B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A98D7E00AA633E00E69B7500E9A07B00EDAD
      8C00B3704B00B88A710000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BBBBBB00A2A2A200C3C3C300C6C6C600CDCD
      CD00A9A9A900B9B9B90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002B2BA8004E4EE2002323A400000000000000
      0000000000003636B7004949DD002222A0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099999900B1B1B10095959500000000000000
      000000000000A0A0A000AEAEAE00949494000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A35D3500E3987200E89E7800EDAC8A00C17D
      5800AD7A5E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009E9E9E00C1C1C100C5C5C500CDCDCD00B1B1
      B100AFAFAF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003839BD004C4EE0001E1F9800000000000000
      00003C3C8C005354E0003C41D10037378D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A2A2A200B1B1B10091919100000000000000
      00009B9B9B00B3B3B300A9A9A9009A9A9A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009F5B3400DB916A00E89D7700ECA98600D38F6A00A567
      4300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009D9D9D00BCBCBC00C4C4C400CBCBCB00BBBBBB00A4A4
      A400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003136B6005257E4002D2DAA00000000004444
      87004040C5004E57E100252CA800000000006D6D93002B2B95002828A4002525
      9E0045458B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009F9F9F00B5B5B5009A9A9A00000000009D9D
      9D00A6A6A600B4B4B4009999990000000000B0B0B00096969600979797009494
      94009F9F9F000000000000000000000000000000000000000000000000000000
      000000000000A0684600CC835B00E89D7700EAA47F00E29E7900A25D34000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A4A4A400B4B4B400C4C4C400C7C7C700C4C4C4009E9E9E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000232797005869E200595BE6003B3BBF004D4D
      D8005760E8003F4EC70015158200252596004646CE006C6FEE007A7BF2007477
      EF005455DC002525910000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000094949400BBBBBB00B7B7B700A3A3A300AFAF
      AF00B9B9B900ACACAC008989890094949400AAAAAA00C0C0C000C6C6C600C4C4
      C400B3B3B3009292920000000000000000000000000000000000000000000000
      0000C29B8300B9704700E89D7700E89F7A00EBA68300B16A4000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C3C3C300A9A9A900C4C4C400C5C5C500C9C9C900A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003038A7007784EB006A75ED005F6F
      EA00516EDC0015198C003737B9006469EC007780E8004E52BA003233A0003B3E
      A9005970E1004A4CD3005F5F8D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009E9E9E00C8C8C800C2C2C200BFBF
      BF00BBBBBB008C8C8C00A1A1A100BEBEBE00C6C6C600ACACAC009B9B9B00A1A1
      A100BEBEBE00AEAEAE00A9A9A900000000000000000000000000000000000000
      0000A8613500E39C7500E8A07A00EAA78300CE886000AC775600000000000000
      0000000000000000000000000000A45F3400A1582B00A1582B00A1582B00A158
      2B00A1582B00A1582B00A1582B00A1582B00A1582B00A1582B00A1582B00A158
      2B00A1582B00A0582B00A2795F00000000000000000000000000000000000000
      00009F9F9F00C3C3C300C5C5C500C9C9C900B7B7B700ADADAD00000000000000
      00000000000000000000000000009E9E9E009A9A9A009A9A9A009A9A9A009A9A
      9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A
      9A009A9A9A009A9A9A00AEAEAE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000025278F005467D3006083
      E900313FB2004C4CD7005E6EEB005568CF00393A960000000000000000000000
      00003743BA00596AEA0038389100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000092929200B8B8B800C6C6
      C600A2A2A200AEAEAE00BFBFBF00B7B7B7009B9B9B0000000000000000000000
      0000A5A5A500BDBDBD009B9B9B0000000000000000000000000000000000B982
      5F00CD895F00E8A67F00E9A98300E5A57F00AA63350000000000000000000000
      00000000000000000000CEA88F00B9734600E8AA8500E8A67F00E8A67F00E8A6
      7F00E8A67F00E8A67F00E8A67F00E8A67F00E8A67F00E8A67F00E8A67F00E8A6
      7F00E8A67F00E7A57E00A45C2E0000000000000000000000000000000000B3B3
      B300B7B7B700C8C8C800CACACA00C7C7C700A0A0A00000000000000000000000
      00000000000000000000CACACA00AAAAAA00CBCBCB00C8C8C800C8C8C800C8C8
      C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8
      C800C8C8C800C7C7C7009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000091919100808080005C5C7800252CA000565FE5005C5E
      EC005B61EA005D6BEC004F75DD00363786000000000000000000000000005B5B
      91004649CF005675E50047478C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BFBFBF00B5B5B500A4A4A40097979700B8B8B800B9B9
      B900BABABA00BEBEBE00BEBEBE0098989800000000000000000000000000A8A8
      A800ACACAC00BFBFBF00A0A0A00000000000000000000000000000000000B16A
      3A00E7AB8300E8AC8400E9AF8900C8855900B08C730000000000000000000000
      0000000000000000000000000000C28E6C00BE7B4F00EEBA9700EAB18B00E8AC
      8400E8AC8400E8AC8400E8AC8400E8AC8400E8AC8400E8AC8400E8AC8400E8AC
      8400E8AC8400E7AA8300A75E2E0000000000000000000000000000000000A4A4
      A400CACACA00CBCBCB00CDCDCD00B4B4B400B9B9B90000000000000000000000
      0000000000000000000000000000BABABA00AEAEAE00D4D4D400CECECE00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CACACA009D9D9D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000828181009E9B9A00BDB7B500D1C9C600C8C0C50035368B005B77D8007C96
      F2007887E500698AED00414AC800717190000000000000000000464688003737
      B9006177EC00374FB70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B6B6B600C5C5C500D5D5D500DFDFDF00DBDBDB0098989800BEBEBE00D0D0
      D000C8C8C800C9C9C900AAAAAA00B1B1B10000000000000000009E9E9E00A1A1
      A100C2C2C200A9A9A90000000000000000000000000000000000C18C6800D193
      6700E8B08900E8B28B00E7AF8800AE6735000000000000000000000000000000
      000000000000000000000000000000000000C48F6B00C2805300EEBE9B00EAB5
      8F00E8B08900E8B08900E8B08900E8B08900E8B08900E8B08900E8B08900E8B0
      8900E8B08900E7AF8800AA612F00000000000000000000000000B9B9B900BCBC
      BC00CDCDCD00CECECE00CDCDCD00A2A2A2000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00B1B1B100D6D6D600D0D0
      D000CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCD
      CD00CDCDCD00CDCDCD009F9F9F00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000086868600BDBB
      B900E5E0DD00E6E0DD00E3DCD900E1D9D600D6CECA00ABA4AD002E2F8E004857
      BD00161A87005482E2005960E8002B2BA800222297002D2DAB004E51DA00707C
      EF00556ACE005355A70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B8B8B800D6D6
      D600ECECEC00ECECEC00EAEAEA00E8E8E800E2E2E200CBCBCB0096969600ADAD
      AD008C8C8C00C3C3C300B9B9B90099999900939393009A9A9A00B1B1B100C6C6
      C600B8B8B800AAAAAA0000000000000000000000000000000000B2693500EAB4
      8B00ECB68E00EDB79000D89B6E00AD7D5B000000000000000000000000000000
      00000000000000000000000000000000000000000000C48F6900C6855600F1C2
      9F00EEB99300ECB68E00ECB68E00ECB68E00ECB68E00ECB68E00ECB68E00ECB6
      8E00ECB68E00EBB58C00AD632F00000000000000000000000000A3A3A300CFCF
      CF00D1D1D100D1D1D100C0C0C000B0B0B0000000000000000000000000000000
      00000000000000000000000000000000000000000000BABABA00B4B4B400D8D8
      D800D2D2D200D1D1D100D1D1D100D1D1D100D1D1D100D1D1D100D1D1D100D1D1
      D100D1D1D100D0D0D0009F9F9F00000000000000000000000000000000000000
      00000000000000000000000000000000000084848400B5B4B400EFECEB00EFEC
      EB00ECE9E600E9E4E200E1DBD800B7B3B100918F8E00D3CCC800C6BEC6004948
      7F00000000002939A200728CEF006F74EF006C6EEF00747EF0007E8DED00414C
      B3005E60AC000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B7B7B700D3D3D300F3F3F300F3F3
      F300F1F1F100EEEEEE00E9E9E900D2D2D200BDBDBD00E0E0E000DBDBDB009D9D
      9D00000000009C9C9C00CBCBCB00C3C3C300C0C0C000C6C6C600CBCBCB00A8A8
      A800AFAFAF000000000000000000000000000000000000000000CA865500F1B9
      9100F1B99100F1BA9200C9855300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C28C6500CA89
      5A00F5C4A100F2BC9500F1B99100F1B99100F1B99100F1B99100F1B99100F1B9
      9100F1B99100F0B89000B0652F00000000000000000000000000B4B4B400D2D2
      D200D2D2D200D3D3D300B3B3B300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B8B8B800B6B6
      B600D9D9D900D4D4D400D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200A0A0A000000000000000000000000000000000000000
      000000000000000000000000000094949400E5E5E500F9F8F700F6F4F400F3F0
      EF00E2DFDD00ADABAA0092919100000000008C8A8A00E1D9D600DFD7D2008786
      850000000000000000002C329B006268CA00676BCE00454AB1003C3D9A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000EFEFEF00FAFAFA00F8F8F800F6F6
      F600EBEBEB00CDCDCD00BFBFBF0000000000BBBBBB00E8E8E800E7E7E700B7B7
      B700000000000000000099999900B7B7B700B9B9B900A7A7A7009D9D9D000000
      00000000000000000000000000000000000000000000CB977100DEA07300F2BB
      9300F2BB9300F2BB9400BE764000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B369
      3100E2A67B00F2BB9300F2BB9300F2BB9300F2BB9300F2BB9300F2BB9300F2BB
      9300F2BB9300F1B99100B36730000000000000000000BFBFBF00C3C3C300D4D4
      D400D4D4D400D4D4D400AAAAAA00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A3A3
      A300C7C7C700D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D2D2D200A2A2A200000000000000000000000000000000000000
      0000000000008E8E8E00B2B2B200FCFCFC00FEFEFE00FCFCFB00E5E4E400A7A6
      A6009F9F9F00000000000000000000000000BAB6B400E3DCD900DFD8D3007C7C
      7C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BDBDBD00D2D2D200FDFDFD00FEFEFE00FCFCFC00EFEFEF00CBCB
      CB00C7C7C700000000000000000000000000D4D4D400EAEAEA00E7E7E700B2B2
      B200000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BC774300EBB28900F2BB
      9400F2BB9400F2BB9400BC703700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B2703F00D999
      6A00F2BB9400F2BB9400F2BB9400F2BB9400F2BB9400F2BB9400F2BB9400F2BB
      9400F2BB9400F1B99200B66930000000000000000000AAAAAA00CECECE00D4D4
      D400D4D4D400D4D4D400A6A6A600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A7A7A700BFBF
      BF00D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D2D2D200A2A2A200000000000000000000000000000000000000
      000087878700C9C9C900FFFFFF00FFFFFF00E1E1E100A2A2A200ACACAC000000
      000000000000000000000000000087878600E6E1DF00E6E0DD00CCC6C3008B8B
      8B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900DFDFDF00FFFFFF00FFFFFF00EDEDED00C9C9C900CECECE000000
      0000000000000000000000000000B8B8B800EDEDED00ECECEC00DDDDDD00BCBC
      BC00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BA6C3000F1BA9400F1BB
      9400F1BB9400F1BB9400C2773E00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BC713800E0A37700F1BB
      9400F1BB9400F1BB9400F1BB9400F1BB9400F1BB9400F1BB9400F1BB9400F1BB
      9400F1BB9400F0B99300B96C31000000000000000000A3A3A300D3D3D300D4D4
      D400D4D4D400D4D4D400AAAAAA00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A700C5C5C500D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D2D2D200A3A3A300000000000000000000000000000000008989
      8900DCDCDC00FDFDFD00D5D5D5009D9D9D00B7B7B70000000000000000000000
      0000000000000000000000000000B6B5B400ECE9E600E9E4E200AAA7A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BABA
      BA00EAEAEA00FDFDFD00E6E6E600C5C5C500D4D4D40000000000000000000000
      0000000000000000000000000000D3D3D300F1F1F100EEEEEE00CBCBCB000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BF713400F0BB9500F0BB
      9500F0BB9500F0BB9500D18C5800AA8E79000000000000000000000000000000
      0000000000000000000000000000B37F5500C77E4500E9B08700F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F2C09C00F1BD9800F0BB9500F0BB
      9500F0BB9500EFB99300BC6E31000000000000000000A6A6A600D4D4D400D4D4
      D400D4D4D400D4D4D400B7B7B700BBBBBB000000000000000000000000000000
      0000000000000000000000000000B0B0B000AEAEAE00CDCDCD00D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D6D6D600D5D5D500D4D4D400D4D4
      D400D4D4D400D2D2D200A4A4A4000000000000000000000000008E8E8E00DDDD
      DD00C9C9C9009797970000000000000000000000000000000000000000000000
      0000000000000000000088888800EDEBEA00EFECEB00E9E4E200858484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDBDBD00EBEB
      EB00DFDFDF00C2C2C20000000000000000000000000000000000000000000000
      00000000000000000000BABABA00F2F2F200F3F3F300EEEEEE00B7B7B7000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BF713200EFBA9300F0BB
      9500F0BB9500F0BB9500EAB28900C1763A00A9907D0000000000000000000000
      000000000000B4825A00C3763900DC9D6D00EFBA9400F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BC9600F4C5A300F0C19F00F5C7A600F1BE9900F0BB
      9500F0BB9500EFB99300BF7031000000000000000000A6A6A600D3D3D300D4D4
      D400D4D4D400D4D4D400CECECE00A9A9A900BCBCBC0000000000000000000000
      000000000000B2B2B200A9A9A900C1C1C100D3D3D300D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400DADADA00D8D8D800DBDBDB00D5D5D500D4D4
      D400D4D4D400D2D2D200A5A5A50000000000000000008F8F8F009F9F9F009F9F
      9F00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00F6F4F400F3F0EF00B9B6B500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BDBDBD00C7C7C700C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D8D8D800F8F8F800F6F6F600D4D4D400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C9864F00E8AF8400F0BB
      9600F0BB9500F0BB9500F0BB9500EAB18800D18B5400C5773700C2723100C678
      3A00D0895100DFA17200EEB99200F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F1BD9800F4C6A600E6AE8400C3743400D8966300F4C6A500F1BE
      9900F0BB9500EFBA9300C07232000000000000000000B3B3B300CCCCCC00D4D4
      D400D4D4D400D4D4D400D4D4D400CECECE00B6B6B600A9A9A900A6A6A600AAAA
      AA00B5B5B500C4C4C400D2D2D200D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D5D5D500DBDBDB00CBCBCB00A8A8A800BDBDBD00DBDBDB00D5D5
      D500D4D4D400D3D3D300A6A6A6000000000000000000BEBEBE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008B8B8B00F6F6F500F9F8F700EAE8E80087878700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D9D9D900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BBBBBB00F9F9F900FAFAFA00F1F1F100B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DEB18C00DB996500F1BD
      9800F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F2C19D00F5C9A800DEA17000CA83470000000000D1936100D9966200F4C6
      A400F1BE9900EFBA9300C37432000000000000000000CDCDCD00BEBEBE00D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D7D7D700DCDCDC00C3C3C300B0B0B00000000000BBBBBB00BDBDBD00DBDB
      DB00D5D5D500D3D3D300A7A7A700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C3C3C300FEFEFE00FBFBF9009C9C9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DCDCDC00FEFEFE00FCFCFC00C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CA7B3A00EFBC
      9600F1BE9900F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F1BD9800F5C7
      A600F1C29F00D38B5100D2976600000000000000000000000000D5996600DA97
      6100F4C6A500F0BB9600C5763200000000000000000000000000ACACAC00D4D4
      D400D5D5D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5D500DBDB
      DB00D8D8D800B5B5B500BDBDBD00000000000000000000000000BEBEBE00BDBD
      BD00DBDBDB00D4D4D400A8A8A800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008D8D8D00FBFBFB00FFFFFF00BCBCBC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BCBCBC00FCFCFC00FFFFFF00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEAD8400D893
      5A00F4C6A400F1BE9A00F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BD9800F4C6A500F6CBAB00E2A6
      7600CB7D3C00000000000000000000000000000000000000000000000000D69A
      6800DB976100F2C19D00C7773300000000000000000000000000CBCBCB00BABA
      BA00DBDBDB00D6D6D600D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5D500DBDBDB00DDDDDD00C6C6
      C600ACACAC00000000000000000000000000000000000000000000000000BFBF
      BF00BDBDBD00D7D7D700A9A9A900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C8C8C800FFFFFF00D5D5D5008D8D8D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DFDFDF00FFFFFF00E6E6E600BCBCBC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D89B
      6600DA965D00F5C8A700F4C6A500F1BE9900F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BC9700F3C2A000F6C9A900F6CCAD00E6AB7D00D0824000D9A1
      7100000000000000000000000000000000000000000000000000000000000000
      0000D89C6800DB975F00CC7C380000000000000000000000000000000000BFBF
      BF00BCBCBC00DCDCDC00DBDBDB00D5D5D500D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D8D8D800DCDCDC00DEDEDE00C9C9C900B0B0B000C3C3
      C300000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000BDBDBD00ACACAC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008E8E
      8E00FBFBFB00DFDFDF008C8C8C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BDBD
      BD00FCFCFC00ECECEC00BCBCBC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E0AE8300D1834000E6AB7C00F5CAA900F8CFB100F7CCAD00F7CCAD00F7CC
      AD00F8CFB200F7CDAF00EDB89000DF9D6700CF803C00DBA17000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DEAA7D0000000000000000000000000000000000000000000000
      0000CBCBCB00B0B0B000C9C9C900DDDDDD00E0E0E000DEDEDE00DEDEDE00DEDE
      DE00E0E0E000DFDFDF00D2D2D200C0C0C000ADADAD00C3C3C300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C9C9C90000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C3C3
      C300E4E4E4008A8A8A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DCDC
      DC00EFEFEF00BABABA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E0AB7C00D0823D00D4874500DA925600DB965B00D992
      5500D4874400CF7F3900DB9F6A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C9C9C900AFAFAF00B3B3B300B9B9B900BCBCBC00B9B9
      B900B2B2B200ADADAD00C2C2C200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000093939300DFDF
      DF008C8C8C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000ECEC
      EC00BCBCBC000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000A0A0A0008E8E
      8E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7C7C700BDBD
      BD00000000000000000000000000000000000000000000000000000000000000
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
      00000000000000000000000000000000000000000000B4B4B400939393000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D3D3D300C0C0C0000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      00005959A2005B5BA60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000ABABAB00ACACAC0000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000002525
      96005D5DDC005B5BDA0032329D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009494
      9400B6B6B600B5B5B50099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000433A36002C201C001F131200190E0D00190E0D00190E0D00190E0D00190E
      0D00190E0D00190E0D00190E0E00190E0E00190E0E00190E0E00190E0E00190E
      0E00180E0E00180E0E00180E0E00180E0E003228280061656A00494872005F60
      D4006265E4006466E600595ADB006162B9000000000000000000000000000000
      00008C8C8C007D7D7D0076767600737373007373730073737300737373007373
      7300737373007373730073737300737373007373730073737300737373007373
      73007373730073737300737373007373730082828200A6A6A6009B9B9B00B6B6
      B600BBBBBB00BCBCBC00B5B5B500B2B2B2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000987C6E007942
      2500785645000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B2B2B2008F8F
      8F009C9C9C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003D33
      30003F353200524A4800180D0D00B9B7B600CEC6C400CEC6C400CEC6C400CEC6
      C400CEC6C400CEC6C400CEC6C400CEC6C400CEC6C400CEC6C400D1CBCA008A89
      89008989890088888800878686009292920091999E00676B6E00A8A6A6008A8C
      9B008193E5007B8FEA005266D5006E6FC3000000000000000000000000008787
      8700898989009696960073737300D4D4D400DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E0E0E000BABA
      BA00BABABA00B9B9B900B8B8B800BFBFBF00C3C3C300A9A9A900CBCBCB00BEBE
      BE00CDCDCD00CBCBCB00B7B7B700B9B9B9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C29E8C00A25D3A00E197
      7200AE6945008E644E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C5C5C5009F9F9F00C0C0
      C000A6A6A600A3A3A30000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003E322F003F34
      3200433B390039302E00180D0D00CDC9C700DAC6BF00CEBCB600797776007876
      76007D7A7900DAC6BF00DAC6BF00DAC6BF00DAC6BF00DAC6BF00E6DAD6009696
      960095959500949494009D9D9D008E9DA5003C677C0093919100D9D8D900C5C4
      C4008E909D006B7CD7002E31AD00000000000000000000000000878787008989
      89008D8D8D008686860073737300DFDFDF00DDDDDD00D8D8D800AFAFAF00AFAF
      AF00B1B1B100DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E9E9E900C2C2
      C200C1C1C100C0C0C000C6C6C600C6C6C600A7A7A700BFBFBF00E8E8E800DCDC
      DC00C0C0C000C1C1C1009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000924E2A00E0987300E9A1
      7C00E79C7600B16A460090665000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000096969600C1C1C100C6C6
      C600C3C3C300A6A6A600A5A5A500000000000000000000000000000000000000
      00000000000000000000000000000000000000000000453835003F353200433B
      390039302E0039302E00180D0C00CDC9C800DCC9C200D0BFBA00747474006F6F
      6F0072727100DCC9C200DCC9C200DCC9C200DCC9C200DCC9C200E7DCD8009B9B
      9B009A9A9A00A4A4A40092A0A800286B8A0065D2F1006CB1C700C2C1C100DAD9
      DA00A4A2A2004F4D8F000000000000000000000000008B8B8B00898989008D8D
      8D00868686008686860072727200DFDFDF00DFDFDF00D9D9D900AEAEAE00ABAB
      AB00ACACAC00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00EAEAEA00C5C5
      C500C4C4C400CACACA00C7C7C700A9A9A900E2E2E200D0D0D000DBDBDB00E8E8
      E800C9C9C900A2A2A20000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C29C8700AA674300ECAC
      8B00EAA37E00E79C7600AF694400956D58000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C300A4A4A400CDCD
      CD00C7C7C700C3C3C300A5A5A500A9A9A9000000000000000000000000000000
      000000000000000000000000000000000000000000002E1F1C007F7978003930
      2E0039302E0039302E00180D0C00CDC9C800E1D1CB00D6C8C3007E7E7E007777
      770076757500E1D1CB00E1D1CB00E1D1CB00E1D1CB00E1D1CB00EAE1DE009898
      9800A3A3A30093A1A900286C8B0065D2F10052D4FD004DD1FE0083B5C6009895
      95007B7D8000626060000000000000000000000000007D7D7D00B1B1B1008686
      8600868686008686860072727200DFDFDF00E4E4E400DFDFDF00B4B4B400B0B0
      B000AEAEAE00E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400EDEDED00C3C3
      C300C9C9C900C8C8C800AAAAAA00E2E2E200E4E4E400E3E3E300D3D3D300C1C1
      C100B3B3B300A3A3A30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B98A7100B370
      4B00EDAD8C00E9A07B00E69B7500AA633E00997E6F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B9B9B900A9A9
      A900CDCDCD00C6C6C600C3C3C300A2A2A200B2B2B20000000000000000000000
      000000000000000000000000000000000000000000002E1F1C008E8987003930
      2E0039302E0039302E00170D0C00CAC7C600E7DBD600DDD2CE00898989008282
      82007F7F7F00E7DBD600E7DBD600E7DBD600E7DBD600E7DBD600EDE6E4009898
      98008D9DA500286D8D0065D2F20052D5FD004ED3FE007DDFFF0055B0D3004E6A
      79007E7A7A00322D2A000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860072727200DEDEDE00E9E9E900E4E4E400BABABA00B6B6
      B600B4B4B400E9E9E900E9E9E900E9E9E900E9E9E900E9E9E900F0F0F000C3C3
      C300C6C6C600AAAAAA00E3E3E300E4E4E400E3E3E300EBEBEB00D1D1D100A9A9
      A900B1B1B1008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B37F
      6200C17D5800EDAC8A00E89E7800E3987200A35D350000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B2B2
      B200B1B1B100CDCDCD00C5C5C500C1C1C1009E9E9E0000000000000000000000
      000000000000000000000000000000000000000000002E1F1C008E8987003930
      2E0039302E00382F2D00170D0C00C6C4C300EDE4E000E3DAD700929292008D8D
      8D008A8A8A00EDE4E000EDE4E000EDE4E000EDE4E000EDE4E000F1ECEB008696
      9E00276F8F0065D2F20053D7FD004FD5FE007EE0FF0056B1D300335F77006C68
      680058504F002B2622000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860072727200DCDCDC00EEEEEE00E9E9E900BFBFBF00BCBC
      BC00BBBBBB00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00F3F3F300C2C2
      C200ACACAC00E3E3E300E5E5E500E4E4E400EBEBEB00D1D1D100A3A3A300A7A7
      A700999999008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6684400D38F6A00ECA98600E89D7700DB916A009C593200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A5A500BBBBBB00CBCBCB00C4C4C400BCBCBC009B9B9B00000000000000
      000000000000000000000000000000000000000000002F1F1C008E8987003930
      2E0039302E00382F2D00180E0D00C4C2C200F0E9E600E6DFDD00959595009494
      940094949400F0E9E600F0E9E600F0E9E600F0E9E600F2EBE900BECCD4002771
      910065D3F2005ADCFD0050D7FE007FE1FF0056B3D3002F5B73006C6867004037
      3500524A48002B2522000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860073737300DBDBDB00F1F1F100ECECEC00C1C1C100C0C0
      C000C0C0C000F1F1F100F1F1F100F1F1F100F1F1F100F3F3F300E1E1E100ADAD
      AD00E3E3E300E7E7E700E5E5E500EBEBEB00D1D1D100A1A1A100A7A7A7008B8B
      8B00969696007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A25E3500E29E7900EAA47F00E89D7700CC835B009A6342000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009E9E9E00C4C4C400C7C7C700C4C4C400B4B4B400A1A1A1000000
      000000000000000000000000000000000000000000002F1F1C008E8987003930
      2E0039302E00382F2D00180E0D00C4C2C200F1EAE700E6E0DD00969696009696
      960099999900F1EAE700F1EAE700F1EAE700F2ECE900BDCCD2002A75960066D3
      F20060E0FD0065E5FE0086E6FF0057B5D4003867800055505100403735003930
      2E00524A48002B2522000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860073737300DBDBDB00F2F2F200ECECEC00C2C2C200C2C2
      C200C3C3C300F2F2F200F2F2F200F2F2F200F3F3F300E1E1E100AFAFAF00E3E3
      E300E9E9E900EBEBEB00EEEEEE00D2D2D200A8A8A800999999008B8B8B008686
      8600969696007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B16A4000EBA68200E89F7A00E89D7700B97046009F81
      6E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A5A5A500C9C9C900C5C5C500C4C4C400A9A9A900B4B4
      B40000000000000000000000000000000000000000002F1F1B008E8988003A30
      2E003A302E0039302E00180E0D00A8A7A700CBC9C800CBC9C800CBC9C800CCC9
      C900CCC9C900CCC9C900CCC9C900D5D3D200B5C5CD002A76970066D3F20061E1
      FD0067E7FE008DEBFF0061BDD500396B8300888A8B00201615003A302E003A31
      2F00524B49002B2522000000000000000000000000007D7D7D00BABABA008686
      8600868686008686860073737300CBCBCB00DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00E4E4E400DDDDDD00B0B0B000E3E3E300E9E9
      E900ECECEC00F0F0F000D6D6D600A9A9A900BBBBBB0077777700868686008787
      8700969696007F7F7F00000000000000000000000000BB8B6C00A0582B00A158
      2B00A1582B00A1582B00A1582B00A1582B00A1582B00A1582B00A1582B00A158
      2B00A1582B00A1582B00A1582B00A1582B00A45E330000000000000000000000
      00000000000000000000B77F5B00CE865E00EAA47F00E89D7700E3987200A75F
      33000000000000000000000000000000000000000000B8B8B8009A9A9A009A9A
      9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A
      9A009A9A9A009A9A9A009A9A9A009A9A9A009E9E9E0000000000000000000000
      00000000000000000000B1B1B100B6B6B600C7C7C700C4C4C400C1C1C1009F9F
      9F0000000000000000000000000000000000000000002F1E1B00918B8A003E35
      33003E3533003E35330022181700170D0C00170D0C00170D0C00170D0C00170D
      0C00180E0D002B2221005049480068737A002974940066D3F20061E2FD0067E8
      FE008EECFF0062BED6002E617A00544F50001F151400241A19003E3533003E35
      3300564D4C002B2522000000000000000000000000007D7D7D00BBBBBB008989
      8900898989008989890079797900727272007272720072727200727272007272
      7200737373007E7E7E0095959500ADADAD00AEAEAE00E3E3E300EAEAEA00ECEC
      EC00F0F0F000D6D6D600A3A3A30098989800777777007A7A7A00898989008989
      8900979797007F7F7F00000000000000000000000000A45D2E00E79C7600E89D
      7700E89D7700E89D7700E89D7700E89D7700E89D7700E89D7700E89D7700E89D
      7700E89D7700E89D7700E89D7700E8A27E00B9714400C8A48C00000000000000
      0000000000000000000000000000AA623400E59D7800E9A07B00E89D7700CD84
      5A00A1715100000000000000000000000000000000009C9C9C00C3C3C300C4C4
      C400C4C4C400C4C4C400C4C4C400C4C4C400C4C4C400C4C4C400C4C4C400C4C4
      C400C4C4C400C4C4C400C4C4C400C7C7C700A9A9A900C8C8C800000000000000
      0000000000000000000000000000A0A0A000C4C4C400C6C6C600C4C4C400B4B4
      B400A9A9A900000000000000000000000000000000002F1E1B00948F8D00433A
      3800433A3800433A3800433A3800433A3800433A3800433A3800433A3800433A
      3800534A4800857F7D00A8AFB30070A2B8007DDAF40062E2FD0068E9FE008EED
      FF0063C0D70035698200736F6F004A413F00433A3800433A3800423A3800423A
      380058514F002B2623000000000000000000000000007D7D7D00BDBDBD008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C
      8C0096969600B4B4B400D0D0D000C9C9C900E7E7E700EAEAEA00EDEDED00F1F1
      F100D8D8D800A8A8A800ABABAB00909090008C8C8C008C8C8C008C8C8C008C8C
      8C009A9A9A0080808000000000000000000000000000A75F2E00E79E7800E89F
      7A00E89F7A00E89F7A00E89F7A00E89F7A00E89F7A00E89F7A00E89F7A00E89F
      7A00E89F7A00EAA68100EEB29000BE794C00C18E6B0000000000000000000000
      0000000000000000000000000000CB9D7F00C87F5300E9A37F00E89F7A00E79F
      7900B1683800000000000000000000000000000000009D9D9D00C4C4C400C5C5
      C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5
      C500C5C5C500C9C9C900D0D0D000ADADAD00BABABA0000000000000000000000
      0000000000000000000000000000C3C3C300B1B1B100C7C7C700C5C5C500C5C5
      C500A3A3A300000000000000000000000000000000002F1E1B00979391004941
      3E0049413E0049413E0049413E0049413E0049413E0049413E0049413E004C44
      41006E676500AEAAA900688798009FD9EA0091EBFD0079ECFE008FEEFF0063C0
      D700356A8300777473004F47440048403D00473F3D00473F3D00463F3C00473F
      3D005A5452002C2624000000000000000000000000007D7D7D00BFBFBF009090
      9000909090009090900090909000909090009090900090909000909090009292
      9200A6A6A600CDCDCD00B9B9B900E7E7E700F0F0F000EFEFEF00F1F1F100D8D8
      D800A9A9A900AEAEAE0094949400909090008F8F8F008F8F8F008F8F8F008F8F
      8F009B9B9B0080808000000000000000000000000000AA612F00E7A27C00E8A3
      7D00E8A37D00E8A37D00E8A37D00E8A37D00E8A37D00E8A37D00E8A37D00E8A3
      7D00EAA98400EEB59300C27D5000C38E6B000000000000000000000000000000
      000000000000000000000000000000000000AF663400E6A37D00E8A57F00E8A3
      7D00D18A5F00A67859000000000000000000000000009F9F9F00C6C6C600C7C7
      C700C7C7C700C7C7C700C7C7C700C7C7C700C7C7C700C7C7C700C7C7C700C7C7
      C700CACACA00D1D1D100B0B0B000BABABA000000000000000000000000000000
      000000000000000000000000000000000000A2A2A200C7C7C700C8C8C800C7C7
      C700B8B8B800AEAEAE000000000000000000000000002F1E1B009B9695005048
      4600504846001C131200140B0B00130B0A00130A0A00120A0900110A0900241D
      1C005C57570085888C00769AAE006B98A900C0F1FA00B9F4FF007CCBDE003165
      7E004E4D4E0017121200100B0B00110D0D00120E0E00161212004A4341004C45
      43005F5857002C2623000000000000000000000000007D7D7D00C2C2C2009494
      9400949494007676760072727200717171007171710070707000707070007B7B
      7B009D9D9D00B9B9B900C5C5C500C2C2C200F5F5F500F6F6F600DEDEDE00A6A6
      A600979797007575750071717100727272007373730075757500919191009393
      93009E9E9E0080808000000000000000000000000000AE632F00E7A78000E8A8
      8100E8A88100E8A88100E8A88100E8A88100E8A88100E8A88100E8A88100EAAD
      8800EEB89600C5815300BF8A6500000000000000000000000000000000000000
      000000000000000000000000000000000000C48D6700D5916600E8AA8300E8A8
      8100E6A67F00B16734000000000000000000000000009F9F9F00C9C9C900C9C9
      C900C9C9C900C9C9C900C9C9C900C9C9C900C9C9C900C9C9C900C9C9C900CCCC
      CC00D2D2D200B1B1B100B8B8B800000000000000000000000000000000000000
      000000000000000000000000000000000000B9B9B900BBBBBB00CACACA00C9C9
      C900C8C8C800A3A3A3000000000000000000000000002F1E1B009F9B99005850
      4D0058504D00170E0D00F5F5F500FCFDFD00FDFDFE00FCFDFD00FCFCFD00FBFC
      FC00FCFCFC008DA4B20098BDD1008EB3C6007798A800A8CEDA0087AABB00D6D7
      D800C6C5C400BFBDBD00BDBCBB00BFBDBD00C5C3C2001511110050494700534C
      4A0067605E002B2522000000000000000000000000007D7D7D00C4C4C4009999
      99009999990073737300F9F9F900FDFDFD00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FDFDFD00CACACA00D9D9D900D3D3D300C3C3C300E2E2E200CECECE00E7E7
      E700DCDCDC00D8D8D800D7D7D700D8D8D800DBDBDB0074747400959595009797
      9700A2A2A2007F7F7F00000000000000000000000000B1652F00E7AB8400E8AC
      8500E8AC8500E8AC8500E8AC8500E8AC8500E8AC8500E8AC8500E9AF8A00EEBB
      9800C8855600A779560000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C5804F00E8AD8600E8AC
      8500E8AC8500C6815000000000000000000000000000A0A0A000CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CDCDCD00D4D4
      D400B4B4B400ADADAD0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B1B1B100CCCCCC00CBCB
      CB00CBCBCB00B1B1B1000000000000000000000000002F1E1B00A39F9E005F58
      55005F585500180E0E00F4F4F400FBFBFC00FCFDFE00FDFEFE00FCFDFE00FCFD
      FD00E2E7EA007AA5BB009DCBE1008FB5C8007191A30094AAB500D4D4D400B3B0
      AF00A7A4A200ABA7A500B1AEAC00BAB6B400CECCCB00120E0E005A5350005D56
      5300706A67002C2522000000000000000000000000007D7D7D00C7C7C7009D9D
      9D009D9D9D0073737300F8F8F800FCFCFC00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00F1F1F100CBCBCB00E1E1E100D4D4D400BFBFBF00CDCDCD00E6E6E600D1D1
      D100C9C9C900CBCBCB00CFCFCF00D4D4D400E0E0E000737373009A9A9A009C9C
      9C00A8A8A8007F7F7F00000000000000000000000000B4683000E7AF8700E8B0
      8900E8B08900E8B08900E8B08900E8B08900E8B08900E8B08900E8B08900DB9F
      7400B36831000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD743E00E8B18A00E8B0
      8900E8B08900D7996C00A97E5E000000000000000000A2A2A200CDCDCD00CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00C3C3
      C300A2A2A2000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A9A9A900CECECE00CDCD
      CD00CDCDCD00BFBFBF00B1B1B10000000000000000002F1F1C00A8A3A200665F
      5C00665F5C00190F0E00F3F3F300F9F9FA00FBFBFC00FCFDFE00FDFEFE00FDFE
      FE00849BA50082B1C700739AAE0092A2AB00D1D2D200CFCECE00BCBAB900ACAA
      A800B0ACAA00B8B4B200C3BFBC00CECAC700E3E1DF000F0A0A00645C5A00665F
      5C0078716F002C2521000000000000000000000000007D7D7D00C9C9C900A1A1
      A100A1A1A10073737300F8F8F800FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFD
      FD00C5C5C500D2D2D200C5C5C500C9C9C900E4E4E400E2E2E200D6D6D600CDCD
      CD00CECECE00D2D2D200D9D9D900DFDFDF00EDEDED0070707000A0A0A000A1A1
      A100ACACAC007F7F7F00000000000000000000000000B76A3000EBB48C00ECB5
      8D00ECB58D00ECB58D00ECB58D00ECB58D00ECB58D00ECB58D00ECB58D00ECB5
      8D00D6966700B06E3D0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BB6F3700ECB58D00ECB5
      8D00ECB58D00E6AD8300B8733F000000000000000000A3A3A300D0D0D000D0D0
      D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000BEBEBE00A6A6A60000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A6A6A600D0D0D000D0D0
      D000D0D0D000CBCBCB00A8A8A8000000000000000000301F1B00ACA8A6006D66
      63006D6663001A0F0E00F2F2F100F7F6F600F9F9F900FBFBFC00FDFDFE00F1F3
      F500395D6D00889BA300CFD1D200D1D1D100C9C9C800C0C0BF00BFBDBB00C5C3
      C100CFCBC900D5D0CE00D9D4D200DED9D600EEECEA000F0909006C6562006D67
      64007E7876002C2421000000000000000000000000007D7D7D00CBCBCB00A5A5
      A500A5A5A50073737300F6F6F600F9F9F900FBFBFB00FCFCFC00FDFDFD00F8F8
      F800A1A1A100C5C5C500E4E4E400E4E4E400DFDFDF00D9D9D900D8D8D800DBDB
      DB00E0E0E000E3E3E300E5E5E500E8E8E800F3F3F30070707000A5A5A500A6A6
      A600B0B0B0007F7F7F00000000000000000000000000BA6C3100F0B88F00F1B9
      9100F1B99100F1B99100F1B99100F1B99100F1B99100F1B99100F1B99100F1B9
      9100F1B99100E1A27400BB713700AB9685000000000000000000000000000000
      00000000000000000000000000000000000000000000C2773E00F1B99100F1B9
      9100F1B99100F0B99100BA6C30000000000000000000A4A4A400D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200C5C5C500A7A7A700C0C0C0000000000000000000000000000000
      00000000000000000000000000000000000000000000AAAAAA00D2D2D200D2D2
      D200D2D2D200D2D2D200A3A3A3000000000000000000301E1B00AFABAA00736D
      6A00736D6A001A100F00F1F0F000F4F3F300F1F0F0002DCC410000CD190032D1
      4500D3D4D500D2D2D300CBCBCB00C3C3C300BCBCBC00B9B9B800BCBBBB00BFBE
      BE00C0BFBE00C2C0BF00E1DCDA00E1DCDA00F0EDEC000F0A0900716B6800736D
      6A00837D7B002C2421000000000000000000000000007D7D7D00CECECE00A9A9
      A900A9A9A90074747400F6F6F600F8F8F800F6F6F600BEBEBE00B4B4B400C1C1
      C100E6E6E600E4E4E400E0E0E000DCDCDC00D8D8D800D5D5D500D7D7D700D9D9
      D900D9D9D900D9D9D900EAEAEA00EAEAEA00F4F4F40070707000A8A8A800A9A9
      A900B3B3B3007F7F7F00000000000000000000000000BC6E3100F1B99100F2BB
      9300F2BB9300F2BB9300F2BD9600F4C09B00F2BB9300F2BB9300F2BB9300F2BB
      9300F2BB9300F2BB9300EAB08500C87E4500AF7A500000000000000000000000
      000000000000000000000000000000000000B4998400D28D5800F2BB9300F2BB
      9300F2BB9300F2BB9300BF7134000000000000000000A4A4A400D2D2D200D4D4
      D400D4D4D400D4D4D400D5D5D500D6D6D600D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400CDCDCD00AEAEAE00ADADAD0000000000000000000000
      000000000000000000000000000000000000C1C1C100B8B8B800D4D4D400D4D4
      D400D4D4D400D4D4D400A6A6A6000000000000000000301E1B00B2AEAD007872
      6F0078726F001A100E00F2F1F000F2F1EF00F4F3F200F6F6F600F9F9FA00FBFC
      FC00FCFDFE00FCFDFE00FCFDFE00FAFBFB00F7F8F800F5F5F500F1F1F000EEEC
      EB00EAE8E600E7E4E200E4E0DD00E3DEDC00F0EDEC00100A0A0076706D007872
      6F0087827F002C2421000000000000000000000000007D7D7D00D0D0D000ACAC
      AC00ACACAC0074747400F6F6F600F6F6F600F7F7F700F9F9F900FBFBFB00FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6F600F3F3
      F300F1F1F100EEEEEE00ECECEC00EBEBEB00F4F4F40070707000ABABAB00ACAC
      AC00B5B5B5007F7F7F00000000000000000000000000BF703100F1B99200F1BB
      9400F1BB9400F2BE9800F6C7A600F1C19E00F5C5A300F2BC9500F1BB9400F1BB
      9400F1BB9400F1BB9400F1BB9400F1BA9300DD9D6C00C3763900B07E56000000
      0000000000000000000000000000B0988500C3763A00EBB28900F1BB9400F1BB
      9400F1BB9400F1BA9300BF7032000000000000000000A5A5A500D2D2D200D4D4
      D400D4D4D400D5D5D500DBDBDB00D8D8D800DADADA00D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D3D3D300C1C1C100A9A9A900B0B0B0000000
      0000000000000000000000000000C1C1C100A9A9A900CECECE00D4D4D400D4D4
      D400D4D4D400D3D3D300A6A6A6000000000000000000311E1B00B4B1AF007C76
      73007C7673001A0F0E00F3F1F100F0EEEC00F1F0EF00F4F3F200F6F6F600F9F9
      F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F5F5F500F2F1
      F000EEECEB00EAE8E600E7E4E200E5E1DF00F0EEED00100A0A007A7471007C76
      73008A8583002C2421000000000000000000000000007D7D7D00D1D1D100AEAE
      AE00AEAEAE0073737300F6F6F600F4F4F400F5F5F500F7F7F700F9F9F900FBFB
      FB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6
      F600F3F3F300F1F1F100EEEEEE00EDEDED00F4F4F40070707000ADADAD00AEAE
      AE00B7B7B7007F7F7F00000000000000000000000000C1723200EFBA9300F0BB
      9500F1BE9900F5C6A500D8966300C3743400E6AE8400F5C6A500F1BD9800F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500EFB99200E0A17200D089
      5100C6793A00C2723100C5773700D18B5400EAB18800F0BB9500F0BB9500F0BB
      9500F0BB9600E9AF8400C1804B000000000000000000A6A6A600D3D3D300D4D4
      D400D5D5D500DBDBDB00BDBDBD00A8A8A800CBCBCB00DBDBDB00D5D5D500D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D2D2D200C4C4C400B5B5
      B500AAAAAA00A6A6A600A9A9A900B6B6B600CECECE00D4D4D400D4D4D400D4D4
      D400D4D4D400CCCCCC00AFAFAF000000000000000000311E1B00B5B1AF007D77
      74007D7774001B0F0F00F2F1F000EEECEA00EAE8E7002CCB3F0000CD190000CD
      190000CD190000CD190000CD190000CD190000CD190000CD190000CD190000CD
      190000CD19002CCB4000EAE8E600E8E5E300F1F0EF00110A0A007B7572007D77
      74008B8683002C2421000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F3F3F300F1F1F100BEBEBE00B4B4B400B4B4
      B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4
      B400B4B4B400BEBEBE00F1F1F100EFEFEF00F5F5F50070707000AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000C3743200EFBA9300F1BE
      9900F4C6A400D9966200CB905F0000000000CA834700DEA17000F5C9A800F2C1
      9D00F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F1BD9800DB996500BC997C000000000000000000A7A7A700D3D3D300D5D5
      D500DBDBDB00BDBDBD00B9B9B90000000000B0B0B000C3C3C300DCDCDC00D7D7
      D700D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D5D5D500BEBEBE00C1C1C1000000000000000000311E1B00B5B1AF007D77
      74007D7774001B100F00F2F0F000EDEAE900EDEBE900EFEDEC00F1F0EF00F4F3
      F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9
      F900F5F5F500F2F1F000EEECEB00EBE9E700F3F2F100110B0A007B7572007D77
      74008B8683002C2421000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200F2F2F200F3F3F300F5F5F500F7F7
      F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFA
      FA00F9F9F900F6F6F600F3F3F300F1F1F100F6F6F60071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000C6763200F0BB9600F4C6
      A500DA976100D4986600000000000000000000000000D69B6900D38B5100F1C2
      9F00F5C7A600F1BD9800F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F1BE
      9900EFBC9600CA7B3A00000000000000000000000000A8A8A800D4D4D400DBDB
      DB00BDBDBD00BEBEBE00000000000000000000000000BFBFBF00B5B5B500D8D8
      D800DBDBDB00D5D5D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500D4D4D400ACACAC00000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001C100F00F2F0F000ECEAE900ECE9E800EDEBE900EFEDEC00F1F0
      EF00F4F3F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFC
      FC00F8F9F900F5F5F500F2F1F000EFEDEC00F5F3F300120B0B007B7572007D77
      74008B8683002C2420000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200F2F2F200F2F2F200F3F3F300F5F5
      F500F7F7F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FAFAFA00F9F9F900F6F6F600F3F3F300F8F8F80071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000C8773300F2C19D00DB97
      6100D69A6700000000000000000000000000000000000000000000000000CB7E
      3D00E2A67600F6CBAC00F4C6A500F0BD9800F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F1BE9A00F4C6
      A400D8935A00DDAB8200000000000000000000000000A9A9A900D7D7D700BDBD
      BD00BFBFBF00000000000000000000000000000000000000000000000000ADAD
      AD00C6C6C600DDDDDD00DBDBDB00D5D5D500D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D6D6D600DBDB
      DB00BABABA00CACACA00000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001C100F00F2F0F000ECEAE900E7E4E3002CCB3F0000CD190000CD
      190000CD190000CD190000CD190000CD190000CD190000CD190000CD190000CD
      190000CD19002DCD4100F5F5F500F2F1F100F6F6F500120B0B007B7572007D77
      74008B8683002C2320000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200EFEFEF00BEBEBE00B4B4B400B4B4
      B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4
      B400B4B4B400BEBEBE00F9F9F900F6F6F600F9F9F90071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000CC7C3800DB975F00D89B
      6600000000000000000000000000000000000000000000000000000000000000
      0000DAA17100D0824000E6AB7D00F6CCAD00F6C9A900F3C2A000F0BC9700F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F1BE9900F4C6A500F5C8A700DA96
      5D00D89B670000000000000000000000000000000000ACACAC00BDBDBD00BFBF
      BF00000000000000000000000000000000000000000000000000000000000000
      0000C3C3C300B0B0B000C9C9C900DEDEDE00DCDCDC00D8D8D800D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5D500DBDBDB00DCDCDC00BCBC
      BC00BFBFBF0000000000000000000000000000000000311E1B00B2AEAC006760
      60007D7774001C100F00F2F0F000ECEAE900ECE9E800ECE9E800ECE9E800EDEB
      E900EFEDEC00F1F0EF00F4F3F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFE
      FE00FCFDFE00FAFCFC00F8F9F900F6F6F600F8F8F700130C0B007B7572007D77
      74008B8683002C2320000000000000000000000000007D7D7D00CFCFCF00A2A2
      A200AFAFAF0074747400F6F6F600F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F3F3F300F5F5F500F7F7F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFD
      FD00FDFDFD00FCFCFC00FAFAFA00F9F9F900FAFAFA0072727200AEAEAE00AFAF
      AF00B8B8B8007F7F7F0000000000000000000000000000000000DEAA7D000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000DBA27000CF803C00DF9D6700EDB89000F7CDAF00F8CF
      B200F7CCAD00F7CCAD00F7CCAD00F8CFB100F5CAA900E6AB7C00D1834000E0AE
      8300000000000000000000000000000000000000000000000000C9C9C9000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C4C4C400ADADAD00C0C0C000D2D2D200DFDFDF00E0E0
      E000DEDEDE00DEDEDE00DEDEDE00E0E0E000DDDDDD00C9C9C900B0B0B000CBCB
      CB000000000000000000000000000000000000000000311E1B00B0ACAA004B47
      47007D7774001D111000F2F0F000EDEAE900ECEAE900ECEAE900ECEAE900EDEA
      E900EEEBEA00F0EEEC00F2F1EF00F4F3F300F7F6F600F9F9FA00FBFBFC00FCFD
      FE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F9FAFA00140C0C007B7572007D77
      74008B8683002D2421000000000000000000000000007D7D7D00CECECE009494
      9400AFAFAF0075757500F6F6F600F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F3F3F300F4F4F400F6F6F600F8F8F800F9F9F900FBFBFB00FCFCFC00FDFD
      FD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00FBFBFB0072727200AEAEAE00AFAF
      AF00B8B8B8008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DB9E6A00CF7F3900D487
      4400D9925500DB965B00DA925600D4874500D0823D00DFAB7D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C1C1C100ADADAD00B2B2
      B200B9B9B900BCBCBC00B9B9B900B3B3B300AFAFAF00C9C9C900000000000000
      00000000000000000000000000000000000000000000311E1B00CCC9C800B5B2
      B000B5B2B0001E121100C3B5D000C1B2CE00C1B2CE00C1B2CE00C1B2CE00C1B2
      CE00C1B2CE00C1B2CE00C1B2CE00C1B2CF00C1B2CF00C2B2CF00C2B2CF00C2B2
      CF00C2B3CF00C2B3CF00C2B3CF00C2B2CF00C5B7D100140C0C00B2AFAD00B5B1
      AF008E8986002E2421000000000000000000000000007D7D7D00DFDFDF00D1D1
      D100D1D1D10076767600D9D9D900D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800DADADA0072727200D0D0D000D1D1
      D100B9B9B9008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000534442003D2C29003C2A
      27003C2A27001E111000B5A5C100BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAF
      CD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAF
      CD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00B4A5C100140B0A00382D2A00382E
      2B00362D2A00645D5B0000000000000000000000000092929200858585008484
      84008484840075757500D0D0D000D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D0D0D00071717100858585008585
      850085858500A0A0A00000000000000000000000000000000000000000000000
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
      0000000000006198B2002992C1000987C0000084C0000987C0002892C1005F98
      B200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C4C4C400C0C0C000BABABA00B8B8B800BABABA00C0C0C000C4C4
      C400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000499A
      BE001A97CA0071C8E600A2E3F500B7EFFD00B6EDFE00A7E5FB008CD5F10060BC
      E2001695CA004691B30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C4C4
      C400C2C2C200DEDEDE00EDEDED00F3F3F300F3F3F300EFEFEF00E6E6E600D8D8
      D800C2C2C200BFBFBF0000000000000000000000000000000000000000000000
      0000433A36002C201C001F131200190E0D00190E0D00190E0D00190E0D00190E
      0D00190E0D00190E0D00190E0E00190E0E00190E0E00190E0E00190E0E00190E
      0E00180E0E00180E0E00180E0E00180E0E00180E0E001E151400292421002924
      2100282320005855530000000000000000000000000000000000000000000000
      00008C8C8C007D7D7D0076767600737373007373730073737300737373007373
      7300737373007373730073737300737373007373730073737300737373007373
      73007373730073737300737373007373730073737300777777007F7F7F007F7F
      7F007E7E7E009B9B9B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005DB2D6003FAC
      D900B5EFFF00B5F4FF00A8EFFD0099E8FB0087DEF90075D5F40069CCF30070CD
      F40092D9F80036A5D500649CB600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D2D2D200CFCF
      CF00F4F4F400F6F6F600F3F3F300EFEFEF00EAEAEA00E5E5E500E1E1E100E2E2
      E200E9E9E900CBCBCB00C5C5C500000000000000000000000000000000003D33
      30003F353200524A4800180D0D00B9B7B600CEC6C400CEC6C400CEC6C400CEC6
      C400CEC6C400CEC6C400CEC6C400CEC6C400CEC6C400CEC6C400D1CBCA008A89
      890089898900888888008786860086858500828282001A0F0F00534B4900534B
      4900524A48002B26230000000000000000000000000000000000000000008787
      8700898989009696960073737300D4D4D400DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E0E0E000BABA
      BA00BABABA00B9B9B900B8B8B800B8B8B800B6B6B60074747400969696009696
      9600969696008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DEE0E1000887C0009CE2
      FD0097E8FE00B0F3FE00B1F1FD00A4EBFB0095E2FA0082D9F70071CFF30058C4
      F00059C2F0008AD3F300138CC100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EDEDED00BABABA00EEEE
      EE00EFEFEF00F5F5F500F4F4F400F1F1F100EDEDED00E8E8E800E3E3E300DCDC
      DC00DCDCDC00E6E6E600BDBDBD000000000000000000000000003E322F003F34
      3200433B390039302E00180D0D00CDC9C700DAC6BF00CEBCB600797776007876
      76007D7A7900DAC6BF00DAC6BF00DAC6BF00DAC6BF00DAC6BF00E6DAD6009696
      96009595950094949400919191008E8E8E0088888800190E0E0039302E003930
      2E00524A48002B26230000000000000000000000000000000000878787008989
      89008D8D8D008686860073737300DFDFDF00DDDDDD00D8D8D800AFAFAF00AFAF
      AF00B1B1B100DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E9E9E900C2C2
      C200C1C1C100C0C0C000BFBFBF00BDBDBD00B9B9B90073737300868686008686
      8600969696008080800000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008D7D7700855D54009859
      4A00A3564300A3523F00A25543009958480084594E00AFA7A8000088C200B1EB
      FF00A4EBFE0098E3F20084D7E90079D2E80066C7E50050BCE10039B0DE003AB1
      E3006CC9F200A3DFF9000687C100000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B3B3B300A1A1A100A0A0
      A0009E9E9E009C9C9C009E9E9E009F9F9F009F9F9F00CCCCCC00BABABA00F2F2
      F200F1F1F100EBEBEB00E4E4E400E2E2E200DCDCDC00D6D6D600D1D1D100D2D2
      D200E0E0E000ECECEC00BABABA000000000000000000453835003F353200433B
      390039302E0039302E00180D0C00CDC9C800DCC9C200D0BFBA00747474006F6F
      6F0072727100DCC9C200DCC9C200DCC9C200DCC9C200DCC9C200E7DCD8009B9B
      9B009A9A9A0098989800969696009191910089898900190E0D0039302E003930
      2E00524A48002B2623000000000000000000000000008B8B8B00898989008D8D
      8D00868686008686860072727200DFDFDF00DFDFDF00D9D9D900AEAEAE00ABAB
      AB00ACACAC00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00EAEAEA00C5C5
      C500C4C4C400C3C3C300C2C2C200BFBFBF00BABABA0073737300868686008686
      8600969696008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000938173005D64480050633E00A76E4D00CF7E
      5E00D5836200D8856200D6846200CF7E5D00C5755700CCA397000C90C60091D8
      F20073CFEB0095E8F70097EBFB0088E2F80074D8F5005CCBF10041BDED0027AC
      E5002DA6DC0086CCEC000487C100000000000000000000000000000000000000
      0000000000000000000000000000B4B4B4009F9F9F009C9C9C00A8A8A800B3B3
      B300B6B6B600B7B7B700B7B7B700B3B3B300AEAEAE00CACACA00BFBFBF00E7E7
      E700E1E1E100EEEEEE00F0F0F000EBEBEB00E6E6E600DFDFDF00D8D8D800CFCF
      CF00CCCCCC00E2E2E200BABABA0000000000000000002E1F1C007F7978003930
      2E0039302E0039302E00180D0C00CDC9C800E1D1CB00D6C8C3007E7E7E007777
      770076757500E1D1CB00E1D1CB00E1D1CB00E1D1CB00E1D1CB00EAE1DE009898
      98009898980096969600939393008E8E8E0085858500190E0D0039302E003930
      2E00524A48002B2623000000000000000000000000007D7D7D00B1B1B1008686
      8600868686008686860072727200DFDFDF00E4E4E400DFDFDF00B4B4B400B0B0
      B000AEAEAE00E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400EDEDED00C3C3
      C300C3C3C300C2C2C200C0C0C000BDBDBD00B8B8B80073737300868686008686
      8600969696008080800000000000000000000000000000000000000000000000
      00000000000000000000A967570023683D0026A97B0030A4760063825600B07E
      5500D5825B00D8825C00D9835C00D9845D00DB875F00E3B8A4000087BD0087D7
      F40083E2FC009CEFFD0097EBFB0088E2F80074D8F5005CCBF10041BDED0029B1
      EA0035B3EB0079C7EC000484BC00000000000000000000000000000000000000
      00000000000000000000A8A8A8009B9B9B00BCBCBC00BABABA00ACACAC00B0B0
      B000B4B4B400B5B5B500B5B5B500B6B6B600B7B7B700D4D4D400B9B9B900E7E7
      E700EBEBEB00F2F2F200F0F0F000EBEBEB00E6E6E600DFDFDF00D8D8D800D2D2
      D200D4D4D400DFDFDF00B8B8B80000000000000000002E1F1C008E8987003930
      2E0039302E0039302E00170D0C00CAC7C600E7DBD600DDD2CE00898989008282
      82007F7F7F00E7DBD600E7DBD600E7DBD600E7DBD600E7DBD600EDE6E4008B8B
      8B008A8A8A0089898900868686008181810079797900180E0D0039302E003930
      2E00524A48002B2623000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860072727200DEDEDE00E9E9E900E4E4E400BABABA00B6B6
      B600B4B4B400E9E9E900E9E9E900E9E9E900E9E9E900E9E9E900F0F0F000BBBB
      BB00BBBBBB00BABABA00B8B8B800B5B5B500B1B1B10073737300868686008686
      8600969696008080800000000000000000000000000000000000000000000000
      0000CDA59C00B56A5300CD7E5E000C7643002EC69E0031C59D002DAC8000587C
      4D00BF7B5200D6815800D6805800D6805800D67E5700DFB39E000087C00091E0
      FD0092E7FD00BDF7FF00BBF3FE00B3EFFD00A6E9FB0097E0F90085D8F70072CE
      F3004FBDEE0081CFF3000485BE00000000000000000000000000000000000000
      0000CBCBCB00A8A8A800B3B3B3009F9F9F00CCCCCC00CCCCCC00BEBEBE00A8A8
      A800AFAFAF00B4B4B400B3B3B300B3B3B300B3B3B300D1D1D100B9B9B900ECEC
      EC00EEEEEE00F7F7F700F5F5F500F3F3F300F0F0F000ECECEC00E7E7E700E2E2
      E200D9D9D900E3E3E300B9B9B90000000000000000002E1F1C008E8987003930
      2E0039302E00382F2D00170D0C00C6C4C300EDE4E000E3DAD700929292008D8D
      8D008A8A8A00EDE4E000EDE4E000EDE4E000EDE4E000EDE4E000F0EBE9007B7B
      7B007A7A7A007979790076767600737373006B6B6B00180E0D0039302E003930
      2E00524A48002B2622000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860072727200DCDCDC00EEEEEE00E9E9E900BFBFBF00BCBC
      BC00BBBBBB00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00F2F2F200B2B2
      B200B1B1B100B1B1B100AFAFAF00ADADAD00A9A9A90073737300868686008686
      860096969600808080000000000000000000000000000000000000000000CFA5
      9B00BA6C5300D4856200DA875F003E6C3D001BAD84002CC9A20036D5AA0039C0
      910051855300C0875C00DE896100D6845D00D5825B00DEB39E00048FC500C0F0
      FF00A0E8FB008EDEEE0083D6E80075CEE60065C6E3004EBADF0038AEDC0029A8
      DE0068C6EF00B6E7FC000489C20000000000000000000000000000000000CBCB
      CB00A9A9A900B7B7B700B7B7B7009E9E9E00BEBEBE00CECECE00D4D4D400C9C9
      C900ACACAC00B5B5B500B8B8B800B5B5B500B4B4B400D1D1D100BEBEBE00F5F5
      F500EFEFEF00E8E8E800E4E4E400E0E0E000DCDCDC00D5D5D500D0D0D000CDCD
      CD00DEDEDE00F1F1F100BBBBBB0000000000000000002F1F1C008E8987003930
      2E0039302E00382F2D00180E0D00C4C2C200F0E9E600E6DFDD00959595009494
      940094949400F0E9E600F0E9E600F0E9E600F0E9E600F0E9E600F1EDEC006F6F
      6F006F6F6F006D6D6D006B6B6B006868680062626200180E0D0039302E003930
      2E00524A48002B2522000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860073737300DBDBDB00F1F1F100ECECEC00C1C1C100C0C0
      C000C0C0C000F1F1F100F1F1F100F1F1F100F1F1F100F1F1F100F4F4F400ABAB
      AB00ABABAB00AAAAAA00A9A9A900A7A7A700A3A3A30073737300868686008686
      8600969696007F7F7F000000000000000000000000000000000000000000B767
      5000D5846200DA845E00DE845900B6895D0008A2700032E0B5002DDFB20040EC
      BF0042DEAD0034905900F09C6F00F49B6F00E7916800E0B7A2000E94C80080CC
      E90073D0EB0095E9F70097EBFB0088E2F80074D8F5005CCBF10041BDED0027AE
      E7002DA6DC0073C0E200048BC20000000000000000000000000000000000A8A8
      A800B7B7B700B6B6B600B5B5B500B5B5B500B5B5B500D9D9D900D8D8D800E0E0
      E000D8D8D800AEAEAE00C3C3C300C2C2C200BDBDBD00D4D4D400C0C0C000E1E1
      E100E2E2E200EEEEEE00F0F0F000EBEBEB00E6E6E600DFDFDF00D8D8D800D0D0
      D000CCCCCC00DBDBDB00BCBCBC0000000000000000002F1F1C008E8987003930
      2E0039302E00382F2D00180E0D00C4C2C200F1EAE700E6E0DD00969696009696
      960099999900F1EAE700F1EAE700F1EAE700F1EAE700F1EAE700F1EDEC006B6B
      6B006B6B6B006A6A6A00686868006565650061616100180E0D0039302E003930
      2E00524A48002B2522000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860073737300DBDBDB00F2F2F200ECECEC00C2C2C200C2C2
      C200C3C3C300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F4F4F400A9A9
      A900A9A9A900A8A8A800A7A7A700A5A5A500A3A3A30073737300868686008686
      8600969696007F7F7F0000000000000000000000000000000000B56A5400D281
      5F00E28D6300F0906400F79A6C0082915D0021BD88003CECBF0029DEB00029DE
      B0003AEBBD003FE5B50063885200F9A17400FDA27500F2C4AC000086BC008FDE
      FB0083E2FC009CEFFD0097EBFB0088E2F80074D8F5005CCBF10041BDED0029B1
      EA0035B3EB007FCEF2000386BB00000000000000000000000000A9A9A900B5B5
      B500BBBBBB00BCBCBC00C2C2C200B4B4B400C5C5C500E0E0E000D7D7D700D7D7
      D700DFDFDF00DCDCDC00ADADAD00C6C6C600C6C6C600DBDBDB00B8B8B800EBEB
      EB00EBEBEB00F2F2F200F0F0F000EBEBEB00E6E6E600DFDFDF00D8D8D800D2D2
      D200D4D4D400E3E3E300B8B8B80000000000000000002F1F1B008E8988003A30
      2E003A302E0039302E00180E0D00A8A7A700CBC9C800CBC9C800CBC9C800CCC9
      C900CCC9C900CCC9C900CCC9C900CCC9C900CCC9C900CCC9C900CAC8C8006767
      67006666660065656500646464006464640061616100180E0D003A302E003A31
      2F00524B49002B2522000000000000000000000000007D7D7D00BABABA008686
      8600868686008686860073737300CBCBCB00DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00A6A6
      A600A6A6A600A5A5A500A5A5A500A5A5A500A3A3A30073737300868686008787
      8700969696007F7F7F00000000000000000000000000C9978800C5725300ED96
      6C00F9976A00FE9B6F00BD93620017AF780046F2C50030E2B50029DEB00030E0
      B4003FE6BF0056F7CE0029A26C00DC9B6C00FDA27500F4C5AE00008EC50093E1
      FE0081E1FC008CE6F5007EDCEF006FD4EC005DCAEA004AC1E70035B6E50025AE
      E60035B3EB0083D2F500038CC3000000000000000000C3C3C300ADADAD00C0C0
      C000C0C0C000C3C3C300BABABA00BCBCBC00E3E3E300DADADA00D7D7D700D9D9
      D900DEDEDE00E8E8E800B7B7B700C0C0C000C6C6C600DCDCDC00BDBDBD00EDED
      ED00EBEBEB00EDEDED00E7E7E700E3E3E300DEDEDE00D9D9D900D4D4D400D0D0
      D000D4D4D400E5E5E500BBBBBB0000000000000000002F1E1B00918B8A003E35
      33003E3533003E35330022181700170D0C00170D0C00170D0C00170D0C00170D
      0C00170D0C00170D0C00170D0C00170D0C00170D0C00170D0C00170D0C00170D
      0C00170D0C00170D0C00170D0C00170D0C00170D0C00241A19003E3533003E35
      3300564E4C002B2522000000000000000000000000007D7D7D00BBBBBB008989
      8900898989008989890079797900727272007272720072727200727272007272
      7200727272007272720072727200727272007272720072727200727272007272
      720072727200727272007272720072727200727272007A7A7A00898989008989
      8900989898007F7F7F00000000000000000000000000B96B5200EA936C00FE9E
      7200FE9B6E00FDA071006A87510033DCAC0036E6BA0029DEB0002BDEB20040E5
      BD005CE1C30064D1AF005BA47500D39B6B00FDA27600F8C6AE00008DC40080D6
      F00094E3F300B4F2FC00C8FCFF00CFFFFF00D3FFFF00CDFEFF00C0F8FF00A2E9
      F9007FD6EF0077CCEB00048CC3000000000000000000A9A9A900BFBFBF00C5C5
      C500C3C3C300C5C5C500ADADAD00D6D6D600DCDCDC00D7D7D700D8D8D800DDDD
      DD00DFDFDF00D6D6D600BCBCBC00C0C0C000C7C7C700DCDCDC00BCBCBC00E5E5
      E500EBEBEB00F4F4F400F9F9F900FBFBFB00FBFBFB00FAFAFA00F8F8F800EFEF
      EF00E5E5E500E0E0E000BCBCBC0000000000000000002F1E1B00948F8D00433A
      3800433A3800433A3800433A3800433A3800433A3800433A3800433A3800433A
      3800433A3800433A3800433A3800433A3800433A3800433A3800433A3800433A
      3800433A3800433A3800433A3800433A3800433A3800433A3800433A3800443B
      39005A5351002B2522000000000000000000000000007D7D7D00BDBDBD008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008D8D
      8D009B9B9B007F7F7F00000000000000000000000000C5705000FFA47A00FFA0
      7100FEA27400FDA677007A8F5B0025CC9C0039E5BB0039E4BC0049EAC60033D1
      A70061AA7C009C9D6C00CD9E6E00F8A77800FDA77A00EEC7AD00028CC100C7FB
      FF00D3FFFF00D3FFFF00D3FFFF00D3FFFF00D3FFFF00D3FFFF00D3FFFF00D3FF
      FF00D3FFFF00C4F9FF00098FC4000000000000000000ACACAC00C8C8C800C5C5
      C500C6C6C600C8C8C800B3B3B300CDCDCD00DCDCDC00DCDCDC00E1E1E100D2D2
      D200C0C0C000BDBDBD00C1C1C100C9C9C900C9C9C900DCDCDC00BCBCBC00F9F9
      F900FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00F8F8F800BDBDBD0000000000000000002F1E1B00979391004941
      3E0049413E0049413E0049413E0049413E0049413E0049413E0049413E004941
      3E0049413E0049413E0049413E0049413E0049413E0049413E0049413E004941
      3E0049413E0049413E0049413E0049413E0049413E0049413E0049413E004A41
      3F00605956002B2522000000000000000000000000007D7D7D00BFBFBF009090
      9000909090009090900090909000909090009090900090909000909090009090
      9000909090009090900090909000909090009090900090909000909090009090
      9000909090009090900090909000909090009090900090909000909090009090
      90009E9E9E007F7F7F000000000000000000CA8F7A00DA845F00FFA97B00FFA4
      7500FDA97B00FBAD7E009097650027C1940069F7DA006FF4DB0052D8B6005DAD
      7E00CCA97B00FAB08300FDAF8200FDAE8100FBAE8000D2D0B7001DA3B30026A4
      D2007DD3EC00A4E8F800C6FAFF00CDFEFF00D2FFFF00CDFDFF00C4F9FF00A2E7
      F7007AD0EB0023A2D00060B6D70000000000BDBDBD00B6B6B600CACACA00C7C7
      C700CACACA00CBCBCB00B9B9B900C8C8C800EBEBEB00EBEBEB00D9D9D900C1C1
      C100C7C7C700CDCDCD00CDCDCD00CDCDCD00CDCDCD00DFDFDF00C3C3C300C9C9
      C900E3E3E300EFEFEF00F9F9F900FAFAFA00FBFBFB00FAFAFA00F8F8F800EEEE
      EE00E2E2E200C8C8C800D4D4D40000000000000000002F1E1B009B9695005048
      4600504846001C131200140B0B00130B0A00130A0A00120A0900110A09001109
      0900100908000F0908000F0808000E0808000E0807000D0707000D0707000C07
      07000C0606000C0606000B0606000B0606000B060600100B0A00504745005148
      4600665E5C002B2522000000000000000000000000007D7D7D00C2C2C2009494
      9400949494007676760072727200717171007171710070707000707070007070
      700070707000707070006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F
      6F006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E0071717100949494009494
      9400A1A1A1007F7F7F000000000000000000BD715600EB946B00FFAA7C00FEAA
      7C00D8A97800769C680040BA890026AC7D0046B085004FAD810087AF8200D4B5
      8800F9BA8E00FDBA8F00FDB98B00FDB88900F9B5880085B0800071E2C30037D2
      C5001AB1BE000DA2C2000493C000028EC100008BC300018DC0000C88B70037A2
      CA006DBBD800000000000000000000000000ACACAC00BFBFBF00CACACA00CACA
      CA00C7C7C700B9B9B900C6C6C600BDBDBD00C2C2C200C1C1C100C5C5C500CECE
      CE00D3D3D300D3D3D300D2D2D200D1D1D100D0D0D000C5C5C500E0E0E000D8D8
      D800C9C9C900C4C4C400BEBEBE00BCBCBC00BBBBBB00BCBCBC00B9B9B900C8C8
      C800D6D6D600000000000000000000000000000000002F1E1B009F9B99005850
      4D0058504D00170E0D00F5F5F500FCFDFD00FDFDFE00FCFDFD00FCFCFD00FBFB
      FB00FAFAFA00F8F8F800F6F6F500F5F4F300F3F2F100F2F0EF00F1EFEE00F0EE
      EC00F0EEEC00F0EEEC00F0EEEC00F0EEEC00F1EFEE000E090900564E4C005850
      4E006C6563002B2522000000000000000000000000007D7D7D00C4C4C4009999
      99009999990073737300F9F9F900FDFDFD00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FCFCFC00FAFAFA00F9F9F900F8F8F800F6F6F600F5F5F500F5F5F500F4F4
      F400F4F4F400F4F4F400F4F4F400F4F4F400F5F5F50070707000989898009999
      9900A5A5A5007F7F7F000000000000000000B9644200F49D7400FDAD7D0096A3
      700032B07B0043D8AD0024AE8000B0B38300E6C19500E7C19700F4C59C00FFC7
      A000FFC79E00FFC59C00FEC29700FEC09500FDBF930095A9780040C89E00A7F6
      E60089F0DA0087EFD8008DEFDA00A0ECDF00A5E8DD008AE0CA00AAB29D000000
      000000000000000000000000000000000000A4A4A400C4C4C400CBCBCB00BFBF
      BF00BFBFBF00D6D6D600BEBEBE00CACACA00D5D5D500D5D5D500D9D9D900DBDB
      DB00DBDBDB00D9D9D900D7D7D700D6D6D600D5D5D500C3C3C300CECECE00F1F1
      F100EBEBEB00EAEAEA00EBEBEB00ECECEC00EBEBEB00E3E3E300CDCDCD000000
      000000000000000000000000000000000000000000002F1E1B00A39F9E005F58
      55005F585500180E0E00F4F4F400FBFBFC00FCFDFE00FDFEFE00FCFDFE00FBFC
      FC00F8F9F900F6F6F600F2F1F100EFEDEC00EBE9E700E8E5E300E5E1DF00E3DE
      DC00E1DCDA00E1DCD900E1DCD900E2DDDB00F0EDEC000E0909005E5653005F58
      5500726C69002C2522000000000000000000000000007D7D7D00C7C7C7009D9D
      9D009D9D9D0073737300F8F8F800FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FAFAFA00F9F9F900F6F6F600F3F3F300F1F1F100EFEFEF00EDEDED00EBEB
      EB00EAEAEA00EAEAEA00EAEAEA00EAEAEA00F4F4F400707070009C9C9C009D9D
      9D00A9A9A9007F7F7F000000000000000000BA654000D59E70005F9A640024BB
      880043E7BF001FAD7E0085AC7B0070A97800CCC19900FFD5B200FFD6B400FFD8
      B400FFD6B300FFD4AF00FFD1AA00FFCDA600FFCBA100F1C397006FA97A003DCE
      A90069F4DB0069F7DE005DEED30037B8930019875D001D976F007A734D000000
      000000000000000000000000000000000000A4A4A400C2C2C200B7B7B700C4C4
      C400DFDFDF00BDBDBD00C3C3C300C0C0C000D4D4D400E3E3E300E3E3E300E4E4
      E400E3E3E300E2E2E200E0E0E000DEDEDE00DCDCDC00D7D7D700C0C0C000D2D2
      D200EAEAEA00ECECEC00E6E6E600C6C6C600AAAAAA00B3B3B300A7A7A7000000
      000000000000000000000000000000000000000000002F1F1C00A8A3A200665F
      5C00665F5C00190F0E00F3F3F300F9F9FA00FBFBFC00FCFDFE00FDFEFE00FCFD
      FE00FAFCFC00F8F9F900F5F5F500F2F1F000EEECEB00EAE8E600E7E4E200E4E0
      DD00E1DCDA00E0DBD800E0DBD800E1DCD900F0EDEC000E090900655D5B00675F
      5D00787270002C2521000000000000000000000000007D7D7D00C9C9C900A1A1
      A100A1A1A10073737300F8F8F800FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFD
      FD00FCFCFC00FAFAFA00F9F9F900F6F6F600F3F3F300F1F1F100EEEEEE00ECEC
      EC00EAEAEA00E9E9E900E9E9E900EAEAEA00F4F4F40070707000A1A1A100A2A2
      A200ADADAD007F7F7F000000000000000000AE6843005D99680013B9850037E0
      B00044EEC1002BC08E0020AF7B002DC999008FBA8E00FFE1C100FFE4C500FFE4
      C600FFE4C500FFE0C100FFDCBB00FFD9B400FFD5AE00FFD2A800DFC3960038AE
      7E003ECDA8004ACFAF0044C5A00025905E004A88590049835300A06F4B000000
      000000000000000000000000000000000000A5A5A500B7B7B700C2C2C200D9D9
      D900E2E2E200C7C7C700BEBEBE00CCCCCC00CBCBCB00E9E9E900EBEBEB00EBEB
      EB00EBEBEB00E9E9E900E7E7E700E4E4E400E2E2E200E0E0E000D5D5D500BFBF
      BF00D1D1D100D4D4D400CECECE00AEAEAE00ADADAD00AAAAAA00A8A8A8000000
      00000000000000000000000000000000000000000000301F1B00ACA8A6006D66
      63006D6663001A0F0E00F2F2F100F7F6F600F9F9F900FBFBFC00FCFDFE00FDFE
      FE00FCFDFE00FAFCFC00F8F9F900F5F5F500F2F1F000EEECEB00EAE8E600E7E4
      E200E4E0DD00E1DCDA00E0DBD800E1DCD900F0EDEC000F0909006C6562006D67
      64007E7876002C2421000000000000000000000000007D7D7D00CBCBCB00A5A5
      A500A5A5A50073737300F6F6F600F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFD
      FD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6F600F3F3F300F1F1F100EEEE
      EE00ECECEC00EAEAEA00E9E9E900EAEAEA00F4F4F40070707000A5A5A500A6A6
      A600B0B0B0007F7F7F000000000000000000A97E5A000CA977003DE6BA0049F4
      CC004BF7D3004DF9D3004DF8D10049F1C3004ABF8F00CED5B300FFE8CD00FFED
      D400FFEDD300FFEACF00FFE6C900FFE1C100FFDCBA00FFD8B200E0C89E0042BD
      8F0036D5AD000EB07B001DA871000DB27E0018B084000B9A6A008D7A5A000000
      000000000000000000000000000000000000B0B0B000B9B9B900DDDDDD00E6E6
      E600E8E8E800E9E9E900E8E8E800E3E3E300C9C9C900E0E0E000EEEEEE00F1F1
      F100F1F1F100EFEFEF00EDEDED00E9E9E900E6E6E600E4E4E400D9D9D900C8C8
      C800D4D4D400BCBCBC00B9B9B900BEBEBE00BFBFBF00B2B2B200ADADAD000000
      00000000000000000000000000000000000000000000301E1B00AFABAA00736D
      6A00736D6A001A100F00F1F0F000F4F3F300F1F0F000C9C9C900C9CACA00CACA
      CA00CACACB00CACACB00C9CACA00C8C9C900C7C7C700C6C6C600C5C4C400C4C2
      C200C2C1C000C3C1C000E1DCDA00E1DCDA00F0EDEC000F0A0900716B6800736D
      6A00837D7B002C2421000000000000000000000000007D7D7D00CECECE00A9A9
      A900A9A9A90074747400F6F6F600F8F8F800F6F6F600DFDFDF00DFDFDF00E0E0
      E000E0E0E000E0E0E000DFDFDF00DFDFDF00DEDEDE00DDDDDD00DCDCDC00DBDB
      DB00DADADA00DADADA00EAEAEA00EAEAEA00F4F4F40070707000A8A8A800A9A9
      A900B3B3B3007F7F7F000000000000000000BB9F820031A677004CF7D3004BF7
      D80046F4D80046F4D80046F4D6004BF7D30040E6B90050CDA00095CEA900EAE6
      CC00FFF4DE00FFF3DB00FFEED500FFE8CD00FFE2C300FFDCBA00FFD5AF0090BF
      940039CBA40058F4D1003FE2B90055F3CC0044DBB900329B70009F9279000000
      000000000000000000000000000000000000C4C4C400BBBBBB00E8E8E800E9E9
      E900E7E7E700E7E7E700E7E7E700E8E8E800DDDDDD00D1D1D100D8D8D800EBEB
      EB00F5F5F500F4F4F400F1F1F100EEEEEE00EAEAEA00E6E6E600E2E2E200CECE
      CE00D0D0D000E7E7E700DCDCDC00E6E6E600D9D9D900B6B6B600BCBCBC000000
      00000000000000000000000000000000000000000000301E1B00B2AEAD007872
      6F0078726F001A100E00F2F1F000F2F1EF00F4F3F200F6F6F600F9F9F900FBFB
      FC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F5F5F500F2F1F000EEEC
      EB00EAE8E600E7E4E200E4E0DD00E3DEDC00F0EDEC00100A0A0076706D007872
      6F0087827F002C2421000000000000000000000000007D7D7D00D0D0D000ACAC
      AC00ACACAC0074747400F6F6F600F6F6F600F7F7F700F9F9F900FBFBFB00FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6F600F3F3
      F300F1F1F100EEEEEE00ECECEC00EBEBEB00F4F4F40070707000ABABAB00ACAC
      AC00B5B5B5007F7F7F000000000000000000000000005E8E61003CE6C5004DF8
      DE004AF8DC004AF8DC004AF8DC0055F9E0005BFDDE004AF2C90030CC9D004FC3
      9700B3DABC00FFF5E000FFF4DF00FFEDD600FFE7CC00FFE1C200FFDBB900F8D2
      AC002AAE7D0034D4AE0055EDCF0070F9E1003ED1B2005C8A5F00000000000000
      00000000000000000000000000000000000000000000B2B2B200DFDFDF00EAEA
      EA00EAEAEA00EAEAEA00EAEAEA00EBEBEB00EDEDED00E4E4E400CECECE00CCCC
      CC00E1E1E100F5F5F500F5F5F500F1F1F100EDEDED00EAEAEA00E6E6E600E0E0
      E000BEBEBE00D4D4D400E4E4E400EEEEEE00D4D4D400B0B0B000000000000000
      00000000000000000000000000000000000000000000311E1B00B4B1AF007C76
      73007C7673001A0F0E00F3F1F100F0EEEC00F1F0EF00F4F3F200F6F6F600F9F9
      F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F5F5F500F2F1
      F000EEECEB00EAE8E600E7E4E200E5E1DF00F0EEED00100A0A007A7471007C76
      73008A8583002C2421000000000000000000000000007D7D7D00D1D1D100AEAE
      AE00AEAEAE0073737300F6F6F600F4F4F400F5F5F500F7F7F700F9F9F900FBFB
      FB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6
      F600F3F3F300F1F1F100EEEEEE00EDEDED00F4F4F40070707000ADADAD00AEAE
      AE00B7B7B7007F7F7F00000000000000000000000000958860002EC7A10052FA
      E1004FFAE1004DF9E10062FEE80077FFEE0071FDE8005EFEE1004DF5CF0030D5
      AD007ACDAA00FFF5E200FFF8E400FFF2DC00FFEBD200FFE4C700FFDEBD00ECD1
      AC002DBB8E0013B385000EAC7C0052BB9300639E710098865F00000000000000
      00000000000000000000000000000000000000000000B4B4B400CDCDCD00ECEC
      EC00ECECEC00EBEBEB00EFEFEF00F2F2F200F0F0F000EEEEEE00E7E7E700D4D4
      D400D6D6D600F6F6F600F7F7F700F3F3F300EFEFEF00EBEBEB00E7E7E700DFDF
      DF00C6C6C600C0C0C000BBBBBB00C9C9C900BABABA00B3B3B300000000000000
      00000000000000000000000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001B0F0F00F2F1F000EEECEA00EAE8E700C6C6C500C7C7C700C8C8
      C800C9C9C900C9CACA00CACACA00CACACB00CACACB00C9CACA00C8C9C900C7C7
      C700C6C6C600C7C6C600EAE8E600E8E5E300F1F0EF00110A0A007B7572007D77
      74008B8683002C2421000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F3F3F300F1F1F100DDDDDD00DEDEDE00DFDF
      DF00DFDFDF00DFDFDF00E0E0E000E0E0E000E0E0E000DFDFDF00DFDFDF00DEDE
      DE00DDDDDD00DDDDDD00F1F1F100EFEFEF00F5F5F50070707000AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000C4B29800559E750049ED
      D40057FDE70069FFED0066FAE50025BA90001EC2990075FFED0063F4DC005DD1
      AE00DFE8D100FFF7E400FFF5E100FFF2DE00FFECD400FFE5CB00FFDFC000E1CE
      AA0019AC7C0063AA7A00E6BA9000EEB28700D5895C00D0A98E00000000000000
      00000000000000000000000000000000000000000000CECECE00BABABA00E4E4
      E400EEEEEE00F1F1F100EEEEEE00C5C5C500C9C9C900F2F2F200EAEAEA00D6D6
      D600EDEDED00F7F7F700F6F6F600F4F4F400F0F0F000EDEDED00E8E8E800DDDD
      DD00BCBCBC00C0C0C000D2D2D200CECECE00B7B7B700CACACA00000000000000
      00000000000000000000000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001B100F00F2F0F000EDEAE900EDEBE900EFEDEC00F1F0EF00F4F3
      F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9
      F900F5F5F500F2F1F000EEECEB00EBE9E700F3F2F100110B0A007B7572007D77
      74008B8683002C2421000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200F2F2F200F3F3F300F5F5F500F7F7
      F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFA
      FA00F9F9F900F6F6F600F3F3F300F1F1F100F6F6F60071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000000000009B946B003DBF
      99005CF9E20056F5DE002EB38700C7CBA30017BC910033D6B3000DB28300A6D5
      B500C5DEC00030C3940030BF8F00EAE6CB00FFE7CD00FFE4C700FFDCBD00FFD5
      B300FFCDA800FFC59D00FFBC9400F59D7100D68D650000000000000000000000
      0000000000000000000000000000000000000000000000000000BABABA00CACA
      CA00EDEDED00EAEAEA00C2C2C200D9D9D900C5C5C500D5D5D500BFBFBF00DDDD
      DD00E4E4E400CACACA00C7C7C700EBEBEB00EDEDED00EBEBEB00E7E7E700E3E3
      E300DEDEDE00D9D9D900D5D5D500C3C3C300BABABA0000000000000000000000
      00000000000000000000000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001C100F00F2F0F000ECEAE900ECE9E800EDEBE900EFEDEC00F1F0
      EF00F4F3F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFC
      FC00F8F9F900F5F5F500F2F1F000EFEDEC00F5F3F300120B0B007B7572007D77
      74008B8683002C2420000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200F2F2F200F2F2F200F3F3F300F5F5
      F500F7F7F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FAFAFA00F9F9F900F6F6F600F3F3F300F8F8F80071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F0000000000000000000000000000000000000000007997
      6A004CD2B00050F1CE005CC19300B6C39B001EA978001DCB9E0025CEA10074C6
      A0003CB98D004BF2C70044EEC00039C7970090CFA800D9D6B400FFD5B400FFCF
      AC00FFC8A200FFC29B00FEAC8100DD8B5F000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B8B8
      B800D5D5D500E6E6E600CBCBCB00D4D4D400BBBBBB00CDCDCD00CFCFCF00D1D1
      D100C6C6C600E4E4E400E1E1E100CCCCCC00D8D8D800E1E1E100E3E3E300E0E0
      E000DBDBDB00D8D8D800CCCCCC00B9B9B9000000000000000000000000000000
      00000000000000000000000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001C100F00F2F0F000ECEAE900E7E4E300C4C3C300C5C4C300C5C5
      C400C6C6C500C7C7C700C8C8C800C9C9C900C9CACA00CACACA00CACACB00CACA
      CB00C9CACA00CACBCB00F5F5F500F2F1F100F6F6F500120B0B007B7572007D77
      74008B8683002C2320000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200EFEFEF00DCDCDC00DCDCDC00DCDC
      DC00DDDDDD00DEDEDE00DFDFDF00DFDFDF00DFDFDF00E0E0E000E0E0E000E0E0
      E000DFDFDF00E0E0E000F9F9F900F6F6F600F9F9F90071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F000000000000000000000000000000000000000000D9CA
      B400799B6F004FD2AD002ED5AD0019B5840013AD7C003CE4C0003ED9B4005CBC
      910021B587004AF2CF0053FEDB004DF9D10031E0B20040C39400C9C59E00FFCB
      A700FFC8A200FFB08800E18F6300E9BEA7000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DDDD
      DD00BABABA00D5D5D500D4D4D400C0C0C000BCBCBC00DDDDDD00D8D8D800CACA
      CA00C2C2C200E6E6E600EDEDED00E8E8E800D9D9D900CBCBCB00D6D6D600DDDD
      DD00DBDBDB00CFCFCF00BBBBBB00D8D8D8000000000000000000000000000000
      00000000000000000000000000000000000000000000311E1B00B2AEAC006760
      60007D7774001C100F00F2F0F000ECEAE900ECE9E800ECE9E800ECE9E800EDEB
      E900EFEDEC00F1F0EF00F4F3F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFE
      FE00FCFDFE00FAFCFC00F8F9F900F6F6F600F8F8F700130C0B007B7572007D77
      74008B8683002C2320000000000000000000000000007D7D7D00CFCFCF00A2A2
      A200AFAFAF0074747400F6F6F600F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F3F3F300F5F5F500F7F7F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFD
      FD00FDFDFD00FCFCFC00FAFAFA00F9F9F900FAFAFA0072727200AEAEAE00AFAF
      AF00B8B8B8007F7F7F0000000000000000000000000000000000000000000000
      0000DECAB40085996C0056A97E0017C0950011A775003DBF950088C39D00ADBF
      970046CC9D004DF7CE0052FDE50053FDE40058FEDF0040E5BA0089C59900FAC6
      A000FBAF8500E08F6200E7B79B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DDDDDD00B9B9B900BFBFBF00C7C7C700B8B8B800C9C9C900D1D1D100D1D1
      D100D0D0D000E7E7E700EEEEEE00EEEEEE00EDEDED00DDDDDD00D1D1D100DADA
      DA00CECECE00BBBBBB00D3D3D300000000000000000000000000000000000000
      00000000000000000000000000000000000000000000311E1B00B0ACAA004B47
      47007D7774001D111000F2F0F000EDEAE900ECEAE900ECEAE900ECEAE900EDEA
      E900EEEBEA00F0EEEC00F2F1EF00F4F3F300F7F6F600F9F9FA00FBFBFC00FCFD
      FE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F9FAFA00140C0C007B7572007D77
      74008B8683002D2421000000000000000000000000007D7D7D00CECECE009494
      9400AFAFAF0075757500F6F6F600F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F3F3F300F4F4F400F6F6F600F8F8F800F9F9F900FBFBFB00FCFCFC00FDFD
      FD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00FBFBFB0072727200AEAEAE00AFAF
      AF00B8B8B8008080800000000000000000000000000000000000000000000000
      00000000000000000000B6956B00499764003FA16E00A4BB90006FBB8E004BC0
      91004FE8C00069FFE50075FFF2007CFFF4007AFFEE004FE8C90056B68900C99A
      6A00E0966D000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00B4B4B400B8B8B800CECECE00CACACA00CACA
      CA00E0E0E000F0F0F000F3F3F300F3F3F300F2F2F200E2E2E200C6C6C600BFBF
      BF00BFBFBF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000311E1B00CCC9C800B5B2
      B000B5B2B0001E121100C3B5D000C1B2CE00C1B2CE00C1B2CE00C1B2CE00C1B2
      CE00C1B2CE00C1B2CE00C1B2CE00C1B2CF00C1B2CF00C2B2CF00C2B2CF00C2B2
      CF00C2B3CF00C2B3CF00C2B3CF00C2B2CF00C5B7D100140C0C00B2AFAD00B5B1
      AF008E8986002E2421000000000000000000000000007D7D7D00DFDFDF00D1D1
      D100D1D1D10076767600D9D9D900D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800DADADA0072727200D0D0D000D1D1
      D100B9B9B9008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000CDB99B00BD906200C18F5D004DA2700025C6
      9D005DEED40080FFED006BF4DE0044D5B50045B58D00789D6F00AF956900DCB9
      9D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D2D2D200B9B9B900B8B8B800BABABA00CBCB
      CB00E6E6E600F3F3F300EBEBEB00D6D6D600C5C5C500BBBBBB00BBBBBB00D4D4
      D400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000534442003D2C29003C2A
      27003C2A27001E111000B5A5C100BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAF
      CD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAF
      CD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00B4A5C100140B0A00382D2A00382E
      2B00362D2A00645D5B0000000000000000000000000092929200858585008484
      84008484840075757500D0D0D000D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D0D0D00071717100858585008585
      850085858500A0A0A00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4B3930097A1
      740072996600679666007498680095A17700BCB4940000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CECECE00BFBF
      BF00B7B7B700B5B5B500B8B8B800C0C0C000CECECE0000000000000000000000
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
      0000000000000000000000000000000000000000000000000000A5B2BB003365
      8D0043647F0000000000000000000000000000000000000000007B94A7002561
      8E0038678B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D3D3D300A8A8
      A800A7A7A7000000000000000000000000000000000000000000C1C1C100A7A7
      A700A9A9A9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BCBCBC009696960088888800848484008484840084848400848484008484
      84008484840084848400848484008484840088888800C2C2C2005580A1001784
      CD00157EC4000F5488005972840081838500A0A8AD0044759B000F69A7001D9A
      EB000F66A4000000000000000000000000000000000000000000000000000000
      0000D8D8D800C2C2C200B9B9B900B7B7B700B7B7B700B7B7B700B7B7B700B7B7
      B700B7B7B700B7B7B700B7B7B700B7B7B700B9B9B900DBDBDB00B7B7B700BCBC
      BC00B8B8B8009F9F9F00AEAEAE00B7B7B700CCCCCC00B1B1B100ACACAC00C9C9
      C900AAAAAA000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000AFAFAF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00FFFFFF00789FBC00198C
      D20022B0FF001EA0EA000E629C005888AD001C5F92001785C80022AFFE0022B0
      FF000C5D96000000000000000000000000000000000000000000000000000000
      0000D0D0D000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00FFFFFF00C9C9C900C0C0
      C000D5D5D500CBCBCB00A8A8A800BCBCBC00A6A6A600BCBCBC00D4D4D400D5D5
      D500A5A5A50000000000000000000000000000000000839FA7000077A5000077
      A8000077A8000077A8000077A8000077A8000077A8000077A8000077A8000077
      A8000077A8000077A8000077A8000077A8000077A8000077A8000077A8000077
      A8000077A8000077A8000077A8000077A8000077A8000077A800067AA900528A
      A2000000000000000000000000000000000000000000C6C6C600B0B0B000B0B0
      B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0
      B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0
      B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B1B1B100BBBB
      BB00000000000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FBFB
      FB00FBFBFB00FBFBFA00FAFAFA00FAFAFA00FAFAF900FCFCFC00B9CCDB001273
      AF0024B8FF0024B8FF0023B5FC001A92D10020A6EA0024B8FF0024B8FF001FA5
      EA00316B97000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FDFDFD00E2E2E200B1B1
      B100D8D8D800D8D8D800D6D6D600C2C2C200CECECE00D8D8D800D8D8D800CDCD
      CD00ACACAC00000000000000000000000000000000002F7F9A008DCBE200D9F5
      FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5
      FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5
      FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D2F1FD002892
      BB000000000000000000000000000000000000000000B3B3B300E0E0E000F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F6F6F600BFBF
      BF00000000000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FDFCFC00FCFCFC00FCFCFC00FCFCFB00FBFBFB00FBFB
      FB00FBFAFA00FAFAFA00FAFAFA00FAF9F900F9F9F900FAFBFA00ECF1F4000E58
      8E0025BDFC0026C0FF0026C0FF0026C0FF0026C0FF0026C0FF0026C0FF001786
      C10091B0C7000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FCFCFC00F6F6F600A2A2
      A200D9D9D900DBDBDB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00BBBB
      BB00D2D2D20000000000000000000000000000000000417C8F006DBAD700BCED
      FF0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1
      FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1
      FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE00BCEDFF0071BC
      D8006690A20000000000000000000000000000000000B1B1B100D6D6D600F3F3
      F300EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00F3F3F300D7D7
      D700BEBEBE000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FBFBFA00FAFA
      FA00FAFAFA00FAFAF900F9F9F900F9F9F900F9F8F800F3F5F700FBFBFB003971
      9D0021ADE40028C8FF0028C8FF0028C8FF0028C8FF0028C8FF0028C8FF00106C
      A200000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FBFBFB00FBFBFB00FBFBFB00FAFAFA00F9F9F900FCFCFC00B0B0
      B000CFCFCF00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00ACAC
      AC00000000000000000000000000000000000000000060797D003BA0C500C9F0
      FE007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9
      FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9
      FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC009BE2FD00AADD
      EF002C88AB0000000000000000000000000000000000B0B0B000C7C7C700F5F5
      F500E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800EDEDED00EAEA
      EA00B9B9B9000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FCFCFB00FBFBFB00FBFBFB00FBFAFA00FAFAFA00FAFA
      FA00FAF9F900F9F9F900F9F9F800F8F8F800F5F6F600E7EDF100628EB000106D
      A10028C8F8002ACFFF002ACFFF002ACFFF002ACFFF002ACFFF002ACFFF0020A9
      DA00115A8D000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00F9F9F900F4F4F400C0C0C000ACAC
      AC00DCDCDC00E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000CCCC
      CC00A2A2A200000000000000000000000000000000007B766E00198DB900C9EE
      FC0079D7FB006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3
      FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3
      FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA0072D5FA00CFF1
      FE001D8FBB0000000000000000000000000000000000AEAEAE00BCBCBC00F4F4
      F400E7E7E700E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E6E6E600F6F6
      F600BEBEBE000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAF900F9F9
      F900F9F9F900F9F8F800F8F8F800F1F4F400D1DDE7002A6896001A92C1002BD3
      FD002BD6FF002BD6FF002BD6FF002BD6FF002BD6FF002BD6FF002BD6FF002BD6
      FF0026C2ED000F699C006798B900000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFB
      FB00FBFBFB00FAFAFA00FAFAFA00F8F8F800EBEBEB00AAAAAA00BFBFBF00E1E1
      E100E2E2E200E2E2E200E2E2E200E2E2E200E2E2E200E2E2E200E2E2E200E2E2
      E200D8D8D800AAAAAA00C5C5C500000000000000000082776D00439EC0009BD6
      ED0086DBFB0058CCF70058CCF70058CCF70058CCF70058CCF70058CCF70058CC
      F70058CCF70058CCF70058CCF70058CCF70058CCF70058CCF70058CCF70058CC
      F70058CCF70058CCF70058CCF70058CCF70058CCF70058CCF70058CCF700B5E9
      FD0052AED10000000000000000000000000000000000AEAEAE00C6C6C600E7E7
      E700E9E9E900E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0
      E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0
      E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000F2F2
      F200CFCFCF000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FBFAFA00FAFAFA00FAFAFA00FAF9F900F9F9F900F9F9
      F800F8F8F800F8F8F700F5F5F600ECF0F400195E900023B4DD002DDBFF002DDB
      FF002DDBFF002DDBFF002DDBFF002DDBFF002DDBFF002DDBFF002DDBFF002DDB
      FF002DDBFF002BD6FA00147BAB0075A0BE000000000000000000000000000000
      0000D1D1D100FFFFFF00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FAFA
      FA00FAFAFA00FAFAFA00F9F9F900F6F6F600A5A5A500D0D0D000E4E4E400E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E2E2E200B3B3B300CACACA0000000000847A6F0079B3C8006CBD
      DB009FE2FC0048C7F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7
      F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7
      F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7F6008CDD
      FB0084CAE5005A93A700000000000000000000000000B0B0B000D2D2D200D8D8
      D800EEEEEE00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00EAEA
      EA00E0E0E000BFBFBF0000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FAFAFA00FAFAF900F9F9F900F9F9F900F9F8F800F8F8
      F800F8F8F700F7F7F700F9F9F900C6D5E1001171A20029CDF0002BD4F6002ACF
      F10028C9EC002CD6F8002EDEFF002EDEFF002EDEFF002EDEFF002AD1F4002AD0
      F2002CD7F8002BD5F70022B1D8003E759F000000000000000000000000000000
      0000D1D1D100FFFFFF00FCFCFC00FBFBFB00FBFBFB00FBFBFB00FAFAFA00FAFA
      FA00FAFAFA00FAFAFA00FBFBFB00E7E7E700AEAEAE00DCDCDC00E0E0E000DDDD
      DD00DBDBDB00E1E1E100E5E5E500E5E5E500E5E5E500E5E5E500DFDFDF00DEDE
      DE00E2E2E200E0E0E000CECECE00B2B2B20000000000897D7200ACC6CF003EA7
      CC00BDEDFD0043C6F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6
      F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6
      F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6F60064D1
      F900B5E5F6002D96BE00000000000000000000000000B2B2B200DDDDDD00CBCB
      CB00F3F3F300DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E3E3
      E300EFEFEF00C1C1C10000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FAF9F900F9F9F900F9F9F800F8F8F800F8F8F700F8F7
      F700F7F7F700F7F6F600F9F9F900F5F7F700799EBB00296694001B5C8E002563
      9200336D99000E6396002DDDFE002EDFFF002EDFFF001FA7D00018598B001D5C
      8C0015588A00195A8C004177A100CCDBE6000000000000000000000000000000
      0000D1D1D100FFFFFF00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFA
      FA00FAFAFA00F9F9F900FBFBFB00F9F9F900C9C9C900A9A9A900A4A4A400A8A8
      A800ADADAD00A7A7A700E4E4E400E6E6E600E6E6E600C9C9C900A2A2A200A4A4
      A400A1A1A100A2A2A200B3B3B300EAEAEA00000000008D817500D5D5D3002C9B
      C400C4ECFB0080DDFF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8
      FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8
      FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF0070D8
      FF00CCF1FF0039A4CB00000000000000000000000000B4B4B400E6E6E600C4C4
      C400F3F3F300EAEAEA00E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7
      E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7
      E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7
      E700F6F6F600C9C9C90000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F9F9F900F9F8F800F8F8F800F8F8F700F7F7F700F7F7
      F600F7F6F600F6F6F500F6F6F600FAFAFA00FAFAFA00FAFAFA00FAFAF900FAF9
      F900F9F9F9004277A00025BEE3002EDFFF002EDFFF00147AA900C7D6E300C5C5
      C500F6F6F600FEFEFE00FFFFFF00000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9
      F900F9F9F900F9F9F900F9F9F900FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFB
      FB00FBFBFB00B3B3B300D5D5D500E6E6E600E6E6E600B3B3B300E8E8E800DDDD
      DD00F9F9F900FEFEFE00FFFFFF00000000000000000093867B00DDD9D70059A9
      C50098D5EC00A0E5FF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DA
      FF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DA
      FF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DA
      FF00B8ECFF0068BCDB00000000000000000000000000B7B7B700E8E8E800CCCC
      CC00E6E6E600EFEFEF00E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800F3F3F300D7D7D70000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F8F8F800F8F8F700F8F7F700F7F7F700F7F6F600F6F6
      F600F6F6F500F6F5F500F5F5F400F5F4F400F4F5F400F3F3F400F2F3F300F3F3
      F400F9F9F80099B5CA001A91BD002EDFFF002DDCFC000E598D00E9F1F600989C
      9F00000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9
      F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F8F8F800F7F7F700F8F8
      F800FAFAFA00D5D5D500BEBEBE00E6E6E600E4E4E400A2A2A200F6F6F600C5C5
      C5000000000000000000000000000000000000000000998C7F00E2E0DB008CBA
      C90068BCDB00BBEDFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDB
      FF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDB
      FF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDB
      FF00A5E6FF0098D5EC005395AE000000000000000000BABABA00EBEBEB00D6D6
      D600D7D7D700F3F3F300EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEA
      EA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEA
      EA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEA
      EA00F0F0F000E6E6E600C0C0C000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F8F8F700F7F7F700F7F7F600F7F6F600F6F6F500F6F5
      F500F5F5F400F5F4F400F4F4F400F4F4F300F4F3F300F3F3F200F3F2F200F2F2
      F100F8F8F800DBE3E9000D6194002DDDFD0023B5DC005384A800FCFDFE008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9
      F900F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6
      F600FAFAFA00EEEEEE00A6A6A600E4E4E400D1D1D100BABABA00FDFDFD00B8B8
      B80000000000000000000000000000000000000000009F918300E9E5E200C1CE
      CC003AA4CB00D1F3FF0089DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DE
      FE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DE
      FE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DE
      FE0096E2FE00C7EDFB002C99C1000000000000000000BDBDBD00EFEFEF00E0E0
      E000C9C9C900F7F7F700EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EDEDED00F3F3F300C2C2C200000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F7F7F700F7F6F600F6F6F600F6F6F500F6F5F500F5F5
      F400F5F4F400F4F4F300F4F3F300F3F3F200F3F2F200F3F2F100F2F2F100F2F1
      F000F5F5F400F8F8F8005584A900188CB9000F679900BBCEDB00FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8
      F800F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6F600F6F6
      F600F8F8F800FAFAFA00BABABA00BCBCBC00A9A9A900E3E3E300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000A4968800F0EDE800E5DB
      D200339DC400C1E8F700A1E4FD0090DFFC0090DFFC0090DFFC0090DFFC0090DF
      FC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DF
      FC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DF
      FC0090DFFC00D2F2FE0042A8CE000000000000000000C0C0C000F3F3F300E8E8
      E800C5C5C500F1F1F100EEEEEE00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00F6F6F600CBCBCB00000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F7F6F600F6F6F500F6F5F500F5F5F400F5F4F400F4F4
      F400F4F4F300F4F3F300F3F3F200F3F2F200F2F2F100F2F1F100F1F1F000F1F0
      F000F1F1F000F8F7F700E6EAEF0089AAC200B1C6D500E5EAEB00FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6F600F6F6F600F6F6
      F600F6F6F600FAFAFA00F3F3F300CFCFCF00DFDFDF00F2F2F200FFFFFF00B8B8
      B8000000000000000000000000000000000000000000A8998A00F6F2EE00ECE1
      D60068AFC70091D0E700BBEBFD009AE1FB009AE1FB009AE1FB009AE1FB009AE1
      FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1
      FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1
      FB009AE1FB00C5EEFD006FBEDD000000000000000000C2C2C200F6F6F600EBEB
      EB00D0D0D000E3E3E300F2F2F200EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00F4F4F400D9D9D900000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F6F6F500F6F5F500F5F5F400F5F4F400F4F4F300F4F3
      F300F3F3F200F3F2F200F3F2F100F2F2F100F2F1F000F1F1F000F1F0F000F0F0
      EF00F0EFEF00F2F1F100F7F7F700F7F7F600EAEDEE00EEEDEC00FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F8F8
      F800F7F7F700F7F7F700F6F6F600F6F6F600F6F6F600F6F6F600F6F6F600F5F5
      F500F5F5F500F6F6F600FAFAFA00F9F9F900F3F3F300F3F3F300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AA9B8C00FBF5F100F2E6
      DA00A2C5CE0054B0D300D5F2FD00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4
      FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4
      FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4
      FF00D9F4FF00D9F4FF008ACCE5007EBAD10000000000C3C3C300F8F8F800EEEE
      EE00DCDCDC00D0D0D000F6F6F600F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800E0E0E000D7D7D7000000000000000000000000000000
      0000B0B0B000FFFFFF00F5F5F400F5F4F400F4F4F400F4F4F300F4F3F300F3F3
      F200F3F2F200F2F2F100F2F1F100F1F1F000F1F0F000F1F0EF00F0F0EF00F0EF
      EE00EFEFEE00EFEEED00EFEEED00EEEDEE00EEEDEC00EDEDEC00FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F7F7
      F700F7F7F700F6F6F600F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5
      F500F5F5F500F4F4F400F4F4F400F4F4F400F3F3F300F3F3F300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FEF9F200F9EB
      DE00ECE3D70062AEC8002298C3001F97C3001F97C3001F97C3001F97C3001F97
      C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97
      C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97
      C3001F97C3001F97C3002C9DC6000000000000000000C3C3C300FAFAFA00F1F1
      F100EDEDED00CFCFCF00C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2
      C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2
      C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2
      C200C2C2C200C2C2C200C5C5C500000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F5F4F400F4F4F300F4F3F300F3F3F200F3F2F200F3F2
      F100F2F2F100F2F1F000F1F1F000F1F0F000F0F0EF00F0EFEF00F0EFEE00EFEE
      EE00EFEEED00EEEEED00EEEDEC00EEEDEC00EDECEC00EDECEB00FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6
      F600F6F6F600F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFF9F300FDEE
      DF00FAEBDB00F6E7D900F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6
      D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6
      D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5EFE900A59688000000
      00000000000000000000000000000000000000000000C3C3C300FAFAFA00F3F3
      F300F1F1F100EFEFEF00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEE
      EE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEE
      EE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00F4F4F400C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F4F4F300F4F3F300F3F3F200F3F2F200F2F2F100F2F1
      F100F1F1F000F1F0F000F1F0EF00F0F0EF00F0EFEE00EFEFEE00EFEEED00EFEE
      ED00EEEDED00EEEDEC00EDEDEC00EDECEB00EDECEB00ECEBEB00FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6
      F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5F500F4F4F400F4F4
      F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFFAF300FFF0
      E000FEEEDF00FDEDDE00FDEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCED
      DD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCED
      DD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCF5F000A8998A000000
      00000000000000000000000000000000000000000000C3C3C300FAFAFA00F3F3
      F300F3F3F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F8F8F800C2C2C2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F3F3F200F3F2F200F3F2F100F2F2F100F2F1F000F1F1
      F000F1F0F000F0F0EF00F0EFEF00F0EFEE00EFEEEE00EFEEED00EEEEED00EEED
      EC00EEEDEC00EDECEC00EDECEB00EDECEB00ECEBEA00ECEBEA00FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F7F7F700F7F7F700F6F6F600F6F6F600F6F6F600F6F6
      F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3
      F300F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFF9F200FFF1
      E200FFF1E300FFF1E300FFF1E300FFF1E300FFF1E300FFF1E300FFF1E300FFF1
      E300FFF0E100FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFEF
      DE00FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFF9F200A99A8B000000
      00000000000000000000000000000000000000000000C3C3C300FAFAFA00F5F5
      F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3
      F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300FAFAFA00C2C2C2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F3F2F200F2F2F100F2F1F100F1F1F000F1F0F000F1F0
      EF00F0F0EF00F0EFEE00EFEFEE00EFEEED00EFEEED00EEEDED00EEEDEC00EDED
      EC00EDECEB00EDECEB00ECEBEB00ECEBEA00ECEBEA00EBEAEA00FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F7F7F700F6F6F600F6F6F600F6F6F600F6F6F600F5F5
      F500F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F4F4F400F3F3F300F3F3
      F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2F200FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFFCF700FFE6
      D200FFDFC400FFE1C400FFE2C500FFE4C600FFE6C700FFE7C800FFE9C800FFEB
      C900FFF1D900FFF0E100FFF6ED00FFF9F200FFF9F200FFF9F200FFF9F200FFF9
      F200FFF9F200FFF9F200FFF9F200FFF9F200FFF9F200FFFBF600AA9B8C000000
      00000000000000000000000000000000000000000000C3C3C300FCFCFC00EEEE
      EE00E9E9E900EAEAEA00EAEAEA00EBEBEB00ECECEC00EDEDED00EDEDED00EEEE
      EE00F3F3F300F4F4F400F8F8F800FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFA
      FA00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FBFBFB00C2C2C2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F2F2F100F2F1F000F1F1F000F1F0F000F0F0EF00F0EF
      EF00F0EFEE00EFEEEE00EFEEED00EEEEED00EEEDEC00EEEDEC00EDECEC00EDEC
      EB00EDECEB00ECEBEA00ECEBEA00ECEBEA00EBEAE900EBEAE900FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F6F6F600F6F6F600F6F6F600F6F6F600F5F5F500F5F5
      F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3
      F300F3F3F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFFCF900FFD9
      B900FFCCA100FFCFA200FFD2A400FFD5A500FFD7A700FFDAA800FFDEAA00FFE0
      AB00FFECCA00FFF5E900E6DFD800AE9F9100AE9F9100AE9F9100AE9F9100AE9F
      9100AE9F9100AE9F9100AE9F9100AE9F9100AE9F9100AD9E9000BEB8AC000000
      00000000000000000000000000000000000000000000C3C3C300FCFCFC00E5E5
      E500DCDCDC00DEDEDE00DFDFDF00E0E0E000E2E2E200E3E3E300E4E4E400E5E5
      E500EFEFEF00F7F7F700EBEBEB00C5C5C500C5C5C500C5C5C500C5C5C500C5C5
      C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500D3D3D3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F1F1F000F1F0F000F1F0EF00F0F0EF00F0EFEE00EFEF
      EE00EFEEED00EFEEED00EEEDED00EEEDEC00EDEDEC00EDECEB00EDECEB00ECEB
      EB00F7F7F600F9F8F800F9F8F800F8F8F800F8F8F800F8F8F800FFFFFF008585
      8500000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3
      F300F9F9F900FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFFBF500FFEE
      DF00FFEBD800FFECD800FFECD900FFEDD900FFEEDA00FFEFDA00FFF0DB00FFF0
      DB00FFF4E800FAF4EF00B3A69800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C300FBFBFB00F3F3
      F300F1F1F100F1F1F100F1F1F100F2F2F200F2F2F200F2F2F200F3F3F300F3F3
      F300F6F6F600F8F8F800C9C9C900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F1F0F000F0F0EF00F0EFEF00F0EFEE00EFEEEE00EFEE
      ED00EEEEED00EEEDEC00EEEDEC00EDECEC00EDECEB00EDECEB00ECEBEA00ECEB
      EA00F9F9F800AEAEAC00A3A3A100A3A3A100A3A3A100BDBDBC00FFFFFF009999
      9800000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F6F6F600F5F5F500F5F5F500F5F5F500F5F5F500F4F4
      F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2
      F200FAFAFA00CFCFCF00C9C9C900C9C9C900C9C9C900D8D8D800FFFFFF00C2C2
      C2000000000000000000000000000000000000000000BDB2A600DDD4CB00FDF7
      F100FFF9F000FFF9F000FFF9F000FFF9F000FFF9F000FFF9F000FFF9F000FFF9
      F000FAF5EF00C3B7AB00D5CDC600000000000000000000000000000000000000
      000000000000000000000000000000000000000000007DB17F00207E2300207F
      2400207F240022802600599E5A000000000000000000D0D0D000E4E4E400F9F9
      F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9
      F900F8F8F800D3D3D300E1E1E100000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C5C5C5009D9D9D009D9D
      9D009D9D9D009E9E9E00B6B6B600000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F0F0EF00F0EFEE00EFEFEE00EFEEEE00EFEEED00EEED
      ED00EEEDEC00EDEDEC00EDECEB00EDECEB00ECEBEB00ECEBEA00ECEBEA00EBEA
      EA00F9F8F800A0A09E00F9F9F800F8F8F800EFEFEE00FFFFFF00A4A4A200C7C7
      C700000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F5F5F500F5F5F500F5F5F500F5F5F500F4F4F400F4F4
      F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2
      F200FAFAFA00C7C7C700FAFAFA00FAFAFA00F5F5F500FFFFFF00C9C9C900DEDE
      DE00000000000000000000000000000000000000000000000000BFB3A900AC9E
      8F00AA9B8C00AA9B8C00AA9B8C00AA9B8C00AA9B8C00AA9B8C00AA9B8C00AA9B
      8C00AFA09200CFC6BD0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001E7E22000084
      220000892900008929001F7E2300000000000000000000000000D1D1D100C4C4
      C400C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3
      C300C6C6C600DCDCDC0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009D9D9D009D9D
      9D00A0A0A000A0A0A0009D9D9D00000000000000000000000000000000000000
      0000B0B0B000FFFFFF00F0EFEE00EFEEEE00EFEEED00EEEEED00EEEDEC00EEED
      EC00EDECEC00EDECEB00EDECEB00ECEBEA00ECEBEA00ECEBEA00EBEAE900EBEA
      E900F9F8F800A0A09E00FCFCFB00F3F3F300FFFFFF00A4A4A200BDBDBD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3
      F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200FAFAFA00C7C7C700FCFCFC00F8F8F800FFFFFF00C9C9C900D8D8D8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000074
      0F00008A2A00008A2A001C7D2100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009494
      9400A1A1A100A1A1A1009D9D9D00000000000000000000000000000000000000
      0000B0B0B000FFFFFF00EFEEEE00EFEEED00EEEDED00EEEDEC00EDEDEC00EDEC
      EB00EDECEB00ECEBEB00ECEBEA00ECEBEA00EBEAEA00EBEAE900EBEAE900EAEA
      E900F8F8F800A0A09E00F2F2F100FFFFFF00A4A4A200C1C1C100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F5F5F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3
      F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200FAFAFA00C7C7C700F6F6F600FFFFFF00C9C9C900DBDBDB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001B7D22000088
      2700007F1C00008422001C7D2100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009C9C9C009F9F
      9F009A9A9A009D9D9D009D9D9D00000000000000000000000000000000000000
      0000B0B0B000FFFFFF00EEEEED00EEEDEC00EEEDEC00EDECEC00EDECEB00EDEC
      EB00ECEBEA00ECEBEA00ECEBEA00EBEAE900EBEAE900EBEAE900EAE9E800EAE9
      E800F8F8F800B9B9B800FFFFFF00A4A4A200C9C9C90000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3
      F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F1F1F100F1F1
      F100FAFAFA00D5D5D500FFFFFF00C9C9C900DFDFDF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001E7F230000882600007E
      1B00539B550026822900197A1C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009D9D9D009F9F9F009A9A
      9A00B4B4B400A0A0A0009A9A9A00000000000000000000000000000000000000
      0000B0B0B000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A4A4A200C9C9C9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C9C9C900DFDFDF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000D70
      0D005B9E5B00000000000000000069A86A000877140000882700007A14005EA2
      5F00000000000000000067A56700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009393
      9300B6B6B6000000000000000000BDBDBD00979797009F9F9F0097979700B8B8
      B8000000000000000000BBBBBB00000000000000000000000000000000000000
      0000C8C8C800AEAEAE00AEAEAE00AEAEAE00AEAEAE00AEAEAE00AEAEAE00AEAE
      AE00AEAEAE00AEAEAE00AEAEAE00AEAEAE00AEAEAE00AEAEAE00AEAEAE00AEAE
      AE00AEAEAE00A4A4A200CBCBCB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DEDEDE00D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000D0D0D000C9C9C900E1E1E100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001B7A1D0000710B0000761100007E1A00007C180021802700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009A9A9A0093939300969696009A9A9A00989898009F9F9F00000000000000
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
      000000000000000000007CB37D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C5C5C500000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000600100000100010000000000001600000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
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
      00000000000000000000000000000000FFFF8000FFFF8000FFFFFFFFFFFFFFFF
      FFFF8000FFFF8000FFF00FFFFFF00FFFFFC78000FFC78000FF8003FFFF8003FF
      FFE38000FFE38000FE0000FFFE0000FFFFF18000FFF18000FC00007FFC00007F
      FFF88000FFF88000F800003FF800003FFF800000FF800000F000001FF000001F
      0600000006000000E000000FE000000F0C3880000C388000C0000007C0000007
      18F1800018F18000C0000007C000000719E3800019E380008000000380000003
      11468000114680008000000380000003137C8000137C80008000000380000003
      120080001200800000000001000000011E0080001E0080000000000100000001
      0000FFFF0000FFFF0000000100000001000007FF000007FF0000000100000001
      000007FF000007FF0000000100000001000007FF000007FF0000000100000001
      000007FF000007FF0000000100000001000007FF000007FF8000000380000003
      000007FF000007FF8000000380000003000007FF000007FF8000000380000003
      000007FF000007FFC0000007C0000007000007FF000007FFC0000007C0000007
      000007FF000007FFE000000FE000000F000007FF000007FFF000001FF000001F
      00000FFF00000FFFF800003FF800003F00001FFF00001FFFFC00007FFC00007F
      00003FFF00003FFFFE0000FFFE0000FF00007FFF00007FFFFF8003FFFF8003FF
      0000FFFF0000FFFFFFF01FFFFFF01FFFFFFFFFFFFFFFFFFFFFE00000FFE00000
      FFF00FFFFFF00FFFFFE00000FFE00000FF8003FFFF8003FFFFE00000FFE00000
      FE0000FFFE0000FFFFE00000FFE00000FC00007FFC00007FFFE00000FFE00000
      F800003FF800003FFFE00000FFE00000F000001FF000001FFFE00000FFE00000
      E000000FE000000FFFE00000FFE00000C0000007C0000007FFE00000FFE00000
      C0000007C0000007FFE00000FFE000008000000380000003FFE00000FFE00000
      8000000380000003FFE00000FFE000008000000380000003FFE00000FFE00000
      0000000100000001FFE00000FFE000000000000100000001FFE00000FFE00000
      0000000100000001FFE00000FFE000000000000100000001FFFF0CC0FFFF0CC0
      0000000100000001000118600001186000000001000000010001303100013031
      00000001000000010001E01B0001E01B80000003800000030001C48F0001C48F
      80000003800000030001CCCF0001CCCF80000003800000030001DCEF0001DCEF
      C0000007C00000070001F8FF0001F8FFC0000007C00000070001F9FF0001F9FF
      E000000FE000000F0001F1FF0001F1FFF000001FF000001F0001C3FF0001C3FF
      F800003FF800003F000187FF000187FFFC00007FFC00007F00019FFF00019FFF
      FE0000FFFE0000FF0001FFFF0001FFFFFF8003FFFF8003FF0001FFFF0001FFFF
      FFF01FFFFFF01FFF0001FFFF0001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFF00FFFFFF00FFFFFF00FFFFFF00FFFFF8003FFFF8003FFFF8003FFFF8003FF
      FE0000FFFE0000FFFE0000FFFE0000FFFC00007FFC00007FFC00007FFC00007F
      F800003FF800003FF800003FF800003FF000001FF000001FF000001FF000001F
      E000000FE000000FE000000FE000000FC0000007C0000007C0000007C0000007
      C0000007C0000007C0000007C000000780000003800000038000000380000003
      8000000380000003800000038000000380000003800000038000000380000003
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000180000003800000038000000380000003
      8000000380000003800000038000000380000003800000038000000380000003
      C0000007C0000007C0000007C0000007C0000007C0000007C0000007C0000007
      E000000FE000000FE000000FE000000FF000001FF000001FF000001FF000001F
      F800003FF800003FF800003FF800003FFC00007FFC00007FFC00007FFC00007F
      FE0000FFFE0000FFFE0000FFFE0000FFFF8003FFFF8003FFFF8003FFFF8003FF
      FFF01FFFFFF01FFFFFF01FFFFFF01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFF00FFFFFF00FFFFFF00FFFFFF00FFFFF8003FFFF8003FFFF8003FFFF8003FF
      FE0000FFFE0000FFFE0000FFFE0000FFFC00007FFC00007FFC00007FFC00007F
      F800003FF800003FF800003FF800003FF000001FF000001FF000001FF000001F
      E000000FE000000FE000000FE000000FC0000007C0000007C0000007C0000007
      C0000007C0000007C0000007C000000780000003800000038000000380000003
      8000000380000003800000038000000380000003800000038000000380000003
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000180000003800000038000000380000003
      8000000380000003800000038000000380000003800000038000000380000003
      C0000007C0000007C0000007C0000007C0000007C0000007C0000007C0000007
      E000000FE000000FE000000FE000000FF000001FF000001FF000001FF000001F
      F800003FF800003FF800003FF800003FFC00007FFC00007FFC00007FFC00007F
      FE0000FFFE0000FFFE0000FFFE0000FFFF8003FFFF8003FFFF8003FFFF8003FF
      FFF01FFFFFF01FFFFFF01FFFFFF01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFC7FFFFFFC7FFF00FFFFFF00FFFFFFFFF83FFFFFF83FF8003FFFF8003FF
      FFFFFF01FFFFFF01FE0000FFFE0000FFFFFFFE01FFFFFE01FC00007FFC00007F
      FFFFFC01FFFFFC01F800003FF800003FFFFFF803FFFFF803F000001FF000001F
      FFC07807FFC07807E000000FE000000FFF00100FFF00100FC0000007C0000007
      FC00000FFC00000FC0000007C0000007F800001FF800001F8000000380000003
      F000007FF000007F8000000380000003E00000FFE00000FF8000000380000003
      E00000FFE00000FF0000000100000001C000007FC000007F0000000100000001
      C000007FC000007F0000000100000001C000003FC000003F0000000100000001
      8000003F8000003F00000001000000018000003F8000003F0000000100000001
      8000003F8000003F00000001000000018000003F8000003F8000000380000003
      8000003F8000003F8000000380000003C000003FC000003F8000000380000003
      C000007FC000007FC0000007C0000007C000007FC000007FC0000007C0000007
      E00000FFE00000FFE000000FE000000FE00000FFE00000FFF000001FF000001F
      F00001FFF00001FFF800003FF800003FF80003FFF80003FFFC00007FFC00007F
      FC0007FFFC0007FFFE0000FFFE0000FFFF001FFFFF001FFFFF8003FFFF8003FF
      FFE07FFFFFE07FFFFFF01FFFFFF01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFC00003FFC00003FFC00003FFC00003FFC00003FFC00003FFC00003FFC00003
      FFC00003FFC00003FFC00003FFC00003FFC00003FFC00003C0000003C0000003
      FFC00003FFC000038000000380000003FFC00003FFC000038000000380000003
      FFC00003FFC000038000000380000003E0000003E00000038000000380000003
      E0000003E00000038000000380000003E0000003E00000038000000380000003
      E0000003E00000038000000380000003E0000003E00000038000000380000003
      E0000003E00000038000000380000003E0000003E00000038000000380000003
      E0000003E00000038000000380000003E0000003E00000038000000380000003
      E0000003E00000038000000380000003E0000003E00000038000000380000003
      E0000007E00000078000000780000007E000000FE000000F8000000F8000000F
      E000001FE000001F8000001F8000001FE000003FE000003F8000003F8000003F
      E000007FE000007F8000007F8000007FE00007FFE00007FF8000007F8000007F
      E00007FFE00007FF8000007F8000007FE00007FFE00007FF8000007F8000007F
      E0000FFFE0000FFF8000007F8000007FE0001FFFE0001FFF8000007F8000007F
      E0003FFFE0003FFF8000007F8000007FFFFFFFFFFFFFFFFFFFC0FFFFFFC0FFFF
      FFFFFFFFFFFFFFFFFFC1FFFFFFC1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFE3FFFFFFE3FFFFE3FFFFFFE3FFFFFFFF81FFFFFF81FF
      FFC1FFFFFFC1FFFFFFFF00FFFFFF00FFFF81FFFFFF81FFFFFFFF08FFFFFF08FF
      FF01FFFFFF01FFFFFFFE18FFFFFE18FFFE03FFFFFE03FFFFFFFE38FFFFFE38FF
      FE07FFFFFE07FFFFFFFE30FFFFFE30FFFC0FFFFFFC0FFFFFFFFE2107FFFE2107
      F81FFFFFF81FFFFFFFFE0003FFFE0003F03FFFFFF03FFFFFFFFF0001FFFF0001
      F03E0001F03E0001FFFF8071FFFF8071E07C0001E07C0001FFFC00E1FFFC00E1
      E07E0001E07E0001FFF000C3FFF000C3C0FF0001C0FF0001FFC00003FFC00003
      C0FF8001C0FF8001FF000807FF000807C1FFC001C1FFC001FE010C1FFE010C1F
      81FFE00181FFE001F8070FFFF8070FFF81FFC00181FFC001F01E0FFFF01E0FFF
      81FF800181FF8001E07E1FFFE07E1FFF80FE000180FE0001C3FC1FFFC3FC1FFF
      80780001807800018FFC3FFF8FFC3FFF8000000180000001BFF83FFFBFF83FFF
      8000008180000081FFF87FFFFFF87FFFC00001C1C00001C1FFF0FFFFFFF0FFFF
      C00007E1C00007E1FFF0FFFFFFF0FFFFE0000FF1E0000FF1FFE1FFFFFFE1FFFF
      F0003FFBF0003FFBFFE3FFFFFFE3FFFFFC01FFFFFC01FFFFFFC7FFFFFFC7FFFF
      FFFFFFFFFFFFFFFFFFCFFFFFFFCFFFFFFFFFFFFFFFFFFFFFFF9FFFFFFF9FFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3FFFFFFF3FFFFFFFFFFFFFFFF
      FFFFFFE1FFFFFFE1FFFFFFFFFFFFFFFFF0000000F0000000FFFFC7FFFFFFC7FF
      E0000000E0000000FFFF83FFFFFF83FFC0000001C0000001FFFF81FFFFFF81FF
      8000000380000003FFFF80FFFFFF80FF8000000380000003FFFFC07FFFFFC07F
      8000000380000003FFFFE07FFFFFE07F8000000380000003FFFFF03FFFFFF03F
      8000000380000003FFFFF81FFFFFF81F8000000380000003FFFFFC0FFFFFFC0F
      800000038000000380007C0F80007C0F800000038000000380003E0780003E07
      800000038000000380007E0780007E0780000003800000038000FF038000FF03
      80000003800000038001FF038001FF0380000003800000038003FF838003FF83
      80000003800000038007FF818007FF8180000003800000038003FF818003FF81
      80000003800000038000FF818000FF81800000038000000380007F0180007F01
      800000038000000380001E0180001E0180000003800000038000000180000001
      8000000380000003810000018100000180000003800000038380000383800003
      800000038000000387E0000387E0000380000003800000038FF000078FF00007
      8000000380000003DFFC000FDFFC000F8000000380000003FFFF803FFFFF803F
      8000000380000003FFFFFFFFFFFFFFFF8000000380000003FFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFF80FFFFFF80FFFFFFFFFFFFFFFFFFFFFE003FFFFE003F0000003F0000003
      FFFFC001FFFFC001E0000003E0000003FFFF8001FFFF8001C0000003C0000003
      FF800001FF8000018000000380000003FE000001FE0000018000000380000003
      FC000001FC0000018000000380000003F0000001F00000018000000380000003
      E0000001E00000018000000380000003E0000001E00000018000000380000003
      C0000001C0000001800000038000000380000001800000018000000380000003
      8000000180000001800000038000000380000001800000018000000380000003
      0000000100000001800000038000000300000007000000078000000380000003
      0000001F0000001F80000003800000030000001F0000001F8000000380000003
      0000001F0000001F80000003800000030000001F0000001F8000000380000003
      0000001F0000001F80000003800000038000003F8000003F8000000380000003
      8000003F8000003F80000003800000038000003F8000003F8000000380000003
      C000007FC000007F8000000380000003E00000FFE00000FF8000000380000003
      E00000FFE00000FF8000000380000003F00001FFF00001FF8000000380000003
      FC0007FFFC0007FF8000000380000003FE000FFFFE000FFF8000000380000003
      FFC07FFFFFC07FFFFFFFFFFFFFFFFFFFFFFFC7C7FFFFC7C7FFFFFFFFFFFFFFFF
      F0000007F0000007FFFFFFFFFFFFFFFFF0000007F00000078000000F8000000F
      F0000007F00000078000000F8000000FF0000007F00000078000000780000007
      F000000FF000000F8000000780000007F0000007F00000078000000780000007
      F0000001F00000018000000780000007F0000000F00000008000000380000003
      F0000000F00000008000000380000003F0000000F00000008000000380000003
      F0000001F00000018000000380000003F000000FF000000F8000000180000001
      F000000FF000000F8000000180000001F000000FF000000F8000000180000001
      F000000FF000000F8000000180000001F000000FF000000F8000000080000000
      F000000FF000000F8000000180000001F000000FF000000F8000001F8000001F
      F000000FF000000F8000001F8000001FF000000FF000000F8000001F8000001F
      F000000FF000000F8000001F8000001FF000000FF000000F8000001F8000001F
      F000000FF000000F8001FFFF8001FFFFF000000FF000000F8001FF818001FF81
      F000000FF000000FC003FFC1C003FFC1F000001FF000001FFFFFFFE1FFFFFFE1
      F000003FF000003FFFFFFFC1FFFFFFC1F000007FF000007FFFFFFF81FFFFFF81
      F00000FFF00000FFFFFFE60DFFFFE60DF00001FFF00001FFFFFFF03FFFFFF03F
      FFFFFFFFFFFFFFFFFFFFFDFFFFFFFDFF00000000000000000000000000000000
      000000000000}
  end
  object pmTreeView: TPopupMenu
    OnPopup = pmTreeViewPopup
    Left = 960
    Top = 168
    object miTvInsert: TMenuItem
      Caption = 'Insert'
    end
    object miTvDelete: TMenuItem
      Caption = 'Delete'
    end
    object miTvUp: TMenuItem
      Caption = 'Up'
    end
    object miTvDown: TMenuItem
      Caption = 'Down'
    end
    object miTvIn: TMenuItem
      Caption = 'In'
    end
    object miTvOut: TMenuItem
      Caption = 'Out'
    end
  end
  object imgDisabled: TImageList
    Height = 32
    Width = 32
    Left = 1008
    Top = 116
    Bitmap = {
      494C010127006C00B40020002000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000800000004001000001002000000000000080
      0200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB000000000000000000000000000000
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
      00000000000000000000000000000000000000000000BBBBBB00B8B8B800B5B5
      B500B5B5B500B5B5B500B7B7B700BBBBBB00BBBBBB00BBBBBB00BBBBBB00B8B8
      B800B5B5B500B5B5B500B8B8B800BBBBBB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000816863007E5751008C584F008D574D008A574E007D5751007D6460008A80
      7F00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A7A7A7009F9F9F009F9F9F009E9E9E009E9E9E009E9E9E00A6A6A600B5B5
      B500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AAAAAA00AAAA
      AA00AAAAAA0000000000000000000000000000000000B8B8B800EAEAEA00FFFF
      FF00FFFFFF00FFFFFF00E7E7E700B5B5B500BBBBBB00BBBBBB00B7B7B700D9D9
      D900FFFFFF00FFFFFF00DDDDDD00B8B8B8000000000000000000000000000000
      0000000000000000000000000000000000000000000094716A0092554700AC6F
      5C00C48B7300D69F8300E0AA8D00E2AC8F00DFA98D00D59C8200C3887200AA6C
      5A00915346008D6E680000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ADADAD009D9D9D00ACAC
      AC00BBBBBB00C5C5C500CBCBCB00CDCDCD00CBCBCB00C4C4C400B9B9B900AAAA
      AA009C9C9C00ABABAB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A5A5
      A500A4A4A400AAAAAA00000000000000000000000000BBBBBB00B5B5B500F5F5
      F500FFFFFF00FFFFFF00FFFFFF00D9D9D900B7B7B700B8B8B800CBCBCB00FFFF
      FF00FFFFFF00EBEBEB00B4B4B400BBBBBB000000000000000000000000000000
      00000000000000000000000000009676700099594A00C88E7700ECB79800EFB6
      9500EEAE8C00ECA78400ECA68200ECA68200ECA68200EDA98500EEAF8C00EFB7
      9600EAB59600C48972009757490090726C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B0009F9F9F00BCBCBC00D2D2D200D2D2
      D200CECECE00CACACA00C9C9C900C9C9C900C9C9C900CACACA00CECECE00D2D2
      D200D1D1D100B9B9B9009F9F9F00ADADAD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A5A500A4A4A400AAAAAA000000000000000000BBBBBB00BABABA00B9B9
      B900FDFDFD00FFFFFF00FFFFFF00FFFFFF00CACACA00BFBFBF00FFFFFF00FFFF
      FF00F6F6F600B6B6B600BBBBBB00BBBBBB000000000000000000000000000000
      0000000000000000000090594E00BC826C00EDB89900EEB08E00EBA27D00EB9F
      7900EB9F7900E4987300DA8F6A00DA8F6C00E4987300EB9F7900EB9F7900EB9F
      7900EBA37E00EEB18F00EBB69700B77B66008E574D0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A0A0A000B6B6B600D3D3D300CECECE00C7C7C700C5C5
      C500C5C5C500C1C1C100BCBCBC00BCBCBC00C1C1C100C5C5C500C5C5C500C5C5
      C500C7C7C700CFCFCF00D2D2D200B2B2B2009F9F9F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A4A4A400A4A4A400AAAAAA0000000000BBBBBB00BBBBBB00B9B9
      B900C1C1C100FFFFFF00FFFFFF00FFFFFF00FDFDFD00F8F8F800FFFFFF00FFFF
      FF00BBBBBB00BABABA00BBBBBB00BBBBBB000000000000000000000000000000
      00000000000097574A00D7A08400EFB79600EBA37E00EB9F7900EB9F7900E99C
      7700CE835F00C5836500D7A79100D7A69100C5836500CD835F00E99C7700EB9F
      7900EB9F7900EB9F7900ECA48000F1B89700D2987E0093564900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009E9E9E00C6C6C600D2D2D200C7C7C700C5C5C500C5C5C500C3C3
      C300B5B5B500B5B5B500CACACA00CACACA00B5B5B500B5B5B500C3C3C300C5C5
      C500C5C5C500C5C5C500C8C8C800D3D3D300C2C2C2009E9E9E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B9B9B900A7A7A700A4A4
      A400A4A4A400A4A4A400A4A4A400A4A4A400AAAAAA00BBBBBB00BBBBBB00BBBB
      BB00B8B8B800CCCCCC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C4C4
      C400B9B9B900BBBBBB00BBBBBB00BBBBBB000000000000000000000000000000
      000098594B00DFA98C00EEB39000EB9F7900EB9F7900EB9F7900EB9F7900D085
      6200D6A58F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D6A58F00D0856200EB9F
      7900EB9F7900EB9F7900EB9F7900EBA07A00EFB49200D9A28600945749000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009F9F9F00CBCBCB00D0D0D000C5C5C500C5C5C500C5C5C500C5C5C500B7B7
      B700CACACA00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CACACA00B7B7B700C5C5
      C500C5C5C500C5C5C500C5C5C500C6C6C600D1D1D100C7C7C7009E9E9E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BFBFBF00A9A9A900A9A9A900A9A9
      A900B8B8B8000000000000000000B1B1B100A4A4A400A4A4A400A4A4A400A4A4
      A400A4A4A400A4A4A400A4A4A400A4A4A400ABABAB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00B7B7B700D8D8D800FFFFFF00FFFFFF00FFFFFF00F2F2F200B4B4
      B400BBBBBB00BBBBBB00BBBBBB00BBBBBB00000000000000000000000000975E
      4F00D9A28600EEB19000EB9F7900EB9F7900EB9F7900EB9F7900E99C7800C27A
      5900FFFBF800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFBF800C2795900E99C
      7800EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EFB49200D2997E00915A
      4D0000000000000000000000000000000000000000000000000000000000A2A2
      A200C7C7C700D0D0D000C5C5C500C5C5C500C5C5C500C5C5C500C4C4C400B0B0
      B000FCFCFC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FCFCFC00B0B0B000C4C4
      C400C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500D1D1D100C2C2C200A0A0
      A000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF000000000000000000A9A9A900A4A4A400A5A5A500C5C5C500000000000000
      000000000000A4A4A400A4A4A400A9A9A90000000000BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00B7B7B700D6D6D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7E7
      E700B5B5B500BBBBBB00BBBBBB00BBBBBB000000000000000000B5887C00C387
      6F00EFB69500EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900E4987300C887
      6A00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6856800E699
      7300EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EBA07A00F1B89700BA7E
      670093746D000000000000000000000000000000000000000000BABABA00B8B8
      B800D2D2D200C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C1C1C100B8B8
      B800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B7B7B700C2C2
      C200C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C6C6C600D3D3D300B3B3
      B300AFAFAF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF000000
      000000000000B1B1B100A4A4A400ACACAC000000000000000000000000000000
      0000ABABAB00A4A4A400A9A9A9000000000000000000BBBBBB00BBBBBB00BBBB
      BB00B8B8B800CBCBCB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00D8D8D800B7B7B700BBBBBB00BBBBBB000000000000000000A2624E00EEB9
      9900EBA27D00EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EA9D7800C279
      5800FFF9F600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEF8F500C1785800EA9D
      7800EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900ECA48000ECB7
      97009D5D4A000000000000000000000000000000000000000000A4A4A400D4D4
      D400C7C7C700C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C4C4C400B0B0
      B000FAFAFA00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FAFAFA00AFAFAF00C4C4
      C400C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C8C8C800D2D2
      D200A2A2A2000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF000000
      000000000000A4A4A400A5A5A50000000000000000000000000000000000ADAD
      AD00A4A4A400A9A9A900000000000000000000000000BBBBBB00BBBBBB00B9B9
      B900C1C1C100FFFFFF00FFFFFF00F8F8F800B9B9B900FDFDFD00FFFFFF00FFFF
      FF00FFFFFF00CACACA00B8B8B800BBBBBB0000000000B3847700CF967A00EEAE
      8B00EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900D287
      6400D39F8700FFFDFB00FFFFFF00FFFFFF00FFFCF900D39D8600D3876500EB9F
      7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EB9F7900EEB0
      8E00C98E730093726900000000000000000000000000B8B8B800C0C0C000CDCD
      CD00C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500B8B8
      B800C6C6C600FDFDFD00FFFFFF00FFFFFF00FCFCFC00C5C5C500B8B8B800C5C5
      C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500CECE
      CE00BCBCBC00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF000000
      0000B8B8B800A4A4A400C5C5C50000000000FFFFFF0000000000AAAAAA00AAAA
      AA00AAAAAA000000000000000000FFFFFF0000000000BBBBBB00BABABA00B9B9
      B900FDFDFD00FFFFFF00FFFFFF00BCBCBC00B8B8B800C2C2C200FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000B9B9B90000000000A05E4900EEB99900EBA3
      7D00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EAA0
      7A00D0876400C37E5F00D29D8500D29C8500C37E5F00D0886500EAA07A00EBA1
      7B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA17B00EBA5
      8000EBB696009C5B4700000000000000000000000000A2A2A200D4D4D400C7C7
      C700C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600B7B7B700B3B3B300C5C5C500C5C5C500B3B3B300B8B8B800C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C8C8
      C800D2D2D200A0A0A00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF000000
      0000A6A6A600A4A4A4000000000000000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0000000000BBBBBB00B9B9B900F9F9
      F900FFFFFF00FFFFFF00C6C6C600B9B9B900BBBBBB00B8B8B800CDCDCD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00BCBCBC0000000000BA7D6400EEB49100EBA5
      7E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA5
      7E00E39D7800D9956F00D08C6700D28C6700D9956F00E09A7400EBA57E00EBA5
      7E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA57E00EBA5
      7E00EFB69300B4765C00000000000000000000000000B3B3B300D1D1D100C8C8
      C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8
      C800C3C3C300BFBFBF00B9B9B900B9B9B900BFBFBF00C2C2C200C8C8C800C8C8
      C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8
      C800D2D2D200AEAEAE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF000000
      0000CFCFCF00CFCFCF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000BBBBBB00BBBBBB00B8B8
      B800B7B7B700B7B7B700B9B9B900BBBBBB00BBBBBB00BBBBBB00B9B9B900B7B7
      B700B7B7B700B7B7B700B8B8B800BBBBBB00C3998B00D2977A00ECB08B00EBAB
      8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300E4A4
      7D00C4866600D4A48D00D4A48D00D4A48D00D4A48D00C88D7000DD9B7600EBAB
      8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB8300EBAB
      8300EDB18D00CA8F72009A80770000000000C4C4C400C1C1C100CECECE00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00C7C7
      C700B7B7B700C9C9C900C9C9C900C9C9C900C9C9C900BBBBBB00C2C2C200CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CFCFCF00BCBCBC00B5B5B500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00B07B6700E0A78700EBB18900EBAF
      8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700DFA2
      7B00D2A28900FFFFFF00FFFFFF00FFFFFF00FFFFFF00DEBBA900D6987300EBAF
      8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF8700EBAF
      8700ECB18C00D9A08100966C5E0000000000B1B1B100C9C9C900CECECE00CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00C6C6
      C600C7C7C700FFFFFF00FFFFFF00FFFFFF00FFFFFF00D6D6D600C0C0C000CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCD
      CD00CECECE00C6C6C600AAAAAA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5674F00E9B39100EBB58D00EBB4
      8D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00E3AB
      8400CC997D00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F3E3DA00C4876600E6AD
      8600EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB48D00EBB4
      8D00ECB58E00E2AB8900A46C580000000000A5A5A500D0D0D000D0D0D000D0D0
      D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000CACA
      CA00C2C2C200FFFFFF00FFFFFF00FFFFFF00FFFFFF00EEEEEE00B7B7B700CBCB
      CB00D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000D0D0D000CBCBCB00A9A9A900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFE
      FE00A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A4634900EDB89600EFBA9300EFBA
      9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFB9
      9200C98C6A00F3E2D900FFFFFF00FFFFFF00FFFFFF00FFFFFF00E9D2C300C88C
      6900E9B38C00EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA9300EFBA
      9300EFBA9300E6AF8E00A56C540000000000A3A3A300D2D2D200D3D3D300D3D3
      D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D2D2
      D200B9B9B900EDEDED00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E3E3E300B9B9
      B900CFCFCF00D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3
      D300D3D3D300CECECE00A9A9A900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00FFFFFF00FEFE
      FE00A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A9694F00ECB79300F5C09900F5C0
      9900F5C09900F5C09900F5C09900F5C09900F5C09900F5C09900F5C09900F5C0
      9900E8B18C00CB937400F9F1EB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EDD7
      CB00CB906E00EDB79100F5C09900F5C09900F5C09900F5C09900F5C09900F5C0
      9900F5C09900E6AE8D00A770590000000000A7A7A700D2D2D200D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600CECECE00BEBEBE00F6F6F600FFFFFF00FFFFFF00FFFFFF00FFFFFF00E6E6
      E600BCBCBC00D1D1D100D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600CDCDCD00ABABAB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFE
      FE00A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B37B6300E4AE8C00F5C29C00F6C2
      9C00F6C29C00F6C29C00F6C29C00F6C29C00F6C29C00F6C29C00F6C29C00F6C2
      9C00F6C29C00E6AF8C00CD967A00FCF3EE00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00EED8CD00CC907000F3BF9900F6C29C00F6C29C00F6C29C00F6C29C00F6C2
      9C00F6C29D00DEA585009C73610000000000B1B1B100CDCDCD00D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800CDCDCD00C0C0C000F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00E7E7E700BCBCBC00D6D6D600D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800C8C8C800ACACAC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00FFFFFF00FEFE
      FE00A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C89D8900D79D7D00F6C3A000F6C3
      9F00F6C39F00F6C39F00F6C39F00F6C39F00F6C39F00F6C39F00F6C39F00F6C3
      9F00F6C39F00F6C39F00E4AE8B00CE9A7E00FFFBF800FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E1BFAC00D9A28000F6C39F00F6C39F00F6C39F00F6C39F00F6C3
      9F00F5C39F00D0967600A2877B0000000000C5C5C500C4C4C400D9D9D900D9D9
      D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9
      D900D9D9D900D9D9D900CDCDCD00C2C2C200FCFCFC00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00D8D8D800C6C6C600D9D9D900D9D9D900D9D9D900D9D9D900D9D9
      D900D8D8D800C0C0C000B8B8B800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C4876600F5C4A200F5C4
      A000F5C4A000F5C4A000F5C4A000F5C4A000F5C4A000F5C3A000F1BF9B00F5C4
      A000F5C4A000F5C4A000F5C4A000DAA48300DEBAA600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFBF800C9907100F4C29F00F5C4A000F5C4A000F5C4A000F5C4
      A000F5C4A200BF805F00000000000000000000000000B7B7B700D9D9D900D9D9
      D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D6D6D600D9D9
      D900D9D9D900D9D9D900D9D9D900C7C7C700D5D5D500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FCFCFC00BCBCBC00D8D8D800D9D9D900D9D9D900D9D9D900D9D9
      D900D9D9D900B3B3B30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00FFFFFF00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AE6B4B00F3C1A000F5C5
      A300F4C4A200F4C4A200F4C4A200F4C4A200F4C4A200D39C7C00C9937700D29A
      7A00E0AC8C00EAB89700ECBA9900E1AD8C00D2A38B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00D2A48C00EAB89700F4C4A200F4C4A200F4C4A200F5C5
      A400EEBC9B00AD6B4C00000000000000000000000000A7A7A700D8D8D800DADA
      DA00D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900C3C3C300BEBEBE00C2C2
      C200CCCCCC00D2D2D200D4D4D400CDCDCD00C8C8C800FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C9C9C900D2D2D200D9D9D900D9D9D900D9D9D900DADA
      DA00D5D5D500A7A7A70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C2907800D9A28000F5C8
      A600F4C4A300F4C4A300F4C4A300F4C4A300EEBF9D00CA967A00FFFFFE00EFDF
      D500DEBBAA00D2A58D00CE9F8600D6AD9700F4E6DF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00D2A68F00E9B89700F4C4A300F4C4A300F4C4A300F5C8
      A600D49A7900A7847200000000000000000000000000BDBDBD00C6C6C600DBDB
      DB00D9D9D900D9D9D900D9D9D900D9D9D900D6D6D600C0C0C000FEFEFE00EBEB
      EB00D6D6D600C9C9C900C5C5C500CECECE00EFEFEF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CACACA00D2D2D200D9D9D900D9D9D900D9D9D900DBDB
      DB00C2C2C200B6B6B60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B7745100F3C5
      A400F5C6A600F4C5A400F4C5A400F4C5A400DEAB8C00DEBCAB00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C9937700F1C1A000F4C5A400F4C5A400F5C8A700F1C2
      A100B3704C000000000000000000000000000000000000000000ACACAC00DADA
      DA00DBDBDB00DADADA00DADADA00DADADA00CBCBCB00D6D6D600FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BEBEBE00D8D8D800DADADA00DADADA00DCDCDC00D8D8
      D800A9A9A9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FDFD
      FD00A9A9A9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7997F00D399
      7700F7CBAC00F4C5A400F4C5A400F4C5A400CE987900F4E6DF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00EBD5C900D09B7C00F4C5A400F4C5A400F4C5A500F7CBAC00CC91
      6E00B18F7C000000000000000000000000000000000000000000C2C2C200C1C1
      C100DDDDDD00DADADA00DADADA00DADADA00C0C0C000EFEFEF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E5E5E500C2C2C200DADADA00DADADA00DADADA00DDDDDD00BCBC
      BC00BCBCBC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00FFFFFF00FFFFFF00C0C0C000A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A900A9A9A9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B576
      5400E7B59300F8CEAF00F4C5A400F4C5A400CB957600E3C6B700FFFBF800FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F1E0D700C9927600ECBB9B00F4C5A400F4C5A400F8CDAF00E0AD8B00AB72
      530000000000000000000000000000000000000000000000000000000000ADAD
      AD00D1D1D100DFDFDF00DADADA00DADADA00BFBFBF00DCDCDC00FCFCFC00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00ECECEC00BEBEBE00D5D5D500DADADA00DADADA00DFDFDF00CCCCCC00AAAA
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A9A9A900FEFEFE00FFFFFF00FFFFFF00FEFEFE00CECE
      CE00B7B7B7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B6744F00EDBF9F00F9CFB000F4C5A400EDBE9D00D7A28300C8917400DAB6
      A200EBD6CB00F9F1EC00FFFDFC00FFFFFF00FFFDFC00F9F1EC00ECD7CD00D6AD
      9800CB937600E9B89800F4C5A400F4C5A500F9CFB100E9B89800B3734E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000ACACAC00D6D6D600E0E0E000DADADA00D6D6D600C6C6C600BDBDBD00D3D3
      D300E6E6E600F6F6F600FDFDFD00FFFFFF00FDFDFD00F6F6F600E7E7E700CECE
      CE00BEBEBE00D3D3D300DADADA00DADADA00E0E0E000D3D3D300ABABAB000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00FFFFFF00FFFFFF00A9A9A900FEFEFE00FFFFFF00FEFEFE00D0D0D000B2B2
      B200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B7754F00EABA9A00FCD3B600F5C9A700F4C5A400F1C1A000E1AF
      8F00D5A18200CB957600C68E7000C48C6E00C68E7000CB957600D5A08100E3B1
      9100F2C3A200F4C5A400F6C9A900FCD3B600E6B49300B6744E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000ACACAC00D4D4D400E3E3E300DCDCDC00DADADA00D8D8D800CECE
      CE00C6C6C600BFBFBF00BBBBBB00BABABA00BBBBBB00BFBFBF00C5C5C500CFCF
      CF00D9D9D900DADADA00DCDCDC00E3E3E300D0D0D000ACACAC00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A9A9A900FEFEFE00FFFFFF00D2D2D200B1B1B1000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B97A5500D8A17E00FDD5B800FBD2B400F5C8A700F4C5
      A400F4C5A400F4C5A400F4C5A400F4C5A400F4C5A400F4C5A400F4C5A400F4C5
      A400F5C9A900FCD3B500FBD3B600D49B7800BC7E5B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AFAFAF00C5C5C500E3E3E300E2E2E200DCDCDC00DADA
      DA00DADADA00DADADA00DADADA00DADADA00DADADA00DADADA00DADADA00DADA
      DA00DCDCDC00E2E2E200E2E2E200C2C2C200B1B1B10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A9A9A900FEFEFE00D5D5D500AEAEAE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CEA08400BE7B5400E2AF8E00FED7BB00FED7
      BB00FBD2B400F8CDAD00F7CBAC00F7CBAB00F7CBAC00F8CDAE00FCD2B400FFD7
      BB00FDD6BA00DFAB8800BA785000D2A78F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C5C5C500AFAFAF00CDCDCD00E5E5E500E5E5
      E500E2E2E200DFDFDF00DDDDDD00DDDDDD00DDDDDD00DFDFDF00E2E2E200E5E5
      E500E4E4E400CBCBCB00ADADAD00CACACA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABABAB00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A9A9A900D6D6D600ADADAD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CC9C7F00B9764E00CE95
      6F00E2B08F00EFC3A500F7CEB300FBD3B800F7CEB100EEC2A300E0AD8C00CD91
      6D00B8764E00CFA2870000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C300ACACAC00BEBE
      BE00CECECE00D9D9D900E0E0E000E3E3E300E0E0E000D8D8D800CCCCCC00BCBC
      BC00ACACAC00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BFBFBF00A9A9A900A9A9A900A9A9
      A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A900A9A9A900A9A9A900A9A9A900B1B1B1000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6AE9600C9937400BF815D00BC7C5700BE815D00CA967600D9B29B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CECECE00BEBEBE00B3B3B300B0B0B000B3B3B300BFBFBF00D1D1D1000000
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
      0000000000000000000000000000000000000000000000000000000000009191
      91006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B00929292000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BFBF
      BF00A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A900A9A9A900A9A9A900A9A9A900BFBFBF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000085AD4B0085AD4B0085AD
      4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD
      4B0085AD4B0085AD4B0085AD4B0085AD4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00A9A9A9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000085AD4B007FA942007BA6
      3C007BA63C007BA63C007EA8410085AD4B0085AD4B0085AD4B0085AD4B0080A9
      44007BA63C007BA63C0080A9440085AD4B000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00A9A9A9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3783E00B378
      3E00B3783E00000000000000000000000000000000007FA94200D9E6C800FFFF
      FF00FFFFFF00FFFFFF00D5E2C1007AA53A0085AD4B0085AD4B007DA73F00BCD2
      9C00FFFFFF00FFFFFF00C4D7A80080A944000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00A9A9A9000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AF71
      3400AE6F3100B3783E0000000000000000000000000084AC4A007BA63D00EDF3
      E500FFFFFF00FFFFFF00FFFFFF00BBD29B007DA73F0080A94300A2C17700FFFF
      FF00FFFFFF00DCE7CD0079A53A0085AD4B000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FFFFFF006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FFFFFF00A9A9A9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000AF713400AE6F3100B3783E00000000000000000085AD4B0083AC490082AB
      4700FCFDFC00FFFFFF00FFFFFF00FFFFFF00A0C074008CB15500FFFFFF00FFFF
      FF00F1F5EA007BA73E0084AC4A0085AD4B000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE00A9A9A9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AE703200AE6F3100B3783E000000000085AD4B0085AD4B0082AB
      46008FB45B00FFFFFF00FFFFFF00FFFFFF00FDFDFF00F5F7F000FFFFFF00FFFF
      FF0085AD4C0083AC480085AD4B0085AD4B000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FEFEFE006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FEFEFE00A9A9A9000000000000000000000000000000
      00000000000000000000000000000000000000000000C1906100B0743800AE6F
      3100AE6F3100AE6F3100AE6F3100AE6F3100AF77420085AD4B0085AD4B0085AD
      4B0080A94300A3C27900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0094B8
      620081AA460085AD4B0085AD4B0085AD4B00000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D4D4D400D5D5D500D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE00A9A9A900919191006B6B6B006B6B6B006B6B
      6B00868686000000000000000000BA844F00AE6F3100AE6F3100AE6F3100AE6F
      3100AE6F3100AE6F3100AE6F3100AE6F3100AF77430085AD4B0085AD4B0085AD
      4B0085AD4B007DA73F00B9D09800FFFFFF00FFFFFF00FFFFFF00EAF0E00078A4
      370084AC4A0085AD4B0085AD4B0085AD4B000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200DADADA00C9C9C900DEDEDE00D3D3D300D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FEFEFE006C6C6C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FEFEFE00A9A9A9006F6F6F00FFFFFF00FFFFFF00FFFF
      FF000000000000000000B2773D00AE6F3100AF713500CBA47C00000000000000
      000000000000AE703200AE6F3100B3783D000000000085AD4B0085AD4B0085AD
      4B0085AD4B007DA73F00B7CE9600FFFFFF00FFFFFF00FFFFFF00FFFFFF00D4E3
      C2007AA53A0085AD4B0085AD4B0085AD4B000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200CECECE006B6B6B00AEAEAE00DFDFDF00D4D4D400D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE006C6C6C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE00A9A9A9006F6F6F00FFFFFF00FFFFFF000000
      000000000000B9834E00AE6F3100B57D45000000000000000000000000000000
      0000B47B4200AE6F3100B3783D00000000000000000085AD4B0085AD4B0085AD
      4B0080A94300A2C17700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00BAD099007DA73F0085AD4B0085AD4B0000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200CBCBCB006B6B6B006B6B6B009E9E9E00DFDFDF00D5D5
      D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FEFEFE006C6C6C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FEFEFE00A9A9A9006F6F6F00FFFFFF00FFFFFF000000
      000000000000AE6F3100AF71350000000000000000000000000000000000B67D
      4600AE6F3100B2773D0000000000000000000000000085AD4B0085AD4B0082AB
      46008FB45B00FFFFFF00FFFFFF00F3F6EE0081AA4500FDFDFD00FFFFFF00FFFF
      FF00FFFFFF00A1BF740080A9440085AD4B0000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400CDCDCD006B6B6B006B6B6B006B6B6B008F8F8F00DDDD
      DD00D7D7D700D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE006C6C6C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FEFEFE00A9A9A9006F6F6F00FFFFFF00FFFFFF000000
      0000C08F5F00AE6F3100CBA47C0000000000FFFFFF0000000000B3783E00B378
      3E00B3783E000000000000000000FFFFFF000000000085AD4B0083AC490081AB
      4700FCFDFB00FFFFFF00FFFFFF0087AE4F0080A9440090B55E00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008DB3570082AB470000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500DADADA00E3E3E300E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500DCDCDC006D6D6D006B6B6B006B6B6B006B6B6B008484
      8400D9D9D900DADADA00D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D00FFFFFF00FEFEFE006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00FFFFFF00FEFEFE00A9A9A9006F6F6F00FFFFFF00FFFFFF000000
      0000B0723600AE6F31000000000000000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000084AC4A0081AA4600F6F9
      F300FFFFFF00FFFFFF0098BA690081AA450085AD4B007FA94300A6C27C00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0087AE4F00CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D4D4D400757575006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B007B7B7B00D2D2D200DDDDDD00D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFDFD006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFDFD00A9A9A9006F6F6F00FFFFFF00FFFFFF000000
      0000D4B39300D4B3930000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000085AD4A0084AC4B0080AA
      44007EA840007DA7400082AB470085AD4B0085AD4B0085AD4B0081AA45007DA7
      41007EA840007EA840007FA9420085AC4A00B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D1D1D100777777006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E0076767600C9C9C900E0E0E000D8D8D800D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFDFD006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFDFD00A9A9A9006F6F6F00FFFFFF00FFFFFF000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000085AD4B0085AD4A0084AC
      4A0084AC4A0084AC4A0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0084AC
      4A0084AC4A0084AC4A0084AC4A0084AD4A00B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900D2D2D2007A7A7A00707070007070700070707000707070007070
      7000707070007070700070707000707070007070700070707000707070007070
      7000707070007070700074747400BEBEBE00E1E1E100D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FCFCFC006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABAB
      AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FDFDFD00A9A9A9006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DCDCDC00D4D4D4007E7E7E00747474007474740074747400747474007474
      7400747474007474740074747400747474007474740074747400747474007474
      740074747400747474007474740075757500BFBFBF00DBDBDB00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008D8D8D008D8D8D008D8D8D008D8D
      8D000000000000000000B3783E00B3783E000000000000000000A0A0A0006B6B
      6B006B6B6B006B6B6B006C6C6C006B6B6B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BCBCBC00BCBCBC00BCBCBC00BCBC
      BC000000000000000000AAAAAA00AAAAAA000000000000000000C7C7C700A9A9
      A900A9A9A900A9A9A900A9A9A900A9A9A9006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFE
      FE006C6C6C000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00D5D5D50081818100787878007878780078787800787878007878
      7800787878007878780078787800787878007878780078787800787878007878
      7800787878007878780079797900A8A8A800D2D2D200DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF000000000085AD4B0085AD4B0085AD4B0085AD
      4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0085AD
      4B0085AD4B0085AD4B0085AD4B0000000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000B3783D00AE6F3100AE6F3100B3783E000000000000000000FEFE
      FE00FFFFFF00FEFEFE00ABABAB0084848400BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB0000000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000A9A9A900A4A4A400A4A4A400AAAAAA000000000000000000FEFE
      FE00FFFFFF00FEFEFE00CECECE00B7B7B7006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D00FFFFFF00FEFE
      FE006C6C6C000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00D7D7D700858585007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B0080808000B5B5B500D5D5D500DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE000000000085AD4B007FA942007BA63C007BA6
      3C007BA63C007EA8410085AD4B0085AD4B0085AD4B0085AD4B0080A944007BA6
      3C007BA63C0080A9440085AD4B00000000008D8D8D008D8D8D00000000000000
      0000B3783D00AE6F3100AE6F3100AE6F3100AE6F3100B3783E00000000000000
      0000FFFFFF00AEAEAE007B7B7B0000000000BBBBBB00B8B8B800B5B5B500B5B5
      B500B5B5B500B7B7B700BBBBBB00BBBBBB00BBBBBB00BBBBBB00B8B8B800B5B5
      B500B5B5B500B8B8B800BBBBBB0000000000BCBCBC00BCBCBC00000000000000
      0000A9A9A900A4A4A400A4A4A400A4A4A400A4A4A400AAAAAA00000000000000
      0000FFFFFF00D0D0D000B2B2B200000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFE
      FE006C6C6C000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100D6D6D6008B8B8B00828282008282820082828200828282008282
      82008282820082828200828282007F7F7F007E7E7E007E7E7E007E7E7E007E7E
      7E0088888800BFBFBF00D9D9D900E0E0E000E1E1E100E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA00000000007FA94200D9E6C800FFFFFF00FFFF
      FF00FFFFFF00D5E2C1007AA53A0085AD4B0085AD4B007DA73F00BCD29C00FFFF
      FF00FFFFFF00C4D7A80080A9440000000000000000000000000000000000B277
      3D00AE6F3100AE703200AE6F3100AE6F3100AE703200AE6F3100B3783E000000
      000000000000787878000000000000000000B8B8B800EAEAEA00FFFFFF00FFFF
      FF00FFFFFF00E7E7E700B5B5B500BBBBBB00BBBBBB00B7B7B700D9D9D900FFFF
      FF00FFFFFF00DDDDDD00B8B8B80000000000000000000000000000000000A9A9
      A900A4A4A400A4A4A400A4A4A400A4A4A400A4A4A400A4A4A400AAAAAA000000
      000000000000B1B1B10000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D00FFFFFF00FEFE
      FE006B6B6B000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300DDDDDD00D4D4D400D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200CECECE00868686008282820082828200828282009292
      9200C9C9C900DDDDDD00E2E2E200E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C700000000000000000084AC4A007BA63D00EDF3E500FFFF
      FF00FFFFFF00FFFFFF00BBD29B007DA73F0080A94300A2C17700FFFFFF00FFFF
      FF00DCE7CD0079A53A0085AD4B00000000000000000000000000B3783E00AE6F
      3100B47B420000000000AE6F3100AE6F310000000000AF713400AE6F3100B378
      3E0000000000000000000000000000000000BBBBBB00B5B5B500F5F5F500FFFF
      FF00FFFFFF00FFFFFF00D9D9D900B7B7B700B8B8B800CBCBCB00FFFFFF00FFFF
      FF00EBEBEB00B4B4B400BBBBBB00000000000000000000000000AAAAAA00A4A4
      A400ABABAB0000000000A4A4A400A4A4A40000000000A5A5A500A4A4A400AAAA
      AA00000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD006B6B6B000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E4E4E400DBDBDB008A8A8A0085858500858585009D9D9D00D0D0
      D000E0E0E000E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B500000000000000000085AD4B0083AC490082AB4700FCFD
      FC00FFFFFF00FFFFFF00FFFFFF00A0C074008CB15500FFFFFF00FFFFFF00F1F5
      EA007BA73E0084AC4A0085AD4B00000000000000000000000000B3783E00B67D
      46000000000000000000AE6F3100AE6F31000000000000000000AF713400B378
      3E0000000000000000000000000000000000BBBBBB00BABABA00B9B9B900FDFD
      FD00FFFFFF00FFFFFF00FFFFFF00CACACA00BFBFBF00FFFFFF00FFFFFF00F6F6
      F600B6B6B600BBBBBB00BBBBBB00000000000000000000000000AAAAAA00ADAD
      AD000000000000000000A4A4A400A4A4A4000000000000000000A5A5A500AAAA
      AA00000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D00FFFFFF00FDFD
      FD006B6B6B000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500DCDCDC008C8C8C0088888800AAAAAA00D5D5D500E3E3
      E300E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B700000000000000000085AD4B0085AD4B0082AB46008FB4
      5B00FFFFFF00FFFFFF00FFFFFF00FDFDFF00F5F7F000FFFFFF00FFFFFF0085AD
      4C0083AC480085AD4B0085AD4B00000000000000000000000000B3783E000000
      00000000000000000000AE6F3100B0743800000000000000000000000000B378
      3E0000000000000000000000000000000000BBBBBB00BBBBBB00B9B9B900C1C1
      C100FFFFFF00FFFFFF00FFFFFF00FDFDFD00F8F8F800FFFFFF00FFFFFF00BBBB
      BB00BABABA00BBBBBB00BBBBBB00000000000000000000000000AAAAAA000000
      00000000000000000000A4A4A400A7A7A700000000000000000000000000AAAA
      AA00000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD006B6B6B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600DBDBDB0090909000B5B5B500D9D9D900E5E5E500E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA0000000000000000000000000085AD4B0085AD4B0085AD4B0080A9
      4300A3C27900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0094B8620081AA
      460085AD4B0085AD4B0085AD4B00000000000000000000000000000000000000
      000000000000CBA47C00AE6F3100C19061000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00B8B8
      B800CCCCCC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C4C4C400B9B9
      B900BBBBBB00BBBBBB00BBBBBB00000000000000000000000000000000000000
      000000000000C5C5C500A4A4A400B9B9B9000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFD
      FD006B6B6B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600DBDBDB00C3C3C300DBDBDB00E5E5E500E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C30000000000000000000000000085AD4B0085AD4B0085AD4B0085AD
      4B007DA73F00B9D09800FFFFFF00FFFFFF00FFFFFF00EAF0E00078A4370084AC
      4A0085AD4B0085AD4B0085AD4B00000000000000000000000000000000000000
      000000000000AF713500AE6F3100000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00B7B7B700D8D8D800FFFFFF00FFFFFF00FFFFFF00F2F2F200B4B4B400BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00000000000000000000000000000000000000
      000000000000A5A5A500A4A4A400000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FCFC
      FC006B6B6B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E5E5E500E4E4E400E5E5E500E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B7000000000000000000000000000000000085AD4B0085AD4B0085AD4B0085AD
      4B007DA73F00B7CE9600FFFFFF00FFFFFF00FFFFFF00FFFFFF00D4E3C2007AA5
      3A0085AD4B0085AD4B0085AD4B00000000000000000000000000000000000000
      0000B57D4500AE6F3100BA844F00000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00B7B7B700D6D6D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7E7E700B5B5
      B500BBBBBB00BBBBBB00BBBBBB00000000000000000000000000000000000000
      0000ACACAC00A4A4A400B1B1B100000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D00FFFFFF00FFFFFF00949494006B6B6B006B6B6B006B6B6B006B6B6B006C6C
      6C006B6B6B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      00000000000000000000000000000000000085AD4B0085AD4B0085AD4B0080A9
      4300A2C17700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BAD0
      99007DA73F0085AD4B0085AD4B00000000000000000000000000CBA47C00AF71
      3500AE6F3100B2773D0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00B8B8
      B800CBCBCB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D8D8
      D800B7B7B700BBBBBB00BBBBBB00000000000000000000000000C5C5C500A5A5
      A500A4A4A400A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006B6B6B00FEFEFE00FFFFFF00FFFFFF00FEFEFE00ABAB
      AB00848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      00000000000000000000000000000000000085AD4B0085AD4B0082AB46008FB4
      5B00FFFFFF00FFFFFF00F3F6EE0081AA4500FDFDFD00FFFFFF00FFFFFF00FFFF
      FF00A1BF740080A9440085AD4B000000000000000000AE6F3100AE6F3100AE6F
      3100B9834E000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00B9B9B900C1C1
      C100FFFFFF00FFFFFF00F8F8F800B9B9B900FDFDFD00FFFFFF00FFFFFF00FFFF
      FF00CACACA00B8B8B800BBBBBB000000000000000000A4A4A400A4A4A400A4A4
      A400B1B1B1000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF008D8D
      8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D00FFFFFF00FFFFFF006B6B6B00FEFEFE00FFFFFF00FEFEFE00AEAEAE007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      00000000000000000000000000000000000085AD4B0083AC490081AB4700FCFD
      FB00FFFFFF00FFFFFF0087AE4F0080A9440090B55E00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008DB3570082AB47000000000000000000B0723600C08F5F000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BABABA00B9B9B900FDFD
      FD00FFFFFF00FFFFFF00BCBCBC00B8B8B800C2C2C200FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C0C0C000B9B9B9000000000000000000A6A6A600B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006B6B6B00FEFEFE00FFFFFF00B2B2B200787878000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      00000000000000000000000000000000000084AC4A0081AA4600F6F9F300FFFF
      FF00FFFFFF0098BA690081AA450085AD4B007FA94300A6C27C00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0087AE4F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00B9B9B900F9F9F900FFFF
      FF00FFFFFF00C6C6C600B9B9B900BBBBBB00B8B8B800CDCDCD00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BCBCBC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006B6B6B00FEFEFE00B7B7B70074747400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000085AD4A0084AC4B0080AA44007EA8
      40007DA7400082AB470085AD4B0085AD4B0085AD4B0081AA45007DA741007EA8
      40007EA840007FA9420085AC4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00B8B8B800B7B7
      B700B7B7B700B9B9B900BBBBBB00BBBBBB00BBBBBB00B9B9B900B7B7B700B7B7
      B700B7B7B700B8B8B800BBBBBB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006C6C6C00B9B9B9007373730000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000085AD4B0085AD4A0084AC4A0084AC
      4A0084AC4A0085AD4B0085AD4B0085AD4B0085AD4B0085AD4B0084AC4A0084AC
      4A0084AC4A0084AC4A0084AD4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000919191006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B00787878000000000000000000000000000000
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
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000053705D00326446001F6F48001A7248001E6E4700316346004E6C59007D85
      7D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000053705D00326446001F6F48001A7248001E6E4700316346004E6C59007D85
      7D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D3D3D300D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      000000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D4D4D400E0E0E000CECECE00DCDCDC00D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A300000000000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB003AD0AB0044D3AF003ED2AD0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      500000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D5D5D500D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A300000000000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB003ED2AD0044D3AF003AD0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      5000000000000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200DBDBDB00A4A4A4006B6B6B00BEBEBE00D9D9D900D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE00000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003CD0AC006ADDC00074ADA0005AD8B90039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D3D3D300DEDEDE00C9C9C900DADADA00D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE00000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB005AD8B90074ADA0006ADDC0003CD0AC0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200DADADA00B5B5B5006B6B6B006B6B6B006B6B6B00C9C9C900D8D8D800D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD2
      AD006FDEC200458171000000000035C6A30039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C600179266000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4
      D400DFDFDF00AEAEAE006B6B6B00CECECE00D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0035C6A30000000000458171006FDEC2003FD2AD0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C6001792660000000000000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D8D8
      D800C5C5C5006B6B6B006B6B6B006B6B6B006B6B6B0072727200D3D3D300D6D6
      D600D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD0000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0045D3AF0073DE
      C30031625500000000000000000033C19D0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D5D5D500DFDF
      DF009E9E9E006B6B6B006B6B6B00CBCBCB00D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD0000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0033C19D0000000000000000003162550073DEC30045D3
      AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D8D8D800D0D0
      D000707070006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B007E7E7E00D9D9
      D900D6D6D600D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB00000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC004BD6B40076D9C1002245
      3D0000000000000000000001000037C5A2003CD2AD003CD2AD003CD2AD003CD2
      AD003CD2AD003CD2AD003CD2AD003BD2AD003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D7D7D700DDDDDD008F8F
      8F006B6B6B006B6B6B006B6B6B00CDCDCD00D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB00000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003BD2AD003CD2AD003CD2AD003CD2AD003CD2AD003CD2
      AD003CD2AD003CD2AD0037C5A20000010000000000000000000022463D0076D9
      C1004BD6B4003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D7D7D700D9D9D9007E7E
      7E006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B008E8E
      8E00DEDEDE00D6D6D600D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C30000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF0055D9BA0073D0BA00183029000000
      000000000000000000000006030076D6BF009BE0D0009BE0D0009BE0D0009BE0
      D0009BE0D0009BE0D0009BE0D0008DDECB0056D9BA003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500DADADA00D9D9D900848484006B6B
      6B006B6B6B006B6B6B006D6D6D00DCDCDC00E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500E5E5E500E3E3E300DADADA00D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C30000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF0056D9BA008DDECB009BE0D0009BE0D0009BE0D0009BE0D0009BE0
      D0009BE0D0009BE0D00076D6BF00000603000000000000000000000000001830
      290073D0BA0055D9BA003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B000000000000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D8D8D800DFDFDF008D8D8D006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B009F9F9F00DFDFDF00D7D7D700D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B400000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B3003ED5B30040D5B3005FDDC0006EC3AF0013201C00010000000100
      0000010000000100000001000000010000000100000001000000010000000100
      00000100000001000000010000000815110040D0B0003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D6D6D600D6D6D600DDDDDD00D2D2D2007B7B7B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B
      6B006B6B6B006B6B6B006B6B6B0075757500D4D4D400D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B400000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B30040D0B00008151100010000000100000001000000010000000100
      0000010000000100000001000000010000000100000001000000010000000100
      000013201C006EC3AF005FDDC00040D5B3003ED5B3003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800E0E0E0009F9F9F006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E00B1B1B100E0E0E000D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA0000000000429A780058E8C20047DCB90041D7
      B70041D7B70044D8B8006AE1C80066B1A1001116140008060400080604000806
      0400080604000806040008060400080604000806040008060400080604000806
      04000806040008060400080604000C1815003CCAAB0041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D8D8D800E0E0E000C9C9C900767676006E6E6E006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E0077777700D1D1D100D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA0000000000429A780058E8C20047DCB90041D7
      B70041D7B7003CCAAB000C181500080604000806040008060400080604000806
      0400080604000806040008060400080604000806040008060400080604000806
      0400080604001116140066B1A1006AE1C80044D8B80041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900D9D9D900D9D9D900E0E0E000B2B2B20070707000707070007070
      7000707070007070700070707000707070007070700070707000707070007070
      70007070700072727200C0C0C000DFDFDF00D9D9D900D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC000000000028916A0058E9C30047DCBB0045D9
      BA0047DABB0069E2C900599B8D00111210000D0B0A000D0B0A000D0B0A000D0B
      0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B
      0A000D0B0A000D0B0A000D0B0A00121E1A003FCCAE0045D9BA0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900E1E1E100BEBEBE00747474007070700070707000707070007070
      7000707070007070700070707000707070007070700070707000707070007070
      70007070700070707000707070007A7A7A00D2D2D200D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC000000000028916A0058E9C30047DCBB0045D9
      BA0045D9BA003FCCAE00121E1A000D0B0A000D0B0A000D0B0A000D0B0A000D0B
      0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B0A000D0B
      0A000D0B0A000D0B0A0011121000599B8D0069E2C90047DABB0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DCDCDC00DCDCDC00DCDCDC00BCBCBC007575750074747400747474007474
      7400747474007474740074747400747474007474740074747400747474007474
      7400747474007474740079797900C6C6C600DCDCDC00DCDCDC00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD000000000020936A0058E8C40049DDBF0048DD
      BF0049DCBE003AA48F0014131100141210001412100014121000141210001412
      1000141210001412100014121000141210001412100014121000141210001412
      10001412100014121000141210001825200043CEB30048DDBF0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DBDBDB00BFBFBF0075757500747474007474740074747400747474007474
      7400747474007474740074747400747474007474740074747400747474007474
      74007474740074747400747474007E7E7E00D4D4D400DCDCDC00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD000000000020936A0058E8C40049DDBF0048DD
      BF0048DDBF0043CEB30018252000141210001412100014121000141210001412
      1000141210001412100014121000141210001412100014121000141210001412
      1000141210001412100014121000141311003AA48F0049DCBE0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DCDCDC00D1D1D100CDCDCD00CCCCCC00CCCCCC00C7C7
      C7007777770077777700777777007777770077777700777777007B7B7B00CACA
      CA00CCCCCC00CCCCCC00CDCDCD00D2D2D200DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF000000000028956D0056E8C6004DE0C4004BDF
      C4004BDEC30043C9B0002F7769001B1B17001B1816001B1816001B1816001B18
      16001B1816001B1816001B1816001B1816001B1816001B1816001B1816001B18
      16001B1816001B1816001B1816001F2A260046D0B7004BDFC4004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00D2D2D200A8A8A800797979007878780078787800787878007878
      7800787878007878780078787800787878007878780078787800787878007878
      780078787800787878007878780081818100D5D5D500DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF000000000028956D0056E8C6004DE0C4004BDF
      C4004BDFC40046D0B7001F2A26001B1816001B1816001B1816001B1816001B18
      16001B1816001B1816001B1816001B1816001B1816001B1816001B1816001B18
      16001B1816001B1816001B1B17002F77680043C9B0004BDEC3004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00D3D3
      D3007A7A7A007A7A7A007A7A7A007A7A7A007A7A7A007A7A7A007E7E7E00D6D6
      D600DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C9004FE0C80047CEB700368F800022272300211E1C00211E1C00211E
      1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E
      1C00211E1C00211E1C00211E1C0025302B0048D3BB004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00DFDFDF00D5D5D500B5B5B500808080007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B0085858500D7D7D700DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C90048D3BB0025302B00211E1C00211E1C00211E1C00211E1C00211E
      1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E
      1C00211E1C0022272300368F800047CEB7004FE0C8004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E0E0E000D5D5
      D5007D7D7D007D7D7D007D7D7D007D7D7D007D7D7D007D7D7D0081818100D8D8
      D800E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA00000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0052E4CC0052E3CC004BD5BF003DA492002A363200272321002723
      2100272321002723210027252100292B2800292B2800292B2800292B2800292B
      2800292B2800292B2800292B28002B3B350049CFB90052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B3006893800000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100E1E1E100E0E0E000D9D9D900BFBFBF00888888007E7E7E007E7E
      7E007E7E7E007E7E7E007F7F7F00828282008282820082828200828282008282
      82008282820082828200828282008B8B8B00D6D6D600E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA00000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0049CFB9002B3B3500292B2800292B2800292B2800292B2800292B
      2800292B2800292B2800292B2800272521002723210027232100272321002723
      21002A3632003DA492004BD5BF0052E3CC0052E4CC0052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B300689380000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E2E2E200D6D6
      D60080808000808080008080800080808000808080008080800084848400DADA
      DA00E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C7000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00055E7D00055E7D00054E6D00051DCC60044B5A300324944002D29
      27002D2927002D2927002F322F0045C0AE0047C6B40047C6B40047C6B40047C6
      B40047C6B40047C6B40047C6B40049CAB70051DDC80055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300E3E3E300E3E3E300E2E2E200DDDDDD00C9C9C900929292008282
      8200828282008282820086868600CECECE00D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DDDDDD00E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C7000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00051DDC80049CAB70047C6B40047C6B40047C6B40047C6B40047C6
      B40047C6B40047C6B40045C0AE002F322F002D2927002D2927002D2927003249
      440044B5A30051DCC60054E6D00055E7D00055E7D00055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400D8D8
      D80082828200828282008282820082828200828282008282820086868600DBDB
      DB00E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B500000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40054E1CD0048C2B000395E
      5600322F2B00322F2B003439340050D7C30058E8D40058E8D40058E8D40058E8
      D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E0E0E000D0D0D0009D9D
      9D0085858500858585008A8A8A00DBDBDB00E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B500000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8
      D40058E8D40058E8D40050D7C30034393400322F2B00322F2B00395E560048C2
      B00054E1CD0058E8D40058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E4E4E400D9D9
      D90085858500858585008585850085858500858585008585850089898900DCDC
      DC00E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B7000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70058E6D3004DCB
      BA003F766D0037333000393C390052D9C8005AEAD7005AEAD7005AEAD7005AEA
      D7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E3E3E300D5D5
      D500AAAAAA00888888008C8C8C00DCDCDC00E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B7000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEA
      D7005AEAD7005AEAD70052D9C800393C3900373330003F766D004DCBBA0058E6
      D3005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C0000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E5E5E500D9D9
      D9008888880088888800888888008888880088888800888888008B8B8B00DDDD
      DD00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA00000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005AE9
      D60050D2C100448D82003C433E0052D7C5005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E5E5
      E500D9D9D900B5B5B50090909000DBDBDB00E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA00000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80052D7C5003C433E00448D820050D2C1005AE9D6005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E5E5E500D9D9
      D9008989890089898900898989008989890089898900898989008D8D8D00DCDC
      DC00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C300000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005AEAD70052D7C60048A99B0052D7C5005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E5E5E500DBDBDB00C3C3C300DBDBDB00E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C300000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80052D7C50048A99B0052D7C6005AEAD7005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A79100000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600D6D6
      D6008F8F8F008F8F8F008F8F8F008F8F8F008F8F8F008F8F8F0091919100D9D9
      D900E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B7000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005AEAD70059E8D5005AE9D6005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B0000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E5E5E500E4E4E400E5E5E500E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B7000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005AE9D60059E8D5005AEAD7005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B00000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600DFDF
      DF00D5D5D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D6D6D600E0E0
      E000E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
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
      0000A3A3A3009494940091919100919191009090900094949400A0A0A000B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000053705D00326446001F6F48001A7248001E6E4700316346004E6C59007D85
      7D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B6B6
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000053705D00326446001F6F48001A7248001E6E4700316346004E6C59007D85
      7D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A6A6A60092929200A3A3
      A300B2B2B200BCBCBC00BEBEBE00BEBEBE00BEBEBE00BCBCBC00B2B2B200A3A3
      A30092929200A3A3A30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000ABABAB0094949400B3B3B300BEBEBE00BABA
      BA00B5B5B500B1B1B100B0B0B000B0B0B000B0B0B000B1B1B100B5B5B500BABA
      BA00BEBEBE00B3B3B30094949400A6A6A6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000093939300ADADAD00BEBEBE00B7B7B700AEAEAE00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00AEAEAE00B7B7B700BEBEBE00ADADAD009191910000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000091919100B8B8B800BBBBBB00AFAFAF00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00AFAFAF00BBBBBB00B8B8B80091919100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000092929200BBBBBB00B8B8B800ACACAC00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ACACAC00B8B8B800BBBBBB00919191000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB003BD0AC003FD2AD003AD0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000009494
      9400B8B8B800B8B8B800ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00B8B8B800B8B8B8009292
      9200000000000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0058D8
      B8008EE6D00099E9D50099E9D50099E9D50099E9D50099E9D5008CE6CF0054D7
      B70039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      500000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D9D9
      D900E6E6E600E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E6E6E600D9D9
      D900D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A300000000000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB003DD2AC0073DFC40079B8A90063DABC003AD0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      5000000000000000000000000000000000000000000000000000B4B4B400AEAE
      AE00BBBBBB00ACACAC00ABABAB00ABABAB00ABABAB00ABABAB00A5A5A500A9A9
      A900ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00A9A9
      A900A5A5A500ABABAB00ABABAB00ABABAB00ABABAB00ACACAC00BBBBBB00ADAD
      AD00A7A7A700000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003CC8
      A400000000000000000000000000000000000000000000000000000100003DCD
      AA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200CFCF
      CF006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE00000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003BD0AB0060DABB00307060000000000049A08B0056D8B80039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A00000000000000000000000000000000000000000096969600BEBE
      BE00AEAEAE00ABABAB00ABABAB00ABABAB00ABABAB00A6A6A600D4D4D400BBBB
      BB00AAAAAA00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00C0C0
      C000D2D2D200A6A6A600ABABAB00ABABAB00ABABAB00ABABAB00AEAEAE00BEBE
      BE00959595000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0037CFAA0031BA
      98000000000000000000000000000000000000000000000000000000000033C1
      9F0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C600179266000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200C7C7
      C7006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00CBCB
      CB00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB005AD8B9003F917D0000000000000000000000000054B69D0050D6B50039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C6001792660000000000000000000000000000000000B1B1B100B4B4B400B6B6
      B600ABABAB00ABABAB00ABABAB00ABABAB00A6A6A600D4D4D400FBFBFB00FBFB
      FB00BCBCBC00ABABAB00ABABAB00ABABAB00ABABAB00ABABAB00C0C0C000FBFB
      FB00FBFBFB00D3D3D300A6A6A600ABABAB00ABABAB00ABABAB00ABABAB00B6B6
      B600B3B3B300A4A4A40000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0037CFAA0031B9
      98000000000000000000000000000000000000000000000000000000000033C1
      9D0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200C7C7
      C7006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00CBCB
      CB00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD0000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0052D7
      B6004FAE96000000000000000000000000000000000001110C005DC8AE004AD5
      B30039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F670000000000000000000000000094949400BDBDBD00AEAE
      AE00ADADAD00ADADAD00ADADAD00A5A5A500D4D4D400FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00BCBCBC00ACACAC00ADADAD00ACACAC00C1C1C100FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00D4D4D400A7A7A700ADADAD00ADADAD00ADADAD00AEAE
      AE00BDBDBD0093939300000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD0AC0032BB
      99000000000000000000000000000000000000000000000000000000000034C2
      A0003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D3D3D300C8C8
      C8006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00CBCB
      CB00D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB00000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC004ED6B5005BC3
      AA00020D0A0000000000000000000000000000000000000000000E28210064D5
      B90046D5B1003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000A5A5A500B9B9B900AEAE
      AE00AEAEAE00AEAEAE00AEAEAE00A6A6A600D5D5D500FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00BCBCBC00A8A8A800C1C1C100FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00C2C2C200ACACAC00AEAEAE00AEAEAE00AEAEAE00AEAE
      AE00B9B9B900A4A4A40000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF0034BC
      9C000000000000000000000000000000000000000000000000000001000036C3
      A2003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500C9C9
      C9006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B00CDCD
      CD00D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C30000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF0048D6B50064D4B9001026
      1F00000000000000000000000000000000000000000000000000000000001D45
      3A0068DDC10043D5B3003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B000000000000000000BCBCBC00B3B3B300B5B5B500B1B1
      B100B1B1B100B1B1B100B1B1B100B1B1B100AAAAAA00D6D6D600FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00E3E3E300FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00C2C2C200B0B0B000B1B1B100B1B1B100B1B1B100B1B1B100B1B1
      B100B6B6B600B2B2B200AEAEAE00000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B30036BF
      A0000000000000000000000000000000000000000000000000000107040039C5
      A5003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600CBCB
      CB006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006E6E6E00CECE
      CE00D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B400000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B3003ED5B3003ED5B3003ED5B30046D7B60069DDC3001F433A000100
      0000010000000100000001000000010000000100000001000000010000000100
      00002F64580068DFC40043D6B4003ED5B3003ED5B3003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000A5A5A500BBBBBB00B5B5B500B4B4
      B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400ACACAC00D6D6D600FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00C3C3C300B2B2B200B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4
      B400B5B5B500BBBBBB009C9C9C0000000000429A780058E8C20047DCB90041D7
      B70041D7B70041D7B70041D7B70041D7B70041D7B70041D7B70041D7B7003AC1
      A300040201000402010004020100040201000402010004020100070B09003CC8
      AA0041D7B70041D7B70041D7B70041D7B70041D7B70041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800CCCC
      CC006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B006B6B6B0070707000D0D0
      D000D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA0000000000429A780058E8C20047DCB90041D7
      B70041D7B70041D7B70041D7B70045D8B8006AE0C80031655900070604000706
      0400070604000706040007060400070604000706040007060400070604000706
      0400070604004086770067E0C50043D7B70041D7B70041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D6000000000009A9A9A00BCBCBC00B7B7B700B7B7
      B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700AEAEAE00D7D7
      D700FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00C3C3
      C300B5B5B500B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7
      B700B7B7B700BCBCBC00979797000000000028916A0058E9C30047DCBB0045D9
      BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA003DC4
      A7000A0807000A0807000A0807000A0807000A0807000A0807000C110D0040CC
      AE0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900CECE
      CE006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F0073737300D2D2
      D200D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC000000000028916A0058E9C30047DCBB0045D9
      BA0045D9BA0045D9BA0046DABB0066E1C800448879000D0B09000D0B09000D0B
      09000D0B09000D0B09000D0B09000D0B09000D0B09000D0B09000D0B09000D0B
      09000D0B09000E0D0C0051A3910062E0C50046D9BA0045D9BA0045D9BA0045D9
      BA0047DCBB0058E9C300258A64000000000098989800BCBCBC00BABABA00BABA
      BA00BABABA00BABABA00BABABA00BABABA00BABABA00BABABA00BABABA00ABAB
      AB00F6F6F600FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00E4E4E400B0B0
      B000BABABA00BABABA00BABABA00BABABA00BABABA00BABABA00BABABA00BABA
      BA00BABABA00BCBCBC00969696000000000020936A0058E8C40049DDBF0048DD
      BF0048DDBF0048DDBF004FDEC1007DE8D300A4EFE100A6F1E100A6F1E10077DF
      CA00100D0C00100D0C00100D0C00100D0C00100D0C00100D0C001316140080E7
      D300A6F1E100A6F1E100A3EFE00077E6CF004DDDC00048DDBF0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DCDCDC00DCDCDC00DDDDDD00E6E6E600EEEEEE00EEEEEE00EEEEEE00E1E1
      E10072727200727272007272720072727200727272007272720077777700E6E6
      E600EEEEEE00EEEEEE00EDEDED00E4E4E400DCDCDC00DCDCDC00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD000000000020936A0058E8C40049DDBF0048DD
      BF0048DDBF0048DDBF004BDCC0003A9F89001313110013110E0013110E001311
      0E0013110E0013110E0013110E0013110E0013110E0013110E0013110E001311
      0E0013110E0013110E00151A16003FB19A004ADDBF0048DDBF0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D6600000000009B9B9B00BEBEBE00BDBDBD00BEBE
      BE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00B9B9B900C5C5
      C500FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00C0C0
      C000B9B9B900BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBE
      BE00BDBDBD00BEBEBE00989898000000000028956D0056E8C6004DE0C4004BDF
      C4004BDFC4004BDFC4004ADDC200369684001615130015131200151312001513
      1200151312001513120015131200151312001513120015131200151312001513
      12001513120015131200171B18003BA995004BDEC3004BDFC4004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DCDCDC00B8B8B8007676760075757500757575007575
      7500757575007575750075757500757575007575750075757500757575007575
      7500757575007575750079797900C2C2C200DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF000000000028956D0056E8C6004DE0C4004BDF
      C4004BDFC4004BDFC4004ADDC20043C8AF003EBFA7003EBEA6003EBEA6003BB4
      9F001816140018161400181614001816140018161400181614001A1D1B003DB9
      A3003EBEA6003EBEA6003FBFA70044CAB1004ADEC3004BDFC4004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000A6A6A600BEBEBE00C1C1C100C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000BCBCBC00C5C5C500FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00C0C0C000BCBCBC00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C1C1C100BEBEBE009D9D9D000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C9004FE2C9004FE1C90047CDB700307769001C1816001C1816001C18
      16001C1816001C1816001C1816001C1816001C1816001C1816001C1816001C18
      16001C1816001C1B1800348B7B0048D2BA004FE1C9004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00D5D5D500A8A8A80078787800787878007878
      7800787878007878780078787800787878007878780078787800787878007878
      7800787878007A7A7A00B3B3B300D7D7D700DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE1C90046CA
      B4001E1C1A001E1C1A001E1C1A001E1C1A001E1C1A001E1C1A002022200048D0
      B9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BDBDBD00B6B6B600C3C3C300C3C3
      C300C3C3C300C3C3C300C3C3C300C3C3C300BFBFBF00C6C6C600FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00F8F8F800FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00C1C1C100BFBFBF00C3C3C300C3C3C300C3C3C300C3C3C300C3C3
      C300C3C3C300B5B5B500B0B0B000000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0052E4CC0052E4CC0051E2CB0047CBB5002F635900211E1C00211E
      1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E1C00211E
      1C00211E1C00347A6E0049CFB90052E3CB0052E4CC0052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B3006893800000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100E1E1E100E1E1E100E0E0E000D4D4D4009F9F9F007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B00AAAAAA00D6D6D600E0E0E000E1E1E100E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA00000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E3CC0049CD
      B70023201E0023201E0023201E0023201E0023201E0023201E00262825004BD4
      BE0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B300689380000000000000000000A8A8A800C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600BFBFBF00C6C6C600FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00C7C7C700B3B3B300D9D9D900FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00C2C2C200C0C0C000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600A8A8A8000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00055E7D00055E7D00055E7D00054E3CE0048C5B4002F514A002723
      2000272320002723200027232000272320002723200027232000272320002723
      200033685E004ACDB90054E4CF0055E7D00055E7D00055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E1E1E100D1D1D100969696007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E00A1A1A100D5D5D500E2E2E200E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C7000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00054E6D0004DCF
      BB002926220029262200292622002926220029262200292622002A2D29004ED6
      C20055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7
      D0005BE9D0002BBA970000000000000000000000000096969600C6C6C600C9C9
      C900C9C9C900C9C9C900C9C9C900B3B3B300ECECEC00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00C7C7C700C4C4C400C9C9C900BBBBBB00D9D9D900FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00D9D9D900BBBBBB00C9C9C900C9C9C900C9C9C900C9C9
      C900C6C6C60096969600000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40055E3CF0047BFAD003145
      3F002C2826002C2826002C2826002C2826002C2826002C2826002C2826003459
      51004AC9B70056E6D20058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E1E1E100CECECE009090
      9000818181008181810081818100818181008181810081818100818181009A9A
      9A00D4D4D400E3E3E300E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B500000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D3004FD2
      BF002F2A28002F2A28002F2A28002F2A28002F2A28002F2A280030322F0050D7
      C40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B4B4B400BEBEBE00CBCB
      CB00CACACA00CACACA00CACACA00CACACA00B4B4B400ECECEC00FEFEFE00FEFE
      FE00C7C7C700C5C5C500CACACA00CACACA00CACACA00BCBCBC00DADADA00FEFE
      FE00FEFEFE00DADADA00BCBCBC00CACACA00CACACA00CACACA00CACACA00CBCB
      CB00BCBCBC00A9A9A9000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70055E1CF0047B4
      A500333C3700312C2A00312C2A00312C2A00312C2A00312C2A00354D47004AC2
      B30058E6D3005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E0E0E000C9C9
      C9008C8C8C00848484008484840084848400848484008484840094949400D0D0
      D000E3E3E300E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B7000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70059E9D60050D3
      C100332F2B00332F2B00332F2B00332F2B00332F2B00332F2B003436320052D9
      C8005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000000000009C9C9C00CBCB
      CB00CDCDCD00CBCBCB00CBCBCB00CBCBCB00CBCBCB00B6B6B600ECECEC00C9C9
      C900C6C6C600CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00BCBCBC00DBDB
      DB00DBDBDB00BDBDBD00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CDCDCD00CBCB
      CB009B9B9B00000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEAD70055DF
      CD0045A698003637330035312D0035312D0035312D0037443E0048B9AA0058E3
      D2005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600DFDF
      DF00C2C2C2008989890086868600868686008686860090909000CBCBCB00E2E2
      E200E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA00000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005AEAD70051D4
      C200373330003733300037333000373330003733300037333000393A360053DA
      C9005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000B8B8B800B9B9
      B900CECECE00CCCCCC00CBCBCB00CBCBCB00CBCBCB00CBCBCB00B8B8B800C5C5
      C500CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00BBBB
      BB00BBBBBB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CCCCCC00CECECE00B8B8
      B800B2B2B200000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005AEA
      D70053D9C9004496890039353200393431003A3E3A0047AD9F0055E0CE005BEA
      D7005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E5E5
      E500DCDCDC00B9B9B90089898900888888008D8D8D00C6C6C600E0E0E000E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C300000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005AEAD70051D4
      C2003B3633003B3633003B3633003B3633003B3633003B3633003C3D390053D9
      C8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000000000009C9C
      9C00C7C7C700D0D0D000CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00D0D0D000C7C7C7009A9A
      9A000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005AEAD70051D5C300438379003D3B370046A0920054DCCA005AEAD7005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B0000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E5E5E500DADADA00B0B0B0008C8C8C00BFBFBF00DEDEDE00E5E5E500E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B7000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEAD7004FCD
      BC003E403C003E403C003E403C003E403C003E403C003E403C003E46400050D4
      C2005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B00000000000000000000000000000000000000000000000000000000000000
      000099999900CCCCCC00D0D0D000CCCCCC00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CCCCCC00D0D0D000CCCCCC00989898000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005AE9D60050D2C10048AB9D0052D8C6005AEAD7005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E5E5E500D9D9D900C5C5C500DCDCDC00E5E5E500E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEAD70055DE
      CC004ECCBB004DCAB9004DCAB9004DCAB9004DCAB9004DCAB9004ECDBB0055E0
      CD005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000099999900CBCBCB00D2D2D200CECECE00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CECECE00D2D2D200CACACA0098989800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005AEAD7005AE8D5005AEAD7005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E5E5E500E4E4E400E5E5E500E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009D9D9D00BEBEBE00D4D4D400D2D2D200CDCDCD00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CDCDCD00D2D2D200D4D4D400BEBEBE009D9D9D0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BABABA009F9F9F00C7C7C700D6D6D600D4D4
      D400D1D1D100D0D0D000CECECE00CECECE00CECECE00D0D0D000D1D1D100D4D4
      D400D6D6D600C7C7C7009F9F9F00BABABA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B7B7B7009C9C9C00B3B3
      B300C7C7C700D4D4D400D7D7D700D7D7D700D7D7D700D4D4D400C7C7C700B3B3
      B3009C9C9C00B8B8B80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2C2C200ADADAD009F9F9F009E9E9E009F9F9F00ADADAD00C2C2C2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
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
      0000000000000000000000000000000000000000000000000000A6A6A600A8A8
      A800B3B3B3000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000526F5C00326446001F6F48001A7248001E6E4700316346004E6C59007A82
      7A00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6A6A6009C9C9C009F9F9F009F9F9F009D9D9D009B9B9B00A4A4A400B4B4
      B400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000056567B0038386F002C2C79002A2A7C002B2B780037376F00515176007E7E
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A700BFBFBF00CECE
      CE00B3B3B300AFAFAF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000567D65001581540030AD
      82004AD2A9005BE9C2005FEEC8005FEEC8005FEEC8005BE9C2004AD2A90030AD
      82001580540050775F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00A6A6A600BFBF
      BF00D4D4D400E2E2E200E4E4E400E4E4E400E4E4E400E2E2E200D4D4D400BFBF
      BF00A6A6A600A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A5A870027278A003F3F
      B1005757D1006666E6006A6AEB006A6AEB006A6AEB006666E6005757D1003F3F
      B100272789005454820000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A9A9A900BFBFBF00D6D6D600D0D0
      D000D3D3D300B5B5B500BDBDBD00000000000000000000000000000000000000
      00000000000000000000000000005E846D00188D5F004DD5AC005FEEC80058E8
      C2004DE0B90045D9B40041D7B10041D7B10041D7B10045D9B4004DE0B90058E8
      C2005FEEC8004DD5AC00188D5F00557B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000ACACAC00D6D6D600E4E4E400E1E1
      E100DCDCDC00D8D8D800D6D6D600D6D6D600D6D6D600D8D8D800DCDCDC00E1E1
      E100E4E4E400D6D6D600ACACAC00ABABAB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000062628D002A2A94005959D4006A6AEB006363
      E5005959DE005252D8004F4FD6004F4FD6004F4FD6005252D8005959DE006363
      E5006A6AEB005959D4002A2A9400595985000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000ABABAB00BEBEBE00D6D6D600CDCDCD00CECE
      CE00D8D8D800D5D5D500ACACAC00000000000000000000000000000000000000
      000000000000000000001E774E0043C89D005EEDC60051E3BC003FD5AF0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB003FD5AF0051E3BC005EEDC60043C69D001B734B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A2A2A200CECECE00E4E4E400DDDDDD00D5D5D500D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D5D5D500DDDDDD00E4E4E400CDCDCD00A1A1A10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002E2E80005050C8006969EA005D5DE1004D4DD4004747
      D0004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      D0004D4DD4005D5DE1006969EA005050C8002B2B7E0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000ACACAC00BEBEBE00D5D5D500CECECE00CECECE00D8D8
      D800E7E7E700E0E0E000AFAFAF00000000000000000000000000000000000000
      0000000000001381540055E1BA0059E9C20040D6B00039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0040D6B00059E9C20055E1B900127F5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6A6A600DDDDDD00E1E1E100D6D6D600D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D6D6D600E1E1E100DDDDDD00A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000025258A006161E0006464E6004E4ED5004747D0004747D0004747
      D0004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      D0004747D0004747D0004E4ED5006464E6006161DF0024248800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B0B0B000BEBEBE00D5D5D500CECECE00CECECE00D8D8D800E7E7
      E700E3E3E300B4B4B40000000000000000000000000000000000000000000000
      0000148456005AE7C00053E4BF003AD2AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0039D0AB0039D0AB0039D0AB003AD2AC0053E6BF0059E7BF00128154000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A800E0E0E000DFDFDF00D4D4D400D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D4D4D400DFDFDF00E0E0E000A6A6A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000026268C006565E4005F5FE2004848D1004747D0004747D0004747D0004747
      D0004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      D0004747D0004747D0004747D0004848D1005F5FE3006565E400252589000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BBBBBB00B1B1
      B100B4B4B400B5B5B500B4B4B400B2B2B200BABABA0000000000000000000000
      000000000000B1B1B100D2D2D200D0D0D000CECECE00D8D8D800E6E6E600E4E4
      E400B7B7B7000000000000000000000000000000000000000000000000001F7B
      530055E2BA0053E4BF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0044C5A40045C1A20045C1A20045C1A20045C1A20048C5A60044C5A40039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0053E6BF0055E1B9001C77
      500000000000000000000000000000000000000000000000000000000000A4A4
      A400DDDDDD00DFDFDF00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200CECECE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CFCFCF00CECECE00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DFDFDF00DDDDDD00A3A3
      A300000000000000000000000000000000000000000000000000000000002E2E
      84006161E0005F5FE2004747D0004747D0004747D0004747D0004747D0004747
      D0004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      D0004747D0004747D0004747D0004747D0004747D0005F5FE3006161DF002B2B
      8100000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B0B0B000C7C7C700DFDFDF00F0F0
      F000F5F5F500F6F6F600F5F5F500F0F0F000DFDFDF00C8C8C800AFAFAF000000
      0000B0B0B000A9A9A900B7B7B700D1D1D100D7D7D700E6E6E600E5E5E500BABA
      BA0000000000000000000000000000000000000000000000000061A6890043CA
      A00059E9C2003AD0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0043C4A400F8FDF800FCFDF900FCFDF900FCFDF900ECF8F30045C1A20039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003AD2AB0059E9C20041C9
      9F0057806A000000000000000000000000000000000000000000C1C1C100D0D0
      D000E1E1E100D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200CECECE00FCFCFC00FCFCFC00FCFCFC00FCFCFC00F9F9F900CDCDCD00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D4D4D400E1E1E100CECE
      CE00AEAEAE0000000000000000000000000000000000000000006C6CAC005050
      CA006464E6004848D0004747D0004747D0004747D0004747D0004242B8004646
      C3004747D0004747D0004747D0004747D0004747D0004747D0004747D0004747
      C3004242B9004747D0004747D0004747D0004747D0004848D1006464E6004F4F
      C9005A5A8A000000000000000000000000000000000000000000000000000000
      00000000000000000000B3B3B300CACACA00EFEFEF00F6F6F600E4E4E400D2D2
      D200C5C5C500C2C2C200C5C5C500D2D2D200E5E5E500F6F6F600EFEFEF00CCCC
      CC00A7A7A700A8A8A800ADADAD00BABABA00D8D8D800E6E6E600BCBCBC00DBDB
      DB00000000000000000000000000000000000000000000000000199568005EED
      C6003FD5AF0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0045C1A200FFFDFB00FFFFFD00FFFFFD00FFFFFD00F5FBF60045C1A20039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB003FD6B0005EED
      C600179266000000000000000000000000000000000000000000B0B0B000E4E4
      E400D5D5D500D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200CDCDCD00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FAFAFA00CDCDCD00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D6D6D600E4E4
      E400AFAFAF0000000000000000000000000000000000000000002B2B9B006969
      EA004D4DD4004747D0004747D0004747D0004747D0004544B700AFABCD007C7A
      B5004B4AC5004747D0004747D0004747D0004747D0004747D0004B4BC5008685
      BA00ACA9CC004544B9004747D0004747D0004747D0004747D0004D4DD5006969
      EA00292999000000000000000000000000000000000000000000000000000000
      000000000000B3B3B300E2E2E200F4F4F400D8D8D800BBBBBB00D3D3D300E8E8
      E800F4F4F400F7F7F700F4F4F400EAEAEA00D4D4D400BCBCBC00D9D9D900F4F4
      F400E3E3E300B1B1B100C8C8C800B0B0B000BCBCBC00BDBDBD00D7D7D7000000
      0000000000000000000000000000000000000000000059A687004ED8AF004FE1
      BA0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0
      AB0045C1A200FFFEFC00FFFFFE00FFFFFE00FFFFFE00F5FBF70045C1A20039D0
      AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB0039D0AB004FE2
      BB004BD6AE00517F6700000000000000000000000000C0C0C000D8D8D800DCDC
      DC00D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200CDCDCD00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FBFBFB00CDCDCD00D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200DDDD
      DD00D6D6D600ADADAD000000000000000000000000006565AC005A5AD7005B5B
      DF004747D0004747D0004747D0004747D0004544B700AFABCE00FFFAF800FFFA
      F8007C7AB6004B4BC5004747D0004747D0004747D0004B4BC5008685BA00FFFA
      F800FFFAF800ACAACD004545B8004747D0004747D0004747D0004747D0005B5B
      E0005858D6005555880000000000000000000000000000000000000000000000
      0000B3B3B300E7E7E700EDEDED00BDBDBD00D6D6D600F3F3F300F5F5F500F5F5
      F500F7F7F700F9F9F900F9F9F900FAFAFA00F9F9F900F7F7F700D9D9D900BEBE
      BE00EDEDED00E9E9E900B3B3B300A9A9A900B3B3B30000000000000000000000
      00000000000000000000000000000000000000000000138F61005DEDC6003ED5
      AF003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2
      AC0045C1A200FFFEFC00FFFFFE00FFFFFE00FFFFFE00F5FCF70045C1A2003AD2
      AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003AD2AC003FD5
      B0005DEDC600128C5F00000000000000000000000000ADADAD00E3E3E300D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400CDCDCD00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FBFBFB00CDCDCD00D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500E3E3E300ABABAB00000000000000000000000000262694006868EA004C4E
      D400484AD100484AD100484AD1004243B700AFACCE00FFFBF900FFFBF900FFFB
      F900FFFBF9007C7BB6004B4DC600484AD1004B4CC5008785BA00FFFBF900FFFB
      F900FFFBF900FFFBF900ADABCD004546BB00484AD100484AD100484AD1004D4E
      D4006868EA00252594000000000000000000000000000000000000000000B7B7
      B700E2E2E200EBEBEB00BABABA00E3E3E300F2F2F200F1F1F100F2F2F200F4F4
      F400F6F6F600F9F9F900FBFBFB00FCFCFC00FAFAFA00F9F9F900F8F8F800EAEA
      EA00BABABA00EBEBEB00E4E4E400AFAFAF000000000000000000000000000000
      0000000000000000000000000000000000000000000031B78C0054E6C0003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0045C1A200FFFEFD00FFFFFE00FFFFFE00FFFFFE00F5FCF70043C4A4003CD3
      AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3AF003CD3
      AF0054E6C00030B68B00000000000000000000000000C4C4C400DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500CDCDCD00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FBFBFB00CECECE00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500DFDFDF00C3C3C3000000000000000000000000004040B9006061E3004A4F
      D2004A4FD2004A4FD2004A4FD2004346B600B1AFCF00FFFBF900FFFBF900FFFB
      F900FFFBF900FFFBF9007C7CB600474AB8008786BB00FFFBF900FFFBF900FFFB
      F900FFFBF900FFFBF9008987BB004B4EC5004A4FD2004A4FD2004A4FD2004A4F
      D2006061E3003F3FB8000000000000000000000000000000000000000000CBCB
      CB00EEEEEE00BCBCBC00E0E0E000EFEFEF00EFEFEF00F0F0F000F1F1F100F2F2
      F200F5F5F500F9F9F900FCFCFC00FDFDFD00FDFDFD00FCFCFC00F9F9F900F7F7
      F700EAEAEA00BDBDBD00EEEEEE00CECECE000000000000000000000000000000
      0000000000000000000000000000000000006DB69B0049D6AD004ADEBA003ED5
      B3003ED5B3003ED5B30045C5A60045C1A20045C1A20045C1A20045C1A2004AC8
      A90047C6A700FFFFFE00FFFFFF00FFFFFF00FFFFFF00F5FCF80047C6A70045C1
      A20045C1A20045C1A20045C1A20049C6A70045C5A6003ED5B3003ED5B3003ED5
      B3004ADFBA0049D5AC00648A770000000000CACACA00D6D6D600DBDBDB00D6D6
      D600D6D6D600D6D6D600CFCFCF00CDCDCD00CDCDCD00CDCDCD00CDCDCD00D1D1
      D100D0D0D000FEFEFE00FFFFFF00FFFFFF00FFFFFF00FCFCFC00D0D0D000CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00D0D0D000CFCFCF00D6D6D600D6D6D600D6D6
      D600DBDBDB00D5D5D500B4B4B400000000007979BA005657D500575CDC004C55
      D4004C55D4004C55D4004C55D4004C55D400494EBA00B1AFD000FFFBFA00FFFB
      FA00FFFBFA00FFFBFA00FFFBFA00CDCADF00FFFBFA00FFFBFA00FFFBFA00FFFB
      FA00FFFBFA008988BC005057C9004C55D4004C55D4004C55D4004C55D4004C55
      D400575DDD005656D40067679200000000000000000000000000B6B6B600EDED
      ED00CFCFCF00D1D1D100ECECEC00ECECEC00EDEDED00EEEEEE00F0F0F000F2F2
      F200F4F4F400F7F7F700FBFBFB00FDFDFD00FEFEFE00FDFDFD00FCFCFC00F9F9
      F900F7F7F700DADADA00D0D0D000EDEDED00B2B2B20000000000000000000000
      000000000000000000000000000000000000429A780058E8C20047DCB90041D7
      B70041D7B70041D7B70045C5A600F9FFFB00FCFFFC00FCFFFC00FCFFFC00FCFF
      FC00FCFFFC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FCFFFC00FCFF
      FC00FCFFFC00FCFFFC00FCFFFC00E8FBF40045C1A20041D7B70041D7B70041D7
      B70047DCBA0058E8C1003C7D600000000000B8B8B800E1E1E100DADADA00D8D8
      D800D8D8D800D8D8D800CFCFCF00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00F9F9F900CDCDCD00D8D8D800D8D8D800D8D8
      D800DADADA00E0E0E000AAAAAA00000000004C4CA1006365E500545EDA004F5B
      D6004F5BD6004F5BD6004F5BD6004F5BD6004F5BD6004B53BB00B1B1D100FFFC
      FA00FFFCFA00FFFCFA00FFFCFA00FFFCFA00FFFCFA00FFFCFA00FFFCFA00FFFC
      FA00898ABC00535CCA004F5BD6004F5BD6004F5BD6004F5BD6004F5BD6004F5B
      D600545EDA006365E50040408600000000000000000000000000C9C9C900EAEA
      EA00BABABA00E6E6E600EAEAEA00EAEAEA00ECECEC00EDEDED00EEEEEE00EFEF
      EF00F2F2F200F5F5F500F9F9F900FDFDFD00FEFEFE00FEFEFE00FDFDFD00FBFB
      FB00F9F9F900F7F7F700BDBDBD00EBEBEB00CBCBCB0000000000000000000000
      00000000000000000000000000000000000028916A0058E9C30047DCBB0045D9
      BA0045D9BA0045D9BA0045C1A200FFFFFE00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00EFFCF70045C1A20045D9BA0045D9BA0045D9
      BA0047DCBB0058E9C300258A640000000000B0B0B000E2E2E200DBDBDB00D9D9
      D900D9D9D900D9D9D900CDCDCD00FEFEFE00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FBFBFB00CDCDCD00D9D9D900D9D9D900D9D9
      D900DBDBDB00E2E2E200ACACAC0000000000343598006368E6005463DA005263
      D8005263D8005263D8005263D8005263D8005263D8005263D8004D58BC00B2B2
      D200FFFCFB00FFFCFB00FFFCFB00FFFCFB00FFFCFB00FFFCFB00FFFCFB008A8B
      BD005563CC005263D8005263D8005263D8005263D8005263D8005263D8005263
      D8005463DA006368E60030319100000000000000000000000000DEDEDE00D8D8
      D800CACACA00E5E5E500E7E7E700E9E9E900EAEAEA00ECECEC00EDEDED00EFEF
      EF00F1F1F100F3F3F300F7F7F700FAFAFA00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00F9F9F900F9F9F900D7D7D700D9D9D900E0E0E000C0C0C000000000000000
      00000000000000000000000000000000000020936A0058E8C40049DDBF0048DD
      BF0048DDBF0048DDBF0045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00F1FCF70045C1A20048DDBF0048DDBF0048DD
      BF0049DDBF0056E8C4001E8D660000000000B1B1B100E1E1E100DCDCDC00DCDC
      DC00DCDCDC00DCDCDC00CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FBFBFB00CDCDCD00DCDCDC00DCDCDC00DCDC
      DC00DCDCDC00E1E1E100ADADAD00000000002F309900636AE500566ADB00556A
      DB00556ADB00556ADB00556ADB00556ADB00556ADB00556ADB00556ADB004854
      B300F0EFF300FFFCFB00FFFCFB00FFFCFB00FFFCFB00FFFCFB00CDCDE1004F5D
      BF00556ADB00556ADB00556ADB00556ADB00556ADB00556ADB00556ADB00556A
      DB00566ADB00626AE5002D2D95000000000000000000B9B9B900EBEBEB00CACA
      CA00D3D3D300E3E3E300E7E7E700E7E7E700E9E9E900EAEAEA00ECECEC00EDED
      ED00EFEFEF00F1F1F100F4F4F400F7F7F700F9F9F900FBFBFB00FCFCFC00FBFB
      FB00FAFAFA00F9F9F900EBEBEB00CBCBCB00EDEDED00B8B8B800000000000000
      00000000000000000000000000000000000028956D0056E8C6004DE0C4004BDF
      C4004BDFC4004BDFC40045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00F1FDF90045C1A2004BDFC4004BDFC4004BDF
      C4004DE0C40056E8C600268E690000000000B3B3B300E2E2E200DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FCFCFC00CDCDCD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00E2E2E200AFAFAF000000000034359B00626DE5005971DE005872
      DD005872DD005872DD005872DD005872DD005872DD005872DD00586DCF00898D
      BE00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC007F84
      B900596ED0005872DD005872DD005872DD005872DD005872DD005872DD005872
      DD005971DE00626DE500313295000000000000000000BFBFBF00EEEEEE00C2C2
      C200D4D4D400E1E1E100E5E5E500E7E7E700E7E7E700E9E9E900EAEAEA00ECEC
      EC00EDEDED00EFEFEF00F1F1F100F3F3F300F5F5F500F7F7F700F9F9F900F9F9
      F900F9F9F900F9F9F900F4F4F400C3C3C300EEEEEE00BBBBBB00000000000000
      00000000000000000000000000000000000042A2800055E6C50052E3CA004FE2
      C9004FE2C9004FE2C90045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00EFFCF80047C6AA004FE2C9004FE2C9004FE2
      C90052E3CA0054E6C4003E84690000000000BCBCBC00E0E0E000E0E0E000DFDF
      DF00DFDFDF00DFDFDF00CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FBFBFB00D0D0D000DFDFDF00DFDFDF00DFDF
      DF00E0E0E000E0E0E000AEAEAE00000000004B4CA700616FE3005E79E1005B79
      E0005B79E0005B79E0005B79E0005B79E0005B79E0005B73D2008A8EBF00FFFD
      FC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFDFC00FFFD
      FC008085BA005B74D3005B79E0005B79E0005B79E0005B79E0005B79E0005B79
      E0005E79E100606FE30041418C000000000000000000C0C0C000EEEEEE00C0C0
      C000D2D2D200DDDDDD00E4E4E400E5E5E500E7E7E700E7E7E700E9E9E900EAEA
      EA00ECECEC00EDEDED00EFEFEF00F1F1F100F2F2F200F4F4F400F6F6F600F6F6
      F600F7F7F700F7F7F700F6F6F600C1C1C100EEEEEE00BEBEBE00000000000000
      0000000000000000000000000000000000006DBDA30045D5B40055E6CC0052E4
      CC0052E4CC0052E4CC0048C8AB0047C6A90045C1A20045C1A20045C1A20045C1
      A20046C5A700FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6FEFD0045C2A30045C1
      A20045C1A20045C1A20045C1A20045C1A20048C9AB0052E4CC0052E4CC0052E4
      CC0055E6CC0044D4B3006893800000000000CECECE00D6D6D600E2E2E200E1E1
      E100E1E1E100E1E1E100D1D1D100D0D0D000CDCDCD00CDCDCD00CDCDCD00CDCD
      CD00CFCFCF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00CDCDCD00CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00D1D1D100E1E1E100E1E1E100E1E1
      E100E2E2E200D6D6D600BABABA00000000007879C0005262D400617EE3005E80
      E2005E80E2005E80E2005E80E2005E80E2005E79D4008A90BF00FFFDFD00FFFD
      FD00FFFDFD00FFFDFD00FFFDFD00F2F2F600FFFDFD00FFFDFD00FFFDFD00FFFD
      FD00FFFDFD008087BB005E7AD5005E80E2005E80E2005E80E2005E80E2005E80
      E200617EE3005161D30069699A000000000000000000C1C1C100EEEEEE00C3C3
      C300CECECE00D6D6D600E2E2E200E4E4E400E5E5E500E7E7E700E7E7E700E9E9
      E900EAEAEA00ECECEC00EDEDED00EFEFEF00EFEFEF00F2F2F200F3F3F300F4F4
      F400F4F4F400F5F5F500F2F2F200C5C5C500EEEEEE00BDBDBD00000000000000
      000000000000000000000000000000000000000000002CBB98005BE9D00055E7
      D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7
      D00048C8AB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6FFFE0045C1A20055E7
      D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7D00055E7
      D0005BE9D0002BBA9700000000000000000000000000C7C7C700E4E4E400E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300D1D1D100FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00CDCDCD00E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400C7C7C7000000000000000000000000003C4ABD006684E6006187
      E4006187E4006187E4006187E4005D7CD3008B92C000FFFEFD00FFFEFD00FFFE
      FD00FFFEFD00FFFEFD008D93C1005066BA00B3B6D400FFFEFD00FFFEFD00FFFE
      FD00FFFEFD00FFFEFD008189BB005E7DD5006187E4006187E4006187E4006187
      E4006683E6003B49BC00000000000000000000000000CECECE00EBEBEB00CECE
      CE00C9C9C900CFCFCF00DEDEDE00E3E3E300E4E4E400E5E5E500E7E7E700E7E7
      E700E9E9E900EAEAEA00ECECEC00EDEDED00EEEEEE00F0F0F000F1F1F100F2F2
      F200F2F2F200F3F3F300E8E8E800CFCFCF00EDEDED00B8B8B800000000000000
      00000000000000000000000000000000000000000000109C740059E9D2005CE9
      D60058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8
      D40045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF0045C1A20058E8
      D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D40058E8D4005CE9
      D60059E9D200109C7400000000000000000000000000B5B5B500E4E4E400E5E5
      E500E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00CDCDCD00E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E5E5
      E500E4E4E400B5B5B500000000000000000000000000232AA1006486E600678E
      E600638DE500638DE500638DE5004E67B800DADBEA00FFFEFE00FFFEFE00FFFE
      FE00FFFEFE008D94C1006386D700638DE5005975C600B3B7D500FFFEFE00FFFE
      FE00FFFEFE00FFFEFE00B3B7D5005876C800638DE500638DE500638DE500678E
      E6006486E6002329A10000000000000000000000000000000000DFDFDF00DFDF
      DF00C2C2C200CCCCCC00D4D4D400E1E1E100E3E3E300E4E4E400E5E5E500E7E7
      E700E7E7E700E9E9E900EAEAEA00ECECEC00EDEDED00EEEEEE00F0F0F000F1F1
      F100F1F1F100F1F1F100D6D6D600DFDFDF00E0E0E000C0C0C000000000000000
      000000000000000000000000000000000000000000005AB59A0049D9C00060EB
      D9005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEA
      D70045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF0045C1A2005AEA
      D7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD7005AEAD70060EB
      D80047D8BE0058927C00000000000000000000000000C9C9C900DBDBDB00E6E6
      E600E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FEFEFE00CDCDCD00E5E5
      E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E6E6
      E600D9D9D900B7B7B7000000000000000000000000006467B9005675D8006B92
      E8006591E7006591E7006591E7006591E600526AB700DADCEA00FFFEFE00FFFE
      FE008D94C1006589D9006591E7006591E7006591E7005A78C700B4B8D500FFFE
      FE00FFFEFE00B4B8D5005B78C8006591E7006591E7006591E7006591E7006B92
      E8005474D7005A5A990000000000000000000000000000000000CDCDCD00F1F1
      F100BDBDBD00CBCBCB00CCCCCC00D8D8D800E1E1E100E3E3E300E4E4E400E5E5
      E500E7E7E700E7E7E700E9E9E900EAEAEA00ECECEC00EDEDED00EEEEEE00F0F0
      F000F0F0F000EDEDED00C1C1C100F1F1F100CECECE0000000000000000000000
      000000000000000000000000000000000000000000000000000015A782005DEB
      D80062ECDA005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6FFFF0045C1A2005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD80062ECDA005DEB
      D80014A581000000000000000000000000000000000000000000BBBBBB00E6E6
      E600E7E7E700E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00CDCDCD00E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E7E7E700E6E6
      E600BABABA0000000000000000000000000000000000000000002736AA006893
      E8006C95E9006694E8006694E8006694E8006694E800546EBB00D9DBEA008E96
      C200668BD9006694E8006694E8006694E8006694E8006694E8005B7AC800B4B9
      D600B4B9D6005B7BC8006694E8006694E8006694E8006694E8006C95E9006893
      E8002634A9000000000000000000000000000000000000000000C6C6C600EEEE
      EE00D7D7D700C3C3C300C9C9C900CDCDCD00D8D8D800E1E1E100E3E3E300E4E4
      E400E5E5E500E7E7E700E7E7E700E9E9E900EAEAEA00ECECEC00EDEDED00EDED
      ED00EBEBEB00D6D6D600D8D8D800EFEFEF00BDBDBD0000000000000000000000
      000000000000000000000000000000000000000000000000000063BBA10041D0
      B70069EDDD005DEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80045C1A200FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6FFFF0045C1A2005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005DEBD80069EDDD0040CF
      B60061A791000000000000000000000000000000000000000000CCCCCC00D5D5
      D500E8E8E800E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600CDCDCD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00CDCDCD00E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E8E8E800D5D5
      D500C3C3C30000000000000000000000000000000000000000006C6FBE004F6E
      D0007397EA006894E8006694E8006694E8006694E8006694E8005471C0006288
      D7006694E8006694E8006694E8006694E8006694E8006694E8006694E8005878
      C7005878C7006694E8006694E8006694E8006694E8006894E8007397EA004E6D
      CF006668AD00000000000000000000000000000000000000000000000000D2D2
      D200F3F3F300C2C2C200C6C6C600C9C9C900CCCCCC00D6D6D600DFDFDF00E2E2
      E200E4E4E400E5E5E500E7E7E700E7E7E700E9E9E900EAEAEA00E9E9E900E7E7
      E700DBDBDB00C5C5C500F3F3F300D4D4D4000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000020A2
      7E005DE4D20071EFE0005CEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80046C3A40045C1A20045C1A20045C1A20045C1A20045C1A20046C3A4005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005CEBD80071EFE0005CE4D0001FA0
      7B0000000000000000000000000000000000000000000000000000000000B9B9
      B900E3E3E300EAEAEA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600CECECE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CECECE00E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600EAEAEA00E2E2E200B7B7
      B700000000000000000000000000000000000000000000000000000000002E34
      A600688AE2007A99EC006794E8006694E8006694E8006694E8006694E8006694
      E8006694E8006694E8006694E8006694E8006694E8006694E8006694E8006694
      E8006694E8006694E8006694E8006694E8006794E8007A99EC00678AE2002D33
      A40000000000000000000000000000000000000000000000000000000000D0D0
      D000E7E7E700EEEEEE00C0C0C000C6C6C600C8C8C800CACACA00CFCFCF00D6D6
      D600DCDCDC00DFDFDF00E1E1E100E1E1E100E0E0E000DDDDDD00DADADA00D2D2
      D200C1C1C100EFEFEF00E9E9E900BEBEBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013A37E006CEBDA0076F1E1005DEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005DEBD80076F1E1006CEBD90012A17C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900E7E7E700EBEBEB00E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600EBEBEB00E7E7E700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000252CA6007593E8007E9AED006894E8006694E8006694E8006694E8006694
      E8006694E8006694E8006694E8006694E8006694E8006694E8006694E8006694
      E8006694E8006694E8006694E8006894E8007E9AED007592E800242BA5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C8C8C800EFEFEF00EFEFEF00C3C3C300C3C3C300C8C8C800C8C8C800CBCB
      CB00CDCDCD00CFCFCF00D1D1D100D2D2D200D1D1D100D2D2D200C8C8C800C5C5
      C500EFEFEF00F0F0F000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014A47E006FE9D80080F3E60066EDDC005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D8005BEBD8005BEBD80066EDDC0080F3E6006EE9D70014A47D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BABABA00E7E7E700EEEEEE00E8E8E800E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E6E6E600E6E6E600E8E8E800EEEEEE00E6E6E600B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000262DA700788FE600879DEF007097EA006694E8006694E8006694
      E8006694E8006694E8006694E8006694E8006694E8006694E8006694E8006694
      E8006694E8006694E8007097EA00879DEF00778EE600262CA600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C8C8C800ECECEC00F6F6F600D8D8D800C0C0C000C2C2C200C7C7
      C700CACACA00CBCBCB00CBCBCB00CACACA00C5C5C500C1C1C100D9D9D900F7F7
      F700EDEDED00C1C1C10000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020A682005AD8C20088F5E9007EF3E40066EDDC005BEB
      D8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEBD8005BEB
      D80066EDDC007EF3E40088F5E9005AD7C20020A5820000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00DCDCDC00EFEFEF00EDEDED00E8E8E800E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6
      E600E8E8E800EDEDED00EFEFEF00DBDBDB00BBBBBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003234AA006575D6008F9FF100869DEF007096EA006694
      E8006694E8006694E8006694E8006694E8006694E8006694E8006694E8006694
      E8007096EA00869DEF008F9FF1006574D6003234AA0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D2D2D200D9D9D900F8F8F800F6F6F600E2E2E200D2D2
      D200C9C9C900C6C6C600C9C9C900D3D3D300E2E2E200F6F6F600F9F9F900DADA
      DA00CBCBCB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000064C1A80022B08E0071E4D30092F7EC008CF5
      E9007DF2E40071EFE0006CEEDE006AEEDE006CEEDE0071EFE0007DF2E4008CF5
      E90092F7EC0071E3D30022B08E0063C0A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D0D0D000C1C1C100E4E4E400F1F1F100EFEF
      EF00EDEDED00EAEAEA00E9E9E900E9E9E900E9E9E900EAEAEA00EDEDED00EFEF
      EF00F1F1F100E4E4E400C1C1C100CFCFCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007071C3003339B2007A86E20098A2F30092A0
      F100859CEE007A99EC007598EB007498EB007598EB007A99EC00859CEE0092A0
      F10098A2F3007A85E1003339B2006F71C3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CBCBCB00D7D7D700EDEDED00F9F9
      F900FCFCFC00FCFCFC00FCFCFC00FAFAFA00EDEDED00D8D8D800CACACA000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005DBFA5001DAA87004ACA
      B00076E3D30092F5EA0099F8EE009AF8EE0099F8EE0092F5EA0076E3D3004AC9
      B0001CAA86005DBFA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00BDBDBD00D2D2
      D200E4E4E400F0F0F000F2F2F200F2F2F200F2F2F200F0F0F000E4E4E400D2D2
      D200BDBDBD00CECECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000696AC2002D30AD00575D
      C9007E84E100989EF1009EA4F4009FA4F4009EA4F400989EF1007E84E100575D
      C9002D30AD006A6BC20000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D2D2
      D200CACACA00CACACA00CACACA00D2D2D200E2E2E20000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000075CAB40046B799002CA9870023A885002CA9870047B7990075CAB4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6D6D600C8C8C800BEBEBE00BDBDBD00BEBEBE00C8C8C800D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080CB005555BA003838AD003435AC003838AD005555BA008081CB000000
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
      0000000000000000000000000000000000000000000000000000D3D3D300BEBE
      BE00BABABA00B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000929292006F6F
      6F00707070007171710072727200727272007272720072727200727272007272
      7200727272007272720072727200727272007272720072727200727272007272
      7200727272008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF00ABAB
      AB00ACACAC00ACACAC00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00ADADAD00B7B7B70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000098635A00A765
      5900937B77000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00B8B8B80000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000070707000FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB007676760000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ACACAC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5665900D2917900E7AE
      9100C27B6600A273690000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFB
      FB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9
      F900FCFCFC00B8B8B80000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000070707000FBFB
      FB00FDFDFD00FCFCFC00FCFCFC00FCFBFB00FBFBFB00FBFBFA00FAFAFA00FAFA
      F900F9F9F900F9F9F800F8F8F800F8F7F700F7F7F700F7F6F600F6F6F500F6F5
      F500FBFBFB007676760000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ACACAC00FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFB
      FB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9
      F900FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A66A5C00D3917800F2BE9C00EEB4
      9000EEB99600C6806700A98C8500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFB
      FB00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8
      F800FCFCFC00B8B8B8000000000000000000000000000000000038587A002A4D
      71002A4D71002A4D71002A4D71002A4D71002A4D71002B4E710070707000FBFB
      FB00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FBFAFA00FAFAFA00FAF9F900F9F9
      F900F9F8F800F8F8F700F7F7F700F7F7F600F6F6F600F6F5F500F5F5F500F5F4
      F400FBFBFB007676760000000000000000000000000000000000A1A1A1009B9B
      9B009B9B9B009B9B9B009B9B9B009B9B9B009B9B9B009B9B9B00ACACAC00FCFC
      FC00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFB
      FB00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8
      F800FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A76E5D00D4917600F2BE9D00EDAE8B00ECB1
      8900F4C29D00EEBC9C00BB6F5900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FAFAFA00FAFA
      FA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8
      F800FCFCFC00B8B8B8000000000000000000000000003C5D7E005084B1005286
      B3005286B3005185B3005185B1005083B0004F82AE004E80AA0070707000FBFB
      FB00FCFBFB00FBFBFB00FBFBFA00FAFAFA00FAFAF900F9F9F900F9F9F800F8F8
      F800F8F7F700F7F7F700F7F6F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3
      F300FBFBFB0076767600000000000000000000000000A3A3A300BBBBBB00BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BBBBBB00BABABA00B8B8B800ACACAC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FAFAFA00FAFA
      FA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8
      F800FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A7725F00D5917300F1BC9C00EDAF8C00ECB18900F5C3
      9F00FFDAC000F9CFB400C2765A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C400FCFC
      FC00FCFCFC00FAFAFA00CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00CACACA00CACACA00CACACA00CACACA00CACACA00CBCBCB00F8F8F800F7F7
      F700FCFCFC00B8B8B8000000000000000000000000002D51740077A8D0005390
      C300528EC200518EC100518DBF00508BBD004E88B9004D85B40070707000FBFB
      FB00FBFBFB00FBF9F800F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B47100F4F3F300F3F3
      F200FBFBFB00767676000000000000000000000000009D9D9D00D0D0D000C3C3
      C300C2C2C200C2C2C200C1C1C100C0C0C000BEBEBE00BCBCBC00ACACAC00FCFC
      FC00FCFCFC00FAFAFA00CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00CACACA00CACACA00CACACA00CACACA00CACACA00CBCBCB00F8F8F800F7F7
      F700FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AB796500D6917100F1BC9C00EEB08D00ECB18900F5C29F00FFDA
      BF00FCD3B800CC80610000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C1C1C100FCFC
      FC00FCFCFC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9
      F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6
      F600FCFCFC00B8B8B8000000000000000000000000002C5074007BABD3005490
      C3005490C300538FC200528EC000518CBD004F89BA004E86B40070707000FBFB
      FB00FAFAFA00FAFAF900F9F9F900F9F9F800F8F8F800F8F7F700F7F7F700F7F6
      F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3F300F3F3F200F3F2F200F2F2
      F100FBFBFB00767676000000000000000000000000009C9C9C00D1D1D100C3C3
      C300C3C3C300C2C2C200C2C2C200C0C0C000BFBFBF00BCBCBC00ACACAC00FCFC
      FC00FCFCFC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9
      F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6
      F600FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C8B8A007C7A
      7900827F7D0085818000827F7D007F7D7B008B89890000000000000000000000
      000000000000C47B5900ECB79700EEB39000ECB18900F5C29D00FFD9BE00FDD5
      B900D2866300000000000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FCFCFC00C4C4C400FCFC
      FC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9
      F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6
      F600FCFCFC00B8B8B8000000000000000000000000002D5176007CACD3005692
      C5005591C4005590C300548FC100538DBF00518ABB004F86B50070707000FBFB
      FB00FAF9F900F9F9F900F9F8F800F8F8F700F7F7F700F7F7F600F6F6F600F6F5
      F500F5F5F500F5F4F400F4F4F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1
      F000FBFBFB00767676000000000000000000000000009D9D9D00D2D2D200C4C4
      C400C3C3C300C3C3C300C2C2C200C1C1C100BFBFBF00BCBCBC00ACACAC00FCFC
      FC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9
      F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6
      F600FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B797700A2A09F00CBC9C900E8E7
      E600F2EFEE00F3F1EF00F2EFEE00E9E7E600CCCAC900A4A2A0007B7776000000
      000079777600A46E5200CD886300EAB59200F4C09C00FFD9BC00FFD7BB00D78D
      650000000000000000000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FBFBFB00C4C4C400FCFC
      FC00FAFAFA00F9F9F900CDCDCD00CDCDCD00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CECECE00F6F6F600F5F5
      F500FCFCFC00B8B8B8000000000000000000000000002E5277007DADD4005894
      C6005793C5005793C4005691C200558FC000538CBC005188B60070707000FBFB
      FB00F9F9F800F8F7F600F7B57400F7B57400F7B57300F7B57300F7B57300F7B5
      7300F6B57300F6B57300F6B57300F6B57300F6B57300F6B77700F1F0F000F0F0
      EF00FBFBFB00767676000000000000000000000000009E9E9E00D2D2D200C5C5
      C500C5C5C500C4C4C400C3C3C300C2C2C200C0C0C000BEBEBE00ACACAC00FCFC
      FC00FAFAFA00F9F9F900CDCDCD00CDCDCD00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CECECE00F6F6F600F5F5
      F500FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      00000000000000000000807E7C00A9A6A400E7E4E300F3F1F100D5D3D000B5B3
      B100A09C9B009A979600A09C9B00B6B3B100D6D3D200F4F2F100E8E6E400ABA9
      A7006D6967006D6A6900AB745900CF8D6700EFC0A100FFD8BC00DC916800EAC3
      AE0000000000000000000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FDFDFD00FCFCFC00FBFBFB00F7F7F700F3F3F300C3C3C300FCFC
      FC00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8F800F8F8
      F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5
      F500FCFCFC00B8B8B8000000000000000000000000002F5479007EAED5005A95
      C7005995C6005994C5005892C3005791C100558EBD005389B70070707000FBFB
      FB00F8F8F700F7F7F700F7F7F600F6F6F600F6F5F500F5F5F500F5F4F400F4F4
      F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1F000F1F0EF00F0EFEF00F0EF
      EE00FBFBFB00767676000000000000000000000000009F9F9F00D3D3D300C6C6
      C600C5C5C500C5C5C500C4C4C400C3C3C300C1C1C100BEBEBE00ACACAC00FCFC
      FC00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8F800F8F8
      F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5
      F500FCFCFC00AFAFAF0000000000000000000000000000000000000000000000
      000000000000817D7C00CFCECD00F1EDEC00C1BEBB00918C8900BDB6AE00E2DC
      D100F6EFE400FBF5EB00F6F0E700E3DDD500BEB7B200928E8A00C2BFBE00F1ED
      EC00D2D0CF007D7A7900A4A1A000B17B5C00D5926A00DD936900E4BDA7000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FCFCFC00FBFBFB00F9F9F900F5F5F500EEEEEE00C2C2C200FCFC
      FC00FAFAFA00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7
      F700F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4
      F400FCFCFC00B7B7B70000000000000000000000000030557A0080AFD6005C97
      C9005C97C8005B96C7005A94C5005992C200578FBE00558BB90070707000FBFB
      FB00F7F7F700F7F6F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3F300F3F3
      F200F3F2F200F2F2F100F2F1F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEE
      ED00FBFBFB00757575000000000000000000000000009F9F9F00D4D4D400C7C7
      C700C6C6C600C6C6C600C5C5C500C3C3C300C2C2C200BFBFBF00ACACAC00FCFC
      FC00FAFAFA00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F7F7
      F700F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4
      F400FCFCFC00AEAEAE0000000000000000000000000000000000000000000000
      0000827D7D00DAD8D700E6E1E00094908E00C3BDB100FBF0DE00FFF5DE00FFF4
      DF00FFF7E500FFF9EB00FFFBEE00FFFBEF00FFF9EE00FFF6EA00C7C0B8009591
      8E00E7E2E000DDDAD900807C7B00726E6B00A2806C0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FCFCFC00FBFBFB00F8F8F800F4F4F400EDEDED00C2C2C200FCFC
      FC00F9F9F900F8F8F800CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CDCDCD00F4F4F400F3F3
      F300FCFCFC00B7B7B70000000000000000000000000031567C0081B0D7005E99
      CA005E99C9005D98C8005C96C6005B94C3005991C000578DBA0070707000FBFB
      FB00F6F6F600F6F5F300F7B57300F7B57300F6B57300F6B57300F6B57300F6B5
      7300F6B57300F6B57300F6B57300F6B57300F6B57300F6B67700EEEEED00EEED
      EC00FBFBFB0074747400000000000000000000000000A1A1A100D4D4D400C7C7
      C700C7C7C700C7C7C700C6C6C600C5C5C500C3C3C300C0C0C000ACACAC00FCFC
      FC00F9F9F900F8F8F800CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CDCDCD00F4F4F400F3F3
      F300FCFCFC00AEAEAE0000000000000000000000000000000000000000008784
      8300D0CFCE00E1DEDA00908A8700DDD4C400FFF0D400FFF0D000FFF1D300FFF4
      D900FFF6E200FFF9EB00FFFCF200FFFDF400FFFCF000FFF9EC00FFF8E900E5DE
      D300908B8800E2DFDC00D4D2D0007A7775000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FBFBFB00E1E1E100DEDEDE00DBDBDB00D5D5D500C0C0C000FCFC
      FC00F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6
      F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3
      F300FCFCFC00B7B7B70000000000000000000000000032587E0083B1D700619B
      CC00619BCB00609ACA005F98C8005E96C5005C93C100598FBB0070707000FBFB
      FB00F6F5F500F5F5F400F4F4F400F4F3F300F3F3F200F3F2F200F2F2F100F2F1
      F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEEED00EEEDED00EEEDEC00EDEC
      EC00FBFBFB0074747400000000000000000000000000A2A2A200D5D5D500C9C9
      C900C9C9C900C8C8C800C7C7C700C6C6C600C4C4C400C2C2C200ACACAC00FCFC
      FC00F9F9F900F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6
      F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3
      F300FCFCFC00AEAEAE000000000000000000000000000000000000000000ABA7
      A600E9E3E100928E8A00DBD0BD00FEEDCB00FFEEC600FFEFC900FFF0CF00FFF1
      D500FFF6DF00FFF9EA00FFFEF400FFFFF800FFFFF800FFFDF400FFF9EC00FFF7
      E800E5DED30096908D00E9E3E100AEABAA000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FCFCFC00FCFCFC00FBFBFB00F8F8F800F3F3F300ECECEC00C2C2C200FCFC
      FC00F8F8F800F8F8F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6F600F5F5
      F500F5F5F500F5F5F500F5F5F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3
      F300FCFCFC00B7B7B700000000000000000000000000345A800083B2D800639D
      CE00639DCD00629CCC00619ACA006098C7005E95C3005C91BC0070707000FBFB
      FB00F5F4F400F4F4F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1F000F1F0
      EF00F0EFEF00F0EFEE00EFEEEE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECEC
      EB00FBFBFB0074747400000000000000000000000000A3A3A300D5D5D500CACA
      CA00CACACA00C9C9C900C8C8C800C7C7C700C5C5C500C2C2C200ACACAC00FCFC
      FC00F8F8F800F8F8F800F8F8F800F7F7F700F6F6F600F6F6F600F6F6F600F5F5
      F500F5F5F500F5F5F500F5F5F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3
      F300FCFCFC00AEAEAE000000000000000000000000000000000086838100E4E1
      DF00B4AEAB00BDB5A500F8E8C300FFEABC00FFEDC000FFEDC600FFEFCA00FFF1
      D000FFF4DB00FFF8E600FFFDF200FFFFFB00FFFFFD00FFFFF900FFFDF400FFF9
      EC00FFF8E800C8C1B900B7B0AD00E6E2E000827D7C0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FCFCFC00FBFBFB00FAFAFA00F7F7F700F3F3F300ECECEC00C2C2C200FCFC
      FC00F8F8F800F6F6F600CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00EFEFEF00F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2
      F200FCFCFC00B7B7B700000000000000000000000000365C830086B4DA0066A0
      CF00669FCE00659ECD00649CCB00629AC8006097C4005E93BE0070707000FBFB
      FB00F4F3F300F3F2F000F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00F0E7DF00EFEEED00EEEDED00EEEDEC00EDECEC00EDECEB00ECEBEB00ECEB
      EA00FBFBFB0074747400000000000000000000000000A4A4A400D6D6D600CBCB
      CB00CBCBCB00CACACA00C9C9C900C8C8C800C6C6C600C3C3C300ACACAC00FCFC
      FC00F8F8F800F6F6F600CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00EFEFEF00F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2
      F200FCFCFC00AEAEAE0000000000000000000000000000000000A7A4A200E3DD
      D9008F898400EADDBE00FEE7B400FFEAB700FFECBB00FFEDC000FFEDC600FFEF
      CB00FFF1D400FFF6DF00FFFBED00FFFFF700FFFFFE00FFFFFD00FFFFF800FFFD
      F200FFF9EB00FFF6EA0094908C00E4DEDA00ABA7A60000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FCFCFC00FBFBFB00E2E2E200E0E0E000DCDCDC00D6D6D600C0C0C000FCFC
      FC00F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2
      F200FCFCFC00B7B7B700000000000000000000000000375E840087B6DB0069A2
      D10068A1D00067A0CF00679FCD00659DCA006399C6006195BF0070707000FBFB
      FB00F3F3F200F2F2F100F2F1F100F1F1F000F1F0EF00F0EFEF00F0EFEE00EFEE
      EE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECECEB00ECEBEA00ECEBEA00EBEA
      E900FBFBFB0074747400000000000000000000000000A5A5A500D7D7D700CDCD
      CD00CCCCCC00CBCBCB00CBCBCB00CACACA00C7C7C700C5C5C500ACACAC00FCFC
      FC00F7F7F700F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2
      F200FCFCFC00AEAEAE0000000000000000000000000000000000CCC8C500C4BE
      BA00B1A89A00F2DFB200FFE7AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEE
      C600FFF0CE00FFF3D700FFF7E400FFFDEF00FFFFF800FFFFFB00FFFFF800FFFE
      F500FFFBEE00FFFAEC00C2BDB500C6BFBC00CECAC90095939200000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FCFCFC00FBFBFB00FAFAFA00F7F7F700F3F3F300ECECEC00C2C2C200FCFC
      FC00F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4F400F4F4
      F400F3F3F300F3F3F300F3F3F300F9F9F900FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FDFDFD00B7B7B700000000000000000000000000385F860089B7DC006BA4
      D3006BA3D2006AA3D10069A1CF00689FCC00659CC7006397C10070707000FBFB
      FB00F2F2F100F2F1F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEEED00EEED
      ED00EEEDEC00EDECEC00EDECEB00F7F7F600FAF9F900F9F9F900F9F9F900F9F9
      F900FCFCFC0074747400000000000000000000000000A6A6A600D8D8D800CECE
      CE00CDCDCD00CDCDCD00CCCCCC00CBCBCB00C9C9C900C6C6C600ACACAC00FCFC
      FC00F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F4F4F400F4F4
      F400F3F3F300F3F3F300F3F3F300F9F9F900FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FDFDFD00AEAEAE000000000000000000000000008B878700E3DEDC00AAA4
      A200C5BAA200F6DDA700FFE6A800FFE7AC00FFE8B200FFEAB700FFECBB00FFED
      C000FFEEC700FFF0CF00FFF4D800FFF7E400FFFBED00FFFDF300FFFEF500FFFD
      F300FFFBEF00FFF9EC00E6E0D600ACA6A400E6E0DE0089878500000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FBFBFB00FBFBFB00F9F9F900F7F7F700F2F2F200EBEBEB00C2C2C200FCFC
      FC00F6F6F600F5F5F500CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00EEEEEE00F3F3F300F2F2F200FAFAFA00CBCBCB00C0C0C000C0C0C000D6D6
      D600E2E2E200BCBCBC000000000000000000000000003A6188008BB8DD006EA6
      D4006EA6D3006DA5D2006BA3D0006AA1CD00689EC9006699C30070707000FBFB
      FB00F1F1F000F1EFED00F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00EEE5DC00ECECEB00ECEBEA00F9F8F800A7A7A5009595930095959300BBBB
      BA00F3F3F30086868500000000000000000000000000A7A7A700D8D8D800CFCF
      CF00CFCFCF00CECECE00CDCDCD00CBCBCB00CACACA00C7C7C700ACACAC00FCFC
      FC00F6F6F600F5F5F500CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00EEEEEE00F3F3F300F2F2F200FAFAFA00CBCBCB00C0C0C000C0C0C000D6D6
      D600F8F8F800B8B8B80000000000000000000000000097929100E9E2E0009B97
      9500CABDA000F7DB9F00FFE4A200FFE6A800FFE7AC00FFE8B200FFEAB700FFEC
      BB00FFEDC100FFEEC800FFF0CF00FFF3D800FFF6DF00FFF8E700FFF9EB00FFF9
      ED00FFF9EC00FFF9EA00F7F0E6009F999700E9E2E000918C8A00000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FBFBFB00FAFAFA00E2E2E200E0E0E000DCDCDC00D6D6D600C0C0C000FCFC
      FC00F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3
      F300F3F3F300F2F2F200F2F2F200FAFAFA00C5C5C500F9F9F900FBFBFB00E7E7
      E700C0C0C000000000000000000000000000000000003B638A008CB9DE0071A8
      D60070A8D50070A7D4006EA5D2006DA3CF006BA0CB00699CC50070707000FBFB
      FB00F0F0EF00F0EFEF00EFEFEE00EFEEED00EEEDED00EEEDEC00EDECEC00EDEC
      EB00ECEBEB00ECEBEA00EBEAEA00F8F8F7009C9C9A00F6F6F600FAF9F900F3F3
      F3009191900000000000000000000000000000000000A8A8A800D9D9D900D0D0
      D000D0D0D000D0D0D000CECECE00CDCDCD00CBCBCB00C9C9C900ACACAC00FCFC
      FC00F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3
      F300F3F3F300F2F2F200F2F2F200FAFAFA00C5C5C500F9F9F900FBFBFB00F8F8
      F800BEBEBE000000000000000000000000000000000098949300EAE3E0009993
      9100C7BA9B00F2D69600FFE29D00FFE5A200FFE6A800FFE7AC00FFE8B200FFEA
      B700FFECBB00FFEDC000FFEEC700FFF0CE00FFF2D500FFF4DB00FFF6E000FFF6
      E300FFF7E500FFF7E600FCF5E8009A969300EAE3E00095908E00000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FBFBFB00FAFAFA00F9F9F900F7F7F700F2F2F200EBEBEB00C2C2C200FCFC
      FC00F5F5F500F5F5F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2
      F200F2F2F200F2F2F200F2F2F200FAFAFA00C5C5C500F6F6F600E8E8E800C0C0
      C00000000000000000000000000000000000000000003C648C008EBBDE0073AB
      D80073AAD70072AAD50071A8D40070A6D1006EA4CE006C9FC80070707000FBFB
      FB00F0EFEE00EFEEEE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECECEB00ECEB
      EA00ECEBEA00EBEAE900EBEAE900F8F8F7009C9C9A00F2F2F100F3F3F3009292
      90000000000000000000000000000000000000000000A9A9A900DADADA00D2D2
      D200D1D1D100D1D1D100D0D0D000CECECE00CDCDCD00CACACA00ACACAC00FCFC
      FC00F5F5F500F5F5F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2
      F200F2F2F200F2F2F200F2F2F200FAFAFA00C5C5C500F6F6F600F8F8F800BFBF
      BF0000000000000000000000000000000000000000009A979300EBE4E1009F9A
      9800C2B49600E6CA8B00FFE19800FFE39D00FFE5A200FFE6A800FFE7AC00FFE8
      B200FFEAB700FFECBB00FFEDC000FFEEC600FFEFCB00FFF1D100FFF2D700FFF4
      DA00FFF4DD00FFF5DF00F4ECDF00A19C9A00EBE4E100938F8D00000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FBFBFB00FAFAFA00F9F9F900F6F6F600F2F2F200ECECEC00C2C2C200FCFC
      FC00F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2
      F200F2F2F200F2F2F200F1F1F100FAFAFA00D8D8D800E8E8E800C1C1C1000000
      000000000000000000000000000000000000000000003D668E008FBCE00076AD
      D90076ACD80075ACD70074AAD60073A9D40071A7D1006FA3CC0070707000FBFB
      FB00EFEEED00EEEDED00EEEDEC00EDECEC00EDECEB00ECEBEB00ECEBEA00EBEA
      EA00EBEAE900EBEAE900EAE9E800F8F7F700BEBEBD00F3F3F300919190000000
      00000000000000000000000000000000000000000000AAAAAA00DBDBDB00D2D2
      D200D2D2D200D2D2D200D1D1D100D0D0D000CFCFCF00CDCDCD00ACACAC00FCFC
      FC00F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2
      F200F2F2F200F2F2F200F1F1F100FAFAFA00D8D8D800F8F8F800BFBFBF000000
      00000000000000000000000000000000000000000000B0ADAB00E6DFDC00B0AC
      AA00B5AA9100D6BD8400FADA9100FFE29900FFE39D00FFE5A200FFE6A800FFE7
      AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEDC600FFEFCA00FFF0CF00FFF1
      D300FFF2D700FFF2D900E3DBCD00B3AEAC00E8E1DE0089858400000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FAFAFA00F9F9F900E1E1E100DFDFDF00DCDCDC00D8D8D800C0C0C000FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FDFDFD00E7E7E700C2C2C200000000000000
      000000000000000000000000000000000000000000003F67900090BCE00078AF
      DB0078AEDA0078AEDA0077ADD80076ABD70075A9D40073A6D00070707000FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FCFCFC00F3F3F30093939100000000000000
      00000000000000000000000000000000000000000000AAAAAA00DBDBDB00D4D4
      D400D4D4D400D4D4D400D2D2D200D2D2D200D0D0D000CECECE00ACACAC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FDFDFD00F8F8F800C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000D0CAC600CCC9
      C600A29A8900CBB68500E7C88100FFDF9300FFE29900FFE39D00FFE5A200FFE6
      A800FFE7AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEDC600FFEFC900FFF0
      CF00FFF0D200FCEED600C2BCB200CECAC800D3CCC90096939200000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FAFAFA00FAFAFA00F9F9F900F7F7F700F4F4F400F0F0F000D0D0D000C1C1
      C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1
      C100C3C3C300C0C0C000C3C3C300C4C4C400C4C4C40000000000000000000000
      00000000000000000000000000000000000000000000416A920092BEE1007BB1
      DC007BB1DC007AAFDA0078ADD70076AAD40075A8D10074A7CF006F7D89006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E006E6E
      6E006E6E6E006E6E6E006E6E6E006E6E6E007E80800000000000000000000000
      00000000000000000000000000000000000000000000ACACAC00DCDCDC00D5D5
      D500D5D5D500D4D4D400D2D2D200D1D1D100D0D0D000CFCFCF00B4B4B400AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00B4B4B40000000000000000000000
      0000000000000000000000000000000000000000000000000000B0AAA600ECE8
      E600948F8900BEAF8B00D5B97900F1D18500FFE19400FFE29900FFE39D00FFE5
      A200FFE6A800FFE7AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEDC600FFEF
      C900FFEDCC00F2E6D1009D989300EDE9E700B3ACA90000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FAFAFA00F9F9F900F9F9F900F7F7F700F5F5F500F3F3F300EFEFEF00EDED
      ED00EBEBEB00E9E9E900E9E9E900ECECEC00ECECEC00EDEDED00EEEEEE00F3F3
      F300FDFDFD000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000426B930093C0E2007DB3
      DE007CB3DD007AAFD90076A9D10071A2C9006E9EC4006C9CC1006C9BC0006B9A
      BF006B99BE006B99BD006A99BD006A98BD006A98BD006B99BE006E9DC30072A4
      CB0076A9D20078ACD50078ACD5006998C0003F68910000000000000000000000
      00000000000000000000000000000000000000000000ADADAD00DDDDDD00D6D6
      D600D6D6D600D4D4D400D0D0D000CCCCCC00C9C9C900C8C8C800C7C7C700C7C7
      C700C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C9C9C900CDCD
      CD00D0D0D000D2D2D200D2D2D200C6C6C600ABABAB0000000000000000000000
      0000000000000000000000000000000000000000000000000000A39F9D00EBE4
      E000BFBBBA00A59D8A00C4B08100D9BC7700F2D18700FFE09400FFE29900FFE3
      9D00FFE5A200FFE6A800FFE7AC00FFE8B200FFEAB700FFECBB00FFEDC000FFEA
      C300F5E6C800C4BDB000C1BEBC00ECE6E200928F8C0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FAFAFA00F9F9F900E2E2E200E1E1E100E0E0E000DEDEDE00DDDDDD00DBDB
      DB00EEEEEE00EFEFEF00EFEFEF00F3F3F300DBDBDB00D6D6D600D6D6D600E4E4
      E400EFEFEF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000436C950095C0E3007FB5
      DF007FB4DE007BAFD80074A3C9006A94B600648DAD006289A9006188A7006087
      A6006087A6006086A5006086A5006087A5006188A700638BAB006993B50072A2
      C80079ACD5007CB0DA007CB0DA006B9AC200406A930000000000000000000000
      00000000000000000000000000000000000000000000ADADAD00DDDDDD00D7D7
      D700D6D6D600D4D4D400CDCDCD00C3C3C300BFBFBF00BDBDBD00BCBCBC00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BCBCBC00BEBEBE00C3C3C300CBCB
      CB00D2D2D200D4D4D400D4D4D400C7C7C700ACACAC0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B9B3
      AF00F1ECEB009E999700B1A68C00C6AF7C00D8BB7600EACB8200FDDB9100FFE1
      9900FFE39D00FFE5A200FFE6A800FFE7AC00FFE8B200FFE9B500FCE6B800F1DF
      BC00D2C7B400A29D9A00F2EDEB00BBB6B3000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00F9F9F900F9F9F900F9F9F900F8F8F800F7F7F700F6F6F600F5F5F500F5F5
      F500F3F3F300F3F3F300F3F3F300F7F7F700DBDBDB00F7F7F700F8F8F800EFEF
      EF00DDDDDD000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000446D960096C1E40081B6
      E00080B6E0007DB1DA005A636A005A5A5A00595A5A0059595900595959005959
      5900595959005959590059595900595959005959590059595900585858006073
      81007CB0D9007FB4DD007FB5DE006D9DC400416B940000000000000000000000
      00000000000000000000000000000000000000000000AEAEAE00DDDDDD00D8D8
      D800D8D8D800D5D5D500A5A5A5009F9F9F009E9E9E009E9E9E009E9E9E009E9E
      9E009E9E9E009E9E9E009E9E9E009E9E9E009E9E9E009E9E9E009E9E9E00AEAE
      AE00D4D4D400D6D6D600D7D7D700C9C9C900ADADAD0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B4B1
      AE00DED8D500E7E3E2009A948F00B1A68A00C2AD7E00D1B67600DDBF7A00E9CB
      8600F2D58F00F7D99700F8DC9C00F6DBA000F1D8A200E7D1A100D9C8A400C1B7
      A3009C979300E8E4E300E0DAD80092908E000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00F9F9F900F9F9F900F9F9F900F8F8F800F7F7F700F7F7F700F7F7F700F6F6
      F600F6F6F600F6F6F600F5F5F500FAFAFA00DDDDDD00F8F8F800F0F0F000DDDD
      DD00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000456E970097C3E50082B8
      E20082B8E10080B6DF005D5F6000C2C2C200DCDCDC00DCDCDC00DBDBDB00DBDB
      DB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00ABABAB00626E
      760080B5DE0081B7E00082B8E1006E9EC600426C950000000000000000000000
      00000000000000000000000000000000000000000000AEAEAE00DFDFDF00D9D9
      D900D9D9D900D8D8D800A2A2A200DBDBDB00EAEAEA00EAEAEA00EAEAEA00EAEA
      EA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00CECECE00ABAB
      AB00D7D7D700D8D8D800D9D9D900CACACA00ADADAD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A09E00EAE6E300E7E4E300A09B9800A69D8A00BAAB8600C4AE7D00CDB4
      7A00D4B97D00D6BC8000D7BE8600D5BE8B00D0BD9200C7BA9B00AFA69600A19D
      9A00E8E6E400ECE7E40098949200000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00F9F9F900F8F8F800F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F7F7
      F700F7F7F700F6F6F600F6F6F600FBFBFB00E9E9E900F2F2F200DDDDDD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000456F9800A3C7E400A0C7
      E600A0C7E6009FC6E5008AA5B9006C7276007E7F7F00ADADAD00E0E0E000D6D6
      D600E4E4E400E3E3E300D6D6D600DBDBDB00A4A4A400787878006F767C0090AE
      C6009FC6E5009FC7E600A0C7E6007AA9D0004B739B0000000000000000000000
      00000000000000000000000000000000000000000000AFAFAF00E0E0E000E0E0
      E000E0E0E000E0E0E000CBCBCB00ADADAD00B4B4B400CFCFCF00EDEDED00E7E7
      E700EFEFEF00EEEEEE00E7E7E700EAEAEA00CACACA00B0B0B000AFAFAF00D1D1
      D100E0E0E000E0E0E000E0E0E000D0D0D000B1B1B10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A7A1A000E3E0DE00F4F2EF00C1BEBC009A948F00A49B8900B2A6
      8B00BAAD8C00BEAF8D00BDAF9100B6AB9100A9A090009B959000C2BFBE00F5F2
      F100E6E1DF009B96940000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00F2F2F200DEDEDE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000097AFC60048729A004872
      9B0048729B0048729B0048729B0048729A004E6E8D00606E7B009B9C9C00F0F0
      F0008282820097989800F0F0F000868686005B6D7F004C6E900048729B004872
      9B0048729B0048729B0048729B0049729A00A0B5C80000000000000000000000
      00000000000000000000000000000000000000000000D2D2D200B1B1B100B1B1
      B100B1B1B100B1B1B100B1B1B100B1B1B100ADADAD00ACACAC00C5C5C500F6F6
      F600B6B6B600C2C2C200F6F6F600B8B8B800ACACAC00AEAEAE00B1B1B100B1B1
      B100B1B1B100B1B1B100B1B1B100B0B0B000D5D5D50000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B7B3B100C2BFBC00F7F4F300F3F1EF00D0CECC00B7B4
      B100A9A4A200A39F9C00A9A4A200B7B4B300D2CECD00F4F2F100F8F5F400C4C1
      BF00A9A6A5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008A8A8A00EDED
      ED00A0A0A000B4B4B400E5E5E500828282000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BBBBBB00F4F4
      F400C7C7C700D3D3D300EFEFEF00B6B6B6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ABA6A500C0BBBA00E3E1DF00F9F7
      F600FDFBF900FCFBF900FDFBF900FBF8F700E4E1E000C1BEBB00A9A4A3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BCBDBD008888
      880085858500858585008A8A8A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D8D8D800B9B9
      B900B8B8B800B8B8B800BBBBBB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B8B4
      B200A9A6A300AAA5A200A9A6A200B7B3B100D1CECD0000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004343
      7C0032327C0058587F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009B9B
      9B0094949400A3A3A30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B4B4B4009191
      9100888888008282820082828200828282008282820082828200828282008282
      8200828282008282820082828200828282008282820082828200828282008282
      8200828282009393930000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009E9E
      9E008F8F8F00A9A9A90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000555599003333B5005555
      E1006464EC004848D0002B2B8E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A8009E9E9E00B4B4
      B400BCBCBC00ACACAC0094949400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9A9A00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB008686860000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A9A9A900A5A5
      A500C0C0C0009F9F9F00B6B6B600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006C6CB3004141C6006262E6006B6B
      D7006161CA006464E2004D4DDA0042428B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B6B6B600A7A7A700BABABA00BBBB
      BB00B5B5B500BABABA00B0B0B0009D9D9D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9A9A00FBFB
      FB00FDFDFD00FCFCFC00FCFCFC00FCFBFB00FBFBFB00FBFBFA00FAFAFA00FAFA
      F900F9F9F900F9F9F800F8F8F800F8F7F700F7F7F700F7F6F600F6F6F500F6F5
      F500FBFBFB008686860000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A800A5A5A500C3C3
      C300C6C6C600C1C1C10096969600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002E2EAB004E4EDF003636B3005F5F
      AB00000000002424A7004D4DE1002727A2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009B9B9B00B1B1B1009F9F9F00AEAE
      AE000000000096969600B1B1B100969696000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9A9A00FBFB
      FB00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FBFAFA00FAFAFA00FAF9F900F9F9
      F900F9F8F800F8F8F700F7F7F700F7F7F600F6F6F600F6F5F500F5F5F500F5F4
      F400FBFBFB008686860000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ADADAD00A5A5A500C3C3C300C7C7
      C700CDCDCD00A4A4A400C2C2C200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000004848A3005151E0003232C4004B4B8C000000
      00000000000024249F004C4CE1002D2DAE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A4A4A400B2B2B200A1A1A100A1A1A1000000
      00000000000094949400B0B0B0009B9B9B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9A9A00FBFB
      FB00FCFBFB00FBFBFB00FBFBFA00FAFAFA00FAFAF900F9F9F900F9F9F800F8F8
      F800F8F7F700F7F7F700F7F6F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3
      F300FBFBFB008686860000000000000000000000000000000000000000000000
      0000000000000000000000000000BBBBBB00A2A2A200C3C3C300C6C6C600CDCD
      CD00A9A9A900B9B9B90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002B2BA8004E4EE2002323A400000000000000
      0000000000003636B7004949DD002222A0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099999900B1B1B10095959500000000000000
      000000000000A0A0A000AEAEAE00949494000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000099999900FBFB
      FB00FBFBFB00FBF9F800F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B47100F4F3F300F3F3
      F200FBFBFB008686860000000000000000000000000000000000000000000000
      00000000000000000000000000009E9E9E00C1C1C100C5C5C500CDCDCD00B1B1
      B100AFAFAF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003839BD004C4EE0001E1F9800000000000000
      00003C3C8C005354E0003C41D10037378D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A2A2A200B1B1B10091919100000000000000
      00009B9B9B00B3B3B300A9A9A9009A9A9A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000094949400FBFB
      FB00FAFAFA00FAFAF900F9F9F900F9F9F800F8F8F800F8F7F700F7F7F700F7F6
      F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3F300F3F3F200F3F2F200F2F2
      F100FBFBFB008686860000000000000000000000000000000000000000000000
      000000000000000000009D9D9D00BCBCBC00C4C4C400CBCBCB00BBBBBB00A4A4
      A400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003136B6005257E4002D2DAA00000000004444
      87004040C5004E57E100252CA800000000006D6D93002B2B95002828A4002525
      9E0045458B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009F9F9F00B5B5B5009A9A9A00000000009D9D
      9D00A6A6A600B4B4B4009999990000000000B0B0B00096969600979797009494
      94009F9F9F00000000000000000000000000000000000000000000000000FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FAFAFA009A9A9A00FBFB
      FB00FAF9F900F9F9F900F9F8F800F8F8F700F7F7F700F7F7F600F6F6F600F6F5
      F500F5F5F500F5F4F400F4F4F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1
      F000FBFBFB008686860000000000000000000000000000000000000000000000
      000000000000A4A4A400B4B4B400C4C4C400C7C7C700C4C4C4009E9E9E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000232797005869E200595BE6003B3BBF004D4D
      D8005760E8003F4EC70015158200252596004646CE006C6FEE007A7BF2007477
      EF005455DC002525910000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000094949400BBBBBB00B7B7B700A3A3A300AFAF
      AF00B9B9B900ACACAC008989890094949400AAAAAA00C0C0C000C6C6C600C4C4
      C400B3B3B300929292000000000000000000000000000000000000000000FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FDFCFC00FCFCFC00FAFAFA009A9A9A00FBFB
      FB00F9F9F800F8F7F600F7B57400F7B57400F7B57300F7B57300F7B57300F7B5
      7300F6B57300F6B57300F6B57300F6B57300F6B57300F6B77700F1F0F000F0F0
      EF00FBFBFB008686860000000000000000000000000000000000000000000000
      0000C3C3C300A9A9A900C4C4C400C5C5C500C9C9C900A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003038A7007784EB006A75ED005F6F
      EA00516EDC0015198C003737B9006469EC007780E8004E52BA003233A0003B3E
      A9005970E1004A4CD3005F5F8D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009E9E9E00C8C8C800C2C2C200BFBF
      BF00BBBBBB008C8C8C00A1A1A100BEBEBE00C6C6C600ACACAC009B9B9B00A1A1
      A100BEBEBE00AEAEAE00A9A9A90000000000000000000000000000000000FCFC
      FC00FDFDFD00FCFCFC00FBFBFB00F8F8F800F3F3F300EBEBEB0099999900FBFB
      FB00F8F8F700F7F7F700F7F7F600F6F6F600F6F5F500F5F5F500F5F4F400F4F4
      F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1F000F1F0EF00F0EFEF00F0EF
      EE00FBFBFB008686860000000000000000000000000000000000000000000000
      00009F9F9F00C3C3C300C5C5C500C9C9C900B7B7B700ADADAD00000000000000
      00000000000000000000000000009E9E9E009A9A9A009A9A9A009A9A9A009A9A
      9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A
      9A009A9A9A009A9A9A00AEAEAE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000025278F005467D3006083
      E900313FB2004C4CD7005E6EEB005568CF00393A960000000000000000000000
      00003743BA00596AEA0038389100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000092929200B8B8B800C6C6
      C600A2A2A200AEAEAE00BFBFBF00B7B7B7009B9B9B0000000000000000000000
      0000A5A5A500BDBDBD009B9B9B0000000000000000000000000000000000FCFC
      FC00FDFCFC00FCFBFB00FAFAF900F5F5F500EEEEEE00E3E3E20097979700FBFB
      FB00F7F7F700F7F6F600F6F6F500F6F5F500F5F5F400F4F4F400F4F3F300F3F3
      F200F3F2F200F2F2F100F2F1F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEE
      ED00FBFBFB00858585000000000000000000000000000000000000000000B3B3
      B300B7B7B700C8C8C800CACACA00C7C7C700A0A0A00000000000000000000000
      00000000000000000000CACACA00AAAAAA00CBCBCB00C8C8C800C8C8C800C8C8
      C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8C800C8C8
      C800C8C8C800C7C7C7009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000091919100808080005C5C7800252CA000565FE5005C5E
      EC005B61EA005D6BEC004F75DD00363786000000000000000000000000005B5B
      91004649CF005675E50047478C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BFBFBF00B5B5B500A4A4A40097979700B8B8B800B9B9
      B900BABABA00BEBEBE00BEBEBE0098989800000000000000000000000000A8A8
      A800ACACAC00BFBFBF00A0A0A00000000000000000000000000000000000FCFC
      FC00FCFCFC00FBFBFB00F9F9F900F5F5F500EDEDED00E1E1E00097979700FBFB
      FB00F6F6F600F6F5F300F7B57300F7B57300F6B57300F6B57300F6B57300F6B5
      7300F6B57300F6B57300F6B57300F6B57300F6B57300F6B67700EEEEED00EEED
      EC00FBFBFB00848484000000000000000000000000000000000000000000A4A4
      A400CACACA00CBCBCB00CDCDCD00B4B4B400B9B9B90000000000000000000000
      0000000000000000000000000000BABABA00AEAEAE00D4D4D400CECECE00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCB
      CB00CBCBCB00CACACA009D9D9D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000828181009E9B9A00BDB7B500D1C9C600C8C0C50035368B005B77D8007C96
      F2007887E500698AED00414AC800717190000000000000000000464688003737
      B9006177EC00374FB70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B6B6B600C5C5C500D5D5D500DFDFDF00DBDBDB0098989800BEBEBE00D0D0
      D000C8C8C800C9C9C900AAAAAA00B1B1B10000000000000000009E9E9E00A1A1
      A100C2C2C200A9A9A9000000000000000000000000000000000000000000FCFC
      FC00FCFCFC00FBFAFA00F7D2AD00F3CEAA00EBC8A500DFBF9D0097949000FBFB
      FB00F6F5F500F5F5F400F4F4F400F4F3F300F3F3F200F3F2F200F2F2F100F2F1
      F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEEED00EEEDED00EEEDEC00EDEC
      EC00FBFBFB008484840000000000000000000000000000000000B9B9B900BCBC
      BC00CDCDCD00CECECE00CDCDCD00A2A2A2000000000000000000000000000000
      000000000000000000000000000000000000BBBBBB00B1B1B100D6D6D600D0D0
      D000CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCD
      CD00CDCDCD00CDCDCD009F9F9F00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000086868600BDBB
      B900E5E0DD00E6E0DD00E3DCD900E1D9D600D6CECA00ABA4AD002E2F8E004857
      BD00161A87005482E2005960E8002B2BA800222297002D2DAB004E51DA00707C
      EF00556ACE005355A70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B8B8B800D6D6
      D600ECECEC00ECECEC00EAEAEA00E8E8E800E2E2E200CBCBCB0096969600ADAD
      AD008C8C8C00C3C3C300B9B9B90099999900939393009A9A9A00B1B1B100C6C6
      C600B8B8B800AAAAAA000000000000000000000000000000000000000000FCFC
      FC00FCFBFB00FBFAFA00F8F8F800F3F3F300ECECEB00DFDFDF0097979700FBFB
      FB00F5F4F400F4F4F300F4F3F300F3F3F200F2F2F100F2F1F100F1F1F000F1F0
      EF00F0EFEF00F0EFEE00EFEEEE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECEC
      EB00FBFBFB008484840000000000000000000000000000000000A3A3A300CFCF
      CF00D1D1D100D1D1D100C0C0C000B0B0B0000000000000000000000000000000
      00000000000000000000000000000000000000000000BABABA00B4B4B400D8D8
      D800D2D2D200D1D1D100D1D1D100D1D1D100D1D1D100D1D1D100D1D1D100D1D1
      D100D1D1D100D0D0D0009F9F9F00000000000000000000000000000000000000
      00000000000000000000000000000000000084848400B5B4B400EFECEB00EFEC
      EB00ECE9E600E9E4E200E1DBD800B7B3B100918F8E00D3CCC800C6BEC6004948
      7F00000000002939A200728CEF006F74EF006C6EEF00747EF0007E8DED00414C
      B3005E60AC000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B7B7B700D3D3D300F3F3F300F3F3
      F300F1F1F100EEEEEE00E9E9E900D2D2D200BDBDBD00E0E0E000DBDBDB009D9D
      9D00000000009C9C9C00CBCBCB00C3C3C300C0C0C000C6C6C600CBCBCB00A8A8
      A800AFAFAF00000000000000000000000000000000000000000000000000FCFC
      FC00FBFBFB00FAFAFA00F8F7F700F3F3F300EBEBEB00DFDFDF0097979700FBFB
      FB00F4F3F300F3F2F000F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00F0E7DF00EFEEED00EEEDED00EEEDEC00EDECEC00EDECEB00ECEBEB00ECEB
      EA00FBFBFB008484840000000000000000000000000000000000B4B4B400D2D2
      D200D2D2D200D3D3D300B3B3B300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B8B8B800B6B6
      B600D9D9D900D4D4D400D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200D2D2D200A0A0A000000000000000000000000000000000000000
      000000000000000000000000000094949400E5E5E500F9F8F700F6F4F400F3F0
      EF00E2DFDD00ADABAA0092919100000000008C8A8A00E1D9D600DFD7D2008786
      850000000000000000002C329B006268CA00676BCE00454AB1003C3D9A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000EFEFEF00FAFAFA00F8F8F800F6F6
      F600EBEBEB00CDCDCD00BFBFBF0000000000BBBBBB00E8E8E800E7E7E700B7B7
      B700000000000000000099999900B7B7B700B9B9B900A7A7A7009D9D9D000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00FBFBFB00FAF9F900F7D4B100F3D1AE00EBCAA800DFC0A00097949100FBFB
      FB00F3F3F200F2F2F100F2F1F100F1F1F000F1F0EF00F0EFEF00F0EFEE00EFEE
      EE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECECEB00ECEBEA00ECEBEA00EBEA
      E900FBFBFB0084848400000000000000000000000000BFBFBF00C3C3C300D4D4
      D400D4D4D400D4D4D400AAAAAA00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A3A3
      A300C7C7C700D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D2D2D200A2A2A200000000000000000000000000000000000000
      0000000000008E8E8E00B2B2B200FCFCFC00FEFEFE00FCFCFB00E5E4E400A7A6
      A6009F9F9F00000000000000000000000000BAB6B400E3DCD900DFD8D3007C7C
      7C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BDBDBD00D2D2D200FDFDFD00FEFEFE00FCFCFC00EFEFEF00CBCB
      CB00C7C7C700000000000000000000000000D4D4D400EAEAEA00E7E7E700B2B2
      B200000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00FAFAFA00F9F9F900F7F7F600F2F2F200EBEAEA00DFDEDE0097979700FBFB
      FB00F2F2F100F2F1F000F1F0F000F0F0EF00F0EFEF00EFEFEE00EFEEED00EEED
      ED00EEEDEC00EDECEC00EDECEB00F7F7F600FAF9F900F9F9F900F9F9F900F9F9
      F900FCFCFC0084848400000000000000000000000000AAAAAA00CECECE00D4D4
      D400D4D4D400D4D4D400A6A6A600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A7A7A700BFBF
      BF00D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D2D2D200A2A2A200000000000000000000000000000000000000
      000087878700C9C9C900FFFFFF00FFFFFF00E1E1E100A2A2A200ACACAC000000
      000000000000000000000000000087878600E6E1DF00E6E0DD00CCC6C3008B8B
      8B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900DFDFDF00FFFFFF00FFFFFF00EDEDED00C9C9C900CECECE000000
      0000000000000000000000000000B8B8B800EDEDED00ECECEC00DDDDDD00BCBC
      BC00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00FAFAFA00F9F9F900F6F6F600F2F2F200EAE9E900DEDEDD0097979700FBFB
      FB00F1F1F000F1EFED00F7B26D00F7B26D00F7B26D00F7B26D00F7B26D00F7B2
      6D00EEE5DC00ECECEB00ECEBEA00F9F8F800A7A7A5009595930095959300BBBB
      BA00CFCFCE008E8E8D00000000000000000000000000A3A3A300D3D3D300D4D4
      D400D4D4D400D4D4D400AAAAAA00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A700C5C5C500D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D2D2D200A3A3A300000000000000000000000000000000008989
      8900DCDCDC00FDFDFD00D5D5D5009D9D9D00B7B7B70000000000000000000000
      0000000000000000000000000000B6B5B400ECE9E600E9E4E200AAA7A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BABA
      BA00EAEAEA00FDFDFD00E6E6E600C5C5C500D4D4D40000000000000000000000
      0000000000000000000000000000D3D3D300F1F1F100EEEEEE00CBCBCB000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00FAF9F900F9F7F700F7D4B100F3D0AE00EBCAA900DFC0A00097949100FBFB
      FB00F0F0EF00F0EFEF00EFEFEE00EFEEED00EEEDED00EEEDEC00EDECEC00EDEC
      EB00ECEBEB00ECEBEA00EBEAEA00F8F8F7009C9C9A00F6F6F600FAF9F900D8D8
      D7009393910000000000000000000000000000000000A6A6A600D4D4D400D4D4
      D400D4D4D400D4D4D400B7B7B700BBBBBB000000000000000000000000000000
      0000000000000000000000000000B0B0B000AEAEAE00CDCDCD00D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D6D6D600D5D5D500D4D4D400D4D4
      D400D4D4D400D2D2D200A4A4A4000000000000000000000000008E8E8E00DDDD
      DD00C9C9C9009797970000000000000000000000000000000000000000000000
      0000000000000000000088888800EDEBEA00EFECEB00E9E4E200858484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDBDBD00EBEB
      EB00DFDFDF00C2C2C20000000000000000000000000000000000000000000000
      00000000000000000000BABABA00F2F2F200F3F3F300EEEEEE00B7B7B7000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F9F9F900F8F8F700F6F5F500F1F1F100EAEAEA00DEDDDD0097979700FBFB
      FB00F0EFEE00EFEEEE00EEEEED00EEEDEC00EDEDEC00EDECEB00ECECEB00ECEB
      EA00ECEBEA00EBEAE900EBEAE900F8F8F7009C9C9A00F2F2F100D9D9D8009494
      92000000000000000000000000000000000000000000A6A6A600D3D3D300D4D4
      D400D4D4D400D4D4D400CECECE00A9A9A900BCBCBC0000000000000000000000
      000000000000B2B2B200A9A9A900C1C1C100D3D3D300D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400DADADA00D8D8D800DBDBDB00D5D5D500D4D4
      D400D4D4D400D2D2D200A5A5A50000000000000000008F8F8F009F9F9F009F9F
      9F00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00F6F4F400F3F0EF00B9B6B500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BDBDBD00C7C7C700C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D8D8D800F8F8F800F6F6F600D4D4D400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F9F9F800F7F7F700F5F5F500F1F1F100EAEAEA00DFDFDF0097979700FBFB
      FB00EFEEED00EEEDED00EEEDEC00EDECEC00EDECEB00ECEBEB00ECEBEA00EBEA
      EA00EBEAE900EBEAE900EAE9E800F8F7F700BEBEBD00DADAD900979795000000
      00000000000000000000000000000000000000000000B3B3B300CCCCCC00D4D4
      D400D4D4D400D4D4D400D4D4D400CECECE00B6B6B600A9A9A900A6A6A600AAAA
      AA00B5B5B500C4C4C400D2D2D200D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D5D5D500DBDBDB00CBCBCB00A8A8A800BDBDBD00DBDBDB00D5D5
      D500D4D4D400D3D3D300A6A6A6000000000000000000BEBEBE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008B8B8B00F6F6F500F9F8F700EAE8E80087878700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D9D9D900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BBBBBB00F9F9F900FAFAFA00F1F1F100B9B9B900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F8F8F700F7F6F500F7D3AE00F4D0AC00EECBA700E5C3A10098949100FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FCFCFC00D8D8D70098989600000000000000
      00000000000000000000000000000000000000000000CDCDCD00BEBEBE00D5D5
      D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D7D7D700DCDCDC00C3C3C300B0B0B00000000000BBBBBB00BDBDBD00DBDB
      DB00D5D5D500D3D3D300A7A7A700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C3C3C300FEFEFE00FBFBF9009C9C9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DCDCDC00FEFEFE00FCFCFC00C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F8F7F700F7F6F600F5F5F500F2F2F200EDEDED00E7E7E600AFAFAF009696
      9600959595009595950095959500959595009595950095959500959595009696
      9600999999009393930099999900999999009B9B9A0000000000000000000000
      0000000000000000000000000000000000000000000000000000ACACAC00D4D4
      D400D5D5D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5D500DBDB
      DB00D8D8D800B5B5B500BDBDBD00000000000000000000000000BEBEBE00BDBD
      BD00DBDBDB00D4D4D400A8A8A800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008D8D8D00FBFBFB00FFFFFF00BCBCBC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BCBCBC00FCFCFC00FFFFFF00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F7F7F700F7F6F600F5F5F500F3F3F300F0F0EF00EBEAEA00E5E5E500E1E1
      E000DDDDDD00DBDBDB00DBDAD900DFDFDF00E1E0E000E1E1E100E3E3E300EBEB
      EB00FDFDFD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CBCBCB00BABA
      BA00DBDBDB00D6D6D600D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5D500DBDBDB00DDDDDD00C6C6
      C600ACACAC00000000000000000000000000000000000000000000000000BFBF
      BF00BDBDBD00D7D7D700A9A9A900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C8C8C800FFFFFF00D5D5D5008D8D8D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DFDFDF00FFFFFF00E6E6E600BCBCBC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F7F6F600F6F6F500F9D4AF00F8D3AE00F6D2AD00F3CFAA00F0CCA900EEC9
      A600E7E3DE00E5E5E400E4E4E400EBEBEB00C2C2C100B9B9B800BABAB900D3D3
      D200E5E5E4000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BFBF
      BF00BCBCBC00DCDCDC00DBDBDB00D5D5D500D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D8D8D800DCDCDC00DEDEDE00C9C9C900B0B0B000C3C3
      C300000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000BDBDBD00ACACAC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008E8E
      8E00FBFBFB00DFDFDF008C8C8C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BDBD
      BD00FCFCFC00ECECEC00BCBCBC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F6F6F600F6F6F500F5F5F500F4F4F400F3F3F200F1F1F100F0F0EF00EFEE
      ED00EDEDEC00ECECEB00ECECEB00F2F2F200C3C3C200F2F2F200F5F5F500E4E4
      E300C5C5C4000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CBCBCB00B0B0B000C9C9C900DDDDDD00E0E0E000DEDEDE00DEDEDE00DEDE
      DE00E0E0E000DFDFDF00D2D2D200C0C0C000ADADAD00C3C3C300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C9C9C90000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C3C3
      C300E4E4E4008A8A8A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DCDC
      DC00EFEFEF00BABABA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F6F6F500F6F5F500F5F5F500F5F4F400F4F3F300F3F3F200F3F2F200F1F1
      F000F1F1F000F0F0F000F0F0EF00F7F7F700C6C6C500F4F4F400E7E7E700C5C5
      C400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C9C9C900AFAFAF00B3B3B300B9B9B900BCBCBC00B9B9
      B900B2B2B200ADADAD00C2C2C200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000093939300DFDF
      DF008C8C8C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000ECEC
      EC00BCBCBC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F6F5F500F5F5F400F5F5F400F5F4F400F4F4F400F4F4F300F3F3F200F3F3
      F200F3F2F200F2F2F100F2F2F100F9F9F900DBDBDA00E9E9E900C6C6C5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A0A0A0008E8E
      8E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7C7C700BDBD
      BD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FDFDFD00E9E9E900C7C7C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B4B4B400939393000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D3D3D300C0C0C0000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000ABABAB00ACACAC0000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000009494
      9400B6B6B600B5B5B50099999900000000000000000000000000000000000000
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
      00008C8C8C007D7D7D0076767600737373007373730073737300737373007373
      7300737373007373730073737300737373007373730073737300737373007373
      73007373730073737300737373007373730082828200A6A6A6009B9B9B00B6B6
      B600BBBBBB00BCBCBC00B5B5B500B2B2B2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000987C6E007942
      2500785645000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B2B2B2008F8F
      8F009C9C9C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007C5B
      4A0079422500856E630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008787
      8700898989009696960073737300D4D4D400DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E0E0E000BABA
      BA00BABABA00B9B9B900B8B8B800BFBFBF00C3C3C300A9A9A900CBCBCB00BEBE
      BE00CDCDCD00CBCBCB00B7B7B700B9B9B9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C29E8C00A25D3A00E197
      7200AE6945008E644E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C5C5C5009F9F9F00C0C0
      C000A6A6A600A3A3A30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000956C5800AC67
      4400E1967200A25D3A009C847700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000878787008989
      89008D8D8D008686860073737300DFDFDF00DDDDDD00D8D8D800AFAFAF00AFAF
      AF00B1B1B100DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E9E9E900C2C2
      C200C1C1C100C0C0C000C6C6C600C6C6C600A7A7A700BFBFBF00E8E8E800DCDC
      DC00C0C0C000C1C1C1009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000924E2A00E0987300E9A1
      7C00E79C7600B16A460090665000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000096969600C1C1C100C6C6
      C600C3C3C300A6A6A600A5A5A500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000966D5700AF694400E69C
      7600E9A17C00E0987300924E2A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008B8B8B00898989008D8D
      8D00868686008686860072727200DFDFDF00DFDFDF00D9D9D900AEAEAE00ABAB
      AB00ACACAC00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00EAEAEA00C5C5
      C500C4C4C400CACACA00C7C7C700A9A9A900E2E2E200D0D0D000DBDBDB00E8E8
      E800C9C9C900A2A2A20000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C29C8700AA674300ECAC
      8B00EAA37E00E79C7600AF694400956D58000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C300A4A4A400CDCD
      CD00C7C7C700C3C3C300A5A5A500A9A9A9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C766100AF684300E79C7600EAA3
      7E00ECAC8B00AA674300BD988400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007D7D7D00B1B1B1008686
      8600868686008686860072727200DFDFDF00E4E4E400DFDFDF00B4B4B400B0B0
      B000AEAEAE00E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400EDEDED00C3C3
      C300C9C9C900C8C8C800AAAAAA00E2E2E200E4E4E400E3E3E300D3D3D300C1C1
      C100B3B3B300A3A3A30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B98A7100B370
      4B00EDAD8C00E9A07B00E69B7500AA633E00997E6F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B9B9B900A9A9
      A900CDCDCD00C6C6C600C3C3C300A2A2A200B2B2B20000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A98D7E00AA633E00E69B7500E9A07B00EDAD
      8C00B3704B00B88A710000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860072727200DEDEDE00E9E9E900E4E4E400BABABA00B6B6
      B600B4B4B400E9E9E900E9E9E900E9E9E900E9E9E900E9E9E900F0F0F000C3C3
      C300C6C6C600AAAAAA00E3E3E300E4E4E400E3E3E300EBEBEB00D1D1D100A9A9
      A900B1B1B1008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B37F
      6200C17D5800EDAC8A00E89E7800E3987200A35D350000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B2B2
      B200B1B1B100CDCDCD00C5C5C500C1C1C1009E9E9E0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A35D3500E3987200E89E7800EDAC8A00C17D
      5800AD7A5E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860072727200DCDCDC00EEEEEE00E9E9E900BFBFBF00BCBC
      BC00BBBBBB00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00F3F3F300C2C2
      C200ACACAC00E3E3E300E5E5E500E4E4E400EBEBEB00D1D1D100A3A3A300A7A7
      A700999999008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A6684400D38F6A00ECA98600E89D7700DB916A009C593200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A5A500BBBBBB00CBCBCB00C4C4C400BCBCBC009B9B9B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009F5B3400DB916A00E89D7700ECA98600D38F6A00A567
      4300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860073737300DBDBDB00F1F1F100ECECEC00C1C1C100C0C0
      C000C0C0C000F1F1F100F1F1F100F1F1F100F1F1F100F3F3F300E1E1E100ADAD
      AD00E3E3E300E7E7E700E5E5E500EBEBEB00D1D1D100A1A1A100A7A7A7008B8B
      8B00969696007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A25E3500E29E7900EAA47F00E89D7700CC835B009A6342000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009E9E9E00C4C4C400C7C7C700C4C4C400B4B4B400A1A1A1000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A0684600CC835B00E89D7700EAA47F00E29E7900A25D34000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860073737300DBDBDB00F2F2F200ECECEC00C2C2C200C2C2
      C200C3C3C300F2F2F200F2F2F200F2F2F200F3F3F300E1E1E100AFAFAF00E3E3
      E300E9E9E900EBEBEB00EEEEEE00D2D2D200A8A8A800999999008B8B8B008686
      8600969696007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B16A4000EBA68200E89F7A00E89D7700B97046009F81
      6E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A5A5A500C9C9C900C5C5C500C4C4C400A9A9A900B4B4
      B400000000000000000000000000000000000000000000000000000000000000
      0000C29B8300B9704700E89D7700E89F7A00EBA68300B16A4000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007D7D7D00BABABA008686
      8600868686008686860073737300CBCBCB00DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00E4E4E400DDDDDD00B0B0B000E3E3E300E9E9
      E900ECECEC00F0F0F000D6D6D600A9A9A900BBBBBB0077777700868686008787
      8700969696007F7F7F00000000000000000000000000BB8B6C00A0582B00A158
      2B00A1582B00A1582B00A1582B00A1582B00A1582B00A1582B00A1582B00A158
      2B00A1582B00A1582B00A1582B00A1582B00A45E330000000000000000000000
      00000000000000000000B77F5B00CE865E00EAA47F00E89D7700E3987200A75F
      33000000000000000000000000000000000000000000B8B8B8009A9A9A009A9A
      9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A
      9A009A9A9A009A9A9A009A9A9A009A9A9A009E9E9E0000000000000000000000
      00000000000000000000B1B1B100B6B6B600C7C7C700C4C4C400C1C1C1009F9F
      9F00000000000000000000000000000000000000000000000000000000000000
      0000A8613500E39C7500E8A07A00EAA78300CE886000AC775600000000000000
      0000000000000000000000000000A45F3400A1582B00A1582B00A1582B00A158
      2B00A1582B00A1582B00A1582B00A1582B00A1582B00A1582B00A1582B00A158
      2B00A1582B00A0582B00A2795F0000000000000000007D7D7D00BBBBBB008989
      8900898989008989890079797900727272007272720072727200727272007272
      7200737373007E7E7E0095959500ADADAD00AEAEAE00E3E3E300EAEAEA00ECEC
      EC00F0F0F000D6D6D600A3A3A30098989800777777007A7A7A00898989008989
      8900979797007F7F7F00000000000000000000000000A45D2E00E79C7600E89D
      7700E89D7700E89D7700E89D7700E89D7700E89D7700E89D7700E89D7700E89D
      7700E89D7700E89D7700E89D7700E8A27E00B9714400C8A48C00000000000000
      0000000000000000000000000000AA623400E59D7800E9A07B00E89D7700CD84
      5A00A1715100000000000000000000000000000000009C9C9C00C3C3C300C4C4
      C400C4C4C400C4C4C400C4C4C400C4C4C400C4C4C400C4C4C400C4C4C400C4C4
      C400C4C4C400C4C4C400C4C4C400C7C7C700A9A9A900C8C8C800000000000000
      0000000000000000000000000000A0A0A000C4C4C400C6C6C600C4C4C400B4B4
      B400A9A9A900000000000000000000000000000000000000000000000000B982
      5F00CD895F00E8A67F00E9A98300E5A57F00AA63350000000000000000000000
      00000000000000000000CEA88F00B9734600E8AA8500E8A67F00E8A67F00E8A6
      7F00E8A67F00E8A67F00E8A67F00E8A67F00E8A67F00E8A67F00E8A67F00E8A6
      7F00E8A67F00E7A57E00A45C2E0000000000000000007D7D7D00BDBDBD008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C
      8C0096969600B4B4B400D0D0D000C9C9C900E7E7E700EAEAEA00EDEDED00F1F1
      F100D8D8D800A8A8A800ABABAB00909090008C8C8C008C8C8C008C8C8C008C8C
      8C009A9A9A0080808000000000000000000000000000A75F2E00E79E7800E89F
      7A00E89F7A00E89F7A00E89F7A00E89F7A00E89F7A00E89F7A00E89F7A00E89F
      7A00E89F7A00EAA68100EEB29000BE794C00C18E6B0000000000000000000000
      0000000000000000000000000000CB9D7F00C87F5300E9A37F00E89F7A00E79F
      7900B1683800000000000000000000000000000000009D9D9D00C4C4C400C5C5
      C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5
      C500C5C5C500C9C9C900D0D0D000ADADAD00BABABA0000000000000000000000
      0000000000000000000000000000C3C3C300B1B1B100C7C7C700C5C5C500C5C5
      C500A3A3A300000000000000000000000000000000000000000000000000B16A
      3A00E7AB8300E8AC8400E9AF8900C8855900B08C730000000000000000000000
      0000000000000000000000000000C28E6C00BE7B4F00EEBA9700EAB18B00E8AC
      8400E8AC8400E8AC8400E8AC8400E8AC8400E8AC8400E8AC8400E8AC8400E8AC
      8400E8AC8400E7AA8300A75E2E0000000000000000007D7D7D00BFBFBF009090
      9000909090009090900090909000909090009090900090909000909090009292
      9200A6A6A600CDCDCD00B9B9B900E7E7E700F0F0F000EFEFEF00F1F1F100D8D8
      D800A9A9A900AEAEAE0094949400909090008F8F8F008F8F8F008F8F8F008F8F
      8F009B9B9B0080808000000000000000000000000000AA612F00E7A27C00E8A3
      7D00E8A37D00E8A37D00E8A37D00E8A37D00E8A37D00E8A37D00E8A37D00E8A3
      7D00EAA98400EEB59300C27D5000C38E6B000000000000000000000000000000
      000000000000000000000000000000000000AF663400E6A37D00E8A57F00E8A3
      7D00D18A5F00A67859000000000000000000000000009F9F9F00C6C6C600C7C7
      C700C7C7C700C7C7C700C7C7C700C7C7C700C7C7C700C7C7C700C7C7C700C7C7
      C700CACACA00D1D1D100B0B0B000BABABA000000000000000000000000000000
      000000000000000000000000000000000000A2A2A200C7C7C700C8C8C800C7C7
      C700B8B8B800AEAEAE0000000000000000000000000000000000C18C6800D193
      6700E8B08900E8B28B00E7AF8800AE6735000000000000000000000000000000
      000000000000000000000000000000000000C48F6B00C2805300EEBE9B00EAB5
      8F00E8B08900E8B08900E8B08900E8B08900E8B08900E8B08900E8B08900E8B0
      8900E8B08900E7AF8800AA612F0000000000000000007D7D7D00C2C2C2009494
      9400949494007676760072727200717171007171710070707000707070007B7B
      7B009D9D9D00B9B9B900C5C5C500C2C2C200F5F5F500F6F6F600DEDEDE00A6A6
      A600979797007575750071717100727272007373730075757500919191009393
      93009E9E9E0080808000000000000000000000000000AE632F00E7A78000E8A8
      8100E8A88100E8A88100E8A88100E8A88100E8A88100E8A88100E8A88100EAAD
      8800EEB89600C5815300BF8A6500000000000000000000000000000000000000
      000000000000000000000000000000000000C48D6700D5916600E8AA8300E8A8
      8100E6A67F00B16734000000000000000000000000009F9F9F00C9C9C900C9C9
      C900C9C9C900C9C9C900C9C9C900C9C9C900C9C9C900C9C9C900C9C9C900CCCC
      CC00D2D2D200B1B1B100B8B8B800000000000000000000000000000000000000
      000000000000000000000000000000000000B9B9B900BBBBBB00CACACA00C9C9
      C900C8C8C800A3A3A30000000000000000000000000000000000B2693500EAB4
      8B00ECB68E00EDB79000D89B6E00AD7D5B000000000000000000000000000000
      00000000000000000000000000000000000000000000C48F6900C6855600F1C2
      9F00EEB99300ECB68E00ECB68E00ECB68E00ECB68E00ECB68E00ECB68E00ECB6
      8E00ECB68E00EBB58C00AD632F0000000000000000007D7D7D00C4C4C4009999
      99009999990073737300F9F9F900FDFDFD00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FDFDFD00CACACA00D9D9D900D3D3D300C3C3C300E2E2E200CECECE00E7E7
      E700DCDCDC00D8D8D800D7D7D700D8D8D800DBDBDB0074747400959595009797
      9700A2A2A2007F7F7F00000000000000000000000000B1652F00E7AB8400E8AC
      8500E8AC8500E8AC8500E8AC8500E8AC8500E8AC8500E8AC8500E9AF8A00EEBB
      9800C8855600A779560000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C5804F00E8AD8600E8AC
      8500E8AC8500C6815000000000000000000000000000A0A0A000CBCBCB00CBCB
      CB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CBCBCB00CDCDCD00D4D4
      D400B4B4B400ADADAD0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B1B1B100CCCCCC00CBCB
      CB00CBCBCB00B1B1B10000000000000000000000000000000000CA865500F1B9
      9100F1B99100F1BA9200C9855300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C28C6500CA89
      5A00F5C4A100F2BC9500F1B99100F1B99100F1B99100F1B99100F1B99100F1B9
      9100F1B99100F0B89000B0652F0000000000000000007D7D7D00C7C7C7009D9D
      9D009D9D9D0073737300F8F8F800FCFCFC00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00F1F1F100CBCBCB00E1E1E100D4D4D400BFBFBF00CDCDCD00E6E6E600D1D1
      D100C9C9C900CBCBCB00CFCFCF00D4D4D400E0E0E000737373009A9A9A009C9C
      9C00A8A8A8007F7F7F00000000000000000000000000B4683000E7AF8700E8B0
      8900E8B08900E8B08900E8B08900E8B08900E8B08900E8B08900E8B08900DB9F
      7400B36831000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD743E00E8B18A00E8B0
      8900E8B08900D7996C00A97E5E000000000000000000A2A2A200CDCDCD00CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00C3C3
      C300A2A2A2000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A9A9A900CECECE00CDCD
      CD00CDCDCD00BFBFBF00B1B1B1000000000000000000CB977100DEA07300F2BB
      9300F2BB9300F2BB9400BE764000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B369
      3100E2A67B00F2BB9300F2BB9300F2BB9300F2BB9300F2BB9300F2BB9300F2BB
      9300F2BB9300F1B99100B367300000000000000000007D7D7D00C9C9C900A1A1
      A100A1A1A10073737300F8F8F800FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFD
      FD00C5C5C500D2D2D200C5C5C500C9C9C900E4E4E400E2E2E200D6D6D600CDCD
      CD00CECECE00D2D2D200D9D9D900DFDFDF00EDEDED0070707000A0A0A000A1A1
      A100ACACAC007F7F7F00000000000000000000000000B76A3000EBB48C00ECB5
      8D00ECB58D00ECB58D00ECB58D00ECB58D00ECB58D00ECB58D00ECB58D00ECB5
      8D00D6966700B06E3D0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BB6F3700ECB58D00ECB5
      8D00ECB58D00E6AD8300B8733F000000000000000000A3A3A300D0D0D000D0D0
      D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000BEBEBE00A6A6A60000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A6A6A600D0D0D000D0D0
      D000D0D0D000CBCBCB00A8A8A8000000000000000000BC774300EBB28900F2BB
      9400F2BB9400F2BB9400BC703700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B2703F00D999
      6A00F2BB9400F2BB9400F2BB9400F2BB9400F2BB9400F2BB9400F2BB9400F2BB
      9400F2BB9400F1B99200B669300000000000000000007D7D7D00CBCBCB00A5A5
      A500A5A5A50073737300F6F6F600F9F9F900FBFBFB00FCFCFC00FDFDFD00F8F8
      F800A1A1A100C5C5C500E4E4E400E4E4E400DFDFDF00D9D9D900D8D8D800DBDB
      DB00E0E0E000E3E3E300E5E5E500E8E8E800F3F3F30070707000A5A5A500A6A6
      A600B0B0B0007F7F7F00000000000000000000000000BA6C3100F0B88F00F1B9
      9100F1B99100F1B99100F1B99100F1B99100F1B99100F1B99100F1B99100F1B9
      9100F1B99100E1A27400BB713700AB9685000000000000000000000000000000
      00000000000000000000000000000000000000000000C2773E00F1B99100F1B9
      9100F1B99100F0B99100BA6C30000000000000000000A4A4A400D2D2D200D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D2D2D200C5C5C500A7A7A700C0C0C0000000000000000000000000000000
      00000000000000000000000000000000000000000000AAAAAA00D2D2D200D2D2
      D200D2D2D200D2D2D200A3A3A3000000000000000000BA6C3000F1BA9400F1BB
      9400F1BB9400F1BB9400C2773E00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BC713800E0A37700F1BB
      9400F1BB9400F1BB9400F1BB9400F1BB9400F1BB9400F1BB9400F1BB9400F1BB
      9400F1BB9400F0B99300B96C310000000000000000007D7D7D00CECECE00A9A9
      A900A9A9A90074747400F6F6F600F8F8F800F6F6F600BEBEBE00B4B4B400C1C1
      C100E6E6E600E4E4E400E0E0E000DCDCDC00D8D8D800D5D5D500D7D7D700D9D9
      D900D9D9D900D9D9D900EAEAEA00EAEAEA00F4F4F40070707000A8A8A800A9A9
      A900B3B3B3007F7F7F00000000000000000000000000BC6E3100F1B99100F2BB
      9300F2BB9300F2BB9300F2BD9600F4C09B00F2BB9300F2BB9300F2BB9300F2BB
      9300F2BB9300F2BB9300EAB08500C87E4500AF7A500000000000000000000000
      000000000000000000000000000000000000B4998400D28D5800F2BB9300F2BB
      9300F2BB9300F2BB9300BF7134000000000000000000A4A4A400D2D2D200D4D4
      D400D4D4D400D4D4D400D5D5D500D6D6D600D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400CDCDCD00AEAEAE00ADADAD0000000000000000000000
      000000000000000000000000000000000000C1C1C100B8B8B800D4D4D400D4D4
      D400D4D4D400D4D4D400A6A6A6000000000000000000BF713400F0BB9500F0BB
      9500F0BB9500F0BB9500D18C5800AA8E79000000000000000000000000000000
      0000000000000000000000000000B37F5500C77E4500E9B08700F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F2C09C00F1BD9800F0BB9500F0BB
      9500F0BB9500EFB99300BC6E310000000000000000007D7D7D00D0D0D000ACAC
      AC00ACACAC0074747400F6F6F600F6F6F600F7F7F700F9F9F900FBFBFB00FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6F600F3F3
      F300F1F1F100EEEEEE00ECECEC00EBEBEB00F4F4F40070707000ABABAB00ACAC
      AC00B5B5B5007F7F7F00000000000000000000000000BF703100F1B99200F1BB
      9400F1BB9400F2BE9800F6C7A600F1C19E00F5C5A300F2BC9500F1BB9400F1BB
      9400F1BB9400F1BB9400F1BB9400F1BA9300DD9D6C00C3763900B07E56000000
      0000000000000000000000000000B0988500C3763A00EBB28900F1BB9400F1BB
      9400F1BB9400F1BA9300BF7032000000000000000000A5A5A500D2D2D200D4D4
      D400D4D4D400D5D5D500DBDBDB00D8D8D800DADADA00D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D3D3D300C1C1C100A9A9A900B0B0B0000000
      0000000000000000000000000000C1C1C100A9A9A900CECECE00D4D4D400D4D4
      D400D4D4D400D3D3D300A6A6A6000000000000000000BF713200EFBA9300F0BB
      9500F0BB9500F0BB9500EAB28900C1763A00A9907D0000000000000000000000
      000000000000B4825A00C3763900DC9D6D00EFBA9400F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BC9600F4C5A300F0C19F00F5C7A600F1BE9900F0BB
      9500F0BB9500EFB99300BF70310000000000000000007D7D7D00D1D1D100AEAE
      AE00AEAEAE0073737300F6F6F600F4F4F400F5F5F500F7F7F700F9F9F900FBFB
      FB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6
      F600F3F3F300F1F1F100EEEEEE00EDEDED00F4F4F40070707000ADADAD00AEAE
      AE00B7B7B7007F7F7F00000000000000000000000000C1723200EFBA9300F0BB
      9500F1BE9900F5C6A500D8966300C3743400E6AE8400F5C6A500F1BD9800F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500EFB99200E0A17200D089
      5100C6793A00C2723100C5773700D18B5400EAB18800F0BB9500F0BB9500F0BB
      9500F0BB9600E9AF8400C1804B000000000000000000A6A6A600D3D3D300D4D4
      D400D5D5D500DBDBDB00BDBDBD00A8A8A800CBCBCB00DBDBDB00D5D5D500D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D2D2D200C4C4C400B5B5
      B500AAAAAA00A6A6A600A9A9A900B6B6B600CECECE00D4D4D400D4D4D400D4D4
      D400D4D4D400CCCCCC00AFAFAF000000000000000000C9864F00E8AF8400F0BB
      9600F0BB9500F0BB9500F0BB9500EAB18800D18B5400C5773700C2723100C678
      3A00D0895100DFA17200EEB99200F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F1BD9800F4C6A600E6AE8400C3743400D8966300F4C6A500F1BE
      9900F0BB9500EFBA9300C072320000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F3F3F300F1F1F100BEBEBE00B4B4B400B4B4
      B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4
      B400B4B4B400BEBEBE00F1F1F100EFEFEF00F5F5F50070707000AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000C3743200EFBA9300F1BE
      9900F4C6A400D9966200CB905F0000000000CA834700DEA17000F5C9A800F2C1
      9D00F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F1BD9800DB996500BC997C000000000000000000A7A7A700D3D3D300D5D5
      D500DBDBDB00BDBDBD00B9B9B90000000000B0B0B000C3C3C300DCDCDC00D7D7
      D700D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D5D5D500BEBEBE00C1C1C1000000000000000000DEB18C00DB996500F1BD
      9800F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F2C19D00F5C9A800DEA17000CA83470000000000D1936100D9966200F4C6
      A400F1BE9900EFBA9300C374320000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200F2F2F200F3F3F300F5F5F500F7F7
      F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFA
      FA00F9F9F900F6F6F600F3F3F300F1F1F100F6F6F60071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000C6763200F0BB9600F4C6
      A500DA976100D4986600000000000000000000000000D69B6900D38B5100F1C2
      9F00F5C7A600F1BD9800F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F1BE
      9900EFBC9600CA7B3A00000000000000000000000000A8A8A800D4D4D400DBDB
      DB00BDBDBD00BEBEBE00000000000000000000000000BFBFBF00B5B5B500D8D8
      D800DBDBDB00D5D5D500D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5
      D500D4D4D400ACACAC0000000000000000000000000000000000CA7B3A00EFBC
      9600F1BE9900F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F1BD9800F5C7
      A600F1C29F00D38B5100D2976600000000000000000000000000D5996600DA97
      6100F4C6A500F0BB9600C576320000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200F2F2F200F2F2F200F3F3F300F5F5
      F500F7F7F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FAFAFA00F9F9F900F6F6F600F3F3F300F8F8F80071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000C8773300F2C19D00DB97
      6100D69A6700000000000000000000000000000000000000000000000000CB7E
      3D00E2A67600F6CBAC00F4C6A500F0BD9800F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F1BE9A00F4C6
      A400D8935A00DDAB8200000000000000000000000000A9A9A900D7D7D700BDBD
      BD00BFBFBF00000000000000000000000000000000000000000000000000ADAD
      AD00C6C6C600DDDDDD00DBDBDB00D5D5D500D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D6D6D600DBDB
      DB00BABABA00CACACA0000000000000000000000000000000000DEAD8400D893
      5A00F4C6A400F1BE9A00F0BB9500F0BB9500F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F0BD9800F4C6A500F6CBAB00E2A6
      7600CB7D3C00000000000000000000000000000000000000000000000000D69A
      6800DB976100F2C19D00C777330000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200EFEFEF00BEBEBE00B4B4B400B4B4
      B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4
      B400B4B4B400BEBEBE00F9F9F900F6F6F600F9F9F90071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000CC7C3800DB975F00D89B
      6600000000000000000000000000000000000000000000000000000000000000
      0000DAA17100D0824000E6AB7D00F6CCAD00F6C9A900F3C2A000F0BC9700F0BB
      9500F0BB9500F0BB9500F0BB9500F0BB9500F1BE9900F4C6A500F5C8A700DA96
      5D00D89B670000000000000000000000000000000000ACACAC00BDBDBD00BFBF
      BF00000000000000000000000000000000000000000000000000000000000000
      0000C3C3C300B0B0B000C9C9C900DEDEDE00DCDCDC00D8D8D800D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D5D5D500DBDBDB00DCDCDC00BCBC
      BC00BFBFBF00000000000000000000000000000000000000000000000000D89B
      6600DA965D00F5C8A700F4C6A500F1BE9900F0BB9500F0BB9500F0BB9500F0BB
      9500F0BB9500F0BC9700F3C2A000F6C9A900F6CCAD00E6AB7D00D0824000D9A1
      7100000000000000000000000000000000000000000000000000000000000000
      0000D89C6800DB975F00CC7C380000000000000000007D7D7D00CFCFCF00A2A2
      A200AFAFAF0074747400F6F6F600F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F3F3F300F5F5F500F7F7F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFD
      FD00FDFDFD00FCFCFC00FAFAFA00F9F9F900FAFAFA0072727200AEAEAE00AFAF
      AF00B8B8B8007F7F7F0000000000000000000000000000000000DEAA7D000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000DBA27000CF803C00DF9D6700EDB89000F7CDAF00F8CF
      B200F7CCAD00F7CCAD00F7CCAD00F8CFB100F5CAA900E6AB7C00D1834000E0AE
      8300000000000000000000000000000000000000000000000000C9C9C9000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C4C4C400ADADAD00C0C0C000D2D2D200DFDFDF00E0E0
      E000DEDEDE00DEDEDE00DEDEDE00E0E0E000DDDDDD00C9C9C900B0B0B000CBCB
      CB00000000000000000000000000000000000000000000000000000000000000
      0000E0AE8300D1834000E6AB7C00F5CAA900F8CFB100F7CCAD00F7CCAD00F7CC
      AD00F8CFB200F7CDAF00EDB89000DF9D6700CF803C00DBA17000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DEAA7D000000000000000000000000007D7D7D00CECECE009494
      9400AFAFAF0075757500F6F6F600F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F3F3F300F4F4F400F6F6F600F8F8F800F9F9F900FBFBFB00FCFCFC00FDFD
      FD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00FBFBFB0072727200AEAEAE00AFAF
      AF00B8B8B8008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DB9E6A00CF7F3900D487
      4400D9925500DB965B00DA925600D4874500D0823D00DFAB7D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C1C1C100ADADAD00B2B2
      B200B9B9B900BCBCBC00B9B9B900B3B3B300AFAFAF00C9C9C900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E0AB7C00D0823D00D4874500DA925600DB965B00D992
      5500D4874400CF7F3900DB9F6A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007D7D7D00DFDFDF00D1D1
      D100D1D1D10076767600D9D9D900D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800DADADA0072727200D0D0D000D1D1
      D100B9B9B9008080800000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000092929200858585008484
      84008484840075757500D0D0D000D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D0D0D00071717100858585008585
      850085858500A0A0A00000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      00005959A2005B5BA60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C4C4C400C0C0C000BABABA00B8B8B800BABABA00C0C0C000C4C4
      C400000000000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000002525
      96005D5DDC005B5BDA0032329D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C4C4
      C400C2C2C200DEDEDE00EDEDED00F3F3F300F3F3F300EFEFEF00E6E6E600D8D8
      D800C2C2C200BFBFBF0000000000000000000000000000000000000000000000
      0000433A36002C201C001F131200190E0D00190E0D00190E0D00190E0D00190E
      0D00190E0D00190E0D00190E0E00190E0E00190E0E00190E0E00190E0E00190E
      0E00180E0E00180E0E00180E0E00180E0E00180E0E001E151400292421002924
      2100282320005855530000000000000000000000000000000000000000000000
      00008C8C8C007D7D7D0076767600737373007373730073737300737373007373
      7300737373007373730073737300737373007373730073737300737373007373
      73007373730073737300737373007373730073737300777777007F7F7F007F7F
      7F007E7E7E009B9B9B0000000000000000000000000000000000000000000000
      0000433A36002C201C001F131200190E0D00190E0D00190E0D00190E0D00190E
      0D00190E0D00190E0D00190E0E00190E0E00190E0E00190E0E00190E0E00190E
      0E00180E0E00180E0E00180E0E00180E0E003228280061656A00494872005F60
      D4006265E4006466E600595ADB006162B9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D2D2D200CFCF
      CF00F4F4F400F6F6F600F3F3F300EFEFEF00EAEAEA00E5E5E500E1E1E100E2E2
      E200E9E9E900CBCBCB00C5C5C500000000000000000000000000000000003D33
      30003F353200524A4800180D0D00B9B7B600CEC6C400CEC6C400CEC6C400CEC6
      C400CEC6C400CEC6C400CEC6C400CEC6C400CEC6C400CEC6C400D1CBCA008A89
      890089898900888888008786860086858500828282001A0F0F00534B4900534B
      4900524A48002B26230000000000000000000000000000000000000000008787
      8700898989009696960073737300D4D4D400DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E0E0E000BABA
      BA00BABABA00B9B9B900B8B8B800B8B8B800B6B6B60074747400969696009696
      9600969696008080800000000000000000000000000000000000000000003D33
      30003F353200524A4800180D0D00B9B7B600CEC6C400CEC6C400CEC6C400CEC6
      C400CEC6C400CEC6C400CEC6C400CEC6C400CEC6C400CEC6C400D1CBCA008A89
      89008989890088888800878686009292920091999E00676B6E00A8A6A6008A8C
      9B008193E5007B8FEA005266D5006E6FC3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EDEDED00BABABA00EEEE
      EE00EFEFEF00F5F5F500F4F4F400F1F1F100EDEDED00E8E8E800E3E3E300DCDC
      DC00DCDCDC00E6E6E600BDBDBD000000000000000000000000003E322F003F34
      3200433B390039302E00180D0D00CDC9C700DAC6BF00CEBCB600797776007876
      76007D7A7900DAC6BF00DAC6BF00DAC6BF00DAC6BF00DAC6BF00E6DAD6009696
      96009595950094949400919191008E8E8E0088888800190E0E0039302E003930
      2E00524A48002B26230000000000000000000000000000000000878787008989
      89008D8D8D008686860073737300DFDFDF00DDDDDD00D8D8D800AFAFAF00AFAF
      AF00B1B1B100DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E9E9E900C2C2
      C200C1C1C100C0C0C000BFBFBF00BDBDBD00B9B9B90073737300868686008686
      86009696960080808000000000000000000000000000000000003E322F003F34
      3200433B390039302E00180D0D00CDC9C700DAC6BF00CEBCB600797776007876
      76007D7A7900DAC6BF00DAC6BF00DAC6BF00DAC6BF00DAC6BF00E6DAD6009696
      960095959500949494009D9D9D008E9DA5003C677C0093919100D9D8D900C5C4
      C4008E909D006B7CD7002E31AD00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B3B3B300A1A1A100A0A0
      A0009E9E9E009C9C9C009E9E9E009F9F9F009F9F9F00CCCCCC00BABABA00F2F2
      F200F1F1F100EBEBEB00E4E4E400E2E2E200DCDCDC00D6D6D600D1D1D100D2D2
      D200E0E0E000ECECEC00BABABA000000000000000000453835003F353200433B
      390039302E0039302E00180D0C00CDC9C800DCC9C200D0BFBA00747474006F6F
      6F0072727100DCC9C200DCC9C200DCC9C200DCC9C200DCC9C200E7DCD8009B9B
      9B009A9A9A0098989800969696009191910089898900190E0D0039302E003930
      2E00524A48002B2623000000000000000000000000008B8B8B00898989008D8D
      8D00868686008686860072727200DFDFDF00DFDFDF00D9D9D900AEAEAE00ABAB
      AB00ACACAC00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00EAEAEA00C5C5
      C500C4C4C400C3C3C300C2C2C200BFBFBF00BABABA0073737300868686008686
      86009696960080808000000000000000000000000000453835003F353200433B
      390039302E0039302E00180D0C00CDC9C800DCC9C200D0BFBA00747474006F6F
      6F0072727100DCC9C200DCC9C200DCC9C200DCC9C200DCC9C200E7DCD8009B9B
      9B009A9A9A00A4A4A40092A0A800286B8A0065D2F1006CB1C700C2C1C100DAD9
      DA00A4A2A2004F4D8F0000000000000000000000000000000000000000000000
      0000000000000000000000000000B4B4B4009F9F9F009C9C9C00A8A8A800B3B3
      B300B6B6B600B7B7B700B7B7B700B3B3B300AEAEAE00CACACA00BFBFBF00E7E7
      E700E1E1E100EEEEEE00F0F0F000EBEBEB00E6E6E600DFDFDF00D8D8D800CFCF
      CF00CCCCCC00E2E2E200BABABA0000000000000000002E1F1C007F7978003930
      2E0039302E0039302E00180D0C00CDC9C800E1D1CB00D6C8C3007E7E7E007777
      770076757500E1D1CB00E1D1CB00E1D1CB00E1D1CB00E1D1CB00EAE1DE009898
      98009898980096969600939393008E8E8E0085858500190E0D0039302E003930
      2E00524A48002B2623000000000000000000000000007D7D7D00B1B1B1008686
      8600868686008686860072727200DFDFDF00E4E4E400DFDFDF00B4B4B400B0B0
      B000AEAEAE00E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400EDEDED00C3C3
      C300C3C3C300C2C2C200C0C0C000BDBDBD00B8B8B80073737300868686008686
      860096969600808080000000000000000000000000002E1F1C007F7978003930
      2E0039302E0039302E00180D0C00CDC9C800E1D1CB00D6C8C3007E7E7E007777
      770076757500E1D1CB00E1D1CB00E1D1CB00E1D1CB00E1D1CB00EAE1DE009898
      9800A3A3A30093A1A900286C8B0065D2F10052D4FD004DD1FE0083B5C6009895
      95007B7D80006260600000000000000000000000000000000000000000000000
      00000000000000000000A8A8A8009B9B9B00BCBCBC00BABABA00ACACAC00B0B0
      B000B4B4B400B5B5B500B5B5B500B6B6B600B7B7B700D4D4D400B9B9B900E7E7
      E700EBEBEB00F2F2F200F0F0F000EBEBEB00E6E6E600DFDFDF00D8D8D800D2D2
      D200D4D4D400DFDFDF00B8B8B80000000000000000002E1F1C008E8987003930
      2E0039302E0039302E00170D0C00CAC7C600E7DBD600DDD2CE00898989008282
      82007F7F7F00E7DBD600E7DBD600E7DBD600E7DBD600E7DBD600EDE6E4008B8B
      8B008A8A8A0089898900868686008181810079797900180E0D0039302E003930
      2E00524A48002B2623000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860072727200DEDEDE00E9E9E900E4E4E400BABABA00B6B6
      B600B4B4B400E9E9E900E9E9E900E9E9E900E9E9E900E9E9E900F0F0F000BBBB
      BB00BBBBBB00BABABA00B8B8B800B5B5B500B1B1B10073737300868686008686
      860096969600808080000000000000000000000000002E1F1C008E8987003930
      2E0039302E0039302E00170D0C00CAC7C600E7DBD600DDD2CE00898989008282
      82007F7F7F00E7DBD600E7DBD600E7DBD600E7DBD600E7DBD600EDE6E4009898
      98008D9DA500286D8D0065D2F20052D5FD004ED3FE007DDFFF0055B0D3004E6A
      79007E7A7A00322D2A0000000000000000000000000000000000000000000000
      0000CBCBCB00A8A8A800B3B3B3009F9F9F00CCCCCC00CCCCCC00BEBEBE00A8A8
      A800AFAFAF00B4B4B400B3B3B300B3B3B300B3B3B300D1D1D100B9B9B900ECEC
      EC00EEEEEE00F7F7F700F5F5F500F3F3F300F0F0F000ECECEC00E7E7E700E2E2
      E200D9D9D900E3E3E300B9B9B90000000000000000002E1F1C008E8987003930
      2E0039302E00382F2D00170D0C00C6C4C300EDE4E000E3DAD700929292008D8D
      8D008A8A8A00EDE4E000EDE4E000EDE4E000EDE4E000EDE4E000F0EBE9007B7B
      7B007A7A7A007979790076767600737373006B6B6B00180E0D0039302E003930
      2E00524A48002B2622000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860072727200DCDCDC00EEEEEE00E9E9E900BFBFBF00BCBC
      BC00BBBBBB00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00F2F2F200B2B2
      B200B1B1B100B1B1B100AFAFAF00ADADAD00A9A9A90073737300868686008686
      860096969600808080000000000000000000000000002E1F1C008E8987003930
      2E0039302E00382F2D00170D0C00C6C4C300EDE4E000E3DAD700929292008D8D
      8D008A8A8A00EDE4E000EDE4E000EDE4E000EDE4E000EDE4E000F1ECEB008696
      9E00276F8F0065D2F20053D7FD004FD5FE007EE0FF0056B1D300335F77006C68
      680058504F002B2622000000000000000000000000000000000000000000CBCB
      CB00A9A9A900B7B7B700B7B7B7009E9E9E00BEBEBE00CECECE00D4D4D400C9C9
      C900ACACAC00B5B5B500B8B8B800B5B5B500B4B4B400D1D1D100BEBEBE00F5F5
      F500EFEFEF00E8E8E800E4E4E400E0E0E000DCDCDC00D5D5D500D0D0D000CDCD
      CD00DEDEDE00F1F1F100BBBBBB0000000000000000002F1F1C008E8987003930
      2E0039302E00382F2D00180E0D00C4C2C200F0E9E600E6DFDD00959595009494
      940094949400F0E9E600F0E9E600F0E9E600F0E9E600F0E9E600F1EDEC006F6F
      6F006F6F6F006D6D6D006B6B6B006868680062626200180E0D0039302E003930
      2E00524A48002B2522000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860073737300DBDBDB00F1F1F100ECECEC00C1C1C100C0C0
      C000C0C0C000F1F1F100F1F1F100F1F1F100F1F1F100F1F1F100F4F4F400ABAB
      AB00ABABAB00AAAAAA00A9A9A900A7A7A700A3A3A30073737300868686008686
      8600969696007F7F7F000000000000000000000000002F1F1C008E8987003930
      2E0039302E00382F2D00180E0D00C4C2C200F0E9E600E6DFDD00959595009494
      940094949400F0E9E600F0E9E600F0E9E600F0E9E600F2EBE900BECCD4002771
      910065D3F2005ADCFD0050D7FE007FE1FF0056B3D3002F5B73006C6867004037
      3500524A48002B2522000000000000000000000000000000000000000000A8A8
      A800B7B7B700B6B6B600B5B5B500B5B5B500B5B5B500D9D9D900D8D8D800E0E0
      E000D8D8D800AEAEAE00C3C3C300C2C2C200BDBDBD00D4D4D400C0C0C000E1E1
      E100E2E2E200EEEEEE00F0F0F000EBEBEB00E6E6E600DFDFDF00D8D8D800D0D0
      D000CCCCCC00DBDBDB00BCBCBC0000000000000000002F1F1C008E8987003930
      2E0039302E00382F2D00180E0D00C4C2C200F1EAE700E6E0DD00969696009696
      960099999900F1EAE700F1EAE700F1EAE700F1EAE700F1EAE700F1EDEC006B6B
      6B006B6B6B006A6A6A00686868006565650061616100180E0D0039302E003930
      2E00524A48002B2522000000000000000000000000007D7D7D00B9B9B9008686
      8600868686008686860073737300DBDBDB00F2F2F200ECECEC00C2C2C200C2C2
      C200C3C3C300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F4F4F400A9A9
      A900A9A9A900A8A8A800A7A7A700A5A5A500A3A3A30073737300868686008686
      8600969696007F7F7F000000000000000000000000002F1F1C008E8987003930
      2E0039302E00382F2D00180E0D00C4C2C200F1EAE700E6E0DD00969696009696
      960099999900F1EAE700F1EAE700F1EAE700F2ECE900BDCCD2002A75960066D3
      F20060E0FD0065E5FE0086E6FF0057B5D4003867800055505100403735003930
      2E00524A48002B25220000000000000000000000000000000000A9A9A900B5B5
      B500BBBBBB00BCBCBC00C2C2C200B4B4B400C5C5C500E0E0E000D7D7D700D7D7
      D700DFDFDF00DCDCDC00ADADAD00C6C6C600C6C6C600DBDBDB00B8B8B800EBEB
      EB00EBEBEB00F2F2F200F0F0F000EBEBEB00E6E6E600DFDFDF00D8D8D800D2D2
      D200D4D4D400E3E3E300B8B8B80000000000000000002F1F1B008E8988003A30
      2E003A302E0039302E00180E0D00A8A7A700CBC9C800CBC9C800CBC9C800CCC9
      C900CCC9C900CCC9C900CCC9C900CCC9C900CCC9C900CCC9C900CAC8C8006767
      67006666660065656500646464006464640061616100180E0D003A302E003A31
      2F00524B49002B2522000000000000000000000000007D7D7D00BABABA008686
      8600868686008686860073737300CBCBCB00DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00A6A6
      A600A6A6A600A5A5A500A5A5A500A5A5A500A3A3A30073737300868686008787
      8700969696007F7F7F000000000000000000000000002F1F1B008E8988003A30
      2E003A302E0039302E00180E0D00A8A7A700CBC9C800CBC9C800CBC9C800CCC9
      C900CCC9C900CCC9C900CCC9C900D5D3D200B5C5CD002A76970066D3F20061E1
      FD0067E7FE008DEBFF0061BDD500396B8300888A8B00201615003A302E003A31
      2F00524B49002B252200000000000000000000000000C3C3C300ADADAD00C0C0
      C000C0C0C000C3C3C300BABABA00BCBCBC00E3E3E300DADADA00D7D7D700D9D9
      D900DEDEDE00E8E8E800B7B7B700C0C0C000C6C6C600DCDCDC00BDBDBD00EDED
      ED00EBEBEB00EDEDED00E7E7E700E3E3E300DEDEDE00D9D9D900D4D4D400D0D0
      D000D4D4D400E5E5E500BBBBBB0000000000000000002F1E1B00918B8A003E35
      33003E3533003E35330022181700170D0C00170D0C00170D0C00170D0C00170D
      0C00170D0C00170D0C00170D0C00170D0C00170D0C00170D0C00170D0C00170D
      0C00170D0C00170D0C00170D0C00170D0C00170D0C00241A19003E3533003E35
      3300564E4C002B2522000000000000000000000000007D7D7D00BBBBBB008989
      8900898989008989890079797900727272007272720072727200727272007272
      7200727272007272720072727200727272007272720072727200727272007272
      720072727200727272007272720072727200727272007A7A7A00898989008989
      8900989898007F7F7F000000000000000000000000002F1E1B00918B8A003E35
      33003E3533003E35330022181700170D0C00170D0C00170D0C00170D0C00170D
      0C00180E0D002B2221005049480068737A002974940066D3F20061E2FD0067E8
      FE008EECFF0062BED6002E617A00544F50001F151400241A19003E3533003E35
      3300564D4C002B252200000000000000000000000000A9A9A900BFBFBF00C5C5
      C500C3C3C300C5C5C500ADADAD00D6D6D600DCDCDC00D7D7D700D8D8D800DDDD
      DD00DFDFDF00D6D6D600BCBCBC00C0C0C000C7C7C700DCDCDC00BCBCBC00E5E5
      E500EBEBEB00F4F4F400F9F9F900FBFBFB00FBFBFB00FAFAFA00F8F8F800EFEF
      EF00E5E5E500E0E0E000BCBCBC0000000000000000002F1E1B00948F8D00433A
      3800433A3800433A3800433A3800433A3800433A3800433A3800433A3800433A
      3800433A3800433A3800433A3800433A3800433A3800433A3800433A3800433A
      3800433A3800433A3800433A3800433A3800433A3800433A3800433A3800443B
      39005A5351002B2522000000000000000000000000007D7D7D00BDBDBD008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008D8D
      8D009B9B9B007F7F7F000000000000000000000000002F1E1B00948F8D00433A
      3800433A3800433A3800433A3800433A3800433A3800433A3800433A3800433A
      3800534A4800857F7D00A8AFB30070A2B8007DDAF40062E2FD0068E9FE008EED
      FF0063C0D70035698200736F6F004A413F00433A3800433A3800423A3800423A
      380058514F002B262300000000000000000000000000ACACAC00C8C8C800C5C5
      C500C6C6C600C8C8C800B3B3B300CDCDCD00DCDCDC00DCDCDC00E1E1E100D2D2
      D200C0C0C000BDBDBD00C1C1C100C9C9C900C9C9C900DCDCDC00BCBCBC00F9F9
      F900FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00F8F8F800BDBDBD0000000000000000002F1E1B00979391004941
      3E0049413E0049413E0049413E0049413E0049413E0049413E0049413E004941
      3E0049413E0049413E0049413E0049413E0049413E0049413E0049413E004941
      3E0049413E0049413E0049413E0049413E0049413E0049413E0049413E004A41
      3F00605956002B2522000000000000000000000000007D7D7D00BFBFBF009090
      9000909090009090900090909000909090009090900090909000909090009090
      9000909090009090900090909000909090009090900090909000909090009090
      9000909090009090900090909000909090009090900090909000909090009090
      90009E9E9E007F7F7F000000000000000000000000002F1E1B00979391004941
      3E0049413E0049413E0049413E0049413E0049413E0049413E0049413E004C44
      41006E676500AEAAA900688798009FD9EA0091EBFD0079ECFE008FEEFF0063C0
      D700356A8300777473004F47440048403D00473F3D00473F3D00463F3C00473F
      3D005A5452002C2624000000000000000000BDBDBD00B6B6B600CACACA00C7C7
      C700CACACA00CBCBCB00B9B9B900C8C8C800EBEBEB00EBEBEB00D9D9D900C1C1
      C100C7C7C700CDCDCD00CDCDCD00CDCDCD00CDCDCD00DFDFDF00C3C3C300C9C9
      C900E3E3E300EFEFEF00F9F9F900FAFAFA00FBFBFB00FAFAFA00F8F8F800EEEE
      EE00E2E2E200C8C8C800D4D4D40000000000000000002F1E1B009B9695005048
      4600504846001C131200140B0B00130B0A00130A0A00120A0900110A09001109
      0900100908000F0908000F0808000E0808000E0807000D0707000D0707000C07
      07000C0606000C0606000B0606000B0606000B060600100B0A00504745005148
      4600665E5C002B2522000000000000000000000000007D7D7D00C2C2C2009494
      9400949494007676760072727200717171007171710070707000707070007070
      700070707000707070006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F
      6F006E6E6E006E6E6E006E6E6E006E6E6E006E6E6E0071717100949494009494
      9400A1A1A1007F7F7F000000000000000000000000002F1E1B009B9695005048
      4600504846001C131200140B0B00130B0A00130A0A00120A0900110A0900241D
      1C005C57570085888C00769AAE006B98A900C0F1FA00B9F4FF007CCBDE003165
      7E004E4D4E0017121200100B0B00110D0D00120E0E00161212004A4341004C45
      43005F5857002C2623000000000000000000ACACAC00BFBFBF00CACACA00CACA
      CA00C7C7C700B9B9B900C6C6C600BDBDBD00C2C2C200C1C1C100C5C5C500CECE
      CE00D3D3D300D3D3D300D2D2D200D1D1D100D0D0D000C5C5C500E0E0E000D8D8
      D800C9C9C900C4C4C400BEBEBE00BCBCBC00BBBBBB00BCBCBC00B9B9B900C8C8
      C800D6D6D600000000000000000000000000000000002F1E1B009F9B99005850
      4D0058504D00170E0D00F5F5F500FCFDFD00FDFDFE00FCFDFD00FCFCFD00FBFB
      FB00FAFAFA00F8F8F800F6F6F500F5F4F300F3F2F100F2F0EF00F1EFEE00F0EE
      EC00F0EEEC00F0EEEC00F0EEEC00F0EEEC00F1EFEE000E090900564E4C005850
      4E006C6563002B2522000000000000000000000000007D7D7D00C4C4C4009999
      99009999990073737300F9F9F900FDFDFD00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FCFCFC00FAFAFA00F9F9F900F8F8F800F6F6F600F5F5F500F5F5F500F4F4
      F400F4F4F400F4F4F400F4F4F400F4F4F400F5F5F50070707000989898009999
      9900A5A5A5007F7F7F000000000000000000000000002F1E1B009F9B99005850
      4D0058504D00170E0D00F5F5F500FCFDFD00FDFDFE00FCFDFD00FCFCFD00FBFC
      FC00FCFCFC008DA4B20098BDD1008EB3C6007798A800A8CEDA0087AABB00D6D7
      D800C6C5C400BFBDBD00BDBCBB00BFBDBD00C5C3C2001511110050494700534C
      4A0067605E002B2522000000000000000000A4A4A400C4C4C400CBCBCB00BFBF
      BF00BFBFBF00D6D6D600BEBEBE00CACACA00D5D5D500D5D5D500D9D9D900DBDB
      DB00DBDBDB00D9D9D900D7D7D700D6D6D600D5D5D500C3C3C300CECECE00F1F1
      F100EBEBEB00EAEAEA00EBEBEB00ECECEC00EBEBEB00E3E3E300CDCDCD000000
      000000000000000000000000000000000000000000002F1E1B00A39F9E005F58
      55005F585500180E0E00F4F4F400FBFBFC00FCFDFE00FDFEFE00FCFDFE00FBFC
      FC00F8F9F900F6F6F600F2F1F100EFEDEC00EBE9E700E8E5E300E5E1DF00E3DE
      DC00E1DCDA00E1DCD900E1DCD900E2DDDB00F0EDEC000E0909005E5653005F58
      5500726C69002C2522000000000000000000000000007D7D7D00C7C7C7009D9D
      9D009D9D9D0073737300F8F8F800FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FAFAFA00F9F9F900F6F6F600F3F3F300F1F1F100EFEFEF00EDEDED00EBEB
      EB00EAEAEA00EAEAEA00EAEAEA00EAEAEA00F4F4F400707070009C9C9C009D9D
      9D00A9A9A9007F7F7F000000000000000000000000002F1E1B00A39F9E005F58
      55005F585500180E0E00F4F4F400FBFBFC00FCFDFE00FDFEFE00FCFDFE00FCFD
      FD00E2E7EA007AA5BB009DCBE1008FB5C8007191A30094AAB500D4D4D400B3B0
      AF00A7A4A200ABA7A500B1AEAC00BAB6B400CECCCB00120E0E005A5350005D56
      5300706A67002C2522000000000000000000A4A4A400C2C2C200B7B7B700C4C4
      C400DFDFDF00BDBDBD00C3C3C300C0C0C000D4D4D400E3E3E300E3E3E300E4E4
      E400E3E3E300E2E2E200E0E0E000DEDEDE00DCDCDC00D7D7D700C0C0C000D2D2
      D200EAEAEA00ECECEC00E6E6E600C6C6C600AAAAAA00B3B3B300A7A7A7000000
      000000000000000000000000000000000000000000002F1F1C00A8A3A200665F
      5C00665F5C00190F0E00F3F3F300F9F9FA00FBFBFC00FCFDFE00FDFEFE00FCFD
      FE00FAFCFC00F8F9F900F5F5F500F2F1F000EEECEB00EAE8E600E7E4E200E4E0
      DD00E1DCDA00E0DBD800E0DBD800E1DCD900F0EDEC000E090900655D5B00675F
      5D00787270002C2521000000000000000000000000007D7D7D00C9C9C900A1A1
      A100A1A1A10073737300F8F8F800FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFD
      FD00FCFCFC00FAFAFA00F9F9F900F6F6F600F3F3F300F1F1F100EEEEEE00ECEC
      EC00EAEAEA00E9E9E900E9E9E900EAEAEA00F4F4F40070707000A1A1A100A2A2
      A200ADADAD007F7F7F000000000000000000000000002F1F1C00A8A3A200665F
      5C00665F5C00190F0E00F3F3F300F9F9FA00FBFBFC00FCFDFE00FDFEFE00FDFE
      FE00849BA50082B1C700739AAE0092A2AB00D1D2D200CFCECE00BCBAB900ACAA
      A800B0ACAA00B8B4B200C3BFBC00CECAC700E3E1DF000F0A0A00645C5A00665F
      5C0078716F002C2521000000000000000000A5A5A500B7B7B700C2C2C200D9D9
      D900E2E2E200C7C7C700BEBEBE00CCCCCC00CBCBCB00E9E9E900EBEBEB00EBEB
      EB00EBEBEB00E9E9E900E7E7E700E4E4E400E2E2E200E0E0E000D5D5D500BFBF
      BF00D1D1D100D4D4D400CECECE00AEAEAE00ADADAD00AAAAAA00A8A8A8000000
      00000000000000000000000000000000000000000000301F1B00ACA8A6006D66
      63006D6663001A0F0E00F2F2F100F7F6F600F9F9F900FBFBFC00FCFDFE00FDFE
      FE00FCFDFE00FAFCFC00F8F9F900F5F5F500F2F1F000EEECEB00EAE8E600E7E4
      E200E4E0DD00E1DCDA00E0DBD800E1DCD900F0EDEC000F0909006C6562006D67
      64007E7876002C2421000000000000000000000000007D7D7D00CBCBCB00A5A5
      A500A5A5A50073737300F6F6F600F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFD
      FD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6F600F3F3F300F1F1F100EEEE
      EE00ECECEC00EAEAEA00E9E9E900EAEAEA00F4F4F40070707000A5A5A500A6A6
      A600B0B0B0007F7F7F00000000000000000000000000301F1B00ACA8A6006D66
      63006D6663001A0F0E00F2F2F100F7F6F600F9F9F900FBFBFC00FDFDFE00F1F3
      F500395D6D00889BA300CFD1D200D1D1D100C9C9C800C0C0BF00BFBDBB00C5C3
      C100CFCBC900D5D0CE00D9D4D200DED9D600EEECEA000F0909006C6562006D67
      64007E7876002C2421000000000000000000B0B0B000B9B9B900DDDDDD00E6E6
      E600E8E8E800E9E9E900E8E8E800E3E3E300C9C9C900E0E0E000EEEEEE00F1F1
      F100F1F1F100EFEFEF00EDEDED00E9E9E900E6E6E600E4E4E400D9D9D900C8C8
      C800D4D4D400BCBCBC00B9B9B900BEBEBE00BFBFBF00B2B2B200ADADAD000000
      00000000000000000000000000000000000000000000301E1B00AFABAA00736D
      6A00736D6A001A100F00F1F0F000F4F3F300F1F0F000C9C9C900C9CACA00CACA
      CA00CACACB00CACACB00C9CACA00C8C9C900C7C7C700C6C6C600C5C4C400C4C2
      C200C2C1C000C3C1C000E1DCDA00E1DCDA00F0EDEC000F0A0900716B6800736D
      6A00837D7B002C2421000000000000000000000000007D7D7D00CECECE00A9A9
      A900A9A9A90074747400F6F6F600F8F8F800F6F6F600DFDFDF00DFDFDF00E0E0
      E000E0E0E000E0E0E000DFDFDF00DFDFDF00DEDEDE00DDDDDD00DCDCDC00DBDB
      DB00DADADA00DADADA00EAEAEA00EAEAEA00F4F4F40070707000A8A8A800A9A9
      A900B3B3B3007F7F7F00000000000000000000000000301E1B00AFABAA00736D
      6A00736D6A001A100F00F1F0F000F4F3F300F1F0F0002DCC410000CD190032D1
      4500D3D4D500D2D2D300CBCBCB00C3C3C300BCBCBC00B9B9B800BCBBBB00BFBE
      BE00C0BFBE00C2C0BF00E1DCDA00E1DCDA00F0EDEC000F0A0900716B6800736D
      6A00837D7B002C2421000000000000000000C4C4C400BBBBBB00E8E8E800E9E9
      E900E7E7E700E7E7E700E7E7E700E8E8E800DDDDDD00D1D1D100D8D8D800EBEB
      EB00F5F5F500F4F4F400F1F1F100EEEEEE00EAEAEA00E6E6E600E2E2E200CECE
      CE00D0D0D000E7E7E700DCDCDC00E6E6E600D9D9D900B6B6B600BCBCBC000000
      00000000000000000000000000000000000000000000301E1B00B2AEAD007872
      6F0078726F001A100E00F2F1F000F2F1EF00F4F3F200F6F6F600F9F9F900FBFB
      FC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F5F5F500F2F1F000EEEC
      EB00EAE8E600E7E4E200E4E0DD00E3DEDC00F0EDEC00100A0A0076706D007872
      6F0087827F002C2421000000000000000000000000007D7D7D00D0D0D000ACAC
      AC00ACACAC0074747400F6F6F600F6F6F600F7F7F700F9F9F900FBFBFB00FCFC
      FC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6F600F3F3
      F300F1F1F100EEEEEE00ECECEC00EBEBEB00F4F4F40070707000ABABAB00ACAC
      AC00B5B5B5007F7F7F00000000000000000000000000301E1B00B2AEAD007872
      6F0078726F001A100E00F2F1F000F2F1EF00F4F3F200F6F6F600F9F9FA00FBFC
      FC00FCFDFE00FCFDFE00FCFDFE00FAFBFB00F7F8F800F5F5F500F1F1F000EEEC
      EB00EAE8E600E7E4E200E4E0DD00E3DEDC00F0EDEC00100A0A0076706D007872
      6F0087827F002C242100000000000000000000000000B2B2B200DFDFDF00EAEA
      EA00EAEAEA00EAEAEA00EAEAEA00EBEBEB00EDEDED00E4E4E400CECECE00CCCC
      CC00E1E1E100F5F5F500F5F5F500F1F1F100EDEDED00EAEAEA00E6E6E600E0E0
      E000BEBEBE00D4D4D400E4E4E400EEEEEE00D4D4D400B0B0B000000000000000
      00000000000000000000000000000000000000000000311E1B00B4B1AF007C76
      73007C7673001A0F0E00F3F1F100F0EEEC00F1F0EF00F4F3F200F6F6F600F9F9
      F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F5F5F500F2F1
      F000EEECEB00EAE8E600E7E4E200E5E1DF00F0EEED00100A0A007A7471007C76
      73008A8583002C2421000000000000000000000000007D7D7D00D1D1D100AEAE
      AE00AEAEAE0073737300F6F6F600F4F4F400F5F5F500F7F7F700F9F9F900FBFB
      FB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00F9F9F900F6F6
      F600F3F3F300F1F1F100EEEEEE00EDEDED00F4F4F40070707000ADADAD00AEAE
      AE00B7B7B7007F7F7F00000000000000000000000000311E1B00B4B1AF007C76
      73007C7673001A0F0E00F3F1F100F0EEEC00F1F0EF00F4F3F200F6F6F600F9F9
      F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F5F5F500F2F1
      F000EEECEB00EAE8E600E7E4E200E5E1DF00F0EEED00100A0A007A7471007C76
      73008A8583002C242100000000000000000000000000B4B4B400CDCDCD00ECEC
      EC00ECECEC00EBEBEB00EFEFEF00F2F2F200F0F0F000EEEEEE00E7E7E700D4D4
      D400D6D6D600F6F6F600F7F7F700F3F3F300EFEFEF00EBEBEB00E7E7E700DFDF
      DF00C6C6C600C0C0C000BBBBBB00C9C9C900BABABA00B3B3B300000000000000
      00000000000000000000000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001B0F0F00F2F1F000EEECEA00EAE8E700C6C6C500C7C7C700C8C8
      C800C9C9C900C9CACA00CACACA00CACACB00CACACB00C9CACA00C8C9C900C7C7
      C700C6C6C600C7C6C600EAE8E600E8E5E300F1F0EF00110A0A007B7572007D77
      74008B8683002C2421000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F3F3F300F1F1F100DDDDDD00DEDEDE00DFDF
      DF00DFDFDF00DFDFDF00E0E0E000E0E0E000E0E0E000DFDFDF00DFDFDF00DEDE
      DE00DDDDDD00DDDDDD00F1F1F100EFEFEF00F5F5F50070707000AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001B0F0F00F2F1F000EEECEA00EAE8E7002CCB3F0000CD190000CD
      190000CD190000CD190000CD190000CD190000CD190000CD190000CD190000CD
      190000CD19002CCB4000EAE8E600E8E5E300F1F0EF00110A0A007B7572007D77
      74008B8683002C242100000000000000000000000000CECECE00BABABA00E4E4
      E400EEEEEE00F1F1F100EEEEEE00C5C5C500C9C9C900F2F2F200EAEAEA00D6D6
      D600EDEDED00F7F7F700F6F6F600F4F4F400F0F0F000EDEDED00E8E8E800DDDD
      DD00BCBCBC00C0C0C000D2D2D200CECECE00B7B7B700CACACA00000000000000
      00000000000000000000000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001B100F00F2F0F000EDEAE900EDEBE900EFEDEC00F1F0EF00F4F3
      F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9
      F900F5F5F500F2F1F000EEECEB00EBE9E700F3F2F100110B0A007B7572007D77
      74008B8683002C2421000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200F2F2F200F3F3F300F5F5F500F7F7
      F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FAFA
      FA00F9F9F900F6F6F600F3F3F300F1F1F100F6F6F60071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001B100F00F2F0F000EDEAE900EDEBE900EFEDEC00F1F0EF00F4F3
      F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFCFC00F8F9
      F900F5F5F500F2F1F000EEECEB00EBE9E700F3F2F100110B0A007B7572007D77
      74008B8683002C24210000000000000000000000000000000000BABABA00CACA
      CA00EDEDED00EAEAEA00C2C2C200D9D9D900C5C5C500D5D5D500BFBFBF00DDDD
      DD00E4E4E400CACACA00C7C7C700EBEBEB00EDEDED00EBEBEB00E7E7E700E3E3
      E300DEDEDE00D9D9D900D5D5D500C3C3C300BABABA0000000000000000000000
      00000000000000000000000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001C100F00F2F0F000ECEAE900ECE9E800EDEBE900EFEDEC00F1F0
      EF00F4F3F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFC
      FC00F8F9F900F5F5F500F2F1F000EFEDEC00F5F3F300120B0B007B7572007D77
      74008B8683002C2420000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200F2F2F200F2F2F200F3F3F300F5F5
      F500F7F7F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FAFAFA00F9F9F900F6F6F600F3F3F300F8F8F80071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001C100F00F2F0F000ECEAE900ECE9E800EDEBE900EFEDEC00F1F0
      EF00F4F3F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFEFE00FCFDFE00FAFC
      FC00F8F9F900F5F5F500F2F1F000EFEDEC00F5F3F300120B0B007B7572007D77
      74008B8683002C2420000000000000000000000000000000000000000000B8B8
      B800D5D5D500E6E6E600CBCBCB00D4D4D400BBBBBB00CDCDCD00CFCFCF00D1D1
      D100C6C6C600E4E4E400E1E1E100CCCCCC00D8D8D800E1E1E100E3E3E300E0E0
      E000DBDBDB00D8D8D800CCCCCC00B9B9B9000000000000000000000000000000
      00000000000000000000000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001C100F00F2F0F000ECEAE900E7E4E300C4C3C300C5C4C300C5C5
      C400C6C6C500C7C7C700C8C8C800C9C9C900C9CACA00CACACA00CACACB00CACA
      CB00C9CACA00CACBCB00F5F5F500F2F1F100F6F6F500120B0B007B7572007D77
      74008B8683002C2320000000000000000000000000007D7D7D00D1D1D100AFAF
      AF00AFAFAF0074747400F6F6F600F2F2F200EFEFEF00DCDCDC00DCDCDC00DCDC
      DC00DDDDDD00DEDEDE00DFDFDF00DFDFDF00DFDFDF00E0E0E000E0E0E000E0E0
      E000DFDFDF00E0E0E000F9F9F900F6F6F600F9F9F90071717100AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000311E1B00B5B1AF007D77
      74007D7774001C100F00F2F0F000ECEAE900E7E4E3002CCB3F0000CD190000CD
      190000CD190000CD190000CD190000CD190000CD190000CD190000CD190000CD
      190000CD19002DCD4100F5F5F500F2F1F100F6F6F500120B0B007B7572007D77
      74008B8683002C2320000000000000000000000000000000000000000000DDDD
      DD00BABABA00D5D5D500D4D4D400C0C0C000BCBCBC00DDDDDD00D8D8D800CACA
      CA00C2C2C200E6E6E600EDEDED00E8E8E800D9D9D900CBCBCB00D6D6D600DDDD
      DD00DBDBDB00CFCFCF00BBBBBB00D8D8D8000000000000000000000000000000
      00000000000000000000000000000000000000000000311E1B00B2AEAC006760
      60007D7774001C100F00F2F0F000ECEAE900ECE9E800ECE9E800ECE9E800EDEB
      E900EFEDEC00F1F0EF00F4F3F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFE
      FE00FCFDFE00FAFCFC00F8F9F900F6F6F600F8F8F700130C0B007B7572007D77
      74008B8683002C2320000000000000000000000000007D7D7D00CFCFCF00A2A2
      A200AFAFAF0074747400F6F6F600F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F3F3F300F5F5F500F7F7F700F9F9F900FBFBFB00FCFCFC00FDFDFD00FDFD
      FD00FDFDFD00FCFCFC00FAFAFA00F9F9F900FAFAFA0072727200AEAEAE00AFAF
      AF00B8B8B8007F7F7F00000000000000000000000000311E1B00B2AEAC006760
      60007D7774001C100F00F2F0F000ECEAE900ECE9E800ECE9E800ECE9E800EDEB
      E900EFEDEC00F1F0EF00F4F3F200F6F6F600F9F9F900FBFBFC00FCFDFE00FDFE
      FE00FCFDFE00FAFCFC00F8F9F900F6F6F600F8F8F700130C0B007B7572007D77
      74008B8683002C23200000000000000000000000000000000000000000000000
      0000DDDDDD00B9B9B900BFBFBF00C7C7C700B8B8B800C9C9C900D1D1D100D1D1
      D100D0D0D000E7E7E700EEEEEE00EEEEEE00EDEDED00DDDDDD00D1D1D100DADA
      DA00CECECE00BBBBBB00D3D3D300000000000000000000000000000000000000
      00000000000000000000000000000000000000000000311E1B00B0ACAA004B47
      47007D7774001D111000F2F0F000EDEAE900ECEAE900ECEAE900ECEAE900EDEA
      E900EEEBEA00F0EEEC00F2F1EF00F4F3F300F7F6F600F9F9FA00FBFBFC00FCFD
      FE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F9FAFA00140C0C007B7572007D77
      74008B8683002D2421000000000000000000000000007D7D7D00CECECE009494
      9400AFAFAF0075757500F6F6F600F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F3F3F300F4F4F400F6F6F600F8F8F800F9F9F900FBFBFB00FCFCFC00FDFD
      FD00FDFDFD00FDFDFD00FCFCFC00FAFAFA00FBFBFB0072727200AEAEAE00AFAF
      AF00B8B8B80080808000000000000000000000000000311E1B00B0ACAA004B47
      47007D7774001D111000F2F0F000EDEAE900ECEAE900ECEAE900ECEAE900EDEA
      E900EEEBEA00F0EEEC00F2F1EF00F4F3F300F7F6F600F9F9FA00FBFBFC00FCFD
      FE00FDFEFE00FCFDFE00FAFCFC00F8F9F900F9FAFA00140C0C007B7572007D77
      74008B8683002D24210000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00B4B4B400B8B8B800CECECE00CACACA00CACA
      CA00E0E0E000F0F0F000F3F3F300F3F3F300F2F2F200E2E2E200C6C6C600BFBF
      BF00BFBFBF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000311E1B00CCC9C800B5B2
      B000B5B2B0001E121100C3B5D000C1B2CE00C1B2CE00C1B2CE00C1B2CE00C1B2
      CE00C1B2CE00C1B2CE00C1B2CE00C1B2CF00C1B2CF00C2B2CF00C2B2CF00C2B2
      CF00C2B3CF00C2B3CF00C2B3CF00C2B2CF00C5B7D100140C0C00B2AFAD00B5B1
      AF008E8986002E2421000000000000000000000000007D7D7D00DFDFDF00D1D1
      D100D1D1D10076767600D9D9D900D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800DADADA0072727200D0D0D000D1D1
      D100B9B9B90080808000000000000000000000000000311E1B00CCC9C800B5B2
      B000B5B2B0001E121100C3B5D000C1B2CE00C1B2CE00C1B2CE00C1B2CE00C1B2
      CE00C1B2CE00C1B2CE00C1B2CE00C1B2CF00C1B2CF00C2B2CF00C2B2CF00C2B2
      CF00C2B3CF00C2B3CF00C2B3CF00C2B2CF00C5B7D100140C0C00B2AFAD00B5B1
      AF008E8986002E24210000000000000000000000000000000000000000000000
      0000000000000000000000000000D2D2D200B9B9B900B8B8B800BABABA00CBCB
      CB00E6E6E600F3F3F300EBEBEB00D6D6D600C5C5C500BBBBBB00BBBBBB00D4D4
      D400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000534442003D2C29003C2A
      27003C2A27001E111000B5A5C100BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAF
      CD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAF
      CD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00B4A5C100140B0A00382D2A00382E
      2B00362D2A00645D5B0000000000000000000000000092929200858585008484
      84008484840075757500D0D0D000D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D0D0D00071717100858585008585
      850085858500A0A0A000000000000000000000000000534442003D2C29003C2A
      27003C2A27001E111000B5A5C100BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAF
      CD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00BFAF
      CD00BFAFCD00BFAFCD00BFAFCD00BFAFCD00B4A5C100140B0A00382D2A00382E
      2B00362D2A00645D5B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CECECE00BFBF
      BF00B7B7B700B5B5B500B8B8B800C0C0C000CECECE0000000000000000000000
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
      0000000000000000000000000000000000000000000000000000D3D3D300A8A8
      A800A7A7A7000000000000000000000000000000000000000000C1C1C100A7A7
      A700A9A9A9000000000000000000000000000000000000000000000000000000
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
      0000D8D8D800C2C2C200B9B9B900B7B7B700B7B7B700B7B7B700B7B7B700B7B7
      B700B7B7B700B7B7B700B7B7B700B7B7B700B9B9B900DBDBDB00B7B7B700BCBC
      BC00B8B8B8009F9F9F00AEAEAE00B7B7B700CCCCCC00B1B1B100ACACAC00C9C9
      C900AAAAAA000000000000000000000000000000000000000000000000000000
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
      0000000000006198B2002992C1000987C0000084C0000987C0002892C1005F98
      B200000000000000000000000000000000000000000000000000000000000000
      0000D0D0D000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFDFD00FFFFFF00C9C9C900C0C0
      C000D5D5D500CBCBCB00A8A8A800BCBCBC00A6A6A600BCBCBC00D4D4D400D5D5
      D500A5A5A50000000000000000000000000000000000839FA7000077A5000077
      A8000077A8000077A8000077A8000077A8000077A8000077A8000077A8000077
      A8000077A8000077A8000077A8000077A8000077A8000077A8000077A8000077
      A8000077A8000077A8000077A8000077A8000077A8000077A800067AA900528A
      A2000000000000000000000000000000000000000000C6C6C600B0B0B000B0B0
      B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0
      B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0
      B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B0B0B000B1B1B100BBBB
      BB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000499A
      BE001A97CA0071C8E600A2E3F500B7EFFD00B6EDFE00A7E5FB008CD5F10060BC
      E2001695CA004691B30000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FDFDFD00E2E2E200B1B1
      B100D8D8D800D8D8D800D6D6D600C2C2C200CECECE00D8D8D800D8D8D800CDCD
      CD00ACACAC00000000000000000000000000000000002F7F9A008DCBE200D9F5
      FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5
      FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5
      FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D9F5FF00D2F1FD002892
      BB000000000000000000000000000000000000000000B3B3B300E0E0E000F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F6F6F600BFBF
      BF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005DB2D6003FAC
      D900B5EFFF00B5F4FF00A8EFFD0099E8FB0087DEF90075D5F40069CCF30070CD
      F40092D9F80036A5D500649CB600000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FDFDFD00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FCFCFC00F6F6F600A2A2
      A200D9D9D900DBDBDB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00DBDBDB00BBBB
      BB00D2D2D20000000000000000000000000000000000417C8F006DBAD700BCED
      FF0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1
      FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1
      FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE0091E1FE00BCEDFF0071BC
      D8006690A20000000000000000000000000000000000B1B1B100D6D6D600F3F3
      F300EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00F3F3F300D7D7
      D700BEBEBE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DEE0E1000887C0009CE2
      FD0097E8FE00B0F3FE00B1F1FD00A4EBFB0095E2FA0082D9F70071CFF30058C4
      F00059C2F0008AD3F300138CC100000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FDFDFD00FDFDFD00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00FBFBFB00FBFBFB00FBFBFB00FAFAFA00F9F9F900FCFCFC00B0B0
      B000CFCFCF00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00ACAC
      AC00000000000000000000000000000000000000000060797D003BA0C500C9F0
      FE007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9
      FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9
      FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC007FD9FC009BE2FD00AADD
      EF002C88AB0000000000000000000000000000000000B0B0B000C7C7C700F5F5
      F500E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800EDEDED00EAEA
      EA00B9B9B9000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008D7D7700855D54009859
      4A00A3564300A3523F00A25543009958480084594E00AFA7A8000088C200B1EB
      FF00A4EBFE0098E3F20084D7E90079D2E80066C7E50050BCE10039B0DE003AB1
      E3006CC9F200A3DFF9000687C100000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FBFBFB00FBFBFB00FAFAFA00FAFAFA00F9F9F900F4F4F400C0C0C000ACAC
      AC00DCDCDC00E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000CCCC
      CC00A2A2A200000000000000000000000000000000007B766E00198DB900C9EE
      FC0079D7FB006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3
      FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3
      FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA006BD3FA0072D5FA00CFF1
      FE001D8FBB0000000000000000000000000000000000AEAEAE00BCBCBC00F4F4
      F400E7E7E700E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E6E6E600F6F6
      F600BEBEBE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000938173005D64480050633E00A76E4D00CF7E
      5E00D5836200D8856200D6846200CF7E5D00C5755700CCA397000C90C60091D8
      F20073CFEB0095E8F70097EBFB0088E2F80074D8F5005CCBF10041BDED0027AC
      E5002DA6DC0086CCEC000487C100000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFB
      FB00FBFBFB00FAFAFA00FAFAFA00F8F8F800EBEBEB00AAAAAA00BFBFBF00E1E1
      E100E2E2E200E2E2E200E2E2E200E2E2E200E2E2E200E2E2E200E2E2E200E2E2
      E200D8D8D800AAAAAA00C5C5C500000000000000000082776D00439EC0009BD6
      ED0086DBFB0058CCF70058CCF70058CCF70058CCF70058CCF70058CCF70058CC
      F70058CCF70058CCF70058CCF70058CCF70058CCF70058CCF70058CCF70058CC
      F70058CCF70058CCF70058CCF70058CCF70058CCF70058CCF70058CCF700B5E9
      FD0052AED10000000000000000000000000000000000AEAEAE00C6C6C600E7E7
      E700E9E9E900E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0
      E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0
      E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000E0E0E000F2F2
      F200CFCFCF000000000000000000000000000000000000000000000000000000
      00000000000000000000A967570023683D0026A97B0030A4760063825600B07E
      5500D5825B00D8825C00D9835C00D9845D00DB875F00E3B8A4000087BD0087D7
      F40083E2FC009CEFFD0097EBFB0088E2F80074D8F5005CCBF10041BDED0029B1
      EA0035B3EB0079C7EC000484BC00000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFBFB00FAFA
      FA00FAFAFA00FAFAFA00F9F9F900F6F6F600A5A5A500D0D0D000E4E4E400E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4E400E4E4
      E400E4E4E400E2E2E200B3B3B300CACACA0000000000847A6F0079B3C8006CBD
      DB009FE2FC0048C7F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7
      F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7
      F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7F60048C7F6008CDD
      FB0084CAE5005A93A700000000000000000000000000B0B0B000D2D2D200D8D8
      D800EEEEEE00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00EAEA
      EA00E0E0E000BFBFBF0000000000000000000000000000000000000000000000
      0000CDA59C00B56A5300CD7E5E000C7643002EC69E0031C59D002DAC8000587C
      4D00BF7B5200D6815800D6805800D6805800D67E5700DFB39E000087C00091E0
      FD0092E7FD00BDF7FF00BBF3FE00B3EFFD00A6E9FB0097E0F90085D8F70072CE
      F3004FBDEE0081CFF3000485BE00000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FCFCFC00FBFBFB00FBFBFB00FBFBFB00FAFAFA00FAFA
      FA00FAFAFA00FAFAFA00FBFBFB00E7E7E700AEAEAE00DCDCDC00E0E0E000DDDD
      DD00DBDBDB00E1E1E100E5E5E500E5E5E500E5E5E500E5E5E500DFDFDF00DEDE
      DE00E2E2E200E0E0E000CECECE00B2B2B20000000000897D7200ACC6CF003EA7
      CC00BDEDFD0043C6F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6
      F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6
      F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6F60042C6F60064D1
      F900B5E5F6002D96BE00000000000000000000000000B2B2B200DDDDDD00CBCB
      CB00F3F3F300DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E3E3
      E300EFEFEF00C1C1C1000000000000000000000000000000000000000000CFA5
      9B00BA6C5300D4856200DA875F003E6C3D001BAD84002CC9A20036D5AA0039C0
      910051855300C0875C00DE896100D6845D00D5825B00DEB39E00048FC500C0F0
      FF00A0E8FB008EDEEE0083D6E80075CEE60065C6E3004EBADF0038AEDC0029A8
      DE0068C6EF00B6E7FC000489C200000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FBFBFB00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFA
      FA00FAFAFA00F9F9F900FBFBFB00F9F9F900C9C9C900A9A9A900A4A4A400A8A8
      A800ADADAD00A7A7A700E4E4E400E6E6E600E6E6E600C9C9C900A2A2A200A4A4
      A400A1A1A100A2A2A200B3B3B300EAEAEA00000000008D817500D5D5D3002C9B
      C400C4ECFB0080DDFF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8
      FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8
      FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF006ED8FF0070D8
      FF00CCF1FF0039A4CB00000000000000000000000000B4B4B400E6E6E600C4C4
      C400F3F3F300EAEAEA00E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7
      E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7
      E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7
      E700F6F6F600C9C9C9000000000000000000000000000000000000000000B767
      5000D5846200DA845E00DE845900B6895D0008A2700032E0B5002DDFB20040EC
      BF0042DEAD0034905900F09C6F00F49B6F00E7916800E0B7A2000E94C80080CC
      E90073D0EB0095E9F70097EBFB0088E2F80074D8F5005CCBF10041BDED0027AE
      E7002DA6DC0073C0E200048BC200000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FBFBFB00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9
      F900F9F9F900F9F9F900F9F9F900FCFCFC00FCFCFC00FCFCFC00FBFBFB00FBFB
      FB00FBFBFB00B3B3B300D5D5D500E6E6E600E6E6E600B3B3B300E8E8E800DDDD
      DD00F9F9F900FEFEFE00FFFFFF00000000000000000093867B00DDD9D70059A9
      C50098D5EC00A0E5FF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DA
      FF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DA
      FF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DAFF0076DA
      FF00B8ECFF0068BCDB00000000000000000000000000B7B7B700E8E8E800CCCC
      CC00E6E6E600EFEFEF00E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800F3F3F300D7D7D70000000000000000000000000000000000B56A5400D281
      5F00E28D6300F0906400F79A6C0082915D0021BD88003CECBF0029DEB00029DE
      B0003AEBBD003FE5B50063885200F9A17400FDA27500F2C4AC000086BC008FDE
      FB0083E2FC009CEFFD0097EBFB0088E2F80074D8F5005CCBF10041BDED0029B1
      EA0035B3EB007FCEF2000386BB00000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FAFAFA00FAFAFA00FAFAFA00FAFAFA00F9F9F900F9F9
      F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F8F8F800F7F7F700F8F8
      F800FAFAFA00D5D5D500BEBEBE00E6E6E600E4E4E400A2A2A200F6F6F600C5C5
      C5000000000000000000000000000000000000000000998C7F00E2E0DB008CBA
      C90068BCDB00BBEDFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDB
      FF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDB
      FF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDBFF007EDB
      FF00A5E6FF0098D5EC005395AE000000000000000000BABABA00EBEBEB00D6D6
      D600D7D7D700F3F3F300EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEA
      EA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEA
      EA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEAEA00EAEA
      EA00F0F0F000E6E6E600C0C0C0000000000000000000C9978800C5725300ED96
      6C00F9976A00FE9B6F00BD93620017AF780046F2C50030E2B50029DEB00030E0
      B4003FE6BF0056F7CE0029A26C00DC9B6C00FDA27500F4C5AE00008EC50093E1
      FE0081E1FC008CE6F5007EDCEF006FD4EC005DCAEA004AC1E70035B6E50025AE
      E60035B3EB0083D2F500038CC300000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FAFAFA00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9
      F900F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6
      F600FAFAFA00EEEEEE00A6A6A600E4E4E400D1D1D100BABABA00FDFDFD00B8B8
      B80000000000000000000000000000000000000000009F918300E9E5E200C1CE
      CC003AA4CB00D1F3FF0089DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DE
      FE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DE
      FE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DEFE0086DE
      FE0096E2FE00C7EDFB002C99C1000000000000000000BDBDBD00EFEFEF00E0E0
      E000C9C9C900F7F7F700EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EDEDED00F3F3F300C2C2C2000000000000000000B96B5200EA936C00FE9E
      7200FE9B6E00FDA071006A87510033DCAC0036E6BA0029DEB0002BDEB20040E5
      BD005CE1C30064D1AF005BA47500D39B6B00FDA27600F8C6AE00008DC40080D6
      F00094E3F300B4F2FC00C8FCFF00CFFFFF00D3FFFF00CDFEFF00C0F8FF00A2E9
      F9007FD6EF0077CCEB00048CC300000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FAFAFA00F9F9F900F9F9F900F9F9F900F9F9F900F8F8
      F800F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6F600F6F6
      F600F8F8F800FAFAFA00BABABA00BCBCBC00A9A9A900E3E3E300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000A4968800F0EDE800E5DB
      D200339DC400C1E8F700A1E4FD0090DFFC0090DFFC0090DFFC0090DFFC0090DF
      FC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DF
      FC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DFFC0090DF
      FC0090DFFC00D2F2FE0042A8CE000000000000000000C0C0C000F3F3F300E8E8
      E800C5C5C500F1F1F100EEEEEE00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEBEB00EBEB
      EB00EBEBEB00F6F6F600CBCBCB000000000000000000C5705000FFA47A00FFA0
      7100FEA27400FDA677007A8F5B0025CC9C0039E5BB0039E4BC0049EAC60033D1
      A70061AA7C009C9D6C00CD9E6E00F8A77800FDA77A00EEC7AD00028CC100C7FB
      FF00D3FFFF00D3FFFF00D3FFFF00D3FFFF00D3FFFF00D3FFFF00D3FFFF00D3FF
      FF00D3FFFF00C4F9FF00098FC400000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F9F9F900F9F9F900F9F9F900F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6F600F6F6F600F6F6
      F600F6F6F600FAFAFA00F3F3F300CFCFCF00DFDFDF00F2F2F200FFFFFF00B8B8
      B8000000000000000000000000000000000000000000A8998A00F6F2EE00ECE1
      D60068AFC70091D0E700BBEBFD009AE1FB009AE1FB009AE1FB009AE1FB009AE1
      FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1
      FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1FB009AE1
      FB009AE1FB00C5EEFD006FBEDD000000000000000000C2C2C200F6F6F600EBEB
      EB00D0D0D000E3E3E300F2F2F200EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00F4F4F400D9D9D90000000000CA8F7A00DA845F00FFA97B00FFA4
      7500FDA97B00FBAD7E009097650027C1940069F7DA006FF4DB0052D8B6005DAD
      7E00CCA97B00FAB08300FDAF8200FDAE8100FBAE8000D2D0B7001DA3B30026A4
      D2007DD3EC00A4E8F800C6FAFF00CDFEFF00D2FFFF00CDFDFF00C4F9FF00A2E7
      F7007AD0EB0023A2D00060B6D700000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F9F9F900F9F9F900F8F8F800F8F8F800F8F8F800F8F8
      F800F7F7F700F7F7F700F6F6F600F6F6F600F6F6F600F6F6F600F6F6F600F5F5
      F500F5F5F500F6F6F600FAFAFA00F9F9F900F3F3F300F3F3F300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AA9B8C00FBF5F100F2E6
      DA00A2C5CE0054B0D300D5F2FD00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4
      FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4
      FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4FF00D9F4
      FF00D9F4FF00D9F4FF008ACCE5007EBAD10000000000C3C3C300F8F8F800EEEE
      EE00DCDCDC00D0D0D000F6F6F600F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800F8F8F800F8F8F800E0E0E000D7D7D700BD715600EB946B00FFAA7C00FEAA
      7C00D8A97800769C680040BA890026AC7D0046B085004FAD810087AF8200D4B5
      8800F9BA8E00FDBA8F00FDB98B00FDB88900F9B5880085B0800071E2C30037D2
      C5001AB1BE000DA2C2000493C000028EC100008BC300018DC0000C88B70037A2
      CA006DBBD8000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F7F7
      F700F7F7F700F6F6F600F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5
      F500F5F5F500F4F4F400F4F4F400F4F4F400F3F3F300F3F3F300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FEF9F200F9EB
      DE00ECE3D70062AEC8002298C3001F97C3001F97C3001F97C3001F97C3001F97
      C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97
      C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97C3001F97
      C3001F97C3001F97C3002C9DC6000000000000000000C3C3C300FAFAFA00F1F1
      F100EDEDED00CFCFCF00C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2
      C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2
      C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2C200C2C2
      C200C2C2C200C2C2C200C5C5C50000000000B9644200F49D7400FDAD7D0096A3
      700032B07B0043D8AD0024AE8000B0B38300E6C19500E7C19700F4C59C00FFC7
      A000FFC79E00FFC59C00FEC29700FEC09500FDBF930095A9780040C89E00A7F6
      E60089F0DA0087EFD8008DEFDA00A0ECDF00A5E8DD008AE0CA00AAB29D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F8F8F800F8F8F800F8F8F800F7F7F700F7F7F700F6F6
      F600F6F6F600F6F6F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFF9F300FDEE
      DF00FAEBDB00F6E7D900F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6
      D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6
      D800F5E6D800F5E6D800F5E6D800F5E6D800F5E6D800F5EFE900A59688000000
      00000000000000000000000000000000000000000000C3C3C300FAFAFA00F3F3
      F300F1F1F100EFEFEF00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEE
      EE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEE
      EE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00EEEEEE00F4F4F400C0C0C0000000
      000000000000000000000000000000000000BA654000D59E70005F9A640024BB
      880043E7BF001FAD7E0085AC7B0070A97800CCC19900FFD5B200FFD6B400FFD8
      B400FFD6B300FFD4AF00FFD1AA00FFCDA600FFCBA100F1C397006FA97A003DCE
      A90069F4DB0069F7DE005DEED30037B8930019875D001D976F007A734D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F8F8F800F8F8F800F7F7F700F7F7F700F6F6F600F6F6
      F600F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5F500F4F4F400F4F4
      F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFFAF300FFF0
      E000FEEEDF00FDEDDE00FDEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCED
      DD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCED
      DD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCEDDD00FCF5F000A8998A000000
      00000000000000000000000000000000000000000000C3C3C300FAFAFA00F3F3
      F300F3F3F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F8F8F800C2C2C2000000
      000000000000000000000000000000000000AE6843005D99680013B9850037E0
      B00044EEC1002BC08E0020AF7B002DC999008FBA8E00FFE1C100FFE4C500FFE4
      C600FFE4C500FFE0C100FFDCBB00FFD9B400FFD5AE00FFD2A800DFC3960038AE
      7E003ECDA8004ACFAF0044C5A00025905E004A88590049835300A06F4B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F7F7F700F7F7F700F6F6F600F6F6F600F6F6F600F6F6
      F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3
      F300F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFF9F200FFF1
      E200FFF1E300FFF1E300FFF1E300FFF1E300FFF1E300FFF1E300FFF1E300FFF1
      E300FFF0E100FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFEF
      DE00FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFEFDE00FFF9F200A99A8B000000
      00000000000000000000000000000000000000000000C3C3C300FAFAFA00F5F5
      F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3
      F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300FAFAFA00C2C2C2000000
      000000000000000000000000000000000000A97E5A000CA977003DE6BA0049F4
      CC004BF7D3004DF9D3004DF8D10049F1C3004ABF8F00CED5B300FFE8CD00FFED
      D400FFEDD300FFEACF00FFE6C900FFE1C100FFDCBA00FFD8B200E0C89E0042BD
      8F0036D5AD000EB07B001DA871000DB27E0018B084000B9A6A008D7A5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F7F7F700F6F6F600F6F6F600F6F6F600F6F6F600F5F5
      F500F5F5F500F5F5F500F5F5F500F4F4F400F4F4F400F4F4F400F3F3F300F3F3
      F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2F200FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFFCF700FFE6
      D200FFDFC400FFE1C400FFE2C500FFE4C600FFE6C700FFE7C800FFE9C800FFEB
      C900FFF1D900FFF0E100FFF6ED00FFF9F200FFF9F200FFF9F200FFF9F200FFF9
      F200FFF9F200FFF9F200FFF9F200FFF9F200FFF9F200FFFBF600AA9B8C000000
      00000000000000000000000000000000000000000000C3C3C300FCFCFC00EEEE
      EE00E9E9E900EAEAEA00EAEAEA00EBEBEB00ECECEC00EDEDED00EDEDED00EEEE
      EE00F3F3F300F4F4F400F8F8F800FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFA
      FA00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FBFBFB00C2C2C2000000
      000000000000000000000000000000000000BB9F820031A677004CF7D3004BF7
      D80046F4D80046F4D80046F4D6004BF7D30040E6B90050CDA00095CEA900EAE6
      CC00FFF4DE00FFF3DB00FFEED500FFE8CD00FFE2C300FFDCBA00FFD5AF0090BF
      940039CBA40058F4D1003FE2B90055F3CC0044DBB900329B70009F9279000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F6F6F600F6F6F600F6F6F600F6F6F600F5F5F500F5F5
      F500F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3
      F300F3F3F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFFCF900FFD9
      B900FFCCA100FFCFA200FFD2A400FFD5A500FFD7A700FFDAA800FFDEAA00FFE0
      AB00FFECCA00FFF5E900E6DFD800AE9F9100AE9F9100AE9F9100AE9F9100AE9F
      9100AE9F9100AE9F9100AE9F9100AE9F9100AE9F9100AD9E9000BEB8AC000000
      00000000000000000000000000000000000000000000C3C3C300FCFCFC00E5E5
      E500DCDCDC00DEDEDE00DFDFDF00E0E0E000E2E2E200E3E3E300E4E4E400E5E5
      E500EFEFEF00F7F7F700EBEBEB00C5C5C500C5C5C500C5C5C500C5C5C500C5C5
      C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500D3D3D3000000
      000000000000000000000000000000000000000000005E8E61003CE6C5004DF8
      DE004AF8DC004AF8DC004AF8DC0055F9E0005BFDDE004AF2C90030CC9D004FC3
      9700B3DABC00FFF5E000FFF4DF00FFEDD600FFE7CC00FFE1C200FFDBB900F8D2
      AC002AAE7D0034D4AE0055EDCF0070F9E1003ED1B2005C8A5F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F6F6F600F6F6F600F5F5F500F5F5F500F5F5F500F5F5
      F500F4F4F400F4F4F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3
      F300F9F9F900FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FFFFFF00B8B8
      B8000000000000000000000000000000000000000000AB9C8D00FFFBF500FFEE
      DF00FFEBD800FFECD800FFECD900FFEDD900FFEEDA00FFEFDA00FFF0DB00FFF0
      DB00FFF4E800FAF4EF00B3A69800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C300FBFBFB00F3F3
      F300F1F1F100F1F1F100F1F1F100F2F2F200F2F2F200F2F2F200F3F3F300F3F3
      F300F6F6F600F8F8F800C9C9C900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000958860002EC7A10052FA
      E1004FFAE1004DF9E10062FEE80077FFEE0071FDE8005EFEE1004DF5CF0030D5
      AD007ACDAA00FFF5E200FFF8E400FFF2DC00FFEBD200FFE4C700FFDEBD00ECD1
      AC002DBB8E0013B385000EAC7C0052BB9300639E710098865F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F6F6F600F5F5F500F5F5F500F5F5F500F5F5F500F4F4
      F400F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2
      F200FAFAFA00CFCFCF00C9C9C900C9C9C900C9C9C900D8D8D800FFFFFF00C2C2
      C2000000000000000000000000000000000000000000BDB2A600DDD4CB00FDF7
      F100FFF9F000FFF9F000FFF9F000FFF9F000FFF9F000FFF9F000FFF9F000FFF9
      F000FAF5EF00C3B7AB00D5CDC600000000000000000000000000000000000000
      000000000000000000000000000000000000000000007DB17F00207E2300207F
      2400207F240022802600599E5A000000000000000000D0D0D000E4E4E400F9F9
      F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9F900F9F9
      F900F8F8F800D3D3D300E1E1E100000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C5C5C5009D9D9D009D9D
      9D009D9D9D009E9E9E00B6B6B6000000000000000000C4B29800559E750049ED
      D40057FDE70069FFED0066FAE50025BA90001EC2990075FFED0063F4DC005DD1
      AE00DFE8D100FFF7E400FFF5E100FFF2DE00FFECD400FFE5CB00FFDFC000E1CE
      AA0019AC7C0063AA7A00E6BA9000EEB28700D5895C00D0A98E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F5F5F500F5F5F500F5F5F500F5F5F500F4F4F400F4F4
      F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2
      F200FAFAFA00C7C7C700FAFAFA00FAFAFA00F5F5F500FFFFFF00C9C9C900DEDE
      DE00000000000000000000000000000000000000000000000000BFB3A900AC9E
      8F00AA9B8C00AA9B8C00AA9B8C00AA9B8C00AA9B8C00AA9B8C00AA9B8C00AA9B
      8C00AFA09200CFC6BD0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001E7E22000084
      220000892900008929001F7E2300000000000000000000000000D1D1D100C4C4
      C400C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3
      C300C6C6C600DCDCDC0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009D9D9D009D9D
      9D00A0A0A000A0A0A0009D9D9D000000000000000000000000009B946B003DBF
      99005CF9E20056F5DE002EB38700C7CBA30017BC910033D6B3000DB28300A6D5
      B500C5DEC00030C3940030BF8F00EAE6CB00FFE7CD00FFE4C700FFDCBD00FFD5
      B300FFCDA800FFC59D00FFBC9400F59D7100D68D650000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F5F5F500F5F5F500F4F4F400F4F4F400F3F3F300F3F3
      F300F3F3F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200FAFAFA00C7C7C700FCFCFC00F8F8F800FFFFFF00C9C9C900D8D8D8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000074
      0F00008A2A00008A2A001C7D2100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009494
      9400A1A1A100A1A1A1009D9D9D00000000000000000000000000000000007997
      6A004CD2B00050F1CE005CC19300B6C39B001EA978001DCB9E0025CEA10074C6
      A0003CB98D004BF2C70044EEC00039C7970090CFA800D9D6B400FFD5B400FFCF
      AC00FFC8A200FFC29B00FEAC8100DD8B5F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F5F5F500F4F4F400F4F4F400F3F3F300F3F3F300F3F3
      F300F3F3F300F3F3F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200FAFAFA00C7C7C700F6F6F600FFFFFF00C9C9C900DBDBDB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001B7D22000088
      2700007F1C00008422001C7D2100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009C9C9C009F9F
      9F009A9A9A009D9D9D009D9D9D0000000000000000000000000000000000D9CA
      B400799B6F004FD2AD002ED5AD0019B5840013AD7C003CE4C0003ED9B4005CBC
      910021B587004AF2CF0053FEDB004DF9D10031E0B20040C39400C9C59E00FFCB
      A700FFC8A200FFB08800E18F6300E9BEA7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00F4F4F400F3F3F300F3F3F300F3F3F300F3F3F300F3F3
      F300F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F2F2F200F1F1F100F1F1
      F100FAFAFA00D5D5D500FFFFFF00C9C9C900DFDFDF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001E7F230000882600007E
      1B00539B550026822900197A1C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009D9D9D009F9F9F009A9A
      9A00B4B4B400A0A0A0009A9A9A00000000000000000000000000000000000000
      0000DECAB40085996C0056A97E0017C0950011A775003DBF950088C39D00ADBF
      970046CC9D004DF7CE0052FDE50053FDE40058FEDF0040E5BA0089C59900FAC6
      A000FBAF8500E08F6200E7B79B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1D1D100FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C9C9C900DFDFDF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000D70
      0D005B9E5B00000000000000000069A86A000877140000882700007A14005EA2
      5F00000000000000000067A56700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009393
      9300B6B6B6000000000000000000BDBDBD00979797009F9F9F0097979700B8B8
      B8000000000000000000BBBBBB00000000000000000000000000000000000000
      00000000000000000000B6956B00499764003FA16E00A4BB90006FBB8E004BC0
      91004FE8C00069FFE50075FFF2007CFFF4007AFFEE004FE8C90056B68900C99A
      6A00E0966D000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DEDEDE00D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000D0D0D000C9C9C900E1E1E100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001B7A1D0000710B0000761100007E1A00007C180021802700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009A9A9A0093939300969696009A9A9A00989898009F9F9F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CDB99B00BD906200C18F5D004DA2700025C6
      9D005DEED40080FFED006BF4DE0044D5B50045B58D00789D6F00AF956900DCB9
      9D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007CB37D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C5C5C500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4B3930097A1
      740072996600679666007498680095A17700BCB4940000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000400100000100010000000000001400000000000000000000
      000000000000000000000000FFFFFF00FFFF8000FFFFFFFFFFFFFFFF00000000
      FFFF8000FFF00FFFFFF00FFF00000000FFC78000FF8003FFFF8003FF00000000
      FFE38000FE0000FFFE0000FF00000000FFF18000FC00007FFC00007F00000000
      FFF88000F800003FF800003F00000000FF800000F000001FF000001F00000000
      06000000E000000FE000000F000000000C388000C0000007C000000700000000
      18F18000C0000007C00000070000000019E38000800000038000000300000000
      11468000800000038000000300000000137C8000800000038000000300000000
      120080000000000100000001000000001E008000000000010000000100000000
      0000FFFF000000010000000100000000000007FF000000010000000100000000
      000007FF000000010000000100000000000007FF000000010000000100000000
      000007FF000000010000000100000000000007FF800000038000000300000000
      000007FF800000038000000300000000000007FF800000038000000300000000
      000007FFC0000007C000000700000000000007FFC0000007C000000700000000
      000007FFE000000FE000000F00000000000007FFF000001FF000001F00000000
      00000FFFF800003FF800003F0000000000001FFFFC00007FFC00007F00000000
      00003FFFFE0000FFFE0000FF0000000000007FFFFF8003FFFF8003FF00000000
      0000FFFFFFF01FFFFFF01FFF00000000FFFFFFFFFFE00000FFE00000FFFF8000
      FFF00FFFFFE00000FFE00000FFFF8000FF8003FFFFE00000FFE00000FFC78000
      FE0000FFFFE00000FFE00000FFE38000FC00007FFFE00000FFE00000FFF18000
      F800003FFFE00000FFE00000FFF88000F000001FFFE00000FFE00000FF800000
      E000000FFFE00000FFE0000006000000C0000007FFE00000FFE000000C388000
      C0000007FFE00000FFE0000018F1800080000003FFE00000FFE0000019E38000
      80000003FFE00000FFE000001146800080000003FFE00000FFE00000137C8000
      00000001FFE00000FFE000001200800000000001FFE00000FFE000001E008000
      00000001FFE00000FFE000000000FFFF00000001FFFF0CC0FFFF0CC0000007FF
      000000010001186000011860000007FF000000010001303100013031000007FF
      000000010001E01B0001E01B000007FF800000030001C48F0001C48F000007FF
      800000030001CCCF0001CCCF000007FF800000030001DCEF0001DCEF000007FF
      C00000070001F8FF0001F8FF000007FFC00000070001F9FF0001F9FF000007FF
      E000000F0001F1FF0001F1FF000007FFF000001F0001C3FF0001C3FF000007FF
      F800003F000187FF000187FF00000FFFFC00007F00019FFF00019FFF00001FFF
      FE0000FF0001FFFF0001FFFF00003FFFFF8003FF0001FFFF0001FFFF00007FFF
      FFF01FFF0001FFFF0001FFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFF00FFFFFF00FFFFFF00FFFFFF00FFFFF8003FFFF8003FFFF8003FFFF8003FF
      FE0000FFFE0000FFFE0000FFFE0000FFFC00007FFC00007FFC00007FFC00007F
      F800003FF800003FF800003FF800003FF000001FF000001FF000001FF000001F
      E000000FE000000FE000000FE000000FC0000007C0000007C0000007C0000007
      C0000007C0000007C0000007C000000780000003800000038000000380000003
      8000000380000003800000038000000380000003800000038000000380000003
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000180000003800000038000000380000003
      8000000380000003800000038000000380000003800000038000000380000003
      C0000007C0000007C0000007C0000007C0000007C0000007C0000007C0000007
      E000000FE000000FE000000FE000000FF000001FF000001FF000001FF000001F
      F800003FF800003FF800003FF800003FFC00007FFC00007FFC00007FFC00007F
      FE0000FFFE0000FFFE0000FFFE0000FFFF8003FFFF8003FFFF8003FFFF8003FF
      FFF01FFFFFF01FFFFFF01FFFFFF01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFF00FFFFFF00FFFFFF00FFFFFF00FFFFF8003FFFF8003FFFF8003FFFF8003FF
      FE0000FFFE0000FFFE0000FFFE0000FFFC00007FFC00007FFC00007FFC00007F
      F800003FF800003FF800003FF800003FF000001FF000001FF000001FF000001F
      E000000FE000000FE000000FE000000FC0000007C0000007C0000007C0000007
      C0000007C0000007C0000007C000000780000003800000038000000380000003
      8000000380000003800000038000000380000003800000038000000380000003
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000100000001000000010000000100000001
      0000000100000001000000010000000180000003800000038000000380000003
      8000000380000003800000038000000380000003800000038000000380000003
      C0000007C0000007C0000007C0000007C0000007C0000007C0000007C0000007
      E000000FE000000FE000000FE000000FF000001FF000001FF000001FF000001F
      F800003FF800003FF800003FF800003FFC00007FFC00007FFC00007FFC00007F
      FE0000FFFE0000FFFE0000FFFE0000FFFF8003FFFF8003FFFF8003FFFF8003FF
      FFF01FFFFFF01FFFFFF01FFFFFF01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFC7FFF00FFFFFF00FFFFFF00FFFFFFFFF83FF8003FFFF8003FFFF8003FF
      FFFFFF01FE0000FFFE0000FFFE0000FFFFFFFE01FC00007FFC00007FFC00007F
      FFFFFC01F800003FF800003FF800003FFFFFF803F000001FF000001FF000001F
      FFC07807E000000FE000000FE000000FFF00100FC0000007C0000007C0000007
      FC00000FC0000007C0000007C0000007F800001F800000038000000380000003
      F000007F800000038000000380000003E00000FF800000038000000380000003
      E00000FF000000010000000100000001C000007F000000010000000100000001
      C000007F000000010000000100000001C000003F000000010000000100000001
      8000003F0000000100000001000000018000003F000000010000000100000001
      8000003F0000000100000001000000018000003F800000038000000380000003
      8000003F800000038000000380000003C000003F800000038000000380000003
      C000007FC0000007C0000007C0000007C000007FC0000007C0000007C0000007
      E00000FFE000000FE000000FE000000FE00000FFF000001FF000001FF000001F
      F00001FFF800003FF800003FF800003FF80003FFFC00007FFC00007FFC00007F
      FC0007FFFE0000FFFE0000FFFE0000FFFF001FFFFF8003FFFF8003FFFF8003FF
      FFE07FFFFFF01FFFFFF01FFFFFF01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFC00003FFC00003FFC00003FFFFFFC7FFC00003FFC00003FFC00003FFFFFF83
      FFC00003FFC00003FFC00003FFFFFF01FFC00003C0000003C0000003FFFFFE01
      FFC000038000000380000003FFFFFC01FFC000038000000380000003FFFFF803
      FFC000038000000380000003FFC07807E00000038000000380000003FF00100F
      E00000038000000380000003FC00000FE00000038000000380000003F800001F
      E00000038000000380000003F000007FE00000038000000380000003E00000FF
      E00000038000000380000003E00000FFE00000038000000380000003C000007F
      E00000038000000380000003C000007FE00000038000000380000003C000003F
      E000000380000003800000038000003FE000000380000003800000038000003F
      E000000780000007800000078000003FE000000F8000000F8000000F8000003F
      E000001F8000001F8000001F8000003FE000003F8000003F8000003FC000003F
      E000007F8000007F8000007FC000007FE00007FF8000007F8000007FC000007F
      E00007FF8000007F8000007FE00000FFE00007FF8000007F8000007FE00000FF
      E0000FFF8000007F8000007FF00001FFE0001FFF8000007F8000007FF80003FF
      E0003FFF8000007F8000007FFC0007FFFFFFFFFFFFC0FFFFFFC0FFFFFF001FFF
      FFFFFFFFFFC1FFFFFFC1FFFFFFE07FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE3FFFFFFE3FFFFC00003FFE3FFFFFFFF81FFFFFF81FFFFC00003
      FFC1FFFFFFFF00FFFFFF00FFFFC00003FF81FFFFFFFF08FFFFFF08FFFFC00003
      FF01FFFFFFFE18FFFFFE18FFFFC00003FE03FFFFFFFE38FFFFFE38FFFFC00003
      FE07FFFFFFFE30FFFFFE30FFFFC00003FC0FFFFFFFFE2107FFFE2107E0000003
      F81FFFFFFFFE0003FFFE0003E0000003F03FFFFFFFFF0001FFFF0001E0000003
      F03E0001FFFF8071FFFF8071E0000003E07C0001FFFC00E1FFFC00E1E0000003
      E07E0001FFF000C3FFF000C3E0000003C0FF0001FFC00003FFC00003E0000003
      C0FF8001FF000807FF000807E0000003C1FFC001FE010C1FFE010C1FE0000003
      81FFE001F8070FFFF8070FFFE000000381FFC001F01E0FFFF01E0FFFE0000003
      81FF8001E07E1FFFE07E1FFFE000000780FE0001C3FC1FFFC3FC1FFFE000000F
      807800018FFC3FFF8FFC3FFFE000001F80000001BFF83FFFBFF83FFFE000003F
      80000081FFF87FFFFFF87FFFE000007FC00001C1FFF0FFFFFFF0FFFFE00007FF
      C00007E1FFF0FFFFFFF0FFFFE00007FFE0000FF1FFE1FFFFFFE1FFFFE00007FF
      F0003FFBFFE3FFFFFFE3FFFFE0000FFFFC01FFFFFFC7FFFFFFC7FFFFE0001FFF
      FFFFFFFFFFCFFFFFFFCFFFFFE0003FFFFFFFFFFFFF9FFFFFFF9FFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3FFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFE1FFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFC7FFFFFFC7FFFFE3FFFF
      E0000000FFFF83FFFFFF83FFFFC1FFFFC0000001FFFF81FFFFFF81FFFF81FFFF
      80000003FFFF80FFFFFF80FFFF01FFFF80000003FFFFC07FFFFFC07FFE03FFFF
      80000003FFFFE07FFFFFE07FFE07FFFF80000003FFFFF03FFFFFF03FFC0FFFFF
      80000003FFFFF81FFFFFF81FF81FFFFF80000003FFFFFC0FFFFFFC0FF03FFFFF
      8000000380007C0F80007C0FF03E00018000000380003E0780003E07E07C0001
      8000000380007E0780007E07E07E0001800000038000FF038000FF03C0FF0001
      800000038001FF038001FF03C0FF8001800000038003FF838003FF83C1FFC001
      800000038007FF818007FF8181FFE001800000038003FF818003FF8181FFC001
      800000038000FF818000FF8181FF80018000000380007F0180007F0180FE0001
      8000000380001E0180001E018078000180000003800000018000000180000001
      80000003810000018100000180000081800000038380000383800003C00001C1
      8000000387E0000387E00003C00007E1800000038FF000078FF00007E0000FF1
      80000003DFFC000FDFFC000FF0003FFB80000003FFFF803FFFFF803FFC01FFFF
      80000003FFFFFFFFFFFFFFFFFFFFFFFF80000003FFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3
      FFFFF80FFFFFFFFFFFFFFFFFFFFFFFE1FFFFE003F0000003F0000003F0000000
      FFFFC001E0000003E0000003E0000000FFFF8001C0000003C0000003C0000001
      FF800001800000038000000380000003FE000001800000038000000380000003
      FC000001800000038000000380000003F0000001800000038000000380000003
      E0000001800000038000000380000003E0000001800000038000000380000003
      C000000180000003800000038000000380000001800000038000000380000003
      8000000180000003800000038000000380000001800000038000000380000003
      0000000180000003800000038000000300000007800000038000000380000003
      0000001F8000000380000003800000030000001F800000038000000380000003
      0000001F8000000380000003800000030000001F800000038000000380000003
      0000001F8000000380000003800000038000003F800000038000000380000003
      8000003F8000000380000003800000038000003F800000038000000380000003
      C000007F800000038000000380000003E00000FF800000038000000380000003
      E00000FF800000038000000380000003F00001FF800000038000000380000003
      FC0007FF800000038000000380000003FE000FFF800000038000000380000003
      FFC07FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7C7FFFFFFFFFFFFFFFFFFFFFFFF
      F0000007FFFFFFFFFFFFFFFFFFFFF80FF00000078000000F8000000FFFFFE003
      F00000078000000F8000000FFFFFC001F00000078000000780000007FFFF8001
      F000000F8000000780000007FF800001F00000078000000780000007FE000001
      F00000018000000780000007FC000001F00000008000000380000003F0000001
      F00000008000000380000003E0000001F00000008000000380000003E0000001
      F00000018000000380000003C0000001F000000F800000018000000180000001
      F000000F800000018000000180000001F000000F800000018000000180000001
      F000000F800000018000000100000001F000000F800000008000000000000007
      F000000F80000001800000010000001FF000000F8000001F8000001F0000001F
      F000000F8000001F8000001F0000001FF000000F8000001F8000001F0000001F
      F000000F8000001F8000001F0000001FF000000F8000001F8000001F8000003F
      F000000F8001FFFF8001FFFF8000003FF000000F8001FF818001FF818000003F
      F000000FC003FFC1C003FFC1C000007FF000001FFFFFFFE1FFFFFFE1E00000FF
      F000003FFFFFFFC1FFFFFFC1E00000FFF000007FFFFFFF81FFFFFF81F00001FF
      F00000FFFFFFE60DFFFFE60DFC0007FFF00001FFFFFFF03FFFFFF03FFE000FFF
      FFFFFFFFFFFFFDFFFFFFFDFFFFC07FFF00000000000000000000000000000000
      000000000000}
  end
  object pmtvStructure: TPopupMenu
    OnPopup = pmtvStructurePopup
    Left = 839
    Top = 251
    object mnuStructureInsert: TMenuItem
      Caption = '&Insert'
      OnClick = btnAddRuleClick
    end
    object mnuStructureDelete: TMenuItem
      Caption = 'De&lete'
      OnClick = btnDeleteRuleClick
    end
    object mnuStructureUp: TMenuItem
      Caption = '&Up'
      OnClick = btnRuleUpClick
    end
    object mnuStructureDown: TMenuItem
      Caption = '&Down'
      OnClick = btnRuleDownClick
    end
  end
end
