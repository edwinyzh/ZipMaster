object Form1: TForm1
  Left = 472
  Top = 341
  Width = 625
  Height = 433
  Caption = 'ZIP Demo 3'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 609
    Height = 222
    Align = alTop
    BevelInner = bvRaised
    TabOrder = 0
    object Label1: TLabel
      Left = 128
      Top = 50
      Width = 88
      Height = 16
      Caption = 'ZIP File Name:'
    end
    object Label2: TLabel
      Left = 130
      Top = 85
      Width = 70
      Height = 16
      Caption = 'File Spec 1:'
    end
    object Label3: TLabel
      Left = 130
      Top = 119
      Width = 70
      Height = 16
      Caption = 'File Spec 2:'
    end
    object ZipFName: TLabel
      Left = 233
      Top = 49
      Width = 56
      Height = 16
      Caption = '< none >'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RadioTraceOpt: TRadioGroup
      Left = 163
      Top = 150
      Width = 98
      Height = 60
      Caption = 'Trace DLL'
      Columns = 2
      Items.Strings = (
        'No'
        'Yes')
      TabOrder = 0
    end
    object RadioVerboseOpt: TRadioGroup
      Left = 19
      Top = 150
      Width = 102
      Height = 60
      Caption = 'Verbose Status'
      Columns = 2
      Items.Strings = (
        'No'
        'Yes')
      TabOrder = 1
    end
    object Edit1: TEdit
      Left = 208
      Top = 79
      Width = 385
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object Edit2: TEdit
      Left = 208
      Top = 116
      Width = 385
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object RadioRecurse: TRadioGroup
      Left = 454
      Top = 150
      Width = 131
      Height = 60
      Caption = 'Recurse Directories'
      Columns = 2
      Items.Strings = (
        'No'
        'Yes')
      TabOrder = 4
    end
    object NewBut: TButton
      Left = 17
      Top = 55
      Width = 81
      Height = 25
      Caption = '&New ZIP'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = NewButClick
    end
    object OpenBut: TButton
      Left = 18
      Top = 12
      Width = 80
      Height = 25
      Caption = '&Open ZIP'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = OpenButClick
    end
    object VersionBut: TButton
      Left = 16
      Top = 100
      Width = 81
      Height = 26
      Caption = 'DLL &Version'
      TabOrder = 7
      OnClick = VersionButClick
    end
    object ListBut: TButton
      Left = 128
      Top = 12
      Width = 62
      Height = 25
      Caption = '&List'
      TabOrder = 8
      OnClick = ListButClick
    end
    object AddBut: TButton
      Left = 208
      Top = 12
      Width = 62
      Height = 25
      Caption = '&Add'
      TabOrder = 9
      OnClick = AddButClick
    end
    object DeleteBut: TButton
      Left = 288
      Top = 12
      Width = 62
      Height = 25
      Caption = '&Delete'
      TabOrder = 10
      OnClick = DeleteButClick
    end
    object ExtractBut: TButton
      Left = 368
      Top = 12
      Width = 62
      Height = 25
      Caption = '&Extract'
      TabOrder = 11
      OnClick = ExtractButClick
    end
    object AbortBut: TButton
      Left = 484
      Top = 12
      Width = 53
      Height = 25
      Caption = 'A&bort'
      TabOrder = 12
      OnClick = AbortButClick
    end
    object ExitBut: TButton
      Left = 553
      Top = 12
      Width = 53
      Height = 25
      Caption = 'E&xit'
      TabOrder = 13
      OnClick = ExitButClick
    end
    object RadioDirNames: TRadioGroup
      Left = 308
      Top = 149
      Width = 108
      Height = 61
      Caption = 'Save Dir Names'
      Columns = 2
      Items.Strings = (
        'No'
        'Yes')
      TabOrder = 14
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 222
    Width = 609
    Height = 173
    Align = alClient
    BevelInner = bvRaised
    TabOrder = 1
    object Panel3: TPanel
      Left = 2
      Top = 46
      Width = 605
      Height = 125
      Align = alClient
      TabOrder = 0
      object RichEdit1: TRichEdit
        Left = 1
        Top = 1
        Width = 603
        Height = 123
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object Panel4: TPanel
      Left = 2
      Top = 2
      Width = 605
      Height = 44
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      TabOrder = 1
      object FileBeingZipped: TLabel
        Left = 246
        Top = 12
        Width = 106
        Height = 16
        Alignment = taRightJustify
        Caption = 'File Being Zipped'
      end
      object ProgressBar1: TProgressBar
        Left = 361
        Top = 12
        Width = 232
        Height = 16
        TabOrder = 0
      end
    end
  end
  object OpenDialog: TOpenDialog
    Title = 'Open Existing ZIP file'
    Left = 106
    Top = 229
  end
  object ZipMaster1: TZipMaster
    AddOptions = []
    AddStoreSuffixes = [AssGIF, AssPNG, AssZ, AssZIP, AssZOO, AssARC, AssLZH, AssARJ, AssTAZ, AssTGZ, AssLHA, AssRAR, AssACE, AssCAB, AssGZ, AssGZIP, AssJAR, AssJPG, AssJPEG, Ass7Zp, AssMP3, AssWMV, AssWMA, AssDVR, AssAVI]
    ConfirmErase = False
    DLLDirectory = '..\..\dll'
    DLL_Load = False
    ExtrOptions = []
    KeepFreeOnAllDisks = 0
    KeepFreeOnDisk1 = 0
    MaxVolumeSizeKb = 0
    NoReadAux = False
    SFXOptions = []
    SFXOverwriteMode = OvrAlways
    SpanOptions = []
    Trace = False
    Unattended = False
    UseUTF8 = False
    Verbose = False
    Version = '1.9.2.0005'
    WriteOptions = []
    Left = 32
    Top = 232
  end
end
