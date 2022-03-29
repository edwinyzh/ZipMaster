object Mainform: TMainform
  Left = 146
  Top = 172
  Caption = 'Create Self-Extracting Archive (*.EXE)'
  ClientHeight = 401
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 614
    Height = 101
    Align = alTop
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 160
      Height = 99
      Align = alLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object DeleteZipBut: TButton
        Left = 12
        Top = 55
        Width = 137
        Height = 26
        Caption = 'Destroy Current Archive'
        TabOrder = 1
        OnClick = DeleteZipButClick
      end
      object NewZipBut: TButton
        Left = 12
        Top = 12
        Width = 137
        Height = 26
        Caption = '&Create New Archive'
        TabOrder = 0
        OnClick = NewZipButClick
      end
    end
    object Panel1: TPanel
      Left = 510
      Top = 1
      Width = 103
      Height = 99
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Bevel2: TBevel
        Left = 9
        Top = 60
        Width = 85
        Height = 33
      end
      object Label1: TLabel
        Left = 56
        Top = 70
        Width = 23
        Height = 15
        Caption = 'files'
      end
      object FilesLabel: TLabel
        Left = 28
        Top = 70
        Width = 7
        Height = 15
        Caption = '0'
      end
      object CloseBut: TButton
        Left = 9
        Top = 16
        Width = 85
        Height = 27
        Caption = 'E&xit'
        TabOrder = 0
        OnClick = CloseButClick
      end
    end
    object Panel4: TPanel
      Left = 161
      Top = 1
      Width = 349
      Height = 99
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object ZipFName: TLabel
        Left = 40
        Top = 70
        Width = 309
        Height = 15
        AutoSize = False
        Caption = '<none>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 6
        Top = 70
        Width = 30
        Height = 14
        Caption = 'Name:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Bevel1: TBevel
        Left = 16
        Top = 9
        Width = 329
        Height = 48
      end
      object DeleteBut: TButton
        Left = 210
        Top = 20
        Width = 85
        Height = 24
        Caption = '&Delete Files'
        TabOrder = 1
        OnClick = DeleteButClick
      end
      object AddBut: TButton
        Left = 62
        Top = 21
        Width = 79
        Height = 24
        Caption = '&Add Files'
        TabOrder = 0
        OnClick = AddButClick
      end
    end
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 101
    Width = 614
    Height = 300
    Align = alClient
    ColCount = 4
    DefaultColWidth = 90
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goColMoving, goRowSelect]
    TabOrder = 1
  end
  object ZipMaster1: TZipMaster
    AddOptions = []
    AddStoreSuffixes = [assGIF, assPNG, assZ, assZIP, assZOO, assARC, assLZH, assARJ, assTAZ, assTGZ, assLHA, assRAR, assACE, assCAB, assGZ, assGZIP, assJAR]
    ConfirmErase = False
    DLLDirectory = '..\..\dll'
    DLL_Load = False
    ExtrOptions = []
    KeepFreeOnAllDisks = 0
    KeepFreeOnDisk1 = 0
    Language = 'US: Default'
    MaxVolumeSize = 0
    MaxVolumeSizeKb = 0
    NoReadAux = False
    OnDirUpdate = ZipMaster1DirUpdate
    OnMessage = ZipMaster1Message
    SFXCaption = 'Self Extracting Archive'
    SFXDefaultDir = 'e:\temp\sfx1\'
    SFXOptions = []
    SFXOverwriteMode = ovrAlways
    SpanOptions = []
    Trace = False
    Unattended = False
    Verbose = False
    Version = '1.9.2.0003'
    WriteOptions = []
    Left = 482
    Top = 64
  end
  object OpenDialog1: TOpenDialog
    Left = 448
    Top = 64
  end
end
