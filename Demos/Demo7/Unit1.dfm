object Form1: TForm1
  Left = 236
  Top = 109
  Caption = 'Form1'
  ClientHeight = 186
  ClientWidth = 223
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 32
    Width = 209
    Height = 121
    Stretch = True
  end
  object StaticText1: TStaticText
    Left = 8
    Top = 8
    Width = 4
    Height = 4
    TabOrder = 0
  end
  object BtnPic1: TButton
    Left = 8
    Top = 160
    Width = 65
    Height = 25
    Caption = 'Pic &1'
    TabOrder = 1
    OnClick = BtnPic1Click
  end
  object BtnPic2: TButton
    Left = 80
    Top = 160
    Width = 65
    Height = 25
    Caption = 'Pic &2'
    TabOrder = 2
    OnClick = BtnPic2Click
  end
  object BtnPic3: TButton
    Left = 152
    Top = 160
    Width = 65
    Height = 25
    Caption = 'Pic &3'
    TabOrder = 3
    OnClick = BtnPic3Click
  end
  object ZipMaster1: TZipMaster
    AddOptions = []
    AddStoreSuffixes = [assGIF, assPNG, assZ, assZIP, assZOO, assARC, assLZH, assARJ, assTAZ, assTGZ, assLHA, assRAR, assACE, assCAB, assGZ, assGZIP, assJAR, assJPG, assJPEG, ass7Zp, assMP3, assWMV, assWMA, assDVR, assAVI]
    ConfirmErase = False
    DLLDirectory = '..\..\dll'
    DLL_Load = False
    ExtrOptions = []
    KeepFreeOnAllDisks = 0
    KeepFreeOnDisk1 = 0
    MaxVolumeSize = 0
    MaxVolumeSizeKb = 0
    SFXOptions = []
    SFXOverwriteMode = ovrAlways
    SpanOptions = []
    Trace = False
    Unattended = False
    Verbose = False
    Version = '1.9.1.03'
    WriteOptions = []
    Left = 104
  end
end
