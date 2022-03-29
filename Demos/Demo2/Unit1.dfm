object Form1: TForm1
  Left = 565
  Top = 419
  Caption = 'Zip Demo 2'
  ClientHeight = 261
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ZipBut: TButton
    Left = 144
    Top = 20
    Width = 105
    Height = 25
    Caption = '&Zip TEST.DAT'
    TabOrder = 0
    OnClick = ZipButClick
  end
  object UnzipBut: TButton
    Left = 144
    Top = 64
    Width = 105
    Height = 25
    Caption = '&Unzip TEST.ZIP'
    TabOrder = 1
    OnClick = UnzipButClick
  end
  object ExitBut: TButton
    Left = 164
    Top = 216
    Width = 67
    Height = 25
    Caption = '&Exit'
    TabOrder = 2
    OnClick = ExitButClick
  end
  object DelBut: TButton
    Left = 144
    Top = 108
    Width = 105
    Height = 25
    Caption = '&Delete from Zip'
    TabOrder = 3
    OnClick = DelButClick
  end
  object VersionBut: TButton
    Left = 144
    Top = 156
    Width = 105
    Height = 25
    Caption = '&Get DLL Version'
    TabOrder = 4
    OnClick = VersionButClick
  end
  object ZipMaster1: TZipMaster
    ConfirmErase = False
    DLLDirectory = '..\..\dll\'
    Version = '1.9.2.0013'
    Left = 16
    Top = 24
  end
end
