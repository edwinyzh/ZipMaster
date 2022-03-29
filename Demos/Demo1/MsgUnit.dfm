object Msgform: TMsgform
  Left = 214
  Top = 189
  Caption = 'Zip Messages'
  ClientHeight = 393
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 552
    Height = 29
    Align = alTop
    TabOrder = 0
    object FileBeingZipped: TLabel
      Left = 252
      Top = 8
      Width = 105
      Height = 14
      Alignment = taRightJustify
      Caption = 'File being manipulated'
    end
    object ProgressBar1: TProgressBar
      Left = 364
      Top = 8
      Width = 177
      Height = 13
      Min = 1
      Max = 10001
      Position = 1
      Step = 100
      TabOrder = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 374
    Width = 552
    Height = 19
    Panels = <
      item
        Bevel = pbNone
        Width = 110
      end
      item
        Bevel = pbNone
        Width = 80
      end
      item
        Style = psOwnerDraw
        Width = 150
      end>
  end
  object Panel2: TPanel
    Left = 0
    Top = 349
    Width = 552
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 364
      Top = 4
      Width = 132
      Height = 20
      Caption = '&Dismiss this Window'
      TabOrder = 0
      OnClick = DismissButClick
    end
    object Button2: TButton
      Left = 192
      Top = 4
      Width = 104
      Height = 20
      Cancel = True
      Caption = '&Cancel Operation'
      TabOrder = 1
      OnClick = CancelButClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 29
    Width = 552
    Height = 320
    Align = alClient
    Color = clInfoBk
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
end
