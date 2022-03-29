object Msgform: TMsgform
  Left = 198
  Top = 193
  Caption = 'Zip Messages'
  ClientHeight = 391
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 347
    Width = 552
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object DismissBut: TButton
      Left = 374
      Top = 4
      Width = 118
      Height = 20
      Caption = '&Dismiss this Window'
      TabOrder = 1
      OnClick = DismissButClick
    end
    object CancelBut: TButton
      Left = 192
      Top = 4
      Width = 104
      Height = 20
      Cancel = True
      Caption = '&Cancel Operation'
      TabOrder = 0
      OnClick = CancelButClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 552
    Height = 29
    Align = alTop
    TabOrder = 1
    object FileBeingZipped: TLabel
      Left = 259
      Top = 8
      Width = 105
      Height = 13
      Alignment = taRightJustify
      Caption = 'File being manipulated'
    end
    object ProgressBar1: TProgressBar
      Left = 376
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
    Top = 372
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
  object Memo1: TMemo
    Left = 0
    Top = 29
    Width = 552
    Height = 318
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
end
