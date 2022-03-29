object Msgform: TMsgform
  Left = 214
  Top = 189
  Width = 555
  Height = 423
  Caption = 'Zip Messages'
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
    Width = 547
    Height = 25
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
      Position = 1
      Step = 1
      TabOrder = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 370
    Width = 547
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
    Top = 345
    Width = 547
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
  object PageControl1: TPageControl
    Left = 0
    Top = 25
    Width = 547
    Height = 320
    ActivePage = Script
    Align = alClient
    TabOrder = 3
    object MsgSheet: TTabSheet
      Caption = 'Messages'
      object MsgMemo: TMemo
        Left = 0
        Top = 0
        Width = 539
        Height = 291
        Align = alClient
        Color = clInfoBk
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object IncSheet: TTabSheet
      Caption = 'Included files'
      ImageIndex = 1
      object IncMemo: TMemo
        Left = 0
        Top = 0
        Width = 539
        Height = 291
        Align = alClient
        Color = clInfoBk
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object ExcSheet: TTabSheet
      Caption = 'Excluded files'
      ImageIndex = 2
      object ExcMemo: TMemo
        Left = 0
        Top = 0
        Width = 539
        Height = 291
        Align = alClient
        Color = clInfoBk
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object Script: TTabSheet
      Caption = 'Script'
      ImageIndex = 3
      object ScriptMemo: TMemo
        Left = 0
        Top = 0
        Width = 539
        Height = 291
        Align = alClient
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
end
