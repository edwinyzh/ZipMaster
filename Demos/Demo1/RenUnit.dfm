object RenForm: TRenForm
  Left = 218
  Top = 248
  Caption = 'Rename items in zip archive'
  ClientHeight = 270
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 17
    Top = 3
    Width = 99
    Height = 13
    Caption = '&Selected for change:'
  end
  object OkBitBtn: TBitBtn
    Left = 45
    Top = 248
    Width = 85
    Height = 25
    Caption = '&Ok Start'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Kind = bkOK
    NumGlyphs = 2
    ParentFont = False
    TabOrder = 3
    OnClick = OkBitBtnClick
  end
  object CancelBitBtn: TBitBtn
    Left = 320
    Top = 248
    Width = 85
    Height = 25
    Caption = '&Cancel'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 2
    OnClick = CancelBitBtnClick
  end
  object Panel1: TPanel
    Left = 16
    Top = 136
    Width = 417
    Height = 105
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 140
      Top = 3
      Width = 138
      Height = 13
      Caption = 'Choose Old &File Specification'
    end
    object Label2: TLabel
      Left = 96
      Top = 39
      Width = 132
      Height = 13
      Caption = 'Type &New File Specification'
    end
    object Label4: TLabel
      Left = 316
      Top = 39
      Width = 76
      Height = 13
      Caption = '&Date/time spec.'
    end
    object OldCombo: TComboBox
      Left = 8
      Top = 16
      Width = 401
      Height = 21
      DropDownCount = 20
      TabOrder = 0
      OnClick = OldComboClick
    end
    object NewEdit: TEdit
      Left = 8
      Top = 52
      Width = 297
      Height = 21
      AutoSelect = False
      AutoSize = False
      TabOrder = 1
    end
    object AddBtn: TButton
      Left = 168
      Top = 78
      Width = 75
      Height = 21
      Caption = '&Add'
      TabOrder = 2
      OnClick = AddBtnClick
    end
    object DTEdit: TEdit
      Left = 308
      Top = 52
      Width = 101
      Height = 21
      TabOrder = 3
    end
    object DTAllBtn: TButton
      Left = 316
      Top = 79
      Width = 87
      Height = 21
      Hint = 'Set the date/time for all files; even when not selected!!!'
      Caption = '&Use for all files'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = DTAllBtnClick
    end
  end
  object RemoveBtn: TButton
    Left = 308
    Top = 112
    Width = 125
    Height = 21
    Caption = '&Remove selected'
    ModalResult = 1
    TabOrder = 0
    OnClick = RemoveBtnClick
  end
end
