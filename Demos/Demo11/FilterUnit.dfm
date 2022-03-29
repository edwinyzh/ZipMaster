object FilterForm: TFilterForm
  Left = 192
  Top = 114
  Caption = 'File Filter'
  ClientHeight = 179
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblCustom: TLabel
    Left = 168
    Top = 35
    Width = 60
    Height = 13
    Caption = 'Custom filter:'
  end
  object FiltersBox: TCheckListBox
    Left = 0
    Top = 19
    Width = 153
    Height = 160
    OnClickCheck = FiltersBoxClickCheck
    Align = alLeft
    ItemHeight = 13
    Items.Strings = (
      '')
    TabOrder = 0
  end
  object edCustom: TEdit
    Left = 234
    Top = 32
    Width = 162
    Height = 21
    TabOrder = 1
    OnChange = edCustomChange
  end
  object cbIncGlobal: TCheckBox
    Left = 159
    Top = 86
    Width = 129
    Height = 17
    Caption = 'Include global excludes'
    TabOrder = 2
    OnClick = cbIncGlobalClick
  end
  object edFilter: TEdit
    Left = 159
    Top = 59
    Width = 242
    Height = 21
    AutoSelect = False
    ReadOnly = True
    TabOrder = 3
    OnChange = edFilterChange
  end
  object OkBtn: TButton
    Left = 185
    Top = 146
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelBtn: TButton
    Left = 304
    Top = 146
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object TabSet1: TTabSet
    Left = 0
    Top = 0
    Width = 411
    Height = 19
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Tabs.Strings = (
      'Select'
      'Exclude')
    TabIndex = 0
    OnChange = TabSet1Change
  end
  object cbIgnoreGlobal: TCheckBox
    Left = 160
    Top = 112
    Width = 121
    Height = 17
    Caption = 'Ignore global excludes'
    TabOrder = 7
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 320
    Top = 96
  end
end
