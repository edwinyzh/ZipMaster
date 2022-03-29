object InstForm: TInstForm
  Left = 276
  Top = 263
  Width = 411
  Height = 281
  Caption = 'Install/Uninstall'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 12
    Top = 209
    Width = 75
    Height = 13
    Caption = 'This Program is:'
  end
  object ProgramNameLabel: TLabel
    Left = 96
    Top = 209
    Width = 148
    Height = 13
    Caption = '<program path and name>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 224
    Top = 184
    Width = 163
    Height = 13
    Caption = 'Un-install does not delete any files.'
  end
  object Label1: TLabel
    Left = 28
    Top = 4
    Width = 343
    Height = 13
    Caption = 'Warning: This is a preliminary test program!  No Guarantees!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 20
    Top = 24
    Width = 197
    Height = 173
    Caption = 'Install/Un-install Options'
    TabOrder = 4
    object AssocCB: TCheckBox
      Left = 22
      Top = 126
      Width = 97
      Height = 17
      Caption = '.zip Association'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object SendToCB: TCheckBox
      Left = 22
      Top = 84
      Width = 137
      Height = 17
      Caption = 'Explorer "Send To" link'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object KillCB: TCheckBox
      Left = 22
      Top = 146
      Width = 160
      Height = 22
      Caption = 'Remove EXE after install'
      TabOrder = 2
    end
    object StartMenuRB: TRadioButton
      Left = 40
      Top = 31
      Width = 81
      Height = 17
      Caption = 'Start Menu'
      TabOrder = 3
    end
    object ProgramRB: TRadioButton
      Left = 40
      Top = 46
      Width = 113
      Height = 17
      Caption = 'Programs menu'
      Checked = True
      TabOrder = 4
      TabStop = True
    end
  end
  object InstBut: TButton
    Left = 296
    Top = 28
    Width = 75
    Height = 25
    Caption = '&Install'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = InstButClick
  end
  object StartMenuCB: TCheckBox
    Left = 42
    Top = 38
    Width = 81
    Height = 17
    Caption = 'Menu Icon'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = StartMenuCBClick
  end
  object DesktopCB: TCheckBox
    Left = 42
    Top = 88
    Width = 113
    Height = 17
    Caption = 'Desktop Icon'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object RegistryCB: TCheckBox
    Left = 42
    Top = 129
    Width = 153
    Height = 17
    Caption = 'Application Registry Entries'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object UninstBut: TButton
    Left = 296
    Top = 68
    Width = 75
    Height = 25
    Caption = '&Un-install'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = UninstButClick
  end
  object CancelBut: TButton
    Left = 296
    Top = 108
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 6
    OnClick = CancelButClick
  end
end
