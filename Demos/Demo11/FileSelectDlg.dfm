object FileSelDialog: TFileSelDialog
  Tag = 15000
  Left = 253
  Top = 160
  HelpContext = 4000
  BorderStyle = bsDialog
  Caption = 'Backup2Floppy - File select'
  ClientHeight = 179
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DLabel2: TLabel
    Left = 201
    Top = 4
    Width = 30
    Height = 13
    Caption = 'Filters:'
  end
  object DLabel5: TLabel
    Left = 24
    Top = 70
    Width = 30
    Height = 13
    Caption = 'File(s):'
  end
  object DLabel6: TLabel
    Left = 15
    Top = 118
    Width = 141
    Height = 13
    Caption = 'Filters also work for subfolders'
  end
  object FilterComboBox: TFilterComboBox
    Left = 201
    Top = 18
    Width = 235
    Height = 21
    HelpContext = 4000
    Filter = 
      'All files (*.*)|*.*|BCB Project files(*.bpg;*.bpr;*.bpk)|*.bpg;*' +
      '.bpr;*.bpk|BCB Sources(*.cpp;*.hpp;*.c;*.h;*.dfm)|*.cpp;*.hpp;*.' +
      'c;*.h;*.dfm|Compiled(*.obj;*.dll;*.exe;*.hlp;*.cgl)|*.obj;*.dll;' +
      '*.exe;*.hlp;*.cgl|Resource(*.rc;*.res;*.bmp;*.ico)|*.rc;*.res;*.' +
      'bmp;*.ico|Linker files(*.il*;*.tds)|*.il*;*.tds|Back-upFiles(*.b' +
      'ak;*.~*)|*.bak;*.~*|Text files(*.txt)|*.txt|Docs en RTF(*.doc;*.' +
      'rtf)|*.doc;*.rtf|Help Construct(*.cnt;*.hpj;*.hhp;*.hhc;*.hhk)|*' +
      '.cnt;*.hpj;*.hhc;*.hhk;*.hhp|PCH-files(*.csm;*.#*;*.dcu)|*.csm;*' +
      '.#*;*.dcu'
    TabOrder = 0
    OnChange = SelectionChange
  end
  object Edit: TEdit
    Left = 15
    Top = 86
    Width = 418
    Height = 21
    HelpContext = 4000
    TabOrder = 1
  end
  object CheckBox: TCheckBox
    Left = 15
    Top = 51
    Width = 137
    Height = 17
    Caption = 'Include Sub-directories'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = SelectionChange
  end
  object OKBtn: TBitBtn
    Left = 91
    Top = 136
    Width = 75
    Height = 25
    Caption = 'ok'
    DoubleBuffered = True
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object CancelBtn: TBitBtn
    Left = 270
    Top = 136
    Width = 83
    Height = 25
    Caption = '&Cancel'
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
  end
  object BrowseBtn: TButton
    Left = 15
    Top = 16
    Width = 150
    Height = 25
    Caption = 'BrowseBtn'
    TabOrder = 5
    OnClick = BrowseBtnClick
  end
end
