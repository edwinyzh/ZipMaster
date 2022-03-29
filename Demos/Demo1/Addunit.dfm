object AddForm: TAddForm
  Left = 315
  Top = 175
  Width = 604
  Height = 436
  AutoSize = True
  Caption = 'Add Files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 173
    Top = 0
    Width = 423
    Height = 402
    Align = alClient
    TabOrder = 0
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 104
      Height = 400
      Align = alLeft
      TabOrder = 0
      object Bevel1: TBevel
        Left = 3
        Top = 24
        Width = 99
        Height = 209
      end
      object Label2: TLabel
        Left = 28
        Top = 6
        Width = 44
        Height = 13
        Caption = 'Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object AddFileBut: TButton
        Left = 11
        Top = 242
        Width = 84
        Height = 20
        Caption = '&Add File ==>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = AddFileButClick
      end
      object RemoveBut: TButton
        Left = 11
        Top = 301
        Width = 84
        Height = 21
        Caption = '&Remove <=='
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = RemoveButClick
      end
      object OKBut: TButton
        Left = 11
        Top = 335
        Width = 83
        Height = 20
        Caption = 'DO IT &NOW'
        Default = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = OKButClick
      end
      object CancelBut: TButton
        Left = 11
        Top = 384
        Width = 83
        Height = 20
        Cancel = True
        Caption = '&Cancel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = CancelButClick
      end
      object DirNameCB: TCheckBox
        Left = 6
        Top = 27
        Width = 93
        Height = 17
        Caption = 'Save DirNames'
        TabOrder = 4
      end
      object RecurseCB: TCheckBox
        Left = 6
        Top = 43
        Width = 83
        Height = 17
        Caption = 'Recurse Dirs'
        TabOrder = 5
      end
      object AddDirBut: TButton
        Left = 11
        Top = 271
        Width = 85
        Height = 21
        Caption = 'Add &Dir  ==>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        OnClick = AddDirButClick
      end
      object EncryptCB: TCheckBox
        Left = 6
        Top = 59
        Width = 83
        Height = 17
        Caption = 'Encrypt Files'
        TabOrder = 7
      end
      object DiskSpanCB: TCheckBox
        Left = 6
        Top = 115
        Width = 83
        Height = 17
        Caption = 'Span disk'
        TabOrder = 8
        OnClick = DiskSpanCBClick
      end
      object FreeDisk1But: TButton
        Left = 6
        Top = 184
        Width = 93
        Height = 21
        Caption = 'Set free on disk 1'
        Enabled = False
        TabOrder = 9
        OnClick = FreeDiskAllButClick
      end
      object VolSizeBut: TButton
        Left = 6
        Top = 160
        Width = 93
        Height = 21
        Caption = 'Set max. vol. size'
        Enabled = False
        TabOrder = 10
        OnClick = VolSizeButClick
      end
      object AddBtn: TButton
        Left = 6
        Top = 136
        Width = 93
        Height = 21
        Caption = 'Action: Add'
        TabOrder = 11
        OnClick = AddBtnClick
      end
      object AtribOnlyCB: TCheckBox
        Left = 6
        Top = 72
        Width = 81
        Height = 17
        Caption = 'Arch only'
        TabOrder = 12
      end
      object AtribResetCB: TCheckBox
        Left = 6
        Top = 88
        Width = 81
        Height = 17
        Caption = 'Reset Archive'
        TabOrder = 13
      end
      object FreeDiskAllBut: TButton
        Left = 6
        Top = 208
        Width = 93
        Height = 21
        Caption = 'Set free all disks'
        Enabled = False
        TabOrder = 14
        OnClick = FreeDiskAllButClick
      end
    end
    object Panel6: TPanel
      Left = 105
      Top = 1
      Width = 317
      Height = 400
      Align = alClient
      TabOrder = 1
      object SelectedList: TListBox
        Left = 1
        Top = 27
        Width = 315
        Height = 372
        TabStop = False
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
      end
      object Panel7: TPanel
        Left = 1
        Top = 1
        Width = 315
        Height = 26
        Align = alTop
        TabOrder = 1
        object Label1: TLabel
          Left = 53
          Top = 7
          Width = 72
          Height = 13
          Caption = 'Files to Add:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object SortBut: TButton
          Left = 150
          Top = 6
          Width = 46
          Height = 16
          Caption = '&Sort'
          TabOrder = 0
          OnClick = SortButClick
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 173
    Height = 402
    Align = alLeft
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 171
      Height = 23
      Align = alTop
      TabOrder = 0
      object DriveComboBox1: TDriveComboBox
        Left = 3
        Top = 3
        Width = 157
        Height = 19
        DirList = DirectoryListBox1
        TabOrder = 0
      end
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 195
      Width = 171
      Height = 206
      Align = alClient
      ItemHeight = 16
      MultiSelect = True
      ShowGlyphs = True
      TabOrder = 1
    end
    object Panel4: TPanel
      Left = 1
      Top = 24
      Width = 171
      Height = 145
      Align = alTop
      TabOrder = 2
      object DirectoryListBox1: TDirectoryListBox
        Left = 1
        Top = 1
        Width = 169
        Height = 143
        Align = alClient
        FileList = FileListBox1
        ItemHeight = 16
        TabOrder = 0
      end
    end
    object Panel8: TPanel
      Left = 1
      Top = 169
      Width = 171
      Height = 26
      Align = alTop
      TabOrder = 3
      object SelectAllBut: TButton
        Left = 43
        Top = 4
        Width = 81
        Height = 17
        Caption = 'Select All Files'
        TabOrder = 0
        OnClick = SelectAllButClick
      end
    end
  end
  object PopupMenu1: TPopupMenu
    AutoPopup = False
    Left = 238
    Top = 377
    object Add1: TMenuItem
      Tag = 1
      Caption = 'Add'
      Checked = True
      Default = True
      RadioItem = True
      OnClick = Add1Click
    end
    object Update1: TMenuItem
      Tag = 2
      Caption = 'Update'
      RadioItem = True
      OnClick = Add1Click
    end
    object Freshen1: TMenuItem
      Tag = 3
      Caption = 'Freshen'
      RadioItem = True
      OnClick = Add1Click
    end
    object Move1: TMenuItem
      Tag = 4
      Caption = 'Move'
      RadioItem = True
      OnClick = Add1Click
    end
  end
end
