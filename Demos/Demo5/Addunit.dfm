object AddForm: TAddForm
  Left = 172
  Top = 234
  Width = 612
  Height = 429
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
    Width = 431
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
      object AddFileBut: TButton
        Left = 8
        Top = 130
        Width = 84
        Height = 20
        Caption = '&Add File ==>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = AddFileButClick
      end
      object RemoveBut: TButton
        Left = 8
        Top = 188
        Width = 84
        Height = 21
        Caption = '&Remove <=='
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = RemoveButClick
      end
      object OKBut: TButton
        Left = 9
        Top = 267
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
        TabOrder = 3
        OnClick = OKButClick
      end
      object CancelBut: TButton
        Left = 9
        Top = 304
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
        TabOrder = 4
        OnClick = CancelButClick
      end
      object AddDirBut: TButton
        Left = 8
        Top = 96
        Width = 85
        Height = 21
        Caption = 'Add &Dir  ==>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = AddDirButClick
      end
    end
    object Panel6: TPanel
      Left = 105
      Top = 1
      Width = 325
      Height = 400
      Align = alClient
      TabOrder = 1
      object SelectedList: TListBox
        Left = 1
        Top = 27
        Width = 323
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
        Width = 323
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
      Top = 147
      Width = 171
      Height = 254
      Align = alClient
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 1
    end
    object Panel4: TPanel
      Left = 1
      Top = 24
      Width = 171
      Height = 97
      Align = alTop
      TabOrder = 2
      object DirectoryListBox1: TDirectoryListBox
        Left = 1
        Top = 1
        Width = 169
        Height = 95
        Align = alClient
        FileList = FileListBox1
        ItemHeight = 16
        TabOrder = 0
      end
    end
    object Panel8: TPanel
      Left = 1
      Top = 121
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
end
