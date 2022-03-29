object MakeSFX: TMakeSFX
  Left = 196
  Top = 201
  Width = 590
  Height = 400
  Caption = 'Make Self Extracting Archive'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 582
    Height = 108
    Align = alTop
    TabOrder = 0
    object CmdLineCB: TCheckBox
      Left = 177
      Top = 12
      Width = 272
      Height = 13
      Caption = 'Allow User to avoid running the command line'
      TabOrder = 1
    end
    object FileListCB: TCheckBox
      Left = 177
      Top = 31
      Width = 272
      Height = 13
      Caption = 'Allow User to modify the file list to be extracted'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object HideOverwriteCB: TCheckBox
      Left = 177
      Top = 50
      Width = 272
      Height = 13
      Caption = 'Hide the Overwrite Action group box at run time'
      TabOrder = 3
    end
    object DfltOverWriteGrp: TRadioGroup
      Left = 25
      Top = 7
      Width = 132
      Height = 95
      Caption = 'Default Overwrite Action'
      ItemIndex = 0
      Items.Strings = (
        'Ask User'
        'Always Overwrite'
        'Skip extraction')
      TabOrder = 0
    end
    object ExecBut: TButton
      Left = 475
      Top = 20
      Width = 86
      Height = 21
      Caption = 'Do It Now'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = ExecButClick
    end
    object CancelBut: TButton
      Left = 475
      Top = 60
      Width = 86
      Height = 20
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 7
      OnClick = CancelButClick
    end
    object AutoRunCB: TCheckBox
      Left = 177
      Top = 69
      Width = 272
      Height = 13
      Hint = 'Filename must start with an '#39'!'#39
      Caption = 'Create an AutoRun SFX'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = AutoRunCBClick
    end
    object NoMsgShowCB: TCheckBox
      Left = 177
      Top = 88
      Width = 272
      Height = 13
      Caption = 'Do Not show the success Msg after file extraction'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 108
    Width = 582
    Height = 258
    Align = alClient
    TabOrder = 1
    object SFXPage: TPageControl
      Left = 1
      Top = 12
      Width = 580
      Height = 257
      ActivePage = TabSheet1
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Startup Message'
        object Label9: TLabel
          Left = 29
          Top = 171
          Width = 153
          Height = 13
          Caption = 'Message to show before starting'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object MsgEdit: TEdit
          Left = 25
          Top = 186
          Width = 524
          Height = 21
          MaxLength = 255
          TabOrder = 2
        end
        object Memo3: TMemo
          Left = 79
          Top = 20
          Width = 422
          Height = 57
          TabStop = False
          BorderStyle = bsNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'Here you can enter a message that will be displayed before the '
            'main extraction dialog.'
            'You can also specifiy the type of the message box:'
            ''
            
              '- Confirmation : the messagebox shows the buttons '#39'Yes'#39' and '#39'No'#39 +
              '. if '
            'the User presses '#39'No'#39', sfx will stop.'
            
              '- Information : the messagebox shows the buttons '#39'OK'#39' and '#39'Cance' +
              'l'#39'. if '
            'the User presses '#39'Cancel'#39', sfx will stop.'
            
              '- Standard : the messagebox shows an '#39'OK'#39' button. The User canno' +
              't '
            'cancel starting the SFX application.')
          ParentColor = True
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object GroupBox1: TGroupBox
          Left = 24
          Top = 88
          Width = 525
          Height = 57
          Caption = 'Message Type:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object OkCancelRB: TRadioButton
            Left = 156
            Top = 24
            Width = 181
            Height = 13
            Caption = 'Information, Ok, Cancel buttons'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object YesNoRB: TRadioButton
            Left = 344
            Top = 24
            Width = 173
            Height = 13
            Caption = 'Confirmation, Yes, No buttons'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
          object OkBttnRB: TRadioButton
            Left = 21
            Top = 24
            Width = 124
            Height = 13
            Caption = 'Standard, Ok button'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            TabStop = True
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Caption'
        object Label1: TLabel
          Left = 133
          Top = 115
          Width = 271
          Height = 16
          Caption = 'Caption of the Self Extracting Archive Program'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object CaptionEdit: TEdit
          Left = 25
          Top = 186
          Width = 524
          Height = 21
          MaxLength = 255
          TabOrder = 0
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Custom Icon'
        object Image1: TImage
          Left = 32
          Top = 20
          Width = 32
          Height = 32
          AutoSize = True
        end
        object Label12: TLabel
          Left = 33
          Top = 171
          Width = 84
          Height = 13
          Caption = 'Custom Icon Path'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object IconIndexLabel: TLabel
          Left = 33
          Top = 123
          Width = 50
          Height = 13
          Caption = 'Icon Index'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Memo4: TMemo
          Left = 115
          Top = 48
          Width = 310
          Height = 53
          TabStop = False
          BorderStyle = bsNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'If you want to specify a different icon for the Self '
            'Extracting Archive, you may select it here.')
          ParentColor = True
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
        end
        object IconEdit: TEdit
          Left = 25
          Top = 186
          Width = 388
          Height = 21
          TabOrder = 1
          OnKeyPress = IconEditKeyPress
        end
        object GroupBox2: TGroupBox
          Left = 416
          Top = 96
          Width = 129
          Height = 121
          Caption = 'Icon select:'
          TabOrder = 2
          object Label2: TLabel
            Left = 41
            Top = 92
            Width = 68
            Height = 13
            Caption = 'Select an icon'
          end
          object DefIconRB: TRadioButton
            Left = 23
            Top = 21
            Width = 85
            Height = 17
            Caption = 'Default icon'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = DefIconRBClick
          end
          object AutoIconRB: TRadioButton
            Tag = 1
            Left = 23
            Top = 45
            Width = 97
            Height = 17
            Caption = 'AutoRun icon'
            TabOrder = 1
            OnClick = DefIconRBClick
          end
          object BitBtn2: TBitBtn
            Left = 6
            Top = 88
            Width = 31
            Height = 24
            TabOrder = 2
            OnClick = BitBtn2Click
            Glyph.Data = {
              86050000424D8605000000000000360000002800000016000000140000000100
              18000000000050050000000000000000000000000000000000006F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F00006F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              00006F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F00006F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F00006F6F6F6F6F6F6F6F6F737273494F493E463E40474040
              47404047404047404047404047404047403D443D4F544F7473746F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F7777770F0F0F000000000000
              0000000000000000000000000000000000000000000000001215125359537473
              746F6F6F6F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F7777770F0F0F0000
              00006262008484007F7F007F7F007F7F007F7F007F7F007F7F0087870051520F
              03015158517473746F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F77777700
              00009CA39C2E2321005657008686007F7F007F7F007F7F007F7F007F7F007F7F
              008686005B5B0D01005056507372736F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F
              7777770F000000D1D29BE4DD351F1C004B4B008787007F7F007F7F007F7F007F
              7F007F7F007F7F0084840060600B00004C524C7272726F6F6F6F6F6F00006F6F
              6F6F6F6F7777770000009C89831CFDFC8EE1DB442927004546008989007F7F00
              7F7F007F7F007F7F007F7F007F7F0083830063630F04047777776F6F6F6F6F6F
              00006F6F6F6F6F6F7777770F000000D1D29BC9C322F1F080E7E24B3D3A000000
              0000000000000000000000000000000000000000000000000F0F0F7777776F6F
              6F6F6F6F00006F6F6F6F6F6F7777770000009C89831CFDFC8EC5BF30EBE978CE
              C948E2DF60D8D460D8D448F6F38068640001005359537473746F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F00006F6F6F6F6F6F7777770F000000D1D29BC9C322F1F080
              CBC638E8E668D4D050DEDB50DEDB77E3DE00A6A81200005359537473746F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F7777770000009C89831CFDFC
              8EE1DB4437350000000000000000000000000000000000002424247B7B7B6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F6F6F6F7878781616
              160000000000004B4B4B7676766F6F6F6F6F6F6F6F6F6F6F6F6F6F6F67757594
              5252FF0000FF0000EC0F0F6577776F6F6F6F6F6F00006F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F697474905656FF0000EC0F0F6577776F6F6F6F6F6F00006F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F617A7ABF3232D024
              245F7B7B6775759A4E4EE91111816161F806066577776F6F6F6F6F6F00006F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F62
              7979AD3F3FFF0000FF0000DA1D1D607B7B6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              00006F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F00006F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
              6F6F6F6F6F6F6F6F0000}
          end
          object OrigIconRB: TRadioButton
            Tag = 2
            Left = 23
            Top = 66
            Width = 97
            Height = 17
            Caption = 'Original icon'
            TabOrder = 3
            OnClick = DefIconRBClick
          end
        end
        object IconIndexEdit: TEdit
          Left = 25
          Top = 140
          Width = 45
          Height = 21
          Color = clBtnFace
          Enabled = False
          TabOrder = 3
          Text = '0'
        end
        object IconIndexUD: TUpDown
          Left = 70
          Top = 140
          Width = 14
          Height = 21
          Associate = IconIndexEdit
          Enabled = False
          Min = 0
          Max = 32767
          Position = 0
          TabOrder = 4
          Thousands = False
          Wrap = True
          OnClick = IconIndexUDClick
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Command line to Execute after Extraction'
        object Memo2: TMemo
          Left = 27
          Top = 2
          Width = 518
          Height = 187
          TabStop = False
          BorderStyle = bsNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'Special Command Line Symbols :'
            
              'A pipe symbol '#39'|'#39' separates the program name from the argument l' +
              'ist.'
            
              'Two Arrows '#39'><'#39' will be replaced by the actual extract directory' +
              #39'.'
            
              'The following example runs notepad to display readme.txt in extr' +
              'act dir :'
            '"notepad|><readme.txt"'
            ''
            
              'You can also install .inf scripts, even with support for differe' +
              'nt sections depending on the '
            'operating system'
            
              '"><setup.inf" will run the [DefaultInstall] section of "EXTRACTP' +
              'ATH\setup.inf".'
            
              '"><setup.inf|.ntx86" will run the [DefaultInstall] section if th' +
              'e User'#39's OS is Win95/98,'
            'but [DefaultInstall.ntx86] section if it'#39's Windows NT.')
          ParentColor = True
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
        end
        object CommandEdit: TEdit
          Left = 25
          Top = 186
          Width = 524
          Height = 21
          MaxLength = 255
          TabOrder = 1
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Default Extract Path'
        object BitBtn1: TBitBtn
          Left = 519
          Top = 184
          Width = 31
          Height = 24
          TabOrder = 1
          OnClick = BitBtn1Click
          Glyph.Data = {
            86050000424D8605000000000000360000002800000016000000140000000100
            18000000000050050000000000000000000000000000000000006F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F00006F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            00006F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F00006F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F00006F6F6F6F6F6F6F6F6F737273494F493E463E40474040
            47404047404047404047404047404047403D443D4F544F7473746F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F7777770F0F0F000000000000
            0000000000000000000000000000000000000000000000001215125359537473
            746F6F6F6F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F7777770F0F0F0000
            00006262008484007F7F007F7F007F7F007F7F007F7F007F7F0087870051520F
            03015158517473746F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F77777700
            00009CA39C2E2321005657008686007F7F007F7F007F7F007F7F007F7F007F7F
            008686005B5B0D01005056507372736F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F
            7777770F000000D1D29BE4DD351F1C004B4B008787007F7F007F7F007F7F007F
            7F007F7F007F7F0084840060600B00004C524C7272726F6F6F6F6F6F00006F6F
            6F6F6F6F7777770000009C89831CFDFC8EE1DB442927004546008989007F7F00
            7F7F007F7F007F7F007F7F007F7F0083830063630F04047777776F6F6F6F6F6F
            00006F6F6F6F6F6F7777770F000000D1D29BC9C322F1F080E7E24B3D3A000000
            0000000000000000000000000000000000000000000000000F0F0F7777776F6F
            6F6F6F6F00006F6F6F6F6F6F7777770000009C89831CFDFC8EC5BF30EBE978CE
            C948E2DF60D8D460D8D448F6F38068640001005359537473746F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F00006F6F6F6F6F6F7777770F000000D1D29BC9C322F1F080
            CBC638E8E668D4D050DEDB50DEDB77E3DE00A6A81200005359537473746F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F7777770000009C89831CFDFC
            8EE1DB4437350000000000000000000000000000000000002424247B7B7B6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F00006F6F6F6F6F6F6F6F6F7878781616
            160000000000004B4B4B7676766F6F6F6F6F6F6F6F6F6F6F6F6F6F6F67757594
            5252FF0000FF0000EC0F0F6577776F6F6F6F6F6F00006F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F697474905656FF0000EC0F0F6577776F6F6F6F6F6F00006F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F617A7ABF3232D024
            245F7B7B6775759A4E4EE91111816161F806066577776F6F6F6F6F6F00006F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F62
            7979AD3F3FFF0000FF0000DA1D1D607B7B6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            00006F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F00006F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F
            6F6F6F6F6F6F6F6F0000}
        end
        object Memo1: TMemo
          Left = 27
          Top = 24
          Width = 514
          Height = 157
          TabStop = False
          BorderStyle = bsNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'Default Extract Directory on User'#39's Computer'
            '(if '#39'><'#39' then the windows temporary directory will be used).'
            ''
            
              'The path can also be read from the user'#39's registry by typing a r' +
              'egistry key (some '
            
              'sample directory describing values can be found in the combobox ' +
              'items below).'
            ''
            
              'Also creating subfolders in these registry-read pathes is possib' +
              'le by entering a pipe ('#39'|'#39') '
            
              'and then the name of the new sub directory after the registry ke' +
              'y.')
          ParentColor = True
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
        end
        object DirectoryEdit: TComboBox
          Left = 26
          Top = 186
          Width = 492
          Height = 21
          DropDownCount = 9
          ItemHeight = 13
          Items.Strings = (
            'HKEY_LOCAL_MACHINE\'
            'HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\ProgramFilesDir'
            'HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\CommonFilesDir'
            'HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\WallPaperDir'
            'HKEY_CURRENT_USER\Software\Borland\C++Builder\3.0\'
            'HKEY_CURRENT_USER\Software\Borland\Delphi\4.0\'
            'HKCU\Software\'
            
              'HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Fo' +
              'lders\'
            
              'HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Fo' +
              'lders\')
          MaxLength = 255
          TabOrder = 2
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'All files with icons|*.ico;*.dll;*.exe;*.cur;*.ani;*.icl;*.ica|I' +
      'con (*.ico)|*.ico|Executable (*.exe;*.dll)|*.exe;*.dll|Cursor (*' +
      '.cur)|*.cur|Animated cursor (*.ani)|*.ani|Icon library (*.icl;*.' +
      'il)|*.icl;*.il|Icon Archive (*.ica)|*.ica'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofShareAware]
    Title = 'Select an Icon File'
    Left = 437
    Top = 20
  end
  object ImageList1: TImageList
    AllocBy = 2
    Height = 32
    Width = 32
    Left = 437
    Top = 56
  end
end
