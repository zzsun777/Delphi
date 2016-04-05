inherited fCropMark: TfCropMark
  BorderStyle = bsToolWindow
  Caption = 'fCropMark'
  ClientHeight = 251
  ClientWidth = 411
  OnCreate = FormCreate
  ExplicitWidth = 417
  ExplicitHeight = 280
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 337
    Height = 177
    ActivePage = ts1
    TabOrder = 0
    object ts1: TTabSheet
      Caption = #35009#20999#26631#35760
      object lbl7: TLabel
        Left = 3
        Top = 122
        Width = 24
        Height = 13
        Caption = #20013#32447
      end
      object lbl8: TLabel
        Left = 166
        Top = 122
        Width = 24
        Height = 13
        Caption = #22841#20992
      end
      object grp1: TGroupBox
        Left = 3
        Top = 3
        Width = 150
        Height = 102
        Caption = #35009#20999#26631#35760
        TabOrder = 0
        object lbl1: TLabel
          Left = 15
          Top = 32
          Width = 24
          Height = 13
          Caption = #38271#24230
        end
        object lbl2: TLabel
          Left = 15
          Top = 61
          Width = 24
          Height = 13
          Caption = #20559#31227
        end
        object lbl3: TLabel
          Left = 107
          Top = 32
          Width = 24
          Height = 13
          Caption = #27627#31859
        end
        object lbl4: TLabel
          Left = 107
          Top = 61
          Width = 24
          Height = 13
          Caption = #27627#31859
        end
        object nudLen: TCnSpinEdit
          Left = 45
          Top = 29
          Width = 56
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 2
        end
        object nudOffset: TCnSpinEdit
          Left = 45
          Top = 57
          Width = 56
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = -1
        end
      end
      object grp2: TGroupBox
        Left = 167
        Top = 3
        Width = 145
        Height = 102
        Caption = #20986#34880#32447
        TabOrder = 1
        object lbl5: TLabel
          Left = 111
          Top = 61
          Width = 24
          Height = 13
          Caption = #27627#31859
        end
        object lbl6: TLabel
          Left = 18
          Top = 61
          Width = 24
          Height = 13
          Caption = #20986#34880
        end
        object chk_Chu: TCheckBox
          Left = 16
          Top = 31
          Width = 97
          Height = 17
          Caption = #21253#25324#20986#34880#26631#35760
          TabOrder = 0
        end
        object nudCu: TSpinEdit
          Left = 48
          Top = 57
          Width = 57
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
      object chkZxSP: TCheckBox
        Left = 44
        Top = 120
        Width = 44
        Height = 17
        Caption = #27700#24179
        TabOrder = 2
      end
      object chkZxCZ: TCheckBox
        Left = 105
        Top = 120
        Width = 44
        Height = 17
        Caption = #22402#30452
        TabOrder = 3
      end
      object chkJdCZ: TCheckBox
        Left = 268
        Top = 120
        Width = 44
        Height = 17
        Caption = #22402#30452
        TabOrder = 4
      end
      object chkJdSP: TCheckBox
        Left = 207
        Top = 120
        Width = 44
        Height = 17
        Caption = #27700#24179
        TabOrder = 5
      end
    end
  end
  object btn_OK: TButton
    Left = 160
    Top = 210
    Width = 75
    Height = 25
    Caption = #24212#29992
    TabOrder = 1
    OnClick = btn_OKClick
  end
end
