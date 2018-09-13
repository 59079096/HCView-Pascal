object frmBorderBackColor: TfrmBorderBackColor
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #36793#26694#21450#32972#26223#33394
  ClientHeight = 176
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl8: TLabel
    Left = 16
    Top = 16
    Width = 36
    Height = 13
    Caption = #24212#29992#20110
  end
  object lbl1: TLabel
    Left = 58
    Top = 48
    Width = 24
    Height = 13
    Caption = #36793#26694
  end
  object lbl2: TLabel
    Left = 58
    Top = 83
    Width = 36
    Height = 13
    Caption = #32972#26223#33394
  end
  object cbbRang: TComboBox
    Left = 58
    Top = 13
    Width = 247
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = #21333#20803#26684
    Items.Strings = (
      #21333#20803#26684
      #25972#20010#34920#26684)
  end
  object chkLeft: TCheckBox
    Left = 92
    Top = 47
    Width = 44
    Height = 17
    Caption = #24038
    TabOrder = 1
  end
  object chkTop: TCheckBox
    Left = 154
    Top = 47
    Width = 44
    Height = 17
    Caption = #19978
    TabOrder = 2
  end
  object chkRight: TCheckBox
    Left = 210
    Top = 47
    Width = 44
    Height = 17
    Caption = #21491
    TabOrder = 3
  end
  object chkBottom: TCheckBox
    Left = 267
    Top = 47
    Width = 44
    Height = 17
    Caption = #19979
    TabOrder = 4
  end
  object cbbBackColor: TColorBox
    Left = 100
    Top = 80
    Width = 81
    Height = 22
    DefaultColorColor = clNone
    NoneColorColor = clWhite
    Selected = clNone
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
    DropDownCount = 20
    TabOrder = 5
  end
  object btnOk: TButton
    Left = 117
    Top = 136
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 6
    OnClick = btnOkClick
  end
end
