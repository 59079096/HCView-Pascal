object frmPageSet: TfrmPageSet
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #39029#38754#35774#32622
  ClientHeight = 295
  ClientWidth = 387
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
  object lbl1: TLabel
    Left = 46
    Top = 106
    Width = 12
    Height = 13
    Caption = #24038
  end
  object lbl2: TLabel
    Left = 203
    Top = 106
    Width = 12
    Height = 13
    Caption = #21491
  end
  object lbl3: TLabel
    Left = 46
    Top = 79
    Width = 12
    Height = 13
    Caption = #19978
  end
  object lbl4: TLabel
    Left = 203
    Top = 79
    Width = 12
    Height = 13
    Caption = #19979
  end
  object lbl5: TLabel
    Left = 34
    Top = 21
    Width = 24
    Height = 13
    Caption = #32440#24352
  end
  object lbl6: TLabel
    Left = 203
    Top = 19
    Width = 12
    Height = 13
    Caption = #23485
  end
  object lbl7: TLabel
    Left = 284
    Top = 19
    Width = 12
    Height = 13
    Caption = #39640
  end
  object lbl8: TLabel
    Left = 34
    Top = 143
    Width = 24
    Height = 13
    Caption = #26041#21521
  end
  object lbl9: TLabel
    Left = 34
    Top = 48
    Width = 48
    Height = 13
    Caption = #36793#36317'(mm)'
  end
  object lbl10: TLabel
    Left = 141
    Top = 79
    Width = 24
    Height = 13
    Caption = #27627#31859
  end
  object lbl11: TLabel
    Left = 141
    Top = 106
    Width = 24
    Height = 13
    Caption = #27627#31859
  end
  object lbl12: TLabel
    Left = 300
    Top = 79
    Width = 24
    Height = 13
    Caption = #27627#31859
  end
  object lbl13: TLabel
    Left = 300
    Top = 106
    Width = 24
    Height = 13
    Caption = #27627#31859
  end
  object edtTop: TEdit
    Left = 64
    Top = 76
    Width = 73
    Height = 21
    TabOrder = 0
    Text = '35'
  end
  object edtBottom: TEdit
    Left = 221
    Top = 76
    Width = 73
    Height = 21
    TabOrder = 1
    Text = '15'
  end
  object edtLeft: TEdit
    Left = 64
    Top = 103
    Width = 73
    Height = 21
    TabOrder = 2
    Text = '20'
  end
  object edtRight: TEdit
    Left = 221
    Top = 103
    Width = 73
    Height = 21
    TabOrder = 3
    Text = '15'
  end
  object btnOk: TButton
    Left = 284
    Top = 250
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 4
    OnClick = btnOkClick
  end
  object edtWidth: TEdit
    Left = 221
    Top = 16
    Width = 57
    Height = 21
    TabOrder = 5
    Text = 'edtWidth'
  end
  object edtHeight: TEdit
    Left = 302
    Top = 16
    Width = 40
    Height = 21
    TabOrder = 6
    Text = 'edtHeight'
  end
  object cbbPaper: TComboBox
    Left = 64
    Top = 16
    Width = 101
    Height = 21
    Style = csDropDownList
    TabOrder = 7
    OnChange = cbbPaperChange
    Items.Strings = (
      #33258#23450#20041
      'A3'
      'A4'
      'A5'
      'B5'
      '16K')
  end
  object chkShowLineNo: TCheckBox
    Left = 34
    Top = 207
    Width = 74
    Height = 17
    Caption = #26174#31034#34892#21495
    TabOrder = 8
  end
  object chkShowLineActiveMark: TCheckBox
    Left = 136
    Top = 207
    Width = 142
    Height = 17
    Caption = #26174#31034#24403#21069#32534#36753#34892#25351#31034#31526
    TabOrder = 9
  end
  object chkShowUnderLine: TCheckBox
    Left = 34
    Top = 235
    Width = 142
    Height = 17
    Caption = #26174#31034#19979#21010#32447
    TabOrder = 10
  end
  object cbbPageOrientation: TComboBox
    Left = 64
    Top = 140
    Width = 101
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 11
    Text = #32437#21521
    Items.Strings = (
      #32437#21521
      #27178#21521)
  end
  object chkPageNoVisible: TCheckBox
    Left = 34
    Top = 180
    Width = 79
    Height = 17
    Caption = #26174#31034#39029#30721
    TabOrder = 12
  end
  object chkSymmetryMargin: TCheckBox
    Left = 95
    Top = 47
    Width = 97
    Height = 17
    Caption = #23545#31216#26174#31034
    TabOrder = 13
  end
  object chkParaLastMark: TCheckBox
    Left = 136
    Top = 180
    Width = 89
    Height = 17
    Caption = #26174#31034#25442#34892#31526
    TabOrder = 14
  end
end
