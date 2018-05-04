object frmParagraph: TfrmParagraph
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #27573#33853#23646#24615#35774#32622
  ClientHeight = 225
  ClientWidth = 327
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
    Left = 16
    Top = 16
    Width = 68
    Height = 13
    Caption = #34892#38388#36317'('#20687#32032')'
  end
  object lbl2: TLabel
    Left = 16
    Top = 119
    Width = 80
    Height = 13
    Caption = #39318#34892#32553#36827'('#26242#26080')'
  end
  object lbl3: TLabel
    Left = 192
    Top = 119
    Width = 68
    Height = 13
    Caption = #24038#32553#36827'('#26242#26080')'
  end
  object lbl4: TLabel
    Left = 192
    Top = 16
    Width = 36
    Height = 13
    Caption = #32972#26223#33394
  end
  object lbl5: TLabel
    Left = 16
    Top = 72
    Width = 48
    Height = 13
    Caption = #27700#24179#23545#40784
  end
  object lbl6: TLabel
    Left = 192
    Top = 72
    Width = 48
    Height = 13
    Caption = #22402#30452#23545#40784
  end
  object ud1: TUpDown
    Left = 146
    Top = 13
    Width = 17
    Height = 21
    TabOrder = 0
  end
  object edtLineSpace: TEdit
    Left = 86
    Top = 13
    Width = 61
    Height = 21
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 128
    Top = 160
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 2
    OnClick = btnOkClick
  end
  object clrbxBG: TColorBox
    Left = 234
    Top = 13
    Width = 81
    Height = 22
    DefaultColorColor = clNone
    NoneColorColor = clNone
    Selected = clScrollBar
    TabOrder = 3
  end
  object cbbAlignHorz: TComboBox
    Left = 86
    Top = 69
    Width = 77
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = #24038
    Items.Strings = (
      #24038
      #23621#20013
      #21491
      #20004#31471
      #20998#25955)
  end
  object cbbAlignVert: TComboBox
    Left = 246
    Top = 69
    Width = 67
    Height = 21
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 5
    Text = #19979
    Items.Strings = (
      #19978
      #23621#20013
      #19979)
  end
end
