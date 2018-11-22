object frmSearchAndReplace: TfrmSearchAndReplace
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = #26597#25214#21644#26367#25442
  ClientHeight = 204
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 27
    Top = 14
    Width = 26
    Height = 13
    Caption = #26597#25214
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbl2: TLabel
    Left = 27
    Top = 90
    Width = 39
    Height = 13
    Caption = #26367#25442#20026
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object chkSearchCase: TCheckBox
    Left = 27
    Top = 59
    Width = 89
    Height = 17
    Caption = #21306#20998#22823#23567#20889
    TabOrder = 1
  end
  object chkSearchHight: TCheckBox
    Left = 131
    Top = 59
    Width = 89
    Height = 17
    Caption = #39640#20142#26174#31034
    Enabled = False
    TabOrder = 2
  end
  object cbbSearch: TComboBox
    Left = 27
    Top = 33
    Width = 366
    Height = 21
    TabOrder = 0
  end
  object btnSearchForward: TButton
    Left = 27
    Top = 153
    Width = 80
    Height = 25
    Caption = #26597#25214#19978#19968#22788
    TabOrder = 4
    OnClick = btnSearchForwardClick
  end
  object btnSearchBackward: TButton
    Left = 118
    Top = 153
    Width = 80
    Height = 25
    Caption = #26597#25214#19979#19968#22788
    TabOrder = 5
    OnClick = btnSearchBackwardClick
  end
  object cbbReplace: TComboBox
    Left = 27
    Top = 109
    Width = 366
    Height = 21
    TabOrder = 3
  end
  object btnReplace: TButton
    Left = 234
    Top = 153
    Width = 75
    Height = 25
    Caption = #26367'  '#25442
    TabOrder = 6
    OnClick = btnReplaceClick
  end
  object btn1: TButton
    Left = 319
    Top = 153
    Width = 75
    Height = 25
    Caption = #20840#37096#26367#25442
    TabOrder = 7
  end
end
