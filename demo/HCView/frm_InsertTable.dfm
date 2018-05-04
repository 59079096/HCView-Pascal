object frmInsertTable: TfrmInsertTable
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #34920#26684#20449#24687
  ClientHeight = 149
  ClientWidth = 237
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 40
    Top = 29
    Width = 24
    Height = 13
    Caption = #34892#25968
  end
  object lbl2: TLabel
    Left = 40
    Top = 64
    Width = 24
    Height = 13
    Caption = #21015#25968
  end
  object edtRows: TEdit
    Left = 70
    Top = 26
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '2'
  end
  object edtCols: TEdit
    Left = 71
    Top = 61
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '2'
  end
  object btnOk: TButton
    Left = 86
    Top = 106
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 2
    OnClick = btnOkClick
  end
end
