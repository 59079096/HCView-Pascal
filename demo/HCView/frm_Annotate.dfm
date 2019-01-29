object frmAnnotate: TfrmAnnotate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20462#25913#25209#27880
  ClientHeight = 237
  ClientWidth = 319
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
    Left = 15
    Top = 16
    Width = 36
    Height = 13
    Caption = #26631#39064#65306
  end
  object lbl2: TLabel
    Left = 15
    Top = 72
    Width = 24
    Height = 13
    Caption = #25209#27880
  end
  object edtTitle: TEdit
    Left = 15
    Top = 35
    Width = 289
    Height = 21
    TabOrder = 0
  end
  object mmoText: TMemo
    Left = 15
    Top = 91
    Width = 289
    Height = 89
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 112
    Top = 193
    Width = 75
    Height = 25
    Caption = #20445#23384#20462#25913
    TabOrder = 2
    OnClick = btnOKClick
  end
end
