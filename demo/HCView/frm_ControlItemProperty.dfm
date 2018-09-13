object frmControlItemProperty: TfrmControlItemProperty
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'ControlItem'#23646#24615
  ClientHeight = 164
  ClientWidth = 281
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
  object pnlSize: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lbl1: TLabel
      Left = 35
      Top = 46
      Width = 12
      Height = 13
      Caption = #23485
    end
    object lbl2: TLabel
      Left = 148
      Top = 46
      Width = 12
      Height = 13
      Caption = #39640
    end
    object chkAutoSize: TCheckBox
      Left = 23
      Top = 12
      Width = 97
      Height = 17
      Caption = #33258#21160#35745#31639#23485#39640
      TabOrder = 0
      OnClick = chkAutoSizeClick
    end
    object edtWidth: TEdit
      Left = 53
      Top = 43
      Width = 80
      Height = 21
      TabOrder = 1
    end
    object edtHeight: TEdit
      Left = 173
      Top = 43
      Width = 80
      Height = 21
      TabOrder = 2
    end
  end
  object pnlBorder: TPanel
    Left = 0
    Top = 73
    Width = 281
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lbl3: TLabel
      Left = 23
      Top = 3
      Width = 24
      Height = 13
      Caption = #36793#26694
    end
    object chkBorderTop: TCheckBox
      Left = 35
      Top = 23
      Width = 39
      Height = 17
      Caption = #19978
      TabOrder = 0
    end
    object chkBorderLeft: TCheckBox
      Left = 157
      Top = 23
      Width = 40
      Height = 17
      Caption = #24038
      TabOrder = 1
    end
    object chkBorderRight: TCheckBox
      Left = 221
      Top = 23
      Width = 32
      Height = 17
      Caption = #21491
      TabOrder = 2
    end
    object chkBorderBottom: TCheckBox
      Left = 93
      Top = 23
      Width = 37
      Height = 17
      Caption = #19979
      TabOrder = 3
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 123
    Width = 281
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object btnOk: TButton
      Left = 117
      Top = 8
      Width = 75
      Height = 25
      Caption = #30830#23450
      TabOrder = 0
      OnClick = btnOkClick
    end
  end
end
