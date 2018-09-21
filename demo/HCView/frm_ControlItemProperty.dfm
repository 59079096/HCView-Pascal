object frmControlItemProperty: TfrmControlItemProperty
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'ControlItem'#23646#24615
  ClientHeight = 537
  ClientWidth = 314
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
    Width = 314
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
    Width = 314
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
    Top = 496
    Width = 314
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 330
    object btnOk: TButton
      Left = 111
      Top = 8
      Width = 75
      Height = 25
      Caption = #30830#23450
      TabOrder = 0
      OnClick = btnOkClick
    end
  end
  object pnlCombobox: TPanel
    Left = 0
    Top = 123
    Width = 314
    Height = 166
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 3
    object lbl5: TLabel
      Left = 27
      Top = 128
      Width = 12
      Height = 13
      Caption = #20540
    end
    object edtValue: TEdit
      Left = 45
      Top = 125
      Width = 128
      Height = 21
      TabOrder = 0
    end
    object btnAdd: TButton
      Left = 179
      Top = 123
      Width = 25
      Height = 25
      Caption = #22686
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 239
      Top = 123
      Width = 25
      Height = 25
      Caption = #21024
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnSave: TButton
      Left = 209
      Top = 123
      Width = 25
      Height = 25
      Caption = #25913
      TabOrder = 3
      OnClick = btnSaveClick
    end
    object lstCombobox: TListBox
      Left = 20
      Top = 0
      Width = 274
      Height = 117
      Align = alTop
      ItemHeight = 13
      TabOrder = 4
      OnClick = lstComboboxClick
    end
  end
  object pnlDateTime: TPanel
    Left = 0
    Top = 455
    Width = 314
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitTop = 289
    object lbl4: TLabel
      Left = 27
      Top = 13
      Width = 24
      Height = 13
      Caption = #26684#24335
    end
    object cbbDTFormat: TComboBox
      Left = 60
      Top = 10
      Width = 145
      Height = 21
      ItemIndex = 1
      TabOrder = 0
      Text = 'YYYY-MM-DD'
      Items.Strings = (
        'YYYY-MM-DD HH:mm:SS'
        'YYYY-MM-DD'
        'HH:mm:SS'
        'YYYY'#24180'MM'#26376'DD'#26085
        'HH'#26102'mm'#20998)
    end
  end
  object pnlRadioGroup: TPanel
    Left = 0
    Top = 289
    Width = 314
    Height = 166
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 5
    ExplicitTop = 131
    object lbl6: TLabel
      Left = 27
      Top = 128
      Width = 12
      Height = 13
      Caption = #20540
    end
    object edtRadioValue: TEdit
      Left = 45
      Top = 125
      Width = 128
      Height = 21
      TabOrder = 0
    end
    object btnAddRadioItem: TButton
      Left = 179
      Top = 123
      Width = 25
      Height = 25
      Caption = #22686
      TabOrder = 1
      OnClick = btnAddRadioItemClick
    end
    object btnDeleteRadioItem: TButton
      Left = 239
      Top = 123
      Width = 25
      Height = 25
      Caption = #21024
      TabOrder = 2
      OnClick = btnDeleteRadioItemClick
    end
    object btnModRadioItem: TButton
      Left = 209
      Top = 123
      Width = 25
      Height = 25
      Caption = #25913
      TabOrder = 3
      OnClick = btnModRadioItemClick
    end
    object lstRadioItem: TListBox
      Left = 20
      Top = 0
      Width = 274
      Height = 119
      Align = alTop
      ItemHeight = 13
      TabOrder = 4
      OnClick = lstRadioItemClick
    end
  end
end
