object frmTableProperty: TfrmTableProperty
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #34920#26684#23646#24615
  ClientHeight = 375
  ClientWidth = 420
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
  object pgTable: TPageControl
    Left = 0
    Top = 0
    Width = 420
    Height = 334
    ActivePage = tsTable
    Align = alClient
    TabOrder = 0
    object tsTable: TTabSheet
      Caption = #34920#26684
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbl1: TLabel
        Left = 24
        Top = 10
        Width = 60
        Height = 13
        Caption = #21333#20803#26684#36793#36317
      end
      object lbl2: TLabel
        Left = 135
        Top = 68
        Width = 48
        Height = 13
        Caption = #36793#26694#23485#24230
      end
      object lbl4: TLabel
        Left = 43
        Top = 33
        Width = 24
        Height = 13
        Caption = #24038#21491
      end
      object lbl5: TLabel
        Left = 159
        Top = 33
        Width = 24
        Height = 13
        Caption = #19978#19979
      end
      object edtCellHPadding: TEdit
        Left = 73
        Top = 30
        Width = 65
        Height = 21
        TabOrder = 0
        Text = 'edtCellHPadding'
        OnChange = edtCellHPaddingChange
      end
      object edtCellVPadding: TEdit
        Left = 189
        Top = 30
        Width = 65
        Height = 21
        TabOrder = 1
        Text = 'edtCellVPadding'
        OnChange = edtCellHPaddingChange
      end
      object edtBorderWidth: TEdit
        Left = 189
        Top = 64
        Width = 65
        Height = 21
        TabOrder = 2
        Text = 'edtBorderWidth'
        OnChange = edtCellHPaddingChange
      end
      object chkBorderVisible: TCheckBox
        Left = 24
        Top = 66
        Width = 73
        Height = 17
        Caption = #26174#31034#36793#26694
        TabOrder = 3
      end
      object btnBorderBackColor: TButton
        Left = 280
        Top = 63
        Width = 99
        Height = 25
        Caption = #36793#26694#21450#32972#26223#33394
        TabOrder = 4
        OnClick = btnBorderBackColorClick
      end
    end
    object tsRow: TTabSheet
      Caption = #34892'(0)'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbl3: TLabel
        Left = 24
        Top = 50
        Width = 48
        Height = 13
        Caption = #22402#30452#23545#40784
        Visible = False
      end
      object lbl6: TLabel
        Left = 24
        Top = 10
        Width = 56
        Height = 13
        Caption = #34892#39640'('#20687#32032')'
      end
      object cbbRowAlignVert: TComboBox
        Left = 78
        Top = 47
        Width = 91
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = #33258#23450#20041
        Visible = False
        OnChange = edtCellHPaddingChange
        Items.Strings = (
          #33258#23450#20041
          #39030
          #23621#20013
          #24213)
      end
      object edtRowHeight: TEdit
        Left = 88
        Top = 7
        Width = 91
        Height = 21
        TabOrder = 1
        Text = 'edtRowHeight'
        OnChange = edtCellHPaddingChange
      end
    end
    object tsCell: TTabSheet
      Caption = #21333#20803#26684'(0,0)'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbl7: TLabel
        Left = 24
        Top = 10
        Width = 48
        Height = 13
        Caption = #22402#30452#23545#40784
      end
      object cbbCellAlignVert: TComboBox
        Left = 78
        Top = 7
        Width = 91
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = #39030
        OnChange = edtCellHPaddingChange
        Items.Strings = (
          #39030
          #23621#20013
          #24213)
      end
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 334
    Width = 420
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOk: TButton
      Left = 163
      Top = 6
      Width = 75
      Height = 25
      Caption = #30830#23450
      TabOrder = 0
      OnClick = btnOkClick
    end
  end
end
