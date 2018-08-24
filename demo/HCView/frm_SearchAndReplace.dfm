object frmSearchAndReplace: TfrmSearchAndReplace
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #26597#25214#21644#26367#25442
  ClientHeight = 161
  ClientWidth = 434
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
  object pgTab: TPageControl
    Left = 0
    Top = 0
    Width = 434
    Height = 161
    ActivePage = tsSearch
    Align = alClient
    TabOrder = 0
    object tsSearch: TTabSheet
      Caption = #26597#25214
      object cbbSearch: TComboBox
        Left = 16
        Top = 16
        Width = 393
        Height = 21
        TabOrder = 0
      end
      object btnSearchBackward: TButton
        Left = 334
        Top = 96
        Width = 75
        Height = 25
        Caption = #19979#19968#22788
        TabOrder = 1
        OnClick = btnSearchBackwardClick
      end
      object btnSearchForward: TButton
        Left = 239
        Top = 96
        Width = 75
        Height = 25
        Caption = #19978#19968#22788
        TabOrder = 2
        OnClick = btnSearchForwardClick
      end
      object chkSearchCase: TCheckBox
        Left = 16
        Top = 50
        Width = 97
        Height = 17
        Caption = #21306#20998#22823#23567#20889
        TabOrder = 3
      end
      object chkSearchHight: TCheckBox
        Left = 120
        Top = 50
        Width = 97
        Height = 17
        Caption = #39640#20142#26174#31034
        Enabled = False
        TabOrder = 4
      end
    end
    object tsReplace: TTabSheet
      Caption = #26367#25442
      ImageIndex = 1
    end
  end
end
