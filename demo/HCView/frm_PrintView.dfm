object frmPrintView: TfrmPrintView
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #25171#21360#39044#35272
  ClientHeight = 489
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 489
    Align = alLeft
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 0
    object lbl1: TLabel
      Left = 20
      Top = 27
      Width = 36
      Height = 13
      Caption = #25171#21360#26426
    end
    object lbl2: TLabel
      Left = 20
      Top = 97
      Width = 30
      Height = 13
      Caption = #39029'  '#30721
    end
    object lbl3: TLabel
      Left = 20
      Top = 62
      Width = 30
      Height = 13
      Caption = #20221'  '#25968
    end
    object btnPrint: TButton
      Left = 20
      Top = 157
      Width = 246
      Height = 50
      Caption = #25171#21360
      TabOrder = 0
      OnClick = btnPrintClick
    end
    object cbbPrinter: TComboBox
      Left = 65
      Top = 24
      Width = 201
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object edtCopies: TEdit
      Left = 65
      Top = 59
      Width = 72
      Height = 21
      TabOrder = 2
      Text = '1'
      OnExit = edtCopiesExit
      OnKeyPress = edtCopiesKeyPress
    end
    object cbbPage: TComboBox
      Left = 65
      Top = 94
      Width = 201
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = #20840#37096#39029
      OnChange = cbbPageChange
      Items.Strings = (
        #20840#37096#39029
        #24403#21069#39029
        #22855#25968#39029
        #20598#25968#39029
        #33258#23450#20041#39029)
    end
    object ud1: TUpDown
      Left = 137
      Top = 57
      Width = 17
      Height = 25
      TabOrder = 4
      OnClick = ud1Click
    end
    object edtPrintPageNos: TEdit
      Left = 65
      Top = 121
      Width = 201
      Height = 21
      ImeMode = imClose
      TabOrder = 5
      Visible = False
    end
  end
  object pnl2: TPanel
    Left = 289
    Top = 0
    Width = 430
    Height = 489
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pbPage: TPaintBox
      Left = 0
      Top = 0
      Width = 413
      Height = 448
      Align = alClient
      OnMouseDown = pbPageMouseDown
      OnPaint = pbPagePaint
      ExplicitLeft = 283
      ExplicitWidth = 442
      ExplicitHeight = 473
    end
    object scrlbrPage: TScrollBar
      Left = 413
      Top = 0
      Width = 17
      Height = 448
      Align = alRight
      Kind = sbVertical
      PageSize = 1
      TabOrder = 0
      OnChange = scrlbrPageChange
    end
    object pnl3: TPanel
      Left = 0
      Top = 448
      Width = 430
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object lblPageCount: TLabel
        Left = 86
        Top = 13
        Width = 63
        Height = 13
        AutoSize = False
        Caption = 'lblPageCount'
      end
      object lbl4: TLabel
        Left = 173
        Top = 13
        Width = 26
        Height = 13
        AutoSize = False
        Caption = #32553#25918
      end
      object edtPageNo: TEdit
        Left = 36
        Top = 10
        Width = 44
        Height = 21
        TabOrder = 0
        Text = '1'
        OnExit = edtPageNoExit
        OnKeyPress = edtPageNoKeyPress
      end
      object cbbZoom: TComboBox
        Left = 201
        Top = 10
        Width = 53
        Height = 21
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 1
        Text = '75'
        OnChange = cbbZoomChange
        Items.Strings = (
          '25'
          '50'
          '75'
          '100'
          '150'
          '200')
      end
    end
  end
end
