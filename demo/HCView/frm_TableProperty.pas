unit frm_TableProperty;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HCView, {HCGridView,} HCTableItem, ComCtrls, StdCtrls, ExtCtrls, Buttons;

type
  TfrmTableProperty = class(TForm)
    pgTable: TPageControl;
    tsTable: TTabSheet;
    tsRow: TTabSheet;
    tsCell: TTabSheet;
    edtCellHPadding: TEdit;
    edtCellVPadding: TEdit;
    edtBorderWidth: TEdit;
    chkBorderVisible: TCheckBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    pnl1: TPanel;
    btnOk: TButton;
    cbbRowAlignVert: TComboBox;
    lbl3: TLabel;
    lbl6: TLabel;
    edtRowHeight: TEdit;
    lbl7: TLabel;
    cbbCellAlignVert: TComboBox;
    btnBorderBackColor: TButton;
    lbl8: TLabel;
    lbl9: TLabel;
    edtFixRowFirst: TEdit;
    lbl10: TLabel;
    edtFixRowLast: TEdit;
    lbl11: TLabel;
    lbl12: TLabel;
    lbl13: TLabel;
    edtFixColFirst: TEdit;
    lbl14: TLabel;
    edtFixColLast: TEdit;
    lbl15: TLabel;
    btnCellBottomBorder: TSpeedButton;
    btnCellTopBorder: TSpeedButton;
    btnCellLeftBorder: TSpeedButton;
    btnCellRightBorder: TSpeedButton;
    btnCellRTLBBorder: TSpeedButton;
    btnCellLTRBBorder: TSpeedButton;
    lbl16: TLabel;
    lbl17: TLabel;
    lbl18: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtCellHPaddingChange(Sender: TObject);
    procedure btnBorderBackColorClick(Sender: TObject);
  private
    { Private declarations }
    FReFormt: Boolean;
    FView: THCView;
    //FGridView: THCGridView;
    FTableItem: THCTableItem;
    procedure GetTableProperty;
    procedure SetTableProperty;
  public
    { Public declarations }
    procedure SetView(const AView: THCView);
    //procedure SetGridView(const AGridView: THCGridView);
  end;

implementation

uses
  HCRichData, HCTableCell, HCCommon, frm_TableBorderBackColor;

{$R *.dfm}

{ TfrmTableProperty }

procedure TfrmTableProperty.btnBorderBackColorClick(Sender: TObject);
var
  vFrmBorderBackColor: TfrmBorderBackColor;
begin
  vFrmBorderBackColor := TfrmBorderBackColor.Create(Self);
  try
    if Assigned(FView) then
      vFrmBorderBackColor.SetView(FView)
    {else
      vFrmBorderBackColor.SetGridView(FGridView)};
  finally
    FreeAndNil(vFrmBorderBackColor);
  end;
end;

procedure TfrmTableProperty.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmTableProperty.edtCellHPaddingChange(Sender: TObject);
begin
  FReFormt := True;
end;

procedure TfrmTableProperty.FormShow(Sender: TObject);
begin
  pgTable.ActivePageIndex := 0;
  FReFormt := False;
end;

procedure TfrmTableProperty.GetTableProperty;

  procedure GetCellProperty_(const ACell: THCTableCell);
  begin
    cbbCellAlignVert.ItemIndex := Ord(ACell.AlignVert);

    btnCellLeftBorder.Down := TBorderSide.cbsLeft in ACell.BorderSides;
    btnCellTopBorder.Down := TBorderSide.cbsTop in ACell.BorderSides;
    btnCellRightBorder.Down := TBorderSide.cbsRight in ACell.BorderSides;
    btnCellBottomBorder.Down := TBorderSide.cbsBottom in ACell.BorderSides;
    btnCellLTRBBorder.Down := TBorderSide.cbsLTRB in ACell.BorderSides;
    btnCellRTLBBorder.Down := TBorderSide.cbsRTLB in ACell.BorderSides;
  end;

var
  vCell: THCTableCell;
begin
  // ���
  edtCellHPadding.Text := FormatFloat('0.##', FTableItem.CellHPaddingMM);
  edtCellVPadding.Text := FormatFloat('0.##', FTableItem.CellVPaddingMM);
  chkBorderVisible.Checked := FTableItem.BorderVisible;
  edtBorderWidth.Text := FormatFloat('0.##', FTableItem.BorderWidthPt);

  edtFixRowFirst.Text := IntToStr(FTableItem.FixRow);
  if FTableItem.FixRowCount > 0 then
    edtFixRowLast.Text := IntToStr(FTableItem.FixRow + FTableItem.FixRowCount - 1)
  else
    edtFixRowLast.Text := edtFixRowFirst.Text;

  edtFixColFirst.Text := IntToStr(FTableItem.FixCol);
  if FTableItem.FixColCount > 0 then
    edtFixColLast.Text := IntToStr(FTableItem.FixCol + FTableItem.FixColCount - 1)
  else
    edtFixColLast.Text := edtFixColFirst.Text;

  // ��
  if FTableItem.SelectCellRang.StartRow >= 0 then
  begin
    tsRow.Caption := '��(' + IntToStr(FTableItem.SelectCellRang.StartRow + 1) + ')';
    if FTableItem.SelectCellRang.EndRow > 0 then
      tsRow.Caption := tsRow.Caption + ' - (' + IntToStr(FTableItem.SelectCellRang.EndRow + 1) + ')';

    edtRowHeight.Text := IntToStr(FTableItem.Rows[FTableItem.SelectCellRang.StartRow].Height);  // �и�
  end
  else
    tsRow.TabVisible := False;

  {vAlignVert := FTableItem.GetEditCell.AlignVert;
  cbbRowAlignVert.ItemIndex := Ord(vAlignVert) + 1;
  for i := 0 to FTableItem.Rows[FTableItem.SelectCellRang.StartRow].ColCount - 1 do
  begin
    if vAlignVert <> FTableItem.Cells[FTableItem.SelectCellRang.StartRow, i].AlignVert then  // �в�ͬ
    begin
      cbbRowAlignVert.ItemIndex := 0;  // �Զ���
      Break;
    end;
  end;
  vRowAlignIndex := cbbRowAlignVert.ItemIndex;}

  // ��Ԫ��
  if (FTableItem.SelectCellRang.StartRow >= 0) and (FTableItem.SelectCellRang.StartCol >= 0) then
  begin
    if FTableItem.SelectCellRang.EndRow >= 0 then  // ��ѡ
    begin
      vCell := FTableItem.Cells[FTableItem.SelectCellRang.StartRow,
        FTableItem.SelectCellRang.StartCol];

      tsCell.Caption := '��Ԫ��(' + IntToStr(FTableItem.SelectCellRang.StartRow + 1) + ','
        + IntToStr(FTableItem.SelectCellRang.StartCol + 1) + ') - ('
        + IntToStr(FTableItem.SelectCellRang.EndRow + 1) + ','
        + IntToStr(FTableItem.SelectCellRang.EndCol + 1) + ')';
    end
    else
    begin
      vCell := FTableItem.GetEditCell;
      tsCell.Caption := '��Ԫ��(' + IntToStr(FTableItem.SelectCellRang.StartRow + 1) + ','
        + IntToStr(FTableItem.SelectCellRang.StartCol + 1) + ')';
    end;

    GetCellProperty_(vCell);
  end
  else
    tsCell.TabVisible := False;
end;

{procedure TfrmTableProperty.SetGridView(const AGridView: THCGridView);
begin
  FView := nil;
  FGridView := AGridView;
  FTableItem := AGridView.Page.GetActiveItem as THCTableItem;

  GetTableProperty;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    FGridView.BeginUpdate;
    try
      SetTableProperty;

      if FReFormt then
        FGridView.ReFormatActiveItem;

      FGridView.Style.UpdateInfoRePaint;
    finally
      FGridView.EndUpdate;
    end;
  end;
end;}

procedure TfrmTableProperty.SetTableProperty;
var
  vR, vC, viValue: Integer;
  vCell: THCTableCell;
begin
  // ���
  FTableItem.CellHPaddingMM := StrToFloatDef(edtCellHPadding.Text, 0.2);
  FTableItem.CellVPaddingMM := StrToFloatDef(edtCellVPadding.Text, 0);
  FTableItem.BorderWidthPt := StrToFloatDef(edtBorderWidth.Text, 0.5);
  FTableItem.BorderVisible := chkBorderVisible.Checked;

  FTableItem.SetFixRowAndCount(StrToIntDef(edtFixRowFirst.Text, -1),
    StrToIntDef(edtFixRowLast.Text, -1) - StrToIntDef(edtFixRowFirst.Text, -1) + 1);
  FTableItem.SetFixColAndCount(StrToIntDef(edtFixColFirst.Text, -1),
    StrToIntDef(edtFixColLast.Text, -1) - StrToIntDef(edtFixColFirst.Text, -1) + 1);

  // ��
  if (FTableItem.SelectCellRang.StartRow >= 0) and (TryStrToInt(edtRowHeight.Text, viValue)) then
  begin
    if FTableItem.SelectCellRang.EndRow > 0 then  // ��ѡ�ж���
    begin
      for vR := FTableItem.SelectCellRang.StartRow to FTableItem.SelectCellRang.EndRow do
        FTableItem.Rows[vR].Height := viValue;  // �и�
    end
    else  // ֻѡ��һ��
      FTableItem.Rows[FTableItem.SelectCellRang.StartRow].Height := viValue;  // �и�
  end;

  // ��Ԫ��
  if (FTableItem.SelectCellRang.StartRow >= 0) and (FTableItem.SelectCellRang.StartCol >= 0) then
  begin
    if FTableItem.SelectCellRang.EndCol > 0 then  // ��ѡ�ж����Ԫ��
    begin
      for vR := FTableItem.SelectCellRang.StartRow to FTableItem.SelectCellRang.EndRow do
      begin
        for vC := FTableItem.SelectCellRang.StartCol to FTableItem.SelectCellRang.EndCol do
        begin
          vCell := FTableItem.Cells[vR, vC];
          vCell.AlignVert := THCAlignVert(cbbCellAlignVert.ItemIndex);

          if btnCellLeftBorder.Down then
            vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsLeft]
          else
            vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsLeft];

          if btnCellTopBorder.Down then
            vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsTop]
          else
            vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsTop];

          if btnCellRightBorder.Down then
            vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsRight]
          else
            vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsRight];

          if btnCellBottomBorder.Down then
            vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsBottom]
          else
            vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsBottom];

          if btnCellLTRBBorder.Down then
            vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsLTRB]
          else
            vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsLTRB];

          if btnCellRTLBBorder.Down then
            vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsRTLB]
          else
            vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsRTLB];
        end;
      end;
    end
    else  // ����һ����Ԫ��
    begin
      vCell := FTableItem.GetEditCell;
      vCell.AlignVert := THCAlignVert(cbbCellAlignVert.ItemIndex);

      if btnCellLeftBorder.Down then
        vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsLeft]
      else
        vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsLeft];

      if btnCellTopBorder.Down then
        vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsTop]
      else
        vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsTop];

      if btnCellRightBorder.Down then
        vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsRight]
      else
        vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsRight];

      if btnCellBottomBorder.Down then
        vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsBottom]
      else
        vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsBottom];

      if btnCellLTRBBorder.Down then
        vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsLTRB]
      else
        vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsLTRB];

      if btnCellRTLBBorder.Down then
        vCell.BorderSides := vCell.BorderSides + [TBorderSide.cbsRTLB]
      else
        vCell.BorderSides := vCell.BorderSides - [TBorderSide.cbsRTLB];
    end;
  end;
end;

procedure TfrmTableProperty.SetView(const AView: THCView);
begin
  //FGridView := nil;
  FView := AView;

  FTableItem := FView.ActiveSection.ActiveData.GetActiveItem as THCTableItem;

  GetTableProperty;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    FView.BeginUpdate;
    try
      SetTableProperty;

      if FReFormt then
        FView.ActiveSection.ReFormatActiveItem;

      FView.Style.UpdateInfoRePaint;
    finally
      FView.EndUpdate;
    end;
  end;
end;

end.
