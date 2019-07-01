unit frm_TableBorderBackColor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HCView, HCGridView, HCTableItem, StdCtrls, ExtCtrls;

type
  TfrmBorderBackColor = class(TForm)
    lbl8: TLabel;
    lbl1: TLabel;
    lbl2: TLabel;
    cbbRang: TComboBox;
    chkLeft: TCheckBox;
    chkTop: TCheckBox;
    chkRight: TCheckBox;
    chkBottom: TCheckBox;
    cbbBackColor: TColorBox;
    btnOk: TButton;
    chkLTRB: TCheckBox;
    chkRTLB: TCheckBox;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    FTableItem: THCTableItem;
    procedure GetTableProperty;
    procedure SetTableProperty;
  public
    { Public declarations }
    procedure SetView(const AView: THCView);
    procedure SetGridView(const AGridView: THCGridView);
  end;

implementation

uses
  HCTableCell, HCCommon;

{$R *.dfm}

procedure TfrmBorderBackColor.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmBorderBackColor.GetTableProperty;
var
  vCell: THCTableCell;
  vBorderSides: TBorderSides;
begin
  if FTableItem.SelectCellRang.StartRow >= 0 then
    vCell := FTableItem.Cells[FTableItem.SelectCellRang.StartRow, FTableItem.SelectCellRang.StartCol]
  else
    vCell := FTableItem.Cells[0, 0];

  cbbBackColor.Selected := vCell.BackgroundColor;

  vBorderSides := vCell.BorderSides;
  chkLeft.Checked := cbsLeft in vBorderSides;
  chkTop.Checked := cbsTop in vBorderSides;
  chkRight.Checked := cbsRight in vBorderSides;
  chkBottom.Checked := cbsBottom in vBorderSides;
  chkLTRB.Checked := cbsLTRB in vBorderSides;
  chkRTLB.Checked := cbsRTLB in vBorderSides;
end;

procedure TfrmBorderBackColor.SetGridView(const AGridView: THCGridView);
begin
  FTableItem := AGridView.Page.GetActiveItem as THCTableItem;
  GetTableProperty;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AGridView.BeginUpdate;
    try
      SetTableProperty;

      AGridView.Style.UpdateInfoRePaint;
    finally
      AGridView.EndUpdate;
    end;
  end;
end;

procedure TfrmBorderBackColor.SetTableProperty;
var
  vBorderSides: TBorderSides;

  procedure SetCellBorderBackColor(const ARow, ACol: Integer);
  begin
    if cbbBackColor.Selected = HCTransparentColor then
      FTableItem.Cells[ARow, ACol].BackgroundColor := HCTransparentColor  //cbbBackColor.NoneColorColor
    else
      FTableItem.Cells[ARow, ACol].BackgroundColor := cbbBackColor.Selected;

    vBorderSides := FTableItem.Cells[ARow, ACol].BorderSides;

    if chkLeft.Checked then
      Include(vBorderSides, cbsLeft)
    else
      Exclude(vBorderSides, cbsLeft);

    if chkTop.Checked then
      Include(vBorderSides, cbsTop)
    else
      Exclude(vBorderSides, cbsTop);

    if chkRight.Checked then
      Include(vBorderSides, cbsRight)
    else
      Exclude(vBorderSides, cbsRight);

    if chkBottom.Checked then
      Include(vBorderSides, cbsBottom)
    else
      Exclude(vBorderSides, cbsBottom);

    if chkLTRB.Checked then
      Include(vBorderSides, cbsLTRB)
    else
      Exclude(vBorderSides, cbsLTRB);

    if chkRTLB.Checked then
      Include(vBorderSides, cbsRTLB)
    else
      Exclude(vBorderSides, cbsRTLB);

    FTableItem.Cells[ARow, ACol].BorderSides := vBorderSides;
  end;

  procedure ApplyAllTable;
  var
    vR, vC: Integer;
  begin
    for vR := 0 to FTableItem.RowCount - 1 do
    begin
      for vC := 0 to FTableItem.ColCount - 1 do
        SetCellBorderBackColor(vR, vC);
    end;
  end;

var
  vR, vC: Integer;
begin
  if cbbRang.ItemIndex = 0 then  // 单元格
  begin
    if FTableItem.SelectCellRang.EditCell then  // 在同一个单元格编辑
      SetCellBorderBackColor(FTableItem.SelectCellRang.StartRow, FTableItem.SelectCellRang.StartCol)
    else  // 多选或一个也没选
    begin
      if FTableItem.SelectCellRang.StartRow >= 0 then  // 多选
      begin
        for vR := FTableItem.SelectCellRang.StartRow to FTableItem.SelectCellRang.EndRow do
        begin
          for vC := FTableItem.SelectCellRang.StartCol to FTableItem.SelectCellRang.EndCol do
            SetCellBorderBackColor(vR, vC);
        end;
      end
      else  // 一个也没选，按整个表格处理
        ApplyAllTable;
    end;
  end
  else
    ApplyAllTable;  // 整个表格
end;

procedure TfrmBorderBackColor.SetView(const AView: THCView);
begin
  FTableItem := AView.ActiveSection.ActiveData.GetActiveItem as THCTableItem;
  GetTableProperty;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AView.BeginUpdate;
    try
      SetTableProperty;

      AView.Style.UpdateInfoRePaint;
    finally
      AView.EndUpdate;
    end;
  end;
end;

end.
