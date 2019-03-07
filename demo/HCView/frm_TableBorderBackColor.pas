unit frm_TableBorderBackColor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HCView, HCTableItem, StdCtrls, ExtCtrls;

type
  TfrmBorderBackColor = class(TForm)
    lbl8: TLabel;
    cbbRang: TComboBox;
    chkLeft: TCheckBox;
    chkTop: TCheckBox;
    chkRight: TCheckBox;
    chkBottom: TCheckBox;
    cbbBackColor: TColorBox;
    btnOk: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

uses
  HCTableCell, HCCommon;

{$R *.dfm}

procedure TfrmBorderBackColor.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmBorderBackColor.SetHCView(const AHCView: THCView);
var
  vBorderSides: TBorderSides;
  vTableItem: THCTableItem;

  procedure SetCellBorderBackColor(const ARow, ACol: Integer);
  begin
    if cbbBackColor.Selected = HCTransparentColor then
      vTableItem.Cells[ARow, ACol].BackgroundColor := HCTransparentColor  //cbbBackColor.NoneColorColor
    else
      vTableItem.Cells[ARow, ACol].BackgroundColor := cbbBackColor.Selected;

    vBorderSides := vTableItem.Cells[ARow, ACol].BorderSides;

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

    vTableItem.Cells[ARow, ACol].BorderSides := vBorderSides;
  end;

  procedure ApplyAllTable;
  var
    vR, vC: Integer;
  begin
    for vR := 0 to vTableItem.RowCount - 1 do
    begin
      for vC := 0 to vTableItem.ColCount - 1 do
        SetCellBorderBackColor(vR, vC);
    end;
  end;

var
  vR, vC: Integer;
  vCell: THCTableCell;
begin
  vTableItem := AHCView.ActiveSection.ActiveData.GetCurItem as THCTableItem;

  if vTableItem.SelectCellRang.StartRow >= 0 then
    vCell := vTableItem.Cells[vTableItem.SelectCellRang.StartRow, vTableItem.SelectCellRang.StartCol]
  else
    vCell := vTableItem.Cells[0, 0];

  cbbBackColor.Selected := vCell.BackgroundColor;

  vBorderSides := vCell.BorderSides;
  chkLeft.Checked := cbsLeft in vBorderSides;
  chkTop.Checked := cbsTop in vBorderSides;
  chkRight.Checked := cbsRight in vBorderSides;
  chkBottom.Checked := cbsBottom in vBorderSides;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AHCView.BeginUpdate;
    try
      if cbbRang.ItemIndex = 0 then  // 单元格
      begin
        if vTableItem.SelectCellRang.EditCell then  // 在同一个单元格编辑
          SetCellBorderBackColor(vTableItem.SelectCellRang.StartRow, vTableItem.SelectCellRang.StartCol)
        else  // 多选或一个也没选
        begin
          if vTableItem.SelectCellRang.StartRow >= 0 then  // 多选
          begin
            for vR := vTableItem.SelectCellRang.StartRow to vTableItem.SelectCellRang.EndRow do
            begin
              for vC := vTableItem.SelectCellRang.StartCol to vTableItem.SelectCellRang.EndCol do
                SetCellBorderBackColor(vR, vC);
            end;
          end
          else  // 一个也没选，按整个表格处理
            ApplyAllTable;
        end;
      end
      else
        ApplyAllTable;  // 整个表格

      AHCView.Style.UpdateInfoRePaint;
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

end.
