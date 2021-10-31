unit frm_TableBorderBackColor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HCView, HCTableItem, StdCtrls, ExtCtrls;

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
  if cbbRang.ItemIndex = 0 then  // ��Ԫ��
  begin
    if FTableItem.SelectCellRang.EditCell then  // ��ͬһ����Ԫ��༭
      SetCellBorderBackColor(FTableItem.SelectCellRang.StartRow, FTableItem.SelectCellRang.StartCol)
    else  // ��ѡ��һ��Ҳûѡ
    begin
      if FTableItem.SelectCellRang.StartRow >= 0 then  // ��ѡ
      begin
        for vR := FTableItem.SelectCellRang.StartRow to FTableItem.SelectCellRang.EndRow do
        begin
          for vC := FTableItem.SelectCellRang.StartCol to FTableItem.SelectCellRang.EndCol do
            SetCellBorderBackColor(vR, vC);
        end;
      end
      else  // һ��Ҳûѡ�������������
        ApplyAllTable;
    end;
  end
  else
    ApplyAllTable;  // �������
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
