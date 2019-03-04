unit frm_TableProperty;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HCView, HCTableItem, ComCtrls, StdCtrls, ExtCtrls;

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
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtCellHPaddingChange(Sender: TObject);
    procedure btnBorderBackColorClick(Sender: TObject);
  private
    { Private declarations }
    FReFormt: Boolean;
    FHCView: THCView;
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

uses
  HCRichData, HCTableCell, frm_TableBorderBackColor;

{$R *.dfm}

{ TfrmTableProperty }

procedure TfrmTableProperty.btnBorderBackColorClick(Sender: TObject);
var
  vFrmBorderBackColor: TfrmBorderBackColor;
begin
  vFrmBorderBackColor := TfrmBorderBackColor.Create(Self);
  try
    vFrmBorderBackColor.SetHCView(FHCView);
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

procedure TfrmTableProperty.SetHCView(const AHCView: THCView);
var
  vR, vC, viValue{, vRowAlignIndex}: Integer;
  vData: THCRichData;
  vAlignVert: THCAlignVert;
  vTableItem: THCTableItem;
begin
  FHCView := AHCView;
  vData := FHCView.ActiveSection.ActiveData;
  vTableItem := vData.GetCurItem as THCTableItem;

  // 表格
  edtCellHPadding.Text := IntToStr(vTableItem.CellHPadding);
  edtCellVPadding.Text := IntToStr(vTableItem.CellVPadding);
  chkBorderVisible.Checked := vTableItem.BorderVisible;
  edtBorderWidth.Text := IntToStr(vTableItem.BorderWidth);

  // 行
  if vTableItem.SelectCellRang.StartRow >= 0 then
  begin
    tsRow.Caption := '行(' + IntToStr(vTableItem.SelectCellRang.StartRow + 1) + ')';
    if vTableItem.SelectCellRang.EndRow > 0 then
      tsRow.Caption := tsRow.Caption + ' - (' + IntToStr(vTableItem.SelectCellRang.EndRow + 1) + ')';

    edtRowHeight.Text := IntToStr(vTableItem.Rows[vTableItem.SelectCellRang.StartRow].Height);  // 行高
  end
  else
    tsRow.TabVisible := False;

  {vAlignVert := FTableItem.GetEditCell.AlignVert;
  cbbRowAlignVert.ItemIndex := Ord(vAlignVert) + 1;
  for i := 0 to FTableItem.Rows[FTableItem.SelectCellRang.StartRow].ColCount - 1 do
  begin
    if vAlignVert <> FTableItem.Cells[FTableItem.SelectCellRang.StartRow, i].AlignVert then  // 有不同
    begin
      cbbRowAlignVert.ItemIndex := 0;  // 自定义
      Break;
    end;
  end;
  vRowAlignIndex := cbbRowAlignVert.ItemIndex;}

  // 单元格
  if (vTableItem.SelectCellRang.StartRow >= 0) and (vTableItem.SelectCellRang.StartCol >= 0) then
  begin
    if vTableItem.SelectCellRang.EndRow >= 0 then  // 多选
    begin
      vAlignVert := vTableItem.Cells[vTableItem.SelectCellRang.StartRow,
        vTableItem.SelectCellRang.StartCol].AlignVert;

      tsCell.Caption := '单元格(' + IntToStr(vTableItem.SelectCellRang.StartRow + 1) + ','
        + IntToStr(vTableItem.SelectCellRang.StartCol + 1) + ') - ('
        + IntToStr(vTableItem.SelectCellRang.EndRow + 1) + ','
        + IntToStr(vTableItem.SelectCellRang.EndCol + 1) + ')';
    end
    else
    begin
      vAlignVert := vTableItem.GetEditCell.AlignVert;

      tsCell.Caption := '单元格(' + IntToStr(vTableItem.SelectCellRang.StartRow + 1) + ','
        + IntToStr(vTableItem.SelectCellRang.StartCol + 1) + ')';
    end;

    cbbCellAlignVert.ItemIndex := Ord(vAlignVert);
  end
  else
    tsCell.TabVisible := False;

  //
  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    FHCView.BeginUpdate;
    try
      // 表格
      vTableItem.CellHPadding := StrToIntDef(edtCellHPadding.Text, 5);
      vTableItem.CellVPadding := StrToIntDef(edtCellVPadding.Text, 0);
      vTableItem.BorderWidth := StrToIntDef(edtBorderWidth.Text, 1);
      vTableItem.BorderVisible := chkBorderVisible.Checked;

      // 行
      if (vTableItem.SelectCellRang.StartRow >= 0) and (TryStrToInt(edtRowHeight.Text, viValue)) then
      begin
        if vTableItem.SelectCellRang.EndRow > 0 then  // 有选中多行
        begin
          for vR := vTableItem.SelectCellRang.StartRow to vTableItem.SelectCellRang.EndRow do
            vTableItem.Rows[vR].Height := viValue;  // 行高
        end
        else  // 只选中一行
          vTableItem.Rows[vTableItem.SelectCellRang.StartRow].Height := viValue;  // 行高
      end;

      // 单元格
      if (vTableItem.SelectCellRang.StartRow >= 0) and (vTableItem.SelectCellRang.StartCol >= 0) then
      begin
        if vTableItem.SelectCellRang.EndCol > 0 then  // 有选中多个单元格
        begin
          for vR := vTableItem.SelectCellRang.StartRow to vTableItem.SelectCellRang.EndRow do
          begin
            for vC := vTableItem.SelectCellRang.StartCol to vTableItem.SelectCellRang.EndCol do
              vTableItem.Cells[vR, vC].AlignVert := THCAlignVert(cbbCellAlignVert.ItemIndex);
          end;
        end
        else
          vTableItem.GetEditCell.AlignVert := THCAlignVert(cbbCellAlignVert.ItemIndex);
      end;

      if FReFormt then
        FHCView.ActiveSection.ReFormatActiveItem;
    finally
      FHCView.EndUpdate;
    end;
  end;
end;

end.
