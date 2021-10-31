unit frm_PageSet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Generics.Collections, HCView;

type
  TPaperInfo = class
  public
    Size: Integer;
    SizeName: string;
    Width, Height: Single;
  end;

  TPaperInfos = class(TObjectList<TPaperInfo>)
  public
    procedure Append(const ASize: Integer; const ASizeName: string;
      const AWidth, AHeight: Single);
  end;

  TfrmPageSet = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    edtTop: TEdit;
    edtBottom: TEdit;
    edtLeft: TEdit;
    edtRight: TEdit;
    btnOk: TButton;
    lbl5: TLabel;
    edtWidth: TEdit;
    edtHeight: TEdit;
    lbl6: TLabel;
    lbl7: TLabel;
    cbbPaper: TComboBox;
    chkShowLineNo: TCheckBox;
    chkShowLineActiveMark: TCheckBox;
    chkShowUnderLine: TCheckBox;
    cbbPaperOrientation: TComboBox;
    lbl8: TLabel;
    chkPageNoVisible: TCheckBox;
    lbl9: TLabel;
    chkSymmetryMargin: TCheckBox;
    chkParaLastMark: TCheckBox;
    lbl10: TLabel;
    lbl11: TLabel;
    lbl12: TLabel;
    lbl13: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure cbbPaperChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FPaperInfos: TPaperInfos;
    function GetPaperInfoIndexByName(const AName: string): Integer;
  public
    { Public declarations }
    procedure SetView(const AHCView: THCView);
  end;

implementation

uses
  HCCommon;

{$R *.dfm}

procedure TfrmPageSet.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmPageSet.cbbPaperChange(Sender: TObject);
var
  vIndex: Integer;
begin
  vIndex := GetPaperInfoIndexByName(cbbPaper.Text);
  if vIndex > 0 then  // ��׼ֽ�Ŵ�С
  begin
    edtWidth.Text := FormatFloat('0.#', FPaperInfos[vIndex].Width);
    edtHeight.Text := FormatFloat('0.#', FPaperInfos[vIndex].Height);
    edtWidth.ReadOnly := True;
    edtHeight.ReadOnly := True;
  end
  else  // �Զ���ֽ�Ŵ�С
  begin
    edtWidth.ReadOnly := False;
    edtHeight.ReadOnly := False;
  end;
end;

procedure TfrmPageSet.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FPaperInfos := TPaperInfos.Create;
  FPaperInfos.Append(DMPAPER_USER, '�Զ���', 210, 297);
  FPaperInfos.Append(DMPAPER_A3, 'A3', 297, 420);
  FPaperInfos.Append(DMPAPER_A4, 'A4', 210, 297);
  FPaperInfos.Append(DMPAPER_A5, 'A5', 148, 210);
  FPaperInfos.Append(DMPAPER_B5, 'B5', 182, 257);
  FPaperInfos.Append(DMPAPER_HC_16K, '16K', 195, 271);

  for i := 0 to FPaperInfos.Count - 1 do
    cbbPaper.Items.Add(FPaperInfos[i].SizeName);
end;

procedure TfrmPageSet.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPaperInfos);
end;

function TfrmPageSet.GetPaperInfoIndexByName(const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPaperInfos.Count - 1 do
  begin
    if FPaperInfos[i].SizeName = AName then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TfrmPageSet.SetView(const AHCView: THCView);
var
  vIndex: Integer;
begin
  cbbPaper.ItemIndex := cbbPaper.Items.IndexOf(GetPaperSizeName(AHCView.ActiveSection.PaperSize));
  if cbbPaper.ItemIndex < 0 then  // �Զ���
    cbbPaper.ItemIndex := 0;

  edtWidth.ReadOnly := cbbPaper.ItemIndex > 0;
  edtHeight.ReadOnly := cbbPaper.ItemIndex > 0;

  if AHCView.ActiveSection.PaperOrientation = TPaperOrientation.cpoPortrait then
  begin
    cbbPaperOrientation.ItemIndex := 0;
    edtWidth.Text := FormatFloat('0.#', AHCView.ActiveSection.PaperWidth);
    edtHeight.Text := FormatFloat('0.#', AHCView.ActiveSection.PaperHeight);
  end
  else
  begin
    cbbPaperOrientation.ItemIndex := 1;
    edtWidth.Text := FormatFloat('0.#', AHCView.ActiveSection.PaperHeight);
    edtHeight.Text := FormatFloat('0.#', AHCView.ActiveSection.PaperWidth);
  end;

  edtTop.Text := FormatFloat('0.#', AHCView.ActiveSection.PaperMarginTop);
  edtLeft.Text := FormatFloat('0.#', AHCView.ActiveSection.PaperMarginLeft);
  edtRight.Text := FormatFloat('0.#', AHCView.ActiveSection.PaperMarginRight);
  edtBottom.Text := FormatFloat('0.#', AHCView.ActiveSection.PaperMarginBottom);

  chkSymmetryMargin.Checked := AHCView.ActiveSection.SymmetryMargin;

  chkPageNoVisible.Checked := AHCView.ActiveSection.PageNoVisible;
  chkParaLastMark.Checked := AHCView.Style.ShowParaLastMark;
  chkShowLineNo.Checked := AHCView.ShowLineNo;
  chkShowLineActiveMark.Checked := AHCView.ShowLineActiveMark;
  chkShowUnderLine.Checked := AHCView.ShowUnderLine;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AHCView.BeginUpdate;
    try
      vIndex := GetPaperInfoIndexByName(cbbPaper.Text);
      AHCView.ActiveSection.PaperSize := FPaperInfos[vIndex].Size;

      if cbbPaperOrientation.ItemIndex = 0 then  // ����
        AHCView.ActiveSection.PaperOrientation := TPaperOrientation.cpoPortrait
      else
        AHCView.ActiveSection.PaperOrientation := TPaperOrientation.cpoLandscape;

      if vIndex = 0 then  // �Զ���
      begin
        if cbbPaperOrientation.ItemIndex = 0 then  // ����
        begin
          AHCView.ActiveSection.PaperWidth := StrToFloat(edtWidth.Text);
          AHCView.ActiveSection.PaperHeight := StrToFloat(edtHeight.Text);
        end
        else
        begin
          AHCView.ActiveSection.PaperWidth := StrToFloat(edtHeight.Text);
          AHCView.ActiveSection.PaperHeight := StrToFloat(edtWidth.Text);
        end;
      end
      else
      begin
        if cbbPaperOrientation.ItemIndex = 0 then  // ����
        begin
          AHCView.ActiveSection.PaperWidth := FPaperInfos[vIndex].Width;
          AHCView.ActiveSection.PaperHeight := FPaperInfos[vIndex].Height;
        end
        else
        begin
          AHCView.ActiveSection.PaperWidth := FPaperInfos[vIndex].Height;
          AHCView.ActiveSection.PaperHeight := FPaperInfos[vIndex].Width;
        end;
      end;

      AHCView.ActiveSection.PaperMarginTop := StrToFloat(edtTop.Text);
      AHCView.ActiveSection.PaperMarginLeft := StrToFloat(edtLeft.Text);
      AHCView.ActiveSection.PaperMarginRight := StrToFloat(edtRight.Text);
      AHCView.ActiveSection.PaperMarginBottom := StrToFloat(edtBottom.Text);

      AHCView.ActiveSection.SymmetryMargin := chkSymmetryMargin.Checked;

      AHCView.ActiveSection.PageNoVisible := chkPageNoVisible.Checked;
      AHCView.Style.ShowParaLastMark := chkParaLastMark.Checked;
      AHCView.ShowLineNo := chkShowLineNo.Checked;
      AHCView.ShowLineActiveMark := chkShowLineActiveMark.Checked;
      AHCView.ShowUnderLine := chkShowUnderLine.Checked;
      AHCView.ResetActiveSectionMargin;
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

{ TPaperInfos }

procedure TPaperInfos.Append(const ASize: Integer; const ASizeName: string;
  const AWidth, AHeight: Single);
var
  vPaperInfo: TPaperInfo;
begin
  vPaperInfo := TPaperInfo.Create;
  vPaperInfo.Size := ASize;
  vPaperInfo.SizeName := ASizeName;
  vPaperInfo.Width := AWidth;
  vPaperInfo.Height := AHeight;

  Add(vPaperInfo);
end;

end.
