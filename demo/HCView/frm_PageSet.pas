unit frm_PageSet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HCView;

type
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
    cbbPageOrientation: TComboBox;
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
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
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
begin
  if cbbPaper.Text = 'A4' then
  begin
    edtWidth.Text := '210';
    edtHeight.Text := '297';
  end;
end;

procedure TfrmPageSet.SetHCView(const AHCView: THCView);
begin
  cbbPaper.ItemIndex := cbbPaper.Items.IndexOf(GetPaperSizeStr(AHCView.ActiveSection.PaperSize));
  if cbbPaper.ItemIndex < 0 then
    cbbPaper.ItemIndex := 0;
  edtWidth.Text := FloatToStr(AHCView.ActiveSection.PaperWidth);
  edtHeight.Text := FloatToStr(AHCView.ActiveSection.PaperHeight);

  edtTop.Text := FloatToStr(AHCView.ActiveSection.PaperMarginTop);
  edtLeft.Text := FloatToStr(AHCView.ActiveSection.PaperMarginLeft);
  edtRight.Text := FloatToStr(AHCView.ActiveSection.PaperMarginRight);
  edtBottom.Text := FloatToStr(AHCView.ActiveSection.PaperMarginBottom);

  chkSymmetryMargin.Checked := AHCView.ActiveSection.SymmetryMargin;

  if AHCView.ActiveSection.PageOrientation = TPageOrientation.cpoPortrait then
    cbbPageOrientation.ItemIndex := 0
  else
    cbbPageOrientation.ItemIndex := 1;

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
      AHCView.ActiveSection.PaperSize := DMPAPER_A4;
      AHCView.ActiveSection.PaperWidth := StrToFloat(edtWidth.Text);
      AHCView.ActiveSection.PaperHeight := StrToFloat(edtHeight.Text);

      AHCView.ActiveSection.PaperMarginTop := StrToFloat(edtTop.Text);
      AHCView.ActiveSection.PaperMarginLeft := StrToFloat(edtLeft.Text);
      AHCView.ActiveSection.PaperMarginRight := StrToFloat(edtRight.Text);
      AHCView.ActiveSection.PaperMarginBottom := StrToFloat(edtBottom.Text);

      AHCView.ActiveSection.SymmetryMargin := chkSymmetryMargin.Checked;

      if cbbPageOrientation.ItemIndex = 0 then
        AHCView.ActiveSection.PageOrientation := TPageOrientation.cpoPortrait
      else
        AHCView.ActiveSection.PageOrientation := TPageOrientation.cpoLandscape;

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

end.
