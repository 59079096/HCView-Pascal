unit frm_Paragraph;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, HCView;

type
  TfrmParagraph = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    btnOk: TButton;
    clrbxBG: TColorBox;
    cbbAlignHorz: TComboBox;
    cbbAlignVert: TComboBox;
    cbbSpaceMode: TComboBox;
    edtFirstIndent: TEdit;
    edtLeftIndent: TEdit;
    lbl7: TLabel;
    lbl8: TLabel;
    lbl9: TLabel;
    edtRightIndent: TEdit;
    lbl10: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure edtFirstIndentKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

uses
  HCParaStyle;

{$R *.dfm}

procedure TfrmParagraph.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmParagraph.edtFirstIndentKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
    Key := #0
end;

procedure TfrmParagraph.SetHCView(const AHCView: THCView);
var
  vParaStyle: THCParaStyle;
  vFirstIndent, vLeftIndent, vRightIndent: Integer;
  vReformatPara: Boolean;
begin
  vParaStyle := AHCView.Style.ParaStyles[AHCView.CurParaNo];

  cbbSpaceMode.ItemIndex := Ord(vParaStyle.LineSpaceMode);
  cbbAlignHorz.ItemIndex := Ord(vParaStyle.AlignHorz);
  cbbAlignVert.ItemIndex := Ord(vParaStyle.AlignVert);
  clrbxBG.Color := vParaStyle.BackColor;
  edtFirstIndent.Text := FormatFloat('0.#', vParaStyle.FirstIndent);
  edtLeftIndent.Text := FormatFloat('0.#', vParaStyle.LeftIndent);
  edtRightIndent.Text := FormatFloat('0.#', vParaStyle.RightIndent);

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AHCView.BeginUpdate;
    try
      AHCView.ApplyParaLineSpace(TParaLineSpaceMode(cbbSpaceMode.ItemIndex));
      AHCView.ApplyParaAlignHorz(TParaAlignHorz(cbbAlignHorz.ItemIndex));
      AHCView.ApplyParaAlignVert(TParaAlignVert(cbbAlignVert.ItemIndex));
      AHCView.ApplyParaBackColor(clrbxBG.Color);
      vFirstIndent := StrToIntDef(edtFirstIndent.Text, 0);
      vLeftIndent := StrToIntDef(edtLeftIndent.Text, 0);
      vRightIndent := StrToIntDef(edtRightIndent.Text, 0);

      vReformatPara := False;
      if vParaStyle.FirstIndent <> vFirstIndent then
      begin
        AHCView.ApplyParaFirstIndent(vFirstIndent);
        vReformatPara := True;
      end;

      if vParaStyle.LeftIndent <> vLeftIndent then
      begin
        AHCView.ApplyParaLeftIndent(vLeftIndent);
        vReformatPara := True;
      end;

      if vParaStyle.RightIndent <> vRightIndent then
      begin
        AHCView.ApplyParaRightIndent(vRightIndent);
        vReformatPara := True;
      end;

      if vReformatPara then
        AHCView.ActiveSection.ReFormatActiveParagraph;
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

end.
