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
    ud1: TUpDown;
    edtLineSpace: TEdit;
    btnOk: TButton;
    clrbxBG: TColorBox;
    cbbAlignHorz: TComboBox;
    cbbAlignVert: TComboBox;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

var
  frmParagraph: TfrmParagraph;

implementation

uses
  HCParaStyle;

{$R *.dfm}

procedure TfrmParagraph.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmParagraph.SetHCView(const AHCView: THCView);
begin
  edtLineSpace.Text := IntToStr(AHCView.Style.ParaStyles[AHCView.Style.CurParaNo].LineSpace);
  cbbAlignHorz.ItemIndex := Ord(AHCView.Style.ParaStyles[AHCView.Style.CurParaNo].AlignHorz);
  cbbAlignVert.ItemIndex := Ord(AHCView.Style.ParaStyles[AHCView.Style.CurParaNo].AlignVert);
  clrbxBG.Color := AHCView.Style.ParaStyles[AHCView.Style.CurParaNo].BackColor;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AHCView.BeginUpdate;
    try
      AHCView.ApplyParaLineSpace(StrToIntDef(edtLineSpace.Text, 8));
      AHCView.ApplyParaAlignHorz(TParaAlignHorz(cbbAlignHorz.ItemIndex));
      AHCView.ApplyParaAlignVert(TParaAlignVert(cbbAlignVert.ItemIndex));
      AHCView.ApplyParaBackColor(clrbxBG.Color);
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

end.
