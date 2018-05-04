unit frm_Paragraph;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

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
  end;

var
  frmParagraph: TfrmParagraph;

implementation

{$R *.dfm}

procedure TfrmParagraph.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

end.
