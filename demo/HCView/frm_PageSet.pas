unit frm_PageSet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

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
    procedure btnOkClick(Sender: TObject);
    procedure cbbPaperChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPageSet: TfrmPageSet;

implementation

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

end.
