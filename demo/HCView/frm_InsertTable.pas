unit frm_InsertTable;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmInsertTable = class(TForm)
    edtRows: TEdit;
    edtCols: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    btnOk: TButton;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmInsertTable: TfrmInsertTable;

implementation

{$R *.dfm}

procedure TfrmInsertTable.btnOkClick(Sender: TObject);
var
  vValue: Integer;
begin
  if not TryStrToInt(edtRows.Text, vValue) then
    ShowMessage('请输入正确的行数！')
  else
  if not TryStrToInt(edtCols.Text, vValue) then
    ShowMessage('请输入正确的列数！')
  else
    Self.ModalResult := mrOk;
end;

end.
