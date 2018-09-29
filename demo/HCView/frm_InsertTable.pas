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

implementation

{$R *.dfm}

procedure TfrmInsertTable.btnOkClick(Sender: TObject);
var
  vRowCount, vColCount: Integer;
begin
  if not TryStrToInt(edtRows.Text, vRowCount) then
    ShowMessage('请输入正确的行数！')
  else
  if not TryStrToInt(edtCols.Text, vColCount) then
    ShowMessage('请输入正确的列数！')
  else
  if vRowCount < 1 then
    ShowMessage('行数至少为1！')
  else
  if vRowCount > 256 then
    ShowMessage('行数不能超过256行！')
  else
  if vColCount < 1 then
    ShowMessage('列数至少为1！')
  else
  if vColCount > 32 then
    ShowMessage('列数不能超过32列！')
  else
    Self.ModalResult := mrOk;
end;

end.
