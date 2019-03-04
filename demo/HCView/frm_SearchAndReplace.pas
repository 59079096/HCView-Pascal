unit frm_SearchAndReplace;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, HCView;

type
  TfrmSearchAndReplace = class(TForm)
    chkSearchCase: TCheckBox;
    chkSearchHight: TCheckBox;
    cbbSearch: TComboBox;
    btnSearchForward: TButton;
    btnSearchBackward: TButton;
    cbbReplace: TComboBox;
    lbl1: TLabel;
    lbl2: TLabel;
    btnReplace: TButton;
    btn1: TButton;
    procedure btnSearchBackwardClick(Sender: TObject);
    procedure btnSearchForwardClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
  private
    { Private declarations }
    FHCView: THCView;
    procedure AddSearchKey(const AText: string);
    procedure AddReplaceKey(const AText: string);
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
    procedure ShowSearch;
  end;

var
  frmSearchAndReplace: TfrmSearchAndReplace;

implementation

{$R *.dfm}

{ TfrmSearchAndReplace }

procedure TfrmSearchAndReplace.AddReplaceKey(const AText: string);
begin
  if cbbReplace.Items.IndexOf(AText) < 0 then
    cbbReplace.Items.Insert(0, AText);
end;

procedure TfrmSearchAndReplace.AddSearchKey(const AText: string);
begin
  if cbbSearch.Items.IndexOf(AText) < 0 then
    cbbSearch.Items.Insert(0, AText);
end;

procedure TfrmSearchAndReplace.btnReplaceClick(Sender: TObject);
begin
  if FHCView.ActiveSection.SelectExists then
  begin
    FHCView.Replace(cbbReplace.Text);
    AddReplaceKey(cbbReplace.Text);
  end;

  if not FHCView.Search(cbbSearch.Text, False, chkSearchCase.Checked) then
    ShowMessage('向后查找完成，未找到关键字！');
end;

procedure TfrmSearchAndReplace.btnSearchBackwardClick(Sender: TObject);
begin
  if not FHCView.Search(cbbSearch.Text, False, chkSearchCase.Checked) then
    ShowMessage('向后查找完成，未找到关键字！');

  AddSearchKey(cbbSearch.Text);
end;

procedure TfrmSearchAndReplace.btnSearchForwardClick(Sender: TObject);
begin
  if not FHCView.Search(cbbSearch.Text, True, chkSearchCase.Checked) then
    ShowMessage('向前查找完成，未找到关键字！');

  AddSearchKey(cbbSearch.Text);
end;

procedure TfrmSearchAndReplace.FormShow(Sender: TObject);
begin
  cbbSearch.SetFocus;
end;

procedure TfrmSearchAndReplace.SetHCView(const AHCView: THCView);
begin
  FHCView := AHCView;
end;

procedure TfrmSearchAndReplace.ShowSearch;
begin
  Self.Show;
end;

end.
