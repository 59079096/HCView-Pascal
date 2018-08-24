unit frm_SearchAndReplace;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, HCView;

type
  TfrmSearchAndReplace = class(TForm)
    pgTab: TPageControl;
    tsSearch: TTabSheet;
    tsReplace: TTabSheet;
    cbbSearch: TComboBox;
    btnSearchBackward: TButton;
    btnSearchForward: TButton;
    chkSearchCase: TCheckBox;
    chkSearchHight: TCheckBox;
    procedure btnSearchBackwardClick(Sender: TObject);
    procedure btnSearchForwardClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FHCView: THCView;
    procedure AddKey(const AText: string);
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

procedure TfrmSearchAndReplace.AddKey(const AText: string);
begin
  if cbbSearch.Items.IndexOf(AText) < 0 then
    cbbSearch.Items.Insert(0, AText);
end;

procedure TfrmSearchAndReplace.btnSearchBackwardClick(Sender: TObject);
begin
  if not FHCView.Search(cbbSearch.Text, False, chkSearchCase.Checked) then
    ShowMessage('向后查找完成，未找到关键字！');

  AddKey(cbbSearch.Text);
end;

procedure TfrmSearchAndReplace.btnSearchForwardClick(Sender: TObject);
begin
  if not FHCView.Search(cbbSearch.Text, True, chkSearchCase.Checked) then
    ShowMessage('向前查找完成，未找到关键字！');

  AddKey(cbbSearch.Text);
end;

procedure TfrmSearchAndReplace.FormShow(Sender: TObject);
begin
  if pgTab.ActivePage = tsSearch then
    cbbSearch.SetFocus;
end;

procedure TfrmSearchAndReplace.SetHCView(const AHCView: THCView);
begin
  FHCView := AHCView;
end;

procedure TfrmSearchAndReplace.ShowSearch;
begin
  pgTab.ActivePage := tsSearch;
  Self.Show;
end;

end.
