unit frm_Annotate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HCView, HCAnnotateData, HCAnnotateItem, StdCtrls;

type
  TfrmAnnotate = class(TForm)
    edtTitle: TEdit;
    mmoText: TMemo;
    btnOK: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    //procedure SetAnnotate(const AAnnotate: THCDataAnnotate);
    procedure SetView(const AView: THCView);
  end;

var
  frmAnnotate: TfrmAnnotate;

implementation

{$R *.dfm}

procedure TfrmAnnotate.btnOKClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

{procedure TfrmAnnotate.SetAnnotate(const AAnnotate: THCDataAnnotate);
begin
//  if Assigned(AAnnotate) then
//  begin
//    btnOK.Caption := '修改';
//    edtTitle.Text := AAnnotate.Title;
//    mmoText.Text := AAnnotate.Text;
//    Caption := '批注序号-' + IntToStr(AAnnotate.ID);
//    Self.ShowModal;
//    if Self.ModalResult = mrOk then
//    begin
//      AAnnotate.Title := edtTitle.Text;
//      AAnnotate.Text := mmoText.Text;
//    end;
//  end
//  else
  begin
    btnOK.Caption := '确定';
    Caption := '新建批注';
    edtTitle.Clear;
    mmoText.Clear;
    Self.ShowModal;
  end;
end;}

procedure TfrmAnnotate.SetView(const AView: THCView);
var
  vTopData: THCAnnotateData;
  vAnnotateItem: THCAnnotateItem;
begin
  if Assigned(AView) then  // 修改
  begin
    vTopData := AView.ActiveSectionTopLevelData as THCAnnotateData;
    if (vTopData.ActiveAnnotate.BeginNo < 0) then Exit;

    vAnnotateItem := vTopData.Items[vTopData.ActiveAnnotate.EndNo] as THCAnnotateItem;

    btnOK.Caption := '修改';
    edtTitle.Text := vAnnotateItem.Content.Title;
    mmoText.Text := vAnnotateItem.Content.Text;
    Caption := '批注序号-' + IntToStr(vAnnotateItem.ID);
    Self.ShowModal;
    if Self.ModalResult = mrOk then
    begin
      vAnnotateItem.Content.Title := edtTitle.Text;
      vAnnotateItem.Content.Text := mmoText.Text;

      vAnnotateItem := vTopData.Items[vTopData.ActiveAnnotate.BeginNo] as THCAnnotateItem;
      vAnnotateItem.Content.Title := edtTitle.Text;
      vAnnotateItem.Content.Text := mmoText.Text;
      AView.UpdateView;
    end;
  end
  else
  begin
    btnOK.Caption := '确定';
    Caption := '新建批注';
    edtTitle.Clear;
    mmoText.Clear;
    Self.ShowModal;
  end;
end;

end.
