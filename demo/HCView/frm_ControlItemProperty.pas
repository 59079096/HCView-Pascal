unit frm_ControlItemProperty;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, HCView, HCRectItem,
  Vcl.ExtCtrls;

type
  TfrmControlItemProperty = class(TForm)
    pnlSize: TPanel;
    chkAutoSize: TCheckBox;
    edtWidth: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    edtHeight: TEdit;
    pnlBorder: TPanel;
    pnl1: TPanel;
    btnOk: TButton;
    chkBorderTop: TCheckBox;
    chkBorderLeft: TCheckBox;
    chkBorderRight: TCheckBox;
    chkBorderBottom: TCheckBox;
    lbl3: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

uses
  HCEditItem, HCCommon;

{$R *.dfm}

procedure TfrmControlItemProperty.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmControlItemProperty.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmControlItemProperty.SetHCView(const AHCView: THCView);
var
  vControlItem: THCControlItem;
  vEditItem: THCEditItem;
begin
  vControlItem := AHCView.ActiveSectionTopLevelData.GetCurItem as THCControlItem;

  chkAutoSize.Checked := vControlItem.AutoSize;
  edtWidth.Text := IntToStr(vControlItem.Width);
  edtHeight.Text := IntToStr(vControlItem.Height);

  if vControlItem is THCEditItem then
  begin
    vEditItem := vControlItem as THCEditItem;
    chkBorderLeft.Checked := cbsLeft in vEditItem.BorderSides;
    chkBorderTop.Checked := cbsTop in vEditItem.BorderSides;
    chkBorderRight.Checked := cbsRight in vEditItem.BorderSides;
    chkBorderBottom.Checked := cbsBottom in vEditItem.BorderSides;
  end
  else
  begin
    vEditItem := nil;
    pnlBorder.Visible := False;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    vControlItem.AutoSize := chkAutoSize.Checked;
    if not chkAutoSize.Checked then  // 自定义大小
    begin
      vControlItem.Width := StrToIntDef(edtWidth.Text, vControlItem.Width);
      vControlItem.Height := StrToIntDef(edtHeight.Text, vControlItem.Height);
    end;

    if vEditItem <> nil then
    begin
      if chkBorderLeft.Checked then
        vEditItem.BorderSides := vEditItem.BorderSides + [cbsLeft]
      else
        vEditItem.BorderSides := vEditItem.BorderSides - [cbsLeft];

      if chkBorderTop.Checked then
        vEditItem.BorderSides := vEditItem.BorderSides + [cbsTop]
      else
        vEditItem.BorderSides := vEditItem.BorderSides - [cbsTop];

      if chkBorderRight.Checked then
        vEditItem.BorderSides := vEditItem.BorderSides + [cbsRight]
      else
        vEditItem.BorderSides := vEditItem.BorderSides - [cbsRight];

      if chkBorderBottom.Checked then
        vEditItem.BorderSides := vEditItem.BorderSides + [cbsBottom]
      else
        vEditItem.BorderSides := vEditItem.BorderSides - [cbsBottom];
    end;

    AHCView.BeginUpdate;
    try
      AHCView.ActiveSection.ReFormatActiveItem;
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

end.
