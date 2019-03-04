unit frm_ControlItemProperty;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HCView, HCRectItem, ExtCtrls;

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
    pnlCombobox: TPanel;
    edtValue: TEdit;
    lbl5: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    btnSave: TButton;
    pnlDateTime: TPanel;
    cbbDTFormat: TComboBox;
    lstCombobox: TListBox;
    lbl4: TLabel;
    pnlRadioGroup: TPanel;
    lbl6: TLabel;
    edtRadioValue: TEdit;
    btnAddRadioItem: TButton;
    btnDeleteRadioItem: TButton;
    btnModRadioItem: TButton;
    lstRadioItem: TListBox;
    procedure btnOkClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lstComboboxClick(Sender: TObject);
    procedure btnAddRadioItemClick(Sender: TObject);
    procedure btnModRadioItemClick(Sender: TObject);
    procedure btnDeleteRadioItemClick(Sender: TObject);
    procedure lstRadioItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

uses
  HCEditItem, HCComboboxItem, HCDateTimePicker, HCRadioGroup, HCCommon;

{$R *.dfm}

procedure TfrmControlItemProperty.btnAddClick(Sender: TObject);
begin
  if edtValue.Text <> '' then
  begin
    lstCombobox.Items.Add(edtValue.Text);
    edtValue.Clear;
  end;
end;

procedure TfrmControlItemProperty.btnAddRadioItemClick(Sender: TObject);
begin
  if edtRadioValue.Text <> '' then
  begin
    lstRadioItem.Items.Add(edtRadioValue.Text);
    edtRadioValue.Clear;
  end;
end;

procedure TfrmControlItemProperty.btnDeleteClick(Sender: TObject);
begin
  lstCombobox.DeleteSelected;
end;

procedure TfrmControlItemProperty.btnDeleteRadioItemClick(Sender: TObject);
begin
  lstRadioItem.DeleteSelected;
end;

procedure TfrmControlItemProperty.btnModRadioItemClick(Sender: TObject);
begin
  lstRadioItem.Items[lstRadioItem.ItemIndex] := edtRadioValue.Text;
end;

procedure TfrmControlItemProperty.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmControlItemProperty.btnSaveClick(Sender: TObject);
begin
  lstCombobox.Items[lstCombobox.ItemIndex] := edtValue.Text;
end;

procedure TfrmControlItemProperty.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmControlItemProperty.lstComboboxClick(Sender: TObject);
begin
  if lstCombobox.ItemIndex >= 0 then
    edtValue.Text := lstCombobox.Items[lstCombobox.ItemIndex];
end;

procedure TfrmControlItemProperty.lstRadioItemClick(Sender: TObject);
begin
  if lstRadioItem.ItemIndex >= 0 then
    edtRadioValue.Text := lstRadioItem.Items[lstRadioItem.ItemIndex];
end;

procedure TfrmControlItemProperty.SetHCView(const AHCView: THCView);
var
  i: Integer;
  vControlItem: THCControlItem;
  vEditItem: THCEditItem;
  vCombobox: THCComboboxItem;
  vDateTimePicker: THCDateTimePicker;
  vRadioGroup: THCRadioGroup;
begin
  vControlItem := AHCView.ActiveSectionTopLevelData.GetCurItem as THCControlItem;

  chkAutoSize.Checked := vControlItem.AutoSize;
  edtWidth.Text := IntToStr(vControlItem.Width);
  edtHeight.Text := IntToStr(vControlItem.Height);

  if vControlItem is THCEditItem then  // EditItem
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

  if vControlItem is THCComboboxItem then  // ComboboxItem
  begin
    vCombobox := vControlItem as THCComboboxItem;
    lstCombobox.Items.Assign(vCombobox.Items);
  end
  else
  begin
    vCombobox := nil;
    pnlCombobox.Visible := False;
  end;

  if vControlItem is THCDateTimePicker then  // DateTime
  begin
    vDateTimePicker := vControlItem as THCDateTimePicker;
    cbbDTFormat.Text := vDateTimePicker.Format;
  end
  else
  begin
    vDateTimePicker := nil;
    pnlDateTime.Visible := False;
  end;

  if vControlItem is THCRadioGroup then
  begin
    vRadioGroup := vControlItem as THCRadioGroup;
    for i := 0 to vRadioGroup.Items.Count - 1 do
      lstRadioItem.Items.Add(vRadioGroup.Items[i].Text);
  end
  else
  begin
    vRadioGroup := nil;
    pnlRadioGroup.Visible := False;
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

    if vCombobox <> nil then
      vCombobox.Items.Assign(lstCombobox.Items);

    if vDateTimePicker <> nil then
      vDateTimePicker.Format := cbbDTFormat.Text;

    if vRadioGroup <> nil then
    begin
      vRadioGroup.Items.Clear;

      for i := 0 to lstRadioItem.Items.Count - 1 do
        vRadioGroup.AddItem(lstRadioItem.Items[i]);
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
