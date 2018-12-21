unit frm_HCEditDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, XPMan, ImgList, ComCtrls, ExtCtrls, StdCtrls, ToolWin,
  HCEdit, System.ImageList;

type
  TfrmHCEdit = class(TForm)
    tlbFontSize: TToolBar;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    btnprint: TToolButton;
    btn3: TToolButton;
    cbbZoom: TComboBox;
    btnAnnotation: TToolButton;
    btnSymmetryMargin: TToolButton;
    btn4: TToolButton;
    cbbFont: TComboBox;
    btn1: TToolButton;
    cbbFontSize: TComboBox;
    clrbxBackColor: TColorBox;
    clrbxFontColor: TColorBox;
    btnBold: TToolButton;
    btnItalic: TToolButton;
    btnUnderLine: TToolButton;
    btnStrikeOut: TToolButton;
    btnSuperScript: TToolButton;
    btnSubScript: TToolButton;
    btn2: TToolButton;
    btnAlignLeft: TToolButton;
    btnAlignCenter: TToolButton;
    btnAlignRight: TToolButton;
    btnAlignJustify: TToolButton;
    btnAlignScatter: TToolButton;
    btnLineSpace: TToolButton;
    btn9: TToolButton;
    btn10: TToolButton;
    btn11: TToolButton;
    btn12: TToolButton;
    statbar: TStatusBar;
    il1: TImageList;
    mm1: TMainMenu;
    mniN1: TMenuItem;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    mniSaveAs: TMenuItem;
    mniN5: TMenuItem;
    mniN11: TMenuItem;
    mniN3: TMenuItem;
    mniCut: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    mniN10: TMenuItem;
    mniN29: TMenuItem;
    mniN30: TMenuItem;
    mniN31: TMenuItem;
    mniN2: TMenuItem;
    mniInsertTable: TMenuItem;
    mniN16: TMenuItem;
    mniN12: TMenuItem;
    mnir1: TMenuItem;
    mniN4: TMenuItem;
    mniN9: TMenuItem;
    mniN13: TMenuItem;
    mniN14: TMenuItem;
    mniN15: TMenuItem;
    mniC1: TMenuItem;
    mniN27: TMenuItem;
    mniN28: TMenuItem;
    mniN32: TMenuItem;
    xpmnfst: TXPManifest;
    pmRichEdit: TPopupMenu;
    mniN7: TMenuItem;
    mniN6: TMenuItem;
    mniN8: TMenuItem;
    mniTable: TMenuItem;
    mniInsertRowTop: TMenuItem;
    mniInsertRowBottom: TMenuItem;
    mniInsertColLeft: TMenuItem;
    mniInsertColRight: TMenuItem;
    mniDeleteRow: TMenuItem;
    mniDeleteCol: TMenuItem;
    mniN25: TMenuItem;
    mniDisBorder: TMenuItem;
    mniN26: TMenuItem;
    pmLineSpace: TPopupMenu;
    mniLineSpace: TMenuItem;
    mniN17: TMenuItem;
    mniN21: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure mniN9Click(Sender: TObject);
    procedure mniInsertTableClick(Sender: TObject);
  private
    { Private declarations }
    FHCEdit: THCEdit;
  public
    { Public declarations }
  end;

var
  frmHCEdit: TfrmHCEdit;

implementation

uses
  HCTextStyle, HCCommon, HCImageItem, HCRichData;

{$R *.dfm}

procedure TfrmHCEdit.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FHCEdit.ApplyTextStyle(THCFontStyle.tsBold);
    1: FHCEdit.ApplyTextStyle(THCFontStyle.tsItalic);
    2: FHCEdit.ApplyTextStyle(THCFontStyle.tsUnderline);
    3: FHCEdit.ApplyTextStyle(THCFontStyle.tsStrikeOut);
    4: FHCEdit.ApplyTextStyle(THCFontStyle.tsSuperscript);
    5: FHCEdit.ApplyTextStyle(THCFontStyle.tsSubscript);
  end;
end;

procedure TfrmHCEdit.btnOpenClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '文件|*' + HC_EDIT_EXT;
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
      begin
        Application.ProcessMessages;  // 解决双击打开文件后，触发下层控件的Mousemove，Mouseup事件
        FHCEdit.LoadFromFile(vOpenDlg.FileName);
      end;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmHCEdit.btnSaveClick(Sender: TObject);
var
  vDlg: TSaveDialog;
begin
  vDlg := TSaveDialog.Create(Self);
  try
    vDlg.Filter := '文件|*' + HC_EDIT_EXT;
    vDlg.Execute;
    if vDlg.FileName <> '' then
    begin
      if ExtractFileName(vDlg.FileName) <> HC_EDIT_EXT then
        vDlg.FileName := vDlg.FileName + HC_EDIT_EXT;
      FHCEdit.SaveToFile(vDlg.FileName);
    end;
  finally
    vDlg.Free;
  end;
end;

procedure TfrmHCEdit.FormCreate(Sender: TObject);
begin
  FHCEdit := THCEdit.Create(Self);
  FHCEdit.Parent := Self;
  FHCEdit.Align := alClient;
end;

procedure TfrmHCEdit.FormDestroy(Sender: TObject);
begin
  FHCEdit.Free;
end;

procedure TfrmHCEdit.mniInsertTableClick(Sender: TObject);
begin
  FHCEdit.InsertTable(2, 2);
end;

procedure TfrmHCEdit.mniN9Click(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
  vImageItem: THCImageItem;
  vTopData: THCRichData;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '图像文件|*.bmp';//|*.jpg|*.jpge|*.png';
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
      begin
        vTopData := FHCEdit.TopLevelData;
        vImageItem := THCImageItem.Create(vTopData);
        vImageItem.LoadFromBmpFile(vOpenDlg.FileName);
        vImageItem.RestrainSize(vTopData.Width, vImageItem.Height);
        Application.ProcessMessages;  // 解决双击打开文件后，触发下层控件的Mousemove，Mouseup事件
        FHCEdit.InsertItem(vImageItem);
      end;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

end.
