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
  HCTextStyle;

{$R *.dfm}

procedure TfrmHCEdit.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FHCEdit.ApplyTextStyle(TFontStyleEx.tsBold);
    1: FHCEdit.ApplyTextStyle(TFontStyleEx.tsItalic);
    2: FHCEdit.ApplyTextStyle(TFontStyleEx.tsUnderline);
    3: FHCEdit.ApplyTextStyle(TFontStyleEx.tsStrikeOut);
    4: FHCEdit.ApplyTextStyle(TFontStyleEx.tsSuperscript);
    5: FHCEdit.ApplyTextStyle(TFontStyleEx.tsSubscript);
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

end.
