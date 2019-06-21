program HCGridViewdemo;

uses
  Forms,
  frm_GricViewDemo in 'frm_GricViewDemo.pas' {frmGridViewDemo},
  frm_PrintView in '..\HCView\frm_PrintView.pas' {frmPrintView},
  frm_Paragraph in '..\HCView\frm_Paragraph.pas' {frmParagraph},
  frm_ControlItemProperty in '..\HCView\frm_ControlItemProperty.pas' {frmControlItemProperty},
  frm_InsertTable in '..\HCView\frm_InsertTable.pas' {frmInsertTable},
  frm_PageSet in '..\HCView\frm_PageSet.pas' {frmPageSet},
  frm_Annotate in '..\HCView\frm_Annotate.pas' {frmAnnotate},
  frm_TableBorderBackColor in '..\HCView\frm_TableBorderBackColor.pas' {frmBorderBackColor},
  frm_TableProperty in '..\HCView\frm_TableProperty.pas' {frmTableProperty};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmGridViewDemo, frmGridViewDemo);
  Application.Run;
end.
