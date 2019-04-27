program HCGridViewdemo;

uses
  Forms,
  frm_GricViewDemo in 'frm_GricViewDemo.pas' {Form9},
  frm_PrintView in '..\HCView\frm_PrintView.pas' {frmPrintView},
  frm_Paragraph in '..\HCView\frm_Paragraph.pas' {frmParagraph},
  frm_ControlItemProperty in '..\HCView\frm_ControlItemProperty.pas' {frmControlItemProperty},
  frm_InsertTable in '..\HCView\frm_InsertTable.pas' {frmInsertTable},
  frm_PageSet in '..\HCView\frm_PageSet.pas' {frmPageSet},
  frm_Annotate in '..\HCView\frm_Annotate.pas' {frmAnnotate};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
