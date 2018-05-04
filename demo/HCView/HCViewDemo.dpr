program HCViewDemo;

uses
  Forms,
  frm_HCViewDemo in 'frm_HCViewDemo.pas' {frmEmrView},
  frm_InsertTable in 'frm_InsertTable.pas' {frmInsertTable},
  frm_PageSet in 'frm_PageSet.pas' {frmPageSet},
  frm_Paragraph in 'frm_Paragraph.pas' {frmParagraph};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmEmrView, frmEmrView);
  Application.CreateForm(TfrmInsertTable, frmInsertTable);
  Application.CreateForm(TfrmPageSet, frmPageSet);
  Application.CreateForm(TfrmParagraph, frmParagraph);
  Application.Run;
end.
