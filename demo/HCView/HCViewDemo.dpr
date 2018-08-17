program HCViewDemo;

uses
  Forms,
  frm_HCViewDemo in 'frm_HCViewDemo.pas' {frmHCViewDemo},
  frm_InsertTable in 'frm_InsertTable.pas' {frmInsertTable},
  frm_PageSet in 'frm_PageSet.pas' {frmPageSet},
  frm_Paragraph in 'frm_Paragraph.pas' {frmParagraph},
  frm_TableProperty in 'frm_TableProperty.pas' {frmTableProperty};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHCViewDemo, frmHCViewDemo);
  Application.CreateForm(TfrmInsertTable, frmInsertTable);
  Application.CreateForm(TfrmPageSet, frmPageSet);
  Application.CreateForm(TfrmParagraph, frmParagraph);
  Application.CreateForm(TfrmTableProperty, frmTableProperty);
  Application.Run;
end.
