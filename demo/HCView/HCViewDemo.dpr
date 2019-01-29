program HCViewDemo;

uses
  EMemLeaks,
  Forms,
  frm_HCViewDemo in 'frm_HCViewDemo.pas' {frmHCViewDemo},
  frm_InsertTable in 'frm_InsertTable.pas' {frmInsertTable},
  frm_PageSet in 'frm_PageSet.pas' {frmPageSet},
  frm_Paragraph in 'frm_Paragraph.pas' {frmParagraph},
  frm_TableProperty in 'frm_TableProperty.pas' {frmTableProperty},
  frm_SearchAndReplace in 'frm_SearchAndReplace.pas' {frmSearchAndReplace},
  frm_PrintView in 'frm_PrintView.pas' {frmPrintView},
  frm_ControlItemProperty in 'frm_ControlItemProperty.pas' {frmControlItemProperty},
  frm_TableBorderBackColor in 'frm_TableBorderBackColor.pas' {frmBorderBackColor},
  frm_Annotate in 'frm_Annotate.pas' {frmAnnotate};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHCViewDemo, frmHCViewDemo);
  Application.CreateForm(TfrmAnnotate, frmAnnotate);
  Application.Run;
end.
