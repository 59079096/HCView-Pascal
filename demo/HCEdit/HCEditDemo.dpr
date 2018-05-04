program HCEditDemo;

uses
  Forms,
  frm_HCEditDemo in 'frm_HCEditDemo.pas' {frmHCEdit};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHCEdit, frmHCEdit);
  Application.Run;
end.
