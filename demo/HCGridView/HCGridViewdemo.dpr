program HCGridViewdemo;

uses
  Forms,
  frm_GricViewDemo in 'frm_GricViewDemo.pas' {Form9};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
