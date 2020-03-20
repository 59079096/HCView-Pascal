unit HCControlReg;

interface

uses
  Classes, HCView;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HCControls', [THCView]);
end;

end.
