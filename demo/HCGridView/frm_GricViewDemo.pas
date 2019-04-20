unit frm_GricViewDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HCGridView, HCTableCell, Grids;

type
  TForm9 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FGridView: THCGridView;
    procedure DoGridCellPaintBackground(const AColumn: THCTableCell;
      const ACanvas: TCanvas; const ARect: TRect);
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

procedure TForm9.DoGridCellPaintBackground(const AColumn: THCTableCell;
  const ACanvas: TCanvas; const ARect: TRect);
begin
  if (ARect.Left + FGridView.HorOffset > 250) then
  //if (ARect.Top + FGridView.VerOffset > 250) then
  begin
    ACanvas.Brush.Color := clYellow;
    ACanvas.FillRect(ARect);
  end;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  FGridView := THCGridView.CreateEx(nil, 80, 15);
  FGridView.OnCellPaintBackground := DoGridCellPaintBackground;
  FGridView.Align := alClient;
  FGridView.Parent := Self;
end;

procedure TForm9.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGridView);
end;

end.
