{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2020-2-20             }
{                                                       }
{                文档高级滚动条实现单元                 }
{                                                       }
{*******************************************************}

unit HCStatusScrollBar;

interface

uses
  Windows, Classes, Controls, Graphics, Generics.Collections, HCScrollBar;

type
  THCStatus = class(TObject)
  private
    FWidth: Integer;
    FText: string;
    FOnChange: TNotifyEvent;
    procedure SetWidth(const Value: Integer);
    procedure SetText(const Value: string);
    procedure DoChange;
  public
    constructor Create; virtual;
    property Width: Integer read FWidth write SetWidth;
    property Text: string read FText write SetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THCStatusScrollBar = class(THCScrollBar)
  private
    FStatuses: TObjectList<THCStatus>;
    procedure DoStatusChange(Sender: TObject);
  protected
    //procedure MouseMove(Shift: TShiftState; X, Y: Integer); override; 可实现拖动让鼠标处的THCStatus变窄以显示后面看不见的THCStatus内容
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintToEx(const ACanvas: TCanvas); override;
    procedure AddStatus(const AWidth: Integer);
    property Statuses: TObjectList<THCStatus> read FStatuses;
  end;

implementation

{ THCStatusScrollBar }

procedure THCStatusScrollBar.AddStatus(const AWidth: Integer);
var
  vStatus: THCStatus;
  vWidth: Integer;
  i: Integer;
begin
  vStatus := THCStatus.Create;
  vStatus.OnChange := DoStatusChange;
  vStatus.Width := AWidth;
  FStatuses.Add(vStatus);

  vWidth := 0;
  for i := 0 to FStatuses.Count - 1 do
    vWidth := vWidth + FStatuses[i].Width;

  FLeftBlank := vWidth;
  Self.Invalidate;
end;

constructor THCStatusScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatuses := TObjectList<THCStatus>.Create;
end;

destructor THCStatusScrollBar.Destroy;
begin
  FStatuses.Free;
  inherited Destroy;
end;

procedure THCStatusScrollBar.DoStatusChange(Sender: TObject);
begin
  Self.Invalidate;
end;

procedure THCStatusScrollBar.PaintToEx(const ACanvas: TCanvas);
var
  i, vLeft: Integer;
  vRect: TRect;
  vText: string;
begin
  inherited PaintToEx(ACanvas);

  if Orientation = oriHorizontal then  // 水平滚动条
  begin
    if FStatuses.Count > 0 then
    begin
      ACanvas.Brush.Color := $006B5952;
      ACanvas.FillRect(Rect(2, 2, FLeftBtnRect.Left, Height - 2));
      ACanvas.Font.Size := 8;
      ACanvas.Font.Color := $D5D1D0;
      ACanvas.Font.Name := 'Arial';
      ACanvas.Font.Style := [];

      vLeft := 4;
      for i := 0 to FStatuses.Count - 1 do
      begin
        vText := FStatuses[i].Text;
        vRect := Bounds(vLeft, 2, FStatuses[i].Width, Height - 4);
        ACanvas.TextRect(vRect, vText, [tfLeft, tfVerticalCenter, tfSingleLine]);
        vLeft := vLeft + FStatuses[i].Width + 2
      end;
    end;
  end;
end;

{ THCStatus }

constructor THCStatus.Create;
begin
  FWidth := 100;
  FText := '';
end;

procedure THCStatus.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THCStatus.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    DoChange;
  end;
end;

procedure THCStatus.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange;
  end;
end;

end.
