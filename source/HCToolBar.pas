unit HCToolBar;

interface

uses
  Windows, Classes, Controls, Graphics, Generics.Collections, ImgList, HCShape;

type
  THCToolBarControl = class(TObject)
  strict private
    FWidth, FHeight: Integer;
    FOnResize: TNotifyEvent;
    procedure DoResize;
  protected
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
  public
    Text: string;
    //Rect: TRect;
    Tag: Integer;
    constructor Create; virtual;
    procedure PaintTo(const ALeft, ATop: Integer; const ACanvas: TCanvas); virtual;
  published
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  THCCustomToolButton = class(THCToolBarControl)

  end;

  THCToolButton = class(THCCustomToolButton)

  end;

  THCToolControls = class(TObjectList<THCToolBarControl>)
  strict private
    FOnCountChange: TNotifyEvent;
  protected
    procedure Notify(const Value: THCToolBarControl; Action: TCollectionNotification); override;
  public
    property OnCountChange: TNotifyEvent read FOnCountChange write FOnCountChange;
  end;

  TUpdateViewEvent = procedure(const ARect: TRect; const ACanvas: TCanvas) of object;

  TToolBarControlPaint = procedure(const AControl: THCToolBarControl;
    const ALeft, ATop: Integer; const ACanvas: TCanvas) of object;

  TToolBarControlClick = procedure(const Sender: TObject; const AControl: THCToolBarControl) of object;

  THCToolBar = class(TObject)
  strict private
    FVisible: Boolean;
    FPadding: Byte;
    FLeft, FTop, FHotIndex, FActiveIndex: Integer;
    FControls: THCToolControls;
    FGraphic: TBitmap;
    FOnUpdateView: TUpdateViewEvent;
    FOnControlPaint: TToolBarControlPaint;
    FOnControlClick: TToolBarControlClick;

    procedure DoControlCountChange(Sender: TObject);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetControlAt(const X, Y: Integer): Integer;
  protected
    procedure SetVisible(const Value: Boolean); virtual;
    procedure SetActiveIndex(const Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure MouseEnter;
    procedure MouseLeave;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function AddControl(const AControl: THCToolBarControl): Integer;
    function AddButton: THCToolButton;
    function Bound: TRect;
    function ActiveControl: THCToolBarControl;
    procedure SetBounds;
    procedure UpdateView; overload;
    procedure UpdateView(const ARect: TRect); overload;
    procedure PaintTo(const ACanvas: TCanvas; const ALeft, ATop: Integer);
    property Controls: THCToolControls read FControls;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop Write FTop;
    property HotIndex: Integer read FHotIndex;
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Visible: Boolean read FVisible write SetVisible;
    property OnUpdateView: TUpdateViewEvent read FOnUpdateView write FOnUpdateView;
    property OnControlPaint: TToolBarControlPaint read FOnControlPaint write FOnControlPaint;
    property OnControlClick: TToolBarControlClick read FOnControlClick write FOnControlClick;
  end;

  THCTableToolBar = class(THCToolBar)
  protected
    procedure SetVisible(const Value: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  THCImageToolBar = class(THCToolBar)
  protected
    procedure SetVisible(const Value: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ THCToolBarControl }

constructor THCToolBarControl.Create;
begin
  FWidth := 20;
  FHeight := 20;
end;

procedure THCToolBarControl.DoResize;
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure THCToolBarControl.PaintTo(const ALeft, ATop: Integer;
  const ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := clBlue;
  ACanvas.Rectangle(Bounds(ALeft, ATop, FWidth, FHeight));
end;

procedure THCToolBarControl.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    DoResize;
  end;
end;

procedure THCToolBarControl.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoResize;
  end;
end;

{ THCToolControls }

procedure THCToolControls.Notify(const Value: THCToolBarControl;
  Action: TCollectionNotification);
begin
  inherited Notify(Value, Action);

  if (Action = cnAdded) or (Action = cnRemoved) then
  begin
    if Assigned(FOnCountChange) then
      FOnCountChange(Self);
  end;
end;

{ THCToolBar }

function THCToolBar.ActiveControl: THCToolBarControl;
begin
  if FActiveIndex < 0 then
    Result := nil
  else
    Result := FControls[FActiveIndex];
end;

function THCToolBar.AddButton: THCToolButton;
begin
  Result := THCToolButton.Create;
  Result.Width := FGraphic.Height;
  Result.Height := FGraphic.Height;
  FControls.Add(Result);
end;

function THCToolBar.AddControl(const AControl: THCToolBarControl): Integer;
begin
  Result := FControls.Add(AControl);
end;

function THCToolBar.Bound: TRect;
begin
  Result := Bounds(FLeft, FTop, FGraphic.Width, FGraphic.Height);
end;

constructor THCToolBar.Create;
begin
  FVisible := False;
  FPadding := 5;
  FHotIndex := -1;
  FActiveIndex := -1;
  FGraphic := TBitmap.Create;
  FGraphic.SetSize(10, 25);
  FControls := THCToolControls.Create;
  FControls.OnCountChange := DoControlCountChange;
end;

destructor THCToolBar.Destroy;
begin
  FControls.Free;
  FGraphic.Free;

  inherited Destroy;
end;

procedure THCToolBar.DoControlCountChange(Sender: TObject);
begin
  SetBounds;
end;

function THCToolBar.GetControlAt(const X, Y: Integer): Integer;
var
  i, vLeft: Integer;
  vPt: TPoint;
begin
  Result := -1;

  vPt.X := X;
  vPt.Y := Y;

  vLeft := FPadding;
  for i := 0 to FControls.Count - 1 do
  begin
    if PtInRect(Bounds(vLeft, 0, FControls[i].Width, FControls[i].Height), vPt) then
    begin
      Result := i;
      Break;
    end;

    vLeft := vLeft + FControls[i].Width + FPadding;
  end;
end;

function THCToolBar.GetHeight: Integer;
begin
  Result := FGraphic.Height;
end;

function THCToolBar.GetWidth: Integer;
begin
  Result := FGraphic.Width;
end;

procedure THCToolBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  ActiveIndex := GetControlAt(X, Y);
end;

procedure THCToolBar.MouseEnter;
begin

end;

procedure THCToolBar.MouseLeave;
begin
  if FHotIndex >= 0 then
  begin
    FHotIndex := -1;
    UpdateView;
  end;
end;

procedure THCToolBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vIndex: Integer;
begin
  vIndex := GetControlAt(X, Y);
  if FHotIndex <> vIndex then
  begin
    FHotIndex := vIndex;
    UpdateView;
  end;
end;

procedure THCToolBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (FHotIndex >= 0) and Assigned(FOnControlClick) then
    FOnControlClick(Self, FControls[FHotIndex]);
end;

procedure THCToolBar.PaintTo(const ACanvas: TCanvas; const ALeft, ATop: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  BitBlt(ACanvas.Handle, FLeft, FTop, FGraphic.Width, FGraphic.Height,
    FGraphic.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure THCToolBar.SetActiveIndex(const Value: Integer);
begin
  if FActiveIndex <> Value then
  begin
    FActiveIndex := Value;
    UpdateView;
  end;
end;

procedure THCToolBar.SetBounds;
var
  i, vWidth: Integer;
begin
  vWidth := FPadding;
  for i := 0 to FControls.Count - 1 do
    vWidth := vWidth + FControls[i].Width + FPadding;

  if FGraphic.Width <> vWidth then
  begin
    FGraphic.Width := vWidth;
    UpdateView;
  end;
end;

procedure THCToolBar.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    UpdateView;
  end;
end;

procedure THCToolBar.UpdateView(const ARect: TRect);
var
  i, vLeft: Integer;
begin
  FGraphic.Canvas.Brush.Color := clBtnFace;
  FGraphic.Canvas.FillRect(Bounds(0, 0, FGraphic.Width, FGraphic.Height));

  FGraphic.Canvas.Pen.Color := clBtnShadow;
  FGraphic.Canvas.MoveTo(0, 0);
  FGraphic.Canvas.LineTo(0, FGraphic.Height - 2);
  FGraphic.Canvas.LineTo(FGraphic.Width - 2, FGraphic.Height - 2);
  FGraphic.Canvas.LineTo(FGraphic.Width - 2, 0);
  FGraphic.Canvas.LineTo(0, 0);

  FGraphic.Canvas.Pen.Color := $00666666;
  FGraphic.Canvas.MoveTo(1, FGraphic.Height - 1);
  FGraphic.Canvas.LineTo(FGraphic.Width - 1, FGraphic.Height - 1);
  FGraphic.Canvas.LineTo(FGraphic.Width - 1, 1);

  vLeft := FPadding;
  for i := 0 to FControls.Count - 1 do
  begin
    if FControls[i] is THCCustomToolButton then
    begin
      if i = FActiveIndex then
      begin
        FGraphic.Canvas.Brush.Color := clHighlight;
        FGraphic.Canvas.FillRect(Bounds(vLeft, 1, FControls[i].Width, FGraphic.Height - 3));
      end
      else
      if i = FHotIndex then
      begin
        FGraphic.Canvas.Brush.Color := clHotLight;
        FGraphic.Canvas.FillRect(Bounds(vLeft, 1, FControls[i].Width, FGraphic.Height - 3));
      end;

      if Assigned(FOnControlPaint) then
        FOnControlPaint(FControls[i], vLeft, 0, FGraphic.Canvas)
      else
        FControls[i].PaintTo(vLeft, 0, FGraphic.Canvas);
    end;
    vLeft := vLeft + FControls[i].Width + FPadding;
  end;

  if Assigned(FOnUpdateView) then
    FOnUpdateView(ARect, FGraphic.Canvas);
end;

procedure THCToolBar.UpdateView;
begin
  UpdateView(Bounds(0, 0, FGraphic.Width, FGraphic.Height));
end;

{ THCImageToolBar }

constructor THCImageToolBar.Create;
var
  vButton: THCToolButton;
begin
  inherited Create;
  // 鼠标箭头
  vButton := Self.AddButton;
  vButton.Tag := 0;
  // 直线
  vButton := Self.AddButton;
  vButton.Tag := Ord(THCShapeStyle.hssLine);
  // 矩形
  vButton := Self.AddButton;
  vButton.Tag := Ord(THCShapeStyle.hssRectangle);
  // 椭圆
  vButton := Self.AddButton;
  vButton.Tag := Ord(THCShapeStyle.hssEllipse);
  // 多边形
  vButton := Self.AddButton;
  vButton.Tag := Ord(THCShapeStyle.hssPolygon);

  {vButton := Self.AddButton;
  //vButton.Tag := Ord(THCShapeStyle.itsPolygon);
  vButton.OnClick := DoButtonClick;

  vButton := Self.AddButton;
  //vButton.Tag := Ord(THCShapeStyle.itsPolygon);
  vButton.OnClick := DoButtonClick;

  vButton := Self.AddButton;
  //vButton.Tag := Ord(THCShapeStyle.itsPolygon);
  vButton.OnClick := DoButtonClick;

  vButton := Self.AddButton;
  //vButton.Tag := Ord(THCShapeStyle.itsPolygon);
  vButton.OnClick := DoButtonClick;

  vButton := Self.AddButton;
  //vButton.Tag := Ord(THCShapeStyle.itsPolygon);
  vButton.OnClick := DoButtonClick;}
end;

destructor THCImageToolBar.Destroy;
begin

  inherited Destroy;
end;

procedure THCImageToolBar.SetVisible(const Value: Boolean);
begin
  if Visible <> Value then
  begin
    inherited SetVisible(Value);
    if Value then
      ActiveIndex := 0;
  end;
end;

{ THCTableToolBar }

constructor THCTableToolBar.Create;
var
  vButton: THCToolButton;
begin
  inherited Create;
  vButton := Self.AddButton;
  vButton.Tag := 9;
  //vButton.OnClick := DoButtonClick;
end;

destructor THCTableToolBar.Destroy;
begin

  inherited;
end;

procedure THCTableToolBar.SetVisible(const Value: Boolean);
begin
  if Visible <> Value then
  begin
    inherited SetVisible(Value);
    if Value then
      ActiveIndex := -1;
  end;
end;

end.
