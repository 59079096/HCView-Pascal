unit HCGridView;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, Generics.Collections, Messages,
  HCScrollBar, HCRichScrollBar, HCTableCell, HCStyle, HCItem;

type
  TGridColumn = class;

  THCColumnPaintEvent = procedure(const AColumn: TGridColumn;
    const ACanvas: TCanvas; const ARect: TRect) of object;

  TGridColumn = class(THCTableCell)
  private
    FOnPaintBackground: THCColumnPaintEvent;
  public
    procedure PaintTo(const ACanvas: TCanvas; const ALeft, ATop, AWidth, AHeight: Integer;
      const APaintInfo: TPaintInfo);
    property OnPaintBackground: THCColumnPaintEvent read FOnPaintBackground write FOnPaintBackground;
  end;

  THCRowColumnPaintBackground = procedure(Sender: TObject; const AColumn: TGridColumn;
    const ACanvas: TCanvas; const ARect: TRect) of object;

  TGridRow = class(TObjectList<TGridColumn>)
  strict private
    FHeight: Integer;
    FOnColumnPaintBackground: THCRowColumnPaintBackground;
    procedure DoColumnPaintBackground(const AColumn: TGridColumn;
      const ACanvas: TCanvas; const ARect: TRect);
  protected
    procedure Notify(const AColumn: TGridColumn; Action: TCollectionNotification); override;
  public
    constructor Create(const AStyle: THCStyle; const AColCount: Integer); virtual;
    destructor Destroy; override;
    property Height: Integer read FHeight write FHeight;
    /// <summary> 行中有列绘制背景 </summary>
    property OnColumnPaintBackground: THCRowColumnPaintBackground read FOnColumnPaintBackground write FOnColumnPaintBackground;
  end;

  THCSelectInfo = class(TObject)
  strict private
    FStartRow, FStartCol, FEndRow, FEndCol: Integer;
  public
    constructor Create; virtual;
    procedure Initilaze;
    procedure InitilazeEnd;
    procedure SetStart(const ARow, ACol: Integer);
    procedure SetEnd(const ARow, ACol: Integer);
    property StartRow: Integer read FStartRow write FStartRow;
    property StartCol: Integer read FStartCol write FStartCol;
    property EndRow: Integer read FEndRow write FEndRow;
    property EndCol: Integer read FEndCol write FEndCol;
  end;

  THCGridPaintEvent = procedure(const ACanvas: TCanvas; const ARect: TRect) of object;

  THCCustomGridView = class(TCustomControl)
  strict private
    FStyle: THCStyle;
    FBitmap: TBitmap;
    FHScrollBar: THCScrollBar;
    FVScrollBar: THCRichScrollBar;
    FActiveCell: TGridColumn;
    FRows: TObjectList<TGridRow>;
    FColWidths: TList<Integer>;
    FColDefaultWidth, FViewWidth, FViewHeight,
    FDispFirstRow, FDispLastRow, FDispFirstCol, FDispLastCol: Integer;
    FOnVerScroll, FOnHorScroll: TNotifyEvent;
    FSelectInfo: THCSelectInfo;
    FOnPaintBackground: THCGridPaintEvent;
    FOnCellPaintBackground: THCColumnPaintEvent;
    procedure DoHorScroll(Sender: TObject; ScrollCode: TScrollCode;
      const ScrollPos: Integer);
    procedure DoVerScroll(Sender: TObject; ScrollCode: TScrollCode;
      const ScrollPos: Integer);
    function GetCell(const ARow, ACol: Integer): TGridColumn;
    function GetRowAt(const X, Y: Integer): TGridRow;
    function GetRowIndexAt(const X, Y: Integer): Integer;
    function GetColIndexAt(const X, Y: Integer): Integer;
    procedure GetCellIndex(const X, Y: Integer; var ARow, ACol: Integer);
    function GetCellAt(const X, Y: Integer): TGridColumn;
    function GetHorOffset: Integer;
    function GetVerOffset: Integer;

    procedure GetViewWidth;
    procedure GetViewHeight;
    procedure CalcDisplayCellRange;
    procedure CalcScrollRang;
    function GetViewRect: TRect;
    procedure PaintTo(const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure UpdateView; overload;
    procedure UpdateView(const ARect: TRect); overload;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure Resize; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure Paint; override;
    /// <summary> 有行添加到Rows管理器 </summary>
    procedure DoRowNotify(Sender: TObject; const ARow: TGridRow;
      Action: TCollectionNotification);

    /// <summary> 单元格绘制背景 </summary>
    procedure DoCellPaintBackground(Sender: TObject; const AColumn: TGridColumn;
      const ACanvas: TCanvas; const ARect: TRect);
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const ARowCount, AColCount: Integer); overload;
    destructor Destroy; override;
    function ContentHeight: Integer;
    function ContentWidth: Integer;
    property Cell[const Row, Col: Integer]: TGridColumn read GetCell;
    property HorOffset: Integer read GetHorOffset;
    property VerOffset: Integer read GetVerOffset;
    property OnPaintBackground: THCGridPaintEvent read FOnPaintBackground write FOnPaintBackground;
    property OnCellPaintBackground: THCColumnPaintEvent read FOnCellPaintBackground write FOnCellPaintBackground;
    /// <summary> 垂直滚动条滚动时触发 </summary>
    property OnVerScroll: TNotifyEvent read FOnVerScroll write FOnVerScroll;

    /// <summary> 水平滚动条滚动时触发 </summary>
    property OnHorScroll: TNotifyEvent read FOnHorScroll write FOnHorScroll;
  end;

  THCGridView = class(THCCustomGridView)
  public
    property Color;
    property OnMouseWheel;
  end;

implementation

{ THCCustomGridView }

constructor THCCustomGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBitmap := TBitmap.Create;
  Self.Color := clRed;
  FColDefaultWidth := 50;
  FActiveCell := nil;
  FDispFirstRow := -1;
  FDispLastRow := -1;
  FDispFirstCol := -1;
  FDispLastCol := -1;

  FStyle := THCStyle.CreateEx(True, True);
  //FStyle.OnInvalidateRect := DoStyleInvalidateRect;

  FRows := TObjectList<TGridRow>.Create;
  FRows.OnNotify := DoRowNotify;
  FColWidths := TList<Integer>.Create;
  FSelectInfo := THCSelectInfo.Create;
  FSelectInfo.Initilaze;

  FHScrollBar := THCScrollBar.Create(Self);
  FHScrollBar.OnScroll := DoHorScroll;
  FHScrollBar.Parent := Self;

  FVScrollBar := THCRichScrollBar.Create(Self);
  FVScrollBar.Orientation := TOrientation.oriVertical;
  FVScrollBar.OnScroll := DoVerScroll;
  FVScrollBar.Parent := Self;
end;

procedure THCCustomGridView.CalcDisplayCellRange;
var
  vPos, i: Integer;
begin
  FDispFirstRow := -1;
  FDispLastRow := -1;
  FDispFirstCol := -1;
  FDispLastCol := -1;

  vPos := 0;
  for i := 0 to FRows.Count - 1 do
  begin
    if (FDispFirstRow < 0) and (vPos + FRows[i].Height - 1 > FVScrollBar.Position) then
      FDispFirstRow := i;

    if vPos - FVScrollBar.Position >= Self.Height then
    begin
      FDispLastRow := i;
      Break;
    end;

    vPos := vPos + FRows[i].Height - 1;
  end;

  if FDispLastRow < 0 then
    FDispLastRow := FRows.Count - 1;

  vPos := 0;
  for i := 0 to FColWidths.Count - 1 do
  begin
    if (FDispFirstCol < 0) and (vPos + FColWidths[i] - 1 > FHScrollBar.Position) then
      FDispFirstCol := i;

    if vPos - FHScrollBar.Position >= Self.Width then
    begin
      FDispLastCol := i;
      Break;
    end;

    vPos := vPos + FColWidths[i] - 1;
  end;

  if FDispLastCol < 0 then
    FDispLastCol := FColWidths.Count - 1;
end;

procedure THCCustomGridView.CalcScrollRang;
begin
  FVScrollBar.Max := ContentHeight;
  FHScrollBar.Max := ContentWidth;
end;

function THCCustomGridView.ContentHeight: Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to FRows.Count - 1 do
    Result := Result + FRows[i].Height - 1;
end;

function THCCustomGridView.ContentWidth: Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to FColWidths.Count - 1 do
    Result := Result + FColWidths[i] - 1;
end;

constructor THCCustomGridView.Create(AOwner: TComponent; const ARowCount,
  AColCount: Integer);
var
  i: Integer;
  vRow: TGridRow;
begin
  Create(AOwner);

  for i := 0 to ARowCount - 1 do
  begin
    vRow := TGridRow.Create(FStyle, AColCount);
    FRows.Add(vRow);
  end;

  for i := 0 to AColCount - 1 do
    FColWidths.Add(FColDefaultWidth);
end;

destructor THCCustomGridView.Destroy;
begin
  FreeAndNil(FRows);
  FreeAndNil(FColWidths);
  FreeAndNil(FSelectInfo);
  FreeAndNil(FBitmap);
  FreeAndNil(FHScrollBar);
  FreeAndNil(FVScrollBar);
  FreeAndNil(FStyle);
  inherited Destroy;
end;

procedure THCCustomGridView.DoCellPaintBackground(Sender: TObject;
  const AColumn: TGridColumn; const ACanvas: TCanvas; const ARect: TRect);
begin
  if Assigned(FOnCellPaintBackground) then
    FOnCellPaintBackground(AColumn, ACanvas, ARect);
end;

procedure THCCustomGridView.DoHorScroll(Sender: TObject;
  ScrollCode: TScrollCode; const ScrollPos: Integer);
begin
  UpdateView;
  if Assigned(FOnHorScroll) then
    FOnHorScroll(Self);
end;

function THCCustomGridView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if ssCtrl in Shift then
    FHScrollBar.Position := FHScrollBar.Position - WheelDelta
  else
    FVScrollBar.Position := FVScrollBar.Position - WheelDelta;

  Result := True;
end;

procedure THCCustomGridView.DoRowNotify(Sender: TObject; const ARow: TGridRow;
  Action: TCollectionNotification);
begin
  if Action = cnAdded then
    ARow.OnColumnPaintBackground := DoCellPaintBackground;
end;

procedure THCCustomGridView.DoVerScroll(Sender: TObject;
  ScrollCode: TScrollCode; const ScrollPos: Integer);
begin
  UpdateView;
  if Assigned(FOnVerScroll) then
    FOnVerScroll(Self);
end;

function THCCustomGridView.GetCell(const ARow, ACol: Integer): TGridColumn;
begin
  Result := FRows[ARow][ACol];
end;

function THCCustomGridView.GetCellAt(const X, Y: Integer): TGridColumn;
var
  i, vRow, vCol: Integer;
begin
  Result := nil;

  vRow := GetRowIndexAt(X, Y);
  if vRow >= 0 then
  begin
    vCol := GetColIndexAt(X, Y);
    if vCol >= 0 then
      Result := FRows[vRow][vCol];
  end;
end;

procedure THCCustomGridView.GetCellIndex(const X, Y: Integer; var ARow,
  ACol: Integer);
begin
  ARow := GetRowIndexAt(X, Y);
  ACol := GetColIndexAt(X, Y);
end;

function THCCustomGridView.GetColIndexAt(const X, Y: Integer): Integer;
var
  i, vLeft: Integer;
begin
  Result := -1;

  vLeft := 0;
  for i := 0 to FColWidths.Count - 1 do
  begin
    if (X > vLeft) and (X < vLeft + FColWidths[i]) then
    begin
      Result := i;
      Break;
    end;

    vLeft := vLeft + FColWidths[i] - 1;
  end;
end;

function THCCustomGridView.GetHorOffset: Integer;
begin
  Result := FHScrollBar.Position;
end;

function THCCustomGridView.GetRowAt(const X, Y: Integer): TGridRow;
var
  vRow: Integer;
begin
  Result := nil;
  vRow := GetRowIndexAt(X, Y);
  if vRow >= 0 then
    Result := FRows[vRow];
end;

function THCCustomGridView.GetRowIndexAt(const X, Y: Integer): Integer;
var
  i, vTop: Integer;
begin
  Result := -1;

  vTop := 0;
  for i := 0 to FRows.Count - 1 do
  begin
    if (Y > vTop) and (Y < vTop + FRows[i].Height) then
    begin
      Result := i;
      Break;
    end;

    vTop := vTop + FRows[i].Height - 1;
  end;
end;

function THCCustomGridView.GetVerOffset: Integer;
begin
  Result := FVScrollBar.Position;
end;

procedure THCCustomGridView.GetViewHeight;
begin
  if FHScrollBar.Visible then
    FViewHeight := Height - FHScrollBar.Height
  else
    FViewHeight := Height;
end;

function THCCustomGridView.GetViewRect: TRect;
begin
  Result := Bounds(0, 0, FViewWidth, FViewHeight);
end;

procedure THCCustomGridView.GetViewWidth;
begin
  if FVScrollBar.Visible then
    FViewWidth := Width - FVScrollBar.Width
  else
    FViewWidth := Width;
end;

procedure THCCustomGridView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FActiveCell) then
    FActiveCell.CellData.KeyDown(Key, Shift);

  inherited KeyDown(Key, Shift);
end;

procedure THCCustomGridView.KeyPress(var Key: Char);
begin
  if Assigned(FActiveCell) then
    FActiveCell.CellData.KeyPress(Key);

  inherited KeyPress(Key);
end;

procedure THCCustomGridView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FActiveCell) then
    FActiveCell.CellData.KeyUp(Key, Shift);

  inherited KeyUp(Key, Shift);
end;

procedure THCCustomGridView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  vRow, vCol: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  FActiveCell := nil;
  FSelectInfo.InitilazeEnd;
  GetCellIndex(X, Y, vRow, vCol);
  if (vRow <> FSelectInfo.StartRow) or (vCol <> FSelectInfo.StartCol) then
  begin
    FSelectInfo.SetStart(vRow, vCol);
    FActiveCell := GetCellAt(X, Y);
  end;
end;

procedure THCCustomGridView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

end;

procedure THCCustomGridView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);;

end;

procedure THCCustomGridView.Paint;
begin
  inherited Paint;
  BitBlt(Canvas.Handle, 0, 0, FViewWidth, FViewHeight,
    FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure THCCustomGridView.PaintTo(const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  vRect: TRect;
  vR, vC, vLeft, vTop, vPaintLeft: Integer;
begin
  vRect := GetViewRect;
  if Assigned(FOnPaintBackground) then
    FOnPaintBackground(ACanvas, vRect)
  else
  begin
    ACanvas.Brush.Color := Self.Color;
    ACanvas.FillRect(vRect);
  end;
  //
  ACanvas.Pen.Color := clBlack;
  vPaintLeft := -FHScrollBar.Position;
  vTop := -FVScrollBar.Position;

  for vR := 0 to FDispFirstRow - 1 do
    vTop := vTop + FRows[vR].Height - 1;

  for vC := 0 to FDispFirstCol - 1 do
    vPaintLeft := vPaintLeft + FColWidths[vC] - 1;

  vLeft := vPaintLeft;
  for vR := FDispFirstRow to FDispLastRow do
  begin
    for vC := FDispFirstCol to FDispLastCol do
    begin
      Cell[vR, vC].PaintTo(ACanvas, vLeft, vTop, FColWidths[vC], FRows[vR].Height,
        APaintInfo);
      //ACanvas.TextOut(vLeft + 2, vTop + 2, IntToStr(vC));
      vLeft := vLeft + FColWidths[vC] - 1;
    end;

    ACanvas.TextOut(0, vTop, IntToStr(vR));

    vTop := vTop + FRows[vR].Height - 1;
    vLeft := vPaintLeft;
  end;
end;

procedure THCCustomGridView.Resize;
begin
  inherited Resize;
  GetViewWidth;
  GetViewHeight;
  FBitmap.SetSize(FViewWidth, FViewHeight);

  FVScrollBar.Left := Width - FVScrollBar.Width;
  FVScrollBar.Height := Height - FHScrollBar.Height;

  FHScrollBar.Top := Height - FHScrollBar.Height;
  FHScrollBar.Width := Width - FVScrollBar.Width;
  CalcScrollRang;
  UpdateView;
end;

procedure THCCustomGridView.UpdateView(const ARect: TRect);
var
  vPaintInfo: TPaintInfo;
begin
  CalcDisplayCellRange;

  FBitmap.Canvas.Lock;
  try
    IntersectClipRect(FBitmap.Canvas.Handle, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    vPaintInfo := TPaintInfo.Create;
    try
      vPaintInfo.ScaleX := 1;
      vPaintInfo.ScaleY := 1;
      vPaintInfo.Zoom := 1;
      vPaintInfo.WindowWidth := FViewWidth;
      vPaintInfo.WindowHeight := FViewHeight;

      PaintTo(FBitmap.Canvas, vPaintInfo);
    finally
      vPaintInfo.Free;
    end;
  finally
    FBitmap.Canvas.Unlock;
  end;

  BitBlt(Canvas.Handle, ARect.Left, ARect.Top,
    ARect.Right - ARect.Left, ARect.Bottom - ARect.Top,
    FBitmap.Canvas.Handle, ARect.Left, ARect.Top, SRCCOPY);
  InvalidateRect(Self.Handle, ARect, False);
  //UpdateWindow(Self.Handle);
end;

procedure THCCustomGridView.UpdateView;
begin
  UpdateView(GetViewRect);
end;

procedure THCCustomGridView.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      begin
        if not (csDesigning in ComponentState) and not Focused then
          Self.SetFocus;
      end;
  end;
end;

{ TGridRow }

constructor TGridRow.Create(const AStyle: THCStyle; const AColCount: Integer);
var
  i: Integer;
  vColumn: TGridColumn;
begin
  inherited Create(True);

  FHeight := 25;
  for i := 0 to AColCount - 1 do
  begin
    vColumn := TGridColumn.Create(AStyle);
    Self.Add(vColumn);
  end;
end;

destructor TGridRow.Destroy;
begin
  inherited Destroy;
end;

procedure TGridRow.DoColumnPaintBackground(const AColumn: TGridColumn;
  const ACanvas: TCanvas; const ARect: TRect);
begin
  if Assigned(FOnColumnPaintBackground) then
    FOnColumnPaintBackground(Self, AColumn, ACanvas, ARect);
end;

procedure TGridRow.Notify(const AColumn: TGridColumn;
  Action: TCollectionNotification);
begin
  inherited Notify(AColumn, Action);

  if Action = cnAdded then
    AColumn.OnPaintBackground := DoColumnPaintBackground;
end;

{ TGridColumn }

procedure TGridColumn.PaintTo(const ACanvas: TCanvas; const ALeft,
  ATop, AWidth, AHeight: Integer; const APaintInfo: TPaintInfo);
var
  vRect: TRect;
begin
  if Assigned(Self.CellData) then
  begin
    vRect := Bounds(ALeft, ATop, AWidth, AHeight);
    if Assigned(FOnPaintBackground) then
      FOnPaintBackground(Self, ACanvas, vRect);

    Self.CellData.PaintData(ALeft, ATop, ATop + AHeight, ATop, ATop + AHeight,
      0, 0, Self.CellData.DrawItems.Count - 1, ACanvas, APaintInfo);

    ACanvas.MoveTo(vRect.Left, vRect.Top);
    ACanvas.LineTo(vRect.Right - 1, vRect.Top);
    ACanvas.LineTo(vRect.Right - 1, vRect.Bottom - 1);
    ACanvas.LineTo(vRect.Left, vRect.Bottom - 1);
    ACanvas.LineTo(vRect.Left, vRect.Top);
  end;
end;

{ THCSelectInfo }

constructor THCSelectInfo.Create;
begin
  inherited Create;
  Initilaze;
end;

procedure THCSelectInfo.SetEnd(const ARow, ACol: Integer);
begin
  FEndRow := ARow;
  FEndCol := ACol;
end;

procedure THCSelectInfo.Initilaze;
begin
  FStartRow := -1;
  FStartCol := -1;
  FEndRow := -1;
  FEndCol := -1;
end;

procedure THCSelectInfo.InitilazeEnd;
begin
  FEndRow := -1;
  FEndCol := -1;
end;

procedure THCSelectInfo.SetStart(const ARow, ACol: Integer);
begin
  FStartRow := ARow;
  FStartCol := ACol;
end;

end.
