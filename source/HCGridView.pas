unit HCGridView;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, Generics.Collections, Messages,
  HCScrollBar, HCRichScrollBar, HCTableCell, HCStyle, HCItem, HCCommon;

type
  THCColumnPaintEvent = procedure(const ACell: THCTableCell;
    const ACanvas: TCanvas; const ARect: TRect) of object;

  TGridRow = class(TObjectList<THCTableCell>)
  strict private
    FHeight: Integer;
    FAutoHeight: Boolean;
  public
    constructor Create(const AStyle: THCStyle; const AColCount: Integer); virtual;
    destructor Destroy; override;
    property Height: Integer read FHeight write FHeight;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight;
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
    FCaret: THCCaret;
    FHScrollBar: THCScrollBar;
    FVScrollBar: THCRichScrollBar;
    FActiveCell: THCTableCell;
    FRows: TObjectList<TGridRow>;
    FColWidths: TList<Integer>;
    FColDefaultWidth, FViewWidth, FViewHeight,
    FDispFirstRow, FDispLastRow, FDispFirstCol, FDispLastCol,
    FUpdateCount: Integer;
    FBorderWidth,  // 边框宽度
    FCellHPadding,  // 单元格内容水平偏移
    FCellVPadding   // 单元格内容垂直偏移(不能大于最低的DrawItem高度，否则会影响跨页)
      : Byte;  // 单元格数据和单元格边框的距离
    FOnVerScroll, FOnHorScroll, FOnCaretChange: TNotifyEvent;
    FSelectInfo: THCSelectInfo;
    FOnPaintBackground: THCGridPaintEvent;
    FOnCellPaintBackground: THCColumnPaintEvent;
    procedure DoHorScroll(Sender: TObject; ScrollCode: TScrollCode;
      const ScrollPos: Integer);
    procedure DoVerScroll(Sender: TObject; ScrollCode: TScrollCode;
      const ScrollPos: Integer);
    function GetCell(const ARow, ACol: Integer): THCTableCell;
    function GetRowAt(const X, Y: Integer): TGridRow;
    function GetRowIndexAt(const X, Y: Integer): Integer;
    function GetColIndexAt(const X, Y: Integer): Integer;
    procedure GetCellIndex(const X, Y: Integer; var ARow, ACol: Integer);
    function GetCellAt(const X, Y: Integer): THCTableCell;
    procedure GetDestCell(const ARow, ACol: Cardinal; var ADestRow, ADestCol: Integer);
    procedure GetSourceCell(const ARow, ACol: Cardinal; var ASrcRow, ASrcCol: Integer);
    //
    function GetHorOffset: Integer;
    function GetVerOffset: Integer;
    procedure SetBorderWidth(const Value: Byte);
    procedure SetCellHPadding(const Value: Byte);
    procedure SetCellVPadding(const Value: Byte);
    procedure GetViewWidth;
    procedure GetViewHeight;
    procedure CalcDisplayCellRange;
    procedure CalcScrollRang;
    function GetViewRect: TRect;
    function GetActiveCellRect: TRect;
    function GetCellRect(const ARow, ACol: Integer): TRect;
    procedure UpdateView; overload;
    procedure UpdateView(const ARect: TRect); overload;
    procedure DoCaretChange;
    /// <summary> 重新获取光标位置 </summary>
    procedure ReBuildCaret;
    procedure CheckUpdateInfo;
    //
    procedure PaintTo(const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    // Imm
    procedure UpdateImmPosition;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWnd; override;
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
    procedure DoMapChanged;
    procedure FormatRow(const ARow: Integer);
    procedure CalcRowCellHeight(const ARow: Integer);
    procedure Format;
    /// <summary> 单元格绘制背景 </summary>
    procedure DoCellPaintBackground(Sender: TObject; const ACell: THCTableCell;
      const ACanvas: TCanvas; const ARect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; const ARowCount, AColCount: Integer);
    destructor Destroy; override;
    function ContentHeight: Integer;
    function ContentWidth: Integer;
    /// <summary> 开始批量处理 </summary>
    procedure BeginUpdate;
    /// <summary> 结束批量处理 </summary>
    procedure EndUpdate;
    property Cells[const Row, Col: Integer]: THCTableCell read GetCell;
    property HorOffset: Integer read GetHorOffset;
    property VerOffset: Integer read GetVerOffset;
    property BorderWidth: Byte read FBorderWidth write SetBorderWidth;
    property CellHPadding: Byte read FCellHPadding write SetCellHPadding;
    property CellVPadding: Byte read FCellVPadding write SetCellVPadding;
    property OnPaintBackground: THCGridPaintEvent read FOnPaintBackground write FOnPaintBackground;
    property OnCellPaintBackground: THCColumnPaintEvent read FOnCellPaintBackground write FOnCellPaintBackground;
    /// <summary> 垂直滚动条滚动时触发 </summary>
    property OnVerScroll: TNotifyEvent read FOnVerScroll write FOnVerScroll;

    /// <summary> 水平滚动条滚动时触发 </summary>
    property OnHorScroll: TNotifyEvent read FOnHorScroll write FOnHorScroll;

    /// <summary> 光标位置改变时触发 </summary>
    property OnCaretChange: TNotifyEvent read FOnCaretChange write FOnCaretChange;
  end;

  THCGridView = class(THCCustomGridView)
  public
    property Color;
    property OnMouseWheel;
  end;

implementation

uses
  Math;

{ THCCustomGridView }

constructor THCCustomGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBitmap := TBitmap.Create;
  Self.Color := clWhite;
  FUpdateCount := 0;
  FColDefaultWidth := 50;
  FActiveCell := nil;
  FDispFirstRow := -1;
  FDispLastRow := -1;
  FDispFirstCol := -1;
  FDispLastCol := -1;
  FBorderWidth := 1;
  FCellHPadding := 2;
  FCellVPadding := 2;

  FStyle := THCStyle.CreateEx(True, True);
  FStyle.ShowParaLastMark := False;
  //FStyle.OnInvalidateRect := DoStyleInvalidateRect;

  FRows := TObjectList<TGridRow>.Create;
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

constructor THCCustomGridView.CreateEx(AOwner: TComponent; const ARowCount,
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

  Format;
end;

procedure THCCustomGridView.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FCaret) then
      FreeAndNil(FCaret);

    FCaret := THCCaret.Create(Handle);
  end;
end;

procedure THCCustomGridView.BeginUpdate;
begin
  Inc(FUpdateCount);
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
    vPos := vPos + FBorderWidth + FRows[i].Height;
    if (FDispFirstRow < 0) and (vPos > FVScrollBar.Position) then  // 行底部能显示(不含底部边框)
      FDispFirstRow := i;

    if vPos + FBorderWidth - FVScrollBar.Position >= Self.Height then
    begin
      FDispLastRow := i;
      Break;
    end;
  end;

  if FDispLastRow < 0 then
    FDispLastRow := FRows.Count - 1;

  vPos := 0;
  for i := 0 to FColWidths.Count - 1 do
  begin
    vPos := vPos + FBorderWidth + FColWidths[i];
    if (FDispFirstCol < 0) and (vPos > FHScrollBar.Position) then
      FDispFirstCol := i;

    if vPos + FBorderWidth - FHScrollBar.Position >= Self.Width then
    begin
      FDispLastCol := i;
      Break;
    end;
  end;

  if FDispLastCol < 0 then
    FDispLastCol := FColWidths.Count - 1;
end;

procedure THCCustomGridView.CalcRowCellHeight(const ARow: Integer);
var
  vNorHeightMax: Integer;
  vC: Integer;
  vCell: THCTableCell;
begin
  vNorHeightMax := 0;
  for vC := 0 to FColWidths.Count - 1 do
  begin
    vCell := FRows[ARow][vC];
    if Assigned(vCell.CellData) and (vCell.RowSpan = 0) then
      vNorHeightMax := Max(vNorHeightMax, vCell.CellData.Height);
  end;

  vNorHeightMax := vNorHeightMax + FCellVPadding + FCellVPadding;
  for vC := 0 to FColWidths.Count - 1 do
    FRows[ARow][vC].Height := vNorHeightMax;

  if FRows[ARow].AutoHeight then
    FRows[ARow].Height := vNorHeightMax
  else  // 拖动改变了行高度
  begin
    if vNorHeightMax > FRows[ARow].Height then  // 拖动高度失效
    begin
      FRows[ARow].AutoHeight := True;
      FRows[ARow].Height := vNorHeightMax;
    end;
  end;
end;

procedure THCCustomGridView.CalcScrollRang;
begin
  FVScrollBar.Max := ContentHeight;
  FHScrollBar.Max := ContentWidth;
end;

procedure THCCustomGridView.CheckUpdateInfo;
begin
  if FUpdateCount > 0 then Exit;

  if Assigned(FCaret) and FStyle.UpdateInfo.ReCaret then
  begin
    ReBuildCaret;
    FStyle.UpdateInfo.ReCaret := False;

    UpdateImmPosition;
  end;

  if FStyle.UpdateInfo.RePaint then
  begin
    FStyle.UpdateInfo.RePaint := False;
    UpdateView;
  end;
end;

function THCCustomGridView.ContentHeight: Integer;
var
  i: Integer;
begin
  Result := FBorderWidth;
  for i := 0 to FRows.Count - 1 do
    Result := Result + FRows[i].Height + FBorderWidth;
end;

function THCCustomGridView.ContentWidth: Integer;
var
  i: Integer;
begin
  Result := FBorderWidth;
  for i := 0 to FColWidths.Count - 1 do
    Result := Result + FColWidths[i] + FBorderWidth;
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
  FreeAndNil(FCaret);
  inherited Destroy;
end;

procedure THCCustomGridView.DoCaretChange;
begin
  if Assigned(FOnCaretChange) then
    FOnCaretChange(Self);
end;

procedure THCCustomGridView.DoCellPaintBackground(Sender: TObject;
  const ACell: THCTableCell; const ACanvas: TCanvas; const ARect: TRect);
begin
  if Assigned(FOnCellPaintBackground) then
    FOnCellPaintBackground(ACell, ACanvas, ARect);
end;

procedure THCCustomGridView.DoHorScroll(Sender: TObject;
  ScrollCode: TScrollCode; const ScrollPos: Integer);
begin
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret(False);
  CheckUpdateInfo;
  if Assigned(FOnHorScroll) then
    FOnHorScroll(Self);
end;

procedure THCCustomGridView.DoMapChanged;
begin
  if FUpdateCount = 0 then
  begin
    CalcScrollRang;
    CheckUpdateInfo;
  end;
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

procedure THCCustomGridView.DoVerScroll(Sender: TObject;
  ScrollCode: TScrollCode; const ScrollPos: Integer);
begin
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret(False);
  CheckUpdateInfo;
  if Assigned(FOnVerScroll) then
    FOnVerScroll(Self);
end;

procedure THCCustomGridView.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);

  DoMapChanged;
end;

procedure THCCustomGridView.Format;
var
  vR: Integer;
begin
  for vR := 0 to FRows.Count - 1 do
  begin
    FormatRow(vR);
    CalcRowCellHeight(vR);
  end;

  //CalcMergeRowHeightFrom(0);
end;

procedure THCCustomGridView.FormatRow(const ARow: Integer);
var
  vC: Integer;
  vCell: THCTableCell;
begin
  for vC := 0 to FColWidths.Count - 1 do
  begin
    vCell := FRows[ARow][vC];
    vCell.Width := FColWidths[vC];
    if Assigned(vCell.CellData) then
    begin
      vCell.CellData.Width := vCell.Width - FCellHPadding - FCellHPadding;
      vCell.CellData.ReFormat;
    end;
  end;
end;

function THCCustomGridView.GetActiveCellRect: TRect;
begin
  Result := GetCellRect(FSelectInfo.StartRow, FSelectInfo.StartCol);
end;

function THCCustomGridView.GetCell(const ARow, ACol: Integer): THCTableCell;
begin
  Result := FRows[ARow][ACol];
end;

function THCCustomGridView.GetCellAt(const X, Y: Integer): THCTableCell;
var
  vRow, vCol: Integer;
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

function THCCustomGridView.GetCellRect(const ARow, ACol: Integer): TRect;
var
  i, vBorderHalf, vRow, vCol: Integer;
begin
  GetDestCell(ARow, ACol, vRow, vCol);

  vBorderHalf := FBorderWidth div 2;

  Result.Top := 0;
  for i := 0 to vRow - 1 do
    Result.Top := Result.Top + FBorderWidth + FRows[i].Height;

  Result.Top := Result.Top + vBorderHalf - FVScrollBar.Position;

  Result.Left := 0;
  for i := 0 to vCol - 1 do
    Result.Left := Result.Left + FBorderWidth + FColWidths[i];

  Result.Left := Result.Left + vBorderHalf - FHScrollBar.Position;

  Result.Right := Result.Left + FColWidths[vCol];

  if FRows[vRow][vCol].RowSpan = 0 then
    Result.Bottom := Result.Top + FRows[vRow].Height
  else
    Result.Bottom := Result.Top + FRows[vRow][vCol].Height;
end;

function THCCustomGridView.GetColIndexAt(const X, Y: Integer): Integer;
var
  i, vLeft: Integer;
begin
  Result := -1;

  vLeft := FBorderWidth - FHScrollBar.Position;
  for i := 0 to FColWidths.Count - 1 do
  begin
    if (X > vLeft) and (X < vLeft + FColWidths[i]) then
    begin
      Result := i;
      Break;
    end;

    vLeft := vLeft + FColWidths[i] + FBorderWidth;
  end;
end;

procedure THCCustomGridView.GetDestCell(const ARow, ACol: Cardinal;
  var ADestRow, ADestCol: Integer);
begin
  if Cells[ARow, ACol].CellData <> nil then
  begin
    ADestRow := ARow;
    ADestCol := ACol;
  end
  else
  begin
    ADestRow := ARow + Cells[ARow, ACol].RowSpan;
    ADestCol := ACol + Cells[ARow, ACol].ColSpan;
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

  vTop := FBorderWidth - FVScrollBar.Position;
  for i := 0 to FRows.Count - 1 do
  begin
    if (Y > vTop) and (Y < vTop + FRows[i].Height) then
    begin
      Result := i;
      Break;
    end;

    vTop := vTop + FRows[i].Height + FBorderWidth;
  end;
end;

procedure THCCustomGridView.GetSourceCell(const ARow, ACol: Cardinal;
  var ASrcRow, ASrcCol: Integer);
begin
  if Cells[ARow, ACol].CellData <> nil then
  begin
    ASrcRow := ARow + FRows[ARow][ACol].RowSpan;
    ASrcCol := ACol + FRows[ARow][ACol].ColSpan;
  end
  else  // 源单元格不能获取源单元格
    raise Exception.Create(HCS_EXCEPTION_VOIDSOURCECELL);
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
  begin
    FActiveCell.CellData.KeyPress(Key);
    FormatRow(FSelectInfo.StartRow);
    CalcRowCellHeight(FSelectInfo.StartRow);
    DoMapChanged;
  end;

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
  vRect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);

  FSelectInfo.InitilazeEnd;
  GetCellIndex(X, Y, vRow, vCol);
  if (vRow <> FSelectInfo.StartRow) or (vCol <> FSelectInfo.StartCol) then
  begin
    FSelectInfo.SetStart(vRow, vCol);
    FActiveCell := GetCellAt(X, Y);
  end;

  if Assigned(FActiveCell) then
  begin
    vRect := GetActiveCellRect;
    FActiveCell.CellData.MouseDown(Button, Shift, X - FCellHPadding - vRect.Left, Y - FCellVPadding - vRect.Top);
  end
  else
    FStyle.UpdateInfoReCaret;

  CheckUpdateInfo;
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

  function CreatExtPen: HPen;
  var
    APenParams: TLogBrush;
  const
    PenTypes: array[Boolean] of Integer = (PS_COSMETIC, PS_GEOMETRIC);
    //PenStyles: array[psSolid..psInsideFrame] of Word =
    //  (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL, PS_SOLID);
  begin
    APenParams.lbStyle := BS_SOLID;
    APenParams.lbColor := ACanvas.Pen.Color;
    APenParams.lbHatch := 0;
    Result := ExtCreatePen(PenTypes[FBorderWidth <> 1] or PS_ENDCAP_SQUARE,
      FBorderWidth, APenParams, 0, nil);
  end;

var
  vRect: TRect;
  vR, vC, vLeft, vTop, vRight, vBottom, vPaintLeft, vBorderHalf: Integer;
  vCell: THCTableCell;
  //vExtPen: ^TExtLogPen;
  vExtPen: HPEN;
  vOldPen: HGDIOBJ;
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
  ACanvas.Pen.Width := FBorderWidth;
  if GetObjectType(ACanvas.Pen.Handle) = OBJ_EXTPEN then
  begin
    vExtPen := ACanvas.Pen.Handle;
    //vBottom := GetObject(ACanvas.Pen.Handle, 0, nil);
    //GetObject(ACanvas.Pen.Handle, vBottom, vExtPen);
  end
  else
    vExtPen := CreatExtPen;  // 因为默认的画笔没有线帽的控制，新增支持线帽的画笔

  vBorderHalf := FBorderWidth div 2;
  vTop := 0;
  for vR := 0 to FDispFirstRow - 1 do
    vTop := vTop + FBorderWidth + FRows[vR].Height;

  vTop := vTop + vBorderHalf - FVScrollBar.Position;

  vPaintLeft := 0;
  for vC := 0 to FDispFirstCol - 1 do
    vPaintLeft := vPaintLeft + FBorderWidth + FColWidths[vC];

  vPaintLeft := vPaintLeft + vBorderHalf - FHScrollBar.Position;

  vLeft := vPaintLeft;
  for vR := FDispFirstRow to FDispLastRow do
  begin
    for vC := FDispFirstCol to FDispLastCol do
    begin
      vCell := Cells[vR, vC];
      if Assigned(vCell.CellData) then
      begin
        if vCell.ColSpan = 0 then
          vRight := vLeft + FBorderWidth + FColWidths[vC]
        else
          vRight := vLeft + FBorderWidth + vCell.Width;

        if vCell.RowSpan = 0 then
          vBottom := vTop + FBorderWidth + FRows[vR].Height
        else
          vBottom := vTop + FBorderWidth + vCell.Height;

        vRect := Rect(vLeft + vBorderHalf, vTop + vBorderHalf, vRight, vBottom);
        if Assigned(FOnCellPaintBackground) then  // 绘制自定义背景
          FOnCellPaintBackground(vCell, ACanvas, vRect);
        // 绘制内容
        vCell.CellData.PaintData(vLeft + vBorderHalf + FCellHPadding,
          vTop + vBorderHalf + FCellVPadding,
          vTop + vBorderHalf + vCell.Height - FCellVPadding,
          0, Self.Height,
          0, 0, vCell.CellData.DrawItems.Count - 1, ACanvas, APaintInfo);
        // 绘制边框
        vOldPen := SelectObject(ACanvas.Handle, vExtPen);
        ACanvas.MoveTo(vLeft, vTop);
        ACanvas.LineTo(vRight, vTop);
        ACanvas.LineTo(vRight, vBottom);
        ACanvas.LineTo(vLeft, vBottom);
        ACanvas.LineTo(vLeft, vTop);
        SelectObject(ACanvas.Handle, vOldPen);
      end;

      vLeft := vLeft + FBorderWidth + FColWidths[vC];
    end;

    vTop := vTop + FBorderWidth + FRows[vR].Height;
    vLeft := vPaintLeft;
  end;

  DeleteObject(vExtPen);
end;

procedure THCCustomGridView.ReBuildCaret;
var
  vCaretInfo: THCCaretInfo;
  vRect: TRect;
begin
  if not Assigned(FCaret) then Exit;

  if (not Self.Focused)
    or (not Assigned(FActiveCell))
    or ((not FStyle.UpdateInfo.Draging) and FActiveCell.CellData.SelectExists)
  then
  begin
    FCaret.Hide;
    Exit;
  end;

  vCaretInfo.X := 0;
  vCaretInfo.Y := 0;
  vCaretInfo.Height := 0;
  vCaretInfo.Visible := True;

  FActiveCell.CellData.GetCaretInfoCur(vCaretInfo);

  if not vCaretInfo.Visible then
  begin
    FCaret.Hide;
    Exit;
  end;

  vRect := GetActiveCellRect;

  FCaret.X := vRect.Left + FCellHPadding + vCaretInfo.X;
  FCaret.Y := vRect.Top + FCellVPadding + vCaretInfo.Y;
  FCaret.Height := vCaretInfo.Height;
  FCaret.Show;
  DoCaretChange;
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
  DoMapChanged;
end;

procedure THCCustomGridView.SetBorderWidth(const Value: Byte);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Format;
    DoMapChanged;
  end;
end;

procedure THCCustomGridView.SetCellHPadding(const Value: Byte);
begin
  if FCellHPadding <> Value then
  begin
    FCellHPadding := Value;
    Format;
    DoMapChanged;
  end;
end;

procedure THCCustomGridView.SetCellVPadding(const Value: Byte);
begin
  if FCellVPadding <> Value then
  begin
    FCellVPadding := Value;
    Format;
    DoMapChanged;
  end;
end;

procedure THCCustomGridView.UpdateImmPosition;
begin

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
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      begin
        if not (csDesigning in ComponentState) and not Focused then
          Self.SetFocus;
      end;
  end;

  inherited WndProc(Message);
end;

{ TGridRow }

constructor TGridRow.Create(const AStyle: THCStyle; const AColCount: Integer);
var
  i: Integer;
  vCell: THCTableCell;
begin
  inherited Create(True);

  FAutoHeight := True;
  FHeight := 0;
  for i := 0 to AColCount - 1 do
  begin
    vCell := THCTableCell.Create(AStyle);
    Self.Add(vCell);
  end;
end;

destructor TGridRow.Destroy;
begin
  inherited Destroy;
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
