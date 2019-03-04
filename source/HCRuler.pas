{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-2-25             }
{                                                       }
{                  文档标尺实现单元                     }
{                                                       }
{*******************************************************}

unit HCRuler;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, HCView, HCCommon, HCUnitConversion;

type
  THCCustomRuler = class(TControl)
  strict private
    FMemBitmap: HBITMAP;
    FDC, FMemDC: HDC;
    FMinGraduation: Single;  // 最小刻度毫米
    FViewWidth  // 显示页面的区域大小(不包含滚动条)
      : Integer;
    FCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    Zoom, MarginLeft, MarginRight,
    PaperWidth  // 纸张宽度毫米
      : Single;
    MarginLeftWidth, MarginRightWidth,
    /// <summary> 标尺起始位置（左侧） </summary>
    GradLeft,
    /// <summary> 标尺结束位置（右侧） </summary>
    GradRight,
    GradRectTop, GradRectBottom
      : Integer;
    GradFontColor, GradLineColor: TColor;
    procedure Resize; override;
    procedure Paint;
    function ZoomIn(const Value: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintToEx(const ACanvas: TCanvas); virtual;
    procedure UpdateView; overload;
    procedure UpdateView(const ARect: TRect); overload;

    property MinGraduation: Single read FMinGraduation write FMinGraduation;
    property ViewWidth: Integer read FViewWidth write FViewWidth;
  published
    property Color;
  end;

  THCSlider = (sdNone, sdMarginLeft, sdMarginRight, sdLeftIndent, sdFirstIndent, sdLeftFirstIndent, sdRightIndent);

  THCHorizontalRuler = class(THCCustomRuler)
  strict private
    FView: THCView;
    FMouseGrad: Integer;
    FLeftFirstIndentRect: TRect;  // 左缩进和首行缩进整体控制块
    FLeftIndentRgn, FFirstIndentRgn, FRightIndentRgn: HRGN;
    FSlider: THCSlider;
    FFirstIndent, FLeftIndent, FRightIndent: Single;
    FOnViewResize: TNotifyEvent;
    procedure DoViewResize(Sender: TObject);
    procedure RestoreViewEvent(const AView: THCView);
    procedure SaveViewEvent;
    function PtInMarginLeftGap(const X, Y: Integer): Boolean;
    function PtInMarginRightGap(const X, Y: Integer): Boolean;
  protected
    procedure PaintToEx(const ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetView(const Value: THCView);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    property View: THCView read FView write SetView;
  end;

implementation

uses
  Math, SysUtils, HCParaStyle;

{ THCRuler }

constructor THCCustomRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FCanvas := TControlCanvas.Create;
  //TControlCanvas(FCanvas).Control := Self;
  FDC := GetDC(0);
  FMemDC := CreateCompatibleDC(FDC);
  FCanvas := TCanvas.Create;
  FCanvas.Handle := FMemDC;

  MinGraduation := 10;
  PaperWidth := 210;
  FViewWidth := 875;

  MarginLeft := 25;
  MarginRight := 20;
  Zoom := 1;

  GradRectTop := 6;
  GradRectBottom := 17;
  GradFontColor := $262322;
  GradLineColor := $B3ABAA;

  Self.Color := clWhite;
  Width := 23;
  Height := 23;
end;

destructor THCCustomRuler.Destroy;
begin
  FCanvas.Free;
  if FMemBitmap <> 0 then
    DeleteObject(FMemBitmap);
  DeleteDC(FMemDC);
  ReleaseDC(0, FDC);
  inherited Destroy;
end;

procedure THCCustomRuler.Paint;
begin
  PaintToEx(FCanvas);
end;

procedure THCCustomRuler.PaintToEx(const ACanvas: TCanvas);
var
  vGradWidth, vPageWidth, vGraCount,
  vLeft, vTop, vDev, vDevInc, vTabWidth: Integer;
  i: Integer;
  vS: string;
begin
  ACanvas.Brush.Color := Self.Color;
  ACanvas.FillRect(Rect(0, 0, Width, Height));

  vPageWidth := ZoomIn(MillimeterToPixX(PaperWidth));  // 测量范围宽度
  GradLeft := Max((FViewWidth - vPageWidth) div 2, PagePadding);
  GradRight := GradLeft + vPageWidth;
  // 填充除左右边框外的可书写区域
  MarginLeftWidth := ZoomIn(MillimeterToPixX(MarginLeft));
  MarginRightWidth := ZoomIn(MillimeterToPixX(MarginRight));
  //ACanvas.Brush.Color := $D5D1D0;
  ACanvas.Brush.Color := $958988;
  ACanvas.FillRect(Rect(GradLeft + MarginLeftWidth, GradRectTop, GradRight - MarginRightWidth, GradRectBottom));

  vGradWidth := ZoomIn(MillimeterToPixX(MinGraduation));  // 最小刻度宽度

  ACanvas.Pen.Color := GradLineColor;
  // 绘制测量范围矩形框
  ACanvas.MoveTo(GradLeft, GradRectTop);
  ACanvas.LineTo(GradLeft + vPageWidth, GradRectTop);
  ACanvas.LineTo(GradLeft + vPageWidth, GradRectBottom);
  ACanvas.LineTo(GradLeft, GradRectBottom);
  ACanvas.LineTo(GradLeft, GradRectTop);

  ACanvas.Font.Size := 8;
  ACanvas.Font.Name := 'Courier New';
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Color := GradFontColor;
  vTop := GradRectTop + (GradRectBottom - GradRectTop - ACanvas.TextExtent('1').cy) div 2;

  //======== 绘制刻度线 ========
  vLeft := vPageWidth - MarginLeftWidth;  // 借用变量vLeft
  vDev := vLeft mod vGradWidth;
  vGraCount := vLeft div vGradWidth;  // 书写区刻度线数量
  // 计算刻度的间距 vGradWidth
  vDevInc := 0;
  while vDev > vGraCount do
  begin
    vDevInc := vDevInc + vDev div vGraCount;
    vDev := vDev mod vGraCount;
  end;
  vGradWidth := vGradWidth + vDevInc;

  // 往左
  vLeft := GradLeft + MarginLeftWidth;  // 正文刻度起始位置，0刻度的位置
  // 跳过刻度0
  i := 1;
  Dec(vLeft, vGradWidth);
  while vLeft >= GradLeft do
  begin
    if not Odd(i) then
    begin
      vS := FormatFloat('#.#', i * MinGraduation);
      ACanvas.TextOut(vLeft - ACanvas.TextWidth(vS) div 2, vTop, vS);
    end
    else
    begin
      ACanvas.MoveTo(vLeft, vTop + 5);
      ACanvas.LineTo(vLeft, vTop + 9);
    end;
    Dec(vLeft, vGradWidth);
    Inc(i);
  end;

  { 往右 }
  vLeft := GradLeft + MarginLeftWidth;  // 正文刻度起始位置，0刻度的位置
  // 跳过刻度0
  if vDev > 0 then
  begin
    Inc(vLeft, vGradWidth + 1);
    Dec(vDev);
  end
  else
    Inc(vLeft, vGradWidth);

  ACanvas.Font.Color := GradFontColor;
  for i := 1 to vGraCount - 1 do
  begin
    if not Odd(i) then
    begin
      vS := FormatFloat('#.#', i * MinGraduation);
      ACanvas.TextOut(vLeft - ACanvas.TextWidth(vS) div 2, vTop, vS);
    end
    else
    begin
      ACanvas.MoveTo(vLeft, vTop + 5);
      ACanvas.LineTo(vLeft, vTop + 9);
    end;

    if vDev > 0 then
    begin
      Inc(vLeft, vGradWidth + 1);
      Dec(vDev);
    end
    else
      Inc(vLeft, vGradWidth);
  end;

  //======== 绘制Tab刻度线 ========
  vTabWidth := ZoomIn(TabCharWidth);
  vLeft := vPageWidth - MarginLeftWidth - MarginRightWidth;  // 借用变量vLeft
  vDev := vLeft mod vTabWidth;
  vGraCount := vLeft div vTabWidth;  // Tab刻度线数量
  // 计算刻度的间距 vGradWidth
  vDevInc := 0;
  while vDev > vGraCount do
  begin
    vDevInc := vDevInc + vDev div vGraCount;
    vDev := vDev mod vGraCount;
  end;
  vGradWidth := vTabWidth + vDevInc;

  vLeft := GradLeft + MarginLeftWidth;  // 正文刻度起始位置，0刻度的位置
  // 跳过刻度0
  if vDev > 0 then
  begin
    Inc(vLeft, vGradWidth + 1);
    Dec(vDev);
  end
  else
    Inc(vLeft, vGradWidth);

  for i := 1 to vGraCount - 1 do
  begin
    ACanvas.MoveTo(vLeft, GradRectBottom + 2);
    ACanvas.LineTo(vLeft, GradRectBottom + 5);

    if vDev > 0 then
    begin
      Inc(vLeft, vGradWidth + 1);
      Dec(vDev);
    end
    else
      Inc(vLeft, vGradWidth);
  end;
end;

procedure THCCustomRuler.Resize;
begin
  inherited Resize;
  if FMemBitmap <> 0 then
    DeleteObject(FMemBitmap);

  FMemBitmap := CreateCompatibleBitmap(FDC, Width, Height);
  SelectObject(FMemDC, FMemBitmap);

  UpdateView;
end;

procedure THCCustomRuler.UpdateView(const ARect: TRect);
var
  vRect: TRect;
begin
  Paint;
  if Assigned(Parent) then
  begin
    vRect := ARect;
    vRect.Offset(Left, Top);
    InvalidateRect(Parent.Handle, vRect, False);
    UpdateWindow(Parent.Handle);
    //RedrawWindow(Parent.Handle, vRect, 0, RDW_INVALIDATE or RDW_NOERASE);
  end;
end;

procedure THCCustomRuler.UpdateView;
begin
  UpdateView(Bounds(0, 0, Width, Height));
end;

procedure THCCustomRuler.WMPaint(var Message: TWMPaint);
begin
  if (Message.DC <> 0) and not (csDestroying in ComponentState) then
  begin
    BitBlt(Message.DC, 0, 0, Width, Height, FMemDC, 0, 0, SRCCOPY);
    {FCanvas.Lock;
    try
      FCanvas.Handle := Message.DC;
      try
        Paint;
      finally
        FCanvas.Handle := 0;
      end;
    finally
      FCanvas.Unlock;
    end; }
  end;
end;

function THCCustomRuler.ZoomIn(const Value: Integer): Integer;
begin
  Result := Round(Value * Zoom);
end;

{ THCRuler }

constructor THCHorizontalRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLeftIndentRgn := 0;
  FFirstIndentRgn := 0;
  FRightIndentRgn := 0;
end;

destructor THCHorizontalRuler.Destroy;
begin
  if FLeftIndentRgn > 0 then
    DeleteObject(FLeftIndentRgn);

  if FFirstIndentRgn > 0 then
    DeleteObject(FFirstIndentRgn);

  if FRightIndentRgn > 0 then
    DeleteObject(FRightIndentRgn);
  inherited Destroy;
end;

procedure THCHorizontalRuler.DoViewResize(Sender: TObject);
begin
  Self.Zoom := FView.Zoom;
  Self.ViewWidth := FView.ViewWidth;
  Self.PaperWidth := FView.ActiveSection.PaperWidth;
  Self.MarginLeft := FView.ActiveSection.PaperMarginLeft;
  Self.MarginRight := FView.ActiveSection.PaperMarginRight;
  FFirstIndent := FView.Style.ParaStyles[FView.Style.CurParaNo].FirstIndent;
  FLeftIndent := FView.Style.ParaStyles[FView.Style.CurParaNo].LeftIndent;
  FRightIndent := FView.Style.ParaStyles[FView.Style.CurParaNo].RightIndent;
  Self.UpdateView;

  if Assigned(FOnViewResize) then
    FOnViewResize(Sender);
end;

procedure THCHorizontalRuler.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if PtInMarginLeftGap(X, Y) then
  begin
    FSlider := sdMarginLeft;
    FMouseGrad := GradLeft + MarginLeftWidth;
  end
  else
  if PtInMarginRightGap(X, Y) then
  begin
    FSlider := sdMarginRight;
    FMouseGrad := GradRight - MarginRightWidth;
  end
  else
  if PtInRegion(FFirstIndentRgn, X, Y) then
  begin
    FSlider := sdFirstIndent;
    FMouseGrad := GradLeft + MarginLeftWidth + MillimeterToPixX(FLeftIndent) + MillimeterToPixX(FFirstIndent);
  end
  else
  if PtInRegion(FLeftIndentRgn, X, Y) then
  begin
    FSlider := sdLeftIndent;
    FMouseGrad := GradLeft + MarginLeftWidth + MillimeterToPixX(FLeftIndent);
  end
  else
  if PtInRegion(FRightIndentRgn, X, Y) then
  begin
    FSlider := sdRightIndent;
    FMouseGrad := GradRight - MarginRightWidth - MillimeterToPixX(FRightIndent);
  end
  else
  if PtInRect(FLeftFirstIndentRect, Point(X, Y)) then
  begin
    FSlider := sdLeftFirstIndent;
    FMouseGrad := GradLeft + MarginLeftWidth + MillimeterToPixX(FLeftIndent);
  end
  else
    FSlider := sdNone;
end;

procedure THCHorizontalRuler.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vGap: Integer;
begin
  inherited;
  if PtInMarginLeftGap(X, Y) or PtInMarginRightGap(X, Y) then
    Self.Cursor := crSizeWE
  else
    Self.Cursor := crDefault;

  vGap := Round(PixXToMillimeter(X - FMouseGrad));
  if Abs(vGap) < 1 then Exit;  // 差异大于1毫米再变动，否则会假死

  if FSlider = sdMarginLeft then
  begin
    Self.MarginLeft := Self.MarginLeft + vGap;
    UpdateView;
    FMouseGrad := GradLeft + MarginLeftWidth;
  end
  else
  if FSlider = sdMarginRight then
  begin
    Self.MarginRight := Self.MarginRight - vGap;
    UpdateView;
    FMouseGrad := GradRight - MarginRightWidth;
  end
  else
  if FSlider = sdFirstIndent then
  begin
    FFirstIndent := FFirstIndent + vGap;
    UpdateView;
    FMouseGrad := GradLeft + MarginLeftWidth + MillimeterToPixX(FLeftIndent) + MillimeterToPixX(FFirstIndent);
  end
  else
  if FSlider = sdLeftIndent then
  begin
    FLeftIndent := FLeftIndent + vGap;
    UpdateView;
    FMouseGrad := GradLeft + MarginLeftWidth + MillimeterToPixX(FLeftIndent);
  end
  else
  if FSlider = sdRightIndent then
  begin
    FRightIndent := FRightIndent - vGap;
    UpdateView;
    FMouseGrad := GradRight - MarginRightWidth - MillimeterToPixX(FRightIndent);
  end;
end;

procedure THCHorizontalRuler.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vParaStyle: THCParaStyle;
begin
  inherited;

  if FSlider = sdMarginLeft then
  begin
    FView.ActiveSection.PaperMarginLeft := MarginLeft;
    FView.ActiveSection.ResetMargin;  // ResetActiveSectionMargin;
  end
  else
  if FSlider = sdMarginRight then
  begin
    FView.ActiveSection.PaperMarginRight := Self.MarginRight;
    FView.ActiveSection.ResetMargin;
  end
  else
  if FSlider = sdFirstIndent then
  begin
    FView.ApplyParaFirstIndent(FFirstIndent);
  end
  else
  if FSlider = sdLeftIndent then
  begin
    FView.ApplyParaLeftIndent(FLeftIndent);
  end
  else
  if FSlider = sdRightIndent then
  begin
    FView.ApplyParaRightIndent(FRightIndent);
  end;

  FSlider := sdNone;
end;

procedure THCHorizontalRuler.PaintToEx(const ACanvas: TCanvas);
var
  vLeft: Integer;
  vParaStyle: THCParaStyle;
  vPoints: array[0..4] of TPoint;
begin
  inherited PaintToEx(ACanvas);
  if not Assigned(FView) then Exit;

  vParaStyle := FView.Style.ParaStyles[FView.Style.CurParaNo];

  // ======== 左缩进+首行缩进整体控制块 ========
  vLeft := GradLeft + MarginLeftWidth + MillimeterToPixX(FLeftIndent);  // 正文刻度起始位置，0刻度的位置
  ACanvas.Brush.Color := $D5D1D0;
  ACanvas.Pen.Color := GradLineColor;
  FLeftFirstIndentRect := Rect(vLeft - 4, GradRectBottom, vLeft + 5, Height);
  ACanvas.Rectangle(FLeftFirstIndentRect);
  // ======== 左缩进控制块 ========
  vPoints[0] := Point(vLeft - 4, GradRectBottom);
  vPoints[1] := Point(vLeft - 4, GradRectBottom - 4);
  ACanvas.MoveTo(vPoints[0].X, vPoints[0].Y);
  ACanvas.LineTo(vPoints[1].X, vPoints[1].Y);  // | 上
  // 三角形边框线
  ACanvas.Brush.Color := GradLineColor;
  ACanvas.FillRect(Bounds(vLeft - 3, GradRectBottom - 4, 1, 1));
  ACanvas.FillRect(Bounds(vLeft - 2, GradRectBottom - 5, 1, 1));
  ACanvas.FillRect(Bounds(vLeft - 1, GradRectBottom - 6, 1, 1));
  ACanvas.FillRect(Bounds(vLeft, GradRectBottom - 7, 1, 1));  // / 右上
  vPoints[2] := Point(vLeft, GradRectBottom - 7);

  ACanvas.FillRect(Bounds(vLeft + 1, GradRectBottom - 6, 1, 1));
  ACanvas.FillRect(Bounds(vLeft + 2, GradRectBottom - 5, 1, 1));
  ACanvas.FillRect(Bounds(vLeft + 3, GradRectBottom - 4, 1, 1));  // \  右下

  vPoints[3] := Point(vLeft + 4, GradRectBottom - 3);
  vPoints[4] := Point(vLeft + 4, GradRectBottom);
  ACanvas.MoveTo(vPoints[3].X, vPoints[3].Y);
  ACanvas.LineTo(vPoints[4].X, vPoints[4].Y);  // | 下
  // 三角形内部填充
  ACanvas.Brush.Color := $D5D1D0;
  ACanvas.FillRect(Bounds(vLeft - 3, GradRectBottom - 3, 7, 3));
  ACanvas.FillRect(Bounds(vLeft - 2, GradRectBottom - 4, 5, 1));
  ACanvas.FillRect(Bounds(vLeft - 1, GradRectBottom - 5, 3, 1));
  ACanvas.FillRect(Bounds(vLeft,     GradRectBottom - 6, 1, 1));

  if FLeftIndentRgn > 0 then
    DeleteObject(FLeftIndentRgn);
  FLeftIndentRgn := CreatePolygonRgn(vPoints, 5, ALTERNATE);
  //FrameRgn(ACanvas.Handle, FLeftIndentRgn, ACanvas.Brush.Handle, 1, 1);

  // ======== 首行缩进控制块 ========
  vLeft := vLeft + MillimeterToPixX(FFirstIndent);
  vPoints[0] := Point(vLeft - 4, GradRectTop);
  vPoints[1] := Point(vLeft - 4, GradRectTop - 3);
  ACanvas.MoveTo(vPoints[0].X, vPoints[0].Y);
  ACanvas.LineTo(vPoints[1].X, vPoints[1].Y);  // |  上
  vPoints[2] := Point(vLeft + 4, GradRectTop - 3);
  vPoints[3] := Point(vLeft + 4, GradRectTop);
  ACanvas.LineTo(vPoints[2].X, vPoints[2].Y);  // —  横
  ACanvas.LineTo(vPoints[3].X, vPoints[3].Y);  // |  下
  // 三角形边框线
  ACanvas.Brush.Color := GradLineColor;
  ACanvas.FillRect(Bounds(vLeft + 3, GradRectTop + 1, 1, 1));
  ACanvas.FillRect(Bounds(vLeft + 2, GradRectTop + 2, 1, 1));
  ACanvas.FillRect(Bounds(vLeft + 1, GradRectTop + 3, 1, 1));
  vPoints[4] := Point(vLeft, GradRectTop + 4);
  ACanvas.FillRect(Bounds(vPoints[4].X, vPoints[4].Y, 1, 1));  // / 左下

  ACanvas.FillRect(Bounds(vLeft - 1, GradRectTop + 3, 1, 1));
  ACanvas.FillRect(Bounds(vLeft - 2, GradRectTop + 2, 1, 1));
  ACanvas.FillRect(Bounds(vLeft - 3, GradRectTop + 1, 1, 1));  // \ 左上
  ACanvas.FillRect(Bounds(vPoints[4].X, vPoints[4].Y, 1, 1));

  // 三角形内部填充
  ACanvas.Brush.Color := $D5D1D0;
  ACanvas.FillRect(Bounds(vLeft - 3, GradRectTop - 2, 7, 3));
  ACanvas.FillRect(Bounds(vLeft - 2, GradRectTop + 1, 5, 1));
  ACanvas.FillRect(Bounds(vLeft - 1, GradRectTop + 2, 3, 1));
  ACanvas.FillRect(Bounds(vLeft,     GradRectTop + 3, 1, 1));

  if FFirstIndentRgn > 0 then
    DeleteObject(FFirstIndentRgn);
  FFirstIndentRgn := CreatePolygonRgn(vPoints, 5, ALTERNATE);
  //FrameRgn(ACanvas.Handle, FFirstIndentRgn, ACanvas.Brush.Handle, 1, 1);

  // ======== 右缩进控制块 ========
  vLeft := GradRight - MarginRightWidth - MillimeterToPixX(FRightIndent);
  vPoints[0] := Point(vLeft - 4, GradRectBottom - 3);
  vPoints[1] := Point(vLeft - 4, GradRectBottom);
  ACanvas.MoveTo(vPoints[0].X, vPoints[0].Y);
  ACanvas.LineTo(vPoints[1].X, vPoints[1].Y);
  vPoints[2] := Point(vLeft + 4, GradRectBottom);
  vPoints[3] := Point(vLeft + 4, GradRectBottom - 4);
  ACanvas.LineTo(vPoints[2].X, vPoints[2].Y);
  ACanvas.LineTo(vPoints[3].X, vPoints[3].Y);
  // 三角形边框线
  ACanvas.Brush.Color := GradLineColor;
  ACanvas.FillRect(Bounds(vLeft + 3, GradRectBottom - 4, 1, 1));
  ACanvas.FillRect(Bounds(vLeft + 2, GradRectBottom - 5, 1, 1));
  ACanvas.FillRect(Bounds(vLeft + 1, GradRectBottom - 6, 1, 1));
  vPoints[4] := Point(vLeft, GradRectBottom - 7);
  ACanvas.FillRect(Bounds(vPoints[4].X, vPoints[4].Y, 1, 1));

  ACanvas.FillRect(Bounds(vLeft - 1, GradRectBottom - 6, 1, 1));
  ACanvas.FillRect(Bounds(vLeft - 2, GradRectBottom - 5, 1, 1));
  ACanvas.FillRect(Bounds(vLeft - 3, GradRectBottom - 4, 1, 1));

  // 三角形内部填充
  ACanvas.Brush.Color := $D5D1D0;
  ACanvas.FillRect(Bounds(vLeft - 3, GradRectBottom - 3, 7, 3));
  ACanvas.FillRect(Bounds(vLeft - 2, GradRectBottom - 4, 5, 1));
  ACanvas.FillRect(Bounds(vLeft - 1, GradRectBottom - 5, 3, 1));
  ACanvas.FillRect(Bounds(vLeft,     GradRectBottom - 6, 1, 1));

  if FRightIndentRgn > 0 then
    DeleteObject(FRightIndentRgn);
  FRightIndentRgn := CreatePolygonRgn(vPoints, 5, ALTERNATE);
  //FrameRgn(ACanvas.Handle, FRightIndentRgn, ACanvas.Brush.Handle, 1, 1);
end;

function THCHorizontalRuler.PtInMarginLeftGap(const X, Y: Integer): Boolean;
begin
  Result := (X > GradLeft + MarginLeftWidth - 2) and (X < GradLeft + MarginLeftWidth + 2)
    and (Y > GradRectTop + 2) and (Y < GradRectBottom - 4);
end;

function THCHorizontalRuler.PtInMarginRightGap(const X, Y: Integer): Boolean;
begin
  Result := (X > GradRight - MarginRightWidth - 2) and (X < GradRight - MarginRightWidth + 2)
    and (Y > GradRectTop) and (Y < GradRectBottom - 4);
end;

procedure THCHorizontalRuler.Reset;
begin
  DoViewResize(Self);
end;

procedure THCHorizontalRuler.RestoreViewEvent(const AView: THCView);
begin
  if Assigned(FOnViewResize) then
    AView.OnViewResize := FOnViewResize;

  FOnViewResize := nil;
end;

procedure THCHorizontalRuler.SaveViewEvent;
begin
  FOnViewResize := FView.OnViewResize;
  FView.OnViewResize := DoViewResize;
end;

procedure THCHorizontalRuler.SetView(const Value: THCView);
begin
  if Assigned(FView) then
    RestoreViewEvent(FView);

  FView := Value;
  SaveViewEvent;
end;

end.
