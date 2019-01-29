{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                  文档滚动条实现单元                   }
{                                                       }
{*******************************************************}

unit HCScrollBar;

interface

uses
  Windows, Classes, Controls, Graphics;

const
  ButtonSize = 20;

type
  TOrientation = (oriHorizontal, oriVertical);

  TScrollCode = (scLineUp, scLineDown, scPageUp, scPageDown, scPosition,
    scTrack, scTop, scBottom, scEndScroll);

  TScrollEvent = procedure(Sender: TObject; ScrollCode: TScrollCode;
    const ScrollPos: Integer) of object;

  TBarControl = (cbcBar, cbcLeftBtn, cbcThum, cbcRightBtn);

  THCScrollBar = class(TGraphicControl)  // 为实现滚动条上按下拖动到控件外也能继续滚动,使用 SetCapture 需要句柄
  private
    /// <summary> 滚动条位置的最小值 </summary>
    FMin,
    /// <summary> 滚动条位置的最大值 </summary>
    FMax,
    /// <summary> 滚动条的最大位置与最小位置差 </summary>
    FRange,
    /// <summary> 垂直滚动条当前位置 </summary>
    FPosition: Integer;

    /// <summary> 界面可移动范围和实际范围的比率 </summary>
    FPercent: Single;

    /// <summary> 点击按钮可移动的大小 </summary>
    FBtnStep: Integer;

    /// <summary> 页面大小 </summary>
    FPageSize: Integer;

    /// <summary> 是水平还是垂直滚动条 </summary>
    FOrientation: TOrientation;

    FMousePt: TPoint;

    /// <summary> 滚动事件 </summary>
    FOnScroll: TScrollEvent;

    FMouseDownControl: TBarControl;

    /// <summary> 滑块区域 </summary>
    FThumRect: TRect;

    /// <summary>
    /// 水平滚动条对应左按钮，垂直滚动条对应上按钮
    /// </summary>
    FLeftBtnRect: TRect;

    /// <summary>
    /// 水平滚动条对应右按钮，垂直滚动条对应下按钮
    /// </summary>
    FRightBtnRect: TRect;

    FOnVisibleChanged: TNotifyEvent;

    /// <summary>
    /// 得到鼠标上去要实现改变的区域
    /// </summary>
    procedure ReCalcButtonRect;

    /// <summary>
    /// 计算滑块区域
    /// </summary>
    procedure ReCalcThumRect;

    /// <summary>
    /// 设置滚动条类型（垂直滚动条、水平滚动条）
    /// </summary>
    /// <param name="Value">滚动条类型</param>
    procedure SetOrientation(Value: TOrientation);

    /// <summary>
    /// 设置滚动条的最小值
    /// </summary>
    /// <param name="Value">最小值</param>
    procedure SetMin(const Value: Integer);

    /// <summary>
    /// 设置滚动条的最大值
    /// </summary>
    /// <param name="Value">最大值</param>
    procedure SetMax(const Value: Integer);

    /// <summary>
    /// 设置滚动条的初始位置
    /// </summary>
    /// <param name="Value">初始位置</param>
    procedure SetPosition(Value: Integer);

    /// <summary>
    /// 设置滚动条表示的页面大小（相对Max - Min）
    /// </summary>
    /// <param name="Value">页面大小</param>
    procedure SetPageSize(const Value :Integer);

    /// <summary>
    /// 点击滚动条按钮页面移动范围
    /// </summary>
    /// <param name="Value">移动范围</param>
    procedure SetBtnStep(const Value: Integer);

    procedure UpdateRangRect;
  protected
    procedure Resize; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure ScrollStep(ScrollCode: TScrollCode);
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure DoDrawThumBefor(const ACanvas: TCanvas; const AThumRect: TRect); virtual;

    property Percent: Single read FPercent write FPercent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintToEx(const ACanvas: TCanvas);
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property Rang: Integer read FRange;
    property PageSize: Integer read FPageSize write SetPageSize;
    property BtnStep: Integer read FBtnStep write SetBtnStep;
    property Position: Integer read FPosition write SetPosition;
    property Orientation: TOrientation read FOrientation write SetOrientation default oriHorizontal;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;
  end;

implementation

uses
  Math;

const
  LineColor = clMedGray;
  IconWidth = 16;
  TitleBackColor = $B3ABAA;
  ThumBackColor = $D5D1D0;

{ THCScrollBar }

constructor THCScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := 0;
  FMax := 100;
  FRange := 100;
  FPageSize := 0;
  FBtnStep := 5;
  //
  Width := 20;
  Height := 20;
  Cursor := crArrow;  // crDefault为什么不行？
end;

destructor THCScrollBar.Destroy;
begin

  inherited;
end;

procedure THCScrollBar.DoDrawThumBefor(const ACanvas: TCanvas;
  const AThumRect: TRect);
begin
end;

procedure THCScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMousePt.X := X;
  FMousePt.Y := Y;
  if PtInRect(FLeftBtnRect, FMousePt) then  // 判断鼠标是否在滚动条上/左按钮区域
  begin
    FMouseDownControl := cbcLeftBtn;  // 鼠标所在区域类型
    ScrollStep(scLineUp);  // 数据向上（左）滚动
  end
  else
  if PtInRect(FThumRect, FMousePt) then  // 鼠标在滑块区域
  begin
    FMouseDownControl := cbcThum;
  end
  else
  if PtInRect(FRightBtnRect, FMousePt) then  // 鼠标在右/下区域
  begin
    FMouseDownControl := cbcRightBtn;
    ScrollStep(scLineDown);  // 数据向下（右）滚动
  end
  else  // 鼠标在滚动条的其他区域
  begin
    FMouseDownControl := cbcBar;  // 滚动条其他区域类型
    if (FThumRect.Top > Y) or (FThumRect.Left > X) then
      ScrollStep(scPageUp)  // 数据向上（左）翻页
    else
    if (FThumRect.Bottom < Y) or (FThumRect.Right < X) then
      ScrollStep(scPageDown);  // 数据向下（右）翻页
  end;
end;

procedure THCScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vOffs: Integer;
begin
  inherited;
  if ssLeft in Shift then  // 拖动
  begin
    if FOrientation = oriHorizontal then  // 水平
    begin
      if FMouseDownControl = cbcThum then  // 鼠标在水平滚动条滑块区域
      begin
        vOffs := X - FMousePt.X;
        Position := FPosition + Round(vOffs / FPercent);;
        FMousePt.X := X;  // 对水平坐标赋值
      end;
    end
    else  // 垂直
    begin
      if FMouseDownControl = cbcThum then  // 在滑块内拖动
      begin
        vOffs := Y - FMousePt.Y;  // 拖块在最下面时，往下快速拖动，还是会触发滚动事件，造成闪烁，如何解决？word是限制拖动块附近的范围
        Position := FPosition + Round(vOffs / FPercent);
        FMousePt.Y := Y;  // 对垂直坐标赋当前Y值
      end;
    end;
  end;
end;

procedure THCScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure THCScrollBar.Paint;
begin
  PaintToEx(Canvas);
end;

procedure THCScrollBar.PaintToEx(const ACanvas: TCanvas);
var
  vRect: TRect;
begin
  ACanvas.Brush.Color := TitleBackColor;
  ACanvas.FillRect(Bounds(0, 0, Width, Height));
  case FOrientation of
    oriHorizontal:  // 水平滚动条
      begin
        // 左按钮
        ACanvas.Pen.Color := clWhite;
        vRect.Left := FLeftBtnRect.Left + ((FLeftBtnRect.Right - FLeftBtnRect.Left) - 4) div 2 + 4;
        vRect.Top := FLeftBtnRect.Top + ((FLeftBtnRect.Bottom - FLeftBtnRect.Top) - 7) div 2;
        ACanvas.MoveTo(vRect.Left, vRect.Top);
        ACanvas.LineTo(vRect.Left, vRect.Top + 7);
        ACanvas.MoveTo(vRect.Left - 1, vRect.Top + 1);
        ACanvas.LineTo(vRect.Left - 1, vRect.Top + 6);
        ACanvas.MoveTo(vRect.Left - 2, vRect.Top + 2);
        ACanvas.LineTo(vRect.Left - 2, vRect.Top + 5);
        ACanvas.MoveTo(vRect.Left - 3, vRect.Top + 3);
        ACanvas.LineTo(vRect.Left - 3, vRect.Top + 4);

        // 右按钮
        vRect.Left := FRightBtnRect.Left + ((FRightBtnRect.Right - FRightBtnRect.Left) - 4) div 2;
        vRect.Top := FRightBtnRect.Top + ((FRightBtnRect.Bottom - FRightBtnRect.Top) - 7) div 2;
        ACanvas.MoveTo(vRect.Left, vRect.Top);
        ACanvas.LineTo(vRect.Left, vRect.Top + 7);
        ACanvas.MoveTo(vRect.Left + 1, vRect.Top + 1);
        ACanvas.LineTo(vRect.Left + 1, vRect.Top + 6);
        ACanvas.MoveTo(vRect.Left + 2, vRect.Top + 2);
        ACanvas.LineTo(vRect.Left + 2, vRect.Top + 5);
        ACanvas.MoveTo(vRect.Left + 3, vRect.Top + 3);
        ACanvas.LineTo(vRect.Left + 3, vRect.Top + 4);

        // 水平滑块
        vRect := FThumRect;
        InflateRect(vRect, 0, -1);

        DoDrawThumBefor(ACanvas, vRect);

        ACanvas.Brush.Color := ThumBackColor;
        ACanvas.Pen.Color := LineColor;
        ACanvas.Rectangle(vRect);
        // 滑块上的修饰
        vRect.Left := vRect.Left + (vRect.Right - vRect.Left) div 2;
        ACanvas.MoveTo(vRect.Left, 5);
        ACanvas.LineTo(vRect.Left, Height - 5);
        ACanvas.MoveTo(vRect.Left + 3, 5);
        ACanvas.LineTo(vRect.Left + 3, Height - 5);
        ACanvas.MoveTo(vRect.Left - 3, 5);
        ACanvas.LineTo(vRect.Left - 3, Height - 5);
      end;

    oriVertical:  // 垂直滚动条
      begin
        // 上按钮
        ACanvas.Pen.Color := clWhite;
        vRect.Left := FLeftBtnRect.Left + ((FLeftBtnRect.Right - FLeftBtnRect.Left) - 7) div 2;
        vRect.Top := FLeftBtnRect.Top + ((FLeftBtnRect.Bottom - FLeftBtnRect.Top) - 4) div 2 + 4;
        ACanvas.MoveTo(vRect.Left, vRect.Top);
        ACanvas.LineTo(vRect.Left + 7, vRect.Top);
        ACanvas.MoveTo(vRect.Left + 1, vRect.Top - 1);
        ACanvas.LineTo(vRect.Left + 6, vRect.Top - 1);
        ACanvas.MoveTo(vRect.Left + 2, vRect.Top - 2);
        ACanvas.LineTo(vRect.Left + 5, vRect.Top - 2);
        ACanvas.MoveTo(vRect.Left + 3, vRect.Top - 3);
        ACanvas.LineTo(vRect.Left + 4, vRect.Top - 3);

        // 下按钮
        vRect.Left := FRightBtnRect.Left + ((FRightBtnRect.Right - FRightBtnRect.Left) - 7) div 2;
        vRect.Top := FRightBtnRect.Top + ((FRightBtnRect.Bottom - FRightBtnRect.Top) - 4) div 2;
        ACanvas.MoveTo(vRect.Left, vRect.Top);
        ACanvas.LineTo(vRect.Left + 7, vRect.Top);
        ACanvas.MoveTo(vRect.Left + 1, vRect.Top + 1);
        ACanvas.LineTo(vRect.Left + 6, vRect.Top + 1);
        ACanvas.MoveTo(vRect.Left + 2, vRect.Top + 2);
        ACanvas.LineTo(vRect.Left + 5, vRect.Top + 2);
        ACanvas.MoveTo(vRect.Left + 3, vRect.Top + 3);
        ACanvas.LineTo(vRect.Left + 4, vRect.Top + 3);

        // 滑块
        vRect := FThumRect;
        InflateRect(vRect, -1, 0);

        DoDrawThumBefor(ACanvas, vRect);

        ACanvas.Brush.Color := ThumBackColor;
        ACanvas.Pen.Color := LineColor;
        ACanvas.Rectangle(vRect);
        // 滑块上的修饰
        vRect.Top := vRect.Top + (vRect.Bottom - vRect.Top) div 2;
        ACanvas.MoveTo(5, vRect.Top);
        ACanvas.LineTo(Width - 5, vRect.Top);
        ACanvas.MoveTo(5, vRect.Top - 3);
        ACanvas.LineTo(Width - 5, vRect.Top - 3);
        ACanvas.MoveTo(5, vRect.Top + 3);
        ACanvas.LineTo(Width - 5, vRect.Top + 3);
      end;
  end;
end;

procedure THCScrollBar.ReCalcButtonRect;
begin
  case FOrientation of
    oriHorizontal:
      begin
        FLeftBtnRect := Rect(0, 0, ButtonSize, Height);
        FRightBtnRect := Rect(Width - ButtonSize, 0, Width, Height);
      end;
    oriVertical:
      begin
        FLeftBtnRect := Rect(0, 0, Width, ButtonSize);
        FRightBtnRect := Rect(0, Height - ButtonSize, Width, Height);
      end;
  end;
end;

procedure THCScrollBar.ReCalcThumRect;
var
  vPer: Single;
  vThumHeight: Integer;
begin
  case FOrientation of
    oriHorizontal:
      begin
        FThumRect.Top := 0;
        FThumRect.Bottom := Height;
        if FPageSize < FRange then  // 页面小于范围
        begin
          vPer := FPageSize / FRange;  // 计算滑块比例
          // 计算滑块的高度
          vThumHeight := Round((Width - 2 * ButtonSize) * vPer);
          if vThumHeight < ButtonSize then  // 滑块高不能小于默认最小高度
            vThumHeight := ButtonSize;

          FPercent := (Width - 2 * ButtonSize - vThumHeight) / (FRange - FPageSize);  // 界面可滚动范围和实际代表范围的比率
          if FPercent < 0 then Exit;  // 防止vThumHeight小于Leftbtn、RightBtn、ThumBtn默认高度总和 3 * ButtonSize时计算出错
          if FPercent = 0 then
            FPercent := 1;

          FThumRect.Left := ButtonSize + Round(FPosition * FPercent);
          FThumRect.Right := FThumRect.Left + vThumHeight;
        end
        else  // 滚动轨道大于等于范围
        begin
          FThumRect.Left := ButtonSize;
          FThumRect.Right := Width - ButtonSize;
        end;
      end;
    oriVertical:
      begin
        FThumRect.Left := 0;
        FThumRect.Right := Width;
        if FPageSize < FRange then  // 页面小于范围
        begin
          vPer := FPageSize / FRange;  // 计算滑块比例
          // 计算滑块的高度
          vThumHeight := Round((Height - 2 * ButtonSize) * vPer);
          if vThumHeight < ButtonSize then  // 滑块高不能小于默认最小高度
            vThumHeight := ButtonSize;

          FPercent := (Height - 2 * ButtonSize - vThumHeight) / (FRange - FPageSize);  // 界面可滚动范围和实际代表范围的比率
          if FPercent < 0 then Exit;  // 防止vThumHeight小于Leftbtn、RightBtn、ThumBtn默认高度总和 3 * ButtonSize时计算出错
          if FPercent = 0 then
            FPercent := 1;

          FThumRect.Top := ButtonSize + Round(FPosition * FPercent);
          FThumRect.Bottom := FThumRect.Top + vThumHeight;
          //Scroll(scTrack, FPosition);  //鼠标移动改变滑块的垂直位置
        end
        else  // 滚动轨道大于等于范围
        begin
          FThumRect.Top := ButtonSize;
          FThumRect.Bottom := Height - ButtonSize;
        end;
      end;
  end;
end;

procedure THCScrollBar.Resize;
begin
  inherited Resize;
end;

procedure THCScrollBar.ScrollStep(ScrollCode: TScrollCode);
var
  vPos: Integer;
begin
  case ScrollCode of
    scLineUp:  // 点击上（左）按钮
      begin
        vPos := FPosition - FBtnStep;
        if vPos < FMin then  // 控制上（左）越界
          vPos := FMin;
        if FPosition <> vPos then
        begin
          Position := vPos;
          //Scroll(scLineUp, FPosition);
        end;
      end;
    scLineDown:
      begin
        vPos := FPosition + FBtnStep;
        if vPos > FRange - FPageSize then  // 控制下（右）越界
          vPos := FRange - FPageSize;
        if FPosition <> vPos then
        begin
          Position := vPos;
          //Scroll(scLineDown, FPosition);
        end;
      end;
    scPageUp:
      begin
        vPos := FPosition - FPageSize;
        {if FKind = sbVertical then
          vPos := Position - Height
        else
          vPos := Position - Width;}
        if vPos < FMin then
          vPos := FMin;
        if FPosition <> vPos then
        begin
          Position := vPos;
          //Scroll(scPageUp, FPosition);
        end;
      end;
    scPageDown:
      begin
        vPos := FPosition + FPageSize;
        {if FKind = sbVertical then
          vPos := Position + Height
        else
          vPos := Position + Width;}
        if vPos > FRange - FPageSize then
          vPos := FRange - FPageSize;
        if FPosition <> vPos then
        begin
          Position := vPos;
          //Scroll(scPageDown, FPosition);
        end;
      end;
    scPosition: ;
    scTrack: ;
    scTop: ;
    scBottom: ;
    scEndScroll: ;
  end;
end;

procedure THCScrollBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if FOrientation = oriVertical then
    Self.FPageSize := Height
  else
    Self.FPageSize := Width;

  if FPosition + FPageSize > FMax then  // 大小变化后，需要重新确定Position
    FPosition := Math.Max(FMax - FPageSize, FMin);

  ReCalcThumRect;  // 重新计算滑块区域
  ReCalcButtonRect;  // 重新计算按钮区域
end;

procedure THCScrollBar.SetBtnStep(const Value: Integer);
begin
  if FBtnStep <> Value then
    FBtnStep := Value;
end;

procedure THCScrollBar.SetMax(const Value: Integer);
begin
  if FMax <> Value then
  begin
    if Value < FMin then
      FMax := FMin
    else
      FMax := Value;

    if FPosition + FPageSize > FMax then
      FPosition := Math.Max(FMax - FPageSize, FMin);

    FRange := FMax - FMin;
    ReCalcThumRect;  // 滑块区域
    UpdateRangRect;  // 重绘
  end;
end;

procedure THCScrollBar.SetMin(const Value: Integer);
begin
  if FMin <> Value then
  begin
    if Value > FMax then
      FMin := FMax
    else
      FMin := Value;
    if FPosition < FMin then
      FPosition := FMin;
    FRange := FMax - FMin;
    ReCalcThumRect;  // 滑块区域
    UpdateRangRect;  // 重绘
  end;
end;

procedure THCScrollBar.SetOrientation(Value: TOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if Value = oriHorizontal then  // 设置为水平滚动条
    begin
      Height := 20;  // 赋值水平滚动条的高度为 20
    end
    else
    if Value = oriVertical then  // 垂直滚动条
    begin
      Width := 20;
    end;
    ReCalcButtonRect;
    ReCalcThumRect;
    UpdateRangRect;  // 重绘
  end;
end;

procedure THCScrollBar.SetPageSize(const Value: Integer);
begin
  if FPageSize <> Value then
  begin
    FPageSize := Value;
    //ReCalcButtonRect;
    ReCalcThumRect;  // 重新计算相对比率（相对Max - Min）
    UpdateRangRect;  // 重绘
  end;
end;

procedure THCScrollBar.SetPosition(Value: Integer);
var
  vPos: Integer;
begin
  if Value < FMin then
    vPos := FMin
  else
  if Value + FPageSize > FMax then
    vPos := Math.Max(FMax - FPageSize, FMin)
  else
    vPos := Value;

  if FPosition <> vPos then
  begin
    FPosition := vPos;
    ReCalcThumRect;  // 滑块区域
    //Repaint;
    UpdateRangRect;  // 重绘

    if Assigned(FOnScroll) then  // 滚动
      FOnScroll(Self, scPosition, FPosition);
  end;
end;

procedure THCScrollBar.UpdateRangRect;
var
  vRect: TRect;
begin
  //if HandleAllocated then
  if Assigned(Parent) and Parent.HandleAllocated then
  begin
    vRect := ClientRect;
    OffsetRect(vRect, Left, Top);
    InvalidateRect(Parent.Handle, vRect, False);
    UpdateWindow(Parent.Handle);
    //RedrawWindow(Handle, nil, 0, RDW_INVALIDATE);
  end;
end;

end.
