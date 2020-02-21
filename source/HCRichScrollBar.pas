{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文档高级滚动条实现单元                 }
{                                                       }
{*******************************************************}

unit HCRichScrollBar;

interface

uses
  Windows, Classes, Controls, Graphics, Generics.Collections, HCScrollBar;

type
  TAreaMark = class(TObject)  // 滚动区域标记
  strict private
    FTag, FPosition, FHeight: Integer;
  public
    property Position: Integer read FPosition write FPosition;
    property Height: Integer read FHeight write FHeight;
    property Tag: Integer read FTag write FTag;
  end;

  TAreaMarks = TObjectList<TAreaMark>;

  THCRichScrollBar = class(THCScrollBar)
  private
    FAreaMarks: TAreaMarks;  // 适合固定区域的标记
    FOnPageUpClick, FOnPageDownClick: TNotifyEvent;
    function GetAreaMarkByTag(const ATag: Integer): Integer;
    function GetAreaMarkRect(const AIndex: Integer): TRect;
  protected
    procedure DoDrawThumBefor(const ACanvas: TCanvas; const AThumRect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintToEx(const ACanvas: TCanvas); override;
    //
    procedure SetAreaPos(const ATag, APosition, AHeight: Integer);
    property OnPageUpClick: TNotifyEvent read FOnPageUpClick write FOnPageUpClick;
    property OnPageDownClick: TNotifyEvent read FOnPageDownClick write FOnPageDownClick;
  end;

implementation

{ THCRichScrollBar }

constructor THCRichScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRightBlank := 40; // 2 + 16 + 2 + 16 + 2 + 2
end;

destructor THCRichScrollBar.Destroy;
begin
  if Assigned(FAreaMarks) then
    FAreaMarks.Free;

  inherited Destroy;
end;

procedure THCRichScrollBar.DoDrawThumBefor(const ACanvas: TCanvas;
  const AThumRect: TRect);
var
  i: Integer;
  vRect: TRect;
begin
  case Self.Orientation of
    oriHorizontal:
      begin
      end;

    oriVertical:
      begin
        if Assigned(FAreaMarks) then  // 有标记区域
        begin
          ACanvas.Brush.Color := $006B5952;

          for i := 0 to FAreaMarks.Count - 1 do
          begin
            vRect := GetAreaMarkRect(i);

            if (vRect.Bottom > FLeftBlank + HCScrollBar.ButtonSize)
              and (vRect.Top < Self.Height - FRightBlank - HCScrollBar.ButtonSize)
            then
              ACanvas.FillRect(vRect);
          end;
        end;
      end;
  end;
end;

function THCRichScrollBar.GetAreaMarkByTag(const ATag: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FAreaMarks.Count - 1 do
  begin
    if FAreaMarks[i].Tag = ATag then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function THCRichScrollBar.GetAreaMarkRect(const AIndex: Integer): TRect;
var
  vTop, vHeight: Integer;
begin
  SetRectEmpty(Result);

  case Self.Orientation of
    oriHorizontal:
      begin
      end;

    oriVertical:
      begin
        vTop := FLeftBlank + ButtonSize + Round(FAreaMarks[AIndex].Position * Percent);
        vHeight := Round(FAreaMarks[AIndex].Height * Percent);
        if vHeight < 2 then
          vHeight := 2;  // 最小高度

        Result := Bounds(0, vTop, Width, vHeight);
      end;
  end;
end;

procedure THCRichScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
  vRect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if PtInRect(FThumRect, FMouseDownPt) then Exit;

  case Self.Orientation of
    oriHorizontal:
      begin
      end;

    oriVertical:
      begin
        if Assigned(FAreaMarks) then
        begin
          for i := 0 to FAreaMarks.Count - 1 do
          begin
            vRect := GetAreaMarkRect(i);
            if PtInRect(vRect, FMouseDownPt) then
            begin
              Self.Position := FAreaMarks[i].Position - vRect.Top;
              Break;
            end;
          end;
        end;
      end;
  end;
end;

procedure THCRichScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if Orientation = oriVertical then  // 垂直滚动条
  begin
    if PtInRect(Bounds(2, Height - FRightBlank + 2, 16, 16), Point(X, Y)) then  // 上翻页按钮
    begin
      if Assigned(FOnPageUpClick) then
        FOnPageUpClick(Self);
    end
    else
    if PtInRect(Bounds(2, Height - FRightBlank + 2 + 16 + 2, 16, 16), Point(X, Y)) then  // 下翻页按钮
    begin
      if Assigned(FOnPageDownClick) then
        FOnPageDownClick(Self);
    end;
  end;
end;

procedure THCRichScrollBar.PaintToEx(const ACanvas: TCanvas);
var
  vX, vY: Integer;
begin
  inherited PaintToEx(ACanvas);

  if Orientation = oriVertical then  // 垂直滚动条
  begin
    if FRightBlank > 0 then
    begin
      ACanvas.Brush.Color := $006B5952;
      ACanvas.FillRect(Rect(2, Height - FRightBlank + 2, Width - 2, Height - 2));

      // 上按钮
      ACanvas.Pen.Color := $00B3ABAA;
      vX := (Width - 5) div 2;
      vY := Height - FRightBlank + 2 + ButtonSize - 9;
      ACanvas.MoveTo(vX, vY);
      ACanvas.LineTo(vX + 5, vY);
      ACanvas.MoveTo(vX + 1, vY - 1);
      ACanvas.LineTo(vX + 4, vY - 1);
      ACanvas.MoveTo(vX + 2, vY - 2);
      ACanvas.LineTo(vX + 3, vY - 2);

      vY := vY - 3;
      ACanvas.MoveTo(vX, vY);
      ACanvas.LineTo(vX + 5, vY);
      ACanvas.MoveTo(vX + 1, vY - 1);
      ACanvas.LineTo(vX + 4, vY - 1);
      ACanvas.MoveTo(vX + 2, vY - 2);
      ACanvas.LineTo(vX + 3, vY - 2);

      // 下按钮
      vY := Height - FRightBlank + 2 + ButtonSize + 2 + 3;
      ACanvas.MoveTo(vX, vY);
      ACanvas.LineTo(vX + 5, vY);
      ACanvas.MoveTo(vX + 1, vY + 1);
      ACanvas.LineTo(vX + 4, vY + 1);
      ACanvas.MoveTo(vX + 2, vY + 2);
      ACanvas.LineTo(vX + 3, vY + 2);

      vY := vY + 3;
      ACanvas.MoveTo(vX, vY);
      ACanvas.LineTo(vX + 5, vY);
      ACanvas.MoveTo(vX + 1, vY + 1);
      ACanvas.LineTo(vX + 4, vY + 1);
      ACanvas.MoveTo(vX + 2, vY + 2);
      ACanvas.LineTo(vX + 3, vY + 2);
    end;
  end;
end;

procedure THCRichScrollBar.SetAreaPos(const ATag, APosition, AHeight: Integer);
var
  vIndex: Integer;
  vAreaMark: TAreaMark;
  vRect: TRect;
begin
  if not Assigned(FAreaMarks) then
    FAreaMarks := TAreaMarks.Create;

  vIndex := GetAreaMarkByTag(ATag);
  if vIndex < 0 then
  begin
    vAreaMark := TAreaMark.Create;
    vAreaMark.Tag := ATag;
    vAreaMark.Position := APosition;
    vAreaMark.Height := AHeight;

    FAreaMarks.Add(vAreaMark);
    vRect := GetAreaMarkRect(FAreaMarks.Count - 1);
    OffsetRect(vRect, Left, Top);
    InvalidateRect(Self.Parent.Handle, vRect, False);
  end
  else  // 有对应tag的，重新修改值
  if (FAreaMarks[vIndex].Position <> APosition) or (FAreaMarks[vIndex].Height <> AHeight) then
  begin
    vRect := GetAreaMarkRect(vIndex);
    FAreaMarks[vIndex].Position := APosition;
    FAreaMarks[vIndex].Height := AHeight;
    OffsetRect(vRect, Left, Top);
    InvalidateRect(Self.Parent.Handle, vRect, False);  // 旧的去

    vRect := GetAreaMarkRect(vIndex);
    OffsetRect(vRect, Left, Top);
    InvalidateRect(Self.Parent.Handle, vRect, False);  // 新的入
  end;
end;

end.
