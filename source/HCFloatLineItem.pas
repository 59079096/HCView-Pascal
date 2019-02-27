{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-8-17             }
{                                                       }
{         文档FloatLineItem(直线)对象实现单元           }
{                                                       }
{*******************************************************}

unit HCFloatLineItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Messages, HCCustomFloatItem, HCStyle,
  HCItem, HCCustomData, HCCommon, HCXml;

type
  TLineObj = (cloNone, cloLine, cloLeftOrTop, cloRightOrBottom);

  THCFloatLineItem = class(THCCustomFloatItem)  // 可浮动LineItem
  private
    FStartPt, FEndPt, FLeftTop: TPoint;
    FMouseDownObj: TLineObj;
    function GetLineObjAt(const X, Y: Integer): TLineObj;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    function PtInClient(const APoint: TPoint): Boolean; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
  end;

implementation

uses
  Math;

{ THCFloatLineItem }

procedure THCFloatLineItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FStartPt.X := (Source as THCFloatLineItem).FStartPt.X;
  FStartPt.Y := (Source as THCFloatLineItem).FStartPt.Y;
  FEndPt.X := (Source as THCFloatLineItem).FEndPt.X;
  FEndPt.Y := (Source as THCFloatLineItem).FEndPt.Y;
end;

constructor THCFloatLineItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCFloatStyle.Line;
  FMouseDownObj := cloNone;
  Width := 100;
  Height := 70;
  FStartPt := Point(0, 0);
  FEndPt := Point(Width, Height);
end;

procedure THCFloatLineItem.DoPaint(const AStyle: THCStyle;
  const ADrawRect: TRect; const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
{var
  vRgn: HRGN;
  vPointArr: array[0..3] of TPoint;}
begin
  {vPointArr[0] := Point(FStartPt.X - PointSize, FStartPt.Y);
  vPointArr[0].Offset(ADrawRect.TopLeft);
  vPointArr[1] := Point(FStartPt.X + PointSize, FStartPt.Y);
  vPointArr[1].Offset(ADrawRect.TopLeft);
  vPointArr[2] := Point(FEndPt.X + PointSize, FEndPt.Y);
  vPointArr[2].Offset(ADrawRect.TopLeft);
  vPointArr[3] := Point(FEndPt.X - PointSize, FEndPt.Y);
  vPointArr[3].Offset(ADrawRect.TopLeft);}

  {inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);}

  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Style := psSolid;

  ACanvas.MoveTo(FStartPt.X + Self.DrawRect.Left, FStartPt.Y + Self.DrawRect.Top);
  ACanvas.LineTo(FEndPt.X + Self.DrawRect.Left, FEndPt.Y + Self.DrawRect.Top);

  if Self.Active and (not APaintInfo.Print) then  // 激活
  begin
    ACanvas.Rectangle(FStartPt.X + Self.DrawRect.Left - PointSize, FStartPt.Y + Self.DrawRect.Top - PointSize,
      FStartPt.X + Self.DrawRect.Left + PointSize, FStartPt.Y + Self.DrawRect.Top + PointSize);
    ACanvas.Rectangle(FEndPt.X + Self.DrawRect.Left - PointSize, FEndPt.Y + Self.DrawRect.Top - PointSize,
      FEndPt.X + Self.DrawRect.Left + PointSize, FEndPt.Y + Self.DrawRect.Top + PointSize);
  end;

  {vRgn := CreatePolygonRgn(vPointArr, 4, WINDING);
  try
    ACanvas.Brush.Color := clRed;
    FrameRgn(ACanvas.Handle, vRgn, ACanvas.Brush.Handle, 2, 2);
  finally
    DeleteObject(vRgn);
  end;}
end;

function THCFloatLineItem.GetLineObjAt(const X, Y: Integer): TLineObj;
var
  vRgn: HRGN;
  vPointArr: array[0..3] of TPoint;
begin
  Result := cloNone;

  if PtInRect(Rect(FStartPt.X - PointSize, FStartPt.Y - PointSize, FStartPt.X + PointSize, FStartPt.Y + PointSize), Point(X, Y)) then
    Result := cloLeftOrTop
  else
  if PtInRect(Rect(FEndPt.X - PointSize, FEndPt.Y - PointSize, FEndPt.X + PointSize, FEndPt.Y + PointSize), Point(X, Y)) then
    Result := cloRightOrBottom
  else
  begin
    vPointArr[0] := Point(FStartPt.X - PointSize, FStartPt.Y);
    vPointArr[1] := Point(FStartPt.X + PointSize, FStartPt.Y);
    vPointArr[2] := Point(FEndPt.X + PointSize, FEndPt.Y);
    vPointArr[3] := Point(FEndPt.X - PointSize, FEndPt.Y);

    vRgn := CreatePolygonRgn(vPointArr, 4, WINDING);
    try
      if PtInRegion(vRgn, X, Y) then
        Result := cloLine;
    finally
      DeleteObject(vRgn);
    end;
  end;
end;

procedure THCFloatLineItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FStartPt.X, SizeOf(Integer));  // FStartPt.X FixedInt
  AStream.ReadBuffer(FStartPt.Y, SizeOf(Integer));
  AStream.ReadBuffer(FEndPt.X, SizeOf(Integer));
  AStream.ReadBuffer(FEndPt.Y, SizeOf(Integer));
end;

procedure THCFloatLineItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  // inherited
  if Active then
  begin
    FMouseDownObj := GetLineObjAt(X, Y);

    Self.Resizing := (Button = mbLeft) and (Shift = [ssLeft]) and (FMouseDownObj in [cloLeftOrTop, cloRightOrBottom]);
    if Self.Resizing then  // 开始缩放
    begin
      Self.FResizeX := X;
      Self.FResizeY := Y;

      // 缩放前的Rect的LeftTop
      if FStartPt.X < FEndPt.X then
        FLeftTop.X := FStartPt.X
      else
        FLeftTop.X := FEndPt.X;

      if FStartPt.Y < FEndPt.Y then
        FLeftTop.Y := FStartPt.Y
      else
        FLeftTop.Y := FEndPt.Y;
    end;
  end
  else
  begin
    FMouseDownObj := cloNone;
    Active := PtInClient(X, Y);
  end;
end;

procedure THCFloatLineItem.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vLineObj: TLineObj;
begin
  // inherited;
  if Active then
  begin
    if Self.Resizing then  // 拖拽端点
    begin
      if FMouseDownObj = cloLeftOrTop then
        FStartPt.Offset(X - Self.FResizeX, Y - Self.FResizeY)
      else
        FEndPt.Offset(X - Self.FResizeX, Y - Self.FResizeY);

      Self.FResizeX := X;
      Self.FResizeY := Y;

      GCursor := crCross;
    end
    else
    begin
      vLineObj := GetLineObjAt(X, Y);
      if vLineObj in [cloLeftOrTop, cloRightOrBottom] then
        GCursor := crCross
      else
      if vLineObj <> cloNone then
        GCursor := crSize;
    end;
  end
  else
    GCursor := crDefault;
end;

procedure THCFloatLineItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vNewLeftTop: TPoint;
begin
  // inherited;
  if Self.Resizing then
  begin
    Self.Resizing := False;

    // 缩放后的Rect的LeftTop
    if FStartPt.X < FEndPt.X then
      vNewLeftTop.X := FStartPt.X
    else
      vNewLeftTop.X := FEndPt.X;

    if FStartPt.Y < FEndPt.Y then
      vNewLeftTop.Y := FStartPt.Y
    else
      vNewLeftTop.Y := FEndPt.Y;

    vNewLeftTop.X := vNewLeftTop.X - FLeftTop.X;
    vNewLeftTop.Y := vNewLeftTop.Y - FLeftTop.Y;

    FStartPt.Offset(-vNewLeftTop.X, -vNewLeftTop.Y);
    FEndPt.Offset(-vNewLeftTop.X, -vNewLeftTop.Y);

    Self.Left := Self.Left + vNewLeftTop.X;
    Self.Top := Self.Top + vNewLeftTop.Y;

    Self.Width := Abs(FEndPt.X - FStartPt.X);
    Self.Height := Abs(FEndPt.Y - FStartPt.Y);
  end;
end;

procedure THCFloatLineItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FStartPt.X := ANode.Attributes['sx'];
  FStartPt.Y := ANode.Attributes['sy'];
  FEndPt.Y := ANode.Attributes['ex'];
  FEndPt.Y := ANode.Attributes['ey'];
end;

function THCFloatLineItem.PtInClient(const APoint: TPoint): Boolean;
begin
  Result := GetLineObjAt(APoint.X, APoint.Y) <> cloNone;
end;

procedure THCFloatLineItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FStartPt.X, SizeOf(Integer));  // FStartPt.X FixedInt
  AStream.WriteBuffer(FStartPt.Y, SizeOf(Integer));
  AStream.WriteBuffer(FEndPt.X, SizeOf(Integer));
  AStream.WriteBuffer(FEndPt.Y, SizeOf(Integer));
end;

procedure THCFloatLineItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['sx'] := FStartPt.X;
  ANode.Attributes['sy'] := FStartPt.Y;
  ANode.Attributes['ex'] := FEndPt.Y;
  ANode.Attributes['ey'] := FEndPt.Y;
end;

end.
