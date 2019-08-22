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
  HCItem, HCCustomData, HCCommon, HCXml, HCShape;

type
  THCFloatLineItem = class(THCCustomFloatItem)  // 可浮动LineItem
  private
    FLeftTop: TPoint;
    FShapeLine: THCShapeLine;
  protected
    procedure SetActive(const Value: Boolean); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    function PointInClient(const APoint: TPoint): Boolean; override;
    procedure Assign(Source: THCCustomItem); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
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
  FShapeLine.Assign((Source as THCFloatLineItem).FShapeLine);
end;

constructor THCFloatLineItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := Ord(THCShapeStyle.hssLine);
  Width := 100;
  Height := 70;
  FShapeLine := THCShapeLine.CreateEx(Point(0, 0), Point(Width, Height));
end;

procedure THCFloatLineItem.DoPaint(const AStyle: THCStyle;
  const ADrawRect: TRect; const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  FShapeLine.PaintTo(ACanvas, ADrawRect, APaintInfo);  // 用Self.DrawRect？
end;

procedure THCFloatLineItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vX, vY: Integer;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 26 then
    FShapeLine.LoadFromStream(AStream)
  else
  begin
    FShapeLine.Width := 1;
    FShapeLine.Color := clBlack;
    AStream.ReadBuffer(vX, SizeOf(Integer));
    AStream.ReadBuffer(vY, SizeOf(Integer));
    FShapeLine.StartPt := Point(vX, vY);
    AStream.ReadBuffer(vX, SizeOf(Integer));
    AStream.ReadBuffer(vY, SizeOf(Integer));
    FShapeLine.EndPt := Point(vX, vY);
  end;
end;

function THCFloatLineItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer): Boolean;
begin
  // inherited
  Result := FShapeLine.MouseDown(Button, Shift, X, Y);
  Active := FShapeLine.ActiveObj <> sloNone;
  if Active then
  begin
    Self.Resizing := (Button = mbLeft) and (Shift = [ssLeft])
      and (FShapeLine.ActiveObj in [sloStart, sloEnd]);

    if Self.Resizing then  // 开始缩放
    begin
      Self.FResizeX := X;
      Self.FResizeY := Y;

      // 缩放前的Rect的LeftTop
      if FShapeLine.StartPt.X < FShapeLine.EndPt.X then
        FLeftTop.X := FShapeLine.StartPt.X
      else
        FLeftTop.X := FShapeLine.EndPt.X;

      if FShapeLine.StartPt.Y < FShapeLine.EndPt.Y then
        FLeftTop.Y := FShapeLine.StartPt.Y
      else
        FLeftTop.Y := FShapeLine.EndPt.Y;
    end;
  end;
end;

function THCFloatLineItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  // inherited;
  Result := FShapeLine.MouseMove(Shift, X, Y);
  if Active then
  begin
    if Self.Resizing then  // 拖拽端点
    begin
      Self.FResizeX := X;
      Self.FResizeY := Y;
    end;
  end;

  if Result then
    GCursor := FShapeLine.Cursor;
end;

function THCFloatLineItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vNewLeftTop: TPoint;
begin
  // inherited;
  if Self.Resizing then
  begin
    Self.Resizing := False;

    // 缩放后的Rect的LeftTop
    if FShapeLine.StartPt.X < FShapeLine.EndPt.X then
      vNewLeftTop.X := FShapeLine.StartPt.X
    else
      vNewLeftTop.X := FShapeLine.EndPt.X;

    if FShapeLine.StartPt.Y < FShapeLine.EndPt.Y then
      vNewLeftTop.Y := FShapeLine.StartPt.Y
    else
      vNewLeftTop.Y := FShapeLine.EndPt.Y;

    vNewLeftTop.X := vNewLeftTop.X - FLeftTop.X;
    vNewLeftTop.Y := vNewLeftTop.Y - FLeftTop.Y;

    Self.Left := Self.Left + vNewLeftTop.X;
    Self.Top := Self.Top + vNewLeftTop.Y;
    // 线的点坐标以新LeftTop为原点
    FShapeLine.StartPt.Offset(-vNewLeftTop.X, -vNewLeftTop.Y);
    FShapeLine.EndPt.Offset(-vNewLeftTop.X, -vNewLeftTop.Y);

    Self.Width := Abs(FShapeLine.EndPt.X - FShapeLine.StartPt.X);
    Self.Height := Abs(FShapeLine.EndPt.Y - FShapeLine.StartPt.Y);
  end;

  Result := FShapeLine.MouseUp(Button, Shift, X, Y);
end;

procedure THCFloatLineItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FShapeLine.ParseXml(ANode);
end;

function THCFloatLineItem.PointInClient(const APoint: TPoint): Boolean;
begin
  Result := FShapeLine.PointInClient(APoint);
end;

procedure THCFloatLineItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  FShapeLine.SaveToStream(AStream);
end;

procedure THCFloatLineItem.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  FShapeLine.Active := Self.Active;
end;

procedure THCFloatLineItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  FShapeLine.ToXml(ANode);
end;

end.
