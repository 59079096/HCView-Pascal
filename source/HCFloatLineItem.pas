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
    function GetShapeLeftTop: TPoint;
  protected
    procedure SetActive(const Value: Boolean); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    function PointInClient(const X, Y: Integer): Boolean; override;
    procedure Assign(Source: THCCustomItem); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
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
  Self.StyleNo := THCStyle.FloatLine;
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

function THCFloatLineItem.GetShapeLeftTop: TPoint;
begin
  if FShapeLine.StartPt.X < FShapeLine.EndPt.X then
    Result.X := FShapeLine.StartPt.X
  else
    Result.X := FShapeLine.EndPt.X;

  if FShapeLine.StartPt.Y < FShapeLine.EndPt.Y then
    Result.Y := FShapeLine.StartPt.Y
  else
    Result.Y := FShapeLine.EndPt.Y;
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
  Active := FShapeLine.ActiveObj <> THCShapeLineObj.sloNone;
  if Active then
  begin
    if Button = mbLeft then
    begin
      Self.Resizing := FShapeLine.ActiveObj in [sloStart, sloEnd];
      if Self.Resizing then  // 开始缩放
      begin
        Self.FResizeX := X;
        Self.FResizeY := Y;
        FLeftTop := GetShapeLeftTop;  // 缩放前的Rect的LeftTop
      end
      else
      if FShapeLine.ActiveObj = sloLine then  // 按下在直线上，拖拽开始，记录鼠标点
        FLeftTop := GetShapeLeftTop;  // 移动前的Rect的LeftTop
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

  procedure _CalcNewLeftTop;
  var
    vNewLeftTop: TPoint;
  begin
    vNewLeftTop := GetShapeLeftTop; // 缩放后的Rect的LeftTop

    Self.Left := Self.Left + vNewLeftTop.X - FLeftTop.X;
    Self.Top := Self.Top + vNewLeftTop.Y - FLeftTop.Y;

    // 线的点坐标以新LeftTop为原点
    FShapeLine.StartPt.Offset(-vNewLeftTop.X, -vNewLeftTop.Y);
    FShapeLine.EndPt.Offset(-vNewLeftTop.X, -vNewLeftTop.Y);
  end;

begin
  // inherited;
  if Self.Resizing then
  begin
    Self.Resizing := False;
    _CalcNewLeftTop;  // 计算新的LeftTop

    Self.Width := Abs(FShapeLine.EndPt.X - FShapeLine.StartPt.X);
    Self.Height := Abs(FShapeLine.EndPt.Y - FShapeLine.StartPt.Y);
  end
  else
  if FShapeLine.ActiveObj = THCShapeLineObj.sloLine then  // 按下是拖动
    _CalcNewLeftTop;  // 计算新的LeftTop

  Result := FShapeLine.MouseUp(Button, Shift, X, Y);
end;

procedure THCFloatLineItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FShapeLine.ParseXml(ANode);
end;

function THCFloatLineItem.PointInClient(const X, Y: Integer): Boolean;
begin
  Result := FShapeLine.PointInClient(X, Y);
end;

procedure THCFloatLineItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
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
