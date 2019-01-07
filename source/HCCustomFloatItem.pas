{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-8-16             }
{                                                       }
{            文档FloatItem(浮动)对象实现单元            }
{                                                       }
{*******************************************************}

unit HCCustomFloatItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Messages, HCItem, HCRectItem,
  HCStyle, HCCustomData, HCXml;

const
  PointSize = 5;

type
  THCCustomFloatItem = class(THCResizeRectItem)  // 可浮动Item
  private
    FLeft, FTop, FPageIndex: Integer;
    FDrawRect: TRect;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    function PtInClient(const APoint: TPoint): Boolean; overload; virtual;
    function PtInClient(const X, Y: Integer): Boolean; overload;
    procedure Assign(Source: THCCustomItem); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property DrawRect: TRect read FDrawRect write FDrawRect;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property PageIndex: Integer read FPageIndex write FPageIndex;
  end;

implementation

{ THCCustomFloatItem }

procedure THCCustomFloatItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FLeft := (Source as THCCustomFloatItem).Left;
  FTop := (Source as THCCustomFloatItem).Top;
  Width := (Source as THCCustomFloatItem).Width;
  Height := (Source as THCCustomFloatItem).Height;
end;

constructor THCCustomFloatItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  //Self.StyleNo := THCStyle.FloatItem;
end;

function THCCustomFloatItem.PtInClient(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(Bounds(0, 0, Width, Height), APoint);
end;

procedure THCCustomFloatItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  //inherited;
  if Self.Active then
    ACanvas.DrawFocusRect(FDrawRect);
end;

procedure THCCustomFloatItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vValue: Integer;
begin
  //AStream.ReadBuffer(StyleNo, SizeOf(StyleNo));  // 加载时先读取并根据值创建
  AStream.ReadBuffer(FLeft, SizeOf(FLeft));
  AStream.ReadBuffer(FTop, SizeOf(FTop));

  AStream.ReadBuffer(vValue, SizeOf(vValue));
  Width := vValue;
  AStream.ReadBuffer(vValue, SizeOf(vValue));
  Height := vValue;
end;

procedure THCCustomFloatItem.ParseXml(const ANode: IHCXMLNode);
begin
  StyleNo := ANode.Attributes['sno'];
  FLeft := ANode.Attributes['left'];
  FTop := ANode.Attributes['top'];
  Width := ANode.Attributes['width'];
  Height := ANode.Attributes['height'];
end;

function THCCustomFloatItem.PtInClient(const X, Y: Integer): Boolean;
begin
  Result := PtInClient(Point(X, Y));
end;

procedure THCCustomFloatItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vValue: Integer;
begin
  AStream.WriteBuffer(Self.StyleNo, SizeOf(Self.StyleNo));
  AStream.WriteBuffer(FLeft, SizeOf(FLeft));
  AStream.WriteBuffer(FTop, SizeOf(FTop));

  vValue := Width;
  AStream.WriteBuffer(vValue, SizeOf(vValue));
  vValue := Height;
  AStream.WriteBuffer(vValue, SizeOf(vValue));
end;

procedure THCCustomFloatItem.ToXml(const ANode: IHCXMLNode);
begin
  ANode.Attributes['sno'] := StyleNo;
  ANode.Attributes['left'] := FLeft;
  ANode.Attributes['top'] := FTop;
  ANode.Attributes['width'] := Width;
  ANode.Attributes['height'] := Height;
end;

end.
