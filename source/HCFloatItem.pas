{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-8-16             }
{                                                       }
{            文档FloatItem(浮动)对象实现单元            }
{                                                       }
{*******************************************************}

unit HCFloatItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Messages, HCItem, HCRectItem,
  HCStyle;

const
  PointSize = 5;

type
  THCFloatItem = class(THCResizeRectItem)  // 可浮动Item
  private
    FLeft, FTop: Integer;
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    function PtInClient(const X, Y: Integer): Boolean; overload;
    function PtInClient(const APoint: TPoint): Boolean; overload; virtual;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
  end;

implementation

{ THCFloatItem }

procedure THCFloatItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  {inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);}

  if Self.Active then
    ACanvas.DrawFocusRect(ADrawRect);
end;

function THCFloatItem.PtInClient(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(Bounds(0, 0, Width, Height), APoint);
end;

function THCFloatItem.PtInClient(const X, Y: Integer): Boolean;
begin
  Result := PtInClient(Point(X, Y));
end;

end.
