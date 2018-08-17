{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  Windows, SysUtils, Classes, Controls, Graphics, Messages, HCFloatItem, HCStyle,
  HCItem;

type
  THCFloatLineItem = class(THCFloatItem)  // 可浮动LineItem
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  end;

implementation

{ THCFloatLineItem }

procedure THCFloatLineItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Style := psSolid;

  ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Top);
  ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom);

  if Self.Active then
  begin
    ACanvas.Ellipse(ADrawRect.Left - 5, ADrawRect.Top - 5, ADrawRect.Left + 5, ADrawRect.Top + 5);
    ACanvas.Ellipse(ADrawRect.Right - 5, ADrawRect.Bottom - 5, ADrawRect.Right + 5, ADrawRect.Bottom + 5);
  end;
end;

end.
