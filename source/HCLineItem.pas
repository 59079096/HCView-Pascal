{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{           文档LineItem(直线)对象实现单元              }
{                                                       }
{*******************************************************}

unit HCLineItem;

interface

uses
  Windows, Classes, Graphics, HCStyle, HCItem, HCRectItem;

type
  TLineItem = class(THCCustomRectItem)
  private
    FLineHeght: byte;
    FLineStyle: TPenStyle;
  protected
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
  public
    constructor Create(const AWidth, AHeight: Integer); override;
    property LineStyle: TPenStyle read FLineStyle write FLineStyle;
    property LineHeght: byte read FLineHeght write FLineHeght;
  end;

implementation

{ TLineItem }

constructor TLineItem.Create(const AWidth, AHeight: Integer);
begin
  inherited;
  FLineHeght := 1;
  FLineStyle := TPenStyle.psSolid;
  StyleNo := THCStyle.RsLine;
end;

procedure TLineItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vTop: Integer;
begin
  ACanvas.Pen.Width := FLineHeght;
  ACanvas.Pen.Style := FLineStyle;
  ACanvas.Pen.Color := clBlack;
  vTop := (ADrawRect.Top + ADrawRect.Bottom) div 2;
  ACanvas.MoveTo(ADrawRect.Left, vTop);
  ACanvas.LineTo(ADrawRect.Right, vTop);
end;

function TLineItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X < Width div 2 then
    Result := OffsetBefor
  else
    Result := OffsetAfter;
end;

procedure TLineItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FLineHeght, SizeOf(FLineHeght));
  AStream.ReadBuffer(FLineStyle, SizeOf(FLineStyle));
end;

procedure TLineItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FLineHeght, SizeOf(FLineHeght));
  AStream.WriteBuffer(FLineStyle, SizeOf(FLineStyle));
end;

end.
