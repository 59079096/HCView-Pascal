{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
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
  Windows, Classes, Graphics, HCStyle, HCItem, HCRectItem, HCCustomData, HCXml,
  HCRichData;

type
  THCLineItem = class(THCCustomRectItem)
  private
    FLineHeight: byte;
    FLineStyle: TPenStyle;
  protected
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AWidth, AHeight: Integer); override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property LineStyle: TPenStyle read FLineStyle write FLineStyle;
    property LineHeight: byte read FLineHeight write FLineHeight;
  end;

implementation

{ THCLineItem }

procedure THCLineItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FLineHeight := (Source as THCLineItem).LineHeight;
  FLineStyle := (Source as THCLineItem).LineStyle;
end;

constructor THCLineItem.Create(const AOwnerData: THCCustomData; const AWidth, AHeight: Integer);
begin
  inherited Create(AOwnerData);
  FLineHeight := 1;
  Width := AWidth;
  Height := AHeight;
  FLineStyle := TPenStyle.psSolid;
  StyleNo := THCStyle.Line;
end;

procedure THCLineItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vTop: Integer;
begin
  ACanvas.Pen.Width := FLineHeight;
  ACanvas.Pen.Style := FLineStyle;
  ACanvas.Pen.Color := clBlack;
  vTop := (ADrawRect.Top + ADrawRect.Bottom) div 2;
  ACanvas.MoveTo(ADrawRect.Left, vTop);
  ACanvas.LineTo(ADrawRect.Right, vTop);
end;

procedure THCLineItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
begin
  Width := THCRichData(ARichData).Width;
  Height := FLineHeight;
end;

function THCLineItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X < Width div 2 then
    Result := OffsetBefor
  else
    Result := OffsetAfter;
end;

procedure THCLineItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FLineHeight, SizeOf(FLineHeight));
  AStream.ReadBuffer(FLineStyle, SizeOf(FLineStyle));
end;

procedure THCLineItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FLineHeight := ANode.Attributes['height'];
  FLineStyle := TPenStyle(ANode.Attributes['style']);
end;

procedure THCLineItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FLineHeight, SizeOf(FLineHeight));
  AStream.WriteBuffer(FLineStyle, SizeOf(FLineStyle));
end;

procedure THCLineItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['height'] := FLineHeight;
  ANode.Attributes['style'] := Ord(FLineStyle);
end;

end.
