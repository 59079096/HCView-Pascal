{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{         文档BarCodeItem(一维码)对象实现单元           }
{                                                       }
{*******************************************************}

unit HCBarCodeItem;

interface

uses
  Windows, Graphics, Classes, SysUtils, HCStyle, HCItem, HCRectItem, HCCustomData,
  HCCommon, HCCode128B, HCXml;

type
  THCBarCodeItem = class(THCResizeRectItem)
  private
    FText: string;
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string);
    destructor Destroy; override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summary> 约束到指定大小范围内 </summary>
    procedure RestrainSize(const AWidth, AHeight: Integer); override;
  end;

implementation

{ THCBarCodeItem }

constructor THCBarCodeItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  inherited Create(AOwnerData);
  StyleNo := THCStyle.BarCode;
  Width := 100;
  Height := 100;
  SetText(AText);
end;

destructor THCBarCodeItem.Destroy;
begin
  inherited Destroy;
end;

procedure THCBarCodeItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vCode128B: THCCode128B;
  vBitmap: TBitmap;
begin
  vBitmap := TBitmap.Create;
  try
    vCode128B := THCCode128B.Create;
    try
      vCode128B.Height := Height;
      vCode128B.CodeKey := FText;

      vBitmap.SetSize(vCode128B.Width, vCode128B.Height);
      vCode128B.PaintToEx(vBitmap.Canvas);

      ACanvas.StretchDraw(ADrawRect, vBitmap);
    finally
      FreeAndNil(vCode128B);
    end;
  finally
    vBitmap.Free;
  end;

  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);
end;

function THCBarCodeItem.GetText: string;
begin
  Result := FText;
end;

procedure THCBarCodeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FText);
end;

procedure THCBarCodeItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FText := ANode.Text;
end;

procedure THCBarCodeItem.RestrainSize(const AWidth, AHeight: Integer);
var
  vBL: Single;
begin
  if Width > AWidth then
  begin
    vBL := Width / AWidth;
    Width := AWidth;
    Height := Round(Height / vBL);
  end;

  if Height > AHeight then
  begin
    vBL := Height / AHeight;
    Height := AHeight;
    Width := Round(Width / vBL);
  end;
end;

procedure THCBarCodeItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FText);
end;

procedure THCBarCodeItem.SetText(const Value: string);
var
  vBarCode: THCCode128B;
begin
  if FText <> Value then
  begin
    FText := Value;

    vBarCode := THCCode128B.Create;
    try
      vBarCode.CodeKey := FText;
      Width := vBarCode.Width;
    finally
      vBarCode.Free;
    end;
  end;
end;

procedure THCBarCodeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Text := FText;
end;

end.
