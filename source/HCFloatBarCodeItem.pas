unit HCFloatBarCodeItem;

interface

uses
  Windows, Classes, Controls, SysUtils, Graphics, HCStyle, HCCustomData, HCXml,
  HCItem, HCCustomFloatItem, HCCode128B, HCCommon;

type
  THCFloatBarCodeItem = class(THCCustomFloatItem)
  private
    FText: string;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
  end;

implementation

{ THCFloatBarCodeItem }

procedure THCFloatBarCodeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FText := (Source as THCFloatBarCodeItem).Text;
end;

constructor THCFloatBarCodeItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := Ord(THCStyle.FloatBarCode);
  Width := 80;
  Height := 60;
  SetText('0000');
end;

procedure THCFloatBarCodeItem.DoPaint(const AStyle: THCStyle;
  const ADrawRect: TRect; const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vCode128B: THCCode128B;
  vBitmap: TBitmap;
begin
  vBitmap := TBitmap.Create;
  try
    vCode128B := THCCode128B.Create;
    try
      vCode128B.Margin := 2;
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

function THCFloatBarCodeItem.GetText: string;
begin
  Result := FText;
end;

procedure THCFloatBarCodeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FText, AFileVersion);
end;

procedure THCFloatBarCodeItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FText := ANode.Text;
end;

procedure THCFloatBarCodeItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FText);
end;

procedure THCFloatBarCodeItem.SetText(const Value: string);
var
  vBarCode: THCCode128B;
begin
  if FText <> Value then
  begin
    FText := Value;

    vBarCode := THCCode128B.Create;
    try
      vBarCode.Margin := 2;
      vBarCode.CodeKey := FText;
      Width := vBarCode.Width;
    finally
      vBarCode.Free;
    end;
  end;
end;

procedure THCFloatBarCodeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Text := FText;
end;

end.
