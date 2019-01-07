{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{          文档QRCodeItem(二维码)对象实现单元           }
{                                                       }
{*******************************************************}

unit HCQRCodeItem;

interface

uses
  Windows, Graphics, Classes, SysUtils, HCStyle, HCItem, HCRectItem, HCCustomData,
  HCCommon, DelphiZXIngQRCode, HCXml;

type
  THCQRCodeItem = class(THCResizeRectItem)
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

{ THCQRCodeItem }

constructor THCQRCodeItem.Create(const AOwnerData: THCCustomData; const AText: string);
{var
  vQRCode: TDelphiZXingQRCode;}
begin
  inherited Create(AOwnerData);
  StyleNo := THCStyle.QRCode;
  FText := AText;

  Width := 100;
  Height := 100;

  {vQRCode := TDelphiZXingQRCode.Create;
  try
    vQRCode.Data := FText;
    //vQRCode.Encoding := TQRCodeEncoding.qrAuto;
    //vQRCode.QuietZone := 4;

    Width := vQRCode.Columns;
    Height := vQRCode.Rows;
  finally
    vQRCode.Free;
  end;}
end;

destructor THCQRCodeItem.Destroy;
begin
  inherited Destroy;
end;

procedure THCQRCodeItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vQRCode: TDelphiZXingQRCode;
  vRow, vColumn: Integer;
  vBitmap: TBitmap;
begin
  vBitmap := TBitmap.Create;
  try
    vQRCode := TDelphiZXingQRCode.Create;
    try
      vQRCode.QuietZone := 1;  // 边距空白区
      vQRCode.Data := FText;
      //vQRCode.Encoding := TQRCodeEncoding.qrAuto;
      vBitmap.SetSize(vQRCode.Rows, vQRCode.Columns);
      for vRow := 0 to vQRCode.Rows - 1 do
      begin
        for vColumn := 0 to vQRCode.Columns - 1 do
        begin
          if vQRCode.IsBlack[vRow, vColumn] then
            vBitmap.Canvas.Pixels[vColumn, vRow] := clBlack
          else
            vBitmap.Canvas.Pixels[vColumn, vRow] := clWhite;
        end;
      end;
    finally
      vQRCode.Free;
    end;

    ACanvas.StretchDraw(ADrawRect, vBitmap);
  finally
    vBitmap.Free;
  end;

  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);
end;

function THCQRCodeItem.GetText: string;
begin
  Result := FText;
end;

procedure THCQRCodeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vSize: Word;
  vBuffer: TBytes;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);

  AStream.ReadBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    SetLength(vBuffer, vSize);
    AStream.ReadBuffer(vBuffer[0], vSize);
    FText := StringOf(vBuffer);
  end;
end;

procedure THCQRCodeItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FText := ANode.Text;
end;

procedure THCQRCodeItem.RestrainSize(const AWidth, AHeight: Integer);
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

procedure THCQRCodeItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FText);
end;

procedure THCQRCodeItem.SetText(const Value: string);
begin
  if FText <> Value then
    FText := Value;
end;

procedure THCQRCodeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Text := FText;
end;

end.
