{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{          �ĵ�QRCodeItem(��ά��)����ʵ�ֵ�Ԫ           }
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
    constructor Create(const AOwnerData: THCCustomData; const AText: string); virtual;
    destructor Destroy; override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summary> Լ����ָ����С��Χ�� </summary>
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
      vQRCode.QuietZone := 1;  // �߾�հ���
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
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FText, AFileVersion);
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

procedure THCQRCodeItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
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
