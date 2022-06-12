{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{         �ĵ�BarCodeItem(һά��)����ʵ�ֵ�Ԫ           }
{                                                       }
{*******************************************************}

unit HCBarCodeItem;

interface

uses
  Windows, Graphics, Classes, SysUtils, HCStyle, HCItem, HCRectItem, HCCustomData,
  HCCommon, HCXml, HCCode128;

type
  THCBarCodeItem = class(THCResizeRectItem)
  private
    FCode128: THCCode128;
  protected
    procedure DoCodeWidthChanged(Sender: TObject);
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetHeight(const Value: Integer); override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); virtual;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summary> Լ����ָ����С��Χ�� </summary>
    procedure RestrainSize(const AWidth, AHeight: Integer); override;
  end;

implementation

{ THCBarCodeItem }

procedure THCBarCodeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FCode128.Text := (Source as THCBarCodeItem).Text;
end;

constructor THCBarCodeItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  inherited Create(AOwnerData);
  StyleNo := THCStyle.BarCode;
  FCode128 := THCCode128.Create(AText);
  FCode128.OnWidthChanged := DoCodeWidthChanged;
  Width := FCode128.Width;
  Height := 100;
end;

destructor THCBarCodeItem.Destroy;
begin
  FreeAndNil(FCode128);
  inherited Destroy;
end;

procedure THCBarCodeItem.DoCodeWidthChanged(Sender: TObject);
begin
  Self.Width := FCode128.Width;
end;

procedure THCBarCodeItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  FCode128.PaintTo(ACanvas, ADrawRect);
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);
end;

function THCBarCodeItem.GetText: string;
begin
  Result := FCode128.Text;
end;

procedure THCBarCodeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vText: string;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vText, AFileVersion);
  FCode128.Text := vText;
end;

procedure THCBarCodeItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FCode128.Text := ANode.Text;
end;

procedure THCBarCodeItem.RestrainSize(const AWidth, AHeight: Integer);
begin
  if Height > AHeight then
    Height := AHeight;
end;

procedure THCBarCodeItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FCode128.Text);
end;

procedure THCBarCodeItem.SetHeight(const Value: Integer);
begin
  inherited SetHeight(Value);
  FCode128.Height := Self.Height;
end;

procedure THCBarCodeItem.SetText(const Value: string);
begin
  FCode128.Text := Value;
end;
procedure THCBarCodeItem.SetWidth(const Value: Integer);
begin
  inherited SetWidth(FCode128.Width);
end;

procedure THCBarCodeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Text := FCode128.Text;
end;

end.
