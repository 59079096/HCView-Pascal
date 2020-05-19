{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2019-8-19             }
{                                                       }
{  文档THCFloatBarCodeItem（浮动一维码）对象实现单元    }
{                                                       }
{*******************************************************}

unit HCFloatBarCodeItem;

interface

uses
  Windows, Classes, Controls, SysUtils, Graphics, HCStyle, HCCustomData, HCXml,
  HCItem, HCCustomFloatItem, HCCode128, HCCommon;

type
  THCFloatBarCodeItem = class(THCCustomFloatItem)
  private
    FCode128: THCCode128;
    FAutoSize: Boolean;  // 无用的占位变量
    procedure DoCodeWidthChanged(Sender: TObject);
    function GetPenWidth: Byte;
    procedure SetPenWidth(const Value: Byte);
    function GetShowText: Boolean;
    procedure SetShowText(const Value: Boolean);
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetHeight(const Value: Integer); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property PenWidth: Byte read GetPenWidth write SetPenWidth;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property ShowText: Boolean read GetShowText write SetShowText;
  end;

implementation

{ THCFloatBarCodeItem }

procedure THCFloatBarCodeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FCode128.Text := (Source as THCFloatBarCodeItem).Text;
end;

constructor THCFloatBarCodeItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := Ord(THCStyle.FloatBarCode);
  FCode128 := THCCode128.Create('123456');
  FCode128.OnWidthChanged := DoCodeWidthChanged;
  Width := FCode128.Width;
  Height := 100;
end;

destructor THCFloatBarCodeItem.Destroy;
begin
  FreeAndNil(FCode128);
  inherited Destroy;
end;

procedure THCFloatBarCodeItem.DoCodeWidthChanged(Sender: TObject);
begin
  Self.Width := FCode128.Width;
end;

procedure THCFloatBarCodeItem.DoPaint(const AStyle: THCStyle;
  const ADrawRect: TRect; const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  FCode128.PaintTo(ACanvas, ADrawRect);
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);
end;

function THCFloatBarCodeItem.GetPenWidth: Byte;
begin
  Result := FCode128.Zoom;
end;

function THCFloatBarCodeItem.GetShowText: Boolean;
begin
  Result := FCode128.TextVisible;
end;

function THCFloatBarCodeItem.GetText: string;
begin
  Result := FCode128.Text;
end;

procedure THCFloatBarCodeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vText: string;
  vShowText: Boolean;
  vPenWidth: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vText, AFileVersion);
  FCode128.Text := vText;
  if AFileVersion > 34 then
  begin
    AStream.ReadBuffer(FAutoSize, SizeOf(FAutoSize));
    AStream.ReadBuffer(vShowText, SizeOf(vShowText));
    FCode128.TextVisible := vShowText;
    AStream.ReadBuffer(vPenWidth, SizeOf(vPenWidth));
    FCode128.Zoom := vPenWidth;
  end;
end;

procedure THCFloatBarCodeItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FCode128.Text := ANode.Text;

  if ANode.HasAttribute('autosize') then
    FAutoSize := ANode.Attributes['autosize']
  else
    FAutoSize := True;

  if ANode.HasAttribute('showtext') then
    FCode128.TextVisible := ANode.Attributes['showtext']
  else
    FCode128.TextVisible := True;

  if ANode.HasAttribute('penwidth') then
    FCode128.Zoom := ANode.Attributes['penwidth']
  else
    FCode128.Zoom := 1;
end;

procedure THCFloatBarCodeItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vShowText: Boolean;
  vPenWidth: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FCode128.Text);
  AStream.WriteBuffer(FAutoSize, SizeOf(FAutoSize));
  vShowText := FCode128.TextVisible;
  AStream.WriteBuffer(vShowText, SizeOf(vShowText));
  vPenWidth := FCode128.Zoom;
  AStream.WriteBuffer(vPenWidth, SizeOf(vPenWidth));
end;

procedure THCFloatBarCodeItem.SetHeight(const Value: Integer);
begin
  inherited SetHeight(Value);
  FCode128.Height := Self.Height;
end;

procedure THCFloatBarCodeItem.SetPenWidth(const Value: Byte);
begin
  FCode128.Zoom := Value;
end;

procedure THCFloatBarCodeItem.SetShowText(const Value: Boolean);
begin
  FCode128.TextVisible := Value;
end;

procedure THCFloatBarCodeItem.SetText(const Value: string);
begin
  FCode128.Text := Value;
end;

procedure THCFloatBarCodeItem.SetWidth(const Value: Integer);
begin
  inherited SetWidth(FCode128.Width);
end;

procedure THCFloatBarCodeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Text := FCode128.Text;
  ANode.Attributes['autosize'] := FAutoSize;
  ANode.Attributes['showtext'] := FCode128.TextVisible;
  ANode.Attributes['penwidth'] := FCode128.Zoom;
end;

end.
