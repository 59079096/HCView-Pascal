{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                 文档Tab对象实现单元                   }
{                                                       }
{*******************************************************}

unit HCTabItem;

interface

uses
  Windows, Controls, Classes, Graphics, HCItem, HCRectItem, HCStyle, HCCommon,
  HCCustomData;

type
  TTabItem = class(THCTextRectItem)
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    function JustifySplit: Boolean; override;
    function GetOffsetAt(const X: Integer): Integer; override;
  end;

implementation

{ TTabItem }

constructor TTabItem.Create(const AOwnerData: THCCustomData);
var
  vSize: TSize;
begin
  inherited Create(AOwnerData);
  StyleNo := THCStyle.Tab;
  AOwnerData.Style.ApplyTempStyle(TextStyleNo);
  vSize := AOwnerData.Style.TempCanvas.TextExtent('汉字');  // 默认2个汉字
  Width := vSize.cx;
  Height := vSize.cy;
end;

procedure TTabItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited;
  {if SelectComplate then
  begin
    ACanvas.Brush.Color := GStyle.SelColor;
    ACanvas.FillRect(ADrawRect);
  end;}
end;

function TTabItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X < Width div 2 then
    Result := OffsetBefor
  else
    Result := OffsetAfter;
end;

function TTabItem.JustifySplit: Boolean;
begin
  Result := False;
end;

end.
