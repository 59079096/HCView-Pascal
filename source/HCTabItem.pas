{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  Windows, Controls, Classes, Graphics, HCItem, HCRectItem, HCStyle, HCCommon;

type
  TTabItem = class(THCCustomRectItem)
  protected
    function JustifySplit: Boolean; override;
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect; const ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom: Integer; const ACanvas: TCanvas;
      const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AWidth, AHeight: Integer); override;
  end;

implementation

{ TTabItem }

constructor TTabItem.Create(const AWidth, AHeight: Integer);
begin
  inherited Create;
  StyleNo := THCStyle.RsTab;
  Width := AWidth;
  Height := AHeight;
end;

procedure TTabItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
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
