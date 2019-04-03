{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{         文档PageBreakItem(分页）对象实现单元          }
{                                                       }
{*******************************************************}

unit HCPageBreakItem;

interface

uses
  Windows, HCRectItem, HCStyle, HCCommon, HCCustomData;

type
  TPageBreakItem = class(THCCustomRectItem)
  protected
    function JustifySplit: Boolean; override;
    //function GetOffsetAt(const X: Integer): Integer; override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
  end;

implementation

{ TPageBreakItem }

constructor TPageBreakItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  StyleNo := THCStyle.PageBreak;

  Width := 0;
  if AOwnerData.CurStyleNo > THCStyle.Null then
  begin
    AOwnerData.Style.ApplyTempStyle(AOwnerData.CurStyleNo);
    Height := AOwnerData.Style.TextStyles[AOwnerData.CurStyleNo].FontHeight;
  end
  else
  begin
    AOwnerData.Style.ApplyTempStyle(0);
    Height := AOwnerData.Style.TextStyles[0].FontHeight;
  end;
end;

{function TPageBreakItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X < 0 then
    Result := OffsetBefor
  else
    Result := OffsetAfter;
end;}

function TPageBreakItem.JustifySplit: Boolean;
begin
  Result := False;
end;

end.
