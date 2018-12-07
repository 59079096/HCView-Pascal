{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文本类的HCItem基类单元                 }
{                                                       }
{*******************************************************}

unit HCControlItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCStyle, HCRectItem, HCItem, HCCustomData;

type
  THCControlItem = class(THCCustomRectItem)
  private
    FControl: TControl;
  public
    constructor Create(const AOwnerData: THCCustomData); overload; override;
  end;

implementation

{ THCControlItem }

constructor THCControlItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.Control;
end;

end.
