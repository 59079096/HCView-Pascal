{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文档节对象管理公共单元                 }
{                                                       }
{*******************************************************}

unit HCDataCommon;

interface

uses
  HCCustomData;

type
  TTraverseItemEvent = procedure(const AData: THCCustomData;
    const AItemNo, ATag: Integer; var AStop: Boolean) of object;

  TItemTraverse = class(TObject)
  public
    Tag: Integer;
    Stop: Boolean;
    Process: TTraverseItemEvent;
  end;

implementation

end.
