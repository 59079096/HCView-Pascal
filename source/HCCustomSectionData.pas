{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文档节对象基本管理单元                 }
{                                                       }
{*******************************************************}

unit HCCustomSectionData;

interface

uses
  Classes, HCRichData;

type
  // 用于文档页眉、页脚、页面Data基类，主要用于处理文档级Data变化时特有的属性或事件
  // 如只读状态切换，页眉、页脚、页面切换时需要通知外部控件以做界面控件状态变化，
  // 而单元格只读切换时不需要
  THCCustomSectionData = class(THCRichData)
  private
    FOnReadOnlySwitch: TNotifyEvent;
  protected
    procedure SetReadOnly(const Value: Boolean); override;
  public
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;
  end;

  THCHeaderData = class(THCCustomSectionData);

  THCFooterData = class(THCCustomSectionData);

implementation

{ THCCustomSectionData }

procedure THCCustomSectionData.SetReadOnly(const Value: Boolean);
begin
  if Self.ReadOnly <> Value then
  begin
    inherited SetReadOnly(Value);

    if Assigned(FOnReadOnlySwitch) then
      FOnReadOnlySwitch(Self);
  end;
end;

end.
