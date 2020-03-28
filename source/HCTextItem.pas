{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文本类的HCItem基类单元                 }
{                                                       }
{*******************************************************}

unit HCTextItem;

interface

uses
  Windows, Classes, SysUtils, Graphics, HCStyle, HCItem, HCXml;

type
  THCTextItemClass = class of THCTextItem;

  THCTextItem = class(THCCustomItem)
  private
    FText, FHyperLink: string;
    //FOwnerData: THCCustomData;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function GetHyperLink: string; override;
    procedure SetHyperLink(const Value: string); override;
    function GetLength: Integer; override;
  public
    constructor CreateByText(const AText: string); virtual;
    procedure Assign(Source: THCCustomItem); override;
    function BreakByOffset(const AOffset: Integer): THCCustomItem; override;
    function CanConcatItems(const AItem: THCCustomItem): Boolean; override;

    // 保存和读取
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;

    function ToHtml(const APath: string): string; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summaryy 复制一部分文本 </summary>
    /// <param name="AStartOffs">复制的起始位置(大于0)</param>
    /// <param name="ALength">众起始位置起复制的长度</param>
    /// <returns>文本内容</returns>
    function SubString(const AStartOffs, ALength: Integer): string;
    //property OwnerData: THCCustomData read FOwnerData write FOwnerData;
  end;

var
  HCDefaultTextItemClass: THCTextItemClass = THCTextItem;

implementation

uses
  HCCommon, HCTextStyle;

{ THCTextItem }

function THCTextItem.CanConcatItems(const AItem: THCCustomItem): Boolean;
begin
  Result := inherited CanConcatItems(AItem);
  if Result then
    Result := FHyperLink = AItem.HyperLink;
end;

constructor THCTextItem.CreateByText(const AText: string);
begin
  Create;  // 这里如果 inherited Create; 则调用THCCustomItem的Create，子类TEmrTextItem调用CreateByText时不能执行自己的Create
  FText := AText;
  FHyperLink := '';
end;

procedure THCTextItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FText := (Source as THCTextItem).Text;
  FHyperLink := (Source as THCTextItem).HyperLink;
end;

function THCTextItem.BreakByOffset(const AOffset: Integer): THCCustomItem;
begin
  if (AOffset >= Length) or (AOffset <= 0) then
    Result := nil
  else
  begin
    Result := inherited BreakByOffset(AOffset);
    Result.Text := Self.SubString(AOffset + 1, Length - AOffset);
    Delete(FText, AOffset + 1, Length - AOffset);  // 当前Item减去光标后的字符串
  end;
end;

function THCTextItem.GetHyperLink: string;
begin
  Result := FHyperLink;
end;

function THCTextItem.GetLength: Integer;
begin
  Result := System.Length(FText);
end;

function THCTextItem.GetText: string;
begin
  Result := FText;
end;

function THCTextItem.SubString(const AStartOffs, ALength: Integer): string;
begin
  Result := Copy(FText, AStartOffs, ALength);
end;

procedure THCTextItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vSize: Word;
  vDSize: DWORD;
  vBuffer: TBytes;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion < 11 then  // 兼容65536级别的字符数量
  begin
    AStream.ReadBuffer(vSize, SizeOf(Word));
    vDSize := vSize;
  end
  else
    AStream.ReadBuffer(vDSize, SizeOf(DWORD));

  if vDSize > 0 then
  begin
    SetLength(vBuffer, vDSize);
    AStream.Read(vBuffer[0], vDSize);

    if AFileVersion > 24 then
      FText := TEncoding.Unicode.GetString(vBuffer)
    else
      FText := StringOf(vBuffer);
  end;

  if AFileVersion > 34 then
    HCLoadTextFromStream(AStream, FHyperLink, AFileVersion)
  else
    FHyperLink := '';
end;

procedure THCTextItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FHyperLink := ANode.Attributes['link'];
  FText := ANode.Text;
end;

procedure THCTextItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
var
  vS: string;
  vBuffer: TBytes;
  vSize: DWORD;
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  vS := SubString(AStart + 1, AEnd - AStart);
  //  DWORD大小不能用HCSaveTextToStream(AStream, vS);
  vBuffer := TEncoding.Unicode.GetBytes(vS);
  if System.Length(vBuffer) > HC_TEXTMAXSIZE then
    raise Exception.Create(HCS_EXCEPTION_TEXTOVER);

  vSize := System.Length(vBuffer);
  AStream.WriteBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
    AStream.WriteBuffer(vBuffer[0], vSize);

  HCSaveTextToStream(AStream, FHyperLink);
end;

procedure THCTextItem.SetHyperLink(const Value: string);
begin
  FHyperLink := Value;
end;

procedure THCTextItem.SetText(const Value: string);
begin
  //if Value <> '' then  // 如果判断了，会影响空变为有字符后的撤销时的赋值
  FText := HCDeleteBreak(Value);
end;

function THCTextItem.ToHtml(const APath: string): string;
begin
  Result := '<a class="fs' + IntToStr(StyleNo) + '">' + Text + '</a>';
end;

procedure THCTextItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['link'] := FHyperLink;
  ANode.Text := Text;
end;

end.
