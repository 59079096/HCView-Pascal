{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{               文档对象对应的绘制对象                  }
{                                                       }
{*******************************************************}

unit HCDrawItem;

interface

uses
  Windows, Classes;

type
  TDrawOption = (doLineFirst, doLineLast, doParaFirst);
  TDrawOptions = set of TDrawOption;  //(doLineFrist, doParaFrist);

  THCCustomDrawItem = class(TPersistent)
  private
    FOptions: TDrawOptions;
  protected
    function GetLineFirst: Boolean;
    procedure SetLineFirst(const Value: Boolean);
    function GetParaFirst: Boolean;
    procedure SetParaFirst(const Value: Boolean);
  public
    ItemNo,    // 对应的Item
    /// <summary> 从第几个字符开始 >=1 </summary>
    CharOffs,
    /// <summary> 从CharOffs开始的字符长度 </summary>
    CharLen
      : Integer;
    /// <summary> 格式化区域 </summary>
    Rect: TRect;
    //
    function CharOffsetStart: Integer;
    function CharOffsetEnd: Integer;
    function Width: Integer;
    function Height: Integer;
    property LineFirst: Boolean read GetLineFirst write SetLineFirst;
    property ParaFirst: Boolean read GetParaFirst write SetParaFirst;
  end;

  THCDrawItems = class(TList)
  private
    // 格式化相关参数
    FDeleteStartDrawItemNo,
    FDeleteCount: Integer;
    function GetItem(Index: Integer): THCCustomDrawItem;
    procedure SetItem(Index: Integer; const Value: THCCustomDrawItem);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure Clear; override;
    /// <summary> 在格式化前标记要删除的起始和结束DrawItemNo </summary>
    procedure MarkFormatDelete(const AStartDrawItemNo, AEndDrawItemNo: Integer);

    /// <summary> 删除格式化前标记的起始和结束DrawItemNo </summary>
    procedure DeleteFormatMark;

    /// <summary> 初始化格式化参数 </summary>
    procedure ClearFormatMark;
    procedure DeleteRange(const AIndex, ACount: Integer);
    procedure Insert(const AIndex: Integer; const AItem: THCCustomDrawItem);
    function Last: THCCustomDrawItem;
    property Items[Index: Integer]: THCCustomDrawItem read GetItem write SetItem; default;
  end;

implementation

{ THCCustomDrawItem }

function THCCustomDrawItem.CharOffsetStart: Integer;
begin
  Result := CharOffs - 1;
end;

function THCCustomDrawItem.GetLineFirst: Boolean;
begin
  Result := doLineFirst in FOptions;
end;

function THCCustomDrawItem.GetParaFirst: Boolean;
begin
  Result := doParaFirst in FOptions;
end;

function THCCustomDrawItem.Height: Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function THCCustomDrawItem.CharOffsetEnd: Integer;
begin
  Result := CharOffs + CharLen - 1;
end;

procedure THCCustomDrawItem.SetLineFirst(const Value: Boolean);
begin
  if Value then
    Include(FOptions, doLineFirst)
  else
    Exclude(FOptions, doLineFirst);
end;

procedure THCCustomDrawItem.SetParaFirst(const Value: Boolean);
begin
  if Value then
    Include(FOptions, doParaFirst)
  else
    Exclude(FOptions, doParaFirst);
end;

function THCCustomDrawItem.Width: Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

{ THCDrawItems }

procedure THCDrawItems.Clear;
begin
  inherited Clear;
  ClearFormatMark;
end;

procedure THCDrawItems.ClearFormatMark;
begin
  FDeleteStartDrawItemNo := -1;
  FDeleteCount := 0;
end;

procedure THCDrawItems.DeleteFormatMark;
begin
  Self.DeleteRange(FDeleteStartDrawItemNo, FDeleteCount);
  FDeleteStartDrawItemNo := -1;
  FDeleteCount := 0;
end;

procedure THCDrawItems.DeleteRange(const AIndex, ACount: Integer);
var
  i, vEndIndex: Integer;
begin
  if ACount <> 0 then
  begin
    vEndIndex := AIndex + ACount - 1;
    if vEndIndex > Self.Count - 1 then
      vEndIndex := Self.Count - 1;
    for i := vEndIndex downto AIndex do
      Delete(i);
  end;
end;

function THCDrawItems.GetItem(Index: Integer): THCCustomDrawItem;
begin
  Result := THCCustomDrawItem(inherited Get(Index));
end;

procedure THCDrawItems.Insert(const AIndex: Integer; const AItem: THCCustomDrawItem);
begin
  if FDeleteCount = 0 then  // 需要删除的已经被下面替换完了
    inherited Insert(AIndex, AItem)
  else
  begin
    Assert(AIndex = FDeleteStartDrawItemNo);
    Inc(FDeleteStartDrawItemNo);
    Dec(FDeleteCount);
    Items[AIndex] := AItem;
  end;
end;

function THCDrawItems.Last: THCCustomDrawItem;
begin
  Result := GetItem(Self.Count - 1);
end;

procedure THCDrawItems.MarkFormatDelete(const AStartDrawItemNo, AEndDrawItemNo: Integer);
begin
  FDeleteStartDrawItemNo := AStartDrawItemNo;
  FDeleteCount := AEndDrawItemNo - AStartDrawItemNo + 1;
end;

procedure THCDrawItems.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = TListNotification.lnDeleted then
    THCCustomDrawItem(Ptr).Free;

  inherited Notify(Ptr, Action);
end;

procedure THCDrawItems.SetItem(Index: Integer; const Value: THCCustomDrawItem);
begin
  inherited Put(Index, Value);
end;

end.
