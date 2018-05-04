{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{              HCView中List类实现单元                   }
{                                                       }
{*******************************************************}

unit HCList;

interface

uses
  SysUtils;

const
  MaxListSize = Maxint div 16;

type
  PIntegerList = ^TIntegerList;
  TIntegerList = array[0..MaxListSize - 1] of Integer;

  THCList = class(TObject)
  private
    FList: PIntegerList;
    FCount: Integer;
    FCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
    function GetItems(Index: Integer): Integer;
    procedure SetItems(Index: Integer; const Value: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Integer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property List: PIntegerList read FList;
    property Items[Index: Integer]: Integer read GetItems write SetItems; default;
  end;

implementation

{ THCList }

function THCList.Add(Item: Integer): Integer;
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + 4);
  FList^[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure THCList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure THCList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('非法的 Index:%d', [Index]);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Integer));
  Dec(FCount);
end;

destructor THCList.Destroy;
begin
  Clear;
  inherited;
end;

procedure THCList.SetCapacity(const Value: Integer);
begin
  if (Value < FCount) or (Value > MaxListSize) then
    raise Exception.CreateFmt('非法数据:%d', [Value]);
  if FCapacity <> Value then
  begin
    ReallocMem(FList, Value * SizeOf(Integer));
    FCapacity := Value;
  end;
end;

procedure THCList.SetCount(const Value: Integer);
var
  i: Integer;
begin
  if (Value < 0) or (Value > MaxListSize) then
    raise Exception.CreateFmt('非法数据:%d', [Value]);
  if Value > FCapacity then
    SetCapacity(Value);
  if Value > FCount then
    FillChar(FList^[FCount], (Value - FCount) * SizeOf(Integer), 0)
  else
  begin
    for i := FCount - 1 downto Value do
      Delete(I);
  end;
  FCount := Value;
end;

function THCList.GetItems(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('异常:%d', [Index]);
  Result := FList^[Index];
end;

procedure THCList.SetItems(Index: Integer; const Value: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('异常:%d', [Index]);

  if Value <> FList^[Index] then
    FList^[Index] := Value;
end;

end.
