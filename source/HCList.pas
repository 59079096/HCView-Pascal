{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
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

  THCIntegerList = class(TObject)
  private
    FList: PIntegerList;
    FCount: Integer;
    FCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Value: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Integer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function IndexOf(const AItem: Integer): Integer;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property List: PIntegerList read FList;
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  THCObjectList = class(TObject)
  private
    FOwnsObjects: Boolean;
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; const Value: Pointer);
  public
    constructor Create(const AOwnsObjects: Boolean = True); virtual;
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function IndexOf(Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property List: PPointerList read FList;
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
  end;

implementation

{ THCIntegerList }

function THCIntegerList.Add(Item: Integer): Integer;
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + 4);
  FList^[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure THCIntegerList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure THCIntegerList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('非法的 Index:%d', [Index]);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Integer));
  Dec(FCount);
end;

destructor THCIntegerList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure THCIntegerList.SetCapacity(const Value: Integer);
begin
  if (Value < FCount) or (Value > MaxListSize) then
    raise Exception.CreateFmt('非法数据:%d', [Value]);
  if FCapacity <> Value then
  begin
    ReallocMem(FList, Value * SizeOf(Integer));
    FCapacity := Value;
  end;
end;

procedure THCIntegerList.SetCount(const Value: Integer);
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
      Delete(i);
  end;
  FCount := Value;
end;

function THCIntegerList.GetItem(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('异常:%d', [Index]);
  Result := FList^[Index];
end;

function THCIntegerList.IndexOf(const AItem: Integer): Integer;
var
  P: PInteger;
begin
  P := PInteger(FList);
  for Result := 0 to FCount - 1 do
  begin
    if P^ = AItem then
      Exit;
    Inc(P);
  end;
  Result := -1;
end;

procedure THCIntegerList.SetItem(Index: Integer; const Value: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('异常:%d', [Index]);

  if Value <> FList^[Index] then
    FList^[Index] := Value;
end;

{ THCObjectList }

function THCObjectList.Add(Item: Pointer): Integer;
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + 4);
  FList^[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure THCObjectList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

constructor THCObjectList.Create(const AOwnsObjects: Boolean = True);
begin
  FOwnsObjects := AOwnsObjects;
end;

procedure THCObjectList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('非法的 Index:%d', [Index]);

  if Index < FCount then
  begin
    if FOwnsObjects then
      TObject(GetItem(Index)).Free;

    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index)* SizeOf(Pointer));
  end;

  Dec(FCount);
end;

destructor THCObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function THCObjectList.GetItem(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('异常:%d', [Index]);
  Result := FList^[Index];
end;

function THCObjectList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure THCObjectList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Exit;
  if FCount = FCapacity then
    SetCapacity(FCapacity + 4);
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure THCObjectList.SetCapacity(const Value: Integer);
begin
  if (Value < FCount) or (Value > MaxListSize) then
    raise Exception.CreateFmt('非法数据:%d', [Value]);

  if FCapacity <> Value then
  begin
    ReallocMem(FList, Value * SizeOf(Pointer));
    FCapacity := Value;
  end;
end;

procedure THCObjectList.SetCount(const Value: Integer);
var
  i: Integer;
begin
  if (Value < 0) or (Value > MaxListSize) then
    raise Exception.CreateFmt('非法数据:%d', [Value]);

  if Value > FCapacity then SetCapacity(Value);

  if Value > FCount then
    FillChar(FList^[FCount], (Value - FCount) * SizeOf(Pointer), 0)
  else
  begin
    for i := FCount - 1 downto Value do
      Delete(i);
  end;

  FCount := Value;
end;

procedure THCObjectList.SetItem(Index: Integer; const Value: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('异常:%d', [Index]);

  if Value <> FList^[Index] then
    FList^[Index] := Value;
end;

end.
