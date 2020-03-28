{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                    表格行实现单元                     }
{                                                       }
{*******************************************************}

unit HCTableRow;

interface

uses
  HCCustomData, HCTableCell, HCTableCellData, HCStyle, HCXml;

const
  MinRowHeight = 20;  // < 10时受FGripSize拖块大小会不容易点进去
  MinColWidth = 20;  // 如果修改要和左缩进离边距的最小值匹配
  MaxListSize = Maxint div 16;

type
  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize] of Pointer;

  THCTableRow = class(TObject)
  private
    FList: PPointerList;
    FColCount,
    FCapacity,
    FHeight,  // 行高，不带上下边框 = 行中单元格最高的(单元格高包括单元格为分页而在某行额外偏移的高度)
    FFmtOffset  // 格式化时的偏移，主要是处理当前行整体下移到下一页时，简化上一页底部的鼠标点击、移动时的计算
      : Integer;
    FAutoHeight: Boolean;  // True根据内容自动匹配合适的高度 False用户拖动后的自定义高度
    procedure SetCapacity(const Value: Integer);
    function GetItems(Index: Integer): Pointer;
    procedure SetItems(Index: Integer; const Value: Pointer);
    procedure SetColCount(const Value: Integer);
  protected
    function GetCols(const Index: Integer): THCTableCell;

    property Items[Index: Integer]: Pointer read GetItems write SetItems;
  public
    constructor Create(const AStyle: THCStyle; const AColCount: Integer);
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    function Insert(Index: Integer; Item: Pointer): Boolean;
    procedure Clear;
    procedure Delete(Index: Integer);
    //
    //function CalcFormatHeight: Integer;
    procedure SetRowWidth(const AWidth: Integer);
    procedure SetHeight(const Value: Integer);  // 外部拖动改变行高

    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);

    //property Capacity: Integer read FCapacity write SetCapacity;
    property ColCount: Integer read FColCount write SetColCount;
    //property List: PCellDataList read FList;
    //
    property Cols[const Index: Integer]: THCTableCell read GetCols; default;

    /// <summary> 当前行中所有没有发生合并单元格的高度(含CellVPadding * 2因为会受有合并列的影响，所以>=数据高度) </summary>
    property Height: Integer read FHeight write SetHeight;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight;

    /// <summary>因跨页向下整体偏移的量</summary>
    property FmtOffset: Integer read FFmtOffset write FFmtOffset;
  end;

  TRowAddEvent = procedure(const ARow: THCTableRow) of object;

implementation

uses
  SysUtils, Math;

{ THCTableRow }

function THCTableRow.Add(Item: Pointer): Integer;
begin
  if FColCount = FCapacity then
    SetCapacity(FCapacity + 4);  // 每次扩充4个
  FList^[FColCount] := Item;
  Result := FColCount;
  Inc(FColCount);
end;

function THCTableRow.Insert(Index: Integer; Item: Pointer): Boolean;
begin
  if (Index < 0) or (Index > FColCount) then
    raise Exception.CreateFmt('[Insert]非法数据:%d', [Index]);
  if FColCount = FCapacity then
    SetCapacity(FCapacity + 4);  // 每次扩充4个
  if Index < FColCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FColCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FColCount);
  Result := True;
end;

procedure THCTableRow.ParseXml(const ANode: IHCXMLNode);
var
  i: Integer;
begin
  FAutoHeight := ANode.Attributes['autoheight'];
  FHeight := ANode.Attributes['height'];
  for i := 0 to ANode.ChildNodes.Count - 1 do
    Cols[i].ParseXml(ANode.ChildNodes[i]);
end;

procedure THCTableRow.SetRowWidth(const AWidth: Integer);
var
  i, vWidth: Integer;
begin
  vWidth := AWidth div FColCount;
  for i := 0 to FColCount - 2 do
  begin
    {if i = 0 then
      Cols[i].CellData.Items[0].Text := Cols[i].CellData.Items[0].Text + 'aaaaaa患者';
    if i = 1 then
    begin
      Cols[i].CellData.Items[0].StyleNo := 1;
      Cols[i].CellData.Items[0].Text := '12345678910';
    end;}
    Cols[i].Width := vWidth;
  end;
  Cols[FColCount - 1].Width := AWidth - (FColCount - 1) * vWidth;  // 结余的全部给最后一个单元格
end;

procedure THCTableRow.ToXml(const ANode: IHCXMLNode);
var
  i: Integer;
begin
  ANode.Attributes['autoheight'] := FAutoHeight;
  ANode.Attributes['height'] := FHeight;

  for i := 0 to FColCount - 1 do  // 各列数据
    Cols[i].ToXml(ANode.AddChild('cell'));
end;

{function THCTableRow.CalcFormatHeight: Integer;
var
  i, vH: Integer;
begin
  Result := MinRowHeight;
  for i := 0 to FColCount - 1 do
  begin
    if Cols[i] <> nil then
    begin
      vH := Cols[i].DrawItems[Cols[i].DrawItems.Count - 1].Rect.Bottom
        + Cols[i].DrawItems[Cols[i].DrawItems.Count - 1].TopOffset;

      if vH > Result then
        Result := vH;
    end;
  end;
end;}

procedure THCTableRow.Clear;
begin
  SetColCount(0);
  SetCapacity(0);
end;

constructor THCTableRow.Create(const AStyle: THCStyle; const AColCount: Integer);
var
  vCell: THCTableCell;
  i: Integer;
begin
  FColCount := 0;
  FCapacity := 0;
  for i := 0 to AColCount - 1 do
  begin
    vCell := THCTableCell.Create(AStyle);
    Add(vCell);
  end;

  FAutoHeight := True;
end;

procedure THCTableRow.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FColCount) then
    raise Exception.CreateFmt('[Delete]非法的 Index:%d', [Index]);
  THCTableCell(FList^[Index]).Free;
  if Index < FColCount then
    System.Move(FList^[Index + 1], FList^[Index], (FColCount - Index) * SizeOf(Pointer));
  Dec(FColCount);
end;

destructor THCTableRow.Destroy;
begin
  Clear;
  inherited;
end;

function THCTableRow.GetCols(const Index: Integer): THCTableCell;
begin
  Result := THCTableCell(Items[Index]);
end;

function THCTableRow.GetItems(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FColCount) then
    raise Exception.CreateFmt('异常:[THCTableRow.GetItems]参数Index值%d超出范围！', [Index]);
  Result := FList^[Index];
end;

procedure THCTableRow.SetCapacity(const Value: Integer);
begin
  if (Value < FColCount) or (Value > MaxListSize) then
    raise Exception.CreateFmt('[SetCapacity]非法数据:%d', [Value]);
  if FCapacity <> Value then
  begin
    // 重新分配指定大小内存块，参数P必须是nil或者指向一个由
    // GetMem, AllocMem, 或 ReallocMem分配的内存变量，其分配的内存是连续的，
    // 会把前面已有的数据移到新分配的内存中去
    ReallocMem(FList, Value * SizeOf(Pointer));
    FCapacity := Value;
  end;
end;

procedure THCTableRow.SetColCount(const Value: Integer);
var
  i: Integer;
begin
  if (Value < 0) or (Value > MaxListSize) then
    raise Exception.CreateFmt('[SetCount]非法数据:%d', [Value]);
  if Value > FCapacity then
    SetCapacity(Value);
  if Value > FColCount then
    FillChar(FList^[FColCount], (Value - FColCount) * SizeOf(Pointer), 0)
  else
    for i := FColCount - 1 downto Value do
      Delete(I);
  FColCount := Value;
end;

procedure THCTableRow.SetHeight(const Value: Integer);
var
  i, vMaxDataHeight: Integer;
begin
  if FHeight <> Value then
  begin
    vMaxDataHeight := 0;
    for i := 0 to FColCount - 1 do  // 找行中最高的单元格
    begin
      if (Cols[i].CellData <> nil) and (Cols[i].RowSpan = 0) then  // 不是被合并的单元格，不是行合并的行单元格
      begin
        if Cols[i].CellData.Height > vMaxDataHeight then
          vMaxDataHeight := Cols[i].CellData.Height;
      end;
    end;

    if vMaxDataHeight < Value then  // 设置的高度大于最高内容，以设置的为准(这里应该是Data高度+上下FCellVPadding和Value比更准确)
      FHeight := Value
    else
      FHeight := vMaxDataHeight;

    for i := 0 to FColCount - 1 do
      Cols[i].Height := FHeight;
  end;
end;

procedure THCTableRow.SetItems(Index: Integer; const Value: Pointer);
begin
  if (Index < 0) or (Index >= FColCount) then
    raise Exception.CreateFmt('[SetItems]异常:%d', [Index]);

  if Value <> FList^[Index] then
    FList^[Index] := Value;
end;

end.
