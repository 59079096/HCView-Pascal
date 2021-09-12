{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{                    �����ʵ�ֵ�Ԫ                     }
{                                                       }
{*******************************************************}

unit HCTableRow;

interface

uses
  Classes, HCCustomData, HCTableCell, HCTableCellData, HCStyle, HCXml;

const
  MinRowHeight = 20;  // < 10ʱ��FGripSize�Ͽ��С�᲻���׵��ȥ
  MinColWidth = 20;  // ����޸�Ҫ����������߾����Сֵƥ��
  MaxListSize = Maxint div 16;

type
  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize] of Pointer;
  TOnGetTableIntEvent = function(): Integer of object;

  THCTableRow = class(TObject)
  private
    FList: PPointerList;
    FColCount,
    FCapacity,
    FHeight,  // �иߣ��������±߿� = ���е�Ԫ����ߵ�(��Ԫ��߰�����Ԫ��Ϊ��ҳ����ĳ�ж���ƫ�Ƶĸ߶�)
    FFmtOffset  // ��ʽ��ʱ��ƫ�ƣ���Ҫ�Ǵ���ǰ���������Ƶ���һҳʱ������һҳ�ײ�����������ƶ�ʱ�ļ���
      : Integer;

    FAutoHeight: Boolean;  // True���������Զ�ƥ����ʵĸ߶� False�û��϶�����Զ���߶�
    FOnGetVPaddingPix, FOnGetDefaultRowHeight: TOnGetTableIntEvent;
    procedure SetCapacity(const Value: Integer);
    function CalcMaxCellDataHeight: Integer;
    function GetItems(Index: Integer): Pointer;
    procedure SetItems(Index: Integer; const Value: Pointer);
    procedure SetColCount(const Value: Integer);
    procedure SetHeight(const Value: Integer);  // �ⲿ�϶��ı��и�
  protected
    function DoCreateCell(const AStyle: THCStyle): THCTableCell; virtual;
    function GetCols(const Index: Integer): THCTableCell;
    function GetVPadding: Byte;
    property Items[Index: Integer]: Pointer read GetItems write SetItems;
  public
    constructor Create(const AStyle: THCStyle; const AColCount: Integer);
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    function Insert(Index: Integer; Item: Pointer): Boolean;
    procedure Clear;
    procedure Delete(Index: Integer);
    //
    procedure FormatInit;
    procedure SetRowWidth(const AWidth: Integer);
    function CreateCell(const AStyle: THCStyle): THCTableCell;
    procedure SaveToStream(const AStream: TStream); virtual;
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word); virtual;
    procedure ToXml(const ANode: IHCXMLNode); virtual;
    procedure ParseXml(const ANode: IHCXMLNode); virtual;

    //property Capacity: Integer read FCapacity write SetCapacity;
    property ColCount: Integer read FColCount write SetColCount;
    //property List: PCellDataList read FList;
    //
    property Cols[const Index: Integer]: THCTableCell read GetCols; default;

    /// <summary> ��ǰ��������û�з����ϲ���Ԫ��ĸ߶�(��CellVPadding * 2��Ϊ�����кϲ��е�Ӱ�죬����>=���ݸ߶�) </summary>
    property Height: Integer read FHeight write SetHeight;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight;
    /// <summary>���ҳ��������ƫ�Ƶ���</summary>
    property FmtOffset: Integer read FFmtOffset write FFmtOffset;
    property OnGetDefaultRowHeight: TOnGetTableIntEvent read FOnGetDefaultRowHeight write FOnGetDefaultRowHeight;
    property OnGetVPaddingPix: TOnGetTableIntEvent read FOnGetVPaddingPix write FOnGetVPaddingPix;
  end;

  TRowAddEvent = procedure(const ARow: THCTableRow) of object;

implementation

uses
  SysUtils, Math;

{ THCTableRow }

function THCTableRow.Add(Item: Pointer): Integer;
begin
  if FColCount = FCapacity then
    SetCapacity(FCapacity + 4);  // ÿ������4��
  FList^[FColCount] := Item;
  Result := FColCount;
  Inc(FColCount);
end;

function THCTableRow.Insert(Index: Integer; Item: Pointer): Boolean;
begin
  if (Index < 0) or (Index > FColCount) then
    raise Exception.CreateFmt('[Insert]�Ƿ�����:%d', [Index]);
  if FColCount = FCapacity then
    SetCapacity(FCapacity + 4);  // ÿ������4��
  if Index < FColCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FColCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FColCount);
  Result := True;
end;

procedure THCTableRow.LoadFromStream(const AStream: TStream; const AFileVersion: Word);
begin
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
      Cols[i].CellData.Items[0].Text := Cols[i].CellData.Items[0].Text + 'aaaaaa����';
    if i = 1 then
    begin
      Cols[i].CellData.Items[0].StyleNo := 1;
      Cols[i].CellData.Items[0].Text := '12345678910';
    end;}
    Cols[i].Width := vWidth;
  end;
  Cols[FColCount - 1].Width := AWidth - (FColCount - 1) * vWidth;  // �����ȫ�������һ����Ԫ��
end;

procedure THCTableRow.ToXml(const ANode: IHCXMLNode);
var
  i: Integer;
begin
  ANode.Attributes['autoheight'] := FAutoHeight;
  ANode.Attributes['height'] := FHeight;

  for i := 0 to FColCount - 1 do  // ��������
    Cols[i].ToXml(ANode.AddChild('cell'));
end;

function THCTableRow.CalcMaxCellDataHeight: Integer;
var
  i: Integer;
begin
  if Assigned(FOnGetDefaultRowHeight) then
    Result := FOnGetDefaultRowHeight
  else
    Result := MinRowHeight;

  for i := 0 to FColCount - 1 do  // ��������ߵĵ�Ԫ��
  begin
    if (Cols[i].CellData <> nil) and (Cols[i].RowSpan = 0) then  // ���Ǳ��ϲ��ĵ�Ԫ�񣬲����кϲ����е�Ԫ��
    begin
      if Cols[i].CellData.Height > Result then
        Result := Cols[i].CellData.Height;
    end;
  end;
end;

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
    vCell := DoCreateCell(AStyle);
    Add(vCell);
  end;

  FAutoHeight := True;
end;

function THCTableRow.CreateCell(const AStyle: THCStyle): THCTableCell;
begin
  Result := DoCreateCell(AStyle);
end;

procedure THCTableRow.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FColCount) then
    raise Exception.CreateFmt('[Delete]�Ƿ��� Index:%d', [Index]);

  FreeAndNil(THCTableCell(FList^[Index]));
  if Index < FColCount then
    System.Move(FList^[Index + 1], FList^[Index], (FColCount - Index) * SizeOf(Pointer));
  Dec(FColCount);
end;

destructor THCTableRow.Destroy;
begin
  Clear;
  inherited;
end;

function THCTableRow.DoCreateCell(const AStyle: THCStyle): THCTableCell;
begin
  Result := THCTableCell.Create(AStyle);
end;

procedure THCTableRow.FormatInit;
var
  i, vHeight: Integer;
begin
  vHeight := CalcMaxCellDataHeight + GetVPadding + GetVPadding;
  if FAutoHeight then
    FHeight := vHeight
  else
  if FHeight < vHeight then
  begin
    FHeight := vHeight;
    FAutoHeight := True;
  end;

  for i := 0 to FColCount - 1 do
    Cols[i].Height := FHeight;
end;

function THCTableRow.GetCols(const Index: Integer): THCTableCell;
begin
  Result := THCTableCell(Items[Index]);
end;

function THCTableRow.GetVPadding: Byte;
begin
  if Assigned(FOnGetVPaddingPix) then
    Result := FOnGetVPaddingPix
  else
    Result := 0;
end;


function THCTableRow.GetItems(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FColCount) then
    raise Exception.CreateFmt('�쳣:[THCTableRow.GetItems]����Indexֵ%d������Χ��', [Index]);
  Result := FList^[Index];
end;

procedure THCTableRow.SaveToStream(const AStream: TStream);
begin
end;

procedure THCTableRow.SetCapacity(const Value: Integer);
begin
  if (Value < FColCount) or (Value > MaxListSize) then
    raise Exception.CreateFmt('[SetCapacity]�Ƿ�����:%d', [Value]);
  if FCapacity <> Value then
  begin
    // ���·���ָ����С�ڴ�飬����P������nil����ָ��һ����
    // GetMem, AllocMem, �� ReallocMem������ڴ�������������ڴ��������ģ�
    // ���ǰ�����е������Ƶ��·�����ڴ���ȥ
    ReallocMem(FList, Value * SizeOf(Pointer));
    FCapacity := Value;
  end;
end;

procedure THCTableRow.SetColCount(const Value: Integer);
var
  i: Integer;
begin
  if (Value < 0) or (Value > MaxListSize) then
    raise Exception.CreateFmt('[SetCount]�Ƿ�����:%d', [Value]);
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
  i, vMaxHeight: Integer;
begin
  if FHeight <> Value then
  begin
    vMaxHeight := CalcMaxCellDataHeight + GetVPadding + GetVPadding;

    if vMaxHeight < Value then
      FHeight := Value
    else
      FHeight := vMaxHeight;

    for i := 0 to FColCount - 1 do
      Cols[i].Height := FHeight;
  end;
end;

procedure THCTableRow.SetItems(Index: Integer; const Value: Pointer);
begin
  if (Index < 0) or (Index >= FColCount) then
    raise Exception.CreateFmt('[SetItems]�쳣:%d', [Index]);

  if Value <> FList^[Index] then
    FList^[Index] := Value;
end;

end.
