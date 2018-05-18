{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                  表格单元格实现单元                   }
{                                                       }
{*******************************************************}

unit HCTableCell;

interface

uses
  Classes, Graphics, HCStyle, HCCustomData, HCTableCellData, HCCommon;

type
  THCTableCell = class
  private
    FCellData: THCTableCellData;
    FWidth,    // 被合并后记录原始宽(否则当行第一列被合并后，第二列无法确认水平起始位置)
    FHeight,   // 被合并后记录原始高、记录拖动改变后高
    FRowSpan,  // 单元格跨几行，用于合并目标单元格记录合并了几行，合并源记录合并到单元格的行号，0没有行合并
    FColSpan   // 单元格跨几列，用于合并目标单元格记录合并了几列，合并源记录合并到单元格的列号，0没有列合并
      : Integer;
    FBackgroundColor: TColor;
  protected
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetHeight(const Value: Integer);
  public
    constructor Create(const AStyle: THCStyle);
    destructor Destroy; override;
    function MergeSource: Boolean;
    function MergeDest: Boolean;

    /// <summary> 清除并返回为处理分页比净高增加的高度(为重新格式化时后面计算偏移用) </summary>
    function ClearFormatExtraHeight: Integer;

    procedure SaveToStream(const AStream: TStream); virtual;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word);

    property CellData: THCTableCellData read FCellData write FCellData;

    /// <summary>
    /// 单元格宽度，为不使用全局的GCellPadding，数据的宽度在TableItem中处理
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary> 单元格高度(含CellVPadding * 2 主要用于合并目标单元格，如果发生合并，则>=数据高度) </summary>
    property Height: Integer read FHeight write SetHeight;
    property RowSpan: Integer read FRowSpan write FRowSpan;
    property ColSpan: Integer read FColSpan write FColSpan;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    // 用于表格切换编辑的单元格
    property Active: Boolean read GetActive write SetActive;
  end;

implementation

uses
  SysUtils;

{ THCTableCell }

constructor THCTableCell.Create(const AStyle: THCStyle);
begin
  FCellData := THCTableCellData.Create(AStyle);
  //FCellData.ParentData := AParentData;
  FBackgroundColor := AStyle.BackgroudColor;
  FRowSpan := 0;
  FColSpan := 0;
end;

destructor THCTableCell.Destroy;
begin
  FCellData.Free;
  inherited;
end;

function THCTableCell.GetActive: Boolean;
begin
  if FCellData <> nil then
    Result := FCellData.Active
  else
    Result := False;
end;

function THCTableCell.ClearFormatExtraHeight: Integer;
begin
  Result := 0;
  if FCellData <> nil then
    Result := FCellData.ClearFormatExtraHeight;
end;

procedure THCTableCell.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vNullData: Boolean;
begin
  AStream.ReadBuffer(FWidth, SizeOf(FWidth));
  AStream.ReadBuffer(FHeight, SizeOf(FHeight));
  AStream.ReadBuffer(FRowSpan, SizeOf(FRowSpan));
  AStream.ReadBuffer(FColSpan, SizeOf(FColSpan));

  AStream.ReadBuffer(vNullData, SizeOf(vNullData));
  if not vNullData then
  begin
    FCellData.LoadFromStream(AStream, AStyle, AFileVersion);
    FCellData.CellHeight := FHeight;
  end
  else
  begin
    FCellData.Free;
    FCellData := nil;
  end;
end;

function THCTableCell.MergeDest: Boolean;
begin
  Result := (FRowSpan > 0) or (FColSpan > 0);
end;

function THCTableCell.MergeSource: Boolean;
begin
  Result := FCellData = nil;
end;

procedure THCTableCell.SaveToStream(const AStream: TStream);
var
  vNullData: Boolean;
begin
  { 因为可能是合并后的单元格，所以单独存宽、高 }
  AStream.WriteBuffer(FWidth, SizeOf(FWidth));
  AStream.WriteBuffer(FHeight, SizeOf(FHeight));
  AStream.WriteBuffer(FRowSpan, SizeOf(FRowSpan));
  AStream.WriteBuffer(FColSpan, SizeOf(FColSpan));

  { 存数据 }
  vNullData := FCellData = nil;
  AStream.WriteBuffer(vNullData, SizeOf(vNullData));
  if not vNullData then
    FCellData.SaveToStream(AStream);
end;

procedure THCTableCell.SetActive(const Value: Boolean);
begin
  if FCellData <> nil then
    FCellData.Active := Value;
end;

procedure THCTableCell.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if FCellData <> nil then
      FCellData.CellHeight := Value;
  end;
end;

end.
