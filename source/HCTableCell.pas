{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
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
  Classes, Graphics, Controls, HCStyle, HCCustomData, HCTableCellData, HCItem,
  HCCommon, HCXml;

type
  TTableSite = (
    tsOutside,  // 表格外面
    tsCell,  // 单元格中
    tsBorderLeft,{只有第一列使用此元素}
    tsBorderTop,  {只有第一行使用此元素}
    tsBorderRight,  // 第X列右边
    tsBorderBottom  // 第X行下边
  );

  //PResizeInfo = ^TResizeInfo;
  TResizeInfo = record  // 缩放信息
    TableSite: TTableSite;
    DestX, DestY: Integer;
  end;

  TOutsideInfo = record  // 表格外面信息
    Row: Integer;  // 外面位置处对应的行
    Leftside: Boolean;  // True：左边 False：右边
  end;

  TSelectCellRang = class
  strict private
    FStartRow,  // 选中起始行
    FStartCol,  // 选中起始列
    FEndRow,    // 选中结束行
    FEndCol     // 选中结束列
      : Integer;
  public
    constructor Create;

    /// <summary> 初始化字段和变量 </summary>
    procedure Initialize;

    procedure InitializeEnd;
    procedure SetStart(const ARow, ACol: Integer);
    procedure SetEnd(const ARow, ACol: Integer);

    /// <summary> 在同一单元中编辑 </summary>
    function EditCell: Boolean;

    /// <summary> 选中在同一行 </summary>
    function SameRow: Boolean;

    /// <summary> 选中在同一列 </summary>
    function SameCol: Boolean;

    /// <summary> 选中1-n个单元格 </summary>
    function SelectExists: Boolean;
    property StartRow: Integer read FStartRow write FStartRow;
    property StartCol: Integer read FStartCol write FStartCol;
    property EndRow: Integer read FEndRow write FEndRow;
    property EndCol: Integer read FEndCol write FEndCol;
  end;

  /// <summary> 垂直对齐方式：上、居中、下) </summary>
  THCAlignVert = (cavTop, cavCenter, cavBottom);

  THCTableCell = class
  private
    FCellData: THCTableCellData;
    FWidth,    // 被合并后记录原始宽(否则当行第一列被合并后，第二列无法确认水平起始位置)
    FHeight,   // 被合并后记录原始高、记录拖动改变后高
    FRowSpan,  // 单元格跨几行，用于合并目标单元格记录合并了几行，合并源记录合并到单元格的行号，0没有行合并
    FColSpan   // 单元格跨几列，用于合并目标单元格记录合并了几列，合并源记录合并到单元格的列号，0没有列合并
      : Integer;
    FBackgroundColor: TColor;
    FAlignVert: THCAlignVert;
    FBorderSides: TBorderSides;
  protected
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetHeight(const Value: Integer);
  public
    constructor Create(const AStyle: THCStyle);
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      const ACellHPadding, ACellVPadding: Byte);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; const ACellHPadding, ACellVPadding: Byte);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      const ACellHPadding, ACellVPadding: Byte);

    function IsMergeSource: Boolean;
    function IsMergeDest: Boolean;

    /// <summary> 清除并返回为处理分页比净高增加的高度(为重新格式化时后面计算偏移用) </summary>
    function ClearFormatExtraHeight: Integer;

    procedure SaveToStream(const AStream: TStream); virtual;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word);
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);
    function GetCellDataTop(const ACellVPadding: Byte): Integer;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer;
      const ACellHPadding, ACellVPadding: Byte; var ACaretInfo: THCCaretInfo);

    /// <summary> 绘制数据 </summary>
    /// <param name="ADrawLeft">绘制目标区域Left</param>
    /// <param name="ADrawTop">绘制目标区域的Top</param>
    /// <param name="ADataDrawBottom">绘制目标区域的Bottom</param>
    /// <param name="ADataScreenTop">屏幕区域Top</param>
    /// <param name="ADataScreenBottom">屏幕区域Bottom</param>
    /// <param name="AVOffset">指定从哪个位置开始的数据绘制到目标区域的起始位置</param>
    /// <param name="ACanvas">画布</param>
    procedure PaintTo(const ADrawLeft, ADrawTop, ADrawRight, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
      const ACellHPadding, ACellVPadding: Byte;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

    property CellData: THCTableCellData read FCellData write FCellData;

    /// <summary> 单元格宽度(含CellHPadding)，数据的宽度在TableItem中处理 </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary> 单元格高度(含CellVPadding * 2 主要用于合并目标单元格，如果发生合并，则>=数据高度) </summary>
    property Height: Integer read FHeight write SetHeight;
    property RowSpan: Integer read FRowSpan write FRowSpan;
    property ColSpan: Integer read FColSpan write FColSpan;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    // 用于表格切换编辑的单元格
    property Active: Boolean read GetActive write SetActive;
    property AlignVert: THCAlignVert read FAlignVert write FAlignVert;
    property BorderSides: TBorderSides read FBorderSides write FBorderSides;
  end;

implementation

uses
  SysUtils;

{ THCTableCell }

constructor THCTableCell.Create(const AStyle: THCStyle);
begin
  FCellData := THCTableCellData.Create(AStyle);
  FAlignVert := cavTop;
  FBorderSides := [cbsLeft, cbsTop, cbsRight, cbsBottom];
  FBackgroundColor := HCTransparentColor;
  FRowSpan := 0;
  FColSpan := 0;
end;

destructor THCTableCell.Destroy;
begin
  FreeAndNil(FCellData);
  inherited;
end;

function THCTableCell.GetActive: Boolean;
begin
  if FCellData <> nil then
    Result := FCellData.Active
  else
    Result := False;
end;

procedure THCTableCell.GetCaretInfo(const AItemNo, AOffset: Integer;
  const ACellHPadding, ACellVPadding: Byte; var ACaretInfo: THCCaretInfo);
begin
  if Assigned(FCellData) then
  begin
    FCellData.GetCaretInfo(AItemNo, AOffset, ACaretInfo);
    if ACaretInfo.Visible then
    begin
      ACaretInfo.X := ACaretInfo.X + ACellHPadding;
      ACaretInfo.Y := ACaretInfo.Y + GetCellDataTop(ACellVPadding)
    end;
  end
  else
    ACaretInfo.Visible := False;
end;

function THCTableCell.GetCellDataTop(const ACellVPadding: Byte): Integer;
begin
  case FAlignVert of
    cavTop: Result := ACellVPadding;
    cavCenter: Result := ACellVPadding + (FHeight - ACellVPadding - FCellData.Height - ACellVPadding) div 2;
    cavBottom: Result := FHeight - ACellVPadding - FCellData.Height;
  end;
end;

function THCTableCell.ClearFormatExtraHeight: Integer;
begin
  if Assigned(FCellData) then
    Result := FCellData.ClearFormatExtraHeight
  else
    Result := 0;
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

  if AFileVersion > 11 then
  begin
    AStream.ReadBuffer(FAlignVert, SizeOf(FAlignVert));  // 垂直对齐方式
    if AFileVersion > 18 then
      HCLoadColorFromStream(AStream, FBackgroundColor)
    else
      AStream.ReadBuffer(FBackgroundColor, SizeOf(FBackgroundColor));  // 背景色
  end;

  if AFileVersion > 13 then
    AStream.ReadBuffer(FBorderSides, SizeOf(FBorderSides));

  AStream.ReadBuffer(vNullData, SizeOf(vNullData));
  if not vNullData then
  begin
    FCellData.LoadFromStream(AStream, AStyle, AFileVersion);
    FCellData.CellHeight := FHeight;
  end
  else
  if (FRowSpan < 0) or (FColSpan < 0) then  // 修正表格合并处理不准确造成的错误，容错打不开的情况
  begin
    FCellData.Free;
    FCellData := nil;
  end;
end;

procedure THCTableCell.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const ACellHPadding, ACellVPadding: Byte);
var
  vX, vY: Integer;
begin
  Self.Active := True;
  if Assigned(FCellData) then
  begin
    vX := X - ACellHPadding;
    vY := Y - GetCellDataTop(ACellVPadding);
    FCellData.MouseDown(Button, Shift, vX, vY);
  end;
end;

procedure THCTableCell.MouseMove(Shift: TShiftState; X, Y: Integer;
  const ACellHPadding, ACellVPadding: Byte);
var
  vX, vY: Integer;
begin
  if Assigned(FCellData) then
  begin
    vX := X - ACellHPadding;
    vY := Y - GetCellDataTop(ACellVPadding);
    FCellData.MouseMove(Shift, vX, vY);
  end;
end;

procedure THCTableCell.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const ACellHPadding, ACellVPadding: Byte);
var
  vX, vY: Integer;
begin
  if Assigned(FCellData) then
  begin
    vX := X - ACellHPadding;
    vY := Y - GetCellDataTop(ACellVPadding);
    FCellData.MouseUp(Button, Shift, vX, vY);
  end;
end;

procedure THCTableCell.PaintTo(const ADrawLeft, ADrawTop, ADrawRight, ADataDrawBottom,
  ADataScreenTop, ADataScreenBottom, AVOffset: Integer; const ACellHPadding,
  ACellVPadding: Byte; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vTop: Integer;
begin
  if Assigned(FCellData) then
  begin
    if (not APaintInfo.Print) and (FCellData.Script <> '') then
    begin
      ACanvas.Pen.Width := 1;
      ACanvas.Pen.Color := clGray;
      ACanvas.Pen.Style := psSolid;
      ACanvas.MoveTo(ADrawRight - 5, ADrawTop);
      ACanvas.LineTo(ADrawRight, ADrawTop);

      ACanvas.MoveTo(ADrawRight - 4, ADrawTop + 1);
      ACanvas.LineTo(ADrawRight, ADrawTop + 1);

      ACanvas.MoveTo(ADrawRight - 3, ADrawTop + 2);
      ACanvas.LineTo(ADrawRight, ADrawTop + 2);

      ACanvas.MoveTo(ADrawRight - 2, ADrawTop + 3);
      ACanvas.LineTo(ADrawRight, ADrawTop + 3);

      ACanvas.MoveTo(ADrawRight - 1, ADrawTop + 4);
      ACanvas.LineTo(ADrawRight, ADrawTop + 4);

      ACanvas.MoveTo(ADrawRight, ADrawTop + 5);
      ACanvas.LineTo(ADrawRight, ADrawTop + 5);
    end;

    vTop := ADrawTop + GetCellDataTop(ACellVPadding);
    FCellData.PaintData(ADrawLeft + ACellHPadding, vTop, ADrawRight - ACellHPadding,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, AVOffset, ACanvas, APaintInfo);
  end;
end;

procedure THCTableCell.ParseXml(const ANode: IHCXMLNode);
begin
  FWidth := ANode.Attributes['width'];
  FHeight := ANode.Attributes['height'];
  FRowSpan := ANode.Attributes['rowspan'];
  FColSpan := ANode.Attributes['colspan'];
  FAlignVert := THCAlignVert(ANode.Attributes['vert']);
  FBackgroundColor := HCRGBStringToColor(ANode.Attributes['bkcolor']);  // 背景色
  SetBorderSideByPro(ANode.Attributes['border'], FBorderSides);

  if (FRowSpan < 0) or (FColSpan < 0) then
  begin
    FCellData.Free;
    FCellData := nil;
  end
  else
  begin
    FCellData.Width := FWidth;  // 不准确的赋值，应该减去2个水平padding，加载时使用无大碍
    FCellData.ParseXml(ANode.ChildNodes.FindNode('items'));
  end;
end;

function THCTableCell.IsMergeDest: Boolean;
begin
  Result := (FRowSpan > 0) or (FColSpan > 0);
end;

function THCTableCell.IsMergeSource: Boolean;
begin
  Result := not Assigned(FCellData);
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

  AStream.WriteBuffer(FAlignVert, SizeOf(FAlignVert));  // 垂直对齐方式
  HCSaveColorToStream(AStream, FBackgroundColor);  // 背景色

  AStream.WriteBuffer(FBorderSides, SizeOf(FBorderSides));

  { 存数据 }
  vNullData := not Assigned(FCellData);
  AStream.WriteBuffer(vNullData, SizeOf(vNullData));
  if not vNullData then
    FCellData.SaveToStream(AStream);
end;

procedure THCTableCell.SetActive(const Value: Boolean);
begin
  if Assigned(FCellData) then
    FCellData.Active := Value;
end;

procedure THCTableCell.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Assigned(FCellData) then
      FCellData.CellHeight := Value;
  end;
end;

procedure THCTableCell.ToXml(const ANode: IHCXMLNode);
begin
  { 因为可能是合并后的单元格，所以单独存宽、高 }
  ANode.Attributes['width'] := FWidth;
  ANode.Attributes['height'] := FHeight;
  ANode.Attributes['rowspan'] := FRowSpan;
  ANode.Attributes['colspan'] := FColSpan;
  ANode.Attributes['vert'] := Ord(FAlignVert);
  ANode.Attributes['bkcolor'] := HCColorToRGBString(FBackgroundColor);
  ANode.Attributes['border'] := GetBorderSidePro(FBorderSides);

  if Assigned(FCellData) then  // 存数据
    FCellData.ToXml(ANode.AddChild('items'));
end;

{ TSelectCellRang }

constructor TSelectCellRang.Create;
begin
  Initialize;
end;

function TSelectCellRang.EditCell: Boolean;
begin
   Result := (FStartRow >= 0) and (FEndRow < 0);  // 这样比SameRow和SameCol更快捷？
end;

procedure TSelectCellRang.Initialize;
begin
  FStartRow := -1;
  FStartCol := -1;
  InitializeEnd;
end;

procedure TSelectCellRang.InitializeEnd;
begin
  FEndRow := -1;
  FEndCol := -1;
end;

function TSelectCellRang.SameCol: Boolean;
begin
  Result := (FStartCol >= 0) and (FStartCol = FEndCol);
end;

function TSelectCellRang.SameRow: Boolean;
begin
  Result := (FStartRow >= 0) and (FStartRow = FEndRow);
end;

function TSelectCellRang.SelectExists: Boolean;
begin
  Result := (FEndRow >= 0) or (FEndCol >= 0);  // 暂时没有用到此方法
end;

procedure TSelectCellRang.SetEnd(const ARow, ACol: Integer);
begin
  FEndRow := ARow;
  FEndCol := ACol;
end;

procedure TSelectCellRang.SetStart(const ARow, ACol: Integer);
begin
  FStartRow := ARow;
  FStartCol := ACol;
end;

end.
