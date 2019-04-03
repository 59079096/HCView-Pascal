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
  Classes, Graphics, HCStyle, HCCustomData, HCTableCellData, HCItem, HCCommon, HCXml;

type
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
    function IsMergeSource: Boolean;
    function IsMergeDest: Boolean;

    /// <summary> 清除并返回为处理分页比净高增加的高度(为重新格式化时后面计算偏移用) </summary>
    function ClearFormatExtraHeight: Integer;

    procedure SaveToStream(const AStream: TStream); virtual;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word);
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);

    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: THCCaretInfo);

    /// <summary> 绘制数据 </summary>
    /// <param name="ADataDrawLeft">绘制目标区域Left</param>
    /// <param name="ADataDrawTop">绘制目标区域的Top</param>
    /// <param name="ADataDrawBottom">绘制目标区域的Bottom</param>
    /// <param name="ADataScreenTop">屏幕区域Top</param>
    /// <param name="ADataScreenBottom">屏幕区域Bottom</param>
    /// <param name="AVOffset">指定从哪个位置开始的数据绘制到目标区域的起始位置</param>
    /// <param name="ACanvas">画布</param>
    procedure PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
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

procedure THCTableCell.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: THCCaretInfo);
begin
  if FCellData <> nil then
  begin
    FCellData.GetCaretInfo(AItemNo, AOffset, ACaretInfo);
    if ACaretInfo.Visible then
    begin
      case FAlignVert of
        cavBottom: ACaretInfo.Y := ACaretInfo.Y + FHeight - FCellData.Height;
        cavCenter: ACaretInfo.Y := ACaretInfo.Y + (FHeight - FCellData.Height) div 2;
      end;
    end;
  end
  else
    ACaretInfo.Visible := False;
end;

function THCTableCell.ClearFormatExtraHeight: Integer;
begin
  if FCellData <> nil then
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
  begin
    FCellData.Free;
    FCellData := nil;
  end;
end;

procedure THCTableCell.PaintData(const ADataDrawLeft, ADataDrawTop,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vTop: Integer;
begin
  if FCellData <> nil then
  begin
    case FAlignVert of
      cavTop: vTop := ADataDrawTop;
      cavBottom: vTop := ADataDrawTop + FHeight - FCellData.Height;
      cavCenter: vTop := ADataDrawTop + (FHeight - FCellData.Height) div 2;
    end;

    FCellData.PaintData(ADataDrawLeft, vTop, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom, AVOffset, ACanvas, APaintInfo);
  end;
end;

procedure THCTableCell.ParseXml(const ANode: IHCXMLNode);
begin
  FWidth := ANode.Attributes['width'];
  FHeight := ANode.Attributes['height'];
  FRowSpan := ANode.Attributes['rowspan'];
  FColSpan := ANode.Attributes['colspan'];
  FAlignVert := THCAlignVert(ANode.Attributes['vert']);
  FBackgroundColor := GetXmlRGBColor(ANode.Attributes['bkcolor']);  // 背景色
  SetBorderSideByPro(ANode.Attributes['border'], FBorderSides);

  if (FRowSpan < 0) or (FColSpan < 0) then
  begin
    FCellData.Free;
    FCellData := nil;
  end
  else
    FCellData.ParseXml(ANode.ChildNodes.FindNode('items'));
end;

function THCTableCell.IsMergeDest: Boolean;
begin
  Result := (FRowSpan > 0) or (FColSpan > 0);
end;

function THCTableCell.IsMergeSource: Boolean;
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

procedure THCTableCell.ToXml(const ANode: IHCXMLNode);
begin
  { 因为可能是合并后的单元格，所以单独存宽、高 }
  ANode.Attributes['width'] := FWidth;
  ANode.Attributes['height'] := FHeight;
  ANode.Attributes['rowspan'] := FRowSpan;
  ANode.Attributes['colspan'] := FColSpan;
  ANode.Attributes['vert'] := Ord(FAlignVert);
  ANode.Attributes['bkcolor'] := GetColorXmlRGB(FBackgroundColor);
  ANode.Attributes['border'] := GetBorderSidePro(FBorderSides);

  if Assigned(FCellData) then  // 存数据
    FCellData.ToXml(ANode.AddChild('items'));
end;

end.
