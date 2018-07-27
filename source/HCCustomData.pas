{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                 文档对象基本管理单元                  }
{                                                       }
{*******************************************************}

unit HCCustomData;

interface

uses
  Windows, Classes, Types, Controls, Graphics, HCItem, HCDrawItem,
  HCStyle, HCParaStyle, HCTextStyle, HCStyleMatch, HCCommon, HCUndo;

type
  TSelectInfo = class
  strict private
    FStartItemNo,  // 不能使用DrawItem记录，因为内容变动时Item的指定Offset对应的DrawItem，可能和变动前不一样
    FStartItemOffset,  // 选中起始在第几个字符后面，0表示在Item最前面
    FEndItemNo,
    FEndItemOffset  // 选中结束在第几个字符后面
      : Integer;
  public
    constructor Create;
    procedure Initialize;

    /// <summary> 选中起始Item序号 </summary>
    property StartItemNo: Integer read FStartItemNo write FStartItemNo;

    property StartItemOffset: Integer read FStartItemOffset write FStartItemOffset;

    /// <summary> 选中结束Item序号 </summary>
    property EndItemNo: Integer read FEndItemNo write FEndItemNo;

    property EndItemOffset: Integer read FEndItemOffset write FEndItemOffset;
  end;

  THCCustomData = class(TObject)  // 为支持域，所以不能有太多属性，以免和CustomRichData冲突
  private
    FStyle: THCStyle;
    FItems: THCItems;
    FDrawItems: THCDrawItems;
    FSelectInfo: TSelectInfo;
    FDrawOptions: TDrawOptions;
    FCaretDrawItemNo: Integer;  // 当前Item光标处的DrawItem限定其只在相关的光标处理中使用(解决同一Item分行后Offset为行尾时不能区分是上行尾还是下行始)
    FOnGetUndoList: TGetUndoListEvent;
    procedure DrawItemPaintBefor(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DrawItemPaintAfter(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
  protected
    /// <summary> 处理选中范围内Item的全选中、部分选中状态 </summary>
    procedure MatchItemSelectState;

    /// <summary> 式化时，记录起始DrawItem和段最后的DrawItem </summary>
    /// <param name="AStartItemNo"></param>
    procedure FormatItemPrepare(const AStartItemNo: Integer; const AEndItemNo: Integer = -1);

    /// <summary>
    /// 转换指定Item指定Offs格式化为DItem
    /// </summary>
    /// <param name="AItemNo">指定的Item</param>
    /// <param name="AOffset">指定的格式化起始位置</param>
    /// <param name="AContentWidth">当前Data格式化宽度</param>
    /// <param name="APageContenBottom">当前页格式化底部位置</param>
    /// <param name="APos">起始位置</param>
    /// <param name="ALastDNo">起始DItemNo前一个值</param>
    /// <param name="vPageBoundary">数据页底部边界</param>
    procedure _FormatItemToDrawItems(const AItemNo, AOffset, AContentWidth: Integer;
      var APos: TPoint; var ALastDNo: Integer);

    /// <summary> 获取DItem中指定偏移处的内容绘制宽度 </summary>
    /// <param name="ADrawItemNo"></param>
    /// <param name="ADrawOffs">相对与DItem的CharOffs的Offs</param>
    /// <returns></returns>
    function GetDrawItemOffsetWidth(const ADrawItemNo, ADrawOffs: Integer): Integer;

    /// <summary> 根据指定Item获取其所在段的起始和结束ItemNo </summary>
    /// <param name="AFirstItemNo1">指定</param>
    /// <param name="AFirstItemNo">起始</param>
    /// <param name="ALastItemNo">结束</param>
    procedure GetParaItemRang(const AItemNo: Integer;
      var AFirstItemNo, ALastItemNo: Integer);
    function GetParaFirstItemNo(const AItemNo: Integer): Integer;
    function GetParaLastItemNo(const AItemNo: Integer): Integer;

    /// <summary> 取行第一个DrawItem对应的ItemNo(用于格式化时计算一个较小的ItemNo范围) </summary>
    function GetLineFirstItemNo(const AItemNo, AOffset: Integer): Integer;

    /// <summary> 取行最后一个DrawItem对应的ItemNo(用于格式化时计算一个较小的ItemNo范围) </summary>
    function GetLineLastItemNo(const AItemNo, AOffset: Integer): Integer;

    /// <summary> 根据指定Item获取其所在行的起始和结束DrawItemNo </summary>
    /// <param name="AFirstItemNo1">指定</param>
    /// <param name="AFirstItemNo">起始</param>
    /// <param name="ALastItemNo">结束</param>
    procedure GetLineDrawItemRang(var AFirstDItemNo, ALastDItemNo: Integer); virtual;

    /// <summary> 获取指定DItem对应的Text </summary>
    /// <param name="ADrawItemNo"></param>
    /// <returns></returns>
    function GetDrawItemText(const ADrawItemNo: Integer): string;

    procedure SetCaretDrawItemNo(const Value: Integer);

    function GetUndoList: THCUndoList;

    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
  public
    constructor Create(const AStyle: THCStyle); virtual;
    destructor Destroy; override;
    //
    procedure Clear; virtual;

    procedure InitializeField; virtual;

    /// <summary>
    /// 当前Data是不是无内容(仅有一个Item且内容为空)
    /// </summary>
    /// <returns></returns>
    function IsEmptyData: Boolean;

    /// <summary> 嵌套时获取根级Data </summary>
    function GetRootData: THCCustomData; virtual;
    function CreateDefaultTextItem: THCCustomItem; virtual;
    function CreateDefaultDomainItem: THCCustomItem; virtual;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: TCaretInfo); virtual;

    /// <summary>
    /// 根据给定的位置获取在此范围内的起始和结束DItem
    /// </summary>
    /// <param name="ATop"></param>
    /// <param name="ABottom"></param>
    /// <param name="AFristDItemNo"></param>
    /// <param name="ALastDItemNo"></param>
    procedure GetDataDrawItemRang(const ATop, ABottom: Integer;
      var AFirstDItemNo, ALastDItemNo: Integer);

    /// <summary>
    /// 返回指定坐标下的Item和Offset
    /// </summary>
    /// <param name="X">水平坐标值X</param>
    /// <param name="Y">垂直坐标值Y</param>
    /// <param name="AItemNo">坐标处的Item</param>
    /// <param name="AOffset">坐标在Item中的位置</param>
    /// <param name="ARestrain">True并不是在AItemNo范围内(在行最右侧或最后一行底部，通过约束坐标找到的)</param>
    procedure GetItemAt(const X, Y: Integer; var AItemNo, AOffset, ADrawItemNo: Integer;
      var ARestrain: Boolean); virtual;

    /// <summary>
    /// 获取指定Item格式化时起始Item
    /// </summary>
    /// <param name="AItemNo"></param>
    /// <returns></returns>
    //function GetFormatStartItemNo(const AItemNo: Integer): Integer;

    {procedure GetParaDrawItemRang(const AItemNo: Integer;
      var AFirstDItemNo, ALastDItemNo: Integer);}

    { Item和DItem互查 }
    /// <summary>
    /// 获取Item对应的最后一个DItem
    /// </summary>
    /// <param name="AItemNo"></param>
    /// <returns></returns>
    function GetItemLastDrawItemNo(const AItemNo: Integer): Integer;

    /// <summary>
    /// Item指定偏移位置是否被选中(仅用于文本Item和粗略Rect)
    /// </summary>
    /// <param name="AItemNo"></param>
    /// <param name="AOffset"></param>
    /// <returns></returns>
    function OffsetInSelect(const AItemNo, AOffset: Integer): Boolean;

    /// <summary> 坐标是否在AItem的选中区域中 </summary>
    /// <param name="X"></param>
    /// <param name="Y"></param>
    /// <param name="AItemNo">X、Y处的Item</param>
    /// <param name="AOffset">X、Y处的Item偏移(供在RectItem上时计算)</param>
    /// <param name="ARestrain">AItemNo, AOffset是X、Y位置约束后的(此参数为方便单元格Data处理)</param>
    function CoordInSelect(const X, Y, AItemNo, AOffset: Integer;
      const ARestrain: Boolean): Boolean; virtual;
    /// <summary>
    /// 获取Data中的坐标X、Y处的Item和Offset，并返回X、Y相对DrawItem的坐标
    /// </summary>
    /// <param name="X"></param>
    /// <param name="Y"></param>
    /// <param name="AItemNo"></param>
    /// <param name="AOffset"></param>
    /// <param name="AX"></param>
    /// <param name="AY"></param>
    procedure CoordToItemOffset(const X, Y, AItemNo, AOffset: Integer; var AX, AY: Integer);

    /// <summary>
    /// 返回Item中指定Offset处的DrawItem序号
    /// </summary>
    /// <param name="AItemNo">指定Item</param>
    /// <param name="AOffset">Item中指定Offset</param>
    /// <returns>Offset处的DrawItem序号</returns>
    function GetDrawItemNoByOffset(const AItemNo, AOffset: Integer): Integer;
    function IsLineLastDrawItem(const ADrawItemNo: Integer): Boolean;
    function IsParaLastDrawItem(const ADrawItemNo: Integer): Boolean;
    function IsParaLastItem(const AItemNo: Integer): Boolean;

    function GetCurDrawItemNo: Integer;
    function GetCurDrawItem: THCCustomDrawItem;
    function GetCurItemNo: Integer;
    function GetCurItem: THCCustomItem;

    /// <summary> 返回Item的文本样式 </summary>
    function GetItemStyle(const AItemNo: Integer): Integer;

    /// <summary> 返回DDrawItem对应的Item的文本样式 </summary>
    function GetDrawItemStyle(const ADrawItemNo: Integer): Integer;

    /// <summary> 返回Item对应的段落样式 </summary>
    function GetItemParaStyle(const AItemNo: Integer): Integer;

    /// <summary> 返回DDrawItem对应的Item的段落样式 </summary>
    function GetDrawItemParaStyle(const ADrawItemNo: Integer): Integer;

    /// <summary> 得到指定横坐标X处，是DItem内容的第几个字符 </summary>
    /// <param name="ADrawItemNo">指定的DItem</param>
    /// <param name="X">在Data中的横坐标</param>
    /// <returns>第几个字符</returns>
    function GetDrawItemOffset(const ADrawItemNo, X: Integer): Integer;

    { 获取选中相关信息 }
    /// <summary> 当前选中起始DItemNo </summary>
    /// <returns></returns>
    function GetSelectStartDrawItemNo: Integer;

    /// <summary> 当前选中结束DItemNo </summary>
    /// <returns></returns>
    function GetSelectEndDrawItemNo: Integer;

    /// <summary> 获取选中内容是否在同一个DItem中 </summary>
    /// <returns></returns>
    function SelectInSameDItem: Boolean;

    /// <summary> 取消选中 </summary>
    /// <returns>取消时当前是否有选中，True：有选中；False：无选中</returns>
    function DisSelect: Boolean; virtual;

    /// <summary> 当前选中内容允许拖动 </summary>
    /// <returns></returns>
    function SelectedCanDrag: Boolean;

    /// <summary> 当前选中内容只有RectItem且正处于缩放状态 </summary>
    /// <returns></returns>
    function SelectedResizing: Boolean;

    /// <summary> 全选 </summary>
    procedure SelectAll; virtual;

    /// <summary> 当前内容是否全选中了 </summary>
    function SelectedAll: Boolean; virtual;

    /// <summary> 为段应用对齐方式 </summary>
    /// <param name="AAlign">对方方式</param>
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz); virtual;
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert); virtual;
    procedure ApplyParaBackColor(const AColor: TColor); virtual;
    procedure ApplyParaLineSpace(const ASpace: Integer); virtual;

    // 选中内容应用样式
    function ApplySelectTextStyle(const AMatchStyle: TStyleMatch): Integer; virtual;
    function ApplySelectParaStyle(const AMatchStyle: TParaMatch): Integer; virtual;

    /// <summary> 删除选中 </summary>
    function DeleteSelected: Boolean; virtual;

    /// <summary> 为选中文本使用指定的文本样式 </summary>
    /// <param name="AFontStyle">文本样式</param>
    procedure ApplyTextStyle(const AFontStyle: TFontStyleEx); virtual;
    procedure ApplyTextFontName(const AFontName: TFontName); virtual;
    procedure ApplyTextFontSize(const AFontSize: Integer); virtual;
    procedure ApplyTextColor(const AColor: TColor); virtual;
    procedure ApplyTextBackColor(const AColor: TColor); virtual;

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
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    /// <summary> 添加Data到当前 </summary>
    /// <param name="ASrcData">源Data</param>
    procedure AddData(const ASrcData: THCCustomData);

    /// <summary> 是否有选中 </summary>
    function SelectExists(const AIfRectItem: Boolean = True): Boolean;
    procedure MarkStyleUsed(const AMark: Boolean);

    procedure SaveToStream(const AStream: TStream); overload; virtual;
    procedure SaveToStream(const AStream: TStream; const AStartItemNo, AStartOffset,
      AEndItemNo, AEndOffset: Integer); overload; virtual;

    function SaveToText: string; overload;
    function SaveToText(const AStartItemNo, AStartOffset,
      AEndItemNo, AEndOffset: Integer): string; overload;

    /// <summary> 保存选中内容到流 </summary>
    procedure SaveSelectToStream(const AStream: TStream); virtual;
    function SaveSelectToText: string;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; virtual;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); virtual;
    //
    property Style: THCStyle read FStyle;
    property Items: THCItems read FItems;
    property DrawItems: THCDrawItems read FDrawItems;
    property SelectInfo: TSelectInfo read FSelectInfo;
    property DrawOptions: TDrawOptions read FDrawOptions write FDrawOptions;
    property CaretDrawItemNo: Integer read FCaretDrawItemNo write SetCaretDrawItemNo;
    property OnGetUndoList: TGetUndoListEvent read FOnGetUndoList write FOnGetUndoList;
  end;

type
  TTraverseItemEvent = procedure(const AData: THCCustomData;
    const AItemNo, ATag: Integer; var AStop: Boolean) of object;

  TItemTraverse = class(TObject)
  public
    Tag: Integer;
    Stop: Boolean;
    Process: TTraverseItemEvent;
  end;

implementation

uses
  SysUtils, Math, HCList, HCTextItem, HCRectItem;

{ THCCustomData }

/// <summary> 返回字符串AText的分散分隔数量和各分隔的起始位置 </summary>
/// <param name="AText">要计算的字符串</param>
/// <param name="ACharIndexs">记录各分隔的起始位置</param>
/// <returns>分散分隔数量</returns>
function GetJustifyCount(const AText: string; const ACharIndexs: THCIntegerList): Integer;

  function IsCharSameType(const A, B: Char): Boolean;
  begin
    //if A = B then
    //  Result := True
    //else
      Result := False;
  end;

var
  i: Integer;
  vProvChar: Char;
begin
  Result := 0;
  if AText = '' then
    raise Exception.Create('异常：不能对空字符串计算分散！');

  if ACharIndexs <> nil then
    ACharIndexs.Clear;
  vProvChar := #0;
  for i := 1 to Length(AText) do
  begin
    if not IsCharSameType(vProvChar, AText[i]) then
    begin
      Inc(Result);
      if ACharIndexs <> nil then
        ACharIndexs.Add(i);
    end;
    vProvChar := AText[i];
  end;
  if ACharIndexs <> nil then
    ACharIndexs.Add(Length(AText) + 1);
end;

procedure THCCustomData.AddData(const ASrcData: THCCustomData);
var
  i: Integer;
begin
  for i := 0 to ASrcData.FItems.Count - 1 do
  begin
    FItems[FItems.Count - 1].Text := FItems[FItems.Count - 1].Text
      + ASrcData.FItems[i].Text;
  end;
end;

procedure THCCustomData.ApplyTextBackColor(const AColor: TColor);
var
  vMatchStyle: TBackColorStyleMatch;
begin
  vMatchStyle := TBackColorStyleMatch.Create;
  try
    vMatchStyle.Color := AColor;
    ApplySelectTextStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyTextColor(const AColor: TColor);
var
  vMatchStyle: TColorStyleMatch;
begin
  vMatchStyle := TColorStyleMatch.Create;
  try
    vMatchStyle.Color := AColor;
    ApplySelectTextStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyTextFontName(const AFontName: TFontName);
var
  vMatchStyle: TFontNameStyleMatch;
begin
  vMatchStyle := TFontNameStyleMatch.Create;
  try
    vMatchStyle.FontName := AFontName;
    ApplySelectTextStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyTextFontSize(const AFontSize: Integer);
var
  vMatchStyle: TFontSizeStyleMatch;
begin
  vMatchStyle := TFontSizeStyleMatch.Create;
  try
    vMatchStyle.FontSize := AFontSize;
    ApplySelectTextStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyTextStyle(const AFontStyle: TFontStyleEx);
var
  vMatchStyle: TTextStyleMatch;
begin
  vMatchStyle := TTextStyleMatch.Create;
  try
    vMatchStyle.FontStyle := AFontStyle;
    ApplySelectTextStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
var
  vMatchStyle: TParaAlignHorzMatch;
begin
  vMatchStyle := TParaAlignHorzMatch.Create;
  try
    vMatchStyle.Align := AAlign;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyParaAlignVert(const AAlign: TParaAlignVert);
var
  vMatchStyle: TParaAlignVertMatch;
begin
  vMatchStyle := TParaAlignVertMatch.Create;
  try
    vMatchStyle.Align := AAlign;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyParaBackColor(const AColor: TColor);
var
  vMatchStyle: TParaBackColorMatch;
begin
  vMatchStyle := TParaBackColorMatch.Create;
  try
    vMatchStyle.BackColor := AColor;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyParaLineSpace(const ASpace: Integer);
var
  vMatchStyle: TParaLineSpaceMatch;
begin
  vMatchStyle := TParaLineSpaceMatch.Create;
  try
    vMatchStyle.Space := ASpace;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

function THCCustomData.ApplySelectParaStyle(const AMatchStyle: TParaMatch): Integer;
begin
end;

function THCCustomData.ApplySelectTextStyle(const AMatchStyle: TStyleMatch): Integer;
begin
end;

procedure THCCustomData.Clear;
begin
  //DisSelect;  用不着DisSelect吧
  FSelectInfo.Initialize;
  FCaretDrawItemNo := -1;
  FDrawItems.Clear;
  FItems.Clear;
end;

function THCCustomData.CoordInSelect(const X, Y, AItemNo,
  AOffset: Integer; const ARestrain: Boolean): Boolean;
var
  vX, vY, vDrawItemNo: Integer;
  vDrawRect: TRect;
begin
  Result := False;
  if (AItemNo < 0) or (AOffset < 0) then Exit;
  if ARestrain then Exit;

  // 判断坐标是否在AItemNo对应的AOffset上
  vDrawItemNo := GetDrawItemNoByOffset(AItemNo, AOffset);
  vDrawRect := DrawItems[vDrawItemNo].Rect;
  Result := PtInRect(vDrawRect, Point(X, Y));
  if Result then  // 在对应的DrawItem上
  begin
    if FItems[AItemNo].StyleNo < THCStyle.RsNull then
    begin
      vX := X - vDrawRect.Left;
      vY := Y - vDrawRect.Top - FStyle.ParaStyles[Items[AItemNo].ParaNo].LineSpaceHalf;

      Result := (FItems[AItemNo] as THCCustomRectItem).CoordInSelect(vX, vY)
    end
    else
      Result := OffsetInSelect(AItemNo, AOffset);  // 对应的AOffset在选中内容中
  end;
end;

procedure THCCustomData.CoordToItemOffset(const X, Y, AItemNo,
  AOffset: Integer; var AX, AY: Integer);
var
  vDrawItemNo: Integer;
  vDrawRect: TRect;
begin
  AX := X;
  AY := Y;
  if AItemNo < 0 then Exit;

  vDrawItemNo := GetDrawItemNoByOffset(AItemNo, AOffset);
  vDrawRect := FDrawItems[vDrawItemNo].Rect;
  AX := AX - vDrawRect.Left;
  AY := AY - vDrawRect.Top;
  if FItems[AItemNo].StyleNo < THCStyle.RsNull then
  begin
    case FStyle.ParaStyles[FItems[AItemNo].ParaNo].AlignVert of  // 垂直对齐方式
      pavCenter: AY := AY - (vDrawRect.Height - (FItems[AItemNo] as THCCustomRectItem).Height) div 2;

      pavTop: AY := AY - FStyle.ParaStyles[FItems[AItemNo].ParaNo].LineSpaceHalf;
    else
      AY := AY - (vDrawRect.Height - (FItems[AItemNo] as THCCustomRectItem).Height);
    end;
  end;
end;

constructor THCCustomData.Create(const AStyle: THCStyle);
begin
  FStyle := AStyle;
  FDrawItems := THCDrawItems.Create;
  FItems := THCItems.Create;
  FCaretDrawItemNo := -1;
  FSelectInfo := TSelectInfo.Create;
end;

function THCCustomData.CreateDefaultDomainItem: THCCustomItem;
begin
  Result := THCDomainItem.Create(Self);
  Result.ParaNo := FStyle.CurParaNo;
end;

function THCCustomData.CreateDefaultTextItem: THCCustomItem;
begin
  Result := THCTextItem.Create;
  if FStyle.CurStyleNo < THCStyle.RsNull then
    Result.StyleNo := 0
  else
    Result.StyleNo := FStyle.CurStyleNo;

  Result.ParaNo := FStyle.CurParaNo;
end;

function THCCustomData.GetCurDrawItem: THCCustomDrawItem;
var
  vCurDItemNo: Integer;
begin
  vCurDItemNo := GetCurDrawItemNo;
  if vCurDItemNo < 0 then
    Result := nil
  else
    Result := FDrawItems[vCurDItemNo];
end;

function THCCustomData.GetCurDrawItemNo: Integer;
var
  i, vItemNo: Integer;
  vDItem: THCCustomDrawItem;
begin
  Result := -1;
  if SelectInfo.StartItemNo < 0 then  // 没有选择

  else
  begin
    if SelectExists then  // 有选中时，当前以选中结束位置的Item为当前Item
    begin
      if FSelectInfo.EndItemNo >= 0 then
        vItemNo := FSelectInfo.EndItemNo
      else
        vItemNo := FSelectInfo.StartItemNo;
    end
    else
      vItemNo := FSelectInfo.StartItemNo;
    if FItems[vItemNo].StyleNo < 0 then  // 非文本
      Result := FItems[vItemNo].FirstDItemNo
    else  // 文本
    begin
      for i := FItems[vItemNo].FirstDItemNo to FDrawItems.Count - 1 do
      begin
        vDItem := FDrawItems[i];
        if SelectInfo.StartItemOffset - vDItem.CharOffs + 1 <= vDItem.CharLen then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function THCCustomData.GetCurItem: THCCustomItem;
var
  vItemNo: Integer;
begin
  vItemNo := GetCurItemNo;
  if vItemNo < 0 then
    Result := nil
  else
    Result := FItems[vItemNo];
end;

function THCCustomData.GetCurItemNo: Integer;
begin
  {if IsEmptyData then
    Result := 0
  else}
    Result := FSelectInfo.StartItemNo
end;

function THCCustomData.DeleteSelected: Boolean;
begin
end;

destructor THCCustomData.Destroy;
begin
  FreeAndNil(FDrawItems);
  FreeAndNil(FItems);
  FreeAndNil(FSelectInfo);

  inherited Destroy;
end;

function THCCustomData.DisSelect: Boolean;
var
  i: Integer;
  vItem: THCCustomItem;
begin
  { THCCustomRichData.MouseUp看的DisSelectAfterStartItemNo中有保留起始不清选中，
   如果多处需要保留起始，可以在此方法增加是否保留起始参数以共用 }

  Result := SelectExists;
  if Result then  // 有选中内容
  begin
    // 如果选中是在RectItem中进，下面循环SelectInfo.EndItemNo<0，不能取消选中，所以单独处理StartItemNo
    vItem := FItems[SelectInfo.StartItemNo];
    vItem.DisSelect;
    vItem.Active := False;

    for i := SelectInfo.StartItemNo + 1 to SelectInfo.EndItemNo do  // 遍历选中的其他Item
    begin
      vItem := FItems[i];
      vItem.DisSelect;
      vItem.Active := False;
    end;
    SelectInfo.EndItemNo := -1;
    SelectInfo.EndItemOffset := -1;
  end
  else  // 没有选中
  if SelectInfo.StartItemNo >= 0 then
  begin
    vItem := FItems[SelectInfo.StartItemNo];
    vItem.DisSelect;
    vItem.Active := False;
  end;

  SelectInfo.StartItemNo := -1;
  SelectInfo.StartItemOffset := -1;
end;

procedure THCCustomData.DoDrawItemPaintAfter(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
end;

procedure THCCustomData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
end;

procedure THCCustomData.DrawItemPaintAfter(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDCState: Integer;
begin
  vDCState := SaveDC(ACanvas.Handle);
  try
    DoDrawItemPaintAfter(AData, ADrawItemNo, ADrawRect, ADataDrawLeft, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  finally
    ReleaseDC(ACanvas.Handle, vDCState);
  end;
end;

procedure THCCustomData.DrawItemPaintBefor(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDCState: Integer;
begin
  vDCState := SaveDC(ACanvas.Handle);
  try
    DoDrawItemPaintBefor(AData, ADrawItemNo, ADrawRect, ADataDrawLeft, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  finally
    ReleaseDC(ACanvas.Handle, vDCState);
  end;
end;

function THCCustomData.IsEmptyData: Boolean;
begin
  Result := (FItems.Count = 1) and (FItems[0].StyleNo > THCStyle.RsNull) and (FItems[0].Text = '');
end;

procedure THCCustomData.GetDataDrawItemRang(const ATop,
  ABottom: Integer; var AFirstDItemNo, ALastDItemNo: Integer);
var
  i: Integer;
begin
  AFirstDItemNo := -1;
  ALastDItemNo := -1;
  // 获取第一个可显示的DrawItem
  for i := 0 to FDrawItems.Count - 1 do
  begin
    if (FDrawItems[i].LineFirst)
      and (FDrawItems[i].Rect.Bottom > ATop)  // 底部超过区域上边
      and (FDrawItems[i].Rect.Top < ABottom)  // 顶部没超过区域下边
    then
    begin
      AFirstDItemNo := i;
      Break;
    end;
  end;

  if AFirstDItemNo < 0 then Exit;  // 第1个不存在则退出

  // 获取最后一个可显示的DrawItem
  for i := AFirstDItemNo to FDrawItems.Count - 1 do
  begin
    if (FDrawItems[i].LineFirst) and (FDrawItems[i].Rect.Top >= ABottom) then
    begin
      ALastDItemNo := i - 1;
      Break;
    end
    {else
    if (FDrawItems[i].Rect.Bottom > ABottom) then
    begin
      ALastDItemNo := i;
      Break;
    end};
  end;
  if ALastDItemNo < 0 then  // 高度超过Data高度时，以最后1个结束
    ALastDItemNo := FDrawItems.Count - 1;
end;

function THCCustomData.GetDrawItemNoByOffset(const AItemNo, AOffset: Integer): Integer;
var
  i: Integer;
  vDrawItem: THCCustomDrawItem;
begin
  Result := -1;
  if FItems[AItemNo].StyleNo < THCStyle.RsNull then  // RectItem
    Result := FItems[AItemNo].FirstDItemNo
  else  // TextItem
  begin
    for i := FItems[AItemNo].FirstDItemNo to FDrawItems.Count - 1 do
    begin
      vDrawItem := FDrawItems[i];
      if vDrawItem.ItemNo <> AItemNo then
        Break;

      if AOffset - vDrawItem.CharOffs < vDrawItem.CharLen then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function THCCustomData.GetDrawItemOffset(const ADrawItemNo, X: Integer): Integer;
var
  vX, vCharWidth: Integer;
  vDrawItem: THCCustomDrawItem;
  vText: string;
  vS: string;
  vLineLast: Boolean;

  i, j,
  vSplitCount,
  viSplitW,  // 各字符绘制时中间的间隔
  vMod: Integer;
  vItem: THCCustomItem;

  vParaStyle: THCParaStyle;
  vSplitList: THCIntegerList;
begin
  Result := 0;
  vDrawItem := FDrawItems[ADrawItemNo];
  vItem  := FItems[vDrawItem.ItemNo];
  if vItem.StyleNo < FStyle.RsNull then  // 非文本
    Result := (vItem as THCCustomRectItem).GetOffsetAt(X - vDrawItem.Rect.Left)
  else  // 文本
  begin
    Result := vDrawItem.CharLen;  // 赋值为最后，为方便行最右侧点击时返回为最后一个
    vText := (vItem as THCTextItem).GetTextPart(vDrawItem.CharOffs, vDrawItem.CharLen);
    FStyle.TextStyles[vItem.StyleNo].ApplyStyle(FStyle.DefCanvas);
    vParaStyle := FStyle.ParaStyles[vItem.ParaNo];
    vX := vDrawItem.Rect.Left;

    case vParaStyle.AlignHorz of
      pahLeft, pahRight, pahCenter:
        Result := GetCharOffsetByX(FStyle.DefCanvas, vText, X - vX);

      pahJustify, pahScatter:  // 20170220001 两端、分散对齐相关处理
        begin
          if vParaStyle.AlignHorz = pahJustify then  // 两端对齐
          begin
            if IsParaLastDrawItem(ADrawItemNo) then  // 两端对齐、段最后一行不处理
            begin
              Result := GetCharOffsetByX(FStyle.DefCanvas, vText, X - vX);
              Exit;
            end;
          end;
          vMod := 0;
          viSplitW := vDrawItem.Width - FStyle.DefCanvas.TextWidth(vText);  // 当前DItem的Rect中用于分散的空间
          // 计算当前Ditem内容分成几份，每一份在内容中的起始位置
          vSplitList := THCIntegerList.Create;
          try
            vSplitCount := GetJustifyCount(vText, vSplitList);
            vLineLast := IsLineLastDrawItem(ADrawItemNo);
            if vLineLast and (vSplitCount > 0) then  // 行最后DItem，少分一个
              Dec(vSplitCount);
            if vSplitCount > 0 then  // 有分到间距
            begin
              vMod := viSplitW mod vSplitCount;
              viSplitW := viSplitW div vSplitCount;
            end;

            //vSplitCount := 0;
            for i := 0 to vSplitList.Count - 2 do  // vSplitList最后一个是字符串长度所以多减1
            begin
              vS := Copy(vText, vSplitList[i], vSplitList[i + 1] - vSplitList[i]);  // 当前分隔的一个字符串
              vCharWidth := FStyle.DefCanvas.TextWidth(vS);
              if vMod > 0 then
              begin
                Inc(vCharWidth);  // 多分的余数
                vSplitCount := 1;
                Dec(vMod);
              end
              else
                vSplitCount := 0;
              { 增加间距 }
              if i <> vSplitList.Count - 2 then  // 不是当前DItem分隔的最后一个
                vCharWidth := vCharWidth + viSplitW  // 分隔间距
              else  // 是当前DItem分隔的最后一个
              begin
                if not vLineLast then  // 不是行最后一个DItem
                  vCharWidth := vCharWidth + viSplitW;  // 分隔间距
              end;

              if vX + vCharWidth > X then  // 当前字符结束位置在X后，找到了位置
              begin
                vMod := Length(vS);  // 借用变量，准备处理  a b c d e fgh ijklm n opq的形式(多个字符为一个分隔串)
                for j := 1 to vMod do  // 找在当前分隔的一个字符串中哪一个位置
                begin
                  vCharWidth := FStyle.DefCanvas.TextWidth(vS[j]);
                  if i <> vSplitList.Count - 2 then  // 不是当前DItem分隔的最后一个
                  begin
                    if j = vMod then
                      vCharWidth := vCharWidth + viSplitW + vSplitCount;
                  end
                  else  // 是当前DItem分隔的最后一个
                  begin
                    if not vLineLast then  // 不是行最后一个DItem
                      vCharWidth := vCharWidth + viSplitW + vSplitCount;  // 分隔间距
                  end;

                  vX := vX + vCharWidth;
                  if vX > X then  // 当前字符结束位置在X后
                  begin
                    if vX - vCharWidth div 2 > X then  // 点击在前半部分
                      Result := vSplitList[i] - 1 + j - 1  // 计为前一个后面
                    else
                      Result := vSplitList[i] - 1 + j;
                    Break;
                  end;
                end;

                Break;
              end;

              vX := vX + vCharWidth;
            end;
          finally
            vSplitList.Free;
          end;
        end;
    end;
  end;
end;

function THCCustomData.GetDrawItemOffsetWidth(const ADrawItemNo, ADrawOffs: Integer): Integer;
var
  vStyleNo: Integer;
  vAlignHorz: TParaAlignHorz;
  vDItem: THCCustomDrawItem;

  vSplitList: THCIntegerList;
  vLineLast: Boolean;
  vText, vS: string;
  i, j, viSplitW, vSplitCount, vMod, vCharWidth, vDOffset
    : Integer;
begin
  Result := 0;
  if ADrawOffs = 0 then Exit;

  vDItem := FDrawItems[ADrawItemNo];
  vStyleNo := FItems[vDItem.ItemNo].StyleNo;
  if vStyleNo < THCStyle.RsNull then  // 非文本
  begin
    if ADrawOffs > OffsetBefor then
      Result := FDrawItems[ADrawItemNo].Width;
  end
  else
  begin
    FStyle.TextStyles[vStyleNo].ApplyStyle(FStyle.DefCanvas);

    vAlignHorz := FStyle.ParaStyles[GetDrawItemParaStyle(ADrawItemNo)].AlignHorz;
    case vAlignHorz of
      pahLeft, pahRight, pahCenter:
        begin
          Result := FStyle.DefCanvas.TextWidth(Copy(FItems[vDItem.ItemNo].Text,
            vDItem.CharOffs, ADrawOffs));
        end;
      pahJustify, pahScatter:  // 20170220001 两端、分散对齐相关处理
        begin
          if vAlignHorz = pahJustify then  // 两端对齐
          begin
            if IsParaLastDrawItem(ADrawItemNo) then  // 两端对齐、段最后一行不处理
            begin
              Result := FStyle.DefCanvas.TextWidth(Copy(FItems[vDItem.ItemNo].Text,
                vDItem.CharOffs, ADrawOffs));
              Exit;
            end;
          end;

          vText := GetDrawItemText(ADrawItemNo);
          viSplitW := vDItem.Width - FStyle.DefCanvas.TextWidth(vText);  // 当前DItem的Rect中用于分散的空间
          vMod := 0;
          // 计算当前Ditem内容分成几份，每一份在内容中的起始位置
          vSplitList := THCIntegerList.Create;
          try
            vSplitCount := GetJustifyCount(vText, vSplitList);
            vLineLast := IsLineLastDrawItem(ADrawItemNo);
            if vLineLast and (vSplitCount > 0) then  // 行最后DItem，少分一个
              Dec(vSplitCount);
            if vSplitCount > 0 then  // 有分到间距
            begin
              vMod := viSplitW mod vSplitCount;
              viSplitW := viSplitW div vSplitCount;
            end;

            vSplitCount := 0;  // 借用变量
            for i := 0 to vSplitList.Count - 2 do  // vSplitList最后一个是字符串长度所以多减1
            begin
              vS := Copy(vText, vSplitList[i], vSplitList[i + 1] - vSplitList[i]);  // 当前分隔的一个字符串
              vCharWidth := FStyle.DefCanvas.TextWidth(vS);
              if vMod > 0 then
              begin
                Inc(vCharWidth);  // 多分的余数
                vSplitCount := 1;
                Dec(vMod);
              end
              else
                vSplitCount := 0;

              vDOffset := vSplitList[i] + Length(vS) - 1;
              if vDOffset <= ADrawOffs then  // 当前字符结束位置在AOffs前
              begin
                { 增加间距 }
                if i <> vSplitList.Count - 2 then  // 不是当前DItem分隔的最后一个
                  vCharWidth := vCharWidth + viSplitW  // 分隔间距
                else  // 是当前DItem分隔的最后一个
                begin
                  if not vLineLast then  // 不是行最后一个DItem
                    vCharWidth := vCharWidth + viSplitW;  // 分隔间距
                end;

                Result := Result + vCharWidth;
                if vDOffset = ADrawOffs then
                  Break;
              end
              else  // 当前字符结束位置在AOffs后，找具体位置
              begin
                // 准备处理  a b c d e fgh ijklm n opq的形式(多个字符为一个分隔串)
                for j := 1 to Length(vS) do  // 找在当前分隔的这串字符串中哪一个位置
                begin
                  vCharWidth := FStyle.DefCanvas.TextWidth(vS[j]);

                  vDOffset := vSplitList[i] - 1 + j;
                  if vDOffset = vDItem.CharLen then  // 是当前DItem最后一个分隔串
                  begin
                    if not vLineLast then  // 当前DItem不是行最后一个DItem
                      vCharWidth := vCharWidth + viSplitW + vSplitCount;  // 当前DItem最后一个字符享受分隔间距和多分的余数
                    //else 行最后一个DItem的最后一个字符不享受分隔间距和多分的余数，因为串格式化时最后一个分隔字符串右侧就不分间距
                  end;
                  Result := Result + vCharWidth;

                  if vDOffset = ADrawOffs then  // 当前字符结束位置在X后
                    Break;
                end;

                Break;
              end;
            end;
          finally
            vSplitList.Free;
          end;
        end;
    end;
  end;
end;

function THCCustomData.GetDrawItemParaStyle(const ADrawItemNo: Integer): Integer;
begin
  Result := GetItemParaStyle(FDrawItems[ADrawItemNo].ItemNo);
end;

function THCCustomData.GetDrawItemStyle(const ADrawItemNo: Integer): Integer;
begin
  Result := GetItemStyle(FDrawItems[ADrawItemNo].ItemNo);
end;

function THCCustomData.GetDrawItemText(const ADrawItemNo: Integer): string;
var
  vDItem: THCCustomDrawItem;
begin
  vDItem := FDrawItems[ADrawItemNo];
  Result := FItems[vDItem.ItemNo].Text;
  if Result <> '' then
    Result := Copy(Result, vDItem.CharOffs, vDItem.CharLen);
end;

{function THCCustomData.GetFormatStartItemNo(const AItemNo: Integer): Integer;
var
  i: Integer;
begin
  Result := AItemNo;
  for i := FItems[AItemNo].FirstDItemNo downto 0 do
  begin
    if FDrawItems[i].LineFirst then
    begin
      Result := FDrawItems[i].ItemNo;
      Break;
    end;
  end;
end;}

procedure THCCustomData.GetItemAt(const X, Y: Integer;
  var AItemNo, AOffset, ADrawItemNo: Integer; var ARestrain: Boolean);
var
  i, vStartDItemNo, vEndDItemNo: Integer;
  vDrawRect: TRect;
begin
  AItemNo := -1;
  AOffset := -1;
  ADrawItemNo := -1;
  ARestrain := True;  // 默认为约束找到(不在Item上面)

  if IsEmptyData then
  begin
    AItemNo := 0;
    AOffset := 0;
    ADrawItemNo := 0;
    Exit;
  end;

  { 获取对应位置最接近的起始DrawItem }
  if Y < 0 then
    vStartDItemNo := 0
  else  // 判断在哪一行
  begin
    vDrawRect := FDrawItems.Last.Rect;
    if Y > vDrawRect.Bottom then  // 最后一个下面
      vStartDItemNo := FDrawItems.Count - 1
    else  // 二分法查找哪个Item
    begin
      vStartDItemNo := 0;
      vEndDItemNo := FDrawItems.Count - 1;

      while True do
      begin
        if vEndDItemNo - vStartDItemNo > 1 then  // 相差大于1
        begin
          i := vStartDItemNo + (vEndDItemNo - vStartDItemNo) div 2;
          if Y > FDrawItems[i].Rect.Bottom then  // 大于中间位置
          begin
            vStartDItemNo := i + 1;  // 中间位置下一个
            Continue;
          end
          else
          if Y < FDrawItems[i].Rect.Top then  // 小于中间位置
          begin
            vEndDItemNo := i - 1;  // 中间位置上一个
            Continue;
          end
          else
          begin
            vStartDItemNo := i;  // 正好是中间位置的
            Break;
          end;
        end
        else  // 相差1
        begin
          if Y > FDrawItems[vEndDItemNo].Rect.Bottom then  // 第二个下面
            vStartDItemNo := vEndDItemNo
          else
          if Y >= FDrawItems[vEndDItemNo].Rect.Top then  // 第二个
            vStartDItemNo := vEndDItemNo;
          //else 不处理即第一个
          Break;
        end;
      end;
    end;

    if Y < FDrawItems[vStartDItemNo].Rect.Top then  // 处理在页底部数据下面时，vStartDItemNo是下一页第一个的情况
      Dec(vStartDItemNo);
  end;

  // 判断是指定行中哪一个Item
  GetLineDrawItemRang(vStartDItemNo, vEndDItemNo);  // 行起始和结束DrawItem
  if X <= FDrawItems[vStartDItemNo].Rect.Left then  // 在行第一个左边点击
  begin
    ADrawItemNo := vStartDItemNo;
    AItemNo := FDrawItems[vStartDItemNo].ItemNo;
    if FItems[AItemNo].StyleNo < THCStyle.RsNull then
      AOffset := GetDrawItemOffset(vStartDItemNo, X)
    else
      AOffset := FDrawItems[vStartDItemNo].CharOffs - 1;  // DrawItem起始
  end
  else
  if X >= FDrawItems[vEndDItemNo].Rect.Right then  // 在行右边点击
  begin
    ADrawItemNo := vEndDItemNo;
    AItemNo := FDrawItems[vEndDItemNo].ItemNo;
    if FItems[AItemNo].StyleNo < THCStyle.RsNull then
      AOffset := GetDrawItemOffset(vEndDItemNo, X)
    else
      AOffset := FDrawItems[vEndDItemNo].CharOffs + FDrawItems[vEndDItemNo].CharLen - 1;  // DrawItem最后
  end
  else
  begin
    for i := vStartDItemNo to vEndDItemNo do  // 行中间
    begin
      vDrawRect := FDrawItems[i].Rect;
      if (X >= vDrawRect.Left) and (X < vDrawRect.Right) then  // 2个中间算后面的
      begin
        ADrawItemNo := i;
        AItemNo := FDrawItems[i].ItemNo;
        if FItems[AItemNo].StyleNo < THCStyle.RsNull then
          AOffset := GetDrawItemOffset(i, X)
        else
          AOffset := FDrawItems[i].CharOffs + GetDrawItemOffset(i, X) - 1;
        ARestrain := (Y < vDrawRect.Top) or (Y > vDrawRect.Bottom);
        Break;
      end;
    end;
  end;
end;

function THCCustomData.GetItemLastDrawItemNo(const AItemNo: Integer): Integer;
//var
//  vItemNo: Integer;
begin
  Result := -1;
  // 在ReFormat中调用此方法时，当AItemNo前面存在没有格式化过的Item时，
  // AItemNo对应的原DrawItem的ItemNo属性是小于AItemNo的值，所以判断
  // AItemNo在重新格式化前的最后一个DrawItem，需要使用AItemNo原DrawItem的
  // ItemNo做为DrawItem兄弟的判断值
  // 正在格式化时最好不使用此方法，因为DrawItems.Count可能只是当前格式化到的Items
  {if FItems[AItemNo].FirstDItemNo < 0 then
    vItemNo := AItemNo
  else
    vItemNo := FDrawItems[FItems[AItemNo].FirstDItemNo].ItemNo; }
  if FItems[AItemNo].FirstDItemNo < 0 then Exit;  // 还没有格式化过

  Result := FItems[AItemNo].FirstDItemNo + 1;
  while Result < FDrawItems.Count do
  begin
    if FDrawItems[Result].ParaFirst or (FDrawItems[Result].ItemNo <> AItemNo) then
      Break
    else
      Inc(Result);
  end;
  Dec(Result);
end;

function THCCustomData.GetItemParaStyle(const AItemNo: Integer): Integer;
begin
  Result := FItems[AItemNo].ParaNo;
end;

function THCCustomData.GetItemStyle(const AItemNo: Integer): Integer;
begin
  Result := FItems[AItemNo].StyleNo;
end;

procedure THCCustomData.GetLineDrawItemRang(var AFirstDItemNo, ALastDItemNo: Integer);
begin
  while AFirstDItemNo > 0 do
  begin
    if FDrawItems[AFirstDItemNo].LineFirst then
      Break
    else
      Dec(AFirstDItemNo);
  end;

  ALastDItemNo := AFirstDItemNo + 1;
  while ALastDItemNo < FDrawItems.Count do
  begin
    if FDrawItems[ALastDItemNo].LineFirst then
      Break
    else
      Inc(ALastDItemNo);
  end;
  Dec(ALastDItemNo);
end;

function THCCustomData.GetLineFirstItemNo(const AItemNo,
  AOffset: Integer): Integer;
var
  vFirstDItemNo: Integer;
begin
  Result := AItemNo;
  vFirstDItemNo := GetDrawItemNoByOffset(AItemNo, AOffset);

  while vFirstDItemNo > 0 do
  begin
    if DrawItems[vFirstDItemNo].LineFirst then
      Break
    else
      Dec(vFirstDItemNo);
  end;

  Result := DrawItems[vFirstDItemNo].ItemNo;
end;

function THCCustomData.GetLineLastItemNo(const AItemNo,
  AOffset: Integer): Integer;
var
  vLastDItemNo: Integer;
begin
  Result := AItemNo;
  vLastDItemNo := GetDrawItemNoByOffset(AItemNo, AOffset) + 1;  // 下一个开始，否则行第一个获取最后一个时还是行第一个
  while vLastDItemNo < FDrawItems.Count do
  begin
    if FDrawItems[vLastDItemNo].LineFirst then
      Break
    else
      Inc(vLastDItemNo);
  end;
  Dec(vLastDItemNo);

  Result := DrawItems[vLastDItemNo].ItemNo;
end;

{procedure THCCustomData.GetParaDrawItemRang(const AItemNo: Integer;
  var AFirstDItemNo, ALastDItemNo: Integer);
var
  vFrItemNo, vLtItemNo: Integer;
begin
  GetParaItemRang(AItemNo, vFrItemNo, vLtItemNo);
  AFirstDItemNo := FItems[vFrItemNo].FirstDItemNo;
  ALastDItemNo := GetItemLastDrawItemNo(vLtItemNo);
end;}

function THCCustomData.GetParaFirstItemNo(const AItemNo: Integer): Integer;
begin
  Result := AItemNo;
  while Result > 0 do
  begin
    if FItems[Result].ParaFirst then
      Break
    else
      Dec(Result);
  end;
end;

procedure THCCustomData.GetParaItemRang(const AItemNo: Integer;
  var AFirstItemNo, ALastItemNo: Integer);
begin
  AFirstItemNo := AItemNo;
  while AFirstItemNo > 0 do
  begin
    if FItems[AFirstItemNo].ParaFirst then
      Break
    else
      Dec(AFirstItemNo);
  end;

  ALastItemNo := AItemNo + 1;
  while ALastItemNo < FItems.Count do
  begin
    if FItems[ALastItemNo].ParaFirst then
      Break
    else
      Inc(ALastItemNo);
  end;
  Dec(ALastItemNo);
end;

function THCCustomData.GetParaLastItemNo(const AItemNo: Integer): Integer;
begin
  // 目前需要外部自己约束AItemNo < FItems.Count
  Result := AItemNo + 1;
  while Result < FItems.Count do
  begin
    if FItems[Result].ParaFirst then
      Break
    else
      Inc(Result);
  end;
  Dec(Result);
end;

function THCCustomData.GetRootData: THCCustomData;
begin
  Result := Self;
end;

function THCCustomData.GetSelectEndDrawItemNo: Integer;
begin
  if FSelectInfo.EndItemNo < 0 then
    Result := -1
  else
    Result := GetDrawItemNoByOffset(FSelectInfo.EndItemNo,
      FSelectInfo.EndItemOffset);
end;

function THCCustomData.GetSelectStartDrawItemNo: Integer;
begin
  if FSelectInfo.StartItemNo < 0 then
    Result := -1
  else
  begin
    Result := GetDrawItemNoByOffset(FSelectInfo.StartItemNo,
      FSelectInfo.StartItemOffset);

    if (FSelectInfo.EndItemNo >= 0)
      and (FDrawItems[Result].CharOffsetEnd = FSelectInfo.StartItemOffset)
    then  // 有选中时，SelectInfo.StartItemOffset在本行最后时，要转为下一行行首
      Inc(Result);
  end;
end;

function THCCustomData.GetUndoList: THCUndoList;
begin
  if Assigned(FOnGetUndoList) then
    Result := FOnGetUndoList
  else
    Result := nil;
end;

procedure THCCustomData._FormatItemToDrawItems(const AItemNo, AOffset, AContentWidth: Integer;
  var APos: TPoint; var ALastDNo: Integer);

type
  TBreakPosition = (  // 截断位置
    jbpNone,  // 不截断
    jbpPrev  // 在前一个后面截断
    //jbpCur    // 在当前后面截断
    );

  {$REGION 'FinishLine'}
  /// <summary> 重整行 </summary>
  /// <param name="AEndDItemNo">行最后一个DItem</param>
  /// <param name="ARemWidth">行剩余宽度</param>
  procedure FinishLine(const ALineEndDItemNo, ARemWidth: Integer);
  var
    i, vLineBegDItemNo,  // 行第一个DItem
    vMaxBottom,
    viSplitW, vExtraW, vW
      : Integer;
    vReSize: Boolean;
    vAlignHorz: TParaAlignHorz;
    vLineSpaceCount,   // 当前行分几份
    vDItemSpaceCount,  // 当前DrawItem分几份
    vDWidth,
    vModWidth,
    vCountWillSplit  // 当前行有几个DItem参与分份
      : Integer;
    vDrawItemSplitCounts: array of Word;  // 当前行各DItem分几份
  begin
    { 根据行中最高的DrawItem处理其他DrawItem的高度 }
    vLineBegDItemNo := ALineEndDItemNo;
    for i := ALineEndDItemNo downto 0 do  // 得到行起始DItemNo
    begin
      if FDrawItems[i].LineFirst then
      begin
        vLineBegDItemNo := i;
        Break;
      end;
    end;
    Assert((vLineBegDItemNo >= 0), '断言失败：行起始DItemNo小于0！');
    // 找行DItem中Rect底位置最大的
    vReSize := False;  // 默认本行不需要调整各DItem的Rect
    // 现取样式，防止当下一段起始时，调用此方法结束上一段，2段样式不一样造成错误
    vMaxBottom := FDrawItems[ALineEndDItemNo].Rect.Bottom;  // 先默认行最后一个DItem的Rect底位置最大
    for i := ALineEndDItemNo - 1 downto vLineBegDItemNo do
    begin
      //FDrawItems[i].RemWidth := ARemWidth;
      if FDrawItems[i].Rect.Bottom <> vMaxBottom then  // 需要重新调整行中各DItem的Rect
        vReSize := True;
      if FDrawItems[i].Rect.Bottom > vMaxBottom then
        vMaxBottom := FDrawItems[i].Rect.Bottom;  // 记下最大的Rect底位置
    end;

    if vReSize then  // 需要重新调整行中各DItem高度，处理行内不同样式的DItem
    begin
      for i := ALineEndDItemNo downto vLineBegDItemNo do
        FDrawItems[i].Rect.Bottom := vMaxBottom;
    end;

    // 处理对齐方式，放在这里，是因为方便计算行起始和结束DItem，避免绘制时的运算
    vAlignHorz := FStyle.ParaStyles[GetDrawItemParaStyle(ALineEndDItemNo)].AlignHorz;
    case vAlignHorz of  // 段内容水平对齐方式
      pahLeft: ;
      pahRight:
        begin
          for i := ALineEndDItemNo downto vLineBegDItemNo do
            OffsetRect(FDrawItems[i].Rect, ARemWidth, 0);
        end;

      pahCenter:
        begin
          viSplitW := ARemWidth div 2;
          for i := ALineEndDItemNo downto vLineBegDItemNo do
            OffsetRect(FDrawItems[i].Rect, viSplitW, 0);
        end;

      pahJustify, pahScatter:  // 20170220001 两端、分散对齐相关处理
        begin
          if vAlignHorz = pahJustify then  // 两端对齐
          begin
            if IsParaLastDrawItem(ALineEndDItemNo) then  // 两端对齐，段最后一行不处理
              Exit;
          end
          else  // 分散对齐，空行或只有一个字符时居中
          begin
            if vLineBegDItemNo = ALineEndDItemNo then  // 行只有一个DrawItem
            begin
              if FItems[FDrawItems[vLineBegDItemNo].ItemNo].Length < 2 then  // 此DrawItem对应的内容长度不足2个按居中处理
              begin
                viSplitW := ARemWidth div 2;
                OffsetRect(FDrawItems[vLineBegDItemNo].Rect, viSplitW, 0);

                Exit;
              end;
            end;
          end;

          vCountWillSplit := 0;
          vLineSpaceCount := 0;
          vExtraW := 0;
          vModWidth := 0;
          viSplitW := ARemWidth;
          SetLength(vDrawItemSplitCounts, ALineEndDItemNo - vLineBegDItemNo + 1);
          for i := vLineBegDItemNo to ALineEndDItemNo do  // 计算空余分成几份
          begin
            if GetDrawItemStyle(i) < THCStyle.RsNull then  // RectItem
            begin
              if (FItems[FDrawItems[i].ItemNo] as THCCustomRectItem).JustifySplit then  // 分散对齐占间距
                vDItemSpaceCount := 1  // Graphic等占间距
              else
                vDItemSpaceCount := 0; // Tab等不占间距
            end
            else  // TextItem
            begin
              vDItemSpaceCount := GetJustifyCount(GetDrawItemText(i), nil);  // 当前DItem分了几份
              if (i = ALineEndDItemNo) and (vDItemSpaceCount > 0) then  // 行尾的DItem，少分一个
                Dec(vDItemSpaceCount);
            end;

            vDrawItemSplitCounts[i - vLineBegDItemNo] := vDItemSpaceCount;  // 记录当前DItem分几份
            vLineSpaceCount := vLineSpaceCount + vDItemSpaceCount;  // 记录行内总共分几份
            if vDItemSpaceCount > 0 then  // 当前DItem有分到间距
              Inc(vCountWillSplit);  // 增加分到间距的DItem数量
          end;

          if vLineSpaceCount > 1 then  // 份数大于1
          begin
            viSplitW := ARemWidth div vLineSpaceCount;  // 每一份的大小
            vDItemSpaceCount := ARemWidth mod vLineSpaceCount;  // 余数，借用变量
            if vDItemSpaceCount > vCountWillSplit then  // 余数大于行中参与分的DItem的数量
            begin
              vExtraW := vDItemSpaceCount div vCountWillSplit;  // 参与分的每一个DItem额外再分的量
              vModWidth := vDItemSpaceCount mod vCountWillSplit;  // 额外分完后剩余(小于行参与分DItem个数)
            end
            else  // 余数小于行中参与分的DItem数量
              vModWidth := vDItemSpaceCount;
          end;

          { 行中第一个DrawItem增加的空间 }
          if vDrawItemSplitCounts[0] > 0 then
          begin
            FDrawItems[vLineBegDItemNo].Rect.Right := FDrawItems[vLineBegDItemNo].Rect.Right
              + vDrawItemSplitCounts[0] * viSplitW + vExtraW;
            if vModWidth > 0 then  // 额外的没有分完
            begin
              Inc(FDrawItems[vLineBegDItemNo].Rect.Right);  // 当前DrawItem多分一个像素
              Dec(vModWidth);  // 额外的减少一个像素
            end;
          end;

          for i := vLineBegDItemNo + 1 to ALineEndDItemNo do  // 以第一个为基准，其余各DrawItem增加的空间
          begin
            vW := FDrawItems[i].Width;  // DrawItem原来Width
            if vDrawItemSplitCounts[i - vLineBegDItemNo] > 0 then  // 有分到间距
            begin
              vDWidth := vDrawItemSplitCounts[i - vLineBegDItemNo] * viSplitW + vExtraW;  // 多分到的width
              if vModWidth > 0 then  // 额外的没有分完
              begin
                if GetDrawItemStyle(i) < THCStyle.RsNull then
                begin
                  if (FItems[FDrawItems[i].ItemNo] as THCCustomRectItem).JustifySplit then
                  begin
                    Inc(vDWidth);  // 当前DrawItem多分一个像素
                    Dec(vModWidth);  // 额外的减少一个像素
                  end;
                end
                else
                begin
                  Inc(vDWidth);  // 当前DrawItem多分一个像素
                  Dec(vModWidth);  // 额外的减少一个像素
                end;
              end;
            end
            else  // 没有分到间距
              vDWidth := 0;

            FDrawItems[i].Rect.Left := FDrawItems[i - 1].Rect.Right;

            if GetDrawItemStyle(i) < THCStyle.RsNull then  // RectItem
            begin
              if (FItems[FDrawItems[i].ItemNo] as THCCustomRectItem).JustifySplit then  // 分散对齐占间距
                FDrawItems[i].Rect.Right := FDrawItems[i].Rect.Left + vW + vDWidth
              else
                FDrawItems[i].Rect.Right := FDrawItems[i].Rect.Left + vW;
            end
            else  // TextItem
              FDrawItems[i].Rect.Right := FDrawItems[i].Rect.Left + vW + vDWidth;
          end;
        end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'NewDrawItem'}
  procedure NewDrawItem(const AItemNo, AOffs, ACharLen: Integer;
    const ARect: TRect; const AParaFirst, ALineFirst: Boolean);
  var
    vDrawItem: THCCustomDrawItem;
  begin
    vDrawItem := THCCustomDrawItem.Create;
    vDrawItem.ItemNo := AItemNo;
    vDrawItem.CharOffs := AOffs;
    vDrawItem.CharLen := ACharLen;
    vDrawItem.ParaFirst := AParaFirst;
    vDrawItem.LineFirst := ALineFirst;
    vDrawItem.Rect := ARect;
    Inc(ALastDNo);
    FDrawItems.Insert(ALastDNo, vDrawItem);
    if AOffs = 1 then
      FItems[AItemNo].FirstDItemNo := ALastDNo;
  end;
  {$ENDREGION}

  {$REGION 'GetTextPlace'}
  //function GetTextPlace(const AWidth: Integer; const AStr: string): Integer;

  {$REGION '二分法'}
  {var
    viLen, viLastCan, vIndex: Integer;
    vWidth, vWidthCan: Integer;
    vTempStr: string;
    vNeedReCalc: Boolean;
    vSize: TSize;
  begin
    vWidthCan := 0;
    vNeedReCalc := False;
    // 二分法
    viLastCan := 0;
    viLen := Length(AStr);
    Result := viLen;
    vIndex := Result;
    while Result > 0 do
    begin
      vTempStr := Copy(AStr, 1, Result);
      //vWidth := FStyle.DefCanvas.TextWidth(vTempStr);
      Windows.GetTextExtentPoint32(FStyle.DefCanvas.Handle, vTempStr, Result, vSize);
      vWidth := vSize.cx;
      if vWidth > AWidth then  // 放不下
      begin
        if viLastCan > 0 then // 由能放下到放不下
        begin
          vNeedReCalc := True;  // 需要重新确定具体断点
          Break;
        end;
        Result := Result - vIndex div 2;  // 往前二分
        if Result = vIndex then  // 一个也放不下?
        begin
          vNeedReCalc := True;
          Break;
        end;
        vIndex := Result;
        if Result = viLastCan then
        begin
          vNeedReCalc := False;  // 确定断点
          Break;
        end;
      end
      else
      begin
        if Result = viLastCan then
        begin
          vNeedReCalc := False;  // 确定断点
          Break;
        end;
        viLastCan := Result;  // 记录最后一个不需要断点的位置
        vIndex := viLastCan;
        vWidthCan := vWidth;  // 最后一个不需要断点所占掉宽度
        Result := Result + (viLen - Result) div 2;  // 往后二分
      end;
    end;

    if vNeedReCalc then  // 需要确定断点
    begin
      vWidth := AWidth - vWidthCan;  // 除最后能放下的后剩余宽度
      viLen := Result - viLastCan;  // 除最后能放下的到放不下的中间个数
      for vIndex := 1 to viLen do  // 依次判断在哪处断点
      begin
        vTempStr := Copy(AStr, viLastCan + 1, vIndex);
        Windows.GetTextExtentPoint32(FStyle.DefCanvas.Handle, vTempStr, vIndex, vSize);
        vWidthCan := vSize.cx;  // FStyle.DefCanvas.TextWidth(vTempStr);
        if vWidthCan > vWidth then
        begin
          Result := viLastCan + vIndex - 1;  // 确定断点
          Break;
        end;
      end;
    end;
  end; }
  {$ENDREGION}

  {var
    i, vWidth, vCharWidth: Integer;
  begin
    Result := 0;
    vWidth := FStyle.DefCanvas.TextWidth(AStr);  // 当前字符串宽度
    vCharWidth := 0;
    for i := Length(AStr) downto 1 do  // 从后往前找截断位置(比二分法更快吧)
    begin
      vCharWidth := vCharWidth + FStyle.DefCanvas.TextWidth(AStr[i]);  // 从后往前字符宽度和
      if vWidth - vCharWidth <= AWidth then
      begin
        Result := i - 1;
        Break;
      end;
    end;
  end;}
  {$ENDREGION}

  {$REGION 'FindLineBreak'}
  /// <summary>
  /// 获取字符串排版时截断到下一行的位置
  /// </summary>
  /// <param name="AText"></param>
  /// <param name="APos">在第X个后面断开 X > 0</param>
  procedure FindLineBreak(const AText: string; const AStartPos: Integer; var APos: Integer);

    {$REGION 'GetHeadTailBreak 根据行首、尾对字符的约束条件，获取截断位置'}
    procedure GetHeadTailBreak(const AText: string; var APos: Integer);
    var
      vChar: Char;
    begin
      vChar := AText[APos + 1];  // 因为是要处理截断，所以APos肯定是小于Length(AText)的，不用考虑越界
      if PosCharHC(vChar, DontLineFirstChar) > 0 then  // 下一个是不能放在行首的字符
      begin
        Dec(APos);  // 当前要移动到下一行，往前一个截断重新判断
        GetHeadTailBreak(AText, APos);
      end
      else  // 下一个可以放在行首，当前位置能否放置到行尾
      begin
        vChar := AText[APos];  // 当前位置字符
        if PosCharHC(vChar, DontLineLastChar) > 0 then  // 是不能放在行尾的字符
        begin
          Dec(APos);  // 再往前寻找截断位置
          GetHeadTailBreak(AText, APos);
        end;
      end;
    end;
    {$ENDREGION}

    function MatchBreak(const APrevType, APosType: TCharType): TBreakPosition;
    begin
      Result := jbpNone;
      case APosType of
        jctHZ:
          begin
            if APrevType in [jctZM, jctSZ, jctHZ] then  // 当前位置是汉字，前一个是字母、数字、汉字
              Result := jbpPrev;
          end;

        jctZM:
          begin
            if not (APrevType in [jctZM, jctSZ]) then  // 当前是字母，前一个不是数字、字母
              Result := jbpPrev;
          end;

        jctSZ:
          begin
            if not (APrevType in [jctZM, jctSZ]) then  // 当前是数字，前一个不是字母、数字
              Result := jbpPrev;
          end;

        jctFH:
          begin
            if APrevType <> jctFH then  // 当前是符号，前一个不是符号
              Result := jbpPrev;
          end;
      end;
    end;

  var
    i: Integer;
    vPosType, vPrevType, vNextType: TCharType;
  begin
    GetHeadTailBreak(AText, APos);  // 根据行首、尾的约束条件找APos不符合时应该在哪一个位置并重新赋值给APos

    vPosType := GetCharType(Word(AText[APos]));  // 当前类型
    vNextType := GetCharType(Word(AText[APos + 1]));  // 下一个字符类型

    if MatchBreak(vPosType, vNextType) <> jbpPrev then  // 不能在当前截断，当前往前找截断
    begin
      if vPosType <> jctBreak then
      begin
        for i := APos - 1 downto AStartPos + 1 do
        begin
          vPrevType := GetCharType(Word(AText[i]));
          if MatchBreak(vPrevType, vPosType) = jbpPrev then
          begin
            APos := i;
            Break;
          end;

          vPosType := vPrevType;
        end;
      end;
    end;
  end;
  {$ENDREGION}

var
  vText: string;
  vRect: TRect;
  viLen,  // 文本Item字符串长度
  vItemHeight,  // 当前Item高度
  vRemainderWidth
    : Integer;
  vItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
  vParaStyle: THCParaStyle;
  vParaFirst, vLineFirst: Boolean;
  vCharWidths: array of Cardinal;

  procedure DoFormatRectItemToDrawItem;
  var
    vWidth: Integer;
  begin
    vRectItem.FormatToDrawItem(Self, AItemNo);
    vWidth := AContentWidth - APos.X;
    if (vRectItem.Width > vWidth) and (not vLineFirst) then  // 当前行剩余宽度放不下且不是行首
    begin
      // 偏移到下一行
      FinishLine(ALastDNo, vWidth);
      APos.X := 0;
      APos.Y := FDrawItems[ALastDNo].Rect.Bottom;
      vLineFirst := True;  // 作为行首
    end;

    // 当前行空余宽度能放下或放不下但已经是行首了
    vRect.Left := APos.X;
    vRect.Top := APos.Y;
    vRect.Right := vRect.Left + vRectItem.Width;
    vRect.Bottom := vRect.Top + vRectItem.Height + vParaStyle.LineSpace;
    NewDrawItem(AItemNo, AOffset, 1, vRect, vParaFirst, vLineFirst);

    vRemainderWidth := AContentWidth - vRect.Right;  // 放入后的剩余量
  end;

  /// <summary> 从指定偏移和指定位置开始格式化Text </summary>
  /// <param name="ACharOffset">文本格式化的起始偏移</param>
  /// <param name="APlaceWidth">呈放文本的宽度</param>
  /// <param name="ABasePos">vCharWidths中对应偏移的起始位置</param>
  procedure DoFormatTextItemToDrawItems(const ACharOffset, APlaceWidth, ABasePos: Integer);
  var
    i, viPlaceOffset,  // 能放下第几个字符
    viBreakOffset,  // 第几个字符放不下
    vFirstCharWidth  // 第一个字符的宽度
      : Integer;
  begin
    vLineFirst := APos.X = 0;
    viBreakOffset := 0;  // 换行位置，第几个字符放不下
    vFirstCharWidth := vCharWidths[ACharOffset - 1] - ABasePos;  // 第一个字符的宽度

    for i := ACharOffset - 1 to viLen - 1 do
    begin
      if vCharWidths[i] - ABasePos > APlaceWidth then
      begin
        viBreakOffset := i + 1;
        Break;
      end;
    end;

    if viBreakOffset < 1 then  // 当前行剩余空间把vText全放置下了
    begin
      vRect.Left := APos.X;
      vRect.Top := APos.Y;
      vRect.Right := vRect.Left + vCharWidths[viLen - 1] - ABasePos;  // 使用自定义测量的结果
      vRect.Bottom := vRect.Top + vItemHeight;
      NewDrawItem(AItemNo, ACharOffset, viLen - ACharOffset + 1, vRect, vParaFirst, vLineFirst);
      vParaFirst := False;

      vRemainderWidth := AContentWidth - vRect.Right;  // 放入最多后的剩余量
    end
    else
    if viBreakOffset = 1 then  // 当前行剩余空间连第一个字符也放不下
    begin
      if vFirstCharWidth > AContentWidth then  // Data的宽度不足一个字符
      begin
        vRect.Left := APos.X;
        vRect.Top := APos.Y;
        vRect.Right := vRect.Left + vCharWidths[viLen - 1] - ABasePos;  // 使用自定义测量的结果
        vRect.Bottom := vRect.Top + vItemHeight;
        NewDrawItem(AItemNo, ACharOffset, 1, vRect, vParaFirst, vLineFirst);
        vParaFirst := False;

        vRemainderWidth := AContentWidth - vRect.Right;  // 放入最多后的剩余量
        FinishLine(ALastDNo, vRemainderWidth);

        // 偏移到下一行顶端，准备另起一行
        APos.X := 0;
        APos.Y := FDrawItems[ALastDNo].Rect.Bottom;  // 不使用 vRect.Bottom 因为如果行中间有高的，会修正vRect.Bottom

        if viBreakOffset < viLen then
          DoFormatTextItemToDrawItems(viBreakOffset + 1, AContentWidth, vCharWidths[viBreakOffset - 1]);
      end
      else
      begin
        vRemainderWidth := APlaceWidth;
        FinishLine(ALastDNo, vRemainderWidth);
        // 偏移到下一行开始计算
        APos.X := 0;
        APos.Y := FDrawItems[ALastDNo].Rect.Bottom;
        //if not vLineFirst then
        DoFormatTextItemToDrawItems(ACharOffset, AContentWidth, ABasePos);
      end;
    end
    else  // 当前行剩余宽度能放下当前Text的一部分
    begin
      if vFirstCharWidth > AContentWidth then  // Data的宽度不足一个字符
        viPlaceOffset := viBreakOffset
      else
        viPlaceOffset := viBreakOffset - 1;  // 第viBreakOffset个字符放不下，前一个能放下

      FindLineBreak(vText, ACharOffset, viPlaceOffset);  // 判断从viPlaceOffset后打断是否合适

      if viPlaceOffset < ACharOffset then  // 找不到截断位置，就在原位置截断(如整行文本都是逗号)
      begin
        if vFirstCharWidth > AContentWidth then  // Data的宽度不足一个字符
          viPlaceOffset := viBreakOffset
        else
          viPlaceOffset := viBreakOffset - 1;
      end;

      vRect.Left := APos.X;
      vRect.Top := APos.Y;
      vRect.Right := vRect.Left + vCharWidths[viPlaceOffset - 1] - ABasePos;  // 使用自定义测量的结果
      vRect.Bottom := vRect.Top + vItemHeight;

      NewDrawItem(AItemNo, ACharOffset, viPlaceOffset - ACharOffset + 1, vRect, vParaFirst, vLineFirst);
      vParaFirst := False;

      vRemainderWidth := AContentWidth - vRect.Right;  // 放入最多后的剩余量
      FinishLine(ALastDNo, vRemainderWidth);

      // 偏移到下一行顶端，准备另起一行
      APos.X := 0;
      APos.Y := FDrawItems[ALastDNo].Rect.Bottom;  // 不使用 vRect.Bottom 因为如果行中间有高的，会修正vRect.Bottom

      if viPlaceOffset < viLen then
        DoFormatTextItemToDrawItems(viPlaceOffset + 1, AContentWidth, vCharWidths[viPlaceOffset - 1]);
    end;
  end;

var
  vSize: TSize;
begin
  if not FItems[AItemNo].Visible then Exit;

  vRemainderWidth := 0;
  vItem := FItems[AItemNo];
  vParaStyle := FStyle.ParaStyles[vItem.ParaNo];
  if (AOffset = 1) and vItem.ParaFirst then  // 第一次处理段第一个Item
  begin
    vParaFirst := True;
    vLineFirst := True;
  end
  else  // 非段第1个
  begin
    vParaFirst := False;
    vLineFirst := APos.X = 0;
  end;

  if vItem.StyleNo < THCStyle.RsNull then  // 是RectItem
  begin
    vRectItem := vItem as THCCustomRectItem;
    DoFormatRectItemToDrawItem;
  end
  else  // 文本
  begin
    FStyle.TextStyles[vItem.StyleNo].ApplyStyle(FStyle.DefCanvas);
    //vItemHeight := FStyle.DefCanvas.TextHeight('字') + vParaStyle.LineSpace;  // 行高
    Windows.GetTextExtentPoint32(FStyle.DefCanvas.Handle, '字', 1, vSize);
    vItemHeight := vSize.cy + vParaStyle.LineSpace;  // 行高
    vRemainderWidth := AContentWidth - APos.X;
    vText := vItem.Text;

    if vText = '' then  // 空item(肯定是空行)
    begin
      Assert(vItem.ParaFirst, HCS_EXCEPTION_NULLTEXT);
      vRect.Left := APos.X;
      vRect.Top := APos.Y;
      vRect.Right := 0;
      vRect.Bottom := vRect.Top + vItemHeight;  //DefaultCaretHeight;
      vParaFirst := True;
      vLineFirst := True;
      NewDrawItem(AItemNo, AOffset, 0, vRect, vParaFirst, vLineFirst);
      vParaFirst := False;
    end
    else  // 非空Item
    begin
      viLen := Length(vText);
      SetLength(vCharWidths, viLen);

      GetTextExtentExPoint(FStyle.DefCanvas.Handle, PChar(vText), viLen, 0,  //vRemainderWidth,
        nil, PInteger(vCharWidths), vSize);

      DoFormatTextItemToDrawItems(AOffset, AContentWidth - APos.X, 0);

      SetLength(vCharWidths, 0);
    end;
  end;

  // 计算下一个的位置
  if AItemNo = FItems.Count - 1 then  // 是最后一个
    FinishLine(ALastDNo, vRemainderWidth)
  else  // 不是最后一个，则为下一个Item准备位置
  begin
    if FItems[AItemNo + 1].ParaFirst then // 下一个是段起始
    begin
      FinishLine(ALastDNo, vRemainderWidth);
      // 偏移到下一行顶端，准备另起一行
      APos.X := 0;
      APos.Y := FDrawItems[ALastDNo].Rect.Bottom;  // 不使用 vRect.Bottom 因为如果行中间有高的，会修正其bottom
    end
    else  // 下一个不是段起始
      APos.X := vRect.Right;  // 下一个的起始坐标
  end;
end;

function THCCustomData.IsLineLastDrawItem(const ADrawItemNo: Integer): Boolean;
begin
  // 不能在格式化进行中使用，因为DrawItems.Count可能只是当前格式化到的Item
  Result := (ADrawItemNo = FDrawItems.Count - 1) or (FDrawItems[ADrawItemNo + 1].LineFirst);
  {(ADItemNo < FDrawItems.Count - 1) and (not FDrawItems[ADItemNo + 1].LineFirst)}
end;

function THCCustomData.IsParaLastDrawItem(const ADrawItemNo: Integer): Boolean;
var
  vItemNo: Integer;
begin
  Result := False;
  vItemNo := FDrawItems[ADrawItemNo].ItemNo;
  if vItemNo < FItems.Count - 1 then  // 不是最后一个Item
  begin
    if FItems[vItemNo + 1].ParaFirst then  // 下一个是段首
      Result := FDrawItems[ADrawItemNo].CharOffsetEnd = FItems[vItemNo].Length;  // 是Item最后一个DrawItem
  end
  else  // 是最后一个Item
    Result := FDrawItems[ADrawItemNo].CharOffsetEnd = FItems[vItemNo].Length;  // 是Item最后一个DrawItem
  // 不能用下面这样的判断，因为正在格式化进行时，当前肯定是DrawItems的最后一个
  //Result :=(ADItemNo = FDrawItems.Count - 1) or (FDrawItems[ADItemNo + 1].ParaFirst);
end;

function THCCustomData.IsParaLastItem(const AItemNo: Integer): Boolean;
begin
  Result := (AItemNo = FItems.Count - 1) or (FItems[AItemNo + 1].ParaFirst);
end;

procedure THCCustomData.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  Clear;
end;

procedure THCCustomData.MarkStyleUsed(const AMark: Boolean);
var
  i: Integer;
  vItem: THCCustomItem;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    vItem := FItems[i];
    if AMark then  // 标记
    begin
      FStyle.ParaStyles[vItem.ParaNo].CheckSaveUsed := True;
      if vItem.StyleNo < THCStyle.RsNull then
        (vItem as THCCustomRectItem).MarkStyleUsed(AMark)
      else
        FStyle.TextStyles[vItem.StyleNo].CheckSaveUsed := True;
    end
    else  // 重新赋值
    begin
      vItem.ParaNo := FStyle.ParaStyles[vItem.ParaNo].TempNo;
      if vItem.StyleNo < THCStyle.RsNull then
        (vItem as THCCustomRectItem).MarkStyleUsed(AMark)
      else
        vItem.StyleNo := FStyle.TextStyles[vItem.StyleNo].TempNo;
    end;
  end;
end;

procedure THCCustomData.MatchItemSelectState;

  {$REGION ' CheckItemSelectedState检测某个Item的选中状态 '}
  procedure CheckItemSelectedState(const AItemNo: Integer);
  begin
    if (AItemNo > SelectInfo.StartItemNo) and (AItemNo < SelectInfo.EndItemNo) then  // 在选中范围之间
      Items[AItemNo].SelectComplate
    else
    if AItemNo = SelectInfo.StartItemNo then  // 选中起始
    begin
      if AItemNo = SelectInfo.EndItemNo then  // 选中在同一个Item
      begin
        if Items[AItemNo].StyleNo < THCStyle.RsNull then  // RectItem
        begin
          if (SelectInfo.StartItemOffset = OffsetInner)
            or (SelectInfo.EndItemOffset = OffsetInner)
          then
            Items[AItemNo].SelectPart
          else
            Items[AItemNo].SelectComplate;
        end
        else  // TextItem
        begin
          if (SelectInfo.StartItemOffset = 0)
            and (SelectInfo.EndItemOffset = Items[AItemNo].Length)
          then
            Items[AItemNo].SelectComplate
          else
            Items[AItemNo].SelectPart;
        end;
      end
      else  // 选中在不同的Item，当前是起始
      begin
        if SelectInfo.StartItemOffset = 0 then
          Items[AItemNo].SelectComplate
        else
          Items[AItemNo].SelectPart;
      end;
    end
    else  // 选中在不同的Item，当前是结尾 if AItemNo = SelectInfo.EndItemNo) then
    begin
      if Items[AItemNo].StyleNo < THCStyle.RsNull then  // RectItem
      begin
        if SelectInfo.EndItemOffset = OffsetAfter then
          Items[AItemNo].SelectComplate
        else
          Items[AItemNo].SelectPart;
      end
      else  // TextItem
      begin
        if SelectInfo.EndItemOffset = Items[AItemNo].Length then
          Items[AItemNo].SelectComplate
        else
          Items[AItemNo].SelectPart;
      end;
    end;
  end;
  {$ENDREGION}

var
  i: Integer;
begin
  if SelectExists then
  begin
    for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do  // 起始结束之间的按全选中处理
      CheckItemSelectedState(i);
  end;
end;

function THCCustomData.OffsetInSelect(const AItemNo, AOffset: Integer): Boolean;
begin
  Result := False;
  if (AItemNo < 0) or (AOffset < 0) then Exit;

  if FItems[AItemNo].StyleNo < THCStyle.RsNull then // 非文本粗略判断，如需要精确用CoordInSelect间接调用
  begin
    if (AOffset = OffsetInner) and FItems[AItemNo].IsSelectComplate then
      Result := True;

    Exit;
  end;

  if SelectExists then
  begin
    if (AItemNo > FSelectInfo.StartItemNo) and (AItemNo < FSelectInfo.EndItemNo) then
      Result := True
    else
    if AItemNo = FSelectInfo.StartItemNo then
    begin
      if AItemNo = FSelectInfo.EndItemNo then
        Result := (AOffset >= FSelectInfo.StartItemOffset) and (AOffset <= FSelectInfo.EndItemOffset)
      else
        Result := AOffset >= FSelectInfo.StartItemOffset;
    end
    else
    if AItemNo = FSelectInfo.EndItemNo then
      Result := AOffset <= FSelectInfo.EndItemOffset;
  end;
end;

procedure THCCustomData.PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
  ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vFristDItemNo, vLastDItemNo: Integer;
  vTextDrawTop: Integer;

  {$REGION ' 当前显示范围内要绘制的DrawItem全部是选中的 '}
  function DrawItemSelectAll: Boolean;
  var
    vSelStartDItemNo, vSelEndDItemNo: Integer;
  begin
    vSelStartDItemNo := GetSelectStartDrawItemNo;
    vSelEndDItemNo := GetSelectEndDrawItemNo;

    Result :=  // 当前页是否全选中了
      (
        (vSelStartDItemNo < vFristDItemNo)
        or
        (
          (vSelStartDItemNo = vFristDItemNo)
          and
          (SelectInfo.StartItemOffset = FDrawItems[vSelStartDItemNo].CharOffs)
        )
      )
      and
      (
        (vSelEndDItemNo > vLastDItemNo)
        or
        (
          (vSelEndDItemNo = vLastDItemNo)
          and
          (SelectInfo.EndItemOffset = FDrawItems[vSelEndDItemNo].CharOffs + FDrawItems[vSelEndDItemNo].CharLen)
        )
      );
  end;
  {$ENDREGION}

  {$REGION ' DrawTextJsutify 20170220001 分散对齐相关处理 '}
  procedure DrawTextJsutify(const ARect: TRect; const AText: string; const ALineLast: Boolean);
  var
    vSplitCount, vX, vLen, viSplitW, vMod: Integer;
    vSplitList: THCIntegerList;
    i: Integer;
    vS: string;
    //vRect: TRect;
  begin
    vMod := 0;
    vX := ARect.Left;
    viSplitW := (ARect.Right - ARect.Left) - FStyle.DefCanvas.TextWidth(AText);
    // 计算当前Ditem内容分成几份，每一份在内容中的起始位置
    vSplitList := THCIntegerList.Create;
    try
      vSplitCount := GetJustifyCount(AText, vSplitList);
      if ALineLast and (vSplitCount > 0) then  // 行最后DItem，少分一个
        Dec(vSplitCount);
      if vSplitCount > 0 then  // 有分到间距
      begin
        vMod := viSplitW mod vSplitCount;
        viSplitW := viSplitW div vSplitCount;
      end;

      for i := 0 to vSplitList.Count - 2 do  // vSplitList最后一个是字符串长度所以多减1
      begin
        vLen := vSplitList[i + 1] - vSplitList[i];
        vS := Copy(AText, vSplitList[i], vLen);
        //vRect := Rect(vX, vTextDrawTop, ARect.Right, ARect.Bottom);
        //Windows.DrawText(ACanvas.Handle, vS, -1, vRect, DT_LEFT or DT_SINGLELINE or vAlignVert);
        //ACanvas.TextOut(vX, vTextDrawTop, vS);

        { 201805161718
        ETO_CLIPPED：正文将裁剪到矩形中。
        ETO_GLYPH_INDEX：LpString指向由GetCharacterPlacement返回的数组，如果没有进一步的特殊语言处理的要求，则此数组直接由GDI解析，仅对字体应用符号索引，但此标志可用于位图和向量字体，以表示不必做进一步的语言处理，GDI应用直接处理此字符串。
        ETO_OPAQUE：用当前的背景色来填充矩形。
        ETO_RTLREADING：在Middle_Eastern Windows中如果指定了此值，且Hebrew或Arabic字体被选进设备环境，则此字符串用以从右到左的阅读顺序来输出。如果没有指定此值，则字符串以从左到右的顺序输出。在SetTextAlign中设置TA_RTLREADING值可获得同样的效果。为向后兼容，此值作为保留值。
        ETO_GLYPH_INDEX和ETO_RTLREADING值不能在一起使用。因为ETO_GLYPH_INDEX表示所有的语言处理已经完成，函数就会忽略被指定的ETO_RTLREADING值。}
        Windows.ExtTextOut(ACanvas.Handle, vX, vTextDrawTop, ETO_OPAQUE,
          nil, PChar(vS), vLen, nil);
        vX := vX + FStyle.DefCanvas.TextWidth(vS) + viSplitW;
        if vMod > 0 then
        begin
          Inc(vX);
          Dec(vMod);
        end;
      end;
    finally
      vSplitList.Free;
    end;
  end;
  {$ENDREGION}

var
  i, vSelStartDNo, vSelStartDOffs, vSelEndDNo, vSelEndDOffs,
  vPrioStyleNo, vPrioParaNo, vVOffset, vTextHeight: Integer;
  vItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
  vDItem: THCCustomDrawItem;
  vAlignHorz: TParaAlignHorz;
  vDrawRect: TRect;
  S: string;

  //vCharWidths: array of Integer;
  {j, vFit, }vLen: Integer;
  //vSize: TSize;

  vDrawsSelectAll: Boolean;
  vDCState: Integer;
begin
  if FItems.Count = 0 then Exit;

  vVOffset := ADataDrawTop - AVOffset;  // 将数据起始位置映射到绘制位置

  GetDataDrawItemRang(Max(ADataDrawTop, ADataScreenTop) - vVOffset,  // 可显示出来的DItem范围
    Min(ADataDrawBottom, ADataScreenBottom) - vVOffset, vFristDItemNo, vLastDItemNo);

  if (vFristDItemNo < 0) or (vLastDItemNo < 0) then Exit;

  // 选中信息
  vSelStartDNo := GetSelectStartDrawItemNo;  // 选中起始DItem
  if vSelStartDNo < 0 then
    vSelStartDOffs := -1
  else
    vSelStartDOffs := FSelectInfo.StartItemOffset - FDrawItems[vSelStartDNo].CharOffs + 1;
  vSelEndDNo := GetSelectEndDrawItemNo;      // 选中结束DrawItem
  if vSelEndDNo < 0 then
    vSelEndDOffs := -1
  else
    vSelEndDOffs := FSelectInfo.EndItemOffset - FDrawItems[vSelEndDNo].CharOffs + 1;
  vDrawsSelectAll := DrawItemSelectAll;

  vPrioStyleNo := -1;
  vPrioParaNo := -1;

  ACanvas.Refresh;
  vDCState := SaveDC(ACanvas.Handle);
  try
    for i := vFristDItemNo to vLastDItemNo do  // 遍历要绘制的数据
    begin
      vDItem := FDrawItems[i];
      vItem := FItems[vDItem.ItemNo];
      vDrawRect := vDItem.Rect;
      OffsetRect(vDrawRect, ADataDrawLeft, vVOffset);  // 偏移到指定的画布绘制位置(SectionData时为页数据在格式化中可显示起始位置)

      { 绘制内容前 }
      DrawItemPaintBefor(Self, i, vDrawRect, ADataDrawLeft, ADataDrawBottom,
        ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

      if vPrioParaNo <> vItem.ParaNo then  // 水平对齐方式
      begin
        vPrioParaNo := vItem.ParaNo;
        vAlignHorz := FStyle.ParaStyles[vItem.ParaNo].AlignHorz;  // 段内容水平对齐方式
      end;

      if vItem.StyleNo < THCStyle.RsNull then  // RectItem自行处理绘制
      begin
        vRectItem := vItem as THCCustomRectItem;

        vPrioStyleNo := vRectItem.StyleNo;

        if vRectItem.IsSelectComplate then  // 选中背景区域
        begin
          ACanvas.Brush.Color := FStyle.SelColor;
          ACanvas.FillRect(vDrawRect);
        end;
        // 除去行间距净Rect，即内容的显示区域(不需要除去，因为vDrawRect已经是净高了)
        InflateRect(vDrawRect, 0, -FStyle.ParaStyles[vItem.ParaNo].LineSpaceHalf);
        if vRectItem.JustifySplit then  // 分散占空间
          vDrawRect.Right := vDrawRect.Left + vRectItem.Width;

        case FStyle.ParaStyles[vItem.ParaNo].AlignVert of  // 垂直对齐方式
          pavCenter: InflateRect(vDrawRect, 0, -(vDrawRect.Height - vRectItem.Height) div 2);
          pavTop: ;
        else
          vDrawRect.Top := vDrawRect.Bottom - vRectItem.Height;
        end;

        vItem.PaintTo(FStyle, vDrawRect, ADataDrawTop, ADataDrawBottom,
          ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
      end
      else  // 文本Item
      begin
        if vItem.StyleNo <> vPrioStyleNo then  // 需要重新应用样式
        begin
          vPrioStyleNo := vItem.StyleNo;
          FStyle.TextStyles[vPrioStyleNo].ApplyStyle(ACanvas);
          FStyle.TextStyles[vPrioStyleNo].ApplyStyle(FStyle.DefCanvas);
          vTextHeight := FStyle.DefCanvas.TextHeight('字');
        end;

        // 文字背景
        if ACanvas.Brush.Style <> bsClear then
        begin
          ACanvas.Brush.Color := FStyle.TextStyles[vPrioStyleNo].BackColor;
          ACanvas.FillRect(Rect(vDrawRect.Left, vDrawRect.Top, vDrawRect.Left + vDItem.Width, vDrawRect.Bottom));
        end;

        { 绘制文字、段、选中情况下的背景 }
        if not APaintInfo.Print then  // 不是打印
        begin
          if vDrawsSelectAll then  // 当前要绘制的起始和结束DrawItem都被选中或单元格被全选中，背景为选中
          begin
            ACanvas.Brush.Color := FStyle.SelColor;
            ACanvas.FillRect(Rect(vDrawRect.Left, vDrawRect.Top,
              vDrawRect.Left + vDItem.Width, Math.Min(vDrawRect.Bottom, ADataScreenBottom)));
          end
          else  // 处理一部分选中
          if vSelEndDNo >= 0 then  // 有选中内容，部分背景为选中
          begin
            ACanvas.Brush.Color := FStyle.SelColor;
            if (vSelStartDNo = vSelEndDNo) and (i = vSelStartDNo) then  // 选中内容都在当前DrawItem
            begin
              ACanvas.FillRect(Rect(vDrawRect.Left + GetDrawItemOffsetWidth(i, vSelStartDOffs),
                vDrawRect.Top,
                vDrawRect.Left + GetDrawItemOffsetWidth(i, vSelEndDOffs),
                Math.Min(vDrawRect.Bottom, ADataScreenBottom)));
            end
            else
            if i = vSelStartDNo then  // 选中在不同DrawItem，当前是起始
            begin
              ACanvas.FillRect(Rect(vDrawRect.Left + GetDrawItemOffsetWidth(i, vSelStartDOffs),
                vDrawRect.Top,
                vDrawRect.Right,
                Math.Min(vDrawRect.Bottom, ADataScreenBottom)));
            end
            else
            if i = vSelEndDNo then  // 选中在不同的DrawItem，当前是结束
            begin
              ACanvas.FillRect(Rect(vDrawRect.Left,
                vDrawRect.Top,
                vDrawRect.Left + GetDrawItemOffsetWidth(i, vSelEndDOffs),
                Math.Min(vDrawRect.Bottom, ADataScreenBottom)));
            end
            else
            if (i > vSelStartDNo) and (i < vSelEndDNo) then  // 选中起始和结束DrawItem之间的DrawItem
              ACanvas.FillRect(vDrawRect);
          end;
        end;

        // 除去行间距净Rect，即内容的显示区域
        InflateRect(vDrawRect, 0, -FStyle.ParaStyles[vItem.ParaNo].LineSpaceHalf);
        if tsSuperscript in FStyle.TextStyles[vPrioStyleNo].FontStyle then
          vDrawRect.Bottom := vDrawRect.Top + vTextHeight
        else
        if tsSubscript in FStyle.TextStyles[vPrioStyleNo].FontStyle then
          vDrawRect.Top := vDrawRect.Bottom - vTextHeight;

        vItem.PaintTo(FStyle, vDrawRect, ADataDrawTop, ADataDrawBottom,
          ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);  // 触发Item绘制事件

        // 绘制文本
        ACanvas.Brush.Style := bsClear;
        S := Copy(vItem.Text, vDItem.CharOffs, vDItem.CharLen);  // 为减少判断，没有直接使用GetDrawItemText(i)
        if S <> '' then
        begin
          case FStyle.ParaStyles[vItem.ParaNo].AlignVert of  // 垂直对齐方式
            pavCenter: vTextDrawTop := vDrawRect.Top + (vDrawRect.Bottom - vDrawRect.Top - vTextHeight) div 2;
            pavTop: vTextDrawTop := vDrawRect.Top;
          else
            vTextDrawTop := vDrawRect.Bottom - vTextHeight;
          end;

          case vAlignHorz of  // 水平对齐方式
            pahLeft, pahRight, pahCenter:  // 一般对齐
              begin
                {vLen := Length(S);
                SetLength(vCharWidths, vLen);
                if GetTextExtentExPoint(FStyle.DefCanvas.Handle, PChar(S), vLen,
                  vDrawRect.Right, @vFit, PInteger(vCharWidths), vSize)
                then
                begin
                  for j := vLen - 1 downto 1 do
                    Dec(vCharWidths[j], vCharWidths[j - 1]);
                  case vAlignVert of
                    DT_TOP: vDrawTop := vDrawRect.Top;
                    DT_CENTER: vDrawTop := vDrawRect.Top + (vDrawRect.Bottom - vDrawRect.Top - vTextHeight) div 2;
                  else
                    vDrawTop := vDrawRect.Bottom - vTextHeight;
                  end;
                  ExtTextOut(ACanvas.Handle, vDrawRect.Left, vDrawTop, ETO_CLIPPED, @vDrawRect, S, vLen, PInteger(vCharWidths));
                end
                else
                  Windows.DrawText(ACanvas.Handle, S, -1, vDrawRect, DT_LEFT or DT_SINGLELINE or vAlignVert);} // -1全部

                vLen := Length(S);

                Windows.ExtTextOut(ACanvas.Handle, vDrawRect.Left, vTextDrawTop,
                  ETO_OPAQUE, nil, PChar(S), vLen, nil);  // 参数说明见 201805161718
                //Windows.TextOut(ACanvas.Handle, vDrawRect.Left, vTextDrawTop, PChar(S), vLen);
              end;

            pahJustify, pahScatter:  // 分散、两端对齐
              DrawTextJsutify(vDrawRect, S, IsLineLastDrawItem(i));
          end;
        end
        else  // 空行
        begin
          if not vItem.ParaFirst then  // 不是空行
            raise Exception.Create(HCS_EXCEPTION_NULLTEXT);
        end;
      end;

      DrawItemPaintAfter(Self, i, vDrawRect, ADataDrawLeft, ADataDrawBottom,
        ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);  // 绘制内容后
    end;
  finally
    RestoreDC(ACanvas.Handle, vDCState);
    //ACanvas.Refresh;  为什么有这句，表格隐藏边框后某些单元格绘制边框不正确？
  end;
end;

procedure THCCustomData.FormatItemPrepare(const AStartItemNo: Integer;
  const AEndItemNo: Integer = -1);
var
  vFirstDrawItemNo, vLastDrawItemNo: Integer;
begin
  vFirstDrawItemNo := FItems[AStartItemNo].FirstDItemNo;
  if AEndItemNo < 0 then
    vLastDrawItemNo := GetItemLastDrawItemNo(AStartItemNo)
  else
    vLastDrawItemNo := GetItemLastDrawItemNo(AEndItemNo);
  FDrawItems.MarkFormatDelete(vFirstDrawItemNo, vLastDrawItemNo);
  FDrawItems.FormatBeforBottom := FDrawItems[vLastDrawItemNo].Rect.Bottom;
end;

procedure THCCustomData.SaveToStream(const AStream: TStream);
begin
  SaveToStream(AStream, 0, 0, Items.Count - 1, Items.Last.Length);
end;

procedure THCCustomData.SaveSelectToStream(const AStream: TStream);
begin
  if SelectExists then
  begin
    if (FSelectInfo.EndItemNo < 0)
      and (FItems[FSelectInfo.StartItemNo].StyleNo < THCStyle.RsNull)
    then  // 选择发生在同一个RectItem
    begin
      if FItems[FSelectInfo.StartItemNo].IsSelectComplate then  // 全选中了
      begin
        Self.SaveToStream(AStream, FSelectInfo.StartItemNo, OffsetBefor,
          FSelectInfo.StartItemNo, OffsetAfter);
      end
      else
        (FItems[FSelectInfo.StartItemNo] as THCCustomRectItem).SaveSelectToStream(AStream);
    end
    else
    begin
      Self.SaveToStream(AStream, FSelectInfo.StartItemNo, FSelectInfo.StartItemOffset,
        FSelectInfo.EndItemNo, FSelectInfo.EndItemOffset);
    end;
  end;
end;

function THCCustomData.SaveSelectToText: string;
begin
  Result := '';

  if SelectExists then
  begin
    if (FSelectInfo.EndItemNo < 0) and (FItems[FSelectInfo.StartItemNo].StyleNo < THCStyle.RsNull) then
      Result := (FItems[FSelectInfo.StartItemNo] as THCCustomRectItem).SaveSelectToText
    else
    begin
      Result := Self.SaveToText(FSelectInfo.StartItemNo, FSelectInfo.StartItemOffset,
        FSelectInfo.EndItemNo, FSelectInfo.EndItemOffset);
    end;
  end;
end;

procedure THCCustomData.SaveToStream(const AStream: TStream; const AStartItemNo,
  AStartOffset, AEndItemNo, AEndOffset: Integer);
var
  i: Integer;
  vBegPos, vEndPos: Int64;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 数据大小占位，便于越过
  //
  { if IsEmpty then i := 0 else 空Item也要存，CellData加载时高度可由此Item样式计算 }
  i := AEndItemNo - AStartItemNo + 1;
  AStream.WriteBuffer(i, SizeOf(i));
  if i > 0 then
  begin
    if AStartItemNo <> AEndItemNo then
    begin
      FItems[AStartItemNo].SaveToStream(AStream, AStartOffset, FItems[AStartItemNo].Length);
      for i := AStartItemNo + 1 to AEndItemNo - 1 do
        FItems[i].SaveToStream(AStream);
      FItems[AEndItemNo].SaveToStream(AStream, 0, AEndOffset);
    end
    else
      FItems[AStartItemNo].SaveToStream(AStream, AStartOffset, AEndOffset);
  end;
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 当前页数据大小
  AStream.Position := vEndPos;
end;

function THCCustomData.SaveToText(const AStartItemNo, AStartOffset, AEndItemNo,
  AEndOffset: Integer): string;
var
  i: Integer;
begin
  Result := '';
  i := AEndItemNo - AStartItemNo + 1;
  if i > 0 then
  begin
    if AStartItemNo <> AEndItemNo then
    begin
      if FItems[AStartItemNo].StyleNo > THCStyle.RsNull then
        Result := (FItems[AStartItemNo] as THCTextItem).GetTextPart(AStartOffset + 1, FItems[AStartItemNo].Length - AStartOffset)
      else
        Result := (FItems[AStartItemNo] as THCCustomRectItem).SaveSelectToText;

      for i := AStartItemNo + 1 to AEndItemNo - 1 do
        Result := Result + FItems[i].Text;

      if FItems[AEndItemNo].StyleNo > THCStyle.RsNull then
        Result := Result + (FItems[AEndItemNo] as THCTextItem).GetTextPart(1, AEndOffset)
      else
        Result := (FItems[AEndItemNo] as THCCustomRectItem).SaveSelectToText;
    end
    else  // 选中在同一Item
    begin
      if FItems[AStartItemNo].StyleNo > THCStyle.RsNull then
        Result := (FItems[AStartItemNo] as THCTextItem).GetTextPart(AStartOffset + 1, AEndOffset - AStartOffset);
    end;
  end;
end;

function THCCustomData.SaveToText: string;
begin
  SaveToText(0, 0, Items.Count - 1, Items.Last.Length);
end;

procedure THCCustomData.SelectAll;
begin
  if FItems.Count > 0 then
  begin
    FSelectInfo.StartItemNo := 0;
    FSelectInfo.StartItemOffset := 0;
    if not IsEmptyData then
    begin
      FSelectInfo.EndItemNo := FItems.Count - 1;
      if FItems[FSelectInfo.EndItemNo].StyleNo < THCStyle.RsNull then
        FSelectInfo.EndItemOffset := OffsetAfter
      else
        FSelectInfo.EndItemOffset := FItems.Last.Length;
    end
    else
    begin
      FSelectInfo.EndItemNo := -1;
      FSelectInfo.EndItemOffset := -1;
    end;

    MatchItemSelectState;
  end;
end;

function THCCustomData.SelectedCanDrag: Boolean;
var
  i: Integer;
begin
  Result := True;
  if FSelectInfo.EndItemNo < 0 then
  begin
    if FSelectInfo.StartItemNo >= 0 then
      Result := FItems[FSelectInfo.StartItemNo].CanDrag;
  end
  else
  begin
    for i := FSelectInfo.StartItemNo to FSelectInfo.EndItemNo do
    begin
      if FItems[i].StyleNo < THCStyle.RsNull then
      begin
        if not FItems[i].IsSelectComplate then
        begin
          Result := False;
          Break;
        end;
      end;

      if not FItems[i].CanDrag then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function THCCustomData.SelectedResizing: Boolean;
begin
  Result := False;
  if (FSelectInfo.StartItemNo >= 0)
    and (FSelectInfo.EndItemNo < 0)
    and (FItems[FSelectInfo.StartItemNo] is THCResizeRectItem)
  then
    Result := (FItems[FSelectInfo.StartItemNo] as THCResizeRectItem).Resizing;
end;

function THCCustomData.SelectedAll: Boolean;
begin
  Result := (FSelectInfo.StartItemNo = 0)
    and (FSelectInfo.StartItemOffset = 0)
    and (FSelectInfo.EndItemNo = FItems.Count - 1);
  if Result then
  begin
    if FItems[FSelectInfo.EndItemNo].StyleNo < THCStyle.RsNull then
      Result := FSelectInfo.EndItemOffset = OffsetAfter
    else
      Result := FSelectInfo.EndItemOffset = FItems.Last.Length;
  end;
end;

function THCCustomData.SelectExists(const AIfRectItem: Boolean = True): Boolean;
begin
  Result := False;
  if FSelectInfo.StartItemNo >= 0 then
  begin
    if FSelectInfo.EndItemNo >= 0 then
    begin
      if FSelectInfo.StartItemNo <> FSelectInfo.EndItemNo then  // 选择在不同的Item
        Result := True
      else  // 在同一Item
        Result := FSelectInfo.StartItemOffset <> FSelectInfo.EndItemOffset;  // 同一Item不同位置
    end
    else  // 当前光标仅在一个Item中(在Rect中即使有选中，相对当前层的Data也算在一个Item)
    begin
      if AIfRectItem and (FItems[FSelectInfo.StartItemNo].StyleNo < THCStyle.RsNull) then
      begin
        //if FSelectInfo.StartItemOffset = OffsetInner then  表格整体选中时不成立
          Result := (FItems[FSelectInfo.StartItemNo] as THCCustomRectItem).SelectExists;
      end;
    end;
  end;
end;

function THCCustomData.SelectInSameDItem: Boolean;
var
  vStartDNo: Integer;
begin
  vStartDNo := GetSelectStartDrawItemNo;
  if vStartDNo < 0 then
    Result := False
  else
  begin
    if GetDrawItemStyle(vStartDNo) < THCStyle.RsNull then
      Result := FItems[FDrawItems[vStartDNo].ItemNo].IsSelectComplate and (FSelectInfo.EndItemNo < 0)
    else
      Result := vStartDNo = GetSelectEndDrawItemNo;
  end;
end;

procedure THCCustomData.SetCaretDrawItemNo(const Value: Integer);
var
  vItemNo: Integer;
begin
  if FCaretDrawItemNo <> Value then
  begin
    if (FCaretDrawItemNo >= 0) and (FCaretDrawItemNo < FDrawItems.Count) then
    begin
      vItemNo := FDrawItems[FCaretDrawItemNo].ItemNo;
      FItems[vItemNo].Active := False;
    end
    else
      vItemNo := -1;

    FCaretDrawItemNo := Value;

    if (FCaretDrawItemNo >= 0) and (FDrawItems[FCaretDrawItemNo].ItemNo <> vItemNo) then
    begin
      if FItems[FDrawItems[FCaretDrawItemNo].ItemNo].StyleNo < THCStyle.RsNull then
      begin
        if FSelectInfo.StartItemOffset = OffsetInner then
          FItems[FDrawItems[FCaretDrawItemNo].ItemNo].Active := True
      end
      else
        FItems[FDrawItems[FCaretDrawItemNo].ItemNo].Active := True;
    end;
  end;
end;

procedure THCCustomData.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: TCaretInfo);
var
  vDrawItemNo, vStyleItemNo: Integer;
  vDrawItem: THCCustomDrawItem;
begin
  { 注意：为处理RectItem往外迭代，这里位置处理为叠加，而不是直接赋值 }
  if FCaretDrawItemNo < 0 then
  begin
    if FItems[AItemNo].StyleNo < THCStyle.RsNull then  // RectItem
      vDrawItemNo := FItems[AItemNo].FirstDItemNo
    else
      vDrawItemNo := GetDrawItemNoByOffset(AItemNo, AOffset);  // AOffset处对应的DrawItemNo
  end
  else
    vDrawItemNo := FCaretDrawItemNo;

  vDrawItem := FDrawItems[vDrawItemNo];
  ACaretInfo.Height := vDrawItem.Height;  // 光标高度

  if FStyle.UpdateInfo.ReStyle then  // 以光标前样式为当前样式
  begin
    vStyleItemNo := AItemNo;
    if AOffset = 0 then  // 在最前面
    begin
      if (not FItems[AItemNo].ParaFirst)
        and (AItemNo > 0)
        and (Items[AItemNo - 1].StyleNo > THCStyle.RsNull)
      then  // 前一个是TextItem
        vStyleItemNo := AItemNo - 1;
    end;

    FStyle.CurStyleNo := Items[vStyleItemNo].StyleNo;
    FStyle.CurParaNo := Items[vStyleItemNo].ParaNo;
  end;

  if FItems[AItemNo].StyleNo < THCStyle.RsNull then  // RectItem
  begin
    if AOffset = OffsetBefor then  // 在其左侧
      ACaretInfo.X := ACaretInfo.X + vDrawItem.Rect.Left
    else
    if AOffset = OffsetInner then  // 正在其上，由内部决定
    begin
      (FItems[AItemNo] as THCCustomRectItem).GetCaretInfo(ACaretInfo);
      ACaretInfo.X := ACaretInfo.X + vDrawItem.Rect.Left;
      case FStyle.ParaStyles[FItems[AItemNo].ParaNo].AlignVert of  // 垂直对齐方式
        pavCenter: ACaretInfo.Y := ACaretInfo.Y + (vDrawItem.Rect.Height - (FItems[AItemNo] as THCCustomRectItem).Height) div 2;

        pavTop: ACaretInfo.Y := ACaretInfo.Y + FStyle.ParaStyles[FItems[AItemNo].ParaNo].LineSpaceHalf;
      else
        ACaretInfo.Y := ACaretInfo.Y + vDrawItem.Rect.Height - (FItems[AItemNo] as THCCustomRectItem).Height;
      end;
    end
    else  // 在其右侧
      ACaretInfo.X := ACaretInfo.X + vDrawItem.Rect.Right;
  end
  else  // TextItem
    ACaretInfo.X := ACaretInfo.X + vDrawItem.Rect.Left
      + GetDrawItemOffsetWidth(vDrawItemNo, AOffset - vDrawItem.CharOffs + 1);

  ACaretInfo.Y := ACaretInfo.Y + vDrawItem.Rect.Top;
end;

procedure THCCustomData.InitializeField;
begin
  if FCaretDrawItemNo >= 0 then
    FItems[FDrawItems[FCaretDrawItemNo].ItemNo].Active := False;

  FCaretDrawItemNo := -1;
end;

function THCCustomData.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
begin
end;

{ TSelectInfo }

constructor TSelectInfo.Create;
begin
  Self.Initialize;
end;

procedure TSelectInfo.Initialize;
begin
  FStartItemNo := -1;
  FStartItemOffset := -1;
  FEndItemNo := -1;
  FEndItemOffset := -1;
end;

end.
