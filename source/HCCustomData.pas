{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                 文档对象基本管理单元                  }
{                                                       }
{*******************************************************}

unit HCCustomData;

interface

{$I HCView.inc}

uses
  Windows, Classes, Types, Controls, Graphics, Generics.Collections, HCItem,
  HCDrawItem, HCStyle, HCParaStyle, HCTextStyle, HCStyleMatch, HCCommon, HCUndo,
  HCXml, HCList;

type
  TSelectInfo = class(TObject)
  strict private
    FStartItemNo,  // 不能使用DrawItem记录，因为内容变动时Item的指定Offset对应的DrawItem，可能和变动前不一样
    FStartItemOffset,  // 选中起始在第几个字符后面，0表示在Item最前面
    FEndItemNo,
    FEndItemOffset  // 选中结束在第几个字符后面
      : Integer;
    FStartRestrain: Boolean;
  public
    constructor Create; virtual;
    procedure Initialize; virtual;

    /// <summary> 选中起始Item序号 </summary>
    property StartItemNo: Integer read FStartItemNo write FStartItemNo;
    property StartItemOffset: Integer read FStartItemOffset write FStartItemOffset;
    property StartRestrain: Boolean read FStartRestrain write FStartRestrain;
    /// <summary> 选中结束Item序号 </summary>
    property EndItemNo: Integer read FEndItemNo write FEndItemNo;
    property EndItemOffset: Integer read FEndItemOffset write FEndItemOffset;
  end;

  THCCustomData = class;

  THCDomainInfo = class(TObject)
  strict private
    FBeginNo, FEndNo: Integer;
  public
    Data: THCCustomData;
    constructor Create;
    procedure Clear;
    /// <summary> 域中是否包含此Item(头、尾也算) </summary>
    function Contain(const AItemNo: Integer): Boolean;
    property BeginNo: Integer read FBeginNo write FBeginNo;
    property EndNo: Integer read FEndNo write FEndNo;
  end;

  TDomainStack = class(TObjectStack<THCDomainInfo>);

  TDataDomainItemNoEvent = procedure(const AData: THCCustomData; const ADomainStack: TDomainStack; const AItemNo: Integer) of object;
  TDataItemEvent = procedure(const AData: THCCustomData; const AItem: THCCustomItem) of object;
  TDataItemNoEvent = procedure(const AData: THCCustomData; const AItemNo: Integer) of object;
  TDataItemNoFunEvent = function(const AData: THCCustomData; const AItemNo: Integer): Boolean of object;
  TDataActionEvent = function(const AData: THCCustomData; const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean of object;

  TDrawItemPaintEvent = procedure(const AData: THCCustomData;
    const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
    ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
    const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;

  TDrawItemPaintContentEvent = procedure(const AData: THCCustomData;
    const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADrawText: string;
    const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
    const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;

  THCCustomData = class(TObject)
  private
    FParentData: THCCustomData;  // 便于判断父Data是否只读以便约束
    FStyle: THCStyle;
    FCurStyleNo, FCurParaNo: Integer;
    FItems: THCItems;
    FDrawItems: THCDrawItems;
    FSelectInfo: TSelectInfo;
    FDrawOptions: TDrawOptions;
    FLoading: Boolean;  // 当粘贴时调用InsertStream中有表格时，全局并不是loading但对于单元格来说是loading所以需要单独表示操作状态，如果将来需要更多状态可以考虑使用THCStates
    FCaretDrawItemNo: Integer;  // 当前Item光标处的DrawItem限定其只在相关的光标处理中使用(解决同一Item分行后Offset为行尾时不能区分是上行尾还是下行始)

    FOnInsertItem, FOnRemoveItem: TDataItemEvent;
    FOnSaveItem: TDataItemNoFunEvent;  // 可控制保存时不保存指定的Item实现一次性的功能
    FOnGetUndoList: TGetUndoListEvent;
    FOnCurParaNoChange: TNotifyEvent;
    FOnDrawItemPaintBefor, FOnDrawItemPaintAfter: TDrawItemPaintEvent;
    FOnDrawItemPaintContent: TDrawItemPaintContentEvent;

    procedure DrawItemPaintBefor(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DrawItemPaintAfter(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DrawItemPaintContent(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADrawText: string; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure SetCurStyleNo(const Value: Integer);
    procedure SetCurParaNo(const Value: Integer);
  protected
    /// <summary> 合并2个文本Item </summary>
    /// <param name="ADestItem">合并后的Item</param>
    /// <param name="ASrcItem">源Item</param>
    /// <returns>True:合并成功，False不能合并</returns>
    function MergeItemText(const ADestItem, ASrcItem: THCCustomItem): Boolean;

    /// <summary> Item成功合并到同段前一个Item </summary>
    function MergeItemToPrio(const AItemNo: Integer): Boolean;

    /// <summary> Item成功合并到同段后一个Item </summary>
    function MergeItemToNext(const AItemNo: Integer): Boolean;

    function CalcContentHeight: Integer;

    /// <summary> 处理选中范围内Item的全选中、部分选中状态 </summary>
    procedure MatchItemSelectState;

    /// <summary> 根据指定Item获取其所在段的起始和结束ItemNo </summary>
    /// <param name="AFirstItemNo1">指定</param>
    /// <param name="AFirstItemNo">起始</param>
    /// <param name="ALastItemNo">结束</param>
    procedure GetParaItemRang(const AItemNo: Integer; var AFirstItemNo, ALastItemNo: Integer);
    function GetParaFirstItemNo(const AItemNo: Integer): Integer;
    function GetParaLastItemNo(const AItemNo: Integer): Integer;

    /// <summary> 取行第一个DrawItem对应的ItemNo(用于格式化时计算一个较小的ItemNo范围) </summary>
    function GetLineFirstDrawItemNo(const AItemNo, AOffset: Integer): Integer;

    /// <summary> 取行最后一个DrawItem对应的ItemNo(用于格式化时计算一个较小的ItemNo范围) </summary>
    function GetLineLastItemNo(const AItemNo, AOffset: Integer): Integer;

    /// <summary> 根据指定Item获取其所在行的起始和结束DrawItemNo </summary>
    /// <param name="AFirstItemNo1">指定</param>
    /// <param name="AFirstItemNo">起始</param>
    /// <param name="ALastItemNo">结束</param>
    procedure GetLineDrawItemRang(var AFirstDrawItemNo, ALastDrawItemNo: Integer); virtual;

    /// <summary> 返回字符串AText的分散分隔数量和各分隔的起始位置 </summary>
    /// <param name="AText">要计算的字符串</param>
    /// <param name="ACharIndexs">记录各分隔的起始位置</param>
    /// <returns>分散分隔数量</returns>
    function GetJustifyCount(const AText: string; const ACharIndexs: THCIntegerList): Integer;

    procedure SetCaretDrawItemNo(const Value: Integer);

    /// <summary> 计算行高(文本高+行间距) </summary>
    function CalculateLineHeight(const ATextStyle: THCTextStyle; const AParaStyle: THCParaStyle): Integer;
    function GetUndoList: THCUndoList; virtual;
    /// <summary> 是否允许保存该Item </summary>
    function DoSaveItem(const AItemNo: Integer): Boolean; virtual;
    procedure DoInsertItem(const AItem: THCCustomItem); virtual;
    procedure DoRemoveItem(const AItem: THCCustomItem); virtual;
    procedure DoItemAction(const AItemNo, AOffset: Integer; const AAction: THCAction); virtual;
    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    procedure DoDrawItemPaintContent(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADrawText: string; const ADataDrawLeft, ADataDrawRight,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer; const ACanvas: TCanvas;
      const APaintInfo: TPaintInfo); virtual;
    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); virtual;
    procedure DoCaretItemChanged; virtual;

    property Loading: Boolean read FLoading;
  public
    constructor Create(const AStyle: THCStyle); virtual;
    destructor Destroy; override;

    /// <summary> 在Data层面是否可编辑 </summary>
    function CanEdit: Boolean; virtual;

    /// <summary> 全选 </summary>
    procedure SelectAll; virtual;

    /// <summary> 当前内容是否全选中了 </summary>
    function SelectedAll: Boolean; virtual;
    procedure Clear; virtual;
    procedure InitializeField; virtual;

    /// <summary> 嵌套时获取根级Data </summary>
    function GetRootData: THCCustomData; virtual;

    /// <summary> 将指定的坐标转换为屏幕坐标 </summary>
    function GetScreenCoord(const X, Y: Integer): TPoint; virtual;
    function CreateDefaultTextItem: THCCustomItem; virtual;
    function CreateDefaultDomainItem: THCCustomItem; virtual;
    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; virtual;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: THCCaretInfo); virtual;

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

    /// <summary> 坐标是否在AItem的选中区域中 </summary>
    /// <param name="X"></param>
    /// <param name="Y"></param>
    /// <param name="AItemNo">X、Y处的Item</param>
    /// <param name="AOffset">X、Y处的Item偏移(供在RectItem上时计算)</param>
    /// <param name="ARestrain">AItemNo, AOffset是X、Y位置约束后的(此参数为方便单元格Data处理)</param>
    function CoordInSelect(const X, Y, AItemNo, AOffset: Integer;
      const ARestrain: Boolean): Boolean; virtual;

    /// <summary> 获取指定DrawItem对应的Text </summary>
    /// <param name="ADrawItemNo"></param>
    /// <returns></returns>
    function GetDrawItemText(const ADrawItemNo: Integer): string;

    /// <summary> 获取DItem中指定偏移处的内容绘制宽度 </summary>
    /// <param name="ADrawItemNo"></param>
    /// <param name="ADrawOffs">相对与DItem的CharOffs的Offs</param>
    /// <returns></returns>
    function GetDrawItemOffsetWidth(const ADrawItemNo, ADrawOffs: Integer;
      const AStyleCanvas: TCanvas = nil): Integer;

    {$IFDEF UNPLACEHOLDERCHAR}
    /// <summary> 返回文本字符串指定位置实际有效的前、后位置 </summary>
    /// <param name="AAfter = False">True:前，False:后</param>
    function GetItemActualOffset(const AItemNo, AOffset: Integer; const AAfter: Boolean = False): Integer;
    {$ENDIF}

    /// <summary> 获取指定的Item最后面位置 </summary>
    /// <param name="AItemNo">指定的Item</param>
    /// <returns>最后面位置</returns>
    function GetItemOffsetAfter(const AItemNo: Integer): Integer;

    /// <summary>
    /// 根据给定的位置获取在此范围内的起始和结束DItem
    /// </summary>
    /// <param name="ATop"></param>
    /// <param name="ABottom"></param>
    /// <param name="AFristDItemNo"></param>
    /// <param name="ALastDItemNo"></param>
    procedure GetDataDrawItemRang(const ATop, ABottom: Integer;
      var AFirstDItemNo, ALastDItemNo: Integer);

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

    /// <summary> 获取Data中的坐标X、Y处的Item和Offset，并返回X、Y相对DrawItem的坐标 </summary>
    /// <param name="X"></param>
    /// <param name="Y"></param>
    /// <param name="AItemNo"></param>
    /// <param name="AOffset"></param>
    /// <param name="AX"></param>
    /// <param name="AY"></param>
    procedure CoordToItemOffset(const X, Y, AItemNo, AOffset: Integer; var AX, AY: Integer);

    /// <summary> 返回Item中指定Offset处的DrawItem序号，如正是换行位置，返回的是下一行DrawItem </summary>
    /// <param name="AItemNo">指定Item</param>
    /// <param name="AOffset">Item中指定Offset</param>
    /// <returns>Offset处的DrawItem序号</returns>
    function GetDrawItemNoByOffset(const AItemNo, AOffset: Integer): Integer;
    function IsLineLastDrawItem(const ADrawItemNo: Integer): Boolean;
    function IsParaLastDrawItem(const ADrawItemNo: Integer): Boolean;
    function IsParaLastItem(const AItemNo: Integer): Boolean;

    /// <summary> 返回当前光标处的顶层Data </summary>
    function GetTopLevelData: THCCustomData;

    /// <summary> 返回指定位置处的顶层Data </summary>
    function GetTopLevelDataAt(const X, Y: Integer): THCCustomData;
    function GetActiveDrawItemNo: Integer;
    function GetActiveDrawItem: THCCustomDrawItem;
    function GetActiveItemNo: Integer; virtual;
    function GetActiveItem: THCCustomItem; virtual;
    function GetTopLevelItem: THCCustomItem;
    function GetTopLevelDrawItem: THCCustomDrawItem;
    function GetTopLevelDrawItemCoord: TPoint;
    function GetTopLevelRectDrawItem: THCCustomDrawItem;
    function GetTopLevelRectDrawItemCoord: TPoint;

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
    function GetDrawItemOffsetAt(const ADrawItemNo, X: Integer): Integer;

    { 获取选中相关信息 }
    /// <summary> 当前选中起始DItemNo </summary>
    /// <returns></returns>
    function GetSelectStartDrawItemNo: Integer;

    /// <summary> 当前选中结束DItemNo </summary>
    /// <returns></returns>
    function GetSelectEndDrawItemNo: Integer;

    /// <summary> 获取选中内容是否在同一个DrawItem中 </summary>
    /// <returns></returns>
    function SelectInSameDrawItem: Boolean;

    /// <summary> 取消选中 </summary>
    /// <returns>取消时当前是否有选中，True：有选中；False：无选中</returns>
    function DisSelect: Boolean; virtual;

    /// <summary> 当前选中内容允许拖动 </summary>
    /// <returns></returns>
    function SelectedCanDrag: Boolean;

    /// <summary> 当前选中内容只有RectItem且正处于缩放状态 </summary>
    /// <returns></returns>
    function SelectedResizing: Boolean;

    /// <summary> 当前Data是不是无内容(仅有一个Item且内容为空) </summary>
    /// <returns></returns>
    function IsEmptyData: Boolean;

    /// <summary> 当前Item是不是空行 </summary>
    function IsEmptyLine(const AItemNo: Integer): Boolean;

    /// <summary> 为段应用对齐方式 </summary>
    /// <param name="AAlign">对方方式</param>
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz); virtual;
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert); virtual;
    procedure ApplyParaBackColor(const AColor: TColor); virtual;
    procedure ApplyParaBreakRough(const ARough: Boolean); virtual;
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode; const ASpace: Single); virtual;
    procedure ApplyParaLeftIndent(const AIndent: Single); virtual;
    procedure ApplyParaRightIndent(const AIndent: Single); virtual;
    procedure ApplyParaFirstIndent(const AIndent: Single); virtual;

    // 选中内容应用样式
    procedure ApplySelectTextStyle(const AMatchStyle: THCStyleMatch); virtual;
    procedure ApplySelectParaStyle(const AMatchStyle: THCParaMatch); virtual;
    procedure ApplyTableCellAlign(const AAlign: THCContentAlign); virtual;

    /// <summary> 删除选中 </summary>
    function DeleteSelected: Boolean; virtual;

    /// <summary> 为选中文本使用指定的文本样式 </summary>
    /// <param name="AFontStyle">文本样式</param>
    procedure ApplyTextStyle(const AFontStyle: THCFontStyle); virtual;
    procedure ApplyTextFontName(const AFontName: TFontName); virtual;
    procedure ApplyTextFontSize(const AFontSize: Single); virtual;
    procedure ApplyTextColor(const AColor: TColor); virtual;
    procedure ApplyTextBackColor(const AColor: TColor); virtual;

    /// <summary> 绘制数据 </summary>
    /// <param name="ADataDrawLeft">绘制目标区域Left</param>
    /// <param name="ADataDrawTop">绘制目标区域的Top</param>
    /// <param name="ADataDrawBottom">绘制目标区域的Bottom</param>
    /// <param name="ADataScreenTop">屏幕区域Top</param>
    /// <param name="ADataScreenBottom">屏幕区域Bottom</param>
    /// <param name="AVOffset">指定从哪个位置开始的数据绘制到目标区域的起始位置</param>
    /// <param name="AFristDItemNo">指定从哪个DrawItem开始绘制</param>
    /// <param name="ALastDItemNo">指定绘制到哪个DrawItem结束</param>
    /// <param name="ACanvas">画布</param>
    procedure PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawRight, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset, AFristDItemNo, ALastDItemNo: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); overload; virtual;

    /// <summary> 绘制数据 </summary>
    /// <param name="ADataDrawLeft">绘制目标区域Left</param>
    /// <param name="ADataDrawTop">绘制目标区域的Top</param>
    /// <param name="ADataDrawBottom">绘制目标区域的Bottom</param>
    /// <param name="ADataScreenTop">屏幕区域Top</param>
    /// <param name="ADataScreenBottom">屏幕区域Bottom</param>
    /// <param name="AVOffset">指定从哪个位置开始的数据绘制到目标区域的起始位置</param>
    /// <param name="ACanvas">画布</param>
    procedure PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawRight, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); overload; virtual;

    /// <summary> 根据行中某DrawItem获取当前行间距(行中除文本外的空白空间) </summary>
    /// <param name="ADrawNo">行中指定的DrawItem</param>
    /// <returns>行间距</returns>
    function GetLineBlankSpace(const ADrawNo: Integer): Integer;

    /// <summary> 获取指定DrawItem的行间距 </summary>
    /// <param name="ADrawNo">指定的DrawItem</param>
    /// <returns>DrawItem的行间距</returns>
    function GetDrawItemLineSpace(const ADrawNo: Integer): Integer;

    /// <summary> 是否有选中 </summary>
    function SelectExists(const AIfRectItem: Boolean = True): Boolean;
    procedure MarkStyleUsed(const AMark: Boolean);

    procedure SaveItemToStream(const AStream: TStream; const AStartItemNo, AStartOffset,
      AEndItemNo, AEndOffset: Integer);
    procedure SaveToStream(const AStream: TStream); overload; virtual;
    procedure SaveToStream(const AStream: TStream; const AStartItemNo, AStartOffset,
      AEndItemNo, AEndOffset: Integer); overload; virtual;

    function SaveToText: string; overload;
    function SaveToText(const AStartItemNo, AStartOffset,
      AEndItemNo, AEndOffset: Integer): string; overload;

    /// <summary> 保存选中内容到流 </summary>
    procedure SaveSelectToStream(const AStream: TStream); virtual;
    function SaveSelectToText: string;
    function GetSelectText: string;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; virtual;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word);

    function ToHtml(const APath: string): string;
    procedure ToXml(const ANode: IHCXMLNode); virtual;
    procedure ParseXml(const ANode: IHCXMLNode); virtual;
    //
    property ParentData: THCCustomData read FParentData write FParentData;
    property Style: THCStyle read FStyle;
    property CurStyleNo: Integer read FCurStyleNo write SetCurStyleNo;
    property CurParaNo: Integer read FCurParaNo write SetCurParaNo;
    property Items: THCItems read FItems;
    property DrawItems: THCDrawItems read FDrawItems;
    property SelectInfo: TSelectInfo read FSelectInfo;
    property DrawOptions: TDrawOptions read FDrawOptions write FDrawOptions;
    property CaretDrawItemNo: Integer read FCaretDrawItemNo write SetCaretDrawItemNo;
    property OnGetUndoList: TGetUndoListEvent read FOnGetUndoList write FOnGetUndoList;
    property OnCurParaNoChange: TNotifyEvent read FOnCurParaNoChange write FOnCurParaNoChange;
    property OnDrawItemPaintBefor: TDrawItemPaintEvent read FOnDrawItemPaintBefor write FOnDrawItemPaintBefor;
    property OnDrawItemPaintAfter: TDrawItemPaintEvent read FOnDrawItemPaintAfter write FOnDrawItemPaintAfter;
    property OnDrawItemPaintContent: TDrawItemPaintContentEvent read FOnDrawItemPaintContent write FOnDrawItemPaintContent;
    property OnInsertItem: TDataItemEvent read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TDataItemEvent read FOnRemoveItem write FOnRemoveItem;
    property OnSaveItem: TDataItemNoFunEvent read FOnSaveItem write FOnSaveItem;
  end;

  TTraverseItemEvent = reference to procedure(const AData: THCCustomData;
    const AItemNo, ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean);

  THCItemTraverse = class(TObject)
  public
    Areas: TSectionAreas;
    Tag: Integer;
    Stop: Boolean;
    Process: TTraverseItemEvent;
    DomainStack: TDomainStack;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{$I HCView.inc}

uses
  SysUtils, Math, HCTextItem, HCRectItem, HCUnitConversion;

{ THCCustomData }

procedure THCCustomData.ApplyTableCellAlign(const AAlign: THCContentAlign);
begin
  // 因为要处理撤销恢复，所以不在这里实现
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

procedure THCCustomData.ApplyTextFontSize(const AFontSize: Single);
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

procedure THCCustomData.ApplyTextStyle(const AFontStyle: THCFontStyle);
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

procedure THCCustomData.ApplyParaBreakRough(const ARough: Boolean);
var
  vMatchStyle: TParaBreakRoughMatch;
begin
  vMatchStyle := TParaBreakRoughMatch.Create;
  try
    vMatchStyle.BreakRough := ARough;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyParaFirstIndent(const AIndent: Single);
var
  vMatchStyle: TParaFirstIndentMatch;
begin
  vMatchStyle := TParaFirstIndentMatch.Create;
  try
    vMatchStyle.Indent := AIndent;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyParaLeftIndent(const AIndent: Single);
var
  vMatchStyle: TParaLeftIndentMatch;
begin
  vMatchStyle := TParaLeftIndentMatch.Create;
  try
    vMatchStyle.Indent := AIndent;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode;
  const ASpace: Single);
var
  vMatchStyle: TParaLineSpaceMatch;
begin
  vMatchStyle := TParaLineSpaceMatch.Create;
  try
    vMatchStyle.SpaceMode := ASpaceMode;
    vMatchStyle.Space := ASpace;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplyParaRightIndent(const AIndent: Single);
var
  vMatchStyle: TParaRightIndentMatch;
begin
  vMatchStyle := TParaRightIndentMatch.Create;
  try
    vMatchStyle.Indent := AIndent;
    ApplySelectParaStyle(vMatchStyle);
  finally
    vMatchStyle.Free;
  end;
end;

procedure THCCustomData.ApplySelectParaStyle(const AMatchStyle: THCParaMatch);
begin
  // 因为要处理撤销恢复，所以不在这里实现
end;

procedure THCCustomData.ApplySelectTextStyle(const AMatchStyle: THCStyleMatch);
begin
  // 因为要处理撤销恢复，所以不在这里实现
end;

procedure THCCustomData.Clear;
begin
  //DisSelect;  用不着DisSelect吧
  FSelectInfo.Initialize;
  FCaretDrawItemNo := -1;
  FDrawItems.Clear;
  FItems.Clear;
  FCurStyleNo := 0;
  FCurParaNo := 0;
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
    if FItems[AItemNo].StyleNo < THCStyle.Null then
    begin
      vX := X - vDrawRect.Left;
      vY := Y - vDrawRect.Top - GetLineBlankSpace(vDrawItemNo) div 2;

      Result := (FItems[AItemNo] as THCCustomRectItem).CoordInSelect(vX, vY);
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

  InflateRect(vDrawRect, 0, -GetLineBlankSpace(vDrawItemNo) div 2);

  AX := AX - vDrawRect.Left;
  AY := AY - vDrawRect.Top;
  if FItems[AItemNo].StyleNo < THCStyle.Null then
  begin
    case FStyle.ParaStyles[FItems[AItemNo].ParaNo].AlignVert of  // 垂直对齐方式
      pavCenter: AY := AY - (vDrawRect.Height - (FItems[AItemNo] as THCCustomRectItem).Height) div 2;

      pavTop: ;
    else
      AY := AY - (vDrawRect.Height - (FItems[AItemNo] as THCCustomRectItem).Height);
    end;
  end;
end;

constructor THCCustomData.Create(const AStyle: THCStyle);
begin
  FParentData := nil;
  FStyle := AStyle;
  FDrawItems := THCDrawItems.Create;
  FItems := THCItems.Create;
  FItems.OnInsertItem := DoInsertItem;
  FItems.OnRemoveItem := DoRemoveItem;

  FLoading := False;
  FCurStyleNo := 0;
  FCurParaNo := 0;
  FCaretDrawItemNo := -1;
  FSelectInfo := TSelectInfo.Create;
end;

function THCCustomData.CreateDefaultDomainItem: THCCustomItem;
begin
  Result := HCDefaultDomainItemClass.Create(Self);
  Result.ParaNo := FCurParaNo;
end;

function THCCustomData.CreateDefaultTextItem: THCCustomItem;
begin
  Result := HCDefaultTextItemClass.CreateByText('');  // 必需有参数否则不能调用属性创建
  if FCurStyleNo < THCStyle.Null then
    Result.StyleNo := FStyle.GetStyleNo(FStyle.DefaultTextStyle, True)
  else
    Result.StyleNo := FCurStyleNo;

  Result.ParaNo := FCurParaNo;
end;

function THCCustomData.CreateItemByStyle(const AStyleNo: Integer): THCCustomItem;
begin
  Result := nil;
end;

function THCCustomData.DeleteSelected: Boolean;
begin
  Result := False;
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
    //vItem.Active := False;  // 方向键移动到EditItem里激活，鼠标再点EditItem时不应该取消激活后面再激活
  end;

  SelectInfo.StartItemNo := -1;
  SelectInfo.StartItemOffset := -1;
end;

procedure THCCustomData.DoCaretItemChanged;
begin
end;

procedure THCCustomData.DoDrawItemPaintAfter(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintAfter) then
  begin
    FOnDrawItemPaintAfter(AData, AItemNo, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintBefor) then
  begin
    FOnDrawItemPaintBefor(AData, AItemNo, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomData.DoDrawItemPaintContent(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintContent) then
  begin
    FOnDrawItemPaintContent(AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADrawText,
      ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomData.DoInsertItem(const AItem: THCCustomItem);
begin
  //if AItem is THCTextItem then
  //  (AItem as THCTextItem).OwnerData := Self;

  if Assigned(FOnInsertItem) then
    FOnInsertItem(Self, AItem);
end;

procedure THCCustomData.DoItemAction(const AItemNo, AOffset: Integer;
  const AAction: THCAction);
begin
end;

procedure THCCustomData.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  Clear;
end;

procedure THCCustomData.DoRemoveItem(const AItem: THCCustomItem);
begin
  if Assigned(FOnRemoveItem) and (not FStyle.States.Contain(THCState.hosDestroying)) then
    FOnRemoveItem(Self, AItem);
end;

function THCCustomData.DoSaveItem(const AItemNo: Integer): Boolean;
begin
  if Assigned(FOnSaveItem) then
    Result := FOnSaveItem(Self, AItemNo)
  else
    Result := True;
end;

procedure THCCustomData.DrawItemPaintAfter(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDCState: THCCanvas;
begin
  vDCState := SaveCanvas(ACanvas);
  try
    DoDrawItemPaintAfter(AData, AItemNo, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  finally
    vDCState.ToCanvas(ACanvas);
    FreeAndNil(vDCState);
  end;
end;

procedure THCCustomData.DrawItemPaintBefor(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDCState: THCCanvas;
begin
  vDCState := SaveCanvas(ACanvas);
  try
    DoDrawItemPaintBefor(AData, AItemNo, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  finally
    vDCState.ToCanvas(ACanvas);
    FreeAndNil(vDCState);
  end;
end;

procedure THCCustomData.DrawItemPaintContent(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDCState: THCCanvas;
begin
  vDCState := SaveCanvas(ACanvas);
  try
    DoDrawItemPaintContent(AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADrawText,
      ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  finally
    vDCState.ToCanvas(ACanvas);
    FreeAndNil(vDCState);
  end;
end;

function THCCustomData.IsEmptyData: Boolean;
begin
  Result := (FItems.Count = 1) and IsEmptyLine(0);
end;

function THCCustomData.IsEmptyLine(const AItemNo: Integer): Boolean;
begin
  Result := (FItems[AItemNo].StyleNo > THCStyle.Null) and (Items[AItemNo].Text = '');
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
    end;
  end;
  if ALastDItemNo < 0 then  // 高度超过Data高度时，以最后1个结束
    ALastDItemNo := FDrawItems.Count - 1;
end;

function THCCustomData.GetDrawItemLineSpace(const ADrawNo: Integer): Integer;
//var
//  vCanvas: TCanvas;
begin
  Result := FStyle.LineSpaceMin;

  if GetDrawItemStyle(ADrawNo) >= THCStyle.Null then
  begin
    //vCanvas := THCStyle.CreateStyleCanvas;
    //try
      Result := CalculateLineHeight(FStyle.TextStyles[GetDrawItemStyle(ADrawNo)],
        FStyle.ParaStyles[GetDrawItemParaStyle(ADrawNo)]);
    //finally
    //  THCStyle.DestroyStyleCanvas(vCanvas);
    //end;
  end;
end;

function THCCustomData.GetDrawItemNoByOffset(const AItemNo, AOffset: Integer): Integer;
var
  i: Integer;
  vDrawItem: THCCustomDrawItem;
begin
  Result := FItems[AItemNo].FirstDItemNo;
  if FItems[AItemNo].StyleNo > THCStyle.Null then  // TextItem
  begin
    if FItems[AItemNo].Length > 0 then
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
end;

function THCCustomData.GetDrawItemOffsetAt(const ADrawItemNo, X: Integer): Integer;
var
  vRight, vWidth: Integer;
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
  vLen: Integer;
  vCharWArr: array of Integer;  // 每个字符绘制结束位置
  vSize: TSize;
  vExtraAll, vExtra: Integer;
begin
  Result := 0;
  vDrawItem := FDrawItems[ADrawItemNo];
  vItem  := FItems[vDrawItem.ItemNo];
  if vItem.StyleNo < THCStyle.Null then  // 非文本
    Result := (vItem as THCCustomRectItem).GetOffsetAt(X - vDrawItem.Rect.Left)
  else  // 文本
  begin
    Result := vDrawItem.CharLen;  // 赋值为最后，为方便行最右侧点击时返回为最后一个
    vText := (vItem as THCTextItem).SubString(vDrawItem.CharOffs, vDrawItem.CharLen);
    FStyle.ApplyTempStyle(vItem.StyleNo);
    vParaStyle := FStyle.ParaStyles[vItem.ParaNo];
    vWidth := X - vDrawItem.Rect.Left;

    case vParaStyle.AlignHorz of
      pahLeft, pahRight, pahCenter:
        Result := GetNorAlignCharOffsetAt(FStyle.TempCanvas, vText, vWidth);

      pahJustify, pahScatter:  // 20170220001 两端、分散对齐相关处理
        begin
          if vParaStyle.AlignHorz = pahJustify then  // 两端对齐
          begin
            if IsParaLastDrawItem(ADrawItemNo) then  // 两端对齐、段最后一行不处理
            begin
              Result := GetNorAlignCharOffsetAt(FStyle.TempCanvas, vText, vWidth);
              Exit;
            end;
          end;

          vLen := Length(vText);
          SetLength(vCharWArr, vLen);
          GetTextExtentExPoint(FStyle.TempCanvas.Handle, PChar(vText), vLen, 0,
            nil, PInteger(vCharWArr), vSize);  // 超过65535数组元素取不到值
          // 20190618002 需要同步修改的字符和位置相关的计算
          viSplitW := vDrawItem.Width - vCharWArr[vLen - 1];  // 当前DItem的Rect中用于分散的空间

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
            end
            else
              vMod := 0;

            vRight := 0;
            vExtraAll := 0;
            vExtra := 0;

            for i := 0 to vSplitList.Count - 2 do  // vSplitList最后一个是字符串长度所以多减1
            begin
              // 计算结束位置
              if vLineLast and (i = vSplitList.Count - 2) then  // 是当前DrawItem分隔的最后一个
                vExtra := 0
              else
              begin
                if vMod > 0 then
                begin
                  vExtra := viSplitW + 1;
                  Dec(vMod);
                end
                else
                  vExtra := viSplitW;

                vExtraAll := vExtraAll + vExtra;
              end;

              vRight := vCharWArr[(vSplitList[i + 1] - 1) - 1] + vExtraAll;  // 下一个分段的前一个字符，保证是这一个的最后字符，确保藏文最后字符

              if vRight > vWidth then  // 当前字符串结束位置超出给定的宽度，找具体位置
              begin
                j := vSplitList[i];
                while j < vSplitList[i + 1] do
                begin
                  {$IFDEF UNPLACEHOLDERCHAR}
                  j := GetTextActualOffset(vText, j, True);
                  {$ENDIF}

                  if vCharWArr[j - 1] + vExtraAll > vWidth then
                  begin
                    vRight := vExtraAll - vExtra div 2
                      + GetCharHalfFarfrom({$IFDEF UNPLACEHOLDERCHAR}vText,{$ENDIF} j, vCharWArr);  // 中间位置

                    if vWidth > vRight then  // 后半部分
                      Result := j
                    else  // 前半部分，返回前一个字符段最后位置
                    begin
                      Result := j - 1;
                      {$IFDEF UNPLACEHOLDERCHAR}
                      if IsUnPlaceHolderChar(vText[Result + 1]) then
                        Result := GetTextActualOffset(vText, Result) - 1;
                      {$ENDIF}
                    end;

                    Break;
                  end;
                end;

                Break;
              end;
            end;

            SetLength(vCharWArr, 0);
          finally
            vSplitList.Free;
          end;
        end;
    end;
  end;
end;

function THCCustomData.GetDrawItemOffsetWidth(const ADrawItemNo, ADrawOffs: Integer;
  const AStyleCanvas: TCanvas = nil): Integer;
var
  vCanvas: TCanvas;
  vDrawItem: THCCustomDrawItem;
  vText: string;
  vLen: Integer;
  vCharWArr: array of Integer;  // 每个字符绘制结束位置
  vSize: TSize;

  function _GetNorAlignDrawItemOffsetWidth: Integer;
  begin
    Result := 0;

    {$IFDEF UNPLACEHOLDERCHAR}
    vText := GetDrawItemText(ADrawItemNo);
    if vText <> '' then
    begin
      vLen := Length(vText);
      SetLength(vCharWArr, vLen);
      GetTextExtentExPoint(vCanvas.Handle, PChar(vText), vLen, 0,
        nil, PInteger(vCharWArr), vSize);
      Result := vCharWArr[ADrawOffs - 1];
      SetLength(vCharWArr, 0);
    end;
    {$ELSE}
    vText := Copy(FItems[vDrawItem.ItemNo].Text, vDrawItem.CharOffs, ADrawOffs);
    if vText <> '' then
      Result := vCanvas.TextWidth(vText);
    {$ENDIF}
  end;

var
  vStyleNo: Integer;
  vAlignHorz: TParaAlignHorz;

  vSplitList: THCIntegerList;
  vLineLast: Boolean;
  i, j, viSplitW, vSplitCount, vMod, {vCharWidth} vInnerOffs
    : Integer;
  vExtra, vX: Integer;
begin
  Result := 0;
  if ADrawOffs = 0 then Exit;

  vDrawItem := FDrawItems[ADrawItemNo];
  vStyleNo := FItems[vDrawItem.ItemNo].StyleNo;
  if vStyleNo < THCStyle.Null then  // 非文本
  begin
    if ADrawOffs > OffsetBefor then
      Result := FDrawItems[ADrawItemNo].Width;
  end
  else
  begin
    if Assigned(AStyleCanvas) then
      vCanvas := AStyleCanvas
    else
    begin
      vCanvas := FStyle.TempCanvas;
      FStyle.ApplyTempStyle(vStyleNo);
    end;

    vAlignHorz := FStyle.ParaStyles[GetDrawItemParaStyle(ADrawItemNo)].AlignHorz;
    case vAlignHorz of
      pahLeft, pahRight, pahCenter:
        Result := _GetNorAlignDrawItemOffsetWidth;

      pahJustify, pahScatter:  // 20170220001 两端、分散对齐相关处理
        begin
          if vAlignHorz = pahJustify then  // 两端对齐
          begin
            if IsParaLastDrawItem(ADrawItemNo) then  // 两端对齐、段最后一行不处理
            begin
              Result := _GetNorAlignDrawItemOffsetWidth;
              Exit;
            end;
          end;

          vText := GetDrawItemText(ADrawItemNo);
          vLen := Length(vText);
          SetLength(vCharWArr, vLen);
          GetTextExtentExPoint(vCanvas.Handle, PChar(vText), vLen, 0,
            nil, PInteger(vCharWArr), vSize);  // 超过65535数组元素取不到值
          // 20190618002 需要同步修改的字符和位置相关的计算
          viSplitW := vDrawItem.Width - vCharWArr[vLen - 1];  // 当前DItem的Rect中用于分散的空间

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
            end
            else
              vMod := 0;

            vExtra := 0;
            for i := 0 to vSplitList.Count - 2 do  // vSplitList最后一个是字符串长度所以多减1
            begin
              // 计算结束位置
              if vLineLast and (i = vSplitList.Count - 2) then  // 是当前DrawItem分隔的最后一个

              else
              if vMod > 0 then
              begin
                vExtra := vExtra + viSplitW + 1;
                Dec(vMod);
              end
              else
                vExtra := vExtra + viSplitW;

              vInnerOffs := vSplitList[i + 1] - 1;  // 在DrawItem中的偏移，下一个的前一个，保证是这一个的最后字符，确保藏文最后字符
              if vInnerOffs = ADrawOffs then  // 当前字符结束位置就是ADrawOffs
              begin
                Result := vCharWArr[vInnerOffs - 1] + vExtra;
                Break;
              end
              else
              if vInnerOffs > ADrawOffs then// 当前字符串结束位置在ADrawOffs后，找具体位置
              begin
                Result := vCharWArr[ADrawOffs - vSplitList[i]] + vExtra;
                Break;
              end;
            end;

            SetLength(vCharWArr, 0);
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
  vDrawItem: THCCustomDrawItem;
begin
  vDrawItem := FDrawItems[ADrawItemNo];
  Result := Copy(FItems[vDrawItem.ItemNo].Text, vDrawItem.CharOffs, vDrawItem.CharLen);
end;

function THCCustomData.GetItemOffsetAfter(const AItemNo: Integer): Integer;
begin
  if FItems[AItemNo].StyleNo < THCStyle.Null then
    Result := OffsetAfter
  else
    Result := FItems[AItemNo].Length;
end;

{$IFDEF UNPLACEHOLDERCHAR}
function THCCustomData.GetItemActualOffset(const AItemNo, AOffset: Integer;
  const AAfter: Boolean = False): Integer;
begin
  Result := GetTextActualOffset(FItems[AItemNo].Text, AOffset, AAfter);
end;
{$ENDIF}

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
          i := vStartDItemNo + (vEndDItemNo - vStartDItemNo) div 2;  // 二分

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
          if Y >= FDrawItems[vEndDItemNo].Rect.Bottom then  // 第二个下面，下边界算当前的，这样在划选到页底部时不至于选中下一页的
            vStartDItemNo := vEndDItemNo
          else
          if Y > FDrawItems[vEndDItemNo].Rect.Top then  // 第二个
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
    if FItems[AItemNo].StyleNo < THCStyle.Null then
      AOffset := OffsetBefor  // GetDrawItemOffsetAt(vStartDItemNo, X)
    else
      AOffset := FDrawItems[vStartDItemNo].CharOffs - 1;  // DrawItem起始
  end
  else
  if X >= FDrawItems[vEndDItemNo].Rect.Right then  // 在行右边点击
  begin
    ADrawItemNo := vEndDItemNo;
    AItemNo := FDrawItems[vEndDItemNo].ItemNo;
    if FItems[AItemNo].StyleNo < THCStyle.Null then
      AOffset := OffsetAfter  // GetDrawItemOffsetAt(vEndDItemNo, X)
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
        ARestrain := (Y < vDrawRect.Top) or (Y > vDrawRect.Bottom);

        ADrawItemNo := i;
        AItemNo := FDrawItems[i].ItemNo;
        if FItems[AItemNo].StyleNo < THCStyle.Null then  // RectItem
        begin
          if ARestrain then  // 垂直方向上约束
          begin
            if X < vDrawRect.Left + vDrawRect.Width div 2 then
              AOffset := OffsetBefor
            else
              AOffset := OffsetAfter;
          end
          else
            AOffset := GetDrawItemOffsetAt(i, X);
        end
        else  // TextItem
          AOffset := FDrawItems[i].CharOffs + GetDrawItemOffsetAt(i, X) - 1;

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

function THCCustomData.GetJustifyCount(const AText: string;
  const ACharIndexs: THCIntegerList): Integer;
var
  i: Integer;
begin
  Result := 0;
  if AText = '' then
    raise Exception.Create('异常：不能对空字符串计算分散！');

  if ACharIndexs <> nil then
    ACharIndexs.Clear;

  for i := 1 to Length(AText) do
  begin
    {$IFDEF UNPLACEHOLDERCHAR}
    if Pos(AText[i], UnPlaceholderChar) = 0 then
    {$ENDIF}
    begin
      Inc(Result);
      if ACharIndexs <> nil then
        ACharIndexs.Add(i);
    end;
  end;

  if ACharIndexs <> nil then
    ACharIndexs.Add(Length(AText) + 1);  // 为方便循环时 + 1不越界
end;

procedure THCCustomData.GetLineDrawItemRang(var AFirstDrawItemNo, ALastDrawItemNo: Integer);
begin
  while AFirstDrawItemNo > 0 do
  begin
    if FDrawItems[AFirstDrawItemNo].LineFirst then
      Break
    else
      Dec(AFirstDrawItemNo);
  end;

  ALastDrawItemNo := AFirstDrawItemNo + 1;
  while ALastDrawItemNo < FDrawItems.Count do
  begin
    if FDrawItems[ALastDrawItemNo].LineFirst then
      Break
    else
      Inc(ALastDrawItemNo);
  end;
  Dec(ALastDrawItemNo);
end;

function THCCustomData.GetLineFirstDrawItemNo(const AItemNo,
  AOffset: Integer): Integer;
begin
  //Result := AItemNo;
  Result := GetDrawItemNoByOffset(AItemNo, AOffset);

  while Result > 0 do
  begin
    if DrawItems[Result].LineFirst then
      Break
    else
      Dec(Result);
  end;
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

function THCCustomData.GetLineBlankSpace(const ADrawNo: Integer): Integer;
var
  i, vFirst, vLast, vHi, vMaxItemHi, vMaxDrawItemNo: Integer;
begin
  Result := 0;

  vFirst := ADrawNo;
  GetLineDrawItemRang(vFirst, vLast);

  // 找行起始和结束DrawItem
  // 找行中最高的DrawItem
  vMaxItemHi := 0;

  vMaxDrawItemNo := vFirst;
  for i := vFirst to vLast do
  begin
    if GetDrawItemStyle(i) < THCStyle.Null then
      vHi := (FItems[FDrawItems[i].ItemNo] as THCCustomRectItem).Height
    else
      vHi := FStyle.TextStyles[FItems[FDrawItems[i].ItemNo].StyleNo].FontHeight;

    if vHi > vMaxItemHi then
    begin
      vMaxItemHi := vHi;
      vMaxDrawItemNo := i;  // 记下最高的DrawItemNo
    end;
  end;

  if GetDrawItemStyle(vMaxDrawItemNo) < THCStyle.Null then
    Result := FStyle.LineSpaceMin
  else
    Result := GetDrawItemLineSpace(vMaxDrawItemNo) - vMaxItemHi;  // 根据最高的DrawItem取行间距
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

function THCCustomData.GetScreenCoord(const X, Y: Integer): TPoint;
begin
  Result := Self.GetRootData.GetScreenCoord(X, Y);
end;

function THCCustomData.GetSelectEndDrawItemNo: Integer;
begin
  if FSelectInfo.EndItemNo < 0 then
    Result := -1
  else
    Result := GetDrawItemNoByOffset(FSelectInfo.EndItemNo, FSelectInfo.EndItemOffset);
end;

function THCCustomData.GetSelectStartDrawItemNo: Integer;
begin
  if FSelectInfo.StartItemNo < 0 then
    Result := -1
  else
  begin
    Result := GetDrawItemNoByOffset(FSelectInfo.StartItemNo, FSelectInfo.StartItemOffset);
    if (FSelectInfo.EndItemNo >= 0) and (Result < FDrawItems.Count - 1) then  // 有选中结束
    begin
      if FItems[FSelectInfo.StartItemNo].StyleNo < THCStyle.Null then
      begin
        if FSelectInfo.StartItemOffset = OffsetAfter then
          Inc(Result);
      end
      else
      if (FDrawItems[Result].CharOffsetEnd = FSelectInfo.StartItemOffset) then  // 有选中时，SelectInfo.StartItemOffset在本行最后时，要转为下一行行首
        Inc(Result);
    end;
  end;
end;

function THCCustomData.GetSelectText: string;
begin
  Result := SaveSelectToText;
end;

function THCCustomData.GetTopLevelData: THCCustomData;
begin
  Result := nil;
  if (FSelectInfo.StartItemNo >= 0) and (FSelectInfo.EndItemNo < 0) then
  begin
    if (FItems[FSelectInfo.StartItemNo].StyleNo < THCStyle.Null)
      and (FSelectInfo.StartItemOffset = OffsetInner)
    then
      Result := (FItems[FSelectInfo.StartItemNo] as THCCustomRectItem).GetTopLevelData;
  end;

  if Result = nil then
    Result := Self;
end;

function THCCustomData.GetTopLevelDataAt(const X, Y: Integer): THCCustomData;
var
  vItemNo, vOffset, vDrawItemNo, vX, vY: Integer;
  vRestrain: Boolean;
begin
  Result := nil;
  GetItemAt(X, Y, vItemNo, vOffset, vDrawItemNo, vRestrain);
  if (not vRestrain) and (vItemNo >= 0) then
  begin
    if FItems[vItemNo].StyleNo < THCStyle.Null then
    begin
      CoordToItemOffset(X, Y, vItemNo, vOffset, vX, vY);
      Result := (FItems[vItemNo] as THCCustomRectItem).GetTopLevelDataAt(vX, vY);
    end;
  end;
  if Result = nil then
    Result := Self;
end;

function THCCustomData.GetTopLevelDrawItem: THCCustomDrawItem;
var
  vItem: THCCustomItem;
begin
  Result := nil;
  vItem := GetActiveItem;
  if vItem.StyleNo < THCStyle.Null then
    Result := (vItem as THCCustomRectItem).GetTopLevelDrawItem;

  if Result = nil then
    Result := GetActiveDrawItem;
end;

function THCCustomData.GetTopLevelItem: THCCustomItem;
begin
  Result := GetActiveItem;
  if (Result <> nil) and (Result.StyleNo < THCStyle.Null) then
    Result := (Result as THCCustomRectItem).GetTopLevelItem;
end;

function THCCustomData.GetTopLevelRectDrawItem: THCCustomDrawItem;
var
  vItem: THCCustomItem;
begin
  Result := nil;
  vItem := GetActiveItem;
  if vItem.StyleNo < THCStyle.Null then
  begin
    Result := (vItem as THCCustomRectItem).GetTopLevelRectDrawItem;
    if not Assigned(Result) then
      Result := GetActiveDrawItem;
  end;
end;

function THCCustomData.GetTopLevelRectDrawItemCoord: TPoint;
var
  vItem: THCCustomItem;
  vPt: TPoint;
begin
  Result := Point(-1, -1);
  vItem := GetActiveItem;
  if Assigned(vItem) and (vItem.StyleNo < THCStyle.Null) then
  begin
    Result := FDrawItems[vItem.FirstDItemNo].Rect.TopLeft;
    vPt := (vItem as THCCustomRectItem).GetTopLevelRectDrawItemCoord;
    if vPt.X >= 0 then
    begin
      vPt.Y := vPt.Y + FStyle.LineSpaceMin div 2;
      Result.X := Result.X + vPt.X;
      Result.Y := Result.Y + vPt.Y;
    end;
  end;
end;

function THCCustomData.GetUndoList: THCUndoList;
begin
  if Assigned(FOnGetUndoList) then
    Result := FOnGetUndoList
  else
    Result := nil;
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
  FLoading := True;
  try
    DoLoadFromStream(AStream, AStyle, AFileVersion);
  finally
    FLoading := False;
  end;
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
      if vItem.StyleNo < THCStyle.Null then
        (vItem as THCCustomRectItem).MarkStyleUsed(AMark)
      else
        FStyle.TextStyles[vItem.StyleNo].CheckSaveUsed := True;
    end
    else  // 重新赋值
    begin
      vItem.ParaNo := FStyle.ParaStyles[vItem.ParaNo].TempNo;
      if vItem.StyleNo < THCStyle.Null then
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
        if Items[AItemNo].StyleNo < THCStyle.Null then  // RectItem
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
      if Items[AItemNo].StyleNo < THCStyle.Null then  // RectItem
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

function THCCustomData.MergeItemText(const ADestItem,
  ASrcItem: THCCustomItem): Boolean;
begin
  Result := ADestItem.CanConcatItems(ASrcItem);
  if Result then
    ADestItem.Text := ADestItem.Text + ASrcItem.Text;
end;

function THCCustomData.MergeItemToNext(const AItemNo: Integer): Boolean;
begin
  Result := (AItemNo < Items.Count - 1) and (not Items[AItemNo + 1].ParaFirst)
    and MergeItemText(Items[AItemNo], Items[AItemNo + 1]);
end;

function THCCustomData.MergeItemToPrio(const AItemNo: Integer): Boolean;
begin
  Result := (AItemNo > 0) and (not Items[AItemNo].ParaFirst)
    and MergeItemText(Items[AItemNo - 1], Items[AItemNo]);
end;

function THCCustomData.OffsetInSelect(const AItemNo, AOffset: Integer): Boolean;
begin
  Result := False;
  if (AItemNo < 0) or (AOffset < 0) then Exit;

  if FItems[AItemNo].StyleNo < THCStyle.Null then // 非文本粗略判断，如需要精确用CoordInSelect间接调用
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

procedure THCCustomData.PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawRight,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom, AVOffset, AFristDItemNo,
  ALastDItemNo: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vTextDrawTop: Integer;

  {$REGION ' 当前显示范围内要绘制的DrawItem是否全选 '}
  function DrawItemSelectAll: Boolean;
  var
    vSelStartDItemNo, vSelEndDItemNo: Integer;
  begin
    vSelStartDItemNo := GetSelectStartDrawItemNo;
    vSelEndDItemNo := GetSelectEndDrawItemNo;

    Result :=  // 当前页是否全选中了
      (
        (vSelStartDItemNo < AFristDItemNo)
        or
        (
          (vSelStartDItemNo = AFristDItemNo)
          and
          (SelectInfo.StartItemOffset = FDrawItems[vSelStartDItemNo].CharOffsetStart)
        )
      )
      and
      (
        (vSelEndDItemNo > ALastDItemNo)
        or
        (
          (vSelEndDItemNo = ALastDItemNo)
          and
          (SelectInfo.EndItemOffset = FDrawItems[vSelEndDItemNo].CharOffsetEnd)
        )
      );
  end;
  {$ENDREGION}

  {$REGION ' DrawTextJsutify 20170220001 分散对齐相关处理 '}
  procedure DrawTextJsutify(const ARect: TRect; const AText: string; const ALineLast: Boolean);
  var
    i, vSplitCount, vX, vLen, viSplitW, vMod: Integer;
    vSplitList: THCIntegerList;
    vS: string;
    //vPos: array[0..0] of Integer;
    vExtra: Integer;
    vCharWArr: array of Integer;  // 每个字符绘制结束位置
    vSize: TSize;
  begin
    vX := ARect.Left;
    //  20160618001 处理紧缩字符
    vLen := Length(AText);
    SetLength(vCharWArr, vLen);
    GetTextExtentExPoint(FStyle.TempCanvas.Handle, PChar(AText), vLen, 0,
      nil, PInteger(vCharWArr), vSize);  // 超过65535数组元素取不到值
    // 20190618002 需要同步修改的字符和位置相关的计算
    viSplitW := ARect.Width - vCharWArr[vLen - 1];
    if viSplitW > 0 then  // 有多余的空间
    begin
      vSplitList := THCIntegerList.Create;  // 计算当前DrawItem内容分成几份，每一份在内容中的起始位置
      try
        vSplitCount := GetJustifyCount(AText, vSplitList);
        if ALineLast and (vSplitCount > 0) then  // 行最后DrawItem，少分一个
          Dec(vSplitCount);

        if vSplitCount > 0 then  // 有分到间距
        begin
          vMod := viSplitW mod vSplitCount;
          viSplitW := viSplitW div vSplitCount;
        end
        else
          vMod := 0;

        vX := 0;
        vExtra := 0;

        for i := 0 to vSplitList.Count - 2 do  // vSplitList最后一个是字符串长度所以多减1
        begin
          vLen := vSplitList[i + 1] - vSplitList[i];
          vS := Copy(AText, vSplitList[i], vLen);

          if i > 0 then  // 第一个左对齐，后面的才增加间距
            vX := vCharWArr[vSplitList[i] - 2] + vExtra;

          //Windows.ExtTextOut(ACanvas.Handle, vX, vTextDrawTop, 0, nil, PChar(vS), vLen, @vPos);  // 不支持藏文
          Windows.ExtTextOut(ACanvas.Handle, ARect.Left + vX, vTextDrawTop, 0, nil, PChar(vS), vLen, nil);

          // 准备下一个的起始位置
          if ALineLast and (i = vSplitList.Count - 2) then  // 最后一个

          else
          if vMod > 0 then
          begin
            vExtra := vExtra + viSplitW + 1;
            Dec(vMod);
          end
          else
            vExtra := vExtra + viSplitW;
        end;

        {for i := 0 to vSplitList.Count - 2 do  // vSplitList最后一个是字符串长度所以多减1
        begin
          vLen := vSplitList[i + 1] - vSplitList[i];
          vS := Copy(AText, vSplitList[i], vLen);

          if ALineLast and (i = vSplitList.Count - 2) then
            vPos[0] := FStyle.TempCanvas.TextWidth(vS)
          else
          if vMod > 0 then
          begin
            vPos[0] := FStyle.TempCanvas.TextWidth(vS) + viSplitW + 1;
            Dec(vMod);
          end
          else
            vPos[0] := FStyle.TempCanvas.TextWidth(vS) + viSplitW;

          //vRect := Rect(vX, vTextDrawTop, ARect.Right, ARect.Bottom);
          //Windows.DrawText(ACanvas.Handle, vS, -1, vRect, DT_LEFT or DT_SINGLELINE or vAlignVert);
          //ACanvas.TextOut(vX, vTextDrawTop, vS);
          // 201805161718
          //ETO_CLIPPED：正文将裁剪到矩形中。
          //ETO_GLYPH_INDEX：LpString指向由GetCharacterPlacement返回的数组，如果没有进一步的特殊语言处理的要求，则此数组直接由GDI解析，仅对字体应用符号索引，但此标志可用于位图和向量字体，以表示不必做进一步的语言处理，GDI应用直接处理此字符串。
          //ETO_OPAQUE：用当前的背景色来填充矩形。
          //ETO_RTLREADING：在Middle_Eastern Windows中如果指定了此值，且Hebrew或Arabic字体被选进设备环境，则此字符串用以从右到左的阅读顺序来输出。如果没有指定此值，则字符串以从左到右的顺序输出。在SetTextAlign中设置TA_RTLREADING值可获得同样的效果。为向后兼容，此值作为保留值。
          //ETO_GLYPH_INDEX和ETO_RTLREADING值不能在一起使用。因为ETO_GLYPH_INDEX表示所有的语言处理已经完成，函数就会忽略被指定的ETO_RTLREADING值。

          //Windows.ExtTextOut(ACanvas.Handle, vX, vTextDrawTop, 0, nil, PChar(vS), vLen, @vPos);  // 不支持藏文
          Windows.ExtTextOut(ACanvas.Handle, vX, vTextDrawTop, 0, nil, PChar(vS), vLen, nil);
          vX := vX + vPos[0];
        end;}
      finally
        vSplitList.Free;
      end;
    end
    else  // 直接绘制
      Windows.ExtTextOut(ACanvas.Handle, vX, vTextDrawTop, 0, nil, PChar(AText), Length(AText), nil);

    SetLength(vCharWArr, 0);
  end;
  {$ENDREGION}

var
  i, vSelStartDNo, vSelStartDOffs, vSelEndDNo, vSelEndDOffs,
  vPrioStyleNo, vPrioParaNo, vVOffset, vTextHeight, vLineSpace: Integer;
  vItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
  vDrawItem: THCCustomDrawItem;
  vAlignHorz: TParaAlignHorz;
  vDrawRect, vClearRect: TRect;
  vText: string;
  vLen: Integer;

  vDrawsSelectAll: Boolean;
  vDCState: THCCanvas;
begin
  if (AFristDItemNo < 0) or (ALastDItemNo < 0) then Exit;

  if not APaintInfo.Print then  // 非打印时获取选中信息
  begin
    vSelStartDNo := GetSelectStartDrawItemNo;  // 选中起始DrawItem
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
  end;

  vPrioStyleNo := THCStyle.Null;
  vPrioParaNo := THCStyle.Null;
  vVOffset := ADataDrawTop - AVOffset;  // 将数据起始位置映射到绘制位置

  vDCState := SaveCanvas(ACanvas);
  try
    if not FDrawItems[AFristDItemNo].LineFirst then
      vLineSpace := GetLineBlankSpace(AFristDItemNo);

    for i := AFristDItemNo to ALastDItemNo do  // 遍历要绘制的数据
    begin
      vDrawItem := FDrawItems[i];
      vItem := FItems[vDrawItem.ItemNo];
      vDrawRect := vDrawItem.Rect;
      OffsetRect(vDrawRect, ADataDrawLeft, vVOffset);  // 偏移到指定的画布绘制位置(SectionData时为页数据在格式化中可显示起始位置)

      if FDrawItems[i].LineFirst then
        vLineSpace := GetLineBlankSpace(i);

      { 绘制内容前 }
      DrawItemPaintBefor(Self, vDrawItem.ItemNo, i, vDrawRect, ADataDrawLeft,
        ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

      if vPrioParaNo <> vItem.ParaNo then  // 水平对齐方式
      begin
        vPrioParaNo := vItem.ParaNo;
        vAlignHorz := FStyle.ParaStyles[vItem.ParaNo].AlignHorz;  // 段内容水平对齐方式
      end;

      vClearRect := vDrawRect;
      InflateRect(vClearRect, 0, -vLineSpace div 2);  // 除去行间距净Rect，即内容的显示区域
      if vItem.StyleNo < THCStyle.Null then  // RectItem自行处理绘制
      begin
        vRectItem := vItem as THCCustomRectItem;
        vPrioStyleNo := vRectItem.StyleNo;

        if vRectItem.JustifySplit then  // 分散占空间  相互关联 201903261121
        begin
          if ((vAlignHorz = pahJustify) and (not IsParaLastDrawItem(i)))  // 两端对齐且不是段最后
            or (vAlignHorz = pahScatter)  // 分散对齐
          then
          begin
            if IsLineLastDrawItem(i) then  // 行最后一个，分散增加的位置放到最前面
              vClearRect.Offset(vClearRect.Width - vRectItem.Width, 0);
          end
          else
            vClearRect.Right := vClearRect.Left + vRectItem.Width;
        end;

        case FStyle.ParaStyles[vItem.ParaNo].AlignVert of  // 垂直对齐方式
          pavCenter: InflateRect(vClearRect, 0, -(vClearRect.Height - vRectItem.Height) div 2);
          pavTop: ;
        else
          vClearRect.Top := vClearRect.Bottom - vRectItem.Height;
        end;

        DrawItemPaintContent(Self, vDrawItem.ItemNo, i, vDrawRect, vClearRect, '', ADataDrawLeft,
          ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

        if vRectItem.IsSelectComplate then  // 选中背景区域
        begin
          ACanvas.Brush.Color := FStyle.SelColor;
          ACanvas.FillRect(vDrawRect);
        end;

        vItem.PaintTo(FStyle, vClearRect, ADataDrawTop, ADataDrawBottom,
          ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
      end
      else  // 文本Item
      begin
        if vItem.StyleNo <> vPrioStyleNo then  // 需要重新应用样式
        begin
          vPrioStyleNo := vItem.StyleNo;

          // 上标、下标仅是文本样式，高度以正常高度为准，先计算，防止ApplyStyle后变小
          //FStyle.DefCanvas.Font.Size := Round(FStyle.TextStyles[vPrioStyleNo].Size);
          //vTextHeight := THCStyle.GetFontHeight(FStyle.DefCanvas);

          FStyle.TextStyles[vPrioStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);
          FStyle.ApplyTempStyle(vPrioStyleNo);//, APaintInfo.ScaleY / APaintInfo.Zoom);
          //GetTextMetrics(FStyle.DefCanvas.Handle, vTextMetric);
          vTextHeight := FStyle.TextStyles[vPrioStyleNo].FontHeight;// THCStyle.GetFontHeight(FStyle.TempCanvas);
          if (tsSuperscript in FStyle.TextStyles[vPrioStyleNo].FontStyles)
            or (tsSubscript in FStyle.TextStyles[vPrioStyleNo].FontStyles)
          then
            vTextHeight := vTextHeight + vTextHeight;

          if vItem.HyperLink <> '' then
          begin
            ACanvas.Font.Color := HyperTextColor;
            ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
          end;
        end;

        case FStyle.ParaStyles[vItem.ParaNo].AlignVert of  // 垂直对齐方式
          pavCenter: vTextDrawTop := vClearRect.Top + (vClearRect.Bottom - vClearRect.Top - vTextHeight) div 2;
          pavTop: vTextDrawTop := vClearRect.Top;
        else
          vTextDrawTop := vClearRect.Bottom - vTextHeight;
        end;

        if tsSubscript in FStyle.TextStyles[vPrioStyleNo].FontStyles then  // 上标时位置不变，下标时中间位置
          vTextDrawTop := vTextDrawTop + vTextHeight div 2;

        if FStyle.TextStyles[vPrioStyleNo].BackColor <> HCTransparentColor then  // 文字背景
        begin
          ACanvas.Brush.Color := FStyle.TextStyles[vPrioStyleNo].BackColor;
          ACanvas.FillRect(Rect(vClearRect.Left, vClearRect.Top, vClearRect.Left + vDrawItem.Width, vClearRect.Bottom));
        end;

        vText := Copy(vItem.Text, vDrawItem.CharOffs, vDrawItem.CharLen);  // 为减少判断，没有直接使用GetDrawItemText(i)
        DrawItemPaintContent(Self, vDrawItem.ItemNo, i, vDrawRect, vClearRect, vText, ADataDrawLeft,
          ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

        {$REGION ' 绘制优先级更高的选中情况下的背景 '}
        if not APaintInfo.Print then  // 不是打印
        begin
          if vDrawsSelectAll then  // 当前要绘制的起始和结束DrawItem都被选中或单元格被全选中，背景为选中
          begin
            ACanvas.Brush.Color := FStyle.SelColor;
            ACanvas.FillRect(Rect(vDrawRect.Left, vDrawRect.Top,
              vDrawRect.Left + vDrawItem.Width, Math.Min(vDrawRect.Bottom, ADataScreenBottom)));
          end
          else  // 处理一部分选中
          if vSelEndDNo >= 0 then  // 有选中内容，部分背景为选中
          begin
            ACanvas.Brush.Color := FStyle.SelColor;
            if (vSelStartDNo = vSelEndDNo) and (i = vSelStartDNo) then  // 选中内容都在当前DrawItem
            begin
              ACanvas.FillRect(Rect(vDrawRect.Left + GetDrawItemOffsetWidth(i, vSelStartDOffs, FStyle.TempCanvas),
                vDrawRect.Top,
                vDrawRect.Left + GetDrawItemOffsetWidth(i, vSelEndDOffs, FStyle.TempCanvas),
                Math.Min(vDrawRect.Bottom, ADataScreenBottom)));
            end
            else
            if i = vSelStartDNo then  // 选中在不同DrawItem，当前是起始
            begin
              ACanvas.FillRect(Rect(vDrawRect.Left + GetDrawItemOffsetWidth(i, vSelStartDOffs, FStyle.TempCanvas),
                vDrawRect.Top,
                vDrawRect.Right,
                Math.Min(vDrawRect.Bottom, ADataScreenBottom)));
            end
            else
            if i = vSelEndDNo then  // 选中在不同的DrawItem，当前是结束
            begin
              ACanvas.FillRect(Rect(vDrawRect.Left,
                vDrawRect.Top,
                vDrawRect.Left + GetDrawItemOffsetWidth(i, vSelEndDOffs, FStyle.TempCanvas),
                Math.Min(vDrawRect.Bottom, ADataScreenBottom)));
            end
            else
            if (i > vSelStartDNo) and (i < vSelEndDNo) then  // 选中起始和结束DrawItem之间的DrawItem
              ACanvas.FillRect(vDrawRect);
          end;
        end;
        {$ENDREGION}

        vItem.PaintTo(FStyle, vClearRect, ADataDrawTop, ADataDrawBottom,
          ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);  // 触发Item绘制事件

        // 绘制文本
        if vText <> '' then
        begin
          if APaintInfo.Print and vItem.PrintInvisible then

          else
          begin
            ACanvas.Brush.Style := bsClear;  // 防止选中后面的输出文本时背景没有清空的问题
            case vAlignHorz of  // 水平对齐方式
              pahLeft, pahRight, pahCenter:  // 一般对齐
                begin
                  {if GetTextExtentExPoint(FStyle.DefCanvas.Handle, PChar(S), vLen,
                    vDrawRect.Right, @vFit, PInteger(vCharWidths), vSize)
                    ExtTextOut(ACanvas.Handle, vDrawRect.Left, vDrawTop, ETO_CLIPPED, @vDrawRect, S, vLen, PInteger(vCharWidths));
                    Windows.DrawText(ACanvas.Handle, S, -1, vDrawRect, DT_LEFT or DT_SINGLELINE or vAlignVert);} // -1全部

                  vLen := Length(vText);

                  Windows.ExtTextOut(ACanvas.Handle, vClearRect.Left, vTextDrawTop,
                    0, nil, PChar(vText), vLen, nil);  // 参数说明见 201805161718
                  //Windows.TextOut(ACanvas.Handle, vDrawRect.Left, vTextDrawTop, PChar(S), vLen);
                end;

              pahJustify, pahScatter:  // 两端、分散对齐
                DrawTextJsutify(vClearRect, vText, IsLineLastDrawItem(i));
            end;
          end;
        end
        else  // 空行
        begin
          {$IFDEF CHECKNULLITEM}
          if not vItem.ParaFirst then  // 不是段空行
            raise Exception.Create(HCS_EXCEPTION_NULLTEXT);
          {$ENDIF}
        end;
      end;

      DrawItemPaintAfter(Self, vDrawItem.ItemNo, i, vClearRect, ADataDrawLeft,
        ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);  // 绘制内容后
    end;
  finally
    vDCState.ToCanvas(ACanvas);
    FreeAndNil(vDCState);
  end;
end;

procedure THCCustomData.PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawRight,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vFirstDItemNo, vLastDItemNo, vVOffset: Integer;
begin
  if FItems.Count = 0 then Exit;

  vVOffset := ADataDrawTop - AVOffset;  // 将数据起始位置映射到绘制位置

  GetDataDrawItemRang(Max(ADataDrawTop, ADataScreenTop) - vVOffset,  // 可显示出来的DrawItem范围
    Min(ADataDrawBottom, ADataScreenBottom) - vVOffset, vFirstDItemNo, vLastDItemNo);

  PaintData(ADataDrawLeft, ADataDrawTop, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, AVOffset, vFirstDItemNo, vLastDItemNo, ACanvas, APaintInfo);
end;

procedure THCCustomData.ParseXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vItemNode: IHCXMLNode;
  vItem: THCCustomItem;
begin
  Clear;

  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    vItemNode := ANode.ChildNodes[i];
    vItem := CreateItemByStyle(vItemNode.Attributes['sno']);
    vItem.ParseXml(vItemNode);
    FItems.Add(vItem);
  end;

  if Items[0].Length = 0 then  // 删除Clear后默认的第一个空行Item
    FItems.Delete(0);
end;

function THCCustomData.CalcContentHeight: Integer;
begin
  if FDrawItems.Count > 0 then
    Result := FDrawItems[DrawItems.Count - 1].Rect.Bottom - FDrawItems[0].Rect.Top
  else
    Result := 0;
end;

function THCCustomData.CalculateLineHeight(const ATextStyle: THCTextStyle;
  const AParaStyle: THCParaStyle): Integer;
var
  //vOutlineTextmetric: ^TOutlineTextmetric;
  //vFontSignature: TFontSignature;
  //vHorizontalHeader: TT_HHEA;
  vLineSpacing, vDelta, vLeading, vOtherLeading: Integer;
  vAscent, vDescent{, vLineGap}: Word;
  //vOutMetSize: Cardinal;
  vSizeScale: Single;
  vTextMetric: TTextMetric;
begin
  //ATextStyle.ApplyStyle(ACanvas);  调用前请确认应用过样式了

  Result := ATextStyle.FontHeight;// THCStyle.GetFontHeight(ACanvas);  // 行高

  if AParaStyle.LineSpaceMode = TParaLineSpaceMode.plsMin then Exit;

  if AParaStyle.LineSpaceMode = TParaLineSpaceMode.plsFix then
  begin
    vLineSpacing := HCUnitConversion.MillimeterToPixY(AParaStyle.LineSpace * 0.3527);
    if vLineSpacing < Result then

    else
      Result := vLineSpacing;

    Exit;
  end;

  if FStyle.FormatVersion = 2 then
  begin
    vTextMetric := ATextStyle.TextMetric;

    case AParaStyle.LineSpaceMode of
      pls115: Result := Result + Round(vTextMetric.tmHeight * 0.15);

      pls150: Result := Result + Round(vTextMetric.tmHeight * 0.5);

      pls200: Result := Result + vTextMetric.tmHeight;

      plsMult: Result := Result + Round(vTextMetric.tmHeight * AParaStyle.LineSpace);
    end;
  end
  else
  if (ATextStyle.OutMetSize > 0) and ATextStyle.CJKFont then
  begin
    if (ATextStyle.OutlineTextmetric.otmfsSelection and 128) <> 0 then  // 有加粗或其他样式
    begin
      vAscent := ATextStyle.OutlineTextmetric.otmAscent;
      vDescent := -ATextStyle.OutlineTextmetric.otmDescent;
      //vLineSpacing := vAscent + vDescent + ATextStyle.OutlineTextmetric.otmLineGap;
    end
    else
    begin
      vAscent := SwapBytes(ATextStyle.FontHeader.Ascender);
      vDescent := -SwapBytes(ATextStyle.FontHeader.Descender); // 基线向下
      //vLineGap := SwapBytes(ATextStyle.FontHeader.LineGap);
      //vLineSpacing := vAscent + vDescent + vLineGap;
      vSizeScale := ATextStyle.Size / FontSizeScale / ATextStyle.OutlineTextmetric.otmEMSquare;
      vAscent := Ceil(vAscent * vSizeScale);
      vDescent := Ceil(vDescent * vSizeScale);
      //vLineSpacing := Ceil(vLineSpacing * vSizeScale);
      //vUnderlinePosition := Ceil(vAscent * 1.15 + vDescent * 0.85);
      vLineSpacing := Ceil(1.3 * (vAscent + vDescent));
      vDelta := vLineSpacing - (vAscent + vDescent);
      vLeading := vDelta div 2;
      vOtherLeading := vDelta - vLeading;
      Inc(vAscent, vLeading);
      Inc(vDescent, vOtherLeading);

      Result := vAscent + vDescent;
      case AParaStyle.LineSpaceMode of
        pls115: Result := Result + Trunc(3 * Result / 20);

        pls150: Result := Trunc(3 * Result / 2);

        pls200: Result := Result * 2;

        plsMult: Result := Trunc(Result * AParaStyle.LineSpace);
      end;
    end;
  end
  else
  begin
    //GetTextMetrics(vDC, vTextMetric);  // 得到字体度量信息
    vTextMetric := ATextStyle.TextMetric;

    case AParaStyle.LineSpaceMode of
      pls100: Result := Result + vTextMetric.tmExternalLeading; // Round(vTextMetric.tmHeight * 0.2);

      pls115: Result := Result + vTextMetric.tmExternalLeading + Round((vTextMetric.tmHeight + vTextMetric.tmExternalLeading) * 0.15);

      pls150: Result := Result + vTextMetric.tmExternalLeading + Round((vTextMetric.tmHeight + vTextMetric.tmExternalLeading) * 0.5);

      pls200: Result := Result + vTextMetric.tmExternalLeading + vTextMetric.tmHeight + vTextMetric.tmExternalLeading;

      plsMult: Result := Result + vTextMetric.tmExternalLeading + Round((vTextMetric.tmHeight + vTextMetric.tmExternalLeading) * AParaStyle.LineSpace);
    end;
  end;
end;

function THCCustomData.CanEdit: Boolean;
begin
  Result := True;
end;

procedure THCCustomData.SaveToStream(const AStream: TStream);
begin
  SaveToStream(AStream, 0, 0, FItems.Count - 1, FItems.Last.Length);
end;

procedure THCCustomData.SaveItemToStream(const AStream: TStream;
  const AStartItemNo, AStartOffset, AEndItemNo, AEndOffset: Integer);
var
  i, vCount, vCountAct: Integer;
  vBegPos, vEndPos: Int64;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 数据大小占位，便于越过
  //
  { if IsEmpty then i := 0 else 空Item也要存，CellData加载时高度可由此Item样式计算 }
  vCount := AEndItemNo - AStartItemNo + 1;
  AStream.WriteBuffer(vCount, SizeOf(vCount));  // 默认数量
  if vCount > 0 then
  begin
    vCountAct := 0;

    if AStartItemNo <> AEndItemNo then
    begin
      if DoSaveItem(AStartItemNo) then
      begin
        FItems[AStartItemNo].SaveToStream(AStream, AStartOffset, FItems[AStartItemNo].Length);
        Inc(vCountAct);
      end;

      for i := AStartItemNo + 1 to AEndItemNo - 1 do
      begin
        if DoSaveItem(i) then
        begin
          FItems[i].SaveToStream(AStream);
          Inc(vCountAct);
        end;
      end;

      if DoSaveItem(AEndItemNo) then
      begin
        FItems[AEndItemNo].SaveToStream(AStream, 0, AEndOffset);
        Inc(vCountAct);
      end;
    end
    else
    if DoSaveItem(AStartItemNo) then
    begin
      FItems[AStartItemNo].SaveToStream(AStream, AStartOffset, AEndOffset);
      Inc(vCountAct);
    end;
  end;
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 当前页数据大小
  if vCount <> vCountAct then
    AStream.WriteBuffer(vCountAct, SizeOf(vCountAct));  // 实际数量

  AStream.Position := vEndPos;
end;

procedure THCCustomData.SaveSelectToStream(const AStream: TStream);
begin
  if SelectExists then
  begin
    if (FSelectInfo.EndItemNo < 0)
      and (FItems[FSelectInfo.StartItemNo].StyleNo < THCStyle.Null)
    then  // 选择仅发生在同一个RectItem
    begin
      if (FItems[FSelectInfo.StartItemNo] as THCCustomRectItem).IsSelectComplateTheory then  // 理论全选中了
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
    if (FSelectInfo.EndItemNo < 0) and (FItems[FSelectInfo.StartItemNo].StyleNo < THCStyle.Null) then
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
begin
  Self.SaveItemToStream(AStream, AStartItemNo, AStartOffset, AEndItemNo, AEndOffset);
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
      if DoSaveItem(AStartItemNo) then // 起始
      begin
        if FItems[AStartItemNo].StyleNo > THCStyle.Null then
          Result := (FItems[AStartItemNo] as THCTextItem).SubString(AStartOffset + 1, FItems[AStartItemNo].Length - AStartOffset)
        else
          Result := (FItems[AStartItemNo] as THCCustomRectItem).SaveSelectToText;
      end;

      for i := AStartItemNo + 1 to AEndItemNo - 1 do  // 中间
      begin
        if DoSaveItem(i) then
        begin
          if FItems[i].ParaFirst then
            Result := Result + sLineBreak + FItems[i].Text
          else
            Result := Result + FItems[i].Text;
        end;
      end;

      if DoSaveItem(AEndItemNo) then  // 结尾
      begin
        if FItems[AEndItemNo].StyleNo > THCStyle.Null then
        begin
          if FItems[i].ParaFirst then
            Result := Result + sLineBreak;

          Result := Result + (FItems[AEndItemNo] as THCTextItem).SubString(1, AEndOffset);
        end
        else
        begin
          if FItems[i].ParaFirst then
            Result := Result + sLineBreak;

          Result := Result + (FItems[AEndItemNo] as THCCustomRectItem).SaveSelectToText;
        end;
      end;
    end
    else  // 选中在同一Item
    begin
      if DoSaveItem(AStartItemNo) then
      begin
        if FItems[AStartItemNo].StyleNo > THCStyle.Null then
          Result := (FItems[AStartItemNo] as THCTextItem).SubString(AStartOffset + 1, AEndOffset - AStartOffset);
      end;
    end;
  end;
end;

function THCCustomData.SaveToText: string;
begin
  Result := SaveToText(0, 0, FItems.Count - 1, FItems.Last.Length);
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
      FSelectInfo.EndItemOffset := GetItemOffsetAfter(FSelectInfo.EndItemNo);
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
      if FItems[i].StyleNo < THCStyle.Null then
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
    and (FSelectInfo.EndItemNo = FItems.Count - 1)
    and (FSelectInfo.EndItemOffset = GetItemOffsetAfter(FItems.Count - 1));
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
      if AIfRectItem and (FItems[FSelectInfo.StartItemNo].StyleNo < THCStyle.Null) then
      begin
        //if FSelectInfo.StartItemOffset = OffsetInner then  表格整体选中时不成立
          Result := (FItems[FSelectInfo.StartItemNo] as THCCustomRectItem).SelectExists;
      end;
    end;
  end;
end;

function THCCustomData.SelectInSameDrawItem: Boolean;
var
  vStartDNo: Integer;
begin
  vStartDNo := GetSelectStartDrawItemNo;
  if vStartDNo < 0 then
    Result := False
  else
  begin
    if GetDrawItemStyle(vStartDNo) < THCStyle.Null then
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
    if (FCaretDrawItemNo >= 0) and (FCaretDrawItemNo < FDrawItems.Count) then  // 有旧的
    begin
      vItemNo := FDrawItems[FCaretDrawItemNo].ItemNo;
      if (Value >= 0) and (vItemNo <> FDrawItems[Value].ItemNo) then
        FItems[vItemNo].Active := False;  // 旧的取消激活
    end
    else
      vItemNo := -1;

    FCaretDrawItemNo := Value;

    if FStyle.States.Contain(THCState.hosLoading) then
      Exit;

    SetCurStyleNo(FItems[FDrawItems[FCaretDrawItemNo].ItemNo].StyleNo);
    SetCurParaNo(FItems[FDrawItems[FCaretDrawItemNo].ItemNo].ParaNo);

    if (FCaretDrawItemNo >= 0) and (FDrawItems[FCaretDrawItemNo].ItemNo <> vItemNo) then  // 有新的
    begin
      if FItems[FDrawItems[FCaretDrawItemNo].ItemNo].StyleNo < THCStyle.Null then
      begin
        if FSelectInfo.StartItemOffset = OffsetInner then
          FItems[FDrawItems[FCaretDrawItemNo].ItemNo].Active := True;
      end
      else
      begin
      //if (FSelectInfo.StartItemOffset > 0)  // 在Item上
      //  and (FSelectInfo.StartItemOffset < FItems[FDrawItems[FCaretDrawItemNo].ItemNo].Length)
      //then
        FItems[FDrawItems[FCaretDrawItemNo].ItemNo].Active := True;  // 激活新的
      end;

      DoCaretItemChanged;
    end;
  end;
end;

procedure THCCustomData.SetCurParaNo(const Value: Integer);
begin
  if FCurParaNo <> Value then
  begin
    FCurParaNo := Value;
    if Assigned(FOnCurParaNoChange) then
      FOnCurParaNoChange(Self);
  end;
end;

procedure THCCustomData.SetCurStyleNo(const Value: Integer);
begin
  if FCurStyleNo <> Value then
    FCurStyleNo := Value;
end;

function THCCustomData.ToHtml(const APath: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].ParaFirst then
    begin
      if i <> 0 then
        Result := Result + sLineBreak + '</p>';
      Result := Result + sLineBreak + '<p class="ps' + IntToStr(Items[i].ParaNo) + '">';
    end;

    Result := Result + sLineBreak + Items[i].ToHtml(APath);
  end;

  Result := Result + sLineBreak + '</p>';
end;

procedure THCCustomData.ToXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vNode: IHCXMLNode;
begin
  ANode.Attributes['itemcount'] := FItems.Count;
  for i := 0 to FItems.Count - 1 do
  begin
    vNode := ANode.AddChild('item');
    FItems[i].ToXml(vNode);
  end;
end;

function THCCustomData.GetActiveDrawItem: THCCustomDrawItem;
var
  vDrawItemNo: Integer;
begin
  vDrawItemNo := GetActiveDrawItemNo;
  if vDrawItemNo < 0 then
    Result := nil
  else
    Result := FDrawItems[vDrawItemNo];
end;

function THCCustomData.GetTopLevelDrawItemCoord: TPoint;
var
  vItem: THCCustomItem;
  vDrawItem: THCCustomDrawItem;
  vPt: TPoint;
begin
  Result := Point(0, 0);
  vPt := Point(0, 0);
  vDrawItem := GetActiveDrawItem;
  if Assigned(vDrawItem) then
  begin
    Result := vDrawItem.Rect.TopLeft;

    vItem := GetActiveItem;
    if vItem.StyleNo < THCStyle.Null then
    begin
      vPt := (vItem as THCCustomRectItem).GetTopLevelDrawItemCoord;
      vPt.Y := vPt.Y + FStyle.LineSpaceMin div 2;
    end;

    Result.X := Result.X + vPt.X;
    Result.Y := Result.Y + vPt.Y;
  end;
end;

function THCCustomData.GetActiveDrawItemNo: Integer;
var
  i, vItemNo: Integer;
  vDrawItem: THCCustomDrawItem;
begin
  Result := -1;
  if FCaretDrawItemNo >= 0 then
  begin
    Result := FCaretDrawItemNo;
    Exit;
  end;

  if FSelectInfo.StartItemNo < 0 then  // 没有选择

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

    if FItems[vItemNo].StyleNo < THCStyle.Null then  // RectItem
      Result := FItems[vItemNo].FirstDItemNo
    else  // 文本
    begin
      for i := FItems[vItemNo].FirstDItemNo to FDrawItems.Count - 1 do
      begin
        vDrawItem := FDrawItems[i];
        if FSelectInfo.StartItemOffset - vDrawItem.CharOffs + 1 <= vDrawItem.CharLen then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function THCCustomData.GetActiveItem: THCCustomItem;
var
  vItemNo: Integer;
begin
  vItemNo := GetActiveItemNo;
  if vItemNo < 0 then
    Result := nil
  else
    Result := FItems[vItemNo];
end;

function THCCustomData.GetActiveItemNo: Integer;
begin
  Result := FSelectInfo.StartItemNo;
end;

procedure THCCustomData.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: THCCaretInfo);
var
  vDrawItemNo: Integer;
  vDrawItem: THCCustomDrawItem;
  vRectItem: THCCustomRectItem;

  procedure GetRectItemInnerCaretInfo;
  var
    vLineSpaceHalf: Integer;
    vDrawRect: TRect;
  begin
    vRectItem.GetCaretInfo(ACaretInfo);

    vDrawRect := vDrawItem.Rect;
    vLineSpaceHalf := GetLineBlankSpace(vDrawItemNo) div 2;
    InflateRect(vDrawRect, 0, -vLineSpaceHalf);

    case FStyle.ParaStyles[FItems[AItemNo].ParaNo].AlignVert of  // 垂直对齐方式
      pavCenter: ACaretInfo.Y := ACaretInfo.Y + vLineSpaceHalf + (vDrawRect.Height - vRectItem.Height) div 2;

      pavTop: ACaretInfo.Y := ACaretInfo.Y + vLineSpaceHalf;
    else
      ACaretInfo.Y := ACaretInfo.Y + vLineSpaceHalf + vDrawRect.Height - vRectItem.Height;
    end;
  end;

var
  vStyleItemNo: Integer;
begin
  { 注意：为处理RectItem往外迭代，这里位置处理为叠加，而不是直接赋值 }
  if FCaretDrawItemNo < 0 then
  begin
    if FItems[AItemNo].StyleNo < THCStyle.Null then  // RectItem
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
        and (Items[AItemNo - 1].StyleNo > THCStyle.Null)
      then  // 前一个是TextItem
        vStyleItemNo := AItemNo - 1;
    end;

    if (Items[vStyleItemNo] is THCTextRectItem) and (FSelectInfo.StartItemOffset = OffsetInner) then
      Self.CurStyleNo := (Items[vStyleItemNo] as THCTextRectItem).TextStyleNo
    else
      Self.CurStyleNo := Items[vStyleItemNo].StyleNo;

    Self.CurParaNo := Items[vStyleItemNo].ParaNo;
  end;

  if FItems[AItemNo].StyleNo < THCStyle.Null then  // RectItem
  begin
    vRectItem := FItems[AItemNo] as THCCustomRectItem;

    if AOffset = OffsetBefor then  // 在其左侧
    begin
      if vRectItem.CanPageBreak then
        GetRectItemInnerCaretInfo;

      ACaretInfo.X := ACaretInfo.X + vDrawItem.Rect.Left;
    end
    else
    if AOffset = OffsetInner then  // 正在其上，由内部决定
    begin
      GetRectItemInnerCaretInfo;
      ACaretInfo.X := ACaretInfo.X + vDrawItem.Rect.Left;
    end
    else  // 在其右侧
    begin
      if vRectItem.CanPageBreak then
        GetRectItemInnerCaretInfo;

      ACaretInfo.X := ACaretInfo.X + vDrawItem.Rect.Right;
    end;

    if vRectItem.JustifySplit then  // 分散占空间  相互关联 201903261121
    begin
      if ((FStyle.ParaStyles[vRectItem.ParaNo].AlignHorz = pahJustify) and (not IsParaLastDrawItem(vDrawItemNo)))  // 两端对齐且不是段最后
        or (FStyle.ParaStyles[vRectItem.ParaNo].AlignHorz = pahScatter)  // 分散对齐
      then
      begin
        if IsLineLastDrawItem(vDrawItemNo) then  // 行最后一个，分散增加的位置放到最前面
          ACaretInfo.X := ACaretInfo.X + vDrawItem.Width - vRectItem.Width;
      end
      else
        ACaretInfo.X := ACaretInfo.X + vDrawItem.Width - vRectItem.Width;
    end;
  end
  else  // TextItem
    ACaretInfo.X := ACaretInfo.X + vDrawItem.Rect.Left
      + GetDrawItemOffsetWidth(vDrawItemNo, AOffset - vDrawItem.CharOffs + 1);

  ACaretInfo.Y := ACaretInfo.Y + vDrawItem.Rect.Top;
end;

procedure THCCustomData.InitializeField;
begin
  //if FCaretDrawItemNo >= 0 then  // 表格上删除选中单元格中的内容时，因为删除前表格Active被这里处理成False了导致删除时单元格没有选中删除无效
  //  FItems[FDrawItems[FCaretDrawItemNo].ItemNo].Active := False;

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
  FStartRestrain := False;
  FEndItemNo := -1;
  FEndItemOffset := -1;
end;

{ THCDomainInfo }

procedure THCDomainInfo.Clear;
begin
  Data := nil;
  FBeginNo := -1;
  FEndNo := -1;
end;

function THCDomainInfo.Contain(const AItemNo: Integer): Boolean;
begin
  Result := (AItemNo >= FBeginNo) and (AItemNo <= FEndNo);
end;

constructor THCDomainInfo.Create;
begin
  Clear;
end;

{ THCItemTraverse }

constructor THCItemTraverse.Create;
begin
  Areas := [];
  Tag := 0;
  Stop := False;
  DomainStack := TDomainStack.Create;
end;

destructor THCItemTraverse.Destroy;
begin
  FreeAndNil(DomainStack);
  inherited Destroy;
end;

end.
