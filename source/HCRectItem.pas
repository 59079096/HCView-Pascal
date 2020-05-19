{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{            文档RectItem对象基类实现单元               }
{                                                       }
{*******************************************************}

unit HCRectItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCItem, HCDrawItem, HCTextStyle, HCParaStyle,
  HCStyleMatch, HCStyle, HCCommon, HCCustomData, HCUndo, HCXml;

const
  /// <summary> 光标在RectItem前面 </summary>
  OffsetBefor = 0;

  /// <summary> 光标在RectItem区域内 </summary>
  OffsetInner = 1;

  /// <summary> 光标在RectItem后面 </summary>
  OffsetAfter = 2;

type
  THCCustomRectItem = class(THCCustomItem)  // RectItem基类
  strict private
    FWidth, FHeight: Integer;
    FTextWrapping: Boolean;  // 文本环绕
    FOwnerData: THCCustomData;
    // 标识内部高度是否发生了变化，用于此Item内部格式化时给其所属的Data标识需要重新格式化此Item
    // 如表格的一个单元格内容变化在没有引起表格整体变化时，不需要重新格式化表格，也不需要重新计算页数
    // 由拥有此Item的Data使用完后应该立即赋值为False，可参考TableItem.KeyPress的使用
    FSizeChanged,
    FCanPageBreak: Boolean;  // 在当前页显示不下时是否可以分页截断显示
    FOnGetMainUndoList: TGetUndoListEvent;
  protected
    FMangerUndo: Boolean;  // 是否自己管理撤销恢复
    function GetWidth: Integer; virtual;
    procedure SetWidth(const Value: Integer); virtual;
    function GetHeight: Integer; virtual;
    procedure SetHeight(const Value: Integer); virtual;
    procedure DoSizeChanged; virtual;
    procedure SetSizeChanged(const Value: Boolean);
    //
    procedure SelfUndoListInitializate(const AUndoList: THCUndoList);
    procedure SelfUndo_New;
    function GetSelfUndoList: THCUndoList;

    procedure DoSelfUndoDestroy(const AUndo: THCUndo); virtual;
    function DoSelfUndoNew: THCUndo; virtual;
    procedure DoSelfUndo(const AUndo: THCUndo); virtual;
    procedure DoSelfRedo(const ARedo: THCUndo); virtual;
  public
    /// <summary> 适用于运行期间创建 </summary>
    constructor Create(const AOwnerData: THCCustomData); overload; virtual;
    /// <summary> 适用于加载时创建 </summary>
    constructor Create(const AOwnerData: THCCustomData; const AWidth, AHeight: Integer); overload; virtual;

    procedure ApplySelectTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch); virtual;
    /// <summary> RectItem内部Data是否响应了修改样式 </summary>
    procedure ApplySelectParaStyle(const AStyle: THCStyle; const AMatchStyle: THCParaMatch); virtual;
    /// <summary> RectItem内容对齐方式 </summary>
    procedure ApplyContentAlign(const AAlign: THCContentAlign); virtual;
    // 当前RectItem格式化时所属的Data(为松耦合请传入TCustomRichData类型)
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); virtual;

    /// <summary> 清除并返回为处理分页比净高增加的高度(为重新格式化时后面计算偏移用) </summary>
    function ClearFormatExtraHeight: Integer; virtual;
    procedure ReFormatActiveItem; virtual;
    /// <summary> ActiveItem重新适应其环境(供外部直接修改Item属性后重新和其前后Item连接组合) </summary>
    procedure ActiveItemReAdaptEnvironment; virtual;
    function DeleteSelected: Boolean; virtual;
    /// <summary> 删除当前域 </summary>
    function DeleteActiveDomain: Boolean; virtual;
    /// <summary> 删除当前Data指定范围内的Item </summary>
    procedure DeleteActiveDataItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean); virtual;
    /// <summary> 直接设置当前TextItem的Text值 </summary>
    procedure SetActiveItemText(const AText: string); virtual;
    procedure MarkStyleUsed(const AMark: Boolean); virtual;
    procedure SaveSelectToStream(const AStream: TStream); virtual;
    function SaveSelectToText: string; virtual;
    function GetActiveItem: THCCustomItem; virtual;
    function GetTopLevelItem: THCCustomItem; virtual;
    function GetTopLevelDrawItem: THCCustomDrawItem; virtual;
    function GetTopLevelDrawItemCoord: TPoint; virtual;
    function GetTopLevelRectDrawItem: THCCustomDrawItem; virtual;
    function GetTopLevelRectDrawItemCoord: TPoint; virtual;
    /// <summary> 获取指定X位置对应的Offset </summary>
    function GetOffsetAt(const X: Integer): Integer; virtual;
    /// <summary> 获取坐标X、Y是否在选中区域中 </summary>
    function CoordInSelect(const X, Y: Integer): Boolean; virtual;
    /// <summary> 正在其上时内部是否处理指定的Key和Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; virtual;
    /// <summary> 分散对齐时是否分间距 </summary>
    function JustifySplit: Boolean; virtual;
    /// <summary> 更新光标位置 </summary>
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); virtual;

    /// <summary> 获取在指定高度内的结束位置处最下端(暂时没用到注释了) </summary>
    /// <param name="AHeight">指定的高度范围</param>
    /// <param name="ADItemMostBottom">最底端DItem的底部位置</param>
    //procedure GetPageFmtBottomInfo(const AHeight: Integer; var ADItemMostBottom: Integer); virtual;

    // 准备判断分页前触发，可用于清除自己记录的分页信息，准备重新计算分页
    procedure CheckFormatPageBreakBefor; virtual;

    /// <summary> 计算格式化后的分页位置 </summary>
    /// <param name="ADrawItemRectTop">对应的DrawItem的Rect.Top往下行间距一半</param>
    /// <param name="ADrawItemRectBottom">对应的DrawItem的Rect.Bottom往上行间距一半</param>
    /// <param name="APageDataFmtTop">页数据Top</param>
    /// <param name="APageDataFmtBottom">页数据Bottom</param>
    /// <param name="AStartSeat">开始计算分页位置</param>
    /// <param name="ABreakSeat">需要分页位置</param>
    /// <param name="AFmtOffset">为了避开分页位置整体向下偏移的高度</param>
    /// <param name="AFmtHeightInc">为了避开分页位置高度增加值</param>
    procedure CheckFormatPageBreak(const APageIndex, ADrawItemRectTop,
      ADrawItemRectBottom, APageDataFmtTop, APageDataFmtBottom, AStartSeat: Integer;
      var ABreakSeat, AFmtOffset, AFmtHeightInc: Integer); virtual;

    function InsertItem(const AItem: THCCustomItem): Boolean; virtual;
    function InsertText(const AText: string): Boolean; virtual;
    function InsertGraphic(const AGraphic: TGraphic; const ANewPara: Boolean): Boolean; virtual;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;

    /// <summary> “理论”上全选中(适用于仅RectItem点击选中情况下的选中判断) </summary>
    function IsSelectComplateTheory: Boolean; virtual;

    function SelectExists: Boolean; virtual;

    /// <summary> 当前位置开始查找指定的内容 </summary>
    /// <param name="AKeyword">要查找的关键字</param>
    /// <param name="AForward">True：向前，False：向后</param>
    /// <param name="AMatchCase">True：区分大小写，False：不区分大小写</param>
    /// <returns>True：找到</returns>
    function Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean; virtual;
    procedure Clear; virtual;

    /// <summary> 返回指定位置处的顶层Data(为松耦合请返回TCustomData类型) </summary>
    function GetTopLevelDataAt(const X, Y: Integer): THCCustomData; virtual;
    function GetTopLevelData: THCCustomData; virtual;
    /// <summary> 当前RectItem是否有需要处理的Data(为松耦合请返回TCustomData类型) </summary>
    function GetActiveData: THCCustomData; virtual;
    procedure FormatDirty; virtual;
    procedure TraverseItem(const ATraverse: THCItemTraverse); virtual;
    function SaveToBitmap(var ABitmap: TBitmap): Boolean; virtual;
    //
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function BreakByOffset(const AOffset: Integer): THCCustomItem; override;
    function CanConcatItems(const AItem: THCCustomItem): Boolean; override;
    procedure Assign(Source: THCCustomItem); override;

    // 撤销重做相关方法
    procedure Undo(const AUndoAction: THCCustomUndoAction); override;
    procedure Redo(const ARedoAction: THCCustomUndoAction); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function ToHtml(const APath: string): string; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    function GetLength: Integer; override;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property TextWrapping: Boolean read FTextWrapping write FTextWrapping;  // 文本环绕
    property SizeChanged: Boolean read FSizeChanged write SetSizeChanged;

    /// <summary> 在当前页显示不下时是否可以分页截断显示 </summary>
    property CanPageBreak: Boolean read FCanPageBreak write FCanPageBreak;
    property OwnerData: THCCustomData read FOwnerData;
    property MangerUndo: Boolean read FMangerUndo;
  end;

  THCDomainItemClass = class of THCDomainItem;

  THCDomainItem = class(THCCustomRectItem)  // 域
  private
    FLevel: Byte;  // 域嵌套层级关系，最外为0，其内为1
    FDrawRect: TRect;
    FMarkType: TMarkType;
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    class function IsBeginMark(const AItem: THCCustomItem): Boolean;
    class function IsEndMark(const AItem: THCCustomItem): Boolean;
    function GetOffsetAt(const X: Integer): Integer; override;
    function JustifySplit: Boolean; override;
    // 当前RectItem格式化时所属的Data(为松耦合请传入TCustomRichData类型)
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure PaintTop(const ACanvas: TCanvas); override;
    function SaveToBitmap(var ABitmap: TBitmap): Boolean; override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property MarkType: TMarkType read FMarkType write FMarkType;
    property Level: Byte read FLevel write FLevel;
  end;

  THCTextRectItem = class(THCCustomRectItem)  // 带文本样式的RectItem
  private
    FTextStyleNo: Integer;
  protected
    procedure SetTextStyleNo(const Value: Integer); virtual;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    function JustifySplit: Boolean; override;
    procedure ApplySelectTextStyle(const AStyle: THCStyle;
      const AMatchStyle: THCStyleMatch); override;
    procedure MarkStyleUsed(const AMark: Boolean); override;
    function SelectExists: Boolean; override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property TextStyleNo: Integer read FTextStyleNo write SetTextStyleNo;
  end;

  THCControlItem = class(THCTextRectItem)
  private
    FAutoSize: Boolean;  // 是根据内容自动大小，还是外部指定大小
  protected
    FPaddingLeft, FPaddingRight, FPaddingTop, FPaddingBottom: Byte;
    FMinWidth, FMinHeight: Integer;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
  end;

  TGripType = (gtNone, gtLeftTop, gtRightTop, gtLeftBottom, gtRightBottom,
    gtLeft, gtTop, gtRight, gtBottom);

  THCResizeRectItem = class(THCCustomRectItem)  // 可改变大小的RectItem
  private
    FGripSize: Word;  // 拖动块大小
    FResizing: Boolean;  // 正在拖动改变大小
    FCanResize: Boolean;  // 当前是否处于可改变大小状态
    FResizeGrip: TGripType;
    FResizeRect: TRect;
    FResizeWidth, FResizeHeight: Integer;  // 缩放后的宽、高
    function GetGripType(const X, Y: Integer): TGripType;
  protected
    FResizeX, FResizeY: Integer;  // 拖动缩放时起始位置
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;

    // 撤销恢复相关方法
    procedure SelfUndo_Resize(const ANewWidth, ANewHeight: Integer);
    procedure DoSelfUndoDestroy(const AUndo: THCUndo); override;
    procedure DoSelfUndo(const AUndo: THCUndo); override;
    procedure DoSelfRedo(const ARedo: THCUndo); override;

    function GetResizing: Boolean; virtual;
    procedure SetResizing(const Value: Boolean); virtual;
    property ResizeGrip: TGripType read FResizeGrip;
    property ResizeRect: TRect read FResizeRect;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    /// <summary> 获取坐标X、Y是否在选中区域中 </summary>
    function CoordInSelect(const X, Y: Integer): Boolean; override;
    procedure PaintTop(const ACanvas: TCanvas); override;
    // 继承THCCustomItem抽象方法
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function CanDrag: Boolean; override;

    /// <summary> 更新光标位置 </summary>
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    function SelectExists: Boolean; override;

    /// <summary> 约束到指定大小范围内 </summary>
    procedure RestrainSize(const AWidth, AHeight: Integer); virtual;
    property GripSize: Word read FGripSize write FGripSize;
    property Resizing: Boolean read GetResizing write SetResizing;
    property ResizeWidth: Integer read FResizeWidth;
    property ResizeHeight: Integer read FResizeHeight;
    property CanResize: Boolean read FCanResize write FCanResize;
  end;

  THCAnimateRectItem = class(THCCustomRectItem)  // 动画RectItem
  public
    function GetOffsetAt(const X: Integer): Integer; override;
  end;

  THCDataItem = class(THCResizeRectItem)  // 内部有管理Data的Item，也便于统一判断是否进入RectItem内部判断或寻找
  { to do: 将不是所有RectItem用到的方法移植到这里来}
  end;

var
  HCDefaultDomainItemClass: THCDomainItemClass = THCDomainItem;

implementation

uses
  SysUtils;

{ THCCustomRectItem }

procedure THCCustomRectItem.ApplyContentAlign(const AAlign: THCContentAlign);
begin
end;

procedure THCCustomRectItem.ApplySelectParaStyle(const AStyle: THCStyle;
  const AMatchStyle: THCParaMatch);
begin
end;

procedure THCCustomRectItem.ApplySelectTextStyle(const AStyle: THCStyle;
  const AMatchStyle: THCStyleMatch);
begin
end;

procedure THCCustomRectItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FWidth := (Source as THCCustomRectItem).Width;
  FHeight := (Source as THCCustomRectItem).Height;
end;

function THCCustomRectItem.BreakByOffset(const AOffset: Integer): THCCustomItem;
begin
  Result := nil;
end;

function THCCustomRectItem.CanConcatItems(const AItem: THCCustomItem): Boolean;
begin
  Result := False;
end;

procedure THCCustomRectItem.CheckFormatPageBreak(const APageIndex, ADrawItemRectTop,
  ADrawItemRectBottom, APageDataFmtTop, APageDataFmtBottom, AStartSeat: Integer;
  var ABreakSeat, AFmtOffset, AFmtHeightInc: Integer);
begin
  ABreakSeat := -1;
  AFmtOffset := 0;
  AFmtHeightInc := 0;

  if FCanPageBreak then  // 可分页显示，当前页保留一部分
  begin
    ABreakSeat := Height - AStartSeat - (APageDataFmtBottom - ADrawItemRectTop);
    if ADrawItemRectBottom > APageDataFmtBottom then
      AFmtHeightInc := APageDataFmtBottom - ADrawItemRectBottom;
  end
  else  // 不分页显示，偏移到下一页从头显示
  begin
    ABreakSeat := 0;
    if ADrawItemRectBottom > APageDataFmtBottom then
      AFmtOffset := APageDataFmtBottom - ADrawItemRectTop;
  end;
end;

procedure THCCustomRectItem.CheckFormatPageBreakBefor;
begin
end;

function THCCustomRectItem.CoordInSelect(const X, Y: Integer): Boolean;
begin
  Result := False;
end;

constructor THCCustomRectItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create;
  FOwnerData := AOwnerData;
  Self.ParaNo := AOwnerData.CurParaNo;
  FOnGetMainUndoList := (AOwnerData as THCCustomData).OnGetUndoList;
  FWidth := 100;   // 默认尺寸
  FHeight := 50;
  FTextWrapping := False;
  FSizeChanged := False;
  FCanPageBreak := False;
  FMangerUndo := False;
end;

constructor THCCustomRectItem.Create(const AOwnerData: THCCustomData; const AWidth, AHeight: Integer);
begin
  Create(AOwnerData);
  Width := AWidth;
  Height := AHeight;
end;

procedure THCCustomRectItem.DeleteActiveDataItems(const AStartNo, AEndNo: Integer;
  const AKeepPara: Boolean);
begin
end;

function THCCustomRectItem.DeleteActiveDomain: Boolean;
begin
  Result := False;
end;

function THCCustomRectItem.DeleteSelected: Boolean;
begin
  Result := False;
end;

function THCCustomRectItem.DoSelfUndoNew: THCUndo;
begin
  // Sender.Data可绑定自定义的对象
  Result := THCUndo.Create;
end;

procedure THCCustomRectItem.DoSizeChanged;
begin
  FormatDirty;
end;

procedure THCCustomRectItem.DoSelfRedo(const ARedo: THCUndo);
begin
end;

procedure THCCustomRectItem.DoSelfUndo(const AUndo: THCUndo);
begin
end;

procedure THCCustomRectItem.DoSelfUndoDestroy(const AUndo: THCUndo);
begin
  if Assigned(AUndo.Data) then
  begin
    AUndo.Data.Free;
    AUndo.Data := nil;
  end;
end;

procedure THCCustomRectItem.FormatDirty;
begin
end;

procedure THCCustomRectItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
begin
end;

procedure THCCustomRectItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FWidth := ANode.Attributes['width'];
  FHeight := ANode.Attributes['height'];
end;

function THCCustomRectItem.GetActiveData: THCCustomData;
begin
  Result := nil;
end;

function THCCustomRectItem.GetTopLevelDrawItem: THCCustomDrawItem;
begin
  Result := nil;
end;

function THCCustomRectItem.GetTopLevelDrawItemCoord: TPoint;
begin
  Result := Point(-1, -1);
end;

function THCCustomRectItem.GetActiveItem: THCCustomItem;
begin
  Result := Self;
end;

procedure THCCustomRectItem.GetCaretInfo(var ACaretInfo: THCCaretInfo);
begin
end;

procedure THCCustomRectItem.Clear;
begin
end;

function THCCustomRectItem.ClearFormatExtraHeight: Integer;
begin
  Result := 0;
end;

function THCCustomRectItem.GetHeight: Integer;
begin
  Result := FHeight;
end;

function THCCustomRectItem.GetLength: Integer;
begin
  Result := 1;
end;

function THCCustomRectItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X <= 0 then
    Result := OffsetBefor
  else
  if X >= Width then
    Result := OffsetAfter
  else
    Result := OffsetInner;
end;

function THCCustomRectItem.GetTopLevelData: THCCustomData;
begin
  Result := nil;
end;

function THCCustomRectItem.GetTopLevelDataAt(const X, Y: Integer): THCCustomData;
begin
  Result := nil;
end;

function THCCustomRectItem.GetTopLevelItem: THCCustomItem;
begin
  Result := Self;
end;

function THCCustomRectItem.GetTopLevelRectDrawItem: THCCustomDrawItem;
begin
  Result := nil;
end;

function THCCustomRectItem.GetTopLevelRectDrawItemCoord: TPoint;
begin
  Result := Point(-1, -1);
end;

function THCCustomRectItem.GetSelfUndoList: THCUndoList;
var
  vItemAction: THCItemSelfUndoAction;
begin
  Result := nil;
  if not Assigned(FOnGetMainUndoList) then Exit;

  Result := FOnGetMainUndoList;
  if Assigned(Result) and Result.Enable and (Result.Last.Actions.Last is THCItemSelfUndoAction) then
  begin
    vItemAction := Result.Last.Actions.Last as THCItemSelfUndoAction;
    if not Assigned(vItemAction.&Object) then
    begin
      vItemAction.&Object := THCUndoList.Create;
      SelfUndoListInitializate(vItemAction.&Object as THCUndoList);
    end;

    Result := vItemAction.&Object as THCUndoList;
  end;
end;

//procedure THCCustomRectItem.GetPageFmtBottomInfo(const AHeight: Integer;
//  var ADItemMostBottom: Integer);
//begin
//end;

function THCCustomRectItem.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure THCCustomRectItem.SelfUndoListInitializate(
  const AUndoList: THCUndoList);
begin
  AUndoList.OnUndoNew := DoSelfUndoNew;
  AUndoList.OnUndo := DoSelfUndo;
  AUndoList.OnRedo := DoSelfRedo;
  AUndoList.OnUndoDestroy := DoSelfUndoDestroy;
end;

function THCCustomRectItem.InsertGraphic(const AGraphic: TGraphic;
  const ANewPara: Boolean): Boolean;
begin
end;

function THCCustomRectItem.InsertItem(const AItem: THCCustomItem): Boolean;
begin
end;

function THCCustomRectItem.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
begin
end;

function THCCustomRectItem.InsertText(const AText: string): Boolean;
begin
end;

function THCCustomRectItem.IsSelectComplateTheory: Boolean;
begin
  Result := IsSelectComplate or Active;
end;

function THCCustomRectItem.JustifySplit: Boolean;
begin
  Result := True;
end;

procedure THCCustomRectItem.KeyDown(var Key: Word; Shift: TShiftState);
begin
  Key := 0;
end;

procedure THCCustomRectItem.KeyPress(var Key: Char);
begin
  Key := #0
end;

procedure THCCustomRectItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FWidth, SizeOf(FWidth));
  AStream.ReadBuffer(FHeight, SizeOf(FHeight));
  FormatDirty;
end;

procedure THCCustomRectItem.MarkStyleUsed(const AMark: Boolean);
begin
end;

function THCCustomRectItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer): Boolean;
begin
  Self.Active := PtInRect(Rect(0, 0, FWidth, FHeight), Point(X, Y));
  Result := Self.Active;
end;

procedure THCCustomRectItem.ActiveItemReAdaptEnvironment;
begin
end;

procedure THCCustomRectItem.Redo(const ARedoAction: THCCustomUndoAction);
var
  vUndoList: THCUndoList;
begin
  if ARedoAction is THCItemSelfUndoAction then
  begin
    vUndoList := (ARedoAction as THCItemSelfUndoAction).&Object as THCUndoList;
    if Assigned(vUndoList) then
    begin
      if vUndoList.Seek < 0 then  //
        SelfUndoListInitializate(vUndoList);  // 插入RectItem撤销后会删除，再恢复是从流加载的新实例，相关事件已经名存实亡了，需要重新赋值
      vUndoList.Redo;
    end
    else
      inherited Redo(ARedoAction);
  end
  else
    inherited Redo(ARedoAction);
end;

procedure THCCustomRectItem.ReFormatActiveItem;
begin
end;

procedure THCCustomRectItem.SaveSelectToStream(const AStream: TStream);
begin
end;

function THCCustomRectItem.SaveSelectToText: string;
begin
  Result := '';
end;

function THCCustomRectItem.SaveToBitmap(var ABitmap: TBitmap): Boolean;
var
  vPaintInfo: TPaintInfo;
begin
  Result := False;

  if (FWidth = 0) or (FHeight = 0) then Exit;

  ABitmap.SetSize(FWidth, FHeight);

  vPaintInfo := TPaintInfo.Create;
  try
    vPaintInfo.Print := True;
    vPaintInfo.WindowWidth := ABitmap.Width;
    vPaintInfo.WindowHeight := ABitmap.Height;
    vPaintInfo.ScaleX := 1;
    vPaintInfo.ScaleY := 1;
    vPaintInfo.Zoom := 1;
    Self.DoPaint(OwnerData.Style, Rect(0, 0, ABitmap.Width, ABitmap.Height),
      0, ABitmap.Height, 0, ABitmap.Height, ABitmap.Canvas, vPaintInfo);
  finally
    vPaintInfo.Free;
  end;

  Result := True;
end;

procedure THCCustomRectItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FWidth, SizeOf(FWidth));
  AStream.WriteBuffer(FHeight, SizeOf(FHeight));
end;

function THCCustomRectItem.Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean;
begin
  Result := False;
end;

function THCCustomRectItem.SelectExists: Boolean;
begin
  Result := False;
end;

procedure THCCustomRectItem.SetActiveItemText(const AText: string);
begin
end;

procedure THCCustomRectItem.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure THCCustomRectItem.SetSizeChanged(const Value: Boolean);
begin
  if FSizeChanged <> Value then
  begin
    FSizeChanged := Value;
    DoSizeChanged;
  end;
end;

procedure THCCustomRectItem.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

function THCCustomRectItem.ToHtml(const APath: string): string;
var
  vFileName: string;
  vBitmap: TBitmap;
begin
  Result := '';
  vBitmap := TBitmap.Create;
  try
    if not SaveToBitmap(vBitmap) then Exit;

    if APath <> '' then  // 保存到指定的文件夹中
    begin
      if not DirectoryExists(APath + 'images') then
        CreateDir(APath + 'images');
      vFileName := OwnerData.Style.GetHtmlFileTempName + '.bmp';
      vBitmap.SaveToFile(APath + 'images\' + vFileName);

      Result := '<img width="' + IntToStr(FWidth) + '" height="' + IntToStr(FHeight)
        + '" src="images/' + vFileName + '" alt="' + Self.ClassName + '" />';
    end
    else  // 保存为Base64
    begin
      Result := '<img width="' + IntToStr(FWidth) + '" height="' + IntToStr(FHeight)
        + '" src="data:img/jpg;base64,' + GraphicToBase64(vBitmap) + '" alt="THCImageItem" />';
    end;
  finally
    FreeAndNil(vBitmap);
  end;
end;

procedure THCCustomRectItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['width'] := FWidth;
  ANode.Attributes['height'] := FHeight;
end;

procedure THCCustomRectItem.TraverseItem(const ATraverse: THCItemTraverse);
begin
end;

procedure THCCustomRectItem.Undo(const AUndoAction: THCCustomUndoAction);
var
  vUndoList: THCUndoList;
begin
  if AUndoAction is THCItemSelfUndoAction then
  begin
    vUndoList := (AUndoAction as THCItemSelfUndoAction).&Object as THCUndoList;
    if Assigned(vUndoList) then
      vUndoList.Undo
    else
      inherited Undo(AUndoAction);
  end
  else
    inherited Undo(AUndoAction);
end;

procedure THCCustomRectItem.SelfUndo_New;
var
  vUndoList: THCUndoList;
begin
  vUndoList := GetSelfUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
    vUndoList.UndoNew;
end;

function THCCustomRectItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := False;
end;

{ THCResizeRectItem }

function THCResizeRectItem.CanDrag: Boolean;
begin
  Result := not FResizing;
end;

function THCResizeRectItem.CoordInSelect(const X, Y: Integer): Boolean;
begin
  Result := SelectExists and PtInRect(Bounds(0, 0, Width, Height), Point(X, Y))
    and (GetGripType(X, Y) = gtNone);
end;

constructor THCResizeRectItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  FCanResize := True;
  FGripSize := 8;
end;

procedure THCResizeRectItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited;

  if (not APaintInfo.Print) and Active then  // 激活状态，绘制焦点框、锚点
  begin
    if Resizing then
    begin
      case FResizeGrip of
        gtLeftTop:
          FResizeRect := Bounds(ADrawRect.Left + Width - FResizeWidth,
            ADrawRect.Top + Height - FResizeHeight, FResizeWidth, FResizeHeight);

        gtRightTop:
          FResizeRect := Bounds(ADrawRect.Left,
            ADrawRect.Top + Height - FResizeHeight, FResizeWidth, FResizeHeight);

        gtLeftBottom:
          FResizeRect := Bounds(ADrawRect.Left + Width - FResizeWidth,
            ADrawRect.Top, FResizeWidth, FResizeHeight);

        gtRightBottom:
          FResizeRect := Bounds(ADrawRect.Left, ADrawRect.Top, FResizeWidth, FResizeHeight);
      end;

      APaintInfo.TopItems.Add(Self);
    end;

    // 绘制缩放拖动提示锚点
    ACanvas.Brush.Color := clGray;
    ACanvas.FillRect(Bounds(ADrawRect.Left, ADrawRect.Top, GripSize, GripSize));
    ACanvas.FillRect(Bounds(ADrawRect.Right - GripSize, ADrawRect.Top, GripSize, GripSize));
    ACanvas.FillRect(Bounds(ADrawRect.Left, ADrawRect.Bottom - GripSize, GripSize, GripSize));
    ACanvas.FillRect(Bounds(ADrawRect.Right - GripSize, ADrawRect.Bottom - GripSize, GripSize, GripSize));
  end;
end;

procedure THCResizeRectItem.DoSelfRedo(const ARedo: THCUndo);
var
  vSizeUndoData: THCSizeUndoData;
begin
  if ARedo.Data is THCSizeUndoData then
  begin
    vSizeUndoData := ARedo.Data as THCSizeUndoData;
    Self.Width := vSizeUndoData.NewWidth;
    Self.Height := vSizeUndoData.NewHeight;
  end
  else
    inherited DoSelfRedo(ARedo);
end;

procedure THCResizeRectItem.DoSelfUndo(const AUndo: THCUndo);
var
  vSizeUndoData: THCSizeUndoData;
begin
  if AUndo.Data is THCSizeUndoData then
  begin
    vSizeUndoData := AUndo.Data as THCSizeUndoData;
    Self.Width := vSizeUndoData.OldWidth;
    Self.Height := vSizeUndoData.OldHeight;
  end
  else
    inherited DoSelfUndo(AUndo);
end;

procedure THCResizeRectItem.DoSelfUndoDestroy(const AUndo: THCUndo);
begin
  if Assigned(AUndo.Data) and (AUndo.Data is THCSizeUndoData) then
  begin
    (AUndo.Data as THCSizeUndoData).Free;
    AUndo.Data := nil;
  end;

  inherited DoSelfUndoDestroy(AUndo);
end;

procedure THCResizeRectItem.GetCaretInfo(var ACaretInfo: THCCaretInfo);
begin
  if Self.Active then
    ACaretInfo.Visible := False;
end;

function THCResizeRectItem.GetGripType(const X, Y: Integer): TGripType;
var
  vPt: TPoint;
begin
  vPt := Point(X, Y);
  if PtInRect(Bounds(0, 0, GripSize, GripSize), vPt) then
    Result := gtLeftTop
  else
  if PtInRect(Bounds(Width - GripSize, 0, GripSize, GripSize), vPt) then
    Result := gtRightTop
  else
  if PtInRect(Bounds(0, Height - GripSize, GripSize, GripSize), vPt) then
    Result := gtLeftBottom
  else
  if PtInRect(Bounds(Width - GripSize, Height - GripSize, GripSize, GripSize), vPt) then
    Result := gtRightBottom
  else
    Result := gtNone;
end;

function THCResizeRectItem.GetResizing: Boolean;
begin
  Result := FResizing;
end;

function THCResizeRectItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  FResizeGrip := gtNone;
  Result := inherited MouseDown(Button, Shift, X, Y);
  if Active then
  begin
    FResizeGrip := GetGripType(X, Y);
    FResizing := FResizeGrip <> gtNone;
    if FResizing then
    begin
      FResizeX := X;
      FResizeY := Y;
      FResizeWidth := Width;
      FResizeHeight := Height;
    end;
  end;
end;

function THCResizeRectItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  vW, vH, vTempW, vTempH: Integer;
  vBL: Single;
begin
  Result := inherited MouseMove(Shift, X, Y);
  GCursor := crDefault;
  if Active then
  begin
    if FResizing then  // 正在缩放中
    begin
      vBL := Width / Height;
      vW := X - FResizeX;
      vH := Y - FResizeY;

      // 根据缩放位置在对角线的不同方位计算长宽
      case FResizeGrip of
        gtLeftTop:
          begin
            vTempW := Round(vH * vBL);
            vTempH := Round(vW / vBL);
            if vTempW > vW then
              vH := vTempH
            else
              vW := vTempW;

            FResizeWidth := Width - vW;
            FResizeHeight := Height - vH;
          end;

        gtRightTop:
          begin
            vTempW := Abs(Round(vH * vBL));
            vTempH := Abs(Round(vW / vBL));

            if vW < 0 then
            begin
              if vH > vTempH then
                vH := vTempH
              else
              if vH > 0 then
                vW := -vTempW
              else
                vW := vTempW;
            end
            else
            begin
              if -vH < vTempH then
                vH := -vTempH
              else
                vW := vTempW;
            end;

            FResizeWidth := Width + vW;
            FResizeHeight := Height - vH;
          end;

        gtLeftBottom:
          begin
            vTempW := Abs(Round(vH * vBL));
            vTempH := Abs(Round(vW / vBL));

            if vW < 0 then  // 左侧
            begin
              if vH < vTempH then  // 对角线上面，纵向以横向为准
                vH := vTempH
              else  // 对角线下面，横向以纵向为准
                vW := -vTempW;
            end
            else  // 右侧
            begin
              if (vW > vTempW) or (vH > vTempH) then  // 对角线下面，横向以纵向为准
              begin
                if vH < 0 then
                  vW := vTempW
                else
                  vW := -vTempW;
              end
              else  // 对角线上面，纵向以横向为准
                vH := -vTempH;
            end;

            FResizeWidth := Width - vW;
            FResizeHeight := Height + vH;
          end;

        gtRightBottom:
          begin
            vTempW := Round(vH * vBL);
            vTempH := Round(vW / vBL);
            if vTempW > vW then
              vW := vTempW
            else
              vH := vTempH;

            FResizeWidth := Width + vW;
            FResizeHeight := Height + vH;
          end;
      end;
    end
    else  // 非缩放中
    begin
      case GetGripType(X, Y) of
        gtLeftTop, gtRightBottom:
          GCursor := crSizeNWSE;

        gtRightTop, gtLeftBottom:
          GCursor := crSizeNESW;

        gtLeft, gtRight:
          GCursor := crSizeWE;

        gtTop, gtBottom:
          GCursor := crSizeNS;
      end;
    end;
  end;
end;

function THCResizeRectItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := inherited MouseUp(Button, Shift, X, Y);
  if FResizing then
  begin
    if (FResizeWidth < 0) or (FResizeHeight < 0) then
    begin
      FResizing := False;
      Exit;
    end;

    SelfUndo_Resize(FResizeWidth, FResizeHeight);
    Width := FResizeWidth;
    Height := FResizeHeight;
    FResizing := False;
  end;
end;

procedure THCResizeRectItem.PaintTop(const ACanvas: TCanvas);
begin
  inherited PaintTop(ACanvas);
  if FResizing then
  begin
    //ACanvas.DrawFocusRect(ADrawRect);  // 焦点框，为啥画不出来呢
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(FResizeRect);
    ACanvas.Brush.Color := clWhite;
    ACanvas.Font.Size := 8;
    ACanvas.Font.Color := clBlack;
    ACanvas.Font.Style := [];
    ACanvas.TextOut(FResizeRect.Left + 2, FResizeRect.Top + 2,
      IntToStr(FResizeWidth) + ' x ' + IntToStr(FResizeHeight));
  end;
end;

procedure THCResizeRectItem.RestrainSize(const AWidth, AHeight: Integer);
begin
end;

function THCResizeRectItem.SelectExists: Boolean;
begin
  Result := IsSelectComplateTheory;
end;

procedure THCResizeRectItem.SetResizing(const Value: Boolean);
begin
  if FResizing <> Value then
    FResizing := Value;
end;

procedure THCResizeRectItem.SelfUndo_Resize(const ANewWidth, ANewHeight: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vSizeUndoData: THCSizeUndoData;
begin
  vUndoList := GetSelfUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    SelfUndo_New;
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vSizeUndoData := THCSizeUndoData.Create;
      vSizeUndoData.OldWidth := Self.Width;
      vSizeUndoData.OldHeight := Self.Height;
      vSizeUndoData.NewWidth := ANewWidth;
      vSizeUndoData.NewHeight := ANewHeight;

      vUndo.Data := vSizeUndoData;
    end;
  end;
end;

{ THCTextRectItem }

procedure THCTextRectItem.ApplySelectTextStyle(const AStyle: THCStyle;
  const AMatchStyle: THCStyleMatch);
begin
  FTextStyleNo := AMatchStyle.GetMatchStyleNo(AStyle, FTextStyleNo);
end;

procedure THCTextRectItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FTextStyleNo := (Source as THCTextRectItem).TextStyleNo;
end;

constructor THCTextRectItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  if AOwnerData.CurStyleNo > THCStyle.Null then
    FTextStyleNo := AOwnerData.CurStyleNo
  else
    FTextStyleNo := 0;
end;

procedure THCTextRectItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FTextStyleNo := ANode.Attributes['textsno'];
end;

function THCTextRectItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X < Width div 2 then
    Result := OffsetBefor
  else
    Result := OffsetAfter;
end;

function THCTextRectItem.JustifySplit: Boolean;
begin
  Result := False;
end;

procedure THCTextRectItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FTextStyleNo, SizeOf(FTextStyleNo));

  if Assigned(AStyle) and (FTextStyleNo > AStyle.TextStyles.Count - 1) then  // 兼容历史错误(删除多余样式时没有)
    FTextStyleNo := 0;
end;

procedure THCTextRectItem.MarkStyleUsed(const AMark: Boolean);
begin
  if AMark then  // 标记
    OwnerData.Style.TextStyles[FTextStyleNo].CheckSaveUsed := True
  else  // 重新赋值
    FTextStyleNo := OwnerData.Style.TextStyles[FTextStyleNo].TempNo;
end;

procedure THCTextRectItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FTextStyleNo, SizeOf(FTextStyleNo));
end;

function THCTextRectItem.SelectExists: Boolean;
begin
  Result := GetSelectComplate;
end;

procedure THCTextRectItem.SetTextStyleNo(const Value: Integer);
begin
  if FTextStyleNo <> Value then
    FTextStyleNo := Value;
end;

procedure THCTextRectItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['textsno'] := FTextStyleNo;
end;

{ THCAnimateRectItem }

function THCAnimateRectItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X < Width div 2 then
    Result := OffsetBefor
  else
    Result := OffsetAfter;
end;

{ THCControlItem }

procedure THCControlItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FAutoSize := (Source as THCControlItem).AutoSize;
end;

constructor THCControlItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  FAutoSize := True;
  FPaddingLeft := 5;
  FPaddingRight := 5;
  FPaddingTop := 5;
  FPaddingBottom := 5;
  FMinWidth := 20;
  FMinHeight := 10;
end;

procedure THCControlItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FAutoSize := ANode.Attributes['autosize'];
end;

procedure THCControlItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FAutoSize, SizeOf(FAutoSize));
end;

procedure THCControlItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FAutoSize, SizeOf(FAutoSize));
end;

procedure THCControlItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['autosize'] := FAutoSize;
end;

{ THCDomainItem }

constructor THCDomainItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.Domain;
  FLevel := 0;
  Width := 0;
  Height := AOwnerData.Style.TextStyles[0].FontHeight;
end;

procedure THCDomainItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then  // 绘制[和]
  begin
    FDrawRect := ADrawRect;
    APaintInfo.TopItems.Add(Self);
  end;
end;

procedure THCDomainItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vItem: THCCustomItem;
begin
  Self.Width := 0;
  Self.Height := ARichData.Style.TextStyles[0].FontHeight;;  // 默认大小
  if FMarkType = TMarkType.cmtBeg then  // 域起始标识
  begin
    if AItemNo < ARichData.Items.Count - 1 then  // 插入时可能是在Data最后面插入起始，后面不一定有结束
    begin
      vItem := ARichData.Items[AItemNo + 1];
      if (vItem.StyleNo = Self.StyleNo)  // 下一个是域标识
        and ((vItem as THCDomainItem).MarkType = TMarkType.cmtEnd)  // 下一个是结束标识
      then
        Self.Width := 10  // 增加宽度以便输入时光标可方便点击
      else
      if vItem.ParaFirst then  // 下一个是段首，我是段尾
        Self.Width := 10  // 增加宽度以便输入时光标可方便点击
      else
      if vItem.StyleNo > THCStyle.Null then  // 后面是文本，跟随后面的高度
        Self.Height := ARichData.Style.TextStyles[vItem.StyleNo].FontHeight;
    end
    else
      Self.Width := 10;
  end
  else  // 域结束标识
  begin
    vItem := ARichData.Items[AItemNo - 1];  // 前一个
    if (vItem.StyleNo = Self.StyleNo)
      and ((vItem as THCDomainItem).MarkType = TMarkType.cmtBeg)
    then  // 前一个是起始标识
      Self.Width := 10
    else
    if Self.ParaFirst then  // 结束标识是段首，增加宽度
      Self.Width := 10
    else
    if vItem.StyleNo > THCStyle.Null then  // 前面是文本，距离前面的高度
      Self.Height := ARichData.Style.TextStyles[vItem.StyleNo].FontHeight;
  end;
end;

function THCDomainItem.GetOffsetAt(const X: Integer): Integer;
begin
  if (X >= 0) and (X <= Width) then
  begin
    if FMarkType = cmtBeg then
      Result := OffsetAfter
    else
      Result := OffsetBefor;
  end
  else
    Result := inherited GetOffsetAt(X);
end;

class function THCDomainItem.IsBeginMark(const AItem: THCCustomItem): Boolean;
begin
  Result := (AItem is THCDomainItem) and ((AItem as THCDomainItem).MarkType = TMarkType.cmtBeg);
end;

class function THCDomainItem.IsEndMark(const AItem: THCCustomItem): Boolean;
begin
  Result := (AItem is THCDomainItem) and ((AItem as THCDomainItem).MarkType = TMarkType.cmtEnd);
end;

function THCDomainItem.JustifySplit: Boolean;
begin
  Result := False;
end;

procedure THCDomainItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FMarkType, SizeOf(FMarkType));
  if AFileVersion > 38 then
    AStream.ReadBuffer(FLevel, SizeOf(FLevel));
end;

procedure THCDomainItem.PaintTop(const ACanvas: TCanvas);
begin
  inherited PaintTop(ACanvas);

  ACanvas.Pen.Width := 1;
  if FMarkType = cmtBeg then
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clBlue;
    ACanvas.MoveTo(FDrawRect.Left + 2, FDrawRect.Top - 1);
    ACanvas.LineTo(FDrawRect.Left, FDrawRect.Top - 1);
    ACanvas.LineTo(FDrawRect.Left, FDrawRect.Bottom + 1);
    ACanvas.LineTo(FDrawRect.Left + 2, FDrawRect.Bottom + 1);
  end
  else
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clBlue;
    ACanvas.MoveTo(FDrawRect.Right - 2, FDrawRect.Top - 1);
    ACanvas.LineTo(FDrawRect.Right, FDrawRect.Top - 1);
    ACanvas.LineTo(FDrawRect.Right, FDrawRect.Bottom + 1);
    ACanvas.LineTo(FDrawRect.Right - 2, FDrawRect.Bottom + 1);
  end;
end;

procedure THCDomainItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FMarkType := TMarkType(ANode.Attributes['mark']);
end;

function THCDomainItem.SaveToBitmap(var ABitmap: TBitmap): Boolean;
begin
  Result := False;
end;

procedure THCDomainItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FMarkType, SizeOf(FMarkType));
  AStream.WriteBuffer(FLevel, SizeOf(FLevel));
end;

procedure THCDomainItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['mark'] := Ord(FMarkType);
  ANode.Attributes['level'] := FLevel;
end;

end.
