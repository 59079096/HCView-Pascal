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
  HCStyleMatch, HCStyle, HCCommon, HCCustomData, HCUndo;

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
    FSizeChanged: Boolean;
    FCanPageBreak: Boolean;  // 在当前页显示不下时是否可以分页截断显示
    FOnGetMainUndoList: TGetUndoListEvent;
  protected
    function GetWidth: Integer; virtual;
    procedure SetWidth(const Value: Integer); virtual;
    function GetHeight: Integer; virtual;
    procedure SetHeight(const Value: Integer); virtual;

    // 撤销重做相关方法
    procedure DoNewUndo(const Sender: THCUndo); virtual;
    procedure DoUndoDestroy(const Sender: THCUndo); virtual;
    procedure DoUndo(const Sender: THCUndo); virtual;
    procedure DoRedo(const Sender: THCUndo); virtual;
    procedure Undo_StartRecord;
    function GetSelfUndoList: THCUndoList;
  public
    /// <summary> 适用于工作期间创建 </summary>
    constructor Create(const AOwnerData: THCCustomData); overload; virtual;
    /// <summary> 适用于加载时创建 </summary>
    constructor Create(const AOwnerData: THCCustomData; const AWidth, AHeight: Integer); overload; virtual;
    // 抽象方法，供继承
    function ApplySelectTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch): Integer; virtual;
    procedure ApplySelectParaStyle(const AStyle: THCStyle; const AMatchStyle: THCParaMatch); virtual;

    // 当前RectItem格式化时所属的Data(为松耦合请传入TCustomRichData类型)
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); virtual;

    /// <summary> 清除并返回为处理分页比净高增加的高度(为重新格式化时后面计算偏移用) </summary>
    function ClearFormatExtraHeight: Integer; virtual;
    function DeleteSelected: Boolean; virtual;
    procedure MarkStyleUsed(const AMark: Boolean); virtual;
    procedure SaveSelectToStream(const AStream: TStream); virtual;
    function SaveSelectToText: string; virtual;
    function GetActiveItem: THCCustomItem; virtual;
    function GetActiveDrawItem: THCCustomDrawItem; virtual;
    function GetActiveDrawItemCoord: TPoint; virtual;
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

    // 变动是否在分页处
    function ChangeNearPageBreak: Boolean; virtual;

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

    /// <summary> 当前RectItem是否有需要处理的Data(为松耦合请返回TCustomRichData类型) </summary>
    function GetActiveData: THCCustomData; virtual;

    /// <summary> 返回指定位置处的顶层Data(为松耦合请返回TCustomRichData类型) </summary>
    function GetTopLevelDataAt(const X, Y: Integer): THCCustomData; virtual;

    procedure TraverseItem(const ATraverse: TItemTraverse); virtual;
    //
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function BreakByOffset(const AOffset: Integer): THCCustomItem; override;
    function CanConcatItems(const AItem: THCCustomItem): Boolean; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function GetLength: Integer; override;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property TextWrapping: Boolean read FTextWrapping write FTextWrapping;  // 文本环绕
    property SizeChanged: Boolean read FSizeChanged write FSizeChanged;

    /// <summary> 在当前页显示不下时是否可以分页截断显示 </summary>
    property CanPageBreak: Boolean read FCanPageBreak write FCanPageBreak;
    property OwnerData: THCCustomData read FOwnerData;
  end;

  THCDomainItemClass = class of THCDomainItem;

  THCDomainItem = class(THCCustomRectItem)  // 域
  private
    FLevel: Byte;
    FMarkType: TMarkType;
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    function JustifySplit: Boolean; override;
    // 当前RectItem格式化时所属的Data(为松耦合请传入TCustomRichData类型)
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    property MarkType: TMarkType read FMarkType write FMarkType;
    property Level: Byte read FLevel write FLevel;
  end;

  THCTextRectItem = class(THCCustomRectItem)  // 带文本样式的RectItem
  private
    FTextStyleNo: Integer;
  protected
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure SetTextStyleNo(const Value: Integer); virtual;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    function JustifySplit: Boolean; override;
    function ApplySelectTextStyle(const AStyle: THCStyle;
      const AMatchStyle: THCStyleMatch): Integer; override;
    function SelectExists: Boolean; override;
    property TextStyleNo: Integer read FTextStyleNo write SetTextStyleNo;
  end;

  THCControlItem = class(THCTextRectItem)
  private
    FAutoSize: Boolean;  // 是根据内容自动大小，还是外部指定大小
  protected
    FMargin: Byte;
    FMinWidth, FMinHeight: Integer;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    // 撤销恢复相关方法
    procedure Undo_Resize(const ANewWidth, ANewHeight: Integer);
    procedure DoUndoDestroy(const Sender: THCUndo); override;
    procedure DoUndo(const Sender: THCUndo); override;
    procedure DoRedo(const Sender: THCUndo); override;

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
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
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

var
  HCDefaultDomainItemClass: THCDomainItemClass = THCDomainItem;

implementation

{ THCCustomRectItem }

procedure THCCustomRectItem.ApplySelectParaStyle(const AStyle: THCStyle;
  const AMatchStyle: THCParaMatch);
begin
end;

function THCCustomRectItem.ApplySelectTextStyle(const AStyle: THCStyle;
  const AMatchStyle: THCStyleMatch): Integer;
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

function THCCustomRectItem.ChangeNearPageBreak: Boolean;
begin
  Result := False;  // 需求见 201810172235
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
  Self.ParaNo := AOwnerData.Style.CurParaNo;
  FOnGetMainUndoList := (AOwnerData as THCCustomData).OnGetUndoList;
  FWidth := 100;   // 默认尺寸
  FHeight := 50;
  FTextWrapping := False;
  FSizeChanged := False;
  FCanPageBreak := False;
end;

constructor THCCustomRectItem.Create(const AOwnerData: THCCustomData; const AWidth, AHeight: Integer);
begin
  Create(AOwnerData);
  Width := AWidth;
  Height := AHeight;
end;

function THCCustomRectItem.DeleteSelected: Boolean;
begin
  Result := False;
end;

procedure THCCustomRectItem.DoNewUndo(const Sender: THCUndo);
begin
  // Sender.Data可绑定自定义的对象
end;

procedure THCCustomRectItem.DoRedo(const Sender: THCUndo);
begin
end;

procedure THCCustomRectItem.DoUndo(const Sender: THCUndo);
begin
end;

procedure THCCustomRectItem.DoUndoDestroy(const Sender: THCUndo);
begin
  if Sender.Data <> nil then
    Sender.Data.Free;
end;

procedure THCCustomRectItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
begin
end;

function THCCustomRectItem.GetActiveData: THCCustomData;
begin
  Result := nil;
end;

function THCCustomRectItem.GetActiveDrawItem: THCCustomDrawItem;
begin
  Result := nil;
end;

function THCCustomRectItem.GetActiveDrawItemCoord: TPoint;
begin
  Result := Point(0, 0);
end;

function THCCustomRectItem.GetActiveItem: THCCustomItem;
begin
  Result := Self;
end;

procedure THCCustomRectItem.GetCaretInfo(var ACaretInfo: THCCaretInfo);
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

function THCCustomRectItem.GetTopLevelDataAt(const X, Y: Integer): THCCustomData;
begin
  Result := nil;
end;

function THCCustomRectItem.GetSelfUndoList: THCUndoList;
var
  vMainUndoList: THCUndoList;
  vItemAction: THCItemSelfUndoAction;
begin
  Result := nil;
  vMainUndoList := FOnGetMainUndoList;
  if vMainUndoList.Last.Actions.Last is THCItemSelfUndoAction then
  begin
    vItemAction := vMainUndoList.Last.Actions.Last as THCItemSelfUndoAction;
    if not Assigned(vItemAction.&Object) then
    begin
      vItemAction.&Object := THCUndoList.Create;
      (vItemAction.&Object as THCUndoList).OnNewUndo := DoNewUndo;
      (vItemAction.&Object as THCUndoList).OnUndo := DoUndo;
      (vItemAction.&Object as THCUndoList).OnRedo := DoRedo;
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
end;

procedure THCCustomRectItem.MarkStyleUsed(const AMark: Boolean);
begin
end;

procedure THCCustomRectItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Self.Active := PtInRect(Rect(0, 0, FWidth, FHeight), Point(X, Y));
end;

procedure THCCustomRectItem.SaveSelectToStream(const AStream: TStream);
begin
end;

function THCCustomRectItem.SaveSelectToText: string;
begin
  Result := '';
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

procedure THCCustomRectItem.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure THCCustomRectItem.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

procedure THCCustomRectItem.TraverseItem(const ATraverse: TItemTraverse);
begin
end;

procedure THCCustomRectItem.Undo_StartRecord;
begin
  if FOwnerData.Style.EnableUndo then
    GetSelfUndoList.NewUndo;
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

procedure THCResizeRectItem.DoRedo(const Sender: THCUndo);
var
  vSizeAction: THCUndoSize;
begin
  if Sender.Data is THCUndoSize then
  begin
    vSizeAction := Sender.Data as THCUndoSize;
    Self.Width := vSizeAction.NewWidth;
    Self.Height := vSizeAction.NewHeight;
  end
  else
    inherited DoRedo(Sender);
end;

procedure THCResizeRectItem.DoUndo(const Sender: THCUndo);
var
  vSizeAction: THCUndoSize;
begin
  if Sender.Data is THCUndoSize then
  begin
    vSizeAction := Sender.Data as THCUndoSize;
    Self.Width := vSizeAction.OldWidth;
    Self.Height := vSizeAction.OldHeight;
  end
  else
    inherited DoUndo(Sender);
end;

procedure THCResizeRectItem.DoUndoDestroy(const Sender: THCUndo);
begin
  if Sender.Data is THCUndoSize then
    (Sender.Data as THCUndoSize).Free;

  inherited DoUndoDestroy(Sender);
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

procedure THCResizeRectItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FResizeGrip := gtNone;
  inherited MouseDown(Button, Shift, X, Y);
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

procedure THCResizeRectItem.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vW, vH, vTempW, vTempH: Integer;
  vBL: Single;
begin
  inherited;
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

procedure THCResizeRectItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FResizing then
  begin
    FResizing := False;

    if (FResizeWidth < 0) or (FResizeHeight < 0) then Exit;

    Undo_Resize(FResizeWidth, FResizeHeight);
    Width := FResizeWidth;
    Height := FResizeHeight;
  end;
end;

procedure THCResizeRectItem.PaintTop(const ACanvas: TCanvas);
begin
  inherited;
  //ACanvas.DrawFocusRect(ADrawRect);  // 焦点框，为啥画不出来呢
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(FResizeRect);
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

procedure THCResizeRectItem.Undo_Resize(const ANewWidth, ANewHeight: Integer);
var
  vUndo: THCUndo;
  vUndoSize: THCUndoSize;
begin
  if OwnerData.Style.EnableUndo then
  begin
    Undo_StartRecord;
    vUndo := GetSelfUndoList.Last;
    if vUndo <> nil then
    begin
      vUndoSize := THCUndoSize.Create;
      vUndoSize.OldWidth := Self.Width;
      vUndoSize.OldHeight := Self.Height;
      vUndoSize.NewWidth := ANewWidth;
      vUndoSize.NewHeight := ANewHeight;

      vUndo.Data := vUndoSize;
    end;
  end;
end;

{ THCTextRectItem }

function THCTextRectItem.ApplySelectTextStyle(const AStyle: THCStyle;
  const AMatchStyle: THCStyleMatch): Integer;
begin
  FTextStyleNo := AMatchStyle.GetMatchStyleNo(AStyle, FTextStyleNo);
  Result := FTextStyleNo;
end;

procedure THCTextRectItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FTextStyleNo := (Source as THCTextRectItem).TextStyleNo;
end;

constructor THCTextRectItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  if AOwnerData.Style.CurStyleNo > THCStyle.Null then
    FTextStyleNo := AOwnerData.Style.CurStyleNo
  else
    FTextStyleNo := 0;
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
end;

procedure THCTextRectItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FTextStyleNo, SizeOf(FTextStyleNo));
end;

function THCTextRectItem.SelectExists: Boolean;
begin
  Result := ioSelectComplate in Options;
end;

procedure THCTextRectItem.SetTextStyleNo(const Value: Integer);
begin
  if FTextStyleNo <> Value then
    FTextStyleNo := Value;
end;

{ THCDomainItem }

constructor THCDomainItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.Domain;
  FLevel := 0;
  Width := 0;
  Height := 10;
end;

procedure THCDomainItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited;
  if not APaintInfo.Print then  // 绘制[和]
  begin
    if FMarkType = cmtBeg then
    begin
      ACanvas.Pen.Style := psSolid;
      ACanvas.Pen.Color := clActiveBorder;
      ACanvas.MoveTo(ADrawRect.Left + 2, ADrawRect.Top - 1);
      ACanvas.LineTo(ADrawRect.Left, ADrawRect.Top - 1);
      ACanvas.LineTo(ADrawRect.Left, ADrawRect.Bottom + 1);
      ACanvas.LineTo(ADrawRect.Left + 2, ADrawRect.Bottom + 1);
    end
    else
    begin
      ACanvas.Pen.Style := psSolid;
      ACanvas.Pen.Color := clActiveBorder;
      ACanvas.MoveTo(ADrawRect.Right - 2, ADrawRect.Top - 1);
      ACanvas.LineTo(ADrawRect.Right, ADrawRect.Top - 1);
      ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom + 1);
      ACanvas.LineTo(ADrawRect.Right - 2, ADrawRect.Bottom + 1);
    end;
  end;
end;

procedure THCDomainItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vItem: THCCustomItem;
begin
  Self.Width := 0;
  Self.Height := 5;  // 默认大小
  if Self.MarkType = TMarkType.cmtBeg then  // 域起始标识
  begin
    if AItemNo < ARichData.Items.Count - 1 then  // 插入时可能是在Data最后面插入起始，后面不一定有结束
    begin
      vItem := ARichData.Items[AItemNo + 1];
      if (vItem.StyleNo = Self.StyleNo)  // 下一个是组标识
        and ((vItem as THCDomainItem).MarkType = TMarkType.cmtEnd)  // 下一个是结束标识
      then
        Self.Width := 10  // 增加宽度以便输入时光标可方便点击
      else
      if vItem.StyleNo > THCStyle.Null then  // 后面是文本，跟随后面的高度
      begin
        ARichData.Style.TextStyles[vItem.StyleNo].ApplyStyle(ARichData.Style.DefCanvas);
        Self.Height := ARichData.Style.DefCanvas.TextExtent('H').cy;
      end;
    end
    else
      Self.Width := 10;
  end
  else  // 域结束标识
  begin
    vItem := ARichData.Items[AItemNo - 1];
    if (vItem.StyleNo = Self.StyleNo)
      and ((vItem as THCDomainItem).MarkType = TMarkType.cmtBeg)
    then  // 前一个是起始标识
      Self.Width := 10
    else
    if vItem.StyleNo > THCStyle.Null then  // 前面是文本，距离前面的高度
    begin
      ARichData.Style.TextStyles[vItem.StyleNo].ApplyStyle(ARichData.Style.DefCanvas);
      Self.Height := ARichData.Style.DefCanvas.TextExtent('H').cy;
    end;
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

function THCDomainItem.JustifySplit: Boolean;
begin
  Result := False;
end;

procedure THCDomainItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FMarkType, SizeOf(FMarkType));
end;

procedure THCDomainItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FMarkType, SizeOf(FMarkType));
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
  FMargin := 5;
  FMinWidth := 20;
  FMinHeight := 10;
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

end.
