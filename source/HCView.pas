{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                 文档内容按页呈现控件                  }
{                                                       }
{*******************************************************}

unit HCView;

interface

{-$DEFINE SYNPDF}

uses
  Windows, Classes, Controls, Graphics, Messages, HCStyle, HCCustomData, {$IFDEF SYNPDF}SynPdf,{$ENDIF}
  Generics.Collections, SysUtils, HCCommon, HCViewData, HCRichData, HCDrawItem,
  HCSection, HCScrollBar, HCRichScrollBar, HCStatusScrollBar, HCParaStyle,
  HCTextStyle, HCRectItem, HCTextItem, HCItem, HCCustomFloatItem, HCUndo,
  HCAnnotateData, HCSectionData;

type
  TLoadSectionProc = reference to procedure(const AFileVersion: Word);

  THCDrawAnnotate = class(THCDrawItemAnnotate)  // Data批注信息
  public
    Data: THCCustomData;
    Rect: TRect;  // 在批注区绘制时的区域
  end;

  THCDrawAnnotateDynamic = class(THCDrawAnnotate)
  public
    Title, Text: string;
  end;

  THCAnnotatePre = class(TObject)  // 管理显示出来的批注
  private
    FDrawRect: TRect;
    FCount: Integer;  // 批注总数
    FMouseIn, FVisible: Boolean;
    FDrawAnnotates: TObjectList<THCDrawAnnotate>;
    FActiveDrawAnnotateIndex: Integer;
    // 在批注区引起的需要重绘时触发，如鼠标移入移出批注区域的高亮切换
    FOnUpdateView: TNotifyEvent;

    function GetDrawCount: Integer;
    function GetDrawAnnotateAt(const X, Y: Integer): Integer; overload;
    function GetDrawAnnotateAt(const APoint: TPoint): Integer; overload;
    procedure DoUpdateView;
    procedure SetMouseIn(const Value: Boolean);
  protected
    property MouseIn: Boolean read FMouseIn write SetMouseIn;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary> 绘制批注 </summary>
    /// <param name="Sender"></param>
    /// <param name="APageRect"></param>
    /// <param name="ACanvas"></param>
    /// <param name="APaintInfo"></param>
    procedure PaintDrawAnnotate(const Sender: TObject; const APageRect: TRect;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);

    /// <summary> 有批注插入 </summary>
    procedure InsertDataAnnotate(const ADataAnnotate: THCDataAnnotate);

    /// <summary> 有批注移除 </summary>
    procedure RemoveDataAnnotate(const ADataAnnotate: THCDataAnnotate);

    /// <summary> 添加一个需要绘制的DrawAnnotate </summary>
    procedure AddDrawAnnotate(const ADrawAnnotate: THCDrawAnnotate);
    /// <summary> 重绘前清除上次绘制 </summary>
    procedure ClearDrawAnnotate;

    function ActiveAnnotate: THCDataAnnotate;

    /// <summary> 通过DrawAnnotate删除Annotate </summary>
    procedure DeleteDataAnnotateByDraw(const AIndex: Integer);
    procedure MouseDown(const X, Y: Integer);
    procedure MouseMove(const X, Y: Integer);
    property DrawCount: Integer read GetDrawCount;
    property DrawAnnotates: TObjectList<THCDrawAnnotate> read FDrawAnnotates;
    property Visible: Boolean read FVisible;// write FVisible;
    property Count: Integer read FCount;
    property DrawRect: TRect read FDrawRect;
    property ActiveDrawAnnotateIndex: Integer read FActiveDrawAnnotateIndex;
    property OnUpdateView: TNotifyEvent read FOnUpdateView write FOnUpdateView;
  end;

  TPaintEvent = procedure(const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;
  THCCopyPasteEvent = function(const AFormat: Word): Boolean of object;

  THCView = class(TCustomControl)
  private
    { Private declarations }
    FFileName, FPageNoFormat: string;
    FStyle: THCStyle;
    FSections: TObjectList<THCSection>;
    FUndoList: THCUndoList;
    FHScrollBar: THCStatusScrollBar;
    FVScrollBar: THCRichScrollBar;
    FDataBmp: TBitmap;  // 数据显示位图
    FActiveSectionIndex,
    FViewWidth, FViewHeight,
    FDisplayFirstSection, FDisplayLastSection: Integer;
    FUpdateCount: Cardinal;
    FPagePadding: Byte;
    FZoom: Single;
    FAutoZoom,  // 自动缩放
    FIsChanged: Boolean;  // 是否发生了改变

    FAnnotatePre: THCAnnotatePre;  // 批注管理

    FViewModel: THCViewModel;  // 界面显示模式：页面、Web
    FCaret: THCCaret;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnCaretChange, FOnVerScroll, FOnHorScroll, FOnSectionCreateItem,
    FOnSectionReadOnlySwitch, FOnSectionCurParaNoChange, FOnSectionActivePageChange
      : TNotifyEvent;
    FOnSectionCaretItemChanged: TSectionDataItemEvent;
    FOnSectionCreateStyleItem: TStyleItemEvent;
    FOnSectionCanEdit: TOnCanEditEvent;
    FOnSectionInsertTextBefor: TTextEvent;
    FOnSectionInsertItem, FOnSectionRemoveItem: TSectionDataItemEvent;
    FOnSectionDrawItemMouseMove: TSectionDataDrawItemMouseEvent;
    FOnSectionSaveItem: TSectionDataItemNoFunEvent;
    FOnSectionAcceptAction: TSectionDataActionEvent;
    FOnSectionDrawItemPaintAfter, FOnSectionDrawItemPaintBefor: TSectionDrawItemPaintEvent;

    FOnSectionPaintHeaderBefor, FOnSectionPaintHeaderAfter,
      FOnSectionPaintFooterBefor, FOnSectionPaintFooterAfter,
      FOnSectionPaintPageBefor, FOnSectionPaintPageAfter,
      FOnSectionPaintPaperBefor, FOnSectionPaintPaperAfter: TSectionPaintEvent;
    FOnPaintViewBefor, FOnPaintViewAfter: TPaintEvent;

    FOnChange, FOnChangeSwitch, FOnZoomChange, FOnSetFocus,
    FOnViewResize  // 视图大小发生变化
      : TNotifyEvent;

    /// <summary> 根据节页面参数设置打印机 </summary>
    /// <param name="ASectionIndex"></param>
    procedure SetPrintBySectionInfo(const ASectionIndex: Integer);
    //
    procedure GetViewWidth;
    procedure GetViewHeight;
    //
    function GetSymmetryMargin: Boolean;
    procedure SetSymmetryMargin(const Value: Boolean);
    procedure DoVerScroll(Sender: TObject; ScrollCode: TScrollCode; const ScrollPos: Integer);
    procedure DoHorScroll(Sender: TObject; ScrollCode: TScrollCode; const ScrollPos: Integer);
    procedure DoSectionDataChange(Sender: TObject);
    procedure DoSectionChangeTopLevelData(Sender: TObject);

    // 仅重绘和重建光标，不触发Change事件
    procedure DoSectionDataCheckUpdateInfo(Sender: TObject);
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const ALoadSectionProc: TLoadSectionProc);

    function DoUndoNew: THCUndo;
    function DoUndoGroupBegin(const AItemNo, AOffset: Integer): THCUndoGroupBegin;
    function DoUndoGroupEnd(const AItemNo, AOffset: Integer): THCUndoGroupEnd;
    procedure DoUndo(const Sender: THCUndo);
    procedure DoRedo(const Sender: THCUndo);
    procedure DoUndoDestroy(const Sender: THCUndo);

    procedure DoViewResize;
    /// <summary> 文档"背板"变动(数据无变化，如对称边距，缩放视图) </summary>
    procedure DoMapChanged;
    procedure DoSectionReadOnlySwitch(Sender: TObject);
    function DoSectionGetScreenCoord(const X, Y: Integer): TPoint;
    procedure DoSectionItemResize(const AData: THCCustomData; const AItemNo: Integer);
    procedure DoSectionPaintHeaderBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintHeaderAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintFooterBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintFooterAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintPageBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintPageAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintPaperBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintPaperAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionDrawItemAnnotate(const Sender: TObject; const AData: THCCustomData;
      const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate);
    function DoSectionGetUndoList: THCUndoList;
    procedure DoSectionInsertAnnotate(const Sender: TObject; const AData: THCCustomData;
      const ADataAnnotate: THCDataAnnotate);
    procedure DoSectionRemoveAnnotate(const Sender: TObject; const AData: THCCustomData;
      const ADataAnnotate: THCDataAnnotate);
    procedure DoSectionCurParaNoChange(Sender: TObject);
    procedure DoSectionActivePageChange(Sender: TObject);

    procedure DoStyleInvalidateRect(const ARect: TRect);
    procedure DoAnnotatePreUpdateView(Sender: TObject);
    //
    function NewDefaultSection: THCSection;

    function GetViewRect: TRect;

    function GetPageIndexTop(const APageIndex: Integer): Integer;
    procedure DoPageUp(Sender: TObject);
    procedure DoPageDown(Sender: TObject);

    /// <summary> 重新获取光标位置 </summary>
    procedure ReBuildCaret;
    procedure GetSectionByCrood(const X, Y: Integer; var ASectionIndex: Integer);
    procedure SetZoom(const Value: Single);

    function GetHScrollValue: Integer;
    function GetCurStyleNo: Integer;
    function GetCurParaNo: Integer;

    function GetShowLineActiveMark: Boolean;
    procedure SetShowLineActiveMark(const Value: Boolean);

    function GetShowLineNo: Boolean;
    procedure SetShowLineNo(const Value: Boolean);

    function GetShowUnderLine: Boolean;
    procedure SetShowUnderLine(const Value: Boolean);

    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetPageNoFormat(const Value: string);
    procedure SetViewModel(const Value: THCViewModel);
    procedure SetActiveSectionIndex(const Value: Integer);
    procedure SetIsChanged(const Value: Boolean);
    procedure SetPagePadding(const Value: Byte);
    /// <summary> 获取当前节对象 </summary>
    function GetActiveSection: THCSection;

    procedure AutoScrollTimer(const AStart: Boolean);
    procedure GetPagesAndActive;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoChange; virtual;
    procedure DoCaretChange; virtual;
    procedure DoKillFocus; virtual;
    procedure DoSetFocus; virtual;  // 为处理DLL和主程序的DBGrid焦点问题增加此事件供主程序处理
    procedure DoSectionCreateItem(Sender: TObject); virtual;
    function DoSectionAcceptAction(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean; virtual;
    function DoSectionCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem; virtual;
    function DoSectionPaintDomainRegion(const Sender: TObject; const AData: THCCustomData; const AItemNo: Integer): Boolean; virtual;
    procedure DoSectionCaretItemChanged(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem); virtual;
    procedure DoSectionInsertItem(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem); virtual;
    procedure DoSectionRemoveItem(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem); virtual;
    function DoSectionSaveItem(const Sender: TObject; const AData: THCCustomData; const AItemNo: Integer): Boolean; virtual;
    procedure DoSectionItemMouseDown(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, AOffset: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoSectionItemMouseUp(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, AOffset: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoSectionDrawItemMouseMove(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, AOffset, ADrawItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    function DoSectionCanEdit(const Sender: TObject): Boolean; virtual;
    function DoSectionInsertTextBefor(const AData: THCCustomData; const AItemNo, AOffset: Integer;
      const AText: string): Boolean; virtual;
    procedure DoSectionDrawItemPaintBefor(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    procedure DoSectionDrawItemPaintContent(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
      const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    procedure DoSectionDrawItemPaintAfter(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    /// <summary> 实现插入文本 </summary>
    function DoInsertText(const AText: string): Boolean; virtual;

    /// <summary> 复制前，便于控制是否允许复制 </summary>
    function DoCopyRequest(const AFormat: Word): Boolean; virtual;

    /// <summary> 粘贴前，便于控制是否允许粘贴 </summary>
    function DoPasteRequest(const AFormat: Word): Boolean; virtual;

    /// <summary> 复制前，便于订制特征数据如内容来源 </summary>
    procedure DoCopyAsStream(const AStream: TStream); virtual;

    /// <summary> 粘贴前，便于确认订制特征数据如内容来源 </summary>
    function DoPasteFromStream(const AStream: TStream): Boolean; virtual;

    /// <summary> 视图绘制开始 </summary>
    procedure DoPaintViewBefor(const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    /// <summary> 视图绘制完成 </summary>
    procedure DoPaintViewAfter(const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    /// <summary> 保存文档前触发事件，便于订制特征数据 </summary>
    procedure DoSaveStreamBefor(const AStream: TStream); virtual;

    /// <summary> 保存文档后触发事件，便于订制特征数据 </summary>
    procedure DoSaveStreamAfter(const AStream: TStream); virtual;

    /// <summary> 读取文档前触发事件，便于确认订制特征数据 </summary>
    procedure DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word); virtual;

    /// <summary> 读取文档后触发事件，便于确认订制特征数据 </summary>
    procedure DoLoadStreamAfter(const AStream: TStream; const AFileVersion: Word); virtual;
    //
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    // 消息
    /// <summary> 响应Tab键和方向键 </summary>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;

    // 接收输入法输入的内容
    procedure WMImeComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure UpdateImeComposition(const ALParam: Integer); virtual;
    procedure UpdateImePosition; virtual;  // IME 通知输入法更新位置

    procedure WndProc(var Message: TMessage); override;
    //
    procedure CalcScrollRang;

    /// <summary> 是否由滚动条位置变化引起的更新 </summary>
    procedure CheckUpdateInfo;

    /// <summary> 当前光标信息 </summary>
    property Caret: THCCaret read FCaret;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    /// <summary> 删除不使用的文本样式 </summary>
    class procedure DeleteUnUsedStyle(const AStyle: THCStyle;
      const ASections: TObjectList<THCSection>; const AAreas: TSectionAreas = [saHeader, saPage, saFooter]);

    /// <summary> 重设当前节纸张边距 </summary>
    procedure ResetActiveSectionMargin;

    /// <summary> ActiveItem重新适应其环境(供外部直接修改Item属性后重新和其前后Item连接组合) </summary>
    procedure ActiveItemReAdaptEnvironment;

    /// <summary> 全部清空(清除各节页眉、页脚、页面的Item及DrawItem) </summary>
    procedure Clear;

    /// <summary> 取消选中 </summary>
    procedure DisSelect;

    /// <summary> 删除选中内容 </summary>
    procedure DeleteSelected;

    /// <summary> 删除当前域 </summary>
    function DeleteActiveDomain: Boolean;

    /// <summary> 删除当前Data指定范围内的Item </summary>
    procedure DeleteActiveDataItems(const AStartNo: Integer; const AEndNo: Integer = -1;
      const AKeepPara: Boolean = True);

    /// <summary> 删除当前节 </summary>
    procedure DeleteActiveSection;

    /// <summary> 各节重新计算排版 </summary>
    procedure FormatData;

    /// <summary> 插入流 </summary>
    function InsertStream(const AStream: TStream): Boolean;

    function AppendStream(const AStream: TStream): Boolean;

    /// <summary> 插入文本(可包括#13#10) </summary>
    function InsertText(const AText: string): Boolean;

    /// <summary> 插入指定行列的表格 </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;

    /// <summary> 插入图片 </summary>
    function InsertImage(const AFile: string): Boolean; overload;

    function InsertImage(const AImage: TGraphic): Boolean; overload;

    /// <summary> 插入GIF图片 </summary>
    function InsertGifImage(const AFile: string): Boolean;

    /// <summary> 插入水平线 </summary>
    function InsertLine(const ALineHeight: Integer): Boolean;

    /// <summary> 插入一个Item </summary>
    function InsertItem(const AItem: THCCustomItem): Boolean; overload;

    /// <summary> 在指定的位置插入一个Item </summary>
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem): Boolean; overload;

    /// <summary> 插入浮动Item </summary>
    function InsertFloatItem(const AFloatItem: THCCustomFloatItem): Boolean;

    /// <summary> 插入批注 </summary>
    function InsertAnnotate(const ATitle, AText: string): Boolean;

    /// <summary> 从当前位置后换行 </summary>
    function InsertBreak: Boolean;

    /// <summary> 从当前位置后分页 </summary>
    function InsertPageBreak: Boolean;

    /// <summary> 从当前位置后分节 </summary>
    function InsertSectionBreak: Boolean;

    /// <summary> 插入域 </summary>
    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;

    /// <summary> 当前图片重设为指定的图像和大小 </summary>
    function SetActiveImage(const AImageStream: TStream): Boolean;

    /// <summary> 当前表格重设为指定的行列数 </summary>
    function ActiveTableResetRowCol(const ARowCount, AColCount: Byte): Boolean;

    /// <summary> 当前表格选中行下面插入行 </summary>
    function ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;

    /// <summary> 当前表格选中行上面插入行 </summary>
    function ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;

    /// <summary> 当前表格删除选中的行 </summary>
    function ActiveTableDeleteCurRow: Boolean;

    /// <summary> 当前表格选中的单元格拆分行 </summary>
    function ActiveTableSplitCurRow: Boolean;

    /// <summary> 当前表格选中的单元格拆分列 </summary>
    function ActiveTableSplitCurCol: Boolean;

    /// <summary> 当前表格选中列左侧插入列 </summary>
    function ActiveTableInsertColBefor(const AColCount: Byte): Boolean;

    /// <summary> 当前表格选中列右侧插入列 </summary>
    function ActiveTableInsertColAfter(const AColCount: Byte): Boolean;

    /// <summary> 当前表格删除选中的列 </summary>
    function ActiveTableDeleteCurCol: Boolean;

    /// <summary> 修改当前光标所在段水平对齐方式 </summary>
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);

    /// <summary> 修改当前光标所在段垂直对齐方式 </summary>
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);

    /// <summary> 修改当前光标所在段背景色 </summary>
    procedure ApplyParaBackColor(const AColor: TColor);

    /// <summary> 修改当前光标所在段换行截断方式 </summary>
    procedure ApplyParaBreakRough(const ARough: Boolean);

    /// <summary> 修改当前光标所在段行间距 </summary>
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode; const ASpace: Single = 1);

    /// <summary> 修改当前光标所在段左缩进 </summary>
    procedure ApplyParaLeftIndent(const Add: Boolean = True); overload;
    /// <summary> 修改当前光标所在段左缩进 </summary>
    procedure ApplyParaLeftIndent(const AIndent: Single); overload;

    /// <summary> 修改当前光标所在段右缩进 </summary>
    procedure ApplyParaRightIndent(const AIndent: Single);

    /// <summary> 修改当前光标所在段首行缩进 </summary>
    procedure ApplyParaFirstIndent(const AIndent: Single);

    /// <summary> 修改当前选中文本的样式 </summary>
    procedure ApplyTextStyle(const AFontStyle: THCFontStyle);

    /// <summary> 修改当前选中文本的字体 </summary>
    procedure ApplyTextFontName(const AFontName: TFontName);

    /// <summary> 修改当前选中文本的字号 </summary>
    procedure ApplyTextFontSize(const AFontSize: Single);

    /// <summary> 修改当前选中文本的颜色 </summary>
    procedure ApplyTextColor(const AColor: TColor);

    /// <summary> 修改当前选中文本的背景颜色 </summary>
    procedure ApplyTextBackColor(const AColor: TColor);

    /// <summary> 修改当前单元内容对齐方式 </summary>
    procedure ApplyTableCellAlign(const AAlign: THCContentAlign);

    /// <summary> 全选(所有节内容) </summary>
    procedure SelectAll;

    /// <summary> 剪切选中内容 </summary>
    procedure Cut;

    /// <summary> 复制选中内容(tcf格式) </summary>
    procedure Copy;

    /// <summary> 复制选中内容为文本 </summary>
    procedure CopyAsText;

    /// <summary> 粘贴剪贴板中的内容 </summary>
    procedure Paste;

    /// <summary> 放大视图 </summary>
    function ZoomIn(const Value: Integer): Integer;

    /// <summary> 缩小视图 </summary>
    function ZoomOut(const Value: Integer): Integer;

    /// <summary> 重绘客户区域 </summary>
    procedure UpdateView; overload;

    /// <summary> 重绘客户区指定区域 </summary>
    procedure UpdateView(const ARect: TRect); overload;

    /// <summary> 开始批量重绘 </summary>
    procedure BeginUpdate;

    /// <summary> 结束批量重绘 </summary>
    procedure EndUpdate;

    /// <summary> 开始一组撤销操作 </summary>
    procedure UndoGroupBegin;

    /// <summary> 结束一组撤销操作 </summary>
    procedure UndoGroupEnd;
    //
    /// <summary> 返回当前节当前Item </summary>
    function GetActiveItem: THCCustomItem;

    /// <summary> 返回当前节顶层Item </summary>
    function GetTopLevelItem: THCCustomItem;

    /// <summary> 返回当前节顶层DrawItem </summary>
    function GetTopLevelDrawItem: THCCustomDrawItem;

    /// <summary> 返回当前光标所在页序号 </summary>
    function GetActivePageIndex: Integer;

    /// <summary> 返回当前预览起始页序号 </summary>
    function GetPagePreviewFirst: Integer;

    /// <summary> 返回总页数 </summary>
    function GetPageCount: Integer;

    procedure PageUp;

    procedure PageDown;

    /// <summary> 返回指定节页面绘制时Left位置 </summary>
    function GetSectionDrawLeft(const ASectionIndex: Integer): Integer;

    /// <summary> 返回格式化位置相对当前页显示的窗体坐标 </summary>
    function GetFormatPointToViewCoord(const APoint: TPoint): TPoint;

    /// <summary> 返回光标处DrawItem相对当前页显示的窗体坐标 </summary>
    /// <returns>坐标</returns>
    function GetTopLevelDrawItemViewCoord: TPoint;

    function GetTopLevelRectDrawItemViewCoord: TPoint;

    /// <summary> 设置当前TextItem的文本内容 </summary>
    procedure SetActiveItemText(const AText: string);

    /// <summary> 格式化指定节的数据 </summary>
    procedure FormatSection(const ASectionIndex: Integer);

    /// <summary> 获取当前节顶层Data </summary>
    function ActiveSectionTopLevelData: THCCustomData;

    /// <summary> 指定节在整个胶卷中的Top位置 </summary>
    function GetSectionTopFilm(const ASectionIndex: Integer): Integer;

    // 保存文档
    /// <summary> 文档保存为hcf格式 </summary>
    procedure SaveToFile(const AFileName: string; const AQuick: Boolean = False);

    /// <summary> 读取hcf文件 </summary>
    function LoadFromFile(const AFileName: string): Boolean;

    /// <summary> 读取其他格式的文件 </summary>
    procedure LoadFromDocumentFile(const AFileName: string; const AExt: string);

    /// <summary> 另存为其他格式的文件 </summary>
    procedure SaveToDocumentFile(const AFileName: string; const AExt: string);

    /// <summary> 读取其他格式的文件流 </summary>
    procedure LoadFromDocumentStream(const AStream: TStream; const AExt: string);

    /// <summary> 文档保存为PDF格式 </summary>
    procedure SaveToPDF(const AFileName: string);

    procedure SaveToPDFStream(const AStream: TStream; const APageImage: Boolean = False);

    /// <summary> 以字符串形式获取文档各节正文内容 </summary>
    function SaveToText: string;

    /// <summary> 读文本到第一节正文 </summary>
    function LoadFromText(const AText: string): Boolean;

    /// <summary> 文档各节正文字符串保存为文本格式文件 </summary>
    procedure SaveToTextFile(const AFileName: string; const AEncoding: TEncoding);

    /// <summary> 读取文本文件内容到第一节正文 </summary>
    function LoadFromTextFile(const AFileName: string; const AEncoding: TEncoding): Boolean;

    /// <summary> 文档各节正文字符串保存为文本格式流 </summary>
    procedure SaveToTextStream(const AStream: TStream; const AEncoding: TEncoding);

    /// <summary> 读取文本文件流 </summary>
    function LoadFromTextStream(const AStream: TStream; AEncoding: TEncoding): Boolean;

    /// <summary> 文档保存到流 </summary>
    procedure SaveToStream(const AStream: TStream; const AQuick: Boolean = False;
      const AAreas: TSectionAreas = [saHeader, saPage, saFooter]); virtual;

    /// <summary> 读取文件流 </summary>
    function LoadFromStream(const AStream: TStream): Boolean; virtual;

    /// <summary> 文档保存为xml格式 </summary>
    procedure SaveToXml(const AFileName: string; const AEncoding: TEncoding);

    procedure SaveToXmlStream(const AStream: TStream; const AEncoding: TEncoding);

    /// <summary> 读取xml格式 </summary>
    function LoadFromXml(const AFileName: string): Boolean;

    function LoadFromXmlStream(const AStream: TStream): Boolean;

    /// <summary> 导出为html格式 </summary>
    /// <param name="ASeparateSrc">True：图片等保存到文件夹，False以base64方式存储到页面中</param>
    procedure SaveToHtml(const AFileName: string; const ASeparateSrc: Boolean = False);

    /// <summary>
    /// 将文档每一页保存为图片
    /// </summary>
    /// <param name="APath">图片路径</param>
    /// <param name="APrefix">单张图片时文件名，多图片时每一张名称的前缀</param>
    /// <param name="AImageType">图片格式如 BMP, JPG, PNG</param>
    /// <param name="AOnePaper">True单张图片</param>
    procedure SaveToImage(const APath, APrefix, AImageType: string; const AOnePaper: Boolean = True);

    /// <summary> 获取指定页所在的节和相对此节的页序号 </summary>
    /// <param name="APageIndex">页序号</param>
    /// <param name="ASectionPageIndex">返回节第一页相对所有页的序号</param>
    /// <returns>返回页序号所在的节序号</returns>
    function GetSectionPageIndexByPageIndex(const APageIndex: Integer; var ASectionPageIndex: Integer): Integer;

    // 打印
    /// <summary> 使用默认打印机打印所有页 </summary>
    /// <returns>打印结果</returns>
    function Print: TPrintResult; overload;

    /// <summary> 使用指定的打印机打印所有页 </summary>
    /// <param name="APrinter">指定打印机</param>
    /// <param name="ACopies">打印份数</param>
    /// <returns>打印结果</returns>
    function Print(const APrinter: string; const ACopies: Integer = 1): TPrintResult; overload;

    /// <summary> 使用指定的打印机打印指定页序号范围内的页 </summary>
    /// <param name="APrinter">指定打印机</param>
    /// <param name="AStartPageIndex">起始页序号</param>
    /// <param name="AEndPageIndex">结束页序号</param>
    /// <param name="ACopies">打印份数</param>
    /// <returns></returns>
    function Print(const APrinter: string; const AStartPageIndex, AEndPageIndex, ACopies: Integer): TPrintResult; overload;

    /// <summary> 使用指定的打印机打印指定页 </summary>
    /// <param name="APrinter">指定打印机</param>
    /// <param name="ACopies">打印份数</param>
    /// <param name="APages">要打印的页序号数组</param>
    /// <returns>打印结果</returns>
    function Print(const APrinter: string; const ACopies: Integer;
      const APages: array of Integer): TPrintResult; overload;

    /// <summary> 使用指定的打印机打印奇数页 </summary>
    function PrintOdd(const APrinter: string): TPrintResult;

    /// <summary> 使用指定的打印机打印偶数页 </summary>
    function PrintEven(const APrinter: string): TPrintResult;

    /// <summary> 从当前行打印当前页(仅限正文) </summary>
    /// <param name="APrintHeader"> 是否打印页眉 </param>
    /// <param name="APrintFooter"> 是否打印页脚 </param>
    function PrintCurPageByActiveLine(const APrinter: string; const APrintHeader, APrintFooter: Boolean): TPrintResult;

    /// <summary> 打印当前页指定的起始、结束Item(仅限正文) </summary>
    /// <param name="APrintHeader"> 是否打印页眉 </param>
    /// <param name="APrintFooter"> 是否打印页脚 </param>
    function PrintCurPageByItemRange(const APrinter: string; const APrintHeader, APrintFooter: Boolean;
      const AStartItemNo, AStartOffset, AEndItemNo, AEndOffset: Integer): TPrintResult;

    /// <summary> 打印当前页选中的起始、结束Item(仅限正文) </summary>
    /// <param name="APrintHeader"> 是否打印页眉 </param>
    /// <param name="APrintFooter"> 是否打印页脚 </param>
    function PrintCurPageSelected(const APrinter: string; const APrintHeader, APrintFooter: Boolean): TPrintResult;

    /// <summary> 合并表格选中的单元格 </summary>
    function MergeTableSelectCells: Boolean;
    function TableApplyContentAlign(const AAlign: THCContentAlign): Boolean;

    /// <summary> 撤销 </summary>
    procedure Undo;

    /// <summary> 恢复/重做 </summary>
    procedure Redo;

    /// <summary> 当前位置开始查找指定的内容 </summary>
    /// <param name="AKeyword">要查找的关键字</param>
    /// <param name="AForward">True：向前，False：向后</param>
    /// <param name="AMatchCase">True：区分大小写，False：不区分大小写</param>
    /// <returns>True：找到</returns>
    function Search(const AKeyword: string; const AForward: Boolean = False;
      const AMatchCase: Boolean = False): Boolean;

    /// <summary> 替换已经通过查找选中的内容 </summary>
    /// <param name="AText">要替换为的内容</param>
    /// <returns>是否替换成功</returns>
    function Replace(const AText: string): Boolean;

    // 属性部分
    /// <summary> 当前文档名称 </summary>
    property FileName: string read FFileName write FFileName;

    /// <summary> 当前文档样式表 </summary>
    property Style: THCStyle read FStyle;

    /// <summary> 是否对称边距 </summary>
    property SymmetryMargin: Boolean read GetSymmetryMargin write SetSymmetryMargin;

    /// <summary> 当前光标所在页的序号 </summary>
    property ActivePageIndex: Integer read GetActivePageIndex;

    /// <summary> 当前预览的页序号 </summary>
    property PagePreviewFirst: Integer read GetPagePreviewFirst;

    /// <summary> 总页数 </summary>
    property PageCount: Integer read GetPageCount;

    /// <summary> 当前光标所在节的序号 </summary>
    property ActiveSectionIndex: Integer read FActiveSectionIndex write SetActiveSectionIndex;

    /// <summary> 当前光标所在的节 </summary>
    property ActiveSection: THCSection read GetActiveSection;

    /// <summary> 水平滚动条 </summary>
    property HScrollBar: THCStatusScrollBar read FHScrollBar;

    /// <summary> 垂直滚动条 </summary>
    property VScrollBar: THCRichScrollBar read FVScrollBar;
    /// <summary> 当前光标处的样式 </summary>
    property CurStyleNo: Integer read GetCurStyleNo;
    /// <summary> 当前光标处的段样式 </summary>
    property CurParaNo: Integer read GetCurParaNo;

    /// <summary> 缩放值 </summary>
    property Zoom: Single read FZoom write SetZoom;

    /// <summary> 当前文档所有节 </summary>
    property Sections: TObjectList<THCSection> read FSections;

    /// <summary> 是否显示当前行指示符 </summary>
    property ShowLineActiveMark: Boolean read GetShowLineActiveMark write SetShowLineActiveMark;

    /// <summary> 是否显示行号 </summary>
    property ShowLineNo: Boolean read GetShowLineNo write SetShowLineNo;

    /// <summary> 是否显示下划线 </summary>
    property ShowUnderLine: Boolean read GetShowUnderLine write SetShowUnderLine;

    /// <summary> 当前文档是否有变化 </summary>
    property IsChanged: Boolean read FIsChanged write SetIsChanged;

    /// <summary> 当前文档胶卷视图时页之间的间距 </summary>
    property PagePadding: Byte read FPagePadding write SetPagePadding;

    /// <summary> 当前文档显示出来的批注 </summary>
    property AnnotatePre: THCAnnotatePre read FAnnotatePre;

    /// <summary> 当前文档可显示的宽度 </summary>
    property ViewWidth: Integer read FViewWidth;

    /// <summary> 当前文档可显示的高度 </summary>
    property ViewHeight: Integer read FViewHeight;
  published
    { Published declarations }

    /// <summary> 节有新的Item创建时触发 </summary>
    property OnSectionCreateItem: TNotifyEvent read FOnSectionCreateItem write FOnSectionCreateItem;

    /// <summary> 节有新的Item插入时触发 </summary>
    property OnSectionItemInsert: TSectionDataItemEvent read FOnSectionInsertItem write FOnSectionInsertItem;

    /// <summary> 节有新的Item删除时触发 </summary>
    property OnSectionRemoveItem: TSectionDataItemEvent read FOnSectionRemoveItem write FOnSectionRemoveItem;

    /// <summary> 节中Data的DrawItem MouseMove事件 </summary>
    property OnSectionDrawItemMouseMove: TSectionDataDrawItemMouseEvent read FOnSectionDrawItemMouseMove write FOnSectionDrawItemMouseMove;

    /// <summary> 节保存Item前触发，控制是否允许保存该Item </summary>
    property OnSectionSaveItem: TSectionDataItemNoFunEvent read FOnSectionSaveItem write FOnSectionSaveItem;

    /// <summary> 节中指定事件发生时触发，控制是否允许该事件 </summary>
    property OnSectionAcceptAction: TSectionDataActionEvent read FOnSectionAcceptAction write FOnSectionAcceptAction;

    /// <summary> Item绘制开始前触发 </summary>
    property OnSectionDrawItemPaintBefor: TSectionDrawItemPaintEvent read FOnSectionDrawItemPaintBefor write FOnSectionDrawItemPaintBefor;

    /// <summary> DrawItem绘制完成后触发 </summary>
    property OnSectionDrawItemPaintAfter: TSectionDrawItemPaintEvent read FOnSectionDrawItemPaintAfter write FOnSectionDrawItemPaintAfter;

    /// <summary> 节页眉绘制时触发 </summary>
    property OnSectionPaintHeaderBefor: TSectionPaintEvent read FOnSectionPaintHeaderBefor write FOnSectionPaintHeaderBefor;
    property OnSectionPaintHeaderAfter: TSectionPaintEvent read FOnSectionPaintHeaderAfter write FOnSectionPaintHeaderAfter;

    /// <summary> 节页脚绘制时触发 </summary>
    property OnSectionPaintFooterBefor: TSectionPaintEvent read FOnSectionPaintFooterBefor write FOnSectionPaintFooterBefor;
    property OnSectionPaintFooterAfter: TSectionPaintEvent read FOnSectionPaintFooterAfter write FOnSectionPaintFooterAfter;

    /// <summary> 节页面绘制时触发 </summary>
    property OnSectionPaintPageBefor: TSectionPaintEvent read FOnSectionPaintPageBefor write FOnSectionPaintPageBefor;
    property OnSectionPaintPageAfter: TSectionPaintEvent read FOnSectionPaintPageAfter write FOnSectionPaintPageAfter;

    /// <summary> 节整页绘制前触发 </summary>
    property OnSectionPaintPaperBefor: TSectionPaintEvent read FOnSectionPaintPaperBefor write FOnSectionPaintPaperBefor;

    /// <summary> 节整页绘制后触发 </summary>
    property OnSectionPaintPaperAfter: TSectionPaintEvent read FOnSectionPaintPaperAfter write FOnSectionPaintPaperAfter;

    /// <summary> 节只读属性有变化时触发 </summary>
    property OnSectionReadOnlySwitch: TNotifyEvent read FOnSectionReadOnlySwitch write FOnSectionReadOnlySwitch;

    /// <summary> 界面显示模式：胶卷、页面 </summary>
    property ViewModel: THCViewModel read FViewModel write SetViewModel;

    /// <summary> 是否根据宽度自动计算缩放比例 </summary>
    property AutoZoom: Boolean read FAutoZoom write FAutoZoom;

    /// <summary> 所有Section是否只读 </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;

    /// <summary> 页码的格式 </summary>
    property PageNoFormat: string read FPageNoFormat write SetPageNoFormat;

    /// <summary> 鼠标按下时触发 </summary>
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;

    /// <summary> 鼠标弹起时触发 </summary>
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;

    /// <summary> 光标位置改变时触发 </summary>
    property OnCaretChange: TNotifyEvent read FOnCaretChange write FOnCaretChange;

    /// <summary> 垂直滚动条滚动时触发 </summary>
    property OnVerScroll: TNotifyEvent read FOnVerScroll write FOnVerScroll;

    /// <summary> 水平滚动条滚动时触发 </summary>
    property OnHorScroll: TNotifyEvent read FOnHorScroll write FOnHorScroll;

    /// <summary> 文档内容变化时触发 </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    /// <summary> 文档Change状态切换时触发 </summary>
    property OnChangedSwitch: TNotifyEvent read FOnChangeSwitch write FOnChangeSwitch;

    /// <summary> 文档Zoom缩放变化后触发 </summary>
    property OnZoomChanged: TNotifyEvent read FOnZoomChange write FOnZoomChange;

    /// <summary> 窗口重绘开始时触发 </summary>
    property OnPaintViewBefor: TPaintEvent read FOnPaintViewBefor write FOnPaintViewBefor;

    /// <summary> 窗口重绘结束后触发 </summary>
    property OnPaintViewAfter: TPaintEvent read FOnPaintViewAfter write FOnPaintViewAfter;

    /// <summary> 创建指定样式的Item时触发 </summary>
    property OnSectionCreateStyleItem: TStyleItemEvent read FOnSectionCreateStyleItem write FOnSectionCreateStyleItem;

    /// <summary> 当编辑只读状态的Data时触发 </summary>
    property OnSectionCanEdit: TOnCanEditEvent read FOnSectionCanEdit write FOnSectionCanEdit;

    property OnSectionInsertTextBefor: TTextEvent read FOnSectionInsertTextBefor write FOnSectionInsertTextBefor;

    /// <summary> 节当前位置段样式和上一次不一样时触发 </summary>
    property OnSectionCurParaNoChange: TNotifyEvent read FOnSectionCurParaNoChange write FOnSectionCurParaNoChange;

    property OnSectionCaretItemChanged: TSectionDataItemEvent read FOnSectionCaretItemChanged write FOnSectionCaretItemChanged;

    /// <summary> 节当前位置文本样式和上一次不一样时触发 </summary>
    property OnSectionActivePageChange: TNotifyEvent read FOnSectionActivePageChange write FOnSectionActivePageChange;

    /// <summary> 文档视图有变动时触发 </summary>
    property OnViewResize: TNotifyEvent read FOnViewResize write FOnViewResize;

    property OnSetFocus: TNotifyEvent read FOnSetFocus write FOnSetFocus;

    property Color;
    property PopupMenu;
  end;

implementation

uses
  HCPrinters, Imm, Forms, Math, Clipbrd, HCImageItem, ShellAPI, HCXml, HCDocumentRW,
  HCRtfRW, HCUnitConversion, HCEditItem;

const
  IMN_UPDATECURSTRING = $F000;  // 和输入法交互，当前光标处的字符串

{ THCView }

procedure THCView.ApplyTextStyle(const AFontStyle: THCFontStyle);
begin
  ActiveSection.ApplyTextStyle(AFontStyle);
end;

procedure THCView.AutoScrollTimer(const AStart: Boolean);
begin
  if not AStart then
    KillTimer(Handle, 2)
  else
  if SetTimer(Handle, 2, 100, nil) = 0 then
    raise EOutOfResources.Create(HCS_EXCEPTION_TIMERRESOURCEOUTOF);
end;

procedure THCView.ApplyTableCellAlign(const AAlign: THCContentAlign);
begin
  ActiveSection.ApplyTableCellAlign(AAlign);
end;

procedure THCView.ApplyTextBackColor(const AColor: TColor);
begin
  ActiveSection.ApplyTextBackColor(AColor);
end;

procedure THCView.ApplyTextColor(const AColor: TColor);
begin
  ActiveSection.ApplyTextColor(AColor);
end;

procedure THCView.ApplyTextFontName(const AFontName: TFontName);
begin
  ActiveSection.ApplyTextFontName(AFontName);
end;

procedure THCView.ApplyTextFontSize(const AFontSize: Single);
begin
  ActiveSection.ApplyTextFontSize(AFontSize);
end;

procedure THCView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure THCView.CalcScrollRang;
var
  i, vWidth, vVMax, vHMax: Integer;
begin
  vVMax := 0;

  if FViewModel = hvmFilm then
  begin
    vHMax := FSections[0].PaperWidthPix;
    for i := 0 to FSections.Count - 1 do  //  计算节垂直总和，以及节中最宽的页宽度
    begin
      vVMax := vVMax + FSections[i].GetFilmHeight;

      vWidth := FSections[i].PaperWidthPix;

      if vWidth > vHMax then
        vHMax := vWidth;
    end;
  end
  else
  begin
    if FViewModel = hvmPage then
      vHMax := FSections[0].PaperWidthPix
    else
      vHMax := FSections[0].GetPageWidth;

    for i := 0 to FSections.Count - 1 do  //  计算节垂直总和，以及节中最宽的页宽度
    begin
      vVMax := vVMax + FSections[i].GetFilmHeight;

      if FViewModel = hvmPage then
        vWidth := FSections[i].PaperWidthPix
      else
        vWidth := FSections[i].GetPageWidth;

      if vWidth > vHMax then
        vHMax := vWidth;
    end;
  end;

  if FAnnotatePre.Visible then
    vHMax := vHMax + AnnotationWidth;

  if FViewModel = hvmFilm then
  begin
    vVMax := ZoomIn(vVMax + FPagePadding);  // 补充最后一页后面的PagePadding
    vHMax := ZoomIn(vHMax + FPagePadding + FPagePadding);
  end
  else
  if FViewModel = hvmPage then
    vHMax := ZoomIn(vHMax + FPagePadding + FPagePadding);

  FVScrollBar.Max := vVMax;
  FHScrollBar.Max := vHMax;
end;

procedure THCView.CheckUpdateInfo;
begin
  if FUpdateCount > 0 then Exit;

  // UpdateView里会计算当前显示页，ReBuildCaret里会通知外部重新获取当前预览页和光标所在页
  // ReBuildCaret后有些需要高亮重绘，所以要先处理，所以防止处理完通知外部取页数不对的问题
  // 使用vCaretChange将通知放到最后
  if (FCaret <> nil) and FStyle.UpdateInfo.ReCaret then  // 先处理光标，因为可能光标处有些需要高亮重绘
  begin
    ReBuildCaret;
    FStyle.UpdateInfo.ReCaret := False;
    FStyle.UpdateInfo.ReStyle := False;
    FStyle.UpdateInfo.ReScroll := False;
    UpdateImePosition;
  end;

  if FStyle.UpdateInfo.RePaint then
  begin
    FStyle.UpdateInfo.RePaint := False;
    UpdateView;
  end;
end;

procedure THCView.Clear;
begin
  Self.BeginUpdate;  // 防止：清空时隐藏工具，工具条隐藏时又触发重绘，重绘时还没有格式化完
  try
    FStyle.Initialize;  // 先清样式，防止Data初始化为EmptyData时空Item样式赋值为CurStyleNo
    FSections.DeleteRange(1, FSections.Count - 1);
    FActiveSectionIndex := 0;
    FDisplayFirstSection := -1;
    FDisplayLastSection := -1;

    FUndoList.SaveState;
    try
      FUndoList.Enable := False;
      FSections[0].Clear;
      FUndoList.Clear;
    finally
      FUndoList.RestoreState;
    end;

    FHScrollBar.Position := 0;
    FVScrollBar.Position := 0;
    FStyle.UpdateInfoRePaint;
    FStyle.UpdateInfoReCaret;
    DoMapChanged;
    DoViewResize;
  finally
    Self.EndUpdate;
  end;
end;

procedure THCView.Copy;
var
  vStream: TMemoryStream;
  vMem: Cardinal;
  vPtr: Pointer;
begin
  if ActiveSection.SelectExists then
  begin
    FStyle.States.Include(THCState.hosCopying);
    try
      //if DoCopyAllow(HC_FILEFORMAT) then  为避免下面重复调用，牺牲掉此处判断
      vStream := TMemoryStream.Create;
      try
        _SaveFileFormatAndVersion(vStream);  // 保存文件格式和版本
        DoCopyAsStream(vStream);  // 通知保存事件
        //DeleteUnUsedStyle(FStyle, FSections);  // 删除不使用的样式 大文档有点耗时
        FStyle.SaveToStream(vStream);
        Self.ActiveSectionTopLevelData.SaveSelectToStream(vStream);
        vMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, vStream.Size);
        try
          if vMem = 0 then
            raise Exception.Create(HCS_EXCEPTION_MEMORYLESS);
          vPtr := GlobalLock(vMem);
          Move(vStream.Memory^, vPtr^, vStream.Size);
        finally
          GlobalUnlock(vMem);
        end;
      finally
        vStream.Free;
      end;

      Clipboard.Open;
      try
        Clipboard.Clear;

        if DoCopyRequest(HC_FILEFORMAT) then
          Clipboard.SetAsHandle(HC_FILEFORMAT, vMem);  // HC格式

        if DoCopyRequest(CF_UNICODETEXT) then
          Clipboard.AsText := Self.ActiveSectionTopLevelData.SaveSelectToText;  // 文本格式
      finally
        Clipboard.Close;
      end;
    finally
      FStyle.States.Exclude(THCState.hosCopying);
    end;
  end;
end;

procedure THCView.CopyAsText;
begin
  if DoCopyRequest(CF_UNICODETEXT) then
    Clipboard.AsText := Self.ActiveSectionTopLevelData.SaveSelectToText;
end;

constructor THCView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //
  Self.Color := RGB(82, 89, 107);

  FUndoList := THCUndoList.Create;
  FUndoList.OnUndo := DoUndo;
  FUndoList.OnRedo := DoRedo;
  FUndoList.OnUndoNew := DoUndoNew;
  FUndoList.OnUndoGroupStart := DoUndoGroupBegin;
  FUndoList.OnUndoGroupEnd := DoUndoGroupEnd;
  FUndoList.OnUndoDestroy := DoUndoDestroy;

  FAnnotatePre := THCAnnotatePre.Create;
  FAnnotatePre.OnUpdateView := DoAnnotatePreUpdateView;

  FFileName := '';
  FPageNoFormat := '%d/%d';
  FIsChanged := False;
  FZoom := 1;
  FAutoZoom := False;
  FViewModel := hvmFilm;
  FPagePadding := 20;

  FDataBmp := TBitmap.Create;
  FStyle := THCStyle.CreateEx(True, True);
  FStyle.OnInvalidateRect := DoStyleInvalidateRect;
  FSections := TObjectList<THCSection>.Create;
  FSections.Add(NewDefaultSection);
  FActiveSectionIndex := 0;
  FDisplayFirstSection := 0;
  FDisplayLastSection := 0;
  // 垂直滚动条，范围在Resize中设置
  FVScrollBar := THCRichScrollBar.Create(Self);
  FVScrollBar.Orientation := TOrientation.oriVertical;
  FVScrollBar.OnScroll := DoVerScroll;
  FVScrollBar.OnPageUpClick := DoPageUp;
  FVScrollBar.OnPageDownClick := DoPageDown;
  // 水平滚动条，范围在Resize中设置
  FHScrollBar := THCStatusScrollBar.Create(Self);
  FHScrollBar.Orientation := TOrientation.oriHorizontal;
  FHScrollBar.AddStatus(100);
  FHScrollBar.OnScroll := DoHorScroll;

  FHScrollBar.Parent := Self;
  FVScrollBar.Parent := Self;

  CalcScrollRang;
end;

procedure THCView.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FCaret) then  // 防止切换Parent时多次创建Caret
      FreeAndNil(FCaret);

    FCaret := THCCaret.Create(Handle);
  end;
end;

procedure THCView.Cut;
begin
  Copy;
  ActiveSection.DeleteSelected;
end;

procedure THCView.DeleteActiveDataItems(const AStartNo: Integer;
  const AEndNo: Integer = -1; const AKeepPara: Boolean = True);
begin
  if AEndNo < AStartNo then
    ActiveSection.DeleteActiveDataItems(AStartNo, AStartNo, AKeepPara)
  else
    ActiveSection.DeleteActiveDataItems(AStartNo, AEndNo, AKeepPara);
end;

function THCView.DeleteActiveDomain: Boolean;
begin
  Result := ActiveSection.DeleteActiveDomain;
end;

procedure THCView.DeleteActiveSection;
begin
  if FActiveSectionIndex > 0 then
  begin
    FSections.Delete(FActiveSectionIndex);
    FActiveSectionIndex := FActiveSectionIndex - 1;
    FDisplayFirstSection := -1;
    FDisplayLastSection := -1;
    FStyle.UpdateInfoRePaint;
    FStyle.UpdateInfoReCaret;

    DoChange;
  end;
end;

procedure THCView.DeleteSelected;
begin
  ActiveSection.DeleteSelected;
end;

class procedure THCView.DeleteUnUsedStyle(const AStyle: THCStyle;
  const ASections: TObjectList<THCSection>; const AAreas: TSectionAreas = [saHeader, saPage, saFooter]);
var
  i, vUnCount: Integer;
  vData: THCCustomData;
begin
  for i := 0 to AStyle.TextStyles.Count - 1 do
  begin
    AStyle.TextStyles[i].CheckSaveUsed := False;
    AStyle.TextStyles[i].TempNo := THCStyle.Null;
  end;

  for i := 0 to AStyle.ParaStyles.Count - 1 do
  begin
    AStyle.ParaStyles[i].CheckSaveUsed := False;
    AStyle.ParaStyles[i].TempNo := THCStyle.Null;
  end;

  for i := 0 to ASections.Count - 1 do
    ASections[i].MarkStyleUsed(True, AAreas);

  vUnCount := 0;
  for i := 0 to AStyle.TextStyles.Count - 1 do
  begin
    if AStyle.TextStyles[i].CheckSaveUsed then
      AStyle.TextStyles[i].TempNo := i - vUnCount
    else
      Inc(vUnCount);
  end;

  vUnCount := 0;
  for i := 0 to AStyle.ParaStyles.Count - 1 do
  begin
    if AStyle.ParaStyles[i].CheckSaveUsed then
      AStyle.ParaStyles[i].TempNo := i - vUnCount
    else
      Inc(vUnCount);
  end;

  for i := 0 to ASections.Count - 1 do
  begin
    ASections[i].MarkStyleUsed(False);

    vData := ASections[i].ActiveData.GetTopLevelData;
    if vData.CurStyleNo > THCStyle.Null then  // 光标不在RectItem
      vData.CurStyleNo := AStyle.TextStyles[vData.CurStyleNo].TempNo;

    vData.CurParaNo := AStyle.ParaStyles[vData.CurParaNo].TempNo;
  end;

  for i := AStyle.TextStyles.Count - 1 downto 0 do
  begin
    if not AStyle.TextStyles[i].CheckSaveUsed then
      AStyle.TextStyles.Delete(i);
  end;

  for i := AStyle.ParaStyles.Count - 1 downto 0 do
  begin
    if not AStyle.ParaStyles[i].CheckSaveUsed then
      AStyle.ParaStyles.Delete(i);
  end;
end;

destructor THCView.Destroy;
begin
  FStyle.States.Include(THCState.hosDestroying);
  FreeAndNil(FSections);
  FreeAndNil(FCaret);
  FreeAndNil(FHScrollBar);
  FreeAndNil(FVScrollBar);
  FreeAndNil(FDataBmp);
  FreeAndNil(FStyle);
  FreeAndNil(FUndoList);
  FreeAndNil(FAnnotatePre);
  inherited Destroy;
end;

procedure THCView.DisSelect;
begin
  ActiveSection.DisSelect;
  //DoMapChanged;
  DoSectionDataCheckUpdateInfo(Self);
end;

procedure THCView.GetViewHeight;
begin
  if FHScrollBar.Visible then
    FViewHeight := Height - FHScrollBar.Height
  else
    FViewHeight := Height;
end;

function THCView.GetViewRect: TRect;
begin
  Result := Bounds(0, 0, FViewWidth, FViewHeight);
end;

procedure THCView.GetViewWidth;
begin
  if FVScrollBar.Visible then
    FViewWidth := Width - FVScrollBar.Width
  else
    FViewWidth := Width;
end;

function THCView.GetHScrollValue: Integer;
begin
  Result := FHScrollBar.Position;
end;

procedure THCView.DoMapChanged;
begin
  if FUpdateCount = 0 then
  begin
    CalcScrollRang;
    CheckUpdateInfo;
  end;
end;

function THCView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if GetAsyncKeyState(VK_CONTROL) < 0 then  // 按下ctrl
  begin
    if WheelDelta > 0 then  // 向上
      Zoom := Zoom + 0.1
    else
      Zoom := Zoom - 0.1;
  end
  else
  begin
    if ssShift in Shift then
      FHScrollBar.Position := FHScrollBar.Position - WheelDelta
    else
      FVScrollBar.Position := FVScrollBar.Position - WheelDelta;
  end;

  Result := True;
end;

procedure THCView.DoSectionPaintPageAfter(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if Assigned(FOnSectionPaintPageAfter) then
    FOnSectionPaintPageAfter(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintPageBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if Assigned(FOnSectionPaintPageBefor) then
    FOnSectionPaintPageBefor(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

function THCView.DoSectionPaintDomainRegion(const Sender: TObject;
  const AData: THCCustomData; const AItemNo: Integer): Boolean;
begin
  Result := True;
end;

procedure THCView.DoSectionPaintFooterAfter(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
var
  i, vSectionStartPageIndex, vSectionIndex, vAllPageCount: Integer;
  vS: string;
  vSection: THCSection;
begin
  vSection := Sender as THCSection;

  if vSection.PageNoVisible then  // 显示页码
  begin
    vSectionIndex := FSections.IndexOf(vSection);
    vSectionStartPageIndex := 0;
    vAllPageCount := 0;
    for i := 0 to FSections.Count - 1 do
    begin
      if i = vSectionIndex then
        vSectionStartPageIndex := vAllPageCount;

      vAllPageCount := vAllPageCount + FSections[i].PageCount;
    end;
    vS := Format(FPageNoFormat, [vSectionStartPageIndex + vSection.PageNoFrom + APageIndex, vAllPageCount]);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Size := 10;
    ACanvas.Font.Name := '宋体';
    ACanvas.TextOut(ARect.Left + (ARect.Width - ACanvas.TextWidth(vS)) div 2,
      ARect.Top + vSection.Footer.Height, vS);
  end;

  if Assigned(FOnSectionPaintFooterAfter) then
    FOnSectionPaintFooterAfter(vSection, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintFooterBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if Assigned(FOnSectionPaintFooterBefor) then
    FOnSectionPaintFooterBefor(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintHeaderAfter(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if Assigned(FOnSectionPaintHeaderAfter) then
    FOnSectionPaintHeaderAfter(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintHeaderBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if Assigned(FOnSectionPaintHeaderBefor) then
    FOnSectionPaintHeaderBefor(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintPaperAfter(const Sender: TObject; const APageIndex: Integer;
  const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
begin
  // HCView推广信息，本信息属于开源协议一部分，请保留
  if (not APaintInfo.Print) and (FViewModel = THCViewModel.hvmFilm) and ((Sender as THCSection).PagePadding > 10) then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Size := 10;
    ACanvas.Font.Name := '宋体';
    ACanvas.Font.Style := [];
    ACanvas.Font.Color := $D5D1D0;
    ACanvas.TextOut(ARect.Left, ARect.Bottom + 4, '编辑器由 HCView 提供，技术交流QQ群：649023932');
  end;

  if FAnnotatePre.Visible then  // 当前页有批注，绘制批注
    FAnnotatePre.PaintDrawAnnotate(Sender, ARect, ACanvas, APaintInfo);

  if Assigned(FOnSectionPaintPaperAfter) then
    FOnSectionPaintPaperAfter(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintPaperBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if APaintInfo.Print and (FAnnotatePre.DrawCount > 0) then  // 打印是单面绘制，所以每一页前清除
    FAnnotatePre.ClearDrawAnnotate;

  if Assigned(FOnSectionPaintPaperBefor) then
    FOnSectionPaintPaperBefor(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionReadOnlySwitch(Sender: TObject);
begin
  if Assigned(FOnSectionReadOnlySwitch) then
    FOnSectionReadOnlySwitch(Sender);
end;

procedure THCView.DoSectionRemoveAnnotate(const Sender: TObject;
  const AData: THCCustomData; const ADataAnnotate: THCDataAnnotate);
begin
  FAnnotatePre.RemoveDataAnnotate(ADataAnnotate);
end;

procedure THCView.DoSectionRemoveItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnSectionRemoveItem) then
    FOnSectionRemoveItem(Sender, AData, AItem);
end;

function THCView.DoSectionSaveItem(const Sender: TObject;
  const AData: THCCustomData; const AItemNo: Integer): Boolean;
begin
  if Assigned(FOnSectionSaveItem) then
    Result := FOnSectionSaveItem(Sender, AData, AItemNo)
  else
    Result := True;
end;

procedure THCView.DoSetFocus;
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self);
end;

function THCView.DoSectionGetScreenCoord(const X, Y: Integer): TPoint;
begin
  Result := ClientToScreen(Point(X, Y));
end;

function THCView.DoSectionGetUndoList: THCUndoList;
begin
  Result := FUndoList;
end;

procedure THCView.DoPageDown(Sender: TObject);
var
  vPageIndex: Integer;
begin
  vPageIndex := GetPagePreviewFirst;
  if vPageIndex < GetPageCount - 1 then
    FVScrollBar.Position := GetPageIndexTop(vPageIndex + 1);
end;

procedure THCView.DoPageUp(Sender: TObject);
var
  vPageIndex: Integer;
begin
  vPageIndex := GetPagePreviewFirst;
  if vPageIndex > 0 then
    FVScrollBar.Position := GetPageIndexTop(vPageIndex - 1);
end;

procedure THCView.DoPaintViewAfter(const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnPaintViewAfter) then
    FOnPaintViewAfter(ACanvas, APaintInfo);
end;

procedure THCView.DoPaintViewBefor(const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnPaintViewBefor) then
    FOnPaintViewBefor(ACanvas, APaintInfo);
end;

function THCView.DoPasteRequest(const AFormat: Word): Boolean;
var
  vTopItem: THCCustomItem;
begin
  vTopItem := ActiveSection.GetTopLevelItem;
  if vTopItem is THCEditItem then
  begin
    Result := (AFormat = CF_TEXT) or (AFormat = CF_UNICODETEXT);
    Exit;
  end;

  Result := True;
end;

function THCView.DoPasteFromStream(const AStream: TStream): Boolean;
begin
  Result := True;
end;

procedure THCView.DoRedo(const Sender: THCUndo);
begin
  if Sender is THCSectionUndo then
  begin
    if FActiveSectionIndex <> (Sender as THCSectionUndo).SectionIndex then
      SetActiveSectionIndex((Sender as THCSectionUndo).SectionIndex);

    FHScrollBar.Position := (Sender as THCSectionUndo).HScrollPos;
    FVScrollBar.Position := (Sender as THCSectionUndo).VScrollPos;
  end
  else
  if Sender is THCSectionUndoGroupEnd then
  begin
    if FActiveSectionIndex <> (Sender as THCSectionUndoGroupEnd).SectionIndex then
      SetActiveSectionIndex((Sender as THCSectionUndoGroupEnd).SectionIndex);

    FHScrollBar.Position := (Sender as THCSectionUndoGroupEnd).HScrollPos;
    FVScrollBar.Position := (Sender as THCSectionUndoGroupEnd).VScrollPos;
  end;

  ActiveSection.Redo(Sender);
end;

procedure THCView.DoSaveStreamAfter(const AStream: TStream);
begin
  //SetIsChanged(False);  做定备份保存时不能做为业务保存成功
end;

procedure THCView.DoSaveStreamBefor(const AStream: TStream);
begin
  // 用于外部程序存储自定义数据，如上次浏览位置等
end;

procedure THCView.DoSectionActivePageChange(Sender: TObject);
begin
  if Assigned(FOnSectionActivePageChange) then
    FOnSectionActivePageChange(Sender);
end;

function THCView.DoSectionCanEdit(const Sender: TObject): Boolean;
begin
  if Assigned(FOnSectionCanEdit) then
    Result := FOnSectionCanEdit(Sender)
  else
    Result := True;
end;

procedure THCView.DoSectionCaretItemChanged(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnSectionCaretItemChanged) then
    FOnSectionCaretItemChanged(Sender, AData, AItem);
end;

procedure THCView.DoSectionChangeTopLevelData(Sender: TObject);
begin
  DoViewResize;
end;

procedure THCView.DoSectionCreateItem(Sender: TObject);
begin
  if Assigned(FOnSectionCreateItem) then
    FOnSectionCreateItem(Sender);
end;

function THCView.DoSectionCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
begin
  if Assigned(FOnSectionCreateStyleItem) then
    Result := FOnSectionCreateStyleItem(AData, AStyleNo)
  else
    Result := nil;
end;

procedure THCView.DoSectionCurParaNoChange(Sender: TObject);
begin
  if Assigned(FOnSectionCurParaNoChange) then
    FOnSectionCurParaNoChange(Sender);
end;

procedure THCView.DoSectionDataChange(Sender: TObject);
begin
  DoChange;
end;

procedure THCView.DoSectionDataCheckUpdateInfo(Sender: TObject);
begin
  CheckUpdateInfo;
end;

function THCView.DoSectionAcceptAction(const Sender: TObject; const AData: THCCustomData;
  const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
begin
  if Assigned(FOnSectionAcceptAction) then
    Result := FOnSectionAcceptAction(Sender, AData, AItemNo, AOffset, AAction)
  else
    Result := True;
end;

procedure THCView.DoAnnotatePreUpdateView(Sender: TObject);
begin
  if FAnnotatePre.Visible then
  begin
    FStyle.UpdateInfoRePaint;
    DoMapChanged;
  end
  else
    UpdateView;
end;

procedure THCView.DoCaretChange;
begin
  GetPagesAndActive;
  if Assigned(FOnCaretChange) then
    FOnCaretChange(Self);
end;

procedure THCView.DoChange;
begin
  SetIsChanged(True);
  DoMapChanged;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function THCView.DoCopyRequest(const AFormat: Word): Boolean;
var
  vTopItem: THCCustomItem;
begin
  vTopItem := ActiveSection.GetTopLevelItem;
  if vTopItem is THCEditItem then
  begin
    if (vTopItem as THCEditItem).SelectTextExists then
    begin
      Result := (AFormat = CF_TEXT) or (AFormat = CF_UNICODETEXT);
      Exit;
    end;
  end;

  Result := True;
end;

procedure THCView.DoCopyAsStream(const AStream: TStream);
begin
end;

procedure THCView.DoHorScroll(Sender: TObject; ScrollCode: TScrollCode; const ScrollPos: Integer);
begin
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret(False);
  CheckUpdateInfo;
  if Assigned(FOnHorScroll) then
    FOnHorScroll(Self);
end;

function THCView.DoUndoNew: THCUndo;
begin
  Result := THCSectionUndo.Create;
  (Result as THCSectionUndo).SectionIndex := FActiveSectionIndex;
  (Result as THCSectionUndo).HScrollPos := FHScrollBar.Position;
  (Result as THCSectionUndo).VScrollPos := FVScrollBar.Position;
  Result.Data := ActiveSection.ActiveData;
end;

function THCView.DoInsertText(const AText: string): Boolean;
begin
  Result := ActiveSection.InsertText(AText);
end;

procedure THCView.DoKillFocus;
begin
  if Assigned(FCaret) then
  begin
    FCaret.Hide(True);
    UpdateView(Bounds(FCaret.X - 1, FCaret.Y, FCaret.Width + 1, FCaret.Height));
  end;
end;

procedure THCView.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const ALoadSectionProc: TLoadSectionProc);
var
  vFileExt: string;
  vVersion: Word;
  vLang: Byte;
begin
  AStream.Position := 0;
  _LoadFileFormatAndVersion(AStream, vFileExt, vVersion, vLang);  // 文件格式和版本
  if (vFileExt <> HC_EXT) and (vFileExt <> 'cff.') then
    raise Exception.Create('加载失败，不是' + HC_EXT + '文件！');
  if vVersion > HC_FileVersionInt then
    raise Exception.Create('加载失败，当前编辑器最高支持版本为'
      + IntToStr(HC_FileVersionInt) + '的文件，无法打开版本为'
      + IntToStr(vVersion) + '的文件！');

  DoLoadStreamBefor(AStream, vVersion);  // 触发加载前事件
  AStyle.LoadFromStream(AStream, vVersion);  // 加载样式表
  ALoadSectionProc(vVersion);  // 加载节数量、节数据
  DoLoadStreamAfter(AStream, vVersion);
  DoMapChanged;
end;

procedure THCView.DoSectionInsertAnnotate(const Sender: TObject;
  const AData: THCCustomData; const ADataAnnotate: THCDataAnnotate);
begin
  FAnnotatePre.InsertDataAnnotate(ADataAnnotate);
end;

procedure THCView.DoSectionInsertItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnSectionInsertItem) then
    FOnSectionInsertItem(Sender, AData, AItem);
end;

function THCView.DoSectionInsertTextBefor(const AData: THCCustomData;
  const AItemNo, AOffset: Integer; const AText: string): Boolean;
begin
  if Assigned(FOnSectionInsertTextBefor) then
    Result := FOnSectionInsertTextBefor(AData, AItemNo, AOffset, AText)
  else
    Result := True;
end;

procedure THCView.DoSectionItemMouseDown(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, AOffset: Integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure THCView.DoSectionItemMouseUp(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, AOffset: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) and (AData.Items[AItemNo].HyperLink <> '')then
    ShellExecute(Application.Handle, nil, PChar(AData.Items[AItemNo].HyperLink), nil, nil, SW_SHOWNORMAL);
end;

procedure THCView.DoSectionItemResize(const AData: THCCustomData;
  const AItemNo: Integer);
begin
  DoViewResize;
end;

procedure THCView.DoSectionDrawItemAnnotate(const Sender: TObject; const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate);
var
  vDrawAnnotate: THCDrawAnnotate;
begin
  vDrawAnnotate := THCDrawAnnotate.Create;
  //vAnnotate.Section := Sender;
  vDrawAnnotate.Data := AData;
  vDrawAnnotate.DrawRect := ADrawRect;
  vDrawAnnotate.DataAnnotate := ADataAnnotate;
  FAnnotatePre.AddDrawAnnotate(vDrawAnnotate);
end;

procedure THCView.DoSectionDrawItemMouseMove(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, AOffset, ADrawItemNo: Integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnSectionDrawItemMouseMove) then
    FOnSectionDrawItemMouseMove(Sender as THCSection, AData, AItemNo, AOffset, ADrawItemNo, Button, Shift, X, Y);
end;

procedure THCView.DoSectionDrawItemPaintAfter(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect;
  const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if AData.Items[AItemNo].HyperLink <> '' then
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clBlue;
    ACanvas.Pen.Width := 1;
    ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Bottom);
    ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom);
  end;

  if Assigned(FOnSectionDrawItemPaintAfter) then
    FOnSectionDrawItemPaintAfter(Sender, AData, AItemNo, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionDrawItemPaintBefor(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect;
  const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnSectionDrawItemPaintBefor) then
    FOnSectionDrawItemPaintBefor(Sender, AData, AItemNo, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionDrawItemPaintContent(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  // 背景处理完，绘制文本前触发，可处理高亮关键字
end;

procedure THCView.DoStyleInvalidateRect(const ARect: TRect);
begin
  UpdateView(ARect);
end;

procedure THCView.DoUndo(const Sender: THCUndo);
begin
  if Sender is THCSectionUndo then
  begin
    if FActiveSectionIndex <> (Sender as THCSectionUndo).SectionIndex then
      SetActiveSectionIndex((Sender as THCSectionUndo).SectionIndex);

    FHScrollBar.Position := (Sender as THCSectionUndo).HScrollPos;
    FVScrollBar.Position := (Sender as THCSectionUndo).VScrollPos;
  end
  else
  if Sender is THCSectionUndoGroupBegin then
  begin
    if FActiveSectionIndex <> (Sender as THCSectionUndoGroupBegin).SectionIndex then
      SetActiveSectionIndex((Sender as THCSectionUndoGroupBegin).SectionIndex);

    FHScrollBar.Position := (Sender as THCSectionUndoGroupBegin).HScrollPos;
    FVScrollBar.Position := (Sender as THCSectionUndoGroupBegin).VScrollPos;
  end;

  ActiveSection.Undo(Sender);
end;

procedure THCView.DoUndoDestroy(const Sender: THCUndo);
begin
  //Sender的Data是ActiveSection.ActiveData，所以不需要释放
end;

function THCView.DoUndoGroupBegin(const AItemNo,
  AOffset: Integer): THCUndoGroupBegin;
begin
  Result := THCSectionUndoGroupBegin.Create;
  (Result as THCSectionUndoGroupBegin).SectionIndex := FActiveSectionIndex;
  (Result as THCSectionUndoGroupBegin).HScrollPos := FHScrollBar.Position;
  (Result as THCSectionUndoGroupBegin).VScrollPos := FVScrollBar.Position;
  Result.Data := ActiveSection.ActiveData;
  Result.CaretDrawItemNo := ActiveSection.ActiveData.CaretDrawItemNo;
end;

function THCView.DoUndoGroupEnd(const AItemNo,
  AOffset: Integer): THCUndoGroupEnd;
begin
  Result := THCSectionUndoGroupEnd.Create;
  (Result as THCSectionUndoGroupEnd).SectionIndex := FActiveSectionIndex;
  (Result as THCSectionUndoGroupEnd).HScrollPos := FHScrollBar.Position;
  (Result as THCSectionUndoGroupEnd).VScrollPos := FVScrollBar.Position;
  Result.Data := ActiveSection.ActiveData;
  Result.CaretDrawItemNo := ActiveSection.ActiveData.CaretDrawItemNo;
end;

procedure THCView.DoLoadStreamAfter(const AStream: TStream; const AFileVersion: Word);
begin
end;

procedure THCView.DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word);
begin
end;

procedure THCView.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);

  DoMapChanged;
end;

procedure THCView.FormatData;
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
  begin
    FSections[i].FormatData;
    FSections[i].BuildSectionPages(0);
  end;

  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret;
  DoMapChanged;
end;

function THCView.GetTopLevelDrawItem: THCCustomDrawItem;
begin
  Result := ActiveSection.GetTopLevelDrawItem;
end;

function THCView.GetTopLevelItem: THCCustomItem;
begin
  Result := ActiveSection.GetTopLevelItem;
end;

function THCView.GetTopLevelRectDrawItemViewCoord: TPoint;
begin
  Result := Self.ActiveSection.GetTopLevelRectDrawItemCoord;  // 有选中时，以选中结束位置的DrawItem格式化坐标
  Result := Self.GetFormatPointToViewCoord(Result);
end;

function THCView.GetTopLevelDrawItemViewCoord: TPoint;
begin
  Result := Self.ActiveSection.GetTopLevelDrawItemCoord;  // 有选中时，以选中结束位置的DrawItem格式化坐标
  Result := Self.GetFormatPointToViewCoord(Result);
end;

function THCView.GetActiveItem: THCCustomItem;
begin
  Result := ActiveSection.GetActiveItem;
end;

function THCView.GetActivePageIndex: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FActiveSectionIndex - 1 do
    Result := Result + FSections[i].PageCount;

  Result := Result + ActiveSection.ActivePageIndex;
end;

function THCView.GetActiveSection: THCSection;
begin
  Result := FSections[FActiveSectionIndex];
end;

function THCView.GetCurParaNo: Integer;
begin
  Result := ActiveSection.CurParaNo;
end;

function THCView.GetCurStyleNo: Integer;
begin
  Result := ActiveSection.CurStyleNo;
end;

function THCView.GetFormatPointToViewCoord(const APoint: TPoint): TPoint;
var
  vSection: THCSection;
  vPageIndex: Integer;
begin
  Result := Point(APoint.X, APoint.Y);
  vSection := Self.ActiveSection;

  if vSection.ActiveData = vSection.Page then
    vPageIndex := vSection.GetPageIndexByFormat(Result.Y)
  else
    vPageIndex := vSection.ActivePageIndex;

  // 映射到节页面(白色区域)
  Result.X := ZoomIn(GetSectionDrawLeft(Self.ActiveSectionIndex)
    + (vSection.GetPageMarginLeft(vPageIndex) + Result.X)) - FHScrollBar.Position;

  if vSection.ActiveData = vSection.Header then  // 页眉
    Result.Y := ZoomIn(GetSectionTopFilm(Self.ActiveSectionIndex)
      + vSection.GetPageTopFilm(vPageIndex)  // 20
      + vSection.GetHeaderPageDrawTop
      + Result.Y)
      //- vSection.GetPageDataFmtTop(vPageIndex))  // 0
      - FVScrollBar.Position
  else
  if vSection.ActiveData = vSection.Footer then  // 页脚
    Result.Y := ZoomIn(GetSectionTopFilm(Self.ActiveSectionIndex)
      + vSection.GetPageTopFilm(vPageIndex)  // 20
      + vSection.PaperHeightPix - vSection.PaperMarginBottomPix
      + Result.Y)
      //- vSection.GetPageDataFmtTop(vPageIndex))  // 0
      - FVScrollBar.Position
  else  // 正文
    Result.Y := ZoomIn(GetSectionTopFilm(Self.ActiveSectionIndex)
      + vSection.GetPageTopFilm(vPageIndex)  // 20
      + vSection.GetHeaderAreaHeight // 94
      + Result.Y
      - vSection.GetPageDataFmtTop(vPageIndex))  // 0
      - FVScrollBar.Position;
end;

procedure THCView.GetSectionByCrood(const X, Y: Integer;
  var ASectionIndex: Integer);
var
  i, vY: Integer;
begin
  ASectionIndex := -1;
  vY := 0;
  for i := 0 to FSections.Count - 1 do
  begin
    vY := vY + FSections[i].GetFilmHeight;

    if vY > Y then
    begin
      ASectionIndex := i;
      Break;
    end;
  end;
  if (ASectionIndex < 0) and (vY + FPagePadding >= Y) then  // 最后一页后面的Padding
    ASectionIndex := FSections.Count - 1;

  if ASectionIndex < 0 then
    ASectionIndex := 0;
end;

function THCView.GetSectionDrawLeft(const ASectionIndex: Integer): Integer;
begin
  if FViewModel = THCViewModel.hvmFilm then
  begin
    if FAnnotatePre.Visible then  // 显示批注
      Result := Max((FViewWidth - ZoomIn(FSections[ASectionIndex].PaperWidthPix + AnnotationWidth)) div 2, ZoomIn(FPagePadding))
    else
      Result := Max((FViewWidth - ZoomIn(FSections[ASectionIndex].PaperWidthPix)) div 2, ZoomIn(FPagePadding));
  end
  else
  if FViewModel = hvmPage then
    Result := Max((FViewWidth - ZoomIn(FSections[ASectionIndex].PaperWidthPix)) div 2, ZoomIn(FPagePadding))
  else
    Result := 0;

  Result := ZoomOut(Result);
end;

function THCView.GetSectionPageIndexByPageIndex(const APageIndex: Integer;
  var ASectionPageIndex: Integer): Integer;
var
  i, vPageCount: Integer;
begin
  Result := -1;
  vPageCount := 0;
  for i := 0 to FSections.Count - 1 do
  begin
    if vPageCount + FSections[i].PageCount > APageIndex then
    begin
      Result := i;  // 找到节序号
      ASectionPageIndex := APageIndex - vPageCount;  // 节中页序号

      Break;
    end
    else
      vPageCount := vPageCount + FSections[i].PageCount;
  end;
end;

function THCView.GetSectionTopFilm(const ASectionIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ASectionIndex - 1 do
    Result := Result + FSections[i].GetFilmHeight;
end;

function THCView.GetShowLineActiveMark: Boolean;
begin
  Result := FSections[0].Page.ShowLineActiveMark;
end;

function THCView.GetShowLineNo: Boolean;
begin
  Result := FSections[0].Page.ShowLineNo;
end;

function THCView.GetShowUnderLine: Boolean;
begin
  Result := FSections[0].Page.ShowUnderLine;
end;

function THCView.GetSymmetryMargin: Boolean;
begin
  Result := ActiveSection.SymmetryMargin;
end;

function THCView.InsertAnnotate(const ATitle, AText: string): Boolean;
begin
  Result := ActiveSection.InsertAnnotate(ATitle, AText);
end;

function THCView.InsertBreak: Boolean;
begin
  Result := Self.ActiveSection.InsertBreak;
end;

function THCView.InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
begin
  Result := ActiveSection.InsertDomain(AMouldDomain);
end;

function THCView.InsertFloatItem(const AFloatItem: THCCustomFloatItem): Boolean;
begin
  Result := ActiveSection.InsertFloatItem(AFloatItem);
end;

function THCView.InsertGifImage(const AFile: string): Boolean;
begin
  Self.BeginUpdate;
  try
    Result := ActiveSection.InsertGifImage(AFile);
  finally
    Self.EndUpdate;
  end;
end;

function THCView.InsertImage(const AFile: string): Boolean;
var
  vWicImage: TWICImage;
begin
  vWicImage := TWICImage.Create;
  try
    vWicImage.LoadFromFile(AFile);
    Result := ActiveSection.InsertImage(vWicImage);
  finally
    vWicImage.Free;
  end;
end;

function THCView.InsertImage(const AImage: TGraphic): Boolean;
begin
  Self.BeginUpdate;
  try
    Result := ActiveSection.InsertImage(AImage);
  finally
    Self.EndUpdate;
  end;
end;

function THCView.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem): Boolean;
begin
  Result := ActiveSection.InsertItem(AIndex, AItem);
end;

function THCView.InsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := ActiveSection.InsertItem(AItem);
end;

function THCView.InsertLine(const ALineHeight: Integer): Boolean;
begin
  Result := ActiveSection.InsertLine(ALineHeight);
end;

function THCView.InsertPageBreak: Boolean;
begin
  Result := Self.ActiveSection.InsertPageBreak;
end;

function THCView.InsertSectionBreak: Boolean;
var
  vSection: THCSection;
begin
  Result := False;
  vSection := NewDefaultSection;
  FSections.Insert(FActiveSectionIndex + 1, vSection);
  FActiveSectionIndex := FActiveSectionIndex + 1;
  Result := True;
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret;
  FStyle.UpdateInfoReScroll;
  DoChange;
end;

function THCView.InsertStream(const AStream: TStream): Boolean;
var
  vStyle: THCStyle;
  vResult: Boolean;
begin
  Result := False;
  vResult := False;

  Self.BeginUpdate;
  try
    vStyle := THCStyle.Create;
    try
      DoLoadFromStream(AStream, vStyle, procedure(const AFileVersion: Word)
        var
          vByte: Byte;
          vSection: THCSection;
          vShowUnderLine: Boolean;  // 下划线
          vDataStream: TMemoryStream;
        begin
          AStream.ReadBuffer(vByte, 1);  // 节数量

          vDataStream := TMemoryStream.Create;
          try
            vSection := THCSection.Create(vStyle);
            try
              vSection.OnCreateItemByStyle := DoSectionCreateStyleItem;  // 样式要保持一致

              // 不循环，只插入第一节的正文
              vSection.LoadFromStream(AStream, vStyle, AFileVersion);
              vDataStream.Clear;
              vSection.Page.SaveToStream(vDataStream);
              vDataStream.Position := 0;
              vDataStream.ReadBuffer(vShowUnderLine, SizeOf(vShowUnderLine));
              vResult := ActiveSection.InsertStream(vDataStream, vStyle, HC_FileVersionInt);  // 只插入第一节的数据
            finally
              FreeAndNil(vSection);
            end;
          finally
            FreeAndNil(vDataStream);
          end;
        end);
    finally
      FreeAndNil(vStyle);
    end;
  finally
    Self.EndUpdate;
  end;

  Result := vResult;
end;

function THCView.ActiveTableDeleteCurCol: Boolean;
begin
  Result := ActiveSection.ActiveTableDeleteCurCol;
end;

function THCView.ActiveTableDeleteCurRow: Boolean;
begin
  Result := ActiveSection.ActiveTableDeleteCurRow;
end;

function THCView.ActiveTableInsertColAfter(const AColCount: Byte): Boolean;
begin
  Result := ActiveSection.ActiveTableInsertColAfter(AColCount);
end;

function THCView.ActiveTableInsertColBefor(const AColCount: Byte): Boolean;
begin
  Result := ActiveSection.ActiveTableInsertColBefor(AColCount);
end;

function THCView.ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;
begin
  Result := ActiveSection.ActiveTableInsertRowAfter(ARowCount);
end;

function THCView.ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;
begin
  Result := ActiveSection.ActiveTableInsertRowBefor(ARowCount);
end;

function THCView.ActiveTableResetRowCol(const ARowCount, AColCount: Byte): Boolean;
begin
  Result := ActiveSection.ActiveTableResetRowCol(ARowCount, AColCount);
end;

function THCView.ActiveTableSplitCurCol: Boolean;
begin
  Result := ActiveSection.ActiveTableSplitCurCol;
end;

function THCView.ActiveTableSplitCurRow: Boolean;
begin
  Result := ActiveSection.ActiveTableSplitCurRow;
end;

function THCView.SetActiveImage(const AImageStream: TStream): Boolean;
begin
  Result := ActiveSection.SetActiveImage(AImageStream);
end;

function THCView.ActiveSectionTopLevelData: THCCustomData;
begin
  Result := ActiveSection.ActiveData.GetTopLevelData;
end;

function THCView.InsertTable(const ARowCount, AColCount: Integer): Boolean;
begin
  Self.BeginUpdate;
  try
    Result := ActiveSection.InsertTable(ARowCount, AColCount);
  finally
    Self.EndUpdate;
  end;
end;

function THCView.InsertText(const AText: string): Boolean;
begin
  Self.BeginUpdate;
  try
    Result := DoInsertText(AText);
  finally
    Self.EndUpdate;
  end;
end;

procedure THCView.KeyDown(var Key: Word; Shift: TShiftState);

  {$REGION '快捷键'}
  function IsCopyShortKey: Boolean;
  begin
    Result := (Shift = [ssCtrl]) and (Key = ord('C'));
  end;

  function IsCopyTextShortKey: Boolean;
  begin
    Result := (Shift = [ssCtrl, ssShift]) and (Key = ord('C'));
  end;

  function IsCutShortKey: Boolean;
  begin
    Result := (Shift = [ssCtrl]) and (Key = ord('X'));
  end;

  function IsPasteShortKey: Boolean;
  begin
    Result := (Shift = [ssCtrl]) and (Key = ord('V'));
  end;

  function IsSelectAllShortKey: Boolean;
  begin
    Result := (Shift = [ssCtrl]) and (Key = ord('A'));
  end;

  function IsUndoKey: Boolean;
  begin
    Result := (Shift = [ssCtrl]) and (Key = ord('Z'));
  end;

  function IsRedoKey: Boolean;
  begin
    Result := (Shift = [ssCtrl]) and (Key = ord('Y'));
  end;
  {$ENDREGION}

begin
  inherited KeyDown(Key, Shift);
  if IsCopyTextShortKey then
    Self.CopyAsText
  else
  if IsCopyShortKey then
    Self.Copy
  else
  if IsCutShortKey then
    Self.Cut
  else
  if IsPasteShortKey then
    Self.Paste
  else
  if IsSelectAllShortKey then
    Self.SelectAll
  else
  if IsUndoKey then
    Self.Undo
  else
  if IsRedoKey then
    Self.Redo
  else
    ActiveSection.KeyDown(Key, Shift);
end;

procedure THCView.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if GetKeyState(VK_CONTROL) < 0 then  // 按下ctrl，不阻止shift，放过上挡键
    Exit;

  ActiveSection.KeyPress(Key);
end;

procedure THCView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  ActiveSection.KeyUp(Key, Shift);
end;

procedure THCView.LoadFromDocumentFile(const AFileName: string; const AExt: string);
begin
  Self.BeginUpdate;
  try
    // 清除撤销恢复数据
    FUndoList.Clear;
    FUndoList.SaveState;
    try
      FUndoList.Enable := False;

      Self.Clear;
      HCViewLoadFromDocumentFile(Self, AFileName, AExt);
      Self.FormatData;
      DoViewResize;
    finally
      FUndoList.RestoreState;
    end;
  finally
    Self.EndUpdate;
  end;
end;

procedure THCView.LoadFromDocumentStream(const AStream: TStream;
  const AExt: string);
begin
  Self.BeginUpdate;
  try
    // 清除撤销恢复数据
    FUndoList.Clear;
    FUndoList.SaveState;
    try
      FUndoList.Enable := False;

      Self.Clear;
      HCViewLoadFromDocumentStream(Self, AStream, AExt);
      Self.FormatData;
      DoViewResize;
    finally
      FUndoList.RestoreState;
    end;
  finally
    Self.EndUpdate;
  end;
end;

function THCView.LoadFromFile(const AFileName: string): Boolean;
var
  vStream: TStream;
begin
  Result := False;
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(vStream);
    if Result then
      FFileName := AFileName;
  finally
    FreeAndNil(vStream);
  end;
end;

function THCView.LoadFromStream(const AStream: TStream): Boolean;
begin
  Result := False;
  if ReadOnly then Exit;

  Self.BeginUpdate;
  try
    // 清除撤销恢复数据
    FUndoList.Clear;
    FUndoList.SaveState;
    try
      FUndoList.Enable := False;
      Self.Clear;

      FStyle.States.Include(hosLoading);
      try
        AStream.Position := 0;
        DoLoadFromStream(AStream, FStyle, procedure(const AFileVersion: Word)
          var
            i: Integer;
            vByte: Byte;
            vSection: THCSection;
          begin
            AStream.ReadBuffer(vByte, 1);  // 节数量
            // 各节数据
            FSections[0].LoadFromStream(AStream, FStyle, AFileVersion);
            for i := 1 to vByte - 1 do
            begin
              vSection := NewDefaultSection;
              vSection.LoadFromStream(AStream, FStyle, AFileVersion);
              FSections.Add(vSection);
            end;
          end);
      finally
        FStyle.States.Exclude(hosLoading);
      end;

      Result := True;
      DoViewResize;
    finally
      FUndoList.RestoreState;
    end;
  finally
    Self.EndUpdate;
  end;
end;

function THCView.LoadFromText(const AText: string): Boolean;
begin
  Result := False;
  if ReadOnly then Exit;

  Self.Clear;
  FStyle.Initialize;

  if AText <> '' then
    Result := ActiveSection.InsertText(AText);
end;

function THCView.LoadFromTextFile(const AFileName: string; const AEncoding: TEncoding): Boolean;
var
  vStream: TMemoryStream;
begin
  Result := False;
  vStream := TMemoryStream.Create;
  try
    vStream.LoadFromFile(AFileName);
    vStream.Position := 0;
    Result := LoadFromTextStream(vStream, AEncoding);
  finally
    FreeAndNil(vStream);
  end;
end;

function THCView.LoadFromTextStream(const AStream: TStream; AEncoding: TEncoding): Boolean;
var
  vSize: Integer;
  vBuffer: TBytes;
  vS: string;
begin
  Result := False;
  vSize := AStream.Size - AStream.Position;
  SetLength(vBuffer, vSize);
  AStream.Read(vBuffer[0], vSize);
  vSize := TEncoding.GetBufferEncoding(vBuffer, AEncoding);
  vS := AEncoding.GetString(vBuffer, vSize, Length(vBuffer) - vSize);
  Result := LoadFromText(vS);
end;

function THCView.LoadFromXml(const AFileName: string): Boolean;
var
  vStream: TMemoryStream;
begin
  Result := False;
  vStream := TMemoryStream.Create;
  try
    vStream.LoadFromFile(AFileName);
    vStream.Position := 0;
    Result := LoadFromXmlStream(vStream);
    if Result then
      FFileName := AFileName;
  finally
    vStream.Free;
  end;
end;

function THCView.LoadFromXmlStream(const AStream: TStream): Boolean;

  {function GetEncodingName(const ARaw: string): TEncoding;
  var
    vPs, vPd: Integer;
    vEncd: string;
  begin
    vPs := Pos('<?xml', ARaw) + 5;
    vPd := Pos('?>', ARaw) - 1;

    vEncd := System.Copy(ARaw, vPs, vPd - vPs + 1);
    if vEncd = 'UTF-8' then
      Result := TEncoding.UTF8
    else
      Result := TEncoding.Unicode;
  end; }

var
  vSection: THCSection;
  vXml: IHCXMLDocument;
  vNode: IHCXMLNode;
  vVersion: string;
  vLang: Byte;
  i, j: Integer;
begin
  Result := False;

  if ReadOnly then Exit;

  Self.BeginUpdate;
  try
    // 清除撤销恢复数据
    FUndoList.Clear;
    FUndoList.SaveState;
    try
      FUndoList.Enable := False;
      Self.Clear;

      vXml := THCXMLDocument.Create(nil);
      //vXml.LoadFromFile(AFileName);
      vXml.LoadFromStream(AStream);
      if vXml.DocumentElement.LocalName = 'HCView' then
      begin
        if vXml.DocumentElement.Attributes['EXT'] <> HC_EXT then Exit;

        vVersion := vXml.DocumentElement.Attributes['ver'];
        vLang := vXml.DocumentElement.Attributes['lang'];

        FStyle.States.Include(hosLoading);
        try
          for i := 0 to vXml.DocumentElement.ChildNodes.Count - 1 do
          begin
            vNode := vXml.DocumentElement.ChildNodes[i];
            if vNode.NodeName = 'style' then
              FStyle.ParseXml(vNode)
            else
            if vNode.NodeName = 'sections' then
            begin
              FSections[0].ParseXml(vNode.ChildNodes[0]);
              for j := 1 to vNode.ChildNodes.Count - 1 do
              begin
                vSection := NewDefaultSection;
                vSection.ParseXml(vNode.ChildNodes[j]);
                FSections.Add(vSection);
              end;
            end;
          end;

          DoMapChanged;
        finally
          FStyle.States.Exclude(hosLoading);
        end;

        Result := True;
        DoViewResize;
      end;
    finally
      FUndoList.RestoreState;
    end;
  finally
    Self.EndUpdate;
  end;
end;

function THCView.MergeTableSelectCells: Boolean;
begin
  Result := ActiveSection.MergeTableSelectCells;
end;

procedure THCView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vSectionIndex, vSectionDrawLeft: Integer;
  vPt: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);

  GetSectionByCrood(ZoomOut(FHScrollBar.Position + X), ZoomOut(FVScrollBar.Position + Y), vSectionIndex);
  if vSectionIndex <> FActiveSectionIndex then
    SetActiveSectionIndex(vSectionIndex);
  if FActiveSectionIndex < 0 then Exit;

  vSectionDrawLeft := GetSectionDrawLeft(FActiveSectionIndex);

  if FAnnotatePre.DrawCount > 0 then  // 有批注被绘制
//  begin
//    //if (X + HScrollValue > vSectionDrawLeft + FSections[FActiveSectionIndex].PaperWidthPix)
//    //  and (X + HScrollValue < vSectionDrawLeft + FSections[FActiveSectionIndex].PaperWidthPix + AnnotationWidth)
//    if PtInRect(FAnnotatePre.DrawRect, Point(X, Y)) then  // 点在批注区域中
//    begin
      FAnnotatePre.MouseDown(ZoomOut(X), ZoomOut(Y));
//      FStyle.UpdateInfoRePaint;
//      //DoSectionDataCheckUpdateInfo(Self);
//      //Exit;
//    end;
//  end;

  // 映射到节页面(白色区域)
  vPt.X := ZoomOut(FHScrollBar.Position + X) - vSectionDrawLeft;
  vPt.Y := ZoomOut(FVScrollBar.Position + Y) - GetSectionTopFilm(FActiveSectionIndex);
  //vPageIndex := FSections[FActiveSectionIndex].GetPageByFilm(vPt.Y);
  FSections[FActiveSectionIndex].MouseDown(Button, Shift, vPt.X, vPt.Y);

  CheckUpdateInfo;  // 换光标、切换激活Item
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure THCView.MouseMove(Shift: TShiftState; X, Y: Integer);

  {$REGION 'ProcessHint'}
  procedure ProcessHint;
  var
    //vPt: Tpoint;
    vHint: string;
  begin
    vHint := ActiveSection.GetHint;
    if vHint <> Hint then
    begin
//      {if CustomHint <> nil then
//        CustomHint.HideHint;}
      Hint := vHint;
      Application.CancelHint;
    end
//    else
//    begin
//      {if CustomHint <> nil then
//        CustomHint.ShowHint(Self)
//      else
//      begin }
//        GetCursorPos(vPt);
//        Application.ActivateHint(vPt);
//     // end;
//    end;
  end;
  {$ENDREGION}

begin
  inherited MouseMove(Shift, X, Y);
  //GetSectionByCrood(FHScrollBar.Value + X, FVScrollBar.Value + Y, vSectionIndex);
  if FActiveSectionIndex >= 0 then  // 按下时在节中
  begin
    FSections[FActiveSectionIndex].MouseMove(Shift,
      ZoomOut(FHScrollBar.Position + X) - GetSectionDrawLeft(FActiveSectionIndex),
      ZoomOut(FVScrollBar.Position + Y) - GetSectionTopFilm(FActiveSectionIndex));

    if ShowHint then
      ProcessHint;

    if FStyle.UpdateInfo.Selecting then
      AutoScrollTimer(True);
  end;

  if FAnnotatePre.DrawCount > 0 then  // 有批注被绘制
    FAnnotatePre.MouseMove(ZoomOut(X), ZoomOut(Y));

  CheckUpdateInfo;  // 可能需要高亮鼠标处Item

  if FStyle.UpdateInfo.Draging then
    Screen.Cursor := GCursor  // 放到OnDrag里是不是就不用设置Screen了或者设置Self.DragKind？
  else
    Cursor := GCursor;
end;

procedure THCView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FStyle.UpdateInfo.Selecting then
    AutoScrollTimer(False);

  if Button = mbRight then Exit;  // 右键弹出菜单
  //GetSectionByCrood(FHScrollBar.Value + X, FVScrollBar.Value + Y, vSectionIndex);
  if FActiveSectionIndex >= 0 then  // 按下时在节中
    FSections[FActiveSectionIndex].MouseUp(Button, Shift,
      ZoomOut(FHScrollBar.Position + X) - GetSectionDrawLeft(FActiveSectionIndex),
      ZoomOut(FVScrollBar.Position + Y) - GetSectionTopFilm(FActiveSectionIndex));

  if FStyle.UpdateInfo.Draging then
    Screen.Cursor := crDefault;

  Cursor := GCursor;

  CheckUpdateInfo;  // 在选中区域中按下不移动弹起鼠标时需要更新

  FStyle.UpdateInfo.Selecting := False;
  FStyle.UpdateInfo.Draging := False;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

function THCView.NewDefaultSection: THCSection;
begin
  Result := THCSection.Create(FStyle);
  // 创建节后马上赋值事件（保证后续插入表格等需要这些事件的操作可获取到事件）
  Result.OnDataChange := DoSectionDataChange;
  Result.OnChangeTopLevelData := DoSectionChangeTopLevelData;
  Result.OnCheckUpdateInfo := DoSectionDataCheckUpdateInfo;
  Result.OnCreateItem := DoSectionCreateItem;
  Result.OnDataAcceptAction := DoSectionAcceptAction;
  Result.OnCreateItemByStyle := DoSectionCreateStyleItem;
  Result.OnPaintDomainRegion := DoSectionPaintDomainRegion;
  Result.OnCanEdit := DoSectionCanEdit;
  Result.OnInsertTextBefor := DoSectionInsertTextBefor;
  Result.OnInsertItem := DoSectionInsertItem;
  Result.OnRemoveItem := DoSectionRemoveItem;
  Result.OnSaveItem := DoSectionSaveItem;
  Result.OnItemMouseDown := DoSectionItemMouseDown;
  Result.OnItemMouseUp := DoSectionItemMouseUp;
  Result.OnDrawItemMouseMove := DoSectionDrawItemMouseMove;
  Result.OnItemResize := DoSectionItemResize;
  Result.OnReadOnlySwitch := DoSectionReadOnlySwitch;
  Result.OnGetScreenCoord := DoSectionGetScreenCoord;
  Result.OnDrawItemPaintAfter := DoSectionDrawItemPaintAfter;
  Result.OnDrawItemPaintBefor := DoSectionDrawItemPaintBefor;
  Result.OnDrawItemPaintContent := DoSectionDrawItemPaintContent;
  Result.OnPaintHeaderBefor := DoSectionPaintHeaderBefor;
  Result.OnPaintHeaderAfter := DoSectionPaintHeaderAfter;
  Result.OnPaintFooterBefor := DoSectionPaintFooterBefor;
  Result.OnPaintFooterAfter := DoSectionPaintFooterAfter;
  Result.OnPaintPageBefor := DoSectionPaintPageBefor;
  Result.OnPaintPageAfter := DoSectionPaintPageAfter;
  Result.OnPaintPaperBefor := DoSectionPaintPaperBefor;
  Result.OnPaintPaperAfter := DoSectionPaintPaperAfter;
  Result.OnInsertAnnotate := DoSectionInsertAnnotate;
  Result.OnRemoveAnnotate := DoSectionRemoveAnnotate;
  Result.OnDrawItemAnnotate := DoSectionDrawItemAnnotate;
  Result.OnGetUndoList := DoSectionGetUndoList;
  Result.OnCurParaNoChange := DoSectionCurParaNoChange;
  Result.OnCaretItemChanged := DoSectionCaretItemChanged;
  Result.OnActivePageChange := DoSectionActivePageChange;
end;

procedure THCView.DoVerScroll(Sender: TObject; ScrollCode: TScrollCode;
  const ScrollPos: Integer);
begin
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret(False);
  CheckUpdateInfo;
  GetPagesAndActive;
  if Assigned(FOnVerScroll) then
    FOnVerScroll(Self);
end;

procedure THCView.DoViewResize;
begin
  if Assigned(FOnViewResize) then
    FOnViewResize(Self);
end;

function THCView.GetPageCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FSections.Count - 1 do
    Result := Result + FSections[i].PageCount;
end;

function THCView.GetPageIndexTop(const APageIndex: Integer): Integer;
var
  vSectionIndex, vPageIndex: Integer;
begin
  vSectionIndex := GetSectionPageIndexByPageIndex(APageIndex, vPageIndex);
  Result := GetSectionTopFilm(vSectionIndex);

  if vPageIndex > 0 then
  begin
    if FSections[vSectionIndex].ViewModel = hvmFilm then
      Result := Result + vPageIndex * (FPagePadding + FSections[vSectionIndex].PaperHeightPix)
    else
    if FSections[vSectionIndex].ViewModel = hvmPage then
      Result := Result + vPageIndex * (FPagePadding + FSections[vSectionIndex].PaperHeightPix)
    else
      Result := Result + vPageIndex * (FPagePadding + FSections[vSectionIndex].GetPageHeight);
  end;
end;

function THCView.GetPagePreviewFirst: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FActiveSectionIndex - 1 do
    Result := Result + FSections[i].PageCount;

  Result := Result + FSections[FActiveSectionIndex].DisplayFirstPageIndex;
end;

procedure THCView.GetPagesAndActive;
begin
  FHScrollBar.Statuses[0].Text := '预览' + IntToStr(PagePreviewFirst + 1)
    + ' 光标' + IntToStr(ActivePageIndex + 1)
    + '/' + IntToStr(PageCount) + '页';
end;

function THCView.GetReadOnly: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FSections.Count - 1 do
  begin
    if not FSections[i].ReadOnly then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure THCView.PageDown;
begin
  DoPageDown(Self);
end;

procedure THCView.PageUp;
begin
  DoPageUp(Self);
end;

procedure THCView.Paint;
begin
  //Canvas.Draw(0, 0, FDataBmp);
  BitBlt(Canvas.Handle, 0, 0, FViewWidth, FViewHeight,
      FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
  Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(Bounds(FVScrollBar.Left, FHScrollBar.Top, FVScrollBar.Width, FHScrollBar.Height));
end;

procedure THCView.Paste;

  {$REGION 'PasteBitmapImage'}
  procedure PasteBitmapImage;
  var
    vImageItem: THCImageItem;
    vTopData: THCRichData;
    vBitmap: TBitmap;
  begin
    vTopData := Self.ActiveSectionTopLevelData as THCRichData;
    vImageItem := THCImageItem.Create(vTopData);

    vBitmap := TBitmap.Create;
    try
      vBitmap.Assign(Clipboard);
      vImageItem.Image.Assign(vBitmap);
    finally
      FreeAndNil(vBitmap);
    end;
    vImageItem.Width := vImageItem.Image.Width;
    vImageItem.Height := vImageItem.Image.Height;

    vImageItem.RestrainSize(vTopData.Width, vImageItem.Height);
    Self.InsertItem(vImageItem);
  end;
  {$ENDREGION}

  {$REGION 'PasteRtf'}
  procedure PasteRtf;
  var
    vData, vLen: Cardinal;
    vStream: TMemoryStream;
    //vBuffer: TBytes;
    //vsRtf: AnsiString;
    //tlist: TStringList;
  begin
    vStream := TMemoryStream.Create;
    try
      Clipboard.Open;
      vData := GetClipboardData(CF_RTF);
      try
        if (vData <> 0) and (vData <> INVALID_HANDLE_VALUE) then
        begin
          vLen := GlobalSize(vData);
          //SetLength(vBuffer, vLen);
          //Move(GlobalLock(vData)^, vBuffer[0], vLen);
          //vsRtf := StringOf(vBuffer);
          //tlist := TStringList.Create;
          //tlist.Text := vsRtf;
          //tlist.SaveToFile('c:\a.txt');
          vStream.Size := vLen;
          Move(GlobalLock(vData)^, vStream.Memory^, vLen);
        end;
      finally
        if vData <> 0 then
          GlobalUnlock(vData);

        Clipboard.Close;
      end;

      THCRtfReader.InsertStream(Self, vStream);
    finally
      FreeAndNil(vStream);
    end;
  end;
  {$ENDREGION}

  {$REGION 'PasteHtml'}
  procedure PasteHtml;
  var
    vMem: Cardinal;
    vHtmlText: string;
    vPHtml: PAnsiChar;  // PUTF8Char
    //vList: TStringList;
  begin
    Clipboard.Open;
    try
      vMem := Clipboard.GetAsHandle(CF_HTML);
      vPHtml := GlobalLock(vMem);
      vHtmlText := UTF8Decode(vPHtml);
      GlobalUnlock(vMem);
    finally
      Clipboard.Close;
    end;

    {vList := TStringList.Create;
    try
      vList.Text := vHtmlText;
      vList.SaveToFile('C:\html.txt');
    finally
      vList.Free;
    end;}

    if not ActiveSection.ParseHtml(vHtmlText) then
    begin
      if Clipboard.HasFormat(CF_TEXT) then
        InsertText(Clipboard.AsText);
    end;
  end;
  {$ENDREGION}

var
  vStream: TMemoryStream;
  vMem: Cardinal;
  vPtr: Pointer;
  vSize: Integer;
  vFileFormat: string;
  vFileVersion: Word;
  vLang: Byte;
  vStyle: THCStyle;
begin
  FStyle.States.Include(hosPasting);
  try
    if Clipboard.HasFormat(HC_FILEFORMAT) and DoPasteRequest(HC_FILEFORMAT) then
    begin
      vStream := TMemoryStream.Create;
      try
        Clipboard.Open;
        try
          vMem := Clipboard.GetAsHandle(HC_FILEFORMAT);
          vSize := GlobalSize(vMem);
          vStream.SetSize(vSize);
          vPtr := GlobalLock(vMem);
          Move(vPtr^, vStream.Memory^, vSize);
          GlobalUnlock(vMem);
        finally
          Clipboard.Close;
        end;
        //
        vStream.Position := 0;
        _LoadFileFormatAndVersion(vStream, vFileFormat, vFileVersion, vLang);  // 文件格式和版本
        if not DoPasteFromStream(vStream) then Exit;

        vStyle := THCStyle.Create;
        try
          vStyle.LoadFromStream(vStream, vFileVersion);
          Self.BeginUpdate;
          try
            ActiveSection.InsertStream(vStream, vStyle, vFileVersion);
          finally
            Self.EndUpdate;
          end;
        finally
          FreeAndNil(vStyle);
        end;
      finally
        vStream.Free;
      end;
    end
    else
    if Clipboard.HasFormat(CF_RTF) and DoPasteRequest(CF_RTF) then
      PasteRtf
    else
    if Clipboard.HasFormat(CF_HTML) and DoPasteRequest(CF_HTML) then
      PasteHtml
    else
    if Clipboard.HasFormat(CF_TEXT) and DoPasteRequest(CF_TEXT) then
      InsertText(Clipboard.AsText)
    else
    if Clipboard.HasFormat(CF_UNICODETEXT) and DoPasteRequest(CF_UNICODETEXT) then
      InsertText(Clipboard.AsText)
    else
    if Clipboard.HasFormat(CF_BITMAP) and DoPasteRequest(CF_BITMAP) then
      PasteBitmapImage;
  finally
    FStyle.States.Exclude(hosPasting);
  end;
end;

function THCView.Print(const APrinter: string; const ACopies: Integer = 1): TPrintResult;
begin
  Result := Print(APrinter, 0, PageCount - 1, ACopies);
end;

function THCView.Print: TPrintResult;
begin
  Result := Print('');
end;

function THCView.Print(const APrinter: string; const ACopies: Integer;
  const APages: array of Integer): TPrintResult;
var
  i, vFirstPageIndex, vSectionIndex, vPrintWidth, vPrintHeight,
  vPrintOffsetX, vPrintOffsetY: Integer;
  vPrintCanvas: TCanvas;
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
begin
  Result := prError;

  if APrinter <> '' then
    HCPrinter.PrinterIndex := HCPrinter.Printers.IndexOf(APrinter);

  if HCPrinter.PrinterIndex < 0 then Exit;

  HCPrinter.Title := FFileName;

  // 取打印机打印区域相关参数
  vPrintOffsetX := -GetDeviceCaps(HCPrinter.Handle, PHYSICALOFFSETX);  // 73
  vPrintOffsetY := -GetDeviceCaps(HCPrinter.Handle, PHYSICALOFFSETY);  // 37

  HCPrinter.Copies := ACopies;

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;

    HCPrinter.BeginDoc(False);
    try
      vPrintCanvas := TCanvas.Create;
      try
        vPrintCanvas.Handle := HCPrinter.Canvas.Handle;  // 为什么不用vPrintCanvas中介打印就不行呢？

        for i := Low(APages) to High(APages) do
        begin
          // 根据页码获取起始节和结束节
          vSectionIndex := GetSectionPageIndexByPageIndex(APages[i], vFirstPageIndex);
          if vPaintInfo.SectionIndex <> vSectionIndex then
          begin
            vPaintInfo.DPI := GetDeviceCaps(vPrintCanvas.Handle, LOGPIXELSX);
            vPaintInfo.SectionIndex := vSectionIndex;

            SetPrintBySectionInfo(vSectionIndex);
            HCPrinter.NewPage(False);

            vPrintWidth := GetDeviceCaps(HCPrinter.Handle, PHYSICALWIDTH);  // 4961
            vPrintHeight := GetDeviceCaps(HCPrinter.Handle, PHYSICALHEIGHT);  // 7016

            if FSections[vSectionIndex].Page.DataAnnotates.Count > 0 then
            begin
              vPaintInfo.ScaleX := vPrintWidth / (FSections[vSectionIndex].PaperWidthPix + AnnotationWidth);
              vPaintInfo.ScaleY := vPrintHeight / (FSections[vSectionIndex].PaperHeightPix + AnnotationWidth * vPrintHeight / vPrintWidth);
              vPaintInfo.Zoom := FSections[vSectionIndex].PaperWidthPix / (FSections[vSectionIndex].PaperWidthPix + AnnotationWidth);
            end
            else
            begin
              vPaintInfo.ScaleX := vPrintWidth / FSections[vSectionIndex].PaperWidthPix;  // GetDeviceCaps(HCPrinter.Handle, LOGPIXELSX) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSX);
              vPaintInfo.ScaleY := vPrintHeight / FSections[vSectionIndex].PaperHeightPix;  // GetDeviceCaps(HCPrinter.Handle, LOGPIXELSY) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSY);
              vPaintInfo.Zoom := 1;
            end;

            vPaintInfo.WindowWidth := vPrintWidth;  // FSections[vStartSection].PaperWidthPix;
            vPaintInfo.WindowHeight := vPrintHeight;  // FSections[vStartSection].PaperHeightPix;

            vPrintOffsetX := Round(vPrintOffsetX / vPaintInfo.ScaleX);
            vPrintOffsetY := Round(vPrintOffsetY / vPaintInfo.ScaleY);
          end
          else
            HCPrinter.NewPage(False);

          vScaleInfo := vPaintInfo.ScaleCanvas(vPrintCanvas);
          try
            vPaintInfo.PageIndex := APages[i];

            FSections[vSectionIndex].PaintPaper(vFirstPageIndex,
              vPrintOffsetX, vPrintOffsetY, vPrintCanvas, vPaintInfo);

            HCPrinter.EndPage;
          finally
            vPaintInfo.RestoreCanvasScale(vPrintCanvas, vScaleInfo);
          end;
        end;
      finally
        vPrintCanvas.Handle := 0;
        vPrintCanvas.Free;
      end;
    finally
      if HCPrinter.Printing then
        HCPrinter.EndDoc;
    end;
  finally
    vPaintInfo.Free;
  end;

  Result := prOk;
end;

function THCView.Print(const APrinter: string; const AStartPageIndex,
  AEndPageIndex, ACopies: Integer): TPrintResult;
var
  i: Integer;
  vPages: array of Integer;
begin
  SetLength(vPages, AEndPageIndex - AStartPageIndex + 1);
  for i := AStartPageIndex to AEndPageIndex do
    vPages[i - AStartPageIndex] := i;

  Result := Print(APrinter, ACopies, vPages);
end;

function THCView.PrintCurPageByActiveLine(const APrinter: string; const APrintHeader,
  APrintFooter: Boolean): TPrintResult;
var
  vPt: TPoint;
  vPrintCanvas: TCanvas;
  vPrintWidth, vPrintHeight, vPrintOffsetX, vPrintOffsetY: Integer;
  //vMarginLeft, vMarginRight: Integer;
  vRect: TRect;
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
begin
  Result := TPrintResult.prError;

  if APrinter <> '' then
    HCPrinter.PrinterIndex := HCPrinter.Printers.IndexOf(APrinter);

  if HCPrinter.PrinterIndex < 0 then Exit;

  HCPrinter.Title := FFileName;

  vPrintOffsetX := GetDeviceCaps(HCPrinter.Handle, PHYSICALOFFSETX);  // 90
  vPrintOffsetY := GetDeviceCaps(HCPrinter.Handle, PHYSICALOFFSETY);  // 99

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;
    vPaintInfo.SectionIndex := Self.ActiveSectionIndex;
    vPaintInfo.PageIndex := Self.ActiveSection.ActivePageIndex;

    SetPrintBySectionInfo(Self.ActiveSectionIndex);

    vPrintWidth := GetDeviceCaps(HCPrinter.Handle, PHYSICALWIDTH);
    vPrintHeight := GetDeviceCaps(HCPrinter.Handle, PHYSICALHEIGHT);

    if Self.ActiveSection.Page.DataAnnotates.Count > 0 then
    begin
      vPaintInfo.ScaleX := vPrintWidth / (Self.ActiveSection.PaperWidthPix + AnnotationWidth);
      vPaintInfo.ScaleY := vPrintHeight / (Self.ActiveSection.PaperHeightPix + AnnotationWidth * vPrintHeight / vPrintWidth);
      vPaintInfo.Zoom := Self.ActiveSection.PaperWidthPix / (Self.ActiveSection.PaperWidthPix + AnnotationWidth);
    end
    else
    begin
      vPaintInfo.ScaleX := vPrintWidth / Self.ActiveSection.PaperWidthPix;  // GetDeviceCaps(HCPrinter.Handle, LOGPIXELSX) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSX);
      vPaintInfo.ScaleY := vPrintHeight / Self.ActiveSection.PaperHeightPix;  // GetDeviceCaps(HCPrinter.Handle, LOGPIXELSY) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSY);
      vPaintInfo.Zoom := 1;
    end;

    vPaintInfo.WindowWidth := vPrintWidth;  // FSections[vStartSection].PaperWidthPix;
    vPaintInfo.WindowHeight := vPrintHeight;  // FSections[vStartSection].PaperHeightPix;

    vPrintOffsetX := Round(vPrintOffsetX / vPaintInfo.ScaleX);
    vPrintOffsetY := Round(vPrintOffsetY / vPaintInfo.ScaleY);

    HCPrinter.BeginDoc;
    try
      vPrintCanvas := TCanvas.Create;
      try
        vPrintCanvas.Handle := HCPrinter.Canvas.Handle;  // 为什么不用vPageCanvas中介打印就不行呢？
        vPaintInfo.DPI := GetDeviceCaps(vPrintCanvas.Handle, LOGPIXELSX);
        vScaleInfo := vPaintInfo.ScaleCanvas(vPrintCanvas);
        try
          Self.ActiveSection.PaintPaper(Self.ActiveSection.ActivePageIndex,
            vPrintOffsetX, vPrintOffsetY, vPrintCanvas, vPaintInfo);

          if Self.ActiveSection.ActiveData = Self.ActiveSection.Page then
          begin
            vPt := Self.ActiveSection.GetTopLevelDrawItemCoord;
            vPt.Y := vPt.Y - ActiveSection.GetPageDataFmtTop(Self.ActiveSection.ActivePageIndex);
          end
          else
          begin
            Result := TPrintResult.prNoSupport;
            Exit;
          end;

          //Self.ActiveSection.GetPageMarginLeftAndRight(Self.ActiveSection.ActivePageIndex,
          //  vMarginLeft, vMarginRight);

          // "抹"掉不需要显示的地方
          vPrintCanvas.Brush.Color := clWhite;

          if APrintHeader then  // 打印页眉
            vRect := Bounds(vPrintOffsetX,// + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight,  // 页眉下边
              Self.ActiveSection.PaperWidthPix,// - vMarginLeft - vMarginRight,
              vPt.Y)
          else  // 不打印页眉
            vRect := Bounds(vPrintOffsetX,// + vMarginLeft, 防止页眉浮动Item在页边距中
              vPrintOffsetY,
              Self.ActiveSection.PaperWidthPix,// - vMarginLeft - vMarginRight,
              Self.ActiveSection.GetHeaderAreaHeight + vPt.Y);

          vPrintCanvas.FillRect(vRect);

          if not APrintFooter then  // 不打印页脚
          begin
            vRect := Bounds(vPrintOffsetX,// + vMarginLeft, 防止页脚浮动Item在页边距中
              vPrintOffsetY + Self.ActiveSection.PaperHeightPix - Self.ActiveSection.PaperMarginBottomPix,
              Self.ActiveSection.PaperWidthPix,// - vMarginLeft - vMarginRight,
              Self.ActiveSection.PaperMarginBottomPix);

            vPrintCanvas.FillRect(vRect);
          end;
        finally
          vPaintInfo.RestoreCanvasScale(vPrintCanvas, vScaleInfo);
        end;
      finally
        vPrintCanvas.Handle := 0;
        vPrintCanvas.Free;
      end;
    finally
      HCPrinter.EndDoc;
    end;
  finally
    vPaintInfo.Free;
  end;

  Result := TPrintResult.prOk;
end;

function THCView.PrintCurPageByItemRange(const APrinter: string; const APrintHeader, APrintFooter: Boolean;
  const AStartItemNo, AStartOffset, AEndItemNo, AEndOffset: Integer): TPrintResult;
var
  vData: THCRichData;
  vPt: TPoint;
  vPrintCanvas: TCanvas;
  vPrintWidth, vPrintHeight, vPrintOffsetX, vPrintOffsetY: Integer;
  vMarginLeft, vMarginRight, vDrawItemNo: Integer;
  vRect: TRect;
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
begin
  // 注意：此方法需要起始Item格式化第一个DrawItem和结束ItemNo格式化最后一个DrawItem在同一页
  Result := TPrintResult.prError;

  if APrinter <> '' then
    HCPrinter.PrinterIndex := HCPrinter.Printers.IndexOf(APrinter);

  if HCPrinter.PrinterIndex < 0 then Exit;

  HCPrinter.Title := FFileName;

  vPrintOffsetX := GetDeviceCaps(HCPrinter.Handle, PHYSICALOFFSETX);  // 90
  vPrintOffsetY := GetDeviceCaps(HCPrinter.Handle, PHYSICALOFFSETY);  // 99

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;
    vPaintInfo.SectionIndex := Self.ActiveSectionIndex;
    vPaintInfo.PageIndex := Self.ActiveSection.ActivePageIndex;

    SetPrintBySectionInfo(Self.ActiveSectionIndex);

    vPrintWidth := GetDeviceCaps(HCPrinter.Handle, PHYSICALWIDTH);
    vPrintHeight := GetDeviceCaps(HCPrinter.Handle, PHYSICALHEIGHT);

    if Self.ActiveSection.Page.DataAnnotates.Count > 0 then
    begin
      vPaintInfo.ScaleX := vPrintWidth / (Self.ActiveSection.PaperWidthPix + AnnotationWidth);
      vPaintInfo.ScaleY := vPrintHeight / (Self.ActiveSection.PaperHeightPix + AnnotationWidth * vPrintHeight / vPrintWidth);
      vPaintInfo.Zoom := Self.ActiveSection.PaperWidthPix / (Self.ActiveSection.PaperWidthPix + AnnotationWidth);
    end
    else
    begin
      vPaintInfo.ScaleX := vPrintWidth / Self.ActiveSection.PaperWidthPix;  // GetDeviceCaps(HCPrinter.Handle, LOGPIXELSX) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSX);
      vPaintInfo.ScaleY := vPrintHeight / Self.ActiveSection.PaperHeightPix;  // GetDeviceCaps(HCPrinter.Handle, LOGPIXELSY) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSY);
      vPaintInfo.Zoom := 1;
    end;

    vPaintInfo.WindowWidth := vPrintWidth;  // FSections[vStartSection].PaperWidthPix;
    vPaintInfo.WindowHeight := vPrintHeight;  // FSections[vStartSection].PaperHeightPix;

    vPrintOffsetX := Round(vPrintOffsetX / vPaintInfo.ScaleX);
    vPrintOffsetY := Round(vPrintOffsetY / vPaintInfo.ScaleY);

    HCPrinter.BeginDoc;
    try
      vPrintCanvas := TCanvas.Create;
      try
        vPrintCanvas.Handle := HCPrinter.Canvas.Handle;  // 为什么不用vPageCanvas中介打印就不行呢？
        vPaintInfo.DPI := GetDeviceCaps(vPrintCanvas.Handle, LOGPIXELSX);
        vScaleInfo := vPaintInfo.ScaleCanvas(vPrintCanvas);
        try
          Self.ActiveSection.PaintPaper(Self.ActiveSection.ActivePageIndex,
            vPrintOffsetX, vPrintOffsetY, vPrintCanvas, vPaintInfo);

          if Self.ActiveSection.ActiveData = Self.ActiveSection.Page then
          begin
            vData := Self.ActiveSection.ActiveData;
            vDrawItemNo := vData.GetDrawItemNoByOffset(AStartItemNo, AStartOffset);
            vPt := vData.DrawItems[vDrawItemNo].Rect.TopLeft;
            vPt.Y := vPt.Y - ActiveSection.GetPageDataFmtTop(Self.ActiveSection.ActivePageIndex);
          end
          else
          begin
            Result := TPrintResult.prNoSupport;
            Exit;
          end;

          Self.ActiveSection.GetPageMarginLeftAndRight(Self.ActiveSection.ActivePageIndex,
            vMarginLeft, vMarginRight);

          // "抹"掉不需要显示的地方
          vPrintCanvas.Brush.Color := clWhite;

          // 抹选中内容上面
          if APrintHeader then  // 打印页眉
            vRect := Bounds(vPrintOffsetX,  // + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight,  // 页眉下边
              Self.ActiveSection.PaperWidthPix,  // - vMarginLeft - vMarginRight,
              vPt.Y)
          else  // 不打印页眉
            vRect := Bounds(vPrintOffsetX, // + vMarginLeft, 防止页眉浮动Item在页边距中
              vPrintOffsetY,
              Self.ActiveSection.PaperWidthPix,  // - vMarginLeft - vMarginRight,
              Self.ActiveSection.GetHeaderAreaHeight + vPt.Y);

          vPrintCanvas.FillRect(vRect);

          // 抹选中内容起始左边
          vRect := Bounds(vPrintOffsetX + vMarginLeft,
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y,
            vData.DrawItems[vDrawItemNo].Rect.Left + vData.GetDrawItemOffsetWidth(vDrawItemNo, AStartOffset - vData.DrawItems[vDrawItemNo].CharOffs + 1),
            vData.DrawItems[vDrawItemNo].Rect.Height);

          vPrintCanvas.FillRect(vRect);

          // 抹选中内容结束右边
          vDrawItemNo := vData.GetDrawItemNoByOffset(AEndItemNo, AEndOffset);
          vPt := vData.DrawItems[vDrawItemNo].Rect.TopLeft;
          vPt.Y := vPt.Y - ActiveSection.GetPageDataFmtTop(Self.ActiveSection.ActivePageIndex);

          vRect := Rect(vPrintOffsetX + vMarginLeft + vData.DrawItems[vDrawItemNo].Rect.Left +
              vData.GetDrawItemOffsetWidth(vDrawItemNo, AEndOffset - vData.DrawItems[vDrawItemNo].CharOffs + 1),
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y,
            vPrintOffsetX + Self.ActiveSection.PaperWidthPix - vMarginRight,
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y + vData.DrawItems[vDrawItemNo].Rect.Height);

          vPrintCanvas.FillRect(vRect);

          // 抹选中内容下面
          if not APrintFooter then  // 不打印页脚
          begin
            vRect := Rect(vPrintOffsetX,// + vMarginLeft, 防止页脚浮动Item在页边距中
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y + vData.DrawItems[vDrawItemNo].Rect.Height,
              vPrintOffsetX + Self.ActiveSection.PaperWidthPix,// - vMarginRight,
              vPrintOffsetY + Self.ActiveSection.PaperHeightPix);

            vPrintCanvas.FillRect(vRect);
          end
          else  // 打印页脚
          begin
            vRect := Rect(vPrintOffsetX,  // + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y + vData.DrawItems[vDrawItemNo].Rect.Height,
              vPrintOffsetX + Self.ActiveSection.PaperWidthPix,  // - vMarginRight,
              vPrintOffsetY + Self.ActiveSection.PaperHeightPix - Self.ActiveSection.PaperMarginBottomPix);

            vPrintCanvas.FillRect(vRect);
          end;
        finally
          vPaintInfo.RestoreCanvasScale(vPrintCanvas, vScaleInfo);
        end;
      finally
        vPrintCanvas.Handle := 0;
        vPrintCanvas.Free;
      end;
    finally
      HCPrinter.EndDoc;
    end;
  finally
    vPaintInfo.Free;
  end;

  Result := TPrintResult.prOk;
end;

function THCView.PrintCurPageSelected(const APrinter: string; const APrintHeader,
  APrintFooter: Boolean): TPrintResult;
begin
  if Self.ActiveSection.ActiveData.SelectExists(False) then
  begin
    Result := PrintCurPageByItemRange(APrinter, APrintHeader, APrintFooter,
      Self.ActiveSection.ActiveData.SelectInfo.StartItemNo,
      Self.ActiveSection.ActiveData.SelectInfo.StartItemOffset,
      Self.ActiveSection.ActiveData.SelectInfo.EndItemNo,
      Self.ActiveSection.ActiveData.SelectInfo.EndItemOffset);
  end
  else
    Result := TPrintResult.prNoSupport;
end;

function THCView.PrintEven(const APrinter: string): TPrintResult;
var
  i: Integer;
  vPages: array of Integer;
begin
  for i := 0 to PageCount - 1 do
  begin
    if Odd(i) then  // 奇数序号是偶数页
    begin
      SetLength(vPages, Length(vPages) + 1);
      vPages[Length(vPages) - 1] := i;
    end;
  end;

  Result := Print(APrinter, 1, vPages);  // 奇数页
end;

function THCView.PrintOdd(const APrinter: string): TPrintResult;
var
  i: Integer;
  vPages: array of Integer;
begin
  for i := 0 to PageCount - 1 do
  begin
    if not Odd(i) then  // 偶数序号是奇数页
    begin
      SetLength(vPages, Length(vPages) + 1);
      vPages[Length(vPages) - 1] := i;
    end;
  end;

  Result := Print(APrinter, 1, vPages);  // 奇数页
end;

procedure THCView.ActiveItemReAdaptEnvironment;
begin
  ActiveSection.ActiveItemReAdaptEnvironment;
end;

procedure THCView.ReBuildCaret;
var
  vCaretInfo: THCCaretInfo;
begin
  if not Assigned(FCaret) then Exit;

  if (not Self.Focused) or ((not FStyle.UpdateInfo.Draging) and ActiveSection.SelectExists) then
  begin
    FCaret.Hide;
    Exit;
  end;

  { 初始化光标信息，为处理表格内往外迭代，只能放在这里 }
  vCaretInfo.X := 0;
  vCaretInfo.Y := 0;
  vCaretInfo.Height := 0;
  vCaretInfo.Visible := True;

  ActiveSection.GetPageCaretInfo(vCaretInfo);

  if not vCaretInfo.Visible then
  begin
    FCaret.Hide;
    Exit;
  end;

  vCaretInfo.Y := vCaretInfo.Y + GetSectionTopFilm(FActiveSectionIndex);
  FVScrollBar.SetAreaPos(-1, vCaretInfo.Y, vCaretInfo.Height);

  FCaret.X := ZoomIn(GetSectionDrawLeft(FActiveSectionIndex) + vCaretInfo.X) - FHScrollBar.Position;
  FCaret.Y := ZoomIn(vCaretInfo.Y) - FVScrollBar.Position;
  FCaret.Height := ZoomIn(vCaretInfo.Height);

  if not FStyle.UpdateInfo.ReScroll then // 滚动条滚动触发的获取光标位置，可能将光标卷掉看不见
  begin
    if (FCaret.X < 0) or (FCaret.X > FViewWidth) then
    begin
      FCaret.Hide;
      Exit;
    end;

    if (FCaret.Y + FCaret.Height < 0) or (FCaret.Y > FViewHeight) then
    begin
      FCaret.Hide;
      Exit;
    end;
  end
  else  // 非滚动条(方向键、点击等)引起的光标位置变化
  begin
    if FCaret.Height < FViewHeight then
    begin
      if not FCaret.VScroll then
      begin
        FCaret.VScroll := True;  // 防止下面滚动变化后再次取滚动条位置，造成死循环
        try
          if FCaret.Y < 0 then
            FVScrollBar.Position := FVScrollBar.Position + FCaret.Y - FPagePadding
          else
          if FCaret.Y + FCaret.Height + FPagePadding > FViewHeight then
            FVScrollBar.Position := FVScrollBar.Position + FCaret.Y + FCaret.Height + FPagePadding - FViewHeight;
        finally
          FCaret.VScroll := False;
        end;
      end;

      if not FCaret.HScroll then
      begin
        FCaret.HScroll := True;
        try
          if FCaret.X < 0 then
            FHScrollBar.Position := FHScrollBar.Position + FCaret.X - FPagePadding
          else
          if FCaret.X + FPagePadding > FViewWidth then
            FHScrollBar.Position := FHScrollBar.Position + FCaret.X + FPagePadding - FViewWidth;
        finally
          FCaret.HScroll := False;
        end;
      end;
    end;
  end;

  if FCaret.VScroll or FCaret.HScroll then Exit;  //Exit;  // 防止滚动条重新计算光标位置后重复执行下面的代码

  if FCaret.Y + FCaret.Height > FViewHeight then
    FCaret.Height := FViewHeight - FCaret.Y;

  FCaret.Show;
  DoCaretChange;
end;

procedure THCView.Redo;
begin
  FStyle.States.Include(THCState.hosRedoing);
  try
    if FUndoList.Enable then  // 恢复过程不要产生新的Redo
    begin
      try
        FUndoList.Enable := False;

        BeginUpdate;
        try
          FUndoList.Redo;
        finally
          EndUpdate;
        end;
      finally
        FUndoList.Enable := True;
      end;
    end;
  finally
    FStyle.States.Exclude(THCState.hosRedoing);
  end;
end;

function THCView.Replace(const AText: string): Boolean;
begin
  Result := Self.ActiveSection.Replace(AText);
end;

procedure THCView.FormatSection(const ASectionIndex: Integer);
begin
  FSections[ASectionIndex].FormatData;
  FSections[ASectionIndex].BuildSectionPages(0);
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret;

  DoChange;
end;

procedure THCView.ResetActiveSectionMargin;
begin
  ActiveSection.ResetMargin;
  DoViewResize;
end;

procedure THCView.Resize;
begin
  inherited;

  GetViewWidth;
  GetViewHeight;

  if (FViewWidth > 0) and (FViewHeight > 0) then
    FDataBmp.SetSize(FViewWidth, FViewHeight);  // 设置为除滚动条外的大小

  if FAutoZoom then
  begin
    if FAnnotatePre.Visible then  // 显示批注
      FZoom := (FViewWidth - FPagePadding * 2) / (ActiveSection.PaperWidthPix + AnnotationWidth)
    else
      FZoom := (FViewWidth - FPagePadding * 2) / ActiveSection.PaperWidthPix;
  end;

  CalcScrollRang;

  FStyle.UpdateInfoRePaint;
  if FCaret <> nil then
    FStyle.UpdateInfoReCaret(False);

  CheckUpdateInfo;
  DoViewResize;
end;

procedure THCView.SaveToDocumentFile(const AFileName, AExt: string);
begin
  HCViewSaveToDocumentFile(Self, AFileName, AExt);
end;

procedure THCView.SaveToFile(const AFileName: string; const AQuick: Boolean = False);
var
  vStream: TStream;
begin
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(vStream, AQuick);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure THCView.SaveToHtml(const AFileName: string; const ASeparateSrc: Boolean = False);
var
  vHtmlTexts: TStrings;
  i: Integer;
  vPath: string;
begin
  FUndoList.Clear; // 防止删除样式后的撤销找不到原样式
  DeleteUnUsedStyle(FStyle, FSections, [saHeader, saPage, saFooter]);

  FStyle.GetHtmlFileTempName(True);
  if ASeparateSrc then
    vPath := ExtractFilePath(AFileName)
  else
    vPath := '';

  vHtmlTexts := TStringList.Create;
  try
    vHtmlTexts.Add('<!DOCTYPE HTML>');
    vHtmlTexts.Add('<html>');
    vHtmlTexts.Add('<head>');
    vHtmlTexts.Add('<title>');
    vHtmlTexts.Add('</title>');

    vHtmlTexts.Add(FStyle.ToCSS);

    vHtmlTexts.Add('</head>');

    vHtmlTexts.Add('<body>');
    for i := 0 to FSections.Count - 1 do
      vHtmlTexts.Add(FSections[i].ToHtml(vPath));

    vHtmlTexts.Add('</body>');
    vHtmlTexts.Add('</html>');

    vHtmlTexts.SaveToFile(AFileName);
  finally
    FreeAndNil(vHtmlTexts);
  end;
end;

procedure THCView.SaveToImage(const APath, APrefix, AImageType: string;
  const AOnePaper: Boolean);
var
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
  i, vWidth, vHeight, vSectionIndex, vSectionPageIndex, vTop: Integer;
  vBmp: TBitmap;
begin
  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.ScaleX := 1;
    vPaintInfo.ScaleY := 1;
    vPaintInfo.Zoom := 1;
    vPaintInfo.Print := True;
    vPaintInfo.DPI := HCUnitConversion.PixelsPerInchX;
    vPaintInfo.ViewModel := THCViewModel.hvmFilm;

    if AOnePaper then
    begin
      vWidth := 0;
      vHeight := 0;

      for i := 0 to FSections.Count - 1 do
      begin
        vHeight := vHeight + FSections[i].PaperHeightPix * FSections[i].PageCount;
        if vWidth < FSections[i].PaperWidthPix then
          vWidth := FSections[i].PaperWidthPix;
      end;

      vPaintInfo.WindowWidth := vWidth;
      vPaintInfo.WindowHeight := vHeight;

      vBmp := TBitmap.Create;
      try
        vBmp.SetSize(vWidth, vHeight);

        vSectionIndex := 0;
        vSectionPageIndex := 0;
        vTop := 0;

        for i := 0 to Self.PageCount - 1 do
        begin
          vSectionIndex := GetSectionPageIndexByPageIndex(i, vSectionPageIndex);
          //vWidth := FSections[vSectionIndex].PaperWidthPix;
          vHeight := FSections[vSectionIndex].PaperHeightPix;

          vBmp.Canvas.Brush.Color := clWhite;
          vBmp.Canvas.FillRect(Rect(0, vTop, vWidth, vTop + vHeight));

          vScaleInfo := vPaintInfo.ScaleCanvas(vBmp.Canvas);
          try
            FSections[vSectionIndex].PaintPaper(vSectionPageIndex, 0, vTop, vBmp.Canvas, vPaintInfo);
            vTop := vTop + vHeight;
          finally
            vPaintInfo.RestoreCanvasScale(vBmp.Canvas, vScaleInfo);
          end;
        end;

        if AImageType = 'BMP' then
          vBmp.SaveToFile(APath + APrefix + '.bmp')
        else
        if AImageType = 'JPG' then
          BitmapSaveAsJPGE(vBmp, APath + APrefix + '.jpg')
        else
          BitmapSaveAsPNG(vBmp, APath + APrefix + '.png');
      finally
        FreeAndNil(vBmp);
      end;
    end
    else
    begin
      vSectionIndex := 0;
      vSectionPageIndex := 0;
      for i := 0 to Self.PageCount - 1 do
      begin
        vSectionIndex := GetSectionPageIndexByPageIndex(i, vSectionPageIndex);

        vBmp := TBitmap.Create;
        try
          vBmp.SetSize(FSections[vSectionIndex].PaperWidthPix, FSections[vSectionIndex].PaperHeightPix);
          vBmp.Canvas.Brush.Color := clWhite;
          vBmp.Canvas.FillRect(Rect(0, 0, vBmp.Width, vBmp.Height));

          vPaintInfo.WindowWidth := vBmp.Width;
          vPaintInfo.WindowHeight := vBmp.Height;
          vScaleInfo := vPaintInfo.ScaleCanvas(vBmp.Canvas);
          try
            vBmp.Canvas.Brush.Color := clWhite;
            vBmp.Canvas.FillRect(Rect(0, 0, vBmp.Width, vBmp.Height));
            FSections[vSectionIndex].PaintPaper(vSectionPageIndex, 0, 0, vBmp.Canvas, vPaintInfo);
          finally
            vPaintInfo.RestoreCanvasScale(vBmp.Canvas, vScaleInfo);
          end;

          if AImageType = 'BMP' then
            vBmp.SaveToFile(APath + APrefix + IntToStr(i + 1) + '.bmp')
          else
          if AImageType = 'JPG' then
            BitmapSaveAsJPGE(vBmp, aPath + APrefix + IntToStr(i + 1) + '.jpg')
          else
            BitmapSaveAsPNG(vBmp, APath + APrefix + IntToStr(i + 1) + '.png')
        finally
          FreeAndNil(vBmp);
        end;
      end;
    end;
  finally
    FreeAndNil(vPaintInfo);
  end;
end;

procedure THCView.SaveToPDF(const AFileName: string);
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    SaveToPDFStream(vStream);
    vStream.SaveToFile(AFileName);
  finally
    vStream.Free;
  end;
end;

function THCView.SaveToText: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FSections.Count - 1 do  // 各节数据
    Result := Result + sLineBreak + FSections[i].SaveToText;
end;

procedure THCView.SaveToTextFile(const AFileName: string; const AEncoding: TEncoding);
var
  vStream: TStream;
begin
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToTextStream(vStream, AEncoding);
  finally
    vStream.Free;
  end;
end;

procedure THCView.SaveToTextStream(const AStream: TStream; const AEncoding: TEncoding);
var
  vText: string;
  vBuffer, vPreamble: TBytes;
begin
  vText := SaveToText;

  vBuffer := AEncoding.GetBytes(vText);
  vPreamble := AEncoding.GetPreamble;

  if Length(vPreamble) > 0 then
    AStream.WriteBuffer(vPreamble[0], Length(vPreamble));

  AStream.WriteBuffer(vBuffer[0], Length(vBuffer));
end;

procedure THCView.SaveToStream(const AStream: TStream; const AQuick: Boolean = False;
  const AAreas: TSectionAreas = [saHeader, saPage, saFooter]);
var
  vByte: Byte;
  i: Integer;
begin
  _SaveFileFormatAndVersion(AStream);  // 文件格式和版本
  DoSaveStreamBefor(AStream);

  if not AQuick then
  begin
    FUndoList.Clear; // 防止删除样式后的撤销找不到原样式
    DeleteUnUsedStyle(FStyle, FSections, AAreas);  // 删除不使用的样式
  end;

  FStyle.SaveToStream(AStream);
  // 节数量
  vByte := FSections.Count;
  AStream.WriteBuffer(vByte, 1);
  // 各节数据
  for i := 0 to FSections.Count - 1 do
    FSections[i].SaveToStream(AStream, AAreas);

  DoSaveStreamAfter(AStream);
end;

procedure THCView.SaveToXml(const AFileName: string; const AEncoding: TEncoding);
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    SaveToXmlStream(vStream, AEncoding);
    vStream.SaveToFile(AFileName);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure THCView.SaveToXmlStream(const AStream: TStream;
  const AEncoding: TEncoding);
var
  vXml: IHCXMLDocument;
  vNode: IHCXMLNode;
  i: Integer;
begin
  FUndoList.Clear; // 防止删除样式后的撤销找不到原样式
  DeleteUnUsedStyle(FStyle, FSections, [saHeader, saPage, saFooter]);

  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.Encoding := GetEncodingName(AEncoding);

  vXml.DocumentElement := vXml.CreateNode('HCView');
  vXml.DocumentElement.Attributes['EXT'] := HC_EXT;
  vXml.DocumentElement.Attributes['ver'] := HC_FileVersion;
  vXml.DocumentElement.Attributes['lang'] := HC_PROGRAMLANGUAGE;

  vNode := vXml.DocumentElement.AddChild('style');
  FStyle.ToXml(vNode);  // 样式表

  vNode := vXml.DocumentElement.AddChild('sections');
  vNode.Attributes['count'] := FSections.Count;  // 节数量

  for i := 0 to FSections.Count - 1 do  // 各节数据
    FSections[i].ToXml(vNode.AddChild('sc'));

  //vXml.SaveToFile(AFileName);
  vXml.SaveToStream(AStream);
end;

procedure THCView.SaveToPDFStream(const AStream: TStream; const APageImage: Boolean = False);
  {$IFDEF SYNPDF}
  function GetPDFPaperSize(const APaperSize: Integer): TPDFPaperSize;
  begin
    case APaperSize of
      DMPAPER_A3: Result := TPDFPaperSize.psA3;
      DMPAPER_A4: Result := TPDFPaperSize.psA4;
      DMPAPER_A5: Result := TPDFPaperSize.psA5;
      //DMPAPER_B5: Result := TPDFPaperSize.psB5;
    else
      Result := TPDFPaperSize.psUserDefined;
    end;
  end;

var
  i, j, vDPI: Integer;
  vPDF: TPdfDocumentGDI;
  vPage: TPdfPage;
  vPaintInfo: TSectionPaintInfo;

  vScaleInfo: TScaleInfo;
  vBmp: TBitmap;
  //vMetaFile: TMetafile;
  //vMetaCanvas: TCanvas;
  //vDC: HDC;
begin
  vPDF := TPdfDocumentGDI.Create;
  try
    {vPDF.ScreenLogPixels := 96;
    vPDF.DefaultPaperSize := TPDFPaperSize.psA4;
    vPDF.AddPage;
    vPDF.VCLCanvas.Brush.Style := bsClear;
    vPDF.VCLCanvas.Font.Name := '宋体'; // 宋体
    vPDF.VCLCanvas.TextOut( 20, 20, '发现双引号不正确“问题”' );
    vPDF.SaveToFile('c:\Syntest.pdf');

    Exit;}


    vPDF.Info.Author := 'HCView';
    vPDF.Info.CreationDate := Now;
    vPDF.Info.Creator := 'HCView';  // jt
    vPDF.Info.Keywords := '';  // 入院记录
    vPDF.Info.ModDate := Now;
    vPDF.Info.Subject := '';  // HIT 电子病历
    vPDF.Info.Title := '';  // 张三第1次

    vPDF.UseUniscribe := True;

    vDPI := PixelsPerInchX;
    vPDF.ScreenLogPixels := vDPI;
    //vPDF.VCLCanvas.Font.Charset = ANSI_CHARSET
    //https://synopse.info/forum/viewtopic.php?id=2494
    if not APageImage then
    begin
      vPDF.EmbeddedTTF := True;
      //vPDF.EmbeddedWholeTTF := True;
      vPDF.EmbeddedTTFIgnore.Text := MSWINDOWS_DEFAULT_FONTS + #13#10'宋体';
    end;

    vPaintInfo := TSectionPaintInfo.Create;
    try
      vPaintInfo.Print := True;

      for i := 0 to FSections.Count - 1 do
      begin
        vPaintInfo.SectionIndex := i;
        vPaintInfo.Zoom := vPDF.ScreenLogPixels / vDPI;
        vPaintInfo.ScaleX := vPaintInfo.Zoom;
        vPaintInfo.ScaleY := vPaintInfo.Zoom;

        for j := 0 to FSections[i].PageCount - 1 do
        begin
          vPage := vPDF.AddPage;

          if FSections[i].PaperOrientation = TPaperOrientation.cpoLandscape then
            vPage.PageLandscape := True
          else
            vPage.PageLandscape := False;

          //vPDF.DefaultPaperSize := GetPDFPaperSize(FSections[i].PaperSize);
          //if vPDF.DefaultPaperSize = TPDFPaperSize.psUserDefined then
          begin  // 英寸单位下总共多少像素
            vPage.PageWidth := Round(FSections[i].PaperWidth / 25.4 * 72);
            vPage.PageHeight := Round(FSections[i].PaperHeight / 25.4 * 72);
          end;

          vPaintInfo.PageIndex := j;
          vPaintInfo.WindowWidth := FSections[i].PaperWidthPix;
          vPaintInfo.WindowHeight := FSections[i].PaperHeightPix;

          if not APageImage then
            FSections[i].PaintPaper(j, 0, 0, vPDF.VCLCanvas, vPaintInfo)
          else
          begin
            {vMetaFile := TMetafile.Create;
            vMetaFile.Width  := FSections[i].PaperWidthPix;
            vMetaFile.Height := FSections[i].PaperHeightPix;
            vDC := CreateCompatibleDC(0);
            vMetaCanvas := TMetaFileCanvas.Create(vMetaFile, vDC);
            FSections[i].PaintPaper(j, 0, 0, vMetaCanvas, vPaintInfo);

            vMetaCanvas.Free;
            DeleteDC(vDC);
            //vMetaFile.SaveToFile('c:\aa.emf');
            vPDF.VCLCanvas.Draw(0, 0, vMetaFile);
            vMetaFile.Free;}

            vBmp := TBitmap.Create;
            try
              vBmp.SetSize(FSections[i].PaperWidthPix, FSections[i].PaperHeightPix);
              vBmp.Canvas.Brush.Color := clWhite;
              vBmp.Canvas.FillRect(Rect(0, 0, vBmp.Width, vBmp.Height));
              //vScaleInfo := vPaintInfo.ScaleCanvas(vBmp.Canvas);
              //try
                FSections[i].PaintPaper(j, 0, 0, vBmp.Canvas, vPaintInfo);
              //finally
              //  vPaintInfo.RestoreCanvasScale(vBmp.Canvas, vScaleInfo);
              //end;

              vPDF.VCLCanvas.Draw(0, 0, vBmp);
            finally
              vBmp.Free;
            end;
          end;
        end;
      end;
    finally
      vPaintInfo.Free;
    end;

    vPDF.SaveToStream(AStream);
  finally
    vPDF.Free;
  end;
  {$ELSE}
begin
  {$ENDIF}
end;

function THCView.ZoomIn(const Value: Integer): Integer;
begin
  Result := Round(Value * FZoom);
end;

function THCView.ZoomOut(const Value: Integer): Integer;
begin
  Result := Round(Value / FZoom);
end;

function THCView.Search(const AKeyword: string; const AForward: Boolean = False;
  const AMatchCase: Boolean = False): Boolean;
var
  vTopData: THCCustomData;
  vStartDrawItemNo, vEndDrawItemNo: Integer;
  vPt: TPoint;
  vStartDrawRect, vEndDrawRect: TRect;
begin
  Result := Self.ActiveSection.Search(AKeyword, AForward, AMatchCase);
  if Result then
  begin
    vPt := GetTopLevelDrawItemViewCoord;  // 返回光标处DrawItem相对当前页显示的窗体坐标，有选中时，以选中结束位置的DrawItem格式化坐标

    vTopData := ActiveSectionTopLevelData;
    with vTopData do
    begin
      vStartDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      vEndDrawItemNo := GetDrawItemNoByOffset(SelectInfo.EndItemNo, SelectInfo.EndItemOffset);

      if vStartDrawItemNo = vEndDrawItemNo then  // 选中在同一个DrawItem
      begin
        vStartDrawRect.Left := vPt.X + ZoomIn(GetDrawItemOffsetWidth(vStartDrawItemNo,
          SelectInfo.StartItemOffset - DrawItems[vStartDrawItemNo].CharOffs + 1));
        vStartDrawRect.Top := vPt.Y;
        vStartDrawRect.Right := vPt.X + ZoomIn(GetDrawItemOffsetWidth(vEndDrawItemNo,
          SelectInfo.EndItemOffset - DrawItems[vEndDrawItemNo].CharOffs + 1));
        vStartDrawRect.Bottom := vPt.Y + ZoomIn(DrawItems[vEndDrawItemNo].Rect.Height);

        vEndDrawRect := vStartDrawRect;
      end
      else  // 选中不在同一个DrawItem
      begin
        vStartDrawRect.Left := vPt.X + ZoomIn(DrawItems[vStartDrawItemNo].Rect.Left - DrawItems[vEndDrawItemNo].Rect.Left
          + GetDrawItemOffsetWidth(vStartDrawItemNo, SelectInfo.StartItemOffset - DrawItems[vStartDrawItemNo].CharOffs + 1));
        vStartDrawRect.Top := vPt.Y + ZoomIn(DrawItems[vStartDrawItemNo].Rect.Top - DrawItems[vEndDrawItemNo].Rect.Top);
        vStartDrawRect.Right := vPt.X + ZoomIn(DrawItems[vStartDrawItemNo].Rect.Left - DrawItems[vEndDrawItemNo].Rect.Left
          + DrawItems[vStartDrawItemNo].Rect.Width);
        vStartDrawRect.Bottom := vStartDrawRect.Top + ZoomIn(DrawItems[vStartDrawItemNo].Rect.Height);

        vEndDrawRect.Left := vPt.X;
        vEndDrawRect.Top := vPt.Y;
        vEndDrawRect.Right := vPt.X + ZoomIn(GetDrawItemOffsetWidth(vEndDrawItemNo,
          SelectInfo.EndItemOffset - DrawItems[vEndDrawItemNo].CharOffs + 1));
        vEndDrawRect.Bottom := vPt.Y + ZoomIn(DrawItems[vEndDrawItemNo].Rect.Height);
      end;
    end;

    if vStartDrawRect.Top < 0 then
      Self.FVScrollBar.Position := Self.FVScrollBar.Position + vStartDrawRect.Top
    else
    if vStartDrawRect.Bottom > FViewHeight then
      Self.FVScrollBar.Position := Self.FVScrollBar.Position + vStartDrawRect.Bottom - FViewHeight;

    if vStartDrawRect.Left < 0 then
      Self.FHScrollBar.Position := Self.FHScrollBar.Position + vStartDrawRect.Left
    else
    if vStartDrawRect.Right > FViewWidth then
      Self.FHScrollBar.Position := Self.FHScrollBar.Position + vStartDrawRect.Right - FViewWidth;
  end;
end;

procedure THCView.SelectAll;
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].SelectAll;

  FStyle.UpdateInfoRePaint;
  CheckUpdateInfo;
end;

procedure THCView.SetActiveItemText(const AText: string);
begin
  ActiveSection.SetActiveItemText(AText);
end;

procedure THCView.SetActiveSectionIndex(const Value: Integer);
begin
  if FActiveSectionIndex <> Value then
  begin
    if FActiveSectionIndex >= 0 then
      FSections[FActiveSectionIndex].DisActive;

    FActiveSectionIndex := Value;
    DoViewResize;
  end;
end;

procedure THCView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  FVScrollBar.Left := Width - FVScrollBar.Width;
  FVScrollBar.Height := Height - FHScrollBar.Height;
  //
  FHScrollBar.Top := Height - FHScrollBar.Height;
  FHScrollBar.Width := Width - FVScrollBar.Width;
end;

procedure THCView.SetIsChanged(const Value: Boolean);
begin
  if FIsChanged <> Value then
  begin
    FIsChanged := Value;
    if Assigned(FOnChangeSwitch) then
      FOnChangeSwitch(Self);
  end;
end;

procedure THCView.SetPageNoFormat(const Value: string);
begin
  if FPageNoFormat <> Value then
  begin
    FPageNoFormat := Value;
    UpdateView;
  end;
end;

procedure THCView.SetPagePadding(const Value: Byte);
var
  i: Integer;
begin
  if FPagePadding <> Value then
  begin
    FPagePadding := Value;
    for i := 0 to FSections.Count - 1 do
      FSections[i].PagePadding := FPagePadding;

    FStyle.UpdateInfoRePaint;
    FStyle.UpdateInfoReCaret(False);
    DoMapChanged;
    DoViewResize;
  end;
end;

procedure THCView.SetPrintBySectionInfo(const ASectionIndex: Integer);
var
  vDevice: Array[0..(cchDeviceName - 1)] of Char;
  vDriver: Array[0..(MAX_PATH - 1)] of Char;
  vPort: Array[0..32] of Char;
  vHDMode: THandle;
  vPDMode: PDevMode;
begin
  HCPrinter.GetPrinter(vDevice, vDriver, vPort, vHDMode);
  if vHDMode <> 0 then
  begin
    // 获取指向DeviceMode的指针
    vPDMode := GlobalLock(vHDMode);
    try
      if vPDMode <> nil then
      begin
        try
          vPDMode^.dmPaperSize := FSections[ASectionIndex].PaperSize;
          if vPDMode^.dmPaperSize = DMPAPER_HC_16K then  //  16K等非国际标准纸张认定为自定义纸张
            vPDMode^.dmPaperSize := DMPAPER_USER;

          vPDMode^.dmFields := vPDMode^.dmFields or DM_PAPERSIZE or DM_PAPERLENGTH or DM_PAPERWIDTH;

          if FSections[ASectionIndex].PaperOrientation = TPaperOrientation.cpoPortrait then
          begin
            vPDMode^.dmOrientation := DMORIENT_PORTRAIT;
            vPDMode^.dmPaperLength := Round(FSections[ASectionIndex].PaperHeight * 10); //纸长你可用变量获得纸张的长、宽。
            vPDMode^.dmPaperWidth := Round(FSections[ASectionIndex].PaperWidth * 10);   //纸宽
          end
          else
          begin
            vPDMode^.dmOrientation := DMORIENT_LANDSCAPE;
            vPDMode^.dmPaperLength := Round(FSections[ASectionIndex].PaperWidth * 10); //纸长你可用变量获得纸张的长、宽。
            vPDMode^.dmPaperWidth := Round(FSections[ASectionIndex].PaperHeight * 10);   //纸宽
          end;
        finally
          ResetDC(HCPrinter.Handle, vPDMode^);
        end;
      end;
    finally
      GlobalUnlock(vHDMode);
    end;
    ResetDC(HCPrinter.Handle, vPDMode^);
    //HCPrinter.SetPrinter(vDevice, vDriver, vPort, vHDMode);
  end;
end;

procedure THCView.SetReadOnly(const Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].ReadOnly := Value;

  UpdateView;
end;

procedure THCView.SetShowLineActiveMark(const Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].Page.ShowLineActiveMark := Value;

  UpdateView;
end;

procedure THCView.SetShowLineNo(const Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].Page.ShowLineNo := Value;

  UpdateView;
end;

procedure THCView.SetShowUnderLine(const Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].Page.ShowUnderLine := Value;

  UpdateView;
end;

procedure THCView.SetSymmetryMargin(const Value: Boolean);
begin
  if ActiveSection.SymmetryMargin <> Value then
  begin
    ActiveSection.SymmetryMargin := Value;
    FStyle.UpdateInfoRePaint;
    FStyle.UpdateInfoReCaret(False);
    DoMapChanged;
    DoViewResize;
  end;
end;

procedure THCView.SetViewModel(const Value: THCViewModel);
var
  i: Integer;
begin
  if FViewModel <> Value then
  begin
    FViewModel := Value;

    for i := 0 to FSections.Count - 1 do
      FSections[i].ViewModel := Value;

    if Value = hvmFilm then
      PagePadding := 20
    else
      PagePadding := 0;
  end;
end;

procedure THCView.SetZoom(const Value: Single);
var
  vValue: Single;
begin
  if Value < 0.25 then
    vValue := 0.25
  else
  if Value > 5 then
    vValue := 5
  else
    vValue := Value;

  if FZoom <> vValue then
  begin
    Self.SetFocus;
    FZoom := vValue;

    FStyle.UpdateInfoRePaint;
    FStyle.UpdateInfoReCaret(False);
    if Assigned(FOnZoomChange) then
      FOnZoomChange(Self);

    DoMapChanged;
    DoViewResize;
  end;
end;

function THCView.TableApplyContentAlign(const AAlign: THCContentAlign): Boolean;
begin
  Result := ActiveSection.TableApplyContentAlign(AAlign);
end;

function THCView.AppendStream(const AStream: TStream): Boolean;
begin
  Result := False;
  ActiveSection.ActiveData.SelectLastItemAfterWithCaret;
  InsertBreak;
  ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
  Result := InsertStream(AStream);
end;

procedure THCView.ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
begin
  ActiveSection.ApplyParaAlignHorz(AAlign);
end;

procedure THCView.ApplyParaAlignVert(const AAlign: TParaAlignVert);
begin
  ActiveSection.ApplyParaAlignVert(AAlign);
end;

procedure THCView.ApplyParaBackColor(const AColor: TColor);
begin
  ActiveSection.ApplyParaBackColor(AColor);
end;

procedure THCView.ApplyParaBreakRough(const ARough: Boolean);
begin
  ActiveSection.ApplyParaBreakRough(ARough);
end;

procedure THCView.ApplyParaFirstIndent(const AIndent: Single);
begin
  ActiveSection.ApplyParaFirstIndent(AIndent);
end;

procedure THCView.ApplyParaLeftIndent(const AIndent: Single);
begin
  ActiveSection.ApplyParaLeftIndent(AIndent);
end;

procedure THCView.ApplyParaLeftIndent(const Add: Boolean = True);
begin
  if Add then
    ActiveSection.ApplyParaLeftIndent(FStyle.ParaStyles[CurParaNo].LeftIndent + PixXToMillimeter(TabCharWidth))
  else
    ActiveSection.ApplyParaLeftIndent(FStyle.ParaStyles[CurParaNo].LeftIndent - PixXToMillimeter(TabCharWidth));
end;

procedure THCView.ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode; const ASpace: Single = 1);
begin
  ActiveSection.ApplyParaLineSpace(ASpaceMode, ASpace);
end;

procedure THCView.ApplyParaRightIndent(const AIndent: Single);
begin
  ActiveSection.ApplyParaRightIndent(AIndent);
end;

procedure THCView.Undo;
begin
  FStyle.States.Include(THCState.hosUndoing);
  try
    if FUndoList.Enable then  // 撤销过程不要产生新的Undo
    begin
      try
        FUndoList.Enable := False;

        BeginUpdate;
        try
          FUndoList.Undo;
        finally
          EndUpdate;
        end;
      finally
        FUndoList.Enable := True;
      end;
    end;
  finally
    FStyle.States.Exclude(THCState.hosUndoing);
  end;
end;

procedure THCView.UndoGroupBegin;
var
  vData: THCRichData;
begin
  if FUndoList.Enable then
  begin
    vData := ActiveSection.ActiveData;
    FUndoList.UndoGroupBegin(vData.SelectInfo.StartItemNo, vData.SelectInfo.StartItemOffset);
  end;
end;

procedure THCView.UndoGroupEnd;
var
  vData: THCRichData;
begin
  if FUndoList.Enable then
  begin
    vData := ActiveSection.ActiveData;
    FUndoList.UndoGroupEnd(vData.SelectInfo.StartItemNo, vData.SelectInfo.StartItemOffset);
  end;
end;

procedure THCView.UpdateView(const ARect: TRect);

  {$REGION ' CalcDisplaySectionAndPage 获取可显示的起始和结束节、页序号 '}
  procedure CalcDisplaySectionAndPage;
  var
    i, j, vPos, vY: Integer;
    vFirstPage, vLastPage: Integer;
  begin
    if FDisplayFirstSection >= 0 then
    begin
      FSections[FDisplayFirstSection].DisplayFirstPageIndex := -1;
      FSections[FDisplayFirstSection].DisplayLastPageIndex := -1;
      FDisplayFirstSection := -1;
    end;

    if FDisplayLastSection >= 0 then
    begin
      FSections[FDisplayLastSection].DisplayFirstPageIndex := -1;
      FSections[FDisplayLastSection].DisplayLastPageIndex := -1;
      FDisplayLastSection := -1;
    end;

    vFirstPage := -1;
    vLastPage := -1;
    vPos := 0;

    for i := 0 to FSections.Count - 1 do
    begin
      for j := 0 to FSections[i].PageCount - 1 do
      begin
        if FSections[i].ViewModel = hvmFilm then
          vPos := vPos + ZoomIn(FPagePadding + FSections[i].PaperHeightPix)
        else
          vPos := vPos + ZoomIn(FSections[i].GetPageHeight);

        if vPos > FVScrollBar.Position then
        begin
          vFirstPage := j;
          Break;
        end;
      end;

      if vFirstPage >= 0 then
      begin
        FDisplayFirstSection := i;
        FSections[FDisplayFirstSection].DisplayFirstPageIndex := vFirstPage;
        Break;
      end;
    end;

    if FDisplayFirstSection >= 0 then
    begin
      vY := FVScrollBar.Position + FViewHeight;
      for i := FDisplayFirstSection to FSections.Count - 1 do
      begin
        for j := vFirstPage to FSections[i].PageCount - 1 do
        begin
          if vPos < vY then
          begin
            if FSections[i].ViewModel = hvmFilm then
              vPos := vPos + ZoomIn(FPagePadding + FSections[i].PaperHeightPix)
            else
              vPos := vPos + ZoomIn(FPagePadding + FSections[i].GetPageHeight);
          end
          else
          begin
            vLastPage := j;
            Break;
          end;
        end;

        if vLastPage >= 0 then
        begin
          FDisplayLastSection := i;
          FSections[FDisplayLastSection].DisplayLastPageIndex := vLastPage;
          Break;
        end;

        vFirstPage := 0;  // 当前节没找到，下一节从0开始
      end;

      if FDisplayLastSection < 0 then  // 没有找到结束页，赋值为最后一节最后一页
      begin
        FDisplayLastSection := FSections.Count - 1;
        FSections[FDisplayLastSection].DisplayLastPageIndex := FSections[FDisplayLastSection].PageCount - 1;
      end;
    end;

    if (FDisplayFirstSection < 0) or (FDisplayLastSection < 0) then
      raise Exception.Create('异常：获取当前显示起始页和结束页失败！')
    else
    begin
      if FDisplayFirstSection <> FDisplayLastSection then  // 起始和结束不在同一节
      begin
        FSections[FDisplayFirstSection].DisplayLastPageIndex := FSections[FDisplayFirstSection].PageCount - 1;
        FSections[FDisplayLastSection].DisplayFirstPageIndex := 0;
      end;
    end;
  end;
  {$ENDREGION}

var
  i, vOffsetY: Integer;
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
  vRgn: HRGN;
begin
  if FStyle.States.Contain(THCState.hosDestroying) then Exit;

  if (FUpdateCount = 0) and HandleAllocated then
  begin
    FDataBmp.Canvas.Lock;
    try
      // 创建一个新的剪切区域，该区域是当前剪切区域和一个特定矩形的交集
      //IntersectClipRect(FDataBmp.Canvas.Handle, 0, 0, FViewWidth , FViewHeight);
      vRgn := CreateRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      try
        SelectClipRgn(FDataBmp.Canvas.Handle, vRgn);
      finally
        DeleteObject(vRgn);
      end;

      // 控件背景
      if FViewModel = hvmFilm then
        FDataBmp.Canvas.Brush.Color := Self.Color
      else
      if FViewModel = hvmPage then
        FDataBmp.Canvas.Brush.Color := Self.Color
      else
        FDataBmp.Canvas.Brush.Color := FStyle.BackgroundColor;

      FDataBmp.Canvas.FillRect(Rect(0, 0, FDataBmp.Width, FDataBmp.Height));

      // 因基于此计算当前页面数据起始结束，所以不能用ARect代替
      CalcDisplaySectionAndPage;  // 计算当前范围内可显示的起始节、页和结束节、页

      vPaintInfo := TSectionPaintInfo.Create;
      try
        vPaintInfo.ScaleX := FZoom;
        vPaintInfo.ScaleY := FZoom;
        vPaintInfo.Zoom := FZoom;
        vPaintInfo.ViewModel := FViewModel;
        vPaintInfo.WindowWidth := FViewWidth;
        vPaintInfo.WindowHeight := FViewHeight;
        vPaintInfo.DPI := HCUnitConversion.PixelsPerInchX;

        vScaleInfo := vPaintInfo.ScaleCanvas(FDataBmp.Canvas);
        try
          DoPaintViewBefor(FDataBmp.Canvas, vPaintInfo);  // 本次窗口重绘开始

          if FAnnotatePre.DrawCount > 0 then  // 在整体绘制前清除一次，整个绘制中记录各页的批注，便于鼠标移动时判断光标处
            FAnnotatePre.ClearDrawAnnotate;     // 如果每页绘制前清除，则鼠标移动时仅能判断是否在最后绘制页的批注范围内，前面页中的都没有了

          for i := FDisplayFirstSection to FDisplayLastSection do
          begin
            vPaintInfo.SectionIndex := i;

            vOffsetY := ZoomOut(FVScrollBar.Position) - GetSectionTopFilm(i);  // 转为原始Y向偏移
            FSections[i].PaintDisplayPaper(GetSectionDrawLeft(i) - ZoomOut(FHScrollBar.Position),  // 原始X向偏移
              vOffsetY, FDataBmp.Canvas, vPaintInfo);
          end;

          for i := 0 to vPaintInfo.TopItems.Count - 1 do  // 绘制顶层Item
            vPaintInfo.TopItems[i].PaintTop(FDataBmp.Canvas);

          DoPaintViewAfter(FDataBmp.Canvas, vPaintInfo);  // 本次窗口重绘结束

          if (not vPaintInfo.Print) and Assigned(FCaret) and FCaret.DisFocus then  // 伪光标
          begin
            FDataBmp.Canvas.Pen.Color := clGray;
            FDataBmp.Canvas.Pen.Style := psSolid;
            FDataBmp.Canvas.Pen.Width := FCaret.Width;
            FDataBmp.Canvas.MoveTo(FCaret.X, FCaret.Y);
            FDataBmp.Canvas.LineTo(FCaret.X, FCaret.Y + FCaret.Height);
          end;
        finally
          vPaintInfo.RestoreCanvasScale(FDataBmp.Canvas, vScaleInfo);
        end;
      finally
        vPaintInfo.Free;
      end;
    finally
      FDataBmp.Canvas.Unlock;
    end;

    BitBlt(Canvas.Handle, 0, 0, FViewWidth, FViewHeight, FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
    //BitBlt(Canvas.Handle, ARect.Left, ARect.Top, ARect.Width, ARect.Height,
    //  FDataBmp.Canvas.Handle, ARect.Left, ARect.Top, SRCCOPY);

    // 只更新变动区域，防止闪烁，解决BitBlt光标滞留切换位置显示不及时的问题
    InvalidateRect(Self.Handle, ARect, False);
  end;
end;

procedure THCView.UpdateView;
begin
  UpdateView(GetViewRect);
end;

procedure THCView.UpdateImeComposition(const ALParam: Integer);
var
  vhIMC: HIMC;
  vSize: Integer;
  vBuffer: TBytes;
  vS: string;
begin
  if (ALParam and GCS_RESULTSTR) <> 0 then  // 通知检索或更新上屏字符串
  begin
    // 处理上屏文本一次性插入，否则会不停的触发KeyPress事件
    vhIMC := ImmGetContext(Handle);
    if vhIMC <> 0 then
    begin
      try
        vSize := ImmGetCompositionString(vhIMC, GCS_RESULTSTR, nil, 0);  // 获取IME结果字符串的大小
        if vSize > 0 then  	// 如果IME结果字符串不为空，且没有错误
        begin
          // 取出字符串
          SetLength(vBuffer, vSize);
          ImmGetCompositionString(vhIMC, GCS_RESULTSTR, vBuffer, vSize);
          //SetLength(vBuffer, vSize);  // vSize - 2
          vS := WideStringOf(vBuffer);
          if vS <> '' then
            InsertText(vS);
        end;
      finally
        ImmReleaseContext(Handle, vhIMC);
      end;
    end;
  end;
end;

procedure THCView.UpdateImePosition;
var
  vhIMC: HIMC;
  vCF: TCompositionForm;
  //vLogFont: TLogFont;
begin
  vhIMC := ImmGetContext(Handle);
  try
    // 告诉输入法当前光标处字体信息
    {ImmGetCompositionFont(vhIMC, @vLogFont);
    vLogFont.lfHeight := 22;
    ImmSetCompositionFont(vhIMC, @vLogFont);}

    // 告诉输入法当前光标位置信息
    vCF.ptCurrentPos := Point(FCaret.X, FCaret.Y + FCaret.Height + 4);  // 输入法弹出窗体位置
    vCF.dwStyle := CFS_FORCE_POSITION;  // 强制按我的位置  CFS_RECT
    vCF.rcArea := ClientRect;
    ImmSetCompositionWindow(vhIMC, @vCF);
  finally
    ImmReleaseContext(Handle, vhIMC);
  end;
  {if ActiveSection.SelectInfo.StartItemOffset > 1 then  // 告诉输入法根据当前位置词条更新备选
  begin
    if GetCurItem.StyleNo < 0 then Exit;

    vS := GetCurItem.GetTextPart(ActiveSection.SelectInfo.StartItemOffset - 1, 2);  // 返回光标前2个字符
    if vS <> '' then
    begin
      if vS = '后期' then
        vCandiID := 4743
      else
      if vS = '周身' then
        vCandiID := 10019
      else
      if vS = '失去' then
        vCandiID := 10657
      else
        vCandiID := -1;
      if vCandiID > 0 then
      begin
        vIMEWnd := ImmGetDefaultIMEWnd(Handle);
        //SendMessage(vIMEWnd, WM_IME_CONTROL, IMC_SETCOMPOSITIONWINDOW, Integer(@vPt));
        SendMessage(vIMEWnd, WM_IME_NOTIFY, IMN_UPDATECURSTRING, vCandiID);
      end;
    end;
  end;}
end;

procedure THCView.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure THCView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;

procedure THCView.WMImeComposition(var Message: TMessage);
begin
  UpdateImeComposition(Message.LParam);
  inherited;
end;

procedure THCView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if Message.FocusedWnd <> Self.Handle then
    DoKillFocus;
end;

procedure THCView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  //ActiveSection.DblClick(Message.XPos, Message.YPos);  // 双击也放到MouseDown中了
end;

procedure THCView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  DoSetFocus;
  // 光标在行最后，通过工具栏修改光标处字号后会重新设置焦点到HCView，如果不停止重取样式，
  // 则会以当前光标前文本样式做为当前样式了，如果是焦点失去后鼠标重新点击获取焦点
  // 会先触发这里，再触发MouseDown，在MouseDown里会重新取当前样式不受影响
  FStyle.UpdateInfoReCaret(False);
  FStyle.UpdateInfoRePaint;
  //FStyle.UpdateInfoReScroll;  // 失去焦点前光标不在显示页，获取焦点后不要乱跳
  CheckUpdateInfo;
end;

procedure THCView.WndProc(var Message: TMessage);
var
  vPt: TPoint;
{  DC: HDC;
  PS: TPaintStruct;}
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      begin
        if not (csDesigning in ComponentState) and not Focused then
        begin
          Windows.SetFocus(Handle);
          if not Focused then
            Exit;
        end;
      end;

    WM_TIMER:
      begin
        if Message.WParam = 2 then  // 划选时自动滚动
        begin
          GetCursorPos(vPt);
          vPt := ScreenToClient(vPt);
          if vPt.Y > Self.Height - FHScrollBar.Height then
            FVScrollBar.Position := FVScrollBar.Position + 60
          else
          if vPt.Y < 0 then
            FVScrollBar.Position := FVScrollBar.Position - 60;

          if vPt.X > Self.Width - FVScrollBar.Width then
            FHScrollBar.Position := FHScrollBar.Position + 60
          else
          if vPt.X < 0 then
            FHScrollBar.Position := FHScrollBar.Position - 60;
        end;
      end;

    WM_IME_CHAR:  // 通过 UpdateImeComposition 取了输入法，防止回车字符重复上屏
      begin
        Message.Result := 1;
        Exit;
      end;
  end;

  inherited WndProc(Message);
end;

{ THCAnnotate }

function THCAnnotatePre.ActiveAnnotate: THCDataAnnotate;
begin
  if FActiveDrawAnnotateIndex < 0 then
    Result := nil
  else
    Result := FDrawAnnotates[FActiveDrawAnnotateIndex].DataAnnotate;
end;

procedure THCAnnotatePre.AddDrawAnnotate(const ADrawAnnotate: THCDrawAnnotate);
begin
  FDrawAnnotates.Add(ADrawAnnotate);
end;

procedure THCAnnotatePre.ClearDrawAnnotate;
begin
  FDrawAnnotates.Clear;
end;

constructor THCAnnotatePre.Create;
begin
  inherited Create;
  FDrawAnnotates := TObjectList<THCDrawAnnotate>.Create;
  FCount := 0;
  FVisible := False;
  FMouseIn := False;
  FActiveDrawAnnotateIndex := -1;
end;

procedure THCAnnotatePre.DeleteDataAnnotateByDraw(const AIndex: Integer);
begin
  if AIndex >= 0 then
  begin
    (FDrawAnnotates[AIndex].Data as THCAnnotateData).DataAnnotates.DeleteByID(FDrawAnnotates[AIndex].DataAnnotate.ID);
    DoUpdateView;
  end;
end;

destructor THCAnnotatePre.Destroy;
begin
  FreeAndNil(FDrawAnnotates);
  inherited Destroy;
end;

procedure THCAnnotatePre.DoUpdateView;
begin
  if Assigned(FOnUpdateView) then
    FOnUpdateView(Self);
end;

function THCAnnotatePre.GetDrawAnnotateAt(const X, Y: Integer): Integer;
begin
  Result := GetDrawAnnotateAt(Point(X, Y));
end;

function THCAnnotatePre.GetDrawAnnotateAt(const APoint: TPoint): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FDrawAnnotates.Count - 1 do
  begin
    if PtInRect(FDrawAnnotates[i].Rect, APoint) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function THCAnnotatePre.GetDrawCount: Integer;
begin
  Result := FDrawAnnotates.Count;
end;

procedure THCAnnotatePre.InsertDataAnnotate(const ADataAnnotate: THCDataAnnotate);
begin
  Inc(FCount);
  FVisible := True;
end;

procedure THCAnnotatePre.MouseDown(const X, Y: Integer);
begin
  FActiveDrawAnnotateIndex := GetDrawAnnotateAt(X, Y);
end;

procedure THCAnnotatePre.MouseMove(const X, Y: Integer);
var
  //vIndex: Integer;
  vPt: TPoint;
begin
  vPt := Point(X, Y);
  MouseIn := PtInRect(FDrawRect, vPt);
  //vIndex := GetDrawAnnotateAt(vPt);
end;

procedure THCAnnotatePre.PaintDrawAnnotate(const Sender: TObject; const APageRect: TRect;
  const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  i, vVOffset, vTop, vBottom, vRePlace, vSpace, vFirst, vLast: Integer;
  vHeaderAreaHeight: Integer absolute vSpace;
  vTextRect: TRect;
  vSection: THCSection;
  vData: THCAnnotateData;
  vDrawAnnotate: THCDrawAnnotate;
  vText: string;
begin
  FDrawRect := Rect(APageRect.Right, APageRect.Top,
    APageRect.Right + AnnotationWidth, APageRect.Bottom);

  if not APaintInfo.Print then
  begin
    if FMouseIn then
      ACanvas.Brush.Color := $00D5D1D0
    else
      ACanvas.Brush.Color := $00F4F4F4;  // 填充批注区域

    ACanvas.FillRect(FDrawRect);
  end;

  if FDrawAnnotates.Count > 0 then  // 有批注
  begin
    vFirst := -1;
    vLast := -1;
    vSection := Sender as THCSection;
    vHeaderAreaHeight := vSection.GetHeaderAreaHeight;  // 页眉高度
    //正文中的批注
    vVOffset := APageRect.Top + vHeaderAreaHeight - APaintInfo.PageDataFmtTop;
    vTop := APaintInfo.PageDataFmtTop + vVOffset;
    vBottom := vTop + vSection.PaperHeightPix - vHeaderAreaHeight - vSection.PaperMarginBottomPix;

    for i := 0 to FDrawAnnotates.Count - 1 do  // 找本页的起始和结束批注
    begin
      vDrawAnnotate := FDrawAnnotates[i];
      if vDrawAnnotate.DrawRect.Top > vBottom then
        Break
      else
      if vDrawAnnotate.DrawRect.Bottom > vTop then
      begin
        vLast := i;
        if vFirst < 0 then
          vFirst := i;
      end;
    end;

    if vFirst >= 0 then  // 本页有批注
    begin
      ACanvas.Font.Size := 8;
      ACanvas.Font.Name := '宋体';
      ACanvas.Font.Color := clBlack;

      // 计算本页各批注显示位置
      vTop := FDrawAnnotates[vFirst].DrawRect.Top;
      for i := vFirst to vLast do
      begin
        vDrawAnnotate := FDrawAnnotates[i];
        if vDrawAnnotate.DrawRect.Top > vTop then
          vTop := vDrawAnnotate.DrawRect.Top;

        if vDrawAnnotate is THCDrawAnnotateDynamic then
          vText := (vDrawAnnotate as THCDrawAnnotateDynamic).Title + ':' + (vDrawAnnotate as THCDrawAnnotateDynamic).Text
        else
          vText := vDrawAnnotate.DataAnnotate.Title + ':' + vDrawAnnotate.DataAnnotate.Text;

        vDrawAnnotate.Rect := Rect(0, 0, AnnotationWidth - 30, 0);
        Windows.DrawTextEx(ACanvas.Handle, PChar(vText), -1, vDrawAnnotate.Rect,
          DT_TOP or DT_LEFT or DT_WORDBREAK or DT_CALCRECT, nil);  // 计算区域
        if vDrawAnnotate.Rect.Right < AnnotationWidth - 30 then
          vDrawAnnotate.Rect.Right := AnnotationWidth - 30;

        vDrawAnnotate.Rect.Offset(APageRect.Right + 20, vTop + 5);
        vDrawAnnotate.Rect.Inflate(5, 5);

        vTop := vDrawAnnotate.Rect.Bottom + 5;
      end;

      if FDrawAnnotates[vLast].Rect.Bottom > APageRect.Bottom then  // 超过页底部了，重新排列本页各批注显示位置
      begin
        vVOffset := FDrawAnnotates[vLast].Rect.Bottom - APageRect.Bottom + 5;  // 需要上移这么大的空间可放下

        vSpace := 0;  // 各批注之间除固定间距外的空隙
        vRePlace := -1;  // 从哪一个开始调整
        vTop := FDrawAnnotates[vLast].Rect.Top;
        for i := vLast - 1 downto vFirst do  // 紧凑排列，去掉中间的空隙
        begin
          vSpace := vTop - FDrawAnnotates[i].Rect.Bottom - 5;
          vVOffset := vVOffset - vSpace;  // 消减后的剩余
          if vVOffset <= 0 then  // 空隙够了
          begin
            vRePlace := i + 1;
            if vVOffset < 0 then
              vSpace := vSpace + vVOffset;  // vRePlace处实际需要偏移的量

            Break;
          end;

          vTop := FDrawAnnotates[i].Rect.Top;
        end;

        if vRePlace < 0 then  // 当前页批注全部紧凑后仍放不下，看看页面顶部到第一个批注之间的空隙
        begin
          vRePlace := vFirst;
          vSpace := FDrawAnnotates[vFirst].Rect.Top - APageRect.Top - 5;
          if vSpace > vVOffset then  // 间距超出需要的
            vSpace := vVOffset;  // 只调整到需要的位置
        end;

        FDrawAnnotates[vRePlace].Rect.Offset(0, -vSpace);
        vTop := FDrawAnnotates[vRePlace].Rect.Bottom + 5;
        for i := vRePlace + 1 to vLast do
        begin
          vVOffset := vTop - FDrawAnnotates[i].Rect.Top;
          FDrawAnnotates[i].Rect.Offset(0, vVOffset);
          vTop := FDrawAnnotates[i].Rect.Bottom + 5;
        end;
      end;

      ACanvas.Pen.Color := clRed;
      for i := vFirst to vLast do  // 绘制批注
      begin
        vDrawAnnotate := FDrawAnnotates[i];
        if vDrawAnnotate is THCDrawAnnotateDynamic then
        begin
          vText := (vDrawAnnotate as THCDrawAnnotateDynamic).Title + ':' + (vDrawAnnotate as THCDrawAnnotateDynamic).Text;
          ACanvas.Pen.Style := TPenStyle.psDot;
          ACanvas.Pen.Width := 1;
          ACanvas.Brush.Color := AnnotateBKColor;
        end
        else
        begin
          vText := vDrawAnnotate.DataAnnotate.Title + ':' + vDrawAnnotate.DataAnnotate.Text;
          vData := vDrawAnnotate.Data as THCAnnotateData;

          if vDrawAnnotate.DataAnnotate = vData.HotAnnotate then
          begin
            ACanvas.Pen.Style := TPenStyle.psSolid;
            ACanvas.Pen.Width := 1;
            ACanvas.Brush.Color := AnnotateBKActiveColor;
          end
          else
          if vDrawAnnotate.DataAnnotate = vData.ActiveAnnotate then
          begin
            ACanvas.Pen.Style := TPenStyle.psSolid;
            ACanvas.Pen.Width := 2;
            ACanvas.Brush.Color := AnnotateBKActiveColor;
          end
          else
          begin
            ACanvas.Pen.Style := TPenStyle.psDot;
            ACanvas.Pen.Width := 1;
            ACanvas.Brush.Color := AnnotateBKColor;
          end;
        end;

        if APaintInfo.Print then
          ACanvas.Brush.Style := bsClear;

        ACanvas.RoundRect(vDrawAnnotate.Rect, 5, 5);  // 填充批注区域
        vTextRect := vDrawAnnotate.Rect;
        vTextRect.Inflate(-5, -5);
        DrawTextEx(ACanvas.Handle, PChar(IntToStr(i) + vText), -1, vTextRect,
          DT_VCENTER or DT_LEFT or DT_WORDBREAK, nil);

        // 绘制指向线
        ACanvas.Brush.Style := bsClear;
        ACanvas.MoveTo(vDrawAnnotate.DrawRect.Right, vDrawAnnotate.DrawRect.Bottom);
        ACanvas.LineTo(APageRect.Right, vDrawAnnotate.DrawRect.Bottom);
        ACanvas.LineTo(vDrawAnnotate.Rect.Left, vTextRect.Top);
      end;
    end;
  end;
end;

procedure THCAnnotatePre.RemoveDataAnnotate(const ADataAnnotate: THCDataAnnotate);
begin
  Dec(FCount);
  if FCount = 0 then
    FVisible := False;
end;

procedure THCAnnotatePre.SetMouseIn(const Value: Boolean);
begin
  if FMouseIn <> Value then
  begin
    FMouseIn := Value;
    DoUpdateView;
  end;
end;

end.
