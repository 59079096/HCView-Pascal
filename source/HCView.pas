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

uses
  Windows, Classes, Controls, Graphics, Messages, HCStyle, HCCustomData, SynPdf,
  Generics.Collections, SysUtils, HCCommon, HCViewData, HCRichData, HCDrawItem,
  HCSection, HCScrollBar, HCRichScrollBar, HCParaStyle, HCTextStyle, HCRectItem,
  HCTextItem, HCItem, HCCustomFloatItem, HCUndo, HCAnnotateData, HCSectionData;

type
  THCPageScrollModel = (psmVertical, psmHorizontal);

  TLoadSectionProc = reference to procedure(const AFileVersion: Word);

  THCDrawAnnotate = class(THCDrawItemAnnotate)  // Data批注信息
  public
    //Section: TObject;
    Data: THCCustomData;
    //Item: THCCustomItem;
    Rect: TRect;
  end;

  THCAnnotatePre = class(TObject)  // 管理显示出来的批注
  private
    FDrawRect: TRect;
    FCount: Integer;
    FMouseIn: Boolean;
    FDrawAnnotates: TObjectList<THCDrawAnnotate>;
    FActiveDrawAnnotateIndex: Integer;
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
    property Count: Integer read FCount;
    property DrawRect: TRect read FDrawRect;
    property ActiveDrawAnnotateIndex: Integer read FActiveDrawAnnotateIndex;
    property OnUpdateView: TNotifyEvent read FOnUpdateView write FOnUpdateView;
  end;

  TPaintEvent = procedure(const ACanvas: TCanvas) of object;

  THCView = class(TCustomControl)
  private
    { Private declarations }
    FFileName, FPageNoFormat: string;
    FStyle: THCStyle;
    FSections: TObjectList<THCSection>;
    FUndoList: THCUndoList;
    FHScrollBar: THCScrollBar;
    FVScrollBar: THCRichScrollBar;
    FDataBmp: TBitmap;  // 数据显示位图
    FActiveSectionIndex,
    FViewWidth, FViewHeight,
    FDisplayFirstSection, FDisplayLastSection: Integer;
    FUpdateCount: Cardinal;
    FZoom: Single;
    FAutoZoom,  // 自动缩放
    FIsChanged: Boolean;  // 是否发生了改变

    FAnnotatePre: THCAnnotatePre;  // 批注管理

    FViewModel: THCViewModel;  // 界面显示模式：页面、Web
    FPageScrollModel: THCPageScrollModel;  // 页面滚动显示模式：纵向、横向
    FCaret: THCCaret;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnCaretChange, FOnVerScroll, FOnHorScroll, FOnSectionCreateItem,
    FOnSectionReadOnlySwitch, FOnSectionCurParaNoChange, FOnSectionActivePageChange
      : TNotifyEvent;
    FOnSectionCreateStyleItem: TStyleItemEvent;
    FOnSectionCanEdit: TOnCanEditEvent;
    FOnSectionInsertItem, FOnSectionRemoveItem: TSectionDataItemNotifyEvent;
    FOnSectionDrawItemPaintAfter, FOnSectionDrawItemPaintBefor: TSectionDrawItemPaintEvent;

    FOnSectionPaintHeader, FOnSectionPaintFooter, FOnSectionPaintPage,
      FOnSectionPaintWholePageBefor, FOnSectionPaintWholePageAfter: TSectionPagePaintEvent;
    FOnPaintViewBefor, FOnPaintViewAfter: TPaintEvent;

    FOnChange, FOnChangedSwitch, FOnZoomChanged,
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
    function DoSectionCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
    function DoSectionCanEdit(const Sender: TObject): Boolean;
    procedure DoCaretChange;
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

    procedure DoViewResize;
    /// <summary> 文档"背板"变动(数据无变化，如对称边距，缩放视图) </summary>
    procedure DoMapChanged;
    procedure DoSectionCreateItem(Sender: TObject);
    procedure DoSectionReadOnlySwitch(Sender: TObject);
    function DoSectionGetScreenCoord(const X, Y: Integer): TPoint;
    procedure DoSectionInsertItem(const Sender: TObject; const AData: THCCustomData;
      const AItem: THCCustomItem);
    procedure DoSectionRemoveItem(const Sender: TObject; const AData: THCCustomData;
      const AItem: THCCustomItem);
    procedure DoSectionItemMouseUp(const Sender: TObject; const AData: THCCustomData;
      const AItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoSectionItemResize(const AData: THCCustomData; const AItemNo: Integer);
    procedure DoSectionDrawItemPaintBefor(const Sender: TObject; const AData: THCCustomData;
      const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

    procedure DoSectionDrawItemPaintContent(const AData: THCCustomData;
      const ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
      const ADrawText: string; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

    procedure DoSectionPaintHeader(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintFooter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintPage(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintWholePageBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintWholePageAfter(const Sender: TObject; const APageIndex: Integer;
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

    /// <summary> 重新获取光标位置 </summary>
    procedure ReBuildCaret;
    procedure GetSectionByCrood(const X, Y: Integer; var ASectionIndex: Integer);
    procedure SetZoom(const Value: Single);

    /// <summary> 删除不使用的文本样式 </summary>
    procedure _DeleteUnUsedStyle(const AAreas: TSectionAreas = [saHeader, saPage, saFooter]);

    function GetHScrollValue: Integer;
    function GetVScrollValue: Integer;
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

    procedure AutoScrollTimer(const AStart: Boolean);
    // Imm
    procedure UpdateImmPosition;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoChange; virtual;
    procedure DoSectionDrawItemPaintAfter(const Sender: TObject; const AData: THCCustomData;
      const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    /// <summary> 是否上屏输入法输入的词条屏词条ID和词条 </summary>
    function DoProcessIMECandi(const ACandi: string): Boolean; virtual;

    /// <summary> 实现插入文本 </summary>
    function DoInsertText(const AText: string): Boolean; virtual;

    /// <summary> 复制前，便于订制特征数据如内容来源 </summary>
    procedure DoCopyDataBefor(const AStream: TStream); virtual;

    /// <summary> 粘贴前，便于确认订制特征数据如内容来源 </summary>
    procedure DoPasteDataBefor(const AStream: TStream; const AVersion: Word); virtual;

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
    procedure WndProc(var Message: TMessage); override;
    //
    procedure CalcScrollRang;

    /// <summary> 是否由滚动条位置变化引起的更新 </summary>
    procedure CheckUpdateInfo;
    //
    procedure SetPageScrollModel(const Value: THCPageScrollModel);
    procedure SetViewModel(const Value: THCViewModel);
    procedure SetActiveSectionIndex(const Value: Integer);
    //
    procedure SetIsChanged(const Value: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary> 重设当前节纸张边距 </summary>
    procedure ResetActiveSectionMargin;

    /// <summary> 全部清空(清除各节页眉、页脚、页面的Item及DrawItem) </summary>
    procedure Clear;

    /// <summary> 取消选中 </summary>
    procedure DisSelect;

    /// <summary> 删除选中内容 </summary>
    procedure DeleteSelected;

    /// <summary> 删除当前节 </summary>
    procedure DeleteActiveSection;

    /// <summary> 各节重新计算排版 </summary>
    procedure FormatData;

    /// <summary> 插入流 </summary>
    function InsertStream(const AStream: TStream): Boolean;

    /// <summary> 插入文本(可包括#13#10) </summary>
    function InsertText(const AText: string): Boolean;

    /// <summary> 插入指定行列的表格 </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;

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

    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;

    /// <summary> 当前表格选中行下面插入行 </summary>
    function ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;

    /// <summary> 当前表格选中行上面插入行 </summary>
    function ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;

    /// <summary> 当前表格删除选中的行 </summary>
    function ActiveTableDeleteCurRow: Boolean;

    function ActiveTableSplitCurRow: Boolean;
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

    /// <summary> 修改当前光标所在段行间距 </summary>
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode);

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
    //
    /// <summary> 返回当前节当前Item </summary>
    function GetCurItem: THCCustomItem;

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

    /// <summary> 返回指定节页面绘制时Left位置 </summary>
    function GetSectionDrawLeft(const ASectionIndex: Integer): Integer;

    /// <summary> 返回光标处DrawItem相对当前页显示的窗体坐标 </summary>
    /// <returns>坐标</returns>
    function GetActiveDrawItemClientCoord: TPoint;

    /// <summary> 格式化指定节的数据 </summary>
    procedure FormatSection(const ASectionIndex: Integer);

    /// <summary> 获取当前节对象 </summary>
    function ActiveSection: THCSection;

    /// <summary> 获取当前节顶层Data </summary>
    function ActiveSectionTopLevelData: THCRichData;

    /// <summary> 指定节在整个胶卷中的Top位置 </summary>
    function GetSectionTopFilm(const ASectionIndex: Integer): Integer;

    // 保存文档
    /// <summary> 文档保存为hcf格式 </summary>
    procedure SaveToFile(const AFileName: string; const AQuick: Boolean = False);

    /// <summary> 读取hcf文件 </summary>
    procedure LoadFromFile(const AFileName: string);

    /// <summary> 读取其他格式的文件 </summary>
    procedure LoadFromDocumentFile(const AFileName: string; const AExt: string);

    /// <summary> 另存为其他格式的文件 </summary>
    procedure SaveToDocumentFile(const AFileName: string; const AExt: string);

    /// <summary> 读取其他格式的文件流 </summary>
    procedure LoadFromDocumentStream(const AStream: TStream; const AExt: string);

    /// <summary> 文档保存为PDF格式 </summary>
    procedure SaveToPDF(const AFileName: string);

    /// <summary> 以字符串形式获取文档各节正文内容 </summary>
    function SaveToText: string;

    /// <summary> 读文本到第一节正文 </summary>
    procedure LoadFromText(const AText: string);

    /// <summary> 文档各节正文字符串保存为文本格式文件 </summary>
    procedure SaveToTextFile(const AFileName: string; const AEncoding: TEncoding);

    /// <summary> 读取文本文件内容到第一节正文 </summary>
    procedure LoadFromTextFile(const AFileName: string; const AEncoding: TEncoding);

    /// <summary> 文档各节正文字符串保存为文本格式流 </summary>
    procedure SaveToTextStream(const AStream: TStream; const AEncoding: TEncoding);

    /// <summary> 读取文本文件流 </summary>
    procedure LoadFromTextStream(const AStream: TStream; AEncoding: TEncoding);

    /// <summary> 文档保存到流 </summary>
    procedure SaveToStream(const AStream: TStream; const AQuick: Boolean = False;
      const AAreas: TSectionAreas = [saHeader, saPage, saFooter]); virtual;

    /// <summary> 读取文件流 </summary>
    procedure LoadFromStream(const AStream: TStream); virtual;

    /// <summary> 文档保存为xml格式 </summary>
    procedure SaveToXml(const AFileName: string; const AEncoding: TEncoding);

    /// <summary> 读取xml格式 </summary>
    procedure LoadFromXml(const AFileName: string);

    /// <summary> 导出为html格式 </summary>
    /// <param name="ASeparateSrc">True：图片等保存到文件夹，False以base64方式存储到页面中</param>
    procedure SaveToHtml(const AFileName: string; const ASeparateSrc: Boolean = False);

    /// <summary> 获取指定页所在的节和相对此节的页序号 </summary>
    /// <param name="APageIndex">页序号</param>
    /// <param name="ASectionPageIndex">返回相对所在节的序号</param>
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

    /// <summary> 从当前行打印当前页(仅限正文) </summary>
    /// <param name="APrintHeader"> 是否打印页眉 </param>
    /// <param name="APrintFooter"> 是否打印页脚 </param>
    function PrintCurPageByActiveLine(const APrintHeader, APrintFooter: Boolean): TPrintResult;

    /// <summary> 打印当前页指定的起始、结束Item(仅限正文) </summary>
    /// <param name="APrintHeader"> 是否打印页眉 </param>
    /// <param name="APrintFooter"> 是否打印页脚 </param>
    function PrintCurPageByItemRange(const APrintHeader, APrintFooter: Boolean;
      const AStartItemNo, AStartOffset, AEndItemNo, AEndOffset: Integer): TPrintResult;

    /// <summary> 打印当前页选中的起始、结束Item(仅限正文) </summary>
    /// <param name="APrintHeader"> 是否打印页眉 </param>
    /// <param name="APrintFooter"> 是否打印页脚 </param>
    function PrintCurPageSelected(const APrintHeader, APrintFooter: Boolean): TPrintResult;

    /// <summary> 合并表格选中的单元格 </summary>
    function MergeTableSelectCells: Boolean;

    /// <summary> 撤销 </summary>
    procedure Undo;

    /// <summary> 重做 </summary>
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

    /// <summary> 水平滚动条的值 </summary>
    property HScrollValue: Integer read GetHScrollValue;

    /// <summary> 垂直滚动条的值 </summary>
    property VScrollValue: Integer read GetVScrollValue;
    /// <summary> 当前光标处的文本样式 </summary>
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

    property AnnotatePre: THCAnnotatePre read FAnnotatePre;

    property ViewWidth: Integer read FViewWidth;
    property ViewHeight: Integer read FViewHeight;
  published
    { Published declarations }

    /// <summary> 节有新的Item创建时触发 </summary>
    property OnSectionCreateItem: TNotifyEvent read FOnSectionCreateItem write FOnSectionCreateItem;

    /// <summary> 节有新的Item插入时触发 </summary>
    property OnSectionItemInsert: TSectionDataItemNotifyEvent read FOnSectionInsertItem write FOnSectionInsertItem;

    property OnSectionRemoveItem: TSectionDataItemNotifyEvent read FOnSectionRemoveItem write FOnSectionRemoveItem;

    /// <summary> Item绘制开始前触发 </summary>
    property OnSectionDrawItemPaintBefor: TSectionDrawItemPaintEvent read FOnSectionDrawItemPaintBefor write FOnSectionDrawItemPaintBefor;

    /// <summary> DrawItem绘制完成后触发 </summary>
    property OnSectionDrawItemPaintAfter: TSectionDrawItemPaintEvent read FOnSectionDrawItemPaintAfter write FOnSectionDrawItemPaintAfter;

    /// <summary> 节页眉绘制时触发 </summary>
    property OnSectionPaintHeader: TSectionPagePaintEvent read FOnSectionPaintHeader write FOnSectionPaintHeader;

    /// <summary> 节页脚绘制时触发 </summary>
    property OnSectionPaintFooter: TSectionPagePaintEvent read FOnSectionPaintFooter write FOnSectionPaintFooter;

    /// <summary> 节页面绘制时触发 </summary>
    property OnSectionPaintPage: TSectionPagePaintEvent read FOnSectionPaintPage write FOnSectionPaintPage;

    /// <summary> 节整页绘制前触发 </summary>
    property OnSectionPaintWholePageBefor: TSectionPagePaintEvent read FOnSectionPaintWholePageBefor write FOnSectionPaintWholePageBefor;

    /// <summary> 节整页绘制后触发 </summary>
    property OnSectionPaintWholePageAfter: TSectionPagePaintEvent read FOnSectionPaintWholePageAfter write FOnSectionPaintWholePageAfter;

    /// <summary> 节只读属性有变化时触发 </summary>
    property OnSectionReadOnlySwitch: TNotifyEvent read FOnSectionReadOnlySwitch write FOnSectionReadOnlySwitch;

    /// <summary> 页面滚动显示模式：纵向、横向 </summary>
    property PageScrollModel: THCPageScrollModel read FPageScrollModel write SetPageScrollModel;

    /// <summary> 界面显示模式：页面、Web </summary>
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
    property OnChangedSwitch: TNotifyEvent read FOnChangedSwitch write FOnChangedSwitch;

    /// <summary> 文档Zoom缩放变化后触发 </summary>
    property OnZoomChanged: TNotifyEvent read FOnZoomChanged write FOnZoomChanged;

    /// <summary> 窗口重绘开始时触发 </summary>
    property OnPaintViewBefor: TPaintEvent read FOnPaintViewBefor write FOnPaintViewBefor;

    /// <summary> 窗口重绘结束后触发 </summary>
    property OnPaintViewAfter: TPaintEvent read FOnPaintViewAfter write FOnPaintViewAfter;

    property OnSectionCreateStyleItem: TStyleItemEvent read FOnSectionCreateStyleItem write FOnSectionCreateStyleItem;

    property OnSectionCanEdit: TOnCanEditEvent read FOnSectionCanEdit write FOnSectionCanEdit;

    property OnSectionCurParaNoChange: TNotifyEvent read FOnSectionCurParaNoChange write FOnSectionCurParaNoChange;
    property OnSectionActivePageChange: TNotifyEvent read FOnSectionActivePageChange write FOnSectionActivePageChange;

    property OnViewResize: TNotifyEvent read FOnViewResize write FOnViewResize;

    property Color;
    property PopupMenu;
  end;

//procedure Register;

implementation

uses
  Printers, Imm, Forms, Math, Clipbrd, HCImageItem, ShellAPI, HCXml, HCDocumentRW,
  HCRtfRW, HCUnitConversion;

const
  IMN_UPDATECURSTRING = $F000;  // 和输入法交互，当前光标处的字符串

{procedure Register;
begin
  RegisterComponents('HCControl', [THCView]);
end;  }

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
  vHMax := FSections[0].PageWidthPix;
  for i := 0 to FSections.Count - 1 do  //  计算节垂直总和，以及节中最宽的页宽度
  begin
    vVMax := vVMax + FSections[i].GetFilmHeight;

    vWidth := FSections[i].PageWidthPix;

    if vWidth > vHMax then
      vHMax := vWidth;
  end;

  if FAnnotatePre.Count > 0 then
    vHMax := vHMax + AnnotationWidth;

  vVMax := ZoomIn(vVMax + PagePadding);  // 补充最后一页后面的PagePadding
  vHMax := ZoomIn(vHMax + PagePadding + PagePadding);

  FVScrollBar.Max := vVMax;
  FHScrollBar.Max := vHMax;
end;

procedure THCView.CheckUpdateInfo;
var
  vCaretChange: Boolean;
begin
  if FUpdateCount > 0 then Exit;

  // UpdateView里会计算当前显示页，ReBuildCaret里会通知外部重新获取当前预览页和光标所在页
  // ReBuildCaret后有些需要高亮重绘，所以要先处理，所以防止处理完通知外部取页数不对的问题
  // 使用vCaretChange将通知放到最后
  if (FCaret <> nil) and FStyle.UpdateInfo.ReCaret then  // 先处理光标，因为可能光标处有些需要高亮重绘
  begin
    ReBuildCaret;
    vCaretChange := True;
    FStyle.UpdateInfo.ReCaret := False;
    FStyle.UpdateInfo.ReStyle := False;
    FStyle.UpdateInfo.ReScroll := False;
    UpdateImmPosition;
  end
  else
    vCaretChange := False;

  if FStyle.UpdateInfo.RePaint then
  begin
    FStyle.UpdateInfo.RePaint := False;
    UpdateView;
  end;

  if vCaretChange then
    DoCaretChange;
end;

procedure THCView.Clear;
begin
  FStyle.Initialize;  // 先清样式，防止Data初始化为EmptyData时空Item样式赋值为CurStyleNo
  FSections.DeleteRange(1, FSections.Count - 1);

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
  FActiveSectionIndex := 0;
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret;
  DoMapChanged;
  DoViewResize;
end;

procedure THCView.Copy;
var
  vStream: TMemoryStream;
  vMem: Cardinal;
  vPtr: Pointer;
begin
  if ActiveSection.SelectExists then
  begin
    vStream := TMemoryStream.Create;
    try
      _SaveFileFormatAndVersion(vStream);  // 保存文件格式和版本
      DoCopyDataBefor(vStream);  // 通知保存事件
      _DeleteUnUsedStyle;  // 保存已使用的样式
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

      Clipboard.SetAsHandle(HC_FILEFORMAT, vMem);  // HC格式
      Clipboard.AsText := Self.ActiveSectionTopLevelData.SaveSelectToText;  // 文本格式
    finally
      Clipboard.Close;
    end;
  end;
end;

procedure THCView.CopyAsText;
begin
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

  FAnnotatePre := THCAnnotatePre.Create;
  FAnnotatePre.OnUpdateView := DoAnnotatePreUpdateView;

  FFileName := '';
  FPageNoFormat := '%d/%d';
  FIsChanged := False;
  FZoom := 1;
  FAutoZoom := False;
  FViewModel := vmPage;
  FPageScrollModel := psmVertical;  //psmHorizontal;

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
  // 水平滚动条，范围在Resize中设置
  FHScrollBar := THCScrollBar.Create(Self);
  FHScrollBar.Orientation := TOrientation.oriHorizontal;
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

function THCView.ActiveSection: THCSection;
begin
  Result := FSections[FActiveSectionIndex];
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

destructor THCView.Destroy;
begin
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
    if FPageScrollModel = psmVertical then
      FVScrollBar.Position := FVScrollBar.Position - WheelDelta
    else
      FHScrollBar.Position := FHScrollBar.Position - WheelDelta;
  end;
  Result := True;
end;

procedure THCView.DoSectionPaintPage(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if Assigned(FOnSectionPaintPage) then
    FOnSectionPaintPage(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintFooter(const Sender: TObject;
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
    ACanvas.TextOut(ARect.Left + (ARect.Width - ACanvas.TextWidth(vS)) div 2, ARect.Top + 20, vS);
  end;

  if Assigned(FOnSectionPaintFooter) then
    FOnSectionPaintFooter(vSection, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintHeader(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if Assigned(FOnSectionPaintHeader) then
    FOnSectionPaintHeader(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintWholePageAfter(const Sender: TObject; const APageIndex: Integer;
  const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
begin
  if FAnnotatePre.Count > 0 then  // 当前页有批注，绘制批注
    FAnnotatePre.PaintDrawAnnotate(Sender, ARect, ACanvas, APaintInfo);

  if Assigned(FOnSectionPaintWholePageAfter) then
    FOnSectionPaintWholePageAfter(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintWholePageBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if APaintInfo.Print and (FAnnotatePre.DrawCount > 0) then  // 打印是单面绘制，所以每一页前清除
    FAnnotatePre.ClearDrawAnnotate;

  if Assigned(FOnSectionPaintWholePageBefor) then
    FOnSectionPaintWholePageBefor(Sender, APageIndex, ARect, ACanvas, APaintInfo);
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

function THCView.DoSectionGetScreenCoord(const X, Y: Integer): TPoint;
begin
  Result := ClientToScreen(Point(X, Y));
end;

function THCView.DoSectionGetUndoList: THCUndoList;
begin
  Result := FUndoList;
end;

procedure THCView.DoPasteDataBefor(const AStream: TStream; const AVersion: Word);
begin
end;

function THCView.DoProcessIMECandi(const ACandi: string): Boolean;
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

procedure THCView.DoAnnotatePreUpdateView(Sender: TObject);
begin
  if FAnnotatePre.Count = 0 then
  begin
    FStyle.UpdateInfoRePaint;
    DoMapChanged;
  end
  else
    UpdateView;
end;

procedure THCView.DoCaretChange;
begin
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

procedure THCView.DoCopyDataBefor(const AStream: TStream);
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

procedure THCView.DoSectionItemMouseUp(const Sender: TObject;
  const AData: THCCustomData; const AItemNo: Integer; Button: TMouseButton;
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

procedure THCView.DoSectionDrawItemPaintAfter(const Sender: TObject;
  const AData: THCCustomData; const ADrawItemNo: Integer; const ADrawRect: TRect;
  const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnSectionDrawItemPaintAfter) then
    FOnSectionDrawItemPaintAfter(Sender, AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionDrawItemPaintBefor(const Sender: TObject;
  const AData: THCCustomData; const ADrawItemNo: Integer; const ADrawRect: TRect;
  const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnSectionDrawItemPaintBefor) then
    FOnSectionDrawItemPaintBefor(Sender, AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionDrawItemPaintContent(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
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

function THCView.GetActiveDrawItemClientCoord: TPoint;
var
  vPageIndex: Integer;
begin
  Result := ActiveSection.GetActiveDrawItemCoord;  // 有选中时，以选中结束位置的DrawItem格式化坐标
  vPageIndex := ActiveSection.GetPageIndexByFormat(Result.Y);

  // 映射到节页面(白色区域)
  Result.X := ZoomIn(GetSectionDrawLeft(Self.ActiveSectionIndex)
    + (ActiveSection.GetPageMarginLeft(vPageIndex) + Result.X)) - Self.HScrollValue;

  if ActiveSection.ActiveData = ActiveSection.Header then
    Result.Y := ZoomIn(GetSectionTopFilm(Self.ActiveSectionIndex)
      + ActiveSection.GetPageTopFilm(vPageIndex)  // 20
      + ActiveSection.GetHeaderPageDrawTop
      + Result.Y
      - ActiveSection.GetPageDataFmtTop(vPageIndex))  // 0
      - Self.VScrollValue
  else
  if ActiveSection.ActiveData = ActiveSection.Footer then
    Result.Y := ZoomIn(GetSectionTopFilm(Self.ActiveSectionIndex)
      + ActiveSection.GetPageTopFilm(vPageIndex)  // 20
      + ActiveSection.PageHeightPix - ActiveSection.PageMarginBottomPix
      + Result.Y
      - ActiveSection.GetPageDataFmtTop(vPageIndex))  // 0
      - Self.VScrollValue
  else
    Result.Y := ZoomIn(GetSectionTopFilm(Self.ActiveSectionIndex)
      + ActiveSection.GetPageTopFilm(vPageIndex)  // 20
      + ActiveSection.GetHeaderAreaHeight // 94
      + Result.Y
      - ActiveSection.GetPageDataFmtTop(vPageIndex))  // 0
      - Self.VScrollValue;
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

function THCView.GetCurItem: THCCustomItem;
begin
  Result := ActiveSection.GetCurItem;
end;

function THCView.GetCurParaNo: Integer;
begin
  Result := ActiveSection.CurParaNo;
end;

function THCView.GetCurStyleNo: Integer;
begin
  Result := ActiveSection.CurStyleNo;
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
  if (ASectionIndex < 0) and (vY + PagePadding >= Y) then  // 最后一页后面的Padding
    ASectionIndex := FSections.Count - 1;

  Assert(ASectionIndex >= 0, '没有获取到正确的节序号！');
end;

function THCView.GetSectionDrawLeft(const ASectionIndex: Integer): Integer;
begin
  if FAnnotatePre.Count > 0 then  // 显示批注
    Result := Max((FViewWidth - ZoomIn(FSections[ASectionIndex].PageWidthPix + AnnotationWidth)) div 2, ZoomIn(PagePadding))
  else
    Result := Max((FViewWidth - ZoomIn(FSections[ASectionIndex].PageWidthPix)) div 2, ZoomIn(PagePadding));
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
      ASectionPageIndex := APageIndex - vPageCount;  // FSections[i].PageCount;
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
  Result := FSections[0].PageData.ShowLineActiveMark;
end;

function THCView.GetShowLineNo: Boolean;
begin
  Result := FSections[0].PageData.ShowLineNo;
end;

function THCView.GetShowUnderLine: Boolean;
begin
  Result := FSections[0].PageData.ShowUnderLine;
end;

function THCView.GetSymmetryMargin: Boolean;
begin
  Result := ActiveSection.SymmetryMargin;
end;

function THCView.GetVScrollValue: Integer;
begin
  Result := FVScrollBar.Position;
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
              // 不循环，只插入第一节的正文
              vSection.LoadFromStream(AStream, vStyle, AFileVersion);
              vDataStream.Clear;
              vSection.PageData.SaveToStream(vDataStream);
              vDataStream.Position := 0;
              vDataStream.ReadBuffer(vShowUnderLine, SizeOf(vShowUnderLine));
              vResult := ActiveSection.InsertStream(vDataStream, vStyle, AFileVersion);  // 只插入第一节的数据
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

function THCView.ActiveTableSplitCurCol: Boolean;
begin
  Result := ActiveSection.ActiveTableSplitCurCol;
end;

function THCView.ActiveTableSplitCurRow: Boolean;
begin
  Result := ActiveSection.ActiveTableSplitCurRow;
end;

function THCView.ActiveSectionTopLevelData: THCRichData;
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
  ActiveSection.KeyPress(Key);
end;

procedure THCView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
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

procedure THCView.LoadFromFile(const AFileName: string);
var
  vStream: TStream;
begin
  FFileName := AFileName;
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure THCView.LoadFromStream(const AStream: TStream);
var
  vByte: Byte;
  vSection: THCSection;
begin
  Self.BeginUpdate;
  try
    // 清除撤销恢复数据
    FUndoList.Clear;
    FUndoList.SaveState;
    try
      FUndoList.Enable := False;

      Self.Clear;
      AStream.Position := 0;
      DoLoadFromStream(AStream, FStyle, procedure(const AFileVersion: Word)
        var
          i: Integer;
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

      DoViewResize;
    finally
      FUndoList.RestoreState;
    end;
  finally
    Self.EndUpdate;
  end;
end;

procedure THCView.LoadFromText(const AText: string);
begin
  Self.Clear;
  FStyle.Initialize;

  if AText <> '' then
    ActiveSection.InsertText(AText);
end;

procedure THCView.LoadFromTextFile(const AFileName: string; const AEncoding: TEncoding);
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    vStream.LoadFromFile(AFileName);
    vStream.Position := 0;
    LoadFromTextStream(vStream, AEncoding);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure THCView.LoadFromTextStream(const AStream: TStream; AEncoding: TEncoding);
var
  vSize: Integer;
  vBuffer: TBytes;
  vS: string;
begin
  vSize := AStream.Size - AStream.Position;
  SetLength(vBuffer, vSize);
  AStream.Read(vBuffer[0], vSize);
  vSize := TEncoding.GetBufferEncoding(vBuffer, AEncoding);
  vS := AEncoding.GetString(vBuffer, vSize, Length(vBuffer) - vSize);
  LoadFromText(vS);
end;

procedure THCView.LoadFromXml(const AFileName: string);

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
  Self.BeginUpdate;
  try
    // 清除撤销恢复数据
    FUndoList.Clear;
    FUndoList.SaveState;
    try
      FUndoList.Enable := False;
      Self.Clear;

      vXml := THCXMLDocument.Create(nil);
      vXml.LoadFromFile(AFileName);
      if vXml.DocumentElement.LocalName = 'HCView' then
      begin
        if vXml.DocumentElement.Attributes['EXT'] <> HC_EXT then Exit;

        vVersion := vXml.DocumentElement.Attributes['ver'];
        vLang := vXml.DocumentElement.Attributes['lang'];

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

//  if FAnnotatePre.DrawCount > 0 then  // 有批注被绘制
//  begin
//    //if (X + HScrollValue > vSectionDrawLeft + FSections[FActiveSectionIndex].PageWidthPix)
//    //  and (X + HScrollValue < vSectionDrawLeft + FSections[FActiveSectionIndex].PageWidthPix + AnnotationWidth)
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

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);

  FStyle.UpdateInfo.Selecting := False;
  FStyle.UpdateInfo.Draging := False;
end;

function THCView.NewDefaultSection: THCSection;
begin
  Result := THCSection.Create(FStyle);
  // 创建节后马上赋值事件（保证后续插入表格等需要这些事件的操作可获取到事件）
  Result.OnDataChange := DoSectionDataChange;
  Result.OnChangeTopLevelData := DoSectionChangeTopLevelData;
  Result.OnCheckUpdateInfo := DoSectionDataCheckUpdateInfo;
  Result.OnCreateItem := DoSectionCreateItem;
  Result.OnCreateItemByStyle := DoSectionCreateStyleItem;
  Result.OnCanEdit := DoSectionCanEdit;
  Result.OnInsertItem := DoSectionInsertItem;
  Result.OnRemoveItem := DoSectionRemoveItem;
  Result.OnItemMouseUp := DoSectionItemMouseUp;
  Result.OnItemResize := DoSectionItemResize;
  Result.OnReadOnlySwitch := DoSectionReadOnlySwitch;
  Result.OnGetScreenCoord := DoSectionGetScreenCoord;
  Result.OnDrawItemPaintAfter := DoSectionDrawItemPaintAfter;
  Result.OnDrawItemPaintBefor := DoSectionDrawItemPaintBefor;
  Result.OnDrawItemPaintContent := DoSectionDrawItemPaintContent;
  Result.OnPaintHeader := DoSectionPaintHeader;
  Result.OnPaintFooter := DoSectionPaintFooter;
  Result.OnPaintPage := DoSectionPaintPage;
  Result.OnPaintWholePageBefor := DoSectionPaintWholePageBefor;
  Result.OnPaintWholePageAfter := DoSectionPaintWholePageAfter;
  Result.OnInsertAnnotate := DoSectionInsertAnnotate;
  Result.OnRemoveAnnotate := DoSectionRemoveAnnotate;
  Result.OnDrawItemAnnotate := DoSectionDrawItemAnnotate;
  Result.OnGetUndoList := DoSectionGetUndoList;
  Result.OnCurParaNoChange := DoSectionCurParaNoChange;
  Result.OnActivePageChange := DoSectionActivePageChange;
end;

procedure THCView.DoVerScroll(Sender: TObject; ScrollCode: TScrollCode;
  const ScrollPos: Integer);
begin
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret(False);
  CheckUpdateInfo;
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

function THCView.GetPagePreviewFirst: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FActiveSectionIndex - 1 do
    Result := Result + FSections[i].PageCount;
  Result := Result + FSections[FActiveSectionIndex].DisplayFirstPageIndex;
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

procedure THCView.Paint;
begin
  //Canvas.Draw(0, 0, FDataBmp);
  BitBlt(Canvas.Handle, 0, 0, FViewWidth, FViewHeight,
      FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
  Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(Bounds(FVScrollBar.Left, FHScrollBar.Top, FVScrollBar.Width, FHScrollBar.Height));
end;

procedure THCView.Paste;

  procedure PasteImage;
  var
    vImageItem: THCImageItem;
    vTopData: THCRichData;
  begin
    vTopData := Self.ActiveSectionTopLevelData;
    vImageItem := THCImageItem.Create(vTopData);
    vImageItem.Image.Assign(Clipboard);
    vImageItem.Width := vImageItem.Image.Width;
    vImageItem.Height := vImageItem.Image.Height;

    vImageItem.RestrainSize(vTopData.Width, vImageItem.Height);
    Self.InsertItem(vImageItem);
  end;

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
      try
        vData := GetClipboardData(CF_RTF);
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
  if Clipboard.HasFormat(HC_FILEFORMAT) then
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
      DoPasteDataBefor(vStream, vFileVersion);
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
  if Clipboard.HasFormat(CF_RTF) then
    PasteRtf
  else
  if Clipboard.HasFormat(CF_HTML) then
    PasteHtml
  else
  if Clipboard.HasFormat(CF_TEXT) then
    InsertText(Clipboard.AsText)
  else
  if Clipboard.HasFormat(CF_BITMAP) then
    PasteImage;
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
  i, vPageIndex, vSectionIndex, vPrintWidth, vPrintHeight,
  vPrintOffsetX, vPrintOffsetY: Integer;
  vPrintCanvas: TCanvas;
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
begin
  Result := prError;

  if APrinter <> '' then
    Printer.PrinterIndex := Printer.Printers.IndexOf(APrinter);

  if Printer.PrinterIndex < 0 then Exit;

  Printer.Title := FFileName;

  // 取打印机打印区域相关参数
  vPrintOffsetX := -GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);  // 73
  vPrintOffsetY := -GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);  // 37

  Printer.Copies := ACopies;

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;

    Printer.BeginDoc;
    try
      vPrintCanvas := TCanvas.Create;
      try
        vPrintCanvas.Handle := Printer.Canvas.Handle;  // 为什么不用vPrintCanvas中介打印就不行呢？

        for i := Low(APages) to High(APages) do
        begin
          // 根据页码获取起始节和结束节
          vSectionIndex := GetSectionPageIndexByPageIndex(APages[i], vPageIndex);
          if vPaintInfo.SectionIndex <> vSectionIndex then
          begin
            vPaintInfo.SectionIndex := vSectionIndex;
            SetPrintBySectionInfo(vSectionIndex);

            vPrintWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);  // 4961
            vPrintHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);  // 7016

            if FSections[vSectionIndex].PageData.DataAnnotates.Count > 0 then
            begin
              vPaintInfo.ScaleX := vPrintWidth / (FSections[vSectionIndex].PageWidthPix + AnnotationWidth);
              vPaintInfo.ScaleY := vPrintHeight / (FSections[vSectionIndex].PageHeightPix + AnnotationWidth * vPrintHeight / vPrintWidth);
              vPaintInfo.Zoom := FSections[vSectionIndex].PageWidthPix / (FSections[vSectionIndex].PageWidthPix + AnnotationWidth);
            end
            else
            begin
              vPaintInfo.ScaleX := vPrintWidth / FSections[vSectionIndex].PageWidthPix;  // GetDeviceCaps(Printer.Handle, LOGPIXELSX) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSX);
              vPaintInfo.ScaleY := vPrintHeight / FSections[vSectionIndex].PageHeightPix;  // GetDeviceCaps(Printer.Handle, LOGPIXELSY) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSY);
              vPaintInfo.Zoom := 1;
            end;

            vPaintInfo.WindowWidth := vPrintWidth;  // FSections[vStartSection].PageWidthPix;
            vPaintInfo.WindowHeight := vPrintHeight;  // FSections[vStartSection].PageHeightPix;

            vPrintOffsetX := Round(vPrintOffsetX / vPaintInfo.ScaleX);
            vPrintOffsetY := Round(vPrintOffsetY / vPaintInfo.ScaleY);
          end;

          vScaleInfo := vPaintInfo.ScaleCanvas(vPrintCanvas);
          try
            vPaintInfo.PageIndex := APages[i];

            FSections[vSectionIndex].PaintPage(APages[i], vPrintOffsetX, vPrintOffsetY,
              vPrintCanvas, vPaintInfo);

            if i < High(APages) then
              Printer.NewPage;
          finally
            vPaintInfo.RestoreCanvasScale(vPrintCanvas, vScaleInfo);
          end;
        end;
      finally
        vPrintCanvas.Handle := 0;
        vPrintCanvas.Free;
      end;
    finally
      Printer.EndDoc;
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

function THCView.PrintCurPageByActiveLine(const APrintHeader,
  APrintFooter: Boolean): TPrintResult;
var
  vPt: TPoint;
  vPrintCanvas: TCanvas;
  vPrintWidth, vPrintHeight, vPrintOffsetX, vPrintOffsetY: Integer;
  vMarginLeft, vMarginRight: Integer;
  vRect: TRect;
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
begin
  Result := TPrintResult.prError;

  vPrintOffsetX := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);  // 90
  vPrintOffsetY := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);  // 99

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;
    vPaintInfo.SectionIndex := Self.ActiveSectionIndex;
    vPaintInfo.PageIndex := Self.ActiveSection.ActivePageIndex;

    SetPrintBySectionInfo(Self.ActiveSectionIndex);

    vPrintWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
    vPrintHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);

    if Self.ActiveSection.PageData.DataAnnotates.Count > 0 then
    begin
      vPaintInfo.ScaleX := vPrintWidth / (Self.ActiveSection.PageWidthPix + AnnotationWidth);
      vPaintInfo.ScaleY := vPrintHeight / (Self.ActiveSection.PageHeightPix + AnnotationWidth * vPrintHeight / vPrintWidth);
      vPaintInfo.Zoom := Self.ActiveSection.PageWidthPix / (Self.ActiveSection.PageWidthPix + AnnotationWidth);
    end
    else
    begin
      vPaintInfo.ScaleX := vPrintWidth / Self.ActiveSection.PageWidthPix;  // GetDeviceCaps(Printer.Handle, LOGPIXELSX) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSX);
      vPaintInfo.ScaleY := vPrintHeight / Self.ActiveSection.PageHeightPix;  // GetDeviceCaps(Printer.Handle, LOGPIXELSY) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSY);
      vPaintInfo.Zoom := 1;
    end;

    vPaintInfo.WindowWidth := vPrintWidth;  // FSections[vStartSection].PageWidthPix;
    vPaintInfo.WindowHeight := vPrintHeight;  // FSections[vStartSection].PageHeightPix;

    vPrintOffsetX := Round(vPrintOffsetX / vPaintInfo.ScaleX);
    vPrintOffsetY := Round(vPrintOffsetY / vPaintInfo.ScaleY);

    Printer.BeginDoc;
    try
      vPrintCanvas := TCanvas.Create;
      try
        vPrintCanvas.Handle := Printer.Canvas.Handle;  // 为什么不用vPageCanvas中介打印就不行呢？

        vScaleInfo := vPaintInfo.ScaleCanvas(vPrintCanvas);
        try
          Self.ActiveSection.PaintPage(Self.ActiveSection.ActivePageIndex,
            vPrintOffsetX, vPrintOffsetY, vPrintCanvas, vPaintInfo);

          if Self.ActiveSection.ActiveData = Self.ActiveSection.PageData then
          begin
            vPt := Self.ActiveSection.GetActiveDrawItemCoord;
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

          if APrintHeader then  // 打印页眉
            vRect := Bounds(vPrintOffsetX + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight,  // 页眉下边
              Self.ActiveSection.PageWidthPix - vMarginLeft - vMarginRight, vPt.Y)
          else  // 不打印页眉
            vRect := Bounds(vPrintOffsetX + vMarginLeft, vPrintOffsetY,
              Self.ActiveSection.PageWidthPix - vMarginLeft - vMarginRight,
              Self.ActiveSection.GetHeaderAreaHeight + vPt.Y);
          vPrintCanvas.FillRect(vRect);

          if not APrintFooter then  // 不打印页脚
          begin
            vRect := Bounds(vPrintOffsetX + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.PageHeightPix - Self.ActiveSection.PageMarginBottomPix,
              Self.ActiveSection.PageWidthPix - vMarginLeft - vMarginRight,
              Self.ActiveSection.PageMarginBottomPix);

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
      Printer.EndDoc;
    end;
  finally
    vPaintInfo.Free;
  end;

  Result := TPrintResult.prOk;
end;

function THCView.PrintCurPageByItemRange(const APrintHeader, APrintFooter: Boolean;
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

  vPrintOffsetX := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);  // 90
  vPrintOffsetY := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);  // 99

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;
    vPaintInfo.SectionIndex := Self.ActiveSectionIndex;
    vPaintInfo.PageIndex := Self.ActiveSection.ActivePageIndex;

    SetPrintBySectionInfo(Self.ActiveSectionIndex);

    vPrintWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
    vPrintHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);

    if Self.ActiveSection.PageData.DataAnnotates.Count > 0 then
    begin
      vPaintInfo.ScaleX := vPrintWidth / (Self.ActiveSection.PageWidthPix + AnnotationWidth);
      vPaintInfo.ScaleY := vPrintHeight / (Self.ActiveSection.PageHeightPix + AnnotationWidth * vPrintHeight / vPrintWidth);
      vPaintInfo.Zoom := Self.ActiveSection.PageWidthPix / (Self.ActiveSection.PageWidthPix + AnnotationWidth);
    end
    else
    begin
      vPaintInfo.ScaleX := vPrintWidth / Self.ActiveSection.PageWidthPix;  // GetDeviceCaps(Printer.Handle, LOGPIXELSX) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSX);
      vPaintInfo.ScaleY := vPrintHeight / Self.ActiveSection.PageHeightPix;  // GetDeviceCaps(Printer.Handle, LOGPIXELSY) / GetDeviceCaps(FStyle.DefCanvas.Handle, LOGPIXELSY);
      vPaintInfo.Zoom := 1;
    end;

    vPaintInfo.WindowWidth := vPrintWidth;  // FSections[vStartSection].PageWidthPix;
    vPaintInfo.WindowHeight := vPrintHeight;  // FSections[vStartSection].PageHeightPix;

    vPrintOffsetX := Round(vPrintOffsetX / vPaintInfo.ScaleX);
    vPrintOffsetY := Round(vPrintOffsetY / vPaintInfo.ScaleY);

    Printer.BeginDoc;
    try
      vPrintCanvas := TCanvas.Create;
      try
        vPrintCanvas.Handle := Printer.Canvas.Handle;  // 为什么不用vPageCanvas中介打印就不行呢？

        vScaleInfo := vPaintInfo.ScaleCanvas(vPrintCanvas);
        try
          Self.ActiveSection.PaintPage(Self.ActiveSection.ActivePageIndex,
            vPrintOffsetX, vPrintOffsetY, vPrintCanvas, vPaintInfo);

          if Self.ActiveSection.ActiveData = Self.ActiveSection.PageData then
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

          if APrintHeader then  // 打印页眉
            vRect := Bounds(vPrintOffsetX + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight,  // 页眉下边
              Self.ActiveSection.PageWidthPix - vMarginLeft - vMarginRight, vPt.Y)
          else  // 不打印页眉
            vRect := Bounds(vPrintOffsetX + vMarginLeft, vPrintOffsetY,
              Self.ActiveSection.PageWidthPix - vMarginLeft - vMarginRight,
              Self.ActiveSection.GetHeaderAreaHeight + vPt.Y);
          vPrintCanvas.FillRect(vRect);

          vRect := Bounds(vPrintOffsetX + vMarginLeft,
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y,
            vData.GetDrawItemOffsetWidth(vDrawItemNo, AStartOffset - vData.DrawItems[vDrawItemNo].CharOffs + 1),
            vData.DrawItems[vDrawItemNo].Rect.Height);
          vPrintCanvas.FillRect(vRect);

          //
          vDrawItemNo := vData.GetDrawItemNoByOffset(AEndItemNo, AEndOffset);
          vPt := vData.DrawItems[vDrawItemNo].Rect.TopLeft;
          vPt.Y := vPt.Y - ActiveSection.GetPageDataFmtTop(Self.ActiveSection.ActivePageIndex);

          vRect := Rect(
            vPrintOffsetX + vMarginLeft +
              vData.GetDrawItemOffsetWidth(vDrawItemNo, AEndOffset - vData.DrawItems[vDrawItemNo].CharOffs + 1),
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y,
            vPrintOffsetX + Self.ActiveSection.PageWidthPix - vMarginRight,
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y + vData.DrawItems[vDrawItemNo].Rect.Height);
          vPrintCanvas.FillRect(vRect);

          if not APrintFooter then  // 不打印页脚
          begin
            vRect := Rect(vPrintOffsetX + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y + vData.DrawItems[vDrawItemNo].Rect.Height,
              vPrintOffsetX + Self.ActiveSection.PageWidthPix - vMarginRight,
              vPrintOffsetY + Self.ActiveSection.PageHeightPix);

            vPrintCanvas.FillRect(vRect);
          end
          else  // 打印页脚
          begin
            vRect := Rect(vPrintOffsetX + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y + vData.DrawItems[vDrawItemNo].Rect.Height,
              vPrintOffsetX + Self.ActiveSection.PageWidthPix - vMarginRight,
              vPrintOffsetY + Self.ActiveSection.PageHeightPix - Self.ActiveSection.PageMarginBottomPix);
          end;
        finally
          vPaintInfo.RestoreCanvasScale(vPrintCanvas, vScaleInfo);
        end;
      finally
        vPrintCanvas.Handle := 0;
        vPrintCanvas.Free;
      end;
    finally
      Printer.EndDoc;
    end;
  finally
    vPaintInfo.Free;
  end;

  Result := TPrintResult.prOk;
end;

function THCView.PrintCurPageSelected(const APrintHeader,
  APrintFooter: Boolean): TPrintResult;
begin
  if Self.ActiveSection.ActiveData.SelectExists(False) then
  begin
    Result := PrintCurPageByItemRange(APrintHeader, APrintFooter,
      Self.ActiveSection.ActiveData.SelectInfo.StartItemNo,
      Self.ActiveSection.ActiveData.SelectInfo.StartItemOffset,
      Self.ActiveSection.ActiveData.SelectInfo.EndItemNo,
      Self.ActiveSection.ActiveData.SelectInfo.EndItemOffset);
  end
  else
    Result := TPrintResult.prNoSupport;
end;

procedure THCView.ReBuildCaret;
var
  vCaretInfo: THCCaretInfo;
begin
  if FCaret = nil then Exit;

  if (not Self.Focused) or ((not Style.UpdateInfo.Draging) and ActiveSection.SelectExists) then
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
  FCaret.X := ZoomIn(GetSectionDrawLeft(FActiveSectionIndex) + vCaretInfo.X) - FHScrollBar.Position;
  FCaret.Y := ZoomIn(GetSectionTopFilm(FActiveSectionIndex) + vCaretInfo.Y) - FVScrollBar.Position;
  FCaret.Height := ZoomIn(vCaretInfo.Height);

  if not FStyle.UpdateInfo.ReScroll then // 滚动条平滑滚动时，可能将光标卷掉看不见
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
      if FCaret.Y < 0 then
        FVScrollBar.Position := FVScrollBar.Position + FCaret.Y - PagePadding
      else
      if FCaret.Y + FCaret.Height + PagePadding > FViewHeight then
        FVScrollBar.Position := FVScrollBar.Position + FCaret.Y + FCaret.Height + PagePadding - FViewHeight;

      if FCaret.X < 0 then
        FHScrollBar.Position := FHScrollBar.Position + FCaret.X - PagePadding
      else
      if FCaret.X + PagePadding > FViewWidth then
        FHScrollBar.Position := FHScrollBar.Position + FCaret.X + PagePadding - FViewWidth;
    end;
  end;

  if FCaret.Y + FCaret.Height > FViewHeight then
    FCaret.Height := FViewHeight - FCaret.Y;

  FCaret.Show;
  DoCaretChange;
end;

procedure THCView.Redo;
begin
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
    if FAnnotatePre.Count > 0 then  // 显示批注
      FZoom := (FViewWidth - PagePadding * 2) / (ActiveSection.PageWidthPix + AnnotationWidth)
    else
      FZoom := (FViewWidth - PagePadding * 2) / ActiveSection.PageWidthPix;
  end;

  CalcScrollRang;

  FStyle.UpdateInfoRePaint;
  if FCaret <> nil then
    FStyle.UpdateInfoReCaret(False);
  CheckUpdateInfo;

  DoViewResize;
end;

procedure THCView._DeleteUnUsedStyle(const AAreas: TSectionAreas = [saHeader, saPage, saFooter]);
var
  i, vUnCount: Integer;
begin
  for i := 0 to FStyle.TextStyles.Count - 1 do
  begin
    FStyle.TextStyles[i].CheckSaveUsed := False;
    FStyle.TextStyles[i].TempNo := THCStyle.Null;
  end;
  for i := 0 to FStyle.ParaStyles.Count - 1 do
  begin
    FStyle.ParaStyles[i].CheckSaveUsed := False;
    FStyle.ParaStyles[i].TempNo := THCStyle.Null;
  end;

  for i := 0 to FSections.Count - 1 do
    FSections[i].MarkStyleUsed(True, AAreas);

  vUnCount := 0;
  for i := 0 to FStyle.TextStyles.Count - 1 do
  begin
    if FStyle.TextStyles[i].CheckSaveUsed then
      FStyle.TextStyles[i].TempNo := i - vUnCount
    else
      Inc(vUnCount);
  end;

  vUnCount := 0;
  for i := 0 to FStyle.ParaStyles.Count - 1 do
  begin
    if FStyle.ParaStyles[i].CheckSaveUsed then
      FStyle.ParaStyles[i].TempNo := i - vUnCount
    else
      Inc(vUnCount);
  end;

  for i := 0 to FSections.Count - 1 do
    FSections[i].MarkStyleUsed(False);

  for i := FStyle.TextStyles.Count - 1 downto 0 do
  begin
    if not FStyle.TextStyles[i].CheckSaveUsed then
      FStyle.TextStyles.Delete(i);
  end;

  for i := FStyle.ParaStyles.Count - 1 downto 0 do
  begin
    if not FStyle.ParaStyles[i].CheckSaveUsed then
      FStyle.ParaStyles.Delete(i);
  end;
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
  _DeleteUnUsedStyle([saHeader, saPage, saFooter]);
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

procedure THCView.SaveToPDF(const AFileName: string);
var
  i, j, vDPI: Integer;
  vPDF: TPdfDocumentGDI;
  vPage: TPdfPage;
  vPaintInfo: TSectionPaintInfo;
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

    //vPDF.UseUniscribe := True;

    vDPI := Screen.PixelsPerInch;
    vPDF.ScreenLogPixels := vDPI;

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

          vPage.PageLandscape := False;
          vPDF.DefaultPaperSize := GetPDFPaperSize(FSections[i].PaperSize);
          if vPDF.DefaultPaperSize = TPDFPaperSize.psUserDefined then
          begin  // 英寸单位下总共多少像素
            vPage.PageWidth := Round(FSections[i].PaperWidth / 25.4 * 72);
            vPage.PageHeight := Round(FSections[i].PaperHeight / 25.4 * 72);
          end;

          vPaintInfo.PageIndex := j;
          vPaintInfo.WindowWidth := FSections[i].PageWidthPix;
          vPaintInfo.WindowHeight := FSections[i].PageHeightPix;

          FSections[i].PaintPage(j, 0, 0, vPDF.VCLCanvas, vPaintInfo);
        end;
      end;
    finally
      vPaintInfo.Free;
    end;

    vPDF.SaveToFile(AFileName);
  finally
    vPDF.Free;
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
  if not AQuick then
  begin
    DoSaveStreamBefor(AStream);
    _DeleteUnUsedStyle(AAreas);  // 删除不使用的样式(可否改为把有用的存了，加载时Item的StyleNo取有用)
  end;
  FStyle.SaveToStream(AStream);
  // 节数量
  vByte := FSections.Count;
  AStream.WriteBuffer(vByte, 1);
  // 各节数据
  for i := 0 to FSections.Count - 1 do
    FSections[i].SaveToStream(AStream, AAreas);

  if not AQuick then
    DoSaveStreamAfter(AStream);
end;

procedure THCView.SaveToXml(const AFileName: string; const AEncoding: TEncoding);

  function GetEncodingName: string;
  begin
    if AEncoding = TEncoding.UTF8 then
      Result := 'UTF-8'
    else
      Result := 'Unicode';
  end;

var
  vXml: IHCXMLDocument;
  vNode: IHCXMLNode;
  i: Integer;
begin
  _DeleteUnUsedStyle([saHeader, saPage, saFooter]);

  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.Encoding := GetEncodingName;

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

  vXml.SaveToFile(AFileName);
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
  vTopData: THCRichData;
  vStartDrawItemNo, vEndDrawItemNo: Integer;
  vPt: TPoint;
  vStartDrawRect, vEndDrawRect: TRect;
begin
  Result := Self.ActiveSection.Search(AKeyword, AForward, AMatchCase);
  if Result then
  begin
    vPt := GetActiveDrawItemClientCoord;  // 返回光标处DrawItem相对当前页显示的窗体坐标，有选中时，以选中结束位置的DrawItem格式化坐标

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

procedure THCView.SetActiveSectionIndex(const Value: Integer);
begin
  if FActiveSectionIndex <> Value then
  begin
    if FActiveSectionIndex >= 0 then
      FSections[FActiveSectionIndex].DisActive;
    FActiveSectionIndex := Value;
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
    if Assigned(FOnChangedSwitch) then
      FOnChangedSwitch(Self);
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

procedure THCView.SetPageScrollModel(const Value: THCPageScrollModel);
begin
  if FViewModel = vmWeb then Exit;
  if FPageScrollModel <> Value then
    FPageScrollModel := Value;
end;

procedure THCView.SetPrintBySectionInfo(const ASectionIndex: Integer);
var
  vDevice: Array[0..(cchDeviceName - 1)] of Char;
  vDriver: Array[0..(MAX_PATH - 1)] of Char;
  vPort: Array[0..32] of Char;
  vHDMode: THandle;
  vPDMode: PDevMode;
begin
  Printer.GetPrinter(vDevice, vDriver, vPort, vHDMode);
  if vHDMode <> 0 then
  begin
    // 获取指向DeviceMode的指针
    vPDMode := GlobalLock(vHDMode);
    try
      if vPDMode <> nil then
      begin
        vPDMode^.dmPaperSize := FSections[ASectionIndex].PaperSize;
        if vPDMode^.dmPaperSize = DMPAPER_USER then  // 自定义纸张
        begin
          //vPDMode^.dmPaperSize := DMPAPER_USER;
          vPDMode^.dmPaperLength := Round(FSections[ASectionIndex].PaperHeight * 10); //纸长你可用变量获得纸张的长、宽。
          vPDMode^.dmPaperWidth := Round(FSections[ASectionIndex].PaperWidth * 10);   //纸宽
          vPDMode^.dmFields := vPDMode^.dmFields or DM_PAPERSIZE or DM_PAPERLENGTH or DM_PAPERWIDTH;
        end;

        if FSections[ASectionIndex].PageOrientation = TPageOrientation.cpoPortrait then
          vPDMode^.dmOrientation := DMORIENT_PORTRAIT
        else
          vPDMode^.dmOrientation := DMORIENT_LANDSCAPE;
      end;

      ResetDC(Printer.Handle, vPDMode^);
    finally
      GlobalUnlock(vHDMode);
    end;
    //Printer.SetPrinter(vDevice, vDriver, vPort, vHDMode);
  end;
end;

procedure THCView.SetReadOnly(const Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].ReadOnly := Value;
end;

procedure THCView.SetShowLineActiveMark(const Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].PageData.ShowLineActiveMark := Value;

  UpdateView;
end;

procedure THCView.SetShowLineNo(const Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].PageData.ShowLineNo := Value;

  UpdateView;
end;

procedure THCView.SetShowUnderLine(const Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].PageData.ShowUnderLine := Value;

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
begin
  if FPageScrollModel = psmHorizontal then Exit; // 水平滚动不能切换模式
  if FViewModel <> Value then
    FViewModel := Value;
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
    if Assigned(FOnZoomChanged) then
      FOnZoomChanged(Self);

    DoMapChanged;
    DoViewResize;
  end;
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

procedure THCView.ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode);
begin
  ActiveSection.ApplyParaLineSpace(ASpaceMode);
end;

procedure THCView.ApplyParaRightIndent(const AIndent: Single);
begin
  ActiveSection.ApplyParaRightIndent(AIndent);
end;

procedure THCView.Undo;
begin
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
    if FPageScrollModel = psmVertical then
    begin
      for i := 0 to FSections.Count - 1 do
      begin
        for j := 0 to FSections[i].PageCount - 1 do
        begin
          vPos := vPos + ZoomIn(PagePadding + FSections[i].PageHeightPix);
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
              vPos := vPos + ZoomIn(PagePadding + FSections[i].PageHeightPix)
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
        end;
        if FDisplayLastSection < 0 then  // 没有找到结束页，赋值为最后一节最后一页
        begin
          FDisplayLastSection := FSections.Count - 1;
          FSections[FDisplayLastSection].DisplayLastPageIndex := FSections[FDisplayLastSection].PageCount - 1;
        end;
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
begin
  if (FUpdateCount = 0) and HandleAllocated then
  begin
    FDataBmp.Canvas.Lock;
    try
      // 创建一个新的剪切区域，该区域是当前剪切区域和一个特定矩形的交集
      IntersectClipRect(FDataBmp.Canvas.Handle, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

      // 控件背景
      FDataBmp.Canvas.Brush.Color := Self.Color;// $00E7BE9F;
      FDataBmp.Canvas.FillRect(Rect(0, 0, FDataBmp.Width, FDataBmp.Height));
      // 因基于此计算当前页面数据起始结束，所以不能用ARect代替
      CalcDisplaySectionAndPage;  // 计算当前范围内可显示的起始节、页和结束节、页

      vPaintInfo := TSectionPaintInfo.Create;
      try
        vPaintInfo.ScaleX := FZoom;
        vPaintInfo.ScaleY := FZoom;
        vPaintInfo.Zoom := FZoom;
        vPaintInfo.WindowWidth := FViewWidth;
        vPaintInfo.WindowHeight := FViewHeight;

        vScaleInfo := vPaintInfo.ScaleCanvas(FDataBmp.Canvas);
        try
          if Assigned(FOnPaintViewBefor) then  // 本次窗口重绘开始
            FOnPaintViewBefor(FDataBmp.Canvas);

          if FAnnotatePre.DrawCount > 0 then  // 在整体绘制前清除一次，整个绘制中记录各页的批注，便于鼠标移动时判断光标处
            FAnnotatePre.ClearDrawAnnotate;     // 如果每页绘制前清除，则鼠标移动时仅能判断是否在最后绘制页的批注范围内，前面页中的都没有了

          for i := FDisplayFirstSection to FDisplayLastSection do
          begin
            vPaintInfo.SectionIndex := i;

            vOffsetY := ZoomOut(FVScrollBar.Position) - GetSectionTopFilm(i);  // 转为原始Y向偏移
            FSections[i].PaintDisplayPage(GetSectionDrawLeft(i) - ZoomOut(FHScrollBar.Position),  // 原始X向偏移
              vOffsetY, FDataBmp.Canvas, vPaintInfo);
          end;

          for i := 0 to vPaintInfo.TopItems.Count - 1 do  // 绘制顶层Item
            vPaintInfo.TopItems[i].PaintTop(FDataBmp.Canvas);

          if Assigned(FOnPaintViewAfter) then  // 本次窗口重绘结束
            FOnPaintViewAfter(FDataBmp.Canvas);
        finally
          vPaintInfo.RestoreCanvasScale(FDataBmp.Canvas, vScaleInfo);
        end;
      finally
        vPaintInfo.Free;
      end;
    finally
      FDataBmp.Canvas.Unlock;
    end;

    BitBlt(Canvas.Handle, ARect.Left, ARect.Top, ARect.Width, ARect.Height,
      FDataBmp.Canvas.Handle, ARect.Left, ARect.Top, SRCCOPY);

    InvalidateRect(Self.Handle, ARect, False);  // 只更新变动区域，防止闪烁，解决BitBlt光标滞留问题
  end;
end;

procedure THCView.UpdateView;
begin
  UpdateView(GetViewRect);
end;

procedure THCView.UpdateImmPosition;
var
  vhIMC: HIMC;
  vCF: TCompositionForm;
  vLogFont: TLogFont;
  //vIMEWnd: THandle;
  //vS: string;
  //vCandiID: Integer;
begin
  vhIMC := ImmGetContext(Handle);
  try
    // 告诉输入法当前光标处字体信息
    ImmGetCompositionFont(vhIMC, @vLogFont);
    vLogFont.lfHeight := 22;
    ImmSetCompositionFont(vhIMC, @vLogFont);
    // 告诉输入法当前光标位置信息
    vCF.ptCurrentPos := Point(FCaret.X, FCaret.Y + 5);  // 输入法弹出窗体位置
    vCF.dwStyle := CFS_RECT;
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
var
  vhIMC: HIMC;
  vSize: Integer;
  vBuffer: TBytes;
  vS: string;
begin
  if (Message.LParam and GCS_RESULTSTR) <> 0 then  // 通知检索或更新上屏字符串
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
          begin
            if DoProcessIMECandi(vS) then
              InsertText(vS);
          end;
        end;
      finally
        ImmReleaseContext(Handle, vhIMC);
      end;
      Message.Result := 0;
    end;
  end
  else
    inherited;
end;

procedure THCView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if Message.FocusedWnd <> Self.Handle then
    FCaret.Hide;
end;

procedure THCView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  //ActiveSection.DblClick(Message.XPos, Message.YPos);  // 双击也放到MouseDown中了
end;

procedure THCView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  // 光标在行最后，通过工具栏修改光标处字号后会重新设置焦点到HCView，如果不停止重取样式，
  // 则会以当前光标前文本样式做为当前样式了，如果是焦点失去后鼠标重新点击获取焦点
  // 会先触发这里，再触发MouseDown，在MouseDown里会重新取当前样式不受影响
  FStyle.UpdateInfoReCaret(False);
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReScroll;
  CheckUpdateInfo;
end;

procedure THCView.WndProc(var Message: TMessage);
var
  vPt: TPoint;
{  DC: HDC;
  PS: TPaintStruct;}
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
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
    {WM_PAINT:
      begin
        DC := BeginPaint(Handle, PS);
        try
          BitBlt(DC,
            PS.rcPaint.Left, PS.rcPaint.Top,
            PS.rcPaint.Right - PS.rcPaint.Left - FVScrollBar.Width,
            PS.rcPaint.Bottom - PS.rcPaint.Top - FHScrollBar.Height,
            FSectionData.DataBmp.Canvas.Handle,
            PS.rcPaint.Left, PS.rcPaint.Top,
            SRCCOPY);
        finally
          EndPaint(Handle, PS);
        end;
      end; }
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
end;

procedure THCAnnotatePre.MouseDown(const X, Y: Integer);
begin
  FActiveDrawAnnotateIndex := GetDrawAnnotateAt(X, Y);
end;

procedure THCAnnotatePre.MouseMove(const X, Y: Integer);
var
  vIndex: Integer;
  vPt: TPoint;
begin
  vPt := Point(X, Y);
  MouseIn := PtInRect(FDrawRect, vPt);
  vIndex := GetDrawAnnotateAt(vPt);
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
    vBottom := vTop + vSection.PageHeightPix - vHeaderAreaHeight - vSection.PageMarginBottomPix;

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

      // 计算本页各批注显示位置
      vTop := FDrawAnnotates[vFirst].DrawRect.Top;
      for i := vFirst to vLast do
      begin
        vDrawAnnotate := FDrawAnnotates[i];
        if vDrawAnnotate.DrawRect.Top > vTop then
          vTop := vDrawAnnotate.DrawRect.Top;

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

        if APaintInfo.Print then
          ACanvas.Brush.Style := bsClear;

        ACanvas.RoundRect(vDrawAnnotate.Rect, 5, 5);  // 填充批注区域

        // 绘制批注文本
        vText := vDrawAnnotate.DataAnnotate.Title + ':' + vDrawAnnotate.DataAnnotate.Text;
        vTextRect := vDrawAnnotate.Rect;
        vTextRect.Inflate(-5, -5);
        DrawTextEx(ACanvas.Handle, PChar(IntToStr(i) + vText), -1, vTextRect, DT_VCENTER or DT_LEFT or DT_WORDBREAK, nil);

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
