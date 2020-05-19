{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-8-17             }
{                                                       }
{                  文档节基类实现单元                   }
{                                                       }
{*******************************************************}

unit HCSection;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, HCViewData, HCSectionData,
  HCRichData, HCTextStyle, HCParaStyle, HCItem, HCCustomFloatItem, HCDrawItem,
  HCPage, HCRectItem, HCCommon, HCStyle, HCAnnotateData, HCCustomData, HCUndo, HCXml;

type
  TPrintResult = (prOk, prNoPrinter, prNoSupport, prError);

  TSectionPaintInfo = class(TPaintInfo)
  strict private
    FSectionIndex, FPageIndex, FPageDataFmtTop: Integer;
  public
    constructor Create; override;
    property SectionIndex: Integer read FSectionIndex write FSectionIndex;
    property PageIndex: Integer read FPageIndex write FPageIndex;
    property PageDataFmtTop: Integer read FPageDataFmtTop write FPageDataFmtTop;
    //property HeaderAreaHeight: Integer read FHeaderAreaHeight write FHeaderAreaHeight;
  end;

  TSectionPaintEvent = procedure(const Sender: TObject; const APageIndex: Integer;
    const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo) of object;
  TSectionDrawItemPaintEvent = procedure(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect;
      const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;
  TSectionDataItemEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AItem: THCCustomItem) of object;
  TSectionDataActionEvent = function(const Sender: TObject; const AData: THCCustomData;
    const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean of object;
  TSectionDataItemNoFunEvent = function(const Sender: TObject; const AData: THCCustomData;
    const AItemNo: Integer): Boolean of object;
  TSectionDrawItemAnnotateEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate) of object;
  TSectionAnnotateEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const ADataAnnotate: THCDataAnnotate) of object;
  TSectionDataItemMouseEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AItemNo, AOffset: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TSectionDataDrawItemMouseEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AItemNo, AOffset, ADrawItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  THCCustomSection = class(TObject)
  private
    FStyle: THCStyle;
    FPages: THCPages;  // 所有页面
    FPaper: THCPaper;
    FPaperOrientation: TPaperOrientation;
    FHeader: THCHeaderData;
    FFooter: THCFooterData;
    FPage: THCPageData;
    FViewModel: THCViewModel;
    FActiveData: THCSectionData;  // 页眉、正文、页脚
    FMoveData: THCSectionData;

    /// <summary> 是否对称边距 </summary>
    FSymmetryMargin: Boolean;
    FPageNoVisible: Boolean;  // 是否显示页码
    FPagePadding: Byte;
    FPageNoFrom,  // 页码从几开始
    FActivePageIndex,  // 当前激活的页
    FMousePageIndex,  // 当前鼠标所在页
    FDisplayFirstPageIndex,  // 屏显第一页
    FDisplayLastPageIndex,   // 屏显最后一页
    FHeaderOffset  // 页眉顶部偏移
      : Integer;

    FOnDataChange,  // 页眉、页脚、页面某一个修改时触发
    FOnCheckUpdateInfo,  // 当前Data需要UpdateInfo更新时触发
    FOnReadOnlySwitch,  // 页眉、页脚、页面只读状态发生变化时触发
    FOnChangeTopLevelData  // 切换页眉、页脚、正文、表格单元格时触发
      : TNotifyEvent;

    FOnGetScreenCoord: TGetScreenCoordEvent;

    FOnPaintHeaderBefor, FOnPaintHeaderAfter,
    FOnPaintFooterBefor, FOnPaintFooterAfter,
    FOnPaintPageBefor, FOnPaintPageAfter,
    FOnPaintPaperBefor, FOnPaintPaperAfter: TSectionPaintEvent;
    FOnDrawItemPaintBefor, FOnDrawItemPaintAfter: TSectionDrawItemPaintEvent;

    FOnInsertAnnotate, FOnRemoveAnnotate: TSectionAnnotateEvent;
    FOnDrawItemAnnotate: TSectionDrawItemAnnotateEvent;

    FOnDrawItemPaintContent: TDrawItemPaintContentEvent;
    FOnInsertItem, FOnRemoveItem: TSectionDataItemEvent;
    FOnSaveItem, FOnPaintDomainRegion: TSectionDataItemNoFunEvent;
    FOnDataAcceptAction: TSectionDataActionEvent;
    FOnItemMouseDown, FOnItemMouseUp: TSectionDataItemMouseEvent;
    FOnDrawItemMouseMove: TSectionDataDrawItemMouseEvent;
    FOnItemResize: TDataItemNoEvent;
    FOnCreateItem, FOnCurParaNoChange, FOnActivePageChange: TNotifyEvent;
    FOnCaretItemChanged: TSectionDataItemEvent;
    FOnCreateItemByStyle: TStyleItemEvent;
    FOnCanEdit: TOnCanEditEvent;
    FOnInsertTextBefor: TTextEvent;
    FOnGetUndoList: TGetUndoListEvent;

    /// <summary> 返回当前节指定的垂直偏移处对应的页 </summary>
    /// <param name="AVOffset">垂直偏移</param>
    /// <returns>页序号，-1表示无对应页</returns>
    function GetPageIndexByFilm(const AVOffset: Integer): Integer;

    /// <summary> 当前Data需要UpdateInfo更新 </summary>
    procedure DoActiveDataCheckUpdateInfo;
    procedure DoDataReadOnlySwitch(Sender: TObject);
    function DoGetScreenCoordEvent(const X, Y: Integer): TPoint;
    procedure DoDataDrawItemPaintBefor(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DoDataDrawItemPaintContent(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
      const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DoDataDrawItemPaintAfter(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

    procedure DoDataInsertAnnotate(const AData: THCCustomData; const ADataAnnotate: THCDataAnnotate);
    procedure DoDataRemoveAnnotate(const AData: THCCustomData; const ADataAnnotate: THCDataAnnotate);
    procedure DoDataDrawItemAnnotate(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate);

    procedure DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem);
    procedure DoDataRemoveItem(const AData: THCCustomData; const AItem: THCCustomItem);
    function DoDataSaveItem(const AData: THCCustomData; const AItemNo: Integer): Boolean;
    function DoDataPaintDomainRegion(const AData: THCCustomData; const AItemNo: Integer): Boolean;
    function DoDataAcceptAction(const AData: THCCustomData; const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
    procedure DoDataItemMouseDown(const AData: THCCustomData; const AItemNo, AOffset: Integer;
       Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoDataItemMouseUp(const AData: THCCustomData; const AItemNo, AOffset: Integer;
       Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoDataDrawItemMouseMove(const AData: THCCustomData; const AItemNo, AOffset,
       ADrawItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoDataChanged(Sender: TObject);
    procedure DoDataItemRequestFormat(const ASectionData: THCCustomData; const AItem: THCCustomItem);

    /// <summary> 缩放Item约束不要超过整页宽、高 </summary>
    procedure DoDataItemResized(const AData: THCCustomData; const AItemNo: Integer);
    function DoDataCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
    function DoDataCanEdit(const Sender: TObject): Boolean;
    function DoDataInsertTextBefor(const AData: THCCustomData; const AItemNo, AOffset: Integer;
      const AText: string): Boolean;
    procedure DoDataCreateItem(Sender: TObject);
    procedure DoDataCurParaNoChange(Sender: TObject);
    procedure DoDataCaretItemChanged(const AData: THCCustomData; const AItem: THCCustomItem);
    function DoDataGetUndoList: THCUndoList;

    /// <summary> 返回页面Data指定DrawItem所在的页(跨页的按最后位置所在页) </summary>
    /// <param name="ADrawItemNo"></param>
    /// <returns></returns>
    function GetPageIndexByPageDataDrawItem(const ADrawItemNo: Integer): Integer;

    /// <summary> 将某一页的坐标转换到页指定Data的坐标(此方法需要AX、AY在此页上的前提) </summary>
    /// <param name="APageIndex"></param>
    /// <param name="AData"></param>
    /// <param name="AX"></param>
    /// <param name="AY"></param>
    /// <param name="ARestrain">是否约束到Data的绝对区域中</param>
    procedure PaperCoordToData(const APageIndex: Integer;
      const AData: THCViewData; var AX, AY: Integer;
      const ARestrain: Boolean = True);

    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetActivePageIndex(const Value: Integer);
    function GetCurStyleNo: Integer;
    function GetCurParaNo: Integer;
  protected
    procedure KillFocus;

    // 纸张信息
    function GetPaperSize: Integer;
    procedure SetPaperSize(const Value: Integer);
    // 边距信息
    function GetPaperWidth: Single;
    function GetPaperHeight: Single;
    function GetPaperMarginTop: Single;
    function GetPaperMarginLeft: Single;
    function GetPaperMarginRight: Single;
    function GetPaperMarginBottom: Single;

    procedure SetPaperWidth(const Value: Single);
    procedure SetPaperHeight(const Value: Single);
    procedure SetPaperMarginTop(const Value: Single);
    procedure SetPaperMarginLeft(const Value: Single);
    procedure SetPaperMarginRight(const Value: Single);
    procedure SetPaperMarginBottom(const Value: Single);

    function GetPaperWidthPix: Integer;
    function GetPaperHeightPix: Integer;
    function GetPaperMarginTopPix: Integer;
    function GetPaperMarginLeftPix: Integer;
    function GetPaperMarginRightPix: Integer;
    function GetPaperMarginBottomPix: Integer;

    procedure SetHeaderOffset(const Value: Integer);
    function NewEmptyPage: THCPage;
    function GetPageCount: Integer;

    function GetSectionDataAt(const X, Y: Integer): THCSectionData;
    function GetActiveArea: TSectionArea;
    procedure SetActiveData(const Value: THCSectionData);

    /// <summary> 返回数据格式化AVertical位置在胶卷中的位置 </summary>
    /// <param name="AVertical"></param>
    /// <returns></returns>
    function GetDataFmtTopFilm(const AVertical: Integer): Integer;
    function DoSectionDataAction(const AData: THCSectionData; const AAction: THCFunction): Boolean;

    property Style: THCStyle read FStyle;
  public
    constructor Create(const AStyle: THCStyle);
    destructor Destroy; override;
    //
    /// <summary> 修改纸张边距 </summary>
    procedure ResetMargin;
    /// <summary> ActiveItem重新适应其环境(供外部直接修改Item属性后重新和其前后Item连接组合) </summary>
    procedure ActiveItemReAdaptEnvironment;
    procedure DisActive;
    function SelectExists: Boolean;
    procedure SelectAll;
    function GetHint: string;
    function GetActiveItem: THCCustomItem;
    function GetTopLevelItem: THCCustomItem;
    function GetTopLevelDrawItem: THCCustomDrawItem;
    function GetTopLevelDrawItemCoord: TPoint;

    function GetTopLevelRectDrawItemCoord: TPoint;

    /// <summary> 返回光标或选中结束位置所在页序号 </summary>
    function GetPageIndexByCurrent: Integer;

    /// <summary> 返回正文格式位置所在页序号 </summary>
    function GetPageIndexByFormat(const AVOffset: Integer): Integer;
    /// <summary> 直接设置当前TextItem的Text值 </summary>
    procedure SetActiveItemText(const AText: string);

    procedure PaintDisplayPaper(const AFilmOffsetX, AFilmOffsetY: Integer;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);

    procedure KeyPress(var Key: Char);
    procedure KeyDown(var Key: Word; Shift: TShiftState);
    procedure KeyUp(var Key: Word; Shift: TShiftState);
    //
    procedure ApplyTextStyle(const AFontStyle: THCFontStyle);
    procedure ApplyTextFontName(const AFontName: TFontName);
    procedure ApplyTextFontSize(const AFontSize: Single);
    procedure ApplyTextColor(const AColor: TColor);
    procedure ApplyTextBackColor(const AColor: TColor);

    procedure ApplyTableCellAlign(const AAlign: THCContentAlign);

    function InsertText(const AText: string): Boolean;
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
    function InsertImage(const AImage: TGraphic): Boolean;
    function InsertGifImage(const AFile: string): Boolean;
    function InsertLine(const ALineHeight: Integer): Boolean;
    function InsertItem(const AItem: THCCustomItem): Boolean; overload;
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem): Boolean; overload;

    /// <summary> 从当前位置后换行 </summary>
    function InsertBreak: Boolean;

    /// <summary> 从当前位置后分页 </summary>
    function InsertPageBreak: Boolean;

    /// <summary> 根据传入的域"模具"创建域 </summary>
    /// <param name="AMouldDomain">"模具"调用完此方法后请自行释放</param>
    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;

    /// <summary> 当前选中的内容添加批注 </summary>
    function InsertAnnotate(const ATitle, AText: string): Boolean;

    function SetActiveImage(const AImageStream: TStream): Boolean;
    //
    function ActiveTableResetRowCol(const ARowCount, AColCount: Byte): Boolean;
    function ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;
    function ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;
    function ActiveTableDeleteCurRow: Boolean;
    function ActiveTableSplitCurRow: Boolean;
    function ActiveTableSplitCurCol: Boolean;
    function ActiveTableInsertColAfter(const AColCount: Byte): Boolean;
    function ActiveTableInsertColBefor(const AColCount: Byte): Boolean;
    function ActiveTableDeleteCurCol: Boolean;
    //
    //// <summary> 节坐标转换到指定页坐标 </summary>
    procedure SectionCoordToPaper(const APageIndex, X, Y: Integer; var
      APageX, APageY: Integer);

    /// <summary> 为段应用对齐方式 </summary>
    /// <param name="AAlign">对方方式</param>
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);
    procedure ApplyParaBackColor(const AColor: TColor);
    procedure ApplyParaBreakRough(const ARough: Boolean);
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode; const ASpace: Single);
    procedure ApplyParaLeftIndent(const AIndent: Single);
    procedure ApplyParaRightIndent(const AIndent: Single);
    procedure ApplyParaFirstIndent(const AIndent: Single);

    function DataAction(const AData: THCSectionData; const AAction: THCFunction): Boolean;
    /// <summary> 获取光标在Dtat中的位置信息并映射到指定页面 </summary>
    /// <param name="APageIndex">要映射到的页序号</param>
    /// <param name="ACaretInfo">光标位置信息</param>
    procedure GetPageCaretInfo(var ACaretInfo: THCCaretInfo);

    /// <summary> 绘制指定页到指定的位置，为配合打印，开放ADisplayWidth, ADisplayHeight参数 </summary>
    /// <param name="APageIndex">要绘制的页码</param>
    /// <param name="ALeft">绘制X偏移</param>
    /// <param name="ATop">绘制Y偏移</param>
    /// <param name="ACanvas"></param>
    procedure PaintPaper(const APageIndex, ALeft, ATop: Integer;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure Clear; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    /// <summary> 某页在整个节中的Top位置 </summary>
    /// <param name="APageIndex"></param>
    /// <returns></returns>
    function GetPageTopFilm(const APageIndex: Integer): Integer;
    function GetPageTop(const APageIndex: Integer): Integer;

    /// <summary> 返回指定页数据起始位置在整个Data中的Top，注意 20161216001 </summary>
    /// <param name="APageIndex"></param>
    /// <returns></returns>
    function GetPageDataFmtTop(const APageIndex: Integer): Integer;

    /// <summary> 页眉内容在页中绘制时的起始位置 </summary>
    /// <returns></returns>
    function GetHeaderPageDrawTop: Integer;

    function GetPageMarginLeft(const APageIndex: Integer): Integer;

    /// <summary> 根据页面对称属性，获取指定页的左右边距 </summary>
    /// <param name="APageIndex"></param>
    /// <param name="AMarginLeft"></param>
    /// <param name="AMarginRight"></param>
    procedure GetPageMarginLeftAndRight(const APageIndex: Integer;
      var AMarginLeft, AMarginRight: Integer);

    /// <summary> 从正文指定Item开始重新计算页 </summary>
    /// <param name="AStartItemNo"></param>
    procedure BuildSectionPages(const AStartDrawItemNo: Integer);
    function DeleteSelected: Boolean;
    procedure DisSelect;
    function DeleteActiveDomain: Boolean;
    procedure DeleteActiveDataItems(const AStartNo, AEndNo: Integer;
      const AKeepPara: Boolean);
    function MergeTableSelectCells: Boolean;
    function TableApplyContentAlign(const AAlign: THCContentAlign): Boolean;
    procedure ReFormatActiveParagraph;
    procedure ReFormatActiveItem;
    function GetHeaderAreaHeight: Integer;
    /// <summary> 节页面正文区域高度，即页面除页眉、页脚后净高 </summary>
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    function GetFilmHeight: Cardinal;  // 所有页面高+分隔条
    function GetFilmWidth: Cardinal;

    /// <summary> 标记样式是否在用或删除不使用的样式后修正样式序号 </summary>
    /// <param name="AMark">True:标记样式是否在用，Fasle:修正原样式因删除不使用样式后的新序号</param>
    procedure MarkStyleUsed(const AMark: Boolean;
      const AParts: TSectionAreas = [saHeader, saPage, saFooter]);
    procedure SaveToStream(const AStream: TStream;
      const ASaveParts: TSectionAreas = [saHeader, saPage, saFooter]);
    function SaveToText: string;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word);
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean;
    procedure FormatData;

    /// <summary> 设置选中范围(如不需要更新界面可直接调用Data的SetSelectBound) </summary>
    procedure ActiveDataSetSelectBound(const AStartNo, AStartOffset, AEndNo, AEndOffset: Integer);

    procedure Undo(const AUndo: THCUndo);
    procedure Redo(const ARedo: THCUndo);
    // 属性
    // 页面
    property PaperSize: Integer read GetPaperSize write SetPaperSize;
    property PaperWidth: Single read GetPaperWidth write SetPaperWidth;
    property PaperHeight: Single read GetPaperHeight write SetPaperHeight;
    property PaperMarginTop: Single read GetPaperMarginTop write SetPaperMarginTop;
    property PaperMarginLeft: Single read GetPaperMarginLeft write SetPaperMarginLeft;
    property PaperMarginRight: Single read GetPaperMarginRight write SetPaperMarginRight;
    property PaperMarginBottom: Single read GetPaperMarginBottom write SetPaperMarginBottom;
    property PaperOrientation: TPaperOrientation read FPaperOrientation write FPaperOrientation;
    //
    property PaperWidthPix: Integer read GetPaperWidthPix;
    property PaperHeightPix: Integer read GetPaperHeightPix;
    property PaperMarginTopPix: Integer read GetPaperMarginTopPix;
    property PaperMarginLeftPix: Integer read GetPaperMarginLeftPix;
    property PaperMarginRightPix: Integer read GetPaperMarginRightPix;
    property PaperMarginBottomPix: Integer read GetPaperMarginBottomPix;

    property HeaderOffset: Integer read FHeaderOffset write SetHeaderOffset;
    property Header: THCHeaderData read FHeader;
    property Footer: THCFooterData read FFooter;
    property Page: THCPageData read FPage;
    property CurStyleNo: Integer read GetCurStyleNo;
    property CurParaNo: Integer read GetCurParaNo;

    /// <summary> 当前文档激活区域(页眉、页脚、页面)的数据对象 </summary>
    property ActiveData: THCSectionData read FActiveData write SetActiveData;

    /// <summary> 当前文档激活区域页眉、页脚、页面 </summary>
    property ActiveArea: TSectionArea read GetActiveArea;
    property ActivePageIndex: Integer read FActivePageIndex;
    property ViewModel: THCViewModel read FViewModel write FViewModel;
    /// <summary> 是否对称边距 </summary>
    property SymmetryMargin: Boolean read FSymmetryMargin write FSymmetryMargin;
    property DisplayFirstPageIndex: Integer read FDisplayFirstPageIndex write FDisplayFirstPageIndex;  // 屏显第一页
    property DisplayLastPageIndex: Integer read FDisplayLastPageIndex write FDisplayLastPageIndex;  // 屏显最后一页
    property PageCount: Integer read GetPageCount;
    property PageNoVisible: Boolean read FPageNoVisible write FPageNoVisible;
    property PageNoFrom: Integer read FPageNoFrom write FPageNoFrom;
    property PagePadding: Byte read FPagePadding write FPagePadding;

    /// <summary> 文档所有部分是否只读 </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnChangeTopLevelData: TNotifyEvent read FOnChangeTopLevelData write FOnChangeTopLevelData;
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;
    property OnGetScreenCoord: TGetScreenCoordEvent read FOnGetScreenCoord write FOnGetScreenCoord;
    property OnCheckUpdateInfo: TNotifyEvent read FOnCheckUpdateInfo write FOnCheckUpdateInfo;
    property OnInsertItem: TSectionDataItemEvent read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TSectionDataItemEvent read FOnRemoveItem write FOnRemoveItem;
    property OnSaveItem: TSectionDataItemNoFunEvent read FOnSaveItem write FOnSaveItem;
    property OnItemResize: TDataItemNoEvent read FOnItemResize write FOnItemResize;
    property OnItemMouseDown: TSectionDataItemMouseEvent read FOnItemMouseDown write FOnItemMouseDown;
    property OnItemMouseUp: TSectionDataItemMouseEvent read FOnItemMouseUp write FOnItemMouseUp;
    property OnDrawItemMouseMove: TSectionDataDrawItemMouseEvent read FOnDrawItemMouseMove write FOnDrawItemMouseMove;
    property OnPaintHeaderBefor: TSectionPaintEvent read FOnPaintHeaderBefor write FOnPaintHeaderBefor;
    property OnPaintHeaderAfter: TSectionPaintEvent read FOnPaintHeaderAfter write FOnPaintHeaderAfter;
    property OnPaintFooterBefor: TSectionPaintEvent read FOnPaintFooterBefor write FOnPaintFooterBefor;
    property OnPaintFooterAfter: TSectionPaintEvent read FOnPaintFooterAfter write FOnPaintFooterAfter;
    property OnPaintPageBefor: TSectionPaintEvent read FOnPaintPageBefor write FOnPaintPageBefor;
    property OnPaintPageAfter: TSectionPaintEvent read FOnPaintPageAfter write FOnPaintPageAfter;
    property OnPaintPaperBefor: TSectionPaintEvent read FOnPaintPaperBefor write FOnPaintPaperBefor;
    property OnPaintPaperAfter: TSectionPaintEvent read FOnPaintPaperAfter write FOnPaintPaperAfter;
    property OnDrawItemPaintBefor: TSectionDrawItemPaintEvent read FOnDrawItemPaintBefor write FOnDrawItemPaintBefor;
    property OnDrawItemPaintAfter: TSectionDrawItemPaintEvent read FOnDrawItemPaintAfter write FOnDrawItemPaintAfter;
    property OnDrawItemPaintContent: TDrawItemPaintContentEvent read FOnDrawItemPaintContent write FOnDrawItemPaintContent;
    property OnInsertAnnotate: TSectionAnnotateEvent read FOnInsertAnnotate write FOnInsertAnnotate;
    property OnRemoveAnnotate: TSectionAnnotateEvent read FOnRemoveAnnotate write FOnRemoveAnnotate;
    property OnDrawItemAnnotate: TSectionDrawItemAnnotateEvent read FOnDrawItemAnnotate write FOnDrawItemAnnotate;
    property OnCreateItem: TNotifyEvent read FOnCreateItem write FOnCreateItem;
    property OnDataAcceptAction: TSectionDataActionEvent read FOnDataAcceptAction write FOnDataAcceptAction;
    property OnCreateItemByStyle: TStyleItemEvent read FOnCreateItemByStyle write FOnCreateItemByStyle;
    property OnPaintDomainRegion: TSectionDataItemNoFunEvent read FOnPaintDomainRegion write FOnPaintDomainRegion;
    property OnCanEdit: TOnCanEditEvent read FOnCanEdit write FOnCanEdit;
    property OnInsertTextBefor: TTextEvent read FOnInsertTextBefor write FOnInsertTextBefor;
    property OnGetUndoList: TGetUndoListEvent read FOnGetUndoList write FOnGetUndoList;
    property OnCurParaNoChange: TNotifyEvent read FOnCurParaNoChange write FOnCurParaNoChange;
    property OnCaretItemChanged: TSectionDataItemEvent read FOnCaretItemChanged write FOnCaretItemChanged;
    property OnActivePageChange: TNotifyEvent read FOnActivePageChange write FOnActivePageChange;
  end;

  THCSection = class(THCCustomSection)
  public
    /// <summary> 当前位置开始查找指定的内容 </summary>
    /// <param name="AKeyword">要查找的关键字</param>
    /// <param name="AForward">True：向前，False：向后</param>
    /// <param name="AMatchCase">True：区分大小写，False：不区分大小写</param>
    /// <returns>True：找到</returns>
    function Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean;
    function Replace(const AText: string): Boolean;
    function ParseHtml(const AHtmlText: string): Boolean;
    function InsertFloatItem(const AFloatItem: THCCustomFloatItem): Boolean;

    function ToHtml(const APath: string): string;
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);
  end;

implementation

uses
  Math, HCHtml;

{ THCCustomSection }

procedure THCCustomSection.ActiveDataSetSelectBound(const AStartNo,
  AStartOffset, AEndNo, AEndOffset: Integer);
begin
  FActiveData.SetSelectBound(AStartNo, AStartOffset, AEndNo, AEndOffset, False);
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret;
  FStyle.UpdateInfoReScroll;

  DoActiveDataCheckUpdateInfo;
end;

function THCCustomSection.SetActiveImage(const AImageStream: TStream): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.SetActiveImage(AImageStream);
    end);
end;

function THCCustomSection.ActiveTableDeleteCurCol: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.ActiveTableDeleteCurCol;
    end);
end;

function THCCustomSection.ActiveTableDeleteCurRow: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.ActiveTableDeleteCurRow;
    end);
end;

function THCCustomSection.ActiveTableInsertColAfter(const AColCount: Byte): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableInsertColAfter(AColCount);
    end);
end;

function THCCustomSection.ActiveTableInsertColBefor(const AColCount: Byte): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableInsertColBefor(AColCount);
    end);
end;

function THCCustomSection.ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableInsertRowAfter(ARowCount);
    end);
end;

function THCCustomSection.ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableInsertRowBefor(ARowCount);
    end);
end;

function THCCustomSection.ActiveTableResetRowCol(const ARowCount,
  AColCount: Byte): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.ActiveTableResetRowCol(ARowCount, AColCount);
    end);
end;

function THCCustomSection.ActiveTableSplitCurCol: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.ActiveTableSplitCurCol;
    end);
end;

function THCCustomSection.ActiveTableSplitCurRow: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.ActiveTableSplitCurRow;
    end);
end;

procedure THCCustomSection.ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyParaAlignHorz(AAlign);
    end);
end;

procedure THCCustomSection.ApplyParaAlignVert(const AAlign: TParaAlignVert);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyParaAlignVert(AAlign);
    end);
end;

procedure THCCustomSection.ApplyParaBackColor(const AColor: TColor);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyParaBackColor(AColor);
    end);
end;

procedure THCCustomSection.ApplyParaBreakRough(const ARough: Boolean);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyParaBreakRough(ARough);
    end);
end;

procedure THCCustomSection.ApplyParaFirstIndent(const AIndent: Single);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyParaFirstIndent(AIndent);
    end);
end;

procedure THCCustomSection.ApplyParaLeftIndent(const AIndent: Single);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    var
      vContentWidth: Single;
    begin
      if AIndent < 0 then
        FActiveData.ApplyParaLeftIndent(0)
      else
      begin
        vContentWidth := FPaper.Width - FPaper.MarginLeft - FPaper.MarginRight;
        if AIndent > vContentWidth - 5 then
          FActiveData.ApplyParaLeftIndent(vContentWidth - 5)
        else
          FActiveData.ApplyParaLeftIndent(AIndent);
      end;
    end);
end;

procedure THCCustomSection.ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode;
  const ASpace: Single);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyParaLineSpace(ASpaceMode, ASpace);
    end);
end;

procedure THCCustomSection.ApplyParaRightIndent(const AIndent: Single);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyParaRightIndent(AIndent);
    end);
end;

procedure THCCustomSection.ApplyTableCellAlign(const AAlign: THCContentAlign);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyTableCellAlign(AAlign);
    end);
end;

procedure THCCustomSection.ApplyTextBackColor(const AColor: TColor);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyTextBackColor(AColor);
    end);
end;

procedure THCCustomSection.ApplyTextColor(const AColor: TColor);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyTextColor(AColor);
    end);
end;

procedure THCCustomSection.ApplyTextFontName(const AFontName: TFontName);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyTextFontName(AFontName);
    end);
end;

procedure THCCustomSection.ApplyTextFontSize(const AFontSize: Single);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyTextFontSize(AFontSize);
    end);
end;

procedure THCCustomSection.ApplyTextStyle(const AFontStyle: THCFontStyle);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ApplyTextStyle(AFontStyle);
    end);
end;

procedure THCCustomSection.Clear;
begin
  FHeader.Clear;
  FFooter.Clear;
  FPage.Clear;
  FPages.ClearEx;
  FActivePageIndex := 0;
end;

constructor THCCustomSection.Create(const AStyle: THCStyle);
var
  vWidth: Integer;

  procedure SetDataProperty(const AData: THCSectionData);
  begin
    AData.Width := vWidth;
    AData.OnInsertItem := DoDataInsertItem;
    AData.OnRemoveItem := DoDataRemoveItem;
    AData.OnSaveItem := DoDataSaveItem;
    AData.OnAcceptAction := DoDataAcceptAction;
    AData.OnItemResized := DoDataItemResized;
    AData.OnItemMouseDown := DoDataItemMouseDown;
    AData.OnItemMouseUp := DoDataItemMouseUp;
    AData.OnDrawItemMouseMove := DoDataDrawItemMouseMove;
    AData.OnItemRequestFormat := DoDataItemRequestFormat;
    AData.OnCreateItemByStyle := DoDataCreateStyleItem;
    AData.OnPaintDomainRegion := DoDataPaintDomainRegion;
    AData.OnCanEdit := DoDataCanEdit;
    AData.OnInsertTextBefor := DoDataInsertTextBefor;
    AData.OnCreateItem := DoDataCreateItem;
    AData.OnReadOnlySwitch := DoDataReadOnlySwitch;
    AData.OnGetScreenCoord := DoGetScreenCoordEvent;
    AData.OnDrawItemPaintBefor := DoDataDrawItemPaintBefor;
    AData.OnDrawItemPaintAfter := DoDataDrawItemPaintAfter;
    AData.OnDrawItemPaintContent := DoDataDrawItemPaintContent;
    AData.OnInsertAnnotate := DoDataInsertAnnotate;
    AData.OnRemoveAnnotate := DoDataRemoveAnnotate;
    AData.OnDrawItemAnnotate := DoDataDrawItemAnnotate;
    AData.OnGetUndoList := DoDataGetUndoList;
    AData.OnCurParaNoChange := DoDataCurParaNoChange;
    AData.OnCaretItemChanged := DoDataCaretItemChanged;
  end;

begin
  inherited Create;
  FStyle := AStyle;
  FActiveData := nil;
  FMoveData := nil;
  FPageNoVisible := True;
  FPageNoFrom := 1;
  FHeaderOffset := 20;
  FViewModel := hvmFilm;
  FPagePadding := 20;
  FDisplayFirstPageIndex := -1;
  FDisplayLastPageIndex := -1;

  FPaper := THCPaper.Create;
  FPaperOrientation := TPaperOrientation.cpoPortrait;
  vWidth := GetPageWidth;

  FPage := THCPageData.Create(AStyle);
  SetDataProperty(FPage);

  // FData.PageHeight := HeightPix - PageMarginBottomPix - GetHeaderAreaHeight;
  // 在ReFormatSectionData中处理了FData.PageHeight

  FHeader := THCHeaderData.Create(AStyle);
  SetDataProperty(FHeader);

  FFooter := THCFooterData.Create(AStyle);
  SetDataProperty(FFooter);

  FActiveData := FPage;
  FSymmetryMargin := True;  // 对称页边距 debug

  FPages := THCPages.Create;
  NewEmptyPage;           // 创建空白页
  FPages[0].StartDrawItemNo := 0;
  FPages[0].EndDrawItemNo := 0;
end;

function THCCustomSection.DataAction(const AData: THCSectionData;
  const AAction: THCFunction): Boolean;
begin
  Result := DoSectionDataAction(AData, AAction);
end;

procedure THCCustomSection.DeleteActiveDataItems(const AStartNo, AEndNo: Integer;
  const AKeepPara: Boolean);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.DeleteActiveDataItems(AStartNo, AEndNo, AKeepPara);
      Result := True;
    end);
end;

function THCCustomSection.DeleteActiveDomain: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.DeleteActiveDomain;
    end);
end;

function THCCustomSection.DeleteSelected: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.DeleteSelected;
    end);
end;

destructor THCCustomSection.Destroy;
begin
  FHeader.Free;
  FFooter.Free;
  FPage.Free;
  FPaper.Free;
  FPages.Free;
  inherited Destroy;
end;

procedure THCCustomSection.DisActive;
begin
  //if FActiveData <> nil then
    FActiveData.DisSelect;

  FHeader.InitializeField;
  FFooter.InitializeField;
  FPage.InitializeField;
  FActiveData := FPage;
end;

procedure THCCustomSection.DisSelect;
begin
  FActiveData.GetTopLevelData.DisSelect;
end;

procedure THCCustomSection.DoActiveDataCheckUpdateInfo;
begin
  if Assigned(FOnCheckUpdateInfo) then
    FOnCheckUpdateInfo(Self);
end;

function THCCustomSection.DoDataCanEdit(const Sender: TObject): Boolean;
begin
  if Assigned(FOnCanEdit) then
    Result := FOnCanEdit(Sender)
  else
    Result := True;
end;

procedure THCCustomSection.DoDataCaretItemChanged(const AData: THCCustomData;
  const AItem: THCCustomItem);
begin
  if Assigned(FOnCaretItemChanged) then
    FOnCaretItemChanged(Self, AData, AItem);
end;

procedure THCCustomSection.DoDataChanged(Sender: TObject);
begin
  if Assigned(FOnDataChange) then
    FOnDataChange(Sender);
end;

procedure THCCustomSection.DoDataCreateItem(Sender: TObject);
begin
  if Assigned(FOnCreateItem) then
    FOnCreateItem(Sender);
end;

function THCCustomSection.DoDataCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  if Assigned(FOnCreateItemByStyle) then
    Result := FOnCreateItemByStyle(AData, AStyleNo)
  else
    Result := nil;
end;

procedure THCCustomSection.DoDataCurParaNoChange(Sender: TObject);
begin
  if Assigned(FOnCurParaNoChange) then
    FOnCurParaNoChange(Sender);
end;

procedure THCCustomSection.DoDataInsertAnnotate(const AData: THCCustomData;
  const ADataAnnotate: THCDataAnnotate);
begin
  if Assigned(FOnInsertAnnotate) then
    FOnInsertAnnotate(Self, AData, ADataAnnotate);
end;

procedure THCCustomSection.DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnInsertItem) then
    FOnInsertItem(Self, AData, AItem);
end;

function THCCustomSection.DoDataInsertTextBefor(const AData: THCCustomData;
  const AItemNo, AOffset: Integer; const AText: string): Boolean;
begin
  if Assigned(FOnInsertTextBefor) then
    Result := FOnInsertTextBefor(AData, AItemNo, AOffset, AText)
  else
    Result := True;
end;

function THCCustomSection.DoDataAcceptAction(const AData: THCCustomData;
  const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
begin
  if Assigned(FOnDataAcceptAction) then
    Result := FOnDataAcceptAction(Self, AData, AItemNo, AOffset, AAction)
  else
    Result := True;
end;

procedure THCCustomSection.DoDataDrawItemAnnotate(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate);
begin
  if Assigned(FOnDrawItemAnnotate) then
    FOnDrawItemAnnotate(Self, AData, ADrawItemNo, ADrawRect, ADataAnnotate);
end;

procedure THCCustomSection.DoDataDrawItemMouseMove(const AData: THCCustomData;
  const AItemNo, AOffset, ADrawItemNo: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnDrawItemMouseMove) then
    FOnDrawItemMouseMove(Self, AData, AItemNo, AOffset, ADrawItemNo, Button, Shift, X, Y);
end;

procedure THCCustomSection.DoDataDrawItemPaintAfter(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintAfter) then
  begin
    FOnDrawItemPaintAfter(Self, AData, AItemNo, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.DoDataDrawItemPaintBefor(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintBefor) then
  begin
    FOnDrawItemPaintBefor(Self, AData, AItemNo, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.DoDataDrawItemPaintContent(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADrawText: string;
  const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintContent) then
    FOnDrawItemPaintContent(AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADrawText,
      ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCCustomSection.DoDataItemMouseDown(const AData: THCCustomData;
  const AItemNo, AOffset: Integer; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnItemMouseDown) then
    FOnItemMouseDown(Self, AData, AItemNo, AOffset, Button, Shift, X, Y);
end;

procedure THCCustomSection.DoDataItemMouseUp(const AData: THCCustomData;
  const AItemNo, AOffset: Integer; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnItemMouseUp) then
    FOnItemMouseUp(Self, AData, AItemNo, AOffset, Button, Shift, X, Y);
end;

procedure THCCustomSection.DoDataItemRequestFormat(const ASectionData: THCCustomData;
  const AItem: THCCustomItem);
begin
  DoSectionDataAction(ASectionData as THCSectionData, function(): Boolean
    begin
      (ASectionData as THCSectionData).ReFormatActiveItem;
      Result := True;
    end);
end;

procedure THCCustomSection.DoDataItemResized(const AData: THCCustomData; const AItemNo: Integer);
var
  vData: THCCustomData;
  vResizeItem: THCResizeRectItem;
  vWidth, vHeight: Integer;
begin
  vResizeItem := AData.Items[AItemNo] as THCResizeRectItem;
  vWidth := GetPageWidth;  // 页宽

  vData := AData.GetRootData;  // 获取是哪一部分的ResizeItem
  if vData = FHeader then
    vHeight := GetHeaderAreaHeight
  else
  if vData = FFooter then
    vHeight := FPaper.MarginBottomPix
  else
  //if vData = FPageData then
    vHeight := GetPageHeight;// - FStyle.ParaStyles[vResizeItem.ParaNo].LineSpace;

  vResizeItem.RestrainSize(vWidth, vHeight);

  if Assigned(FOnItemResize) then
    FOnItemResize(AData, AItemNo);
end;

function THCCustomSection.DoDataPaintDomainRegion(const AData: THCCustomData;
  const AItemNo: Integer): Boolean;
begin
  if Assigned(FOnPaintDomainRegion) then
    Result := FOnPaintDomainRegion(Self, AData, AItemNo)
  else
    Result := True;
end;

procedure THCCustomSection.DoDataReadOnlySwitch(Sender: TObject);
begin
  if Assigned(FOnReadOnlySwitch) then
    FOnReadOnlySwitch(Self);
end;

procedure THCCustomSection.DoDataRemoveAnnotate(const AData: THCCustomData;
  const ADataAnnotate: THCDataAnnotate);
begin
  if Assigned(FOnRemoveAnnotate) then
    FOnRemoveAnnotate(Self, AData, ADataAnnotate);
end;

procedure THCCustomSection.DoDataRemoveItem(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnRemoveItem) then
    OnRemoveItem(Self, AData, AItem);
end;

function THCCustomSection.DoDataSaveItem(const AData: THCCustomData;
  const AItemNo: Integer): Boolean;
begin
  if Assigned(FOnSaveItem) then
    Result := FOnSaveItem(Self, AData, AItemNo)
  else
    Result := True;
end;

function THCCustomSection.DoGetScreenCoordEvent(const X, Y: Integer): TPoint;
begin
  if Assigned(FOnGetScreenCoord) then
    Result := FOnGetScreenCoord(X, Y);
end;

function THCCustomSection.DoDataGetUndoList: THCUndoList;
begin
  if Assigned(FOnGetUndoList) then
    Result := FOnGetUndoList
  else
    Result := nil;
end;

procedure THCCustomSection.SetActiveData(const Value: THCSectionData);
begin
  if FActiveData <> Value then
  begin
    if FActiveData <> nil then
    begin
      FActiveData.DisSelect;
      FActiveData.DisActive;  // 旧的取消激活
    end;

    FActiveData := Value;
    FStyle.UpdateInfoReScroll;
  end;
end;

procedure THCCustomSection.SetActiveItemText(const AText: string);
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.SetActiveItemText(AText);
    end);
end;

procedure THCCustomSection.SetActivePageIndex(const Value: Integer);
begin
  if FActivePageIndex <> Value then
  begin
    FActivePageIndex := Value;
    if Assigned(FOnActivePageChange) then
      FOnActivePageChange(Self);
  end;
end;

procedure THCCustomSection.FormatData;
begin
  FActiveData.DisSelect;  // 先清选中，防止格式化后选中位置不存在
  FHeader.ReFormat;
  FFooter.ReFormat;
  FPage.ReFormat;
end;

function THCCustomSection.GetPageIndexByCurrent: Integer;
var
  i, vCaretDrawItemNo: Integer;
  vCaretInfo: THCCaretInfo;
begin
  Result := -1;
  if FActiveData <> FPage then
    Result := FActivePageIndex
  else
  begin
    if FPage.CaretDrawItemNo < 0 then
    begin
      vCaretDrawItemNo := FPage.GetDrawItemNoByOffset(FPage.SelectInfo.StartItemNo,
        FPage.SelectInfo.StartItemOffset);
    end
    else
      vCaretDrawItemNo := FPage.CaretDrawItemNo;

    for i := 0 to FPages.Count - 1 do
    begin
      if FPages[i].EndDrawItemNo >= vCaretDrawItemNo then
      begin
        if (i < FPages.Count - 1)
          and (FPages[i + 1].StartDrawItemNo = vCaretDrawItemNo)
        then  // 跨页了
        begin
          if FPage.SelectInfo.StartItemNo >= 0 then
          begin
            vCaretInfo.Y := 0;
            (FPage as THCCustomData).GetCaretInfo(FPage.SelectInfo.StartItemNo,
              FPage.SelectInfo.StartItemOffset, vCaretInfo);

            Result := GetPageIndexByFormat(vCaretInfo.Y);
          end
          else
            Result := GetPageIndexByPageDataDrawItem(vCaretDrawItemNo);
        end
        else
          Result := i;

        Break;
      end;
    end;
  end;
end;

function THCCustomSection.GetActiveArea: TSectionArea;
begin
  if FActiveData = FHeader then
    Result := TSectionArea.saHeader
  else
  if FActiveData = FFooter then
    Result := TSectionArea.saFooter
  else
    Result := TSectionArea.saPage;
end;

function THCCustomSection.GetTopLevelDrawItem: THCCustomDrawItem;
begin
  Result := FActiveData.GetTopLevelDrawItem;
end;

function THCCustomSection.GetTopLevelDrawItemCoord: TPoint;
begin
  Result := FActiveData.GetTopLevelDrawItemCoord;
end;

function THCCustomSection.GetActiveItem: THCCustomItem;
begin
  Result := FActiveData.GetActiveItem;
end;

function THCCustomSection.GetTopLevelItem: THCCustomItem;
begin
  Result := FActiveData.GetTopLevelItem;
end;

function THCCustomSection.GetTopLevelRectDrawItemCoord: TPoint;
begin
  Result := FActiveData.GetTopLevelRectDrawItemCoord;
end;

function THCCustomSection.GetPageHeight: Integer;
begin
  Result := FPaper.HeightPix - GetHeaderAreaHeight - FPaper.MarginBottomPix;
end;

function THCCustomSection.GetPageWidth: Integer;
begin
  Result := FPaper.WidthPix - FPaper.MarginLeftPix - FPaper.MarginRightPix;
end;

function THCCustomSection.GetCurParaNo: Integer;
begin
  Result := FActiveData.GetTopLevelData.CurParaNo;
end;

function THCCustomSection.GetCurStyleNo: Integer;
begin
  Result := FActiveData.GetTopLevelData.CurStyleNo;
end;

function THCCustomSection.GetReadOnly: Boolean;
begin
  Result := FHeader.ReadOnly and FFooter.ReadOnly and FPage.ReadOnly;
end;

function THCCustomSection.GetSectionDataAt(const X, Y: Integer): THCSectionData;
//var
//  vPageIndex: Integer;
begin
  Result := nil;
  //vPageIndex := GetPageIndexByFilm(Y);
  //GetPageMarginLeftAndRight(vPageIndex, vMarginLeft, vMarginRight);
  // 确定点击页面显示区域
  if X < 0 then  // 点在页左边的MinPadding区域TEditArea.eaLeftPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if X > FPaper.WidthPix then  // 点在页右边的MinPadding区域TEditArea.eaRightPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if Y < 0 then  // 点在页上边的MinPadding区域TEditArea.eaTopPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if Y > FPaper.HeightPix then  // 只有在最后一页下边的MinPadding区域点击时触发TEditArea.eaBottomPad
  begin
    Result := FActiveData;
    Exit;
  end;

  // 边距信息，先上下，再左右
  if Y >= FPaper.HeightPix - FPaper.MarginBottomPix then  // 20200101001 =号约束到边界内部，防止划选时判断不正确
    Exit(FFooter);

  // 页眉区域实际高(页眉内容高度>上边距时，取页眉内容高度)
  if Y < GetHeaderAreaHeight then  // 点击在页眉/上边距区域TEditArea.eaMarginTop
    Exit(FHeader);

  //if X > FPageSize.PageWidthPix - vMarginRight then Exit;  // 点击在页右边距区域TEditArea.eaMarginRight
  //if X < vMarginLeft then Exit;  // 点击在页左边距区域TEditArea.eaMarginLeft
  //如果要区分左、右边距不是正文，注意双击等判断ActiveData为nil
  Result := FPage;
end;

function THCCustomSection.GetDataFmtTopFilm(const AVertical: Integer): Integer;
var
  i, vTop, vPageHeight: Integer;
begin
  Result := 0;
  vTop := 0;
  vPageHeight := GetPageHeight;
  for i := 0 to FPages.Count - 1 do
  begin
    vTop := vTop + vPageHeight;
    if vTop >= AVertical then
    begin
      vTop := AVertical - (vTop - vPageHeight);
      Break;
    end
    else
      Result := Result + FPagePadding + FPaper.HeightPix;
  end;
  Result := Result + FPagePadding + GetHeaderAreaHeight + vTop;
end;

function THCCustomSection.GetFilmHeight: Cardinal;
begin
  if FViewModel = hvmFilm then
    Result := FPages.Count * (FPagePadding + FPaper.HeightPix)
  else
    Result := FPages.Count * ({FPagePadding + }GetPageHeight)
end;

function THCCustomSection.GetFilmWidth: Cardinal;
begin
  Result := FPages.Count * (FPagePadding + FPaper.WidthPix);
end;

function THCCustomSection.GetHeaderAreaHeight: Integer;
begin
  Result := FHeaderOffset + FHeader.Height;
  if Result < FPaper.MarginTopPix then
    Result := FPaper.MarginTopPix;
  //Result := Result + 20;  // debug
end;

function THCCustomSection.GetHeaderPageDrawTop: Integer;
var
  vHeaderHeight: Integer;
begin
  Result := FHeaderOffset;
  vHeaderHeight := FHeader.Height;
  if vHeaderHeight < (FPaper.MarginTopPix - FHeaderOffset) then
    Result := Result + (FPaper.MarginTopPix - FHeaderOffset - vHeaderHeight) div 2;
end;

function THCCustomSection.GetHint: string;
begin
  Result := (FActiveData.GetTopLevelData as THCRichData).GetHint;
end;

function THCCustomSection.GetPageIndexByFormat(const AVOffset: Integer): Integer;
begin
  Result := AVOffset div GetPageHeight;
end;

function THCCustomSection.GetPageIndexByPageDataDrawItem(const ADrawItemNo: Integer): Integer;
var
  i: Integer;
begin
  // 取ADrawItemNo起始位置所在页，没有考虑ADrawItemNo跨页情况，如果要考虑可参考TSection.BuildSectionPages
  Result := 0;
  if ADrawItemNo < 0 then Exit;
  Result := FPages.Count - 1;
  for i := 0 to FPages.Count - 1 do
  begin
    if FPages[i].EndDrawItemNo >= ADrawItemNo then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function THCCustomSection.GetPageIndexByFilm(const AVOffset: Integer): Integer;
var
  i, vPos: Integer;
begin
  Result := -1;
  vPos := 0;
  for i := 0 to FPages.Count - 1 do
  begin
    if FViewModel = hvmFilm then
      vPos := vPos + FPagePadding + FPaper.HeightPix
    else
      vPos := vPos + GetPageHeight;

    if vPos >= AVOffset then  // AVOffset < 0时有2种可能，1当前节第一页前面的Padding，2在上一节里
    begin
      Result := i;
      Break;
    end;
  end;

  if (Result < 0) and (AVOffset > vPos) then  // 同节最后一页下面，按下一页里面
    Result := FPages.Count - 1;
end;

procedure THCCustomSection.GetPageCaretInfo(var ACaretInfo: THCCaretInfo);
var
  vMarginLeft, vPageIndex: Integer;
begin
  if FStyle.UpdateInfo.Draging then
    vPageIndex := FMousePageIndex
  else
    vPageIndex := FActivePageIndex;

  if (FActiveData.SelectInfo.StartItemNo < 0) or (vPageIndex < 0) then
  begin
    ACaretInfo.Visible := False;
    Exit;
  end;

  ACaretInfo.PageIndex := vPageIndex;  // 鼠标点击处所在的页
  FActiveData.GetCaretInfoCur(ACaretInfo);

  if ACaretInfo.Visible then  // 光标可显示
  begin
    if FActiveData = FPage then  // 页面变动，需要判断光标处所在页
    begin
      vMarginLeft := GetPageIndexByFormat(ACaretInfo.Y);  // 借用变量vMarginLeft表示页序号
      if vPageIndex <> vMarginLeft then  // 表格第一行一部分跨页时，点击下一页同行无内容的单元格光标回到上一页
      begin
        vPageIndex := vMarginLeft;
        SetActivePageIndex(vPageIndex);
      end;
    end;

    if FViewModel = hvmFilm then
    begin
      vMarginLeft := GetPageMarginLeft(vPageIndex);
      ACaretInfo.X := ACaretInfo.X + vMarginLeft;
      ACaretInfo.Y := ACaretInfo.Y + GetPageTopFilm(vPageIndex);

      if FActiveData = FHeader then
        ACaretInfo.Y := ACaretInfo.Y + GetHeaderPageDrawTop  // 页在节中的Top位置
      else
      if FActiveData = FPage then
        ACaretInfo.Y := ACaretInfo.Y + GetHeaderAreaHeight - GetPageDataFmtTop(vPageIndex)  // - 页起始数据在Data中的位置
      else
      if FActiveData = FFooter then
        ACaretInfo.Y := ACaretInfo.Y + FPaper.HeightPix - FPaper.MarginBottomPix;
    end
    else
    begin
      if FViewModel = hvmPage then
        ACaretInfo.X := ACaretInfo.X + FPaper.MarginLeftPix;

      ACaretInfo.Y := ACaretInfo.Y + GetPageTop(vPageIndex);
      if FActiveData = FPage then
        ACaretInfo.Y := ACaretInfo.Y - GetPageDataFmtTop(vPageIndex);  // - 页起始数据在Data中的位置
    end;
  end;
end;

function THCCustomSection.GetPageCount: Integer;
begin
  Result := FPages.Count;  // 所有页面
end;

function THCCustomSection.GetPageDataFmtTop(const APageIndex: Integer): Integer;
var
  i, vPageHeight: Integer;
begin
  Result := 0;
  if APageIndex > 0 then
  begin
    vPageHeight := GetPageHeight;

    for i := 0 to APageIndex - 1 do
      Result := Result + vPageHeight;
  end;
end;

function THCCustomSection.GetPaperHeightPix: Integer;
begin
  Result := FPaper.HeightPix;
end;

function THCCustomSection.GetPaperMarginBottomPix: Integer;
begin
  Result := FPaper.MarginBottomPix;
end;

function THCCustomSection.GetPageMarginLeft(const APageIndex: Integer): Integer;
var
  vMarginRight: Integer;
begin
  GetPageMarginLeftAndRight(APageIndex, Result, vMarginRight);
end;

procedure THCCustomSection.GetPageMarginLeftAndRight(const APageIndex: Integer;
  var AMarginLeft, AMarginRight: Integer);
begin
  if FViewModel = hvmFilm then
  begin
    if FSymmetryMargin and Odd(APageIndex) then
    begin
      AMarginLeft := FPaper.MarginRightPix;
      AMarginRight := FPaper.MarginLeftPix;
    end
    else
    begin
      AMarginLeft := FPaper.MarginLeftPix;
      AMarginRight := FPaper.MarginRightPix;
    end;
  end
  else
  if FViewModel = hvmPage then
  begin
    AMarginLeft := FPaper.MarginLeftPix;
    AMarginRight := FPaper.MarginRightPix;
  end;
end;

function THCCustomSection.GetPaperMarginLeftPix: Integer;
begin
  Result := FPaper.MarginLeftPix;
end;

function THCCustomSection.GetPaperMarginRightPix: Integer;
begin
  Result := FPaper.MarginRightPix;
end;

function THCCustomSection.GetPaperMarginTopPix: Integer;
begin
  Result := FPaper.MarginTopPix;
end;

function THCCustomSection.GetPageTop(const APageIndex: Integer): Integer;
var
  i, vPageHeight: Integer;
begin
  Result := 0;
  vPageHeight := GetPageHeight;
  for i := 0 to APageIndex - 1 do
    Result := Result + vPageHeight;
end;

function THCCustomSection.GetPageTopFilm(const APageIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := FPagePadding;
  for i := 0 to APageIndex - 1 do
    Result := Result + FPaper.HeightPix + FPagePadding;  // 每一页和其上面的分隔计为一整个处理单元
end;

function THCCustomSection.GetPaperWidthPix: Integer;
begin
  Result := FPaper.WidthPix;
end;

function THCCustomSection.GetPaperHeight: Single;
begin
  Result := FPaper.Height;
end;

function THCCustomSection.GetPaperMarginBottom: Single;
begin
  Result := FPaper.MarginBottom;
end;

function THCCustomSection.GetPaperMarginLeft: Single;
begin
  Result := FPaper.MarginLeft;
end;

function THCCustomSection.GetPaperMarginRight: Single;
begin
  Result := FPaper.MarginRight;
end;

function THCCustomSection.GetPaperMarginTop: Single;
begin
  Result := FPaper.MarginTop;
end;

function THCCustomSection.GetPaperSize: Integer;
begin
  Result := FPaper.Size;
end;

function THCCustomSection.GetPaperWidth: Single;
begin
  Result := FPaper.Width;
end;

function THCCustomSection.InsertAnnotate(const ATitle, AText: string): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertAnnotate(ATitle, AText);
    end);
end;

function THCCustomSection.InsertBreak: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertBreak;
    end);
end;

function THCCustomSection.InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertDomain(AMouldDomain);
    end);
end;

function THCCustomSection.InsertGifImage(const AFile: string): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertGifImage(AFile);
    end);
end;

function THCCustomSection.InsertImage(const AImage: TGraphic): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertImage(AImage);
    end);
end;

function THCCustomSection.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertItem(AIndex, AItem);
    end);
end;

function THCCustomSection.InsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertItem(AItem);
    end);
end;

function THCCustomSection.InsertLine(const ALineHeight: Integer): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertLine(ALineHeight);
    end);
end;

function THCCustomSection.InsertPageBreak: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FPage.InsertPageBreak;
    end);
end;

function THCCustomSection.DoSectionDataAction(const AData: THCSectionData; const AAction: THCFunction): Boolean;
begin
  if not AData.CanEdit then Exit(False);
  if AData.FloatItemIndex >= 0 then Exit(False);

  Result := AAction;  // 处理变动

  if AData.FormatChange then  // 数据高度变化了
  begin
    AData.FormatChange := False;  // 防止下次表格里变动并未引起表格高度变化，本次格式化FormatChange为True的影响

    if AData = FPage then
      BuildSectionPages(AData.FormatStartDrawItemNo)
    else
      BuildSectionPages(0);
  end;

  DoDataChanged(Self);
end;

function THCCustomSection.InsertStream(const AStream: TStream; const AStyle: THCStyle;
  const AFileVersion: Word): Boolean;
var
  vResult: Boolean;
begin
  Result := False;
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertStream(AStream, AStyle, AFileVersion);
      vResult := Result;
    end);
  Result := vResult;
end;

function THCCustomSection.InsertTable(const ARowCount, AColCount: Integer): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertTable(ARowCount, AColCount);
    end);
end;

function THCCustomSection.InsertText(const AText: string): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.InsertText(AText);
    end);
end;

procedure THCCustomSection.KeyDown(var Key: Word; Shift: TShiftState);
var
  vKey: Word;
begin
  if IsKeyDownEdit(Key) and (not FActiveData.CanEdit) then Exit;

  if FActiveData.KeyDownFloatItem(Key, Shift) then  // FloatItem使用了按键
  begin
    DoActiveDataCheckUpdateInfo;
    Exit;
  end;

  if IsKeyDownWant(Key) then
  begin
    case Key of
      VK_BACK, VK_DELETE, VK_RETURN, VK_TAB:
        begin
          vKey := Key;
          DoSectionDataAction(FActiveData, function(): Boolean
            begin
              FActiveData.KeyDown(vKey, Shift);
            end);
          Key := vKey;
        end;

      VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END:
        begin
          FActiveData.KeyDown(Key, Shift);
          SetActivePageIndex(GetPageIndexByCurrent);  // 方向键可能移动到了其他页
          DoActiveDataCheckUpdateInfo;
        end;
    end;
  end;
end;

procedure THCCustomSection.KeyPress(var Key: Char);
var
  vKey: Char;
begin
  if not FActiveData.CanEdit then Exit;

  if IsKeyPressWant(Key) then
  begin
    vKey := Key;
    DoSectionDataAction(FActiveData, function(): Boolean
      begin
        FActiveData.KeyPress(vKey);
      end);
    Key := vKey;
  end
  else
    Key := #0;
end;

procedure THCCustomSection.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not FActiveData.CanEdit then Exit;
  FActiveData.KeyUp(Key, Shift);
end;

procedure THCCustomSection.KillFocus;
begin
  //if FActiveData <> nil then
    FActiveData.KillFocus;
end;

procedure THCCustomSection.LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
  const AFileVersion: Word);
var
  vDataSize: Int64;
  vArea: Boolean;
  vLoadParts: TSectionAreas;
begin
  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));

  AStream.ReadBuffer(FSymmetryMargin, SizeOf(FSymmetryMargin));  // 是否对称页边距

  if AFileVersion > 11 then
  begin
    AStream.ReadBuffer(FPaperOrientation, SizeOf(FPaperOrientation));  // 纸张方向
    AStream.ReadBuffer(FPageNoVisible, SizeOf(FPageNoVisible));  // 是否显示页码
  end;

  FPaper.LoadToStream(AStream, AFileVersion);  // 页面参数
  FPage.Width := GetPageWidth;

  // 文档都有哪些部件的数据
  vLoadParts := [];
  AStream.ReadBuffer(vArea, SizeOf(vArea));
  if vArea then
    vLoadParts := vLoadParts + [saHeader];

  AStream.ReadBuffer(vArea, SizeOf(vArea));
  if vArea then
    vLoadParts := vLoadParts + [saFooter];

  AStream.ReadBuffer(vArea, SizeOf(vArea));
  if vArea then
    vLoadParts := vLoadParts + [saPage];

  if saHeader in vLoadParts then
  begin
    AStream.ReadBuffer(FHeaderOffset, SizeOf(FHeaderOffset));
    FHeader.Width := FPage.Width;
    FHeader.LoadFromStream(AStream, FStyle, AFileVersion);
  end;

  if saFooter in vLoadParts then
  begin
    FFooter.Width := FPage.Width;
    FFooter.LoadFromStream(AStream, FStyle, AFileVersion);
  end;

  if saPage in vLoadParts then
    FPage.LoadFromStream(AStream, FStyle, AFileVersion);
  // 20190906001 FloatItem不能通过GetPageIndexByFormat(FPage.FloatItems[0].Top)来计算FloatItem
  // 的页序号，因为正文的可能拖到页眉页脚处，按Top算GetPageIndexByFormat并不在当前页中

  BuildSectionPages(0);
end;

procedure THCCustomSection.MarkStyleUsed(const AMark: Boolean;
  const AParts: TSectionAreas = [saHeader, saPage, saFooter]);
begin
  if saHeader in AParts then
    FHeader.MarkStyleUsed(AMark);

  if saFooter in AParts then
    FFooter.MarkStyleUsed(AMark);

  if saPage in AParts then
    FPage.MarkStyleUsed(AMark);
end;

function THCCustomSection.MergeTableSelectCells: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.MergeTableSelectCells;
    end);
end;

procedure THCCustomSection.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vOldTopData: THCCustomData;
  vX, vY, vX2, vY2, vPageIndex: Integer;
  vNewActiveData: THCSectionData;
  vChangeActiveData: Boolean;
begin
  vChangeActiveData := False;
  vOldTopData := FActiveData.GetTopLevelData;
  vPageIndex := GetPageIndexByFilm(Y);  // 鼠标点击处所在的页(和光标所在页可能并不是同一页，如表格跨页时，空单元格第二页点击时，光标回前一页)
  if FActivePageIndex <> vPageIndex then
    SetActivePageIndex(vPageIndex);

  SectionCoordToPaper(FActivePageIndex, X, Y, vX, vY);  // X，Y转换到指定页的坐标vX,vY
  vNewActiveData := GetSectionDataAt(vX, vY);

  if (vNewActiveData <> FActiveData) and (ssDouble in Shift) then  // 双击、新的Data
  begin
    SetActiveData(vNewActiveData);
    vChangeActiveData := True;
  end;

  {$REGION ' 有FloatItem时短路 '}
  if FActiveData.FloatItems.Count > 0 then  // 有FloatItem时优先
  begin
    vX2 := vX;  // 使用另外的变量，防止FloatItem不处理时影响下面的正常计算
    vY2 := vY;
    PaperCoordToData(FActivePageIndex, FActiveData, vX2, vY2, False);  // 浮动Item不受约束可能在外面
    if FActiveData = FPage then  // FloatItem在PageData中
      vY2 := vY2 + GetPageDataFmtTop(FActivePageIndex);

    if FActiveData.MouseDownFloatItem(Button, Shift, vX2, vY2) then Exit;
  end;
  {$ENDREGION}

  PaperCoordToData(FActivePageIndex, FActiveData, vX, vY);

  if FActiveData = FPage then
    vY := vY + GetPageDataFmtTop(FActivePageIndex);

  if (ssDouble in Shift) and (not vChangeActiveData) then  // 在同一Data上双击
    FActiveData.DblClick(vX, vY)
  else
    FActiveData.MouseDown(Button, Shift, vX, vY);

  if vOldTopData <> FActiveData.GetTopLevelData then
  begin
    if Assigned(FOnChangeTopLevelData) then
      FOnChangeTopLevelData(Self);
  end;
end;

procedure THCCustomSection.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vX, vY, vMarginLeft, vMarginRight: Integer;
  vMoveData: THCSectionData;
begin
  GetPageMarginLeftAndRight(FMousePageIndex, vMarginLeft, vMarginRight);

  if X < vMarginLeft then
    GCursor := crDefault  { to do: 向右上角箭头 }
  else
  if X > FPaper.WidthPix - vMarginRight then
    GCursor := crDefault
  else
    GCursor := crIBeam;

  FMousePageIndex := GetPageIndexByFilm(Y);

  Assert(FMousePageIndex >= 0, '不应该出现鼠标移动到空页面上的情况！');
  //if FMousePageIndex < 0 then Exit;  应该永远不会出现

  {$REGION ' 有FloatItem时短路 '}
  if FActiveData.FloatItems.Count > 0 then  // 有FloatItem时优先
  begin
    if (Shift = [ssLeft]) and (FActiveData.FloatItemIndex >= 0) then  // 拖拽移动FloatItem
    begin
      if not FActiveData.ActiveFloatItem.Resizing then  // 非缩放
        FActiveData.ActiveFloatItem.PageIndex := FMousePageIndex;  // 记录鼠标页为FloatItem所在见
    end;

    if FActiveData = FPage then  // FloatItem在PageData中
    begin
      if (FActiveData.FloatItemIndex >= 0) and (FActiveData.ActiveFloatItem.Resizing) then  // 缩放时以所在页为标准
      begin
        SectionCoordToPaper(FActiveData.ActiveFloatItem.PageIndex, X, Y, vX, vY);
        PaperCoordToData(FActiveData.ActiveFloatItem.PageIndex, FActiveData, vX, vY, False);  // 浮动Item不受约束可能在外面
        vY := vY + GetPageDataFmtTop(FActiveData.ActiveFloatItem.PageIndex);
      end
      else  // 以鼠标所在页为准
      begin
        SectionCoordToPaper(FMousePageIndex, X, Y, vX, vY);
        PaperCoordToData(FMousePageIndex, FActiveData, vX, vY, False);  // 浮动Item不受约束可能在外面
        vY := vY + GetPageDataFmtTop(FMousePageIndex);
      end;
    end
    else  // FloatItem在Header或Footer
    begin
      if (FActiveData.FloatItemIndex >= 0) and (FActiveData.ActiveFloatItem.Resizing) then  // 缩放时以所在页为标准
      begin
        SectionCoordToPaper(FActivePageIndex, X, Y, vX, vY);
        PaperCoordToData(FActivePageIndex, FActiveData, vX, vY, False);  // 浮动Item不受约束可能在外面
      end
      else  // 以鼠标所在页为准
      begin
        SectionCoordToPaper(FMousePageIndex, X, Y, vX, vY);
        PaperCoordToData(FMousePageIndex, FActiveData, vX, vY, False);  // 浮动Item不受约束可能在外面
      end;
    end;

    if FActiveData.MouseMoveFloatItem(Shift, vX, vY) then Exit;
  end;
  {$ENDREGION}

  SectionCoordToPaper(FMousePageIndex, X, Y, vX, vY);

  vMoveData := GetSectionDataAt(vX, vY);
  if vMoveData <> FMoveData then
  begin
    if FMoveData <> nil then
      FMoveData.MouseLeave;

    FMoveData := vMoveData;
  end;

  PaperCoordToData(FMousePageIndex, FActiveData, vX, vY, Shift <> []);  // 普通移出正文不Hot任何Item

  if FActiveData = FPage then
    vY := vY + GetPageDataFmtTop(FMousePageIndex);

  FActiveData.MouseMove(Shift, vX, vY);
end;

procedure THCCustomSection.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vX, vY, vPageIndex: Integer;
begin
  vPageIndex := GetPageIndexByFilm(Y);

  {$REGION ' 有FloatItem时短路 '}
  if (FActiveData.FloatItems.Count > 0) and (FActiveData.FloatItemIndex >= 0) then  // 有FloatItem时优先
  begin
    if FActiveData = FPage then  // FloatItem在PageData中
    begin
      SectionCoordToPaper(FActiveData.ActiveFloatItem.PageIndex, X, Y, vX, vY);
      PaperCoordToData(FActiveData.ActiveFloatItem.PageIndex, FActiveData, vX, vY, False);  // 浮动Item不受约束可能在外面
      vY := vY + GetPageDataFmtTop(FActiveData.ActiveFloatItem.PageIndex);
    end
    else  // FloatItem在Header或Footer
    begin
      SectionCoordToPaper(vPageIndex, X, Y, vX, vY);
      PaperCoordToData(vPageIndex, FActiveData, vX, vY, False);  // 浮动Item不受约束可能在外面
    end;

    if FActiveData.MouseUpFloatItem(Button, Shift, vX, vY) then Exit;
  end;
  {$ENDREGION}

  SectionCoordToPaper(vPageIndex, X, Y, vX, vY);
  PaperCoordToData(vPageIndex, FActiveData, vX, vY);

  if FActiveData = FPage then
    vY := vY + GetPageDataFmtTop(vPageIndex);

  // RectItem的缩放在MouseUp中处理，所以需要判断是否需要改变
  if FActiveData.SelectedResizing then
  begin
    DoSectionDataAction(FActiveData, function(): Boolean
      begin
        FActiveData.MouseUp(Button, Shift, vX, vY);
      end);
  end
  else
    FActiveData.MouseUp(Button, Shift, vX, vY);
end;

function THCCustomSection.NewEmptyPage: THCPage;
begin
  Result := THCPage.Create;
  FPages.Add(Result);
end;

procedure THCCustomSection.PaperCoordToData(const APageIndex: Integer;
  const AData: THCViewData; var AX, AY: Integer; const ARestrain: Boolean = True);
var
  viTemp, vMarginLeft: Integer;
begin
  if FViewModel = hvmEdit then Exit;  // hvmTrim

  vMarginLeft := GetPageMarginLeft(APageIndex);
  AX := AX - vMarginLeft;

  if FViewModel = hvmPage then Exit;

  { 水平方向不约束，否则点击在表格右侧认为是要拖动右边框
  if ARestrain then  // 为避免左右边界，不往里约束1，否则无法点到行首光标，尤其是行首是RectItem
  begin
    if AX < 0 then
      AX := 0
    else
    begin
      viTemp := GetPageWidth;
      if AX > viTemp then
        AX := viTemp;
    end;
  end;}

  // 为避免边界(激活正文，在页眉页脚点击时判断仍是在正文位置造成光标错误)约束后都偏移1
  if AData = FHeader then
  begin
    AY := AY - GetHeaderPageDrawTop;  // 相对页眉绘制位置
    if ARestrain then  // 约束到页眉绝对区域中
    begin
      if AY < 0 then  // FHeaderOffset
        AY := 1
      else
      begin
        viTemp := FHeader.Height;
        if AY > viTemp then
          AY := viTemp - 1;
      end;
    end;
  end
  else
  if AData = FFooter then  // 约束到页脚绝对区域中
  begin
    AY := AY - FPaper.HeightPix + FPaper.MarginBottomPix;
    if ARestrain then
    begin
      if AY < 0 then
        AY := 1
      else
      if AY > FPaper.MarginBottomPix then
        AY := FPaper.MarginBottomPix - 1;
    end;
  end
  else
  if AData = FPage then  // 约束到正文绝对区域中
  begin
    //viTemp := GetHeaderAreaHeight;
    AY := AY - GetHeaderAreaHeight;
    if ARestrain then  // 为避免上一页脚下一页眉边界不确定是上还是下，约束后都偏移1
    begin
      if AY < 0 then
        AY := 1  // 处理激活正文，在页眉页脚中点击
      else
      begin
        viTemp := GetPageHeight;
        if AY >= viTemp then  // 20200101001 =号约束到边界内部，防止划选时判断不正确
          AY := viTemp - 1;
      end;
    end;
  end;
end;

procedure THCCustomSection.PaintDisplayPaper(const AFilmOffsetX, AFilmOffsetY: Integer;
  const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  i, vPaperDrawTop, vPaperFilmTop: Integer;
begin
  for i := FDisplayFirstPageIndex to FDisplayLastPageIndex do
  begin
    APaintInfo.PageIndex := i;
    if APaintInfo.ViewModel = hvmFilm then
      vPaperFilmTop := GetPageTopFilm(i)
    else
      vPaperFilmTop := GetPageTop(i);

    vPaperDrawTop := vPaperFilmTop - AFilmOffsetY;  // 映射到当前页面为原点的屏显起始位置(可为负数)
    PaintPaper(i, AFilmOffsetX, vPaperDrawTop, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.PaintPaper(const APageIndex, ALeft, ATop: Integer;
  const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  vHeaderAreaHeight, vMarginLeft, vMarginRight,
  vPaperDrawLeft, vPaperDrawTop, vPaperDrawRight, vPaperDrawBottom,
  vPageDrawLeft, vPageDrawTop, vPageDrawRight, vPageDrawBottom,  // 页区域各位置
  vPageDataScreenTop, vPageDataScreenBottom,  // 页数据屏幕位置
  vScaleWidth, vScaleHeight: Integer;

  {$REGION ' 绘制页眉数据 '}
  procedure PaintHeader;
  var
    vHeaderDataDrawTop, vScreenBottom: Integer;
    vDCState: THCCanvas;
  begin
    // 计算ScreenBottom时，如果缩放变小的情况，APaintInfo.WindowHeight比vPageDrawTop还小，
    // 传入PaintData计算显示不出来Item，所以不能用Min(vPageDrawTop, APaintInfo.WindowHeight),
    if vPaperDrawTop > 0 then
      vScreenBottom := vPaperDrawTop + vHeaderAreaHeight
    else
      vScreenBottom := vHeaderAreaHeight + vPaperDrawTop;

    vHeaderDataDrawTop := vPaperDrawTop + GetHeaderPageDrawTop;

    if Assigned(FOnPaintHeaderBefor) then
    begin
      vDCState := SaveCanvas(ACanvas);
      try
        FOnPaintHeaderBefor(Self, APageIndex, Rect(vPageDrawLeft, vHeaderDataDrawTop,
          vPageDrawRight, vPageDrawTop), ACanvas, APaintInfo);
      finally
        vDCState.ToCanvas(ACanvas);
        FreeAndNil(vDCState);
      end;
    end;

    FHeader.PaintData(vPageDrawLeft, vHeaderDataDrawTop, vPageDrawRight,
      vPageDrawTop, Max(vHeaderDataDrawTop, 0),
      vScreenBottom, 0, ACanvas, APaintInfo);

    if Assigned(FOnPaintHeaderAfter) then
    begin
      vDCState := SaveCanvas(ACanvas);
      try
        FOnPaintHeaderAfter(Self, APageIndex, Rect(vPageDrawLeft, vHeaderDataDrawTop,
          vPageDrawRight, vPageDrawTop), ACanvas, APaintInfo);
      finally
        vDCState.ToCanvas(ACanvas);
        FreeAndNil(vDCState);
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' 绘制页脚数据 '}
  procedure PaintFooter;
  var
    vDCState: THCCanvas;
    vScreenBottom: Integer;
  begin
    // 计算ScreenBottom时，如果缩放变小的情况，APaintInfo.WindowHeight比vPageDrawBottom还小，
    // 传入PaintData计算显示不出来Item，所以不能用Min(vPaperDrawBottom, APaintInfo.WindowHeight),
    vScreenBottom := APaintInfo.WindowHeight - APaintInfo.GetScaleY(vPageDrawBottom);
    if vScreenBottom > FPaper.MarginBottomPix then
      vScreenBottom := vPaperDrawBottom
    else
      vScreenBottom := APaintInfo.WindowHeight;

    if Assigned(FOnPaintFooterBefor) then
    begin
      vDCState := SaveCanvas(ACanvas);
      try
        FOnPaintFooterBefor(Self, APageIndex, Rect(vPageDrawLeft, vPageDrawBottom,
          vPageDrawRight, vPaperDrawBottom), ACanvas, APaintInfo);
      finally
        vDCState.ToCanvas(ACanvas);
        FreeAndNil(vDCState);
      end;
    end;

    FFooter.PaintData(vPageDrawLeft, vPageDrawBottom, vPageDrawRight, vPaperDrawBottom,
      Max(vPageDrawBottom, 0),
      vPageDrawBottom + vScreenBottom,
      0, ACanvas, APaintInfo);

    if Assigned(FOnPaintFooterAfter) then
    begin
      vDCState := SaveCanvas(ACanvas);
      try
        FOnPaintFooterAfter(Self, APageIndex, Rect(vPageDrawLeft, vPageDrawBottom,
          vPageDrawRight, vPaperDrawBottom), ACanvas, APaintInfo);
      finally
        vDCState.ToCanvas(ACanvas);
        FreeAndNil(vDCState);
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' 绘制页面数据 '}
  procedure PaintPage;
  var
    vDCState: THCCanvas;
  begin
    if (FPages[APageIndex].StartDrawItemNo < 0) or (FPages[APageIndex].EndDrawItemNo < 0) then
      Exit;

//    FPageData.CheckAnnotate(vPageDrawLeft + vMarginLeft,
//      vPageDrawTop + vHeaderAreaHeight - APaintInfo.PageDataFmtTop,
//      FPages[APageIndex].StartDrawItemNo, FPages[APageIndex].EndDrawItemNo,
//      APaintInfo.PageDataFmtTop,
//      APaintInfo.PageDataFmtTop + HeightPix - vHeaderAreaHeight - PageMarginBottomPix);
    if Assigned(FOnPaintPageBefor) then
    begin
      vDCState := SaveCanvas(ACanvas);
      try
        FOnPaintPageBefor(Self, APageIndex, Rect(vPageDrawLeft, vPageDrawTop,
          vPageDrawRight, vPageDrawBottom), ACanvas, APaintInfo);
      finally
        vDCState.ToCanvas(ACanvas);
        FreeAndNil(vDCState);
      end;
    end;

    { 绘制数据，把Data中指定位置的数据，绘制到指定的页区域中，并按照可显示出来的区域约束 }
    FPage.PaintData(vPageDrawLeft,  // 当前页数据要绘制到的Left
      vPageDrawTop,     // 当前页数据要绘制到的Top
      vPageDrawRight,   // 当前页数据要绘制到的Right
      vPageDrawBottom,  // 当前页数据要绘制的Bottom
      vPageDataScreenTop,     // 界面呈现当前页数据的Top位置
      vPageDataScreenBottom,  // 界面呈现当前页数据Bottom位置
      APaintInfo.PageDataFmtTop,  // 指定从哪个位置开始的数据绘制到页数据起始位置
      FPages[APageIndex].StartDrawItemNo,
      FPages[APageIndex].EndDrawItemNo,
      ACanvas,
      APaintInfo);

    if Assigned(FOnPaintPageAfter) then
    begin
      vDCState := SaveCanvas(ACanvas);
      try
        FOnPaintPageAfter(Self, APageIndex, Rect(vPageDrawLeft, vPageDrawTop,
          vPageDrawRight, vPageDrawBottom), ACanvas, APaintInfo);
      finally
        vDCState.ToCanvas(ACanvas);
        FreeAndNil(vDCState);
      end;
    end;
  end;
  {$ENDREGION}

var
  vDCState: THCCanvas;
  vPaintRegion: HRGN;
  vClipBoxRect: TRect;
begin
  vScaleWidth := Round(APaintInfo.WindowWidth / APaintInfo.ScaleX);
  vScaleHeight := Round(APaintInfo.WindowHeight / APaintInfo.ScaleY);

  vPaperDrawLeft := ALeft;
  vPaperDrawTop := ATop;

  if APaintInfo.ViewModel = hvmFilm then
  begin
    vPaperDrawRight := vPaperDrawLeft + FPaper.WidthPix;
    vPaperDrawBottom := vPaperDrawTop + FPaper.HeightPix;

    GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);  // 获取页左右边距绘制位置
    vHeaderAreaHeight := GetHeaderAreaHeight;  // 页眉区域实际高 = 页眉数据顶部偏移 + 内容高度，大于上边距时以此为准
    vPageDrawTop := vPaperDrawTop + vHeaderAreaHeight;  // 映射到当前页面左上角为原点的起始位置(可为负数)
    vPageDrawBottom := vPaperDrawBottom - FPaper.MarginBottomPix;  // 页面结束位置(可为负数)
  end
  else
  begin
    if APaintInfo.ViewModel = hvmPage then
    begin
      vPaperDrawRight := vPaperDrawLeft + FPaper.WidthPix;
      vPaperDrawBottom := vPaperDrawTop + GetPageHeight;
      //GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);  // 获取页左右边距绘制位置
      vMarginLeft := FPaper.MarginLeftPix;
      vMarginRight := FPaper.MarginRightPix;
    end
    else
    begin
      vPaperDrawRight := vPaperDrawLeft + GetPageWidth;
      vPaperDrawBottom := vPaperDrawTop + GetPageHeight;

      vMarginLeft := 0;
      vMarginRight := 0;
    end;

    vHeaderAreaHeight := 0;
    vPageDrawTop := vPaperDrawTop;  // 映射到当前页面左上角为原点的起始位置(可为负数)
    vPageDrawBottom := vPaperDrawBottom - 1;  // 页面结束位置(可为负数)
  end;

  vPageDrawLeft := vPaperDrawLeft + vMarginLeft;
  vPageDrawRight := vPaperDrawRight - vMarginRight;

  // 当前页数据能显示出来的区域边界
  vPageDataScreenTop := Max(vPageDrawTop, 0);
  vPageDataScreenBottom := Min(vPageDrawBottom, vScaleHeight);
  { 当前页在当前屏显出来的数据边界映射到格式化中的边界 }
  APaintInfo.PageDataFmtTop := GetPageDataFmtTop(APageIndex);
  GetClipBox(ACanvas.Handle, vClipBoxRect);  // 保存当前的绘图区域

  if not APaintInfo.Print then  // 非打印时绘制的内容
  begin

    {$REGION ' 非打印时填充纸张背景 '}
    ACanvas.Brush.Color := FStyle.BackgroundColor;
    ACanvas.FillRect(Rect(vPaperDrawLeft, vPaperDrawTop,
      Min(vPaperDrawRight, vScaleWidth),  // 约束边界
      Min(vPaperDrawBottom, vScaleHeight)));
    {$ENDREGION}

    if APaintInfo.ViewModel = hvmFilm then
    begin
      {$REGION ' 页眉边距指示符 '}
      if vPageDrawTop > 0 then  // 页眉可显示
      begin
        if vHeaderAreaHeight > FPaper.MarginTopPix then  // 页眉数据超出页上边距
        begin
          ACanvas.Pen.Style := TPenStyle.psDot;
          ACanvas.Pen.Color := clGray;
          APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawLeft, vPageDrawTop - 1),
            Point(vPageDrawRight, vPageDrawTop - 1)]);
        end;

        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := TPenStyle.psSolid;
        if FActiveData = FHeader then  // 当前激活的是页眉，绘制页眉激活线
        begin
          ACanvas.Pen.Color := clBlue;
          ACanvas.MoveTo(vPageDrawLeft, vPageDrawTop);
          ACanvas.LineTo(vPageDrawRight, vPageDrawTop);

          // 正在编辑页眉提示
          ACanvas.Brush.Color := $00F5E8D8;
          ACanvas.FillRect(Rect(vPageDrawLeft - 40, vPageDrawTop, vPageDrawLeft, vPageDrawTop + 20));
          ACanvas.Font.Size := 10;
          ACanvas.Font.Name := '宋体';
          ACanvas.Font.Style := [];
          ACanvas.Font.Color := $008B4215;
          ACanvas.TextOut(vPageDrawLeft - 32, vPageDrawTop + 4, '页眉');
        end
        else
          ACanvas.Pen.Color := clGray;

        // 左上， 左-原-上
        APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawLeft - PMSLineHeight, vPageDrawTop),
          Point(vPageDrawLeft, vPageDrawTop), Point(vPageDrawLeft, vPageDrawTop - PMSLineHeight)]);
        // 右上，右-原-上
        APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawRight + PMSLineHeight, vPageDrawTop),
          Point(vPageDrawRight, vPageDrawTop), Point(vPageDrawRight, vPageDrawTop - PMSLineHeight)]);
      end;
      {$ENDREGION}

      {$REGION ' 页脚边距指示符 '}
      if APaintInfo.GetScaleY(vPageDrawBottom) < APaintInfo.WindowHeight then  // 页脚可显示
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := TPenStyle.psSolid;
        if FActiveData = FFooter then  // 当前激活的是页脚，绘制激活线
        begin
          ACanvas.Pen.Color := clBlue;
          ACanvas.MoveTo(vPageDrawLeft, vPageDrawBottom);
          ACanvas.LineTo(vPageDrawRight, vPageDrawBottom);

          // 正在编辑页脚提示
          ACanvas.Brush.Color := $00F5E8D8;
          ACanvas.FillRect(Rect(vPageDrawLeft - 40, vPageDrawBottom, vPageDrawLeft, vPageDrawBottom - 20));
          ACanvas.Font.Size := 10;
          ACanvas.Font.Name := '宋体';
          ACanvas.Font.Style := [];
          ACanvas.Font.Color := $008B4215;
          ACanvas.TextOut(vPageDrawLeft - 32, vPageDrawBottom - 16, '页脚');
        end
        else
          ACanvas.Pen.Color := clGray;

        // 左下，左-原-下
        APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawLeft - PMSLineHeight, vPageDrawBottom),
          Point(vPageDrawLeft, vPageDrawBottom), Point(vPageDrawLeft, vPageDrawBottom + PMSLineHeight)]);
        // 右下，右-原-下
        APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawRight + PMSLineHeight, vPageDrawBottom),
          Point(vPageDrawRight, vPageDrawBottom), Point(vPageDrawRight, vPageDrawBottom + PMSLineHeight)]);
      end;
      {$ENDREGION}
    end
    else
    begin
      if vPageDrawTop > 0 then  // 页眉可显示
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := TPenStyle.psSolid;  // TPenStyle.psDashDot;
        ACanvas.Pen.Color := clGray;

        ACanvas.MoveTo(vPaperDrawLeft, vPageDrawTop);
        ACanvas.LineTo(vPaperDrawRight, vPageDrawTop);
      end;

      if APaintInfo.GetScaleY(vPageDrawBottom) < APaintInfo.WindowHeight then  // 页脚结束可显示
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := TPenStyle.psSolid;  // TPenStyle.psDashDot;
        ACanvas.Pen.Color := clGray;

        ACanvas.MoveTo(vPaperDrawLeft, vPageDrawBottom);
        ACanvas.LineTo(vPaperDrawRight, vPageDrawBottom);
      end;
    end;
  end;

  if Assigned(FOnPaintPaperBefor) then  // 公开页面绘制前事件
  begin
    vDCState := SaveCanvas(ACanvas);
    try
      FOnPaintPaperBefor(Self, APageIndex,
        Rect(vPaperDrawLeft, vPaperDrawTop, vPaperDrawRight, vPaperDrawBottom),
        ACanvas, APaintInfo);
    finally
      vDCState.ToCanvas(ACanvas);
      FreeAndNil(vDCState);
    end;
  end;

  if APaintInfo.ViewModel = hvmFilm then
  begin
    {$REGION ' 绘制页眉 '}
    if vPageDrawTop > 0 then  // 页眉可显示
    begin
      vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPaperDrawLeft),
        Max(APaintInfo.GetScaleY(vPaperDrawTop + FHeaderOffset), 0),
        APaintInfo.GetScaleX(vPaperDrawRight),  // 表格有时候会拖宽到页面外面vPageDrawRight
        Min(APaintInfo.GetScaleY(vPageDrawTop), APaintInfo.WindowHeight));

      try
        //ACanvas.Brush.Color := clYellow;
        //FillRgn(ACanvas.Handle, vPaintRegion, ACanvas.Brush.Handle);
        SelectClipRgn(ACanvas.Handle, vPaintRegion);  // 设置绘制有效区域
        PaintHeader;
      finally
        DeleteObject(vPaintRegion);
      end;

      {ACanvas.Brush.Color := clInfoBk;
      vRect := Rect(vPageDrawLeft, Max(vPageDrawTop + FHeaderOffset, 0),
      vPageDrawRight,
      Min(vPageDrawTop + vHeaderAreaHeight, vScaleHeight));
      ACanvas.FillRect(vRect);}

    end;
    {$ENDREGION}

    {$REGION ' 绘制页脚 '}
    if APaintInfo.GetScaleY(vPageDrawBottom) < APaintInfo.WindowHeight then  // 页脚可显示
    begin
      vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPaperDrawLeft),
        Max(APaintInfo.GetScaleY(vPageDrawBottom), 0),
        APaintInfo.GetScaleX(vPaperDrawRight),  // 表格有时候会拖宽到页面外面vPageDrawRight
        Min(APaintInfo.GetScaleY(vPaperDrawBottom), APaintInfo.WindowHeight));

      try
        //ACanvas.Brush.Color := clRed;
        //FillRgn(ACanvas.Handle, vPaintRegion, ACanvas.Brush.Handle);
        SelectClipRgn(ACanvas.Handle, vPaintRegion);  // 设置绘制有效区域
        PaintFooter;
      finally
        DeleteObject(vPaintRegion);
      end;

      {ACanvas.Brush.Color := clYellow;
      vRect := Rect(vPageDrawLeft,
        Max(vPageDrawBottom - FPageSize.PageMarginBottomPix, 0),
        vPageDrawRight,
        Min(vPageDrawBottom, vScaleHeight));
      ACanvas.FillRect(vRect);}
    end;
    {$ENDREGION}
  end;

  {$REGION ' 绘制页面 '}
  if vPageDataScreenBottom > vPageDataScreenTop then  // 能露出数据则绘制当前页，绘制正文
  begin
    vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPaperDrawLeft),  // 有行号或行指示符所以从纸张左边
      APaintInfo.GetScaleY(Max(vPageDrawTop, vPageDataScreenTop)),
      APaintInfo.GetScaleX(vPaperDrawRight),  // 表格有时候会拖宽到页面外面vPageDrawRight
      // 底部让出1像素，否则表格底部边框和数据绘制底部一样时，边框绘制不出来。Rgn比Rect约束了1像素？
      APaintInfo.GetScaleY(Min(vPageDrawBottom, vPageDataScreenBottom)) + 1);
    try
      SelectClipRgn(ACanvas.Handle, vPaintRegion);  // 设置绘制有效区域
      PaintPage;
    finally
      DeleteObject(vPaintRegion);
    end;

    {ACanvas.Brush.Color := clYellow;
    vRect := Rect(vPageDrawLeft,
      Max(vPageDrawTop + vHeaderAreaHeight, vPageDataScreenTop),
      vPageDrawRight,
      Min(vPageDrawBottom - PageMarginBottomPix, vPageDataScreenBottom));
    ACanvas.FillRect(vRect);}
  end;
  {$ENDREGION}

  // 恢复区域，准备给整页绘制用(为支持浮动Item，所以纸张边距)
  vPaintRegion := CreateRectRgn(
    APaintInfo.GetScaleX(vPaperDrawLeft),
    APaintInfo.GetScaleX(vPaperDrawTop),
    APaintInfo.GetScaleX(vPaperDrawRight),
    APaintInfo.GetScaleX(vPaperDrawBottom));
  try
    SelectClipRgn(ACanvas.Handle, vPaintRegion);

    if APaintInfo.ViewModel = hvmFilm then
    begin
      FHeader.PaintFloatItems(APageIndex, vPageDrawLeft,
        vPaperDrawTop + GetHeaderPageDrawTop, 0, ACanvas, APaintInfo);

      FFooter.PaintFloatItems(APageIndex, vPageDrawLeft,
        vPageDrawBottom, 0, ACanvas, APaintInfo);
    end;

    FPage.PaintFloatItems(APageIndex, vPageDrawLeft,  // 当前页绘制到的Left
      vPageDrawTop,     // 当前页绘制到的Top
      GetPageDataFmtTop(APageIndex),  // 指定从哪个位置开始的数据绘制到页数据起始位置
      ACanvas,
      APaintInfo);
  finally
    DeleteObject(vPaintRegion);
  end;

  // 恢复区域，准备给外部绘制用
  vPaintRegion := CreateRectRgn(
    APaintInfo.GetScaleX(vClipBoxRect.Left),
    APaintInfo.GetScaleX(vClipBoxRect.Top),
    APaintInfo.GetScaleX(vClipBoxRect.Right),
    APaintInfo.GetScaleX(vClipBoxRect.Bottom));
  try
    SelectClipRgn(ACanvas.Handle, vPaintRegion);
  finally
    DeleteObject(vPaintRegion);
  end;

  if Assigned(FOnPaintPaperAfter) then  // 公开页面绘制后事件
  begin
    vDCState := SaveCanvas(ACanvas);
    try
      FOnPaintPaperAfter(Self, APageIndex,
        Rect(vPaperDrawLeft, vPaperDrawTop, vPaperDrawRight, vPaperDrawBottom),
        ACanvas, APaintInfo);
    finally
      vDCState.ToCanvas(ACanvas);
      FreeAndNil(vDCState);
    end;
  end;
end;

procedure THCCustomSection.BuildSectionPages(const AStartDrawItemNo: Integer);
var
  vPageIndex, vPageDataFmtTop, vPageDataFmtBottom, vPageHeight: Integer;

  {$REGION '_FormatNewPage'}
  procedure _FormatNewPage(const APrioEndDItemNo, ANewStartDItemNo: Integer);
  var
    vPage: THCPage;
  begin
    FPages[vPageIndex].EndDrawItemNo := APrioEndDItemNo;
    vPage := THCPage.Create;
    vPage.StartDrawItemNo := ANewStartDItemNo;
    FPages.Insert(vPageIndex + 1, vPage);
    Inc(vPageIndex);
  end;
  {$ENDREGION}

  {$REGION '_FormatRectItemCheckPageBreak'}
  procedure _FormatRectItemCheckPageBreak(const ADrawItemNo: Integer);
  var
    vRectItem: THCCustomRectItem;
    vSuplus,  // 所有因换页向下偏移量的总和
    vBreakSeat  // 分页位置，不同RectItem的含义不同，表格表示 vBreakRow
      : Integer;

    {$REGION '_RectItemCheckPage'}
    procedure _RectItemCheckPage(const AStartSeat: Integer);  // 开始分页计算的位置，不同RectItem含义不同，表格表示AStartRowNo
    var
      vFmtHeightInc, vFmtOffset: Integer;
      vDrawRect: TRect;
    begin
      vFmtOffset := 0;

      {if FPage.GetDrawItemStyle(ADrawItemNo) = THCStyle.PageBreak then
      begin
        vFmtOffset := vPageDataFmtBottom - FPage.DrawItems[ADrawItemNo].Rect.Top;

        vSuplus := vSuplus + vFmtOffset;
        if vFmtOffset > 0 then  // 整体向下移动了
          OffsetRect(FPage.DrawItems[ADrawItemNo].Rect, 0, vFmtOffset);

        vPageDataFmtTop := vPageDataFmtBottom;
        vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;

        _FormatNewPage(ADrawItemNo - 1, ADrawItemNo);  // 新建页
      end
      else}
      if FPage.DrawItems[ADrawItemNo].Rect.Bottom > vPageDataFmtBottom then  // 当前页放不下表格整体(带行间距)
      begin
        if (FPages[vPageIndex].StartDrawItemNo = ADrawItemNo)
          and (AStartSeat = 0)
          and (not vRectItem.CanPageBreak)
        then  // 当前页从头开始整页放不下，也不允许截断，强制变矮当前页能显示多少显示多少
        begin
          vFmtHeightInc := vPageDataFmtBottom - FPage.DrawItems[ADrawItemNo].Rect.Bottom;
          vSuplus := vSuplus + vFmtHeightInc;
          FPage.DrawItems[ADrawItemNo].Rect.Bottom :=  // 扩充格式化高度
            FPage.DrawItems[ADrawItemNo].Rect.Bottom + vFmtHeightInc;
          vRectItem.Height := vRectItem.Height + vFmtHeightInc;  // 是在这里处理呢还是在RectItem内部更合适？

          Exit;
        end;

        vDrawRect := FPage.DrawItems[ADrawItemNo].Rect;

        //if vSuplus = 0 then  // 第一次计算分页
        InflateRect(vDrawRect, 0, -FPage.GetLineBlankSpace(ADrawItemNo) div 2);  // 减掉行间距，为了达到去掉行间距能放下不换页的效果

        vRectItem.CheckFormatPageBreak(  // 去除行间距后，判断表格跨页位置
          FPages.Count - 1,
          vDrawRect.Top,  // 表格的顶部位置 FPageData.DrawItems[ADrawItemNo].Rect.Top,
          vDrawRect.Bottom,  // 表格的底部位置 FPageData.DrawItems[ADrawItemNo].Rect.Bottom,
          vPageDataFmtTop,
          vPageDataFmtBottom,  // 当前页的数据底部位置
          AStartSeat,  // 起始位置
          vBreakSeat,  // 当前页分页的行(位置)
          vFmtOffset,  // 当前RectItem为了避开分页位置整体向下偏移的高度
          vFmtHeightInc  // 当前行各列为了避开分页位置单元格内容额外偏移的最大高度
          );

        if vBreakSeat < 0 then // 除去行间距后不用跨页就可以显示了
        begin
          vSuplus := vSuplus + vPageDataFmtBottom - vDrawRect.Bottom;
        end
        else  // vBreakSeat >= 0 从vBreakSeat位置跨页
        if vFmtOffset > 0 then  // 整体跨页，整体向下移动了
        begin
          vFmtOffset := vFmtOffset + FPage.GetLineBlankSpace(ADrawItemNo) div 2;  // 整体向下移动增加上面减掉的行间距
          vSuplus := vSuplus + vFmtOffset + vFmtHeightInc;

          OffsetRect(FPage.DrawItems[ADrawItemNo].Rect, 0, vFmtOffset);

          vPageDataFmtTop := vPageDataFmtBottom;
          vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;
          _FormatNewPage(ADrawItemNo - 1, ADrawItemNo);  // 新建页
          _RectItemCheckPage(vBreakSeat);
        end
        else  // 跨页，但未整体下移
        begin
          vSuplus := vSuplus{ + vFmtOffset} + vFmtHeightInc;
          FPage.DrawItems[ADrawItemNo].Rect.Bottom :=  // 扩充格式化高度
            FPage.DrawItems[ADrawItemNo].Rect.Bottom + vFmtHeightInc;
          vRectItem.Height := vRectItem.Height + vFmtHeightInc;  // 是在这里处理呢还是在RectItem内部更合适？

          vPageDataFmtTop := vPageDataFmtBottom;
          vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;
          _FormatNewPage(ADrawItemNo, ADrawItemNo);  // 新建页
          _RectItemCheckPage(vBreakSeat);  // 从分页位置后面继续检查是否分页
        end;
      end;
    end;
    {$ENDREGION}

  var
    i: Integer;
  begin
    vRectItem := FPage.Items[FPage.DrawItems[ADrawItemNo].ItemNo] as THCCustomRectItem;
    vSuplus := 0;
    vBreakSeat := 0;

    vRectItem.CheckFormatPageBreakBefor;
    _RectItemCheckPage(0);  // 从最开始位置，检测表格各行内容是否能显示在当前页

    if vSuplus <> 0 then
    begin
      for i := ADrawItemNo + 1 to FPage.DrawItems.Count - 1 do
        OffsetRect(FPage.DrawItems[i].Rect, 0, vSuplus);
    end;
  end;
  {$ENDREGION}

  {$REGION '_FormatTextItemCheckPageBreak'}
  procedure _FormatTextItemCheckPageBreak(const ADrawItemNo: Integer);
  var
    i, vH: Integer;
  begin
    //if not DrawItems[ADrawItemNo].LineFirst then Exit; // 注意如果文字环绕时这里就不能只判断行第1个
    if FPage.DrawItems[ADrawItemNo].Rect.Bottom > vPageDataFmtBottom then
    begin
      vH := vPageDataFmtBottom - FPage.DrawItems[ADrawItemNo].Rect.Top;
      for i := ADrawItemNo to FPage.DrawItems.Count - 1 do
        OffsetRect(FPage.DrawItems[i].Rect, 0, vH);

      vPageDataFmtTop := vPageDataFmtBottom;
      vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;
      _FormatNewPage(ADrawItemNo - 1, ADrawItemNo); // 新建页
    end;
  end;
  {$ENDREGION}

var
  i, j, vPrioDrawItemNo, vFmtPageOffset: Integer;
  vPage: THCPage;
  vItem: THCCustomItem;
begin
  if FPage.FormatCount <> 0 then Exit;

  // 上一行所在页作为格式化起始页
  vPrioDrawItemNo := AStartDrawItemNo; // FPageData.GetItemLastDrawItemNo(AStartItemNo - 1)  // 上一个最后的DItem
  while vPrioDrawItemNo > 0 do
  begin
    if FPage.DrawItems[vPrioDrawItemNo].LineFirst then
      Break;

    Dec(vPrioDrawItemNo);
  end;
  Dec(vPrioDrawItemNo);  // 上一行末尾

  vPageIndex := 0;
  if vPrioDrawItemNo > 0 then
  begin
    for i := FPages.Count - 1 downto 0 do  // 对于跨页的，按最后位置所在页，所以倒序
    begin
      vPage := FPages[i];
      if (vPrioDrawItemNo >= vPage.StartDrawItemNo)
        and (vPrioDrawItemNo <= vPage.EndDrawItemNo)
      then  // 因为可能有跨页，所以需要判断起始结束都满足
      begin
        vPageIndex := i;
        Break;
      end;
    end;
  end;

  FPages.DeleteRange(vPageIndex + 1, FPages.Count - vPageIndex - 1);  // 删除当前页后面的，准备格式化

  if FPages.Count = 0 then  // 删除没了，补充第一个Page
  begin
    vPage := THCPage.Create;
    vPage.StartDrawItemNo := 0;
    FPages.Add(vPage);
    vPageIndex := 0;
  end;

  vPageDataFmtTop := GetPageDataFmtTop(vPageIndex);
  vPageHeight := GetPageHeight;
  vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;

  for i := vPrioDrawItemNo + 1 to FPage.DrawItems.Count - 1 do
  begin
    if FPage.DrawItems[i].LineFirst then
    begin
      vItem := FPage.Items[FPage.DrawItems[i].ItemNo];
      if vItem.PageBreak and (vItem.FirstDItemNo = i) then  // 分页Item只判断第一个DrawItem
      begin
        vFmtPageOffset := vPageDataFmtBottom - FPage.DrawItems[i].Rect.Top;
        if vFmtPageOffset > 0 then  // 整体向下移动了
        begin
          for j := i to FPage.DrawItems.Count - 1 do
            OffsetRect(FPage.DrawItems[j].Rect, 0, vFmtPageOffset);
        end;

        vPageDataFmtTop := vPageDataFmtBottom;
        vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;

        _FormatNewPage(i - 1, i);  // 新建页
      end;

      if FPage.GetDrawItemStyle(i) < THCStyle.Null then
        _FormatRectItemCheckPageBreak(i)
      else
        _FormatTextItemCheckPageBreak(i);
    end;
  end;

  FPages[vPageIndex].EndDrawItemNo := FPage.DrawItems.Count - 1;
  SetActivePageIndex(GetPageIndexByCurrent);

  for i := FPage.FloatItems.Count - 1 downto 0 do  // 正文中删除页序号超过页总数的FloatItem
  begin
    if FPage.FloatItems[i].PageIndex > FPages.Count - 1 then
      FPage.FloatItems.Delete(i);
  end;
end;

procedure THCCustomSection.ActiveItemReAdaptEnvironment;
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ActiveItemReAdaptEnvironment;
    end);
end;

procedure THCCustomSection.Redo(const ARedo: THCUndo);
var
  vUndoList: THCUndoList;
begin
  vUndoList := DoDataGetUndoList;
  //if vUndoList.Enable then  // 不能判断，因为撤销恢复过程会屏蔽，防止产生新的撤销恢复
  if not vUndoList.GroupWorking then  // 不在组中处理时才重新设置Data和响应变动
  begin
    if FActiveData <> ARedo.Data then
      SetActiveData(ARedo.Data as THCSectionData);

    DoSectionDataAction(FActiveData, function(): Boolean
      begin
        FActiveData.Redo(ARedo);
      end);
  end
  else
    (ARedo.Data as THCSectionData).Redo(ARedo);
end;

procedure THCCustomSection.ReFormatActiveItem;
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ReFormatActiveItem;
    end);
end;

procedure THCCustomSection.ReFormatActiveParagraph;
begin
  DoSectionDataAction(FActiveData, function(): Boolean
    begin
      FActiveData.ReFormatActiveParagraph;
    end);
end;

procedure THCCustomSection.ResetMargin;
begin
  FPage.Width := GetPageWidth;

  FHeader.Width := FPage.Width;
  FFooter.Width := FPage.Width;

  FormatData;

  BuildSectionPages(0);

  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret(False);

  DoDataChanged(Self);
end;

procedure THCCustomSection.SaveToStream(const AStream: TStream;
  const ASaveParts: TSectionAreas = [saHeader, saPage, saFooter]);
var
  vBegPos, vEndPos: Int64;
  vArea: Boolean;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 数据大小占位，便于越过
  //
  if ASaveParts <> [] then
  begin
    AStream.WriteBuffer(FSymmetryMargin, SizeOf(FSymmetryMargin));  // 是否对称页边距

    AStream.WriteBuffer(FPaperOrientation, SizeOf(FPaperOrientation));  // 纸张方向
    AStream.WriteBuffer(FPageNoVisible, SizeOf(FPageNoVisible));  // 是否显示页码

    FPaper.SaveToStream(AStream);  // 页面参数

    vArea := saHeader in ASaveParts;  // 存页眉
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    vArea := saFooter in ASaveParts;  // 存页脚
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    vArea := saPage in ASaveParts;  // 存页面
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    if saHeader in ASaveParts then  // 存页眉
    begin
      AStream.WriteBuffer(FHeaderOffset, SizeOf(FHeaderOffset));
      FHeader.SaveToStream(AStream);
    end;

    if saFooter in ASaveParts then  // 存页脚
      FFooter.SaveToStream(AStream);

    if saPage in ASaveParts then  // 存页面
      FPage.SaveToStream(AStream);
  end;
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 当前节数据大小
  AStream.Position := vEndPos;
end;

function THCCustomSection.SaveToText: string;
begin
  Result := FPage.SaveToText;
end;

procedure THCCustomSection.SectionCoordToPaper(const APageIndex, X, Y: Integer; var APageX,
  APageY: Integer);
var
  vPageFilmTop{, vMarginLeft, vMarginRight}: Integer;
begin
  // 预留页横向排版时计算
  //GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);
  APageX := X;// - vMarginLeft;

  if FViewModel = hvmFilm then
    vPageFilmTop := GetPageTopFilm(APageIndex)
  else
    vPageFilmTop := GetPageTop(APageIndex);

  APageY := Y - vPageFilmTop;  // 映射到当前页面为原点的屏显起始位置(可为负数)
end;

procedure THCCustomSection.SelectAll;
begin
  FActiveData.SelectAll;
end;

function THCCustomSection.SelectExists: Boolean;
begin
  Result := FActiveData.SelectExists;
end;

procedure THCCustomSection.SetHeaderOffset(const Value: Integer);
begin
  if FHeaderOffset <> Value then
  begin
    FHeaderOffset := Value;
    BuildSectionPages(0);
    DoDataChanged(Self);
  end;
end;

procedure THCCustomSection.SetPaperHeight(const Value: Single);
begin
  FPaper.Height := Value;
end;

procedure THCCustomSection.SetPaperMarginBottom(const Value: Single);
begin
  FPaper.MarginBottom := Value;
end;

procedure THCCustomSection.SetPaperMarginLeft(const Value: Single);
begin
  FPaper.MarginLeft := Value;
end;

procedure THCCustomSection.SetPaperMarginRight(const Value: Single);
begin
  FPaper.MarginRight := Value;
end;

procedure THCCustomSection.SetPaperMarginTop(const Value: Single);
begin
  FPaper.MarginTop := Value;
end;

procedure THCCustomSection.SetPaperSize(const Value: Integer);
begin
  FPaper.Size := Value;
end;

procedure THCCustomSection.SetPaperWidth(const Value: Single);
begin
  FPaper.Width := Value;
end;

procedure THCCustomSection.SetReadOnly(const Value: Boolean);
begin
  FHeader.ReadOnly := Value;
  FFooter.ReadOnly := Value;
  FPage.ReadOnly := Value;
end;

function THCCustomSection.TableApplyContentAlign(
  const AAlign: THCContentAlign): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableApplyContentAlign(AAlign);
    end);
end;

procedure THCCustomSection.Undo(const AUndo: THCUndo);
var
  vUndoList: THCUndoList;
begin
  vUndoList := DoDataGetUndoList;
  //if vUndoList.Enable then  // 不能判断，因为撤销恢复过程会屏蔽，防止产生新的撤销恢复
  if not vUndoList.GroupWorking then  // 不在组中处理时才重新设置Data和响应变动
  begin
    if FActiveData <> AUndo.Data then
      SetActiveData(AUndo.Data as THCSectionData);

    DoSectionDataAction(FActiveData, function(): Boolean
      begin
        FActiveData.Undo(AUndo);
      end);
  end
  else
    (AUndo.Data as THCSectionData).Undo(AUndo);
end;

{ THCSection }

function THCSection.InsertFloatItem(const AFloatItem: THCCustomFloatItem): Boolean;
begin
  if not FActiveData.CanEdit then Exit(False);
  AFloatItem.PageIndex := FActivePageIndex;
  Result := FActiveData.InsertFloatItem(AFloatItem);
  DoDataChanged(Self);
end;

function THCSection.ParseHtml(const AHtmlText: string): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    var
      vHtmlFmt: THCHtmlFormat;
    begin
      vHtmlFmt := THCHtmlFormat.Create(FActiveData.GetTopLevelData as THCRichData);
      try
        Result := vHtmlFmt.Parse(AHtmlText);
      finally
        FreeAndNil(vHtmlFmt);
      end;
    end);
end;

procedure THCSection.ParseXml(const ANode: IHCXMLNode);

  procedure GetXmlPaper_;
  var
    vsPaper: TStringList;
  begin
    vsPaper := TStringList.Create;
    try
      vsPaper.Delimiter := ',';
      vsPaper.DelimitedText := ANode.Attributes['pagesize'];
      FPaper.Size := StrToInt(vsPaper[0]);  // 纸张大小
      FPaper.Width := StrToFloat(vsPaper[1]);  // 纸张宽度
      FPaper.Height := StrToFloat(vsPaper[2]);  // 纸张高度
    finally
      FreeAndNil(vsPaper);
    end;
  end;

  procedure GetXmlPaperMargin_;
  var
    vsMargin: TStringList;
  begin
    vsMargin := TStringList.Create;
    try
      vsMargin.Delimiter := ',';
      vsMargin.DelimitedText := ANode.Attributes['margin'];  // 边距
      FPaper.MarginLeft := StrToFloat(vsMargin[0]);
      FPaper.MarginTop := StrToFloat(vsMargin[1]);
      FPaper.MarginRight := StrToFloat(vsMargin[2]);
      FPaper.MarginBottom := StrToFloat(vsMargin[3]);
    finally
      FreeAndNil(vsMargin);
    end;
  end;

var
  i: Integer;
begin
  FSymmetryMargin := ANode.Attributes['symmargin'];  // 是否对称页边距
  FPaperOrientation := TPaperOrientation(ANode.Attributes['ori']);  // 纸张方向

  FPageNoVisible := ANode.Attributes['pagenovisible'];  // 是否对称页边距
  GetXmlPaper_;
  GetXmlPaperMargin_;

  FPage.Width := GetPageWidth;

  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    if ANode.ChildNodes[i].NodeName = 'header' then
    begin
      FHeaderOffset := ANode.ChildNodes[i].Attributes['offset'];
      FHeader.Width := FPage.Width;
      FHeader.ParseXml(ANode.ChildNodes[i]);
    end
    else
    if ANode.ChildNodes[i].NodeName = 'footer' then
    begin
      FFooter.Width := FPage.Width;
      FFooter.ParseXml(ANode.ChildNodes[i]);
    end
    else
    if ANode.ChildNodes[i].NodeName = 'page' then
      FPage.ParseXml(ANode.ChildNodes[i]);
  end;

  BuildSectionPages(0);
end;

function THCSection.Replace(const AText: string): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.Replace(AText);
    end);
end;

function THCSection.Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean;
begin
  Result := FActiveData.Search(AKeyword, AForward, AMatchCase);
  DoActiveDataCheckUpdateInfo;
end;

function THCSection.ToHtml(const APath: string): string;
begin
  Result := FHeader.ToHtml(APath) + sLineBreak + FPage.ToHtml(APath) + sLineBreak + FFooter.ToXml(APath);
end;

procedure THCSection.ToXml(const ANode: IHCXMLNode);
var
  vNode: IHCXMLNode;
begin
  ANode.Attributes['symmargin'] := FSymmetryMargin; // 是否对称页边距
  ANode.Attributes['ori'] := Ord(FPaperOrientation);  // 纸张方向
  ANode.Attributes['pagenovisible'] := FPageNoVisible;  // 是否显示页码

  ANode.Attributes['pagesize'] :=  // 纸张大小
    IntToStr(FPaper.Size)
    + ',' + FormatFloat('0.#', FPaper.Width)
    + ',' + FormatFloat('0.#', FPaper.Height) ;

  ANode.Attributes['margin'] :=  // 边距
    FormatFloat('0.#', FPaper.MarginLeft) + ','
    + FormatFloat('0.#', FPaper.MarginTop) + ','
    + FormatFloat('0.#', FPaper.MarginRight) + ','
    + FormatFloat('0.#', FPaper.MarginBottom);

  // 存页眉
  vNode := ANode.AddChild('header');
  vNode.Attributes['offset'] := FHeaderOffset;
  FHeader.ToXml(vNode);

  // 存页脚
  vNode := ANode.AddChild('footer');
  FFooter.ToXml(vNode);

  // 存页面
  vNode := ANode.AddChild('page');
  FPage.ToXml(vNode);
end;

{ TSectionPaintInfo }

constructor TSectionPaintInfo.Create;
begin
  inherited Create;
  FSectionIndex := -1;
  FPageIndex := -1;
  FPageDataFmtTop := 0;
end;

end.
