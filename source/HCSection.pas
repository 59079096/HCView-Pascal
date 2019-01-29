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
  end;

  TSectionPagePaintEvent = procedure(const Sender: TObject; const APageIndex: Integer;
    const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo) of object;
  TSectionDrawItemPaintEvent = procedure(const Sender: TObject; const AData: THCCustomData;
      const ADrawItemNo: Integer; const ADrawRect: TRect;
      const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;
  TSectionDataItemNotifyEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AItem: THCCustomItem) of object;
  TSectionDrawItemAnnotateEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate) of object;
  TSectionAnnotateEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const ADataAnnotate: THCDataAnnotate) of object;
  TSectionDataItemMouseEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  THCCustomSection = class(TObject)
  private
    FStyle: THCStyle;

    /// <summary> 是否对称边距 </summary>
    FSymmetryMargin: Boolean;
    FPages: THCPages;  // 所有页面
    FPageSize: THCPageSize;
    FPageOrientation: TPageOrientation;
    FHeader: THCHeaderData;
    FFooter: THCFooterData;
    FPageData: THCPageData;
    FActiveData: THCSectionData;  // 页眉、正文、页脚
    FMoveData: THCSectionData;

    FPageNoVisible: Boolean;  // 是否显示页码

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

    FOnPaintHeader, FOnPaintFooter, FOnPaintPage,
    FOnPaintWholePageBefor, FOnPaintWholePageAfter: TSectionPagePaintEvent;
    FOnDrawItemPaintBefor, FOnDrawItemPaintAfter: TSectionDrawItemPaintEvent;

    FOnInsertAnnotate, FOnRemoveAnnotate: TSectionAnnotateEvent;
    FOnDrawItemAnnotate: TSectionDrawItemAnnotateEvent;

    FOnDrawItemPaintContent: TDrawItemPaintContentEvent;
    FOnInsertItem, FOnRemoveItem: TSectionDataItemNotifyEvent;
    FOnItemMouseUp: TSectionDataItemMouseEvent;
    FOnItemResize: TDataItemEvent;
    FOnCreateItem, FOnCurParaNoChange, FOnActivePageChange: TNotifyEvent;
    FOnCreateItemByStyle: TStyleItemEvent;
    FOnCanEdit: TOnCanEditEvent;
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
      const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DoDataDrawItemPaintContent(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADrawText: string;
      const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DoDataDrawItemPaintAfter(const AData: THCCustomData;
      const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

    procedure DoDataInsertAnnotate(const AData: THCCustomData; const ADataAnnotate: THCDataAnnotate);
    procedure DoDataRemoveAnnotate(const AData: THCCustomData; const ADataAnnotate: THCDataAnnotate);
    procedure DoDataDrawItemAnnotate(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate);

    procedure DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem);
    procedure DoDataRemoveItem(const AData: THCCustomData; const AItem: THCCustomItem);
    procedure DoDataItemMouseUp(const AData: THCCustomData; const AItemNo: Integer;
       Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoDataChanged(Sender: TObject);

    /// <summary> 缩放Item约束不要超过整页宽、高 </summary>
    procedure DoDataItemResized(const AData: THCCustomData; const AItemNo: Integer);
    function DoDataCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
    function DoDataCanEdit(const Sender: TObject): Boolean;
    procedure DoDataCreateItem(Sender: TObject);
    procedure DoDataCurParaNoChange(Sender: TObject);
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
    procedure PageCoordToData(const APageIndex: Integer;
      const AData: THCViewData; var AX, AY: Integer;
      const ARestrain: Boolean = False);

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
    procedure SetPageOrientation(const Value: TPageOrientation);

    function GetPageWidthPix: Integer;
    function GetPageHeightPix: Integer;
    function GetPageMarginTopPix: Integer;
    function GetPageMarginLeftPix: Integer;
    function GetPageMarginRightPix: Integer;
    function GetPageMarginBottomPix: Integer;

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
    function ActiveDataChangeByAction(const AFunction: THCFunction): Boolean;

    property Style: THCStyle read FStyle;
  public
    constructor Create(const AStyle: THCStyle);
    destructor Destroy; override;
    //
    /// <summary> 修改纸张边距 </summary>
    procedure ResetMargin;
    procedure DisActive;
    function SelectExists: Boolean;
    procedure SelectAll;
    function GetHint: string;
    function GetCurItem: THCCustomItem;
    function GetTopLevelItem: THCCustomItem;
    function GetTopLevelDrawItem: THCCustomDrawItem;
    function GetActiveDrawItemCoord: TPoint;

    /// <summary> 返回光标或选中结束位置所在页序号 </summary>
    function GetPageIndexByCurrent: Integer;

    /// <summary> 返回正文格式位置所在页序号 </summary>
    function GetPageIndexByFormat(const AVOffset: Integer): Integer;

    procedure PaintDisplayPage(const AFilmOffsetX, AFilmOffsetY: Integer;
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

    function InsertText(const AText: string): Boolean;
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
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
    //
    function ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;
    function ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;
    function ActiveTableDeleteCurRow: Boolean;
    function ActiveTableSplitCurRow: Boolean;
    function ActiveTableSplitCurCol: Boolean;
    function ActiveTableInsertColAfter(const AColCount: Byte): Boolean;
    function ActiveTableInsertColBefor(const AColCount: Byte): Boolean;
    function ActiveTableDeleteCurCol: Boolean;
    //
    //// <summary>  节坐标转换到指定页坐标 </summary>
    procedure SectionCoordToPage(const APageIndex, X, Y: Integer; var
      APageX, APageY: Integer);

    /// <summary> 为段应用对齐方式 </summary>
    /// <param name="AAlign">对方方式</param>
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);
    procedure ApplyParaBackColor(const AColor: TColor);
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode);
    procedure ApplyParaLeftIndent(const AIndent: Single);
    procedure ApplyParaRightIndent(const AIndent: Single);
    procedure ApplyParaFirstIndent(const AIndent: Single);
    /// <summary> 获取光标在Dtat中的位置信息并映射到指定页面 </summary>
    /// <param name="APageIndex">要映射到的页序号</param>
    /// <param name="ACaretInfo">光标位置信息</param>
    procedure GetPageCaretInfo(var ACaretInfo: THCCaretInfo);

    /// <summary> 绘制指定页到指定的位置，为配合打印，开放ADisplayWidth, ADisplayHeight参数 </summary>
    /// <param name="APageIndex">要绘制的页码</param>
    /// <param name="ALeft">绘制X偏移</param>
    /// <param name="ATop">绘制Y偏移</param>
    /// <param name="ACanvas"></param>
    procedure PaintPage(const APageIndex, ALeft, ATop: Integer;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure Clear; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    /// <summary> 某页在整个节中的Top位置 </summary>
    /// <param name="APageIndex"></param>
    /// <returns></returns>
    function GetPageTopFilm(const APageIndex: Integer): Integer;

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
    function MergeTableSelectCells: Boolean;
    procedure ReFormatActiveParagraph;
    procedure ReFormatActiveItem;
    function GetHeaderAreaHeight: Integer;
    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
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
    property PageOrientation: TPageOrientation read FPageOrientation write SetPageOrientation;
    //
    property PageWidthPix: Integer read GetPageWidthPix;
    property PageHeightPix: Integer read GetPageHeightPix;
    property PageMarginTopPix: Integer read GetPageMarginTopPix;
    property PageMarginLeftPix: Integer read GetPageMarginLeftPix;
    property PageMarginRightPix: Integer read GetPageMarginRightPix;
    property PageMarginBottomPix: Integer read GetPageMarginBottomPix;

    property HeaderOffset: Integer read FHeaderOffset write SetHeaderOffset;
    property Header: THCHeaderData read FHeader;
    property Footer: THCFooterData read FFooter;
    property PageData: THCPageData read FPageData;
    property CurStyleNo: Integer read GetCurStyleNo;
    property CurParaNo: Integer read GetCurParaNo;

    /// <summary> 当前文档激活区域(页眉、页脚、页面)的数据对象 </summary>
    property ActiveData: THCSectionData read FActiveData write SetActiveData;

    /// <summary> 当前文档激活区域页眉、页脚、页面 </summary>
    property ActiveArea: TSectionArea read GetActiveArea;
    property ActivePageIndex: Integer read FActivePageIndex;

    /// <summary> 是否对称边距 </summary>
    property SymmetryMargin: Boolean read FSymmetryMargin write FSymmetryMargin;
    property DisplayFirstPageIndex: Integer read FDisplayFirstPageIndex write FDisplayFirstPageIndex;  // 屏显第一页
    property DisplayLastPageIndex: Integer read FDisplayLastPageIndex write FDisplayLastPageIndex;  // 屏显最后一页
    property PageCount: Integer read GetPageCount;
    property PageNoVisible: Boolean read FPageNoVisible write FPageNoVisible;
    property PageNoFrom: Integer read FPageNoFrom write FPageNoFrom;

    /// <summary> 文档所有部分是否只读 </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnChangeTopLevelData: TNotifyEvent read FOnChangeTopLevelData write FOnChangeTopLevelData;
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;
    property OnGetScreenCoord: TGetScreenCoordEvent read FOnGetScreenCoord write FOnGetScreenCoord;
    property OnCheckUpdateInfo: TNotifyEvent read FOnCheckUpdateInfo write FOnCheckUpdateInfo;
    property OnInsertItem: TSectionDataItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TSectionDataItemNotifyEvent read FOnRemoveItem write FOnRemoveItem;
    property OnItemResize: TDataItemEvent read FOnItemResize write FOnItemResize;
    property OnItemMouseUp: TSectionDataItemMouseEvent read FOnItemMouseUp write FOnItemMouseUp;
    property OnPaintHeader: TSectionPagePaintEvent read FOnPaintHeader write FOnPaintHeader;
    property OnPaintFooter: TSectionPagePaintEvent read FOnPaintFooter write FOnPaintFooter;
    property OnPaintPage: TSectionPagePaintEvent read FOnPaintPage write FOnPaintPage;
    property OnPaintWholePageBefor: TSectionPagePaintEvent read FOnPaintWholePageBefor write FOnPaintWholePageBefor;
    property OnPaintWholePageAfter: TSectionPagePaintEvent read FOnPaintWholePageAfter write FOnPaintWholePageAfter;
    property OnDrawItemPaintBefor: TSectionDrawItemPaintEvent read FOnDrawItemPaintBefor write FOnDrawItemPaintBefor;
    property OnDrawItemPaintAfter: TSectionDrawItemPaintEvent read FOnDrawItemPaintAfter write FOnDrawItemPaintAfter;
    property OnDrawItemPaintContent: TDrawItemPaintContentEvent read FOnDrawItemPaintContent write FOnDrawItemPaintContent;
    property OnInsertAnnotate: TSectionAnnotateEvent read FOnInsertAnnotate write FOnInsertAnnotate;
    property OnRemoveAnnotate: TSectionAnnotateEvent read FOnRemoveAnnotate write FOnRemoveAnnotate;
    property OnDrawItemAnnotate: TSectionDrawItemAnnotateEvent read FOnDrawItemAnnotate write FOnDrawItemAnnotate;
    property OnCreateItem: TNotifyEvent read FOnCreateItem write FOnCreateItem;
    property OnCreateItemByStyle: TStyleItemEvent read FOnCreateItemByStyle write FOnCreateItemByStyle;
    property OnCanEdit: TOnCanEditEvent read FOnCanEdit write FOnCanEdit;
    property OnGetUndoList: TGetUndoListEvent read FOnGetUndoList write FOnGetUndoList;
    property OnCurParaNoChange: TNotifyEvent read FOnCurParaNoChange write FOnCurParaNoChange;
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

function THCCustomSection.ActiveTableDeleteCurCol: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.ActiveTableDeleteCurCol;
    end);
end;

function THCCustomSection.ActiveTableDeleteCurRow: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.ActiveTableDeleteCurRow;
    end);
end;

function THCCustomSection.ActiveTableInsertColAfter(const AColCount: Byte): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.TableInsertColAfter(AColCount);
    end);
end;

function THCCustomSection.ActiveTableInsertColBefor(const AColCount: Byte): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.TableInsertColBefor(AColCount);
    end);
end;

function THCCustomSection.ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.TableInsertRowAfter(ARowCount);
    end);
end;

function THCCustomSection.ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.TableInsertRowBefor(ARowCount);
    end);
end;

function THCCustomSection.ActiveTableSplitCurCol: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.ActiveTableSplitCurCol;
    end);
end;

function THCCustomSection.ActiveTableSplitCurRow: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.ActiveTableSplitCurRow;
    end);
end;

procedure THCCustomSection.ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaAlignHorz(AAlign);
    end);
end;

procedure THCCustomSection.ApplyParaAlignVert(const AAlign: TParaAlignVert);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaAlignVert(AAlign);
    end);
end;

procedure THCCustomSection.ApplyParaBackColor(const AColor: TColor);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaBackColor(AColor);
    end);
end;

procedure THCCustomSection.ApplyParaFirstIndent(const AIndent: Single);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaFirstIndent(AIndent);
    end);
end;

procedure THCCustomSection.ApplyParaLeftIndent(const AIndent: Single);
begin
  ActiveDataChangeByAction(function(): Boolean
    var
      vContentWidth: Single;
    begin
      if AIndent < 0 then
        FActiveData.ApplyParaLeftIndent(0)
      else
      begin
        vContentWidth := FPageSize.PaperWidth - FPageSize.PaperMarginLeft - FPageSize.PaperMarginRight;
        if AIndent > vContentWidth - 5 then
          FActiveData.ApplyParaLeftIndent(vContentWidth - 5)
        else
          FActiveData.ApplyParaLeftIndent(AIndent);
      end;
    end);
end;

procedure THCCustomSection.ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaLineSpace(ASpaceMode);
    end);
end;

procedure THCCustomSection.ApplyParaRightIndent(const AIndent: Single);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaRightIndent(AIndent);
    end);
end;

procedure THCCustomSection.ApplyTextBackColor(const AColor: TColor);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextBackColor(AColor);
    end);
end;

procedure THCCustomSection.ApplyTextColor(const AColor: TColor);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextColor(AColor);
    end);
end;

procedure THCCustomSection.ApplyTextFontName(const AFontName: TFontName);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextFontName(AFontName);
    end);
end;

procedure THCCustomSection.ApplyTextFontSize(const AFontSize: Single);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextFontSize(AFontSize);
    end);
end;

procedure THCCustomSection.ApplyTextStyle(const AFontStyle: THCFontStyle);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextStyle(AFontStyle);
    end);
end;

procedure THCCustomSection.Clear;
begin
  FHeader.Clear;
  FFooter.Clear;
  FPageData.Clear;
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
    AData.OnItemResized := DoDataItemResized;
    AData.OnItemMouseUp := DoDataItemMouseUp;
    AData.OnCreateItemByStyle := DoDataCreateStyleItem;
    AData.OnCanEdit := DoDataCanEdit;
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
  end;

begin
  inherited Create;
  FStyle := AStyle;
  FActiveData := nil;
  FMoveData := nil;
  FPageNoVisible := True;
  FPageNoFrom := 1;
  FHeaderOffset := 20;
  FDisplayFirstPageIndex := -1;
  FDisplayLastPageIndex := -1;

  FPageSize := THCPageSize.Create;
  FPageOrientation := TPageOrientation.cpoPortrait;
  vWidth := GetContentWidth;

  FPageData := THCPageData.Create(AStyle);
  SetDataProperty(FPageData);

  // FData.PageHeight := PageHeightPix - PageMarginBottomPix - GetHeaderAreaHeight;
  // 在ReFormatSectionData中处理了FData.PageHeight

  FHeader := THCHeaderData.Create(AStyle);
  SetDataProperty(FHeader);

  FFooter := THCFooterData.Create(AStyle);
  SetDataProperty(FFooter);

  FActiveData := FPageData;
  FSymmetryMargin := True;  // 对称页边距 debug

  FPages := THCPages.Create;
  NewEmptyPage;           // 创建空白页
  FPages[0].StartDrawItemNo := 0;
  FPages[0].EndDrawItemNo := 0;
end;

function THCCustomSection.DeleteSelected: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.DeleteSelected;
    end);
end;

destructor THCCustomSection.Destroy;
begin
  FHeader.Free;
  FFooter.Free;
  FPageData.Free;
  FPageSize.Free;
  FPages.Free;
  inherited Destroy;
end;

procedure THCCustomSection.DisActive;
begin
  //if FActiveData <> nil then
    FActiveData.DisSelect;

  FHeader.InitializeField;
  FFooter.InitializeField;
  FPageData.InitializeField;
  FActiveData := FPageData;
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

procedure THCCustomSection.DoDataDrawItemAnnotate(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate);
begin
  if Assigned(FOnDrawItemAnnotate) then
    FOnDrawItemAnnotate(Self, AData, ADrawItemNo, ADrawRect, ADataAnnotate);
end;

procedure THCCustomSection.DoDataDrawItemPaintAfter(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintAfter) then
  begin
    FOnDrawItemPaintAfter(Self, AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.DoDataDrawItemPaintBefor(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintBefor) then
  begin
    FOnDrawItemPaintBefor(Self, AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.DoDataDrawItemPaintContent(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADrawText: string;
  const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintContent) then
    FOnDrawItemPaintContent(AData, ADrawItemNo, ADrawRect, AClearRect, ADrawText,
      ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCCustomSection.DoDataItemMouseUp(const AData: THCCustomData;
  const AItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnItemMouseUp) then
    FOnItemMouseUp(Self, AData, AItemNo, Button, Shift, X, Y);
end;

procedure THCCustomSection.DoDataItemResized(const AData: THCCustomData; const AItemNo: Integer);
var
  vData: THCCustomData;
  vResizeItem: THCResizeRectItem;
  vWidth, vHeight: Integer;
begin
  vResizeItem := AData.Items[AItemNo] as THCResizeRectItem;
  vWidth := GetContentWidth;  // 页宽

  vData := AData.GetRootData;  // 获取是哪一部分的ResizeItem
  if vData = FHeader then
    vHeight := GetHeaderAreaHeight
  else
  if vData = FFooter then
    vHeight := FPageSize.PageMarginBottomPix
  else
  //if vData = FPageData then
    vHeight := GetContentHeight;// - FStyle.ParaStyles[vResizeItem.ParaNo].LineSpace;

  vResizeItem.RestrainSize(vWidth, vHeight);

  if Assigned(FOnItemResize) then
    FOnItemResize(AData, AItemNo);
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
  Footer.ReFormat;
  FPageData.ReFormat;
end;

function THCCustomSection.GetPageIndexByCurrent: Integer;
var
  i, vCaretDrawItemNo: Integer;
  vCaretInfo: THCCaretInfo;
begin
  Result := -1;
  if FActiveData <> FPageData then
    Result := FActivePageIndex
  else
  begin
    if FPageData.CaretDrawItemNo < 0 then
    begin
      vCaretDrawItemNo := FPageData.GetDrawItemNoByOffset(FPageData.SelectInfo.StartItemNo,
        FPageData.SelectInfo.StartItemOffset);
    end
    else
      vCaretDrawItemNo := FPageData.CaretDrawItemNo;

    for i := 0 to FPages.Count - 1 do
    begin
      if FPages[i].EndDrawItemNo >= vCaretDrawItemNo then
      begin
        if (i < FPages.Count - 1)
          and (FPages[i + 1].StartDrawItemNo = vCaretDrawItemNo)
        then  // 跨页了
        begin
          if FPageData.SelectInfo.StartItemNo >= 0 then
          begin
            vCaretInfo.Y := 0;
            (FPageData as THCCustomData).GetCaretInfo(FPageData.SelectInfo.StartItemNo,
              FPageData.SelectInfo.StartItemOffset, vCaretInfo);

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

function THCCustomSection.GetActiveDrawItemCoord: TPoint;
begin
  Result := FActiveData.GetActiveDrawItemCoord;
end;

function THCCustomSection.GetTopLevelItem: THCCustomItem;
begin
  Result := FActiveData.GetTopLevelItem;
end;

function THCCustomSection.GetContentHeight: Integer;
begin
  Result := FPageSize.PageHeightPix  // 节页面正文区域高度，即页面除页眉、页脚后净高
    - FPageSize.PageMarginBottomPix - GetHeaderAreaHeight;
end;

function THCCustomSection.GetContentWidth: Integer;
begin
  Result := FPageSize.PageWidthPix - FPageSize.PageMarginLeftPix - FPageSize.PageMarginRightPix;
end;

function THCCustomSection.GetCurItem: THCCustomItem;
begin
  Result := FActiveData.GetCurItem;
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
  Result := FHeader.ReadOnly and FFooter.ReadOnly and FPageData.ReadOnly;
end;

function THCCustomSection.GetSectionDataAt(const X, Y: Integer): THCSectionData;
var
  vPageIndex, vMarginLeft, vMarginRight: Integer;
begin
  Result := nil;
  vPageIndex := GetPageIndexByFilm(Y);
  GetPageMarginLeftAndRight(vPageIndex, vMarginLeft, vMarginRight);
  // 确定点击页面显示区域
  if X < 0 then  // 点在页左边的MinPadding区域TEditArea.eaLeftPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if X > FPageSize.PageWidthPix then  // 点在页右边的MinPadding区域TEditArea.eaRightPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if Y < 0 then  // 点在页上边的MinPadding区域TEditArea.eaTopPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if Y > FPageSize.PageHeightPix then  // 只有在最后一页下边的MinPadding区域点击时触发TEditArea.eaBottomPad
  begin
    Result := FActiveData;
    Exit;
  end;

  // 边距信息，先上下，再左右
  if Y > FPageSize.PageHeightPix - FPageSize.PageMarginBottomPix then  // 点击在页下边距区域TEditArea.eaMarginBottom
    Exit(FFooter);

  // 页眉区域实际高(页眉内容高度>上边距时，取页眉内容高度)
  if Y < GetHeaderAreaHeight then  // 点击在页眉/上边距区域TEditArea.eaMarginTop
    Exit(FHeader);

  //if X > FPageSize.PageWidthPix - vMarginRight then Exit;  // 点击在页右边距区域TEditArea.eaMarginRight
  //if X < vMarginLeft then Exit;  // 点击在页左边距区域TEditArea.eaMarginLeft
  //如果要区分左、右边距不是正文，注意双击等判断ActiveData为nil
  Result := FPageData;
end;

function THCCustomSection.GetDataFmtTopFilm(const AVertical: Integer): Integer;
var
  i, vTop, vContentHeight: Integer;
begin
  Result := 0;
  vTop := 0;
  vContentHeight := GetContentHeight;
  for i := 0 to FPages.Count - 1 do
  begin
    vTop := vTop + vContentHeight;
    if vTop >= AVertical then
    begin
      vTop := AVertical - (vTop - vContentHeight);
      Break;
    end
    else
      Result := Result + PagePadding + FPageSize.PageHeightPix;
  end;
  Result := Result + PagePadding + GetHeaderAreaHeight + vTop;
end;

function THCCustomSection.GetFilmHeight: Cardinal;
begin
  Result := FPages.Count * (PagePadding + FPageSize.PageHeightPix);
end;

function THCCustomSection.GetFilmWidth: Cardinal;
begin
  Result := FPages.Count * (PagePadding + FPageSize.PageWidthPix);
end;

function THCCustomSection.GetHeaderAreaHeight: Integer;
begin
  Result := FHeaderOffset + FHeader.Height;
  if Result < FPageSize.PageMarginTopPix then
    Result := FPageSize.PageMarginTopPix;
  //Result := Result + 20;  // debug
end;

function THCCustomSection.GetHeaderPageDrawTop: Integer;
var
  vHeaderHeight: Integer;
begin
  Result := FHeaderOffset;
  vHeaderHeight := FHeader.Height;
  if vHeaderHeight < (FPageSize.PageMarginTopPix - FHeaderOffset) then
    Result := Result + (FPageSize.PageMarginTopPix - FHeaderOffset - vHeaderHeight) div 2;
end;

function THCCustomSection.GetHint: string;
begin
  //Result := '';
  //if FActiveData <> nil then
    Result := FActiveData.GetTopLevelData.GetHint;
end;

function THCCustomSection.GetPageIndexByFormat(const AVOffset: Integer): Integer;
var
  vContentHeight: Integer;
begin
  vContentHeight := GetContentHeight;
  Result := AVOffset div vContentHeight;
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
    vPos := vPos + PagePadding + FPageSize.PageHeightPix;
    if vPos >= AVOffset then  // AVOffset < 0时有2种可能，1当前节第一页前面的Padding，2在上一节里
    begin
      Result := i;
      Break;
    end;
  end;

  if (Result < 0) and (AVOffset > vPos) then  // 同节最后一页下面，按下一页里面
    Result := FPages.Count - 1;

  Assert(Result >= 0, '没有获取到正确的页序号！');
end;

procedure THCCustomSection.GetPageCaretInfo(var ACaretInfo: THCCaretInfo);
var
  vMarginLeft, vMarginRight, vPageIndex: Integer;
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
    if FActiveData = FPageData then  // 页面变动，需要判断光标处所在页
    begin
      vMarginLeft := GetPageIndexByFormat(ACaretInfo.Y);  // 借用变量vMarginLeft表示页序号
      if vPageIndex <> vMarginLeft then  // 表格第一行一部分跨页时，点击下一页同行无内容的单元格光标回到上一页
      begin
        vPageIndex := vMarginLeft;
        SetActivePageIndex(vPageIndex);
      end;
    end;

    GetPageMarginLeftAndRight(vPageIndex, vMarginLeft, vMarginRight);
    ACaretInfo.X := ACaretInfo.X + vMarginLeft;
    ACaretInfo.Y := ACaretInfo.Y + GetPageTopFilm(vPageIndex);

    if FActiveData = FHeader then
      ACaretInfo.Y := ACaretInfo.Y + GetHeaderPageDrawTop  // 页在节中的Top位置
    else
    if FActiveData = FPageData then
      ACaretInfo.Y := ACaretInfo.Y + GetHeaderAreaHeight - GetPageDataFmtTop(vPageIndex)  // - 页起始数据在Data中的位置
    else
    if FActiveData = FFooter then
      ACaretInfo.Y := ACaretInfo.Y + FPageSize.PageHeightPix - FPageSize.PageMarginBottomPix;
  end;
end;

function THCCustomSection.GetPageCount: Integer;
begin
  Result := FPages.Count;  // 所有页面
end;

function THCCustomSection.GetPageDataFmtTop(const APageIndex: Integer): Integer;
var
  i, vContentHeight: Integer;
begin
  Result := 0;
  if APageIndex > 0 then
  begin
    vContentHeight := GetContentHeight;

    for i := 0 to APageIndex - 1 do
      Result := Result + vContentHeight;
  end;
end;

function THCCustomSection.GetPageHeightPix: Integer;
begin
  Result := FPageSize.PageHeightPix;
end;

function THCCustomSection.GetPageMarginBottomPix: Integer;
begin
  Result := FPageSize.PageMarginBottomPix;
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
  if FSymmetryMargin and Odd(APageIndex) then
  begin
    AMarginLeft := FPageSize.PageMarginRightPix;
    AMarginRight := FPageSize.PageMarginLeftPix;
  end
  else
  begin
    AMarginLeft := FPageSize.PageMarginLeftPix;
    AMarginRight := FPageSize.PageMarginRightPix;
  end;
end;

function THCCustomSection.GetPageMarginLeftPix: Integer;
begin
  Result := FPageSize.PageMarginLeftPix;
end;

function THCCustomSection.GetPageMarginRightPix: Integer;
begin
  Result := FPageSize.PageMarginRightPix;
end;

function THCCustomSection.GetPageMarginTopPix: Integer;
begin
  Result := FPageSize.PageMarginTopPix;
end;

function THCCustomSection.GetPageTopFilm(const APageIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := PagePadding;
  for i := 0 to APageIndex - 1 do
    Result := Result + FPageSize.PageHeightPix + PagePadding;  // 每一页和其上面的分隔计为一整个处理单元
end;

function THCCustomSection.GetPageWidthPix: Integer;
begin
  Result := FPageSize.PageWidthPix;
end;

function THCCustomSection.GetPaperHeight: Single;
begin
  Result := FPageSize.PaperHeight;
end;

function THCCustomSection.GetPaperMarginBottom: Single;
begin
  Result := FPageSize.PaperMarginBottom;
end;

function THCCustomSection.GetPaperMarginLeft: Single;
begin
  Result := FPageSize.PaperMarginLeft;
end;

function THCCustomSection.GetPaperMarginRight: Single;
begin
  Result := FPageSize.PaperMarginRight;
end;

function THCCustomSection.GetPaperMarginTop: Single;
begin
  Result := FPageSize.PaperMarginTop;
end;

function THCCustomSection.GetPaperSize: Integer;
begin
  Result := FPageSize.PaperSize;
end;

function THCCustomSection.GetPaperWidth: Single;
begin
  Result := FPageSize.PaperWidth;
end;

function THCCustomSection.InsertAnnotate(const ATitle, AText: string): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertAnnotate(ATitle, AText);
    end);
end;

function THCCustomSection.InsertBreak: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertBreak;
    end);
end;

function THCCustomSection.InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertDomain(AMouldDomain);
    end);
end;

function THCCustomSection.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertItem(AIndex, AItem);
    end);
end;

function THCCustomSection.InsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertItem(AItem);
    end);
end;

function THCCustomSection.InsertLine(const ALineHeight: Integer): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertLine(ALineHeight);
    end);
end;

function THCCustomSection.InsertPageBreak: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FPageData.InsertPageBreak;
    end);
end;

function THCCustomSection.ActiveDataChangeByAction(const AFunction: THCFunction): Boolean;
begin
  if not FActiveData.CanEdit then Exit(False);
  if FActiveData.FloatItemIndex >= 0 then Exit(False);

  Result := AFunction;  // 处理变动

  if FActiveData.FormatHeightChange or FActiveData.FormatDrawItemChange then  // 数据高度变化了
  begin
    if FActiveData = FPageData then
      BuildSectionPages(FActiveData.FormatStartDrawItemNo)
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
  ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertStream(AStream, AStyle, AFileVersion);
      vResult := Result;
    end);
  Result := vResult;
end;

function THCCustomSection.InsertTable(const ARowCount, AColCount: Integer): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertTable(ARowCount, AColCount);
    end);
end;

function THCCustomSection.InsertText(const AText: string): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertText(AText);
    end);
end;

procedure THCCustomSection.KeyDown(var Key: Word; Shift: TShiftState);
var
  vKey: Word;
begin
  if not FActiveData.CanEdit then Exit;

  if FActiveData.KeyDownFloatItem(Key, Shift) then  // FloatItem使用了按键
  begin
    DoActiveDataCheckUpdateInfo;
    Exit;
  end;

  if IsKeyDownWant(Key) then
  begin
    vKey := Key;
    case Key of
      VK_BACK, VK_DELETE, VK_RETURN, VK_TAB:
        begin
          ActiveDataChangeByAction(function(): Boolean
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
    ActiveDataChangeByAction(function(): Boolean
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
    AStream.ReadBuffer(FPageOrientation, SizeOf(FPageOrientation));  // 纸张方向
    AStream.ReadBuffer(FPageNoVisible, SizeOf(FPageNoVisible));  // 是否显示页码
  end;

  FPageSize.LoadToStream(AStream, AFileVersion);  // 页面参数
  FPageData.Width := GetContentWidth;

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
    FHeader.Width := FPageData.Width;
    FHeader.LoadFromStream(AStream, FStyle, AFileVersion);
  end;

  if saFooter in vLoadParts then
  begin
    FFooter.Width := FPageData.Width;
    FFooter.LoadFromStream(AStream, FStyle, AFileVersion);
  end;

  if saPage in vLoadParts then
    FPageData.LoadFromStream(AStream, FStyle, AFileVersion);

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
    FPageData.MarkStyleUsed(AMark);
end;

function THCCustomSection.MergeTableSelectCells: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.MergeTableSelectCells;
    end);
end;

procedure THCCustomSection.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vOldTopData: THCRichData;
  vX, vY, vPageIndex: Integer;
  vNewActiveData: THCSectionData;
  vChangeActiveData: Boolean;
begin
  vChangeActiveData := False;
  vOldTopData := FActiveData.GetTopLevelData;
  vPageIndex := GetPageIndexByFilm(Y);  // 鼠标点击处所在的页(和光标所在页可能并不是同一页，如表格跨页时，空单元格第二页点击时，光标回前一页)
  if FActivePageIndex <> vPageIndex then
    SetActivePageIndex(vPageIndex);

  {$REGION ' 有FloatItem时短路 '}
  if FActiveData.FloatItems.Count > 0 then  // 有FloatItem时优先
  begin
    SectionCoordToPage(FActivePageIndex, X, Y, vX, vY);
    PageCoordToData(FActivePageIndex, FActiveData, vX, vY);
    if FActiveData = FPageData then  // FloatItem在PageData中
      vY := vY + GetPageDataFmtTop(FActivePageIndex);

    if FActiveData.MouseDownFloatItem(Button, Shift, vX, vY) then Exit;
  end;
  {$ENDREGION}

  SectionCoordToPage(FActivePageIndex, X, Y, vX, vY);  // X，Y转换到指定页的坐标vX,vY

  vNewActiveData := GetSectionDataAt(vX, vY);

  if (vNewActiveData <> FActiveData) and (ssDouble in Shift) then  // 双击、新的Data
  begin
    SetActiveData(vNewActiveData);
    vChangeActiveData := True;
  end;

  if (vNewActiveData <> FActiveData) and (FActiveData = FPageData) then  // 激活正文，点击页眉、页脚
    PageCoordToData(FActivePageIndex, FActiveData, vX, vY, True)  // 约束到Data中，防止点页脚认为是下一页
  else
    PageCoordToData(FActivePageIndex, FActiveData, vX, vY);

  if FActiveData = FPageData then
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
  if X > FPageSize.PageWidthPix - vMarginRight then
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
        FActiveData.ActiveFloatItem.PageIndex := FMousePageIndex;
    end;

    if FActiveData = FPageData then  // FloatItem在PageData中
    begin
      if (FActiveData.FloatItemIndex >= 0) and (FActiveData.ActiveFloatItem.Resizing) then  // 缩放时以所在页为标准
      begin
        SectionCoordToPage(FActiveData.ActiveFloatItem.PageIndex, X, Y, vX, vY);
        PageCoordToData(FActiveData.ActiveFloatItem.PageIndex, FActiveData, vX, vY);
        vY := vY + GetPageDataFmtTop(FActiveData.ActiveFloatItem.PageIndex);
      end
      else
      begin
        SectionCoordToPage(FMousePageIndex, X, Y, vX, vY);
        PageCoordToData(FMousePageIndex, FActiveData, vX, vY);
        vY := vY + GetPageDataFmtTop(FMousePageIndex);
      end;
    end
    else  // FloatItem在Header或Footer
    begin
      if (FActiveData.FloatItemIndex >= 0) and (FActiveData.ActiveFloatItem.Resizing) then  // 缩放时以所在页为标准
      begin
        SectionCoordToPage(FActivePageIndex, X, Y, vX, vY);
        PageCoordToData(FActivePageIndex, FActiveData, vX, vY);
      end
      else
      begin
        SectionCoordToPage(FMousePageIndex, X, Y, vX, vY);
        PageCoordToData(FMousePageIndex, FActiveData, vX, vY);
      end;
    end;

    if FActiveData.MouseMoveFloatItem(Shift, vX, vY) then Exit;
  end;
  {$ENDREGION}

  SectionCoordToPage(FMousePageIndex, X, Y, vX, vY);

  vMoveData := GetSectionDataAt(vX, vY);
  if vMoveData <> FMoveData then
  begin
    if FMoveData <> nil then
      FMoveData.MouseLeave;
    FMoveData := vMoveData;
  end;

  PageCoordToData(FMousePageIndex, FActiveData, vX, vY, FActiveData.Selecting);  // 划选时约束到Data中

  if FActiveData = FPageData then
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
    if FActiveData = FPageData then  // FloatItem在PageData中
    begin
      SectionCoordToPage(FActiveData.ActiveFloatItem.PageIndex, X, Y, vX, vY);
      PageCoordToData(FActiveData.ActiveFloatItem.PageIndex, FActiveData, vX, vY);
      vY := vY + GetPageDataFmtTop(FActiveData.ActiveFloatItem.PageIndex);
    end
    else  // FloatItem在Header或Footer
    begin
      SectionCoordToPage(vPageIndex, X, Y, vX, vY);
      PageCoordToData(vPageIndex, FActiveData, vX, vY);
    end;

    if FActiveData.MouseUpFloatItem(Button, Shift, vX, vY) then Exit;
  end;
  {$ENDREGION}

  SectionCoordToPage(vPageIndex, X, Y, vX, vY);

  //if  <> FActiveData then Exit;  // 不在当前激活的Data上
  if (GetSectionDataAt(vX, vY) <> FActiveData) and (FActiveData = FPageData) then  // 激活正文，点击页眉、页脚
    PageCoordToData(vPageIndex, FActiveData, vX, vY, True)
  else
    PageCoordToData(vPageIndex, FActiveData, vX, vY);

  if FActiveData = FPageData then
    vY := vY + GetPageDataFmtTop(vPageIndex);

  // RectItem的缩放在MouseUp中处理，所以需要判断是否需要改变
  if FActiveData.SelectedResizing then
  begin
    ActiveDataChangeByAction(function(): Boolean
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

procedure THCCustomSection.PageCoordToData(const APageIndex: Integer;
  const AData: THCViewData; var AX, AY: Integer; const ARestrain: Boolean = False);
var
  viTemp, vMarginLeft, vMarginRight: Integer;
begin
  GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);
  AX := AX - vMarginLeft;
  if ARestrain then  // 为避免左右边界，不往里约束1，否则无法点到行首光标，尤其是行首是RectItem
  begin
    if AX < 0 then
      AX := 0
    else
    begin
      viTemp := FPageSize.PageWidthPix - vMarginLeft - vMarginRight;
      if AX > viTemp then
        AX := viTemp;
    end;
  end;

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
    AY := AY - FPageSize.PageHeightPix + FPageSize.PageMarginBottomPix;
    if ARestrain then
    begin
      if AY < 0 then
        AY := 1
      else
      if AY > FPageSize.PageMarginBottomPix then
        AY := FPageSize.PageMarginBottomPix - 1;
    end;
  end
  else
  if AData = FPageData then  // 约束到正文绝对区域中
  begin
    //viTemp := GetHeaderAreaHeight;
    AY := AY - GetHeaderAreaHeight;
    if ARestrain then  // 为避免上一页脚下一页眉边界不确定是上还是下，约束后都偏移1
    begin
      if AY < 0 then
        AY := 1  // 处理激活正文，在页眉页脚中点击
      else
      begin
        viTemp := FPageSize.PageHeightPix - GetHeaderAreaHeight - FPageSize.PageMarginBottomPix;
        if AY > viTemp then
          AY := viTemp - 1;
      end;
    end;
  end;
end;

procedure THCCustomSection.PaintDisplayPage(const AFilmOffsetX, AFilmOffsetY: Integer;
  const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  i, vPageDrawTop, vPageFilmTop: Integer;
begin
  //vPageDrawLeft := AFilmOffsetX;
  //vHeaderAreaHeight := GetHeaderAreaHeight;  // 页眉区域实际高(内容高度>上边距时取内容高度)
  for i := FDisplayFirstPageIndex to FDisplayLastPageIndex do
  begin
    APaintInfo.PageIndex := i;
    vPageFilmTop := GetPageTopFilm(i);
    vPageDrawTop := vPageFilmTop - AFilmOffsetY;  // 映射到当前页面为原点的屏显起始位置(可为负数)
    PaintPage(i, AFilmOffsetX, vPageDrawTop, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.PaintPage(const APageIndex, ALeft, ATop: Integer;
  const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  vHeaderAreaHeight, vMarginLeft, vMarginRight,
  vPageDrawLeft, vPageDrawRight, vPageDrawTop, vPageDrawBottom,  // 页区域各位置
  vPageDataScreenTop, vPageDataScreenBottom,  // 页数据屏幕位置
  vScaleWidth, vScaleHeight: Integer;

  {$REGION ' 绘制页眉数据 '}
  procedure PaintHeader;
  var
    vHeaderDataDrawTop, vHeaderDataDrawBottom, vDCState: Integer;
  begin
    vHeaderDataDrawTop := vPageDrawTop + GetHeaderPageDrawTop;
    vHeaderDataDrawBottom := vPageDrawTop + vHeaderAreaHeight;

    FHeader.PaintData(vPageDrawLeft + vMarginLeft, vHeaderDataDrawTop,
      vHeaderDataDrawBottom, Max(vHeaderDataDrawTop, 0),
      Min(vHeaderDataDrawBottom, APaintInfo.WindowHeight), 0, ACanvas, APaintInfo);

    if (not APaintInfo.Print) and (FActiveData = FHeader) then  // 当前激活的是页眉，绘制页眉激活线
    begin
      ACanvas.Pen.Color := clBlue;
      ACanvas.MoveTo(vPageDrawLeft, vHeaderDataDrawBottom - 1);
      ACanvas.LineTo(vPageDrawRight, vHeaderDataDrawBottom - 1);
    end;

    if Assigned(FOnPaintHeader) then
    begin
      vDCState := Windows.SaveDC(ACanvas.Handle);
      try
        FOnPaintHeader(Self, APageIndex, Rect(vPageDrawLeft + vMarginLeft, vHeaderDataDrawTop,
          vPageDrawRight - vMarginRight, vHeaderDataDrawBottom), ACanvas, APaintInfo);
      finally
        Windows.RestoreDC(ACanvas.Handle, vDCState);
        ACanvas.Refresh;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' 绘制页脚数据 '}
  procedure PaintFooter;
  var
    vFooterDataDrawTop, vDCState: Integer;
  begin
    vFooterDataDrawTop := vPageDrawBottom - FPageSize.PageMarginBottomPix;
    FFooter.PaintData(vPageDrawLeft + vMarginLeft, vFooterDataDrawTop, vPageDrawBottom,
      Max(vFooterDataDrawTop, 0), Min(vPageDrawBottom, APaintInfo.WindowHeight), 0, ACanvas, APaintInfo);

    if (not APaintInfo.Print) and (FActiveData = FFooter) then  // 当前激活的是页脚，绘制激活线
    begin
      ACanvas.Pen.Color := clBlue;
      ACanvas.MoveTo(vPageDrawLeft, vFooterDataDrawTop);
      ACanvas.LineTo(vPageDrawRight, vFooterDataDrawTop);
    end;

    if Assigned(FOnPaintFooter) then
    begin
      vDCState := Windows.SaveDC(ACanvas.Handle);
      try
        FOnPaintFooter(Self, APageIndex, Rect(vPageDrawLeft + vMarginLeft, vFooterDataDrawTop,
          vPageDrawRight - vMarginRight, vPageDrawBottom), ACanvas, APaintInfo);
      finally
        Windows.RestoreDC(ACanvas.Handle, vDCState);
        ACanvas.Refresh;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' 绘制页面数据 '}
  procedure PaintPageData;
  var
    vDCState: Integer;
  begin
    if (FPages[APageIndex].StartDrawItemNo < 0) or (FPages[APageIndex].EndDrawItemNo < 0) then
      Exit;

//    FPageData.CheckAnnotate(vPageDrawLeft + vMarginLeft,
//      vPageDrawTop + vHeaderAreaHeight - APaintInfo.PageDataFmtTop,
//      FPages[APageIndex].StartDrawItemNo, FPages[APageIndex].EndDrawItemNo,
//      APaintInfo.PageDataFmtTop,
//      APaintInfo.PageDataFmtTop + PageHeightPix - vHeaderAreaHeight - PageMarginBottomPix);

    { 绘制数据，把Data中指定位置的数据，绘制到指定的页区域中，并按照可显示出来的区域约束 }
    FPageData.PaintData(vPageDrawLeft + vMarginLeft,  // 当前页数据要绘制到的Left
      vPageDrawTop + vHeaderAreaHeight,     // 当前页数据要绘制到的Top
      vPageDrawBottom - PageMarginBottomPix,  // 当前页数据要绘制的Bottom
      vPageDataScreenTop,     // 界面呈现当前页数据的Top位置
      vPageDataScreenBottom,  // 界面呈现当前页数据Bottom位置
      APaintInfo.PageDataFmtTop,  // 指定从哪个位置开始的数据绘制到页数据起始位置
      FPages[APageIndex].StartDrawItemNo,
      FPages[APageIndex].EndDrawItemNo,
      ACanvas,
      APaintInfo);

    if Assigned(FOnPaintPage) then
    begin
      vDCState := Windows.SaveDC(ACanvas.Handle);
      try
        FOnPaintPage(Self, APageIndex, Rect(vPageDrawLeft + vMarginLeft,
          vPageDrawTop + vHeaderAreaHeight, vPageDrawRight - vMarginRight,
          vPageDrawBottom - PageMarginBottomPix), ACanvas, APaintInfo);
      finally
        Windows.RestoreDC(ACanvas.Handle, vDCState);
        ACanvas.Refresh;
      end;
    end;
  end;
  {$ENDREGION}

var
  vX, vY: Integer;
  vPaintRegion: HRGN;
  vClipBoxRect: TRect;
begin
  vScaleWidth := Round(APaintInfo.WindowWidth / APaintInfo.ScaleX);
  vScaleHeight := Round(APaintInfo.WindowHeight / APaintInfo.ScaleY);

  vPageDrawLeft := ALeft;
  vPageDrawRight := vPageDrawLeft + FPageSize.PageWidthPix;

  vHeaderAreaHeight := GetHeaderAreaHeight;  // 页眉区域实际高 = 页眉数据顶部偏移 + 内容高度，大于上边距时以此为准
  GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);  // 获取页左右边距绘制位置

  vPageDrawTop := ATop;  // 映射到当前页面左上角为原点的起始位置(可为负数)
  vPageDrawBottom := vPageDrawTop + FPageSize.PageHeightPix;  // 页面结束位置(可为负数)
  // 当前页数据能显示出来的区域边界
  vPageDataScreenTop := Max(vPageDrawTop + vHeaderAreaHeight, 0);
  vPageDataScreenBottom := Min(vPageDrawBottom - FPageSize.PageMarginBottomPix, vScaleHeight);
  { 当前页在当前屏显出来的数据边界映射到格式化中的边界 }
  APaintInfo.PageDataFmtTop := GetPageDataFmtTop(APageIndex);
  GetClipBox(ACanvas.Handle, vClipBoxRect);  // 保存当前的绘图区域

  if not APaintInfo.Print then  // 非打印时绘制的内容
  begin

    {$REGION ' 非打印时填充页面背景 '}
    ACanvas.Brush.Color := FStyle.BackgroudColor;
    ACanvas.FillRect(Rect(vPageDrawLeft, vPageDrawTop,
      Min(vPageDrawRight, vScaleWidth),  // 约束边界
      Min(vPageDrawBottom, vScaleHeight)));
    {$ENDREGION}

    {$REGION ' 页眉边距指示符 '}
    if vPageDrawTop + vHeaderAreaHeight > 0 then  // 页眉可显示
    begin
      vY := vPageDrawTop + vHeaderAreaHeight;
      if vHeaderAreaHeight > FPageSize.PageMarginTopPix then  // 页眉数据超出页上边距
      begin
        ACanvas.Pen.Style := TPenStyle.psDot;
        ACanvas.Pen.Color := clGray;
        APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawLeft, vY - 1),
          Point(vPageDrawRight, vY - 1)]);
      end;

      ACanvas.Pen.Width := 1;
      ACanvas.Pen.Color := clGray;
      ACanvas.Pen.Style := TPenStyle.psSolid;

      // 左上， 左-原-上
      vX := vPageDrawLeft + vMarginLeft;
      vY := vPageDrawTop + FPageSize.PageMarginTopPix;
      APaintInfo.DrawNoScaleLine(ACanvas, [Point(vX - PMSLineHeight, vY),
        Point(vX, vY), Point(vX, vY - PMSLineHeight)]);
      // 右上，右-原-上
      vX := vPageDrawLeft + FPageSize.PageWidthPix - vMarginRight;
      APaintInfo.DrawNoScaleLine(ACanvas, [Point(vX + PMSLineHeight, vY),
        Point(vX, vY), Point(vX, vY - PMSLineHeight)]);
    end;
    {$ENDREGION}

    {$REGION ' 页脚边距指示符 '}
    vY := vPageDrawBottom - FPageSize.PageMarginBottomPix;
    if vY < APaintInfo.WindowHeight then  // 页脚可显示
    begin
      ACanvas.Pen.Width := 1;
      ACanvas.Pen.Color := clGray;
      ACanvas.Pen.Style := TPenStyle.psSolid;

      // 左下，左-原-下
      vX := vPageDrawLeft + vMarginLeft;
      APaintInfo.DrawNoScaleLine(ACanvas, [Point(vX - PMSLineHeight, vY),
        Point(vX, vY), Point(vX, vY + PMSLineHeight)]);
      // 右下，右-原-下
      vX := vPageDrawRight - vMarginRight;
      APaintInfo.DrawNoScaleLine(ACanvas, [Point(vX + PMSLineHeight, vY),
        Point(vX, vY), Point(vX, vY + PMSLineHeight)]);
    end;
    {$ENDREGION}

  end;

  if Assigned(FOnPaintWholePageBefor) then  // 公开页面绘制前事件
  begin
    FOnPaintWholePageBefor(Self, APageIndex,
      Rect(vPageDrawLeft, vPageDrawTop, vPageDrawRight, vPageDrawBottom),
      ACanvas, APaintInfo);
  end;

  {$REGION ' 绘制页眉 '}
  if vPageDrawTop + vHeaderAreaHeight > 0 then  // 页眉可显示
  begin
    vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPageDrawLeft),
      Max(APaintInfo.GetScaleY(vPageDrawTop + FHeaderOffset), 0),
      APaintInfo.GetScaleX(vPageDrawRight),
      Min(APaintInfo.GetScaleY(vPageDrawTop + vHeaderAreaHeight), APaintInfo.WindowHeight));

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
  if APaintInfo.GetScaleY(vPageDrawBottom - FPageSize.PageMarginBottomPix) < APaintInfo.WindowHeight then  // 页脚可显示
  begin
    vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPageDrawLeft),
      Max(APaintInfo.GetScaleY(vPageDrawBottom - FPageSize.PageMarginBottomPix), 0),
      APaintInfo.GetScaleX(vPageDrawRight),
      Min(APaintInfo.GetScaleY(vPageDrawBottom), APaintInfo.WindowHeight));

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

  {$REGION ' 绘制页面 '}
  if vPageDataScreenBottom > vPageDataScreenTop then  // 能露出数据则绘制当前页，绘制正文
  begin
    vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPageDrawLeft),
      APaintInfo.GetScaleY(Max(vPageDrawTop + vHeaderAreaHeight, vPageDataScreenTop)),
      APaintInfo.GetScaleX(vPageDrawRight),
      // 底部让出1像素，否则表格底部边框和数据绘制底部一样时，边框绘制不出来。Rgn比Rect约束了1像素？
      APaintInfo.GetScaleY(Min(vPageDrawBottom - FPageSize.PageMarginBottomPix, vPageDataScreenBottom)) + 1);
    try
      SelectClipRgn(ACanvas.Handle, vPaintRegion);  // 设置绘制有效区域
      PaintPageData;
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

  // 恢复区域，准备给整页绘制用(各部分浮动Item)
  vPaintRegion := CreateRectRgn(
    APaintInfo.GetScaleX(vPageDrawLeft),
    APaintInfo.GetScaleX(vPageDrawTop),
    APaintInfo.GetScaleX(vPageDrawRight),
    APaintInfo.GetScaleX(vPageDrawBottom));
  try
    SelectClipRgn(ACanvas.Handle, vPaintRegion);

    FHeader.PaintFloatItems(APageIndex, vPageDrawLeft + vMarginLeft,
      vPageDrawTop + GetHeaderPageDrawTop,
      //vPageDrawTop + vHeaderAreaHeight,
      //Max(vPageDrawTop + GetHeaderPageDrawTop, 0),
      //Min(vPageDrawTop + vHeaderAreaHeight, APaintInfo.WindowHeight),
      0, ACanvas, APaintInfo);

    FFooter.PaintFloatItems(APageIndex, vPageDrawLeft + vMarginLeft,
      vPageDrawBottom - FPageSize.PageMarginBottomPix,
      //vPageDrawBottom,
      //Max(vPageDrawBottom - FPageSize.PageMarginBottomPix, 0),
      //Min(vPageDrawBottom, APaintInfo.WindowHeight),
      0, ACanvas, APaintInfo);

    FPageData.PaintFloatItems(APageIndex, vPageDrawLeft + vMarginLeft,  // 当前页绘制到的Left
      vPageDrawTop + vHeaderAreaHeight,     // 当前页绘制到的Top
      //vPageDrawBottom - PageMarginBottomPix,  // 当前页绘制的Bottom
      //vPageDataScreenTop,     // 界面呈现当前页的Top位置
      //vPageDataScreenBottom,  // 界面呈现当前页的Bottom位置
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

  if Assigned(FOnPaintWholePageAfter) then  // 公开页面绘制后事件
  begin
    FOnPaintWholePageAfter(Self, APageIndex,
      Rect(vPageDrawLeft, vPageDrawTop, vPageDrawRight, vPageDrawBottom),
      ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.BuildSectionPages(const AStartDrawItemNo: Integer);
var
  vPageIndex, vPageDataFmtTop, vPageDataFmtBottom, vContentHeight: Integer;

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
      if FPageData.GetDrawItemStyle(ADrawItemNo) = THCStyle.PageBreak then
      begin
        vFmtOffset := vPageDataFmtBottom - FPageData.DrawItems[ADrawItemNo].Rect.Top;

        vSuplus := vSuplus + vFmtOffset;
        if vFmtOffset > 0 then  // 整体向下移动了
          OffsetRect(FPageData.DrawItems[ADrawItemNo].Rect, 0, vFmtOffset);

        vPageDataFmtTop := vPageDataFmtBottom;
        vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;

        _FormatNewPage(ADrawItemNo - 1, ADrawItemNo);  // 新建页
      end
      else
      if FPageData.DrawItems[ADrawItemNo].Rect.Bottom > vPageDataFmtBottom then  // 当前页放不下表格整体(带行间距)
      begin
        if (FPages[vPageIndex].StartDrawItemNo = ADrawItemNo)
          and (AStartSeat = 0)
          and (not vRectItem.CanPageBreak)
        then  // 当前页从头开始整页放不下，也不允许截断，强制变矮当前页能显示多少显示多少
        begin
          vFmtHeightInc := vPageDataFmtBottom - FPageData.DrawItems[ADrawItemNo].Rect.Bottom;
          vSuplus := vSuplus + vFmtHeightInc;
          FPageData.DrawItems[ADrawItemNo].Rect.Bottom :=  // 扩充格式化高度
            FPageData.DrawItems[ADrawItemNo].Rect.Bottom + vFmtHeightInc;
          vRectItem.Height := vRectItem.Height + vFmtHeightInc;  // 是在这里处理呢还是在RectItem内部更合适？

          Exit;
        end;

        vDrawRect := FPageData.DrawItems[ADrawItemNo].Rect;

        //if vSuplus = 0 then  // 第一次计算分页
        InflateRect(vDrawRect, 0, -FPageData.GetLineBlankSpace(ADrawItemNo) div 2);  // 减掉行间距，为了达到去掉行间距能放下不换页的效果

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
          vFmtOffset := vFmtOffset + FPageData.GetLineBlankSpace(ADrawItemNo) div 2;  // 整体向下移动增加上面减掉的行间距
          vSuplus := vSuplus + vFmtOffset + vFmtHeightInc;

          OffsetRect(FPageData.DrawItems[ADrawItemNo].Rect, 0, vFmtOffset);

          vPageDataFmtTop := vPageDataFmtBottom;
          vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;
          _FormatNewPage(ADrawItemNo - 1, ADrawItemNo);  // 新建页
          _RectItemCheckPage(vBreakSeat);
        end
        else  // 跨页，但未整体下移
        begin
          vSuplus := vSuplus{ + vFmtOffset} + vFmtHeightInc;
          FPageData.DrawItems[ADrawItemNo].Rect.Bottom :=  // 扩充格式化高度
            FPageData.DrawItems[ADrawItemNo].Rect.Bottom + vFmtHeightInc;
          vRectItem.Height := vRectItem.Height + vFmtHeightInc;  // 是在这里处理呢还是在RectItem内部更合适？

          vPageDataFmtTop := vPageDataFmtBottom;
          vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;
          _FormatNewPage(ADrawItemNo, ADrawItemNo);  // 新建页
          _RectItemCheckPage(vBreakSeat);  // 从分页位置后面继续检查是否分页
        end;
      end;
    end;
    {$ENDREGION}

  var
    i: Integer;
  begin
    vRectItem := FPageData.Items[FPageData.DrawItems[ADrawItemNo].ItemNo] as THCCustomRectItem;
    vSuplus := 0;
    vBreakSeat := 0;

    vRectItem.CheckFormatPageBreakBefor;
    _RectItemCheckPage(0);  // 从最开始位置，检测表格各行内容是否能显示在当前页

    if vSuplus <> 0 then
    begin
      for i := ADrawItemNo + 1 to FPageData.DrawItems.Count - 1 do
        OffsetRect(FPageData.DrawItems[i].Rect, 0, vSuplus);
    end;
  end;
  {$ENDREGION}

  {$REGION '_FormatTextItemCheckPageBreak'}
  procedure _FormatTextItemCheckPageBreak(const ADrawItemNo: Integer);
  var
    i, vH: Integer;
  begin
    //if not DrawItems[ADrawItemNo].LineFirst then Exit; // 注意如果文字环绕时这里就不能只判断行第1个
    if FPageData.DrawItems[ADrawItemNo].Rect.Bottom > vPageDataFmtBottom then
    begin
      vH := vPageDataFmtBottom - FPageData.DrawItems[ADrawItemNo].Rect.Top;
      for i := ADrawItemNo to FPageData.DrawItems.Count - 1 do
        OffsetRect(FPageData.DrawItems[i].Rect, 0, vH);

      vPageDataFmtTop := vPageDataFmtBottom;
      vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;
      _FormatNewPage(ADrawItemNo - 1, ADrawItemNo); // 新建页
    end;
  end;
  {$ENDREGION}

var
  i, vPrioDrawItemNo: Integer;
  vPage: THCPage;
begin
  // 上一行所在页作为格式化起始页
  vPrioDrawItemNo := AStartDrawItemNo; // FPageData.GetItemLastDrawItemNo(AStartItemNo - 1)  // 上一个最后的DItem
  while vPrioDrawItemNo > 0 do
  begin
    if FPageData.DrawItems[vPrioDrawItemNo].LineFirst then
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

//  // 因为行首可能是分页，所以需要从行首开始判断跨页
//  for i := FPageData.Items[AStartItemNo].FirstDItemNo downto 0 do
//  begin
//    if FPageData.DrawItems[i].LineFirst then
//    begin
//      vPrioDrawItemNo := i;
//      Break;
//    end;
//  end;

//  if vPrioDrawItemNo = FPages[vPageIndex].StartDrawItemNo then  // 行首是页的第一个DrawItem
//  begin
//    FPages.DeleteRange(vPageIndex, FPages.Count - vPageIndex);  // 删除当前页一直到最后
//
//    // 从上一页最后开始计算分页
//    Dec(vPageIndex);
//    if vPageIndex >= 0 then
//      FPages[vPageIndex].EndDrawItemNo := -1;
//  end
//  else  // 行首不是页的第一个DrawItem
    FPages.DeleteRange(vPageIndex + 1, FPages.Count - vPageIndex - 1);  // 删除当前页后面的，准备格式化

  if FPages.Count = 0 then  // 删除没了，补充第一个Page
  begin
    vPage := THCPage.Create;
    vPage.StartDrawItemNo := 0;
    FPages.Add(vPage);
    vPageIndex := 0;
  end;

  vPageDataFmtTop := GetPageDataFmtTop(vPageIndex);
  vContentHeight := GetContentHeight;
  vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;

  for i := vPrioDrawItemNo + 1 to FPageData.DrawItems.Count - 1 do
  begin
    if FPageData.DrawItems[i].LineFirst then
    begin
      if FPageData.Items[FPageData.DrawItems[i].ItemNo].StyleNo < THCStyle.Null then
        _FormatRectItemCheckPageBreak(i)
      else
        _FormatTextItemCheckPageBreak(i);
    end;
  end;

  FPages[vPageIndex].EndDrawItemNo := FPageData.DrawItems.Count - 1;
  SetActivePageIndex(GetPageIndexByCurrent);

  for i := FPageData.FloatItems.Count - 1 downto 0 do  // 正文中删除页序号超过页总数的FloatItem
  begin
    if FPageData.FloatItems[i].PageIndex > FPages.Count - 1 then
      FPageData.FloatItems.Delete(i);
  end;
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

    ActiveDataChangeByAction(function(): Boolean
      begin
        FActiveData.Redo(ARedo);
      end);
  end
  else
    (ARedo.Data as THCSectionData).Redo(ARedo);
end;

procedure THCCustomSection.ReFormatActiveItem;
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ReFormatActiveItem;
    end);
end;

procedure THCCustomSection.ReFormatActiveParagraph;
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ReFormatActiveParagraph;
    end);
end;

procedure THCCustomSection.ResetMargin;
begin
  FPageData.Width := GetContentWidth;

  FHeader.Width := FPageData.Width;
  FFooter.Width := FPageData.Width;

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

    AStream.WriteBuffer(FPageOrientation, SizeOf(FPageOrientation));  // 纸张方向
    AStream.WriteBuffer(FPageNoVisible, SizeOf(FPageNoVisible));  // 是否显示页码

    FPageSize.SaveToStream(AStream);  // 页面参数

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
      FPageData.SaveToStream(AStream);
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
  Result := FPageData.SaveToText;
end;

procedure THCCustomSection.SectionCoordToPage(const APageIndex, X, Y: Integer; var APageX,
  APageY: Integer);
var
  vPageFilmTop{, vMarginLeft, vMarginRight}: Integer;
begin
  // 预留页横向排版时计算
  //GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);
  APageX := X;// - vMarginLeft;

  vPageFilmTop := GetPageTopFilm(APageIndex);
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

procedure THCCustomSection.SetPageOrientation(const Value: TPageOrientation);
var
  vfW: Single;
begin
  if FPageOrientation <> Value then
  begin
    FPageOrientation := Value;

    vfW := FPageSize.PaperWidth;
    FPageSize.PaperWidth := FPageSize.PaperHeight;
    FPageSize.PaperHeight := vfW;
  end;
end;

procedure THCCustomSection.SetPaperHeight(const Value: Single);
begin
  FPageSize.PaperHeight := Value;
  FPageSize.PaperSize := DMPAPER_USER;
end;

procedure THCCustomSection.SetPaperMarginBottom(const Value: Single);
begin
  FPageSize.PaperMarginBottom := Value;
end;

procedure THCCustomSection.SetPaperMarginLeft(const Value: Single);
begin
  FPageSize.PaperMarginLeft := Value;
end;

procedure THCCustomSection.SetPaperMarginRight(const Value: Single);
begin
  FPageSize.PaperMarginRight := Value;
end;

procedure THCCustomSection.SetPaperMarginTop(const Value: Single);
begin
  FPageSize.PaperMarginTop := Value;
end;

procedure THCCustomSection.SetPaperSize(const Value: Integer);
begin
  FPageSize.PaperSize := Value;
end;

procedure THCCustomSection.SetPaperWidth(const Value: Single);
begin
  FPageSize.PaperWidth := Value;
  FPageSize.PaperSize := DMPAPER_USER;
end;

procedure THCCustomSection.SetReadOnly(const Value: Boolean);
begin
  FHeader.ReadOnly := Value;
  FFooter.ReadOnly := Value;
  FPageData.ReadOnly := Value;
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

    ActiveDataChangeByAction(function(): Boolean
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
  Result := ActiveDataChangeByAction(function(): Boolean
    var
      vHtmlFmt: THCHtmlFormat;
    begin
      vHtmlFmt := THCHtmlFormat.Create(FActiveData.GetTopLevelData);
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
      FPageSize.PaperSize := StrToInt(vsPaper[0]);  // 纸张大小
      FPageSize.PaperWidth := StrToFloat(vsPaper[1]);  // 纸张宽度
      FPageSize.PaperHeight := StrToFloat(vsPaper[2]);  // 纸张高度
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
      FPageSize.PaperMarginLeft := StrToInt(vsMargin[0]);
      FPageSize.PaperMarginTop := StrToFloat(vsMargin[1]);
      FPageSize.PaperMarginRight := StrToFloat(vsMargin[2]);
      FPageSize.PaperMarginBottom := StrToFloat(vsMargin[3]);
    finally
      FreeAndNil(vsMargin);
    end;
  end;

var
  i: Integer;
begin
  FSymmetryMargin := ANode.Attributes['symmargin'];  // 是否对称页边距
  FPageOrientation := TPageOrientation(ANode.Attributes['ori']);  // 纸张方向

  FPageNoVisible := ANode.Attributes['pagenovisible'];  // 是否对称页边距
  GetXmlPaper_;
  GetXmlPaperMargin_;

  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    if ANode.ChildNodes[i].NodeName = 'header' then
    begin
      FHeaderOffset := ANode.ChildNodes[i].Attributes['offset'];
      FHeader.ParseXml(ANode.ChildNodes[i]);
    end
    else
    if ANode.ChildNodes[i].NodeName = 'footer' then
      FFooter.ParseXml(ANode.ChildNodes[i])
    else
    if ANode.ChildNodes[i].NodeName = 'page' then
      FPageData.ParseXml(ANode.ChildNodes[i]);
  end;

  BuildSectionPages(0);
end;

function THCSection.Replace(const AText: string): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
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
  Result := FPageData.ToHtml(APath);
end;

procedure THCSection.ToXml(const ANode: IHCXMLNode);
var
  vNode: IHCXMLNode;
begin
  ANode.Attributes['symmargin'] := FSymmetryMargin; // 是否对称页边距
  ANode.Attributes['ori'] := Ord(FPageOrientation);  // 纸张方向
  ANode.Attributes['pagenovisible'] := FPageNoVisible;  // 是否显示页码

  ANode.Attributes['pagesize'] :=  // 纸张大小
    IntToStr(FPageSize.PaperSize)
    + ',' + FormatFloat('0.#', FPageSize.PaperWidth)
    + ',' + FormatFloat('0.#', FPageSize.PaperHeight) ;

  ANode.Attributes['margin'] :=  // 边距
    FormatFloat('0.#', FPageSize.PaperMarginLeft) + ','
    + FormatFloat('0.#', FPageSize.PaperMarginTop) + ','
    + FormatFloat('0.#', FPageSize.PaperMarginRight) + ','
    + FormatFloat('0.#', FPageSize.PaperMarginBottom);

  // 存页眉
  vNode := ANode.AddChild('header');
  vNode.Attributes['offset'] := FHeaderOffset;
  FHeader.ToXml(vNode);

  // 存页脚
  vNode := ANode.AddChild('footer');
  FFooter.ToXml(vNode);

  // 存页面
  vNode := ANode.AddChild('page');
  FPageData.ToXml(vNode);
end;

{ TSectionPaintInfo }

constructor TSectionPaintInfo.Create;
begin
  inherited Create;
  FSectionIndex := -1;
  FPageIndex := -1;
end;

end.
