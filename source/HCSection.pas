{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                  文档节功能实现单元                   }
{                                                       }
{*******************************************************}

unit HCSection;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, HCRichData, HCSectionData,
  HCCustomRichData, HCTextStyle, HCParaStyle, HCItem, HCDrawItem, HCPage,
  HCCommon, HCStyle, HCCustomSectionData, HCCustomData, HCUndo;

type
  TPrintResult = (prOk, prNoPrinter, prError);

  TSectionPaintInfo = class(TPaintInfo)
  strict private
    FSectionIndex, FPageIndex: Integer;
    FPageDrawRight: Integer;
  public
    property SectionIndex: Integer read FSectionIndex write FSectionIndex;
    property PageIndex: Integer read FPageIndex write FPageIndex;
    property PageDrawRight: Integer read FPageDrawRight write FPageDrawRight;
  end;

  THCSection = class;

  TSectionPagePaintEvent = procedure(Sender: THCSection; const APageIndex: Integer;
    const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo) of object;

  THCSection = class(TObject)
  private
    FStyle: THCStyle;

    /// <summary> 是否对称边距 </summary>
    FSymmetryMargin: Boolean;
    FPages: THCPages;  // 所有页面
    FPageSize: THCPageSize;
    FHeader: THCHeaderData;
    FFooter: THCFooterData;
    FPageData: THCSectionData;
    FActiveData: THCRichData;  // 页眉、正文、页脚

    FPageNoVisible: Boolean;  // 是否显示页码

    FPageNoFrom,  // 页码从几开始
    FActivePageIndex,  // 当前激活的页
    FMousePageIndex,  // 当前鼠标所在页
    FDisplayFirstPageIndex,  // 屏显第一页
    FDisplayLastPageIndex,   // 屏显最后一页
    FHeaderOffset  // 页眉顶部偏移
      : Integer;

    FOnDataChanged,  // 页眉、页脚、页面某一个修改时触发
    FOnCheckUpdateInfo,  // 当前Data需要UpdateInfo更新时触发
    FOnReadOnlySwitch  // 页眉、页脚、页面只读状态发生变化时触发
      : TNotifyEvent;

    FOnPaintHeader, FOnPaintFooter, FOnPaintData, FOnPaintPage: TSectionPagePaintEvent;
    FOnItemPaintBefor, FOnItemPaintAfter: TItemPaintEvent;
    FOnInsertItem: TItemNotifyEvent;
    FOnItemResized: TDataItemEvent;
    FOnCreateItem: TNotifyEvent;
    FOnGetUndoList: TGetUndoListEvent;

    /// <summary> 当前Data内容变动完成后 </summary>
    /// <param name="AInsertActItemNo">插入发生的位置</param>
    /// <param name="ABuildSectionPage">需要重新计算页</param>
    procedure DoActiveDataChanged(const AActiveItemNo: Integer;
      const ABuildSectionPage: Boolean);

    /// <summary> 当前Data需要UpdateInfo更新 </summary>
    procedure DoActiveDataCheckUpdateInfo;

    procedure DoDataReadOnlySwitch(Sender: TObject);
    procedure DoDataItemPaintBefor(const AData: THCCustomData;
      const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DoDataItemPaintAfter(const AData: THCCustomData;
      const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

    procedure DoDataInsertItem(const AItem: THCCustomItem);

    /// <summary> 缩放Item约束不要超过整页宽、高 </summary>
    procedure DoDataItemResized(const AData: THCCustomData; const AItemNo: Integer);
    procedure DoDataCreateItem(Sender: TObject);
    function DoDataGetUndoList: THCUndoList;

    /// <summary>
    /// 返回页面指定DrawItem所在的页(跨页的按最后位置所在页)
    /// </summary>
    /// <param name="ADrawItemNo"></param>
    /// <returns></returns>
    function GetPageDataDrawItemPageIndex(const ADrawItemNo: Integer): Integer;

    /// <summary> 将某一页的坐标转换到页指定Data的坐标(此方法需要AX、AY在此页上的前提) </summary>
    /// <param name="APageIndex"></param>
    /// <param name="AData"></param>
    /// <param name="AX"></param>
    /// <param name="AY"></param>
    /// <param name="ARestrain">是否约束到Data的绝对区域中</param>
    procedure PageCoordToData(const APageIndex: Integer;
      const AData: THCRichData; var AX, AY: Integer;
      const ARestrain: Boolean = True);

    procedure DoDataChanged(Sender: TObject);

    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
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

    function GetPageWidthPix: Integer;
    function GetPageHeightPix: Integer;
    function GetPageMarginTopPix: Integer;
    function GetPageMarginLeftPix: Integer;
    function GetPageMarginRightPix: Integer;
    function GetPageMarginBottomPix: Integer;

    procedure SetPageWidthPix(const Value: Integer);
    procedure SetPageHeightPix(const Value: Integer);
    procedure SetPageMarginTopPix(const Value: Integer);
    procedure SetPageMarginLeftPix(const Value: Integer);
    procedure SetPageMarginRightPix(const Value: Integer);
    procedure SetPageMarginBottomPix(const Value: Integer);
    procedure SetHeaderOffset(const Value: Integer);
    function NewEmptyPage: THCPage;
    function GetPageCount: Integer;

    function GetRichDataAt(const X, Y: Integer): THCRichData;
    function GetActiveArea: TSectionArea;
    procedure SetActiveData(const Value: THCRichData);

    /// <summary>
    /// 返回数据格式化AVertical位置在胶卷中的位置
    /// </summary>
    /// <param name="AVertical"></param>
    /// <returns></returns>
    function GetDataFmtTopFilm(const AVertical: Integer): Integer;
    function ActiveDataChangeByAction(const AProc: TChangeProc): Boolean;
  public
    constructor Create(const AStyle: THCStyle);
    destructor Destroy; override;
    //
    /// <summary> 修改纸张边距 </summary>
    procedure ReMarginPaper;
    procedure Clear;
    procedure SetEmptyData;
    procedure DisActive;
    function SelectExists: Boolean;
    procedure SelectAll;
    function GetHint: string;
    function GetCurItem: THCCustomItem;
    function GetActiveItem: THCCustomItem;
    function GetActiveDrawItem: THCCustomDrawItem;
    function GetActiveDrawItemCoord: TPoint;
    function GetCurrentPage: Integer;
    procedure PaintDisplayPage(const AFilmOffsetX, AFilmOffsetY: Integer;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    /// <summary>
    /// 绘制指定页到指定的位置，为配合打印，开放ADisplayWidth, ADisplayHeight参数
    /// </summary>
    /// <param name="APageIndex">要绘制的页码</param>
    /// <param name="ALeft">绘制X偏移</param>
    /// <param name="ATop">绘制Y偏移</param>
    /// <param name="ACanvas"></param>
    procedure PaintPage(const APageIndex, ALeft, ATop: Integer;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    //
    procedure KillFocus;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure KeyPress(var Key: Char);
    procedure KeyDown(var Key: Word; Shift: TShiftState);
    procedure KeyUp(var Key: Word; Shift: TShiftState);
    //
    procedure ApplyTextStyle(const AFontStyle: TFontStyleEx);
    procedure ApplyTextFontName(const AFontName: TFontName);
    procedure ApplyTextFontSize(const AFontSize: Integer);
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
    //
    function ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;
    function ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;
    function ActiveTableDeleteCurRow: Boolean;
    function ActiveTableInsertColAfter(const AColCount: Byte): Boolean;
    function ActiveTableInsertColBefor(const AColCount: Byte): Boolean;
    function ActiveTableDeleteCurCol: Boolean;
    //
    // 节坐标转换到指定页坐标
    procedure SectionCoordToPage(const APageIndex, X, Y: Integer; var
      APageX, APageY: Integer);

    /// <summary>
    /// 为段应用对齐方式
    /// </summary>
    /// <param name="AAlign">对方方式</param>
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);
    procedure ApplyParaBackColor(const AColor: TColor);
    procedure ApplyParaLineSpace(const ASpace: Integer);

    /// <summary>
    /// 获取光标在Dtat中的位置信息并映射到指定页面
    /// </summary>
    /// <param name="APageIndex">要映射到的页序号</param>
    /// <param name="ACaretInfo">光标位置信息</param>
    procedure GetPageCaretInfo(var ACaretInfo: TCaretInfo);

    /// <summary>
    /// 返回当前节指定的垂直偏移处对应的页
    /// </summary>
    /// <param name="AVOffset">垂直偏移</param>
    /// <returns>页序号，-1表示无对应页</returns>
    function GetPageByFilm(const AVOffset: Integer): Integer;

    /// <summary> 某页在整个节中的Top位置 </summary>
    /// <param name="APageIndex"></param>
    /// <returns></returns>
    function GetPageTopFilm(const APageIndex: Integer): Integer;

    /// <summary>
    /// 返回指定页数据起始位置在整个Data中的Top，注意 20161216001
    /// </summary>
    /// <param name="APageIndex"></param>
    /// <returns></returns>
    function GetPageDataFmtTop(const APageIndex: Integer): Integer;

    /// <summary> 页眉内容在页中的起始位置 </summary>
    /// <returns></returns>
    function GetHeaderPageDrawTop: Integer;

    /// <summary>
    /// 获取格式化垂直位置在数据的哪一页(目前只在GetPageCaretInfo中用到了，是否具有通用性？)
    /// </summary>
    /// <param name="AVertical">垂直位置</param>
    /// <returns>页序号</returns>
    //function GetPageByDataFmt(const AVertical: Integer): Integer;

    function GetPageMarginLeft(const APageIndex: Integer): Integer;

    /// <summary>
    /// 根据页面对称属性，获取指定页的左右边距
    /// </summary>
    /// <param name="APageIndex"></param>
    /// <param name="AMarginLeft"></param>
    /// <param name="AMarginRight"></param>
    procedure GetPageMarginLeftAndRight(const APageIndex: Integer;
      var AMarginLeft, AMarginRight: Integer);
    /// <summary>
    /// 从正文指定Item开始重新计算页
    /// </summary>
    /// <param name="AStartItemNo"></param>
    procedure BuildSectionPages(const AStartItemNo: Integer);
    function DeleteSelected: Boolean;
    procedure DisSelect;
    function MergeTableSelectCells: Boolean;
    procedure ReFormatActiveItem;
    function GetHeaderAreaHeight: Integer;
    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
    function GetFilmHeight: Cardinal;  // 所有页面高+分隔条
    function GetFilmWidth: Cardinal;

    /// <summary>
    /// 标记样式是否在用或删除不使用的样式后修正样式序号
    /// </summary>
    /// <param name="AMark">True:标记样式是否在用，Fasle:修正原样式因删除不使用样式后的新序号</param>
    procedure MarkStyleUsed(const AMark: Boolean;
      const AParts: TSaveParts = [saHeader, saData, saFooter]);
    procedure SaveToStream(const AStream: TStream;
      const ASaveParts: TSaveParts = [saHeader, saData, saFooter]);
    procedure LoadFromText(const AFileName: string; const AEncoding: TEncoding);
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word);
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean;
    procedure FormatData;
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
    //
    property PageWidthPix: Integer read GetPageWidthPix write SetPageWidthPix;
    property PageHeightPix: Integer read GetPageHeightPix write SetPageHeightPix;
    property PageMarginTopPix: Integer read GetPageMarginTopPix write SetPageMarginTopPix;
    property PageMarginLeftPix: Integer read GetPageMarginLeftPix write SetPageMarginLeftPix;
    property PageMarginRightPix: Integer read GetPageMarginRightPix write SetPageMarginRightPix;
    property PageMarginBottomPix: Integer read GetPageMarginBottomPix write SetPageMarginBottomPix;

    property HeaderOffset: Integer read FHeaderOffset write SetHeaderOffset;
    property Header: THCHeaderData read FHeader;
    property Footer: THCFooterData read FFooter;
    property PageData: THCSectionData read FPageData;

    /// <summary> 当前文档激活区域(页眉、页脚、页面)的数据对象 </summary>
    property ActiveData: THCRichData read FActiveData write SetActiveData;

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
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;
    property OnCheckUpdateInfo: TNotifyEvent read FOnCheckUpdateInfo write FOnCheckUpdateInfo;
    property OnInsertItem: TItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnItemResized: TDataItemEvent read FOnItemResized write FOnItemResized;
    property OnPaintHeader: TSectionPagePaintEvent read FOnPaintHeader write FOnPaintHeader;
    property OnPaintFooter: TSectionPagePaintEvent read FOnPaintFooter write FOnPaintFooter;
    property OnPaintData: TSectionPagePaintEvent read FOnPaintData write FOnPaintData;
    property OnPaintPage: TSectionPagePaintEvent read FOnPaintPage write FOnPaintPage;
    property OnItemPaintBefor: TItemPaintEvent read FOnItemPaintBefor write FOnItemPaintBefor;
    property OnItemPaintAfter: TItemPaintEvent read FOnItemPaintAfter write FOnItemPaintAfter;
    property OnCreateItem: TNotifyEvent read FOnCreateItem write FOnCreateItem;
    property OnGetUndoList: TGetUndoListEvent read FOnGetUndoList write FOnGetUndoList;
  end;

implementation

uses
  Math, HCRectItem;

{ THCSection }

function THCSection.ActiveTableDeleteCurCol: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.ActiveTableDeleteCurCol;
    end);
end;

function THCSection.ActiveTableDeleteCurRow: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.ActiveTableDeleteCurRow;
    end);
end;

function THCSection.ActiveTableInsertColAfter(const AColCount: Byte): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.TableInsertColAfter(AColCount);
    end);
end;

function THCSection.ActiveTableInsertColBefor(const AColCount: Byte): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.TableInsertColBefor(AColCount);
    end);
end;

function THCSection.ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.TableInsertRowAfter(ARowCount);
    end);
end;

function THCSection.ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.TableInsertRowBefor(ARowCount);
    end);
end;

procedure THCSection.ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaAlignHorz(AAlign);
    end);
end;

procedure THCSection.ApplyParaAlignVert(const AAlign: TParaAlignVert);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaAlignVert(AAlign);
    end);
end;

procedure THCSection.ApplyParaBackColor(const AColor: TColor);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaBackColor(AColor);
    end);
end;

procedure THCSection.ApplyParaLineSpace(const ASpace: Integer);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyParaLineSpace(ASpace);
    end);
end;

procedure THCSection.ApplyTextBackColor(const AColor: TColor);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextBackColor(AColor);
    end);
end;

procedure THCSection.ApplyTextColor(const AColor: TColor);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextColor(AColor);
    end);
end;

procedure THCSection.ApplyTextFontName(const AFontName: TFontName);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextFontName(AFontName);
    end);
end;

procedure THCSection.ApplyTextFontSize(const AFontSize: Integer);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextFontSize(AFontSize);
    end);
end;

procedure THCSection.ApplyTextStyle(const AFontStyle: TFontStyleEx);
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ApplyTextStyle(AFontStyle);
    end);
end;

procedure THCSection.Clear;
begin
  FHeader.Clear;
  FFooter.Clear;
  FPageData.Clear;
  FPages.ClearEx;
end;

constructor THCSection.Create(const AStyle: THCStyle);
var
  vWidth: Integer;

  procedure SetDataProperty(const AData: THCCustomSectionData);
  begin
    AData.Width := vWidth;
    AData.OnInsertItem := DoDataInsertItem;
    AData.OnItemResized := DoDataItemResized;
    AData.OnCreateItem := DoDataCreateItem;
    AData.OnReadOnlySwitch := DoDataReadOnlySwitch;
    AData.OnItemPaintBefor := DoDataItemPaintBefor;
    AData.OnItemPaintAfter := DoDataItemPaintAfter;
    AData.OnGetUndoList := DoDataGetUndoList;
  end;

begin
  inherited Create;
  FStyle := AStyle;
  FPageNoVisible := True;
  FPageNoFrom := 1;
  FHeaderOffset := 20;
  FDisplayFirstPageIndex := -1;
  FDisplayLastPageIndex := -1;

  FPageSize := THCPageSize.Create(AStyle.PixelsPerInchX, AStyle.PixelsPerInchY);
  vWidth := GetContentWidth;

  FPageData := THCSectionData.Create(AStyle);
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

function THCSection.DeleteSelected: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.DeleteSelected;
    end);
end;

destructor THCSection.Destroy;
begin
  FHeader.Free;
  FFooter.Free;
  FPageData.Free;
  FPageSize.Free;
  FPages.Free;
  inherited Destroy;
end;

procedure THCSection.DisActive;
begin
  //if FActiveData <> nil then
    FActiveData.DisSelect;

  FHeader.InitializeField;
  FFooter.InitializeField;
  FPageData.InitializeField;
  FActiveData := FPageData;
end;

procedure THCSection.DisSelect;
begin
  FActiveData.GetTopLevelData.DisSelect;
end;

procedure THCSection.DoActiveDataChanged(const AActiveItemNo: Integer;
  const ABuildSectionPage: Boolean);
begin
  if ABuildSectionPage then
  begin
    if FActiveData = FPageData then
      BuildSectionPages(AActiveItemNo)
    else
      BuildSectionPages(0);
  end;

  DoDataChanged(Self);
end;

procedure THCSection.DoActiveDataCheckUpdateInfo;
begin
  if Assigned(FOnCheckUpdateInfo) then
    FOnCheckUpdateInfo(Self);
end;

procedure THCSection.DoDataChanged(Sender: TObject);
begin
  if Assigned(FOnDataChanged) then
    FOnDataChanged(Sender);
end;

procedure THCSection.DoDataCreateItem(Sender: TObject);
begin
  if Assigned(FOnCreateItem) then
    FOnCreateItem(Sender);
end;

procedure THCSection.DoDataInsertItem(const AItem: THCCustomItem);
begin
  if Assigned(FOnInsertItem) then
    FOnInsertItem(AItem);
end;

procedure THCSection.DoDataItemPaintAfter(const AData: THCCustomData;
  const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnItemPaintAfter) then
  begin
    FOnItemPaintAfter(AData, ADrawItemIndex, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCSection.DoDataItemPaintBefor(const AData: THCCustomData;
  const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnItemPaintBefor) then
  begin
    FOnItemPaintBefor(AData, ADrawItemIndex, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCSection.DoDataItemResized(const AData: THCCustomData; const AItemNo: Integer);
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
  if vData = FPageData then
    vHeight := GetContentHeight - FStyle.ParaStyles[vResizeItem.ParaNo].LineSpace;

  vResizeItem.RestrainSize(vWidth, vHeight);

  if Assigned(FOnItemResized) then
    FOnItemResized(AData, AItemNo);
end;

procedure THCSection.DoDataReadOnlySwitch(Sender: TObject);
begin
  if Assigned(FOnReadOnlySwitch) then
    FOnReadOnlySwitch(Self);
end;

function THCSection.DoDataGetUndoList: THCUndoList;
begin
  if Assigned(FOnGetUndoList) then
    Result := FOnGetUndoList
  else
    Result := nil;
end;

procedure THCSection.SetActiveData(const Value: THCRichData);
begin
  if not (Value is THCCustomSectionData) then
    raise Exception.Create(HCS_EXCEPTION_UNACCEPTDATATYPE);

  if FActiveData <> Value then
  begin
    if FActiveData <> nil then
      FActiveData.DisActive;  // 旧的取消激活
    FActiveData := Value;
    FStyle.UpdateInfoReScroll;
  end;
end;

procedure THCSection.SetEmptyData;
begin
  FHeader.SetEmptyData;
  FFooter.SetEmptyData;
  FPageData.SetEmptyData;
end;

procedure THCSection.FormatData;
begin
  FActiveData.DisSelect;  // 先清选中，防止格式化后选中位置不存在
  FHeader.ReFormat(0);
  Footer.ReFormat(0);
  FPageData.ReFormat(0);
end;

function THCSection.GetCurrentPage: Integer;
begin
  Result := -1;
  if FActiveData <> FPageData then
    Result := FActivePageIndex
  else
  begin
    if FPageData.CaretDrawItemNo < 0 then
    begin
      if FPageData.SelectInfo.StartItemNo >= 0 then
      begin
        Result := GetPageDataDrawItemPageIndex(
          FPageData.GetDrawItemNoByOffset(FPageData.SelectInfo.StartItemNo,
          FPageData.SelectInfo.StartItemOffset))
      end;
    end
    else
      Result := GetPageDataDrawItemPageIndex(FPageData.CaretDrawItemNo);
  end;
end;

function THCSection.GetActiveArea: TSectionArea;
begin
  if FActiveData = FHeader then
    Result := TSectionArea.saHeader
  else
  if FActiveData = FFooter then
    Result := TSectionArea.saFooter
  else
    Result := TSectionArea.saData;
end;

function THCSection.GetActiveDrawItem: THCCustomDrawItem;
begin
  Result := FActiveData.GetActiveDrawItem;
end;

function THCSection.GetActiveDrawItemCoord: TPoint;
begin
  Result := FActiveData.GetActiveDrawItemCoord;
end;

function THCSection.GetActiveItem: THCCustomItem;
begin
  Result := FActiveData.GetActiveItem;
end;

function THCSection.GetContentHeight: Integer;
begin
  Result := FPageSize.PageHeightPix  // 节页面正文区域高度，即页面除页眉、页脚后净高
    - FPageSize.PageMarginBottomPix - GetHeaderAreaHeight;
end;

function THCSection.GetContentWidth: Integer;
begin
  Result := FPageSize.PageWidthPix - FPageSize.PageMarginLeftPix - FPageSize.PageMarginRightPix;
end;

function THCSection.GetCurItem: THCCustomItem;
begin
  FActiveData.GetCurItem;
end;

function THCSection.GetReadOnly: Boolean;
begin
  Result := FHeader.ReadOnly and FFooter.ReadOnly and FPageData.ReadOnly;
end;

function THCSection.GetRichDataAt(const X, Y: Integer): THCRichData;
var
  vPageIndex, vMarginLeft, vMarginRight: Integer;
begin
  Result := nil;
  vPageIndex := GetPageByFilm(Y);
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

function THCSection.GetDataFmtTopFilm(const AVertical: Integer): Integer;
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
      Result := Result + MinPadding + FPageSize.PageHeightPix;
  end;
  Result := Result + MinPadding + GetHeaderAreaHeight + vTop;
end;

function THCSection.GetPageDataDrawItemPageIndex(const ADrawItemNo: Integer): Integer;
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

function THCSection.GetFilmHeight: Cardinal;
begin
  Result := FPages.Count * (MinPadding + FPageSize.PageHeightPix);
end;

function THCSection.GetFilmWidth: Cardinal;
begin
  Result := FPages.Count * (MinPadding + FPageSize.PageWidthPix);
end;

function THCSection.GetHeaderAreaHeight: Integer;
begin
  Result := FHeaderOffset + FHeader.Height;
  if Result < FPageSize.PageMarginTopPix then
    Result := FPageSize.PageMarginTopPix;
  //Result := Result + 20;  // debug
end;

function THCSection.GetHeaderPageDrawTop: Integer;
var
  vHeaderHeight: Integer;
begin
  Result := FHeaderOffset;
  vHeaderHeight := FHeader.Height;
  if vHeaderHeight < (FPageSize.PageMarginTopPix - FHeaderOffset) then
    Result := Result + (FPageSize.PageMarginTopPix - FHeaderOffset - vHeaderHeight) div 2;
end;

function THCSection.GetHint: string;
begin
  //Result := '';
  //if FActiveData <> nil then
    Result := FActiveData.GetTopLevelData.GetHint;
end;

function THCSection.GetPageByFilm(const AVOffset: Integer): Integer;
var
  i, vPos: Integer;
begin
  Result := -1;
  vPos := 0;
  for i := 0 to FPages.Count - 1 do
  begin
    vPos := vPos + MinPadding + FPageSize.PageHeightPix;
    if vPos >= AVOffset then  // AVOffset < 0时有2种可能，1当前节第一页前面的Padding，2在上一节里
    begin
      Result := i;
      Break;
    end;
  end;

  if (Result < 0) and (AVOffset > vPos) then  // 同节最后一页下面，下一页里面
    Result := FPages.Count - 1;

  Assert(Result >= 0, '没有获取到正确的页序号！');
end;

procedure THCSection.GetPageCaretInfo(var ACaretInfo: TCaretInfo);
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

  FActiveData.GetCaretInfoCur(ACaretInfo);

  if ACaretInfo.Visible then
  begin
    //APageIndex := GetPageByDataFmt(ACaretInfo.Y);  // 由于表格跨页后GetSelectStartPageIndex只能取到开始页，所以这里需要根据垂直位置获取光标所在页
    GetPageMarginLeftAndRight(vPageIndex, vMarginLeft, vMarginRight);
    ACaretInfo.X := ACaretInfo.X + vMarginLeft;
    ACaretInfo.Y := ACaretInfo.Y + GetPageTopFilm(vPageIndex);

    if FActiveData = FHeader then
      ACaretInfo.Y := ACaretInfo.Y + GetHeaderPageDrawTop
    else
    if FActiveData = FPageData then
      ACaretInfo.Y := ACaretInfo.Y + GetHeaderAreaHeight - GetPageDataFmtTop(vPageIndex)
    else
    if FActiveData = FFooter then
      ACaretInfo.Y := ACaretInfo.Y + FPageSize.PageHeightPix - FPageSize.PageMarginBottomPix;
  end;
end;

function THCSection.GetPageCount: Integer;
begin
  Result := FPages.Count;  // 所有页面
end;

function THCSection.GetPageDataFmtTop(const APageIndex: Integer): Integer;
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

function THCSection.GetPageHeightPix: Integer;
begin
  Result := FPageSize.PageHeightPix;
end;

function THCSection.GetPageMarginBottomPix: Integer;
begin
  Result := FPageSize.PageMarginBottomPix;
end;

function THCSection.GetPageMarginLeft(const APageIndex: Integer): Integer;
var
  vMarginRight: Integer;
begin
  GetPageMarginLeftAndRight(APageIndex, Result, vMarginRight);
end;

procedure THCSection.GetPageMarginLeftAndRight(const APageIndex: Integer;
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

function THCSection.GetPageMarginLeftPix: Integer;
begin
  Result := FPageSize.PageMarginLeftPix;
end;

function THCSection.GetPageMarginRightPix: Integer;
begin
  Result := FPageSize.PageMarginRightPix;
end;

function THCSection.GetPageMarginTopPix: Integer;
begin
  Result := FPageSize.PageMarginTopPix;
end;

function THCSection.GetPageTopFilm(const APageIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := MinPadding;
  for i := 0 to APageIndex - 1 do
    Result := Result + FPageSize.PageHeightPix + MinPadding;  // 每一页和其上面的分隔计为一整个处理单元
end;

function THCSection.GetPageWidthPix: Integer;
begin
  Result := FPageSize.PageWidthPix;
end;

function THCSection.GetPaperHeight: Single;
begin
  Result := FPageSize.PaperHeight;
end;

function THCSection.GetPaperMarginBottom: Single;
begin
  Result := FPageSize.PaperMarginBottom;
end;

function THCSection.GetPaperMarginLeft: Single;
begin
  Result := FPageSize.PaperMarginLeft;
end;

function THCSection.GetPaperMarginRight: Single;
begin
  Result := FPageSize.PaperMarginRight;
end;

function THCSection.GetPaperMarginTop: Single;
begin
  Result := FPageSize.PaperMarginTop;
end;

function THCSection.GetPaperSize: Integer;
begin
  Result := FPageSize.PaperSize;
end;

function THCSection.GetPaperWidth: Single;
begin
  Result := FPageSize.PaperWidth;
end;

function THCSection.InsertBreak: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FPageData.InsertBreak;
    end);
end;

function THCSection.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertItem(AIndex, AItem);
    end);
end;

function THCSection.InsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertItem(AItem);
    end);
end;

function THCSection.InsertLine(const ALineHeight: Integer): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertLine(ALineHeight);
    end);
end;

function THCSection.InsertPageBreak: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FPageData.InsertPageBreak;
    end);
end;

function THCSection.ActiveDataChangeByAction(const AProc: TChangeProc): Boolean;
var
  vHeight, vDrawItemCount, vNewDrawItemCount, vCurItemNo, vNewItemNo: Integer;
begin
  if not FActiveData.CanEdit then Exit(False);

  // 记录变动前的状态
  vHeight := FActiveData.Height;
  // 应用选中文本样式等操作，并不引起高度变化，但会引起DrawItem数量变化
  // 也需要重新计算各页起始结束DrawItem
  vDrawItemCount := FActiveData.DrawItems.Count;
  vCurItemNo := FActiveData.GetCurItemNo;

  Result := AProc;  // 处理变动

  // 变动后的状态
  vNewItemNo := FActiveData.GetCurItemNo;  // 变动后的当前ItemNo
  vNewDrawItemCount := FActiveData.DrawItems.Count;
  if vNewItemNo < 0 then  // 如果变动后小于0，修正为第0个
    vNewItemNo := 0;

  DoActiveDataChanged(Min(vCurItemNo, vNewItemNo),  // vCurItemNo是最后一个时，样式修改合并到前一个后不存在了，所以取范围小的
    (vDrawItemCount <> vNewDrawItemCount) or (vHeight <> FActiveData.Height));
end;

function THCSection.InsertStream(const AStream: TStream; const AStyle: THCStyle;
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

function THCSection.InsertTable(const ARowCount, AColCount: Integer): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertTable(ARowCount, AColCount);
    end);
end;

function THCSection.InsertText(const AText: string): Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.InsertText(AText);
    end);
end;

procedure THCSection.KeyDown(var Key: Word; Shift: TShiftState);
var
  vKey: Word;
begin
  if not FActiveData.CanEdit then Exit;

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
          FActivePageIndex := GetCurrentPage;  // 方向键可能移动到了其他页
          DoActiveDataCheckUpdateInfo;
        end;
    end;
  end;
end;

procedure THCSection.KeyPress(var Key: Char);
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

procedure THCSection.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not FActiveData.CanEdit then Exit;
  FActiveData.KeyUp(Key, Shift);
end;

procedure THCSection.KillFocus;
begin
  //if FActiveData <> nil then
    FActiveData.KillFocus;
end;

procedure THCSection.LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
  const AFileVersion: Word);
var
  vDataSize: Int64;
  vArea: Boolean;
  vLoadParts: TSaveParts;
begin
  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));

  AStream.ReadBuffer(FSymmetryMargin, SizeOf(FSymmetryMargin));

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
    vLoadParts := vLoadParts + [saData];

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

  if saData in vLoadParts then
    FPageData.LoadFromStream(AStream, FStyle, AFileVersion);

  BuildSectionPages(0);
end;

procedure THCSection.LoadFromText(const AFileName: string;
  const AEncoding: TEncoding);
begin
  FPageData.LoadFromText(AFileName, AEncoding);
  BuildSectionPages(0);
end;

procedure THCSection.MarkStyleUsed(const AMark: Boolean;
  const AParts: TSaveParts = [saHeader, saData, saFooter]);
begin
  if saHeader in AParts then
    FHeader.MarkStyleUsed(AMark);

  if saFooter in AParts then
    FFooter.MarkStyleUsed(AMark);

  if saData in AParts then
    FPageData.MarkStyleUsed(AMark);
end;

function THCSection.MergeTableSelectCells: Boolean;
begin
  Result := ActiveDataChangeByAction(function(): Boolean
    begin
      Result := FActiveData.MergeTableSelectCells;
    end);
end;

procedure THCSection.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vX, vY, vPageIndex: Integer;
  vNewActiveData: THCRichData;
  vChangeActiveData: Boolean;
begin
  vChangeActiveData := False;
  vPageIndex := GetPageByFilm(Y);
  if FActivePageIndex <> vPageIndex then
    FActivePageIndex := vPageIndex;

  SectionCoordToPage(vPageIndex, X, Y, vX, vY);  // X，Y转换到指定页的坐标vX,vY

  vNewActiveData := GetRichDataAt(vX, vY);

  if (vNewActiveData <> FActiveData) and (ssDouble in Shift) then  // 双击、新的Data
  begin
    SetActiveData(vNewActiveData);
    vChangeActiveData := True;
  end;

  PageCoordToData(vPageIndex, FActiveData, vX, vY);

  if FActiveData = FPageData then
    vY := vY + GetPageDataFmtTop(vPageIndex);

  if (ssDouble in Shift) and (not vChangeActiveData) then  // 在同一Data上双击
    FActiveData.DblClick(vX, vY)
  else
    FActiveData.MouseDown(Button, Shift, vX, vY);
end;

procedure THCSection.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vMarginLeft, vMarginRight: Integer;
  vX, vY: Integer;
  vMoveData: THCRichData;
begin
  // 默认光标
  GetPageMarginLeftAndRight(FMousePageIndex, vMarginLeft, vMarginRight);
  if (X > vMarginLeft) and (X < FPageSize.PageWidthPix - vMarginRight) then
    GCursor := crIBeam
  else
    GCursor := crDefault;

  FMousePageIndex := GetPageByFilm(Y);
  if FMousePageIndex < 0 then Exit;

  SectionCoordToPage(FMousePageIndex, X, Y, vX, vY);

  //if Shift <> [] then  // 有操作时判断，不在当前激活Data上时不操作(如鼠标由页面划选到页脚时不再继续找选中结束位置)
  vMoveData := GetRichDataAt(vX, vY);
  if (vMoveData <> FActiveData) and (Shift = []) then
  begin
    FActiveData.MouseLeave;
    //if not (FActiveData.GetTopLevelData.GetActiveItem is THCResizeRectItem) then
    Exit;  // 不在当前激活的Data上移动
  end;

  PageCoordToData(FMousePageIndex, FActiveData, vX, vY, not FActiveData.SelectedResizing);

  if FActiveData = FPageData then
    vY := vY + GetPageDataFmtTop(FMousePageIndex);

  FActiveData.MouseMove(Shift, vX, vY);
end;

procedure THCSection.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vX, vY, vPageIndex: Integer;
begin
  vPageIndex := GetPageByFilm(Y);
  SectionCoordToPage(vPageIndex, X, Y, vX, vY);

  //if GetRichDataAt(vX, vY) <> FActiveData then Exit;  // 不在当前激活的Data上

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

function THCSection.NewEmptyPage: THCPage;
begin
  Result := THCPage.Create;
  FPages.Add(Result);
end;

procedure THCSection.PageCoordToData(const APageIndex: Integer;
  const AData: THCRichData; var AX, AY: Integer; const ARestrain: Boolean = True);
var
  viTemp: Integer;
begin
  AX := AX - GetPageMarginLeft(APageIndex);
  if ARestrain then  // 为避免边界(激活正文，在页眉页脚点击时判断仍是在正文位置造成光标错误)约束后都偏移1
  begin
    if AX < 0 then
      AX := 1
    else
    begin
      viTemp := FPageSize.PageWidthPix - FPageSize.PageMarginRightPix;
      if AX > viTemp then
        AX := viTemp - 1;
    end;
  end;

  if AData = FHeader then
  begin
    AY := AY - GetHeaderPageDrawTop;
    if ARestrain then  // 约束到页眉绝对区域中
    begin
      if AY < FHeaderOffset then
        AY := FHeaderOffset + 1
      else
      begin
        viTemp := GetHeaderAreaHeight;
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
    viTemp := GetHeaderAreaHeight;
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

procedure THCSection.PaintDisplayPage(const AFilmOffsetX, AFilmOffsetY: Integer;
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

procedure THCSection.PaintPage(const APageIndex, ALeft, ATop: Integer;
  const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  vHeaderAreaHeight, vMarginLeft, vMarginRight,
  vPageDrawLeft, vPageDrawRight, vPageDrawTop, vPageDrawBottom,  // 页区域各位置
  vPageDataScreenTop, vPageDataScreenBottom,  // 页数据屏幕位置
  vScaleWidth, vScaleHeight: Integer;

  {$REGION '页眉'}
  procedure PaintHeader;
  var
    vHeaderDataDrawTop, vHeaderDataDrawBottom, vDCState: Integer;
  begin
    vHeaderDataDrawTop := vPageDrawTop + GetHeaderPageDrawTop;
    vHeaderDataDrawBottom := vPageDrawTop + vHeaderAreaHeight;

    FHeader.PaintData(vPageDrawLeft + vMarginLeft, vHeaderDataDrawTop,
      vHeaderDataDrawBottom, Max(vHeaderDataDrawTop, 0),
      Min(vHeaderDataDrawBottom, APaintInfo.WindowHeight), 0, ACanvas, APaintInfo);

    if FActiveData = FHeader then  // 当前激活的是页眉
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
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION '页脚'}
  procedure PaintFooter;
  var
    vY, vDCState: Integer;
    vS: string;
  begin
    vY := vPageDrawBottom - FPageSize.PageMarginBottomPix;
    FFooter.PaintData(vPageDrawLeft + vMarginLeft, vY, vPageDrawBottom,
      Max(vY, 0), Min(vPageDrawBottom, APaintInfo.WindowHeight), 0, ACanvas, APaintInfo);

    if FActiveData = FFooter then  // 当前激活的是页脚
    begin
      ACanvas.Pen.Color := clBlue;
      ACanvas.MoveTo(vPageDrawLeft, vY);
      ACanvas.LineTo(vPageDrawRight, vY);
    end;

    if Assigned(FOnPaintFooter) then
    begin
      vDCState := Windows.SaveDC(ACanvas.Handle);
      try
        FOnPaintFooter(Self, APageIndex, Rect(vPageDrawLeft + vMarginLeft, vY,
          vPageDrawRight - vMarginRight, vPageDrawBottom), ACanvas, APaintInfo);
      finally
        Windows.RestoreDC(ACanvas.Handle, vDCState);
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION '绘制页面数据'}
  procedure PaintPageData;
  var
    vPageDataFmtTop, vDCState: Integer;
  begin
    if (FPages[APageIndex].StartDrawItemNo < 0) or (FPages[APageIndex].EndDrawItemNo < 0) then
      Exit;

    //vPageDataOffsetX := Max(AFilmOffsetX - vPageDrawLeft - PageMarginLeftPix, 0);
    { 当前页在当前屏显出来的数据边界映射到格式化中的边界 }
    vPageDataFmtTop := GetPageDataFmtTop(APageIndex);

    { 绘制数据，把Data中指定位置的数据，绘制到指定的页区域中，并按照可显示出来的区域约束 }
    FPageData.PaintData(vPageDrawLeft + vMarginLeft,  // 当前页数据要绘制到的Left
      vPageDrawTop + vHeaderAreaHeight,     // 当前页数据要绘制到的Top
      vPageDrawBottom - PageMarginBottomPix,  // 当前页数据要绘制的Bottom
      vPageDataScreenTop,     // 界面呈现当前页数据的Top位置
      vPageDataScreenBottom,  // 界面呈现当前页数据Bottom位置
      vPageDataFmtTop,  // 指定从哪个位置开始的数据绘制到页数据起始位置
      ACanvas,
      APaintInfo);

    if Assigned(FOnPaintData) then
    begin
      vDCState := Windows.SaveDC(ACanvas.Handle);
      try
        FOnPaintData(Self, APageIndex, Rect(vPageDrawLeft + vMarginLeft,
          vPageDrawTop + vHeaderAreaHeight, vPageDrawRight - vMarginRight,
          vPageDrawBottom - PageMarginBottomPix), ACanvas, APaintInfo);
      finally
        Windows.RestoreDC(ACanvas.Handle, vDCState);
      end;
    end;
  end;
  {$ENDREGION}

var
  vX, vY: Integer;
  vPaintRegion: HRGN;
  vClipBoxRect, vRect: TRect;
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

  APaintInfo.PageDrawRight := vPageDrawRight;

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

      ACanvas.Pen.Style := TPenStyle.psSolid;
      ACanvas.Pen.Color := clGray;

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
      ACanvas.Pen.Color := clGray;
      ACanvas.Pen.Style := TPenStyle.psSolid;
      // 左下，左-原-下
      vX := vPageDrawLeft + vMarginLeft;
      ACanvas.MoveTo(vX - PMSLineHeight, vY);
      ACanvas.LineTo(vX, vY);
      ACanvas.LineTo(vX, vY + PMSLineHeight);
      // 右下，右-原-下
      vX := vPageDrawRight - vMarginRight;
      ACanvas.MoveTo(vX + PMSLineHeight, vY);
      ACanvas.LineTo(vX, vY);
      ACanvas.LineTo(vX, vY + PMSLineHeight);
    end;
    {$ENDREGION}

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
    finally
      DeleteObject(vPaintRegion);
    end;

    {ACanvas.Brush.Color := clInfoBk;
    vRect := Rect(vPageDrawLeft, Max(vPageDrawTop + FHeaderOffset, 0),
    vPageDrawRight,
    Min(vPageDrawTop + vHeaderAreaHeight, vScaleHeight));
    ACanvas.FillRect(vRect);}

    PaintHeader;
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
    finally
      DeleteObject(vPaintRegion);
    end;

    {ACanvas.Brush.Color := clYellow;
    vRect := Rect(vPageDrawLeft,
      Max(vPageDrawBottom - FPageSize.PageMarginBottomPix, 0),
      vPageDrawRight,
      Min(vPageDrawBottom, vScaleHeight));
    ACanvas.FillRect(vRect);}

    PaintFooter;
  end;
  {$ENDREGION}

  {$REGION ' 绘制正文 '}
  if vPageDataScreenBottom > vPageDataScreenTop then  // 能露出数据则绘制当前页，绘制正文
  begin
    vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPageDrawLeft),
      APaintInfo.GetScaleY(Max(vPageDrawTop + vHeaderAreaHeight, vPageDataScreenTop)),
      APaintInfo.GetScaleX(vPageDrawRight),
      APaintInfo.GetScaleY(Min(vPageDrawBottom - FPageSize.PageMarginBottomPix, vPageDataScreenBottom)));
    try
      SelectClipRgn(ACanvas.Handle, vPaintRegion);  // 设置绘制有效区域
    finally
      DeleteObject(vPaintRegion);
    end;

    {ACanvas.Brush.Color := clYellow;
    vRect := Rect(vPageDrawLeft,
      Max(vPageDrawTop + vHeaderAreaHeight, vPageDataScreenTop),
      vPageDrawRight,
      Min(vPageDrawBottom - PageMarginBottomPix, vPageDataScreenBottom));
    ACanvas.FillRect(vRect);}

    PaintPageData;
  end;
  {$ENDREGION}

  // 恢复区域，准备给后续绘制用
  vPaintRegion := CreateRectRgn(vClipBoxRect.Left, vClipBoxRect.Top,
    vClipBoxRect.Right, vClipBoxRect.Bottom);
  try
    SelectClipRgn(ACanvas.Handle, vPaintRegion);
  finally
    DeleteObject(vPaintRegion);
  end;

  if Assigned(FOnPaintPage) then  // 公开页面绘制事件
  begin
    FOnPaintPage(Self, APageIndex,
      Rect(vPageDrawLeft, vPageDrawTop, vPageDrawRight, vPageDrawBottom),
      ACanvas, APaintInfo);
  end;
end;

procedure THCSection.BuildSectionPages(const AStartItemNo: Integer);
var
  vPageIndex, vPageDataFmtTop, vPageDataFmtBottom, vContentHeight,
  vSuplus,  // 所有因换页向下偏移量的总和
  vBreakSeat  // 分页位置，不同RectItem的含义不同，表格表示 vBreakRow
    : Integer;

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

  {$REGION '_FormatRectItemCheckPage'}
  procedure _FormatRectItemCheckPage(const ADrawItemNo: Integer);
  var
    vRectItem: THCCustomRectItem;

    {$REGION '_RectItemCheckPage'}
    procedure _RectItemCheckPage(const AStartSeat: Integer);  // 开始分页计算的位置，不同RectItem含义不同，表格表示AStartRowNo
    var
      vFmtHeightInc, vFmtOffset: Integer;
      vDrawRect: TRect;
    begin
      if FPageData.GetDrawItemStyle(ADrawItemNo) = THCStyle.RsPageBreak then
      begin
        vFmtOffset := vPageDataFmtBottom - FPageData.DrawItems[ADrawItemNo].Rect.Top;

        vSuplus := vSuplus + vFmtOffset;
        if vFmtOffset > 0 then  // 整体向下移动了
          OffsetRect(FPageData.DrawItems[ADrawItemNo].Rect, 0, vFmtOffset);

        vPageDataFmtTop := vPageDataFmtBottom;
        vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;

        _FormatNewPage(ADrawItemNo - 1, ADrawItemNo)  // 新建页
      end
      else
      if FPageData.DrawItems[ADrawItemNo].Rect.Bottom > vPageDataFmtBottom then  // 当前页放不下表格整体(带行间距)
      begin
        if (FPages[vPageIndex].StartDrawItemNo = ADrawItemNo)
          and (AStartSeat = 0)
          and (not vRectItem.CanBreak)
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
        InflateRect(vDrawRect, 0, -FStyle.ParaStyles[vRectItem.ParaNo].LineSpaceHalf);  // 减掉行间距

        vRectItem.CheckFormatPage(  // 去除行间距后，判断表格跨页位置
          vDrawRect.Top,  // 表格的顶部位置 FPageData.DrawItems[ADrawItemNo].Rect.Top,
          vDrawRect.Bottom,  // 表格的底部位置 FPageData.DrawItems[ADrawItemNo].Rect.Bottom,
          vPageDataFmtTop,
          vPageDataFmtBottom,  // 当前页的数据底部位置
          AStartSeat,
          vBreakSeat,  // 当前页分页的行(位置)
          vFmtOffset,  // 当前RectItem为了避开分页位置整体向下偏移的高度
          vFmtHeightInc  // 当前行各列为了避开分页位置单元格内容额外偏移的最大高度
          );

        if vBreakSeat > 0 then  // 从vBreakSeat位置跨页
        begin
          //vFmtHeightInc := vFmtHeightInc - FStyle.ParaStyles[vRectItem.ParaNo].LineSpaceHalf;
          vSuplus := vSuplus + vFmtOffset + vFmtHeightInc;
          FPageData.DrawItems[ADrawItemNo].Rect.Bottom :=  // 扩充格式化高度
            FPageData.DrawItems[ADrawItemNo].Rect.Bottom + vFmtHeightInc;
          vRectItem.Height := vRectItem.Height + vFmtHeightInc;  // 是在这里处理呢还是在RectItem内部更合适？

          vPageDataFmtTop := vPageDataFmtBottom;
          vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;
          _FormatNewPage(ADrawItemNo, ADrawItemNo);  // 新建页
          _RectItemCheckPage(vBreakSeat);  // 从分页位置后面继续检查是否分页
        end
        else
        if vBreakSeat < 0 then // 除去行间距后不用跨页就可以显示了
        begin
          vSuplus := vSuplus + vPageDataFmtBottom - vDrawRect.Bottom;
        end
        else  // 整体跨页，整体向下移动了
        begin
          vSuplus := vSuplus + vFmtOffset + vFmtHeightInc;

          OffsetRect(FPageData.DrawItems[ADrawItemNo].Rect, 0, vFmtOffset);

          //FPageData.DrawItems[ADrawItemNo].Rect.Bottom :=  // 扩充格式化高度
          //  FPageData.DrawItems[ADrawItemNo].Rect.Bottom + vFmtHeightInc;
          //vRectItem.Height := vRectItem.Height + vFmtHeightInc;  // 是在这里处理呢还是在RectItem内部更合适？

          vPageDataFmtTop := vPageDataFmtBottom;
          vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;
          _FormatNewPage(ADrawItemNo - 1, ADrawItemNo);  // 新建页
          _RectItemCheckPage(vBreakSeat);
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

    _RectItemCheckPage(0);  // 从最开始位置，检测表格各行内容是否能显示在当前页

    if vSuplus <> 0 then
    begin
      for i := ADrawItemNo + 1 to FPageData.DrawItems.Count - 1 do
        OffsetRect(FPageData.DrawItems[i].Rect, 0, vSuplus);
    end;
  end;
  {$ENDREGION}

  {$REGION '_FormatTextItemCheckPage'}
  procedure _FormatTextItemCheckPage(const ADrawItemNo: Integer);
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
  if AStartItemNo > 0 then
    vPrioDrawItemNo := FPageData.GetItemLastDrawItemNo(AStartItemNo - 1)  // 上一个最后的DItem
  else
    vPrioDrawItemNo := -1;

  // 上一个DrawItemNo所在页作为格式化起始页
  vPageIndex := -1;
  if vPrioDrawItemNo < 0 then  // 没有DrawItem按第0页
    vPageIndex := 0
  else  // 指定了DrawItemNo
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

  // 因为行首可能是分页，所以需要从行首开始判断跨页
  for i := FPageData.Items[AStartItemNo].FirstDItemNo downto 0 do
  begin
    if FPageData.DrawItems[i].LineFirst then
    begin
      vPrioDrawItemNo := i;
      Break;
    end;
  end;

  if vPrioDrawItemNo = FPages[vPageIndex].StartDrawItemNo then  // 行首是页的第一个DrawItem
  begin
    FPages.DeleteRange(vPageIndex, FPages.Count - vPageIndex);  // 删除当前页一直到最后

    // 从上一页最后开始计算分页
    Dec(vPageIndex);
    if vPageIndex >= 0 then
      FPages[vPageIndex].EndDrawItemNo := -1;
  end
  else  // 行首不是页的第一个DrawItem
    FPages.DeleteRange(vPageIndex + 1, FPages.Count - vPageIndex - 1);  // 删除当前页后面的，准备格式化

  if FPages.Count = 0 then  // 删除没了，补充第一个Page
  begin
    vPage := THCPage.Create;
    vPage.StartDrawItemNo := vPrioDrawItemNo;
    FPages.Add(vPage);
    vPageIndex := 0;
  end;

  vPageDataFmtTop := GetPageDataFmtTop(vPageIndex);
  vContentHeight := GetContentHeight;
  vPageDataFmtBottom := vPageDataFmtTop + vContentHeight;

  for i := vPrioDrawItemNo to FPageData.DrawItems.Count - 1 do
  begin
    if FPageData.DrawItems[i].LineFirst then
    begin
      if FPageData.Items[FPageData.DrawItems[i].ItemNo].StyleNo < THCStyle.RsNull then
        _FormatRectItemCheckPage(i)
      else
        _FormatTextItemCheckPage(i);
    end;
  end;

  FPages[vPageIndex].EndDrawItemNo := FPageData.DrawItems.Count - 1;
  FActivePageIndex := GetCurrentPage;
end;

procedure THCSection.Redo(const ARedo: THCUndo);
begin
  if FActiveData <> ARedo.Data then
    SetActiveData(ARedo.Data as THCCustomSectionData);

  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.Redo(ARedo);
    end);
end;

procedure THCSection.ReFormatActiveItem;
begin
  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.ReFormatActiveItem;
    end);
end;

procedure THCSection.ReMarginPaper;
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

procedure THCSection.SaveToStream(const AStream: TStream;
  const ASaveParts: TSaveParts = [saHeader, saData, saFooter]);
var
  vBegPos, vEndPos: Int64;
  vArea: Boolean;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 数据大小占位，便于越过
  //
  if ASaveParts <> [] then
  begin
    AStream.WriteBuffer(FSymmetryMargin, SizeOf(FSymmetryMargin));
    FPageSize.SaveToStream(AStream);  // 页面参数

    vArea := saHeader in ASaveParts;  // 存页眉
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    vArea := saFooter in ASaveParts;  // 存页脚
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    vArea := saData in ASaveParts;  // 存页面
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    if saHeader in ASaveParts then  // 存页眉
    begin
      AStream.WriteBuffer(FHeaderOffset, SizeOf(FHeaderOffset));
      FHeader.SaveToStream(AStream);
    end;

    if saFooter in ASaveParts then  // 存页脚
      FFooter.SaveToStream(AStream);

    if saData in ASaveParts then  // 存页面
      FPageData.SaveToStream(AStream);
  end;
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 当前节数据大小
  AStream.Position := vEndPos;
end;

procedure THCSection.SectionCoordToPage(const APageIndex, X, Y: Integer; var APageX,
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

procedure THCSection.SelectAll;
begin
  FActiveData.SelectAll;
end;

function THCSection.SelectExists: Boolean;
begin
  //Result := False;
  //if FActiveData <> nil then
    Result := FActiveData.SelectExists
end;

procedure THCSection.SetHeaderOffset(const Value: Integer);
begin
  if FHeaderOffset <> Value then
  begin
    FHeaderOffset := Value;
    BuildSectionPages(0);
    DoDataChanged(Self);
  end;
end;

procedure THCSection.SetPageHeightPix(const Value: Integer);
begin
  if FPageSize.PageHeightPix <> Value then
    FPageSize.PageHeightPix := Value;
end;

procedure THCSection.SetPageMarginBottomPix(const Value: Integer);
begin
  if FPageSize.PageMarginBottomPix <> Value then
    FPageSize.PageMarginBottomPix := Value;
end;

procedure THCSection.SetPageMarginLeftPix(const Value: Integer);
begin
  if FPageSize.PageMarginLeftPix <> Value then
    FPageSize.PageMarginLeftPix := Value;
end;

procedure THCSection.SetPageMarginRightPix(const Value: Integer);
begin
  if FPageSize.PageMarginRightPix <> Value then
    FPageSize.PageMarginRightPix := Value;
end;

procedure THCSection.SetPageMarginTopPix(const Value: Integer);
begin
  if FPageSize.PageMarginTopPix <> Value then
    FPageSize.PageMarginTopPix := Value;
end;

procedure THCSection.SetPageWidthPix(const Value: Integer);
begin
  if FPageSize.PageWidthPix <> Value then
    FPageSize.PageWidthPix := Value;
end;

procedure THCSection.SetPaperHeight(const Value: Single);
begin
  FPageSize.PaperHeight := Value;
  FPageSize.PaperSize := DMPAPER_USER;
end;

procedure THCSection.SetPaperMarginBottom(const Value: Single);
begin
  FPageSize.PaperMarginBottom := Value;
end;

procedure THCSection.SetPaperMarginLeft(const Value: Single);
begin
  FPageSize.PaperMarginLeft := Value;
end;

procedure THCSection.SetPaperMarginRight(const Value: Single);
begin
  FPageSize.PaperMarginRight := Value;
end;

procedure THCSection.SetPaperMarginTop(const Value: Single);
begin
  FPageSize.PaperMarginTop := Value;
end;

procedure THCSection.SetPaperSize(const Value: Integer);
begin
  FPageSize.PaperSize := Value;
end;

procedure THCSection.SetPaperWidth(const Value: Single);
begin
  FPageSize.PaperWidth := Value;
  FPageSize.PaperSize := DMPAPER_USER;
end;

procedure THCSection.SetReadOnly(const Value: Boolean);
begin
  FHeader.ReadOnly := Value;
  FFooter.ReadOnly := Value;
  FPageData.ReadOnly := Value;
end;

procedure THCSection.Undo(const AUndo: THCUndo);
begin
  if FActiveData <> AUndo.Data then
    SetActiveData(AUndo.Data as THCCustomSectionData);

  ActiveDataChangeByAction(function(): Boolean
    begin
      FActiveData.Undo(AUndo);
    end);
end;

end.
