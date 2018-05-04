{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  Windows, Classes, Controls, Graphics, Messages, HCStyle, HCCustomData,
  Generics.Collections, HCCommon, HCCustomRichData, HCDrawItem, HCSection,
  HCScrollBar, HCRichScrollBar, HCParaStyle, HCTextStyle, HCItem;

type
  TPageScrollModel = (psmVertical, psmHorizontal);

  TLoadSectionProc = reference to procedure(const AFileVersion: Word);

  TAnnotation = class(TObject)  // 批注
  private
    FDrawItemRect, FPaintRect: TRect;
    FText: string;
  public
    property DrawItemRect: TRect read FDrawItemRect write FDrawItemRect;
    property PaintRect: TRect read FPaintRect write FPaintRect;
    property Text: string read FText write FText;
  end;

  TAnnotations = class(TObjectList<TAnnotation>)  // 批注s
  strict private
    FIndex: Integer;
  public
    constructor Create;
    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRect;
      const APaintInfo: TSectionPaintInfo);
    procedure AddAnnotation(const ADrawItemRect: TRect; const AText: string);
    procedure MouseDown(const X, Y: Integer);
  end;

  THCView = class(TCustomControl)
  private
    { Private declarations }
    FStyle: THCStyle;
    FSections: TObjectList<THCSection>;
    FHScrollBar: THCScrollBar;
    FVScrollBar: THCRichScrollBar;
    FDataBmp: TBitmap;  // 数据显示位图
    FActiveSectionIndex,
    FDisplayFirstSection, FDisplayLastSection,
    FUpdateCount: Integer;
    FZoom: Single;
    FShowAnnotation: Boolean;  // 显示批注
    FIsChanged: Boolean;  // 是否发生了改变
    FAnnotations: TAnnotations;  // 批注

    FViewModel: TViewModel;  // 界面显示模式：页面、Web
    FPageScrollModel: TPageScrollModel;  // 页面滚动显示模式：纵向、横向
    FCaret: TCaret;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnCaretChange: TNotifyEvent;
    FOnVerScroll: TNotifyEvent;
    FOnInsertItem: TItemNotifyEvent;
    FOnItemPaintAfter, FOnItemPaintBefor: TItemPaintEvent;

    FOnPaintHeader, FOnPaintFooter, FOnPaintData: TSectionPagePaintEvent;
    FOnChange, FOnChangedSwitch: TNotifyEvent;
    FOnPaintPage: TSectionPagePaintEvent;
    //
    function GetDisplayWidth: Integer;
    function GetDisplayHeight: Integer;
    //
    function GetSymmetryMargin: Boolean;
    procedure SetSymmetryMargin(const Value: Boolean);
    procedure DoVScrollChange(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    //
    function NewDefaultSection: THCSection;

    /// <summary> 重新获取光标位置 </summary>
    procedure ReBuildCaret(const AScrollBar: Boolean = False);
    procedure GetSectionByCrood(const X, Y: Integer; var ASectionIndex: Integer);
    procedure SetZoom(const Value: Single);

    /// <summary> 删除不使用的文本样式 </summary>
    procedure _DeleteUnUsedStyle(const AParts: TSaveParts = [saHeader, saData, saFooter]);

    function GetHScrollValue: Integer;
    function GetVScrollValue: Integer;
    function GetShowLineActiveMark: Boolean;
    function GetShowLineNo: Boolean;
    function GetShowUnderLine: Boolean;
    procedure SetShowLineActiveMark(Value: Boolean);
    procedure SetShowLineNo(Value: Boolean);
    procedure SetShowUnderLine(Value: Boolean);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    // Imm
    procedure UpdateImmPosition;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoCaretChange;
    procedure DoSectionDataChanged(Sender: TObject);
    procedure DoSectionDataCheckUpdateInfo(Sender: TObject);
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const ALoadSectionProc: TLoadSectionProc);

    /// <summary> 文档"背板"变动(数据无变化，如对称边距，缩放视图) </summary>
    procedure DoMapChanged;
    procedure DoChange; virtual;
    procedure DoSectionInsertItem(const AItem: THCCustomItem);
    procedure DoSectionItemPaintBefor(const AData: THCCustomData;
      const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DoSectionItemPaintAfter(const AData: THCCustomData;
      const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    procedure DoSectionGetPageInfo(Sender: THCSection; var AStartPageIndex,
      AAllPageCount: Integer);
    procedure DoPaintPage(Sender: THCSection; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    /// <summary>
    /// 是否不上屏输入法输入的词条屏词条ID和词条
    /// </summary>
    /// <param name="ACandiID">词条ID</param>
    /// <param name="ACandi">词条</param>
    /// <returns></returns>
    function DoProcessIMECandi(const ACandi: string): Boolean; virtual;
    /// <summary> 复制前，便于订制特征数据如内容来源 </summary>
    procedure DoCopyDataBefor(const AStream: TStream); virtual;

    /// <summary> 粘贴前，便于确认订制特征数据如内容来源 </summary>
    procedure DoPasteDataBefor(const AStream: TStream; const AVersion: Word); virtual;

    /// <summary> 保存文档前触发事件，便于订制特征数据 </summary>
    procedure DoSaveBefor(const AStream: TStream); virtual;

    /// <summary> 保存文档后触发事件，便于订制特征数据 </summary>
    procedure DoSaveAfter(const AStream: TStream); virtual;

    /// <summary> 读取文档前触发事件，便于确认订制特征数据 </summary>
    procedure DoLoadBefor(const AStream: TStream; const AFileVersion: Word); virtual;

    /// <summary> 读取文档后触发事件，便于确认订制特征数据 </summary>
    procedure DoLoadAfter(const AStream: TStream; const AFileVersion: Word); virtual;
    //
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    // 消息
    /// <summary> 响应Tab键和方向键 </summary>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;

    // 接收输入法输入的内容
    procedure WMImeComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WndProc(var Message: TMessage); override;
    //
    procedure CalcScrollRang;

    /// <summary> 是否由滚动条位置变化引起的更新 </summary>
    procedure CheckUpdateInfo(const AScrollBar: Boolean = False);
    //
    procedure SetPageScrollModel(const Value: TPageScrollModel);
    procedure SetViewModel(const Value: TViewModel);
    procedure SetShowAnnotation(const Value: Boolean);

    procedure SetOnPaintHeader(const Value: TSectionPagePaintEvent);
    procedure SetOnPaintFooter(const Value: TSectionPagePaintEvent);
    procedure SetOnPaintData(const Value: TSectionPagePaintEvent);
    procedure SetOnInsertItem(const Value: TItemNotifyEvent);
    procedure SetOnItemPaintAfter(const Value: TItemPaintEvent);
    procedure SetOnItemPaintBefor(const Value: TItemPaintEvent);

    function GetOnReadOnlySwitch: TNotifyEvent;
    procedure SetOnReadOnlySwitch(const Value: TNotifyEvent);

    function GetOnCreateItem: TNotifyEvent;
    procedure SetOnCreateItem(const Value: TNotifyEvent);
    //
    procedure SetIsChanged(const Value: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    /// <summary> 修改纸张边距 </summary>
    procedure ReMarginPaper;
    procedure Clear;
    procedure DisSelect;
    procedure DeleteSelected;
    procedure DeleteSection;
    procedure FormatData;

    function InsertStream(const AStream: TStream): Boolean;
    function InsertText(const AText: string): Boolean;
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
    function InsertLine(const ALineHeight: Integer): Boolean;
    function InsertItem(const AItem: THCCustomItem): Boolean; overload;
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem): Boolean; overload;
    function InsertBreak: Boolean;
    function InsertPageBreak: Boolean;
    function InsertPageSeparator: Boolean;
    // 表格插入行列
    function ActiveTableInsertRowAfter(const ARowCount: Byte): Boolean;
    function ActiveTableInsertRowBefor(const ARowCount: Byte): Boolean;
    function ActiveTableDeleteRow(const ARowCount: Byte): Boolean;
    function ActiveTableInsertColAfter(const AColCount: Byte): Boolean;
    function ActiveTableInsertColBefor(const AColCount: Byte): Boolean;
    function ActiveTableDeleteCol(const AColCount: Byte): Boolean;
    //
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);
    procedure ApplyParaBackColor(const AColor: TColor);
    procedure ApplyParaLineSpace(const ASpace: Integer);
    procedure ApplyTextStyle(const AFontStyle: TFontStyleEx);
    procedure ApplyTextFontName(const AFontName: TFontName);
    procedure ApplyTextFontSize(const AFontSize: Integer);
    procedure ApplyTextColor(const AColor: TColor);
    procedure ApplyTextBackColor(const AColor: TColor);

    procedure Cut;
    procedure Copy;
    procedure CopyAsText;
    procedure Paste;
    function ZoomIn(const Value: Integer): Integer;
    function ZoomOut(const Value: Integer): Integer;
    //
    procedure UpdateBuffer;
    procedure BeginUpdate;
    procedure EndUpdate;
    //
    procedure GetCurStyle(var AStyleNo, AParaNo: Integer);
    function GetCurItem: THCCustomItem;
    function GetActiveItem: THCCustomItem;
    function GetActiveDrawItem: THCCustomDrawItem;
    function GetActivePageIndex: Integer;
    function GetPagePreviewFirst: Integer;
    function GetPageCount: Integer;
    function GetSectionDrawLeft(const ASectionNo: Integer): Integer;

    /// <summary>
    /// 获取指定节页眉区域实际高(内容高度>上边距时取内容高度)
    /// </summary>
    /// <param name="ASectionNo">页眉序号</param>
    /// <returns></returns>
    //function GetHeaderAreaHeight(const ASectionIndex: Integer): Integer;
    function ActiveSection: THCSection;

    /// <summary> 节在整个胶卷中的Top位置 </summary>
    /// <param name="ASectionIndex"></param>
    /// <returns></returns>
    function GetSectionTopFilm(const ASectionIndex: Integer): Integer;
    // 保存文档
    procedure SaveToText(const AFileName: string);
    procedure SaveToBitmap(const AFileName: string);
    procedure SaveToFile(const AFileName: string);

    /// <summary> 保存内容到流 </summary>
    procedure SaveToStream(const AStream: TStream;
      const ASaveParts: TSaveParts = [saHeader, saData, saFooter]); virtual;
    // 读取文档
    procedure LoadFromText(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(const AStream: TStream); virtual;
    //
    function Print(const APrinter: string): TPrintResult;
    function PrintPageRang(const AStartPageNo, AEndPageNo: Integer): TPrintResult;
    function MergeTableSelectCells: Boolean;
    //
    property Style: THCStyle read FStyle;

    /// <summary> 是否对称边距 </summary>
    property SymmetryMargin: Boolean read GetSymmetryMargin write SetSymmetryMargin;
    property ActivePageIndex: Integer read GetActivePageIndex;
    property PagePreviewFirst: Integer read GetPagePreviewFirst;
    property PageCount: Integer read GetPageCount;
    property ActiveSectionIndex: Integer read FActiveSectionIndex;
    property HScrollValue: Integer read GetHScrollValue;
    property VScrollValue: Integer read GetVScrollValue;
    property Zoom: Single read FZoom write SetZoom;
    property Sections: TObjectList<THCSection> read FSections;
    property ShowLineActiveMark: Boolean read GetShowLineActiveMark write SetShowLineActiveMark;
    property ShowLineNo: Boolean read GetShowLineNo write SetShowLineNo;
    property ShowUnderLine: Boolean read GetShowUnderLine write SetShowUnderLine;
    property IsChanged: Boolean read FIsChanged write SetIsChanged;
    property Annotations: TAnnotations read FAnnotations;
    property OnCreateItem: TNotifyEvent read GetOnCreateItem write SetOnCreateItem;
  published
    { Published declarations }
    property PageScrollModel: TPageScrollModel read FPageScrollModel write SetPageScrollModel;
    property ViewModel: TViewModel read FViewModel write SetViewModel;

    /// <summary> 是否显示批注 </summary>
    property ShowAnnotation: Boolean read FShowAnnotation write SetShowAnnotation;

    /// <summary> 所有Section是否只读 </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnCaretChange: TNotifyEvent read FOnCaretChange write FOnCaretChange;
    property OnVerScroll: TNotifyEvent read FOnVerScroll write FOnVerScroll;
    property OnItemInsert: TItemNotifyEvent read FOnInsertItem write SetOnInsertItem;
    property OnItemPaintAfter: TItemPaintEvent read FOnItemPaintAfter write SetOnItemPaintAfter;
    property OnItemPaintBefor: TItemPaintEvent read FOnItemPaintBefor write SetOnItemPaintBefor;
    property OnPaintHeader: TSectionPagePaintEvent read FOnPaintHeader write SetOnPaintHeader;
    property OnPaintFooter: TSectionPagePaintEvent read FOnPaintFooter write SetOnPaintFooter;
    property OnPaintData: TSectionPagePaintEvent read FOnPaintData write SetOnPaintData;
    property OnPaintPage: TSectionPagePaintEvent read FOnPaintPage write FOnPaintPage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangedSwitch: TNotifyEvent read FOnChangedSwitch write FOnChangedSwitch;
    property OnReadOnlySwitch: TNotifyEvent read GetOnReadOnlySwitch write SetOnReadOnlySwitch;
    property PopupMenu;
  end;

//procedure Register;

implementation

uses
  Printers, Imm, SysUtils, Forms, Math, Clipbrd;

const
  IMN_UPDATECURSTRING = $F000;  // 和输入法交互，当前光标处的字符串

{procedure Register;
begin
  RegisterComponents('HCControl', [THCView]);
end;  }

{ THCView }

procedure THCView.ApplyTextStyle(const AFontStyle: TFontStyleEx);
begin
  ActiveSection.ApplyTextStyle(AFontStyle);
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

procedure THCView.ApplyTextFontSize(const AFontSize: Integer);
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
  for i := 0 to FSections.Count - 1 do
  begin
    vVMax := vVMax + FSections[i].GetFilmHeight;

    vWidth := FSections[i].PageWidthPix;

    if vWidth > vHMax then
      vHMax := vWidth;
  end;

  if FShowAnnotation then
    vHMax := vHMax + AnnotationWidth;

  vVMax := ZoomIn(vVMax + MinPadding);
  vHMax := ZoomIn(vHMax + MinPadding + MinPadding);

  FVScrollBar.Max := vVMax;
  FHScrollBar.Max := vHMax;
end;

procedure THCView.CheckUpdateInfo(const AScrollBar: Boolean = False);
begin
  if (FCaret <> nil) and FStyle.UpdateInfo.ReCaret then  // 先处理光标，因为可能光标处有些需要高亮重绘
  begin
    FStyle.UpdateInfo.ReCaret := False;
    ReBuildCaret(AScrollBar);
    UpdateImmPosition;
  end;

  if FStyle.UpdateInfo.RePaint then
  begin
    FStyle.UpdateInfo.RePaint := False;
    UpdateBuffer;
  end;
end;

procedure THCView.Clear;
begin
  FSections.DeleteRange(1, FSections.Count - 1);
  FSections[0].Clear;
  FStyle.Initialize;
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
      ActiveSection.ActiveData.GetTopLevelData.SaveSelectToStream(vStream);
      vMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, vStream.Size);
      if vMem = 0 then
        raise Exception.Create(CFE_EXCEPTION + '复制时没有申请到足够的内存！');
      vPtr := GlobalLock(vMem);
      Move(vStream.Memory^, vPtr^, vStream.Size);
      GlobalUnlock(vMem);
    finally
      vStream.Free;
    end;
    
    Clipboard.Clear;
    Clipboard.Open;
    try
      Clipboard.SetAsHandle(HC_FILEFORMAT, vMem);
    finally
      Clipboard.Close;
    end;
  end;
end;

procedure THCView.CopyAsText;
begin
  Clipboard.AsText := ActiveSection.ActiveData.GetTopLevelData.SaveSelectToText;
end;

constructor THCView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //
  FIsChanged := False;
  FZoom := 1;
  FShowAnnotation := False;
  FViewModel := vmPage;
  FPageScrollModel := psmVertical;  //psmHorizontal;

  FDataBmp := TBitmap.Create;
  FStyle := THCStyle.CreateEx(True, True);
  FSections := TObjectList<THCSection>.Create;
  FSections.Add(NewDefaultSection);
  FActiveSectionIndex := 0;
  FDisplayFirstSection := 0;
  FDisplayLastSection := 0;
  // 垂直滚动条，范围在Resize中设置
  FVScrollBar := THCRichScrollBar.Create(Self);
  FVScrollBar.Parent := Self;
  FVScrollBar.Orientation := TOrientation.oriVertical;
  FVScrollBar.OnScroll := DoVScrollChange;
  // 水平滚动条，范围在Resize中设置
  FHScrollBar := THCScrollBar.Create(Self);
  FHScrollBar.Parent := Self;
  FHScrollBar.Orientation := TOrientation.oriHorizontal;
  FHScrollBar.OnScroll := DoVScrollChange;

  CalcScrollRang;
end;

procedure THCView.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    FCaret := TCaret.Create(Handle);
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

procedure THCView.DeleteSection;
begin
  if FActiveSectionIndex > 0 then
  begin
    FSections.Delete(FActiveSectionIndex);
    FActiveSectionIndex := FActiveSectionIndex - 1;
    FDisplayFirstSection := -1;
    FDisplayLastSection := -1;
    FStyle.UpdateInfoReCaret;
    FStyle.UpdateInfoRePaint;

    DoChange;
  end;
end;

procedure THCView.DeleteSelected;
begin
  ActiveSection.DeleteSelected;
end;

destructor THCView.Destroy;
begin
  if Assigned(FAnnotations) then
    FreeAndNil(FAnnotations);

  FreeAndNil(FSections);
  FreeAndNil(FCaret);
  FreeAndNil(FHScrollBar);
  FreeAndNil(FVScrollBar);
  FreeAndNil(FDataBmp);
  FreeAndNil(FStyle);
  inherited Destroy;
end;

procedure THCView.DisSelect;
begin
  ActiveSection.DisSelect;
  //DoMapChanged;
  DoSectionDataCheckUpdateInfo(Self);
end;

function THCView.GetDisplayHeight: Integer;
begin
  if FHScrollBar.Visible then
    Result := Height - FHScrollBar.Height
  else
    Result := Height;
end;

function THCView.GetDisplayWidth: Integer;
begin
  if FVScrollBar.Visible then
    Result := Width - FVScrollBar.Width
  else
    Result := Width;
end;

function THCView.GetHScrollValue: Integer;
begin
  Result := FHScrollBar.Position;
end;

function THCView.GetOnCreateItem: TNotifyEvent;
begin
  Result := FSections[0].PageData.OnCreateItem;
end;

function THCView.GetOnReadOnlySwitch: TNotifyEvent;
begin
  Result := FSections[0].OnReadOnlySwitch;
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
  if FPageScrollModel = psmVertical then
    FVScrollBar.Position := FVScrollBar.Position - WheelDelta div 1
  else
    FHScrollBar.Position := FHScrollBar.Position - WheelDelta div 1;
  Result := True;
end;

procedure THCView.DoPaintPage(Sender: THCSection; const APageIndex: Integer;
  const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  vDCState: Integer;
begin
  if FShowAnnotation then  // 绘制批注
  begin
    vDCState := Windows.SaveDC(ACanvas.Handle);
    try
      FAnnotations.PaintTo(ACanvas, ARect, APaintInfo);
    finally
      Windows.RestoreDC(ACanvas.Handle, vDCState);
    end;
  end;

  if Assigned(FOnPaintPage) then
  begin
    vDCState := Windows.SaveDC(ACanvas.Handle);
    try
      FOnPaintPage(Sender, APageIndex, ARect, ACanvas, APaintInfo);
    finally
      Windows.RestoreDC(ACanvas.Handle, vDCState);
    end;
  end;
end;

procedure THCView.DoPasteDataBefor(const AStream: TStream; const AVersion: Word);
begin
end;

function THCView.DoProcessIMECandi(const ACandi: string): Boolean;
begin
  Result := False;
end;

procedure THCView.DoSaveAfter(const AStream: TStream);
begin
  SetIsChanged(False);
end;

procedure THCView.DoSaveBefor(const AStream: TStream);
begin
  // 用于外部程序存储自定义数据，如上次浏览位置等
end;

procedure THCView.DoSectionDataChanged(Sender: TObject);
begin
  DoChange;
end;

procedure THCView.DoSectionDataCheckUpdateInfo(Sender: TObject);
begin
  if FUpdateCount = 0 then
    CheckUpdateInfo;
end;

procedure THCView.DoSectionGetPageInfo(Sender: THCSection;
  var AStartPageIndex, AAllPageCount: Integer);
var
  i, vSectionIndex: Integer;
begin
  AAllPageCount := 0;
  AStartPageIndex := 0;
  vSectionIndex := FSections.IndexOf(Sender);
  for i := 0 to FSections.Count - 1 do
  begin
    if i = vSectionIndex then
      AStartPageIndex := AAllPageCount;

    AAllPageCount := AAllPageCount + FSections[i].PageCount;
  end;
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

procedure THCView.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const ALoadSectionProc: TLoadSectionProc);
var
  vFileExt, vFileVersion: string;
  viVersion: Word;
begin
  AStream.Position := 0;
  _LoadFileFormatAndVersion(AStream, vFileExt, vFileVersion);  // 文件格式和版本
  if (vFileExt <> HC_EXT) and (vFileExt <> 'cff.') then
    raise Exception.Create('加载失败，不是' + HC_EXT + '文件！');

  viVersion := GetVersionAsInteger(vFileVersion);

  DoLoadBefor(AStream, viVersion);  // 触发加载前事件
  AStyle.LoadFromStream(AStream, viVersion);  // 加载样式表
  ALoadSectionProc(viVersion);  // 加载节数量、节数据
  DoMapChanged;
end;

procedure THCView.DoSectionInsertItem(const AItem: THCCustomItem);
begin
  if Assigned(FOnInsertItem) then
    FOnInsertItem(AItem);
end;

procedure THCView.DoSectionItemPaintAfter(const AData: THCCustomData;
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

procedure THCView.DoSectionItemPaintBefor(const AData: THCCustomData;
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

procedure THCView.DoLoadAfter(const AStream: TStream; const AFileVersion: Word);
begin
end;

procedure THCView.DoLoadBefor(const AStream: TStream; const AFileVersion: Word);
begin
end;

procedure THCView.EndUpdate;
begin
  Dec(FUpdateCount);
  DoMapChanged;
end;

procedure THCView.FormatData;
var
  i: Integer;
begin
  for i := 0 to Sections.Count - 1 do
    Sections[i].FormatData;

  FStyle.UpdateInfoReCaret;
  FStyle.UpdateInfoRePaint;
  DoMapChanged;
end;

function THCView.GetActiveDrawItem: THCCustomDrawItem;
begin
  Result := ActiveSection.GetActiveDrawItem;
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

function THCView.GetCurItem: THCCustomItem;
begin
  Result := ActiveSection.GetCurItem;
end;

procedure THCView.GetCurStyle(var AStyleNo, AParaNo: Integer);
begin
  ActiveSection.GetCurStyle(AStyleNo, AParaNo);
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
end;

function THCView.GetSectionDrawLeft(const ASectionNo: Integer): Integer;
begin
  if FShowAnnotation then  // 显示批注
    Result := Max((GetDisplayWidth - ZoomIn(FSections[ASectionNo].PageWidthPix + AnnotationWidth)) div 2, ZoomIn(MinPadding))
  else
    Result := Max((GetDisplayWidth - ZoomIn(FSections[ASectionNo].PageWidthPix)) div 2, ZoomIn(MinPadding));
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

function THCView.InsertBreak: Boolean;
begin
  Result := Self.ActiveSection.InsertBreak;
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

function THCView.InsertPageSeparator: Boolean;
var
  vSection: THCSection;
begin
  Result := False;
  vSection := NewDefaultSection;
  FSections.Insert(FActiveSectionIndex + 1, vSection);
  FActiveSectionIndex := FActiveSectionIndex + 1;
  Result := True;
  FStyle.UpdateInfoReCaret;
  FStyle.UpdateInfoRePaint;
  DoChange;
end;

function THCView.InsertStream(const AStream: TStream): Boolean;
var
  vStyle: THCStyle;
begin
  Result := False;

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
            ActiveSection.InsertStream(vDataStream, vStyle, AFileVersion);  // 只插入第一节的数据
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
end;

function THCView.ActiveTableDeleteCol(const AColCount: Byte): Boolean;
begin
  Result := activeSection.ActiveTableDeleteCol(AColCount);
end;

function THCView.ActiveTableDeleteRow(const ARowCount: Byte): Boolean;
begin
  Result := ActiveSection.ActiveTableDeleteRow(ARowCount);
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

function THCView.InsertTable(const ARowCount, AColCount: Integer): Boolean;
begin
  Self.BeginUpdate;
  try
    Result := ActiveSection.InsertTable(ARowCount, AColCount);
  finally
    Self.EndUpdate
  end;
end;

function THCView.InsertText(const AText: string): Boolean;
begin
  Self.BeginUpdate;
  try
    Result := ActiveSection.InsertText(AText);
  finally
    Self.EndUpdate;
  end;
end;

procedure THCView.KeyDown(var Key: Word; Shift: TShiftState);

  {$REGION '快捷键'}
  function IsCopyShortKey(Key: Word; Shift: TShiftState): Boolean;
  begin
    Result := (ssCtrl in Shift) and (Key = ord('C')) and not (ssAlt in Shift);
  end;

  function IsCopyTextShortKey(Key: Word; Shift: TShiftState): Boolean;
  begin
    Result := (ssCtrl in Shift) and (ssShift in Shift) and (Key = ord('C')) and not (ssAlt in Shift);
  end;

  function IsCutShortKey(Key: Word; Shift: TShiftState): Boolean;
  begin
    Result := (ssCtrl in Shift) and (Key = ord('X')) and not (ssAlt in Shift);
  end;

  function IsPasteShortKey(Key: Word; Shift: TShiftState): Boolean;
  begin
    Result := (ssCtrl in Shift) and (Key = ord('V')) and not (ssAlt in Shift);
  end;
  {$ENDREGION}

begin
  inherited;
  if IsCopyTextShortKey(Key, Shift) then
    Self.CopyAsText
  else
  if IsCopyShortKey(Key, Shift) then
    Self.Copy
  else
  if IsCutShortKey(Key, Shift) then
    Self.Cut
  else
  if IsPasteShortKey(Key, Shift) then
    Self.Paste
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

procedure THCView.LoadFromFile(const AFileName: string);
var
  vStream: TStream;
begin
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
end;

procedure THCView.LoadFromText(const AFileName: string);
begin
  Self.Clear;
  FStyle.Initialize;
  ActiveSection.LoadFromText(AFileName, TEncoding.ASCII);
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
  inherited;

  GetSectionByCrood(ZoomOut(FHScrollBar.Position + X), ZoomOut(FVScrollBar.Position + Y), vSectionIndex);
  if vSectionIndex <> FActiveSectionIndex then
  begin
    if FActiveSectionIndex >= 0 then
      FSections[FActiveSectionIndex].DisActive;
    FActiveSectionIndex := vSectionIndex;
  end;
  if FActiveSectionIndex < 0 then Exit;

  vSectionDrawLeft := GetSectionDrawLeft(FActiveSectionIndex);

  if FShowAnnotation then  // 批注在显示
  begin
    if (X > vSectionDrawLeft + FSections[FActiveSectionIndex].PageWidthPix)
      and (X < vSectionDrawLeft + FSections[FActiveSectionIndex].PageWidthPix + AnnotationWidth)
    then  // 点在批注区域中
    begin
      FAnnotations.MouseDown(X, Y);
      FStyle.UpdateInfoRePaint;
      DoSectionDataCheckUpdateInfo(Self);
      Exit;
    end;
  end;

  // 映射到节页面(白色区域)
  vPt.X := ZoomOut(FHScrollBar.Position + X - vSectionDrawLeft);
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
  inherited;
  //GetSectionByCrood(FHScrollBar.Value + X, FVScrollBar.Value + Y, vSectionIndex);
  if FActiveSectionIndex >= 0 then  // 按下时在节中
  begin
    FSections[FActiveSectionIndex].MouseMove(Shift,
      ZoomOut(FHScrollBar.Position + X - GetSectionDrawLeft(FActiveSectionIndex)),
      ZoomOut(FVScrollBar.Position + Y) - GetSectionTopFilm(FActiveSectionIndex));
    if ShowHint then
      ProcessHint;
  end;

  {if FStyle.UpdateInfo.Draging then
    GCursor := crDrag;}

  Cursor := GCursor;
  CheckUpdateInfo;  // 高亮光标下
end;

procedure THCView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button = mbRight then Exit;  // 右键弹出菜单
  //GetSectionByCrood(FHScrollBar.Value + X, FVScrollBar.Value + Y, vSectionIndex);
  if FActiveSectionIndex >= 0 then  // 按下时在节中
    FSections[FActiveSectionIndex].MouseUp(Button, Shift,
      ZoomOut(FHScrollBar.Position + X - GetSectionDrawLeft(FActiveSectionIndex)),
      ZoomOut(FVScrollBar.Position + Y) - GetSectionTopFilm(FActiveSectionIndex));
  Cursor := GCursor;
  CheckUpdateInfo;  // 在选中区域中按下不移动弹起鼠标时需要更新

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

function THCView.NewDefaultSection: THCSection;
begin
  Result := THCSection.Create(FStyle);
  Result.OnDataChanged := DoSectionDataChanged;
  Result.OnCheckUpdateInfo := DoSectionDataCheckUpdateInfo;
  Result.OnInsertItem := DoSectionInsertItem;
  Result.OnItemPaintAfter := DoSectionItemPaintAfter;
  Result.OnItemPaintBefor := DoSectionItemPaintBefor;
  Result.OnGetPageInfo := DoSectionGetPageInfo;
  Result.OnPaintHeader := FOnPaintHeader;
  Result.OnPaintFooter := FOnPaintFooter;
  Result.OnPaintData := FOnPaintData;
  Result.OnPaintPage := DoPaintPage;
end;

procedure THCView.DoVScrollChange(Sender: TObject; ScrollCode: TScrollCode;
    var ScrollPos: Integer);
begin
  FStyle.UpdateInfoReCaret;
  FStyle.UpdateInfoRePaint;
  CheckUpdateInfo(True);
  if Assigned(FOnVerScroll) then
    FOnVerScroll(Self);
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
//  Canvas.Draw(0, 0, FDataBmp);
  BitBlt(Canvas.Handle, 0, 0, GetDisplayWidth, GetDisplayHeight,
      FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure THCView.Paste;
var
  vStream: TMemoryStream;
  vMem: Cardinal;
  vPtr: Pointer;
  vSize, viVersion: Integer;
  vFileFormat, vFileVersion: string;
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
      _LoadFileFormatAndVersion(vStream, vFileFormat, vFileVersion);  // 文件格式和版本
      viVersion := GetVersionAsInteger(vFileVersion);
      DoPasteDataBefor(vStream, viVersion);
      vStyle := THCStyle.Create;
      try
        vStyle.LoadFromStream(vStream, viVersion);
        ActiveSection.InsertStream(vStream, vStyle, viVersion);
      finally
        FreeAndNil(vStyle);
      end;
    finally
      vStream.Free;
    end;
  end
  else
  if Clipboard.HasFormat(CF_TEXT) then
    ActiveSection.InsertText(Clipboard.AsText);
end;

function THCView.Print(const APrinter: string): TPrintResult;
begin
  Result := prError;
  if APrinter <> '' then
    Printer.PrinterIndex := Printer.Printers.IndexOf(APrinter);

  if Printer.PrinterIndex >= 0 then
  begin
    //Printer.Title := FFileName;
    Result := PrintPageRang(1, PageCount);
  end;
end;

function THCView.PrintPageRang(const AStartPageNo,
  AEndPageNo: Integer): TPrintResult;

  // 获取指定页所在的节和相对节的页数
  function GetSectionPageIndexByPageNo(const APageNo: Integer; var ASectionPageIndex: Integer): Integer;
  var
    i, vPageCount: Integer;
  begin
    vPageCount := 0;
    for i := 0 to FSections.Count - 1 do
    begin
      if vPageCount + FSections[i].PageCount >= APageNo then
      begin
        Result := i;
        ASectionPageIndex := APageNo - vPageCount - 1;  // FSections[i].PageCount;
        Break;
      end
      else
        vPageCount := vPageCount + FSections[i].PageCount;
    end;
  end;

  procedure SetPrintBySectionInfo(const ASectionIndex: Integer);
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
      if vPDMode <> nil then
      begin
        {vOlddmPaperSize := vPDMode^.dmPaperSize;
        vOlddmPaperLength := vPDMode^.dmPaperLength;
        vOlddmPaperWidth := vPDMode^.dmPaperWidth;}
        // 发现常用尺寸和直接设置对应长宽后以B5为例 宽差 0.4cm 故先关了
        vPDMode^.dmPaperSize := FSections[ASectionIndex].PaperSize;
        if vPDMode^.dmPaperSize = DMPAPER_USER then
        begin
          vPDMode^.dmPaperSize := DMPAPER_USER;  // 自定义纸张
          vPDMode^.dmPaperLength := Round(FSections[ASectionIndex].PaperHeight * 10); //纸长你可用变量获得纸张的长、宽。
          vPDMode^.dmPaperWidth := Round(FSections[ASectionIndex].PaperWidth * 10);   //纸宽
          vPDMode^.dmFields := vPDMode^.dmFields or DM_PAPERSIZE or DM_PAPERLENGTH or DM_PAPERWIDTH;
        end
      end;

      ResetDC(Printer.Handle, vPDMode^);
      GlobalUnlock(vHDMode);
      //Printer.SetPrinter(vDevice, vDriver, vPort, vHDMode);
    end;
  end;

var
  i, vStartSection, vSectionStartPageIndex,
  vEndSection, vSectionEndPageIndex, vPrintWidth , vPrintHeight,
  vPrintOffsetX, vPrintOffsetY: Integer;
  vPageCanvas: TCanvas;
  vPaintInfo: TSectionPaintInfo;
begin
  Result := prError;
  vStartSection := GetSectionPageIndexByPageNo(AStartPageNo, vSectionStartPageIndex);
  vEndSection := GetSectionPageIndexByPageNo(AEndPageNo, vSectionEndPageIndex);

  vPrintOffsetX := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);  // 90
  vPrintOffsetY := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);  // 99
  vPrintWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  vPrintHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;
    if vStartSection = vEndSection then  // 打印页在同一节
    begin
      SetPrintBySectionInfo(vStartSection);
      vPaintInfo.SectionIndex := vStartSection;

      Printer.BeginDoc;
      try
        vPageCanvas  := TCanvas.Create;
        try
          vPageCanvas.Handle := Printer.Canvas.Handle;  // 为什么不用vPageCanvas中介打印就不行呢？
          for i := vSectionStartPageIndex to vSectionEndPageIndex do
          begin
            vPaintInfo.PageIndex := i;

            FSections[vStartSection].PaintPage(i, vPrintOffsetX, vPrintOffsetY,
              FSections[vStartSection].PageWidthPix,
              FSections[vStartSection].PageHeightPix,
              vPrintWidth / FSections[vStartSection].PageWidthPix,
              vPrintHeight / FSections[vStartSection].PageHeightPix, vPageCanvas,
              vPaintInfo);

            if i < vSectionEndPageIndex then
              Printer.NewPage;
          end;
        finally
          vPageCanvas.Handle := 0;
          vPageCanvas.Free;
        end;
      finally
        Printer.EndDoc;
      end;
    end
    else  // 打印页在不同节
    begin
      {for i := vStartSection + 1 to vEndSection - 1 do
        FSections[i].PrintAll;
      FSections[vEndSection].PrintRang(1, vSecEndPage);}

      Result := prOk;
    end;
  finally
    vPaintInfo.Free;
  end;
end;

procedure THCView.ReBuildCaret(const AScrollBar: Boolean = False);
var
  vCaretInfo: TCaretInfo;
  vDisplayHeight: Integer;
begin
  if not Self.Focused then Exit;

  if FCaret = nil then Exit;

  if (not Style.UpdateInfo.Draging) and ActiveSection.SelectExists then
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
  FCaret.X := GetSectionDrawLeft(FActiveSectionIndex) + ZoomIn(vCaretInfo.X) - FHScrollBar.Position;
  FCaret.Y := ZoomIn(GetSectionTopFilm(FActiveSectionIndex) + vCaretInfo.Y) - FVScrollBar.Position;
  FCaret.Height := ZoomIn(vCaretInfo.Height);

  vDisplayHeight := GetDisplayHeight;
  if AScrollBar then // 滚动条平滑滚动时，可能将光标卷掉看不见
  begin
    if (FCaret.X < 0) or (FCaret.X > GetDisplayWidth) then
    begin
      FCaret.Hide;
      Exit;
    end;

    if (FCaret.Y + FCaret.Height < 0) or (FCaret.Y > vDisplayHeight) then
    begin
      FCaret.Hide;
      Exit;
    end;
  end
  else  // 非滚动条(方向键、点击等)引起的光标位置变化
  begin
    if FCaret.Height < vDisplayHeight then
    begin
      if FCaret.Y < 0 then
        FVScrollBar.Position := FVScrollBar.Position + FCaret.Y - MinPadding
      else
      if FCaret.Y + FCaret.Height + MinPadding > vDisplayHeight then
        FVScrollBar.Position := FVScrollBar.Position + FCaret.Y + FCaret.Height + MinPadding - vDisplayHeight;
    end;
  end;

  if FCaret.Y + FCaret.Height > vDisplayHeight then
    FCaret.Height := vDisplayHeight - FCaret.Y;

  FCaret.Show;
  DoCaretChange;
end;

procedure THCView.ReMarginPaper;
begin
  ActiveSection.ReMarginPaper;
end;

procedure THCView.Resize;
begin
  inherited;
  FDataBmp.SetSize(GetDisplayWidth, GetDisplayHeight);
  if FCaret <> nil then
    FStyle.UpdateInfoReCaret;
  FStyle.UpdateInfoRePaint;
  CheckUpdateInfo;
end;

procedure THCView._DeleteUnUsedStyle(const AParts: TSaveParts = [saHeader, saData, saFooter]);
var
  i, vUnCount: Integer;
begin
  for i := 0 to FStyle.TextStyles.Count - 1 do
  begin
    FStyle.TextStyles[i].CheckSaveUsed := False;
    FStyle.TextStyles[i].TempNo := THCStyle.RsNull;
  end;
  for i := 0 to FStyle.ParaStyles.Count - 1 do
  begin
    FStyle.ParaStyles[i].CheckSaveUsed := False;
    FStyle.ParaStyles[i].TempNo := THCStyle.RsNull;
  end;

  for i := 0 to FSections.Count - 1 do
    FSections[i].MarkStyleUsed(True, AParts);

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

procedure THCView.SaveToBitmap(const AFileName: string);
begin

end;

procedure THCView.SaveToFile(const AFileName: string);
var
  vStream: TStream;
begin
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure THCView.SaveToStream(const AStream: TStream;
  const ASaveParts: TSaveParts = [saHeader, saData, saFooter]);
var
  vByte: Byte;
  i: Integer;
begin
  _SaveFileFormatAndVersion(AStream);  // 文件格式和版本
  DoSaveBefor(AStream);
  _DeleteUnUsedStyle(ASaveParts);  // 删除不使用的样式(可否改为把有用的存了，加载时Item的StyleNo取有用)
  FStyle.SaveToStream(AStream);
  // 节数量
  vByte := FSections.Count;
  AStream.WriteBuffer(vByte, 1);
  // 各节数据
  for i := 0 to FSections.Count - 1 do
    FSections[i].SaveToStream(AStream, ASaveParts);
  DoSaveAfter(AStream);
end;

procedure THCView.SaveToText(const AFileName: string);
begin

end;

function THCView.ZoomIn(const Value: Integer): Integer;
begin
  Result := Round(Value * FZoom);
end;

function THCView.ZoomOut(const Value: Integer): Integer;
begin
  Result := Round(Value / FZoom);
end;

procedure THCView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FVScrollBar.Left := Width - FVScrollBar.Width;
  FVScrollBar.Height := Height - FHScrollBar.Height;
  FVScrollBar.PageSize := FVScrollBar.Height;
  //
  FHScrollBar.Top := Height - FHScrollBar.Height;
  FHScrollBar.Width := Width - FVScrollBar.Width;
  FHScrollBar.PageSize := FHScrollBar.Width;
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

procedure THCView.SetOnCreateItem(const Value: TNotifyEvent);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].OnCreateItem := Value;
end;

procedure THCView.SetOnInsertItem(const Value: TItemNotifyEvent);
var
  i: Integer;
begin
  FOnInsertItem := Value;
  for i := 0 to FSections.Count - 1 do
    FSections[i].OnInsertItem := FOnInsertItem;
end;

procedure THCView.SetOnItemPaintAfter(const Value: TItemPaintEvent);
var
  i: Integer;
begin
  FOnItemPaintAfter := Value;
  for i := 0 to FSections.Count - 1 do
    FSections[i].OnItemPaintAfter := FOnItemPaintAfter;
end;

procedure THCView.SetOnItemPaintBefor(const Value: TItemPaintEvent);
var
  i: Integer;
begin
  FOnItemPaintBefor := Value;
  for i := 0 to FSections.Count - 1 do
    FSections[i].OnItemPaintBefor := FOnItemPaintBefor;
end;

procedure THCView.SetOnPaintData(const Value: TSectionPagePaintEvent);
var
  i: Integer;
begin
  FOnPaintData := Value;
  for i := 0 to FSections.Count - 1 do
    FSections[i].OnPaintData := FOnPaintData;
end;

procedure THCView.SetOnPaintFooter(const Value: TSectionPagePaintEvent);
var
  i: Integer;
begin
  FOnPaintFooter := Value;
  for i := 0 to FSections.Count - 1 do
    FSections[i].OnPaintFooter := FOnPaintFooter;
end;

procedure THCView.SetOnPaintHeader(const Value: TSectionPagePaintEvent);
var
  i: Integer;
begin
  FOnPaintHeader := Value;
  for i := 0 to FSections.Count - 1 do
    FSections[i].OnPaintHeader := FOnPaintHeader;
end;

procedure THCView.SetOnReadOnlySwitch(const Value: TNotifyEvent);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].OnReadOnlySwitch := Value;
end;

procedure THCView.SetPageScrollModel(const Value: TPageScrollModel);
begin
  if FViewModel = vmWeb then Exit;
  if FPageScrollModel <> Value then
    FPageScrollModel := Value;
end;

procedure THCView.SetReadOnly(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].ReadOnly := Value;
end;

procedure THCView.SetShowAnnotation(const Value: Boolean);
begin
  if FShowAnnotation <> Value then
  begin
    if not Assigned(FAnnotations) then
      FAnnotations := TAnnotations.Create;

    FShowAnnotation := Value;
    FStyle.UpdateInfoReCaret;
    FStyle.UpdateInfoRePaint;
    DoMapChanged;
  end;
end;

procedure THCView.SetShowLineActiveMark(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].PageData.ShowLineActiveMark := Value;

  UpdateBuffer;
end;

procedure THCView.SetShowLineNo(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].PageData.ShowLineNo := Value;

  UpdateBuffer;
end;

procedure THCView.SetShowUnderLine(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections[i].PageData.ShowUnderLine := Value;

  UpdateBuffer;
end;

procedure THCView.SetSymmetryMargin(const Value: Boolean);
begin
  if ActiveSection.SymmetryMargin <> Value then
  begin
    ActiveSection.SymmetryMargin := Value;
    FStyle.UpdateInfoReCaret;
    FStyle.UpdateInfoRePaint;
    DoMapChanged;
  end;
end;

procedure THCView.SetViewModel(const Value: TViewModel);
begin
  if FPageScrollModel = psmHorizontal then Exit; // 水平滚动不能切换模式
  if FViewModel <> Value then
    FViewModel := Value;
end;

procedure THCView.SetZoom(const Value: Single);
begin
  if FZoom <> Value then
  begin
    Self.SetFocus;
    FZoom := Value;
    FStyle.UpdateInfoReCaret;
    FStyle.UpdateInfoRePaint;
    DoMapChanged;
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

procedure THCView.ApplyParaLineSpace(const ASpace: Integer);
begin
  ActiveSection.ApplyParaLineSpace(ASpace);
end;

procedure THCView.UpdateBuffer;

  {$REGION '获取当前滚动条位置可显示的起始和结束节、页序号'}
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
          vPos := vPos + ZoomIn(MinPadding + FSections[i].PageHeightPix);
          if vPos > FVScrollBar.Position then
          begin
            vFirstPage := j;
            Break;
          end;
        end;
        if vFirstPage >= 0 then
        begin
          FDisplayFirstSection := i;
          FSections[FDisplayFirstSection].DisplayFirstPageIndex := j;
          Break;
        end;
      end;
      if FDisplayFirstSection >= 0 then
      begin
        vY := FVScrollBar.Position + GetDisplayHeight;
        for i := FDisplayFirstSection to FSections.Count - 1 do
        begin
          for j := vFirstPage to FSections[i].PageCount - 1 do
          begin
            if vPos < vY then
              vPos := vPos + ZoomIn(MinPadding + FSections[i].PageHeightPix)
            else
            begin
              vLastPage := j;
              Break;
            end;
          end;
          if vLastPage >= 0 then
          begin
            FDisplayLastSection := i;
            FSections[FDisplayLastSection].DisplayLastPageIndex := j;
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
  i, vOffsetY, vDisplayWidth, vDisplayHeight: Integer;
  vPaintInfo: TSectionPaintInfo;
begin
  if (FUpdateCount = 0) and HandleAllocated then
  begin
    FDataBmp.Canvas.Lock;
    try
      if FShowAnnotation then
        FAnnotations.Clear;
      // 控件背景
      FDataBmp.Canvas.Brush.Color := RGB(82, 89, 107);// $00E7BE9F;
      FDataBmp.Canvas.FillRect(Rect(0, 0, FDataBmp.Width, FDataBmp.Height));
      //
      vDisplayWidth := GetDisplayWidth;
      vDisplayHeight := GetDisplayHeight;
      CalcDisplaySectionAndPage;  // 计算当前范围内可显示的起始节、页和结束节、页

      vPaintInfo := TSectionPaintInfo.Create;
      try
        for i := FDisplayFirstSection to FDisplayLastSection do
        begin
          vPaintInfo.SectionIndex := i;
          vOffsetY := ZoomOut(FVScrollBar.Position) - GetSectionTopFilm(i);  // 转为相对当前节的Y向偏移
          FSections[i].PaintDisplayPage(ZoomOut(GetSectionDrawLeft(i) - FHScrollBar.Position),
            vOffsetY, vDisplayWidth, vDisplayHeight, FZoom, FDataBmp.Canvas, vPaintInfo);
        end;

        for i := 0 to vPaintInfo.TopItems.Count - 1 do
          vPaintInfo.TopItems[i].PaintTop(FDataBmp.Canvas);
      finally
        vPaintInfo.Free;
      end;
    finally
      FDataBmp.Canvas.Unlock;
    end;

    BitBlt(Canvas.Handle, 0, 0, vDisplayWidth, vDisplayHeight, FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
    InvalidateRect(Handle, ClientRect, False);  // 通知Edit只更新变动区域，防止闪烁，解决BitBlt光标滞留问题
  end;
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
    vCF.rcArea  := ClientRect;
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
          SetLength(vBuffer, vSize);  // vSize - 2
          vS := WideStringOf(vBuffer);
          if vS <> '' then
          begin
            if not DoProcessIMECandi(vS) then
              ActiveSection.InsertText(vS);
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
  ActiveSection.KillFocus;
end;

procedure THCView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  //ActiveSection.DblClick(Message.XPos, Message.YPos);  // 双击也放到MouseDown中了
end;

procedure THCView.WndProc(var Message: TMessage);
{var
  DC: HDC;
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

{ TAnnotations }

procedure TAnnotations.AddAnnotation(const ADrawItemRect: TRect; const AText: string);
var
  vAnnotation: TAnnotation;
begin
  vAnnotation := TAnnotation.Create;
  vAnnotation.DrawItemRect := ADrawItemRect;
  vAnnotation.Text := AText;
  Self.Add(vAnnotation);
end;

constructor TAnnotations.Create;
begin
  inherited Create(True);
  FIndex := -1;
end;

procedure TAnnotations.MouseDown(const X, Y: Integer);
var
  i: Integer;
  vPt: TPoint;
begin
  FIndex := -1;
  vPt := Point(X, Y);
  for i := 0 to Self.Count - 1 do
  begin
    if PtInRect(Self[i].PaintRect, vPt) then
    begin
      FIndex := i;
      Break;
    end;
  end;
end;

procedure TAnnotations.PaintTo(const ACanvas: TCanvas; const ARect: TRect;
  const APaintInfo: TSectionPaintInfo);
var
  i, vPos: Integer;
  vAnnotation: TAnnotation;
  vTextRect, vPaintRect: TRect;
begin
  if APaintInfo.Print then Exit;

  ACanvas.Brush.Color := $00F4F4F4;
  ACanvas.FillRect(Rect(ARect.Right, ARect.Top, ARect.Right + AnnotationWidth, ARect.Bottom));
  if Self.Count > 0 then
  begin
    vPos := 0;

    //ACanvas.Refresh;
    ACanvas.Font.Size := 8;
    //ACanvas.Pen.Mode := TPenMode.pmXor;
    for i := 0 to Self.Count - 1 do
    begin
      if i <> FIndex then
      begin
        ACanvas.Pen.Style := TPenStyle.psDot;
        ACanvas.Pen.Color := clRed;
      end
      else
      begin
        ACanvas.Pen.Style := TPenStyle.psSolid;
        ACanvas.Pen.Color := clMaroon;
      end;

      vAnnotation := Self.Items[i];
      if vPos < vAnnotation.DrawItemRect.Top then
        vPos := vAnnotation.DrawItemRect.Top
      else
      if vAnnotation.DrawItemRect.Top <= vPos then
        vPos := vPos + (vAnnotation.DrawItemRect.Bottom - vAnnotation.DrawItemRect.Top);

      // 计算批注文本显示区域
      vTextRect := Rect(ARect.Right + 30, vPos, ARect.Right + AnnotationWidth - 10,
        vAnnotation.DrawItemRect.Bottom);
      DrawTextEx(ACanvas.Handle, PChar(vAnnotation.Text), -1, vTextRect,
        DT_TOP or DT_LEFT or DT_WORDBREAK or DT_EDITCONTROL or DT_CALCRECT, nil);  // 计算区域

      // 填充批注区域
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := clYellow;
      vPaintRect := vTextRect;
      InflateRect(vPaintRect, 5, 5);
      ACanvas.RoundRect(vPaintRect, 5, 5);

      vAnnotation.PaintRect := vPaintRect;  // 记录 PaintRect

      // 绘制指向线
      ACanvas.Brush.Style := bsClear;
      ACanvas.MoveTo(vAnnotation.DrawItemRect.Right, vAnnotation.DrawItemRect.Bottom + 2);
      ACanvas.LineTo(ARect.Right, vAnnotation.DrawItemRect.Bottom + 2);
      ACanvas.LineTo(ARect.Right + 30, vPos);

      // 绘制批注文本
      DrawTextEx(ACanvas.Handle, PChar(vAnnotation.Text), -1, vTextRect, DT_TOP or DT_LEFT or DT_WORDBREAK, nil);

      vPos := vTextRect.Bottom + 5;
    end;
  end;
end;

end.
