{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{                 �ĵ����ݰ�ҳ���ֿؼ�                  }
{                                                       }
{*******************************************************}

unit HCView;

interface

{$I HCView.inc}

uses
  Windows, Classes, Controls, Graphics, Messages, HCStyle, HCCustomData, {$IFDEF SYNPDF}SynPdf,{$ENDIF}
  Generics.Collections, SysUtils, HCCommon, HCViewData, HCRichData, HCDrawItem,
  HCSection, HCScrollBar, HCRichScrollBar, HCStatusScrollBar, HCParaStyle, HCXml,
  HCTextStyle, HCRectItem, HCTextItem, HCItem, HCCustomFloatItem, HCUndo,
  HCAnnotateData, HCSectionData, HCAnnotateItem;

type
  THCDrawAnnotate = class(TObject)
  public
    DrawRect: TRect;
    AnnotateItem: THCAnnotateItem;
    Data: THCCustomData;
    Rect: TRect;
  end;

  THCDrawAnnotateDynamic = class(THCDrawAnnotate)
  public
    Title, Text: string;
  end;

  THCAnnotatePre = class(TObject)  // ������ʾ��������ע
  private
    FDrawRect: TRect;
    FCount: Integer;  // ��ע����
    FMouseIn, FVisible: Boolean;
    FDrawAnnotates: TObjectList<THCDrawAnnotate>;
    FActiveDrawAnnotateIndex: Integer;
    // ����ע���������Ҫ�ػ�ʱ����������������Ƴ���ע����ĸ����л�
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

    /// <summary> ������ע </summary>
    /// <param name="Sender"></param>
    /// <param name="APageRect"></param>
    /// <param name="ACanvas"></param>
    /// <param name="APaintInfo"></param>
    procedure PaintDrawAnnotate(const Sender: TObject; const APageRect: TRect;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);

    /// <summary> ����ע���� </summary>
    procedure InsertDataAnnotate(const AAnnotateItem: THCAnnotateItem);

    /// <summary> ����ע�Ƴ� </summary>
    procedure RemoveDataAnnotate(const AAnnotateItem: THCAnnotateItem);

    /// <summary> ���һ����Ҫ���Ƶ�DrawAnnotate </summary>
    procedure AddDrawAnnotate(const ADrawAnnotate: THCDrawAnnotate);
    /// <summary> �ػ�ǰ����ϴλ��� </summary>
    procedure ClearDrawAnnotate;
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
    FFileName: string;
    FStyle: THCStyle;
    FSections: TObjectList<THCSection>;
    FUndoList: THCUndoList;
    FHScrollBar: THCStatusScrollBar;
    FVScrollBar: THCRichScrollBar;
    FDataBmp: TBitmap;  // ������ʾλͼ
    FActiveSectionIndex,
    FViewWidth, FViewHeight,
    FDisplayFirstSection, FDisplayLastSection: Integer;
    FUpdateCount: Cardinal;
    FPagePadding: Byte;
    FZoom: Single;
    FAutoZoom,  // �Զ�����
    FIsChanged: Boolean;  // �Ƿ����˸ı�
    FCanEditChecked, FCanEditSnapShot: Boolean;
    FAnnotatePre: THCAnnotatePre;  // ��ע����
    FPrintAnnotatePre: Boolean;

    FViewModel: THCViewModel;  // ������ʾģʽ��ҳ�桢Web
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
    FOnViewResize  // ��ͼ��С�����仯
      : TNotifyEvent;

    /// <summary> ���ݽ�ҳ��������ô�ӡ�� </summary>
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
    procedure DoSetChange(Sender: TObject);
    procedure DoSectionChangeTopLevelData(Sender: TObject);

    // ���ػ���ؽ���꣬������Change�¼�
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
    /// <summary> �ĵ�"����"�䶯(�����ޱ仯����ԳƱ߾࣬������ͼ) </summary>
    procedure DoMapChanged;
    procedure DoSectionReadOnlySwitch(Sender: TObject);
    function DoSectionGetScreenCoord(const X, Y: Integer): TPoint;
    procedure DoSectionItemResize(const AData: THCCustomData; const AItemNo: Integer);
    procedure DoSectionDrawItemAnnotate(const Sender: TObject; const AData: THCCustomData;
      const ADrawItemNo: Integer; const ADrawRect: TRect; const AAnnotateItem: THCAnnotateItem);
    function DoSectionGetUndoList: THCUndoList;
    procedure DoSectionInsertAnnotate(const Sender: TObject; const AData: THCCustomData;
      const AAnnotateItem: THCAnnotateItem);
    procedure DoSectionRemoveAnnotate(const Sender: TObject; const AData: THCCustomData;
      const AAnnotateItem: THCAnnotateItem);
    procedure DoSectionCurParaNoChange(Sender: TObject);
    procedure DoSectionActivePageChange(Sender: TObject);

    procedure DoStyleInvalidateRect(const ARect: TRect);
    procedure DoAnnotatePreUpdateView(Sender: TObject);
    //
    function NewDefaultSection: THCSection;

    function GetViewRect: TRect;

    function GetPageIndexFilmTop(const APageIndex: Integer): Integer;
    procedure DoPageUp(Sender: TObject);
    procedure DoPageDown(Sender: TObject);

    /// <summary> ���»�ȡ���λ�� </summary>
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
    function GetPageNoFormat: string;
    procedure SetPageNoFormat(const Value: string);
    procedure SetViewModel(const Value: THCViewModel);
    procedure SetActiveSectionIndex(const Value: Integer);
    procedure SetIsChanged(const Value: Boolean);
    procedure SetPagePadding(const Value: Byte);
    /// <summary> ��ȡ��ǰ�ڶ��� </summary>
    function GetActiveSection: THCSection;

    procedure AutoScrollTimer(const AStart: Boolean);
    procedure GetPagesAndActive;
    procedure ResetCanEditShot;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoChange; virtual;
    procedure DoCaretChange; virtual;
    procedure DoKillFocus; virtual;
    procedure DoSetFocus; virtual;  // Ϊ����DLL���������DBGrid�����������Ӵ��¼�����������
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
    procedure DoSectionPaintHeaderBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintHeaderAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintFooterBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintFooterAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintPageBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo); virtual;
    procedure DoSectionPaintPageAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintPaperBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionPaintPaperAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSectionDrawItemPaintBefor(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    procedure DoSectionDrawItemPaintContent(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
      const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    procedure DoSectionDrawItemPaintAfter(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    /// <summary> ʵ�ֲ����ı� </summary>
    function DoInsertText(const AText: string): Boolean; virtual;

    /// <summary> ����ǰ�����ڿ����Ƿ������� </summary>
    function DoCopyRequest(const AFormat: Word): Boolean; virtual;

    /// <summary> ճ��ǰ�����ڿ����Ƿ�����ճ�� </summary>
    function DoPasteRequest(const AFormat: Word): Boolean; virtual;

    /// <summary> ����ǰ�����ڶ�������������������Դ </summary>
    procedure DoCopyAsStream(const AStream: TStream); virtual;

    /// <summary> ճ��ǰ������ȷ�϶�������������������Դ </summary>
    function DoPasteFromStream(const AStream: TStream): Boolean; virtual;

    /// <summary> ��ͼ���ƿ�ʼ </summary>
    procedure DoPaintViewBefor(const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    /// <summary> ��ͼ������� </summary>
    procedure DoPaintViewAfter(const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    procedure DoPaintViewHintLayer(const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    /// <summary> �����ĵ�ǰ�����¼������ڶ����������� </summary>
    procedure DoSaveStreamBefor(const AStream: TStream); virtual;

    /// <summary> �����ĵ��󴥷��¼������ڶ����������� </summary>
    procedure DoSaveStreamAfter(const AStream: TStream); virtual;

    /// <summary> ��ȡ�ĵ�ǰ�����¼�������ȷ�϶����������� </summary>
    procedure DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word); virtual;

    /// <summary> ��ȡ�ĵ��󴥷��¼�������ȷ�϶����������� </summary>
    procedure DoLoadStreamAfter(const AStream: TStream; const AFileVersion: Word); virtual;

    procedure DoSaveXmlDocument(const AXmlDoc: IHCXMLDocument); virtual;
    procedure DoLoadXmlDocument(const AXmlDoc: IHCXMLDocument); virtual;
    //
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    // ��Ϣ
    /// <summary> ��ӦTab���ͷ���� </summary>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;

    // �������뷨���������
    procedure WMImeComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure UpdateImeComposition(const ALParam: Integer); virtual;
    procedure UpdateImePosition; virtual;  // IME ֪ͨ���뷨����λ��

    procedure WndProc(var Message: TMessage); override;
    //
    procedure DataSaveLiteStream(const AStream: TStream; const AProc: THCProcedure);
    procedure DataLoadLiteStream(const AStream: TStream; const AProc: THCLoadProc);

    procedure CalcScrollRang;

    /// <summary> �Ƿ��ɹ�����λ�ñ仯����ĸ��� </summary>
    procedure CheckUpdateInfo;

    /// <summary> ��ǰ�����Ϣ </summary>
    property Caret: THCCaret read FCaret;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    /// <summary> ɾ����ʹ�õ��ı���ʽ </summary>
    class procedure DeleteUnUsedStyle(const AStyle: THCStyle;
      const ASections: TObjectList<THCSection>; const AAreas: TSectionAreas = [saHeader, saPage, saFooter]);

    /// <summary> ���赱ǰ��ֽ�ű߾� </summary>
    procedure ResetActiveSectionMargin;

    /// <summary> ActiveItem������Ӧ�价��(���ⲿֱ���޸�Item���Ժ����º���ǰ��Item�������) </summary>
    procedure ActiveItemReAdaptEnvironment;

    /// <summary> ȫ�����(�������ҳü��ҳ�š�ҳ���Item��DrawItem) </summary>
    procedure Clear; virtual;

    procedure ClearUndo;

    /// <summary> ȡ��ѡ�� </summary>
    procedure DisSelect;

    /// <summary> ɾ��ѡ������ </summary>
    procedure DeleteSelected;

    /// <summary> ɾ����ǰ�� </summary>
    function DeleteActiveDomain: Boolean;
    function DeleteActiveAnnotate: Boolean;
    /// <summary> ɾ����ǰDataָ����Χ�ڵ�Item </summary>
    procedure DeleteActiveDataItems(const AStartNo: Integer; const AEndNo: Integer = -1;
      const AKeepPara: Boolean = True);

    /// <summary> ɾ����ǰ�� </summary>
    procedure DeleteActiveSection;

    /// <summary> �������¼����Ű� </summary>
    procedure FormatData;

    /// <summary> �����ļ��� </summary>
    function InsertStream(const AStream: TStream): Boolean;
    /// <summary> ����Lite�� </summary>
    function InsertLiteStream(const AStream: TStream): Boolean;
    function AppendStream(const AStream: TStream): Boolean;
    function LoadHeaderFromStream(const AStream: TStream; const APaperInfo: Boolean): Boolean;
    function LoadFooterFromStream(const AStream: TStream; const APaperInfo: Boolean): Boolean;

    /// <summary> �����ı�(�ɰ���#13#10) </summary>
    function InsertText(const AText: string): Boolean;

    /// <summary> ����ָ�����еı�� </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;

    /// <summary> ����ͼƬ </summary>
    function InsertImage(const AFile: string): Boolean; overload;

    function InsertImage(const AImage: TGraphic): Boolean; overload;

    /// <summary> ����GIFͼƬ </summary>
    function InsertGifImage(const AFile: string): Boolean;

    /// <summary> ����ˮƽ�� </summary>
    function InsertLine(const ALineHeight: Integer): Boolean;

    /// <summary> ����һ��Item </summary>
    function InsertItem(const AItem: THCCustomItem): Boolean; overload;

    /// <summary> ��ָ����λ�ò���һ��Item </summary>
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem): Boolean; overload;

    /// <summary> ���븡��Item </summary>
    function InsertFloatItem(const AFloatItem: THCCustomFloatItem): Boolean;

    /// <summary> ������ע </summary>
    function InsertAnnotate(const ATitle, AText: string): Boolean;

    /// <summary> �ӵ�ǰλ�ú��� </summary>
    function InsertBreak: Boolean;

    /// <summary> �ӵ�ǰλ�ú��ҳ </summary>
    function InsertPageBreak: Boolean;

    /// <summary> �ӵ�ǰλ�ú�ֽ� </summary>
    function InsertSectionBreak: Boolean;

    /// <summary> ������ </summary>
    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;

    /// <summary> ��ǰͼƬ����Ϊָ����ͼ��ʹ�С </summary>
    function SetActiveImage(const AImageStream: TStream): Boolean;

    /// <summary> ��ǰ�������Ϊָ���������� </summary>
    function ActiveTableResetRowCol(const ARowCount, AColCount: Integer): Boolean;

    /// <summary> ��ǰ���ѡ������������� </summary>
    function ActiveTableInsertRowAfter(const ARowCount: Integer): Boolean;

    /// <summary> ��ǰ���ѡ������������� </summary>
    function ActiveTableInsertRowBefor(const ARowCount: Integer): Boolean;

    /// <summary> ��ǰ���ɾ��ѡ�е��� </summary>
    function ActiveTableDeleteCurRow: Boolean;

    /// <summary> ��ǰ���ѡ�еĵ�Ԫ������ </summary>
    function ActiveTableSplitCurRow: Boolean;

    /// <summary> ��ǰ���ѡ�еĵ�Ԫ������ </summary>
    function ActiveTableSplitCurCol: Boolean;

    /// <summary> ��ǰ���ѡ������������ </summary>
    function ActiveTableInsertColBefor(const AColCount: Integer): Boolean;

    /// <summary> ��ǰ���ѡ�����Ҳ������ </summary>
    function ActiveTableInsertColAfter(const AColCount: Integer): Boolean;

    /// <summary> ��ǰ���ɾ��ѡ�е��� </summary>
    function ActiveTableDeleteCurCol: Boolean;

    /// <summary> �޸ĵ�ǰ������ڶ�ˮƽ���뷽ʽ </summary>
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);

    /// <summary> �޸ĵ�ǰ������ڶδ�ֱ���뷽ʽ </summary>
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);

    /// <summary> �޸ĵ�ǰ������ڶα���ɫ </summary>
    procedure ApplyParaBackColor(const AColor: TColor);

    /// <summary> �޸ĵ�ǰ������ڶλ��нضϷ�ʽ </summary>
    procedure ApplyParaBreakRough(const ARough: Boolean);

    /// <summary> �޸ĵ�ǰ������ڶ��м�� </summary>
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode; const ASpace: Single = 1);

    /// <summary> �޸ĵ�ǰ������ڶ������� </summary>
    procedure ApplyParaLeftIndent(const Add: Boolean = True); overload;
    /// <summary> �޸ĵ�ǰ������ڶ������� </summary>
    procedure ApplyParaLeftIndent(const AIndent: Single); overload;

    /// <summary> �޸ĵ�ǰ������ڶ������� </summary>
    procedure ApplyParaRightIndent(const AIndent: Single);

    /// <summary> �޸ĵ�ǰ������ڶ��������� </summary>
    procedure ApplyParaFirstIndent(const AIndent: Single);

    /// <summary> �޸ĵ�ǰѡ���ı�����ʽ </summary>
    procedure ApplyTextStyle(const AFontStyle: THCFontStyle);

    /// <summary> �޸ĵ�ǰѡ���ı������� </summary>
    procedure ApplyTextFontName(const AFontName: TFontName);

    /// <summary> �޸ĵ�ǰѡ���ı����ֺ� </summary>
    procedure ApplyTextFontSize(const AFontSize: Single);

    /// <summary> �޸ĵ�ǰѡ���ı�����ɫ </summary>
    procedure ApplyTextColor(const AColor: TColor);

    /// <summary> �޸ĵ�ǰѡ���ı��ı�����ɫ </summary>
    procedure ApplyTextBackColor(const AColor: TColor);

    /// <summary> �޸ĵ�ǰ��Ԫ���ݶ��뷽ʽ </summary>
    procedure ApplyTableCellAlign(const AAlign: THCContentAlign);

    /// <summary> ȫѡ(���н�����) </summary>
    procedure SelectAll;

    /// <summary> ����ѡ������ </summary>
    procedure Cut;

    /// <summary> ����ѡ������(tcf��ʽ) </summary>
    procedure Copy;

    /// <summary> ����ѡ������Ϊ�ı� </summary>
    procedure CopyAsText;

    /// <summary> ճ���������е����� </summary>
    procedure Paste;

    /// <summary> �Ŵ���ͼ </summary>
    function ZoomIn(const Value: Integer): Integer;

    /// <summary> ��С��ͼ </summary>
    function ZoomOut(const Value: Integer): Integer;

    procedure MapChange;

    /// <summary> �ػ�ͻ����� </summary>
    procedure UpdateView; overload;

    /// <summary> �ػ�ͻ���ָ������ </summary>
    procedure UpdateView(const ARect: TRect); overload;

    /// <summary> ��ʼ�����ػ� </summary>
    procedure BeginUpdate;

    /// <summary> ���������ػ� </summary>
    procedure EndUpdate;

    /// <summary> ��ʼһ�鳷������ </summary>
    procedure UndoGroupBegin;

    /// <summary> ����һ�鳷������ </summary>
    procedure UndoGroupEnd;
    //
    /// <summary> ���ص�ǰ�ڵ�ǰItem </summary>
    function GetActiveItem: THCCustomItem;

    /// <summary> ���ص�ǰ�ڶ���Item </summary>
    function GetTopLevelItem: THCCustomItem;

    /// <summary> ���ص�ǰ�ڶ���DrawItem </summary>
    function GetTopLevelDrawItem: THCCustomDrawItem;

    /// <summary> ���ص�ǰ�������ҳ��� </summary>
    function GetActivePageIndex: Integer;

    /// <summary> ���ص�ǰԤ����ʼҳ��� </summary>
    function GetPagePreviewFirst: Integer;

    /// <summary> ������ҳ�� </summary>
    function GetPageCount: Integer;

    procedure PageUp;

    procedure PageDown;

    /// <summary> ����ָ����ҳ�����ʱLeftλ�� </summary>
    function GetSectionDrawLeft(const ASectionIndex: Integer): Integer;

    /// <summary> ���ظ�ʽ��λ����Ե�ǰҳ��ʾ�Ĵ������� </summary>
    function GetFormatPointToViewCoord(const APoint: TPoint): TPoint;

    /// <summary> ���ع�괦DrawItem��Ե�ǰҳ��ʾ�Ĵ������� </summary>
    /// <returns>����</returns>
    function GetTopLevelDrawItemViewCoord: TPoint;

    function GetTopLevelRectDrawItemViewCoord: TPoint;

    /// <summary> ���õ�ǰTextItem���ı����� </summary>
    procedure SetActiveItemText(const AText: string);

    /// <summary> ��ʽ��ָ���ڵ����� </summary>
    procedure FormatSection(const ASectionIndex: Integer);

    /// <summary> ��ȡ��ǰ�ڶ���Data </summary>
    function ActiveSectionTopLevelData: THCCustomData;

    /// <summary> ָ���������������е�Topλ�� </summary>
    function GetSectionTopFilm(const ASectionIndex: Integer): Integer;

    // �����ĵ�
    /// <summary> �ĵ�����Ϊhcf��ʽ </summary>
    procedure SaveToFile(const AFileName: string; const AQuick: Boolean = False);

    /// <summary> ��ȡhcf�ļ� </summary>
    function LoadFromFile(const AFileName: string): Boolean;

    /// <summary> ��ȡ������ʽ���ļ� </summary>
    procedure LoadFromDocumentFile(const AFileName: string; const AExt: string);

    /// <summary> ���Ϊ������ʽ���ļ� </summary>
    procedure SaveToDocumentFile(const AFileName: string; const AExt: string);

    /// <summary> ��ȡ������ʽ���ļ��� </summary>
    procedure LoadFromDocumentStream(const AStream: TStream; const AExt: string);

    /// <summary> �ĵ�����ΪPDF��ʽ </summary>
    procedure SaveToPDF(const AFileName: string; const APageImage: Boolean = False);

    procedure SaveToPDFStream(const AStream: TStream; const APageImage: Boolean = False);

    /// <summary> ���ַ�����ʽ��ȡ�ĵ������������� </summary>
    function SaveToText: string;

    /// <summary> ���ı�����һ������ </summary>
    function LoadFromText(const AText: string): Boolean;

    /// <summary> �ĵ����������ַ�������Ϊ�ı���ʽ�ļ� </summary>
    procedure SaveToTextFile(const AFileName: string; const AEncoding: TEncoding);

    /// <summary> ��ȡ�ı��ļ����ݵ���һ������ </summary>
    function LoadFromTextFile(const AFileName: string; const AEncoding: TEncoding): Boolean;

    /// <summary> �ĵ����������ַ�������Ϊ�ı���ʽ�� </summary>
    procedure SaveToTextStream(const AStream: TStream; const AEncoding: TEncoding);

    /// <summary> ��ȡ�ı��ļ��� </summary>
    function LoadFromTextStream(const AStream: TStream; AEncoding: TEncoding): Boolean;

    /// <summary> �ĵ����浽�� </summary>
    procedure SaveToStream(const AStream: TStream; const AQuick: Boolean = False;
      const AAreas: TSectionAreas = [saHeader, saPage, saFooter]); virtual;

    /// <summary> ��ȡ�ļ��� </summary>
    function LoadFromStream(const AStream: TStream): Boolean; virtual;

    /// <summary> �ĵ�����Ϊxml��ʽ </summary>
    procedure SaveToXml(const AFileName: string; const AEncoding: TEncoding);

    procedure SaveToXmlStream(const AStream: TStream; const AEncoding: TEncoding);

    /// <summary> ��ȡxml��ʽ </summary>
    function LoadFromXml(const AFileName: string): Boolean;

    function LoadFromXmlStream(const AStream: TStream): Boolean;

    /// <summary> ����Ϊhtml��ʽ </summary>
    /// <param name="ASeparateSrc">True��ͼƬ�ȱ��浽�ļ��У�False��base64��ʽ�洢��ҳ����</param>
    procedure SaveToHtml(const AFileName: string; const ASeparateSrc: Boolean = False);

    /// <summary>
    /// ���ĵ�ÿһҳ����ΪͼƬ
    /// </summary>
    /// <param name="APath">ͼƬ·��</param>
    /// <param name="APrefix">����ͼƬʱ�ļ�������ͼƬʱÿһ�����Ƶ�ǰ׺</param>
    /// <param name="AImageType">ͼƬ��ʽ�� BMP, JPG, PNG</param>
    /// <param name="AOnePaper">True����ͼƬ</param>
    procedure SaveToImage(const APath, APrefix, AImageType: string; const AOnePaper: Boolean = True);

    /// <summary> ��ȡָ��ҳ���ڵĽں���Դ˽ڵ�ҳ��� </summary>
    /// <param name="APageIndex">ҳ���</param>
    /// <param name="ASectionPageIndex">���ؽڵ�һҳ�������ҳ�����</param>
    /// <returns>����ҳ������ڵĽ����</returns>
    function GetSectionPageIndexByPageIndex(const APageIndex: Integer; var ASectionPageIndex: Integer): Integer;

    // ��ӡ
    /// <summary> ʹ��Ĭ�ϴ�ӡ����ӡ����ҳ </summary>
    /// <returns>��ӡ���</returns>
    function Print: TPrintResult; overload;

    /// <summary> ʹ��ָ���Ĵ�ӡ����ӡ����ҳ </summary>
    /// <param name="APrinter">ָ����ӡ��</param>
    /// <param name="ACopies">��ӡ����</param>
    /// <returns>��ӡ���</returns>
    function Print(const APrinter: string; const ACopies: Integer = 1): TPrintResult; overload;

    /// <summary> ʹ��ָ���Ĵ�ӡ����ӡָ��ҳ��ŷ�Χ�ڵ�ҳ </summary>
    /// <param name="APrinter">ָ����ӡ��</param>
    /// <param name="AStartPageIndex">��ʼҳ���</param>
    /// <param name="AEndPageIndex">����ҳ���</param>
    /// <param name="ACopies">��ӡ����</param>
    /// <returns></returns>
    function Print(const APrinter: string; const AStartPageIndex, AEndPageIndex, ACopies: Integer): TPrintResult; overload;

    /// <summary> ʹ��ָ���Ĵ�ӡ����ӡָ��ҳ </summary>
    /// <param name="APrinter">ָ����ӡ��</param>
    /// <param name="ACopies">��ӡ����</param>
    /// <param name="APages">Ҫ��ӡ��ҳ�������</param>
    /// <returns>��ӡ���</returns>
    function Print(const APrinter: string; const ACopies: Integer;
      const APages: array of Integer): TPrintResult; overload;

    /// <summary> ʹ��ָ���Ĵ�ӡ����ӡ����ҳ </summary>
    function PrintOdd(const APrinter: string): TPrintResult;

    /// <summary> ʹ��ָ���Ĵ�ӡ����ӡż��ҳ </summary>
    function PrintEven(const APrinter: string): TPrintResult;

    /// <summary> �ӵ�ǰ�д�ӡ��ǰҳ(��������) </summary>
    /// <param name="APrintHeader"> �Ƿ��ӡҳü </param>
    /// <param name="APrintFooter"> �Ƿ��ӡҳ�� </param>
    function PrintCurPageByActiveLine(const APrinter: string; const APrintHeader, APrintFooter: Boolean): TPrintResult;

    /// <summary> ��ӡ��ǰҳָ������ʼ������Item(��������) </summary>
    /// <param name="APrintHeader"> �Ƿ��ӡҳü </param>
    /// <param name="APrintFooter"> �Ƿ��ӡҳ�� </param>
    function PrintCurPageByItemRange(const APrinter: string; const APrintHeader, APrintFooter: Boolean;
      const AStartItemNo, AStartOffset, AEndItemNo, AEndOffset: Integer): TPrintResult;

    /// <summary> ��ӡ��ǰҳѡ�е���ʼ������Item(��������) </summary>
    /// <param name="APrintHeader"> �Ƿ��ӡҳü </param>
    /// <param name="APrintFooter"> �Ƿ��ӡҳ�� </param>
    function PrintCurPageSelected(const APrinter: string; const APrintHeader, APrintFooter: Boolean): TPrintResult;

    /// <summary> �ϲ����ѡ�еĵ�Ԫ�� </summary>
    function MergeTableSelectCells: Boolean;
    function TableApplyContentAlign(const AAlign: THCContentAlign): Boolean;

    /// <summary> ���� </summary>
    procedure Undo;

    /// <summary> �ָ�/���� </summary>
    procedure Redo;

    /// <summary> ��ǰλ�ÿ�ʼ����ָ�������� </summary>
    /// <param name="AKeyword">Ҫ���ҵĹؼ���</param>
    /// <param name="AForward">True����ǰ��False�����</param>
    /// <param name="AMatchCase">True�����ִ�Сд��False�������ִ�Сд</param>
    /// <returns>True���ҵ�</returns>
    function Search(const AKeyword: string; const AForward: Boolean = False;
      const AMatchCase: Boolean = False): Boolean; virtual;

    /// <summary> �滻�Ѿ�ͨ������ѡ�е����� </summary>
    /// <param name="AText">Ҫ�滻Ϊ������</param>
    /// <returns>�Ƿ��滻�ɹ�</returns>
    function Replace(const AText: string): Boolean; virtual;

    /// <summary> ���� </summary>
    function NumberOfWords: Cardinal;

    function CanEdit: Boolean;

    // ���Բ���
    /// <summary> ��ǰ�ĵ����� </summary>
    property FileName: string read FFileName write FFileName;

    /// <summary> ��ǰ�ĵ���ʽ�� </summary>
    property Style: THCStyle read FStyle;

    /// <summary> �Ƿ�ԳƱ߾� </summary>
    property SymmetryMargin: Boolean read GetSymmetryMargin write SetSymmetryMargin;

    /// <summary> ��ǰ�������ҳ����� </summary>
    property ActivePageIndex: Integer read GetActivePageIndex;

    /// <summary> ��ǰԤ����ҳ��� </summary>
    property PagePreviewFirst: Integer read GetPagePreviewFirst;

    /// <summary> ��ҳ�� </summary>
    property PageCount: Integer read GetPageCount;

    /// <summary> ��ǰ������ڽڵ���� </summary>
    property ActiveSectionIndex: Integer read FActiveSectionIndex write SetActiveSectionIndex;

    /// <summary> ��ǰ������ڵĽ� </summary>
    property ActiveSection: THCSection read GetActiveSection;

    /// <summary> ˮƽ������ </summary>
    property HScrollBar: THCStatusScrollBar read FHScrollBar;

    /// <summary> ��ֱ������ </summary>
    property VScrollBar: THCRichScrollBar read FVScrollBar;
    /// <summary> ��ǰ��괦����ʽ </summary>
    property CurStyleNo: Integer read GetCurStyleNo;
    /// <summary> ��ǰ��괦�Ķ���ʽ </summary>
    property CurParaNo: Integer read GetCurParaNo;

    /// <summary> ����ֵ </summary>
    property Zoom: Single read FZoom write SetZoom;

    /// <summary> ��ǰ�ĵ����н� </summary>
    property Sections: TObjectList<THCSection> read FSections;

    /// <summary> �Ƿ���ʾ��ǰ��ָʾ�� </summary>
    property ShowLineActiveMark: Boolean read GetShowLineActiveMark write SetShowLineActiveMark;

    /// <summary> �Ƿ���ʾ�к� </summary>
    property ShowLineNo: Boolean read GetShowLineNo write SetShowLineNo;

    /// <summary> �Ƿ���ʾ�»��� </summary>
    property ShowUnderLine: Boolean read GetShowUnderLine write SetShowUnderLine;

    /// <summary> ��ǰ�ĵ��Ƿ��б仯 </summary>
    property IsChanged: Boolean read FIsChanged write SetIsChanged;
    property PrintAnnotatePre: Boolean read FPrintAnnotatePre write FPrintAnnotatePre;
    /// <summary> ��ǰ�ĵ�������ͼʱҳ֮��ļ�� </summary>
    property PagePadding: Byte read FPagePadding write SetPagePadding;

    /// <summary> ��ǰ�ĵ���ʾ��������ע </summary>
    property AnnotatePre: THCAnnotatePre read FAnnotatePre;

    /// <summary> ��ǰ�ĵ�����ʾ�Ŀ�� </summary>
    property ViewWidth: Integer read FViewWidth;

    /// <summary> ��ǰ�ĵ�����ʾ�ĸ߶� </summary>
    property ViewHeight: Integer read FViewHeight;
  published
    { Published declarations }

    /// <summary> �����µ�Item����ʱ���� </summary>
    property OnSectionCreateItem: TNotifyEvent read FOnSectionCreateItem write FOnSectionCreateItem;

    /// <summary> �����µ�Item����ʱ���� </summary>
    property OnSectionInsertItem: TSectionDataItemEvent read FOnSectionInsertItem write FOnSectionInsertItem;

    /// <summary> �����µ�Itemɾ��ʱ���� </summary>
    property OnSectionRemoveItem: TSectionDataItemEvent read FOnSectionRemoveItem write FOnSectionRemoveItem;

    /// <summary> ����Data��DrawItem MouseMove�¼� </summary>
    property OnSectionDrawItemMouseMove: TSectionDataDrawItemMouseEvent read FOnSectionDrawItemMouseMove write FOnSectionDrawItemMouseMove;

    /// <summary> �ڱ���Itemǰ�����������Ƿ��������Item </summary>
    property OnSectionSaveItem: TSectionDataItemNoFunEvent read FOnSectionSaveItem write FOnSectionSaveItem;

    /// <summary> ����ָ���¼�����ʱ�����������Ƿ�������¼� </summary>
    property OnSectionAcceptAction: TSectionDataActionEvent read FOnSectionAcceptAction write FOnSectionAcceptAction;

    /// <summary> Item���ƿ�ʼǰ���� </summary>
    property OnSectionDrawItemPaintBefor: TSectionDrawItemPaintEvent read FOnSectionDrawItemPaintBefor write FOnSectionDrawItemPaintBefor;

    /// <summary> DrawItem������ɺ󴥷� </summary>
    property OnSectionDrawItemPaintAfter: TSectionDrawItemPaintEvent read FOnSectionDrawItemPaintAfter write FOnSectionDrawItemPaintAfter;

    /// <summary> ��ҳü����ʱ���� </summary>
    property OnSectionPaintHeaderBefor: TSectionPaintEvent read FOnSectionPaintHeaderBefor write FOnSectionPaintHeaderBefor;
    property OnSectionPaintHeaderAfter: TSectionPaintEvent read FOnSectionPaintHeaderAfter write FOnSectionPaintHeaderAfter;

    /// <summary> ��ҳ�Ż���ʱ���� </summary>
    property OnSectionPaintFooterBefor: TSectionPaintEvent read FOnSectionPaintFooterBefor write FOnSectionPaintFooterBefor;
    property OnSectionPaintFooterAfter: TSectionPaintEvent read FOnSectionPaintFooterAfter write FOnSectionPaintFooterAfter;

    /// <summary> ��ҳ�����ʱ���� </summary>
    property OnSectionPaintPageBefor: TSectionPaintEvent read FOnSectionPaintPageBefor write FOnSectionPaintPageBefor;
    property OnSectionPaintPageAfter: TSectionPaintEvent read FOnSectionPaintPageAfter write FOnSectionPaintPageAfter;

    /// <summary> ����ҳ����ǰ���� </summary>
    property OnSectionPaintPaperBefor: TSectionPaintEvent read FOnSectionPaintPaperBefor write FOnSectionPaintPaperBefor;

    /// <summary> ����ҳ���ƺ󴥷� </summary>
    property OnSectionPaintPaperAfter: TSectionPaintEvent read FOnSectionPaintPaperAfter write FOnSectionPaintPaperAfter;

    /// <summary> ��ֻ�������б仯ʱ���� </summary>
    property OnSectionReadOnlySwitch: TNotifyEvent read FOnSectionReadOnlySwitch write FOnSectionReadOnlySwitch;

    /// <summary> ������ʾģʽ������ҳ�� </summary>
    property ViewModel: THCViewModel read FViewModel write SetViewModel;

    /// <summary> �Ƿ���ݿ���Զ��������ű��� </summary>
    property AutoZoom: Boolean read FAutoZoom write FAutoZoom;

    /// <summary> ����Section�Ƿ�ֻ�� </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;

    /// <summary> ҳ��ĸ�ʽ </summary>
    property PageNoFormat: string read GetPageNoFormat write SetPageNoFormat;

    /// <summary> ��갴��ʱ���� </summary>
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;

    /// <summary> ��굯��ʱ���� </summary>
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;

    /// <summary> ���λ�øı�ʱ���� </summary>
    property OnCaretChange: TNotifyEvent read FOnCaretChange write FOnCaretChange;

    /// <summary> ��ֱ����������ʱ���� </summary>
    property OnVerScroll: TNotifyEvent read FOnVerScroll write FOnVerScroll;

    /// <summary> ˮƽ����������ʱ���� </summary>
    property OnHorScroll: TNotifyEvent read FOnHorScroll write FOnHorScroll;

    /// <summary> �ĵ����ݱ仯ʱ���� </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    /// <summary> �ĵ�Change״̬�л�ʱ���� </summary>
    property OnChangedSwitch: TNotifyEvent read FOnChangeSwitch write FOnChangeSwitch;

    /// <summary> �ĵ�Zoom���ű仯�󴥷� </summary>
    property OnZoomChanged: TNotifyEvent read FOnZoomChange write FOnZoomChange;

    /// <summary> �����ػ濪ʼʱ���� </summary>
    property OnPaintViewBefor: TPaintEvent read FOnPaintViewBefor write FOnPaintViewBefor;

    /// <summary> �����ػ�����󴥷� </summary>
    property OnPaintViewAfter: TPaintEvent read FOnPaintViewAfter write FOnPaintViewAfter;

    /// <summary> ����ָ����ʽ��Itemʱ���� </summary>
    property OnSectionCreateStyleItem: TStyleItemEvent read FOnSectionCreateStyleItem write FOnSectionCreateStyleItem;

    /// <summary> ���༭ֻ��״̬��Dataʱ���� </summary>
    property OnSectionCanEdit: TOnCanEditEvent read FOnSectionCanEdit write FOnSectionCanEdit;

    property OnSectionInsertTextBefor: TTextEvent read FOnSectionInsertTextBefor write FOnSectionInsertTextBefor;

    /// <summary> �ڵ�ǰλ�ö���ʽ����һ�β�һ��ʱ���� </summary>
    property OnSectionCurParaNoChange: TNotifyEvent read FOnSectionCurParaNoChange write FOnSectionCurParaNoChange;

    property OnSectionCaretItemChanged: TSectionDataItemEvent read FOnSectionCaretItemChanged write FOnSectionCaretItemChanged;

    /// <summary> �ڵ�ǰλ���ı���ʽ����һ�β�һ��ʱ���� </summary>
    property OnSectionActivePageChange: TNotifyEvent read FOnSectionActivePageChange write FOnSectionActivePageChange;

    /// <summary> �ĵ���ͼ�б䶯ʱ���� </summary>
    property OnViewResize: TNotifyEvent read FOnViewResize write FOnViewResize;

    property OnSetFocus: TNotifyEvent read FOnSetFocus write FOnSetFocus;

    property Color;
    property PopupMenu;
  end;

implementation

uses
  Printers, Imm, Forms, Math, Clipbrd, HCImageItem, ShellAPI, HCDocumentRW,
  HCRtfRW, HCUnitConversion, HCEditItem;

const
  IMN_UPDATECURSTRING = $F000;  // �����뷨��������ǰ��괦���ַ���

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
    for i := 0 to FSections.Count - 1 do  //  ����ڴ�ֱ�ܺͣ��Լ���������ҳ���
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

    for i := 0 to FSections.Count - 1 do  //  ����ڴ�ֱ�ܺͣ��Լ���������ҳ���
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
    vVMax := ZoomIn(vVMax + FPagePadding);  // �������һҳ�����PagePadding
    vHMax := ZoomIn(vHMax + FPagePadding + FPagePadding);
  end
  else
  if FViewModel = hvmPage then
    vHMax := ZoomIn(vHMax + FPagePadding + FPagePadding);

  FVScrollBar.Max := vVMax;
  FHScrollBar.Max := vHMax;
end;

function THCView.CanEdit: Boolean;
begin
  Result := DoSectionCanEdit(Self);
end;

procedure THCView.CheckUpdateInfo;
begin
  if FUpdateCount > 0 then Exit;

  // UpdateView�����㵱ǰ��ʾҳ��ReBuildCaret���֪ͨ�ⲿ���»�ȡ��ǰԤ��ҳ�͹������ҳ
  // ReBuildCaret����Щ��Ҫ�����ػ棬����Ҫ�ȴ������Է�ֹ������֪ͨ�ⲿȡҳ�����Ե�����
  // ʹ��vCaretChange��֪ͨ�ŵ����
  if (FCaret <> nil) and FStyle.UpdateInfo.ReCaret then  // �ȴ����꣬��Ϊ���ܹ�괦��Щ��Ҫ�����ػ�
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
  Self.BeginUpdate;  // ��ֹ�����ʱ���ع��ߣ�����������ʱ�ִ����ػ棬�ػ�ʱ��û�и�ʽ����
  try
    FStyle.Initialize;  // ������ʽ����ֹData��ʼ��ΪEmptyDataʱ��Item��ʽ��ֵΪCurStyleNo
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

procedure THCView.ClearUndo;
begin
  FUndoList.Clear;
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
      //if DoCopyAllow(HC_FILEFORMAT) then  Ϊ���������ظ����ã��������˴��ж�
      vStream := TMemoryStream.Create;
      try
        DoCopyAsStream(vStream);  // ֪ͨ�����¼������ڼ�����

        DataSaveLiteStream(vStream, procedure()
        begin
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
        end);
      finally
        vStream.Free;
      end;

      Clipboard.Open;
      try
        Clipboard.Clear;

        if DoCopyRequest(HC_FILEFORMAT) then
          Clipboard.SetAsHandle(HC_FILEFORMAT, vMem);  // HC��ʽ

        if DoCopyRequest(CF_UNICODETEXT) then
          Clipboard.AsText := Self.ActiveSectionTopLevelData.SaveSelectToText;  // �ı���ʽ
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
  Self.Color := RGB(82, 89, 107);  // ����paper֮���������ɫ
  FUpdateCount := 0;

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
  FIsChanged := False;
  FPrintAnnotatePre := False;
  ResetCanEditShot;

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
  // ��ֱ����������Χ��Resize������
  FVScrollBar := THCRichScrollBar.Create(Self);
  FVScrollBar.Orientation := TOrientation.oriVertical;
  FVScrollBar.OnScroll := DoVerScroll;
  FVScrollBar.OnPageUpClick := DoPageUp;
  FVScrollBar.OnPageDownClick := DoPageDown;
  // ˮƽ����������Χ��Resize������
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
    if Assigned(FCaret) then  // ��ֹ�л�Parentʱ��δ���Caret
      FreeAndNil(FCaret);

    FCaret := THCCaret.Create(Handle);
  end;
end;

procedure THCView.Cut;
begin
  Copy;
  ActiveSection.DeleteSelected;
end;

procedure THCView.DataLoadLiteStream(const AStream: TStream; const AProc: THCLoadProc);
var
  vFileFormat: string;
  vFileVersion: Word;
  vLang: Byte;
  vStyle: THCStyle;
begin
  _LoadFileFormatAndVersion(AStream, vFileFormat, vFileVersion, vLang);  // �ļ���ʽ�Ͱ汾
  vStyle := THCStyle.Create;
  try
    vStyle.LoadFromStream(AStream, vFileVersion);
    AProc(vFileVersion, vStyle);
  finally
    FreeAndNil(vStyle);
  end;
end;

procedure THCView.DataSaveLiteStream(const AStream: TStream; const AProc: THCProcedure);
begin
  _SaveFileFormatAndVersion(AStream);  // �ļ���ʽ�Ͱ汾
  //_DeleteUnUsedStyle;  // ɾ����ʹ�õ���ʽ
  FStyle.SaveToStream(AStream);
  AProc;
end;

function THCView.DeleteActiveAnnotate: Boolean;
begin
  Result := ActiveSection.DeleteActiveAnnotate;
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
  AStyle.TextStyles[0].CheckSaveUsed := True;
  AStyle.TextStyles[0].TempNo := 0;
  for i := 1 to AStyle.TextStyles.Count - 1 do
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
  for i := 1 to AStyle.TextStyles.Count - 1 do
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
    if vData.CurStyleNo > THCStyle.Null then
      vData.CurStyleNo := AStyle.TextStyles[vData.CurStyleNo].TempNo;

    vData.CurParaNo := AStyle.ParaStyles[vData.CurParaNo].TempNo;
  end;

  for i := AStyle.TextStyles.Count - 1 downto 1 do
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
  if GetAsyncKeyState(VK_CONTROL) < 0 then  // ����ctrl
  begin
    if WheelDelta > 0 then  // ����
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

  if vSection.PageNoVisible then  // ��ʾҳ��
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
    vS := Format(vSection.PageNoFormat, [vSectionStartPageIndex + vSection.PageNoFrom + APageIndex, vAllPageCount]);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Size := 10;
    ACanvas.Font.Name := '����';
    ACanvas.Font.Color := clBlack;
    ACanvas.Font.Style := [];
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
  // HCView�ƹ���Ϣ������Ϣ���ڿ�ԴЭ��һ���֣��뱣��
  if (not APaintInfo.Print) and (FViewModel = THCViewModel.hvmFilm) and ((Sender as THCSection).PagePadding > 10) then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Size := 10;
    ACanvas.Font.Name := '����';
    ACanvas.Font.Style := [];
    ACanvas.Font.Color := $D5D1D0;
    ACanvas.TextOut(ARect.Left, ARect.Bottom + 4, '�༭���� HCView �ṩ����������QQȺ��649023932');
  end;

  if FAnnotatePre.Visible and not APaintInfo.Print then  // ��ǰҳ����ע��������ע
    FAnnotatePre.PaintDrawAnnotate(Sender, ARect, ACanvas, APaintInfo);

  if Assigned(FOnSectionPaintPaperAfter) then
    FOnSectionPaintPaperAfter(Sender, APageIndex, ARect, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionPaintPaperBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if APaintInfo.Print and (FAnnotatePre.DrawCount > 0) then  // ��ӡ�ǵ�����ƣ�����ÿһҳǰ���
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
  const AData: THCCustomData; const AAnnotateItem: THCAnnotateItem);
begin
  FAnnotatePre.RemoveDataAnnotate(AAnnotateItem);
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
    FVScrollBar.Position := GetPageIndexFilmTop(vPageIndex + 1);
end;

procedure THCView.DoPageUp(Sender: TObject);
var
  vPageIndex: Integer;
begin
  vPageIndex := GetPagePreviewFirst;
  if vPageIndex > 0 then
    FVScrollBar.Position := GetPageIndexFilmTop(vPageIndex - 1)
  else
    FVScrollBar.Position := 0;
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

procedure THCView.DoPaintViewHintLayer(const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
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
  //SetIsChanged(False);  �������ݱ���ʱ������Ϊҵ�񱣴�ɹ�
end;

procedure THCView.DoSaveStreamBefor(const AStream: TStream);
begin
  // �����ⲿ����洢�Զ������ݣ����ϴ����λ�õ�
end;

procedure THCView.DoSaveXmlDocument(const AXmlDoc: IHCXMLDocument);
begin
end;

procedure THCView.DoSectionActivePageChange(Sender: TObject);
begin
  if Assigned(FOnSectionActivePageChange) then
    FOnSectionActivePageChange(Sender);
end;

function THCView.DoSectionCanEdit(const Sender: TObject): Boolean;
begin
  {if FCanEditChecked then
  begin
    Result := FCanEditSnapShot;
    Exit;
  end;}

  if (not Style.States.Contain(hosLoading)) and Assigned(FOnSectionCanEdit) then
    Result := FOnSectionCanEdit(Sender)
  else
    Result := True;

  FCanEditChecked := True;
  FCanEditSnapShot := Result;
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

procedure THCView.DoSetChange(Sender: TObject);
begin
  FCanEditChecked := False;
  SetIsChanged(True);
  if Assigned(FOnChange) then
    FOnChange(Self);
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
  DoMapChanged;
  DoSetChange(Self);
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
  _LoadFileFormatAndVersion(AStream, vFileExt, vVersion, vLang);  // �ļ���ʽ�Ͱ汾
  if (vFileExt <> HC_EXT) and (vFileExt <> 'cff.') then
    raise Exception.Create('����ʧ�ܣ�����' + HC_EXT + '�ļ���');
  if vVersion > HC_FileVersionInt then
    raise Exception.Create('����ʧ�ܣ���ǰ�༭�����֧�ְ汾Ϊ'
      + IntToStr(HC_FileVersionInt) + '���ļ����޷��򿪰汾Ϊ'
      + IntToStr(vVersion) + '���ļ���');

  DoLoadStreamBefor(AStream, vVersion);  // ��������ǰ�¼�
  AStyle.LoadFromStream(AStream, vVersion);  // ������ʽ��
  ALoadSectionProc(vVersion);  // ���ؽ�������������
  DoLoadStreamAfter(AStream, vVersion);
  DoMapChanged;
end;

procedure THCView.DoSectionInsertAnnotate(const Sender: TObject; const AData: THCCustomData; const AAnnotateItem: THCAnnotateItem);
begin
  FAnnotatePre.InsertDataAnnotate(AAnnotateItem);
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
  const ADrawItemNo: Integer; const ADrawRect: TRect; const AAnnotateItem: THCAnnotateItem);
var
  vDrawAnnotate: THCDrawAnnotate;
begin
  vDrawAnnotate := THCDrawAnnotate.Create;
  //vAnnotate.Section := Sender;
  vDrawAnnotate.Data := AData;
  vDrawAnnotate.DrawRect := ADrawRect;
  vDrawAnnotate.AnnotateItem := AAnnotateItem;
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
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if AData.Items[AItemNo].HyperLink <> '' then
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clBlue;
    ACanvas.Pen.Width := 1;
    ACanvas.MoveTo(AClearRect.Left, AClearRect.Bottom);
    ACanvas.LineTo(AClearRect.Right, AClearRect.Bottom);
  end;

  if Assigned(FOnSectionDrawItemPaintAfter) then
    FOnSectionDrawItemPaintAfter(Sender, AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionDrawItemPaintBefor(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnSectionDrawItemPaintBefor) then
    FOnSectionDrawItemPaintBefor(Sender, AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCView.DoSectionDrawItemPaintContent(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  // ���������꣬�����ı�ǰ�������ɴ�������ؼ���
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
  //Sender��Data��ActiveSection.ActiveData�����Բ���Ҫ�ͷ�
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
  ResetCanEditShot;
end;

procedure THCView.DoLoadXmlDocument(const AXmlDoc: IHCXMLDocument);
begin
  ResetCanEditShot;
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
  Result := Self.ActiveSection.GetTopLevelRectDrawItemCoord;  // ��ѡ��ʱ����ѡ�н���λ�õ�DrawItem��ʽ������
  Result := Self.GetFormatPointToViewCoord(Result);
end;

function THCView.GetTopLevelDrawItemViewCoord: TPoint;
begin
  Result := Self.ActiveSection.GetTopLevelDrawItemCoord;  // ��ѡ��ʱ����ѡ�н���λ�õ�DrawItem��ʽ������
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

  // ӳ�䵽��ҳ��(��ɫ����)
  Result.X := ZoomIn(GetSectionDrawLeft(Self.ActiveSectionIndex)
    + (vSection.GetPageMarginLeft(vPageIndex) + Result.X)) - FHScrollBar.Position;

  if vSection.ActiveData = vSection.Header then  // ҳü
    Result.Y := ZoomIn(GetSectionTopFilm(Self.ActiveSectionIndex)
      + vSection.GetPageTopFilm(vPageIndex)  // 20
      + vSection.GetHeaderPageDrawTop
      + Result.Y)
      //- vSection.GetPageDataFmtTop(vPageIndex))  // 0
      - FVScrollBar.Position
  else
  if vSection.ActiveData = vSection.Footer then  // ҳ��
    Result.Y := ZoomIn(GetSectionTopFilm(Self.ActiveSectionIndex)
      + vSection.GetPageTopFilm(vPageIndex)  // 20
      + vSection.PaperHeightPix - vSection.PaperMarginBottomPix
      + Result.Y)
      //- vSection.GetPageDataFmtTop(vPageIndex))  // 0
      - FVScrollBar.Position
  else  // ����
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
  if (ASectionIndex < 0) and (vY + FPagePadding >= Y) then  // ���һҳ�����Padding
    ASectionIndex := FSections.Count - 1;

  if ASectionIndex < 0 then
    ASectionIndex := 0;
end;

function THCView.GetSectionDrawLeft(const ASectionIndex: Integer): Integer;
begin
  if FViewModel = THCViewModel.hvmFilm then
  begin
    if FAnnotatePre.Visible then  // ��ʾ��ע
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
      Result := i;  // �ҵ������
      ASectionPageIndex := APageIndex - vPageCount;  // ����ҳ���

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

function THCView.InsertLiteStream(const AStream: TStream): Boolean;
var
  vResult: Boolean;
begin
  Result := False;
  DataLoadLiteStream(AStream, procedure(const AFileVersion: Word; const AStyle: THCStyle)
  begin
    Self.BeginUpdate;
    try
      vResult := ActiveSection.InsertStream(AStream, AStyle, AFileVersion);
    finally
      Self.EndUpdate;
    end;
  end);
  Result := vResult;
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
  Self.BeginUpdate;
  try
    vSection := NewDefaultSection;
    vSection.PageNoFormat := FSections[FActiveSectionIndex].PageNoFormat;
    vSection.AssignPaper(FSections[FActiveSectionIndex]);  // ������һ�ڵ�ҳ��С�ͱ߾�
    FSections.Insert(FActiveSectionIndex + 1, vSection);
    FActiveSectionIndex := FActiveSectionIndex + 1;
    Result := True;
    FStyle.UpdateInfoRePaint;
    FStyle.UpdateInfoReCaret;
    FStyle.UpdateInfoReScroll;
    DoChange;
  finally
    Self.EndUpdate;
  end;
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
          vShowUnderLine: Boolean;  // �»���
          vDataStream: TMemoryStream;
        begin
          AStream.ReadBuffer(vByte, 1);  // ������

          if AFileVersion > 42 then
          begin
            ActiveSection.SeekStreamToArea(AStream, vStyle, AFileVersion, TSectionArea.saPage, False);
            vResult := ActiveSection.InsertStream(AStream, vStyle, AFileVersion);  // ֻ�����һ�ڵ�����
            Exit;
          end;

          vDataStream := TMemoryStream.Create;
          try
            vSection := THCSection.Create(vStyle);
            try
              vSection.OnCreateItemByStyle := DoSectionCreateStyleItem;  // ��ʽҪ����һ��

              // ��ѭ����ֻ�����һ�ڵ�����
              vSection.LoadFromStream(AStream, vStyle, AFileVersion);
              vDataStream.Clear;
              vSection.Page.SaveToStream(vDataStream);
              vDataStream.Position := 0;
              vDataStream.ReadBuffer(vShowUnderLine, SizeOf(vShowUnderLine));
              vDataStream.Position := vDataStream.Position + 8;  // ����DataSize
              vResult := ActiveSection.InsertStream(vDataStream, vStyle, HC_FileVersionInt);  // ֻ�����һ�ڵ�����
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

function THCView.ActiveTableInsertColAfter(const AColCount: Integer): Boolean;
begin
  Result := ActiveSection.ActiveTableInsertColAfter(AColCount);
end;

function THCView.ActiveTableInsertColBefor(const AColCount: Integer): Boolean;
begin
  Result := ActiveSection.ActiveTableInsertColBefor(AColCount);
end;

function THCView.ActiveTableInsertRowAfter(const ARowCount: Integer): Boolean;
begin
  Result := ActiveSection.ActiveTableInsertRowAfter(ARowCount);
end;

function THCView.ActiveTableInsertRowBefor(const ARowCount: Integer): Boolean;
begin
  Result := ActiveSection.ActiveTableInsertRowBefor(ARowCount);
end;

function THCView.ActiveTableResetRowCol(const ARowCount, AColCount: Integer): Boolean;
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

  {$REGION '��ݼ�'}
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
  begin
    // �����ʽ�ĸ���
    Self.CopyAsText;
  end
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
  if GetKeyState(VK_CONTROL) < 0 then  // ����ctrl������ֹshift���Ź��ϵ���
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
    // ��������ָ�����
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
    // ��������ָ�����
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

  Self.BeginUpdate;
  try
    // ��������ָ�����
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
            AStream.ReadBuffer(vByte, 1);  // ������
            // ��������
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

  Self.Clear;
  FStyle.Initialize;

  if AText <> '' then
  begin
    FStyle.States.Include(hosLoading);
    try
      Result := ActiveSection.InsertText(AText);
    finally
      FStyle.States.Exclude(hosLoading);
    end;
  end;
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

  Self.BeginUpdate;
  try
    // ��������ָ�����
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

        DoLoadXmlDocument(vXml);

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

function THCView.LoadHeaderFromStream(const AStream: TStream; const APaperInfo: Boolean): Boolean;
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
        begin
          AStream.ReadBuffer(vByte, 1);  // ������

          if AFileVersion > 42 then  // ���Ʒ��������ʱ�޷��������ӦAPaperInfo��������Paper��Ϣ
          begin
            Self.ActiveSection.Header.BeginFormat;
            try
              Self.ActiveSection.SeekStreamToArea(AStream, vStyle, AFileVersion, TSectionArea.saHeader, APaperInfo);
              Self.ActiveSection.Header.LoadFromStream(AStream, vStyle, AFileVersion);  // ֻ�����һ�ڵ�����
            finally
              Self.ActiveSection.Header.EndFormat(False);
            end;

            //if APaperInfo then
            Self.ResetActiveSectionMargin;

            vResult := True;
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

function THCView.LoadFooterFromStream(const AStream: TStream; const APaperInfo: Boolean): Boolean;
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
        begin
          AStream.ReadBuffer(vByte, 1);  // ������

          if AFileVersion > 42 then
          begin
            Self.ActiveSection.Footer.BeginFormat;
            try
              Self.ActiveSection.SeekStreamToArea(AStream, vStyle, AFileVersion, TSectionArea.saFooter, APaperInfo);
              Self.ActiveSection.Footer.LoadFromStream(AStream, vStyle, AFileVersion);  // ֻ�����һ�ڵ�����
            finally
              Self.ActiveSection.Footer.EndFormat(False);
            end;

            //if APaperInfo then
            Self.ResetActiveSectionMargin;

            vResult := True;
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

procedure THCView.MapChange;
begin
  DoMapChanged;
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

  if FAnnotatePre.DrawCount > 0 then  // ����ע������
//  begin
//    //if (X + HScrollValue > vSectionDrawLeft + FSections[FActiveSectionIndex].PaperWidthPix)
//    //  and (X + HScrollValue < vSectionDrawLeft + FSections[FActiveSectionIndex].PaperWidthPix + AnnotationWidth)
//    if PtInRect(FAnnotatePre.DrawRect, Point(X, Y)) then  // ������ע������
//    begin
      FAnnotatePre.MouseDown(ZoomOut(X), ZoomOut(Y));
//      FStyle.UpdateInfoRePaint;
//      //DoSectionDataCheckUpdateInfo(Self);
//      //Exit;
//    end;
//  end;

  // ӳ�䵽��ҳ��(��ɫ����)
  vPt.X := ZoomOut(FHScrollBar.Position + X) - vSectionDrawLeft;
  vPt.Y := ZoomOut(FVScrollBar.Position + Y) - GetSectionTopFilm(FActiveSectionIndex);
  //vPageIndex := FSections[FActiveSectionIndex].GetPageByFilm(vPt.Y);
  FSections[FActiveSectionIndex].MouseDown(Button, Shift, vPt.X, vPt.Y);

  CheckUpdateInfo;  // ����ꡢ�л�����Item
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
  if FActiveSectionIndex >= 0 then  // ����ʱ�ڽ���
  begin
    FSections[FActiveSectionIndex].MouseMove(Shift,
      ZoomOut(FHScrollBar.Position + X) - GetSectionDrawLeft(FActiveSectionIndex),
      ZoomOut(FVScrollBar.Position + Y) - GetSectionTopFilm(FActiveSectionIndex));

    if ShowHint then
      ProcessHint;

    if FStyle.UpdateInfo.Selecting then
      AutoScrollTimer(True);
  end;

  if FAnnotatePre.DrawCount > 0 then  // ����ע������
    FAnnotatePre.MouseMove(ZoomOut(X), ZoomOut(Y));

  CheckUpdateInfo;  // ������Ҫ������괦Item

  if FStyle.UpdateInfo.DragingSelected then
    Screen.Cursor := GCursor  // �ŵ�OnDrag���ǲ��ǾͲ�������Screen�˻�������Self.DragKind��
  else
    Cursor := GCursor;
end;

procedure THCView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FStyle.UpdateInfo.Selecting then
    AutoScrollTimer(False);

  if Button = mbRight then Exit;  // �Ҽ������˵�
  //GetSectionByCrood(FHScrollBar.Value + X, FVScrollBar.Value + Y, vSectionIndex);
  if FActiveSectionIndex >= 0 then  // ����ʱ�ڽ���
    FSections[FActiveSectionIndex].MouseUp(Button, Shift,
      ZoomOut(FHScrollBar.Position + X) - GetSectionDrawLeft(FActiveSectionIndex),
      ZoomOut(FVScrollBar.Position + Y) - GetSectionTopFilm(FActiveSectionIndex));

  if FStyle.UpdateInfo.DragingSelected then
    Screen.Cursor := crDefault;

  Cursor := GCursor;

  CheckUpdateInfo;  // ��ѡ�������а��²��ƶ��������ʱ��Ҫ����

  FStyle.UpdateInfo.Selecting := False;
  FStyle.UpdateInfo.DragingSelected := False;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

function THCView.NewDefaultSection: THCSection;
begin
  Result := THCSection.Create(FStyle);
  // �����ں����ϸ�ֵ�¼�����֤�������������Ҫ��Щ�¼��Ĳ����ɻ�ȡ���¼���
  Result.OnDataChange := DoSectionDataChange;
  Result.OnDataSetChange := DoSetChange;
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

function THCView.NumberOfWords: Cardinal;
var
  vText: string;
begin
  vText := SaveToText;
  Result := Length(vText);
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

function THCView.GetPageIndexFilmTop(const APageIndex: Integer): Integer;
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

function THCView.GetPageNoFormat: string;
begin
  Result := Self.ActiveSection.PageNoFormat;
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
  FHScrollBar.Statuses[0].Text := 'Ԥ��' + IntToStr(PagePreviewFirst + 1)
    + ' ���' + IntToStr(ActivePageIndex + 1)
    + '/' + IntToStr(PageCount) + 'ҳ';
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
    vImageItem := vTopData.CreateItemByStyle(THCStyle.Image) as THCImageItem;

    vBitmap := TBitmap.Create;
    try
      vBitmap.Assign(Clipboard);
      vImageItem.ImageAssign(vBitmap);
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
        if not DoPasteFromStream(vStream) then Exit;
        InsertLiteStream(vStream);
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
    Printer.PrinterIndex := Printer.Printers.IndexOf(APrinter);

  if Printer.PrinterIndex < 0 then Exit;

  Printer.Title := FFileName;

  // ȡ��ӡ����ӡ������ز���
  vPrintOffsetX := -GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);  // 73
  vPrintOffsetY := -GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);  // 37

  Printer.Copies := ACopies;

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;

    try
      vPrintCanvas := TCanvas.Create;
      try
        for i := 0 to Length(APages) - 1 do
        begin
          // ����ҳ���ȡ��ʼ�ںͽ�����
          vSectionIndex := GetSectionPageIndexByPageIndex(APages[i], vFirstPageIndex);
          if vPaintInfo.SectionIndex <> vSectionIndex then
          begin
            if Printer.Printing then
              Printer.EndDoc;

            SetPrintBySectionInfo(vSectionIndex);
            Printer.BeginDoc;
            vPrintCanvas.Handle := Printer.Canvas.Handle;  // Ϊʲô����vPrintCanvas�н��ӡ�Ͳ����أ�

            vPaintInfo.DPI := GetDeviceCaps(vPrintCanvas.Handle, LOGPIXELSX);
            vPaintInfo.SectionIndex := vSectionIndex;

            vPrintWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);  // 4961
            vPrintHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);  // 7016

            if FPrintAnnotatePre and FAnnotatePre.Visible then
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
            Printer.NewPage;

          vScaleInfo := vPaintInfo.ScaleCanvas(vPrintCanvas);
          try
            vPaintInfo.PageIndex := APages[i];

            FSections[vSectionIndex].PaintPaper(vFirstPageIndex,
              vPrintOffsetX, vPrintOffsetY, vPrintCanvas, vPaintInfo);

            //Printer.EndPage;
          finally
            vPaintInfo.RestoreCanvasScale(vPrintCanvas, vScaleInfo);
          end;
        end;
      finally
        vPrintCanvas.Handle := 0;
        vPrintCanvas.Free;
      end;
    finally
      if Printer.Printing then
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
    Printer.PrinterIndex := Printer.Printers.IndexOf(APrinter);

  if Printer.PrinterIndex < 0 then Exit;

  Printer.Title := FFileName;

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

    if FPrintAnnotatePre and FAnnotatePre.Visible then
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

    Printer.BeginDoc;
    try
      vPrintCanvas := TCanvas.Create;
      try
        vPrintCanvas.Handle := Printer.Canvas.Handle;  // Ϊʲô����vPageCanvas�н��ӡ�Ͳ����أ�
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

          // "Ĩ"������Ҫ��ʾ�ĵط�
          vPrintCanvas.Brush.Color := clWhite;

          if APrintHeader then  // ��ӡҳü
            vRect := Bounds(vPrintOffsetX,// + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight,  // ҳü�±�
              Self.ActiveSection.PaperWidthPix,// - vMarginLeft - vMarginRight,
              vPt.Y)
          else  // ����ӡҳü
            vRect := Bounds(vPrintOffsetX,// + vMarginLeft, ��ֹҳü����Item��ҳ�߾���
              vPrintOffsetY,
              Self.ActiveSection.PaperWidthPix,// - vMarginLeft - vMarginRight,
              Self.ActiveSection.GetHeaderAreaHeight + vPt.Y);

          vPrintCanvas.FillRect(vRect);

          if not APrintFooter then  // ����ӡҳ��
          begin
            vRect := Bounds(vPrintOffsetX,// + vMarginLeft, ��ֹҳ�Ÿ���Item��ҳ�߾���
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
      Printer.EndDoc;
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
  // ע�⣺�˷�����Ҫ��ʼItem��ʽ����һ��DrawItem�ͽ���ItemNo��ʽ�����һ��DrawItem��ͬһҳ
  Result := TPrintResult.prError;

  if APrinter <> '' then
    Printer.PrinterIndex := Printer.Printers.IndexOf(APrinter);

  if Printer.PrinterIndex < 0 then Exit;

  Printer.Title := FFileName;

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

    if FPrintAnnotatePre and FAnnotatePre.Visible then
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

    Printer.BeginDoc;
    try
      vPrintCanvas := TCanvas.Create;
      try
        vPrintCanvas.Handle := Printer.Canvas.Handle;  // Ϊʲô����vPageCanvas�н��ӡ�Ͳ����أ�
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

          // "Ĩ"������Ҫ��ʾ�ĵط�
          vPrintCanvas.Brush.Color := clWhite;

          // Ĩѡ����������
          if APrintHeader then  // ��ӡҳü
            vRect := Bounds(vPrintOffsetX,  // + vMarginLeft,
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight,  // ҳü�±�
              Self.ActiveSection.PaperWidthPix,  // - vMarginLeft - vMarginRight,
              vPt.Y)
          else  // ����ӡҳü
            vRect := Bounds(vPrintOffsetX, // + vMarginLeft, ��ֹҳü����Item��ҳ�߾���
              vPrintOffsetY,
              Self.ActiveSection.PaperWidthPix,  // - vMarginLeft - vMarginRight,
              Self.ActiveSection.GetHeaderAreaHeight + vPt.Y);

          vPrintCanvas.FillRect(vRect);

          // Ĩѡ��������ʼ���
          vRect := Bounds(vPrintOffsetX + vMarginLeft,
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y,
            vData.DrawItems[vDrawItemNo].Rect.Left + vData.GetDrawItemOffsetWidth(vDrawItemNo, AStartOffset - vData.DrawItems[vDrawItemNo].CharOffs + 1),
            vData.DrawItems[vDrawItemNo].Rect.Height);

          vPrintCanvas.FillRect(vRect);

          // Ĩѡ�����ݽ����ұ�
          vDrawItemNo := vData.GetDrawItemNoByOffset(AEndItemNo, AEndOffset);
          vPt := vData.DrawItems[vDrawItemNo].Rect.TopLeft;
          vPt.Y := vPt.Y - ActiveSection.GetPageDataFmtTop(Self.ActiveSection.ActivePageIndex);

          vRect := Rect(vPrintOffsetX + vMarginLeft + vData.DrawItems[vDrawItemNo].Rect.Left +
              vData.GetDrawItemOffsetWidth(vDrawItemNo, AEndOffset - vData.DrawItems[vDrawItemNo].CharOffs + 1),
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y,
            vPrintOffsetX + Self.ActiveSection.PaperWidthPix - vMarginRight,
            vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y + vData.DrawItems[vDrawItemNo].Rect.Height);

          vPrintCanvas.FillRect(vRect);

          // Ĩѡ����������
          if not APrintFooter then  // ����ӡҳ��
          begin
            vRect := Rect(vPrintOffsetX,// + vMarginLeft, ��ֹҳ�Ÿ���Item��ҳ�߾���
              vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight + vPt.Y + vData.DrawItems[vDrawItemNo].Rect.Height,
              vPrintOffsetX + Self.ActiveSection.PaperWidthPix,// - vMarginRight,
              vPrintOffsetY + Self.ActiveSection.PaperHeightPix);

            vPrintCanvas.FillRect(vRect);
          end
          else  // ��ӡҳ��
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
      Printer.EndDoc;
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
    if Odd(i) then  // ���������ż��ҳ
    begin
      SetLength(vPages, Length(vPages) + 1);
      vPages[Length(vPages) - 1] := i;
    end;
  end;

  Result := Print(APrinter, 1, vPages);  // ����ҳ
end;

function THCView.PrintOdd(const APrinter: string): TPrintResult;
var
  i: Integer;
  vPages: array of Integer;
begin
  for i := 0 to PageCount - 1 do
  begin
    if not Odd(i) then  // ż�����������ҳ
    begin
      SetLength(vPages, Length(vPages) + 1);
      vPages[Length(vPages) - 1] := i;
    end;
  end;

  Result := Print(APrinter, 1, vPages);  // ����ҳ
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

  // ��ȥ����ѡ��ʱҲȡ��꣬�����������쳣����DrawItem��ѡʱ����괦��DrawItemNo
  // ��GetCaretInfoʱȡ�Ļ��г�ͻ�����绮ѡ��ɺ��������ȡ���λ�ã�ȡ����SelectInfo.Start��Ϣ
  // ���ԣ�Ӧ����SeekItemNo��صĴ������ǲ��Ǹ�����
  if ((not FStyle.UpdateInfo.DragingSelected) and ActiveSection.SelectExists) then
  begin
    FCaret.Hide;
    Exit;
  end;

  { ��ʼ�������Ϣ��Ϊ�����������������ֻ�ܷ������� }
  vCaretInfo.X := 0;
  vCaretInfo.Y := 0;
  vCaretInfo.Height := 0;
  vCaretInfo.Visible := True;

  ActiveSection.GetPageCaretInfo(vCaretInfo);  // ��Щ�ⲿ�����ݵĲ����Ի����˵����߽��㣬�������괦����Ϣ��Ҫ��ʱ�ĸ��£����������ʼ����λ�ñ仯

  if (not Self.Focused) or (not vCaretInfo.Visible) then
  begin
    FCaret.Hide;
    Exit;
  end;

  vCaretInfo.Y := vCaretInfo.Y + GetSectionTopFilm(FActiveSectionIndex);
  FVScrollBar.SetAreaPos(-1, vCaretInfo.Y, vCaretInfo.Height);

  FCaret.X := ZoomIn(GetSectionDrawLeft(FActiveSectionIndex) + vCaretInfo.X) - FHScrollBar.Position;
  FCaret.Y := ZoomIn(vCaretInfo.Y) - FVScrollBar.Position;
  FCaret.Height := ZoomIn(vCaretInfo.Height);

  if not FStyle.UpdateInfo.ReScroll then // ���������������Ļ�ȡ���λ�ã����ܽ������������
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
  else  // �ǹ�����(������������)����Ĺ��λ�ñ仯
  begin
    if FCaret.Height < FViewHeight then
    begin
      if not FCaret.VScroll then
      begin
        FCaret.VScroll := True;  // ��ֹ��������仯���ٴ�ȡ������λ�ã������ѭ��
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

  if FCaret.VScroll or FCaret.HScroll then Exit;  //Exit;  // ��ֹ���������¼�����λ�ú��ظ�ִ������Ĵ���

  if FCaret.Y + FCaret.Height > FViewHeight then
    FCaret.Height := FViewHeight - FCaret.Y;

  FCaret.Show;
  DoCaretChange;
end;

procedure THCView.Redo;
begin
  FStyle.States.Include(THCState.hosRedoing);
  try
    if FUndoList.Enable then  // �ָ����̲�Ҫ�����µ�Redo
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

procedure THCView.ResetCanEditShot;
begin
  FCanEditChecked := False;
  FCanEditSnapShot := False;
end;

procedure THCView.Resize;
begin
  inherited;

  GetViewWidth;
  GetViewHeight;

  if (FViewWidth > 0) and (FViewHeight > 0) then
    FDataBmp.SetSize(FViewWidth, FViewHeight);  // ����Ϊ����������Ĵ�С

  if FAutoZoom then
  begin
    if FAnnotatePre.Visible then  // ��ʾ��ע
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
  FUndoList.Clear; // ��ֹɾ����ʽ��ĳ����Ҳ���ԭ��ʽ
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

procedure THCView.SaveToPDF(const AFileName: string; const APageImage: Boolean = False);
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
  Result := FSections[0].SaveToText;
  for i := 1 to FSections.Count - 1 do  // ��������
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
  {$IFDEF USESCRIPT}
  if not AQuick then
    Self.DisSelect;
  {$ENDIF}
  FStyle.States.Include(hosSaving);
  try
    _SaveFileFormatAndVersion(AStream);  // �ļ���ʽ�Ͱ汾
    DoSaveStreamBefor(AStream);

    if not AQuick then
    begin
      FUndoList.Clear; // ��ֹɾ����ʽ��ĳ����Ҳ���ԭ��ʽ
      DeleteUnUsedStyle(FStyle, FSections, AAreas);  // ɾ����ʹ�õ���ʽ
    end;
    FStyle.SaveToStream(AStream);

    // ������
    vByte := FSections.Count;
    AStream.WriteBuffer(vByte, 1);
    // ��������
    for i := 0 to FSections.Count - 1 do
      FSections[i].SaveToStream(AStream, AAreas);

    DoSaveStreamAfter(AStream);
  finally
    FStyle.States.Exclude(hosSaving);
  end;
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
  FStyle.States.Include(hosSaving);
  try
    FUndoList.Clear; // ��ֹɾ����ʽ��ĳ����Ҳ���ԭ��ʽ
    DeleteUnUsedStyle(FStyle, FSections, [saHeader, saPage, saFooter]);

    vXml := THCXMLDocument.Create(nil);
    vXml.Active := True;
    vXml.Version := '1.0';
    vXml.Encoding := GetEncodingName(AEncoding);

    vXml.DocumentElement := vXml.CreateNode('HCView');
    vXml.DocumentElement.Attributes['EXT'] := HC_EXT;
    vXml.DocumentElement.Attributes['ver'] := HC_FileVersion;
    vXml.DocumentElement.Attributes['lang'] := HC_PROGRAMLANGUAGE;
    DoSaveXmlDocument(vXml);

    vNode := vXml.DocumentElement.AddChild('style');
    FStyle.ToXml(vNode);  // ��ʽ��

    vNode := vXml.DocumentElement.AddChild('sections');
    vNode.Attributes['count'] := FSections.Count;  // ������

    for i := 0 to FSections.Count - 1 do  // ��������
      FSections[i].ToXml(vNode.AddChild('sc'));

    //vXml.SaveToFile(AFileName);
    vXml.SaveToStream(AStream);
  finally
    FStyle.States.Exclude(hosSaving);
  end;
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
    vPDF.VCLCanvas.Font.Name := '����'; // ����
    vPDF.VCLCanvas.TextOut( 20, 20, '����˫���Ų���ȷ�����⡱' );
    vPDF.SaveToFile('c:\Syntest.pdf');

    Exit;}


    vPDF.Info.Author := 'HCView';
    vPDF.Info.CreationDate := Now;
    vPDF.Info.Creator := 'HCView';  // jt
    vPDF.Info.Keywords := '';  // ��Ժ��¼
    vPDF.Info.ModDate := Now;
    vPDF.Info.Subject := '';  // HIT ���Ӳ���
    vPDF.Info.Title := '';  // ������1��

    vPDF.UseUniscribe := True;

    vDPI := PixelsPerInchX;
    vPDF.ScreenLogPixels := vDPI;
    //vPDF.VCLCanvas.Font.Charset = ANSI_CHARSET
    //https://synopse.info/forum/viewtopic.php?id=2494
    if not APageImage then
    begin
      vPDF.EmbeddedTTF := True;
      //vPDF.EmbeddedWholeTTF := True;
      vPDF.EmbeddedTTFIgnore.Text := MSWINDOWS_DEFAULT_FONTS + #13#10'����';
    end;

    vPaintInfo := TSectionPaintInfo.Create;
    try
      vPaintInfo.Print := True;
      vPaintInfo.DPI := vPDF.ScreenLogPixels;

      for i := 0 to FSections.Count - 1 do
      begin
        vPaintInfo.SectionIndex := i;
        vPaintInfo.Zoom := 1;
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
          begin  // Ӣ�絥λ���ܹ���������
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
  vDrawRect: TRect;
begin
  Result := Self.ActiveSection.Search(AKeyword, AForward, AMatchCase);
  if Result then
  begin
    vPt := GetTopLevelDrawItemViewCoord;  // ���ع�괦DrawItem��Ե�ǰҳ��ʾ�Ĵ������꣬��ѡ��ʱ����ѡ�н���λ�õ�DrawItem��ʽ������

    vTopData := ActiveSectionTopLevelData;
    with vTopData do
    begin
      vStartDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      vEndDrawItemNo := GetDrawItemNoByOffset(SelectInfo.EndItemNo, SelectInfo.EndItemOffset);

      if vStartDrawItemNo = vEndDrawItemNo then  // ѡ����ͬһ��DrawItem
      begin
        vDrawRect.Left := vPt.X + ZoomIn(GetDrawItemOffsetWidth(vStartDrawItemNo,
          SelectInfo.StartItemOffset - DrawItems[vStartDrawItemNo].CharOffs + 1));
        vDrawRect.Top := vPt.Y;
        vDrawRect.Right := vPt.X + ZoomIn(GetDrawItemOffsetWidth(vEndDrawItemNo,
          SelectInfo.EndItemOffset - DrawItems[vEndDrawItemNo].CharOffs + 1));
        vDrawRect.Bottom := vPt.Y + ZoomIn(DrawItems[vEndDrawItemNo].Rect.Height);
      end
      else  // ѡ�в���ͬһ��DrawItem
      begin
        if not AForward then
        begin
          vDrawRect.Left := vPt.X;
          vDrawRect.Top := vPt.Y;
          vDrawRect.Right := vPt.X + ZoomIn(GetDrawItemOffsetWidth(vEndDrawItemNo,
            SelectInfo.EndItemOffset - DrawItems[vEndDrawItemNo].CharOffs + 1));
          vDrawRect.Bottom := vPt.Y + ZoomIn(DrawItems[vEndDrawItemNo].Rect.Height);
        end
        else
        begin
          vDrawRect.Left := vPt.X + ZoomIn(DrawItems[vStartDrawItemNo].Rect.Left - DrawItems[vEndDrawItemNo].Rect.Left
            + GetDrawItemOffsetWidth(vStartDrawItemNo, SelectInfo.StartItemOffset - DrawItems[vStartDrawItemNo].CharOffs + 1));
          vDrawRect.Top := vPt.Y + ZoomIn(DrawItems[vStartDrawItemNo].Rect.Top - DrawItems[vEndDrawItemNo].Rect.Top);
          vDrawRect.Right := vPt.X + ZoomIn(DrawItems[vStartDrawItemNo].Rect.Left - DrawItems[vEndDrawItemNo].Rect.Left
            + DrawItems[vStartDrawItemNo].Rect.Width);
          vDrawRect.Bottom := vDrawRect.Top + ZoomIn(DrawItems[vStartDrawItemNo].Rect.Height);
        end;
      end;
    end;

    if vDrawRect.Top < 0 then
      Self.FVScrollBar.Position := Self.FVScrollBar.Position + vDrawRect.Top
    else
    if vDrawRect.Bottom > FViewHeight then
      Self.FVScrollBar.Position := Self.FVScrollBar.Position + vDrawRect.Bottom - FViewHeight;

    if vDrawRect.Left < 0 then
      Self.FHScrollBar.Position := Self.FHScrollBar.Position + vDrawRect.Left
    else
    if vDrawRect.Right > FViewWidth then
      Self.FHScrollBar.Position := Self.FHScrollBar.Position + vDrawRect.Right - FViewWidth;
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
  Self.ActiveSection.PageNoFormat := Value;
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
  Printer.GetPrinter(vDevice, vDriver, vPort, vHDMode);
  if vHDMode <> 0 then
  begin
    // ��ȡָ��DeviceMode��ָ��
    vPDMode := GlobalLock(vHDMode);
    try
      if vPDMode <> nil then
      begin
        try
          vPDMode^.dmPaperSize := FSections[ASectionIndex].PaperSize;
          if vPDMode^.dmPaperSize = DMPAPER_HC_16K then  //  16K�ȷǹ��ʱ�׼ֽ���϶�Ϊ�Զ���ֽ��
            vPDMode^.dmPaperSize := DMPAPER_USER;

          vPDMode^.dmFields := vPDMode^.dmFields or DM_PAPERSIZE or DM_PAPERLENGTH or DM_PAPERWIDTH;

          if FSections[ASectionIndex].PaperOrientation = TPaperOrientation.cpoPortrait then
          begin
            vPDMode^.dmOrientation := DMORIENT_PORTRAIT;
            vPDMode^.dmPaperLength := Round(FSections[ASectionIndex].PaperHeight * 10); //ֽ������ñ������ֽ�ŵĳ�����
            vPDMode^.dmPaperWidth := Round(FSections[ASectionIndex].PaperWidth * 10);   //ֽ��
          end
          else
          begin
            vPDMode^.dmOrientation := DMORIENT_LANDSCAPE;
            vPDMode^.dmPaperLength := Round(FSections[ASectionIndex].PaperWidth * 10); //ֽ������ñ������ֽ�ŵĳ�����
            vPDMode^.dmPaperWidth := Round(FSections[ASectionIndex].PaperHeight * 10);   //ֽ��
          end;
        finally
          ResetDC(Printer.Handle, vPDMode^);
        end;
      end;
    finally
      GlobalUnlock(vHDMode);
    end;
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
    if FUndoList.Enable then  // �������̲�Ҫ�����µ�Undo
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

  {$REGION ' CalcDisplaySectionAndPage ��ȡ����ʾ����ʼ�ͽ����ڡ�ҳ��� '}
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

        vFirstPage := 0;  // ��ǰ��û�ҵ�����һ�ڴ�0��ʼ
      end;

      if FDisplayLastSection < 0 then  // û���ҵ�����ҳ����ֵΪ���һ�����һҳ
      begin
        FDisplayLastSection := FSections.Count - 1;
        FSections[FDisplayLastSection].DisplayLastPageIndex := FSections[FDisplayLastSection].PageCount - 1;
      end;
    end;

    if (FDisplayFirstSection < 0) or (FDisplayLastSection < 0) then
      raise Exception.Create('�쳣����ȡ��ǰ��ʾ��ʼҳ�ͽ���ҳʧ�ܣ�')
    else
    begin
      if FDisplayFirstSection <> FDisplayLastSection then  // ��ʼ�ͽ�������ͬһ��
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
      // ����һ���µļ������򣬸������ǵ�ǰ���������һ���ض����εĽ���
      //IntersectClipRect(FDataBmp.Canvas.Handle, 0, 0, FViewWidth , FViewHeight);
      vRgn := CreateRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      try
        SelectClipRgn(FDataBmp.Canvas.Handle, vRgn);
      finally
        DeleteObject(vRgn);
      end;

      // �ؼ�����
      if FViewModel = hvmFilm then
        FDataBmp.Canvas.Brush.Color := Self.Color
      else
      if FViewModel = hvmPage then
        FDataBmp.Canvas.Brush.Color := Self.Color
      else
        FDataBmp.Canvas.Brush.Color := FStyle.BackgroundColor;

      FDataBmp.Canvas.FillRect(Rect(0, 0, FDataBmp.Width, FDataBmp.Height));

      // ����ڴ˼��㵱ǰҳ��������ʼ���������Բ�����ARect����
      CalcDisplaySectionAndPage;  // ���㵱ǰ��Χ�ڿ���ʾ����ʼ�ڡ�ҳ�ͽ����ڡ�ҳ

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
          DoPaintViewBefor(FDataBmp.Canvas, vPaintInfo);  // ���δ����ػ濪ʼ

          if FAnnotatePre.DrawCount > 0 then  // ���������ǰ���һ�Σ����������м�¼��ҳ����ע����������ƶ�ʱ�жϹ�괦
            FAnnotatePre.ClearDrawAnnotate;     // ���ÿҳ����ǰ�����������ƶ�ʱ�����ж��Ƿ���������ҳ����ע��Χ�ڣ�ǰ��ҳ�еĶ�û����

          for i := FDisplayFirstSection to FDisplayLastSection do
          begin
            vPaintInfo.SectionIndex := i;

            vOffsetY := ZoomOut(FVScrollBar.Position) - GetSectionTopFilm(i);  // תΪԭʼY��ƫ��
            FSections[i].PaintDisplayPaper(GetSectionDrawLeft(i) - ZoomOut(FHScrollBar.Position),  // ԭʼX��ƫ��
              vOffsetY, FDataBmp.Canvas, vPaintInfo);
          end;

          for i := 0 to vPaintInfo.TopItems.Count - 1 do  // ���ƶ���Item
            vPaintInfo.TopItems[i].PaintTop(FDataBmp.Canvas);

          DoPaintViewAfter(FDataBmp.Canvas, vPaintInfo);  // ���δ����ػ����

          if (not vPaintInfo.Print) and Assigned(FCaret) and FCaret.DisFocus then  // α���
          begin
            FDataBmp.Canvas.Pen.Color := clGray;
            FDataBmp.Canvas.Pen.Style := psSolid;
            FDataBmp.Canvas.Pen.Width := FCaret.Width;
            FDataBmp.Canvas.MoveTo(FCaret.X, FCaret.Y);
            FDataBmp.Canvas.LineTo(FCaret.X, FCaret.Y + FCaret.Height);
          end;

          DoPaintViewHintLayer(FDataBmp.Canvas, vPaintInfo);
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

    // ֻ���±䶯���򣬷�ֹ��˸�����BitBlt��������л�λ����ʾ����ʱ������
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
  if (ALParam and GCS_RESULTSTR) <> 0 then  // ֪ͨ��������������ַ���
  begin
    // ���������ı�һ���Բ��룬����᲻ͣ�Ĵ���KeyPress�¼�
    vhIMC := ImmGetContext(Handle);
    if vhIMC <> 0 then
    begin
      try
        vSize := ImmGetCompositionString(vhIMC, GCS_RESULTSTR, nil, 0);  // ��ȡIME����ַ����Ĵ�С
        if vSize > 0 then  	// ���IME����ַ�����Ϊ�գ���û�д���
        begin
          // ȡ���ַ���
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
    // �������뷨��ǰ��괦������Ϣ
    {ImmGetCompositionFont(vhIMC, @vLogFont);
    vLogFont.lfHeight := 22;
    ImmSetCompositionFont(vhIMC, @vLogFont);}

    // �������뷨��ǰ���λ����Ϣ
    vCF.ptCurrentPos := Point(FCaret.X, FCaret.Y + FCaret.Height + 4);  // ���뷨��������λ��
    vCF.dwStyle := CFS_FORCE_POSITION;  // ǿ�ư��ҵ�λ��  CFS_RECT
    vCF.rcArea := ClientRect;
    ImmSetCompositionWindow(vhIMC, @vCF);
  finally
    ImmReleaseContext(Handle, vhIMC);
  end;
  {if ActiveSection.SelectInfo.StartItemOffset > 1 then  // �������뷨���ݵ�ǰλ�ô������±�ѡ
  begin
    if GetCurItem.StyleNo < 0 then Exit;

    vS := GetCurItem.GetTextPart(ActiveSection.SelectInfo.StartItemOffset - 1, 2);  // ���ع��ǰ2���ַ�
    if vS <> '' then
    begin
      if vS = '����' then
        vCandiID := 4743
      else
      if vS = '����' then
        vCandiID := 10019
      else
      if vS = 'ʧȥ' then
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
  //ActiveSection.DblClick(Message.XPos, Message.YPos);  // ˫��Ҳ�ŵ�MouseDown����
end;

procedure THCView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  DoSetFocus;
  // ����������ͨ���������޸Ĺ�괦�ֺź���������ý��㵽HCView�������ֹͣ��ȡ��ʽ��
  // ����Ե�ǰ���ǰ�ı���ʽ��Ϊ��ǰ��ʽ�ˣ�����ǽ���ʧȥ��������µ����ȡ����
  // ���ȴ�������ٴ���MouseDown����MouseDown�������ȡ��ǰ��ʽ����Ӱ��
  FStyle.UpdateInfoReCaret(False);
  FStyle.UpdateInfoRePaint;
  //FStyle.UpdateInfoReScroll;  // ʧȥ����ǰ��겻����ʾҳ����ȡ�����Ҫ����
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
        if Message.WParam = 2 then  // ��ѡʱ�Զ�����
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

    WM_IME_CHAR:  // ͨ�� UpdateImeComposition ȡ�����뷨����ֹ�س��ַ��ظ�����
      begin
        Message.Result := 1;
        Exit;
      end;
  end;

  inherited WndProc(Message);
end;

{ THCAnnotate }

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

procedure THCAnnotatePre.InsertDataAnnotate(const AAnnotateItem: THCAnnotateItem);
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
  i, j, vVOffset, vTop, vBottom, vRePlace, vSpace, vFirst, vLast: Integer;
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
      ACanvas.Brush.Color := $00F4F4F4;  // �����ע����

    ACanvas.FillRect(FDrawRect);
  end;

  if FDrawAnnotates.Count > 0 then  // ����ע
  begin
    vFirst := -1;
    vLast := -1;
    vSection := Sender as THCSection;
    vHeaderAreaHeight := vSection.GetHeaderAreaHeight;  // ҳü�߶�
    //�����е���ע
    vVOffset := APageRect.Top + vHeaderAreaHeight - APaintInfo.PageDataFmtTop;
    vTop := APaintInfo.PageDataFmtTop + vVOffset;
    vBottom := vTop + vSection.PaperHeightPix - vHeaderAreaHeight - vSection.PaperMarginBottomPix;

    for i := 0 to FDrawAnnotates.Count - 1 do  // �ұ�ҳ����ʼ�ͽ�����ע
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

    if vFirst >= 0 then  // ��ҳ����ע
    begin
      ACanvas.Font.Size := 8;
      ACanvas.Font.Name := '����';
      ACanvas.Font.Color := clBlack;

      // ���㱾ҳ����ע��ʾλ��
      vTop := FDrawAnnotates[vFirst].DrawRect.Top;
      for i := vFirst to vLast do
      begin
        vDrawAnnotate := FDrawAnnotates[i];
        if vDrawAnnotate.DrawRect.Top > vTop then
          vTop := vDrawAnnotate.DrawRect.Top;

        if vDrawAnnotate is THCDrawAnnotateDynamic then
          vText := (vDrawAnnotate as THCDrawAnnotateDynamic).Title + ':' + (vDrawAnnotate as THCDrawAnnotateDynamic).Text
        else
          vText := vDrawAnnotate.AnnotateItem.Content.Title + ':' + vDrawAnnotate.AnnotateItem.Content.Text;

        vTextRect := Rect(0, 0, AnnotationWidth - 30, 0);
        Windows.DrawTextEx(ACanvas.Handle, PChar(IntToStr(i) + vText), -1, vTextRect,
          DT_TOP or DT_LEFT or DT_WORDBREAK or DT_CALCRECT, nil);  // ��������
        if vTextRect.Right < AnnotationWidth - 30 then
          vTextRect.Right := AnnotationWidth - 30;

        if vDrawAnnotate.AnnotateItem.Replys.Count > 0 then
        begin
          vTextRect.Bottom := vTextRect.Bottom + vDrawAnnotate.AnnotateItem.Replys.Count * 20;
          for j := 0 to vDrawAnnotate.AnnotateItem.Replys.Count - 1 do
          begin

          end;
        end;

        vTextRect.Offset(APageRect.Right + 20, vTop + 5);
        vTextRect.Inflate(5, 5);
        vDrawAnnotate.Rect := vTextRect;

        vTop := vDrawAnnotate.Rect.Bottom + 5;
      end;

      if FDrawAnnotates[vLast].Rect.Bottom > APageRect.Bottom then  // ����ҳ�ײ��ˣ��������б�ҳ����ע��ʾλ��
      begin
        vVOffset := FDrawAnnotates[vLast].Rect.Bottom - APageRect.Bottom + 5;  // ��Ҫ������ô��Ŀռ�ɷ���

        vSpace := 0;  // ����ע֮����̶������Ŀ�϶
        vRePlace := -1;  // ����һ����ʼ����
        vTop := FDrawAnnotates[vLast].Rect.Top;
        for i := vLast - 1 downto vFirst do  // �������У�ȥ���м�Ŀ�϶
        begin
          vSpace := vTop - FDrawAnnotates[i].Rect.Bottom - 5;
          vVOffset := vVOffset - vSpace;  // �������ʣ��
          if vVOffset <= 0 then  // ��϶����
          begin
            vRePlace := i + 1;
            if vVOffset < 0 then
              vSpace := vSpace + vVOffset;  // vRePlace��ʵ����Ҫƫ�Ƶ���

            Break;
          end;

          vTop := FDrawAnnotates[i].Rect.Top;
        end;

        if vRePlace < 0 then  // ��ǰҳ��עȫ�����պ��ԷŲ��£�����ҳ�涥������һ����ע֮��Ŀ�϶
        begin
          vRePlace := vFirst;
          vSpace := FDrawAnnotates[vFirst].Rect.Top - APageRect.Top - 5;
          if vSpace > vVOffset then  // ��೬����Ҫ��
            vSpace := vVOffset;  // ֻ��������Ҫ��λ��
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
      for i := vFirst to vLast do  // ������ע
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
          vText := vDrawAnnotate.AnnotateItem.Content.Title + ':' + vDrawAnnotate.AnnotateItem.Content.Text;
          vData := vDrawAnnotate.Data as THCAnnotateData;

          if (vData.HotAnnotate.EndNo >= 0) and (vDrawAnnotate.AnnotateItem = vData.HotAnnotate.Data.Items[vData.HotAnnotate.EndNo]) then
          begin
            ACanvas.Pen.Style := TPenStyle.psSolid;
            ACanvas.Pen.Width := 1;
            ACanvas.Brush.Color := AnnotateBKActiveColor;
          end
          else
          if (vData.ActiveAnnotate.EndNo >= 0) and (vDrawAnnotate.AnnotateItem = vData.ActiveAnnotate.Data.Items[vData.ActiveAnnotate.EndNo]) then
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

        ACanvas.RoundRect(vDrawAnnotate.Rect, 5, 5);  // �����ע����
        vTextRect := vDrawAnnotate.Rect;
        vTextRect.Inflate(-5, -5);
        DrawTextEx(ACanvas.Handle, PChar(IntToStr(i) + vText), -1, vTextRect,
          DT_VCENTER or DT_LEFT or DT_WORDBREAK, nil);

        // ����ָ����
        ACanvas.Brush.Style := bsClear;
        ACanvas.MoveTo(vDrawAnnotate.DrawRect.Right, vDrawAnnotate.DrawRect.Bottom);
        ACanvas.LineTo(APageRect.Right, vDrawAnnotate.DrawRect.Bottom);
        ACanvas.LineTo(vDrawAnnotate.Rect.Left, vTextRect.Top);
      end;
    end;
  end;
end;

procedure THCAnnotatePre.RemoveDataAnnotate(const AAnnotateItem: THCAnnotateItem);
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
