{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-8-17             }
{                                                       }
{                  �ĵ��ڻ���ʵ�ֵ�Ԫ                   }
{                                                       }
{*******************************************************}

unit HCSection;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, HCViewData, HCSectionData,
  HCRichData, HCTextStyle, HCParaStyle, HCItem, HCCustomFloatItem, HCDrawItem,
  HCPage, HCRectItem, HCAnnotateItem, HCCommon, HCStyle, HCAnnotateData,
  HCCustomData, HCUndo, HCXml;

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
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
      const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;
  TSectionDataItemEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AItem: THCCustomItem) of object;
  TSectionDataActionEvent = function(const Sender: TObject; const AData: THCCustomData;
    const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean of object;
  TSectionDataItemNoFunEvent = function(const Sender: TObject; const AData: THCCustomData;
    const AItemNo: Integer): Boolean of object;
  TSectionDrawItemAnnotateEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const ADrawItemNo: Integer; const ADrawRect: TRect; const AAnnotateItem: THCAnnotateItem) of object;
  TSectionAnnotateEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AAnnotateItem: THCAnnotateItem) of object;
  TSectionDataItemMouseEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AItemNo, AOffset: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TSectionDataDrawItemMouseEvent = procedure(const Sender: TObject; const AData: THCCustomData;
    const AItemNo, AOffset, ADrawItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  THCCustomSection = class(TObject)
  private
    FStyle: THCStyle;
    FPages: THCPages;  // ����ҳ��
    FPaper: THCPaper;
    FPaperOrientation: TPaperOrientation;
    FHeader: THCHeaderData;
    FFooter: THCFooterData;
    FPage: THCPageData;
    FViewModel: THCViewModel;
    FActiveData: THCSectionData;  // ҳü�����ġ�ҳ��
    FMoveData: THCSectionData;

    /// <summary> �Ƿ�ԳƱ߾� </summary>
    FSymmetryMargin: Boolean;
    FPageNoVisible: Boolean;  // �Ƿ���ʾҳ��
    FPageNoFormat: string;
    FPagePadding: Byte;
    FPageNoFrom,  // ҳ��Ӽ���ʼ
    FActivePageIndex,  // ��ǰ�����ҳ
    FMousePageIndex,  // ��ǰ�������ҳ
    FDisplayFirstPageIndex,  // ���Ե�һҳ
    FDisplayLastPageIndex,   // �������һҳ
    FHeaderOffset,  // ҳü����ƫ��
    FPageNoOffset
      : Integer;
    FPageNoHorAlign: THCTextHorAlign;
    FOnDataChange,  // ҳü��ҳ�š�ҳ��ĳһ���޸�ʱ����
    FOnDataSetChange,
    FOnCheckUpdateInfo,  // ��ǰData��ҪUpdateInfo����ʱ����
    FOnReadOnlySwitch,  // ҳü��ҳ�š�ҳ��ֻ��״̬�����仯ʱ����
    FOnChangeTopLevelData  // �л�ҳü��ҳ�š����ġ����Ԫ��ʱ����
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

    procedure SetPageNoFormat(const Value: string);
    procedure SetViewModel(const Value: THCViewModel);
    /// <summary> ���ص�ǰ��ָ���Ĵ�ֱƫ�ƴ���Ӧ��ҳ </summary>
    /// <param name="AVOffset">��ֱƫ��</param>
    /// <returns>ҳ��ţ�-1��ʾ�޶�Ӧҳ</returns>
    function GetPageIndexByFilm(const AVOffset: Integer): Integer;

    /// <summary> ��ǰData��ҪUpdateInfo���� </summary>
    procedure DoActiveDataCheckUpdateInfo;
    procedure DoDataReadOnlySwitch(Sender: TObject);
    function DoGetScreenCoordEvent(const X, Y: Integer): TPoint;
    procedure DoDataDrawItemPaintBefor(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DoDataDrawItemPaintContent(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
      const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure DoDataDrawItemPaintAfter(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

    procedure DoDataInsertAnnotate(const AData: THCCustomData; const AAnnotateItem: THCAnnotateItem);
    procedure DoDataRemoveAnnotate(const AData: THCCustomData; const AAnnotateItem: THCAnnotateItem);
    procedure DoDataDrawItemAnnotate(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const AAnnotateItem: THCAnnotateItem);

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
    procedure DoDataSetChange(Sender: TObject);
    procedure DoDataItemReFormatRequest(const ASectionData: THCCustomData; const AItem: THCCustomItem);
    procedure DoDataItemSetCaretRequest(const ASectionData: THCCustomData; const AItemNo, AOffset: Integer);
    /// <summary> ����ItemԼ����Ҫ������ҳ���� </summary>
    procedure DoDataItemResized(const AData: THCCustomData; const AItemNo: Integer);
    function DoDataCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
    function DoDataCanEdit(const Sender: TObject): Boolean;
    function DoDataInsertTextBefor(const AData: THCCustomData; const AItemNo, AOffset: Integer;
      const AText: string): Boolean;
    procedure DoDataCreateItem(Sender: TObject);
    procedure DoDataCurParaNoChange(Sender: TObject);
    procedure DoDataCaretItemChanged(const AData: THCCustomData; const AItem: THCCustomItem);
    function DoDataGetUndoList: THCUndoList;

    /// <summary> ����ҳ��Dataָ��DrawItem���ڵ�ҳ(��ҳ�İ����λ������ҳ) </summary>
    /// <param name="ADrawItemNo"></param>
    /// <returns></returns>
    function GetPageIndexByPageDataDrawItem(const ADrawItemNo: Integer): Integer;

    /// <summary> ��ĳһҳ������ת����ҳָ��Data������(�˷�����ҪAX��AY�ڴ�ҳ�ϵ�ǰ��) </summary>
    /// <param name="APageIndex"></param>
    /// <param name="AData"></param>
    /// <param name="AX"></param>
    /// <param name="AY"></param>
    /// <param name="ARestrain">�Ƿ�Լ����Data�ľ���������</param>
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

    // ֽ����Ϣ
    function GetPaperSize: Integer;
    procedure SetPaperSize(const Value: Integer);
    // �߾���Ϣ
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
    function DoSectionDataAction(const AData: THCSectionData; const AAction: THCFunction): Boolean;

    procedure DoSaveToStream(const AStream: TStream); virtual;
    procedure DoLoadFromStream(const AStream: TStream; const AFileVersion: Word); virtual;

    property Style: THCStyle read FStyle;
  public
    constructor Create(const AStyle: THCStyle);
    destructor Destroy; override;
    //
    /// <summary> �޸�ֽ�ű߾� </summary>
    procedure ResetMargin;
    /// <summary> ActiveItem������Ӧ�价��(���ⲿֱ���޸�Item���Ժ����º���ǰ��Item�������) </summary>
    procedure ActiveItemReAdaptEnvironment;
    procedure DisActive;
    function SelectExists: Boolean;
    procedure SelectAll;
    function GetHint: string;
    function GetActiveItem: THCCustomItem;
    function GetActiveRectItem: THCCustomRectItem;
    function GetTopLevelItem: THCCustomItem;
    function GetTopLevelDrawItem: THCCustomDrawItem;
    function GetTopLevelDrawItemCoord: TPoint;

    function GetTopLevelRectDrawItemCoord: TPoint;
    /// <summary> �������ݸ�ʽ��AVerticalλ���ڽ����е�λ�� </summary>
    /// <param name="AVertical"></param>
    /// <returns></returns>
    function PageDataFormtToFilmCoord(const AVertical: Integer): Integer;

    /// <summary> ���ع���ѡ�н���λ������ҳ��� </summary>
    function GetPageIndexByCurrent: Integer;

    /// <summary> �������ĸ�ʽλ������ҳ��� </summary>
    function GetPageIndexByFormat(const AVOffset: Integer): Integer;
    /// <summary> ֱ�����õ�ǰTextItem��Textֵ </summary>
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

    /// <summary> �ӵ�ǰλ�ú��� </summary>
    function InsertBreak: Boolean;

    /// <summary> �ӵ�ǰλ�ú��ҳ </summary>
    function InsertPageBreak: Boolean;

    /// <summary> ���ݴ������"ģ��"������ </summary>
    /// <param name="AMouldDomain">"ģ��"������˷������������ͷ�</param>
    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;

    /// <summary> ��ǰѡ�е����������ע </summary>
    function InsertAnnotate(const ATitle, AText: string): Boolean;

    function SetActiveImage(const AImageStream: TStream): Boolean;
    //
    function ActiveTableResetRowCol(const ARowCount, AColCount: Integer): Boolean;
    function ActiveTableInsertRowAfter(const ARowCount: Integer): Boolean;
    function ActiveTableInsertRowBefor(const ARowCount: Integer): Boolean;
    function ActiveTableDeleteCurRow: Boolean;
    function ActiveTableSplitCurRow: Boolean;
    function ActiveTableSplitCurCol: Boolean;
    function ActiveTableInsertColAfter(const AColCount: Integer): Boolean;
    function ActiveTableInsertColBefor(const AColCount: Integer): Boolean;
    function ActiveTableDeleteCurCol: Boolean;
    //
    //// <summary> ������ת����ָ��ҳ���� </summary>
    procedure SectionCoordToPaper(const APageIndex, X, Y: Integer; var
      APageX, APageY: Integer);

    /// <summary> Ϊ��Ӧ�ö��뷽ʽ </summary>
    /// <param name="AAlign">�Է���ʽ</param>
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);
    procedure ApplyParaBackColor(const AColor: TColor);
    procedure ApplyParaBreakRough(const ARough: Boolean);
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode; const ASpace: Single);
    procedure ApplyParaLeftIndent(const AIndent: Single);
    procedure ApplyParaRightIndent(const AIndent: Single);
    procedure ApplyParaFirstIndent(const AIndent: Single);

    function DataAction(const AData: THCSectionData; const AAction: THCFunction): Boolean;
    /// <summary> ��ȡ�����Dtat�е�λ����Ϣ��ӳ�䵽ָ��ҳ�� </summary>
    /// <param name="APageIndex">Ҫӳ�䵽��ҳ���</param>
    /// <param name="ACaretInfo">���λ����Ϣ</param>
    procedure GetPageCaretInfo(var ACaretInfo: THCCaretInfo);

    /// <summary> ����ָ��ҳ��ָ����λ�ã�Ϊ��ϴ�ӡ������ADisplayWidth, ADisplayHeight���� </summary>
    /// <param name="APageIndex">Ҫ���Ƶ�ҳ��</param>
    /// <param name="ALeft">����Xƫ��</param>
    /// <param name="ATop">����Yƫ��</param>
    /// <param name="ACanvas"></param>
    procedure PaintPaper(const APageIndex, ALeft, ATop: Integer;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure Clear; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    /// <summary> ĳҳ���������е�Topλ�� </summary>
    /// <param name="APageIndex"></param>
    /// <returns></returns>
    function GetPageTopFilm(const APageIndex: Integer): Integer;
    function GetPageTop(const APageIndex: Integer): Integer;

    /// <summary> ����ָ��ҳ������ʼλ��������Data�е�Top��ע�� 20161216001 </summary>
    /// <param name="APageIndex"></param>
    /// <returns></returns>
    function GetPageDataFmtTop(const APageIndex: Integer): Integer;

    function GetPageDataHeight(const APageIndex: Integer): Integer;

    /// <summary> ҳü������ҳ�л���ʱ����ʼλ�� </summary>
    /// <returns></returns>
    function GetHeaderPageDrawTop: Integer;

    function GetPageMarginLeft(const APageIndex: Integer): Integer;

    /// <summary> ����ҳ��Գ����ԣ���ȡָ��ҳ�����ұ߾� </summary>
    /// <param name="APageIndex"></param>
    /// <param name="AMarginLeft"></param>
    /// <param name="AMarginRight"></param>
    procedure GetPageMarginLeftAndRight(const APageIndex: Integer;
      var AMarginLeft, AMarginRight: Integer);

    /// <summary> ������ָ��Item��ʼ���¼���ҳ </summary>
    /// <param name="AStartItemNo"></param>
    procedure BuildSectionPages(const AStartDrawItemNo: Integer);
    function DeleteSelected: Boolean;
    procedure DisSelect;
    function DeleteActiveAnnotate: Boolean;
    function DeleteActiveDomain: Boolean;
    procedure DeleteActiveDataItems(const AStartNo, AEndNo: Integer;
      const AKeepPara: Boolean);
    function MergeTableSelectCells: Boolean;
    function TableApplyContentAlign(const AAlign: THCContentAlign): Boolean;
    procedure ReFormatActiveParagraph;
    procedure ReFormatActiveItem;
    function GetHeaderAreaHeight: Integer;
    /// <summary> ��ҳ����������߶ȣ���ҳ���ҳü��ҳ�ź󾻸� </summary>
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    function GetFilmHeight: Cardinal;  // ����ҳ���+�ָ���
    function GetFilmWidth: Cardinal;

    /// <summary> �����ʽ�Ƿ����û�ɾ����ʹ�õ���ʽ��������ʽ��� </summary>
    /// <param name="AMark">True:�����ʽ�Ƿ����ã�Fasle:����ԭ��ʽ��ɾ����ʹ����ʽ��������</param>
    procedure MarkStyleUsed(const AMark: Boolean;
      const AParts: TSectionAreas = [saHeader, saPage, saFooter]);
    procedure SaveToStream(const AStream: TStream;
      const ASaveParts: TSectionAreas = [saHeader, saPage, saFooter]); virtual;
    function SaveToText: string;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word);
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean;
    procedure FormatData;

    /// <summary> ����ѡ�з�Χ(�粻��Ҫ���½����ֱ�ӵ���Data��SetSelectBound) </summary>
    procedure ActiveDataSetSelectBound(const AStartNo, AStartOffset, AEndNo, AEndOffset: Integer);

    procedure Undo(const AUndo: THCUndo);
    procedure Redo(const ARedo: THCUndo);
    // ����
    // ҳ��
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

    /// <summary> ��ǰ�ĵ���������(ҳü��ҳ�š�ҳ��)�����ݶ��� </summary>
    property ActiveData: THCSectionData read FActiveData write SetActiveData;

    /// <summary> ��ǰ�ĵ���������ҳü��ҳ�š�ҳ�� </summary>
    property ActiveArea: TSectionArea read GetActiveArea;
    property ActivePageIndex: Integer read FActivePageIndex;
    property ViewModel: THCViewModel read FViewModel write SetViewModel;
    /// <summary> �Ƿ�ԳƱ߾� </summary>
    property SymmetryMargin: Boolean read FSymmetryMargin write FSymmetryMargin;
    property DisplayFirstPageIndex: Integer read FDisplayFirstPageIndex write FDisplayFirstPageIndex;  // ���Ե�һҳ
    property DisplayLastPageIndex: Integer read FDisplayLastPageIndex write FDisplayLastPageIndex;  // �������һҳ
    property PageCount: Integer read GetPageCount;
    property PageNoVisible: Boolean read FPageNoVisible write FPageNoVisible;
    property PageNoFrom: Integer read FPageNoFrom write FPageNoFrom;
    property PageNoFormat: string read FPageNoFormat write SetPageNoFormat;
    property PageNoOffset: Integer read FPageNoOffset write FPageNoOffset;
    property PageNoHorAlign: THCTextHorAlign read FPageNoHorAlign write FPageNoHorAlign;
    property PagePadding: Byte read FPagePadding write FPagePadding;

    /// <summary> �ĵ����в����Ƿ�ֻ�� </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnDataSetChange: TNotifyEvent read FOnDataSetChange write FOnDataSetChange;
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
  private
    FPropertys: TStringList;
  protected
    procedure DoSaveToStream(const AStream: TStream); override;
    procedure DoLoadFromStream(const AStream: TStream; const AFileVersion: Word); override;
  public
    constructor Create(const AStyle: THCStyle);
    destructor Destroy; override;
    procedure AssignPaper(const ASource: THCCustomSection);
    /// <summary> ��ǰλ�ÿ�ʼ����ָ�������� </summary>
    /// <param name="AKeyword">Ҫ���ҵĹؼ���</param>
    /// <param name="AForward">True����ǰ��False�����</param>
    /// <param name="AMatchCase">True�����ִ�Сд��False�������ִ�Сд</param>
    /// <returns>True���ҵ�</returns>
    function Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean;
    function Replace(const AText: string): Boolean;
    function ParseHtml(const AHtmlText: string): Boolean;
    function InsertFloatItem(const AFloatItem: THCCustomFloatItem): Boolean;
    function SeekStreamToArea(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word; const APart: TSectionArea; const AUsePaper: Boolean): Boolean;
    function ToHtml(const APath: string): string;
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);

    property Propertys: TStringList read FPropertys;
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

function THCCustomSection.ActiveTableInsertColAfter(const AColCount: Integer): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableInsertColAfter(AColCount);
    end);
end;

function THCCustomSection.ActiveTableInsertColBefor(const AColCount: Integer): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableInsertColBefor(AColCount);
    end);
end;

function THCCustomSection.ActiveTableInsertRowAfter(const ARowCount: Integer): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableInsertRowAfter(ARowCount);
    end);
end;

function THCCustomSection.ActiveTableInsertRowBefor(const ARowCount: Integer): Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.TableInsertRowBefor(ARowCount);
    end);
end;

function THCCustomSection.ActiveTableResetRowCol(const ARowCount,
  AColCount: Integer): Boolean;
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
    begin
      FActiveData.ApplyParaLeftIndent(AIndent);
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

  FHeader.FormatChange := False;
  FFooter.FormatChange := False;
  FPage.FormatChange := False;
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
    AData.OnItemReFormatRequest := DoDataItemReFormatRequest;
    AData.OnItemSetCaretRequest := DoDataItemSetCaretRequest;
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
    AData.OnChange := DoDataSetChange;
  end;

begin
  inherited Create;
  FStyle := AStyle;
  FActiveData := nil;
  FMoveData := nil;
  FPageNoVisible := True;
  FPageNoFrom := 1;
  FPageNoFormat := '%d/%d';
  FPageNoHorAlign := THCTextHorAlign.hthaCenter;
  FPageNoOffset := 0;
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
  // ��ReFormatSectionData�д�����FData.PageHeight

  FHeader := THCHeaderData.Create(AStyle);
  SetDataProperty(FHeader);

  FFooter := THCFooterData.Create(AStyle);
  SetDataProperty(FFooter);

  FActiveData := FPage;
  FSymmetryMargin := True;  // �Գ�ҳ�߾� debug

  FPages := THCPages.Create;
  NewEmptyPage;           // �����հ�ҳ
  FPages[0].StartDrawItemNo := 0;
  FPages[0].EndDrawItemNo := 0;
end;

function THCCustomSection.DataAction(const AData: THCSectionData;
  const AAction: THCFunction): Boolean;
begin
  Result := DoSectionDataAction(AData, AAction);
end;

function THCCustomSection.DeleteActiveAnnotate: Boolean;
begin
  Result := DoSectionDataAction(FActiveData, function(): Boolean
    begin
      Result := FActiveData.DeleteActiveAnnotate;
    end);
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
  FActiveData.DisSelect;
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

procedure THCCustomSection.DoDataInsertAnnotate(const AData: THCCustomData; const AAnnotateItem: THCAnnotateItem);
begin
  if Assigned(FOnInsertAnnotate) then
    FOnInsertAnnotate(Self, AData, AAnnotateItem);
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
  const ADrawItemNo: Integer; const ADrawRect: TRect; const AAnnotateItem: THCAnnotateItem);
begin
  if Assigned(FOnDrawItemAnnotate) then
    FOnDrawItemAnnotate(Self, AData, ADrawItemNo, ADrawRect, AAnnotateItem);
end;

procedure THCCustomSection.DoDataDrawItemMouseMove(const AData: THCCustomData;
  const AItemNo, AOffset, ADrawItemNo: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnDrawItemMouseMove) then
    FOnDrawItemMouseMove(Self, AData, AItemNo, AOffset, ADrawItemNo, Button, Shift, X, Y);
end;

procedure THCCustomSection.DoDataDrawItemPaintAfter(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintAfter) then
  begin
    FOnDrawItemPaintAfter(Self, AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.DoDataDrawItemPaintBefor(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  if Assigned(FOnDrawItemPaintBefor) then
  begin
    FOnDrawItemPaintBefor(Self, AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADataDrawLeft,
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

procedure THCCustomSection.DoDataItemReFormatRequest(const ASectionData: THCCustomData;
  const AItem: THCCustomItem);
begin
  DoSectionDataAction(ASectionData as THCSectionData, function(): Boolean
    begin
      (ASectionData as THCSectionData).ItemReFormatResponse(AItem);
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
  vWidth := GetPageWidth;  // ҳ��

  vData := AData.GetRootData;  // ��ȡ����һ���ֵ�ResizeItem
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

procedure THCCustomSection.DoDataItemSetCaretRequest(const ASectionData: THCCustomData; const AItemNo, AOffset: Integer);
begin
  DoActiveDataCheckUpdateInfo;
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
  const AAnnotateItem: THCAnnotateItem);
begin
  if Assigned(FOnRemoveAnnotate) then
    FOnRemoveAnnotate(Self, AData, AAnnotateItem);
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

procedure THCCustomSection.DoDataSetChange(Sender: TObject);
begin
  if Assigned(FOnDataSetChange) then
    FOnDataSetChange(Sender);
end;

function THCCustomSection.DoGetScreenCoordEvent(const X, Y: Integer): TPoint;
begin
  if Assigned(FOnGetScreenCoord) then
    Result := FOnGetScreenCoord(X, Y);
end;

procedure THCCustomSection.DoLoadFromStream(const AStream: TStream; const AFileVersion: Word);
begin
  //---- ע��� SeekStreamTo ����һ��
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
  if (FViewModel <> hvmFilm) and (Value <> FPage) then Exit;

  if FActiveData <> Value then
  begin
    if FActiveData <> nil then
    begin
      FActiveData.DisSelect;
      FActiveData.DisActive;  // �ɵ�ȡ������
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
  //FActiveData.DisSelect;  // ����ѡ�У���ֹ��ʽ����ѡ��λ�ò����ڣ����λ�ù�0Ӱ�����飬��Data�ڲ�ReFormatʱ�Լ��ж�ѡ��λ���Ƿ�Խ��
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
        then  // ��ҳ��
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

function THCCustomSection.GetActiveRectItem: THCCustomRectItem;
begin
  Result := FActiveData.GetActiveRectItem as THCCustomRectItem;
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
  // ȷ�����ҳ����ʾ����
  if X < 0 then  // ����ҳ��ߵ�MinPadding����TEditArea.eaLeftPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if X > FPaper.WidthPix then  // ����ҳ�ұߵ�MinPadding����TEditArea.eaRightPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if Y < 0 then  // ����ҳ�ϱߵ�MinPadding����TEditArea.eaTopPad
  begin
    Result := FActiveData;
    Exit;
  end;

  if Y > FPaper.HeightPix then  // ֻ�������һҳ�±ߵ�MinPadding������ʱ����TEditArea.eaBottomPad
  begin
    Result := FActiveData;
    Exit;
  end;

  // �߾���Ϣ�������£�������
  if Y >= FPaper.HeightPix - FPaper.MarginBottomPix then  // 20200101001 =��Լ�����߽��ڲ�����ֹ��ѡʱ�жϲ���ȷ
    Exit(FFooter);

  // ҳü����ʵ�ʸ�(ҳü���ݸ߶�>�ϱ߾�ʱ��ȡҳü���ݸ߶�)
  if Y < GetHeaderAreaHeight then  // �����ҳü/�ϱ߾�����TEditArea.eaMarginTop
    Exit(FHeader);

  //if X > FPageSize.PageWidthPix - vMarginRight then Exit;  // �����ҳ�ұ߾�����TEditArea.eaMarginRight
  //if X < vMarginLeft then Exit;  // �����ҳ��߾�����TEditArea.eaMarginLeft
  //���Ҫ�������ұ߾಻�����ģ�ע��˫�����ж�ActiveDataΪnil
  Result := FPage;
end;

function THCCustomSection.PageDataFormtToFilmCoord(const AVertical: Integer): Integer;
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
  // ȡADrawItemNo��ʼλ������ҳ��û�п���ADrawItemNo��ҳ��������Ҫ���ǿɲο�TSection.BuildSectionPages
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

    if vPos >= AVOffset then  // AVOffset < 0ʱ��2�ֿ��ܣ�1��ǰ�ڵ�һҳǰ���Padding��2����һ����
    begin
      Result := i;
      Break;
    end;
  end;

  if (Result < 0) and (AVOffset > vPos) then  // ͬ�����һҳ���棬����һҳ����
    Result := FPages.Count - 1;
end;

procedure THCCustomSection.GetPageCaretInfo(var ACaretInfo: THCCaretInfo);
var
  vMarginLeft, vPageIndex: Integer;
begin
  if FStyle.UpdateInfo.DragingSelected then
    vPageIndex := FMousePageIndex
  else
    vPageIndex := FActivePageIndex;

  if (FActiveData.SelectInfo.StartItemNo < 0) or (vPageIndex < 0) then
  begin
    ACaretInfo.Visible := False;
    Exit;
  end;

  ACaretInfo.PageIndex := vPageIndex;  // ����������ڵ�ҳ
  FActiveData.GetCaretInfoCur(ACaretInfo);

  if ACaretInfo.Visible then  // ������ʾ
  begin
    if FActiveData = FPage then  // ҳ��䶯����Ҫ�жϹ�괦����ҳ
    begin
      vMarginLeft := GetPageIndexByFormat(ACaretInfo.Y);  // ���ñ���vMarginLeft��ʾҳ���
      if vPageIndex <> vMarginLeft then  // ����һ��һ���ֿ�ҳʱ�������һҳͬ�������ݵĵ�Ԫ����ص���һҳ
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
        ACaretInfo.Y := ACaretInfo.Y + GetHeaderPageDrawTop  // ҳ�ڽ��е�Topλ��
      else
      if FActiveData = FPage then
        ACaretInfo.Y := ACaretInfo.Y + GetHeaderAreaHeight - GetPageDataFmtTop(vPageIndex)  // - ҳ��ʼ������Data�е�λ��
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
        ACaretInfo.Y := ACaretInfo.Y - GetPageDataFmtTop(vPageIndex);  // - ҳ��ʼ������Data�е�λ��
    end;
  end;
end;

function THCCustomSection.GetPageCount: Integer;
begin
  Result := FPages.Count;  // ����ҳ��
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

function THCCustomSection.GetPageDataHeight(const APageIndex: Integer): Integer;
begin
  Result := Self.Page.DrawItems[FPages[APageIndex].EndDrawItemNo].Rect.Bottom
    - Self.Page.DrawItems[FPages[APageIndex].StartDrawItemNo].Rect.Top;
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
    Result := Result + FPaper.HeightPix + FPagePadding;  // ÿһҳ��������ķָ���Ϊһ��������Ԫ
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

procedure THCCustomSection.DoSaveToStream(const AStream: TStream);
begin
end;

function THCCustomSection.DoSectionDataAction(const AData: THCSectionData; const AAction: THCFunction): Boolean;
begin
  if not AData.CanEdit then Exit(False);
  //if AData.FloatItemIndex >= 0 then Exit(False);

  Result := AAction;  // ����䶯

  if AData.FormatChange then  // ���ݸ߶ȱ仯��
  begin
    AData.FormatChange := False;  // ��ֹ�´α����䶯��δ������߶ȱ仯�����θ�ʽ��FormatChangeΪTrue��Ӱ��

    if AData = FPage then
      BuildSectionPages(AData.FormatStartDrawItemNo)
    else
      BuildSectionPages(0);
  end;

  DoDataChanged(Self);  // ������޸Ĳ��������ﴥ���༭������ĸı�
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

  if FActiveData.KeyDownFloatItem(Key, Shift) then  // FloatItemʹ���˰���
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
          SetActivePageIndex(GetPageIndexByCurrent);  // ����������ƶ���������ҳ
          DoActiveDataCheckUpdateInfo;
        end;
    end;
  end
  else
    FActiveData.KeyDown(Key, Shift);
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
  vByte: Byte;
begin
  //---- ע��� SeekStreamTo ����һ��
  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));
  if AFileVersion > 41 then
    DoLoadFromStream(AStream, AFileVersion);

  AStream.ReadBuffer(FSymmetryMargin, SizeOf(FSymmetryMargin));  // �Ƿ�Գ�ҳ�߾�

  if AFileVersion > 11 then
  begin
    AStream.ReadBuffer(FPaperOrientation, SizeOf(FPaperOrientation));  // ֽ�ŷ���
    AStream.ReadBuffer(FPageNoVisible, SizeOf(FPageNoVisible));  // �Ƿ���ʾҳ��
  end;

  if AFileVersion > 45 then
  begin
    AStream.ReadBuffer(FPageNoFrom, SizeOf(FPageNoFrom));
    HCLoadTextFromStream(AStream, FPageNoFormat, AFileVersion);
    // ����JavaScript��C#
    FPageNoFormat := StringReplace(FPageNoFormat, '{0}', '%d', [rfReplaceAll, rfIgnoreCase]);
    FPageNoFormat := StringReplace(FPageNoFormat, '{1}', '%d', [rfReplaceAll, rfIgnoreCase]);
  end;

  FPaper.LoadToStream(AStream, AFileVersion);  // ҳ�����
  FPage.Width := GetPageWidth;

  // �ĵ�������Щ����������
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
  // 20190906001 FloatItem����ͨ��GetPageIndexByFormat(FPage.FloatItems[0].Top)������FloatItem
  // ��ҳ��ţ���Ϊ���ĵĿ����ϵ�ҳüҳ�Ŵ�����Top��GetPageIndexByFormat�����ڵ�ǰҳ��

  if AFileVersion > 58 then  // ҳ���ˮƽ�������ҳ��ײ���ƫ��
  begin
    AStream.ReadBuffer(FPageNoHorAlign, SizeOf(FPageNoHorAlign));
    AStream.ReadBuffer(FPageNoOffset, SizeOf(FPageNoOffset));
  end
  else
  begin
    FPageNoHorAlign := THCTextHorAlign.hthaCenter;
    FPageNoOffset := FFooter.Height;
  end;

  if AFileVersion > 61 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

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
  vPageIndex := GetPageIndexByFilm(Y);  // ����������ڵ�ҳ(�͹������ҳ���ܲ�����ͬһҳ�������ҳʱ���յ�Ԫ��ڶ�ҳ���ʱ������ǰһҳ)
  if FActivePageIndex <> vPageIndex then
    SetActivePageIndex(vPageIndex);

  SectionCoordToPaper(FActivePageIndex, X, Y, vX, vY);  // X��Yת����ָ��ҳ������vX,vY
  vNewActiveData := GetSectionDataAt(vX, vY);

  if (vNewActiveData <> FActiveData) and (ssDouble in Shift) and (FViewModel = hvmFilm) then  // ˫�����µ�Data
  begin
    SetActiveData(vNewActiveData);
    vChangeActiveData := True;
  end;

  {$REGION ' ��FloatItemʱ��· '}
  if FActiveData.FloatItems.Count > 0 then  // ��FloatItemʱ����
  begin
    vX2 := vX;  // ʹ������ı�������ֹFloatItem������ʱӰ���������������
    vY2 := vY;
    PaperCoordToData(FActivePageIndex, FActiveData, vX2, vY2, False);  // ����Item����Լ������������
    if FActiveData = FPage then  // FloatItem��PageData��
      vY2 := vY2 + GetPageDataFmtTop(FActivePageIndex);

    if FActiveData.MouseDownFloatItem(Button, Shift, vX2, vY2) then Exit;
  end;
  {$ENDREGION}

  PaperCoordToData(FActivePageIndex, FActiveData, vX, vY);

  if FActiveData = FPage then
    vY := vY + GetPageDataFmtTop(FActivePageIndex);

  if (ssDouble in Shift) and (not vChangeActiveData) then  // ��ͬһData��˫��
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
    GCursor := crDefault  { to do: �����ϽǼ�ͷ }
  else
  if X > FPaper.WidthPix - vMarginRight then
    GCursor := crDefault
  else
    GCursor := crIBeam;

  FMousePageIndex := GetPageIndexByFilm(Y);

  Assert(FMousePageIndex >= 0, '��Ӧ�ó�������ƶ�����ҳ���ϵ������');
  //if FMousePageIndex < 0 then Exit;  Ӧ����Զ�������

  {$REGION ' ��FloatItemʱ��· '}
  if FActiveData.FloatItems.Count > 0 then  // ��FloatItemʱ����
  begin
    if (Shift = [ssLeft]) and (FActiveData.FloatItemIndex >= 0) then  // ��ק�ƶ�FloatItem
    begin
      if not FActiveData.ActiveFloatItem.Resizing then  // ������
        FActiveData.ActiveFloatItem.PageIndex := FMousePageIndex;  // ��¼���ҳΪFloatItem���ڼ�
    end;

    if FActiveData = FPage then  // FloatItem��PageData��
    begin
      if (FActiveData.FloatItemIndex >= 0) and (FActiveData.ActiveFloatItem.Resizing) then  // ����ʱ������ҳΪ��׼
      begin
        SectionCoordToPaper(FActiveData.ActiveFloatItem.PageIndex, X, Y, vX, vY);
        PaperCoordToData(FActiveData.ActiveFloatItem.PageIndex, FActiveData, vX, vY, False);  // ����Item����Լ������������
        vY := vY + GetPageDataFmtTop(FActiveData.ActiveFloatItem.PageIndex);
      end
      else  // ���������ҳΪ׼
      begin
        SectionCoordToPaper(FMousePageIndex, X, Y, vX, vY);
        PaperCoordToData(FMousePageIndex, FActiveData, vX, vY, False);  // ����Item����Լ������������
        vY := vY + GetPageDataFmtTop(FMousePageIndex);
      end;
    end
    else  // FloatItem��Header��Footer
    begin
      if (FActiveData.FloatItemIndex >= 0) and (FActiveData.ActiveFloatItem.Resizing) then  // ����ʱ������ҳΪ��׼
      begin
        SectionCoordToPaper(FActivePageIndex, X, Y, vX, vY);
        PaperCoordToData(FActivePageIndex, FActiveData, vX, vY, False);  // ����Item����Լ������������
      end
      else  // ���������ҳΪ׼
      begin
        SectionCoordToPaper(FMousePageIndex, X, Y, vX, vY);
        PaperCoordToData(FMousePageIndex, FActiveData, vX, vY, False);  // ����Item����Լ������������
      end;
    end;

    if FActiveData.MouseMoveFloatItem(Shift, vX, vY) then Exit;
  end;
  {$ENDREGION}

  SectionCoordToPaper(FMousePageIndex, X, Y, vX, vY);

  vMoveData := GetSectionDataAt(vX, vY);
  if vMoveData <> FMoveData then
  begin
    if Assigned(FMoveData) then
    begin
      if not FMoveData.SelectedResizing then
      begin
        FMoveData.MouseLeave;
        FMoveData := vMoveData;
      end;
    end
    else
      FMoveData := vMoveData;
  end;

  PaperCoordToData(FMousePageIndex, FActiveData, vX, vY, Shift <> []);  // ��ͨ�Ƴ����Ĳ�Hot�κ�Item

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

  {$REGION ' ��FloatItemʱ��· '}
  if (FActiveData.FloatItems.Count > 0) and (FActiveData.FloatItemIndex >= 0) then  // ��FloatItemʱ����
  begin
    if FActiveData = FPage then  // FloatItem��PageData��
    begin
      SectionCoordToPaper(FActiveData.ActiveFloatItem.PageIndex, X, Y, vX, vY);
      PaperCoordToData(FActiveData.ActiveFloatItem.PageIndex, FActiveData, vX, vY, False);  // ����Item����Լ������������
      vY := vY + GetPageDataFmtTop(FActiveData.ActiveFloatItem.PageIndex);
    end
    else  // FloatItem��Header��Footer
    begin
      SectionCoordToPaper(vPageIndex, X, Y, vX, vY);
      PaperCoordToData(vPageIndex, FActiveData, vX, vY, False);  // ����Item����Լ������������
    end;

    if FActiveData.MouseUpFloatItem(Button, Shift, vX, vY) then Exit;
  end;
  {$ENDREGION}

  SectionCoordToPaper(vPageIndex, X, Y, vX, vY);
  PaperCoordToData(vPageIndex, FActiveData, vX, vY);

  if FActiveData = FPage then
    vY := vY + GetPageDataFmtTop(vPageIndex);

  // RectItem��������MouseUp�д���������Ҫ�ж��Ƿ���Ҫ�ı�
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

  { ˮƽ����Լ�����������ڱ���Ҳ���Ϊ��Ҫ�϶��ұ߿�
  if ARestrain then  // Ϊ�������ұ߽磬������Լ��1�������޷��㵽���׹�꣬������������RectItem
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

  // Ϊ����߽�(�������ģ���ҳüҳ�ŵ��ʱ�ж�����������λ����ɹ�����)Լ����ƫ��1
  if AData = FHeader then
  begin
    AY := AY - GetHeaderPageDrawTop;  // ���ҳü����λ��
    if ARestrain then  // Լ����ҳü����������
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
  if AData = FFooter then  // Լ����ҳ�ž���������
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
  if AData = FPage then  // Լ�������ľ���������
  begin
    //viTemp := GetHeaderAreaHeight;
    AY := AY - GetHeaderAreaHeight;
    if ARestrain then  // Ϊ������һҳ����һҳü�߽粻ȷ�����ϻ����£�Լ����ƫ��1
    begin
      if AY < 0 then
        AY := 1  // ���������ģ���ҳüҳ���е��
      else
      begin
        viTemp := GetPageHeight;
        if AY >= viTemp then  // 20200101001 =��Լ�����߽��ڲ�����ֹ��ѡʱ�жϲ���ȷ
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

    vPaperDrawTop := vPaperFilmTop - AFilmOffsetY;  // ӳ�䵽��ǰҳ��Ϊԭ���������ʼλ��(��Ϊ����)
    PaintPaper(i, AFilmOffsetX, vPaperDrawTop, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomSection.PaintPaper(const APageIndex, ALeft, ATop: Integer;
  const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  vHeaderAreaHeight, vMarginLeft, vMarginRight,
  vPaperDrawLeft, vPaperDrawTop, vPaperDrawRight, vPaperDrawBottom,
  vPageDrawLeft, vPageDrawTop, vPageDrawRight, vPageDrawBottom,  // ҳ�����λ��
  vPageDataScreenTop, vPageDataScreenBottom,  // ҳ������Ļλ��
  vScaleWidth, vScaleHeight: Integer;

  {$REGION ' ����ҳü���� '}
  procedure PaintHeader;
  var
    vHeaderDataDrawTop, vScreenBottom: Integer;
    vDCState: THCCanvas;
  begin
    // ����ScreenBottomʱ��������ű�С�������APaintInfo.WindowHeight��vPageDrawTop��С��
    // ����PaintData������ʾ������Item�����Բ�����Min(vPageDrawTop, APaintInfo.WindowHeight),
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

  {$REGION ' ����ҳ������ '}
  procedure PaintFooter;
  var
    vDCState: THCCanvas;
    vScreenBottom: Integer;
  begin
    // ����ScreenBottomʱ��������ű�С�������APaintInfo.WindowHeight��vPageDrawBottom��С��
    // ����PaintData������ʾ������Item�����Բ�����Min(vPaperDrawBottom, APaintInfo.WindowHeight),
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

  {$REGION ' ����ҳ������ '}
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

    { �������ݣ���Data��ָ��λ�õ����ݣ����Ƶ�ָ����ҳ�����У������տ���ʾ����������Լ�� }
    FPage.PaintData(vPageDrawLeft,  // ��ǰҳ����Ҫ���Ƶ���Left
      vPageDrawTop,     // ��ǰҳ����Ҫ���Ƶ���Top
      vPageDrawRight,   // ��ǰҳ����Ҫ���Ƶ���Right
      vPageDrawBottom,  // ��ǰҳ����Ҫ���Ƶ�Bottom
      vPageDataScreenTop,     // ������ֵ�ǰҳ���ݵ�Topλ��
      vPageDataScreenBottom,  // ������ֵ�ǰҳ����Bottomλ��
      APaintInfo.PageDataFmtTop,  // ָ�����ĸ�λ�ÿ�ʼ�����ݻ��Ƶ�ҳ������ʼλ��
      //FPages[APageIndex].StartDrawItemNo,
      //FPages[APageIndex].EndDrawItemNo,
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

    GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);  // ��ȡҳ���ұ߾����λ��
    vHeaderAreaHeight := GetHeaderAreaHeight;  // ҳü����ʵ�ʸ� = ҳü���ݶ���ƫ�� + ���ݸ߶ȣ������ϱ߾�ʱ�Դ�Ϊ׼
    vPageDrawTop := vPaperDrawTop + vHeaderAreaHeight;  // ӳ�䵽��ǰҳ�����Ͻ�Ϊԭ�����ʼλ��(��Ϊ����)
    vPageDrawBottom := vPaperDrawBottom - FPaper.MarginBottomPix;  // ҳ�����λ��(��Ϊ����)
  end
  else
  begin
    if APaintInfo.ViewModel = hvmPage then
    begin
      vPaperDrawRight := vPaperDrawLeft + FPaper.WidthPix;
      vPaperDrawBottom := vPaperDrawTop + GetPageHeight;
      //GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);  // ��ȡҳ���ұ߾����λ��
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
    vPageDrawTop := vPaperDrawTop;  // ӳ�䵽��ǰҳ�����Ͻ�Ϊԭ�����ʼλ��(��Ϊ����)
    vPageDrawBottom := vPaperDrawBottom - 1;  // ҳ�����λ��(��Ϊ����)
  end;

  vPageDrawLeft := vPaperDrawLeft + vMarginLeft;
  vPageDrawRight := vPaperDrawRight - vMarginRight;

  // ��ǰҳ��������ʾ����������߽�
  vPageDataScreenTop := Max(vPageDrawTop, 0);
  vPageDataScreenBottom := Min(vPageDrawBottom, vScaleHeight);
  { ��ǰҳ�ڵ�ǰ���Գ��������ݱ߽�ӳ�䵽��ʽ���еı߽� }
  APaintInfo.PageDataFmtTop := GetPageDataFmtTop(APageIndex);
  GetClipBox(ACanvas.Handle, vClipBoxRect);  // ���浱ǰ�Ļ�ͼ����

  if not APaintInfo.Print then  // �Ǵ�ӡʱ���Ʊ���ɫ
  begin
    ACanvas.Brush.Color := FStyle.BackgroundColor;
    ACanvas.FillRect(Rect(vPaperDrawLeft, vPaperDrawTop,
      Min(vPaperDrawRight, vScaleWidth),  // Լ���߽�
      Min(vPaperDrawBottom, vScaleHeight)));
  end;

  if Assigned(FOnPaintPaperBefor) then  // ����ҳ�����ǰ�¼�
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

  if not APaintInfo.Print then  // �Ǵ�ӡʱ����ҳüҳ��ָʾ��
  begin
    if APaintInfo.ViewModel = hvmFilm then
    begin
      {$REGION ' ҳü�߾�ָʾ�� '}
      if vPageDrawTop > 0 then  // ҳü����ʾ
      begin
        if vHeaderAreaHeight > FPaper.MarginTopPix then  // ҳü���ݳ���ҳ�ϱ߾�
        begin
          ACanvas.Pen.Style := TPenStyle.psDot;
          ACanvas.Pen.Color := clWebLightgrey;
          APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawLeft, vPageDrawTop - 1),
            Point(vPageDrawRight, vPageDrawTop - 1)]);
        end;

        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := TPenStyle.psSolid;
        if FActiveData = FHeader then  // ��ǰ�������ҳü������ҳü������
        begin
          ACanvas.Pen.Color := clBlue;
          ACanvas.MoveTo(vPageDrawLeft, vPageDrawTop);
          ACanvas.LineTo(vPageDrawRight, vPageDrawTop);
          // ����ҳü���ݷ�Χ����߿�
          ACanvas.MoveTo(vPageDrawLeft, vPageDrawTop);
          ACanvas.LineTo(vPageDrawLeft, vPaperDrawTop + FHeaderOffset);
          ACanvas.LineTo(vPageDrawRight, vPaperDrawTop + FHeaderOffset);
          ACanvas.LineTo(vPageDrawRight, vPageDrawTop);

          // ���ڱ༭ҳü��ʾ
          ACanvas.Brush.Color := $00F5E8D8;
          ACanvas.FillRect(Rect(vPageDrawLeft - 40, vPageDrawTop, vPageDrawLeft, vPageDrawTop + 20));
          ACanvas.Font.Size := 10;
          ACanvas.Font.Name := '����';
          ACanvas.Font.Style := [];
          ACanvas.Font.Color := $008B4215;
          ACanvas.TextOut(vPageDrawLeft - 32, vPageDrawTop + 4, 'ҳü');
        end
        else
          ACanvas.Pen.Color := clGray;

        // ���ϣ� ��-ԭ-��
        APaintInfo.DrawNoScaleLine(ACanvas, [
          Point(vPageDrawLeft - PMSLineHeight, ATop + FPaper.MarginTopPix),
          Point(vPageDrawLeft, ATop + FPaper.MarginTopPix),
          Point(vPageDrawLeft, ATop + FPaper.MarginTopPix - PMSLineHeight)]);
        // ���ϣ���-ԭ-��
        APaintInfo.DrawNoScaleLine(ACanvas, [
          Point(vPageDrawRight + PMSLineHeight, ATop + FPaper.MarginTopPix),
          Point(vPageDrawRight, ATop + FPaper.MarginTopPix),
          Point(vPageDrawRight, ATop + FPaper.MarginTopPix - PMSLineHeight)]);
      end;
      {$ENDREGION}

      {$REGION ' ҳ�ű߾�ָʾ�� '}
      if APaintInfo.GetScaleY(vPageDrawBottom) < APaintInfo.WindowHeight then  // ҳ�ſ���ʾ
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := TPenStyle.psSolid;
        if FActiveData = FFooter then  // ��ǰ�������ҳ�ţ����Ƽ�����
        begin
          ACanvas.Pen.Color := clBlue;
          ACanvas.MoveTo(vPageDrawLeft, vPageDrawBottom);
          ACanvas.LineTo(vPageDrawRight, vPageDrawBottom);

          // ���ڱ༭ҳ����ʾ
          ACanvas.Brush.Color := $00F5E8D8;
          ACanvas.FillRect(Rect(vPageDrawLeft - 40, vPageDrawBottom, vPageDrawLeft, vPageDrawBottom - 20));
          ACanvas.Font.Size := 10;
          ACanvas.Font.Name := '����';
          ACanvas.Font.Style := [];
          ACanvas.Font.Color := $008B4215;
          ACanvas.TextOut(vPageDrawLeft - 32, vPageDrawBottom - 16, 'ҳ��');
        end
        else
          ACanvas.Pen.Color := clGray;

        // ���£���-ԭ-��
        APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawLeft - PMSLineHeight, vPageDrawBottom),
          Point(vPageDrawLeft, vPageDrawBottom), Point(vPageDrawLeft, vPageDrawBottom + PMSLineHeight)]);
        // ���£���-ԭ-��
        APaintInfo.DrawNoScaleLine(ACanvas, [Point(vPageDrawRight + PMSLineHeight, vPageDrawBottom),
          Point(vPageDrawRight, vPageDrawBottom), Point(vPageDrawRight, vPageDrawBottom + PMSLineHeight)]);
      end;
      {$ENDREGION}
    end
    else
    begin
      if vPageDrawTop > 0 then  // ҳü����ʾ
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := TPenStyle.psSolid;  // TPenStyle.psDashDot;
        ACanvas.Pen.Color := clGray;

        ACanvas.MoveTo(vPaperDrawLeft, vPageDrawTop);
        ACanvas.LineTo(vPaperDrawRight, vPageDrawTop);
      end;

      if APaintInfo.GetScaleY(vPageDrawBottom) < APaintInfo.WindowHeight then  // ҳ�Ž�������ʾ
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := TPenStyle.psSolid;  // TPenStyle.psDashDot;
        ACanvas.Pen.Color := clGray;

        ACanvas.MoveTo(vPaperDrawLeft, vPageDrawBottom);
        ACanvas.LineTo(vPaperDrawRight, vPageDrawBottom);
      end;
    end;
  end;

  if APaintInfo.ViewModel = hvmFilm then
  begin
    {$REGION ' ����ҳü '}
    if vPageDrawTop > 0 then  // ҳü����ʾ
    begin
      vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPaperDrawLeft),
        Max(APaintInfo.GetScaleY(vPaperDrawTop + FHeaderOffset), 0),
        APaintInfo.GetScaleX(vPaperDrawRight),  // �����ʱ����Ͽ�ҳ������vPageDrawRight
        Min(APaintInfo.GetScaleY(vPageDrawTop), APaintInfo.WindowHeight));

      try
        //ACanvas.Brush.Color := clYellow;
        //FillRgn(ACanvas.Handle, vPaintRegion, ACanvas.Brush.Handle);
        SelectClipRgn(ACanvas.Handle, vPaintRegion);  // ���û�����Ч����
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

    {$REGION ' ����ҳ�� '}
    if APaintInfo.GetScaleY(vPageDrawBottom) < APaintInfo.WindowHeight then  // ҳ�ſ���ʾ
    begin
      vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPaperDrawLeft),
        Max(APaintInfo.GetScaleY(vPageDrawBottom), 0),
        APaintInfo.GetScaleX(vPaperDrawRight),  // �����ʱ����Ͽ�ҳ������vPageDrawRight
        Min(APaintInfo.GetScaleY(vPaperDrawBottom), APaintInfo.WindowHeight));

      try
        //ACanvas.Brush.Color := clRed;
        //FillRgn(ACanvas.Handle, vPaintRegion, ACanvas.Brush.Handle);
        SelectClipRgn(ACanvas.Handle, vPaintRegion);  // ���û�����Ч����
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

  {$REGION ' ����ҳ�� '}
  if vPageDataScreenBottom > vPageDataScreenTop then  // ��¶����������Ƶ�ǰҳ����������
  begin
    vPaintRegion := CreateRectRgn(APaintInfo.GetScaleX(vPaperDrawLeft),  // ���кŻ���ָʾ�����Դ�ֽ�����
      APaintInfo.GetScaleY(Max(vPageDrawTop, vPageDataScreenTop)),
      APaintInfo.GetScaleX(vPaperDrawRight),  // �����ʱ����Ͽ�ҳ������vPageDrawRight
      // �ײ��ó�1���أ�������ײ��߿�����ݻ��Ƶײ�һ��ʱ���߿���Ʋ�������Rgn��RectԼ����1���أ�
      APaintInfo.GetScaleY(Min(vPageDrawBottom, vPageDataScreenBottom)) + 1);
    try
      SelectClipRgn(ACanvas.Handle, vPaintRegion);  // ���û�����Ч����
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

  // �ָ�����׼������ҳ������(Ϊ֧�ָ���Item������ֽ�ű߾�)
  vPaintRegion := CreateRectRgn(
    APaintInfo.GetScaleX(vPaperDrawLeft),
    APaintInfo.GetScaleY(vPaperDrawTop),
    APaintInfo.GetScaleX(vPaperDrawRight),
    APaintInfo.GetScaleY(vPaperDrawBottom));
  try
    SelectClipRgn(ACanvas.Handle, vPaintRegion);

    if APaintInfo.ViewModel = hvmFilm then
    begin
      FHeader.PaintFloatItems(APageIndex, vPageDrawLeft,
        vPaperDrawTop + GetHeaderPageDrawTop, 0, ACanvas, APaintInfo);

      FFooter.PaintFloatItems(APageIndex, vPageDrawLeft,
        vPageDrawBottom, 0, ACanvas, APaintInfo);
    end;

    FPage.PaintFloatItems(APageIndex, vPageDrawLeft,  // ��ǰҳ���Ƶ���Left
      vPageDrawTop,     // ��ǰҳ���Ƶ���Top
      GetPageDataFmtTop(APageIndex),  // ָ�����ĸ�λ�ÿ�ʼ�����ݻ��Ƶ�ҳ������ʼλ��
      ACanvas,
      APaintInfo);
  finally
    DeleteObject(vPaintRegion);
  end;

  // �ָ�����׼�����ⲿ������
  vPaintRegion := CreateRectRgn(
    APaintInfo.GetScaleX(vClipBoxRect.Left),
    APaintInfo.GetScaleY(vClipBoxRect.Top),
    APaintInfo.GetScaleX(vClipBoxRect.Right),
    APaintInfo.GetScaleY(vClipBoxRect.Bottom));
  try
    SelectClipRgn(ACanvas.Handle, vPaintRegion);
  finally
    DeleteObject(vPaintRegion);
  end;

  if Assigned(FOnPaintPaperAfter) then  // ����ҳ����ƺ��¼�
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
    vSuplus,  // ������ҳ����ƫ�������ܺ�
    vBreakSeat  // ��ҳλ�ã���ͬRectItem�ĺ��岻ͬ������ʾ vBreakRow
      : Integer;

    {$REGION '_RectItemCheckPage'}
    procedure _RectItemCheckPage(const AStartSeat: Integer);  // ��ʼ��ҳ�����λ�ã���ͬRectItem���岻ͬ������ʾAStartRowNo
    var
      vFmtHeightInc, vFmtOffset: Integer;
      vDrawRect: TRect;
    begin
      vFmtOffset := 0;

      {if FPage.GetDrawItemStyle(ADrawItemNo) = THCStyle.PageBreak then
      begin
        vFmtOffset := vPageDataFmtBottom - FPage.DrawItems[ADrawItemNo].Rect.Top;

        vSuplus := vSuplus + vFmtOffset;
        if vFmtOffset > 0 then  // ���������ƶ���
          OffsetRect(FPage.DrawItems[ADrawItemNo].Rect, 0, vFmtOffset);

        vPageDataFmtTop := vPageDataFmtBottom;
        vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;

        _FormatNewPage(ADrawItemNo - 1, ADrawItemNo);  // �½�ҳ
      end
      else}
      if FPage.DrawItems[ADrawItemNo].Rect.Bottom > vPageDataFmtBottom then  // ��ǰҳ�Ų��±������(���м��)
      begin
        if (FPages[vPageIndex].StartDrawItemNo = ADrawItemNo)
          and (AStartSeat = 0)
          and (not vRectItem.CanPageBreak)
        then  // ��ǰҳ��ͷ��ʼ��ҳ�Ų��£�Ҳ������ضϣ�ǿ�Ʊ䰫��ǰҳ����ʾ������ʾ����
        begin
          vFmtHeightInc := vPageDataFmtBottom - FPage.DrawItems[ADrawItemNo].Rect.Bottom;
          vSuplus := vSuplus + vFmtHeightInc;
          FPage.DrawItems[ADrawItemNo].Rect.Bottom :=  // �����ʽ���߶�
            FPage.DrawItems[ADrawItemNo].Rect.Bottom + vFmtHeightInc;
          vRectItem.Height := vRectItem.Height + vFmtHeightInc;  // �������ﴦ���ػ�����RectItem�ڲ������ʣ�

          Exit;
        end;

        vDrawRect := FPage.DrawItems[ADrawItemNo].Rect;

        //if vSuplus = 0 then  // ��һ�μ����ҳ
        // �����м���ж��ˣ���Ϊ���������ı����ݱ�RectItem���ߣ�
        // �������RectItem��ߣ�RectItem��ʽ��ʱ�и���Style.LineSpaceMin��Ҳ��Ӱ��
        // �������Rect���е�һ���������ܷ������ڱ�ҳ��ͬ�к������ı�ȴ�Ų�����ҳ���ƣ���ɴ���
        //if vRectItem is THCDataItem then
        if (ADrawItemNo = FPage.DrawItems.Count - 1) or (FPage.DrawItems[ADrawItemNo + 1].LineFirst) then
          InflateRect(vDrawRect, 0, -FPage.GetLineBlankSpace(ADrawItemNo) div 2);

        vRectItem.CheckFormatPageBreak(  // ȥ���м����жϱ���ҳλ��
          FPages.Count - 1,
          vDrawRect.Top,  // ���Ķ���λ�� FPageData.DrawItems[ADrawItemNo].Rect.Top,
          vDrawRect.Bottom,  // ���ĵײ�λ�� FPageData.DrawItems[ADrawItemNo].Rect.Bottom,
          vPageDataFmtTop,
          vPageDataFmtBottom,  // ��ǰҳ�����ݵײ�λ��
          AStartSeat,  // ��ʼλ��
          vBreakSeat,  // ��ǰҳ��ҳ����(λ��)
          vFmtOffset,  // ��ǰRectItemΪ�˱ܿ���ҳλ����������ƫ�Ƶĸ߶�
          vFmtHeightInc  // ��ǰ�и���Ϊ�˱ܿ���ҳλ�õ�Ԫ�����ݶ���ƫ�Ƶ����߶�
        );

        if vBreakSeat < 0 then // ��ȥ�м����ÿ�ҳ�Ϳ�����ʾ��
        begin
          vSuplus := vSuplus + vPageDataFmtBottom - vDrawRect.Bottom;
        end
        else  // vBreakSeat >= 0 ��vBreakSeatλ�ÿ�ҳ
        if vFmtOffset > 0 then  // �����ҳ�����������ƶ���
        begin
          vFmtOffset := vFmtOffset + FPage.GetLineBlankSpace(ADrawItemNo) div 2;  // ���������ƶ���������������м��
          vSuplus := vSuplus + vFmtOffset + vFmtHeightInc;

          OffsetRect(FPage.DrawItems[ADrawItemNo].Rect, 0, vFmtOffset);

          vPageDataFmtTop := vPageDataFmtBottom;
          vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;
          _FormatNewPage(ADrawItemNo - 1, ADrawItemNo);  // �½�ҳ
          _RectItemCheckPage(vBreakSeat);
        end
        else  // ��ҳ����δ��������
        begin
          vSuplus := vSuplus{ + vFmtOffset} + vFmtHeightInc;
          FPage.DrawItems[ADrawItemNo].Rect.Bottom :=  // �����ʽ���߶�
            FPage.DrawItems[ADrawItemNo].Rect.Bottom + vFmtHeightInc;
          vRectItem.Height := vRectItem.Height + vFmtHeightInc;  // �������ﴦ���ػ�����RectItem�ڲ������ʣ�

          vPageDataFmtTop := vPageDataFmtBottom;
          vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;
          _FormatNewPage(ADrawItemNo, ADrawItemNo);  // �½�ҳ
          _RectItemCheckPage(vBreakSeat);  // �ӷ�ҳλ�ú����������Ƿ��ҳ
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
    _RectItemCheckPage(0);  // ���ʼλ�ã��������������Ƿ�����ʾ�ڵ�ǰҳ

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
    //if not DrawItems[ADrawItemNo].LineFirst then Exit; // ע��������ֻ���ʱ����Ͳ���ֻ�ж��е�1��
    if FPage.DrawItems[ADrawItemNo].Rect.Bottom > vPageDataFmtBottom then
    begin
      vH := vPageDataFmtBottom - FPage.DrawItems[ADrawItemNo].Rect.Top;
      for i := ADrawItemNo to FPage.DrawItems.Count - 1 do
        OffsetRect(FPage.DrawItems[i].Rect, 0, vH);

      vPageDataFmtTop := vPageDataFmtBottom;
      vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;
      _FormatNewPage(ADrawItemNo - 1, ADrawItemNo); // �½�ҳ
    end;
  end;
  {$ENDREGION}

var
  i, j, vPrioDrawItemNo, vFmtPageOffset: Integer;
  vPage: THCPage;
  vItem: THCCustomItem;
begin
  if FPage.FormatCount <> 0 then Exit;

  // ��һ������ҳ��Ϊ��ʽ����ʼҳ
  vPrioDrawItemNo := AStartDrawItemNo; // FPageData.GetItemLastDrawItemNo(AStartItemNo - 1)  // ��һ������DItem
  while vPrioDrawItemNo > 0 do
  begin
    if FPage.DrawItems[vPrioDrawItemNo].LineFirst then
      Break;

    Dec(vPrioDrawItemNo);
  end;
  Dec(vPrioDrawItemNo);  // ��һ��ĩβ

  vPageIndex := 0;
  if vPrioDrawItemNo > 0 then
  begin
    for i := FPages.Count - 1 downto 0 do  // ���ڿ�ҳ�ģ������λ������ҳ�����Ե���
    begin
      vPage := FPages[i];
      if (vPrioDrawItemNo >= vPage.StartDrawItemNo)
        and (vPrioDrawItemNo <= vPage.EndDrawItemNo)
      then  // ��Ϊ�����п�ҳ��������Ҫ�ж���ʼ����������
      begin
        vPageIndex := i;
        Break;
      end;
    end;
  end;

  FPages.DeleteRange(vPageIndex + 1, FPages.Count - vPageIndex - 1);  // ɾ����ǰҳ����ģ�׼����ʽ��

  if FPages.Count = 0 then  // ɾ��û�ˣ������һ��Page
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
      if vItem.PageBreak and (vItem.FirstDItemNo = i) then  // ��ҳItemֻ�жϵ�һ��DrawItem
      begin
        vFmtPageOffset := vPageDataFmtBottom - FPage.DrawItems[i].Rect.Top;
        if vFmtPageOffset > 0 then  // ���������ƶ���
        begin
          for j := i to FPage.DrawItems.Count - 1 do
            OffsetRect(FPage.DrawItems[j].Rect, 0, vFmtPageOffset);
        end;

        vPageDataFmtTop := vPageDataFmtBottom;
        vPageDataFmtBottom := vPageDataFmtTop + vPageHeight;

        _FormatNewPage(i - 1, i);  // �½�ҳ
      end;

      if FPage.GetDrawItemStyle(i) < THCStyle.Null then
        _FormatRectItemCheckPageBreak(i)
      else
        _FormatTextItemCheckPageBreak(i);
    end;
  end;

  FPages[vPageIndex].EndDrawItemNo := FPage.DrawItems.Count - 1;
  SetActivePageIndex(GetPageIndexByCurrent);

  for i := FPage.FloatItems.Count - 1 downto 0 do  // ������ɾ��ҳ��ų���ҳ������FloatItem
  begin
    if FPage.FloatItems[i].PageIndex > FPages.Count - 1 then
      FPage.FloatItems.Delete(i);
  end;

  FHeader.FormatChange := False;
  FFooter.FormatChange := False;
  FPage.FormatChange := False;
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
  //if vUndoList.Enable then  // �����жϣ���Ϊ�����ָ����̻����Σ���ֹ�����µĳ����ָ�
  if not vUndoList.GroupWorking then  // �������д���ʱ����������Data����Ӧ�䶯
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
  vByte: Byte;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // ���ݴ�Сռλ������Խ��
  //
  DoSaveToStream(AStream);
  if ASaveParts <> [] then
  begin
    AStream.WriteBuffer(FSymmetryMargin, SizeOf(FSymmetryMargin));  // �Ƿ�Գ�ҳ�߾�

    AStream.WriteBuffer(FPaperOrientation, SizeOf(FPaperOrientation));  // ֽ�ŷ���
    AStream.WriteBuffer(FPageNoVisible, SizeOf(FPageNoVisible));  // �Ƿ���ʾҳ��
    AStream.WriteBuffer(FPageNoFrom, SizeOf(FPageNoFrom));
    HCSaveTextToStream(AStream, FPageNoFormat);

    FPaper.SaveToStream(AStream);  // ҳ�����

    vArea := saHeader in ASaveParts;  // ��ҳü
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    vArea := saFooter in ASaveParts;  // ��ҳ��
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    vArea := saPage in ASaveParts;  // ��ҳ��
    AStream.WriteBuffer(vArea, SizeOf(vArea));

    if saHeader in ASaveParts then  // ��ҳü
    begin
      AStream.WriteBuffer(FHeaderOffset, SizeOf(FHeaderOffset));
      FHeader.SaveToStream(AStream);
    end;

    if saFooter in ASaveParts then  // ��ҳ��
      FFooter.SaveToStream(AStream);

    if saPage in ASaveParts then  // ��ҳ��
      FPage.SaveToStream(AStream);
  end;

  AStream.WriteBuffer(FPageNoHorAlign, SizeOf(FPageNoHorAlign));
  AStream.WriteBuffer(FPageNoOffset, SizeOf(FPageNoOffset));
  vByte := 0;
  AStream.WriteBuffer(vByte, SizeOf(vByte));
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // ��ǰ�����ݴ�С
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
  // Ԥ��ҳ�����Ű�ʱ����
  //GetPageMarginLeftAndRight(APageIndex, vMarginLeft, vMarginRight);
  APageX := X;// - vMarginLeft;

  if FViewModel = hvmFilm then
    vPageFilmTop := GetPageTopFilm(APageIndex)
  else
    vPageFilmTop := GetPageTop(APageIndex);

  APageY := Y - vPageFilmTop;  // ӳ�䵽��ǰҳ��Ϊԭ���������ʼλ��(��Ϊ����)
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

procedure THCCustomSection.SetPageNoFormat(const Value: string);
begin
  if FPageNoFormat <> Value then
  begin
    FPageNoFormat := Value;
    DoActiveDataCheckUpdateInfo;
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

procedure THCCustomSection.SetViewModel(const Value: THCViewModel);
begin
  if FViewModel <> Value then
  begin
    FViewModel := Value;
    SetActiveData(FPage);
  end;
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
  //if vUndoList.Enable then  // �����жϣ���Ϊ�����ָ����̻����Σ���ֹ�����µĳ����ָ�
  if not vUndoList.GroupWorking then  // �������д���ʱ����������Data����Ӧ�䶯
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

procedure THCSection.AssignPaper(const ASource: THCCustomSection);
begin
  Self.PaperSize := ASource.PaperSize;
  Self.PaperWidth := ASource.PaperWidth;
  Self.PaperHeight := ASource.PaperHeight;
  Self.PaperMarginTop := ASource.PaperMarginTop;
  Self.PaperMarginLeft := ASource.PaperMarginLeft;
  Self.PaperMarginRight := ASource.PaperMarginRight;
  Self.PaperMarginBottom := ASource.PaperMarginBottom;
  Self.PaperOrientation := ASource.PaperOrientation;
  Self.HeaderOffset := ASource.HeaderOffset;
  Self.ResetMargin;
end;

constructor THCSection.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  FPropertys := TStringList.Create;
end;

destructor THCSection.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

procedure THCSection.DoLoadFromStream(const AStream: TStream; const AFileVersion: Word);
var
  vS: string;
begin
  //---- ע��� SeekStreamTo ����һ��
  inherited DoLoadFromStream(AStream, AFileVersion);
  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

procedure THCSection.DoSaveToStream(const AStream: TStream);
begin
  inherited DoSaveToStream(AStream);
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

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
      FPaper.Size := StrToInt(vsPaper[0]);  // ֽ�Ŵ�С
      FPaper.Width := StrToFloat(vsPaper[1]);  // ֽ�ſ��
      FPaper.Height := StrToFloat(vsPaper[2]);  // ֽ�Ÿ߶�
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
      vsMargin.DelimitedText := ANode.Attributes['margin'];  // �߾�
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
  FSymmetryMargin := ANode.Attributes['symmargin'];  // �Ƿ�Գ�ҳ�߾�
  FPaperOrientation := TPaperOrientation(ANode.Attributes['ori']);  // ֽ�ŷ���

  FPageNoVisible := ANode.Attributes['pagenovisible'];  // �Ƿ�Գ�ҳ�߾�
  if ANode.HasAttribute('pagenofrom') then
    FPageNoFrom := ANode.Attributes['pagenofrom'];

  if ANode.HasAttribute('pagenoformat') then
    FPageNoFormat := ANode.Attributes['pagenoformat'];

  GetXmlPaper_;
  GetXmlPaperMargin_;

  if ANode.HasAttribute('property') then
    FPropertys.Text := GetXmlRN(ANode.Attributes['property']);

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

function THCSection.SeekStreamToArea(const AStream: TStream; const AStyle: THCStyle;
  const AFileVersion: Word; const APart: TSectionArea; const AUsePaper: Boolean): Boolean;
var
  vDataSize: Int64;
  vS: string;
  vPaper: THCPaper;
  vLoadParts: TSectionAreas;
  vArea: Boolean;
begin
  Result := False;
  //---- ע��� LoadFromStream ����һ��
  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));
  if AFileVersion > 41 then  //--- ע��� DoLoadFromStream ����һ��
    HCLoadTextFromStream(AStream, vS, AFileVersion);

  AStream.Position := AStream.Position + SizeOf(FSymmetryMargin);  // �Ƿ�Գ�ҳ�߾�

  if AFileVersion > 11 then
  begin
    AStream.Position := AStream.Position + SizeOf(FPaperOrientation);  // ֽ�ŷ���
    AStream.Position := AStream.Position + SizeOf(FPageNoVisible);  // �Ƿ���ʾҳ��
  end;

  if AFileVersion > 45 then
  begin
    AStream.Position := AStream.Position + SizeOf(FPageNoFrom);  // FPageNoFrom
    HCLoadTextFromStream(AStream, vS, AFileVersion);  // FPageNoFormat
  end;

  if AUsePaper then
  begin
    FPaper.LoadToStream(AStream, AFileVersion);  // ҳ�����
  end
  else
  begin
    vPaper := THCPaper.Create;
    try
      vPaper.LoadToStream(AStream, AFileVersion);  // ҳ�����
    finally
      FreeAndNil(vPaper);
    end;
  end;

  // �ĵ�������Щ����������
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
    AStream.Position := AStream.Position + SizeOf(FHeaderOffset);
    if APart = TSectionArea.saHeader then Exit(True);

    //FHeader.LoadFromStream(AStream, FStyle, AFileVersion);
    AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));
    AStream.Position := AStream.Position + vDataSize;
  end;
  if APart = TSectionArea.saHeader then Exit(True);

  if APart = TSectionArea.saFooter then Exit(True);
  if saFooter in vLoadParts then
  begin
    //FFooter.LoadFromStream(AStream, FStyle, AFileVersion);
    AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));
    AStream.Position := AStream.Position + vDataSize;
  end;

  if APart = TSectionArea.saPage then
  begin
    if AFileVersion > 63 then
    begin
      AStream.Position := AStream.Position + SizeOf(Byte);
      //FShowUnderLine := Odd(vByte shr 7);
      //FShowBorder := Odd(vByte shr 6);
      AStream.Position := AStream.Position + SizeOf(FPage.LineStyle);
      //HCLoadColorFromStream(AStream, vColor);
      AStream.Position := AStream.Position + SizeOf(Byte);  // a
      AStream.Position := AStream.Position + SizeOf(Byte);  // r
      AStream.Position := AStream.Position + SizeOf(Byte);  // g
      AStream.Position := AStream.Position + SizeOf(Byte);  // b

      AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));

      AStream.Position := AStream.Position + SizeOf(FPage.FormatDirection);
    end
    else
    begin
      AStream.Position := AStream.Position + SizeOf(FPage.ShowUnderLine);  // �»���
      AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));
    end;

    Result := True;
  end;
end;

function THCSection.ToHtml(const APath: string): string;
begin
  Result := FHeader.ToHtml(APath) + sLineBreak + FPage.ToHtml(APath) + sLineBreak + FFooter.ToHtml(APath);
end;

procedure THCSection.ToXml(const ANode: IHCXMLNode);
var
  vNode: IHCXMLNode;
begin
  ANode.Attributes['symmargin'] := FSymmetryMargin; // �Ƿ�Գ�ҳ�߾�
  ANode.Attributes['ori'] := Ord(FPaperOrientation);  // ֽ�ŷ���
  ANode.Attributes['pagenovisible'] := FPageNoVisible;  // �Ƿ���ʾҳ��
  ANode.Attributes['pagenofrom'] := FPageNoFrom;
  ANode.Attributes['pagenoformat'] := FPageNoFormat;

  ANode.Attributes['pagesize'] :=  // ֽ�Ŵ�С
    IntToStr(FPaper.Size)
    + ',' + FormatFloat('0.#', FPaper.Width)
    + ',' + FormatFloat('0.#', FPaper.Height) ;

  ANode.Attributes['margin'] :=  // �߾�
    FormatFloat('0.#', FPaper.MarginLeft) + ','
    + FormatFloat('0.#', FPaper.MarginTop) + ','
    + FormatFloat('0.#', FPaper.MarginRight) + ','
    + FormatFloat('0.#', FPaper.MarginBottom);

  if FPropertys.Text <> '' then
    ANode.Attributes['property'] := FPropertys.Text;

  // ��ҳü
  vNode := ANode.AddChild('header');
  vNode.Attributes['offset'] := FHeaderOffset;
  FHeader.ToXml(vNode);

  // ��ҳ��
  vNode := ANode.AddChild('footer');
  FFooter.ToXml(vNode);

  // ��ҳ��
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
