{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{            �ĵ�RectItem�������ʵ�ֵ�Ԫ               }
{                                                       }
{*******************************************************}

unit HCRectItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCItem, HCDrawItem, HCTextStyle, HCParaStyle,
  HCStyleMatch, HCStyle, HCCommon, HCCustomData, HCUndo, HCXml;

const
  /// <summary> �����RectItemǰ�� </summary>
  OffsetBefor = 0;

  /// <summary> �����RectItem������ </summary>
  OffsetInner = 1;

  /// <summary> �����RectItem���� </summary>
  OffsetAfter = 2;

type
  THCCustomRectItem = class(THCCustomItem)  // RectItem����
  strict private
    FWidth, FHeight: Integer;
    FTextWrapping: Boolean;  // �ı�����
    FOwnerData: THCCustomData;
    // ��ʶ�ڲ��߶��Ƿ����˱仯�����ڴ�Item�ڲ���ʽ��ʱ����������Data��ʶ��Ҫ���¸�ʽ����Item
    // �����һ����Ԫ�����ݱ仯��û������������仯ʱ������Ҫ���¸�ʽ�����Ҳ����Ҫ���¼���ҳ��
    // ��ӵ�д�Item��Dataʹ�����Ӧ��������ֵΪFalse���ɲο�TableItem.KeyPress��ʹ��
    FIsFormatDirty,
    FCanPageBreak: Boolean;  // �ڵ�ǰҳ��ʾ����ʱ�Ƿ���Է�ҳ�ض���ʾ
    FOnGetMainUndoList: TGetUndoListEvent;
  protected
    FMangerUndo: Boolean;  // �Ƿ��Լ��������ָ�
    function GetWidth: Integer; virtual;
    procedure SetWidth(const Value: Integer); virtual;
    function GetHeight: Integer; virtual;
    procedure SetHeight(const Value: Integer); virtual;
    procedure DoChange; virtual;
    //
    procedure SelfUndoListInitializate(const AUndoList: THCUndoList);
    procedure SelfUndo_New;
    function GetSelfUndoList: THCUndoList;

    procedure DoSelfUndoDestroy(const AUndo: THCUndo); virtual;
    function DoSelfUndoNew: THCUndo; virtual;
    procedure DoSelfUndo(const AUndo: THCUndo); virtual;
    procedure DoSelfRedo(const ARedo: THCUndo); virtual;
    function GetPageBreakCount: Cardinal; virtual;
  public
    /// <summary> �����������ڼ䴴�� </summary>
    constructor Create(const AOwnerData: THCCustomData); overload; virtual;
    /// <summary> �����ڼ���ʱ���� </summary>
    constructor Create(const AOwnerData: THCCustomData; const AWidth, AHeight: Integer); overload; virtual;
    function MatchTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch): Boolean; virtual;
    procedure ApplySelectTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch); virtual;
    /// <summary> RectItem�ڲ�Data�Ƿ���Ӧ���޸���ʽ </summary>
    procedure ApplySelectParaStyle(const AStyle: THCStyle; const AMatchStyle: THCParaMatch); virtual;
    /// <summary> RectItem���ݶ��뷽ʽ </summary>
    procedure ApplyContentAlign(const AAlign: THCContentAlign); virtual;
    // ��ǰRectItem��ʽ��ʱ������Data(Ϊ������봫��TCustomRichData����)
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); virtual;

    /// <summary> ���������Ϊ�����ҳ�Ⱦ������ӵĸ߶�(Ϊ���¸�ʽ��ʱ�������ƫ����) </summary>
    function ClearFormatExtraHeight: Integer; virtual;
    procedure ReFormatActiveItem; virtual;
    procedure ReFormatRequest; virtual;
    /// <summary> ActiveItem������Ӧ�价��(���ⲿֱ���޸�Item���Ժ����º���ǰ��Item�������) </summary>
    procedure ActiveItemReAdaptEnvironment; virtual;
    function DeleteSelected: Boolean; virtual;
    function InsertAnnotate(const ATitle, AText: string): Boolean; virtual;
    function DeleteActiveAnnotate: Boolean; virtual;
    /// <summary> ɾ����ǰ�� </summary>
    function DeleteActiveDomain: Boolean; virtual;
    /// <summary> ɾ����ǰDataָ����Χ�ڵ�Item </summary>
    procedure DeleteActiveDataItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean); virtual;
    /// <summary> ֱ�����õ�ǰTextItem��Textֵ </summary>
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
    /// <summary> ��ȡָ��Xλ�ö�Ӧ��Offset </summary>
    function GetOffsetAt(const X: Integer): Integer; virtual;
    /// <summary> ��ȡ����X��Y�Ƿ���ѡ�������� </summary>
    function CoordInSelect(const X, Y: Integer): Boolean; virtual;
    /// <summary> ��������ʱ�ڲ��Ƿ���ָ����Key��Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; virtual;
    /// <summary> ��ɢ����ʱ�Ƿ�ּ�� </summary>
    function JustifySplit: Boolean; virtual;
    /// <summary> ���¹��λ�� </summary>
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); virtual;

    /// <summary> ��ȡ��ָ���߶��ڵĽ���λ�ô����¶�(��ʱû�õ�ע����) </summary>
    /// <param name="AHeight">ָ���ĸ߶ȷ�Χ</param>
    /// <param name="ADItemMostBottom">��׶�DItem�ĵײ�λ��</param>
    //procedure GetPageFmtBottomInfo(const AHeight: Integer; var ADItemMostBottom: Integer); virtual;

    // ׼���жϷ�ҳǰ����������������Լ���¼�ķ�ҳ��Ϣ��׼�����¼����ҳ
    procedure CheckFormatPageBreakBefor; virtual;

    /// <summary> �����ʽ����ķ�ҳλ�� </summary>
    /// <param name="ADrawItemRectTop">��Ӧ��DrawItem��Rect.Top�����м��һ��</param>
    /// <param name="ADrawItemRectBottom">��Ӧ��DrawItem��Rect.Bottom�����м��һ��</param>
    /// <param name="APageDataFmtTop">ҳ����Top</param>
    /// <param name="APageDataFmtBottom">ҳ����Bottom</param>
    /// <param name="AStartSeat">��ʼ�����ҳλ��</param>
    /// <param name="ABreakSeat">��Ҫ��ҳλ��</param>
    /// <param name="AFmtOffset">Ϊ�˱ܿ���ҳλ����������ƫ�Ƶĸ߶�</param>
    /// <param name="AFmtHeightInc">Ϊ�˱ܿ���ҳλ�ø߶�����ֵ</param>
    procedure CheckFormatPageBreak(const APageIndex, ADrawItemRectTop,
      ADrawItemRectBottom, APageDataFmtTop, APageDataFmtBottom, AStartSeat: Integer;
      var ABreakSeat, AFmtOffset, AFmtHeightInc: Integer); virtual;
    function CheckPageSurplus(const ASurplus: Integer): Integer; virtual;

    function InsertItem(const AItem: THCCustomItem): Boolean; virtual;
    function InsertText(const AText: string): Boolean; virtual;
    function InsertGraphic(const AGraphic: TGraphic; const ANewPara: Boolean): Boolean; virtual;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;

    /// <summary> �����ۡ���ȫѡ��(�����ڽ�RectItem���ѡ������µ�ѡ���ж�) </summary>
    function IsSelectComplateTheory: Boolean; virtual;

    function SelectExists: Boolean; virtual;

    /// <summary> ��ǰλ�ÿ�ʼ����ָ�������� </summary>
    /// <param name="AKeyword">Ҫ���ҵĹؼ���</param>
    /// <param name="AForward">True����ǰ��False�����</param>
    /// <param name="AMatchCase">True�����ִ�Сд��False�������ִ�Сд</param>
    /// <returns>True���ҵ�</returns>
    function Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean; virtual;
    procedure Clear; virtual;

    /// <summary> ����ָ��λ�ô��Ķ���Data(Ϊ������뷵��TCustomData����) </summary>
    function GetTopLevelDataAt(const X, Y: Integer): THCCustomData; virtual;
    function GetTopLevelData: THCCustomData; virtual;
    /// <summary> ��ǰRectItem�Ƿ�����Ҫ�����Data(Ϊ������뷵��TCustomData����) </summary>
    function GetActiveData: THCCustomData; virtual;
    procedure FormatDirty; virtual;
    procedure TraverseItem(const ATraverse: THCItemTraverse); virtual;
    function SaveToBitmap(var ABitmap: TBitmap): Boolean; virtual;
    //
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function BreakByOffset(const AOffset: Integer): THCCustomItem; override;
    function CanConcatItems(const AItem: THCCustomItem): Boolean; override;
    procedure Assign(Source: THCCustomItem); override;

    // ����������ط���
    procedure Undo(const AUndoAction: THCCustomUndoAction); override;
    procedure Redo(const ARedoAction: THCCustomUndoAction); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function ToHtml(const APath: string): string; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    function GetLength: Integer; override;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property TextWrapping: Boolean read FTextWrapping write FTextWrapping;  // �ı�����
    property IsFormatDirty: Boolean read FIsFormatDirty write FIsFormatDirty;

    /// <summary> �ڵ�ǰҳ��ʾ����ʱ�Ƿ���Է�ҳ�ض���ʾ </summary>
    property CanPageBreak: Boolean read FCanPageBreak write FCanPageBreak;
    property PageBreakCount: Cardinal read GetPageBreakCount;
    property OwnerData: THCCustomData read FOwnerData;
    property MangerUndo: Boolean read FMangerUndo;
  end;

  THCDomainItemClass = class of THCDomainItem;

  THCDomainItem = class(THCCustomRectItem)  // ��
  private
    FLevel: Byte;  // ��Ƕ�ײ㼶��ϵ������Ϊ0������Ϊ1
    FMarkType: TMarkType;
  protected
    FDrawRect: TRect;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    Empty: Boolean;
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    class function IsBeginMark(const AItem: THCCustomItem): Boolean;
    class function IsEndMark(const AItem: THCCustomItem): Boolean;
    function GetOffsetAt(const X: Integer): Integer; override;
    function JustifySplit: Boolean; override;
    // ��ǰRectItem��ʽ��ʱ������Data(Ϊ������봫��TCustomRichData����)
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure PaintTop(const ACanvas: TCanvas); override;
    function SaveToBitmap(var ABitmap: TBitmap): Boolean; override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property MarkType: TMarkType read FMarkType write FMarkType;
    property Level: Byte read FLevel write FLevel;
  end;

  THCTextHorAlign = (hthaLeft, hthaCenter, hthaRight);

  THCTextRectItem = class(THCCustomRectItem)  // ���ı���ʽ��RectItem
  private
    FTextStyleNo: Integer;
  protected
    procedure SetTextStyleNo(const Value: Integer); virtual;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    function JustifySplit: Boolean; override;
    function MatchTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch): Boolean; override;
    procedure ApplySelectTextStyle(const AStyle: THCStyle;
      const AMatchStyle: THCStyleMatch); override;
    procedure MarkStyleUsed(const AMark: Boolean); override;
    function SelectExists: Boolean; override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property TextStyleNo: Integer read FTextStyleNo write SetTextStyleNo;
  end;

  THCControlItem = class(THCTextRectItem)
  private
    FTag: Integer;
    FAutoSize: Boolean;  // �Ǹ��������Զ���С�������ⲿָ����С
    FEnabled: Boolean;
    FOnClick: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
  protected
    FMouseIn: Boolean;
    FPaddingLeft, FPaddingRight, FPaddingTop, FPaddingBottom: Byte;
    FMinWidth, FMinHeight: Integer;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure DoClick; virtual;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    function ClientRect: TRect; virtual;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property Enabled: Boolean read FEnabled write FEnabled;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
  end;

  TGripType = (gtNone, gtLeftTop, gtRightTop, gtLeftBottom, gtRightBottom,
    gtLeft, gtTop, gtRight, gtBottom);

  THCResizeRectItem = class(THCCustomRectItem)  // �ɸı��С��RectItem
  private
    FGripSize: Word;  // �϶����С
    FResizing: Boolean;  // �����϶��ı��С
    FCanResize: Boolean;  // ��ǰ�Ƿ��ڿɸı��С״̬
    FResizeGrip: TGripType;
    FResizeRect: TRect;
    FResizeWidth, FResizeHeight: Integer;  // ���ź�Ŀ���
    function GetGripType(const X, Y: Integer): TGripType;
  protected
    FResizeX, FResizeY: Integer;  // �϶�����ʱ��ʼλ��
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;

    // �����ָ���ط���
    procedure SelfUndo_Resize(const ANewWidth, ANewHeight: Integer);
    procedure DoSelfUndoDestroy(const AUndo: THCUndo); override;
    procedure DoSelfUndo(const AUndo: THCUndo); override;
    procedure DoSelfRedo(const ARedo: THCUndo); override;

    function GetResizing: Boolean; virtual;
    procedure SetResizing(const Value: Boolean); virtual;
    function GetAllowResize: Boolean;  // �����Ƿ���������(����OwnerDataֻ��ʱ)����ʱû�ж���Ϊ�鷽��
    property ResizeGrip: TGripType read FResizeGrip;
    property ResizeRect: TRect read FResizeRect;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    procedure Assign(Source: THCCustomItem); override;
    /// <summary> ��ȡ����X��Y�Ƿ���ѡ�������� </summary>
    function CoordInSelect(const X, Y: Integer): Boolean; override;
    procedure PaintTop(const ACanvas: TCanvas); override;
    // �̳�THCCustomItem���󷽷�
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function CanDrag: Boolean; override;
    function SelectExists: Boolean; override;
    function IsSelectComplateTheory: Boolean; override;
    function GetOffsetAt(const X: Integer): Integer; override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summary> Լ����ָ����С��Χ�� </summary>
    procedure RestrainSize(const AWidth, AHeight: Integer); virtual;
    property GripSize: Word read FGripSize write FGripSize;
    property Resizing: Boolean read GetResizing write SetResizing;
    property ResizeWidth: Integer read FResizeWidth;
    property ResizeHeight: Integer read FResizeHeight;
    property CanResize: Boolean read FCanResize write FCanResize;
    property AllowResize: Boolean read GetAllowResize;
  end;

  THCAnimateRectItem = class(THCCustomRectItem)  // ����RectItem
  public
    function GetOffsetAt(const X: Integer): Integer; override;
  end;

  THCDataItem = class(THCResizeRectItem)  // �ڲ��й���Data��Item��Ҳ����ͳһ�ж��Ƿ����RectItem�ڲ��жϻ�Ѱ��
  { to do: ����������RectItem�õ��ķ�����ֲ��������}
  protected
    procedure DoChange; override;
  end;

var
  HCDefaultDomainItemClass: THCDomainItemClass = THCDomainItem;

implementation

uses
  SysUtils, HCFormatData;

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

  if FCanPageBreak then  // �ɷ�ҳ��ʾ����ǰҳ����һ����
  begin
    ABreakSeat := Height - AStartSeat - (APageDataFmtBottom - ADrawItemRectTop);
    if ADrawItemRectBottom > APageDataFmtBottom then
      AFmtHeightInc := APageDataFmtBottom - ADrawItemRectBottom;
  end
  else  // ����ҳ��ʾ��ƫ�Ƶ���һҳ��ͷ��ʾ
  begin
    ABreakSeat := 0;
    if ADrawItemRectBottom > APageDataFmtBottom then
      AFmtOffset := APageDataFmtBottom - ADrawItemRectTop;
  end;
end;

procedure THCCustomRectItem.CheckFormatPageBreakBefor;
begin
end;

function THCCustomRectItem.CheckPageSurplus(const ASurplus: Integer): Integer;
begin
  Result := 0;
end;

function THCCustomRectItem.CoordInSelect(const X, Y: Integer): Boolean;
begin
  if IsSelectComplate then
    Result := PtInRect(Bounds(0, 0, FWidth, FHeight), Point(X, Y))
  else
    Result := False;
end;

constructor THCCustomRectItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create;
  FOwnerData := AOwnerData;
  Self.ParaNo := AOwnerData.CurParaNo;
  FOnGetMainUndoList := (AOwnerData as THCCustomData).OnGetUndoList;
  FWidth := 100;   // Ĭ�ϳߴ�
  FHeight := 50;
  FTextWrapping := False;
  FIsFormatDirty := False;
  FCanPageBreak := False;
  FMangerUndo := False;
end;

constructor THCCustomRectItem.Create(const AOwnerData: THCCustomData; const AWidth, AHeight: Integer);
begin
  Create(AOwnerData);
  Width := AWidth;
  Height := AHeight;
end;

function THCCustomRectItem.DeleteActiveAnnotate: Boolean;
begin
  Result := False;
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
  // Sender.Data�ɰ��Զ���Ķ���
  Result := THCUndo.Create;
end;

procedure THCCustomRectItem.DoChange;
begin
  OwnerData.Change;
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
  FIsFormatDirty := True;
  (Self.OwnerData as THCFormatData).FormatDirty;
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
  //if Self.Active then
  ACaretInfo.Visible := False;
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

function THCCustomRectItem.GetPageBreakCount: Cardinal;
begin
  Result := 0;
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
  if Assigned(Result) and Result.Enable
    and (Result.Count > 0)  // ��ֹͨ������ֱ�Ӳ���(�ǽ���)��Ԫ��Data��������ʱ��Undo�б�Ϊ��ʱ���쳣
    and (Result.Last.Actions.Count > 0)
    and (Result.Last.Actions.Last is THCItemSelfUndoAction)
  then
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

function THCCustomRectItem.InsertAnnotate(const ATitle, AText: string): Boolean;
begin
  Result := False;
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

function THCCustomRectItem.MatchTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch): Boolean;
begin
  Result := False;
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
        SelfUndoListInitializate(vUndoList);  // ����RectItem�������ɾ�����ٻָ��Ǵ������ص���ʵ��������¼��Ѿ�����ʵ���ˣ���Ҫ���¸�ֵ
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

procedure THCCustomRectItem.ReFormatRequest;
begin
  FormatDirty;
  (OwnerData as THCFormatData).ItemReFormatRequest(Self);
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

procedure THCCustomRectItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
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

    if APath <> '' then  // ���浽ָ�����ļ�����
    begin
      if not DirectoryExists(APath + 'images') then
        CreateDir(APath + 'images');
      vFileName := OwnerData.Style.GetHtmlFileTempName + '.bmp';
      vBitmap.SaveToFile(APath + 'images\' + vFileName);

      Result := '<img width="' + IntToStr(FWidth) + '" height="' + IntToStr(FHeight)
        + '" src="images/' + vFileName + '" alt="' + Self.ClassName + '" />';
    end
    else  // ����ΪBase64
    begin
      Result := '<img width="' + IntToStr(FWidth) + '" height="' + IntToStr(FHeight)
        + '" src="data:image/jpg;base64,' + GraphicToBase64(vBitmap) + '" alt="' + Self.ClassName + '" />';
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

procedure THCResizeRectItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FCanResize := (Source as THCResizeRectItem).CanResize;
end;

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

  if (not APaintInfo.Print) and Active then  // ����״̬�����ƽ����ê��
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

    if AllowResize then // ���������϶���ʾê��
    begin
      ACanvas.Brush.Color := clGray;
      ACanvas.FillRect(Bounds(ADrawRect.Left, ADrawRect.Top, GripSize, GripSize));
      ACanvas.FillRect(Bounds(ADrawRect.Right - GripSize, ADrawRect.Top, GripSize, GripSize));
      ACanvas.FillRect(Bounds(ADrawRect.Left, ADrawRect.Bottom - GripSize, GripSize, GripSize));
      ACanvas.FillRect(Bounds(ADrawRect.Right - GripSize, ADrawRect.Bottom - GripSize, GripSize, GripSize));
    end;
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

function THCResizeRectItem.GetAllowResize: Boolean;
begin
  Result := FCanResize and OwnerData.CanEdit;
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

function THCResizeRectItem.GetOffsetAt(const X: Integer): Integer;
begin
  if AllowResize then
    Result := inherited GetOffsetAt(X)
  else
  begin
    if X < Width div 2 then
      Result := OffsetBefor
    else
      Result := OffsetAfter;
  end;
end;

function THCResizeRectItem.GetResizing: Boolean;
begin
  Result := FResizing;
end;

function THCResizeRectItem.IsSelectComplateTheory: Boolean;
begin
  if not AllowResize then
    Result := False
  else
    Result := inherited IsSelectComplateTheory;
end;

procedure THCResizeRectItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 44 then
    AStream.ReadBuffer(FCanResize, SizeOf(FCanResize));
end;

function THCResizeRectItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  FResizeGrip := gtNone;
  Result := inherited MouseDown(Button, Shift, X, Y);
  if Active and AllowResize then
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
  if Active and AllowResize then
  begin
    if FResizing then  // ����������
    begin
      vBL := Width / Height;
      vW := X - FResizeX;
      vH := Y - FResizeY;

      // ��������λ���ڶԽ��ߵĲ�ͬ��λ���㳤��
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

            if vW < 0 then  // ���
            begin
              if vH < vTempH then  // �Խ������棬�����Ժ���Ϊ׼
                vH := vTempH
              else  // �Խ������棬����������Ϊ׼
                vW := -vTempW;
            end
            else  // �Ҳ�
            begin
              if (vW > vTempW) or (vH > vTempH) then  // �Խ������棬����������Ϊ׼
              begin
                if vH < 0 then
                  vW := vTempW
                else
                  vW := -vTempW;
              end
              else  // �Խ������棬�����Ժ���Ϊ׼
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
    else  // ��������
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
    //ACanvas.DrawFocusRect(ADrawRect);  // �����Ϊɶ����������
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

procedure THCResizeRectItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('canresize') then
    FCanResize := ANode.Attributes['canresize'];
end;

procedure THCResizeRectItem.RestrainSize(const AWidth, AHeight: Integer);
begin
end;

procedure THCResizeRectItem.SaveToStreamRange(const AStream: TStream;
  const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
  AStream.WriteBuffer(FCanResize, SizeOf(FCanResize));
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

procedure THCResizeRectItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['canresize'] := FCanResize;
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

  if not OwnerData.Style.States.Contain(THCState.hosLoading) then
  begin
    if Assigned(AStyle) then
      FTextStyleNo := OwnerData.Style.GetStyleNo(AStyle.TextStyles[FTextStyleNo], True)
    else
      FTextStyleNo := OwnerData.Style.GetDefaultStyleNo;
  end
  else
  if Assigned(AStyle) and (FTextStyleNo > AStyle.TextStyles.Count - 1) then  // ������ʷ����(ɾ��������ʽʱû��)
    FTextStyleNo := 0;
end;

procedure THCTextRectItem.MarkStyleUsed(const AMark: Boolean);
begin
  if AMark then  // ���
    OwnerData.Style.TextStyles[FTextStyleNo].CheckSaveUsed := True
  else  // ���¸�ֵ
    FTextStyleNo := OwnerData.Style.TextStyles[FTextStyleNo].TempNo;
end;

function THCTextRectItem.MatchTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch): Boolean;
begin
  AMatchStyle.Append := not AMatchStyle.StyleHasMatch(AStyle, FTextStyleNo);
  Result := True;
end;

procedure THCTextRectItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
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
  FEnabled := (Source as THCControlItem).Enabled;
end;

function THCControlItem.ClientRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

constructor THCControlItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  FMouseIn := False;
  FAutoSize := True;
  FEnabled := True;
  FPaddingLeft := 5;
  FPaddingRight := 5;
  FPaddingTop := 5;
  FPaddingBottom := 5;
  FMinWidth := 20;
  FMinHeight := 10;
  FTag := 0;
end;

procedure THCControlItem.DoClick;
begin
  if Assigned(FOnClick) and OwnerData.CanEdit then
    FOnClick(Self);
end;

procedure THCControlItem.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //inherited KeyDown(Key, Shift);
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure THCControlItem.KeyPress(var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
end;

procedure THCControlItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FAutoSize := ANode.Attributes['autosize'];
  if ANode.HasAttribute('enabled') then
    FEnabled := ANode.Attributes['enabled']
  else
    FEnabled := True;

  if ANode.HasAttribute('tag') then
    FTag := ANode.Attributes['tag']
  else
    FTag := 0;
end;

procedure THCControlItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion < 51 then
    AStream.ReadBuffer(FAutoSize, SizeOf(FAutoSize))
  else
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    FAutoSize := Odd(vByte shr 7);
    FEnabled := Odd(vByte shr 6);
    if AFileVersion > 53 then
      AStream.ReadBuffer(FTag, SizeOf(FTag))
    else
      FTag := 0;
  end;
end;

procedure THCControlItem.MouseEnter;
begin
  FMouseIn := True;
  inherited MouseEnter;
end;

procedure THCControlItem.MouseLeave;
begin
  FMouseIn := False;
  inherited MouseLeave;
end;

function THCControlItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  if (Button = mbLeft) and FEnabled and PtInRect(Self.ClientRect, Point(X, Y)) then
    DoClick;

  Result := inherited MouseUp(Button, Shift, X, Y);
end;

procedure THCControlItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FAutoSize then
    vByte := vByte or (1 shl 7);

  if FEnabled then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  AStream.WriteBuffer(FTag, SizeOf(FTag));
end;

procedure THCControlItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['autosize'] := FAutoSize;
  ANode.Attributes['enabled'] := FEnabled;
  ANode.Attributes['tag'] := FTag;
end;

{ THCDomainItem }

procedure THCDomainItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FMarkType := (Source as THCDomainItem).MarkType;
  FLevel := (Source as THCDomainItem).Level;
  Empty := (Source as THCDomainItem).Empty;
end;

constructor THCDomainItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.Domain;
  FMarkType := TMarkType.cmtBeg;
  FLevel := 0;
  Width := 0;
  Height := AOwnerData.Style.TextStyles[0].FontHeight;
  Empty := True;
end;

procedure THCDomainItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then  // ����[��]
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
  Self.Height := ARichData.Style.TextStyles[0].FontHeight;;  // Ĭ�ϴ�С
  Empty := False;

  if FMarkType = TMarkType.cmtBeg then  // ����ʼ��ʶ
  begin
    if AItemNo < ARichData.Items.Count - 1 then
    begin
      vItem := ARichData.Items[AItemNo + 1];
      if (vItem.StyleNo = Self.StyleNo)
        and ((vItem as THCDomainItem).MarkType = TMarkType.cmtEnd)
      then
      begin
        Self.Width := 10;
        Empty := True;
      end
      else
      if vItem.ParaFirst then
        Self.Width := 10
      else
      if vItem.StyleNo > THCStyle.Null then
        Self.Height := ARichData.Style.TextStyles[vItem.StyleNo].FontHeight;
    end
    else
      Self.Width := 10;
  end
  else  // �������ʶ
  begin
    vItem := ARichData.Items[AItemNo - 1];  // ǰһ��
    if (vItem.StyleNo = Self.StyleNo)
      and ((vItem as THCDomainItem).MarkType = TMarkType.cmtBeg)
    then
    begin
      Self.Width := 10;
      Empty := True;
    end
    else
    if Self.ParaFirst then
      Self.Width := 10
    else
    if vItem.StyleNo > THCStyle.Null then
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

procedure THCDomainItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
  AStream.WriteBuffer(FMarkType, SizeOf(FMarkType));
  AStream.WriteBuffer(FLevel, SizeOf(FLevel));
end;

procedure THCDomainItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['mark'] := Ord(FMarkType);
  ANode.Attributes['level'] := FLevel;
end;

{ THCDataItem }

procedure THCDataItem.DoChange;
begin
  Self.FormatDirty;
  inherited DoChange;
end;

end.
