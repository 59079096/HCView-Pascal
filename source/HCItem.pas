{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{                �ĵ��������ʵ�ֵ�Ԫ                   }
{                                                       }
{*******************************************************}

unit HCItem;

interface

uses
  Windows, Classes, Controls, Graphics, Generics.Collections, HCCommon, HCStyle,
  HCUndo, HCXml;

type
  TScaleInfo = record
    MapMode: Integer;
    WindowOrg: TSize;
    WindowExt: TSize;
    ViewportOrg: TSize;
    ViewportExt: TSize;
  end;

  THCViewModel = (
    /// <summary> ������ͼ����ʾҳü��ҳ�� </summary>
    hvmFilm,
    /// <summary> Page��ͼ����ʾ���ұ߾࣬����ʾҳü��ҳ�� </summary>
    hvmPage,
    /// <summary> Text��ͼ������ʾҳ�߾��ҳüҳ�� </summary>
    hvmEdit
  );

  TItemOptions = set of (ioParaFirst, ioPageBreak);

  TItemSelectState = (issNone, issPart, issComplate);

  THCCustomItemClass = class of THCCustomItem;

  THCCustomItem = class;

  TPaintInfo = class(TObject)  // ����ʱ����Ϣ�����ڸ��ⲿ�¼����Ӹ������Ϣ
  private
    FPrint: Boolean;
    FViewModel: THCViewModel;
    FTopItems: TObjectList<THCCustomItem>;
    FWindowWidth, FWindowHeight, FDPI: Integer;
    FScaleX, FScaleY,  // Ŀ�껭������ʾ������dpi����(��ӡ��dpi����ʾ��dpi��һ��ʱ�����ű���)
    FZoom  // ��ͼ���õķŴ����
      : Single;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ScaleCanvas(const ACanvas: TCanvas): TScaleInfo;
    procedure RestoreCanvasScale(const ACanvas: TCanvas; const AOldInfo: TScaleInfo);
    function GetScaleX(const AValue: Integer): Integer;
    function GetScaleY(const AValue: Integer): Integer;
    procedure DrawNoScaleLine(const ACanvas: TCanvas; const APoints: array of TPoint); overload;
    procedure DrawNoScaleLine(const ACanvas: TCanvas; const AX1, AY1, AX2, AY2: Integer); overload;
    property Print: Boolean read FPrint write FPrint;

    /// <summary> ������ʾģʽ��ҳ�桢Web </summary>
    property ViewModel: THCViewModel read FViewModel write FViewModel;

    /// <summary> ֻ���������ͷ� </summary>
    property TopItems: TObjectList<THCCustomItem> read FTopItems;

    /// <summary> ���ڻ��Ƶ�����߶� </summary>
    property WindowWidth: Integer read FWindowWidth write FWindowWidth;

    /// <summary> ���ڻ��Ƶ������� </summary>
    property WindowHeight: Integer read FWindowHeight write FWindowHeight;

    /// <summary> �������� </summary>
    property ScaleX: Single read FScaleX write FScaleX;

    /// <summary> �������� </summary>
    property ScaleY: Single read FScaleY write FScaleY;

    property Zoom: Single read FZoom write FZoom;

    property DPI: Integer read FDPI write FDPI;
  end;

  THCCustomItem = class(TObject)
  strict private
    FParaNo,
    FStyleNo,
    FFirstDItemNo: Integer;
    FActive, FVisible,
    FPrintInvisible  // ��ӡʱ���ɼ�
      : Boolean;
    FOptions: TItemOptions;
    FSelectState: TItemSelectState;
    FOnDblClick: TNotifyEvent;
  protected
    function GetParaFirst: Boolean;
    procedure SetParaFirst(const Value: Boolean);
    function GetPageBreak: Boolean;
    procedure SetPageBreak(const Value: Boolean);
    function GetSelectComplate: Boolean; virtual;
    function GetSelectPart: Boolean;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    function GetHyperLink: string; virtual;
    procedure SetHyperLink(const Value: string); virtual;
    procedure SetActive(const Value: Boolean); virtual;
    function GetLength: Integer; virtual;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
  public
    constructor Create; virtual;

    procedure Assign(Source: THCCustomItem); virtual;
    /// <summary> ����Item���¼� </summary>
    /// <param name="ACanvas"></param>
    /// <param name="ADrawRect">��ǰDrawItem������</param>
    /// <param name="ADataDrawBottom">Item���ڵ�Data���λ��Ƶײ�λ��</param>
    /// <param name="ADataScreenTop"></param>
    /// <param name="ADataScreenBottom"></param>
    procedure PaintTo(const AStyle: THCStyle; const ADrawRect: TRect;
      const APageDataDrawTop, APageDataDrawBottom, APageDataScreenTop, APageDataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual; final;  // ���ɼ̳�

    procedure PaintTop(const ACanvas: TCanvas); virtual;

    /// <summary>
    /// ��2��Item�ϲ�Ϊͬһ��
    /// </summary>
    /// <param name="AItemA">ItemA</param>
    /// <param name="AItemB">ItemB</param>
    /// <returns>True�ϲ��ɹ������򷵻�False</returns>
    function CanConcatItems(const AItem: THCCustomItem): Boolean; virtual;

    procedure DisSelect; virtual;
    function CanDrag: Boolean; virtual;
    procedure KillFocus; virtual;
    procedure DblClick(const X, Y: Integer); virtual;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    function GetHint: string; virtual;
    procedure SelectComplate; virtual;
    procedure SelectPart;
    function Selected: Boolean;
    /// <summaryy ��ָ����λ���Ƿ�ɽ��ܲ��롢ɾ���Ȳ��� </summary>
    function AcceptAction(const AOffset: Integer; const ARestrain: Boolean; const AAction: THCAction): Boolean; virtual;
    /// <summary> ��ָ��λ�ý���ǰitem�ֳ�ǰ�������� </summary>
    /// <param name="AOffset">����λ��</param>
    /// <returns>��벿�ֶ�Ӧ��Item</returns>
    function BreakByOffset(const AOffset: Integer): THCCustomItem; virtual;
    procedure SaveToStream(const AStream: TStream); overload;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); overload; virtual;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); virtual;
    function ToHtml(const APath: string): string; virtual;
    procedure ToXml(const ANode: IHCXMLNode); virtual;
    procedure ParseXml(const ANode: IHCXMLNode); virtual;

    // ����������ط���
    procedure Undo(const AUndoAction: THCCustomUndoAction); virtual;
    procedure Redo(const ARedoAction: THCCustomUndoAction); virtual;
    //
    property Options: TItemOptions read FOptions;
    property Text: string read GetText write SetText;
    property Length: Integer read GetLength;
    property ParaFirst: Boolean read GetParaFirst write SetParaFirst;
    property PageBreak: Boolean read GetPageBreak write SetPageBreak;
    property HyperLink: string read GetHyperLink write SetHyperLink;

    property IsSelectComplate: Boolean read GetSelectComplate;
    property IsSelectPart: Boolean read GetSelectPart;

    property StyleNo: Integer read FStyleNo write FStyleNo;
    property ParaNo: Integer read FParaNo write FParaNo;
    property FirstDItemNo: Integer read FFirstDItemNo write FFirstDItemNo;
    property Active: Boolean read FActive write SetActive;
    property Visible: Boolean read FVisible write FVisible;
    property PrintInvisible: Boolean read FPrintInvisible write FPrintInvisible;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

  TItemNotifyEvent = procedure(const AItem: THCCustomItem) of object;

  THCItems = class(TObjectList<THCCustomItem>)
  private
    FOnInsertItem, FOnRemoveItem: TItemNotifyEvent;
  protected
    procedure Notify(const Value: THCCustomItem; Action: TCollectionNotification); override;
  public
    property OnInsertItem: TItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TItemNotifyEvent read FOnRemoveItem write FOnRemoveItem;
  end;

implementation

uses
  SysUtils;

{ THCCustomItem }

function THCCustomItem.CanDrag: Boolean;
begin
  Result := True;
end;

procedure THCCustomItem.Assign(Source: THCCustomItem);
begin
  Self.FStyleNo := Source.StyleNo;
  Self.FParaNo := Source.ParaNo;
  Self.FPrintInvisible := Source.PrintInvisible;
  Self.FOptions := Source.Options;
end;

function THCCustomItem.BreakByOffset(const AOffset: Integer): THCCustomItem;
begin
  // �̳����Լ��ж��ܷ�Break
  Result := THCCustomItemClass(Self.ClassType).Create;
  Result.Assign(Self);
  Result.ParaFirst := False;  // ��Ϻ󣬺���Ŀ϶����Ƕ���
end;

function THCCustomItem.AcceptAction(const AOffset: Integer; const ARestrain: Boolean;
  const AAction: THCAction): Boolean;
begin
  Result := True;
end;

function THCCustomItem.CanConcatItems(const AItem: THCCustomItem): Boolean;
begin
  // ������ֻ֧���ж�ԴAItem���Ƕ��ף����ж��Լ��Ƿ�Ϊ����
  Result := (Self.ClassType = AItem.ClassType)
    and (Self.FStyleNo = AItem.StyleNo)
    //and (not AItem.ParaFirst);  // ԴItem���Ƕ��ף�������Ҫ��κϲ��Ŀɼ�201804111209
end;

constructor THCCustomItem.Create;
begin
  FStyleNo := THCStyle.Null;
  FParaNo := THCStyle.Null;
  FFirstDItemNo := -1;
  FOptions := [];
  FSelectState := issNone;
  FVisible := True;
  FActive := False;
  FPrintInvisible := False;
end;

procedure THCCustomItem.DblClick(const X, Y: Integer);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure THCCustomItem.DisSelect;
begin
  FSelectState := issNone;  // �����Լ���ȫѡ������ѡ״̬
end;

procedure THCCustomItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
end;

function THCCustomItem.GetHint: string;
begin
  Result := '';
end;

function THCCustomItem.GetHyperLink: string;
begin
  Result := '';
end;

function THCCustomItem.GetLength: Integer;
begin
  Result := 0;
end;

function THCCustomItem.GetPageBreak: Boolean;
begin
  Result := ioPageBreak in FOptions;
end;

function THCCustomItem.GetParaFirst: Boolean;
begin
  Result := ioParaFirst in FOptions;
end;

function THCCustomItem.GetSelectComplate: Boolean;
begin
  Result := FSelectState = issComplate;
end;

function THCCustomItem.GetSelectPart: Boolean;
begin
  Result := FSelectState = issPart;
end;

function THCCustomItem.GetText: string;
begin
  Result := '';
end;

procedure THCCustomItem.KillFocus;
begin
end;

procedure THCCustomItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vParFirst: Boolean;
  vByte: Byte;
begin
  //AStream.ReadBuffer(FStyleNo, SizeOf(FStyleNo));  // ��TCustomData.InsertStream��������
  AStream.ReadBuffer(FParaNo, SizeOf(FParaNo));

  if AFileVersion > 25 then
    AStream.ReadBuffer(FOptions, SizeOf(FOptions))
  else
  begin
    AStream.ReadBuffer(vParFirst, SizeOf(vParFirst));
    ParaFirst := vParFirst;
  end;

  if AFileVersion > 33 then
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    FPrintInvisible := Odd(vByte shr 7);
  end;
end;

function THCCustomItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Active := True;
  Result := FActive;
end;

procedure THCCustomItem.MouseEnter;
begin
end;

procedure THCCustomItem.MouseLeave;
begin
end;

function THCCustomItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := FActive;
end;

function THCCustomItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := FActive;
end;

procedure THCCustomItem.PaintTo(const AStyle: THCStyle; const ADrawRect: TRect;
  const APageDataDrawTop, APageDataDrawBottom, APageDataScreenTop, APageDataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDCState: THCCanvas;
begin
  if APaintInfo.Print and FPrintInvisible then Exit;  // ��ӡ������ӡ

  vDCState := SaveCanvas(ACanvas);
  try
    DoPaint(AStyle, ADrawRect, APageDataDrawTop, APageDataDrawBottom,
      APageDataScreenTop, APageDataScreenBottom, ACanvas, APaintInfo);
  finally
    vDCState.ToCanvas(ACanvas);
    FreeAndNil(vDCState);
  end;
end;

procedure THCCustomItem.PaintTop(const ACanvas: TCanvas);
begin
end;

procedure THCCustomItem.ParseXml(const ANode: IHCXMLNode);
begin
  FStyleNo := ANode.Attributes['sno'];
  FParaNo := ANode.Attributes['pno'];
  Self.ParaFirst := ANode.Attributes['parafirst'];
  Self.PageBreak := ANode.Attributes['pagebreak'];
  if ANode.HasAttribute('printvisible') then
    FPrintInvisible := ANode.Attributes['printvisible']
  else
    FPrintInvisible := False;
end;

procedure THCCustomItem.Redo(const ARedoAction: THCCustomUndoAction);
begin
end;

function THCCustomItem.Selected: Boolean;
begin
  Result := FSelectState <> issNone;
end;

procedure THCCustomItem.SelectComplate;
begin
  FSelectState := issComplate;
end;

procedure THCCustomItem.SelectPart;
begin
  FSelectState := issPart;
end;

procedure THCCustomItem.SetText(const Value: string);
begin
end;

function THCCustomItem.ToHtml(const APath: string): string;
begin
  Result := '';
end;

procedure THCCustomItem.ToXml(const ANode: IHCXMLNode);
begin
  ANode.Attributes['sno'] := FStyleNo;
  ANode.Attributes['pno'] := FParaNo;
  ANode.Attributes['parafirst'] := Self.ParaFirst;
  ANode.Attributes['pagebreak'] := Self.PageBreak;
  if FPrintInvisible then
    ANode.Attributes['printvisible'] := '1';
end;

procedure THCCustomItem.Undo(const AUndoAction: THCCustomUndoAction);
begin
end;

procedure THCCustomItem.SaveToStream(const AStream: TStream);
begin
  SaveToStreamRange(AStream, 0, Self.Length);
end;

procedure THCCustomItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  AStream.WriteBuffer(FStyleNo, SizeOf(FStyleNo));
  AStream.WriteBuffer(FParaNo, SizeOf(FParaNo));
  AStream.WriteBuffer(FOptions, SizeOf(FOptions));

  vByte := 0;
  if FPrintInvisible then
    vByte := vByte or (1 shl 7);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
end;

procedure THCCustomItem.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
    FActive := Value;
end;

procedure THCCustomItem.SetHyperLink(const Value: string);
begin
end;

procedure THCCustomItem.SetPageBreak(const Value: Boolean);
begin
  if Value then
    Include(FOptions, ioPageBreak)
  else
    Exclude(FOptions, ioPageBreak);
end;

procedure THCCustomItem.SetParaFirst(const Value: Boolean);
begin
  if Value then
    Include(FOptions, ioParaFirst)
  else
    Exclude(FOptions, ioParaFirst);
end;

{ THCItems }

procedure THCItems.Notify(const Value: THCCustomItem;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      begin
        if Assigned(FOnInsertItem) then
          FOnInsertItem(Value);
      end;

    cnRemoved:
      begin
        if Assigned(FOnRemoveItem) then
          FOnRemoveItem(Value);
      end;

    cnExtracted: ;
  end;

  inherited Notify(Value, Action);
end;

{ TPaintInfo }

constructor TPaintInfo.Create;
begin
  FTopItems := TObjectList<THCCustomItem>.Create(False);  // ֻ���������ͷ�
  FScaleX := 1;
  FScaleY := 1;
  FZoom := 1;
  FViewModel := hvmFilm;
end;

destructor TPaintInfo.Destroy;
begin
  FTopItems.Free;
  inherited Destroy;
end;

procedure TPaintInfo.DrawNoScaleLine(const ACanvas: TCanvas; const AX1, AY1,
  AX2, AY2: Integer);
var
  vPt: TPoint;
begin
  SetViewportExtEx(ACanvas.Handle, FWindowWidth, FWindowHeight, @vPt);
  try
    ACanvas.MoveTo(GetScaleX(AX1), GetScaleY(AY1));
    ACanvas.LineTo(GetScaleX(AX2), GetScaleY(AY2));
  finally
    SetViewportExtEx(ACanvas.Handle, Round(FWindowWidth * FScaleX),
      Round(FWindowHeight * FScaleY), @vPt);
  end;
end;

procedure TPaintInfo.DrawNoScaleLine(const ACanvas: TCanvas;
  const APoints: array of TPoint);
var
  vPt: TPoint;
  i: Integer;
begin
  SetViewportExtEx(ACanvas.Handle, FWindowWidth, FWindowHeight, @vPt);
  try
    ACanvas.MoveTo(GetScaleX(APoints[0].X), GetScaleY(APoints[0].Y));
    for i := 1 to Length(APoints) - 1 do
      ACanvas.LineTo(GetScaleX(APoints[i].X), GetScaleY(APoints[i].Y));
  finally
    SetViewportExtEx(ACanvas.Handle, Round(FWindowWidth * FScaleX),
      Round(FWindowHeight * FScaleY), @vPt);
  end;
end;

function TPaintInfo.GetScaleX(const AValue: Integer): Integer;
begin
  Result := Round(AValue * FScaleX);
end;

function TPaintInfo.GetScaleY(const AValue: Integer): Integer;
begin
  Result := Round(AValue * FScaleY);
end;

procedure TPaintInfo.RestoreCanvasScale(const ACanvas: TCanvas;
  const AOldInfo: TScaleInfo);
begin
  if AOldInfo.MapMode = 0 then Exit;

  SetViewportOrgEx(ACanvas.Handle, AOldInfo.ViewportOrg.cx, AOldInfo.ViewportOrg.cy, nil);
  SetViewportExtEx(ACanvas.Handle, AOldInfo.ViewportExt.cx, AOldInfo.ViewportExt.cy, nil);
  SetWindowOrgEx(ACanvas.Handle, AOldInfo.WindowOrg.cx, AOldInfo.WindowOrg.cy, nil);
  SetWindowExtEx(ACanvas.Handle, AOldInfo.WindowExt.cx, AOldInfo.WindowExt.cy, nil);
  SetMapMode(ACanvas.Handle, AOldInfo.MapMode);
end;

function TPaintInfo.ScaleCanvas(const ACanvas: TCanvas): TScaleInfo;
begin
  if (FScaleX = 1) and (FScaleY = 1) then
  begin
    Result.MapMode := 0;
    Exit;
  end;

  Result.MapMode := GetMapMode(ACanvas.Handle);  // ����ӳ�䷽ʽ������ʧ��
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);  // �߼���λת���ɾ����������������ⵥλ����SetWindowsEx��SetViewportExtEx����ָ����λ���������Ҫ�ı���
  SetWindowOrgEx(ACanvas.Handle, 0, 0, @Result.WindowOrg);  // ��ָ�������������豸�����Ĵ���ԭ��
  SetWindowExtEx(ACanvas.Handle, FWindowWidth, FWindowHeight, @Result.WindowExt);  // Ϊ�豸�������ô��ڵ�ˮƽ�ĺʹ�ֱ�ķ�Χ

  SetViewportOrgEx(ACanvas.Handle, 0, 0, @Result.ViewportOrg);  // �ĸ��豸��ӳ�䵽����ԭ��(0,0)
  // ��ָ����ֵ������ָ���豸���������X�ᡢY�᷶Χ
  SetViewportExtEx(ACanvas.Handle, Round(FWindowWidth * FScaleX),
    Round(FWindowHeight * FScaleY), @Result.ViewportExt);
end;

end.
