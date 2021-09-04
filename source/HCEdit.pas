{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{                   �ĵ����ݳ��ֿؼ�                    }
{                                                       }
{*******************************************************}

unit HCEdit;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, SysUtils, Forms, IMM, HCViewData,
  HCCommon, HCScrollBar, HCStyle, HCTextStyle, HCParaStyle, HCItem, HCRectItem,
  HCUndo, HCCustomData, HCRichData, HCEditItem;

const
  HC_EDIT_EXT = '.hef';

type
  THCEdit = class(TCustomControl)
  private
    FStyle: THCStyle;
    FData: THCViewData;
    FDataBmp: TBitmap;  // ������ʾλͼ
    FUndoList: THCUndoList;
    FCaret: THCCaret;
    FHScrollBar: THCScrollBar;
    FVScrollBar: THCScrollBar;
    FUpdateCount: Integer;
    FChanged: Boolean;
    FOnChange: TNotifyEvent;
    FOnCaretChange: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnCreateStyleItem: TStyleItemEvent;
    FOnInsertItem, FOnRemoveItem: TDataItemEvent;
    //
    function GetViewWidth: Integer;
    function GetViewHeight: Integer;

    function GetCurStyleNo: Integer;
    function GetCurParaNo: Integer;

    /// <summary> ���»�ȡ���λ�� </summary>
    procedure ReBuildCaret(const AScrollBar: Boolean = False);

    /// <summary> �Ƿ��ɹ�����λ�ñ仯����ĸ��� </summary>
    procedure CheckUpdateInfo(const AScrollBar: Boolean = False);
    procedure DoVScrollChange(Sender: TObject; ScrollCode: TScrollCode;
      const ScrollPos: Integer);

    /// <summary> �ĵ�"����"�䶯(�����ޱ仯����ԳƱ߾࣬������ͼ) </summary>
    procedure DoMapChanged;
    procedure DoCaretChange;
    procedure DoDataCheckUpdateInfo;
    procedure DoChange;
    procedure CalcScrollRang;
    // Imm
    procedure UpdateImmPosition;

    /// <summary> ɾ����ʹ�õ��ı���ʽ </summary>
    procedure _DeleteUnUsedStyle;
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
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
    procedure DoDrawItemPaintBefor(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
    function DoDataCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem; virtual;
    procedure DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem); virtual;
    procedure DoDataRemoveItem(const AData: THCCustomData; const AItem: THCCustomItem); virtual;
    procedure DataSaveLiteStream(const AStream: TStream; const AProc: THCProcedure);
    procedure DataLoadLiteStream(const AStream: TStream; const AProc: THCLoadProc);
    /// <summary> ����ǰ�����ڿ����Ƿ������� </summary>
    function DoCopyRequest(const AFormat: Word): Boolean; virtual;
    /// <summary> ճ��ǰ�����ڿ����Ƿ�����ճ�� </summary>
    function DoPasteRequest(const AFormat: Word): Boolean; virtual;
    /// <summary> ����ǰ�����ڶ�������������������Դ </summary>
    procedure DoCopyAsStream(const AStream: TStream); virtual;
    /// <summary> ճ��ǰ������ȷ�϶�������������������Դ </summary>
    function DoPasteFromStream(const AStream: TStream): Boolean; virtual;
    // ��Ϣ
    /// <summary> ��ӦTab���ͷ���� </summary>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;

    // �������뷨���������
    procedure WMImeComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WndProc(var Message: TMessage); override;
    //
    procedure Cut;
    procedure Copy;
    procedure Paste;
    //
    function DataChangeByAction(const AFun: THCFunction): Boolean;

    function DoGetUndoList: THCUndoList;
    function DoUndoNew: THCUndo;
    function DoUndoGroupBegin(const AItemNo, AOffset: Integer): THCUndoGroupBegin;
    function DoUndoGroupEnd(const AItemNo, AOffset: Integer): THCUndoGroupEnd;
    procedure DoUndo(const Sender: THCUndo);
    procedure DoRedo(const Sender: THCUndo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);
    procedure ApplyParaBackColor(const AColor: TColor);
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode; const ASpace: Single = 1);
    procedure ApplyTextStyle(const AFontStyle: THCFontStyle);
    procedure ApplyTextFontName(const AFontName: TFontName);
    procedure ApplyTextFontSize(const AFontSize: Single);
    procedure ApplyTextColor(const AColor: TColor);
    procedure ApplyTextBackColor(const AColor: TColor);
    function InsertText(const AText: string): Boolean;
    function InsertItem(const AItem: THCCustomItem): Boolean; overload;
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem): Boolean; overload;
    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
    /// <summary> ����ָ�����еı�� </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
    /// <summary> ��ȡ����Data </summary>
    function TopLevelData: THCCustomData;

    /// <summary> ���õ�ǰTextItem���ı����� </summary>
    procedure SetActiveItemText(const AText: string);

    /// <summary> ȫѡ </summary>
    procedure SelectAll;
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream);
    procedure Clear;
    /// <summary> ���� </summary>
    procedure Undo;
    /// <summary> ���� </summary>
    procedure Redo;
    /// <summary> ��ʼһ�鳷������ </summary>
    procedure UndoGroupBegin;
    /// <summary> ����һ�鳷������ </summary>
    procedure UndoGroupEnd;

    procedure UpdateView;
    procedure BeginUpdate;
    procedure EndUpdate;

    /// <summary> ����Lite�� </summary>
    function InsertLiteStream(const AStream: TStream): Boolean;

    /// <summary> ��ǰ��괦���ı���ʽ </summary>
    property CurStyleNo: Integer read GetCurStyleNo;
    /// <summary> ��ǰ��괦�Ķ���ʽ </summary>
    property CurParaNo: Integer read GetCurParaNo;

    property Data: THCViewData read FData;
    property Style: THCStyle read FStyle;
    property Changed: Boolean read FChanged write FChanged;
  published
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Clipbrd;

{ THCEdit }

procedure THCEdit.ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
begin
  FData.ApplyParaAlignHorz(AAlign);
  CheckUpdateInfo;
end;

procedure THCEdit.ApplyParaAlignVert(const AAlign: TParaAlignVert);
begin
  FData.ApplyParaAlignVert(AAlign);
  CheckUpdateInfo;
end;

procedure THCEdit.ApplyParaBackColor(const AColor: TColor);
begin
  FData.ApplyParaBackColor(AColor);
  CheckUpdateInfo;
end;

procedure THCEdit.ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode;
  const ASpace: Single = 1);
begin
  FData.ApplyParaLineSpace(ASpaceMode, ASpace);
  CheckUpdateInfo;
end;

procedure THCEdit.ApplyTextBackColor(const AColor: TColor);
begin
  FData.ApplyTextBackColor(AColor);
  CheckUpdateInfo;
end;

procedure THCEdit.ApplyTextColor(const AColor: TColor);
begin
  FData.ApplyTextColor(AColor);
  CheckUpdateInfo;
end;

procedure THCEdit.ApplyTextFontName(const AFontName: TFontName);
begin
  FData.ApplyTextFontName(AFontName);
  CheckUpdateInfo;
end;

procedure THCEdit.ApplyTextFontSize(const AFontSize: Single);
begin
  FData.ApplyTextFontSize(AFontSize);
  CheckUpdateInfo;
end;

procedure THCEdit.ApplyTextStyle(const AFontStyle: THCFontStyle);
begin
  FData.ApplyTextStyle(AFontStyle);
  CheckUpdateInfo;
end;

procedure THCEdit.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure THCEdit.CalcScrollRang;
begin
  FHScrollBar.Max := Self.Padding.Left + Self.Padding.Right;
  FVScrollBar.Max := FData.Height + Self.Padding.Top + Self.Padding.Bottom;
end;

procedure THCEdit.CheckUpdateInfo(const AScrollBar: Boolean = False);
begin
  if (FCaret <> nil) and FStyle.UpdateInfo.ReCaret then
  begin
    FStyle.UpdateInfo.ReCaret := False;
    FStyle.UpdateInfo.ReStyle := False;
    FStyle.UpdateInfo.ReScroll := False;
    ReBuildCaret(AScrollBar);
    UpdateImmPosition;
  end;

  if FStyle.UpdateInfo.RePaint then
  begin
    FStyle.UpdateInfo.RePaint := False;
    UpdateView;
  end;
end;

procedure THCEdit.Clear;
begin
  FData.Clear;
end;

procedure THCEdit.Copy;
var
  vStream: TMemoryStream;
  vMem: Cardinal;
  vPtr: Pointer;
begin
  if FData.SelectExists then
  begin
    vStream := TMemoryStream.Create;
    try
      DoCopyAsStream(vStream);  // ֪ͨ�����¼������ڼ�����

      DataSaveLiteStream(vStream, procedure()
      begin
        Self.FData.GetTopLevelData.SaveSelectToStream(vStream);
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

    Clipboard.Clear;
    Clipboard.Open;
    try
      if DoCopyRequest(HC_FILEFORMAT) then
        Clipboard.SetAsHandle(HC_FILEFORMAT, vMem);

      if DoCopyRequest(CF_UNICODETEXT) then
          Clipboard.AsText := FData.SaveSelectToText;  // �ı���ʽ
    finally
      Clipboard.Close;
    end;
  end;
end;

constructor THCEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //
  FStyle := THCStyle.CreateEx(True, True);

  FUndoList := THCUndoList.Create;
  FUndoList.OnUndo := DoUndo;
  FUndoList.OnRedo := DoRedo;
  FUndoList.OnUndoNew := DoUndoNew;
  FUndoList.OnUndoGroupStart := DoUndoGroupBegin;
  FUndoList.OnUndoGroupEnd := DoUndoGroupEnd;

  FData := THCViewData.Create(FStyle);
  FData.Width := 200;
  FData.OnGetUndoList := DoGetUndoList;
  FData.OnCreateItemByStyle := DoDataCreateStyleItem;
  FData.OnDrawItemPaintBefor := DoDrawItemPaintBefor;
  FData.OnInsertItem := DoDataInsertItem;
  FData.OnRemoveItem := DoDataRemoveItem;

  FDataBmp := TBitmap.Create;

  // ��ֱ����������Χ��Resize������
  FVScrollBar := THCScrollBar.Create(Self);
  FVScrollBar.Parent := Self;
  FVScrollBar.Orientation := TOrientation.oriVertical;
  FVScrollBar.OnScroll := DoVScrollChange;
  // ˮƽ����������Χ��Resize������
  FHScrollBar := THCScrollBar.Create(Self);
  FHScrollBar.Parent := Self;
  FHScrollBar.Orientation := TOrientation.oriHorizontal;
  FHScrollBar.OnScroll := DoVScrollChange;

  FChanged := False;
end;

procedure THCEdit.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FCaret) then  // ��ֹ�л�Parentʱ��δ���
      FreeAndNil(FCaret);

    FCaret := THCCaret.Create(Handle);
  end;
end;

procedure THCEdit.Cut;
begin
  Copy;
  FData.DeleteSelected;
  CheckUpdateInfo;
end;

function THCEdit.DataChangeByAction(const AFun: THCFunction): Boolean;
//var
//  vHeight, vCruItemNo: Integer;
begin
  //vHeight := FData.Height;
  //vCruItemNo := FData.GetCurItemNo;
  Result := AFun;
  DoChange;
end;

procedure THCEdit.DataLoadLiteStream(const AStream: TStream; const AProc: THCLoadProc);
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

procedure THCEdit.DataSaveLiteStream(const AStream: TStream; const AProc: THCProcedure);
begin
  _SaveFileFormatAndVersion(AStream);  // �ļ���ʽ�Ͱ汾
  //_DeleteUnUsedStyle;  // ɾ����ʹ�õ���ʽ
  FStyle.SaveToStream(AStream);
  AProc;
end;

destructor THCEdit.Destroy;
begin
  FData.Free;
  FCaret.Free;
  FHScrollBar.Free;
  FVScrollBar.Free;
  FDataBmp.Free;
  FreeAndNil(FStyle);
  FUndoList.Free;
  inherited Destroy;
end;

procedure THCEdit.DoCaretChange;
begin
  if Assigned(FOnCaretChange) then
    FOnCaretChange(Self);
end;

procedure THCEdit.DoChange;
begin
  FChanged := True;
  DoMapChanged;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THCEdit.DoCopyAsStream(const AStream: TStream);
begin
end;

function THCEdit.DoCopyRequest(const AFormat: Word): Boolean;
var
  vTopItem: THCCustomItem;
begin
  vTopItem := FData.GetTopLevelItem;
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

procedure THCEdit.DoMapChanged;
begin
  if FUpdateCount = 0 then
  begin
    CalcScrollRang;
    CheckUpdateInfo;
  end;
end;

function THCEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if not (ssCtrl in Shift) then
    FVScrollBar.Position := FVScrollBar.Position - WheelDelta div 1
  else
    FHScrollBar.Position := FHScrollBar.Position - WheelDelta div 1;

  Result := True;
end;

function THCEdit.DoPasteFromStream(const AStream: TStream): Boolean;
begin
  Result := True;
end;

function THCEdit.DoPasteRequest(const AFormat: Word): Boolean;
var
  vTopItem: THCCustomItem;
begin
  vTopItem := FData.GetTopLevelItem;
  if vTopItem is THCEditItem then
  begin
    Result := (AFormat = CF_TEXT) or (AFormat = CF_UNICODETEXT);
    Exit;
  end;

  Result := True;
end;

procedure THCEdit.DoRedo(const Sender: THCUndo);
var
  vUndoList: THCUndoList;
begin
  if Sender is THCEditUndo then
  begin
    FHScrollBar.Position := (Sender as THCEditUndo).HScrollPos;
    FVScrollBar.Position := (Sender as THCEditUndo).VScrollPos;
  end
  else
  if Sender is THCUndoEditGroupEnd then
  begin
    FHScrollBar.Position := (Sender as THCUndoEditGroupEnd).HScrollPos;
    FVScrollBar.Position := (Sender as THCUndoEditGroupEnd).VScrollPos;
  end;

  vUndoList := DoGetUndoList;
  //if vUndoList.Enable then  // �����жϣ���Ϊ�����ָ����̻����Σ���ֹ�����µĳ����ָ�
  if not vUndoList.GroupWorking then  // �������д���ʱ����������Data����Ӧ�䶯
  begin
    DataChangeByAction(function(): Boolean
      begin
        FData.Redo(Sender);
      end);
  end
  else
    FData.Redo(Sender);
end;

procedure THCEdit.DoUndo(const Sender: THCUndo);
var
  vUndoList: THCUndoList;
begin
  if Sender is THCEditUndo then
  begin
    FHScrollBar.Position := (Sender as THCEditUndo).HScrollPos;
    FVScrollBar.Position := (Sender as THCEditUndo).VScrollPos;
  end
  else
  if Sender is THCUndoEditGroupBegin then
  begin
    FHScrollBar.Position := (Sender as THCUndoEditGroupBegin).HScrollPos;
    FVScrollBar.Position := (Sender as THCUndoEditGroupBegin).VScrollPos;
  end;

  vUndoList := DoGetUndoList;
  //if vUndoList.Enable then  // �����жϣ���Ϊ�����ָ����̻����Σ���ֹ�����µĳ����ָ�
  if not vUndoList.GroupWorking then  // �������д���ʱ����������Data����Ӧ�䶯
  begin
    DataChangeByAction(function(): Boolean
      begin
        FData.Undo(Sender);
      end);
  end
  else
    FData.Undo(Sender);
end;

function THCEdit.DoUndoGroupBegin(const AItemNo,
  AOffset: Integer): THCUndoGroupBegin;
begin
  Result := THCUndoEditGroupBegin.Create;
  (Result as THCUndoEditGroupBegin).HScrollPos := FHScrollBar.Position;
  (Result as THCUndoEditGroupBegin).VScrollPos := FVScrollBar.Position;
  Result.Data := FData;
  Result.CaretDrawItemNo := FData.CaretDrawItemNo;
end;

function THCEdit.DoUndoGroupEnd(const AItemNo,
  AOffset: Integer): THCUndoGroupEnd;
begin
  Result := THCUndoEditGroupEnd.Create;
  (Result as THCUndoEditGroupEnd).HScrollPos := FHScrollBar.Position;
  (Result as THCUndoEditGroupEnd).VScrollPos := FVScrollBar.Position;
  Result.Data := FData;
  Result.CaretDrawItemNo := FData.CaretDrawItemNo;
end;

function THCEdit.DoUndoNew: THCUndo;
begin
  Result := THCEditUndo.Create;
  (Result as THCEditUndo).HScrollPos := FHScrollBar.Position;
  (Result as THCEditUndo).VScrollPos := FVScrollBar.Position;
  Result.Data := FData;
end;

procedure THCEdit.DoDataCheckUpdateInfo;
begin
  if FUpdateCount = 0 then
    CheckUpdateInfo;
end;

function THCEdit.DoDataCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  if Assigned(FOnCreateStyleItem) then
    Result := FOnCreateStyleItem(AData, AStyleNo)
  else
    Result := nil;
end;

procedure THCEdit.DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnInsertItem) then
    FOnInsertItem(AData, AItem);
end;

procedure THCEdit.DoDataRemoveItem(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnRemoveItem) then
    FOnRemoveItem(AData, AItem);
end;

procedure THCEdit.DoDrawItemPaintBefor(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
end;

function THCEdit.DoGetUndoList: THCUndoList;
begin
  Result := FUndoList;
end;

procedure THCEdit.DoVScrollChange(Sender: TObject; ScrollCode: TScrollCode;
  const ScrollPos: Integer);
begin
  FStyle.UpdateInfoRePaint;
  FStyle.UpdateInfoReCaret(False);
  CheckUpdateInfo(True);
//  if Assigned(FOnVerScroll) then
//    FOnVerScroll(Self);
end;

procedure THCEdit.EndUpdate;
begin
  Dec(FUpdateCount);
  DoMapChanged;
end;

function THCEdit.GetCurParaNo: Integer;
begin
  Result := FData.GetTopLevelData.CurStyleNo;
end;

function THCEdit.GetCurStyleNo: Integer;
begin
  Result := FData.GetTopLevelData.CurParaNo;
end;

function THCEdit.GetViewHeight: Integer;
begin
  if FHScrollBar.Visible then
    Result := Height - FHScrollBar.Height
  else
    Result := Height;
end;

function THCEdit.GetViewWidth: Integer;
begin
  if FVScrollBar.Visible then
    Result := Width - FVScrollBar.Width
  else
    Result := Width;
end;

function THCEdit.InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
begin
  Result := DataChangeByAction(function(): Boolean
    begin
      Result := FData.InsertDomain(AMouldDomain);
    end);
end;

function THCEdit.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem): Boolean;
begin
  Result := DataChangeByAction(function(): Boolean
    begin
      Result := FData.InsertItem(AIndex, AItem);
    end);
end;

function THCEdit.InsertLiteStream(const AStream: TStream): Boolean;
var
  vResult: Boolean;
begin
  Result := False;
  DataLoadLiteStream(AStream, procedure(const AFileVersion: Word; const AStyle: THCStyle)
  begin
    Self.BeginUpdate;
    try
      vResult := FData.InsertStream(AStream, AStyle, AFileVersion);
    finally
      Self.EndUpdate;
    end;
  end);
  Result := vResult;
end;

function THCEdit.InsertTable(const ARowCount, AColCount: Integer): Boolean;
begin
  Result := DataChangeByAction(function(): Boolean
    var
      vTopData: THCRichData;
    begin
      vTopData := FData.GetTopLevelData as THCRichData;
      vTopData.InsertTable(ARowCount, AColCount);
    end);
end;

function THCEdit.InsertText(const AText: string): Boolean;
begin
  Result := DataChangeByAction(function(): Boolean
    begin
      Result := FData.InsertText(AText);
    end);
end;

function THCEdit.InsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := DataChangeByAction(function(): Boolean
    begin
      Result := FData.InsertItem(AItem);
    end);
end;

procedure THCEdit.KeyDown(var Key: Word; Shift: TShiftState);

  {$REGION '��ݼ�'}
  function IsCopyShortKey(Key: Word; Shift: TShiftState): Boolean;
  begin
    Result := (ssCtrl in Shift) and (Key = ord('C')) and not (ssAlt in Shift);
  end;

  function IsCutShortKey(Key: Word; Shift: TShiftState): Boolean;
  begin
    Result := (ssCtrl in Shift) and (Key = ord('X')) and not (ssAlt in Shift);
  end;

  function IsPasteShortKey(Key: Word; Shift: TShiftState): Boolean;
  begin
    Result := (ssCtrl in Shift) and (Key = ord('V')) and not (ssAlt in Shift);
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
  inherited;
  if IsCopyShortKey(Key, Shift) then
    Self.Copy
  else
  if IsCutShortKey(Key, Shift) then
    Self.Cut
  else
  if IsPasteShortKey(Key, Shift) then
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
  begin
    FData.KeyDown(Key, Shift);
    if IsKeyDownEdit(Key) then
      DoChange
    else
    if IsDirectionKey(Key) then
      DoDataCheckUpdateInfo;
  end;

  CheckUpdateInfo;
end;

procedure THCEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if IsKeyPressWant(Key) then
  begin
    FData.KeyPress(Key);
    DoChange;
    CheckUpdateInfo;
  end;
end;

procedure THCEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  FData.KeyUp(Key, Shift);
end;

procedure THCEdit.LoadFromFile(const AFileName: string);
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

procedure THCEdit.LoadFromStream(const AStream: TStream);
var
  vFileExt: string;
  viVersion: Word;
  vLang: Byte;
begin
  Self.BeginUpdate;
  try
    // ��������ָ�����
    FUndoList.Clear;
    FUndoList.SaveState;
    try
      FUndoList.Enable := False;

      FData.Clear;
      FStyle.Initialize;
      AStream.Position := 0;
      _LoadFileFormatAndVersion(AStream, vFileExt, viVersion, vLang);  // �ļ���ʽ�Ͱ汾
      if vFileExt <> HC_EXT then
        raise Exception.Create('����ʧ�ܣ�����' + HC_EXT + '�ļ���');

      FStyle.LoadFromStream(AStream, viVersion);  // ������ʽ��
      FData.LoadFromStream(AStream, FStyle, viVersion);
      DoMapChanged;
    finally
      FUndoList.RestoreState;
    end;
  finally
    Self.EndUpdate;
  end;
end;

procedure THCEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FData.MouseDown(Button, Shift, X - Self.Padding.Left + FHScrollBar.Position,
    Y - Self.Padding.Top + FVScrollBar.Position);

  CheckUpdateInfo;  // ����ꡢ�л�����Item
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure THCEdit.MouseMove(Shift: TShiftState; X, Y: Integer);

  {$REGION 'ProcessHint'}
  procedure ProcessHint;
  var
    vHint: string;
  begin
    vHint := FData.GetHint;
    if vHint <> Hint then
    begin
      Hint := vHint;
      //Application.CancelHint;
    end
  end;
  {$ENDREGION}

begin
  inherited;
  GCursor := crIBeam;
  FData.MouseMove(Shift, X - Self.Padding.Left + FHScrollBar.Position,
    Y - Self.Padding.Top + FVScrollBar.Position);

  if ShowHint then
    ProcessHint;

  if FStyle.UpdateInfo.DragingSelected then
    Screen.Cursor := crDrag
  else
    Cursor := GCursor;

  CheckUpdateInfo;  // ���������
end;

procedure THCEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button = mbRight then Exit;  // �Ҽ������˵�
  FData.MouseUp(Button, Shift, X - Self.Padding.Left + FHScrollBar.Position,
    Y - Self.Padding.Top + FVScrollBar.Position);

  if FStyle.UpdateInfo.DragingSelected then
    Screen.Cursor := crDefault;

  Cursor := GCursor;

  CheckUpdateInfo;  // ��ѡ�������а��²��ƶ��������ʱ��Ҫ����

  FStyle.UpdateInfo.Selecting := False;
  FStyle.UpdateInfo.DragingSelected := False;
end;

procedure THCEdit.Paint;
begin
  BitBlt(Canvas.Handle, 0, 0, GetViewWidth, GetViewHeight,
    FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure THCEdit.Paste;
var
  vStream: TMemoryStream;
  vMem: Cardinal;
  vPtr: Pointer;
  vSize: Integer;
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
      if not DoPasteFromStream(vStream) then Exit;
        InsertLiteStream(vStream);
    finally
      vStream.Free;
    end;
  end
  else
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    Self.BeginUpdate;
    try
      FData.InsertText(Clipboard.AsText);
    finally
      Self.EndUpdate;
    end;
  end;
end;

procedure THCEdit.ReBuildCaret(const AScrollBar: Boolean = False);
var
  vCaretInfo: THCCaretInfo;
  vViewHeight, vViewWidth: Integer;
begin
  if FCaret = nil then Exit;

  if (not Self.Focused) or ((not FStyle.UpdateInfo.DragingSelected) and FData.SelectExists) then
  begin
    FCaret.Hide;
    Exit;
  end;

  { ��ʼ�������Ϣ��Ϊ�����������������ֻ�ܷ������� }
  vCaretInfo.X := 0;
  vCaretInfo.Y := 0;
  vCaretInfo.Height := 0;
  vCaretInfo.Visible := True;

  FData.GetCaretInfo(FData.SelectInfo.StartItemNo, FData.SelectInfo.StartItemOffset, vCaretInfo);

  if not vCaretInfo.Visible then
  begin
    FCaret.Hide;
    Exit;
  end;

  FCaret.X := vCaretInfo.X - FHScrollBar.Position + Self.Padding.Left;
  FCaret.Y := vCaretInfo.Y - FVScrollBar.Position + Self.Padding.Top;
  FCaret.Height := vCaretInfo.Height;

  vViewHeight := GetViewHeight;
  vViewWidth := GetViewWidth;
  if not FStyle.UpdateInfo.ReScroll then // ������ƽ������ʱ�����ܽ������������
  begin
    if (FCaret.X < 0) or (FCaret.X > vViewWidth) then
    begin
      FCaret.Hide;
      Exit;
    end;

    if (FCaret.Y + FCaret.Height < 0) or (FCaret.Y > vViewHeight) then
    begin
      FCaret.Hide;
      Exit;
    end;
  end
  else  // �ǹ�����(������������)����Ĺ��λ�ñ仯
  begin
    if FCaret.Height < vViewHeight then
    begin
      if not FCaret.VScroll then
      begin
        FCaret.VScroll := True;
        try
          if FCaret.Y < 0 then
            FVScrollBar.Position := FVScrollBar.Position + FCaret.Y - Self.Padding.Top
          else
          if FCaret.Y + FCaret.Height + Self.Padding.Top > vViewHeight then
            FVScrollBar.Position := FVScrollBar.Position + FCaret.Y + FCaret.Height + Self.Padding.Top - vViewHeight;
        finally
          FCaret.VScroll := False;
        end;
      end;

      if not FCaret.HScroll then
      begin
        FCaret.HScroll := True;
        try
          if FCaret.X < 0 then
            FHScrollBar.Position := FHScrollBar.Position + FCaret.X - Self.Padding.Left
          else
          if FCaret.X + Self.Padding.Left > vViewWidth then
            FHScrollBar.Position := FHScrollBar.Position + FCaret.X + Self.Padding.Left - vViewWidth;
        finally
          FCaret.HScroll := False;
        end;
      end;
    end;
  end;

  if FCaret.VScroll or FCaret.HScroll then Exit;

  if FCaret.Y + FCaret.Height > vViewHeight then
    FCaret.Height := vViewHeight - FCaret.Y;

  FCaret.Show;
  DoCaretChange;
end;

procedure THCEdit.Redo;
begin
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
end;

procedure THCEdit.Resize;
begin
  inherited;
  FDataBmp.SetSize(GetViewWidth, GetViewHeight);
  FData.Width := FDataBmp.Width - Self.Padding.Left - Self.Padding.Right;
  FData.ReFormat;
  FStyle.UpdateInfoRePaint;
  if FCaret <> nil then
    FStyle.UpdateInfoReCaret(False);

  DoMapChanged;
end;

procedure THCEdit.SaveToFile(const AFileName: string);
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

procedure THCEdit.SaveToStream(const AStream: TStream);
begin
  _SaveFileFormatAndVersion(AStream);  // �ļ���ʽ�Ͱ汾
  _DeleteUnUsedStyle;  // ɾ����ʹ�õ���ʽ(�ɷ��Ϊ�����õĴ��ˣ�����ʱItem��StyleNoȡ����)
  FStyle.SaveToStream(AStream);
  FData.SaveToStream(AStream);
end;

procedure THCEdit.SelectAll;
begin
  FData.SelectAll;

  FStyle.UpdateInfoRePaint;
  CheckUpdateInfo;
end;

procedure THCEdit.SetActiveItemText(const AText: string);
begin
  FData.SetActiveItemText(AText);
  CheckUpdateInfo;
end;

procedure THCEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
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

function THCEdit.TopLevelData: THCCustomData;
begin
  Result := FData.GetTopLevelData;
end;

procedure THCEdit.Undo;
begin
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
end;

procedure THCEdit.UndoGroupBegin;
begin
  if FUndoList.Enable then
    FUndoList.UndoGroupBegin(FData.SelectInfo.StartItemNo, FData.SelectInfo.StartItemOffset);
end;

procedure THCEdit.UndoGroupEnd;
begin
  if FUndoList.Enable then
    FUndoList.UndoGroupEnd(FData.SelectInfo.StartItemNo, FData.SelectInfo.StartItemOffset);
end;

procedure THCEdit.UpdateView;
var
  i, vViewWidth, vViewHeight: Integer;
  vPaintInfo: TPaintInfo;
begin
  if FUpdateCount = 0 then
  begin
    FDataBmp.Canvas.Lock;
    try
      // �ؼ�����
      FDataBmp.Canvas.Brush.Color := clWhite;// $00E7BE9F;
      FDataBmp.Canvas.FillRect(Rect(0, 0, FDataBmp.Width, FDataBmp.Height));
      //
      vViewWidth := GetViewWidth;
      vViewHeight := GetViewHeight;

      vPaintInfo := TPaintInfo.Create;
      try
        FData.PaintData(Self.Padding.Left - FHScrollBar.Position,  // ��ǰҳ����Ҫ���Ƶ���Left
          Self.Padding.Top,     // ��ǰҳ����Ҫ���Ƶ���Top
          Self.Width - FHScrollBar.Position - Self.Padding.Right,
          Self.Padding.Top + FData.Height,  // ��ǰҳ����Ҫ���Ƶ�Bottom
          Self.Padding.Top,     // ������ֵ�ǰҳ���ݵ�Topλ��
          Self.Height - FHScrollBar.Height,  // ������ֵ�ǰҳ����Bottomλ��
          FVScrollBar.Position,  // ָ�����ĸ�λ�ÿ�ʼ�����ݻ��Ƶ�ҳ������ʼλ��
          FDataBmp.Canvas,
          vPaintInfo);

        for i := 0 to vPaintInfo.TopItems.Count - 1 do
          vPaintInfo.TopItems[i].PaintTop(FDataBmp.Canvas);
      finally
        vPaintInfo.Free;
      end;

      BitBlt(Canvas.Handle, 0, 0, vViewWidth, vViewHeight, FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
      InvalidateRect(Handle, ClientRect, False);  // ֪ͨEditֻ���±䶯���򣬷�ֹ��˸�����BitBlt�����������
    finally
      FDataBmp.Canvas.Unlock;
    end;
  end;
end;

procedure THCEdit.UpdateImmPosition;
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
    // �������뷨��ǰ��괦������Ϣ
    ImmGetCompositionFont(vhIMC, @vLogFont);
    vLogFont.lfHeight := 22;
    ImmSetCompositionFont(vhIMC, @vLogFont);
    // �������뷨��ǰ���λ����Ϣ
    vCF.ptCurrentPos := Point(FCaret.X, FCaret.Y + 5);  // ���뷨��������λ��
    vCF.dwStyle := CFS_RECT;
    vCF.rcArea  := ClientRect;
    ImmSetCompositionWindow(vhIMC, @vCF);
  finally
    ImmReleaseContext(Handle, vhIMC);
  end;
end;

procedure THCEdit.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure THCEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;

procedure THCEdit.WMImeComposition(var Message: TMessage);
var
  vhIMC: HIMC;
  vSize: Integer;
  vBuffer: TBytes;
  vS: string;
begin
  if (Message.LParam and GCS_RESULTSTR) <> 0 then  // ֪ͨ��������������ַ���
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
          SetLength(vBuffer, vSize);  // vSize - 2
          vS := WideStringOf(vBuffer);
          if vS <> '' then
          begin
            FData.InsertText(vS);
            FStyle.UpdateInfoRePaint;
            FStyle.UpdateInfoReCaret;
            CheckUpdateInfo;
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

procedure THCEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if Message.FocusedWnd <> Self.Handle then
    FCaret.Hide;
end;

procedure THCEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
end;

procedure THCEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  // ����������ͨ���������޸Ĺ�괦�ֺź���������ý��㵽HCView�������ֹͣ��ȡ��ʽ��
  // ����Ե�ǰ���ǰ�ı���ʽ��Ϊ��ǰ��ʽ�ˣ�����ǽ���ʧȥ��������µ����ȡ����
  // ���ȴ�������ٴ���MouseDown����MouseDown�������ȡ��ǰ��ʽ����Ӱ��
  FStyle.UpdateInfoReCaret(False);
  FStyle.UpdateInfoRePaint;
  //FStyle.UpdateInfoReScroll;  // ʧȥ����ǰ��겻����ʾҳ����ȡ�����Ҫ����
  CheckUpdateInfo;
end;

procedure THCEdit.WndProc(var Message: TMessage);
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
  end;
  inherited WndProc(Message);
end;

procedure THCEdit._DeleteUnUsedStyle;
var
  i, vUnCount: Integer;
begin
  FStyle.TextStyles[0].CheckSaveUsed := True;
  FStyle.TextStyles[0].TempNo := 0;
  for i := 1 to FStyle.TextStyles.Count - 1 do
  begin
    FStyle.TextStyles[i].CheckSaveUsed := False;
    FStyle.TextStyles[i].TempNo := THCStyle.Null;
  end;
  for i := 0 to FStyle.ParaStyles.Count - 1 do
  begin
    FStyle.ParaStyles[i].CheckSaveUsed := False;
    FStyle.ParaStyles[i].TempNo := THCStyle.Null;
  end;

  FData.MarkStyleUsed(True);

  vUnCount := 0;
  for i := 1 to FStyle.TextStyles.Count - 1 do
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

  FData.MarkStyleUsed(False);

  for i := FStyle.TextStyles.Count - 1 downto 1 do
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

end.
