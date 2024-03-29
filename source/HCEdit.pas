{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                   文档内容呈现控件                    }
{                                                       }
{*******************************************************}

unit HCEdit;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, SysUtils, Forms, IMM, HCViewData,
  HCCommon, HCScrollBar, HCStyle, HCTextStyle, HCParaStyle, HCItem, HCRectItem,
  HCUndo, HCCustomData, HCRichData, HCEditItem, HCFormatData;

const
  HC_EDIT_EXT = '.hef';

type
  THCEdit = class(TCustomControl)
  private
    FStyle: THCStyle;
    FData: THCViewData;
    FDataBmp: TBitmap;  // 数据显示位图
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

    /// <summary> 重新获取光标位置 </summary>
    procedure ReBuildCaret(const AScrollBar: Boolean = False);

    /// <summary> 是否由滚动条位置变化引起的更新 </summary>
    procedure CheckUpdateInfo(const AScrollBar: Boolean = False);
    procedure DoVScrollChange(Sender: TObject; ScrollCode: TScrollCode;
      const ScrollPos: Integer);

    /// <summary> 文档"背板"变动(数据无变化，如对称边距，缩放视图) </summary>
    procedure DoMapChanged;
    procedure DoCaretChange;
    procedure DoDataCheckUpdateInfo;
    procedure DoChange;
    procedure CalcScrollRang;
    // Imm
    procedure UpdateImmPosition;

    /// <summary> 删除不使用的文本样式 </summary>
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
    procedure DoDataItemReFormatRequest(const AData: THCCustomData; const AItem: THCCustomItem);
    function DoDataGetScreenCoord(const X, Y: Integer): TPoint;
    procedure DataSaveLiteStream(const AStream: TStream; const AProc: THCProcedure);
    procedure DataLoadLiteStream(const AStream: TStream; const AProc: THCLoadProc);
    /// <summary> 复制前，便于控制是否允许复制 </summary>
    function DoCopyRequest(const AFormat: Word): Boolean; virtual;
    /// <summary> 粘贴前，便于控制是否允许粘贴 </summary>
    function DoPasteRequest(const AFormat: Word): Boolean; virtual;
    /// <summary> 复制前，便于订制特征数据如内容来源 </summary>
    procedure DoCopyAsStream(const AStream: TStream); virtual;
    /// <summary> 粘贴前，便于确认订制特征数据如内容来源 </summary>
    function DoPasteFromStream(const AStream: TStream): Boolean; virtual;
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
    /// <summary> 插入指定行列的表格 </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
    /// <summary> 获取顶层Data </summary>
    function TopLevelData: THCCustomData;

    /// <summary> 设置当前TextItem的文本内容 </summary>
    procedure SetActiveItemText(const AText: string);

    /// <summary> 全选 </summary>
    procedure SelectAll;
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream);
    procedure Clear;
    /// <summary> 撤销 </summary>
    procedure Undo;
    /// <summary> 重做 </summary>
    procedure Redo;
    /// <summary> 开始一组撤销操作 </summary>
    procedure UndoGroupBegin;
    /// <summary> 结束一组撤销操作 </summary>
    procedure UndoGroupEnd;

    procedure UpdateView;
    procedure BeginUpdate;
    procedure EndUpdate;

    /// <summary> 插入Lite流 </summary>
    function InsertLiteStream(const AStream: TStream): Boolean;

    /// <summary> 当前光标处的文本样式 </summary>
    property CurStyleNo: Integer read GetCurStyleNo;
    /// <summary> 当前光标处的段样式 </summary>
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
  if FUpdateCount > 0 then
    Exit;

  if (FCaret <> nil) and FStyle.UpdateInfo.ReCaret then
  begin
    ReBuildCaret(AScrollBar);
    FStyle.UpdateInfo.ReCaret := False;
    FStyle.UpdateInfo.ReStyle := False;
    FStyle.UpdateInfo.ReScroll := False;
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
      DoCopyAsStream(vStream);  // 通知保存事件，便于加特征

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
          Clipboard.AsText := FData.SaveSelectToText;  // 文本格式
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
  FData.OnItemReFormatRequest := DoDataItemReFormatRequest;
  //FData.OnGetScreenCoord := DoDataGetScreenCoord;

  FDataBmp := TBitmap.Create;

  // 垂直滚动条，范围在Resize中设置
  FVScrollBar := THCScrollBar.Create(Self);
  FVScrollBar.Parent := Self;
  FVScrollBar.Orientation := TOrientation.oriVertical;
  FVScrollBar.OnScroll := DoVScrollChange;
  // 水平滚动条，范围在Resize中设置
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
    if Assigned(FCaret) then  // 防止切换Parent时多次创建
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
  vLang, vSType: Byte;
  vStyle: THCStyle;
begin
  _LoadFileFormatAndVersion(AStream, vFileFormat, vFileVersion, vLang);  // 文件格式和版本
  if vFileVersion > 59 then
  begin
    AStream.ReadBuffer(vSType, 1);
    if vSType <> HC_STREAM_LITE then
      Exit;
  end;

  vStyle := THCStyle.Create;
  try
    vStyle.LoadFromStream(AStream, vFileVersion);
    AProc(vFileVersion, vStyle);
  finally
    FreeAndNil(vStyle);
  end;
end;

procedure THCEdit.DataSaveLiteStream(const AStream: TStream; const AProc: THCProcedure);
var
  vSType: Byte;
begin
  _SaveFileFormatAndVersion(AStream);  // 文件格式和版本
  //_DeleteUnUsedStyle;  // 删除不使用的样式
  vSType := HC_STREAM_LITE;
  AStream.WriteBuffer(vSType, 1);
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
  //if vUndoList.Enable then  // 不能判断，因为撤销恢复过程会屏蔽，防止产生新的撤销恢复
  if not vUndoList.GroupWorking then  // 不在组中处理时才重新设置Data和响应变动
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
  //if vUndoList.Enable then  // 不能判断，因为撤销恢复过程会屏蔽，防止产生新的撤销恢复
  if not vUndoList.GroupWorking then  // 不在组中处理时才重新设置Data和响应变动
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

function THCEdit.DoDataGetScreenCoord(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result := Self.ClientToScreen(Result);
end;

procedure THCEdit.DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnInsertItem) then
    FOnInsertItem(AData, AItem);
end;

procedure THCEdit.DoDataItemReFormatRequest(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if not AData.CanEdit then Exit;
  (AData as THCFormatData).ReFormatActiveItem();  // 处理变动
  DoChange();
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

  {$REGION '快捷键'}
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
  vLang, vSType: Byte;
begin
  Self.BeginUpdate;
  try
    // 清除撤销恢复数据
    FUndoList.Clear;
    FUndoList.SaveState;
    try
      FUndoList.Enable := False;

      FData.Clear;
      FStyle.Initialize;
      AStream.Position := 0;
      _LoadFileFormatAndVersion(AStream, vFileExt, viVersion, vLang);  // 文件格式和版本
      if vFileExt <> HC_EXT then
        raise Exception.Create('加载失败，不是' + HC_EXT + '文件！');

      if viVersion > 59 then
      begin
        AStream.ReadBuffer(vSType, 1);
        if vSType <> HC_STREAM_LITE then  // 不是Lite文件流 20220520001
          Exit;
      end;

      FStyle.LoadFromStream(AStream, viVersion);  // 加载样式表
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

  CheckUpdateInfo;  // 换光标、切换激活Item
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

  CheckUpdateInfo;  // 高亮光标下
end;

procedure THCEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button = mbRight then Exit;  // 右键弹出菜单
  FData.MouseUp(Button, Shift, X - Self.Padding.Left + FHScrollBar.Position,
    Y - Self.Padding.Top + FVScrollBar.Position);

  if FStyle.UpdateInfo.DragingSelected then
    Screen.Cursor := crDefault;

  Cursor := GCursor;

  CheckUpdateInfo;  // 在选中区域中按下不移动弹起鼠标时需要更新

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
      Self.InsertText(Clipboard.AsText);
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

  { 初始化光标信息，为处理表格内往外迭代，只能放在这里 }
  vCaretInfo.X := 0;
  vCaretInfo.Y := 0;
  vCaretInfo.Height := 0;
  vCaretInfo.Visible := True;

  FData.GetCaretInfoCur(vCaretInfo);

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
  if not FStyle.UpdateInfo.ReScroll then // 滚动条平滑滚动时，可能将光标卷掉看不见
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
  else  // 非滚动条(方向键、点击等)引起的光标位置变化
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
  FStyle.States.Include(THCState.hosRedoing);
  try
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
  finally
    FStyle.States.Exclude(THCState.hosRedoing);
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
var
  vSType: Byte;
begin
  _SaveFileFormatAndVersion(AStream);  // 文件格式和版本
  vSType := HC_STREAM_LITE;
  AStream.WriteBuffer(vSType, 1);
  _DeleteUnUsedStyle;  // 删除不使用的样式(可否改为把有用的存了，加载时Item的StyleNo取有用)
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
  FStyle.States.Include(THCState.hosUndoing);
  try
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
  finally
    FStyle.States.Exclude(THCState.hosUndoing);
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
      // 控件背景
      FDataBmp.Canvas.Brush.Color := FStyle.BackgroundColor;// $00E7BE9F;
      FDataBmp.Canvas.FillRect(Rect(0, 0, FDataBmp.Width, FDataBmp.Height));
      //
      vViewWidth := GetViewWidth;
      vViewHeight := GetViewHeight;

      vPaintInfo := TPaintInfo.Create;
      try
        FData.PaintData(Self.Padding.Left - FHScrollBar.Position,  // 当前页数据要绘制到的Left
          Self.Padding.Top,     // 当前页数据要绘制到的Top
          Self.Width - FHScrollBar.Position - Self.Padding.Right,
          Self.Padding.Top + FData.Height,  // 当前页数据要绘制的Bottom
          Self.Padding.Top,     // 界面呈现当前页数据的Top位置
          Self.Height - FHScrollBar.Height,  // 界面呈现当前页数据Bottom位置
          FVScrollBar.Position,  // 指定从哪个位置开始的数据绘制到页数据起始位置
          FDataBmp.Canvas,
          vPaintInfo);

        for i := 0 to vPaintInfo.TopItems.Count - 1 do
          vPaintInfo.TopItems[i].PaintTop(FDataBmp.Canvas);
      finally
        vPaintInfo.Free;
      end;

      BitBlt(Canvas.Handle, 0, 0, vViewWidth, vViewHeight, FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
      InvalidateRect(Handle, ClientRect, False);  // 通知Edit只更新变动区域，防止闪烁，解决BitBlt光标滞留问题
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
            Self.InsertText(vS);
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
  // 光标在行最后，通过工具栏修改光标处字号后会重新设置焦点到HCView，如果不停止重取样式，
  // 则会以当前光标前文本样式做为当前样式了，如果是焦点失去后鼠标重新点击获取焦点
  // 会先触发这里，再触发MouseDown，在MouseDown里会重新取当前样式不受影响
  FStyle.UpdateInfoReCaret(False);
  FStyle.UpdateInfoRePaint;
  //FStyle.UpdateInfoReScroll;  // 失去焦点前光标不在显示页，获取焦点后不要乱跳
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

    WM_IME_CHAR:
      begin
        Message.Result := 1;
        Exit;
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
