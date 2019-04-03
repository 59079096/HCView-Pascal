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
  HCCommon, HCScrollBar, HCStyle, HCTextStyle, HCParaStyle, HCItem, HCUndo, HCRichData;

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
    //
    function GetDisplayWidth: Integer;
    function GetDisplayHeight: Integer;

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
    procedure UpdateBuffer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CalcScrollRang;
    // Imm
    procedure UpdateImmPosition;

    /// <summary> 删除不使用的文本样式 </summary>
    procedure _DeleteUnUsedStyle;
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
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
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure ApplyParaAlignHorz(const AAlign: TParaAlignHorz);
    procedure ApplyParaAlignVert(const AAlign: TParaAlignVert);
    procedure ApplyParaBackColor(const AColor: TColor);
    procedure ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode);
    procedure ApplyTextStyle(const AFontStyle: THCFontStyle);
    procedure ApplyTextFontName(const AFontName: TFontName);
    procedure ApplyTextFontSize(const AFontSize: Single);
    procedure ApplyTextColor(const AColor: TColor);
    procedure ApplyTextBackColor(const AColor: TColor);
    function InsertItem(const AItem: THCCustomItem): Boolean; overload;
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem): Boolean; overload;
    /// <summary> 插入指定行列的表格 </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
    /// <summary> 获取顶层Data </summary>
    function TopLevelData: THCRichData;

    /// <summary> 全选 </summary>
    procedure SelectAll;
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream);

    /// <summary> 撤销 </summary>
    procedure Undo;

    /// <summary> 重做 </summary>
    procedure Redo;

    /// <summary> 当前光标处的文本样式 </summary>
    property CurStyleNo: Integer read GetCurStyleNo;
    /// <summary> 当前光标处的段样式 </summary>
    property CurParaNo: Integer read GetCurParaNo;

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

procedure THCEdit.ApplyParaLineSpace(const ASpaceMode: TParaLineSpaceMode);
begin
  FData.ApplyParaLineSpace(ASpaceMode);
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

procedure THCEdit.CheckUpdateInfo(const AScrollBar: Boolean);
begin
  if (FCaret <> nil) and FStyle.UpdateInfo.ReCaret then
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
      _SaveFileFormatAndVersion(vStream);  // 保存文件格式和版本
      //DoCopyDataBefor(vStream);  // 通知保存事件
      _DeleteUnUsedStyle;  // 保存已使用的样式
      FStyle.SaveToStream(vStream);
      FData.GetTopLevelData.SaveSelectToStream(vStream);
      vMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, vStream.Size);
      if vMem = 0 then
        raise Exception.Create(HCS_EXCEPTION_MEMORYLESS);
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

destructor THCEdit.Destroy;
begin
  FData.Free;
  FCaret.Free;
  FHScrollBar.Free;
  FVScrollBar.Free;
  FDataBmp.Free;
  FreeAndNil(FStyle);
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

function THCEdit.GetDisplayHeight: Integer;
begin
  if FHScrollBar.Visible then
    Result := Height - FHScrollBar.Height
  else
    Result := Height;
end;

function THCEdit.GetDisplayWidth: Integer;
begin
  if FVScrollBar.Visible then
    Result := Width - FVScrollBar.Width
  else
    Result := Width;
end;

function THCEdit.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem): Boolean;
begin
  Result := DataChangeByAction(function(): Boolean
    begin
      Result := FData.InsertItem(AIndex, AItem);
    end);
end;

function THCEdit.InsertTable(const ARowCount, AColCount: Integer): Boolean;
begin
  Result := DataChangeByAction(function(): Boolean
    var
      vTopData: THCRichData;
    begin
      vTopData := FData.GetTopLevelData;
      vTopData.InsertTable(ARowCount, AColCount);
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
    case Key of
      VK_BACK, VK_DELETE, VK_RETURN, VK_TAB:
        DoChange;

      VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END:
        DoDataCheckUpdateInfo;
    end;
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
  FData.Clear;
  FStyle.Initialize;
  AStream.Position := 0;
  _LoadFileFormatAndVersion(AStream, vFileExt, viVersion, vLang);  // 文件格式和版本
  if vFileExt <> HC_EXT then
    raise Exception.Create('加载失败，不是' + HC_EXT + '文件！');

  FStyle.LoadFromStream(AStream, viVersion);  // 加载样式表
  FData.LoadFromStream(AStream, FStyle, viVersion);
  DoMapChanged;
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

  if FStyle.UpdateInfo.Draging then
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

  if FStyle.UpdateInfo.Draging then
    Screen.Cursor := crDefault;

  Cursor := GCursor;

  CheckUpdateInfo;  // 在选中区域中按下不移动弹起鼠标时需要更新

  FStyle.UpdateInfo.Selecting := False;
  FStyle.UpdateInfo.Draging := False;
end;

procedure THCEdit.Paint;
begin
  BitBlt(Canvas.Handle, 0, 0, GetDisplayWidth, GetDisplayHeight,
    FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure THCEdit.Paste;
var
  vStream: TMemoryStream;
  vMem: Cardinal;
  vPtr: Pointer;
  vSize: Integer;
  viVersion: Word;
  vFileFormat: string;
  vLang: Byte;
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
      _LoadFileFormatAndVersion(vStream, vFileFormat, viVersion, vLang);  // 文件格式和版本
      //DoPasteDataBefor(vStream, viVersion);
      vStyle := THCStyle.Create;
      try
        vStyle.LoadFromStream(vStream, viVersion);
        FData.InsertStream(vStream, vStyle, viVersion);
      finally
        FreeAndNil(vStyle);
      end;
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

procedure THCEdit.ReBuildCaret(const AScrollBar: Boolean);
var
  vCaretInfo: THCCaretInfo;
  vDisplayHeight: Integer;
begin
  if FCaret = nil then Exit;

  if (not Self.Focused) or ((not Style.UpdateInfo.Draging) and FData.SelectExists) then
  begin
    FCaret.Hide;
    Exit;
  end;

  { 初始化光标信息，为处理表格内往外迭代，只能放在这里 }
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
        FVScrollBar.Position := FVScrollBar.Position + FCaret.Y - Self.Padding.Top
      else
      if FCaret.Y + FCaret.Height + Self.Padding.Top > vDisplayHeight then
        FVScrollBar.Position := FVScrollBar.Position + FCaret.Y + FCaret.Height + Self.Padding.Top - vDisplayHeight;
    end;
  end;

  if FCaret.Y + FCaret.Height > vDisplayHeight then
    FCaret.Height := vDisplayHeight - FCaret.Y;

  FCaret.Show;
  DoCaretChange;
end;

procedure THCEdit.Redo;
begin
  if FUndoList.Enable then  // 恢复过程不要产生新的Redo
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

procedure THCEdit.Resize;
begin
  inherited;
  FDataBmp.SetSize(GetDisplayWidth, GetDisplayHeight);
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
  _SaveFileFormatAndVersion(AStream);  // 文件格式和版本
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

function THCEdit.TopLevelData: THCRichData;
begin
  Result := FData.GetTopLevelData;
end;

procedure THCEdit.Undo;
begin
  if FUndoList.Enable then  // 撤销过程不要产生新的Undo
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

procedure THCEdit.UpdateBuffer;
var
  i, vDisplayWidth, vDisplayHeight: Integer;
  vPaintInfo: TPaintInfo;
begin
  if FUpdateCount = 0 then
  begin
    FDataBmp.Canvas.Lock;
    try
      // 控件背景
      FDataBmp.Canvas.Brush.Color := clWhite;// $00E7BE9F;
      FDataBmp.Canvas.FillRect(Rect(0, 0, FDataBmp.Width, FDataBmp.Height));
      //
      vDisplayWidth := GetDisplayWidth;
      vDisplayHeight := GetDisplayHeight;

      vPaintInfo := TPaintInfo.Create;
      try
        FData.PaintData(Self.Padding.Left - FHScrollBar.Position,  // 当前页数据要绘制到的Left
          Self.Padding.Top,     // 当前页数据要绘制到的Top
          Self.Padding.Top + FData.Height,  // 当前页数据要绘制的Bottom
          Self.Padding.Top,     // 界面呈现当前页数据的Top位置
          Self.Height,  // 界面呈现当前页数据Bottom位置
          FVScrollBar.Position,  // 指定从哪个位置开始的数据绘制到页数据起始位置
          FDataBmp.Canvas,
          vPaintInfo);

        for i := 0 to vPaintInfo.TopItems.Count - 1 do
          vPaintInfo.TopItems[i].PaintTop(FDataBmp.Canvas);
      finally
        vPaintInfo.Free;
      end;

      BitBlt(Canvas.Handle, 0, 0, vDisplayWidth, vDisplayHeight, FDataBmp.Canvas.Handle, 0, 0, SRCCOPY);
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
  FData.KillFocus;
end;

procedure THCEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
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
  for i := 0 to FStyle.TextStyles.Count - 1 do
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

  FData.MarkStyleUsed(False);

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

end.
