{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-8-7              }
{                                                       }
{           文档PopupForm(弹出窗体)实现单元             }
{                                                       }
{*******************************************************}

unit HCPopupForm;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Messages, Forms;

const
  CM_HC_KILLPOPUP = WM_USER + $4000;

type
  TPopupPaintEvent = procedure (const ACanvas: TCanvas; const AClientRect: TRect) of object;

  THCPopupForm = class(TObject)
  private
    FOpened: Boolean;
    FPopupWindow: HWND;
    FWidth, FHeight,
    FWheelAccumulator: Integer;  // 鼠标滚轮蓄能器

    FOnPaint: TPopupPaintEvent;
    FOnPopupClose: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;

    procedure RegFormClass;
    procedure CreateFormHandle;
    procedure DestroyForm;
    procedure DoPopupFormPaint;

    function CalcCursorPos: TPoint;
    procedure DoMouseDown(const Message: TWMMouse; Button: TMouseButton; Shift: TShiftState);
    procedure DoMouseUp(const Message: TWMMouse; Button: TMouseButton);
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;

    procedure WMMouseWheel(var Message: TWMMouseWheel);
    procedure WMLButtonDown(var Message: TWMLButtonDown);
    procedure WMMouseMove(var Message: TWMMouseMove);
    procedure WMLButtonUp(var Message: TWMLButtonUp);
    procedure WndProc(var Message: TMessage);
  protected
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(X, Y: Integer);
    procedure ClosePopup(const ACancel: Boolean);
    procedure UpdatePopup;

    property OnPopupClose: TNotifyEvent read FOnPopupClose write FOnPopupClose;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property OnPaint: TPopupPaintEvent read FOnPaint write FOnPaint;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
  end;

implementation

type
  TApplicationAccess = class(TApplication);

{ THCPopupForm }

function THCPopupForm.CalcCursorPos: TPoint;
begin
  GetCursorPos(Result);
  ScreenToClient(FPopupWindow, Result);
end;

procedure THCPopupForm.ClosePopup(const ACancel: Boolean);
begin
  // 先触发事件再关闭Popup，这样会感觉响应更快
  if (not ACancel) and Assigned(FOnPopupClose) then
    FOnPopupClose(Self);
  //ShowWindow(FPopupWindow, SW_HIDE);
  DestroyForm;  // 如果仅是SW_HIDE，Windows仍会将PopupForm记录为最顶层窗体，导致隐藏后按下键后，焦点回到PopuForm

  FOpened := False;
end;

constructor THCPopupForm.Create;
begin
  FPopupWindow := 0;
  FOpened := False;
  FWidth := 50;
  FHeight := 100;
  RegFormClass;
end;

procedure THCPopupForm.CreateFormHandle;
var
  vClassName: string;
begin
  if not IsWindow(FPopupWindow) then  // 如果提示窗体没有创建
  begin
    vClassName := ClassName;
    FPopupWindow := CreateWindowEx(
        WS_EX_TOPMOST or WS_EX_TOOLWINDOW,  // 顶层窗口
        PChar(vClassName),
        nil,
        WS_POPUP,  // 弹出式窗口,支持双击
        0, 0, FWidth, FHeight, 0, 0, HInstance, nil);

    SetWindowLong(FPopupWindow, GWL_WNDPROC, Longint(MakeObjectInstance(WndProc)));  // 窗口函数替换为类方法
  end;
end;

procedure THCPopupForm.DestroyForm;
begin
  if IsWindow(FPopupWindow) then
  begin
    DestroyWindow(FPopupWindow);
    FPopupWindow := 0;
  end;
end;

destructor THCPopupForm.Destroy;
begin
  DestroyForm;
  inherited Destroy;
end;

procedure THCPopupForm.DoMouseDown(const Message: TWMMouse; Button: TMouseButton;
  Shift: TShiftState);
begin
  if Assigned(FOnMouseDown) then
  begin
    with Message do
    begin
      if (Width > 32768) or (Height > 32768) then
      begin
        with CalcCursorPos do
        begin
          FOnMouseDown(Self, Button, KeysToShiftState(Keys) + Shift + MouseOriginToShiftState, X, Y);
        end;
      end
      else
        FOnMouseDown(Self, Button, KeysToShiftState(Keys) + Shift + MouseOriginToShiftState, XPos, YPos);
    end;
  end;
end;

procedure THCPopupForm.DoMouseUp(const Message: TWMMouse; Button: TMouseButton);
begin
  if Assigned(FOnMouseUp) then
  begin
    with Message do
    begin
      FOnMouseUp(Self, Button, KeysToShiftState(Keys) + MouseOriginToShiftState, XPos, YPos);
    end;
  end;
end;

function THCPopupForm.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  IsNeg: Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        if FWheelAccumulator <> 0 then
          FWheelAccumulator := -FWheelAccumulator;

        if Assigned(FOnMouseWheelDown) then
          FOnMouseWheelDown(Self, Shift, MousePos, Result);
      end
      else
      begin
        if Assigned(FOnMouseWheelUp) then
          FOnMouseWheelUp(Self, Shift, MousePos, Result);
      end;
    end;
  end;
end;

procedure THCPopupForm.DoPopupFormPaint;
var
  vDC: HDC;
  vPaintStruct: TPaintStruct;
  vCanvas: TCanvas;
  vRect: TRect;
begin
  if Assigned(FOnPaint) then
  begin
    vDC := BeginPaint(FPopupWindow, vPaintStruct);
    try
      //vPaintStruct.fErase := False;  // 不擦除背景
      vCanvas := TCanvas.Create;
      try
        vCanvas.Handle := vDC;
        GetClientRect(FPopupWindow, vRect);

        FOnPaint(vCanvas, vRect);
      finally
        vCanvas.Handle := 0;
        FreeAndNil(vCanvas);
      end;
    finally
      EndPaint(FPopupWindow, vPaintStruct);
    end;
  end;
end;

procedure THCPopupForm.Popup(X, Y: Integer);
var
  vMsg: TMsg;

  function IsFPopupWindow(Wnd: HWnd): Boolean;
  begin
    while (Wnd <> 0) and (Wnd <> FPopupWindow) do
      Wnd := GetParent(Wnd);
    Result := Wnd = FPopupWindow;
  end;

  {$REGION 'MessageLoop'}
  procedure MessageLoop;
  begin
    try
      repeat
        if not FOpened then Exit;

        if PeekMessage(vMsg, 0, 0, 0, PM_NOREMOVE) then  // 20160708001 以查看的方式从系统中获取消息，可以不将消息从系统中移除
        begin
          case vMsg.message of
            WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
            WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK, WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
            WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
              begin
                if not IsFPopupWindow(vMsg.hwnd) then
                begin
                  //if FRemoveMessageOnClose then  // 点击在非Popup窗体关闭时，是否移除消息(不传递到鼠标位置控件上)
                    PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);  // 退出后移除当前窗体消息(点击Popup窗体外的按钮时关闭Popup窗体不执行按钮事件)防止仅为关闭Popup窗体而误操作
                  Break;
                end;
                //if vMsg.hwnd = FPopupWindow then  // 兼容 TCPopup中没有实际的Control置于FPopupWindow上的情况
                //  Break;
                //PeekMessage(vMsg, 0, vMsg.message, vMsg.message, PM_REMOVE);
                //SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
              end;

            {WM_LBUTTONUP:
              begin
                if IsFPopupWindow(vMsg.hwnd) then
                begin
                  //PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);
                  //SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
                end;
              end;}

            WM_MOUSEWHEEL:  // 弹出后响应所有滚轮事件
              begin
                PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);
                SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
                Continue;
              end;

            WM_KEYFIRST..WM_KEYLAST:
              begin
                PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);
                SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
                Continue;
              end;

            //WM_C_KILLPOPUP: Exit;  // 外部发送关闭Popu消息

            WM_KILLFOCUS, CM_HC_KILLPOPUP:
              Exit;

            CM_DEACTIVATE, WM_ACTIVATEAPP:
              Break;
          end;

          Application.HandleMessage;
        end
        else
          TApplicationAccess(Application).Idle(vMsg);
      until Application.Terminated;
    finally
      if FOpened then
        ClosePopup(True);
    end;
  end;
  {$ENDREGION}

var
  vBound: TRect;
  vW, vH: Integer;
  vMonitor: TMonitor;
begin
  CreateFormHandle;

  GetWindowRect(FPopupWindow, vBound);

  //Windows.SetFocus(FPopupWindow);

  vW := vBound.Right - vBound.Left;
  vH := vBound.Bottom - vBound.Top;

  vMonitor := Screen.MonitorFromPoint(Point(X, Y));

  if vMonitor <> nil then
  begin
    if X + vW > vMonitor.WorkareaRect.Right then
      X := vMonitor.WorkareaRect.Right - vW;
    if Y + vH > vMonitor.WorkareaRect.Bottom then
      Y := vBound.Top - vH;

    if X < vMonitor.WorkareaRect.Left then
      X := vMonitor.WorkareaRect.Left;
    if Y < vMonitor.WorkareaRect.Top then
      Y := vMonitor.WorkareaRect.Top;
  end
  else
  begin
    if X + vW > Screen.WorkareaRect.Right then
      X := Screen.WorkareaRect.Right - vW;
    if Y + vH > Screen.WorkareaRect.Bottom then
      Y := vBound.Top - vH;

    if X < Screen.WorkareaRect.Left then
      X := Screen.WorkareaRect.Left;
    if Y < Screen.WorkareaRect.Top then
      Y := Screen.WorkareaRect.Top;
  end;

  //FTopWindow := GetActiveWindow;  // GetActiveWindow 程序的当前激活的窗口，GetForegroundWindow 整个Windows系统的当前激活的窗口
  //FTopWindowActive := GetFocus;  // 当前有焦点的窗口

  //MoveWindow(FPopupWindow, X, Y, vW, vH, True);
  //ShowWindow(FPopupWindow, SW_SHOWNOACTIVATE);  // SW_SHOWNOACTIVATE SW_SHOW

  SetWindowPos(FPopupWindow, hwnd_Top, X, Y, vW, vH, swp_NoActivate or swp_ShowWindow );
  FOpened := True;

  MessageLoop;
end;

procedure THCPopupForm.RegFormClass;
var
  vWndCls: TWndClassEx;
  vClassName: string;
begin
  vClassName := ClassName;
  if not GetClassInfoEx(HInstance, PChar(vClassName), vWndCls) then
  begin
    vWndCls.cbSize        := SizeOf(TWndClassEx);
    vWndCls.lpszClassName := PChar(vClassName);
    vWndCls.style         := CS_VREDRAW or CS_HREDRAW
      or CS_DROPSHADOW or CS_DBLCLKS;  // 通过此样式实现窗口边框阴影效果，只能在注册窗口类时使用此属性，注册后可通过SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or CS_DROPSHADOW);再增加

    vWndCls.hInstance     := HInstance;
    vWndCls.lpfnWndProc   := @DefWindowProc;
    vWndCls.cbClsExtra    := 0;
    vWndCls.cbWndExtra    := SizeOf(DWord) * 2;
    vWndCls.hIcon         := LoadIcon(HInstance, MakeIntResource('MAINICON'));
    vWndCls.hIconSm       := LoadIcon(HInstance, MakeIntResource('MAINICON'));
    vWndCls.hCursor       := LoadCursor(0, IDC_ARROW);
    vWndCls.hbrBackground := GetStockObject(WHITE_BRUSH);
    vWndCls.lpszMenuName  := nil;

    if RegisterClassEx(vWndCls) = 0 then
    begin
      //MessageBox(0, '注册TCustomPopup错误!', 'TCFCustomPopup', MB_OK);
      raise Exception.Create('异常：注册THCPopupForm错误!');
      Exit;
    end;
  end;
end;

procedure THCPopupForm.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    SetWindowPos(FPopupWindow, 0, 0, 0, FWidth, FHeight, SWP_NOZORDER);
  end;
end;

procedure THCPopupForm.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    SetWindowPos(FPopupWindow, 0, 0, 0, FWidth, FHeight, SWP_NOZORDER);
  end;
end;

procedure THCPopupForm.UpdatePopup;
var
  vRect: TRect;
begin
  if IsWindowVisible(FPopupWindow) then
  begin
    GetClientRect(FPopupWindow, vRect);
    InvalidateRect(FPopupWindow, vRect, False);
  end;
end;

procedure THCPopupForm.WMLButtonDown(var Message: TWMLButtonDown);
begin
  DoMouseDown(Message, mbLeft, []);
end;

procedure THCPopupForm.WMLButtonUp(var Message: TWMLButtonUp);
begin
  DoMouseUp(Message, mbLeft);
end;

procedure THCPopupForm.WMMouseMove(var Message: TWMMouseMove);
begin
  if Assigned(FOnMouseMove) then
  begin
    with Message do
    begin
      if (Width > 32768) or (Height > 32768) then
      begin
        with CalcCursorPos do
          FOnMouseMove(Self, KeysToShiftState(Keys) + MouseOriginToShiftState, X, Y)
      end
      else
        FOnMouseMove(Self, KeysToShiftState(Keys) + MouseOriginToShiftState, XPos, YPos);
    end;
  end;
end;

procedure THCPopupForm.WMMouseWheel(var Message: TWMMouseWheel);
var
  vKeyState: TKeyboardState;
begin
  with Message do
  begin
    Result := 0;
    GetKeyboardState(vKeyState);

    if DoMouseWheel(KeyboardStateToShiftState(vKeyState), WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1
    {else if Parent <> nil then
      with TMessage(Message) do
        Result := Parent.Perform(CM_MOUSEWHEEL, WParam, LParam);}
  end;
end;

procedure THCPopupForm.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_PAINT:
      begin
        DoPopupFormPaint;
        Message.Result := 0;
      end;

    //WM_SETCURSOR:
    //  StripSetCursor(AWnd, lParam);
    WM_ERASEBKGND:  // 通知已经重画背景了
      Message.Result := 1;

    //WM_CAPTURECHANGED:
    //  ShowWindow(AWnd, SW_HIDE);
         //WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP, WM_DESTROY:

    WM_MOUSEACTIVATE:
      Message.Result := MA_NOACTIVATE;

    WM_NCACTIVATE:
      begin
        FOpened := False;
        Message.Result := 1;
      end;

    wm_SysCommand:  // jt+
      ClosePopup(True);

    wm_KeyFirst..wm_KeyLast:  // jt+
      begin
        if Message.WParam = vk_Escape then
          ClosePopup(True);
      end;

    WM_LBUTTONDOWN: WMLButtonDown(TWMLButtonDown(Message));

    WM_LBUTTONUP: WMLButtonUp(TWMLButtonUp(Message));

    WM_MOUSEMOVE: WMMouseMove(TWMMouseMove(Message));

    WM_MOUSEWHEEL: WMMouseWheel(TWMMouseWheel(Message));
  else
    Message.Result := DefWindowProc(FPopupWindow, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

end.
