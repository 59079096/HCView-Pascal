{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-8-6              }
{                                                       }
{       文档ComboboxItem(下拉选择框)对象实现单元        }
{                                                       }
{*******************************************************}

unit HCComboboxItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, HCItem, HCRectItem, HCXml,
  HCStyle, HCCustomData, HCCommon, HCEditItem, HCScrollBar, HCPopupForm;

type
  THCComScrollBar = class(THCScrollBar);

  THCComboboxItem = class(THCEditItem)
  private
    FSaveItem: Boolean;
    FItems: TStrings;
    FItemIndex, FMoveItemIndex: Integer;
    FButtonRect, FButtonDrawRect: TRect;
    FMouseInButton: Boolean;
    FPopupForm: THCPopupForm;
    FScrollBar: THCComScrollBar;
    FOnPopupItem: TNotifyEvent;

    // DropDown部分
    function ScrollBarVisible: Boolean;
    function GetItemRect: TRect;
    procedure DoScroll(Sender: TObject; ScrollCode: TScrollCode; const ScrollPos: Integer);
    function GetItemIndexAt(const X, Y: Integer): Integer;
    procedure DoItemsChange(Sender: TObject);
    procedure DoPopupFormPaint(const ACanvas: TCanvas; const AClientRect: TRect);
    procedure DoPopupFormClose(Sender: TObject);
    procedure DoPopup;

    procedure DoPopupFormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoPopupFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoPopupFormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoPopupFormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    procedure SetItems(const Value: TStrings);
    procedure SetItemIndex(const Value: Integer);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    /// <summary> 是否保存选项 </summary>
    property SaveItem: Boolean read FSaveItem write FSaveItem;
    property OnPopupItem: TNotifyEvent read FOnPopupItem write FOnPopupItem;
  end;

implementation

uses
  Math;

const
  DROPDOWNFONTSIZE = 8;  // 8号字
  DROPDOWNITEMHEIGHT = 16;  // 8号字高度14 上下各加1间距
  DROPDOWNCOUNT = 8;  // 下拉弹出时显示的Item数量

{ THCComboboxItem }

procedure THCComboboxItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FItems.Assign((Source as THCComboboxItem).Items);
end;

constructor THCComboboxItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  inherited Create(AOwnerData, AText);
  Self.StyleNo := THCStyle.Combobox;
  Width := 80;
  FSaveItem := True;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := DoItemsChange;

  FScrollBar := THCComScrollBar.Create(nil);
  FScrollBar.Orientation := TOrientation.oriVertical;
  FScrollBar.OnScroll := DoScroll;

  FPopupForm := THCPopupForm.Create;
  FPopupForm.OnPaint := DoPopupFormPaint;
  FPopupForm.OnPopupClose := DoPopupFormClose;
  FPopupForm.OnMouseDown := DoPopupFormMouseDown;
  FPopupForm.OnMouseMove := DoPopupFormMouseMove;
  FPopupForm.OnMouseUp := DoPopupFormMouseUp;
  FPopupForm.OnMouseWheel := DoPopupFormMouseWheel;
end;

destructor THCComboboxItem.Destroy;
begin
  FPopupForm.Free;
  FItems.Free;
  FScrollBar.Free;
  inherited Destroy;
end;

procedure THCComboboxItem.DoItemsChange(Sender: TObject);
begin
  if FItems.Count < DROPDOWNCOUNT then  // 默认最大8个的高度
    FPopupForm.Height := DROPDOWNITEMHEIGHT * FItems.Count
  else
    FPopupForm.Height := DROPDOWNITEMHEIGHT * DROPDOWNCOUNT;

  FScrollBar.Max := DROPDOWNITEMHEIGHT * FItems.Count;
  FScrollBar.Height := FPopupForm.Height;
end;

procedure THCComboboxItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  i, vLeft, vTop: Integer;
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  FButtonDrawRect := FButtonRect;
  FButtonDrawRect.Offset(ADrawRect.Location);

  if IsSelectComplate then
    ACanvas.Brush.Color := AStyle.SelColor
  else
  if FMouseInButton then
    ACanvas.Brush.Color := clMenu
  else
    ACanvas.Brush.Color := clWindow;
  ACanvas.FillRect(FButtonDrawRect);

  ACanvas.Pen.Color := clBlack;
  vLeft := FButtonDrawRect.Left + (BTNWIDTH - 7) div 2;
  vTop := FButtonDrawRect.Top + (FButtonDrawRect.Height - 4) div 2;

  for i := 0 to 3 do
  begin
    ACanvas.MoveTo(vLeft, vTop);
    ACanvas.LineTo(vLeft + 7 - i - i, vTop);

    Inc(vLeft);
    Inc(vTop);
  end;
end;

procedure THCComboboxItem.DoPopup;
var
  vPt: TPoint;
begin
  if Assigned(FOnPopupItem) then
    FOnPopupItem(Self);

  vPt := OwnerData.GetScreenCoord(FButtonDrawRect.Left - (Self.Width - FButtonDrawRect.Width),
    FButtonDrawRect.Bottom + 1);
  FPopupForm.Popup(vPt.X, vPt.Y);
end;

procedure THCComboboxItem.DoPopupFormClose(Sender: TObject);
begin
  if FItemIndex < 0 then
    Text := ''
  else
    Text := FItems[FItemIndex];

  FMoveItemIndex := -1;
  OwnerData.Style.UpdateInfoRePaint;
end;

procedure THCComboboxItem.DoPopupFormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vRect: TRect;
begin
  vRect := GetItemRect;
  if (not PtInRect(vRect, Point(X, Y))) and ScrollBarVisible then
    FScrollBar.MouseDown(Button, Shift, X - vRect.Right, Y - vRect.Top);
end;

procedure THCComboboxItem.DoPopupFormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  vIndex: Integer;
  vRect: TRect;
begin
  vRect := GetItemRect;
  if PtInRect(vRect, Point(X, Y)) then
  begin
    vIndex := GetItemIndexAt(X, Y);
    if vIndex <> FMoveItemIndex then
    begin
      FMoveItemIndex := vIndex;
      FPopupForm.UpdatePopup;
    end;
  end
  else
  if ScrollBarVisible then
    FScrollBar.MouseMove(Shift, X - vRect.Right, Y - vRect.Top);
end;

procedure THCComboboxItem.DoPopupFormMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vRect: TRect;
begin
  vRect := GetItemRect;
  if PtInRect(vRect, Point(X, Y)) then
  begin
    FItemIndex := GetItemIndexAt(X, Y);
    FPopupForm.ClosePopup(False);
  end
  else
  if ScrollBarVisible then
    FScrollBar.MouseUp(Button, Shift, X - vRect.Right, Y - vRect.Top);
end;

procedure THCComboboxItem.DoPopupFormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if ScrollBarVisible then
  begin
    if WheelDelta > 0 then
      FScrollBar.Position := FScrollBar.Position - DROPDOWNITEMHEIGHT
    else
      FScrollBar.Position := FScrollBar.Position + DROPDOWNITEMHEIGHT;
  end;
end;

procedure THCComboboxItem.DoPopupFormPaint(const ACanvas: TCanvas;
  const AClientRect: TRect);

  procedure GetDisplayRange(var AStartIndex, AEndIndex: Integer);
  var
    i, vH: Integer;
  begin
    AStartIndex := 0;
    AEndIndex := FItems.Count - 1;

    if ScrollBarVisible and (FItems.Count > 0) then
    begin
      vH := DROPDOWNITEMHEIGHT;
      for i := 0 to FItems.Count - 1 do
      begin
        if vH - FScrollBar.Position > 0 then
        begin
          AStartIndex := i;
          Break;
        end
        else
          Inc(vH, DROPDOWNITEMHEIGHT);
      end;

      for i := AStartIndex to FItems.Count - 1 do
      begin
        if vH - FScrollBar.Position > AClientRect.Bottom then
        begin
          AEndIndex := i;
          Break;
        end
        else
          Inc(vH, DROPDOWNITEMHEIGHT);
      end;
    end;
  end;

var
  i, vTop, vStartIndex, vEndIndex: Integer;
begin
  ACanvas.Brush.Color := clInfoBk;
  ACanvas.FillRect(GetItemRect);  // AClientRect

  ACanvas.Font.Size := DROPDOWNFONTSIZE;  // 8号字，高14

  GetDisplayRange(vStartIndex, vEndIndex);
  if ScrollBarVisible then
    vTop := vStartIndex * DROPDOWNITEMHEIGHT - FScrollBar.Position
  else
    vTop := 0;

  for i := vStartIndex to vEndIndex do
  begin
    if i = FMoveItemIndex then
    begin
      ACanvas.Brush.Color := clHighlight;
      ACanvas.FillRect(Bounds(0, vTop, Width, DROPDOWNITEMHEIGHT));
    end
    else
      ACanvas.Brush.Color := clInfoBk;

    ACanvas.TextOut(2, vTop + 1, FItems[i]);
    vTop := vTop + DROPDOWNITEMHEIGHT;
  end;

  if ScrollBarVisible then
  begin
    MoveWindowOrg(ACanvas.Handle, Width - FScrollBar.Width, 0);
    IntersectClipRect(ACanvas.Handle, 0, 0, FScrollBar.Width, FScrollBar.Height);  // 创建新的剪切区域，该区域是当前剪切区域和一个特定矩形的交集
    FScrollBar.PaintToEx(ACanvas);
  end;
end;

procedure THCComboboxItem.DoScroll(Sender: TObject; ScrollCode: TScrollCode;
  const ScrollPos: Integer);
begin
  FPopupForm.UpdatePopup;
end;

procedure THCComboboxItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vSize: TSize;
begin
  if Self.AutoSize then
  begin
    ARichData.Style.ApplyTempStyle(TextStyleNo);
    if Self.Text <> '' then
      vSize := ARichData.Style.TempCanvas.TextExtent(Self.Text)
    else
      vSize := ARichData.Style.TempCanvas.TextExtent('H');

    Width := FMargin + vSize.cx + FMargin + BTNWIDTH;  // 间距
    Height := FMargin + vSize.cy + FMargin;
  end;

  if Width < FMinWidth then
    Width := FMinWidth;
  if Height < FMinHeight then
    Height := FMinHeight;

  FButtonRect := Bounds(Width - BTNMARGIN - BTNWIDTH, BTNMARGIN, BTNWIDTH, Height - BTNMARGIN - BTNMARGIN);
  FPopupForm.Width := Self.Width;
end;

procedure THCComboboxItem.GetCaretInfo(var ACaretInfo: THCCaretInfo);
begin
  inherited GetCaretInfo(ACaretInfo);
  if (not Self.AutoSize) and (ACaretInfo.X > Width - BTNWIDTH) then
    ACaretInfo.Visible := False;
end;

function THCComboboxItem.GetItemIndexAt(const X, Y: Integer): Integer;
var
  vTop: Integer;
begin
  Result := -1;
  if ScrollBarVisible then
    vTop := FScrollBar.Position + Y
  else
    vTop := Y;

  Result := vTop div DROPDOWNITEMHEIGHT;
  if Result > FItems.Count - 1 then
    Result := FItems.Count - 1;
end;

function THCComboboxItem.GetItemRect: TRect;
begin
  if ScrollBarVisible then
    Result := Rect(0, 0, FPopupForm.Width - FScrollBar.Width, FPopupForm.Height)
  else
    Result := Rect(0, 0, FPopupForm.Width, FPopupForm.Height);
end;

procedure THCComboboxItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Button = mbLeft) and PtInRect(FButtonRect, Point(X, Y)) then
    DoPopup
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure THCComboboxItem.MouseLeave;
begin
  inherited MouseLeave;
  FMouseInButton := False;
end;

procedure THCComboboxItem.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if PtInRect(FButtonRect, Point(X, Y)) then
  begin
    if not FMouseInButton then
    begin
      FMouseInButton := True;
      OwnerData.Style.UpdateInfoRePaint;
    end;

    GCursor := crDefault;
  end
  else
  begin
    if FMouseInButton then
    begin
      FMouseInButton := False;
      OwnerData.Style.UpdateInfoRePaint;
    end;

    inherited MouseMove(Shift, X, Y);
  end;
end;

procedure THCComboboxItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FItems.Text := ANode.Attributes['item'];
end;

procedure THCComboboxItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vText: string;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vText); // 读Items
  FItems.Text := vText;
end;

procedure THCComboboxItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vSize: Word;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  if FSaveItem then  // 存Items
    HCSaveTextToStream(AStream, FItems.Text)
  else
  begin
    vSize := 0;
    AStream.WriteBuffer(vSize, SizeOf(vSize));
  end;
end;

function THCComboboxItem.ScrollBarVisible: Boolean;
begin
  Result := FItems.Count > DROPDOWNCOUNT;
end;

procedure THCComboboxItem.SetItemIndex(const Value: Integer);
begin
  if not ReadOnly then
  begin
    if (Value >= 0) and (Value <= FItems.Count - 1) then
    begin
      FItemIndex := Value;
      Text := FItems[FItemIndex];
    end
    else
    begin
      FItemIndex := -1;
      Text := '';
    end;
  end;
end;

procedure THCComboboxItem.SetItems(const Value: TStrings);
begin
  if not ReadOnly then
    FItems.Assign(Value);
end;

procedure THCComboboxItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);

  if FSaveItem then  // 存Items
    ANode.Attributes['item'] := FItems.Text;
end;

end.
