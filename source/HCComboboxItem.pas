{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-8-6              }
{                                                       }
{       �ĵ�ComboboxItem(����ѡ���)����ʵ�ֵ�Ԫ        }
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
    FSaveItem, FStatic: Boolean;
    FItems, FItemValues: TStrings;
    FItemIndex, FMoveItemIndex: Integer;
    FButtonRect, FButtonDrawRect: TRect;
    FMouseInButton: Boolean;
    FPopupForm: THCPopupForm;
    FScrollBar: THCComScrollBar;
    FOnPopupItem, FOnItemIndexChanged: TNotifyEvent;

    // DropDown����
    function ScrollBarVisible: Boolean;
    function GetItemRect: TRect;
    procedure DoScroll(Sender: TObject; ScrollCode: TScrollCode; const ScrollPos: Integer);
    function GetItemIndexAt(const X, Y: Integer): Integer;
    procedure DoItemsChange(Sender: TObject);
    procedure DoPopupFormPaint(const ACanvas: TCanvas; const AClientRect: TRect);
    procedure DoPopupFormClose(Sender: TObject);

    procedure DoPopupFormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoPopupFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoPopupFormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoPopupFormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    procedure DoPopup; virtual;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure MouseLeave; override;
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    procedure SetItemIndex(const Value: Integer);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property Items: TStrings read FItems;// write SetItems;
    property ItemValues: TStrings read FItemValues;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    /// <summary> �Ƿ񱣴�ѡ�� </summary>
    property SaveItem: Boolean read FSaveItem write FSaveItem;
    property &Static: Boolean read FStatic write FStatic;
    property OnPopupItem: TNotifyEvent read FOnPopupItem write FOnPopupItem;
    property OnItemIndexChanged: TNotifyEvent read FOnItemIndexChanged write FOnItemIndexChanged;
  end;

implementation

uses
  Math;

const
  DROPDOWNFONTSIZE = 8;  // 8����
  DROPDOWNITEMHEIGHT = 16;  // 8���ָ߶�14 ���¸���1���
  DROPDOWNCOUNT = 8;  // ��������ʱ��ʾ��Item����

{ THCComboboxItem }

procedure THCComboboxItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FSaveItem := (Source as THCComboboxItem).SaveItem;
  FItems.Assign((Source as THCComboboxItem).Items);
  FItemValues.Assign((Source as THCComboboxItem).ItemValues);
end;

constructor THCComboboxItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  inherited Create(AOwnerData, AText);
  Self.StyleNo := THCStyle.Combobox;
  Width := 80;
  FPaddingRight := BTNWIDTH;
  FSaveItem := True;
  FStatic := False;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := DoItemsChange;

  FItemValues := TStringList.Create;

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
  FItemValues.Free;
  FScrollBar.Free;
  inherited Destroy;
end;

procedure THCComboboxItem.DoItemsChange(Sender: TObject);
begin
  if FItems.Count < DROPDOWNCOUNT then  // Ĭ�����8���ĸ߶�
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

  if not APaintInfo.Print then  // �Ǵ�ӡ
  begin
    if IsSelectComplate then
      ACanvas.Brush.Color := AStyle.SelColor
    else
    if FMouseInButton then
      ACanvas.Brush.Color := clMenu
    else
      ACanvas.Brush.Color := clWindow;

    ACanvas.FillRect(FButtonDrawRect);
  end
  else  // ��ӡ
    Exit;

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
    Self.ItemIndex := GetItemIndexAt(X, Y);
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
  ACanvas.Font.Size := DROPDOWNFONTSIZE;  // 8���֣���14

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
    IntersectClipRect(ACanvas.Handle, 0, 0, FScrollBar.Width, FScrollBar.Height);  // �����µļ������򣬸������ǵ�ǰ���������һ���ض����εĽ���
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
begin
  inherited FormatToDrawItem(ARichData, AItemNo);

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
begin
  Result := -1;
  if ScrollBarVisible then
    Result := FScrollBar.Position + Y
  else
    Result := Y;

  Result := Result div DROPDOWNITEMHEIGHT;
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

procedure THCComboboxItem.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FStatic and (not Key in [VK_BACK, VK_DELETE]) then
    inherited KeyDown(Key, Shift)
  else
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure THCComboboxItem.KeyPress(var Key: Char);
begin
  if not FStatic and Self.Enabled then
    inherited KeyPress(Key);
end;

function THCComboboxItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  if Self.Enabled and (not Self.ReadOnly) and OwnerData.CanEdit
    and (Button = mbLeft) and PtInRect(FButtonRect, Point(X, Y))
  then
  begin
    Result := True;
    DoPopup;
  end
  else
    Result := inherited MouseDown(Button, Shift, X, Y);
end;

procedure THCComboboxItem.MouseLeave;
begin
  inherited MouseLeave;
  FMouseInButton := False;
end;

function THCComboboxItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  if PtInRect(FButtonRect, Point(X, Y)) then
  begin
    if not FMouseInButton then
    begin
      FMouseInButton := True;
      OwnerData.Style.UpdateInfoRePaint;
    end;

    GCursor := crDefault;
    Result := True;
  end
  else
  begin
    if FMouseInButton then
    begin
      FMouseInButton := False;
      OwnerData.Style.UpdateInfoRePaint;
    end;

    Result := inherited MouseMove(Shift, X, Y);
  end;
end;

procedure THCComboboxItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FSaveItem := ANode.Attributes['saveitem'] or (FItems.Count > 0);
  if FSaveItem then
  begin
    FItems.Text := GetXmlRN(ANode.Attributes['item']);
    DelimitedXMLRN(ANode.Attributes['itemvalue'], FItemValues);
  end
  else
  begin
    FItems.Clear;
    FItemValues.Clear;
  end;

  if ANode.HasAttribute('static') then
    FStatic := ANode.Attributes['static'];
end;

procedure THCComboboxItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vText: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 36 then
  begin
    if AFileVersion > 40 then
    begin
      AStream.ReadBuffer(vByte, SizeOf(vByte));
      FStatic := Odd(vByte shr 7);
      FSaveItem := Odd(vByte shr 6);
    end
    else
      AStream.ReadBuffer(FSaveItem, SizeOf(FSaveItem));

    if FSaveItem then
    begin
      HCLoadTextFromStream(AStream, vText, AFileVersion); // ��Items
      FItems.Text := vText;
      HCLoadTextFromStream(AStream, vText, AFileVersion); // ��ItemValues
      FItemValues.Text := vText;
    end;
  end
  else
  begin
    HCLoadTextFromStream(AStream, vText, AFileVersion); // ��Items
    FItems.Text := vText;
    if (vText <> '') and (AFileVersion > 35) then
    begin
      HCLoadTextFromStream(AStream, vText, AFileVersion); // ��ItemValues
      FItemValues.Text := vText;
    end
    else
      FItemValues.Clear;

    FSaveItem := FItems.Count > 0;
  end;
end;

procedure THCComboboxItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FStatic then
    vByte := vByte or (1 shl 7);

  if FSaveItem then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));

  if FSaveItem then  // ��Items
  begin
    HCSaveTextToStream(AStream, FItems.Text);
    HCSaveTextToStream(AStream, FItemValues.Text);
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

    Self.DoChange;
  end;
end;

procedure THCComboboxItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);

  if FStatic then
    ANode.Attributes['static'] := FStatic;

  ANode.Attributes['saveitem'] := FSaveItem;
  if FSaveItem then  // ��Items
  begin
    ANode.Attributes['item'] := FItems.Text;
    ANode.Attributes['itemvalue'] := FItemValues.Text;
  end;
end;

end.
