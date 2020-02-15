{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-7-9              }
{                                                       }
{          文档EditItem(文本框)对象实现单元             }
{                                                       }
{*******************************************************}

unit HCEditItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, HCItem, HCRectItem, HCStyle,
  HCCustomData, HCFormatData, HCCommon, HCXml;

const
  BTNWIDTH = 16;
  BTNMARGIN = 1;

type
  THCEditItem = class(THCControlItem)
  private
    FText: string;
    FBorderWidth: Byte;
    FBorderSides: TBorderSides;
    FMouseIn, FReadOnly, FPrintOnlyText: Boolean;
    FCaretOffset: ShortInt;
  protected
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure SetActive(const Value: Boolean); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    /// <summary> 正在其上时内部是否处理指定的Key和Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); virtual;
    procedure Assign(Source: THCCustomItem); override;
    procedure Clear; override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;
    function InsertText(const AText: string): Boolean; override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property PrintOnlyText: Boolean read FPrintOnlyText write FPrintOnlyText;
    property BorderSides: TBorderSides read FBorderSides write FBorderSides;
    property BorderWidth: Byte read FBorderWidth write FBorderWidth;
  end;

implementation

uses
  Math, Clipbrd;

{ THCEditItem }

procedure THCEditItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FText := (Source as THCEditItem).Text;
  FReadOnly := (Source as THCEditItem).ReadOnly;
  FPrintOnlyText := (Source as THCEditItem).PrintOnlyText;
  FBorderSides := (Source as THCEditItem).BorderSides;
  FBorderWidth := (Source as THCEditItem).BorderWidth;
end;

procedure THCEditItem.Clear;
begin
  Self.Text := '';
end;

constructor THCEditItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.Edit;
  FText := AText;
  FMouseIn := False;
  FMargin := 4;
  FCaretOffset := -1;
  Width := 50;
  FPrintOnlyText := False;
  FBorderWidth := 1;
  FBorderSides := [cbsLeft, cbsTop, cbsRight, cbsBottom];
end;

procedure THCEditItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if Self.IsSelectComplate and (not APaintInfo.Print) then
  begin
    ACanvas.Brush.Color := AStyle.SelColor;
    ACanvas.FillRect(ADrawRect);
  end;

  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);

  if not Self.AutoSize then
    ACanvas.TextRect(ADrawRect, ADrawRect.Left + FMargin, ADrawRect.Top + FMargin, FText)
  else
    ACanvas.TextOut(ADrawRect.Left + FMargin,// + (ADrawRect.Width - FMargin - ACanvas.TextWidth(FText) - FMargin) div 2,
      ADrawRect.Top + FMargin, FText);

  if APaintInfo.Print and FPrintOnlyText then Exit;

  // 非打印
  if FMouseIn then  // 鼠标在其中
    ACanvas.Pen.Color := clBlue
  else  // 鼠标不在其中或打印
    ACanvas.Pen.Color := clBlack;

  ACanvas.Pen.Width := FBorderWidth;
  ACanvas.Pen.Style := psSolid;

  if cbsLeft in FBorderSides then
  begin
    ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Top);
    ACanvas.LineTo(ADrawRect.Left, ADrawRect.Bottom);
  end;

  if cbsTop in FBorderSides then
  begin
    ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Top);
    ACanvas.LineTo(ADrawRect.Right, ADrawRect.Top);
  end;

  if cbsRight in FBorderSides then
  begin
    ACanvas.MoveTo(ADrawRect.Right - 1, ADrawRect.Top);
    ACanvas.LineTo(ADrawRect.Right - 1, ADrawRect.Bottom);
  end;

  if cbsBottom in FBorderSides then
  begin
    ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Bottom - 1);
    ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom - 1);
  end;
end;

procedure THCEditItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vSize: TSize;
begin
  if Self.AutoSize then
  begin
    ARichData.Style.ApplyTempStyle(TextStyleNo);
    if FText <> '' then
      vSize := ARichData.Style.TempCanvas.TextExtent(FText)
    else
      vSize := ARichData.Style.TempCanvas.TextExtent('H');

    Width := FMargin + vSize.cx + FMargin;  // 间距
    Height := FMargin + vSize.cy + FMargin;
  end;

  if Width < FMinWidth then
    Width := FMinWidth;
  if Height < FMinHeight then
    Height := FMinHeight;
end;

procedure THCEditItem.GetCaretInfo(var ACaretInfo: THCCaretInfo);
var
  vSize: TSize;
  vS: string;
begin
  if FCaretOffset < 0 then
  begin
    ACaretInfo.Visible := False;
    Exit;
  end;

  vS := Copy(FText, 1, FCaretOffset);
  OwnerData.Style.ApplyTempStyle(TextStyleNo);

  if vS <> '' then
  begin
    vSize := OwnerData.Style.TempCanvas.TextExtent(vS);
    ACaretInfo.Height := vSize.cy + OwnerData.Style.TextStyles[TextStyleNo].TextMetric.tmExternalLeading;
    ACaretInfo.X := FMargin + vSize.cx;// + (Width - FMargin - OwnerData.Style.DefCanvas.TextWidth(FText) - FMargin) div 2;
  end
  else
  begin
    ACaretInfo.Height := OwnerData.Style.TextStyles[TextStyleNo].FontHeight
      + OwnerData.Style.TextStyles[TextStyleNo].TextMetric.tmExternalLeading;
    ACaretInfo.X := FMargin;// + (Width - FMargin - OwnerData.Style.DefCanvas.TextWidth(FText) - FMargin) div 2;
  end;

  ACaretInfo.Y := FMargin;

  if (not Self.AutoSize) and (ACaretInfo.X > Width) then
    ACaretInfo.Visible := False;
end;

function THCEditItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X <= FMargin then
    Result := OffsetBefor
  else
  if X >= Width - FMargin then
    Result := OffsetAfter
  else
    Result := OffsetInner;
end;

function THCEditItem.GetText: string;
begin
  Result := FText;
end;

function THCEditItem.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
begin
  Result := False;
  if OwnerData.Style.States.Contain(THCState.hosPasting) then
    Result := InsertText(Clipboard.AsText);
end;

function THCEditItem.InsertText(const AText: string): Boolean;
begin
  System.Insert(AText, FText, FCaretOffset + 1);
  Inc(FCaretOffset, System.Length(AText));
  Self.SizeChanged := True;
end;

procedure THCEditItem.KeyDown(var Key: Word; Shift: TShiftState);

  procedure BackspaceKeyDown;
  begin
    if FCaretOffset > 0 then
    begin
      System.Delete(FText, FCaretOffset, 1);
      Dec(FCaretOffset);
    end;
    Self.SizeChanged := True;
  end;

  procedure LeftKeyDown;
  begin
    if FCaretOffset > 0 then
      Dec(FCaretOffset);
  end;

  procedure RightKeyDown;
  begin
    if FCaretOffset < System.Length(FText) then
      Inc(FCaretOffset);
  end;

  procedure DeleteKeyDown;
  begin
    if FCaretOffset < System.Length(FText) then
      System.Delete(FText, FCaretOffset + 1, 1);

    Self.SizeChanged := True;
  end;

begin
  if not FReadOnly then
  begin
    case Key of
      VK_BACK: BackspaceKeyDown;  // 回删
      VK_LEFT: LeftKeyDown;       // 左方向键
      VK_RIGHT: RightKeyDown;     // 右方向键
      VK_DELETE: DeleteKeyDown;   // 删除键
      VK_HOME: FCaretOffset := 0;  // Home键
      VK_END: FCaretOffset := System.Length(FText);  // End键
    else
      inherited KeyDown(Key, Shift);
    end;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure THCEditItem.KeyPress(var Key: Char);
begin
  if not FReadOnly then
  begin
    Inc(FCaretOffset);
    System.Insert(Key, FText, FCaretOffset);

    Self.SizeChanged := True;
  end
  else
    inherited KeyPress(Key);
end;

function THCEditItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vX: Integer;
  vOffset: Integer;
begin
  Result := inherited MouseDown(Button, Shift, X, Y);
  OwnerData.Style.ApplyTempStyle(TextStyleNo);
  vX := X - FMargin;// - (Width - FMargin - OwnerData.Style.DefCanvas.TextWidth(FText) - FMargin) div 2;
  vOffset := GetNorAlignCharOffsetAt(OwnerData.Style.TempCanvas, FText, vX);
  if vOffset <> FCaretOffset then
  begin
    FCaretOffset := vOffset;
    OwnerData.Style.UpdateInfoReCaret;
  end;
end;

procedure THCEditItem.MouseEnter;
begin
  inherited MouseEnter;
  FMouseIn := True;
end;

procedure THCEditItem.MouseLeave;
begin
  inherited MouseLeave;
  FMouseIn := False;
end;

procedure THCEditItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FReadOnly := ANode.Attributes['readonly'];
  FPrintOnlyText := ANode.Attributes['printonlytext'];
  SetBorderSideByPro(ANode.Attributes['border'], FBorderSides);
  FBorderWidth := ANode.Attributes['borderwidth'];
  FText := ANode.Text;
end;

procedure THCEditItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FText, AFileVersion);  // 读取Text

  if AFileVersion > 33 then
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    FReadOnly := Odd(vByte shr 7);
    FPrintOnlyText := Odd(vByte shr 6);
  end
  else
  begin
    AStream.ReadBuffer(FReadOnly, SizeOf(FReadOnly));
    FPrintOnlyText := False;
  end;

  if AFileVersion > 15 then
  begin
    AStream.ReadBuffer(FBorderSides, SizeOf(FBorderSides));
    AStream.ReadBuffer(FBorderWidth, SizeOf(FBorderWidth));
  end;
end;

procedure THCEditItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FText); // 存Text

  vByte := 0;
  if FReadOnly then
    vByte := vByte or (1 shl 7);

  if FPrintOnlyText then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  AStream.WriteBuffer(FBorderSides, SizeOf(FBorderSides));
  AStream.WriteBuffer(FBorderWidth, SizeOf(FBorderWidth));
end;

procedure THCEditItem.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Value then
    FCaretOffset := -1;
end;

procedure THCEditItem.SetText(const Value: string);
begin
  if (not FReadOnly) and (FText <> Value) then
  begin
    FText := Value;
    if FCaretOffset > System.Length(FText) then
      FCaretOffset := 0;

    if Self.AutoSize then
      (OwnerData as THCFormatData).ItemRequestFormat(Self)
    else
      OwnerData.Style.UpdateInfoRePaint;
  end;
end;

procedure THCEditItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['readonly'] := FReadOnly;
  ANode.Attributes['printonlytext'] := FPrintOnlyText;
  ANode.Attributes['border'] := GetBorderSidePro(FBorderSides);
  ANode.Attributes['borderwidth'] := FBorderWidth;
  ANode.Text := FText;
end;

function THCEditItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := False;

  if Key = VK_LEFT then
  begin
    if FCaretOffset = 0 then  // 最左再次向左，移出

    else
    if FCaretOffset < 0 then  // 外面左键移入
    begin
      FCaretOffset := System.Length(FText);
      Result := True;
    end
    else  // > 0
      Result := True;
  end
  else
  if Key = VK_RIGHT then
  begin
    if FCaretOffset = System.Length(FText) then  // 最右再次向右，移出

    else
    if FCaretOffset < 0 then  // 外面右键移入
    begin
      FCaretOffset := 0;
      Result := True;
    end
    else  // < Length
      Result := True;
  end
  else
    Result := True;
end;

end.
