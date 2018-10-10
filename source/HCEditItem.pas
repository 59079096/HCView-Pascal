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
  HCCustomData, HCCommon;

const
  BTNWIDTH = 16;
  BTNMARGIN = 1;

type
  THCEditItem = class(THCControlItem)
  private
    FText: string;
    FBorderWidth: Byte;
    FBorderSides: TBorderSides;
    FMouseIn, FReadOnly: Boolean;
    FCaretOffset: ShortInt;
  protected
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function GetOffsetAt(const X: Integer): Integer; override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary> 正在其上时内部是否处理指定的Key和Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    function InsertText(const AText: string): Boolean; override;
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;

    procedure SetText(const Value: string); virtual;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); virtual;
    procedure Assign(Source: THCCustomItem); override;
    property Text: string read FText write SetText;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property BorderSides: TBorderSides read FBorderSides write FBorderSides;
    property BorderWidth: Byte read FBorderWidth write FBorderWidth;
  end;

implementation

uses
  Math;

{ THCEditItem }

procedure THCEditItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FText := (Source as THCEditItem).Text;
  FReadOnly := (Source as THCEditItem).ReadOnly;
  FBorderSides := (Source as THCEditItem).BorderSides;
  FBorderWidth := (Source as THCEditItem).BorderWidth;
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

  if FMouseIn and (not APaintInfo.Print) then  // 鼠标在其中，且非打印
    ACanvas.Pen.Color := clBlue
  else  // 鼠标不在其中或打印
    ACanvas.Pen.Color := clBlack;

  ACanvas.Pen.Width := FBorderWidth;

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
    ACanvas.MoveTo(ADrawRect.Right, ADrawRect.Top);
    ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom);
  end;

  if cbsBottom in FBorderSides then
  begin
    ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Bottom);
    ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom);
  end;
end;

procedure THCEditItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vSize: TSize;
begin
  if Self.AutoSize then
  begin
    ARichData.Style.TextStyles[TextStyleNo].ApplyStyle(ARichData.Style.DefCanvas);
    if FText <> '' then
      vSize := ARichData.Style.DefCanvas.TextExtent(FText)
    else
      vSize := ARichData.Style.DefCanvas.TextExtent('I');
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
  vS := Copy(FText, 1, FCaretOffset);
  OwnerData.Style.TextStyles[TextStyleNo].ApplyStyle(OwnerData.Style.DefCanvas);

  if vS <> '' then
  begin
    vSize := OwnerData.Style.DefCanvas.TextExtent(vS);
    ACaretInfo.Height := vSize.cy;
    ACaretInfo.X := FMargin + vSize.cx;// + (Width - FMargin - OwnerData.Style.DefCanvas.TextWidth(FText) - FMargin) div 2;
  end
  else
  begin
    ACaretInfo.Height := OwnerData.Style.DefCanvas.TextHeight('H');
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

procedure THCEditItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vX: Integer;
  vOffset: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  OwnerData.Style.TextStyles[TextStyleNo].ApplyStyle(OwnerData.Style.DefCanvas);
  vX := X - FMargin;// - (Width - FMargin - OwnerData.Style.DefCanvas.TextWidth(FText) - FMargin) div 2;
  vOffset := GetCharOffsetByX(OwnerData.Style.DefCanvas, FText, vX);
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

procedure THCEditItem.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  //GCursor := crIBeam;
end;

procedure THCEditItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure THCEditItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vSize: Word;
  vBuffer: TBytes;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  // 读取Text
  AStream.ReadBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    SetLength(vBuffer, vSize);
    AStream.ReadBuffer(vBuffer[0], vSize);
    FText := StringOf(vBuffer);
  end;

  AStream.ReadBuffer(FReadOnly, SizeOf(FReadOnly));

  if AFileVersion > 15 then
  begin
    AStream.ReadBuffer(FBorderSides, SizeOf(FBorderSides));
    AStream.ReadBuffer(FBorderWidth, SizeOf(FBorderWidth));
  end;
end;

procedure THCEditItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vBuffer: TBytes;
  vSize: Word;  // 最多65536个字节，如果超过65536，可使用写入文本后再写一个结束标识(如#9)，解析时遍历直到此标识
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  // 存Text
  vBuffer := BytesOf(FText);
  if System.Length(vBuffer) > MAXWORD then
    raise Exception.Create(HCS_EXCEPTION_TEXTOVER);
  vSize := System.Length(vBuffer);
  AStream.WriteBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
    AStream.WriteBuffer(vBuffer[0], vSize);

  AStream.WriteBuffer(FReadOnly, SizeOf(FReadOnly));

  AStream.WriteBuffer(FBorderSides, SizeOf(FBorderSides));
  AStream.WriteBuffer(FBorderWidth, SizeOf(FBorderWidth));
end;

procedure THCEditItem.SetText(const Value: string);
begin
  if (not FReadOnly) and (FText <> Value) then
  begin
    FText := Value;
    if FCaretOffset > System.Length(FText) then
      FCaretOffset := 0;

    OwnerData.Style.UpdateInfoRePaint;
  end;
end;

function THCEditItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := True;
end;

end.
