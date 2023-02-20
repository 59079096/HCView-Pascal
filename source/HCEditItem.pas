{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-7-9              }
{                                                       }
{          �ĵ�EditItem(�ı���)����ʵ�ֵ�Ԫ             }
{                                                       }
{*******************************************************}

unit HCEditItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, HCItem, HCRectItem, HCStyle,
  HCCustomData, HCRichData, HCCommon, HCXml;

const
  BTNWIDTH = 16;
  BTNMARGIN = 1;

type
  THCEditItem = class(THCControlItem)
  private
    FText: string;
    FTextAlignHorz: THCTextHorAlign;
    FBorderWidth: Byte;
    FBorderSides: TBorderSides;
    FReadOnly, FPrintOnlyText: Boolean;
    FCaretOffset, FSelEnd,
    FSelMove,  // to do: shift + �����ѡ��
    FLeftOffset  // 0λ���������FMargin
      : Integer;
    procedure CalcTextSize;
    procedure CalcSize;
    procedure ScrollAdjust(const AOffset: Integer);
    function GetCharDrawLeft(const AOffset: Integer): Integer;
    function OffsetInSelect(const AOffset: Integer): Boolean;
    procedure DeleteSelectText;
    procedure DisSelectText;
  protected
    FTextSize: TSize;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure SetActive(const Value: Boolean); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    /// <summary> ��������ʱ�ڲ��Ƿ���ָ����Key��Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure SetTextAlignHorz(const Value: THCTextHorAlign);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); virtual;
    function SelectTextExists: Boolean;
    /// <summary> ��ȡ����X��Y�Ƿ���ѡ�������� </summary>
    function CoordInSelect(const X, Y: Integer): Boolean; override;
    function SelectExists: Boolean; override;
    function IsSelectComplateTheory: Boolean; override;
    function DeleteSelected: Boolean; override;
    procedure DisSelect; override;
    function SaveSelectToText: string; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure Clear; override;
    procedure ReFormatRequest; override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;
    function InsertText(const AText: string): Boolean; override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property PrintOnlyText: Boolean read FPrintOnlyText write FPrintOnlyText;
    property BorderSides: TBorderSides read FBorderSides write FBorderSides;
    property BorderWidth: Byte read FBorderWidth write FBorderWidth;
    property TextAlignHorz: THCTextHorAlign read FTextAlignHorz write SetTextAlignHorz;
  end;

implementation

{$I HCView.inc}

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

procedure THCEditItem.CalcSize;
begin
  CalcTextSize;

  if Self.AutoSize then
  begin
    Width := FPaddingLeft + FTextSize.cx + FPaddingRight;  // ���
    Height := FPaddingTop + FTextSize.cy + FPaddingBottom;
  end;

  if Width < FMinWidth then
    Width := FMinWidth;

  if Height < FMinHeight then
    Height := FMinHeight;
end;

procedure THCEditItem.CalcTextSize;
begin
  OwnerData.Style.ApplyTempStyle(TextStyleNo);
  if FText <> '' then
    FTextSize := OwnerData.Style.TempCanvas.TextExtent(FText)
  else
    FTextSize := OwnerData.Style.TempCanvas.TextExtent('H');
end;

procedure THCEditItem.Clear;
begin
  Self.Text := '';
end;

function THCEditItem.CoordInSelect(const X, Y: Integer): Boolean;
begin
  Result := SelectExists and PtInRect(Bounds(0, 0, Width, Height), Point(X, Y));
end;

constructor THCEditItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.Edit;
  FText := AText;
  FPaddingLeft := 4;
  FPaddingRight := 4;
  FPaddingTop := 4;
  FPaddingBottom := 4;
  FLeftOffset := 0;
  FCaretOffset := -1;
  FSelEnd := -1;
  FSelMove := -1;
  Width := 50;
  FTextAlignHorz := hthaLeft;
  FPrintOnlyText := False;
  FBorderWidth := 1;
  FBorderSides := [cbsLeft, cbsTop, cbsRight, cbsBottom];
  CalcSize;
end;

function THCEditItem.DeleteSelected: Boolean;
begin
  Result := inherited DeleteSelected;
  if SelectTextExists then
  begin
    DeleteSelectText;
    Result := True;
  end;
end;

procedure THCEditItem.DeleteSelectText;
begin
  System.Delete(FText, FCaretOffset + 1, FSelEnd - FCaretOffset);
  FSelEnd := -1;
  FSelMove := FCaretOffset;
  CalcTextSize;
end;

procedure THCEditItem.DisSelect;
begin
  DisSelectText;
  inherited DisSelect;
end;

procedure THCEditItem.DisSelectText;
begin
  FSelMove := FCaretOffset;
  if Self.SelectTextExists then
    FSelEnd := -1;
end;

procedure THCEditItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vLeft, vRight: Integer;
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then
  begin
    if FReadOnly then
    begin
      ACanvas.Brush.Color := clBtnFace;
      ACanvas.FillRect(ADrawRect);
    end
    else
    if Self.IsSelectComplate then
    begin
      ACanvas.Brush.Color := AStyle.SelColor;
      ACanvas.FillRect(ADrawRect);
    end;

    if SelectTextExists then
    begin
      ACanvas.Brush.Color := AStyle.SelColor;
      vLeft := GetCharDrawLeft(FCaretOffset);
      vRight := GetCharDrawLeft(FSelEnd);
      vLeft := Max(0, Min(vLeft, Width));
      vRight := Max(0, Min(vRight, Width));
      ACanvas.FillRect(Rect(ADrawRect.Left + vLeft, ADrawRect.Top, ADrawRect.Left + vRight, ADrawRect.Bottom));
    end;
  end;

  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);

  if (not Self.AutoSize) and (not APaintInfo.Print) then
    ACanvas.TextRect(ADrawRect, ADrawRect.Left + FPaddingLeft - FLeftOffset, ADrawRect.Top + FPaddingTop, FText)
  else
    ACanvas.TextOut(ADrawRect.Left + FPaddingLeft, ADrawRect.Top + FPaddingTop, FText);

  if APaintInfo.Print and FPrintOnlyText then Exit;

  if FBorderSides <> [] then // �Ǵ�ӡ
  begin
    if (not APaintInfo.Print) and (FMouseIn or Active) then
      ACanvas.Pen.Color := clBlue
    else  // ��겻�����л��ӡ
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
end;

procedure THCEditItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
begin
  CalcSize;
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

  if SelectTextExists then
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
    ACaretInfo.X := FPaddingLeft - FLeftOffset + vSize.cx;// + (Width - FMargin - OwnerData.Style.DefCanvas.TextWidth(FText) - FMargin) div 2;
  end
  else
  begin
    ACaretInfo.Height := OwnerData.Style.TextStyles[TextStyleNo].FontHeight
      + OwnerData.Style.TextStyles[TextStyleNo].TextMetric.tmExternalLeading;
    ACaretInfo.X := FPaddingLeft;// - FLeftOffset;// + (Width - FMargin - OwnerData.Style.DefCanvas.TextWidth(FText) - FMargin) div 2;
  end;

  ACaretInfo.Y := FPaddingTop;

  if (not Self.AutoSize) and (ACaretInfo.X > Width) then
    ACaretInfo.Visible := False;
end;

function THCEditItem.GetCharDrawLeft(const AOffset: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  if AOffset > 0 then
  begin
    if AOffset = System.Length(FText) then
      Result := Width
    else
    begin
      OwnerData.Style.ApplyTempStyle(TextStyleNo);
      Result := FPaddingLeft + OwnerData.Style.TempCanvas.TextWidth(Copy(FText, 1, AOffset)) - FLeftOffset;
    end;
  end;
end;

function THCEditItem.GetOffsetAt(const X: Integer): Integer;
begin
  if X <= FPaddingLeft then
    Result := OffsetBefor
  else
  if X >= Width - FPaddingRight then
    Result := OffsetAfter
  else
    Result := OffsetInner;
end;

function THCEditItem.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
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
  ScrollAdjust(FCaretOffset);
  Self.FormatDirty;
end;

function THCEditItem.IsSelectComplateTheory: Boolean;
begin
  Result := IsSelectComplate;
end;

procedure THCEditItem.KeyDown(var Key: Word; Shift: TShiftState);

  procedure BackspaceKeyDown;
  begin
    if SelectTextExists then
      DeleteSelectText
    else
    if FCaretOffset > 0 then
    begin
      System.Delete(FText, FCaretOffset, 1);
      Dec(FCaretOffset);
      //CalcTextSize;
    end;

    ScrollAdjust(FCaretOffset);
    Self.FormatDirty;
  end;

  procedure LeftKeyDown;
  begin
    DisSelectText;
    if FCaretOffset > 0 then
      Dec(FCaretOffset);

    ScrollAdjust(FCaretOffset);
    OwnerData.Style.UpdateInfoRePaint;
  end;

  procedure RightKeyDown;
  begin
    DisSelectText;
    if FCaretOffset < System.Length(FText) then
      Inc(FCaretOffset);

    ScrollAdjust(FCaretOffset);
    OwnerData.Style.UpdateInfoRePaint;
  end;

  procedure DeleteKeyDown;
  begin
    if SelectTextExists then
      DeleteSelectText
    else
    if FCaretOffset < System.Length(FText) then
    begin
      System.Delete(FText, FCaretOffset + 1, 1);
      //CalcTextSize;
    end;

    ScrollAdjust(FCaretOffset);
    Self.FormatDirty;
  end;

begin
  if Self.Enabled and not FReadOnly then
  begin
    case Key of
      VK_BACK: BackspaceKeyDown;  // ��ɾ
      VK_LEFT: LeftKeyDown;       // �����
      VK_RIGHT: RightKeyDown;     // �ҷ����
      VK_DELETE: DeleteKeyDown;   // ɾ����
      VK_HOME:
        begin
          FCaretOffset := 0;  // Home��
          ScrollAdjust(FCaretOffset);
        end;
      VK_END:
        begin
          FCaretOffset := System.Length(FText);  // End��
          ScrollAdjust(FCaretOffset);
        end;
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure THCEditItem.KeyPress(var Key: Char);
begin
  if Self.Enabled and not FReadOnly then
  begin
    if SelectTextExists then
      DeleteSelectText;

    Inc(FCaretOffset);
    System.Insert(Key, FText, FCaretOffset);
    ScrollAdjust(FCaretOffset);
    Self.FormatDirty;
  end;

  inherited KeyPress(Key);
end;

function THCEditItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vOffset: Integer;
begin
  Result := inherited MouseDown(Button, Shift, X, Y);
  if not Self.Active then Exit;

  OwnerData.Style.ApplyTempStyle(TextStyleNo);
  vOffset := GetNorAlignCharOffsetAt(OwnerData.Style.TempCanvas, FText, X - FPaddingLeft + FLeftOffset);
  if Button = mbLeft then
    DisSelectText
  else
  begin
    if not OffsetInSelect(vOffset) then
      DisSelectText
    else
      Exit;
  end;

  if vOffset <> FCaretOffset then
  begin
    FCaretOffset := vOffset;
    FSelMove := vOffset;
    ScrollAdjust(vOffset);
    OwnerData.Style.UpdateInfoReCaret;
  end;
end;

function THCEditItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  if ssLeft in Shift then
  begin
    if X < 0 then
      FLeftOffset := Max(0, FLeftOffset - OwnerData.Style.TextStyles[TextStyleNo].TextMetric.tmAveCharWidth)
    else
    if X > Width - FPaddingRight then
      FLeftOffset := Max(0, Min(FTextSize.cx - Width + FPaddingRight, FLeftOffset + OwnerData.Style.TextStyles[TextStyleNo].TextMetric.tmAveCharWidth));

    FSelEnd := GetNorAlignCharOffsetAt(OwnerData.Style.TempCanvas, FText, X - FPaddingLeft + FLeftOffset);
    FSelMove := FSelEnd;
    if (not SelectTextExists) and (FSelEnd >= 0) then  // �ص�ͬһλ��
    begin
      FSelEnd := -1;
      FSelMove := FCaretOffset;
    end;

    ScrollAdjust(FSelMove);
  end;
end;

function THCEditItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vSel: ShortInt;
begin
  if (Button = mbLeft) and (FSelEnd >= 0) and (FSelEnd < FCaretOffset) then
  begin
    vSel := FCaretOffset;
    FCaretOffset := FSelEnd;
    FSelEnd := vSel;
  end;

  if OwnerData.Style.UpdateInfo.DragingSelected then
    Self.DisSelect;

  Result := inherited MouseUp(Button, Shift, X, Y);
end;

function THCEditItem.OffsetInSelect(const AOffset: Integer): Boolean;
begin
  Result := (AOffset >= FCaretOffset) and (AOffset <= FSelEnd);
end;

procedure THCEditItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FReadOnly := ANode.Attributes['readonly'];
  FPrintOnlyText := ANode.Attributes['printonlytext'];
  SetBorderSideByPro(ANode.Attributes['border'], FBorderSides);
  FBorderWidth := ANode.Attributes['borderwidth'];
  FTextAlignHorz := ANode.Attributes['textalignhorz'];
  FText := ANode.Text;
end;

procedure THCEditItem.ReFormatRequest;
begin
  inherited ReFormatRequest;
end;

procedure THCEditItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FText, AFileVersion);  // ��ȡText

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

  if AFileVersion > 56 then
    AStream.ReadBuffer(FTextAlignHorz, SizeOf(THCTextHorAlign));
end;

function THCEditItem.SaveSelectToText: string;
begin
  if SelectTextExists then
    Result := Copy(FText, FCaretOffset + 1, FSelEnd - FCaretOffset)
  else
  if Self.IsSelectComplate then
    Result := FText
  else
    Result := inherited SaveSelectToText;
end;

procedure THCEditItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FText); // ��Text

  vByte := 0;
  if FReadOnly then
    vByte := vByte or (1 shl 7);

  if FPrintOnlyText then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  AStream.WriteBuffer(FBorderSides, SizeOf(FBorderSides));
  AStream.WriteBuffer(FBorderWidth, SizeOf(FBorderWidth));
  AStream.WriteBuffer(FTextAlignHorz, SizeOf(THCTextHorAlign));
end;

procedure THCEditItem.ScrollAdjust(const AOffset: Integer);
var
  vText: string;
  vRight: Integer;
begin
  if Self.AutoSize then
  begin
    FLeftOffset := 0;
    Exit;
  end;

  if FTextSize.cx + FPaddingLeft <= Width - FPaddingRight then
  begin
    FLeftOffset := 0;
    Exit;
  end;

  if FTextSize.cx + FPaddingLeft - FLeftOffset < Width - FPaddingRight then
  begin
    FLeftOffset := FLeftOffset - (Width - FPaddingLeft - FTextSize.cx + FLeftOffset - FPaddingRight);
    Exit;
  end;

  OwnerData.Style.ApplyTempStyle(TextStyleNo);
  vText := Copy(FText, 1, AOffset);
  vRight := OwnerData.Style.TempCanvas.TextWidth(vText) + FPaddingLeft - FLeftOffset;
  if vRight > Width - FPaddingRight then
    FLeftOffset := FLeftOffset + vRight - Width + FPaddingRight
  else
  if vRight < 0 then
    FLeftOffset := FLeftOffset + vRight;
end;

function THCEditItem.SelectExists: Boolean;
begin
  Result := inherited SelectExists or SelectTextExists;
end;

function THCEditItem.SelectTextExists: Boolean;
begin
  Result := (FSelEnd >= 0) and (FSelEnd <> FCaretOffset);
end;

procedure THCEditItem.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Value then
  begin
    DisSelectText;
    FLeftOffset := 0;
    FCaretOffset := -1;
  end;
end;

procedure THCEditItem.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
    FReadOnly := Value;
end;

procedure THCEditItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    if FCaretOffset > System.Length(FText) then
      FCaretOffset := 0;

    if Self.AutoSize then
      (OwnerData as THCRichData).ItemReFormatRequest(Self)
    else
      OwnerData.Style.UpdateInfoRePaint;
  end;
end;

procedure THCEditItem.SetTextAlignHorz(const Value: THCTextHorAlign);
begin
  if FTextAlignHorz <> Value then
  begin
    FTextAlignHorz := Value;
    ReFormatRequest;
  end;
end;

procedure THCEditItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['readonly'] := FReadOnly;
  ANode.Attributes['printonlytext'] := FPrintOnlyText;
  ANode.Attributes['border'] := GetBorderSidePro(FBorderSides);
  ANode.Attributes['borderwidth'] := FBorderWidth;
  ANode.Attributes['textalignhorz'] := FTextAlignHorz;
  ANode.Text := FText;
end;

function THCEditItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := False;

  if Key = VK_LEFT then
  begin
    if FCaretOffset = 0 then  // �����ٴ������Ƴ�
      FCaretOffset := -1
    else
    if FCaretOffset < 0 then  // �����������
    begin
      FCaretOffset := System.Length(FText);
      ScrollAdjust(FCaretOffset);
      OwnerData.Style.UpdateInfoRePaint;
      Result := True;
    end
    else  // > 0
      Result := True;
  end
  else
  if Key = VK_RIGHT then
  begin
    if FCaretOffset = System.Length(FText) then  // �����ٴ����ң��Ƴ�
      FCaretOffset := -1
    else
    if FCaretOffset < 0 then  // �����Ҽ�����
    begin
      FCaretOffset := 0;
      ScrollAdjust(FCaretOffset);
      OwnerData.Style.UpdateInfoRePaint;
      Result := True;
    end
    else  // < Length
      Result := True;
  end
  else
    Result := True;
end;

end.
