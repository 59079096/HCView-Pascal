{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{          文档ExpressItem(公式)对象实现单元            }
{                                                       }
{*******************************************************}

unit HCExpressItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCStyle, HCItem, HCRectItem, HCCustomData,
  HCCommon;

type
  TExpressArea = (ceaNone, ceaLeft, ceaTop, ceaRight, ceaBottom);

  // 公式
  TExperssItem = class(THCCustomRectItem)
  private
    FStyle: THCStyle;
    FLeftText, FTopText, FRightText, FBottomText: string;
    FLeftRect, FTopRect, FRightRect, FBottomRect: TRect;
    FPadding: Byte;
    FActiveArea: TExpressArea;
    FCaretOffset: ShortInt;
    FMouseLBDowning, FOutSelectInto: Boolean;
    function GetExpressArea(const X, Y: Integer): TExpressArea;
  protected
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary> 正在其上时内部是否处理指定的Key和Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure GetCaretInfo(var ACaretInfo: TCaretInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
  public
    constructor Create(const ALeftText, ATopText, ARightText, ABottomText: string);
  end;

implementation

uses
  SysUtils;

{ TExperssItem }

constructor TExperssItem.Create(const ALeftText, ATopText, ARightText, ABottomText: string);
begin
  inherited Create;
  Self.StyleNo := THCStyle.RsExpress;
  FPadding := 5;
  FActiveArea := TExpressArea.ceaNone;
  FCaretOffset := -1;

  FLeftText := ALeftText;
  FTopText := ATopText;
  FRightText := ARightText;
  FBottomText := ABottomText;
end;

procedure TExperssItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  AStyle.TextStyles[0].ApplyStyle(ACanvas);
  ACanvas.TextOut(ADrawRect.Left + FLeftRect.Left, ADrawRect.Top + FLeftRect.Top, FLeftText);
  ACanvas.TextOut(ADrawRect.Left + FTopRect.Left, ADrawRect.Top + FTopRect.Top, FTopText);
  ACanvas.TextOut(ADrawRect.Left + FRightRect.Left, ADrawRect.Top + FRightRect.Top, FRightText);
  ACanvas.TextOut(ADrawRect.Left + FBottomRect.Left, ADrawRect.Top + FBottomRect.Top, FBottomText);

  ACanvas.MoveTo(ADrawRect.Left + FLeftRect.Right + FPadding, ADrawRect.Top + FTopRect.Bottom + FPadding);
  ACanvas.LineTo(ADrawRect.Left + FRightRect.Left - FPadding, ADrawRect.Top + FTopRect.Bottom + FPadding);
end;

procedure TExperssItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vH, vLeftW, vRightW, vTopW, vBottomW: Integer;
begin
  FStyle := ARichData.Style;
  FStyle.TextStyles[0].ApplyStyle(FStyle.DefCanvas);
  vH := FStyle.DefCanvas.TextHeight('字');
  vLeftW := FStyle.DefCanvas.TextWidth(FLeftText);
  vTopW := FStyle.DefCanvas.TextWidth(FTopText);
  vRightW := FStyle.DefCanvas.TextWidth(FRightText);
  vBottomW := FStyle.DefCanvas.TextWidth(FBottomText);
  // 计算尺寸
  if vTopW > vBottomW then  // 上面比下面宽
    Width := vLeftW + vTopW + vRightW + 6 * FPadding
  else
    Width := vLeftW + vBottomW + vRightW + 6 * FPadding;

  Height := vH * 2 + 4 * FPadding;
  // 计算各字符串位置
  vH := FStyle.DefCanvas.TextHeight('字');
  //
  FLeftRect := Bounds(FPadding, (Height - vH) div 2, vLeftW, vH);
  FRightRect := Bounds(Width - FPadding - vRightW, (Height - vH) div 2, vRightW, vH);
  FTopRect := Bounds(FLeftRect.Right + FPadding + (FRightRect.Left - FPadding - (FLeftRect.Right + FPadding) - vTopW) div 2,
    FPadding, vTopW, vH);
  FBottomRect := Bounds(FLeftRect.Right + FPadding + (FRightRect.Left - FPadding - (FLeftRect.Right + FPadding) - vBottomW) div 2,
    Height - FPadding - vH, vBottomW, vH);
end;

procedure TExperssItem.GetCaretInfo(var ACaretInfo: TCaretInfo);
begin
  if FActiveArea <> TExpressArea.ceaNone then
  begin
    FStyle.TextStyles[0].ApplyStyle(FStyle.DefCanvas);
    case FActiveArea of
      ceaLeft:
        begin
          ACaretInfo.Height := FLeftRect.Bottom - FLeftRect.Top;
          ACaretInfo.X := FLeftRect.Left + FStyle.DefCanvas.TextWidth(Copy(FLeftText, 1, FCaretOffset));
          ACaretInfo.Y := FLeftRect.Top;
        end;

      ceaTop:
        begin
          ACaretInfo.Height := FTopRect.Bottom - FTopRect.Top;
          ACaretInfo.X := FTopRect.Left + FStyle.DefCanvas.TextWidth(Copy(FTopText, 1, FCaretOffset));
          ACaretInfo.Y := FTopRect.Top;
        end;

      ceaRight:
        begin
          ACaretInfo.Height := FRightRect.Bottom - FRightRect.Top;
          ACaretInfo.X := FRightRect.Left + FStyle.DefCanvas.TextWidth(Copy(FRightText, 1, FCaretOffset));
          ACaretInfo.Y := FRightRect.Top;
        end;

      ceaBottom:
        begin
          ACaretInfo.Height := FBottomRect.Bottom - FBottomRect.Top;
          ACaretInfo.X := FBottomRect.Left + FStyle.DefCanvas.TextWidth(Copy(FBottomText, 1, FCaretOffset));
          ACaretInfo.Y := FBottomRect.Top;
        end;
    end;
  end;
end;

function TExperssItem.GetExpressArea(const X, Y: Integer): TExpressArea;
var
  vPt: TPoint;
begin
  Result := TExpressArea.ceaNone;
  vPt := Point(X, Y);
  if PtInRect(FLeftRect, vPt) then
    Result := TExpressArea.ceaLeft
  else
  if PtInRect(FTopRect, vPt) then
    Result := TExpressArea.ceaTop
  else
  if PtInRect(FRightRect, vPt) then
    Result := TExpressArea.ceaRight
  else
  if PtInRect(FBottomRect, vPt) then
    Result := TExpressArea.ceaBottom;

  // 不在字符串所在的区域中
  if Result = TExpressArea.ceaNone then  // 没取到区域，可能是某元素字符串为空了
  begin
    if (X > FLeftRect.Right + FPadding) and (X < FRightRect.Left - FPadding) then
    begin
      if Y < FTopRect.Bottom then
        Result := TExpressArea.ceaTop
      else
      if Y > FBottomRect.Top then
        Result := TExpressArea.ceaBottom;
    end
    else
    if X < FLeftRect.Right then
      Result := TExpressArea.ceaLeft
    else
    if X > FRightRect.Left then
      Result := TExpressArea.ceaRight;
  end;
end;

function TExperssItem.GetOffsetAt(const X: Integer): Integer;
begin
  if FOutSelectInto then
  begin
    if X < Width div 2 then
      Result := OffsetBefor
    else
      Result := OffsetAfter;
  end
  else
    Result := inherited GetOffsetAt(X);
end;

procedure TExperssItem.KeyDown(var Key: Word; Shift: TShiftState);

  procedure BackspaceKeyDown;

    procedure BackDeleteChar(var S: string);
    begin
      if FCaretOffset > 0 then
      begin
        System.Delete(S, FCaretOffset, 1);
        Dec(FCaretOffset);
      end;
    end;

  begin
    case FActiveArea of
      ceaLeft: BackDeleteChar(FLeftText);
      ceaTop: BackDeleteChar(FTopText);
      ceaRight: BackDeleteChar(FRightText);
      ceaBottom: BackDeleteChar(FBottomText);
    end;

    Self.SizeChanged := True;
  end;

  procedure LeftKeyDown;
  begin
    if FCaretOffset > 0 then
      Dec(FCaretOffset);
  end;

  procedure RightKeyDown;
  var
    vS: string;
  begin
    case FActiveArea of
      ceaLeft: vS := FLeftText;
      ceaTop: vS := FTopText;
      ceaRight: vS := FRightText;
      ceaBottom: vS := FBottomText;
    end;
    if FCaretOffset < System.Length(vS) then
      Inc(FCaretOffset);
  end;

  procedure DeleteKeyDown;

    procedure DeleteChar(var S: string);
    begin
      if FCaretOffset < System.Length(S) then
        System.Delete(S, FCaretOffset + 1, 1);
    end;

  begin
    case FActiveArea of
      ceaLeft: DeleteChar(FLeftText);
      ceaTop: DeleteChar(FTopText);
      ceaRight: DeleteChar(FRightText);
      ceaBottom: DeleteChar(FBottomText);
    end;

    Self.SizeChanged := True;
  end;

  procedure HomeKeyDown;
  begin
    FCaretOffset := 0;
  end;

  procedure EndKeyDown;
  var
    vS: string;
  begin
    case FActiveArea of
      ceaLeft: vS := FLeftText;
      ceaTop: vS := FTopText;
      ceaRight: vS := FRightText;
      ceaBottom: vS := FBottomText;
    end;
    FCaretOffset := System.Length(vS);
  end;

begin
  case Key of
    VK_BACK: BackspaceKeyDown;  // 回删
    VK_LEFT: LeftKeyDown;       // 左方向键
    VK_RIGHT: RightKeyDown;     // 右方向键
    VK_DELETE: DeleteKeyDown;   // 删除键
    VK_HOME: HomeKeyDown;       // Home键
    VK_END: EndKeyDown;         // End键
  end;
end;

procedure TExperssItem.KeyPress(var Key: Char);
begin
  if FActiveArea <> ceaNone then
  begin
    Inc(FCaretOffset);
    case FActiveArea of
      ceaLeft: System.Insert(Key, FLeftText, FCaretOffset);
      ceaTop: System.Insert(Key, FTopText, FCaretOffset);
      ceaRight: System.Insert(Key, FRightText, FCaretOffset);
      ceaBottom: System.Insert(Key, FBottomText, FCaretOffset);
    end;

    Self.SizeChanged := True;
  end
  else
    Key := #0;
end;

procedure TExperssItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);

  procedure LoadPartText(var S: string);
  var
    vSize: Word;
    vBuffer: TBytes;
  begin
    AStream.ReadBuffer(vSize, SizeOf(vSize));
    if vSize > 0 then
    begin
      SetLength(vBuffer, vSize);
      AStream.Read(vBuffer[0], vSize);
      S := StringOf(vBuffer);
    end
    else
      S := '';
  end;

begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  LoadPartText(FLeftText);
  LoadPartText(FTopText);
  LoadPartText(FRightText);
  LoadPartText(FBottomText);
end;

procedure TExperssItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vS: string;
  vX: Integer;
  vArea: TExpressArea;
  vOffset: Integer;
begin
  inherited;
  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);
  FOutSelectInto := False;

  vArea := GetExpressArea(X, Y);
  if vArea <> FActiveArea then
  begin
    FActiveArea := vArea;
    FStyle.UpdateInfoReCaret;
  end;

  case FActiveArea of
    //ceaNone: ;
    ceaLeft:
      begin
        vS := FLeftText;
        vX := X - FLeftRect.Left;
      end;

    ceaTop:
      begin
        vS := FTopText;
        vX := X - FTopRect.Left;
      end;

    ceaRight:
      begin
        vS := FRightText;
        vX := X - FRightRect.Left;
      end;

    ceaBottom:
      begin
        vS := FBottomText;
        vX := X - FBottomRect.Left;
      end;
  end;
  if FActiveArea <> TExpressArea.ceaNone then
  begin
    FStyle.TextStyles[0].ApplyStyle(FStyle.DefCanvas);
    vOffset := GetCharOffsetByX(FStyle.DefCanvas, vS, vX)
  end
  else
    vOffset := -1;
  if vOffset <> FCaretOffset then
  begin
    FCaretOffset := vOffset;
    FStyle.UpdateInfoReCaret;
  end;
end;

procedure TExperssItem.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (not FMouseLBDowning) and (Shift = [ssLeft]) then
    FOutSelectInto := True;

  inherited MouseMove(Shift, X, Y);
end;

procedure TExperssItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FMouseLBDowning := False;
  FOutSelectInto := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TExperssItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);

  procedure SavePartText(const S: string);
  var
    vBuffer: TBytes;
    vSize: Word;
  begin
    vBuffer := BytesOf(S);
    if System.Length(vBuffer) > MAXWORD then
      raise Exception.Create(CFE_EXCEPTION + 'TextItem的内容超出最大字符数据！');
    vSize := System.Length(vBuffer);
    AStream.WriteBuffer(vSize, SizeOf(vSize));
    if vSize > 0 then
      AStream.WriteBuffer(vBuffer[0], vSize);
  end;

begin
  inherited SaveToStream(AStream, AStart, AEnd);
  SavePartText(FLeftText);
  SavePartText(FTopText);
  SavePartText(FRightText);
  SavePartText(FBottomText);
end;

function TExperssItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := True;
end;

end.
