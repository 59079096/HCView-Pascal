{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-9-29             }
{                                                       }
{       文档SupSubScript(同时上下标)对象实现单元        }
{                                                       }
{*******************************************************}

unit HCSupSubScriptItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCStyle, HCTextStyle, HCItem, HCRectItem,
  HCCustomData, HCCommon, HCXml;

type
  THCSupSubScriptItem = class(THCTextRectItem)  // 分数(上、下文本，分数线)
  private
    FSupText, FSubText: string;
    FSupRect, FSubRect: TRect;
    FCaretOffset, FPadding: ShortInt;
    FMouseLBDowning, FOutSelectInto: Boolean;
    FActiveArea, FMouseMoveArea: TExpressArea;
  protected
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure SetActive(const Value: Boolean); override;
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
    function GetExpressArea(const X, Y: Integer): TExpressArea; virtual;
    property SupRect: TRect read FSupRect write FSupRect;
    property SubRect: TRect read FSubRect write FSubRect;
  public
    constructor Create(const AOwnerData: THCCustomData; const ASupText, ASubText: string);
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property SupText: string read FSupText write FSupText;
    property SubText: string read FSubText write FSubText;
  end;

implementation

uses
  SysUtils, Math;

{ THCSupSubScriptItem }

procedure ApplySupSubStyle(const ATextStyle: THCTextStyle; const ACanvas: TCanvas; const AScale: Single = 1);
var
  vFont: TFont;
  vLogFont: TLogFont;
begin
  with ACanvas do
  begin
    if ATextStyle.BackColor = HCTransparentColor then
      Brush.Style := bsClear
    else
      Brush.Color := ATextStyle.BackColor;

    Font.Color := ATextStyle.Color;
    Font.Name := ATextStyle.Family;
    Font.Size := Round(ATextStyle.Size * 2 / 3);
    if tsBold in ATextStyle.FontStyles then
      Font.Style := Font.Style + [TFontStyle.fsBold]
    else
      Font.Style := Font.Style - [TFontStyle.fsBold];

    if tsItalic in ATextStyle.FontStyles then
      Font.Style := Font.Style + [TFontStyle.fsItalic]
    else
      Font.Style := Font.Style - [TFontStyle.fsItalic];

    if tsUnderline in ATextStyle.FontStyles then
      Font.Style := Font.Style + [TFontStyle.fsUnderline]
    else
      Font.Style := Font.Style - [TFontStyle.fsUnderline];

    if tsStrikeOut in ATextStyle.FontStyles then
      Font.Style := Font.Style + [TFontStyle.fsStrikeOut]
    else
      Font.Style := Font.Style - [TFontStyle.fsStrikeOut];

    //if AScale <> 1 then
    begin
      vFont := TFont.Create;
      try
        vFont.Assign(ACanvas.Font);
        GetObject(vFont.Handle, SizeOf(vLogFont), @vLogFont);
        if vLogFont.lfHeight < 0 then
          vLogFont.lfHeight := -Round(ATextStyle.Size * 2 / 3 * GetDeviceCaps(ACanvas.Handle, LOGPIXELSY) / 72 / AScale)
        else
          vLogFont.lfHeight := Round(ATextStyle.Size * 2 / 3 * GetDeviceCaps(ACanvas.Handle, LOGPIXELSY) / 72 / AScale);

        vFont.Handle := CreateFontIndirect(vLogFont);
        ACanvas.Font.Assign(vFont);
      finally
        vFont.Free;
      end;
    end;
  end;
end;

procedure THCSupSubScriptItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FSupText := (Source as THCSupSubScriptItem).SupText;
  FSubText := (Source as THCSupSubScriptItem).SubText;
end;

constructor THCSupSubScriptItem.Create(const AOwnerData: THCCustomData;
  const ASupText, ASubText: string);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.SupSubScript;
  FPadding := 1;
  FActiveArea := TExpressArea.ceaNone;
  FCaretOffset := -1;

  FSupText := ASupText;
  FSubText := ASubText;
end;

procedure THCSupSubScriptItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vFocusRect: TRect;
begin
  if Self.Active and (not APaintInfo.Print) then
  begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FillRect(ADrawRect);
  end;

  if not APaintInfo.Print then
  begin
    if FActiveArea <> ceaNone then
    begin
      case FActiveArea of
        ceaTop: vFocusRect := FSupRect;
        ceaBottom: vFocusRect := FSubRect;
      end;

      vFocusRect.Offset(ADrawRect.Location);
      vFocusRect.Inflate(2, 2);
      ACanvas.Pen.Color := clBlue;
      ACanvas.Rectangle(vFocusRect);
    end;

    if (FMouseMoveArea <> ceaNone) and (FMouseMoveArea <> FActiveArea) then
    begin
      case FMouseMoveArea of
        ceaTop: vFocusRect := FSupRect;
        ceaBottom: vFocusRect := FSubRect;
      end;

      vFocusRect.Offset(ADrawRect.Location);
      vFocusRect.Inflate(2, 2);
      ACanvas.Pen.Color := clMedGray;
      ACanvas.Rectangle(vFocusRect);
    end;
  end;

  ApplySupSubStyle(AStyle.TextStyles[TextStyleNo], ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);
  ACanvas.TextOut(ADrawRect.Left + FSupRect.Left, ADrawRect.Top + FSupRect.Top, FSupText);
  ACanvas.TextOut(ADrawRect.Left + FSubRect.Left, ADrawRect.Top + FSubRect.Top, FSubText);
end;

procedure THCSupSubScriptItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vH, vTopW, vBottomW: Integer;
  vStyle: THCStyle;
begin
  vStyle := ARichData.Style;
  ApplySupSubStyle(vStyle.TextStyles[TextStyleNo], vStyle.TempCanvas);
  vH := vStyle.TempCanvas.TextHeight('H');
  vTopW := Max(vStyle.TempCanvas.TextWidth(FSupText), FPadding);
  vBottomW := Max(vStyle.TempCanvas.TextWidth(FSubText), FPadding);
  vStyle.ApplyTempStyle(THCStyle.Null);
  // 计算尺寸
  if vTopW > vBottomW then  // 上面比下面宽
    Width := vTopW + 4 * FPadding
  else
    Width := vBottomW + 4 * FPadding;

  Height := vH * 2 + 4 * FPadding;

  // 计算各字符串位置
  FSupRect := Bounds(FPadding, FPadding, vTopW, vH);
  FSubRect := Bounds(FPadding, Height - FPadding - vH, vBottomW, vH);
end;

procedure THCSupSubScriptItem.GetCaretInfo(var ACaretInfo: THCCaretInfo);
begin
  if FActiveArea <> TExpressArea.ceaNone then
  begin
    ApplySupSubStyle(OwnerData.Style.TextStyles[TextStyleNo], OwnerData.Style.TempCanvas);
    case FActiveArea of
      ceaTop:
        begin
          ACaretInfo.Height := FSupRect.Bottom - FSupRect.Top;
          ACaretInfo.X := FSupRect.Left + OwnerData.Style.TempCanvas.TextWidth(Copy(FSupText, 1, FCaretOffset));
          ACaretInfo.Y := FSupRect.Top;
        end;
      ceaBottom:
        begin
          ACaretInfo.Height := FSubRect.Bottom - FSubRect.Top;
          ACaretInfo.X := FSubRect.Left + OwnerData.Style.TempCanvas.TextWidth(Copy(FSubText, 1, FCaretOffset));
          ACaretInfo.Y := FSubRect.Top;
        end;
    end;
  end
  else
    ACaretInfo.Visible := False;
end;

function THCSupSubScriptItem.GetExpressArea(const X, Y: Integer): TExpressArea;
var
  vPt: TPoint;
begin
  Result := TExpressArea.ceaNone;
  vPt := Point(X, Y);
  if PtInRect(FSupRect, vPt) then
    Result := TExpressArea.ceaTop
  else
  if PtInRect(FSubRect, vPt) then
    Result := TExpressArea.ceaBottom;
end;

function THCSupSubScriptItem.GetOffsetAt(const X: Integer): Integer;
begin
  if FOutSelectInto then
    Result := inherited GetOffsetAt(X)
  else
  begin
    if X <= 0 then
      Result := OffsetBefor
    else
    if X >= Width then
      Result := OffsetAfter
    else
      Result := OffsetInner;
  end;
end;

function THCSupSubScriptItem.InsertText(const AText: string): Boolean;
begin
  if FActiveArea <> ceaNone then
  begin
    case FActiveArea of
      ceaTop: System.Insert(AText, FSupText, FCaretOffset + 1);
      ceaBottom: System.Insert(AText, FSubText, FCaretOffset + 1);
    end;
    Inc(FCaretOffset, System.Length(AText));

    Self.SizeChanged := True;
    Result := True;
  end
  else
    Result := False;
end;

procedure THCSupSubScriptItem.KeyDown(var Key: Word; Shift: TShiftState);

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
      ceaTop: BackDeleteChar(FSupText);
      ceaBottom: BackDeleteChar(FSubText);
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
      ceaTop: vS := FSupText;
      ceaBottom: vS := FSubText;
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
      ceaTop: DeleteChar(FSupText);
      ceaBottom: DeleteChar(FSubText);
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
      ceaTop: vS := FSupText;
      ceaBottom: vS := FSubText;
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

procedure THCSupSubScriptItem.KeyPress(var Key: Char);
begin
  if FActiveArea <> ceaNone then
    InsertText(Key)
  else
    Key := #0;
end;

procedure THCSupSubScriptItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FSupText);
  HCLoadTextFromStream(AStream, FSubText);
end;

procedure THCSupSubScriptItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vS: string;
  vX: Integer;
  vOffset: Integer;
begin
  inherited;
  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);
  FOutSelectInto := False;

  if FMouseMoveArea <> FActiveArea then
  begin
    FActiveArea := FMouseMoveArea;
    OwnerData.Style.UpdateInfoReCaret;
  end;

  case FActiveArea of
    //ceaNone: ;

    ceaTop:
      begin
        vS := FSupText;
        vX := X - FSupRect.Left;
      end;

    ceaBottom:
      begin
        vS := FSubText;
        vX := X - FSubRect.Left;
      end;
  end;

  if FActiveArea <> TExpressArea.ceaNone then
  begin
    ApplySupSubStyle(OwnerData.Style.TextStyles[TextStyleNo], OwnerData.Style.TempCanvas);
    vOffset := GetCharOffsetAt(OwnerData.Style.TempCanvas, vS, vX);
  end
  else
    vOffset := -1;

  if vOffset <> FCaretOffset then
  begin
    FCaretOffset := vOffset;
    OwnerData.Style.UpdateInfoReCaret;
  end;
end;

procedure THCSupSubScriptItem.MouseLeave;
begin
  inherited MouseLeave;
  FMouseMoveArea := ceaNone;
end;

procedure THCSupSubScriptItem.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vArea: TExpressArea;
begin
  if (not FMouseLBDowning) and (Shift = [ssLeft]) then
    FOutSelectInto := True;

  if not FOutSelectInto then
  begin
    vArea := GetExpressArea(X, Y);
    if vArea <> FMouseMoveArea then
    begin
      FMouseMoveArea := vArea;
      OwnerData.Style.UpdateInfoRePaint;
    end;
  end
  else
    FMouseMoveArea := ceaNone;

  inherited MouseMove(Shift, X, Y);
end;

procedure THCSupSubScriptItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FMouseLBDowning := False;
  FOutSelectInto := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure THCSupSubScriptItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FSupText := ANode.Attributes['sup'];
  FSubText := ANode.Attributes['sub'];
end;

procedure THCSupSubScriptItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FSupText);
  HCSaveTextToStream(AStream, FSubText);
end;

procedure THCSupSubScriptItem.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Value then
    FActiveArea := ceaNone;
end;

procedure THCSupSubScriptItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['sup'] := FSupText;
  ANode.Attributes['sub'] := FSubText;
end;

function THCSupSubScriptItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := True;
end;

end.
