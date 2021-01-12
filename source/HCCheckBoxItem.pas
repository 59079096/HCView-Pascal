{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{        文档CheckBoxItem(勾选框)对象实现单元           }
{                                                       }
{*******************************************************}

unit HCCheckBoxItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, HCItem, HCRectItem, HCStyle,
  HCCustomData, HCCommon, HCXml;

type
  THCCheckBoxItem = class(THCControlItem)
  private
    FText: string;
    FChecked, FItemHit: Boolean;
    function GetBoxRect: TRect;
    procedure SetChecked(const Value: Boolean);
  protected
    procedure DoSetChecked(const Value: Boolean); virtual;
    //
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string; const AChecked: Boolean); virtual;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property Checked: Boolean read FChecked write SetChecked;
  end;

implementation

uses
  Math;

const
  CheckBoxSize = 14;

{ THCCheckBoxItem }

function THCCheckBoxItem.GetBoxRect: TRect;
begin
  Result := Classes.Bounds(FPaddingLeft, (Height - CheckBoxSize) div 2, CheckBoxSize, CheckBoxSize);
end;

function THCCheckBoxItem.GetText: string;
begin
  Result := FText;
end;

procedure THCCheckBoxItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FChecked := (Source as THCCheckBoxItem).Checked;  // 勾选状态
  FText := (Source as THCCheckBoxItem).Text;
end;

constructor THCCheckBoxItem.Create(const AOwnerData: THCCustomData; const AText: string;
  const AChecked: Boolean);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.CheckBox;
  FChecked := AChecked;
  FText := AText;
  FItemHit := False;
  FPaddingLeft := 2;
end;

procedure THCCheckBoxItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vBoxRect: TRect;
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  vBoxRect := GetBoxRect;
  OffsetRect(vBoxRect, ADrawRect.Left, ADrawRect.Top);

  if APaintInfo.Print then
  begin
    if FChecked then
      HCDrawFrameControl(ACanvas, vBoxRect, THCControlState.hcsChecked, THCControlStyle.hcyCheck)
    else
      HCDrawFrameControl(ACanvas, vBoxRect, THCControlState.hcsCustom, THCControlStyle.hcyCheck);
  end
  else
  begin
    if Self.IsSelectComplate then
    begin
      ACanvas.Brush.Color := AStyle.SelColor;
      ACanvas.FillRect(ADrawRect);
    end
    else
    if FMouseIn then  // 鼠标在其中
    begin
      ACanvas.Brush.Color := clBtnFace;
      ACanvas.FillRect(ADrawRect);
    end;

    if FChecked then
      DrawFrameControl(ACanvas.Handle, vBoxRect, DFC_MENU, DFCS_CHECKED or DFCS_MENUCHECK);

    HCDrawFrameControl(ACanvas, vBoxRect, THCControlState.hcsCustom, THCControlStyle.hcyCheck);
  end;

  ACanvas.Brush.Style := bsClear;
  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);
  ACanvas.TextOut(ADrawRect.Left + FPaddingLeft + CheckBoxSize + FPaddingLeft,
    ADrawRect.Top + (Height - ACanvas.TextHeight('H')) div 2, FText);
end;

procedure THCCheckBoxItem.DoSetChecked(const Value: Boolean);
begin
  SetChecked(Value);
end;

procedure THCCheckBoxItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vSize: TSize;
begin
  if Self.AutoSize then
  begin
    ARichData.Style.ApplyTempStyle(TextStyleNo);
    vSize := ARichData.Style.TempCanvas.TextExtent(FText);
    Width := FPaddingLeft + CheckBoxSize + FPaddingLeft + vSize.cx;  // 间距
    Height := Max(vSize.cy, CheckBoxSize);
  end;

  if Width < FMinWidth then
    Width := FMinWidth;

  if Height < FMinHeight then
    Height := FMinHeight;
end;

function THCCheckBoxItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  //if PtInRect(GetBoxRect, Point(X, Y)) then
  GCursor := crArrow;
end;

function THCCheckBoxItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vSize: TSize;
begin
  Result := inherited MouseUp(Button, Shift, X, Y);
  if OwnerData.CanEdit and not OwnerData.Style.UpdateInfo.Selecting then
  begin
    if FItemHit then  // 文本就命中
    begin
      Self.OwnerData.Style.ApplyTempStyle(TextStyleNo);
      vSize := Self.OwnerData.Style.TempCanvas.TextExtent(FText);
      if PtInRect(Classes.Bounds(FPaddingLeft, 0, FPaddingLeft + CheckBoxSize + FPaddingLeft + vSize.cx, vSize.cy),
        Point(X, Y))
      then
        DoSetChecked(not FChecked);
    end
    else
    if PtInRect(GetBoxRect, Point(X, Y)) then  // 点在了勾选框中
      DoSetChecked(not FChecked);
  end;
end;

procedure THCCheckBoxItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FChecked := ANode.Attributes['check'];
  FText := ANode.Text;
end;

procedure THCCheckBoxItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FChecked, SizeOf(FChecked));  // 读勾选状态
  HCLoadTextFromStream(AStream, FText, AFileVersion);  // 读Text
end;

procedure THCCheckBoxItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  AStream.WriteBuffer(FChecked, SizeOf(FChecked));  // 存勾选状态
  HCSaveTextToStream(AStream, FText);
end;

procedure THCCheckBoxItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Self.DoChange;
  end;
end;

procedure THCCheckBoxItem.SetText(const Value: string);
begin
  FText := Value;
end;

procedure THCCheckBoxItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['check'] := FChecked;  // 存勾选状态
  ANode.Text := FText;
end;

end.
