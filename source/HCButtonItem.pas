{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{           文档ButtonItem(按钮)对象实现单元            }
{                                                       }
{*******************************************************}

unit HCButtonItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, HCItem, HCRectItem, HCStyle,
  HCCustomData, HCCommon, HCXml;

type
  THCButtonItem = class(THCControlItem)
  private
    FText: string;
    FDown: Boolean;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure MouseLeave; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); virtual;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
  end;

implementation

{ THCButtonItem }

function THCButtonItem.GetText: string;
begin
  Result := FText;
end;

procedure THCButtonItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FText := (Source as THCButtonItem).Text;
end;

constructor THCButtonItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.Button;
  FText := AText;
  FDown := False;
end;

procedure THCButtonItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vSize: TSize;
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  //if not APaintInfo.Print then  // 暂时不处理打印时候的样式
  begin
    if Self.IsSelectComplate then
      ACanvas.Brush.Color := AStyle.SelColor
    else
    if Self.FDown then
      ACanvas.Brush.Color := clHighlight
    else
    if FMouseIn then  // 鼠标在其中
      ACanvas.Brush.Color := clBtnFace
    else
      ACanvas.Brush.Color := clMedGray;

    ACanvas.FillRect(ADrawRect);
  end;

  ACanvas.Brush.Style := bsClear;
  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);
  vSize := ACanvas.TextExtent(FText);
  ACanvas.TextOut(ADrawRect.Left + (ADrawRect.Width - vSize.cx) div 2,
    ADrawRect.Top + (ADrawRect.Height - vSize.cy) div 2, FText);
end;

procedure THCButtonItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vSize: TSize;
begin
  if Self.AutoSize then
  begin
    ARichData.Style.ApplyTempStyle(TextStyleNo);
    vSize := ARichData.Style.TempCanvas.TextExtent(FText);
    Width := FPaddingLeft + FPaddingRight + vSize.cx;  // 间距
    Height := FPaddingTop + FPaddingBottom + vSize.cy;
  end;

  if Width < FMinWidth then
    Width := FMinWidth;

  if Height < FMinHeight then
    Height := FMinHeight;
end;

function THCButtonItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  if Self.Enabled and PtInRect(Self.ClientRect, Point(X, Y)) then
    FDown := ssLeft in Shift;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THCButtonItem.MouseLeave;
begin
  FDown := False;
  inherited MouseLeave;
end;

function THCButtonItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  //if PtInRect(GetBoxRect, Point(X, Y)) then
  GCursor := crArrow;
end;

function THCButtonItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  FDown := False;
  Result := inherited MouseUp(Button, Shift, X, Y);
end;

procedure THCButtonItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FText := ANode.Text;
end;

procedure THCButtonItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FText, AFileVersion);  // 读Text
end;

procedure THCButtonItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FText);
end;

procedure THCButtonItem.SetText(const Value: string);
begin
  FText := Value;
end;

procedure THCButtonItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Text := FText;
end;

end.
