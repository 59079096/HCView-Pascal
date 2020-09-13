{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-8-16             }
{                                                       }
{            文档FloatItem(浮动)对象实现单元            }
{                                                       }
{*******************************************************}

unit HCCustomFloatItem;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Messages, Generics.Collections,
  HCItem, HCRectItem, HCStyle, HCCustomData, HCXml;

type
  THCCustomFloatItem = class(THCResizeRectItem)  // 可浮动Item
  private
    FLeft, FTop,  // 位置
    FPageIndex  // 当前在哪一页  20190906001
      : Integer;
    FLock: Boolean;
    FDrawRect: TRect;
    FMousePt: TPoint;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    function PointInClient(const X, Y: Integer): Boolean; overload; virtual;
    procedure Assign(Source: THCCustomItem); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property DrawRect: TRect read FDrawRect write FDrawRect;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property PageIndex: Integer read FPageIndex write FPageIndex;
    property Lock: Boolean read FLock write FLock;
  end;

  THCFloatItems = class(TObjectList<THCCustomFloatItem>)
  private
    FOnInsertItem, FOnRemoveItem: TItemNotifyEvent;
  protected
    procedure Notify(const Value: THCCustomFloatItem; Action: TCollectionNotification); override;
  public
    property OnInsertItem: TItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TItemNotifyEvent read FOnRemoveItem write FOnRemoveItem;
  end;

implementation

{ THCCustomFloatItem }

procedure THCCustomFloatItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FLeft := (Source as THCCustomFloatItem).Left;
  FTop := (Source as THCCustomFloatItem).Top;
  Width := (Source as THCCustomFloatItem).Width;
  Height := (Source as THCCustomFloatItem).Height;
end;

constructor THCCustomFloatItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  //Self.StyleNo := THCStyle.FloatItem;
  FLock := False;
end;

procedure THCCustomFloatItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom,
     ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  if Self.Active then
    ACanvas.DrawFocusRect(FDrawRect);
end;

procedure THCCustomFloatItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vValue: Integer;
begin
  //AStream.ReadBuffer(StyleNo, SizeOf(StyleNo));  // 加载时先读取并根据值创建
  AStream.ReadBuffer(FLeft, SizeOf(FLeft));
  AStream.ReadBuffer(FTop, SizeOf(FTop));

  AStream.ReadBuffer(vValue, SizeOf(vValue));
  Width := vValue;
  AStream.ReadBuffer(vValue, SizeOf(vValue));
  Height := vValue;

  if AFileVersion > 28 then
    AStream.ReadBuffer(FPageIndex, SizeOf(FPageIndex));

  if AFileVersion > 37 then
    AStream.ReadBuffer(FLock, SizeOf(FLock))
  else
    FLock := False;
end;

function THCCustomFloatItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer): Boolean;
begin
  if FLock then
    Result := Self.Active
  else
  begin
    Result := inherited MouseDown(Button, Shift, X, Y);
    if not Self.Resizing then
      FMousePt := Point(X, Y);
  end;
end;

function THCCustomFloatItem.MouseMove(Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  if FLock then
    Result := Self.Active
  else
  begin
    Result := inherited MouseMove(Shift, X, Y);
    if (not Self.Resizing) and (Shift = [ssLeft]) then
    begin
      FLeft := FLeft + X - FMousePt.X;
      FTop := FTop + Y - FMousePt.Y;
      // 因为移动后，Left和Top变化，原鼠标位置相对左上角的位置不变，所以不用修正FMousePt
    end;
  end;
end;

function THCCustomFloatItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  //Result := inherited MouseUp(Button, Shift, X, Y);
  // 继承里的Undo没有处理好，处理时可考虑继承父类的MouseUp方法
  Result := False;
  if FLock then Exit;

  if Self.Resizing then
  begin
    Self.Resizing := False;

    if (Self.ResizeWidth < 0) or (Self.ResizeHeight < 0) then Exit;

    Width := Self.ResizeWidth;
    Height := Self.ResizeHeight;
    Result := True;
  end;
end;

procedure THCCustomFloatItem.ParseXml(const ANode: IHCXMLNode);
begin
  StyleNo := ANode.Attributes['sno'];
  FLeft := ANode.Attributes['left'];
  FTop := ANode.Attributes['top'];
  Width := ANode.Attributes['width'];
  Height := ANode.Attributes['height'];
  FPageIndex := ANode.Attributes['pageindex'];
  if ANode.HasAttribute('lock') then
    FLock := ANode.Attributes['lock']
  else
    FLock := False;
end;

function THCCustomFloatItem.PointInClient(const X, Y: Integer): Boolean;
begin
  Result := PtInRect(Bounds(0, 0, Width, Height), Point(X, Y));
end;

procedure THCCustomFloatItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vValue: Integer;
begin
  AStream.WriteBuffer(Self.StyleNo, SizeOf(Self.StyleNo));
  AStream.WriteBuffer(FLeft, SizeOf(FLeft));
  AStream.WriteBuffer(FTop, SizeOf(FTop));

  vValue := Width;
  AStream.WriteBuffer(vValue, SizeOf(vValue));
  vValue := Height;
  AStream.WriteBuffer(vValue, SizeOf(vValue));

  // 20190906001 FloatItem不能通过GetPageIndexByFormat(FPage.FloatItems[0].Top)来计算FloatItem
  // 的页序号，因为正文的可能拖到页眉页脚处，按Top算GetPageIndexByFormat并不在当前页中，所以单独存
  AStream.WriteBuffer(FPageIndex, SizeOf(FPageIndex));
  AStream.WriteBuffer(FLock, SizeOf(FLock));
end;

procedure THCCustomFloatItem.ToXml(const ANode: IHCXMLNode);
begin
  ANode.Attributes['sno'] := StyleNo;
  ANode.Attributes['left'] := FLeft;
  ANode.Attributes['top'] := FTop;
  ANode.Attributes['width'] := Width;
  ANode.Attributes['height'] := Height;
  ANode.Attributes['pageindex'] := FPageIndex;
  ANode.Attributes['lock'] := FLock;
end;

{ THCFloatItems }

procedure THCFloatItems.Notify(const Value: THCCustomFloatItem;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      begin
        if Assigned(FOnInsertItem) then
          FOnInsertItem(Value);
      end;

    cnRemoved:
      begin
        if Assigned(FOnRemoveItem) then
          FOnRemoveItem(Value);
      end;

    cnExtracted: ;
  end;

  inherited Notify(Value, Action);
end;

end.
