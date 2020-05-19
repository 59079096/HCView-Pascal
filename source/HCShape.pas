{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文档形状对象管理单元                   }
{                                                       }
{*******************************************************}

unit HCShape;

interface

uses
  Windows, Classes, Controls, SysUtils, Graphics, Generics.Collections, HCItem,
  HCXml, HCCommon;

const
  PointSize = 4;

type
  THCShapeStyle = (
    /// <summary> 无形状 </summary>
    hssNone,
    /// <summary> 直线 </summary>
    hssLine,
    /// <summary> 矩形 </summary>
    hssRectangle,
    /// <summary> 椭圆 </summary>
    hssEllipse,
    /// <summary> 多边形 </summary>
    hssPolygon);

  THCStructState = (
    /// <summary> 构建停止 </summary>
    hstcStop,
    /// <summary> 构建开始 </summary>
    hstcStart,
    /// <summary> 构建中 </summary>
    hstcStructing);

  THCShape = class abstract
  strict private
    FVersion: Byte;
    FStyle: THCShapeStyle;
    FActive: Boolean;
    FColor: TColor;
    FStructState: THCStructState;
    FOnStructOver: TNotifyEvent;
  protected
    procedure PaintAnchor(const ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure SetColor(const Value: TColor); virtual;
    property Version: Byte read FVersion write FVersion;
    property StructState: THCStructState read FStructState write FStructState;
  public
    Cursor: TCursor;
    constructor Create; virtual;
    procedure Assign(const Source: THCShape); virtual;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
    function KeyUp(var Key: Word; Shift: TShiftState): Boolean; virtual;
    function KeyPress(var Key: Char): Boolean; virtual;
    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRect; const APaintInfo: TPaintInfo); virtual; abstract;
    function PointInClient(const X, Y: Integer): Boolean; virtual;
    function ClientRect: TRect; virtual;
    procedure SaveToStream(const AStream: TStream); virtual;
    procedure LoadFromStream(const AStream: TStream); virtual;
    procedure ToXml(const ANode: IHCXMLNode); virtual;
    procedure ParseXml(const ANode: IHCXMLNode); virtual;
    procedure StructStart; virtual;
    procedure StructOver; virtual;
    property Style: THCShapeStyle read FStyle write FStyle;
    property Active: Boolean read FActive write SetActive;
    property Color: TColor read FColor write SetColor;

    property OnStructOver: TNotifyEvent read FOnStructOver write FOnStructOver;
  end;

  THCShapeLineObj = (sloNone, sloLine, sloStart, sloEnd);

  THCShapeLine = class(THCShape)
  strict private
    FStartPt, FEndPt, FMousePt: TPoint;
    FActiveObj: THCShapeLineObj;
    FWidth: Byte;
    FLineStyle: TPenStyle;
    procedure SetWidth(const Value: Byte);
    procedure SetLineStyle(const Value: TPenStyle);
  protected
    procedure PaintAnchor(const ACanvas: TCanvas; const ARect: TRect); override;
    procedure SetActive(const Value: Boolean); override;
    function GetObjAt(const X, Y: Integer): THCShapeLineObj; virtual;
  public
    constructor Create; override;
    constructor CreateEx(const AStartPt, AEndPt: TPoint); virtual;
    procedure Assign(const Source: THCShape); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRect; const APaintInfo: TPaintInfo); override;
    function PointInClient(const X, Y: Integer): Boolean; override;
    function ClientRect: TRect; override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property StartPt: TPoint read FStartPt write FStartPt;
    property EndPt: TPoint read FEndPt write FEndPt;
    property Width: Byte read FWidth write SetWidth;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle;
    property ActiveObj: THCShapeLineObj read FActiveObj;
  end;

  THCShapeRectangle = class(THCShapeLine)
  private
    FBackColor: TColor;
  protected
    function GetObjAt(const X, Y: Integer): THCShapeLineObj; override;
  public
    constructor Create; override;
    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRect; const APaintInfo: TPaintInfo); override;

    property BackColor: TColor read FBackColor write FBackColor;
  end;

  THCShapeEllipse = class(THCShapeRectangle)
  protected
    function GetObjAt(const X, Y: Integer): THCShapeLineObj; override;
  public
    constructor Create; override;
    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRect; const APaintInfo: TPaintInfo); override;
  end;

  THCPoint = class(TObject)
  public
    X, Y: Integer;
    constructor Create(const AX, AY: Integer);
    procedure Init(const AX, AY: Integer);
    procedure Offset(const AX, AY: Integer);
  end;

  THCShapePolygon = class(THCShape)
  strict private
    FMousePt: TPoint;
    FPoints: TObjectList<THCPoint>;
    FWidth: Byte;
    FLineStyle: TPenStyle;
    FActivePointIndex, FActiveLineIndex: Integer;

    procedure OffsetPoints(const X, Y: Integer);
    procedure SetWidth(const Value: Byte);
    procedure SetLineStyle(const Value: TPenStyle);
  protected
    procedure PaintAnchor(const ACanvas: TCanvas; const ARect: TRect); override;
    procedure SetActive(const Value: Boolean); override;
    function GetPointAt(const X, Y: Integer): Integer;
    function GetLineAt(const X, Y: Integer): Integer;
    property Points: TObjectList<THCPoint> read FPoints;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(const Source: THCShape); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; override;

    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRect; const APaintInfo: TPaintInfo); override;
    function PointInClient(const X, Y: Integer): Boolean; override;
    function ClientRect: TRect; override;
    procedure StructOver; override;

    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
  end;

  THCShapeManager = class(TObjectList<THCShape>)
  private
    FActiveIndex, FHotIndex: Integer;
    FOperStyle: THCShapeStyle;
    FOnStructOver: TNotifyEvent;
    function NewShape(const AStyle: THCShapeStyle): Integer;
    procedure DoShapeStructOver(Sender: TObject);
    procedure SetOperStyle(const Value: THCShapeStyle);
    procedure SetActiveIndex(const Value: Integer);
  public
    constructor Create;
    //destructor Destroy; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean;
    function KeyUp(var Key: Word; Shift: TShiftState): Boolean;
    function KeyPress(var Key: Char): Boolean;

    procedure DisActive;
    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRect; const APaintInfo: TPaintInfo);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream);
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);

    property OperStyle: THCShapeStyle read FOperStyle write SetOperStyle;
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex;
    property HotIndex: Integer read FHotIndex;
    property OnStructOver: TNotifyEvent read FOnStructOver write FOnStructOver;
  end;

implementation

{ THCShapeManager }

constructor THCShapeManager.Create;
begin
  inherited Create(True);
  FActiveIndex := -1;
  FHotIndex := -1;
  FOperStyle := THCShapeStyle.hssNone;
end;

procedure THCShapeManager.DisActive;
begin
  FOperStyle := THCShapeStyle.hssNone;
  if FActiveIndex >= 0 then
    Self[FActiveIndex].Active := False;
end;

procedure THCShapeManager.DoShapeStructOver(Sender: TObject);
begin
  ActiveIndex := -1;
  if Assigned(FOnStructOver) then
    FOnStructOver(Sender);
end;

function THCShapeManager.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  if FActiveIndex >= 0 then
  begin
    if Self[FActiveIndex].KeyDown(Key, Shift) then
      Result := True
    else
    begin
      case Key of
        VK_BACK, VK_DELETE:
          begin
            Self.Delete(FActiveIndex);
            FActiveIndex := -1;
            Result := True;
          end;
      end;
    end;
  end;
end;

function THCShapeManager.KeyPress(var Key: Char): Boolean;
begin

end;

function THCShapeManager.KeyUp(var Key: Word; Shift: TShiftState): Boolean;
begin

end;

procedure THCShapeManager.LoadFromStream(const AStream: TStream);
var
  i, vCount: Integer;
  vShape: THCShape;
  vStyle: THCShapeStyle;
begin
  Self.Clear;
  AStream.ReadBuffer(vCount, SizeOf(vCount));
  for i := 0 to vCount - 1 do
  begin
    AStream.ReadBuffer(vStyle, SizeOf(vStyle));
    case vStyle of
      THCShapeStyle.hssNone: raise Exception.Create('THCShape读取失败，无效的样式值！');

      THCShapeStyle.hssLine: vShape := THCShapeLine.Create;
      THCShapeStyle.hssRectangle: vShape := THCShapeRectangle.Create;
      THCShapeStyle.hssEllipse: vShape := THCShapeEllipse.Create;
      THCShapeStyle.hssPolygon: vShape := THCShapePolygon.Create;
    end;

    AStream.Position := AStream.Position - SizeOf(vStyle);  // reback position
    vShape.LoadFromStream(AStream);
    Self.Add(vShape);
  end;
end;

function THCShapeManager.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  i, vIndex: Integer;
begin
  Result := False;

  if FOperStyle <> THCShapeStyle.hssNone then  // 在绘制
  begin
    if FActiveIndex < 0 then
    begin
      ActiveIndex := NewShape(FOperStyle);
      Self[FActiveIndex].StructStart;
    end;

    if FActiveIndex >= 0 then
      Result := Self[FActiveIndex].MouseDown(Button, Shift, X, Y);
  end
  else  // 不在绘制
  begin
    vIndex := -1;
    for i := 0 to Count - 1 do
    begin
      if Self[i].PointInClient(X, Y) then
      begin
        if Self[i].MouseDown(Button, Shift, X, Y) then
        begin
          vIndex := i;
          Result := True;
          Break;
        end;
      end;
    end;

    if vIndex <> FActiveIndex then
    begin
      ActiveIndex := vIndex;
      Result := True;
    end;
  end;
end;

function THCShapeManager.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;

  if FActiveIndex >= 0 then  // 有激活的Shape
  begin
    if Self[FActiveIndex].MouseMove(Shift, X, Y) then  // 激活的优先响应了
    begin
      FHotIndex := FActiveIndex;
      Result := True;
      Exit;
    end;
  end;

  FHotIndex := -1;
  for i := 0 to Count - 1 do
  begin
    if Self[i].PointInClient(X, Y) then
    begin
      if Self[i].MouseMove(Shift, X, Y) then
      begin
        FHotIndex := i;
        Result := True;
        Break;
      end;
    end;
  end;
end;

function THCShapeManager.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if Self[i].MouseUp(Button, Shift, X, Y) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function THCShapeManager.NewShape(const AStyle: THCShapeStyle): Integer;
var
  vShape: THCShape;
begin
  Result := -1;
  vShape := nil;

  case AStyle of
    THCShapeStyle.hssNone: ;
    THCShapeStyle.hssLine: vShape := THCShapeLine.Create;
    THCShapeStyle.hssRectangle: vShape := THCShapeRectangle.Create;
    THCShapeStyle.hssEllipse: vShape := THCShapeEllipse.Create;
    THCShapeStyle.hssPolygon: vShape := THCShapePolygon.Create;
  end;

  if Assigned(vShape) then
  begin
    vShape.OnStructOver := DoShapeStructOver;
    Result := Self.Add(vShape);
  end;
end;

procedure THCShapeManager.PaintTo(const ACanvas: TCanvas; const ARect: TRect;
  const APaintInfo: TPaintInfo);
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Self[i].PaintTo(ACanvas, ARect, APaintInfo);
end;

procedure THCShapeManager.ParseXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vShapeNode: IHCXMLNode;
  vShape: THCShape;
  vStyle: THCShapeStyle;
begin
  Self.Clear;

  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    vShapeNode := ANode.ChildNodes[i];
    vStyle := vShapeNode.Attributes['style'];
    case vStyle of
      THCShapeStyle.hssNone: raise Exception.Create('THCShape读取失败，无效的样式值！');

      THCShapeStyle.hssLine: vShape := THCShapeLine.Create;
      THCShapeStyle.hssRectangle: vShape := THCShapeRectangle.Create;
      THCShapeStyle.hssEllipse: vShape := THCShapeEllipse.Create;
      THCShapeStyle.hssPolygon: vShape := THCShapePolygon.Create;
    end;

    vShape.ParseXml(vShapeNode);
    Self.Add(vShape);
  end;
end;

procedure THCShapeManager.SaveToStream(const AStream: TStream);
var
  i: Integer;
  //vBegPos, vEndPos: Int64;
begin
  //vBegPos := AStream.Position;
  //AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 数据大小占位，便于越过

  AStream.WriteBuffer(Count, SizeOf(Count));
  for i := 0 to Count - 1 do
    Self[i].SaveToStream(AStream);

  //vEndPos := AStream.Position;
  //AStream.Position := vBegPos;
  //vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  //AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 样式数据总大小
  //AStream.Position := vEndPos;
end;

procedure THCShapeManager.SetActiveIndex(const Value: Integer);
begin
  if FActiveIndex <> Value then
  begin
    if FActiveIndex >= 0 then
      Self[FActiveIndex].Active := False;

    FActiveIndex := Value;
    if FActiveIndex >= 0 then
      Self[FActiveIndex].Active := True;
  end;
end;

procedure THCShapeManager.SetOperStyle(const Value: THCShapeStyle);
begin
  if FOperStyle <> Value then  // 切换操作样式
  begin
    ActiveIndex := -1;
    FOperStyle := Value;
  end;
end;

procedure THCShapeManager.ToXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vShapeNode: IHCXMLNode;
begin
  for i := 0 to Count - 1 do
  begin
    vShapeNode := ANode.AddChild('shape');
    Self[i].ToXml(vShapeNode);
  end;
end;

{ THCShapeLine }

procedure THCShapeLine.Assign(const Source: THCShape);
begin
  inherited Assign(Source);
  FStartPt.X := (Source as THCShapeLine).FStartPt.X;
  FStartPt.Y := (Source as THCShapeLine).FStartPt.Y;
  FEndPt.X := (Source as THCShapeLine).FEndPt.X;
  FEndPt.Y := (Source as THCShapeLine).FEndPt.Y;
end;

function THCShapeLine.ClientRect: TRect;
begin
  if FStartPt.X < FEndPt.X then
  begin
    Result.Left := FStartPt.X;
    Result.Right := FEndPt.X;
  end
  else
  begin
    Result.Left := FEndPt.X;
    Result.Right := FStartPt.X;
  end;

  if FStartPt.Y < FEndPt.Y then
  begin
    Result.Top := FStartPt.Y;
    Result.Bottom := FEndPt.Y;
  end
  else
  begin
    Result.Top := FEndPt.Y;
    Result.Bottom := FStartPt.Y;
  end;
end;

constructor THCShapeLine.Create;
begin
  inherited Create;
  Style := THCShapeStyle.hssLine;
  FStartPt := Point(0, 0);
  FEndPt := Point(0, 0);
  FWidth := 1;
  FActiveObj := sloNone;
  FLineStyle := TPenStyle.psSolid;
end;

constructor THCShapeLine.CreateEx(const AStartPt, AEndPt: TPoint);
begin
  Create;
  FStartPt := AStartPt;
  FEndPt := AEndPt;
end;

function THCShapeLine.GetObjAt(const X, Y: Integer): THCShapeLineObj;
var
  vRgn: HRGN;
  vPointArr: array[0..3] of TPoint;
begin
  Result := sloNone;

  if PtInRect(Rect(FStartPt.X - PointSize, FStartPt.Y - PointSize,
                   FStartPt.X + PointSize, FStartPt.Y + PointSize),
              Point(X, Y))
  then
    Result := sloStart
  else
  if PtInRect(Rect(FEndPt.X - PointSize, FEndPt.Y - PointSize,
                   FEndPt.X + PointSize, FEndPt.Y + PointSize),
              Point(X, Y))
  then
    Result := sloEnd
  else
  begin
    vPointArr[0] := Point(FStartPt.X - PointSize, FStartPt.Y);
    vPointArr[1] := Point(FStartPt.X + PointSize, FStartPt.Y);
    vPointArr[2] := Point(FEndPt.X + PointSize, FEndPt.Y);
    vPointArr[3] := Point(FEndPt.X - PointSize, FEndPt.Y);

    vRgn := CreatePolygonRgn(vPointArr, 4, WINDING);
    try
      if PtInRegion(vRgn, X, Y) then
        Result := sloLine;
    finally
      DeleteObject(vRgn);
    end;
  end;
end;

procedure THCShapeLine.LoadFromStream(const AStream: TStream);
begin
  inherited LoadFromStream(AStream);
  AStream.ReadBuffer(FWidth, SizeOf(FWidth));
  AStream.ReadBuffer(FLineStyle, SizeOf(FLineStyle));
  AStream.ReadBuffer(FStartPt.X, SizeOf(Integer));
  AStream.ReadBuffer(FStartPt.Y, SizeOf(Integer));
  AStream.ReadBuffer(FEndPt.X, SizeOf(Integer));
  AStream.ReadBuffer(FEndPt.Y, SizeOf(Integer));
end;

function THCShapeLine.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vLineObj: THCShapeLineObj;
begin
  Result := False;

  if (Button <> TMouseButton.mbLeft) or (Shift <> [ssLeft]) then Exit;

  if StructState <> THCStructState.hstcStop then  // 正在构建
  begin
    if StructState = THCStructState.hstcStart then  // 开始构建
    begin
      FStartPt := Point(X, Y);
      FEndPt := Point(X, Y);
      StructState := THCStructState.hstcStructing;
    end
    else  // 构建进行中按下，完成构建
      StructOver;

    Result := True;
  end
  else
  begin
    vLineObj := GetObjAt(X, Y);
    if FActiveObj <> vLineObj then
    begin
      FActiveObj := vLineObj;
      Active := FActiveObj <> THCShapeLineObj.sloNone;
      Result := Active;
    end
    else
      Result := vLineObj <> THCShapeLineObj.sloNone;

    if Result and (FActiveObj = THCShapeLineObj.sloLine) then
    begin
      FMousePt.X := X;
      FMousePt.Y := Y;
    end;
  end;
end;

function THCShapeLine.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  vLineObj: THCShapeLineObj;
begin
  Result := False;

  if StructState = THCStructState.hstcStructing then  // 构建中
  begin
    FEndPt := Point(X, Y);
    Result := True;
    Exit;
  end;

  if (Shift = [ssLeft]) and (FActiveObj <> sloNone) then  // 左键按下
  begin
    Result := True;

    case FActiveObj of
      sloLine:
        begin
          FStartPt.X := FStartPt.X + X - FMousePt.X;
          FStartPt.Y := FStartPt.Y + Y - FMousePt.Y;
          FEndPt.X := FEndPt.X + X - FMousePt.X;
          FEndPt.Y := FEndPt.Y + Y - FMousePt.Y;
          FMousePt.X := X;
          FMousePt.Y := Y;
        end;

      sloStart:
        begin
          FStartPt.X := X;
          FStartPt.Y := Y;
        end;

      sloEnd:
        begin
          FEndPt.X := X;
          FEndPt.Y := Y;
        end;
    end;
  end
  else
  begin
    vLineObj := GetObjAt(X, Y);
    if Active and (vLineObj in [sloStart, sloEnd]) then
      Cursor := crCross
    else
    if vLineObj <> sloNone then
      Cursor := crSize;

    Result := vLineObj <> sloNone;
  end;
end;

function THCShapeLine.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := False;
end;

procedure THCShapeLine.PaintAnchor(const ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Color := clWhite;

  ACanvas.Rectangle(FStartPt.X + ARect.Left - PointSize, FStartPt.Y + ARect.Top - PointSize,
    FStartPt.X + ARect.Left + PointSize, FStartPt.Y + ARect.Top + PointSize);
  ACanvas.Rectangle(FEndPt.X + ARect.Left - PointSize, FEndPt.Y + ARect.Top - PointSize,
    FEndPt.X + ARect.Left + PointSize, FEndPt.Y + ARect.Top + PointSize);
end;

procedure THCShapeLine.PaintTo(const ACanvas: TCanvas; const ARect: TRect;
  const APaintInfo: TPaintInfo);
begin
  ACanvas.Pen.Color := Color;
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FLineStyle;

  ACanvas.MoveTo(FStartPt.X + ARect.Left, FStartPt.Y + ARect.Top);
  ACanvas.LineTo(FEndPt.X + ARect.Left, FEndPt.Y + ARect.Top);

  if (not APaintInfo.Print) and Self.Active then  // 激活
    PaintAnchor(ACanvas, ARect);
end;

procedure THCShapeLine.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FWidth := ANode.Attributes['width'];
  FLineStyle := ANode.Attributes['ls'];
  FStartPt.X := ANode.Attributes['sx'];
  FStartPt.Y := ANode.Attributes['sy'];
  FEndPt.X := ANode.Attributes['ex'];
  FEndPt.Y := ANode.Attributes['ey'];
end;

function THCShapeLine.PointInClient(const X, Y: Integer): Boolean;
begin
  Result := GetObjAt(X, Y) <> sloNone;
end;

procedure THCShapeLine.SaveToStream(const AStream: TStream);
begin
  inherited SaveToStream(AStream);
  AStream.WriteBuffer(FWidth, SizeOf(FWidth));
  AStream.WriteBuffer(FLineStyle, SizeOf(FLineStyle));
  AStream.WriteBuffer(FStartPt.X, SizeOf(Integer));
  AStream.WriteBuffer(FStartPt.Y, SizeOf(Integer));
  AStream.WriteBuffer(FEndPt.X, SizeOf(Integer));
  AStream.WriteBuffer(FEndPt.Y, SizeOf(Integer));
end;

procedure THCShapeLine.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Self.Active then
    FActiveObj := sloNone;
end;

procedure THCShapeLine.SetLineStyle(const Value: TPenStyle);
begin
  if FLineStyle <> Value then
    FLineStyle := Value;
end;

procedure THCShapeLine.SetWidth(const Value: Byte);
begin
  if FWidth <> Value then
    FWidth := Value;
end;

procedure THCShapeLine.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['width'] := FWidth;
  ANode.Attributes['ls'] := FLineStyle;
  ANode.Attributes['sx'] := FStartPt.X;
  ANode.Attributes['sy'] := FStartPt.Y;
  ANode.Attributes['ex'] := FEndPt.X;
  ANode.Attributes['ey'] := FEndPt.Y;
end;

{ THCShape }

procedure THCShape.Assign(const Source: THCShape);
begin
  FStyle := Source.Style;
  FVersion := Source.Version;
  FColor := Source.Color;
end;

function THCShape.ClientRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

constructor THCShape.Create;
begin
  FStyle := THCShapeStyle.hssNone;
  FStructState := THCStructState.hstcStop;
  FVersion := 0;
  FActive := False;
  FColor := clBlack;
  Cursor := crDefault;
end;

function THCShape.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function THCShape.KeyPress(var Key: Char): Boolean;
begin
  Result := False;
end;

function THCShape.KeyUp(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

procedure THCShape.LoadFromStream(const AStream: TStream);
begin
  AStream.ReadBuffer(FStyle, SizeOf(FStyle));
  AStream.ReadBuffer(FVersion, SizeOf(FVersion));
  AStream.ReadBuffer(FColor, SizeOf(FColor));
end;

function THCShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Active := True;
  Result := FActive;
end;

function THCShape.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := FActive;
end;

function THCShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := FActive;
end;

procedure THCShape.PaintAnchor(const ACanvas: TCanvas; const ARect: TRect);
begin
end;

procedure THCShape.ParseXml(const ANode: IHCXMLNode);
begin
  FStyle := ANode.Attributes['style'];
  FVersion := ANode.Attributes['ver'];
  FColor := GetXmlRGBColor(ANode.Attributes['color']);
end;

function THCShape.PointInClient(const X, Y: Integer): Boolean;
begin
  Result := PtInRect(ClientRect, Point(X, Y));
end;

procedure THCShape.SaveToStream(const AStream: TStream);
begin
  if FStyle = THCShapeStyle.hssNone then
    raise Exception.Create('THCShape保存失败，无效的样式值！');

  AStream.WriteBuffer(FStyle, SizeOf(FStyle));
  AStream.WriteBuffer(FVersion, SizeOf(FVersion));
  AStream.WriteBuffer(FColor, SizeOf(FColor));
end;

procedure THCShape.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if not FActive then
      FStructState := THCStructState.hstcStop;
  end;
end;

procedure THCShape.SetColor(const Value: TColor);
begin
  if FColor <> Value then
    FColor := Value;
end;

procedure THCShape.StructOver;
begin
  FStructState := THCStructState.hstcStop;
  if Assigned(FOnStructOver) then
    FOnStructOver(Self);
end;

procedure THCShape.StructStart;
begin
  FStructState := THCStructState.hstcStart;
end;

procedure THCShape.ToXml(const ANode: IHCXMLNode);
begin
  ANode.Attributes['style'] := FStyle;
  ANode.Attributes['ver'] := FVersion;
  ANode.Attributes['color'] := GetColorXmlRGB(FColor);
end;

{ THCShapeRectangle }

constructor THCShapeRectangle.Create;
begin
  inherited Create;
  Style := THCShapeStyle.hssRectangle;
  FBackColor := HCTransparentColor;
end;

function THCShapeRectangle.GetObjAt(const X, Y: Integer): THCShapeLineObj;
var
  vRect: TRect;
begin
  Result := sloNone;

  if PtInRect(Rect(StartPt.X - PointSize, StartPt.Y - PointSize,
                   StartPt.X + PointSize, StartPt.Y + PointSize),
              Point(X, Y))
  then
    Result := sloStart
  else
  if PtInRect(Rect(EndPt.X - PointSize, EndPt.Y - PointSize,
                   EndPt.X + PointSize, EndPt.Y + PointSize),
              Point(X, Y))
  then
    Result := sloEnd
  else
  //if PointInClient(Point(X, Y)) then  // 粗暴判断
  begin
    // 判断四条边线
    vRect := ClientRect;
    InflateRect(vRect, PointSize, PointSize);
    if PtInRect(vRect, Point(X, Y)) then  // 在边框点宽度外
    begin
      InflateRect(vRect, -PointSize - PointSize, -PointSize - PointSize);
      if not PtInRect(vRect, Point(X, Y)) then  // 不在边框点宽度内
        Result := sloLine;
    end;
  end;
end;

procedure THCShapeRectangle.PaintTo(const ACanvas: TCanvas; const ARect: TRect;
  const APaintInfo: TPaintInfo);
begin
  ACanvas.Pen.Color := Color;
  ACanvas.Pen.Width := Width;
  ACanvas.Pen.Style := LineStyle;
  if FBackColor = HCTransparentColor then
    ACanvas.Brush.Style := bsClear;

  ACanvas.Rectangle(StartPt.X + ARect.Left, StartPt.Y + ARect.Top,
    EndPt.X + ARect.Left, EndPt.Y + ARect.Top);

  if (not APaintInfo.Print) and Self.Active then  // 激活
    PaintAnchor(ACanvas, ARect);
end;

{ THCShapeEllipse }

constructor THCShapeEllipse.Create;
begin
  inherited Create;
  Style := THCShapeStyle.hssEllipse;
end;

function THCShapeEllipse.GetObjAt(const X, Y: Integer): THCShapeLineObj;
var
  vRect: TRect;
  vRgn1, vRgn2: HRGN;
begin
  Result := sloNone;

  if PtInRect(Rect(StartPt.X - PointSize, StartPt.Y - PointSize,
                   StartPt.X + PointSize, StartPt.Y + PointSize),
              Point(X, Y))
  then
    Result := sloStart
  else
  if PtInRect(Rect(EndPt.X - PointSize, EndPt.Y - PointSize,
                   EndPt.X + PointSize, EndPt.Y + PointSize),
              Point(X, Y))
  then
    Result := sloEnd
  else
  //if PointInClient(Point(X, Y)) then  // 粗暴判断
  begin
    vRect := ClientRect;
    InflateRect(vRect, PointSize, PointSize);
    vRgn1 := CreateEllipticRgnIndirect(vRect);
    try
      if PtInRegion(vRgn1, X, Y) then  // 在外围
      begin
        InflateRect(vRect, -PointSize - PointSize, -PointSize - PointSize);
        vRgn2 := CreateEllipticRgnIndirect(vRect);
        try
          if not PtInRegion(vRgn2, X, Y) then  // 不在内围
            Result := sloLine;
        finally
          DeleteObject(vRgn2);
        end;
      end;
    finally
      DeleteObject(vRgn1);
    end;
  end;
end;

procedure THCShapeEllipse.PaintTo(const ACanvas: TCanvas; const ARect: TRect;
  const APaintInfo: TPaintInfo);
begin
  ACanvas.Pen.Color := Color;
  ACanvas.Pen.Width := Width;
  ACanvas.Pen.Style := LineStyle;
  if BackColor = HCTransparentColor then
    ACanvas.Brush.Style := bsClear;

  ACanvas.Ellipse(StartPt.X + ARect.Left, StartPt.Y + ARect.Top,
    EndPt.X + ARect.Left, EndPt.Y + ARect.Top);

  if (not APaintInfo.Print) and Self.Active then  // 激活
    PaintAnchor(ACanvas, ARect);
end;

{ THCShapePolygon }

procedure THCShapePolygon.Assign(const Source: THCShape);
var
  i: Integer;
  vPoint: THCPoint;
begin
  inherited Assign(Source);

  FPoints.Clear;
  for i := 0 to (Source as THCShapePolygon).Points.Count - 1 do
  begin
    vPoint := THCPoint.Create(
      (Source as THCShapePolygon).Points[i].X,
      (Source as THCShapePolygon).Points[i].Y);

    FPoints.Add(vPoint);
  end;
end;

function THCShapePolygon.ClientRect: TRect;
begin
  Result := inherited;
end;

constructor THCShapePolygon.Create;
begin
  inherited Create;
  Style := THCShapeStyle.hssPolygon;
  FWidth := 1;
  FLineStyle := TPenStyle.psSolid;
  FPoints := TObjectList<THCPoint>.Create;
  FActivePointIndex := -1;
  FActiveLineIndex := -1;
end;

destructor THCShapePolygon.Destroy;
begin
  FPoints.Free;
  inherited Destroy;
end;

function THCShapePolygon.GetLineAt(const X, Y: Integer): Integer;
var
  i: Integer;
  vRgn: HRGN;
  vPointArr: array [0..3] of TPoint;
begin
  Result := -1;

  for i := 0 to FPoints.Count - 1 do
  begin
    vPointArr[0] := Point(FPoints[i].X - PointSize, FPoints[i].Y);
    vPointArr[1] := Point(FPoints[i].X + PointSize, FPoints[i].Y);

    if i = FPoints.Count - 1 then
    begin
      vPointArr[2] := Point(FPoints[0].X + PointSize, FPoints[0].Y);
      vPointArr[3] := Point(FPoints[0].X - PointSize, FPoints[0].Y);
    end
    else
    begin
      vPointArr[2] := Point(FPoints[i + 1].X + PointSize, FPoints[i + 1].Y);
      vPointArr[3] := Point(FPoints[i + 1].X - PointSize, FPoints[i + 1].Y);
    end;

    vRgn := CreatePolygonRgn(vPointArr, 4, WINDING);
    try
      if PtInRegion(vRgn, X, Y) then
      begin
        Result := i;
        Break;
      end;
    finally
      DeleteObject(vRgn);
    end;
  end;
end;

function THCShapePolygon.GetPointAt(const X, Y: Integer): Integer;
var
  i: Integer;
  vPoint: THCPoint;
begin
  Result := -1;
  for i := 0 to FPoints.Count - 1 do
  begin
    vPoint := FPoints[i];
    if PtInRect(Rect(vPoint.X - PointSize, vPoint.Y - PointSize,
                     vPoint.X + PointSize, vPoint.Y + PointSize),
                Point(X, Y))
    then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function THCShapePolygon.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  if Key in [VK_BACK, VK_DELETE] then
  begin
    if (StructState = THCStructState.hstcStop) and (FActivePointIndex >= 0) then  // 删除当前点
    begin
      if FPoints.Count > 2 then
      begin
        FPoints.Delete(FActivePointIndex);
        FActivePointIndex := -1;
        Result := True;
      end;
    end;
  end;
end;

procedure THCShapePolygon.LoadFromStream(const AStream: TStream);
var
  i, vCount, vX, vY: Integer;
  vPoint: THCPoint;
begin
  FPoints.Clear;

  inherited LoadFromStream(AStream);
  AStream.ReadBuffer(FWidth, SizeOf(FWidth));
  AStream.ReadBuffer(FLineStyle, SizeOf(FLineStyle));

  AStream.ReadBuffer(vCount, SizeOf(vCount));
  for i := 0 to vCount - 1 do
  begin
    AStream.ReadBuffer(vX, SizeOf(vX));
    AStream.ReadBuffer(vY, SizeOf(vY));
    vPoint := THCPoint.Create(vX, vY);
    FPoints.Add(vPoint);
  end;
end;

function THCShapePolygon.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vIndex: Integer;
  vPoint: THCPoint;
begin
  Result := False;

  if Button = TMouseButton.mbRight then
  begin
    if StructState = THCStructState.hstcStructing then
      StructOver;

    Exit;
  end;

  if (Button <> TMouseButton.mbLeft) or (Shift <> [ssLeft]) then Exit;

  if StructState <> THCStructState.hstcStop then  // 没有处于构建状态
  begin
    if StructState = THCStructState.hstcStart then  // 开始构建
    begin
      vPoint := THCPoint.Create(X, Y);
      FPoints.Add(vPoint);

      vPoint := THCPoint.Create(X, Y);
      FActivePointIndex := FPoints.Add(vPoint);
      StructState := THCStructState.hstcStructing;
    end
    else
    if StructState = THCStructState.hstcStructing then
    begin
      vPoint := THCPoint.Create(X, Y);
      FActivePointIndex := FPoints.Add(vPoint);
    end
    else  // 构建进行中按下，完成构建
      StructOver;

    Result := True;
  end
  else
  begin
    vIndex := GetPointAt(X, Y);
    if FActivePointIndex <> vIndex then
    begin
      FActivePointIndex := vIndex;
      Active := FActivePointIndex >= 0;
      Result := Active;
    end
    else
      Result := vIndex >= 0;

    if not Result then  // 是否在线段上
    begin
      vIndex := GetLineAt(X, Y);
      if FActiveLineIndex <> vIndex then
      begin
        FActiveLineIndex := vIndex;
        Active := FActiveLineIndex >= 0;
        Result := Active;
      end
      else
        Result := vIndex >= 0;
    end;

    if Result then
    begin
      FMousePt.X := X;
      FMousePt.Y := Y;
    end;
  end;
end;

function THCShapePolygon.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  vIndex: Integer;
begin
  Result := False;

  if StructState = THCStructState.hstcStructing then  // 构建中
  begin
    FPoints[FActivePointIndex].Init(X, Y);
    Result := True;
    Exit;
  end;

  if Shift = [ssLeft] then  // 左键按下
  begin
    if FActivePointIndex >= 0 then
    begin
      FPoints[FActivePointIndex].Init(X, Y);
      Result := True;
    end
    else
    if FActiveLineIndex >= 0 then  // 整体移动
    begin
      OffsetPoints(X - FMousePt.X, Y - FMousePt.Y);

      FMousePt.X := X;
      FMousePt.Y := Y;

      Result := True;
    end;
  end
  else
  begin
    vIndex := GetPointAt(X, Y);
    if vIndex >= 0 then
    begin
      Cursor := crCross;
      Result := True;
    end
    else
    begin
      vIndex := GetLineAt(X, Y);
      if vIndex >= 0 then
      begin
        Cursor := crSizeAll;
        Result := True;
      end;
    end;
  end;
end;

function THCShapePolygon.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := False;
end;

procedure THCShapePolygon.OffsetPoints(const X, Y: Integer);
var
  i: Integer;
begin
  for i := 0 to FPoints.Count - 1 do
    FPoints[i].Offset(X, Y);
end;

procedure THCShapePolygon.PaintAnchor(const ACanvas: TCanvas;
  const ARect: TRect);
var
  i: Integer;
begin
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Color := clWhite;

  for i := 0 to FPoints.Count - 1 do
  begin
    ACanvas.Rectangle(FPoints[i].X + ARect.Left - PointSize, FPoints[i].Y + ARect.Top - PointSize,
      FPoints[i].X + ARect.Left + PointSize, FPoints[i].Y + ARect.Top + PointSize);
  end;

  if FActivePointIndex >= 0 then
  begin
    ACanvas.Pen.Color := clRed;
    if StructState = THCStructState.hstcStructing then
      ACanvas.Pen.Style := psDot;

    ACanvas.Rectangle(
      FPoints[FActivePointIndex].X + ARect.Left - PointSize,
      FPoints[FActivePointIndex].Y + ARect.Top - PointSize,
      FPoints[FActivePointIndex].X + ARect.Left + PointSize,
      FPoints[FActivePointIndex].Y + ARect.Top + PointSize);
  end;
end;

procedure THCShapePolygon.PaintTo(const ACanvas: TCanvas; const ARect: TRect;
  const APaintInfo: TPaintInfo);
var
  i: Integer;
begin
  ACanvas.Pen.Color := Color;
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FLineStyle;

  ACanvas.MoveTo(FPoints[0].X + ARect.Left, FPoints[0].Y + ARect.Top);
  for i := 1 to FPoints.Count - 1 do
    ACanvas.LineTo(FPoints[i].X + ARect.Left, FPoints[i].Y + ARect.Top);

  if FPoints.Count > 1 then  // 首尾相连
    ACanvas.LineTo(FPoints[0].X + ARect.Left, FPoints[0].Y + ARect.Top);

  if (not APaintInfo.Print) and Self.Active then  // 激活
    PaintAnchor(ACanvas, ARect);
end;

procedure THCShapePolygon.ParseXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vPoint: THCPoint;
begin
  FPoints.Clear;

  inherited ParseXml(ANode);
  FWidth := ANode.Attributes['width'];
  FLineStyle := ANode.Attributes['ls'];

  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    vPoint := THCPoint.Create(ANode.ChildNodes[i].Attributes['x'],
      ANode.ChildNodes[i].Attributes['y']);

    FPoints.Add(vPoint);
  end;
end;

function THCShapePolygon.PointInClient(const X, Y: Integer): Boolean;
var
  vIndex: Integer;
begin
  vIndex := GetPointAt(X, Y);
  if vIndex >= 0 then
    Result := True
  else
  begin
    vIndex := GetLineAt(X, Y);
    if vIndex >= 0 then
      Result := True;
  end;
end;

procedure THCShapePolygon.SaveToStream(const AStream: TStream);
var
  i: Integer;
begin
  inherited SaveToStream(AStream);

  AStream.WriteBuffer(FWidth, SizeOf(FWidth));
  AStream.WriteBuffer(FLineStyle, SizeOf(FLineStyle));

  AStream.WriteBuffer(FPoints.Count, SizeOf(Integer));
  for i := 0 to FPoints.Count - 1 do
  begin
    AStream.WriteBuffer(FPoints[i].X, SizeOf(Integer));
    AStream.WriteBuffer(FPoints[i].Y, SizeOf(Integer));
  end;
end;

procedure THCShapePolygon.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Self.Active then
  begin
    FActivePointIndex := -1;
    FActiveLineIndex := -1;
  end;
end;

procedure THCShapePolygon.SetLineStyle(const Value: TPenStyle);
begin
  if FLineStyle <> Value then
    FLineStyle := Value;
end;

procedure THCShapePolygon.SetWidth(const Value: Byte);
begin
  if FWidth <> Value then
    FWidth := Value;
end;

procedure THCShapePolygon.StructOver;
begin
  FActivePointIndex := -1;
  FActiveLineIndex := -1;
  if FPoints.Count > 2 then
    FPoints.Delete(FPoints.Count - 1);

  inherited StructOver;
end;

procedure THCShapePolygon.ToXml(const ANode: IHCXMLNode);
var
  vNode: IHCXMLNode;
  i: Integer;
begin
  inherited ToXml(ANode);
  ANode.Attributes['width'] := FWidth;
  ANode.Attributes['ls'] := FLineStyle;

  for i := 0 to FPoints.Count - 1 do
  begin
    vNode := ANode.AddChild('pt');
    vNode.Attributes['x'] := FPoints[i].X;
    vNode.Attributes['y'] := FPoints[i].Y;
  end;
end;

{ THCPoint }

constructor THCPoint.Create(const AX, AY: Integer);
begin
  inherited Create;
  Init(AX, AY);
end;

procedure THCPoint.Init(const AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

procedure THCPoint.Offset(const AX, AY: Integer);
begin
  X := X + AX;
  Y := Y + AY;
end;

end.
