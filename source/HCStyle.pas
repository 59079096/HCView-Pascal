{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{               文档内对象样式管理单元                  }
{                                                       }
{*******************************************************}

unit HCStyle;

interface

uses
  Windows, Classes, Graphics, Generics.Collections, HCTextStyle, HCParaStyle, HCXml;

type
  /// <summary> 全局状态更新控制 </summary>
  TUpdateInfo = class(TObject)    // 为保证更新操作相互之间不影响
  public
    // 以下字段，仅在需要为True时赋值，不可使用 RePaint := A <> B的形式，防止将其他处修改的True覆盖
    RePaint: Boolean;  // 所有参数只能写赋值为True的代码，不能赋值为多个变量的与、或、非
    //ReSized: Boolean;  // Item有大小改变，只在TCustomRichData.MouseUp中判断
    ReCaret,  // 重新计算光标
    ReStyle,  // 重新计算光标时获取光标处样式
    ReScroll,  // 滚动到光标位置
    Selecting,  // 全局划选标识
    Draging  // 全局拖拽标识
      : Boolean;
    constructor Create;
  end;

  TInvalidateRectEvent = procedure(const ARect: TRect) of object;

  THCStyle = class(TPersistent)
  strict private
    FDefCanvas: TCanvas;
    /// <summary> 外部当前的段样式 </summary>
    FCurParaNo,
    /// <summary> 外部当前的文本样式 </summary>
    FCurStyleNo: Integer;
    FSelColor: TColor;
    FBackgroudColor: TColor;
    FTextStyles: TObjectList<THCTextStyle>;
    FParaStyles: TObjectList<THCParaStyle>;
    FPixelsPerInchX, FPixelsPerInchY: Integer;  // 1英寸对应的像素数
    FFontSizeScale,  // 字体需要缩放的比例
    FPixelsPerMMX, FPixelsPerMMY: Single;  // 1毫米dpi数
    FUpdateInfo: TUpdateInfo;
    FShowParaLastMark: Boolean;  // 是否显示换行符
    FHtmlFileTempName: Integer;

    FOnInvalidateRect: TInvalidateRectEvent;
  protected
    procedure SetShowParaLastMark(Value: Boolean);
  public const
    Null = -1;  // TextItem和RectItem分界线
    Image = -2;
    Table = -3;
    Tab = -4;
    Line = -5;
    Express = -6;
    Vector = -7;  // SVG
    Domain = -8;
    PageBreak = -9;
    CheckBox = -10;
    Gif = -11;
    Control = -12;
    Edit = -13;
    Combobox = -14;
    QRCode = -15;
    BarCode = -16;
    Fraction = -17;
    DateTimePicker = -18;
    RadioGroup = -19;
    SupSubScript = -20;
    Custom = -1000;  // 自定义类型分界线
  public
    constructor Create; virtual;
    constructor CreateEx(const ADefTextStyle, ADefParaStyle: Boolean);
    destructor Destroy; override;
    procedure Initialize;
    procedure UpdateInfoRePaint;
    procedure UpdateInfoReStyle;
    procedure UpdateInfoReScroll;

    /// <summary> 更新光标位置/summary>
    /// <param name="ACaretStyle">重新获取光标处样式</param>
    procedure UpdateInfoReCaret(const ACaretStyle: Boolean = True);
    function AddTextStyle(const ATextStyle: THCTextStyle): Integer;

    class function GetFontHeight(const ACanvas: TCanvas): Integer;
    class function CreateStyleCanvas: TCanvas;
    class procedure DestroyStyleCanvas(const ACanvas: TCanvas);

    /// <summary> 创建一个新字体样式 </summary>
    /// <returns>样式编号</returns>
    function NewDefaultTextStyle: Integer;
    function NewDefaultParaStyle: Integer;
    function GetStyleNo(const ATextStyle: THCTextStyle; const ACreateIfNull: Boolean): Integer;
    function GetParaNo(const AParaStyle: THCParaStyle; const ACreateIfNull: Boolean): Integer;

    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);

    function GetHtmlFileTempName(const AReset: Boolean = False): string;
    function ToCSS: string;
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);

    procedure InvalidateRect(const ARect: TRect);

    property TextStyles: TObjectList<THCTextStyle> read FTextStyles write FTextStyles;
    property ParaStyles: TObjectList<THCParaStyle> read FParaStyles write FParaStyles;
    property BackgroudColor: TColor read FBackgroudColor write FBackgroudColor;
    property SelColor: TColor read FSelColor write FSelColor;
    property CurParaNo: Integer read FCurParaNo write FCurParaNo;
    property CurStyleNo: Integer read FCurStyleNo write FCurStyleNo;
    property DefCanvas: TCanvas read FDefCanvas;
    property PixelsPerInchX: Integer read FPixelsPerInchX;
    property PixelsPerInchY: Integer read FPixelsPerInchY;
    property PixelsPerMMX: single read FPixelsPerMMX;
    property PixelsPerMMY: single read FPixelsPerMMY;
    property FontSizeScale: single read FFontSizeScale;
    property UpdateInfo: TUpdateInfo read FUpdateInfo;
    property ShowParaLastMark: Boolean read FShowParaLastMark write SetShowParaLastMark;
    property OnInvalidateRect: TInvalidateRectEvent read FOnInvalidateRect write FOnInvalidateRect;
  end;

  THCFloatStyle = class(TObject)  // 浮动Item样式
  public const
    Line = 1;  // 直线
  end;

implementation

uses
  SysUtils, HCCommon;

{ THCStyle }

function THCStyle.AddTextStyle(const ATextStyle: THCTextStyle): Integer;
begin
  Result := FTextStyles.Add(ATextStyle);
end;

procedure THCStyle.Initialize;
begin
  FTextStyles.DeleteRange(1, FTextStyles.Count - 1);  // 保留默认文本样式
  FParaStyles.DeleteRange(1, FParaStyles.Count - 1);  // 保留默认段样式
  FCurStyleNo := 0;
  FCurParaNo := 0;
end;

procedure THCStyle.InvalidateRect(const ARect: TRect);
begin
  if Assigned(FOnInvalidateRect) then
    FOnInvalidateRect(ARect);
end;

constructor THCStyle.Create;
begin
  inherited Create;

  FDefCanvas := CreateStyleCanvas;
  FPixelsPerInchX := GetDeviceCaps(FDefCanvas.Handle, LOGPIXELSX);  // 每英寸水平逻辑像素数，1英寸dpi数
  FPixelsPerInchY := GetDeviceCaps(FDefCanvas.Handle, LOGPIXELSY);  // 每英寸水平逻辑像素数，1英寸dpi数

  // 1英寸25.4毫米   FPixelsPerInchX
  FPixelsPerMMX := Windows.GetDeviceCaps(FDefCanvas.Handle, LOGPIXELSX) / 25.4;  // 1毫米对应像素 = 1英寸dpi数 / 1英寸对应毫米
  FPixelsPerMMY := Windows.GetDeviceCaps(FDefCanvas.Handle, LOGPIXELSY) / 25.4;  // 1毫米对应像素 = 1英寸dpi数 / 1英寸对应毫米

  FFontSizeScale := 72 / FPixelsPerInchX;

  FBackgroudColor := $00FFFFFE;
  FSelColor := clSkyBlue;
  FShowParaLastMark := True;
  FUpdateInfo := TUpdateInfo.Create;
  FTextStyles := TObjectList<THCTextStyle>.Create;
  FParaStyles := TObjectList<THCParaStyle>.Create;
end;

constructor THCStyle.CreateEx(const ADefTextStyle, ADefParaStyle: Boolean);
begin
  Create;
  if ADefTextStyle then
    NewDefaultTextStyle;

  if ADefParaStyle then
    NewDefaultParaStyle;
end;

class function THCStyle.CreateStyleCanvas: TCanvas;
var
  vDC: HDC;
begin
  vDC := CreateCompatibleDC(0);
  Result := TCanvas.Create;
  Result.Handle := vDC;
end;

destructor THCStyle.Destroy;
begin
  DestroyStyleCanvas(FDefCanvas);

  FTextStyles.Free;
  FParaStyles.Free;
  FUpdateInfo.Free;
  inherited Destroy;
end;

class procedure THCStyle.DestroyStyleCanvas(const ACanvas: TCanvas);
var
  vDC: HDC;
begin
  vDC := ACanvas.Handle;
  ACanvas.Handle := 0;
  ACanvas.Free;
  DeleteDC(vDC);
end;

function THCStyle.GetParaNo(const AParaStyle: THCParaStyle; const ACreateIfNull: Boolean): Integer;
var
  i: Integer;
  vParaStyle: THCParaStyle;
begin
  Result := -1;
  for i := 0 to FParaStyles.Count - 1 do
  begin
    if FParaStyles[i].EqualsEx(AParaStyle) then
    begin
      Result := i;
      Exit;
    end;
  end;
  if ACreateIfNull and (Result < 0) then
  begin
    vParaStyle := THCParaStyle.Create;
    vParaStyle.AssignEx(AParaStyle);
    FParaStyles.Add(vParaStyle);
    Result := FParaStyles.Count - 1;
  end;
end;

function THCStyle.GetStyleNo(const ATextStyle: THCTextStyle;
  const ACreateIfNull: Boolean): Integer;
var
  i: Integer;
  vTextStyle: THCTextStyle;
begin
  Result := -1;
  for i := 0 to FTextStyles.Count - 1 do
  begin
    if FTextStyles[i].EqualsEx(ATextStyle) then
    begin
      Result := i;
      Exit;
    end;
  end;
  if ACreateIfNull and (Result < 0) then
  begin
    vTextStyle := THCTextStyle.Create;
    vTextStyle.AssignEx(ATextStyle);
    FTextStyles.Add(vTextStyle);
    Result := FTextStyles.Count - 1;
  end;
end;

class function THCStyle.GetFontHeight(const ACanvas: TCanvas): Integer;
begin
  Result := ACanvas.TextHeight('H');
end;

function THCStyle.GetHtmlFileTempName(const AReset: Boolean = False): string;
begin
  if AReset then
    FHtmlFileTempName := 0
  else
    Inc(FHtmlFileTempName);
  Result := IntToStr(FHtmlFileTempName);
end;

procedure THCStyle.LoadFromStream(const AStream: TStream; const AFileVersion: Word);

  {$REGION '段落样式'}
  procedure LoadParaStyles;
  var
    i, vStyleCount: Integer;
  begin
    FParaStyles.Clear;
    AStream.ReadBuffer(vStyleCount, SizeOf(vStyleCount));
    for i := 0 to vStyleCount - 1 do
      FParaStyles[NewDefaultParaStyle].LoadFromStream(AStream, AFileVersion);
  end;
  {$ENDREGION}

  {$REGION '文本样式'}
  procedure LoadTextStyles;
  var
    i, vStyleCount: Integer;
  begin
    FTextStyles.Clear;
    AStream.ReadBuffer(vStyleCount, SizeOf(vStyleCount));
    for i := 0 to vStyleCount - 1 do
      FTextStyles[NewDefaultTextStyle].LoadFromStream(AStream, AFileVersion);
  end;
  {$ENDREGION}

var
  vDataSize: Int64;
begin
  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));
  //
  LoadParaStyles;
  LoadTextStyles;
end;

function THCStyle.NewDefaultTextStyle: Integer;
var
  vTextStyle: THCTextStyle;
begin
  vTextStyle := THCTextStyle.Create;
  Result := FTextStyles.Add(vTextStyle);
end;

procedure THCStyle.ParseXml(const ANode: IHCXMLNode);
var
  i, j: Integer;
begin
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    if ANode.ChildNodes[i].NodeName = 'textstyles' then
    begin
      FTextStyles.Clear;
      for j := 0 to ANode.ChildNodes[i].ChildNodes.Count - 1 do
        FTextStyles[NewDefaultTextStyle].ParseXml(ANode.ChildNodes[i].ChildNodes[j]);
    end
    else
    if ANode.ChildNodes[i].NodeName = 'parastyles' then
    begin
      FParaStyles.Clear;
      for j := 0 to ANode.ChildNodes[i].ChildNodes.Count - 1 do
        FParaStyles[NewDefaultParaStyle].ParseXml(ANode.ChildNodes[i].ChildNodes[j]);
    end;
  end;
end;

procedure THCStyle.SaveToStream(const AStream: TStream);

  {$REGION '段落样式'}
  procedure SaveParaStyles;
  var
    i: Integer;
  begin
    AStream.WriteBuffer(FParaStyles.Count, SizeOf(FParaStyles.Count));
    for i := 0 to FParaStyles.Count - 1 do
      FParaStyles[i].SaveToStream(AStream);
  end;
  {$ENDREGION}

  {$REGION '文本样式'}
  procedure SaveTextStyles;
  var
    i: Integer;
  begin
    AStream.WriteBuffer(FTextStyles.Count, SizeOf(FTextStyles.Count));
    for i := 0 to FTextStyles.Count - 1 do
      FTextStyles[i].SaveToStream(AStream);
  end;
  {$ENDREGION}

var
  vBegPos, vEndPos: Int64;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 数据大小占位，便于越过
  //
  SaveParaStyles;
  SaveTextStyles;
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 样式数据总大小
  AStream.Position := vEndPos;
end;

procedure THCStyle.SetShowParaLastMark(Value: Boolean);
begin
  if FShowParaLastMark <> Value then
  begin
    FShowParaLastMark := Value;
    UpdateInfoRePaint;
  end;
end;

function THCStyle.ToCSS: string;
var
  i: Integer;
begin
  Result := '<style type="text/css">';
  for i := 0 to FTextStyles.Count - 1 do
  begin
    Result := Result + sLineBreak + 'a.fs' + IntToStr(i) + ' {';
    Result := Result + FTextStyles[i].ToCSS + ' }';
  end;

  for i := 0 to FParaStyles.Count - 1 do
  begin
    Result := Result + sLineBreak + 'p.ps' + IntToStr(i) + ' {';
    Result := Result + FParaStyles[i].ToCSS + ' }';
  end;
  Result := Result + sLineBreak + '</style>';
end;

procedure THCStyle.ToXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vNode: IHCXMLNode;
begin
  ANode.Attributes['fscount'] := FTextStyles.Count;
  ANode.Attributes['pscount'] := FParaStyles.Count;

  vNode := ANode.AddChild('textstyles');
  for i := 0 to FTextStyles.Count - 1 do
    FTextStyles[i].ToXml(vNode.AddChild('ts'));

  vNode := ANode.AddChild('parastyles');
  for i := 0 to FParaStyles.Count - 1 do
    FParaStyles[i].ToXml(vNode.AddChild('ps'));
end;

procedure THCStyle.UpdateInfoReCaret(const ACaretStyle: Boolean = True);
begin
  FUpdateInfo.ReCaret := True;
  if ACaretStyle then
    FUpdateInfo.ReStyle := True;
end;

procedure THCStyle.UpdateInfoRePaint;
begin
  FUpdateInfo.RePaint := True;
end;

procedure THCStyle.UpdateInfoReScroll;
begin
  FUpdateInfo.ReScroll := True;
end;

procedure THCStyle.UpdateInfoReStyle;
begin
  FUpdateInfo.ReStyle := True;
end;

function THCStyle.NewDefaultParaStyle: Integer;
var
  vParaStyle: THCParaStyle;
begin
  vParaStyle := THCParaStyle.Create;
  Result := FParaStyles.Add(vParaStyle);
end;

{ TUpdateInfo }

constructor TUpdateInfo.Create;
begin
  RePaint := False;
  ReCaret := False;
  ReStyle := False;
  Draging := False;
end;

end.
