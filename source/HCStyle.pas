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
  Windows, Classes, Graphics, SysUtils, Generics.Collections, HCTextStyle, HCParaStyle,
  HCXml, HCCommon;

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

  THCStateDictionary = class(TObject)
    State: THCState;
    Count: Integer;
  end;

  THCStates = class(TObject)
  private
    FStates: TList;
    procedure DeleteState(const AIndex: Integer);
    function GetStateIndex(const AState: THCState): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Include(const AState: THCState);
    procedure Exclude(const AState: THCState);
    function Contain(const AState: THCState): Boolean;
  end;

  TInvalidateRectEvent = procedure(const ARect: TRect) of object;

  THCStyle = class(TPersistent)
  strict private
    // 临时Canvas，请使用ApplyTempStyle修改其属性，避免根据FTempStyleNo判断不需要
    // 重新设置时其属性和FTempStyleNo并不对应
    FTempCanvas: TCanvas;
    /// <summary> 记录最近一次格式化的样式 </summary>
    FTempStyleNo: Integer;
    FLineSpaceMin: Byte;  // 行间距最小值
    FSelColor: TColor;
    FBackgroundColor: TColor;
    FDefaultTextStyle: THCTextStyle;
    FTextStyles: TObjectList<THCTextStyle>;
    FParaStyles: TObjectList<THCParaStyle>;
    FUpdateInfo: TUpdateInfo;
    FShowParaLastMark: Boolean;  // 是否显示换行符
    FDrawActiveDomainRegion, FDrawHotDomainRegion: Boolean;  // 是否绘制域边框
    FHtmlFileTempName: Integer;
    FStates: THCStates;  // 全局操作状态
    FFormatVersion: Byte;  // 排版算法版本

    FOnInvalidateRect: TInvalidateRectEvent;
  protected
    procedure SetShowParaLastMark(const Value: Boolean);
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
    FloatLine = -101;
    FloatBarCode = -102;
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

    class function CreateStyleCanvas: TCanvas;
    class procedure DestroyStyleCanvas(const ACanvas: TCanvas);

    /// <summary> 创建一个新字体样式 </summary>
    /// <returns>样式编号</returns>
    function NewDefaultTextStyle: Integer;
    function NewDefaultParaStyle: Integer;
    function GetStyleNo(const ATextStyle: THCTextStyle; const ACreateIfNull: Boolean): Integer;
    function GetParaNo(const AParaStyle: THCParaStyle; const ACreateIfNull: Boolean): Integer;
    procedure ApplyTempStyle(const Value: Integer; const AScale: Single = 1);

    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);

    function GetHtmlFileTempName(const AReset: Boolean = False): string;
    function ToCSS: string;
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);

    procedure InvalidateRect(const ARect: TRect);

    property DefaultTextStyle: THCTextStyle read FDefaultTextStyle;
    property TextStyles: TObjectList<THCTextStyle> read FTextStyles write FTextStyles;
    property ParaStyles: TObjectList<THCParaStyle> read FParaStyles write FParaStyles;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property SelColor: TColor read FSelColor write FSelColor;
    property LineSpaceMin: Byte read FLineSpaceMin write FLineSpaceMin;
    property TempStyleNo: Integer read FTempStyleNo;
    /// <summary> 临时Canvas，请使用ApplyTempStyle修改其属性避免根据FTempStyleNo判断不需要重新设置时其属性和FTempStyleNo并不对应 </summary>
    property TempCanvas: TCanvas read FTempCanvas;
    property UpdateInfo: TUpdateInfo read FUpdateInfo;
    property DrawActiveDomainRegion: Boolean read FDrawActiveDomainRegion write FDrawActiveDomainRegion;
    property DrawHotDomainRegion: Boolean read FDrawHotDomainRegion write FDrawHotDomainRegion;
    property ShowParaLastMark: Boolean read FShowParaLastMark write SetShowParaLastMark;
    property States: THCStates read FStates;
    property FormatVersion: Byte read FFormatVersion;
    property OnInvalidateRect: TInvalidateRectEvent read FOnInvalidateRect write FOnInvalidateRect;
  end;

implementation

{ THCStyle }

function THCStyle.AddTextStyle(const ATextStyle: THCTextStyle): Integer;
begin
  Result := FTextStyles.Add(ATextStyle);
end;

procedure THCStyle.Initialize;
begin
  FTextStyles.DeleteRange(1, FTextStyles.Count - 1);  // 保留默认文本样式
  FParaStyles.DeleteRange(1, FParaStyles.Count - 1);  // 保留默认段样式
end;

procedure THCStyle.InvalidateRect(const ARect: TRect);
begin
  if Assigned(FOnInvalidateRect) then
    FOnInvalidateRect(ARect);
end;

procedure THCStyle.ApplyTempStyle(const Value: Integer; const AScale: Single);
begin
  if FTempStyleNo <> Value then
  begin
    FTempStyleNo := Value;
    if Value > THCStyle.Null then
      FTextStyles[Value].ApplyStyle(FTempCanvas, AScale);
  end;
end;

constructor THCStyle.Create;
begin
  inherited Create;
  FTempCanvas := CreateStyleCanvas;
  FTempStyleNo := THCStyle.Null;
  FBackgroundColor := $00FFFFFF;
  FSelColor := clSkyBlue;
  FLineSpaceMin := 8;
  FFormatVersion := 2;
  FShowParaLastMark := True;
  FDrawHotDomainRegion := True;
  FDrawActiveDomainRegion := True;
  FStates := THCStates.Create;
  FUpdateInfo := TUpdateInfo.Create;
  FTextStyles := TObjectList<THCTextStyle>.Create;
  FParaStyles := TObjectList<THCParaStyle>.Create;
  FDefaultTextStyle := THCTextStyle.Create;
  FDefaultTextStyle.ApplyStyle(FTempCanvas);  // 生成测量信息
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
  DestroyStyleCanvas(FTempCanvas);
  FDefaultTextStyle.Free;
  FTextStyles.Free;
  FParaStyles.Free;
  FUpdateInfo.Free;
  FStates.Free;
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
  if (AFileVersion > 33) then
    AStream.ReadBuffer(FFormatVersion, SizeOf(FFormatVersion))
  else
    FFormatVersion := 1;

  LoadParaStyles;
  LoadTextStyles;
end;

function THCStyle.NewDefaultTextStyle: Integer;
var
  vTextStyle: THCTextStyle;
begin
  vTextStyle := THCTextStyle.Create;
  Result := FTextStyles.Add(vTextStyle);
  vTextStyle.ApplyStyle(FTempCanvas);  // 生成测量信息
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
  AStream.WriteBuffer(FFormatVersion, SizeOf(FFormatVersion));
  SaveParaStyles;
  SaveTextStyles;
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 样式数据总大小
  AStream.Position := vEndPos;
end;

procedure THCStyle.SetShowParaLastMark(const Value: Boolean);
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
  // 注意单倍行间距，文本底色不为白色也不透明时会造成字符看起来叠加的问题
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
  FUpdateInfo.ReStyle := ACaretStyle;
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

{ THCStates }

function THCStates.Contain(const AState: THCState): Boolean;
begin
  Result := GetStateIndex(AState) >= 0;
end;

constructor THCStates.Create;
begin
  FStates := TList.Create;
end;

procedure THCStates.DeleteState(const AIndex: Integer);
begin
  THCStateDictionary(FStates[AIndex]).Free;
  FStates.Delete(AIndex);
end;

destructor THCStates.Destroy;
var
  i: Integer;
begin
  for i := 0 to FStates.Count - 1 do
    THCStateDictionary(FStates[i]).Free;

  FStates.Free;
  inherited Destroy;
end;

procedure THCStates.Exclude(const AState: THCState);
var
  vIndex: Integer;
begin
  vIndex := GetStateIndex(AState);
  if vIndex >= 0 then
  begin
    if THCStateDictionary(FStates[vIndex]).Count > 1 then
      Dec(THCStateDictionary(FStates[vIndex]).Count)
    else
      DeleteState(vIndex);
  end;
end;

function THCStates.GetStateIndex(const AState: THCState): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FStates.Count - 1 do
  begin
    if THCStateDictionary(FStates[i]).State = AState then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure THCStates.Include(const AState: THCState);
var
  vIndex: Integer;
  vStateDic: THCStateDictionary;
begin
  vIndex := GetStateIndex(AState);
  if vIndex >= 0 then
    Inc(THCStateDictionary(FStates[vIndex]).Count)
  else
  begin
    vStateDic := THCStateDictionary.Create;
    vStateDic.State := AState;
    vStateDic.Count := 1;
    FStates.Add(vStateDic);
  end;
end;

end.
