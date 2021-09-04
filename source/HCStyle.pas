{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{               �ĵ��ڶ�����ʽ����Ԫ                  }
{                                                       }
{*******************************************************}

unit HCStyle;

interface

uses
  Windows, Classes, Graphics, SysUtils, Generics.Collections, HCTextStyle, HCParaStyle,
  HCXml, HCCommon;

type
  /// <summary> ȫ��״̬���¿��� </summary>
  TUpdateInfo = class(TObject)    // Ϊ��֤���²����໥֮�䲻Ӱ��
  public
    // �����ֶΣ�������ҪΪTrueʱ��ֵ������ʹ�� RePaint := A <> B����ʽ����ֹ���������޸ĵ�True����
    RePaint: Boolean;  // ���в���ֻ��д��ֵΪTrue�Ĵ��룬���ܸ�ֵΪ����������롢�򡢷�
    //ReSized: Boolean;  // Item�д�С�ı䣬ֻ��TCustomRichData.MouseUp���ж�
    ReCaret,  // ���¼�����
    ReStyle,  // ���¼�����ʱ��ȡ��괦��ʽ
    ReScroll,  // ���������λ��
    Selecting,  // ȫ�ֻ�ѡ��ʶ
    DragingSelected  // ȫ����קѡ�����ݱ�ʶ
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
    // ��ʱCanvas����ʹ��ApplyTempStyle�޸������ԣ��������FTempStyleNo�жϲ���Ҫ
    // ��������ʱ�����Ժ�FTempStyleNo������Ӧ
    FTempCanvas: TCanvas;
    /// <summary> ��¼���һ�θ�ʽ������ʽ </summary>
    FTempStyleNo: Integer;
    FLineSpaceMin: Byte;  // �м����Сֵ
    FSelColor: TColor;
    FBackgroundColor: TColor;
    FDefaultTextStyle: THCTextStyle;
    FTextStyles: TObjectList<THCTextStyle>;
    FParaStyles: TObjectList<THCParaStyle>;
    FUpdateInfo: TUpdateInfo;
    FShowParaLastMark: Boolean;  // �Ƿ���ʾ���з�
    FDrawActiveDomainRegion, FDrawHotDomainRegion: Boolean;  // �Ƿ������߿�
    FHtmlFileTempName: Integer;
    FStates: THCStates;  // ȫ�ֲ���״̬
    FFormatVersion: Byte;  // �Ű��㷨�汾

    FOnInvalidateRect: TInvalidateRectEvent;
  protected
    procedure SetShowParaLastMark(const Value: Boolean);
  public const
    Null = -1;  // TextItem��RectItem�ֽ���
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
    Button = -21;
    Bullet = -22;
    FloatLine = -101;
    FloatBarCode = -102;
    Annotate = -118;
    Custom = -1000;  // �Զ������ͷֽ���
  public
    constructor Create; virtual;
    constructor CreateEx(const ADefTextStyle, ADefParaStyle: Boolean);
    destructor Destroy; override;
    procedure Initialize;
    procedure UpdateInfoRePaint;
    procedure UpdateInfoReStyle;
    procedure UpdateInfoReScroll;

    /// <summary> ���¹��λ��/summary>
    /// <param name="ACaretStyle">���»�ȡ��괦��ʽ</param>
    procedure UpdateInfoReCaret(const ACaretStyle: Boolean = True);
    function AddTextStyle(const ATextStyle: THCTextStyle): Integer;

    class function CreateStyleCanvas: TCanvas;
    class procedure DestroyStyleCanvas(const ACanvas: TCanvas);

    /// <summary> ����һ����������ʽ </summary>
    /// <returns>��ʽ���</returns>
    function NewDefaultTextStyle: Integer;
    function NewDefaultParaStyle: Integer;
    function GetDefaultStyleNo: Integer;
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
    /// <summary> ��ʱCanvas����ʹ��ApplyTempStyle�޸������Ա������FTempStyleNo�жϲ���Ҫ��������ʱ�����Ժ�FTempStyleNo������Ӧ </summary>
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
  FTextStyles.DeleteRange(1, FTextStyles.Count - 1);  // ����Ĭ���ı���ʽ
  FParaStyles.DeleteRange(1, FParaStyles.Count - 1);  // ����Ĭ�϶���ʽ
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
  FDefaultTextStyle.ApplyStyle(FTempCanvas);  // ���ɲ�����Ϣ
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

function THCStyle.GetStyleNo(const ATextStyle: THCTextStyle; const ACreateIfNull: Boolean): Integer;
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
    vTextStyle.ApplyStyle(FTempCanvas);
    Result := FTextStyles.Count - 1;
    FTempStyleNo := Result;
  end;
end;

function THCStyle.GetDefaultStyleNo: Integer;
begin
  Result := GetStyleNo(FDefaultTextStyle, True);
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

  {$REGION '������ʽ'}
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

  {$REGION '�ı���ʽ'}
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

  if AFileVersion > 49 then
    AStream.ReadBuffer(FLineSpaceMin, SizeOf(FLineSpaceMin))
  else
    FLineSpaceMin := 8;

  LoadParaStyles;
  LoadTextStyles;
end;

function THCStyle.NewDefaultTextStyle: Integer;
var
  vTextStyle: THCTextStyle;
begin
  vTextStyle := THCTextStyle.Create;
  Result := FTextStyles.Add(vTextStyle);
  vTextStyle.ApplyStyle(FTempCanvas);
  FTempStyleNo := Result;
end;

procedure THCStyle.ParseXml(const ANode: IHCXMLNode);
var
  i, j: Integer;
begin
  if ANode.HasAttribute('fmtver') then
    FFormatVersion := ANode.Attributes['fmtver']
  else
    FFormatVersion := 1;

  if ANode.HasAttribute('linespacemin') then
    FLineSpaceMin := ANode.Attributes['linespacemin']
  else
    FLineSpaceMin := 8;

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

  {$REGION '������ʽ'}
  procedure SaveParaStyles;
  var
    i: Integer;
  begin
    AStream.WriteBuffer(FParaStyles.Count, SizeOf(FParaStyles.Count));
    for i := 0 to FParaStyles.Count - 1 do
      FParaStyles[i].SaveToStream(AStream);
  end;
  {$ENDREGION}

  {$REGION '�ı���ʽ'}
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
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // ���ݴ�Сռλ������Խ��
  //
  AStream.WriteBuffer(FFormatVersion, SizeOf(FFormatVersion));
  AStream.WriteBuffer(FLineSpaceMin, SizeOf(FLineSpaceMin));

  SaveParaStyles;
  SaveTextStyles;
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // ��ʽ�����ܴ�С
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
  // ע�ⵥ���м�࣬�ı���ɫ��Ϊ��ɫҲ��͸��ʱ������ַ����������ӵ�����
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
  ANode.Attributes['fmtver'] := FFormatVersion;
  ANode.Attributes['linespacemin'] := FLineSpaceMin;

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
  DragingSelected := False;
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
