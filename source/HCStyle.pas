{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  Classes, Graphics, Generics.Collections, HCTextStyle, HCParaStyle;

type
  /// <summary> 全局状态更新控制 </summary>
  TUpdateInfo = class(TObject)    // 为保证更新操作相互之间不影响
  public
    // 以下字段，仅在需要为True时赋值，不可使用 RePaint := A <> B的形式，防止将其他处修改的True覆盖
    RePaint: Boolean;  // 所有参数只能写赋值为True的代码，不能赋值为多个变量的与、或、非
    //ReSized: Boolean;  // Item有大小改变，只在TCustomRichData.MouseUp中判断
    ReCaret,  // 重新计算光标
    ReStyle,  // 重新计算光标时获取光标处样式
    Draging: Boolean;
    constructor Create;
  end;

  THCStyle = class(TPersistent)
  strict private
    FDefCanvas: TCanvas;
    /// <summary> 外部当前的段样式 </summary>
    FCurParaNo,
    /// <summary> 外部当前的文本样式 </summary>
    FCurStyleNo: Integer;
    FSelColor: TColor;
    FBackgroudColor: TColor;
    FTextStyles: TObjectList<TTextStyle>;
    FParaStyles: TObjectList<TParaStyle>;
    FPixelsPerInchX, FPixelsPerInchY: Single;  // 屏幕1英寸dpi数
    FUpdateInfo: TUpdateInfo;
    FShowLineLastMark: Boolean;  // 是否显示换行符
  protected
    procedure SetShowLineLastMark(Value: Boolean);
  public const
    LineSpace100 = 8;
    LineSpace150 = 12;
    LineSpace200 = 16;
    //
    RsNull = -1;  // 用于表示StyleNo没有赋值，出错状态
    RsBitmap = -2;
    RsTable = -3;
    RsTab = -4;
    RsLine = -5;
    RsExpress = -6;
    //RsVector = -7;  // 矢量图也许在GraphicItem上做用格式区别
    RsDomain = -8;
    RsPageBreak = -9;
    RsControl = -10;
    RsCustom = -100;  // 自定义类型分界线
  public
    constructor Create; virtual;
    constructor CreateEx(const ADefTextStyle, ADefParaStyle: Boolean);
    destructor Destroy; override;
    procedure Initialize;
    procedure UpdateInfoRePaint;
    procedure UpdateInfoReStyle;
    procedure UpdateInfoReCaret(const ACaretStyle: Boolean = True);
    function AddTextStyle(const ATextStyle: TTextStyle): Integer;
    /// <summary>
    /// 创建一个新字体样式
    /// </summary>
    /// <returns>样式编号</returns>
    function NewDefaultTextStyle: Integer;
    function NewDefaultParaStyle: Integer;
    function GetStyleNo(const ATextStyle: TTextStyle; const ACreateIfNull: Boolean): Integer;
    function GetParaNo(const AParaStyle: TParaStyle; const ACreateIfNull: Boolean): Integer;

    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);

    property TextStyles: TObjectList<TTextStyle> read FTextStyles write FTextStyles;
    property ParaStyles: TObjectList<TParaStyle> read FParaStyles write FParaStyles;
    property BackgroudColor: TColor read FBackgroudColor write FBackgroudColor;
    property SelColor: TColor read FSelColor write FSelColor;
    property CurParaNo: Integer read FCurParaNo write FCurParaNo;
    property CurStyleNo: Integer read FCurStyleNo write FCurStyleNo;
    property DefCanvas: TCanvas read FDefCanvas;
    property PixelsPerInchX: single read FPixelsPerInchX;
    property PixelsPerInchY: single read FPixelsPerInchY;
    property UpdateInfo: TUpdateInfo read FUpdateInfo;
    property ShowLineLastMark: Boolean read FShowLineLastMark write SetShowLineLastMark;
  end;

implementation

uses
  Windows, HCCommon;

{ THCStyle }

function THCStyle.AddTextStyle(const ATextStyle: TTextStyle): Integer;
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

constructor THCStyle.Create;
var
  vDC: HDC;
begin
  inherited Create;

  vDC := CreateCompatibleDC(0);
  FDefCanvas := TCanvas.Create;
  FDefCanvas.Handle := vDC;
  //FDefCanvas.Font.PixelsPerInch := 96;
  FDefCanvas.Font.PixelsPerInch := GetDeviceCaps(vDC, LOGPIXELSX);
  FPixelsPerInchX := Windows.GetDeviceCaps(vDC, LOGPIXELSX) / 25.4;  // 1毫米对应打印像素 = 1英寸dpi数 / 1英寸对应25.4毫米
  FPixelsPerInchY := Windows.GetDeviceCaps(vDC, LOGPIXELSY) / 25.4;  // 1毫米对应打印像素 = 1英寸dpi数 / 1英寸对应25.4毫米

  FBackgroudColor := $00FFFFFE;
  FSelColor := clSkyBlue;
  FShowLineLastMark := True;
  FUpdateInfo := TUpdateInfo.Create;
  FTextStyles := TObjectList<TTextStyle>.Create;
  FParaStyles := TObjectList<TParaStyle>.Create;
end;

constructor THCStyle.CreateEx(const ADefTextStyle, ADefParaStyle: Boolean);
begin
  Create;
  if ADefTextStyle then
    NewDefaultTextStyle;

  if ADefParaStyle then
    NewDefaultParaStyle;
end;

destructor THCStyle.Destroy;
var
  vDC: HDC;
begin
  vDC := FDefCanvas.Handle;
  FDefCanvas.Handle := 0;
  FDefCanvas.Free;
  DeleteDC(vDC);

  FTextStyles.Free;
  FParaStyles.Free;
  FUpdateInfo.Free;
  inherited Destroy;
end;

function THCStyle.GetParaNo(const AParaStyle: TParaStyle; const ACreateIfNull: Boolean): Integer;
var
  i: Integer;
  vParaStyle: TParaStyle;
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
    vParaStyle := TParaStyle.Create;
    vParaStyle.AssignEx(AParaStyle);
    FParaStyles.Add(vParaStyle);
    Result := FParaStyles.Count - 1;
  end;
end;

function THCStyle.GetStyleNo(const ATextStyle: TTextStyle;
  const ACreateIfNull: Boolean): Integer;
var
  i: Integer;
  vTextStyle: TTextStyle;
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
    vTextStyle := TTextStyle.Create;
    vTextStyle.AssignEx(ATextStyle);
    FTextStyles.Add(vTextStyle);
    Result := FTextStyles.Count - 1;
  end;
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
  vTextStyle: TTextStyle;
begin
  vTextStyle := TTextStyle.Create;
  Result := FTextStyles.Add(vTextStyle);
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

procedure THCStyle.SetShowLineLastMark(Value: Boolean);
begin
  if FShowLineLastMark <> Value then
  begin
    FShowLineLastMark := Value;
    UpdateInfoRePaint;
  end;
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

procedure THCStyle.UpdateInfoReStyle;
begin
  FUpdateInfo.ReStyle := True;
end;

function THCStyle.NewDefaultParaStyle: Integer;
var
  vParaStyle: TParaStyle;
begin
  vParaStyle := TParaStyle.Create;
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
