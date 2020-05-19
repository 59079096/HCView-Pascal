{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                 文本文字样式实现单元                  }
{                                                       }
{*******************************************************}

unit HCTextStyle;

interface

uses
  Windows, Classes, Graphics, SysUtils, HCXml;

type
  THCFontStyle = (tsBold, tsItalic, tsUnderline, tsStrikeOut, tsSuperscript,
    tsSubscript);

  THCFontStyles = set of THCFontStyle;

  // https://github.com/wine-mirror/wine/blob/master/dlls/gdiplus/font.c
  TT_HHEA = packed record
    Version: Cardinal;
    Ascender: SmallInt;
    Descender: SmallInt;
    LineGap: SmallInt;
    advanceWidthMax: Word;
    minLeftSideBearing: SmallInt;
    minRightSideBearing: SmallInt;
    xMaxExtent: SmallInt;
    caretSlopeRise: SmallInt;
    caretSlopeRun: SmallInt;
    caretOffset: SmallInt;
    reserved: array[0..3] of SmallInt;
    metricDataFormat: SmallInt;
    numberOfHMetrics: Word;
  end;

  THCTextStyle = class(TPersistent)
  private const
    DefaultFontSize: Single = 10.5;  // 五号
    DefaultFontFamily = '宋体';
    MaxFontSize: Single = 512;
  strict private
    FSize: Single;
    FFontHeight: Integer;
    FFamily: TFontName;
    FFontStyles: THCFontStyles;
    FColor: TColor;  // 字体颜色
    FBackColor: TColor;
    FTextMetric: TTextMetric;

    FCJKFont: Boolean;
    FOutMetSize: Cardinal;
    FOutlineTextmetric: POutlineTextmetric;
    FFontHeader: TT_HHEA;
  protected
    procedure SetFamily(const Value: TFontName);
    procedure SetSize(const Value: Single);
    procedure SetFontStyles(const Value: THCFontStyles);
  public
    CheckSaveUsed: Boolean;
    TempNo: Integer;
    constructor Create;
    destructor Destroy; override;
    function IsSizeStored: Boolean;
    function IsFamilyStored: Boolean;
    procedure ApplyStyle(const ACanvas: TCanvas; const AScale: Single = 1);
    function EqualsEx(const ASource: THCTextStyle): Boolean;
    procedure AssignEx(const ASource: THCTextStyle);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
    function ToCSS: string;
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXmlNode);
    property TextMetric: TTextMetric read FTextMetric;
    property OutMetSize: Cardinal read FOutMetSize;
    property OutlineTextmetric: POutlineTextmetric read FOutlineTextmetric;
    property FontHeader: TT_HHEA read FFontHeader;
    property CJKFont: Boolean read FCJKFont;
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
    property FontHeight: Integer read FFontHeight;
    property FontStyles: THCFontStyles read FFontStyles write SetFontStyles default [];
    property Color: TColor read FColor write FColor default clBlack;
    property BackColor: TColor read FBackColor write FBackColor default clWhite;
  end;

implementation

uses
  HCCommon;

{ THCTextStyle }

procedure THCTextStyle.ApplyStyle(const ACanvas: TCanvas; const AScale: Single = 1);

const
  MS_HHEA_TAG = $61656868;  // MS_MAKE_TAG('h','h','e','a')
  CJK_CODEPAGE_BITS = (1 shl 17) or (1 shl 18) or (1 shl 19) or (1 shl 20) or (1 shl 21);

var
  //vFont: TFont;
  vLogFont: TLogFont;
  vPixPerInch: Integer;
  vFontSignature: TFontSignature;
begin
  with ACanvas do
  begin
    if FBackColor = HCTransparentColor then
      Brush.Style := bsClear
    else
      Brush.Color := FBackColor;

    Font.Color := FColor;
    Font.Name := FFamily;
    Font.Size := Round(FSize);
    if tsBold in FFontStyles then
      Font.Style := Font.Style + [TFontStyle.fsBold]
    else
      Font.Style := Font.Style - [TFontStyle.fsBold];

    if tsItalic in FFontStyles then
      Font.Style := Font.Style + [TFontStyle.fsItalic]
    else
      Font.Style := Font.Style - [TFontStyle.fsItalic];

    if tsUnderline in FFontStyles then
      Font.Style := Font.Style + [TFontStyle.fsUnderline]
    else
      Font.Style := Font.Style - [TFontStyle.fsUnderline];

    if tsStrikeOut in FFontStyles then
      Font.Style := Font.Style + [TFontStyle.fsStrikeOut]
    else
      Font.Style := Font.Style - [TFontStyle.fsStrikeOut];
  end;

  vPixPerInch := GetDeviceCaps(ACanvas.Handle, LOGPIXELSY);  // 要根据环境实时取不可使用HCUnitConversion中默认DC的值
  GetObject(ACanvas.Font.Handle, SizeOf(vLogFont), @vLogFont);

  // 如果引用了HCStyle，下面的GetDeviceCaps可以通过其PixelsPerInchY属性替换
  if (tsSuperscript in FFontStyles) or (tsSubscript in FFontStyles) then
  begin
    if vLogFont.lfHeight < 0 then
      vLogFont.lfHeight := -Round(FSize / 2 * vPixPerInch / 72 / AScale)
    else
      vLogFont.lfHeight := Round(FSize / 2 * vPixPerInch / 72 / AScale)
  end
  else
  begin
    if vLogFont.lfHeight < 0 then
      vLogFont.lfHeight := -Round(FSize * vPixPerInch / 72 / AScale)
    else
      vLogFont.lfHeight := Round(FSize * vPixPerInch / 72 / AScale)
  end;

  ACanvas.Font.Handle := CreateFontIndirect(vLogFont);

  GetTextMetrics(ACanvas.Handle, FTextMetric);  // 得到字体信息
  FFontHeight := ACanvas.TextHeight('H');

  {$IFDEF DELPHIXE}
  if (GetTextCharsetInfo(ACanvas.Handle, @vFontSignature, 0) <> DEFAULT_CHARSET)
  {$ELSE}
  if (Integer(GetTextCharsetInfo(ACanvas.Handle, @vFontSignature, 0)) <> DEFAULT_CHARSET)
  {$ENDIF}
    and (vFontSignature.fsCsb[0] and CJK_CODEPAGE_BITS <> 0)
  then
    FCJKFont := True
  else
    FCJKFont := False;

  if FOutMetSize > 0 then
    FreeMem(FOutlineTextmetric, FOutMetSize);

  FOutMetSize := GetOutlineTextMetrics(ACanvas.Handle, 0, nil);
  //vOutlineTextmetric.otmSize := SizeOf(vOutlineTextmetric);
  if FOutMetSize <> 0 then
  begin
    GetMem(FOutlineTextmetric, FOutMetSize);
    GetOutlineTextMetrics(ACanvas.Handle, FOutMetSize, FOutlineTextmetric);  // 取到TrueType字体的正文度量
  end;

  if GetFontData(ACanvas.Handle, MS_HHEA_TAG, 0, @FFontHeader, SizeOf(FFontHeader)) = GDI_ERROR then  // 接收一种可缩放字体文件的数据
    Exit;
end;

procedure THCTextStyle.AssignEx(const ASource: THCTextStyle);
begin
  Self.FSize := ASource.Size;
  Self.FFontHeight := ASource.FontHeight;
  Self.FFontStyles := ASource.FontStyles;
  Self.FFamily := ASource.Family;
  Self.FColor := ASource.Color;
  Self.FBackColor := ASource.BackColor;
end;

constructor THCTextStyle.Create;
begin
  FSize := DefaultFontSize;
  FFamily := DefaultFontFamily;
  FFontStyles := [];
  FColor := clBlack;
  FBackColor := HCTransparentColor;
  FOutMetSize := 0;
end;

destructor THCTextStyle.Destroy;
begin
  if FOutMetSize > 0 then
    FreeMem(FOutlineTextmetric, FOutMetSize);

  inherited Destroy;
end;

function THCTextStyle.EqualsEx(const ASource: THCTextStyle): Boolean;
begin
  Result :=
    (Self.FSize = ASource.Size)
    and (Self.FFontStyles = ASource.FontStyles)
    and (Self.FFamily = ASource.Family)
    and (Self.FColor = ASource.Color)
    and (Self.FBackColor = ASource.BackColor);
end;

function THCTextStyle.IsFamilyStored: Boolean;
begin
  Result := FFamily <> DefaultFontFamily;
end;

function THCTextStyle.IsSizeStored: Boolean;
begin
  Result := FSize = DefaultFontSize;
end;

procedure THCTextStyle.LoadFromStream(const AStream: TStream; const AFileVersion: Word);
var
  vOldSize: Integer;
  vFamily: string;
begin
  if AFileVersion < 12 then
  begin
    AStream.ReadBuffer(vOldSize, SizeOf(vOldSize));  // 字号
    FSize := vOldSize;
  end
  else
    AStream.ReadBuffer(FSize, SizeOf(FSize));  // 字号

  HCLoadTextFromStream(AStream, vFamily, AFileVersion);  // 字体
  FFamily := vFamily;

  AStream.ReadBuffer(FFontStyles, SizeOf(FFontStyles));

  if AFileVersion > 18 then
  begin
    HCLoadColorFromStream(AStream, FColor);
    HCLoadColorFromStream(AStream, FBackColor);
  end
  else
  begin
    AStream.ReadBuffer(FColor, SizeOf(FColor));
    AStream.ReadBuffer(FBackColor, SizeOf(FBackColor));
  end;
end;

procedure THCTextStyle.ParseXml(const ANode: IHCXmlNode);

  procedure GetFontStyles_;
  var
    vsStyles: TStringList;
    i: Integer;
  begin
    vsStyles := TStringList.Create;
    try
      vsStyles.Delimiter := ',';
      vsStyles.DelimitedText := ANode.Attributes['style'];
      for i := 0 to vsStyles.Count - 1 do
      begin
        if vsStyles[i] = 'bold' then
          FFontStyles := FFontStyles + [tsBold]
        else
        if vsStyles[i] = 'italic' then
          FFontStyles := FFontStyles + [tsItalic]
        else
        if vsStyles[i] = 'underline' then
          FFontStyles := FFontStyles + [tsUnderline]
        else
        if vsStyles[i] = 'strikeout' then
          FFontStyles := FFontStyles + [tsStrikeOut]
        else
        if vsStyles[i] = 'sup' then
          FFontStyles := FFontStyles + [tsSuperscript]
        else
        if vsStyles[i] = 'sub' then
          FFontStyles := FFontStyles + [tsSubscript];
      end;
    finally
      FreeAndNil(vsStyles);
    end;
  end;

begin
  FFamily := ANode.Text;
  FSize := ANode.Attributes['size'];
  FColor := GetXmlRGBColor(ANode.Attributes['color']);
  FBackColor := GetXmlRGBColor(ANode.Attributes['bkcolor']);
  GetFontStyles_;
end;

procedure THCTextStyle.SaveToStream(const AStream: TStream);
begin
  AStream.WriteBuffer(FSize, SizeOf(FSize));
  HCSaveTextToStream(AStream, FFamily);
  AStream.WriteBuffer(FFontStyles, SizeOf(FFontStyles));
  HCSaveColorToStream(AStream, FColor);
  HCSaveColorToStream(AStream, FBackColor);
end;

procedure THCTextStyle.SetFamily(const Value: TFontName);
begin
  if FFamily <> Value then
    FFamily := Value;
end;

procedure THCTextStyle.SetSize(const Value: Single);
begin
  if FSize <> Value then
    FSize := Value;
  // 是否需要在Style里的FTextStyles里绑定这里设置完Size后马上获取测量信息？
end;

function THCTextStyle.ToCSS: string;

  function GetTextDecoration: string;
  begin
    Result := '';
    if tsUnderline in FFontStyles then
      Result := ' underline';
    if tsStrikeOut in FFontStyles then
    begin
      if Result <> '' then
        Result := Result + ', line-through'
      else
        Result := ' line-through';
    end;
    //if rvfsOverline in TextStyle.StyleEx then
    //  Result := Result + ', overline';

    Result := 'text-decoration:' + Result + ';';
  end;

begin
  Result := Format(' font-size: %fpt;', [FSize])
    + Format(' font-family: %s;', [FFamily])
    + Format(' color:rgb(%d, %d, %d);', [GetRValue(FColor), GetGValue(FColor), GetBValue(FColor)]);

  if (FBackColor <> clWhite) and (FBackColor <> HCTransparentColor) then
    Result := Result + Format(' background-color:rgb(%d, %d, %d);',
      [GetRValue(FBackColor), GetGValue(FBackColor), GetBValue(FBackColor)]);

  if tsItalic in FFontStyles then
    Result := Result + Format(' font-style: %s;', ['italic'])
  else
    Result := Result + Format(' font-style: %s;', ['normal']);

  if (tsBold in FFontStyles) or (tsStrikeOut in FFontStyles) then
    Result := Result + Format(' font-weight: %s;', ['bold'])
  else
    Result := Result + Format(' font-weight: %s;', ['normal']);

  if (tsUnderline in FFontStyles) or (tsStrikeOut in FFontStyles) then
    Result := Result + ' ' + GetTextDecoration;

  if tsSuperscript in FFontStyles then
    Result := Result + ' vertical-align:super;';

  if tsSubscript in FFontStyles then
    Result := Result + ' vertical-align:sub;';
end;

procedure THCTextStyle.ToXml(const ANode: IHCXMLNode);

  function GetFontStyleXML: string;
  begin
    if tsBold in FFontStyles then
      Result := 'bold';

    if tsItalic in FFontStyles then
    begin
      if Result <> '' then
        Result := Result + ',italic'
      else
        Result := 'italic';
    end;

    if tsUnderline in FFontStyles then
    begin
      if Result <> '' then
        Result := Result + ',underline'
      else
        Result := 'underline';
    end;

    if tsStrikeOut in FFontStyles then
    begin
      if Result <> '' then
        Result := Result + ',strikeout'
      else
        Result := 'strikeout';
    end;

    if tsSuperscript in FFontStyles then
    begin
      if Result <> '' then
        Result := Result + ',sup'
      else
        Result := 'sup';
    end;

    if tsSubscript in FFontStyles then
    begin
      if Result <> '' then
        Result := Result + ',sub'
      else
        Result := 'sub';
    end;
  end;

begin
  ANode.Attributes['size'] := FormatFloat('0.#', FSize);
  ANode.Attributes['color'] := GetColorXmlRGB(FColor);
  ANode.Attributes['bkcolor'] := GetColorXmlRGB(FBackColor);
  ANode.Attributes['style'] := GetFontStyleXML;
  ANode.Text := FFamily;
end;

procedure THCTextStyle.SetFontStyles(const Value: THCFontStyles);
begin
  if FFontStyles <> Value then
    FFontStyles := Value;
end;

end.
