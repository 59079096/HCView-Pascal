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

  THCTextStyle = class(TPersistent)
  private const
    DefaultFontSize: Single = 10.5;  // 五号
    DefaultFontFamily = '宋体';
    MaxFontSize: Single = 512;
  strict private
    FSize: Single;
    FFamily: TFontName;
    FFontStyles: THCFontStyles;
    FColor: TColor;  // 字体颜色
    FBackColor: TColor;
  protected
    procedure SetFamily(const Value: TFontName);
    procedure SetSize(const Value: Single);
    procedure SetFontStyles(const Value: THCFontStyles);
  public
    CheckSaveUsed: Boolean;
    TempNo: Integer;
    constructor Create;
    function IsSizeStored: Boolean;
    function IsFamilyStored: Boolean;
    procedure ApplyStyle(const ACanvas: TCanvas; const AScale: Single = 1);
    function EqualsEx(const ASource: THCTextStyle): Boolean;
    procedure AssignEx(const ASource: THCTextStyle);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
    function ToCSS: string;
    function ToXml: string;
    procedure FromXml(const ANode: IHCXmlNode);
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
    property FontStyles: THCFontStyles read FFontStyles write SetFontStyles default [];
    property Color: TColor read FColor write FColor default clBlack;
    property BackColor: TColor read FBackColor write FBackColor default clWhite;
  end;

implementation

uses
  HCCommon;

{ THCTextStyle }

procedure THCTextStyle.ApplyStyle(const ACanvas: TCanvas; const AScale: Single = 1);
var
  vFont: TFont;
  vLogFont: TLogFont;
begin
  with ACanvas do
  begin
    Brush.Color := FBackColor;
    if FBackColor = clNone then
      Brush.Style := bsClear;

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

    //if AScale <> 1 then
    begin
      vFont := TFont.Create;
      try
        vFont.Assign(ACanvas.Font);
        GetObject(vFont.Handle, SizeOf(vLogFont), @vLogFont);

        if (tsSuperscript in FFontStyles) or (tsSubscript in FFontStyles) then
          vLogFont.lfHeight := -Round(FSize / 2 * GetDeviceCaps(ACanvas.Handle, LOGPIXELSY) / 72 / AScale)
        else
          vLogFont.lfHeight := -Round(FSize * GetDeviceCaps(ACanvas.Handle, LOGPIXELSY) / 72 / AScale);

        vFont.Handle := CreateFontIndirect(vLogFont);
        ACanvas.Font.Assign(vFont);
      finally
        vFont.Free;
      end;
    end;
  end;
end;

procedure THCTextStyle.AssignEx(const ASource: THCTextStyle);
begin
  Self.FSize := ASource.Size;
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
  FBackColor := clNone;
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

procedure THCTextStyle.FromXml(const ANode: IHCXmlNode);

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
        if vsStyles[i] = 'strike' then
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
  vSize: Word;
  vBuffer: TBytes;
begin
  if AFileVersion < 12 then
  begin
    AStream.ReadBuffer(vOldSize, SizeOf(vOldSize));  // 字号
    FSize := vOldSize;
  end
  else
    AStream.ReadBuffer(FSize, SizeOf(FSize));  // 字号

  // 字体
  AStream.ReadBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    SetLength(vBuffer, vSize);
    AStream.Read(vBuffer[0], vSize);
    FFamily := StringOf(vBuffer);
  end;

  AStream.ReadBuffer(FFontStyles, SizeOf(FFontStyles));

  if AFileVersion > 18 then
  begin
    LoadColorFromStream(FColor, AStream);
    LoadColorFromStream(FBackColor, AStream);
  end
  else
  begin
    AStream.ReadBuffer(FColor, SizeOf(FColor));
    AStream.ReadBuffer(FBackColor, SizeOf(FBackColor));
  end;
end;

procedure THCTextStyle.SaveToStream(const AStream: TStream);
var
  vBuffer: TBytes;
  vSize: Word;
begin
  AStream.WriteBuffer(FSize, SizeOf(FSize));

  vBuffer := BytesOf(FFamily);
  vSize := System.Length(vBuffer);
  AStream.WriteBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
    AStream.WriteBuffer(vBuffer[0], vSize);

  AStream.WriteBuffer(FFontStyles, SizeOf(FFontStyles));
  SaveColorToStream(FColor, AStream);
  SaveColorToStream(FBackColor, AStream);
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
    + Format(' color:rgb(%d, %d, %d);', [GetRValue(FColor), GetGValue(FColor), GetBValue(FColor)])
    + Format(' background-color:rgb(%d, %d, %d);', [GetRValue(FBackColor), GetGValue(FBackColor), GetBValue(FBackColor)]);
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

function THCTextStyle.ToXml: string;

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
        Result := Result + ',strike'
      else
        Result := 'strike';
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
  Result := '<ts size="' + FormatFloat('#.#', FSize) + '"'
    + ' color="' + GetColorXmlRGB(FColor) + '"'
    + ' bkcolor="' + GetColorXmlRGB(FBackColor) + '"'
    + ' style="' + GetFontStyleXML + '">'
    + Family + '</ts>';
end;

procedure THCTextStyle.SetFontStyles(const Value: THCFontStyles);
begin
  if FFontStyles <> Value then
    FFontStyles := Value;
end;

end.
