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
  Windows, Classes, Graphics, SysUtils;

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
    destructor Destroy; override;
    function IsSizeStored: Boolean;
    function IsFamilyStored: Boolean;
    procedure ApplyStyle(const ACanvas: TCanvas; const AScale: Single = 1);
    function EqualsEx(const ASource: THCTextStyle): Boolean;
    procedure AssignEx(const ASource: THCTextStyle);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
    property FontStyles: THCFontStyles read FFontStyles write SetFontStyles default [];
    property Color: TColor read FColor write FColor default clBlack;
    property BackColor: TColor read FBackColor write FBackColor default clWhite;
  end;

implementation

{ THCTextStyle }

procedure THCTextStyle.ApplyStyle(const ACanvas: TCanvas; const AScale: Single = 1);
var
  vFont: TFont;
  vLogFont: TLogFont;
begin
  with ACanvas do
  begin
    if FBackColor = clNone then
      Brush.Style := bsClear
    else
    begin
      Brush.Style := bsSolid;
      Brush.Color := FBackColor;
    end;
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

destructor THCTextStyle.Destroy;
begin

  inherited;
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
  AStream.ReadBuffer(FColor, SizeOf(FColor));
  AStream.ReadBuffer(FBackColor, SizeOf(FBackColor));
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
  AStream.WriteBuffer(FColor, SizeOf(FColor));
  AStream.WriteBuffer(FBackColor, SizeOf(FBackColor));
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

procedure THCTextStyle.SetFontStyles(const Value: THCFontStyles);
begin
  if FFontStyles <> Value then
    FFontStyles := Value;
end;

end.
