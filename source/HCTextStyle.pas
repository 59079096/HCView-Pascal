{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  Classes, Graphics, SysUtils;

type
  TFontStyleEx = (tsBold, tsItalic, tsUnderline, tsStrikeOut, tsSuperscript,
    tsSubscript);
  TFontStyleExs = set of TFontStyleEx;

  TTextStyle = class(TPersistent)
  private const
    DefaultFontSize: Integer = 11;  // 五号
    DefaultFontFamily = '宋体';
    MaxFontSize: Single = 512;
  strict private
    FSize: Integer;
    FFamily: TFontName;
    FFontStyle: TFontStyleExs;
    FColor: TColor;
    FBackColor: TColor;
  protected
    procedure SetFamily(const Value: TFontName);
    procedure SetSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyleExs);
  public
    CheckSaveUsed: Boolean;
    TempNo: Integer;
    constructor Create;
    destructor Destroy; override;
    function IsSizeStored: Boolean;
    function IsFamilyStored: Boolean;
    procedure ApplyStyle(const ACanvas: TCanvas);
    function EqualsEx(const ASource: TTextStyle): Boolean;
    procedure AssignEx(const ASource: TTextStyle);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored;
    property Size: Integer read FSize write SetSize stored IsSizeStored nodefault;
    property FontStyle: TFontStyleExs read FFontStyle write SetFontStyle default [];
    property Color: TColor read FColor write FColor default clBlack;
    property BackColor: TColor read FBackColor write FBackColor default clWhite;
  end;

implementation

{ TTextStyle }

procedure TTextStyle.ApplyStyle(const ACanvas: TCanvas);
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
    Font.Size := FSize;
    if tsBold in FFontStyle then
      Font.Style := Font.Style + [TFontStyle.fsBold]
    else
      Font.Style := Font.Style - [TFontStyle.fsBold];

    if tsItalic in FFontStyle then
      Font.Style := Font.Style + [TFontStyle.fsItalic]
    else
      Font.Style := Font.Style - [TFontStyle.fsItalic];

    if tsUnderline in FFontStyle then
      Font.Style := Font.Style + [TFontStyle.fsUnderline]
    else
      Font.Style := Font.Style - [TFontStyle.fsUnderline];

    if tsStrikeOut in FFontStyle then
      Font.Style := Font.Style + [TFontStyle.fsStrikeOut]
    else
      Font.Style := Font.Style - [TFontStyle.fsStrikeOut];

    if tsSuperscript in FFontStyle then
      Font.Size := Font.Size div 2
    else
    if tsSubscript in FFontStyle then
      Font.Size := Font.Size div 2;
  end;
end;

procedure TTextStyle.AssignEx(const ASource: TTextStyle);
begin
  Self.FSize := ASource.Size;
  Self.FFontStyle := ASource.FontStyle;
  Self.FFamily := ASource.Family;
  Self.FColor := ASource.Color;
  Self.FBackColor := ASource.BackColor;
end;

constructor TTextStyle.Create;
begin
  FSize := DefaultFontSize;
  FFamily := DefaultFontFamily;
  FFontStyle := [];
  FColor := clBlack;
  FBackColor := clNone;
end;

destructor TTextStyle.Destroy;
begin

  inherited;
end;

function TTextStyle.EqualsEx(const ASource: TTextStyle): Boolean;
begin
  Result :=
    (Self.FSize = ASource.Size)
    and (Self.FFontStyle = ASource.FontStyle)
    and (Self.FFamily = ASource.Family)
    and (Self.FColor = ASource.Color)
    and (Self.FBackColor = ASource.BackColor);
end;

function TTextStyle.IsFamilyStored: Boolean;
begin
  Result := FFamily <> DefaultFontFamily;
end;

function TTextStyle.IsSizeStored: Boolean;
begin
  Result := FSize = DefaultFontSize;
end;

procedure TTextStyle.LoadFromStream(const AStream: TStream; const AFileVersion: Word);
var
  vSize: Word;
  vBuffer: TBytes;
begin
  AStream.ReadBuffer(FSize, SizeOf(FSize));  // 字号
  // 字体
  AStream.ReadBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    SetLength(vBuffer, vSize);
    AStream.Read(vBuffer[0], vSize);
    FFamily := StringOf(vBuffer);
  end;

  AStream.ReadBuffer(FFontStyle, SizeOf(FFontStyle));
  AStream.ReadBuffer(FColor, SizeOf(FColor));
  AStream.ReadBuffer(FBackColor, SizeOf(FBackColor));
end;

procedure TTextStyle.SaveToStream(const AStream: TStream);
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

  AStream.WriteBuffer(FFontStyle, SizeOf(FFontStyle));
  AStream.WriteBuffer(FColor, SizeOf(FColor));
  AStream.WriteBuffer(FBackColor, SizeOf(FBackColor));
end;

procedure TTextStyle.SetFamily(const Value: TFontName);
begin
  if FFamily <> Value then
    FFamily := Value;
end;

procedure TTextStyle.SetSize(const Value: Integer);
begin
  if FSize <> Value then
    FSize := Value;
end;

procedure TTextStyle.SetFontStyle(const Value: TFontStyleExs);
begin
  if FFontStyle <> Value then
    FFontStyle := Value;
end;

end.
