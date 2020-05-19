{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2020-3-17             }
{                                                       }
{                  Code128条码实现单元                  }
{                                                       }
{*******************************************************}

unit HCCode128;

interface

uses
  Windows, Classes, SysUtils, Graphics;

type
  THCCode128Encoding = (encA, encB, encC, encAorB, encNone);
  THCCodeLineType = (White, Black, BlackHalf, BlackTrack, BlackAscend, BlackDescend);
  THCCode128 = class
  private
    FWidth, FHeight, FModul: Integer;
    FModules: array[0..3] of Byte;
    FZoom: Byte;
    FText: string;
    FTextVisible: Boolean;
    FCode: AnsiString;
    FOnWidthChanged: TNotifyEvent;
    procedure CalcWidth;
    procedure OneBarProps(ACode: AnsiChar; var AWidth: Integer; var ALineType: THCCodeLineType);
    procedure SetText(const Value: string);
    procedure SetZoom(const Value: Byte);
    function GetCode(const AText: string): AnsiString;
    function GetBarWidth(const ACode: string): Integer;
  public
    constructor Create(const AText: string);
    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRect);
    property Text: string read FText write SetText;
    property TextVisible: Boolean read FTextVisible write FTextVisible;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight write FHeight;
    property Zoom: Byte read FZoom write SetZoom;
    property OnWidthChanged: TNotifyEvent read FOnWidthChanged write FOnWidthChanged;
  end;

implementation

const
  Table128: array[0..105] of array[0..3] of ShortString = (
  (' ', ' ', '00', '212222'), ('!', '!', '01', '222122'), ('"', '"', '02', '222221'),
  ('#', '#', '03', '121223'), ('$', '$', '04', '121322'), ('%', '%', '05', '131222'),
  ('&', '&', '06', '122213'), ('''', '''', '07', '122312'), ('(', '(', '08', '132212'),
  (')', ')', '09', '221213'), ('*', '*', '10', '221312'), ('+', '+', '11', '231212'),
  (',', ',', '12', '112232'), ('-', '-', '13', '122132'), ('.', '.', '14', '122231'),
  ('/', '/', '15', '113222'), ('0', '0', '16', '123122'), ('1', '1', '17', '123221'),
  ('2', '2', '18', '223211'), ('3', '3', '19', '221132'), ('4', '4', '20', '221231'),
  ('5', '5', '21', '213212'), ('6', '6', '22', '223112'), ('7', '7', '23', '312131'),
  ('8', '8', '24', '311222'), ('9', '9', '25', '321122'), (':', ':', '26', '321221'),
  (';', ';', '27', '312212'), ('<', '<', '28', '322112'), ('=', '=', '29', '322211'),
  ('>', '>', '30', '212123'), ('?', '?', '31', '212321'), ('@', '@', '32', '232121'),
  ('A', 'A', '33', '111323'), ('B', 'B', '34', '131123'), ('C', 'C', '35', '131321'),
  ('D', 'D', '36', '112313'), ('E', 'E', '37', '132113'), ('F', 'F', '38', '132311'),
  ('G', 'G', '39', '211313'), ('H', 'H', '40', '231113'), ('I', 'I', '41', '231311'),
  ('J', 'J', '42', '112133'), ('K', 'K', '43', '112331'), ('L', 'L', '44', '132131'),
  ('M', 'M', '45', '113123'), ('N', 'N', '46', '113321'), ('O', 'O', '47', '133121'),
  ('P', 'P', '48', '313121'), ('Q', 'Q', '49', '211331'), ('R', 'R', '50', '231131'),
  ('S', 'S', '51', '213113'), ('T', 'T', '52', '213311'), ('U', 'U', '53', '213131'),
  ('V', 'V', '54', '311123'), ('W', 'W', '55', '311321'), ('X', 'X', '56', '331121'),
  ('Y', 'Y', '57', '312113'), ('Z', 'Z', '58', '312311'), ('[', '[', '59', '332111'),
  ('\', '\', '60', '314111'), (']', ']', '61', '221411'), ('^', '^', '62', '431111'),
  ('_', '_', '63', '111224'), (#00, '`', '64', '111422'), (#01, 'a', '65', '121124'),
  (#02, 'b', '66', '121421'), (#03, 'c', '67', '141122'), (#04, 'd', '68', '141221'),
  (#05, 'e', '69', '112214'), (#06, 'f', '70', '112412'), (#07, 'g', '71', '122114'),
  (#08, 'h', '72', '122411'), (#09, 'i', '73', '142112'), (#10, 'j', '74', '142211'),
  (#11, 'k', '75', '241211'), (#12, 'l', '76', '221114'), (#13, 'm', '77', '413111'),
  (#14, 'n', '78', '241112'), (#15, 'o', '79', '134111'), (#16, 'p', '80', '111242'),
  (#17, 'q', '81', '121142'), (#18, 'r', '82', '121241'), (#19, 's', '83', '114212'),
  (#20, 't', '84', '124112'), (#21, 'u', '85', '124211'), (#22, 'v', '86', '411212'),
  (#23, 'w', '87', '421112'), (#24, 'x', '88', '421211'), (#25, 'y', '89', '212141'),
  (#26, 'z', '90', '214121'), (#27, '{', '91', '412121'), (#28, '|', '92', '111143'),
  (#29, '}', '93', '111341'), (#30, '~', '94', '131141'), (#31, ' ', '95', '114113'),
  (' ', ' ', '96', '114311'), (' ', ' ', '97', '411113'), (' ', ' ', '98', '411311'),
  (' ', ' ', '99', '113141'), (' ', ' ', '  ', '114131'), (' ', ' ', '  ', '311141'),
  (' ', ' ', '  ', '411131'), (' ', ' ', '  ', '211412'), (' ', ' ', '  ', '211214'),
  (' ', ' ', '  ', '211232'));

{ THCCode128 }

procedure THCCode128.CalcWidth;
var
  vW: Integer;
begin
  if FCode <> '' then
    vW := GetBarWidth(FCode) * FZoom
  else
    vW := 60;

  if FWidth <> vW then
  begin
    FWidth := vW;
    if Assigned(FOnWidthChanged) then
      FOnWidthChanged(Self);
  end;
end;

constructor THCCode128.Create(const AText: string);
begin
  FTextVisible := True;
  FModul := 1;
  FZoom := 1;
  FHeight := 100;
  SetText(AText);
end;

function THCCode128.GetBarWidth(const ACode: string): Integer;
var
  i, vW: Integer;
  vLineType: THCCodeLineType;
begin
  Result := 0;
  FModules[0] := FModul;
  FModules[1] := FModul * 2;  // 2为宽条宽度
  FModules[2] := FModules[1] * 3 div 2;
  FModules[3] := FModules[1] * 2;

  for i := 1 to Length(FCode) do
  begin
    OneBarProps(FCode[i], vW, vLineType);
    Inc(Result, vW);
  end;
end;

function GetNextChar(const ACode: AnsiString; var AIndex: Integer; var AEncoding: THCCode128Encoding): AnsiString;
var
  vC: AnsiString;
begin
  Result := '';
  if AIndex > Length(ACode) then
    Exit;

  if (ACode[AIndex] = '&') and (AIndex + 2 <= Length(ACode)) and (ACode[AIndex + 2] = ';') then
  begin
    vC := AnsiString(AnsiUpperCase(String(ACode[AIndex + 1])));
    if (vC = 'A') or (vC = 'B') or (vC = 'C') or (vC = 'S') or (vC = '1') or (vC = '2') or (vC = '3') or (vC = '4') then
    begin
      Inc(AIndex, 3);
      Result := '&' + vC + ';';
      Exit;
    end;
  end;

  if (AEncoding = encC) and (AIndex + 1 <= Length(ACode)) then
  begin
    Result := Copy(ACode, AIndex, 2);
    Inc(AIndex, 2);
    Exit;
  end;

  Result := Copy(ACode, AIndex, 1);
  Inc(AIndex);
end;

function StripControlCodes(const ACode: AnsiString; AStripFNCodes: Boolean): AnsiString;
var
  vIndex: Integer;
  vNextChar: AnsiString;
  vEncoding: THCCode128Encoding;
begin
  Result := '';
  vIndex := 1;
  vEncoding := encNone;

  while vIndex <= Length(ACode) do
  begin
    vNextChar := GetNextChar(ACode, vIndex, vEncoding);
    if (vNextChar <> '&A;') and (vNextChar <> '&B;') and (vNextChar <> '&C;') and (vNextChar <> '&S;') then
    begin
      if (not AStripFNCodes) or ((vNextChar <> '&1;') and (vNextChar <> '&2;') and (vNextChar <> '&3;') and (vNextChar <> '&4;')) then
        Result := Result + vNextChar;
    end;
  end;
end;

function IsDigit(AChar: AnsiChar): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function IsFourOrMoreDigits(const ACode: AnsiString; AIndex: Integer; var ANumDigits: Integer): Boolean;
begin
  ANumDigits := 0;
  if IsDigit(ACode[AIndex]) and (AIndex + 4 <= Length(ACode)) then
  begin
    while (AIndex + ANumDigits <= Length(ACode)) and IsDigit(ACode[AIndex + ANumDigits]) do
      Inc(ANumDigits);
  end;

  Result := ANumDigits >= 4;
end;

function FindCodeA(AChar: AnsiChar): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(Table128) - 1 do
  begin
    if AChar = Table128[i][0] then
    begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

function FindCodeB(AChar: AnsiChar): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(Table128) - 1 do
  begin
    if AChar = Table128[i][1] then
    begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

function FindCodeC(const ACode: AnsiString): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(Table128) - 1 do
  begin
    if ACode = Table128[i][2] then
    begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

function GetNextPortion(const ACode: AnsiString; var AIndex: Integer; var AEncoding: THCCode128Encoding): AnsiString;
var
  vaIndex, vbIndex, vNumDigits, vNumChars: Integer;
  vFirstCharEncoding, vNextCharEncoding: THCCode128Encoding;
  vPrefix, vChar: AnsiString;
begin
  Result := '';
  if AIndex > Length(ACode) then
    Exit;

  vChar := '';
  if (ACode[AIndex] = '&') and (AIndex + 2 <= Length(ACode)) and (ACode[AIndex + 2] = ';') then
  begin
    vChar := AnsiString(AnsiUpperCase(String(ACode[AIndex + 1])));
    if (vChar = 'A') or (vChar = 'B') or (vChar = 'C') or (vChar = 'S') or (vChar = '1') or (vChar = '2') or (vChar = '3') or (vChar = '4') then
    begin
      vChar := Copy(ACode, AIndex, 3);
      Inc(AIndex, 3);
    end
    else
      vChar := '';
  end;

  vaIndex := FindCodeA(ACode[AIndex]);
  vbIndex := FindCodeB(ACode[AIndex]);
  vFirstCharEncoding := encA;
  if (vaIndex = -1) and (vbIndex <> -1) then
    vFirstCharEncoding := encB
  else
  if (vaIndex <> -1) and (vbIndex <> -1) then
    vFirstCharEncoding := encAorB;

  vNumDigits := 0;
  if IsFourOrMoreDigits(ACode, AIndex, vNumDigits) then
    vFirstCharEncoding := encC;

  if vFirstCharEncoding = encC then
  begin
    vNumDigits := (vNumDigits div 2) * 2;
    Result := Copy(ACode, AIndex, vNumDigits);
    Inc(AIndex, vNumDigits);
    if AEncoding <> encC then
      Result := '&C;' + vChar + Result
    else
      Result := vChar + Result;
    AEncoding := encC;
    Exit;
  end;

  vNumChars := 1;
  while AIndex + vNumChars <= Length(ACode) do
  begin
    vaIndex := FindCodeA(ACode[AIndex + vNumChars]);
    vbIndex := FindCodeB(ACode[AIndex + vNumChars]);
    vNextCharEncoding := encA;
    if (vaIndex = -1) and (vbIndex <> -1) then
      vNextCharEncoding := encB
    else
    if (vaIndex <> -1) and (vbIndex <> -1) then
      vNextCharEncoding := encAorB;

    if IsFourOrMoreDigits(ACode, AIndex + vNumChars, vNumDigits) then
      vNextCharEncoding := encC;

    if (vNextCharEncoding <> encC) and (vNextCharEncoding <> vFirstCharEncoding) then
    begin
      if vFirstCharEncoding = encAorB then
        vFirstCharEncoding := vNextCharEncoding
      else
      if vNextCharEncoding = encAorB then
        vNextCharEncoding := vFirstCharEncoding;
    end;

    if vFirstCharEncoding <> vNextCharEncoding then
      Break;

    Inc(vNumChars);
  end;

  if vFirstCharEncoding = encAorB then
    vFirstCharEncoding := encB;

  if vFirstCharEncoding = encA then
    vPrefix := '&A;'
  else
    vPrefix := '&B;';

  if (AEncoding <> vFirstCharEncoding) and
    (vNumChars = 1) and
    ((AEncoding = encA) or (AEncoding = encB)) and
    ((vFirstCharEncoding = encA) or (vFirstCharEncoding = encB))
  then
    vPrefix := '&S;'
  else
    AEncoding := vFirstCharEncoding;

  Result := vPrefix + vChar + Copy(ACode, AIndex, vNumChars);
  Inc(AIndex, vNumChars);
end;

function Encode(ACode: AnsiString): AnsiString;
var
  vIndex: Integer;
  vEncoding: THCCode128Encoding;
begin
  ACode := StripControlCodes(ACode, False);
  Result := '';
  vIndex := 1;
  vEncoding := encNone;

  while vIndex <= Length(ACode) do
    Result := Result + GetNextPortion(ACode, vIndex, vEncoding);
end;

function Convert(const s:AnsiString):AnsiString;
var
  i, v : integer;
begin
  Result := s;
  for i := 1 to Length(s) do
  begin
    v := ord(s[i]) - 1;

    if odd(i) then
      Inc(v, 5);

    Result[i] := AnsiChar(Chr(v));
  end;
end;

function THCCode128.GetCode(const AText: string): AnsiString;
var
  vCode, vNextChar, vStartCode, vCheckSumCode: AnsiString;
  vEncoding: THCCode128Encoding;
  vIndex, vChecksum, vCodeWordPos, vIdx: Integer;
begin
  vCode := AText;
  vCode := AnsiString(StringReplace(String(vCode), '&FNC1;', '&1;', [rfReplaceAll]));
  vCode := Encode(vCode);

  vEncoding := encNone;
  vIndex := 1;
  vNextChar := GetNextChar(vCode, vIndex, vEncoding);
  vStartCode := '';

  if vNextChar = '&A;' then
  begin
    vEncoding := encA;
    vChecksum := 103;
    vStartCode := Table128[103][3];
  end
  else
  if vNextChar = '&B;' then
  begin
    vEncoding := encB;
    vChecksum := 104;
    vStartCode := Table128[104][3];
  end
  else
  if vNextChar = '&C;' then
  begin
    vEncoding := encC;
    vChecksum := 105;
    vStartCode := Table128[105][3];
  end
  else
    raise Exception.Create('无效的条码内容！');

  Result := vStartCode;
  vCodeWordPos := 1;

  while vIndex <= Length(vCode) do
  begin
    vNextChar := GetNextChar(vCode, vIndex, vEncoding);

    if vNextChar = '&A;' then
    begin
      vEncoding := encA;
      vIdx := 101;
    end
    else
    if vNextChar = '&B;' then
    begin
      vEncoding := encB;
      vIdx := 100;
    end
    else
    if vNextChar = '&C;' then
    begin
      vEncoding := encC;
      vIdx := 99;
    end
    else
    if vNextChar = '&S;' then
    begin
      if vEncoding = encA then
        vEncoding := encB
      else
        vEncoding := encA;
      vIdx := 98;
    end
    else
    if vNextChar = '&1;' then
      vIdx := 102
    else
    if vNextChar = '&2;' then
      vIdx := 97
    else
    if vNextChar = '&3;' then
      vIdx := 96
    else
    if vNextChar = '&4;' then
    begin
      if vEncoding = encA then
        vIdx := 101
      else
        vIdx := 100;
    end
    else
    begin
      if vEncoding = encA then
        vIdx := FindCodeA(vNextChar[1])
      else
      if vEncoding = encB then
        vIdx := FindCodeB(vNextChar[1])
      else
        vIdx := FindCodeC(vNextChar);
    end;

    if vIdx < 0 then
      raise Exception.Create('无效的条码内容！');

    Result := Result + Table128[vIdx][3];
    Inc(vChecksum, vIdx * vCodeWordPos);
    Inc(vCodeWordPos);

    if vNextChar = '&S;' then
    begin
      if vEncoding = encA then
        vEncoding := encB
      else
        vEncoding := encA;
    end;
  end;

  vChecksum := vChecksum mod 103;
  Result := Result + Table128[vChecksum][3];

  Result := Result + '2331112';
  Result := Convert(Result);
end;

procedure THCCode128.OneBarProps(ACode: AnsiChar; var AWidth: Integer;
  var ALineType: THCCodeLineType);
begin
  case ACode of
    '0':
      begin
        AWidth := FModules[0];
        ALineType := THCCodeLineType.White;
      end;

    '1':
      begin
        AWidth := FModules[1];
        ALineType := THCCodeLineType.White;
      end;

    '2':
      begin
        AWidth := FModules[2];
        ALineType := THCCodeLineType.White;
      end;

    '3':
      begin
        AWidth := FModules[3];
        ALineType := THCCodeLineType.White;
      end;

    '5':
      begin
        AWidth := FModules[0];
        ALineType := THCCodeLineType.Black;
      end;

    '6':
      begin
        AWidth := FModules[1];
        ALineType := THCCodeLineType.Black;
      end;

    '7':
      begin
        AWidth := FModules[2];
        ALineType := THCCodeLineType.Black;
      end;

    '8':
      begin
        AWidth := FModules[3];
        ALineType := THCCodeLineType.Black;
      end;

    'A':
      begin
        AWidth := FModules[0];
        ALineType := THCCodeLineType.BlackHalf;
      end;

    'B':
      begin
        AWidth := FModules[1];
        ALineType := THCCodeLineType.BlackHalf;
      end;

    'C':
      begin
        AWidth := FModules[2];
        ALineType := THCCodeLineType.BlackHalf;
      end;

    'D':
      begin
        AWidth := FModules[3];
        ALineType := THCCodeLineType.BlackHalf;
      end;

    'F':
      begin
        AWidth := FModules[0];
        ALineType := THCCodeLineType.BlackTrack;
      end;

    'G':
      begin
        AWidth := FModules[0];
        ALineType := THCCodeLineType.BlackAscend;
      end;

    'H':
      begin
        AWidth := FModules[0];
        ALineType := THCCodeLineType.BlackDescend;
      end;
  else
    raise Exception.Create('HCCode128计算宽度出错！');
  end;
end;

procedure THCCode128.PaintTo(const ACanvas: TCanvas; const ARect: TRect);
var
  i, vX, vWidth, vHeight: Integer;
  vRect: TRect;
  vLineType: THCCodeLineType;
begin
  vX := 0;
  vHeight := Self.Height;
  if FTextVisible then
    vHeight := vHeight - 12;

  for i := 1 to Length(FCode) do
  begin
    OneBarProps(FCode[i], vWidth, vLineType);
    if vLineType <> THCCodeLineType.White then
      ACanvas.Brush.Color := clBlack
    else
      ACanvas.Brush.Color := clWhite;

    vRect := Rect(vX, 0, vX + vWidth * FZoom, vHeight);
    vX := vRect.Right;
    OffsetRect(vRect, ARect.Left, ARect.Top);
    ACanvas.FillRect(vRect);
  end;

  if FCode = '' then
  begin
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Rectangle(ARect);
  end;

  if FTextVisible then
  begin
    ACanvas.Font.Size := 8;
    ACanvas.Font.Style := [];
    ACanvas.Font.Name := 'Arial';
    ACanvas.Font.Color := clBlack;
    ACanvas.Brush.Style := bsClear;
    if FCode <> '' then
    begin
      ACanvas.TextOut(ARect.Left + (ARect.Right - ARect.Left - ACanvas.TextWidth(FText)) div 2,
        ARect.Top + vHeight + 2, FText);
    end
    else
    begin
      ACanvas.TextOut(ARect.Left + (ARect.Right - ARect.Left - ACanvas.TextWidth('无效条码' + FText)) div 2,
        ARect.Top + (ARect.Bottom - ARect.Top - ACanvas.TextHeight('H')) div 2, '无效条码' +FText);
    end;
  end;
end;

procedure THCCode128.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    try
      FCode := GetCode(FText);
    except
      FCode := '';
    end;

    CalcWidth;
  end;
end;

procedure THCCode128.SetZoom(const Value: Byte);
begin
  if FZoom <> Value then
  begin
    FZoom := Value;
    CalcWidth;
  end;
end;

end.
