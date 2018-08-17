unit HCCode128B;

{1、Code128A：标准数字和字母，控制符，特殊字符；
2、Code128B：标准数字和字母，小写字母，特殊字符；
3、Code128C/EAN128：[00]-[99]的数字对集合，共100个，即只能表示偶数位长度的数字。

如果使用B和C混合编码，可减少条码长度
a1234

头          StarB   211241    104(B表)
第一字符    a       121124    65(B表)
后面是C码   codec   113141    99(B表)
12          12      112232    12(C表)
34          34      131123    34(C表)
校验码      24      311222    104 + 65 + 2*99 + 3*12 + 4*34 = 539 mod 103 = 24
尾          Stop    2331112
}

interface

uses
  Classes, SysUtils, Graphics;

const
  CodeBTable: array[0..106, 0..2] of PChar = (
    ('0', '', '212222'),   ('1', '!', '222122'),  ('2', '"', '222221'),  ('3', '#', '121223'),  ('4', '$', '121322'),
    ('5', '%', '131222'),  ('6', '&', '122213'),  ('7', '''', '122312'), ('8', '(', '132212'),  ('9', ')', '221213'),
    ('10', '*', '221312'), ('11', '+', '231212'), ('12', ',', '112232'), ('13', '-', '122132'), ('14', '.', '122231'),
    ('15', '/', '113222'), ('16', '0', '123122'), ('17', '1', '123221'), ('18', '2', '223211'), ('19', '3', '221132'),
    ('20', '4', '221231'), ('21', '5', '213212'), ('22', '6', '223112'), ('23', '7', '312131'), ('24', '8', '311222'),
    ('25', '9', '321122'), ('26', ':', '321221'), ('27', ';', '312212'), ('28', '<', '322112'), ('29', '=', '322211'),
    ('30', '>', '212123'), ('31', '?', '212321'), ('32', '@', '232121'), ('33', 'A', '111323'), ('34', 'B', '131123'),
    ('35', 'C', '131321'), ('36', 'D', '112313'), ('37', 'E', '132113'), ('38', 'F', '132311'), ('39', 'G', '211313'),
    ('40', 'H', '231113'), ('41', 'I', '231311'), ('42', 'J', '112133'), ('43', 'K', '112331'), ('44', 'L', '132131'),
    ('45', 'M', '113123'), ('46', 'N', '113321'), ('47', 'O', '133121'), ('48', 'P', '313121'), ('49', 'Q', '211331'),
    ('50', 'R', '231131'), ('51', 'S', '213113'), ('52', 'T', '213311'), ('53', 'U', '213131'), ('54', 'V', '311123'),
    ('55', 'W', '311321'), ('56', 'X', '331121'), ('57', 'Y', '312113'), ('58', 'Z', '312311'), ('59', '[', '332111'),
    ('60', '\', '314111'), ('61', ']', '221411'), ('62', '^', '431111'), ('63', '_', '111224'), ('64', '`', '111422'),
    ('65', 'a', '121124'), ('66', 'b', '121421'), ('67', 'c', '141122'), ('68', 'd', '141221'), ('69', 'e', '112214'),
    ('70', 'f', '112412'), ('71', 'g', '122114'), ('72', 'h', '122411'), ('73', 'i', '142112'), ('74', 'j', '142211'),
    ('75', 'k', '241211'), ('76', 'l', '221114'), ('77', 'm', '413111'), ('78', 'n', '241112'), ('79', 'o', '134111'),
    ('80', 'p', '111242'), ('81', 'q', '121142'), ('82', 'r', '121241'), ('83', 's', '114212'), ('84', 't', '124112'),
    ('85', 'u', '124211'), ('86', 'v', '411212'), ('87', 'w', '421112'), ('88', 'x', '421211'), ('89', 'y', '212141'),
    ('90', 'z', '214121'), ('91', '{', '412121'), ('92', '|', '111143'), ('93', '}', '111341'), ('94', '~', '131141'),
    ('95', 'DEL', '114113'), ('96', 'FNC 3', '114311'), ('97', 'FNC 2', '411113'), ('98', 'SHIFT', '411311'),
    ('99', 'CODE C', '113141'), ('100', 'FNC 4', '114131'), ('101', 'CODE A', '311141'), ('102', 'FNC 1', '411131'),
    ('103', 'Start A', '211412'), ('104', 'Start B', '211214'), ('105', 'Start C', '211232'), ('106', 'Stop', '2331112')
    );

  CodeCTable: array[0..106, 0..2] of PChar = (
    ('0', '0', '212222'),   ('1', '1', '222122'),   ('2', '2', '222221'),   ('3', '3', '121223'),   ('4', '4', '121322'),
    ('5', '5', '131222'),   ('6', '6', '122213'),   ('7', '7', '122312'),   ('8', '8', '132212'),   ('9', '9', '221213'),
    ('10', '10', '221312'), ('11', '11', '231212'), ('12', '12', '112232'), ('13', '13', '122132'), ('14', '14', '122231'),
    ('15', '15', '113222'), ('16', '16', '123122'), ('17', '17', '123221'), ('18', '18', '223211'), ('19', '19', '221132'),
    ('20', '20', '221231'), ('21', '21', '213212'), ('22', '22', '223112'), ('23', '23', '312131'), ('24', '24', '311222'),
    ('25', '25', '321122'), ('26', '26', '321221'), ('27', '27', '312212'), ('28', '28', '322112'), ('29', '29', '322211'),
    ('30', '30', '212123'), ('31', '31', '212321'), ('32', '32', '232121'), ('33', '33', '111323'), ('34', '34', '131123'),
    ('35', '35', '131321'), ('36', '36', '112313'), ('37', '37', '132113'), ('38', '38', '132311'), ('39', '39', '211313'),
    ('40', '40', '231113'), ('41', '41', '231311'), ('42', '42', '112133'), ('43', '43', '112331'), ('44', '44', '132131'),
    ('45', '45', '113123'), ('46', '46', '113321'), ('47', '47', '133121'), ('48', '48', '313121'), ('49', '49', '211331'),
    ('50', '50', '231131'), ('51', '51', '213113'), ('52', '52', '213311'), ('53', '53', '213131'), ('54', '54', '311123'),
    ('55', '55', '311321'), ('56', '56', '331121'), ('57', '57', '312113'), ('58', '58', '312311'), ('59', '59', '332111'),
    ('60', '60', '314111'), ('61', '61', '221411'), ('62', '62', '431111'), ('63', '63', '111224'), ('64', '64', '111422'),
    ('65', '65', '121124'), ('66', '66', '121421'), ('67', '67', '141122'), ('68', '68', '141221'), ('69', '69', '112214'),
    ('70', '70', '112412'), ('71', '71', '122114'), ('72', '72', '122411'), ('73', '73', '142112'), ('74', '74', '142211'),
    ('75', '75', '241211'), ('76', '76', '221114'), ('77', '77', '413111'), ('78', '78', '241112'), ('79', '79', '134111'),
    ('80', '80', '111242'), ('81', '81', '121142'), ('82', '82', '121241'), ('83', '83', '114212'), ('84', '84', '124112'),
    ('85', '85', '124211'), ('86', '86', '411212'), ('87', '87', '421112'), ('88', '88', '421211'), ('89', '89', '212141'),
    ('90', '90', '214121'), ('91', '91', '412121'), ('92', '92', '111143'), ('93', '93', '111341'), ('94', '94', '131141'),
    ('95', '95', '114113'), ('96', '96', '114311'), ('97', '97', '411113'), ('98', '98', '411311'), ('99', '99', '113141'),
    ('100', 'CODE B','114131'), ('101', 'CODE A','311141'), ('102', 'FNC 1','411131'), ('103', 'Start A','211412'),
    ('104', 'Start B','211214'), ('105', 'Start C','211232'), ('106', 'Stop','2331112')
    );

type
  THCCode128B = class(TObject)
  private
    { Private declarations }
    FCodeKey: string;
    FShowCodeKey: Boolean;
    FWidth, FHeight, FPenWidth, FMargin: Integer;  // 条码宽度的放大倍数，考虑到画笔本身的宽度，最好设置为2的倍数
    procedure CalcWidth;
    function GetCode: string;
    procedure SetCodeKey(Value: string);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    procedure PaintToEx(const ACanvas: TCanvas);
  published
    property CodeKey: string read FCodeKey write SetCodeKey;
    property PenWidth: Integer read FPenWidth write FPenWidth;
    property ShowCodeKey: Boolean read FShowCodeKey write FShowCodeKey;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

implementation

{ THCCode128B }

procedure THCCode128B.CalcWidth;
var
  i, vWidth: Integer;
  vCode128B: string;
begin
  if FCodeKey = '' then
    FWidth := 100;

  vCode128B := GetCode;
  vWidth := 0;

  for i := 1 to Length(vCode128B) do
  begin
    vWidth := StrToInt(vCode128B[i]) * FPenWidth;  // Copy(vCode128B, i, 1)
    FWidth := FWidth + vWidth;
  end;

  FWidth := FWidth + FMargin + FMargin;
end;

constructor THCCode128B.Create;
begin
  FPenWidth := 2;
  FMargin := 5;
  FHeight := 50;
  FShowCodeKey := True;
end;

destructor THCCode128B.Destroy;
begin
  inherited Destroy;
end;

function THCCode128B.GetCode: string;
type
  TBandCodeArr = array of string;
  TCodeIdArr = array of Integer;

var
  vBandDcodeArr: TBandCodeArr;
  vIdArr: TCodeIdArr;  // 数据索引
  i, j, vCheck,  // 校验位
  vDataLen
    : Integer;

  vKey: string;  // strindex,字符中某位 c条码代码字符串
  vNullKey: Boolean;
begin
  Result := '';

  // 从数据库中提取代码
  vDataLen := Length(FCodeKey);
  SetLength(vBandDcodeArr, vDataLen);
  SetLength(vIdArr, vDataLen);

  for i := 0 to vDataLen - 1 do
  begin
    vKey := FCodeKey[i + 1];  // Copy(FCodeKey, i + 1, 1);
    vNullKey := True;

    for j := 0 to Length(CodeBTable) - 1 do
    begin
      if vKey = CodeBTable[j][1] then
      begin
        vIdArr[i] := StrToInt(CodeBTable[j][0]);  // 码表中的序号
        vBandDcodeArr[i] := CodeBTable[j][2];  // 码值 BandCode
        vNullKey := False;
        Break;
      end;
    end;

    if vNullKey then
      raise Exception.Create('HCCode128：存在无法编码的字符,请核对！');
  end;

  {vIdArr[4] := 0;
  vIdArr[3] := 34;
  vIdArr[2] := 12;
  vIdArr[1] := 99;
  vIdArr[0] := 65;}
  // 计算校验位
  vCheck := 104;  // 开始位StartB对应的ID为103
  for i := 0 to vDataLen - 1 do
    vCheck := vCheck + (i + 1) * vIdArr[i];

  vCheck := vCheck mod 103;

  // 合成条码的数字代码
  Result := '211214';  // 211214为起始位StartB的BandCode
  for i := 0 to vDataLen - 1 do
    Result := Result + vBandDcodeArr[i];
  //Result := Result + vBandDcodeArr[0] + '113141' + '112232' + '131123';

  SetLength(vBandDcodeArr, 0);
  SetLength(vIdArr, 0);

  for j := 0 to Length(CodeBTable) - 1 do
  begin
    if vCheck = StrToInt(CodeBTable[j][0]) then
    begin
      Result := Result + CodeBTable[j][2] + '2331112';  // 2331112为终止位Stop的BandCode
      Break;
    end;
  end;
end;

procedure THCCode128B.PaintToEx(const ACanvas: TCanvas);
var
  i, vPenMovWidth,  // 为画笔移动单位
  vKeyHeight, viKey
    : Integer;
  vCode128B: string;  // strindex,字符中某位 c条码代码字符串
begin
  if FCodeKey = '' then Exit;

  if FShowCodeKey then
  begin
    ACanvas.Font.Size := 8;
    vKeyHeight := FMargin + ACanvas.TextHeight('H');
  end
  else
    vKeyHeight := 0;

  vCode128B := GetCode;
  vPenMovWidth := 0;

  // 根据数字代码绘制条码，设数字代码的1为单位1
  //ACanvas.Pen.Style := psInsideFrame;  // 实线; 但笔宽是向里扩展
  for i := 1 to Length(vCode128B) do
  begin
    if i mod 2 <> 0 then  // 奇数
      ACanvas.Pen.Color := clblack
    else
      ACanvas.Pen.Color := clwhite;

    viKey := StrToInt(vCode128B[i]);  // StrToInt(Copy(vCode128b, i, 1));
    ACanvas.Pen.Width := viKey * FPenWidth;
    ACanvas.MoveTo(FMargin + vPenMovWidth + viKey, FMargin);
    ACanvas.LineTo(FMargin + vPenMovWidth + viKey, FHeight - vKeyHeight - FMargin);
    vPenMovWidth := vPenMovWidth + viKey * FPenWidth;
  end;

  // 抹白上下毛边
  ACanvas.Brush.Color := clwhite;
  ACanvas.FillRect(Rect(0, 0, FWidth, FMargin));
  ACanvas.FillRect(Rect(0, FHeight - vKeyHeight - FMargin, FWidth, FHeight));

  if FShowCodeKey then
    ACanvas.TextOut((FWidth - ACanvas.TextWidth(FCodeKey)) div 2, FHeight - vKeyHeight, FCodeKey);
end;

procedure THCCode128B.SetCodeKey(Value: string);
begin
  if FCodeKey <> Value then
  begin
    FCodeKey := Value;
    CalcWidth;
  end;
end;

end.
