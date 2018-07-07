{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                  HCView代码公共单元                   }
{                                                       }
{*******************************************************}

unit HCCommon;

interface

uses
  Windows, Controls, Classes, Graphics, HCStyle;

const
  HC_TEXTMAXSIZE = 4294967295;
  HC_EXCEPTION = 'HC异常：';
  HCS_EXCEPTION_NULLTEXT = HC_EXCEPTION + '文本Item的内容出现为空的情况！';
  HCS_EXCEPTION_TEXTOVER = HC_EXCEPTION + 'TextItem的内容超出允许的最大字节数4294967295！';
  HCS_EXCEPTION_MEMORYLESS = HC_EXCEPTION + '复制时没有申请到足够的内存！';
  HCS_EXCEPTION_UNACCEPTDATATYPE = HC_EXCEPTION + '不可接受的数据类型！';

  HC_EXT = '.hcf';

  HC_FileVersion = '1.1';
  HC_FileVersionInt = 11;

  MinPadding = 20;
  PMSLineHeight = 24;  // 书写范围线的长度
  AnnotationWidth = 200;  // 批注显示区域宽度
  // 不能在行首的字符             |                    |                   |
  DontLineFirstChar = '`-=[]\;'',./~!@#$%^&*()_+{}|:"<>?·－＝【】＼；‘，。、～！＠＃￥％……＆×（）——＋｛｝｜：“《》？°';
  DontLineLastChar = '/\＼';

type
  TChangeProc = reference to function(): Boolean;

  TViewModel = (
    vmPage,  // 页面视图，显示页眉、页脚
    vmWeb  // Web视图，不显示页眉、页脚
  );

  TSectionArea = (saHeader, saData, saFooter);  // 当前激活的是文档哪一部分
  TSaveParts = set of TSectionArea;  // 保存时存哪几部分内容

  TPaperSize = (psCustom, ps4A0, ps2A0, psA0, psA1, psA2,
    psA3, psA4, psA5, psA6, psA7, psA8,
    psA9, psA10, psB0, psB1, psB2, psB3,
    psB4, psB5, psB6, psB7, psB8, psB9,
    psB10, psC0, psC1, psC2, psC3, psC4,
    psC5, psC6, psC7, psC8, psC9, psC10,
    psLetter, psLegal, psLedger, psTabloid,
    psStatement, psQuarto, psFoolscap, psFolio,
    psExecutive, psMonarch, psGovernmentLetter,
    psPost, psCrown, psLargePost, psDemy,
    psMedium, psRoyal, psElephant, psDoubleDemy,
    psQuadDemy, psIndexCard3_5, psIndexCard4_6,
    psIndexCard5_8, psInternationalBusinessCard,
    psUSBusinessCard, psEmperor, psAntiquarian,
    psGrandEagle, psDoubleElephant, psAtlas,
    psColombier, psImperial, psDoubleLargePost,
    psPrincess, psCartridge, psSheet, psHalfPost,
    psDoublePost, psSuperRoyal, psCopyDraught,
    psPinchedPost, psSmallFoolscap, psBrief, psPott,
    psPA0, psPA1, psPA2, psPA3, psPA4, psPA5,
    psPA6, psPA7, psPA8, psPA9, psPA10, psF4,
    psA0a, psJISB0, psJISB1, psJISB2, psJISB3,
    psJISB4, psJISB5, psJISB6, psJISB7, psJISB8,
    psJISB9, psJISB10, psJISB11, psJISB12,
    psANSI_A, psANSI_B, psANSI_C, psANSI_D,
    psANSI_E, psArch_A, psArch_B, psArch_C,
    psArch_D, psArch_E, psArch_E1,
    ps16K, ps32K);

  TCaretInfo = record
    X, Y, Height: Integer;
    Visible: Boolean;
  end;

  TMarkType = (cmtBeg, cmtEnd);

  TCaret = Class
  private
    FHeight: Integer;
    FOwnHandle: THandle;
  protected
    procedure SetHeight(const Value: Integer);
  public
    X, Y: Integer;
    //Visible: Boolean;
    constructor Create(const AHandle: THandle);
    destructor Destroy; override;
    procedure ReCreate;
    procedure Show(const AX, AY: Integer); overload;
    procedure Show; overload;
    procedure Hide;
    property Height: Integer read FHeight write SetHeight;
  end;

  function IsKeyPressWant(const AKey: Char): Boolean;
  function IsKeyDownWant(const AKey: Word): Boolean;

  function PosCharHC(const AChar: Char; const AStr: string{; const Offset: Integer = 1}): Integer;

  /// <summary>
  /// 返回指定位置在字符串哪个字符后面(0：第一个前面)
  /// </summary>
  /// <param name="ACanvas"></param>
  /// <param name="AText"></param>
  /// <param name="X"></param>
  /// <returns></returns>
  function GetCharOffsetByX(const ACanvas: TCanvas; const AText: string; const X: Integer): Integer;

  // 根据汉字大小获取字体数字大小
  function GetFontSize(const AFontSize: string): Integer;
  function GetFontSizeStr(AFontSize: Integer): string;
  function GetPaperSizeStr(APaperSize: Integer): string;

  function GetVersionAsInteger(const AVersion: string): Integer;

  /// <summary> 保存文件格式、版本 </summary>
  procedure _SaveFileFormatAndVersion(const AStream: TStream);
  /// <summary> 读取文件格式、版本 </summary>
  procedure _LoadFileFormatAndVersion(const AStream: TStream; var AFileFormat, AVersion: string);

  {$IFDEF DEBUG}
  procedure DrawDebugInfo(const ACanvas: TCanvas; const ALeft, ATop: Integer; const AInfo: string);
  {$ENDIF}

var
  GCursor: TCursor;
  HC_FILEFORMAT: Word;

implementation

uses
  SysUtils;

{$IFDEF DEBUG}
procedure DrawDebugInfo(const ACanvas: TCanvas; const ALeft, ATop: Integer; const AInfo: string);
var
  vFont: TFont;
begin
  vFont := TFont.Create;
  try
    vFont.Assign(ACanvas.Font);
    ACanvas.Font.Color := clGray;
    ACanvas.Font.Size := 8;
    ACanvas.Font.Style := [];
    ACanvas.Font.Name := 'Courier New';
    ACanvas.Brush.Style := bsClear;

    ACanvas.TextOut(ALeft, ATop, AInfo);
  finally
    ACanvas.Font.Assign(vFont);
    FreeAndNil(vFont);
  end;
end;
{$ENDIF}

function IsKeyPressWant(const AKey: Char): Boolean;
begin
  Result := AKey in [#32..#126];  // <#32是ASCII控制字 #127是ASCII DEL
end;

function IsKeyDownWant(const AKey: Word): Boolean;
begin
  Result := AKey in [VK_BACK, VK_DELETE, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_RETURN,
    VK_HOME, VK_END, VK_TAB];
end;

function PosCharHC(const AChar: Char; const AStr: string{; const Offset: Integer = 1}): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AStr) do
  begin
    if AChar = AStr[i] then
    begin
      Result := i;
      Exit
    end;
  end;
end;

function GetFontSize(const AFontSize: string): Integer;
begin
  if not TryStrToInt(AFontSize, Result) then
  begin
    if AFontSize = '初号' then Result := 42
    else
    if AFontSize = '小初' then Result := 36
    else
    if AFontSize = '一号' then Result := 26
    else
    if AFontSize = '小一' then Result := 24
    else
    if AFontSize = '二号' then Result := 22
    else
    if AFontSize = '小二' then Result := 18
    else
    if AFontSize = '三号' then Result := 16
    else
    if AFontSize = '小三' then Result := 15
    else
    if AFontSize = '四号' then Result := 14
    else
    if AFontSize = '小四' then Result := 12
    else
    if AFontSize = '五号' then Result := 11
    else
    if AFontSize = '小五' then Result := 9
    else
    if AFontSize = '六号' then Result := 8
    else
    if AFontSize = '小六' then Result := 7
    else
    if AFontSize = '七号' then Result := 5
    else
      raise Exception.Create(HC_EXCEPTION + '计算字号大小出错，无法识别的值：' + AFontSize);
  end;
end;

function GetFontSizeStr(AFontSize: Integer): string;
begin
  if AFontSize = 42 then Result := '初号'
  else
  if AFontSize = 36 then Result := '小初'
  else
  if AFontSize = 26 then Result := '一号'
  else
  if AFontSize = 24 then Result := '小一'
  else
  if AFontSize = 22 then Result := '二号'
  else
  if AFontSize = 18 then Result := '小二'
  else
  if AFontSize = 16 then Result := '三号'
  else
  if AFontSize = 15 then Result := '小三'
  else
  if AFontSize = 14 then Result := '四号'
  else
  if AFontSize = 12 then Result := '小四'
  else
  if AFontSize = 11 then Result := '五号'
  else
  if AFontSize = 9 then Result := '小五'
  else
  if AFontSize = 7 then Result := '六号'
  else
  if AFontSize = 6 then Result := '小六'
  else
  if AFontSize = 5 then Result := '七号'
  else
    Result := IntToStr(AFontSize);
end;

function GetPaperSizeStr(APaperSize: Integer): string;
begin
  case APaperSize of
    DMPAPER_A3: Result := 'A3';
    DMPAPER_A4: Result := 'A4';
    DMPAPER_A5: Result := 'A5';
    DMPAPER_B5: Result := 'B5';
  else
    Result := '自定义';
  end;
end;

function GetVersionAsInteger(const AVersion: string): Integer;
var
  vsVer: string;
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AVersion) do
  begin
    if AVersion[i] in ['0'..'9'] then
      vsVer := vsVer + AVersion[i];
  end;
  Result := vsVer.ToInteger;
end;

/// <summary> 保存文件格式、版本 </summary>
procedure _SaveFileFormatAndVersion(const AStream: TStream);
var
  vS: string;
begin
  vS := HC_EXT;
  AStream.WriteBuffer(vS[1], Length(vS) * SizeOf(Char));
  // 版本
  vS := HC_FileVersion;
  AStream.WriteBuffer(vS[1], Length(vS) * SizeOf(Char));
end;

/// <summary> 读取文件格式、版本 </summary>
procedure _LoadFileFormatAndVersion(const AStream: TStream; var AFileFormat, AVersion: string);
begin
  // 文件格式
  SetLength(AFileFormat, Length(HC_EXT));
  AStream.ReadBuffer(AFileFormat[1], Length(HC_EXT) * SizeOf(Char));

  // 版本
  SetLength(AVersion, Length(HC_FileVersion));
  AStream.ReadBuffer(AVersion[1], Length(HC_FileVersion) * SizeOf(Char));
end;

function GetCharOffsetByX(const ACanvas: TCanvas; const AText: string; const X: Integer): Integer;
var
  i, vX, vCharWidth: Integer;
begin
  Result := -1;

  if X < 0 then
    Result := 0
  else
  if X > ACanvas.TextWidth(AText) then
    Result := Length(AText)
  else
  begin
    vX := 0;
    for i := 1 to Length(AText) do  { TODO : 有空改为二分法更高效 }
    begin
      vCharWidth := ACanvas.TextWidth(AText[i]);
      vX := vX + vCharWidth;
      if vX > X then  // 当前字符结束位置在X后
      begin
        if vX - vCharWidth div 2 > X then  // 点击在前半部分
          Result := i - 1  // 计为前一个后面
        else
          Result := i;
        Break;
      end;
    end;
  end;
end;

{ TCaret }

constructor TCaret.Create(const AHandle: THandle);
begin
  FOwnHandle := AHandle;
  CreateCaret(FOwnHandle, 0, 2, 20);
end;

destructor TCaret.Destroy;
begin
  DestroyCaret;
  FOwnHandle := 0;
  inherited;
end;

procedure TCaret.Hide;
begin
  HideCaret(FOwnHandle);
end;

procedure TCaret.ReCreate;
begin
  DestroyCaret;
  CreateCaret(FOwnHandle, 0, 2, FHeight);
end;

procedure TCaret.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    ReCreate;
  end;
end;

procedure TCaret.Show;
begin
  Show(X, Y);
end;


procedure TCaret.Show(const AX, AY: Integer);
begin
  ReCreate;
  SetCaretPos(AX, AY);
  ShowCaret(FOwnHandle);
end;

initialization
  if HC_FILEFORMAT = 0 then
    HC_FILEFORMAT := RegisterClipboardFormat(HC_EXT);

end.
