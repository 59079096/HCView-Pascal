{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                  HCView代码公共单元                   }
{                                                       }
{*******************************************************}

unit HCCommon;

// 20160618001 由于有的设备紧缩字符，比如空格在藏文中和单独时TextWidth返回的宽度不一样，
// 因此一个字符串里字符单独范围之和或许不等于字符串的范围

interface

{$I HCView.inc}

uses
  Windows, Controls, Classes, Graphics, SysUtils;

const
  HC_TEXTMAXSIZE = 4294967295;
  HC_EXCEPTION = 'HC异常：';
  HCS_EXCEPTION_NULLTEXT = HC_EXCEPTION + '文本Item的内容出现为空的情况！';
  HCS_EXCEPTION_TEXTOVER = HC_EXCEPTION + 'TextItem的内容超出允许的最大字节数4294967295！';
  HCS_EXCEPTION_MEMORYLESS = HC_EXCEPTION + '复制时没有申请到足够的内存！';
  //HCS_EXCEPTION_UNACCEPTDATATYPE = HC_EXCEPTION + '不可接受的数据类型！';
  //HCS_EXCEPTION_STRINGLENGTHLIMIT = HC_EXCEPTION + '此版本不支持连续不换行样式字符串超过65535！';
  HCS_EXCEPTION_VOIDSOURCECELL = HC_EXCEPTION + '源单元格无法再获取源单元格！';
  HCS_EXCEPTION_TIMERRESOURCEOUTOF = HC_EXCEPTION + '安装计时器的资源不足！';

  DMPAPER_HC_16K = -1000;
  HC_EXT_DOCX = '.docx';

  HC_EXT = '.hcf';
  HC_PROGRAMLANGUAGE = 1;  // 1字节表示使用的编程语言 1:delphi, 2:C#, 3:VC++, 4:HTML5

  {1.3 支持浮动对象保存和读取(未处理向下兼容)
   1.4 支持表格单元格边框显示属性的保存和读取
   1.5 重构行间距的计算方式
   1.6 EditItem增加边框属性
   1.7 增加了重构后的行间距的存储
   1.8 增加了段样式垂直对齐样式的存储
   1.9 重构了颜色的存储方式以便于兼容其他语言生成的文件
   2.0 ImageItem存图像时增加图像数据大小的存储以兼容不同语言图像数据的存储方式
       文件保存时增加由哪种编程语言生成的标识
   2.1 GifImage保存读取改用兼容其他语言的方式
   2.2 增加段缩进的存储
   2.3 增加批注的保存和读取
   2.4 兼容EmrView保存保护元素属性
   2.5 使用unicode字符集保存文档以便支持藏文等
   2.6 文件保存时直接使用TItemOptions集合变量的值，不再单独判断成员存储
   2.7 浮动直线改为ShapeLine
   2.8 浮动Item都使用HCStyle的样式定义(负数)，这样便于统一按Item处理遍历等操作
   2.9 浮动Item保存PageIndex，原因见 20190906001
   3.0 表格增加边框宽度的存储
   3.1 增加行间距 最小值、固定值、多倍的存储
   3.2 表格边框改用磅为单位、段样式增加BreakRough处理截断、兼容EmrView使用TDeImageItem类处理ImageItem
   3.3 兼容32版本图片保存时没有按DeImageItem保存，读取时不正确的问题
   3.4 RadioGroun控件保存选项样式、保存文件所用的排版算法版本、增加Item打印不可见属性，EditItem增加仅打印文本属性
   3.5 数据元增加DeleteProtect控制是否能删除掉整个数据元，表格存储CellPadding，FloatBarCode存储单线条宽度
   3.6 Combobox和RadioGrou的选项改为键值对的形式
   3.7 兼容Combobox无下拉选项时保存选项后打不开的问题
   3.8 浮动Item增加Lock属性用于锁定Item不可移动和修改
   3.9 域Item保存时存Level
  }

  HC_FileVersion = '3.9';
  HC_FileVersionInt = 39;

  TabCharWidth = 28;  // 默认Tab宽度(五号) 14 * 2个
  DefaultColWidth = 50;
  PMSLineHeight = 24;  // 书写范围线的长度
  AnnotationWidth = 200;  // 批注显示区域宽度
  AnnotateBKColor = $00D5D5FF;
  AnnotateBKActiveColor = $00A8A8FF;
  HyperTextColor = $00C16305;
  HCTransparentColor = clNone;  // 透明色
  {$IFDEF UNPLACEHOLDERCHAR}
  /// <summary> 不占位字符或紧缩字符 </summary>
  UnPlaceholderChar =
    #3962{e} + #3956{u} + #3954{i} + #3964{o}  // 藏文元音字母
    // 藏文其他紧缩字符
    + #4024 + #3966 + #3971 + #3895 + #3893 + #3967 + #4023 + #4026 + #3989
    + #3990 + #3963 + #4018 + #3999 + #4017 + #4013 + #3968 + #3965 + #4005
    + #4009 + #4010 + #4011 + #4016 + #4022 + #4001 + #4006 + #3988 + #4008
    + #3972 + #3986 + #3986 + #4014 + #4015 + #4020 + #3984 + #3985 + #4004
    + #4003 + #4000 + #3991 + #3993 + #4028 + #4027 + #3865 + #3953 + #3902
    + #3903 + #3975 + #3974 + #3958 + #3959 + #3960 + #3961 + #3955 + #3994
    + #3957 + #3955 + #3996 + #4038 + #4021 + #4025 + #3970 + #3998 + #3995;
  {$ENDIF}
  /// <summary> 不能在行首的字符 </summary>
  DontLineFirstChar = '`-=[]\;,./~!@#$%^&*()_+{}|:"<>?·－＝【】＼；’，。、～！＠＃￥％……＆×（）——＋｛｝｜：”《》？°'
  {$IFDEF UNPLACEHOLDERCHAR}
    + UnPlaceholderChar  // 紧缩字符
  {$ENDIF}
    ;
  /// <summary> 不能在行尾的字符 </summary>
  DontLineLastChar = '/\＼“‘';
  /// <summary> 可以挤压宽度的字符 </summary>
  LineSqueezeChar = '，。；、？“”';

  HCBoolText: array [Boolean] of Char = ('0', '1');

type
  THCProcedure = reference to procedure();
  THCFunction = reference to function(): Boolean;

  TPaperOrientation = (cpoPortrait, cpoLandscape);  // 纸张方向：纵像、横向

  TExpressArea = (ceaNone, ceaLeft, ceaTop, ceaRight, ceaBottom);  // 公式的区域，仅适用于上下左右格式的

  TBorderSide = (cbsLeft, cbsTop, cbsRight, cbsBottom, cbsLTRB, cbsRTLB);
  TBorderSides = set of TBorderSide;

  TSectionArea = (saHeader, saPage, saFooter);  // 当前激活的是文档哪一部分
  TSectionAreas = set of TSectionArea;  // 保存时存哪几部分内容

  // 表格单元格对齐方式
  THCContentAlign = (tcaTopLeft, tcaTopCenter, tcaTopRight, tcaCenterLeft,
    tcaCenterCenter, tcaCenterRight, tcaBottomLeft, tcaBottomCenter, tcaBottomRight);

  THCState = (hosLoading,  // 文档加载
              hosCopying,  // 复制
              hosPasting,  // 粘贴
              hosUndoing,
              hosRedoing,
              hosBatchInsert,  // 调用InsertItem批量插入多个Item时(如数据组批量插入2个)防止别的操作引起位置变化导致后面插入位置不正确
              hosDestroying  // 编辑器在销毁中
              );

  TCharType = (
    jctBreak,  //  截断点
    jctHZ,  // 汉字
    jctZM,  // 半角字母
    //jctCNZM,  // 全角字母
    jctSZ,  // 半角数字
    //jctCNSZ,  // 全角数字
    jctFH  // 半角符号
    //jctCNFH   // 全角符号
    );

  THCAction = (
    actBackDeleteText,  // 向前删除文本
    actDeleteText,  // 向后删除文本
    actReturnItem,  // 在Item上回车
    actInsertText,  // 插入文本
    actSetItemText,    // 直接赋值Item的Text
    actDeleteItem,  // 删除Item
    actInsertItem,  // 插入Item
    actItemProperty,  // Item属性变化
    actItemSelf,  // Item自己管理
    actItemMirror,  // Item镜像
    actConcatText  // 粘接文本(两头)
    );

//  TPaperSize = (psCustom, ps4A0, ps2A0, psA0, psA1, psA2,
//    psA3, psA4, psA5, psA6, psA7, psA8,
//    psA9, psA10, psB0, psB1, psB2, psB3,
//    psB4, psB5, psB6, psB7, psB8, psB9,
//    psB10, psC0, psC1, psC2, psC3, psC4,
//    psC5, psC6, psC7, psC8, psC9, psC10,
//    psLetter, psLegal, psLedger, psTabloid,
//    psStatement, psQuarto, psFoolscap, psFolio,
//    psExecutive, psMonarch, psGovernmentLetter,
//    psPost, psCrown, psLargePost, psDemy,
//    psMedium, psRoyal, psElephant, psDoubleDemy,
//    psQuadDemy, psIndexCard3_5, psIndexCard4_6,
//    psIndexCard5_8, psInternationalBusinessCard,
//    psUSBusinessCard, psEmperor, psAntiquarian,
//    psGrandEagle, psDoubleElephant, psAtlas,
//    psColombier, psImperial, psDoubleLargePost,
//    psPrincess, psCartridge, psSheet, psHalfPost,
//    psDoublePost, psSuperRoyal, psCopyDraught,
//    psPinchedPost, psSmallFoolscap, psBrief, psPott,
//    psPA0, psPA1, psPA2, psPA3, psPA4, psPA5,
//    psPA6, psPA7, psPA8, psPA9, psPA10, psF4,
//    psA0a, psJISB0, psJISB1, psJISB2, psJISB3,
//    psJISB4, psJISB5, psJISB6, psJISB7, psJISB8,
//    psJISB9, psJISB10, psJISB11, psJISB12,
//    psANSI_A, psANSI_B, psANSI_C, psANSI_D,
//    psANSI_E, psArch_A, psArch_B, psArch_C,
//    psArch_D, psArch_E, psArch_E1,
//    ps16K, ps32K);

  THCFont = class(TFont)
  public
    procedure FromCanvas(const ACanvas: TCanvas);
    procedure ToCanvas(const ACanvas: TCanvas);
  end;

  THCPen = class(TPen)
  public
    procedure FromCanvas(const ACanvas: TCanvas);
    procedure ToCanvas(const ACanvas: TCanvas);
  end;

  THCBrush = class(TBrush)
  public
    procedure FromCanvas(const ACanvas: TCanvas);
    procedure ToCanvas(const ACanvas: TCanvas);
  end;

  THCCanvas = class
  private
    FFont: THCFont;
    FPen: THCPen;
    FBrush: THCBrush;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FromCanvas(const ACanvas: TCanvas);
    procedure ToCanvas(const ACanvas: TCanvas);
  end;

  THCCaretInfo = record
    X, Y, Height, PageIndex: Integer;
    Visible: Boolean;
  end;

  {$IFNDEF DELPHIXE}
  THCPoint = record helper for TPoint
  public
    procedure Offset(const DX, DY : Integer); overload;
    procedure Offset(const Point: TPoint); overload;
  end;

  THCRect = record helper for TRect
  protected
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    function GetWidth: Integer;
    procedure SetWidth(const Value: Integer);
    function GetLocation: TPoint;
    procedure SetLocation(const Point: TPoint);
  public
    procedure Offset(const DX, DY: Integer); overload;
    procedure Offset(const Point: TPoint); overload;
    procedure Inflate(const DX, DY: Integer);
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
    property Location: TPoint read GetLocation write SetLocation;
  end;
  {$ENDIF}

  TMarkType = (cmtBeg, cmtEnd);

  THCCaret = Class(TObject)
  private
    FReCreate, FDisFocus, FVScroll, FHScroll: Boolean;
    FHeight: Integer;
    FOwnHandle: THandle;
    FX, FY: Integer;
    FWidth: Byte;
  protected
    procedure SetX(const Value: Integer);
    procedure SetY(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Byte);
  public
    constructor Create(const AHandle: THandle);
    destructor Destroy; override;
    procedure ReCreate;
    procedure Show(const AX, AY: Integer); overload;
    procedure Show; overload;
    procedure Hide(const ADisFocus: Boolean = False);
    property Height: Integer read FHeight write SetHeight;
    property Width: Byte read FWidth write SetWidth;
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
    property DisFocus: Boolean read FDisFocus;
    property VScroll: Boolean read FVScroll write FVScroll;
    property HScroll: Boolean read FHScroll write FHScroll;
  end;

  function SwapBytes(AValue: Word): Word;
  function IsKeyPressWant(const AKey: Char): Boolean;
  function IsKeyDownWant(const AKey: Word): Boolean;
  function IsKeyDownEdit(const AKey: Word): Boolean;  // 引起内容变化的KeyDown
  function IsDirectionKey(const AKey: Word): Boolean;

  function CreateExtPen(const APen: TPen): HPEN;

  /// <summary> 效率更高的返回字符在字符串位置函数 </summary>
  function PosCharHC(const AChar: Char; const AStr: string{; const Offset: Integer = 1}): Integer;

  /// <summary> 返回字符类型 </summary>
  function GetUnicodeCharType(const AChar: Char): TCharType;

  {$IFDEF UNPLACEHOLDERCHAR}
  /// <summary> 返回字符串指定位置的实际有效字前、后位置 </summary>
  /// <param name="AText"></param>
  /// <param name="AIndex"></param>
  /// <param name="AAfter = False">True:前，False:后</param>
  /// <returns></returns>
  function GetTextActualOffset(const AText: string; const AOffset: Integer; const AAfter: Boolean = False): Integer;

  function IsUnPlaceHolderChar(const AChar: Char): Boolean;
  {$ENDIF}  // UNPLACEHOLDERCHAR

  /// <summary> 返回字符串位置数组中指定的字符中间位置 </summary>
  /// <param name="AIndex">指定第几个字符</param>
  /// <param name="ACharWArr">字符串位置数组</param>
  /// <returns>中间位置</returns>
  function GetCharHalfFarfrom(
    {$IFDEF UNPLACEHOLDERCHAR}
    const AText: string;
    {$ENDIF}  // UNPLACEHOLDERCHAR
    const AOffset: Integer;
    const ACharWArr: array of Integer): Integer;

  /// <summary> 返回普通对齐方式(左中右)指定位置在字符串哪个字符后面(0：第一个前面) </summary>
  /// <param name="ACanvas"></param>
  /// <param name="AText"></param>
  /// <param name="X"></param>
  /// <returns></returns>
  function GetNorAlignCharOffsetAt(const ACanvas: TCanvas; const AText: string; const X: Integer): Integer;

  // 根据汉字大小获取字体数字大小
  function GetFontSize(const AFontSize: string): Single;
  function GetFontSizeStr(AFontSize: Single): string;
  function GetPaperSizeStr(APaperSize: Integer): string;

  function GetVersionAsInteger(const AVersion: string): Integer;
  function GetBorderSidePro(const ABorderSides: TBorderSides): string;
  procedure SetBorderSideByPro(const AValue: string; var ABorderSides: TBorderSides);

  function HCDeleteBreak(const S: string): string;

  /// <summary> 保存长度小于65536个字节的字符串到流 </summary>
  procedure HCSaveTextToStream(const AStream: TStream; const S: string);
  procedure HCLoadTextFromStream(const AStream: TStream; var S: string; const AFileVersion: Word);

  procedure HCSaveColorToStream(const AStream: TStream; const AColor: TColor);
  procedure HCLoadColorFromStream(const AStream: TStream; var AColor: TColor);

  procedure BitmapSaveAsJPGE(const ABitmap: TBitmap; const AFile: string);
  procedure BitmapSaveAsPNG(const ABitmap: TBitmap; const AFile: string);

  /// <summary> 保存文件格式、版本 </summary>
  procedure _SaveFileFormatAndVersion(const AStream: TStream);
  /// <summary> 读取文件格式、版本 </summary>
  procedure _LoadFileFormatAndVersion(const AStream: TStream;
    var AFileFormat: string; var AVersion: Word; var ALang: Byte);

  {$IFDEF DEBUG}
  procedure DrawDebugInfo(const ACanvas: TCanvas; const ALeft, ATop: Integer; const AInfo: string);
  {$ENDIF}

  procedure HCDrawArrow(const ACanvas: TCanvas; const AColor: TColor; const ALeft, ATop: Integer; const AType: Byte);

  function SaveCanvas(const ACanvas: TCanvas): THCCanvas;

var
  GCursor: TCursor;
  HC_FILEFORMAT, CF_HTML, CF_RTF: Word;

implementation

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

procedure HCDrawArrow(const ACanvas: TCanvas; const AColor: TColor; const ALeft, ATop: Integer; const AType: Byte);
begin
  case AType of
    0: // 上
      begin
        ACanvas.Pen.Color := AColor;
        ACanvas.MoveTo(ALeft, ATop);
        ACanvas.LineTo(ALeft - 1, ATop);
        ACanvas.MoveTo(ALeft - 1, ATop + 1);
        ACanvas.LineTo(ALeft + 2, ATop + 1);
        ACanvas.MoveTo(ALeft - 2, ATop + 2);
        ACanvas.LineTo(ALeft + 3, ATop + 2);
        ACanvas.MoveTo(ALeft - 3, ATop + 3);
        ACanvas.LineTo(ALeft + 4, ATop + 3);
        ACanvas.MoveTo(ALeft - 4, ATop + 4);
        ACanvas.LineTo(ALeft + 5, ATop + 4);
      end;

    1: // 下
      begin
        ACanvas.Pen.Color := AColor;
        ACanvas.MoveTo(ALeft, ATop);
        ACanvas.LineTo(ALeft - 1, ATop);
        ACanvas.MoveTo(ALeft - 1, ATop - 1);
        ACanvas.LineTo(ALeft + 2, ATop - 1);
        ACanvas.MoveTo(ALeft - 2, ATop - 2);
        ACanvas.LineTo(ALeft + 3, ATop - 2);
        ACanvas.MoveTo(ALeft - 3, ATop - 3);
        ACanvas.LineTo(ALeft + 4, ATop - 3);
        ACanvas.MoveTo(ALeft - 4, ATop - 4);
        ACanvas.LineTo(ALeft + 5, ATop - 4);
      end;

    2: // 左
      begin

      end;

    3:  // 右
      begin

      end;
  end;
end;

function CreateExtPen(const APen: TPen): HPEN;
var
  vPenParams: TLogBrush;
const
  PenTypes: array[Boolean] of Integer = (PS_COSMETIC, PS_GEOMETRIC);
  PenStyles: array[psSolid..psInsideFrame] of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL, PS_SOLID);
begin
  vPenParams.lbStyle := PenStyles[APen.Style];
  vPenParams.lbColor := APen.Color;
  vPenParams.lbHatch := 0;
  Result := ExtCreatePen({PenTypes[APen.Width <> 1]}PS_GEOMETRIC or PS_ENDCAP_SQUARE,
    APen.Width, vPenParams, 0, nil);
end;

function SaveCanvas(const ACanvas: TCanvas): THCCanvas;
begin
  Result := THCCanvas.Create;
  Result.FromCanvas(ACanvas);
end;

function SwapBytes(AValue: Word): Word;
begin
  Result := (AValue shr 8) or Word(AValue shl 8);
end;

procedure HCSaveTextToStream(const AStream: TStream; const S: string);
var
  vBuffer: TBytes;
  vSize: Word;
begin
  {$IFDEF UNPLACEHOLDERCHAR}
  vBuffer := TEncoding.Unicode.GetBytes(S);
  {$ELSE}
  vBuffer := BytesOf(S);
  {$ENDIF}
  if System.Length(vBuffer) > MAXWORD then
    raise Exception.Create(HCS_EXCEPTION_TEXTOVER);

  vSize := System.Length(vBuffer);
  AStream.WriteBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
    AStream.WriteBuffer(vBuffer[0], vSize);
end;

procedure HCLoadTextFromStream(const AStream: TStream; var S: string;
  const AFileVersion: Word);
var
  vSize: Word;
  vBuffer: TBytes;
begin
  AStream.ReadBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    SetLength(vBuffer, vSize);
    AStream.Read(vBuffer[0], vSize);
    {$IFDEF UNPLACEHOLDERCHAR}
    if AFileVersion > 24 then
      S := TEncoding.Unicode.GetString(vBuffer)
    else
      S := StringOf(vBuffer);
    {$ELSE}
      S := StringOf(vBuffer);
    {$ENDIF}
  end
  else
    S := '';
end;

function HCDeleteBreak(const S: string): string;
begin
  Result := StringReplace(S, sLineBreak, '', [rfReplaceAll]);
end;

function GetUnicodeCharType(const AChar: Char): TCharType;
begin
  case Cardinal(AChar) of
    $2E80..$2EF3,  // 部首扩展 115
    $2F00..$2FD5,  // 康熙部首 214
    $2FF0..$2FFB,  // 汉字结构 12
    $3007,  // 〇 1
    $3105..$312F,  // 汉字注音 43
    $31A0..$31BA,  // 注音扩展 22
    $31C0..$31E3,  // 汉字笔划 36
    $3400..$4DB5,  // 扩展A 6582个
    $4E00..$9FA5,  // 基本汉字 20902个
    $9FA6..$9FEF,  // 基本汉字补充 74个
    $E400..$E5E8,  // 部件扩展 452
    $E600..$E6CF,  // PUA增补 207
    $E815..$E86F,  // PUA(GBK)部件 81
    $F900..$FAD9,  // 兼容汉字 477
    $20000..$2A6D6, // 扩展B 42711个
    $2A700..$2B734,  // 扩展C 4149
    $2B740..$2B81D,  // 扩展D 222
    $2B820..$2CEA1,  // 扩展E 5762
    $2CEB0..$2EBE0,  // 扩展F 7473
    $2F800..$2FA1D  // 兼容扩展 542
      : Result := jctHZ;  // 汉字

    $0F00..$0FFF:  // 藏文
      Result := jctHZ;

    $1800..$18AF: Result := jctHZ;  // 蒙古字符

    $21..$2F,  // !"#$%&'()*+,-./
    $3A..$40,  // :;<=>?@
    $5B..$60,  // [\]^_`
    $7B..$7E,   // {|}~
    $FFE0  // ￠
      : Result := jctFH;

    //$FF01..$FF0F,  // ！“＃￥％＆‘（）×＋，－。、

    $30..$39: Result := jctSZ;  // 0..9

    $41..$5A, $61..$7A: Result := jctZM;  // A..Z, a..z
  else
    Result := jctBreak;
  end;
end;

function IsKeyPressWant(const AKey: Char): Boolean;
begin
  {$IFDEF UNPLACEHOLDERCHAR}
  case AKey of
    #32..#126,  // <#32是ASCII控制字 #127是ASCII DEL
    #3840..#4095,  // 藏文
    #6144..#6319:  // 蒙古文
      Result := True;
  else
    Result := False;
  end;
  {$ELSE}
  Result := AKey in [#32..#126];  // <#32是ASCII控制字 #127是ASCII DEL
  {$ENDIF}
end;

function IsKeyDownWant(const AKey: Word): Boolean;
begin
  Result := AKey in [VK_BACK, VK_DELETE, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_RETURN,
    VK_HOME, VK_END, VK_TAB];
end;

function IsKeyDownEdit(const AKey: Word): Boolean;
begin
  Result := AKey in [VK_BACK, VK_DELETE, VK_RETURN, VK_TAB];
end;

function IsDirectionKey(const AKey: Word): Boolean;
begin
  Result := AKey in [VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN];
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

function GetFontSize(const AFontSize: string): Single;
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
  if AFontSize = '五号' then Result := 10.5
  else
  if AFontSize = '小五' then Result := 9
  else
  if AFontSize = '六号' then Result := 7.5
  else
  if AFontSize = '小六' then Result := 6.5
  else
  if AFontSize = '七号' then Result := 5.5
  else
  if AFontSize = '八号' then Result := 5
  else
  if not TryStrToFloat(AFontSize, Result) then
    raise Exception.Create(HC_EXCEPTION + '计算字号大小出错，无法识别的值：' + AFontSize);
end;

function GetFontSizeStr(AFontSize: Single): string;
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
  if AFontSize = 10.5 then Result := '五号'
  else
  if AFontSize = 9 then Result := '小五'
  else
  if AFontSize = 7.5 then Result := '六号'
  else
  if AFontSize = 6.5 then Result := '小六'
  else
  if AFontSize = 5.5 then Result := '七号'
  else
  if AFontSize = 5 then Result := '八号'
  else
    Result := FormatFloat('0.#', AFontSize);
end;

function GetPaperSizeStr(APaperSize: Integer): string;
begin
  case APaperSize of
    DMPAPER_A3: Result := 'A3';
    DMPAPER_A4: Result := 'A4';
    DMPAPER_A5: Result := 'A5';
    DMPAPER_B5: Result := 'B5';
    DMPAPER_HC_16K: Result := '16K';
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
  Result := StrToInt(vsVer);
end;

function GetBorderSidePro(const ABorderSides: TBorderSides): string;
begin
  if cbsLeft in ABorderSides then
    Result := 'left';

  if cbsTop in ABorderSides then
  begin
    if Result <> '' then
      Result := Result + ',top'
    else
      Result := 'top';
  end;

  if cbsRight in ABorderSides then
  begin
    if Result <> '' then
      Result := Result + ',right'
    else
      Result := 'right';
  end;

  if cbsBottom in ABorderSides then
  begin
    if Result <> '' then
      Result := Result + ',bottom'
    else
      Result := 'bottom';
  end;

  if cbsLTRB in ABorderSides then
  begin
    if Result <> '' then
      Result := Result + ',ltrb'
    else
      Result := 'ltrb';
  end;

  if cbsRTLB in ABorderSides then
  begin
    if Result <> '' then
      Result := Result + ',rtlb'
    else
      Result := 'rtlb';
  end;
end;

procedure SetBorderSideByPro(const AValue: string; var ABorderSides: TBorderSides);
var
  vList: TStringList;
  i: Integer;
begin
  ABorderSides := [];
  vList := TStringList.Create;
  try
    vList.Delimiter := ',';
    vList.DelimitedText := AValue;
    for i := 0 to vList.Count - 1 do
    begin
      if vList[i] = 'left' then
        Include(ABorderSides, cbsLeft)
      else
      if vList[i] = 'top' then
        Include(ABorderSides, cbsTop)
      else
      if vList[i] = 'right' then
        Include(ABorderSides, cbsRight)
      else
      if vList[i] = 'bottom' then
        Include(ABorderSides, cbsBottom)
      else
      if vList[i] = 'ltrb' then
        Include(ABorderSides, cbsLTRB)
      else
      if vList[i] = 'rtlb' then
        Include(ABorderSides, cbsRTLB)
    end;
  finally
    FreeAndNil(vList);
  end;
end;

/// <summary> 保存文件格式、版本 </summary>
procedure _SaveFileFormatAndVersion(const AStream: TStream);
var
  vS: string;
  vLang: Byte;
begin
  vS := HC_EXT;
  AStream.WriteBuffer(vS[1], Length(vS) * SizeOf(Char));
  // 版本
  vS := HC_FileVersion;
  AStream.WriteBuffer(vS[1], Length(vS) * SizeOf(Char));
  // 使用的编程语言
  vLang := HC_PROGRAMLANGUAGE;
  AStream.WriteBuffer(vLang, 1);
end;

/// <summary> 读取文件格式、版本 </summary>
procedure _LoadFileFormatAndVersion(const AStream: TStream;
  var AFileFormat: string; var AVersion: Word; var ALang: Byte);
var
  vFileVersion: string;
begin
  // 文件格式
  SetLength(AFileFormat, Length(HC_EXT));
  AStream.ReadBuffer(AFileFormat[1], Length(HC_EXT) * SizeOf(Char));

  // 版本
  SetLength(vFileVersion, Length(HC_FileVersion));
  AStream.ReadBuffer(vFileVersion[1], Length(HC_FileVersion) * SizeOf(Char));
  AVersion := GetVersionAsInteger(vFileVersion);

  if AVersion > 19 then // 使用的编程语言
    AStream.ReadBuffer(ALang, 1);
end;

procedure HCSaveColorToStream(const AStream: TStream; const AColor: TColor);
var
  vByte: Byte;
begin
  if AColor = HCTransparentColor then  // 透明
  begin
    vByte := 0;
    AStream.WriteBuffer(vByte, 1);
    vByte := 255;
    AStream.WriteBuffer(vByte, 1);
    AStream.WriteBuffer(vByte, 1);
    AStream.WriteBuffer(vByte, 1);
  end
  else
  begin
    vByte := 255;
    AStream.WriteBuffer(vByte, 1);

    vByte := GetRValue(AColor);
    AStream.WriteBuffer(vByte, 1);

    vByte := GetGValue(AColor);
    AStream.WriteBuffer(vByte, 1);

    vByte := GetBValue(AColor);
    AStream.WriteBuffer(vByte, 1);
  end;
end;

procedure HCLoadColorFromStream(const AStream: TStream; var AColor: TColor);
var
  vA, vR, vG, vB: Byte;
begin
  AStream.ReadBuffer(vA, 1);
  AStream.ReadBuffer(vR, 1);
  AStream.ReadBuffer(vG, 1);
  AStream.ReadBuffer(vB, 1);

  if vA = 0 then
    AColor := HCTransparentColor
  else
  if vA = 255 then
    AColor := vB shl 16 + vG shl 8 + vR;
end;

procedure BitmapSaveAsJPGE(const ABitmap: TBitmap; const AFile: string);
var
  vSM: TMemoryStream;
  vImage: TWICImage;
begin
  vSM := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(vSM);
    vSM.Position := 0;
    vImage := TWICImage.Create;
    try
      vImage.LoadFromStream(vSM);
      vImage.ImageFormat := TWICImageFormat.wifJpeg;
      vImage.SaveToFile(AFile);
    finally
      FreeAndNil(vImage);
    end;
  finally
    FreeAndNil(vSM);
  end;
end;

procedure BitmapSaveAsPNG(const ABitmap: TBitmap; const AFile: string);
var
  vSM: TMemoryStream;
  vImage: TWICImage;
begin
  vSM := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(vSM);
    vSM.Position := 0;
    vImage := TWICImage.Create;
    try
      vImage.LoadFromStream(vSM);
      vImage.ImageFormat := TWICImageFormat.wifPng;
      vImage.SaveToFile(AFile);
    finally
      FreeAndNil(vImage);
    end;
  finally
    FreeAndNil(vSM);
  end;
end;

{$IFDEF UNPLACEHOLDERCHAR}
function IsUnPlaceHolderChar(const AChar: Char): Boolean;
begin
  Result := Pos(AChar, UnPlaceholderChar) > 0;
end;

function GetTextActualOffset(const AText: string; const AOffset: Integer;
  const AAfter: Boolean = False): Integer;
var
  vLen: Integer;
begin
  Result := AOffset;  // 返回哪一个后面

  vLen := Length(AText);
  if AAfter then  // 后面
  begin
    while Result < vLen do
    begin
      if Pos(AText[Result + 1], UnPlaceholderChar) > 0 then
        Inc(Result)
      else
        Break;
    end;
  end
  else  // 前面
  begin
    while Result > 1 do
    begin
      if Pos(AText[Result], UnPlaceholderChar) > 0 then
        Dec(Result)
      else
        Break;
    end;
  end;
end;

function GetCharHalfFarfrom(const AText: string; const AOffset: Integer;
  const ACharWArr: array of Integer): Integer;
var
  vBeginOffs, vEndOffs: Integer;
begin
  vEndOffs := GetTextActualOffset(AText, AOffset, True);
  vBeginOffs := GetTextActualOffset(AText, AOffset) - 1;

  if vBeginOffs > 0 then
  begin
    if vEndOffs = vBeginOffs then
    begin
      if vBeginOffs > 1 then
        Result := ACharWArr[vBeginOffs - 2] + ((ACharWArr[vEndOffs - 1] - ACharWArr[vBeginOffs - 2]) div 2)
      else
        Result := ACharWArr[vBeginOffs - 1] div 2;
    end
    else
      Result := ACharWArr[vBeginOffs - 1] + ((ACharWArr[vEndOffs - 1] - ACharWArr[vBeginOffs - 1]) div 2);
  end
  else  // = 0
    Result := ACharWArr[vEndOffs - 1] div 2;
end;
{$ELSE}
function GetCharHalfFarfrom(const AOffset: Integer;
  const ACharWArr: array of Integer): Integer;
begin
  if AOffset > 1 then
    Result := ACharWArr[AOffset - 2] + ((ACharWArr[AOffset - 1] - ACharWArr[AOffset - 2]) div 2)
  else
  if AOffset = 1 then
    Result := ACharWArr[AOffset - 1] div 2
  else
    Result := 0;
end;
{$ENDIF}  // UNPLACEHOLDERCHAR

function GetNorAlignCharOffsetAt(const ACanvas: TCanvas; const AText: string; const X: Integer): Integer;
var
  vCharWArr: array of Integer;  // 每个字符绘制结束位置
  i, vLen: Integer;
  vSize: TSize;
begin
  Result := -1;

  if X < 0 then
    Result := 0
  else
  begin
    vLen := Length(AText);
    // 由于有的设备紧缩字符，因此一个字符串里字符的范围之和或许不等于字符串的范围
    SetLength(vCharWArr, vLen);
    GetTextExtentExPoint(ACanvas.Handle, PChar(AText), vLen, 0,
      nil, PInteger(vCharWArr), vSize);  // 超过65535数组元素取不到值
    // 20190618002 需要同步修改的字符和位置相关的计算
    if X > vSize.cx then
      Result := vLen
    else
    begin
      i := 1;
      while i <= vLen do
      begin
        {$IFDEF UNPLACEHOLDERCHAR}
        i := GetTextActualOffset(AText, i, True);
        {$ENDIF}

        if X = vCharWArr[i - 1] then
        begin
          Result := i;
          Break;
        end
        else
        if X > vCharWArr[i - 1] then
          Inc(i)
        else  // X < vCharWArr[i - 1]
        begin
          if X > GetCharHalfFarfrom({$IFDEF UNPLACEHOLDERCHAR}AText,{$ENDIF} i, vCharWArr) then  // 在结束的后半部分
            Result := i
          else  // 在起始的前半部分，返回前一个字符段最后
          begin
            {$IFDEF UNPLACEHOLDERCHAR}
            Result := GetTextActualOffset(AText, i) - 1;
            {$ELSE}
            Result := i - 1;
            {$ENDIF}
          end;

          Break;
        end;
      end;
    end;

    SetLength(vCharWArr, 0);
  end;
end;

{ THCCaret }

constructor THCCaret.Create(const AHandle: THandle);
begin
  FOwnHandle := AHandle;
  FWidth := 2;
  CreateCaret(FOwnHandle, 0, FWidth, 20);
  FReCreate := False;
  FDisFocus := False;
  FVScroll := False;
  FHScroll := False;
end;

destructor THCCaret.Destroy;
begin
  DestroyCaret;
  FOwnHandle := 0;
  inherited Destroy;
end;

procedure THCCaret.Hide(const ADisFocus: Boolean = False);
begin
  FDisFocus := ADisFocus;
  HideCaret(FOwnHandle);
end;

procedure THCCaret.ReCreate;
begin
  DestroyCaret;
  CreateCaret(FOwnHandle, 0, FWidth, FHeight);
end;

procedure THCCaret.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    FReCreate := True;
  end;
end;

procedure THCCaret.SetWidth(const Value: Byte);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    FReCreate := True;
  end;
end;

procedure THCCaret.SetX(const Value: Integer);
begin
  if FX <> Value then
  begin
    FX := Value;
    //FReCreate := True;
    Show;
  end;
end;

procedure THCCaret.SetY(const Value: Integer);
begin
  if FY <> Value then
  begin
    FY := Value;
    Show;
    //FReCreate := True;
  end;
end;

procedure THCCaret.Show;
begin
  Self.Show(FX, FY);
end;


procedure THCCaret.Show(const AX, AY: Integer);
begin
  FDisFocus := False;

  if FReCreate then
    ReCreate;

  SetCaretPos(AX, AY);
  ShowCaret(FOwnHandle);
end;

{$IFNDEF DELPHIXE}
{ THCRect }

function THCRect.GetHeight: Integer;
begin
  Result := Self.Bottom - Self.Top;
end;

function THCRect.GetLocation: TPoint;
begin
  Result := TopLeft;
end;

function THCRect.GetWidth: Integer;
begin
  Result := Self.Right - Self.Left;
end;

procedure THCRect.Inflate(const DX, DY: Integer);
begin
  TopLeft.Offset(-DX, -DY);
  BottomRight.Offset(DX, DY);
end;

procedure THCRect.Offset(const Point: TPoint);
begin
  TopLeft.Offset(Point);
  BottomRight.Offset(Point);
end;

procedure THCRect.Offset(const DX, DY: Integer);
begin
  TopLeft.Offset(DX, DY);
  BottomRight.Offset(DX, DY);
end;

procedure THCRect.SetHeight(const Value: Integer);
begin
  Self.Bottom := Self.Top + Value;
end;

procedure THCRect.SetLocation(const Point: TPoint);
begin
  Offset(Point.X - Left, Point.Y - Top);
end;

procedure THCRect.SetWidth(const Value: Integer);
begin
  Self.Right := Self.Left + Value;
end;

{ THCPoint }

procedure THCPoint.Offset(const DX, DY: Integer);
begin
  Inc(Self.X, DX);
  Inc(Self.Y, DY);
end;

procedure THCPoint.Offset(const Point: TPoint);
begin
  Self.Offset(Point.X, Point.Y);
end;
{$ENDIF}

{ THCFont }

procedure THCFont.FromCanvas(const ACanvas: TCanvas);
begin
  Self.Assign(ACanvas.Font);
end;

procedure THCFont.ToCanvas(const ACanvas: TCanvas);
begin
  ACanvas.Font.Assign(Self);
end;

{ THCCanvas }

constructor THCCanvas.Create;
begin
  FFont := THCFont.Create;
  FPen := THCPen.Create;
  FBrush := THCBrush.Create;
end;

destructor THCCanvas.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FPen);
  FreeAndNil(FBrush);
  inherited Destroy;
end;

procedure THCCanvas.FromCanvas(const ACanvas: TCanvas);
begin
  FFont.FromCanvas(ACanvas);
end;

procedure THCCanvas.ToCanvas(const ACanvas: TCanvas);
begin
  FFont.ToCanvas(ACanvas);
end;

{ THCPen }

procedure THCPen.FromCanvas(const ACanvas: TCanvas);
begin
  Self.Assign(ACanvas.Pen);
end;

procedure THCPen.ToCanvas(const ACanvas: TCanvas);
begin
  ACanvas.Pen.Assign(Self);
end;

{ THCBrush }

procedure THCBrush.FromCanvas(const ACanvas: TCanvas);
begin
  Self.Assign(ACanvas.Brush);
end;

procedure THCBrush.ToCanvas(const ACanvas: TCanvas);
begin
  ACanvas.Brush.Assign(Self);
end;

initialization
  if HC_FILEFORMAT = 0 then
    HC_FILEFORMAT := RegisterClipboardFormat(HC_EXT);

  if CF_HTML = 0 then
    CF_HTML := RegisterClipboardFormat('HTML Format');

  if CF_RTF = 0 then
    CF_RTF := RegisterClipboardFormat('Rich Text Format');

end.
