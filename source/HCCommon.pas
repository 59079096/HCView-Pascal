{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{                  HCView���빫����Ԫ                   }
{                                                       }
{*******************************************************}

unit HCCommon;

// 20160618001 �����е��豸�����ַ�������ո��ڲ����к͵���ʱTextWidth���صĿ�Ȳ�һ����
// ���һ���ַ������ַ�������Χ֮�ͻ��������ַ����ķ�Χ

interface

{$I HCView.inc}

uses
  Windows, Controls, Classes, Graphics, SysUtils;

const
  HC_TEXTMAXSIZE = 4294967295;
  HC_EXCEPTION = 'HC�쳣��';
  HCS_EXCEPTION_NULLTEXT = HC_EXCEPTION + '�ı�Item�����ݳ���Ϊ�յ������';
  HCS_EXCEPTION_TEXTOVER = HC_EXCEPTION + 'TextItem�����ݳ������������ֽ���4294967295��';
  HCS_EXCEPTION_MEMORYLESS = HC_EXCEPTION + '����ʱû�����뵽�㹻���ڴ棡';
  //HCS_EXCEPTION_UNACCEPTDATATYPE = HC_EXCEPTION + '���ɽ��ܵ��������ͣ�';
  //HCS_EXCEPTION_STRINGLENGTHLIMIT = HC_EXCEPTION + '�˰汾��֧��������������ʽ�ַ�������65535��';
  HCS_EXCEPTION_VOIDSOURCECELL = HC_EXCEPTION + 'Դ��Ԫ���޷��ٻ�ȡԴ��Ԫ��';
  HCS_EXCEPTION_TIMERRESOURCEOUTOF = HC_EXCEPTION + '��װ��ʱ������Դ���㣡';

  DMPAPER_HC_16K = -1000;
  HC_EXT_DOCX = '.docx';

  HC_EXT = '.hcf';
  HC_PROGRAMLANGUAGE = 1;  // 1�ֽڱ�ʾʹ�õı������ 1:delphi, 2:C#, 3:C++, 4:HTML5

  {1.3 ֧�ָ������󱣴�Ͷ�ȡ(δ�������¼���)
   1.4 ֧�ֱ��Ԫ��߿���ʾ���Եı���Ͷ�ȡ
   1.5 �ع��м��ļ��㷽ʽ
   1.6 EditItem���ӱ߿�����
   1.7 �������ع�����м��Ĵ洢
   1.8 �����˶���ʽ��ֱ������ʽ�Ĵ洢
   1.9 �ع�����ɫ�Ĵ洢��ʽ�Ա��ڼ��������������ɵ��ļ�
   2.0 ImageItem��ͼ��ʱ����ͼ�����ݴ�С�Ĵ洢�Լ��ݲ�ͬ����ͼ�����ݵĴ洢��ʽ
       �ļ�����ʱ���������ֱ���������ɵı�ʶ
   2.1 GifImage�����ȡ���ü����������Եķ�ʽ
   2.2 ���Ӷ������Ĵ洢
   2.3 ������ע�ı���Ͷ�ȡ
   2.4 ����EmrView���汣��Ԫ������
   2.5 ʹ��unicode�ַ��������ĵ��Ա�֧�ֲ��ĵ�
   2.6 �ļ�����ʱֱ��ʹ��TItemOptions���ϱ�����ֵ�����ٵ����жϳ�Ա�洢
   2.7 ����ֱ�߸�ΪShapeLine
   2.8 ����Item��ʹ��HCStyle����ʽ����(����)����������ͳһ��Item��������Ȳ���
   2.9 ����Item����PageIndex��ԭ��� 20190906001
   3.0 ������ӱ߿��ȵĴ洢
   3.1 �����м�� ��Сֵ���̶�ֵ���౶�Ĵ洢
   3.2 ���߿���ð�Ϊ��λ������ʽ����BreakRough����ضϡ�����EmrViewʹ��TDeImageItem�ദ��ImageItem
   3.3 ����32�汾ͼƬ����ʱû�а�DeImageItem���棬��ȡʱ����ȷ������
   3.4 RadioGroun�ؼ�����ѡ����ʽ�������ļ����õ��Ű��㷨�汾������Item��ӡ���ɼ����ԣ�EditItem���ӽ���ӡ�ı�����
   3.5 ����Ԫ����DeleteProtect�����Ƿ���ɾ������������Ԫ�����洢CellPadding��FloatBarCode�洢���������
   3.6 Combobox��RadioGroup��ѡ���Ϊ��ֵ�Ե���ʽ
   3.7 ����Combobox������ѡ��ʱ����ѡ���򲻿�������
   3.8 ����Item����Lock������������Item�����ƶ����޸�
   3.9 ��Item����ʱ��Level
   4.0 RadioGroup�洢��������ԣ�HCView��ҳ���ʽ
   4.1 Combobox����Static���Կ���ֻѡ���ɱ༭
   4.2 Section��������Ϣ
   4.3 Data��Script���ԣ�SectionData�����ݴ�Сռλ
   4.4 ��EmrView���Լ�����Ϣ
   4.5 ResizeItem��CanResize����
   4.6 �ڴ洢ҳ���ʽ
   4.7 ����Ԫ�ۼ���ʽ��Ϊ���ϣ���ۼ�����
  }

  HC_FileVersion = '5.8';
  HC_FileVersionInt = 58;

  TabCharWidth = 28;  // Ĭ��Tab���(���) 14 * 2��
  DefaultColWidth = 50;
  PMSLineHeight = 24;  // ��д��Χ�ߵĳ���
  AnnotationWidth = 200;  // ��ע��ʾ������
  AnnotateBKColor = $00D5D5FF;
  AnnotateBKActiveColor = $00A8A8FF;
  HyperTextColor = $00C16305;
  HCTransparentColor = clNone;  // ͸��ɫ
  {$IFDEF UNPLACEHOLDERCHAR}
  /// <summary> ��ռλ�ַ�������ַ� </summary>
  TibetanVowel = #3962{e} + #3956{u} + #3954{i} + #3964{o};  // ����Ԫ����ĸ
  TibetanOther =  // �������������ַ�
      #4024 + #3966 + #3971 + #3895 + #3893 + #3967 + #4023 + #4026 + #3989
    + #3990 + #3963 + #4019 + #4018 + #3999 + #4017 + #4013 + #3968 + #3965 + #4005
    + #4009 + #4010 + #4011 + #4016 + #4022 + #4001 + #4006 + #3988 + #4008
    + #3972 + #3986 + #3986 + #4014 + #4015 + #4020 + #3984 + #3985 + #4004
    + #4003 + #4000 + #3991 + #3993 + #4028 + #4027 + #3865 + #3953 + #3902
    + #3903 + #3975 + #3974 + #3958 + #3959 + #3960 + #3961 + #3955 + #3994
    + #3957 + #3955 + #3996 + #4038 + #4021 + #4025 + #3970 + #3998 + #3995;
  UnPlaceholderChar = TibetanVowel + TibetanOther;
  {$ENDIF}
  /// <summary> ���������׵��ַ� </summary>
  DontLineFirstChar = '`-=[]\;,./~!@#$%^&*()_+{}|:"<>?�����������ܣ���������������������������������������������������������'
  {$IFDEF UNPLACEHOLDERCHAR}
    + UnPlaceholderChar  // �����ַ�
  {$ENDIF}
    ;
  /// <summary> ��������β���ַ� </summary>
  DontLineLastChar = '/\�ܡ���';
  /// <summary> ���Լ�ѹ��ȵ��ַ� </summary>
  LineSqueezeChar = '��������������';
  HCsLineBreak = sLineBreak;
  HCRecordSeparator = Char(#30);
  HCUnitSeparator = Char(#31);

  HCBoolText: array [Boolean] of Char = ('0', '1');

  PenTypes: array[Boolean] of Integer = (PS_COSMETIC, PS_GEOMETRIC);
  PenStyles: array[psSolid..psInsideFrame] of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL, PS_SOLID);

type
  THCProcedure = reference to procedure();
  THCFunction = reference to function(): Boolean;
  TLoadSectionProc = reference to procedure(const AFileVersion: Word);

  TPaperOrientation = (cpoPortrait, cpoLandscape);  // ֽ�ŷ������񡢺���

  TExpressArea = (ceaNone, ceaLeft, ceaTop, ceaRight, ceaBottom);  // ��ʽ�����򣬽��������������Ҹ�ʽ��

  TBorderSide = (cbsLeft, cbsTop, cbsRight, cbsBottom, cbsLTRB, cbsRTLB);
  TBorderSides = set of TBorderSide;

  TSectionArea = (saHeader, saPage, saFooter);  // ��ǰ��������ĵ���һ����
  TSectionAreas = set of TSectionArea;  // ����ʱ���ļ���������

  // ���Ԫ����뷽ʽ
  THCContentAlign = (tcaTopLeft, tcaTopCenter, tcaTopRight, tcaCenterLeft,
    tcaCenterCenter, tcaCenterRight, tcaBottomLeft, tcaBottomCenter, tcaBottomRight);

  THCState = (
    hosLoading,  // �ĵ�����
    hosSaving,   // �ĵ�����
    hosCopying,  // ����
    hosPasting,  // ճ��
    hosDomainWholeReplace,  // �������滻
    hosUndoing,
    hosRedoing,
    hosInsertBreakItem,
    hosBatchInsert,  // ����InsertItem����������Itemʱ(����������������2��)��ֹ��Ĳ�������λ�ñ仯���º������λ�ò���ȷ
    hosDestroying,  // �༭����������
    hosFormatBrushing  // ��ʽˢ
  );

  TCharType = (
    jctBreak,  //  �ضϵ�
    jctHZ,  // ����
    jctZM,  // �����ĸ
    //jctCNZM,  // ȫ����ĸ
    jctSZ,  // �������
    //jctCNSZ,  // ȫ������
    jctFH  // ��Ƿ���
    //jctCNFH   // ȫ�Ƿ���
  );

  THCAction = (
    actBackDeleteText,  // ��ǰɾ���ı�
    actDeleteText,  // ���ɾ���ı�
    actReturnItem,  // ��Item�ϻس�
    actInsertText,  // �����ı�
    actSetItemText,    // ֱ�Ӹ�ֵItem��Text
    actDeleteItem,  // ɾ��Item
    actInsertItem,  // ����Item
    actItemProperty,  // Item���Ա仯
    actItemSelf,  // Item�Լ�����
    actItemMirror,  // Item����
    actConcatText,  // ճ���ı�(��ͷ)
    actDeleteSelected  // ɾ��ѡ������
  );

    THCControlState = (hcsCustom, hcsChecked);
    THCControlStyle = (hcyRadio, hcyCheck);

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

  {$IFNDEF DELPHIXE2}
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
  function IsKeyDownEdit(const AKey: Word): Boolean;  // �������ݱ仯��KeyDown
  function IsDirectionKey(const AKey: Word): Boolean;

  function CreateExtPen(const APen: TPen): HPEN;

  /// <summary> Ч�ʸ��ߵķ����ַ����ַ���λ�ú��� </summary>
  function PosCharHC(const AChar: Char; const AStr: string{; const Offset: Integer = 1}): Integer;

  /// <summary> �����ַ����� </summary>
  function GetUnicodeCharType(const AChar: Char): TCharType;

  {$IFDEF UNPLACEHOLDERCHAR}
  /// <summary> �����ַ���ָ��λ�õ�ʵ����Ч��ǰ����λ�� </summary>
  /// <param name="AText"></param>
  /// <param name="AIndex"></param>
  /// <param name="AAfter = False">True:ǰ��False:��</param>
  /// <returns></returns>
  function GetTextActualOffset(const AText: string; const AOffset: Integer; const AAfter: Boolean = False): Integer;

  function IsUnPlaceHolderChar(const AChar: Char): Boolean;
  {$ENDIF}  // UNPLACEHOLDERCHAR

  /// <summary> �����ַ���λ��������ָ�����ַ��м�λ�� </summary>
  /// <param name="AIndex">ָ���ڼ����ַ�</param>
  /// <param name="ACharWArr">�ַ���λ������</param>
  /// <returns>�м�λ��</returns>
  function GetCharHalfFarfrom(
    {$IFDEF UNPLACEHOLDERCHAR}
    const AText: string;
    {$ENDIF}  // UNPLACEHOLDERCHAR
    const AOffset: Integer;
    const ACharWArr: array of Integer): Integer;

  /// <summary> ������ͨ���뷽ʽ(������)ָ��λ�����ַ����ĸ��ַ�����(0����һ��ǰ��) </summary>
  /// <param name="ACanvas"></param>
  /// <param name="AText"></param>
  /// <param name="X"></param>
  /// <returns></returns>
  function GetNorAlignCharOffsetAt(const ACanvas: TCanvas; const AText: string; const X: Integer): Integer;

  // ���ݺ��ִ�С��ȡ�������ִ�С
  function GetFontSize(const AFontSize: string): Single;
  function GetFontSizeStr(AFontSize: Single): string;
  function GetPaperSizeName(APaperSize: Integer): string;

  function GetVersionAsInteger(const AVersion: string): Integer;
  function GetBorderSidePro(const ABorderSides: TBorderSides): string;
  procedure SetBorderSideByPro(const AValue: string; var ABorderSides: TBorderSides);

  function HCDeleteBreak(const S: string): string;

  /// <summary> ���泤��С��65536���ֽڵ��ַ������� </summary>
  procedure HCSaveTextToStream(const AStream: TStream; const S: string);
  procedure HCLoadTextFromStream(const AStream: TStream; var S: string; const AFileVersion: Word);

  procedure HCSaveColorToStream(const AStream: TStream; const AColor: TColor);
  procedure HCLoadColorFromStream(const AStream: TStream; var AColor: TColor);

  function HCColorToRGBString(const AColor: TColor): string;
  function HCRGBStringToColor(const AColorStr: string): TColor;

  procedure BitmapSaveAsJPGE(const ABitmap: TBitmap; const AFile: string);
  procedure BitmapSaveAsPNG(const ABitmap: TBitmap; const AFile: string);

  procedure HCSetProperty(const APropertys: TStrings; const APropName, APropValue: string);
  procedure HCRemoveProperty(const APropertys: TStrings; const APropName: string);

  function CreateScriptObject: TStringList;

  /// <summary> �����ļ���ʽ���汾 </summary>
  procedure _SaveFileFormatAndVersion(const AStream: TStream);
  /// <summary> ��ȡ�ļ���ʽ���汾 </summary>
  procedure _LoadFileFormatAndVersion(const AStream: TStream;
    var AFileFormat: string; var AVersion: Word; var ALang: Byte);

  {$IFDEF DEBUG}
  procedure DrawDebugInfo(const ACanvas: TCanvas; const ALeft, ATop: Integer; const AInfo: string);
  {$ENDIF}

  procedure HCDrawArrow(const ACanvas: TCanvas; const AColor: TColor; const ALeft, ATop: Integer; const AType: Byte);
  procedure HCDrawFrameControl(const ACanvas: TCanvas; const ARect: TRect;
    const AState: THCControlState; AStyle: THCControlStyle);
  procedure HCDrawWave(const ACanvas: TCanvas; const ARect: TRect);

  function SaveCanvas(const ACanvas: TCanvas): THCCanvas;
  function ReplaceUnPreChar(const AText: string): string;  // �滻���ַ�����Ӣ�Ŀհ׺�#9�Ȳ�����ʶ����ַ�

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
    0: // ��
      begin
        ACanvas.Pen.Color := AColor;
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := psSolid;
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

    1: // ��
      begin
        ACanvas.Pen.Color := AColor;
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := psSolid;
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

    2: // ��
      begin

      end;

    3:  // ��
      begin

      end;
  end;
end;

procedure HCDrawFrameControl(const ACanvas: TCanvas; const ARect: TRect;
  const AState: THCControlState; AStyle: THCControlStyle);
var
  vRect: TRect;
begin
  vRect := Bounds(ARect.Left + (ARect.Width - 16) div 2, ARect.Top + (ARect.Height - 16) div 2 + 1, 16 - 2, 16 - 2);

  ACanvas.Pen.Color := $00848484;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsClear;
  if AStyle = THCControlStyle.hcyRadio then
  begin
    ACanvas.Ellipse(vRect);
    if AState = THCControlState.hcsChecked then
    begin
      ACanvas.Pen.Style := psClear;
      ACanvas.Brush.Color := clBlack;
      InflateRect(vRect, -2, -2);
      ACanvas.Ellipse(vRect);
    end;
  end
  else  // DFCS_BUTTONCHECK
  begin
    ACanvas.Rectangle(vRect);
    if AState = THCControlState.hcsChecked then
    begin
      ACanvas.Pen.Color := clBlack;
      ACanvas.Pen.Width := 2;
      ACanvas.MoveTo(vRect.Left + 3, vRect.Top + 16 div 2);
      ACanvas.LineTo(vRect.Left - 2 + 16 div 2, vRect.Bottom - 3);
      ACanvas.LineTo(vRect.Right - 3, vRect.Top + 3);
    end;
  end;
end;

function ReplaceUnPreChar(const AText: string): string;
begin
  Result := StringReplace(AText, Char($2002), Char($0020), [rfReplaceAll, rfIgnoreCase]);  // �滻EN SPACEΪSPACE
  Result := StringReplace(Result, #9, '', [rfReplaceAll, rfIgnoreCase]);  // �滻tabΪ��
end;

procedure HCDrawWave(const ACanvas: TCanvas; const ARect: TRect);
var
  vDT: Boolean;
  vStart: Integer;
begin
  vDT := False;
  vStart := ARect.Left;
  ACanvas.MoveTo(vStart, ARect.Bottom);
  while vStart < ARect.Right do
  begin
    vStart := vStart + 2;
    if vStart > ARect.Right then
      vStart := ARect.Right;

    if not vDT then
      ACanvas.LineTo(vStart, ARect.Bottom + 2)
    else
      ACanvas.LineTo(vStart, ARect.Bottom);

    vDT := not vDT;
  end;
end;

function CreateExtPen(const APen: TPen): HPEN;
var
  vPenParams: TLogBrush;
begin
  vPenParams.lbStyle := PenStyles[APen.Style];
  vPenParams.lbColor := APen.Color;
  vPenParams.lbHatch := 0;
  Result := ExtCreatePen(PenTypes[APen.Width <> 1] or PS_ENDCAP_SQUARE or vPenParams.lbStyle,
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

procedure HCSetProperty(const APropertys: TStrings; const APropName, APropValue: string);
begin
  if Pos('=', APropValue) > 0 then
    raise Exception.Create('HCSetProperty����ֵ�в�������"="��');

  if APropValue <> '' then
    APropertys.Values[APropName] := APropValue
  else
    HCRemoveProperty(APropertys, APropName);
end;

procedure HCRemoveProperty(const APropertys: TStrings; const APropName: string);
var
  vIndex: Integer;
begin
  vIndex := APropertys.IndexOfName(APropName);
  if vIndex >= 0 then
    APropertys.Delete(vIndex);
end;

function HCDeleteBreak(const S: string): string;
begin
  Result := StringReplace(S, sLineBreak, '', [rfReplaceAll]);
end;

function CreateScriptObject: TStringList;
begin
  Result := TStringList.Create;
  Result.NameValueSeparator := HCRecordSeparator;
  Result.LineBreak := HCUnitSeparator;
end;

function GetUnicodeCharType(const AChar: Char): TCharType;
begin
  case Cardinal(AChar) of
    $2E80..$2EF3,  // ������չ 115
    $2F00..$2FD5,  // �������� 214
    $2FF0..$2FFB,  // ���ֽṹ 12
    $3007,  // �� 1
    $3105..$312F,  // ����ע�� 43
    $31A0..$31BA,  // ע����չ 22
    $31C0..$31E3,  // ���ֱʻ� 36
    $3400..$4DB5,  // ��չA 6582��
    $4E00..$9FA5,  // �������� 20902��
    $9FA6..$9FEF,  // �������ֲ��� 74��
    $E400..$E5E8,  // ������չ 452
    $E600..$E6CF,  // PUA���� 207
    $E815..$E86F,  // PUA(GBK)���� 81
    $F900..$FAD9,  // ���ݺ��� 477
    $20000..$2A6D6, // ��չB 42711��
    $2A700..$2B734,  // ��չC 4149
    $2B740..$2B81D,  // ��չD 222
    $2B820..$2CEA1,  // ��չE 5762
    $2CEB0..$2EBE0,  // ��չF 7473
    $2F800..$2FA1D  // ������չ 542
      : Result := jctHZ;  // ����

    $0600..$06FF:
      Result := jctHZ;  // �������ģ�ά�����

    $0F00..$0FFF:  // ����
      Result := jctHZ;

    $1800..$18AF: Result := jctHZ;  // �ɹ��ַ�

    $21..$2F,  // !"#$%&'()*+,-./
    $3A..$40,  // :;<=>?@
    $5B..$60,  // [\]^_`
    $7B..$7E,   // {|}~
    $FFE0  // ��
      : Result := jctFH;

    //$FF01..$FF0F,  // ������������������������������

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
    #32..#126,  // <#32��ASCII������ #127��ASCII DEL
    #1536..#1791,  // �������ģ�ά�����
    #3840..#4095,  // ����
    #6144..#6319:  // �ɹ���
      Result := True;
  else
    Result := False;
  end;
  {$ELSE}
  Result := AKey in [#32..#126];  // <#32��ASCII������ #127��ASCII DEL
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
  if AFontSize = '����' then Result := 42
  else
  if AFontSize = 'С��' then Result := 36
  else
  if AFontSize = 'һ��' then Result := 26
  else
  if AFontSize = 'Сһ' then Result := 24
  else
  if AFontSize = '����' then Result := 22
  else
  if AFontSize = 'С��' then Result := 18
  else
  if AFontSize = '����' then Result := 16
  else
  if AFontSize = 'С��' then Result := 15
  else
  if AFontSize = '�ĺ�' then Result := 14
  else
  if AFontSize = 'С��' then Result := 12
  else
  if AFontSize = '���' then Result := 10.5
  else
  if AFontSize = 'С��' then Result := 9
  else
  if AFontSize = '����' then Result := 7.5
  else
  if AFontSize = 'С��' then Result := 6.5
  else
  if AFontSize = '�ߺ�' then Result := 5.5
  else
  if AFontSize = '�˺�' then Result := 5
  else
  if not TryStrToFloat(AFontSize, Result) then
    raise Exception.Create(HC_EXCEPTION + '�����ֺŴ�С�����޷�ʶ���ֵ��' + AFontSize);
end;

function GetFontSizeStr(AFontSize: Single): string;
begin
  if AFontSize = 42 then Result := '����'
  else
  if AFontSize = 36 then Result := 'С��'
  else
  if AFontSize = 26 then Result := 'һ��'
  else
  if AFontSize = 24 then Result := 'Сһ'
  else
  if AFontSize = 22 then Result := '����'
  else
  if AFontSize = 18 then Result := 'С��'
  else
  if AFontSize = 16 then Result := '����'
  else
  if AFontSize = 15 then Result := 'С��'
  else
  if AFontSize = 14 then Result := '�ĺ�'
  else
  if AFontSize = 12 then Result := 'С��'
  else
  if AFontSize = 10.5 then Result := '���'
  else
  if AFontSize = 9 then Result := 'С��'
  else
  if AFontSize = 7.5 then Result := '����'
  else
  if AFontSize = 6.5 then Result := 'С��'
  else
  if AFontSize = 5.5 then Result := '�ߺ�'
  else
  if AFontSize = 5 then Result := '�˺�'
  else
    Result := FormatFloat('0.#', AFontSize);
end;

function GetPaperSizeName(APaperSize: Integer): string;
begin
  case APaperSize of
    DMPAPER_A3: Result := 'A3';
    DMPAPER_A4: Result := 'A4';
    DMPAPER_A5: Result := 'A5';
    DMPAPER_B5: Result := 'B5';
    DMPAPER_HC_16K: Result := '16K';
  else
    Result := '�Զ���';
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

/// <summary> �����ļ���ʽ���汾 </summary>
procedure _SaveFileFormatAndVersion(const AStream: TStream);
var
  vS: string;
  vLang: Byte;
begin
  vS := HC_EXT;
  AStream.WriteBuffer(vS[1], Length(vS) * SizeOf(Char));
  // �汾
  vS := HC_FileVersion;
  AStream.WriteBuffer(vS[1], Length(vS) * SizeOf(Char));
  // ʹ�õı������
  vLang := HC_PROGRAMLANGUAGE;
  AStream.WriteBuffer(vLang, 1);
end;

/// <summary> ��ȡ�ļ���ʽ���汾 </summary>
procedure _LoadFileFormatAndVersion(const AStream: TStream;
  var AFileFormat: string; var AVersion: Word; var ALang: Byte);
var
  vFileVersion: string;
begin
  // �ļ���ʽ
  SetLength(AFileFormat, Length(HC_EXT));
  AStream.ReadBuffer(AFileFormat[1], Length(HC_EXT) * SizeOf(Char));

  // �汾
  SetLength(vFileVersion, Length(HC_FileVersion));
  AStream.ReadBuffer(vFileVersion[1], Length(HC_FileVersion) * SizeOf(Char));
  AVersion := GetVersionAsInteger(vFileVersion);

  if AVersion > 19 then // ʹ�õı������
    AStream.ReadBuffer(ALang, 1);
end;

procedure HCSaveColorToStream(const AStream: TStream; const AColor: TColor);
var
  vByte: Byte;
  vInt: Integer;
begin
  if AColor = HCTransparentColor then  // ͸��
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
    vInt := ColorToRGB(AColor);  // ת��clBtnFace�����ĸ���
    vByte := vInt and $FF;  // R
    AStream.WriteBuffer(vByte, 1);

    vByte := (vInt shr 8) and $FF;  // G
    AStream.WriteBuffer(vByte, 1);

    vByte := (vInt shr 16) and $FF;  // B
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
    AColor := vR or (vG shl 8) or (vB shl 16);
end;

function HCColorToRGBString(const AColor: TColor): string;
var
  vR, vG, vB: Byte;
begin
  if AColor = HCTransparentColor then
    Result := '0,255,255,255'
  else
  begin
    vR := Byte(AColor);
    vG := Byte(AColor shr 8);
    vB := Byte(AColor shr 16);
    Result := Format('255,%d,%d,%d', [vR, vG, vB]);
  end;
end;

function HCRGBStringToColor(const AColorStr: string): TColor;
var
  vsRGB: TStringList;
begin
  vsRGB := TStringList.Create;
  try
    vsRGB.Delimiter := ',';
    vsRGB.DelimitedText := AColorStr;

    if vsRGB.Count > 3 then
    begin
      if vsRGB[0] = '0' then
        Result := HCTransparentColor
      else
        Result := RGB(StrToInt(vsRGB[1]), StrToInt(vsRGB[2]), StrToInt(vsRGB[3]));
    end
    else
      Result := RGB(StrToInt(vsRGB[0]), StrToInt(vsRGB[1]), StrToInt(vsRGB[2]));
  finally
    FreeAndNil(vsRGB);
  end;
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
  Result := AOffset;  // ������һ������

  vLen := Length(AText);
  if AAfter then  // ����
  begin
    while Result < vLen do
    begin
      if Pos(AText[Result + 1], UnPlaceholderChar) > 0 then
        Inc(Result)
      else
        Break;
    end;
  end
  else  // ǰ��
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
  vCharWArr: array of Integer;  // ÿ���ַ����ƽ���λ��
  i, vLen: Integer;
  vSize: TSize;
begin
  Result := -1;

  if X < 0 then
    Result := 0
  else
  begin
    vLen := Length(AText);
    // �����е��豸�����ַ������һ���ַ������ַ��ķ�Χ֮�ͻ��������ַ����ķ�Χ
    SetLength(vCharWArr, vLen);
    GetTextExtentExPoint(ACanvas.Handle, PChar(AText), vLen, 0,
      nil, PInteger(vCharWArr), vSize);  // ����65535����Ԫ��ȡ����ֵ
    // 20190618002 ��Ҫͬ���޸ĵ��ַ���λ����صļ���
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
          if X > GetCharHalfFarfrom({$IFDEF UNPLACEHOLDERCHAR}AText,{$ENDIF} i, vCharWArr) then  // �ڽ����ĺ�벿��
            Result := i
          else  // ����ʼ��ǰ�벿�֣�����ǰһ���ַ������
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
