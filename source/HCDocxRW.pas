{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2019-1-28             }
{                                                       }
{              docx格式文件读写单元                     }
{                                                       }
{*******************************************************}

unit HCDocxRW;

interface

uses
  Windows, Classes, Graphics, HCView, HCCommon, HCXml, HCViewData,
  HCStyle, HCTextStyle, HCParaStyle, HCDocumentRW, Generics.Collections,
  HCItem, HCImageItem, HCGifItem, HCTableItem, HCTableRow, HCTableCell;

type
  THCZLibPackageFile = class
  strict private
    FFileName, FType, FContentType: string;
    FStream: TStream;
  public
    constructor Create(const AFileName: string; AStream: TStream; AStreamLength: Integer);
    destructor Destroy; override;
    property FileName: string read FFileName;
    property &Type: string read FType write FType;
    property ContentType: string read FContentType write FContentType;
    property Stream: TStream read FStream;
  end;

  THCDocxResFile = class
  public
    ID, &Type, Target, Extension: string;
    Stream: TMemoryStream;
    destructor Destroy; override;
  end;

  THCOpenXmlRelation = class
  strict private
    FId, FTarget, FType, FTargetMode: string;
  public
    constructor Create;

    property Id: string read FId write FId;
    property Target: string read FTarget write FTarget;
    property &Type: string read FType write FType;
    property TargetMode: string read FTargetMode write FTargetMode;
  end;

  THCOpenXmlRelations = class(TObjectList<THCOpenXmlRelation>)
  public
    function LookupRelationById(const AId: string): THCOpenXmlRelation;
    function LookupRelationByType(const AType: string): THCOpenXmlRelation;
    function GenerateId: string;
    function LookupRelationByTargetAndType(const ATarget, AType: string): THCOpenXmlRelation;
  end;

  THCDocxProperty = class(TObject)
  public
    DefaultTabStop: Cardinal;
  end;

  THCDocxStyle = class(TObject)
    StyleName: string;
    StyleID: string;
    Rsid: string;
    Link: string;
    BaseOn: string;
    UIPriority: Integer;
    SemiHidden: Boolean;
    Default: Boolean;
  end;

  THCDocxScriptStyle = (dssNone, dssSubscript, dssSuperscript);

  THCDocxTextStyle = class(THCDocxStyle)
  public
    AllCaps: Boolean;
    BackColor: TColor;
    DoubleFontSize: Integer;
    FontBold: Boolean;
    ForeColor: TColor;
    FontItalic: Boolean;
    FontName: string;
    //FontUnderlineType: TdxUnderlineType;
    //FontStrikeoutType: TdxStrikeoutType;
    Hidden: Boolean;
    NoProof: Boolean;
    Script: THCDocxScriptStyle;
    StrikeoutColor: TColor;
    StrikeoutWordsOnly: Boolean;
    UnderlineColor: TColor;
    UnderlineWordsOnly: Boolean;

    function GetHCFontStyles: THCFontStyles;
    procedure CopyFrom(const ASrc: THCDocxTextStyle);
    constructor Create;
  end;

  TDocxBorderInfo = record
  public
    Style: TBorderSide;
    Color: TColor;
    Width: Integer;
    Offset: Integer;
    Frame: Boolean;
    Shadow: Boolean;
  end;

  THCDocxParaStyle = class(THCDocxStyle)
  public
    QFormat: Boolean;
    AfterAutoSpacing: Boolean;
    Alignment: TParaAlignHorz;
    VerticalAlignment: TParaAlignVert;
    BackColor: TColor;
    BeforeAutoSpacing: Boolean;
    ContextualSpacing: Boolean;
    FirstLineIndent: Integer;  // 首行缩进(单位mm)
    FirstLineIndentType: TParaFirstLineIndent;
    KeepLinesTogether: Boolean;
    KeepWithNext: Boolean;
    LeftIndent, RightIndent: Integer;  // 左、右缩进(单位mm)
    LineSpacing: Single;
    LineSpacingType: TParaLineSpaceMode;
    OutlineLevel: Integer;
    PageBreakBefore: Boolean;
    SpacingAfter: Integer;
    SpacingBefore: Integer;
    SuppressHyphenation: Boolean;
    SuppressLineNumbers: Boolean;
    WidowOrphanControl: Boolean;

    LeftBorder: TDocxBorderInfo;
    RightBorder: TDocxBorderInfo;
    TopBorder: TDocxBorderInfo;
    BottomBorder: TDocxBorderInfo;
    TextStyle: THCDocxTextStyle;
    procedure CopyFrom(const ASrc: THCDocxParaStyle);
    procedure SetParaAlignment(const AAlign: string);
    procedure SetParaVerticalAlignment(const AAlign: string);
    constructor Create;
    destructor Destroy; override;
  end;

  THCDocxTableStyle = class(THCDocxStyle)
    Ind_W: Integer;
    Ind_Type: string;
    CellMargin_Left: Integer;
    CellMargin_LeftType: string;

    CellMargin_Top: Integer;
    CellMargin_TopType: string;

    CellMargin_Right: Integer;
    CellMargin_RightType: string;

    CellMargin_Bottom: Integer;
    CellMargin_BottomType: string;
  end;

  THCDocxCellMerge = (cmNone, cmRestart, cmContinue);

  TCellReadDataEvent = procedure(const AViewData: THCViewData; const ANode: IHCXMLNode; const AStyle: THCStyle) of object;

  THCDocxCell = class(TObject)
  private
    FOnReadData: TCellReadDataEvent;
    function GetAlignVertByName(const AName: string): THCAlignVert;
  public
    ViewData: THCViewData;
    Width: Integer;
    RowSpan, ColSpan: Integer;
    FillColor: TColor;
    AlignVert: THCAlignVert;
    Merge: THCDocxCellMerge;
    constructor Create; overload;
    constructor Create(const ANode: IHCXMLNode; const AStyle: THCStyle;
      const AOnReadData: TCellReadDataEvent); overload;
    destructor Destroy; override;
  end;

  THCDocxRow = class(TObject)
  private
    FOnReadData: TCellReadDataEvent;
  public
    Width: Integer;
    Cells: TObjectList<THCDocxCell>;
    constructor Create(const ANode: IHCXMLNode; const AStyle: THCStyle;
      const AOnReadData: TCellReadDataEvent);
    destructor Destroy; override;
    property OnReadData: TCellReadDataEvent read FOnReadData write FOnReadData;
  end;

  THCDocxTable = class(TObject)
  public
    Rows: TObjectList<THCDocxRow>;
    constructor Create(const ANode: IHCXMLNode; const AStyle: THCStyle;
      const AOnReadData: TCellReadDataEvent);
    destructor Destroy; override;
    function ConvertToHCTable(const AData: THCViewData): THCTableItem;
  end;

  THCDocxReader = class(THCDocumentReader)
  private
    FHCView: THCView;
    /// <summary> 生成docx文件的程序 </summary>
    FApplication: string;
    /// <summary> docx文件版本 </summary>
    FAppVersion: string;
    FCreator: string;
    FCreatedDT: string;
    FLastModifiedBy: string;
    FModifiedDT: string;
    FHeaderID, FFooterID: string;
    /// <summary> 解压后的word文件夹路径 </summary>
    FWordFolder: string;
    /// <summary> 解压后的docProps文件夹路径 </summary>
    FDocProps: string;
    FResFileId: Cardinal;
    //
    FPackageFile: TObjectList<THCZLibPackageFile>;
    FDocxProperty: THCDocxProperty;
    FTextStyles: TObjectList<THCDocxTextStyle>;
    FParaStyles: TObjectList<THCDocxParaStyle>;
    FTableStyles: TObjectList<THCDocxTableStyle>;
    FResFiles: TObjectList<THCDocxResFile>;

    procedure LoadFromZLibStream(const AStream: TStream);
    function GetZLibPackageFile(const AFileName: string): THCZLibPackageFile;
    function GetRelationTargetByType(const ARelations: THCOpenXmlRelations; const AType: string): string;
    function ReadRelations(const AFileName: string): THCOpenXmlRelations;
    //
    function FindTextStyleDefault: THCDocxTextStyle;
    function FindTextStyleById(const AStyleId: string): THCDocxTextStyle;
    function FindParaStyleDefault: THCDocxParaStyle;
    function FindParaStyleById(const AStyleId: string): THCDocxParaStyle;
    function FindResFileById(const AResID: string): THCDocxResFile;
    //
    function FindHCParaNo(const AParaStyle: THCDocxParaStyle): Integer;
    function FindHCStyleNo(const ATextStyle: THCDocxTextStyle): Integer;
    //
    procedure ReadExtended_(const AFile: string);
    procedure ReadCore_(const AFile: string);
    procedure ReadFontTable_(const AFile: string);
    procedure ReadSettings_(const AFile: string);
    procedure ReadWebSettings_(const AFile: string);
    procedure ReadTextStyleProperty(const ATextStyle: THCDocxTextStyle; const APrNode: IHCXMLNode);
    procedure ReadParaStyleProperty(const AParaStyle: THCDocxParaStyle; const APrNode: IHCXMLNode);
    procedure ReadStyles_(const AFile: string);
    procedure ReadStyleWithEffects_(const AFile: string);
    procedure ReadNumbering_(const AFile: string);
    procedure ReadFootNotes_(const AFile: string);
    procedure ReadEndNotes_(const AFile: string);
    procedure ReadComments_(const AFile: string);
    procedure ReadHeader_(const AFile: string);
    procedure ReadFooter_(const AFile: string);
    procedure ReadDocument_(const AFile: string);
    //
    procedure ReadTextItem_(const AData: THCViewData; const AParaFirst: Boolean;
      const ANode: IHCXMLNode; const AParaNo, AStyleNo: Integer);
    procedure ReadTable_(const AData: THCViewData; const AParaFirst: Boolean;
      const ATableNode: IHCXMLNode; const AStyle: THCStyle);
    procedure ReadDrawing_(const AData: THCViewData; const AParaFirst: Boolean;
      const ANode: IHCXMLNode; const AParaNo: Integer);

    /// <summary> 读取段中的内容 </summary>
    /// <param name="AData"></param>
    /// <param name="AParaNode"></param>
    /// <param name="AStyle"></param>
    /// <param name="AFirstTime">第一次读取段内容(便于迭代调用时区另)</param>
    procedure ReadParagraph_(const AData: THCViewData; const AParaNode: IHCXMLNode;
      const AStyle: THCStyle; const AFirstTime: Boolean = True);
    procedure ReadData_(const AData: THCViewData; const ANode: IHCXMLNode; const AStyle: THCStyle);
    //
    function NextResFileId: string;
    procedure WriteTextStyleProperty(const ATextStyle: THCTextStyle; const APrNode: IHCXMLNode);
    procedure WriteData_(const AData: THCViewData; const ANode: IHCXMLNode);
    procedure WriteTable_(const ANode: IHCXMLNode; const ATableItem: THCTableItem);
    procedure WriteDrawingImage_(const ANode: IHCXMLNode; const AImageItem: THCImageItem);
    procedure WriteDrawingGif_(const ANode: IHCXMLNode; const AGifItem: THCGifItem);
    //
    function WriteDocumentXmlRels_(const APackageFile: TObjectList<THCZLibPackageFile>;
      const AFileName: string): THCZLibPackageFile;
    function WriteExtended_: THCZLibPackageFile;
    function WriteCore_: THCZLibPackageFile;
    function WriteFontTable_: THCZLibPackageFile;
    function WriteSettings_: THCZLibPackageFile;
    function WriteWebSettings_: THCZLibPackageFile;
    function WriteStyles_: THCZLibPackageFile;
    function WriteStyleWithEffects_: THCZLibPackageFile;
    function WriteNumbering_: THCZLibPackageFile;
    function WriteFootNotes_: THCZLibPackageFile;
    function WriteEndNotes_: THCZLibPackageFile;
    function WriteComments_: THCZLibPackageFile;
    function WriteHeader1_: THCZLibPackageFile;
    function WriteFooter1_: THCZLibPackageFile;
    function WriteDocument_: THCZLibPackageFile;
    function WriteRootRelations_: THCZLibPackageFile;
    function WriteContentTypes_(const APackageFile: TObjectList<THCZLibPackageFile>): THCZLibPackageFile;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(const AHCView: THCView; const AStream: TStream); override;
    procedure SaveToStream(const AHCView: THCView; const AStream: TStream); override;
  end;

implementation

{$I HCView.inc}

uses
  SysUtils, IOUtils, Math, HCZLib, HCUnitConversion;

const
  HCDOCX_DEFAULTFONT = 'Calibri';
  HCDOCX_Relationships = 'http://schemas.openxmlformats.org/package/2006/relationships';
  HCDOCX_DocumentRelationships = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships';
  HCDOCX_OfficeDocument = HCDOCX_DocumentRelationships + '/officeDocument';
  HCDOCX_CoreProperties = 'http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties';
  HCDOCX_ExtendedProperties = HCDOCX_DocumentRelationships + '/extended-properties';

  HCDOCX_Main = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main';
  HCDOCX_FontTable = HCDOCX_DocumentRelationships + '/fontTable';
  HCDOCX_Settings = HCDOCX_DocumentRelationships + '/settings';
  HCDOCX_WebSettings = HCDOCX_DocumentRelationships + '/webSettings';
  HCDOCX_Styles = HCDOCX_DocumentRelationships + '/styles';
  HCDOCX_Numbering = HCDOCX_DocumentRelationships + '/numbering';
  HCDOCX_FootNotes = HCDOCX_DocumentRelationships + '/footnotes';
  HCDOCX_EndNotes = HCDOCX_DocumentRelationships + '/endnotes';
  HCDOCX_Comments = HCDOCX_DocumentRelationships + '/comments';
  HCDOCX_StyleWithEffects = 'http://schemas.microsoft.com/office/2007/relationships/stylesWithEffects';
  HCDOCX_Header = HCDOCX_DocumentRelationships + '/header';
  HCDOCX_Footer = HCDOCX_DocumentRelationships + '/footer';
  HCDOCX_IMAGE = HCDOCX_DocumentRelationships + '/image';

  HCDOCX_NSwps = 'http://schemas.microsoft.com/office/word/2010/wordprocessingShape';
  HCDOCX_NSwp14 = 'http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing';
  HCDOCX_NSmc = 'http://schemas.openxmlformats.org/markup-compatibility/2006';

  HCDOCX_HeaderContentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml';
  HCDOCX_FooterContentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml';

  DOCXERROR = 'HCDocx异常：';
  HCDOCXERROR_ENCRYPTED = DOCXERROR + '无法打开加密的docx文件！';

{ THCDocxReader }

{$REGION 'GetColorByName 根据颜色名称返回颜色值'}
function GetColorByName(const AColorName: string): TColor;
var
  vColorName: string;
begin
  vColorName := LowerCase(AColorName);
  if vColorName = 'aliceblue' then
    Result := $00FFF8F0
  else
  if vColorName = 'antiquewhite' then
    Result := $00D7EBFA
  else
  if vColorName = 'aqua' then
    Result := clAqua
  else
  if vColorName = 'aquamarine' then
    Result := $007FD4FF
  else
  if vColorName = 'azure' then
    Result := $00FFFFF0
  else
  if vColorName = 'beige' then
    Result := $00DCF5F5
  else
  if vColorName = 'bisque' then
    Result := $00C4E4FF
  else
  if vColorName = 'black' then
    Result := clBlack
  else
  if vColorName = 'blanchedalmond' then
    Result := $00CDFFFF
  else
  if vColorName = 'blue' then
    Result := clBlue
  else
  if vColorName = 'blueviolet' then
    Result := $00E22B8A
  else
  if vColorName = 'brown' then
    Result := $002A2AA5
  else
  if vColorName = 'burlywood' then
    Result := $0087B8DE
  else
  if vColorName = 'cadetblue' then
    Result := $00A09E5F
  else
  if vColorName = 'chartreuse' then
    Result := $0000FF7F
  else
  if vColorName = 'chocolate' then
    Result := $001E69D2
  else
  if vColorName = 'coral' then
    Result := $00507FFF
  else
  if vColorName = 'cornflowerblue' then
    Result := $00ED9564
  else
  if vColorName = 'cornsilk' then
    Result := $00DCF8FF
  else
  if vColorName = 'crimson' then
    Result := $003C14DC
  else
  if vColorName = 'cyan' then
    Result := $00FFFF00
  else
  if vColorName = 'darkblue' then
    Result := $008B0000
  else
  if vColorName = 'darkcyan' then
    Result := $008B8B00
  else
  if vColorName = 'darkgoldenrod' then
    Result := $000B86B8
  else
  if vColorName = 'darkgray' then
    Result := $00A9A9A9
  else
  if vColorName = 'darkgreen' then
    Result := $00006400
  else
  if vColorName = 'darkkhaki' then
    Result := $006BB7BD
  else
  if vColorName = 'darkmagenta' then
    Result := $008B008B
  else
  if vColorName = 'darkolivegreen' then
    Result := $002F6B55
  else
  if vColorName = 'darkorange' then
    Result := $00008CFF
  else
  if vColorName = 'darkorchid' then
    Result := $00CC3299
  else
  if vColorName = 'darkred' then
    Result := $0000008B
  else
  if vColorName = 'darksalmon' then
    Result := $007A96E9
  else
  if vColorName = 'darkseagreen' then
    Result := $008FBC8F
  else
  if vColorName = 'darkslateblue' then
    Result := $008B3D48
  else
  if vColorName = 'darkslategray' then
    Result := $004F4F2F
  else
  if vColorName = 'darkturquoise' then
    Result := $00D1CE00
  else
  if vColorName = 'darkviolet' then
    Result := $00D30094
  else
  if vColorName = 'deeppink' then
    Result := $009314FF
  else
  if vColorName = 'deepskyblue' then
    Result := $00FFBF00
  else
  if vColorName = 'dimgray' then
    Result := $00696969
  else
  if vColorName = 'dodgerblue' then
    Result := $00FF901E
  else
  if vColorName = 'firebrick' then
    Result := $002222B2
  else
  if vColorName = 'floralwhite' then
    Result := $00F0FAFF
  else
  if vColorName = 'forestgreen' then
    Result := $00228B22
  else
  if vColorName = 'fuchsia' then
    Result := $00FF00FF
  else
  if vColorName = 'gainsboro' then
    Result := $00DCDCDC
  else
  if vColorName = 'ghostwhite' then
    Result := $00FFF8F8
  else
  if vColorName = 'gold' then
    Result := $0000D7FF
  else
  if vColorName = 'goldenrod' then
    Result := $0020A5DA
  else
  if vColorName = 'gray' then
    Result := clGray
  else
  if vColorName = 'green' then
    Result := clGreen
  else
  if vColorName = 'greenyellow' then
    Result := $002FFFAD
  else
  if vColorName = 'honeydew' then
    Result := $00F0FFF0
  else
  if vColorName = 'hotpink' then
    Result := $00B469FF
  else
  if vColorName = 'indianred' then
    Result := $005C5CCD
  else
  if vColorName = 'indigo' then
    Result := $0082004B
  else
  if vColorName = 'ivory' then
    Result := $00F0F0FF
  else
  if vColorName = 'khaki' then
    Result := $008CE6F0
  else
  if vColorName = 'lavender' then
    Result := $00FAE6E6
  else
  if vColorName = 'lavenderblush' then
    Result := $00F5F0FF
  else
  if vColorName = 'lawngreen' then
    Result := $0000FC7C
  else
  if vColorName = 'lemonchiffon' then
    Result := $00CDFAFF
  else
  if vColorName = 'lightblue' then
    Result := $00E6D8AD
  else
  if vColorName = 'lightcoral' then
    Result := $008080F0
  else
  if vColorName = 'lightcyan' then
    Result := $00FFFFE0
  else
  if vColorName = 'lightgoldenrodyellow' then
    Result := $00D2FAFA
  else
  if vColorName = 'lightgreen' then
    Result := $0090EE90
  else
  if vColorName = 'lightgrey' then
    Result := $00D3D3D3
  else
  if vColorName = 'lightpink' then
    Result := $00C1B6FF
  else
  if vColorName = 'lightsalmon' then
    Result := $007AA0FF
  else
  if vColorName = 'lightseagreen' then
    Result := $00AAB220
  else
  if vColorName = 'lightskyblue' then
    Result := $00FACE87
  else
  if vColorName = 'lightslategray' then
    Result := $00998877
  else
  if vColorName = 'lightsteelblue' then
    Result := $00DEC4B0
  else
  if vColorName = 'lightyellow' then
    Result := $00E0FFFF
  else
  if vColorName = 'lime' then
    Result := clLime
  else
  if vColorName = 'limegreen' then
    Result := $0032CD32
  else
  if vColorName = 'linen' then
    Result := $00E6F0FA
  else
  if vColorName = 'magenta' then
    Result := $00FF00FF
  else
  if vColorName = 'maroon' then
    Result := clMaroon
  else
  if vColorName = 'mediumaquamarine' then
    Result := $00AACD66
  else
  if vColorName = 'mediumblue' then
    Result := $00CD0000
  else
  if vColorName = 'mediumorchid' then
    Result := $00D355BA
  else
  if vColorName = 'mediumpurple' then
    Result := $00DB7093
  else
  if vColorName = 'mediumseagreen' then
    Result := $0071B33C
  else
  if vColorName = 'mediumslateblue' then
    Result := $00EE687B
  else
  if vColorName = 'mediumspringgreen' then
    Result := $009AFA00
  else
  if vColorName = 'mediumturquoise' then
    Result := $00CCD148
  else
  if vColorName = 'mediumvioletred' then
    Result := $008515C7
  else
  if vColorName = 'midnightblue' then
    Result := $00701919
  else
  if vColorName = 'mintcream' then
    Result := $00FAFFF5
  else
  if vColorName = 'mistyrose' then
    Result := $00E1E4FF
  else
  if vColorName = 'moccasin' then
    Result := $00B5E4FF
  else
  if vColorName = 'navajowhite' then
    Result := $00ADDEFF
  else
  if vColorName = 'navy' then
    Result := clNavy
  else
  if vColorName = 'oldlace' then
    Result := $00E6F5FD
  else
  if vColorName = 'olive' then
    Result := $00008080
  else
  if vColorName = 'olivedrab' then
    Result := $00238E6B
  else
  if vColorName = 'orange' then
    Result := $0000A5FF
  else
  if vColorName = 'orangered' then
    Result := $000045FF
  else
  if vColorName = 'orchid' then
    Result := $00D670DA
  else
  if vColorName = 'palegoldenrod' then
    Result := $00AAE8EE
  else
  if vColorName = 'palegreen' then
    Result := $0098FB98
  else
  if vColorName = 'paleturquoise' then
    Result := $00EEEEAF
  else
  if vColorName = 'palevioletred' then
    Result := $009370DB
  else
  if vColorName = 'papayawhip' then
    Result := $00D5EFFF
  else
  if vColorName = 'peachpuff' then
    Result := $00BDDBFF
  else
  if vColorName = 'peru' then
    Result := $003F85CD
  else
  if vColorName = 'pink' then
    Result := $00CBC0FF
  else
  if vColorName = 'plum' then
    Result := $00DDA0DD
  else
  if vColorName = 'powderblue' then
    Result := $00E6E0B0
  else
  if vColorName = 'purple' then
    Result := $00800080
  else
  if vColorName = 'red' then
    Result := clRed
  else
  if vColorName = 'rosybrown' then
    Result := $008F8FBC
  else
  if vColorName = 'royalblue' then
    Result := $00E16941
  else
  if vColorName = 'saddlebrown' then
    Result := $0013458B
  else
  if vColorName = 'salmon' then
    Result := $007280FA
  else
  if vColorName = 'sandybrown' then
    Result := $0060A4F4
  else
  if vColorName = 'seagreen' then
    Result := $00578B2E
  else
  if vColorName = 'seashell' then
    Result := $00EEF5FF
  else
  if vColorName = 'sienna' then
    Result := $002D52A0
  else
  if vColorName = 'silver' then
    Result := $00C0C0C0
  else
  if vColorName = 'skyblue' then
    Result := $00EBCE87
  else
  if vColorName = 'slateblue' then
    Result := $00CD5A6A
  else
  if vColorName = 'slategray' then
    Result := $00908070
  else
  if vColorName = 'snow' then
    Result := $00FAFAFF
  else
  if vColorName = 'springgreen' then
    Result := $007FFF00
  else
  if vColorName = 'steelblue' then
    Result := $00B48246
  else
  if vColorName = 'tan' then
    Result := $008CB4D2
  else
  if vColorName = 'teal' then
    Result := clTeal
  else
  if vColorName = 'thistle' then
    Result := $00D8BFD8
  else
  if vColorName = 'tomato' then
    Result := $004763FD
  else
  if vColorName = 'turquoise' then
    Result := $00D0E040
  else
  if vColorName = 'violet' then
    Result := $00EE82EE
  else
  if vColorName = 'wheat' then
    Result := $00B3DEF5
  else
  if vColorName = 'white' then
    Result := clWhite
  else
  if vColorName = 'whitesmoke' then
    Result := $00F5F5F5
  else
  if vColorName = 'yellow' then
    Result := clYellow
  else
  if vColorName = 'yellowgreen' then
    Result := $0032CD9A
  else
  if vColorName = 'activeborder' then
    Result := clActiveBorder
  else
  if vColorName = 'activecaption' then
    Result := clActiveCaption
  else
  if vColorName = 'appworkspace' then
    Result := clAppWorkSpace
  else
  if vColorName = 'background' then
    Result := clBackground
  else
  if vColorName = 'buttonface' then
    Result := clBtnFace
  else
  if vColorName = 'buttonhighlight' then
    Result := clBtnHighlight
  else
  if vColorName = 'buttonshadow' then
    Result := clBtnShadow
  else
  if vColorName = 'buttontext' then
    Result := clBtnText
  else
  if vColorName = 'captiontext' then
    Result := clCaptionText
  else
  if vColorName = 'graytext' then
    Result := clGrayText
  else
  if vColorName = 'highlight' then
    Result := clHighlight
  else
  if vColorName = 'highlighttext' then
    Result := clHighlightText
  else
  if vColorName = 'inactiveborder' then
    Result := clInactiveBorder
  else
  if vColorName = 'inactivecaption' then
    Result := clInactiveCaption
  else
  if vColorName = 'inactivecaptiontext' then
    Result := clInactiveCaptionText
  else
  if vColorName = 'infobackground' then
    Result := clInfoBk
  else
  if vColorName = 'infotext' then
    Result := clInfoText
  else
  if vColorName = 'menu' then
    Result := clMenu
  else
  if vColorName = 'menutext' then
    Result := clMenuText
  else
  if vColorName = 'scrollbar' then
    Result := clScrollBar
  else
  if vColorName = 'threeddarkshadow' then
    Result := cl3DDkShadow
  else
  if vColorName = 'threedface' then
    Result := clBtnFace
  else
  if vColorName = 'threedhighlight' then
    Result := clHighlightText
  else
  if vColorName = 'threedlightshadow' then
    Result := cl3DLight
  else
  if vColorName = 'threedshadow' then
    Result := clBtnShadow
  else
  if vColorName = 'window' then
    Result := clWindow
  else
  if vColorName = 'windowframe' then
    Result := clWindowFrame
  else
  if vColorName = 'windowtext' then
    Result := clWindowText
  else
    Result := clBlack;
end;
{$ENDREGION}

function ConvertHtmlColor(const ARGB: Cardinal): TColor;
var
  vR, vG, vB: Byte;
begin
  //vA := ARGB shr 24;
  vB := Byte(ARGB);
  vG := Byte(ARGB shr 8);
  vR := Byte(ARGB shr 16);

  Result := vR or (vG shl 8) or (vB shl 16);
end;

function GetOpenXmlNodeColor(const ARGB: string): TColor;
var
  vColor: Integer;
begin
  if TryStrToInt('$' + ARGB, vColor) then
    Result := ConvertHtmlColor(vColor)
  else
    Result := GetColorByName(ARGB);
end;

function ConvertColorToOpenXml(const AColor: TColor): string;
var
  vR, vG, vB: Byte;
begin
  //vA := ARGB shr 24;
  vR := Byte(AColor);
  vG := Byte(AColor shr 8);
  vB := Byte(AColor shr 16);

  Result := Copy(IntToHex(vR or (vG shl 8) or (vB shl 16){$IFNDEF DELPHIXE}, 8{$ENDIF}), 3, 6);
end;

function GetAttributeAsString(const ANode: IHCXMLNode; const AttrName: string): string;
var
  vNode: IHCXMLNode;
begin
  Result := '';
  vNode := ANode.AttributeNodes.FindNode(AttrName);
  if Assigned(vNode) then
    Result := vNode.Text;
end;

function FindChildNodeByName(const ANode: IHCXMLNode; const ANodeName: string): IHCXMLNode;
var
  i: Integer;
begin
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    if ANode.ChildNodes[i].NodeName = ANodeName then
    begin
      Result := ANode.ChildNodes[i];
      Break;
    end;
  end;
end;

constructor THCDocxReader.Create;
begin
  inherited Create;
  FPackageFile := TObjectList<THCZLibPackageFile>.Create;
  FDocxProperty := THCDocxProperty.Create;
  FTextStyles := TObjectList<THCDocxTextStyle>.Create;
  FParaStyles := TObjectList<THCDocxParaStyle>.Create;
  FTableStyles := TObjectList<THCDocxTableStyle>.Create;
  FResFiles := TObjectList<THCDocxResFile>.Create;
end;

destructor THCDocxReader.Destroy;
begin
  FPackageFile.Free;
  FDocxProperty.Free;
  FTextStyles.Free;
  FParaStyles.Free;
  FTableStyles.Free;
  FResFiles.Free;
  inherited Destroy;
end;

function THCDocxReader.FindHCParaNo(const AParaStyle: THCDocxParaStyle): Integer;
var
  vHCParaStyle: THCParaStyle;
begin
  Result := THCStyle.Null;

  vHCParaStyle := THCParaStyle.Create;
  try
    vHCParaStyle.AlignHorz := AParaStyle.Alignment;
    vHCParaStyle.AlignVert := AParaStyle.VerticalAlignment;
    vHCParaStyle.LineSpaceMode := AParaStyle.LineSpacingType;
    vHCParaStyle.FirstIndent := AParaStyle.FirstLineIndent;
    vHCParaStyle.LeftIndent := AParaStyle.LeftIndent;
    vHCParaStyle.RightIndent := AParaStyle.RightIndent;
    vHCParaStyle.BackColor := AParaStyle.BackColor;

    Result := FHCView.Style.GetParaNo(vHCParaStyle, True);
  finally
    FreeAndNil(vHCParaStyle);
  end;
end;

function THCDocxReader.FindHCStyleNo(const ATextStyle: THCDocxTextStyle): Integer;
var
  vHCTextStyle: THCTextStyle;
begin
  Result := THCStyle.Null;

  vHCTextStyle := THCTextStyle.Create;
  try
    vHCTextStyle.Family := ATextStyle.FontName;
    vHCTextStyle.Size := ATextStyle.DoubleFontSize / 2;
    vHCTextStyle.FontStyles := ATextStyle.GetHCFontStyles;
    vHCTextStyle.Color := ATextStyle.ForeColor;
    vHCTextStyle.BackColor := ATextStyle.BackColor;

    Result := FHCView.Style.GetStyleNo(vHCTextStyle, True);
  finally
    FreeAndNil(vHCTextStyle);
  end;
end;

function THCDocxReader.FindParaStyleById(const AStyleId: string): THCDocxParaStyle;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FParaStyles.Count - 1 do
  begin
    if FParaStyles[i].StyleID = AStyleId then
    begin
      Result := FParaStyles[i];
      Break;
    end;
  end;
end;

function THCDocxReader.FindParaStyleDefault: THCDocxParaStyle;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FParaStyles.Count - 1 do
  begin
    if FParaStyles[i].Default then
    begin
      Result := FParaStyles[i];
      Break;
    end;
  end;
end;

function THCDocxReader.FindResFileById(const AResID: string): THCDocxResFile;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FResFiles.Count - 1 do
  begin
    if FResFiles[i].ID = AResID then
    begin
      Result := FResFiles[i];
      Break;
    end;
  end;
end;

function THCDocxReader.FindTextStyleById(const AStyleId: string): THCDocxTextStyle;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FTextStyles.Count - 1 do
  begin
    if FTextStyles[i].StyleID = AStyleId then
    begin
      Result := FTextStyles[i];
      Break;
    end;
  end;
end;

function THCDocxReader.FindTextStyleDefault: THCDocxTextStyle;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FTextStyles.Count - 1 do
  begin
    if FTextStyles[i].Default then
    begin
      Result := FTextStyles[i];
      Break;
    end;
  end;
end;

function THCDocxReader.GetRelationTargetByType(const ARelations: THCOpenXmlRelations; const AType: string): string;
var
  vRelation: THCOpenXmlRelation;
begin
  Result := '';
  vRelation := ARelations.LookupRelationByType(AType);
  if Assigned(vRelation) then
    Result := vRelation.Target;
end;

function THCDocxReader.GetZLibPackageFile(const AFileName: string): THCZLibPackageFile;
var
  i: Integer;
  vFileName: string;
begin
  Result := nil;
  //if FPackageFile = nil then Exit;
  vFileName := StringReplace(AFileName, '\', '/', [rfReplaceAll]);
  if (Length(vFileName) > 0) and (vFileName[1] = '/') then
    Delete(vFileName, 1, 1);

  for i := 0 to FPackageFile.Count - 1 do
  begin
    if FPackageFile[i].FileName = vFileName then
    begin
      Result := FPackageFile[i];
      Break;
    end;
  end;
end;

function THCDocxReader.ReadRelations(const AFileName: string): THCOpenXmlRelations;
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
  vNode: IHCXMLNode;
  vRelation: THCOpenXmlRelation;
  i: Integer;
begin
  Result := THCOpenXmlRelations.Create;
  vPackageFile := GetZLibPackageFile(AFileName);
  if vPackageFile <> nil then
  begin
    vXml := THCXMLDocument.Create(nil);
    vXml.LoadFromStream(vPackageFile.Stream);
    if vXml.DocumentElement.NamespaceURI <> HCDOCX_Relationships then Exit;

    for i := 0 to vXml.DocumentElement.ChildNodes.Count - 1 do
    begin
      vNode := vXml.DocumentElement.ChildNodes[i];

      if (vNode.AttributeNodes.FindNode('Id') = nil)
        or (vNode.AttributeNodes.FindNode('Type') = nil)
        or (vNode.AttributeNodes.FindNode('Target') = nil)
      then
        Continue;

      vRelation := THCOpenXmlRelation.Create;
      vRelation.Id := vNode.Attributes['Id'];
      vRelation.&Type := vNode.Attributes['Type'];
      vRelation.Target := vNode.Attributes['Target'];

      if vNode.AttributeNodes.FindNode('TargetMode') <> nil then
        vRelation.TargetMode := vNode.Attributes['TargetMode']
      else
        vRelation.TargetMode := '';

      Result.Add(vRelation);
    end;
  end;
end;

procedure THCDocxReader.LoadFromStream(const AHCView: THCView;
  const AStream: TStream);
var
  vRelations: THCOpenXmlRelations;
  vRelation: THCOpenXmlRelation;
  vFile: string;
  i: Integer;
  vResFile: THCDocxResFile;
begin
  FHCView := AHCView;
  LoadFromZLibStream(AStream);

  vRelations := ReadRelations('_rels/.rels');
  try
    vFile := GetRelationTargetByType(vRelations, HCDOCX_CoreProperties);  // 取app.xml和core.xml的路径(docProps)
    if vFile <> '' then
    begin
      FDocProps := TPath.GetDirectoryName(vFile);
      vFile := GetRelationTargetByType(vRelations, HCDOCX_OfficeDocument);  // 取document.xml的路径(word)
      ReadExtended_(FDocProps + '/app.xml');
      ReadCore_(FDocProps + '/core.xml');
    end;

    vFile := GetRelationTargetByType(vRelations, HCDOCX_OfficeDocument);  // 取word的路径(word)
    FWordFolder := TPath.GetDirectoryName(vFile);  // word路径目录
  finally
    vRelations.Free;
  end;

  // 取document.xml.rels中的关系信息
  vRelations := ReadRelations(FWordFolder + '/_rels/document.xml.rels');
  try
    for i := 0 to vRelations.Count - 1 do
    begin
      vRelation := vRelations[i];

      if vRelation.&Type = HCDOCX_Numbering then  // numbering.xml
        ReadNumbering_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_FontTable then  // fontTable.xml
        ReadFontTable_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_Settings then  // settings.xml
        ReadSettings_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_WebSettings then  // webSettings.xml
        ReadWebSettings_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_Comments then  // comments.xml
        ReadComments_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_Styles then  // styles.xml
        ReadStyles_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_StyleWithEffects then  // stylesWithEffects.xml
        ReadStyleWithEffects_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_FootNotes then  // footnotes.xml
        ReadFootNotes_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_EndNotes then  // endnotes.xml
        ReadEndNotes_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_Header then  // header1、2、3.xml
        ReadHeader_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_Footer then  // footer1、2、3.xml
        ReadFooter_(FWordFolder + '/' + vRelation.Target)
      else
      if vRelation.&Type = HCDOCX_IMAGE then  // image
      begin
        vResFile := THCDocxResFile.Create;
        vResFile.ID := vRelation.Id;
        if vRelation.Target[1] <> '/' then
          vResFile.Target := FWordFolder + '/' + vRelation.Target
        else
          vResFile.Target := vRelation.Target;

        FResFiles.Add(vResFile);
      end;
    end;
  finally
    vRelations.Free;
  end;

  ReadDocument_(FWordFolder + '/document.xml');
end;

procedure THCDocxReader.LoadFromZLibStream(const AStream: TStream);
var
  vZLib: THCZLib;
  vFileEntry: THCZLibFileEntry;
  vDataStream: TMemoryStream;
  vName: string;
  i: Integer;
begin
  FPackageFile.Clear;

  vZLib := THCZLib.Create;
  try
    vZLib.LoadFromStream(AStream);

    for i := 0 to vZLib.Files.Count - 1 do
    begin
      vFileEntry := vZLib.Files[I];

      vDataStream := TMemoryStream.Create;
      try
        vZLib.Extract(vFileEntry, vDataStream);
        vDataStream.Position := 0;
      except
        FreeAndNil(vDataStream);
        raise;
      end;

      vName := StringReplace(string(vFileEntry.Name), '\', '/', [rfReplaceAll]);
      FPackageFile.Add(THCZLibPackageFile.Create(vName, vDataStream, vDataStream.Size));
    end;
  finally
    FreeAndNil(vZLib);
  end;
end;

function THCDocxReader.NextResFileId: string;
begin
  Inc(FResFileId);
  Result := IntToStr(FResFileId);
end;

procedure THCDocxReader.ReadComments_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;
end;

procedure THCDocxReader.ReadCore_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
  vXmlNode: IHCXMLNode;
  i: Integer;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_CoreProperties then Exit;

  for i := 0 to vXml.DocumentElement.ChildNodes.Count - 1 do
  begin
    vXmlNode := vXml.DocumentElement.ChildNodes[i];
    if vXmlNode.NodeName = 'dc:creator' then
      FCreator := vXmlNode.Text
    else
    if vXmlNode.NodeName = 'cp:lastModifiedBy' then
      FLastModifiedBy := vXmlNode.Text
    else
    if vXmlNode.NodeName = 'dcterms:created' then
      FCreatedDT := vXmlNode.Text
    else
    if vXmlNode.NodeName = 'dcterms:modified' then
      FModifiedDT := vXmlNode.Text
  end;
end;

procedure THCDocxReader.ReadData_(const AData: THCViewData;
  const ANode: IHCXMLNode; const AStyle: THCStyle);
var
  i: Integer;
  vNode: IHCXMLNode;
begin
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    vNode := ANode.ChildNodes[i];
    if vNode.NodeName = 'w:p' then  // paragraph
      ReadParagraph_(AData, vNode, AStyle)
    else
    if vNode.NodeName = 'w:tbl' then  // table
      ReadTable_(AData, True, vNode, AStyle);
  end;
end;

procedure THCDocxReader.ReadDocument_(const AFile: string);
var
  vData: THCViewData;
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
  vBodyNode, vXmlNode: IHCXMLNode;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;

  vData := FHCView.ActiveSection.PageData;

  vBodyNode := vXml.DocumentElement.ChildNodes.FindNode('w:body');
  if not Assigned(vBodyNode) then Exit;

  FHCView.BeginUpdate;
  try
    vXmlNode := vBodyNode.ChildNodes.FindNode('w:sectPr');  // 页面属性
    if Assigned(vXmlNode) then
    begin
      vXmlNode := vXmlNode.ChildNodes.FindNode('w:pgMar');
      FHCView.Sections[0].PaperSize := DMPAPER_A4;
      FHCView.Sections[0].PaperWidth := 210;
      FHCView.Sections[0].PaperHeight := 297;

      FHCView.Sections[0].PaperMarginTop := TwipToMillimeter(Single(vXmlNode.Attributes['w:top']));
      FHCView.Sections[0].PaperMarginLeft := TwipToMillimeter(Single(vXmlNode.Attributes['w:left']));
      FHCView.Sections[0].PaperMarginRight := TwipToMillimeter(Single(vXmlNode.Attributes['w:right']));
      FHCView.Sections[0].PaperMarginBottom := TwipToMillimeter(Single(vXmlNode.Attributes['w:bottom']));

      FHCView.Sections[0].PageOrientation := TPageOrientation.cpoPortrait;  // TPageOrientation.cpoLandscape;
      FHCView.Sections[0].ResetMargin;
    end;

    ReadData_(vData, vBodyNode, FHCView.Style);
  finally
    FHCView.EndUpdate;
  end;
end;

procedure THCDocxReader.ReadDrawing_(const AData: THCViewData;
  const AParaFirst: Boolean; const ANode: IHCXMLNode; const AParaNo: Integer);
var
  vInLineNode, vGraphicNode, vGraphicDataNode,
  vPicNode, vPicblipFillNode, vBlipNode, vNode: IHCXMLNode;
  vImageResID, vFile: string;
  vImageItem: THCImageItem;
  vGifItem: THCGifItem;
  vZLibPackageFile: THCZLibPackageFile;
  vW, vH: Integer;
  {$IFDEF BMPIMAGEITEM}
  vWICImage: TWICImage;
  vStream: TMemoryStream;
  {$ENDIF}
begin
  vInLineNode := FindChildNodeByName(ANode, 'wp:inline');
  if not Assigned(vInLineNode) then Exit;

  vGraphicNode := FindChildNodeByName(vInLineNode, 'a:graphic');
  if not Assigned(vGraphicNode) then Exit;

  vGraphicDataNode := FindChildNodeByName(vGraphicNode, 'a:graphicData');
  if not Assigned(vGraphicDataNode) then Exit;

  vPicNode := FindChildNodeByName(vGraphicDataNode, 'pic:pic');
  if not Assigned(vPicNode) then Exit;

  vPicblipFillNode := FindChildNodeByName(vPicNode, 'pic:blipFill');
  if not Assigned(vPicblipFillNode) then Exit;

  vBlipNode := FindChildNodeByName(vPicblipFillNode, 'a:blip');
  if not Assigned(vBlipNode) then Exit;
  vImageResID := vBlipNode.Attributes['r:embed'];

  if vImageResID <> '' then
  begin
    // 取图片
    vFile := FindResFileById(vImageResID).Target;
    vZLibPackageFile := GetZLibPackageFile(vFile);
    // 设置大小
    vNode := FindChildNodeByName(vInLineNode, 'wp:extent');
    if not Assigned(vNode) then Exit;

    vW := vNode.Attributes['cx'];
    vH := vNode.Attributes['cy'];
    vFile := LowerCase(TPath.GetExtension(vFile));
    if vFile = '.gif' then
    begin
      vGifItem := THCGifItem.Create(AData,
        TwipToPixel(MillimeterToTwip(vW / 36000.0), PixelsPerInchX),
        TwipToPixel(MillimeterToTwip(vH / 36000.0), PixelsPerInchY));
      vGifItem.ParaFirst := AParaFirst;
      vGifItem.ParaNo := AParaNo;
      vGifItem.Animate := True;

      vZLibPackageFile.Stream.Position := 0;
      vGifItem.Image.LoadFromStream(vZLibPackageFile.Stream);
      vZLibPackageFile.Stream.Position := 0;

      AData.Items.Add(vGifItem);
    end
    else
    if (vFile = '.bmp') or (vFile = '.jpeg') or (vFile = '.jpg') or (vFile = '.png') then
    begin
      vImageItem := THCImageItem.Create(AData,
        TwipToPixel(MillimeterToTwip(vW / 36000.0), PixelsPerInchX),
        TwipToPixel(MillimeterToTwip(vH / 36000.0), PixelsPerInchY));
      vImageItem.ParaFirst := AParaFirst;
      vImageItem.ParaNo := AParaNo;

      {$IFDEF BMPIMAGEITEM}
      if vFile <> '.bmp' then
      begin
        vWICImage := TWICImage.Create;
        try
          vZLibPackageFile.Stream.Position := 0;
          vWICImage.LoadFromStream(vZLibPackageFile.Stream);
          vZLibPackageFile.Stream.Position := 0;
          vWICImage.Transparent := True;
          vWICImage.ImageFormat := TWICImageFormat.wifBmp;
          vStream := TMemoryStream.Create;
          try
            vWICImage.SaveToStream(vStream);
            vStream.Position := 0;
            vImageItem.Image.LoadFromStream(vStream);
            vImageItem.Image.Transparent := True;
            //vImageItem.Image.TransparentColor := clBlack;
          finally
            FreeAndNil(vStream);
          end;
        finally
          FreeAndNil(vWICImage);
        end;
      end
      else
      begin
        vZLibPackageFile.Stream.Position := 0;
        vImageItem.Image.LoadFromStream(vZLibPackageFile.Stream);
        vZLibPackageFile.Stream.Position := 0;
      end;
      {$ELSE}
        vZLibPackageFile.Stream.Position := 0;
        vImageItem.Image.LoadFromStream(vZLibPackageFile.Stream);
        vZLibPackageFile.Stream.Position := 0;
      {$ENDIF}

      vImageItem.RestrainSize(AData.Width, vImageItem.Height);
      AData.Items.Add(vImageItem);
    end;
  end;
end;

procedure THCDocxReader.ReadEndNotes_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;
end;

procedure THCDocxReader.ReadExtended_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
  vXmlNode: IHCXMLNode;
  i: Integer;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_ExtendedProperties then Exit;

  for i := 0 to vXml.DocumentElement.ChildNodes.Count - 1 do
  begin
    vXmlNode := vXml.DocumentElement.ChildNodes[i];
    if vXmlNode.NodeName = 'Application' then
      FApplication := vXmlNode.Text
    else
    if vXmlNode.NodeName = 'AppVersion' then
      FAppVersion := vXmlNode.Text;    
  end;
end;

procedure THCDocxReader.ReadFontTable_(const AFile: string);
begin

end;

procedure THCDocxReader.ReadFooter_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;

  ReadData_(FHCView.ActiveSection.Footer, vXml.DocumentElement, FHCView.Style);
end;

procedure THCDocxReader.ReadFootNotes_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;
end;

procedure THCDocxReader.ReadHeader_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;

  ReadData_(FHCView.Sections[0].Header, vXml.DocumentElement, FHCView.Style);
end;

procedure THCDocxReader.ReadNumbering_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;
end;

procedure THCDocxReader.ReadParagraph_(const AData: THCViewData;
  const AParaNode: IHCXMLNode; const AStyle: THCStyle; const AFirstTime: Boolean = True);
var
  vParaStyleLink: string;

  {$REGION '取段样式编号'}
  function GetHCParaNo: Integer;
  var
    vParaStyle: THCDocxParaStyle;
    vParaPrNode, vNode: IHCXMLNode;
    i: Integer;
  begin
    Result := THCStyle.Null;

    vParaStyle := THCDocxParaStyle.Create;
    try
      vParaStyle.CopyFrom(FindParaStyleDefault);

      vParaPrNode := AParaNode.ChildNodes.FindNode('w:pPr');
      if Assigned(vParaPrNode) then  // 有自己的段属性
      begin
        for i := 0 to vParaPrNode.ChildNodes.Count - 1 do
        begin
          vNode := vParaPrNode.ChildNodes[i];
          if vNode.NodeName = 'w:pStyle' then
            vParaStyle.CopyFrom(FindParaStyleById(vNode.Attributes['w:val']))
          else
          if vNode.NodeName = 'w:jc' then
            vParaStyle.SetParaAlignment(vNode.Attributes['w:val']);
        end;
      end;

      Result := FindHCParaNo(vParaStyle);
      vParaStyleLink := vParaStyle.Link;
    finally
      FreeAndNil(vParaStyle);
    end;
  end;
  {$ENDREGION}

var
  i, j, vParaNo, vStyleNo: Integer;
  vWRNode, vRPRNode, vNode: IHCXMLNode;
  vParaFirst: Boolean;
  vParaStyle: THCDocxParaStyle;
  vTextStyle: THCDocxTextStyle;
begin
  vParaFirst := AFirstTime;
  vParaNo := THCStyle.Null;
  vParaStyleLink := '';

  if AParaNode.ChildNodes.Count = 0 then  // 空段
  begin
    if vParaNo < 0 then
      vParaNo := GetHCParaNo;

    vTextStyle := THCDocxTextStyle.Create;
    try
      vParaStyle := FindParaStyleDefault;
      if Assigned(vParaStyle.TextStyle) then
        vTextStyle.CopyFrom(vParaStyle.TextStyle)
      else
        vTextStyle.CopyFrom(FindTextStyleDefault);

      vStyleNo := FindHCStyleNo(vTextStyle);
    finally
      FreeAndNil(vTextStyle);
    end;

    ReadTextItem_(AData, vParaFirst, AParaNode, vParaNo, vStyleNo);
    Exit;
  end;

  for i := 0 to AParaNode.ChildNodes.Count - 1 do
  begin
    vWRNode := AParaNode.ChildNodes[i];
    if vWRNode.NodeName = 'w:r' then
    begin
      for j := 0 to vWRNode.ChildNodes.Count - 1 do
      begin
        vNode := vWRNode.ChildNodes[j];
        if (vNode.NodeName = 'w:t') and (vParaFirst or (vNode.Text <> '')) then
        begin
          if vParaNo < 0 then
            vParaNo := GetHCParaNo;

          vTextStyle := THCDocxTextStyle.Create;
          try
            if vParaStyleLink = '' then
            begin
              vParaStyle := FindParaStyleDefault;
              if Assigned(vParaStyle.TextStyle) then
                vTextStyle.CopyFrom(vParaStyle.TextStyle)
              else
                vTextStyle.CopyFrom(FindTextStyleDefault);
            end
            else
              vTextStyle.CopyFrom(FindTextStyleById(vParaStyleLink));

            vRPRNode := vWRNode.ChildNodes.FindNode('w:rPr');
            if Assigned(vRPRNode) then
              ReadTextStyleProperty(vTextStyle, vRPRNode);

            vStyleNo := FindHCStyleNo(vTextStyle);
          finally
            FreeAndNil(vTextStyle);
          end;

          ReadTextItem_(AData, vParaFirst, vNode, vParaNo, vStyleNo);
          vParaFirst := False;
        end
        else
        if vNode.NodeName = 'w:drawing' then
        begin
          if vParaNo < 0 then
            vParaNo := GetHCParaNo;

          ReadDrawing_(AData, vParaFirst, vNode, vParaNo);
          vParaFirst := False;
        end;
      end;
    end
    else
    if vWRNode.NodeName = 'w:hyperlink' then
      ReadParagraph_(AData, vWRNode, AStyle, False);
  end;
end;

procedure THCDocxReader.ReadParaStyleProperty(
  const AParaStyle: THCDocxParaStyle; const APrNode: IHCXMLNode);
var
  i: Integer;
  vNode: IHCXMLNode;
begin
  AParaStyle.Alignment := TParaAlignHorz.pahJustify;
  AParaStyle.VerticalAlignment := TParaAlignVert.pavBottom;

  for i := 0 to APrNode.ChildNodes.Count - 1 do
  begin
    vNode := APrNode.ChildNodes[i];

    if vNode.NodeName = 'w:widowControl' then
      AParaStyle.WidowOrphanControl := StrToBoolDef(GetAttributeAsString(vNode, 'w:val'), True)
    else
    if vNode.NodeName = 'w:jc' then
    begin
      if vNode.HasAttribute('w:val') then
        AParaStyle.SetParaAlignment(vNode.Attributes['w:val']);
    end
    else
    if vNode.NodeName = 'w:textAlignment' then
    begin
      if vNode.HasAttribute('w:val') then
        AParaStyle.SetParaVerticalAlignment(vNode.Attributes['w:val']);
    end
    else
    if vNode.NodeName = 'w:keepNext' then
    begin
      if vNode.HasAttribute('w:val') then
        AParaStyle.KeepWithNext := vNode.Attributes['w:val'];
    end
    else
    if vNode.NodeName = 'w:keepLines' then
    begin
      if vNode.HasAttribute('w:val') then
        AParaStyle.KeepLinesTogether := vNode.Attributes['w:val'];
    end
    else
    if vNode.NodeName = 'w:suppressLineNumbers' then
    begin
      if vNode.HasAttribute('w:val') then
        AParaStyle.SuppressLineNumbers := vNode.Attributes['w:val'];
    end;
  end;
end;

procedure THCDocxReader.ReadSettings_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
  vXmlNode: IHCXMLNode;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;

  vXmlNode := vXml.DocumentElement.ChildNodes.FindNode('w:defaultTabStop');
  FDocxProperty.DefaultTabStop := vXmlNode.Attributes['w:val'];
end;

procedure THCDocxReader.ReadStyles_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
  vXmlNode, vXmlNode2, vXmlNode3, vXmlNode4: IHCXMLNode;
  vTextStyle, vDocDefaultTextStyle: THCDocxTextStyle;
  vParaStyle, vDocDefaultParaStyle: THCDocxParaStyle;
  vTableStyle: THCDocxTableStyle;
  i, j: Integer;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;

  for i := 0 to vXml.DocumentElement.ChildNodes.Count - 1 do
  begin
    vXmlNode := vXml.DocumentElement.ChildNodes[i];
    if vXmlNode.NodeName = 'w:docDefaults' then  // 读取默认文本、段样式
    begin
      vXmlNode2 := vXmlNode.ChildNodes.FindNode('w:rPrDefault');
      if Assigned(vXmlNode2) then  // 文档默认文本样式
      begin
        vXmlNode2 := vXmlNode2.ChildNodes.FindNode('w:rPr');
        if Assigned(vXmlNode2) then
        begin
          vDocDefaultTextStyle := THCDocxTextStyle.Create;
          ReadTextStyleProperty(vDocDefaultTextStyle, vXmlNode2);
        end;
      end;

      // 段样式
      vXmlNode2 := vXmlNode.ChildNodes.FindNode('w:pPrDefault');
      if Assigned(vXmlNode2) then
      begin
        vXmlNode2 := vXmlNode2.ChildNodes.FindNode('w:pPr');
        if Assigned(vXmlNode2) then
        begin
          vDocDefaultParaStyle := THCDocxParaStyle.Create;
          ReadParaStyleProperty(vDocDefaultParaStyle, vXmlNode2);
        end;
      end;
    end
    else
    if vXmlNode.NodeName = 'w:style' then
    begin
      if vXmlNode.Attributes['w:type'] = 'paragraph' then
      begin
        vParaStyle := THCDocxParaStyle.Create;
        if Assigned(vDocDefaultParaStyle) then
          vParaStyle.CopyFrom(vDocDefaultParaStyle);

        vParaStyle.Default := StrToBoolDef(GetAttributeAsString(vXmlNode, 'w:default'), False);
        vParaStyle.StyleID := GetAttributeAsString(vXmlNode, 'w:styleId');

        for j := 0 to vXmlNode.ChildNodes.Count - 1 do
        begin
          vXmlNode2 := vXmlNode.ChildNodes[j];
          if vXmlNode2.NodeName = 'w:name' then
            vParaStyle.StyleName := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:uiPriority' then
            vParaStyle.UIPriority := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:semiHidden' then
            vParaStyle.SemiHidden := StrToBoolDef(GetAttributeAsString(vXmlNode2, 'w:semiHidden'), False)
          else
          if vXmlNode2.NodeName = 'w:basedOn' then
            vParaStyle.BaseOn := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:link' then
            vParaStyle.Link := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:rsid' then
            vParaStyle.Rsid := vXmlNode2.Attributes['w:val']  // 以上为通用属性
          else
          if vXmlNode2.NodeName = 'w:qFormat' then
            vParaStyle.QFormat := StrToBoolDef(GetAttributeAsString(vXmlNode2, 'w:val'), True)
          else
          if vXmlNode2.NodeName = 'w:pPr' then
            ReadParaStyleProperty(vParaStyle, vXmlNode2)
          else
          if vXmlNode2.NodeName = 'w:rPr' then
          begin
            vParaStyle.TextStyle := THCDocxTextStyle.Create;

            if Assigned(vDocDefaultTextStyle) then
              vParaStyle.TextStyle.CopyFrom(vDocDefaultTextStyle);
            ReadTextStyleProperty(vParaStyle.TextStyle, vXmlNode2);
          end;
        end;

        FParaStyles.Add(vParaStyle);
      end
      else
      if vXmlNode.Attributes['w:type'] = 'character' then
      begin
        vTextStyle := THCDocxTextStyle.Create;
        if Assigned(vDocDefaultTextStyle) then
          vTextStyle.CopyFrom(vDocDefaultTextStyle);
        vTextStyle.Default := StrToBoolDef(GetAttributeAsString(vXmlNode, 'w:default'), False);
        vTextStyle.StyleID := GetAttributeAsString(vXmlNode, 'w:styleId');

        for j := 0 to vXmlNode.ChildNodes.Count - 1 do
        begin
          vXmlNode2 := vXmlNode.ChildNodes[j];
          if vXmlNode2.NodeName = 'w:name' then
            vTextStyle.StyleName := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:uiPriority' then
            vTextStyle.UIPriority := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:semiHidden' then
            vTextStyle.SemiHidden := StrToBoolDef(GetAttributeAsString(vXmlNode2, 'w:semiHidden'), False)
          else
          if vXmlNode2.NodeName = 'w:basedOn' then
            vTextStyle.BaseOn := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:link' then
            vTextStyle.Link := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:rsid' then
            vTextStyle.Rsid := vXmlNode2.Attributes['w:val']  // 以上为通用属性
          else
          if vXmlNode2.NodeName = 'w:rPr' then
            ReadTextStyleProperty(vTextStyle, vXmlNode2);
        end;

        FTextStyles.Add(vTextStyle);
      end
      else
      if vXmlNode.Attributes['w:type'] = 'table' then
      begin
        vTableStyle := THCDocxTableStyle.Create;
        vTableStyle.Default := StrToBoolDef(GetAttributeAsString(vXmlNode, 'w:default'), False);
        vTableStyle.StyleID := GetAttributeAsString(vXmlNode, 'w:styleId');

        for j := 0 to vXmlNode.ChildNodes.Count - 1 do
        begin
          vXmlNode2 := vXmlNode.ChildNodes[j];
          if vXmlNode2.NodeName = 'w:name' then
            vParaStyle.StyleName := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:uiPriority' then
            vParaStyle.UIPriority := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:semiHidden' then
            vParaStyle.SemiHidden := StrToBoolDef(GetAttributeAsString(vXmlNode2, 'w:semiHidden'), False)
          else
          if vXmlNode2.NodeName = 'w:basedOn' then
            vParaStyle.BaseOn := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:link' then
            vParaStyle.Link := vXmlNode2.Attributes['w:val']
          else
          if vXmlNode2.NodeName = 'w:rsid' then
            vParaStyle.Rsid := vXmlNode2.Attributes['w:val']  // 以上为通用属性
          else
          if vXmlNode2.NodeName = 'w:tblPr' then  // 封装为 ReadTableStyleProperty(vTableStyle, vXmlNode2);
          begin
            vXmlNode3 := vXmlNode2.ChildNodes.FindNode('w:tblInd');
            if vXmlNode3 <> nil then
            begin
              vTableStyle.Ind_W := vXmlNode3.Attributes['w:w'];
              vTableStyle.Ind_Type := vXmlNode3.Attributes['w:type'];
            end;

            vXmlNode3 := vXmlNode2.ChildNodes.FindNode('w:tblCellMar');
            if vXmlNode3 <> nil then
            begin
              vXmlNode4 := vXmlNode3.ChildNodes.FindNode('w:left');
              vTableStyle.CellMargin_Left := vXmlNode4.Attributes['w:w'];
              vTableStyle.CellMargin_LeftType := vXmlNode4.Attributes['w:type'];

              vXmlNode4 := vXmlNode3.ChildNodes.FindNode('w:top');
              vTableStyle.CellMargin_Top := vXmlNode4.Attributes['w:w'];
              vTableStyle.CellMargin_TopType := vXmlNode4.Attributes['w:type'];

              vXmlNode4 := vXmlNode3.ChildNodes.FindNode('w:right');
              vTableStyle.CellMargin_Right := vXmlNode4.Attributes['w:w'];
              vTableStyle.CellMargin_RightType := vXmlNode4.Attributes['w:type'];

              vXmlNode4 := vXmlNode3.ChildNodes.FindNode('w:bottom');
              vTableStyle.CellMargin_Bottom := vXmlNode4.Attributes['w:w'];
              vTableStyle.CellMargin_BottomType := vXmlNode4.Attributes['w:type'];
            end;
          end;
        end;

        FTableStyles.Add(vTableStyle);
      end
      else
      if vXmlNode.Attributes['w:type'] = 'numbering' then
      begin

      end;
    end;
  end;

  if Assigned(vDocDefaultTextStyle) then
    FreeAndNil(vDocDefaultTextStyle);

  if Assigned(vDocDefaultParaStyle) then
    FreeAndNil(vDocDefaultParaStyle);
end;

procedure THCDocxReader.ReadStyleWithEffects_(const AFile: string);
begin

end;

procedure THCDocxReader.ReadTable_(const AData: THCViewData;
  const AParaFirst: Boolean; const ATableNode: IHCXMLNode; const AStyle: THCStyle);
var
  vTable: THCDocxTable;
  vTableItem: THCTableItem;
begin
  vTable := THCDocxTable.Create(ATableNode, AStyle, ReadData_);
  try
    vTableItem := vTable.ConvertToHCTable(AData);
    vTableItem.ParaFirst := AParaFirst;
    AData.Items.Add(vTableItem);  // InsertItem(AData.Items.Count, vTableItem);
  finally
    FreeAndNil(vTable);
  end;
end;

procedure THCDocxReader.ReadTextItem_(const AData: THCViewData;
 const AParaFirst: Boolean; const ANode: IHCXMLNode; const AParaNo, AStyleNo: Integer);
var
  vTextItem: THCCustomItem;
begin
  if AParaFirst or (ANode.Text <> '') then
  begin
    vTextItem := AData.CreateDefaultTextItem;
    vTextItem.Text := ANode.Text;
    vTextItem.ParaFirst := AParaFirst;
    vTextItem.ParaNo := AParaNo;
    vTextItem.StyleNo := AStyleNo;

    if (not AParaFirst) and AData.Items.Last.CanConcatItems(vTextItem) then
    begin
      AData.Items.Last.Text := AData.Items.Last.Text + ANode.Text;
      FreeAndNil(vTextItem);
    end
    else
    begin
      if AData.IsEmptyData then
        AData.Items.Clear;

      AData.Items.Add(vTextItem);
    end;
  end;
end;

procedure THCDocxReader.ReadTextStyleProperty(
  const ATextStyle: THCDocxTextStyle; const APrNode: IHCXMLNode);
var
  i: Integer;
  vNode: IHCXMLNode;
begin
  for i := 0 to APrNode.ChildNodes.Count - 1 do
  begin
    vNode := APrNode.ChildNodes[i];

    if vNode.NodeName = 'w:rStyle' then
      ATextStyle.CopyFrom(FindTextStyleById(vNode.Attributes['w:val']))
    else
    if vNode.NodeName = 'w:rFonts' then
    begin
      if vNode.HasAttribute('w:ascii') then  // 如果没有，需要取w:eastAsia或w:cs
        ATextStyle.FontName := vNode.Attributes['w:ascii'];
    end
    else
    if vNode.NodeName = 'w:sz' then
      ATextStyle.DoubleFontSize := vNode.Attributes['w:val']
    else
    if vNode.NodeName = 'w:highlight' then
      ATextStyle.BackColor := GetOpenXmlNodeColor(vNode.Attributes['w:val'])
    else
    if vNode.NodeName = 'w:color' then
      ATextStyle.ForeColor := GetOpenXmlNodeColor(vNode.Attributes['w:val'])
    else
    if vNode.NodeName = 'w:strike' then
    begin
      if vNode.HasAttribute('w:val') then
        ATextStyle.StrikeoutWordsOnly := vNode.Attributes['w:val']
      else
        ATextStyle.StrikeoutWordsOnly := True;
    end
    else
    if vNode.NodeName = 'w:u' then
    begin
      if vNode.HasAttribute('w:val') then
        ATextStyle.UnderlineWordsOnly := vNode.Attributes['w:val'] <> 'none'
      else
        ATextStyle.UnderlineWordsOnly := True;
    end
    else
    if vNode.NodeName = 'w:b' then
    begin
      if vNode.HasAttribute('w:val') then
        ATextStyle.FontBold := vNode.Attributes['w:val']
      else
        ATextStyle.FontBold := True;
    end
    else
    if vNode.NodeName = 'w:i' then
    begin
      if vNode.HasAttribute('w:val') then
        ATextStyle.FontItalic := vNode.Attributes['w:val']
      else
        ATextStyle.FontItalic := True;
    end
    else
    if vNode.NodeName = 'w:vertAlign' then
    begin
      if vNode.Attributes['w:val'] = 'superscript' then
        ATextStyle.Script := THCDocxScriptStyle.dssSuperscript
      else
      if vNode.Attributes['w:val'] = 'subscript' then
        ATextStyle.Script := THCDocxScriptStyle.dssSubscript;
    end;
  end;
end;

procedure THCDocxReader.ReadWebSettings_(const AFile: string);
var
  vPackageFile: THCZLibPackageFile;
  vXml: IHCXMLDocument;
begin
  vPackageFile := GetZLibPackageFile(AFile);
  if not Assigned(vPackageFile) then
    raise Exception.Create(HCDOCXERROR_ENCRYPTED);

  vXml := THCXMLDocument.Create(nil);
  vXml.LoadFromStream(vPackageFile.Stream);
  if vXml.DocumentElement.NamespaceURI <> HCDOCX_Main then Exit;
end;

procedure THCDocxReader.SaveToStream(const AHCView: THCView; const AStream: TStream);
var
  vZLib: THCZLib;
  //vWordPackFile, vDocPropsPackFile: TObjectList<THCZLibPackageFile>;
  vPackageFile, vDocumentPackageFile: THCZLibPackageFile;
  i: Integer;
begin
  FHCView := AHCView;
  FResFileId := 0;
  FHeaderID := '';
  FFooterID := '';
  FPackageFile.Clear;

  vZLib := THCZLib.Create;
  try
    vZLib.SetFileStream(AStream);

    //vWordPackFile.Add(WriteNumbering_);
    //vWordPackFile.Add(WriteFontTable_);
    FPackageFile.Add(WriteSettings_);  // settings.xml
    //vWordPackFile.Add(WriteWebSettings_);
    //vWordPackFile.Add(WriteComments_);
    FPackageFile.Add(WriteStyles_);  // styles.xml
    //vWordPackFile.Add(WriteStyleWithEffects_);
    //vWordPackFile.Add(WriteFootNotes_);
    //vWordPackFile.Add(WriteEndNotes_);

    vPackageFile := WriteHeader1_;  // header1.xml
    if Assigned(vPackageFile) then
    begin
      FPackageFile.Add(vPackageFile);
      FHeaderID := 'rId' + IntToStr(FPackageFile.Count);
    end;

    vPackageFile := WriteFooter1_;  // footer1.xml
    if Assigned(vPackageFile) then
    begin
      FPackageFile.Add(vPackageFile);
      FFooterID := 'rId' + IntToStr(FPackageFile.Count);
    end;

    vDocumentPackageFile := WriteDocument_;

    vPackageFile := WriteDocumentXmlRels_(FPackageFile, 'word\_rels\document.xml.rels');
    try
      vZLib.AppendFile(vPackageFile.FileName, vPackageFile.Stream);
    finally
      FreeAndNil(vPackageFile);
    end;

    for i := 0 to FResFiles.Count - 1 do  // 写资源文件
      vZLib.AppendFile(FResFiles[i].Target, FResFiles[i].Stream);

    FPackageFile.Add(vDocumentPackageFile);  // document.xml
    FPackageFile.Add(WriteContentTypes_(FPackageFile));  // [Content_Types].xml
    for i := 0 to FPackageFile.Count - 1 do
    begin
      vPackageFile := FPackageFile[i];
      vZLib.AppendFile(vPackageFile.FileName, vPackageFile.Stream);
    end;

    vPackageFile := WriteRootRelations_;  // _rels/.rels
    try
      vZLib.AppendFile(vPackageFile.FileName, vPackageFile.Stream);
    finally
      FreeAndNil(vPackageFile);
    end;

    vZLib.FinishFile;
  finally
    FreeAndNil(vZLib);
  end;

  FPackageFile.Clear;
end;

function THCDocxReader.WriteComments_: THCZLibPackageFile;
begin
  Result.&Type := HCDOCX_Comments;
  Result.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml';
end;

function THCDocxReader.WriteContentTypes_(const APackageFile: TObjectList<THCZLibPackageFile>): THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vXmlNode: IHCXMLNode;
  vStream: TMemoryStream;
  i: Integer;
  vExtList: TStringList;
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.AddChild('Types', 'http://schemas.openxmlformats.org/package/2006/content-types');
  vXmlNode := vXml.DocumentElement.AddChild('Default');
  vXmlNode.Attributes['Extension'] := 'rels';
  vXmlNode.Attributes['ContentType'] := 'application/vnd.openxmlformats-package.relationships+xml';

  vExtList := TStringList.Create;
  try
    for i := 0 to FResFiles.Count - 1 do
    begin
      if vExtList.IndexOf(FResFiles[i].Extension) < 0 then
        vExtList.Add(FResFiles[i].Extension)
      else
        Continue;

      if FResFiles[i].Extension = 'bmp' then
      begin
        vXmlNode := vXml.DocumentElement.AddChild('Default');
        vXmlNode.Attributes['Extension'] := 'bmp';
        vXmlNode.Attributes['ContentType'] := 'image/bitmap';
      end
      else
      if (FResFiles[i].Extension = 'jpeg') or (FResFiles[i].Extension = 'jpg') then
      begin
        vXmlNode := vXml.DocumentElement.AddChild('Default');
        vXmlNode.Attributes['Extension'] := 'jpeg';
        vXmlNode.Attributes['ContentType'] := 'image/jpeg';
      end
      else
      if FResFiles[i].Extension = 'png' then
      begin
        vXmlNode := vXml.DocumentElement.AddChild('Default');
        vXmlNode.Attributes['Extension'] := 'png';
        vXmlNode.Attributes['ContentType'] := 'image/png';
      end
      else
      if FResFiles[i].Extension = 'gif' then
      begin
        vXmlNode := vXml.DocumentElement.AddChild('Default');
        vXmlNode.Attributes['Extension'] := 'gif';
        vXmlNode.Attributes['ContentType'] := 'image/gif';
      end;
    end;
  finally
    FreeAndNil(vExtList);
  end;

  for i := 0 to APackageFile.Count - 1 do
  begin
    vXmlNode := vXml.DocumentElement.AddChild('Override');
    vXmlNode.Attributes['PartName'] := '/' + StringReplace(APackageFile[i].FileName, '\', '/', [rfReplaceAll]);
    vXmlNode.Attributes['ContentType'] := APackageFile[i].ContentType;
  end;

  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create('[Content_Types].xml', vStream, vStream.Size);
end;

function THCDocxReader.WriteCore_: THCZLibPackageFile;
begin

end;

procedure THCDocxReader.WriteData_(const AData: THCViewData;
  const ANode: IHCXMLNode);
var
  i: Integer;
  vItem: THCCustomItem;
  vParaNode, vWRNode, vNode: IHCXMLNode;
begin
  for i := 0 to AData.Items.Count - 1 do
  begin
    vItem := AData.Items[i];
    if vItem.ParaFirst and (vItem.StyleNo <> THCStyle.Table) then  // docx里表格是段首
      vParaNode := ANode.AddChild('w:p');
    if (vItem.StyleNo > THCStyle.Null) and (vItem.Text <> '') then
    begin
      vWRNode := vParaNode.AddChild('w:r');
      if vItem.StyleNo > 0 then
      begin
        vNode := vWRNode.AddChild('w:rPr');
        vNode.AddChild('w:rStyle').Attributes['w:val'] := 'C' + IntToStr(vItem.StyleNo);
      end;
      vNode := vWRNode.AddChild('w:t');
      vNode.Text := vItem.Text;
    end
    else
    if vItem.StyleNo = THCStyle.Image then
      WriteDrawingImage_(vParaNode, vItem as THCImageItem)
    else
    if vItem.StyleNo = THCStyle.Gif then
      WriteDrawingGif_(vParaNode, vItem as THCGifItem)
    else
    if vItem.StyleNo = THCStyle.Table then
      WriteTable_(ANode, vItem as THCTableItem);
  end;
end;

function THCDocxReader.WriteDocumentXmlRels_(const APackageFile: TObjectList<THCZLibPackageFile>;
  const AFileName: string): THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vXmlNode: IHCXMLNode;
  vStream: TMemoryStream;
  vTarget: string;
  i: Integer;
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.AddChild('Relationships', HCDOCX_Relationships);

  for i := 0 to FResFiles.Count - 1 do
  begin
    vXmlNode := vXml.DocumentElement.AddChild('Relationship');
    vXmlNode.Attributes['Id'] := FResFiles[i].ID;
    vXmlNode.Attributes['Type'] := FResFiles[i].&Type;
    vXmlNode.Attributes['Target'] := FResFiles[i].Target;
  end;

  for i := 0 to APackageFile.Count - 1 do
  begin
    vXmlNode := vXml.DocumentElement.AddChild('Relationship');
    vXmlNode.Attributes['Id'] := 'rId' + IntToStr(i + 1);
    vXmlNode.Attributes['Type'] := APackageFile[i].&Type;
    vTarget := StringReplace(APackageFile[i].FileName, 'word\', '', [rfReplaceAll]);
    vXmlNode.Attributes['Target'] := StringReplace(vTarget, '\', '/', [rfReplaceAll]);
  end;

  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create(AFileName, vStream, vStream.Size);
end;

function THCDocxReader.WriteRootRelations_: THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vXmlNode: IHCXMLNode;
  vStream: TMemoryStream;
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.AddChild('Relationships', HCDOCX_Relationships);

  vXmlNode := vXml.DocumentElement.AddChild('Relationship');
  vXmlNode.Attributes['Id'] := 'rId1';
  vXmlNode.Attributes['Type'] := HCDOCX_OfficeDocument;
  vXmlNode.Attributes['Target'] := '/word/document.xml';

  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create('_rels/.rels', vStream, vStream.Size);
end;

function THCDocxReader.WriteDocument_: THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vXmlNode, vXmlNode2, vXmlNode3: IHCXMLNode;
  vStream: TMemoryStream;
  vData: THCViewData;
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.CreateNode('w:document');
  vXml.DocumentElement.Attributes['xmlns:w'] := HCDOCX_Main;
  //vXml.DocumentElement.Attributes['xmlns:wpg'] := 'http://schemas.microsoft.com/office/word/2010/wordprocessingGroup';
  //vXml.DocumentElement.Attributes['xmlns:wpi'] := 'http://schemas.microsoft.com/office/word/2010/wordprocessingInk';
  vXml.DocumentElement.Attributes['xmlns:wps'] := HCDOCX_NSwps;
  //vXml.DocumentElement.Attributes['xmlns:w10'] := 'urn:schemas-microsoft-com:office:word';
  vXml.DocumentElement.Attributes['xmlns:wp14'] := HCDOCX_NSwp14;
  //vXml.DocumentElement.Attributes['xmlns:v'] := 'urn:schemas-microsoft-com:vml';
  vXml.DocumentElement.Attributes['xmlns:mc'] := HCDOCX_NSmc;
  vXml.DocumentElement.Attributes['mc:Ignorable'] := 'wp14';

  vXmlNode := vXml.DocumentElement.AddChild('w:body');

  vData := FHCView.Sections[0].PageData;
  if not vData.IsEmptyData then
    WriteData_(vData, vXmlNode);
  // w:sectPr
  vXmlNode2 := vXmlNode.AddChild('w:sectPr');
  if FHeaderID <> '' then
  begin
    vXmlNode3 := vXmlNode2.AddChild('w:headerReference');
    vXmlNode3.Attributes['xmlns:r'] := HCDOCX_DocumentRelationships;
    vXmlNode3.Attributes['w:type'] := 'default';
    vXmlNode3.Attributes['r:id'] := FHeaderID;
  end;

  if FFooterID <> '' then
  begin
    vXmlNode3 := vXmlNode2.AddChild('w:footerReference');
    vXmlNode3.Attributes['xmlns:r'] := HCDOCX_DocumentRelationships;
    vXmlNode3.Attributes['w:type'] := 'default';
    vXmlNode3.Attributes['r:id'] := FFooterID;
  end;

  vXmlNode3 := vXmlNode2.AddChild('w:pgSz');
  vXmlNode3.Attributes['w:w'] := '11906';
  vXmlNode3.Attributes['w:h'] := '16838';
  vXmlNode3 := vXmlNode2.AddChild('w:pgMar');
  vXmlNode3.Attributes['w:left'] := '1800';
  vXmlNode3.Attributes['w:top'] := '1440';
  vXmlNode3.Attributes['w:right'] := '1800';
  vXmlNode3.Attributes['w:bottom'] := '1440';
  vXmlNode3.Attributes['w:header'] := '851';
  vXmlNode3.Attributes['w:footer'] := '992';
  //
  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create('word\document.xml', vStream, vStream.Size);
  Result.&Type := HCDOCX_OfficeDocument;
  Result.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml';
end;

procedure THCDocxReader.WriteDrawingGif_(const ANode: IHCXMLNode;
  const AGifItem: THCGifItem);
var
  vWPNode, vGraphicNode, vGraphicDataNode, vPicNode, vPicPrNode,
  vblipFillNode, vblipNode, vspPrNode, vNode: IHCXMLNode;
  vW, vH: Cardinal;
  vResFileId: string;
  vResFile: THCDocxResFile;
begin
  vWPNode := ANode.AddChild('w:r').AddChild('w:drawing').AddChild('wp:inline');
  vWPNode.Attributes['xmlns:wp'] := 'http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing';

  vW := Round(TwipToMillimeter(PixelToTwip(AGifItem.Width, PixelsPerInchX) * 36000));
  vH := Round(TwipToMillimeter(PixelToTwip(AGifItem.Height, PixelsPerInchY) * 36000));
  vNode := vWPNode.AddChild('wp:extent');
  vNode.Attributes['cx'] := vW;
  vNode.Attributes['cy'] := vH;

  vResFileId := NextResFileId;
  vNode := vWPNode.AddChild('wp:docPr');
  vNode.Attributes['id'] := 1;
  vNode.Attributes['name'] := 'Picture ' + vResFileId;

  vGraphicNode := vWPNode.AddChild('a:graphic');
  vGraphicNode.Attributes['xmlns:a'] := 'http://schemas.openxmlformats.org/drawingml/2006/main';

  vGraphicDataNode := vGraphicNode.AddChild('a:graphicData');
  vGraphicDataNode.Attributes['uri'] := 'http://schemas.openxmlformats.org/drawingml/2006/picture';

  vPicNode := vGraphicDataNode.AddChild('pic:pic');
  vPicNode.Attributes['xmlns:pic'] := 'http://schemas.openxmlformats.org/drawingml/2006/picture';

  vPicPrNode := vPicNode.AddChild('pic:nvPicPr');
  vNode := vPicPrNode.AddChild('pic:cNvPr');
  vNode.Attributes['id'] := 1;
  vNode.Attributes['name'] := 'Picture ' + vResFileId;
  vNode := vPicPrNode.AddChild('pic:cNvPicPr');

  vblipFillNode := vPicNode.AddChild('pic:blipFill');

  vblipNode := vblipFillNode.AddChild('a:blip');
  vblipNode.Attributes['xmlns:r'] := HCDOCX_DocumentRelationships;
  vResFileId := 'image' + vResFileId;
  vblipNode.Attributes['r:embed'] := vResFileId;

  vblipFillNode.AddChild('a:stretch').AddChild('a:fillRect');  //

  vspPrNode := vPicNode.AddChild('pic:spPr');
  vNode := vspPrNode.AddChild('a:xfrm');
  vblipNode := vNode.AddChild('a:off');  //
  vblipNode.Attributes['x'] := 0;
  vblipNode.Attributes['y'] := 0;
  vblipNode := vNode.AddChild('a:ext');
  vblipNode.Attributes['cx'] := vW;
  vblipNode.Attributes['cy'] := vH;
  vNode := vspPrNode.AddChild('a:prstGeom');
  vNode.Attributes['prst'] := 'rect';
  vNode := vspPrNode.AddChild('a:noFill');

  vResFile := THCDocxResFile.Create;
  vResFile.ID := vResFileId;
  vResFile.&Type := HCDOCX_IMAGE;
  vResFile.Extension := 'gif';

  vResFile.Target := '/media/' + vResFileId + '.' + vResFile.Extension;

  vResFile.Stream := TMemoryStream.Create;
  AGifItem.Image.SaveToStream(vResFile.Stream);
  vResFile.Stream.Position := 0;

  FResFiles.Add(vResFile);
end;

procedure THCDocxReader.WriteDrawingImage_(const ANode: IHCXMLNode;
  const AImageItem: THCImageItem);
var
  vWPNode, vGraphicNode, vGraphicDataNode, vPicNode, vPicPrNode,
  vblipFillNode, vblipNode, vspPrNode, vNode: IHCXMLNode;
  vW, vH: Cardinal;
  vResFileId: string;
  vResFile: THCDocxResFile;
begin
  vWPNode := ANode.AddChild('w:r').AddChild('w:drawing').AddChild('wp:inline');
  vWPNode.Attributes['xmlns:wp'] := 'http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing';

  vW := Round(TwipToMillimeter(PixelToTwip(AImageItem.Width, PixelsPerInchX) * 36000));
  vH := Round(TwipToMillimeter(PixelToTwip(AImageItem.Height, PixelsPerInchY) * 36000));
  vNode := vWPNode.AddChild('wp:extent');
  vNode.Attributes['cx'] := vW;
  vNode.Attributes['cy'] := vH;

  vResFileId := NextResFileId;
  vNode := vWPNode.AddChild('wp:docPr');
  vNode.Attributes['id'] := 1;
  vNode.Attributes['name'] := 'Picture ' + vResFileId;

  vGraphicNode := vWPNode.AddChild('a:graphic');
  vGraphicNode.Attributes['xmlns:a'] := 'http://schemas.openxmlformats.org/drawingml/2006/main';

  vGraphicDataNode := vGraphicNode.AddChild('a:graphicData');
  vGraphicDataNode.Attributes['uri'] := 'http://schemas.openxmlformats.org/drawingml/2006/picture';

  vPicNode := vGraphicDataNode.AddChild('pic:pic');
  vPicNode.Attributes['xmlns:pic'] := 'http://schemas.openxmlformats.org/drawingml/2006/picture';

  vPicPrNode := vPicNode.AddChild('pic:nvPicPr');
  vNode := vPicPrNode.AddChild('pic:cNvPr');
  vNode.Attributes['id'] := 1;
  vNode.Attributes['name'] := 'Picture ' + vResFileId;
  vNode := vPicPrNode.AddChild('pic:cNvPicPr');

  vblipFillNode := vPicNode.AddChild('pic:blipFill');

  vblipNode := vblipFillNode.AddChild('a:blip');
  vblipNode.Attributes['xmlns:r'] := HCDOCX_DocumentRelationships;
  vResFileId := 'image' + vResFileId;
  vblipNode.Attributes['r:embed'] := vResFileId;

  vblipFillNode.AddChild('a:stretch').AddChild('a:fillRect');  //

  vspPrNode := vPicNode.AddChild('pic:spPr');
  vNode := vspPrNode.AddChild('a:xfrm');
  vblipNode := vNode.AddChild('a:off');  //
  vblipNode.Attributes['x'] := 0;
  vblipNode.Attributes['y'] := 0;
  vblipNode := vNode.AddChild('a:ext');
  vblipNode.Attributes['cx'] := vW;
  vblipNode.Attributes['cy'] := vH;
  vNode := vspPrNode.AddChild('a:prstGeom');
  vNode.Attributes['prst'] := 'rect';
  vNode := vspPrNode.AddChild('a:noFill');

  vResFile := THCDocxResFile.Create;
  vResFile.ID := vResFileId;
  vResFile.&Type := HCDOCX_IMAGE;
  {$IFDEF BMPIMAGEITEM}
  vResFile.Extension := 'bmp';
  {$ELSE}
  case AImageItem.Image.ImageFormat of
    wifBmp: vResFile.Extension := 'bmp';
    wifPng: vResFile.Extension := 'png';
    wifJpeg: vResFile.Extension := 'jpeg';
    wifGif: vResFile.Extension := 'gif';
    //wifTiff: ;
    //wifWMPhoto: ;
    //wifOther: ;
  end;
  {$ENDIF}

  vResFile.Target := '/media/' + vResFileId + '.' + vResFile.Extension;

  vResFile.Stream := TMemoryStream.Create;
  AImageItem.Image.SaveToStream(vResFile.Stream);
  vResFile.Stream.Position := 0;

  FResFiles.Add(vResFile);
end;

function THCDocxReader.WriteEndNotes_: THCZLibPackageFile;
begin
  Result.&Type := HCDOCX_EndNotes;
  Result.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml';
end;

function THCDocxReader.WriteExtended_: THCZLibPackageFile;
begin

end;

function THCDocxReader.WriteFontTable_: THCZLibPackageFile;
begin
  Result.&Type := HCDOCX_FontTable;
  Result.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml';
end;

function THCDocxReader.WriteFooter1_: THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vStream: TMemoryStream;
begin
  Result := nil;

  if FHCView.Sections[0].Footer.IsEmptyData then Exit;

  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.CreateNode('w:ftr');
  vXml.DocumentElement.Attributes['xmlns:w'] := HCDOCX_Main;
  vXml.DocumentElement.Attributes['xmlns:wps'] := HCDOCX_NSwps;
  vXml.DocumentElement.Attributes['xmlns:wp14'] := HCDOCX_NSwp14;
  vXml.DocumentElement.Attributes['xmlns:mc'] := HCDOCX_NSmc;
  vXml.DocumentElement.Attributes['mc:Ignorable'] := 'wp14';

  WriteData_(FHCView.Sections[0].Footer, vXml.DocumentElement);

  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create('word\footer1.xml', vStream, vStream.Size);
  Result.&Type := HCDOCX_Footer;
  Result.ContentType := HCDOCX_FooterContentType;
end;

function THCDocxReader.WriteFootNotes_: THCZLibPackageFile;
begin
  Result.&Type := HCDOCX_FootNotes;
  Result.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml';
end;

function THCDocxReader.WriteHeader1_: THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vStream: TMemoryStream;
begin
  Result := nil;

  if FHCView.Sections[0].Header.IsEmptyData then Exit;

  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.CreateNode('w:hdr');
  vXml.DocumentElement.Attributes['xmlns:w'] := HCDOCX_Main;
  vXml.DocumentElement.Attributes['xmlns:wps'] := HCDOCX_NSwps;
  vXml.DocumentElement.Attributes['xmlns:mc'] := HCDOCX_NSmc;
  vXml.DocumentElement.Attributes['xmlns:wp14'] := HCDOCX_NSwp14;
  vXml.DocumentElement.Attributes['mc:Ignorable'] := 'wp14';

  WriteData_(FHCView.Sections[0].Header, vXml.DocumentElement);

  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create('word\header1.xml', vStream, vStream.Size);
  Result.&Type := HCDOCX_Header;
  Result.ContentType := HCDOCX_HeaderContentType;
end;

function THCDocxReader.WriteNumbering_: THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vStream: TMemoryStream;
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.CreateNode('w:numbering');
  vXml.DocumentElement.Attributes['xmlns:w'] := HCDOCX_Main;

  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create('word\numbering.xml', vStream, vStream.Size);
  Result.&Type := HCDOCX_Numbering;
  Result.ContentType := '';
end;

function THCDocxReader.WriteSettings_: THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vXmlNode: IHCXMLNode;
  vStream: TMemoryStream;
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.CreateNode('w:settings');
  vXml.DocumentElement.Attributes['xmlns:w'] := HCDOCX_Main;

  vXmlNode := vXml.DocumentElement.AddChild('w:defaultTabStop');
  vXmlNode.Attributes['w:val'] := 720;
  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create('word\settings.xml', vStream, vStream.Size);
  Result.&Type := HCDOCX_Settings;
  Result.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml';
end;

function THCDocxReader.WriteStyles_: THCZLibPackageFile;
var
  vXml: IHCXMLDocument;
  vXmlNode2, vXmlNode3, vXmlNode4, vXmlNode5: IHCXMLNode;
  vStream: TMemoryStream;
  i: Integer;
  vParaStyle: THCParaStyle;
  vTextStyle: THCTextStyle;
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.CreateNode('w:styles');
  vXml.DocumentElement.Attributes['xmlns:w'] := HCDOCX_Main;

  // w:style  可先把不使用的样式删除了
  for i := 0 to FHCView.Style.ParaStyles.Count - 1 do  // 段样式
  begin
    vParaStyle := FHCView.Style.ParaStyles[i];

    vXmlNode2 := vXml.DocumentElement.AddChild('w:style');
    vXmlNode2.Attributes['w:type'] := 'paragraph';
    vXmlNode2.Attributes['w:styleId'] := 'P' + IntToStr(i);
    if i = 0 then
      vXmlNode2.Attributes['w:default'] := '1'
    else
      vXmlNode2.AddChild('w:basedOn').Attributes['w:val'] := 'P';

    vXmlNode3 := vXmlNode2.AddChild('w:name');
    vXmlNode3.Attributes['w:val'] := 'P' + IntToStr(i);

    vXmlNode3 := vXmlNode2.AddChild('w:pPr');
    vXmlNode4 := vXmlNode3.AddChild('w:jc');
    case vParaStyle.AlignHorz of
      pahLeft: vXmlNode4.Attributes['w:val'] := 'left';
      pahRight: vXmlNode4.Attributes['w:val'] := 'right';
      pahCenter: vXmlNode4.Attributes['w:val'] := 'center';
      pahJustify: vXmlNode4.Attributes['w:val'] := 'both';
      pahScatter: vXmlNode4.Attributes['w:val'] := 'both';
    end;

    vXmlNode4 := vXmlNode3.AddChild('w:textAlignment');
    case vParaStyle.AlignVert of
      pavTop: vXmlNode4.Attributes['w:val'] := 'top';
      pavCenter: vXmlNode4.Attributes['w:val'] := 'center';
      pavBottom: vXmlNode4.Attributes['w:val'] := 'bottom';
    end;

    {vXmlNode3 := vXmlNode2.AddChild('w:rPr');
    vXmlNode4 := vXmlNode3.AddChild('w:sz');
    vXmlNode4.Attributes['w:val'] := '21';
    vXmlNode4 := vXmlNode3.AddChild('w:szCs');
    vXmlNode4.Attributes['w:val'] := '22';}
  end;

  // 文本样式基础
  {vXmlNode2 := vXml.DocumentElement.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'character';
  vXmlNode2.Attributes['w:styleId'] := 'C0';
  vXmlNode2.Attributes['w:default'] := '1';
  vXmlNode3 := vXmlNode2.AddChild('w:name');
  vXmlNode3.Attributes['w:val'] := 'Default Paragraph Font';
  vXmlNode3 := vXmlNode2.AddChild('w:semiHidden');
  vXmlNode3 := vXmlNode2.AddChild('w:unhideWhenUsed');
  vXmlNode3 := vXmlNode2.AddChild('w:rPr');
  WriteTextStyleProperty(FHCView.Style.TextStyles[0], vXmlNode3); }
   
  for i := 0 to FHCView.Style.TextStyles.Count - 1 do  // 文本样式
  begin
    vTextStyle := FHCView.Style.TextStyles[i];

    vXmlNode2 := vXml.DocumentElement.AddChild('w:style');
    vXmlNode2.Attributes['w:type'] := 'character';
    vXmlNode2.Attributes['w:styleId'] := 'C' + IntToStr(i);
    if i = 0 then
      vXmlNode2.Attributes['w:default'] := '1'
    else
      vXmlNode2.AddChild('w:basedOn').Attributes['w:val'] := 'C0';

    vXmlNode3 := vXmlNode2.AddChild('w:name');
    vXmlNode3.Attributes['w:val'] := 'C' + IntToStr(i);
    vXmlNode3 := vXmlNode2.AddChild('w:semiHidden');
    vXmlNode3 := vXmlNode2.AddChild('w:rPr');
    WriteTextStyleProperty(vTextStyle, vXmlNode3);
  end;

  {vXmlNode2 := vXmlNode.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'character';
  vXmlNode2.Attributes['w:styleId'] := 'L1';
  vXmlNode3 := vXmlNode2.AddChild('w:name');
  vXmlNode3.Attributes['w:val'] := 'Line Number';
  vXmlNode3 := vXmlNode2.AddChild('w:basedOn');
  vXmlNode3.Attributes['w:val'] := 'C0';
  vXmlNode3 := vXmlNode2.AddChild('w:semiHidden');
  vXmlNode3 := vXmlNode2.AddChild('w:rPr');
  //
  vXmlNode2 := vXmlNode.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'character';
  vXmlNode2.Attributes['w:styleId'] := 'C2';
  vXmlNode3 := vXmlNode2.AddChild('w:name');
  vXmlNode3.Attributes['w:val'] := 'Hyperlink';
  vXmlNode3 := vXmlNode2.AddChild('w:rPr');
  vXmlNode4 := vXmlNode3.AddChild('w:color');
  vXmlNode4.Attributes['w:val'] := '0000FF';
  vXmlNode4 := vXmlNode3.AddChild('w:u');
  vXmlNode4.Attributes['w:val'] := 'single';}
  
  // table style T0
  vXmlNode2 := vXml.DocumentElement.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'table';
  vXmlNode2.Attributes['w:styleId'] := 'T0';
  vXmlNode2.Attributes['w:default'] := '1';
  vXmlNode3 := vXmlNode2.AddChild('w:name');
  vXmlNode3.Attributes['w:val'] := 'Normal Table';
  vXmlNode3 := vXmlNode2.AddChild('w:tblPr');
  // 边距
  vXmlNode4 := vXmlNode3.AddChild('w:tblCellMar');
  vXmlNode5 := vXmlNode4.AddChild('w:top');
  vXmlNode5.Attributes['w:w'] := '0';
  vXmlNode5.Attributes['w:type'] := 'dxa';  // twentieths
  vXmlNode5 := vXmlNode4.AddChild('w:left');
  vXmlNode5.Attributes['w:w'] := '108';
  vXmlNode5.Attributes['w:type'] := 'dxa';  // twentieths
  vXmlNode5 := vXmlNode4.AddChild('w:bottom');
  vXmlNode5.Attributes['w:w'] := '0';
  vXmlNode5.Attributes['w:type'] := 'dxa';
  vXmlNode5 := vXmlNode4.AddChild('w:right');
  vXmlNode5.Attributes['w:w'] := '108';
  vXmlNode5.Attributes['w:type'] := 'dxa';

  // table style T1
  vXmlNode2 := vXml.DocumentElement.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'table';
  vXmlNode2.Attributes['w:styleId'] := 'T1';
  vXmlNode3 := vXmlNode2.AddChild('w:name');
  vXmlNode3.Attributes['w:val'] := 'Table Simple 1';
  vXmlNode3 := vXmlNode2.AddChild('w:basedOn');
  vXmlNode3.Attributes['w:val'] := 'T0';
  vXmlNode3 := vXmlNode2.AddChild('w:tblPr');
  // 边框
  vXmlNode4 := vXmlNode3.AddChild('w:tblBorders');
  vXmlNode5 := vXmlNode4.AddChild('w:left');
  vXmlNode5.Attributes['w:val'] := 'single';
  vXmlNode5.Attributes['w:sz'] := 4;
  vXmlNode5.Attributes['w:space'] := 0;  // w:shadow="0" w:frame="0"
  vXmlNode5.Attributes['w:color'] := '000000';

  vXmlNode5 := vXmlNode4.AddChild('w:top');
  vXmlNode5.Attributes['w:val'] := 'single';
  vXmlNode5.Attributes['w:sz'] := 4;
  vXmlNode5.Attributes['w:space'] := 0;
  vXmlNode5.Attributes['w:color'] := '000000';

  vXmlNode5 := vXmlNode4.AddChild('w:right');
  vXmlNode5.Attributes['w:val'] := 'single';
  vXmlNode5.Attributes['w:sz'] := 4;
  vXmlNode5.Attributes['w:space'] := 0;
  vXmlNode5.Attributes['w:color'] := '000000';

  vXmlNode5 := vXmlNode4.AddChild('w:bottom');
  vXmlNode5.Attributes['w:val'] := 'single';
  vXmlNode5.Attributes['w:sz'] := 4;
  vXmlNode5.Attributes['w:space'] := 0;
  vXmlNode5.Attributes['w:color'] := '000000';

  vXmlNode5 := vXmlNode4.AddChild('w:insideH');
  vXmlNode5.Attributes['w:val'] := 'single';
  vXmlNode5.Attributes['w:sz'] := 4;
  vXmlNode5.Attributes['w:space'] := 0;
  vXmlNode5.Attributes['w:color'] := '000000';

  vXmlNode5 := vXmlNode4.AddChild('w:insideV');
  vXmlNode5.Attributes['w:val'] := 'single';
  vXmlNode5.Attributes['w:sz'] := 4;
  vXmlNode5.Attributes['w:space'] := 0;
  vXmlNode5.Attributes['w:color'] := '000000';
  //
  vStream := TMemoryStream.Create;
  vXml.SaveToStream(vStream);
  vStream.Position := 0;
  Result := THCZLibPackageFile.Create('word\styles.xml', vStream, vStream.Size);
  Result.&Type := HCDOCX_Styles;
  Result.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml';
end;

function THCDocxReader.WriteStyleWithEffects_: THCZLibPackageFile;
begin
  Result.&Type := HCDOCX_StyleWithEffects;
  Result.ContentType := 'application/vnd.ms-word.stylesWithEffects+xml';
end;

procedure THCDocxReader.WriteTable_(const ANode: IHCXMLNode;
  const ATableItem: THCTableItem);
var
  vTBLNode, vPrNode, vNode, vRowNode, vCellNode: IHCXMLNode;
  vCell: THCTableCell;
  vR, vC, i, vW: Integer;
begin
  // 表格属性
  vTBLNode := ANode.AddChild('w:tbl');
  vPrNode := vTBLNode.AddChild('w:tblPr');
  vNode := vPrNode.AddChild('w:tblStyle');
  vNode.Attributes['w:val'] := 'T1';
  vNode := vPrNode.AddChild('w:tblW');
  vNode.Attributes['w:w'] := '0';
  vNode.Attributes['w:type'] := 'auto';

  for vR := 0 to ATableItem.RowCount - 1 do
  begin
    vRowNode := vTBLNode.AddChild('w:tr');
    for vC := 0 to ATableItem.ColCount - 1 do
    begin
      vCell := ATableItem.Cells[vR, vC];
      if vCell.ColSpan < 0 then
        Continue;

      vCellNode := vRowNode.AddChild('w:tc');  // 单元格
      vPrNode := vCellNode.AddChild('w:tcPr');  // 单元格属性
      vNode := vPrNode.AddChild('w:tcW');  // 宽度
      vNode.Attributes['w:type'] := 'dxa';     
      vW := ATableItem.ColWidth[vC]; 
      if vCell.ColSpan > 0 then
      begin
        vPrNode.AddChild('w:gridSpan').Attributes['w:val'] := vCell.ColSpan + 1;
        for i := vC + 1 to vC + vCell.ColSpan do
          vW := vW + ATableItem.ColWidth[i];
      end;
      vNode.Attributes['w:w'] := PixelToTwip(vW, PixelsPerInchX);
      // 行合并  
      if vCell.RowSpan > 0 then
        vPrNode.AddChild('w:vMerge').Attributes['w:val'] := 'restart'
      else
      if vCell.RowSpan < 0 then
      begin
        vPrNode.AddChild('w:vMerge').Attributes['w:val'] := 'continue';
        if ATableItem.Cells[vR + vCell.RowSpan, vC].ColSpan > 0 then
          vPrNode.AddChild('w:gridSpan').Attributes['w:val'] := ATableItem.Cells[vR + vCell.RowSpan, vC].ColSpan + 1;
      end;
      
      if vCell.RowSpan >= 0 then// 数据
        WriteData_(vCell.CellData, vCellNode)
      else
        vCellNode.AddChild('w:p');
    end;
  end;
end;

procedure THCDocxReader.WriteTextStyleProperty(const ATextStyle: THCTextStyle;
  const APrNode: IHCXMLNode);
var
  vNode: IHCXMLNode;
begin
  vNode := APrNode.AddChild('w:rFonts');
  vNode.Attributes['w:ascii'] := ATextStyle.Family;
  vNode.Attributes['w:eastAsia'] := ATextStyle.Family;
  vNode.Attributes['w:cs'] := ATextStyle.Family;
  vNode.Attributes['w:hAnsi'] := ATextStyle.Family;

  vNode := APrNode.AddChild('w:sz');
  vNode.Attributes['w:val'] := Round(ATextStyle.Size * 2);
  vNode := APrNode.AddChild('w:szCs');
  vNode.Attributes['w:val'] := Ceil(ATextStyle.Size) * 2;

  if ATextStyle.BackColor <> HCTransparentColor then
  begin
    vNode := APrNode.AddChild('w:highlight');
    vNode.Attributes['w:val'] := ConvertColorToOpenXml(ATextStyle.BackColor);
  end;

  vNode := APrNode.AddChild('w:color');
  vNode.Attributes['w:val'] := ConvertColorToOpenXml(ATextStyle.Color);

  if THCFontStyle.tsStrikeOut in ATextStyle.FontStyles then
    vNode := APrNode.AddChild('w:strike');

  if THCFontStyle.tsUnderline in ATextStyle.FontStyles then
    vNode := APrNode.AddChild('w:u');

  if THCFontStyle.tsBold in ATextStyle.FontStyles then
    vNode := APrNode.AddChild('w:b');

  if THCFontStyle.tsItalic in ATextStyle.FontStyles then
    vNode := APrNode.AddChild('w:i');

  if THCFontStyle.tsSuperscript in ATextStyle.FontStyles then
  begin
    vNode := APrNode.AddChild('w:vertAlign');
    vNode.Attributes['w:val'] := 'superscript';
  end
  else
  if THCFontStyle.tsSubscript in ATextStyle.FontStyles then
  begin
    vNode := APrNode.AddChild('w:vertAlign');
    vNode.Attributes['w:val'] := 'subscript';
  end;
end;

function THCDocxReader.WriteWebSettings_: THCZLibPackageFile;
begin
  Result.&Type := HCDOCX_WebSettings;
  Result.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml';
end;

{ THCZLibPackageFile }

constructor THCZLibPackageFile.Create(const AFileName: string; AStream: TStream;
  AStreamLength: Integer);
begin
  Assert(AFileName <> '');
  Assert(AStream <> nil);
  FFileName := AFileName;
  FStream := AStream;
end;

destructor THCZLibPackageFile.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

{ THCOpenXmlRelation }

constructor THCOpenXmlRelation.Create;
begin
  FId := '';
  FTarget := '';
  FType := '';
  FTargetMode := '';
end;

{ THCOpenXmlRelations }

function THCOpenXmlRelations.GenerateId: string;
begin
  Result := Format('rId%d', [Self.Count + 1]);
end;

function THCOpenXmlRelations.LookupRelationById(const AId: string): THCOpenXmlRelation;
var
  i: Integer;
begin
  Result := nil;
  if AId = '' then
    Exit;

  for i := 0 to Self.Count - 1 do
  begin
    if Items[i].Id = AId then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function THCOpenXmlRelations.LookupRelationByTargetAndType(const ATarget, AType: string): THCOpenXmlRelation;
var
  i: Integer;
begin
  Result := nil;
  if (ATarget = '') or (AType = '') then
    Exit;

  for i := 0 to Self.Count - 1 do
  begin
    if (Items[I].Target = ATarget) and (Items[I].&Type = AType) then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function THCOpenXmlRelations.LookupRelationByType(const AType: string): THCOpenXmlRelation;
var
  i: Integer;
begin
  Result := nil;
  if AType = '' then
    Exit;

  for i := 0 to Self.Count - 1 do
  begin
    if Items[i].&Type = AType then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

{ THCDocxRow }

constructor THCDocxRow.Create(const ANode: IHCXMLNode; const AStyle: THCStyle;
  const AOnReadData: TCellReadDataEvent);
var
  i: Integer;
  vCell: THCDocxCell;
begin
  Cells := TObjectList<THCDocxCell>.Create;
  Width := 0;

  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    if ANode.ChildNodes[i].NodeName = 'w:tc' then
    begin
      vCell := THCDocxCell.Create(ANode.ChildNodes[i], AStyle, AOnReadData);
      Width := Width + vCell.Width;
      Cells.Add(vCell);
    end;
  end;
end;

destructor THCDocxRow.Destroy;
begin
  FreeAndNil(Cells);
  inherited Destroy;
end;

{ THCDocxTable }

function THCDocxTable.ConvertToHCTable(const AData: THCViewData): THCTableItem;
var
  vR, vC, i, j, vDestR, vColCount, vMaxColCountRow, vTableWidth,
  vDelta: Integer;
  vCell: THCDocxCell;
  //vHCCellData, vDocxCellData: THCViewData;
begin
  vColCount := 0;  // 最多的列数
  vMaxColCountRow := 0;  // 列数最多的行
  // 转为HCTableItem的结构
  for vR := 0 to Rows.Count - 1 do  // 补全各行合并的空单元格
  begin
    if vColCount < Rows[vR].Cells.Count then
    begin
      vMaxColCountRow := vR;
      vColCount := Rows[vR].Cells.Count;
    end;

    for vC := Rows[vR].Cells.Count - 1 downto 0 do
    begin
      for i := Rows[vR].Cells[vC].ColSpan - 1 downto 0 do
      begin
        vCell := THCDocxCell.Create;
        vCell.ColSpan := -(i + 1);
        Rows[vR].Cells.Insert(vC + 1, vCell);
      end;

      if (Rows[vR].Cells[vC].ColSpan > 0) and (Rows[vR].Cells[vC].Merge = THCDocxCellMerge.cmContinue) then
        Rows[vR].Cells[vC].ColSpan := 0;
    end;
  end;

  for vR := 0 to Rows.Count - 1 do  // 处理有行合并的RowSpan值
  begin
    for vC := 0 to Rows[vR].Cells.Count - 1 do
    begin
      if Rows[vR].Cells[vC].Merge = THCDocxCellMerge.cmRestart then  // 行合并开始
      begin
        vDestR := Rows.Count - 1;
        for i := vR + 1 to Rows.Count - 1 do
        begin
          if Rows[i].Cells[vC].Merge <> THCDocxCellMerge.cmContinue then
          begin
            vDestR := i - 1;
            Break;
          end;
        end;
        Rows[vR].Cells[vC].RowSpan := vDestR - vR;

        for i := vR + 1 to vDestR do
        begin
          for j := vC to vC + Rows[vR].Cells[vC].ColSpan do
            Rows[i].Cells[j].RowSpan := -(i - vR);
        end;
      end;
    end;
  end;

  vTableWidth := 0;
  for i := 0 to Rows.Count - 1 do  // 找最宽的行做为Docx表格的宽度
  begin
    if vTableWidth < Rows[i].Width then
      vTableWidth := Rows[i].Width;
  end;

  Result := THCTableItem.Create(AData, Rows.Count, vColCount, AData.Width);
  vDestR := AData.Width - (vColCount + 1) * Result.BorderWidth - vTableWidth;  // 得到文档表格Data的总宽度 - Docx中表格的宽度
  vDelta := vDestR div vColCount;  // 宽度的偏差 平均到每列
  vDestR := vDestR mod vColCount;
  for i := 0 to vColCount - 2 do  // 前面的平均分
    Result.ColWidth[i] := Rows[vMaxColCountRow].Cells[i].Width + vDelta;
  Result.ColWidth[vColCount - 1] :=  // 最后一个除平均外再增加余数
    Rows[vMaxColCountRow].Cells[vColCount - 1].Width + vDelta + vDestR;

  for vR := 0 to Rows.Count - 1 do
  begin
    for vC := 0 to Rows[0].Cells.Count - 1 do
    begin
      Result.Cells[vR, vC].RowSpan := Rows[vR].Cells[vC].RowSpan;
      Result.Cells[vR, vC].ColSpan := Rows[vR].Cells[vC].ColSpan;
      Result.Cells[vR, vC].BackgroundColor := Rows[vR].Cells[vC].FillColor;
      Result.Cells[vR, vC].AlignVert := Rows[vR].Cells[vC].AlignVert;
      if (Result.Cells[vR, vC].RowSpan < 0) or (Result.Cells[vR, vC].ColSpan < 0) then
      begin
        Result.Cells[vR, vC].CellData.Free;
        Result.Cells[vR, vC].CellData := nil;
      end
      else
      if not Rows[vR].Cells[vC].ViewData.IsEmptyData then
      begin
        Result.Cells[vR, vC].CellData.Items.Clear;
        Result.Cells[vR, vC].CellData.AddData(Rows[vR].Cells[vC].ViewData);
      end;
    end;
  end;
end;

constructor THCDocxTable.Create(const ANode: IHCXMLNode; const AStyle: THCStyle;
  const AOnReadData: TCellReadDataEvent);
var
  i: Integer;
  vRow: THCDocxRow;
begin
  Rows := TObjectList<THCDocxRow>.Create;

  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    if ANode.ChildNodes[i].NodeName = 'w:tblPr' then
    begin
      // w:tblStyle 从FTableStyles中取表格样式
    end
    else
    if ANode.ChildNodes[i].NodeName = 'w:tr' then
    begin
      vRow := THCDocxRow.Create(ANode.ChildNodes[i], AStyle, AOnReadData);
      Rows.Add(vRow);
    end;
  end;
end;

destructor THCDocxTable.Destroy;
begin
  FreeAndNil(Rows);
  inherited Destroy;
end;

{ THCDocxCell }

constructor THCDocxCell.Create(const ANode: IHCXMLNode; const AStyle: THCStyle;
  const AOnReadData: TCellReadDataEvent);
var
  i: Integer;
  vNodePr, vNode: IHCXMLNode;
begin
  ViewData := THCViewData.Create(AStyle);
  Merge := cmNone;
  RowSpan := 0;
  ColSpan := 0;
  FillColor := HCTransparentColor;

  FOnReadData := AOnReadData;

  vNodePr := ANode.ChildNodes.FindNode('w:tcPr');
  if Assigned(vNodePr) then
  begin
    for i := 0 to vNodePr.ChildNodes.Count - 1 do
    begin
      vNode := vNodePr.ChildNodes[i];
      if vNode.NodeName = 'w:tcW' then
      begin
        if vNode.Attributes['w:type'] = 'dxa' then  // twentieths
          Width := TwipToPixel(Integer(vNode.Attributes['w:w']), PixelsPerInchX)
        else
          Width := vNode.Attributes['w:w'];

        ViewData.Width := Width;
      end
      else
      if vNode.NodeName = 'w:vMerge' then
      begin
        if vNode.Attributes['w:val'] = 'restart' then
          Merge := cmRestart
        else
        if vNode.Attributes['w:val'] = 'continue' then
          Merge := cmContinue;
      end
      else
      if vNode.NodeName = 'w:gridSpan' then
        ColSpan := vNode.Attributes['w:val'] - 1
      else
      if vNode.NodeName = 'w:shd' then  // 底纹/背景色
      begin
        if vNode.HasAttribute('w:fill') then
          FillColor := GetOpenXmlNodeColor(vNode.Attributes['w:fill']);
      end
      else
      if vNode.NodeName = '' then
        AlignVert := GetAlignVertByName(vNode.Attributes['w:val']);
    end;
  end;

  if Assigned(FOnReadData) then
    FOnReadData(ViewData, ANode, AStyle);
end;

constructor THCDocxCell.Create;
begin
  Merge := cmNone;
  RowSpan := 0;
  ColSpan := 0;
  FillColor := clNone;
end;

destructor THCDocxCell.Destroy;
begin
  FreeAndNil(ViewData);
  inherited Destroy;
end;

function THCDocxCell.GetAlignVertByName(const AName: string): THCAlignVert;
begin
  if AName = 'bottom' then
    Result := THCAlignVert.cavBottom
  else
  if AName = 'center' then
    Result := THCAlignVert.cavCenter
  else
    Result := THCAlignVert.cavTop;
end;

{ THCDocxParaStyle }

procedure THCDocxParaStyle.CopyFrom(const ASrc: THCDocxParaStyle);
begin
  StyleName := ASrc.StyleName;
  StyleID := ASrc.StyleID;
  Rsid := ASrc.Rsid;
  Link := ASrc.Link;
  BaseOn := ASrc.BaseOn;
  UIPriority := ASrc.UIPriority;
  SemiHidden := ASrc.SemiHidden;
  Default := ASrc.Default;
  //
  QFormat := ASrc.QFormat;
  AfterAutoSpacing := ASrc.AfterAutoSpacing;
  Alignment := ASrc.Alignment;
  BackColor := ASrc.BackColor;
  BeforeAutoSpacing := ASrc.BeforeAutoSpacing;
  ContextualSpacing := ASrc.ContextualSpacing;
  FirstLineIndent := ASrc.FirstLineIndent;
  FirstLineIndentType := ASrc.FirstLineIndentType;
  KeepLinesTogether := ASrc.KeepLinesTogether;
  KeepWithNext := ASrc.KeepWithNext;
  LeftIndent := ASrc.LeftIndent;
  LineSpacing := ASrc.LineSpacing;
  LineSpacingType := ASrc.LineSpacingType;
  OutlineLevel := ASrc.OutlineLevel;
  PageBreakBefore := ASrc.PageBreakBefore;
  RightIndent := ASrc.RightIndent;
  SpacingAfter := ASrc.SpacingAfter;
  SpacingBefore := ASrc.SpacingBefore;
  SuppressHyphenation := ASrc.SuppressHyphenation;
  SuppressLineNumbers := ASrc.SuppressLineNumbers;
  WidowOrphanControl := ASrc.WidowOrphanControl;

  LeftBorder := ASrc.LeftBorder;
  RightBorder := ASrc.RightBorder;
  TopBorder := ASrc.TopBorder;
  BottomBorder := ASrc.BottomBorder;
end;

constructor THCDocxParaStyle.Create;
begin
  TextStyle := nil;
end;

destructor THCDocxParaStyle.Destroy;
begin
  if Assigned(TextStyle) then
    FreeAndNil(TextStyle);

  inherited Destroy;
end;

procedure THCDocxParaStyle.SetParaAlignment(const AAlign: string);
begin
  if AAlign = 'both' then
    Alignment := TParaAlignHorz.pahJustify
  else
  if AAlign = 'center' then
    Alignment := TParaAlignHorz.pahCenter
  else
  if AAlign = 'right' then
    Alignment := TParaAlignHorz.pahRight
  else
    Alignment := TParaAlignHorz.pahLeft;
end;

procedure THCDocxParaStyle.SetParaVerticalAlignment(const AAlign: string);
begin
  if AAlign = 'top' then
    VerticalAlignment := TParaAlignVert.pavTop
  else
  if AAlign = 'center' then
    VerticalAlignment := TParaAlignVert.pavCenter
  else
    VerticalAlignment := TParaAlignVert.pavBottom;
end;

{ THCDocxTextStyle }

procedure THCDocxTextStyle.CopyFrom(const ASrc: THCDocxTextStyle);
begin
  StyleName := ASrc.StyleName;
  StyleID := ASrc.StyleID;
  Rsid := ASrc.Rsid;
  Link := ASrc.Link;
  BaseOn := ASrc.BaseOn;
  UIPriority := ASrc.UIPriority;
  SemiHidden := ASrc.SemiHidden;
  Default := ASrc.Default;
  //
  AllCaps := ASrc.AllCaps;
  BackColor := ASrc.BackColor;
  DoubleFontSize := ASrc.DoubleFontSize;
  FontBold := ASrc.FontBold;
  ForeColor := ASrc.ForeColor;
  FontItalic := ASrc.FontItalic;
  FontName := ASrc.FontName;
  //FontUnderlineType: TdxUnderlineType;
  //FontStrikeoutType: TdxStrikeoutType;
  Hidden := ASrc.Hidden;
  NoProof := ASrc.NoProof;
  Script := ASrc.Script;
  StrikeoutColor := ASrc.StrikeoutColor;
  StrikeoutWordsOnly := ASrc.StrikeoutWordsOnly;
  UnderlineColor := ASrc.UnderlineColor;
  UnderlineWordsOnly := ASrc.UnderlineWordsOnly;
end;

constructor THCDocxTextStyle.Create;
begin
  ForeColor := clBlack;
  BackColor := HCTransparentColor;
  FontName := HCDOCX_DEFAULTFONT;
  DoubleFontSize := 21;  // 10.5 * 2
end;

function THCDocxTextStyle.GetHCFontStyles: THCFontStyles;
begin
  Result := [];

  if FontBold then
    Result := Result + [THCFontStyle.tsBold];

  if FontItalic then
    Result := Result + [THCFontStyle.tsItalic];

  if StrikeoutWordsOnly then
    Result := Result + [THCFontStyle.tsStrikeOut];

  if UnderlineWordsOnly then
    Result := Result + [THCFontStyle.tsUnderline];

  if Script = THCDocxScriptStyle.dssSuperscript then
    Result := Result + [THCFontStyle.tsSuperscript]
  else
  if Script = THCDocxScriptStyle.dssSubscript then
    Result := Result + [THCFontStyle.tsSubscript];
end;

{ THCDocxResFile }

destructor THCDocxResFile.Destroy;
begin
  if Assigned(Stream) then
    FreeAndNil(Stream);
  inherited Destroy;
end;

end.
