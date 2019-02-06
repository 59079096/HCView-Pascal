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
  Winapi.Windows, System.Classes, Vcl.Graphics, HCView, HCCommon, HCXml, HCViewData,
  HCStyle, HCTextStyle, HCParaStyle, HCDocumentRW, System.Generics.Collections,
  HCItem, HCTableItem;

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
    BackColor: TColor;
    BeforeAutoSpacing: Boolean;
    ContextualSpacing: Boolean;
    FirstLineIndent: Integer;
    FirstLineIndentType: TParaFirstLineIndent;
    KeepLinesTogether: Boolean;
    KeepWithNext: Boolean;
    LeftIndent: Integer;
    LineSpacing: Single;
    LineSpacingType: TParaLineSpaceMode;
    OutlineLevel: Integer;
    PageBreakBefore: Boolean;
    RightIndent: Integer;
    SpacingAfter: Integer;
    SpacingBefore: Integer;
    SuppressHyphenation: Boolean;
    SuppressLineNumbers: Boolean;
    WidowOrphanControl: Boolean;

    LeftBorder: TDocxBorderInfo;
    RightBorder: TDocxBorderInfo;
    TopBorder: TDocxBorderInfo;
    BottomBorder: TDocxBorderInfo;
    procedure CopyFrom(const ASrc: THCDocxParaStyle);
    procedure SetParaAlignment(const AAlign: string);
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
  public
    ViewData: THCViewData;
    Width: Integer;
    RowSpan, ColSpan: Integer;
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
    //
    FPackageFile: TObjectList<THCZLibPackageFile>;
    FDocxProperty: THCDocxProperty;
    FTextStyles: TObjectList<THCDocxTextStyle>;
    FParaStyles: TObjectList<THCDocxParaStyle>;
    FTableStyles: TObjectList<THCDocxTableStyle>;
    FMediaFiles: TStringList;

    procedure LoadFromZLibStream(const AStream: TStream);
    function GetZLibPackageFile(const AFileName: string): THCZLibPackageFile;
    function GetRelationTargetByType(const ARelations: THCOpenXmlRelations; const AType: string): string;
    function ReadRelations(const AFileName: string): THCOpenXmlRelations;
    //
    function FindTextStyleDefault: THCDocxTextStyle;
    function FindTextStyleById(const AStyleId: string): THCDocxTextStyle;
    function FindParaStyleDefault: THCDocxParaStyle;
    function FindParaStyleById(const AStyleId: string): THCDocxParaStyle;
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
      const ANode: IHCXMLNode; const AStyle: THCStyle; const AParaNo, AStyleNo: Integer);
    procedure ReadTable_(const AData: THCViewData; const AParaFirst: Boolean;
      const ATableNode: IHCXMLNode; const AStyle: THCStyle);
    procedure ReadDrawing_(const AData: THCViewData; const AParaFirst: Boolean;
      const ANode: IHCXMLNode; const AStyle: THCStyle; const AParaNo: Integer);
    procedure ReadParagraph_(const AData: THCViewData; const AParaNode: IHCXMLNode; const AStyle: THCStyle);
    procedure ReadData_(const AData: THCViewData; const ANode: IHCXMLNode; const AStyle: THCStyle);
    //
    procedure WriteData_(const AData: THCViewData; const ANode: IHCXMLNode);
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

uses
  System.SysUtils, System.IOUtils, HCZLib, HCImageItem, HCGifItem;

const
  HCDOCX_DEFAULTFONT = 'Calibri';
  HCDOCX_Relationships = 'http://schemas.openxmlformats.org/package/2006/relationships';
  //HCDOCX_XMLNS = 'xmlns';
  HCDOCX_OfficeDocument = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument';
  HCDOCX_CoreProperties = 'http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties';
  HCDOCX_ExtendedProperties = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties';

  HCDOCX_Main = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main';
  HCDOCX_FontTable = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable';
  HCDOCX_Settings = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings';
  HCDOCX_WebSettings = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings';
  HCDOCX_Styles = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles';
  HCDOCX_Numbering = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering';
  HCDOCX_FootNotes = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes';
  HCDOCX_EndNotes = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes';
  HCDOCX_Comments = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments';
  HCDOCX_StyleWithEffects = 'http://schemas.microsoft.com/office/2007/relationships/stylesWithEffects';
  HCDOCX_Header = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/header';
  HCDOCX_Footer = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer';
  HCDOCX_IMAGE = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/image';

  HCDOCX_NSwps = 'http://schemas.microsoft.com/office/word/2010/wordprocessingShape';
  HCDOCX_NSwp14 = 'http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing';
  HCDOCX_NSmc = 'http://schemas.openxmlformats.org/markup-compatibility/2006';

  HCDOCX_HeaderContentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml';
  HCDOCX_FooterContentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml';

  DOCXERROR = 'HCDocx异常：';
  HCDOCXERROR_ENCRYPTED = DOCXERROR + '无法打开加密的docx文件！';

{ THCDocxReader }

function ConvertHtmlColor(const ARGB: Cardinal): TColor;
var
  vA, vR, vG, vB: Byte;
begin
  vA := ARGB shr 24;
  vR := Byte(ARGB shr 16);
  vG := Byte(ARGB shr 8);
  vB := Byte(ARGB);

  Result := vB shl 24 + vG shl 16 + vR;
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

function TwipToPixel(const AValue: Single; const ADpi: Single): Cardinal;
begin
  Result := Round(AValue * ADpi / 1440);
end;

function TwipToMillimeter(const AValue: Single): Single;
begin
  Result := AValue * 25.4 / 1440;
end;

function MillimeterToTwip(const AValue: Single): Single;
begin
  Result := AValue * 1440 / 25.4;
end;

constructor THCDocxReader.Create;
begin
  inherited Create;
  FPackageFile := TObjectList<THCZLibPackageFile>.Create;
  FDocxProperty := THCDocxProperty.Create;
  FTextStyles := TObjectList<THCDocxTextStyle>.Create;
  FParaStyles := TObjectList<THCDocxParaStyle>.Create;
  FTableStyles := TObjectList<THCDocxTableStyle>.Create;
  FMediaFiles := TStringList.Create;
end;

destructor THCDocxReader.Destroy;
begin
  FPackageFile.Free;
  FDocxProperty.Free;
  FTextStyles.Free;
  FParaStyles.Free;
  FTableStyles.Free;
  FMediaFiles.Free;
  inherited Destroy;
end;

function THCDocxReader.FindHCParaNo(const AParaStyle: THCDocxParaStyle): Integer;
var
  i: Integer;
  vHCParaStyle: THCParaStyle;
begin
  Result := THCStyle.Null;

  vHCParaStyle := THCParaStyle.Create;
  try
    vHCParaStyle.AlignHorz := AParaStyle.Alignment;
    vHCParaStyle.LineSpaceMode := AParaStyle.LineSpacingType;
    vHCParaStyle.FristIndent := AParaStyle.FirstLineIndent;
    vHCParaStyle.LeftIndent := AParaStyle.LeftIndent;
    vHCParaStyle.BackColor := AParaStyle.BackColor;

    Result := FHCView.Style.GetParaNo(vHCParaStyle, True);
  finally
    FreeAndNil(vHCParaStyle);
  end;
end;

function THCDocxReader.FindHCStyleNo(const ATextStyle: THCDocxTextStyle): Integer;
var
  i: Integer;
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
        FMediaFiles.Add(vRelation.Id + '=' + vRelation.Target);
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

      FHCView.Sections[0].PaperMarginTop := TwipToMillimeter(vXmlNode.Attributes['w:top']);
      FHCView.Sections[0].PaperMarginLeft := TwipToMillimeter(vXmlNode.Attributes['w:left']);
      FHCView.Sections[0].PaperMarginRight := TwipToMillimeter(vXmlNode.Attributes['w:right']);
      FHCView.Sections[0].PaperMarginBottom := TwipToMillimeter(vXmlNode.Attributes['w:bottom']);

      FHCView.Sections[0].PageOrientation := TPageOrientation.cpoPortrait;  // TPageOrientation.cpoLandscape;
      FHCView.Sections[0].ResetMargin;
    end;

    ReadData_(vData, vBodyNode, FHCView.Style);
  finally
    FHCView.EndUpdate;
  end;
end;

procedure THCDocxReader.ReadDrawing_(const AData: THCViewData;
  const AParaFirst: Boolean; const ANode: IHCXMLNode; const AStyle: THCStyle;
  const AParaNo: Integer);
var
  vInLineNode, vGraphicNode, vGraphicDataNode,
  vPicNode, vPicblipFillNode, vBlipNode, vNode: IHCXMLNode;
  vImageResID, vFile: string;
  vImageItem: THCImageItem;
  vGifItem: THCGifItem;
  vZLibPackageFile: THCZLibPackageFile;
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
    vFile := FMediaFiles.Values[vImageResID];
    vZLibPackageFile := GetZLibPackageFile(vFile);
    // 设置大小
    vNode := FindChildNodeByName(vInLineNode, 'wp:extent');
    if not Assigned(vNode) then Exit;

    if TPath.GetExtension(vFile) = '.bmp' then
    begin
      vImageItem := THCImageItem.Create(AData,
        Round(MillimeterToTwip(vNode.Attributes['cx'] / 36000.0)),
        Round(MillimeterToTwip(vNode.Attributes['cy'] / 36000.0)));
      vImageItem.ParaFirst := AParaFirst;
      vImageItem.ParaNo := AParaNo;

      vZLibPackageFile.Stream.Position := 0;
      vImageItem.Image.LoadFromStream(vZLibPackageFile.Stream);
      vZLibPackageFile.Stream.Position := 0;
      vImageItem.RestrainSize(AData.Width, vImageItem.Height);
      AData.Items.Add(vImageItem);
    end
    else
    begin
      vGifItem := THCGifItem.Create(AData,
        Round(MillimeterToTwip(vNode.Attributes['cx'] / 36000.0)),
        Round(MillimeterToTwip(vNode.Attributes['cy'] / 36000.0)));
      vGifItem.ParaFirst := AParaFirst;
      vGifItem.ParaNo := AParaNo;

      vZLibPackageFile.Stream.Position := 0;
      vGifItem.Image.LoadFromStream(vZLibPackageFile.Stream);
      vZLibPackageFile.Stream.Position := 0;
      AData.Items.Add(vGifItem);
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
  const AParaNode: IHCXMLNode; const AStyle: THCStyle);
var
  vParaStyle: THCDocxParaStyle;
  i, j, vParaNo, vStyleNo: Integer;
  vParaStyleLink: string;
  vWRNode, vParaPrNode, vRPRNode, vFontNode, vNode: IHCXMLNode;
  vParaFirst: Boolean;
  vTextStyle: THCDocxTextStyle;
begin
  vParaNo := THCStyle.Null;
  vParaStyleLink := '';
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

    vParaNo := FindHCParaNo(vParaStyle);
    vParaStyleLink := vParaStyle.Link;
  finally
    FreeAndNil(vParaStyle);
  end;

  vParaFirst := True;
  for i := 0 to AParaNode.ChildNodes.Count - 1 do
  begin
    vWRNode := AParaNode.ChildNodes[i];
    if vWRNode.NodeName = 'w:r' then
    begin
      for j := 0 to vWRNode.ChildNodes.Count - 1 do
      begin
        vNode := vWRNode.ChildNodes[j];
        if (vNode.NodeName = 'w:t') and (vNode.Text <> '') then
        begin
          vTextStyle := THCDocxTextStyle.Create;
          try
            if vParaStyleLink = '' then
              vTextStyle.CopyFrom(FindTextStyleDefault)
            else
              vTextStyle.CopyFrom(FindTextStyleById(vParaStyleLink));

            vRPRNode := vWRNode.ChildNodes.FindNode('w:rPr');
            if Assigned(vRPRNode) then
              ReadTextStyleProperty(vTextStyle, vRPRNode);

            vStyleNo := FindHCStyleNo(vTextStyle);
          finally
            FreeAndNil(vTextStyle);
          end;

          ReadTextItem_(AData, vParaFirst, vNode, AStyle, vParaNo, vStyleNo);
          vParaFirst := False;
        end
        else
        if vNode.NodeName = 'w:drawing' then
        begin
          ReadDrawing_(AData, vParaFirst, vNode, AStyle, vParaNo);
          vParaFirst := False;
        end;
      end;
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
  vTextStyle: THCDocxTextStyle;
  vParaStyle: THCDocxParaStyle;
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
      {vTextStyle := THCDocxTextStyle.Create;

      vXmlNode2 := vXml.DocumentElement.ChildNodes[i];
      vXmlNode2 := vXmlNode2.ChildNodes.FindNode('w:rPrDefault');
      vXmlNode2 := vXmlNode2.ChildNodes.FindNode('w:rPr');
      // 字体
      vXmlNode := vXmlNode2.ChildNodes.FindNode('w:rFonts');
      vXmlNode := vXmlNode.AttributeNodes.FindNode('w:ascii');
      if vXmlNode <> nil then
        vTextStyle.FontName := vXmlNode.Text
      else
        vTextStyle.FontName := HCDOCX_DEFAULTFONT;
      // 字号
      vXmlNode := vXmlNode2.ChildNodes.FindNode('w:sz');
      vTextStyle.DoubleFontSize := vXmlNode.Attributes['w:val'];
      FTextStyles.Add(vTextStyle);

      // 段样式
      vParaStyle := THCDocxParaStyle.Create;
      vXmlNode2 := vXmlNode2.ChildNodes.FindNode('w:pPrDefault');
      FParaStyles.Add(vParaStyle);}
    end
    else
    if vXmlNode.NodeName = 'w:style' then
    begin
      if vXmlNode.Attributes['w:type'] = 'paragraph' then
      begin
        vParaStyle := THCDocxParaStyle.Create;
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
          begin
            vXmlNode3 := vXmlNode2.ChildNodes.FindNode('w:widowControl');
            if vXmlNode3 <> nil then
              vParaStyle.WidowOrphanControl := StrToBoolDef(GetAttributeAsString(vXmlNode3, 'w:val'), True);

            vXmlNode3 := vXmlNode2.ChildNodes.FindNode('w:jc');
            if vXmlNode3 <> nil then
              vParaStyle.SetParaAlignment(vXmlNode3.Attributes['w:val']);
          end;
        end;

        FParaStyles.Add(vParaStyle);
      end
      else
      if vXmlNode.Attributes['w:type'] = 'character' then
      begin
        vTextStyle := THCDocxTextStyle.Create;
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
          if vXmlNode2.NodeName = 'w:tblPr' then
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
 const AParaFirst: Boolean; const ANode: IHCXMLNode; const AStyle: THCStyle;
 const AParaNo, AStyleNo: Integer);
var
  vTextItem: THCCustomItem;
begin
  if ANode.Text <> '' then
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

    if vNode.NodeName = 'w:rFonts' then
    begin
      if vNode.HasAttribute('w:ascii') then  // 如果没有，需要取w:eastAsia或w:cs
        ATextStyle.FontName := vNode.Attributes['w:ascii']
      else
        ATextStyle.FontName := '宋体';
    end
    else
    if vNode.NodeName = 'w:sz' then
      ATextStyle.DoubleFontSize := vNode.Attributes['w:val']
    else
    if vNode.NodeName = 'w:color' then
      ATextStyle.ForeColor := ConvertHtmlColor(StrToInt('$' + vNode.Attributes['w:val']))
    else
    if vNode.NodeName = 'w:strike' then
      ATextStyle.StrikeoutWordsOnly := True
    else
    if vNode.NodeName = 'w:u' then
      ATextStyle.UnderlineWordsOnly := True
    else
    if vNode.NodeName = 'w:b' then
      ATextStyle.FontBold := True
    else
    if vNode.NodeName = 'w:i' then
      ATextStyle.FontBold := True
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
  vPackageFile: THCZLibPackageFile;
  i: Integer;
begin
  FHCView := AHCView;
  FHeaderID := '';
  FFooterID := '';
  FPackageFile.Clear;

  vZLib := THCZLib.Create;
  try
    vZLib.SetFileStream(AStream);

    //vWordPackFile.Add(WriteNumbering_);
    //vWordPackFile.Add(WriteFontTable_);
    FPackageFile.Add(WriteSettings_);
    //vWordPackFile.Add(WriteWebSettings_);
    //vWordPackFile.Add(WriteComments_);
    FPackageFile.Add(WriteStyles_);
    //vWordPackFile.Add(WriteStyleWithEffects_);
    //vWordPackFile.Add(WriteFootNotes_);
    //vWordPackFile.Add(WriteEndNotes_);

    vPackageFile := WriteHeader1_;
    if Assigned(vPackageFile) then
    begin
      FPackageFile.Add(vPackageFile);
      FHeaderID := 'rId' + IntToStr(FPackageFile.Count);
    end;

    vPackageFile := WriteFooter1_;
    if Assigned(vPackageFile) then
    begin
      FPackageFile.Add(vPackageFile);
      FFooterID := 'rId' + IntToStr(FPackageFile.Count);
    end;

    vPackageFile := WriteDocumentXmlRels_(FPackageFile, 'word\_rels\document.xml.rels');
    try
      vZLib.AppendFile(vPackageFile.FileName, vPackageFile.Stream);
    finally
      FreeAndNil(vPackageFile);
    end;

    FPackageFile.Add(WriteDocument_);
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
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.AddChild('Types', 'http://schemas.openxmlformats.org/package/2006/content-types');
  vXmlNode := vXml.DocumentElement.AddChild('Default');
  vXmlNode.Attributes['Extension'] := 'rels';
  vXmlNode.Attributes['ContentType'] := 'application/vnd.openxmlformats-package.relationships+xml';

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
  vParaNode, vRowNode, vItemNode: IHCXMLNode;
begin
  for i := 0 to AData.Items.Count - 1 do
  begin
    vItem := AData.Items[i];
    if vItem.ParaFirst then
      vParaNode := ANode.AddChild('w:p');
    if vItem.StyleNo > THCStyle.Null then
    begin
      vRowNode := vParaNode.AddChild('w:r');
      vItemNode := vRowNode.AddChild('w:t');
      vItemNode.Text := vItem.Text;
    end
    else
    begin

    end;
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
    vXmlNode3.Attributes['xmlns:r'] := 'http://schemas.openxmlformats.org/officeDocument/2006/relationships';
    vXmlNode3.Attributes['w:type'] := 'default';
    vXmlNode3.Attributes['r:id'] := FHeaderID;
  end;

  if FFooterID <> '' then
  begin
    vXmlNode3 := vXmlNode2.AddChild('w:footerReference');
    vXmlNode3.Attributes['xmlns:r'] := 'http://schemas.openxmlformats.org/officeDocument/2006/relationships';
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
  vXmlNode, vXmlNode2, vXmlNode3, vXmlNode4, vXmlNode5: IHCXMLNode;
  vStream: TMemoryStream;
begin
  Result := nil;
  vXml := THCXMLDocument.Create(nil);
  vXml.Active := True;
  vXml.Version := '1.0';
  vXml.DocumentElement := vXml.CreateNode('w:styles');
  vXml.DocumentElement.Attributes['xmlns:w'] := HCDOCX_Main;

  vXmlNode := vXml.DocumentElement.AddChild('w:docDefaults');
  //w:rPrDefault
  vXmlNode2 := vXmlNode.AddChild('w:rPrDefault');
  vXmlNode3 := vXmlNode2.AddChild('w:rPr');
  // w:rFonts
  vXmlNode4 := vXmlNode3.AddChild('w:rFonts');
  vXmlNode4.Attributes['w:asciiTheme'] := 'minorHAnsi';
  vXmlNode4.Attributes['w:eastAsiaTheme'] := 'minorEastAsia';
  vXmlNode4.Attributes['w:hAnsiTheme'] := 'minorHAnsi';
  vXmlNode4.Attributes['w:cstheme'] := 'minorBidi';

  vXmlNode4 := vXmlNode3.AddChild('w:sz');
  vXmlNode4.Attributes['w:val'] := '21';
  vXmlNode4 := vXmlNode3.AddChild('w:szCs');
  vXmlNode4.Attributes['w:val'] := '22';

  //w:pPrDefault
  vXmlNode2 := vXmlNode.AddChild('w:pPrDefault');
  vXmlNode3 := vXmlNode2.AddChild('w:pPr');
  vXmlNode4 := vXmlNode3.AddChild('w:widowControl');
  vXmlNode4.Attributes['w:val'] := '1';
  vXmlNode4 := vXmlNode3.AddChild('w:jc');
  vXmlNode4.Attributes['w:val'] := 'left';

  // w:style
  vXmlNode2 := vXmlNode.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'paragraph';
  vXmlNode2.Attributes['w:styleId'] := 'P0';
  vXmlNode2.Attributes['w:default'] := '1';
  vXmlNode3 := vXmlNode2.AddChild('w:name');
  vXmlNode3.Attributes['w:val'] := 'Normal';
  vXmlNode3 := vXmlNode2.AddChild('w:pPr');
  vXmlNode3 := vXmlNode2.AddChild('w:rPr');
  //
  vXmlNode2 := vXmlNode.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'character';
  vXmlNode2.Attributes['w:styleId'] := 'C0';
  vXmlNode2.Attributes['w:default'] := '1';
  vXmlNode3 := vXmlNode2.AddChild('w:name');
  vXmlNode3.Attributes['w:val'] := 'Default Paragraph Font';
  vXmlNode3 := vXmlNode2.AddChild('w:semiHidden');
  vXmlNode3 := vXmlNode2.AddChild('w:rPr');
  //
  vXmlNode2 := vXmlNode.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'character';
  vXmlNode2.Attributes['w:styleId'] := 'C1';
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
  vXmlNode4.Attributes['w:val'] := 'single';
  //
  vXmlNode2 := vXmlNode.AddChild('w:style');
  vXmlNode2.Attributes['w:type'] := 'table';
  vXmlNode2.Attributes['w:styleId'] := 'T0';
  vXmlNode2.Attributes['w:default'] := '1';
  vXmlNode3 := vXmlNode2.AddChild('w:name');
  vXmlNode3.Attributes['w:val'] := 'Normal Table';
  vXmlNode3 := vXmlNode2.AddChild('w:tblPr');
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
          Width := TwipToPixel(vNode.Attributes['w:w'], AStyle.PixelsPerInchX)
        else
          Width := vNode.Attributes['w:w'];
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
        ColSpan := vNode.Attributes['w:val'] - 1;
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
end;

destructor THCDocxCell.Destroy;
begin
  FreeAndNil(ViewData);
  inherited Destroy;
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
  BackColor := clNone;
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

end.
