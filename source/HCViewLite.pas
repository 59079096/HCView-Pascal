unit HCViewLite;

interface

{$I HCView.inc}

uses
  Windows, Classes, SysUtils, Generics.Collections, Graphics, HCStyle, HCSection,
  HCCustomData, HCItem, HCCommon;

type
  THCViewLite = class(TObject)
  private
    { Private declarations }
    FStyle: THCStyle;
    FSections: TObjectList<THCSection>;
    FActiveSectionIndex: Integer;

    function NewDefaultSection: THCSection;
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const ALoadSectionProc: TLoadSectionProc);
    /// <summary> 获取当前节对象 </summary>
    function GetActiveSection: THCSection;
  protected
    procedure DoSectionPaintFooterAfter(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    /// <summary> 当有新Item创建时触发 </summary>
    /// <param name="AData">创建Item的Data</param>
    /// <param name="AStyleNo">要创建的Item样式</param>
    /// <returns>创建好的Item</returns>
    function DoSectionCreateStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem; virtual;
    procedure DoSaveMutMargin(const AStream: TStream); virtual;
    procedure DoLoadMutMargin(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); virtual;

    /// <summary> 保存文档前触发事件，便于订制特征数据 </summary>
    procedure DoSaveStreamBefor(const AStream: TStream); virtual;

    /// <summary> 保存文档后触发事件，便于订制特征数据 </summary>
    procedure DoSaveStreamAfter(const AStream: TStream); virtual;

    /// <summary> 读取文档前触发事件，便于确认订制特征数据 </summary>
    procedure DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word); virtual;

    /// <summary> 读取文档后触发事件，便于确认订制特征数据 </summary>
    procedure DoLoadStreamAfter(const AStream: TStream; const AFileVersion: Word); virtual;

    procedure DataLoadLiteStream(const AStream: TStream; const AProc: THCLoadProc);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    /// <summary> 删除不使用的文本样式 </summary>
    class procedure DeleteUnUsedStyle(const AStyle: THCStyle;
      const ASections: TObjectList<THCSection>; const AAreas: TSectionAreas = [saHeader, saPage, saFooter]);
    /// <summary> 全部清空(清除各节页眉、页脚、页面的Item及DrawItem) </summary>
    procedure Clear; virtual;
    /// <summary> 返回总页数 </summary>
    function GetPageCount: Integer;
    /// <summary> 获取指定页所在的节和相对此节的页序号 </summary>
    /// <param name="APageIndex">页序号</param>
    /// <param name="ASectionPageIndex">返回节第一页相对所有页的序号</param>
    /// <returns>返回页序号所在的节序号</returns>
    function GetSectionPageIndexByPageIndex(const APageIndex: Integer; var ASectionPageIndex: Integer): Integer;
    // 保存文档
    /// <summary> 文档保存为hcf格式 </summary>
    procedure SaveToFile(const AFileName: string; const AQuick: Boolean = False);

    /// <summary> 读取hcf文件 </summary>
    function LoadFromFile(const AFileName: string): Boolean;

    /// <summary> 读取其他格式的文件 </summary>
    procedure LoadFromDocumentFile(const AFileName: string; const AExt: string);

    /// <summary> 另存为其他格式的文件 </summary>
    procedure SaveToDocumentFile(const AFileName: string; const AExt: string);

    /// <summary> 读取其他格式的文件流 </summary>
    procedure LoadFromDocumentStream(const AStream: TStream; const AExt: string);

    /// <summary> 文档保存为PDF格式 </summary>
    procedure SaveToPDF(const AFileName: string);

    procedure SaveToPDFStream(const AStream: TStream; const APageImage: Boolean = False);

    /// <summary> 以字符串形式获取文档各节正文内容 </summary>
    function SaveToText: string;

    /// <summary> 读文本到第一节正文 </summary>
    function LoadFromText(const AText: string): Boolean;

    /// <summary> 文档各节正文字符串保存为文本格式文件 </summary>
    procedure SaveToTextFile(const AFileName: string; const AEncoding: TEncoding);

    /// <summary> 读取文本文件内容到第一节正文 </summary>
    function LoadFromTextFile(const AFileName: string; const AEncoding: TEncoding): Boolean;

    /// <summary> 文档各节正文字符串保存为文本格式流 </summary>
    procedure SaveToTextStream(const AStream: TStream; const AEncoding: TEncoding);

    /// <summary> 读取文本文件流 </summary>
    function LoadFromTextStream(const AStream: TStream; AEncoding: TEncoding): Boolean;

    /// <summary> 文档保存到流 </summary>
    procedure SaveToStream(const AStream: TStream; const AQuick: Boolean = False;
      const AAreas: TSectionAreas = [saHeader, saPage, saFooter]); virtual;

    /// <summary> 读取文件流 </summary>
    function LoadFromStream(const AStream: TStream): Boolean; virtual;

    /// <summary> 插入Lite流 </summary>
    function InsertLiteStream(const AStream: TStream): Boolean;

    /// <summary> 文档保存为xml格式 </summary>
    procedure SaveToXml(const AFileName: string; const AEncoding: TEncoding);

    procedure SaveToXmlStream(const AStream: TStream; const AEncoding: TEncoding);

    /// <summary> 读取xml格式 </summary>
    function LoadFromXml(const AFileName: string): Boolean;

    function LoadFromXmlStream(const AStream: TStream): Boolean;

    /// <summary> 导出为html格式 </summary>
    /// <param name="ASeparateSrc">True：图片等保存到文件夹，False以base64方式存储到页面中</param>
    procedure SaveToHtml(const AFileName: string; const ASeparateSrc: Boolean = False);

    /// <summary>
    /// 将文档每一页保存为图片
    /// </summary>
    /// <param name="APath">图片路径</param>
    /// <param name="APrefix">单张图片时文件名，多图片时每一张名称的前缀</param>
    /// <param name="AImageType">图片格式如 BMP, JPG, PNG</param>
    /// <param name="AOnePaper">True单张图片</param>
    procedure SaveToImage(const APath, APrefix, AImageType: string; const AOnePaper: Boolean = True);

    /// <summary> 当前光标所在的节 </summary>
    property ActiveSection: THCSection read GetActiveSection;
    /// <summary> 总页数 </summary>
    property PageCount: Integer read GetPageCount;
    /// <summary> 当前文档所有节 </summary>
    property Sections: TObjectList<THCSection> read FSections;
    /// <summary> 当前文档样式表 </summary>
    property Style: THCStyle read FStyle;
  end;

implementation

uses
  HCUnitConversion, HCXml {$IFDEF SYNPDF},SynPdf{$ENDIF};

{ THCViewLite }

procedure THCViewLite.Clear;
begin
  FStyle.Initialize;  // 先清样式，防止Data初始化为EmptyData时空Item样式赋值为CurStyleNo
  FActiveSectionIndex := 0;
  FSections.DeleteRange(1, FSections.Count - 1);
  FSections[0].Clear;
end;

constructor THCViewLite.Create;
begin
  FStyle := THCStyle.CreateEx(True, True);
  FSections := TObjectList<THCSection>.Create;
  FSections.Add(NewDefaultSection);
  FActiveSectionIndex := 0;
end;

procedure THCViewLite.DataLoadLiteStream(const AStream: TStream; const AProc: THCLoadProc);
var
  vFileFormat: string;
  vFileVersion: Word;
  vLang, vSType: Byte;
  vStyle: THCStyle;
begin
  _LoadFileFormatAndVersion(AStream, vFileFormat, vFileVersion, vLang);  // 文件格式和版本
  if vFileVersion > 59 then
  begin
    AStream.ReadBuffer(vSType, 1);
    if vSType <> HC_STREAM_LITE then  // 不是Lite流
      Exit;
  end;

  vStyle := THCStyle.Create;
  try
    vStyle.LoadFromStream(AStream, vFileVersion);
    AProc(vFileVersion, vStyle);
  finally
    FreeAndNil(vStyle);
  end;
end;

class procedure THCViewLite.DeleteUnUsedStyle(const AStyle: THCStyle;
  const ASections: TObjectList<THCSection>; const AAreas: TSectionAreas);
var
  i, vUnCount: Integer;
  vData: THCCustomData;
begin
  AStyle.TextStyles[0].CheckSaveUsed := True;
  AStyle.TextStyles[0].TempNo := 0;
  for i := 1 to AStyle.TextStyles.Count - 1 do
  begin
    AStyle.TextStyles[i].CheckSaveUsed := False;
    AStyle.TextStyles[i].TempNo := THCStyle.Null;
  end;

  for i := 0 to AStyle.ParaStyles.Count - 1 do
  begin
    AStyle.ParaStyles[i].CheckSaveUsed := False;
    AStyle.ParaStyles[i].TempNo := THCStyle.Null;
  end;

  for i := 0 to ASections.Count - 1 do
    ASections[i].MarkStyleUsed(True, AAreas);

  vUnCount := 0;
  for i := 1 to AStyle.TextStyles.Count - 1 do
  begin
    if AStyle.TextStyles[i].CheckSaveUsed then
      AStyle.TextStyles[i].TempNo := i - vUnCount
    else
      Inc(vUnCount);
  end;

  vUnCount := 0;
  for i := 0 to AStyle.ParaStyles.Count - 1 do
  begin
    if AStyle.ParaStyles[i].CheckSaveUsed then
      AStyle.ParaStyles[i].TempNo := i - vUnCount
    else
      Inc(vUnCount);
  end;

  for i := 0 to ASections.Count - 1 do
  begin
    ASections[i].MarkStyleUsed(False);

    vData := ASections[i].ActiveData.GetTopLevelData;
    if vData.CurStyleNo > THCStyle.Null then  // 光标不在RectItem
      vData.CurStyleNo := AStyle.TextStyles[vData.CurStyleNo].TempNo;

    vData.CurParaNo := AStyle.ParaStyles[vData.CurParaNo].TempNo;
  end;

  for i := AStyle.TextStyles.Count - 1 downto 1 do
  begin
    if not AStyle.TextStyles[i].CheckSaveUsed then
      AStyle.TextStyles.Delete(i);
  end;

  for i := AStyle.ParaStyles.Count - 1 downto 0 do
  begin
    if not AStyle.ParaStyles[i].CheckSaveUsed then
      AStyle.ParaStyles.Delete(i);
  end;
end;

destructor THCViewLite.Destroy;
begin
  FreeAndNil(FSections);
  FreeAndNil(FStyle);
  inherited Destroy;
end;

procedure THCViewLite.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const ALoadSectionProc: TLoadSectionProc);
var
  vFileExt: string;
  vVersion: Word;
  vLang, vSType: Byte;
  vSize: Integer;
begin
  AStream.Position := 0;
  _LoadFileFormatAndVersion(AStream, vFileExt, vVersion, vLang);  // 文件格式和版本
  if (vFileExt <> HC_EXT) and (vFileExt <> 'cff.') then
    raise Exception.Create('加载失败，不是' + HC_EXT + '文件！');
  if vVersion > HC_FileVersionInt then
    raise Exception.Create('加载失败，当前编辑器最高支持版本为'
      + IntToStr(HC_FileVersionInt) + '的文件，无法打开版本为'
      + IntToStr(vVersion) + '的文件！');

  if vVersion > 59 then
  begin
    AStream.ReadBuffer(vSType, 1);
    if vSType <> HC_STREAM_VIEW then  // 不是View流
      Exit;
  end;

  DoLoadStreamBefor(AStream, vVersion);  // 触发加载前事件
  AStyle.LoadFromStream(AStream, vVersion);  // 加载样式表
  DoLoadMutMargin(AStream, AStyle, vVersion);

  if vVersion > 55 then
  begin
    AStream.ReadBuffer(vLang, SizeOf(vLang));
    if vLang >= 0 then
       AStream.ReadBuffer(vLang, SizeOf(vLang));

    if vLang >= 0 then
      AStream.ReadBuffer(vSize, SizeOf(vSize));
  end;

  ALoadSectionProc(vVersion);  // 加载节数量、节数据
  DoLoadStreamAfter(AStream, vVersion);
end;

procedure THCViewLite.DoLoadMutMargin(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vByte: Byte;
begin
  if AFileVersion > 61 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));
end;

procedure THCViewLite.DoLoadStreamAfter(const AStream: TStream; const AFileVersion: Word);
begin
end;

procedure THCViewLite.DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word);
begin
end;

procedure THCViewLite.DoSaveMutMargin(const AStream: TStream);
var
  vByte: Byte;
begin
  vByte := 0;
  AStream.WriteBuffer(vByte, SizeOf(vByte));
end;

procedure THCViewLite.DoSaveStreamAfter(const AStream: TStream);
begin
end;

procedure THCViewLite.DoSaveStreamBefor(const AStream: TStream);
begin
end;

function THCViewLite.DoSectionCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
begin
  Result := nil;
end;

procedure THCViewLite.DoSectionPaintFooterAfter(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
var
  i, vSectionStartPageIndex, vSectionIndex, vAllPageCount: Integer;
  vS: string;
  vSection: THCSection;
begin
  vSection := Sender as THCSection;

  if vSection.PageNoVisible then  // 显示页码
  begin
    vSectionIndex := FSections.IndexOf(vSection);
    vSectionStartPageIndex := 0;
    vAllPageCount := 0;
    for i := 0 to FSections.Count - 1 do
    begin
      if i = vSectionIndex then
        vSectionStartPageIndex := vAllPageCount;

      vAllPageCount := vAllPageCount + FSections[i].PageCount;
    end;
    vS := Format(vSection.PageNoFormat, [vSectionStartPageIndex + vSection.PageNoFrom + APageIndex, vAllPageCount]);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Size := 10;
    ACanvas.Font.Name := '宋体';
    ACanvas.TextOut(ARect.Left + (ARect.Width - ACanvas.TextWidth(vS)) div 2,
      ARect.Top + vSection.Footer.Height, vS);
  end;
end;

function THCViewLite.GetActiveSection: THCSection;
begin
  Result := FSections[FActiveSectionIndex];
end;

function THCViewLite.GetPageCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FSections.Count - 1 do
    Result := Result + FSections[i].PageCount;
end;

function THCViewLite.GetSectionPageIndexByPageIndex(const APageIndex: Integer;
  var ASectionPageIndex: Integer): Integer;
var
  i, vPageCount: Integer;
begin
  Result := -1;
  vPageCount := 0;
  for i := 0 to FSections.Count - 1 do
  begin
    if vPageCount + FSections[i].PageCount > APageIndex then
    begin
      Result := i;  // 找到节序号
      ASectionPageIndex := APageIndex - vPageCount;  // 节中页序号

      Break;
    end
    else
      vPageCount := vPageCount + FSections[i].PageCount;
  end;
end;

function THCViewLite.InsertLiteStream(const AStream: TStream): Boolean;
var
  vResult: Boolean;
begin
  Result := False;
  DataLoadLiteStream(AStream, procedure(const AFileVersion: Word; const AStyle: THCStyle)
  begin
    vResult := ActiveSection.InsertStream(AStream, AStyle, AFileVersion);
  end);
  Result := vResult;
end;

procedure THCViewLite.LoadFromDocumentFile(const AFileName, AExt: string);
begin
  Self.Clear;
  //HCViewLoadFromDocumentFile(Self, AFileName, AExt);
end;

procedure THCViewLite.LoadFromDocumentStream(const AStream: TStream; const AExt: string);
begin
  Self.Clear;
  //HCViewLoadFromDocumentStream(Self, AStream, AExt);
end;

function THCViewLite.LoadFromFile(const AFileName: string): Boolean;
var
  vStream: TStream;
begin
  Result := False;
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function THCViewLite.LoadFromStream(const AStream: TStream): Boolean;
begin
  Result := False;
  FStyle.States.Include(hosLoading);
  try
    AStream.Position := 0;
    DoLoadFromStream(AStream, FStyle, procedure(const AFileVersion: Word)
      var
        i: Integer;
        vByte: Byte;
        vSection: THCSection;
      begin
        AStream.ReadBuffer(vByte, 1);  // 节数量
        // 各节数据
        FSections[0].LoadFromStream(AStream, FStyle, AFileVersion);
        for i := 1 to vByte - 1 do
        begin
          vSection := NewDefaultSection;
          vSection.LoadFromStream(AStream, FStyle, AFileVersion);
          FSections.Add(vSection);
        end;
      end);
  finally
    FStyle.States.Exclude(hosLoading);
  end;

  Result := True;
end;

function THCViewLite.LoadFromText(const AText: string): Boolean;
begin
  Result := False;
  Self.Clear;
  FStyle.Initialize;
  if AText <> '' then
  begin
    FStyle.States.Include(hosLoading);
    try
      Result := ActiveSection.InsertText(AText);
    finally
      FStyle.States.Exclude(hosLoading);
    end;
  end;
end;

function THCViewLite.LoadFromTextFile(const AFileName: string; const AEncoding: TEncoding): Boolean;
begin

end;

function THCViewLite.LoadFromTextStream(const AStream: TStream; AEncoding: TEncoding): Boolean;
var
  vSize: Integer;
  vBuffer: TBytes;
  vS: string;
begin
  Result := False;
  vSize := AStream.Size - AStream.Position;
  SetLength(vBuffer, vSize);
  AStream.Read(vBuffer[0], vSize);
  vSize := TEncoding.GetBufferEncoding(vBuffer, AEncoding);
  vS := AEncoding.GetString(vBuffer, vSize, Length(vBuffer) - vSize);
  Result := LoadFromText(vS);
end;

function THCViewLite.LoadFromXml(const AFileName: string): Boolean;
var
  vStream: TMemoryStream;
begin
  Result := False;
  vStream := TMemoryStream.Create;
  try
    vStream.LoadFromFile(AFileName);
    vStream.Position := 0;
    Result := LoadFromXmlStream(vStream);
  finally
    vStream.Free;
  end;
end;

function THCViewLite.LoadFromXmlStream(const AStream: TStream): Boolean;
var
  vSection: THCSection;
  vXml: IHCXMLDocument;
  vNode: IHCXMLNode;
  vVersion: string;
  vLang: Byte;
  i, j: Integer;
begin
  Result := False;
  Self.Clear;

  vXml := THCXMLDocument.Create(nil);
  //vXml.LoadFromFile(AFileName);
  vXml.LoadFromStream(AStream);
  if vXml.DocumentElement.LocalName = 'HCView' then
  begin
    if vXml.DocumentElement.Attributes['EXT'] <> HC_EXT then Exit;

    vVersion := vXml.DocumentElement.Attributes['ver'];
    vLang := vXml.DocumentElement.Attributes['lang'];

    FStyle.States.Include(hosLoading);
    try
      for i := 0 to vXml.DocumentElement.ChildNodes.Count - 1 do
      begin
        vNode := vXml.DocumentElement.ChildNodes[i];
        if vNode.NodeName = 'style' then
          FStyle.ParseXml(vNode)
        else
        if vNode.NodeName = 'sections' then
        begin
          FSections[0].ParseXml(vNode.ChildNodes[0]);
          for j := 1 to vNode.ChildNodes.Count - 1 do
          begin
            vSection := NewDefaultSection;
            vSection.ParseXml(vNode.ChildNodes[j]);
            FSections.Add(vSection);
          end;
        end;
      end;
    finally
      FStyle.States.Exclude(hosLoading);
    end;

    Result := True;
  end;
end;

function THCViewLite.NewDefaultSection: THCSection;
begin
  Result := THCSection.Create(FStyle);
  Result.OnPaintFooterAfter := DoSectionPaintFooterAfter;
  Result.OnCreateItemByStyle := DoSectionCreateStyleItem;
end;

procedure THCViewLite.SaveToDocumentFile(const AFileName, AExt: string);
begin
  //HCViewSaveToDocumentFile(Self, AFileName, AExt);
end;

procedure THCViewLite.SaveToFile(const AFileName: string; const AQuick: Boolean);
var
  vStream: TStream;
begin
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(vStream, AQuick);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure THCViewLite.SaveToHtml(const AFileName: string; const ASeparateSrc: Boolean);
var
  vHtmlTexts: TStrings;
  i: Integer;
  vPath: string;
begin
  DeleteUnUsedStyle(FStyle, FSections, [saHeader, saPage, saFooter]);

  FStyle.GetHtmlFileTempName(True);
  if ASeparateSrc then
    vPath := ExtractFilePath(AFileName)
  else
    vPath := '';

  vHtmlTexts := TStringList.Create;
  try
    vHtmlTexts.Add('<!DOCTYPE HTML>');
    vHtmlTexts.Add('<html>');
    vHtmlTexts.Add('<head>');
    vHtmlTexts.Add('<title>');
    vHtmlTexts.Add('</title>');

    vHtmlTexts.Add(FStyle.ToCSS);

    vHtmlTexts.Add('</head>');

    vHtmlTexts.Add('<body>');
    for i := 0 to FSections.Count - 1 do
      vHtmlTexts.Add(FSections[i].ToHtml(vPath));

    vHtmlTexts.Add('</body>');
    vHtmlTexts.Add('</html>');

    vHtmlTexts.SaveToFile(AFileName);
  finally
    FreeAndNil(vHtmlTexts);
  end;
end;

procedure THCViewLite.SaveToImage(const APath, APrefix, AImageType: string; const AOnePaper: Boolean);
var
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
  i, vWidth, vHeight, vSectionIndex, vSectionPageIndex, vTop: Integer;
  vBmp: TBitmap;
begin
  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.ScaleX := 1;
    vPaintInfo.ScaleY := 1;
    vPaintInfo.Zoom := 1;
    vPaintInfo.Print := True;
    vPaintInfo.DPI := HCUnitConversion.PixelsPerInchX;
    vPaintInfo.ViewModel := THCViewModel.hvmFilm;

    if AOnePaper then
    begin
      vWidth := 0;
      vHeight := 0;

      for i := 0 to FSections.Count - 1 do
      begin
        vHeight := vHeight + FSections[i].PaperHeightPix * FSections[i].PageCount;
        if vWidth < FSections[i].PaperWidthPix then
          vWidth := FSections[i].PaperWidthPix;
      end;

      vPaintInfo.WindowWidth := vWidth;
      vPaintInfo.WindowHeight := vHeight;

      vBmp := TBitmap.Create;
      try
        vBmp.SetSize(vWidth, vHeight);

        vSectionIndex := 0;
        vSectionPageIndex := 0;
        vTop := 0;

        for i := 0 to Self.PageCount - 1 do
        begin
          vSectionIndex := GetSectionPageIndexByPageIndex(i, vSectionPageIndex);
          //vWidth := FSections[vSectionIndex].PaperWidthPix;
          vHeight := FSections[vSectionIndex].PaperHeightPix;

          vBmp.Canvas.Brush.Color := clWhite;
          vBmp.Canvas.FillRect(Rect(0, vTop, vWidth, vTop + vHeight));

          vScaleInfo := vPaintInfo.ScaleCanvas(vBmp.Canvas);
          try
            FSections[vSectionIndex].PaintPaper(vSectionPageIndex, 0, vTop, vBmp.Canvas, vPaintInfo);
            vTop := vTop + vHeight;
          finally
            vPaintInfo.RestoreCanvasScale(vBmp.Canvas, vScaleInfo);
          end;
        end;

        if AImageType = 'BMP' then
          vBmp.SaveToFile(APath + APrefix + '.bmp')
        else
        if AImageType = 'JPG' then
          BitmapSaveAsJPGE(vBmp, APath + APrefix + '.jpg')
        else
          BitmapSaveAsPNG(vBmp, APath + APrefix + '.png');
      finally
        FreeAndNil(vBmp);
      end;
    end
    else
    begin
      vSectionIndex := 0;
      vSectionPageIndex := 0;
      for i := 0 to Self.PageCount - 1 do
      begin
        vSectionIndex := GetSectionPageIndexByPageIndex(i, vSectionPageIndex);

        vBmp := TBitmap.Create;
        try
          vBmp.SetSize(FSections[vSectionIndex].PaperWidthPix, FSections[vSectionIndex].PaperHeightPix);
          vBmp.Canvas.Brush.Color := clWhite;
          vBmp.Canvas.FillRect(Rect(0, 0, vBmp.Width, vBmp.Height));

          vPaintInfo.WindowWidth := vBmp.Width;
          vPaintInfo.WindowHeight := vBmp.Height;
          vScaleInfo := vPaintInfo.ScaleCanvas(vBmp.Canvas);
          try
            vBmp.Canvas.Brush.Color := clWhite;
            vBmp.Canvas.FillRect(Rect(0, 0, vBmp.Width, vBmp.Height));
            FSections[vSectionIndex].PaintPaper(vSectionPageIndex, 0, 0, vBmp.Canvas, vPaintInfo);
          finally
            vPaintInfo.RestoreCanvasScale(vBmp.Canvas, vScaleInfo);
          end;

          if AImageType = 'BMP' then
            vBmp.SaveToFile(APath + APrefix + IntToStr(i + 1) + '.bmp')
          else
          if AImageType = 'JPG' then
            BitmapSaveAsJPGE(vBmp, aPath + APrefix + IntToStr(i + 1) + '.jpg')
          else
            BitmapSaveAsPNG(vBmp, APath + APrefix + IntToStr(i + 1) + '.png')
        finally
          FreeAndNil(vBmp);
        end;
      end;
    end;
  finally
    FreeAndNil(vPaintInfo);
  end;
end;

procedure THCViewLite.SaveToPDF(const AFileName: string);
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    SaveToPDFStream(vStream);
    vStream.SaveToFile(AFileName);
  finally
    vStream.Free;
  end;
end;

procedure THCViewLite.SaveToPDFStream(const AStream: TStream; const APageImage: Boolean);
  {$IFDEF SYNPDF}
  function GetPDFPaperSize(const APaperSize: Integer): TPDFPaperSize;
  begin
    case APaperSize of
      DMPAPER_A3: Result := TPDFPaperSize.psA3;
      DMPAPER_A4: Result := TPDFPaperSize.psA4;
      DMPAPER_A5: Result := TPDFPaperSize.psA5;
      //DMPAPER_B5: Result := TPDFPaperSize.psB5;
    else
      Result := TPDFPaperSize.psUserDefined;
    end;
  end;

var
  i, j, vDPI: Integer;
  vPDF: TPdfDocumentGDI;
  vPage: TPdfPage;
  vPaintInfo: TSectionPaintInfo;

  vScaleInfo: TScaleInfo;
  vBmp: TBitmap;
begin
  // 先要Format一下
  vPDF := TPdfDocumentGDI.Create;
  try
    vPDF.Info.Author := 'HCView';
    vPDF.Info.CreationDate := Now;
    vPDF.Info.Creator := 'HCView';  // jt
    vPDF.Info.Keywords := '';  // 入院记录
    vPDF.Info.ModDate := Now;
    vPDF.Info.Subject := '';  // HIT 电子病历
    vPDF.Info.Title := '';  // 张三第1次

    vPDF.UseUniscribe := True;

    vDPI := PixelsPerInchX;
    vPDF.ScreenLogPixels := vDPI;
    if not APageImage then
    begin
      vPDF.EmbeddedTTF := True;
      //vPDF.EmbeddedWholeTTF := True;
      vPDF.EmbeddedTTFIgnore.Text := MSWINDOWS_DEFAULT_FONTS + #13#10'宋体';
    end;

    vPaintInfo := TSectionPaintInfo.Create;
    try
      vPaintInfo.Print := True;

      for i := 0 to FSections.Count - 1 do
      begin
        vPaintInfo.SectionIndex := i;
        vPaintInfo.Zoom := vPDF.ScreenLogPixels / vDPI;
        vPaintInfo.ScaleX := vPaintInfo.Zoom;
        vPaintInfo.ScaleY := vPaintInfo.Zoom;

        for j := 0 to FSections[i].PageCount - 1 do
        begin
          vPage := vPDF.AddPage;

          if FSections[i].PaperOrientation = TPaperOrientation.cpoLandscape then
            vPage.PageLandscape := True
          else
            vPage.PageLandscape := False;

          //vPDF.DefaultPaperSize := GetPDFPaperSize(FSections[i].PaperSize);
          //if vPDF.DefaultPaperSize = TPDFPaperSize.psUserDefined then
          begin  // 英寸单位下总共多少像素
            vPage.PageWidth := Round(FSections[i].PaperWidth / 25.4 * 72);
            vPage.PageHeight := Round(FSections[i].PaperHeight / 25.4 * 72);
          end;

          vPaintInfo.PageIndex := j;
          vPaintInfo.WindowWidth := FSections[i].PaperWidthPix;
          vPaintInfo.WindowHeight := FSections[i].PaperHeightPix;

          if not APageImage then
            FSections[i].PaintPaper(j, 0, 0, vPDF.VCLCanvas, vPaintInfo)
          else
          begin
            vBmp := TBitmap.Create;
            try
              vBmp.SetSize(FSections[i].PaperWidthPix, FSections[i].PaperHeightPix);
              vBmp.Canvas.Brush.Color := clWhite;
              vBmp.Canvas.FillRect(Rect(0, 0, vBmp.Width, vBmp.Height));
              FSections[i].PaintPaper(j, 0, 0, vBmp.Canvas, vPaintInfo);
              vPDF.VCLCanvas.Draw(0, 0, vBmp);
            finally
              vBmp.Free;
            end;
          end;
        end;
      end;
    finally
      vPaintInfo.Free;
    end;

    vPDF.SaveToStream(AStream);
  finally
    vPDF.Free;
  end;
  {$ELSE}
begin
  {$ENDIF}
end;

procedure THCViewLite.SaveToStream(const AStream: TStream; const AQuick: Boolean; const AAreas: TSectionAreas);
var
  vByte, vSType: Byte;
  i, vSize: Integer;
begin
  FStyle.States.Include(hosSaving);
  try
    _SaveFileFormatAndVersion(AStream);  // 文件格式和版本
    vSType := HC_STREAM_VIEW;
    AStream.WriteBuffer(vSType, 1);

    DoSaveStreamBefor(AStream);

    if not AQuick then
      DeleteUnUsedStyle(FStyle, FSections, AAreas);  // 删除不使用的样式

    FStyle.SaveToStream(AStream);
    DoSaveMutMargin(AStream);

    vByte := 0;
    AStream.WriteBuffer(vByte, SizeOf(vByte));
    AStream.WriteBuffer(vByte, SizeOf(vByte));
    vSize := 0;
    AStream.WriteBuffer(vSize, SizeOf(vSize));

    // 节数量
    vByte := FSections.Count;
    AStream.WriteBuffer(vByte, 1);
    // 各节数据
    for i := 0 to FSections.Count - 1 do
      FSections[i].SaveToStream(AStream, AAreas);

    DoSaveStreamAfter(AStream);
  finally
    FStyle.States.Exclude(hosSaving);
  end;
end;

function THCViewLite.SaveToText: string;
var
  i: Integer;
begin
  FStyle.States.Include(hosSaving);
  try
    Result := FSections[0].SaveToText;
    for i := 1 to FSections.Count - 1 do  // 各节数据
      Result := Result + sLineBreak + FSections[i].SaveToText;
  finally
    FStyle.States.Exclude(hosSaving);
  end;
end;

procedure THCViewLite.SaveToTextFile(const AFileName: string; const AEncoding: TEncoding);
var
  vStream: TStream;
begin
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToTextStream(vStream, AEncoding);
  finally
    vStream.Free;
  end;
end;

procedure THCViewLite.SaveToTextStream(const AStream: TStream; const AEncoding: TEncoding);
var
  vText: string;
  vBuffer, vPreamble: TBytes;
begin
  vText := SaveToText;

  vBuffer := AEncoding.GetBytes(vText);
  vPreamble := AEncoding.GetPreamble;

  if Length(vPreamble) > 0 then
    AStream.WriteBuffer(vPreamble[0], Length(vPreamble));

  AStream.WriteBuffer(vBuffer[0], Length(vBuffer));
end;

procedure THCViewLite.SaveToXml(const AFileName: string; const AEncoding: TEncoding);
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    SaveToXmlStream(vStream, AEncoding);
    vStream.SaveToFile(AFileName);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure THCViewLite.SaveToXmlStream(const AStream: TStream; const AEncoding: TEncoding);
var
  vXml: IHCXMLDocument;
  vNode: IHCXMLNode;
  i: Integer;
begin
  FStyle.States.Include(hosSaving);
  try
    DeleteUnUsedStyle(FStyle, FSections, [saHeader, saPage, saFooter]);

    vXml := THCXMLDocument.Create(nil);
    vXml.Active := True;
    vXml.Version := '1.0';
    vXml.Encoding := GetEncodingName(AEncoding);

    vXml.DocumentElement := vXml.CreateNode('HCView');
    vXml.DocumentElement.Attributes['EXT'] := HC_EXT;
    vXml.DocumentElement.Attributes['ver'] := HC_FileVersion;
    vXml.DocumentElement.Attributes['lang'] := HC_PROGRAMLANGUAGE;

    vNode := vXml.DocumentElement.AddChild('style');
    FStyle.ToXml(vNode);  // 样式表

    vNode := vXml.DocumentElement.AddChild('sections');
    vNode.Attributes['count'] := FSections.Count;  // 节数量

    for i := 0 to FSections.Count - 1 do  // 各节数据
      FSections[i].ToXml(vNode.AddChild('sc'));

    vXml.SaveToStream(AStream);
  finally
    FStyle.States.Exclude(hosSaving);
  end;
end;

end.
