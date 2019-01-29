{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2019-3-19             }
{                                                       }
{               RTF格式文件读写单元                     }
{                                                       }
{*******************************************************}

unit HCRtfRW;

interface

uses
  Windows, Classes, SysUtils, Graphics, Generics.Collections, HCView, HCCommon,
  HCDocumentRW, HCParaStyle, HCTextStyle, HCItem, HCTableItem, HCImageItem,
  HCTextItem, HCViewData, HCRichData, HCStyle, HCTableRow, HCTableCell, HCList,
  HCTableCellData, HCCustomData;

type
  THCRtfDataType = (rdtNor, rdtBin, rdtHex);

  // Rtf Destination State
  THCRtfDestState = (rdsNorm, rdsSkip,  rdsFontTable, rdsStyleSheet, rdsColorTable,
    rdsStyleSheetEntry, rdsPict, rdsTable);

  THCRtfHeadFootType = (hftMainText, hftHeader, hftFooter);

  THCRtfReadState = class(TObject)
  public
    DataType: THCRtfDataType;
    DestState: THCRtfDestState;
    HeadFootType: THCRtfHeadFootType;
    constructor Create;
  end;

  THCRtfColor = class(TObject)
  public
    Red, Green, Blue: Byte;
  end;

  THCRtfSectionProp = class(TObject)
  public
    ColumnCount,
    HeaderY,
    FooterY,
    PageNumberX,
    PageNumberY: Integer;

    procedure Initialize;
  end;

  THCRtfRowSize = THCIntegerList;

  THCRtfTable = class(THCTableItem)
  public
    RowSizes: TObjectList<THCRtfRowSize>;
    LastRow,  // 是否为最后一行
    RowStartRead,  // 是否开始读取行
    RowHasReadCell  // 行中是否已经读过至少一个Cell
      : Boolean;
    constructor Create(const AOwnerData: THCCustomData; const ARowCount, AColCount,
      AWidth: Integer); override;
    destructor Destroy; override;
    procedure MergeCellSameStyleItems;
    procedure NewRowSize;
  end;

  THCRtfReader = class(THCDocumentReader)
  private
    //FHCView: THCView;
    SkipCount: Integer;
    FCodePage: Cardinal;
    FSectionProp: THCRtfSectionProp;
    FPaperWidth, FPaperHeight, FMarginLeft, FMarginRight, FMarginTop, FMarginBottom: Cardinal;
    FLastChar: AnsiChar;
    FTempText, FText: AnsiString;

    FReadState: THCRtfReadState;
    FReadStateStack: TStack<THCRtfReadState>;

    FData: THCViewData;
    FReadData: THCRichData;
    FDataStack: TStack<THCRichData>;

    FColorTable: TObjectList<THCRtfColor>;
    FDefFontNo: Integer;
    FParaStyle: THCParaStyle;
    FTextStyle: THCTextStyle;
    FStyle: THCStyle;
    function CheckRtfSign(const AStream: TStream): Boolean;
    function ReadAnsiChar(const AStream: TStream): AnsiChar;
    procedure PushRtfState;
    procedure PopRtfState;
    procedure CheckTextItemEnd;
    procedure RebuildLastTable(const AData: THCRichData);
    function ReadGroupEnd(const ADestState: THCRtfDestState; const APushState: THCRtfReadState): Boolean;
    function ParseAnsiChar(const AAnsiChar: AnsiChar): Boolean;
    function ParseHexAnsiChar(const AStream: TStream; const AAnsiChar: AnsiChar): Boolean;
    function TranslateKeyword(const AKeyword: string; const AParam: Integer; const AIsParam: Boolean): Boolean;
    function ParseRtfKeyword(const AStream: TStream): Boolean;
    function GetFontIndex(const ANo: Integer): Integer;
    procedure LoadFromRtfStream(const AStream: TStream);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(const AHCView: THCView; const AStream: TStream); override;
    procedure SaveToStream(const AHCView: THCView; const AStream: TStream); override;
    class procedure InsertStream(const AHCView: THCView; const AStream: TStream);
    property Data: THCViewData read FData;
    property Style: THCStyle read FStyle;
  end;

implementation

uses
  HCUnitConversion;

procedure MergeSameStyleItems(const AData: THCRichData);
var
  i: Integer;
  vItemPrev, vItem: THCCustomItem;
begin
  for i := AData.Items.Count - 1 downto 1 do
  begin
    vItem := AData.Items[i];
    if not vItem.ParaFirst then
    begin
      if vItem is THCRtfTable then
        (vItem as THCRtfTable).MergeCellSameStyleItems
      else
      begin
        vItemPrev := AData.Items[i - 1];
        if vItem.CanConcatItems(vItemPrev) then
        begin
          vItemPrev.Text := vItemPrev.Text + vItem.Text;
          AData.Items.Delete(i);
        end;
      end;
    end;
  end;
end;

{ THCRtfReader }

function THCRtfReader.CheckRtfSign(const AStream: TStream): Boolean;
var
  vSign: array[0..4] of Byte;
  vLen: Integer;
begin
  vLen := AStream.Read(vSign, 5);
  AStream.Position := 0;
  Result := (vLen = 5) and (vSign[0] = Ord('{')) and (vSign[1] = Ord('\')) and
    (vSign[2] = Ord('r')) and (vSign[3] = Ord('t')) and (vSign[4] = Ord('f'));
end;

constructor THCRtfReader.Create;
begin
  inherited Create;
  FSectionProp := THCRtfSectionProp.Create;
  FReadState := THCRtfReadState.Create;

  FStyle := THCStyle.CreateEx(True, True);
  FData := THCViewData.Create(FStyle);
  FReadData := FData;

  FReadStateStack := TStack<THCRtfReadState>.Create;

  FDataStack := TStack<THCRichData>.Create;

  FColorTable := TObjectList<THCRtfColor>.Create;
  //FStyle :=  THCStyle.CreateEx(True, True);
  FParaStyle := THCParaStyle.Create;
  FTextStyle := THCTextStyle.Create;
  FDefFontNo := -1;
  FLastChar := #0;
end;

destructor THCRtfReader.Destroy;
begin
  FreeAndNil(FSectionProp);
  FreeAndNil(FReadState);
  FreeAndNil(FReadStateStack);
  FreeAndNil(FData);
  FreeAndNil(FDataStack);
  FreeAndNil(FColorTable);
  FreeAndNil(FStyle);
  FreeAndNil(FParaStyle);
  FreeAndNil(FTextStyle);
  inherited;
end;

function THCRtfReader.GetFontIndex(const ANo: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FStyle.TextStyles.Count - 1 do
  begin
    if FStyle.TextStyles[i].TempNo = ANo then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

class procedure THCRtfReader.InsertStream(const AHCView: THCView; const AStream: TStream);
var
  vRtf: THCRtfReader;
  vMS: TMemoryStream;
begin
  vRtf := THCRtfReader.Create;
  try
    vRtf.LoadFromRtfStream(AStream);
    vMS := TMemoryStream.Create;
    try
      vRtf.Data.SaveToStream(vMS);
      vMS.Position := 0;
      AHCView.ActiveSection.InsertStream(vMS, vRtf.Style, HC_FileVersionInt);
    finally
      FreeAndNil(vMS);
    end;
  finally
    FreeAndNil(vRtf);
  end;
end;

procedure THCRtfReader.LoadFromRtfStream(const AStream: TStream);
var
  vChar: AnsiChar;
  vNextChar, vCharOrd: Integer;
  vByte: Byte;
begin
  if not CheckRtfSign(AStream) then Exit;

  vNextChar := -1;

  while AStream.Position < AStream.Size do
  begin
    if FLastChar <> #0 then
    begin
      vChar := FLastChar;
      FLastChar := #0;
    end
    else
      vChar := ReadAnsiChar(AStream);

    case vChar of
      '{':
        begin
          SkipCount := 0;
          PushRtfState;
        end;

      '}':
        begin
          SkipCount := 0;
          CheckTextItemEnd;
          PopRtfState;
        end;

      '\':
        begin
          if not ParseRtfKeyword(AStream) then
            Exit;
        end;
      #13, #10: ;
    else
      if FReadState.DestState <> rdsSkip then
      begin
        if FReadState.DataType = rdtNor then
        begin
          if SkipCount = 0 then
            ParseAnsiChar(vChar)
          else
            Dec(SkipCount);
        end
        else
        if FReadState.DataType = rdtHex then
          ParseHexAnsiChar(AStream, vChar);
      end;
    end;
  end;

  MergeSameStyleItems(FData);
end;

procedure THCRtfReader.LoadFromStream(const AHCView: THCView; const AStream: TStream);
var
  vMS: TMemoryStream;
begin
  vMS := TMemoryStream.Create;
  try
    LoadFromRtfStream(AStream);
    FData.SaveToStream(vMS);
    AHCView.ActiveSection.Clear;
    AHCView.ActiveSection.ActiveData := AHCView.ActiveSection.PageData;
    vMS.Position := 0;
    AHCView.ActiveSection.InsertStream(vMS, FStyle, HC_FileVersionInt)
  finally
    FreeAndNil(vMS);
  end;
end;

procedure THCRtfReader.CheckTextItemEnd;
var
  vItem: THCCustomItem;
  vParaFirst: Boolean;
begin
  if FText <> '' then
  begin
    if FReadData.IsEmptyData then
    begin
      FReadData.Items.Clear;
      vParaFirst := True;
    end
    else
      vParaFirst := False;

    vItem := FReadData.CreateDefaultTextItem;
    vItem.ParaFirst := vParaFirst;
    vItem.Text := FText;
    FReadData.Items.Add(vItem);
    //FData.InsertItem(FData.Items.Count, vItem, False);
    FText := '';
  end;
end;

function THCRtfReader.ParseAnsiChar(const AAnsiChar: AnsiChar): Boolean;
var
  vRtfColor: THCRtfColor;
begin
  Result := True;

  case FReadState.DestState of
    rdsNorm, rdsTable:
      begin
        if AAnsiChar <> #160 then
          FText := FText + AAnsiChar;
      end;

    rdsFontTable: // rtf中的字体
      begin
        if AAnsiChar <> ';' then
          FTempText := FTempText + AAnsiChar;
      end;

    rdsColorTable:
      begin
        if AAnsiChar = ';' then
        begin
          vRtfColor := THCRtfColor.Create;
          FColorTable.Add(vRtfColor);
        end;
      end;

    rdsStyleSheetEntry:
      begin
        if AAnsiChar <> ';' then
          FTempText := FTempText + AAnsiChar;
      end;
  end;
end;

function THCRtfReader.ParseHexAnsiChar(const AStream: TStream; const AAnsiChar: AnsiChar): Boolean;
var
  vb, vNibble: Integer;
  vChar: AnsiChar;
begin
  Result := True;
  vb := 0;
  vNibble := 2;
  vChar := AAnsiChar;

  while True do
  begin
    vb := vb shl 4;
    if (vChar in ['0'..'9']) then
      vb := vb + (ord(vChar) - ord('0'))
    else
    begin
      if (vChar in ['a'..'z']) then
      begin
        if not (vChar in ['a'..'f']) then
          Break;  // rtf_ec_InvalidHex

        vb := vb + 10 + (ord(vChar) - ord('a'));
      end
      else
      begin
        if not (vChar in ['A'..'F']) then
          Break;  // rtf_ec_InvalidHex

        vb := vb + 10 + (ord(vChar) - ord('A'));
      end;
    end;

    Dec(vNibble);
    if (vNibble = 0) then
    begin
      if SkipCount = 0 then
        Result := ParseAnsiChar(AnsiChar(vb))    // 解析rtf中文本有效内容数据
      else
        Dec(SkipCount);

      FReadState.DataType := rdtNor;
      Break;
    end;

    vChar := ReadAnsiChar(AStream);
  end;
end;

function THCRtfReader.ParseRtfKeyword(const AStream: TStream): Boolean;
var
  vChar: AnsiChar;
  vKeyWord, vS: string;
  vIsParam, vIsNeg: Boolean;
  vParaValue: Integer;
begin
  Result := False;
  vIsParam := False;
  vIsNeg := False;
  vKeyWord := '';
  vChar := ReadAnsiChar(AStream);

  if not (vChar in ['a'..'z','A'..'Z']) then
  begin
    Result := TranslateKeyWord(vChar, 0, vIsParam);
    Exit;
  end;

  while True do
  begin
    vKeyWord := vKeyWord + vChar;
    vChar := ReadAnsiChar(AStream);
    if not (vChar in ['a'..'z','A'..'Z']) then
      Break;
  end;

  if vChar = '-' then
  begin
    vIsNeg := True;
    vChar := ReadAnsiChar(AStream);
  end;

  if vChar in ['0'..'9'] then
  begin
    vIsParam := True;
    vS := '';
    while True do
    begin
      vS := vS + vChar;
      vChar := ReadAnsiChar(AStream);
      if not (vChar in ['0'..'9']) then
        Break;
    end;

    vParaValue := StrToIntDef(vS, 0);
    if vIsNeg then
      vParaValue := -vParaValue;
  end;

  if vChar <> ' ' then
    FLastChar := vChar;

  Result := TranslateKeyword(vKeyword, vParaValue, vIsParam);  // 创建项目符号等
end;

procedure THCRtfReader.PopRtfState;
var
  vDestState: THCRtfDestState;
  vPeekReadState: THCRtfReadState;
  //vIndex: Integer;
begin
  vDestState := FReadState.DestState;
  vPeekReadState := FReadStateStack.Peek;
  if (vDestState = rdsStyleSheetEntry) and (vPeekReadState.DestState = rdsStyleSheet) then
  begin
    // 暂时不附加样式
    //FStyle.GetParaNo(FParaStyle, True);
    //vIndex := FStyle.GetStyleNo(FTextStyle, True);
    //FStyle.TextStyles[vIndex].TempNo := vPeekReadState.DefFontNo;
  end;

  FreeAndNil(FReadState);
  FReadState := FReadStateStack.Pop;
  //if vCurDestState <> vReadState.DestState then
  ReadGroupEnd(vDestState, FReadState);
end;

procedure THCRtfReader.PushRtfState;
var
  vDestState: THCRtfDestState;
begin
  vDestState := FReadState.DestState;
  FReadStateStack.Push(FReadState);
  FReadState := THCRtfReadState.Create;
  FReadState.DestState := vDestState;
  if vDestState = rdsStyleSheet then
    FReadState.DestState := rdsStyleSheetEntry;
end;

function THCRtfReader.ReadAnsiChar(const AStream: TStream): AnsiChar;
var
  vByte: Byte;
begin
  if AStream.Read(vByte, 1) < 0 then
    Result := #0
  else
    Result := AnsiChar(vByte);
end;

function THCRtfReader.ReadGroupEnd(const ADestState: THCRtfDestState;
  const APushState: THCRtfReadState): Boolean;
begin
  Result := True;
  case ADestState of
    rdsFontTable:
      begin
        FStyle.TextStyles.Last.Family := FTempText;
        FTempText := '';
      end;

    rdsStyleSheetEntry:
      begin
        //FStyleSheet.Last.NameEx := FTempText;
        FTempText := '';
      end;
  end;
end;

procedure THCRtfReader.RebuildLastTable(const AData: THCRichData);
var
  vTable, vNewTable: THCRtfTable;
  vCellData: THCTableCellData;
  vR, vC, vWidth, vMaxColRow, vMaxCol: Integer;
begin
  vTable := AData.Items.Last as THCRtfTable;

  vMaxCol := 0;
  for vR := 0 to vTable.RowCount - 1 do
  begin
    if vTable.Rows[vR].ColCount > vMaxCol then
    begin
      vMaxColRow := vR;
      vMaxCol := vTable.Rows[vR].ColCount;
    end;
  end;

  vWidth := vTable.BorderWidth;
  for vC := 0 to vTable.Rows[vMaxColRow].ColCount - 1 do
    vWidth := vWidth + vTable.RowSizes[vMaxColRow][vC] + vTable.BorderWidth;

  vNewTable := THCRtfTable.Create(AData, vTable.RowCount, vTable.Rows[0].ColCount, vWidth);
  for vR := 0 to vNewTable.Rows.Count - 1 do
  begin
    for vC := 0 to vTable.Rows[vR].ColCount - 1 do
    begin
      if Assigned(vTable.Cells[vR, vC].CellData) then
      begin
        vCellData := vNewTable.Cells[vR, vC].CellData;
        vCellData.BeginFormat;
        try
          vCellData.AddData(vTable.Cells[vR, vC].CellData);
        finally
          vCellData.EndFormat(False);
        end;
      end;
    end;
  end;

  for vC := 0 to vTable.Rows[vMaxColRow].ColCount - 1 do
    vNewTable.ColWidth[vC] := vTable.RowSizes[vMaxColRow][vC];

  AData.Items.Delete(AData.Items.Count - 1);
  //AData.InsertItem(AData.Items.Count, vNewTable, False);
  AData.Items.Add(vNewTable);
end;

procedure THCRtfReader.SaveToStream(const AHCView: THCView;
  const AStream: TStream);
begin
  inherited;

end;

function THCRtfReader.TranslateKeyword(const AKeyword: string;
  const AParam: Integer; const AIsParam: Boolean): Boolean;
var
  vTableItem: THCRtfTable;
  vTableRow: THCTableRow;
  vTableCell: THCTableCell;
  vParaFirst: Boolean;
  vIndex: Integer;
begin
  Result := True;

  if AKeyword = 'rtf' then

  else
  if AKeyword = 'ansi' then

  else
  if AKeyword = 'uc' then
    //FReadState.SkinAnsiCount := AParam
  else
  if AKeyword = 'deff' then  // 默认文本样式
  begin
    if FReadState.DestState = rdsNorm then
      FDefFontNo := AParam;
  end
  else
  if AKeyword = 'ansicpg' then
    FCodePage := Cardinal(AParam)
  else
  if AKeyword = 'fonttbl' then
  begin
    FStyle.TextStyles.Clear;
    FReadState.DestState := rdsFontTable;
  end
  else
  if AKeyword = 'f' then
  begin
    case FReadState.DestState of
      rdsFontTable:
        begin
          vIndex := FStyle.NewDefaultTextStyle;
          FStyle.TextStyles[vIndex].TempNo := AParam;
        end;

      rdsStyleSheet:
        FReadData.CurStyleNo := GetFontIndex(AParam);

      rdsNorm:
        FReadData.CurStyleNo := GetFontIndex(AParam);
    end;
  end
  else
  if AKeyword = '*' then
    FReadState.DestState := rdsSkip
  else
  if AKeyword = '''' then
    FReadState.DataType := rdtHex
  else
  if AKeyword = 'colortbl' then
  begin
    FColorTable.Clear;
    FReadState.DestState := rdsColorTable;
  end
  else
  if AKeyword = 'red' then
  begin
    if FReadState.DestState = rdsColorTable then
      FColorTable.Last.Red := AParam;
  end
  else
  if AKeyword = 'green' then
  begin
    if FReadState.DestState = rdsColorTable then
      FColorTable.Last.Green := AParam;
  end
  else
  if AKeyword = 'blue' then
  begin
    if FReadState.DestState = rdsColorTable then
      FColorTable.Last.Blue := AParam;
  end
  else
  if AKeyword = 'stylesheet' then
  begin
    //FStyleSheet.Clear;
    FReadState.DestState := rdsStyleSheet;
  end
  else
  if AKeyword = 'snext' then
  begin
    //if FReadState.DestState = rdsStyleSheetEntry then
    //  FStyleSheet.Last.Next := GetStyleSheet(AParam);
  end
  else
  if AKeyword = 'cs' then
  begin
    if FReadState.DestState = rdsStyleSheet then
    begin
      //FStyleSheet.Last.Number := AParam;
      //FStyleSheet.Last.StyleType := rsstChar;
      FReadState.DestState := rdsStyleSheetEntry;
    end;
  end
  else
  if AKeyword = 'paperw' then
    FPaperWidth := AParam
  else
  if AKeyword = 'paperh' then
    FPaperHeight := AParam
  else
  if AKeyword = 'margl' then
    FMarginLeft := AParam
  else
  if AKeyword = 'margr' then
    FMarginRight := AParam
  else
  if AKeyword = 'margt' then
    FMarginTop := AParam
  else
  if AKeyword = 'margb' then
    FMarginBottom := AParam
  else
  if AKeyword = 'sectd' then
  begin
    FData.Width := TwipToPixel(FPaperWidth, PixelsPerInchX)
      - TwipToPixel(FMarginLeft, PixelsPerInchX) - TwipToPixel(FMarginRight, PixelsPerInchX) ;

    if FReadState.DestState = rdsNorm then
      FSectionProp.Initialize;
  end
  else
  if AKeyword = 'headery' then
    FSectionProp.HeaderY := AParam
  else
  if AKeyword = 'footery' then
    FSectionProp.FooterY := AParam
  else
  if AKeyword = 'pard' then
    //FReadState.ParaStyle.Initialize
  else
  if AKeyword = 'plain' then
    FTextStyle.TempNo := AParam
  else
  if AKeyword = 'qj' then
  begin
    case AParam of
      0: FParaStyle.AlignHorz := pahLeft;
      1: FParaStyle.AlignHorz := pahRight;
      2: FParaStyle.AlignHorz := pahCenter;
    else
      FParaStyle.AlignHorz := pahJustify;
    end;
  end
  else
  if AKeyword = 'li' then
    FParaStyle.LeftIndent := AParam
  else
  if AKeyword = 'ri' then
    FParaStyle.RightIndent := AParam
  else
  if AKeyword = 'fs' then
    FTextStyle.Size := AParam / 2
  else
  if AKeyword = 'trowd' then  // 表格行开始
  begin
    if FReadState.DestState <> rdsTable then  // 第一次读到表格数据
    begin
      if FReadData.IsEmptyData then
      begin
        FReadData.Items.Clear;
        vParaFirst := True;
      end
      else
        vParaFirst := False;

      vTableItem := THCRtfTable.Create(FReadData, 1, 1, 108);
      vTableItem.ParaFirst := vParaFirst;
      vTableItem.RowStartRead := True;
      vTableItem.NewRowSize;
      //FData.InsertItem(FData.Items.Count, vTableItem);
      FReadData.Items.Add(vTableItem);
      FReadState.DestState := rdsTable;

      FDataStack.Push(FReadData);
      FReadData := vTableItem.Cells[0, 0].CellData;
    end;
  end
  else
  if (AKeyword = 'row') and (FReadState.DestState = rdsTable) then  // 行结束
  begin  // 删除此行最后新添加的单元格
    FReadData := FDataStack.Pop;
    vTableItem := FReadData.Items.Last as THCRtfTable;
    vTableItem.RowStartRead := False;
    vTableRow := vTableItem.Rows.Last;
    vTableRow.Delete(vTableRow.ColCount - 1);

    if not vTableItem.LastRow then
    begin
      vTableRow := THCTableRow.Create(FStyle, 1);
      vTableItem.Rows.Add(vTableRow);
      vTableItem.NewRowSize;
      vTableItem.RowHasReadCell := False;
      FDataStack.Push(FReadData);
      FReadData := vTableItem.Cells[vTableItem.RowCount - 1, vTableItem.Rows.Last.ColCount - 1].CellData;
    end
    else
      RebuildLastTable(FReadData);
  end
  else
  if (AKeyword = 'clwWidth') and (FReadState.DestState = rdsTable) then
  begin
    if (FDataStack.Count > 0) and (FDataStack.Peek.Items.Last is THCRtfTable) then
    begin
      vTableItem := FDataStack.Peek.Items.Last as THCRtfTable;
      if vTableItem.RowHasReadCell then
        vTableItem.RowSizes.Last.Add(TwipToPixel(AParam, PixelsPerInchX));
    end;
  end
  else
  if (AKeyword = 'lastrow') and (FReadState.DestState = rdsTable) then  // 表格最后一行读完了
  begin
    FReadData := FDataStack.Pop;
    vTableItem := FReadData.Items.Last as THCRtfTable;
    vTableItem.LastRow := True;
    FDataStack.Push(FReadData);
    FReadData := vTableItem.Cells[vTableItem.RowCount - 1, vTableItem.Rows.Last.ColCount - 1].CellData;
  end
  else
  if (AKeyword = 'clvertalt') and (FReadState.DestState = rdsTable) then
  begin
    {vTableItem := FData.Items.Last as THCRtfTable;
    case AParam of
      0: vTableItem.Rows.Last.Cols[vTableItem.Rows.Last.ColCount - 1].AlignVert := cavTop;
      1: vTableItem.Rows.Last.Cols[vTableItem.Rows.Last.ColCount - 1].AlignVert := cavBottom;
    else
      vTableItem.Rows.Last.Cols[vTableItem.Rows.Last.ColCount - 1].AlignVert := cavCenter;
    end;}
  end
  else
  if AKeyword = 'cell' then  // 单元格结束
  begin
    FReadData := FDataStack.Pop;
    vTableCell := THCTableCell.Create(FStyle);
    vTableItem := FReadData.Items.Last as THCRtfTable;
    vTableRow := vTableItem.Rows.Last;
    vTableItem.RowHasReadCell := True;
    vTableRow.Add(vTableCell);
    FDataStack.Push(FReadData);
    FReadData := vTableCell.CellData;
  end;
end;

{ THCRtfReadState }

constructor THCRtfReadState.Create;
begin
  DataType := rdtNor;
end;

{ THCRtfSectionProp }

procedure THCRtfSectionProp.Initialize;
begin
  ColumnCount := 1;
  HeaderY := 720;
  FooterY := 720;
  PageNumberX := 720;
  PageNumberY := 720;
end;

{ THCRtfTable }

constructor THCRtfTable.Create(const AOwnerData: THCCustomData;
  const ARowCount, AColCount, AWidth: Integer);
begin
  inherited;
  RowSizes := TObjectList<THCRtfRowSize>.Create;
  LastRow := False;
  RowStartRead := False;
  RowHasReadCell := False;
end;

destructor THCRtfTable.Destroy;
begin
  FreeAndNil(RowSizes);
  inherited;
end;

procedure THCRtfTable.MergeCellSameStyleItems;
var
  vR, vC: Integer;
begin
  for vR := 0 to Self.RowCount - 1 do
  begin
    for vC := 0 to Self.ColCount - 1 do
      MergeSameStyleItems(Self.Cells[vR, vC].CellData);
  end;
end;

procedure THCRtfTable.NewRowSize;
var
  vRowSize: THCRtfRowSize;
begin
  vRowSize := THCRtfRowSize.Create;
  RowSizes.Add(vRowSize);
end;

end.
