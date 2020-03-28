{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2019-3-11             }
{                                                       }
{            支持格式化的文档对象管理单元               }
{                                                       }
{*******************************************************}

{ 201903141706
    防止在表格上的变动，具体的单元格变动后没有引起高度变化，表格所在的Data不重新
    格式化造成FFormatHeightChange等没机会变为False
}

unit HCFormatData;

interface

uses
  Windows, SysUtils, Classes, Types, Graphics, HCStyle, HCCustomData, HCItem,
  HCTextStyle, HCParaStyle;

type
  THCFormatData = class(THCCustomData)
  private
    FWidth: Integer;
    FFormatCount,
    /// <summary> Item含行高的高度 </summary>
    FItemFormatHeight,
    /// <summary> 格式化的起始DrawItem </summary>
    FFormatStartDrawItemNo,
    FFormatStartTop,
    FFormatEndBottom,
    FLastFormatParaNo: Integer;
    /// <summary> 当次格式化DrawItem数量是否发生变化 </summary>
    FFormatDrawItemCountChange: Boolean;
    /// <summary> 当次格式化Data高度是否发生变化 </summary>
    FFormatHeightChange: Boolean;
    /// <summary> 多次格式化是否有变动，外部由此决定是否重新计算分页起始结束DrawItemNo </summary>
    FFormatChange: Boolean;

    FOnItemRequestFormat: TDataItemEvent;

    procedure FormatRange(const AStartDrawItemNo, ALastItemNo: Integer);

    procedure CalcItemFormatHeigh(const AItem: THCCustomItem);

    /// <summary> 转换指定Item指定Offs格式化为DItem </summary>
    /// <param name="AItemNo">指定的Item</param>
    /// <param name="AOffset">指定的格式化起始位置</param>
    /// <param name="AContentWidth">当前Data格式化宽度</param>
    /// <param name="APageContenBottom">当前页格式化底部位置</param>
    /// <param name="APos">起始位置</param>
    /// <param name="ALastDNo">起始DItemNo前一个值</param>
    /// <param name="vPageBoundary">数据页底部边界</param>
    procedure FormatItemToDrawItems(const AItemNo, AOffset, AFmtLeft, AFmtRight,
      AContentWidth: Integer; var APos: TPoint; var ALastDrawItemNo: Integer);
  protected
    /// <summary> 初始化格式化的相关参数适用于 201903141706 </summary>
    procedure FormatInit;

    /// <summary> 设置光标位置到指定的Item最后面 </summary>
    procedure ReSetSelectAndCaret(const AItemNo: Integer); overload;

    /// <summary> 设置光标位置到指定的Item指定位置 </summary>
    /// <param name="AItemNo">指定ItemNo</param>
    /// <param name="AOffset">指定位置</param>
    /// <param name="ANextWhenMid">如果此位置前后的DrawItem正好分行，True：后一个DrawItem前面，False：前一个后面</param>
    procedure ReSetSelectAndCaret(const AItemNo, AOffset: Integer;
      const ANextWhenMid: Boolean = False); overload; virtual;

    /// <summary> 当前Item对应的格式化起始Item和结束Item(段最后一个Item) </summary>
    /// <param name="AFirstItemNo">起始ItemNo</param>
    /// <param name="ALastItemNo">结束ItemNo</param>
    procedure GetFormatRange(var AFirstDrawItemNo, ALastItemNo: Integer); overload;

    /// <summary> 指定Item对应的格式化起始Item和结束Item(段最后一个Item) </summary>
    /// <param name="AFirstItemNo">起始ItemNo</param>
    /// <param name="ALastItemNo">结束ItemNo</param>
    procedure GetFormatRange(const AItemNo, AItemOffset: Integer;
      var AFirstDrawItemNo, ALastItemNo: Integer); overload;

    /// <summary> 获取格式化的第一个DrawItem </summary>
    function GetFormatFirstDrawItem(const AItemNo, AItemOffset: Integer): Integer; overload;
    /// <summary> 获取格式化的第一个DrawItem </summary>
    function GetFormatFirstDrawItem(const ADrawItemNo: Integer): Integer; overload;

    /// <summary> 格式化时，记录起始DrawItem和段最后的DrawItem </summary>
    /// <param name="AFirstItemNo"></param>
    procedure FormatPrepare(const AFirstDrawItemNo: Integer; const ALastItemNo: Integer = -1);

    // 分页时只是修改了Item的PageBreak属性并没有造成Item数量和高度变化，
    // 需要强制清除后面的DrawItem因分页额外增加的空间以便本次分页时处理
    /// AForceClearExtra 是否强制清除本次格式化范围下面的DrawItem额外空间
    /// <summary> 格式化指定范围内的DrawItem </summary>
    procedure ReFormatData(const AFirstDrawItemNo: Integer; const ALastItemNo: Integer = -1;
      const AExtraItemCount: Integer = 0; const AForceClearExtra: Boolean = False);
  public
    constructor Create(const AStyle: THCStyle); virtual;
    destructor Destroy; override;
    /// <summary> 重新格式化全部Item </summary>
    procedure ReFormat; virtual;
    /// <summary> 重新格式化当前段(用于修改了段缩进等) </summary>
    procedure ReFormatActiveParagraph; virtual;
    /// <summary> 重新格式化当前Item(用于仅修改当前Item属性或内容) </summary>
    procedure ReFormatActiveItem; virtual;
    procedure ItemRequestFormat(const AItem: THCCustomItem); virtual;
    procedure BeginFormat;
    procedure EndFormat(const AReformat: Boolean = True);
    property Width: Integer read FWidth write FWidth;
    property FormatStartDrawItemNo: Integer read FFormatStartDrawItemNo;
    property FormatHeightChange: Boolean read FFormatHeightChange;
    property FormatDrawItemCountChange: Boolean read FFormatDrawItemCountChange;
    property FormatChange: Boolean read FFormatChange write FFormatChange;
    property FormatCount: Integer read FFormatCount;
    property OnItemRequestFormat: TDataItemEvent read FOnItemRequestFormat write FOnItemRequestFormat;
  end;

implementation

{$I HCView.inc}

uses
  HCRectItem, HCDrawItem, HCCommon;

const
  FormatTextCut = 8192;  // 一次计算多少个字符的间距

{ THCFormatData }

procedure THCFormatData.BeginFormat;
begin
  Inc(FFormatCount);
end;

procedure THCFormatData.CalcItemFormatHeigh(const AItem: THCCustomItem);
begin
  if Style.TempStyleNo <> AItem.StyleNo then  // 减少不必要的重复Apply
  begin
    Style.ApplyTempStyle(AItem.StyleNo);
    FLastFormatParaNo := AItem.ParaNo;
    FItemFormatHeight := CalculateLineHeight(Style.TextStyles[AItem.StyleNo], Style.ParaStyles[AItem.ParaNo]);
  end
  else
  if FLastFormatParaNo <> AItem.ParaNo then
  begin
    FLastFormatParaNo := AItem.ParaNo;
    FItemFormatHeight := CalculateLineHeight(Style.TextStyles[AItem.StyleNo], Style.ParaStyles[AItem.ParaNo]);
  end;
end;

constructor THCFormatData.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  FFormatCount := 0;
  FFormatChange := False;
  FormatInit;
end;

destructor THCFormatData.Destroy;
begin

  inherited Destroy;
end;

procedure THCFormatData.EndFormat(const AReformat: Boolean = True);
begin
  if FFormatCount > 0 then
    Dec(FFormatCount);

  if (FFormatCount = 0) and AReformat then
    ReFormat;
end;

procedure THCFormatData.FormatRange(const AStartDrawItemNo, ALastItemNo: Integer);
var
  i, vStartItemNo, vStartOffset, vPrioDrawItemNo: Integer;
  vPos: TPoint;
  vParaStyle: THCParaStyle;
begin
  //FormatReady(AStartDrawItemNo, vPrioDrawItemNo, vPos);  // 格式化起始DrawItem序号和位置

  FFormatStartDrawItemNo := AStartDrawItemNo;

  // 获取起始DrawItem的上一个序号及格式化开始位置
  if AStartDrawItemNo > 0 then  // 不是第一个
  begin
    vPrioDrawItemNo := AStartDrawItemNo - 1;  // GetItemLastDrawItemNo(AStartItemNo - 1);  // 上一个最后的DrawItem
    vStartItemNo := DrawItems[AStartDrawItemNo].ItemNo;
    vStartOffset := DrawItems[AStartDrawItemNo].CharOffs;
    vParaStyle := Style.ParaStyles[Items[vStartItemNo].ParaNo];
    if DrawItems[AStartDrawItemNo].LineFirst then  // 要格式化行首
    begin
      vPos.X := vParaStyle.LeftIndentPix;
      vPos.Y := DrawItems[vPrioDrawItemNo].Rect.Bottom;
    end
    else
    begin
      vPos.X := DrawItems[vPrioDrawItemNo].Rect.Right;
      vPos.Y := DrawItems[vPrioDrawItemNo].Rect.Top;
    end;
  end
  else  // 是第一个
  begin
    vPrioDrawItemNo := -1;
    vStartItemNo := 0;
    vStartOffset := 1;
    vParaStyle := Style.ParaStyles[Items[vStartItemNo].ParaNo];
    vPos.X := vParaStyle.LeftIndentPix;
    vPos.Y := 0;
  end;

  Style.ApplyTempStyle(THCStyle.Null);
  FormatItemToDrawItems(vStartItemNo, vStartOffset, vParaStyle.LeftIndentPix,
    FWidth - vParaStyle.RightIndentPix, FWidth, vPos, vPrioDrawItemNo);

  for i := vStartItemNo + 1 to ALastItemNo do  // 格式化
  begin
    if Items[i].ParaFirst then
    begin
      vParaStyle := Style.ParaStyles[Items[i].ParaNo];
      vPos.X := vParaStyle.LeftIndentPix;
    end;

    FormatItemToDrawItems(i, 1, vParaStyle.LeftIndentPix,
      FWidth - vParaStyle.RightIndentPix, FWidth, vPos, vPrioDrawItemNo);
  end;

  DrawItems.DeleteFormatMark;
end;

procedure THCFormatData.FormatPrepare(const AFirstDrawItemNo: Integer; const ALastItemNo: Integer = -1);
var
  i, vLastDrawItemNo, vLastItemNo, vFmtTopOffset: Integer;
begin
  if FFormatCount <> 0 then Exit;

  FormatInit;

  if AFirstDrawItemNo > 0 then
    Assert(DrawItems[AFirstDrawItemNo].LineFirst, '行中格式化必需从行首开始，否则会影响分散的计算！');

  //vFirstDrawItemNo := Items[AStartItemNo].FirstDItemNo;
  if ALastItemNo < 0 then
    vLastItemNo := DrawItems[AFirstDrawItemNo].ItemNo
  else
    vLastItemNo := ALastItemNo;

  vLastDrawItemNo := GetItemLastDrawItemNo(vLastItemNo);
  DrawItems.MarkFormatDelete(AFirstDrawItemNo, vLastDrawItemNo);

  if AFirstDrawItemNo > 0 then
  begin
    FFormatStartTop := DrawItems[AFirstDrawItemNo - 1].Rect.Bottom;
    vFmtTopOffset := DrawItems[AFirstDrawItemNo].Rect.Top - FFormatStartTop;
  end
  else
  begin
    FFormatStartTop := 0;
    vFmtTopOffset := 0;
  end;

  for i := AFirstDrawItemNo + 1 to vLastDrawItemNo do
  begin
    if DrawItems[i].LineFirst then
      vFmtTopOffset := vFmtTopOffset + DrawItems[i].Rect.Top - DrawItems[i - 1].Rect.Bottom;
  end;

  if vFmtTopOffset <> 0 then  // 有偏移肯定要重新格式化
    FFormatEndBottom := -1
  else
    FFormatEndBottom := DrawItems[vLastDrawItemNo].Rect.Bottom - vFmtTopOffset;
end;

procedure THCFormatData.FormatInit;
begin
  FFormatHeightChange := False;
  FFormatDrawItemCountChange := False;
  FFormatStartTop := 0;
  FFormatEndBottom := 0;
  FFormatStartDrawItemNo := -1;
  FLastFormatParaNo := THCStyle.Null;
end;

procedure THCFormatData.FormatItemToDrawItems(const AItemNo, AOffset, AFmtLeft,
  AFmtRight, AContentWidth: Integer; var APos: TPoint;
  var ALastDrawItemNo: Integer);
type
  TBreakPosition = (  // 截断位置
    /// <summary> 不截断 </summary>
    jbpNone,
    /// <summary> 在前一个后面截断 </summary>
    jbpPrev
    //jbpCur    // 在当前后面截断
    );

var
  vParaStyle: THCParaStyle;

  {$REGION 'FinishLine'}
  /// <summary> 重整行 </summary>
  /// <param name="AEndDItemNo">行最后一个DItem</param>
  /// <param name="ARemWidth">行剩余宽度</param>
  procedure FinishLine(const ALineEndDItemNo, ARemWidth: Integer);
  var
    i, vLineBegDItemNo,  // 行第一个DItem
    vMaxBottom,
    viSplitW, vW,
    vLineSpaceCount,   // 当前行分几份
    vDItemSpaceCount,  // 当前DrawItem分几份
    vExtraW,
    viSplitMod,
    vDLen
      : Integer;
    vDrawItemSplitCounts: array of Word;  // 当前行各DItem分几份
  begin
    { 根据行中最高的DrawItem处理其他DrawItem的高度 }
    vLineBegDItemNo := ALineEndDItemNo;
    for i := ALineEndDItemNo downto 0 do  // 得到行起始DItemNo
    begin
      if DrawItems[i].LineFirst then
      begin
        vLineBegDItemNo := i;
        Break;
      end;
    end;
    Assert((vLineBegDItemNo >= 0), '断言失败：行起始DrawItemNo小于0！');
    // 找行DrawItem中最高的
    vMaxBottom := DrawItems[ALineEndDItemNo].Rect.Bottom;  // 先默认行最后一个DItem的Rect底位置最大
    for i := ALineEndDItemNo - 1 downto vLineBegDItemNo do
    begin
      if DrawItems[i].Rect.Bottom > vMaxBottom then
        vMaxBottom := DrawItems[i].Rect.Bottom;  // 记下最大的Rect底位置
    end;

    // 根据最高的处理行间距，并影响到同行DrawItem
    for i := ALineEndDItemNo downto vLineBegDItemNo do
      DrawItems[i].Rect.Bottom := vMaxBottom;

    // 处理对齐方式，放在这里，是因为方便计算行起始和结束DrawItem，避免绘制时的运算
    case vParaStyle.AlignHorz of  // 段内容水平对齐方式
      pahLeft: ;  // 默认

      pahRight:
        begin
          for i := ALineEndDItemNo downto vLineBegDItemNo do
            OffsetRect(DrawItems[i].Rect, ARemWidth, 0);
        end;

      pahCenter:
        begin
          viSplitW := ARemWidth div 2;
          for i := ALineEndDItemNo downto vLineBegDItemNo do
            OffsetRect(DrawItems[i].Rect, viSplitW, 0);
        end;

      pahJustify, pahScatter:  // 20170220001 两端、分散对齐相关处理
        begin
          if vParaStyle.AlignHorz = pahJustify then  // 两端对齐
          begin
            if IsParaLastDrawItem(ALineEndDItemNo) then  // 两端对齐，段最后一行不处理
              Exit;
          end
          else  // 分散对齐，空行或只有一个字符时居中
          begin
            if vLineBegDItemNo = ALineEndDItemNo then  // 行只有一个DrawItem
            begin
              if Items[DrawItems[vLineBegDItemNo].ItemNo].Length < 2 then  // 此DrawItem对应的内容长度不足2个按居中处理
              begin
                viSplitW := ARemWidth div 2;
                OffsetRect(DrawItems[vLineBegDItemNo].Rect, viSplitW, 0);

                Exit;
              end;
            end;
          end;

          vLineSpaceCount := 0;
          vExtraW := 0;
          viSplitMod := 0;
          viSplitW := ARemWidth;
          SetLength(vDrawItemSplitCounts, ALineEndDItemNo - vLineBegDItemNo + 1);
          for i := vLineBegDItemNo to ALineEndDItemNo do  // 计算空余分给几个DrawItem
          begin
            if GetDrawItemStyle(i) < THCStyle.Null then  // RectItem  相互关联 201903261121
            begin
              if (Items[DrawItems[i].ItemNo] as THCCustomRectItem).JustifySplit
                and (vLineBegDItemNo <> ALineEndDItemNo)  // 此行不止一个Item
              then  // 分散对齐占间距
              begin
                if i <> ALineEndDItemNo then
                  vDItemSpaceCount := 1  // Graphic等占间距
                else
                  vDItemSpaceCount := 0;
              end
              else
                vDItemSpaceCount := 0; // Tab等不占间距
            end
            else  // TextItem
            begin
              vDItemSpaceCount := GetJustifyCount(GetDrawItemText(i), nil);  // 当前DrawItem分了几份
              if (i = ALineEndDItemNo) and (vDItemSpaceCount > 0) then  // 行尾的DrawItem，少分一个
                Dec(vDItemSpaceCount);
            end;

            vDrawItemSplitCounts[i - vLineBegDItemNo] := vDItemSpaceCount;  // 记录当前DrawItem分几份
            vLineSpaceCount := vLineSpaceCount + vDItemSpaceCount;  // 记录行内总共分几份
          end;

          if vLineSpaceCount > 1 then  // 份数大于1
          begin
            viSplitW := ARemWidth div vLineSpaceCount;  // 每一份的大小
            viSplitMod := ARemWidth mod vLineSpaceCount;  // 余数
          end;

          { 行中第一个DrawItem左不动所以单独处理增加的空间 }
          if vDrawItemSplitCounts[0] > 0 then
          begin
            DrawItems[vLineBegDItemNo].Rect.Right := DrawItems[vLineBegDItemNo].Rect.Right
              + vDrawItemSplitCounts[0] * viSplitW;

            if viSplitMod > 0 then  // 额外的没有分完
            begin
              vDLen := DrawItems[vLineBegDItemNo].CharLen;
              if viSplitMod > vDLen then  // 足够分
              begin
                Inc(DrawItems[vLineBegDItemNo].Rect.Right, vDLen);  // 当前DrawItem多分一个像素
                Dec(viSplitMod, vDLen);  // 额外的减少一个像素
              end
              else  // 不够分
              begin
                Inc(DrawItems[vLineBegDItemNo].Rect.Right, viSplitMod);  // 当前DrawItem多分一个像素
                viSplitMod := 0;;  // 额外的减少一个像素
              end;
            end;
          end;

          for i := vLineBegDItemNo + 1 to ALineEndDItemNo do  // 以第一个为基准，其余各DrawItem增加的空间
          begin
            vW := DrawItems[i].Width;  // DrawItem原来Width
            if vDrawItemSplitCounts[i - vLineBegDItemNo] > 0 then  // 有分到间距
            begin
              vExtraW := vDrawItemSplitCounts[i - vLineBegDItemNo] * viSplitW;  // 多分到的width
              if viSplitMod > 0 then  // 额外的没有分完
              begin
                if GetDrawItemStyle(i) < THCStyle.Null then
                begin
                  if (Items[DrawItems[i].ItemNo] as THCCustomRectItem).JustifySplit then
                  begin
                    Inc(vExtraW);  // 当前DrawItem多分一个像素
                    Dec(viSplitMod);  // 额外的减少一个像素
                  end;
                end
                else
                begin
                  vDLen := DrawItems[i].CharLen;
                  if viSplitMod > vDLen then  // 足够分
                  begin
                    Inc(vExtraW, vDLen);  // 当前DrawItem多分像素
                    Dec(viSplitMod, vDLen);  // 额外的减少像素
                  end
                  else  // 不够分
                  begin
                    Inc(vExtraW, viSplitMod);  // 当前DrawItem多分
                    viSplitMod := 0;;  // 额外的都给了这个
                  end;
                end;
              end;
            end
            else  // 没有分到间距
              vExtraW := 0;

            DrawItems[i].Rect.Left := DrawItems[i - 1].Rect.Right;
            DrawItems[i].Rect.Right := DrawItems[i].Rect.Left + vW + vExtraW;
          end;
        end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'NewDrawItem'}
  procedure NewDrawItem(const AItemNo, ACharOffs, ACharLen: Integer;
    const ARect: TRect; const AParaFirst, ALineFirst: Boolean);
  var
    vDrawItem: THCCustomDrawItem;
  begin
    vDrawItem := THCCustomDrawItem.Create;
    vDrawItem.ItemNo := AItemNo;
    vDrawItem.CharOffs := ACharOffs;
    vDrawItem.CharLen := ACharLen;
    vDrawItem.ParaFirst := AParaFirst;
    vDrawItem.LineFirst := ALineFirst;
    vDrawItem.Rect := ARect;
    Inc(ALastDrawItemNo);
    DrawItems.Insert(ALastDrawItemNo, vDrawItem);
    if ACharOffs = 1 then
      Items[AItemNo].FirstDItemNo := ALastDrawItemNo;
  end;
  {$ENDREGION}

  {$REGION 'FindLineBreak'}
  /// <summary> 获取字符串排版时截断到下一行的位置 </summary>
  /// <param name="AText"></param>
  /// <param name="AStartPos"> 第X个字符 X > 0 </param>
  /// <param name="APos"> 在第Y个后面断开 Y > 0 </param>
  procedure FindLineBreak(const AText: string; const AStartPos: Integer; var APos: Integer);

    {$REGION 'GetHeadTailBreak 根据行首、尾对字符的约束条件，获取截断位置'}
    procedure GetHeadTailBreak(const AText: string; var APos: Integer);
    var
      vChar: Char;
    begin
      if APos < 1 then Exit;

      vChar := AText[APos + 1];  // 因为是要处理截断，所以APos肯定是小于Length(AText)的，不用考虑越界
      if PosCharHC(vChar, DontLineFirstChar) > 0 then  // 下一个是不能放在行首的字符
      begin
        Dec(APos);  // 当前要移动到下一行，往前一个截断重新判断
        GetHeadTailBreak(AText, APos);
      end
      else  // 下一个可以放在行首，当前位置能否放置到行尾
      begin
        vChar := AText[APos];  // 当前位置字符
        if PosCharHC(vChar, DontLineLastChar) > 0 then  // 是不能放在行尾的字符
        begin
          Dec(APos);  // 再往前寻找截断位置
          GetHeadTailBreak(AText, APos);
        end;
      end;
    end;
    {$ENDREGION}

    function MatchBreak(const APrevType, APosType: TCharType; const AIndex: Integer): TBreakPosition;
    begin
      Result := jbpNone;
      case APosType of
        jctHZ:
          begin
            if APrevType in [jctZM, jctSZ, jctHZ, jctFH] then  // 当前位置是汉字，前一个是字母、数字、汉字
              Result := jbpPrev;
          end;

        jctZM:
          begin
            if not (APrevType in [jctZM, jctSZ]) then  // 当前是字母，前一个不是数字、字母
              Result := jbpPrev;
          end;

        jctSZ:
          begin
            case APrevType of
              jctZM, jctSZ: ;  // 当前是数字，前一个是字母、数字，不截断
              jctFH:
                begin
                  if AText[AIndex - 1] = '￠' then

                  else
                  if not CharInSet(AText[AIndex - 1], ['.', ':', '-', '^', '*', '/']) then  // 数字前面是小数点、冒号等数学符号时不截断
                    Result := jbpPrev;
                end;
            else
              Result := jbpPrev;
            end;
          end;

        jctFH:
          begin
            case APrevType of
              jctFH: ;  // 当前是符号，前一个是符号
              jctSZ:  // 当前是符号，前一个是数字
                begin
                  if not CharInSet(AText[AIndex], ['.', ':', '-', '^', '*', '/']) then  // 前面是数字，我不是小数点，时间:
                    Result := jbpPrev;
                end;
              jctZM:  // 当前是符号，前一个是字母
                begin
                  if not CharInSet(AText[AIndex], [':']) then  // 前一个是字母，当前不是冒号
                    Result := jbpPrev;
                end
            else
              Result := jbpPrev;
            end;
          end;
      end;
    end;

  var
    i: Integer;
    vPosCharType, vPrevCharType, vNextCharType: TCharType;
    vFindBreak: Boolean;
  begin
    GetHeadTailBreak(AText, APos);  // 根据行首、尾的约束条件找APos不符合时应该在哪一个位置并重新赋值给APos
    if APos < 1 then Exit;
    if vParaStyle.BreakRough then Exit;

    vPosCharType := GetUnicodeCharType(AText[APos]);  // 当前类型
    vNextCharType := GetUnicodeCharType(AText[APos + 1]);  // 下一个字符类型

    if MatchBreak(vPosCharType, vNextCharType, APos + 1) <> jbpPrev then  // 不能在当前截断，当前往前找截断
    begin
      if vPosCharType <> jctBreak then
      begin
        vFindBreak := False;
        for i := APos - 1 downto AStartPos do
        begin
          vPrevCharType := GetUnicodeCharType(AText[i]);
          if MatchBreak(vPrevCharType, vPosCharType, i + 1) = jbpPrev then
          begin
            APos := i;
            vFindBreak := True;
            Break;
          end;

          vPosCharType := vPrevCharType;
        end;

        if not vFindBreak then  // 没找到
          APos := 0;
      end;
    end;
  end;
  {$ENDREGION}

var
  vText: string;
  vRect: TRect;
  vItemLen,  // 文本Item字符串长度
  vRemainderWidth
    : Integer;
  vItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
  vParaFirst, vLineFirst: Boolean;
  vCharWidths: array of Integer;

  {$REGION ' DoFormatRectItemToDrawItem格式化RectItem '}
  procedure DoFormatRectItemToDrawItem;
  var
    vWidth: Integer;
  begin
    vRectItem.FormatToDrawItem(Self, AItemNo);
    vWidth := AFmtRight - APos.X;
    if (vRectItem.Width > vWidth) and (not vLineFirst) then  // 当前行剩余宽度放不下且不是行首
    begin
      // 偏移到下一行
      FinishLine(ALastDrawItemNo, vWidth);
      APos.X := AFmtLeft;
      APos.Y := DrawItems[ALastDrawItemNo].Rect.Bottom;
      vLineFirst := True;  // 作为行首
    end;

    // 当前行空余宽度能放下或放不下但已经是行首了
    vRect.Left := APos.X;
    vRect.Top := APos.Y;
    vRect.Right := vRect.Left + vRectItem.Width;
    vRect.Bottom := vRect.Top + vRectItem.Height + Style.LineSpaceMin;  // 带上行间距
    NewDrawItem(AItemNo, AOffset, 1, vRect, vParaFirst, vLineFirst);

    vRemainderWidth := AFmtRight - vRect.Right;  // 放入后的剩余量
  end;
  {$ENDREGION}

  procedure _FormatBreakTextDrawItem(const ADrawItemNo: Integer);
  var
    vDrawItem: THCCustomDrawItem;
    vItemBK: THCCustomItem;
    vWidth, vLen: Integer;
  begin
    // 注意：调用此方法前请确定 DrawItems[ADrawItemNo].CharLen > 1
    vDrawItem := DrawItems[ADrawItemNo];
    vItemBK := Items[vDrawItem.ItemNo];
    vLen := Length(vItemBK.Text);

    CalcItemFormatHeigh(vItemBK);

    vWidth := Style.TempCanvas.TextWidth(vItemBK.Text[vLen]);
    // 分裂前
    Dec(vDrawItem.CharLen, 1);
    Dec(vDrawItem.Rect.Right, vWidth);
    vRemainderWidth := AFmtRight - vDrawItem.Rect.Right;
    FinishLine(ADrawItemNo, vRemainderWidth);
    // 分裂后
    APos.X := AFmtLeft;
    APos.Y := vDrawItem.Rect.Bottom;
    vRect.Left := APos.X;
    vRect.Top := APos.Y;
    vRect.Right := vRect.Left + vWidth;
    vRect.Bottom := vRect.Top + FItemFormatHeight;
    NewDrawItem(vDrawItem.ItemNo, vLen, 1, vRect, False{ParaFirst}, True{LineFirst});
    vParaFirst := False;
    APos.X := vRect.Right;

    vRemainderWidth := AFmtRight - vRect.Right;  // 放入最多后的剩余量
  end;

  {$REGION ' DoFormatTextItemToDrawItems从指定偏移和指定位置开始格式化Text '}
  /// <summary> 从指定偏移和指定位置开始格式化Text </summary>
  /// <param name="ACharOffset">文本格式化的起始偏移</param>
  /// <param name="APlaceWidth">呈放文本的宽度</param>
  /// <param name="ABasePos">vCharWidths中对应偏移的起始位置</param>
  procedure DoFormatTextItemToDrawItems(const ACharOffset, APlaceWidth, ABasePos: Integer);
  var
    i, viPlaceOffset,  // 能放下第几个字符
    viBreakOffset,  // 第几个字符放不下
    vFirstCharWidth  // 第一个字符的宽度
      : Integer;

    vSqueeze: Boolean;
  begin
    vLineFirst := vParaFirst or ((APos.X = AFmtLeft) and (DrawItems[ALastDrawItemNo].Width <> 0));  // 段首、最左但不是域后面
    viBreakOffset := 0;  // 换行位置，第几个字符放不下
    vFirstCharWidth := vCharWidths[ACharOffset - 1] - ABasePos;  // 第一个字符的宽度

    if APlaceWidth < 0 then  // 左缩进超过右边距
      viBreakOffset := 1
    else
    begin
      if Style.FormatVersion = 2 then
      begin
        vSqueeze := False;
        for i := ACharOffset - 1 to vItemLen - 1 do
        begin
          if vCharWidths[i] - ABasePos > APlaceWidth then
          begin
            if PosCharHC(vText[i + 1], LineSqueezeChar) > 0 then
            begin
              if vCharWidths[i] - ABasePos - APlaceWidth > 3 then  // (vCharWidths[i] - vCharWidths[i - 1]) div 2 then
              begin
                vSqueeze := True;
                Continue;
              end;
            end;

            viBreakOffset := i + 1;
            Break;
          end;
        end;
      end
      else
      begin
        for i := ACharOffset - 1 to vItemLen - 1 do
        begin
          if vCharWidths[i] - ABasePos > APlaceWidth then
          begin
            viBreakOffset := i + 1;
            Break;
          end;
        end;
      end;
    end;

    if viBreakOffset < 1 then  // 当前行剩余空间把vText全放置下了
    begin
      vRect.Left := APos.X;
      vRect.Top := APos.Y;
      vRect.Right := vRect.Left + vCharWidths[vItemLen - 1] - ABasePos;  // 使用自定义测量的结果
      vRect.Bottom := vRect.Top + FItemFormatHeight;
      NewDrawItem(AItemNo, AOffset + ACharOffset - 1, vItemLen - ACharOffset + 1, vRect, vParaFirst, vLineFirst);
      vParaFirst := False;

      vRemainderWidth := AFmtRight - vRect.Right;  // 放入最多后的剩余量
    end
    else
    if viBreakOffset = 1 then  // 当前行剩余空间连第一个字符也放不下(第一次处理此Item就一个也放不下)
    begin
      if vFirstCharWidth > AFmtRight - AFmtLeft then  // Data的格式化宽度不足一个字符(强制保持在此行)
      begin
        vRect.Left := APos.X;
        vRect.Top := APos.Y;
        vRect.Right := vRect.Left + vCharWidths[vItemLen - 1] - ABasePos;  // 使用自定义测量的结果
        vRect.Bottom := vRect.Top + FItemFormatHeight;
        NewDrawItem(AItemNo, AOffset + ACharOffset - 1, 1, vRect, vParaFirst, vLineFirst);
        vParaFirst := False;

        vRemainderWidth := AFmtRight - vRect.Right;  // 放入最多后的剩余量
        FinishLine(ALastDrawItemNo, vRemainderWidth);

        // 偏移到下一行顶端，准备另起一行
        APos.X := AFmtLeft;
        APos.Y := DrawItems[ALastDrawItemNo].Rect.Bottom;  // 不使用 vRect.Bottom 因为如果行中间有高的，会修正vRect.Bottom

        {if viBreakOffset < viLen then  改用下面的，解决左缩进超过右边距时出错
          DoFormatTextItemToDrawItems(viBreakOffset + 1, AFmtRight - APos.X,
            vCharWidths[viBreakOffset - 1]);}
        if ACharOffset < vItemLen then
          DoFormatTextItemToDrawItems(ACharOffset + 1, AFmtRight - APos.X,
            vCharWidths[ACharOffset - 1]);
      end
      else  // Data的整体宽度足够一个字符(第一次格式化此Item就出现剩余空间连第一个字符都放不下)
      if (PosCharHC(vText[ACharOffset], DontLineFirstChar) > 0) // (肯定不是处理段第一行了)要下移的字符串第一个是不能放在行首的字符
        and (Items[AItemNo - 1].StyleNo > THCStyle.Null)  // 前一个是文本 201902212125.hcf 第二行逗号
        and (DrawItems[ALastDrawItemNo].CharLen > 1)  // 前一个不止一个字符
      then
      begin  // 连带前一个Item的最后字符
        _FormatBreakTextDrawItem(ALastDrawItemNo);  // 上一个重新分裂
        CalcItemFormatHeigh(vItem);  // 恢复行高

        DoFormatTextItemToDrawItems(ACharOffset, AFmtRight - APos.X, ABasePos);
      end
      else  // 整体下移到下一行
      begin
        vRemainderWidth := APlaceWidth;
        FinishLine(ALastDrawItemNo, vRemainderWidth);
        APos.X := AFmtLeft;  // 偏移到下一行开始计算
        APos.Y := DrawItems[ALastDrawItemNo].Rect.Bottom;
        DoFormatTextItemToDrawItems(ACharOffset, AFmtRight - APos.X, ABasePos);
      end;
    end
    else  // 当前行剩余宽度能放下当前Text的一部分
    begin
      if vFirstCharWidth > AFmtRight - AFmtLeft then  // Data的格式化宽度不足一个字符(强制在此位置)
        viPlaceOffset := viBreakOffset
      else
        viPlaceOffset := viBreakOffset - 1;  // 第viBreakOffset个字符放不下，前一个能放下

      FindLineBreak(vText, ACharOffset, viPlaceOffset);  // 判断从viPlaceOffset后打断是否合适

      if (viPlaceOffset = 0)  // 没找到合适的截断位置
        //and (not vLineFirst)  // 不是行第一个DrawItem，才考虑往下换行（用下面的判断代替了）
        and (APos.X > AFmtLeft)  // 如果此DrawItem不是行首但排版位置是从最左边开始(行首是0宽RectItem)，
      then                       // 整体下移后的排版计算仍会是最左边，所以只对不在最左边开始排版的才下移
      begin                      // 避免当前行排版不放东西又往下一行，死循环
        vRemainderWidth := APlaceWidth;
        FinishLine(ALastDrawItemNo, vRemainderWidth);
        APos.X := AFmtLeft;  // 偏移到下一行开始计算
        APos.Y := DrawItems[ALastDrawItemNo].Rect.Bottom;
        DoFormatTextItemToDrawItems(ACharOffset, AFmtRight - APos.X, ABasePos);
      end
      else  // 有适合放到当前行的内容
      begin
        if viPlaceOffset < ACharOffset then  // 找不到截断位置，就在原位置截断(如整行文本都是逗号)
        begin
          if vFirstCharWidth > AFmtRight - AFmtLeft then  // Data的格式化宽度不足一个字符(强制在此位置)
            viPlaceOffset := viBreakOffset
          else
            viPlaceOffset := viBreakOffset - 1;
        end;

        vRect.Left := APos.X;
        vRect.Top := APos.Y;
        vRect.Right := vRect.Left + vCharWidths[viPlaceOffset - 1] - ABasePos;  // 使用自定义测量的结果
        vRect.Bottom := vRect.Top + FItemFormatHeight;

        NewDrawItem(AItemNo, AOffset + ACharOffset - 1, viPlaceOffset - ACharOffset + 1, vRect, vParaFirst, vLineFirst);
        vParaFirst := False;

        if vSqueeze then
          vRemainderWidth := 0
        else
          vRemainderWidth := AFmtRight - vRect.Right;  // 放入最多后的剩余量

        FinishLine(ALastDrawItemNo, vRemainderWidth);

        // 偏移到下一行顶端，准备另起一行
        APos.X := AFmtLeft;
        APos.Y := DrawItems[ALastDrawItemNo].Rect.Bottom;  // 不使用 vRect.Bottom 因为如果行中间有高的，会修正vRect.Bottom

        if viPlaceOffset < vItemLen then
          DoFormatTextItemToDrawItems(viPlaceOffset + 1, AFmtRight - APos.X, vCharWidths[viPlaceOffset - 1]);
      end;
    end;
  end;
  {$ENDREGION}

var
  vSize: TSize;
  //vPoints: array[0..1] of TPoint;
  //vTextMetric: TTextMetric;
  i, vIndex, viBase, viLen: Integer;
  vCharWArr: array of Integer;  // 每个字符绘制结束位置
begin
  if not Items[AItemNo].Visible then Exit;

  vRemainderWidth := 0;
  vItem := Items[AItemNo];
  vParaStyle := Style.ParaStyles[vItem.ParaNo];

  if vItem.ParaFirst and (AOffset = 1) then  // 第一次处理段第一个Item
  begin
    vParaFirst := True;
    vLineFirst := True;
    Inc(APos.X, vParaStyle.FirstIndentPix);
  end
  else  // 非段第1个
  begin
    vParaFirst := False;
    vLineFirst := (APos.X = AFmtLeft) and (DrawItems[ALastDrawItemNo].Width <> 0);  // 最左但不是域后面
  end;

  if not vItem.Visible then  // 不显示的Item
  begin
    vRect.Left := APos.X;
    vRect.Top := APos.Y;
    vRect.Right := vRect.Left;
    vRect.Bottom := vRect.Top + 5;  // 带上行间距
    NewDrawItem(AItemNo, AOffset, vItem.Length, vRect, vParaFirst, vLineFirst);
  end
  else
  if vItem.StyleNo < THCStyle.Null then  // 是RectItem
  begin
    vRectItem := vItem as THCCustomRectItem;
    DoFormatRectItemToDrawItem;
    // 如果进入表格前是样式1，进入表格里有把Style的全局TempStyleNo改成0，表格后面
    // 是样式0的格式化时，由于此时Data的FItemFormatHeight还是样式1的，应用样式0的
    // StyleNo时和全局的并没有变化，并不能应用修改FItemFormatHeight，所以需要清除一下。
    Style.ApplyTempStyle(THCStyle.Null);
  end
  else  // 文本
  begin
    CalcItemFormatHeigh(vItem);
    vRemainderWidth := AFmtRight - APos.X;

    if AOffset <> 1 then
      vText := Copy(vItem.Text, AOffset, vItem.Length - AOffset + 1)
    else
      vText := vItem.Text;

    if vText = '' then  // 空item(肯定是空行)
    begin
      {$IFDEF CHECKNULLITEM}
      Assert(vItem.ParaFirst, HCS_EXCEPTION_NULLTEXT);
      {$ENDIF}
      vRect.Left := APos.X;
      vRect.Top := APos.Y;
      vRect.Right := vRect.Left;
      vRect.Bottom := vRect.Top + FItemFormatHeight;  //DefaultCaretHeight;
      vParaFirst := True;
      vLineFirst := True;
      NewDrawItem(AItemNo, AOffset, 0, vRect, vParaFirst, vLineFirst);
      vParaFirst := False;
    end
    else  // 非空Item
    begin
      vItemLen := Length(vText);

      //if vItemLen > 38347922 then  // 65535
      //  raise Exception.Create(HCS_EXCEPTION_STRINGLENGTHLIMIT);

      SetLength(vCharWidths, vItemLen);

      viLen := vItemLen;
      if viLen > FormatTextCut then
        SetLength(vCharWArr, FormatTextCut);

      vIndex := 0;
      viBase := 0;
      while viLen > FormatTextCut do
      begin
        GetTextExtentExPoint(Style.TempCanvas.Handle, PChar(Copy(vText, vIndex + 1, FormatTextCut)), FormatTextCut, 0,
          nil, PInteger(vCharWArr), vSize);  // 超过65535数组元素取不到值

        for i := 0 to FormatTextCut - 1 do
          vCharWidths[vIndex + i] := vCharWArr[i] + viBase;

        Dec(viLen, FormatTextCut);
        Inc(vIndex, FormatTextCut);
        viBase := vCharWidths[vIndex - 1];
      end;

      SetLength(vCharWArr, viLen);
      GetTextExtentExPoint(Style.TempCanvas.Handle, PChar(Copy(vText, vIndex + 1, viLen)), viLen, 0,
        nil, PInteger(vCharWArr), vSize);  // 超过65535数组元素取不到值

      for i := 0 to viLen - 1 do
        vCharWidths[vIndex + i] := vCharWArr[i] + viBase;

      SetLength(vCharWArr, 0);

      DoFormatTextItemToDrawItems(1, AFmtRight - APos.X, 0);

      SetLength(vCharWidths, 0);
    end;
  end;

  // 计算下一个的位置
  if AItemNo = Items.Count - 1 then  // 是最后一个
    FinishLine(ALastDrawItemNo, vRemainderWidth)
  else  // 不是最后一个，则为下一个Item准备位置
  begin
    if Items[AItemNo + 1].ParaFirst then // 下一个是段起始
    begin
      FinishLine(ALastDrawItemNo, vRemainderWidth);
      // 偏移到下一行顶端，准备另起一行
      APos.X := 0;
      APos.Y := DrawItems[ALastDrawItemNo].Rect.Bottom;  // 不使用 vRect.Bottom 因为如果行中间有高的，会修正其bottom
    end
    else  // 下一个不是段起始
      APos.X := vRect.Right;  // 下一个的起始坐标
  end;
end;

function THCFormatData.GetFormatFirstDrawItem(const AItemNo, AItemOffset: Integer): Integer;
var
  vDrawItemNo: Integer;
begin
  vDrawItemNo := GetDrawItemNoByOffset(AItemNo, AItemOffset);
  Result := GetFormatFirstDrawItem(vDrawItemNo);
end;

function THCFormatData.GetFormatFirstDrawItem(const ADrawItemNo: Integer): Integer;
begin
  Result := ADrawItemNo;
  if not DrawItems[Result].ParaFirst then
  begin
    if DrawItems[Result].LineFirst then  // 行首变化会引起上一行最后换行的计算并影响行高
      Dec(Result);

    while Result > 0 do
    begin
      if DrawItems[Result].LineFirst then
        Break
      else
        Dec(Result);
    end;
  end;
end;

procedure THCFormatData.GetFormatRange(var AFirstDrawItemNo, ALastItemNo: Integer);
begin
  GetFormatRange(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, AFirstDrawItemNo, ALastItemNo);
end;

procedure THCFormatData.GetFormatRange(const AItemNo, AItemOffset: Integer;
  var AFirstDrawItemNo, ALastItemNo: Integer);
begin
  if FFormatCount <> 0 then Exit;
  AFirstDrawItemNo := GetFormatFirstDrawItem(AItemNo, AItemOffset);
  ALastItemNo := GetParaLastItemNo(AItemNo);

  // 行起始为TextItem，同一行后面有RectItem时，编辑TextItem后格式化可能会将RectItem分到下一行，
  // 所以不能直接 FormatItemPrepare(SelectInfo.StartItemNo)否则会因为格式化范围太小，
  // 没有进行FiniLine调整行高，所以从段最后或行最后开始

  // 如果Item分多行，在非起始位置行修改，从起始位置格式化时，起始位置和前面的原来
  // 因分散附加了宽度，所以应该从起始位置所在行首ItemNo开始格式化，否则起始位置格式化时
  // 其前面Item有上一次分散附加的宽度，会造起始位置格式化宽度不正确，造成分行不准确
  // 这样的设计，是支持数据格式化时指定ItemNo和Offset了
  //
  // 如果格式化位置在行首且是ItemB起始，上一行结束是另一ItemA，当插入文本时可和ItemA合并，
  // 需要从ItemA开始格式化
//  if (AItemNo > 0)
//    and DrawItems[Items[AItemNo].FirstDItemNo].LineFirst
//    and (AItemOffset = 0)
//    //and ((Items[AItemNo].StyleNo < THCStyle.RsNull) or (AItemOffset = 0))
//  then  // 在开头
//  begin
//    if not Items[AItemNo].ParaFirst then  // 不是段首
//      AFirstItemNo := GetLineFirstItemNo(AItemNo - 1, Items[AItemNo - 1].Length)
//    else  // 是段首
//      AFirstItemNo := AItemNo;
//  end
//  else
//    AFirstItemNo := GetLineFirstItemNo(AItemNo, 0);  // 取行第一个DrawItem对应的ItemNo
//
//  ALastItemNo := GetParaLastItemNo(AItemNo);
end;

procedure THCFormatData.ItemRequestFormat(const AItem: THCCustomItem);
begin
  if Assigned(FOnItemRequestFormat) then
    FOnItemRequestFormat(Self, AItem);
end;

procedure THCFormatData.ReFormat;
begin
  if FFormatCount = 0 then
  begin
    DrawItems.Clear;
    InitializeField;

    DrawItems.MarkFormatDelete(0, DrawItems.Count - 1);

    FormatInit;
    FormatRange(0, Items.Count - 1);

    // 2个原因直接赋值为True
    // 1. 空行插入RectItem后高度会变化
    // 2. 在最后一行变化时，要去掉第一行到最后一行的偏移才能确定高度是否发生变化，干脆直接为True
    FFormatHeightChange := True;
  end;

  if (SelectInfo.StartItemNo >= 0) and (SelectInfo.StartItemNo < Items.Count) then
    ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset)  // 防止清空后格式化完成后没有选中起始访问出错
  else
    ReSetSelectAndCaret(0, 0);
end;

procedure THCFormatData.ReFormatActiveItem;
var
  vFirstDrawItemNo, vLastItemNo: Integer;
begin
  if Self.SelectExists then Exit;

  if SelectInfo.StartItemNo >= 0 then
  begin
    GetFormatRange(vFirstDrawItemNo, vLastItemNo);
    FormatPrepare(vFirstDrawItemNo, vLastItemNo);

    if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null then  // 当表格里是RadioItem修改引起大小变化时需要重新格式化
      (Items[SelectInfo.StartItemNo] as THCCustomRectItem).ReFormatActiveItem;

    ReFormatData(vFirstDrawItemNo, vLastItemNo);

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;

    if SelectInfo.StartItemOffset > Items[SelectInfo.StartItemNo].Length then
      ReSetSelectAndCaret(SelectInfo.StartItemNo);
  end;
end;

procedure THCFormatData.ReFormatActiveParagraph;
var
  vFirstItemNo, vLastItemNo: Integer;
begin
  if SelectInfo.StartItemNo >= 0 then
  begin
    GetParaItemRang(SelectInfo.StartItemNo, vFirstItemNo, vLastItemNo);
    FormatPrepare(Items[vFirstItemNo].FirstDItemNo, vLastItemNo);
    ReFormatData(Items[vFirstItemNo].FirstDItemNo, vLastItemNo);

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;

    ReSetSelectAndCaret(SelectInfo.StartItemNo);
  end;
end;

procedure THCFormatData.ReFormatData(const AFirstDrawItemNo: Integer;
  const ALastItemNo: Integer = -1; const AExtraItemCount: Integer = 0;
  const AForceClearExtra: Boolean = False);
var
  i, vLastItemNo, vLastDrawItemNo, vDrawItemCount,
  vFmtTopOffset, vClearFmtHeight: Integer;
begin
  if FFormatCount <> 0 then Exit;

  if ALastItemNo < 0 then
    vLastItemNo := DrawItems[AFirstDrawItemNo].ItemNo
  else
    vLastItemNo := ALastItemNo;

  vDrawItemCount := DrawItems.Count;  // 格式化前的DrawItem数量
  FormatRange(AFirstDrawItemNo, vLastItemNo);  // 格式化指定范围内的Item
  FFormatDrawItemCountChange := DrawItems.Count <> vDrawItemCount;  // 格式化前后DrawItem数量有变化

  // 计算格式化后段的底部位置变化
  vLastDrawItemNo := GetItemLastDrawItemNo(vLastItemNo);
  if (Items[vLastItemNo] is THCCustomRectItem) and (Items[vLastItemNo] as THCCustomRectItem).SizeChanged then
    FFormatHeightChange := True
  else
    FFormatHeightChange := AForceClearExtra
                        or (DrawItems[AFirstDrawItemNo].Rect.Top <> FFormatStartTop)  // 段格式化后，高度的增量
                        or (DrawItems[vLastDrawItemNo].Rect.Bottom <> FFormatEndBottom);

  //FFormatChange := False;  // 如果操作引起多次格式化，如选中内容后输入，先删除选中造成FFormatChange为真
  // 如果输入的内容这里置否后没有引起变化，则本次变动的格式化并没有影响到重新计算当前页的起始结束DrawItem
  // 造成绘制时访问DrawItem越界，所以这里不能先置否
  if FFormatHeightChange or (AExtraItemCount <> 0) or FFormatDrawItemCountChange then
  begin                            {FFormatDrawItemCountChange能被前两者代表吗？}
    FFormatChange := True;
    vLastItemNo := -1;
    vFmtTopOffset := 0;

    for i := vLastDrawItemNo + 1 to DrawItems.Count - 1 do  // 从格式化变动段的下一段开始
    begin
      if (AExtraItemCount <> 0) or FFormatDrawItemCountChange then  // 将ItemNo的增量传递给后面的DrawItem
      begin
        // 处理格式化后面各DrawItem对应的ItemNo偏移
        DrawItems[i].ItemNo := DrawItems[i].ItemNo + AExtraItemCount;
        if vLastItemNo <> DrawItems[i].ItemNo then
        begin
          vLastItemNo := DrawItems[i].ItemNo;
          Items[vLastItemNo].FirstDItemNo := i;
        end;
      end;

      if FFormatHeightChange then  // 这里能确认为0时不需要重新处理偏移吗？
      begin
        // 将原格式化因分页等原因引起的整体下移或增加的高度恢复回来
        // 如果不考虑上面处理ItemNo的偏移，可将TTableCellData.ClearFormatExtraHeight方法写到基类，这里直接调用
        if DrawItems[i].LineFirst then
          vFmtTopOffset := DrawItems[i - 1].Rect.Bottom - DrawItems[i].Rect.Top;

        OffsetRect(DrawItems[i].Rect, 0, vFmtTopOffset);

        if Items[DrawItems[i].ItemNo].StyleNo < THCStyle.Null then  // RectItem如表格，在格式化时有行和行中间的偏移，新格式化时要恢复，由分页函数再处理新格式化后的偏移
        begin
          vClearFmtHeight := (Items[DrawItems[i].ItemNo] as THCCustomRectItem).ClearFormatExtraHeight;
          DrawItems[i].Rect.Bottom := DrawItems[i].Rect.Bottom - vClearFmtHeight;
        end;
      end;
    end;
  end;
end;

procedure THCFormatData.ReSetSelectAndCaret(const AItemNo: Integer);
begin
  ReSetSelectAndCaret(AItemNo, GetItemOffsetAfter(AItemNo));
end;

procedure THCFormatData.ReSetSelectAndCaret(const AItemNo, AOffset: Integer;
  const ANextWhenMid: Boolean = False);
var
  vDrawItemNo, vOffset: Integer;
begin
  SelectInfo.StartItemNo := AItemNo;
  SelectInfo.StartItemOffset := AOffset;

  if FFormatCount <> 0 then Exit;

  if Items[AItemNo].StyleNo > THCStyle.Null then
  begin
    if SelectInfo.StartItemOffset > Items[AItemNo].Length then
      vOffset := Items[AItemNo].Length
    else
      vOffset := AOffset;
  end
  else
    vOffset := AOffset;

  vDrawItemNo := GetDrawItemNoByOffset(AItemNo, vOffset);
  if ANextWhenMid
    and (vDrawItemNo < DrawItems.Count - 1)
    and (DrawItems[vDrawItemNo + 1].ItemNo = AItemNo)
    and (DrawItems[vDrawItemNo + 1].CharOffs = vOffset + 1)
  then
    Inc(vDrawItemNo);

  CaretDrawItemNo := vDrawItemNo;
end;

end.
