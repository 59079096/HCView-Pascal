{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                     表格实现单元                      }
{                                                       }
{*******************************************************}

unit HCTableItem;

interface

uses
  Classes, SysUtils, Types, Graphics, Controls, Generics.Collections, HCDrawItem,
  HCRectItem, HCTableRow, HCCustomData, HCRichData, HCTableCell, HCTableCellData,
  HCViewData, HCTextStyle, HCCommon, HCParaStyle, HCStyleMatch, HCItem, HCStyle,
  HCList, HCUndo, HCXml, HCUnitConversion;

type
  TPageBreak = class  // 分页信息
    /// <summary> 在此页结尾分页 </summary>
    PageIndex,
    Row,  // 分页行
    BreakSeat,  // 分页时，此行各列分页截断位置距离表格顶部距离最大的
    BreakBottom  // 分页时，页底部位置距此页表格最顶部的距离(此页有多少空间用来放表格)
      : Integer;
  end;

  THCTableRows = class(TObjectList<THCTableRow>)
  private
    FOnRowAdd: TRowAddEvent;
  protected
    procedure Notify(const Value: THCTableRow; Action: TCollectionNotification); override;
  public
    property OnRowAdd: TRowAddEvent read FOnRowAdd write FOnRowAdd;
  end;

  THCMulCellUndo = class(TObject)
  private
    FEnable: Boolean;
    procedure SetEnable(const Value: Boolean);
  public
    Row, Col: Integer;
    procedure Init(const ARow, ACol: Integer);
    property Enable: Boolean read FEnable write SetEnable;
  end;

  THCCellPaintEvent = procedure(const Sender: TObject; const ACell: THCTableCell;
    const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TPaintInfo;
    var ADrawDefault: Boolean) of object;

  THCCellPaintDataEvent = procedure(const Sender: TObject; const ATableRect, ACellRect: TRect;
    const ARow, ACol, ACellDataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;

  THCTableItem = class(THCDataItem)
  private
    FBorderWidthPix,  // 边框宽度(建议为偶数要求不大于最小行高，否则分页计算会有问题)
    FCellHPaddingPix,  // 单元格内容水平偏移
    FCellVPaddingPix,   // 单元格内容垂直偏移(不能大于最低的DrawItem高度，否则会影响跨页)
    FFixRowCount,  // 固定行数量 > 0有效
    FFixColCount   // 固定列数量 > 0有效
      : Byte;  // 单元格数据和单元格边框的距离

    FBorderWidthPt, FCellVPaddingMM, FCellHPaddingMM: Single;
    FFixCol,  // 从哪列开始固定列
    FFixRow  // 从哪行开始固定行
      : ShortInt;
    FOutsideInfo: TOutsideInfo;  // 点击在表格左右边时对应的行信息

    FMouseDownRow, FMouseDownCol,
    FMouseMoveRow, FMouseMoveCol,
    FMouseDownX, FMouseDownY,
    FFormatHeight
      : Integer;
    FResizeInfo: TResizeInfo;
    FMulCellUndo: THCMulCellUndo;
    FBorderVisible, FMouseLBDowning, FSelecting, FDraging, FOutSelectInto, FFormatDirty,  // 最后变动已经格式化完了
    FResizeKeepWidth  // 拖动改变非最右侧边框宽度时，是否保持当前整体宽度不变
      : Boolean;

    { 选中信息(只有选中起始和结束行都>=0才说明有选中多个单元格
     在单个单元格中选择时结束行、列信息为-1 }
    FSelectCellRang: TSelectCellRang;
    FBorderColor: TColor;  // 边框颜色
    FRows: THCTableRows;  // 行
    FColWidths: TList<Integer>;  // 记录各列宽度(除边框、含FCellHPadding * 2)，方便有合并的单元格获取自己水平开始处的位置
    FPageBreaks: TObjectList<TPageBreak>;  // 记录各行分页时的信息
    FOnCellPaintBK: THCCellPaintEvent;
    FOnCellPaintData: THCCellPaintDataEvent;
    procedure InitializeMouseInfo;
    procedure InitializeCellData(const ACellData: THCTableCellData);
    function DoCellDataGetRootData: THCCustomData;
    procedure DoCellDataItemRequestFormat(const AData: THCCustomData; const AItem: THCCustomItem);

    /// <summary> 表格行有添加时 </summary>
    procedure DoRowAdd(const ARow: THCTableRow);
    procedure CellChangeByAction(const ARow, ACol: Integer; const AProcedure: THCProcedure);

    /// <summary> 获取当前表格格式化高度 </summary>
    /// <returns></returns>
    function GetFormatHeight: Integer;
    /// <summary> 获取行中最高单元格高度，并设置为行中其他单元格的高度和行高 </summary>
    procedure CalcRowCellHeight(const ARow: Integer);
    /// <summary> 计算有合并的单元格高度影响到的行高度 </summary>
    procedure CalcMergeRowHeightFrom(const ARow: Integer);
    function SrcCellDataTopDistanceToDest(const ASrcRow, ADestRow: Integer): Integer;

    /// <summary> 返回指定单元格相对表格的起始位置坐标(如果被合并返回合并到单元格的坐标) </summary>
    /// <param name="ARow"></param>
    /// <param name="ACol"></param>
    /// <returns></returns>
    function GetCellPostion(const ARow, ACol: Integer): TPoint;
    function ActiveDataResizing: Boolean;

    /// <summary> 取消选中范围内除ARow, ACol之外单元格的选中状态(-1表示全部取消) </summary>
    procedure DisSelectSelectedCell(const ARow: Integer = -1; const ACol: Integer = -1);
    procedure SetBorderWidthPt(const Value: Single);
    procedure SetCellVPaddingMM(const Value: Single);
    procedure SetCellHPaddingMM(const Value: Single);
  protected
    function CanDrag: Boolean; override;
    function GetSelectComplate: Boolean; override;
    procedure SelectComplate; override;
    function GetResizing: Boolean; override;
    procedure SetResizing(const Value: Boolean); override;

    /// <summary> 在指定的位置绘制表格 </summary>
    /// <param name="AStyle"></param>
    /// <param name="ADrawRect">绘制时的Rect(相对ADataScreenTop)</param>
    /// <param name="ADataDrawTop">Table所属的Data绘制起始位置(相对ADataScreenTop，可为负数)</param>
    /// <param name="ADataDrawBottom">Table所属的Data绘制起始位置(相对ADataScreenTop，可超过ADataScreenBottom)</param>
    /// <param name="ADataScreenTop">当前页屏显起始位置(相对于点0, 0，>=0)</param>
    /// <param name="ADataScreenBottom">当前页屏幕底部位置(相对于点0, 0，<=窗口高度)</param>
    /// <param name="ACanvas"></param>
    /// <param name="APaintInfo"></param>
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    //procedure PaintPartTo(const ACanvas: TCanvas; const ADrawLeft, ADrawTop, ADrawBottom, ADataScreenTop,
    //  ADataScreenBottom, AStartRow, AStartRowDataOffs, AEndRow, AEndRowDataOffs: Integer); overload;
    {procedure ConvertToDrawItems(const AItemNo, AOffs, AContentWidth,
      AContentHeight: Integer; var APos: TPoint; var APageIndex, ALastDNo: Integer);}

    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure MouseLeave; override;
    procedure KillFocus; override;

    /// <summary> 清除并返回为处理分页行和行中间有向下偏移后，比净高增加的高度(为重新格式化时后面计算偏移用) </summary>
    function ClearFormatExtraHeight: Integer; override;
    function DeleteSelected: Boolean; override;
    procedure DisSelect; override;
    procedure FormatDirty; override;
    procedure MarkStyleUsed(const AMark: Boolean); override;
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    procedure SetActive(const Value: Boolean); override;

    /// <summary> 获取表格在指定高度内的结束位置处行中最下端(暂时没用到注释了) </summary>
    /// <param name="AHeight">指定的高度范围</param>
    /// <param name="ADItemMostBottom">最后一行最底端DItem的底部位置</param>
    //procedure GetPageFmtBottomInfo(const AHeight: Integer; var ADItemMostBottom: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    // 撤销重做相关方法
    function DoSelfUndoNew: THCUndo; override;
    procedure DoSelfUndoDestroy(const AUndo: THCUndo); override;
    procedure DoSelfUndo(const AUndo: THCUndo); override;
    procedure DoSelfRedo(const ARedo: THCUndo); override;
    procedure Undo_ColResize(const ACol, AOldWidth, ANewWidth: Integer);
    procedure Undo_RowResize(const ARow, AOldHeight, ANewHeight: Integer);
    procedure Undo_Mirror;
    function GetRowCount: Integer;
    function GetColCount: Integer;
    procedure CheckFixColSafe(const ACol: Integer);
    procedure CheckFixRowSafe(const ARow: Integer);
    /// <summary> 获取指定行列范围实际对应的行列范围 </summary>
    /// <param name="AStartRow"></param>
    /// <param name="AStartCol"></param>
    /// <param name="AEndRow"></param>
    /// <param name="AEndCol"></param>
    procedure AdjustCellRange(const AStartRow, AStartCol: Integer;
      var AEndRow, AEndCol: Integer);
    function MergeCells(const AStartRow, AStartCol, AEndRow, AEndCol: Integer): Boolean;
    function GetCells(const ARow, ACol: Integer): THCTableCell;
    function GetColWidth(AIndex: Integer): Integer;
    procedure SetColWidth(AIndex: Integer; const AWidth: Integer);
    function InsertCol(const ACol, ACount: Integer): Boolean;
    function InsertRow(const ARow, ACount: Integer): Boolean;
    function DeleteCol(const ACol: Integer): Boolean;
    function DeleteRow(const ARow: Integer): Boolean;
  public
    //DrawItem: TCustomDrawItem;
    constructor Create(const AOwnerData: THCCustomData; const ARowCount, AColCount, AWidth: Integer); virtual;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure DblClick(const X, Y: Integer); override;
    function CoordInSelect(const X, Y: Integer): Boolean; override;
    function GetTopLevelDataAt(const X, Y: Integer): THCCustomData; override;
    function GetTopLevelData: THCCustomData; override;
    function GetActiveData: THCCustomData; override;
    function GetActiveItem: THCCustomItem; override;
    function GetTopLevelItem: THCCustomItem; override;
    function GetTopLevelDrawItem: THCCustomDrawItem; override;
    function GetTopLevelDrawItemCoord: TPoint; override;
    function GetTopLevelRectDrawItem: THCCustomDrawItem; override;
    function GetTopLevelRectDrawItemCoord: TPoint; override;
    function GetHint: string; override;
    function InsertText(const AText: string): Boolean; override;
    function InsertItem(const AItem: THCCustomItem): Boolean; override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word): Boolean; override;
    procedure ReFormatActiveItem; override;
    procedure ActiveItemReAdaptEnvironment; override;
    function DeleteActiveDomain: Boolean; override;
    procedure DeleteActiveDataItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean); override;
    procedure SetActiveItemText(const AText: string); override;
    function IsSelectComplateTheory: Boolean; override;
    function SelectExists: Boolean; override;
    procedure TraverseItem(const ATraverse: THCItemTraverse); override;

    /// <summary> 当前位置开始查找指定的内容 </summary>
    /// <param name="AKeyword">要查找的关键字</param>
    /// <param name="AForward">True：向前，False：向后</param>
    /// <param name="AMatchCase">True：区分大小写，False：不区分大小写</param>
    /// <returns>True：找到</returns>
    function Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean; override;
    procedure CheckFormatPageBreakBefor; override;
    procedure ApplySelectTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch); override;
    procedure ApplySelectParaStyle(const AStyle: THCStyle; const AMatchStyle: THCParaMatch); override;
    /// <summary> RectItem内容对齐方式 </summary>
    procedure ApplyContentAlign(const AAlign: THCContentAlign); override;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    /// <summary> 正在其上时内部是否处理指定的Key和Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;

    /// <summary> 表格分页 </summary>
    /// <param name="ADrawItemRectTop">表格对应的DrawItem的Rect.Top</param>
    /// <param name="ADrawItemRectTop">表格对应的DrawItem的Rect.Bottom</param>
    /// <param name="APageDataFmtTop">当前页的数据顶部位置</param>
    /// <param name="APageDataFmtBottom">当前页的数据底部位置</param>
    /// <param name="ACheckRow">当前页从哪行开始排版</param>
    /// <param name="ABreakRow">当前页最后分页于哪行</param>
    /// <param name="AFmtOffset">表格对应的DrawItem向下整体偏移的量</param>
    /// <param name="ACellMaxInc">返回当前页各列为了避开分页位置额外偏移的最大高度(参数原名AFmtHeightInc为便于分析重命名)</param>
    procedure CheckFormatPageBreak(const APageIndex, ADrawItemRectTop,
      ADrawItemRectBottom, APageDataFmtTop, APageDataFmtBottom, AStartRow: Integer;
      var ABreakRow, AFmtOffset, ACellMaxInc: Integer); override;

    // 保存和读取
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure SaveSelectToStream(const AStream: TStream); override;  // inherited TCustomRect
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    function ToHtml(const APath: string): string; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    function ResetRowCol(const AWidth, ARowCount, AColCount: Integer): Boolean;

    /// <summary> 获取当前表格格式化宽度 </summary>
    function GetFormatWidth: Integer;

    /// <summary> 获取指定位置处的行、列(如果是被合并单元格则返回目标单元格行、列) </summary>
    /// <param name="X">横坐标</param>
    /// <param name="Y">纵坐标</param>
    /// <param name="ARow">坐标处的行</param>
    /// <param name="ACol">坐标处的列</param>
    /// <param name="AReDest">如果坐标是合并源，返回目标</param>
    /// <returns></returns>
    function GetCellAt(const X, Y: Integer; var ARow, ACol: Integer;
      const AReDest: Boolean = True): TResizeInfo;
    procedure GetDestCell(const ARow, ACol: Cardinal; var ADestRow, ADestCol: Integer);
    procedure GetSourceCell(const ARow, ACol: Cardinal; var ASrcRow, ASrcCol: Integer);
    procedure SelectAll;
    procedure PaintRow(const ARow, ALeft, ATop, ABottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure PaintFixRows(const ALeft, ATop, ABottom: Integer; const ACanvas: TCanvas;
      const APaintInfo: TPaintInfo);
    procedure PaintFixCols(const ATableDrawTop, ALeft, ATop, ABottom: Integer; const ACanvas: TCanvas;
      const APaintInfo: TPaintInfo);

    /// <summary> 计算行中单元格宽度并格式化行 </summary>
    procedure FormatRow(const ARow: Cardinal);
    function GetColSpanWidth(const ARow, ACol: Integer): Integer;

    /// <summary> 判断指定范围内的单元格是否可以合并(为了给界面合并菜单控制可用状态放到public域中) </summary>
    /// <param name="AStartRow"></param>
    /// <param name="AStartCol"></param>
    /// <param name="AEndRow"></param>
    /// <param name="AEndCol"></param>
    /// <returns></returns>
    function CellsCanMerge(const AStartRow, AStartCol, AEndRow, AEndCol: Integer): Boolean;

    /// <summary> 指定行是否能删除 </summary>
    function RowCanDelete(const ARow: Integer): Boolean;
    function CurRowCanDelete: Boolean;

    /// <summary> 指定列是否能删除 </summary>
    function ColCanDelete(const ACol: Integer): Boolean;
    function CurColCanDelete: Boolean;

    /// <summary> 获取指定单元格合并后单元格的Data </summary>
    //function GetMergeDestCellData(const ARow, ACol: Integer): THCTableCellData;

    function MergeSelectCells: Boolean;
    function SelectedCellCanMerge: Boolean;
    function GetEditCell: THCTableCell; overload;
    procedure GetEditCell(var ARow, ACol: Integer); overload;
    function InsertRowAfter(const ACount: Integer): Boolean;
    function InsertRowBefor(const ACount: Integer): Boolean;
    function InsertColAfter(const ACount: Integer): Boolean;
    function InsertColBefor(const ACount: Integer): Boolean;
    function DeleteCurCol: Boolean;
    function DeleteCurRow: Boolean;
    function SplitCurRow: Boolean;
    function SplitCurCol: Boolean;
    function IsBreakRow(const ARow: Integer): Boolean;
    function IsFixRow(const ARow: Integer): Boolean;
    function IsFixCol(const ACol: Integer): Boolean;
    function GetFixRowHeight: Integer;
    function GetFixColWidth: Integer;
    function GetFixColLeft: Integer;
    property Cells[const ARow, ACol: Integer]: THCTableCell read GetCells;
    property ColWidth[AIndex: Integer]: Integer read GetColWidth write SetColWidth;
    property Rows: THCTableRows read FRows;
    property RowCount: Integer read GetRowCount;
    property ColCount: Integer read GetColCount;
    property SelectCellRang: TSelectCellRang read FSelectCellRang;
    property BorderVisible: Boolean read FBorderVisible write FBorderVisible;
    property BorderWidthPix: Byte read FBorderWidthPix;
    property BorderWidthPt: Single read FBorderWidthPt write SetBorderWidthPt;
    property CellHPaddingPix: Byte read FCellHPaddingPix;
    property CellVPaddingPix: Byte read FCellVPaddingPix;
    property CellVPaddingMM: Single read FCellVPaddingMM write SetCellVPaddingMM;
    property CellHPaddingMM: Single read FCellHPaddingMM write SetCellHPaddingMM;
    property FixCol: ShortInt read FFixCol write FFixCol;
    /// <summary> 固定列跨度 </summary>
    property FixColCount: Byte read FFixColCount write FFixColCount;
    property FixRow: ShortInt read FFixRow write FFixRow;
    /// <summary> 固定行跨度 </summary>
    property FixRowCount: Byte read FFixRowCount write FFixRowCount;
    property OnCellPaintBK: THCCellPaintEvent read FOnCellPaintBK write FOnCellPaintBK;
    property OnCellPaintData: THCCellPaintDataEvent read FOnCellPaintData write FOnCellPaintData;
  end;

implementation

{$I HCView.inc}

uses
  Math, Windows;

type
  /// <summary> 行跨页信息 </summary>
  TColCross = class(TObject)
  public
    Col,  // 单元格所在的列
    DrawItemNo,  // 跨页的DrawItem
    VDrawOffset  // 跨页DrawItem的偏移
      : Integer;
    //MergeSrc: Boolean;
    constructor Create;
  end;

{$I HCView.inc}

{ THCTableItem }

procedure THCTableItem.CalcMergeRowHeightFrom(const ARow: Integer);
var
  i, vR, vC, vExtraHeight, vDestRow, vDestCol, vH, vDestRow2, vDestCol2: Integer;
begin
  // 为兼容分页时重新格式化调用此方法，所以有对FmtOffset的处理，否则不需要管FmtOffset
  for vR := ARow to RowCount - 1 do  // 计算有行合并情况下各行的高度
  begin
    for vC := 0 to FRows[vR].ColCount - 1 do
    begin
      if FRows[vR][vC].CellData = nil then  // 当前单元格被合并了
      begin
        if FRows[vR][vC].ColSpan < 0 then  // 合并目标只需由正下方的单元格处理合并内容，不用重复处理
          Continue;

        GetDestCell(vR, vC, vDestRow, vDestCol);  // 获取到合并目标单元格所在行号

        if vDestRow + FRows[vDestRow][vC].RowSpan = vR then  // 目标单元格行合并到此结束
        begin
          vExtraHeight := FCellVPaddingPix + FRows[vDestRow][vC].CellData.Height + FCellVPaddingPix;  // 目标单元格除上下边框后的高度
          FRows[vDestRow][vC].Height := vExtraHeight;  // 目标单元格除上下边框后的高度
          vExtraHeight := vExtraHeight - FRows[vDestRow].Height - FBorderWidthPix;  // “消减”自己所在行

          for i := vDestRow + 1 to vR - 1 do  // 从目标下一行到此，经过各行后“消减”掉多少
            vExtraHeight := vExtraHeight - FRows[i].FmtOffset - FRows[i].Height - FBorderWidthPix;

          if vExtraHeight > FRows[vR].FmtOffset + FRows[vR].Height then  // 消减后剩余的比当前行高
          begin
            vH := vExtraHeight - FRows[vR].FmtOffset - FRows[vR].Height;  // 高多少
            FRows[vR].Height := vExtraHeight - FRows[vR].FmtOffset;  // 当前行高赋值新值(内部各单元格高度会处理)

            for i := 0 to FRows[vR].ColCount - 1 do  // 当前行中源列要影响目标单元格
            begin
              if FRows[vR][i].CellData = nil then  // 源列
              begin
                GetDestCell(vR, i, vDestRow2, vDestCol2);  // 获取目标单元格
                if (vDestRow2 <> vDestRow) and (vDestCol2 <> vDestCol) then  // 不是当前要处理的目标单元格
                  FRows[vDestRow2][i].Height := FRows[vDestRow2][i].Height + vH;
              end;
            end;
          end
          else  // 消减后剩余的没有当前行高，高度增加到当前行底部，处理非合并的单元格内容，大于合并结束到此行但数据底部没有此行高的情况
          begin
            FRows[vDestRow][vC].Height :=  // 2017-1-15_1.bmp中[1, 1]输入c时[1, 0]和[1, 2]的情况
              FRows[vDestRow][vC].Height + FRows[vR].FmtOffset + FRows[vR].Height - vExtraHeight;
          end;
        end;
      end;
    end;
  end;
end;

procedure THCTableItem.FormatDirty;
begin
  FFormatDirty := True;
  inherited FormatDirty;
end;

procedure THCTableItem.FormatRow(const ARow: Cardinal);
var
  vC, vWidth: Integer;
  vRow: THCTableRow;
begin
  vRow := FRows[ARow];
  vRow.FmtOffset := 0;  // 恢复上次格式化可能的偏移
  // 格式化各单元格中的Data
  for vC := 0 to vRow.ColCount - 1 do
  begin
    if vRow[vC].CellData <> nil then
    begin
      vWidth := FColWidths[vC] + GetColSpanWidth(ARow, vC);
      {for i := 1 to vRow[vC].ColSpan do
        vWidth := vWidth + FBorderWidth + FColWidths[vC + i];}


      vRow[vC].Width := vWidth;
      vRow[vC].CellData.Width := vWidth - FCellHPaddingPix - FCellHPaddingPix;
      vRow[vC].CellData.ReFormat;
    end
  end;
end;

procedure THCTableItem.FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer);
var
  i, vR, vC: Integer;
begin
  if not FFormatDirty then
  begin
    ClearFormatExtraHeight;
    Exit;
  end;

  for vR := 0 to RowCount - 1 do  // 格式化各行
  begin
    FormatRow(vR);  // 格式化行，并计算行高度
    CalcRowCellHeight(vR);  // 以行中所有无行合并操作列中最大高度更新其他列
  end;

  FFormatDirty := False;

  CalcMergeRowHeightFrom(0);
  Self.Height := GetFormatHeight;  // 计算整体高度
  Self.Width := GetFormatWidth;  // 计算整体宽度
end;

constructor THCTableItem.Create(const AOwnerData: THCCustomData; const ARowCount, AColCount, AWidth: Integer);
var
  vRow: THCTableRow;
  i, vDataWidth: Integer;
begin
  inherited Create(AOwnerData);

  if ARowCount = 0 then
    raise Exception.Create('异常：不能创建行数为0的表格！');
  if AColCount = 0 then
    raise Exception.Create('异常：不能创建列数为0的表格！');

  GripSize := 2;
  FFormatHeight := 0;
  FDraging := False;
  CellHPaddingMM := 0.5;
  CellVPaddingMM := 0;
  BorderWidthPt := 0.5;
  FBorderColor := clBlack;
  FBorderVisible := True;
  FResizeKeepWidth := False;

  StyleNo := THCStyle.Table;
  ParaNo := OwnerData.CurParaNo;
  CanPageBreak := True;
  FPageBreaks := TObjectList<TPageBreak>.Create;
  FMulCellUndo := THCMulCellUndo.Create;
  FSelectCellRang := TSelectCellRang.Create;
  FColWidths := TList<Integer>.Create;
  FRows := THCTableRows.Create;
  FRows.OnRowAdd := DoRowAdd;  // 添加行时触发的事件
  Self.ResetRowCol(AWidth, ARowCount, AColCount);
  FMangerUndo := True;  // 自己管理自己的撤销和恢复操作
end;

function THCTableItem.CurColCanDelete: Boolean;
begin
  Result := (FSelectCellRang.EndCol < 0)
    and (FSelectCellRang.StartCol >= 0)
    and ColCanDelete(FSelectCellRang.StartCol);
end;

function THCTableItem.CurRowCanDelete: Boolean;
begin
  Result := (FSelectCellRang.EndRow < 0)
    and (FSelectCellRang.StartRow >= 0)
    and RowCanDelete(FSelectCellRang.StartRow);
end;

procedure THCTableItem.DblClick(const X, Y: Integer);
var
  vPt: TPoint;
begin
  if FSelectCellRang.EditCell then
  begin
    vPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
    FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.DblClick(
      X - vPt.X - FCellHPaddingPix, Y - vPt.Y - FCellVPaddingPix);
  end
  else
    inherited DblClick(X, Y);
end;

procedure THCTableItem.DeleteActiveDataItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean);
begin
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      var
        vEditCell: THCTableCell;
      begin
        vEditCell := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol];
        vEditCell.CellData.DeleteActiveDataItems(AStartNo, AEndNo, AKeepPara);
      end);
  end;
end;

function THCTableItem.DeleteActiveDomain: Boolean;
var
  vResult: Boolean;
begin
  Result := inherited DeleteActiveDomain;

  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      var
        vEditCell: THCTableCell;
      begin
        vEditCell := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol];
        vResult := vEditCell.CellData.DeleteActiveDomain;
      end);

    Result := vResult;
  end;
end;

function THCTableItem.DeleteCol(const ACol: Integer): Boolean;
var
  i, vRow: Integer;
  viDestRow, viDestCol: Integer;
begin
  Result := False;

  if not ColCanDelete(ACol) then Exit;

  Undo_Mirror;
  for vRow := 0 to RowCount - 1 do
  begin
    if FRows[vRow][ACol].ColSpan < 0 then  // 合并源
    begin
      GetDestCell(vRow, ACol, viDestRow, viDestCol);  // 目标行、列
      for i := ACol + 1 to viDestCol + FRows[viDestRow][viDestCol].ColSpan do  // 当前列右面的合并源列离目标近1
        FRows[vRow][i].ColSpan := FRows[vRow][i].ColSpan + 1;

      if vRow = viDestRow + FRows[viDestRow][viDestCol].RowSpan then  // 最后一源行，最后一源列处理完后，目标行列跨度减少1
        FRows[viDestRow][viDestCol].ColSpan := FRows[viDestRow][viDestCol].ColSpan - 1;
    end
    else
    if FRows[vRow][ACol].ColSpan > 0 then  // 合并目标
    begin

    end;

    FRows[vRow].Delete(ACol);
  end;

  FColWidths.Delete(ACol);
  CheckFixColSafe(ACol);
  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Self.SizeChanged := True;
  Self.FormatDirty;
  Result := True;
end;

function THCTableItem.DeleteCurCol: Boolean;
var
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;

  if FColWidths.Count > 1 then
    Result := DeleteCol(FSelectCellRang.StartCol);
end;

function THCTableItem.DeleteCurRow: Boolean;
var
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;

  if FRows.Count > 1 then
    Result := DeleteRow(FSelectCellRang.StartRow);
end;

function THCTableItem.DeleteRow(const ARow: Integer): Boolean;
var
  i, vCol: Integer;
  viDestRow, viDestCol: Integer;
begin
  Result := False;

  if not RowCanDelete(ARow) then Exit;
  Undo_Mirror;
  for vCol := 0 to FColWidths.Count - 1 do
  begin
    if FRows[ARow][vCol].RowSpan < 0 then  // 合并源
    begin
      GetDestCell(ARow, vCol, viDestRow, viDestCol);  // 目标行、列
      for i := ARow + 1 to viDestRow + FRows[viDestRow][viDestCol].RowSpan do  // 当前行下面的合并源行离目标近1
        FRows[i][vCol].RowSpan := FRows[i][vCol].RowSpan + 1;

      if vCol = viDestCol + FRows[viDestRow][viDestCol].ColSpan then  // 最后一源行，最后一源列处理完后，处理目标列行跨度减少1
        FRows[viDestRow][viDestCol].RowSpan := FRows[viDestRow][viDestCol].RowSpan - 1;
    end
    else
    if FRows[ARow][vCol].ColSpan > 0 then  // 合并目标
    begin

    end;
  end;

  FRows.Delete(ARow);
  CheckFixRowSafe(ARow);
  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Self.SizeChanged := True;
  Self.FormatDirty;
  Result := True;
end;

function THCTableItem.DeleteSelected: Boolean;
var
  vUndoList: THCUndoList;
  vR, vC: Integer;
  vResult: Boolean;
begin
  Result := inherited DeleteSelected;

  if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
  begin
    if FSelectCellRang.EndRow >= 0 then  // 有选择结束行，说明选中不在同一单元格
    begin
      FMulCellUndo.Enable := True;
      try
        vUndoList := GetSelfUndoList;
        vUndoList.UndoGroupBegin(0, 0);
        try
          for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
          begin
            for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
            begin
              if FRows[vR][vC].CellData <> nil then
              begin
                FMulCellUndo.Init(vR, vC);
                FRows[vR][vC].CellData.DeleteSelected;
              end;
            end;
          end;
        finally
          vUndoList.UndoGroupEnd(0, 0);
        end;
      finally
        FMulCellUndo.Enable := False;
      end;

      //Self.SizeChanged := True;
      Self.FormatDirty;
      Result := True;
    end
    else  // 在同一单元格
    begin
      vResult := False;
      CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
        procedure
        begin
          vResult := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.DeleteSelected;
        end);

      Result := vResult;
    end;
  end;
end;

destructor THCTableItem.Destroy;
begin
  FSelectCellRang.Free;
  FPageBreaks.Free;
  FRows.Clear;
  FRows.Free;
  FColWidths.Free;
  FMulCellUndo.Free;
  //Dispose(FResizeInfo);
  //Dispose(FCaretInfo);
  inherited Destroy;
end;

procedure THCTableItem.DisSelect;
begin
  inherited DisSelect;

  DisSelectSelectedCell;

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;

  FSelecting := False;
  FDraging := False;
  FOutSelectInto := False;
end;

procedure THCTableItem.DisSelectSelectedCell(const ARow: Integer = -1; const ACol: Integer = -1);
var
  vRow, vCol: Integer;
  vCellData: THCTableCellData;
begin
  if FSelectCellRang.StartRow >= 0 then
  begin
    // 先清起始，确保当前单元格可执行DisSelect 与201805172309相似
    if (FSelectCellRang.StartRow = ARow) and (FSelectCellRang.StartCol = ACol) then

    else
    begin
      FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].Active := False;
      vCellData := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData;
      if vCellData <> nil then
      begin
        vCellData.DisSelect;
        vCellData.InitializeField;
      end;
    end;

    for vRow := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
    begin
      for vCol := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
      begin
        if (vRow = ARow) and (vCol = ACol) then

        else
        begin
          FRows[vRow][vCol].Active := False;
          vCellData := FRows[vRow][vCol].CellData;
          if vCellData <> nil then
          begin
            vCellData.DisSelect;
            vCellData.InitializeField;
          end;
        end;
      end;
    end;
  end;
end;

function THCTableItem.DoCellDataGetRootData: THCCustomData;
begin
  Result := OwnerData.GetRootData;
end;

procedure THCTableItem.DoCellDataItemRequestFormat(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  (OwnerData as THCRichData).ItemRequestFormat(Self);
end;

function THCTableItem.DoSelfUndoNew: THCUndo;
var
  vCellUndoData: THCCellUndoData;
  vMulCellUndoData: THCMulCellUndoData;
begin
  if FMulCellUndo.Enable then
  begin
    Result := THCDataUndo.Create;
    vMulCellUndoData := THCMulCellUndoData.Create;
    vMulCellUndoData.Row := FMulCellUndo.Row;
    vMulCellUndoData.Col := FMulCellUndo.Col;
    Result.Data := vMulCellUndoData;
  end
  else
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    Result := THCDataUndo.Create;
    vCellUndoData := THCCellUndoData.Create;
    vCellUndoData.Row := FSelectCellRang.StartRow;
    vCellUndoData.Col := FSelectCellRang.StartCol;
    Result.Data := vCellUndoData;
  end
  else
    Result := inherited DoSelfUndoNew;
end;

procedure THCTableItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vR, vC,
  vCellScreenTop,
  vCellScreenBottom,
  vDestCellDataDrawTop,  // 当前目标单元格数据绘制顶部位置（按单元格顶对齐）
  vCellDataDrawTop,  // 当前单元格数据绘制顶部位置（按单元格顶对齐）
  vCellDataDrawBottom,  // 当前单元格数据绘制底部位置
  vCellDrawLeft,  // 单元格绘制时左边起始位置(左边框右边)
  vBorderLeft,
  vBorderTop,
  vBorderRight,
  vBorderBottom,
  vShouLian,
  vDestRow, vDestCol, vDestRow2, vDestCol2, vSrcRowBorderTop,
  vFirstDrawRow  // 本次绘制的第一行
    : Integer;

  vDrawBorder,
  vDrawCellData,  // 此单元格数据是否需要绘制，合并源的只在合并目标时绘制一次，避免多次绘制
  vDrawDefault
    : Boolean;

  vCellData: THCTableCellData;

  {$REGION ' CheckRowBorderShouLian 找当前行各列分页时的收敛位置'}

  procedure CheckRowBorderShouLian(const ARow: Integer);
  var
    vC, i, vRowDataDrawTop, vDestCellDataDrawTop, vBreakBottom: Integer;
    vRect: TRect;
  begin
    if vShouLian = 0 then  // 没有计算过当前行有跨页的所有列中最佳跨页时收敛位置
    begin
      vRowDataDrawTop := ADrawRect.Top + FBorderWidthPix;  // 因为边框在ADrawRect.Top也占1像素，所以要减掉
      for i := 0 to ARow - 1 do
        vRowDataDrawTop := vRowDataDrawTop + FRows[i].FmtOffset + FRows[i].Height + FBorderWidthPix;

      if (FRows[ARow].FmtOffset > 0)  // 分页行整体下移了
        and (ARow <> vFirstDrawRow)  // 不是第一次绘制整体下移的分页行
      then  // 以上一行最低为收敛
      begin
        vShouLian := vRowDataDrawTop - FBorderWidthPix;  // 上一行底部边框位置
        Exit;
      end;

      // 分页行Data绘制起始位置，第一次绘制整体下移分页行时要增加偏移，否则不增加(和上一行结尾相同)做为收敛位置(见前上行Exit处)
      vRowDataDrawTop := vRowDataDrawTop + FRows[ARow].FmtOffset + FCellVPaddingPix;

      vBreakBottom := 0;
      for vC := 0 to FRows[ARow].ColCount - 1 do  // 遍历同行各列，获取截断位置(因为各行在CheckFormatPage已经算好分页位置，所以此处只要一个单元格跨页位置同时适用当前行所有单元格跨页位置)
      begin
        vDestCellDataDrawTop := vRowDataDrawTop; //vCellDataDrawTop;
        GetDestCell(ARow, vC, vDestRow2, vDestCol2);  // 获取到目标单元格所在行号

        if vC <> vDestCol2 + FRows[vDestRow2][vDestCol2].ColSpan then  // 只在当前页分页位置的合并最后源处理一次
          Continue;

        vCellData := FRows[vDestRow2][vDestCol2].CellData;  // 先赋值目标单元格Data
        if vDestRow2 <> ARow then  // 是源行，先取目标位置，再取到此行消减掉后的位置
          vDestCellDataDrawTop := vDestCellDataDrawTop - SrcCellDataTopDistanceToDest(ARow, vDestRow2);

        for i := 0 to vCellData.DrawItems.Count - 1 do
        begin
          if vCellData.DrawItems[i].LineFirst then
          begin
            vRect := vCellData.DrawItems[i].Rect;
            //if DrawiInLastLine(i) then  // 单元格内最后一行内容补充FCellVPadding
            vRect.Bottom := vRect.Bottom + FCellVPaddingPix; // 每一行可能是要截断的，截断时下面要能放下FCellVPadding
            if vDestCellDataDrawTop + vRect.Bottom > ADataDrawBottom then  // 此DrawItem超过当前页了
            begin
              if i > 0 then  // 跨页的Draw不是第一行
              begin
                if ADataDrawBottom - vDestCellDataDrawTop - vCellData.DrawItems[i - 1].Rect.Bottom > FCellVPaddingPix then
                  vShouLian := Max(vShouLian, vDestCellDataDrawTop + vCellData.DrawItems[i - 1].Rect.Bottom + FCellVPaddingPix)
                else
                  vShouLian := Max(vShouLian, vDestCellDataDrawTop + vCellData.DrawItems[i - 1].Rect.Bottom);  // 上一个最下面做为截断位置
              end
              else  // 第一行就在当前页放不下
                vShouLian := Max(vShouLian, vDestCellDataDrawTop - FCellVPaddingPix - FBorderWidthPix);

              Break;
            end
            else  // 此DrawItem没有超过当前页
              vBreakBottom := Max(vBreakBottom, vDestCellDataDrawTop + vRect.Bottom);  // 记录为可放下的最后一个下面(有的单元格在当前页能全部显示，并不跨页)
          end;
        end;
      end;
      vShouLian := Max(vShouLian, vBreakBottom);
    end;
  end;
  {$ENDREGION}

  {$REGION ' DoDrawPageBreakMark 绘制分页标识符 '}

  procedure DoDrawPageBreakMark(const APageEnd: Boolean);
  begin
    ACanvas.Pen.Color := clGray;
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Width := 1;

    if APageEnd then  // 分页符(页结束位置)
    begin
      ACanvas.MoveTo(vBorderRight + 5, vBorderBottom - 1);  // vBorderBottom
      ACanvas.LineTo(vBorderRight + 20, vBorderBottom - 1);

      ACanvas.Pen.Style := psSolid;
      ACanvas.MoveTo(vBorderRight + 19, vBorderBottom - 3);
      ACanvas.LineTo(vBorderRight + 19, vBorderBottom - 10);
      ACanvas.LineTo(vBorderRight + 5, vBorderBottom - 10);
      ACanvas.LineTo(vBorderRight + 5, vBorderBottom - 2);
    end
    else  // 分页符(页起始位置)
    begin
      ACanvas.MoveTo(vBorderRight + 5, ADataDrawTop + 1);  // vBorderTop
      ACanvas.LineTo(vBorderRight + 20, ADataDrawTop + 1);

      ACanvas.Pen.Style := psSolid;
      ACanvas.MoveTo(vBorderRight + 19, ADataDrawTop + 3);
      ACanvas.LineTo(vBorderRight + 19, ADataDrawTop + 10);
      ACanvas.LineTo(vBorderRight + 5, ADataDrawTop + 10);
      ACanvas.LineTo(vBorderRight + 5, ADataDrawTop + 2);
    end;

    ACanvas.Pen.Color := clBlack;
  end;
  {$ENDREGION}

var
  vFirstDrawRowIsBreak: Boolean;
  vExtPen: HPEN;
  vOldPen: HGDIOBJ;
  vBorderOffs: Integer;
  vCellRect: TRect;
begin
  //vFixHeight := GetFixRowHeight;
  vBorderOffs := FBorderWidthPix div 2;
  vFirstDrawRowIsBreak := False;
  vFirstDrawRow := -1;
  vCellDataDrawTop := ADrawRect.Top + FBorderWidthPix;  // 第1行数据绘制起始位置，因为边框在ADrawRect.Top也占1像素，所以要减掉
  for vR := 0 to FRows.Count - 1 do
  begin
    // 不在当前屏幕范围内的不绘制(1)
    vCellDataDrawTop := vCellDataDrawTop + FRows[vR].FmtOffset + FCellVPaddingPix;
    if vCellDataDrawTop > ADataScreenBottom then  // 行数据顶部大于可显示区域显示不出来不绘制
    begin
      if (vFirstDrawRow < 0) and IsBreakRow(vR){(FRows[vR].FmtOffset > 0)} then  // 有标题行导致的第vR行没在此页绘制时显示出来
        vFirstDrawRowIsBreak := (FFixRow >= 0) and (vR > FFixRow + FFixRowCount - 1);

      Break;
    end;

    vCellDataDrawBottom := vCellDataDrawTop - FCellVPaddingPix + FRows[vR].Height - FCellVPaddingPix;

    if vCellDataDrawBottom < ADataScreenTop then  // 当前行底部小于可显示顶部，没显示出来不绘制
    begin
      vCellDataDrawTop := vCellDataDrawBottom + FCellVPaddingPix + FBorderWidthPix;  // 准备判断下一行是否是可显示第一行
      Continue;
    end;

    if vFirstDrawRow < 0 then  // 记录本次绘制的第一行
    begin
      vFirstDrawRow := vR;

      if IsBreakRow(vR) then  //  FRows[vR].FmtOffset > 0 then
        vFirstDrawRowIsBreak := (FFixRow >= 0) and (vR > FFixRow + FFixRowCount - 1);
    end;

    vCellDrawLeft := ADrawRect.Left + FBorderWidthPix;

    // 循环绘制行中各单元格数据和边框
    vShouLian := 0;
    for vC := 0 to FRows[vR].ColCount - 1 do
    begin
      if FRows[vR][vC].ColSpan < 0 then  // 合并列源
      begin
        vCellDrawLeft := vCellDrawLeft + FColWidths[vC] + FBorderWidthPix;
        Continue;  // 普通单元格或合并目标单元格才有数据，否则由目标单元格处理
      end;

      vDrawCellData := True;  // 处理目标行有跨页，且目标行后面有多行合并到此行时，只在跨页后绘制一次目标行的数据
      if FRows[vR][vC].RowSpan < 0 then  // 20170208001 是合并行源单元格(由于上面排除了列源，所以这里只是目标单元格正下方的单元格)
      begin
        if vR <> vFirstDrawRow then  // 不是跨页后第一次绘制的行
          vDrawCellData := False;  // 目标单元格已经这页绘制了数据，不用重复绘制了，否则跨行后的第一次要绘制
      end;

      vDestCellDataDrawTop := vCellDataDrawTop;
      GetDestCell(vR, vC, vDestRow, vDestCol);  // 获取到目标单元格所在行号
      if vDestRow <> vR then
        vDestCellDataDrawTop := vDestCellDataDrawTop - SrcCellDataTopDistanceToDest(vR, vDestRow);

      {$REGION ' 绘制单元格数据 '}
      if vDrawCellData then
      begin
        vCellScreenBottom := Math.Min(ADataScreenBottom,  // 数据内容屏显最下端
          vCellDataDrawTop
          + Max(FRows[vR].Height, FRows[vDestRow][vDestCol].Height) - FCellVPaddingPix);  // 行高和有合并的单元格高中最大的
        //Assert(vCellScreenBottom - vMergeCellDataDrawTop >= FRows[vR].Height, '计划使用Continue但待确认会符合情况的');
        vCellData := FRows[vDestRow][vDestCol].CellData;  // 目标CellData，20170208003 如果移到if vDrawData外面则20170208002不需要了
        vCellScreenTop := Math.Max(ADataScreenTop, vCellDataDrawTop - FCellVPaddingPix);  // 屏显最上端
        if vCellScreenTop - vDestCellDataDrawTop < vCellData.Height then  // 可显示的起始位置小于数据高度(当>=时说明数据高度小于行高时，数据已经完全卷到上面了)
        begin
          vCellRect := Rect(vCellDrawLeft, vCellScreenTop, vCellDrawLeft + FRows[vR][vC].Width, vCellScreenBottom);

          if (Self.IsSelectComplate or vCellData.CellSelectedAll) and (not APaintInfo.Print) then  // 表格全选中或单元格全选中
          begin
            ACanvas.Brush.Color := OwnerData.Style.SelColor;
            ACanvas.FillRect(vCellRect);
          end
          else  // 默认的绘制
          begin
            vDrawDefault := True;
            if Assigned(FOnCellPaintBK) then  // 有外部自定义绘制
              FOnCellPaintBK(Self, FRows[vDestRow][vDestCol], vCellRect, ACanvas, APaintInfo, vDrawDefault);

            if vDrawDefault then  // 允许默认绘制
            begin
              if IsFixRow(vR) or IsFixCol(vC) then  // 是固定行
                ACanvas.Brush.Color := clBtnFace
              else
              if FRows[vDestRow][vDestCol].BackgroundColor <> HCTransparentColor then  // 背景色
                ACanvas.Brush.Color := FRows[vDestRow][vDestCol].BackgroundColor
              else
                ACanvas.Brush.Style := bsClear;

              ACanvas.FillRect(vCellRect);
            end;
          end;

          {$IFDEF SHOWDRAWITEMNO}
          ACanvas.Font.Color := clGray;
          ACanvas.Font.Style := [];
          ACanvas.Font.Size := 8;
          ACanvas.TextOut(vCellDrawLeft + 1, vDestCellDataDrawTop, IntToStr(vR) + '/' + IntToStr(vC));
          {$ENDIF}
          // 获取可显示区域的起始、结束DrawItem
          //vCellData.GetDataDrawItemRang(Math.Max(vCellScreenTop - vDestCellDataDrawTop, 0),
          //  vCellScreenBottom - vDestCellDataDrawTop, vFristDItemNo, vLastDItemNo);
          //if vFristDItemNo >= 0 then
          if vCellScreenBottom - vCellScreenTop > FCellVPaddingPix then  // 有可显示的DrawItem
          begin
            FRows[vDestRow][vDestCol].PaintTo(
              vCellDrawLeft, vDestCellDataDrawTop - FCellVPaddingPix,
              vCellDrawLeft + FColWidths[vC] + GetColSpanWidth(vDestRow, vDestCol),
              ADataDrawBottom, ADataScreenTop, ADataScreenBottom,
              0, FCellHPaddingPix, FCellVPaddingPix, ACanvas, APaintInfo);

            if Assigned(FOnCellPaintData) then  // 有外部自定义绘制
            begin
              FOnCellPaintData(Self, ADrawRect, vCellRect, vDestRow, vDestCol,
                vDestCellDataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom,
                ACanvas, APaintInfo);
            end;
          end;
        end;
      end;
      {$ENDREGION}
      {$REGION ' 绘制各单元格边框线 '}
      if FBorderVisible or (not APaintInfo.Print) then
      begin
        vDrawBorder := True;
        // 目标单元格的上边框绘制位置 vDestCellDataDrawTop本身占掉了1像素
        // FBorderWidth + FCellVPadding = vDestCellDataDrawTop，vDestCellDataDrawTop和FCellVapdding重叠了1像素
        vBorderTop := vDestCellDataDrawTop - FCellVPaddingPix - FBorderWidthPix;
        vBorderBottom := vBorderTop + FBorderWidthPix  // 计算边框最下端
          + Max(FRows[vR].Height, FRows[vDestRow][vDestCol].Height);  // 由于可能是合并目标单元格，所以用单元格高和行高最高的
        // 目标单元格底部边框超过页底部，计算收敛位置
        if vBorderBottom > ADataScreenBottom then  // 目标底部边框 > 页数据屏显底部，底部显示不全或底部跨到下一页了
        begin
          if FRows[vR][vC].RowSpan > 0 then  // 是合并目标单元格
          begin
            vSrcRowBorderTop := vBorderTop;
            vDestRow2 := vR;  // 借用变量
            while vDestRow2 <= FRows.Count - 1 do  // 找显示底部边框的源
            begin
              vSrcRowBorderTop := vSrcRowBorderTop + FRows[vDestRow2].FmtOffset + FRows[vDestRow2].Height + FBorderWidthPix;
              if vSrcRowBorderTop > ADataScreenBottom then  // 此合并源单元格所在的行底部边框显示不出来了
              begin
                if vSrcRowBorderTop > ADataDrawBottom then  // 跨页
                begin
                  CheckRowBorderShouLian(vDestRow2);  // 从当前行找收敛
                  vBorderBottom := vShouLian;  //为什么是2 Min(vBorderBottom, vShouLian);  // ADataDrawBottom
                end;

                Break;
              end;

              Inc(vDestRow2);
            end;
          end
          else
          if FRows[vR][vC].RowSpan < 0 then  // 合并源单元格，由于开始处的20170208001判断，所以此处肯定是合并目标正下方的单元格
          begin
            if vR <> vFirstDrawRow then  // 不是第一次绘制的行，说明由源自己处理了目标的底部边框了
              vDrawBorder := False
            else  // 跨页后第一次绘制
            begin
              { 移动到当前行起始位置 }
              vSrcRowBorderTop := vBorderTop;  // 借用变量，vBorderTop值是目标单元格的上边框
              for vDestRow2 := vDestRow to vR - 1 do
                vSrcRowBorderTop := vSrcRowBorderTop + FRows[vDestRow2].Height + FBorderWidthPix;

              // 我是跨页后目标单元格正在此页源的第一个，我要负责目标在此页的边框
              vDestRow2 := vR;  // 借用变量
              while vDestRow2 <= FRows.Count - 1 do  // 找显示底部边框的源
              begin
                vSrcRowBorderTop := vSrcRowBorderTop + FRows[vDestRow2].Height + FBorderWidthPix;
                if vSrcRowBorderTop > ADataScreenBottom then  // 此合并源单元格所在的行底部边框显示不出来了
                begin
                  if vSrcRowBorderTop > ADataDrawBottom then  // 跨页
                  begin
                    CheckRowBorderShouLian(vDestRow2);  // 从当前行找收敛
                    vBorderBottom := vShouLian;  //为什么是2 Min(vBorderBottom, vShouLian);  // ADataDrawBottom
                  end;

                  Break;
                end;

                Inc(vDestRow2);
              end;
            end;
          end
          else  // 普通单元格(不是合并目标也不是合并源)跨页，计算收敛
          begin
            CheckRowBorderShouLian(vR);
            vBorderBottom := vShouLian;
          end;
        end;

        {if Cells[vR, vC].RowSpan < 0 then  // 合并源单元格，由于开始处的20170208001判断，所以此处肯定是合并目标正下方的单元格
        begin
          if vR <> vFirstDrawRow then  // 不是第一次绘制的行，说明由源自己处理了目标的底部边框了
            vDrawBorder := False
          else
          begin
            // 移动到当前行起始位置
            vSrcRowBorderTop := vBorderTop;  // 借用变量，vBorderTop值是目标单元格的上边框
            for vDestRow2 := vDestRow to vR - 1 do
              vSrcRowBorderTop := vSrcRowBorderTop + FRows[vDestRow2].Height + FBorderWidth;

            // 我是跨页后目标单元格正在此页源的第一个，我要负责目标在此页的边框
            vDestRow2 := vR;  // 借用变量
            while vDestRow2 <= FRows.Count - 1 do  // 找显示底部边框的源
            begin
              vSrcRowBorderTop := vSrcRowBorderTop + FRows[vDestRow2].Height + FBorderWidth;
              if vSrcRowBorderTop > ADataScreenBottom then  // 此合并源单元格所在的行底部边框显示不出来了
              begin
                if vSrcRowBorderTop > ADataDrawBottom then  // 跨页
                begin
                  CheckRowBorderShouLian(vDestRow2);  // 从当前行找收敛
                  vBorderBottom := vShouLian;  //为什么是2 Min(vBorderBottom, vShouLian);  // ADataDrawBottom
                end;

                Break;
              end;

              Inc(vDestRow2);
            end;
          end;
        end;}

        if vDrawBorder then  // 边框可以显示
        begin
          if APaintInfo.Print then
            ACanvas.Pen.Width := Max(1, HCUnitConversion.PtToPixel(FBorderWidthPt, APaintInfo.DPI))
          else
            ACanvas.Pen.Width := FBorderWidthPix;

          if FBorderVisible then  // 未隐藏边框
          begin
            ACanvas.Pen.Color := clBlack;
            ACanvas.Pen.Style := psSolid;
          end
          else
          if not APaintInfo.Print then
          begin
            ACanvas.Pen.Color := clActiveBorder;
            ACanvas.Pen.Style := psDot;
          end;

          vBorderLeft := vCellDrawLeft - FBorderWidthPix;
          vBorderRight := vCellDrawLeft + FColWidths[vC] + GetColSpanWidth(vDestRow, vDestCol);
          {vDestCol2 := FRows[vDestRow][vDestCol].ColSpan;  // 借用变量
          while vDestCol2 > 0 do
          begin
            vBorderRight := vBorderRight + FBorderWidth + FColWidths[vDestCol + vDestCol2];
            Dec(vDestCol2);
          end;}

          if (vBorderTop < ADataScreenTop) and (ADataDrawTop >= 0) then  // 表格当前行显示不全或表格当前行跨页此时在下一页绘制
            vBorderTop := ADataScreenTop;

          {if GetObjectType(ACanvas.Pen.Handle) = OBJ_EXTPEN then
          begin
            vExtPen := ACanvas.Pen.Handle;
            //vBottom := GetObject(ACanvas.Pen.Handle, 0, nil);
            //GetObject(ACanvas.Pen.Handle, vBottom, vExtPen);
          end
          else}
          vExtPen := CreateExtPen(ACanvas.Pen);  // 因为默认的画笔没有线帽的控制，新增支持线帽的画笔
          vOldPen := SelectObject(ACanvas.Handle, vExtPen);
          try
            if (vBorderTop >= 0) and (cbsTop in FRows[vR][vC].BorderSides) then  // 上边框可显示
            begin
              if APaintInfo.Print then
              begin
                APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
                  vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);
              end
              else
              begin
                ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);   // 左上
                ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);  // 右上
              end;
            end;

            if cbsRight in FRows[vR][vC].BorderSides then  // 右边框
            begin
              if APaintInfo.Print then
              begin
                APaintInfo.DrawNoScaleLine(ACanvas, vBorderRight + vBorderOffs, vBorderTop + vBorderOffs,
                  vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
              end
              else
              begin
                ACanvas.MoveTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);  // 右上
                ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);  // 右下
              end;
            end;

            if (vBorderBottom <= ADataScreenBottom) and (cbsBottom in FRows[vR][vC].BorderSides) then  // 下边框
            begin
              if APaintInfo.Print then
              begin
                APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs,
                  vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
              end
              else
              begin
                ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);  // 左下
                ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);  // 右下
              end;
            end;

            if cbsLeft in FRows[vR][vC].BorderSides then  // 左边框
            begin
              if APaintInfo.Print then
              begin
                APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
                  vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
              end
              else
              begin
                ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);
                ACanvas.LineTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
              end;
            end;

            if cbsLTRB in FRows[vR][vC].BorderSides then  // 左上右下对角线
            begin
              if APaintInfo.Print then
              begin
                APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
                  vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
              end
              else
              begin
                ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);
                ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
              end;
            end;

            if cbsRTLB in FRows[vR][vC].BorderSides then  // 右上左下对角线
            begin
              if APaintInfo.Print then
              begin
                APaintInfo.DrawNoScaleLine(ACanvas, vBorderRight + vBorderOffs, vBorderTop + vBorderOffs,
                  vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
              end
              else
              begin
                ACanvas.MoveTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);
                ACanvas.LineTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
              end;
            end;
          finally
            SelectObject(ACanvas.Handle, vOldPen);
            DeleteObject(vExtPen);
          end;

          // "最后一列"负责绘制分页标识
          vDestCol2 := vC + FRows[vR][vC].ColSpan;
          if (not APaintInfo.Print) and (vDestCol2 = FColWidths.Count - 1) then  // 非打印、最后一列绘制分页标识
          begin
            if vCellDataDrawTop + FRows[vR].Height - FCellVPaddingPix > ADataDrawBottom then  // 本行跨页，分页符(本页结尾)
              DoDrawPageBreakMark(True)
            else
            if (vR < Self.RowCount - 1)
              and (vBorderBottom + FRows[vR + 1].FmtOffset + FRows[vR + 1].Height > ADataDrawBottom)
            then  // 下一行跨页
            begin
              if FRows[vR + 1].FmtOffset > 0 then  // 下一行整体下移了，分页符(本页结尾)
                DoDrawPageBreakMark(True)
              else
              if vBorderBottom = ADataDrawBottom then  //* 下一行起始于本页结尾，
                DoDrawPageBreakMark(True);             //* 此时下一行不在本页显示，但FmtOffset并不大于0，
            end;                                       //* 如果这里不处理，循环下一行时底部大于当前页直接跳出循环失去绘制机会

            if (vFirstDrawRow <> 0)  // 起始行不是第一行
              and (vR = vFirstDrawRow)  // 起始行绘制
              and (ADrawRect.Top < ADataDrawTop)  // 第一行在上一页
            then  // 分页符(本页起始)
              DoDrawPageBreakMark(False);
          end;
        end;
      end;
      {$ENDREGION}

      vCellDrawLeft := vCellDrawLeft + FColWidths[vC] + FBorderWidthPix;  // 同行下一列的起始Left位置
    end;

    vCellDataDrawTop := vCellDataDrawBottom + FCellVPaddingPix + FBorderWidthPix;  // 下一行的Top位置
  end;

  if vFirstDrawRowIsBreak then  // 绘制标题行
    PaintFixRows(ADrawRect.Left, ADataDrawTop, ADataScreenBottom, ACanvas, APaintInfo);

  if (FFixCol >= 0) and (GetFixColLeft + ADrawRect.Left < 0) then  // 绘制标题列
    PaintFixCols(ADrawRect.Top, 0, ADataDrawTop, ADataScreenBottom, ACanvas, APaintInfo);

  {$REGION ' 绘制拖动线 '}
  if Resizing and (FResizeInfo.TableSite = tsBorderRight) then  // 垂直
  begin
    ACanvas.Pen.Color := Self.FBorderColor;
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Width := 1;
    ACanvas.MoveTo(ADrawRect.Left + FResizeInfo.DestX, Max(ADataDrawTop, ADrawRect.Top));
    ACanvas.LineTo(ADrawRect.Left + FResizeInfo.DestX, Min(ADataDrawBottom,
      Min(ADrawRect.Bottom, vBorderBottom)));
  end
  else
  if Resizing and (FResizeInfo.TableSite = tsBorderBottom) then  // 水平
  begin
    ACanvas.Pen.Color := Self.FBorderColor;
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Width := 1;
    ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Top + FResizeInfo.DestY);
    ACanvas.LineTo(ADrawRect.Right, ADrawRect.Top + FResizeInfo.DestY);
  end;
  {$ENDREGION}

end;

procedure THCTableItem.DoSelfRedo(const ARedo: THCUndo);
var
  vCellUndoData: THCCellUndoData;
  vMulCellUndoData: THCMulCellUndoData;
  vColSizeUndoData: THCColSizeUndoData;
  vRowSizeUndoData: THCRowSizeUndoData;
  vMirrorUndoData: THCMirrorUndoData;
  vStream: TMemoryStream;
  vStyleNo: Integer;
begin
  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;

  if ARedo.Data is THCMulCellUndoData then
  begin
    vMulCellUndoData := ARedo.Data as THCMulCellUndoData;
    FRows[vMulCellUndoData.Row][vMulCellUndoData.Col].CellData.Redo(ARedo);
    Self.FormatDirty;
  end
  else
  if ARedo.Data is THCCellUndoData then
  begin
    vCellUndoData := ARedo.Data as THCCellUndoData;
    FSelectCellRang.StartRow := vCellUndoData.Row;
    FSelectCellRang.StartCol := vCellUndoData.Col;

    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      begin
        FRows[vCellUndoData.Row][vCellUndoData.Col].CellData.Redo(ARedo);
      end);
  end
  else
  if ARedo.Data is THCColSizeUndoData then
  begin
    vColSizeUndoData := ARedo.Data as THCColSizeUndoData;

    if FResizeKeepWidth and (vColSizeUndoData.Col < FColWidths.Count - 1) then
    begin
      FColWidths[vColSizeUndoData.Col + 1] := FColWidths[vColSizeUndoData.Col + 1]
        - vColSizeUndoData.NewWidth - vColSizeUndoData.OldWidth;
    end;

    FColWidths[vColSizeUndoData.Col] := vColSizeUndoData.NewWidth;
    Self.FormatDirty;
  end
  else
  if ARedo.Data is THCRowSizeUndoData then
  begin
    vRowSizeUndoData := ARedo.Data as THCRowSizeUndoData;
    FRows[vRowSizeUndoData.Row].Height := vRowSizeUndoData.NewHeight;
    Self.FormatDirty;
  end
  else
  if ARedo.Data is THCMirrorUndoData then
  begin
    vStream := TMemoryStream.Create;
    try
      Self.SaveToStream(vStream);  // 记录恢复前状态

      vMirrorUndoData := ARedo.Data as THCMirrorUndoData;
      vMirrorUndoData.Stream.Position := 0;
      vMirrorUndoData.Stream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));
      Self.LoadFromStream(vMirrorUndoData.Stream, OwnerData.Style, HC_FileVersionInt);

      vMirrorUndoData.Stream.Clear;
      vMirrorUndoData.Stream.CopyFrom(vStream, 0);  // 保存恢复前状态
      Self.FormatDirty;
    finally
      vStream.Free;
    end;
  end
  else
    inherited DoSelfRedo(ARedo);
end;

procedure THCTableItem.DoRowAdd(const ARow: THCTableRow);
var
  i: Integer;
  vCellData: THCTableCellData;
begin
  for i := 0 to ARow.ColCount - 1 do
  begin
    vCellData := ARow[i].CellData;
    if vCellData <> nil then
      InitializeCellData(vCellData);
  end;
end;

procedure THCTableItem.DoSelfUndo(const AUndo: THCUndo);
var
  vCellUndoData: THCCellUndoData;
  vMulCellUndoData: THCMulCellUndoData;
  vColSizeUndoData: THCColSizeUndoData;
  vRowSizeUndoData: THCRowSizeUndoData;
  vMirrorUndoData: THCMirrorUndoData;
  vStyleNo: Integer;
  vStream: TMemoryStream;
begin
  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;

  if AUndo.Data is THCMulCellUndoData then
  begin
    vMulCellUndoData := AUndo.Data as THCMulCellUndoData;
    FRows[vMulCellUndoData.Row][vMulCellUndoData.Col].CellData.Undo(AUndo);
    Self.FormatDirty;
  end
  else
  if AUndo.Data is THCCellUndoData then
  begin
    vCellUndoData := AUndo.Data as THCCellUndoData;
    FSelectCellRang.StartRow := vCellUndoData.Row;
    FSelectCellRang.StartCol := vCellUndoData.Col;

    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      begin
        FRows[vCellUndoData.Row][vCellUndoData.Col].CellData.Undo(AUndo);
      end);
  end
  else
  if AUndo.Data is THCColSizeUndoData then
  begin
    vColSizeUndoData := AUndo.Data as THCColSizeUndoData;
    if FResizeKeepWidth and (vColSizeUndoData.Col < FColWidths.Count - 1) then
    begin
      FColWidths[vColSizeUndoData.Col + 1] := FColWidths[vColSizeUndoData.Col + 1]
        + vColSizeUndoData.NewWidth - vColSizeUndoData.OldWidth;
    end;

    FColWidths[vColSizeUndoData.Col] := vColSizeUndoData.OldWidth;
    Self.FormatDirty;
  end
  else
  if AUndo.Data is THCRowSizeUndoData then
  begin
    vRowSizeUndoData := AUndo.Data as THCRowSizeUndoData;
    FRows[vRowSizeUndoData.Row].Height := vRowSizeUndoData.OldHeight;
    Self.FormatDirty;
  end
  else
  if AUndo.Data is THCMirrorUndoData then
  begin
    vStream := TMemoryStream.Create;
    try
      Self.SaveToStream(vStream);  // 记录撤销前状态
      // 恢复原样
      vMirrorUndoData := AUndo.Data as THCMirrorUndoData;
      vMirrorUndoData.Stream.Position := 0;
      vMirrorUndoData.Stream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));
      Self.LoadFromStream(vMirrorUndoData.Stream, OwnerData.Style, HC_FileVersionInt);

      vMirrorUndoData.Stream.Clear;
      vMirrorUndoData.Stream.CopyFrom(vStream, 0);  // 保存撤销前状态
      Self.FormatDirty;
    finally
      vStream.Free;
    end;
  end
  else
    inherited DoSelfUndo(AUndo);
end;

procedure THCTableItem.DoSelfUndoDestroy(const AUndo: THCUndo);
begin
  if AUndo.Data is THCCellUndoData then
  begin
    (AUndo.Data as THCCellUndoData).Free;
    AUndo.Data := nil;
  end;

  inherited DoSelfUndoDestroy(AUndo);
end;

procedure THCTableItem.KeyDown(var Key: Word; Shift: TShiftState);
var
  vEditCell: THCTableCell;

  function DoCrossCellKey(const AKey: Word): Boolean;
  var
    i, vRow, vCol, vOldRow, vOldCol: Integer;
  begin
    Result := False;

    vOldRow := FSelectCellRang.StartRow;
    vOldCol := FSelectCellRang.StartCol;

    vRow := -1;
    vCol := -1;

    {$REGION 'VK_LEFT'}
    if AKey = VK_LEFT then
    begin
      if vEditCell.CellData.SelectFirstItemOffsetBefor then
      begin
        // 找左侧单元格
        for i := FSelectCellRang.StartCol - 1 downto 0 do
        begin
          if FRows[FSelectCellRang.StartRow][i].ColSpan >= 0 then
          begin
            if FRows[FSelectCellRang.StartRow][i].RowSpan < 0 then
              FSelectCellRang.StartRow := FSelectCellRang.StartRow + FRows[FSelectCellRang.StartRow][i].RowSpan;

            vCol := i;
            Break;
          end;
        end;

        if vCol >= 0 then
        begin
          if (vOldRow >= 0) and (vOldCol >= 0) then
            FRows[vOldRow][vOldCol].Active := False;

          FSelectCellRang.StartCol := vCol;
          FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].Active := True;
          FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.SelectLastItemAfterWithCaret;
          Result := True;
        end;
      end;
    end
    {$ENDREGION}
    else
    {$REGION 'VK_RIGHT'}
    if AKey = VK_RIGHT then
    begin
      if vEditCell.CellData.SelectLastItemOffsetAfter then
      begin
        // 找右侧单元格
        for i := FSelectCellRang.StartCol + 1 to FColWidths.Count - 1 do
        begin
          if FRows[FSelectCellRang.StartRow][i].ColSpan >= 0 then
          begin
            if FRows[FSelectCellRang.StartRow][i].RowSpan < 0 then
              FSelectCellRang.StartRow := FSelectCellRang.StartRow + FRows[FSelectCellRang.StartRow][i].RowSpan;

            vCol := i;
            Break;
          end;
        end;

        if vCol >= 0 then
        begin
          if (vOldRow >= 0) and (vOldCol >= 0) then
            FRows[vOldRow][vOldCol].Active := False;

          FSelectCellRang.StartCol := vCol;
          FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].Active := True;
          FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.SelectFirstItemBeforWithCaret;
          Result := True;
        end;
      end
    end
    {$ENDREGION}
    else
    {$REGION 'VK_UP'}
    if AKey = VK_UP then
    begin
      if (vEditCell.CellData.SelectFirstLine) and (FSelectCellRang.StartRow > 0) then  // 找上一行单元格
      begin
        GetDestCell(FSelectCellRang.StartRow - 1, FSelectCellRang.StartCol, vRow, vCol);

        if (vRow >= 0) and (vCol >= 0) then
        begin
          if (vOldRow >= 0) and (vOldCol >= 0) then
            FRows[vOldRow][vOldCol].Active := False;

          FSelectCellRang.StartRow := vRow;
          FSelectCellRang.StartCol := vCol;
          FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].Active := True;
          FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.SelectLastItemAfterWithCaret;
          Result := True;
        end;
      end;
    end
    {$ENDREGION}
    else
    {$REGION 'VK_DOWN'}
    if AKey = VK_DOWN then
    begin
      if (vEditCell.CellData.SelectLastLine) and (FSelectCellRang.StartRow < Self.RowCount - 1) then  // 找下一行单元格
      begin
        GetDestCell(FSelectCellRang.StartRow + 1, FSelectCellRang.StartCol, vRow, vCol);
        if ((vRow <> FSelectCellRang.StartRow) or (vCol <> FSelectCellRang.StartCol))  // 同列下一个单元格的目标不是我
          and (vRow >= 0) and (vCol >= 0) // 下一行有有效的单元格
        then
        begin
          if (vOldRow >= 0) and (vOldCol >= 0) then
            FRows[vOldRow][vOldCol].Active := False;

          FSelectCellRang.StartRow := vRow;
          FSelectCellRang.StartCol := vCol;
          FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].Active := True;
          FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.SelectFirstItemBeforWithCaret;
          Result := True;
        end;
      end;
    end;
    {$ENDREGION}
  end;

var
  vOldKey: Word;
begin
  Self.SizeChanged := False;

  vEditCell := GetEditCell;
  if vEditCell <> nil then
  begin
    vOldKey := Key;
    case Key of
      VK_BACK, VK_DELETE, VK_RETURN, VK_TAB:
        begin
          CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
            procedure
            begin
              vEditCell.CellData.KeyDown(vOldKey, Shift);
            end);
        end;

      VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END:
        begin
          vEditCell.CellData.KeyDown(vOldKey, Shift);
          if (vOldKey = 0) and IsDirectionKey(Key) then  // 单元格Data没处理，且是方向键
          begin
            if DoCrossCellKey(Key) then  // 方向键移动到其他单元格
            begin
              OwnerData.Style.UpdateInfoReCaret;
              Key := vOldKey;
            end;
          end;
        end;
    end;
  end
  else
    Key := 0;
end;

procedure THCTableItem.KeyPress(var Key: Char);
var
  vOldKey: Char;
  vEditCell: THCTableCell;
begin
  vEditCell := GetEditCell;
  if vEditCell <> nil then
  begin
    vOldKey := Key;
    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      begin
        vEditCell.CellData.KeyPress(vOldKey);
      end);
    Key := vOldKey;
  end;
end;

procedure THCTableItem.KillFocus;
begin
  // 如果多个单元格选中，由下面初始化后再点击会按点在选中处，而选中按下行列已被清除
  // 另外外部工具条操作时也已经失去了按下时的行列，所以要么不初始化，要么彻底初始化
  //Self.InitializeMouseInfo;
end;

procedure THCTableItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  i, vR, vC, vWidth: Integer;
  vAutoHeight: Boolean;
  vRow: THCTableRow;
begin
  FRows.Clear;
  inherited LoadFromStream(AStream, AStyle, AFileVersion);

  AStream.ReadBuffer(FBorderVisible, SizeOf(FBorderVisible));

  if AFileVersion > 31 then
  begin
    AStream.ReadBuffer(FBorderWidthPt, SizeOf(FBorderWidthPt));
    FBorderWidthPix := HCUnitConversion.PtToPixel(FBorderWidthPt, HCUnitConversion.PixelsPerInchX);
  end
  else
  if AFileVersion > 29 then
  begin
    AStream.ReadBuffer(FBorderWidthPix, SizeOf(FBorderWidthPix));
    FBorderWidthPt := Min(0.5, HCUnitConversion.PixelToPt(FBorderWidthPix, HCUnitConversion.PixelsPerInchX));
  end;

  if AFileVersion > 34 then
  begin
    AStream.ReadBuffer(FCellVPaddingMM, SizeOf(FCellVPaddingMM));
    FCellVPaddingPix := HCUnitConversion.MillimeterToPixY(FCellVPaddingMM);
    AStream.ReadBuffer(FCellHPaddingMM, SizeOf(FCellHPaddingMM));
    FCellHPaddingPix := HCUnitConversion.MillimeterToPixY(FCellHPaddingMM);
  end
  else
  begin
    FCellVPaddingMM := HCUnitConversion.PixYToMillimeter(2);
    FCellVPaddingPix := 2;
    FCellHPaddingMM := HCUnitConversion.PixXToMillimeter(2);
    FCellHPaddingPix := 2;
  end;

  AStream.ReadBuffer(vR, SizeOf(vR));  // 行数
  AStream.ReadBuffer(vC, SizeOf(vC));  // 列数

  if AFileVersion > 24 then  // 固定行数、列数信息
  begin
    AStream.ReadBuffer(FFixRow, SizeOf(FFixRow));
    AStream.ReadBuffer(FFixRowCount, SizeOf(FFixRowCount));
    AStream.ReadBuffer(FFixCol, SizeOf(FFixCol));
    AStream.ReadBuffer(FFixColCount, SizeOf(FFixColCount));
  end;

  { 创建行、列 }
  for i := 0 to vR - 1 do
  begin
    vRow := THCTableRow.Create(OwnerData.Style, vC);  // 注意行创建时是table拥有者的Style，加载时是传入的AStyle
    FRows.Add(vRow);
  end;

  { 加载各列标准宽度 }
  FColWidths.Clear;
  for i := 0 to vC - 1 do
  begin
    AStream.ReadBuffer(vWidth, SizeOf(vWidth));
    FColWidths.Add(vWidth);
  end;

  { 加载各列数据 }
  for vR := 0 to FRows.Count - 1 do
  begin
    AStream.ReadBuffer(vAutoHeight, SizeOf(Boolean));
    FRows[vR].AutoHeight := vAutoHeight;
    if not FRows[vR].AutoHeight then
    begin
      AStream.ReadBuffer(vWidth, SizeOf(Integer));
      FRows[vR].Height := vWidth;
    end;

    for vC := 0 to FRows[vR].ColCount - 1 do
    begin
      FRows[vR][vC].CellData.Width := FColWidths[vC] - 2 * FCellHPaddingPix;
      FRows[vR][vC].LoadFromStream(AStream, AStyle, AFileVersion);
    end;
  end;
end;

function THCTableItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  vMouseDownRow, vMouseDownCol: Integer; // abstract vBottom;
  vCell: THCTableCell;
  vCellPt: TPoint;
begin
  Result := inherited MouseDown(Button, Shift, X, Y);
  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);
  FOutSelectInto := False;
  FSelecting := False;  // 准备划选
  FDraging := False;  // 准备拖拽
  FOutsideInfo.Row := -1;

  FResizeInfo := GetCellAt(X, Y, vMouseDownRow, vMouseDownCol);

  Resizing := (Button = mbLeft) and ((FResizeInfo.TableSite = tsBorderRight) or (FResizeInfo.TableSite = tsBorderBottom));
  if Resizing then
  begin
    if (FMouseDownRow <> vMouseDownRow) or (FMouseDownCol <> vMouseDownCol) then
    begin
      if FMouseDownRow >= 0 then
        FRows[FMouseDownRow][FMouseDownCol].Active := False;

      FMouseDownRow := vMouseDownRow;
      FMouseDownCol := vMouseDownCol;
    end;

    FMouseDownX := X;
    FMouseDownY := Y;
    OwnerData.Style.UpdateInfoRePaint;
    Exit;
  end;

  if FResizeInfo.TableSite = tsCell then
  begin
    if CoordInSelect(X, Y) then  // 在选中区域中（不包括边框线及边框线容差）
    begin
      if FMouseLBDowning then
        FDraging := True;

      if (FMouseDownRow <> vMouseDownRow) or (FMouseDownCol <> vMouseDownCol) then
      begin
        if (FMouseDownRow >= 0) and (Button <> mbRight) then
          FRows[FMouseDownRow][FMouseDownCol].Active := False;

        FMouseDownRow := vMouseDownRow;  // 记录拖拽起始单元格
        FMouseDownCol := vMouseDownCol;
      end;

      vCellPt := GetCellPostion(FMouseDownRow, FMouseDownCol);
      FRows[FMouseDownRow][FMouseDownCol].MouseDown(Button, Shift,
        X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);
    end
    else  // 不在选中区域中
    begin
      // 如果先执行 DisSelect 会清除Mouse信息，导致当前编辑单元格不能响应取消激活事件
      if (vMouseDownRow <> FMouseDownRow) or (vMouseDownCol <> FMouseDownCol) then  // 新位置
      begin
        vCell := GetEditCell;
        if vCell <> nil then  // 取消原来编辑
          vCell.Active := False;

        OwnerData.Style.UpdateInfoReCaret;
      end;

      DisSelect;  // 清除原选中

      if (FMouseDownRow <> vMouseDownRow) or (FMouseDownCol <> vMouseDownCol) then
      begin
        if FMouseDownRow >= 0 then
          FRows[FMouseDownRow][FMouseDownCol].Active := False;

        FMouseDownRow := vMouseDownRow;
        FMouseDownCol := vMouseDownCol;
      end;

      FSelectCellRang.SetStart(FMouseDownRow, FMouseDownCol);

      vCellPt := GetCellPostion(FMouseDownRow, FMouseDownCol);
      FRows[FMouseDownRow][FMouseDownCol].MouseDown(Button, Shift,
        X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);
    end;
  end
  else  // 不在单元格内
  begin
    DisSelect;  // 取消原来选中
    Self.InitializeMouseInfo;

    if FResizeInfo.TableSite = tsOutside then  // 在四周外围
    begin
      FOutsideInfo.Row := vMouseDownRow;  // 左右边时对应的行
      FOutsideInfo.Leftside := X < 0;  // 左边
    end;
  end;
end;

procedure THCTableItem.MouseLeave;
begin
  inherited;
  if (FMouseMoveRow < 0) or (FMouseMoveCol < 0) then Exit;
  if FRows[FMouseMoveRow][FMouseMoveCol].CellData <> nil then
    FRows[FMouseMoveRow][FMouseMoveCol].CellData.MouseLeave;  // .MouseMove([], -1, -1);  // 处理鼠标移上高亮在迅速移出表格后不能恢复的问题

  if not SelectExists then
    Self.InitializeMouseInfo;
end;

function THCTableItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  vMoveRow, vMoveCol: Integer;

  {$REGION 'AdjustSelectRang'}

  procedure AdjustSelectRang;
  var
    vR, vC: Integer;
  begin
    // 先清除起始单元格之外的，以便下面重新处理选中单元格的全选
    if FSelectCellRang.StartRow >= 0 then
    begin
      for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
      begin
        for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
        begin
          if ((vR = FMouseDownRow) and (vC = FMouseDownCol))
            //or ((vRow = vMoveRow) and (vCol = vMoveCol))
          then  // 保留当前按下的选中信息，防止回到按下中做内容的选中

          else
          begin
            if FRows[vR][vC].CellData <> nil then
              FRows[vR][vC].CellData.DisSelect;
          end;
        end;
      end;
    end;

    if FMouseDownRow < 0 then  // 从表格外面选到里面
    begin
      if vMoveRow = 0 then  // 从上面选入
      begin
        FMouseDownRow := 0;
        FMouseDownCol := 0;

        FSelectCellRang.SetStart(FMouseDownRow, FMouseDownCol);
        FSelectCellRang.SetEnd(vMoveRow, vMoveCol);
      end
      else  // 从下面选入
      begin
        GetDestCell(Self.RowCount - 1, Self.FColWidths.Count - 1, vR, vC);
        FMouseDownRow := vR;
        FMouseDownCol := vC;

        FSelectCellRang.SetStart(vMoveRow, vMoveCol);
        FSelectCellRang.SetEnd(FMouseDownRow, FMouseDownCol);
      end;

      FOutSelectInto := True;
    end
    else
    if FMouseMoveRow > FMouseDownRow then  // 移动行在按下行下面
    begin
      FSelectCellRang.StartRow := FMouseDownRow;
      FSelectCellRang.EndRow := FMouseMoveRow;

      if FMouseMoveCol < FMouseDownCol then  // 移动列在按下列前面
      begin
        FSelectCellRang.StartCol := FMouseMoveCol;
        FSelectCellRang.EndCol := FMouseDownCol;
      end
      else
      begin
        FSelectCellRang.StartCol := FMouseDownCol;
        FSelectCellRang.EndCol := FMouseMoveCol;
      end;
    end
    else
    if FMouseMoveRow < FMouseDownRow then  // 移动行在按下行上面
    begin
      FSelectCellRang.StartRow := FMouseMoveRow;
      FSelectCellRang.EndRow := FMouseDownRow;

      if FMouseMoveCol < FMouseDownCol then  // 移动列在按下列前面
      begin
        FSelectCellRang.StartCol := FMouseMoveCol;
        FSelectCellRang.EndCol := FMouseDownCol;
      end
      else  // 移动列在按下前后面
      begin
        FSelectCellRang.StartCol := FMouseDownCol;
        FSelectCellRang.EndCol := FMouseMoveCol;
      end;
    end
    else  // FMouseMoveRow = FMouseDownRow 移动行 = 按下行
    begin
      FSelectCellRang.StartRow := FMouseDownRow;
      FSelectCellRang.EndRow := FMouseMoveRow;

      if FMouseMoveCol > FMouseDownCol then  // 移动列在按下列右边
      begin
        FSelectCellRang.StartCol := FMouseDownCol;
        FSelectCellRang.EndCol := FMouseMoveCol;
      end
      else
      if FMouseMoveCol < FMouseDownCol then  // 移动列在按下列左边
      begin
        FSelectCellRang.StartCol := FMouseMoveCol;
        FSelectCellRang.EndCol := FMouseDownCol;
      end
      else  // 移动列 = 按下列
      begin
        FSelectCellRang.StartCol := FMouseDownCol;
        FSelectCellRang.EndCol := FMouseMoveCol;
      end;
    end;

    if (FSelectCellRang.StartRow = FSelectCellRang.EndRow)
      and (FSelectCellRang.StartCol = FSelectCellRang.EndCol)
    then  // 不处理合并时，选中在同一单元格
      FSelectCellRang.InitializeEnd
    else
    begin
      if FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].IsMergeSource then  // 起始选择在合并源
      begin
        GetDestCell(FSelectCellRang.StartRow, FSelectCellRang.StartCol, vR, vC);
        FSelectCellRang.SetStart(vR, vC);
      end;

      if FRows[FSelectCellRang.EndRow][FSelectCellRang.EndCol].IsMergeDest then  // 结束在合并目标
      begin
        GetSourceCell(FSelectCellRang.EndRow, FSelectCellRang.EndCol, vR, vC);  // 获取目标方法如果传递的是目标得到的是源
        FSelectCellRang.SetEnd(vR, vC);
      end;

      if (FSelectCellRang.StartRow = FSelectCellRang.EndRow)
        and (FSelectCellRang.StartCol = FSelectCellRang.EndCol)
      then  // 修正合并后在同一单元格
        FSelectCellRang.InitializeEnd;
    end;
  end;
  {$ENDREGION}

  {$REGION 'MatchCellSelectState'}

  procedure MatchCellSelectState;
  var
    vR, vC: Integer;
  begin
    if not FSelectCellRang.EditCell then
    begin
      for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
      begin
        for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
        begin
          {if (vRow = vMoveRow) and (vCol = vMoveCol) then else 什么情况下需要跳过?}
          if FRows[vR][vC].CellData <> nil then
            FRows[vR][vC].CellData.SelectAll;
        end;
      end;
    end;
  end;
  {$ENDREGION}

var
  vCellPt: TPoint;
  vResizeInfo: TResizeInfo;
begin
  Result := True;
  if ActiveDataResizing then
  begin
    vCellPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
    FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].MouseMove(
      Shift, X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);

    Exit;
  end;

  if Resizing then  // (ssLeft in Shift)
  begin
    FResizeInfo.DestX := X;
    FResizeInfo.DestY := Y;
    OwnerData.Style.UpdateInfoRePaint;

    Exit;
  end;

  vResizeInfo := GetCellAt(X, Y, vMoveRow, vMoveCol);

  if vResizeInfo.TableSite = tsCell then  // 鼠标在单元格中
  begin
    if FMouseLBDowning or (Shift = [ssLeft]) then  // 左键按下移动，按下时在表格上 or 没有在表格上按下(划选进入)
    begin
      if FDraging or OwnerData.Style.UpdateInfo.Draging then
      begin
        FMouseMoveRow := vMoveRow;
        FMouseMoveCol := vMoveCol;
        vCellPt := GetCellPostion(FMouseMoveRow, FMouseMoveCol);
        FRows[FMouseMoveRow][FMouseMoveCol].MouseMove(Shift,
          X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);

        Exit;
      end;

      if not FSelecting then
        FSelecting := True;

      if (vMoveRow <> FMouseMoveRow) or (vMoveCol <> FMouseMoveCol) then  // 鼠标移动到新单元格
      begin
        FMouseMoveRow := vMoveRow;
        FMouseMoveCol := vMoveCol;

        AdjustSelectRang;  // 计算选中起始结束范围(会纠正从后、下往前选的情况)
        MatchCellSelectState;  // 处理选中范围内各单元格的选中状态
      end;

      {if (FSelectCellRang.StartRow = FMouseMoveRow)
        and (FSelectCellRang.StartCol = FMouseMoveCol)
      then}  // 选择起始和现在是同一个单元格
      begin
        vCellPt := GetCellPostion(FMouseMoveRow, FMouseMoveCol);
        FRows[FMouseMoveRow][FMouseMoveCol].MouseMove(Shift,
          X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);
      end;
    end
    else  // 鼠标移动，没有按键按下
    begin
      if (vMoveRow <> FMouseMoveRow) or (vMoveCol <> FMouseMoveCol) then  // 鼠标移动到新单元格
      begin
        if (FMouseMoveRow >= 0) and (FMouseMoveCol >= 0) then
        begin
          if FRows[FMouseMoveRow][FMouseMoveCol].CellData <> nil then
            FRows[FMouseMoveRow][FMouseMoveCol].CellData.MouseLeave;  // .MouseMove(Shift, -1, -1);  // 旧单元格移出
        end;

        FMouseMoveRow := vMoveRow;
        FMouseMoveCol := vMoveCol;
      end;

      if (FMouseMoveRow < 0) or (FMouseMoveCol < 0) then Exit;

      vCellPt := GetCellPostion(FMouseMoveRow, FMouseMoveCol);
      FRows[FMouseMoveRow][FMouseMoveCol].MouseMove(Shift,
        X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);
    end;
  end
  else  // 鼠标不在单元格中
  begin
    if (FMouseMoveRow >= 0) and (FMouseMoveCol >= 0) then
    begin
      if FRows[FMouseMoveRow][FMouseMoveCol].CellData <> nil then
        FRows[FMouseMoveRow][FMouseMoveCol].CellData.MouseLeave;  // 旧单元格移出
    end;

    FMouseMoveRow := -1;
    FMouseMoveCol := -1;

    if vResizeInfo.TableSite = tsBorderRight then // 鼠标不在单元格中
      GCursor := crHSplit
    else
    if vResizeInfo.TableSite = tsBorderBottom then
      GCursor := crVSplit;
  end;

  if OwnerData.Style.UpdateInfo.Draging then
    FSelectCellRang.SetStart(FMouseMoveRow, FMouseMoveCol);
end;

function THCTableItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  vCellPt: TPoint;
  vUpRow, vUpCol: Integer;
  vResizeInfo: TResizeInfo;
  //vMouseUpInSelect: Boolean;
begin
  Result := True;
  FMouseLBDowning := False;

  if ActiveDataResizing then
  begin
    vCellPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
    FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].MouseUp(
      Button, Shift, X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);

    Exit;
  end;

  if Resizing then  // 拖动改变列宽，单元格Data宽度的改变由重新格式化处理
  begin
    if FResizeInfo.TableSite = tsBorderRight then  // 拖宽/拖窄
    begin
      vCellPt.X := X - FMouseDownX;  // 不使用FResizeInfo.DestX(会造成按下处弹出也有偏移)
      if vCellPt.X <> 0 then
      begin
        // AReDest为False用于处理拖动改变列宽时，如拖动处列是合并源，其他行此列并无合并操作
        // 这时弹起，如果取拖动列目标列变宽，则其他行拖动处的列并没变宽
        vResizeInfo := GetCellAt(FMouseDownX, FMouseDownY, vUpRow, vUpCol, False{实际位置处的列});

        if (vResizeInfo.TableSite <> tsOutside) and (vCellPt.X <> 0) then  // 没弹起在外面
        begin
          if vCellPt.X > 0 then  // 拖宽了
          begin
            if vUpCol < FColWidths.Count - 1 then  // 右侧有，右侧变窄后不能小于最小宽度
            begin
              if FColWidths[vUpCol + 1] - vCellPt.X < MinColWidth then
                vCellPt.X := FColWidths[vUpCol + 1] - MinColWidth;

              if vCellPt.X <> 0 then
              begin
                Undo_ColResize(vUpCol, FColWidths[vUpCol], FColWidths[vUpCol] + vCellPt.X);

                FColWidths[vUpCol] := FColWidths[vUpCol] + vCellPt.X;  // 当前列变化
                // 右侧的减少，实现拖动不改变表格整体宽度
                if FResizeKeepWidth and (vUpCol < FColWidths.Count - 1) then  // 右侧的弥补变化
                  FColWidths[vUpCol + 1] := FColWidths[vUpCol + 1] - vCellPt.X;
              end;
            end
            else  // 最右侧列拖宽
            begin
              if FResizeKeepWidth and (FColWidths[vUpCol] + vCellPt.X > (OwnerData as THCRichData).Width) then
                vCellPt.X := (OwnerData as THCRichData).Width - FColWidths[vUpCol + 1];

              Undo_ColResize(vUpCol, FColWidths[vUpCol], FColWidths[vUpCol] + vCellPt.X);
              FColWidths[vUpCol] := FColWidths[vUpCol] + vCellPt.X;  // 当前列变化
            end;
          end
          else  // 拖窄了
          begin
            if FColWidths[vUpCol] + vCellPt.X < MinColWidth then  // 小于最小宽度
              vCellPt.X := MinColWidth - FColWidths[vUpCol];

            if vCellPt.X <> 0 then
            begin
              Undo_ColResize(vUpCol, FColWidths[vUpCol], FColWidths[vUpCol] + vCellPt.X);

              FColWidths[vUpCol] := FColWidths[vUpCol] + vCellPt.X;  // 当前列变化
              // 右侧的增加，实现拖动不改变表格整体宽度
              if FResizeKeepWidth and (vUpCol < FColWidths.Count - 1) then  // 右侧的弥补变化
                FColWidths[vUpCol + 1] := FColWidths[vUpCol + 1] + vCellPt.X;
            end;
          end;
        end;
      end;
    end
    else
    if FResizeInfo.TableSite = tsBorderBottom then  // 拖高/拖矮
    begin
      vCellPt.Y := Y - FMouseDownY;  // 不使用FResizeInfo.DestY(会造成按下处弹出也有偏移)
      if vCellPt.Y <> 0 then
      begin
        Undo_RowResize(FMouseDownRow, FRows[FMouseDownRow].Height, FRows[FMouseDownRow].Height + vCellPt.Y);
        FRows[FMouseDownRow].Height := FRows[FMouseDownRow].Height + vCellPt.Y;
        FRows[FMouseDownRow].AutoHeight := False;
      end;
    end;

    Self.FormatDirty;
    Resizing := False;
    GCursor := crDefault;
    OwnerData.Style.UpdateInfoRePaint;
    OwnerData.Style.UpdateInfoReCaret;

    Exit;
  end;

  if FSelecting or OwnerData.Style.UpdateInfo.Selecting then  // 划选完成
  begin
    FSelecting := False;

    // 先在按下单元格弹起，以便单元格中嵌套的表格有机会响应弹起(取消按下、划选状态，划选完成)
    if (FMouseDownRow >= 0) and (not FOutSelectInto) then  // 在表格右侧按下移动时再弹起时无有效的FMouseDownRow和FMouseDownCol
    begin
      vCellPt := GetCellPostion(FMouseDownRow, FMouseDownCol);
      FRows[FMouseDownRow][FMouseDownCol].MouseUp(Button, Shift,
        X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);
    end;

    vResizeInfo := GetCellAt(X, Y, vUpRow, vUpCol);
    if vResizeInfo.TableSite = TTableSite.tsCell then  // 没有划选到页面空白的地方
    begin
      if (vUpRow <> FMouseDownRow) or (vUpCol <> FMouseDownCol) then  // 划选完成后弹起在非按下单元格
      begin
        vCellPt := GetCellPostion(vUpRow, vUpCol);
        FRows[vUpRow][vUpCol].MouseUp(Button, Shift,
          X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);
      end;
    end;
  end
  else
  if FDraging or OwnerData.Style.UpdateInfo.Draging then  // 拖拽弹起
  begin
    FDraging := False;

    vResizeInfo := GetCellAt(X, Y, vUpRow, vUpCol);

    if vResizeInfo.TableSite = TTableSite.tsCell then  // 拖到了某单元格中
    begin
      DisSelect;
      FMouseMoveRow := vUpRow;  // 拖拽时的单元格定位使用的是MouseMove相关数据
      FMouseMoveCol := vUpCol;
      // 原始由下面三行实现
      //DisSelectSelectedCell(vUpRow, vUpCol);  // 取消除弹起处之外的所有拖拽选中单元格的状态
      //FRows[vUpRow][vUpCol].CellData.CellSelectedAll := False;
      //FSelectCellRang.Initialize;  // 准备重新赋值
      // 不管是否在在选中单元格中弹起，拖拽弹起都需要编辑到选中单元格，
      FSelectCellRang.StartRow := vUpRow;
      FSelectCellRang.StartCol := vUpCol;
      vCellPt := GetCellPostion(vUpRow, vUpCol);
      FRows[vUpRow][vUpCol].MouseUp(Button, Shift,
        X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);

      {if FMouseDownRow >= 0 then  // 有点击时的单元格(表格是划选范围内其中一个，在其他上拖拽到表格上时没有按下FMouseDownRow)
        Cells[FMouseDownRow, FMouseDownCol].CellData.InitializeField;}  // 拖拽起始单元格标明拖拽完成了
    end;
  end
  else  // 非划选，非拖拽
  if FMouseDownRow >= 0 then  // 有点击时的单元格
  begin
    vCellPt := GetCellPostion(FMouseDownRow, FMouseDownCol);
    FRows[FMouseDownRow][FMouseDownCol].MouseUp(Button, Shift,
      X - vCellPt.X, Y - vCellPt.Y, FCellHPaddingPix, FCellVPaddingPix);
  end;
end;

procedure THCTableItem.PaintFixCols(const ATableDrawTop, ALeft, ATop, ABottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vR, vC, vCellLeft, vCellTop, vCellBottom, vBorderOffs,
  vBorderLeft, vBorderTop, vBorderRight, vBorderBottom: Integer;
  vRect: TRect;

  vExtPen: HPEN;
  vOldPen: HGDIOBJ;
begin
  vCellTop := Max(ATop, ATableDrawTop) + FBorderWidthPix;
  for vR := 0 to FFixRow + FFixRowCount - 1 do
    vCellTop := vCellTop + FRows[vR].FmtOffset + FRows[vR].Height + FBorderWidthPix;

  vRect := Bounds(ALeft, vCellTop, GetFixColWidth, ABottom - vCellTop);
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(vRect);

  vBorderOffs := FBorderWidthPix div 2;
  vCellTop := ATableDrawTop + FBorderWidthPix;

  for vR := 0 to FRows.Count - 1 do
  begin
    vCellTop := vCellTop + FRows[vR].FmtOffset;
    vCellBottom := vCellTop + FRows[vR].Height;
    if (vCellBottom < ATop) or (vR < FFixRow + FFixRowCount) then
    begin
      vCellTop := vCellBottom + FBorderWidthPix;
      Continue;
    end;

    vCellLeft := ALeft + FBorderWidthPix;
    for vC := FFixCol to FFixCol + FFixColCount - 1 do
    begin
      vRect := Rect(vCellLeft, vCellTop, vCellLeft + FColWidths[vC], vCellBottom);
      if vRect.Top > ABottom then
        Break;

      if vRect.Bottom > ABottom then
        vRect.Bottom := ABottom;

      FRows[vR][vC].PaintTo(vCellLeft, vCellTop, vRect.Right, vCellBottom, ATop, ABottom, 0,
        FCellHPaddingPix, FCellVPaddingPix, ACanvas, APaintInfo);

      {$REGION ' 绘制边框线 '}
      if FBorderVisible or (not APaintInfo.Print) then
      begin
        if APaintInfo.Print then
          ACanvas.Pen.Width := Max(1, HCUnitConversion.PtToPixel(FBorderWidthPt, APaintInfo.DPI))
        else
          ACanvas.Pen.Width := FBorderWidthPix;

        if FBorderVisible then  // 未隐藏边框
        begin
          ACanvas.Pen.Color := clBlack;
          ACanvas.Pen.Style := psSolid;
        end
        else
        if not APaintInfo.Print then
        begin
          ACanvas.Pen.Color := clActiveBorder;
          ACanvas.Pen.Style := psDot;
        end;

        vBorderTop := vCellTop - FBorderWidthPix;
        vBorderBottom := vBorderTop + FBorderWidthPix  // 计算边框最下端
          + Max(FRows[vR].Height, FRows[vR][vC].Height);  // 由于可能是合并目标单元格，所以用单元格高和行高最高的

        vBorderLeft := vCellLeft - FBorderWidthPix;
        vBorderRight := vCellLeft + FColWidths[vC] + GetColSpanWidth(vR, vC);

        vExtPen := CreateExtPen(ACanvas.Pen);  // 因为默认的画笔没有线帽的控制，新增支持线帽的画笔
        vOldPen := SelectObject(ACanvas.Handle, vExtPen);
        try
          if (vBorderTop >= 0) and (cbsTop in FRows[vR][vC].BorderSides) then  // 上边框可显示
          begin
            if APaintInfo.Print then
            begin
              APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
                vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);
            end
            else
            begin
              ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);   // 左上
              ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);  // 右上
            end;
          end;

          if cbsRight in FRows[vR][vC].BorderSides then  // 右边框
          begin
            if APaintInfo.Print then
            begin
              APaintInfo.DrawNoScaleLine(ACanvas, vBorderRight + vBorderOffs, vBorderTop + vBorderOffs,
                vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
            end
            else
            begin
              ACanvas.MoveTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);  // 右上
              ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);  // 右下
              //ACanvas.MoveTo(vBorderRight + vBorderOffs + 1, vBorderTop + vBorderOffs);  // 右上
              //ACanvas.LineTo(vBorderRight + vBorderOffs + 1, vBorderBottom + vBorderOffs);  // 右下
            end;
          end;

          if (vBorderBottom <= ABottom) and (cbsBottom in FRows[vR][vC].BorderSides) then  // 下边框
          begin
            if APaintInfo.Print then
            begin
              APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs,
                vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
            end
            else
            begin
              ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);  // 左下
              ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);  // 右下
            end;
          end;

          if cbsLeft in FRows[vR][vC].BorderSides then  // 左边框
          begin
            if APaintInfo.Print then
            begin
              APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
                vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
            end
            else
            begin
              ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);
              ACanvas.LineTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
            end;
          end;

          if cbsLTRB in FRows[vR][vC].BorderSides then  // 左上右下对角线
          begin
            if APaintInfo.Print then
            begin
              APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
                vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
            end
            else
            begin
              ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);
              ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
            end;
          end;

          if cbsRTLB in FRows[vR][vC].BorderSides then  // 右上左下对角线
          begin
            if APaintInfo.Print then
            begin
              APaintInfo.DrawNoScaleLine(ACanvas, vBorderRight + vBorderOffs, vBorderTop + vBorderOffs,
                vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
            end
            else
            begin
              ACanvas.MoveTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);
              ACanvas.LineTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
            end;
          end;
        finally
          SelectObject(ACanvas.Handle, vOldPen);
          DeleteObject(vExtPen);
        end;
      end;
      {$ENDREGION}

      vCellLeft := vCellLeft + FColWidths[vC] + FBorderWidthPix;
    end;

    vCellTop := vCellBottom + FBorderWidthPix;
  end;
end;

procedure THCTableItem.PaintFixRows(const ALeft, ATop, ABottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vR, vC, vTop, vH: Integer;
  vRect: TRect;
begin
  vRect := Bounds(ALeft, ATop, Width, GetFixRowHeight);
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(vRect);

  vTop := ATop;
  for vR := FFixRow to FFixRow + FFixRowCount - 1 do
  begin
    vH := 0;  // 行中未发生合并的最高单元格
    for vC := 0 to FRows[vR].ColCount - 1 do  // 得到行中未发生合并Data内容最高的单元格高度
    begin
      if (FRows[vR][vC].CellData <> nil)  // 不是被合并的单元格
        and (FRows[vR][vC].RowSpan >= 0)  // 不是行合并的行单元格
      then
        vH := Max(vH, FRows[vR][vC].CellData.Height);
    end;

    vH := FCellVPaddingPix + vH + FCellVPaddingPix;  // 增加上下边距
    vH := Max(vH, FRows[vR].Height) + FBorderWidthPix + FBorderWidthPix;

    vRect := Bounds(ALeft, vTop, Width, vH);
    if vRect.Top >= ABottom then
      Break;

    PaintRow(vR, vRect.Left, vRect.Top, vRect.Bottom, ACanvas, APaintInfo);

    vTop := vTop + FBorderWidthPix + FRows[vR].Height;
  end;
end;

procedure THCTableItem.PaintRow(const ARow, ALeft, ATop, ABottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vC, vCellDrawLeft, vCellDataDrawTop, vCellDrawTop, vCellDrawBottom,
  vBorderLeft, vBorderTop, vBorderRight, vBorderBottom, vBorderOffs: Integer;

  vDrawDefault: Boolean;
  vCellData: THCTableCellData;
  vCellRect: TRect;

  vExtPen: HPEN;
  vOldPen: HGDIOBJ;
begin
  vBorderOffs := FBorderWidthPix div 2;
  vCellDrawLeft := ALeft + FBorderWidthPix;
  vCellDataDrawTop := ATop + FBorderWidthPix + FCellVPaddingPix;

  for vC := 0 to FRows[ARow].ColCount - 1 do
  begin
    if (FRows[ARow][vC].ColSpan < 0) or (FRows[ARow][vC].RowSpan < 0) then  // 合并列源
    begin
      vCellDrawLeft := vCellDrawLeft + FColWidths[vC] + FBorderWidthPix;
      Continue;  // 普通单元格或合并目标单元格才有数据，否则由目标单元格处理
    end;

    {$REGION ' 绘制单元格数据 '}
    vCellDrawBottom := Math.Min(ABottom,  // 数据内容屏显最下端
      vCellDataDrawTop
      + Max(FRows[ARow].Height, FRows[ARow][vC].Height) - FCellVPaddingPix  // 行高和有合并的单元格高中最大的
    );

    vCellRect := Rect(vCellDrawLeft, ATop + FBorderWidthPix, vCellDrawLeft + FRows[ARow][vC].Width, vCellDrawBottom);
    vCellData := FRows[ARow][vC].CellData;

    if (Self.IsSelectComplate or vCellData.CellSelectedAll) and (not APaintInfo.Print) then  // 表格全选中或单元格全选中
    begin
      ACanvas.Brush.Color := OwnerData.Style.SelColor;
      ACanvas.FillRect(vCellRect);
    end
    else  // 默认的绘制
    begin
      vDrawDefault := True;
      if Assigned(FOnCellPaintBK) then  // 有外部自定义绘制
        FOnCellPaintBK(Self, FRows[ARow][vC], vCellRect, ACanvas, APaintInfo, vDrawDefault);

      if vDrawDefault then  // 允许默认绘制
      begin
        if FRows[ARow][vC].BackgroundColor <> HCTransparentColor then  // 背景色
          ACanvas.Brush.Color := FRows[ARow][vC].BackgroundColor
        else
          ACanvas.Brush.Style := bsClear;

        ACanvas.FillRect(vCellRect);
      end;
    end;

    if vCellDrawBottom - vCellDataDrawTop > FCellVPaddingPix then  // 有可显示的DrawItem
    begin
      FRows[ARow][vC].PaintTo(vCellDrawLeft, vCellDataDrawTop - FCellVPaddingPix, vCellRect.Right,
        vCellDrawBottom, ATop, ABottom, 0, FCellHPaddingPix, FCellVPaddingPix, ACanvas, APaintInfo);
    end;
    {$ENDREGION}
    {$REGION ' 绘制各单元格边框线 '}
    if FBorderVisible or (not APaintInfo.Print) then
    begin
      if APaintInfo.Print then
        ACanvas.Pen.Width := Max(1, HCUnitConversion.PtToPixel(FBorderWidthPt, APaintInfo.DPI))
      else
        ACanvas.Pen.Width := FBorderWidthPix;

      if FBorderVisible then  // 未隐藏边框
      begin
        ACanvas.Pen.Color := clBlack;
        ACanvas.Pen.Style := psSolid;
      end
      else
      if not APaintInfo.Print then
      begin
        ACanvas.Pen.Color := clActiveBorder;
        ACanvas.Pen.Style := psDot;
      end;

      vBorderTop := vCellDataDrawTop - FCellVPaddingPix - FBorderWidthPix;
      vBorderBottom := vBorderTop + FBorderWidthPix  // 计算边框最下端
        + Max(FRows[ARow].Height, FRows[ARow][vC].Height);  // 由于可能是合并目标单元格，所以用单元格高和行高最高的

      vBorderLeft := vCellDrawLeft - FBorderWidthPix;
      vBorderRight := vCellDrawLeft + FColWidths[vC] + GetColSpanWidth(ARow, vC);

      vExtPen := CreateExtPen(ACanvas.Pen);  // 因为默认的画笔没有线帽的控制，新增支持线帽的画笔
      vOldPen := SelectObject(ACanvas.Handle, vExtPen);
      try
        if (vBorderTop >= 0) and (cbsTop in FRows[ARow][vC].BorderSides) then  // 上边框可显示
        begin
          if APaintInfo.Print then
          begin
            APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
              vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);
          end
          else
          begin
            ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);   // 左上
            ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);  // 右上
          end;
        end;

        if cbsRight in FRows[ARow][vC].BorderSides then  // 右边框
        begin
          if APaintInfo.Print then
          begin
            APaintInfo.DrawNoScaleLine(ACanvas, vBorderRight + vBorderOffs, vBorderTop + vBorderOffs,
              vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
          end
          else
          begin
            ACanvas.MoveTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);  // 右上
            ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);  // 右下
          end;
        end;

        if (vBorderBottom <= ABottom) and (cbsBottom in FRows[ARow][vC].BorderSides) then  // 下边框
        begin
          if APaintInfo.Print then
          begin
            APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs,
              vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
          end
          else
          begin
            ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);  // 左下
            ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);  // 右下
          end;
        end;

        if cbsLeft in FRows[ARow][vC].BorderSides then  // 左边框
        begin
          if APaintInfo.Print then
          begin
            APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
              vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
          end
          else
          begin
            ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);
            ACanvas.LineTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
          end;
        end;

        if cbsLTRB in FRows[ARow][vC].BorderSides then  // 左上右下对角线
        begin
          if APaintInfo.Print then
          begin
            APaintInfo.DrawNoScaleLine(ACanvas, vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs,
              vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
          end
          else
          begin
            ACanvas.MoveTo(vBorderLeft + vBorderOffs, vBorderTop + vBorderOffs);
            ACanvas.LineTo(vBorderRight + vBorderOffs, vBorderBottom + vBorderOffs);
          end;
        end;

        if cbsRTLB in FRows[ARow][vC].BorderSides then  // 右上左下对角线
        begin
          if APaintInfo.Print then
          begin
            APaintInfo.DrawNoScaleLine(ACanvas, vBorderRight + vBorderOffs, vBorderTop + vBorderOffs,
              vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
          end
          else
          begin
            ACanvas.MoveTo(vBorderRight + vBorderOffs, vBorderTop + vBorderOffs);
            ACanvas.LineTo(vBorderLeft + vBorderOffs, vBorderBottom + vBorderOffs);
          end;
        end;
      finally
        SelectObject(ACanvas.Handle, vOldPen);
        DeleteObject(vExtPen);
      end;
    end;
    {$ENDREGION}

    vCellDrawLeft := vCellDrawLeft + FColWidths[vC] + FBorderWidthPix;  // 同行下一列的起始Left位置
  end;
end;

procedure THCTableItem.ParseXml(const ANode: IHCXMLNode);
var
  i, vR, vC: Integer;
  vRow: THCTableRow;
  vSplit: TStringList;
begin
  FRows.Clear;

  inherited ParseXml(ANode);
  FBorderVisible := ANode.Attributes['bordervisible'];

  if ANode.HasAttribute('borderwidthpt') then
    BorderWidthPt := ANode.Attributes['borderwidthpt']
  else
  if ANode.HasAttribute('borderwidth') then
  begin
    FBorderWidthPix := ANode.Attributes['borderwidth'];
    FBorderWidthPt := Min(0.5, HCUnitConversion.PixelToPt(FBorderWidthPix, HCUnitConversion.PixelsPerInchX));
  end;

  if ANode.HasAttribute('cellvpadding') then
    CellVPaddingMM := ANode.Attributes['cellvpadding'];

  if ANode.HasAttribute('cellhpadding') then
    CellHPaddingMM := ANode.Attributes['cellhpadding'];

  vR := ANode.Attributes['row'];
  vC := ANode.Attributes['col'];
  { 创建行、列 }
  for i := 0 to vR - 1 do
  begin
    vRow := THCTableRow.Create(OwnerData.Style, vC);  // 注意行创建时是table拥有者的Style，加载时是传入的AStyle
    FRows.Add(vRow);
  end;

  { 加载各列标准宽度 }
  FColWidths.Clear;
  vSplit := TStringList.Create;
  try
    vSplit.Delimiter := ',';
    vSplit.DelimitedText := ANode.Attributes['colwidth'];
    for i := 0 to vC - 1 do
      FColWidths.Add(StrToInt(vSplit[i]));
  finally
    FreeAndNil(vSplit);
  end;

  { 加载各列数据 }
  for i := 0 to ANode.ChildNodes.Count - 1 do
    FRows[i].ParseXml(ANode.ChildNodes[i]);
end;

procedure THCTableItem.ActiveItemReAdaptEnvironment;
begin
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      var
        vEditCell: THCTableCell;
      begin
        vEditCell := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol];
        vEditCell.CellData.ActiveItemReAdaptEnvironment;
      end);
  end;
end;

procedure THCTableItem.ReFormatActiveItem;
begin
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      var
        vEditCell: THCTableCell;
      begin
        vEditCell := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol];
        vEditCell.CellData.ReFormatActiveItem;
      end);
  end;

  Self.FormatDirty;  // 对于修改表格整体属性，如边框宽度等并不仅仅是某个单元格的变化，需要强制重新格式化
end;

function THCTableItem.ResetRowCol(const AWidth, ARowCount, AColCount: Integer): Boolean;
var
  i, vDataWidth: Integer;
  {$IFDEF RESETTABLEUSEFIRSTROWHEIGHT}
  vRowHeight: Integer;
  {$ENDIF}
  vRow: THCTableRow;
begin
  Result := False;
  FFixRow := -1;
  FFixRowCount := 0;
  FFixCol := -1;
  FFixColCount := 0;

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;

  Self.Width := AWidth;
  {$IFDEF RESETTABLEUSEFIRSTROWHEIGHT}
  if FRows.Count > 0 then
    vRowHeight := FRows[0].Height
  else
    vRowHeight := MinRowHeight;

  Height := ARowCount * (vRowHeight + FBorderWidthPix) + FBorderWidthPix;
  {$ELSE}
  Height := ARowCount * (MinRowHeight + FBorderWidthPix) + FBorderWidthPix;
  {$ENDIF}

  vDataWidth := AWidth - (AColCount + 1) * FBorderWidthPix;

  FRows.Clear;
  for i := 0 to ARowCount - 1 do
  begin
    vRow := THCTableRow.Create(OwnerData.Style, AColCount);
    vRow.SetRowWidth(vDataWidth);
    {$IFDEF RESETTABLEUSEFIRSTROWHEIGHT}
    vRow.Height := vRowHeight;
    if vRowHeight <> MinRowHeight then
      vRow.AutoHeight := False;
    {$ENDIF}

    FRows.Add(vRow);
  end;

  FColWidths.Clear;
  for i := 0 to AColCount - 1 do
    FColWidths.Add(FRows[0][i].Width);

  Self.FormatDirty;
  Result := True;
end;

function THCTableItem.RowCanDelete(const ARow: Integer): Boolean;
var
  vCol: Integer;
begin
  Result := False;
  for vCol := 0 to FColWidths.Count - 1 do
  begin
    if FRows[ARow][vCol].RowSpan > 0 then  // 行中有行合并目标的列暂时不支持
      Exit;
  end;
  Result := True;
end;

function THCTableItem.ClearFormatExtraHeight: Integer;
var
  vR, vC, vRowFrom, vOldHeight: Integer;
  vCell: THCTableCell;
begin
  Result := 0;

  if Self.Height = FFormatHeight then Exit;

  vOldHeight := Height;
  vRowFrom := -1;
  for vR := FRows.Count - 1 downto 0 do
  begin
    if FRows[vR].FmtOffset <> 0 then  // 不需要重新计算高度，但需要重新布局
    begin
      vRowFrom := vR;
      FRows[vR].FmtOffset := 0;
    end;

    for vC := 0 to ColCount - 1 do
    begin
      vCell := FRows[vR][vC];
      if (vCell.ClearFormatExtraHeight <> 0)  // 有多余的高度
        or (  // 跨页后表格最后一行非合并单元格可能受合并单元格影响被撑高，
              // ClearFormatExtraHeight时返回0但并不代表不需要重新计算高度
              Assigned(vCell.CellData)
              and (vCell.Height <> FCellHPaddingPix + vCell.CellData.Height + FCellHPaddingPix)
           )
      then  // 需要重新计算高度
      begin
        vRowFrom := vR;
        CalcRowCellHeight(vR);
      end;
    end;
  end;

  if vRowFrom >= 0 then  // 需要重新布局的第1行
  begin
    CalcMergeRowHeightFrom(vRowFrom);
    Self.Height := GetFormatHeight;
    Result := vOldHeight - Self.Height;
  end;
end;

function THCTableItem.GetFixColLeft: Integer;
var
  vC: Integer;
begin
  Result := 0;
  if FFixCol > 0 then
  begin
    Result := FBorderWidthPix;
    for vC := 0 to FFixCol - 1 do
      Result := Result + FColWidths[vC] + FBorderWidthPix;
  end;
end;

function THCTableItem.GetFixColWidth: Integer;
var
  vC: Integer;
begin
  if FFixCol < 0 then
    Result := 0
  else
  begin
    Result := FBorderWidthPix;
    for vC := FFixCol to FFixCol + FFixColCount - 1 do
      Result := Result + FColWidths[vC] + FBorderWidthPix;
  end;
end;

function THCTableItem.GetFixRowHeight: Integer;
var
  vR: Integer;
begin
  if FFixRow < 0 then
    Result := 0
  else
  begin
    Result := FBorderWidthPix;
    for vR := FFixRow to FFixRow + FFixRowCount - 1 do
      Result := Result + FRows[vR].Height + FBorderWidthPix;
  end;
end;

function THCTableItem.GetFormatHeight: Integer;
var
  i: Integer;
begin
  Result := FBorderWidthPix;
  for i := 0 to RowCount - 1 do
    Result := Result + FRows[i].Height + FBorderWidthPix;

  FFormatHeight := Result;
end;

function THCTableItem.GetFormatWidth: Integer;
var
  i: Integer;
begin
  Result := FBorderWidthPix;
  for i := 0 to FColWidths.Count - 1 do
    Result := Result + FColWidths[i] + FBorderWidthPix;
end;

function THCTableItem.GetHint: string;
var
  vCell: THCTableCell;
begin
  Result := inherited GetHint;
  if (FMouseMoveRow < 0) or (FMouseMoveCol < 0) then Exit;
  vCell := FRows[FMouseMoveRow][FMouseMoveCol];
  if Assigned(vCell) and Assigned(vCell.CellData) then
    Result := vCell.CellData.GetHint;
end;

function THCTableItem.GetCells(const ARow, ACol: Integer): THCTableCell;
begin
  Result := FRows[ARow][ACol];
end;

function THCTableItem.GetColCount: Integer;
begin
  Result := FColWidths.Count;
end;

function THCTableItem.GetColSpanWidth(const ARow, ACol: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to FRows[ARow][ACol].ColSpan do
    Result := Result + FBorderWidthPix + FColWidths[ACol + i];
end;

function THCTableItem.GetColWidth(AIndex: Integer): Integer;
begin
  Result := FColWidths[AIndex];
end;

procedure THCTableItem.GetDestCell(const ARow, ACol: Cardinal; var ADestRow,
  ADestCol: Integer);
begin
  ADestRow := ARow;
  ADestCol := ACol;

  if FRows[ARow][ACol].RowSpan < 0 then
    ADestRow := ADestRow + FRows[ARow][ACol].RowSpan;

  if FRows[ARow][ACol].ColSpan < 0 then
    ADestCol := ADestCol + FRows[ARow][ACol].ColSpan;
end;

procedure THCTableItem.GetEditCell(var ARow, ACol: Integer);
begin
  ARow := -1;
  ACol := -1;
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    ARow := FSelectCellRang.StartRow;
    ACol := FSelectCellRang.StartCol;
  end;
end;

function THCTableItem.GetEditCell: THCTableCell;
begin
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
    Result := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol]
  else
    Result := nil;
end;

function THCTableItem.GetCellAt(const X, Y: Integer; var ARow, ACol: Integer;
  const AReDest: Boolean = True): TResizeInfo;

  {$REGION 'CheckRowBorderRang'}
  function CheckRowBorderRang(const ABottom: Integer): Boolean;
  begin
    Result := (Y >= ABottom - GripSize) and (Y <= ABottom + GripSize);  // 是否在行边框线上
  end;
  {$ENDREGION}

  {$REGION 'CheckColBorderRang'}

  function CheckColBorderRang(const ALeft: Integer): Boolean;
  begin
    Result := (X >= ALeft - GripSize) and (X <= ALeft + GripSize);  // 是否在行边框线上
  end;
  {$ENDREGION}

var
  i, vTop, vBottom, vDestRow, vDestCol: Integer;
  vLeft: Integer absolute vTop;
  vRight: Integer absolute vBottom;
begin
  Result.TableSite := tsOutside;
  Result.DestX := -1;
  Result.DestY := -1;

  ARow := -1;
  ACol := -1;

  if (Y < 0) or (Y > Height) then Exit;

  if (X < 0) or (X > Width) then  // 不在表格上时，判断对应位置的行，供光标使用
  begin
    vTop := FBorderWidthPix;
    for i := 0 to RowCount - 1 do
    begin
      vTop := vTop + FRows[i].FmtOffset;  // 以实际内容Top为顶位置，避免行有跨页时，在上一页底部点击选中的是下一页第一行
      vBottom := vTop + FRows[i].Height;

      if (vTop < Y) and (vBottom > Y) then  // 在此行中
      begin
        ARow := i;
        Break;
      end;

      vTop := vBottom + FBorderWidthPix;
    end;

    Exit;
  end;

  { 获取是否在行或列的边框上 }
  // 判断是否在最上边框
  vTop := FBorderWidthPix;
  if CheckRowBorderRang(vTop) then  // 第一行最上边框
  begin
    Result.TableSite := tsBorderTop;
    Exit;
  end;
  // 判断是否在最左边框
  if CheckColBorderRang(vTop) then
  begin
    Result.TableSite := tsBorderLeft;
    Exit;
  end;

  // 判断是在行边框上还是行中
  for i := 0 to RowCount - 1 do
  begin
    vTop := vTop + FRows[i].FmtOffset;  // 以实际内容Top为顶位置，避免行有跨页时，在上一页底部点击选中的是下一页第一行
    vBottom := vTop + FRows[i].Height + FBorderWidthPix;
    if CheckRowBorderRang(vBottom) then  // 第i行下边框
    begin
      ARow := i;
      Result.TableSite := tsBorderBottom;
      Result.DestY := vBottom;
      Break;  // 为处理跨单元格划选时，划到下边框时ACol<0造成中间选中的也被忽略掉的问题，不能像下面列找不到时Exit
    end;

    if (vTop < Y) and (vBottom > Y) then  // 在此行中
    begin
      ARow := i;
      Break;
    end;

    vTop := vBottom;
  end;

  if ARow < 0 then Exit;

  // 判断是在列边框上还是列中
  vLeft := FBorderWidthPix;
  for i := 0 to FColWidths.Count - 1 do
  begin
    vRight := vLeft + FColWidths[i] + FBorderWidthPix;
    GetDestCell(ARow, i, vDestRow, vDestCol);
    if CheckColBorderRang(vRight) then  // 第i列右边框
    begin
      ACol := i;
      if vDestCol + FRows[vDestRow][vDestCol].ColSpan <> i then  // 在列边框时，且不是合并源列最后一列，按在单元格中处理
        Result.TableSite := tsCell
      else
        Result.TableSite := tsBorderRight;

      Result.DestX := vRight;

      Break;
    end;

    if (vLeft < X) and (vRight > X) then  // 在此列中
    begin
      ACol := i;
      if (Result.TableSite = tsBorderBottom)
        and (vDestRow + FRows[vDestRow][vDestCol].RowSpan <> ARow)
      then  // 在行边框时，且不是合并源行最后一行，按在单元格中处理
        Result.TableSite := tsCell;

      Break;
    end;
    vLeft := vRight;
  end;

  if ACol >= 0 then  // 有确定的单元格
  begin
    if Result.TableSite = tsOutside then  // 修正外面为单元格内
      Result.TableSite := tsCell;

    if AReDest and (FRows[ARow][ACol].CellData = nil) then // 如果是被合并的单元格，返回合并后的单元格
      GetDestCell(ARow, ACol, ARow, ACol);
  end;
end;

function THCTableItem.GetCellPostion(const ARow, ACol: Integer): TPoint;
var
  i: Integer;
begin
  Result.X := FBorderWidthPix;
  Result.Y := FBorderWidthPix;
  for i := 0 to ARow - 1 do
    Result.Y := Result.Y + FRows[i].FmtOffset + FRows[i].Height + FBorderWidthPix;

  Result.Y := Result.Y + FRows[ARow].FmtOffset;
  for i := 0 to ACol - 1 do
    Result.X := Result.X + FColWidths[i] + FBorderWidthPix;
end;

function THCTableItem.GetResizing: Boolean;
begin
  Result := (inherited GetResizing) or ActiveDataResizing;
end;

function THCTableItem.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function THCTableItem.GetSelectComplate: Boolean;
begin
  Result := // 先判断是否全部单元格都选中了，如果否会清空各单元格的选中状态
    //(not ((RowCount = 1) and (FRows[0].ColCount = 1)))  // 不是只有一个单元格
    (FSelectCellRang.StartRow = 0)
    and (FSelectCellRang.StartCol = 0)
    and (FSelectCellRang.EndRow = FRows.Count - 1)
    and (FSelectCellRang.EndCol = FColWidths.Count - 1);
end;

procedure THCTableItem.GetSourceCell(const ARow, ACol: Cardinal; var ASrcRow,
  ASrcCol: Integer);
begin
  if FRows[ARow][ACol].CellData <> nil then
  begin
    ASrcRow := ARow + FRows[ARow][ACol].RowSpan;
    ASrcCol := ACol + FRows[ARow][ACol].ColSpan;
  end
  else  // 源单元格不能获取源单元格
    raise Exception.Create(HCS_EXCEPTION_VOIDSOURCECELL);
end;

function THCTableItem.GetTopLevelData: THCCustomData;
var
  vCell: THCTableCell;
begin
  vCell := GetEditCell;
  if Assigned(vCell) then
    Result := vCell.CellData.GetTopLevelData
  else
    Result := inherited GetTopLevelData;
end;

function THCTableItem.GetTopLevelDataAt(const X, Y: Integer): THCCustomData;
var
  vResizeInfo: TResizeInfo;
  vRow, vCol: Integer;
  vCellPt: TPoint;
begin
  Result := nil;
  vResizeInfo := GetCellAt(X, Y, vRow, vCol);
  if (vRow < 0) or (vCol < 0) then Exit;
  vCellPt := GetCellPostion(vRow, vCol);
  Result := (FRows[vRow][vCol].CellData as THCRichData).GetTopLevelDataAt(
    X - vCellPt.X - FCellHPaddingPix, Y - vCellPt.Y - FCellVPaddingPix);
end;

function THCTableItem.GetTopLevelItem: THCCustomItem;
var
  vCell: THCTableCell;
begin
  vCell := GetEditCell;
  if Assigned(vCell) then
    Result := vCell.CellData.GetTopLevelItem
  else
    Result := inherited GetTopLevelItem;
end;

function THCTableItem.GetTopLevelRectDrawItem: THCCustomDrawItem;
var
  vCellData: THCTableCellData;
begin
  vCellData := GetActiveData as THCTableCellData;
  if Assigned(vCellData) then
    Result := vCellData.GetTopLevelRectDrawItem
  else
    Result := inherited GetTopLevelRectDrawItem;
end;

function THCTableItem.GetTopLevelRectDrawItemCoord: TPoint;
var
  vCell: THCTableCell;
  vPt: TPoint;
begin
  Result := Point(-1, -1);
  vCell := GetEditCell;
  if Assigned(vCell) then
  begin
    vPt := vCell.CellData.GetTopLevelRectDrawItemCoord;
    if vPt.X >= 0 then
    begin
      Result := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
      Result.X := Result.X + vPt.X + FCellHPaddingPix;
      Result.Y := Result.Y + vPt.Y + vCell.GetCellDataTop(FCellVPaddingPix);
    end;
  end;
end;

procedure THCTableItem.InitializeCellData(const ACellData: THCTableCellData);
begin
  ACellData.ParentData := OwnerData;
  ACellData.OnInsertItem := OwnerData.OnInsertItem;
  ACellData.OnRemoveItem := OwnerData.OnRemoveItem;
  ACellData.OnSaveItem := OwnerData.OnSaveItem;
  ACellData.OnAcceptAction := (OwnerData as THCRichData).OnAcceptAction;
  ACellData.OnItemMouseDown := (OwnerData as THCViewData).OnItemMouseDown;
  ACellData.OnItemMouseUp := (OwnerData as THCViewData).OnItemMouseUp;
  ACellData.OnDrawItemMouseMove := (OwnerData as THCRichData).OnDrawItemMouseMove;
  ACellData.OnItemRequestFormat := DoCellDataItemRequestFormat;

  ACellData.OnCreateItemByStyle := (OwnerData as THCViewData).OnCreateItemByStyle;
  ACellData.OnDrawItemPaintBefor := (OwnerData as THCRichData).OnDrawItemPaintBefor;
  ACellData.OnDrawItemPaintContent := (OwnerData as THCRichData).OnDrawItemPaintContent;
  ACellData.OnDrawItemPaintAfter := (OwnerData as THCViewData).OnDrawItemPaintAfter;

  ACellData.OnInsertAnnotate := (OwnerData as THCViewData).OnInsertAnnotate;
  ACellData.OnRemoveAnnotate := (OwnerData as THCViewData).OnRemoveAnnotate;
  ACellData.OnDrawItemAnnotate := (OwnerData as THCViewData).OnDrawItemAnnotate;
  ACellData.OnCaretItemChanged := (OwnerData as THCViewData).OnCaretItemChanged;

  ACellData.OnCanEdit := (OwnerData as THCViewData).OnCanEdit;
  ACellData.OnInsertTextBefor := (OwnerData as THCViewData).OnInsertTextBefor;
  ACellData.OnPaintDomainRegion := (OwnerData as THCViewData).OnPaintDomainRegion;
  ACellData.OnItemResized := (OwnerData as THCRichData).OnItemResized;
  ACellData.OnCurParaNoChange := (OwnerData as THCRichData).OnCurParaNoChange;

  ACellData.OnCreateItem := (OwnerData as THCRichData).OnCreateItem;
  ACellData.OnGetUndoList := Self.GetSelfUndoList;
  ACellData.OnGetRootData := DoCellDataGetRootData;
end;

procedure THCTableItem.InitializeMouseInfo;
begin
  //FSelectCellRang.Initialize;  // 造成表格中调用其他工具窗体（失去焦点触发KillFocus）插入数据元时失败
  FMouseDownRow := -1;
  FMouseDownCol := -1;
  FMouseMoveRow := -1;
  FMouseMoveCol := -1;
  FMouseLBDowning := False;
end;

function THCTableItem.InsertCol(const ACol, ACount: Integer): Boolean;
var
  i, j, vRow, vWidth: Integer;
  viDestRow, viDestCol: Integer;
  vCell: THCTableCell;
begin
  Result := False;
  Undo_Mirror;
  { TODO : 根据各行当前列平均减少一定的宽度给要插入的列 }
  vWidth := MinColWidth - FBorderWidthPix;
  for i := 0 to ACount - 1 do
  begin
    for vRow := 0 to RowCount - 1 do
    begin
      vCell := THCTableCell.Create(OwnerData.Style);
      vCell.Width := vWidth;
      InitializeCellData(vCell.CellData);

      if (ACol < FColWidths.Count) and (FRows[vRow][ACol].ColSpan < 0) then  // 合并的源列
      begin
        GetDestCell(vRow, ACol, viDestRow, viDestCol);  // 目标行列
        // 新插入的列在当前列后面，也做为被合并的列
        vCell.CellData.Free;
        vCell.CellData := nil;
        vCell.RowSpan := FRows[vRow][ACol].RowSpan;
        vCell.ColSpan := FRows[vRow][ACol].ColSpan;

        for j := ACol to viDestCol + FRows[viDestRow][viDestCol].ColSpan do  // 后续列离目标远1
          FRows[vRow][j].ColSpan := FRows[vRow][j].ColSpan - 1;  // 离目标列远1

        if vRow = viDestRow + FRows[viDestRow][viDestCol].RowSpan then  // 合并范围内的行都插入完后，再将目标列范围扩大，否则提前扩大了其他行原位置远离目标时取的范围会越界
          FRows[viDestRow][viDestCol].ColSpan := FRows[viDestRow][viDestCol].ColSpan + 1;
      end;

      FRows[vRow].Insert(ACol, vCell);
    end;

    FColWidths.Insert(ACol, vWidth);  // 右侧插入列
  end;

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Self.SizeChanged := True;
  Self.FormatDirty;
  Result := True;
end;

function THCTableItem.InsertColAfter(const ACount: Integer): Boolean;
var
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;

  if vCell.ColSpan > 0 then
    Result := InsertCol(FSelectCellRang.StartCol + vCell.ColSpan + 1, ACount)
  else
    Result := InsertCol(FSelectCellRang.StartCol + 1, ACount);
end;

function THCTableItem.InsertColBefor(const ACount: Integer): Boolean;
var
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;
  vCell.CellData.InitializeField;

  Result := InsertCol(FSelectCellRang.StartCol, ACount);
end;

function THCTableItem.InsertItem(const AItem: THCCustomItem): Boolean;
var
  vCell: THCTableCell;
  vResult: Boolean;
begin
  Result := False;
  vCell := GetEditCell;
  if not Assigned(vCell) then Exit;

  CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
    procedure
    begin
      //DoGetSelfUndoList.NewUndo(0, 0);
      //Self.Undo_StartRecord;
      vResult := vCell.CellData.InsertItem(AItem);
    end);

  Result := vResult;
end;

function THCTableItem.InsertRow(const ARow, ACount: Integer): Boolean;
var
  i, j, vCol, viDestRow, viDestCol: Integer;
  vTableRow: THCTableRow;
begin
  Result := False;
  Undo_Mirror;

  for i := 0 to ACount - 1 do
  begin
    vTableRow := THCTableRow.Create(OwnerData.Style, FColWidths.Count);
    for vCol := 0 to FColWidths.Count - 1 do
    begin
      vTableRow[vCol].Width := FColWidths[vCol];

      if (ARow < FRows.Count) and (FRows[ARow][vCol].RowSpan < 0) then  // 在合并的源单元格前面插入
      begin
        GetDestCell(ARow, vCol, viDestRow, viDestCol);

        vTableRow[vCol].CellData.Free;
        vTableRow[vCol].CellData := nil;
        vTableRow[vCol].RowSpan := FRows[ARow][vCol].RowSpan;
        vTableRow[vCol].ColSpan := FRows[ARow][vCol].ColSpan;

        for j := ARow to viDestRow + FRows[viDestRow][viDestCol].RowSpan do  // 目标的行跨度 - 已经跨的
          FRows[j][vCol].RowSpan := FRows[j][vCol].RowSpan - 1;  // 离目标行远1

        if vCol = viDestCol + FRows[viDestRow][viDestCol].ColSpan then
          FRows[viDestRow][viDestCol].RowSpan := FRows[viDestRow][viDestCol].RowSpan + 1;  // 目标行包含的合并源增加1
      end;
    end;

    FRows.Insert(ARow, vTableRow);
  end;

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Self.SizeChanged := True;
  Self.FormatDirty;
  Result := True;
end;

function THCTableItem.InsertRowAfter(const ACount: Integer): Boolean;
var
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;

  if vCell.RowSpan > 0 then
    Result := InsertRow(FSelectCellRang.StartRow + vCell.RowSpan + 1, ACount)
  else
    Result := InsertRow(FSelectCellRang.StartRow + 1, ACount);
end;

function THCTableItem.InsertRowBefor(const ACount: Integer): Boolean;
var
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;

  Result := InsertRow(FSelectCellRang.StartRow, ACount);
end;

function THCTableItem.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
var
  vResult: Boolean;
begin
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      var
        vEditCell: THCTableCell;
      begin
        vEditCell := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol];
        vResult := vEditCell.CellData.InsertStream(AStream, AStyle, AFileVersion);
      end);

    Result := vResult;
  end
  else
    Result := inherited InsertStream(AStream, AStyle, AFileVersion);
end;

function THCTableItem.InsertText(const AText: string): Boolean;
var
  vResult: Boolean;
begin
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      var
        vEditCell: THCTableCell;
      begin
        vEditCell := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol];
        vResult := vEditCell.CellData.InsertText(AText);
      end);

    Result := vResult;
  end
  else
    Result := inherited InsertText(AText);
end;

function THCTableItem.IsBreakRow(const ARow: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to FPageBreaks.Count - 1 do
  begin
    if ARow = FPageBreaks[i].Row then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function THCTableItem.IsFixCol(const ACol: Integer): Boolean;
begin
  Result := False;
  if FFixCol >= 0 then
    Result := (ACol >= FFixCol) and (ACol <= FFixCol + FFixColCount - 1);
end;

function THCTableItem.IsFixRow(const ARow: Integer): Boolean;
begin
  Result := False;
  if FFixRow >= 0 then
    Result := (ARow >= FFixRow) and (ARow <= FFixRow + FFixRowCount - 1);
end;

function THCTableItem.IsSelectComplateTheory: Boolean;
begin
  Result := IsSelectComplate;
end;

procedure THCTableItem.MarkStyleUsed(const AMark: Boolean);
var
  vR, vC: Integer;
begin
  inherited;
  for vR := 0 to FRows.Count - 1 do
  begin
    for vC := 0 to FRows[vR].ColCount - 1 do
    begin
      if FRows[vR][vC].CellData <> nil then
        FRows[vR][vC].CellData.MarkStyleUsed(AMark);
    end;
  end;
end;

function THCTableItem.MergeCells(const AStartRow, AStartCol, AEndRow,
  AEndCol: Integer): Boolean;

  procedure DeleteEmptyRows(const ASRow, AERow: Cardinal);
  var
    vR, vC, i: Integer;
    vEmptyRow: Boolean;
  begin
    for vR := AERow downto ASRow do  // 遍历行
    begin
      vEmptyRow := True;
      for vC := 0 to FRows[vR].ColCount - 1 do  // 当前行各列
      begin
        if FRows[vR][vC].CellData <> nil then  // 存在没有被合并的列
        begin
          vEmptyRow := False;  // 不是空行
          Break;
        end;
      end;

      if vEmptyRow then  // 空行
      begin
        for i := 0 to vR - 1 do
        begin
          for vC := 0 to FRows[i].ColCount - 1 do
          begin
            if FRows[i][vC].RowSpan > 0 then
              FRows[i][vC].RowSpan := FRows[i][vC].RowSpan - 1;
          end;
        end;

        for i := vR + 1 to FRows.Count - 1 do
        begin
          for vC := 0 to FRows[i].ColCount - 1 do
          begin
            if FRows[i][vC].RowSpan < 0 then
              FRows[i][vC].RowSpan := FRows[i][vC].RowSpan + 1;
          end;
        end;

        FRows.Delete(vR);  // 删除当前空行
      end;
    end;
  end;

  procedure DeleteEmptyCols(const ASCol, AECol: Cardinal);
  var
    vR, vC, i: Integer;
    vEmptyCol: Boolean;
    vTableCell: THCTableCell;
  begin
    for vC := AECol downto ASCol do  // 循环各列
    begin
      vEmptyCol := True;
      for vR := 0 to RowCount - 1 do  // 循环各行
      begin
        if FRows[vR][vC].CellData <> nil then  // 某行的第vC列没有被合并
        begin
          vEmptyCol := False;
          Break;
        end;
      end;

      if vEmptyCol then  // 是空列
      begin
        for vR := RowCount - 1 downto 0 do  // 循环各行，删除对应列
        begin
          for i := 0 to vC - 1 do
          begin
            vTableCell := FRows[vR][i];
            if i + vTableCell.ColSpan >= vC then
              vTableCell.ColSpan := vTableCell.ColSpan - 1;
          end;

          for i := vC + 1 to FRows[vR].ColCount - 1 do
          begin
            vTableCell := FRows[vR][i];
            if i + vTableCell.ColSpan < vC then
              vTableCell.ColSpan := vTableCell.ColSpan + 1;
          end;

          FRows[vR].Delete(vC);  // 删除列
        end;

        FColWidths[vC - 1] := FColWidths[vC - 1] + FBorderWidthPix + FColWidths[vC];
        FColWidths.Delete(vC);
      end;
    end;
  end;

var
  vR, vC, vEndRow, vEndCol: Integer;  // 真正的结束位置
begin
  Result := False;
  vEndRow := AEndRow;
  vEndCol := AEndCol;

  AdjustCellRange(AStartRow, AStartCol, vEndRow, vEndCol);

  Result := CellsCanMerge(AStartRow, AStartCol, vEndRow, vEndCol);
  if not Result then Exit;

  // 经过上面的校验和判断后，起始行、列和结束行、列组成一个矩形区域
  if AStartRow = vEndRow then  // 同一行合并
  begin
    for vC := AStartCol + 1 to vEndCol do  // 合并列
    begin
      if FRows[AStartRow][vC].CellData <> nil then  // 防止已经合并的重复再合并
      begin
        FRows[AStartRow][AStartCol].CellData.AddData(FRows[AStartRow][vC].CellData);
        FRows[AStartRow][vC].CellData.Free;
        FRows[AStartRow][vC].CellData := nil;
        //Cells[AStartRow, vC].RowSpan := 0;
      end;

      FRows[AStartRow][vC].ColSpan := AStartCol - vC;
    end;

    FRows[AStartRow][AStartCol].ColSpan := vEndCol - AStartCol;  // 合并源增加

    DeleteEmptyCols(AStartCol + 1, vEndCol);
    Result := True;
  end
  else
  if AStartCol = vEndCol then  // 同列合并
  begin
    for vR := AStartRow + 1 to vEndRow do  // 合并各行
    begin
      if FRows[vR][AStartCol].CellData <> nil then  // 防止已经合并的重复再合并
      begin
        FRows[AStartRow][AStartCol].CellData.AddData(FRows[vR][AStartCol].CellData);
        FRows[vR][AStartCol].CellData.Free;
        FRows[vR][AStartCol].CellData := nil;
        //Cells[vR, AStartCol].ColSpan := 0;
      end;

      FRows[vR][AStartCol].RowSpan := AStartRow - vR;
    end;

    FRows[AStartRow][AStartCol].RowSpan := vEndRow - AStartRow;

    DeleteEmptyRows(AStartRow + 1, vEndRow);
    Result := True;
  end
  else  // 不同行，不同列
  begin
    for vC := AStartCol + 1 to vEndCol do  // 起始行各列合并
    begin
      if FRows[AStartRow][vC].CellData <> nil then  // 防止已经合并的重复再合并
      begin
        FRows[AStartRow][AStartCol].CellData.AddData(FRows[AStartRow][vC].CellData);
        FRows[AStartRow][vC].CellData.Free;
        FRows[AStartRow][vC].CellData := nil;
      end;

      FRows[AStartRow][vC].RowSpan := 0;
      FRows[AStartRow][vC].ColSpan := AStartCol - vC;
    end;

    for vR := AStartRow + 1 to vEndRow do  // 剩余行各列合并
    begin
      for vC := AStartCol to vEndCol do
      begin
        if FRows[vR][vC].CellData <> nil then
        begin
          FRows[AStartRow][AStartCol].CellData.AddData(FRows[vR][vC].CellData);
          FRows[vR][vC].CellData.Free;
          FRows[vR][vC].CellData := nil;
        end;

        FRows[vR][vC].ColSpan := AStartCol - vC;
        FRows[vR][vC].RowSpan := AStartRow - vR;
      end;
    end;

    FRows[AStartRow][AStartCol].RowSpan := vEndRow - AStartRow;
    FRows[AStartRow][AStartCol].ColSpan := vEndCol - AStartCol;

    DeleteEmptyRows(AStartRow + 1, vEndRow);
    // 删除空列
    DeleteEmptyCols(AStartCol + 1, vEndCol);

    Result := True;
  end;
end;

function THCTableItem.MergeSelectCells: Boolean;
var
  vSelRow, vSelCol: Integer;
begin
  if (FSelectCellRang.StartRow >= 0) and (FSelectCellRang.EndRow >= 0) then
  begin
    Undo_Mirror;

    Result := MergeCells(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      FSelectCellRang.EndRow, FSelectCellRang.EndCol);

    if Result then
    begin
      Self.FormatDirty;
      { 防止合并后有空行或空列被删除后，DisSelect访问越界，所以合并后直接赋值结束信息 }
      vSelRow := FSelectCellRang.StartRow;
      vSelCol := FSelectCellRang.StartCol;
      FSelectCellRang.InitializeEnd;
      DisSelect;
      FSelectCellRang.SetStart(vSelRow, vSelCol);
      FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.InitializeField;
    end;
  end
  else
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
    Result := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.MergeTableSelectCells
  else
    Result := False;
end;

procedure THCTableItem.CalcRowCellHeight(const ARow: Integer);
var
  vC, vNorHeightMax: Integer;
begin
  vNorHeightMax := 0;  // 行中未发生合并的最高单元格
  for vC := 0 to FRows[ARow].ColCount - 1 do  // 得到行中未发生合并Data内容最高的单元格高度
  begin
    if (FRows[ARow][vC].CellData <> nil)  // 不是被合并的单元格
      and (FRows[ARow][vC].RowSpan = 0)  // 不是行合并的行单元格
    then
      vNorHeightMax := Max(vNorHeightMax, FRows[ARow][vC].CellData.Height);
  end;

  vNorHeightMax := FCellVPaddingPix + vNorHeightMax + FCellVPaddingPix;  // 增加上下边距
  {for vC := 0 to FRows[ARow].ColCount - 1 do  // 对齐 FRows[ARow].Height中处理了
    FRows[ARow][vC].Height := vNorHeightMax;}

  if FRows[ARow].AutoHeight then  // 以行中各未发生行合并的列中最高的为行高
    FRows[ARow].Height := vNorHeightMax
  else  // 拖动改变了高度
  begin
    if vNorHeightMax > FRows[ARow].Height then  // 拖动高度失效
    begin
      {$IFDEF TABLEROWRESTOREAUTOHEIGHT}
      FRows[ARow].AutoHeight := True;
      {$ENDIF}
      FRows[ARow].Height := vNorHeightMax;
    end;
  end;
end;

function THCTableItem.CanDrag: Boolean;
begin
  Result := inherited CanDrag;
  if Result then
  begin
    if FSelectCellRang.EditCell then  // 在同一单元格中编辑
      Result := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.SelectedCanDrag
    else
      Result := Self.IsSelectComplate or Self.IsSelectPart;
  end;
end;

procedure THCTableItem.CellChangeByAction(const ARow, ACol: Integer; const AProcedure: THCProcedure);
begin
  Self.SizeChanged := False;
  AProcedure();
  if not Self.SizeChanged then
    Self.SizeChanged := FRows[ARow][ACol].CellData.FormatHeightChange;

  //FLastChangeFormated := not Self.SizeChanged;
end;

function THCTableItem.CellsCanMerge(const AStartRow, AStartCol, AEndRow, AEndCol: Integer): Boolean;
var
  vR, vC: Integer;
begin
  Result := False;

  for vR := AStartRow to AEndRow do
  begin
    for vC := AStartCol to AEndCol do
    begin
      if FRows[vR][vC].CellData <> nil then
      begin
        if not FRows[vR][vC].CellData.CellSelectedAll then
          Exit;
      end;
    end;
  end;

  Result := True;

  {GetDestCell(AStartRow, AStartCol, vStartDestRow, vStartDestCol);
  vCell := FRows[vStartDestRow][vStartDestCol];
  vStartDestRow := vStartDestRow + vCell.RowSpan;
  vStartDestCol := vStartDestCol + vCell.ColSpan;

  // 结束单元格的有效范围
  GetDestCell(AEndRow, AEndCol, vEndDestRow, vEndDestCol);
  vCell := FRows[vEndDestRow][vEndDestCol];
  vEndDestRow := vEndDestRow + vCell.RowSpan;
  vEndDestCol := vEndDestCol + vCell.ColSpan;

  if vStartDestRow = vEndDestRow then
    Result := vStartDestCol < vEndDestCol
  else
  if vStartDestRow < vEndDestRow then
    Result := vStartDestCol <= vEndDestCol;}
end;

procedure THCTableItem.CheckFixColSafe(const ACol: Integer);
begin
  if FFixCol + FFixColCount - 1 >= ACol then
  begin
    FFixCol := -1;
    FFixColCount := 0;
  end;
end;

procedure THCTableItem.CheckFixRowSafe(const ARow: Integer);
begin
  if FFixRow + FFixRowCount - 1 >= ARow then
  begin
    FFixRow := -1;
    FFixRowCount := 0;
  end;
end;

procedure THCTableItem.CheckFormatPageBreak(const APageIndex, ADrawItemRectTop,
  ADrawItemRectBottom, APageDataFmtTop, APageDataFmtBottom, AStartRow: Integer;
  var ABreakRow, AFmtOffset, ACellMaxInc: Integer);

  procedure AddPageBreak(const ARow, ABreakSeat: Integer);
  var
    vPageBreak: TPageBreak;
  begin
    vPageBreak := TPageBreak.Create;
    vPageBreak.PageIndex := APageIndex;  // 分页时当前页序号
    vPageBreak.Row := ARow;  // 分页行
    vPageBreak.BreakSeat := ABreakSeat;  // 分页时，此行各列分页位置最大的
    vPageBreak.BreakBottom := APageDataFmtBottom - ADrawItemRectTop;  // 分页时，页底部位置距此页表格最顶部的距离(此页有多少空间用来放表格)

    FPageBreaks.Add(vPageBreak);
  end;

var
  /// <summary> 分页行未分页时格式化起始位置，表格线下 </summary>
  vBreakRowFmtTop,
  /// <summary> 分页行结束位置，含底部横线 </summary>
  vBreakRowBottom,
  /// <summary> 最后一个DrawItem底部距离行底部的距离 </summary>
  vLastDFromRowBottom,
  /// <summary> 单元格数据顶部(被合并单元格分页时以目标单元格为准) </summary>
  vDestCellDataFmtTop,
  /// <summary> 当前DItem向下偏移多少可以显示在下一页顶部 </summary>
  vH,
  /// <summary> 当前行分页后，增加的高度 </summary>
  vCellInc,
  vDestRow, vDestCol,  // 合并的目标单元格
  /// <summary> 分页时，此行各列分页位置最大的(相对于表格顶部的高度) </summary>
  vRowBreakSeat,
  vPageBreakBottom
    : Integer;
  i, vR, vC, vFixHeight: Integer;
  vCellData: THCTableCellData;
  vDrawItem: THCCustomDrawItem;
  vFirstLinePlace  // 各单元格都至少有一行内容可在分页位置上面完整显示
    : Boolean;
  vColCross: TColCross;
  vColCrosses: TObjectList<TColCross>;  // 记录分页行各列分页起始DrawItem和分页偏移
begin
  ABreakRow := -1;
  AFmtOffset := 0;
  ACellMaxInc := 0;  // vCellInc的最大值，表示当前行各列为避开分页额外增加的格式化高度中最高的

  { 得到起始行的Fmt起始位置 }                         {20190613 一半?}
  vBreakRowFmtTop := ADrawItemRectTop + FBorderWidthPix - 1;  // 第1行排版位置(上边框线结束位置)，因为边框在ADrawItemRectTop也占1像素，所以要减掉
  for vR := 0 to AStartRow - 1 do
    vBreakRowFmtTop := vBreakRowFmtTop + FRows[vR].FmtOffset + FRows[vR].Height + FBorderWidthPix;  // 第i行结束位置(含下边框结束位置)

  //vBreakRowFmtTop := vBreakRowFmtTop + FRows[AStartRow].FmtOffset;  // 再加上起始行是整体向下偏移的情况

  { 从起始行开始检测当前页是否能放完表格 }
  vR := AStartRow;
  while vR < Self.RowCount do  // 遍历每一行
  begin
    vBreakRowBottom := vBreakRowFmtTop + FRows[vR].FmtOffset + FRows[vR].Height + FBorderWidthPix;  // 第i行结束位置(含下边框结束位置)
    if vBreakRowBottom > APageDataFmtBottom then  // 第i行结束位置超出页数据结束位置，放不下
    begin
      ABreakRow := vR;  // 第i行需要处理分页
      Break;
    end;
    vBreakRowFmtTop := vBreakRowBottom;  // 第i+1行起始位置(上边框结束位置)

    Inc(vR);
  end;

  if ABreakRow < 0 then Exit;  // 表格都能在当前页放下

  if (not Self.CanPageBreak) and (ABreakRow = 0) then  // 表格不支持分页，且在当前页一行也放不下，整体下移到下一页
  begin
    //if vRowDataFmtTop < APageDataFmtTop then  // 这样判断，当表格在第2页第1个时不准确 当前页开始Item不是当前表格（表格是当前页第一个Item，和分页不同，分页虽然也是第一个，但表格的起始位置并不在分页后的页）
    //begin
    AFmtOffset := APageDataFmtBottom - ADrawItemRectTop;
    Exit;
    //end;
  end;

  // 表格放不下，需要截断，处理截断分页的过程
  // 1.先计算出分页截断位置
  // 2.根据位置，判断各列内容移到下一页增加的偏移量
  // 3.整行偏移，如果偏移量为0要将其他单元格偏移增加的量附加到行Height，否则附加到FmtOffset
  // 4.非整行偏移的，处理Data里各DrawItem的偏移

  {$REGION ' 1.放不下，则判断分页位置，不一定是APageDataFmtBottom，可能是分页向下偏移行顶部' }
  vFirstLinePlace := True;
  vPageBreakBottom := APageDataFmtBottom;

  // 先判断是不是有单元格里第一行内容就放不下，需要整体下移，这样的好处是，如果
  // 有行需要整体下移时，此行中有单元格是行合并源且它的目标底部并不需要跨页，但
  // 分页行整体下移后，此页以分页行上一行底部为收敛，目标内容如果有超过收敛位置
  // 的，要以收敛位置做为分页判断位置而不是页底部位置了。
  // 提前得到有源行需要整体下移时，可以用下移行上一行底部做为目标单元格的截断位置。
  for vC := 0 to FRows[ABreakRow].ColCount - 1 do  // 遍历所有单元格中DrawItem，找从哪个开始向下偏移及偏移量
  begin
    if FRows[ABreakRow][vC].ColSpan < 0 then  // 合并目标只需由目标单元格处理
      Continue;

    GetDestCell(ABreakRow, vC, vDestRow, vDestCol);
    vCellData := FRows[vDestRow][vDestCol].CellData;

    // 计算目标单元格数据起始位置
    vDestCellDataFmtTop := vBreakRowFmtTop + FCellVPaddingPix;  // 分页行数据绘制起始位置

    if ABreakRow <> vDestRow then
      vDestCellDataFmtTop := vDestCellDataFmtTop - SrcCellDataTopDistanceToDest(ABreakRow, vDestRow);

    // 判断合并目标内容在当前分页行的分页位置
    for i := 0 to vCellData.DrawItems.Count - 1 do
    begin
      vDrawItem := vCellData.DrawItems[i];
      if not vDrawItem.LineFirst then  // 只需要判断列中每行第一个
        Continue;

      if vDestCellDataFmtTop + vDrawItem.Rect.Bottom + FCellVPaddingPix + FBorderWidthPix > APageDataFmtBottom then  // 当前DrawItem底部超过页底部了 20160323002 // 行底部的边框线显示不下时也向下偏移
      begin                                    // |如果FBorderWidth比行高大就不合适
        if i = 0 then  // 第一个DrawItem就放不下，需要整体下移(下移位置由下面判断)
        begin
          vFirstLinePlace := False;
          vPageBreakBottom := vBreakRowFmtTop;
          Break;
        end;
      end;
    end;

    if not vFirstLinePlace then
      Break;
  end;
  {$ENDREGION}

  // 根据上面计算出来的截断位置(可能是PageData底部也可能是整体下移行底部)
  // 处理内容的偏移，循环原理和上面找是否有整体下移行一样
  vCellInc := 0;  // 行各内容为避开分页额外增加的格式化高度
  vRowBreakSeat := 0;

  vColCrosses := TObjectList<TColCross>.Create;
  try

    {$REGION ' 2.记录分页行中各单元格中DrawItem分页时向下偏移量和单元格高度增加的量 '}
    for vC := 0 to FRows[ABreakRow].ColCount - 1 do  // 遍历所有单元格中DrawItem，找从哪个开始向下偏移及偏移量
    begin
      if FRows[ABreakRow][vC].ColSpan < 0 then  // 合并源只需由目标单元格处理
        Continue;

      GetDestCell(ABreakRow, vC, vDestRow, vDestCol);
      vCellData := FRows[vDestRow][vDestCol].CellData;
      vLastDFromRowBottom :=  // 原最后一个DrawItem底部距离行底部的空白距离
        FRows[vDestRow][vDestCol].Height - (FCellVPaddingPix + vCellData.Height + FCellVPaddingPix);

      // 计算目标单元格数据起始位置
      vDestCellDataFmtTop := vBreakRowFmtTop + FCellVPaddingPix;  // 分页行数据起始位置
      if ABreakRow <> vDestRow then
        vDestCellDataFmtTop := vDestCellDataFmtTop - SrcCellDataTopDistanceToDest(ABreakRow, vDestRow);
      //
      vColCross := TColCross.Create;
      vColCross.Col := vC;

      // 判断合并目标内容在当前分页行的分页位置
      for i := 0 to vCellData.DrawItems.Count - 1 do
      begin
        vDrawItem := vCellData.DrawItems[i];
        if not vDrawItem.LineFirst then  // 只需要判断列中每行第一个
          Continue;

        if vDestCellDataFmtTop + vDrawItem.Rect.Bottom + FCellVPaddingPix + FBorderWidthPix > vPageBreakBottom then  // 当前DrawItem底部超过页底部了 20160323002 // 行底部的边框线显示不下时也向下偏移
        begin                                    // |如果FBorderWidth比行高大就不合适
          // 计算分页的DrawItem向下偏移多少可在下一页全显示该DrawItem
          vH := APageDataFmtBottom - (vDestCellDataFmtTop + vDrawItem.Rect.Top) // 页Data底部 - 当前DrawItem在页的相对位置
            + FBorderWidthPix + FCellVPaddingPix - 1;  // 预留出顶部边框和FCellVPadding，因为边框在APageDataFmtBottom也占1像素，所以要减掉
                                            {20190613 一半?}
          // 单元格实际增加的高度 = DrawItem分页向下偏移的距离 - 原最后一个DrawItem底部距离行底部的空白距离(不含底部的FCellVPadding)
          if vH > vLastDFromRowBottom then  // 偏移量比当前单元格下面有空白大时，计算单元格增量
            vCellInc := vH - vLastDFromRowBottom
          else  // 偏移量让底部空白抵消了
            vCellInc := 0;

          vColCross.DrawItemNo := i;  // 从第j个DrawItem处开始分页
          vColCross.VDrawOffset := vH;  // DrawItem分页偏移，注意，DrawItem向下偏移和单元格增加的高并不一定相等，如原底部有空白时，单元格增加高度<Draw向下偏移

          if i > 0 then  // 有能放下的DrawItem
          begin
            if vDestCellDataFmtTop + vCellData.DrawItems[i - 1].Rect.Bottom + FCellVPaddingPix + FBorderWidthPix > vRowBreakSeat then
              vRowBreakSeat := vDestCellDataFmtTop + vCellData.DrawItems[i - 1].Rect.Bottom + FCellVPaddingPix + FBorderWidthPix;
          end
          else  // 第一个DrawItem就放不下
          begin
            if vDestCellDataFmtTop > vRowBreakSeat then
              vRowBreakSeat := vDestCellDataFmtTop - FCellVPaddingPix;
          end;

          Break;
        end;
      end;

      if ACellMaxInc < vCellInc then
        ACellMaxInc := vCellInc;  // 记录各列中分页向下偏移的最大增量

      vColCrosses.Add(vColCross);
    end;

    vRowBreakSeat := vRowBreakSeat - ADrawItemRectTop + 1;  // 起始为x，截断为y，截断处高度是x-y+1
    {$ENDREGION}

    if (FFixRow >= 0) and (ABreakRow > FFixRow + FFixRowCount - 1) then  // 分页行超过固定行
    begin
      vFixHeight := GetFixRowHeight;
      ACellMaxInc := ACellMaxInc + vFixHeight;
    end
    else
      vFixHeight := 0;

    if not vFirstLinePlace then  // 某单元格第一行内容就在当前页放不下了，即表格跨页行整行需要下移到下一页(下移量可能是0)
    begin
      //vRowBreakSeat := 0;

      if ABreakRow = 0 then  // 表格第一行所有单元格都在当前页放不下，需要整体下移
      begin
        AFmtOffset := APageDataFmtBottom - ADrawItemRectTop;
        ACellMaxInc := 0;  // 整体向下偏移时，就代表了第一行的向下偏移，或者说第一行的FmtOffset永远是0，因为整体向下偏移的依据是判断第一行
        Exit;
      end;

      // 3.当分页的行中某源对应的目标单元格内容在分页处有向下偏移量，而当前行中
      // 普通单元格正好在顶部分页，偏移量为0(不需要偏移)，如果直接将偏移量增加
      // 到此行是不正确的(导致普通单元格在下一页起始位置不在页顶部)，应该是将
      // 偏移量增加到此行高度
      for i := 0 to vColCrosses.Count - 1 do  // vColCrosses里只有合并目标或普通单元格
      begin
        if (vColCrosses[i].VDrawOffset > 0) and (vColCrosses[i].DrawItemNo = 0) then  // 第一个放不下，需要整行向下偏移
        begin
          FRows[ABreakRow].FmtOffset := vColCrosses[i].VDrawOffset + vFixHeight;  // 表格行向下偏移后整行起始在下一页显示，同行多个单元都放不下第一个时会重复赋相同值(重复赋值会有不一样的值吗？)
          vColCrosses[i].VDrawOffset := 0;  // 整体偏移了，DrawItem就不用单独记偏移量了
        end;
      end;
    end
    else
      FRows[ABreakRow].Height := FRows[ABreakRow].Height + ACellMaxInc;

    AddPageBreak(ABreakRow, vRowBreakSeat);

    for vC := 0 to vColCrosses.Count - 1 do  // 遍历所有内容向下有偏移的单元格，将偏移扩散到分页后面的DrawItem
    begin
      if (vColCrosses[vC].DrawItemNo < 0) or (vColCrosses[vC].VDrawOffset = 0) then  // 不需要偏移
        Continue;

      GetDestCell(ABreakRow, vColCrosses[vC].Col, vDestRow, vDestCol);
      vCellData := FRows[vDestRow][vDestCol].CellData;
      for i := vColCrosses[vC].DrawItemNo to vCellData.DrawItems.Count - 1 do
        OffsetRect(vCellData.DrawItems[i].Rect, 0, vColCrosses[vC].VDrawOffset + vFixHeight);
    end;

    // 当前行分页的单元格，有的可能是合并源，目标对应的源在此行下面，所以为了使
    // 各个单元格分页增加的偏移量能够传递到对应的结束单元格，从分页行重新格式化
    CalcMergeRowHeightFrom(ABreakRow);
  finally
    FreeAndNil(vColCrosses);
  end;
end;

procedure THCTableItem.CheckFormatPageBreakBefor;
begin
  FPageBreaks.Clear;
end;

function THCTableItem.ColCanDelete(const ACol: Integer): Boolean;
var
  vRow: Integer;
begin
  Result := False;
  for vRow := 0 to RowCount - 1 do
  begin
    if FRows[vRow][ACol].ColSpan > 0 then  // 有合并目标的行暂时不支持
      Exit;
  end;
  Result := True;
end;

function THCTableItem.CoordInSelect(const X, Y: Integer): Boolean;
var
  vCellPt: TPoint;
  vCellData: THCTableCellData;
  vX, vY, vItemNo, vDrawItemNo, vOffset, vRow, vCol: Integer;
  vRestrain: Boolean;
  vResizeInfo: TResizeInfo;
begin
  Result := inherited CoordInSelect(X, Y);  // 有选中且在RectItem区域中(粗略估算)
  if Result then
  begin
    vResizeInfo := GetCellAt(X, Y, vRow, vCol);  // 坐标处信息
    Result := vResizeInfo.TableSite = TTableSite.tsCell;  // 坐标处在单元格中不在边框上
    if Result then  // 在单元格中，判断单元格是否在选中范围内
    begin
      if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
      begin
        if FSelectCellRang.EndRow >= 0 then  // 有选择结束行
        begin
          Result := (vRow >= FSelectCellRang.StartRow)
            and (vRow <= FSelectCellRang.EndRow)
            and (vCol >= FSelectCellRang.StartCol)
            and (vCol <= FSelectCellRang.EndCol);
        end
        else  // 无选择结束行，判断是否在当前单元格的选中中
        begin
          vCellData := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData;
          if vCellData.SelectExists then
          begin
            vCellPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
            vX := X - vCellPt.X - FCellHPaddingPix;
            vY := Y - vCellPt.Y - FCellVPaddingPix;
            vCellData.GetItemAt(vX, vY, vItemNo, vOffset, vDrawItemNo, vRestrain);

            Result := vCellData.CoordInSelect(vX, vY, vItemNo, vOffset, vRestrain);
          end;
        end;
      end;
    end;
  end;
end;

procedure THCTableItem.SaveSelectToStream(const AStream: TStream);
var
  vCellData: THCCustomData;
  vData: THCTableCellData;
  //vTable: THCTableItem;
  vRow, vCol: Integer;
begin
  if Self.IsSelectComplate then  // 全选择了
    raise Exception.Create('保存选中内容出错，表格不应该由内部处理全选中的保存！')
  else
  if FSelectCellRang.EditCell then
  begin
    vCellData := GetActiveData;
    if vCellData <> nil then
      vCellData.SaveSelectToStream(AStream);
  end
  else
  if FSelectCellRang.SelectExists then  // 多选单元格
  begin
    //vWidth := 0;
    //for vCol := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
    //  vWidth := vWidth + FColWidths[vCol];

    vData := THCTableCellData.Create(OwnerData.Style);
    try
      vData.BeginFormat;
      //vData.Width := vWidth;
      vData.OnSaveItem := OwnerData.OnSaveItem;
      //vTable := vData.CreateItemByStyle(Self.StyleNo);
      for vRow := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
      begin
        for vCol := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
        begin
          vCellData := FRows[vRow][vCol].CellData;
          if Assigned(vCellData) then
            vData.AddData(vCellData);
        end;
      end;

      //vData.InsertItem(vTable);
      vData.SaveToStream(AStream);
    finally
      FreeAndNil(vData);
    end;
  end;
end;

procedure THCTableItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
var
  i, vR, vC: Integer;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  AStream.WriteBuffer(FBorderVisible, SizeOf(FBorderVisible));
  AStream.WriteBuffer(FBorderWidthPt, SizeOf(FBorderWidthPt));
  AStream.WriteBuffer(FCellVPaddingMM, SizeOf(FCellVPaddingMM));
  AStream.WriteBuffer(FCellHPaddingMM, SizeOf(FCellHPaddingMM));
  AStream.WriteBuffer(FRows.Count, SizeOf(FRows.Count));  // 行数
  AStream.WriteBuffer(FColWidths.Count, SizeOf(FColWidths.Count));  // 列数

  // 固定行数、列数信息
  AStream.WriteBuffer(FFixRow, SizeOf(FFixRow));
  AStream.WriteBuffer(FFixRowCount, SizeOf(FFixRowCount));
  AStream.WriteBuffer(FFixCol, SizeOf(FFixCol));
  AStream.WriteBuffer(FFixColCount, SizeOf(FFixColCount));

  for i := 0 to FColWidths.Count - 1 do  // 各列标准宽度
  begin
    vC := FColWidths[i];
    AStream.WriteBuffer(vC, SizeOf(vC));
  end;

  for vR := 0 to FRows.Count - 1 do  // 各行数据
  begin
    AStream.WriteBuffer(FRows[vR].AutoHeight, SizeOf(Boolean));
    if not FRows[vR].AutoHeight then
      AStream.WriteBuffer(FRows[vR].Height, SizeOf(Integer));

    for vC := 0 to FRows[vR].ColCount - 1 do  // 各列数据
      FRows[vR][vC].SaveToStream(AStream);
  end;
end;

function THCTableItem.Search(const AKeyword: string; const AForward,
  AMatchCase: Boolean): Boolean;
var
  i, j, vRow, vCol: Integer;
begin
  Result := False;

  if AForward then  // 向前查找
  begin
    if FSelectCellRang.StartRow < 0 then  // 没有编辑的单元格
    begin
      FSelectCellRang.StartRow := FRows.Count - 1;
      FSelectCellRang.StartCol := FColWidths.Count - 1;

      vRow := FSelectCellRang.StartRow;
      vCol := FSelectCellRang.StartCol;

      // 从最后开始
      if FRows[vRow][vCol].CellData <> nil then
      begin
        with FRows[vRow][vCol].CellData do
        begin
          SelectInfo.StartItemNo := Items.Count - 1;
          SelectInfo.StartItemOffset := GetItemOffsetAfter(Items.Count - 1);
        end;
      end;
    end;

    vRow := FSelectCellRang.StartRow;
    vCol := FSelectCellRang.StartCol;

    if (vRow >= 0) and (vCol >= 0) then
    begin
      if FRows[vRow][vCol].CellData <> nil then
        Result := FRows[vRow][vCol].CellData.Search(AKeyword, AForward, AMatchCase);

      if not Result then  // 当前单元格没找到
      begin
        for j := vCol - 1 downto 0 do  // 在同行后面的单元格找
        begin
          if (FRows[vRow][j].ColSpan < 0) or (FRows[vRow][j].RowSpan < 0) then
            Continue
          else
          begin
            with FRows[vRow][j].CellData do
            begin
              SelectInfo.StartItemNo := Items.Count - 1;
              SelectInfo.StartItemOffset := GetItemOffsetAfter(Items.Count - 1);
            end;

            Result := FRows[vRow][j].CellData.Search(AKeyword, AForward, AMatchCase);
          end;

          if Result then
          begin
            FSelectCellRang.StartCol := j;
            Break;
          end;
        end;
      end;

      if not Result then  // 同行后面的单元格没找到
      begin
        for i := FSelectCellRang.StartRow - 1 downto 0 do
        begin
          for j := FColWidths.Count - 1 downto 0 do
          begin
            if (FRows[i][j].ColSpan < 0) or (FRows[i][j].RowSpan < 0) then
              Continue
            else
            begin
              with FRows[i][j].CellData do
              begin
                SelectInfo.StartItemNo := Items.Count - 1;
                SelectInfo.StartItemOffset := GetItemOffsetAfter(Items.Count - 1);
              end;

              Result := FRows[i][j].CellData.Search(AKeyword, AForward, AMatchCase);
            end;

            if Result then
            begin
              FSelectCellRang.StartCol := j;
              Break;
            end;
          end;

          if Result then
          begin
            FSelectCellRang.StartRow := i;
            Break;
          end;
        end;
      end;
    end;
  end
  else  // 向后查找
  begin
    if FSelectCellRang.StartRow < 0 then  // 没有编辑的单元格
    begin
      FSelectCellRang.StartRow := 0;
      FSelectCellRang.StartCol := 0;

      // 从头开始
      FRows[0][0].CellData.SelectInfo.StartItemNo := 0;
      FRows[0][0].CellData.SelectInfo.StartItemOffset := 0;
    end;

    vRow := FSelectCellRang.StartRow;
    vCol := FSelectCellRang.StartCol;

    if (vRow >= 0) and (vCol >= 0) then
    begin
      Result := FRows[vRow][vCol].CellData.Search(AKeyword, AForward, AMatchCase);
      if not Result then  // 当前单元格没找到
      begin
        for j := vCol + 1 to FColWidths.Count - 1 do  // 在同行后面的单元格找
        begin
          if (FRows[vRow][j].ColSpan < 0) or (FRows[vRow][j].RowSpan < 0) then
            Continue
          else
          begin
            FRows[vRow][j].CellData.SelectInfo.StartItemNo := 0;
            FRows[vRow][j].CellData.SelectInfo.StartItemOffset := 0;

            Result := FRows[vRow][j].CellData.Search(AKeyword, AForward, AMatchCase);
          end;

          if Result then
          begin
            FSelectCellRang.StartCol := j;
            Break;
          end;
        end;
      end;

      if not Result then  // 同行后面的单元格没找到
      begin
        for i := FSelectCellRang.StartRow + 1 to FRows.Count - 1 do
        begin
          for j := 0 to FColWidths.Count - 1 do
          begin
            if (FRows[i][j].ColSpan < 0) or (FRows[i][j].RowSpan < 0) then
              Continue
            else
            begin
              FRows[i][j].CellData.SelectInfo.StartItemNo := 0;
              FRows[i][j].CellData.SelectInfo.StartItemOffset := 0;

              Result := FRows[i][j].CellData.Search(AKeyword, AForward, AMatchCase);
            end;

            if Result then
            begin
              FSelectCellRang.StartCol := j;
              Break;
            end;
          end;

          if Result then
          begin
            FSelectCellRang.StartRow := i;
            Break;
          end;
        end;
      end;
    end;
  end;

  if not Result then
    FSelectCellRang.Initialize;
end;

procedure THCTableItem.SelectAll;
begin
  SelectComplate;
end;

function THCTableItem.SelectExists: Boolean;
begin
  Result := False;
  if Self.IsSelectComplate then
    Result := True
  else
  if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
  begin
    if FSelectCellRang.EndRow >= 0 then  // 有选择结束行
      Result := True
    else  // 无选择结束行，判断当前单元格是否有选中
      Result := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.SelectExists;
  end;

//  Result := (FSelectCellRang.StartRow >= 0) and (FSelectCellRang.EndRow >= 0);  // 存在选中起始和结束行
//  if Result then
//  begin
//    if FSelectCellRang.SameCell then  // 选择在同一个单元格中，由单元格决定是否有选中内容
//      Result := CellSelectComplate(FSelectCellRang.StartRow, FSelectCellRang.StartCol)
//        or Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.SelectExists;
//  end;
end;

procedure THCTableItem.SetActive(const Value: Boolean);
var
  vCell: THCTableCell;
begin
  if Self.Active <> Value then
  begin
    vCell := GetEditCell;
    if vCell <> nil then
      vCell.Active := Value;

    if not Value then
      Self.InitializeMouseInfo;

    inherited SetActive(Value);
  end;
end;

procedure THCTableItem.SetActiveItemText(const AText: string);
begin
  inherited SetActiveItemText(AText);

  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    CellChangeByAction(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      procedure
      var
        vEditCell: THCTableCell;
      begin
        vEditCell := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol];
        vEditCell.CellData.SetActiveItemText(AText);
      end);
  end;
end;

procedure THCTableItem.SetBorderWidthPt(const Value: Single);
begin
  if FBorderWidthPt <> Value then
  begin
    //if Value > FCellVPadding * 2 then  // 要求不大于最小行高，否则分页计算会有问题
    //  FBorderWidth := FCellVPadding * 2 - 1
    //else
    FBorderWidthPt := Value;
    FBorderWidthPix := HCUnitConversion.PtToPixel(FBorderWidthPt, HCUnitConversion.PixelsPerInchX);
    Self.FormatDirty;
  end;
end;

procedure THCTableItem.SetCellHPaddingMM(const Value: Single);
begin
  if FCellHPaddingMM <> Value then
  begin
    FCellHPaddingMM := Value;
    FCellHPaddingPix := HCUnitConversion.MillimeterToPixX(FCellHPaddingMM);
    Self.FormatDirty;
  end;
end;

procedure THCTableItem.SetCellVPaddingMM(const Value: Single);
begin
  if FCellVPaddingMM <> Value then
  begin
    FCellVPaddingMM := Value;
    FCellVPaddingPix := HCUnitConversion.MillimeterToPixY(FCellVPaddingMM);
    Self.FormatDirty;
  end;
end;

procedure THCTableItem.SetColWidth(AIndex: Integer; const AWidth: Integer);
var
  vR: Integer;
begin
  FColWidths[AIndex] := AWidth;
  for vR := 0 to Self.RowCount - 1 do
  begin
    Cells[vR, AIndex].Width := AWidth;
    if Assigned(Cells[vR, AIndex].CellData) then
      Cells[vR, AIndex].CellData.Width := AWidth - FCellHPaddingPix - FCellHPaddingPix;
  end;
end;

procedure THCTableItem.SetResizing(const Value: Boolean);
begin
  inherited SetResizing(Value);
end;

function THCTableItem.SplitCurCol: Boolean;
var
  i, vR, vCurRow, vCurCol,
  vDestRow, vDestCol, vSrcRow, vSrcCol: Integer;
  vLeftCell: THCTableCell;
begin
  Result := False;

  // 借用 vLeftCell 变量
  vLeftCell := GetEditCell;
  if vLeftCell = nil then Exit;
  vLeftCell.CellData.InitializeField;

  vCurRow := FSelectCellRang.StartRow;
  vCurCol := FSelectCellRang.StartCol;

  Undo_Mirror;
  // 拆分时，光标所单元格RowSpan>=0，ColSpan>=0
  if FRows[vCurRow][vCurCol].ColSpan > 0 then  // 拆分时光标所在的单元格是列合并目标，将合并拆开
  begin
    GetSourceCell(vCurRow, vCurCol, vSrcRow, vSrcCol);  // 得到范围

    FRows[vCurRow][vCurCol].ColSpan := 0;  // 合并目标不再向右合并单元格了
    for i := vCurCol + 1 to vSrcCol do  // 目标列同行右侧的重新设置合并目标
    begin
      for vR := vCurRow to vSrcRow do  // 遍历拆分前光标所在的行各列
        FRows[vR][i].ColSpan := FRows[vR][i].ColSpan + 1;
    end;

    // 原合并目标单元格右侧的单元格作为拆分后，右侧合并源的新目标
    FRows[vCurRow][vCurCol + 1].CellData := THCTableCellData.Create(OwnerData.Style);
    FRows[vCurRow][vCurCol + 1].RowSpan := vSrcRow - vCurRow;
    FRows[vCurRow][vCurCol + 1].ColSpan := vSrcCol - (vCurCol + 1);
  end
  else  // Cells[vCurRow, vCurCol].ColSpan = 0 拆分时光标所在单元格是普通单元格
  if InsertCol(vCurCol + 1, 1) then  // 右侧插入列
  begin
    vR := 0;
    while vR < Self.RowCount do
    begin
      vLeftCell := FRows[vR][vCurCol];

      if vR = vCurRow then  // 拆分时光标所在行，此时vLeftCell.ColSpan = 0
      begin
        if vLeftCell.RowSpan > 0 then  // 前面是行合并目标
        begin
          vSrcRow := vCurRow + vLeftCell.RowSpan;
          while vR <= vSrcRow do
          begin
            FRows[vR][vCurCol + 1].RowSpan := FRows[vR][vCurCol].RowSpan;
            if FRows[vR][vCurCol + 1].RowSpan < 0 then
            begin
              FRows[vR][vCurCol + 1].CellData.Free;
              FRows[vR][vCurCol + 1].CellData := nil;
            end;

            Inc(vR);
          end;
        end
        else  // vLeftCell.RowSpan < 0 的在RowSpan > 0 里处理了，vLeftCell.RowSpan = 0 不需要处理
          Inc(vR);
      end
      else  // vR <> vCurRow
      begin
        if vLeftCell.RowSpan = 0 then
        begin
          if vLeftCell.ColSpan = 0 then  // 左侧是普通单元格
          begin
            FRows[vR][vCurCol + 1].CellData.Free;
            FRows[vR][vCurCol + 1].CellData := nil;
            FRows[vR][vCurCol + 1].ColSpan := -1;
            vLeftCell.ColSpan := 1;
            Inc(vR);
          end
          else
          if vLeftCell.ColSpan < 0 then  // 同行合并的源列
          begin
            vDestCol := vCurCol + vLeftCell.ColSpan;  // 目标列
            vSrcCol := vDestCol + FRows[vR][vDestCol].ColSpan;
            if vCurCol = vSrcCol then  // 左侧是合并范围最后，插入的需要合并进前面
            begin
              FRows[vR][vCurCol + 1].CellData.Free;
              FRows[vR][vCurCol + 1].CellData := nil;
              FRows[vR][vCurCol + 1].ColSpan := vLeftCell.ColSpan - 1;
              FRows[vR][vDestCol].ColSpan := FRows[vR][vDestCol].ColSpan + 1;
            end;

            Inc(vR);
          end
          else  // vLeftCell.ColSpan > 0 左侧是同行合并目标，由右侧插入列处理了插入列的合并
            Inc(vR);
        end
        else
        if vLeftCell.RowSpan > 0 then  // 合并目标
        begin
          if vLeftCell.ColSpan = 0 then  // 同列合并，右侧插入的合并到目标
          begin
            vLeftCell.ColSpan := 1;
            vDestRow := vR;
            vSrcRow := vR + vLeftCell.RowSpan;

            while vR <= vSrcRow do
            begin
              FRows[vR][vCurCol + 1].CellData.Free;
              FRows[vR][vCurCol + 1].CellData := nil;
              FRows[vR][vCurCol + 1].RowSpan := vDestRow - vR;
              FRows[vR][vCurCol + 1].ColSpan := -1;
              Inc(vR);
            end;
          end
          else  // 合并目标不可能 vLeftCell.ColSpan < 0，vLeftCell.ColSpan > 0由右侧插入列处理了合并
            Inc(vR);
        end
        else  // vLeftCell.RowSpan < 0 的情况，由目标单元格在vLeftCell.RowSpan > 0中处理了
          Inc(vR);
      end;
    end;
  end;

  Self.FormatDirty;
  Result := True;
end;

function THCTableItem.SplitCurRow: Boolean;
var
  i, vC, vCurRow, vCurCol,
  vDestRow, vDestCol, vSrcRow, vSrcCol: Integer;
  vTopCell: THCTableCell;
begin
  Result := False;

  // 借用 vTopCell 变量
  vTopCell := GetEditCell;
  if vTopCell = nil then Exit;
  vTopCell.CellData.InitializeField;

  vCurRow := FSelectCellRang.StartRow;
  vCurCol := FSelectCellRang.StartCol;

  Undo_Mirror;
  // 拆分时，光标所单元格RowSpan>=0，ColSpan>=0
  if FRows[vCurRow][vCurCol].RowSpan > 0 then  // 拆分时光标所在的单元格是行合并目标，将合并拆开
  begin
    GetSourceCell(vCurRow, vCurCol, vSrcRow, vSrcCol);  // 得到范围

    FRows[vCurRow][vCurCol].RowSpan := 0;  // 目标不再向下合并单元格了
    for i := vCurRow + 1 to vSrcRow do  // 从目标行下一行开始，重新设置合并目标
    begin
      for vC := vCurCol to vSrcCol do  // 遍历拆分前光标所在的行各列
        FRows[i][vC].RowSpan := FRows[i][vC].RowSpan + 1;
    end;

    // 原合并目标单元格正下面的单元格作为拆分后，下面合并源的新目标
    FRows[vCurRow + 1][vCurCol].CellData := THCTableCellData.Create(OwnerData.Style);
    FRows[vCurRow + 1][vCurCol].RowSpan := vSrcRow - (vCurRow + 1);
    FRows[vCurRow + 1][vCurCol].ColSpan := vSrcCol - vCurCol;
  end
  else  // Cells[vCurRow, vCurCol].RowSpan = 0 拆分时光标所在单元格是普通单元格
  if InsertRow(vCurRow + 1, 1) then  // 下面插入行
  begin
    vC := 0;
    while vC < Self.ColCount do
    begin
      vTopCell := FRows[vCurRow][vC];

      if vC = vCurCol then  // 拆分时光标所在列，此时vTopCell.RowSpan = 0
      begin
        if vTopCell.ColSpan > 0 then  // 上面是列合并目标
        begin
          vSrcCol := vCurCol + vTopCell.ColSpan;
          while vC <= vSrcCol do
          begin
            FRows[vCurRow + 1][vC].ColSpan := FRows[vCurRow][vC].ColSpan;
            if FRows[vCurRow + 1][vC].ColSpan < 0 then
            begin
              FRows[vCurRow + 1][vC].CellData.Free;
              FRows[vCurRow + 1][vC].CellData := nil;
            end;

            Inc(vC);
          end;
        end
        else  // vLeftCell.ColSpan < 0 的在ColSpan > 0 里处理了，vLeftCell.ColSpan = 0 不需要处理
          Inc(vC);
      end
      else  // vC <> vCurCol
      begin
        if vTopCell.ColSpan = 0 then
        begin
          if vTopCell.RowSpan = 0 then  // 上面是普通单元格
          begin
            FRows[vCurRow + 1][vC].CellData.Free;
            FRows[vCurRow + 1][vC].CellData := nil;
            FRows[vCurRow + 1][vC].RowSpan := -1;
            vTopCell.RowSpan := 1;
            Inc(vC);
          end
          else
          if vTopCell.RowSpan < 0 then  // 同列合并的源列
          begin
            vDestRow := vCurRow + vTopCell.RowSpan;  // 目标行
            vSrcRow := vDestRow + FRows[vDestRow][vC].RowSpan;
            if vCurRow = vSrcRow then  // 上面是合并范围最后，插入的需要合并进上面
            begin
              FRows[vCurRow + 1][vC].CellData.Free;
              FRows[vCurRow + 1][vC].CellData := nil;
              FRows[vCurRow + 1][vC].RowSpan := vTopCell.RowSpan - 1;
              FRows[vDestRow][vC].RowSpan := FRows[vDestRow][vC].RowSpan + 1;
            end;

            Inc(vC);
          end
          else  // vTopCell.RowSpan > 0 上面是同行合并目标，由上面插入行处理了插入行的合并
            Inc(vC);
        end
        else
        if vTopCell.ColSpan > 0 then  // 合并目标
        begin
          if vTopCell.RowSpan = 0 then  // 同行合并，下面插入的合并到目标
          begin
            vTopCell.RowSpan := 1;
            vDestCol := vC;
            vSrcCol := vC + vTopCell.ColSpan;

            while vC <= vSrcCol do
            begin
              FRows[vCurRow + 1][vC].CellData.Free;
              FRows[vCurRow + 1][vC].CellData := nil;
              FRows[vCurRow + 1][vC].ColSpan := vDestCol - vC;
              FRows[vCurRow + 1][vC].RowSpan := -1;
              Inc(vC);
            end;
          end
          else  // 合并目标不可能 vTopCell.RowSpan < 0，vTopCell.RowSpan > 0由上面插入行处理了合并
            Inc(vC);
        end
        else  // vLeftCell.ColSpan < 0 的情况，由目标单元格在vLeftCell.ColSpan > 0中处理了
          Inc(vC);
      end;
    end;
  end;

  Self.FormatDirty;
  Result := True;
end;

function THCTableItem.SrcCellDataTopDistanceToDest(const ASrcRow, ADestRow: Integer): Integer;
var
  vR: Integer;
begin
  Result := {FCellVPadding和下面的减约掉 +} FBorderWidthPix + FRows[ASrcRow].FmtOffset;

  vR := ASrcRow - 1;
  while vR > ADestRow do
  begin
    Result := Result + FRows[vR].Height + FBorderWidthPix + FRows[vR].FmtOffset;
    Dec(vR);
  end;

  Result := Result + FRows[ADestRow].Height{ - FCellVPadding和上面加约掉};
end;

procedure THCTableItem.SelectComplate;
var
  vRow, vCol: Integer;
begin
  inherited SelectComplate;

  FSelectCellRang.StartRow := 0;
  FSelectCellRang.StartCol := 0;
  FSelectCellRang.EndRow := Self.RowCount - 1;
  FSelectCellRang.EndCol := FColWidths.Count - 1;

  for vRow := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
  begin
    for vCol := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
    begin
      if FRows[vRow][vCol].CellData <> nil then
        FRows[vRow][vCol].CellData.SelectAll;
    end;
  end;
end;

function THCTableItem.SelectedCellCanMerge: Boolean;
var
  vEndRow, vEndCol: Integer;
begin
  Result := False;
  if FSelectCellRang.SelectExists then
  begin
    vEndRow := FSelectCellRang.EndRow;
    vEndCol := FSelectCellRang.EndCol;
    AdjustCellRange(FSelectCellRang.StartRow, FSelectCellRang.StartCol, vEndRow, vEndCol);
    Result := CellsCanMerge(FSelectCellRang.StartRow, FSelectCellRang.StartCol, vEndRow, vEndCol);
  end;
end;

function THCTableItem.ToHtml(const APath: string): string;
var
  vR, vC: Integer;
  vCell: THCTableCell;
begin
  Result := '<table border="' + IntToStr(FBorderWidthPix) + '" cellpadding="0"; cellspacing="0">';
  for vR := 0 to FRows.Count - 1 do
  begin
    Result := Result + sLineBreak + '<tr>';
    for vC := 0 to FColWidths.Count - 1 do
    begin
      vCell := FRows[vR][vC];
      if (vCell.RowSpan < 0) or (vCell.ColSpan < 0) then
        Continue;

      Result := Result + sLineBreak + Format('<td rowspan="%d"; colspan="%d"; width="%d"; height="%d">',
        [vCell.RowSpan + 1, vCell.ColSpan + 1, vCell.Width, vCell.Height]);

      if Assigned(vCell.CellData) then
        Result := Result + vCell.CellData.ToHtml(APath);

      Result := Result + sLineBreak + '</td>';
    end;
    Result := Result + sLineBreak + '</tr>';
  end;
  Result := Result + sLineBreak + '</table>';
end;

procedure THCTableItem.ToXml(const ANode: IHCXMLNode);
var
  vR, vC: Integer;
  vS: string;
begin
  inherited ToXml(ANode);

  vS := IntToStr(FColWidths[0]);
  for vC := 1 to FColWidths.Count - 1 do  // 各列标准宽度
    vS := vS + ',' + IntToStr(FColWidths[vC]);

  ANode.Attributes['bordervisible'] := FBorderVisible;
  ANode.Attributes['borderwidthpt'] := FBorderWidthPt;
  ANode.Attributes['cellvpadding'] := FCellVPaddingMM;
  ANode.Attributes['cellhpadding'] := FCellHPaddingMM;
  ANode.Attributes['row'] := FRows.Count;
  ANode.Attributes['col'] := FColWidths.Count;
  ANode.Attributes['colwidth'] := vS;
  ANode.Attributes['link'] := '';

  for vR := 0 to FRows.Count - 1 do  // 各行数据
    FRows[vR].ToXml(ANode.AddChild('row'));
end;

procedure THCTableItem.TraverseItem(const ATraverse: THCItemTraverse);
var
  vR, vC: Integer;
begin
  for vR := 0 to FRows.Count - 1 do
  begin
    if ATraverse.Stop then Break;

    for vC := 0 to FColWidths.Count - 1 do
    begin
      if ATraverse.Stop then Break;

      if FRows[vR][vC].CellData <> nil then
        FRows[vR][vC].CellData.TraverseItem(ATraverse);
    end;
  end;
end;

procedure THCTableItem.Undo_ColResize(const ACol, AOldWidth, ANewWidth: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vColSizeUndoData: THCColSizeUndoData;
begin
  vUndoList := GetSelfUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    SelfUndo_New;
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vColSizeUndoData := THCColSizeUndoData.Create;
      vColSizeUndoData.Col := ACol;
      vColSizeUndoData.OldWidth := AOldWidth;
      vColSizeUndoData.NewWidth := ANewWidth;

      vUndo.Data := vColSizeUndoData;
    end;
  end;
end;

procedure THCTableItem.Undo_Mirror;
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vMirrorUndoData: THCMirrorUndoData;
begin
  vUndoList := GetSelfUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    SelfUndo_New;
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vMirrorUndoData := THCMirrorUndoData.Create;
      Self.SaveToStream(vMirrorUndoData.Stream);

      vUndo.Data := vMirrorUndoData;
    end;
  end;
end;

procedure THCTableItem.Undo_RowResize(const ARow, AOldHeight, ANewHeight: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vRowSizeUndoData: THCRowSizeUndoData;
begin
  vUndoList := GetSelfUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    SelfUndo_New;
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vRowSizeUndoData := THCRowSizeUndoData.Create;
      vRowSizeUndoData.Row := ARow;
      vRowSizeUndoData.OldHeight := AOldHeight;
      vRowSizeUndoData.NewHeight := ANewHeight;

      vUndo.Data := vRowSizeUndoData;
    end;
  end;
end;

function THCTableItem.WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean;
begin
  Result := True;
end;

function THCTableItem.ActiveDataResizing: Boolean;
begin
  Result := False;
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
    Result := FRows[FSelectCellRang.StartRow][FSelectCellRang.StartCol].CellData.SelectedResizing;
end;

procedure THCTableItem.AdjustCellRange(const AStartRow, AStartCol: Integer;
  var AEndRow, AEndCol: Integer);
var
  vR, vC, vLastRow, vLastCol, vDestRow, vDestCol: Integer;
  vCell: THCTableCell;
begin
  // 根据起始单元格和选中结束确定的矩形有效范围
  vLastRow := AEndRow;
  vLastCol := AEndCol;
  for vR := AStartRow to AEndRow do
  begin
    for vC := AStartCol to AEndCol do
    begin
      vCell := FRows[vR][vC];
      if (vCell.RowSpan > 0) or (vCell.ColSpan > 0) then
      begin
        GetDestCell(vR, vC, vDestRow, vDestCol);
        vCell := FRows[vDestRow][vDestCol];
        vDestRow := vDestRow + vCell.RowSpan;
        vDestCol := vDestCol + vCell.ColSpan;
        if vLastRow < vDestRow then
          vLastRow := vDestRow;

        if vLastCol < vDestCol then
          vLastCol := vDestCol;
      end;
    end;
  end;

  AEndRow := vLastRow;
  AEndCol := vLastCol;
end;

procedure THCTableItem.ApplyContentAlign(const AAlign: THCContentAlign);

  procedure ApplyCellAlign_(const ACell: THCTableCell);
  var
    vData: THCTableCellData;
  begin
    case AAlign of
      tcaTopLeft, tcaTopCenter, tcaTopRight:
        ACell.AlignVert := THCAlignVert.cavTop;

      tcaCenterLeft, tcaCenterCenter, tcaCenterRight:
        ACell.AlignVert := THCAlignVert.cavCenter;

      tcaBottomLeft, tcaBottomCenter, tcaBottomRight:
        ACell.AlignVert := THCAlignVert.cavBottom;
    end;

    vData := ACell.CellData;
    if Assigned(vData) then
    begin
      vData.BeginFormat;
      try
        case AAlign of
          tcaTopLeft, tcaCenterLeft, tcaBottomLeft:
            vData.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);

          tcaTopCenter, tcaCenterCenter, tcaBottomCenter:
            vData.ApplyParaAlignHorz(TParaAlignHorz.pahCenter);

          tcaTopRight, tcaCenterRight, tcaBottomRight:
            vData.ApplyParaAlignHorz(TParaAlignHorz.pahRight);
        end;
      finally
        vData.EndFormat(True);
      end;
    end;
  end;

var
  vR, vC: Integer;
begin
  inherited ApplyContentAlign(AAlign);

  if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
  begin
    if FSelectCellRang.EndRow >= 0 then  // 有选择结束行，说明选中不在同一单元格
    begin
      for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
      begin
        for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
          ApplyCellAlign_(FRows[vR][vC]);
      end;
    end
    else  // 在同一单元格
      ApplyCellAlign_(GetEditCell);
  end;
end;

procedure THCTableItem.ApplySelectParaStyle(const AStyle: THCStyle;
  const AMatchStyle: THCParaMatch);
var
  vR, vC: Integer;
  vData: THCTableCellData;
begin
  inherited ApplySelectParaStyle(AStyle, AMatchStyle);

  if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
  begin
    if FSelectCellRang.EndRow >= 0 then  // 有选择结束行，说明选中不在同一单元格
    begin
      for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
      begin
        for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
        begin
          vData := FRows[vR][vC].CellData;
          if Assigned(vData) then
          begin
            if Self.SizeChanged then  // 表格会重新格式化，CellData不用格式化了
            begin
              vData.BeginFormat;
              try
                vData.ApplySelectParaStyle(AMatchStyle);
              finally
                vData.EndFormat(False);
              end;
            end
            else
            begin
              vData.ApplySelectParaStyle(AMatchStyle);
              Self.SizeChanged := vData.FormatHeightChange or vData.FormatDrawItemCountChange;
            end;
          end;
        end;
      end;
    end
    else  // 在同一单元格
    begin
      vData := GetEditCell.CellData;
      vData.ApplySelectParaStyle(AMatchStyle);
      Self.SizeChanged := vData.FormatHeightChange or vData.FormatDrawItemCountChange;
    end;

    //FLastChangeFormated := not Self.SizeChanged;
  end
  else
    Self.ParaNo := AMatchStyle.GetMatchParaNo(OwnerData.Style, Self.ParaNo);
end;

procedure THCTableItem.ApplySelectTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch);
var
  vR, vC: Integer;
  vData: THCTableCellData;
begin
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
  begin
    vData := GetEditCell.CellData;
    vData.ApplySelectTextStyle(AMatchStyle);
    Self.SizeChanged := vData.FormatHeightChange or vData.FormatDrawItemCountChange;
  end
  else
  if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
  begin
    for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
    begin
      { TODO -jingtong : 当单元格SelectComplate时，处理全部应用样式 }
      for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
      begin
        vData := FRows[vR][vC].CellData;
        if Assigned(vData) then
        begin
          if Self.SizeChanged then  // 表格会重新格式化，CellData不用格式化了
          begin
            vData.BeginFormat;
            try
              vData.ApplySelectTextStyle(AMatchStyle);
            finally
              vData.EndFormat(False);
            end;
          end
          else
          begin
            vData.ApplySelectTextStyle(AMatchStyle);
            Self.SizeChanged := vData.FormatHeightChange or vData.FormatDrawItemCountChange;
          end;
        end;
      end;
    end;
  end;

  //FLastChangeFormated := not Self.SizeChanged;
end;

procedure THCTableItem.Assign(Source: THCCustomItem);
var
  vR, vC: Integer;
  vSrcTable: THCTableItem;
begin
  // 必需保证行、列数量一致
  inherited Assign(Source);

  vSrcTable := Source as THCTableItem;

  FBorderVisible := vSrcTable.BorderVisible;
  BorderWidthPt := vSrcTable.BorderWidthPt;
  CellHPaddingMM := vSrcTable.CellHPaddingMM;
  CellVPaddingMM := vSrcTable.CellVPaddingMM;
  FFixRow := vSrcTable.FixRow;
  FFixRowCount := vSrcTable.FixRowCount;
  FFixCol := vSrcTable.FixCol;
  FFixColCount := vSrcTable.FixColCount;

  for vC := 0 to Self.ColCount - 1 do
    FColWidths[vC] := vSrcTable.FColWidths[vC];

  for vR := 0 to Self.RowCount - 1 do
  begin
    FRows[vR].AutoHeight := vSrcTable.Rows[vR].AutoHeight;
    FRows[vR].Height := vSrcTable.Rows[vR].Height;

    for vC := 0 to FColWidths.Count - 1 do
    begin
      FRows[vR][vC].Width := FColWidths[vC];
      FRows[vR][vC].RowSpan := vSrcTable.Rows[vR][vC].RowSpan;
      FRows[vR][vC].ColSpan := vSrcTable.Rows[vR][vC].ColSpan;
      FRows[vR][vC].BackgroundColor := vSrcTable.Rows[vR][vC].BackgroundColor;
      FRows[vR][vC].AlignVert := vSrcTable.Rows[vR][vC].AlignVert;
      FRows[vR][vC].BorderSides := vSrcTable.Rows[vR][vC].BorderSides;

      if vSrcTable.Rows[vR][vC].CellData <> nil then
        FRows[vR][vC].CellData.AddData(vSrcTable.Rows[vR][vC].CellData)
      else
      begin
        FRows[vR][vC].CellData.Free;
        FRows[vR][vC].CellData := nil;
      end;
    end;
  end;
end;

function THCTableItem.GetActiveData: THCCustomData;
var
  vCell: THCTableCell;
begin
  vCell := GetEditCell;
  if Assigned(vCell) then
    Result := vCell.CellData
  else
    Result := inherited GetActiveData;
end;

function THCTableItem.GetTopLevelDrawItem: THCCustomDrawItem;
var
  vCellData: THCTableCellData;
begin
  vCellData := GetActiveData as THCTableCellData;
  if Assigned(vCellData) then
    Result := vCellData.GetTopLevelDrawItem
  else
    Result := inherited GetTopLevelDrawItem;
end;

function THCTableItem.GetTopLevelDrawItemCoord: TPoint;
var
  vCell: THCTableCell;
  vPt: TPoint;
begin
  Result := Point(0, 0);
  vCell := GetEditCell;
  if Assigned(vCell) then
  begin
    Result := vCell.CellData.GetTopLevelDrawItemCoord;
    vPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);

    Result.X := Result.X + vPt.X + FCellHPaddingPix;
    Result.Y := Result.Y + vPt.Y + vCell.GetCellDataTop(FCellVPaddingPix);
  end;
end;

function THCTableItem.GetActiveItem: THCCustomItem;
var
  vCell: THCTableCell;
begin
  vCell := GetEditCell;
  if Assigned(vCell) then
    Result := vCell.CellData.GetActiveItem
  else
    Result := inherited GetActiveItem;
end;

procedure THCTableItem.GetCaretInfo(var ACaretInfo: THCCaretInfo);
var
  i, vTop, vBottom, vRow, vCol: Integer;
  vPos: TPoint;
  vCaretCell: THCTableCell;
begin
  if OwnerData.Style.UpdateInfo.Draging then  // 拖拽
  begin
    vRow := FMouseMoveRow;
    vCol := FMouseMoveCol;
  end
  else  // 非拖拽
  begin
    vRow := FSelectCellRang.StartRow;
    vCol := FSelectCellRang.StartCol;
  end;

  if vRow < 0 then  // 没在表格上面，在前面或后面
  begin
    if FOutsideInfo.Row >= 0 then  // 前后坐标对应有行
    begin
      if FOutsideInfo.Leftside then  // 在左边
        ACaretInfo.X := ACaretInfo.X - 2;  // 为使光标更明显，向左偏移2

      vTop := 0;
      for i := FPageBreaks.Count - 1 downto 0 do  // 找光标顶部位置
      begin
        if FPageBreaks[i].Row <= FOutsideInfo.Row then  // 当前行前面分页了
        begin
          if FPageBreaks[i].PageIndex = ACaretInfo.PageIndex - 1 then  // 前面分页正好是当前页前一页
          begin
            vTop := FPageBreaks[i].BreakBottom;  // 分页底部位置
            Break;
          end;
        end;
      end;

      vBottom := Self.Height;
      for i := 0 to FPageBreaks.Count - 1 do  // 找光标底部位置
      begin
        if FPageBreaks[i].Row >= FOutsideInfo.Row then  // 当前行后面分页了
        begin
          if FPageBreaks[i].PageIndex = ACaretInfo.PageIndex then  // 分页是当前页
          begin
            vBottom := FPageBreaks[i].BreakSeat;  // 分页顶部位置
            Break;
          end;
        end;
      end;

      ACaretInfo.Y := ACaretInfo.Y + vTop;
      ACaretInfo.Height := vBottom - vTop;
    end
    else
      ACaretInfo.Visible := False;

    Exit;
  end
  else
    vCaretCell := FRows[vRow][vCol];

  if OwnerData.Style.UpdateInfo.Draging then  // 拖拽
  begin
    if (vCaretCell.CellData.MouseMoveItemNo < 0)
      or (vCaretCell.CellData.MouseMoveItemOffset < 0)
    then
    begin
      ACaretInfo.Visible := False;
      Exit;
    end;

    vCaretCell.GetCaretInfo(vCaretCell.CellData.MouseMoveItemNo,
      vCaretCell.CellData.MouseMoveItemOffset, FCellHPaddingPix, FCellVPaddingPix, ACaretInfo);
  end
  else  // 非拖拽
  begin
    if (vCaretCell.CellData.SelectInfo.StartItemNo < 0)
      or (vCaretCell.CellData.SelectInfo.StartItemOffset < 0)
    then
    begin
      ACaretInfo.Visible := False;
      Exit;
    end;

    vCaretCell.GetCaretInfo(vCaretCell.CellData.SelectInfo.StartItemNo,
      vCaretCell.CellData.SelectInfo.StartItemOffset, FCellHPaddingPix, FCellVPaddingPix, ACaretInfo);
  end;

  vPos := GetCellPostion(vRow, vCol);
  ACaretInfo.X := vPos.X + ACaretInfo.X;
  ACaretInfo.Y := vPos.Y + ACaretInfo.Y;
end;

{ TColCross }

constructor TColCross.Create;
begin
  inherited;
  Col := -1;
  DrawItemNo := -1;
  VDrawOffset := 0;
  //HeightInc := 0;
  //MergeSrc := False;
end;

{ THCTableRows }

procedure THCTableRows.Notify(const Value: THCTableRow; Action: TCollectionNotification);
begin
  inherited;
  if Action = TCollectionNotification.cnAdded then
  begin
    if Assigned(FOnRowAdd) then
      FOnRowAdd(Value);
  end;
end;

{ THCMulCellUndo }

procedure THCMulCellUndo.Init(const ARow, ACol: Integer);
begin
  Row := ARow;
  Col := ACol;
end;

procedure THCMulCellUndo.SetEnable(const Value: Boolean);
begin
  if FEnable <> Value then
  begin
    FEnable := Value;
    if FEnable then
      Init(-1, -1);
  end;
end;

end.
