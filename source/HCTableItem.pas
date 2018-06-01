{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  Classes, SysUtils, Types, Graphics, Controls, Generics.Collections, HCDrawItem, HCRectItem,
  HCTableRow, HCCustomData, HCCustomRichData, HCTableCell, HCTableCellData, HCTextStyle, HCCommon,
  HCParaStyle, HCStyleMatch, HCItem, HCStyle, HCDataCommon;

type
  TSelectCellRang = class
  private
    FStartRow,  // 选中起始行
    FStartCol,  // 选中起始列
    FEndRow,    // 选中结束行
    FEndCol     // 选中结束列
      : Integer;
  public
    constructor Create;

    /// <summary> 初始化字段和变量 </summary>
    procedure Initialize;

    /// <summary> 在同一单元中编辑 </summary>
    function EditCell: Boolean;

    /// <summary> 选中在同一行 </summary>
    function SameRow: Boolean;

    /// <summary> 选中在同一列 </summary>
    function SameCol: Boolean;

    /// <summary> 选中1-n个单元格 </summary>
    function SelectExists: Boolean;
    property StartRow: Integer read FStartRow write FStartRow;
    property StartCol: Integer read FStartCol write FStartCol;
    property EndRow: Integer read FEndRow write FEndRow;
    property EndCol: Integer read FEndCol write FEndCol;
  end;

  TTableSite = (
    tsOutside,  // 表格外面
    tsCell,  // 单元格中
    tsBorderLeft,{只有第一列使用此元素}
    tsBorderTop,  {只有第一行使用此元素}
    tsBorderRight,  // 第X列右边
    tsBorderBottom  // 第X行下边
  );

  TResizeInfo = record
    TableSite: TTableSite;
    DestX, DestY: Integer;
  end;

  TRowAddEvent = procedure(const ARow: TTableRow) of object;

  TTableRows = Class(TObjectList<TTableRow>)
  private
    FOnRowAdd: TRowAddEvent;
  protected
    procedure Notify(const Value: TTableRow; Action: TCollectionNotification); override;
  public
    property OnRowAdd: TRowAddEvent read FOnRowAdd write FOnRowAdd;
  end;

  THCTableItem = class(THCResizeRectItem)
  private
    FBorderWidth  // 边框宽度
      : Integer;
    FCellHPadding,  // 单元格内容水平偏移
    FCellVPadding   // 单元格内容垂直偏移(不能大于最低的DrawItem高度，否则会影响跨页)
      : Byte;  // 单元格数据和单元格边框的距离

    FMouseDownRow, FMouseDownCol,
    FMouseMoveRow, FMouseMoveCol,
    FMouseDownX, FMouseDownY: Integer;

    FResizeInfo: TResizeInfo;
    FBorderVisible, FMouseLBDowning, FSelecting, FDraging, FOutSelectInto: Boolean;
    { 选中信息(只有选中起始和结束行都>=0才说明有选中多个单元格
     在单个单元格中选择时结束行、列信息为-1 }
    FSelectCellRang: TSelectCellRang;
    FBorderColor: TColor;  // 边框颜色
    FRows: TTableRows;  // 行
    FColWidths: TList<Integer>;  // 记录各列宽度(除边框、含FCellHPadding * 2)，方便有合并的单元格获取自己水平开始处的位置
    FOwnerData: THCCustomData;
    procedure InitializeMouseInfo;

    /// <summary> 表格行有添加时 </summary>
    procedure DoRowAdd(const ARow: TTableRow);

    /// <summary> 获取当前表格格式化高度 </summary>
    /// <returns></returns>
    function GetFormatHeight: Integer;

    /// <summary> 返回指定单元格相对表格的起始位置坐标(如果被合并返回合并到单元格的坐标) </summary>
    /// <param name="ARow"></param>
    /// <param name="ACol"></param>
    /// <returns></returns>
    function GetCellPostion(const ARow, ACol: Integer): TPoint;

    function ActiveDataResizing: Boolean;

    /// <summary> 取消选中范围内除ARow, ACol之外单元格的选中状态(-1表示全部取消) </summary>
    procedure DisSelectSelectedCell(const ARow: Integer = -1; const ACol: Integer = -1);
  protected
    function CanDrag: Boolean; override;
    function GetSelectComplate: Boolean; override;
    procedure SelectComplate; override;
    function GetResizing: Boolean; override;
    procedure SetResizing(const Value: Boolean); override;

    /// <summary>
    /// APageDataScreenBottom <= APageDataDrawBottom
    /// </summary>
    /// <param name="ACanvas"></param>
    /// <param name="ADrawRect"></param>
    /// <param name="APageDataDrawBottom">当前页去掉页眉页脚，剩下呈现数据的区域绘制时的底部位置(不管分页收敛)</param>
    /// <param name="APageDataScreenTop"></param>
    /// <param name="APageDataScreenBottom">当前页去掉页眉页脚，剩下呈现数据的区域绘制时在屏显底部位置</param>
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    //procedure PaintPartTo(const ACanvas: TCanvas; const ADrawLeft, ADrawTop, ADrawBottom, ADataScreenTop,
    //  ADataScreenBottom, AStartRow, AStartRowDataOffs, AEndRow, AEndRowDataOffs: Integer); overload;
    {procedure ConvertToDrawItems(const AItemNo, AOffs, AContentWidth,
      AContentHeight: Integer; var APos: TPoint; var APageIndex, ALastDNo: Integer);}
    // 继承THCCustomItem抽象方法
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure KillFocus; override;
    // 继承TCustomRectItem抽象方法
    function ApplySelectTextStyle(const AStyle: THCStyle;
      const AMatchStyle: TStyleMatch): Integer; override;
    procedure ApplySelectParaStyle(const AStyle: THCStyle;
      const AMatchStyle: TParaMatch); override;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    /// <summary> 正在其上时内部是否处理指定的Key和Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;

    /// <summary> 清除并返回为处理分页行和行中间有向下偏移后，比净高增加的高度(为重新格式化时后面计算偏移用) </summary>
    function ClearFormatExtraHeight: Integer; override;
    function DeleteSelected: Boolean; override;
    procedure DisSelect; override;
    procedure MarkStyleUsed(const AMark: Boolean); override;
    procedure GetCaretInfo(var ACaretInfo: TCaretInfo); override;
    procedure SetActive(const Value: Boolean); override;

    /// <summary> 获取表格在指定高度内的结束位置处行中最下端(暂时没用到注释了) </summary>
    /// <param name="AHeight">指定的高度范围</param>
    /// <param name="ADItemMostBottom">最后一行最底端DItem的底部位置</param>
    //procedure GetPageFmtBottomInfo(const AHeight: Integer; var ADItemMostBottom: Integer); override;

    function CoordInSelect(const X, Y: Integer): Boolean; override;
    function GetTopLevelDataAt(const X, Y: Integer): THCCustomData; override;
    function GetActiveData: THCCustomData; override;
    function GetActiveItem: THCCustomItem; override;
    function GetActiveDrawItem: THCCustomDrawItem; override;
    function GetActiveDrawItemCoord: TPoint; override;
    function GetHint: string; override;

    function InsertText(const AText: string): Boolean; override;
    function InsertItem(const AItem: THCCustomItem): Boolean; override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function SelectExists: Boolean; override;
    procedure TraverseItem(const ATraverse: TItemTraverse); override;
    //

    /// <summary> 表格分页 </summary>
    /// <param name="ADrawItemRectTop">表格对应的DrawItem的Rect.Top</param>
    /// <param name="ADrawItemRectTop">表格对应的DrawItem的Rect.Bottom</param>
    /// <param name="APageDataFmtTop">当前页的数据顶部位置</param>
    /// <param name="APageDataFmtBottom">当前页的数据底部位置</param>
    /// <param name="ACheckRow">当前页从哪行开始排版</param>
    /// <param name="ABreakRow">当前页最后分页于哪行</param>
    /// <param name="AFmtOffset">表格对应的DrawItem向下整体偏移的量</param>
    /// <param name="ACellMaxInc">返回当前页各列为了避开分页位置额外偏移的最大高度(参数原名AFmtHeightInc为便于分析重命名)</param>
    procedure CheckFormatPage(const ADrawItemRectTop, ADrawItemRectBottom,
      APageDataFmtTop, APageDataFmtBottom, AStartRowNo: Integer;
      var ABreakRow, AFmtOffset, ACellMaxInc: Integer); override;
    // 保存和读取
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure SaveSelectToStream(const AStream: TStream); override;  // inherited TCustomRect
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;

    function GetRowCount: Integer;
    //function GetColCount: Integer;
    function MergeCells(const AStartRow, AStartCol, AEndRow, AEndCol: Integer):Boolean;
    function GetCells(ARow, ACol: Integer): THCTableCell;
  public
    //DrawItem: TCustomDrawItem;
    constructor Create(const AOwnerData: TCustomData; const ARowCount, AColCount,
      AWidth: Integer);
    destructor Destroy; override;

    /// <summary> 获取指定位置处的行、列(如果是被合并单元格则返回目标单元格行、列) </summary>
    /// <param name="X">横坐标</param>
    /// <param name="Y">纵坐标</param>
    /// <param name="ARow">坐标处的行</param>
    /// <param name="ACol">坐标处的列</param>
    ///  <param name="AReDest">如果坐标是合并源，返回目标</param>
    /// <returns></returns>
    function GetCellAt(const X, Y : Integer; var ARow, ACol: Integer;
      const AReDest: Boolean = True): TResizeInfo;

    procedure GetDestCell(const ARow, ACol: Cardinal; var ADestRow, ADestCol: Integer);

    procedure SelectAll;
    /// <summary> 判断指定范围内的单元格是否可以合并(为了给界面合并菜单控制可用状态放到public域中) </summary>
    /// <param name="AStartRow"></param>
    /// <param name="AStartCol"></param>
    /// <param name="AEndRow"></param>
    /// <param name="AEndCol"></param>
    /// <returns></returns>
    function CellsCanMerge(const AStartRow, AStartCol, AEndRow, AEndCol: Integer): Boolean;

    /// <summary> 获取指定单元格合并后的单元格 </summary>
    procedure GetMergeDest(const ARow, ACol: Integer; var ADestRow, ADestCol: Integer);

    /// <summary> 获取指定单元格合并后单元格的Data </summary>
    //function GetMergeDestCellData(const ARow, ACol: Integer): THCTableCellData;

    /// <summary> 单元格是否处于全选中状态(非内容全选中) </summary>
    /// <param name="ARow"></param>
    /// <param name="ACol"></param>
    /// <returns>true:当前是选中状态</returns>
    function CellSelectComplate(const ARow, ACol: Integer): Boolean;
    function MergeSelectCells: Boolean;
    function GetEditCell: THCTableCell; overload;
    procedure GetEditCell(var ARow, ACol: Integer); overload;

    function InsertRowAfter(const ACount: Byte): Boolean;
    function InsertRowBefor(const ACount: Byte): Boolean;
    function DeleteRow(const ACount: Byte): Boolean;
    function InsertColAfter(const ACount: Byte): Boolean;
    function InsertColBefor(const ACount: Byte): Boolean;
    function DeleteCol(const ACount: Byte): Boolean;

    property Cells[ARow, ACol: Integer]: THCTableCell read GetCells;
    property RowCount: Integer read GetRowCount;
    //property ColCount: Integer read GetColCount;
    property SelectCellRang: TSelectCellRang read FSelectCellRang;
    property BorderVisible: Boolean read FBorderVisible write FBorderVisible;
    property CellHPadding: Byte read FCellHPadding write FCellHPadding;
    property CellVPadding: Byte read FCellVPadding write FCellVPadding;
  end;

implementation

uses
  Math, Windows;

type
  TCellCross = class(TObject)
  public
    Col, DItemNo, VOffset: Integer;
    MergeSrc: Boolean;
    constructor Create;
  end;

{$I HCView.inc}

{ THCTableItem }

procedure THCTableItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);

  {$REGION 'UpdateCellSize 获取行中最高单元格高度，并设置为行中其他单元格的高度'}
  procedure UpdateCellSize(const ARowID: Integer);
  var
    vC, vNorHeightMax: Integer;
  begin
    vNorHeightMax := 0;  // 行中未发生合并的最高单元格
    // 得到行中未发生合并内容最高的单元格高度
    for vC := 0 to FRows[ARowID].ColCount - 1 do
    begin
      if (FRows[ARowID].Cols[vC].CellData <> nil)  // 不是被合并的单元格
        and (FRows[ARowID].Cols[vC].RowSpan = 0)  // 不是行合并的行单元格
      then
        vNorHeightMax := Max(vNorHeightMax, FRows[ARowID].Cols[vC].CellData.Height + FCellVPadding * 2);
    end;

    for vC := 0 to FRows[ARowID].ColCount - 1 do
      FRows[ARowID].Cols[vC].Height := vNorHeightMax;

    if FRows[ARowID].AutoHeight then  // 以行中各未发生行合并的列中最高的为行高
      FRows[ARowID].Height := vNorHeightMax
    else  // 拖动改变了高度
    begin
      if vNorHeightMax > FRows[ARowID].Height then  // 拖动高度失效
      begin
        FRows[ARowID].AutoHeight := True;
        FRows[ARowID].Height := vNorHeightMax;
      end;
      //FRows[ARowID].Height := Max(FRows[ARowID].Height, vNorHeightMax);  // 记录行高，其实可以用行中第一个没有合并的列高度
    end;
  end;
  {$ENDREGION}

  {$REGION 'ConvertRow 格式化指定行(各列)'}
  procedure ConvertRow(const ARow: Cardinal);
  var
    vC, vWidth, i: Integer;
    vRow: TTableRow;
  begin
    vRow := FRows[ARow];
    vRow.FmtOffset := 0;  // 恢复上次格式化可能的偏移
    // 格式化各单元格中的Data
    for vC := 0 to vRow.ColCount - 1 do
    begin
      if vRow.Cols[vC].CellData <> nil then
      begin
        vWidth := FColWidths[vC];
        for i := 1 to vRow.Cols[vC].ColSpan do
          vWidth := vWidth + FBorderWidth + FColWidths[vC + i];
        vRow.Cols[vC].Width := vWidth;
        vRow.Cols[vC].CellData.Width := vWidth - 2 * FCellHPadding;
        vRow.Cols[vC].CellData.ReFormat(0);
      end
    end;
  end;
  {$ENDREGION}

var
  i, vR, vC,
  vMergeDestRow, vMergeDestCol,
  vMergeDestRow2, vMergeDestCol2,
  vExtraHeight: Integer;
begin
  for vR := 0 to RowCount - 1 do  // 格式化各行
  begin
    ConvertRow(vR);  // 格式化行，并计算行高度
    UpdateCellSize(vR);  // 以行中所有无行合并操作列中最大高度更新其他列
  end;

  for vR := 0 to RowCount - 1 do  // 计算有行合并情况下各行的高度
  begin
    for vC := 0 to FRows[vR].ColCount - 1 do
    begin
      if Cells[vR, vC].CellData = nil then  // 当前单元格被合并了
      begin
        if Cells[vR, vC].ColSpan < 0 then  // 合并目标只需由正下方的单元格处理合并内容，不用重复处理
          Continue;

        GetMergeDest(vR, vC, vMergeDestRow, vMergeDestCol);  // 获取到合并目标单元格所在行号

        if vMergeDestRow + Cells[vMergeDestRow, vC].RowSpan = vR then  // 目标单元格行合并到此结束
        begin
          vExtraHeight := Cells[vMergeDestRow, vC].CellData.Height;  // 目标单元格数据高度
          Cells[vMergeDestRow, vC].Height := vExtraHeight;  // 目标单元格高度
          for i := vMergeDestRow to vR - 1 do  // 从目标到此，经过各行后“消减”掉多少
            vExtraHeight := vExtraHeight - FRows[i].Height - FBorderWidth;

          if vExtraHeight > FRows[vR].Height then  // 消减后剩余的比当前行高
          begin
            for i := 0 to vC - 1{FRows[vR].ColCount - 1} do  // 当前行当前列之前的需要重新调整单元格高度，后面的由之后的循环处理
            begin
              if FRows[vR].Cols[i].CellData <> nil then  // 没有被合并
                FRows[vR].Cols[i].Height := vExtraHeight  // 可理解为普通单元格赋值为行高(后面会将行高赋值为vExtraHeight)
              else  // 被合并的源单元格
              begin
                GetMergeDest(vR, i, vMergeDestRow2, vMergeDestCol2);  // 获取目标单元格
                if vMergeDestRow2 + Cells[vMergeDestRow2, i].RowSpan = vR then  // 目标单元格合并到此行结束，处理增量
                  Cells[vMergeDestRow2, i].Height := Cells[vMergeDestRow2, i].Height + vExtraHeight - FRows[vR].Height;
              end;
            end;
            FRows[vR].Height := vExtraHeight;  // 当前行高赋值新值
          end
          else  // 消减后剩余的没有当前行高，高度增加到当前行底部，处理非合并的单元格内容，大于合并结束到此行但数据底部没有此行高的情况
            Cells[vMergeDestRow, vC].Height :=  // 2017-1-15_1.bmp中[1, 1]输入c时[1, 0]和[1, 2]的情况
              Cells[vMergeDestRow, vC].Height + FRows[vR].Height - vExtraHeight;
        end;
      end;
    end;
  end;

  // 计算整体高度
  Height := GetFormatHeight;
  i := FBorderWidth;
  for vC := 0 to FColWidths.Count - 1 do
    i := i + FColWidths[vC] + FBorderWidth;
  Width := i;
end;

constructor THCTableItem.Create(const AOwnerData: TCustomData;
  const ARowCount, AColCount, AWidth: Integer);
var
  vRow: TTableRow;
  i, vDataWidth: Integer;
begin
  inherited Create(AOwnerData);
  FOwnerData := AOwnerData;
  GripSize := 2;
  FCellHPadding := 2;
  FCellVPadding := 0;
  FDraging := False;
  //
  StyleNo := THCStyle.RsTable;
  ParaNo := FOwnerData.Style.CurParaNo;
  if ARowCount = 0 then
    raise Exception.Create('异常：不能创建行数为0的表格！');
  if AColCount = 0 then
    raise Exception.Create('异常：不能创建列数为0的表格！');
  FBorderWidth := 1;
  FBorderColor := clBlack;
  FBorderVisible := True;
  //FWidth := FRows[0].ColCount * (MinColWidth + FBorderWidth) + FBorderWidth;
  Height := ARowCount * (MinRowHeight + FBorderWidth) + FBorderWidth;
  FRows := TTableRows.Create;
  FRows.OnRowAdd := DoRowAdd;
  FSelectCellRang := TSelectCellRang.Create;
  Self.InitializeMouseInfo;
  //
  vDataWidth := AWidth - (AColCount + 1) * FBorderWidth;
  for i := 0 to ARowCount - 1 do
  begin
    vRow := TTableRow.Create(FOwnerData.Style, AColCount);
    vRow.SetRowWidth(vDataWidth);
    FRows.Add(vRow);
  end;
  FColWidths := TList<Integer>.Create;
  for i := 0 to AColCount - 1 do
    FColWidths.Add(FRows[0].Cols[i].Width);
end;

function THCTableItem.DeleteCol(const ACount: Byte): Boolean;
var
  i, j, vDelCount: Integer;
  //viDestRow, viDestCol: Integer;
  //vTableRow: TTableRow;
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;
  if FSelectCellRang.StartCol + ACount > FColWidths.Count - 1 then
    vDelCount := FColWidths.Count - FSelectCellRang.StartCol
  else
    vDelCount := ACount;

  for i := 0 to vDelCount - 1 do
  begin
    for j := 0 to RowCount - 1 do
    begin
      { TODO : 合并单元格的处理 }
      FRows[j].Delete(FSelectCellRang.StartCol);
    end;
    FColWidths.Delete(FSelectCellRang.StartCol);
  end;
  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Result := True;
end;

function THCTableItem.DeleteRow(const ACount: Byte): Boolean;
var
  i, j, k, vDelCount: Integer;
  viDestRow, viDestCol: Integer;
  //vTableRow: TTableRow;
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;
  if FSelectCellRang.StartRow + ACount > RowCount - 1 then
    vDelCount := RowCount - FSelectCellRang.StartRow
  else
    vDelCount := ACount;

  for i := 0 to vDelCount - 1 do
  begin
    for j := FColWidths.Count - 1 downto 0 do
    begin
      if FRows[FSelectCellRang.StartRow].Cols[j].RowSpan <> 0 then  // 删除的单元格是合并源
      begin
        GetDestCell(FSelectCellRang.StartRow, j, viDestRow, viDestCol);

        for k := 0 to Cells[viDestRow, viDestCol].RowSpan + FRows[FSelectCellRang.StartRow].Cols[j].RowSpan do  // 目标的行跨度 - 已经跨的
          FRows[FSelectCellRang.StartRow + k].Cols[j].RowSpan := FRows[FSelectCellRang.StartRow + k].Cols[j].RowSpan - 1;  // 离目标行远1
        FRows[viDestRow].Cols[j].RowSpan := FRows[viDestRow].Cols[j].RowSpan + 1;  // 目标行包含的合并源增加1    }
      end;
    end;
  end;
  FRows.DeleteRange(FSelectCellRang.StartRow, vDelCount);

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Result := True;
end;

function THCTableItem.DeleteSelected: Boolean;
var
  vR, vC: Integer;
begin
  Result := inherited DeleteSelected;

  if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
  begin
    if FSelectCellRang.EndRow >= 0 then  // 有选择结束行，说明选中不在同一单元格
    begin
      Result := True;
      for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
      begin
        for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
        begin
          if Cells[vR, vC].CellData <> nil then
            Cells[vR, vC].CellData.DeleteSelected;
        end;
      end;
    end
    else  // 在同一单元格
      Result := GetEditCell.CellData.DeleteSelected;
  end;
end;

destructor THCTableItem.Destroy;
begin
  FSelectCellRang.Free;
  FRows.Clear;
  FRows.Free;
  FColWidths.Free;
  inherited;
end;

procedure THCTableItem.DisSelect;
begin
  inherited DisSelect;

  DisSelectSelectedCell;

  FSelectCellRang.Initialize;
  Self.InitializeMouseInfo;

  FSelecting := False;
  FDraging := False;
  FOutSelectInto := False;
end;

procedure THCTableItem.DisSelectSelectedCell(const ARow: Integer = -1;
  const ACol: Integer = -1);
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
      vCellData := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData;
      vCellData.DisSelect;
      vCellData.InitializeField;
    end;

    for vRow := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
    begin
      for vCol := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
      begin
        if (vRow = ARow) and (vCol = ACol) then

        else
        begin
          vCellData := FRows[vRow].Cols[vCol].CellData;
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

procedure THCTableItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vR, vC,
  vCellScreenTop,
  vCellScreenBottom,
  vCellDataDrawTop,
  vCellDrawLeft,
  vCellDataDrawBottom,
  vFristDItemNo,
  vLastDItemNo,
  vBorderLeft,
  vBorderTop,
  vBorderRight,
  vBorderBottom,
  vShouLian, vBreakBottom,
  vMergeDestRow, vMergeDestCol, vMergeDestRow2, vMergeDestCol2, vSrcRowBorderTop,
  vMergeCellDataDrawTop, vFirstDrawRow,
  vPageDataScreenBottom  // APageDataScreenBottom减去整行跨页到下一页时，当前页底部的空余
    : Integer;
  //vDrawBord: Boolean;  // 当某一行在当前页能显示出来，但该行没有一列有DItem能绘制，此时(1)处仍能通过，但却不需要绘制边框
  vSelectAll, vDrawBorder,
  vDrawCellData,
  vRowHasDItemDisplay
    : Boolean;
  vCellData: THCTableCellData;

  {$REGION 'CheckRowBorderShouLian'}
  procedure CheckRowBorderShouLian(const ARow: Integer);
  var
    vC, i: Integer;
    vRect: TRect;
  begin
    if vShouLian = 0 then  // 没有计算过当前行有跨页的所有列中最佳跨页时收敛位置
    begin
      vBreakBottom := 0;
      for vC := 0 to FRows[ARow].ColCount - 1 do  // 遍历同行各列，获取截断位置(因为各行在CheckFormatPage已经算好分页位置，所以此处只要一个单元格跨页位置同时适用当前行所有单元格跨页位置)
      begin
        vMergeCellDataDrawTop := vCellDataDrawTop;
        GetMergeDest(ARow, vC, vMergeDestRow2, vMergeDestCol2);  // 获取到目标单元格所在行号
        vCellData := FRows[vMergeDestRow2].Cols[vMergeDestCol2].CellData;
        while vMergeDestRow2 < ARow do  // 目标单元格的CellDataDrawTop
        begin
          vMergeCellDataDrawTop := vMergeCellDataDrawTop - FRows[vMergeDestRow2].Height - FBorderWidth;
          Inc(vMergeDestRow2);
        end;

        for i := 0 to vCellData.DrawItems.Count - 1 do
        begin
          if vCellData.DrawItems[i].LineFirst then
          begin
            vRect := vCellData.DrawItems[i].Rect;
            //if DrawiInLastLine(i) then  // 单元格内最后一行内容补充FCellVPadding
            vRect.Bottom := vRect.Bottom + FCellVPadding; // 每一行可能是要截断的，截断时下面要能放下FCellVPadding
            if vMergeCellDataDrawTop + vRect.Bottom > ADataDrawBottom then  // 超过当前页了
            begin
              if i > 0 then
                vShouLian := Max(vShouLian, vMergeCellDataDrawTop + vCellData.DrawItems[i - 1].Rect.Bottom)  // 上一个最下面做为截断位置
              else
                vShouLian := Max(vShouLian, vMergeCellDataDrawTop - FBorderWidth);

              Break;
            end
            else  // 没有超过当前页
              vBreakBottom := Max(vBreakBottom, vMergeCellDataDrawTop + vRect.Bottom);  // 记录为可放下的最后一个下面(有的单元格在当前页能全部显示，并不跨页)
          end;
        end;
      end;
      vShouLian := Max(vShouLian, vBreakBottom);
    end;
  end;
  {$ENDREGION}

begin
  ACanvas.Pen.Width := FBorderWidth;
  // 单元格
  vFirstDrawRow := -1;
  vCellDataDrawTop := ADrawRect.Top;  // 第1行数据绘制起始位置
  for vR := 0 to FRows.Count - 1 do
  begin
    // 不在当前屏幕范围内的不绘制(1)
    vCellDataDrawTop := vCellDataDrawTop + FRows[vR].FmtOffset + FBorderWidth + FCellVPadding;
    if vCellDataDrawTop > ADataScreenBottom then  // 行数据顶部大于可显示区域底部不绘制
      Break;
    vCellDataDrawBottom := vCellDataDrawTop + FRows[vR].Height - FCellVPadding;

    if vCellDataDrawBottom < ADataScreenTop then  // 当前行底部小于可显示顶部，没显示出来
    begin
      vCellDataDrawTop := vCellDataDrawBottom;  // 准备判断下一行是否是可显示第一行
      Continue;
    end;
    if vFirstDrawRow < 0 then
      vFirstDrawRow := vR;

    vCellDrawLeft := ADrawRect.Left + FBorderWidth;

    // 循环绘制行中各单元格数据和边框
    vShouLian := 0;
    vRowHasDItemDisplay := False;
    for vC := 0 to FRows[vR].ColCount - 1 do
    begin
      if FRows[vR].Cols[vC].ColSpan < 0 then  // 合并列源
      begin
        vCellDrawLeft := vCellDrawLeft + FColWidths[vC] + FBorderWidth;
        Continue;  // 普通单元格或合并目标单元格才有数据，否则由目标单元格处理
      end;

      vDrawCellData := True;  // 处理目标行有跨页，且目标行后面有多行合并到此行时，只在跨页后绘制一次目标行的数据
      if FRows[vR].Cols[vC].RowSpan < 0 then  // 20170208001 是合并行源单元格(由于上面排除了列源，所以这里只是目标单元格正下方的单元格)
      begin
        if vR <> vFirstDrawRow then  // 不是跨页后第一次绘制的行
          vDrawCellData := False;  // 目标单元格已经这页绘制了数据，不用重复绘制了，否则跨行后的第一次要绘制
      end;

      vFristDItemNo := -1;
      vLastDItemNo := -1;
      vMergeCellDataDrawTop := vCellDataDrawTop;
      GetMergeDest(vR, vC, vMergeDestRow, vMergeDestCol);  // 获取到目标单元格所在行号
      vMergeDestRow2 := vMergeDestRow;
      while vMergeDestRow2 < vR do  // 得到目标单元格CellDataDrawTop的值
      begin
        vMergeCellDataDrawTop := vMergeCellDataDrawTop - FRows[vMergeDestRow2].Height - FBorderWidth;
        Inc(vMergeDestRow2);
      end;

      vPageDataScreenBottom := ADataScreenBottom;
      if vDrawCellData then
      begin
        if (vR < FRows.Count - 1) and (FRows[vR + 1].FmtOffset > 0) then
          vPageDataScreenBottom := vPageDataScreenBottom - FRows[vR + 1].FmtOffset;
        vCellScreenBottom := Math.Min(vPageDataScreenBottom,  // 数据内容屏显最下端
          vCellDataDrawTop
          + Max(FRows[vR].Height, FRows[vMergeDestRow].Cols[vMergeDestCol].Height) - FCellVPadding  // 行高和有合并的单元格高中最大的
          );

        //Assert(vCellScreenBottom - vMergeCellDataDrawTop >= FRows[vR].Height, '计划使用Continue但待确认会符合情况的');
        vCellData := FRows[vMergeDestRow].Cols[vMergeDestCol].CellData;  // 目标CellData，20170208003 如果移到if vDrawData外面则20170208002不需要了
        vCellScreenTop := Math.Max(ADataScreenTop, vCellDataDrawTop - FCellVPadding);  // 屏显最上端
        if vCellScreenTop - vMergeCellDataDrawTop < vCellData.Height then  // 可显示的起始位置小于数据高度(当>=时说明数据高度小于行高时，数据已经完全卷到上面了)
        begin
          vCellData.GetDataDrawItemRang(  // 获取可显示区域的起始、结束DItem
            vCellScreenTop - vMergeCellDataDrawTop,
            vCellScreenBottom - vMergeCellDataDrawTop,
            vFristDItemNo, vLastDItemNo);

          vSelectAll := Self.IsSelectComplate or CellSelectComplate(vR, vC);  // 表格全选中或单元格全选中
          if vSelectAll then  // 是全选中
          begin
            ACanvas.Brush.Color := FOwnerData.Style.SelColor;
            vCellData.DrawOptions := vCellData.DrawOptions - [doFontBackColor];
          end
          else
          begin
            ACanvas.Brush.Color := FRows[vMergeDestRow].Cols[vMergeDestCol].BackgroundColor;
            vCellData.DrawOptions := vCellData.DrawOptions + [doFontBackColor];
          end;

          ACanvas.FillRect(Rect(vCellDrawLeft, vCellScreenTop,  // + FRows[vR].Height,
            vCellDrawLeft + FRows[vR].Cols[vC].Width, vCellScreenBottom));

          if vFristDItemNo >= 0 then  // 有可显示的DrawItem
          begin
            {$IFDEF SHOWITEMNO}
            ACanvas.Font.Color := clGray;
            ACanvas.Font.Style := [];
            ACanvas.Font.Size := 8;
            ACanvas.TextOut(vCellDrawLeft + 1, vMergeCellDataDrawTop, IntToStr(vC) + '/' + IntToStr(vR));
            {$ENDIF}

            vCellData.PaintData(vCellDrawLeft + FCellHPadding, vMergeCellDataDrawTop,
              ADataDrawBottom, ADataScreenTop, vPageDataScreenBottom,
              0, ACanvas, APaintInfo);
          end;
        end;
      end;
      // 绘制各单元格边框线
      if FBorderVisible or (not APaintInfo.Print) then
      begin
        vDrawBorder := True;
        vBorderTop := vMergeCellDataDrawTop - FCellVPadding - FBorderWidth;  // 目标单元格的上边框
        vBorderBottom := vBorderTop  // 计算边框最下端
          + Max(FRows[vR].Height, Cells[vMergeDestRow, vMergeDestCol].Height)  // 由于可能是合并目标单元格，所以用单元格高和行高最高的
          + FBorderWidth;

        { 如有跨页，计算收敛位置 }
        if vBorderBottom > vPageDataScreenBottom then  // 底部边框>页数据屏显底部，需要判断合并单元格的情况
        begin
          if Cells[vR, vC].RowSpan > 0 then  // 是合并目标单元格
          begin
            if vFristDItemNo < 0 then  // 没有数据被绘制，即合并目标单元格数据整体移动到下一页了
              //vDrawBorder := False
              vBorderBottom := vBorderTop + FRows[vR].Height + FBorderWidth  // 以当前行结尾
            else
            if vBorderTop + FRows[vR].Height > vPageDataScreenBottom then  // 目标单元格所在的行就需要跨页了
            begin  // 从当前行找收敛
              CheckRowBorderShouLian(vR);
              vBorderBottom := vShouLian;  //为什么是2 Min(vBorderBottom, vShouLian);  // ADataDrawBottom
            end
            else  // 虽然目标单元格跨页了，但其收敛位置并不在目标单元格所在行，则由其收敛行所在的源单元格处理(如 2017-2-8_001.bmp)
              vDrawBorder := False;
          end
          else
          if Cells[vR, vC].RowSpan < 0 then  // 合并源单元格，
          begin                              // 由于开始处的20170208001判断，所以此处肯定是合并目标正下方的单元格
            { 移动到当前行起始位置 }
            vSrcRowBorderTop := vBorderTop;  // 借用变量，vBorderTop值是目标单元格的上边框
            for vMergeDestRow2 := vMergeDestRow to vR - 1 do
              vSrcRowBorderTop := vSrcRowBorderTop + FRows[vMergeDestRow2].Height + FBorderWidth;

            if vSrcRowBorderTop + FRows[vR].Height > vPageDataScreenBottom then  // 此合并源单元格所在的行跨页了
            begin  // 从当前行找收敛
              if vFristDItemNo < 0 then  // 因为数据是由目标单元格负责绘制，所以需要判断本合并源单元格所在行位置能否有数据显示出来
              begin
                vCellData := FRows[vMergeDestRow].Cols[vMergeDestCol].CellData;  // 目标CellData 20170208002 如果20170208003 如果移到if vDrawData外面则不需要了
                vCellData.GetDataDrawItemRang(  // 获取可显示区域的起始、结束DItem
                  vCellScreenTop - vMergeCellDataDrawTop,
                  vCellScreenBottom - vMergeCellDataDrawTop,
                  vFristDItemNo, vLastDItemNo);
              end;
              if vFristDItemNo < 0 then  // 目标单元格没有数据可以当前行显示出来
              begin
                if vShouLian = 0 then  // 说明目标单元格数据并没有超出当前行
                  vShouLian := vSrcRowBorderTop;  // 在当前行上边收敛（当前行普通单元格都移动到下一页开始了）
              end
              else  // 有可以显示的数据，则获取收敛位置
                CheckRowBorderShouLian(vR);

              vBorderBottom := vShouLian;  //为什么是2 Min(vBorderBottom, vShouLian);  // ADataDrawBottom
              // 需要绘制边框，将边框上位置移回目标位置
//                vMergeDestRow2 := vMergeDestRow;
//                while vMergeDestRow2 < vR do  // 得到目标单元格CellDataDrawTop的值
//                begin
//                  vBorderTop := vBorderTop - FRows[vMergeDestRow2].Height - FBorderWidth;
//                  Inc(vMergeDestRow2);
//                end;
            end
            else  // 虽然目标单元格跨页了，但其收敛位置并不在目标单元格所在行，则收其收敛行所在的源单元格处理(如 2017-2-8_001.bmp)
              vDrawBorder := False;
          end
          else  // 普通单元格(不是合并目标也不是合并源)
          begin
            if (vFristDItemNo < 0) and (vR <> vFirstDrawRow) then
              vDrawBorder := False
            else
            begin
              if FRows[vR].AutoHeight then
                CheckRowBorderShouLian(vR)
              else
                vShouLian := vPageDataScreenBottom;

              vBorderBottom := vShouLian;  //为什么是2 Min(vBorderBottom, vShouLian);  // ADataDrawBottom
            end;
          end;
        end;

        if vDrawBorder then
        begin
          if FBorderVisible then
          begin
            ACanvas.Pen.Color := clBlack;
            ACanvas.Pen.Style := psSolid;
          end
          else
          begin
            ACanvas.Pen.Color := clActiveBorder;
            ACanvas.Pen.Style := psDot;
          end;

          vBorderLeft := vCellDrawLeft - FBorderWidth;
          vBorderRight := vBorderLeft + FColWidths[vC] + FBorderWidth;

          vMergeDestCol2 := FRows[vMergeDestRow].Cols[vMergeDestCol].ColSpan;
          while vMergeDestCol2 > 0 do
          begin
            vBorderRight := vBorderRight + FColWidths[vMergeDestCol + vMergeDestCol2] + FBorderWidth;
            Dec(vMergeDestCol2);
          end;

          if vBorderTop < ADataScreenTop then  // 表格当前行跨页了，表格跨页后，第二页起始行表格上边框显示位置需要纠正
            vBorderTop := ADataScreenTop;

          if vBorderTop > 0 then
          begin
            ACanvas.MoveTo(vBorderLeft, vBorderTop);   // 行左上
            ACanvas.LineTo(vBorderRight, vBorderTop);  // 行右上
          end
          else
            ACanvas.MoveTo(vBorderRight, vBorderTop);  // 行右上

          ACanvas.LineTo(vBorderRight, vBorderBottom);  // 行右下
          ACanvas.LineTo(vBorderLeft, vBorderBottom);  // 行左下
          if vC = 0 then  // 第1列绘制前面竖线，其他的由前一列后面竖线代替前面竖线
            ACanvas.LineTo(vBorderLeft, vBorderTop - 1);
        end;
      end;
      vCellDrawLeft := vCellDrawLeft + FColWidths[vC] + FBorderWidth;  // 同行下一列的起始Left位置
    end;
    vCellDataDrawTop := vCellDataDrawBottom;  // 下一行的Top位置
  end;
  // 拖动线
  if Resizing and (FResizeInfo.TableSite = tsBorderRight) then  // 垂直
  begin
    ACanvas.Pen.Color := Self.FBorderColor;
    ACanvas.Pen.Style := psDot;
    ACanvas.MoveTo(ADrawRect.Left + FResizeInfo.DestX, Max(ADataDrawTop, ADrawRect.Top));
    ACanvas.LineTo(ADrawRect.Left + FResizeInfo.DestX, Min(ADataDrawBottom,
      Min(ADrawRect.Bottom, vBorderBottom)));
  end
  else
  if Resizing and (FResizeInfo.TableSite = tsBorderBottom) then  // 水平
  begin
    ACanvas.Pen.Color := Self.FBorderColor;
    ACanvas.Pen.Style := psDot;
    ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Top + FResizeInfo.DestY);
    ACanvas.LineTo(ADrawRect.Right, ADrawRect.Top + FResizeInfo.DestY);
  end;
end;

procedure THCTableItem.DoRowAdd(const ARow: TTableRow);
var
  i: Integer;
begin
  for i := 0 to ARow.ColCount - 1 do
  begin
    if ARow.Cols[i].CellData <> nil then
    begin
      ARow.Cols[i].CellData.OnInsertItem := (FOwnerData as THCCustomRichData).OnInsertItem;
      ARow.Cols[i].CellData.OnItemPaintAfter := (FOwnerData as THCCustomRichData).OnItemPaintAfter;
      ARow.Cols[i].CellData.OnItemPaintBefor := (FOwnerData as THCCustomRichData).OnItemPaintBefor;

      ARow.Cols[i].CellData.OnCreateItem := (FOwnerData as THCCustomRichData).OnCreateItem;
    end;
  end;
end;

procedure THCTableItem.KeyDown(var Key: Word; Shift: TShiftState);

  function IsDirectionKey(const AKey: Word): Boolean;
  begin
    Result := AKey in [VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN];
  end;

var
  vEditCell: THCTableCell;

  function DoCrossCellKey(const AKey: Word): Boolean;
  var
    i, vRow, vCol: Integer;
  begin
    Result := False;

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
          if Cells[FSelectCellRang.StartRow, i].ColSpan = 0 then
          begin
            vCol := i;
            Break;
          end;
        end;

        if vCol >= 0 then
        begin
          FSelectCellRang.StartCol := vCol;
          with Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData do
          begin
            SelectInfo.StartItemNo := 0;
            SelectInfo.StartItemOffset := 0;
            CaretDrawItemNo := DrawItems.Count - 1;
          end;

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
          if Cells[FSelectCellRang.StartRow, i].ColSpan = 0 then
          begin
            vCol := i;
            Break;
          end;
        end;

        if vCol >= 0 then
        begin
          FSelectCellRang.StartCol := vCol;
          with Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData do
          begin
            SelectInfo.StartItemNo := 0;
            SelectInfo.StartItemOffset := 0;
            CaretDrawItemNo := 0;
          end;

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
          FSelectCellRang.StartRow := vRow;
          FSelectCellRang.StartCol := vCol;
          with Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData do
          begin
            SelectInfo.StartItemNo := Items.Count - 1;
            if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull then
              SelectInfo.StartItemOffset := OffsetAfter
            else
              SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;

            CaretDrawItemNo := DrawItems.Count - 1;
          end;

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
        if (vRow >= 0) and (vCol >= 0) then
        begin
          FSelectCellRang.StartRow := vRow;
          FSelectCellRang.StartCol := vCol;
          with Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData do
          begin
            SelectInfo.StartItemNo := 0;
            SelectInfo.StartItemOffset := 0;
            CaretDrawItemNo := 0;
          end;

          Result := True;
        end;
      end;
    end;
    {$ENDREGION}
  end;

var
  vOldHeight: Integer;
  vOldKey: Word;
begin
  Self.SizeChanged := False;

  vEditCell := GetEditCell;
  if vEditCell <> nil then
  begin
    vOldKey := Key;
    vOldHeight := vEditCell.CellData.Height;
    vEditCell.CellData.KeyDown(Key, Shift);
    Self.SizeChanged := vOldHeight <> vEditCell.CellData.Height;

    if (Key = 0) and IsDirectionKey(vOldKey) then  // 单元格Data没处理，且是方向键
    begin
      if DoCrossCellKey(vOldKey) then
      begin
        FOwnerData.Style.UpdateInfoReCaret;
        Key := vOldKey;
      end;
    end;
  end
  else
    Key := 0;
end;

procedure THCTableItem.KeyPress(var Key: Char);
var
  vOldHeight: Integer;
  vEditCell: THCTableCell;
begin
  Self.SizeChanged := False;

  vEditCell := GetEditCell;
  if vEditCell <> nil then
  begin
    vOldHeight := vEditCell.CellData.Height;
    vEditCell.CellData.KeyPress(Key);
    Self.SizeChanged := vOldHeight <> vEditCell.CellData.Height;
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
  vRow: TTableRow;
begin
  FRows.Clear;
  inherited LoadFromStream(AStream, AStyle, AFileVersion);

  AStream.ReadBuffer(FBorderVisible, SizeOf(FBorderVisible));
  AStream.ReadBuffer(vR, SizeOf(vR));  // 行数
  AStream.ReadBuffer(vC, SizeOf(vC));  // 列数
  { 创建行、列 }
  for i := 0 to vR - 1 do
  begin
    vRow := TTableRow.Create(FOwnerData.Style, vC);  // 注意行创建时是table拥有者的Style，加载时是传入的AStyle
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
      FRows[vR].Cols[vC].CellData.Width := FColWidths[vC] - 2 * FCellHPadding;
      FRows[vR].Cols[vC].LoadFromStream(AStream, AStyle, AFileVersion);
    end;
  end;
end;

procedure THCTableItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  //i: Integer;
  vMouseDownRow, vMouseDownCol: Integer;// abstract vBottom;
  vCell: THCTableCell;
  vCellPt: TPoint;
begin
  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);
  FOutSelectInto := False;
  FSelecting := False;  // 准备划选
  FDraging := False;  // 准备拖拽

  FResizeInfo := GetCellAt(X, Y, vMouseDownRow, vMouseDownCol);

  Resizing := (FResizeInfo.TableSite = tsBorderRight) or (FResizeInfo.TableSite = tsBorderBottom);
  if Resizing then
  begin
    FMouseDownRow := vMouseDownRow;
    FMouseDownCol := vMouseDownCol;
    FMouseDownX := X;
    FMouseDownY := Y;
    FOwnerData.Style.UpdateInfoRePaint;
    Exit;
  end;

  if FResizeInfo.TableSite = tsCell then
  begin
    if CoordInSelect(X, Y) then  // 在选中区域中（不包括边框线及边框线容差）
    begin
      if FMouseLBDowning then
        FDraging := True;

      FMouseDownRow := vMouseDownRow;  // 记录拖拽起始单元格
      FMouseDownCol := vMouseDownCol;

      vCellPt := GetCellPostion(FMouseDownRow, FMouseDownCol);
      FRows[FMouseDownRow].Cols[FMouseDownCol].CellData.MouseDown(
        Button, Shift, X - vCellPt.X - FCellHPadding, Y - vCellPt.Y - FCellVPadding);
    end
    else  // 不在选中区域中
    begin
      {DisSelect;  // 清除原选中

      if (vMouseDownRow <> FMouseDownRow) or (vMouseDownCol <> FMouseDownCol) then  // 新位置
      begin
        vCell := GetEditCell;
        if vCell <> nil then  // 取消原来编辑
          vCell.Active := False;

        FMouseDownRow := vMouseDownRow;
        FMouseDownCol := vMouseDownCol;
        FOwnerData.Style.UpdateInfoReCaret;
      end; }

      // 如果先执行DisSelect会清除Mouse信息，导致当前编辑单元格不能响应取消激活事件
      if (vMouseDownRow <> FMouseDownRow) or (vMouseDownCol <> FMouseDownCol) then  // 新位置
      begin
        vCell := GetEditCell;
        if vCell <> nil then  // 取消原来编辑
          vCell.Active := False;
        FOwnerData.Style.UpdateInfoReCaret;
      end;

      DisSelect;  // 清除原选中

      FMouseDownRow := vMouseDownRow;
      FMouseDownCol := vMouseDownCol;

      FSelectCellRang.StartRow := FMouseDownRow;
      FSelectCellRang.StartCol := FMouseDownCol;

      vCellPt := GetCellPostion(FMouseDownRow, FMouseDownCol);

      FRows[FMouseDownRow].Cols[FMouseDownCol].CellData.MouseDown(
        Button, Shift, X - vCellPt.X - FCellHPadding, Y - vCellPt.Y - FCellVPadding);
    end;
  end
  else
  begin
    DisSelect;  // 取消原来选中
    Self.InitializeMouseInfo;
  end;
end;

procedure THCTableItem.MouseLeave;
begin
  inherited;
  if (FMouseMoveRow < 0) or (FMouseMoveCol < 0) then Exit;
  if FRows[FMouseMoveRow].Cols[FMouseMoveCol].CellData <> nil then
    FRows[FMouseMoveRow].Cols[FMouseMoveCol].CellData.MouseMove([], -1, -1);  // 处理鼠标移上高亮在迅速移出表格后不能恢复的问题

  if not SelectExists then
    Self.InitializeMouseInfo;
end;

procedure THCTableItem.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vMoveRow, vMoveCol: Integer;

  {$REGION 'AdjustSelectRang'}
  procedure AdjustSelectRang;
  var
    vRow, vCol: Integer;
  begin
    // 先清除起始单元格之外的，以便下面重新处理选中单元格的全选
    if FSelectCellRang.StartRow >= 0 then
    begin
      for vRow := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
      begin
        for vCol := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
        begin
          if ((vRow = FMouseDownRow) and (vCol = FMouseDownCol))
            //or ((vRow = vMoveRow) and (vCol = vMoveCol))
          then  // 保留当前按下的选中信息，防止回到按下中做内容的选中

          else
          begin
            if Cells[vRow, vCol].CellData <> nil then
              Cells[vRow, vCol].CellData.DisSelect;
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

        FSelectCellRang.StartRow := FMouseDownRow;
        FSelectCellRang.StartCol := FMouseDownCol;
        FSelectCellRang.EndRow := vMoveRow;
        FSelectCellRang.EndCol := vMoveCol;
      end
      else  // 从下面选入
      begin
        GetDestCell(Self.RowCount - 1, Self.FColWidths.Count - 1, vRow, vCol);
        FMouseDownRow := vRow;
        FMouseDownCol := vCol;

        FSelectCellRang.StartRow := vMoveRow;
        FSelectCellRang.StartCol := vMoveCol;
        FSelectCellRang.EndRow := FMouseDownRow;
        FSelectCellRang.EndCol := FMouseDownCol;
      end;

      {with Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData do
      begin
        SelectInfo.StartItemNo := 0;
        SelectInfo.StartItemOffset := 0;
      end;}

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
      else
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
    then
    begin
      FSelectCellRang.EndRow := -1;
      FSelectCellRang.EndCol := -1;
    end;
  end;
  {$ENDREGION}

  {$REGION 'MatchCellSelectState'}
  procedure MatchCellSelectState;
  var
    vRow, vCol: Integer;
  begin
    if not FSelectCellRang.EditCell then
    begin
      for vRow := FSelectCellRang.FStartRow to FSelectCellRang.FEndRow do
      begin
        for vCol := FSelectCellRang.FStartCol to FSelectCellRang.FEndCol do
        begin
          {if (vRow = vMoveRow) and (vCol = vMoveCol) then else 什么情况下需要跳过?}
          if Cells[vRow, vCol].CellData <> nil then
            Cells[vRow, vCol].CellData.SelectAll;
        end;
      end;
    end;
  end;
  {$ENDREGION}

var
  vCellPt: TPoint;
  vResizeInfo: TResizeInfo;
begin
  if ActiveDataResizing then
  begin
    vCellPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
    Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.MouseMove(
      Shift, X - vCellPt.X - FCellHPadding, Y - vCellPt.Y - FCellVPadding);

    Exit;
  end;

  if Resizing then  // (ssLeft in Shift)
  begin
    FResizeInfo.DestX := X;
    FResizeInfo.DestY := Y;
    FOwnerData.Style.UpdateInfoRePaint;

    Exit;
  end;

  vResizeInfo := GetCellAt(X, Y, vMoveRow, vMoveCol);

  if vResizeInfo.TableSite = tsCell then  // 鼠标在单元格中
  begin
    if FMouseLBDowning or (Shift = [ssLeft]) then  // 左键按下移动，按下时在表格上 or 没有在表格上按下(划选进入)
    begin
      if FDraging or FOwnerData.Style.UpdateInfo.Draging then
      begin
        FMouseMoveRow := vMoveRow;
        FMouseMoveCol := vMoveCol;
        vCellPt := GetCellPostion(FMouseMoveRow, FMouseMoveCol);
        Cells[FMouseMoveRow, FMouseMoveCol].CellData.MouseMove(Shift,
          X - vCellPt.X - FCellHPadding, Y - vCellPt.Y - FCellVPadding);

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
        Cells[FMouseMoveRow, FMouseMoveCol].CellData.MouseMove(Shift,
          X - vCellPt.X - FCellHPadding, Y - vCellPt.Y - FCellVPadding);
      end;
    end
    else  // 鼠标移动，没有按键按下
    begin
      if (vMoveRow <> FMouseMoveRow) or (vMoveCol <> FMouseMoveCol) then  // 鼠标移动到新单元格
      begin
        if (FMouseMoveRow >= 0) and (FMouseMoveCol >= 0) then
        begin
          if FRows[FMouseMoveRow].Cols[FMouseMoveCol].CellData <> nil then
            FRows[FMouseMoveRow].Cols[FMouseMoveCol].CellData.MouseMove(Shift, -1, -1);  // 旧单元格移出
        end;

        FMouseMoveRow := vMoveRow;
        FMouseMoveCol := vMoveCol;
      end;

      if (FMouseMoveRow < 0) or (FMouseMoveCol < 0) then Exit;

      vCellPt := GetCellPostion(FMouseMoveRow, FMouseMoveCol);
      FRows[FMouseMoveRow].Cols[FMouseMoveCol].CellData.MouseMove(Shift,
        X - vCellPt.X - FCellHPadding, Y - vCellPt.Y - FCellVPadding);
    end;
  end
  else
  begin
    if (FMouseMoveRow >= 0) and (FMouseMoveCol >= 0) then
    begin
      if FRows[FMouseMoveRow].Cols[FMouseMoveCol].CellData <> nil then
        FRows[FMouseMoveRow].Cols[FMouseMoveCol].CellData.MouseMove(Shift, -1, -1);  // 旧单元格移出
    end;

    FMouseMoveRow := -1;
    FMouseMoveCol := -1;

    if vResizeInfo.TableSite = tsBorderRight then // 鼠标不在单元格中
      GCursor := crHSplit
    else
    if vResizeInfo.TableSite = tsBorderBottom then
      GCursor := crVSplit;
  end;
end;

procedure THCTableItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vPt: TPoint;
  vUpRow, vUpCol: Integer;
  vCellPt: TPoint;
  vResizeInfo: TResizeInfo;
  //vMouseUpInSelect: Boolean;
begin
  FMouseLBDowning := False;

  if ActiveDataResizing then
  begin
    vPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
    Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.MouseUp(
      Button, Shift, X - vPt.X - FCellHPadding, Y - vPt.Y - FCellVPadding);

    Exit;
  end;

  if Resizing then  // 拖动改变列宽，单元格Data宽度的改变由重新格式化处理
  begin
    if FResizeInfo.TableSite = tsBorderRight then  // 拖宽/拖窄
    begin
      vPt.X := X - FMouseDownX;  // 不使用FResizeInfo.DestX(会造成按下处弹出也有偏移)
      if vPt.X <> 0 then
      begin
        // AReDest为False用于处理拖动改变列宽时，如拖动处列是合并源，其他行此列并无合并操作
        // 这时弹起，如果取拖动列目标列变宽，则其他行拖动处的列并没变宽
        vResizeInfo := GetCellAt(FMouseDownX, FMouseDownY, vUpRow, vUpCol, False{实际位置处的列});

        if (vResizeInfo.TableSite <> tsOutside) and (vPt.X <> 0) then  // 没弹起在外面
        begin
          if vPt.X > 0 then  // 拖宽了
          begin
            if vUpCol < FColWidths.Count - 1 then  // 右侧有，右侧变窄后不能小于最小宽度
            begin
              if FColWidths[vUpCol + 1] - vPt.X < MinColWidth then
                vPt.X := FColWidths[vUpCol + 1] - MinColWidth;

              if vPt.X <> 0 then
              begin
                FColWidths[vUpCol] := FColWidths[vUpCol] + vPt.X;  // 当前列变化
                if vUpCol < FColWidths.Count - 1 then  // 右侧的弥补变化
                  FColWidths[vUpCol + 1] := FColWidths[vUpCol + 1] - vPt.X;
              end;
            end
            else  // 最右侧列拖宽
            begin
              {if FColWidths[vUpCol] + vPt.X > PageWidth then  暂时不处理拖动超过页宽
                vPt.X := Width - FColWidths[vUpCol + 1];

              if vPt.X <> 0 then}
                FColWidths[vUpCol] := FColWidths[vUpCol] + vPt.X;  // 当前列变化
            end;
          end
          else  // 拖窄了
          begin
            if FColWidths[vUpCol] + vPt.X < MinColWidth then  // 小于最小宽度
              vPt.X := MinColWidth - FColWidths[vUpCol];

            if vPt.X <> 0 then
            begin
              FColWidths[vUpCol] := FColWidths[vUpCol] + vPt.X;  // 当前列变化
              if vUpCol < FColWidths.Count - 1 then  // 右侧的弥补变化
                FColWidths[vUpCol + 1] := FColWidths[vUpCol + 1] - vPt.X;
            end;
          end;
        end;
      end;
    end
    else
    if FResizeInfo.TableSite = tsBorderBottom then  // 拖高/拖矮
    begin
      vPt.Y := Y - FMouseDownY;  // // 不使用FResizeInfo.DestY(会造成按下处弹出也有偏移)
      if vPt.Y <> 0 then
      begin
        FRows[FMouseDownRow].Height := FRows[FMouseDownRow].Height + vPt.Y;
        FRows[FMouseDownRow].AutoHeight := False;
       end;
    end;

    Resizing := False;
    GCursor := crDefault;
    FOwnerData.Style.UpdateInfoRePaint;
    FOwnerData.Style.UpdateInfoReCaret;

    Exit;
  end;

  if FSelecting or FOwnerData.Style.UpdateInfo.Selecting then  // 划选完成
  begin
    FSelecting := False;

    // 先在按下单元格弹起，以便单元格中嵌套的表格有机会响应弹起(取消按下、划选状态，划选完成)
    if (FMouseDownRow >= 0) and (not FOutSelectInto) then  // 在表格右侧按下移动时再弹起时无有效的FMouseDownRow和FMouseDownCol
    begin
      vPt := GetCellPostion(FMouseDownRow, FMouseDownCol);
      Cells[FMouseDownRow, FMouseDownCol].CellData.MouseUp(Button, Shift,
        X - vPt.X - FCellHPadding, Y - vPt.Y - FCellVPadding);
    end;

    vResizeInfo := GetCellAt(X, Y, vUpRow, vUpCol);
    if vResizeInfo.TableSite = TTableSite.tsCell then  // 没有划选到页面空白的地方
    begin
      if (vUpRow <> FMouseDownRow) or (vUpCol <> FMouseDownCol) then  // 划选完成后弹起在非按下单元格
      begin
        vPt := GetCellPostion(vUpRow, vUpCol);
        Cells[vUpRow, vUpCol].CellData.MouseUp(Button, Shift,
          X - vPt.X - FCellHPadding, Y - vPt.Y - FCellVPadding);
      end;
    end;
  end
  else
  if FDraging or FOwnerData.Style.UpdateInfo.Draging then  // 拖拽弹起
  begin
    FDraging := False;

    vResizeInfo := GetCellAt(X, Y, vUpRow, vUpCol);

    if vResizeInfo.TableSite = TTableSite.tsCell then  // 拖到了某单元格中
    begin
      DisSelectSelectedCell(vUpRow, vUpCol);  // 取消除弹起处之外的所有拖拽选中单元格的状态
      FSelectCellRang.Initialize;  // 准备重新赋值

      // 不管是否在在选中单元格中弹起，拖拽弹起都需要编辑到选中单元格，
      FSelectCellRang.StartRow := vUpRow;
      FSelectCellRang.StartCol := vUpCol;
      vPt := GetCellPostion(vUpRow, vUpCol);
      Cells[vUpRow, vUpCol].CellData.MouseUp(Button, Shift,
        X - vPt.X - FCellHPadding, Y - vPt.Y - FCellVPadding);

      {if FMouseDownRow >= 0 then  // 有点击时的单元格(表格是划选范围内其中一个，在其他上拖拽到表格上时没有按下FMouseDownRow)
        Cells[FMouseDownRow, FMouseDownCol].CellData.InitializeField;}  // 拖拽起始单元格标明拖拽完成了
    end;
  end
  else  // 非划选，非拖拽
  if FMouseDownRow >= 0 then  // 有点击时的单元格
  begin
    vPt := GetCellPostion(FMouseDownRow, FMouseDownCol);
    Cells[FMouseDownRow, FMouseDownCol].CellData.MouseUp(Button, Shift,
      X - vPt.X - FCellHPadding, Y - vPt.Y - FCellVPadding);
  end;
end;

function THCTableItem.ClearFormatExtraHeight: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FRows.Count - 1 do
    Result := Result + FRows[i].ClearFormatExtraHeight;
  Self.Height := Self.Height - Result;
end;

function THCTableItem.GetFormatHeight: Integer;
var
  i: Integer;
begin
  Result := FBorderWidth;
  for i := 0 to RowCount - 1 do
    Result := Result + FRows[i].Height + FBorderWidth;
end;

function THCTableItem.GetHint: string;
var
  vCell: THCTableCell;
begin
  Result := inherited GetHint;
  if (FMouseMoveRow < 0) or (FMouseMoveCol < 0) then Exit;
  vCell := Cells[FMouseMoveRow, FMouseMoveCol];
  if (vCell <> nil) and (vCell.CellData <> nil) then
    Result := vCell.CellData.GetHint;
end;

function THCTableItem.GetCells(ARow, ACol: Integer): THCTableCell;
begin
  Result := FRows[ARow].Cols[ACol];
end;

{function THCTableItem.GetColCount: Integer;
begin
  Result := FColWidths.Count;
end;}

procedure THCTableItem.GetDestCell(const ARow, ACol: Cardinal; var ADestRow,
  ADestCol: Integer);
begin
  if Cells[ARow, ACol].CellData <> nil then
  begin
    ADestRow := ARow;
    ADestCol := ACol;
  end
  else
  begin
    ADestRow := ARow + Cells[ARow, ACol].RowSpan;
    ADestCol := ACol + Cells[ARow, ACol].ColSpan;
  end;
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
    Result := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol]
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
  ARow := -1;
  ACol := -1;
  if (X < 0) or (Y < 0) then Exit;
  if (X > Width) or (Y > Height) then Exit;
  { 获取是否在行或列的边框上 }
  // 判断是否在最上边框
  vTop := FBorderWidth;
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
    vBottom := vTop + FRows[i].Height + FBorderWidth;
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
  vLeft := FBorderWidth;
  for i := 0 to FColWidths.Count - 1 do
  begin
    vRight := vLeft + FColWidths[i] + FBorderWidth;
    GetDestCell(ARow, i, vDestRow, vDestCol);
    if CheckColBorderRang(vRight) then  // 第i列右边框
    begin
      ACol := i;
      if vDestCol + Cells[vDestRow, vDestCol].ColSpan <> i then  // 在列边框时，且不是合并源列最后一列，按在单元格中处理
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
        and (vDestRow + Cells[vDestRow, vDestCol].RowSpan <> ARow)
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

    if AReDest and (Cells[ARow, ACol].CellData = nil) then // 如果是被合并的单元格，返回合并后的单元格
      GetDestCell(ARow, ACol, ARow, ACol);
  end;
end;

function THCTableItem.GetCellPostion(const ARow, ACol: Integer): TPoint;
var
  i: Integer;
begin
  Result.X := FBorderWidth;
  Result.Y := FBorderWidth;
  for i := 0 to ARow - 1 do
    Result.Y := Result.Y + FRows[i].FmtOffset + FRows[i].Height + FBorderWidth;
  Result.Y := Result.Y + FRows[ARow].FmtOffset;
  for i := 0 to ACol - 1 do
    Result.X := Result.X + FColWidths[i] + FBorderWidth;
end;

{function THCTableItem.GetMergeDestCellData(const ARow,
  ACol: Integer): THCTableCellData;
var
  vDestRow, vDestCol: Integer;
begin
  GetMergeDest(ARow, ACol, vDestRow, vDestCol);
  Result := Cells[vDestRow, vDestCol].CellData;
end;}

procedure THCTableItem.GetMergeDest(const ARow, ACol: Integer; var ADestRow,
  ADestCol: Integer);
begin
  ADestRow := ARow;
  ADestCol := ACol;

  if Cells[ARow, ACol].RowSpan < 0 then
    ADestRow := ADestRow + Cells[ARow, ACol].RowSpan;

  if Cells[ARow, ACol].ColSpan < 0 then
    ADestCol := ADestCol + Cells[ARow, ACol].ColSpan;
end;

// 暂时没用注释掉了，因计算逻辑复杂，最好不要删除以备用
//procedure THCTableItem.GetPageFmtBottomInfo(const AHeight: Integer;
//  var ADItemMostBottom: Integer);
//var
//  i, j, vPageLastRow, vTop, vBottom: Integer;
//  vCellData: THCTableCellData;
//begin
//  ADItemMostBottom := Height;  // GetFormatHeight;
//  if ADItemMostBottom < AHeight then  // 表格整体都能放下 和 20160323002 相关
//    Exit;
//  vTop := FBorderWidth;
//  vPageLastRow := RowCount - 1;
//  for i := 0 to RowCount - 1 do  // 哪一行超过高度(分页行)
//  begin
//    vBottom := vTop + FRows[i].Height;
//    if vBottom > AHeight then
//    begin
//      vPageLastRow := i;
//      Break;
//    end
//    else
//      vTop := vBottom + FBorderWidth;
//  end;
//
//  ADItemMostBottom := 0;
//  // 处理vPageLastRow行整体从下一页开始的情况(辅助设计 2016-3-23_001.bmp)
//  for i := 0 to FRows[vPageLastRow].ColCount - 1 do
//  begin
//    if Cells[vPageLastRow, i].CellData = nil then
//      Continue;
//    vCellData := Cells[vPageLastRow, i].CellData;
//    if vCellData.DrawItems[0].Rect.Bottom + vTop > AHeight
//    then  // 如果vPageLastRow行中有列的第1个就在下一页，说明整行从下一页开始，此时当前页是上一行
//    begin
//      Dec(vPageLastRow);  // 上一行
//      // 计算上一行的底部位置
//      //ADItemMostBottom := FBorderWidth;
//      for j := 0 to vPageLastRow do
//        ADItemMostBottom := ADItemMostBottom + FBorderWidth + FRows[i].Height;
//      Exit;
//    end;
//  end;
//  // vPageLastRow行从当前页开始
//  for i := 0 to FRows[vPageLastRow].ColCount - 1 do
//  begin
//    if Cells[vPageLastRow, i].CellData = nil then
//      Continue;
//    vCellData := Cells[vPageLastRow, i].CellData;
//    vBottom := vCellData.DrawItems[0].Rect.Bottom + vTop;
//    if ADItemMostBottom < vBottom then
//      ADItemMostBottom := vBottom;
//    for j := 1 to vCellData.DrawItems.Count - 1 do
//    begin
//      vBottom := vCellData.DrawItems[j].Rect.Bottom + vTop;
//      if vBottom > AHeight then  // 当前DItem在下一页
//      begin
//        if ADItemMostBottom <
//          vCellData.DrawItems[j - 1].Rect.Bottom + vTop
//        then
//          ADItemMostBottom := vCellData.DrawItems[j - 1].Rect.Bottom + vTop;
//      end;
//    end;
//  end;
//end;

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
  Result := (Cells[vRow, vCol].CellData as THCCustomRichData).GetTopLevelDataAt(
    X - vCellPt.X - FCellHPadding, Y - vCellPt.Y - FCellVPadding)
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

function THCTableItem.InsertColAfter(const ACount: Byte): Boolean;
var
  i, j, k: Integer;
  viDestRow, viDestCol: Integer;
  //vTableRow: TTableRow;
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;
  for i := 0 to ACount - 1 do
  begin
    FColWidths.Insert(FSelectCellRang.StartCol + 1, 50);
    for j := 0 to RowCount - 1 do
    begin
      vCell := THCTableCell.Create(FOwnerData.Style);
      vCell.Width := 50;
      vCell.RowSpan := FRows[j].Cols[FSelectCellRang.StartCol].RowSpan;
      vCell.ColSpan := FRows[j].Cols[FSelectCellRang.StartCol].ColSpan;

      if FRows[j].Cols[FSelectCellRang.StartCol].ColSpan <> 0 then  // 合并的源
      begin
        GetDestCell(j, FSelectCellRang.StartCol, viDestRow, viDestCol);
        vCell.CellData.Free;
        vCell.CellData := nil;
        vCell.ColSpan := FRows[j].Cols[FSelectCellRang.StartCol].ColSpan - 1;

        for k := 1 to Cells[viDestRow, viDestCol].ColSpan do  // 目标的列跨度 - 已经跨的
          FRows[j].Cols[FSelectCellRang.StartCol + k].ColSpan := FRows[j].Cols[FSelectCellRang.StartCol + k].ColSpan - 1;  // 离目标列远1
      end;
      FRows[j].Insert(FSelectCellRang.StartCol + 1, vCell);
    end;
  end;

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Result := True;
end;

function THCTableItem.InsertColBefor(const ACount: Byte): Boolean;
var
  i, j, k: Integer;
  viDestRow, viDestCol: Integer;
  //vTableRow: TTableRow;
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;
  vCell.CellData.InitializeField;
  for i := 0 to ACount - 1 do
  begin
    for j := 0 to RowCount - 1 do
    begin
      vCell := THCTableCell.Create(FOwnerData.Style);
      vCell.Width := 50;
      vCell.RowSpan := FRows[j].Cols[FSelectCellRang.StartCol].RowSpan;
      vCell.ColSpan := FRows[j].Cols[FSelectCellRang.StartCol].ColSpan;

      if FRows[j].Cols[FSelectCellRang.StartCol].ColSpan < 0 then  // 合并的源
      begin
        GetDestCell(j, FSelectCellRang.StartCol, viDestRow, viDestCol);
        vCell.CellData.Free;
        vCell.CellData := nil;
        vCell.ColSpan := FRows[j].Cols[FSelectCellRang.StartCol].ColSpan;

        for k := 0 to Cells[viDestRow, viDestCol].ColSpan + FRows[j].Cols[FSelectCellRang.StartCol].ColSpan do  // 目标的列跨度 - 已经跨的
          FRows[j].Cols[FSelectCellRang.StartCol + k].ColSpan := FRows[j].Cols[FSelectCellRang.StartCol + k].ColSpan - 1;  // 离目标列远1
      end;
      FRows[j].Insert(FSelectCellRang.StartCol, vCell);
    end;
  end;

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Result := True;
end;

function THCTableItem.InsertItem(const AItem: THCCustomItem): Boolean;
var
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell <> nil then
    Result := vCell.CellData.InsertItem(AItem);
end;

function THCTableItem.InsertRowAfter(const ACount: Byte): Boolean;
var
  i, j, k: Integer;
  viDestRow, viDestCol: Integer;
  vTableRow: TTableRow;
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell = nil then Exit;

  vCell.CellData.InitializeField;
  for i := 1 to ACount do
  begin
    vTableRow := TTableRow.Create(FOwnerData.Style, FColWidths.Count);

    for j := 0 to FColWidths.Count - 1 do
    begin
      vTableRow.Cols[j].Width := FColWidths[j];

      if FRows[FSelectCellRang.StartRow].Cols[j].RowSpan > 0 then  // 在合并的目标单元格下面插
      begin
        // 目标单元格下面的单元格
        vTableRow.Cols[j].CellData.Free;
        vTableRow.Cols[j].CellData := nil;
        vTableRow.Cols[j].RowSpan := -1;
        if FRows[FSelectCellRang.StartRow].Cols[j].ColSpan < 0 then
          vTableRow.Cols[j].ColSpan := FRows[FSelectCellRang.StartRow].Cols[j].ColSpan;

        for k := 1 to FRows[FSelectCellRang.StartRow].Cols[j].RowSpan do
          FRows[FSelectCellRang.StartRow + k].Cols[j].RowSpan := FRows[FSelectCellRang.StartRow + k].Cols[j].RowSpan - 1;

        FRows[FSelectCellRang.StartRow].Cols[j].RowSpan := FRows[FSelectCellRang.StartRow].Cols[j].RowSpan + 1;  // 目标单元格行跨度增1
      end
      else
      if FRows[FSelectCellRang.StartRow].Cols[j].RowSpan < 0 then  // 合并源单元格下页插
      begin
        vTableRow.Cols[j].CellData.Free;
        vTableRow.Cols[j].CellData := nil;

        for k := 0 to Cells[viDestRow, viDestCol].RowSpan + FRows[FSelectCellRang.StartRow].Cols[j].RowSpan do  // 目标的行跨度 - 已经跨的
          FRows[FSelectCellRang.StartRow + k].Cols[j].RowSpan := FRows[FSelectCellRang.StartRow + k].Cols[j].RowSpan - 1;  // 离目标行远1
        FRows[viDestRow].Cols[j].RowSpan := FRows[viDestRow].Cols[j].RowSpan + 1;  // 目标行包含的合并源增加1
      end;
    end;

    FSelectCellRang.StartRow := FSelectCellRang.StartRow + 1;
    FRows.Insert(FSelectCellRang.StartRow, vTableRow);
  end;

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Result := True;
end;

function THCTableItem.InsertRowBefor(const ACount: Byte): Boolean;
var
  i, j, k: Integer;
  viDestRow, viDestCol: Integer;
  vTableRow: TTableRow;
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell <> nil then
    vCell.CellData.InitializeField;

  for i := 0 to ACount - 1 do
  begin
    vTableRow := TTableRow.Create(FOwnerData.Style, FColWidths.Count);
    for j := 0 to FColWidths.Count - 1 do
    begin
      vTableRow.Cols[j].Width := FColWidths[j];

      if FRows[FSelectCellRang.StartRow].Cols[j].RowSpan < 0 then  // 在合并的源单元格前面插入
      begin
        GetDestCell(FSelectCellRang.StartRow, j, viDestRow, viDestCol);
        vTableRow.Cols[j].CellData.Free;
        vTableRow.Cols[j].CellData := nil;
        vTableRow.Cols[j].RowSpan := FRows[FSelectCellRang.StartRow].Cols[j].RowSpan;

        for k := 0 to Cells[viDestRow, viDestCol].RowSpan + FRows[FSelectCellRang.StartRow].Cols[j].RowSpan do  // 目标的行跨度 - 已经跨的
          FRows[FSelectCellRang.StartRow + k].Cols[j].RowSpan := FRows[FSelectCellRang.StartRow + k].Cols[j].RowSpan - 1;  // 离目标行远1
        FRows[viDestRow].Cols[j].RowSpan := FRows[viDestRow].Cols[j].RowSpan + 1;  // 目标行包含的合并源增加1
      end;
    end;
    FRows.Insert(FSelectCellRang.StartRow, vTableRow);
  end;

  Self.InitializeMouseInfo;
  FSelectCellRang.Initialize;
  Result := True;
end;

function THCTableItem.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
var
  vCell: THCTableCell;
begin
  Result := False;
  vCell := GetEditCell;
  if vCell <> nil then
    Result := vCell.CellData.InsertStream(AStream, AStyle, AFileVersion);
end;

function THCTableItem.InsertText(const AText: string): Boolean;
begin
  Result := False;
  inherited;
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
    Result := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.InsertText(AText);
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
      if FRows[vR].Cols[vC].CellData <> nil then
        FRows[vR].Cols[vC].CellData.MarkStyleUsed(AMark);
    end;
  end;
end;

function THCTableItem.MergeCells(const AStartRow, AStartCol, AEndRow,
  AEndCol: Integer): Boolean;

  procedure DeleteEmptyRows(const ASRow, AERow: Cardinal);
  var
    vR, vC, vR1: Integer;
    vEmptyRow: Boolean;
  begin
    for vR := AERow downto ASRow do  // 遍历行
    begin
      vEmptyRow := True;
      for vC := 0 to FRows[vR].ColCount - 1 do  // 当前行各列
      begin
        if FRows[vR].Cols[vC].CellData <> nil then  // 存在没有被合并的列
        begin
          vEmptyRow := False;  // 不是空行
          Break;
        end;
      end;
      if vEmptyRow then  // 空行
      begin
        for vR1 := 0 to vR - 1 do
        begin
          for vC := 0 to FRows[vR1].ColCount - 1 do
          begin
            if Cells[vR1, vC].RowSpan > 0 then
              Cells[vR1, vC].RowSpan := Cells[vR1, vC].RowSpan - 1;
          end;
        end;
        for vR1 := vR + 1 to FRows.Count - 1 do
        begin
          for vC := 0 to FRows[vR1].ColCount - 1 do
          begin
            if Cells[vR1, vC].RowSpan < 0 then
              Cells[vR1, vC].RowSpan := Cells[vR1, vC].RowSpan + 1;
          end;
        end;
        FRows.Delete(vR);  // 删除当前空行
      end;
    end;
  end;

  procedure DeleteEmptyCols(const ASCol, AECol: Cardinal);
  var
    vR, vC, vC2: Integer;
    vEmptyCol: Boolean;
    vTableCell: THCTableCell;
  begin
    for vC := AECol downto ASCol do  // 循环各列
    begin
      vEmptyCol := True;
      for vR := 0 to RowCount - 1 do  // 循环各行
      begin
        if FRows[vR].Cols[vC].CellData <> nil then  // 某行的第vC列没有被合并
        begin
          vEmptyCol := False;
          Break;
        end;
      end;
      if vEmptyCol then  // 是空列
      begin
        for vR := RowCount - 1 downto 0 do  // 循环各行，删除对应列
        begin
          for vC2 := 0 to vC - 1 do
          begin
            vTableCell := FRows[vR].Cols[vC2];
            if vC2 + vTableCell.ColSpan >= vC then
              vTableCell.ColSpan := vTableCell.ColSpan - 1;
          end;
          for vC2 := vC + 1 to FRows[vR].ColCount - 1 do
          begin
            vTableCell := FRows[vR].Cols[vC2];
            if vC2 + vTableCell.ColSpan < vC then
              vTableCell.ColSpan := vTableCell.ColSpan + 1;
          end;
          FRows[vR].Delete(vC);  // 删除列
        end;
        FColWidths[vC - 1] := FColWidths[vC -1] + FBorderWidth + FColWidths[vC];
        FColWidths.Delete(vC);
      end;
    end;
  end;

var
  vR, vC, vR1, vC1, vRowSpan, vColSpan, vNewColSpan, vNewRowSpan: Integer;
begin
  Result := CellsCanMerge(AStartRow, AStartCol, AEndRow, AEndCol);
  if not Result then Exit;
  if AStartRow = AEndRow then  // 同一行合并，CellsCanMerge里判断好了同一行肯定不同列
  begin
    for vC := AStartCol + 1 to AEndCol do  // 合并列
    begin
      if FRows[AStartRow].Cols[vC].CellData <> nil then  // 防止已经合并的重复再合并
      begin
        Cells[AStartRow, AStartCol].CellData.AddData(Cells[AStartRow, vC].CellData);
        Cells[AStartRow, AStartCol].ColSpan := Cells[AStartRow, AStartCol].ColSpan
          + Cells[AStartRow, vC].ColSpan + 1;

        {FRows[AStartRow].Cols[AStartCol].Width :=
          FRows[AStartRow].Cols[AStartCol].Width
          + FRows[AStartRow].Cols[vC].Width + FBorderWidth;}
        Cells[AStartRow, vC].CellData.Free;
        Cells[AStartRow, vC].CellData := nil;
        // 记录被合并到左侧(便于20161109001绘制时处理其后单元格位置)
        vNewColSpan := AStartCol - vC;
        vRowSpan := Cells[AStartRow, vC].RowSpan;  // 源单元格原来行跨度
        vColSpan := Cells[AStartRow, vC].ColSpan;  // 源单元格原来列跨度
        for vC1 := vC + 1 to vC + vColSpan do  // 源单元格做为目标单元格合并的同行源单元格增加列负跨度
          Cells[AStartRow, vC1].ColSpan := Cells[AStartRow, vC1].ColSpan + vNewColSpan;
        // 源单元格做为目标单元格合并的不同行源单元格增加列负跨度
        for vR1 := AStartRow + 1 to AStartRow + vRowSpan do
        begin
          for vC1 := vC to vC + vColSpan do
            Cells[vR1, vC1].ColSpan := Cells[vR1, vC1].ColSpan + vNewColSpan;
        end;

        Cells[AStartRow, vC].ColSpan := vNewColSpan;
        Cells[AStartRow, vC].RowSpan := 0;
      end;
    end;
    DeleteEmptyCols(AStartCol + 1, AEndCol);
    Result := True;
  end
  else
  if AStartCol = AEndCol then  // 同列合并
  begin
    for vR := AStartRow + 1 to AEndRow do  // 合并各行
    begin
      if FRows[vR].Cols[AStartCol].CellData <> nil then  // 防止已经合并的重复再合并
      begin
        FRows[AStartRow].Cols[AStartCol].CellData.AddData(FRows[vR].Cols[AStartCol].CellData);
        FRows[AStartRow].Cols[AStartCol].RowSpan := FRows[AStartRow].Cols[AStartCol].RowSpan
          + Cells[vR, AStartCol].RowSpan + 1;
        FRows[vR].Cols[AStartCol].CellData.Free;
        FRows[vR].Cols[AStartCol].CellData := nil;

        vNewRowSpan := AStartRow - vR;
        vRowSpan := Cells[vR, AStartCol].RowSpan;  // 源单元格原来行跨度
        vColSpan := Cells[vR, AStartCol].ColSpan;  // 源单元格原来列跨度
        for vR1 := vR + 1 to vR + vRowSpan do  // 源单元格做为目标单元格合并的同列源单元格增加行负跨度
          Cells[vR1, AStartCol].RowSpan := Cells[vR1, AStartCol].RowSpan + vNewRowSpan;
        // 源单元格做为目标单元格合并的不同列源单元格增加行负跨度
        for vC1 := AStartCol + 1 to AStartCol + vColSpan do
        begin
          for vR1 := vR to vR + vRowSpan do
            Cells[vR1, vC1].RowSpan := Cells[vR1, vC1].RowSpan + vNewRowSpan;
        end;

        Cells[vR, AStartCol].RowSpan := vNewRowSpan;
        Cells[vR, AStartCol].ColSpan := 0;
      end;
    end;
    DeleteEmptyRows(AStartRow + 1, AEndRow);
    Result := True;
  end
  else  // 不同行，不同列
  begin
    // 起始行各列合并(2)
    for vC := AStartCol + 1 to AEndCol do
    begin
      if FRows[AStartRow].Cols[vC].CellData <> nil then  // 防止已经合并的重复再合并
      begin
        FRows[AStartRow].Cols[AStartCol].CellData.AddData(FRows[AStartRow].Cols[vC].CellData);
        FRows[AStartRow].Cols[AStartCol].ColSpan := FRows[AStartRow].Cols[AStartCol].ColSpan + 1;
        FRows[AStartRow].Cols[vC].CellData.Free;
        FRows[AStartRow].Cols[vC].CellData := nil;
        // 记录被合并到左侧(便于20161109001绘制时处理其后单元格位置)
        FRows[AStartRow].Cols[vC].ColSpan := AStartCol - vC;
      end;
    end;
    // 剩余行各列合并
    for vR := AStartRow + 1 to AEndRow do  // 循环行
    begin
      //vEmptyRow := True;
      for vC := AStartCol to AEndCol do
      begin
        if FRows[vR].Cols[vC].CellData <> nil then
        begin
          FRows[AStartRow].Cols[AStartCol].CellData.AddData(FRows[vR].Cols[vC].CellData);
          FRows[vR].Cols[vC].CellData.Free;
          FRows[vR].Cols[vC].CellData := nil;
          FRows[vR].Cols[vC].ColSpan := AStartCol - vC;
          FRows[vR].Cols[vC].RowSpan := AStartRow - vR;
          //vEmptyRow := False;
        end;
      end;
      //if not vEmptyRow then
        FRows[AStartRow].Cols[AStartCol].RowSpan := FRows[AStartRow].Cols[AStartCol].RowSpan + 1;
    end;
    DeleteEmptyRows(AStartRow + 1, AEndRow);
    // 删除空列
    DeleteEmptyCols(AStartCol + 1, AEndCol);

    Result := True;
  end;
end;

function THCTableItem.MergeSelectCells: Boolean;
begin
  if (FSelectCellRang.StartRow >= 0) and (FSelectCellRang.EndRow >= 0) then
  begin
    Result := MergeCells(FSelectCellRang.StartRow, FSelectCellRang.StartCol,
      FSelectCellRang.EndRow, FSelectCellRang.EndCol);
    if Result then
    begin
      { 防止合并后有空行或空列被删除后，DisSelect访问越界，所以合并后直接赋值结束信息 }
      //Self.InitializeMouseInfo;  // 合并后不保留选中单元格
      FSelectCellRang.EndRow := -1;
      FSelectCellRang.EndCol := -1;
      Self.Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.InitializeField;
      DisSelect;
    end;
  end
  else
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
    Result := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.MergeTableSelectCells
  else
    Result := False;
end;

function THCTableItem.CanDrag: Boolean;
begin
  Result := inherited CanDrag;
  if Result then
  begin
    if FSelectCellRang.EditCell then  // 在同一单元格中编辑
      Result := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.SelectedCanDrag
    else
      Result := Self.IsSelectComplate or Self.IsSelectPart;
  end;
end;

function THCTableItem.CellsCanMerge(const AStartRow, AStartCol, AEndRow,
  AEndCol: Integer): Boolean;
begin
  if AStartRow = AEndRow then  // 同一行
    Result := AStartCol <> AEndCol  // 不同列
  else  // 不同行
  begin
    Result := (Cells[AStartRow, AStartCol].Width = Cells[AEndRow, AStartCol].Width)
      and (Cells[AStartRow, AEndCol].Width = Cells[AEndRow, AEndCol].Width);
  end;
end;

function THCTableItem.CellSelectComplate(const ARow, ACol: Integer): Boolean;
begin
  Result := False;
  if FRows[ARow].Cols[ACol].CellData = nil then Exit;

  if FSelectCellRang.SameRow then  // 同一行
    Result := (ARow = FSelectCellRang.StartRow)
      and (ACol >= FSelectCellRang.StartCol)
      and (ACol <= FSelectCellRang.EndCol)
  else
  if FSelectCellRang.SameCol then  // 同一列
    Result := (ACol = FSelectCellRang.StartCol)
      and (ARow >= FSelectCellRang.StartRow)
      and (ARow <= FSelectCellRang.EndRow)
  else  // 不同行不同列
    Result := (ACol >= FSelectCellRang.StartCol)
      and (ACol <= FSelectCellRang.EndCol)
      and (ARow >= FSelectCellRang.StartRow)
      and (ARow <= FSelectCellRang.EndRow);
end;

procedure THCTableItem.CheckFormatPage(const ADrawItemRectTop, ADrawItemRectBottom,
  APageDataFmtTop, APageDataFmtBottom, AStartRowNo: Integer;
  var ABreakRow, AFmtOffset, ACellMaxInc: Integer);
var
  vRowDataFmtTop, vBreakRowBottom,
  vLastDFromRowBottom,  // 最后一个DItem底部距离行底部的距离
  vDestCellDataFmtTop,  // 单元格数据顶部(被合并单元格分页时以目标单元格为准)
  vRowMergedHeight,  // 当前行上面合并掉的行高总和
  vH,  // 当前DItem向下偏移多少可以显示在下一页顶部
  vCellInc,  // 当前行分页后，增加的高度
  vMergeDestRow2,
  vDestRow, vDestCol  // 合并的目标单元格
    :Integer;
  i, j, k: Integer;
  vCellData: THCTableCellData;
  vDItem: THCCustomDrawItem;
  vFirstLinePlace: Boolean;  // 各单元格都至少有一行内容可在分页位置上面完整显示
  vRect: TRect;
  vCellCross: TCellCross;
  vCellCrosses: TObjectList<TCellCross>;
begin
  ABreakRow := 0;
  AFmtOffset := 0;
  ACellMaxInc := 0;  // vCellInc的最大值，表示当前行各列为避开分页额外增加的格式化高度中最高的

  { 得到起始行的Fmt起始位置 }
  vRowDataFmtTop := ADrawItemRectTop + FBorderWidth;  // 第1行数据Y坐标的起始位置
  for i := 0 to AStartRowNo - 1 do
    vRowDataFmtTop := vRowDataFmtTop + FRows[i].FmtOffset + FRows[i].Height + FBorderWidth;  // 第i行数据结束位置

  { 从起始行开始检测当前页是否能放完表格 }
  i := AStartRowNo;
  while i < RowCount do  // 遍历每一行
  begin
    vBreakRowBottom := vRowDataFmtTop + FRows[i].FmtOffset + FRows[i].Height + FBorderWidth;  // 第i行数据结束位置
    if vBreakRowBottom > APageDataFmtBottom then  // 第i行数据结束位置超出页数据结束位置，放不下
    begin
      ABreakRow := i;  // 第i行需要处理分页
      //vH := APageDataFmtBottom - vRowDataFmtTop;  // 用于分页时，清楚页底部有多少空间用于放第i行(用于处理有些单元格可能有一部分数据留在当前页)
      Break;
    end;
    vRowDataFmtTop := vBreakRowBottom;  // 第i行数据起始位置
    Inc(i);
  end;

//  if ABreakRow = 0 then  // 表格在当前页一行也放不下，开启后可以实现表格第一行在当前页显示不下时整体下移
//  begin
//    //if vRowDataFmtTop < APageDataFmtTop then  // 这样判断，当表格在第2页第1个时不准确 当前页开始Item不是当前表格（表格是当前页第一个Item，和分页不同，分页虽然也是第一个，但表格的起始位置并不在分页后的页）
//    begin
//      AFmtOffset := APageDataFmtBottom - ADrawItemRectTop;
//      Exit;
//    end;
//  end;

  { 放不下，则判断分页位置 }
  { -分页位置是各单元格第一行的，处理是否存在可以留在分页位置上面的内容- }
  vFirstLinePlace := True;
  //vH := Max(APageDataFmtTop, vRowDataFmtTop);  // 判断当前行分页时的顶部位置(第一次判断时是vRowDataFmtTop，如果跨多页是APageDataFmtTop)
  //vCheckTop := vH - vRowDataFmtTop;  // 顶部位置相对单元格顶部位置
  //vCheckBottom := vCheckTop + APageDataFmtBottom - vH;  // 底部位置相对单元格底部位置
  vCellInc := 0;  // 行各内容为避开分页额外增加的格式化高度

  vCellCrosses := TObjectList<TCellCross>.Create;
  try

    {$REGION '先计算没有合并的单元格，已注释掉'}
//    // 先计算没有合并的单元格，防止 201703241150.bmp 的情况下第2行第2列第一行能在第1页置下，但不能计算出超过了第2行的高度
//    for i := 0 to FRows[ABreakRow].ColCount - 1 do
//    begin
//      if FRows[ABreakRow].Cols[i].ColSpan < 0 then  // 合并目标只需由正下方的单元格处理合并内容，不用重复处理
//        Continue;
//      if FRows[ABreakRow].Cols[i].RowSpan < 0 then  // 先处理没有合并行操作的
//        Continue;
//
//      vCellData := FRows[ABreakRow].Cols[i].CellData;
//      // 处理图 2016-4-15_1.bmp 第2行第2列的情况
//      vLastDFromRowBottom :=  // 原最后一个DItem底部距离行底部的空白距离
//        FRows[ABreakRow].Cols[i].Height - vCellData.Height;
//      vRealCellDataFmtTop := vRowDataFmtTop;
//
//      vCellCross := TCellCross.Create;
//      vCellCross.Col := i;
//
//      for j := 0 to vCellData.DrawItems.Count - 1 do
//      begin
//        vDItem := vCellData.DrawItems[j];
//        if not vDItem.LineFirst then  // 只需要判断行第一个
//          Continue;
//
//        if j <> vCellData.DrawItems.Count - 1 then  // 最后一行的DItem要将边框算上
//          vBottomBorder := 0
//        else
//          vBottomBorder := FBorderWidth;
//
//        if vRealCellDataFmtTop + vDItem.Rect.Bottom + vBottomBorder > APageDataFmtBottom then // 当前DItem底部超过页底部了 20160323002 // 行底部的边框线显示不下时也向下偏移
//        begin
//          if j = 0 then
//            vFirstLinePlace := False;
//
//          // 计算分页的DItem向下偏移多少可在下一页全显示该DItem
//          vH := APageDataFmtBottom - (vRealCellDataFmtTop + vDItem.Rect.Top{ + vBottomBorder}) // 页Data底部 - 当前DItem在页的相对位置
//            + FBorderWidth;  // 增加分页后上一页预留出边框
//          vCellInc := vH - vLastDFromRowBottom;  // 实际增加的高度 = 分页向下偏移的距离 - 原最后一个DItem底部距离行底部的空白距离
//
//          vCellCross.DItemNo := j;
//          vCellCross.VOffset := vH;
//
//          Break;
//        end;
//      end;
//      if ACellMaxInc < vCellInc then
//        ACellMaxInc := vCellInc;  // 记录各单元格中分页向下偏移的最大增量
//
//      vCellCrosses.Add(vCellCross);
//    end;
    {$ENDREGION}

    {$REGION '再计算有行合并的单元格'}
    for i := 0 to FRows[ABreakRow].ColCount - 1 do  // 遍历所有单元格中DItem，向下偏移vH
    begin
      if FRows[ABreakRow].Cols[i].ColSpan < 0 then  // 合并目标只需由正下方的单元格处理合并内容，不用重复处理
        Continue;

      GetMergeDest(ABreakRow, i, vDestRow, vDestCol);
      vCellData := FRows[vDestRow].Cols[vDestCol].CellData;
      // 处理图 2016-4-15_1.bmp 第2行第2列的情况
      vLastDFromRowBottom :=  // 原最后一个DrawItem底部距离行底部的空白距离
        FRows[vDestRow].Cols[vDestCol].Height - vCellData.Height - FCellVPadding;
      vDestCellDataFmtTop := vRowDataFmtTop;
      while vDestRow < ABreakRow do  // 恢复到目标单元格
      begin
        vDestCellDataFmtTop := vDestCellDataFmtTop - FBorderWidth - FRows[vDestRow].Height;
        Inc(vDestRow);
      end;

      vCellCross := TCellCross.Create;
      vCellCross.Col := i;

      for j := 0 to vCellData.DrawItems.Count - 1 do
      begin
        vDItem := vCellData.DrawItems[j];
        if not vDItem.LineFirst then  // 只需要判断行第一个
          Continue;

        vRect := vDItem.Rect;
        if vDestCellDataFmtTop + vRect.Bottom + FBorderWidth + FCellVPadding > APageDataFmtBottom then // 当前DItem底部超过页底部了 20160323002 // 行底部的边框线显示不下时也向下偏移
        begin
          if j = 0 then
            vFirstLinePlace := False;

          // 计算分页的DItem向下偏移多少可在下一页全显示该DItem
          vH := APageDataFmtBottom - (vDestCellDataFmtTop + vRect.Top{ + vBottomBorder}) // 页Data底部 - 当前DItem在页的相对位置
            + FBorderWidth;  // 增加分页后上一页预留出边框
          vCellInc := vH - vLastDFromRowBottom;  // 实际增加的高度 = 分页向下偏移的距离 - 原最后一个DItem底部距离行底部的空白距离

          vCellCross.DItemNo := j;
          vCellCross.VOffset := vH;
          vCellCross.MergeSrc := FRows[ABreakRow].Cols[i].RowSpan < 0;

          Break;
        end;
      end;
      if ACellMaxInc < vCellInc then
        ACellMaxInc := vCellInc;  // 记录各单元格中分页向下偏移的最大增量

      vCellCrosses.Add(vCellCross);
    end;
    {$ENDREGION}

    {$REGION '旧方法2017-1-15日注释掉'}

  //  if not vFirstLinePlace then  // 存在某单元格，在分页处第一行就需要放到下一页(整行从下一页开始)
  //  begin
  //    vH := vCheckBottom - vCheckTop;  // 整体向下移动到下一页
  //    for i := 0 to FRows[ABreakRow].ColCount - 1 do  // 遍历所有单元格中DItem，向下偏移vH
  //    begin
  //      if FRows[ABreakRow].Cols[i].ColSpan < 0 then  // 合并目标只需由正下方的单元格处理合并内容，不用重复处理
  //        Continue;
  //
  //      GetMergeDest(ABreakRow, i, vMergeDestRow, vMergeDestCol);
  //      vCellData := FRows[vMergeDestRow].Cols[vMergeDestCol].CellData;
  //      for j := 0 to vCellData.DrawItems.Count - 1 do  // 向下偏移vH
  //        OffsetRect(vCellData.DrawItems[j].Rect, 0, vH);
  //    end;
  //    ACellMaxInc := vH;
  //  end
  //  else  // 每一列都可以放下第一行内容，各单元格部分内容在当前页，部分内容从下一页开始
  //  begin
  //    // 当前分页行各单元格处理分页位置
  //    vCellInc := 0;  // 行各内容为避开分页额外增加的格式化高度
  //    ACellMaxInc := 0;  // vCellInc的最大值，表示当前行为避开分页额外增加的格式化高度
  //
  //    { 处理没有发生合并或合并目标单元格跨页 }
  //    for i := 0 to FRows[ABreakRow].ColCount - 1 do  // 遍历各列，判断分页位置
  //    begin
  //      if FRows[ABreakRow].Cols[i].ColSpan < 0 then  // 合并目标只需由正下方的单元格处理合并内容，不用重复处理
  //        Continue;
  //
  //      vRealCellDataFmtTop := vRowDataFmtTop;
  //
  //      GetMergeDest(ABreakRow, i, vMergeDestRow, vMergeDestCol);
  //      vCellData := FRows[vMergeDestRow].Cols[vMergeDestCol].CellData;
  //      // 处理图 2016-4-15_1.bmp 第2行第2列的情况
  //      vLastDFromRowBottom :=  // 原最后一个DItem底部距离行底部的空白距离
  //        FRows[vMergeDestRow].Cols[vMergeDestCol].Height - vCellData.Height;
  //      while vMergeDestRow < ABreakRow do  // 恢复到目标单元格
  //      begin
  //        vRealCellDataFmtTop := vRealCellDataFmtTop - FBorderWidth - FRows[vMergeDestRow].Height;
  //        Inc(vMergeDestRow);
  //      end;
  //
  //      for j := 0 to vCellData.DrawItems.Count - 1 do
  //      begin
  //        vDItem := vCellData.DrawItems[j];
  //        if not vDItem.LineFirst then  // 只需要判断行第一个
  //          Continue;
  //        if vRealCellDataFmtTop + vDItem.Rect.Bottom > APageDataFmtBottom then // 当前DItem底部超过页底部了 20160323002 // 行底部的边框线显示不下时也向下偏移
  //        begin
  //          // 计算分页的DItem向下偏移多少可在下一页全显示该DItem
  //          vH := APageDataFmtBottom - (vRealCellDataFmtTop + vDItem.Rect.Top); // 页Data底部 - 当前DItem在页的相对位置
  //            //+ FBorderWidth;  // 分页后下一页预留出边框
  //          // 从当前DItem开始，向下偏移到下一页
  //          for k := j to vCellData.DrawItems.Count - 1 do
  //            OffsetRect(vCellData.DrawItems[k].Rect, 0, vH);
  //
  //          vCellInc := vH - vLastDFromRowBottom;  // 实际增加的高度 = 分页向下偏移的距离 - 原最后一个DItem底部距离行底部的空白距离
  //          Break;
  //        end;
  //      end;
  //      if ACellMaxInc < vCellInc then
  //        ACellMaxInc := vCellInc;  // 记录各单元格中分页向下偏移的最大增量
  //    end;

  //    for i := 0 to FRows[ABreakRow].ColCount - 1 do  // 遍历列，判断分页位置
  //    begin
  //      vRealCellDataFmtTop := vRowDataFmtTop;
  //      vRowMergedHeight := 0;
  //      if FRows[ABreakRow].Cols[i].CellData = nil then  // 被合并
  //      begin
  //        if FRows[ABreakRow].Cols[i].ColSpan < 0 then  // 合并目标只需由正下方的单元格处理合并内容，不用重复处理
  //          Continue;
  //
  //        GetMergeDest(ABreakRow, i, vMergeDestRow, vMergeDestCol);
  //        vCellData := Cells[vMergeDestRow, vMergeDestCol].CellData;
  //        while vMergeDestRow < ABreakRow do
  //        begin
  //          vRowMergedHeight := vRowMergedHeight + FRows[vMergeDestRow].Height + FBorderWidth;
  //          Inc(vMergeDestRow);
  //        end;
  //        vRealCellDataFmtTop := vRealCellDataFmtTop - vRowMergedHeight;
  //      end
  //      else  // 没有被合并
  //      begin
  //        vCellData := FRows[ABreakRow].Cols[i].CellData;
  //      end;
  //
  //      for j := 0 to vCellData.DrawItems.Count - 1 do
  //      begin
  //        vDItem := vCellData.DrawItems[j];
  //        if not vDItem.LineFirst then  // 只需要判断行第一个
  //          Continue;
  //        if vRealCellDataFmtTop + vDItem.Rect.Bottom > APageDataFmtBottom then // 当前DItem底部超过页底部了 20160323002 // 行底部的边框线显示不下时也向下偏移
  //        begin
  //          // 计算向下偏移多少可在下一页全显示该DItem
  //          vH := APageDataFmtBottom - (vRealCellDataFmtTop + vDItem.Rect.Top); // 页Data底部 - 当前DItem在页的相对位置
  //            //+ FBorderWidth;  // 分页后下一页预留出边框
  //          // 处理图 2016-4-15_1.bmp 第2行第2列的情况
  //          vLastDFromRowBottom :=  // 原最后一个DItem底部距离行底部的空白距离
  //            FRows[ABreakRow].Height + vRowMergedHeight - vCellData.LastDItem.Rect.Bottom;
  //          { TODO : DItem不是行第一个时，其前面的各DItem随着行垂直对齐方式不同是否也要向下偏移 }
  //          // 目前仅考虑从当前DItem所在行下一行的各DItem都向下偏移
  //          for k := j to vCellData.DrawItems.Count - 1 do
  //            OffsetRect(vCellData.DrawItems[k].Rect, 0, vH);
  //
  //          vCellInc := vH - vLastDFromRowBottom;  // 实际增加的高度 = 分页向下偏移的距离 - 原最后一个DItem底部距离行底部的空白距离
  //          Break;
  //        end{
  //        else
  //          vCellData.DrawItems[j].FmtTopOffset := 0};
  //      end;
  //      if ACellMaxInc < vCellInc then
  //        ACellMaxInc := vCellInc;  // 记录各单元格中分页向下偏移的最大增量
  //    end;
    {$ENDREGION}

    if ACellMaxInc > 0 then  // 跨页行各列为跨页额外增加的最大量
    begin
      if not vFirstLinePlace then  // 某单元格第一行就在当前页放不下了，即跨页行整行需要下移到下一页
      begin
        if ABreakRow = 0 then
        begin
          AFmtOffset := APageDataFmtBottom - ADrawItemRectTop;
          ACellMaxInc := 0;  // 整体向下偏移时，就代表了第一行的向下偏移，或者说第一行的FmtOffset永远是0，因为整体向下偏移的依据是判断第一行
          Exit;
        end;

        FRows[ABreakRow].FmtOffset := ACellMaxInc;  // 整行下移，普通单元格跨页增加的偏移依据此行的偏移处理了
        // 行合并源单元格，需要处理从目标到此，和整行下移分页行的 上一行的 底部对齐
        for i := 0 to vCellCrosses.Count - 1 do
        begin
          if vCellCrosses[i].MergeSrc then  // 合并源
          begin
            GetMergeDest(ABreakRow, vCellCrosses[i].Col, vDestRow, vDestCol);
            vH := vDestRow + FRows[vDestRow].Cols[vDestCol].RowSpan;  // 合并到的最下面行序号
            vCellData := FRows[vDestRow].Cols[vDestCol].CellData;
            //vLastDFromRowBottom :=  // 原最后一个DItem底部距离行底部的空白距离
            //  FRows[vDestRow].Cols[vDestCol].Height - vCellData.Height - FCellVPadding;
            vDestCellDataFmtTop := vRowDataFmtTop;
            vMergeDestRow2 := vDestRow;
            while vMergeDestRow2 < ABreakRow do  // 恢复到目标单元格
            begin
              vDestCellDataFmtTop := vDestCellDataFmtTop - FBorderWidth - FRows[vMergeDestRow2].Height;
              Inc(vMergeDestRow2);
            end;

            for j := 0 to vCellData.DrawItems.Count - 1 do
            begin
              vDItem := vCellData.DrawItems[j];
              vRect := vDItem.Rect;
              if j = vCellData.DrawItems.Count - 1 then
                vRect.Bottom := vRect.Bottom + FCellVPadding;
              if not vDItem.LineFirst then  // 只需要判断行第一个
                Continue;
              if vDestCellDataFmtTop + vRect.Bottom > vRowDataFmtTop then  // 当前DItem超过整体下移行的上一行底部了
              begin
                vCellInc := vRowDataFmtTop - vDestCellDataFmtTop - vDItem.Rect.Top + FCellVPadding;// + FRows[ABreakRow].FmtOffset;
                for k := j to vCellData.DrawItems.Count - 1 do  // 超过上一行底部的全部移下去
                  OffsetRect(vCellData.DrawItems[k].Rect, 0, vCellInc);

                FRows[vH].Height := FRows[vH].Height + vCellInc;// - FRows[vH].FmtOffset;

                if ACellMaxInc < vCellInc then
                  ACellMaxInc := vCellInc;
                Break;
              end;
            end;

            FRows[vDestRow].Cols[vDestCol].Height := FRows[vDestRow].Cols[vDestCol].Height + ACellMaxInc;
          end;
        end;
      end
      else  // 不需要整行都下移到下一页
      begin
        for i := 0 to vCellCrosses.Count - 1 do  // 遍历所有单元格中DItem，向下偏移vH
        begin
          //if FRows[ABreakRow].Cols[vCellCrosses[i].Col].ColSpan < 0 then  // 合并目标只需由正下方的单元格处理合并内容，不用重复处理
          //  Continue;
          if vCellCrosses[i].DItemNo < 0 then  // 不需要偏移
            Continue;
          GetMergeDest(ABreakRow, vCellCrosses[i].Col, vDestRow, vDestCol);
          vCellData := FRows[vDestRow].Cols[vDestCol].CellData;
          for j := vCellCrosses[i].DItemNo to vCellData.DrawItems.Count - 1 do
            OffsetRect(vCellData.DrawItems[j].Rect, 0, vCellCrosses[i].VOffset);
        end;

        FRows[ABreakRow].Height := FRows[ABreakRow].Height + ACellMaxInc;  // 累加表格为了跨当前页额外增加的高度
        for i := 0 to FRows[ABreakRow].ColCount - 1 do  // 将当前行分当前页的增量影响到各相关单元格，以便下一页分页时计算
        begin
          if Cells[ABreakRow, i].ColSpan < 0 then
            Continue;

          GetMergeDest(ABreakRow, i, vDestRow, vDestCol);
          Cells[vDestRow, vDestCol].Height := Cells[vDestRow, vDestCol].Height + ACellMaxInc;
        end;
      end;
    end;
  finally
    FreeAndNil(vCellCrosses);
  end;
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
                and (vCol <= FSelectCellRang.EndCol)
        end
        else  // 无选择结束行，判断是否在当前单元格的选中中
        begin
          vCellData := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData;
          if vCellData.SelectExists then
          begin
            vCellPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
            vX := X - vCellPt.X - FCellHPadding;
            vY := Y - vCellPt.Y - FCellVPadding;
            vCellData.GetItemAt(vX, vY, vItemNo, vOffset, vDrawItemNo, vRestrain);

            Result := vCellData.CoordInSelect(vX, vY, vItemNo, vOffset, vRestrain);
          end;
        end;
      end;
    end;

    {if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
    begin
      if FSelectCellRang.EndRow >= 0 then  // 有选择结束行
      begin
        vCellPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
        vSelRect.TopLeft := vCellPt;
        vCellPt := GetCellPostion(FSelectCellRang.EndRow, FSelectCellRang.EndCol);
        vSelRect.Right := vCellPt.X + FColWidths[FSelectCellRang.EndCol];
        vSelRect.Bottom := vCellPt.Y + FRows[FSelectCellRang.EndRow].Height;
        Result := PtInRect(vSelRect, Point(X, Y));
      end
      else  // 无选择结束行，判断当前单元格是否有选中
      begin
        vCellData := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData;
        if vCellData.SelectExists then
        begin
          vCellPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
          vX := X - vCellPt.X - FCellHPadding;
          vY := Y - vCellPt.Y - FCellVPadding;
          vCellData.GetItemAt(vX, vY, vItemNo, vOffset, vDrawItemNo, vRestrain);

          Result := (not vRestrain) and vCellData.CoordInSelect(vX, vY, vItemNo, vOffset);
        end;
      end;
    end;}
  end;
end;

procedure THCTableItem.SaveSelectToStream(const AStream: TStream);
var
  vCellData: THCCustomData;
begin
  if Self.IsSelectComplate then  // 全选择了
    raise Exception.Create('保存选中内容出错，表格不应该由内部处理全选中的保存！')
  else
  begin
    vCellData := GetActiveData;
    if vCellData <> nil then
      vCellData.SaveSelectToStream(AStream);
  end;
end;

procedure THCTableItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
var
  i, vR, vC: Integer;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  AStream.WriteBuffer(FBorderVisible, SizeOf(FBorderVisible));
  AStream.WriteBuffer(FRows.Count, SizeOf(FRows.Count));  // 行数
  AStream.WriteBuffer(FColWidths.Count, SizeOf(FColWidths.Count));  // 列数

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
      FRows[vR].Cols[vC].SaveToStream(AStream);
  end;
end;

procedure THCTableItem.SelectAll;
begin
  FSelectCellRang.StartRow := 0;
  FSelectCellRang.StartCol := 0;
  FSelectCellRang.EndRow := RowCount - 1;
  FSelectCellRang.EndCol := FRows[FSelectCellRang.EndRow].ColCount - 1;
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
      Result := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.SelectExists;
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
    if (vCell <> nil) and (vCell.CellData <> nil) then
      vCell.CellData.Active := Value;
    if not Value then
      Self.InitializeMouseInfo;

    inherited SetActive(Value);
  end;
end;

procedure THCTableItem.SetResizing(const Value: Boolean);
begin
  inherited SetResizing(Value);
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
      if FRows[vRow].Cols[vCol].CellData <> nil then
        FRows[vRow].Cols[vCol].CellData.SelectAll;
    end;
  end;
end;

procedure THCTableItem.TraverseItem(const ATraverse: TItemTraverse);
var
  vR, vC: Integer;
begin
  for vR := 0 to FRows.Count - 1 do
  begin
    if ATraverse.Stop then Break;

    for vC := 0 to FColWidths.Count - 1 do
    begin
      if ATraverse.Stop then Break;

      if Cells[vR, vC].CellData <> nil then
        Cells[vR, vC].CellData.TraverseItem(ATraverse);
    end;
  end;
end;

function THCTableItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := True;
end;

function THCTableItem.ActiveDataResizing: Boolean;
begin
  Result := False;
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
    Result := Cells[FSelectCellRang.StartRow, FSelectCellRang.StartCol].CellData.SelectedResizing
end;

procedure THCTableItem.ApplySelectParaStyle(const AStyle: THCStyle;
  const AMatchStyle: TParaMatch);
var
  vR, vC: Integer;
begin
  if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
  begin
    if FSelectCellRang.EndRow >= 0 then  // 有选择结束行，说明选中不在同一单元格
    begin
      for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
      begin
        { TODO -jingtong : 当单元格SelectComplate时，处理全部应用样式 }
        for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
          Cells[vR, vC].CellData.ApplySelectParaStyle(AMatchStyle);
      end;
    end
    else  // 在同一单元格
      GetEditCell.CellData.ApplySelectParaStyle(AMatchStyle);
  end
  else
    Self.ParaNo := AMatchStyle.GetMatchParaNo(FOwnerData.Style, Self.ParaNo);
end;

function THCTableItem.ApplySelectTextStyle(const AStyle: THCStyle;
  const AMatchStyle: TStyleMatch): Integer;
var
  vR, vC: Integer;
begin
  if FSelectCellRang.EditCell then  // 在同一单元格中编辑
    GetEditCell.CellData.ApplySelectTextStyle(AMatchStyle)
  else
  if FSelectCellRang.StartRow >= 0 then  // 有选择起始行
  begin
    for vR := FSelectCellRang.StartRow to FSelectCellRang.EndRow do
    begin
      { TODO -jingtong : 当单元格SelectComplate时，处理全部应用样式 }
      for vC := FSelectCellRang.StartCol to FSelectCellRang.EndCol do
        Cells[vR, vC].CellData.ApplySelectTextStyle(AMatchStyle);
    end;
  end;
end;

function THCTableItem.GetActiveData: THCCustomData;
var
  vCell: THCTableCell;
begin
  Result := nil;
  vCell := GetEditCell;
  if vCell <> nil then
    Result := vCell.CellData.GetTopLevelData;
end;

function THCTableItem.GetActiveDrawItem: THCCustomDrawItem;
var
  vCellData: THCTableCellData;
begin
  Result := nil;
  vCellData := GetActiveData as THCTableCellData;
  if vCellData <> nil then
    Result := vCellData.GetActiveDrawItem;
end;

function THCTableItem.GetActiveDrawItemCoord: TPoint;
var
  vCell: THCTableCell;
  vPt: TPoint;
begin
  Result := Point(0, 0);
  vCell := GetEditCell;
  if vCell <> nil then
  begin
    Result := vCell.CellData.GetActiveDrawItemCoord;
    vPt := GetCellPostion(FSelectCellRang.StartRow, FSelectCellRang.StartCol);
    Result.X := Result.X + vPt.X + FCellHPadding;
    Result.Y := Result.Y + vPt.Y + FCellVPadding;
  end;
end;

function THCTableItem.GetActiveItem: THCCustomItem;
var
  vCell: THCTableCell;
begin
  Result := Self;
  vCell := GetEditCell;
  if vCell <> nil then
    Result := vCell.CellData.GetActiveItem;
end;

procedure THCTableItem.GetCaretInfo(var ACaretInfo: TCaretInfo);
var
  vRow, vCol: Integer;
  vPos: TPoint;
  vCaretCell: THCTableCell;
begin
  if FOwnerData.Style.UpdateInfo.Draging then
  begin
    vRow := FMouseMoveRow;
    vCol := FMouseMoveCol;
  end
  else
  begin
    vRow := FSelectCellRang.StartRow;
    vCol := FSelectCellRang.StartCol;
  end;

  if vRow < 0 then
  begin
    ACaretInfo.Visible := False;
    Exit;
  end
  else
    vCaretCell := Cells[vRow, vCol];

  if FOwnerData.Style.UpdateInfo.Draging then
  begin
    if (vCaretCell.CellData.MouseMoveItemNo < 0)
      or (vCaretCell.CellData.MouseMoveItemOffset < 0)
    then
    begin
      ACaretInfo.Visible := False;
      Exit;
    end;  
    vCaretCell.CellData.GetCaretInfo(vCaretCell.CellData.MouseMoveItemNo,
      vCaretCell.CellData.MouseMoveItemOffset, ACaretInfo)  
  end
  else
  begin
    if (vCaretCell.CellData.SelectInfo.StartItemNo < 0)
      or (vCaretCell.CellData.SelectInfo.StartItemOffset < 0)
    then
    begin
      ACaretInfo.Visible := False;
      Exit;
    end;
    vCaretCell.CellData.GetCaretInfo(vCaretCell.CellData.SelectInfo.StartItemNo,
      vCaretCell.CellData.SelectInfo.StartItemOffset, ACaretInfo);
  end;

  vPos := GetCellPostion(vRow, vCol);
  ACaretInfo.X := vPos.X + ACaretInfo.X + FCellHPadding;
  ACaretInfo.Y := vPos.Y + ACaretInfo.Y + FCellVPadding;
end;

{ TSelectCellRang }

constructor TSelectCellRang.Create;
begin
  Initialize;
end;

function TSelectCellRang.EditCell: Boolean;
begin
  Result := (FStartRow >= 0) and (FEndRow < 0);  // 这样比SameRow和SameCol更快捷？
end;

procedure TSelectCellRang.Initialize;
begin
  FStartRow := -1;
  FStartCol := -1;
  FEndRow := -1;
  FEndCol := -1;
end;

function TSelectCellRang.SameCol: Boolean;
begin
  Result := (FStartCol >= 0) and (FStartCol = FEndCol);
end;

function TSelectCellRang.SameRow: Boolean;
begin
  Result := (FStartRow >= 0) and (FStartRow = FEndRow);
end;

function TSelectCellRang.SelectExists: Boolean;
begin
  Result := (FStartRow >= 0) and (FEndRow >= 0);  // 暂时没有用到此方法
end;

{ TCellCross }

constructor TCellCross.Create;
begin
  inherited;
  Col := -1;
  DItemNo := -1;
  VOffset := 0;
  MergeSrc := False;
end;

{ TTableRows }

procedure TTableRows.Notify(const Value: TTableRow;
  Action: TCollectionNotification);
begin
  inherited;
  if Action = TCollectionNotification.cnAdded then
  begin
    if Assigned(FOnRowAdd) then
      FOnRowAdd(Value);
  end;
end;

end.
