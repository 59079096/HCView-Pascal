{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{             文档内各类对象基本管理单元                }
{                                                       }
{*******************************************************}

{******************* 代码修改说明 ***********************
201807311101 在行首插入内容后光标后移，按下时并没有改变ItemNo和Offset，导致光标不重新回到行首
}

unit HCCustomRichData;

interface

uses
  Windows, Classes, Types, Controls, Graphics, SysUtils, HCCustomData, HCStyle,
  HCItem, HCDrawItem, HCTextStyle, HCParaStyle, HCStyleMatch, HCCommon, HCRectItem,
  HCTextItem, HCUndo;

type
  TInsertProc = reference to function(const AItem: THCCustomItem): Boolean;

  TDrawItemPaintEvent = procedure(const AData: THCCustomData;
    const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
    const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;

  TItemMouseEvent = procedure(const AData: THCCustomData; const AItemNo: Integer;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TDataItemEvent = procedure(const AData: THCCustomData; const AItemNo: Integer) of object;

  THCCustomRichData = class(THCCustomData)
  strict private
    FWidth: Cardinal;
    /// <summary> 鼠标左键按下(打开文件对话框双击文件后会触发MouseMouse，MouseUp) </summary>
    FMouseLBDowning,
    /// <summary> 鼠标双击(处理双击自动选中，弹起清除选中的问题) </summary>
    FMouseLBDouble,
    FMouseDownReCaret,
    FMouseMoveRestrain  // 并不是在Item范围内MouseMove而是通过约束坐标找到的
      : Boolean;

    FMouseDownX, FMouseDownY: Integer;

    FMouseDownItemNo,
    FMouseDownItemOffset,
    FMouseMoveItemNo,
    FMouseMoveItemOffset,

    FSelectSeekNo,
    FSelectSeekOffset  // 选中操作时的游标
      : Integer;

    FReadOnly,
    FSelecting, FDraging: Boolean;

    FOnItemResized: TDataItemEvent;
    FOnInsertItem: TItemNotifyEvent;
    FOnItemMouseDown, FOnItemMouseUp: TItemMouseEvent;
    FOnDrawItemPaintBefor, FOnDrawItemPaintAfter: TDrawItemPaintEvent;
    FOnCreateItem: TNotifyEvent;  // 新建了Item(目前主要是为了打字和用中文输入法输入英文时痕迹的处理)

    procedure FormatData(const AStartItemNo, ALastItemNo: Integer);

    /// <summary> 初始化为只有一个空Item的Data</summary>
    procedure SetEmptyData;

    /// <summary> Data只有空行Item时插入Item(用于替换当前空行Item的情况) </summary>
    function EmptyDataInsertItem(const AItem: THCCustomItem): Boolean;

    /// <summary> 为避免表格插入行、列大量重复代码，使用匿名方法，但不支持D7 </summary>
    function TableInsertRC(const AProc: TInsertProc): Boolean;

    procedure InitializeMouseField;

    /// <summary> 划完完成后最后操作位置是否在选中范围起始 </summary>
    function IsSelectSeekStart: Boolean;

    { 撤销恢复相关方法+ }
    procedure Undo_StartGroup(const AItemNo, AOffset: Integer);
    procedure Undo_EndGroup(const AItemNo, AOffset: Integer);
    procedure Undo_StartRecord;
    /// <summary> 删除Text </summary>
    /// <param name="AItemNo">操作发生时的ItemNo</param>
    /// <param name="AOffset">删除的起始位置</param>
    /// <param name="AText"></param>
    procedure Undo_DeleteText(const AItemNo, AOffset: Integer; const AText: string);
    procedure Undo_InsertText(const AItemNo, AOffset: Integer; const AText: string);

    /// <summary> 删除指定的Item </summary>
    /// <param name="AItemNo">操作发生时的ItemNo</param>
    /// <param name="AOffset">操作发生时的Offset</param>
    procedure Undo_DeleteItem(const AItemNo, AOffset: Integer);

    /// <summary> 插入Item到指定位置 </summary>
    /// <param name="AItemNo">操作发生时的ItemNo</param>
    /// <param name="AOffset">操作发生时的Offset</param>
    procedure Undo_InsertItem(const AItemNo, AOffset: Integer);
    procedure Undo_ItemParaFirst(const AItemNo, AOffset: Integer; const ANewParaFirst: Boolean);

    procedure Undo_ItemSelf(const AItemNo, AOffset: Integer);
    { 撤销恢复相关方法- }
  protected
    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; virtual;

    // <summary> 设置光标位置到指定的Item最后面 </summary>
    procedure ReSetSelectAndCaret(const AItemNo: Integer); overload;

    /// <summary> 设置光标位置到指定的Item指定位置 </summary>
    /// <param name="AItemNo">指定ItemNo</param>
    /// <param name="AOffset">指定位置</param>
    /// <param name="ANextWhenMid">如果此位置前后的DrawItem正好分行，True后一个DrawItem前面，False前一个后面</param>
    procedure ReSetSelectAndCaret(const AItemNo, AOffset: Integer;
      const ANextWhenMid: Boolean = False); overload;

    /// <summary> 当前Item对应的格式化起始Item和结束Item(段最后一个Item) </summary>
    /// <param name="AFirstItemNo">起始ItemNo</param>
    /// <param name="ALastItemNo">结束ItemNo</param>
    procedure GetReformatItemRange(var AFirstItemNo, ALastItemNo: Integer); overload;

    /// <summary> 指定Item对应的格式化起始Item和结束Item(段最后一个Item) </summary>
    /// <param name="AFirstItemNo">起始ItemNo</param>
    /// <param name="ALastItemNo">结束ItemNo</param>
    procedure GetReformatItemRange(var AFirstItemNo, ALastItemNo: Integer; const AItemNo, AItemOffset: Integer); overload;

    /// <summary>
    /// 合并2个文本Item
    /// </summary>
    /// <param name="ADestItem">合并后的Item</param>
    /// <param name="ASrcItem">源Item</param>
    /// <returns>True:合并成功，False不能合并</returns>
    function MergeItemText(const ADestItem, ASrcItem: THCCustomItem): Boolean; virtual;

    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    function CanDeleteItem(const AItemNo: Integer): Boolean; virtual;

    /// <summary> 用于从流加载完Items后，检查不合格的Item并删除 </summary>
    function CheckInsertItemCount(const AStartNo, AEndNo: Integer): Integer; virtual;

    procedure DoItemInsert(const AItem: THCCustomItem); virtual;
    procedure DoItemMouseLeave(const AItemNo: Integer); virtual;
    procedure DoItemMouseEnter(const AItemNo: Integer); virtual;
    procedure DoItemResized(const AItemNo: Integer);
    function DoInsertText(const AText: string): Boolean;
    function GetWidth: Cardinal; virtual;
    procedure SetWidth(const Value: Cardinal);
    function GetHeight: Cardinal; virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;

    /// <summary> 准备格式化参数 </summary>
    /// <param name="AStartItemNo">开始格式化的Item</param>
    /// <param name="APrioDItemNo">上一个Item的最后一个DrawItemNo</param>
    /// <param name="APos">开始格式化位置</param>
    procedure _FormatReadyParam(const AStartItemNo: Integer;
      var APrioDrawItemNo: Integer; var APos: TPoint); virtual;

    // Format仅负责格式化Item，ReFormat负责格式化后对后面Item和DrawItem的关联处理
    procedure ReFormatData_(const AStartItemNo: Integer; const ALastItemNo: Integer = -1;
      const AExtraItemCount: Integer = 0); virtual;

    { 是否允许撤销恢复 }
    function EnableUndo: Boolean; virtual;

    // Item单独保存和读取事件
    procedure SaveItemToStreamAlone(const AItem: THCCustomItem; const AStream: TStream);
    function LoadItemFromStreamAlone(const AStream: TStream): THCCustomItem;

    function CalcContentHeight: Integer;
  public
    constructor Create(const AStyle: THCStyle); override;

    procedure Clear; override;
    // 选中内容应用样式
    function ApplySelectTextStyle(const AMatchStyle: THCStyleMatch): Integer; override;
    function ApplySelectParaStyle(const AMatchStyle: THCParaMatch): Integer; override;

    function DisSelect: Boolean; override;

    /// <summary> 删除选中内容(内部已经判断了是否有选中) </summary>
    /// <returns>True:有选中且删除成功</returns>
    function DeleteSelected: Boolean; override;

    /// <summary> 在光标处插入Item </summary>
    /// <param name="AItem"></param>
    /// <returns></returns>
    function InsertItem(const AItem: THCCustomItem): Boolean; overload; virtual;

    /// <summary> 在指定的位置插入Item </summary>
    /// <param name="AIndex">插入位置</param>
    /// <param name="AItem">插入的Item</param>
    /// <param name="AOffsetBefor">插入时在原位置Item前面(True)或后面(False)</param>
    /// <returns></returns>
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem;
      const AOffsetBefor: Boolean = True): Boolean; overload; virtual;

    procedure KillFocus; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual;

    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyPress(var Key: Char); virtual;

    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;

    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;

    procedure Undo(const AUndo: THCCustomUndo); virtual;
    procedure Redo(const ARedo: THCCustomUndo); virtual;

    /// <summary> 初始化相关字段和变量 </summary>
    procedure InitializeField; override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;
    //
    procedure DblClick(X, Y: Integer);
    function CanEdit: Boolean;
    procedure DeleteItems(const AStartNo: Integer; const AEndNo: Integer = -1);

    /// <summary> 添加Data到当前 </summary>
    /// <param name="ASrcData">源Data</param>
    procedure AddData(const ASrcData: THCCustomData);

    /// <summary> 在光标处换行 </summary>
    function InsertBreak: Boolean;

    /// <summary> 在光标处插入字符串(可带回车换行符) </summary>
    function InsertText(const AText: string): Boolean;

    /// <summary> 在光标处插入指定行列的表格 </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;

    /// <summary> 在光标处插入直线 </summary>
    function InsertLine(const ALineHeight: Integer): Boolean;

    function TableInsertRowAfter(const ARowCount: Byte): Boolean;
    function TableInsertRowBefor(const ARowCount: Byte): Boolean;
    function ActiveTableDeleteCurRow: Boolean;
    function ActiveTableSplitCurRow: Boolean;
    function ActiveTableSplitCurCol: Boolean;
    function TableInsertColAfter(const AColCount: Byte): Boolean;
    function TableInsertColBefor(const AColCount: Byte): Boolean;
    function ActiveTableDeleteCurCol: Boolean;
    function MergeTableSelectCells: Boolean;

    // Format仅负责格式化Item，ReFormat仅负责格式化后对后面Item和DrawItem的关联处理
    // 目前仅单元格用了，需要放到CellData中吗？
    procedure ReFormat(const AStartItemNo: Integer);

    /// <summary> 重新格式化当前Item(用于仅修改当前Item属性或内容) </summary>
    procedure ReFormatActiveItem;
    function GetTopLevelItem: THCCustomItem;
    function GetTopLevelDrawItem: THCCustomDrawItem;
    function GetActiveDrawItemCoord: TPoint;

    /// <summary> 取消激活(用于页眉、页脚、正文切换时原激活的取消) </summary>
    procedure DisActive;

    function GetHint: string;

    /// <summary> 返回当前光标处的顶层Data </summary>
    function GetTopLevelData: THCCustomRichData;

    /// <summary> 返回指定位置处的顶层Data </summary>
    function GetTopLevelDataAt(const X, Y: Integer): THCCustomRichData;

    property MouseDownItemNo: Integer read FMouseDownItemNo;
    property MouseDownItemOffset: Integer read FMouseDownItemOffset;
    property MouseMoveItemNo: Integer read FMouseMoveItemNo;
    property MouseMoveItemOffset: Integer read FMouseMoveItemOffset;
    property MouseMoveRestrain: Boolean read FMouseMoveRestrain;

    property Width: Cardinal read GetWidth write SetWidth;
    property Height: Cardinal read GetHeight;  // 实际内容的高
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Selecting: Boolean read FSelecting;
    property OnInsertItem: TItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnItemResized: TDataItemEvent read FOnItemResized write FOnItemResized;
    property OnItemMouseDown: TItemMouseEvent read FOnItemMouseDown write FOnItemMouseDown;
    property OnItemMouseUp: TItemMouseEvent read FOnItemMouseUp write FOnItemMouseUp;
    property OnDrawItemPaintBefor: TDrawItemPaintEvent read FOnDrawItemPaintBefor write FOnDrawItemPaintBefor;
    property OnDrawItemPaintAfter: TDrawItemPaintEvent read FOnDrawItemPaintAfter write FOnDrawItemPaintAfter;
    property OnCreateItem: TNotifyEvent read FOnCreateItem write FOnCreateItem;
  end;

implementation

uses
  HCTableItem, HCImageItem, HCCheckBoxItem, HCTabItem, HCLineItem, HCExpressItem,
  HCPageBreakItem, HCGifItem, HCEditItem, HCComboboxItem, HCQRCodeItem, HCBarCodeItem,
  HCFractionItem, HCDateTimePicker, HCRadioGroup;

{ THCCustomRichData }

constructor THCCustomRichData.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  Items.OnItemInsert := DoItemInsert;
  FReadOnly := False;
  InitializeField;
  SetEmptyData;
end;

function THCCustomRichData.CreateItemByStyle(
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := nil;
  if AStyleNo < THCStyle.Null then
  begin
    case AStyleNo of
      THCStyle.Image: Result := THCImageItem.Create(Self, 0, 0);
      THCStyle.Table: Result := THCTableItem.Create(Self, 1, 1, 1);
      THCStyle.Tab: Result := TTabItem.Create(Self, 0, 0);
      THCStyle.Line: Result := TLineItem.Create(Self, 1, 1);
      THCStyle.Express: Result := THCExpressItem.Create(Self, '', '', '', '');
      // RsVector
      THCStyle.Domain: Result := CreateDefaultDomainItem;
      THCStyle.PageBreak: Result := TPageBreakItem.Create(Self, 0, 1);
      THCStyle.CheckBox: Result := THCCheckBoxItem.Create(Self, '勾选框', False);
      THCStyle.Gif: Result := THCGifItem.Create(Self, 1, 1);
      THCStyle.Edit: Result := THCEditItem.Create(Self, '');
      THCStyle.Combobox: Result := THCComboboxItem.Create(Self, '');
      THCStyle.QRCode: Result := THCQRCodeItem.Create(Self, '');
      THCStyle.BarCode: Result := THCBarCodeItem.Create(Self, '');
      THCStyle.Fraction: Result := THCFractionItem.Create(Self, '', '');
      THCStyle.DateTimePicker: Result := THCDateTimePicker.Create(Self, Now);
      THCStyle.RadioGroup: Result := THCRadioGroup.Create(Self);
    else
      raise Exception.Create('未找到类型 ' + IntToStr(AStyleNo) + ' 对应的创建Item代码！');
    end;
  end
  else
  begin
    Result := CreateDefaultTextItem;
    Result.StyleNo := AStyleNo;
  end;
end;

procedure THCCustomRichData.DblClick(X, Y: Integer);
var
  i, vItemNo, vItemOffset, vDrawItemNo, vX, vY, vStartOffset, vEndOffset: Integer;
  vRestrain: Boolean;
  vText: string;
  vPosType: TCharType;
begin
  FMouseLBDouble := True;

  GetItemAt(X, Y, vItemNo, vItemOffset, vDrawItemNo, vRestrain);
  if vItemNo < 0 then Exit;

  if Items[vItemNo].StyleNo < THCStyle.Null then
  begin
    CoordToItemOffset(X, Y, vItemNo, vItemOffset, vX, vY);
    Items[vItemNo].DblClick(vX, vY);
  end
  else  // TextItem双击时根据光标处内容，选中范围
  if Items[vItemNo].Length > 0 then
  begin
    vText := GetDrawItemText(vDrawItemNo);  // DrawItem对应的文本
    vItemOffset := vItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;  // 映射到DrawItem上

    if vItemOffset > 0 then  // 光标处的Char类型
      vPosType := GetCharType(Word(vText[vItemOffset]))
    else
      vPosType := GetCharType(Word(vText[1]));

    vStartOffset := 0;
    for i := vItemOffset - 1 downto 1 do  // 往前找Char类型不一样的位置
    begin
      if GetCharType(Word(vText[i])) <> vPosType then
      begin
        vStartOffset := i;
        Break;
      end;
    end;

    vEndOffset := Length(vText);
    for i := vItemOffset + 1 to Length(vText) do  // 往后找Char类型不一样的位置
    begin
      if GetCharType(Word(vText[i])) <> vPosType then
      begin
        vEndOffset := i - 1;
        Break;
      end;
    end;

    Self.SelectInfo.StartItemNo := vItemNo;
    Self.SelectInfo.StartItemOffset := vStartOffset + DrawItems[vDrawItemNo].CharOffs - 1;

    if vStartOffset <> vEndOffset then  // 有选中的文本
    begin
      Self.SelectInfo.EndItemNo := vItemNo;
      Self.SelectInfo.EndItemOffset := vEndOffset + DrawItems[vDrawItemNo].CharOffs - 1;

      MatchItemSelectState;
    end;
  end;

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret(False);
end;

procedure THCCustomRichData.DeleteItems(const AStartNo: Integer; const AEndNo: Integer = -1);
var
  {vStartNo, }vEndNo, vDelCount: Integer;
  vItem: THCCustomItem;
begin
  InitializeField;
  DisSelect;  // 防止删除后原选中ItemNo不存在

  // 因目前调用DeleteItems后都是从0开始格式化，所以此处格式化相关代码暂时注释掉
  {if AStartNo > 0 then  // 从删除的前一个开始格式化
    vStartNo := AStartNo - 1
  else
    vStartNo := 0;}

  if AEndNo < 0 then
    vEndNo := AStartNo
  else
    vEndNo := AEndNo;

  vDelCount := vEndNo - AStartNo + 1;
  //FormatItemPrepare(vStartNo, vEndNo);
  Items.DeleteRange(AStartNo, vDelCount);
  //ReFormatData_(vStartNo, vEndNo - vDelCount, -vDelCount);
  if Items.Count = 0 then  // 删除没了
  begin
    vItem := CreateDefaultTextItem;
    vItem.ParaFirst := True;
    Items.Add(vItem);  // 不使用InsertText，为避免其触发ReFormat时因为没有格式化过，获取不到对应的DrawItem
  end
  else  // 删除完了还有
  if (AStartNo > 0) and (not Items[AStartNo].ParaFirst) then  // 删除位置的Item不是段首，判断是否能合并到前一个
  begin
    if Items[AStartNo - 1].CanConcatItems(Items[AStartNo]) then
    begin
      //vItem := Items[AStartNo - 1];
      Items[AStartNo - 1].Text := Items[AStartNo - 1].Text + Items[AStartNo].Text;
      Items.Delete(AStartNo);
    end;
  end;
end;

function THCCustomRichData.DeleteSelected: Boolean;
var
  vDelCount, vFormatFirstItemNo, vFormatLastItemNo,
  vLen, vParaFirstItemNo, vParaLastItemNo: Integer;
  vStartItem, vEndItem, vNewItem: THCCustomItem;

  {$REGION '删除全选中的单个Item'}
  function DeleteItemSelectComplate: Boolean;
  begin
    Result := False;
    if CanDeleteItem(SelectInfo.StartItemNo) then  // 允许删除
    begin
      Undo_DeleteItem(SelectInfo.StartItemNo, 0);
      Items.Delete(SelectInfo.StartItemNo);

      Inc(vDelCount);
      if (SelectInfo.StartItemNo > vFormatFirstItemNo)
        and (SelectInfo.StartItemNo < vFormatLastItemNo)
      then  // 全选中的Item在起始格式化和结束格式化中间
      begin
        vLen := Items[SelectInfo.StartItemNo - 1].Length;
        if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then  // 删除位置前后可合并
        begin
          Items.Delete(SelectInfo.StartItemNo);
          Inc(vDelCount);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := vLen;
        end
        else  // 删除位置前后不能合并，光标置为前一个后面
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := GetItemAfterOffset(SelectInfo.StartItemNo);
        end;
      end
      else
      if SelectInfo.StartItemNo = vParaFirstItemNo then  // 段第一个ItemNo
      begin
        if vParaFirstItemNo = vParaLastItemNo then  // 段就一个Item全删除了，补充空Item
        begin
          vNewItem := CreateDefaultTextItem;
          vNewItem.ParaFirst := True;
          Items.Insert(SelectInfo.StartItemNo, vNewItem);
          Undo_InsertItem(SelectInfo.StartItemNo, 0);

          SelectInfo.StartItemOffset := 0;
          Dec(vDelCount);
        end
        else
        begin
          SelectInfo.StartItemOffset := 0;
          Items[SelectInfo.StartItemNo].ParaFirst := True;
        end;
      end
      else
      if SelectInfo.StartItemNo = vParaLastItemNo then  // 段最后一个ItemNo
      begin
        {if vParaFirstItemNo = vParaLastItemNo then  // 段就一个Item全删除了,不会走到这里吧
        begin
          vItem := CreateDefaultTextItem;
          vItem.ParaFirst := True;
          Items.Insert(SelectInfo.StartItemNo, vItem);
          SelectInfo.StartItemOffset := 0;
          Dec(vDelCount);
        end
        else}
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
        end;
      end
      else  // 全选中的Item是起始格式化或结束格式化或在段内
      begin
        if SelectInfo.StartItemNo > 0 then
        begin
          vLen := Items[SelectInfo.StartItemNo - 1].Length;
          if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then
          begin
            Items.Delete(SelectInfo.StartItemNo);
            Inc(vDelCount);
            SelectInfo.StartItemOffset := vLen;
          end;
          
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
        end;
      end;
    end;
    Result := True;
  end;
  {$ENDREGION}

var
  i: Integer;
  vText: string;
  vSelectSeekStart,   // 选中范围游标在选中起始
  vSelStartComplate,  // 选中范围内的起始Item全选中了
  vSelEndComplate,    // 选中范围内的结束Item全选中了
  vSelStartParaFirst  // 选中起始是段首
    : Boolean;
begin
  Result := False;

  if not CanEdit then Exit;

  if SelectExists then
  begin
    vSelectSeekStart := IsSelectSeekStart;

    vDelCount := 0;
    Self.InitializeField;  // 删除后原鼠标处可能已经没有了

    if (SelectInfo.EndItemNo < 0)
      and (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)
    then  // 选择仅发生在RectItem内部
    begin
      // 如果变动会引起RectItem的宽度变化，则需要格式化到段最后一个Item
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

      if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).IsSelectComplateTheory then  // 理论全选了
      begin
        Undo_StartRecord;
        GetParaItemRang(SelectInfo.StartItemNo, vParaFirstItemNo, vParaLastItemNo);
        Result := DeleteItemSelectComplate;
      end
      else
        Result := (Items[SelectInfo.StartItemNo] as THCCustomRectItem).DeleteSelected;

      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
    end
    else  // 选中不是发生在RectItem内部
    begin
      vEndItem := Items[SelectInfo.EndItemNo];  // 选中结束Item
      if SelectInfo.EndItemNo = SelectInfo.StartItemNo then  // 选择发生在同一个Item
      begin
        Undo_StartRecord;

        GetParaItemRang(SelectInfo.StartItemNo, vParaFirstItemNo, vParaLastItemNo);
        GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

        if vEndItem.IsSelectComplate then  // 该TextItem全选中了
          Result := DeleteItemSelectComplate
        else  // Item部分选中
        begin
          if vEndItem.StyleNo < THCStyle.Null then  // 同一个RectItem  表格从前选中到一部分？
            (vEndItem as THCCustomRectItem).DeleteSelected
          else  // 同一个TextItem
          begin
            vText := vEndItem.Text;
            Undo_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1,
              Copy(vText, SelectInfo.StartItemOffset + 1, SelectInfo.EndItemOffset - SelectInfo.StartItemOffset));
            Delete(vText, SelectInfo.StartItemOffset + 1, SelectInfo.EndItemOffset - SelectInfo.StartItemOffset);
            vEndItem.Text := vText;
          end;
        end;

        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
      end
      else  // 选中发生在不同Item，起始(可能是段首)全选中结尾没全选，起始没全选结尾全选，起始结尾都没全选
      begin
        vFormatFirstItemNo := GetParaFirstItemNo(SelectInfo.StartItemNo);  // 取段第一个为起始
        vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);  // 取段最后一个为结束，如果变更注意下面

        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

        vSelStartParaFirst := Items[SelectInfo.StartItemNo].ParaFirst;
        vSelStartComplate := Items[SelectInfo.StartItemNo].IsSelectComplate;  // 起始是否全选
        vSelEndComplate := Items[SelectInfo.EndItemNo].IsSelectComplate;  // 结尾是否全选

        {vSelectRangComplate :=  // 选中范围内的Item都是全选中了
          (SelectInfo.StartItemOffset = 0)
           and ( ( (vItem.StyleNo < THCStyle.RsNull)
                     and (SelectInfo.EndItemOffset = OffsetAfter)
                 )
               or ( (vItem.StyleNo > THCStyle.RsNull)
                     and (SelectInfo.EndItemOffset = vItem.Length)
                  )
               );}

        Undo_StartRecord;

        // 先处理选中结束Item
        if vEndItem.StyleNo < THCStyle.Null then  // RectItem
        begin
          if vSelEndComplate then  // 选中在最后面，即全选  SelectInfo.EndItemOffset = OffsetAfter
          begin
            if CanDeleteItem(SelectInfo.EndItemNo) then  // 允许删除
            begin
              Undo_DeleteItem(SelectInfo.EndItemNo, OffsetAfter);
              Items.Delete(SelectInfo.EndItemNo);

              Inc(vDelCount);
            end;
          end
          else
          if SelectInfo.EndItemOffset = OffsetInner then  // 在其上
            (vEndItem as THCCustomRectItem).DeleteSelected;
        end
        else  // TextItem
        begin
          if vSelEndComplate then  // 在选中文本Item结束最后 SelectInfo.EndItemOffset = vEndItem.Length
          begin
            if CanDeleteItem(SelectInfo.EndItemNo) then  // 允许删除
            begin
              Undo_DeleteItem(SelectInfo.EndItemNo, vEndItem.Length);
              Items.Delete(SelectInfo.EndItemNo);
              Inc(vDelCount);
            end;
          end
          else  // 文本且不在选中结束Item最后
          begin
            Undo_DeleteText(SelectInfo.EndItemNo, 1, Copy(vEndItem.Text, 1, SelectInfo.EndItemOffset));
            // 结束Item留下的内容
            vText := (vEndItem as THCTextItem).GetTextPart(SelectInfo.EndItemOffset + 1,
              vEndItem.Length - SelectInfo.EndItemOffset);
            vEndItem.Text := vText;
          end;
        end;

        // 删除选中起始Item下一个到结束Item上一个
        for i := SelectInfo.EndItemNo - 1 downto SelectInfo.StartItemNo + 1 do
        begin
          if CanDeleteItem(i) then  // 允许删除
          begin
            Undo_DeleteItem(i, 0);
            Items.Delete(i);

            Inc(vDelCount);
          end;
        end;

        vStartItem := Items[SelectInfo.StartItemNo];  // 选中起始Item
        if vStartItem.StyleNo < THCStyle.Null then  // 起始是RectItem
        begin
          if SelectInfo.StartItemOffset = OffsetBefor then  // 在其前
          begin
            if CanDeleteItem(SelectInfo.StartItemNo) then  // 允许删除
            begin
              Undo_DeleteItem(SelectInfo.StartItemNo, 0);
              Items.Delete(SelectInfo.StartItemNo);
              Inc(vDelCount);
            end;
            if SelectInfo.StartItemNo > vFormatFirstItemNo then
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          end
          else
          if SelectInfo.StartItemOffset = OffsetInner then  // 在其上
            (vStartItem as THCCustomRectItem).DeleteSelected;
        end
        else  // 选中起始是TextItem
        begin
          if vSelStartComplate then  // 在最前起始全选了 SelectInfo.StartItemOffset = 0
          begin
            if CanDeleteItem(SelectInfo.StartItemNo) then  // 允许删除
            begin
              Undo_DeleteItem(SelectInfo.StartItemNo, 0);
              Items.Delete(SelectInfo.StartItemNo);
              Inc(vDelCount);
            end;
          end
          else
          //if SelectInfo.StartItemOffset < vStartItem.Length then  // 在中间(不用判断了吧？)
          begin
            Undo_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1,
              Copy(vStartItem.Text, SelectInfo.StartItemOffset + 1, vStartItem.Length - SelectInfo.StartItemOffset));
            vText := (vStartItem as THCTextItem).GetTextPart(1, SelectInfo.StartItemOffset);
            vStartItem.Text := vText;  // 起始留下的内容
          end;
        end;

        if vSelStartComplate and vSelEndComplate then  // 选中的Item都删除完
        begin
          if SelectInfo.StartItemNo = vFormatFirstItemNo then  // 选中起始在段最前
          begin
            if SelectInfo.EndItemNo = vFormatLastItemNo then  // 选中结束在当前段或后面某段最后(段里内容全删除了)，补充空行
            begin
              vNewItem := CreateDefaultTextItem;
              vNewItem.ParaFirst := True;
              Items.Insert(SelectInfo.StartItemNo, vNewItem);
              Undo_InsertItem(SelectInfo.StartItemNo, vNewItem.Length);

              Dec(vDelCount);
            end
            else  // 选中结束不在段最后
              Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst := True;  // 选中结束位置后面的成为段首
          end
          else
          if SelectInfo.EndItemNo = vFormatLastItemNo then  // 结束在段最后
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            SelectInfo.StartItemOffset := GetItemAfterOffset(SelectInfo.StartItemNo);
          end
          else  // 选中起始在起始段中间，选中结束在结束段中间
          begin
            vLen := Items[SelectInfo.StartItemNo - 1].Length;
            if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.EndItemNo - vDelCount + 1]) then  // 起始前面和结束后面可合并
            begin
              Undo_InsertText(SelectInfo.StartItemNo - 1,
                Items[SelectInfo.StartItemNo - 1].Length - Items[SelectInfo.EndItemNo - vDelCount + 1].Length + 1,
                Items[SelectInfo.EndItemNo - vDelCount + 1].Text);

              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              SelectInfo.StartItemOffset := vLen;

              Undo_DeleteItem(SelectInfo.EndItemNo - vDelCount + 1, 0);
              Items.Delete(SelectInfo.EndItemNo - vDelCount + 1);
              Inc(vDelCount);
            end
            else  // 起始前面和结束后面不能合并，如果选中起始和结束不在同一段
            begin
              if Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst then
              begin
                Undo_ItemParaFirst(SelectInfo.EndItemNo - vDelCount + 1, 0, False);
                Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst := False;  // 合并不成功就挨着
              end;
            end;
          end;
        end
        else  // 选中范围内的Item没有删除完
        begin
          if vSelStartComplate then  // 起始删除完了
          begin
            if Items[SelectInfo.EndItemNo - vDelCount].ParaFirst <> vSelStartParaFirst then
            begin
              Undo_ItemParaFirst(SelectInfo.EndItemNo - vDelCount, 0, vSelStartParaFirst);
              Items[SelectInfo.EndItemNo - vDelCount].ParaFirst := vSelStartParaFirst
            end;
          end
          else
          if not vSelEndComplate then  // 起始和结束都没有删除完
          begin
            if MergeItemText(Items[SelectInfo.StartItemNo], Items[SelectInfo.EndItemNo - vDelCount])
            then  // 选中起始、结束位置的Item合并成功
            begin
              Undo_InsertText(SelectInfo.StartItemNo,
                Items[SelectInfo.StartItemNo].Length - Items[SelectInfo.EndItemNo - vDelCount].Length + 1,
                Items[SelectInfo.EndItemNo - vDelCount].Text);

              Undo_DeleteItem(SelectInfo.EndItemNo - vDelCount, 0);
              Items.Delete(SelectInfo.EndItemNo - vDelCount);
              Inc(vDelCount);
            end
            else  // 选中起始、结束位置的Item不能合并
            begin
              if SelectInfo.EndItemNo <> vFormatLastItemNo then  // 选中结束不是段最后一个
              begin
                if Items[SelectInfo.EndItemNo - vDelCount].ParaFirst then
                begin
                  Undo_ItemParaFirst(SelectInfo.EndItemNo - vDelCount, 0, False);
                  Items[SelectInfo.EndItemNo - vDelCount].ParaFirst := False;  // 合并不成功就挨着
                end;
              end;
            end;
          end;
        end;

        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
      end;

      for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo - vDelCount do  // 不允许删除的取消选中状态
        Items[i].DisSelect;

      SelectInfo.EndItemNo := -1;
      SelectInfo.EndItemOffset := -1;
    end;

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;

    inherited DeleteSelected;

    ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, not vSelectSeekStart);
    Result := True;
  end;
end;

procedure THCCustomRichData.DisActive;
var
  vItem: THCCustomItem;
begin
  Self.InitializeField;

  if Items.Count > 0 then  // 页眉中元素激活后切换到正文不高亮
  begin
    vItem := GetCurItem;
    if vItem <> nil then
      vItem.Active := False;
  end;
end;

function THCCustomRichData.DisSelect: Boolean;
begin
  Result := inherited DisSelect;
  if Result then
  begin
    // 拖拽完成时清除
    FDraging := False;  // 拖拽完成
    //FMouseLBDowning := False;  // 鼠标按下在选中区域外的时候清选中，但不能改FMouseLBDowning状态
    FSelecting := False;  // 准备划选
    // Self.Initialize;  这里会导致Mouse事件中的FMouseLBDowning等属性被取消掉
    Style.UpdateInfoRePaint;
  end;

  Style.UpdateInfoReCaret;  // 选择起始信息被重置为-1
end;

procedure THCCustomRichData.Undo_DeleteText(const AItemNo, AOffset: Integer;
  const AText: string);
var
  vUndo: THCUndo;
  vTextAction: THCTextUndoAction;
begin
  if EnableUndo then
  begin
    vUndo := GetUndoList.Last;
    if vUndo <> nil then
    begin
      vTextAction := vUndo.ActionAppend(uatDeleteText, AItemNo, AOffset) as THCTextUndoAction;
      vTextAction.Text := AText;
    end;
  end;
end;

procedure THCCustomRichData.Undo_EndGroup(const AItemNo, AOffset: Integer);
begin
  if EnableUndo then
    GetUndoList.EndUndoGroup(AItemNo, AOffset);
end;

procedure THCCustomRichData.Undo_InsertText(const AItemNo, AOffset: Integer;
  const AText: string);
var
  vUndo: THCUndo;
  vTextAction: THCTextUndoAction;
begin
  if EnableUndo then
  begin
    vUndo := GetUndoList.Last;
    if vUndo <> nil then
    begin
      vTextAction := vUndo.ActionAppend(uatInsertText, AItemNo, AOffset) as THCTextUndoAction;
      vTextAction.Text := AText;
    end;
  end;
end;

procedure THCCustomRichData.Undo_ItemParaFirst(const AItemNo, AOffset: Integer;
  const ANewParaFirst: Boolean);
var
  vUndo: THCUndo;
  vItemAction: THCItemParaFirstUndoAction;
begin
  if EnableUndo then
  begin
    vUndo := GetUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := THCItemParaFirstUndoAction.Create;
      vItemAction.ItemNo := AItemNo;
      vItemAction.Offset := AOffset;
      vItemAction.OldParaFirst := Items[AItemNo].ParaFirst;
      vItemAction.NewParaFirst := ANewParaFirst;

      vUndo.Actions.Add(vItemAction);
    end;
  end;
end;

procedure THCCustomRichData.Undo_ItemSelf(const AItemNo, AOffset: Integer);
var
  vUndo: THCUndo;
begin
  if EnableUndo then
  begin
    vUndo := GetUndoList.Last;
    if vUndo <> nil then
      vUndo.ActionAppend(uatItemSelf, AItemNo, AOffset);
  end;
end;

procedure THCCustomRichData.Undo_StartGroup(const AItemNo, AOffset: Integer);
begin
  if EnableUndo then
    GetUndoList.BeginUndoGroup(AItemNo, AOffset);
end;

procedure THCCustomRichData.Undo_StartRecord;
begin
  if EnableUndo then
    GetUndoList.NewUndo;
end;

procedure THCCustomRichData.DoDrawItemPaintAfter(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoDrawItemPaintAfter(AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  if Assigned(FOnDrawItemPaintAfter) then
  begin
    FOnDrawItemPaintAfter(AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomRichData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoDrawItemPaintBefor(AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  if Assigned(FOnDrawItemPaintBefor) then
  begin
    FOnDrawItemPaintBefor(AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

function THCCustomRichData.DoInsertText(const AText: string): Boolean;
var
  vCarteItemNo, vFormatFirstItemNo, vFormatLastItemNo: Integer;
  vNewItem: THCCustomItem;

  {$REGION ' DoTextItemInsert 在文本Item前后或中间插入文本 '}
  function DoTextItemInsert: Boolean;
  var
    vTextItem: THCTextItem;
    vS: string;
    vLen: Integer;
  begin
    Result := False;

    vTextItem := Items[vCarteItemNo] as THCTextItem;

    if vTextItem.StyleNo = Self.Style.CurStyleNo then  // 当前样式和插入位置TextItem样式相同
    begin
      if vTextItem.CanAccept(SelectInfo.StartItemOffset) then  // TextItem此偏移位置可接受输入
      begin
        Undo_StartRecord;
        Undo_InsertText(vCarteItemNo, SelectInfo.StartItemOffset + 1, AText);

        GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

        if SelectInfo.StartItemOffset = 0 then  // 在最前面插入
        begin
          vTextItem.Text := AText + vTextItem.Text;
          vLen := Length(AText);
        end
        else
        if SelectInfo.StartItemOffset = vTextItem.Length then  // 在TextItem最后插入
        begin
          vTextItem.Text := vTextItem.Text + AText;
          vLen := vTextItem.Length;
        end
        else  // 在Item中间
        begin
          vLen := SelectInfo.StartItemOffset + Length(AText);
          vS := vTextItem.Text;
          Insert(AText, vS, SelectInfo.StartItemOffset + 1);
          vTextItem.Text := vS;
        end;

        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);

        ReSetSelectAndCaret(vCarteItemNo, vLen);

        Result := True;
      end
      else  // 此位置不可接受输入
      begin
        if (SelectInfo.StartItemOffset = 0)
          or (SelectInfo.StartItemOffset = vTextItem.Length)   // 在首尾不可接受时，插入到前后位置
        then
        begin
          vNewItem := CreateDefaultTextItem;
          vNewItem.Text := AText;

          if SelectInfo.StartItemOffset = 0 then
            Result := InsertItem(vCarteItemNo, vNewItem, True)
          else
            Result := InsertItem(vCarteItemNo + 1, vNewItem, False);
        end;
      end;
    end
    else  // 插入位置TextItem样式和当前样式不同，在TextItem头、中、尾没选中，但应用了新样式，以新样式处理
    begin
      vNewItem := CreateDefaultTextItem;
      vNewItem.Text := AText;
      Result := InsertItem(vNewItem);
    end;
  end;
  {$ENDREGION}

var
  vCarteItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
begin
  Result := False;
  if AText <> '' then
  begin
    vCarteItemNo := GetCurItemNo;
    vCarteItem := Items[vCarteItemNo];

    if vCarteItem.StyleNo < THCStyle.Null then  // 当前位置是 RectItem
    begin
      if SelectInfo.StartItemOffset = OffsetInner then  // 在其上输入内容
      begin
        Undo_StartRecord;
        Undo_ItemSelf(SelectInfo.StartItemNo, OffsetInner);

        vRectItem := vCarteItem as THCCustomRectItem;
        Result := vRectItem.InsertText(AText);
        if vRectItem.SizeChanged then
        begin
          vRectItem.SizeChanged := False;
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
        end;
      end
      else  // 其后或其前
      if SelectInfo.StartItemOffset = OffsetAfter then  // 在其后输入内容
      begin
        if (vCarteItemNo < Items.Count - 1)
          and (Items[vCarteItemNo + 1].StyleNo > THCStyle.Null)
          and (not Items[vCarteItemNo + 1].ParaFirst)
        then  // 下一个是TextItem且不是段首，则合并到下一个开始
        begin
          Inc(vCarteItemNo);
          SelectInfo.StartItemNo := vCarteItemNo;
          SelectInfo.StartItemOffset := 0;
          Self.Style.CurStyleNo := Items[vCarteItemNo].StyleNo;
          Result := DoTextItemInsert;  // 在下一个TextItem前面插入
        end
        else  // 最后或下一个还是RectItem或当前是段尾
        begin
          vNewItem := CreateDefaultTextItem;
          vNewItem.Text := AText;
          SelectInfo.StartItemNo := vCarteItemNo + 1;
          Result := InsertItem(SelectInfo.StartItemNo, vNewItem, False);  // 在两个RectItem中间插入
        end;
      end
      else  // 在其前输入内容
      begin
        if (vCarteItemNo > 0)
          and (Items[vCarteItemNo - 1].StyleNo > THCStyle.Null)
          and (not Items[vCarteItemNo].ParaFirst)
        then  // 前一个是TextItem，当前不是段首，合并到前一个尾
        begin
          Dec(vCarteItemNo);
          SelectInfo.StartItemNo := vCarteItemNo;
          SelectInfo.StartItemOffset := Items[vCarteItemNo].Length;
          Self.Style.CurStyleNo := Items[vCarteItemNo].StyleNo;
          Result := DoTextItemInsert  // 在前一个后面插入
        end
        else  // 最前或前一个还是RectItem
        begin
          vNewItem := CreateDefaultTextItem;
          vNewItem.Text := AText;
          Result := InsertItem(SelectInfo.StartItemNo, vNewItem, True);  // 在两个RectItem中间插入
        end;
      end;
    end
    else
      Result := DoTextItemInsert;
  end
  else
    Result := InsertBreak;  // 空的按回车新行处理
end;

procedure THCCustomRichData.DoItemInsert(const AItem: THCCustomItem);
begin
  if Assigned(FOnInsertItem) then
    FOnInsertItem(AItem);
end;

procedure THCCustomRichData.DoItemMouseEnter(const AItemNo: Integer);
begin
  Items[AItemNo].MouseEnter;
end;

procedure THCCustomRichData.DoItemMouseLeave(const AItemNo: Integer);
begin
  Items[AItemNo].MouseLeave;
end;

procedure THCCustomRichData.DoItemResized(const AItemNo: Integer);
begin
  if Assigned(FOnItemResized) then
    FOnItemResized(Self, AItemNo);
end;

function THCCustomRichData.EmptyDataInsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := False;

  if (AItem.StyleNo > THCStyle.Null) and (AItem.Text = '') then Exit;

  Undo_DeleteItem(0, 0);
  Items.Clear;
  DrawItems.Clear;
  AItem.ParaFirst := True;
  Items.Add(AItem);
  Undo_InsertItem(0, 0);

  // 因FormatItemPrepare及ReFormatData_会标记第一个的DrawItem为-1，
  // 调用FormatData时_FormatItemToDrawItems里Inc后
  FormatData(0, 0);
  ReSetSelectAndCaret(0);
  Result := True;
end;

function THCCustomRichData.EnableUndo: Boolean;
begin
  Result := Style.EnableUndo;
end;

procedure THCCustomRichData.Redo(const ARedo: THCCustomUndo);
begin
end;

procedure THCCustomRichData.ReFormat(const AStartItemNo: Integer);
begin
  if AStartItemNo > 0 then
  begin
    FormatItemPrepare(AStartItemNo, Items.Count - 1);
    FormatData(AStartItemNo, Items.Count - 1);
    DrawItems.DeleteFormatMark;
  end
  else  // 从0开始，适用于处理外部调用提供的方法(非内部操作)引起的Item变化且没处理Item对应的DrawItem的情况
  begin
    DrawItems.Clear;
    FormatData(0, Items.Count - 1);
  end;
end;

procedure THCCustomRichData.ReFormatActiveItem;
var
  vFormatFirstItemNo, vFormatLastItemNo: Integer;
begin
  if SelectInfo.StartItemNo >= 0 then
  begin
    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
  end;
end;

procedure THCCustomRichData.FormatData(const AStartItemNo, ALastItemNo: Integer);
var
  i, vPrioDrawItemNo: Integer;
  vPos: TPoint;
begin
  _FormatReadyParam(AStartItemNo, vPrioDrawItemNo, vPos);  // 格式化起始DrawItem序号和位置

  for i := AStartItemNo to ALastItemNo do  // 格式化
    _FormatItemToDrawItems(i, 1, FWidth, vPos, vPrioDrawItemNo);
end;

procedure THCCustomRichData.Clear;
var
  i: Integer;
begin
  if Items.Count > 0 then
  begin
    Undo_StartRecord;
    for i := Items.Count - 1 to 0 do
      Undo_DeleteItem(i, 0);
  end;

  InitializeField;

  inherited Clear;
  SetEmptyData;
end;

function THCCustomRichData.GetTopLevelDrawItem: THCCustomDrawItem;
var
  vItem: THCCustomItem;
begin
  Result := nil;
  vItem := GetCurItem;
  if vItem.StyleNo < THCStyle.Null then
    Result := (vItem as THCCustomRectItem).GetActiveDrawItem;

  if Result = nil then
    Result := GetCurDrawItem;
end;

function THCCustomRichData.GetActiveDrawItemCoord: TPoint;
var
  vItem: THCCustomItem;
  vDrawItem: THCCustomDrawItem;
  vPt: TPoint;
begin
  Result := Point(0, 0);
  vPt := Point(0, 0);
  vDrawItem := GetCurDrawItem;
  if vDrawItem <> nil then
  begin
    Result := vDrawItem.Rect.TopLeft;

    vItem := GetCurItem;
    if vItem.StyleNo < THCStyle.Null then
      vPt := (vItem as THCCustomRectItem).GetActiveDrawItemCoord;

    Result.X := Result.X + vPt.X;
    Result.Y := Result.Y + vPt.Y;
  end;
end;

function THCCustomRichData.GetTopLevelItem: THCCustomItem;
begin
  Result := GetCurItem;
  if (Result <> nil) and (Result.StyleNo < THCStyle.Null) then
    Result := (Result as THCCustomRectItem).GetActiveItem;
end;

function THCCustomRichData.GetHeight: Cardinal;
begin
  Result := CalcContentHeight;
end;

function THCCustomRichData.GetHint: string;
begin
  if (not FMouseMoveRestrain) and (FMouseMoveItemNo >= 0) then
    Result := Items[FMouseMoveItemNo].GetHint
  else
    Result := '';
end;

procedure THCCustomRichData.GetReformatItemRange(var AFirstItemNo,
  ALastItemNo: Integer; const AItemNo, AItemOffset: Integer);
//var
//  vDrawItemNo, vParaFirstDItemNo: Integer;
begin
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
  if (AItemNo > 0)
    and DrawItems[Items[AItemNo].FirstDItemNo].LineFirst
    and (AItemOffset = 0)
    //and ((Items[AItemNo].StyleNo < THCStyle.RsNull) or (AItemOffset = 0))
  then  // 在开头
  begin
    if not Items[AItemNo].ParaFirst then  // 不是段首
      AFirstItemNo := GetLineFirstItemNo(AItemNo - 1, Items[AItemNo - 1].Length)
    else  // 是段首
      AFirstItemNo := AItemNo;
  end
  else
    AFirstItemNo := GetLineFirstItemNo(AItemNo, 0);  // 取行第一个DrawItem对应的ItemNo

  ALastItemNo := GetParaLastItemNo(AItemNo);
end;

function THCCustomRichData.GetTopLevelData: THCCustomRichData;
begin
  Result := nil;
  if (SelectInfo.StartItemNo >= 0) and (SelectInfo.EndItemNo < 0) then
  begin
    if (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)
      and (SelectInfo.StartItemOffset = OffsetInner)
    then
      Result := (Items[SelectInfo.StartItemNo] as THCCustomRectItem).GetActiveData as THCCustomRichData;
  end;
  if Result = nil then
    Result := Self;
end;

function THCCustomRichData.GetTopLevelDataAt(const X,
  Y: Integer): THCCustomRichData;
var
  vItemNo, vOffset, vDrawItemNo, vX, vY: Integer;
  vRestrain: Boolean;
begin
  Result := nil;
  GetItemAt(X, Y, vItemNo, vOffset, vDrawItemNo, vRestrain);
  if (not vRestrain) and (vItemNo >= 0) then
  begin
    if Items[vItemNo].StyleNo < THCStyle.Null then
    begin
      CoordToItemOffset(X, Y, vItemNo, vOffset, vX, vY);
      Result := (Items[vItemNo] as THCCustomRectItem).GetTopLevelDataAt(vX, vY) as THCCustomRichData;
    end;
  end;
  if Result = nil then
    Result := Self;
end;

procedure THCCustomRichData.GetReformatItemRange(var AFirstItemNo,
  ALastItemNo: Integer);
begin
  GetReformatItemRange(AFirstItemNo, ALastItemNo, SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
end;

function THCCustomRichData.GetWidth: Cardinal;
begin
  Result := FWidth;
end;

function THCCustomRichData.ActiveTableDeleteCurCol: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).DeleteCurCol;
    end);
end;

function THCCustomRichData.ActiveTableDeleteCurRow: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).DeleteCurRow;
    end);
end;

function THCCustomRichData.ActiveTableSplitCurCol: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).SplitCurCol;
    end);
end;

function THCCustomRichData.ActiveTableSplitCurRow: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).SplitCurRow;
    end);
end;

procedure THCCustomRichData.AddData(const ASrcData: THCCustomData);
var
  i, vAddStartNo: Integer;
  vItem: THCCustomItem;
begin
  Self.InitializeField;

  if Self.Items.Last.CanConcatItems(ASrcData.Items.First) then
  begin
    Self.Items.Last.Text := Self.Items.Last.Text + ASrcData.Items.First.Text;
    vAddStartNo := 1;
  end
  else
    vAddStartNo := 0;

  for i := vAddStartNo to ASrcData.Items.Count - 1 do
  begin
    if (ASrcData.Items[i].StyleNo < THCStyle.Null)
      or ((ASrcData.Items[i].StyleNo > THCStyle.Null) and (ASrcData.Items[i].Text <> ''))
    then
    begin
      vItem := CreateItemByStyle(ASrcData.Items[i].StyleNo);
      vItem.Assign(ASrcData.Items[i]);
      //vItem.ParaFirst := False;  // 需要考虑合并
      vItem.Active := False;
      vItem.DisSelect;
      Self.Items.Add(vItem);
    end;
  end;
end;

function THCCustomRichData.ApplySelectParaStyle(
  const AMatchStyle: THCParaMatch): Integer;
var
  vFirstNo, vLastNo: Integer;

  procedure DoApplyParaStyle(const AItemNo: Integer);
  var
    i, vParaNo: Integer;
  begin
    if GetItemStyle(AItemNo) < THCStyle.Null then  // 当前是RectItem
      (Items[AItemNo] as THCCustomRectItem).ApplySelectParaStyle(Self.Style, AMatchStyle)
    else
    begin
      GetParaItemRang(AItemNo, vFirstNo, vLastNo);
      vParaNo := AMatchStyle.GetMatchParaNo(Self.Style, GetItemParaStyle(AItemNo));
      if GetItemParaStyle(vFirstNo) <> vParaNo then
      begin
        for i := vFirstNo to vLastNo do
          Items[i].ParaNo := vParaNo;
      end;
    end;
  end;

  procedure ApplyParaSelectedRangStyle;
  var
    i: Integer;
  begin
    // 先处理起始位置所在的段，以便后面遍历时减少循环次数
    GetParaItemRang(SelectInfo.StartItemNo, vFirstNo, vLastNo);
    DoApplyParaStyle(SelectInfo.StartItemNo);

    i := vLastNo + 1; // 从当前段的下一个item开始
    while i <= SelectInfo.EndItemNo do  // 小于结束位置
    begin
      if Items[i].ParaFirst then
        DoApplyParaStyle(i);
      Inc(i);
    end;
  end;

var
  vFormatFirstItemNo, vFormatLastItemNo: Integer;
begin
  if SelectInfo.StartItemNo < 0 then Exit;

  //GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
  vFormatFirstItemNo := GetParaFirstItemNo(SelectInfo.StartItemNo);

  if SelectInfo.EndItemNo >= 0 then  // 有选中内容
  begin
    vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    ApplyParaSelectedRangStyle;
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
  end
  else  // 没有选中内容
  begin
    vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    DoApplyParaStyle(SelectInfo.StartItemNo);  // 应用样式
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
  end;
  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
end;

function THCCustomRichData.ApplySelectTextStyle(
  const AMatchStyle: THCStyleMatch): Integer;

  {$REGION ' MergeItemToPrio 当前Item成功合并到同段前一个Item '}
  function MergeItemToPrio(const AItemNo: Integer): Boolean;
  begin
    Result := (AItemNo > 0) and (not Items[AItemNo].ParaFirst)
              and MergeItemText(Items[AItemNo - 1], Items[AItemNo]);
  end;
  {$ENDREGION}

  {$REGION ' MergeItemToNext 同段后一个Item成功合并到当前Item '}
  function MergeItemToNext(const AItemNo: Integer): Boolean;
  begin
    Result := (AItemNo < Items.Count - 1) and (not Items[AItemNo + 1].ParaFirst)
              and MergeItemText(Items[AItemNo], Items[AItemNo + 1]);
  end;
  {$ENDREGION}

var
  vStyleNo, vExtraCount, vLen: Integer;
  vItem: THCCustomItem;
  vText, vSelText: string;

  {$REGION 'ApplySameItem选中在同一个Item'}
  procedure ApplySameItem(const AItemNo: Integer);
  var
    vsBefor: string;
    vSelItem, vAfterItem: THCCustomItem;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // 非文本
    begin
      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
      {Rect的暂时先不处理
      if AMatchStyle is TTextStyleMatch then
      begin
        if (AMatchStyle as TTextStyleMatch).FontStyle = TFontStyleEx.tsCustom then
          DoApplyCustomStyle(vItem);
      end;}
    end
    else  // 文本
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);
      if vItem.IsSelectComplate then  // Item全部被选中了
      begin
        vItem.StyleNo := vStyleNo;  // 直接修改样式编号
        if MergeItemToNext(AItemNo) then  // 后一个Item合并到当前Item
        begin
          Items.Delete(AItemNo + 1);
          Dec(vExtraCount);
        end;
        if AItemNo > 0 then  // 向前合并
        begin
          vLen := Items[AItemNo - 1].Length;
          if MergeItemToPrio(AItemNo) then  // 当前Item合并到上一个Item(如果上面合并了，vItem已经失效，不能直接使用了)
          begin
            Items.Delete(AItemNo);
            Dec(vExtraCount);
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            SelectInfo.StartItemOffset := vLen;
            SelectInfo.EndItemNo := SelectInfo.StartItemNo;
            SelectInfo.EndItemOffset := vLen + SelectInfo.EndItemOffset;
          end;
        end;
      end
      else  // Item一部分被选中
      begin
        vText := vItem.Text;
        vSelText := Copy(vText, SelectInfo.StartItemOffset + 1,  // 选中的文本
          SelectInfo.EndItemOffset - SelectInfo.StartItemOffset);
        vsBefor := Copy(vText, 1, SelectInfo.StartItemOffset);  // 前半部分文本
        vAfterItem := Items[AItemNo].BreakByOffset(SelectInfo.EndItemOffset);  // 后半部分对应的Item
        if vAfterItem <> nil then  // 选择结束位置不在Item最后，则创建后面的独立Item
        begin
          Items.Insert(AItemNo + 1, vAfterItem);
          Inc(vExtraCount);
        end;

        if vsBefor <> '' then  // 选择起始位置不在Item最开始，保留前半部分，以新Item插入选中部分
        begin
          vItem.Text := vsBefor;  // 保留前半部分文本

          // 创建选中文本对应的Item
          vSelItem := CreateDefaultTextItem;
          vSelItem.ParaNo := vItem.ParaNo;
          vSelItem.StyleNo := vStyleNo;
          vSelItem.Text := vSelText;

          if vAfterItem <> nil then  // 有后半部分，中间的是新样式，前后肯定不能合并
          begin
            Items.Insert(AItemNo + 1, vSelItem);
            Inc(vExtraCount);
          end
          else  // 没有后半部分，说明选中需要和后面判断合并
          begin
            if (AItemNo < Items.Count - 1)
              and (not Items[AItemNo + 1].ParaFirst)
              and MergeItemText(vSelItem, Items[AItemNo + 1])
            then
            begin
              Items[AItemNo + 1].Text := vSelText + Items[AItemNo + 1].Text;
              SelectInfo.StartItemNo := AItemNo + 1;
              SelectInfo.StartItemOffset := 0;
              SelectInfo.EndItemNo := AItemNo + 1;
              SelectInfo.EndItemOffset := Length(vSelText);

              Exit;
            end;
            Items.Insert(AItemNo + 1, vSelItem);
            Inc(vExtraCount);
          end;

          SelectInfo.StartItemNo := AItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          SelectInfo.EndItemNo := AItemNo + 1;
          SelectInfo.EndItemOffset := Length(vSelText);
        end
        else  // 选择起始位置是Item最开始
        begin
          //vItem.Text := vSelText;  // BreakByOffset已经保留选中部分文本
          vItem.StyleNo := vStyleNo;

          if MergeItemToPrio(AItemNo) then // 当前Item合并到上一个Item
          begin
            Items.Delete(AItemNo);
            Dec(vExtraCount);

            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            vLen := Items[SelectInfo.StartItemNo].Length;
            SelectInfo.StartItemOffset := vLen - Length(vSelText);
            SelectInfo.EndItemNo := SelectInfo.StartItemNo;
            SelectInfo.EndItemOffset := vLen;
          end;
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'ApplyStartItem选中在不同Item中，处理选中起始Item'}
  procedure ApplyRangeStartItem(const AItemNo: Integer);
  var
    vAfterItem: THCCustomItem;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // 非文本
      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle)
    else  // 文本
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);

      if vItem.StyleNo <> vStyleNo then
      begin
        if vItem.IsSelectComplate then  // Item全选中了
          vItem.StyleNo := vStyleNo
        else  // Item部分选中
        begin
          vAfterItem := Items[AItemNo].BreakByOffset(SelectInfo.StartItemOffset);  // 后半部分对应的Item
          vAfterItem.StyleNo := vStyleNo;
          Items.Insert(AItemNo + 1, vAfterItem);
          Inc(vExtraCount);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          SelectInfo.EndItemNo := SelectInfo.EndItemNo + 1;
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'ApplyEndItem选中在不同Item中，处理选中结束Item'}
  procedure ApplyRangeEndItem(const AItemNo: Integer);
  var
    vBeforItem: THCCustomItem;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // 非文本
      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle)
    else  // 文本
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);

      if vItem.StyleNo <> vStyleNo then
      begin
        if vItem.IsSelectComplate then  // Item全选中了
          vItem.StyleNo := vStyleNo
        else  // Item部分选中了
        begin
          vText := vItem.Text;
          vSelText := Copy(vText, 1, SelectInfo.EndItemOffset); // 选中的文本
          Delete(vText, 1, SelectInfo.EndItemOffset);
          vItem.Text := vText;

          vBeforItem := CreateDefaultTextItem;
          vBeforItem.ParaNo := vItem.ParaNo;
          vBeforItem.StyleNo := vStyleNo;
          vBeforItem.Text := vSelText;  // 创建前半部分文本对应的Item
          vBeforItem.ParaFirst := vItem.ParaFirst;
          vItem.ParaFirst := False;

          Items.Insert(AItemNo, vBeforItem);
          Inc(vExtraCount);
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'ApplyNorItem选中在不同Item，处理中间Item'}
  procedure ApplyRangeNorItem(const AItemNo: Integer);
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // 非文本
      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle)
    else  // 文本
      vItem.StyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);
  end;
  {$ENDREGION}

var
  i, vFormatFirstItemNo, vFormatLastItemNo, vMStart, vMEnd: Integer;
begin
  Self.InitializeField;
  vExtraCount := 0;

  GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
  if not SelectExists then  // 没有选中
  begin
    if Style.CurStyleNo > THCStyle.Null then  // (容错)不在不支持文本样式的RectItem上，如果支持点击其上赋值Style.CurStyleNo为确定的文本样式
    begin
      AMatchStyle.Append := not AMatchStyle.StyleHasMatch(Style, Style.CurStyleNo);  // 根据当前判断是添加样式还是减掉样式
      Style.CurStyleNo := AMatchStyle.GetMatchStyleNo(Style, Style.CurStyleNo);

      Style.UpdateInfoRePaint;
      if Items[SelectInfo.StartItemNo].Length = 0 then  // 空行，改变当前光标处样式
      begin
        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
        Items[SelectInfo.StartItemNo].StyleNo := Style.CurStyleNo;
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
        Style.UpdateInfoReCaret;
      end
      else  // 不是空行
      begin
        if Items[SelectInfo.StartItemNo] is THCTextRectItem then
        begin
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
          (Items[SelectInfo.StartItemNo] as THCTextRectItem).TextStyleNo := Style.CurStyleNo;
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
        end;

        Style.UpdateInfoReCaret(False);
      end;
    end;

    Exit;
  end;

  if SelectInfo.EndItemNo < 0 then  // 没有连续选中内容
  begin
    if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
    begin
      // 如果改变会引起RectItem宽度变化，则需要格式化到最后一个Item
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
      (Items[SelectInfo.StartItemNo] as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
    end;
  end
  else  // 有连续选中内容
  begin
    vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

    for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
    begin
      if Items[i].StyleNo > THCStyle.Null then
      begin
        AMatchStyle.Append := not AMatchStyle.StyleHasMatch(Style, Items[i].StyleNo);  // 根据第一个判断是添加样式还是减掉样式
        Break;
      end
      else
      if Items[i] is THCTextRectItem then
      begin
        AMatchStyle.Append := not AMatchStyle.StyleHasMatch(Style, (Items[i] as THCTextRectItem).TextStyleNo);  // 根据第一个判断是添加样式还是减掉样式
        Break;
      end;
    end;

    if SelectInfo.StartItemNo = SelectInfo.EndItemNo then  // 选中发生在同一Item中
      ApplySameItem(SelectInfo.StartItemNo)
    else  // 选中发生在不同的Item，采用先处理选中范围内样式改变，再处理合并，再处理选中内容全、部分选中状态
    begin
      ApplyRangeEndItem(SelectInfo.EndItemNo);
      for i := SelectInfo.EndItemNo - 1 downto SelectInfo.StartItemNo + 1 do
        ApplyRangeNorItem(i);  // 处理每一个Item的样式
      ApplyRangeStartItem(SelectInfo.StartItemNo);

      { 样式变化后，从后往前处理选中范围内变化后的合并 }
      //if (SelectInfo.EndItemNo < Items.Count - 1) and (not Items[SelectInfo.EndItemNo + 1].ParaFirst) then  // 选中最后面的下一个不是段首
      if SelectInfo.EndItemNo < vFormatLastItemNo + vExtraCount then  // 选中最后面的下一个不是段首
      begin
        if MergeItemToNext(SelectInfo.EndItemNo) then
        begin
          Items.Delete(SelectInfo.EndItemNo + 1);
          Dec(vExtraCount);
        end;
      end;

      for i := SelectInfo.EndItemNo downto SelectInfo.StartItemNo + 1 do
      begin
        vLen := Items[i - 1].Length;
        if MergeItemToPrio(i) then  // 合并到前一个
        begin
          Items.Delete(i);
          Dec(vExtraCount);

          if i = SelectInfo.EndItemNo then  // 只在合并了选中最后Item才加长偏移
            SelectInfo.EndItemOffset := SelectInfo.EndItemOffset + vLen;
          SelectInfo.EndItemNo := SelectInfo.EndItemNo - 1;
        end;
      end;

      // 起始范围
      if (SelectInfo.StartItemNo > 0) and (not Items[SelectInfo.StartItemNo].ParaFirst) then  // 选中最前面不是段的第一个Item
      begin
        vLen := Items[SelectInfo.StartItemNo - 1].Length;
        if MergeItemToPrio(SelectInfo.StartItemNo) then  // 合并到前一个
        begin
          Items.Delete(SelectInfo.StartItemNo);
          Dec(vExtraCount);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := vLen;
          SelectInfo.EndItemNo := SelectInfo.EndItemNo - 1;
          if SelectInfo.StartItemNo = SelectInfo.EndItemNo then  // 选中的都合并成一个了
            SelectInfo.EndItemOffset := SelectInfo.EndItemOffset + vLen;
        end;
      end;
    end;

    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + vExtraCount, vExtraCount);
  end;

  MatchItemSelectState;

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
end;

function THCCustomRichData.CalcContentHeight: Integer;
begin
  if DrawItems.Count > 0 then
    Result := DrawItems[DrawItems.Count - 1].Rect.Bottom - DrawItems[0].Rect.Top
  else
    Result := 0;
end;

function THCCustomRichData.CanDeleteItem(const AItemNo: Integer): Boolean;
begin
  Result := CanEdit;
end;

function THCCustomRichData.CanEdit: Boolean;
begin
  Result := not FReadOnly;
  if not Result then
    Beep; //MessageBeep(MB_OK);
end;

function THCCustomRichData.CheckInsertItemCount(const AStartNo,
  AEndNo: Integer): Integer;
begin
  Result := AEndNo - AStartNo + 1;  // 默认原数返回
end;

procedure THCCustomRichData.InitializeField;
begin
  InitializeMouseField;
  inherited InitializeField;
end;

procedure THCCustomRichData.InitializeMouseField;
begin
  FMouseLBDowning := False;
  FMouseDownItemNo := -1;
  FMouseDownItemOffset := -1;
  FMouseMoveItemNo := -1;
  FMouseMoveItemOffset := -1;
  FMouseMoveRestrain := False;
  FSelecting := False;
  FDraging := False;
end;

function THCCustomRichData.InsertBreak: Boolean;
var
  vKey: Word;
begin
  Result := False;
  if not CanEdit then Exit;

  vKey := VK_RETURN;
  KeyDown(vKey, []);
  InitializeMouseField;  // 201807311101
  Result := True;
end;

function THCCustomRichData.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem; const AOffsetBefor: Boolean = True): Boolean;
var
  vFormatFirstItemNo, vFormatLastItemNo, vInsPos, vIncCount: Integer;
  vMerged: Boolean;
begin
  Result := False;

  if not CanEdit then Exit;

  //-------- 在指定的Index处插入Item --------//

  //DeleteSelection;  // 如果正在选中区域中那么删除后AIndex会越界，所以调用此方法前需要处于未选中状态
  AItem.ParaNo := Style.CurParaNo;
  {if AItem is THCTextItem then  // 插入TextItem Item创建时处理好当前样式了
  begin
    if Style.CurStyleNo > THCStyle.RsNull then  // 在RectItem后面或前面插入TextItem
      AItem.StyleNo := Style.CurStyleNo
    else
      AItem.StyleNo := 0;
  end;}

  if IsEmptyData then
  begin
    Undo_StartRecord;
    Result := EmptyDataInsertItem(AItem);
    Exit;
  end;

  {说明：当插入位置不是最后一个且插入位置是段起始，那么可能是在上一段最后插入，
   也可能是要在下一段最前页插入，这时以AItem的ParaFirst属性为判断依据}

  vIncCount := 0;
  Undo_StartRecord;
  if AItem.StyleNo < THCStyle.Null then  // 插入RectItem
  begin
    vInsPos := AIndex;
    if AIndex < Items.Count then  // 不是在最后添加一个Item
    begin
      if AOffsetBefor then  // 在原位置Item前面插入
      begin
        GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex, 0);
        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
        {if not AItem.ParaFirst then  // 没标明是否段起始，根据环境自适应
        begin
          AItem.ParaFirst := Items[AIndex].ParaFirst;
          if Items[AIndex].ParaFirst then  // 下一段开始变为非开始，如果要独立为一段去掉此判断即可
          begin
            Undo_ItemParaFirst(AIndex, 0, False);
            Items[AIndex].ParaFirst := False;
          end;
        end;}

        if (Items[AIndex].StyleNo > THCStyle.Null) and (Items[AIndex].Text = '') then  // 插入位置处是空行，替换当前
        begin
          AItem.ParaFirst := True;
          Undo_DeleteItem(AIndex, 0);
          Items.Delete(AIndex);
          Dec(vIncCount);
        end
        else  // 插入位置不是空行
        if not AItem.ParaFirst then  // 没标明是否段起始，根据环境自适应
        begin
          AItem.ParaFirst := Items[AIndex].ParaFirst;
          if Items[AIndex].ParaFirst then  // 下一段开始变为非开始，如果要独立为一段去掉此判断即可
          begin
            Undo_ItemParaFirst(AIndex, 0, False);
            Items[AIndex].ParaFirst := False;
          end;
        end;
      end
      else  // 在某Item后面插入
      begin
        if (AIndex > 0)
          and (Items[AIndex - 1].StyleNo > THCStyle.Null)
          and (Items[AIndex - 1].Text = '')
        then  // 插入位置处前一个是空行，替换当前
        begin
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex - 1, 0);
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

          AItem.ParaFirst := True;
          Undo_DeleteItem(AIndex - 1, 0);
          Items.Delete(AIndex - 1);
          Dec(vIncCount);
          Dec(vInsPos);
        end
        else  // 插入位置前一个不是空行
        begin
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex, 0);
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
        end;
      end;
    end
    else  // 在末尾添加一个新Item
    begin
      vFormatFirstItemNo := AIndex - 1;
      vFormatLastItemNo := AIndex - 1;
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

      if (not AItem.ParaFirst)  // 插入不是另起一段
        and (Items[AIndex - 1].StyleNo > THCStyle.Null)  // 前面是TextItem
        and (Items[AIndex - 1].Text = '')  // 空行
      then  // 插入位置处是空行，替换当前
      begin
        AItem.ParaFirst := True;
        Undo_DeleteItem(AIndex - 1, 0);
        Items.Delete(AIndex - 1);
        Dec(vIncCount);
        Dec(vInsPos);
      end;
    end;

    Items.Insert(vInsPos, AItem);
    Undo_InsertItem(vInsPos, OffsetAfter);
    Inc(vIncCount);

    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + vIncCount, vIncCount);
    ReSetSelectAndCaret(vInsPos);
  end
  else  // 插入文本Item
  begin
    vMerged := False;

    if AIndex > 0 then  // 不是在第一个位置
    begin
      if AIndex < Items.Count then
      begin
        if not Items[AIndex].ParaFirst then
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex - 1, 0)
        else
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex, 0);
      end
      else  // 在最后面追加
        GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex - 1, 0)
    end
    else
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, 0, 0);

    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

    if not AItem.ParaFirst then  // 新插入的不另起一段，判断和当前位置的关系
    begin
      // 在2个Item中间插入一个Item，需要同时判断和前后能否合并
      if AOffsetBefor then  // 在Item前面插入
      begin
        if (AIndex < Items.Count) and (Items[AIndex].CanConcatItems(AItem)) then  // 先判断和当前位置处能否合并
        begin
          Undo_InsertText(AIndex, 1, AItem.Text);  // 201806261644
          Items[AIndex].Text := AItem.Text + Items[AIndex].Text;

          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex);

          vMerged := True;
        end
        else
        if (not Items[AIndex].ParaFirst) and (AIndex > 0) and Items[AIndex - 1].CanConcatItems(AItem) then   // 再判断和前一个能否合并
        begin
          Undo_InsertText(AIndex - 1, Items[AIndex - 1].Length + 1, AItem.Text);  // 201806261650
          Items[AIndex - 1].Text := Items[AIndex - 1].Text + AItem.Text;

          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex - 1);

          vMerged := True;
        end;
      end
      else  // 在Item后面插入
      begin
        if (AIndex > 0) and Items[AIndex - 1].CanConcatItems(AItem) then   // 先判断和前一个能否合并
        begin
          Undo_InsertText(AIndex - 1, Items[AIndex - 1].Length + 1, AItem.Text);  // 201806261650
          Items[AIndex - 1].Text := Items[AIndex - 1].Text + AItem.Text;

          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex - 1);

          vMerged := True;
        end
        else
        if (AIndex < Items.Count) and (not Items[AIndex].ParaFirst) and (Items[AIndex].CanConcatItems(AItem)) then  // 先判断和当前位置处能否合并
        begin
          Undo_InsertText(AIndex, 1, AItem.Text);  // 201806261644
          Items[AIndex].Text := AItem.Text + Items[AIndex].Text;

          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex, AItem.Length);

          vMerged := True;
        end;
      end;
    end;

    if not vMerged then  // 不能和插入位置前及插入位置处的Item合并
    begin
      if AOffsetBefor and (not AItem.ParaFirst) then  // 在原位置Item前面插入，且没标明是否段起始，根据环境自适应
      begin
        AItem.ParaFirst := Items[AIndex].ParaFirst;
        if Items[AIndex].ParaFirst then  // 下一段开始变为非开始，如果要独立为一段去掉此判断即可
        begin
          Undo_ItemParaFirst(AIndex, 0, False);
          Items[AIndex].ParaFirst := False;
        end;
      end;

      Items.Insert(AIndex, AItem);
      Undo_InsertItem(AIndex, 0);
      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);

      ReSetSelectAndCaret(AIndex);
    end;
  end;

  Result := True;
end;

function THCCustomRichData.InsertLine(const ALineHeight: Integer): Boolean;
var
  vItem: TLineItem;
  vData: THCCustomRichData;
begin
  Result := False;

  if not CanEdit then Exit;

  vData := GetTopLevelData;
  vItem := TLineItem.Create(vData, vData.Width, 21);
  vItem.LineHeght := ALineHeight;

  Result := InsertItem(vItem);
  InitializeMouseField;  // 201807311101
end;

function THCCustomRichData.TableInsertColAfter(const AColCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).InsertColAfter(AColCount);
    end);
end;

function THCCustomRichData.TableInsertColBefor(const AColCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).InsertColBefor(AColCount);
    end);
end;

function THCCustomRichData.TableInsertRC(const AProc: TInsertProc): Boolean;
var
  vCurItemNo, vFormatFirstItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;
  vCurItemNo := GetCurItemNo;
  if Items[vCurItemNo] is THCTableItem then
  begin
    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, vCurItemNo, 0);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    Result := AProc(Items[vCurItemNo]);
    if Result then
    begin
      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
    end;

    InitializeMouseField;  // 201807311101
  end;
end;

function THCCustomRichData.TableInsertRowAfter(const ARowCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).InsertRowAfter(ARowCount);
    end);
end;

function THCCustomRichData.TableInsertRowBefor(const ARowCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).InsertRowBefor(ARowCount);
    end);
end;

procedure THCCustomRichData.Undo(const AUndo: THCCustomUndo);
begin
end;

procedure THCCustomRichData.Undo_DeleteItem(const AItemNo, AOffset: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vItemAction: THCItemUndoAction;
begin
  if EnableUndo then
  begin
    vUndoList := GetUndoList;
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := vUndo.ActionAppend(uatDeleteItem, AItemNo, AOffset) as THCItemUndoAction;
      SaveItemToStreamAlone(Items[AItemNo], vItemAction.ItemStream);
    end;
  end;
end;

procedure THCCustomRichData.Undo_InsertItem(const AItemNo, AOffset: Integer);
var
  vUndo: THCUndo;
  vItemAction: THCItemUndoAction;
begin
  if EnableUndo then
  begin
    vUndo := GetUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := vUndo.ActionAppend(uatInsertItem, AItemNo, AOffset) as THCItemUndoAction;
      SaveItemToStreamAlone(Items[AItemNo], vItemAction.ItemStream);
    end;
  end;
end;

function THCCustomRichData.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
var
  vInsPos, vFormatFirstItemNo, vFormatLastItemNo: Integer;
  vItem, vAfterItem: THCCustomItem;
  i, vItemCount, vStyleNo, vOffsetStart, vInsetLastNo, vCaretOffse: Integer;
  vInsertBefor: Boolean;
  vDataSize: Int64;
begin
  Result := False;

  if not CanEdit then Exit;

  vAfterItem := nil;
  vInsertBefor := False;

  Undo_StartGroup(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  try
    if Items.Count = 0 then  // 空
      vInsPos := 0
    else  // 有数据
    begin
      DeleteSelected;
      // 确定插入位置
      vInsPos := SelectInfo.StartItemNo;
      if Items[vInsPos].StyleNo < THCStyle.Null then  // RectItem
      begin
        if SelectInfo.StartItemOffset = OffsetInner then  // 其上
        begin
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, SelectInfo.StartItemNo, OffsetInner);
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
          Undo_StartRecord;
          Undo_ItemSelf(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
          Result := (Items[vInsPos] as THCCustomRectItem).InsertStream(AStream, AStyle, AFileVersion);
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);

          Exit;
        end
        else
        if SelectInfo.StartItemOffset = OffsetBefor then  // 其前
          vInsertBefor := True
        else  // 其后
          vInsPos := vInsPos + 1;
      end
      else  // TextItem
      begin
        // 先判断光标是否在最后，防止空Item时SelectInfo.StartItemOffset = 0按其前处理
        if SelectInfo.StartItemOffset = Items[vInsPos].Length then  // 其后
          vInsPos := vInsPos + 1
        else
        if SelectInfo.StartItemOffset = 0 then  // 其前
          vInsertBefor := Items[vInsPos].Length <> 0
        else  // 其中
        begin
          Undo_StartRecord;
          Undo_DeleteText(vInsPos, 1, Copy(Items[vInsPos].Text, 1, SelectInfo.StartItemOffset));

          vAfterItem := Items[vInsPos].BreakByOffset(SelectInfo.StartItemOffset);  // 后半部分对应的Item
          vInsPos := vInsPos + 1;
        end;
      end;
    end;

    AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));
    if vDataSize = 0 then Exit;

    AStream.ReadBuffer(vItemCount, SizeOf(vItemCount));
    if vItemCount = 0 then Exit;

    // 因为插入的第一个可能和插入位置前一个合并，插入位置可能是行首，所以要从插入位置
    // 行上一个开始格式化，为简单处理，直接使用段首，可优化为上一行首
    //GetParaItemRang(SelectInfo.StartItemNo, vFormatFirstItemNo, vFormatLastItemNo);

    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);

    // 计算格式化起始、结束ItemNo
    if Items.Count > 0 then  // 兼容Empty
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo)
    else
    begin
      vFormatFirstItemNo := 0;
      vFormatLastItemNo := -1;
    end;

    Undo_StartRecord;
    for i := 0 to vItemCount - 1 do
    begin
      AStream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));
      vItem := CreateItemByStyle(vStyleNo);
      if vStyleNo < THCStyle.Null then
        Undo_ItemSelf(i, 0);
      vItem.LoadFromStream(AStream, AStyle, AFileVersion);
      if AStyle <> nil then  // 有样式表
      begin
        if vItem.StyleNo > THCStyle.Null then
          vItem.StyleNo := Style.GetStyleNo(AStyle.TextStyles[vItem.StyleNo], True);
        vItem.ParaNo := Style.GetParaNo(AStyle.ParaStyles[vItem.ParaNo], True);
      end
      else  // 无样式表
      begin
        if vItem.StyleNo > THCStyle.Null then
          vItem.StyleNo := Style.CurStyleNo;
        vItem.ParaNo := Style.CurParaNo;
      end;

      if i = 0 then  // 插入的第一个Item
      begin
        if vInsertBefor then  // 第一个在某Item最前面插入(粘贴)
        begin
          vItem.ParaFirst := Items[vInsPos].ParaFirst;

          if Items[vInsPos].ParaFirst then
          begin
            Undo_ItemParaFirst(vInsPos, 0, False);
            Items[vInsPos].ParaFirst := False;
          end;
        end
        else
          vItem.ParaFirst := False
      end;

      Items.Insert(vInsPos + i, vItem);
      Undo_InsertItem(vInsPos + i, 0);
    end;

    vItemCount := CheckInsertItemCount(vInsPos, vInsPos + vItemCount - 1);  // 检查插入的Item是否合格并删除不合格

    vInsetLastNo := vInsPos + vItemCount - 1;  // 光标在最后一个Item
    vCaretOffse := GetItemAfterOffset(vInsetLastNo);  // 最后一个Item后面

    if vAfterItem <> nil then  // 插入操作是在Item中间，原Item补拆分成2个
    begin
      if MergeItemText(Items[vInsetLastNo], vAfterItem) then  // 插入最后一个和后半部分能合并
      begin
        Undo_InsertText(vInsetLastNo, Items[vInsetLastNo].Length - vAfterItem.Length + 1, vAfterItem.Text);

        FreeAndNil(vAfterItem);
      end
      else  // 插入最后一个和后半部分不能合并
      begin
        Items.Insert(vInsetLastNo + 1, vAfterItem);
        Undo_InsertItem(vInsetLastNo + 1, 0);

        Inc(vItemCount);
      end;
    end;

    if {(vInsPos > vFormatFirstItemNo) and} (vInsPos > 0) then  // 在格式化起始位置后插入且不是第0个位置
    begin
      if Items[vInsPos - 1].Length = 0 then  // 插入位置前面是空行Item
      begin
        Undo_ItemParaFirst(vInsPos, 0, Items[vInsPos - 1].ParaFirst);
        Items[vInsPos].ParaFirst := Items[vInsPos - 1].ParaFirst;

        Undo_DeleteItem(vInsPos - 1, 0);
        Items.Delete(vInsPos - 1);  // 删除空行

        Dec(vItemCount);
        Dec(vInsetLastNo);
      end
      else  // 插入位置前面不是空行Item
      begin
        vOffsetStart := Items[vInsPos - 1].Length;
        if (not Items[vInsPos].ParaFirst)
          and MergeItemText(Items[vInsPos - 1], Items[vInsPos])
        then  // 插入的和前面的合并
        begin
          Undo_InsertText(vInsPos - 1, Items[vInsPos - 1].Length + 1, Items[vInsPos].Text);
          Undo_DeleteItem(vInsPos, 0);

          if vItemCount = 1 then
            vCaretOffse := vOffsetStart + vCaretOffse;

          Items.Delete(vInsPos);
          Dec(vItemCount);
          Dec(vInsetLastNo);
        end;
      end;

      if (vInsetLastNo < Items.Count - 1)  // 插入最后Item和后面的能合并
        and (not Items[vInsetLastNo + 1].ParaFirst)
        and MergeItemText(Items[vInsetLastNo], Items[vInsetLastNo + 1])
      then
      begin
        Undo_DeleteItem(vInsetLastNo + 1, 0);

        Items.Delete(vInsetLastNo + 1);
        Dec(vItemCount);
      end;
    end
    else  // 在最开始第0个位置处插入
    //if (vInsetLastNo < Items.Count - 1) then
    begin
      if MergeItemText(Items[vInsetLastNo], Items[vInsetLastNo + 1 {vInsPos + vItemCount}]) then  // 可以和插入前0位置的合并
      begin
        Undo_DeleteItem(vInsPos + vItemCount, 0);

        Items.Delete(vInsPos + vItemCount);
        Dec(vItemCount);
      end;
    end;

    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + vItemCount, vItemCount);

    ReSetSelectAndCaret(vInsetLastNo, vCaretOffse);  // 选中插入内容最后Item位置
  finally
    Undo_EndGroup(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  end;

  InitializeMouseField;  // 201807311101

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
  Style.UpdateInfoReScroll;
end;

function THCCustomRichData.InsertItem(const AItem: THCCustomItem): Boolean;
var
  vCurItemNo: Integer;
  vFormatFirstItemNo, vFormatLastItemNo: Integer;
  vText, vsBefor, vsAfter: string;
  vAfterItem: THCCustomItem;
begin
  Result := False;

  if not CanEdit then Exit;

  DeleteSelected;

  AItem.ParaNo := Style.CurParaNo;
  {if AItem is THCTextItem then  // item创建时处理好当前样式了
  begin
    if Style.CurStyleNo > THCStyle.RsNull then  // 在RectItem后面或前面插入TextItem
      AItem.StyleNo := Style.CurStyleNo
    else
      AItem.StyleNo := 0;
  end;}

  if IsEmptyData then
  begin
    Undo_StartRecord;
    Result := EmptyDataInsertItem(AItem);
    Exit;
  end;
  vCurItemNo := GetCurItemNo;

  if Items[vCurItemNo].StyleNo < THCStyle.Null then  // 当前位置是 RectItem
  begin
    if SelectInfo.StartItemOffset = OffsetInner then  // 正在其上
    begin
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

      Undo_StartRecord;
      Undo_ItemSelf(vCurItemNo, OffsetInner);
      Result := (Items[vCurItemNo] as THCCustomRectItem).InsertItem(AItem);
      if Result then
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
    end
    else  // 其前or其后
    begin
      if SelectInfo.StartItemOffset = OffsetBefor then  // 其前
        Result := InsertItem(SelectInfo.StartItemNo, AItem)
      else  // 其后
        Result := InsertItem(SelectInfo.StartItemNo + 1, AItem, False);
    end;
  end
  else  // 当前位置是TextItem
  begin
    // 先判断是否在后面，这样对于空行插入时从后面插入，否则会造成空行向后积压
    if (SelectInfo.StartItemOffset = Items[vCurItemNo].Length) then  // 在TextItem最后插入
      Result := InsertItem(SelectInfo.StartItemNo + 1, AItem, False)
    else
    if SelectInfo.StartItemOffset = 0 then  // 在最前面插入
      Result := InsertItem(SelectInfo.StartItemNo, AItem)
    else  // 在Item中间
    begin
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

      vText := Items[vCurItemNo].Text;
      vsBefor := Copy(vText, 1, SelectInfo.StartItemOffset);  // 前半部分文本
      vsAfter := Copy(vText, SelectInfo.StartItemOffset + 1, Items[vCurItemNo].Length
        - SelectInfo.StartItemOffset);  // 后半部分文本

      Undo_StartRecord;
      if Items[vCurItemNo].CanConcatItems(AItem) then  // 能合并
      begin
        if AItem.ParaFirst then  // 新段
        begin
          Undo_DeleteText(vCurItemNo, SelectInfo.StartItemOffset + 1, vsAfter);
          Items[vCurItemNo].Text := vsBefor;
          AItem.Text := AItem.Text + vsAfter;

          vCurItemNo := vCurItemNo + 1;
          Items.Insert(vCurItemNo, AItem);
          Undo_InsertItem(vCurItemNo, 0);

          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 1);
          ReSetSelectAndCaret(vCurItemNo);
        end
        else  // 同一段中插入
        begin
          Undo_InsertText(vCurItemNo, SelectInfo.StartItemOffset + 1, AItem.Text);
          vsBefor := vsBefor + AItem.Text;
          Items[vCurItemNo].Text := vsBefor + vsAfter;

          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
          SelectInfo.StartItemNo := vCurItemNo;
          SelectInfo.StartItemOffset := Length(vsBefor);
          //CaretDrawItemNo := GetItemLastDrawItemNo(vCurItemNo);
        end;
      end
      else  // 不能合并
      begin
        Undo_DeleteText(vCurItemNo, SelectInfo.StartItemOffset + 1, vsAfter);
        vAfterItem := Items[vCurItemNo].BreakByOffset(SelectInfo.StartItemOffset);  // 后半部分对应的Item

        // 插入后半部分对应的Item
        vCurItemNo := vCurItemNo + 1;
        Items.Insert(vCurItemNo, vAfterItem);
        Undo_InsertItem(vCurItemNo, 0);
        // 插入新Item
        Items.Insert(vCurItemNo, AItem);
        Undo_InsertItem(vCurItemNo, 0);

        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 2, 2);
        ReSetSelectAndCaret(vCurItemNo);
      end;

      Result := True;
    end;
  end;
end;

function THCCustomRichData.InsertTable(const ARowCount,
  AColCount: Integer): Boolean;
var
  vItem: THCCustomItem;
  vTopData: THCCustomRichData;
begin
  Result := False;

  if not CanEdit then Exit;

  vTopData := GetTopLevelData;

  vItem := THCTableItem.Create(vTopData, ARowCount, AColCount, vTopData.Width);
  Result := InsertItem(vItem);
  InitializeMouseField;  // 201807311101
end;

function THCCustomRichData.InsertText(const AText: string): Boolean;
var
  vPCharStart, vPCharEnd, vPtr: PChar;
  vParaFirst: Boolean;
  vS: string;
  vTextItem: THCCustomItem;
begin
  Result := False;

  if not CanEdit then Exit;

  DeleteSelected;

  vParaFirst := False;
  vPCharStart := PChar(AText);
  vPCharEnd := vPCharStart + Length(AText);
  if vPCharStart = vPCharEnd then Exit;
  vPtr := vPCharStart;
  while vPtr < vPCharEnd do
  begin
    case vPtr^ of
      #13:
        begin
          System.SetString(vS, vPCharStart, vPtr - vPCharStart);

          if vParaFirst then
          begin
            vTextItem := CreateDefaultTextItem;
            vTextItem.ParaFirst := True;
            vTextItem.Text := vS;
            Result := InsertItem(vTextItem);
          end
          else
            Result := DoInsertText(vS);

          vParaFirst := True;

          Inc(vPtr);
          vPCharStart := vPtr;
          Continue;
        end;

      #10:
        begin
          Inc(vPtr);
          vPCharStart := vPtr;
          Continue;
        end;
    end;

    Inc(vPtr);
  end;

  System.SetString(vS, vPCharStart, vPtr - vPCharStart);

  Result := DoInsertText(vS);

  InitializeMouseField;  // 201807311101

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
  Style.UpdateInfoReScroll;
end;

function THCCustomRichData.IsSelectSeekStart: Boolean;
begin
  Result := (FSelectSeekNo = SelectInfo.StartItemNo) and
    (FSelectSeekOffset = SelectInfo.StartItemOffset);
end;

procedure THCCustomRichData.KeyDown(var Key: Word; Shift: TShiftState);

  {$REGION 'CheckSelectEndEff 判断选择结束是否和起始在同一位置，是则取消选中'}
  procedure CheckSelectEndEff;
  begin
    if (SelectInfo.StartItemNo = SelectInfo.EndItemNo)
      and (SelectInfo.StartItemOffset = SelectInfo.EndItemOffset)
    then
    begin
      Items[SelectInfo.EndItemNo].DisSelect;

      SelectInfo.EndItemNo := -1;
      SelectInfo.EndItemOffset := -1;
    end;
  end;
  {$ENDREGION}

  procedure SetSelectSeekStart;
  begin
    FSelectSeekNo := SelectInfo.StartItemNo;
    FSelectSeekOffset := SelectInfo.StartItemOffset;
  end;

  procedure SetSelectSeekEnd;
  begin
    FSelectSeekNo := SelectInfo.EndItemNo;
    FSelectSeekOffset := SelectInfo.EndItemOffset;
  end;

var
  vCurItem: THCCustomItem;
  vParaFirstItemNo, vParaLastItemNo: Integer;
  vFormatFirstItemNo, vFormatLastItemNo: Integer;
  vSelectExist: Boolean;

  {$REGION ' TABKeyDown 按键 '}
  procedure TABKeyDown;
  var
    vTabItem: TTabItem;
  begin
    vTabItem := TTabItem.Create(Self);
    if vCurItem.StyleNo < THCStyle.Null then  // 当前是RectItem
    begin
      if SelectInfo.StartItemOffset = OffsetInner then // 在其上
      begin
        if (vCurItem as THCCustomRectItem).WantKeyDown(Key, Shift) then  // 处理此键
        begin
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
          (vCurItem as THCCustomRectItem).KeyDown(Key, Shift);
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
        end;

        Exit;
      end;
    end;

    Self.InsertItem(vTabItem);
  end;
  {$ENDREGION}

  {$REGION ' LeftKeyDown 左方向键 '}
  procedure LeftKeyDown;

    procedure SelectPrio(var AItemNo, AOffset: Integer);
    begin
      if AOffset > 0 then  // 偏移不在最开始，当前Item往前
      begin
        if Items[AItemNo].StyleNo > THCStyle.Null then
          AOffset := AOffset - 1
        else
          AOffset := OffsetBefor;
      end
      else
      if AItemNo > 0then  // 在最开头，往前一个最后
      begin
        Items[AItemNo].DisSelect;
        AItemNo := AItemNo - 1;
        if Items[AItemNo].StyleNo < THCStyle.Null then
          AOffset := OffsetBefor
        else
          AOffset := Items[AItemNo].Length - 1;  // 倒数第1个前面
      end;
    end;

    procedure SelectStartItemPrio;
    var
      vItemNo, vOffset: Integer;
    begin
      vItemNo := SelectInfo.StartItemNo;
      vOffset := SelectInfo.StartItemOffset;
      SelectPrio(vItemNo, vOffset);
      SelectInfo.StartItemNo := vItemNo;
      SelectInfo.StartItemOffset := vOffset;
    end;

    procedure SelectEndItemPrio;
    var
      vItemNo, vOffset: Integer;
    begin
      vItemNo := SelectInfo.EndItemNo;
      vOffset := SelectInfo.EndItemOffset;
      SelectPrio(vItemNo, vOffset);
      SelectInfo.EndItemNo := vItemNo;
      SelectInfo.EndItemOffset := vOffset;
    end;

  var
    vNewCaretDrawItemNo: Integer;
  begin
    if Shift = [ssShift] then  // Shift+方向键选择
    begin
      if SelectInfo.EndItemNo >= 0 then  // 有选中内容
      begin
        if IsSelectSeekStart then  // 游标在选中起始
        begin
          SelectStartItemPrio;
          SetSelectSeekStart;
        end
        else  // 游标在选中结束
        begin
          SelectEndItemPrio;
          SetSelectSeekEnd;
        end;
      end
      else  // 没有选中
      begin
        if (SelectInfo.StartItemNo > 0) and (SelectInfo.StartItemOffset = 0) then  // 在Item最前面往前
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := GetItemAfterOffset(SelectInfo.StartItemNo);
        end;

        SelectInfo.EndItemNo := SelectInfo.StartItemNo;
        SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;

        SelectStartItemPrio;
        SetSelectSeekStart;
      end;

      CheckSelectEndEff;
      MatchItemSelectState;
      Style.UpdateInfoRePaint;
    end
    else  // 没有按下Shift
    begin
      if vSelectExist then  // 有选中内容
      begin
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // 无选中内容
      begin
        if SelectInfo.StartItemOffset <> 0 then  // 不在Item最开始
          SelectInfo.StartItemOffset := SelectInfo.StartItemOffset - 1
        else  // 在Item最开始左方向键
        begin
          if SelectInfo.StartItemNo > 0 then  // 不是第一个Item的最开始，往前面移动
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;  // 上一个
            SelectInfo.StartItemOffset := GetItemAfterOffset(SelectInfo.StartItemNo);

            if not DrawItems[Items[SelectInfo.StartItemNo + 1].FirstDItemNo].LineFirst then  // 移动前Item不是行起始
            begin
              KeyDown(Key, Shift);
              Exit;
            end;
          end
          else  // 在第一个Item最左面按下左方向键
            Key := 0;
        end;
      end;

      if Key <> 0 then
      begin
        vNewCaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
        if vNewCaretDrawItemNo <> CaretDrawItemNo then  // 换DrawItemNo了
        begin
          if (vNewCaretDrawItemNo = CaretDrawItemNo - 1)  // 移动到前一个了
            and (DrawItems[vNewCaretDrawItemNo].ItemNo = DrawItems[CaretDrawItemNo].ItemNo)  // 是同一个Item
            and (DrawItems[CaretDrawItemNo].LineFirst)  // 原是行首
            and (SelectInfo.StartItemOffset = DrawItems[CaretDrawItemNo].CharOffs - 1)  // 光标位置也是原DrawItem的最前面
          then
            // 不更换
          else
            CaretDrawItemNo := vNewCaretDrawItemNo;
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' RightKeyDown 右方向键，此处不会涉及表格，表格在RectItemKeyDown中处理了 '}
  procedure RightKeyDown;

    procedure SelectNext(var AItemNo, AOffset: Integer);
    begin
      if AOffset = GetItemAfterOffset(AItemNo) then  // 在Item最后，移动到下一个Item
      begin
        if AItemNo < Items.Count - 1 then
        begin
          Inc(AItemNo);

          if Items[AItemNo].StyleNo < THCStyle.Null then
            AOffset := OffsetAfter
          else
            AOffset := 1;
        end;
      end
      else  // 不在最后
      begin
        if Items[AItemNo].StyleNo < THCStyle.Null then
          AOffset := OffsetAfter
        else
          AOffset := AOffset + 1;
      end;
    end;

    procedure SelectStartItemNext;
    var
      vItemNo, vOffset: Integer;
    begin
      vItemNo := SelectInfo.StartItemNo;
      vOffset := SelectInfo.StartItemOffset;
      SelectNext(vItemNo, vOffset);
      SelectInfo.StartItemNo := vItemNo;
      SelectInfo.StartItemOffset := vOffset;
    end;

    procedure SelectEndItemNext;
    var
      vItemNo, vOffset: Integer;
    begin
      vItemNo := SelectInfo.EndItemNo;
      vOffset := SelectInfo.EndItemOffset;
      SelectNext(vItemNo, vOffset);
      SelectInfo.EndItemNo := vItemNo;
      SelectInfo.EndItemOffset := vOffset;
    end;

  var
    vNewCaretDrawItemNo: Integer;
  begin
    if Shift = [ssShift] then  // Shift+方向键选择
    begin
      if SelectInfo.EndItemNo >= 0 then  // 有选中内容
      begin
        if IsSelectSeekStart then  // 游标在选中起始
        begin
          SelectStartItemNext;
          SetSelectSeekStart;
        end
        else  // 游标在选中结束
        begin
          SelectEndItemNext;
          SetSelectSeekEnd;
        end;
      end
      else   // 没有选中
      begin
        if SelectInfo.StartItemNo < Items.Count - 1 then
        begin
          if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
          begin
            if SelectInfo.StartItemOffset = OffsetAfter then
            begin
              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              SelectInfo.StartItemOffset := 0;
            end;
          end
          else
          if SelectInfo.StartItemOffset = Items[SelectInfo.StartItemNo].Length then
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            SelectInfo.StartItemOffset := 0;
          end;
        end;

        SelectInfo.EndItemNo := SelectInfo.StartItemNo;
        SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;

        SelectEndItemNext;
        SetSelectSeekEnd;
      end;

      CheckSelectEndEff;
      MatchItemSelectState;
      Style.UpdateInfoRePaint;
    end
    else  // 没有按下Shift
    begin
      if vSelectExist then  // 有选中内容
      begin
        SelectInfo.StartItemNo := SelectInfo.EndItemNo;
        SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // 无选中内容
      begin
        if SelectInfo.StartItemOffset < vCurItem.Length then  // 不在Item最右边
          SelectInfo.StartItemOffset := SelectInfo.StartItemOffset + 1
        else  // 在Item最右边
        begin
          if SelectInfo.StartItemNo < Items.Count - 1 then  // 不是最后一个Item的最右边
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;  // 选中下一个Item
            SelectInfo.StartItemOffset := 0;  // 下一个最前面
            if not DrawItems[Items[SelectInfo.StartItemNo].FirstDItemNo].LineFirst then  // 下一个Item不是行起始
            begin
              KeyDown(Key, Shift);
              Exit;
            end;
          end
          else  // 在最后一个Item最右面按下右方向键
            Key := 0;
        end;
      end;

      if Key <> 0 then
      begin
        vNewCaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
        if vNewCaretDrawItemNo = CaretDrawItemNo then  // 移动前后是同一个DrawItem
        begin
          if (SelectInfo.StartItemOffset = DrawItems[vNewCaretDrawItemNo].CharOffsetEnd)  // 移动到DrawItem最后面了
            and (vNewCaretDrawItemNo < DrawItems.Count - 1)  // 不是最后一个
            and (DrawItems[vNewCaretDrawItemNo].ItemNo = DrawItems[vNewCaretDrawItemNo + 1].ItemNo)  // 下一个DrawItem和当前是同一个Item
            and (DrawItems[vNewCaretDrawItemNo + 1].LineFirst)  // 下一个是行首
            and (SelectInfo.StartItemOffset = DrawItems[vNewCaretDrawItemNo + 1].CharOffs - 1)  // 光标位置也是下一个DrawItem的最前面
          then
            CaretDrawItemNo := vNewCaretDrawItemNo + 1;  // 更换为下一个行首
        end
        else
          CaretDrawItemNo := vNewCaretDrawItemNo;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' HomeKeyDown 按键 '}
  procedure HomeKeyDown;
  var
    vFirstDItemNo, vLastDItemNo: Integer;
  begin
    if Shift = [ssShift] then  // Shift+Home
    begin
      // 取行首DrawItem
      vFirstDItemNo := GetDrawItemNoByOffset(FSelectSeekNo, FSelectSeekOffset);  // GetSelectStartDrawItemNo
      while vFirstDItemNo > 0 do
      begin
        if DrawItems[vFirstDItemNo].LineFirst then
          Break
        else
          Dec(vFirstDItemNo);
      end;

      if SelectInfo.EndItemNo >= 0 then  // 有选中内容
      begin
        if IsSelectSeekStart then  // 游标在选中起始
        begin
          SelectInfo.StartItemNo := DrawItems[vFirstDItemNo].ItemNo;
          SelectInfo.StartItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;
          SetSelectSeekStart;
        end
        else  // 游标在选中结束
        begin
          if DrawItems[vFirstDItemNo].ItemNo > SelectInfo.StartItemNo then
          begin
            SelectInfo.EndItemNo := DrawItems[vFirstDItemNo].ItemNo;
            SelectInfo.EndItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;
            SetSelectSeekEnd;
          end
          else
          if DrawItems[vFirstDItemNo].ItemNo = SelectInfo.StartItemNo then
          begin
            if DrawItems[vFirstDItemNo].CharOffs - 1 > SelectInfo.StartItemOffset then
            begin
              SelectInfo.EndItemNo := SelectInfo.StartItemNo;
              SelectInfo.EndItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;
              SetSelectSeekEnd;
            end;
          end
          else
          begin
            SelectInfo.EndItemNo := SelectInfo.StartItemNo;
            SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
            SelectInfo.StartItemNo := DrawItems[vFirstDItemNo].ItemNo;
            SelectInfo.StartItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;
            SetSelectSeekStart;
          end;
        end;
      end
      else   // 没有选中
      begin
        SelectInfo.EndItemNo := SelectInfo.StartItemNo;
        SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
        SelectInfo.StartItemNo := DrawItems[vFirstDItemNo].ItemNo;
        SelectInfo.StartItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;
        SetSelectSeekStart;
      end;

      CheckSelectEndEff;
      MatchItemSelectState;
      Style.UpdateInfoRePaint;
    end
    else
    begin
      if vSelectExist then  // 有选中内容
      begin
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // 无选中内容
      begin
        vFirstDItemNo := GetSelectStartDrawItemNo;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);
        SelectInfo.StartItemNo := DrawItems[vFirstDItemNo].ItemNo;
        SelectInfo.StartItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;
      end;

      if Key <> 0 then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    end;
  end;
  {$ENDREGION}

  {$REGION ' EndKeyDown 按键 '}
  procedure EndKeyDown;
  var
    vFirstDItemNo, vLastDItemNo: Integer;
  begin
    if Shift = [ssShift] then  // Shift+End
    begin
      // 取行尾DrawItem
      vLastDItemNo := GetDrawItemNoByOffset(FSelectSeekNo, FSelectSeekOffset);// GetSelectEndDrawItemNo;
      vLastDItemNo := vLastDItemNo + 1;
      while vLastDItemNo < DrawItems.Count do
      begin
        if DrawItems[vLastDItemNo].LineFirst then
          Break
        else
          Inc(vLastDItemNo);
      end;
      Dec(vLastDItemNo);

      if SelectInfo.EndItemNo >= 0 then  // 有选中内容
      begin
        if IsSelectSeekStart then  // 游标在选中起始
        begin
          if DrawItems[vLastDItemNo].ItemNo > SelectInfo.EndItemNo then  // 仍然在结束前面
          begin
            SelectInfo.StartItemNo := DrawItems[vLastDItemNo].ItemNo;
            SelectInfo.StartItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
            SetSelectSeekStart;
          end
          else
          if DrawItems[vLastDItemNo].ItemNo = SelectInfo.EndItemNo then
          begin
            SelectInfo.StartItemNo := SelectInfo.EndItemNo;
            if DrawItems[vLastDItemNo].CharOffsetEnd < SelectInfo.EndItemOffset then
            begin
              SelectInfo.StartItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
              SetSelectSeekStart;
            end
            else
            begin
              SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
              SelectInfo.EndItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
              SetSelectSeekEnd;
            end;
          end
          else
          begin
            SelectInfo.StartItemNo := SelectInfo.EndItemNo;
            SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
            SelectInfo.EndItemNo := DrawItems[vLastDItemNo].ItemNo;
            SelectInfo.EndItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
            SetSelectSeekEnd;
          end;
        end
        else  // 游标在选中结束
        begin
          SelectInfo.EndItemNo := DrawItems[vLastDItemNo].ItemNo;
          SelectInfo.EndItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
          SetSelectSeekEnd;
        end;
      end
      else   // 没有选中
      begin
        SelectInfo.EndItemNo := DrawItems[vLastDItemNo].ItemNo;
        SelectInfo.EndItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
        SetSelectSeekEnd;
      end;

      CheckSelectEndEff;
      MatchItemSelectState;
      Style.UpdateInfoRePaint;
    end
    else
    begin
      if vSelectExist then  // 有选中内容
      begin
        SelectInfo.StartItemNo := SelectInfo.EndItemNo;
        SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // 无选中内容
      begin
        vFirstDItemNo := GetSelectStartDrawItemNo;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);
        SelectInfo.StartItemNo := DrawItems[vLastDItemNo].ItemNo;
        SelectInfo.StartItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
      end;

      if Key <> 0 then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    end;
  end;
  {$ENDREGION}

  {$REGION ' UpKeyDown 上方向按键 '}
  procedure UpKeyDown;

    function GetUpDrawItemNo(var ADrawItemNo, ADrawItemOffset: Integer): Boolean;
    var
      i, vFirstDItemNo, vLastDItemNo, vX: Integer;
    begin
      Result := False;
      vFirstDItemNo := ADrawItemNo;
      GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // 当前行起始结束DrawItemNo
      if vFirstDItemNo > 0 then  // 当前行不是第一行
      begin
        Result := True;
        // 获取当前光标X位置
        vX := DrawItems[ADrawItemNo].Rect.Left + GetDrawItemOffsetWidth(ADrawItemNo, ADrawItemOffset);

        // 获取上一行在X位置对应的DItem和Offset
        vFirstDItemNo := vFirstDItemNo - 1;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // 上一行起始和结束DItem

        for i := vFirstDItemNo to vLastDItemNo do
        begin
          if DrawItems[i].Rect.Right > vX then
          begin
            ADrawItemNo := i;
            ADrawItemOffset := DrawItems[i].CharOffs + GetDrawItemOffset(i, vX) - 1;

            Exit;  // 有合适，则退出
          end;
        end;

        // 没合适则选择到最后
        ADrawItemNo := vLastDItemNo;
        ADrawItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
      end
    end;

  var
    vDrawItemNo, vDrawItemOffset: Integer;
  begin
    if Shift = [ssShift] then  // Shift+Up
    begin
      if SelectInfo.EndItemNo >= 0 then  // 有选中内容
      begin
        if IsSelectSeekStart then  // 游标在选中起始
        begin
          vDrawItemNo := GetSelectStartDrawItemNo;
          vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
          if GetUpDrawItemNo(vDrawItemNo, vDrawItemOffset) then
          begin
            SelectInfo.StartItemNo := DrawItems[vDrawItemNo].ItemNo;
            SelectInfo.StartItemOffset := vDrawItemOffset;
            SetSelectSeekStart;
          end;
        end
        else  // 游标在选中结束
        begin
          vDrawItemNo := GetSelectEndDrawItemNo;
          vDrawItemOffset := SelectInfo.EndItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
          if GetUpDrawItemNo(vDrawItemNo, vDrawItemOffset) then
          begin
            if DrawItems[vDrawItemNo].ItemNo > SelectInfo.StartItemNo then  // 移动到起始后面的Item上
            begin
              SelectInfo.EndItemNo := vDrawItemNo;
              SelectInfo.EndItemOffset := vDrawItemOffset;
              SetSelectSeekEnd;
            end
            else
            if DrawItems[vDrawItemNo].ItemNo = SelectInfo.StartItemNo then  // 移动到起始Item中
            begin
              SelectInfo.EndItemNo := SelectInfo.StartItemNo;
              if vDrawItemOffset > SelectInfo.StartItemOffset then  // 移动在起始Offset后面
              begin
                SelectInfo.EndItemOffset := vDrawItemOffset;
                SetSelectSeekEnd;
              end
              else  // 移动到起始Offset前面
              begin
                SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
                SelectInfo.StartItemOffset := vDrawItemOffset;
                SetSelectSeekStart;
              end;
            end
            else  // 移动到起始Item前面了
            begin
              SelectInfo.EndItemNo := SelectInfo.StartItemNo;
              SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
              SelectInfo.StartItemNo := DrawItems[vDrawItemNo].ItemNo;
              SelectInfo.StartItemOffset := vDrawItemOffset;
              SetSelectSeekStart;
            end;
          end;
        end;
      end
      else   // 没有选中
      begin
        vDrawItemNo := CaretDrawItemNo;
        vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
        if GetUpDrawItemNo(vDrawItemNo, vDrawItemOffset) then
        begin
          SelectInfo.EndItemNo := SelectInfo.StartItemNo;
          SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
          SelectInfo.StartItemNo := DrawItems[vDrawItemNo].ItemNo;
          SelectInfo.StartItemOffset := vDrawItemOffset;
          SetSelectSeekStart;
        end;
      end;

      CheckSelectEndEff;
      MatchItemSelectState;
      Style.UpdateInfoRePaint;
    end
    else  // 无Shift按下
    begin
      if vSelectExist then  // 有选中内容
      begin
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // 无选中内容
      begin
        vDrawItemNo := CaretDrawItemNo;  // GetSelectStartDrawItemNo;
        vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
        if GetUpDrawItemNo(vDrawItemNo, vDrawItemOffset) then
        begin
          SelectInfo.StartItemNo := DrawItems[vDrawItemNo].ItemNo;
          SelectInfo.StartItemOffset := vDrawItemOffset;
          CaretDrawItemNo := vDrawItemNo;
        end
        else
          Key := 0;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' DownKeyDown 下方向键 '}
  procedure DownKeyDown;

    function GetDownDrawItemNo(var ADrawItemNo, ADrawItemOffset: Integer): Boolean;
    var
      i, vFirstDItemNo, vLastDItemNo, vX: Integer;
    begin
      Result := False;
      vFirstDItemNo := ADrawItemNo;  // GetSelectStartDrawItemNo;
      GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // 当前行起始结束DItemNo
      if vLastDItemNo < DrawItems.Count - 1 then  // 当前行不是最后一行
      begin
        Result := True;
        { 获取当前光标X位置 }
        vX := DrawItems[ADrawItemNo].Rect.Left + GetDrawItemOffsetWidth(ADrawItemNo, ADrawItemOffset);

        { 获取下一行在X位置对应的DItem和Offset }
        vFirstDItemNo := vLastDItemNo + 1;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // 下一行起始和结束DItem

        for i := vFirstDItemNo to vLastDItemNo do
        begin
          if DrawItems[i].Rect.Right > vX then
          begin
            ADrawItemNo := i;
            ADrawItemOffset := DrawItems[i].CharOffs + GetDrawItemOffset(i, vX) - 1;

            Exit;  // 有合适，则退出
          end;
        end;

        { 没合适则选择到最后 }
        ADrawItemNo := vLastDItemNo;
        ADrawItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
      end
    end;

  var
    vDrawItemNo, vDrawItemOffset: Integer;
  begin
    if Shift = [ssShift] then  // Shift+Up
    begin
      if SelectInfo.EndItemNo >= 0 then  // 有选中内容
      begin
        if IsSelectSeekStart then  // 游标在选中起始
        begin
          vDrawItemNo := GetSelectStartDrawItemNo;
          vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
          if GetDownDrawItemNo(vDrawItemNo, vDrawItemOffset) then
          begin
            if DrawItems[vDrawItemNo].ItemNo < SelectInfo.EndItemNo then  // 移动到结束ItemNo前面
            begin
              SelectInfo.StartItemNo := SelectInfo.EndItemNo;
              SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
              SetSelectSeekStart;
            end
            else
            if DrawItems[vDrawItemNo].ItemNo = SelectInfo.EndItemNo then  // 移动到和结束Item
            begin
              SelectInfo.StartItemNo := SelectInfo.EndItemNo;
              if vDrawItemOffset < SelectInfo.EndItemOffset then  // 位置在结束Offset前面
              begin
                SelectInfo.StartItemOffset := vDrawItemOffset;
                SetSelectSeekStart;
              end
              else  // 位置在结束Offset后面
              begin
                SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
                SelectInfo.EndItemOffset := vDrawItemOffset;
                SetSelectSeekEnd;
              end;
            end
            else  // 移动到结束Item后面，交换
            begin
              SelectInfo.StartItemNo := SelectInfo.EndItemNo;
              SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
              SelectInfo.EndItemNo := DrawItems[vDrawItemNo].ItemNo;
              SelectInfo.EndItemOffset := vDrawItemOffset;
              SetSelectSeekEnd;
            end;
          end;
        end
        else  // 游标在选中结束
        begin
          vDrawItemNo := GetSelectEndDrawItemNo;
          vDrawItemOffset := SelectInfo.EndItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
          if GetDownDrawItemNo(vDrawItemNo, vDrawItemOffset) then
          begin
            SelectInfo.EndItemNo := DrawItems[vDrawItemNo].ItemNo;
            SelectInfo.EndItemOffset := vDrawItemOffset;
            SetSelectSeekEnd;
          end;
        end;
      end
      else   // 没有选中
      begin
        vDrawItemNo := CaretDrawItemNo;
        vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
        if GetDownDrawItemNo(vDrawItemNo, vDrawItemOffset) then
        begin
          SelectInfo.EndItemNo := DrawItems[vDrawItemNo].ItemNo;
          SelectInfo.EndItemOffset := vDrawItemOffset;
          SetSelectSeekEnd;
        end;
      end;

      CheckSelectEndEff;
      MatchItemSelectState;
      Style.UpdateInfoRePaint;
    end
    else  // 无Shift按下
    begin
      if vSelectExist then  // 有选中内容
      begin
        SelectInfo.StartItemNo := SelectInfo.EndItemNo;
        SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // 无选中内容
      begin
        vDrawItemNo := CaretDrawItemNo;  // GetSelectStartDrawItemNo;
        vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
        if GetDownDrawItemNo(vDrawItemNo, vDrawItemOffset) then
        begin
          SelectInfo.StartItemNo := DrawItems[vDrawItemNo].ItemNo;
          SelectInfo.StartItemOffset := vDrawItemOffset;
          CaretDrawItemNo := vDrawItemNo;
        end
        else  // 当前行是最后一行
          Key := 0;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' RectItemKeyDown Rect类型Item的KeyDown事件 '}
  procedure RectItemKeyDown;
  var
    vItem: THCCustomItem;
    vLen: Integer;
    vRectItem: THCCustomRectItem;
  begin
    vRectItem := vCurItem as THCCustomRectItem;

    if SelectInfo.StartItemOffset = OffsetInner then  // 在其上
    begin
      if vRectItem.WantKeyDown(Key, Shift) then
      begin
        vRectItem.KeyDown(Key, Shift);
        if vRectItem.SizeChanged then
        begin
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

          vRectItem.SizeChanged := False;
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
        end;
      end
      else  // 内部不响应此键
      begin
        case Key of
          VK_BACK:
            begin
              SelectInfo.StartItemOffset := OffsetAfter;
              RectItemKeyDown;
            end;

          VK_DELETE:
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              RectItemKeyDown;
            end;
        end;
      end;
    end
    else
    if SelectInfo.StartItemOffset = OffsetBefor then  // 在RectItem前
    begin
      case Key of
        VK_LEFT:
          LeftKeyDown;

        VK_RIGHT:
          begin
            if Shift = [ssShift] then  // Shift+方向键选择
              RightKeyDown
            else
            begin
              if vRectItem.WantKeyDown(Key, Shift) then
                SelectInfo.StartItemOffset := OffsetInner
              else
                SelectInfo.StartItemOffset := OffsetAfter;

              CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
            end;
          end;

        VK_UP: UpKeyDown;

        VK_DOWN: DownKeyDown;

        VK_END: EndKeyDown;

        VK_HOME: HomeKeyDown;

        VK_RETURN:
          begin
            GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

            if vCurItem.ParaFirst then  // RectItem在段首，插入空行
            begin
              vCurItem := CreateDefaultTextItem;
              vCurItem.ParaFirst := True;
              Items.Insert(SelectInfo.StartItemNo, vCurItem);

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);

              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
            end
            else  // RectItem不在行首
            begin
              vCurItem.ParaFirst := True;
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
            end;
          end;

        VK_BACK:  // 在RectItem前
          begin
            if vCurItem.ParaFirst then  // 是段首
            begin
              if SelectInfo.StartItemNo > 0 then  // 不是Data第一个Item
              begin
                GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
                FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

                Undo_StartRecord;
                Undo_ItemParaFirst(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, False);

                vCurItem.ParaFirst := False;
                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
              end
              else
                DrawItems.ClearFormatMark;  // 第一个前回删不处理，停止格式化
            end
            else  // 不是段首
            begin
              // 选到上一个最后
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              SelectInfo.StartItemOffset := GetItemAfterOffset(SelectInfo.StartItemNo);

              KeyDown(Key, Shift);  // 执行前一个的删除
            end;
          end;

        VK_DELETE:  // 在RectItem前
          begin
            if not CanDeleteItem(SelectInfo.StartItemNo) then  // 不可删除
            begin
              SelectInfo.StartItemOffset := OffsetAfter;
              Exit;
            end;

            GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

            if vCurItem.ParaFirst then  // 是段首
            begin
              if SelectInfo.StartItemNo <> vFormatLastItemNo then  // 段不是只有一个
              begin
                Undo_StartRecord;

                Undo_ItemParaFirst(SelectInfo.StartItemNo + 1, 0, True);
                Items[SelectInfo.StartItemNo + 1].ParaFirst := True;

                Undo_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
              end
              else  // 段删除空了
              begin
                Undo_StartRecord;
                Undo_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                vCurItem := CreateDefaultTextItem;
                vCurItem.ParaFirst := True;
                Items.Insert(SelectInfo.StartItemNo, vCurItem);
                Undo_InsertItem(SelectInfo.StartItemNo, 0);

                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
              end;
            end
            else  // 不是段首
            begin
              if SelectInfo.StartItemNo < vFormatLastItemNo then  // 段中间
              begin
                vLen := GetItemAfterOffset(SelectInfo.StartItemNo - 1);

                Undo_StartRecord;
                Undo_DeleteItem(SelectInfo.StartItemNo, 0);
                // 如果RectItem前面(同一行)有高度小于此RectItme的Item(如Tab)，
                // 其格式化时以RectItem为高，重新格式化时如果从RectItem所在位置起始格式化，
                // 行高度仍会以Tab为行高，也就是RectItem高度，所以需要从行开始格式化
                Items.Delete(SelectInfo.StartItemNo);
                if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then  // 原RectItem前后能合并
                begin
                  Undo_InsertText(SelectInfo.StartItemNo - 1,
                    Items[SelectInfo.StartItemNo - 1].Length + 1, Items[SelectInfo.StartItemNo].Text);

                  Items.Delete(SelectInfo.StartItemNo);
                  ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 2, -2);
                end
                else
                  ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

                SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
                SelectInfo.StartItemOffset := vLen;
              end
              else  // 段尾(段不只一个Item)
              begin
                Undo_StartRecord;
                Undo_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

                SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
                SelectInfo.StartItemOffset := GetItemAfterOffset(SelectInfo.StartItemNo);
              end;
            end;

            ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
          end;

        VK_TAB:
          TABKeyDown;
      end;
    end
    else
    if SelectInfo.StartItemOffset = OffsetAfter then  // 在其后
    begin
      case Key of
        VK_BACK:
          begin
            if not CanDeleteItem(SelectInfo.StartItemNo) then  // 不可删除
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              Exit;
            end;

            GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

            if vCurItem.ParaFirst then  // 是段首
            begin
              if (SelectInfo.StartItemNo >= 0)
                and (SelectInfo.StartItemNo < Items.Count - 1)
                and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)
              then  // 同一段还有内容
              begin
                Undo_StartRecord;
                Undo_DeleteItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
                Items.Delete(SelectInfo.StartItemNo);

                Undo_ItemParaFirst(SelectInfo.StartItemNo, 0, True);
                Items[SelectInfo.StartItemNo].ParaFirst := True;
                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

                ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
              end
              else  // 空段了
              begin
                Undo_StartRecord;
                Undo_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                vItem := CreateDefaultTextItem;
                vItem.ParaFirst := True;
                Items.Insert(SelectInfo.StartItemNo, vItem);
                Undo_InsertItem(SelectInfo.StartItemNo, 0);

                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
                SelectInfo.StartItemOffset := 0;
              end;
            end
            else  // 不是段首
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              Key := VK_DELETE;  // 临时替换
              RectItemKeyDown;
              Key := VK_BACK;  // 还原
            end;
          end;

        VK_DELETE:
          begin
            if SelectInfo.StartItemNo < Items.Count - 1 then  // 不是最后一个
            begin
              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              SelectInfo.StartItemOffset := 0;

              if Items[SelectInfo.StartItemNo].ParaFirst then
              begin
                GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
                FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

                Undo_StartRecord;
                Undo_ItemParaFirst(SelectInfo.StartItemNo, 0, False);

                Items[SelectInfo.StartItemNo].ParaFirst := False;

                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
                ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
              end
              else
                KeyDown(Key, Shift);
              //Exit;
            end;
          end;

        VK_LEFT:
          begin
            if Shift = [ssShift] then  // Shift+方向键选择
              LeftKeyDown
            else
            begin
              if vRectItem.WantKeyDown(Key, Shift) then
                SelectInfo.StartItemOffset := OffsetInner
              else
                SelectInfo.StartItemOffset := OffsetBefor;

              CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
            end;
          end;

        VK_RIGHT: RightKeyDown;

        VK_UP: UpKeyDown;

        VK_DOWN: DownKeyDown;

        VK_END: EndKeyDown;

        VK_HOME: HomeKeyDown;

        VK_RETURN:
          begin
            GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

            if (SelectInfo.StartItemNo < Items.Count - 1)  // 不是最后一个
              and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)  // 下一个不是段首
            then
            begin
              Items[SelectInfo.StartItemNo + 1].ParaFirst := True;
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              SelectInfo.StartItemOffset := 0;
              CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
            end
            else
            begin
              vCurItem := CreateDefaultTextItem;
              vCurItem.ParaFirst := True;
              Items.Insert(SelectInfo.StartItemNo + 1, vCurItem);
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);
              ReSetSelectAndCaret(SelectInfo.StartItemNo + 1, vCurItem.Length);
            end;
          end;

        VK_TAB:
          TABKeyDown;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' EnterKeyDown 回车 '}
  procedure EnterKeyDown;
  var
    vItem: THCCustomItem;
  begin
    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    // 判断光标位置内容如何换行
    if SelectInfo.StartItemOffset = 0 then  // 光标在Item最前面
    begin
      if not vCurItem.ParaFirst then  // 原来不是段首
      begin
        vCurItem.ParaFirst := True;
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
      end
      else  // 原来就是段首
      begin
        vItem := CreateDefaultTextItem;
        vItem.ParaNo := vCurItem.ParaNo;
        vItem.StyleNo := vCurItem.StyleNo;
        vItem.ParaFirst := True;
        Items.Insert(SelectInfo.StartItemNo, vItem);  // 原位置的向下移动
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);
        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
      end;
    end
    else
    if SelectInfo.StartItemOffset = vCurItem.Length then  // 光标在Item最后面
    begin
      if SelectInfo.StartItemNo < Items.Count - 1 then  // 不是最后一个Item
      begin
        vItem := Items[SelectInfo.StartItemNo + 1];  // 下一个Item
        if not vItem.ParaFirst then  // 下一个不是段起始
        begin
          vItem.ParaFirst := True;
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
        end
        else  // 下一个是段起始
        begin
          vItem := CreateDefaultTextItem;
          vItem.ParaNo := vCurItem.ParaNo;
          vItem.StyleNo := vCurItem.StyleNo;
          vItem.ParaFirst := True;
          Items.Insert(SelectInfo.StartItemNo + 1, vItem);
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);
        end;
        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
        SelectInfo.StartItemOffset := 0;
      end
      else  // 是Data最后一个Item，新建空行
      begin
        vItem := CreateDefaultTextItem;
        vItem.ParaNo := vCurItem.ParaNo;
        vItem.StyleNo := vCurItem.StyleNo;
        vItem.ParaFirst := True;
        Items.Insert(SelectInfo.StartItemNo + 1, vItem);
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);
        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
        SelectInfo.StartItemOffset := 0;
      end;
    end
    else  // 光标在Item中间
    begin
      vItem := vCurItem.BreakByOffset(SelectInfo.StartItemOffset);  // 截断当前Item
      vItem.ParaFirst := True;

      Items.Insert(SelectInfo.StartItemNo + 1, vItem);
      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);

      SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
      SelectInfo.StartItemOffset := 0;
    end;
    if Key <> 0 then
      CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  end;
  {$ENDREGION}

  {$REGION ' DeleteKeyDown 向后删除键 '}
  procedure DeleteKeyDown;
  var
    vText: string;
    i, vCurItemNo, vLen, vDelCount, vParaNo: Integer;
  begin
    vDelCount := 0;
    vCurItemNo := SelectInfo.StartItemNo;
    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);

    if SelectInfo.StartItemOffset = vCurItem.Length then  // 光标在Item最右边(包括空行)
    begin
      if vCurItemNo <> Items.Count - 1 then  // 不是最后一个Item最右边删除
      begin
        if Items[vCurItemNo + 1].ParaFirst then  // 下一个是段首，光标处Item是上一段最后一个，下一段要移上来
        begin
          vFormatLastItemNo := GetParaLastItemNo(vCurItemNo + 1);  // 获取下一段最后一个
          if vCurItem.Length = 0 then  // 当前是空行
          begin
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
            Items.Delete(vCurItemNo);
            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
          end
          else  // 当前不是空行
          begin
            if Items[vCurItemNo + 1].StyleNo < THCStyle.Null then  // 下一个段首是RectItem，不能合并
            begin
              SelectInfo.StartItemNo := vCurItemNo + 1;
              SelectInfo.StartItemOffset := OffsetBefor;

              KeyDown(Key, Shift);
              Exit;
            end
            else  // 下一个段首是TextItem(当前在上一段段尾)
            begin
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

              if Items[vCurItemNo + 1].Length = 0 then  // 下一段的段首是是空行
              begin
                Items.Delete(vCurItemNo + 1);
                Inc(vDelCount);
              end
              else  // 下一段的段首不是空行
              begin
                //if (vCurItem.ClassType = Items[vCurItemNo + 1].ClassType)
                //  and (vCurItem.StyleNo = Items[vCurItemNo + 1].StyleNo)
                if vCurItem.CanConcatItems(Items[vCurItemNo + 1]) then  // 下一段段首可合并到当前(当前在上一段段尾) 201804111209 (不能用MergeItemText的情况)
                begin
                  vCurItem.Text := vCurItem.Text + Items[vCurItemNo + 1].Text;
                  Items.Delete(vCurItemNo + 1);
                  Inc(vDelCount);
                end
                else// 下一段段首不是空行也不能合并
                  Items[vCurItemNo + 1].ParaFirst := False;

                // 修正下一段合并上来的Item段样式，对齐样式
                vParaNo := Items[vCurItemNo].ParaNo;
                for i := vCurItemNo + 1 to vFormatLastItemNo - vDelCount do
                  Items[i].ParaNo := vParaNo;
              end;

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
            end;
          end;
        end
        else  // 下一个不能合并也不是段首，移动到下一个开头再调用DeleteKeyDown
        begin
          SelectInfo.StartItemNo := vCurItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          vCurItem := GetCurItem;
          {if vCurItem.StyleNo < THCStyle.RsNull then
            RectItemKeyDown
          else
            DeleteKeyDown;}

          KeyDown(Key, Shift);
          Exit;
        end;
      end;
    end
    else  // 光标不在Item最右边
    begin
      if not CanDeleteItem(vCurItemNo) then  // 不可删除
        SelectInfo.StartItemOffset := SelectInfo.StartItemOffset + 1
      else  // 可删除
      begin
        vText := Items[vCurItemNo].Text;

        Delete(vText, SelectInfo.StartItemOffset + 1, 1);
        vCurItem.Text := vText;
        if vText = '' then  // 删除后没有内容了
        begin
          if not DrawItems[Items[vCurItemNo].FirstDItemNo].LineFirst then  // 该Item不是行首(是行中间或行末尾)
          begin
            if vCurItemNo < Items.Count - 1 then  // 不是行首也不是最后一个Item
            begin
              if MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1]) then  // 下一个可合并到上一个
              begin
                vLen := Items[vCurItemNo + 1].Length;
                GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, vCurItemNo - 1, vLen);
                FormatItemPrepare(vCurItemNo - 1, vFormatLastItemNo);
                Items.Delete(vCurItemNo);  // 删除当前
                Items.Delete(vCurItemNo);  // 删除下一个
                ReFormatData_(vCurItemNo - 1, vFormatLastItemNo - 2, -2);
              end
              else  // 下一个合并不到上一个
              begin
                vLen := 0;
                FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
                Items.Delete(vCurItemNo);
                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
              end;

              // 光标左移
              SelectInfo.StartItemNo := vCurItemNo - 1;
              if GetItemStyle(SelectInfo.StartItemNo) < THCStyle.Null then
                SelectInfo.StartItemOffset := OffsetAfter
              else
                SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length - vLen;
            end
            else  // 是最后一个Item删除空了
            begin
              // 光标左移
              FormatItemPrepare(vCurItemNo);
              Items.Delete(vCurItemNo);
              SelectInfo.StartItemNo := vCurItemNo - 1;
              SelectInfo.StartItemOffset := GetItemAfterOffset(SelectInfo.StartItemNo);
              //ReFormatData_(SelectInfo.StartItemNo, SelectInfo.StartItemNo, -1);
              DrawItems.DeleteFormatMark;
            end;
          end
          else  // 行首Item被删空了
          begin
            if vCurItemNo <> vFormatLastItemNo then  // 当前段后面还有Item
            begin
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
              SelectInfo.StartItemOffset := 0;
              Items[vCurItemNo + 1].ParaFirst := Items[vCurItemNo].ParaFirst;
              Items.Delete(vCurItemNo);
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
            end
            else  // 当前段删除空了
            begin
              FormatItemPrepare(vCurItemNo);
              SelectInfo.StartItemOffset := 0;
              ReFormatData_(vCurItemNo);
            end;
          end;
        end
        else  // 删除后还有内容
        begin
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
        end;
      end;
    end;
    if Key <> 0 then
      CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  end;
  {$ENDREGION}

  {$REGION ' BackspaceKeyDown 向前删除键 '}
  procedure BackspaceKeyDown;
  var
    vText: string;
    i, vCurItemNo, vDrawItemNo, vLen, vDelCount, vParaNo: Integer;
    vParaFirst: Boolean;
  begin
    if SelectInfo.StartItemOffset = 0 then  // 光标在Item最开始
    begin
      if (vCurItem.Text = '') and (Style.ParaStyles[vCurItem.ParaNo].AlignHorz <> TParaAlignHorz.pahJustify) then
        ApplyParaAlignHorz(TParaAlignHorz.pahJustify)  // 居中等对齐的空Item，删除时切换到分散对齐
      else
      if SelectInfo.StartItemNo <> 0 then  // 不是第1个Item最前面删除
      begin
        vCurItemNo := SelectInfo.StartItemNo;
        if vCurItem.ParaFirst then  // 是段起始Item
        begin
          vLen := Items[SelectInfo.StartItemNo - 1].Length;

          //if (vCurItem.ClassType = Items[SelectInfo.StartItemNo - 1].ClassType)
          //  and (vCurItem.StyleNo = Items[SelectInfo.StartItemNo - 1].StyleNo)
          if vCurItem.CanConcatItems(Items[SelectInfo.StartItemNo - 1]) then  // 当前可以和上一个合并(当前在段首) 201804111209 (不能用MergeItemText的情况)
          begin
            Undo_StartRecord;
            Undo_InsertText(SelectInfo.StartItemNo - 1, Items[SelectInfo.StartItemNo - 1].Length + 1,
              Items[SelectInfo.StartItemNo].Text);

            Items[SelectInfo.StartItemNo - 1].Text := Items[SelectInfo.StartItemNo - 1].Text
              + Items[SelectInfo.StartItemNo].Text;

            vFormatFirstItemNo := GetLineFirstItemNo(SelectInfo.StartItemNo - 1, vLen);
            vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

            Undo_DeleteItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
            Items.Delete(SelectInfo.StartItemNo);

            // 修正下一段合并上来的Item的段样式，对齐样式
            vParaNo := Items[SelectInfo.StartItemNo - 1].ParaNo;
            if vParaNo <> vCurItem.ParaNo then  // 2段ParaNo不同
            begin
              for i := SelectInfo.StartItemNo to vFormatLastItemNo - 1 do
              begin
                //Undo_ItemParaNo(i, 0, vParaNo);
                Items[i].ParaNo := vParaNo;
              end;
            end;

            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

            ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, vLen);
          end
          else  // 段起始且不能和上一个合并
          begin
            if vCurItem.Length = 0 then  // 已经没有内容了(不是第1个Item，说明是空行)
            begin
              FormatItemPrepare(SelectInfo.StartItemNo - 1, SelectInfo.StartItemNo);

              Undo_StartRecord;
              Undo_DeleteItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
              Items.Delete(SelectInfo.StartItemNo);

              ReFormatData_(SelectInfo.StartItemNo - 1, SelectInfo.StartItemNo - 1, -1);

              ReSetSelectAndCaret(SelectInfo.StartItemNo - 1);
            end
            else  // 段前删除且不能和上一段最后合并
            begin
              GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

              Undo_StartRecord;
              Undo_ItemParaFirst(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, False);

              vCurItem.ParaFirst := False;  // 当前段和上一段Item拼接成一段

              vParaNo := Items[SelectInfo.StartItemNo - 1].ParaNo;  // 上一段的ParaNo
              if vParaNo <> vCurItem.ParaNo then  // 2段ParaNo不同
              begin
                for i := SelectInfo.StartItemNo to vFormatLastItemNo do
                begin
                  //Undo_ItemParaNo(i, 0, vParaNo);
                  Items[i].ParaNo := vParaNo;
                end;
              end;

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);

              ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
            end;
          end;
        end
        else  // 在Item开始往前删，但Item不是段起始
        begin
          if Items[SelectInfo.StartItemNo - 1].StyleNo < THCStyle.Null then  // 前面是RectItem
          begin
            vCurItemNo := SelectInfo.StartItemNo - 1;
            if CanDeleteItem(vCurItemNo) then  // 能删除
            begin
              Undo_StartRecord;

              vParaFirst := Items[vCurItemNo].ParaFirst;  // 记录前面的RectItem段首属性

              GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

              // 删除前面的RectItem
              Undo_DeleteItem(vCurItemNo, OffsetAfter);
              Items.Delete(vCurItemNo);

              if vParaFirst then  // 前面删除的RectItem是段首
              begin
                Undo_ItemParaFirst(vCurItemNo, 0, vParaFirst);
                vCurItem.ParaFirst := vParaFirst;  // 赋值前面RectItem的段起始属性
                vLen := 0;
              end
              else  // 前面删除的RectItem不是段首
              begin
                vDelCount := 1;
                vCurItemNo := vCurItemNo - 1;  // 上一个
                vLen := Items[vCurItemNo].Length;  // 上一个最后面

                if MergeItemText(Items[vCurItemNo], vCurItem) then  // 当前能合并到上一个
                begin
                  Undo_InsertText(vCurItemNo, vLen + 1, vCurItem.Text);
                  Undo_DeleteItem(vCurItemNo + 1, 0);
                  Items.Delete(vCurItemNo + 1); // 删除当前的
                  vDelCount := 2;
                end;
              end;

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
            end
            else  // 不能删除，光标放最前
              vLen := OffsetBefor;

            ReSetSelectAndCaret(vCurItemNo, vLen);
          end
          else  // 前面是文本，赋值为前面的最后，再重新处理删除
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            SelectInfo.StartItemOffset := GetItemAfterOffset(SelectInfo.StartItemNo);
            vCurItem := GetCurItem;

            Style.UpdateInfoReStyle;
            BackspaceKeyDown;  // 重新处理
            Exit;
          end;
        end;
      end;
    end
    else  // 光标不在Item最开始  文本TextItem
    begin
      if vCurItem.Length = 1 then  // 删除后没有内容了
      begin
        vCurItemNo := SelectInfo.StartItemNo;  // 记录原位置
        if not DrawItems[Items[vCurItemNo].FirstDItemNo].LineFirst then  // 当前不是行首，前面有内容
        begin
          vLen := Items[vCurItemNo - 1].Length;

          if (vCurItemNo > 0) and (vCurItemNo < vParaLastItemNo)  // 不是段最后一个
            and MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1])
          then  // 当前Item位置上一个和当前Item位置下一个可合并
          begin
            Undo_StartRecord;
            Undo_InsertText(vCurItemNo - 1, Items[vCurItemNo - 1].Length - Items[vCurItemNo + 1].Length + 1,
              Items[vCurItemNo + 1].Text);

            GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, vCurItemNo - 1, vLen);
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

            Undo_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
            Items.Delete(vCurItemNo);  // 删除当前

            Undo_DeleteItem(vCurItemNo, 0);
            Items.Delete(vCurItemNo);  // 删除下一个

            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 2, -2);

            ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, vLen);  // 上一个原光标位置
          end
          else  // 当前不是行首，删除后没有内容了，且不能合并上一个和下一个
          begin
            if SelectInfo.StartItemNo = vParaLastItemNo then  // 段最后一个
            begin
              vFormatFirstItemNo := GetLineFirstItemNo(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
              vFormatLastItemNo := vParaLastItemNo;
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

              Undo_StartRecord;
              Undo_DeleteItem(vCurItemNo, SelectInfo.StartItemOffset);
              Items.Delete(vCurItemNo);

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

              ReSetSelectAndCaret(vCurItemNo - 1);
            end
            else  // 不是段最后一个
            begin
              GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

              Undo_StartRecord;
              Undo_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
              Items.Delete(vCurItemNo);

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

              ReSetSelectAndCaret(vCurItemNo - 1);
            end;
          end;
        end
        else  // Item是行第一个、行首Item删除空了，
        begin
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);

          if Items[vCurItemNo].ParaFirst then  // 是段首，删除空了
          begin
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

            if vCurItemNo < vFormatLastItemNo then  // 同段后面还有内容
            begin
              Undo_StartRecord;

              vParaFirst := True;  // Items[vCurItemNo].ParaFirst;  // 记录行首Item的段属性

              Undo_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
              Items.Delete(vCurItemNo);

              if vParaFirst then  // 删除的是段首
              begin
                Undo_ItemParaFirst(vCurItemNo, 0, vParaFirst);
                Items[vCurItemNo].ParaFirst := vParaFirst;  // 其后继承段首属性
              end;

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
              ReSetSelectAndCaret(vCurItemNo, 0);  // 下一个最前面
            end
            else  // 同段后面没有内容了，保持空行
            begin
              Undo_StartRecord;
              Undo_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset,
                vCurItem.Text);  // Copy(vText, SelectInfo.StartItemOffset, 1));

              //System.Delete(vText, SelectInfo.StartItemOffset, 1);
              vCurItem.Text := '';  // vText;
              SelectInfo.StartItemOffset := SelectInfo.StartItemOffset - 1;

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);  // 保留空行
            end;
          end
          else  // 不是段首Item，仅是行首Item删除空了
          begin
            Undo_StartRecord;

            if vCurItemNo < vFormatLastItemNo then  // 如果删除后，同段后面还有内容
            begin
              vLen := Items[vCurItemNo - 1].Length;
              if MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1]) then  // 前后能合并
              begin
                Undo_InsertText(vCurItemNo - 1,
                  Items[vCurItemNo - 1].Length - Items[vCurItemNo + 1].Length + 1, Items[vCurItemNo + 1].Text);

                GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, vCurItemNo - 1, Items[vCurItemNo - 1].Length);  // 取前一个格式化起始位置
                FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

                Undo_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);  // 删除空的Item
                Items.Delete(vCurItemNo);

                Undo_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);  // 被合并的Item
                Items.Delete(vCurItemNo);

                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 2, -2);
                ReSetSelectAndCaret(vCurItemNo - 1, vLen);
              end
              else  // 前后不能合并
              begin
                FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

                Undo_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
                Items.Delete(vCurItemNo);

                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
                ReSetSelectAndCaret(vCurItemNo - 1);
              end;
            end
            else  // 同段后面没有内容了
            begin
              if vFormatFirstItemNo = vCurItemNo then  // 格式化起始的删除了
                Dec(vFormatFirstItemNo);

              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

              Undo_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
              Items.Delete(vCurItemNo);

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
              ReSetSelectAndCaret(vCurItemNo - 1);
            end;
          end;
        end;
      end
      else  // 删除后还有内容
      begin
        { 不是段第一行的行首时，考虑行首是上一行最后的Item整体移动到下一行的情况，
          现在删除了要从上一行开始判断 }
        if SelectInfo.StartItemNo > vParaFirstItemNo then
          vFormatFirstItemNo := GetLineFirstItemNo(SelectInfo.StartItemNo - 1, 0)
        else
          vFormatFirstItemNo := SelectInfo.StartItemNo;

        vFormatLastItemNo := vParaLastItemNo;

        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

        vText := vCurItem.Text;  // 和上面 201806242257 处一样

        Undo_StartRecord;
        Undo_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset,
          Copy(vText, SelectInfo.StartItemOffset, 1));

        System.Delete(vText, SelectInfo.StartItemOffset, 1);
        vCurItem.Text := vText;

        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);

        SelectInfo.StartItemOffset := SelectInfo.StartItemOffset - 1;
        ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      end;
    end;
  end;
  {$ENDREGION}

begin
  if not CanEdit then Exit;

  if Key in [VK_BACK, VK_DELETE, VK_RETURN, VK_TAB] then
    Self.InitializeMouseField;  // 如果Item删除完了，原MouseMove处ItemNo可能不存在了，再MouseMove时清除旧的出错

  vCurItem := GetCurItem;
  if not Assigned(vCurItem) then Exit;  // 跨页合并时，合并后并没有当前Item

  vSelectExist := SelectExists;

  if vSelectExist and (Key in [VK_BACK, VK_DELETE, VK_RETURN, VK_TAB]) then
  begin
    if DeleteSelected then
    begin
      if Key in [VK_BACK, VK_DELETE] then Exit;
    end;
  end;

  GetParaItemRang(SelectInfo.StartItemNo, vParaFirstItemNo, vParaLastItemNo);

  if vCurItem.StyleNo < THCStyle.Null then
    RectItemKeyDown
  else
  begin
    case Key of
      VK_BACK:   BackspaceKeyDown;  // 回删
      VK_RETURN: EnterKeyDown;      // 回车
      VK_LEFT:   LeftKeyDown;       // 左方向键
      VK_RIGHT:  RightKeyDown;      // 右方向键
      VK_DELETE: DeleteKeyDown;     // 删除键
      VK_HOME:   HomeKeyDown;       // Home键
      VK_END:    EndKeyDown;        // End键
      VK_UP:     UpKeyDown;         // 上方向键
      VK_DOWN:   DownKeyDown;       // 下方向键
      VK_TAB:    TABKeyDown;        // TAB键
    end;
  end;

  case Key of
    VK_BACK, VK_DELETE, VK_RETURN, VK_TAB:
      begin
        Style.UpdateInfoRePaint;
        Style.UpdateInfoReCaret;  // 删除后以新位置光标为当前样式
        Style.UpdateInfoReScroll;
      end;

    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END:
      begin
        if vSelectExist then
          Style.UpdateInfoRePaint;
        Style.UpdateInfoReCaret;
        Style.UpdateInfoReScroll;
      end;
  end;
end;

procedure THCCustomRichData.KeyPress(var Key: Char);
var
  vCarteItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
  vFormatFirstItemNo, vFormatLastItemNo: Integer;
begin
  if not CanEdit then Exit;

  DeleteSelected;

  vCarteItem := GetCurItem;
  if not Assigned(vCarteItem) then Exit;  // 跨页合并时，合并后并没有当前Item

  if (vCarteItem.StyleNo < THCStyle.Null)  // 当前位置是 RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // 在其上输入内容
  then
  begin
    Undo_StartRecord;
    Undo_ItemSelf(SelectInfo.StartItemNo, OffsetInner);

    vRectItem := vCarteItem as THCCustomRectItem;
    vRectItem.KeyPress(Key);
    if vRectItem.SizeChanged then
    begin
      vRectItem.SizeChanged := False;
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
      if Key <> #0 then
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);

      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
      Style.UpdateInfoReScroll;
    end;
  end
  else
    InsertText(Key);
end;

procedure THCCustomRichData.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not CanEdit then Exit;
end;

procedure THCCustomRichData.KillFocus;
var
  vItemNo: Integer;
begin
  vItemNo := GetCurItemNo;
  if vItemNo >= 0 then
    Items[vItemNo].KillFocus;
end;

procedure THCCustomRichData.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  if not CanEdit then Exit;

  //Self.InitializeField;  LoadFromStream中的Clear处理了
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  InsertStream(AStream, AStyle, AFileVersion);
  // 加载完成后，初始化(有一部分在LoadFromStream中初始化了)
  ReSetSelectAndCaret(0, 0);
end;

function THCCustomRichData.LoadItemFromStreamAlone(
  const AStream: TStream): THCCustomItem;
var
  vFileExt, vFileVersion: string;
  viVersion: Word;
  vStyleNo, vParaNo: Integer;
  vTextStyle: THCTextStyle;
  vParaStyle: THCParaStyle;
begin
  AStream.Position := 0;
  _LoadFileFormatAndVersion(AStream, vFileExt, vFileVersion);  // 文件格式和版本
  if (vFileExt <> HC_EXT) and (vFileExt <> 'cff.') then
    raise Exception.Create('加载失败，不是' + HC_EXT + '文件！');

  viVersion := GetVersionAsInteger(vFileVersion);

  AStream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));
  Result := CreateItemByStyle(vStyleNo);
  Result.LoadFromStream(AStream, Style, viVersion);

  if vStyleNo > THCStyle.Null then
  begin
    vTextStyle := THCTextStyle.Create;
    try
      vTextStyle.LoadFromStream(AStream, viVersion);
      vStyleNo := Style.GetStyleNo(vTextStyle, True);
      Result.StyleNo := vStyleNo;
    finally
      FreeAndNil(vTextStyle);
    end;
  end;

  vParaStyle := THCParaStyle.Create;
  try
    vParaStyle.LoadFromStream(AStream, viVersion);
    vParaNo := Style.GetParaNo(vParaStyle, True);
  finally
    FreeAndNil(vParaStyle);
  end;

  Result.ParaNo := vParaNo;
end;

function THCCustomRichData.MergeItemText(const ADestItem,
  ASrcItem: THCCustomItem): Boolean;
begin
  Result := ADestItem.CanConcatItems(ASrcItem);
  if Result then
    ADestItem.Text := ADestItem.Text + ASrcItem.Text;
end;

function THCCustomRichData.MergeTableSelectCells: Boolean;
var
  vItemNo, vFormatFirstItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;

  if not CanEdit then Exit;

  vItemNo := GetCurItemNo;
  if Items[vItemNo].StyleNo = THCStyle.Table then
  begin
    Undo_StartRecord;
    Undo_ItemSelf(vItemNo, OffsetInner);
    Result := (Items[vItemNo] as THCTableItem).MergeSelectCells;
    if Result then  // 合并成功
    begin
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
      DisSelect;  // 合并后清空选中，会导致当前ItemNo没有了
      InitializeMouseField;  // 201807311101
      Style.UpdateInfoRePaint;
    end;
  end;
end;

procedure THCCustomRichData.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  {$REGION 'DoItemMouseDown'}
  procedure DoItemMouseDown(const AItemNo, AOffset: Integer);
  var
    vX, vY: Integer;
  begin
    if AItemNo < 0 then Exit;
    CoordToItemOffset(X, Y, AItemNo, AOffset, vX, vY);
    Items[AItemNo].MouseDown(Button, Shift, vX, vY);

    if Assigned(OnItemMouseDown) then
      OnItemMouseDown(Self, AItemNo, Button, Shift, vX, vY);
  end;
  {$ENDREGION}

var
  vMouseDownItemNo, vMouseDownItemOffset, vDrawItemNo: Integer;
  vRestrain, vMouseDownInSelect: Boolean;
begin
  FSelecting := False;  // 准备划选
  FDraging := False;  // 准备拖拽
  FMouseLBDouble := False;
  FMouseDownReCaret := False;
  FSelectSeekOffset := -1;

  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);

  FMouseDownX := X;
  FMouseDownY := Y;

  GetItemAt(X, Y, vMouseDownItemNo, vMouseDownItemOffset, vDrawItemNo, vRestrain);

  vMouseDownInSelect := CoordInSelect(X, Y, vMouseDownItemNo, vMouseDownItemOffset, vRestrain);

  if vMouseDownInSelect then   // 在选中区域中按下
  begin
    if FMouseLBDowning then  // 是左键，开始拖拽
    begin
      FDraging := True;
      Style.UpdateInfo.Draging := True;
    end;

    if Items[vMouseDownItemNo].StyleNo < THCStyle.Null then  // 在RectItem上拖拽
      DoItemMouseDown(vMouseDownItemNo, vMouseDownItemOffset);
  end
  else  // 没点在选中区域中
  begin
    if SelectInfo.StartItemNo >= 0 then  // 旧按下的或者方向键移入的取消激活
    begin
      if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
        (Items[SelectInfo.StartItemNo] as THCCustomRectItem).DisSelect;

      Style.UpdateInfoRePaint;  // 旧的去焦点，新的入焦点
    end;

    if (vMouseDownItemNo <> FMouseDownItemNo)
      or (vMouseDownItemOffset <> FMouseDownItemOffset)
      or (CaretDrawItemNo <> vDrawItemNo)
    then  // 位置发生变化
    begin
      Style.UpdateInfoReCaret;
      FMouseDownReCaret := True;

      DisSelect;

      // 重新赋值新位置
      FMouseDownItemNo := vMouseDownItemNo;
      FMouseDownItemOffset := vMouseDownItemOffset;
      {if not vRestrain then  // 没收敛
        Items[FMouseDownItemNo].Active := True;}

      SelectInfo.StartItemNo := FMouseDownItemNo;
      SelectInfo.StartItemOffset := FMouseDownItemOffset;
      CaretDrawItemNo := vDrawItemNo;
    end;

    //if not vRestrain then  // 没收敛，因跨页Item点击在前后位置时需要处理光标数据所以不能限制
      DoItemMouseDown(FMouseDownItemNo, FMouseDownItemOffset);
  end;
end;

procedure THCCustomRichData.MouseLeave;
begin
  if FMouseMoveItemNo >= 0 then
  begin
    DoItemMouseLeave(FMouseMoveItemNo);
    FMouseMoveItemNo := -1;
    FMouseMoveItemOffset := -1;
    Style.UpdateInfoRePaint;
  end;
end;

procedure THCCustomRichData.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  //vOldMouseMoveItemOffset,
  vMoveDrawItemNo: Integer;

  {$REGION ' AdjustSelectRang '}
  procedure AdjustSelectRang;
  var
    i, vOldStartItemNo, vOldEndItemNo: Integer;
    vLeftToRight: Boolean;
  begin
    vLeftToRight := False;
    // 记录原来选中范围
    vOldStartItemNo := SelectInfo.StartItemNo;
    vOldEndItemNo := SelectInfo.EndItemNo;

    if FMouseDownItemNo < FMouseMoveItemNo then  // 从前往后选择在不同的Item
    begin
      vLeftToRight := True;

      if FMouseDownItemOffset = GetItemAfterOffset(FMouseDownItemNo) then  // 起始在Item最后面，改为下一个Item开始
      begin
        if FMouseDownItemNo < Items.Count - 1 then  // 起始改为下一个Item开始
        begin
          FMouseDownItemNo := FMouseDownItemNo + 1;
          FMouseDownItemOffset := 0;
        end;
      end;

      if (FMouseDownItemNo <> FMouseMoveItemNo) and (FMouseMoveItemNo >= 0)
        and (FMouseMoveItemOffset = 0)
      then  // 结束在Item最前面，改为上一个Item结束
      begin
        Items[FMouseMoveItemNo].DisSelect;  // 从前往后选，鼠标移动到前一次前面，原鼠标处被移出选中范围

        FMouseMoveItemNo := FMouseMoveItemNo - 1;
        FMouseMoveItemOffset := GetItemAfterOffset(FMouseMoveItemNo);
      end;
    end
    else
    if FMouseMoveItemNo < FMouseDownItemNo then  // 从后往前选择在不同的Item
    begin
      vLeftToRight := False;

      if (FMouseDownItemNo > 0) and (FMouseDownItemOffset = 0) then  // 起始在Item最前面，改为上一个Item结束
      begin
        FMouseDownItemNo := FMouseDownItemNo - 1;
        FMouseDownItemOffset := GetItemAfterOffset(FMouseDownItemNo);
      end;

      if (FMouseDownItemNo <> FMouseMoveItemNo)
        and (FMouseMoveItemOffset = GetItemAfterOffset(FMouseMoveItemNo))
      then  // 结束在Item最后面，改为下一个Item开始
      begin
        Items[FMouseMoveItemNo].DisSelect;  // 从后往前选，鼠标移动到前一个后面，原鼠标处被移出选中范围

        if FMouseMoveItemNo < Items.Count - 1 then  // 改为下一个Item开始
        begin
          FMouseMoveItemNo := FMouseMoveItemNo + 1;
          FMouseMoveItemOffset := 0;
        end;
      end;
    end;

    if FMouseDownItemNo = FMouseMoveItemNo then  // 选择操作在同一个Item中进行
    begin
      if FMouseMoveItemOffset > FMouseDownItemOffset then  // 选中结束位置大于起始位置
      begin
        if Items[FMouseDownItemNo].StyleNo < THCStyle.Null then  // RectItem
        begin
          SelectInfo.StartItemNo := FMouseDownItemNo;
          SelectInfo.StartItemOffset := FMouseDownItemOffset;

          if (FMouseDownItemOffset = OffsetBefor)
            and (FMouseMoveItemOffset = OffsetAfter)
          then  // 从RectItem最前面选择到了最后面(全选中)
          begin
            SelectInfo.EndItemNo := FMouseMoveItemNo;
            SelectInfo.EndItemOffset := FMouseMoveItemOffset;
          end
          else  // 没有全选中
          begin
            SelectInfo.EndItemNo := -1;
            SelectInfo.EndItemOffset := -1;
            CaretDrawItemNo := vMoveDrawItemNo;
          end;
        end
        else  // TextItem
        begin
          SelectInfo.StartItemNo := FMouseDownItemNo;
          SelectInfo.StartItemOffset := FMouseDownItemOffset;
          SelectInfo.EndItemNo := FMouseDownItemNo;
          SelectInfo.EndItemOffset := FMouseMoveItemOffset;
        end;
      end
      else
      if FMouseMoveItemOffset < FMouseDownItemOffset then  // 选中结束位置小于起始位置
      begin
        if Items[FMouseDownItemNo].StyleNo < THCStyle.Null then  // RectItem
        begin
          if FMouseMoveItemOffset = OffsetBefor then  // 从后往前选到最前面了
          begin
            SelectInfo.StartItemNo := FMouseDownItemNo;
            SelectInfo.StartItemOffset := FMouseMoveItemOffset;
            SelectInfo.EndItemNo := FMouseDownItemNo;
            SelectInfo.EndItemOffset := FMouseDownItemOffset;
          end
          else  // 从后往前选到OffsetInner了
          begin
            SelectInfo.StartItemNo := FMouseDownItemNo;
            SelectInfo.StartItemOffset := FMouseDownItemOffset;
            SelectInfo.EndItemNo := -1;
            SelectInfo.EndItemOffset := -1;

            CaretDrawItemNo := vMoveDrawItemNo;
          end;
        end
        else  // TextItem
        begin
          SelectInfo.StartItemNo := FMouseMoveItemNo;
          SelectInfo.StartItemOffset := FMouseMoveItemOffset;
          SelectInfo.EndItemNo := FMouseMoveItemNo;
          SelectInfo.EndItemOffset := FMouseDownItemOffset;
        end;
      end
      else  // 结束位置和起始位置相同(同一个Item)
      begin
        if SelectInfo.EndItemNo >= 0 then  // 同一Item中划选回到起始位置
          Items[SelectInfo.EndItemNo].DisSelect;

        SelectInfo.StartItemNo := FMouseDownItemNo;
        SelectInfo.StartItemOffset := FMouseDownItemOffset;
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;

        CaretDrawItemNo := vMoveDrawItemNo;
      end;
    end
    else  // 选择操作不在同一个Item
    begin
      if vLeftToRight then
      begin
        SelectInfo.StartItemNo := FMouseDownItemNo;
        SelectInfo.StartItemOffset := FMouseDownItemOffset;
        SelectInfo.EndItemNo := FMouseMoveItemNo;
        SelectInfo.EndItemOffset := FMouseMoveItemOffset;
      end
      else
      begin
        SelectInfo.StartItemNo := FMouseMoveItemNo;
        SelectInfo.StartItemOffset := FMouseMoveItemOffset;
        SelectInfo.EndItemNo := FMouseDownItemNo;
        SelectInfo.EndItemOffset := FMouseDownItemOffset;
      end;
    end;

    // 新选中范围外的清除选中
    if vOldStartItemNo >= 0 then  // 有旧选中Item
    begin
      if vOldStartItemNo > SelectInfo.StartItemNo then
      begin
        for i := vOldStartItemNo downto SelectInfo.StartItemNo + 1 do
          Items[i].DisSelect;
      end
      else
      begin
        for i := vOldStartItemNo to SelectInfo.StartItemNo - 1 do
          Items[i].DisSelect;
      end;
    end;

    if SelectInfo.EndItemNo < 0 then  // 有选中变成无选中
    begin
      for i := vOldEndItemNo downto SelectInfo.StartItemNo + 1 do  // 当前后面的取消选中
        Items[i].DisSelect;
    end
    else  // 有选中结束
    begin
      for i := vOldEndItemNo downto SelectInfo.EndItemNo + 1 do  // 原结束倒序到现结束下一个的取消选中
        Items[i].DisSelect;
    end;
  end;
  {$ENDREGION}

  {$REGION 'DoItemMouseMove'}
  procedure DoItemMouseMove(const AItemNo, AOffset: Integer);
  var
    vX, vY: Integer;
  begin
    if AItemNo < 0 then Exit;
    CoordToItemOffset(X, Y, AItemNo, AOffset, vX, vY);
    Items[AItemNo].MouseMove(Shift, vX, vY);
  end;
  {$ENDREGION}

var
  vMouseMoveItemNo, vMouseMoveItemOffset: Integer;
  vRestrain: Boolean;
begin
  if SelectedResizing then  // RectItem缩放ing，继续缩放
  begin
    FMouseMoveItemNo := FMouseDownItemNo;
    FMouseMoveItemOffset := FMouseDownItemOffset;
    FMouseMoveRestrain := False;
    DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
    Style.UpdateInfoRePaint;

    Exit;
  end;

  //vOldMouseMoveItemOffset := FMouseMoveItemOffset;

  GetItemAt(X, Y, vMouseMoveItemNo, vMouseMoveItemOffset, vMoveDrawItemNo, vRestrain);

  if FDraging or Style.UpdateInfo.Draging then  // 拖拽
  begin
    GCursor := crDrag;

    FMouseMoveItemNo := vMouseMoveItemNo;
    FMouseMoveItemOffset := vMouseMoveItemOffset;
    FMouseMoveRestrain := vRestrain;
    CaretDrawItemNo := vMoveDrawItemNo;

    Style.UpdateInfoReCaret;

    if (not vRestrain) and (Items[FMouseMoveItemNo].StyleNo < THCStyle.Null) then  // RectItem
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
  end
  else
  if FSelecting then  // 划选
  begin
    FMouseMoveItemNo := vMouseMoveItemNo;
    FMouseMoveItemOffset := vMouseMoveItemOffset;
    FMouseMoveRestrain := vRestrain;
    FSelectSeekNo := vMouseMoveItemNo;
    FSelectSeekOffset := vMouseMoveItemOffset;

    AdjustSelectRang;  // 确定SelectRang
    MatchItemSelectState;  // 设置选中范围内的Item选中状态
    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;

    if (not vRestrain) and (Items[FMouseMoveItemNo].StyleNo < THCStyle.Null) then  // RectItem
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
  end
  else  // 非拖拽，非划选
  if FMouseLBDowning and ((FMouseDownX <> X) or (FMouseDownY <> Y)) then  // 左键按下移动，开始划选
  begin
    FSelecting := True;
    Style.UpdateInfo.Selecting := True;
  end
  else  // 非拖拽，非划选，非按下
  begin
    if vMouseMoveItemNo <> FMouseMoveItemNo then  // 移动到了新的Item上
    begin
      if FMouseMoveItemNo >= 0 then  // 旧的移出
        DoItemMouseLeave(FMouseMoveItemNo);
      if (vMouseMoveItemNo >= 0) and (not vRestrain) then  // 新的移入
        DoItemMouseEnter(vMouseMoveItemNo);

      Style.UpdateInfoRePaint;
    end
    else  // 本次移动到的Item和上一次是同一个(不代表一直在一个Item上移动)
    begin
      if vRestrain <> FMouseMoveRestrain then  // 本次Move和上次Move是同一个Item，2次的收敛发生了变化
      begin
        if (not FMouseMoveRestrain) and vRestrain then  // 上次没收敛，本次收敛了，移出
        begin
          if FMouseMoveItemNo >= 0 then
            DoItemMouseLeave(FMouseMoveItemNo);
        end
        else
        if FMouseMoveRestrain and (not vRestrain) then  // 上次收敛，本次不收敛，移入
        begin
          if vMouseMoveItemNo >= 0 then
            DoItemMouseEnter(vMouseMoveItemNo);
        end;

        Style.UpdateInfoRePaint;
      end;
    end;

    FMouseMoveItemNo := vMouseMoveItemNo;
    FMouseMoveItemOffset := vMouseMoveItemOffset;
    FMouseMoveRestrain := vRestrain;

    if not vRestrain then
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
  end;
end;

procedure THCCustomRichData.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vUpItemNo, vUpItemOffset, vDrawItemNo: Integer;

  {$REGION ' DoItemMouseUp '}
  procedure DoItemMouseUp(const AItemNo, AOffset: Integer);
  var
    vX, vY: Integer;
  begin
    if AItemNo < 0 then Exit;
    CoordToItemOffset(X, Y, AItemNo, AOffset, vX, vY);
    Items[AItemNo].MouseUp(Button, Shift, vX, vY);

    if Assigned(FOnItemMouseUp) then
      FOnItemMouseUp(Self, AItemNo, Button, Shift, vX, vY);
  end;
  {$ENDREGION}

  {$REGION ' DoNormalMouseUp '}
  procedure DoNormalMouseUp;
  begin
    if FMouseMoveItemNo < 0 then
    begin
      SelectInfo.StartItemNo := vUpItemNo;
      SelectInfo.StartItemOffset := vUpItemOffset;
    end
    else
    begin
      SelectInfo.StartItemNo := FMouseMoveItemNo;
      SelectInfo.StartItemOffset := FMouseMoveItemOffset;
    end;

    CaretDrawItemNo := vDrawItemNo;
    Style.UpdateInfoRePaint;

    if not FMouseDownReCaret then  // 避免重复获取光标位置
      Style.UpdateInfoReCaret;

    if Items[vUpItemNo].StyleNo < THCStyle.Null then  // RectItem
      DoItemMouseUp(vUpItemNo, vUpItemOffset);  // 弹起，因为可能是移出Item后弹起，所以这里不受vRestrain约束
  end;
  {$ENDREGION}

var
  i, vFormatFirstItemNo, vFormatLastItemNo: Integer;
  vRestrain: Boolean;
  vMouseUpInSelect: Boolean;
begin
  if not FMouseLBDowning then Exit;  // 屏蔽OpenDialog对话框双击引起的弹起
  FMouseLBDowning := False;

  if FMouseLBDouble then Exit;

  if SelectedResizing then  // RectItem缩放ing，停止缩放
  begin
    Undo_StartRecord;
    Undo_ItemSelf(FMouseDownItemNo, FMouseDownItemOffset);

    DoItemMouseUp(FMouseDownItemNo, FMouseDownItemOffset);
    DoItemResized(FMouseDownItemNo);  // 缩放完成事件(可控制缩放不要超过页面)
    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, FMouseDownItemNo, FMouseDownItemOffset);

    if (vFormatFirstItemNo > 0) and (not Items[vFormatFirstItemNo].ParaFirst) then  // 缩放时宽度变窄，可能会在上一行后面放的下
    begin
      Dec(vFormatFirstItemNo);
      vFormatFirstItemNo := GetLineFirstItemNo(vFormatFirstItemNo, 0);
    end;

    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
    Style.UpdateInfoRePaint;

    Exit;
  end;

  GetItemAt(X, Y, vUpItemNo, vUpItemOffset, vDrawItemNo, vRestrain);

  if FSelecting or Style.UpdateInfo.Selecting then  // 划选完成弹起
  begin
    FSelecting := False;

    // 选中范围内的RectItem取消划选状态(此时表格的FSelecting为True)
    //if SelectInfo.StartItemNo >= 0 then
    begin
      for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
      begin
        if (i <> vUpItemNo) and (Items[i].StyleNo < THCStyle.Null) then
          DoItemMouseUp(i, 0);
      end;
    end;

    if Items[vUpItemNo].StyleNo < THCStyle.Null then  // 弹起时在RectItem
      DoItemMouseUp(vUpItemNo, vUpItemOffset);
  end
  else
  if FDraging or Style.UpdateInfo.Draging then  // 拖拽弹起
  begin
    FDraging := False;
    vMouseUpInSelect := CoordInSelect(X, Y, vUpItemNo, vUpItemOffset, vRestrain);

    // 暂时不支持拖拽
    {if not vMouseUpInSelect then  // 拖拽弹起时不在选中内容中
    begin
      //to do: 取拖拽选中的内容
      DeleteSelected;  // 删除选中内容
    end
    else}  // 拖拽弹起时在选中内容中
    begin
      // 清除弹起位置之外的Item选中状态，弹起处自己处理，弹起处不在选中范围内时
      // 保证弹起处取消(从ItemA上选中拖拽到另一个ItemB时，ItemA选中状态需要取消)
      // 与201805172309相似
      if SelectInfo.StartItemNo >= 0 then  // 弹起时的单元格并不是按下时的，会出现SelectInfo.StartItemNo < 0的情况
      begin
        if SelectInfo.StartItemNo <> vUpItemNo then
        begin
          Items[SelectInfo.StartItemNo].DisSelect;
          //Items[SelectInfo.StartItemNo].Active := False;
        end;
        // 选中范围内其他Item取消选中
        for i := SelectInfo.StartItemNo + 1 to SelectInfo.EndItemNo do  // 遍历弹起位置之外的其他Item
        begin
          if i <> vUpItemNo then
          begin
            Items[i].DisSelect;
            //Items[i].Active := False;
          end;
        end;
      end;
    end;

    // 为拖拽光标准备
    FMouseMoveItemNo := vUpItemNo;
    FMouseMoveItemOffset := vUpItemOffset;
    // 为下一次点击时清除上一次点击选中做准备
    FMouseDownItemNo := vUpItemNo;
    FMouseDownItemOffset := vUpItemOffset;

    DoNormalMouseUp;  // 弹起处自己处理Item选中状态，并以弹起处为当前编辑位置

    SelectInfo.EndItemNo := -1;
    SelectInfo.EndItemOffset := -1;
  end
  else  // 非拖拽、非划选
  begin
    if SelectExists(False) then  // 清除本Data层面内的选中
      DisSelect;

    DoNormalMouseUp;
  end;
end;

procedure THCCustomRichData.ReFormatData_(const AStartItemNo: Integer; const ALastItemNo: Integer = -1;
  const AExtraItemCount: Integer = 0);
var
  i, vLastItemNo, vLastDrawItemNo, vFormatIncHight,
  vDrawItemCount, vFmtTopOffset, vClearFmtHeight: Integer;
begin
  vDrawItemCount := DrawItems.Count;
  if ALastItemNo < 0 then
    FormatData(AStartItemNo, AStartItemNo)
  else
    FormatData(AStartItemNo, ALastItemNo);  // 格式化指定范围内的Item
  DrawItems.DeleteFormatMark;
  vDrawItemCount := DrawItems.Count - vDrawItemCount;

  // 计算格式化后段的底部位置变化
  if ALastItemNo < 0 then
    vLastDrawItemNo := GetItemLastDrawItemNo(AStartItemNo)
  else
    vLastDrawItemNo := GetItemLastDrawItemNo(ALastItemNo);
  vFormatIncHight := DrawItems[vLastDrawItemNo].Rect.Bottom - DrawItems.FormatBeforBottom;  // 段格式化后，高度的增量

  // 某段格式化后，处理对其后面Item对应DrawItem的影响
  // 由图2017-6-8_1变为图2017-6-8_2的过程中，第3段位置没变，也没有新的Item数量变化，
  // 但是DrawItem的数量有变化
  // 第3段Item对应的FirstDItemNo需要修改，所以此处增加DrawItemCount数量的变化
  // 目前格式化时ALastItemNo为段的最后一个，所以vLastDrawItemNo为段最后一个DrawItem
  if (vFormatIncHight <> 0) or (AExtraItemCount <> 0) or (vDrawItemCount <> 0) then
  begin
    if DrawItems.Count > vLastDrawItemNo then
    begin
      vLastItemNo := -1;
      for i := vLastDrawItemNo + 1 to DrawItems.Count - 1 do  // 从格式化变动段的下一段开始
      begin
        // 处理格式化后面各DrawItem对应的ItemNo偏移
        DrawItems[i].ItemNo := DrawItems[i].ItemNo + AExtraItemCount;
        if vLastItemNo <> DrawItems[i].ItemNo then
        begin
          vLastItemNo := DrawItems[i].ItemNo;
          Items[vLastItemNo].FirstDItemNo := i;
        end;

        if vFormatIncHight <> 0 then  // 这里能确认为0时不需要重新处理偏移吗？
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
end;

procedure THCCustomRichData.ReSetSelectAndCaret(const AItemNo, AOffset: Integer;
  const ANextWhenMid: Boolean = False);
var
  vDrawItemNo: Integer;
begin
  SelectInfo.StartItemNo := AItemNo;
  SelectInfo.StartItemOffset := AOffset;

  vDrawItemNo := GetDrawItemNoByOffset(AItemNo, AOffset);
  if ANextWhenMid
    and (vDrawItemNo < DrawItems.Count - 1)
    and (DrawItems[vDrawItemNo + 1].ItemNo = AItemNo)
    and (DrawItems[vDrawItemNo + 1].CharOffs = AOffset + 1)
  then
    Inc(vDrawItemNo);

  CaretDrawItemNo := vDrawItemNo;
end;

procedure THCCustomRichData.ReSetSelectAndCaret(const AItemNo: Integer);
begin
  ReSetSelectAndCaret(AItemNo, GetItemAfterOffset(AItemNo));
end;

procedure THCCustomRichData.SaveItemToStreamAlone(const AItem: THCCustomItem;
  const AStream: TStream);
begin
  _SaveFileFormatAndVersion(AStream);
  AItem.SaveToStream(AStream);
  if AItem.StyleNo > THCStyle.Null then
    Style.TextStyles[AItem.StyleNo].SaveToStream(AStream);

  Style.ParaStyles[AItem.ParaNo].SaveToStream(AStream);
end;

procedure THCCustomRichData.SetEmptyData;
var
  vItem: THCCustomItem;
begin
  if Self.Items.Count = 0 then
  begin
    vItem := CreateDefaultTextItem;
    vItem.ParaFirst := True;
    Items.Add(vItem);  // 不使用InsertText，为避免其触发ReFormat时因为没有格式化过，获取不到对应的DrawItem

    FormatData(0, 0);
    ReSetSelectAndCaret(0);
  end;
end;

procedure THCCustomRichData.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure THCCustomRichData.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
    FWidth := Value;
end;

procedure THCCustomRichData._FormatReadyParam(const AStartItemNo: Integer;
  var APrioDrawItemNo: Integer; var APos: TPoint);
{var
  i, vEndDrawItemNo: Integer;}
begin
  { 获取起始DrawItem的上一个序号及格式化开始位置 }
  if AStartItemNo > 0 then  // 不是第一个
  begin
    APrioDrawItemNo := GetItemLastDrawItemNo(AStartItemNo - 1);  // 上一个最后的DItem
    if Items[AStartItemNo].ParaFirst then
    begin
      APos.X := 0;
      APos.Y := DrawItems[APrioDrawItemNo].Rect.Bottom;
    end
    else
    begin
      APos.X := DrawItems[APrioDrawItemNo].Rect.Right;
      APos.Y := DrawItems[APrioDrawItemNo].Rect.Top;
    end;
  end
  else  // 是第一个
  begin
    APrioDrawItemNo := -1;
    APos.X := 0;
    APos.Y := 0;
  end;
end;

end.


