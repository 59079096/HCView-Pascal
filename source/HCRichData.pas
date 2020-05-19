{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{             支持格式化文档对象管理单元                }
{                                                       }
{*******************************************************}

{******************* 代码修改说明 ***********************
201807311101 在行首插入内容后光标后移，按下时并没有改变ItemNo和Offset，导致光标
             不重新回到行首
}

unit HCRichData;

interface

{$I HCView.inc}

uses
  Windows, Classes, Types, Controls, Graphics, SysUtils, HCCustomData, HCStyle,
  HCItem, HCDrawItem, HCTextStyle, HCParaStyle, HCStyleMatch, HCCommon, HCRectItem,
  HCTextItem, HCUndoData, HCXml;

type
  TTextItemActionEvent = reference to function(const ATextItem: THCTextItem): Boolean;
  TRectItemActionEvent = reference to function(const ARectItem: THCCustomRectItem): Boolean;
  TItemMouseEvent = procedure(const AData: THCCustomData; const AItemNo, AOffset: Integer;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TDrawItemMouseEvent = procedure(const AData: THCCustomData; const AItemNo, AOffset,
    ADrawItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  THCRichData = class(THCUndoData)  // 富文本数据类，可做为其他显示富文本类的基类
  strict private
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
    FMouseMoveDrawItemNo,

    FSelectSeekNo,
    FSelectSeekOffset  // 选中操作时的游标
      : Integer;

    FReadOnly,
    FSelecting, FDraging: Boolean;

    FOnItemResized: TDataItemNoEvent;
    FOnItemMouseDown, FOnItemMouseUp: TItemMouseEvent;
    FOnDrawItemMouseMove: TDrawItemMouseEvent;
    FOnCreateItem: TNotifyEvent;  // 新建了Item(目前主要是为了打字和用中文输入法输入英文时痕迹的处理)
    FOnAcceptAction: TDataActionEvent;

    /// <summary> Shift按键按下时鼠标点击，根据按下位置适配选择范围 </summary>
    /// <param name="AMouseDonwItemNo"></param>
    /// <param name="AMouseDownItemOffset"></param>
    function SelectByMouseDownShift(var AMouseDownItemNo, AMouseDownItemOffset: Integer): Boolean;

    /// <summary> 初始化为只有一个空Item的Data</summary>
    procedure SetEmptyData;

    /// <summary> Data只有空行Item时插入Item(用于替换当前空行Item的情况) </summary>
    function EmptyDataInsertItem(const AItem: THCCustomItem): Boolean;

    /// <summary> 是否完美选中（判断文本和Rect混合选择时，Rect全选中。无选中是返回true）</summary>
    function SelectPerfect: Boolean;

    /// <summary> 当前TextItem内部变化后重新格式化（避免大量重复代码） </summary>
    function TextItemAction(const AAction: TTextItemActionEvent): Boolean;

    /// <summary> 当前RectItem内部变化后重新格式化（避免量重复代码） </summary>
    function RectItemAction(const AAction: TRectItemActionEvent): Boolean;

    /// <summary> 初始化鼠标相关字段 </summary>
    procedure InitializeMouseField;

    /// <summary> 划完完成后最后操作位置是否在选中范围起始 </summary>
    function IsSelectSeekStart: Boolean;
  protected
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;

    /// <summary> 是否接受指定的事件 </summary>
    function DoAcceptAction(const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean; virtual;
    procedure DoDrawItemMouseMove(const AData: THCCustomData; const AItemNo, AOffset,
      ADrawItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    /// <summary> 用于从流加载完Items后，检查不合格的Item并删除 </summary>
    function CheckInsertItemCount(const AStartNo, AEndNo: Integer): Integer; virtual;
    procedure DoItemMouseLeave(const AItemNo: Integer); virtual;
    procedure DoItemMouseEnter(const AItemNo: Integer); virtual;
    procedure DoItemResized(const AItemNo: Integer);
    function GetHeight: Cardinal; virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;

    /// <summary> 给定起始结束位置，判断正确的选中位置并修正输出 </summary>
    /// <param name="ADrawItemNo">光标处的DrawItem(暂时无意义)</param>
    /// <param name="AStartItemNo"></param>
    /// <param name="AStartItemOffset"></param>
    /// <param name="AEndItemNo"></param>
    /// <param name="AEndItemNoOffset"></param>
    procedure AdjustSelectRange(var AStartItemNo, AStartItemOffset, AEndItemNo, AEndItemNoOffset: Integer);

    property MouseMoveDrawItemNo: Integer read FMouseMoveDrawItemNo;
  public
    constructor Create(const AStyle: THCStyle); override;
    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; override;

    /// <summary> 在Data层面是否可编辑 </summary>
    function CanEdit: Boolean; override;

    procedure Clear; override;
    // 选中内容应用样式
    procedure ApplySelectTextStyle(const AMatchStyle: THCStyleMatch); override;
    procedure ApplySelectParaStyle(const AMatchStyle: THCParaMatch); override;
    procedure ApplyTableCellAlign(const AAlign: THCContentAlign); override;

    function DisSelect: Boolean; override;

    /// <summary> 是否成功删除了选中内容（无选中返回true） </summary>
    /// <returns>True:有选中且删除成功</returns>
    function DeleteSelected: Boolean; override;

    /// <summary> 初始化相关字段和变量 </summary>
    procedure InitializeField; override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

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

    /// <summary> 直接设置当前TextItem的Text值 </summary>
    procedure SetActiveItemText(const AText: string);

    procedure KillFocus; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual;

    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyPress(var Key: Char); virtual;

    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyDown(var Key: Word; Shift: TShiftState;
      const APageBreak: Boolean = False); virtual;

    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;

    function DoInsertTextBefor(const AItemNo, AOffset: Integer;
      const AText: string): Boolean; virtual;
    //
    procedure DblClick(X, Y: Integer);
    procedure DeleteItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean);
    procedure DeleteActiveDataItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean);
    /// <summary> 添加Data到当前 </summary>
    /// <param name="ASrcData">源Data</param>
    procedure AddData(const ASrcData: THCCustomData);

    /// <summary> 在光标处换行 </summary>
    function InsertBreak: Boolean;

    /// <summary> 在光标处插入字符串(可带回车换行符) </summary>
    function InsertText(const AText: string): Boolean;

    /// <summary> 在光标处插入指定行列的表格 </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
    function InsertImage(const AImage: TGraphic): Boolean;
    function InsertGifImage(const AFile: string): Boolean;

    /// <summary> 在光标处插入直线 </summary>
    function InsertLine(const ALineHeight: Integer): Boolean;
    function SetActiveImage(const AImageStream: TStream): Boolean;
    function ActiveTableResetRowCol(const ARowCount, AColCount: Byte): Boolean;
    function TableInsertRowAfter(const ARowCount: Byte): Boolean;
    function TableInsertRowBefor(const ARowCount: Byte): Boolean;
    function ActiveTableDeleteCurRow: Boolean;
    function ActiveTableSplitCurRow: Boolean;
    function ActiveTableSplitCurCol: Boolean;
    function TableInsertColAfter(const AColCount: Byte): Boolean;
    function TableInsertColBefor(const AColCount: Byte): Boolean;
    function ActiveTableDeleteCurCol: Boolean;
    function MergeTableSelectCells: Boolean;
    function TableApplyContentAlign(const AAlign: THCContentAlign): Boolean;

    /// <summary> ActiveItem重新适应其环境(供外部直接修改Item属性后重新和其前后Item连接组合) </summary>
    procedure ActiveItemReAdaptEnvironment;

    /// <summary> 取消激活(用于页眉、页脚、正文切换时原激活的取消) </summary>
    procedure DisActive;

    function GetHint: string;

    property MouseDownItemNo: Integer read FMouseDownItemNo;
    property MouseDownItemOffset: Integer read FMouseDownItemOffset;
    property MouseMoveItemNo: Integer read FMouseMoveItemNo;
    property MouseMoveItemOffset: Integer read FMouseMoveItemOffset;
    property MouseMoveRestrain: Boolean read FMouseMoveRestrain;
    property HotDrawItemNo: Integer read FMouseMoveDrawItemNo;

    property Height: Cardinal read GetHeight;  // 实际内容的高
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Selecting: Boolean read FSelecting;
    property OnItemResized: TDataItemNoEvent read FOnItemResized write FOnItemResized;
    property OnItemMouseDown: TItemMouseEvent read FOnItemMouseDown write FOnItemMouseDown;
    property OnItemMouseUp: TItemMouseEvent read FOnItemMouseUp write FOnItemMouseUp;
    property OnDrawItemMouseMove: TDrawItemMouseEvent read FOnDrawItemMouseMove write FOnDrawItemMouseMove;
    property OnCreateItem: TNotifyEvent read FOnCreateItem write FOnCreateItem;
    property OnAcceptAction: TDataActionEvent read FOnAcceptAction write FOnAcceptAction;
  end;

implementation

uses
  Math, HCTableItem, HCImageItem, HCCheckBoxItem, HCTabItem, HCLineItem, HCExpressItem,
  HCGifItem, HCEditItem, HCComboboxItem, HCQRCodeItem, HCBarCodeItem, HCFloatLineItem,
  HCFractionItem, HCDateTimePicker, HCRadioGroup, HCSupSubScriptItem, HCUnitConversion,
  HCFloatBarCodeItem;

{ THCRichData }

constructor THCRichData.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  FReadOnly := False;
  InitializeField;
  SetEmptyData;
end;

function THCRichData.CreateItemByStyle(const AStyleNo: Integer): THCCustomItem;
begin
  Result := nil;
  if AStyleNo < THCStyle.Null then
  begin
    case AStyleNo of
      THCStyle.Image: Result := THCImageItem.Create(Self, 0, 0);
      THCStyle.Table: Result := THCTableItem.Create(Self, 1, 1, 1);
      THCStyle.Tab: Result := TTabItem.Create(Self, 0, 0);
      THCStyle.Line: Result := THCLineItem.Create(Self, 1, 1);
      THCStyle.Express: Result := THCExpressItem.Create(Self, '', '', '', '');
      // RsVector
      THCStyle.Domain: Result := CreateDefaultDomainItem;
      //THCStyle.PageBreak: Result := TPageBreakItem.Create(Self, 0, 1);
      THCStyle.CheckBox: Result := THCCheckBoxItem.Create(Self, '勾选框', False);
      THCStyle.Gif: Result := THCGifItem.Create(Self, 1, 1);
      THCStyle.Edit: Result := THCEditItem.Create(Self, '');
      THCStyle.Combobox: Result := THCComboboxItem.Create(Self, '');
      THCStyle.QRCode: Result := THCQRCodeItem.Create(Self, '');
      THCStyle.BarCode: Result := THCBarCodeItem.Create(Self, '');
      THCStyle.Fraction: Result := THCFractionItem.Create(Self, '', '');
      THCStyle.DateTimePicker: Result := THCDateTimePicker.Create(Self, Now);
      THCStyle.RadioGroup: Result := THCRadioGroup.Create(Self);
      THCStyle.SupSubScript: Result := THCSupSubScriptItem.Create(Self, '', '');
      // FloatItem
      THCStyle.FloatLine: Result := THCFloatLineItem.Create(Self);
      THCStyle.FloatBarCode: Result := THCFloatBarCodeItem.Create(Self);
    else
      raise Exception.Create('未找到类型 ' + IntToStr(AStyleNo) + ' 对应的创建Item代码！');
    end;
  end
  else
  begin
    Result := CreateDefaultTextItem;
    Result.StyleNo := AStyleNo;
  end;

  if Assigned(FOnCreateItem) then
    FOnCreateItem(Result);
end;

procedure THCRichData.DblClick(X, Y: Integer);
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
      vPosType := GetUnicodeCharType(vText[vItemOffset])
    else
      vPosType := GetUnicodeCharType(vText[1]);

    vStartOffset := 0;
    for i := vItemOffset - 1 downto 1 do  // 往前找Char类型不一样的位置
    begin
      if GetUnicodeCharType(vText[i]) <> vPosType then
      begin
        vStartOffset := i;
        Break;
      end;
    end;

    vEndOffset := Length(vText);
    for i := vItemOffset + 1 to Length(vText) do  // 往后找Char类型不一样的位置
    begin
      if GetUnicodeCharType(vText[i]) <> vPosType then
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

procedure THCRichData.DeleteActiveDataItems(const AStartNo, AEndNo: Integer;
  const AKeepPara: Boolean);
var
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
  vActiveItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
begin
  if (AStartNo < 0) or (AStartNo > Items.Count - 1) then Exit;

  vActiveItem := Items[AStartNo];

  if (vActiveItem.StyleNo < THCStyle.Null)  // 当前位置是 RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // 在其上输入内容
  then
  begin
    Undo_New;

    vRectItem := vActiveItem as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    vRectItem.DeleteActiveDataItems(AStartNo, AEndNo, AKeepPara);
    if vRectItem.SizeChanged then
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vRectItem.SizeChanged := False;
    end
    else
      Self.FormatInit;
  end
  else
    DeleteItems(AStartNo, AEndNo, AKeepPara);
end;

procedure THCRichData.DeleteItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean);
var
  i, vFormatFirstDrawItemNo, vFormatLastItemNo, vDelCount: Integer;
  vItem: THCCustomItem;
  vStartParaFirst: Boolean;
begin
  if not CanEdit then Exit;
  if AEndNo < AStartNo then Exit;

  Self.InitializeField;

  GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);

  if (Items[AStartNo].ParaFirst) and (vFormatFirstDrawItemNo > 0) then  // 段首删除时要从上一段最后
  begin
    Dec(vFormatFirstDrawItemNo);
    vFormatFirstDrawItemNo := GetFormatFirstDrawItem(vFormatFirstDrawItemNo);  // 从行首开始
  end;

  FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

  vStartParaFirst := Items[AStartNo].ParaFirst;
  vDelCount := AEndNo - AStartNo + 1;
  Undo_New;
  for i := AEndNo downto AStartNo do
  begin
    UndoAction_DeleteItem(i, 0);
    Items.Delete(i);
  end;

  if Items.Count = 0 then  // 删除没有了，不用SetEmptyData，因为其无Undo
  begin
    vItem := CreateDefaultTextItem;
    Self.CurStyleNo := vItem.StyleNo;
    vItem.ParaFirst := True;
    Items.Add(vItem);
    Dec(vDelCount);
    UndoAction_InsertItem(0, 0);
  end
  else
  if vStartParaFirst then // 段首删除了
  begin
    if (AStartNo < Items.Count - 1)  // 下一个不是最后一个
      and (not Items[AStartNo].ParaFirst)  // 下一个不是段首(同段还有内容，置首)
    then
    begin
      UndoAction_ItemParaFirst(AStartNo, 0, True);
      Items[AStartNo].ParaFirst := True;
    end
    else  // 段删除完了
    if AKeepPara then  // 保持段
    begin
      vItem := CreateDefaultTextItem;
      Self.CurStyleNo := vItem.StyleNo;
      vItem.ParaFirst := True;
      Items.Insert(AStartNo, vItem);
      Dec(vDelCount);
      UndoAction_InsertItem(AStartNo, 0);
    end;
  end;

  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;

  if vStartParaFirst and AKeepPara then
    ReSetSelectAndCaret(AStartNo, 0)
  else
  if AStartNo > 0 then  // 不是从第1个开始删除
    ReSetSelectAndCaret(AStartNo - 1)
  else  // 从第一个开始删除
    ReSetSelectAndCaret(0, 0);  // 光标置到现在的最前面，为其后插入内容做准备
end;

function THCRichData.DeleteSelected: Boolean;
var
  vDelCount, vFormatFirstItemNo, vFormatLastItemNo,
  vLen, vParaFirstItemNo, vParaLastItemNo: Integer;
  vStartItem, vEndItem, vNewItem: THCCustomItem;

  {$REGION '删除全选中的单个Item'}
  function DeleteItemSelectComplate: Boolean;
  begin
    Result := False;
    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actDeleteItem) then Exit;  // 允许删除

    UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
    Items.Delete(SelectInfo.StartItemNo);

    Inc(vDelCount);

    if (SelectInfo.StartItemNo > vFormatFirstItemNo)
      and (SelectInfo.StartItemNo < vFormatLastItemNo)
    then  // 全选中的Item在起始格式化和结束格式化中间
    begin
      vLen := Items[SelectInfo.StartItemNo - 1].Length;
      if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then  // 删除位置前后可合并
      begin
        UndoAction_InsertText(SelectInfo.StartItemNo - 1, vLen + 1, Items[SelectInfo.StartItemNo].Text);
        UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
        Items.Delete(SelectInfo.StartItemNo);
        Inc(vDelCount);

        SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
        SelectInfo.StartItemOffset := vLen;
      end
      else  // 删除位置前后不能合并，光标置为前一个后面
      begin
        SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
        SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
      end;
    end
    else
    if SelectInfo.StartItemNo = vParaFirstItemNo then  // 段第一个ItemNo
    begin
      if vParaFirstItemNo = vParaLastItemNo then  // 段就一个Item全删除了，补充空Item
      begin
        vNewItem := CreateDefaultTextItem;
        Self.CurStyleNo := vNewItem.StyleNo;  // 防止RectItem删除插入文本当前样式不正确
        vNewItem.ParaFirst := True;
        Items.Insert(SelectInfo.StartItemNo, vNewItem);
        UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

        SelectInfo.StartItemOffset := 0;
        Dec(vDelCount);
      end
      else
      begin
        SelectInfo.StartItemOffset := 0;

        UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, True);
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
        SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
      end;
    end
    else  // 全选中的Item是起始格式化或结束格式化或在段内
    begin  // 这里的代码会触发吗？
      if SelectInfo.StartItemNo > 0 then
      begin
        vLen := Items[SelectInfo.StartItemNo - 1].Length;
        if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then
        begin
          UndoAction_InsertText(SelectInfo.StartItemNo - 1, vLen + 1, Items[SelectInfo.StartItemNo].Text);
          UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
          Items.Delete(SelectInfo.StartItemNo);
          Inc(vDelCount);
          SelectInfo.StartItemOffset := vLen;
        end;

        SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
        SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);  // 删除选中再插入时如果是RectItem用原有Offset不行
      end;
    end;

    Result := True;
  end;
  {$ENDREGION}

var
  i, vFormatFirstDrawItemNo: Integer;
  vText: string;
  vSelectSeekStart,   // 选中范围游标在选中起始
  vSelStartComplate,  // 选中范围内的起始Item全选中了
  vSelEndComplate,    // 选中范围内的结束Item全选中了
  vSelStartParaFirst  // 选中起始是段首
    : Boolean;
begin
  Result := False;

  if not CanEdit then Exit;

  if not SelectPerfect then
  begin
    Self.DisSelect;
    Exit;
  end;

  if not SelectExists then
  begin
    Result := True;
    Exit;
  end;

  vSelectSeekStart := IsSelectSeekStart;

  vDelCount := 0;
  Self.InitializeField;  // 删除后原鼠标处可能已经没有了

  if (SelectInfo.EndItemNo < 0)
    and (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)
  then  // 选择仅发生在RectItem内部
  begin
    // 如果变动会引起RectItem的宽度变化，则需要格式化到段最后一个Item
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    if Self.FormatCount > 0 then
      vFormatFirstItemNo := SelectInfo.StartItemNo
    else
      vFormatFirstItemNo := DrawItems[vFormatFirstDrawItemNo].ItemNo;

    Undo_New;

    if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).IsSelectComplateTheory then  // 理论全选了
    begin
      GetParaItemRang(SelectInfo.StartItemNo, vParaFirstItemNo, vParaLastItemNo);
      Result := DeleteItemSelectComplate;
    end
    else
    begin
      if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

      Result := (Items[SelectInfo.StartItemNo] as THCCustomRectItem).DeleteSelected;
    end;

    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
  end
  else  // 选中不是发生在RectItem内部
  begin
    vEndItem := Items[SelectInfo.EndItemNo];  // 选中结束Item
    if SelectInfo.EndItemNo = SelectInfo.StartItemNo then  // 选择发生在同一个Item
    begin
      Undo_New;

      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      if Self.FormatCount > 0 then
        vFormatFirstItemNo := SelectInfo.StartItemNo
      else
        vFormatFirstItemNo := DrawItems[vFormatFirstDrawItemNo].ItemNo;

      if vEndItem.IsSelectComplate then  // 该TextItem全选中了
      begin
        GetParaItemRang(SelectInfo.StartItemNo, vParaFirstItemNo, vParaLastItemNo);
        Result := DeleteItemSelectComplate;
      end
      else  // Item部分选中
      begin
        if vEndItem.StyleNo < THCStyle.Null then  // 同一个RectItem  表格从前选中到一部分？
        begin
          if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).MangerUndo then
            UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
          else
            UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

          (vEndItem as THCCustomRectItem).DeleteSelected;
        end
        else  // 同一个TextItem
        if DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actBackDeleteText) then
        begin
          vText := vEndItem.Text;
          UndoAction_DeleteBackText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1,
            Copy(vText, SelectInfo.StartItemOffset + 1, SelectInfo.EndItemOffset - SelectInfo.StartItemOffset));
          Delete(vText, SelectInfo.StartItemOffset + 1, SelectInfo.EndItemOffset - SelectInfo.StartItemOffset);
          vEndItem.Text := vText;
        end;
      end;

      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
    end
    else  // 选中发生在不同Item，起始(可能是段首)全选中结尾没全选，起始没全选结尾全选，起始结尾都没全选
    begin
      vFormatFirstItemNo := GetParaFirstItemNo(SelectInfo.StartItemNo);  // 取段第一个为起始
      vFormatFirstDrawItemNo := Items[vFormatFirstItemNo].FirstDItemNo;
      vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);  // 取段最后一个为结束，如果变更注意下面

      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vSelStartParaFirst := Items[SelectInfo.StartItemNo].ParaFirst;
      vSelStartComplate := Items[SelectInfo.StartItemNo].IsSelectComplate;  // 起始是否全选
      vSelEndComplate := Items[SelectInfo.EndItemNo].IsSelectComplate;  // 结尾是否全选

      Undo_New;

      // 先处理选中结束Item
      if vEndItem.StyleNo < THCStyle.Null then  // RectItem
      begin
        if vSelEndComplate then  // 选中在最后面，即全选  SelectInfo.EndItemOffset = OffsetAfter
        begin
          if DoAcceptAction(SelectInfo.EndItemNo, SelectInfo.EndItemOffset, THCAction.actDeleteItem) then  // 允许删除
          begin
            UndoAction_DeleteItem(SelectInfo.EndItemNo, OffsetAfter);
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
          if DoAcceptAction(SelectInfo.EndItemNo, SelectInfo.EndItemOffset, THCAction.actDeleteItem) then  // 允许删除
          begin
            UndoAction_DeleteItem(SelectInfo.EndItemNo, vEndItem.Length);
            Items.Delete(SelectInfo.EndItemNo);
            Inc(vDelCount);
          end;
        end
        else  // 文本且不在选中结束Item最后
        if DoAcceptAction(SelectInfo.EndItemNo, SelectInfo.EndItemOffset, THCAction.actBackDeleteText) then
        begin
          UndoAction_DeleteBackText(SelectInfo.EndItemNo, 1, Copy(vEndItem.Text, 1, SelectInfo.EndItemOffset));
          // 结束Item留下的内容
          vText := (vEndItem as THCTextItem).SubString(SelectInfo.EndItemOffset + 1,
            vEndItem.Length - SelectInfo.EndItemOffset);
          vEndItem.Text := vText;
        end;
      end;

      // 删除选中起始Item下一个到结束Item上一个
      for i := SelectInfo.EndItemNo - 1 downto SelectInfo.StartItemNo + 1 do
      begin
        if DoAcceptAction(i, 0, THCAction.actDeleteItem) then  // 允许删除
        begin
          UndoAction_DeleteItem(i, 0);
          Items.Delete(i);

          Inc(vDelCount);
        end;
      end;

      vStartItem := Items[SelectInfo.StartItemNo];  // 选中起始Item
      if vStartItem.StyleNo < THCStyle.Null then  // 起始是RectItem
      begin
        if SelectInfo.StartItemOffset = OffsetBefor then  // 在其前
        begin
          if DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actDeleteItem) then  // 允许删除
          begin
            UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
            Items.Delete(SelectInfo.StartItemNo);
            Inc(vDelCount);
          end;

          if SelectInfo.StartItemNo > vFormatFirstItemNo then
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
          end;
        end
        else
        if SelectInfo.StartItemOffset = OffsetInner then  // 在其上
          (vStartItem as THCCustomRectItem).DeleteSelected;
      end
      else  // 选中起始是TextItem
      begin
        if vSelStartComplate then  // 在最前起始全选了 SelectInfo.StartItemOffset = 0
        begin
          if DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actDeleteItem) then  // 允许删除
          begin
            UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
            Items.Delete(SelectInfo.StartItemNo);
            Inc(vDelCount);
          end;
        end
        else
        //if SelectInfo.StartItemOffset < vStartItem.Length then  // 在中间(不用判断了吧？)
        if DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actBackDeleteText) then
        begin
          UndoAction_DeleteBackText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1,
            Copy(vStartItem.Text, SelectInfo.StartItemOffset + 1, vStartItem.Length - SelectInfo.StartItemOffset));
          vText := (vStartItem as THCTextItem).SubString(1, SelectInfo.StartItemOffset);
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
            Self.CurStyleNo := vNewItem.StyleNo;  // 防止RectItem删除插入文本当前样式不正确
            vNewItem.ParaFirst := True;
            Items.Insert(SelectInfo.StartItemNo, vNewItem);
            UndoAction_InsertItem(SelectInfo.StartItemNo, vNewItem.Length);

            Dec(vDelCount);
          end
          else  // 选中结束不在段最后
            Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst := True;  // 选中结束位置后面的成为段首
        end
        else
        if SelectInfo.EndItemNo = vFormatLastItemNo then  // 结束在段最后
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
        end
        else  // 选中起始在起始段中间，选中结束在结束段中间
        begin
          vLen := Items[SelectInfo.StartItemNo - 1].Length;
          if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.EndItemNo - vDelCount + 1]) then  // 起始前面和结束后面可合并
          begin
            UndoAction_InsertText(SelectInfo.StartItemNo - 1,
              Items[SelectInfo.StartItemNo - 1].Length - Items[SelectInfo.EndItemNo - vDelCount + 1].Length + 1,
              Items[SelectInfo.EndItemNo - vDelCount + 1].Text);

            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            SelectInfo.StartItemOffset := vLen;

            UndoAction_DeleteItem(SelectInfo.EndItemNo - vDelCount + 1, 0);
            Items.Delete(SelectInfo.EndItemNo - vDelCount + 1);
            Inc(vDelCount);
          end
          else  // 起始前面和结束后面不能合并，如果选中起始和结束不在同一段
          begin
            if Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst then
            begin
              UndoAction_ItemParaFirst(SelectInfo.EndItemNo - vDelCount + 1, 0, False);
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
            UndoAction_ItemParaFirst(SelectInfo.EndItemNo - vDelCount, 0, vSelStartParaFirst);
            Items[SelectInfo.EndItemNo - vDelCount].ParaFirst := vSelStartParaFirst;
          end;
        end
        else
        if (not vSelEndComplate) and (SelectInfo.StartItemNo + 1 = SelectInfo.EndItemNo - vDelCount) then  // 起始和结束都没有删除完且中间没有不可删除的
        begin
          if MergeItemText(Items[SelectInfo.StartItemNo], Items[SelectInfo.EndItemNo - vDelCount])  // 起始和结束挨在一起了
          then  // 选中起始、结束位置的Item合并成功
          begin
            UndoAction_InsertText(SelectInfo.StartItemNo,
              Items[SelectInfo.StartItemNo].Length - Items[SelectInfo.EndItemNo - vDelCount].Length + 1,
              Items[SelectInfo.EndItemNo - vDelCount].Text);

            UndoAction_DeleteItem(SelectInfo.EndItemNo - vDelCount, 0);
            Items.Delete(SelectInfo.EndItemNo - vDelCount);
            Inc(vDelCount);
          end
          else  // 选中起始、结束位置的Item不能合并
          begin
            if SelectInfo.EndItemNo <> vFormatLastItemNo then  // 选中结束不是段最后一个
            begin
              if Items[SelectInfo.EndItemNo - vDelCount].ParaFirst then
              begin
                UndoAction_ItemParaFirst(SelectInfo.EndItemNo - vDelCount, 0, False);
                Items[SelectInfo.EndItemNo - vDelCount].ParaFirst := False;  // 合并不成功就挨着
              end;
            end;
          end;
        end;
      end;

      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
    end;

    for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo - vDelCount do
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

procedure THCRichData.DisActive;
var
  vItem: THCCustomItem;
begin
  Self.InitializeField;

  if Items.Count > 0 then  // 页眉中元素激活后切换到正文不高亮
  begin
    vItem := GetActiveItem;
    if vItem <> nil then
      vItem.Active := False;
  end;
end;

function THCRichData.DisSelect: Boolean;
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

function THCRichData.DoInsertTextBefor(const AItemNo, AOffset: Integer;
  const AText: string): Boolean;
begin
  Result := AText <> '';
end;

procedure THCRichData.DoItemMouseEnter(const AItemNo: Integer);
begin
  Items[AItemNo].MouseEnter;
end;

procedure THCRichData.DoItemMouseLeave(const AItemNo: Integer);
begin
  Items[AItemNo].MouseLeave;
end;

procedure THCRichData.DoItemResized(const AItemNo: Integer);
begin
  if Assigned(FOnItemResized) then
    FOnItemResized(Self, AItemNo);
end;

function THCRichData.EmptyDataInsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := False;

  if (AItem.StyleNo > THCStyle.Null) and (AItem.Text = '') then Exit;  // 空行不能再插入空行

  AItem.ParaNo := Items[0].ParaNo;  // 插入在表格上，AItem的StyleNo是和表格一样，在实际的单元格时要以单元格当前的StyleNo为准
  UndoAction_DeleteItem(0, 0);
  Items.Clear;
  DrawItems.Clear;
  AItem.ParaFirst := True;
  Items.Add(AItem);
  UndoAction_InsertItem(0, 0);
  SelectInfo.StartItemOffset := GetItemOffsetAfter(0);

  // 因FormatItemPrepare及ReFormatData_会标记第一个的DrawItem为-1，
  // 调用FormatData时_FormatItemToDrawItems里Inc后
  ReFormat;
  //ReSetSelectAndCaret(0);  // 防止清空后格式化完成后没有选中起始访问出错
  Result := True;
end;

procedure THCRichData.Clear;
begin
  InitializeField;

  inherited Clear;
  SetEmptyData;
end;

function THCRichData.GetHeight: Cardinal;
begin
  Result := CalcContentHeight;
end;

function THCRichData.GetHint: string;
begin
  if (not FMouseMoveRestrain) and (FMouseMoveItemNo >= 0) then
    Result := Items[FMouseMoveItemNo].GetHint
  else
    Result := '';
end;

function THCRichData.SetActiveImage(const AImageStream: TStream): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      (ARectItem as THCImageItem).Image.LoadFromStream(AImageStream);
      Result := True;
    end);
end;

function THCRichData.ActiveTableDeleteCurCol: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).DeleteCurCol;
    end);
end;

function THCRichData.ActiveTableDeleteCurRow: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).DeleteCurRow;
    end);
end;

function THCRichData.ActiveTableResetRowCol(const ARowCount, AColCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).ResetRowCol(Self.Width, ARowCount, AColCount);
    end);
end;

function THCRichData.ActiveTableSplitCurCol: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).SplitCurCol;
    end);
end;

function THCRichData.ActiveTableSplitCurRow: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).SplitCurRow;
    end);
end;

procedure THCRichData.AddData(const ASrcData: THCCustomData);
var
  i, vAddStartNo: Integer;
  vItem: THCCustomItem;
begin
  Self.InitializeField;

  if (Self.Items.Count > 0) and Self.Items.Last.CanConcatItems(ASrcData.Items.First) then
  begin
    Self.Items.Last.Text := Self.Items.Last.Text + ASrcData.Items.First.Text;
    vAddStartNo := 1;
  end
  else
    vAddStartNo := 0;

  for i := vAddStartNo to ASrcData.Items.Count - 1 do
  begin
    if not ASrcData.IsEmptyLine(i) then
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

procedure THCRichData.AdjustSelectRange(var AStartItemNo, AStartItemOffset,
  AEndItemNo, AEndItemNoOffset: Integer);
var
  i, vOldStartItemNo, vOldEndItemNo: Integer;
  vLeftToRight: Boolean;
begin
  // 修正规则：向前选择起始在Item结尾时按下一个起始，向后选择结束在下一个起始时按上一个结束
  vLeftToRight := False;
  // 记录原来选中范围
  vOldStartItemNo := SelectInfo.StartItemNo;
  vOldEndItemNo := SelectInfo.EndItemNo;

  if AStartItemNo < AEndItemNo then  // 从前往后选择在不同的Item
  begin
    vLeftToRight := True;

    if AStartItemOffset = GetItemOffsetAfter(AStartItemNo) then  // 起始在Item最后面，改为下一个Item开始
    begin
      if AStartItemNo < Items.Count - 1 then  // 起始改为下一个Item开始
      begin
        AStartItemNo := AStartItemNo + 1;
        AStartItemOffset := 0;
      end;
    end;

    if (AStartItemNo <> AEndItemNo) and (AEndItemNo >= 0)
      and (AEndItemNoOffset = 0)
    then  // 结束在Item最前面，改为上一个Item结束
    begin
      Items[AEndItemNo].DisSelect;  // 从前往后选，鼠标移动到前一次前面，原鼠标处被移出选中范围

      AEndItemNo := AEndItemNo - 1;
      AEndItemNoOffset := GetItemOffsetAfter(AEndItemNo);
    end;
  end
  else
  if AEndItemNo < AStartItemNo then  // 从后往前选择在不同的Item
  begin
    vLeftToRight := False;

    if (AStartItemNo > 0) and (AStartItemOffset = 0) then  // 起始在Item最前面，改为上一个Item结束
    begin
      AStartItemNo := AStartItemNo - 1;
      AStartItemOffset := GetItemOffsetAfter(AStartItemNo);
    end;

    if (AStartItemNo <> AEndItemNo)
      and (AEndItemNoOffset = GetItemOffsetAfter(AEndItemNo))
    then  // 结束在Item最后面，改为下一个Item开始
    begin
      Items[AEndItemNo].DisSelect;  // 从后往前选，鼠标移动到前一个后面，原鼠标处被移出选中范围

      if AEndItemNo < Items.Count - 1 then  // 改为下一个Item开始
      begin
        AEndItemNo := AEndItemNo + 1;
        AEndItemNoOffset := 0;
      end;
    end;
  end;

  if AStartItemNo = AEndItemNo then  // 选择操作在同一个Item中进行
  begin
    if AEndItemNoOffset > AStartItemOffset then  // 选中结束位置大于起始位置
    begin
      if Items[AStartItemNo].StyleNo < THCStyle.Null then  // RectItem
      begin
        SelectInfo.StartItemNo := AStartItemNo;
        SelectInfo.StartItemOffset := AStartItemOffset;

        if (AStartItemOffset = OffsetBefor)
          and (AEndItemNoOffset = OffsetAfter)
        then  // 从RectItem最前面选择到了最后面(全选中)
        begin
          SelectInfo.EndItemNo := AEndItemNo;
          SelectInfo.EndItemOffset := AEndItemNoOffset;
        end
        else  // 没有全选中
        begin
          SelectInfo.EndItemNo := -1;
          SelectInfo.EndItemOffset := -1;
        end;
      end
      else  // TextItem
      begin
        SelectInfo.StartItemNo := AStartItemNo;
        SelectInfo.StartItemOffset := AStartItemOffset;
        SelectInfo.EndItemNo := AStartItemNo;
        SelectInfo.EndItemOffset := AEndItemNoOffset;
      end;
    end
    else
    if AEndItemNoOffset < AStartItemOffset then  // 选中结束位置小于起始位置
    begin
      if Items[AStartItemNo].StyleNo < THCStyle.Null then  // RectItem
      begin
        if AEndItemNoOffset = OffsetBefor then  // 从后往前选到最前面了
        begin
          SelectInfo.StartItemNo := AStartItemNo;
          SelectInfo.StartItemOffset := AEndItemNoOffset;
          SelectInfo.EndItemNo := AStartItemNo;
          SelectInfo.EndItemOffset := AStartItemOffset;
        end
        else  // 从后往前选到OffsetInner了
        begin
          SelectInfo.StartItemNo := AStartItemNo;
          SelectInfo.StartItemOffset := AStartItemOffset;
          SelectInfo.EndItemNo := -1;
          SelectInfo.EndItemOffset := -1;
        end;
      end
      else  // TextItem
      begin
        SelectInfo.StartItemNo := AEndItemNo;
        SelectInfo.StartItemOffset := AEndItemNoOffset;
        SelectInfo.EndItemNo := AEndItemNo;
        SelectInfo.EndItemOffset := AStartItemOffset;
      end;
    end
    else  // 结束位置和起始位置相同(同一个Item)
    begin
      if SelectInfo.EndItemNo >= 0 then  // 同一Item中划选回到起始位置
        Items[SelectInfo.EndItemNo].DisSelect;

      if (AStartItemOffset = 0) or (AStartItemOffset = GetItemOffsetAfter(SelectInfo.StartItemNo)) then  // 回到两头
        Items[SelectInfo.StartItemNo].DisSelect;

      SelectInfo.StartItemNo := AStartItemNo;
      SelectInfo.StartItemOffset := AStartItemOffset;
      SelectInfo.EndItemNo := -1;
      SelectInfo.EndItemOffset := -1;
    end;
  end
  else  // 选择操作不在同一个Item
  begin
    if vLeftToRight then
    begin
      SelectInfo.StartItemNo := AStartItemNo;
      SelectInfo.StartItemOffset := AStartItemOffset;
      SelectInfo.EndItemNo := AEndItemNo;
      SelectInfo.EndItemOffset := AEndItemNoOffset;
    end
    else
    begin
      SelectInfo.StartItemNo := AEndItemNo;
      SelectInfo.StartItemOffset := AEndItemNoOffset;
      SelectInfo.EndItemNo := AStartItemNo;
      SelectInfo.EndItemOffset := AStartItemOffset;
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

procedure THCRichData.ApplySelectParaStyle(const AMatchStyle: THCParaMatch);

  {$REGION 'DoApplyParagraphStyle'}
  procedure DoApplyParagraphStyle(const AItemNo: Integer);
  var
    i, vParaNo, vFirstNo, vLastNo: Integer;
  begin
    GetParaItemRang(AItemNo, vFirstNo, vLastNo);
    vParaNo := AMatchStyle.GetMatchParaNo(Self.Style, GetItemParaStyle(AItemNo));
    if GetItemParaStyle(vFirstNo) <> vParaNo then
    begin
      UndoAction_ItemParaNo(vFirstNo, 0, vParaNo);
      for i := vFirstNo to vLastNo do
        Items[i].ParaNo := vParaNo;
    end;
  end;
  {$ENDREGION}

  {$REGION 'ApplyParagraphSelecteStyle'}
  procedure ApplyParagraphSelecteStyle;
  var
    i, vFirstNo, vLastNo: Integer;
  begin
    // 先处理起始位置所在的段，以便后面遍历时减少循环次数
    GetParaItemRang(SelectInfo.StartItemNo, vFirstNo, vLastNo);
    DoApplyParagraphStyle(SelectInfo.StartItemNo);

    i := vLastNo + 1; // 从当前段的下一个item开始
    while i <= SelectInfo.EndItemNo do  // 小于结束位置
    begin
      if Items[i].ParaFirst then
        DoApplyParagraphStyle(i);
      Inc(i);
    end;
  end;
  {$ENDREGION}

var
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  if SelectInfo.StartItemNo < 0 then Exit;

  //vFormatFirstDrawItemNo := GetFormatFirst(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);

  //GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
  //vFormatFirstItemNo := GetParaFirstItemNo(SelectInfo.StartItemNo);
  Undo_New;

  if SelectInfo.EndItemNo >= 0 then  // 有选中内容
  begin
    vFormatFirstDrawItemNo := Items[GetParaFirstItemNo(SelectInfo.StartItemNo)].FirstDItemNo;
    vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    ApplyParagraphSelecteStyle;
    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
  end
  else  // 没有选中内容
  begin
    if (GetItemStyle(SelectInfo.StartItemNo) < THCStyle.Null)
      and (SelectInfo.StartItemOffset = OffsetInner)
    then  // 当前是RectItem
    begin
      if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

      vFormatFirstDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
      FormatPrepare(vFormatFirstDrawItemNo, SelectInfo.StartItemNo);
      (Items[SelectInfo.StartItemNo] as THCCustomRectItem).ApplySelectParaStyle(Self.Style, AMatchStyle);
      ReFormatData(vFormatFirstDrawItemNo, SelectInfo.StartItemNo);
    end
    else
    begin
      vFormatFirstDrawItemNo := Items[GetParaFirstItemNo(SelectInfo.StartItemNo)].FirstDItemNo;
      vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      DoApplyParagraphStyle(SelectInfo.StartItemNo);  // 应用样式
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
    end;
  end;

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
end;

procedure THCRichData.ApplySelectTextStyle(const AMatchStyle: THCStyleMatch);
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
      if (vItem as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(AItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(AItemNo, OffsetInner);

      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
    end
    else  // 文本
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);
      CurStyleNo := vStyleNo;
      if vItem.IsSelectComplate then  // Item全部被选中了
      begin
        UndoAction_ItemStyle(AItemNo, SelectInfo.EndItemOffset, vStyleNo);
        vItem.StyleNo := vStyleNo;  // 直接修改样式编号

        if MergeItemToNext(AItemNo) then  // 后一个Item合并到当前Item
        begin
          UndoAction_InsertText(AItemNo, Items[AItemNo].Length - Items[AItemNo + 1].Length + 1, Items[AItemNo + 1].Text);
          UndoAction_DeleteItem(AItemNo + 1, 0);
          Items.Delete(AItemNo + 1);
          Dec(vExtraCount);
        end;

        if AItemNo > 0 then  // 向前合并
        begin
          vLen := Items[AItemNo - 1].Length;
          if MergeItemToPrio(AItemNo) then  // 当前Item合并到上一个Item(如果上面合并了，vItem已经失效，不能直接使用了)
          begin
            UndoAction_InsertText(AItemNo - 1, Items[AItemNo - 1].Length - Items[AItemNo].Length + 1, Items[AItemNo].Text);
            UndoAction_DeleteItem(AItemNo, 0);
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
          UndoAction_DeleteText(AItemNo, SelectInfo.EndItemOffset + 1, vAfterItem.Text);

          Items.Insert(AItemNo + 1, vAfterItem);
          UndoAction_InsertItem(AItemNo + 1, 0);
          Inc(vExtraCount);
        end;

        if vsBefor <> '' then  // 选择起始位置不在Item最开始，保留前半部分，以新Item插入选中部分
        begin
          UndoAction_DeleteText(AItemNo, SelectInfo.StartItemOffset + 1, vSelText);
          vItem.Text := vsBefor;  // 保留前半部分文本

          // 创建选中文本对应的Item
          vSelItem := CreateDefaultTextItem;
          vSelItem.ParaNo := vItem.ParaNo;
          vSelItem.StyleNo := vStyleNo;
          vSelItem.Text := vSelText;

          if vAfterItem <> nil then  // 有后半部分，中间的是新样式，前后肯定不能合并
          begin
            Items.Insert(AItemNo + 1, vSelItem);
            UndoAction_InsertItem(AItemNo + 1, 0);
            Inc(vExtraCount);
          end
          else  // 没有后半部分，说明选中需要和后面判断合并
          begin
            if (AItemNo < Items.Count - 1)
              and (not Items[AItemNo + 1].ParaFirst)
              and MergeItemText(vSelItem, Items[AItemNo + 1])
            then
            begin
              UndoAction_InsertText(AItemNo + 1, 1, vSelText);
              Items[AItemNo + 1].Text := vSelText + Items[AItemNo + 1].Text;
              FreeAndNil(vSelItem);

              SelectInfo.StartItemNo := AItemNo + 1;
              SelectInfo.StartItemOffset := 0;
              SelectInfo.EndItemNo := AItemNo + 1;
              SelectInfo.EndItemOffset := Length(vSelText);

              Exit;
            end;

            Items.Insert(AItemNo + 1, vSelItem);
            UndoAction_InsertItem(AItemNo + 1, 0);
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
          UndoAction_ItemStyle(AItemNo, SelectInfo.EndItemOffset, vStyleNo);
          vItem.StyleNo := vStyleNo;

          if MergeItemToPrio(AItemNo) then // 当前Item合并到上一个Item
          begin
            UndoAction_InsertText(AItemNo - 1, Items[AItemNo - 1].Length - Items[AItemNo].Length + 1, Items[AItemNo].Text);
            UndoAction_DeleteItem(AItemNo, 0);
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

  {$REGION 'ApplyRangeStartItem选中在不同Item中，处理选中起始Item'}
  procedure ApplyRangeStartItem(const AItemNo: Integer);
  var
    vAfterItem: THCCustomItem;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // 非文本
    begin
      if (vItem as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(AItemNo, SelectInfo.StartItemOffset)
      else
        UndoAction_ItemMirror(AItemNo, SelectInfo.StartItemOffset);

      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
    end
    else  // 文本
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);

      if vItem.StyleNo <> vStyleNo then
      begin
        if vItem.IsSelectComplate then  // Item全选中了
        begin
          UndoAction_ItemStyle(AItemNo, 0, vStyleNo);
          vItem.StyleNo := vStyleNo;
        end
        else  // Item部分选中
        begin
          vAfterItem := Items[AItemNo].BreakByOffset(SelectInfo.StartItemOffset);  // 后半部分对应的Item
          UndoAction_DeleteText(AItemNo, SelectInfo.StartItemOffset + 1, vAfterItem.Text);
          // 为方便UndoAction_ItemStyle所以先插入，再修改样式
          Items.Insert(AItemNo + 1, vAfterItem);
          UndoAction_InsertItem(AItemNo + 1, 0);

          UndoAction_ItemStyle(AItemNo + 1, 0, vStyleNo);
          vAfterItem.StyleNo := vStyleNo;

          Inc(vExtraCount);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          SelectInfo.EndItemNo := SelectInfo.EndItemNo + 1;
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'ApplyRangeEndItem选中在不同Item中，处理选中结束Item'}
  procedure ApplyRangeEndItem(const AItemNo: Integer);
  var
    vBeforItem: THCCustomItem;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // 非文本
    begin
      if (vItem as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(AItemNo, SelectInfo.EndItemOffset)
      else
        UndoAction_ItemMirror(AItemNo, SelectInfo.EndItemOffset);

      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
    end
    else  // 文本
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);

      if vItem.StyleNo <> vStyleNo then
      begin
        if vItem.IsSelectComplate then  // Item全选中了
        begin
          UndoAction_ItemStyle(AItemNo, SelectInfo.EndItemOffset, vStyleNo);
          vItem.StyleNo := vStyleNo;
        end
        else  // Item部分选中了
        begin
          vText := vItem.Text;
          vSelText := Copy(vText, 1, SelectInfo.EndItemOffset); // 选中的文本
          UndoAction_DeleteBackText(AItemNo, 1, vSelText);
          Delete(vText, 1, SelectInfo.EndItemOffset);
          vItem.Text := vText;

          vBeforItem := CreateDefaultTextItem;
          vBeforItem.ParaNo := vItem.ParaNo;
          vBeforItem.StyleNo := vStyleNo;
          vBeforItem.Text := vSelText;  // 创建前半部分文本对应的Item
          vBeforItem.ParaFirst := vItem.ParaFirst;
          vItem.ParaFirst := False;

          Items.Insert(AItemNo, vBeforItem);
          UndoAction_InsertItem(AItemNo, 0);
          Inc(vExtraCount);
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'ApplyNorItem选中在不同Item，处理中间Item'}
  procedure ApplyRangeNorItem(const AItemNo: Integer);
  var
    vNewStyleNo: Integer;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // 非文本
    begin
      if (vItem as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(AItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(AItemNo, OffsetInner);

      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
    end
    else  // 文本
    begin
      vNewStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);
      UndoAction_ItemStyle(AItemNo, 0, vNewStyleNo);
      vItem.StyleNo := vNewStyleNo;
    end;
  end;
  {$ENDREGION}

var
  i, vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  Undo_New;

  Self.InitializeField;
  vExtraCount := 0;

  if not SelectExists then  // 没有选中
  begin
    if CurStyleNo > THCStyle.Null then  // (容错)不在不支持文本样式的RectItem上，如果支持点击其上赋值Style.CurStyleNo为确定的文本样式
    begin
      AMatchStyle.Append := not AMatchStyle.StyleHasMatch(Style, CurStyleNo);  // 根据当前判断是添加样式还是减掉样式
      CurStyleNo := AMatchStyle.GetMatchStyleNo(Style, CurStyleNo);

      Style.UpdateInfoRePaint;
      if Items[SelectInfo.StartItemNo].Length = 0 then  // 空行，改变当前光标处样式
      begin
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

        UndoAction_ItemStyle(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, CurStyleNo);
        Items[SelectInfo.StartItemNo].StyleNo := CurStyleNo;

        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
        Style.UpdateInfoReCaret;
      end
      else  // 不是空行
      begin
        if Items[SelectInfo.StartItemNo] is THCTextRectItem then
        begin
          GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
          (Items[SelectInfo.StartItemNo] as THCTextRectItem).TextStyleNo := CurStyleNo;
          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
        end;

        Style.UpdateInfoReCaret(False);
      end;
    end;

    ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    Exit;
  end;

  if SelectInfo.EndItemNo < 0 then  // 没有连续选中内容
  begin
    if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
    begin
      if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

      (Items[SelectInfo.StartItemNo] as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
      if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).SizeChanged then
      begin
        // 如果改变会引起RectItem宽度变化，则需要格式化到最后一个Item
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
        (Items[SelectInfo.StartItemNo] as THCCustomRectItem).SizeChanged := False;
      end
      else
        Self.FormatInit;
    end;
  end
  else  // 有连续选中内容
  begin
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
    if SelectInfo.StartItemNo <> SelectInfo.EndItemNo then  // 选中不在同一Item中
      vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);

    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

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
      ApplyRangeEndItem(SelectInfo.EndItemNo);  // 先处理最后的
      for i := SelectInfo.EndItemNo - 1 downto SelectInfo.StartItemNo + 1 do
        ApplyRangeNorItem(i);  // 处理每一个Item的样式
      ApplyRangeStartItem(SelectInfo.StartItemNo);

      { 样式变化后，从后往前处理选中范围内变化后的合并 }
      //if (SelectInfo.EndItemNo < Items.Count - 1) and (not Items[SelectInfo.EndItemNo + 1].ParaFirst) then  // 选中最后面的下一个不是段首
      if SelectInfo.EndItemNo < vFormatLastItemNo + vExtraCount then  // 选中最后面的下一个不是段首
      begin
        if MergeItemToNext(SelectInfo.EndItemNo) then
        begin
          UndoAction_InsertText(SelectInfo.EndItemNo,
            Items[SelectInfo.EndItemNo].Length - Items[SelectInfo.EndItemNo + 1].Length + 1,
            Items[SelectInfo.EndItemNo + 1].Text);
          UndoAction_DeleteItem(SelectInfo.EndItemNo + 1, 0);
          Items.Delete(SelectInfo.EndItemNo + 1);
          Dec(vExtraCount);
        end;
      end;

      for i := SelectInfo.EndItemNo downto SelectInfo.StartItemNo + 1 do
      begin
        vLen := Items[i - 1].Length;
        if MergeItemToPrio(i) then  // 合并到前一个
        begin
          UndoAction_InsertText(i - 1, Items[i - 1].Length - Items[i].Length + 1, Items[i].Text);
          UndoAction_DeleteItem(i, 0);
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
          UndoAction_InsertText(SelectInfo.StartItemNo - 1,
            Items[SelectInfo.StartItemNo - 1].Length - Items[SelectInfo.StartItemNo].Length + 1,
            Items[SelectInfo.StartItemNo].Text);
          UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
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

    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + vExtraCount, vExtraCount);
  end;

  MatchItemSelectState;

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
end;

procedure THCRichData.ApplyTableCellAlign(const AAlign: THCContentAlign);
begin
  if not CanEdit then Exit;

  RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      (ARectItem as THCTableItem).ApplyContentAlign(AAlign);
      Result := True;
    end);
end;

function THCRichData.DoAcceptAction(const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
begin
  Result := CanEdit;
  if Result then
    Result := Items[AItemNo].AcceptAction(AOffset, SelectInfo.StartRestrain, AAction);

  if Result and Assigned(FOnAcceptAction) then
    Result := FOnAcceptAction(Self, AItemNo, AOffset, AAction);
end;

procedure THCRichData.DoDrawItemMouseMove(const AData: THCCustomData;
  const AItemNo, AOffset, ADrawItemNo: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnDrawItemMouseMove) then
    FOnDrawItemMouseMove(AData, AItemNo, AOffset, ADrawItemNo, Button, Shift, X, Y);
end;

function THCRichData.CanEdit: Boolean;
begin
  Result := not FReadOnly;
  if Result and Assigned(Self.ParentData) then
    Result := (Self.ParentData as THCRichData).CanEdit;
//  if not Result then
//    Beep; //MessageBeep(MB_OK);
end;

function THCRichData.CheckInsertItemCount(const AStartNo,
  AEndNo: Integer): Integer;
begin
  Result := AEndNo - AStartNo + 1;  // 默认原数返回
end;

procedure THCRichData.InitializeField;
begin
  InitializeMouseField;
  inherited InitializeField;
end;

procedure THCRichData.InitializeMouseField;
begin
  FMouseLBDowning := False;
  FMouseDownItemNo := -1;
  FMouseDownItemOffset := -1;
  FMouseMoveItemNo := -1;
  FMouseMoveItemOffset := -1;
  FMouseMoveDrawItemNo := -1;
  FMouseMoveRestrain := False;
  FSelecting := False;
  FDraging := False;
end;

function THCRichData.InsertBreak: Boolean;
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

function THCRichData.InsertGifImage(const AFile: string): Boolean;
var
  vTopData: THCCustomData;
  vGifItem: THCGifItem;
begin
  Result := False;
  if not CanEdit then Exit;

  vTopData := Self.GetTopLevelData;
  vGifItem := THCGifItem.Create(vTopData);
  vGifItem.LoadFromFile(AFile);
  Result := InsertItem(vGifItem);
  InitializeMouseField;
end;

function THCRichData.InsertImage(const AImage: TGraphic): Boolean;
var
  vTopData: THCRichData;
  vImageItem: THCImageItem;
begin
  Result := False;
  if not CanEdit then Exit;

  vTopData := Self.GetTopLevelData as THCRichData;
  vImageItem := THCImageItem.Create(vTopData);
  vImageItem.Image.Assign(AImage);
  vImageItem.Width := AImage.Width;
  vImageItem.Height := AImage.Height;
  vImageItem.RestrainSize(vTopData.Width, vImageItem.Height);
  Result := InsertItem(vImageItem);
  InitializeMouseField;
end;

function THCRichData.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem; const AOffsetBefor: Boolean = True): Boolean;
var
  vFormatFirstDrawItemNo, vFormatLastItemNo, vInsPos, vIncCount: Integer;
  vMerged: Boolean;
begin
  Result := False;

  if not CanEdit then Exit;

  //-------- 在指定的Index处插入Item --------//

  //DeleteSelection;  // 如果正在选中区域中那么删除后AIndex会越界，所以调用此方法前需要处于未选中状态
  if AItem.ParaNo > Self.Style.ParaStyles.Count - 1 then
    AItem.ParaNo := CurParaNo;

  if not AItem.ParaFirst then  // 不是段首
  begin
    if IsEmptyData() then  // 空数据
    begin
      Undo_New();
      Result := EmptyDataInsertItem(AItem);
      CurParaNo := AItem.ParaNo;
      Exit;
    end
    else  // 随其后
      AItem.ParaNo := CurParaNo;
  end;

  {说明：当插入位置不是最后一个且插入位置是段起始，那么可能是在上一段最后插入，
   也可能是要在下一段最前页插入，这时以AItem的ParaFirst属性为判断依据}

  vIncCount := 0;
  Undo_New;
  if AItem.StyleNo < THCStyle.Null then  // 插入RectItem
  begin
    vInsPos := AIndex;
    if AIndex < Items.Count then  // 不是在最后添加一个Item
    begin
      if AOffsetBefor then  // 在原位置Item前面插入
      begin
        GetFormatRange(AIndex, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        {if not AItem.ParaFirst then  // 没标明是否段起始，根据环境自适应
        begin
          AItem.ParaFirst := Items[AIndex].ParaFirst;
          if Items[AIndex].ParaFirst then  // 下一段开始变为非开始，如果要独立为一段去掉此判断即可
          begin
            Undo_ItemParaFirst(AIndex, 0, False);
            Items[AIndex].ParaFirst := False;
          end;
        end;}

        if IsEmptyLine(AIndex) then  // 插入位置处是空行，替换当前
        begin
          AItem.ParaFirst := True;
          UndoAction_DeleteItem(AIndex, 0);
          Items.Delete(AIndex);
          Dec(vIncCount);
        end
        else  // 插入位置不是空行
        if not AItem.ParaFirst then  // 没标明是否段起始，根据环境自适应
        begin
          AItem.ParaFirst := Items[AIndex].ParaFirst;
          if Items[AIndex].ParaFirst then  // 下一段开始变为非开始，如果要独立为一段去掉此判断即可
          begin
            UndoAction_ItemParaFirst(AIndex, 0, False);
            Items[AIndex].ParaFirst := False;

            if Items[AIndex].PageBreak then
            begin
              UndoAction_ItemPageBreak(AIndex, 0, False);
              Items[AIndex].PageBreak := False;
              AItem.PageBreak := True;
            end;
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
          GetFormatRange(AIndex - 1, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          AItem.ParaFirst := True;
          UndoAction_DeleteItem(AIndex - 1, 0);
          Items.Delete(AIndex - 1);
          Dec(vIncCount);
          Dec(vInsPos);
        end
        else  // 插入位置前一个不是空行
        begin
          GetFormatRange(AIndex - 1, GetItemLastDrawItemNo(AIndex - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        end;
      end;
    end
    else  // 在末尾添加一个新Item
    begin
      GetFormatRange(AIndex - 1, GetItemLastDrawItemNo(AIndex - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      if (not AItem.ParaFirst)  // 插入不是另起一段
        and (Items[AIndex - 1].StyleNo > THCStyle.Null)  // 前面是TextItem
        and (Items[AIndex - 1].Text = '')  // 空行
      then  // 插入位置处是空行，替换当前
      begin
        AItem.ParaFirst := True;
        UndoAction_DeleteItem(AIndex - 1, 0);
        Items.Delete(AIndex - 1);
        Dec(vIncCount);
        Dec(vInsPos);
      end;
    end;

    Items.Insert(vInsPos, AItem);
    UndoAction_InsertItem(vInsPos, OffsetAfter);
    Inc(vIncCount);

    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + vIncCount, vIncCount);
    ReSetSelectAndCaret(vInsPos);
  end
  else  // 插入文本Item
  begin
    vMerged := False;

    if not AItem.ParaFirst then  // 新插入的不另起一段，判断和当前位置的关系
    begin
      // 在2个Item中间插入一个Item，需要同时判断和前后能否合并
      if AOffsetBefor then  // 在Item前面插入，未指定另起一段
      begin
        if (AIndex < Items.Count) and (Items[AIndex].CanConcatItems(AItem)) then  // 先判断和当前位置处能否合并
        begin
          GetFormatRange(AIndex, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          UndoAction_InsertText(AIndex, 1, AItem.Text);  // 201806261644
          Items[AIndex].Text := AItem.Text + Items[AIndex].Text;

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex);

          vMerged := True;
        end
        else
        if (not Items[AIndex].ParaFirst) and (AIndex > 0) and Items[AIndex - 1].CanConcatItems(AItem) then   // 再判断和前一个能否合并
        begin
          GetFormatRange(AIndex - 1, GetItemOffsetAfter(AIndex - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          UndoAction_InsertText(AIndex - 1, Items[AIndex - 1].Length + 1, AItem.Text);  // 201806261650
          Items[AIndex - 1].Text := Items[AIndex - 1].Text + AItem.Text;

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex - 1);

          vMerged := True;
        end;
      end
      else  // 在Item后面插入，未指定另起一段，在Item后面插入AIndex肯定是大于0
      begin
        if IsEmptyLine(AIndex - 1) then  // 在空行后插入不换行，替换空行
        begin
          GetFormatRange(AIndex - 1, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          AItem.ParaFirst := True;
          Items.Insert(AIndex, AItem);
          UndoAction_InsertItem(AIndex, 0);

          UndoAction_DeleteItem(AIndex - 1, 0);
          Items.Delete(AIndex - 1);

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex - 1);

          vMerged := True;
        end
        else
        if Items[AIndex - 1].CanConcatItems(AItem) then   // 先判断和前一个能否合并
        begin
          // 能合并，重新获取前一个的格式化信息
          GetFormatRange(AIndex - 1, GetItemOffsetAfter(AIndex - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          UndoAction_InsertText(AIndex - 1, Items[AIndex - 1].Length + 1, AItem.Text);  // 201806261650
          Items[AIndex - 1].Text := Items[AIndex - 1].Text + AItem.Text;

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex - 1);

          vMerged := True;
        end
        else
        if (AIndex < Items.Count) and (not Items[AIndex].ParaFirst) and (Items[AIndex].CanConcatItems(AItem)) then  // 先判断和当前位置处能否合并
        begin
          GetFormatRange(AIndex, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          UndoAction_InsertText(AIndex, 1, AItem.Text);  // 201806261644
          Items[AIndex].Text := AItem.Text + Items[AIndex].Text;

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex, AItem.Length);

          vMerged := True;
        end;
      end;
    end;

    if not vMerged then  // 不能和插入位置前及插入位置处的Item合并
    begin
      if AOffsetBefor then  // 在某Item前面插入
      begin
        GetFormatRange(AIndex, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
        if not AItem.ParaFirst then  // 没标明是否段起始，根据环境自适应
        begin
          AItem.ParaFirst := Items[AIndex].ParaFirst;
          if Items[AIndex].ParaFirst then  // 下一段开始变为非开始，如果要独立为一段去掉此判断即可
          begin
            UndoAction_ItemParaFirst(AIndex, 0, False);
            Items[AIndex].ParaFirst := False;
          end;
        end;
      end
      else  // 在某Item后面插入
        GetFormatRange(AIndex - 1, GetItemOffsetAfter(AIndex - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);

      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      Items.Insert(AIndex, AItem);
      UndoAction_InsertItem(AIndex, 0);
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);

      ReSetSelectAndCaret(AIndex);
    end;
  end;

  Result := True;
end;

function THCRichData.InsertLine(const ALineHeight: Integer): Boolean;
var
  vItem: THCLineItem;
begin
  Result := False;

  if not CanEdit then Exit;

  vItem := THCLineItem.Create(Self, Self.Width, ALineHeight);

  Result := InsertItem(vItem);
  InitializeMouseField;  // 201807311101
end;

function THCRichData.TableApplyContentAlign(const AAlign: THCContentAlign): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      (ARectItem as THCTableItem).ApplyContentAlign(AAlign);
      Result := True;
    end);
end;

function THCRichData.TableInsertColAfter(const AColCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).InsertColAfter(AColCount);
    end);
end;

function THCRichData.TableInsertColBefor(const AColCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).InsertColBefor(AColCount);
    end);
end;

function THCRichData.RectItemAction(const AAction: TRectItemActionEvent): Boolean;
var
  vCurItemNo, vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;
  vCurItemNo := GetActiveItemNo;
  if Items[vCurItemNo] is THCCustomRectItem then
  begin
    GetFormatRange(vCurItemNo, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

    Undo_New;
    if (Items[vCurItemNo] as THCCustomRectItem).MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    UndoAction_ItemSelf(vCurItemNo, OffsetInner);
    Result := AAction(Items[vCurItemNo] as THCCustomRectItem);
    if Result then
    begin
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
      // DisSelect;  // 表格合并后清空选中，会导致当前ItemNo没有了，通过方法往表格里插入时会出错
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
    end;

    InitializeMouseField;  // 201807311101
  end;
end;

function THCRichData.TableInsertRowAfter(const ARowCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).InsertRowAfter(ARowCount);
    end);
end;

function THCRichData.TableInsertRowBefor(const ARowCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).InsertRowBefor(ARowCount);
    end);
end;

function THCRichData.TextItemAction(const AAction: TTextItemActionEvent): Boolean;
var
  vCurItemNo, vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;
  vCurItemNo := GetActiveItemNo;
  if Items[vCurItemNo] is THCTextItem then
  begin
    GetFormatRange(vCurItemNo, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

    Undo_New;
    UndoAction_ItemMirror(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);

    //UndoAction_ItemSelf(vCurItemNo, OffsetInner);
    Result := AAction(Items[vCurItemNo] as THCTextItem);
    if Result then
    begin
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
      //DisSelect;
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
    end;

    InitializeMouseField;
  end;
end;

function THCRichData.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
var
  i, vItemCount, vStyleNo, vOffsetStart, vInsetLastNo, vCaretOffse, vCaretParaNo,
  vInsPos, vFormatFirstDrawItemNo, vFormatLastItemNo, vItemCountAct, vIgnoreCount: Integer;
  vItem, vAfterItem: THCCustomItem;
  vInsertBefor, vInsertEmptyLine: Boolean;
  vDataSize: Int64;
begin
  Result := False;

  if not CanEdit then Exit;
  if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actInsertItem) then Exit;  // TextItem此偏移位置不可接受输入
  if not SelectPerfect then Exit;

  vAfterItem := nil;
  vInsertBefor := False;
  vInsertEmptyLine := False;
  vCaretParaNo := CurParaNo;

  Undo_GroupBegin(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
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
          GetFormatRange(SelectInfo.StartItemNo, OffsetInner, vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          Undo_New;
          if (Items[vInsPos] as THCCustomRectItem).MangerUndo then
            UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
          else
            UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

          Result := (Items[vInsPos] as THCCustomRectItem).InsertStream(AStream, AStyle, AFileVersion);
          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

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
        begin
          vInsertEmptyLine := IsEmptyLine(vInsPos);
          vInsPos := vInsPos + 1;
        end
        else
        if SelectInfo.StartItemOffset = 0 then  // 其前
          vInsertBefor := Items[vInsPos].Length <> 0
        else  // TextItem中间
        begin
          Undo_New;
          UndoAction_DeleteBackText(vInsPos, SelectInfo.StartItemOffset + 1,
            Copy(Items[vInsPos].Text, SelectInfo.StartItemOffset + 1, Items[vInsPos].Length - SelectInfo.StartItemOffset));

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

    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);

    // 计算格式化起始、结束ItemNo
    if Items.Count > 0 then  // 兼容Empty
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo)
    else
    begin
      vFormatFirstDrawItemNo := 0;
      vFormatLastItemNo := -1;
    end;

    vItemCountAct := 0;  // 实际插入的数量
    vIgnoreCount := 0;  // 忽略掉的数据
    Undo_New;
    for i := 0 to vItemCount - 1 do
    begin
      AStream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));
      vItem := CreateItemByStyle(vStyleNo);
      {if vStyleNo < THCStyle.Null then  // 如果插入第i个仅是源的第i个，不能表示当前文档第i个
      begin                              // 插入的RectItem撤销也是整体，所以不用自己管理撤销恢复
        if (vItem as THCCustomRectItem).MangerUndo then
          UndoAction_ItemSelf(i, 0)
        else
          UndoAction_ItemMirror(i, OffsetInner);
      end;}

      vItem.LoadFromStream(AStream, AStyle, AFileVersion);
      if AStyle <> nil then  // 有样式表
      begin
        if vItem.StyleNo > THCStyle.Null then
          vItem.StyleNo := Style.GetStyleNo(AStyle.TextStyles[vItem.StyleNo], True);

        if Style.States.Contain(hosPasting) then  // 粘贴时使用光标位置样式
          vItem.ParaNo := vCaretParaNo
        else
          vItem.ParaNo := Style.GetParaNo(AStyle.ParaStyles[vItem.ParaNo], True);
      end
      else  // 无样式表
      begin
        if vItem.StyleNo > THCStyle.Null then
          vItem.StyleNo := CurStyleNo;

        vItem.ParaNo := vCaretParaNo;
      end;

      if i = 0 then  // 插入的第一个Item
      begin
        if vInsertBefor then  // 第一个在某Item最前面插入(粘贴)
        begin
          vItem.ParaFirst := Items[vInsPos].ParaFirst;

          if Items[vInsPos].ParaFirst then
          begin
            UndoAction_ItemParaFirst(vInsPos, 0, False);
            Items[vInsPos].ParaFirst := False;
          end;
        end
        else
          vItem.ParaFirst := False;
      end
      else  // 插入非第一个Item
      if (not vItem.ParaFirst) and MergeItemText(Items[vInsPos + i - 1 - vIgnoreCount], vItem) then  // 和插入位置前一个能合并，有些不允许复制的粘贴时会造成前后可合并
      begin
        Inc(vIgnoreCount);
        FreeAndNil(vItem);
        Continue;
      end;

      Items.Insert(vInsPos + i - vIgnoreCount, vItem);
      UndoAction_InsertItem(vInsPos + i - vIgnoreCount, 0);
      Inc(vItemCountAct);
    end;

    vItemCount := CheckInsertItemCount(vInsPos, vInsPos + vItemCountAct - 1);  // 检查插入的Item是否合格并删除不合格

    vInsetLastNo := vInsPos + vItemCount - 1;  // 光标在最后一个Item
    vCaretOffse := GetItemOffsetAfter(vInsetLastNo);  // 最后一个Item后面

    if vAfterItem <> nil then  // 插入操作是在Item中间，原Item被拆分成2个
    begin
      if MergeItemText(Items[vInsetLastNo], vAfterItem) then  // 插入最后一个和后半部分能合并
      begin
        UndoAction_InsertText(vInsetLastNo, Items[vInsetLastNo].Length - vAfterItem.Length + 1, vAfterItem.Text);
        FreeAndNil(vAfterItem);
      end
      else  // 插入最后一个和后半部分不能合并
      begin
        Items.Insert(vInsetLastNo + 1, vAfterItem);
        UndoAction_InsertItem(vInsetLastNo + 1, 0);

        Inc(vItemCount);
      end;
    end;

    if {(vInsPos > vFormatFirstItemNo) and} (vInsPos > 0) then  // 在格式化起始位置后插入且不是第0个位置
    begin
      if vInsertEmptyLine then  // 插入位置前面是空行Item
      begin
        UndoAction_ItemParaFirst(vInsPos, 0, Items[vInsPos - 1].ParaFirst);
        Items[vInsPos].ParaFirst := Items[vInsPos - 1].ParaFirst;

        if Items[vInsPos - 1].PageBreak then
        begin
          UndoAction_ItemPageBreak(vInsPos, 0, True);
          Items[vInsPos].PageBreak := True;
        end;

        UndoAction_DeleteItem(vInsPos - 1, 0);
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
          UndoAction_InsertText(vInsPos - 1, Items[vInsPos - 1].Length - Items[vInsPos].Length + 1, Items[vInsPos].Text);

          UndoAction_DeleteItem(vInsPos, 0);
          Items.Delete(vInsPos);

          if vItemCount = 1 then
            vCaretOffse := vOffsetStart + vCaretOffse;

          Dec(vItemCount);
          Dec(vInsetLastNo);
        end;
      end;

      if (vInsetLastNo < Items.Count - 1)  // 插入最后Item和后面的能合并
        and (not Items[vInsetLastNo + 1].ParaFirst)
        and MergeItemText(Items[vInsetLastNo], Items[vInsetLastNo + 1])
      then
      begin
        UndoAction_InsertText(vInsetLastNo,
          Items[vInsetLastNo].Length - Items[vInsetLastNo + 1].Length + 1, Items[vInsetLastNo + 1].Text);
        UndoAction_DeleteItem(vInsetLastNo + 1, 0);

        Items.Delete(vInsetLastNo + 1);
        Dec(vItemCount);
      end;
    end
    else  // 在最开始第0个位置处插入
    //if (vInsetLastNo < Items.Count - 1) then
    begin
      if MergeItemText(Items[vInsetLastNo], Items[vInsetLastNo + 1 {vInsPos + vItemCount}]) then  // 可以和插入前0位置的合并
      begin
        vItem := Items[vInsetLastNo + 1];
        UndoAction_InsertText(vInsetLastNo, Items[vInsetLastNo].Length - vItem.Length + 1, vItem.Text);
        UndoAction_DeleteItem(vInsPos + vItemCount, 0);  // vInsPos + vItemCount = vInsetLastNo + 1?

        Items.Delete(vInsPos + vItemCount);
        Dec(vItemCount);
      end;
    end;

    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + vItemCount, vItemCount);

    ReSetSelectAndCaret(vInsetLastNo, vCaretOffse);  // 选中插入内容最后Item位置
  finally
    Undo_GroupEnd(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  end;

  InitializeMouseField;  // 201807311101

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
  Style.UpdateInfoReScroll;
end;

function THCRichData.InsertItem(const AItem: THCCustomItem): Boolean;
var
  vCurItemNo: Integer;
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
  vText, vsBefor, vsAfter: string;
  vAfterItem: THCCustomItem;
begin
  Result := False;

  if not CanEdit then Exit;

  if not DeleteSelected then Exit;

  if AItem.ParaNo > Self.Style.ParaStyles.Count - 1 then
    AItem.ParaNo := CurParaNo;

  if not AItem.ParaFirst then  // 不是段首
  begin
    if IsEmptyData() then  // 空数据
    begin
      Undo_New();
      Result := EmptyDataInsertItem(AItem);
      CurParaNo := AItem.ParaNo;
      Exit;
    end
    else  // 随其后
      AItem.ParaNo := CurParaNo;
  end;

  vCurItemNo := GetActiveItemNo;
  if Items[vCurItemNo].StyleNo < THCStyle.Null then  // 当前位置是 RectItem
  begin
    if SelectInfo.StartItemOffset = OffsetInner then  // 正在其上
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      Undo_New;
      UndoAction_ItemSelf(vCurItemNo, OffsetInner);
      Result := (Items[vCurItemNo] as THCCustomRectItem).InsertItem(AItem);
      if Result then
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
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
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vText := Items[vCurItemNo].Text;
      vsBefor := Copy(vText, 1, SelectInfo.StartItemOffset);  // 前半部分文本
      vsAfter := Copy(vText, SelectInfo.StartItemOffset + 1, Items[vCurItemNo].Length
        - SelectInfo.StartItemOffset);  // 后半部分文本

      Undo_New;
      if Items[vCurItemNo].CanConcatItems(AItem) then  // 能合并
      begin
        if AItem.ParaFirst then  // 新段
        begin
          UndoAction_DeleteBackText(vCurItemNo, SelectInfo.StartItemOffset + 1, vsAfter);
          Items[vCurItemNo].Text := vsBefor;
          AItem.Text := AItem.Text + vsAfter;

          vCurItemNo := vCurItemNo + 1;
          Items.Insert(vCurItemNo, AItem);
          UndoAction_InsertItem(vCurItemNo, 0);

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);
          ReSetSelectAndCaret(vCurItemNo);
        end
        else  // 同一段中插入
        begin
          UndoAction_InsertText(vCurItemNo, SelectInfo.StartItemOffset + 1, AItem.Text);
          vsBefor := vsBefor + AItem.Text;
          Items[vCurItemNo].Text := vsBefor + vsAfter;

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
          SelectInfo.StartItemNo := vCurItemNo;
          SelectInfo.StartItemOffset := Length(vsBefor);
          //CaretDrawItemNo := GetItemLastDrawItemNo(vCurItemNo);
        end;
      end
      else  // 不能合并
      begin
        UndoAction_DeleteBackText(vCurItemNo, SelectInfo.StartItemOffset + 1, vsAfter);
        vAfterItem := Items[vCurItemNo].BreakByOffset(SelectInfo.StartItemOffset);  // 后半部分对应的Item

        // 插入后半部分对应的Item
        vCurItemNo := vCurItemNo + 1;
        Items.Insert(vCurItemNo, vAfterItem);
        UndoAction_InsertItem(vCurItemNo, 0);
        // 插入新Item
        Items.Insert(vCurItemNo, AItem);
        UndoAction_InsertItem(vCurItemNo, 0);

        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 2, 2);
        ReSetSelectAndCaret(vCurItemNo);
      end;

      Result := True;
    end;
  end;
end;

function THCRichData.InsertTable(const ARowCount,
  AColCount: Integer): Boolean;
var
  vTopData: THCRichData;
  vItem: THCCustomItem;
begin
  Result := False;

  if not CanEdit then Exit;

  vTopData := GetTopLevelData as THCRichData;
  vItem := THCTableItem.Create(vTopData, ARowCount, AColCount, vTopData.Width);
  Result := InsertItem(vItem);
  InitializeMouseField;  // 201807311101
end;

function THCRichData.InsertText(const AText: string): Boolean;
var
  vAddCount: Integer;

  {$REGION ' DoTextItemInsert 在文本Item前后或中间插入文本 '}
  function DoTextItemInsert(const AText: string; const ANewPara: Boolean): Boolean;
  var
    vTextItem: THCTextItem;
    vNewItem, vAfterItem: THCCustomItem;
    vS: string;
    vOffset: Integer;
  begin
    Result := False;
    vTextItem := Items[SelectInfo.StartItemNo] as THCTextItem;
    if vTextItem.StyleNo = CurStyleNo then  // 当前样式和插入位置TextItem样式相同
    begin
      if SelectInfo.StartItemOffset = 0 then  // 在TextItem最前面插入
      begin
        vOffset := Length(AText);

        if ANewPara then  // 另起一段
        begin
          vNewItem := CreateDefaultTextItem;
          vNewItem.ParaFirst := True;
          vNewItem.Text := AText;

          Items.Insert(SelectInfo.StartItemNo, vNewItem);
          UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
          Inc(vAddCount);
        end
        else  // 同段
        if vTextItem.AcceptAction(0, SelectInfo.StartRestrain, actConcatText) then  // TextItem最前面可接受连接字符
        begin
          UndoAction_InsertText(SelectInfo.StartItemNo, 1, AText);
          vTextItem.Text := AText + vTextItem.Text;
        end
        else  // TextItem最前面不接收字符
        begin
          if vTextItem.ParaFirst then  // 段最前面不接收，新插入的作为段首
          begin
            vNewItem := CreateDefaultTextItem;
            vNewItem.Text := AText;
            vNewItem.ParaFirst := True;

            UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, False);
            vTextItem.ParaFirst := False;

            Items.Insert(SelectInfo.StartItemNo, vNewItem);
            UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
            Inc(vAddCount);
          end
          else  // TextItem最前面不接收字符，TextItem不是段最前面
          if Items[SelectInfo.StartItemNo - 1].StyleNo > THCStyle.Null then  // 前一个是文本
          begin
            vTextItem := Items[SelectInfo.StartItemNo - 1] as THCTextItem;
            if vTextItem.AcceptAction(vTextItem.Length, True, actConcatText) then // 前一个在最后可接受连接字符
              //and MergeItemText(Items[SelectInfo.StartItemNo - 1], vNewItem)
            begin
              UndoAction_InsertText(SelectInfo.StartItemNo - 1, vTextItem.Length + 1, AText);
              vTextItem.Text := vTextItem.Text + AText;
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              vOffset := vTextItem.Length;
            end
            else
            begin
              vNewItem := CreateDefaultTextItem;
              vNewItem.Text := AText;

              Items.Insert(SelectInfo.StartItemNo, vNewItem);
              UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
              Inc(vAddCount);
            end;
          end
          else  // 前一个不是文本
          begin
            vNewItem := CreateDefaultTextItem;
            vNewItem.Text := AText;

            Items.Insert(SelectInfo.StartItemNo, vNewItem);
            UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
            Inc(vAddCount);
          end;
        end;
      end
      else
      if SelectInfo.StartItemOffset = vTextItem.Length then  // 在TextItem最后插入
      begin
        if ANewPara then  // 另起一段
        begin
          vNewItem := CreateDefaultTextItem;
          vNewItem.ParaFirst := True;
          vNewItem.Text := AText;

          Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
          UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
          Inc(vAddCount);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          vOffset := vNewItem.Length;
        end
        else  // 最后面插入文本不另起一段
        if vTextItem.AcceptAction(SelectInfo.StartItemOffset, SelectInfo.StartRestrain, actConcatText) then  // 最后面能接收文本
        begin
          UndoAction_InsertText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, AText);
          vTextItem.Text := vTextItem.Text + AText;
          vOffset := vTextItem.Length;
        end
        else  // 最后面不能接收输入
        if not IsParaLastItem(SelectInfo.StartItemNo) then  // 同段后面还有内容
        begin
          if Items[SelectInfo.StartItemNo + 1].StyleNo > THCStyle.Null then  // 同段后面是文本
          begin
            vTextItem := Items[SelectInfo.StartItemNo + 1] as THCTextItem;
            if vTextItem.AcceptAction(0, True, actConcatText) then  // 后一个在最前可接受连接字符
              //and MergeItemText(Items[SelectInfo.StartItemNo - 1], vNewItem)
            begin
              UndoAction_InsertText(SelectInfo.StartItemNo + 1, 1, AText);
              vTextItem.Text := AText + vTextItem.Text;
              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              vOffset := Length(AText);
            end
            else
            begin
              vNewItem := CreateDefaultTextItem;
              vNewItem.Text := AText;

              Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
              UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
              Inc(vAddCount);
              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              vOffset := Length(AText);
            end;
          end
          else  // 同段后面不是文本
          begin
            vNewItem := CreateDefaultTextItem;
            vNewItem.Text := AText;

            Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
            UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
            Inc(vAddCount);
            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            vOffset := Length(AText);
          end;
        end
        else  // 段最后一个后面不接收输入
        begin
          vNewItem := CreateDefaultTextItem;
          vNewItem.Text := AText;

          Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
          UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
          Inc(vAddCount);
          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          vOffset := Length(AText);
        end;
      end
      else  // 在Item中间
      begin
        if ANewPara then  // 在TextItem中间按新段插入Text
        begin
          // 原TextItem打断
          vS := vTextItem.SubString(SelectInfo.StartItemOffset + 1, vTextItem.Length - SelectInfo.StartItemOffset);
          UndoAction_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, vS);
          // 原位置后半部分
          vAfterItem := vTextItem.BreakByOffset(SelectInfo.StartItemOffset);
          vAfterItem.Text := AText + vAfterItem.Text;
          vAfterItem.ParaFirst := True;
          // 插入原TextItem后半部分增加Text后的
          Items.Insert(SelectInfo.StartItemNo + 1, vAfterItem);
          UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
          Inc(vAddCount);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          vOffset := Length(AText);
        end
        else
        begin
          vOffset := SelectInfo.StartItemOffset + Length(AText);
          vS := vTextItem.Text;
          Insert(AText, vS, SelectInfo.StartItemOffset + 1);
          UndoAction_InsertText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, AText);
          vTextItem.Text := vS;
        end;
      end;

      SelectInfo.StartItemOffset := vOffset;

      Result := True;
    end
    else  // 插入位置TextItem样式和当前样式不同，在TextItem头、中、尾没选中，但应用了新样式，以新样式处理
    begin
      vNewItem := CreateDefaultTextItem;
      vNewItem.ParaFirst := ANewPara;
      vNewItem.Text := AText;

      if SelectInfo.StartItemOffset = 0 then  // 在TextItem最前面插入
      begin
        if (not vNewItem.ParaFirst) and vTextItem.ParaFirst then  // 在段首插入非段首Item
        begin
          vNewItem.ParaFirst := True;
          UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, False);
          vTextItem.ParaFirst := False;
        end;

        Items.Insert(SelectInfo.StartItemNo, vNewItem);
        UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
        Inc(vAddCount);

        SelectInfo.StartItemOffset := vNewItem.Length;
      end
      else
      if SelectInfo.StartItemOffset = vTextItem.Length then  // 在TextItem最后面插入
      begin
        Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
        UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
        Inc(vAddCount);

        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
        SelectInfo.StartItemOffset := vNewItem.Length;
      end
      else  // 在TextItem中间插入
      begin
        // 原TextItem打断
        vS := vTextItem.SubString(SelectInfo.StartItemOffset + 1, vTextItem.Length - SelectInfo.StartItemOffset);
        UndoAction_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, vS);
        // 原位置后半部分
        vAfterItem := vTextItem.BreakByOffset(SelectInfo.StartItemOffset);
        vAfterItem.ParaFirst := False;
        // 先插入新的
        Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
        UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
        Inc(vAddCount);
        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
        SelectInfo.StartItemOffset := vNewItem.Length;
        // 再插入原TextItem后半部分
        Items.Insert(SelectInfo.StartItemNo + 1, vAfterItem);
        UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
        Inc(vAddCount);
      end;
    end;

    CurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;  // 防止连续插入不同样式的
  end;
  {$ENDREGION}

  {$REGION 'DoInsertTextEx'}
  procedure DoInsertTextEx(const AText: string; const ANewPara: Boolean);
  var
    vItem: THCCustomItem;
  begin
    vItem := Items[SelectInfo.StartItemNo];

    if vItem.StyleNo < THCStyle.Null then  // 当前位置是 RectItem  并且通过过滤已经不会是其上的情况了
    begin
      if SelectInfo.StartItemOffset = OffsetAfter then  // 在其后输入内容
      begin
        if (SelectInfo.StartItemNo < Items.Count - 1)
          and (Items[SelectInfo.StartItemNo + 1].StyleNo > THCStyle.Null)
          and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)
        then  // 下一个是TextItem且不是段首，则合并到下一个开始
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          CurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;
          DoInsertTextEx(AText, ANewPara);  // 在下一个TextItem前面插入
        end
        else  // 最后或下一个还是RectItem或当前是段尾
        begin
          vItem := CreateDefaultTextItem;
          vItem.Text := AText;
          vItem.ParaFirst := ANewPara;

          Items.Insert(SelectInfo.StartItemNo + 1, vItem);  // 在两个RectItem中间插入
          UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
          Inc(vAddCount);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          SelectInfo.StartItemOffset := vItem.Length;
          CurStyleNo := vItem.StyleNo;
        end;
      end
      else  // 在RectItem前输入内容
      begin
        if (SelectInfo.StartItemNo > 0)
          and (Items[SelectInfo.StartItemNo - 1].StyleNo > THCStyle.Null)
          and (not Items[SelectInfo.StartItemNo].ParaFirst)
        then  // 前一个是TextItem，当前不是段首，合并到前一个尾
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
          CurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;
          DoInsertTextEx(AText, ANewPara);  // 在前一个后面插入
        end
        else  // 在段最前或前一个还是RectItem
        begin
          vItem := CreateDefaultTextItem;
          vItem.Text := AText;
          if ANewPara then  // 新段
            vItem.ParaFirst := True
          else  // 未指定段首，适应当前环境
          if Items[SelectInfo.StartItemNo].ParaFirst then  // 原位置是段首
          begin
            UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, False);
            Items[SelectInfo.StartItemNo].ParaFirst := False;
            vItem.ParaFirst := True;
          end;

          Items.Insert(SelectInfo.StartItemNo, vItem);  // 在两个RectItem中间插入
          UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
          Inc(vAddCount);

          SelectInfo.StartItemOffset := vItem.Length;
          CurStyleNo := vItem.StyleNo;
        end;
      end;
    end
    else  // 当前位置是TextItem
      DoTextItemInsert(AText, ANewPara);
  end;
  {$ENDREGION}

var
  vPCharStart, vPCharEnd, vPtr: PChar;
  vNewPara: Boolean;
  vS: string;
  vRectItem: THCCustomRectItem;
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;

  if not CanEdit then Exit;
  if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actInsertText) then Exit;  // TextItem此偏移位置不可接受输入
  if not DoInsertTextBefor(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, AText) then Exit;
  if not DeleteSelected then Exit;

  Undo_GroupBegin(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  try
    Undo_New;

    if (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)  // 当前位置是 RectItem
      and (SelectInfo.StartItemOffset = OffsetInner)
    then  // 在其上输入内容
    begin
      vRectItem := Items[SelectInfo.StartItemNo] as THCCustomRectItem;
      if vRectItem.MangerUndo then
        UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

      Result := vRectItem.InsertText(AText);
      if vRectItem.SizeChanged then
      begin
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
        vRectItem.SizeChanged := False;
      end
      else
        Self.FormatInit;
    end
    else  // 当前位置是TextItem或RectItem前后
    begin
      vNewPara := False;
      vAddCount := 0;
      CurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;  // 防止静默移动选中位置没有更新当前样式
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

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
              DoInsertTextEx(vS, vNewPara);
              vNewPara := True;

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
      DoInsertTextEx(vS, vNewPara);

      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + vAddCount, vAddCount);
      Result := True;
    end;

    ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);  // 便于Undo记录CaretDrawItem
  finally
    Undo_GroupEnd(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  end;

  InitializeMouseField;  // 201807311101

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
  Style.UpdateInfoReScroll;
end;

function THCRichData.IsSelectSeekStart: Boolean;
begin
  Result := (FSelectSeekNo = SelectInfo.StartItemNo) and
    (FSelectSeekOffset = SelectInfo.StartItemOffset);
end;

procedure THCRichData.KeyDown(var Key: Word; Shift: TShiftState;
  const APageBreak: Boolean = False);

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
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
  vSelectExist: Boolean;

  {$REGION ' TABKeyDown 按键 '}
  procedure TABKeyDown;
  var
    vTabItem: TTabItem;
    vParaStyle: THCParaStyle;
  begin
    if (SelectInfo.StartItemOffset = 0) and (Items[SelectInfo.StartItemNo].ParaFirst) then  // 段首
    begin
      vParaStyle := Style.ParaStyles[vCurItem.ParaNo];
      ApplyParaFirstIndent(vParaStyle.FirstIndent + PixXToMillimeter(TabCharWidth));
    end
    else
    if vCurItem.StyleNo < THCStyle.Null then  // 当前是RectItem
    begin
      if SelectInfo.StartItemOffset = OffsetInner then // 在其上
      begin
        if (vCurItem as THCCustomRectItem).WantKeyDown(Key, Shift) then  // 处理此键
        begin
          GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
          (vCurItem as THCCustomRectItem).KeyDown(Key, Shift);
          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
        end;
      end
      else
      begin
        vTabItem := TTabItem.Create(Self);
        Self.InsertItem(vTabItem);
      end;
    end
    else  // TextItem
    begin
      vTabItem := TTabItem.Create(Self);
      Self.InsertItem(vTabItem);
    end;
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
      if AItemNo > 0 then  // 在最开头，往前一个最后
      begin
        Items[AItemNo].DisSelect;
        AItemNo := AItemNo - 1;
        if Items[AItemNo].StyleNo < THCStyle.Null then
          AOffset := OffsetBefor
        else
          AOffset := Items[AItemNo].Length - 1;  // 倒数第1个前面
      end;

      {$IFDEF UNPLACEHOLDERCHAR}
      if (Items[AItemNo].StyleNo > THCStyle.Null) and IsUnPlaceHolderChar(Items[AItemNo].Text[AOffset + 1]) then
        AOffset := GetItemActualOffset(AItemNo, AOffset) - 1;
      {$ENDIF}
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
    vNewCaretDrawItemNo, i: Integer;
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
          SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
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
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;

        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // 无选中内容
      begin
        if SelectInfo.StartItemOffset <> 0 then  // 不在Item最开始
        begin
          SelectInfo.StartItemOffset := SelectInfo.StartItemOffset - 1;

          {$IFDEF UNPLACEHOLDERCHAR}
          if IsUnPlaceHolderChar(Items[SelectInfo.StartItemNo].Text[SelectInfo.StartItemOffset + 1]) then
            SelectInfo.StartItemOffset := GetItemActualOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset) - 1;
          {$ENDIF}
        end
        else  // 在Item最开始左方向键
        begin
          if SelectInfo.StartItemNo > 0 then  // 不是第一个Item的最开始，往前面移动
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;  // 上一个
            SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);

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
        end
        else
        if SelectInfo.StartRestrain then
        begin
          SelectInfo.StartRestrain := False;
          Items[DrawItems[vNewCaretDrawItemNo].ItemNo].Active := True;
        end;

        Style.UpdateInfoRePaint;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' RightKeyDown 右方向键，此处不会涉及表格，表格在RectItemKeyDown中处理了 '}
  procedure RightKeyDown;

    procedure SelectNext(var AItemNo, AOffset: Integer);
    begin
      if AOffset = GetItemOffsetAfter(AItemNo) then  // 在Item最后，移动到下一个Item
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

      {$IFDEF UNPLACEHOLDERCHAR}
      AOffset := GetItemActualOffset(AItemNo, AOffset, True);
      {$ENDIF}
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
    vNewCaretDrawItemNo, i: Integer;
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
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;

        SelectInfo.StartItemNo := SelectInfo.EndItemNo;
        SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // 无选中内容
      begin
        if SelectInfo.StartItemOffset < vCurItem.Length then  // 不在Item最右边
          {$IFDEF UNPLACEHOLDERCHAR}
          SelectInfo.StartItemOffset := GetItemActualOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, True)
          {$ELSE}
          SelectInfo.StartItemOffset := SelectInfo.StartItemOffset + 1
          {$ENDIF}
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

        Style.UpdateInfoRePaint;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' HomeKeyDown 按键 '}
  procedure HomeKeyDown;
  var
    vFirstDItemNo, vLastDItemNo, i: Integer;
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
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;

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

      Style.UpdateInfoRePaint;
    end;
  end;
  {$ENDREGION}

  {$REGION ' EndKeyDown 按键 '}
  procedure EndKeyDown;
  var
    vFirstDItemNo, vLastDItemNo, i: Integer;
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
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;

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

      Style.UpdateInfoRePaint;
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
            ADrawItemOffset := DrawItems[i].CharOffs + GetDrawItemOffsetAt(i, vX) - 1;

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
          Style.UpdateInfoRePaint;
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
            ADrawItemOffset := DrawItems[i].CharOffs + GetDrawItemOffsetAt(i, vX) - 1;

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
          Style.UpdateInfoRePaint;
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
        Undo_New;
        if vRectItem.MangerUndo then
          UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
        else
          UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

        vRectItem.KeyDown(Key, Shift);
        if vRectItem.SizeChanged then
        begin
          GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
          vRectItem.SizeChanged := False;
        end
        else
          Self.FormatInit;
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

          VK_LEFT:  // 移出去
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              vRectItem.Active := False;
              Style.UpdateInfoRePaint;
            end;

          VK_RIGHT:  // 移出去
            begin
              SelectInfo.StartItemOffset := OffsetAfter;
              vRectItem.Active := False;
              Style.UpdateInfoRePaint;
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
            if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actReturnItem) then Exit;
            GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            if vCurItem.ParaFirst then  // RectItem在段首，插入空行
            begin
              vCurItem := CreateDefaultTextItem;
              vCurItem.ParaFirst := True;
              Items.Insert(SelectInfo.StartItemNo, vCurItem);

              Undo_New;
              UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

              if APageBreak then
              begin
                UndoAction_ItemPageBreak(SelectInfo.StartItemNo + 1, 0, True);  // 我变成下一个了
                Items[SelectInfo.StartItemNo + 1].PageBreak := True;
              end;

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);

              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
            end
            else  // RectItem不在行首
            begin
              Undo_New;

              UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, True);
              vCurItem.ParaFirst := True;

              if APageBreak then
              begin
                UndoAction_ItemPageBreak(SelectInfo.StartItemNo, 0, True);
                vCurItem.PageBreak := True;
              end;

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
            end;
          end;

        VK_BACK:  // 在RectItem前
          begin
            if vCurItem.ParaFirst  // 是段首
              and DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actBackDeleteText)
            then
            begin
              if SelectInfo.StartItemNo > 0 then  // 第一个前回删不处理，停止格式化
              begin
                if vCurItem.ParaFirst and (SelectInfo.StartItemNo > 0) then
                begin
                  vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo - 1,
                    GetItemOffsetAfter(SelectInfo.StartItemNo - 1));

                  vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
                end
                else
                  GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);

                FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

                Undo_New;

                if IsEmptyLine(SelectInfo.StartItemNo - 1) then  // 上一行是空行
                begin
                  UndoAction_DeleteItem(SelectInfo.StartItemNo - 1, 0);
                  Items.Delete(SelectInfo.StartItemNo - 1);

                  SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;

                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
                  ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
                end
                else  // 上一行不是空行
                begin
                  UndoAction_ItemParaFirst(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, False);
                  vCurItem.ParaFirst := False;

                  if vCurItem.PageBreak then
                  begin
                    UndoAction_ItemPageBreak(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, False);
                    vCurItem.PageBreak := False;
                  end;

                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
                end;
              end;
            end
            else  // 不是段首
            begin
              // 选到上一个最后
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);

              KeyDown(Key, Shift);  // 执行前一个的删除
            end;
          end;

        VK_DELETE:  // 在RectItem前
          begin
            if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actDeleteItem) then  // 不可删除
            begin
              SelectInfo.StartItemOffset := OffsetAfter;
              Exit;
            end;

            GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            if vCurItem.ParaFirst then  // 是段首
            begin
              if SelectInfo.StartItemNo <> vFormatLastItemNo then  // 段不是只有一个
              begin
                Undo_New;
                UndoAction_ItemParaFirst(SelectInfo.StartItemNo + 1, 0, True);
                Items[SelectInfo.StartItemNo + 1].ParaFirst := True;

                UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
              end
              else  // 段删除空了
              begin
                Undo_New;
                UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                vCurItem := CreateDefaultTextItem;
                vCurItem.ParaFirst := True;
                Items.Insert(SelectInfo.StartItemNo, vCurItem);
                UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
              end;
            end
            else  // 不是段首
            begin
              if SelectInfo.StartItemNo < vFormatLastItemNo then  // 段中间
              begin
                vLen := GetItemOffsetAfter(SelectInfo.StartItemNo - 1);

                Undo_New;
                UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
                // 如果RectItem前面(同一行)有高度小于此RectItme的Item(如Tab)，
                // 其格式化时以RectItem为高，重新格式化时如果从RectItem所在位置起始格式化，
                // 行高度仍会以Tab为行高，也就是RectItem高度，所以需要从行开始格式化
                Items.Delete(SelectInfo.StartItemNo);
                if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then  // 原RectItem前后能合并
                begin
                  UndoAction_InsertText(SelectInfo.StartItemNo - 1,
                    Items[SelectInfo.StartItemNo - 1].Length + 1, Items[SelectInfo.StartItemNo].Text);

                  Items.Delete(SelectInfo.StartItemNo);
                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 2, -2);
                end
                else
                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

                SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
                SelectInfo.StartItemOffset := vLen;
              end
              else  // 段尾(段不只一个Item)
              begin
                Undo_New;
                UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

                SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
                SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
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
            if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actDeleteItem) then  // 不可删除
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              Exit;
            end;

            GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            if vCurItem.ParaFirst then  // 是段首
            begin
              if (SelectInfo.StartItemNo >= 0)
                and (SelectInfo.StartItemNo < Items.Count - 1)
                and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)
              then  // 同一段后面还有内容
              begin
                Undo_New;
                UndoAction_DeleteItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
                Items.Delete(SelectInfo.StartItemNo);

                UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, True);
                Items[SelectInfo.StartItemNo].ParaFirst := True;
                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

                ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
              end
              else  // 空段了
              begin
                Undo_New;
                UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                vItem := CreateDefaultTextItem;
                vItem.ParaFirst := True;
                Items.Insert(SelectInfo.StartItemNo, vItem);
                UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
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
              if Items[SelectInfo.StartItemNo + 1].ParaFirst then  // 下一个是段首（当前是在段最后面delete删除）
              begin
                vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
                vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo + 1);
                //GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
                FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

                Undo_New;
                if IsEmptyLine(SelectInfo.StartItemNo + 1) then
                begin
                  UndoAction_DeleteItem(SelectInfo.StartItemNo + 1, 0);
                  Items.Delete(SelectInfo.StartItemNo + 1);

                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
                  //ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
                end
                else
                begin
                  UndoAction_ItemParaFirst(SelectInfo.StartItemNo + 1, 0, False);
                  Items[SelectInfo.StartItemNo + 1].ParaFirst := False;

                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
                  ReSetSelectAndCaret(SelectInfo.StartItemNo + 1, 0);
                end;
              end
              else
              begin
                SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
                SelectInfo.StartItemOffset := 0;

                KeyDown(Key, Shift);
              end;
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
            if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actReturnItem) then Exit;
            GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            Undo_New;
            if (SelectInfo.StartItemNo < Items.Count - 1)  // 不是最后一个
              and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)  // 下一个不是段首
            then
            begin
              UndoAction_ItemParaFirst(SelectInfo.StartItemNo + 1, 0, True);
              Items[SelectInfo.StartItemNo + 1].ParaFirst := True;

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              SelectInfo.StartItemOffset := 0;
              CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
            end
            else
            begin
              vCurItem := CreateDefaultTextItem;
              vCurItem.ParaFirst := True;
              vCurItem.PageBreak := APageBreak;

              Items.Insert(SelectInfo.StartItemNo + 1, vCurItem);
              UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);
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
    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actReturnItem) then Exit;
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    // 判断光标位置内容如何换行
    if SelectInfo.StartItemOffset = 0 then  // 光标在Item最前面
    begin
      if not vCurItem.ParaFirst then  // 原来不是段首(光标在Item最前面)
      begin
        Undo_New;

        UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, True);
        vCurItem.ParaFirst := True;

        if APageBreak then
        begin
          UndoAction_ItemPageBreak(SelectInfo.StartItemNo, 0, True);
          vCurItem.PageBreak := True;
        end;

        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0, APageBreak);
      end
      else  // 原来就是段首(光标在Item最前面)
      begin
        if APageBreak then
        begin
          Undo_New;
          if Self.Items.Count = 1 then  // 文档只有一个Item
          begin
            // 补充一个空行
            vItem := CreateDefaultTextItem;
            vItem.StyleNo := vCurItem.StyleNo;
            vItem.ParaNo := vCurItem.ParaNo;
            vItem.ParaFirst := True;

            UndoAction_ItemPageBreak(SelectInfo.StartItemNo, 0, True);
            vCurItem.PageBreak := True;

            Items.Insert(SelectInfo.StartItemNo, vItem);  // 插入到当前
            UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);
          end
          else  // 不是文档唯一Item
          begin
            UndoAction_ItemPageBreak(SelectInfo.StartItemNo, 0, True);
            vCurItem.PageBreak := True;

            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0, True);
          end;
        end
        else
        begin
          vItem := CreateDefaultTextItem;
          vItem.ParaNo := vCurItem.ParaNo;
          vItem.StyleNo := vCurItem.StyleNo;
          vItem.ParaFirst := True;

          Items.Insert(SelectInfo.StartItemNo, vItem);  // 插入到当前

          Undo_New;
          UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);
        end;
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
          Undo_New;

          UndoAction_ItemParaFirst(SelectInfo.StartItemNo + 1, 0, True);
          vItem.ParaFirst := True;

          if APageBreak then
          begin
            UndoAction_ItemPageBreak(SelectInfo.StartItemNo + 1, 0, True);
            vItem.PageBreak := True;
          end;

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0, APageBreak);
        end
        else  // 下一个是段起始
        begin
          vItem := CreateDefaultTextItem;
          vItem.ParaNo := vCurItem.ParaNo;
          vItem.StyleNo := vCurItem.StyleNo;
          vItem.ParaFirst := True;

          if APageBreak then
            vItem.PageBreak := True;

          Items.Insert(SelectInfo.StartItemNo + 1, vItem);

          Undo_New;
          UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1, APageBreak);
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

        if APageBreak then
          vItem.PageBreak := True;

        Items.Insert(SelectInfo.StartItemNo + 1, vItem);

        Undo_New;
        UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);

        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1, APageBreak);
        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
        SelectInfo.StartItemOffset := 0;
      end;
    end
    else  // 光标在Item中间
    begin
      vItem := vCurItem.BreakByOffset(SelectInfo.StartItemOffset);  // 截断当前Item

      Undo_New;
      UndoAction_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, vItem.Text);

      vItem.ParaFirst := True;

      if APageBreak then
        vItem.PageBreak := True;

      Items.Insert(SelectInfo.StartItemNo + 1, vItem);
      UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);

      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1, APageBreak);

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
    vText, vsDelete: string;
    vPageBreak: Boolean;
    vItem: THCCustomItem;
    i, vCurItemNo, vLen, vDelCount, vParaNo: Integer;
    {$IFDEF UNPLACEHOLDERCHAR}
    vCharCount: Integer;
    {$ENDIF}
  begin
    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actDeleteText) then  // 不可删除
    begin
      SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;  // SelectInfo.StartItemOffset + 1
      ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      Exit;
    end;

    vDelCount := 0;
    vCurItemNo := SelectInfo.StartItemNo;
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);

    if SelectInfo.StartItemOffset = vCurItem.Length then  // 光标在Item最右边(包括空行)
    begin
      if vCurItemNo <> Items.Count - 1 then  // 不是最后一个Item最右边删除
      begin
        if Items[vCurItemNo + 1].ParaFirst then  // 下一个是段首，光标处Item是上一段最后一个，下一段要移上来
        begin
          vFormatLastItemNo := GetParaLastItemNo(vCurItemNo + 1);  // 获取下一段最后一个
          if vCurItem.Length = 0 then  // 当前是空行
          begin
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            Undo_New;
            UndoAction_DeleteItem(vCurItemNo, 0);
            Items.Delete(vCurItemNo);

            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
          end
          else  // 当前不是空行
          begin
            if Items[vCurItemNo + 1].StyleNo < THCStyle.Null then  // 下一个段首是RectItem，不能合并
            begin
              vFormatLastItemNo := GetParaLastItemNo(vCurItemNo + 1);  // 获取下一段最后一个
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              Undo_New;

              UndoAction_ItemParaFirst(vCurItemNo + 1, 0, False);
              Items[vCurItemNo + 1].ParaFirst := False;

              if Items[vCurItemNo + 1].PageBreak then
              begin
                UndoAction_ItemPageBreak(vCurItemNo + 1, 0, False);
                Items[vCurItemNo + 1].PageBreak := False;
              end;

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

              SelectInfo.StartItemNo := vCurItemNo + 1;
              SelectInfo.StartItemOffset := OffsetBefor;

              //KeyDown(Key, Shift);
              //Exit;
            end
            else  // 下一个段首是TextItem(当前在上一段段尾)
            begin
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              if Items[vCurItemNo + 1].Length = 0 then  // 下一段的段首是是空行
              begin
                Undo_New;
                UndoAction_DeleteItem(vCurItemNo + 1, 0);
                Items.Delete(vCurItemNo + 1);

                Inc(vDelCount);
              end
              else  // 下一段的段首不是空行
              begin
                //if (vCurItem.ClassType = Items[vCurItemNo + 1].ClassType)
                //  and (vCurItem.StyleNo = Items[vCurItemNo + 1].StyleNo)
                if vCurItem.CanConcatItems(Items[vCurItemNo + 1]) then  // 下一段段首可合并到当前(当前在上一段段尾) 201804111209 (不能用MergeItemText的情况)
                begin
                  Undo_New;

                  UndoAction_InsertText(vCurItemNo, vCurItem.Length + 1, Items[vCurItemNo + 1].Text);
                  vCurItem.Text := vCurItem.Text + Items[vCurItemNo + 1].Text;

                  UndoAction_DeleteItem(vCurItemNo + 1, 0);
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

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
            end;
          end;
        end
        else  // 下一个不能合并也不是段首，移动到下一个开头再调用DeleteKeyDown
        begin
          SelectInfo.StartItemNo := vCurItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;  // GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
          KeyDown(Key, Shift);
          Exit;
        end;
      end;
    end
    else  // 光标不在Item最右边
    begin
      vText := Items[vCurItemNo].Text;
      {$IFDEF UNPLACEHOLDERCHAR}
      vCharCount := GetTextActualOffset(vText, SelectInfo.StartItemOffset + 1, True) - SelectInfo.StartItemOffset;
      vsDelete := Copy(vText, SelectInfo.StartItemOffset + 1, vCharCount);
      Delete(vText, SelectInfo.StartItemOffset + 1, vCharCount);
      {$ELSE}
      vsDelete := Copy(vText, SelectInfo.StartItemOffset + 1, 1);
      Delete(vText, SelectInfo.StartItemOffset + 1, 1);
      {$ENDIF}
      DoItemAction(vCurItemNo, SelectInfo.StartItemOffset + 1, actDeleteText);

      if vText = '' then  // 删除后没有内容了
      begin
        if not DrawItems[Items[vCurItemNo].FirstDItemNo].LineFirst then  // 该Item不是行首(是行中间或行末尾)
        begin
          if vCurItemNo < Items.Count - 1 then  // 不是行首也不是最后一个Item
          begin
            if MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1]) then  // 下一个可合并到上一个
            begin
              vLen := Items[vCurItemNo + 1].Length;

              Undo_New;
              UndoAction_InsertText(vCurItemNo - 1, Items[vCurItemNo - 1].Length - vLen + 1, Items[vCurItemNo + 1].Text);

              GetFormatRange(vCurItemNo - 1, vLen, vFormatFirstDrawItemNo, vFormatLastItemNo);
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              UndoAction_DeleteItem(vCurItemNo, 0);
              Items.Delete(vCurItemNo);  // 删除当前

              UndoAction_DeleteItem(vCurItemNo, 0);
              Items.Delete(vCurItemNo);  // 删除下一个

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 2, -2);
            end
            else  // 下一个合并不到上一个
            begin
              vLen := 0;
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              Undo_New;
              UndoAction_DeleteItem(vCurItemNo, 0);
              Items.Delete(vCurItemNo);

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
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
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            Undo_New;
            UndoAction_DeleteItem(vCurItemNo, 0);
            Items.Delete(vCurItemNo);

            SelectInfo.StartItemNo := vCurItemNo - 1;
            SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
          end;
        end
        else  // 行首Item被删空了
        begin
          if vCurItemNo <> vFormatLastItemNo then  // 当前段后面还有Item
          begin
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
            SelectInfo.StartItemOffset := 0;

            Undo_New;
            UndoAction_ItemParaFirst(vCurItemNo + 1, 0, Items[vCurItemNo].ParaFirst);
            Items[vCurItemNo + 1].ParaFirst := Items[vCurItemNo].ParaFirst;

            Undo_New;
            UndoAction_DeleteItem(vCurItemNo, 0);
            Items.Delete(vCurItemNo);

            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
          end
          else  // 当前段删除空了
          begin
            // 防止不支持键盘输入修改内容的数据元是段第一个且用键盘删空内容后
            // 再用键盘输入时数据元保留空内容的问题(因数据元不支持手动修改内容)
            // 所以使用先删除为空的Item再插入空Item 20190802001
            FormatPrepare(vFormatFirstDrawItemNo);

            vPageBreak := vCurItem.PageBreak;

            Undo_New;
            UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
            Items.Delete(SelectInfo.StartItemNo);

            vItem := CreateDefaultTextItem;
            vItem.ParaFirst := True;
            vItem.PageBreak := vPageBreak;

            Items.Insert(SelectInfo.StartItemNo, vItem);
            UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

            {vCurItem.Text := vText;

            Undo_New;
            UndoAction_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, vsDelete);

            FormatPrepare(vFormatFirstDrawItemNo); }
            SelectInfo.StartItemOffset := 0;

            ReFormatData(vFormatFirstDrawItemNo);
          end;
        end;
      end
      else  // 删除后还有内容
      begin
        vCurItem.Text := vText;

        Undo_New;
        UndoAction_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, vsDelete);

        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
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
    i, vCurItemNo, vLen, vDelCount, vParaNo: Integer;
    vParaFirst, vPageBreak: Boolean;
    vParaStyle: THCParaStyle;
    vItem: THCCustomItem;
  begin
    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actBackDeleteText) then  // 不允许删除
    begin
      //LeftKeyDown  // 往前走
      SelectInfo.StartItemOffset := 0;
      ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      Exit;
    end;

    if SelectInfo.StartItemOffset = 0 then  // 光标在Item最开始
    begin
      if (vCurItem.Text = '') and (Style.ParaStyles[vCurItem.ParaNo].AlignHorz <> TParaAlignHorz.pahJustify) then
        ApplyParaAlignHorz(TParaAlignHorz.pahJustify)  // 居中等对齐的空Item，删除时切换到分散对齐
      else
      if vCurItem.ParaFirst and (Style.ParaStyles[vCurItem.ParaNo].FirstIndent > 0) then  // 在段最前面删除
      begin
        vParaStyle := Style.ParaStyles[vCurItem.ParaNo];
        ApplyParaFirstIndent(Max(0, vParaStyle.FirstIndent - PixXToMillimeter(TabCharWidth)));
      end
      else
      if vCurItem.PageBreak then
      begin
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

        Undo_New;
        UndoAction_ItemPageBreak(SelectInfo.StartItemNo, 0, False);
        vCurItem.PageBreak := False;

        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0, True);
      end
      else
      if SelectInfo.StartItemNo <> 0 then  // 不是第1个Item最前面删除
      begin
        //vCurItemNo := SelectInfo.StartItemNo;
        if vCurItem.ParaFirst then  // 是段起始Item
        begin
          vLen := Items[SelectInfo.StartItemNo - 1].Length;

          //if (vCurItem.ClassType = Items[SelectInfo.StartItemNo - 1].ClassType)
          //  and (vCurItem.StyleNo = Items[SelectInfo.StartItemNo - 1].StyleNo)
          if vCurItem.CanConcatItems(Items[SelectInfo.StartItemNo - 1]) then  // 当前可以和上一个合并(当前在段首) 201804111209 (不能用MergeItemText的情况)
          begin
            vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo - 1, vLen);
            //vFormatFirstItemNo := GetLineFirstItemNo(SelectInfo.StartItemNo - 1, vLen);
            vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            Undo_New;
            UndoAction_InsertText(SelectInfo.StartItemNo - 1, Items[SelectInfo.StartItemNo - 1].Length + 1,
              Items[SelectInfo.StartItemNo].Text);

            Items[SelectInfo.StartItemNo - 1].Text := Items[SelectInfo.StartItemNo - 1].Text
              + Items[SelectInfo.StartItemNo].Text;

            UndoAction_DeleteItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
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

            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

            ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, vLen);
          end
          else  // 段前删除且段第一个Item不能和上一段最后Item合并
          if IsEmptyLine(SelectInfo.StartItemNo - 1) then  // 上一段是空段
          begin
            vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo - 1, vLen);
            vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            Undo_New;
            UndoAction_DeleteItem(SelectInfo.StartItemNo - 1, 0);
            Items.Delete(SelectInfo.StartItemNo - 1);

            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

            ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, 0);
          end
          else
          begin
            if vCurItem.Length = 0 then  // 已经没有内容了(不是第1个Item，说明是空行)
            begin
              vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo - 1, vLen);
              FormatPrepare(vFormatFirstDrawItemNo, SelectInfo.StartItemNo);

              Undo_New;
              UndoAction_DeleteItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
              Items.Delete(SelectInfo.StartItemNo);

              ReFormatData(vFormatFirstDrawItemNo, SelectInfo.StartItemNo - 1, -1);

              ReSetSelectAndCaret(SelectInfo.StartItemNo - 1);
            end
            else  // 段前删除且段第一个Item不能和上一段最后Item合并，上一段不是空行
            begin
              vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo - 1, GetItemOffsetAfter(SelectInfo.StartItemNo - 1));
              vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
              //GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              Undo_New;
              UndoAction_ItemParaFirst(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, False);

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

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

              ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
            end;
          end;
        end
        else  // 在不是第1个Item开始往前删，且Item不是段起始
        begin
          // 从前一个最后开始重新处理
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
          CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
          KeyDown(Key, Shift);
          Exit;
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
            Undo_New;
            UndoAction_InsertText(vCurItemNo - 1, Items[vCurItemNo - 1].Length - Items[vCurItemNo + 1].Length + 1,
              Items[vCurItemNo + 1].Text);

            GetFormatRange(vCurItemNo - 1, vLen, vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
            Items.Delete(vCurItemNo);  // 删除当前

            UndoAction_DeleteItem(vCurItemNo, 0);
            Items.Delete(vCurItemNo);  // 删除下一个

            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 2, -2);

            ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, vLen);  // 上一个原光标位置
          end
          else  // 当前不是行首，删除后没有内容了，且不能合并上一个和下一个
          begin
            if SelectInfo.StartItemNo = vParaLastItemNo then  // 段最后一个
            begin
              //vFormatFirstItemNo := GetLineFirstItemNo(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
              //vFormatLastItemNo := vParaLastItemNo;
              GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              Undo_New;
              UndoAction_DeleteItem(vCurItemNo, SelectInfo.StartItemOffset);
              Items.Delete(vCurItemNo);

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

              ReSetSelectAndCaret(vCurItemNo - 1);
            end
            else  // 不是段最后一个
            begin
              GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              Undo_New;
              UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
              Items.Delete(vCurItemNo);

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

              ReSetSelectAndCaret(vCurItemNo - 1);
            end;
          end;
        end
        else  // Item是行第一个、行首Item删除空了
        begin
          if Items[vCurItemNo].ParaFirst then  // 是段首，删除空了
          begin
            GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            if vCurItemNo < vFormatLastItemNo then  // 同段后面还有内容
            begin
              Undo_New;

              vParaFirst := True;  // Items[vCurItemNo].ParaFirst;  // 记录行首Item的段属性

              UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
              Items.Delete(vCurItemNo);

              if vParaFirst then  // 删除的是段首
              begin
                UndoAction_ItemParaFirst(vCurItemNo, 0, vParaFirst);
                Items[vCurItemNo].ParaFirst := vParaFirst;  // 其后继承段首属性
              end;

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
              ReSetSelectAndCaret(vCurItemNo, 0);  // 下一个最前面
            end
            else  // 是段首删除空，同段后面没有内容了，变成空行
            begin
              // 防止不支持键盘输入修改内容的数据元是段第一个且用键盘Backspace删空内容后
              // 再用键盘输入时数据元保留空内容的问题(因数据元不支持手动修改内容)
              // 所以使用先删除为空的Item再插入空Item 20190802001
              vPageBreak := vCurItem.PageBreak;

              Undo_New;
              UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
              Items.Delete(SelectInfo.StartItemNo);

              vItem := CreateDefaultTextItem;
              vItem.ParaFirst := True;
              vItem.PageBreak := vPageBreak;

              Items.Insert(SelectInfo.StartItemNo, vItem);
              UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
              SelectInfo.StartItemOffset := 0;

              {UndoAction_DeleteBackText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset,
                vCurItem.Text);  // Copy(vText, SelectInfo.StartItemOffset, 1));

              //System.Delete(vText, SelectInfo.StartItemOffset, 1);
              vCurItem.Text := '';  // vText;
              SelectInfo.StartItemOffset := SelectInfo.StartItemOffset - 1; }

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);  // 保留空行
            end;
          end
          else  // 不是段首Item，仅是行首Item删除空了
          begin
            Undo_New;

            if vCurItemNo < GetParaLastItemNo(vCurItemNo) then  // 如果删除后，同段后面还有内容
            begin
              vLen := Items[vCurItemNo - 1].Length;
              if MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1]) then  // 前后能合并
              begin
                UndoAction_InsertText(vCurItemNo - 1,
                  Items[vCurItemNo - 1].Length - Items[vCurItemNo + 1].Length + 1, Items[vCurItemNo + 1].Text);

                GetFormatRange(vCurItemNo - 1, GetItemOffsetAfter(vCurItemNo - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);  // 取前一个格式化起始位置
                FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

                UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);  // 删除空的Item
                Items.Delete(vCurItemNo);

                UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);  // 被合并的Item
                Items.Delete(vCurItemNo);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 2, -2);
                ReSetSelectAndCaret(vCurItemNo - 1, vLen);
              end
              else  // 前后不能合并
              begin
                GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
                FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

                UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
                Items.Delete(vCurItemNo);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
                ReSetSelectAndCaret(vCurItemNo - 1);
              end;
            end
            else  // 同段后面没有内容了
            begin
              GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
              Items.Delete(vCurItemNo);

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
              ReSetSelectAndCaret(vCurItemNo - 1);
            end;
          end;
        end;
      end
      else  // 删除后还有内容 光标不在Item最开始，文本Item
      begin
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

        DoItemAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actBackDeleteText);
        vText := vCurItem.Text;  // 和上面 201806242257 处一样

        Undo_New;
        UndoAction_DeleteBackText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset,
          Copy(vText, SelectInfo.StartItemOffset, 1));

        System.Delete(vText, SelectInfo.StartItemOffset, 1);
        vCurItem.Text := vText;

        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

        SelectInfo.StartItemOffset := SelectInfo.StartItemOffset - 1;
        ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      end;
    end;
  end;
  {$ENDREGION}

begin
  if IsKeyDownEdit(Key) and (not CanEdit) then Exit;

  if Key in [VK_BACK, VK_DELETE, VK_RETURN, VK_TAB] then
    Self.InitializeMouseField;  // 如果Item删除完了，原MouseMove处ItemNo可能不存在了，再MouseMove时清除旧的出错

  vCurItem := GetActiveItem;
  if not Assigned(vCurItem) then Exit;  // 跨页合并时，合并后并没有当前Item

  vSelectExist := SelectExists;

  if vSelectExist and (Key in [VK_BACK, VK_DELETE, VK_RETURN, VK_TAB]) then
  begin
    if not DeleteSelected then Exit;
    if Key in [VK_BACK, VK_DELETE] then Exit;
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

procedure THCRichData.KeyPress(var Key: Char);
var
  vCarteItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  if not CanEdit then Exit;

  //DeleteSelected;

  vCarteItem := GetActiveItem;
  if not Assigned(vCarteItem) then Exit;  // 跨页合并时，合并后并没有当前Item

  if (vCarteItem.StyleNo < THCStyle.Null)  // 当前位置是 RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // 在其上输入内容
  then
  begin
    Undo_New;

    vRectItem := vCarteItem as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    vRectItem.KeyPress(Key);
    if vRectItem.SizeChanged then
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      if Key <> #0 then
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vRectItem.SizeChanged := False;
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
      Style.UpdateInfoReScroll;
    end
    else
      Self.FormatInit;
  end
  else
    InsertText(Key);
end;

procedure THCRichData.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not CanEdit then Exit;
end;

procedure THCRichData.KillFocus;
var
  vItemNo: Integer;
begin
  vItemNo := GetActiveItemNo;
  if vItemNo >= 0 then
    Items[vItemNo].KillFocus;
end;

procedure THCRichData.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  if not CanEdit then Exit;
  //Self.InitializeField;  LoadFromStream中的Clear处理了
  inherited DoLoadFromStream(AStream, AStyle, AFileVersion);

  Self.BeginFormat;
  try
    InsertStream(AStream, AStyle, AFileVersion);
    // 加载完成后，初始化(有一部分在LoadFromStream中初始化了)
    ReSetSelectAndCaret(0, 0);
  finally
    Self.EndFormat;
  end;
end;

function THCRichData.MergeTableSelectCells: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).MergeSelectCells;
    end);
end;

procedure THCRichData.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  {$REGION 'DoItemMouseDown'}
  procedure DoItemMouseDown(const AItemNo, AOffset: Integer);
  var
    vX, vY: Integer;
  begin
    if AItemNo < 0 then Exit;
    CoordToItemOffset(X, Y, AItemNo, AOffset, vX, vY);
    Items[AItemNo].MouseDown(Button, Shift, vX, vY);

    if Assigned(FOnItemMouseDown) then
      FOnItemMouseDown(Self, AItemNo, AOffset, Button, Shift, vX, vY);
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
  //FSelectSeekOffset := -1;

  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);

  FMouseDownX := X;
  FMouseDownY := Y;

  GetItemAt(X, Y, vMouseDownItemNo, vMouseDownItemOffset, vDrawItemNo, vRestrain);

  if (Button = mbLeft) and (ssShift in Shift) then  // shift键重新确定选中范围
  begin
    if SelectByMouseDownShift(vMouseDownItemNo, vMouseDownItemOffset) then
    begin
      MatchItemSelectState;  // 设置选中范围内的Item选中状态
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;

      FMouseDownItemNo := vMouseDownItemNo;
      FMouseDownItemOffset := vMouseDownItemOffset;
      FSelectSeekNo := vMouseDownItemNo;
      FSelectSeekOffset := vMouseDownItemOffset;

      if (not vRestrain) and (Items[FMouseDownItemNo].StyleNo < THCStyle.Null) then  // RectItem
        DoItemMouseDown(FMouseDownItemNo, FMouseDownItemOffset);

      Exit;
    end;
  end;

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
      SelectInfo.StartRestrain := vRestrain;
      CaretDrawItemNo := vDrawItemNo;
    end;

    //if not vRestrain then  // 没收敛，因跨页Item点击在前后位置时需要处理光标数据所以不能限制
      DoItemMouseDown(FMouseDownItemNo, FMouseDownItemOffset);
  end;
end;

procedure THCRichData.MouseLeave;
begin
  if FMouseMoveItemNo >= 0 then
  begin
    DoItemMouseLeave(FMouseMoveItemNo);
    FMouseMoveItemNo := -1;
    FMouseMoveItemOffset := -1;
    FMouseMoveDrawItemNo := -1;
    Style.UpdateInfoRePaint;
  end;
end;

procedure THCRichData.MouseMove(Shift: TShiftState; X, Y: Integer);

  {$REGION 'DoItemMouseMove'}
  procedure DoItemMouseMove(const AItemNo, AOffset: Integer; const ADrawItemMouseMove: Boolean = False);
  var
    vX, vY: Integer;
  begin
    if AItemNo < 0 then Exit;
    CoordToItemOffset(X, Y, AItemNo, AOffset, vX, vY);
    Items[AItemNo].MouseMove(Shift, vX, vY);

    if ADrawItemMouseMove then
      DoDrawItemMouseMove(Self, AItemNo, AOffset, FMouseMoveDrawItemNo, TMouseButton.mbLeft, Shift, vX, vY);
  end;
  {$ENDREGION}

var
  vMouseMoveItemNo, vMouseMoveItemOffset: Integer;
  vRestrain: Boolean;
begin
  if SelectedResizing then  // RectItem resizing，goon
  begin
    FMouseMoveItemNo := FMouseDownItemNo;
    FMouseMoveItemOffset := FMouseDownItemOffset;
    FMouseMoveRestrain := False;
    DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
    Style.UpdateInfoRePaint;

    Exit;
  end;

  GetItemAt(X, Y, vMouseMoveItemNo, vMouseMoveItemOffset, FMouseMoveDrawItemNo, vRestrain);

  if FDraging or Style.UpdateInfo.Draging then  // 拖拽
  begin
    GCursor := crDrag;

    FMouseMoveItemNo := vMouseMoveItemNo;
    FMouseMoveItemOffset := vMouseMoveItemOffset;
    SelectInfo.StartItemNo := vMouseMoveItemNo;
    SelectInfo.StartItemOffset := vMouseMoveItemOffset;
    FMouseMoveRestrain := vRestrain;
    CaretDrawItemNo := FMouseMoveDrawItemNo;

    Style.UpdateInfoReCaret;

    if (not vRestrain) and (Items[FMouseMoveItemNo].StyleNo < THCStyle.Null) then  // RectItem
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
  end
  else
  if FSelecting then  // 划选
  begin
    if (Items[FMouseDownItemNo].StyleNo < THCStyle.Null)
      and (FMouseDownItemOffset = OffsetInner)
    then  // 按下时在RectItem上，划选限制在此RectItem里，否则要处理表格是第一个Item，
    begin // 第一个单元格从后往前划选全部后移出表格后输入内容的替换到表格前面的问题。
      FMouseMoveItemNo := FMouseDownItemNo;
      FMouseMoveItemOffset := FMouseDownItemOffset;

      if vMouseMoveItemNo = FMouseDownItemNo then  // 在按下的RectItem上移动
        FMouseMoveRestrain := vRestrain
      else  // 都视为约束
        FMouseMoveRestrain := True;

      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;

      Exit;  // 限制在RectItem内部处理
    end
    else
    begin
      FMouseMoveItemNo := vMouseMoveItemNo;
      FMouseMoveItemOffset := vMouseMoveItemOffset;
      FMouseMoveRestrain := vRestrain;
    end;

    AdjustSelectRange(FMouseDownItemNo, FMouseDownItemOffset,
      FMouseMoveItemNo, FMouseMoveItemOffset);  // 确定SelectRang

    FSelectSeekNo := FMouseMoveItemNo;
    FSelectSeekOffset := FMouseMoveItemOffset;

    if Self.SelectExists then
      MatchItemSelectState  // 设置选中范围内的Item选中状态
    else
      CaretDrawItemNo := FMouseMoveDrawItemNo;  // 按下上一个DrawItem最后，划选到下一个开始时，没有选中内容，要更换CaretDrawIemNo

    if (Items[FMouseMoveItemNo].StyleNo < THCStyle.Null)
      and (FMouseDownItemOffset = OffsetInner)
    then  // 划选 RectItem
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
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
    begin
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset, True);
      if (ssCtrl in Shift) and (Items[FMouseMoveItemNo].HyperLink <> '') then
        GCursor := crHandPoint;
    end;
  end;
end;

procedure THCRichData.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vUpItemNo, vUpItemOffset, vDrawItemNo: Integer;
  vRestrain: Boolean;

  {$REGION ' DoItemMouseUp '}
  procedure DoItemMouseUp(const AItemNo, AOffset: Integer);
  var
    vX, vY: Integer;
  begin
    if AItemNo < 0 then Exit;
    CoordToItemOffset(X, Y, AItemNo, AOffset, vX, vY);
    Items[AItemNo].MouseUp(Button, Shift, vX, vY);

    if (not vRestrain) and Assigned(FOnItemMouseUp) then
      FOnItemMouseUp(Self, AItemNo, AOffset, Button, Shift, vX, vY);
  end;
  {$ENDREGION}

  {$REGION ' DoNormalMouseUp '}
  procedure DoNormalMouseUp;
  begin
    if FMouseMoveItemNo < 0 then
    begin
      SelectInfo.StartItemNo := vUpItemNo;
      SelectInfo.StartItemOffset := vUpItemOffset;
      SelectInfo.StartRestrain := vRestrain;
      CaretDrawItemNo := vDrawItemNo;
    end
    else
    begin
      SelectInfo.StartItemNo := FMouseMoveItemNo;
      SelectInfo.StartItemOffset := FMouseMoveItemOffset;
      SelectInfo.StartRestrain := vRestrain;
      CaretDrawItemNo := FMouseMoveDrawItemNo;
    end;

    Style.UpdateInfoRePaint;

    if not FMouseDownReCaret then  // 避免重复获取光标位置
      Style.UpdateInfoReCaret;

    //if Items[vUpItemNo].StyleNo < THCStyle.Null then  // RectItem
      DoItemMouseUp(vUpItemNo, vUpItemOffset);  // 弹起，因为可能是移出Item后弹起，所以这里不受vRestrain约束
  end;
  {$ENDREGION}

var
  i, vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
  //vMouseUpInSelect: Boolean;
begin
  //if not FMouseLBDowning then Exit;  // 屏蔽OpenDialog对话框双击引起的弹起
  FMouseLBDowning := False;

  if FMouseLBDouble then Exit;

  if (Button = mbLeft) and (ssShift in Shift) then Exit;  // shift键重新确定选中范围

  if SelectedResizing then  // RectItem缩放ing，停止缩放
  begin
    Undo_New;
    UndoAction_ItemSelf(FMouseDownItemNo, FMouseDownItemOffset);

    DoItemMouseUp(FMouseDownItemNo, FMouseDownItemOffset);
    DoItemResized(FMouseDownItemNo);  // 缩放完成事件(可控制缩放不要超过页面)
    GetFormatRange(FMouseDownItemNo, FMouseDownItemOffset, vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
    Style.UpdateInfoRePaint;

    Exit;
  end;

  GetItemAt(X, Y, vUpItemNo, vUpItemOffset, vDrawItemNo, vRestrain);

  if FSelecting or Style.UpdateInfo.Selecting then  // 划选完成弹起
  begin
    FSelecting := False;

    // 选中范围内的RectItem取消划选状态(此时表格的FSelecting为True)
    if SelectInfo.StartItemNo >= 0 then  // 表格取消选中后单元格Data的StartItemNo被置为-1，下次表格外选到表格里时，UpdateInfo.Selecting为True导致StartItemNo和EndItemNo都是-1出错
    begin
      for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
      begin
        if (i <> vUpItemNo) and (Items[i].StyleNo < THCStyle.Null) then
          DoItemMouseUp(i, 0);
      end;
    end;

    if SelectInfo.EndItemNo < 0 then  // 在RectItem里划选或TextItem划选回起始位置
    begin
      if (FMouseDownItemNo >= 0) and (Items[FMouseDownItemNo].StyleNo < THCStyle.Null) then  // 弹起时在RectItem
        DoItemMouseUp(FMouseDownItemNo, OffsetInner)
      else
        DoItemMouseUp(vUpItemNo, vUpItemOffset);
    end
    else
    if Items[vUpItemNo].StyleNo < THCStyle.Null then  // 弹起时在RectItem
      DoItemMouseUp(vUpItemNo, vUpItemOffset);
  end
  else
  if FDraging or Style.UpdateInfo.Draging then  // 拖拽弹起
  begin
    FDraging := False;
    //vMouseUpInSelect := CoordInSelect(X, Y, vUpItemNo, vUpItemOffset, vRestrain);

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

procedure THCRichData.ParseXml(const ANode: IHCXMLNode);
begin
  if not CanEdit then Exit;
  inherited ParseXml(ANode);

  ReFormat;
  //ReSetSelectAndCaret(0, 0);

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
  Style.UpdateInfoReScroll;
end;

procedure THCRichData.ActiveItemReAdaptEnvironment;
var
  vActiveItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
  vItemNo, vFormatFirstDrawItemNo, vFormatLastItemNo, vLen, vExtraCount: Integer;
begin
  if not CanEdit then Exit;

  Self.InitializeField;
  vActiveItem := GetActiveItem;
  if not Assigned(vActiveItem) then Exit;  // 跨页合并时，合并后并没有当前Item

  if (vActiveItem.StyleNo < THCStyle.Null)  // 当前位置是 RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // 在其上输入内容
  then
  begin
    Undo_New;

    vRectItem := vActiveItem as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    vRectItem.ActiveItemReAdaptEnvironment;
    if vRectItem.SizeChanged then
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vRectItem.SizeChanged := False;
    end
    else
      Self.FormatInit;
  end
  else
  begin
    vExtraCount := 0;
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

    Undo_New;

    vItemNo := SelectInfo.StartItemNo;
    if MergeItemToNext(vItemNo) then  // 可以合并到下一个Item
    begin
      UndoAction_InsertText(vItemNo, Items[vItemNo].Length - Items[vItemNo + 1].Length + 1, Items[vItemNo + 1].Text);
      UndoAction_DeleteItem(vItemNo + 1, 0);
      Items.Delete(vItemNo + 1);
      Dec(vExtraCount);
    end;

    if vItemNo > 0 then  // 向前合并
    begin
      vLen := Items[vItemNo - 1].Length;
      if MergeItemToPrio(vItemNo) then  // 当前Item合并到上一个Item(如果上面合并了，vItem已经失效，不能直接使用了)
      begin
        UndoAction_InsertText(vItemNo - 1, Items[vItemNo - 1].Length - Items[vItemNo].Length + 1, Items[vItemNo].Text);
        UndoAction_DeleteItem(vItemNo, 0);
        Items.Delete(vItemNo);
        Dec(vExtraCount);

        ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, vLen + SelectInfo.StartItemOffset);
      end;
    end;

    if vExtraCount <> 0 then
    begin
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + vExtraCount, vExtraCount);
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
    end;
  end;
end;

function THCRichData.SelectByMouseDownShift(var AMouseDownItemNo,
  AMouseDownItemOffset: Integer): Boolean;
var
  vSelItemNo, vSelItemOffset: Integer;
begin
  Result := True;

  if Self.SelectExists then  // 原来就有选中
  begin
    if IsSelectSeekStart then  // 上一次划选完成后是在选中起始
    begin
      if (AMouseDownItemNo < FSelectSeekNo)
        or ((AMouseDownItemNo = FSelectSeekNo) and (AMouseDownItemOffset < FSelectSeekOffset))
      then  // 点在了原选中范围起始位置前面
      begin
        vSelItemNo := SelectInfo.EndItemNo;
        vSelItemOffset := SelectInfo.EndItemOffset;

        AdjustSelectRange(AMouseDownItemNo, AMouseDownItemOffset, vSelItemNo, vSelItemOffset);  // 确定SelectRang
      end
      else
      if ((AMouseDownItemNo > FSelectSeekNo) and (AMouseDownItemNo < SelectInfo.EndItemNo))
        or ((AMouseDownItemNo = FSelectSeekNo) and (AMouseDownItemOffset > FSelectSeekOffset))
        or ((AMouseDownItemNo = SelectInfo.EndItemNo) and (AMouseDownItemOffset < SelectInfo.EndItemOffset))
      then  // 在原选中范围起始和结束中间
      begin
        vSelItemNo := SelectInfo.EndItemNo;
        vSelItemOffset := SelectInfo.EndItemOffset;

        AdjustSelectRange(AMouseDownItemNo, AMouseDownItemOffset, vSelItemNo, vSelItemOffset);  // 确定SelectRang
      end
      else
      if (AMouseDownItemNo > SelectInfo.EndItemNo)
        or ((AMouseDownItemNo = SelectInfo.EndItemNo) and (AMouseDownItemOffset > SelectInfo.EndItemOffset))
      then  // 在结束位置后面
      begin
        vSelItemNo := SelectInfo.EndItemNo;
        vSelItemOffset := SelectInfo.EndItemOffset;

        AdjustSelectRange(vSelItemNo, vSelItemOffset, AMouseDownItemNo, AMouseDownItemOffset);  // 确定SelectRang
      end
      else
        Result := False;
    end
    else  // 划选完成后是在结束
    begin
      if (AMouseDownItemNo > FSelectSeekNo)
        or ((AMouseDownItemNo = FSelectSeekNo) and (AMouseDownItemOffset > FSelectSeekOffset))
      then  // 点在了原选中范围结束位置后面
      begin
        vSelItemNo := SelectInfo.StartItemNo;
        vSelItemOffset := SelectInfo.StartItemOffset;

        AdjustSelectRange(vSelItemNo, vSelItemOffset, AMouseDownItemNo, AMouseDownItemOffset);  // 确定SelectRang
      end
      else
      if ((AMouseDownItemNo > SelectInfo.StartItemNo) and (AMouseDownItemNo < FSelectSeekNo))
        or ((AMouseDownItemNo = FSelectSeekNo) and (AMouseDownItemOffset < FSelectSeekOffset))
        or ((AMouseDownItemNo = SelectInfo.StartItemNo) and (AMouseDownItemOffset > SelectInfo.StartItemOffset))
      then  // 在原选中范围起始和结束中间
      begin
        vSelItemNo := SelectInfo.StartItemNo;
        vSelItemOffset := SelectInfo.StartItemOffset;

        AdjustSelectRange(vSelItemNo, vSelItemOffset, AMouseDownItemNo, AMouseDownItemOffset);  // 确定SelectRang
      end
      else
      if (AMouseDownItemNo < SelectInfo.StartItemNo)
        or ((AMouseDownItemNo = SelectInfo.StartItemNo) and (AMouseDownItemOffset < SelectInfo.StartItemOffset))
      then  // 在起始位置前面
      begin
        vSelItemNo := SelectInfo.StartItemNo;
        vSelItemOffset := SelectInfo.StartItemOffset;

        AdjustSelectRange(AMouseDownItemNo, AMouseDownItemOffset, vSelItemNo, vSelItemOffset);  // 确定SelectRang
      end
      else
       Result := False;
    end;
  end
  else  // 原来没有选中
  if SelectInfo.StartItemNo >= 0 then
  begin
    if (AMouseDownItemNo < SelectInfo.StartItemNo)
      or ((AMouseDownItemNo = SelectInfo.StartItemNo) and (AMouseDownItemOffset < SelectInfo.StartItemOffset))
    then  // 点在了原光标位置前面
    begin
      vSelItemNo := SelectInfo.StartItemNo;
      vSelItemOffset := SelectInfo.StartItemOffset;

      AdjustSelectRange(AMouseDownItemNo, AMouseDownItemOffset, vSelItemNo, vSelItemOffset);  // 确定SelectRang
    end
    else
    if (AMouseDownItemNo > SelectInfo.StartItemNo)
      or ((AMouseDownItemNo = SelectInfo.StartItemNo) and (AMouseDownItemOffset > SelectInfo.StartItemOffset))
    then  // 点在了原光标位置后面
    begin
      vSelItemNo := SelectInfo.StartItemNo;
      vSelItemOffset := SelectInfo.StartItemOffset;

      AdjustSelectRange(vSelItemNo, vSelItemOffset, AMouseDownItemNo, AMouseDownItemOffset);  // 确定SelectRang
    end
    else
      Result := False;
  end;
end;

function THCRichData.SelectPerfect: Boolean;
var
  i: Integer;
begin
  Result := False;
  if SelectInfo.EndItemNo >= 0 then  // 混选时某RectItem只有一部分选中时不处理,仅取消选中
  begin
    for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
    begin
      if (Self.Items[i].StyleNo < THCStyle.Null) and (not Self.Items[i].IsSelectComplate) then
        Exit;
    end;
  end;

  Result := True;
end;

procedure THCRichData.SetActiveItemText(const AText: string);
var
  vActiveItem: THCCustomItem;
  vRectItem: THCCustomRectItem;
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  if not CanEdit then Exit;

  //if not DoInsertTextBefor(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, AText) then Exit;
  if AText = '' then Exit;

  Self.InitializeField;
  vActiveItem := GetActiveItem;
  if not Assigned(vActiveItem) then Exit;  // 跨页合并时，合并后并没有当前Item

  if (vActiveItem.StyleNo < THCStyle.Null)  // 当前位置是 RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // 在其上输入内容
  then
  begin
    Undo_New;

    vRectItem := vActiveItem as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    vRectItem.SetActiveItemText(AText);
    if vRectItem.SizeChanged then
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vRectItem.SizeChanged := False;
    end
    else
      Self.FormatInit;
  end
  else
  begin
    if Pos(#13#10, AText) > 0 then
    begin
      Undo_GroupBegin(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      try
        DeleteItems(SelectInfo.StartItemNo, SelectInfo.StartItemNo, True);
        InsertText(AText);
      finally
        Undo_GroupEnd(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      end;

      Exit;
    end;

    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actSetItemText) then Exit;  // 不允许设置

    Undo_New;
    UndoAction_SetItemText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, AText);
    Items[SelectInfo.StartItemNo].Text := AText;

    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

    SelectInfo.StartItemOffset := Length(AText);
    SelectInfo.EndItemNo := -1;
    SelectInfo.EndItemOffset := -1;
    Items[SelectInfo.StartItemNo].DisSelect;

    ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  end;

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
  //Style.UpdateInfoReScroll;
end;

procedure THCRichData.SetEmptyData;
var
  vItem: THCCustomItem;
begin
  if Self.Items.Count = 0 then
  begin
    vItem := CreateDefaultTextItem;
    vItem.ParaFirst := True;
    Items.Add(vItem);  // 不使用InsertText，为避免其触发ReFormat时因为没有格式化过，获取不到对应的DrawItem

    ReFormat;
    //ReSetSelectAndCaret(0);  // 防止清空后格式化完成后没有选中起始访问出错
  end;
end;

procedure THCRichData.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

end.
