{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{             文档内各类对象基本管理单元                }
{                                                       }
{*******************************************************}

unit HCCustomRichData;

interface

uses
  Windows, Classes, Types, Controls, Graphics, SysUtils, HCCustomData, HCStyle,
  HCItem, HCDrawItem, HCTextStyle, HCParaStyle, HCStyleMatch, HCCommon;

type
  TInsertProc = reference to function(const AItem: THCCustomItem): Boolean;

  TItemPaintEvent = procedure(const AData: THCCustomData;
    const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
    const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;

  TItemMouseEvent = procedure(const AData: THCCustomData; const AItemNo: Integer;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  THCCustomRichData = class(THCCustomData)
  strict private
    FWidth: Cardinal;
    /// <summary> 鼠标左键按下(打开文件对话框双击文件后会触发MouseMouse，MouseUp) </summary>
    FMouseLBDowning,
    /// <summary> 鼠标在选中区域中 </summary>
    FMouseInSelect,
    FMouseMoveRestrain  // 并不是在Item范围内MouseMove而是通过约束坐标找到的
      : Boolean;

    FMouseDownX, FMouseDownY: Integer;

    FMouseDownItemNo,
    FMouseDownItemOffset,
    FMouseMoveItemNo,
    FMouseMoveItemOffset
      : Integer;

    FReadOnly,
    FSelecting, FDraging: Boolean;

    FOnInsertItem: TItemNotifyEvent;
    FOnItemMouseDown, FOnItemMouseUp: TItemMouseEvent;
    FOnItemPaintBefor, FOnItemPaintAfter: TItemPaintEvent;
    FOnCreateItem: TNotifyEvent;  // 新建了Item(目前主要是为了打字和用中文输入法输入英文时痕迹的处理)

    /// <summary> 添加一个空Item防止Data无Item </summary>
    procedure AddEmptyTextItem;

    /// <summary>
    /// 获取指定ItemNo最近的DrawItemNo(请保证AItemNo的最后一个DrawItemNo为-1)
    /// </summary>
    /// <param name="AItemNo"></param>
    /// <returns></returns>
    //function GetItemNearDrawItemNo(const AItemNo: Integer): Integer;

    /// <summary> 为避免表格插入行、列大量重复代码，使用匿名方法，但不支持D7 </summary>
    function TableInsertRC(const AProc: TInsertProc): Boolean;
  protected
    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; virtual;
    procedure Clear; override;

    // <summary> 根据AItem，初始化私有变量并重新设置选中位置和确定光标，一般用于Data变动后修正光标和选中位置 </summary>
    procedure ReSetSelectAndCaret(const AItemNo: Integer); overload;
    procedure ReSetSelectAndCaret(const AItemNo, AOffset: Integer); overload;

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

    // 选中内容应用样式
    function ApplySelectTextStyle(const AMatchStyle: TStyleMatch): Integer; override;
    function ApplySelectParaStyle(const AMatchStyle: TParaMatch): Integer; override;

    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const ADrawItemIndex: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const ADrawItemIndex: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    function DisSelect: Boolean; override;
    function CanDeleteItem(const AItemNo: Integer): Boolean; virtual;

    /// <summary> 删除选中内容(内部已经判断了是否有选中) </summary>
    /// <returns>True:有选中且删除成功</returns>
    function DeleteSelected: Boolean; override;

    procedure DoItemInsert(const AItem: THCCustomItem); virtual;
    procedure DoItemMouseLeave(const AItemNo: Integer); virtual;
    procedure DoItemMouseEnter(const AItemNo: Integer); virtual;
    function GetWidth: Cardinal; virtual;
    procedure SetWidth(const Value: Cardinal);
    function GetHeight: Cardinal; virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;

    /// <summary>
    /// 准备格式化参数
    /// </summary>
    /// <param name="AStartItemNo">开始格式化的Item</param>
    /// <param name="APrioDItemNo">上一个Item的最后一个DrawItemNo</param>
    /// <param name="APos">开始格式化位置</param>
    procedure _FormatReadyParam(const AStartItemNo: Integer;
      var APrioDrawItemNo: Integer; var APos: TPoint); virtual;

    function CalcContentHeight: Integer;
  public
    constructor Create(const AStyle: THCStyle); override;
    //
    function CanEdit: Boolean;
    /// <summary> 在光标处换行 </summary>
    function InsertBreak: Boolean;
    function InsertText(const AText: string): Boolean;
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
    function InsertLine(const ALineHeight: Integer): Boolean;
    /// <summary> 在光标处插入Item </summary>
    /// <param name="AItem"></param>
    /// <returns></returns>
    function InsertItem(const AItem: THCCustomItem): Boolean; overload; virtual;

    /// <summary> 在指定的位置插入Item </summary>
    /// <param name="AIndex"></param>
    /// <param name="AItem"></param>
    /// <returns></returns>
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem): Boolean; overload; virtual;

    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;

    function TableInsertRowAfter(const ARowCount: Byte): Boolean;
    function TableInsertRowBefor(const ARowCount: Byte): Boolean;
    function ActiveTableDeleteRow(const ARowCount: Byte): Boolean;
    function TableInsertColAfter(const AColCount: Byte): Boolean;
    function TableInsertColBefor(const AColCount: Byte): Boolean;
    function ActiveTableDeleteCol(const AColCount: Byte): Boolean;
    function MergeTableSelectCells: Boolean;
    procedure KillFocus; virtual;
    procedure DblClick(X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    {IKeyEvent}
    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyPress(var Key: Char); virtual;
    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    // Key返回0表示此键按下Data没有做任何事情
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;

    // Format仅负责格式化Item，ReFormat仅负责格式化后对后面Item和DrawItem的关联处理
    // 目前仅单元格用了，需要放到CellData中吗？
    procedure ReFormat(const AStartItemNo: Integer);
    procedure FormatData(const AStartItemNo, ALastItemNo: Integer);
    // Format仅负责格式化Item，ReFormat负责格式化后对后面Item和DrawItem的关联处理
    procedure ReFormatData_(const AStartItemNo: Integer; const ALastItemNo: Integer = -1;
      const AExtraItemCount: Integer = 0);

    /// <summary> 重新格式化当前Item(用于仅修改当前Item属性或内容) </summary>
    procedure ReFormatActiveItem;
    procedure GetCurStyle(var AStyleNo, AParaNo: Integer);
    function GetActiveItem: THCCustomItem;
    function GetActiveDrawItem: THCCustomDrawItem;
    function GetActiveDrawItemCoord: TPoint;

    /// <summary> 取消激活(用于页眉、页脚、正文切换时原激活的取消) </summary>
    procedure DisActive;

    /// <summary> 初始化字段和变量 </summary>
    procedure Initialize; virtual;

    function GetHint: string;

    /// <summary> 返回当前光标处的顶层Data </summary>
    function GetTopLevelData: THCCustomRichData;

    /// <summary> 返回指定位置处的顶层Data </summary>
    function GetTopLevelDataAt(const X, Y: Integer): THCCustomRichData;

    /// <summary>
    /// 绘制数据
    /// </summary>
    /// <param name="AOffsetX">数据偏移</param>
    /// <param name="AOffsetY">数据偏移</param>
    /// <param name="ADataScreenTop">数据在当前屏幕露出来的最上端</param>
    /// <param name="ADataScreenBottom">数据在当前屏幕露出来的最下端</param>
    /// <param name="AStartDItemNo">起始DItem</param>
    /// <param name="AEndDItemNo">结束DItem</param>
    /// <param name="ACanvas"></param>
//    procedure PaintData(const AOffsetX, AOffsetY, ADataScreenTop, ADataScreenBottom,
//      AStartDItemNo, AEndDItemNo: Integer; const ACanvas: TCanvas);

    property MouseDownItemNo: Integer read FMouseDownItemNo;
    property MouseDownItemOffset: Integer read FMouseDownItemOffset;
    property MouseMoveItemNo: Integer read FMouseMoveItemNo;
    property MouseMoveItemOffset: Integer read FMouseMoveItemOffset;
    property MouseMoveRestrain: Boolean read FMouseMoveRestrain;

    property Width: Cardinal read GetWidth write SetWidth;
    property Height: Cardinal read GetHeight;  // 实际内容的高
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property OnInsertItem: TItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnItemMouseDown: TItemMouseEvent read FOnItemMouseDown write FOnItemMouseDown;
    property OnItemMouseUp: TItemMouseEvent read FOnItemMouseUp write FOnItemMouseUp;
    property OnItemPaintBefor: TItemPaintEvent read FOnItemPaintBefor write FOnItemPaintBefor;
    property OnItemPaintAfter: TItemPaintEvent read FOnItemPaintAfter write FOnItemPaintAfter;
    property OnCreateItem: TNotifyEvent read FOnCreateItem write FOnCreateItem;
  end;

implementation

uses
  HCTextItem, HCRectItem, HCTableItem, HCBitmapItem, HCCheckBoxItem,
  HCTabItem, HCLineItem, HCExpressItem, HCPageBreakItem;

{ THCCustomRichData }

constructor THCCustomRichData.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  Items.OnItemInsert := DoItemInsert;
  AddEmptyTextItem;  // 插入空TextItem
  SelectInfo.StartItemNo := 0;
  SelectInfo.StartItemOffset := 0;
  FormatData(0, 0);
  Initialize;
  FReadOnly := False;
end;

function THCCustomRichData.CreateItemByStyle(
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := nil;
  if AStyleNo < THCStyle.RsNull then
  begin
    case AStyleNo of
      THCStyle.RsBitmap: Result := THCBitmapItem.Create(0, 0);
      THCStyle.RsTable: Result := THCTableItem.Create(Style, 1, 1, 1, Self);
      THCStyle.RsTab: Result := TTabItem.Create(0, 0);
      THCStyle.RsLine: Result := TLineItem.Create(1, 1);
      THCStyle.RsExpress: Result := TExperssItem.Create('', '', '', '');
      THCStyle.RsControl: Result := TCheckBoxItem.Create(-1, '', False);
      THCStyle.RsPageBreak: Result := TPageBreakItem.Create(0, 1);
      THCStyle.RsDomain: Result := CreateDefaultDomainItem;
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
  vItemNo, vItemOffset, vDrawItemNo, vX, vY: Integer;
  vRestrain: Boolean;
begin
  GetItemAt(X, Y, vItemNo, vItemOffset, vDrawItemNo, vRestrain);
  if vItemNo < 0 then Exit;
  CoordToItemOffset(X, Y, vItemNo, vItemOffset, vX, vY);
  if Items[vItemNo].StyleNo < THCStyle.RsNull then
    Items[vItemNo].DblClick(vX, vY)
  else
  begin
    Self.SelectInfo.StartItemNo := vItemNo;
    Self.SelectInfo.StartItemOffset := 0;
    if Items[vItemNo].Length > 0 then
    begin
      Self.SelectInfo.EndItemNo := vItemNo;
      Self.SelectInfo.EndItemOffset := Items[vItemNo].Length;
    end;
  end;
  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
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
      Items.Delete(SelectInfo.StartItemNo);
      Inc(vDelCount);
      if (SelectInfo.StartItemNo > vFormatFirstItemNo)
        and (SelectInfo.StartItemNo < vFormatLastItemNo)
      then  // 全选中的Item在起始格式化和结束格式化中间
      begin
        vLen := Items[SelectInfo.StartItemNo - 1].Length;
        if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then
        begin
          Items.Delete(SelectInfo.StartItemNo);
          Inc(vDelCount);
          SelectInfo.StartItemOffset := vLen;
        end;
      end
      else
      if SelectInfo.StartItemNo = vParaFirstItemNo then  // 段第一个ItemNo
      begin
        if vParaFirstItemNo = vParaLastItemNo then  // 段就一个Item全删除了
        begin
          vNewItem := CreateDefaultTextItem;
          vNewItem.ParaFirst := True;
          Items.Insert(SelectInfo.StartItemNo, vNewItem);
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
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
      end;
    end;
    Result := True;
  end;
  {$ENDREGION}

var
  i: Integer;
  vText: string;
  vSelStartComplate,  // 选中范围内的起始Item全选中了
  vSelEndComplate,    // 选中范围内的结束Item全选中了
  vSelStartParaFirst  // 选中起始是段首
    : Boolean;
begin
  Result := False;

  if not CanEdit then Exit;

  if SelectExists then
  begin
    vDelCount := 0;
    if (SelectInfo.EndItemNo < 0) and (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull)
    then  // 选择发生在在RectItem内部
    begin
      // 如果变动会引起RectItem的宽度变化，则需要格式化到段最后一个Item
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

      if Items[SelectInfo.StartItemNo].IsSelectComplate then  // 全选了
      begin
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
        GetParaItemRang(SelectInfo.StartItemNo, vParaFirstItemNo, vParaLastItemNo);

        GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

        if vEndItem.StyleNo < THCStyle.RsNull then  // 同一个RectItem  表格从前选中到一部分？
          (vEndItem as THCCustomRectItem).DeleteSelected
        else  // 同一个TextItem
        begin
          if vEndItem.IsSelectComplate then  // 该TextItem全选中了
            Result := DeleteItemSelectComplate
          else
          begin
            vText := vEndItem.Text;
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

        if vEndItem.StyleNo < THCStyle.RsNull then  // RectItem
        begin
          if vSelEndComplate then  // 最后面  SelectInfo.EndItemOffset = OffsetAfter
          begin
            if CanDeleteItem(SelectInfo.EndItemNo) then  // 允许删除
            begin
              Items.Delete(SelectInfo.EndItemNo);
              Inc(vDelCount);
            end;
          end;
        end
        else  // TextItem
        begin
          if vSelEndComplate then  // 在选中文本Item结束最后 SelectInfo.EndItemOffset = vEndItem.Length
          begin
            if CanDeleteItem(SelectInfo.EndItemNo) then  // 允许删除
            begin
              Items.Delete(SelectInfo.EndItemNo);
              Inc(vDelCount);
            end;
          end
          else  // 文本且不在选中结束Item最后
          begin
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
            Items.Delete(i);
            Inc(vDelCount);
          end;
        end;

        //vStartItemBefor := False;
        vStartItem := Items[SelectInfo.StartItemNo];  // 选中起始Item
        if vStartItem.StyleNo < THCStyle.RsNull then  // 起始是RectItem
        begin
          if SelectInfo.StartItemOffset < OffsetAfter then  // 在其前或其上
          begin
            //vStartItemBefor := True;  // 选择从起始Item最前面开始
            if CanDeleteItem(SelectInfo.StartItemNo) then  // 允许删除
            begin
              Items.Delete(SelectInfo.StartItemNo);
              Inc(vDelCount);
            end;
            if SelectInfo.StartItemNo > vFormatFirstItemNo then
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          end;
        end
        else  // 选中起始是TextItem
        begin
          if vSelStartComplate then  // 在最前起始全选了 SelectInfo.StartItemOffset = 0
          begin
            //vStartItemBefor := True;  // 选择从起始Item最前面开始
            if CanDeleteItem(SelectInfo.StartItemNo) then  // 允许删除
            begin
              Items.Delete(SelectInfo.StartItemNo);
              Inc(vDelCount);
            end;
          end
          else
          //if SelectInfo.StartItemOffset < vStartItem.Length then  // 在中间(不用判断了吧？)
          begin
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
              Dec(vDelCount);
            end
            else  // 选中结束不在段最后
              Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst := True;  // 选中结束位置后面的成为段首
          end
          else
          if SelectInfo.EndItemNo = vFormatLastItemNo then  // 结束在段最后
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull then
              SelectInfo.StartItemOffset := OffsetAfter
            else
              SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
          end
          else  // 选中起始在起始段中间，选中结束在结束段中间
          begin
            vLen := Items[SelectInfo.StartItemNo - 1].Length;
            if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.EndItemNo - vDelCount + 1]) then  // 起始前面和结束后面可合并
            begin
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              SelectInfo.StartItemOffset := vLen;

              Items.Delete(SelectInfo.EndItemNo - vDelCount + 1);
              Inc(vDelCount);
            end
            else  // 起始前面和结束后面不能合并
              Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst := False;  // 合并不成功就挨着
          end;
        end
        else  // 选中范围内的Item没有删除完
        begin
          if vSelStartComplate then  // 起始删除完了
            Items[SelectInfo.EndItemNo - vDelCount].ParaFirst := vSelStartParaFirst
          else
          if not vSelEndComplate then  // 起始和结束都没有删除完
          begin
            if MergeItemText(Items[SelectInfo.StartItemNo], Items[SelectInfo.EndItemNo - vDelCount])
            then  // 选中起始、结束位置的Item合并成功
            begin
              Items.Delete(SelectInfo.EndItemNo - vDelCount);
              Inc(vDelCount);
            end
            else  // 选中起始、结束位置的Item不能合并
            begin
              if SelectInfo.EndItemNo <> vFormatLastItemNo then  // 选中结束不是段最后一个
                Items[SelectInfo.EndItemNo - vDelCount].ParaFirst := False;  // 合并不成功就挨着
            end;
          end;
        end;

        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
      end;

      SelectInfo.EndItemNo := -1;
      SelectInfo.EndItemOffset := -1;
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;

      inherited DeleteSelected;

      ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      Result := True;
    end;
  end;
end;

procedure THCCustomRichData.DisActive;
var
  vItem: THCCustomItem;
begin
  Self.Initialize;

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
    FMouseLBDowning := False;
    FSelecting := False;  // 准备划选  
    
    // Self.Initialize;  这里会导致Mouse事件中的FMouseLBDowning等属性被取消掉
    Style.UpdateInfoReCaret;
    Style.UpdateInfoRePaint;
  end;
end;

procedure THCCustomRichData.DoDrawItemPaintAfter(const AData: THCCustomData;
  const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited;
  if Assigned(FOnItemPaintAfter) then
  begin
    FOnItemPaintAfter(AData, ADrawItemIndex, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCCustomRichData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited;
  if Assigned(FOnItemPaintBefor) then
  begin
    FOnItemPaintBefor(AData, ADrawItemIndex, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
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

procedure THCCustomRichData.ReFormat(const AStartItemNo: Integer);
begin
  FormatItemPrepare(AStartItemNo, Items.Count - 1);
  FormatData(AStartItemNo, Items.Count - 1);
  DrawItems.DeleteFormatMark;
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
begin
  Initialize;
  inherited Clear;
end;

function THCCustomRichData.GetActiveDrawItem: THCCustomDrawItem;
var
  vItem: THCCustomItem;
begin
  Result := nil;
  vItem := GetCurItem;
  if vItem.StyleNo < THCStyle.RsNull then
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
    if vItem.StyleNo < THCStyle.RsNull then
      vPt := (vItem as THCCustomRectItem).GetActiveDrawItemCoord;

    Result.X := Result.X + vPt.X;
    Result.Y := Result.Y + vPt.Y;
  end;
end;

function THCCustomRichData.GetActiveItem: THCCustomItem;
begin
  Result := GetCurItem;
  if (Result <> nil) and (Result.StyleNo < THCStyle.RsNull) then
    Result := (Result as THCCustomRectItem).GetActiveItem;
end;

procedure THCCustomRichData.GetCurStyle(var AStyleNo, AParaNo: Integer);
var
  vCurItemNo: Integer;
begin
  vCurItemNo := GetCurItemNo;
  if Items[vCurItemNo].StyleNo < THCStyle.RsNull then
    (Items[vCurItemNo] as THCCustomRectItem).GetCurStyle(AStyleNo, AParaNo)
  else
  begin
    AStyleNo := Items[vCurItemNo].StyleNo;
    AParaNo := Items[vCurItemNo].ParaNo;
  end;
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
  if (AItemNo > 0) and (AItemOffset = 0) and DrawItems[Items[AItemNo].FirstDItemNo].LineFirst then  // 在开头
    AFirstItemNo := GetLineFirstItemNo(AItemNo - 1, Items[AItemNo - 1].Length)
  else
    AFirstItemNo := GetLineFirstItemNo(AItemNo, 0);  // 取行第一个DrawItem对应的ItemNo

  ALastItemNo := GetParaLastItemNo(AItemNo);
  {

  GetParaItemRang(AItemNo, AFirstItemNo, ALastItemNo);
  vParaFirstDItemNo := Items[AFirstItemNo].FirstDItemNo;
  // 找行首DrawItem
  vDrawItemNo := GetDrawItemNoByOffset(AItemNo, AItemOffset);
  while vDrawItemNo > 0 do
  begin
    if DrawItems[vDrawItemNo].LineFirst then
      Break
    else
      Dec(vDrawItemNo);
  end;
  // 找上一行起始DrawItemNo
  Dec(vDrawItemNo);
  while vDrawItemNo > vParaFirstDItemNo do
  begin
    if DrawItems[vDrawItemNo].LineFirst then
    begin
      AFirstItemNo := DrawItems[vDrawItemNo].ItemNo;
      Break;
    end
    else
      Dec(vDrawItemNo);
  end;  }
end;

function THCCustomRichData.GetTopLevelData: THCCustomRichData;
begin
  Result := nil;
  if (SelectInfo.StartItemNo >= 0) and (SelectInfo.EndItemNo < 0) then
  begin
    if (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull)
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
    if Items[vItemNo].StyleNo < THCStyle.RsNull then
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

{function THCCustomRichData.GetItemNearDrawItemNo(const AItemNo: Integer): Integer;
var
  vItemNo, vLastDrawItemNo: Integer;
begin
  Result := -1;
  vItemNo := AItemNo - 1;
  while vItemNo >= 0 do
  begin
    Result := GetItemLastDrawItemNo(vItemNo);
    if Result >= 0 then
      Break;
    Dec(vItemNo);
  end;
end;}

function THCCustomRichData.GetWidth: Cardinal;
begin
  Result := FWidth;
end;

function THCCustomRichData.ActiveTableDeleteCol(const AColCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).DeleteCol(AColCount);
    end);
end;

function THCCustomRichData.ActiveTableDeleteRow(const ARowCount: Byte): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := TableInsertRC(function(const AItem: THCCustomItem): Boolean
    begin
      Result := (AItem as THCTableItem).DeleteRow(ARowCount);
    end);
end;

procedure THCCustomRichData.AddEmptyTextItem;
var
  vItem: THCCustomItem;
begin
  if Self.Items.Count = 0 then
  begin
    vItem := CreateDefaultTextItem;
    vItem.ParaFirst := True;
    Items.Add(vItem);  // 不使用InsertText，为避免其触发ReFormatPara时因为没有格式化过，获取不到对应的DrawItem
  end;
end;

function THCCustomRichData.ApplySelectParaStyle(
  const AMatchStyle: TParaMatch): Integer;
var
  vFirstNo, vLastNo: Integer;

  procedure DoApplyParaStyle(const AItemNo: Integer);
  var
    i, vParaNo: Integer;
  begin
    if GetItemStyle(AItemNo) < THCStyle.RsNull then  // 当前是RectItem
      (Items[AItemNo] as THCCustomRectItem).ApplySelectParaStyle(Style, AMatchStyle)
    else
    begin
      GetParaItemRang(AItemNo, vFirstNo, vLastNo);
      vParaNo := AMatchStyle.GetMatchParaNo(Style, GetItemParaStyle(AItemNo));
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

  GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
  if SelectInfo.EndItemNo >= 0 then  // 有选中内容
  begin
    vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    ApplyParaSelectedRangStyle;
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
  end
  else  // 没有选中内容
  begin
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    DoApplyParaStyle(SelectInfo.StartItemNo);  // 应用样式
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
  end;
  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
end;

function THCCustomRichData.ApplySelectTextStyle(
  const AMatchStyle: TStyleMatch): Integer;

  // 当前Item成功合并到同段前一个Item
  function MergeItemToPrio(const AItemNo: Integer): Boolean;
  begin
    Result := (AItemNo > 0)
              and (not Items[AItemNo].ParaFirst)
              and MergeItemText(Items[AItemNo - 1], Items[AItemNo]);
  end;

  // 同段后一个Item成功合并到当前Item
  function MergeItemToNext(const AItemNo: Integer): Boolean;
  begin
    Result := (AItemNo < Items.Count - 1)
              and (not Items[AItemNo + 1].ParaFirst)
              and MergeItemText(Items[AItemNo], Items[AItemNo + 1]);
  end;

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
    if vItem.StyleNo < THCStyle.RsNull then  // 非文本
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
            if (AItemNo < Items.Count - 1) and MergeItemText(vSelItem, Items[AItemNo + 1]) then
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
    if vItem.StyleNo < THCStyle.RsNull then  // 非文本
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
    if vItem.StyleNo < THCStyle.RsNull then  // 非文本
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
    if vItem.StyleNo < THCStyle.RsNull then  // 非文本
      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle)
    else  // 文本
      vItem.StyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);
  end;
  {$ENDREGION}

var
  i, vFormatFirstItemNo, vFormatLastItemNo, vMStart, vMEnd: Integer;
begin
  Self.Initialize;
  vExtraCount := 0;

  GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
  if not SelectExists then  // 没有选中
  begin
    if Items[SelectInfo.StartItemNo].Length = 0 then  // 空行，改变当前光标处样式
    begin
      Items[SelectInfo.StartItemNo].StyleNo := AMatchStyle.GetMatchStyleNo(Style, Items[SelectInfo.StartItemNo].StyleNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
    end;

    Exit;
  end;

  if SelectInfo.EndItemNo < 0 then  // 没有连续选中内容
  begin
    if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull then
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
      if Items[i].StyleNo > THCStyle.RsNull then
      begin
        AMatchStyle.Append := not AMatchStyle.StyleHasMatch(Style, Items[i].StyleNo);  // 根据第一个判断是添加样式还是减掉样式
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

procedure THCCustomRichData.Initialize;
begin
  FMouseLBDowning := False;
  FMouseDownItemNo := -1;
  FMouseDownItemOffset := -1;
  FMouseMoveItemNo := -1;
  FMouseMoveItemOffset := -1;
  FMouseMoveRestrain := False;
  CaretDrawItemNo := -1;
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

  Result := True;
end;

function THCCustomRichData.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem): Boolean;
var
  vFormatFirstItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;

  if not CanEdit then Exit;

  //-------- 在指定的Index处插入Item --------//

  //DeleteSelection;  // 如果正在选中区域中那么有歧义所以暂时不删除选中
  AItem.ParaNo := Style.CurParaNo;
  if AItem is THCTextItem then
    AItem.StyleNo := Style.CurStyleNo;

  if IsEmpty then
  begin
    FormatItemPrepare(0);
    Items.Clear;
    AItem.ParaFirst := True;
    Items.Insert(0, AItem);
    ReFormatData_(0, 0, 1);
    ReSetSelectAndCaret(0);
    Result := True;
    Exit;
  end;

  {说明：当插入位置不是最后一个且插入位置是段起始，那么可能是在上一段最后插入，
   也可能是要在下一段最前页插入，这时以AItem的ParaFirst属性为判断依据}

  if AItem.StyleNo < THCStyle.RsNull then  // 插入RectItem
  begin
    if AIndex < Items.Count then  // 不是在末尾添加一个Item
    begin
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex, 0);
      if AItem.ParaFirst then  // 另起一段
      begin
        if Items[AIndex].ParaFirst then  // 下一段开始变为非开始，如果要独立为一段去掉此判断即可
          Items[AIndex].ParaFirst := False;
      end;
    end
    else  // 在末尾添加一个Item
    begin
      vFormatFirstItemNo := AIndex - 1;
      vFormatLastItemNo := AIndex - 1;
    end;

    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    Items.Insert(AIndex, AItem);
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);
    ReSetSelectAndCaret(AIndex);
  end
  else  // 插入文本Item
  begin
    if AIndex < Items.Count then
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex, 0)
    else
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, AIndex - 1, 0);

    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

    if (AIndex < Items.Count) and (not AItem.ParaFirst) and (Items[AIndex].CanConcatItems(AItem)) then  // 和当前位置处能合并
    begin
      Items[AIndex].Text := AItem.Text + Items[AIndex].Text;
      if AItem.ParaFirst then  // 考虑到原位置是段首，新插入不是段首，所以不能直接 Items[AIndex].ParaFirst := AItem.ParaFirst
        Items[AIndex].ParaFirst := True;

      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
      ReSetSelectAndCaret(AIndex, AItem.Length);
    end
    else
    if (AIndex > 0) and (not AItem.ParaFirst) and (Items[AIndex - 1].CanConcatItems(AItem)) then  // 和前一个能合并
    begin
      Items[AIndex - 1].Text := Items[AIndex - 1].Text + AItem.Text;
      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
      ReSetSelectAndCaret(AIndex - 1);
    end
    else  // 不能和插入位置及插入位置前的Item合并
    begin
      if AItem.ParaFirst then
        Items[AIndex].ParaFirst := False;

      if Items[AIndex].Text <> '' then  // 插入位置处不是空行
      begin
        Items.Insert(AIndex, AItem);
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);
      end
      else
      begin
        AItem.ParaFirst := Items[AIndex].ParaFirst;

        Items.Delete(AIndex);
        Items.Insert(AIndex, AItem);
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
      end;

      ReSetSelectAndCaret(AIndex);
    end;
  end;

  Result := True;
end;

function THCCustomRichData.InsertLine(const ALineHeight: Integer): Boolean;
var
  vItem: TLineItem;
begin
  Result := False;

  if not CanEdit then Exit;

  vItem := TLineItem.Create(Self.Width, 21);
  vItem.LineHeght := ALineHeight;
  Result := InsertItem(vItem);
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
      Style.UpdateInfoReCaret;
      Style.UpdateInfoRePaint;
    end;
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

function THCCustomRichData.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
var
  vInsPos, vFormatFirstItemNo, vFormatLastItemNo: Integer;
  vEmpty: Boolean;
  vItem, vAfterItem: THCCustomItem;
  i, vItemCount, vStyleNo: Integer;
  vDataSize: Int64;
begin
  Result := False;

  if not CanEdit then Exit;

  vAfterItem := nil;
  vEmpty := IsEmpty;

  if vEmpty then  // 空
  begin
    Clear;
    vInsPos := 0;
  end
  else  // 有数据
  begin
    DeleteSelected;
    // 确定插入位置
    vInsPos := SelectInfo.StartItemNo;
    if Items[vInsPos].StyleNo < THCStyle.RsNull then  // RectItem
    begin
      if SelectInfo.StartItemOffset = OffsetInner then  // 其上
      begin
        GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, SelectInfo.StartItemNo, OffsetInner);
        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
        Result := (Items[vInsPos] as THCCustomRectItem).InsertStream(AStream, AStyle, AFileVersion);
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);

        Exit;
      end
      else
      if SelectInfo.StartItemOffset = OffsetBefor then  // 其前
        //vIns := vCurItemNo
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
        //vIns := vCurItemNo
      else  // 其中
      begin
        vAfterItem := Items[vInsPos].BreakByOffset(SelectInfo.StartItemOffset);  // 后半部分对应的Item
        vInsPos := vInsPos + 1;
      end;
    end;
  end;

  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));
  AStream.ReadBuffer(vItemCount, SizeOf(vItemCount));
  if vItemCount = 0 then Exit;

  // 因为插入的第一个可能和插入位置前一个合并，插入位置可能是行首，所以要从插入位置
  // 行上一个开始格式化，为简单处理，直接使用行首尾
  GetParaItemRang(SelectInfo.StartItemNo, vFormatFirstItemNo, vFormatLastItemNo);

  // 计算格式化起始、结束ItemNo
  if Items.Count > 0 then  // 兼容Empty
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo)
  else
  begin
    vFormatFirstItemNo := 0;
    vFormatLastItemNo := -1;
  end;

  for i := 0 to vItemCount - 1 do
  begin
    AStream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));
    vItem := CreateItemByStyle(vStyleNo);
    vItem.LoadFromStream(AStream, AStyle, AFileVersion);
    if AStyle <> nil then  // 有样式表
    begin
      if vItem.StyleNo > THCStyle.RsNull then
        vItem.StyleNo := Style.GetStyleNo(AStyle.TextStyles[vItem.StyleNo], True);
      vItem.ParaNo := Style.GetParaNo(AStyle.ParaStyles[vItem.ParaNo], True);
    end
    else  // 无样式表
    begin
      if vItem.StyleNo > THCStyle.RsNull then
        vItem.StyleNo := Style.CurStyleNo;
      vItem.ParaNo := Style.CurParaNo;
    end;

    if (i = 0) and (vInsPos > 0) then  // 第一个不是在开始插入(粘贴)
      vItem.ParaFirst := False;

    Items.Insert(vInsPos + i, vItem);
  end;

  if vAfterItem <> nil then  // 插入操作是在Item中间，原Item补拆分成2个
  begin
    if MergeItemText(Items[vInsPos + vItemCount - 1], vAfterItem) then
      FreeAndNil(vAfterItem)
    else
    begin
      Items.Insert(vInsPos + vItemCount, vAfterItem);
      Inc(vItemCount);
    end;
  end;

  if (vInsPos > vFormatFirstItemNo) and (vInsPos > 0) then
  begin
    if Items[vInsPos - 1].Length = 0 then  // 插入位置前面是空行Item
    begin
      Items[vInsPos].ParaFirst := Items[vInsPos - 1].ParaFirst;
      Items.Delete(vInsPos - 1);
      Dec(vItemCount);
    end
    else
    if MergeItemText(Items[vInsPos - 1], Items[vInsPos]) then  // 插入的和前面的合并
    begin
      Items.Delete(vInsPos);
      Dec(vItemCount);
    end;
  end;

  ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + vItemCount, vItemCount);

  ReSetSelectAndCaret(vInsPos + vItemCount - 1);  // 选中插入内容最后Item位置

  Style.UpdateInfoReCaret;
  Style.UpdateInfoRePaint;
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
  if AItem is THCTextItem then
    AItem.StyleNo := Style.CurStyleNo;

  if IsEmpty then
  begin
    FormatItemPrepare(0);
    Items.Clear;
    AItem.ParaFirst := True;
    if AItem.StyleNo > THCStyle.RsNull then
      AItem.StyleNo := Style.CurStyleNo
    else
    if AItem is THCTextRectItem then
      (AItem as THCTextRectItem).TextStyleNo := Style.CurStyleNo;

    Items.Insert(0, AItem);
    ReFormatData_(0, 0, 1);
    ReSetSelectAndCaret(0);
    Result := True;
    Exit;
  end;
  vCurItemNo := GetCurItemNo;

  if Items[vCurItemNo].StyleNo < THCStyle.RsNull then  // 当前位置是 RectItem
  begin
    if SelectInfo.StartItemOffset = OffsetInner then  // 正在其上
    begin
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
      Result := (Items[vCurItemNo] as THCCustomRectItem).InsertItem(AItem);
      if Result then
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 0);
    end
    else  // 其前or其后
    begin
      if SelectInfo.StartItemOffset = OffsetBefor then  // 其前
        Result := InsertItem(SelectInfo.StartItemNo, AItem)
      else
        Result := InsertItem(SelectInfo.StartItemNo + 1, AItem);
    end;
  end
  else  // 当前位置是TextItem
  begin
    if SelectInfo.StartItemOffset = 0 then  // 在最前面插入
      Result := InsertItem(SelectInfo.StartItemNo, AItem)
    else
    if (SelectInfo.StartItemOffset = Items[vCurItemNo].Length) then  // 在TextItem最后插入
      Result := InsertItem(SelectInfo.StartItemNo + 1, AItem)
    else  // 在Item中间
    begin
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

      vText := Items[vCurItemNo].Text;
      vsBefor := Copy(vText, 1, SelectInfo.StartItemOffset);  // 前半部分文本
      vsAfter := Copy(vText, SelectInfo.StartItemOffset + 1, Items[vCurItemNo].Length
        - SelectInfo.StartItemOffset);  // 后半部分文本

      if Items[vCurItemNo].CanConcatItems(AItem) then  // 能合并
      begin
        if AItem.ParaFirst then  // 新段
        begin
          Items[vCurItemNo].Text := vsBefor;
          AItem.Text := AItem.Text + vsAfter;
          vCurItemNo := vCurItemNo + 1;
          Items.Insert(vCurItemNo, AItem);
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo, 1);
          ReSetSelectAndCaret(vCurItemNo);
        end
        else  // 同一段中插入
        begin
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
        vAfterItem := Items[vCurItemNo].BreakByOffset(SelectInfo.StartItemOffset);  // 后半部分对应的Item

        // 插入后半部分对应的Item
        vCurItemNo := vCurItemNo + 1;
        Items.Insert(vCurItemNo, vAfterItem);
        // 插入新Item
        Items.Insert(vCurItemNo, AItem);
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
begin
  Result := False;

  if not CanEdit then Exit;

  vItem := THCTableItem.Create(Style, ARowCount, AColCount, Self.Width, Self);
  Result := InsertItem(vItem);
end;

function THCCustomRichData.InsertText(const AText: string): Boolean;
var
  vNewPara: Boolean;

  function InsertTextItem(const AText: string): Boolean;
  var
    vItem: THCCustomItem;
  begin
    vItem := CreateDefaultTextItem;
    vItem.Text := AText;
    vItem.ParaFirst := vNewPara;
    Result := InsertItem(vItem);
    vNewPara := True;
  end;

var
  vPCharStart, vPCharEnd, vPtr: PChar;
  vS: string;
begin
  Result := False;

  if not CanEdit then Exit;

  vNewPara := False;
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
          if not InsertTextItem(vS) then Exit;
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
  Result := InsertTextItem(vS)
end;

procedure THCCustomRichData.KeyDown(var Key: Word; Shift: TShiftState);
var
  vCurItem: THCCustomItem;
  vParaFirstItemNo, vParaLastItemNo: Integer;
  vFormatFirstItemNo, vFormatLastItemNo: Integer;
  vSelectExist: Boolean;

  {$REGION 'TABKeyDown 按键'}
  procedure TABKeyDown;

    function CreateTabItem: TTabItem;
    var
      vSize: TSize;
    begin
      Style.TextStyles[Style.CurStyleNo].ApplyStyle(Style.DefCanvas);
      vSize := Style.DefCanvas.TextExtent('汉字');
      Result := TTabItem.Create(vSize.cx, vSize.cy);
      Result.ParaNo := Style.CurParaNo;
    end;

  var
    vItem: THCCustomItem;
  begin
    vItem := CreateTabItem;
    if vCurItem.StyleNo < THCStyle.RsNull then  // 当前是RectItem
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

    Self.InsertItem(vItem);
  end;
  {$ENDREGION}

  {$REGION 'LeftKeyDown 左方向键，此处不会涉及表格，表格在RectItemKeyDown中处理了'}
  procedure LeftKeyDown;
  var
    vNewCaretDrawItemNo: Integer;
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
          if GetItemStyle(SelectInfo.StartItemNo) < THCStyle.RsNull then
            SelectInfo.StartItemOffset := OffsetAfter
          else
            SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;

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
  {$ENDREGION}

  {$REGION 'RightKeyDown 右方向键，此处不会涉及表格，表格在RectItemKeyDown中处理了'}
  procedure RightKeyDown;
  var
    vNewCaretDrawItemNo: Integer;
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
  {$ENDREGION}

  {$REGION 'RectItemKeyDown Rect类型Item的KeyDown事件'}
  procedure RectItemKeyDown;
  var
    vLineFirst: Boolean;
    vItem: THCCustomItem;
    vLen: Integer;
    vRectItem: THCCustomRectItem;
  begin
    vRectItem := vCurItem as THCCustomRectItem;
    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    if SelectInfo.StartItemOffset = OffsetInner then  // 在其上
    begin
      if vRectItem.WantKeyDown(Key, Shift) then
      begin
        vRectItem.KeyDown(Key, Shift);
        if vRectItem.HeightChanged then
        begin
          vRectItem.HeightChanged := False;
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
            if vRectItem.WantKeyDown(Key, Shift) then
              SelectInfo.StartItemOffset := OffsetInner
            else
              SelectInfo.StartItemOffset := OffsetAfter;

            CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
          end;

        VK_RETURN:
          begin
            vLineFirst := DrawItems[vCurItem.FirstDItemNo].LineFirst;
            if vLineFirst then  // RectItem在行首，插入空行
            begin
              vCurItem := CreateDefaultTextItem;
              vCurItem.ParaFirst := True;
              Items.Insert(SelectInfo.StartItemNo, vCurItem);
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);
              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            end
            else  // RectItem不在行首
            begin
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
              vCurItem.ParaFirst := True;
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
            end;
          end;

        VK_BACK:  // 在RectItem前
          begin
            if vCurItem.ParaFirst then  // 是段首
            begin
              vCurItem.ParaFirst := False;
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
            end
            else  // 不是段首
            begin
              // 选到上一个最后
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull then
                SelectInfo.StartItemOffset := OffsetAfter
              else
                SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;

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

            if vCurItem.ParaFirst then  // 是段首
            begin
              if SelectInfo.StartItemNo <> vFormatLastItemNo then  // 段不是只有一个
              begin
                Items[SelectInfo.StartItemNo + 1].ParaFirst := True;
                Items.Delete(SelectInfo.StartItemNo);
                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
              end
              else  // 段删除空了
              begin
                Items.Delete(SelectInfo.StartItemNo);
                vCurItem := CreateDefaultTextItem;
                vCurItem.ParaFirst := True;
                Items.Insert(SelectInfo.StartItemNo, vCurItem);
                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
              end;
            end
            else  // 不是段首
            begin
              if SelectInfo.StartItemNo < vFormatLastItemNo then  // 段中间
              begin
                if Items[SelectInfo.StartItemNo - 1].StyleNo < THCStyle.RsNull then  // 前一个是RectItem
                  vLen := OffsetAfter
                else  // 前一个是TextItem
                  vLen := Items[SelectInfo.StartItemNo - 1].Length;

                // 如果RectItem前面(同一行)有高度小于此RectItme的Item(如Tab)，
                // 其格式化时以RectItem为高，重新格式化时如果从RectItem所在位置起始格式化，
                // 行高度仍会以Tab为行高，也就是RectItem高度，所以需要从行开始格式化
                Items.Delete(SelectInfo.StartItemNo);
                if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then  // 原RectItem前后能合并
                begin
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
                Items.Delete(SelectInfo.StartItemNo);
                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

                SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
                if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull then
                  SelectInfo.StartItemOffset := OffsetAfter
                else
                  SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
              end;
            end;
          end;

        VK_TAB:
          TABKeyDown;
      end;
    end
    else
    if SelectInfo.StartItemOffset = OffsetAfter then  // 在其后，后面补充一个空Item
    begin
      case Key of
        VK_BACK:
          begin
            if not CanDeleteItem(SelectInfo.StartItemNo) then  // 不可删除
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              Exit;
            end;

            if vCurItem.ParaFirst then  // 是段首
            begin
              if (SelectInfo.StartItemNo > 0) and (SelectInfo.StartItemNo < Items.Count - 1) and (not Items[SelectInfo.StartItemNo + 1].ParaFirst) then  // 同一段还有内容
              begin
                Items.Delete(SelectInfo.StartItemNo);
                Items[SelectInfo.StartItemNo].ParaFirst := True;
                ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
              end
              else  // 空段了
              begin
                Items.Delete(SelectInfo.StartItemNo);
                vItem := CreateDefaultTextItem;
                vItem.ParaFirst := True;
                Items.Insert(SelectInfo.StartItemNo, vItem);
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
              KeyDown(Key, Shift);
              Exit;
            end;
          end;

        VK_LEFT:
          begin
            if vRectItem.WantKeyDown(Key, Shift) then
              SelectInfo.StartItemOffset := OffsetInner
            else
              SelectInfo.StartItemOffset := OffsetBefor;

            CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
          end;

        VK_RIGHT:
          RightKeyDown;

        VK_RETURN:
          begin
            vCurItem := CreateDefaultTextItem;
            vCurItem.ParaFirst := True;
            Items.Insert(SelectInfo.StartItemNo + 1, vCurItem);
            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo + 1, 1);
            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            SelectInfo.StartItemOffset := vCurItem.Length;
            CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
          end;

        VK_TAB:
          TABKeyDown;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'EnterKeyDown 回车'}
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

  {$REGION 'DeleteKeyDown 向后删除键'}
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
            if Items[vCurItemNo + 1].StyleNo < THCStyle.RsNull then  // 下一个段首是RectItem，不能合并
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
                if (vCurItem.ClassType = Items[vCurItemNo + 1].ClassType)
                  and (vCurItem.StyleNo = Items[vCurItemNo + 1].StyleNo)
                then  // 下一段段首可合并到当前(当前在上一段段尾) 201804111209 (不能用MergeItemText的情况)
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
              if GetItemStyle(SelectInfo.StartItemNo) < THCStyle.RsNull then
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
              //ReFormatData_(SelectInfo.StartItemNo, SelectInfo.StartItemNo, -1);
              DrawItems.DeleteFormatMark;
              if GetItemStyle(SelectInfo.StartItemNo) < THCStyle.RsNull then
                SelectInfo.StartItemOffset := OffsetAfter
              else
                SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
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

  {$REGION 'BackspaceKeyDown 向前删除键，如果在Item最前面面删除，而此Item前面还有Item，则转为从前面Item最后面删除'}
  procedure BackspaceKeyDown;
  var
    vText: string;
    i, vCurItemNo, vDrawItemNo, vLen, vDelCount, vParaNo: Integer;
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

          if (vCurItem.ClassType = Items[SelectInfo.StartItemNo - 1].ClassType)
            and (vCurItem.StyleNo = Items[SelectInfo.StartItemNo - 1].StyleNo)
          then  // 当前可以和上一个合并(当前在段首) 201804111209 (不能用MergeItemText的情况)
          begin
            Items[SelectInfo.StartItemNo - 1].Text := Items[SelectInfo.StartItemNo - 1].Text
              + Items[SelectInfo.StartItemNo].Text;

            vFormatFirstItemNo := GetLineFirstItemNo(SelectInfo.StartItemNo - 1, vLen);
            vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

            Items.Delete(SelectInfo.StartItemNo);

            // 修正下一段合并上来的Item的段样式，对齐样式
            vParaNo := Items[SelectInfo.StartItemNo - 1].ParaNo;
            for i := SelectInfo.StartItemNo to vFormatLastItemNo - 1 do
              Items[i].ParaNo := vParaNo;

            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

            ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, vLen);
          end
          else  // 段起始且不能和上一个合并
          begin
            if vCurItem.Length = 0 then  // 已经没有内容了(不是第1个Item)(空行)
            begin
              FormatItemPrepare(SelectInfo.StartItemNo - 1, SelectInfo.StartItemNo);
              Items.Delete(SelectInfo.StartItemNo);
              ReFormatData_(SelectInfo.StartItemNo - 1, SelectInfo.StartItemNo - 1, -1);

              ReSetSelectAndCaret(SelectInfo.StartItemNo - 1);
            end
            else  // 段前删除且不能和上一段最后合并
            begin
              GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

              vCurItem.ParaFirst := False;  // 当前段和上一段Item拼接成一段

              vParaNo := Items[SelectInfo.StartItemNo - 1].ParaNo;  // 上一段的ParaNo
              for i := SelectInfo.StartItemNo to vFormatLastItemNo do
                Items[i].ParaNo := vParaNo;

              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);

              ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
            end;
          end;
        end
        else  // 在Item开始往前删，但Item不是段起始
        begin
          if Items[SelectInfo.StartItemNo - 1].StyleNo < THCStyle.RsNull then  // 前面是RectItem
          begin
            vCurItemNo := SelectInfo.StartItemNo - 1;
            if CanDeleteItem(vCurItemNo) then  // 能删除
            begin
              vCurItem.ParaFirst := Items[vCurItemNo].ParaFirst;
              GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
              Items.Delete(vCurItemNo);
              vDelCount := 1;
              vLen := 0;
              if not vCurItem.ParaFirst then  // 删除前面的RectItem后不是段首
              begin
                vCurItemNo := vCurItemNo - 1;  // 上一个
                vLen := Items[vCurItemNo].Length;  // 上一个最后面
                if MergeItemText(Items[vCurItemNo], vCurItem) then  // 当前能合并到上一个
                begin
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
            if GetItemStyle(SelectInfo.StartItemNo) < THCStyle.RsNull then
              SelectInfo.StartItemOffset := OffsetAfter
            else
              SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
            vCurItem := GetCurItem;

            BackspaceKeyDown;  // 重新处理
            Exit;
          end;
        end;
      end;
    end
    else  // 光标不在Item最开始  文本
    begin
      vText := vCurItem.Text;
      Delete(vText, SelectInfo.StartItemOffset, 1);
      vCurItem.Text := vText;
      SelectInfo.StartItemOffset := SelectInfo.StartItemOffset - 1;
      if vText = '' then  // 删除后没有内容了
      begin
        vCurItemNo := SelectInfo.StartItemNo;  // 记录原位置
        if not DrawItems[Items[vCurItemNo].FirstDItemNo].LineFirst then  // 当前不是行首，前面有内容
        begin
          vLen := Items[vCurItemNo - 1].Length;

          if (vCurItemNo > 0) and (vCurItemNo < vParaLastItemNo)
            and MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1])
          then  // 选中位置上一个和选中位置下一个可合并
          begin
            GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, vCurItemNo - 1, vLen);
            FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
            Items.Delete(vCurItemNo);  // 删除当前
            Items.Delete(vCurItemNo);  // 删除下一个
            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 2, -2);

            ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, vLen);  // 上一个原光标位置
          end
          else  // 当前不是行首，删除后没有内容了，且不能合并上一个和下一个
          begin
            vLen := 0;
            if SelectInfo.StartItemNo = vFormatLastItemNo then  // 段最后一个
            begin
              vFormatFirstItemNo := GetLineFirstItemNo(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
              Items.Delete(vCurItemNo);
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

              ReSetSelectAndCaret(vCurItemNo - 1);
            end
            else  // 不是段最后一个
            begin
              GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
              FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
              Items.Delete(vCurItemNo);
              ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);

              ReSetSelectAndCaret(vCurItemNo - 1);
            end;
          end;
        end
        else  // 是行第一个、行首Item删除空了，
        begin
          GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
          FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);

          if vCurItemNo < vFormatLastItemNo then  // 同段后面还有
          begin
            if Items[vCurItemNo].ParaFirst then
              Items[vCurItemNo + 1].ParaFirst := True;
            Items.Delete(vCurItemNo);
            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
            ReSetSelectAndCaret(vCurItemNo, 0);  // 下一个最前面
          end
          else  // 同段后面没有Item了
          if Items[vCurItemNo].ParaFirst then  // 当前是段首，其后无内容，段空了
            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo)  // 保留空行
          else  // 后面无内容，但同段前面还有内容
          begin
            Items.Delete(vCurItemNo);
            ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo - 1, -1);
            ReSetSelectAndCaret(vCurItemNo - 1);
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
        ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'HomeKeyDown 按键'}
  procedure HomeKeyDown;
  var
    vFirstDItemNo, vLastDItemNo: Integer;
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
  {$ENDREGION}

  {$REGION 'EndKeyDown 按键'}
  procedure EndKeyDown;
  var
    vFirstDItemNo, vLastDItemNo: Integer;
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
  {$ENDREGION}

  {$REGION 'UpKeyDown 上方向按键'}
  procedure UpKeyDown;
  var
    i, vCurDItemNo, vFirstDItemNo, vLastDItemNo, vX: Integer;
  begin
    if vSelectExist then  // 有选中内容
    begin
      SelectInfo.EndItemNo := -1;
      SelectInfo.EndItemOffset := -1;
    end
    else  // 无选中内容
    begin
      vFirstDItemNo := CaretDrawItemNo;  // GetSelectStartDrawItemNo;
      GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // 当前行起始结束DItemNo
      if vFirstDItemNo > 0 then  // 当前行不是第一行
      begin
        { 获取当前光标X位置 }
        vCurDItemNo := CaretDrawItemNo;  // GetSelectStartDrawItemNo;  // 当前DItem
        vX := DrawItems[vCurDItemNo].Rect.Left +
          GetDrawItemOffsetWidth(vCurDItemNo,
            SelectInfo.StartItemOffset - DrawItems[vCurDItemNo].CharOffs + 1);

        { 获取上一行在X位置对应的DItem和Offset }
        vFirstDItemNo := vFirstDItemNo - 1;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // 上一行起始和结束DItem

        for i := vFirstDItemNo to vLastDItemNo do
        begin
          if DrawItems[i].Rect.Right > vX then
          begin
            SelectInfo.StartItemNo := DrawItems[i].ItemNo;
            SelectInfo.StartItemOffset := DrawItems[i].CharOffs +
              GetDrawItemOffset(i, vX - DrawItems[i].Rect.Left) - 1;
            CaretDrawItemNo := i;

            Exit;  // 有合适，则退出
          end;
        end;

        { 没合适则选择到最后 }
        SelectInfo.StartItemNo := DrawItems[vLastDItemNo].ItemNo;
        SelectInfo.StartItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
        CaretDrawItemNo := vLastDItemNo;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'DownKeyDown 下方向键'}
  procedure DownKeyDown;
  var
    i, vCurDItemNo, vFirstDItemNo, vLastDItemNo, vX: Integer;
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
      vFirstDItemNo := CaretDrawItemNo;  // GetSelectStartDrawItemNo;
      GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // 当前行起始结束DItemNo
      if vLastDItemNo < DrawItems.Count - 1 then  // 当前行不是最后一行
      begin
        { 获取当前光标X位置 }
        vCurDItemNo := CaretDrawItemNo;  // GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset); // 当前DItem

        vX := DrawItems[vCurDItemNo].Rect.Left +
          GetDrawItemOffsetWidth(vCurDItemNo,
            SelectInfo.StartItemOffset - DrawItems[vCurDItemNo].CharOffs + 1);

        { 获取下一行在X位置对应的DItem和Offset }
        vFirstDItemNo := vLastDItemNo + 1;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // 下一行起始和结束DItem

        for i := vFirstDItemNo to vLastDItemNo do
        begin
          if DrawItems[i].Rect.Right > vX then
          begin
            SelectInfo.StartItemNo := DrawItems[i].ItemNo;
            SelectInfo.StartItemOffset := DrawItems[i].CharOffs +
              GetDrawItemOffset(i, vX - DrawItems[i].Rect.Left) - 1;
            CaretDrawItemNo := i;

            Exit;  // 有合适，则退出
          end;
        end;

        { 没合适则选择到最后 }
        SelectInfo.StartItemNo := DrawItems[vLastDItemNo].ItemNo;
        SelectInfo.StartItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
        CaretDrawItemNo := vLastDItemNo;
      end
      else  // 当前行是最后一行
        Key := 0;
    end;
  end;
  {$ENDREGION}

begin
  if not CanEdit then Exit;

  if Key in [VK_BACK, VK_DELETE, VK_RETURN, VK_TAB] then
    Self.Initialize;  // 如果Item删除完了，会对鼠标移动事件有影响，所以初始化

  vCurItem := GetCurItem;
  if vCurItem = nil then Exit;

  vSelectExist := SelectExists;

  if vSelectExist and (Key in [VK_BACK, VK_DELETE, VK_RETURN, VK_TAB]) then
  begin
    if DeleteSelected then
    begin
      if Key in [VK_BACK, VK_DELETE] then Exit;
    end;
  end;

  GetParaItemRang(SelectInfo.StartItemNo, vParaFirstItemNo, vParaLastItemNo);

  if vCurItem.StyleNo < THCStyle.RsNull then
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
      VK_UP:     UpKeyDown;         // 向上翻页键
      VK_DOWN:   DownKeyDown;       // 向下翻页键
      VK_TAB:    TABKeyDown;        // TAB键
    end;
  end;

  case Key of
    VK_BACK, VK_DELETE, VK_RETURN, VK_TAB:
      begin
        Style.UpdateInfoReCaret;
        Style.UpdateInfoRePaint;
      end;

    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END:
      begin
        if vSelectExist then
          Style.UpdateInfoRePaint;
        Style.UpdateInfoReCaret;
      end;
  end;
end;

procedure THCCustomRichData.KeyPress(var Key: Char);
var
  vCurItem, vNewItem: THCCustomItem;
  vFormatFirstItemNo, vFormatLastItemNo: Integer;

  {$REGION 'RectItemKeyPress'}
  procedure RectItemKeyPress;
  var
    vRectItem: THCCustomRectItem;
  begin
    if SelectInfo.StartItemOffset = OffsetInner then  // 在其上输入内容
    begin
      vRectItem := vCurItem as THCCustomRectItem;
      vRectItem.KeyPress(Key);
      if vRectItem.HeightChanged then
      begin
        vRectItem.HeightChanged := False;
        GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
        FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
        if Key <> #0 then
          //ReFormatPara(SelectInfo.StartItemNo);
          ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
      end;
    end
    else  // 其后或其前
    begin
      vNewItem := CreateDefaultTextItem;
      vNewItem.Text := Key;
      if SelectInfo.StartItemOffset = OffsetAfter then  // 在其后输入内容
      begin
        { TODO : 这里需要处理其后有文本的合并问题吗？ }
        vNewItem.ParaFirst := False;  // 如果非占整行的Item，此处不应该为True
        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
        InsertItem(SelectInfo.StartItemNo, vNewItem);
      end
      else  // 在其前输入内容，补充TextItem
      begin
        if vCurItem.ParaFirst then
        begin
          vNewItem.ParaFirst := True;
          vCurItem.ParaFirst := False;
        end;
        InsertItem(SelectInfo.StartItemNo, vNewItem);
      end;
    end;
  end;
  {$ENDREGION}

var
  vText: string;
begin
  if not CanEdit then Exit;

  DeleteSelected;

  vCurItem := GetCurItem;
  if vCurItem.StyleNo < THCStyle.RsNull then
    RectItemKeyPress
  else
  begin
    vText := vCurItem.Text;
    // 行起始为TextItem，同一行后面有RectItem时，编辑TextItem后格式化可能会将RectItem分到下一行，
    // 所以不能直接 FormatItemPrepare(SelectInfo.StartItemNo)否则会因为格式化范围太小，
    // 没有进行FiniLine调整行高，所以从段最后或行最后开始
    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    Insert(Key, vText, SelectInfo.StartItemOffset + 1);
    vCurItem.Text := vText;
    SelectInfo.StartItemOffset := SelectInfo.StartItemOffset + 1;
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
    Self.Initialize;
  end;

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
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

  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  InsertStream(AStream, AStyle, AFileVersion);
  // 加载完成后，初始化
  //DisSelect; 解决第一个是表格且跨页打开后自动滚动的问题
  SelectInfo.StartItemNo := 0;
  SelectInfo.StartItemOffset := 0;
  Self.Initialize;
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
  if Items[vItemNo].StyleNo = THCStyle.RsTable then
  begin
    Result := (Items[vItemNo] as THCTableItem).MergeSelectCells;
    if Result then  // 合并成功
    begin
      GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
      FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
      ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
      DisSelect;
      //ReFormatData_(vItemNo);
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
  //Style.UpdateInfo.Draging := False;

  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);

  FMouseDownX := X;
  FMouseDownY := Y;

  GetItemAt(X, Y, vMouseDownItemNo, vMouseDownItemOffset, vDrawItemNo, vRestrain);

  vMouseDownInSelect := (not vRestrain) and CoordInSelect(X, Y, vMouseDownItemNo, vMouseDownItemOffset);

  if vMouseDownInSelect then   // 在选中中按下
  begin
    if FMouseLBDowning then  // 是左键
    begin
      FDraging := True;
      Style.UpdateInfo.Draging := True;
    end;

    if Items[vMouseDownItemNo].StyleNo < THCStyle.RsNull then  // 在RectItem上拖拽
      DoItemMouseDown(vMouseDownItemNo, vMouseDownItemOffset);
  end
  else  // 没点在选中区域中
  begin
    if (vMouseDownItemNo <> FMouseDownItemNo)
      or (vMouseDownItemOffset <> FMouseDownItemOffset)
      or (CaretDrawItemNo <> vDrawItemNo)
    then  // 新位置
    begin
      if FMouseDownItemNo >= 0 then
        Items[FMouseDownItemNo].Active := False;  // 旧的取消激活

      Style.UpdateInfoReCaret;
      Style.UpdateInfoRePaint;  // 旧的去焦点，新的入焦点
    end;

    DisSelect;

    // 重新赋值新位置
    FMouseDownItemNo := vMouseDownItemNo;
    FMouseDownItemOffset := vMouseDownItemOffset;
    CaretDrawItemNo := vDrawItemNo;
    SelectInfo.StartItemNo := FMouseDownItemNo;
    SelectInfo.StartItemOffset := FMouseDownItemOffset;

    if not vRestrain then  // 没收敛
      DoItemMouseDown(FMouseDownItemNo, FMouseDownItemOffset);
  end;
end;

procedure THCCustomRichData.MouseMove(Shift: TShiftState; X, Y: Integer);

  {$REGION 'AdjustSelectRang'}
  procedure AdjustSelectRang;

    {$REGION 'Item的指定偏移在最后面'}
    function OffsetInItemAfter(const AItemNo, AOffset: Integer): Boolean;
    begin
      Result := False;
      if Items[AItemNo].StyleNo < THCStyle.RsNull then
        Result := AOffset = OffsetAfter
      else
        Result := AOffset = Items[AItemNo].Length;
    end;
    {$ENDREGION}

  var
    i: Integer;
  begin
    if SelectInfo.StartItemNo < 0 then Exit;

    for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do  // 先清除当前选中的Item及状态
    begin
      if not (i in [FMouseDownItemNo, FMouseMoveItemNo]) then  // 保留当前按下和移动处的选中状态
      begin
        Items[i].Active := False;
        if Items[i].StyleNo < THCStyle.RsNull then  // RectItem自己处理内部的取消选中
          (Items[i] as THCCustomRectItem).DisSelect;  // Item内部处理自己的全选、部分选状态
      end;
    end;
    SelectInfo.Initialize;

    if FMouseDownItemNo < FMouseMoveItemNo then  // 从前往后选择在不同的Item
    begin
      if OffsetInItemAfter(FMouseDownItemNo, FMouseDownItemOffset) then  // 起始在Item最后面
      begin
        if FMouseDownItemNo < Items.Count - 1 then  // 起始改为下一个Item开始
        begin
          FMouseDownItemNo := FMouseDownItemNo + 1;
          FMouseDownItemOffset := 0;
        end;
      end;
      if (FMouseMoveItemOffset = 0) and (FMouseMoveItemNo >= 0) then  // 结束在Item最前面，改为上一个Item结束
      begin
        FMouseMoveItemNo := FMouseMoveItemNo - 1;
        if Items[FMouseMoveItemNo].StyleNo < THCStyle.RsNull then
          FMouseMoveItemOffset := OffsetAfter
        else
          FMouseMoveItemOffset := Items[FMouseMoveItemNo].Length;
      end;

      SelectInfo.StartItemNo := FMouseDownItemNo;
      SelectInfo.StartItemOffset := FMouseDownItemOffset;
      SelectInfo.EndItemNo := FMouseMoveItemNo;
      SelectInfo.EndItemOffset := FMouseMoveItemOffset;
    end
    else
    if (FMouseMoveItemNo >= 0) and (FMouseMoveItemNo < FMouseDownItemNo) then  // 从后往前选择在不同的Item
    begin
      if OffsetInItemAfter(FMouseMoveItemNo, FMouseMoveItemOffset) then  // 结束在Item最后面
      begin
        if FMouseMoveItemNo < Items.Count - 1 then  // 起始改为下一个Item开始
        begin
          FMouseMoveItemNo := FMouseMoveItemNo + 1;
          FMouseMoveItemOffset := 0;
        end;
      end;
      if (FMouseDownItemOffset = 0) and (FMouseDownItemNo > 0) then  // 起始在Item最前面，改为上一个Item结束
      begin
        FMouseDownItemNo := FMouseDownItemNo - 1;
        if Items[FMouseDownItemNo].StyleNo < THCStyle.RsNull then
          FMouseDownItemOffset := OffsetAfter
        else
          FMouseDownItemOffset := Items[FMouseDownItemNo].Length;
      end;

      SelectInfo.StartItemNo := FMouseMoveItemNo;
      SelectInfo.StartItemOffset := FMouseMoveItemOffset;
      SelectInfo.EndItemNo := FMouseDownItemNo;
      SelectInfo.EndItemOffset := FMouseDownItemOffset;
    end
    else  // FMouseDownItemNo = FMouseMoveItemNo  // 选择发生在同一个Item
    begin
      if FMouseMoveItemOffset > FMouseDownItemOffset then  // 选中结束位置大于起始位置
      begin
        SelectInfo.StartItemNo := FMouseDownItemNo;
        SelectInfo.StartItemOffset := FMouseDownItemOffset;
        if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull then  // RectItem
        begin
          if FMouseMoveItemOffset > FMouseDownItemOffset then  // 从RectItem最前面选择到了最后面(全选中)
          begin
            SelectInfo.EndItemNo := FMouseMoveItemNo;
            SelectInfo.EndItemOffset := FMouseMoveItemOffset;
          end
          else  // 没有全选中
          begin
            SelectInfo.EndItemNo := -1;
            SelectInfo.EndItemOffset := -1;
          end;
        end
        else  // TextItem
        begin
          SelectInfo.EndItemNo := FMouseMoveItemNo;
          SelectInfo.EndItemOffset := FMouseMoveItemOffset;
        end;
      end
      else
      if FMouseMoveItemOffset < FMouseDownItemOffset then  // 选中结束位置小于起始位置
      begin
        SelectInfo.StartItemNo := FMouseDownItemNo;
        SelectInfo.StartItemOffset := FMouseMoveItemOffset;
        if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull then  // RectItem
        begin
          if FMouseMoveItemOffset = OffsetBefor then
          begin
            SelectInfo.EndItemNo := FMouseMoveItemNo;
            SelectInfo.EndItemOffset := FMouseDownItemOffset;
          end
          else
          begin
            SelectInfo.EndItemNo := -1;
            SelectInfo.EndItemOffset := -1;
          end;
        end
        else
        begin
          SelectInfo.EndItemNo := FMouseMoveItemNo;
          SelectInfo.EndItemOffset := FMouseDownItemOffset;
        end;
      end
      else  // 结束位置和起始位置相同
      begin
        SelectInfo.StartItemNo := FMouseDownItemNo;
        SelectInfo.StartItemOffset := FMouseDownItemOffset;
        Items[SelectInfo.StartItemNo].Active := not FMouseMoveRestrain;
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end;
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
  vMouseMoveItemNo, vMouseMoveItemOffset, vDrawItemNo: Integer;
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

  GetItemAt(X, Y, vMouseMoveItemNo, vMouseMoveItemOffset, vDrawItemNo, vRestrain);

  if FDraging or Style.UpdateInfo.Draging{解决拖拽在起始单元格光标不定位} then  // 拖拽
  begin
    GCursor := crDrag;

    FMouseMoveItemNo := vMouseMoveItemNo;
    FMouseMoveItemOffset := vMouseMoveItemOffset;
    FMouseMoveRestrain := vRestrain;
    CaretDrawItemNo := vDrawItemNo;

    Style.UpdateInfoReCaret;

    if Items[FMouseMoveItemNo].StyleNo < THCStyle.RsNull then  // RectItem
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
  end
  else
  if FSelecting then  // 划选
  begin
    FMouseMoveItemNo := vMouseMoveItemNo;
    FMouseMoveItemOffset := vMouseMoveItemOffset;
    FMouseMoveRestrain := vRestrain;

    AdjustSelectRang;  // 确定SelectRang
    MatchItemSelectState;  // 设置选中范围内的Item选中状态
    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;

    if Items[FMouseMoveItemNo].StyleNo < THCStyle.RsNull then  // RectItem
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
  end
  else  // 非拖拽，非划选
  if FMouseLBDowning and ((FMouseDownX <> X) or (FMouseDownY <> Y)) then  // 左键按下移动，开始划选
  begin
    FSelecting := True;
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
  vDragStartItemNo, vDragEndItemNo: Integer;

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

  {$REGION ' DragNotify 通知选中的Item，拖拽完成 '}
  procedure DragNotify(const AFinish: Boolean);
  var
    i: Integer;
  begin
    // 起始在RectItem一定要处理(单独处理是防止选中只在RectItem上进行)
    if Items[vDragStartItemNo].StyleNo < THCStyle.RsNull then
      (Items[vDragStartItemNo] as THCCustomRectItem).DragNotify(AFinish);
       
    for i := vDragStartItemNo + 1 to vDragEndItemNo do
    begin
      if Items[i].StyleNo < THCStyle.RsNull then
        (Items[i] as THCCustomRectItem).DragNotify(AFinish);
    end;
  end;
  {$ENDREGION}

  {$REGION ' DragCancel '}
  procedure DragCancel;
  begin
    if DisSelect then
    begin
      Self.Initialize;
      SelectInfo.StartItemNo := vUpItemNo;
      SelectInfo.StartItemOffset := vUpItemOffset;
      CaretDrawItemNo := vDrawItemNo;  // GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      Style.UpdateInfoReCaret;

      if Items[vUpItemNo].StyleNo < THCStyle.RsNull then  // 弹起时在RectItem
        DoItemMouseUp(vUpItemNo, vUpItemOffset);      
    end;
  end;
  {$ENDREGION}

  {$REGION ' DragFinish '}
  procedure DragFinish;
  begin
    { TODO : 得到拖拽内容 }    
    DragCancel;
  end;
  {$ENDREGION}

var
  vFormatFirstItemNo, vFormatLastItemNo: Integer;
  vRestrain: Boolean;
  vMouseUpInSelect: Boolean;
begin
  Style.UpdateInfo.Draging := False;

//  if not FMouseLBDowning then Exit;  // 打开文件对话框双击文件后会触发MouseUp

  FMouseLBDowning := False;

  if SelectedResizing then  // RectItem缩放ing，停止缩放
  begin
    DoItemMouseUp(FMouseDownItemNo, FMouseDownItemOffset);
    GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo, FMouseDownItemNo, FMouseDownItemOffset);
    FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
    ReFormatData_(vFormatFirstItemNo, vFormatLastItemNo);
    Style.UpdateInfoRePaint;

    Exit;
  end;

  GetItemAt(X, Y, vUpItemNo, vUpItemOffset, vDrawItemNo, vRestrain);

  if FSelecting then  // 划选完成弹起
  begin
    FSelecting := False;
    if Items[vUpItemNo].StyleNo < THCStyle.RsNull then  // 弹起时在RectItem
      DoItemMouseUp(vUpItemNo, vUpItemOffset);
  end
  else
  if FDraging then  // 拖拽弹起
  begin
    FDraging := False;

    vDragStartItemNo := SelectInfo.StartItemNo;
    vDragEndItemNo := SelectInfo.EndItemNo;
    
    vMouseUpInSelect := (not vRestrain) and CoordInSelect(X, Y, vUpItemNo, vUpItemOffset);
    if not vMouseUpInSelect then  // 拖拽弹起时不在选中内容中
    begin
      DragFinish;
      DragNotify(True);
    end
    else  // 拖拽弹起时还是在选中内容中
    begin
      DragCancel;
      DragNotify(False);
    end;
  end
  else  // 非拖拽、非划选
  begin
    if SelectExists(False) then  // 清除本Data层面内的选中
      DisSelect;

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
    Style.UpdateInfoReCaret;

    DoItemMouseUp(vUpItemNo, vUpItemOffset);  // 弹起，因为可能是移出Item后弹起，所以这里不受vRestrain约束
  end;
end;

procedure THCCustomRichData.ReFormatData_(const AStartItemNo: Integer; const ALastItemNo: Integer = -1;
  const AExtraItemCount: Integer = 0);
var
  i, vLastItemNo, vLastDrawItemNo, vFormatIncHight,
  vDrawItemCount, vFmtTopOffset, vClearFmtHeight: Integer;
begin
//  vLastDrawItemNo := GetItemLastDrawItemNo(ALastItemNo);  // 段最后一个DrawItem
//
//  if vLastDrawItemNo < 0 then  // 没有被格式化过，取同一段中当前Item前面最近的
//    vLastDrawItemNo := GetItemNearDrawItemNo(ALastItemNo);
//
//  if vLastDrawItemNo < 0 then  // 空文档
//    vNextParaTop := 0
//  else
//  {if vLastDrawItemNo <> DrawItems.Count - 1 then
//    vNextParaTop := DrawItems[vLastDrawItemNo + 1].Rect.Top  // 下一段的起始位置
//  else}
//    vNextParaTop := DrawItems[vLastDrawItemNo].Rect.Bottom;  // 段格式化前，底部位置
  //
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
  // 由图2017-6-8_1变为图2017-6-8_2的过程中，第3段位置没变，也没有新的Item数量变化，但是DrawItem的数量有变化
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

          if Items[DrawItems[i].ItemNo].StyleNo < THCStyle.RsNull then  // RectItem如表格，在格式化时有行和行中间的偏移，新格式化时要恢复，由分页函数再处理新格式化后的偏移
          begin
            vClearFmtHeight := (Items[DrawItems[i].ItemNo] as THCCustomRectItem).ClearFormatExtraHeight;
            DrawItems[i].Rect.Bottom := DrawItems[i].Rect.Bottom - vClearFmtHeight;
          end;
        end;
      end;
    end;

    // 将原格式化因分页等原因引起的整体下移恢复回来

    {if DrawItems.Count > vLastDrawItemNo + 1 then
    begin
      //vFmtTopOffset := DrawItems[vLastDrawItemNo + 1].Rect.Top - DrawItems[vLastDrawItemNo].Rect.Top;
      for i := vLastDrawItemNo + 1 to DrawItems.Count - 1 do  // 从格式化变动段的下一段开始
      begin
        if DrawItems[i].LineFirst then
          vFmtTopOffset := DrawItems[i - 1].Rect.Bottom - DrawItems[i].Rect.Top;

        OffsetRect(DrawItems[i].Rect, 0, vFmtTopOffset);

        if Items[DrawItems[i].ItemNo].StyleNo < THCStyle.RsNull then  // RectItem如表格，在格式化时有行和行中间的偏移，新格式化时要恢复，由分页函数再处理新格式化后的偏移
        begin
          vRectReFormatHight := (Items[DrawItems[i].ItemNo] as THCCustomRectItem).ClearFormatExtraHeight;
          DrawItems[i].Rect.Bottom := DrawItems[i].Rect.Bottom - vRectReFormatHight;
        end;
      end;
    end;}
    {
    vFmtTopOffset := 0;
    for i := vLastDrawItemNo + 1 to DrawItems.Count - 1 do
    begin
      if (i > vLastDrawItemNo + 1) and DrawItems[i].LineFirst then
      begin
        if DrawItems[i].Rect.Top <> DrawItems[i - 1].Rect.Bottom then
          vFmtTopOffset := vFmtTopOffset - (DrawItems[i].Rect.Top - DrawItems[i - 1].Rect.Bottom);

        if Items[DrawItems[i].ItemNo].StyleNo < THCStyle.RsNull then  // RectItem如表格，在格式化时有行和行中间的偏移，新格式化时要恢复，由分页函数再处理新格式化后的偏移
        begin
          vRectReFormatHight := (Items[DrawItems[i].ItemNo] as THCCustomRectItem).GetFormatDiffClearHeight;
          DrawItems[i].Rect.Bottom := DrawItems[i].Rect.Bottom - vRectReFormatHight;
          OffsetRect(DrawItems[i].Rect, 0, vFmtTopOffset);
          vFmtTopOffset := vFmtTopOffset - vRectReFormatHight;  // 将分页增加的高度恢复回来
          Continue;
        end;
      end;
      OffsetRect(DrawItems[i].Rect, 0, vFmtTopOffset);
    end;

    for i := vLastDrawItemNo + 1 to DrawItems.Count - 1 do
    begin
      OffsetRect(DrawItems[i].Rect, 0, vFormatIncHight);  // 新格式化后偏移
    end; }

    {vLastItemNo := -1;
    for i := vLastDrawItemNo + 1 to DrawItems.Count - 1 do
    begin
      DrawItems[i].ItemNo := DrawItems[i].ItemNo + AExtraItemCount;
      if vLastItemNo <> DrawItems[i].ItemNo then
      begin
        vLastItemNo := DrawItems[i].ItemNo;
        Items[vLastItemNo].FirstDItemNo := i;
      end;

      // DrawItems[i]对应的Item整体跨页了或某种原因整体向下偏移了
      if (DrawItems[i].LineFirst) and (DrawItems[i].Rect.Top + vFormatIncHight <> DrawItems[i - 1].Rect.Bottom) then
        vFormatIncHight := vFormatIncHight - (DrawItems[i].Rect.Top + vFormatIncHight - DrawItems[i - 1].Rect.Bottom);  // 重新格式化时先紧贴着上一个下面，由分页函数再处理新格式化后的偏移

      OffsetRect(DrawItems[i].Rect, 0, vFormatIncHight);  // 新格式化后偏移

      if Items[DrawItems[i].ItemNo].StyleNo < THCStyle.RsNull then  // RectItem如表格，在格式化时有行和行中间的偏移，新格式化时要恢复，由分页函数再处理新格式化后的偏移
      begin
        vRectReFormatHight := (Items[DrawItems[i].ItemNo] as THCCustomRectItem).GetFormatDiffClearHeight;
        vFormatIncHight := vFormatIncHight - vRectReFormatHight;  // 将分页增加的高度恢复回来
        DrawItems[i].Rect.Bottom := DrawItems[i].Rect.Bottom - vRectReFormatHight;
      end;
    end; }
  end;
end;

procedure THCCustomRichData.ReSetSelectAndCaret(const AItemNo, AOffset: Integer);
begin
  Self.Initialize;
  SelectInfo.StartItemNo := AItemNo;
  SelectInfo.StartItemOffset := AOffset;
  CaretDrawItemNo := GetDrawItemNoByOffset(AItemNo, AOffset);
end;

procedure THCCustomRichData.ReSetSelectAndCaret(const AItemNo: Integer);
begin
  if Items[AItemNo].StyleNo < THCStyle.RsNull then
    ReSetSelectAndCaret(AItemNo, OffsetAfter)
  else
    ReSetSelectAndCaret(AItemNo, Items[AItemNo].Length);
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

  {if AEndItemNo = Items.Count - 1 then  // 用 -1 判断刚插入没格式化的更精确吗？
    vEndDrawItemNo := DrawItems.Count - 1
  else
    vEndDrawItemNo := GetItemLastDrawItemNo(AEndItemNo);

  if vEndDrawItemNo < 0 then
    vEndDrawItemNo := GetItemNearDrawItemNo(AEndItemNo);  // 还没有被格式化，如将Item回车换行变成2个时，第2个

  for i := vEndDrawItemNo downto APrioDrawItemNo + 1 do
    DrawItems.Delete(i);}
end;

end.


