{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{         支持撤销、恢复操作的文档对象管理单元          }
{                                                       }
{*******************************************************}

unit HCUndoData;

interface

uses
  Windows, Classes, HCCommon, HCUndo, HCFormatData, HCItem, HCStyle;

type
  THCUndoData = class(THCFormatData)  // 支持撤销恢复功能的Data
  private
    FFormatFirstItemNo, FFormatFirstDrawItemNo, FFormatLastItemNo,
      FUndoGroupCount, FItemAddCount: Integer;
    FForceClearExtra: Boolean;  // 分页情况下强制清除格式化后面的分页偏移量
    procedure DoUndoRedo(const AUndo: THCCustomUndo);
  protected
    function GetUndoList: THCUndoList; override;

    { 撤销恢复相关方法+ }
    procedure Undo_New;
    procedure Undo_GroupBegin(const AItemNo, AOffset: Integer);
    procedure Undo_GroupEnd(const AItemNo, AOffset: Integer);

    /// <summary> 删除Text </summary>
    /// <param name="AItemNo">操作发生时的ItemNo</param>
    /// <param name="AOffset">删除的起始位置</param>
    /// <param name="AText"></param>
    procedure UndoAction_DeleteBackText(const AItemNo, AOffset: Integer; const AText: string);
    procedure UndoAction_DeleteText(const AItemNo, AOffset: Integer; const AText: string);
    procedure UndoAction_InsertText(const AItemNo, AOffset: Integer; const AText: string);
    /// <summary> 直接替换TextItem的Text </summary>
    procedure UndoAction_SetItemText(const AItemNo, AOffset: Integer; const ANewText: string);

    /// <summary> 删除指定的Item </summary>
    /// <param name="AItemNo">操作发生时的ItemNo</param>
    /// <param name="AOffset">操作发生时的Offset</param>
    procedure UndoAction_DeleteItem(const AItemNo, AOffset: Integer);

    /// <summary> 插入Item到指定位置 </summary>
    /// <param name="AItemNo">操作发生时的ItemNo</param>
    /// <param name="AOffset">操作发生时的Offset</param>
    procedure UndoAction_InsertItem(const AItemNo, AOffset: Integer);
    procedure UndoAction_ItemStyle(const AItemNo, AOffset, ANewStyleNo: Integer);
    procedure UndoAction_ItemParaNo(const AItemNo, AOffset, ANewParaNo: Integer);

    /// <summary> 修改Item的段起始属性(修改前调用) </summary>
    /// <param name="AItemNo">要修改的ItemNo</param>
    /// <param name="AOffset">修改时所在的偏移位置</param>
    /// <param name="ANewParaFirst">新的段首属性</param>
    procedure UndoAction_ItemParaFirst(const AItemNo, AOffset: Integer; const ANewParaFirst: Boolean);

    /// <summary> 修改Item的分页属性(修改前调用) </summary>
    procedure UndoAction_ItemPageBreak(const AItemNo, AOffset: Integer; const ANewPageBreak: Boolean);

    procedure UndoAction_ItemSelf(const AItemNo, AOffset: Integer);
    procedure UndoAction_ItemMirror(const AItemNo, AOffset: Integer);
    { 撤销恢复相关方法- }
  public
    constructor Create(const AStyle: THCStyle); override;
    procedure Clear; override;
    procedure Undo(const AUndo: THCCustomUndo); virtual;
    procedure Redo(const ARedo: THCCustomUndo); virtual;
    procedure UndoItemMirror(const AItemNo, AOffset: Integer);
    // Item单独保存和读取事件
    procedure SaveItemToStreamAlone(const AStream: TStream; const AItem: THCCustomItem);

    /// <summary> 从流中加载一个Item，如果Item为nil，则会创建它并加载 </summary>
    /// <param name="AStream"></param>
    /// <param name="AItem"></param>
    procedure LoadItemFromStreamAlone(const AStream: TStream; var AItem: THCCustomItem);
  end;

implementation

uses
  SysUtils, HCRectItem, HCTextStyle, HCParaStyle;

{ THCUndoData }

procedure THCUndoData.Clear;
var
  i: Integer;
  vUndoList: THCUndoList;
begin
  if Items.Count > 0 then
  begin
    vUndoList := GetUndoList;
    if Assigned(vUndoList) and vUndoList.Enable then
    begin
      Undo_New;
      for i := Items.Count - 1 downto 0 do
        UndoAction_DeleteItem(i, 0);
    end;
  end;

  inherited Clear;
end;

constructor THCUndoData.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  FUndoGroupCount := 0;
  FItemAddCount := 0;
end;

procedure THCUndoData.DoUndoRedo(const AUndo: THCCustomUndo);
var
  vCaretItemNo, vCaretOffset, vCaretDrawItemNo: Integer;

  procedure DoUndoRedoAction(const AAction: THCCustomUndoAction;
    const AIsUndo: Boolean);

    {$REGION 'UndoRedoDeleteBackText'}
    procedure UndoRedoDeleteBackText;
    var
      vAction: THCTextUndoAction;
      vText: string;
      vLen: Integer;
    begin
      vAction := AAction as THCTextUndoAction;
      vCaretItemNo := vAction.ItemNo;
      vLen := Length(vAction.Text);
      vText := Items[vAction.ItemNo].Text;
      if AIsUndo then
      begin
        Insert(vAction.Text, vText, vAction.Offset);
        vCaretOffset := vAction.Offset - 1;  // 不 + vLen - 1，防止Offset超过当前CaretDrawItem范围
      end
      else
      begin
        Delete(vText, vAction.Offset, vLen);
        vCaretOffset := vAction.Offset - 1;
      end;

      Items[vAction.ItemNo].Text := vText;
    end;
    {$ENDREGION}

    {$REGION 'UndoRedoDeleteText'}
    procedure UndoRedoDeleteText;
    var
      vAction: THCTextUndoAction;
      vText: string;
      vLen: Integer;
    begin
      vAction := AAction as THCTextUndoAction;
      vCaretItemNo := vAction.ItemNo;
      vLen := Length(vAction.Text);
      vText := Items[vAction.ItemNo].Text;
      if AIsUndo then
        Insert(vAction.Text, vText, vAction.Offset)
      else
        Delete(vText, vAction.Offset, vLen);

      vCaretOffset := vAction.Offset - 1;
      Items[vAction.ItemNo].Text := vText;
    end;
    {$ENDREGION}

    {$REGION 'UndoRedoInsertText'}
    procedure UndoRedoInsertText;
    var
      vAction: THCTextUndoAction;
      vText: string;
      vLen: Integer;
    begin
      vAction := AAction as THCTextUndoAction;
      vCaretItemNo := vAction.ItemNo;
      vText := Items[vAction.ItemNo].Text;;
      vLen := Length(vAction.Text);

      if AIsUndo then
      begin
        Delete(vText, vAction.Offset, vLen);
        vCaretOffset := vAction.Offset - 1;
      end
      else
      begin
        Insert(vAction.Text, vText, vAction.Offset);
        vCaretOffset := vAction.Offset + vLen - 1;
      end;

      Items[vAction.ItemNo].Text := vText;
    end;
    {$ENDREGION}

    {$REGION 'UndoRedoSetItemText'}
    procedure UndoRedoSetItemText;
    var
      vAction: THCSetItemTextUndoAction;
      vOldText: string;
    begin
      vAction := AAction as THCSetItemTextUndoAction;
      vCaretItemNo := vAction.ItemNo;

      if AIsUndo then
      begin
        Items[vAction.ItemNo].Text := vAction.Text;
        vCaretOffset := vAction.Offset;
      end
      else
      begin
        Items[vAction.ItemNo].Text := vAction.NewText;
        vCaretOffset := Length(vAction.NewText);
      end;
    end;
    {$ENDREGION}

    {$REGION 'UndoRedoDeleteItem'}
    procedure UndoRedoDeleteItem;
    var
      vAction: THCItemUndoAction;
      vItem: THCCustomItem;
    begin
      vAction := AAction as THCItemUndoAction;
      vCaretItemNo := vAction.ItemNo;

      if AIsUndo then  // 撤销
      begin
        vItem := nil;
        LoadItemFromStreamAlone(vAction.ItemStream, vItem);
        Items.Insert(vAction.ItemNo, vItem);
        Inc(FItemAddCount);

        vCaretOffset := vAction.Offset;
      end
      else  // 重做
      begin
        Items.Delete(vAction.ItemNo);
        Dec(FItemAddCount);

        if vCaretItemNo > 0 then
        begin
          Dec(vCaretItemNo);

          if Items[vCaretItemNo].StyleNo > THCStyle.Null then
            vCaretOffset := Items[vCaretItemNo].Length
          else
            vCaretOffset := OffsetAfter;
        end
        else
          vCaretOffset := 0;
      end;
    end;
    {$ENDREGION}

    {$REGION 'UndoRedoInsertItem'}
    procedure UndoRedoInsertItem;
    var
      vAction: THCItemUndoAction;
      vItem: THCCustomItem;
    begin
      vAction := AAction as THCItemUndoAction;
      vCaretItemNo := vAction.ItemNo;

      if AIsUndo then  // 撤销
      begin
        if vCaretItemNo < Items.Count - 1 then  // 不是最后一个
        begin
          if Items[vCaretItemNo].ParaFirst then  // 段首删除了，光标为下一个开始
          begin
            vCaretOffset := 0;
            vCaretDrawItemNo := Items[vCaretItemNo + 1].FirstDItemNo;
          end
          else  // 删除的不是段首
          //if Items[vCaretItemNo + 1].ParaFirst then  // 下一个是段首，光标保持在同段最后
          begin
            Dec(vCaretItemNo);
            if Items[vCaretItemNo].StyleNo > THCStyle.Null then
              vCaretOffset := Items[vCaretItemNo].Length
            else
              vCaretOffset := OffsetAfter;

            vCaretDrawItemNo := (AUndo as THCDataUndo).CaretDrawItemNo;// - 1;
          end;
        end
        else
        if vCaretItemNo > 0 then  // 不是第一个
        begin
          Dec(vCaretItemNo);
          if Items[vCaretItemNo].StyleNo > THCStyle.Null then
            vCaretOffset := Items[vCaretItemNo].Length
          else
            vCaretOffset := OffsetAfter;

          vCaretDrawItemNo := (AUndo as THCDataUndo).CaretDrawItemNo - 1;
        end
        else
          vCaretOffset := 0;

        Items.Delete(vAction.ItemNo);
        Dec(FItemAddCount);
      end
      else  // 重做
      begin
        vItem := nil;
        LoadItemFromStreamAlone(vAction.ItemStream, vItem);
        Items.Insert(vAction.ItemNo, vItem);
        Inc(FItemAddCount);

        vCaretItemNo := vAction.ItemNo;
        if Items[vCaretItemNo].StyleNo > THCStyle.Null then
          vCaretOffset := Items[vCaretItemNo].Length
        else
          vCaretOffset := OffsetAfter;

        vCaretDrawItemNo := (AUndo as THCDataUndo).CaretDrawItemNo + 1;
      end;
    end;
    {$ENDREGION}

    {$REGION 'UndoRedoItemProperty'}
    procedure UndoRedoItemProperty;
    var
      vAction: THCItemPropertyUndoAction;
      vItem: THCCustomItem;
      i, vParaLastItemNo: Integer;
    begin
      vAction := AAction as THCItemPropertyUndoAction;
      vCaretItemNo := vAction.ItemNo;
      vCaretOffset := vAction.Offset;
      vItem := Items[vAction.ItemNo];

      case vAction.ItemProperty of
        uipStyleNo:
          begin
            if AIsUndo then
              vItem.StyleNo := (vAction as THCItemStyleUndoAction).OldStyleNo
            else
              vItem.StyleNo := (vAction as THCItemStyleUndoAction).NewStyleNo;
          end;

        uipParaNo:
          begin
            vParaLastItemNo := GetParaLastItemNo(vAction.ItemNo);
            if AIsUndo then
            begin
              for i := vAction.ItemNo to vParaLastItemNo do
                Items[i].ParaNo := (vAction as THCItemParaUndoAction).OldParaNo;
            end
            else
            begin
              for i := vAction.ItemNo to vParaLastItemNo do
                Items[i].ParaNo := (vAction as THCItemParaUndoAction).NewParaNo;
            end;
          end;

        uipParaFirst:
          begin
            if AIsUndo then
              vItem.ParaFirst := (vAction as THCItemParaFirstUndoAction).OldParaFirst
            else
              vItem.ParaFirst := (vAction as THCItemParaFirstUndoAction).NewParaFirst;
          end;

        uipPageBreak:
          begin
            FForceClearExtra := True;

            if AIsUndo then
              vItem.PageBreak := (vAction as THCItemPageBreakUndoAction).OldPageBreak
            else
              vItem.PageBreak := (vAction as THCItemPageBreakUndoAction).NewPageBreak;
          end;
      end;
    end;
    {$ENDREGION}

    {$REGION 'UndoRedoItemSelf'}
    procedure UndoRedoItemSelf;
    var
      vAction: THCItemSelfUndoAction;
    begin
      vAction := AAction as THCItemSelfUndoAction;
      vCaretItemNo := vAction.ItemNo;
      vCaretOffset := vAction.Offset;
      if AIsUndo then
        Items[vCaretItemNo].Undo(vAction)
      else
        Items[vCaretItemNo].Redo(vAction);
    end;
    {$ENDREGION}

    {$REGION 'UndoRedoItemMirror'}
    procedure UndoRedoItemMirror;
    var
      vAction: THCItemUndoAction;
      vItem: THCCustomItem;
    begin
      vAction := AAction as THCItemUndoAction;
      vCaretItemNo := vAction.ItemNo;
      vCaretOffset := vAction.Offset;
      vItem := Items[vCaretItemNo];
      if AIsUndo then
        LoadItemFromStreamAlone(vAction.ItemStream, vItem)
      else
        LoadItemFromStreamAlone(vAction.ItemStream, vItem);
    end;
    {$ENDREGION}

  begin
    case AAction.Tag of
      actBackDeleteText: UndoRedoDeleteBackText;
      actDeleteText: UndoRedoDeleteText;
      actInsertText: UndoRedoInsertText;
      actSetItemText: UndoRedoSetItemText;
      actDeleteItem: UndoRedoDeleteItem;
      actInsertItem: UndoRedoInsertItem;
      actItemProperty: UndoRedoItemProperty;
      actItemSelf: UndoRedoItemSelf;
      actItemMirror: UndoRedoItemMirror;
    end;
  end;

  /// <summary> 根据撤销/恢复的事件类型计算前一个ItemNo是否需要格式化 </summary>
  function GetActionAffectFirst(const AAction: THCCustomUndoAction): Integer;
  var
    vPropAction: THCItemPropertyUndoAction;
  begin
    Result := AAction.ItemNo;

    case AAction.Tag of
      actDeleteItem:
        begin
          if Result > 0 then
            Dec(Result);
        end;

      actInsertItem:
        begin
          if AUndo.IsUndo then  // 是撤销
          begin
            if Result > 0 then
              Dec(Result);
          end
          else  // 是恢复
          begin
            if Result > Items.Count - 1 then
              Dec(Result);
          end;
        end;

      actItemProperty:
        begin
          vPropAction := AAction as THCItemPropertyUndoAction;
          if (vPropAction.ItemProperty = TItemProperty.uipParaFirst)  // 段首属性变化
            or (vPropAction.ItemProperty = TItemProperty.uipPageBreak)  // 分页属性变化
          then
          begin
            if Result > 0 then
              Dec(Result);
          end;
        end;
    end;

    if Result > Items.Count - 1 then
      Result := Items.Count - 1;
  end;

  /// <summary> 根据撤销/恢复的事件类型计算后一个ItemNo是否需要格式化 </summary>
  function GetActionAffectLast(const AAction: THCCustomUndoAction): Integer;
  begin
    Result := AAction.ItemNo;

    case AAction.Tag of
      actDeleteItem:
        begin
          if AUndo.IsUndo then
          begin
            if Result > 0 then
              Dec(Result);
          end
          else
          begin
            if Result < Items.Count - 1 then
              Inc(Result);
          end;
        end;

      actInsertItem:
        begin
          if AUndo.IsUndo then
          begin
            if (Result < Items.Count - 1) and AAction.ParaFirst then
              Inc(Result);
          end
          else
          begin
            if Result > Items.Count - 1 then
              Dec(Result);
          end;
        end;
    end;

    if Result > Items.Count - 1 then
      Result := Items.Count - 1;
  end;

var
  i, vItemNo, vCaretDIItem: Integer;
  vUndoList: THCUndoList;
begin
  FForceClearExtra := False;

  if AUndo is THCUndoGroupEnd then  // 组结束(无Actions)
  begin
    if AUndo.IsUndo then  // 组撤销(无Action)
    begin
      if FUndoGroupCount = 0 then  // 组撤销开始
      begin
        vUndoList := GetUndoList;
        FFormatFirstItemNo := (vUndoList[vUndoList.CurGroupBeginIndex] as THCUndoGroupBegin).ItemNo;
        FFormatLastItemNo := (vUndoList[vUndoList.CurGroupEndIndex] as THCUndoGroupEnd).ItemNo;

        // 如果是序号0、1在一行，1删除后，又插入2个，其中第一个和序号0合并，第2个换行，
        // 撤销时FFormatFirstItemNo为1从而FFormatFirstDrawItemNo为1，而此时1为行首，
        // 撤销完后格式化会按在行首开始，导致原来的序号1撤销后换行。所以暴力回退1个
        if FFormatFirstItemNo > 0 then
          Dec(FFormatFirstItemNo);

        // 如果FFormatLastItemNo是从文档最开始插入增加的，撤销时要格式化到原开头，暴力处理直接下一个
        if FFormatLastItemNo < Items.Count - 1 then
          Inc(FFormatLastItemNo);

        if FFormatFirstItemNo <> FFormatLastItemNo then
        begin
          FFormatFirstItemNo := GetParaFirstItemNo(FFormatFirstItemNo);  // 取段第一个为起始
          FFormatFirstDrawItemNo := Items[FFormatFirstItemNo].FirstDItemNo;
          FFormatLastItemNo := GetParaLastItemNo(FFormatLastItemNo);  // 取段最后一个为结束
        end
        else
          GetFormatRange(FFormatFirstItemNo, 1, FFormatFirstDrawItemNo, FFormatLastItemNo);

        FormatPrepare(FFormatFirstDrawItemNo, FFormatLastItemNo);

        SelectInfo.Initialize;
        Self.InitializeField;
        FItemAddCount := 0;
      end;

      Inc(FUndoGroupCount);  // 增加组撤销读数
    end
    else  // 组恢复结束
    begin
      Dec(FUndoGroupCount);  // 减少组恢复读数

      if FUndoGroupCount = 0 then  // 组恢复结束
      begin
        ReFormatData(FFormatFirstDrawItemNo, FFormatLastItemNo + FItemAddCount,
          FItemAddCount, FForceClearExtra);

        SelectInfo.StartItemNo := (AUndo as THCUndoGroupEnd).ItemNo;
        SelectInfo.StartItemOffset := (AUndo as THCUndoGroupEnd).Offset;
        CaretDrawItemNo := (AUndo as THCUndoGroupEnd).CaretDrawItemNo;

        Style.UpdateInfoReCaret;
        Style.UpdateInfoRePaint;
      end;
    end;

    Exit;
  end
  else
  if AUndo is THCUndoGroupBegin then  // 组开始
  begin
    if AUndo.IsUndo then  // 组撤销(无Action)
    begin
      Dec(FUndoGroupCount);  // 减少撤销读数

      if FUndoGroupCount = 0 then  // 组撤销结束
      begin
        ReFormatData(FFormatFirstDrawItemNo, FFormatLastItemNo + FItemAddCount,
          FItemAddCount, FForceClearExtra);

        SelectInfo.StartItemNo := (AUndo as THCUndoGroupBegin).ItemNo;
        SelectInfo.StartItemOffset := (AUndo as THCUndoGroupBegin).Offset;
        CaretDrawItemNo := (AUndo as THCUndoGroupBegin).CaretDrawItemNo;

        Style.UpdateInfoReCaret;
        Style.UpdateInfoRePaint;
      end;
    end
    else  // 组恢复(无Action)
    begin
      if FUndoGroupCount = 0 then  // 组恢复开始
      begin
        vUndoList := GetUndoList;
        FFormatFirstItemNo := -1;
        FFormatLastItemNo := -1;

        for i := vUndoList.CurGroupBeginIndex to vUndoList.CurGroupEndIndex do
        begin
          if vUndoList[i] is THCUndoGroupBegin then
          begin
            if FFormatFirstItemNo > (vUndoList[i] as THCUndoGroupBegin).ItemNo then
              FFormatFirstItemNo := (vUndoList[i] as THCUndoGroupBegin).ItemNo;
          end
          else
          if vUndoList[i] is THCUndoGroupEnd then
          begin
            if FFormatLastItemNo < (vUndoList[i] as THCUndoGroupEnd).ItemNo then
              FFormatLastItemNo := (vUndoList[i] as THCUndoGroupEnd).ItemNo;
          end
          else
          begin
            vItemNo := GetParaFirstItemNo(GetActionAffectFirst(vUndoList[i].Actions.First));
            if FFormatFirstItemNo > vItemNo then
              FFormatFirstItemNo := vItemNo;

            vItemNo := GetParaLastItemNo(GetActionAffectLast(vUndoList[i].Actions.Last));
            if FFormatLastItemNo < vItemNo then
              FFormatLastItemNo := vItemNo;
          end;
        end;

        if FFormatFirstItemNo < 0 then
          FFormatFirstItemNo := 0;

        if FFormatLastItemNo > Items.Count - 1 then  // 防止在最后插入Item的撤销后恢复访问越界
          FFormatLastItemNo := Items.Count - 1;

        FFormatFirstDrawItemNo := GetFormatFirstDrawItem(Items[FFormatFirstItemNo].FirstDItemNo);

        {FFormatFirstItemNo := (AUndo as THCUndoGroupBegin).ItemNo;
        FFormatFirstDrawItemNo := GetFormatFirstDrawItem(Items[FFormatFirstItemNo].FirstDItemNo);

        vUndoList := GetUndoList;
        FFormatLastItemNo := (vUndoList[vUndoList.CurGroupEndIndex] as THCUndoGroupEnd).ItemNo;
        if FFormatLastItemNo > Items.Count - 1 then  // 防止在最后插入Item的撤销后恢复访问越界
          Dec(FFormatLastItemNo);   }

        FormatPrepare(FFormatFirstDrawItemNo, FFormatLastItemNo);

        SelectInfo.Initialize;
        Self.InitializeField;
        FItemAddCount := 0;
      end;

      Inc(FUndoGroupCount);  // 增加组恢复读数
    end;

    Exit;
  end;

  if AUndo.Actions.Count = 0 then Exit;  // 组操作都没有Action，由此处跳出

  if FUndoGroupCount = 0 then
  begin
    SelectInfo.Initialize;
    Self.InitializeField;
    FItemAddCount := 0;
    vCaretDrawItemNo := (AUndo as THCDataUndo).CaretDrawItemNo;

    if AUndo.Actions.First.ItemNo > AUndo.Actions.Last.ItemNo then
    begin
      FFormatFirstItemNo := GetParaFirstItemNo(GetActionAffectFirst(AUndo.Actions.Last));
      FFormatLastItemNo := GetParaLastItemNo(GetActionAffectLast(AUndo.Actions.First));
    end
    else
    begin
      FFormatFirstItemNo := GetParaFirstItemNo(GetActionAffectFirst(AUndo.Actions.First));
      FFormatLastItemNo := GetParaLastItemNo(GetActionAffectLast(AUndo.Actions.Last));
    end;

    FFormatFirstDrawItemNo := Items[FFormatFirstItemNo].FirstDItemNo;
    FormatPrepare(FFormatFirstDrawItemNo, FFormatLastItemNo);
  end;

  if AUndo.IsUndo then  // 撤销
  begin
    for i := AUndo.Actions.Count - 1 downto 0 do
      DoUndoRedoAction(AUndo.Actions[i], True);
  end
  else  // 重做
  begin
    for i := 0 to AUndo.Actions.Count - 1 do
      DoUndoRedoAction(AUndo.Actions[i], False);
  end;

  //if Items.Count = 0 then
  //  DrawItems.Clear;  // 对于含清空的Group撤销，中间环节会出现无Item的情况

  if FUndoGroupCount = 0 then
  begin
    ReFormatData(FFormatFirstDrawItemNo, FFormatLastItemNo + FItemAddCount,
      FItemAddCount, FForceClearExtra);

    vCaretDIItem := GetDrawItemNoByOffset(vCaretItemNo, vCaretOffset);  // 因为多个Action不一定每个会有有效的CaretDrawItem，所以需要重新计算一下
    if (vCaretDrawItemNo < 0) or (vCaretDrawItemNo > Self.DrawItems.Count - 1) then
      vCaretDrawItemNo := vCaretDIItem
    else
    if vCaretDIItem <> vCaretDrawItemNo then
    begin
      if (DrawItems[vCaretDrawItemNo].ItemNo = vCaretItemNo)
        and (DrawItems[vCaretDrawItemNo].CharOffs = vCaretOffset)
      then  // 换行

      else
        vCaretDrawItemNo := vCaretDIItem;  // 纠正
    end;

    CaretDrawItemNo := vCaretDrawItemNo;

    Style.UpdateInfoReCaret;
    Style.UpdateInfoRePaint;
  end;

  // 为提高效率，组撤销或恢复时，只在最后一个(组头、尾)后再进制格式化和计算页数
  // 所以需要每步的撤销或恢复都记录SelectInfo，以便最后一个(组头、尾)做格式化和
  // 分页时有变动前后参照位置
  SelectInfo.StartItemNo := vCaretItemNo;
  SelectInfo.StartItemOffset := vCaretOffset;
end;

function THCUndoData.GetUndoList: THCUndoList;
begin
  if Self.Loading then
    Result := nil
  else
    Result := inherited GetUndoList;
end;

procedure THCUndoData.LoadItemFromStreamAlone(const AStream: TStream;
  var AItem: THCCustomItem);
var
  vFileExt: string;
  viVersion: Word;
  vLang: Byte;
  vStyleNo, vParaNo: Integer;
  vStyle: THCStyle;
  vTextStyle: THCTextStyle;
  vParaStyle: THCParaStyle;
begin
  AStream.Position := 0;
  _LoadFileFormatAndVersion(AStream, vFileExt, viVersion, vLang);  // 文件格式和版本
  if (vFileExt <> HC_EXT) and (vFileExt <> 'cff.') then
    raise Exception.Create('加载失败，不是' + HC_EXT + '文件！');

  vStyle := THCStyle.Create;
  try
    vStyle.LoadFromStream(AStream, viVersion);
    AStream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));
    if vStyleNo > THCStyle.Null then
    begin
      vTextStyle := vStyle.TextStyles[vStyleNo];
      vStyleNo := Style.GetStyleNo(vTextStyle, True);
    end;

    if not Assigned(AItem) then
      AItem := CreateItemByStyle(vStyleNo);

    AItem.LoadFromStream(AStream, vStyle, viVersion);
    AItem.StyleNo := vStyleNo;

    vParaNo := AItem.ParaNo;
    vParaStyle := vStyle.ParaStyles[vParaNo];
    vParaNo := Style.GetParaNo(vParaStyle, True);
    AItem.ParaNo := vParaNo;
  finally
    vStyle.Free;
  end;
end;

procedure THCUndoData.Redo(const ARedo: THCCustomUndo);
begin
  DoUndoRedo(ARedo);
end;

procedure THCUndoData.SaveItemToStreamAlone(const AStream: TStream;
  const AItem: THCCustomItem);
begin
  _SaveFileFormatAndVersion(AStream);
  Style.SaveToStream(AStream);
  AItem.SaveToStream(AStream);
end;

procedure THCUndoData.Undo(const AUndo: THCCustomUndo);
begin
  DoUndoRedo(AUndo);
end;

procedure THCUndoData.UndoAction_DeleteItem(const AItemNo,
  AOffset: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vItemAction: THCItemUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := vUndo.ActionAppend(actDeleteItem, AItemNo, AOffset,
        Items[AItemNo].ParaFirst) as THCItemUndoAction;
      SaveItemToStreamAlone(vItemAction.ItemStream, Items[AItemNo]);
    end;
  end;
end;

procedure THCUndoData.UndoAction_DeleteText(const AItemNo, AOffset: Integer;
  const AText: string);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vTextAction: THCTextUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vTextAction := vUndo.ActionAppend(actDeleteText, AItemNo, AOffset,
        Items[AItemNo].ParaFirst) as THCTextUndoAction;
      vTextAction.Text := AText;
    end;
  end;
end;

procedure THCUndoData.UndoAction_DeleteBackText(const AItemNo, AOffset: Integer;
  const AText: string);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vTextAction: THCTextUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vTextAction := vUndo.ActionAppend(actBackDeleteText, AItemNo, AOffset,
        Items[AItemNo].ParaFirst) as THCTextUndoAction;
      vTextAction.Text := AText;
    end;
  end;
end;

procedure THCUndoData.UndoAction_InsertItem(const AItemNo,
  AOffset: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vItemAction: THCItemUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := vUndo.ActionAppend(actInsertItem, AItemNo, AOffset,
        Items[AItemNo].ParaFirst) as THCItemUndoAction;
      SaveItemToStreamAlone(vItemAction.ItemStream, Items[AItemNo]);
    end;
  end;
end;

procedure THCUndoData.UndoAction_InsertText(const AItemNo, AOffset: Integer;
  const AText: string);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vTextAction: THCTextUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vTextAction := vUndo.ActionAppend(actInsertText, AItemNo, AOffset,
        Items[AItemNo].ParaFirst) as THCTextUndoAction;
      vTextAction.Text := AText;
    end;
  end;
end;

procedure THCUndoData.UndoAction_ItemMirror(const AItemNo,
  AOffset: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vItemAction: THCItemUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := vUndo.ActionAppend(actItemMirror, AItemNo, AOffset,
        Items[AItemNo].ParaFirst) as THCItemUndoAction;
      SaveItemToStreamAlone(vItemAction.ItemStream, Items[AItemNo]);
    end;
  end;
end;

procedure THCUndoData.UndoAction_ItemPageBreak(const AItemNo, AOffset: Integer;
  const ANewPageBreak: Boolean);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vItemAction: THCItemPageBreakUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := THCItemPageBreakUndoAction.Create;
      vItemAction.ItemNo := AItemNo;
      vItemAction.Offset := AOffset;
      vItemAction.OldPageBreak := Items[AItemNo].PageBreak;
      vItemAction.NewPageBreak := ANewPageBreak;

      vUndo.Actions.Add(vItemAction);
    end;
  end;
end;

procedure THCUndoData.UndoAction_ItemParaFirst(const AItemNo,
  AOffset: Integer; const ANewParaFirst: Boolean);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vItemAction: THCItemParaFirstUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
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

procedure THCUndoData.UndoAction_ItemParaNo(const AItemNo, AOffset,
  ANewParaNo: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vItemAction: THCItemParaUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := THCItemParaUndoAction.Create;
      vItemAction.ItemNo := AItemNo;
      vItemAction.Offset := AOffset;
      vItemAction.OldParaNo := Items[AItemNo].ParaNo;
      vItemAction.NewParaNo := ANewParaNo;

      vUndo.Actions.Add(vItemAction);
    end;
  end;
end;

procedure THCUndoData.UndoAction_ItemSelf(const AItemNo, AOffset: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
      vUndo.ActionAppend(actItemSelf, AItemNo, AOffset, Items[AItemNo].ParaFirst);
  end;
end;

procedure THCUndoData.UndoAction_ItemStyle(const AItemNo, AOffset, ANewStyleNo: Integer);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vItemAction: THCItemStyleUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vItemAction := THCItemStyleUndoAction.Create;
      vItemAction.ItemNo := AItemNo;
      vItemAction.Offset := AOffset;
      vItemAction.OldStyleNo := Items[AItemNo].StyleNo;
      vItemAction.NewStyleNo := ANewStyleNo;

      vUndo.Actions.Add(vItemAction);
    end;
  end;
end;

procedure THCUndoData.UndoAction_SetItemText(const AItemNo, AOffset: Integer;
  const ANewText: string);
var
  vUndo: THCUndo;
  vUndoList: THCUndoList;
  vTextAction: THCSetItemTextUndoAction;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
    begin
      vTextAction := vUndo.ActionAppend(actSetItemText, AItemNo, AOffset,
        Items[AItemNo].ParaFirst) as THCSetItemTextUndoAction;
      vTextAction.Text := Items[AItemNo].Text;
      vTextAction.NewText := ANewText;
    end;
  end;
end;

procedure THCUndoData.UndoItemMirror(const AItemNo, AOffset: Integer);
begin
  Self.UndoAction_ItemMirror(AItemNo, AOffset);
end;

procedure THCUndoData.Undo_GroupBegin(const AItemNo, AOffset: Integer);
var
  vUndoList: THCUndoList;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
    vUndoList.UndoGroupBegin(AItemNo, AOffset);
end;

procedure THCUndoData.Undo_GroupEnd(const AItemNo, AOffset: Integer);
var
  vUndoList: THCUndoList;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
    vUndoList.UndoGroupEnd(AItemNo, AOffset);
end;

procedure THCUndoData.Undo_New;
var
  vUndoList: THCUndoList;
  vUndo: THCUndo;
begin
  vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.UndoNew;
    if vUndo is THCDataUndo then
      (vUndo as THCDataUndo).CaretDrawItemNo := CaretDrawItemNo;
  end;
end;

end.
