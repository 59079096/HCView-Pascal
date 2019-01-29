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
    procedure DoUndoRedo(const AUndo: THCCustomUndo);
  protected
    // Item单独保存和读取事件
    procedure SaveItemToStreamAlone(const AItem: THCCustomItem; const AStream: TStream);

    /// <summary> 从流中加载一个Item，如果Item为nil，则会创建它并加载 </summary>
    /// <param name="AStream"></param>
    /// <param name="AItem"></param>
    procedure LoadItemFromStreamAlone(const AStream: TStream; var AItem: THCCustomItem);

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

    /// <summary> 删除指定的Item </summary>
    /// <param name="AItemNo">操作发生时的ItemNo</param>
    /// <param name="AOffset">操作发生时的Offset</param>
    procedure UndoAction_DeleteItem(const AItemNo, AOffset: Integer);

    /// <summary> 插入Item到指定位置 </summary>
    /// <param name="AItemNo">操作发生时的ItemNo</param>
    /// <param name="AOffset">操作发生时的Offset</param>
    procedure UndoAction_InsertItem(const AItemNo, AOffset: Integer);
    procedure UndoAction_ItemStyle(const AItemNo, AOffset, ANewStyleNo: Integer);
    procedure UndoAction_ItemParaFirst(const AItemNo, AOffset: Integer; const ANewParaFirst: Boolean);

    procedure UndoAction_ItemSelf(const AItemNo, AOffset: Integer);
    procedure UndoAction_ItemMirror(const AItemNo, AOffset: Integer);
    { 撤销恢复相关方法- }
  public
    constructor Create(const AStyle: THCStyle); override;
    procedure Clear; override;
    procedure Undo(const AUndo: THCCustomUndo); virtual;
    procedure Redo(const ARedo: THCCustomUndo); virtual;
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
        vCaretOffset := vAction.Offset + vLen - 1;
      end
      else
      begin
        Delete(vText, vAction.Offset, vLen);
        vCaretOffset := vAction.Offset - 1;
      end;

      Items[vAction.ItemNo].Text := vText;
    end;

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

    procedure UndoRedoInsertItem;
    var
      vAction: THCItemUndoAction;
      vItem: THCCustomItem;
    begin
      vAction := AAction as THCItemUndoAction;
      vCaretItemNo := vAction.ItemNo;

      if AIsUndo then  // 撤销
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

    procedure UndoRedoItemProperty;
    var
      vAction: THCItemPropertyUndoAction;
      vItem: THCCustomItem;
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
            if AIsUndo then
              vItem.ParaNo := (vAction as THCItemParaUndoAction).OldParaNo
            else
              vItem.ParaNo := (vAction as THCItemParaUndoAction).NewParaNo;
          end;

        uipParaFirst:
          begin
            if AIsUndo then
              vItem.ParaFirst := (vAction as THCItemParaFirstUndoAction).OldParaFirst
            else
              vItem.ParaFirst := (vAction as THCItemParaFirstUndoAction).NewParaFirst;
          end;
      end;
    end;

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

  begin
    case AAction.Tag of
      uatDeleteBackText: UndoRedoDeleteBackText;
      uatDeleteText: UndoRedoDeleteText;
      uatInsertText: UndoRedoInsertText;
      uatDeleteItem: UndoRedoDeleteItem;
      uatInsertItem: UndoRedoInsertItem;
      uatItemProperty: UndoRedoItemProperty;
      uatItemSelf: UndoRedoItemSelf;
      uatItemMirror: UndoRedoItemMirror;
    end;
  end;

  function GetActionAffect(const AAction: THCCustomUndoAction): Integer;
  begin
    Result := AAction.ItemNo;
    case AAction.Tag of
      uatDeleteItem:
        begin
          if AUndo.IsUndo and (Result > 0) then
            Dec(Result);
        end;

      uatInsertItem:
        begin
          if (not AUndo.IsUndo) and (Result > Items.Count - 1) then
            Dec(Result);
        end;
    else
      if Result > Items.Count - 1 then
        Result := Items.Count - 1;
    end;
  end;

var
  i: Integer;
  vUndoList: THCUndoList;
begin
  if AUndo is THCUndoGroupEnd then  // 组结束(无Actions)
  begin
    if AUndo.IsUndo then  // 组撤销(无Action)
    begin
      if FUndoGroupCount = 0 then  // 组撤销开始
      begin
        vUndoList := GetUndoList;
        FFormatFirstItemNo := (vUndoList[vUndoList.CurGroupBeginIndex] as THCUndoGroupBegin).ItemNo;
        FFormatLastItemNo := (vUndoList[vUndoList.CurGroupEndIndex] as THCUndoGroupEnd).ItemNo;
        {for i := vUndoList.CurGroupEndIndex - 1 downto vUndoList.CurGroupBeginIndex + 1 do
        begin
          vUndo := vUndoList[i];
          for j := vUndo.Actions.Count - 1 downto 0 do
          begin
            if FFormatFirstItemNo > vUndo.Actions[j].ItemNo then
              FFormatFirstItemNo := vUndo.Actions[j].ItemNo;

            if FFormatLastItemNo < vUndo.Actions[j].ItemNo then
              FFormatLastItemNo := vUndo.Actions[j].ItemNo;
          end;
        end;}
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
        ReFormatData(FFormatFirstDrawItemNo, FFormatLastItemNo + FItemAddCount, FItemAddCount);

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
        ReFormatData(FFormatFirstDrawItemNo, FFormatLastItemNo + FItemAddCount, FItemAddCount);

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
        FFormatFirstItemNo := (AUndo as THCUndoGroupBegin).ItemNo;
        FFormatFirstDrawItemNo := Items[FFormatFirstItemNo].FirstDItemNo;
        FFormatLastItemNo := FFormatFirstItemNo;
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
      FFormatFirstItemNo := GetParaFirstItemNo(GetActionAffect(AUndo.Actions.Last));
      FFormatLastItemNo := GetParaLastItemNo(GetActionAffect(AUndo.Actions.First));
    end
    else
    begin
      FFormatFirstItemNo := GetParaFirstItemNo(GetActionAffect(AUndo.Actions.First));
      FFormatLastItemNo := GetParaLastItemNo(GetActionAffect(AUndo.Actions.Last));
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
    ReFormatData(FFormatFirstDrawItemNo, FFormatLastItemNo + FItemAddCount, FItemAddCount);

    if vCaretDrawItemNo < 0 then
      vCaretDrawItemNo := GetDrawItemNoByOffset(vCaretItemNo, vCaretOffset)
    else
    if vCaretDrawItemNo > Self.DrawItems.Count - 1 then
      vCaretDrawItemNo := Self.DrawItems.Count - 1;

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

procedure THCUndoData.LoadItemFromStreamAlone(const AStream: TStream;
  var AItem: THCCustomItem);
var
  vFileExt: string;
  viVersion: Word;
  vLang: Byte;
  vStyleNo, vParaNo: Integer;
  vTextStyle: THCTextStyle;
  vParaStyle: THCParaStyle;
begin
  AStream.Position := 0;
  _LoadFileFormatAndVersion(AStream, vFileExt, viVersion, vLang);  // 文件格式和版本
  if (vFileExt <> HC_EXT) and (vFileExt <> 'cff.') then
    raise Exception.Create('加载失败，不是' + HC_EXT + '文件！');

  AStream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));

  if not Assigned(AItem) then
    AItem := CreateItemByStyle(vStyleNo);

  AItem.LoadFromStream(AStream, nil, viVersion);

  if vStyleNo > THCStyle.Null then
  begin
    vTextStyle := THCTextStyle.Create;
    try
      vTextStyle.LoadFromStream(AStream, viVersion);
      vStyleNo := Style.GetStyleNo(vTextStyle, True);
      AItem.StyleNo := vStyleNo;
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

  AItem.ParaNo := vParaNo;
end;

procedure THCUndoData.Redo(const ARedo: THCCustomUndo);
begin
  DoUndoRedo(ARedo);
end;

procedure THCUndoData.SaveItemToStreamAlone(const AItem: THCCustomItem;
  const AStream: TStream);
begin
  _SaveFileFormatAndVersion(AStream);
  AItem.SaveToStream(AStream);
  if AItem.StyleNo > THCStyle.Null then
    Style.TextStyles[AItem.StyleNo].SaveToStream(AStream);

  Style.ParaStyles[AItem.ParaNo].SaveToStream(AStream);
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
      vItemAction := vUndo.ActionAppend(uatDeleteItem, AItemNo, AOffset) as THCItemUndoAction;
      SaveItemToStreamAlone(Items[AItemNo], vItemAction.ItemStream);
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
      vTextAction := vUndo.ActionAppend(uatDeleteText, AItemNo, AOffset) as THCTextUndoAction;
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
      vTextAction := vUndo.ActionAppend(uatDeleteBackText, AItemNo, AOffset) as THCTextUndoAction;
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
      vItemAction := vUndo.ActionAppend(uatInsertItem, AItemNo, AOffset) as THCItemUndoAction;
      SaveItemToStreamAlone(Items[AItemNo], vItemAction.ItemStream);
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
      vTextAction := vUndo.ActionAppend(uatInsertText, AItemNo, AOffset) as THCTextUndoAction;
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
      vItemAction := vUndo.ActionAppend(uatItemMirror, AItemNo, AOffset) as THCItemUndoAction;
      SaveItemToStreamAlone(Items[AItemNo], vItemAction.ItemStream);
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
      vUndo.ActionAppend(uatItemSelf, AItemNo, AOffset);
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
