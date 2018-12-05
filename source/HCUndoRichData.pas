{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{             文档撤销、恢复相关操作单元                }
{                                                       }
{*******************************************************}

unit HCUndoRichData;

interface

uses
  System.Classes, HCCommon, HCUndo, HCCustomRichData, HCItem, HCStyle;

type
  THCUndoRichData = class(THCCustomRichData)
  private
    FFormatFirstItemNo, FFormatLastItemNo, FUndoGroupCount, FItemAddCount: Integer;
    procedure DoUndoRedo(const AUndo: THCCustomUndo);
  public
    constructor Create(const AStyle: THCStyle); override;
    procedure Undo(const AUndo: THCCustomUndo); override;
    procedure Redo(const ARedo: THCCustomUndo); override;
  end;

implementation

uses
  HCRectItem;

{ THCUndoRichData }

constructor THCUndoRichData.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  FUndoGroupCount := 0;
  FItemAddCount := 0;
end;

procedure THCUndoRichData.DoUndoRedo(const AUndo: THCCustomUndo);
var
  vCaretItemNo, vCaretOffset, vCaretDrawItemNo: Integer;

  procedure DoUndoRedoAction(const AAction: THCCustomUndoAction;
    const AIsUndo: Boolean);

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
        vItem := LoadItemFromStreamAlone(vAction.ItemStream);
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
        vItem := LoadItemFromStreamAlone(vAction.ItemStream);
        Items.Insert(vAction.ItemNo, vItem);
        Inc(FItemAddCount);

        vCaretItemNo := vAction.ItemNo;
        if Items[vCaretItemNo].StyleNo > THCStyle.Null then
          vCaretOffset := Items[vCaretItemNo].Length
        else
          vCaretOffset := OffsetAfter;

        vCaretDrawItemNo := (AUndo as THCDataUndo).CaretDrawItemNo + 1;
        if vCaretDrawItemNo > Self.DrawItems.Count - 1 then
          vCaretDrawItemNo := Self.DrawItems.Count - 1;
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

      case vAction.ItemProperty of
        uipStyleNo: ;
        uipParaNo: ;

        uipParaFirst:
          begin
            vItem := Items[vAction.ItemNo];
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
      if AUndo.IsUndo then
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
      if AUndo.IsUndo then
        LoadItemFromStreamAlone(vAction.ItemStream, vItem)
      else
        LoadItemFromStreamAlone(vAction.ItemStream, vItem);
    end;

  begin
    case AAction.Tag of
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
    end;
  end;

var
  i, j: Integer;
  vUndoList: THCUndoList;
  vUndo: THCCustomUndo;
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
          FFormatLastItemNo := GetParaLastItemNo(FFormatLastItemNo);  // 取段最后一个为结束
        end
        else
          GetReformatItemRange(FFormatFirstItemNo, FFormatLastItemNo, FFormatFirstItemNo, 0);

        _FormatItemPrepare(FFormatFirstItemNo, FFormatLastItemNo);

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
        _ReFormatData(FFormatFirstItemNo, FFormatLastItemNo + FItemAddCount, FItemAddCount);

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
        _ReFormatData(FFormatFirstItemNo, FFormatLastItemNo + FItemAddCount, FItemAddCount);

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
        FFormatLastItemNo := FFormatFirstItemNo;
        _FormatItemPrepare(FFormatFirstItemNo, FFormatLastItemNo);

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
      if FFormatLastItemNo > Items.Count - 1 then
        FFormatLastItemNo := Items.Count - 1;
    end
    else
    begin
      FFormatFirstItemNo := GetParaFirstItemNo(GetActionAffect(AUndo.Actions.First));
      FFormatLastItemNo := GetParaLastItemNo(GetActionAffect(AUndo.Actions.Last));
    end;

    _FormatItemPrepare(FFormatFirstItemNo, FFormatLastItemNo);
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
    _ReFormatData(FFormatFirstItemNo, FFormatLastItemNo + FItemAddCount, FItemAddCount);

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

procedure THCUndoRichData.Redo(const ARedo: THCCustomUndo);
begin
  DoUndoRedo(ARedo);
end;

procedure THCUndoRichData.Undo(const AUndo: THCCustomUndo);
begin
  DoUndoRedo(AUndo);
end;

end.
