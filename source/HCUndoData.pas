{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{         ֧�ֳ������ָ��������ĵ��������Ԫ          }
{                                                       }
{*******************************************************}

unit HCUndoData;

interface

uses
  Windows, Classes, HCCommon, HCUndo, HCFormatData, HCItem, HCStyle;

type
  THCUndoData = class(THCFormatData)  // ֧�ֳ����ָ����ܵ�Data
  private
    FFormatFirstItemNo, FFormatFirstDrawItemNo, FFormatLastItemNo,
      FUndoGroupCount, FItemAddCount: Integer;
    FForceClearExtra: Boolean;  // ��ҳ�����ǿ�������ʽ������ķ�ҳƫ����
    procedure DoUndoRedo(const AUndo: THCCustomUndo);
  protected
    function GetUndoList: THCUndoList; override;

    { �����ָ���ط���+ }
    procedure Undo_New;
    procedure Undo_GroupBegin(const AItemNo, AOffset: Integer);
    procedure Undo_GroupEnd(const AItemNo, AOffset: Integer);

    /// <summary> ɾ��Text </summary>
    /// <param name="AItemNo">��������ʱ��ItemNo</param>
    /// <param name="AOffset">ɾ������ʼλ��</param>
    /// <param name="AText"></param>
    procedure UndoAction_DeleteBackText(const AItemNo, AOffset: Integer; const AText: string);
    procedure UndoAction_DeleteText(const AItemNo, AOffset: Integer; const AText: string);
    procedure UndoAction_InsertText(const AItemNo, AOffset: Integer; const AText: string);
    /// <summary> ֱ���滻TextItem��Text </summary>
    procedure UndoAction_SetItemText(const AItemNo, AOffset: Integer; const ANewText: string);

    /// <summary> ɾ��ָ����Item </summary>
    /// <param name="AItemNo">��������ʱ��ItemNo</param>
    /// <param name="AOffset">��������ʱ��Offset</param>
    procedure UndoAction_DeleteItem(const AItemNo, AOffset: Integer);

    /// <summary> ����Item��ָ��λ�� </summary>
    /// <param name="AItemNo">��������ʱ��ItemNo</param>
    /// <param name="AOffset">��������ʱ��Offset</param>
    procedure UndoAction_InsertItem(const AItemNo, AOffset: Integer);
    procedure UndoAction_ItemStyle(const AItemNo, AOffset, ANewStyleNo: Integer);
    procedure UndoAction_ItemParaNo(const AItemNo, AOffset, ANewParaNo: Integer);

    /// <summary> �޸�Item�Ķ���ʼ����(�޸�ǰ����) </summary>
    /// <param name="AItemNo">Ҫ�޸ĵ�ItemNo</param>
    /// <param name="AOffset">�޸�ʱ���ڵ�ƫ��λ��</param>
    /// <param name="ANewParaFirst">�µĶ�������</param>
    procedure UndoAction_ItemParaFirst(const AItemNo, AOffset: Integer; const ANewParaFirst: Boolean);

    /// <summary> �޸�Item�ķ�ҳ����(�޸�ǰ����) </summary>
    procedure UndoAction_ItemPageBreak(const AItemNo, AOffset: Integer; const ANewPageBreak: Boolean);

    procedure UndoAction_ItemSelf(const AItemNo, AOffset: Integer);
    procedure UndoAction_ItemMirror(const AItemNo, AOffset: Integer);
    { �����ָ���ط���- }
  public
    constructor Create(const AStyle: THCStyle); override;
    procedure Clear; override;
    procedure Undo(const AUndo: THCCustomUndo); virtual;
    procedure Redo(const ARedo: THCCustomUndo); virtual;
    procedure UndoItemMirror(const AItemNo, AOffset: Integer);
    // Item��������Ͷ�ȡ�¼�
    procedure SaveItemToStreamAlone(const AStream: TStream; const AItem: THCCustomItem);

    /// <summary> �����м���һ��Item�����ItemΪnil����ᴴ���������� </summary>
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
        vCaretOffset := vAction.Offset - 1;  // �� + vLen - 1����ֹOffset������ǰCaretDrawItem��Χ
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

      if AIsUndo then  // ����
      begin
        vItem := nil;
        LoadItemFromStreamAlone(vAction.ItemStream, vItem);
        Items.Insert(vAction.ItemNo, vItem);
        Inc(FItemAddCount);

        vCaretOffset := vAction.Offset;
      end
      else  // ����
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

      if AIsUndo then  // ����
      begin
        if vCaretItemNo < Items.Count - 1 then  // �������һ��
        begin
          if Items[vCaretItemNo].ParaFirst then  // ����ɾ���ˣ����Ϊ��һ����ʼ
          begin
            vCaretOffset := 0;
            vCaretDrawItemNo := Items[vCaretItemNo + 1].FirstDItemNo;
          end
          else  // ɾ���Ĳ��Ƕ���
          //if Items[vCaretItemNo + 1].ParaFirst then  // ��һ���Ƕ��ף���걣����ͬ�����
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
        if vCaretItemNo > 0 then  // ���ǵ�һ��
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
      else  // ����
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

  /// <summary> ���ݳ���/�ָ����¼����ͼ���ǰһ��ItemNo�Ƿ���Ҫ��ʽ�� </summary>
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
          if AUndo.IsUndo then  // �ǳ���
          begin
            if Result > 0 then
              Dec(Result);
          end
          else  // �ǻָ�
          begin
            if Result > Items.Count - 1 then
              Dec(Result);
          end;
        end;

      actItemProperty:
        begin
          vPropAction := AAction as THCItemPropertyUndoAction;
          if (vPropAction.ItemProperty = TItemProperty.uipParaFirst)  // �������Ա仯
            or (vPropAction.ItemProperty = TItemProperty.uipPageBreak)  // ��ҳ���Ա仯
            or (vPropAction.ItemProperty = TItemProperty.uipStyleNo)
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

  /// <summary> ���ݳ���/�ָ����¼����ͼ����һ��ItemNo�Ƿ���Ҫ��ʽ�� </summary>
  function GetActionAffectLast(const AAction: THCCustomUndoAction): Integer;
  var
    vPropAction: THCItemPropertyUndoAction;
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

      actItemProperty:
        begin
          vPropAction := AAction as THCItemPropertyUndoAction;
          if (vPropAction.ItemProperty = TItemProperty.uipParaFirst)  // �������Ա仯
            or (vPropAction.ItemProperty = TItemProperty.uipPageBreak)  // ��ҳ���Ա仯
            or (vPropAction.ItemProperty = TItemProperty.uipStyleNo)
          then
          begin
            if Result < Items.Count - 1 then
              Inc(Result);
          end;
        end;
    end;

    if Result > Items.Count - 1 then
      Result := Items.Count - 1;
  end;

  procedure CalcAffectRange;
  var
    i, vFirstNo, vLastNo, vIncCount: Integer;
  begin
    vIncCount := 0;
    FFormatFirstItemNo := GetActionAffectFirst(AUndo.Actions.First);
    FFormatLastItemNo := GetActionAffectLast(AUndo.Actions.First);
    if AUndo.Actions.First.Tag = actInsertItem then
      Inc(vIncCount);

    for i := 1 to AUndo.Actions.Count - 1 do
    begin
      vFirstNo := GetActionAffectFirst(AUndo.Actions[i]);
      vLastNo := GetActionAffectLast(AUndo.Actions[i]);
      if AUndo.Actions[i].Tag = actInsertItem then
        Inc(vIncCount);

      if FFormatFirstItemNo > vFirstNo then
         FFormatFirstItemNo := vFirstNo;

      if FFormatLastItemNo < vLastNo then
        FFormatLastItemNo := vLastNo;
    end;

    if FFormatFirstItemNo < 0 then
      FFormatFirstItemNo := 0;

    if AUndo.IsUndo then
      FFormatLastItemNo := FFormatLastItemNo + vIncCount;

    if FFormatLastItemNo > Items.Count - 1 then
      FFormatLastItemNo := Items.Count - 1;
  end;

var
  i, vItemNo, vCaretDIItem: Integer;
  vUndoList: THCUndoList;
begin
  FForceClearExtra := False;

  if AUndo is THCUndoGroupEnd then  // �����(��Actions)
  begin
    if AUndo.IsUndo then  // �鳷��(��Action)
    begin
      if FUndoGroupCount = 0 then  // �鳷����ʼ
      begin
        vUndoList := GetUndoList;
        FFormatFirstItemNo := (vUndoList[vUndoList.CurGroupBeginIndex] as THCUndoGroupBegin).ItemNo;
        FFormatLastItemNo := (vUndoList[vUndoList.CurGroupEndIndex] as THCUndoGroupEnd).ItemNo;

        // ��������0��1��һ�У�1ɾ�����ֲ���2�������е�һ�������0�ϲ�����2�����У�
        // ����ʱFFormatFirstItemNoΪ1�Ӷ�FFormatFirstDrawItemNoΪ1������ʱ1Ϊ���ף�
        // ��������ʽ���ᰴ�����׿�ʼ������ԭ�������1�������С����Ա�������1��
        if FFormatFirstItemNo > 0 then
          Dec(FFormatFirstItemNo);

        // ���FFormatLastItemNo�Ǵ��ĵ��ʼ�������ӵģ�����ʱҪ��ʽ����ԭ��ͷ����������ֱ����һ��
        if FFormatLastItemNo < Items.Count - 1 then
          Inc(FFormatLastItemNo);

        if FFormatFirstItemNo <> FFormatLastItemNo then
        begin
          FFormatFirstItemNo := GetParaFirstItemNo(FFormatFirstItemNo);  // ȡ�ε�һ��Ϊ��ʼ
          FFormatFirstDrawItemNo := Items[FFormatFirstItemNo].FirstDItemNo;
          FFormatLastItemNo := GetParaLastItemNo(FFormatLastItemNo);  // ȡ�����һ��Ϊ����
        end
        else
          GetFormatRange(FFormatFirstItemNo, 1, FFormatFirstDrawItemNo, FFormatLastItemNo);

        FormatPrepare(FFormatFirstDrawItemNo, FFormatLastItemNo);

        SelectInfo.Initialize;
        Self.InitializeField;
        FItemAddCount := 0;
      end;

      Inc(FUndoGroupCount);  // �����鳷������
    end
    else  // ��ָ�����
    begin
      Dec(FUndoGroupCount);  // ������ָ�����

      if FUndoGroupCount = 0 then  // ��ָ�����
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
  if AUndo is THCUndoGroupBegin then  // �鿪ʼ
  begin
    if AUndo.IsUndo then  // �鳷��(��Action)
    begin
      Dec(FUndoGroupCount);  // ���ٳ�������

      if FUndoGroupCount = 0 then  // �鳷������
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
    else  // ��ָ�(��Action)
    begin
      if FUndoGroupCount = 0 then  // ��ָ���ʼ
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

        if FFormatLastItemNo > Items.Count - 1 then  // ��ֹ��������Item�ĳ�����ָ�����Խ��
          FFormatLastItemNo := Items.Count - 1;

        FFormatFirstDrawItemNo := GetFormatFirstDrawItem(Items[FFormatFirstItemNo].FirstDItemNo);

        {FFormatFirstItemNo := (AUndo as THCUndoGroupBegin).ItemNo;
        FFormatFirstDrawItemNo := GetFormatFirstDrawItem(Items[FFormatFirstItemNo].FirstDItemNo);

        vUndoList := GetUndoList;
        FFormatLastItemNo := (vUndoList[vUndoList.CurGroupEndIndex] as THCUndoGroupEnd).ItemNo;
        if FFormatLastItemNo > Items.Count - 1 then  // ��ֹ��������Item�ĳ�����ָ�����Խ��
          Dec(FFormatLastItemNo);   }

        FormatPrepare(FFormatFirstDrawItemNo, FFormatLastItemNo);

        SelectInfo.Initialize;
        Self.InitializeField;
        FItemAddCount := 0;
      end;

      Inc(FUndoGroupCount);  // ������ָ�����
    end;

    Exit;
  end;

  if AUndo.Actions.Count = 0 then Exit;  // �������û��Action���ɴ˴�����

  if FUndoGroupCount = 0 then
  begin
    SelectInfo.Initialize;
    Self.InitializeField;
    FItemAddCount := 0;
    vCaretDrawItemNo := (AUndo as THCDataUndo).CaretDrawItemNo;

    CalcAffectRange;
    FFormatLastItemNo := GetParaLastItemNo(FFormatLastItemNo);
    FFormatFirstDrawItemNo := GetFormatFirstDrawItem(FFormatFirstItemNo);
    FormatPrepare(FFormatFirstDrawItemNo, FFormatLastItemNo);
  end;

  if AUndo.IsUndo then  // ����
  begin
    for i := AUndo.Actions.Count - 1 downto 0 do
      DoUndoRedoAction(AUndo.Actions[i], True);
  end
  else  // ����
  begin
    for i := 0 to AUndo.Actions.Count - 1 do
      DoUndoRedoAction(AUndo.Actions[i], False);
  end;

  //if Items.Count = 0 then
  //  DrawItems.Clear;  // ���ں���յ�Group�������м价�ڻ������Item�����

  if FUndoGroupCount = 0 then
  begin
    ReFormatData(FFormatFirstDrawItemNo, FFormatLastItemNo + FItemAddCount,
      FItemAddCount, FForceClearExtra);

    vCaretDIItem := GetDrawItemNoByOffset(vCaretItemNo, vCaretOffset);  // ��Ϊ���Action��һ��ÿ��������Ч��CaretDrawItem��������Ҫ���¼���һ��
    if (vCaretDrawItemNo < 0) or (vCaretDrawItemNo > Self.DrawItems.Count - 1) then
      vCaretDrawItemNo := vCaretDIItem
    else
    if vCaretDIItem <> vCaretDrawItemNo then
    begin
      if (DrawItems[vCaretDrawItemNo].ItemNo = vCaretItemNo)
        and (DrawItems[vCaretDrawItemNo].CharOffs = vCaretOffset)
      then  // ����

      else
        vCaretDrawItemNo := vCaretDIItem;  // ����
    end;

    CaretDrawItemNo := vCaretDrawItemNo;

    Style.UpdateInfoReCaret;
    Style.UpdateInfoRePaint;
  end;

  // Ϊ���Ч�ʣ��鳷����ָ�ʱ��ֻ�����һ��(��ͷ��β)���ٽ��Ƹ�ʽ���ͼ���ҳ��
  // ������Ҫÿ���ĳ�����ָ�����¼SelectInfo���Ա����һ��(��ͷ��β)����ʽ����
  // ��ҳʱ�б䶯ǰ�����λ��
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
  vLang, vSType: Byte;
  vStyleNo, vParaNo: Integer;
  vStyle: THCStyle;
  vTextStyle: THCTextStyle;
  vParaStyle: THCParaStyle;
begin
  AStream.Position := 0;
  _LoadFileFormatAndVersion(AStream, vFileExt, viVersion, vLang);  // �ļ���ʽ�Ͱ汾
  if (vFileExt <> HC_EXT) and (vFileExt <> 'cff.') then
    raise Exception.Create('����ʧ�ܣ�����' + HC_EXT + '�ļ���');

  if viVersion > 59 then
  begin
    AStream.ReadBuffer(vSType, 1);
    if vSType <> HC_STREAM_ITEM then  // ����ITEM�ļ���
      Exit;
  end;

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
var
  vSType: Byte;
begin
  _SaveFileFormatAndVersion(AStream);
  vSType := HC_STREAM_ITEM;
  AStream.WriteBuffer(vSType, 1);
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
