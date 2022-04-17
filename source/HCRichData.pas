{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{             ֧�ָ�ʽ���ĵ��������Ԫ                }
{                                                       }
{*******************************************************}

{******************* �����޸�˵�� ***********************
201807311101 �����ײ������ݺ�����ƣ�����ʱ��û�иı�ItemNo��Offset�����¹��
             �����»ص�����
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

  THCRichData = class(THCUndoData)  // ���ı������࣬����Ϊ������ʾ���ı���Ļ���
  strict private
    /// <summary> ����������(���ļ��Ի���˫���ļ���ᴥ��MouseMouse��MouseUp) </summary>
    FMouseLBDowning,
    /// <summary> ���˫��(����˫���Զ�ѡ�У��������ѡ�е�����) </summary>
    FMouseLBDouble,
    FMouseDownReCaret,
    FMouseMoveRestrain  // ��������Item��Χ��MouseMove����ͨ��Լ�������ҵ���
      : Boolean;

    FMouseDownX, FMouseDownY: Integer;

    FMouseDownItemNo,
    FMouseDownItemOffset,
    FMouseMoveItemNo,
    FMouseMoveItemOffset,
    FMouseMoveDrawItemNo,

    FSelectSeekNo,
    FSelectSeekOffset  // ѡ�в���ʱ���α�
      : Integer;

    FReadOnly,
    FSelecting, FDraging: Boolean;
    FCheckEmptyItem: Boolean;

    FOnItemResized: TDataItemNoEvent;
    FOnItemMouseDown, FOnItemMouseUp: TItemMouseEvent;
    FOnDrawItemMouseMove: TDrawItemMouseEvent;
    FOnCreateItem: TNotifyEvent;  // �½���Item(Ŀǰ��Ҫ��Ϊ�˴��ֺ����������뷨����Ӣ��ʱ�ۼ��Ĵ���)
    FOnAcceptAction: TDataActionEvent;

    /// <summary> Shift��������ʱ����������ݰ���λ������ѡ��Χ </summary>
    /// <param name="AMouseDonwItemNo"></param>
    /// <param name="AMouseDownItemOffset"></param>
    function SelectByMouseDownShift(var AMouseDownItemNo, AMouseDownItemOffset: Integer): Boolean;

    /// <summary> ��ʼ��Ϊֻ��һ����Item��Data</summary>
    procedure SetEmptyData;

    /// <summary> Dataֻ�п���Itemʱ����Item(�����滻��ǰ����Item�����) </summary>
    function EmptyDataInsertItem(const AItem: THCCustomItem): Boolean;

    /// <summary> �Ƿ�����ѡ�У��ж��ı���Rect���ѡ��ʱ��Rectȫѡ�С���ѡ���Ƿ���true��</summary>
    function SelectPerfect: Boolean;

    /// <summary> ��ǰTextItem�ڲ��仯�����¸�ʽ������������ظ����룩 </summary>
    function TextItemAction(const AItemNo: Integer; const AAction: TTextItemActionEvent): Boolean;

    /// <summary> ��ǰRectItem�ڲ��仯�����¸�ʽ�����������ظ����룩 </summary>
    function RectItemAction(const AItemNo: Integer; const AAction: TRectItemActionEvent): Boolean;

    /// <summary> ��ʼ���������ֶ� </summary>
    procedure InitializeMouseField;

    /// <summary> ������ɺ�������λ���Ƿ���ѡ�з�Χ��ʼ </summary>
    function IsSelectSeekStart: Boolean;
  protected
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;

    procedure ReSetSelectAndCaret(const AItemNo, AOffset: Integer;
      const ANextWhenMid: Boolean = False); override;

    /// <summary> �Ƿ����ָ�����¼� </summary>
    function DoAcceptAction(const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean; virtual;
    procedure DoDrawItemMouseMove(const AData: THCCustomData; const AItemNo, AOffset,
      ADrawItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    /// <summary> ���ڴ���������Items�󣬼�鲻�ϸ��Item��ɾ�� </summary>
    function CheckInsertItemCount(const AStartNo, AEndNo: Integer): Integer; virtual;
    procedure DoItemMouseLeave(const AItemNo: Integer); virtual;
    procedure DoItemMouseEnter(const AItemNo: Integer); virtual;
    /// <summary> ResizeItem��������¼���������¼�(�ɿ������Ų�Ҫ����ҳ��) </summary>
    procedure DoItemResized(const AItemNo: Integer); virtual;
    function GetHeight: Cardinal; virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;

    /// <summary> ������ʼ����λ�ã��ж���ȷ��ѡ��λ�ò�������� </summary>
    /// <param name="ADrawItemNo">��괦��DrawItem(��ʱ������)</param>
    /// <param name="AStartItemNo"></param>
    /// <param name="AStartItemOffset"></param>
    /// <param name="AEndItemNo"></param>
    /// <param name="AEndItemNoOffset"></param>
    procedure AdjustSelectRange(var AStartItemNo, AStartItemOffset, AEndItemNo, AEndItemNoOffset: Integer);

    property MouseMoveDrawItemNo: Integer read FMouseMoveDrawItemNo;
  public
    constructor Create(const AStyle: THCStyle); override;
    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; override;

    /// <summary> ��Data�����Ƿ�ɱ༭ </summary>
    function CanEdit: Boolean; override;

    procedure Clear; override;
    // ѡ������Ӧ����ʽ
    procedure ApplySelectTextStyle(const AMatchStyle: THCStyleMatch); override;
    procedure ApplySelectParaStyle(const AMatchStyle: THCParaMatch); override;
    procedure ApplyTableCellAlign(const AAlign: THCContentAlign); override;

    function DisSelect: Boolean; override;

    /// <summary> �Ƿ�ɹ�ɾ����ѡ�����ݣ���ѡ�з���true�� </summary>
    /// <returns>True:��ѡ����ɾ���ɹ�</returns>
    function DeleteSelected: Boolean; override;

    /// <summary> ��ʼ������ֶκͱ��� </summary>
    procedure InitializeField; override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summary> �ڹ�괦����Item </summary>
    /// <param name="AItem"></param>
    /// <returns></returns>
    function InsertItem(const AItem: THCCustomItem): Boolean; overload; virtual;

    /// <summary> ��ָ����λ�ò���Item </summary>
    /// <param name="AIndex">����λ��</param>
    /// <param name="AItem">�����Item</param>
    /// <param name="AOffsetBefor">����ʱ��ԭλ��Itemǰ��(True)�����(False)</param>
    /// <returns></returns>
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem;
      const AOffsetBefor: Boolean = True): Boolean; overload; virtual;

    /// <summary> ֱ�����õ�ǰTextItem��Textֵ </summary>
    procedure SetActiveItemText(const AText: string);

    procedure KillFocus; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual;

    // Key����0��ʾ�˼�����Dataû�����κ�����
    procedure KeyPress(var Key: Char); virtual;

    // Key����0��ʾ�˼�����Dataû�����κ�����
    procedure KeyDown(var Key: Word; Shift: TShiftState;
      const APageBreak: Boolean = False); virtual;

    // Key����0��ʾ�˼�����Dataû�����κ�����
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;

    function DoInsertTextBefor(const AItemNo, AOffset: Integer;
      const AText: string): Boolean; virtual;
    //
    procedure DblClick(X, Y: Integer);
    procedure DeleteItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean);
    procedure DeleteActiveDataItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean);
    /// <summary> ���Data����ǰ </summary>
    /// <param name="ASrcData">ԴData</param>
    procedure AddData(const ASrcData: THCCustomData);

    /// <summary> �ڹ�괦���� </summary>
    function InsertBreak: Boolean;

    /// <summary> �ڹ�괦�����ַ���(�ɴ��س����з�) </summary>
    function InsertText(const AText: string): Boolean;

    /// <summary> �ڹ�괦����ָ�����еı�� </summary>
    function InsertTable(const ARowCount, AColCount: Integer): Boolean;
    function InsertImage(const AImage: TGraphic): Boolean;
    function InsertGifImage(const AFile: string): Boolean;

    /// <summary> �ڹ�괦����ֱ�� </summary>
    function InsertLine(const ALineHeight: Integer): Boolean;
    function SetActiveImage(const AImageStream: TStream): Boolean;
    function ActiveTableResetRowCol(const ARowCount, AColCount: Integer): Boolean;
    function TableInsertRowAfter(const ARowCount: Integer): Boolean;
    function TableInsertRowBefor(const ARowCount: Integer): Boolean;
    function ActiveTableDeleteCurRow: Boolean;
    function ActiveTableSplitCurRow: Boolean;
    function ActiveTableSplitCurCol: Boolean;
    function TableInsertColAfter(const AColCount: Integer): Boolean;
    function TableInsertColBefor(const AColCount: Integer): Boolean;
    function ActiveTableDeleteCurCol: Boolean;
    function MergeTableSelectCells: Boolean;
    function TableApplyContentAlign(const AAlign: THCContentAlign): Boolean;

    /// <summary> ActiveItem������Ӧ�价��(���ⲿֱ���޸�Item���Ժ����º���ǰ��Item�������) </summary>
    procedure ActiveItemReAdaptEnvironment;

    /// <summary> ȡ������(����ҳü��ҳ�š������л�ʱԭ�����ȡ��) </summary>
    procedure DisActive;

    function GetHint: string;

    property MouseDownItemNo: Integer read FMouseDownItemNo;
    property MouseDownItemOffset: Integer read FMouseDownItemOffset;
    property MouseMoveItemNo: Integer read FMouseMoveItemNo;
    property MouseMoveItemOffset: Integer read FMouseMoveItemOffset;
    property MouseMoveRestrain: Boolean read FMouseMoveRestrain;
    property HotDrawItemNo: Integer read FMouseMoveDrawItemNo;

    property Height: Cardinal read GetHeight;  // ʵ�����ݵĸ�
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
  HCFloatBarCodeItem, HCButtonItem, HCAnnotateItem;

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
      THCStyle.CheckBox: Result := THCCheckBoxItem.Create(Self, '��ѡ��', False);
      THCStyle.Gif: Result := THCGifItem.Create(Self, 1, 1);
      THCStyle.Edit: Result := THCEditItem.Create(Self, '');
      THCStyle.Combobox: Result := THCComboboxItem.Create(Self, '');
      THCStyle.Button: Result := THCButtonItem.Create(Self, '');
      THCStyle.QRCode: Result := THCQRCodeItem.Create(Self, '');
      THCStyle.BarCode: Result := THCBarCodeItem.Create(Self, '');
      THCStyle.Fraction: Result := THCFractionItem.Create(Self, '', '');
      THCStyle.DateTimePicker: Result := THCDateTimePicker.Create(Self, Now);
      THCStyle.RadioGroup: Result := THCRadioGroup.Create(Self);
      THCStyle.SupSubScript: Result := THCSupSubScriptItem.Create(Self, '', '');
      THCStyle.Annotate: Result := THCAnnotateItem.Create(Self);
      // FloatItem
      THCStyle.FloatLine: Result := THCFloatLineItem.Create(Self);
      THCStyle.FloatBarCode: Result := THCFloatBarCodeItem.Create(Self);
    else
      raise Exception.Create('δ�ҵ����� ' + IntToStr(AStyleNo) + ' ��Ӧ�Ĵ���Item���룡');
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
  else  // TextItem˫��ʱ���ݹ�괦���ݣ�ѡ�з�Χ
  if Items[vItemNo].Length > 0 then
  begin
    vText := GetDrawItemText(vDrawItemNo);  // DrawItem��Ӧ���ı�
    vItemOffset := vItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;  // ӳ�䵽DrawItem��

    if vItemOffset > 0 then  // ��괦��Char����
      vPosType := GetUnicodeCharType(vText[vItemOffset])
    else
      vPosType := GetUnicodeCharType(vText[1]);

    vStartOffset := 0;
    for i := vItemOffset - 1 downto 1 do  // ��ǰ��Char���Ͳ�һ����λ��
    begin
      if GetUnicodeCharType(vText[i]) <> vPosType then
      begin
        vStartOffset := i;
        Break;
      end;
    end;

    vEndOffset := Length(vText);
    for i := vItemOffset + 1 to Length(vText) do  // ������Char���Ͳ�һ����λ��
    begin
      if GetUnicodeCharType(vText[i]) <> vPosType then
      begin
        vEndOffset := i - 1;
        Break;
      end;
    end;

    Self.SelectInfo.StartItemNo := vItemNo;
    Self.SelectInfo.StartItemOffset := vStartOffset + DrawItems[vDrawItemNo].CharOffs - 1;

    if vStartOffset <> vEndOffset then  // ��ѡ�е��ı�
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

  vActiveItem := Self.GetActiveItem;

  if (vActiveItem.StyleNo < THCStyle.Null)  // ��ǰλ���� RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // ��������������
  then
  begin
    Undo_New;

    vRectItem := vActiveItem as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    vRectItem.DeleteActiveDataItems(AStartNo, AEndNo, AKeepPara);
    if vRectItem.IsFormatDirty then
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
    end
    else
      Self.FormatInit;
  end
  else
    DeleteItems(AStartNo, AEndNo, AKeepPara);
end;

procedure THCRichData.DeleteItems(const AStartNo, AEndNo: Integer; const AKeepPara: Boolean);
var
  i, vFormatFirstDrawItemNo, vFormatLastItemNo, vFormatDrawItemNo2, vDelCount: Integer;
  vItem: THCCustomItem;
  vStartParaFirst: Boolean;
begin
  if not CanEdit then Exit;
  if AEndNo < AStartNo then Exit;

  Self.InitializeField;

  GetFormatRange(AStartNo, 0, vFormatFirstDrawItemNo, vFormatLastItemNo);  // ͷ
  if (not AKeepPara) and (AEndNo < Items.Count - 1) and (Items[AEndNo + 1].ParaFirst) then
    GetFormatRange(AEndNo + 1, GetItemOffsetAfter(AEndNo + 1), vFormatDrawItemNo2, vFormatLastItemNo)
  else
    GetFormatRange(AEndNo, GetItemOffsetAfter(AEndNo), vFormatDrawItemNo2, vFormatLastItemNo);  // β

  if (Items[AStartNo].ParaFirst) and (vFormatFirstDrawItemNo > 0) then  // ����ɾ��ʱҪ����һ�����
  begin
    Dec(vFormatFirstDrawItemNo);
    vFormatFirstDrawItemNo := GetFormatFirstDrawItem(vFormatFirstDrawItemNo);  // �����׿�ʼ
  end;

  FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

  vStartParaFirst := Items[AStartNo].ParaFirst;
  vDelCount := AEndNo - AStartNo + 1;
  Undo_New;
  for i := AEndNo downto AStartNo do
  begin
    // if DoAcceptAction(i, 0, THCAction.actDeleteItem) then  // DeleteItemsĿǰ�����ں�̨ɾ��������û����Action�Ŀ���
    UndoAction_DeleteItem(i, 0);
    Items.Delete(i);
  end;

  if Items.Count = 0 then  // ɾ��û���ˣ�����SetEmptyData����Ϊ����Undo
  begin
    vItem := CreateDefaultTextItem;
    Self.CurStyleNo := vItem.StyleNo;
    vItem.ParaFirst := True;
    Items.Add(vItem);
    Dec(vDelCount);
    UndoAction_InsertItem(0, 0);
  end
  else
  if vStartParaFirst then // ����ɾ����
  begin
    if (AStartNo < Items.Count - 1)  // ��һ���������һ��
      and (not Items[AStartNo].ParaFirst)  // ��һ�����Ƕ���(ͬ�λ������ݣ�����)
    then
    begin
      UndoAction_ItemParaFirst(AStartNo, 0, True);
      Items[AStartNo].ParaFirst := True;
    end
    else  // ��ɾ������
    if AKeepPara then  // ���ֶ�
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
  if AStartNo > 0 then  // ���Ǵӵ�1����ʼɾ��
    ReSetSelectAndCaret(AStartNo - 1)
  else  // �ӵ�һ����ʼɾ��
    ReSetSelectAndCaret(0, 0);  // ����õ����ڵ���ǰ�棬Ϊ������������׼��
end;

function THCRichData.DeleteSelected: Boolean;
var
  vDelCount, vFormatFirstItemNo, vFormatLastItemNo,
  vLen, vParaFirstItemNo, vParaLastItemNo: Integer;
  vStartItem, vEndItem, vNewItem: THCCustomItem;

  {$REGION 'ɾ��ȫѡ�еĵ���Item'}
  function DeleteItemSelectComplate: Boolean;
  begin
    Result := False;
    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actDeleteItem) then Exit;  // ����ɾ��

    UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
    Items.Delete(SelectInfo.StartItemNo);

    Inc(vDelCount);

    if (SelectInfo.StartItemNo > vFormatFirstItemNo)
      and (SelectInfo.StartItemNo < vFormatLastItemNo)
    then  // ȫѡ�е�Item����ʼ��ʽ���ͽ�����ʽ���м�
    begin
      vLen := Items[SelectInfo.StartItemNo - 1].Length;
      if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then  // ɾ��λ��ǰ��ɺϲ�
      begin
        UndoAction_InsertText(SelectInfo.StartItemNo - 1, vLen + 1, Items[SelectInfo.StartItemNo].Text);
        UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
        Items.Delete(SelectInfo.StartItemNo);
        Inc(vDelCount);

        SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
        SelectInfo.StartItemOffset := vLen;
      end
      else  // ɾ��λ��ǰ���ܺϲ��������Ϊǰһ������
      begin
        SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
        SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
      end;
    end
    else
    if SelectInfo.StartItemNo = vParaFirstItemNo then  // �ε�һ��ItemNo
    begin
      if vParaFirstItemNo = vParaLastItemNo then  // �ξ�һ��Itemȫɾ���ˣ������Item
      begin
        vNewItem := CreateDefaultTextItem;
        Self.CurStyleNo := vNewItem.StyleNo;  // ��ֹRectItemɾ�������ı���ǰ��ʽ����ȷ
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
    if SelectInfo.StartItemNo = vParaLastItemNo then  // �����һ��ItemNo
    begin
      {if vParaFirstItemNo = vParaLastItemNo then  // �ξ�һ��Itemȫɾ����,�����ߵ������
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
    else  // ȫѡ�е�Item����ʼ��ʽ���������ʽ�����ڶ���
    begin  // ����Ĵ���ᴥ����
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
        SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);  // ɾ��ѡ���ٲ���ʱ�����RectItem��ԭ��Offset����
      end;
    end;

    Result := True;
  end;
  {$ENDREGION}

var
  i, vFormatFirstDrawItemNo,
  vUnDeleteSeekItemNo  // ��ǰһ������ɾ����ɾ��ѡ����ɺ���"����"
    : Integer;
  vText: string;
  vSelectSeekStart,   // ѡ�з�Χ�α���ѡ����ʼ
  vSelStartComplate,  // ѡ�з�Χ�ڵ���ʼItemȫѡ����
  vSelEndComplate,    // ѡ�з�Χ�ڵĽ���Itemȫѡ����
  vSelStartParaFirst, // ѡ����ʼ�Ƕ���
  vStartDel,  // ѡ����ʼɾ����
  vEndDel     // ѡ�н���ɾ����
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

  if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actDeleteSelected) then Exit;

  vSelectSeekStart := IsSelectSeekStart;
  vDelCount := 0;
  Self.InitializeField;  // ɾ����ԭ��괦�����Ѿ�û����

  if (SelectInfo.EndItemNo < 0)
    and (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)
  then  // ѡ���������RectItem�ڲ�
  begin
    // ����䶯������RectItem�Ŀ�ȱ仯������Ҫ��ʽ���������һ��Item
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    if Self.FormatCount > 0 then
      vFormatFirstItemNo := SelectInfo.StartItemNo
    else
      vFormatFirstItemNo := DrawItems[vFormatFirstDrawItemNo].ItemNo;

    Undo_New;

    if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).IsSelectComplateTheory then  // ����ȫѡ��
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
  else  // ѡ�в��Ƿ�����RectItem�ڲ�
  begin
    vEndItem := Items[SelectInfo.EndItemNo];  // ѡ�н���Item

    if SelectInfo.EndItemNo = SelectInfo.StartItemNo then  // ѡ������ͬһ��Item
    begin
      Undo_New;

      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      if Self.FormatCount > 0 then
        vFormatFirstItemNo := SelectInfo.StartItemNo
      else
        vFormatFirstItemNo := DrawItems[vFormatFirstDrawItemNo].ItemNo;

      if vEndItem.IsSelectComplate then  // ��TextItemȫѡ����
      begin
        GetParaItemRang(SelectInfo.StartItemNo, vParaFirstItemNo, vParaLastItemNo);
        Result := DeleteItemSelectComplate;
      end
      else  // Item����ѡ��
      begin
        if vEndItem.StyleNo < THCStyle.Null then  // ͬһ��RectItem  ����ǰѡ�е�һ���֣�
        begin
          if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).MangerUndo then
            UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
          else
            UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

          (vEndItem as THCCustomRectItem).DeleteSelected;
        end
        else  // ͬһ��TextItem
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
    else  // ѡ�з����ڲ�ͬItem����ʼ(�����Ƕ���)ȫѡ�н�βûȫѡ����ʼûȫѡ��βȫѡ����ʼ��β��ûȫѡ
    begin
      vUnDeleteSeekItemNo := SelectInfo.EndItemNo;

      vFormatFirstItemNo := GetParaFirstItemNo(SelectInfo.StartItemNo);  // ȡ�ε�һ��Ϊ��ʼ
      vFormatFirstDrawItemNo := Items[vFormatFirstItemNo].FirstDItemNo;
      vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);  // ȡ�����һ��Ϊ������������ע������

      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vSelStartParaFirst := Items[SelectInfo.StartItemNo].ParaFirst;
      vSelStartComplate := Items[SelectInfo.StartItemNo].IsSelectComplate;  // ��ʼ�Ƿ�ȫѡ
      vSelEndComplate := Items[SelectInfo.EndItemNo].IsSelectComplate;  // ��β�Ƿ�ȫѡ

      Undo_New;

      vStartDel := False;
      vEndDel := False;
      // �ȴ���ѡ�н���Item
      if vEndItem.StyleNo < THCStyle.Null then  // RectItem
      begin
        if vSelEndComplate then  // ѡ��������棬��ȫѡ  SelectInfo.EndItemOffset = OffsetAfter
        begin
          if DoAcceptAction(SelectInfo.EndItemNo, SelectInfo.EndItemOffset, THCAction.actDeleteItem) then  // ����ɾ��
          begin
            UndoAction_DeleteItem(SelectInfo.EndItemNo, OffsetAfter);
            Items.Delete(SelectInfo.EndItemNo);
            vEndDel := True;
            Inc(vDelCount);
            Dec(vUnDeleteSeekItemNo);
          end
          else  // ������ɾ��
            vUnDeleteSeekItemNo := SelectInfo.EndItemNo;
        end
        else
        if SelectInfo.EndItemOffset = OffsetInner then  // ������
          (vEndItem as THCCustomRectItem).DeleteSelected
        else  // ����ǰ
        if Items[SelectInfo.EndItemNo].ParaFirst then  // ���ɾ������ѡ��������ǰ��ɾ���س�
        begin
          UndoAction_ItemParaFirst(SelectInfo.EndItemNo, 0, False);
          Items[SelectInfo.EndItemNo].ParaFirst := False;
        end;
      end
      else  // TextItem
      begin
        if vSelEndComplate then  // ��ѡ���ı�Item������� SelectInfo.EndItemOffset = vEndItem.Length
        begin
          if DoAcceptAction(SelectInfo.EndItemNo, SelectInfo.EndItemOffset, THCAction.actDeleteItem) then  // ����ɾ��
          begin
            UndoAction_DeleteItem(SelectInfo.EndItemNo, vEndItem.Length);
            Items.Delete(SelectInfo.EndItemNo);
            vEndDel := True;
            Inc(vDelCount);
            Dec(vUnDeleteSeekItemNo);
          end
          else
            vUnDeleteSeekItemNo := SelectInfo.EndItemNo;
        end
        else  // �ı��Ҳ���ѡ�н���Item���
        if DoAcceptAction(SelectInfo.EndItemNo, SelectInfo.EndItemOffset, THCAction.actBackDeleteText) then
        begin
          if (SelectInfo.EndItemOffset = 0) and Items[SelectInfo.EndItemNo].ParaFirst then  // ѡ�н����ڶ���
          begin
            UndoAction_ItemParaFirst(SelectInfo.EndItemNo, 0, False);
            Items[SelectInfo.EndItemNo].ParaFirst := False;
          end
          else
          begin
            UndoAction_DeleteBackText(SelectInfo.EndItemNo, 1, Copy(vEndItem.Text, 1, SelectInfo.EndItemOffset));
            // ����Item���µ�����
            vText := (vEndItem as THCTextItem).SubString(SelectInfo.EndItemOffset + 1,
              vEndItem.Length - SelectInfo.EndItemOffset);
            vEndItem.Text := vText;
          end;
        end
        else
          vUnDeleteSeekItemNo := SelectInfo.EndItemNo;
      end;

      // ɾ��ѡ����ʼItem��һ��������Item��һ��
      for i := SelectInfo.EndItemNo - 1 downto SelectInfo.StartItemNo + 1 do
      begin
        if DoAcceptAction(i, 0, THCAction.actDeleteItem) then  // ����ɾ��
        begin
          UndoAction_DeleteItem(i, 0);
          Items.Delete(i);

          Inc(vDelCount);
          Dec(vUnDeleteSeekItemNo);
        end
        else
          vUnDeleteSeekItemNo := i;
      end;

      vStartItem := Items[SelectInfo.StartItemNo];  // ѡ����ʼItem
      if vStartItem.StyleNo < THCStyle.Null then  // ��ʼ��RectItem
      begin
        if SelectInfo.StartItemOffset = OffsetBefor then  // ����ǰ
        begin
          if DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actDeleteItem) then  // ����ɾ��
          begin
            UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
            Items.Delete(SelectInfo.StartItemNo);
            vStartDel := True;
            Inc(vDelCount);
            Dec(vUnDeleteSeekItemNo);
          end
          else
            vUnDeleteSeekItemNo := SelectInfo.StartItemNo;

          {Ӱ����� 2020072601 ר�Ŷ���ʼɾ����λ�õĴ���
          if SelectInfo.StartItemNo > vFormatFirstItemNo then
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
          end;}
        end
        else
        if SelectInfo.StartItemOffset = OffsetInner then  // ������
          (vStartItem as THCCustomRectItem).DeleteSelected;
      end
      else  // ѡ����ʼ��TextItem
      begin
        if vSelStartComplate then  // ����ǰ��ʼȫѡ�� SelectInfo.StartItemOffset = 0
        begin
          if DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actDeleteItem) then  // ����ɾ��
          begin
            UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
            Items.Delete(SelectInfo.StartItemNo);
            vStartDel := True;
            Inc(vDelCount);
            Dec(vUnDeleteSeekItemNo);
          end
          else
            vUnDeleteSeekItemNo := SelectInfo.StartItemNo;
        end
        else
        if SelectInfo.StartItemOffset < vStartItem.Length then  // ���м�(�����ж��˰ɣ���Ϊѡ����ʼ��һ��β��󣬽�������һ���У�ɾ��ѡ��ʱҪɾ�����У�������Ҫ�ж�)
        begin
          if DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, THCAction.actBackDeleteText) then
          begin
            UndoAction_DeleteBackText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1,
              Copy(vStartItem.Text, SelectInfo.StartItemOffset + 1, vStartItem.Length - SelectInfo.StartItemOffset));
            vText := (vStartItem as THCTextItem).SubString(1, SelectInfo.StartItemOffset);
            vStartItem.Text := vText;  // ��ʼ���µ�����
          end
          else
            vUnDeleteSeekItemNo := SelectInfo.StartItemNo;
        end;
      end;

      // ������ʼɾ����ǰλ�� 2020072601
      //if vSelStartComplate and vSelEndComplate then
      if SelectInfo.EndItemNo - SelectInfo.StartItemNo + 1 = vDelCount then  // ѡ�е�Item��ɾ����
      begin
        if SelectInfo.StartItemNo = vFormatFirstItemNo then  // ѡ����ʼ�ڶ���ǰ
        begin
          if SelectInfo.EndItemNo = vFormatLastItemNo then  // ѡ�н����ڵ�ǰ�λ����ĳ�����(��������ȫɾ����)���������
          begin
            vNewItem := CreateDefaultTextItem;
            Self.CurStyleNo := vNewItem.StyleNo;  // ��ֹRectItemɾ�������ı���ǰ��ʽ����ȷ
            vNewItem.ParaFirst := True;
            Items.Insert(SelectInfo.StartItemNo, vNewItem);
            UndoAction_InsertItem(SelectInfo.StartItemNo, vNewItem.Length);

            Dec(vDelCount);
          end
          else  // ѡ�н������ڶ����
            Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst := True;  // ѡ�н���λ�ú���ĳ�Ϊ����
        end
        else
        if SelectInfo.EndItemNo = vFormatLastItemNo then  // �����ڶ����
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
        end
        else  // ѡ����ʼ����ʼ���м䣬ѡ�н����ڽ������м�
        begin
          vLen := Items[SelectInfo.StartItemNo - 1].Length;
          if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.EndItemNo - vDelCount + 1]) then  // ��ʼǰ��ͽ�������ɺϲ�
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
          else  // ��ʼǰ��ͽ������治�ܺϲ������ѡ����ʼ�ͽ�������ͬһ��
          begin
            if Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst then
            begin
              UndoAction_ItemParaFirst(SelectInfo.EndItemNo - vDelCount + 1, 0, False);
              Items[SelectInfo.EndItemNo - vDelCount + 1].ParaFirst := False;  // �ϲ����ɹ��Ͱ���
            end;
          end;
        end;
      end
      else  // ѡ�з�Χ�ڵ�Itemû��ɾ����
      begin
        if vStartDel then  // ��ʼɾ������
        begin
          if Items[vUnDeleteSeekItemNo].ParaFirst <> vSelStartParaFirst then
          begin
            UndoAction_ItemParaFirst(vUnDeleteSeekItemNo, 0, vSelStartParaFirst);
            Items[vUnDeleteSeekItemNo].ParaFirst := vSelStartParaFirst;
          end;
        end
        else
        if (not vEndDel) and (SelectInfo.StartItemNo + 1 = SelectInfo.EndItemNo - vDelCount) then  // ��ʼ�ͽ�����û��ɾ�������м�û�в���ɾ����
        begin
          if MergeItemText(Items[SelectInfo.StartItemNo], Items[SelectInfo.EndItemNo - vDelCount])  // ��ʼ�ͽ�������һ����
          then  // ѡ����ʼ������λ�õ�Item�ϲ��ɹ�
          begin
            UndoAction_InsertText(SelectInfo.StartItemNo,
              Items[SelectInfo.StartItemNo].Length - Items[SelectInfo.EndItemNo - vDelCount].Length + 1,
              Items[SelectInfo.EndItemNo - vDelCount].Text);

            UndoAction_DeleteItem(SelectInfo.EndItemNo - vDelCount, 0);
            Items.Delete(SelectInfo.EndItemNo - vDelCount);
            Inc(vDelCount);
          end
          else  // ѡ����ʼ������λ�õ�Item���ܺϲ�
          begin
            if SelectInfo.EndItemNo <> vFormatLastItemNo then  // ѡ�н������Ƕ����һ��
            begin
              if Items[SelectInfo.EndItemNo - vDelCount].ParaFirst then
              begin
                UndoAction_ItemParaFirst(SelectInfo.EndItemNo - vDelCount, 0, False);
                Items[SelectInfo.EndItemNo - vDelCount].ParaFirst := False;  // �ϲ����ɹ��Ͱ���
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

  if Items.Count > 0 then  // ҳü��Ԫ�ؼ�����л������Ĳ�����
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
    // ��ק���ʱ���
    FDraging := False;  // ��ק���
    //FMouseLBDowning := False;  // ��갴����ѡ���������ʱ����ѡ�У������ܸ�FMouseLBDowning״̬
    FSelecting := False;  // ׼����ѡ
    // Self.Initialize;  ����ᵼ��Mouse�¼��е�FMouseLBDowning�����Ա�ȡ����
    Style.UpdateInfoRePaint;
  end;

  Style.UpdateInfoReCaret;  // ѡ����ʼ��Ϣ������Ϊ-1
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
  Self.Change;
  if Assigned(FOnItemResized) then
    FOnItemResized(Self, AItemNo);
end;

function THCRichData.EmptyDataInsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := False;

  if (AItem.StyleNo > THCStyle.Null) and (AItem.Text = '') then Exit;  // ���в����ٲ������

  AItem.ParaNo := Items[0].ParaNo;  // �����ڱ���ϣ�AItem��StyleNo�Ǻͱ��һ������ʵ�ʵĵ�Ԫ��ʱҪ�Ե�Ԫ��ǰ��StyleNoΪ׼
  UndoAction_DeleteItem(0, 0);
  Items.Clear;
  DrawItems.Clear;
  AItem.ParaFirst := True;
  Items.Add(AItem);
  UndoAction_InsertItem(0, 0);
  SelectInfo.StartItemOffset := GetItemOffsetAfter(0);

  // ��FormatItemPrepare��ReFormatData_���ǵ�һ����DrawItemΪ-1��
  // ����FormatDataʱ_FormatItemToDrawItems��Inc��
  ReFormat;
  //ReSetSelectAndCaret(0);  // ��ֹ��պ��ʽ����ɺ�û��ѡ����ʼ���ʳ���
  Result := True;
end;

procedure THCRichData.Clear;
var
  vStyleNo, vParaNo: Integer;
begin
  InitializeField;
  vStyleNo := Items[0].StyleNo;
  vParaNo := Items[0].ParaNo;
  inherited Clear;
  if (vStyleNo > THCStyle.Null) and (vStyleNo < Self.Style.TextStyles.Count) then
    FCurStyleNo := vStyleNo;

  if vParaNo < Self.Style.ParaStyles.Count then
    FCurParaNo := vParaNo;

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

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      (ARectItem as THCImageItem).Image.LoadFromStream(AImageStream);
      Result := True;
    end);
end;

function THCRichData.ActiveTableDeleteCurCol: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).DeleteCurCol;
    end);
end;

function THCRichData.ActiveTableDeleteCurRow: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).DeleteCurRow;
    end);
end;

function THCRichData.ActiveTableResetRowCol(const ARowCount, AColCount: Integer): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).ResetRowCol(Self.Width, ARowCount, AColCount);
    end);
end;

function THCRichData.ActiveTableSplitCurCol: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).SplitCurCol;
    end);
end;

function THCRichData.ActiveTableSplitCurRow: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
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
      //vItem.ParaFirst := False;  // ��Ҫ���Ǻϲ�
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
  // ����������ǰѡ����ʼ��Item��βʱ����һ����ʼ�����ѡ���������һ����ʼʱ����һ������
  vLeftToRight := False;
  // ��¼ԭ��ѡ�з�Χ
  vOldStartItemNo := SelectInfo.StartItemNo;
  vOldEndItemNo := SelectInfo.EndItemNo;

  if AStartItemNo < AEndItemNo then  // ��ǰ����ѡ���ڲ�ͬ��Item
  begin
    vLeftToRight := True;

    if (Items[AStartItemNo].Length > 0)
      and (AStartItemOffset = GetItemOffsetAfter(AStartItemNo))
    then  // ��ʼ��Item����棬��Ϊ��һ��Item��ʼ
    begin
      if (AStartItemNo < Items.Count - 1) and (not Items[AStartItemNo + 1].ParaFirst) then  // ��һ���Ƕ��ף���ʼ��Ϊ��һ��Item��ʼ
      begin
        AStartItemNo := AStartItemNo + 1;
        AStartItemOffset := 0;
      end;
    end;

    if (AStartItemNo <> AEndItemNo) and (AEndItemNo >= 0)
      and (Items[AEndItemNo].Length > 0) and (AEndItemNoOffset = 0)
      and (not Items[AEndItemNo].ParaFirst)
      and (  (AEndItemNoOffset = 0)
            or (     (Items[AEndItemNo].StyleNo < THCStyle.Null)
                 and (AEndItemNoOffset = OffsetInner)
                 and not (Items[AEndItemNo] as THCCustomRectItem).SelectExists
               )
          )
    then  // ������Item��ǰ�棬��Ϊ��һ��Item����
    begin
      Items[AEndItemNo].DisSelect;  // ��ǰ����ѡ������ƶ���ǰһ��ǰ�棬ԭ��괦���Ƴ�ѡ�з�Χ

      AEndItemNo := AEndItemNo - 1;
      AEndItemNoOffset := GetItemOffsetAfter(AEndItemNo);
    end;
  end
  else
  if AEndItemNo < AStartItemNo then  // �Ӻ���ǰѡ���ڲ�ͬ��Item
  begin
    vLeftToRight := False;

    if (AStartItemNo > 0) and (AStartItemOffset = 0)  // ��ʼ��Item��ǰ�棬��Ϊ��һ��Item����
      and (not Items[AStartItemNo].ParaFirst)
    then
    begin
      AStartItemNo := AStartItemNo - 1;
      AStartItemOffset := GetItemOffsetAfter(AStartItemNo);
    end;

    if (AStartItemNo <> AEndItemNo)
      and ( (AEndItemNoOffset = GetItemOffsetAfter(AEndItemNo))
            or (     (Items[AEndItemNo].StyleNo < THCStyle.Null)
                 and (AEndItemNoOffset = OffsetInner)
                 and not (Items[AEndItemNo] as THCCustomRectItem).SelectExists
               )
          )
    then
    begin
      Items[AEndItemNo].DisSelect;  // �Ӻ���ǰѡ������ƶ���ǰһ�����棬ԭ��괦���Ƴ�ѡ�з�Χ

      if (AEndItemNo < Items.Count - 1) and (not Items[AEndItemNo + 1].ParaFirst) then  // ��Ϊ��һ��Item��ʼ
      begin
        AEndItemNo := AEndItemNo + 1;
        AEndItemNoOffset := 0;
      end;
    end;
  end;

  if AStartItemNo = AEndItemNo then  // ѡ�������ͬһ��Item�н���
  begin
    if AEndItemNoOffset > AStartItemOffset then  // ѡ�н���λ�ô�����ʼλ��
    begin
      if Items[AStartItemNo].StyleNo < THCStyle.Null then  // RectItem
      begin
        SelectInfo.StartItemNo := AStartItemNo;
        SelectInfo.StartItemOffset := AStartItemOffset;

        if (AStartItemOffset = OffsetBefor)
          and (AEndItemNoOffset = OffsetAfter)
        then  // ��RectItem��ǰ��ѡ���������(ȫѡ��)
        begin
          SelectInfo.EndItemNo := AEndItemNo;
          SelectInfo.EndItemOffset := AEndItemNoOffset;
        end
        else  // û��ȫѡ��
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
    if AEndItemNoOffset < AStartItemOffset then  // ѡ�н���λ��С����ʼλ��
    begin
      if Items[AStartItemNo].StyleNo < THCStyle.Null then  // RectItem
      begin
        if AEndItemNoOffset = OffsetBefor then  // �Ӻ���ǰѡ����ǰ����
        begin
          SelectInfo.StartItemNo := AStartItemNo;
          SelectInfo.StartItemOffset := AEndItemNoOffset;
          SelectInfo.EndItemNo := AStartItemNo;
          SelectInfo.EndItemOffset := AStartItemOffset;
        end
        else  // �Ӻ���ǰѡ��OffsetInner��
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
    else  // ����λ�ú���ʼλ����ͬ(ͬһ��Item)
    begin
      if SelectInfo.EndItemNo >= 0 then  // ͬһItem�л�ѡ�ص���ʼλ��
        Items[SelectInfo.EndItemNo].DisSelect;

      if SelectInfo.StartItemNo >= 0 then  // ��ֹͨ������(�ǽ������)ֱ�Ӳ������Ԫ��
      begin
        if (AStartItemOffset = 0) or (AStartItemOffset = GetItemOffsetAfter(SelectInfo.StartItemNo)) then  // �ص���ͷ
          Items[SelectInfo.StartItemNo].DisSelect;
      end;

      SelectInfo.StartItemNo := AStartItemNo;
      SelectInfo.StartItemOffset := AStartItemOffset;
      SelectInfo.EndItemNo := -1;
      SelectInfo.EndItemOffset := -1;
    end;
  end
  else  // ѡ���������ͬһ��Item
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

  // ��ѡ�з�Χ������ѡ��
  if vOldStartItemNo >= 0 then  // �о�ѡ��Item
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

  if SelectInfo.EndItemNo < 0 then  // ��ѡ�б����ѡ��
  begin
    for i := vOldEndItemNo downto SelectInfo.StartItemNo + 1 do  // ��ǰ�����ȡ��ѡ��
      Items[i].DisSelect;
  end
  else  // ��ѡ�н���
  begin
    for i := vOldEndItemNo downto SelectInfo.EndItemNo + 1 do  // ԭ���������ֽ�����һ����ȡ��ѡ��
      Items[i].DisSelect;
  end;
end;

procedure THCRichData.ApplySelectParaStyle(const AMatchStyle: THCParaMatch);
var
  vIndentInc: Single;

  {$REGION 'DoApplyParagraphStyle'}
  procedure DoApplyParagraphStyle(const AItemNo: Integer);
  var
    i, vParaNo, vFirstNo, vLastNo: Integer;
    vParaStyle: THCParaStyle;
    vOldIndent, vRemIndent: Single;
  begin
    if AMatchStyle is TParaFirstIndentMatch then
    begin
      vOldIndent := (AMatchStyle as TParaFirstIndentMatch).Indent;
      vParaStyle := Self.Style.ParaStyles[GetItemParaStyle(AItemNo)];
      vRemIndent := PixXToMillimeter(Width) - vParaStyle.LeftIndent - vParaStyle.RightIndent - PixXToMillimeter(TabCharWidth);
      {if (vParaStyle.FirstIndent > 0) and (vParaStyle.FirstIndent + vIndentInc < 0) then  // ʹ��������ťʱ��ͣ��0λ�ã����񲻻��д�����ʱ��
        (AMatchStyle as TParaFirstIndentMatch).Indent := 0
      else
      if (vParaStyle.FirstIndent < 0) and (vParaStyle.FirstIndent + vIndentInc > 0) then
        (AMatchStyle as TParaFirstIndentMatch).Indent := 0
      else}
      if vParaStyle.FirstIndent + vIndentInc > vRemIndent then  // ��ǰ��������Խ����
        (AMatchStyle as TParaFirstIndentMatch).Indent := vRemIndent
      else
        (AMatchStyle as TParaFirstIndentMatch).Indent := vParaStyle.FirstIndent + vIndentInc;
    end
    else
    if AMatchStyle is TParaLeftIndentMatch then
    begin
      vOldIndent := (AMatchStyle as TParaLeftIndentMatch).Indent;
      vParaStyle := Self.Style.ParaStyles[GetItemParaStyle(AItemNo)];
      vRemIndent := PixXToMillimeter(Width) - vParaStyle.FirstIndent - vParaStyle.RightIndent - PixXToMillimeter(TabCharWidth);
      if (vParaStyle.LeftIndent > 0) and (vParaStyle.LeftIndent + vIndentInc < 0) then  // ʹ��������ťʱ��ͣ��0λ��
        (AMatchStyle as TParaLeftIndentMatch).Indent := 0
      else
      if (vParaStyle.LeftIndent < 0) and (vParaStyle.LeftIndent + vIndentInc > 0) then
        (AMatchStyle as TParaLeftIndentMatch).Indent := 0
      else
      if vParaStyle.LeftIndent + vIndentInc > vRemIndent then  // ��ǰ��������Խ����
        (AMatchStyle as TParaLeftIndentMatch).Indent := vRemIndent
      else
        (AMatchStyle as TParaLeftIndentMatch).Indent := vParaStyle.LeftIndent + vIndentInc;
    end
    else
    if AMatchStyle is TParaRightIndentMatch then
    begin
      vOldIndent := (AMatchStyle as TParaRightIndentMatch).Indent;
      vParaStyle := Self.Style.ParaStyles[GetItemParaStyle(AItemNo)];
      vRemIndent := PixXToMillimeter(Width) - vParaStyle.FirstIndent - vParaStyle.LeftIndent - PixXToMillimeter(TabCharWidth);

      if vParaStyle.RightIndent + vIndentInc > vRemIndent then  // ��ǰ��������Խ����
        (AMatchStyle as TParaRightIndentMatch).Indent := vRemIndent
      else
        (AMatchStyle as TParaRightIndentMatch).Indent := vParaStyle.RightIndent + vIndentInc;
    end;

    vParaNo := AMatchStyle.GetMatchParaNo(Self.Style, GetItemParaStyle(AItemNo));
    if Items[AItemNo].ParaNo <> vParaNo then
    begin
      GetParaItemRang(AItemNo, vFirstNo, vLastNo);
      UndoAction_ItemParaNo(vFirstNo, 0, vParaNo);
      for i := vFirstNo to vLastNo do
        Items[i].ParaNo := vParaNo;
    end;

    if AMatchStyle is TParaFirstIndentMatch then
      (AMatchStyle as TParaFirstIndentMatch).Indent := vOldIndent
    else
    if AMatchStyle is TParaLeftIndentMatch then
      (AMatchStyle as TParaLeftIndentMatch).Indent := vOldIndent
    else
    if AMatchStyle is TParaRightIndentMatch then
      (AMatchStyle as TParaRightIndentMatch).Indent := vOldIndent;
  end;
  {$ENDREGION}

  {$REGION 'ApplyParagraphSelecteStyle'}
  procedure ApplyParagraphSelecteStyle;
  var
    i, vFirstNo, vLastNo: Integer;
  begin
    // �ȴ�����ʼλ�����ڵĶΣ��Ա�������ʱ����ѭ������
    GetParaItemRang(SelectInfo.StartItemNo, vFirstNo, vLastNo);
    DoApplyParagraphStyle(SelectInfo.StartItemNo);

    i := vLastNo + 1; // �ӵ�ǰ�ε���һ��item��ʼ
    while i <= SelectInfo.EndItemNo do  // С�ڽ���λ��
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

  if AMatchStyle is TParaFirstIndentMatch then
  begin
    vIndentInc := (AMatchStyle as TParaFirstIndentMatch).Indent  // ��������ʽ��������ѡ�еĶ�
      - Self.Style.ParaStyles[GetItemParaStyle(SelectInfo.StartItemNo)].FirstIndent;
  end
  else
  if AMatchStyle is TParaLeftIndentMatch then
  begin
    vIndentInc := (AMatchStyle as TParaLeftIndentMatch).Indent  // ��������ʽ��������ѡ�еĶ�
      - Self.Style.ParaStyles[GetItemParaStyle(SelectInfo.StartItemNo)].LeftIndent;
  end
  else
  if AMatchStyle is TParaRightIndentMatch then
  begin
    vIndentInc := (AMatchStyle as TParaRightIndentMatch).Indent  // ��������ʽ��������ѡ�еĶ�
      - Self.Style.ParaStyles[GetItemParaStyle(SelectInfo.StartItemNo)].RightIndent;
  end
  else
    vIndentInc := 0;

  //vFormatFirstDrawItemNo := GetFormatFirst(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);

  //GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
  //vFormatFirstItemNo := GetParaFirstItemNo(SelectInfo.StartItemNo);
  Undo_New;

  if SelectInfo.EndItemNo >= 0 then  // ��ѡ������
  begin
    vFormatFirstDrawItemNo := Items[GetParaFirstItemNo(SelectInfo.StartItemNo)].FirstDItemNo;
    vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    ApplyParagraphSelecteStyle;
    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
  end
  else  // û��ѡ������
  begin
    if (GetItemStyle(SelectInfo.StartItemNo) < THCStyle.Null)
      and (SelectInfo.StartItemOffset = OffsetInner)
      and not (Items[SelectInfo.StartItemNo] as THCCustomRectItem).IsSelectComplateTheory
    then  // ��ǰ��RectItem
    begin
      if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

      vFormatFirstDrawItemNo := GetFormatFirstDrawItem(Items[SelectInfo.StartItemNo].FirstDItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, SelectInfo.StartItemNo);
      (Items[SelectInfo.StartItemNo] as THCCustomRectItem).ApplySelectParaStyle(Self.Style, AMatchStyle);
      ReFormatData(vFormatFirstDrawItemNo, SelectInfo.StartItemNo);
    end
    else
    begin
      vFormatFirstDrawItemNo := Items[GetParaFirstItemNo(SelectInfo.StartItemNo)].FirstDItemNo;
      vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      DoApplyParagraphStyle(SelectInfo.StartItemNo);  // Ӧ����ʽ
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
    end;
  end;

  if FSelectSeekNo >= 0 then
    FCurParaNo := Items[FSelectSeekNo].ParaNo
  else
    FCurParaNo := Items[SelectInfo.StartItemNo].ParaNo;

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
end;

procedure THCRichData.ApplySelectTextStyle(const AMatchStyle: THCStyleMatch);
var
  vStyleNo, vExtraCount, vLen: Integer;
  vItem: THCCustomItem;
  vText, vSelText: string;

  {$REGION 'ApplySameItemѡ����ͬһ��Item'}
  procedure ApplySameItem(const AItemNo: Integer);
  var
    vsBefor: string;
    vSelItem, vAfterItem: THCCustomItem;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // ���ı�
    begin
      if (vItem as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(AItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(AItemNo, OffsetInner);

      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
    end
    else  // �ı�
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);
      CurStyleNo := vStyleNo;
      if vItem.IsSelectComplate then  // Itemȫ����ѡ����
      begin
        UndoAction_ItemStyle(AItemNo, SelectInfo.EndItemOffset, vStyleNo);
        vItem.StyleNo := vStyleNo;  // ֱ���޸���ʽ���

        if MergeItemToNext(AItemNo) then  // ��һ��Item�ϲ�����ǰItem
        begin
          UndoAction_InsertText(AItemNo, Items[AItemNo].Length - Items[AItemNo + 1].Length + 1, Items[AItemNo + 1].Text);
          UndoAction_DeleteItem(AItemNo + 1, 0);
          Items.Delete(AItemNo + 1);
          Dec(vExtraCount);
        end;

        if AItemNo > 0 then  // ��ǰ�ϲ�
        begin
          vLen := Items[AItemNo - 1].Length;
          if MergeItemToPrio(AItemNo) then  // ��ǰItem�ϲ�����һ��Item(�������ϲ��ˣ�vItem�Ѿ�ʧЧ������ֱ��ʹ����)
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
      else  // Itemһ���ֱ�ѡ��
      begin
        vText := vItem.Text;
        vSelText := Copy(vText, SelectInfo.StartItemOffset + 1,  // ѡ�е��ı�
          SelectInfo.EndItemOffset - SelectInfo.StartItemOffset);
        vsBefor := Copy(vText, 1, SelectInfo.StartItemOffset);  // ǰ�벿���ı�
        vAfterItem := Items[AItemNo].BreakByOffset(SelectInfo.EndItemOffset);  // ��벿�ֶ�Ӧ��Item
        if vAfterItem <> nil then  // ѡ�����λ�ò���Item����򴴽�����Ķ���Item
        begin
          UndoAction_DeleteText(AItemNo, SelectInfo.EndItemOffset + 1, vAfterItem.Text);
          Style.States.Include(hosInsertBreakItem);  // ���һ����ԭ���ڲ���ǰ�ʹ��ڵ����ݣ�Ϊ�����¼��ṩ����ϸ���Ĵ���
          try
            Items.Insert(AItemNo + 1, vAfterItem);
          finally
            Style.States.Exclude(hosInsertBreakItem);
          end;

          UndoAction_InsertItem(AItemNo + 1, 0);
          Inc(vExtraCount);
        end;

        if vsBefor <> '' then  // ѡ����ʼλ�ò���Item�ʼ������ǰ�벿�֣�����Item����ѡ�в���
        begin
          UndoAction_DeleteText(AItemNo, SelectInfo.StartItemOffset + 1, vSelText);
          vItem.Text := vsBefor;  // ����ǰ�벿���ı�

          // ����ѡ���ı���Ӧ��Item
          vSelItem := CreateDefaultTextItem;
          vSelItem.ParaNo := vItem.ParaNo;
          vSelItem.StyleNo := vStyleNo;
          vSelItem.Text := vSelText;

          if vAfterItem <> nil then  // �к�벿�֣��м��������ʽ��ǰ��϶����ܺϲ�
          begin
            Items.Insert(AItemNo + 1, vSelItem);
            UndoAction_InsertItem(AItemNo + 1, 0);
            Inc(vExtraCount);
          end
          else  // û�к�벿�֣�˵��ѡ����Ҫ�ͺ����жϺϲ�
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
        else  // ѡ����ʼλ����Item�ʼ
        begin
          //vItem.Text := vSelText;  // BreakByOffset�Ѿ�����ѡ�в����ı�
          UndoAction_ItemStyle(AItemNo, SelectInfo.EndItemOffset, vStyleNo);
          vItem.StyleNo := vStyleNo;

          if MergeItemToPrio(AItemNo) then // ��ǰItem�ϲ�����һ��Item
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

  {$REGION 'ApplyRangeStartItemѡ���ڲ�ͬItem�У�����ѡ����ʼItem'}
  procedure ApplyRangeStartItem(const AItemNo: Integer);
  var
    vAfterItem: THCCustomItem;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // ���ı�
    begin
      if (vItem as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(AItemNo, SelectInfo.StartItemOffset)
      else
        UndoAction_ItemMirror(AItemNo, SelectInfo.StartItemOffset);

      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
    end
    else  // �ı�
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);

      if vItem.StyleNo <> vStyleNo then
      begin
        if vItem.IsSelectComplate then  // Itemȫѡ����
        begin
          UndoAction_ItemStyle(AItemNo, 0, vStyleNo);
          vItem.StyleNo := vStyleNo;
        end
        else  // Item����ѡ��
        begin
          vAfterItem := Items[AItemNo].BreakByOffset(SelectInfo.StartItemOffset);  // ��벿�ֶ�Ӧ��Item
          UndoAction_DeleteText(AItemNo, SelectInfo.StartItemOffset + 1, vAfterItem.Text);
          // Ϊ����UndoAction_ItemStyle�����Ȳ��룬���޸���ʽ
          Style.States.Include(hosInsertBreakItem);
          try
            Items.Insert(AItemNo + 1, vAfterItem);
          finally
            Style.States.Exclude(hosInsertBreakItem);
          end;

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

  {$REGION 'ApplyRangeEndItemѡ���ڲ�ͬItem�У�����ѡ�н���Item'}
  procedure ApplyRangeEndItem(const AItemNo: Integer);
  var
    vBeforItem: THCCustomItem;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // ���ı�
    begin
      if (vItem as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(AItemNo, SelectInfo.EndItemOffset)
      else
        UndoAction_ItemMirror(AItemNo, SelectInfo.EndItemOffset);

      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
    end
    else  // �ı�
    begin
      vStyleNo := AMatchStyle.GetMatchStyleNo(Style, vItem.StyleNo);

      if vItem.StyleNo <> vStyleNo then
      begin
        if vItem.IsSelectComplate then  // Itemȫѡ����
        begin
          UndoAction_ItemStyle(AItemNo, SelectInfo.EndItemOffset, vStyleNo);
          vItem.StyleNo := vStyleNo;
        end
        else  // Item����ѡ����
        begin
          vText := vItem.Text;
          vSelText := Copy(vText, 1, SelectInfo.EndItemOffset); // ѡ�е��ı�
          UndoAction_DeleteBackText(AItemNo, 1, vSelText);
          Delete(vText, 1, SelectInfo.EndItemOffset);
          vItem.Text := vText;

          vBeforItem := CreateDefaultTextItem;
          vBeforItem.ParaNo := vItem.ParaNo;
          vBeforItem.StyleNo := vStyleNo;
          vBeforItem.Text := vSelText;  // ����ǰ�벿���ı���Ӧ��Item
          vBeforItem.ParaFirst := vItem.ParaFirst;
          if vItem.ParaFirst then
          begin
            UndoAction_ItemParaFirst(AItemNo, 0, False);
            vItem.ParaFirst := False;
          end;

          Items.Insert(AItemNo, vBeforItem);
          UndoAction_InsertItem(AItemNo, 0);
          Inc(vExtraCount);
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'ApplyNorItemѡ���ڲ�ͬItem�������м�Item'}
  procedure ApplyRangeNorItem(const AItemNo: Integer);
  var
    vNewStyleNo: Integer;
  begin
    vItem := Items[AItemNo];
    if vItem.StyleNo < THCStyle.Null then  // ���ı�
    begin
      if (vItem as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(AItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(AItemNo, OffsetInner);

      (vItem as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
    end
    else  // �ı�
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

  if not SelectExists then  // û��ѡ��
  begin
    if CurStyleNo > THCStyle.Null then  // (�ݴ�)���ڲ�֧���ı���ʽ��RectItem�ϣ����֧�ֵ�����ϸ�ֵStyle.CurStyleNoΪȷ�����ı���ʽ
    begin
      AMatchStyle.Append := not AMatchStyle.StyleHasMatch(Style, CurStyleNo);  // ���ݵ�ǰ�ж��������ʽ���Ǽ�����ʽ
      CurStyleNo := AMatchStyle.GetMatchStyleNo(Style, CurStyleNo);

      Style.UpdateInfoRePaint;
      if Items[SelectInfo.StartItemNo].Length = 0 then  // ���У��ı䵱ǰ��괦��ʽ
      begin
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

        UndoAction_ItemStyle(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, CurStyleNo);
        Items[SelectInfo.StartItemNo].StyleNo := CurStyleNo;

        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
        Style.UpdateInfoReCaret;
      end
      else  // ���ǿ���
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

  if SelectInfo.EndItemNo < 0 then  // û������ѡ������
  begin
    if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
    begin
      if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).MangerUndo then
        UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

      (Items[SelectInfo.StartItemNo] as THCCustomRectItem).ApplySelectTextStyle(Style, AMatchStyle);
      if (Items[SelectInfo.StartItemNo] as THCCustomRectItem).IsFormatDirty then
      begin
        // ����ı������RectItem��ȱ仯������Ҫ��ʽ�������һ��Item
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
      end
      else
        Self.FormatInit;
    end;
  end
  else  // ������ѡ������
  begin
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
    if SelectInfo.StartItemNo <> SelectInfo.EndItemNo then  // ѡ�в���ͬһItem��
      vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);

    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

    for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
    begin
      if Items[i].StyleNo > THCStyle.Null then
      begin
        AMatchStyle.Append := not AMatchStyle.StyleHasMatch(Style, Items[i].StyleNo);  // ���ݵ�һ���ж��������ʽ���Ǽ�����ʽ
        Break;
      end
      else  // RectItem
      begin
        if (Items[i] as THCCustomRectItem).MatchTextStyle(Style, AMatchStyle) then
          Break;
      end;
    end;

    if SelectInfo.StartItemNo = SelectInfo.EndItemNo then  // ѡ�з�����ͬһItem��
      ApplySameItem(SelectInfo.StartItemNo)
    else  // ѡ�з����ڲ�ͬ��Item�������ȴ���ѡ�з�Χ����ʽ�ı䣬�ٴ���ϲ����ٴ���ѡ������ȫ������ѡ��״̬
    begin
      ApplyRangeEndItem(SelectInfo.EndItemNo);  // �ȴ�������
      for i := SelectInfo.EndItemNo - 1 downto SelectInfo.StartItemNo + 1 do
        ApplyRangeNorItem(i);  // ����ÿһ��Item����ʽ
      ApplyRangeStartItem(SelectInfo.StartItemNo);

      { ��ʽ�仯�󣬴Ӻ���ǰ����ѡ�з�Χ�ڱ仯��ĺϲ� }
      //if (SelectInfo.EndItemNo < Items.Count - 1) and (not Items[SelectInfo.EndItemNo + 1].ParaFirst) then  // ѡ����������һ�����Ƕ���
      if SelectInfo.EndItemNo < vFormatLastItemNo + vExtraCount then  // ѡ����������һ�����Ƕ���
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
        if MergeItemToPrio(i) then  // �ϲ���ǰһ��
        begin
          UndoAction_InsertText(i - 1, Items[i - 1].Length - Items[i].Length + 1, Items[i].Text);
          UndoAction_DeleteItem(i, 0);
          Items.Delete(i);
          Dec(vExtraCount);

          if i = SelectInfo.EndItemNo then  // ֻ�ںϲ���ѡ�����Item�żӳ�ƫ��
            SelectInfo.EndItemOffset := SelectInfo.EndItemOffset + vLen;
          SelectInfo.EndItemNo := SelectInfo.EndItemNo - 1;
        end;
      end;

      // ��ʼ��Χ
      if (SelectInfo.StartItemNo > 0) and (not Items[SelectInfo.StartItemNo].ParaFirst) then  // ѡ����ǰ�治�Ƕεĵ�һ��Item
      begin
        vLen := Items[SelectInfo.StartItemNo - 1].Length;
        if MergeItemToPrio(SelectInfo.StartItemNo) then  // �ϲ���ǰһ��
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
          if SelectInfo.StartItemNo = SelectInfo.EndItemNo then  // ѡ�еĶ��ϲ���һ����
            SelectInfo.EndItemOffset := SelectInfo.EndItemOffset + vLen;
        end;
      end;
    end;

    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + vExtraCount, vExtraCount);
  end;

  MatchItemSelectState;

  {if FSelectSeekNo >= 0 then  // ��Ҫ��
    FCurStyleNo := Items[FSelectSeekNo].StyleNo
  else
    FCurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;}

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;
end;

procedure THCRichData.ApplyTableCellAlign(const AAlign: THCContentAlign);
begin
  if not CanEdit then Exit;

  RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      (ARectItem as THCTableItem).ApplyContentAlign(AAlign);
      Result := True;
    end);
end;

function THCRichData.DoAcceptAction(const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
begin
  //Result := CanEdit; �е�Ƶ���˰ɣ�����ǰ�󲿷־��ж���
  //if Result then
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
  if not FReadOnly then
    Result := inherited CanEdit
  else
    Result := False;
//  if not Result then
//    Beep; //MessageBeep(MB_OK);
end;

function THCRichData.CheckInsertItemCount(const AStartNo,
  AEndNo: Integer): Integer;
begin
  Result := AEndNo - AStartNo + 1;  // Ĭ��ԭ������
end;

procedure THCRichData.InitializeField;
begin
  FSelectSeekNo := -1;
  FSelectSeekOffset := -1;
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
  if not DeleteSelected then Exit;

  //-------- ��ָ����Index������Item --------//

  //DeleteSelection;  // �������ѡ����������ôɾ����AIndex��Խ�磬���Ե��ô˷���ǰ��Ҫ����δѡ��״̬
  if AItem.ParaNo > Self.Style.ParaStyles.Count - 1 then
    AItem.ParaNo := CurParaNo;

  if not AItem.ParaFirst then  // ���Ƕ���
  begin
    if IsEmptyData() then  // ������
    begin
      Undo_New();
      Result := EmptyDataInsertItem(AItem);
      CurParaNo := AItem.ParaNo;
      Exit;
    end
    else  // �����
      AItem.ParaNo := CurParaNo;
  end;

  {˵����������λ�ò������һ���Ҳ���λ���Ƕ���ʼ����ô����������һ�������룬
   Ҳ������Ҫ����һ����ǰҳ���룬��ʱ��AItem��ParaFirst����Ϊ�ж�����}

  vIncCount := 0;
  Undo_New;
  if AItem.StyleNo < THCStyle.Null then  // ����RectItem
  begin
    vInsPos := AIndex;
    if AIndex < Items.Count then  // ������������һ��Item
    begin
      if AOffsetBefor then  // ��ԭλ��Itemǰ�����
      begin
        GetFormatRange(AIndex, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        {if not AItem.ParaFirst then  // û�����Ƿ����ʼ�����ݻ�������Ӧ
        begin
          AItem.ParaFirst := Items[AIndex].ParaFirst;
          if Items[AIndex].ParaFirst then  // ��һ�ο�ʼ��Ϊ�ǿ�ʼ�����Ҫ����Ϊһ��ȥ�����жϼ���
          begin
            Undo_ItemParaFirst(AIndex, 0, False);
            Items[AIndex].ParaFirst := False;
          end;
        end;}

        if IsEmptyLine(AIndex) then  // ����λ�ô��ǿ��У��滻��ǰ
        begin
          AItem.ParaFirst := True;
          AItem.PageBreak := Items[AIndex].PageBreak;
          UndoAction_DeleteItem(AIndex, 0);
          Items.Delete(AIndex);
          Dec(vIncCount);
        end
        else  // ����λ�ò��ǿ���
        if not AItem.ParaFirst then  // û�����Ƿ����ʼ�����ݻ�������Ӧ
        begin
          AItem.ParaFirst := Items[AIndex].ParaFirst;
          if Items[AIndex].ParaFirst then  // ��һ�ο�ʼ��Ϊ�ǿ�ʼ�����Ҫ����Ϊһ��ȥ�����жϼ���
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
      else  // ��ĳItem�������
      begin
        if (AIndex > 0)
          and (Items[AIndex - 1].StyleNo > THCStyle.Null)
          and (Items[AIndex - 1].Text = '')
        then  // ����λ�ô�ǰһ���ǿ��У��滻��ǰ
        begin
          GetFormatRange(AIndex - 1, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          AItem.ParaFirst := True;
          AItem.PageBreak := Items[AIndex - 1].PageBreak;
          UndoAction_DeleteItem(AIndex - 1, 0);
          Items.Delete(AIndex - 1);
          Dec(vIncCount);
          Dec(vInsPos);
        end
        else  // ����λ��ǰһ�����ǿ���
        begin
          GetFormatRange(AIndex - 1, GetItemLastDrawItemNo(AIndex - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        end;
      end;
    end
    else  // ��ĩβ���һ����Item
    begin
      GetFormatRange(AIndex - 1, GetItemLastDrawItemNo(AIndex - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      if (not AItem.ParaFirst)  // ���벻������һ��
        and (Items[AIndex - 1].StyleNo > THCStyle.Null)  // ǰ����TextItem
        and (Items[AIndex - 1].Text = '')  // ����
      then  // ����λ�ô��ǿ��У��滻��ǰ
      begin
        AItem.ParaFirst := True;
        AItem.PageBreak := Items[AIndex - 1].PageBreak;
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
  else  // �����ı�Item
  begin
    vMerged := False;

    if not AItem.ParaFirst then  // �²���Ĳ�����һ�Σ��жϺ͵�ǰλ�õĹ�ϵ
    begin
      // ��2��Item�м����һ��Item����Ҫͬʱ�жϺ�ǰ���ܷ�ϲ�
      if AOffsetBefor then  // ��Itemǰ����룬δָ������һ��
      begin
        if (AIndex < Items.Count) and (Items[AIndex].CanConcatItems(AItem)) then  // ���жϺ͵�ǰλ�ô��ܷ�ϲ�
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
        if (not Items[AIndex].ParaFirst) and (AIndex > 0) and Items[AIndex - 1].CanConcatItems(AItem) then   // ���жϺ�ǰһ���ܷ�ϲ�
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
      else  // ��Item������룬δָ������һ�Σ���Item�������AIndex�϶��Ǵ���0
      begin
        if IsEmptyLine(AIndex - 1) then  // �ڿ��к���벻���У��滻����
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
        if Items[AIndex - 1].CanConcatItems(AItem) then   // ���жϺ�ǰһ���ܷ�ϲ�
        begin
          // �ܺϲ������»�ȡǰһ���ĸ�ʽ����Ϣ
          GetFormatRange(AIndex - 1, GetItemOffsetAfter(AIndex - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          UndoAction_InsertText(AIndex - 1, Items[AIndex - 1].Length + 1, AItem.Text);  // 201806261650
          Items[AIndex - 1].Text := Items[AIndex - 1].Text + AItem.Text;

          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
          ReSetSelectAndCaret(AIndex - 1);

          vMerged := True;
        end
        else
        if (AIndex < Items.Count) and (not Items[AIndex].ParaFirst) and (Items[AIndex].CanConcatItems(AItem)) then  // ���жϺ͵�ǰλ�ô��ܷ�ϲ�
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

    if not vMerged then  // ���ܺͲ���λ��ǰ������λ�ô���Item�ϲ�
    begin
      if AOffsetBefor then  // ��ĳItemǰ�����
      begin
        GetFormatRange(AIndex, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
        if not AItem.ParaFirst then  // û�����Ƿ����ʼ�����ݻ�������Ӧ
        begin
          AItem.ParaFirst := Items[AIndex].ParaFirst;
          if Items[AIndex].ParaFirst then  // ��һ�ο�ʼ��Ϊ�ǿ�ʼ�����Ҫ����Ϊһ��ȥ�����жϼ���
          begin
            UndoAction_ItemParaFirst(AIndex, 0, False);
            Items[AIndex].ParaFirst := False;
          end;
        end;
      end
      else  // ��ĳItem�������
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

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      (ARectItem as THCTableItem).ApplyContentAlign(AAlign);
      Result := True;
    end);
end;

function THCRichData.TableInsertColAfter(const AColCount: Integer): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).InsertColAfter(AColCount);
    end);
end;

function THCRichData.TableInsertColBefor(const AColCount: Integer): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).InsertColBefor(AColCount);
    end);
end;

function THCRichData.RectItemAction(const AItemNo: Integer; const AAction: TRectItemActionEvent): Boolean;
var
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;
  if Items[AItemNo] is THCCustomRectItem then
  begin
    GetFormatRange(AItemNo, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

    Undo_New;
    if (Items[AItemNo] as THCCustomRectItem).MangerUndo then
      UndoAction_ItemSelf(AItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(AItemNo, OffsetInner);

    Result := AAction(Items[AItemNo] as THCCustomRectItem);
    if Result then
    begin
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
      // DisSelect;  // ���ϲ������ѡ�У��ᵼ�µ�ǰItemNoû���ˣ�ͨ����������������ʱ�����
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
    end;

    InitializeMouseField;  // 201807311101
  end;
end;

procedure THCRichData.ReSetSelectAndCaret(const AItemNo, AOffset: Integer;
  const ANextWhenMid: Boolean = False);
begin
  inherited ReSetSelectAndCaret(AItemNo, AOffset, ANextWhenMid);
  FSelectSeekNo := SelectInfo.StartItemNo;
  FSelectSeekOffset := SelectInfo.StartItemOffset;
end;

function THCRichData.TableInsertRowAfter(const ARowCount: Integer): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).InsertRowAfter(ARowCount);
    end);
end;

function THCRichData.TableInsertRowBefor(const ARowCount: Integer): Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
    begin
      Result := (ARectItem as THCTableItem).InsertRowBefor(ARowCount);
    end);
end;

function THCRichData.TextItemAction(const AItemNo: Integer; const AAction: TTextItemActionEvent): Boolean;
var
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;
  if Items[AItemNo] is THCTextItem then
  begin
    GetFormatRange(AItemNo, 0, vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

    Undo_New;
    UndoAction_ItemMirror(AItemNo, 0);

    //UndoAction_ItemSelf(vCurItemNo, OffsetInner);
    Result := AAction(Items[AItemNo] as THCTextItem);
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
  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));  // ����̽��û������
  if vDataSize = 0 then Exit;

  if not CanEdit then Exit;
  if not DeleteSelected then Exit;
  if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actInsertItem) then Exit;  // TextItem��ƫ��λ�ò��ɽ�������
  if not SelectPerfect then Exit;

  vAfterItem := nil;
  vInsertBefor := False;
  vInsertEmptyLine := False;
  vCaretParaNo := CurParaNo;

  Undo_GroupBegin(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  try
    if Items.Count = 0 then  // ��
      vInsPos := 0
    else  // ������
    begin
      DeleteSelected;
      // ȷ������λ��
      vInsPos := SelectInfo.StartItemNo;
      if Items[vInsPos].StyleNo < THCStyle.Null then  // RectItem
      begin
        if SelectInfo.StartItemOffset = OffsetInner then  // ����
        begin
          GetFormatRange(SelectInfo.StartItemNo, OffsetInner, vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

          Undo_New;
          if (Items[vInsPos] as THCCustomRectItem).MangerUndo then
            UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
          else
            UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

          AStream.Position := AStream.Position - SizeOf(vDataSize);  // ����������̽���ӵ�ƫ��
          Result := (Items[vInsPos] as THCCustomRectItem).InsertStream(AStream, AStyle, AFileVersion);
          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

          Exit;
        end
        else
        if SelectInfo.StartItemOffset = OffsetBefor then  // ��ǰ
          vInsertBefor := True
        else  // ���
          vInsPos := vInsPos + 1;
      end
      else  // TextItem
      begin
        // ���жϹ���Ƿ�����󣬷�ֹ��ItemʱSelectInfo.StartItemOffset = 0����ǰ����
        if SelectInfo.StartItemOffset = Items[vInsPos].Length then  // ���
        begin
          vInsertEmptyLine := IsEmptyLine(vInsPos);
          vInsPos := vInsPos + 1;
        end
        else
        if SelectInfo.StartItemOffset = 0 then  // ��ǰ
          vInsertBefor := Items[vInsPos].Length <> 0
        else  // TextItem�м�
        begin
          Undo_New;
          UndoAction_DeleteBackText(vInsPos, SelectInfo.StartItemOffset + 1,
            Copy(Items[vInsPos].Text, SelectInfo.StartItemOffset + 1, Items[vInsPos].Length - SelectInfo.StartItemOffset));

          vAfterItem := Items[vInsPos].BreakByOffset(SelectInfo.StartItemOffset);  // ��벿�ֶ�Ӧ��Item
          vInsPos := vInsPos + 1;
        end;
      end;
    end;

    AStream.ReadBuffer(vItemCount, SizeOf(vItemCount));
    if vItemCount = 0 then Exit;

    // ��Ϊ����ĵ�һ�����ܺͲ���λ��ǰһ���ϲ�������λ�ÿ��������ף�����Ҫ�Ӳ���λ��
    // ����һ����ʼ��ʽ����Ϊ�򵥴���ֱ��ʹ�ö��ף����Ż�Ϊ��һ����
    //GetParaItemRang(SelectInfo.StartItemNo, vFormatFirstItemNo, vFormatLastItemNo);

    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);

    // �����ʽ����ʼ������ItemNo
    if Items.Count > 0 then  // ����Empty
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo)
    else
    begin
      vFormatFirstDrawItemNo := 0;
      vFormatLastItemNo := -1;
    end;

    vItemCountAct := 0;  // ʵ�ʲ��������
    vIgnoreCount := 0;  // ���Ե�������
    Undo_New;
    for i := 0 to vItemCount - 1 do
    begin
      AStream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));
      vItem := CreateItemByStyle(vStyleNo);
      {if vStyleNo < THCStyle.Null then  // ��������i������Դ�ĵ�i�������ܱ�ʾ��ǰ�ĵ���i��
      begin                              // �����RectItem����Ҳ�����壬���Բ����Լ��������ָ�
        if (vItem as THCCustomRectItem).MangerUndo then
          UndoAction_ItemSelf(i, 0)
        else
          UndoAction_ItemMirror(i, OffsetInner);
      end;}

      vItem.LoadFromStream(AStream, AStyle, AFileVersion);
      if AStyle <> nil then  // ����ʽ��
      begin
        if vItem.StyleNo > THCStyle.Null then
          vItem.StyleNo := Style.GetStyleNo(AStyle.TextStyles[vItem.StyleNo], True);

        if Style.States.Contain(hosPasting) then  // ճ��ʱʹ�ù��λ����ʽ
          vItem.ParaNo := vCaretParaNo
        else  // ��ͨ�����ļ�
          vItem.ParaNo := Style.GetParaNo(AStyle.ParaStyles[vItem.ParaNo], True);
      end
      else  // ����ʽ��
      begin
        if vItem.StyleNo > THCStyle.Null then
        begin
          if CurStyleNo < THCStyle.Null then
            vItem.StyleNo := Style.GetStyleNo(Style.DefaultTextStyle, True)
          else
            vItem.StyleNo := CurStyleNo;
        end;

        vItem.ParaNo := vCaretParaNo;
      end;

      if (vItem.StyleNo > THCStyle.Null) and (vItem.Text = '') and not vItem.ParaFirst then
      begin
        Inc(vIgnoreCount);
        FreeAndNil(vItem);
        Continue;
      end;

      if i = 0 then  // ����ĵ�һ��Item
      begin
        if vInsertBefor then  // ��һ����ĳItem��ǰ�����(ճ�����滻������)
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
      else  // ����ǵ�һ��Item
      if (not Loading) and (not vItem.ParaFirst) and MergeItemText(Items[vInsPos + i - 1 - vIgnoreCount], vItem) then  // �Ͳ���λ��ǰһ���ܺϲ�����Щ�������Ƶ�ճ��ʱ�����ǰ��ɺϲ�
      begin                                                                                          // ����ʱ���ı�Item���д���󣬿�Ҳ�����ܺ�ǰ��ϲ����������ò���ȥ���ۼ�
        Inc(vIgnoreCount);
        FreeAndNil(vItem);
        Continue;
      end;

      if (not FCheckEmptyItem) and (i > 0) and (not vItem.ParaFirst) then
      begin
        if (vItem.StyleNo > THCStyle.Null) and (vItem.Length = 0) then
          FCheckEmptyItem := True;

        if (Items[vInsPos + i - vIgnoreCount - 1].StyleNo > THCStyle.Null)
          and (Items[vInsPos + i - vIgnoreCount - 1].Length = 0)
        then
          FCheckEmptyItem := True;
      end;

      Items.Insert(vInsPos + i - vIgnoreCount, vItem);
      UndoAction_InsertItem(vInsPos + i - vIgnoreCount, 0);
      Inc(vItemCountAct);
    end;

    vItemCount := CheckInsertItemCount(vInsPos, vInsPos + vItemCountAct - 1);  // �������Item�Ƿ�ϸ�ɾ�����ϸ�

    vInsetLastNo := vInsPos + vItemCount - 1;  // ��������һ��Item
    vCaretOffse := GetItemOffsetAfter(vInsetLastNo);  // ���һ��Item����

    if vAfterItem <> nil then  // �����������Item�м䣬ԭItem����ֳ�2��
    begin
      if MergeItemText(Items[vInsetLastNo], vAfterItem) then  // �������һ���ͺ�벿���ܺϲ�
      begin
        UndoAction_InsertText(vInsetLastNo, Items[vInsetLastNo].Length - vAfterItem.Length + 1, vAfterItem.Text);
        FreeAndNil(vAfterItem);
      end
      else  // �������һ���ͺ�벿�ֲ��ܺϲ�
      begin
        Style.States.Include(hosInsertBreakItem);  // ���һ����ԭ���ڲ���ǰ�ʹ��ڵ����ݣ�Ϊ�����¼��ṩ����ϸ���Ĵ���
        try
          Items.Insert(vInsetLastNo + 1, vAfterItem);
        finally
          Style.States.Exclude(hosInsertBreakItem);
        end;

        UndoAction_InsertItem(vInsetLastNo + 1, 0);
        Inc(vItemCount);
      end;
    end;

    if {(vInsPos > vFormatFirstItemNo) and} (vInsPos > 0) then  // �ڸ�ʽ����ʼλ�ú�����Ҳ��ǵ�0��λ��
    begin
      if vInsertEmptyLine then  // ����λ��ǰ���ǿ���Item
      begin
        UndoAction_ItemParaFirst(vInsPos, 0, Items[vInsPos - 1].ParaFirst);
        Items[vInsPos].ParaFirst := Items[vInsPos - 1].ParaFirst;

        if Items[vInsPos - 1].PageBreak then
        begin
          UndoAction_ItemPageBreak(vInsPos, 0, True);
          Items[vInsPos].PageBreak := True;
        end;

        UndoAction_DeleteItem(vInsPos - 1, 0);
        Items.Delete(vInsPos - 1);  // ɾ������

        Dec(vItemCount);
        Dec(vInsetLastNo);
      end
      else  // ����λ��ǰ�治�ǿ���Item
      begin
        vOffsetStart := Items[vInsPos - 1].Length;
        if (not Items[vInsPos].ParaFirst)
          and MergeItemText(Items[vInsPos - 1], Items[vInsPos])
        then  // ����ĺ�ǰ��ĺϲ�
        begin
          UndoAction_InsertText(vInsPos - 1, Items[vInsPos - 1].Length - Items[vInsPos].Length + 1, Items[vInsPos].Text);

          UndoAction_DeleteItem(vInsPos, 0);
          Items.Delete(vInsPos);

          if vItemCount = 1 then
            vCaretOffse := vOffsetStart + vCaretOffse;

          Dec(vItemCount);
          Dec(vInsetLastNo);
        end
        else  // ���ǵ�0��λ�ã������滻������ʱ����������ݵ�һ���ǿ���
        if (Items[vInsPos].StyleNo > THCStyle.Null) and (Items[vInsPos].Length = 0) then
        begin
          UndoAction_DeleteItem(vInsPos, 0);
          Items.Delete(vInsPos);

          Dec(vItemCount);
          Dec(vInsetLastNo);
        end;
      end;

      if (vInsetLastNo < Items.Count - 1)  // ������������
        and (not Items[vInsetLastNo + 1].ParaFirst)  // ����λ�ú��治�Ƕ��ף�Ҫ�ж��ܷ�ϲ�
      then
      begin
        if MergeItemText(Items[vInsetLastNo], Items[vInsetLastNo + 1]) then  // �ܺϲ�
        begin
          UndoAction_InsertText(vInsetLastNo,
            Items[vInsetLastNo].Length - Items[vInsetLastNo + 1].Length + 1, Items[vInsetLastNo + 1].Text);
          UndoAction_DeleteItem(vInsetLastNo + 1, 0);

          Items.Delete(vInsetLastNo + 1);
          Dec(vItemCount);
        end
        else  // ���ܺϲ�
        if Items[vInsetLastNo].ParaFirst and IsEmptyLine(vInsetLastNo) then  // ����������һ���ǿ��У�����ı�Ϊ����
        begin
          UndoAction_ItemParaFirst(vInsetLastNo + 1, 0, Items[vInsetLastNo + 1].ParaFirst);
          Items[vInsetLastNo + 1].ParaFirst := True;
          UndoAction_DeleteItem(vInsetLastNo, 0);
          Items.Delete(vInsetLastNo);
          Dec(vItemCount);
        end;
      end;
    end
    else  // ���ʼ��0��λ�ô�����
    //if (vInsetLastNo < Items.Count - 1) then
    begin
      if MergeItemText(Items[vInsetLastNo], Items[vInsetLastNo + 1 {vInsPos + vItemCount}]) then  // ���ԺͲ���ǰ0λ�õĺϲ�
      begin
        vItem := Items[vInsetLastNo + 1];
        UndoAction_InsertText(vInsetLastNo, Items[vInsetLastNo].Length - vItem.Length + 1, vItem.Text);
        UndoAction_DeleteItem(vInsPos + vItemCount, 0);  // vInsPos + vItemCount = vInsetLastNo + 1?

        Items.Delete(vInsPos + vItemCount);
        Dec(vItemCount);
      end;
    end;

    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + vItemCount, vItemCount);

    ReSetSelectAndCaret(vInsetLastNo, vCaretOffse);  // ѡ�в����������Itemλ��
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

  if not AItem.ParaFirst then  // ���Ƕ���
  begin
    if IsEmptyData() then  // ������
    begin
      Undo_New();
      Result := EmptyDataInsertItem(AItem);
      CurParaNo := AItem.ParaNo;
      Exit;
    end
    else  // �����
      AItem.ParaNo := CurParaNo;
  end;

  vCurItemNo := GetActiveItemNo;
  if Items[vCurItemNo].StyleNo < THCStyle.Null then  // ��ǰλ���� RectItem
  begin
    if SelectInfo.StartItemOffset = OffsetInner then  // ��������
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      Undo_New;
      UndoAction_ItemSelf(vCurItemNo, OffsetInner);
      Result := (Items[vCurItemNo] as THCCustomRectItem).InsertItem(AItem);
      if Result then
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo, 0);
    end
    else  // ��ǰor���
    begin
      if SelectInfo.StartItemOffset = OffsetBefor then  // ��ǰ
        Result := InsertItem(SelectInfo.StartItemNo, AItem)
      else  // ���
        Result := InsertItem(SelectInfo.StartItemNo + 1, AItem, False);
    end;
  end
  else  // ��ǰλ����TextItem
  begin
    // ���ж��Ƿ��ں��棬�������ڿ��в���ʱ�Ӻ�����룬�������ɿ�������ѹ
    if (SelectInfo.StartItemOffset = Items[vCurItemNo].Length) then  // ��TextItem������
      Result := InsertItem(SelectInfo.StartItemNo + 1, AItem, False)
    else
    if SelectInfo.StartItemOffset = 0 then  // ����ǰ�����
      Result := InsertItem(SelectInfo.StartItemNo, AItem)
    else  // ��Item�м�
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vText := Items[vCurItemNo].Text;
      vsBefor := Copy(vText, 1, SelectInfo.StartItemOffset);  // ǰ�벿���ı�
      vsAfter := Copy(vText, SelectInfo.StartItemOffset + 1, Items[vCurItemNo].Length
        - SelectInfo.StartItemOffset);  // ��벿���ı�

      Undo_New;
      if Items[vCurItemNo].CanConcatItems(AItem) then  // �ܺϲ�
      begin
        if AItem.ParaFirst then  // �¶�
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
        else  // ͬһ���в���
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
      else  // ���ܺϲ�
      begin
        UndoAction_DeleteBackText(vCurItemNo, SelectInfo.StartItemOffset + 1, vsAfter);
        vAfterItem := Items[vCurItemNo].BreakByOffset(SelectInfo.StartItemOffset);  // ��벿�ֶ�Ӧ��Item

        // �����벿�ֶ�Ӧ��Item
        vCurItemNo := vCurItemNo + 1;
        Style.States.Include(hosInsertBreakItem);
        try
          Items.Insert(vCurItemNo, vAfterItem);
        finally
          Style.States.Exclude(hosInsertBreakItem);
        end;

        UndoAction_InsertItem(vCurItemNo, 0);
        // ������Item
        Items.Insert(vCurItemNo, AItem);
        UndoAction_InsertItem(vCurItemNo, 0);

        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 2, 2);
        ReSetSelectAndCaret(vCurItemNo);
      end;

      Result := True;
    end;
  end;
end;

function THCRichData.InsertTable(const ARowCount, AColCount: Integer): Boolean;
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

  {$REGION ' DoTextItemInsert ���ı�Itemǰ����м�����ı� '}
  function DoTextItemInsert(const AText: string; const ANewPara: Boolean): Boolean;
  var
    vTextItem: THCTextItem;
    vNewItem, vAfterItem: THCCustomItem;
    vS: string;
    vOffset: Integer;
  begin
    Result := False;
    vTextItem := Items[SelectInfo.StartItemNo] as THCTextItem;
    if vTextItem.StyleNo = CurStyleNo then  // ��ǰ��ʽ�Ͳ���λ��TextItem��ʽ��ͬ
    begin
      if (SelectInfo.StartItemOffset = 0) and (vTextItem.Length > 0) then  // ��TextItem��ǰ�����
      begin
        vOffset := Length(AText);

        if ANewPara or (AText = '') then  // ����һ��
        begin
          if (not ANewPara)  // AText = '' �ǿ��ַ����س�����
            and (not Items[SelectInfo.StartItemNo].ParaFirst)
          then
          begin
            UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, True);
            vTextItem.ParaFirst := True;
          end
          else
          begin
            vNewItem := CreateDefaultTextItem;
            vNewItem.Text := AText;
            vNewItem.ParaFirst := True;

            Items.Insert(SelectInfo.StartItemNo, vNewItem);
            UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
            Inc(vAddCount);
          end;
        end
        else  // ͬ��
        if vTextItem.AcceptAction(0, SelectInfo.StartRestrain, actConcatText) then  // TextItem��ǰ��ɽ��������ַ�
        begin
          UndoAction_InsertText(SelectInfo.StartItemNo, 1, AText);
          vTextItem.Text := AText + vTextItem.Text;
        end
        else  // TextItem��ǰ�治�����ַ�
        begin
          if vTextItem.ParaFirst then  // ����ǰ�治���գ��²������Ϊ����
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
          else  // TextItem��ǰ�治�����ַ���TextItem���Ƕ���ǰ��
          if Items[SelectInfo.StartItemNo - 1].StyleNo > THCStyle.Null then  // ǰһ�����ı�
          begin
            vTextItem := Items[SelectInfo.StartItemNo - 1] as THCTextItem;
            if vTextItem.AcceptAction(vTextItem.Length, True, actConcatText) then // ǰһ�������ɽ��������ַ�
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
          else  // ǰһ�������ı�
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
      if SelectInfo.StartItemOffset = vTextItem.Length then  // ��TextItem������
      begin
        if ANewPara or (AText = '') then  // ����һ��
        begin
          if (not ANewPara)  // AText = '' �ǿ��ַ����س�����
            and (SelectInfo.StartItemNo < Items.Count - 1)
            and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)
          then
          begin
            UndoAction_ItemParaFirst(SelectInfo.StartItemNo + 1, 0, True);
            Items[SelectInfo.StartItemNo + 1].ParaFirst := True;

            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            vOffset := 0;
          end
          else
          begin
            vNewItem := CreateDefaultTextItem;
            vNewItem.Text := AText;
            vNewItem.ParaFirst := True;

            Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
            UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
            Inc(vAddCount);

            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            vOffset := vNewItem.Length;
          end;
        end
        else  // ���������ı�������һ��
        if vTextItem.AcceptAction(SelectInfo.StartItemOffset, SelectInfo.StartRestrain, actConcatText) then  // ������ܽ����ı�
        begin
          UndoAction_InsertText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, AText);
          vTextItem.Text := vTextItem.Text + AText;
          vOffset := vTextItem.Length;
        end
        else  // ����治�ܽ�������
        if not IsParaLastItem(SelectInfo.StartItemNo) then  // ͬ�κ��滹������
        begin
          if Items[SelectInfo.StartItemNo + 1].StyleNo > THCStyle.Null then  // ͬ�κ������ı�
          begin
            vTextItem := Items[SelectInfo.StartItemNo + 1] as THCTextItem;
            if vTextItem.AcceptAction(0, True, actConcatText) then  // ��һ������ǰ�ɽ��������ַ�
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
          else  // ͬ�κ��治���ı�
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
        else  // �����һ�����治��������
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
      else  // ��Item�м�
      begin
        if ANewPara or (AText = '') then  // ��TextItem�м䰴�¶β���Text
        begin
          // ԭTextItem���
          vS := vTextItem.SubString(SelectInfo.StartItemOffset + 1, vTextItem.Length - SelectInfo.StartItemOffset);
          UndoAction_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, vS);
          // ԭλ�ú�벿��
          vAfterItem := vTextItem.BreakByOffset(SelectInfo.StartItemOffset);
          vAfterItem.Text := AText + vAfterItem.Text;
          vAfterItem.ParaFirst := True;
          // ����ԭTextItem��벿������Text���
          Style.States.Include(hosInsertBreakItem);  // ���һ����ԭ���ڲ���ǰ�ʹ��ڵ����ݣ�Ϊ�����¼��ṩ����ϸ���Ĵ���
          try
            Items.Insert(SelectInfo.StartItemNo + 1, vAfterItem);
          finally
            Style.States.Exclude(hosInsertBreakItem);
          end;

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
    else  // ����λ��TextItem��ʽ�͵�ǰ��ʽ��ͬ����TextItemͷ���С�βûѡ�У���Ӧ��������ʽ��������ʽ����
    begin
      vNewItem := CreateDefaultTextItem;
      vNewItem.Text := AText;
      vNewItem.ParaFirst := ANewPara or (AText = '');

      if SelectInfo.StartItemOffset = 0 then  // ��TextItem��ǰ�����
      begin
        if (not vNewItem.ParaFirst) and vTextItem.ParaFirst then  // �ڶ��ײ���Ƕ���Item
        begin
          vNewItem.ParaFirst := True;

          if vTextItem.Length = 0 then  // �ն�
          begin
            UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
            Items.Delete(SelectInfo.StartItemNo);
            Dec(vAddCount);
          end
          else
          begin
            UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, False);
            vTextItem.ParaFirst := False;
          end;
        end;

        Items.Insert(SelectInfo.StartItemNo, vNewItem);
        UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
        Inc(vAddCount);

        SelectInfo.StartItemOffset := vNewItem.Length;
      end
      else
      if SelectInfo.StartItemOffset = vTextItem.Length then  // ��TextItem��������
      begin
        Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
        UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
        Inc(vAddCount);

        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
        SelectInfo.StartItemOffset := vNewItem.Length;
      end
      else  // ��TextItem�м����
      begin
        // ԭTextItem���
        vS := vTextItem.SubString(SelectInfo.StartItemOffset + 1, vTextItem.Length - SelectInfo.StartItemOffset);
        UndoAction_DeleteText(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, vS);
        // ԭλ�ú�벿��
        vAfterItem := vTextItem.BreakByOffset(SelectInfo.StartItemOffset);
        vAfterItem.ParaFirst := False;
        // �Ȳ����µ�
        Items.Insert(SelectInfo.StartItemNo + 1, vNewItem);
        UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
        Inc(vAddCount);
        SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
        SelectInfo.StartItemOffset := vNewItem.Length;
        // �ٲ���ԭTextItem��벿��
        Style.States.Include(hosInsertBreakItem);  // ���һ����ԭ���ڲ���ǰ�ʹ��ڵ����ݣ�Ϊ�����¼��ṩ����ϸ���Ĵ���
        try
          Items.Insert(SelectInfo.StartItemNo + 1, vAfterItem);
        finally
          Style.States.Exclude(hosInsertBreakItem);
        end;

        UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
        Inc(vAddCount);
      end;
    end;

    CurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;  // ��ֹ�������벻ͬ��ʽ��
  end;
  {$ENDREGION}

  {$REGION 'DoInsertTextEx'}
  procedure DoInsertTextEx(const AText: string; const ANewPara: Boolean);
  var
    vItem: THCCustomItem;
  begin
    vItem := Items[SelectInfo.StartItemNo];

    if vItem.StyleNo < THCStyle.Null then  // ��ǰλ���� RectItem  ����ͨ�������Ѿ������������ϵ������
    begin
      if SelectInfo.StartItemOffset = OffsetAfter then  // �������������
      begin
        if (SelectInfo.StartItemNo < Items.Count - 1)
          and (Items[SelectInfo.StartItemNo + 1].StyleNo > THCStyle.Null)
          and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)
        then  // ��һ����TextItem�Ҳ��Ƕ��ף���ϲ�����һ����ʼ
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          CurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;
          DoInsertTextEx(AText, ANewPara);  // ����һ��TextItemǰ�����
        end
        else  // ������һ������RectItem��ǰ�Ƕ�β
        begin
          if (AText = '')  // AText = '' �ǿ��ַ����س�����
            and (SelectInfo.StartItemNo < Items.Count - 1)  // ��������RectItem
            and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)
          then
          begin
            UndoAction_ItemParaFirst(SelectInfo.StartItemNo + 1, 0, True);
            Items[SelectInfo.StartItemNo + 1].ParaFirst := True;

            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            SelectInfo.StartItemOffset := 0;
          end
          else
          begin
            vItem := CreateDefaultTextItem;
            vItem.Text := AText;
            vItem.ParaFirst := ANewPara or (AText = '');

            Items.Insert(SelectInfo.StartItemNo + 1, vItem);  // ������RectItem�м����
            UndoAction_InsertItem(SelectInfo.StartItemNo + 1, 0);
            Inc(vAddCount);

            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            SelectInfo.StartItemOffset := vItem.Length;
            CurStyleNo := vItem.StyleNo;
          end;
        end;
      end
      else  // ��RectItemǰ��������
      begin
        if (SelectInfo.StartItemNo > 0)
          and (Items[SelectInfo.StartItemNo - 1].StyleNo > THCStyle.Null)
          and (not vItem.ParaFirst)
        then  // ǰһ����TextItem����ǰ���Ƕ��ף��ϲ���ǰһ��β
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
          CurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;
          DoInsertTextEx(AText, ANewPara);  // ��ǰһ���������
        end
        else  // �ڶ���ǰ��ǰһ������RectItem
        begin
          if (AText = '') and (not vItem.ParaFirst) then  // ���ڶ���ǰ��ǰһ������RectItem
          begin
            UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, True);
            vItem.ParaFirst := True;
          end
          else
          begin
            vItem := CreateDefaultTextItem;
            vItem.Text := AText;
            if ANewPara or (AText = '') then  // �¶�
              vItem.ParaFirst := True;

            //  ����λ���Ƕ��ף���Ӧ��ǰ����
            if Items[SelectInfo.StartItemNo].ParaFirst then  // ԭλ���Ƕ��ף����״β����¶�����  #13#10'�ı�'
            begin
              UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, False);
              Items[SelectInfo.StartItemNo].ParaFirst := False;
              vItem.ParaFirst := True;  // ԭλ���Ƕ��ף�����ǰ���붼��Ϊ����
            end;

            Items.Insert(SelectInfo.StartItemNo, vItem);  // ������RectItem�м����
            UndoAction_InsertItem(SelectInfo.StartItemNo, 0);
            Inc(vAddCount);

            SelectInfo.StartItemOffset := vItem.Length;
            CurStyleNo := vItem.StyleNo;
          end;
        end;
      end;
    end
    else  // ��ǰλ����TextItem
      DoTextItemInsert(AText, ANewPara);
  end;
  {$ENDREGION}

var
  vPCharStart, vPCharEnd, vPtr: PChar;
  vNewPara: Boolean;
  vText, vS: string;
  vRectItem: THCCustomRectItem;
  vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;

  if not CanEdit then Exit;
  if not DeleteSelected then Exit;  // ��ɾ��ѡ�У������������滻ʱ����DoInsertTextBeforֻ�ж�ѡ����ʼ����false
  if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actInsertText) then Exit;  // TextItem��ƫ��λ�ò��ɽ�������
  if not DoInsertTextBefor(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, AText) then Exit;

  Undo_GroupBegin(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  try
    Undo_New;

    if (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)  // ��ǰλ���� RectItem
      and (SelectInfo.StartItemOffset = OffsetInner)
    then  // ��������������
    begin
      vRectItem := Items[SelectInfo.StartItemNo] as THCCustomRectItem;
      if vRectItem.MangerUndo then
        UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
      else
        UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

      Result := vRectItem.InsertText(AText);
      if vRectItem.IsFormatDirty then
      begin
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
      end
      else
        Self.FormatInit;
    end
    else  // ��ǰλ����TextItem��RectItemǰ��
    begin
      vNewPara := False;
      vAddCount := 0;
      CurStyleNo := MatchTextStyleNoAt(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);  // ��ֹ��Ĭ�ƶ�ѡ��λ��û�и��µ�ǰ��ʽ
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vText := ReplaceUnPreChar(AText);
      vPCharStart := PChar(vText);
      vPCharEnd := vPCharStart + Length(vText);
      if vPCharStart = vPCharEnd then Exit;
      vPtr := vPCharStart;
      while vPtr < vPCharEnd do
      begin
        case vPtr^ of
          #13:
            begin
              if vNewPara
                or (vPtr > vPCharStart)
              then
              begin
                System.SetString(vS, vPCharStart, vPtr - vPCharStart);
                DoInsertTextEx(vS, vNewPara);
              end;
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

    ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);  // ����Undo��¼CaretDrawItem
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

  {$REGION 'CheckSelectEndEff �ж�ѡ������Ƿ����ʼ��ͬһλ�ã�����ȡ��ѡ��'}
  procedure CheckSelectEndEff;
  var
    vStartNo, vStartOffs, vEndNo, vEndOffs: Integer;
  begin
    vStartNo := SelectInfo.StartItemNo;
    vStartOffs := SelectInfo.StartItemOffset;
    vEndNo := SelectInfo.EndItemNo;
    vEndOffs := SelectInfo.EndItemOffset;
    AdjustSelectRange(vStartNo, vStartOffs, vEndNo, vEndOffs);
//    if (SelectInfo.StartItemNo = SelectInfo.EndItemNo)
//      and (SelectInfo.StartItemOffset = SelectInfo.EndItemOffset)
//    then
//    begin
//      Items[SelectInfo.EndItemNo].DisSelect;
//
//      SelectInfo.EndItemNo := -1;
//      SelectInfo.EndItemOffset := -1;
//    end;
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

  {$REGION ' TABKeyDown ���� '}
  procedure TABKeyDown;
  var
    vTabItem: TTabItem;
    vStyleNo, vTabW: Integer;
    vParaStyle: THCParaStyle;
  begin
    if (SelectInfo.StartItemOffset = 0) and (Items[SelectInfo.StartItemNo].ParaFirst) then  // ����
    begin
      vParaStyle := Style.ParaStyles[vCurItem.ParaNo];
      vStyleNo := MatchTextStyleNoAt(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      if vStyleNo < THCStyle.Null then
        vStyleNo := Style.GetDefaultStyleNo;

      if vStyleNo < THCStyle.Null then
        vTabW := TabCharWidth
      else
      begin
        Style.ApplyTempStyle(vStyleNo);
        vTabW := Style.TempCanvas.TextWidth('��') * 2;  // 2���ַ����
      end;

      ApplyParaFirstIndent(vParaStyle.FirstIndent + PixXToMillimeter(vTabW));
    end
    else
    if vCurItem.StyleNo < THCStyle.Null then  // ��ǰ��RectItem
    begin
      if SelectInfo.StartItemOffset = OffsetInner then // ������
      begin
        if (vCurItem as THCCustomRectItem).WantKeyDown(Key, Shift) then  // ����˼�
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

  {$REGION ' LeftKeyDown ����� '}
  procedure LeftKeyDown;

    procedure SelectPrio(var AItemNo, AOffset: Integer);
    begin
      if AOffset > 0 then  // ƫ�Ʋ����ʼ����ǰItem��ǰ
      begin
        if Items[AItemNo].StyleNo < THCStyle.Null then
        begin
          if Items[AItemNo] is THCDomainItem then
            AOffset := (Items[AItemNo] as THCDomainItem).GetOffsetAt(0)
          else
            AOffset := OffsetBefor;
        end
        else
          AOffset := AOffset - 1;
      end
      else
      if AItemNo > 0 then  // ѡ���ͷ����ǰһ�����
      begin
        //Items[AItemNo].DisSelect;  // ��Item��ǰ���ٴ���ǰʱ��Item��ȡ��ѡ��
        AItemNo := AItemNo - 1;
        if Items[AItemNo].StyleNo < THCStyle.Null then
        begin
          if Items[AItemNo] is THCDomainItem then
            AOffset := (Items[AItemNo] as THCDomainItem).GetOffsetAt(0)
          else
            AOffset := OffsetBefor;
        end
        else
        begin
          if Items[AItemNo].Length > 0 then
            AOffset := Items[AItemNo].Length - 1  // ������1��ǰ��
          else
            AOffset := 0;
        end;
      end;

      {$IFDEF UNPLACEHOLDERCHAR}
      if (Items[AItemNo].StyleNo > THCStyle.Null)
        and (Items[AItemNo].Length > 0)
        and IsUnPlaceHolderChar(Items[AItemNo].Text[AOffset + 1])
      then
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
    {$IFDEF UNPLACEHOLDERCHAR}
    vChar: Char;
    {$ENDIF}
  begin
    if Shift = [ssShift] then  // Shift+�����ѡ��
    begin
      if SelectInfo.EndItemNo >= 0 then  // ��ѡ������
      begin
        if IsSelectSeekStart then  // �α���ѡ����ʼ
        begin
          SelectStartItemPrio;
          CheckSelectEndEff;
          SetSelectSeekStart;
        end
        else  // �α���ѡ�н���
        begin
          SelectEndItemPrio;
          CheckSelectEndEff;
          SetSelectSeekEnd;
        end;
      end
      else  // û��ѡ��
      begin
        if (SelectInfo.StartItemNo > 0) and (SelectInfo.StartItemOffset = 0) then  // ��Item��ǰ����ǰ
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
        end;

        SelectInfo.EndItemNo := SelectInfo.StartItemNo;
        SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;

        SelectStartItemPrio;
        CheckSelectEndEff;
        SetSelectSeekStart;
      end;

      MatchItemSelectState;
      Style.UpdateInfoRePaint;

      if not SelectExists(False) then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    end
    else  // û�а���Shift
    begin
      if vSelectExist then  // ��ѡ������
      begin
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;

        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // ��ѡ������
      begin
        if SelectInfo.StartItemOffset <> 0 then  // ����Item�ʼ
        begin
          SelectInfo.StartItemOffset := SelectInfo.StartItemOffset - 1;

          {$IFDEF UNPLACEHOLDERCHAR}
          vChar := Items[SelectInfo.StartItemNo].Text[SelectInfo.StartItemOffset + 1];
          if Pos(vChar, TibetanVowel) > 0 then  // ͨ������ƶ���Ԫ��ǰ���丨��

          else
          if IsUnPlaceHolderChar(vChar) then
            SelectInfo.StartItemOffset := GetItemActualOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset) - 1;
          {$ENDIF}
        end
        else  // ��Item�ʼ�����
        begin
          if SelectInfo.StartItemNo > 0 then  // ���ǵ�һ��Item���ʼ����ǰ���ƶ�
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;  // ��һ��
            SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);

            if not DrawItems[Items[SelectInfo.StartItemNo + 1].FirstDItemNo].LineFirst then  // �ƶ�ǰItem��������ʼ
            begin
              Style.UpdateInfoRePaint;  // �л�Itemȡ������
              KeyDown(Key, Shift);
              Exit;
            end;
          end
          else  // �ڵ�һ��Item�����水�������
            Key := 0;
        end;
      end;

      if Key <> 0 then
      begin
        vNewCaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
        if vNewCaretDrawItemNo <> CaretDrawItemNo then  // ��DrawItemNo��
        begin
          if (vNewCaretDrawItemNo = CaretDrawItemNo - 1)  // �ƶ���ǰһ����
            and (DrawItems[vNewCaretDrawItemNo].ItemNo = DrawItems[CaretDrawItemNo].ItemNo)  // ��ͬһ��Item
            and (DrawItems[CaretDrawItemNo].LineFirst)  // ԭ������
            and (SelectInfo.StartItemOffset = DrawItems[CaretDrawItemNo].CharOffs - 1)  // ���λ��Ҳ��ԭDrawItem����ǰ��
          then
            // ������
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

  {$REGION ' RightKeyDown �ҷ�������˴������漰��񣬱����RectItemKeyDown�д����� '}
  procedure RightKeyDown;

    procedure SelectNext(var AItemNo, AOffset: Integer);
    begin
      if AOffset = GetItemOffsetAfter(AItemNo) then  // ��Item����ƶ�����һ��Item
      begin
        if AItemNo < Items.Count - 1 then
        begin
          Inc(AItemNo);

          if Items[AItemNo].StyleNo < THCStyle.Null then
          begin
            if Items[AItemNo] is THCDomainItem then
              AOffset := (Items[AItemNo] as THCDomainItem).GetOffsetAt(0)
            else
              AOffset := OffsetAfter
          end
          else
          if Items[AItemNo].Length > 0 then
            AOffset := 1
          else
            AOffset := 0;
        end;
      end
      else  // �������
      begin
        if Items[AItemNo].StyleNo < THCStyle.Null then
        begin
          if Items[AItemNo] is THCDomainItem then
            AOffset := (Items[AItemNo] as THCDomainItem).GetOffsetAt(0)
          else
            AOffset := OffsetAfter
        end
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
    if Shift = [ssShift] then  // Shift+�����ѡ��
    begin
      if SelectInfo.EndItemNo >= 0 then  // ��ѡ������
      begin
        if IsSelectSeekStart then  // �α���ѡ����ʼ
        begin
          SelectStartItemNext;
          CheckSelectEndEff;
          SetSelectSeekStart;
        end
        else  // �α���ѡ�н���
        begin
          SelectEndItemNext;
          CheckSelectEndEff;
          SetSelectSeekEnd;
        end;
      end
      else   // û��ѡ��
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
        CheckSelectEndEff;
        SetSelectSeekEnd;
      end;

      MatchItemSelectState;
      Style.UpdateInfoRePaint;

      if not SelectExists(False) then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    end
    else  // û�а���Shift
    begin
      if vSelectExist then  // ��ѡ������
      begin
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;

        SelectInfo.StartItemNo := SelectInfo.EndItemNo;
        SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // ��ѡ������
      begin
        if SelectInfo.StartItemOffset < vCurItem.Length then  // ����Item���ұ�
          {$IFDEF UNPLACEHOLDERCHAR}
          SelectInfo.StartItemOffset := GetItemActualOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset + 1, True)
          {$ELSE}
          SelectInfo.StartItemOffset := SelectInfo.StartItemOffset + 1
          {$ENDIF}
        else  // ��Item���ұ�
        begin
          if SelectInfo.StartItemNo < Items.Count - 1 then  // �������һ��Item�����ұ�
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;  // ѡ����һ��Item
            SelectInfo.StartItemOffset := 0;  // ��һ����ǰ��
            if not DrawItems[Items[SelectInfo.StartItemNo].FirstDItemNo].LineFirst then  // ��һ��Item��������ʼ
            begin
              Style.UpdateInfoRePaint;  // �л�Itemȡ������
              KeyDown(Key, Shift);
              Exit;
            end;
          end
          else  // �����һ��Item�����水���ҷ����
            Key := 0;
        end;
      end;

      if Key <> 0 then
      begin
        vNewCaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
        if vNewCaretDrawItemNo = CaretDrawItemNo then  // �ƶ�ǰ����ͬһ��DrawItem
        begin
          if (SelectInfo.StartItemOffset = DrawItems[vNewCaretDrawItemNo].CharOffsetEnd)  // �ƶ���DrawItem�������
            and (vNewCaretDrawItemNo < DrawItems.Count - 1)  // �������һ��
            and (DrawItems[vNewCaretDrawItemNo].ItemNo = DrawItems[vNewCaretDrawItemNo + 1].ItemNo)  // ��һ��DrawItem�͵�ǰ��ͬһ��Item
            and (DrawItems[vNewCaretDrawItemNo + 1].LineFirst)  // ��һ��������
            and (SelectInfo.StartItemOffset = DrawItems[vNewCaretDrawItemNo + 1].CharOffs - 1)  // ���λ��Ҳ����һ��DrawItem����ǰ��
          then
            CaretDrawItemNo := vNewCaretDrawItemNo + 1;  // ����Ϊ��һ������
        end
        else
          CaretDrawItemNo := vNewCaretDrawItemNo;

        Style.UpdateInfoRePaint;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' HomeKeyDown ���� '}
  procedure HomeKeyDown;
  var
    vFirstDItemNo, vLastDItemNo, i: Integer;
  begin
    if Shift = [ssShift] then  // Shift+Home
    begin
      // ȡ����DrawItem
      vFirstDItemNo := GetDrawItemNoByOffset(FSelectSeekNo, FSelectSeekOffset);  // GetSelectStartDrawItemNo
      while vFirstDItemNo > 0 do
      begin
        if DrawItems[vFirstDItemNo].LineFirst then
          Break
        else
          Dec(vFirstDItemNo);
      end;

      if SelectInfo.EndItemNo >= 0 then  // ��ѡ������
      begin
        if IsSelectSeekStart then  // �α���ѡ����ʼ
        begin
          SelectInfo.StartItemNo := DrawItems[vFirstDItemNo].ItemNo;
          if Items[SelectInfo.StartItemNo] is THCDomainItem then
            SelectInfo.StartItemOffset := (Items[SelectInfo.StartItemNo] as THCDomainItem).GetOffsetAt(0)
          else
            SelectInfo.StartItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;

          CheckSelectEndEff;
          SetSelectSeekStart;
        end
        else  // �α���ѡ�н���
        begin
          if DrawItems[vFirstDItemNo].ItemNo > SelectInfo.StartItemNo then  // ����ѡ�У�ѡ����ʼ������
          begin
            SelectInfo.EndItemNo := DrawItems[vFirstDItemNo].ItemNo;
            if Items[SelectInfo.EndItemNo] is THCDomainItem then
              SelectInfo.EndItemOffset := (Items[SelectInfo.EndItemNo] as THCDomainItem).GetOffsetAt(0)
            else
              SelectInfo.EndItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;

            CheckSelectEndEff;
            SetSelectSeekEnd;
          end
          else
          if DrawItems[vFirstDItemNo].ItemNo = SelectInfo.StartItemNo then  // ѡ����ʼ������ItemNo
          begin
            if DrawItems[vFirstDItemNo].CharOffs - 1 > SelectInfo.StartItemOffset then
            begin
              SelectInfo.EndItemNo := SelectInfo.StartItemNo;
              if Items[SelectInfo.EndItemNo] is THCDomainItem then
                SelectInfo.EndItemOffset := (Items[SelectInfo.EndItemNo] as THCDomainItem).GetOffsetAt(0)
              else
                SelectInfo.EndItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;

              CheckSelectEndEff;
              SetSelectSeekEnd;
            end;
          end
          else
          begin
            SelectInfo.EndItemNo := SelectInfo.StartItemNo;
            SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
            SelectInfo.StartItemNo := DrawItems[vFirstDItemNo].ItemNo;
            if Items[SelectInfo.StartItemNo] is THCDomainItem then
              SelectInfo.StartItemOffset := (Items[SelectInfo.StartItemNo] as THCDomainItem).GetOffsetAt(0)
            else
              SelectInfo.StartItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;

            CheckSelectEndEff;
            SetSelectSeekStart;
          end;
        end;
      end
      else   // û��ѡ��
      begin
        SelectInfo.EndItemNo := SelectInfo.StartItemNo;
        SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
        SelectInfo.StartItemNo := DrawItems[vFirstDItemNo].ItemNo;
        if Items[SelectInfo.StartItemNo] is THCDomainItem then
          SelectInfo.StartItemOffset := (Items[SelectInfo.StartItemNo] as THCDomainItem).GetOffsetAt(0)
        else
          SelectInfo.StartItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;

        CheckSelectEndEff;
        SetSelectSeekStart;
      end;

      MatchItemSelectState;
      Style.UpdateInfoRePaint;

      if not SelectExists(False) then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    end
    else
    begin
      if vSelectExist then  // ��ѡ������
      begin
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;

        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // ��ѡ������
      begin
        vFirstDItemNo := GetSelectStartDrawItemNo;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);
        SelectInfo.StartItemNo := DrawItems[vFirstDItemNo].ItemNo;
        if Items[SelectInfo.StartItemNo] is THCDomainItem then
          SelectInfo.StartItemOffset := (Items[SelectInfo.StartItemNo] as THCDomainItem).GetOffsetAt(0)
        else
          SelectInfo.StartItemOffset := DrawItems[vFirstDItemNo].CharOffs - 1;
      end;

      if Key <> 0 then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);

      Style.UpdateInfoRePaint;
    end;
  end;
  {$ENDREGION}

  {$REGION ' EndKeyDown ���� '}
  procedure EndKeyDown;
  var
    vFirstDItemNo, vLastDItemNo, i: Integer;
  begin
    if Shift = [ssShift] then  // Shift+End
    begin
      // ȡ��βDrawItem
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

      if SelectInfo.EndItemNo >= 0 then  // ��ѡ������
      begin
        if IsSelectSeekStart then  // �α���ѡ����ʼ
        begin
          if DrawItems[vLastDItemNo].ItemNo > SelectInfo.EndItemNo then  // ��Ȼ�ڽ���ǰ�棨����ѡ�У�
          begin
            SelectInfo.StartItemNo := DrawItems[vLastDItemNo].ItemNo;
            
            if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
            begin
              if Items[SelectInfo.StartItemNo] is THCDomainItem then
                SelectInfo.StartItemOffset := (Items[SelectInfo.StartItemNo] as THCDomainItem).GetOffsetAt(0)
              else
                SelectInfo.StartItemOffset := OffsetAfter;
            end
            else
              SelectInfo.StartItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;

            CheckSelectEndEff;
            SetSelectSeekStart;
          end
          else
          if DrawItems[vLastDItemNo].ItemNo = SelectInfo.EndItemNo then
          begin
            SelectInfo.StartItemNo := SelectInfo.EndItemNo;
            if DrawItems[vLastDItemNo].CharOffsetEnd < SelectInfo.EndItemOffset then
            begin
              if Items[SelectInfo.StartItemNo] is THCDomainItem then
                SelectInfo.StartItemOffset := (Items[SelectInfo.StartItemNo] as THCDomainItem).GetOffsetAt(0)
              else
                SelectInfo.StartItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;

              CheckSelectEndEff;
              SetSelectSeekStart;
            end
            else
            begin
              SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
              if Items[SelectInfo.EndItemNo] is THCDomainItem then
                SelectInfo.EndItemOffset := (Items[SelectInfo.EndItemNo] as THCDomainItem).GetOffsetAt(0)
              else
                SelectInfo.EndItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;

              CheckSelectEndEff;
              SetSelectSeekEnd;
            end;
          end
          else
          begin
            SelectInfo.StartItemNo := SelectInfo.EndItemNo;
            SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
            SelectInfo.EndItemNo := DrawItems[vLastDItemNo].ItemNo;
            if Items[SelectInfo.EndItemNo] is THCDomainItem then
              SelectInfo.EndItemOffset := (Items[SelectInfo.EndItemNo] as THCDomainItem).GetOffsetAt(0)
            else
              SelectInfo.EndItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;

            CheckSelectEndEff;
            SetSelectSeekEnd;
          end;
        end
        else  // �α���ѡ�н���
        begin
          SelectInfo.EndItemNo := DrawItems[vLastDItemNo].ItemNo;
          if Items[SelectInfo.EndItemNo] is THCDomainItem then
            SelectInfo.EndItemOffset := (Items[SelectInfo.EndItemNo] as THCDomainItem).GetOffsetAt(0)
          else
            SelectInfo.EndItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;

          CheckSelectEndEff;
          SetSelectSeekEnd;
        end;
      end
      else   // û��ѡ��
      begin
        SelectInfo.EndItemNo := DrawItems[vLastDItemNo].ItemNo;
        if Items[SelectInfo.EndItemNo] is THCDomainItem then
          SelectInfo.EndItemOffset := (Items[SelectInfo.EndItemNo] as THCDomainItem).GetOffsetAt(0)
        else
          SelectInfo.EndItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;

        CheckSelectEndEff;

        if SelectInfo.EndItemNo < 0 then
          SetSelectSeekStart
        else
          SetSelectSeekEnd;
      end;

      MatchItemSelectState;
      Style.UpdateInfoRePaint;

      if not SelectExists(False) then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    end
    else
    begin
      if vSelectExist then  // ��ѡ������
      begin
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;

        SelectInfo.StartItemNo := SelectInfo.EndItemNo;
        if Items[SelectInfo.StartItemNo] is THCDomainItem then
          SelectInfo.StartItemOffset := (Items[SelectInfo.StartItemNo] as THCDomainItem).GetOffsetAt(0)
        else
          SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;

        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // ��ѡ������
      begin
        vFirstDItemNo := GetSelectStartDrawItemNo;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);
        SelectInfo.StartItemNo := DrawItems[vLastDItemNo].ItemNo;
        if Items[SelectInfo.StartItemNo] is THCDomainItem then
          SelectInfo.StartItemOffset := (Items[SelectInfo.StartItemNo] as THCDomainItem).GetOffsetAt(0)
        else
          SelectInfo.StartItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
      end;

      if Key <> 0 then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);

      Style.UpdateInfoRePaint;
    end;
  end;
  {$ENDREGION}

  {$REGION ' UpKeyDown �Ϸ��򰴼� '}
  procedure UpKeyDown;

    function GetUpDrawItemNo(var ADrawItemNo, ADrawItemOffset: Integer): Boolean;
    var
      i, vFirstDItemNo, vLastDItemNo, vX: Integer;
    begin
      Result := False;
      vFirstDItemNo := ADrawItemNo;
      GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // ��ǰ����ʼ����DrawItemNo
      if vFirstDItemNo > 0 then  // ��ǰ�в��ǵ�һ��
      begin
        Result := True;
        // ��ȡ��ǰ���Xλ��
        vX := DrawItems[ADrawItemNo].Rect.Left + GetDrawItemOffsetWidth(ADrawItemNo, ADrawItemOffset);

        // ��ȡ��һ����Xλ�ö�Ӧ��DItem��Offset
        vFirstDItemNo := vFirstDItemNo - 1;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // ��һ����ʼ�ͽ���DItem

        for i := vFirstDItemNo to vLastDItemNo do
        begin
          if DrawItems[i].Rect.Right > vX then
          begin
            ADrawItemNo := i;
            if Items[DrawItems[i].ItemNo] is THCDomainItem then
              ADrawItemOffset := (Items[DrawItems[i].ItemNo] as THCDomainItem).GetOffsetAt(0)
            else
              ADrawItemOffset := DrawItems[i].CharOffs + GetDrawItemOffsetAt(i, vX) - 1;

            Exit;  // �к��ʣ����˳�
          end;
        end;

        // û������ѡ�����
        ADrawItemNo := vLastDItemNo;
        if Items[DrawItems[vLastDItemNo].ItemNo] is THCDomainItem then
          ADrawItemOffset := (Items[DrawItems[vLastDItemNo].ItemNo] as THCDomainItem).GetOffsetAt(0)
        else
          ADrawItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
      end
    end;

  var
    vDrawItemNo, vDrawItemOffset: Integer;
  begin
    if Shift = [ssShift] then  // Shift+Up
    begin
      if SelectInfo.EndItemNo >= 0 then  // ��ѡ������
      begin
        if IsSelectSeekStart then  // �α���ѡ����ʼ
        begin
          vDrawItemNo := GetSelectStartDrawItemNo;
          vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
          if GetUpDrawItemNo(vDrawItemNo, vDrawItemOffset) then
          begin
            SelectInfo.StartItemNo := DrawItems[vDrawItemNo].ItemNo;
            SelectInfo.StartItemOffset := vDrawItemOffset;
            CheckSelectEndEff;
            SetSelectSeekStart;
          end;
        end
        else  // �α���ѡ�н���
        begin
          vDrawItemNo := GetSelectEndDrawItemNo;
          vDrawItemOffset := SelectInfo.EndItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
          if GetUpDrawItemNo(vDrawItemNo, vDrawItemOffset) then
          begin
            if DrawItems[vDrawItemNo].ItemNo > SelectInfo.StartItemNo then  // �ƶ�����ʼ�����Item��
            begin
              SelectInfo.EndItemNo := vDrawItemNo;
              SelectInfo.EndItemOffset := vDrawItemOffset;
              CheckSelectEndEff;
              SetSelectSeekEnd;
            end
            else
            if DrawItems[vDrawItemNo].ItemNo = SelectInfo.StartItemNo then  // �ƶ�����ʼItem��
            begin
              SelectInfo.EndItemNo := SelectInfo.StartItemNo;
              if vDrawItemOffset > SelectInfo.StartItemOffset then  // �ƶ�����ʼOffset����
              begin
                SelectInfo.EndItemOffset := vDrawItemOffset;
                CheckSelectEndEff;
                SetSelectSeekEnd;
              end
              else  // �ƶ�����ʼOffsetǰ��
              begin
                SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
                SelectInfo.StartItemOffset := vDrawItemOffset;
                CheckSelectEndEff;
                SetSelectSeekStart;
              end;
            end
            else  // �ƶ�����ʼItemǰ����
            begin
              SelectInfo.EndItemNo := SelectInfo.StartItemNo;
              SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
              SelectInfo.StartItemNo := DrawItems[vDrawItemNo].ItemNo;
              SelectInfo.StartItemOffset := vDrawItemOffset;
              CheckSelectEndEff;
              SetSelectSeekStart;
            end;
          end;
        end;
      end
      else   // û��ѡ��
      begin
        vDrawItemNo := CaretDrawItemNo;
        vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
        if GetUpDrawItemNo(vDrawItemNo, vDrawItemOffset) then
        begin
          SelectInfo.EndItemNo := SelectInfo.StartItemNo;
          SelectInfo.EndItemOffset := SelectInfo.StartItemOffset;
          SelectInfo.StartItemNo := DrawItems[vDrawItemNo].ItemNo;
          SelectInfo.StartItemOffset := vDrawItemOffset;
          CheckSelectEndEff;
          SetSelectSeekStart;
        end;
      end;

      MatchItemSelectState;
      Style.UpdateInfoRePaint;

      if not SelectExists(False) then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    end
    else  // ��Shift����
    begin
      if vSelectExist then  // ��ѡ������
      begin
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // ��ѡ������
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

  {$REGION ' DownKeyDown �·���� '}
  procedure DownKeyDown;

    function GetDownDrawItemNo(var ADrawItemNo, ADrawItemOffset: Integer): Boolean;
    var
      i, vFirstDItemNo, vLastDItemNo, vX: Integer;
    begin
      Result := False;
      vFirstDItemNo := ADrawItemNo;  // GetSelectStartDrawItemNo;
      GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // ��ǰ����ʼ����DItemNo
      if vLastDItemNo < DrawItems.Count - 1 then  // ��ǰ�в������һ��
      begin
        Result := True;
        { ��ȡ��ǰ���Xλ�� }
        vX := DrawItems[ADrawItemNo].Rect.Left + GetDrawItemOffsetWidth(ADrawItemNo, ADrawItemOffset);

        { ��ȡ��һ����Xλ�ö�Ӧ��DItem��Offset }
        vFirstDItemNo := vLastDItemNo + 1;
        GetLineDrawItemRang(vFirstDItemNo, vLastDItemNo);  // ��һ����ʼ�ͽ���DItem

        for i := vFirstDItemNo to vLastDItemNo do
        begin
          if DrawItems[i].Rect.Right > vX then
          begin
            ADrawItemNo := i;
            if Items[DrawItems[i].ItemNo] is THCDomainItem then
              ADrawItemOffset := (Items[DrawItems[i].ItemNo] as THCDomainItem).GetOffsetAt(0)
            else
              ADrawItemOffset := DrawItems[i].CharOffs + GetDrawItemOffsetAt(i, vX) - 1;

            Exit;  // �к��ʣ����˳�
          end;
        end;

        { û������ѡ����� }
        ADrawItemNo := vLastDItemNo;
        if Items[DrawItems[vLastDItemNo].ItemNo] is THCDomainItem then
          ADrawItemOffset := (Items[DrawItems[vLastDItemNo].ItemNo] as THCDomainItem).GetOffsetAt(0)
        else
          ADrawItemOffset := DrawItems[vLastDItemNo].CharOffsetEnd;
      end
    end;

  var
    vDrawItemNo, vDrawItemOffset: Integer;
  begin
    if Shift = [ssShift] then  // Shift+Up
    begin
      if SelectInfo.EndItemNo >= 0 then  // ��ѡ������
      begin
        if IsSelectSeekStart then  // �α���ѡ����ʼ
        begin
          vDrawItemNo := GetSelectStartDrawItemNo;
          vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
          if GetDownDrawItemNo(vDrawItemNo, vDrawItemOffset) then
          begin
            if DrawItems[vDrawItemNo].ItemNo < SelectInfo.EndItemNo then  // �ƶ�������ItemNoǰ��
            begin
              SelectInfo.StartItemNo := SelectInfo.EndItemNo;
              SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
              CheckSelectEndEff;
              SetSelectSeekStart;
            end
            else
            if DrawItems[vDrawItemNo].ItemNo = SelectInfo.EndItemNo then  // �ƶ����ͽ���Item
            begin
              SelectInfo.StartItemNo := SelectInfo.EndItemNo;
              if vDrawItemOffset < SelectInfo.EndItemOffset then  // λ���ڽ���Offsetǰ��
              begin
                SelectInfo.StartItemOffset := vDrawItemOffset;
                CheckSelectEndEff;
                SetSelectSeekStart;
              end
              else  // λ���ڽ���Offset����
              begin
                SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
                SelectInfo.EndItemOffset := vDrawItemOffset;
                CheckSelectEndEff;
                SetSelectSeekEnd;
              end;
            end
            else  // �ƶ�������Item���棬����
            begin
              SelectInfo.StartItemNo := SelectInfo.EndItemNo;
              SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
              SelectInfo.EndItemNo := DrawItems[vDrawItemNo].ItemNo;
              SelectInfo.EndItemOffset := vDrawItemOffset;
              CheckSelectEndEff;
              SetSelectSeekEnd;
            end;
          end;
        end
        else  // �α���ѡ�н���
        begin
          vDrawItemNo := GetSelectEndDrawItemNo;
          vDrawItemOffset := SelectInfo.EndItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
          if GetDownDrawItemNo(vDrawItemNo, vDrawItemOffset) then
          begin
            SelectInfo.EndItemNo := DrawItems[vDrawItemNo].ItemNo;
            SelectInfo.EndItemOffset := vDrawItemOffset;
            CheckSelectEndEff;
            SetSelectSeekEnd;
          end;
        end;
      end
      else   // û��ѡ��
      begin
        vDrawItemNo := CaretDrawItemNo;
        vDrawItemOffset := SelectInfo.StartItemOffset - DrawItems[vDrawItemNo].CharOffs + 1;
        if GetDownDrawItemNo(vDrawItemNo, vDrawItemOffset) then
        begin
          SelectInfo.EndItemNo := DrawItems[vDrawItemNo].ItemNo;
          SelectInfo.EndItemOffset := vDrawItemOffset;
          CheckSelectEndEff;
          SetSelectSeekEnd;
        end;
      end;

      MatchItemSelectState;
      Style.UpdateInfoRePaint;

      if not SelectExists(False) then
        CaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
    end
    else  // ��Shift����
    begin
      if vSelectExist then  // ��ѡ������
      begin
        SelectInfo.StartItemNo := SelectInfo.EndItemNo;
        SelectInfo.StartItemOffset := SelectInfo.EndItemOffset;
        SelectInfo.EndItemNo := -1;
        SelectInfo.EndItemOffset := -1;
      end
      else  // ��ѡ������
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
        else  // ��ǰ�������һ��
          Key := 0;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION ' RectItemKeyDown Rect����Item��KeyDown�¼� '}
  procedure RectItemKeyDown;
  var
    vItem: THCCustomItem;
    vLen: Integer;
    vRectItem: THCCustomRectItem;
  begin
    vRectItem := vCurItem as THCCustomRectItem;

    if SelectInfo.StartItemOffset = OffsetInner then  // ������
    begin
      if vRectItem.WantKeyDown(Key, Shift) then
      begin
        Undo_New;
        if vRectItem.MangerUndo then
          UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
        else
          UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

        vRectItem.KeyDown(Key, Shift);
        if vRectItem.IsFormatDirty then
        begin
          GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
          FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
        end
        else
          Self.FormatInit;
      end
      else  // �ڲ�����Ӧ�˼�
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

          VK_LEFT:  // �Ƴ�ȥ
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              vRectItem.Active := False;
              Style.UpdateInfoRePaint;
            end;

          VK_RIGHT:  // �Ƴ�ȥ
            begin
              SelectInfo.StartItemOffset := OffsetAfter;
              vRectItem.Active := False;
              Style.UpdateInfoRePaint;
            end;
        else
          vRectItem.KeyDown(Key, Shift);
        end;
      end;
    end
    else
    if SelectInfo.StartItemOffset = OffsetBefor then  // ��RectItemǰ
    begin
      case Key of
        VK_LEFT:
          LeftKeyDown;

        VK_RIGHT:
          begin
            if Shift = [ssShift] then  // Shift+�����ѡ��
              RightKeyDown
            else
            begin
              if vRectItem.WantKeyDown(Key, Shift) then
                SelectInfo.StartItemOffset := OffsetInner
              else
              if vRectItem is THCDomainItem then
                SelectInfo.StartItemOffset := vRectItem.GetOffsetAt(vRectItem.Width + 1)
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

            if vCurItem.ParaFirst then  // RectItem�ڶ��ף��������
            begin
              vCurItem := CreateDefaultTextItem;
              vCurItem.ParaFirst := True;
              Items.Insert(SelectInfo.StartItemNo, vCurItem);

              Undo_New;
              UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

              if APageBreak then
              begin
                UndoAction_ItemPageBreak(SelectInfo.StartItemNo + 1, 0, True);  // �ұ����һ����
                Items[SelectInfo.StartItemNo + 1].PageBreak := True;
              end;

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);

              SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
              ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
            end
            else  // RectItem��������
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

        VK_BACK:  // ��RectItemǰ
          begin
            if vCurItem.ParaFirst  // �Ƕ���
              and DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actBackDeleteText)
            then
            begin
              if Style.ParaStyles[vCurItem.ParaNo].FirstIndent > 0 then  // �ڶ���ǰ��ɾ��
                ApplyParaFirstIndent(Max(0, Style.ParaStyles[vCurItem.ParaNo].FirstIndent - PixXToMillimeter(TabCharWidth)))
              else
              if SelectInfo.StartItemNo > 0 then  // ��һ��ǰ��ɾ������ֹͣ��ʽ��
              begin
                //if vCurItem.ParaFirst and (SelectInfo.StartItemNo > 0) then
                //begin
                  vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo - 1,
                    GetItemOffsetAfter(SelectInfo.StartItemNo - 1));

                  vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
                //end
                //else
                //  GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);

                FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

                Undo_New;

                if IsEmptyLine(SelectInfo.StartItemNo - 1) then  // ��һ���ǿ���
                begin
                  UndoAction_DeleteItem(SelectInfo.StartItemNo - 1, 0);
                  Items.Delete(SelectInfo.StartItemNo - 1);

                  SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;

                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
                  ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
                end
                else  // ��һ�в��ǿ���
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
            else  // ���Ƕ���
            begin
              // ѡ����һ�����
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);

              KeyDown(Key, Shift);  // ִ��ǰһ����ɾ��
            end;
          end;

        VK_DELETE:  // ��RectItemǰ
          begin
            if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actDeleteItem) then  // ����ɾ��
            begin
              SelectInfo.StartItemOffset := OffsetAfter;
              Exit;
            end;

            GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            if vCurItem.ParaFirst then  // �Ƕ���
            begin
              if SelectInfo.StartItemNo <> vFormatLastItemNo then  // �β���ֻ��һ��
              begin
                Undo_New;
                UndoAction_ItemParaFirst(SelectInfo.StartItemNo + 1, 0, True);
                Items[SelectInfo.StartItemNo + 1].ParaFirst := True;

                UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
                Items.Delete(SelectInfo.StartItemNo);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
              end
              else  // ��ɾ������
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
            else  // ���Ƕ���
            begin
              if SelectInfo.StartItemNo < vFormatLastItemNo then  // ���м�
              begin
                vLen := GetItemOffsetAfter(SelectInfo.StartItemNo - 1);

                Undo_New;
                UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
                // ���RectItemǰ��(ͬһ��)�и߶�С�ڴ�RectItme��Item(��Tab)��
                // ���ʽ��ʱ��RectItemΪ�ߣ����¸�ʽ��ʱ�����RectItem����λ����ʼ��ʽ����
                // �и߶��Ի���TabΪ�иߣ�Ҳ����RectItem�߶ȣ�������Ҫ���п�ʼ��ʽ��
                Items.Delete(SelectInfo.StartItemNo);
                if MergeItemText(Items[SelectInfo.StartItemNo - 1], Items[SelectInfo.StartItemNo]) then  // ԭRectItemǰ���ܺϲ�
                begin
                  UndoAction_InsertText(SelectInfo.StartItemNo - 1,
                    Items[SelectInfo.StartItemNo - 1].Length - Items[SelectInfo.StartItemNo].Length + 1,
                    Items[SelectInfo.StartItemNo].Text);

                  UndoAction_DeleteItem(SelectInfo.StartItemNo, 0);
                  Items.Delete(SelectInfo.StartItemNo);
                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 2, -2);
                end
                else
                  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

                SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
                SelectInfo.StartItemOffset := vLen;
              end
              else  // ��β(�β�ֻһ��Item)
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
    if SelectInfo.StartItemOffset = OffsetAfter then  // �����
    begin
      case Key of
        VK_BACK:
          begin
            if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actDeleteItem) then  // ����ɾ��
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              Exit;
            end;

            GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            if vCurItem.ParaFirst then  // �Ƕ���
            begin
              if (SelectInfo.StartItemNo >= 0)
                and (SelectInfo.StartItemNo < Items.Count - 1)
                and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)
              then  // ͬһ�κ��滹������
              begin
                Undo_New;
                UndoAction_DeleteItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
                Items.Delete(SelectInfo.StartItemNo);

                UndoAction_ItemParaFirst(SelectInfo.StartItemNo, 0, True);
                Items[SelectInfo.StartItemNo].ParaFirst := True;
                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);

                ReSetSelectAndCaret(SelectInfo.StartItemNo, 0);
              end
              else  // �ն���
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
            else  // ���Ƕ���
            begin
              SelectInfo.StartItemOffset := OffsetBefor;
              Key := VK_DELETE;  // ��ʱ�滻
              RectItemKeyDown;
              Key := VK_BACK;  // ��ԭ
            end;
          end;

        VK_DELETE:
          begin
            if SelectInfo.StartItemNo < Items.Count - 1 then  // �������һ��
            begin
              if Items[SelectInfo.StartItemNo + 1].ParaFirst then  // ��һ���Ƕ��ף���ǰ���ڶ������deleteɾ����
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
            if Shift = [ssShift] then  // Shift+�����ѡ��
              LeftKeyDown
            else
            begin
              if vRectItem.WantKeyDown(Key, Shift) then
                SelectInfo.StartItemOffset := OffsetInner
              else
              if vRectItem is THCDomainItem then
                SelectInfo.StartItemOffset := vRectItem.GetOffsetAt(-1)
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
            if (SelectInfo.StartItemNo < Items.Count - 1)  // �������һ��
              and (not Items[SelectInfo.StartItemNo + 1].ParaFirst)  // ��һ�����Ƕ���
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

  {$REGION ' EnterKeyDown �س� '}
  procedure EnterKeyDown;
  var
    vItem: THCCustomItem;
  begin
    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actReturnItem) then Exit;
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    // �жϹ��λ��������λ���
    if SelectInfo.StartItemOffset = 0 then  // �����Item��ǰ��
    begin
      if not vCurItem.ParaFirst then  // ԭ�����Ƕ���(�����Item��ǰ��)
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
      else  // ԭ�����Ƕ���(�����Item��ǰ��)
      begin
        if APageBreak then
        begin
          Undo_New;
          if Self.Items.Count = 1 then  // �ĵ�ֻ��һ��Item
          begin
            // ����һ������
            vItem := CreateDefaultTextItem;
            vItem.StyleNo := vCurItem.StyleNo;
            vItem.ParaNo := vCurItem.ParaNo;
            vItem.ParaFirst := True;

            UndoAction_ItemPageBreak(SelectInfo.StartItemNo, 0, True);
            vCurItem.PageBreak := True;

            Items.Insert(SelectInfo.StartItemNo, vItem);  // ���뵽��ǰ
            UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);
          end
          else  // �����ĵ�ΨһItem
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

          Items.Insert(SelectInfo.StartItemNo, vItem);  // ���뵽��ǰ

          Undo_New;
          UndoAction_InsertItem(SelectInfo.StartItemNo, 0);

          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + 1, 1);
        end;
      end;
    end
    else
    if SelectInfo.StartItemOffset = vCurItem.Length then  // �����Item�����
    begin
      if SelectInfo.StartItemNo < Items.Count - 1 then  // �������һ��Item
      begin
        vItem := Items[SelectInfo.StartItemNo + 1];  // ��һ��Item
        if not vItem.ParaFirst then  // ��һ�����Ƕ���ʼ
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
        else  // ��һ���Ƕ���ʼ
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
      else  // ��Data���һ��Item���½�����
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
    else  // �����Item�м�
    begin
      vItem := vCurItem.BreakByOffset(SelectInfo.StartItemOffset);  // �ضϵ�ǰItem

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

  {$REGION ' DeleteKeyDown ���ɾ���� '}
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
    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actDeleteText) then  // ����ɾ��
    begin
      SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;  // SelectInfo.StartItemOffset + 1
      ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      Exit;
    end;

    vDelCount := 0;
    vCurItemNo := SelectInfo.StartItemNo;
    GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);

    if SelectInfo.StartItemOffset = vCurItem.Length then  // �����Item���ұ�(��������)
    begin
      if vCurItemNo <> Items.Count - 1 then  // �������һ��Item���ұ�ɾ��
      begin
        if Items[vCurItemNo + 1].ParaFirst then  // ��һ���Ƕ��ף���괦Item����һ�����һ������һ��Ҫ������
        begin
          vFormatLastItemNo := GetParaLastItemNo(vCurItemNo + 1);  // ��ȡ��һ�����һ��
          if vCurItem.Length = 0 then  // ��ǰ�ǿ���
          begin
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            Undo_New;
            UndoAction_DeleteItem(vCurItemNo, 0);
            Items.Delete(vCurItemNo);

            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
          end
          else  // ��ǰ���ǿ���
          begin
            if Items[vCurItemNo + 1].StyleNo < THCStyle.Null then  // ��һ��������RectItem�����ܺϲ�
            begin
              vFormatLastItemNo := GetParaLastItemNo(vCurItemNo + 1);  // ��ȡ��һ�����һ��
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
            else  // ��һ��������TextItem(��ǰ����һ�ζ�β)
            begin
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              if Items[vCurItemNo + 1].Length = 0 then  // ��һ�εĶ������ǿ���
              begin
                Undo_New;
                UndoAction_DeleteItem(vCurItemNo + 1, 0);
                Items.Delete(vCurItemNo + 1);

                Inc(vDelCount);
              end
              else  // ��һ�εĶ��ײ��ǿ���
              begin
                //if (vCurItem.ClassType = Items[vCurItemNo + 1].ClassType)
                //  and (vCurItem.StyleNo = Items[vCurItemNo + 1].StyleNo)
                if vCurItem.CanConcatItems(Items[vCurItemNo + 1]) then  // ��һ�ζ��׿ɺϲ�����ǰ(��ǰ����һ�ζ�β) 201804111209 (������MergeItemText�����)
                begin
                  Undo_New;

                  UndoAction_InsertText(vCurItemNo, vCurItem.Length + 1, Items[vCurItemNo + 1].Text);
                  vCurItem.Text := vCurItem.Text + Items[vCurItemNo + 1].Text;

                  UndoAction_DeleteItem(vCurItemNo + 1, 0);
                  Items.Delete(vCurItemNo + 1);

                  Inc(vDelCount);
                end
                else// ��һ�ζ��ײ��ǿ���Ҳ���ܺϲ�
                  Items[vCurItemNo + 1].ParaFirst := False;

                // ������һ�κϲ�������Item����ʽ��������ʽ
                vParaNo := Items[vCurItemNo].ParaNo;
                for i := vCurItemNo + 1 to vFormatLastItemNo - vDelCount do
                  Items[i].ParaNo := vParaNo;
              end;

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - vDelCount, -vDelCount);
            end;
          end;
        end
        else  // ��һ�����ܺϲ�Ҳ���Ƕ��ף��ƶ�����һ����ͷ�ٵ���DeleteKeyDown
        begin
          SelectInfo.StartItemNo := vCurItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;  // GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
          KeyDown(Key, Shift);
          Exit;
        end;
      end;
    end
    else  // ��겻��Item���ұ�
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

      if vText = '' then  // ɾ����û��������
      begin
        if not DrawItems[Items[vCurItemNo].FirstDItemNo].LineFirst then  // ��Item��������(�����м����ĩβ)
        begin
          if vCurItemNo < Items.Count - 1 then  // ��������Ҳ�������һ��Item
          begin
            if MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1]) then  // ��һ���ɺϲ�����һ��
            begin
              vLen := Items[vCurItemNo + 1].Length;

              Undo_New;
              UndoAction_InsertText(vCurItemNo - 1, Items[vCurItemNo - 1].Length - vLen + 1, Items[vCurItemNo + 1].Text);

              GetFormatRange(vCurItemNo - 1, vLen, vFormatFirstDrawItemNo, vFormatLastItemNo);
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              UndoAction_DeleteItem(vCurItemNo, 0);
              Items.Delete(vCurItemNo);  // ɾ����ǰ

              UndoAction_DeleteItem(vCurItemNo, 0);
              Items.Delete(vCurItemNo);  // ɾ����һ��

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 2, -2);
            end
            else  // ��һ���ϲ�������һ��
            begin
              vLen := 0;
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              Undo_New;
              UndoAction_DeleteItem(vCurItemNo, 0);
              Items.Delete(vCurItemNo);

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
            end;

            // �������
            SelectInfo.StartItemNo := vCurItemNo - 1;
            if GetItemStyle(SelectInfo.StartItemNo) < THCStyle.Null then
              SelectInfo.StartItemOffset := OffsetAfter
            else
              SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length - vLen;
          end
          else  // �����һ��Itemɾ������
          begin
            // �������
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            Undo_New;
            UndoAction_DeleteItem(vCurItemNo, 0);
            Items.Delete(vCurItemNo);

            SelectInfo.StartItemNo := vCurItemNo - 1;
            SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
          end;
        end
        else  // ����Item��ɾ����
        begin
          if vCurItemNo <> vFormatLastItemNo then  // ��ǰ�κ��滹��Item
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
          else  // ��ǰ��ɾ������
          begin
            // ��ֹ��֧�ּ��������޸����ݵ�����Ԫ�Ƕε�һ�����ü���ɾ�����ݺ�
            // ���ü�������ʱ����Ԫ���������ݵ�����(������Ԫ��֧���ֶ��޸�����)
            // ����ʹ����ɾ��Ϊ�յ�Item�ٲ����Item 20190802001
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
      else  // ɾ����������
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

  {$REGION ' BackspaceKeyDown ��ǰɾ���� '}
  procedure BackspaceKeyDown;
  var
    vText: string;
    i, vCurItemNo, vLen, vParaNo: Integer;
    vParaFirst, vPageBreak: Boolean;
    vParaStyle: THCParaStyle;
    vItem: THCCustomItem;
  begin
    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actBackDeleteText) then  // ������ɾ��
    begin
      //LeftKeyDown  // ��ǰ��
      SelectInfo.StartItemOffset := 0;
      ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
      Exit;
    end;

    if SelectInfo.StartItemOffset = 0 then  // �����Item�ʼ
    begin
      if (vCurItem.Text = '') and (Style.ParaStyles[vCurItem.ParaNo].AlignHorz <> TParaAlignHorz.pahJustify) then
        ApplyParaAlignHorz(TParaAlignHorz.pahJustify)  // ���еȶ���Ŀ�Item��ɾ��ʱ�л�����ɢ����
      else
      if vCurItem.ParaFirst and (Style.ParaStyles[vCurItem.ParaNo].FirstIndent > 0) then  // �ڶ���ǰ��ɾ��
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
      if SelectInfo.StartItemNo <> 0 then  // ���ǵ�1��Item��ǰ��ɾ��
      begin
        //vCurItemNo := SelectInfo.StartItemNo;
        if vCurItem.ParaFirst then  // �Ƕ���ʼItem
        begin
          vLen := Items[SelectInfo.StartItemNo - 1].Length;

          //if (vCurItem.ClassType = Items[SelectInfo.StartItemNo - 1].ClassType)
          //  and (vCurItem.StyleNo = Items[SelectInfo.StartItemNo - 1].StyleNo)
          if vCurItem.CanConcatItems(Items[SelectInfo.StartItemNo - 1]) then  // ��ǰ���Ժ���һ���ϲ�(��ǰ�ڶ���) 201804111209 (������MergeItemText�����)
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

            // ������һ�κϲ�������Item�Ķ���ʽ��������ʽ
            vParaNo := Items[SelectInfo.StartItemNo - 1].ParaNo;
            if vParaNo <> vCurItem.ParaNo then  // 2��ParaNo��ͬ
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
          else  // ��ǰɾ���Ҷε�һ��Item���ܺ���һ�����Item�ϲ�
          if IsEmptyLine(SelectInfo.StartItemNo - 1) then  // ��һ���ǿն�
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
            if vCurItem.Length = 0 then  // �Ѿ�û��������(���ǵ�1��Item��˵���ǿ���)
            begin
              vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo - 1, vLen);
              FormatPrepare(vFormatFirstDrawItemNo, SelectInfo.StartItemNo);

              Undo_New;
              UndoAction_DeleteItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
              Items.Delete(SelectInfo.StartItemNo);

              ReFormatData(vFormatFirstDrawItemNo, SelectInfo.StartItemNo - 1, -1);

              ReSetSelectAndCaret(SelectInfo.StartItemNo - 1);
            end
            else  // ��ǰɾ���Ҷε�һ��Item���ܺ���һ�����Item�ϲ�����һ�β��ǿ���
            begin
              vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo - 1, GetItemOffsetAfter(SelectInfo.StartItemNo - 1));
              vFormatLastItemNo := GetParaLastItemNo(SelectInfo.StartItemNo);
              //GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
              FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

              Undo_New;
              UndoAction_ItemParaFirst(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, False);

              vCurItem.ParaFirst := False;  // ��ǰ�κ���һ��Itemƴ�ӳ�һ��

              vParaNo := Items[SelectInfo.StartItemNo - 1].ParaNo;  // ��һ�ε�ParaNo
              if vParaNo <> vCurItem.ParaNo then  // 2��ParaNo��ͬ
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
        else  // �ڲ��ǵ�1��Item��ʼ��ǰɾ����Item���Ƕ���ʼ
        begin
          // ��ǰһ�����ʼ���´���
          SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
          SelectInfo.StartItemOffset := GetItemOffsetAfter(SelectInfo.StartItemNo);
          CaretDrawItemNo := Items[SelectInfo.StartItemNo].FirstDItemNo;
          KeyDown(Key, Shift);
          Exit;
        end;
      end;
    end
    else  // ��겻��Item�ʼ  �ı�TextItem
    begin
      if vCurItem.Length = 1 then  // ɾ����û��������
      begin
        vCurItemNo := SelectInfo.StartItemNo;  // ��¼ԭλ��
        if not DrawItems[Items[vCurItemNo].FirstDItemNo].LineFirst then  // ��ǰ�������ף�ǰ��������
        begin
          vLen := Items[vCurItemNo - 1].Length;

          if (vCurItemNo > 0) and (vCurItemNo < vParaLastItemNo)  // ���Ƕ����һ��
            and MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1])
          then  // ��ǰItemλ����һ���͵�ǰItemλ����һ���ɺϲ�
          begin
            Undo_New;
            UndoAction_InsertText(vCurItemNo - 1, Items[vCurItemNo - 1].Length - Items[vCurItemNo + 1].Length + 1,
              Items[vCurItemNo + 1].Text);

            GetFormatRange(vCurItemNo - 1, vLen, vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
            Items.Delete(vCurItemNo);  // ɾ����ǰ

            UndoAction_DeleteItem(vCurItemNo, 0);
            Items.Delete(vCurItemNo);  // ɾ����һ��

            ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 2, -2);

            ReSetSelectAndCaret(SelectInfo.StartItemNo - 1, vLen);  // ��һ��ԭ���λ��
          end
          else  // ��ǰ�������ף�ɾ����û�������ˣ��Ҳ��ܺϲ���һ������һ��
          begin
            if SelectInfo.StartItemNo = vParaLastItemNo then  // �����һ��
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
            else  // ���Ƕ����һ��
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
        else  // Item���е�һ��������Itemɾ������
        begin
          if Items[vCurItemNo].ParaFirst then  // �Ƕ��ף�ɾ������
          begin
            GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
            FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

            if vCurItemNo < vFormatLastItemNo then  // ͬ�κ��滹������
            begin
              Undo_New;

              vParaFirst := True;  // Items[vCurItemNo].ParaFirst;  // ��¼����Item�Ķ�����

              UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
              Items.Delete(vCurItemNo);

              if vParaFirst then  // ɾ�����Ƕ���
              begin
                UndoAction_ItemParaFirst(vCurItemNo, 0, vParaFirst);
                Items[vCurItemNo].ParaFirst := vParaFirst;  // ���̳ж�������
              end;

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
              ReSetSelectAndCaret(vCurItemNo, 0);  // ��һ����ǰ��
            end
            else  // �Ƕ���ɾ���գ�ͬ�κ���û�������ˣ���ɿ���
            begin
              // ��ֹ��֧�ּ��������޸����ݵ�����Ԫ�Ƕε�һ�����ü���Backspaceɾ�����ݺ�
              // ���ü�������ʱ����Ԫ���������ݵ�����(������Ԫ��֧���ֶ��޸�����)
              // ����ʹ����ɾ��Ϊ�յ�Item�ٲ����Item 20190802001
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

              ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);  // ��������
            end;
          end
          else  // ���Ƕ���Item����������Itemɾ������
          begin
            Undo_New;

            if vCurItemNo < GetParaLastItemNo(vCurItemNo) then  // ���ɾ����ͬ�κ��滹������
            begin
              vLen := Items[vCurItemNo - 1].Length;
              if MergeItemText(Items[vCurItemNo - 1], Items[vCurItemNo + 1]) then  // ǰ���ܺϲ�
              begin
                UndoAction_InsertText(vCurItemNo - 1,
                  Items[vCurItemNo - 1].Length - Items[vCurItemNo + 1].Length + 1, Items[vCurItemNo + 1].Text);

                GetFormatRange(vCurItemNo - 1, GetItemOffsetAfter(vCurItemNo - 1), vFormatFirstDrawItemNo, vFormatLastItemNo);  // ȡǰһ����ʽ����ʼλ��
                FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

                UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);  // ɾ���յ�Item
                Items.Delete(vCurItemNo);

                UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);  // ���ϲ���Item
                Items.Delete(vCurItemNo);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 2, -2);
                ReSetSelectAndCaret(vCurItemNo - 1, vLen);
              end
              else  // ǰ���ܺϲ�
              begin
                GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
                FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

                UndoAction_DeleteItem(vCurItemNo, Items[vCurItemNo].Length);
                Items.Delete(vCurItemNo);

                ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - 1, -1);
                ReSetSelectAndCaret(vCurItemNo - 1);
              end;
            end
            else  // ͬ�κ���û��������
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
      else  // ɾ���������� ��겻��Item�ʼ���ı�Item
      begin
        GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
        FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

        DoItemAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actBackDeleteText);
        vText := vCurItem.Text;  // ������ 201806242257 ��һ��

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
    Self.InitializeMouseField;  // ���Itemɾ�����ˣ�ԭMouseMove��ItemNo���ܲ������ˣ���MouseMoveʱ����ɵĳ���

  vCurItem := GetActiveItem;
  if not Assigned(vCurItem) then Exit;  // ��ҳ�ϲ�ʱ���ϲ���û�е�ǰItem

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
      VK_BACK:   BackspaceKeyDown;  // ��ɾ
      VK_RETURN: EnterKeyDown;      // �س�
      VK_LEFT:   LeftKeyDown;       // �����
      VK_RIGHT:  RightKeyDown;      // �ҷ����
      VK_DELETE: DeleteKeyDown;     // ɾ����
      VK_HOME:   HomeKeyDown;       // Home��
      VK_END:    EndKeyDown;        // End��
      VK_UP:     UpKeyDown;         // �Ϸ����
      VK_DOWN:   DownKeyDown;       // �·����
      VK_TAB:    TABKeyDown;        // TAB��
    end;
  end;

  case Key of
    VK_BACK, VK_DELETE, VK_RETURN, VK_TAB:
      begin
        Style.UpdateInfoRePaint;
        Style.UpdateInfoReCaret;  // ɾ��������λ�ù��Ϊ��ǰ��ʽ
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
  if not Assigned(vCarteItem) then Exit;  // ��ҳ�ϲ�ʱ���ϲ���û�е�ǰItem

  if (vCarteItem.StyleNo < THCStyle.Null)  // ��ǰλ���� RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // ��������������
  then
  begin
    Undo_New;

    vRectItem := vCarteItem as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    vRectItem.KeyPress(Key);
    if vRectItem.IsFormatDirty then
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      if Key <> #0 then
        ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);

      vRectItem.IsFormatDirty := False;
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
  FReadOnly := False;
  if not CanEdit then Exit;
  //Self.InitializeField;  LoadFromStream�е�Clear������
  inherited DoLoadFromStream(AStream, AStyle, AFileVersion);

  Self.BeginFormat;
  try
    FCheckEmptyItem := False;
    InsertStream(AStream, AStyle, AFileVersion);
    if AFileVersion > 48 then

    else
    if AFileVersion > 47 then
    begin
      AStream.ReadBuffer(FReadOnly, SizeOf(FReadOnly));
      FReadOnly := False;
    end;

    if FCheckEmptyItem then
      DeleteEmptyItem;

    ReSetSelectAndCaret(0, 0);
  finally
    Self.EndFormat;
  end;
end;

function THCRichData.MergeTableSelectCells: Boolean;
begin
  if not CanEdit then Exit(False);

  Result := RectItemAction(GetActiveItemNo, function(const ARectItem: THCCustomRectItem): Boolean
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
  FSelecting := False;  // ׼����ѡ
  FDraging := False;  // ׼����ק
  FMouseLBDouble := False;
  FMouseDownReCaret := False;
  vMouseDownInSelect := False;
  //FSelectSeekOffset := -1;

  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);

  FMouseDownX := X;
  FMouseDownY := Y;

  GetItemAt(X, Y, vMouseDownItemNo, vMouseDownItemOffset, vDrawItemNo, vRestrain);
  FSelectSeekNo := vMouseDownItemNo;
  FSelectSeekOffset := vMouseDownItemOffset;

  if (Button = mbLeft) and (ssShift in Shift) then  // shift������ȷ��ѡ�з�Χ
  begin
    FMouseDownItemNo := vMouseDownItemNo;
    FMouseDownItemOffset := vMouseDownItemOffset;

    if (not vRestrain)
      and (Items[FMouseDownItemNo].StyleNo < THCStyle.Null)
      and (vMouseDownItemOffset = OffsetInner)
    then  // RectItem
      DoItemMouseDown(FMouseDownItemNo, FMouseDownItemOffset)
    else
    if SelectByMouseDownShift(vMouseDownItemNo, vMouseDownItemOffset) then
    begin
      MatchItemSelectState;  // ����ѡ�з�Χ�ڵ�Itemѡ��״̬
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;

      FMouseDownItemNo := vMouseDownItemNo;
      FMouseDownItemOffset := vMouseDownItemOffset;
      FSelectSeekNo := vMouseDownItemNo;
      FSelectSeekOffset := vMouseDownItemOffset;
    end;

    Exit;
  end;

  vMouseDownInSelect := CoordInSelect(X, Y, vMouseDownItemNo, vMouseDownItemOffset, vRestrain);

  if vMouseDownInSelect then   // ��ѡ�������а���
  begin
    if Items[vMouseDownItemNo].StyleNo < THCStyle.Null then
      DoItemMouseDown(vMouseDownItemNo, vMouseDownItemOffset);

    if FMouseLBDowning and not SelectedResizing then
    begin
      FDraging := True;
      Style.UpdateInfo.DragingSelected := True;
    end
    else
    begin
      FMouseDownItemNo := vMouseDownItemNo;
      FMouseDownItemOffset := vMouseDownItemOffset;
    end;
  end
  else  // û����ѡ��������
  begin
    if (vMouseDownItemNo <> FMouseDownItemNo)
      or (vMouseDownItemOffset <> FMouseDownItemOffset)
      or (CaretDrawItemNo <> vDrawItemNo)
    then  // λ�÷����仯
    begin
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
      FMouseDownReCaret := True;

      DisSelect;

      // ���¸�ֵ��λ��
      FMouseDownItemNo := vMouseDownItemNo;
      FMouseDownItemOffset := vMouseDownItemOffset;
      if (not SelectInfo.StartRestrain) and vRestrain then  // RectItem�Ӽ����Ϊ�Ǽ���
        Items[FMouseDownItemNo].Active := False;
      {if not vRestrain then  // û����
        Items[FMouseDownItemNo].Active := True;}

      SelectInfo.StartItemNo := FMouseDownItemNo;
      SelectInfo.StartItemOffset := FMouseDownItemOffset;
      SelectInfo.StartRestrain := vRestrain;
      CaretDrawItemNo := vDrawItemNo;
    end;

    //if not vRestrain then  // û���������ҳItem�����ǰ��λ��ʱ��Ҫ�������������Բ�������
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
    CoordToDrawItem(X, Y, FMouseMoveDrawItemNo, vX, vY);
    if ADrawItemMouseMove then  // ���Լ����津�����������MouseMove�ڲ���������֤����괦���ڲ�
      DoDrawItemMouseMove(Self, AItemNo, AOffset, FMouseMoveDrawItemNo, TMouseButton.mbLeft, Shift, vX, vY);

    Items[AItemNo].MouseMove(Shift, vX, vY);
  end;
  {$ENDREGION}

var
  vMouseMoveItemNo, vMouseMoveItemOffset: Integer;
  vRestrain: Boolean;
begin
  if SelectedResizing then  // RectItem resizing��goon
  begin
    FMouseMoveItemNo := FMouseDownItemNo;
    FMouseMoveItemOffset := FMouseDownItemOffset;
    FMouseMoveDrawItemNo := GetDrawItemNoByOffset(FMouseMoveItemNo, FMouseMoveItemOffset);
    FMouseMoveRestrain := False;
    DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
    Style.UpdateInfoRePaint;

    Exit;
  end;

  GetItemAt(X, Y, vMouseMoveItemNo, vMouseMoveItemOffset, FMouseMoveDrawItemNo, vRestrain);

  if FDraging or Style.UpdateInfo.DragingSelected then  // ��ק
  begin
    GCursor := crDrag;

    FMouseMoveItemNo := vMouseMoveItemNo;
    FMouseMoveItemOffset := vMouseMoveItemOffset;
    // ���ӱ�����϶��Ƶ���Ԫ���û�о���MouseDown����ѡ����ʼ��Ϣ�ǿյģ���ɹ����Ϣ����
    SelectInfo.StartItemNo := vMouseMoveItemNo;
    SelectInfo.StartItemOffset := vMouseMoveItemOffset;
    FMouseMoveRestrain := vRestrain;
    CaretDrawItemNo := FMouseMoveDrawItemNo;

    Style.UpdateInfoReCaret;

    if (not vRestrain) and (Items[FMouseMoveItemNo].StyleNo < THCStyle.Null) then  // RectItem
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
  end
  else
  if FSelecting then  // ��ѡ
  begin
    if (Items[FMouseDownItemNo].StyleNo < THCStyle.Null)
      and (FMouseDownItemOffset = OffsetInner)
    then  // ����ʱ��RectItem�ϣ���ѡ�����ڴ�RectItem�����Ҫ�������ǵ�һ��Item��
    begin // ��һ����Ԫ��Ӻ���ǰ��ѡȫ�����Ƴ������������ݵ��滻�����ǰ������⡣
      FMouseMoveItemNo := FMouseDownItemNo;
      FMouseMoveItemOffset := FMouseDownItemOffset;

      if vMouseMoveItemNo = FMouseDownItemNo then  // �ڰ��µ�RectItem���ƶ�
        FMouseMoveRestrain := vRestrain
      else  // ����ΪԼ��
        FMouseMoveRestrain := True;

      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);
      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;

      Exit;  // ������RectItem�ڲ�����
    end
    else
    begin
      FMouseMoveItemNo := vMouseMoveItemNo;
      FMouseMoveItemOffset := vMouseMoveItemOffset;
      FMouseMoveRestrain := vRestrain;
    end;

    if (Items[FMouseMoveItemNo].StyleNo < THCStyle.Null)
      and (FMouseDownItemOffset = OffsetInner)
    then  // ��ѡ RectItem
      DoItemMouseMove(FMouseMoveItemNo, FMouseMoveItemOffset);

    AdjustSelectRange(FMouseDownItemNo, FMouseDownItemOffset,
      FMouseMoveItemNo, FMouseMoveItemOffset);  // ȷ��SelectRang

    FSelectSeekNo := FMouseMoveItemNo;
    FSelectSeekOffset := FMouseMoveItemOffset;

    if Self.SelectExists then
      MatchItemSelectState  // ����ѡ�з�Χ�ڵ�Itemѡ��״̬
    else
      CaretDrawItemNo := FMouseMoveDrawItemNo;  // ������һ��DrawItem��󣬻�ѡ����һ����ʼʱ��û��ѡ�����ݣ�Ҫ����CaretDrawIemNo

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
  end
  else  // ����ק���ǻ�ѡ
  if FMouseLBDowning and ((FMouseDownX <> X) or (FMouseDownY <> Y)) then  // ��������ƶ�����ʼ��ѡ
  begin
    FSelecting := True;
    Style.UpdateInfo.Selecting := True;
  end
  else  // ����ק���ǻ�ѡ���ǰ���
  begin
    if vMouseMoveItemNo <> FMouseMoveItemNo then  // �ƶ������µ�Item��
    begin
      if FMouseMoveItemNo >= 0 then  // �ɵ��Ƴ�
        DoItemMouseLeave(FMouseMoveItemNo);

      if (vMouseMoveItemNo >= 0) and (not vRestrain) then  // �µ�����
        DoItemMouseEnter(vMouseMoveItemNo);

      Style.UpdateInfoRePaint;
    end
    else  // �����ƶ�����Item����һ����ͬһ��(������һֱ��һ��Item���ƶ�)
    begin
      if vRestrain <> FMouseMoveRestrain then  // ����Move���ϴ�Move��ͬһ��Item��2�ε����������˱仯
      begin
        if (not FMouseMoveRestrain) and vRestrain then  // �ϴ�û���������������ˣ��Ƴ�
        begin
          if FMouseMoveItemNo >= 0 then
            DoItemMouseLeave(FMouseMoveItemNo);
        end
        else
        if FMouseMoveRestrain and (not vRestrain) then  // �ϴ����������β�����������
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

    if not FMouseDownReCaret then  // �����ظ���ȡ���λ��
      Style.UpdateInfoReCaret;

    //if Items[vUpItemNo].StyleNo < THCStyle.Null then  // RectItem
      DoItemMouseUp(vUpItemNo, vUpItemOffset);  // ������Ϊ�������Ƴ�Item�����������ﲻ��vRestrainԼ��
  end;
  {$ENDREGION}

var
  i, vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  //if not FMouseLBDowning then Exit;  // ����OpenDialog�Ի���˫������ĵ���
  FMouseLBDowning := False;

  if FMouseLBDouble then Exit;

  if (Button = mbLeft) and (ssShift in Shift) then Exit;  // shift������ȷ��ѡ�з�Χ

  vRestrain := False;
  if SelectedResizing then  // RectItem����ing��ֹͣ����
  begin
    Undo_New;
    UndoAction_ItemSelf(FMouseDownItemNo, FMouseDownItemOffset);

    DoItemMouseUp(FMouseDownItemNo, FMouseDownItemOffset);
    DoItemResized(FMouseDownItemNo);  // ��������¼�(�ɿ������Ų�Ҫ����ҳ��)
    GetFormatRange(FMouseDownItemNo, FMouseDownItemOffset, vFormatFirstDrawItemNo, vFormatLastItemNo);
    FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
    ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
    Style.UpdateInfoRePaint;

    Exit;
  end;

  GetItemAt(X, Y, vUpItemNo, vUpItemOffset, vDrawItemNo, vRestrain);

  if FSelecting or Style.UpdateInfo.Selecting then  // ��ѡ��ɵ���
  begin
    FSelecting := False;

    // ѡ�з�Χ�ڵ�RectItemȡ����ѡ״̬(��ʱ����FSelectingΪTrue)
    if SelectInfo.StartItemNo >= 0 then  // ���ȡ��ѡ�к�Ԫ��Data��StartItemNo����Ϊ-1���´α����ѡ�������ʱ��UpdateInfo.SelectingΪTrue����StartItemNo��EndItemNo����-1����
    begin
      for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
      begin
        if (i <> vUpItemNo) and (Items[i].StyleNo < THCStyle.Null) then
          DoItemMouseUp(i, 0);
      end;
    end;

    if SelectInfo.EndItemNo < 0 then  // ��RectItem�ﻮѡ��TextItem��ѡ����ʼλ��
    begin
      if (FMouseDownItemNo >= 0) and (Items[FMouseDownItemNo].StyleNo < THCStyle.Null) then  // ����ʱ��RectItem
        DoItemMouseUp(FMouseDownItemNo, OffsetInner)
      else
        DoItemMouseUp(vUpItemNo, vUpItemOffset);
    end
    else
    if Items[vUpItemNo].StyleNo < THCStyle.Null then  // ����ʱ��RectItem
      DoItemMouseUp(vUpItemNo, vUpItemOffset);
  end
  else
  if FDraging or Style.UpdateInfo.DragingSelected then  // ��ק����
  begin
    FDraging := False;
    //vMouseUpInSelect := CoordInSelect(X, Y, vUpItemNo, vUpItemOffset, vRestrain);

    // ��ʱ��֧����ק
    {if not vMouseUpInSelect then  // ��ק����ʱ����ѡ��������
    begin
      //to do: ȡ��קѡ�е�����
      DeleteSelected;  // ɾ��ѡ������
    end
    else}  // ��ק����ʱ��ѡ��������
    begin
      // �������λ��֮���Itemѡ��״̬�������Լ��������𴦲���ѡ�з�Χ��ʱ
      // ��֤����ȡ��(��ItemA��ѡ����ק����һ��ItemBʱ��ItemAѡ��״̬��Ҫȡ��)
      // ��201805172309����
      if SelectInfo.StartItemNo >= 0 then  // ����ʱ�ĵ�Ԫ�񲢲��ǰ���ʱ�ģ������SelectInfo.StartItemNo < 0�����
      begin
        for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
          Items[i].DisSelect;
        {if SelectInfo.StartItemNo <> vUpItemNo then
        begin
          Items[SelectInfo.StartItemNo].DisSelect;
          //Items[SelectInfo.StartItemNo].Active := False;
        end;
        // ѡ�з�Χ������Itemȡ��ѡ��
        for i := SelectInfo.StartItemNo + 1 to SelectInfo.EndItemNo do  // ��������λ��֮�������Item
        begin
          if i <> vUpItemNo then
          begin
            Items[i].DisSelect;
            //Items[i].Active := False;
          end;
        end;}
      end;
    end;

    // Ϊ��ק���׼��
    FMouseMoveItemNo := vUpItemNo;
    FMouseMoveItemOffset := vUpItemOffset;
    FMouseMoveDrawItemNo := vDrawItemNo;  // ���������һ����ͼƬ�����ѡ�к��ٵ����Ϊ���϶�����ȡ����Ԫ���ѡ�����FMouseMoveDrawItemNoΪ-1
    // Ϊ��һ�ε��ʱ�����һ�ε��ѡ����׼��
    FMouseDownItemNo := vUpItemNo;
    FMouseDownItemOffset := vUpItemOffset;

    DoNormalMouseUp;  // �����Լ�����Itemѡ��״̬�����Ե���Ϊ��ǰ�༭λ��

    SelectInfo.EndItemNo := -1;
    SelectInfo.EndItemOffset := -1;
  end
  else  // ����ק���ǻ�ѡ
  begin
    if SelectExists(False) then  // �����Data�����ڵ�ѡ��
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
  if not Assigned(vActiveItem) then Exit;  // ��ҳ�ϲ�ʱ���ϲ���û�е�ǰItem

  if (vActiveItem.StyleNo < THCStyle.Null)  // ��ǰλ���� RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // ��������������
  then
  begin
    Undo_New;

    vRectItem := vActiveItem as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    vRectItem.ActiveItemReAdaptEnvironment;
    if vRectItem.IsFormatDirty then
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
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
    if MergeItemToNext(vItemNo) then  // ���Ժϲ�����һ��Item
    begin
      UndoAction_InsertText(vItemNo, Items[vItemNo].Length - Items[vItemNo + 1].Length + 1, Items[vItemNo + 1].Text);
      UndoAction_DeleteItem(vItemNo + 1, 0);
      Items.Delete(vItemNo + 1);
      Dec(vExtraCount);
    end;

    if vItemNo > 0 then  // ��ǰ�ϲ�
    begin
      vLen := Items[vItemNo - 1].Length;
      if MergeItemToPrio(vItemNo) then  // ��ǰItem�ϲ�����һ��Item(�������ϲ��ˣ�vItem�Ѿ�ʧЧ������ֱ��ʹ����)
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

  if Self.SelectExists then  // ԭ������ѡ��
  begin
    if IsSelectSeekStart then  // ��һ�λ�ѡ��ɺ�����ѡ����ʼ
    begin
      if (AMouseDownItemNo < FSelectSeekNo)
        or ((AMouseDownItemNo = FSelectSeekNo) and (AMouseDownItemOffset < FSelectSeekOffset))
      then  // ������ԭѡ�з�Χ��ʼλ��ǰ��
      begin
        vSelItemNo := SelectInfo.EndItemNo;
        vSelItemOffset := SelectInfo.EndItemOffset;

        AdjustSelectRange(AMouseDownItemNo, AMouseDownItemOffset, vSelItemNo, vSelItemOffset);  // ȷ��SelectRang
      end
      else
      if ((AMouseDownItemNo > FSelectSeekNo) and (AMouseDownItemNo < SelectInfo.EndItemNo))
        or ((AMouseDownItemNo = FSelectSeekNo) and (AMouseDownItemOffset > FSelectSeekOffset))
        or ((AMouseDownItemNo = SelectInfo.EndItemNo) and (AMouseDownItemOffset < SelectInfo.EndItemOffset))
      then  // ��ԭѡ�з�Χ��ʼ�ͽ����м�
      begin
        vSelItemNo := SelectInfo.EndItemNo;
        vSelItemOffset := SelectInfo.EndItemOffset;

        AdjustSelectRange(AMouseDownItemNo, AMouseDownItemOffset, vSelItemNo, vSelItemOffset);  // ȷ��SelectRang
      end
      else
      if (AMouseDownItemNo > SelectInfo.EndItemNo)
        or ((AMouseDownItemNo = SelectInfo.EndItemNo) and (AMouseDownItemOffset > SelectInfo.EndItemOffset))
      then  // �ڽ���λ�ú���
      begin
        vSelItemNo := SelectInfo.EndItemNo;
        vSelItemOffset := SelectInfo.EndItemOffset;

        AdjustSelectRange(vSelItemNo, vSelItemOffset, AMouseDownItemNo, AMouseDownItemOffset);  // ȷ��SelectRang
      end
      else
        Result := False;
    end
    else  // ��ѡ��ɺ����ڽ���
    begin
      if (AMouseDownItemNo > FSelectSeekNo)
        or ((AMouseDownItemNo = FSelectSeekNo) and (AMouseDownItemOffset > FSelectSeekOffset))
      then  // ������ԭѡ�з�Χ����λ�ú���
      begin
        vSelItemNo := SelectInfo.StartItemNo;
        vSelItemOffset := SelectInfo.StartItemOffset;

        AdjustSelectRange(vSelItemNo, vSelItemOffset, AMouseDownItemNo, AMouseDownItemOffset);  // ȷ��SelectRang
      end
      else
      if ((AMouseDownItemNo > SelectInfo.StartItemNo) and (AMouseDownItemNo < FSelectSeekNo))
        or ((AMouseDownItemNo = FSelectSeekNo) and (AMouseDownItemOffset < FSelectSeekOffset))
        or ((AMouseDownItemNo = SelectInfo.StartItemNo) and (AMouseDownItemOffset > SelectInfo.StartItemOffset))
      then  // ��ԭѡ�з�Χ��ʼ�ͽ����м�
      begin
        vSelItemNo := SelectInfo.StartItemNo;
        vSelItemOffset := SelectInfo.StartItemOffset;

        AdjustSelectRange(vSelItemNo, vSelItemOffset, AMouseDownItemNo, AMouseDownItemOffset);  // ȷ��SelectRang
      end
      else
      if (AMouseDownItemNo < SelectInfo.StartItemNo)
        or ((AMouseDownItemNo = SelectInfo.StartItemNo) and (AMouseDownItemOffset < SelectInfo.StartItemOffset))
      then  // ����ʼλ��ǰ��
      begin
        vSelItemNo := SelectInfo.StartItemNo;
        vSelItemOffset := SelectInfo.StartItemOffset;

        AdjustSelectRange(AMouseDownItemNo, AMouseDownItemOffset, vSelItemNo, vSelItemOffset);  // ȷ��SelectRang
      end
      else
       Result := False;
    end;
  end
  else  // ԭ��û��ѡ��
  if SelectInfo.StartItemNo >= 0 then
  begin
    if (AMouseDownItemNo < SelectInfo.StartItemNo)
      or ((AMouseDownItemNo = SelectInfo.StartItemNo) and (AMouseDownItemOffset < SelectInfo.StartItemOffset))
    then  // ������ԭ���λ��ǰ��
    begin
      vSelItemNo := SelectInfo.StartItemNo;
      vSelItemOffset := SelectInfo.StartItemOffset;

      AdjustSelectRange(AMouseDownItemNo, AMouseDownItemOffset, vSelItemNo, vSelItemOffset);  // ȷ��SelectRang
    end
    else
    if (AMouseDownItemNo > SelectInfo.StartItemNo)
      or ((AMouseDownItemNo = SelectInfo.StartItemNo) and (AMouseDownItemOffset > SelectInfo.StartItemOffset))
    then  // ������ԭ���λ�ú���
    begin
      vSelItemNo := SelectInfo.StartItemNo;
      vSelItemOffset := SelectInfo.StartItemOffset;

      AdjustSelectRange(vSelItemNo, vSelItemOffset, AMouseDownItemNo, AMouseDownItemOffset);  // ȷ��SelectRang
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
  if SelectInfo.EndItemNo >= 0 then  // ��ѡʱĳRectItemֻ��һ����ѡ��ʱ������,��ȡ��ѡ��
  begin
    for i := SelectInfo.StartItemNo to SelectInfo.EndItemNo do
    begin
      if (Self.Items[i].StyleNo < THCStyle.Null) and (Self.Items[i].IsSelectPart) then
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
  if not Assigned(vActiveItem) then Exit;  // ��ҳ�ϲ�ʱ���ϲ���û�е�ǰItem

  if (vActiveItem.StyleNo < THCStyle.Null)  // ��ǰλ���� RectItem
    and (SelectInfo.StartItemOffset = OffsetInner)  // ��������������
  then
  begin
    Undo_New;

    vRectItem := vActiveItem as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    vRectItem.SetActiveItemText(AText);
    if vRectItem.IsFormatDirty then
    begin
      GetFormatRange(vFormatFirstDrawItemNo, vFormatLastItemNo);
      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo);
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

    if not DoAcceptAction(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, actSetItemText) then Exit;  // ����������

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
    Items.Add(vItem);  // ��ʹ��InsertText��Ϊ�����䴥��ReFormatʱ��Ϊû�и�ʽ��������ȡ������Ӧ��DrawItem

    ReFormat;
    //ReSetSelectAndCaret(0);  // ��ֹ��պ��ʽ����ɺ�û��ѡ����ʼ���ʳ���
  end;
end;

procedure THCRichData.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

end.
