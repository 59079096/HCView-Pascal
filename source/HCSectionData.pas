{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{                �ĵ��ڶ���߼�����Ԫ                 }
{                                                       }
{*******************************************************}

unit HCSectionData;

interface

uses
  Windows, Classes, Graphics, SysUtils, Controls, HCRichData,
  HCCustomData, HCPage, HCItem, HCDrawItem, HCCommon, HCStyle, HCParaStyle, HCTextStyle,
  HCViewData, HCCustomFloatItem, HCRectItem, HCXml;

type
  THCSectionData = class;

  TGetScreenCoordEvent = function (const X, Y: Integer): TPoint of object;
  // �����ĵ�ҳü��ҳ�š�ҳ��Data���࣬��Ҫ���ڴ����ĵ���Data�仯ʱ���е����Ի��¼�
  // ��ֻ��״̬�л���ҳü��ҳ�š�ҳ���л�ʱ��Ҫ֪ͨ�ⲿ�ؼ���������ؼ�״̬�仯��
  // ����Ԫ��ֻ���л�ʱ����Ҫ
  THCSectionData = class(THCViewData)
  private
    FOnReadOnlySwitch: TNotifyEvent;
    FOnGetScreenCoord: TGetScreenCoordEvent;

    FFloatItems: THCFloatItems;
    /// <summary> ��ǰѡ�е�FloatItem </summary>
    FFloatItemIndex,
    /// <summary> ��ǰ�������FloatItem </summary>
    FMouseDownIndex,
    /// <summary> ��ǰ����ƶ�����FloatItem </summary>
    FMouseMoveIndex: Integer;

    function GetFloatItemAt(const X, Y: Integer): Integer;
    procedure DoInsertFloatItem(const AItem: THCCustomItem);
  protected
    procedure SetReadOnly(const Value: Boolean); override;
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure UndoAction_FloatItemMirror(const AItemNo: Integer);
  public
    constructor Create(const AStyle: THCStyle); override;
    destructor Destroy; override;

    function MouseDownFloatItem(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function MouseMoveFloatItem(Shift: TShiftState; X, Y: Integer): Boolean;
    function MouseUpFloatItem(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function KeyDownFloatItem(var Key: Word; Shift: TShiftState): Boolean;

    procedure Clear; override;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: THCCaretInfo); override;
    function GetScreenCoord(const X, Y: Integer): TPoint; override;
    procedure TraverseFloatItem(const ATraverse: THCItemTraverse);

    function GetActiveItemNo: Integer; override;
    function GetActiveItem: THCCustomItem; override;
    function GetActiveFloatItem: THCCustomFloatItem;
    /// <summary> ���븡��Item </summary>
    function InsertFloatItem(const AFloatItem: THCCustomFloatItem): Boolean;

    procedure SaveToStream(const AStream: TStream); override;

    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    procedure PaintFloatItems(const APageIndex, ADataDrawLeft, ADataDrawTop,
      AVOffset: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;

    property FloatItemIndex: Integer read FFloatItemIndex;
    property ActiveFloatItem: THCCustomFloatItem read GetActiveFloatItem;
    property FloatItems: THCFloatItems read FFloatItems;
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;
    property OnGetScreenCoord: TGetScreenCoordEvent read FOnGetScreenCoord write FOnGetScreenCoord;
  end;

  THCHeaderData = class(THCSectionData);

  THCFooterData = class(THCSectionData);

  THCPageData = class(THCSectionData)  // ��������Ҫ������Ԫ��Data����Ҫ��������Ҫ�����Ի��¼�
  private
    FShowLineActiveMark: Boolean;  // ��ǰ�������ǰ��ʾ��ʶ
    FShowUnderLine: Boolean;  // �»���
    FShowLineNo: Boolean;  // �к�
  protected
    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;
  public
    constructor Create(const AStyle: THCStyle); override;

    procedure PaintFloatItems(const APageIndex, ADataDrawLeft, ADataDrawTop,
      AVOffset: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summary> �ӵ�ǰλ�ú��ҳ </summary>
    function InsertPageBreak: Boolean;
    //
    property ShowLineActiveMark: Boolean read FShowLineActiveMark write FShowLineActiveMark;
    property ShowLineNo: Boolean read FShowLineNo write FShowLineNo;
    property ShowUnderLine: Boolean read FShowUnderLine write FShowUnderLine;
  end;

implementation

{$I HCView.inc}

uses
  Math, HCUndo, HCTextItem, HCImageItem, HCTableItem, HCFloatBarCodeItem,
  HCFloatLineItem, HCShape;

{ THCPageData }

constructor THCPageData.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  FShowLineActiveMark := False;
  FShowUnderLine := False;
  FShowLineNo := False;
end;

procedure THCPageData.SaveToStream(const AStream: TStream);
begin
  AStream.WriteBuffer(FShowUnderLine, SizeOf(FShowUnderLine));
  inherited SaveToStream(AStream);
end;

procedure THCPageData.ToXml(const ANode: IHCXMLNode);
begin
  if FShowUnderLine then
    ANode.Attributes['SUL'] := FShowUnderLine;

  inherited ToXml(ANode);
end;

procedure THCPageData.DoDrawItemPaintAfter(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoDrawItemPaintAfter(AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADataDrawLeft,
    ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);;
  {$IFDEF SHOWITEMNO}
  if ADrawItemNo = Items[DrawItems[ADrawItemNo].ItemNo].FirstDItemNo then  //
  {$ENDIF}
  begin
    {$IFDEF SHOWITEMNO}
    DrawDebugInfo(ACanvas, ADrawRect.Left, ADrawRect.Top - 6, IntToStr(DrawItems[ADrawItemNo].ItemNo));
    {$ENDIF}

    {$IFDEF SHOWDRAWITEMNO}
    DrawDebugInfo(ACanvas, ADrawRect.Left, ADrawRect.Top - 6, IntToStr(ADrawItemNo));
    {$ENDIF}
  end;

  if FShowUnderLine then  // �»���
  begin
    if AData.IsLineLastDrawItem(ADrawItemNo) then
    begin
      ACanvas.Pen.Color := clBlack;
      ACanvas.Pen.Style := psSolid;

      // ���¶����ƣ���ֹ��ǰ�е�����Ԫ����ɫ����һ���������Ĩ��
      ACanvas.MoveTo(ADataDrawLeft, ADrawRect.Top);
      ACanvas.LineTo(ADataDrawLeft + Self.Width, ADrawRect.Top);

      ACanvas.MoveTo(ADataDrawLeft, ADrawRect.Bottom);
      ACanvas.LineTo(ADataDrawLeft + Self.Width, ADrawRect.Bottom);
    end;
  end;
end;

procedure THCPageData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vTop: Integer;
  vFont: TFont;
  i, vLineNo: Integer;
begin
  inherited DoDrawItemPaintBefor(AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADataDrawLeft,
    ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then
  begin
    if FShowLineActiveMark then  // ������ָʾ��
    begin
      if ADrawItemNo = GetSelectStartDrawItemNo then  // ��ѡ�е���ʼDrawItem
      begin
        ACanvas.Pen.Color := clBlue;
        ACanvas.Pen.Style := psSolid;
        vTop := ADrawRect.Top + DrawItems[ADrawItemNo].Height div 2;

        ACanvas.MoveTo(ADataDrawLeft - 10, vTop);
        ACanvas.LineTo(ADataDrawLeft - 11, vTop);

        ACanvas.MoveTo(ADataDrawLeft - 11, vTop - 1);
        ACanvas.LineTo(ADataDrawLeft - 11, vTop + 2);
        ACanvas.MoveTo(ADataDrawLeft - 12, vTop - 2);
        ACanvas.LineTo(ADataDrawLeft - 12, vTop + 3);
        ACanvas.MoveTo(ADataDrawLeft - 13, vTop - 3);
        ACanvas.LineTo(ADataDrawLeft - 13, vTop + 4);
        ACanvas.MoveTo(ADataDrawLeft - 14, vTop - 4);
        ACanvas.LineTo(ADataDrawLeft - 14, vTop + 5);
        ACanvas.MoveTo(ADataDrawLeft - 15, vTop - 2);
        ACanvas.LineTo(ADataDrawLeft - 15, vTop + 3);
        ACanvas.MoveTo(ADataDrawLeft - 16, vTop - 2);
        ACanvas.LineTo(ADataDrawLeft - 16, vTop + 3);
      end;
    end;

    if FShowLineNo then  // �к�
    begin
      if DrawItems[ADrawItemNo].LineFirst then
      begin
        vLineNo := 0;
        for i := 0 to ADrawItemNo do
        begin
          if DrawItems[i].LineFirst then
            Inc(vLineNo);
        end;

        vFont := TFont.Create;
        try
          vFont.Assign(ACanvas.Font);
          ACanvas.Font.Color := RGB(180, 180, 180);
          ACanvas.Font.Size := 10;
          ACanvas.Font.Style := [];
          ACanvas.Font.Name := 'Courier New';
          //SetTextColor(ACanvas.Handle, RGB(180, 180, 180));
          ACanvas.Brush.Style := bsClear;
          vTop := ADrawRect.Top + (ADrawRect.Bottom - ADrawRect.Top - 16) div 2;
          ACanvas.TextOut(ADataDrawLeft - 50, vTop, IntToStr(vLineNo));
        finally
          ACanvas.Font.Assign(vFont);
          FreeAndNil(vFont);
        end;
      end;
    end;
  end;
end;

function THCPageData.InsertPageBreak: Boolean;
var
  vKey: Word;
begin
  Result := False;

  if Self.SelectExists then Exit;  // ��ѡ��ʱ���ܷ�ҳ

  if (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)
    and (SelectInfo.StartItemOffset = OffsetInner)
  then  // ��֧����RectItem�ڲ���ҳ
    Exit;

  vKey := VK_RETURN;
  Self.KeyDown(vKey, [], True);

  Result := True;
end;

function THCPageData.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
begin
  // ��Ϊ����ճ��ʱ������ҪFShowUnderLine��Ϊ����ճ������FShowUnderLine��LoadFromStremʱ����
  //AStream.ReadBuffer(FShowUnderLine, SizeOf(FShowUnderLine));
  inherited InsertStream(AStream, AStyle, AFileVersion);
end;

procedure THCPageData.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  AStream.ReadBuffer(FShowUnderLine, SizeOf(FShowUnderLine));
  inherited DoLoadFromStream(AStream, AStyle, AFileVersion);
end;

procedure THCPageData.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vMouseDownItemNo, vMouseDownItemOffset: Integer;
begin
  if FShowLineActiveMark then  // ��ʾ��ǰ�༭��
  begin
    vMouseDownItemNo := Self.MouseDownItemNo;
    vMouseDownItemOffset := Self.MouseDownItemOffset;
    inherited MouseDown(Button, Shift, X, Y);
    if (vMouseDownItemNo <> Self.MouseDownItemNo) or (vMouseDownItemOffset <> Self.MouseDownItemOffset) then
      Style.UpdateInfoRePaint;
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure THCPageData.PaintFloatItems(const APageIndex, ADataDrawLeft,
  ADataDrawTop, AVOffset: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  i: Integer;
  vFloatItem: THCCustomFloatItem;
begin
  for i := 0 to FFloatItems.Count - 1 do
  begin
    vFloatItem := FFloatItems[i];

    if vFloatItem.PageIndex = APageIndex then
    begin
      vFloatItem.DrawRect := Bounds(vFloatItem.Left, vFloatItem.Top, vFloatItem.Width, vFloatItem.Height);
      vFloatItem.DrawRect.Offset(ADataDrawLeft, ADataDrawTop - AVOffset);  // ��������ʼλ��ӳ�䵽����λ��
      vFloatItem.PaintTo(Self.Style, vFloatItem.DrawRect, ADataDrawTop, 0,
        0, 0, ACanvas, APaintInfo);
    end;
  end;
end;

procedure THCPageData.ParseXml(const ANode: IHCXMLNode);
begin
  if ANode.HasAttribute('SUL') then
    FShowUnderLine := ANode.Attributes['SUL'];

  inherited ParseXml(ANode);
end;

{ THCSectionData }

procedure THCSectionData.Clear;
begin
  FFloatItemIndex := -1;
  FMouseDownIndex := -1;
  FMouseMoveIndex := -1;
  FFloatItems.Clear;

  inherited Clear;
end;

constructor THCSectionData.Create(const AStyle: THCStyle);
begin
  FFloatItems := THCFloatItems.Create;
  FFloatItems.OnInsertItem := DoInsertFloatItem;
  FFloatItemIndex := -1;
  FMouseDownIndex := -1;
  FMouseMoveIndex := -1;

  inherited Create(AStyle);
end;

destructor THCSectionData.Destroy;
begin
  FFloatItems.Free;
  inherited Destroy;
end;

function THCSectionData.GetActiveFloatItem: THCCustomFloatItem;
begin
  if FFloatItemIndex < 0 then
    Result := nil
  else
    Result := FFloatItems[FFloatItemIndex];
end;

function THCSectionData.GetActiveItem: THCCustomItem;
begin
  if FFloatItemIndex < 0 then
    Result := inherited GetActiveItem
  else
    Result := nil;
end;

function THCSectionData.GetActiveItemNo: Integer;
begin
  if FFloatItemIndex < 0 then
    Result := inherited GetActiveItemNo
  else
    Result := -1;
end;

procedure THCSectionData.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: THCCaretInfo);
begin
  if FFloatItemIndex >= 0 then
  begin
    ACaretInfo.Visible := False;
    Exit;
  end;

  inherited GetCaretInfo(AItemNo, AOffset, ACaretInfo);
end;

function THCSectionData.GetFloatItemAt(const X, Y: Integer): Integer;
var
  i: Integer;
  vFloatItem: THCCustomFloatItem;
begin
  Result := -1;
  for i := 0 to FFloatItems.Count - 1 do
  begin
    vFloatItem := FFloatItems[i];

    if vFloatItem.PointInClient(X - vFloatItem.Left, Y - vFloatItem.Top) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function THCSectionData.GetScreenCoord(const X, Y: Integer): TPoint;
begin
  if Assigned(FOnGetScreenCoord) then
    Result := FOnGetScreenCoord(X, Y);
end;

function THCSectionData.InsertFloatItem(const AFloatItem: THCCustomFloatItem): Boolean;
var
  vStartNo, vStartOffset, vDrawNo: Integer;
begin
  // ��¼ѡ����ʼλ��
  vStartNo := Self.SelectInfo.StartItemNo;
  vStartOffset := Self.SelectInfo.StartItemOffset;

  // ȡѡ����ʼ����DrawItem
  vDrawNo := Self.GetDrawItemNoByOffset(vStartNo, vStartOffset);

  AFloatItem.Left := Self.DrawItems[vDrawNo].Rect.Left
    + Self.GetDrawItemOffsetWidth(vDrawNo, Self.SelectInfo.StartItemOffset - Self.DrawItems[vDrawNo].CharOffs + 1);
  AFloatItem.Top := Self.DrawItems[vDrawNo].Rect.Top;

  FFloatItemIndex := Self.FloatItems.Add(AFloatItem);
  AFloatItem.Active := True;

  Result := True;

  if not Self.DisSelect then
    Style.UpdateInfoRePaint;
end;

function THCSectionData.KeyDownFloatItem(var Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := True;
  if Self.ReadOnly then Exit;

  if (FFloatItemIndex >= 0) and (not FFloatItems[FFloatItemIndex].Lock) then
  begin
    case Key of
      VK_BACK, VK_DELETE:
        begin
          FFloatItems.Delete(FFloatItemIndex);
          FFloatItemIndex := -1;
          FMouseMoveIndex := -1;
        end;

      VK_LEFT: FFloatItems[FFloatItemIndex].Left := FFloatItems[FFloatItemIndex].Left - 1;

      VK_RIGHT: FFloatItems[FFloatItemIndex].Left := FFloatItems[FFloatItemIndex].Left + 1;

      VK_UP: FFloatItems[FFloatItemIndex].Top := FFloatItems[FFloatItemIndex].Top - 1;

      VK_DOWN: FFloatItems[FFloatItemIndex].Top := FFloatItems[FFloatItemIndex].Top + 1;
    else
      Result := False;
    end;
  end
  else
    Result := False;

  if Result then
    Style.UpdateInfoRePaint;
end;

procedure THCSectionData.DoInsertFloatItem(const AItem: THCCustomItem);
begin
  DoInsertItem(AItem);
end;

procedure THCSectionData.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vFloatCount, vStyleNo: Integer;
  vFloatItem: THCCustomFloatItem;
  vDataSize: Int64;
begin
  if AFileVersion > 42 then
    AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));  // ���ݴ�Сռλ

  inherited DoLoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 12 then
  begin
    AStream.ReadBuffer(vFloatCount, SizeOf(vFloatCount));
    while vFloatCount > 0 do
    begin
      AStream.ReadBuffer(vStyleNo, SizeOf(vStyleNo));

      if (AFileVersion < 28) and (vStyleNo = Ord(THCShapeStyle.hssLine)) then
        vFloatItem := THCFloatLineItem.Create(Self)
      else
        vFloatItem := CreateItemByStyle(vStyleNo) as THCCustomFloatItem;

      vFloatItem.LoadFromStream(AStream, AStyle, AFileVersion);
      FFloatItems.Add(vFloatItem);

      Dec(vFloatCount);
    end;
  end;
end;

function THCSectionData.MouseDownFloatItem(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  vOldIndex: Integer;
begin
  Result := False;

  FMouseDownIndex := GetFloatItemAt(X, Y);

  vOldIndex := FFloatItemIndex;
  if FFloatItemIndex <> FMouseDownIndex then
  begin
    if FFloatItemIndex >= 0 then
      FFloatItems[FFloatItemIndex].Active := False;

    FFloatItemIndex := FMouseDownIndex;

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
  end;

  if FFloatItemIndex >= 0 then
  begin
    if Self.ReadOnly then
    begin
      Result := True;
      Exit;
    end;

    if ssDouble in Shift then
    begin
      FFloatItems[FFloatItemIndex].DblClick(X - FFloatItems[FFloatItemIndex].Left, Y - FFloatItems[FFloatItemIndex].Top);
      Result := True;
    end
    else
    begin
      Result := FFloatItems[FFloatItemIndex].MouseDown(Button, Shift,
        X - FFloatItems[FFloatItemIndex].Left, Y - FFloatItems[FFloatItemIndex].Top);
    end;
  end;

  if (FMouseDownIndex < 0) and (vOldIndex < 0) then
    Result := False;
end;

function THCSectionData.MouseMoveFloatItem(Shift: TShiftState; X, Y: Integer): Boolean;
var
  vItemIndex: Integer;
  vFloatItem: THCCustomFloatItem;
begin
  Result := False;

  if Shift = [ssLeft] then  // ������ק
  begin
    if Self.ReadOnly then Exit;

    if FMouseDownIndex >= 0 then
    begin
      vFloatItem := FFloatItems[FMouseDownIndex];
      Result := vFloatItem.MouseMove(Shift, X - vFloatItem.Left, Y - vFloatItem.Top);
      if Result then
        Style.UpdateInfoRePaint;
    end;
  end
  else  // ��ͨ����ƶ�
  begin
    vItemIndex := GetFloatItemAt(X, Y);
    if FMouseMoveIndex <> vItemIndex then
    begin
      if FMouseMoveIndex >= 0 then  // �ɵ��Ƴ�
        FFloatItems[FMouseMoveIndex].MouseLeave;

      FMouseMoveIndex := vItemIndex;
      if FMouseMoveIndex >= 0 then  // �µ�����
        FFloatItems[FMouseMoveIndex].MouseEnter;
    end;

    if vItemIndex >= 0 then
    begin
      vFloatItem := FFloatItems[vItemIndex];
      Result := vFloatItem.MouseMove(Shift, X - vFloatItem.Left, Y - vFloatItem.Top);
    end;
  end;
end;

function THCSectionData.MouseUpFloatItem(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  vFloatItem: THCCustomFloatItem;
begin
  Result := False;

  if FMouseDownIndex >= 0 then
  begin
    if Self.ReadOnly then
    begin
      Result := True;
      Exit;
    end;

    Undo_New;
    UndoAction_FloatItemMirror(FMouseDownIndex);

    vFloatItem := FFloatItems[FMouseDownIndex];
    Result := vFloatItem.MouseUp(Button, Shift, X - vFloatItem.Left, Y - vFloatItem.Top);
    if Result then
      Style.UpdateInfoRePaint;
  end;
end;

procedure THCSectionData.PaintFloatItems(const APageIndex, ADataDrawLeft,
  ADataDrawTop, AVOffset: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  i: Integer;
  vFloatItem: THCCustomFloatItem;
begin
  for i := 0 to FFloatItems.Count - 1 do
  begin
    vFloatItem := FFloatItems[i];

    //if vFloatItem.PageIndex = APageIndex then
    begin
      vFloatItem.DrawRect := Bounds(vFloatItem.Left, vFloatItem.Top, vFloatItem.Width, vFloatItem.Height);
      vFloatItem.DrawRect.Offset(ADataDrawLeft, ADataDrawTop - AVOffset);  // ��������ʼλ��ӳ�䵽����λ��
      vFloatItem.PaintTo(Self.Style, vFloatItem.DrawRect, ADataDrawTop, 0,
        0, 0, ACanvas, APaintInfo);
    end;
  end;
end;

procedure THCSectionData.ParseXml(const ANode: IHCXMLNode);
var
  vItemsNode, vNode: IHCXMLNode;
  vFloatItem: THCCustomFloatItem;
  i: Integer;
begin
  vItemsNode := ANode.ChildNodes.FindNode('items');
  inherited ParseXml(vItemsNode);

  vItemsNode := ANode.ChildNodes.FindNode('floatitems');
  for i := 0 to vItemsNode.ChildNodes.Count - 1 do
  begin
    vNode := vItemsNode.ChildNodes[i];
    vFloatItem := CreateItemByStyle(vNode.Attributes['sno']) as THCCustomFloatItem;
    vFloatItem.ParseXml(vNode);
    FFloatItems.Add(vFloatItem);
  end;
end;

procedure THCSectionData.SaveToStream(const AStream: TStream);
var
  i, vFloatCount: Integer;
  vBegPos, vEndPos: Int64;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // ���ݴ�Сռλ������Խ��

  inherited SaveToStream(AStream);

  vFloatCount := FFloatItems.Count;
  AStream.WriteBuffer(vFloatCount, SizeOf(vFloatCount));
  for i := 0 to FFloatItems.Count - 1 do
    FFloatItems[i].SaveToStream(AStream);

  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // ��ǰҳ���ݴ�С
  AStream.Position := vEndPos;
end;

procedure THCSectionData.SetReadOnly(const Value: Boolean);
begin
  if Self.ReadOnly <> Value then
  begin
    inherited SetReadOnly(Value);

    if Assigned(FOnReadOnlySwitch) then
      FOnReadOnlySwitch(Self);
  end;
end;

procedure THCSectionData.ToXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vNode: IHCXMLNode;
begin
  vNode := ANode.AddChild('items');
  inherited ToXml(vNode);  // ��Itmes

  // ��FloatItems
  vNode := ANode.AddChild('floatitems');
  vNode.Attributes['count'] := FFloatItems.Count;
  for i := 0 to FFloatItems.Count - 1 do
    FFloatItems[i].ToXml(vNode.AddChild('floatitem'));
end;

procedure THCSectionData.TraverseFloatItem(const ATraverse: THCItemTraverse);
var
  i: Integer;
begin
  if ATraverse <> nil then
  begin
    for i := 0 to FFloatItems.Count - 1 do
    begin
      if ATraverse.Stop then Break;

      ATraverse.Process(Self, i, ATraverse.Tag, ATraverse.DomainStack, ATraverse.Stop);
      //if FFloatItems[i].StyleNo < THCStyle.Null then
      //  (FFloatItems[i] as THCCustomRectItem).TraverseItem(ATraverse);
    end;
  end;
end;

procedure THCSectionData.UndoAction_FloatItemMirror(const AItemNo: Integer);
{var
  vUndo: THCUndo;
  vUndoList: THCUndoList;}
begin
  {vUndoList := GetUndoList;
  if Assigned(vUndoList) and vUndoList.Enable then
  begin
    vUndo := vUndoList.Last;
    if vUndo <> nil then
      vUndo.ActionAppend(uatItemMirror, AItemNo, 0, False);
  end;}
end;

end.
