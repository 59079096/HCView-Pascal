{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-12-3             }
{                                                       }
{            支持批注功能的文档对象管理单元             }
{                                                       }
{*******************************************************}

unit HCAnnotateData;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, Generics.Collections, HCCustomData,
  HCRichData, HCItem, HCStyle, HCParaStyle, HCTextStyle, HCTextItem, HCAnnotateItem,
  HCRectItem, HCCommon, HCList;

type
  THCDataAnnotate = class(TSelectInfo)  // Data批注信息
  private
    FID: Integer;
    FTitle, FText: string;
  public
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
  end;

  THCAnnotateInfo = class(THCDomainInfo)
  end;

  TDataDrawItemAnnotateEvent = procedure(const AData: THCCustomData; const ADrawItemNo: Integer;
    const ADrawRect: TRect; const AAnnotateItem: THCAnnotateItem) of object;
  TDataAnnotateEvent = procedure(const AData: THCCustomData; const AAnnotateItem: THCAnnotateItem) of object;

  THCAnnotateData = class(THCRichData)  // 支持批注功能的Data类
  private
    FNextID, FAnnotateCount: Cardinal;
    FIDStrack: TStack<Cardinal>;
    FHotAnnotate, FActiveAnnotate: THCAnnotateInfo;
    FOnDrawItemAnnotate: TDataDrawItemAnnotateEvent;
    FOnInsertAnnotate, FOnRemoveAnnotate: TDataAnnotateEvent;
  protected
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure DoInsertItem(const AItem: THCCustomItem); override;
    procedure DoRemoveItem(const AItem: THCCustomItem); override;
    function DoAcceptAction(const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean; override;
    procedure DoDrawItemPaintContent(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADrawText: string;
      const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AStyle: THCStyle); override;
    destructor Destroy; override;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: THCCaretInfo); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure InitializeField; override;
    procedure SaveToStream(const AStream: TStream); override;
    function GetAnnotateBeginBefor(const AItemNo: Integer; const AID: Cardinal): Integer;
    function GetAnnotateEndAfter(const AItemNo: Integer; const AID: Cardinal): Integer;
    procedure GetAnnotateFrom(const AItemNo, AOffset: Integer; const AAnnotateInfo: THCAnnotateInfo);
    function InsertAnnotate(const ATitle, AText: string): Boolean;
    function DeleteAnnotate(const ABeginNo, AEndNo: Integer; const AKeepPara: Boolean = True): Boolean;
    function DeleteActiveAnnotate: Boolean;
    property HotAnnotate: THCAnnotateInfo read FHotAnnotate;
    property ActiveAnnotate: THCAnnotateInfo read FActiveAnnotate;
    property OnDrawItemAnnotate: TDataDrawItemAnnotateEvent read FOnDrawItemAnnotate write FOnDrawItemAnnotate;
    property OnInsertAnnotate: TDataAnnotateEvent read FOnInsertAnnotate write FOnInsertAnnotate;
    property OnRemoveAnnotate: TDataAnnotateEvent read FOnRemoveAnnotate write FOnRemoveAnnotate;
  end;

implementation

{ THCAnnotateData }

constructor THCAnnotateData.Create(const AStyle: THCStyle);
begin
  FNextID := 0;
  FAnnotateCount := 0;
  FHotAnnotate := THCAnnotateInfo.Create;
  FHotAnnotate.Data := Self;
  FActiveAnnotate := THCAnnotateInfo.Create;
  FActiveAnnotate.Data := Self;
  FIDStrack := TStack<Cardinal>.Create;
  inherited Create(AStyle);
end;

function THCAnnotateData.DeleteActiveAnnotate: Boolean;
var
  vRectItem: THCCustomRectItem;
  vFirstDrawItemNo, vLastItemNo: Integer;
begin
  Result := False;
  if SelectExists then Exit;

  if FActiveAnnotate.BeginNo >= 0 then
    Result := DeleteAnnotate(FActiveAnnotate.BeginNo, FActiveAnnotate.EndNo)
  else
  if (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)
    and (SelectInfo.StartItemOffset = OffsetInner)
  then
  begin
    Undo_New;

    vRectItem := Items[SelectInfo.StartItemNo] as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    Result := (Items[SelectInfo.StartItemNo] as THCCustomRectItem).DeleteActiveAnnotate;
    if vRectItem.IsFormatDirty then
    begin
      GetFormatRange(vFirstDrawItemNo, vLastItemNo);
      FormatPrepare(vFirstDrawItemNo, vLastItemNo);
      ReFormatData(vFirstDrawItemNo, vLastItemNo);
    end
    else
      Self.FormatInit;
  end;
end;

function THCAnnotateData.DeleteAnnotate(const ABeginNo, AEndNo: Integer; const AKeepPara: Boolean = True): Boolean;
var
  i, vFormatFirstDrawItemNo, vFormatLastItemNo, vFormatDrawItemNo2, vDelCount: Integer;
  vItem: THCCustomItem;
  vStartParaFirst: Boolean;
begin
  Result := False;
  if not CanEdit then Exit;
  if AEndNo < ABeginNo then Exit;

  Self.InitializeField;

  GetFormatRange(ABeginNo, 0, vFormatFirstDrawItemNo, vFormatLastItemNo);
  if (not AKeepPara) and (AEndNo < Items.Count - 1) and (Items[AEndNo + 1].ParaFirst) then
    GetFormatRange(AEndNo + 1, GetItemOffsetAfter(AEndNo + 1), vFormatDrawItemNo2, vFormatLastItemNo)
  else
    GetFormatRange(AEndNo, GetItemOffsetAfter(AEndNo), vFormatDrawItemNo2, vFormatLastItemNo);

  if (Items[ABeginNo].ParaFirst) and (vFormatFirstDrawItemNo > 0) then
  begin
    Dec(vFormatFirstDrawItemNo);
    vFormatFirstDrawItemNo := GetFormatFirstDrawItem(vFormatFirstDrawItemNo);
  end;

  FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);

  vStartParaFirst := Items[ABeginNo].ParaFirst;
  vDelCount := 2;

  Undo_New;

  UndoAction_DeleteItem(AEndNo, 0);
  Items.Delete(AEndNo);
  UndoAction_DeleteItem(ABeginNo, 0);
  Items.Delete(ABeginNo);

  if Items.Count = 0 then
  begin
    vItem := CreateDefaultTextItem;
    Self.CurStyleNo := vItem.StyleNo;
    vItem.ParaFirst := True;
    Items.Add(vItem);
    Dec(vDelCount);
    UndoAction_InsertItem(0, 0);
  end
  else
  if vStartParaFirst then
  begin
    if (ABeginNo < Items.Count - 1) and (not Items[ABeginNo].ParaFirst) then
    begin
      UndoAction_ItemParaFirst(ABeginNo, 0, True);
      Items[ABeginNo].ParaFirst := True;
    end
    else
    if AKeepPara then
    begin
      vItem := CreateDefaultTextItem;
      Self.CurStyleNo := vItem.StyleNo;
      vItem.ParaFirst := True;
      Items.Insert(ABeginNo, vItem);
      Dec(vDelCount);
      UndoAction_InsertItem(ABeginNo, 0);
    end;
  end;

  ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo - vDelCount, -vDelCount);

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;

  if vStartParaFirst and AKeepPara then
    ReSetSelectAndCaret(ABeginNo, 0)
  else
  if ABeginNo > 0 then
    ReSetSelectAndCaret(ABeginNo - 1)
  else
    ReSetSelectAndCaret(0, 0);

  Result := True;
end;

destructor THCAnnotateData.Destroy;
begin
  FreeAndNil(FHotAnnotate);
  FreeAndNil(FActiveAnnotate);
  FreeAndNil(FIDStrack);
  inherited Destroy;
end;

procedure THCAnnotateData.DoDrawItemPaintContent(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom,
  ADataScreenTop, ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  if not APaintInfo.Print and Assigned(FOnDrawItemAnnotate) then
  begin
    if (AData.Items[AItemNo].StyleNo = THCStyle.Annotate) and ((AData.Items[AItemNo] as THCAnnotateItem).MarkType = TMarkType.cmtEnd) then
      FOnDrawItemAnnotate(AData, ADrawItemNo, ADrawRect, AData.Items[AItemNo] as THCAnnotateItem);
  end;

  inherited DoDrawItemPaintContent(AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect,
    ADrawText, ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);
end;

function THCAnnotateData.GetAnnotateBeginBefor(const AItemNo: Integer; const AID: Cardinal): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := AItemNo downto 0 do
  begin
    if Items[i].StyleNo = THCStyle.Annotate then
    begin
      if ((Items[i] as THCAnnotateItem).MarkType = TMarkType.cmtBeg)
        and ((Items[i] as THCAnnotateItem).ID = AID)
      then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function THCAnnotateData.GetAnnotateEndAfter(const AItemNo: Integer; const AID: Cardinal): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := AItemNo to Items.Count - 1 do
  begin
    if Items[i].StyleNo = THCStyle.Annotate then
    begin
      if ((Items[i] as THCAnnotateItem).MarkType = TMarkType.cmtEnd)
        and ((Items[i] as THCAnnotateItem).ID = AID)
      then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure THCAnnotateData.GetAnnotateFrom(const AItemNo, AOffset: Integer; const AAnnotateInfo: THCAnnotateInfo);
var
  i: Integer;
  vID: Cardinal;
  vAnnotateItem: THCAnnotateItem;
begin
  AAnnotateInfo.Clear;

  if (AItemNo < 0) or (AOffset < 0) then Exit;

  if Items[AItemNo].StyleNo = THCStyle.Annotate then
  begin
    vAnnotateItem := Items[AItemNo] as THCAnnotateItem;
    if vAnnotateItem.MarkType = TMarkType.cmtBeg then
    begin
      if AOffset = OffsetAfter then
      begin
        AAnnotateInfo.Data := Self;
        AAnnotateInfo.BeginNo := AItemNo;
        vID := vAnnotateItem.ID;
        AAnnotateInfo.EndNo := GetAnnotateEndAfter(AItemNo + 1, vID);
        Exit;
      end;
    end
    else
    begin
      if AOffset = OffsetBefor then
      begin
        AAnnotateInfo.Data := Self;
        AAnnotateInfo.EndNo := AItemNo;
        vID := vAnnotateItem.ID;
        AAnnotateInfo.BeginNo := GetAnnotateBeginBefor(AItemNo - 1, vID);
        Exit;
      end;
    end;
  end;

  FIDStrack.Clear;
  for i := AItemNo downto 0 do
  begin
    if Items[i].StyleNo = THCStyle.Annotate then
    begin
      vAnnotateItem := Items[i] as THCAnnotateItem;
      if vAnnotateItem.MarkType = TMarkType.cmtEnd then
        FIDStrack.Push(vAnnotateItem.ID)
      else
      begin
        if FIDStrack.Count > 0 then
        begin
          vID := FIDStrack.Peek;
          if vAnnotateItem.ID = vID then
            FIDStrack.Pop
          else
          begin
            AAnnotateInfo.Data := Self;
            AAnnotateInfo.BeginNo := i;
            vID := vAnnotateItem.ID;
            Break;
          end;
        end
        else
        begin
          AAnnotateInfo.Data := Self;
          AAnnotateInfo.BeginNo := i;
          vID := vAnnotateItem.ID;
          Break;
        end;
      end;
    end;
  end;

  if AAnnotateInfo.BeginNo >= 0 then
    AAnnotateInfo.EndNo := GetAnnotateEndAfter(AItemNo + 1, vID);
end;

procedure THCAnnotateData.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: THCCaretInfo);
var
  vTopData: THCCustomData;
begin
  if (FAnnotateCount > 0) and (Self.SelectInfo.StartItemNo >= 0) then
  begin
    vTopData := GetTopLevelData;
    if vTopData = Self then
      GetAnnotateFrom(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, FActiveAnnotate)
  end;

  inherited GetCaretInfo(AItemNo, AOffset, ACaretInfo);
end;

procedure THCAnnotateData.InitializeField;
begin
  inherited InitializeField;
  FHotAnnotate.Clear;
  FActiveAnnotate.Clear;
end;

function THCAnnotateData.InsertAnnotate(const ATitle, AText: string): Boolean;

  function InsertAnnotateByOffset(const AAnnotateItem: THCAnnotateItem; const AItemNo, AOffset: Integer): Integer;
  var
    vS: string;
    vAfterItem: THCCustomItem;
  begin
    Result := 0;

    if AOffset = OffsetBefor then
    begin
      AAnnotateItem.ParaFirst := Items[AItemNo].ParaFirst;
      AAnnotateItem.PageBreak := Items[AItemNo].PageBreak;
      if (Items[AItemNo].StyleNo > THCStyle.Null) and (Items[AItemNo].Text = '') then
      begin
        UndoAction_DeleteItem(AItemNo, 0);
        Items.Delete(AItemNo);
        Dec(Result);
      end
      else
      begin
        if Items[AItemNo].ParaFirst then
        begin
          UndoAction_ItemParaFirst(AItemNo, 0, False);
          Items[AItemNo].ParaFirst := False;
        end;

        if Items[AItemNo].PageBreak then
        begin
          UndoAction_ItemPageBreak(AItemNo, 0, False);
          Items[AItemNo].PageBreak := False;
        end;
      end;

      Items.Insert(AItemNo, AAnnotateItem);
      UndoAction_InsertItem(AItemNo, OffsetBefor);
      Inc(Result);
    end
    else
    if AOffset = GetItemOffsetAfter(AItemNo) then
    begin
      Items.Insert(AItemNo + 1, AAnnotateItem);
      UndoAction_InsertItem(AItemNo + 1, OffsetBefor);
      Inc(Result);
    end
    else
    if IsRectItem(AItemNo) then
    begin
      Items.Insert(AItemNo + 1, AAnnotateItem);
      UndoAction_InsertItem(AItemNo + 1, OffsetBefor);
      Inc(Result);
    end
    else
    begin
      vS := (Items[AItemNo] as THCTextItem).SubString(AOffset + 1, Items[AItemNo].Length - AOffset);
      UndoAction_DeleteText(AItemNo, AOffset + 1, vS);
      vAfterItem := Items[AItemNo].BreakByOffset(AOffset);
      Style.States.Include(hosInsertBreakItem);
      try
        Items.Insert(AItemNo + 1, vAfterItem);
      finally
        Style.States.Exclude(hosInsertBreakItem);
      end;

      UndoAction_InsertItem(AItemNo + 1, 0);
      Inc(Result);

      Items.Insert(AItemNo + 1, AAnnotateItem);
      UndoAction_InsertItem(AItemNo + 1, OffsetBefor);
      Inc(Result);
    end;
  end;

var
  vRectItem: THCCustomRectItem;
  vAnnotateItem: THCAnnotateItem;
  vIncCount, vFormatFirstDrawItemNo, vFormatLastItemNo: Integer;
begin
  Result := False;

  if (Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null)
    and (SelectInfo.StartItemOffset = OffsetInner)
  then
  begin
    Undo_New;

    vRectItem := Items[SelectInfo.StartItemNo] as THCCustomRectItem;
    if vRectItem.MangerUndo then
      UndoAction_ItemSelf(SelectInfo.StartItemNo, OffsetInner)
    else
      UndoAction_ItemMirror(SelectInfo.StartItemNo, OffsetInner);

    Result := (Items[SelectInfo.StartItemNo] as THCCustomRectItem).InsertAnnotate(ATitle, AText);
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
    if not CanEdit then Exit;

    Self.Style.States.Include(hosBatchInsert);
    try
      vIncCount := 0;
      Undo_New;
      if Self.SelectExists then
      begin
        vFormatFirstDrawItemNo := GetFormatFirstDrawItem(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
        vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);
      end
      else
        GetFormatRange(SelectInfo.StartItemNo, 1, vFormatFirstDrawItemNo, vFormatLastItemNo);

      FormatPrepare(vFormatFirstDrawItemNo, vFormatLastItemNo);
      Inc(FNextID);
      // 插入尾
      vAnnotateItem := THCAnnotateItem.Create(Self);
      vAnnotateItem.MarkType := cmtEnd;
      vAnnotateItem.ID := FNextID;
      vAnnotateItem.Content.Title := ATitle;
      vAnnotateItem.Content.Text := AText;
      if SelectInfo.EndItemNo >= 0 then // 有选中结束item
        vIncCount := InsertAnnotateByOffset(vAnnotateItem, SelectInfo.EndItemNo, SelectInfo.EndItemOffset)
      else
        vIncCount := InsertAnnotateByOffset(vAnnotateItem, SelectInfo.StartItemNo, SelectInfo.StartItemOffset);

      // 插入头
      vAnnotateItem := THCAnnotateItem.Create(Self);
      vAnnotateItem.MarkType := cmtBeg;
      vAnnotateItem.ID := FNextID;
      vIncCount := vIncCount + InsertAnnotateByOffset(vAnnotateItem, SelectInfo.StartItemNo, SelectInfo.StartItemOffset);

      ReFormatData(vFormatFirstDrawItemNo, vFormatLastItemNo + vIncCount, vIncCount);
    finally
      Self.Style.States.Exclude(hosBatchInsert);
    end;

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;

    ReSetSelectAndCaret(SelectInfo.StartItemNo);
    Result := True;
  end;
end;

function THCAnnotateData.DoAcceptAction(const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
begin
  Result := True;

  if Style.States.Contain(THCState.hosLoading)
    or Style.States.Contain(THCState.hosUndoing)
    or Style.States.Contain(THCState.hosRedoing)
  then
    Exit;

  if AAction = THCAction.actDeleteItem then
  begin
    if Items[AItemNo].StyleNo = THCStyle.Annotate then
      Result := False;
  end;

  if Result then
    Result := inherited DoAcceptAction(AItemNo, AOffset, AAction);
end;

procedure THCAnnotateData.DoInsertItem(const AItem: THCCustomItem);
begin
  if (AItem.StyleNo = THCStyle.Annotate) and ((AItem as THCAnnotateItem).MarkType = TMarkType.cmtBeg) then
  begin
    Inc(FAnnotateCount);
    if Assigned(FOnInsertAnnotate) then
      FOnInsertAnnotate(Self, AItem as THCAnnotateItem);
  end;

  inherited DoInsertItem(AItem);
end;

procedure THCAnnotateData.DoRemoveItem(const AItem: THCCustomItem);
begin
  if (AItem.StyleNo = THCStyle.Annotate) and ((AItem as THCAnnotateItem).MarkType = TMarkType.cmtBeg) then
  begin
    Dec(FAnnotateCount);
    if Assigned(FOnRemoveAnnotate) then
      FOnRemoveAnnotate(Self, AItem as THCAnnotateItem);
  end;

  inherited DoRemoveItem(AItem);
end;

procedure THCAnnotateData.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vAnnCount: Word;
  i: Integer;
  vAnn: THCDataAnnotate;
begin
  inherited DoLoadFromStream(AStream, AStyle, AFileVersion);
  if (AFileVersion > 22) and (AFileVersion < 55) then
  begin
    AStream.ReadBuffer(vAnnCount, SizeOf(vAnnCount));
    if vAnnCount > 0 then
    begin
      for i := 0 to vAnnCount - 1 do
      begin
        vAnn := THCDataAnnotate.Create;
        vAnn.LoadFromStream(AStream, AFileVersion);
      end;
    end;
  end;

  if AFileVersion > 54 then
    AStream.ReadBuffer(FNextID, SizeOf(FNextID))
  else
    FNextID := 0;
end;

procedure THCAnnotateData.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vTopData: THCAnnotateData;
begin
  inherited MouseMove(Shift, X, Y);
  if FAnnotateCount > 0 then
    Self.GetAnnotateFrom(Self.MouseMoveItemNo, Self.MouseMoveItemOffset, FHotAnnotate)
  else
    FHotAnnotate.Clear;

  vTopData := Self.GetTopLevelDataAt(X, Y) as THCAnnotateData;
  if (vTopData = Self) or (vTopData.HotAnnotate.BeginNo < 0) then
  begin
    if FHotAnnotate.BeginNo >= 0 then
      Style.UpdateInfoRePaint;
  end;
end;

procedure THCAnnotateData.SaveToStream(const AStream: TStream);
begin
  inherited SaveToStream(AStream);
  AStream.WriteBuffer(FNextID, SizeOf(FNextID));
end;

{ THCDataAnnotate }

procedure THCDataAnnotate.LoadFromStream(const AStream: TStream; const AFileVersion: Word);
begin
  AStream.ReadBuffer(FID, SizeOf(FID));
  Self.StartItemNo := FID;
  AStream.ReadBuffer(FID, SizeOf(FID));
  Self.StartItemOffset := FID;
  AStream.ReadBuffer(FID, SizeOf(FID));
  Self.EndItemNo := FID;
  AStream.ReadBuffer(FID, SizeOf(FID));
  Self.EndItemOffset := FID;
  AStream.ReadBuffer(FID, SizeOf(FID));

  HCLoadTextFromStream(AStream, FTitle, AFileVersion);
  HCLoadTextFromStream(AStream, FText, AFileVersion);
end;

end.
