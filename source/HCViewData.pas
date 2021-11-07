{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{            ֧���������ĵ��������Ԫ               }
{                                                       }
{*******************************************************}

unit HCViewData;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, Generics.Collections, HCCustomData,
  HCRichData, HCItem, HCStyle, HCParaStyle, HCTextStyle, HCTextItem, HCRectItem,
  HCCommon, HCViewDevData, HCList;

type
  TStyleItemEvent = function (const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem of object;
  TOnCanEditEvent = function(const Sender: TObject): Boolean of object;
  TTextEvent = function(const AData: THCCustomData; const AItemNo, AOffset: Integer;
    const AText: string): Boolean of object;

  THCViewData = class(THCViewDevData)  // ���и�����Ͳ���Items���ܵ�Data��
  private
    FScript: string;
    FCaretItemChanged: Boolean;
    FDomainStartDeletes: THCIntegerList;  // ������ѡ��ɾ��ʱ��������ʼ������ѡ��ʱ��ɾ���˽����������ʼ�Ŀ�ɾ��
    FHotDomain,  // ��ǰ������
    FActiveDomain  // ��ǰ������
      : THCDomainInfo;
    FDomainCount: Integer;
    FHotDomainRGN, FActiveDomainRGN: HRGN;
    FOnCreateItemByStyle: TStyleItemEvent;
    FOnCanEdit: TOnCanEditEvent;
    FOnInsertTextBefor: TTextEvent;
    FOnCaretItemChanged: TDataItemEvent;
    FOnPaintDomainRegion: TDataItemNoFunEvent;
  protected
    function DoAcceptAction(const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean; override;
    /// <summary> �Ƿ��������Item </summary>
    function DoSaveItem(const AItemNo: Integer): Boolean; override;
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    /// <summary> ���ڴ���������Items�󣬼�鲻�ϸ��Item��ɾ�� </summary>
    function CheckInsertItemCount(const AStartNo, AEndNo: Integer): Integer; override;
    procedure ReSetSelectAndCaret(const AItemNo, AOffset: Integer; const ANextWhenMid: Boolean = False); override;
    procedure DoCaretItemChanged; override;
    procedure DoInsertItem(const AItem: THCCustomItem); override;
    procedure DoRemoveItem(const AItem: THCCustomItem); override;
    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function DoPaintDomainRegion(const AItemNo: Integer): Boolean;
  public
    constructor Create(const AStyle: THCStyle); override;
    destructor Destroy; override;

    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; override;
    procedure PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawRight, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset, AFristDItemNo, ALastDItemNo: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure InitializeField; override;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: THCCaretInfo); override;
    function DeleteSelected: Boolean; override;
    function DeleteActiveDomain: Boolean;
    function DeleteDomain(const ADomain: THCDomainInfo): Boolean;
    function DeleteDomainByItemNo(const AStartNo, AEndNo: Integer): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function InsertItem(const AItem: THCCustomItem): Boolean; overload; override;
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem;
      const AOffsetBefor: Boolean = True): Boolean; overload; override;
    function CanEdit: Boolean; override;

    function DoInsertTextBefor(const AItemNo, AOffset: Integer; const AText: string): Boolean; override;

    procedure SaveToStream(const AStream: TStream); override;
    /// <summary> ���ݴ������"ģ��"������ </summary>
    /// <param name="AMouldDomain">"ģ��"������˷������������ͷ�</param>
    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
    /// <summary> ��ȡָ��λ�����ڵ�����Ϣ </summary>
    procedure GetDomainFrom(const AItemNo, AOffset: Integer; const ADomainInfo: THCDomainInfo;
      const AIsWantDomain: TRectItemActionEvent = nil);
    /// <summary> ����ѡ�з�Χ </summary>
    /// <param name="ASilence">�Ƿ�"��Ĭ"���ã�False����Ӧ��ѡ�в�������ض���(���λ�á��������) True������Ӧ(�ⲿ�Լ�����)</param>
    procedure SetSelectBound(const AStartNo, AStartOffset, AEndNo, AEndOffset: Integer;
      const ASilence: Boolean = True);

    /// <summary> ���ѡ��ָ��Item������� </summary>
    procedure SelectItemAfterWithCaret(const AItemNo: Integer);

    /// <summary> ���ѡ�����һ��Item������� </summary>
    procedure SelectLastItemAfterWithCaret;

    /// <summary> ���ѡ����һ��Item����ǰ�� </summary>
    procedure SelectFirstItemBeforWithCaret;

    /// <summary> ��ȡDomainItem��Ե���һ��ItemNo </summary>
    /// <param name="AItemNo">��ǰDomainItem(ͷ��β)</param>
    function GetDomainAnother(const AItemNo: Integer): Integer;

    /// <summary> ��ǰλ�ÿ�ʼ����ָ�������� </summary>
    /// <param name="AKeyword">Ҫ���ҵĹؼ���</param>
    /// <param name="AForward">True����ǰ��False�����</param>
    /// <param name="AMatchCase">True�����ִ�Сд��False�������ִ�Сд</param>
    /// <returns>True���ҵ�</returns>
    function Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean;
    function Replace(const AText: string): Boolean;

    procedure GetCaretInfoCur(var ACaretInfo: THCCaretInfo);
    procedure TraverseItem(const ATraverse: THCItemTraverse);

    /// <summary>
    /// ���������ݵ��ļ�������Ϊ��汾����ʽ��HCView�����DataSaveLiteStream�ظ�����ʱ�ֲ��ô˷���������û�а�DataSaveLiteStream�ع����˵�Ԫ
    /// </summary>
    /// <param name="AStream">��</param>
    /// <param name="ADomainItemNo">����ʼ�����ItemNo</param>
    //procedure SaveDomainToStream(const AStream: TStream; const ADomainItemNo: Integer);

    function ExecuteScript(const AItemNo: Integer): Boolean; virtual;

    property Script: string read FScript write FScript;
    property OnCaretItemChanged: TDataItemEvent read FOnCaretItemChanged write FOnCaretItemChanged;
    property HotDomain: THCDomainInfo read FHotDomain;
    property ActiveDomain: THCDomainInfo read FActiveDomain;
    property DomainCount: Integer read FDomainCount;
    property OnCreateItemByStyle: TStyleItemEvent read FOnCreateItemByStyle write FOnCreateItemByStyle;
    property OnCanEdit: TOnCanEditEvent read FOnCanEdit write FOnCanEdit;
    property OnInsertTextBefor: TTextEvent read FOnInsertTextBefor write FOnInsertTextBefor;
    property OnPaintDomainRegion: TDataItemNoFunEvent read FOnPaintDomainRegion write FOnPaintDomainRegion;
  end;

implementation

{$I HCView.inc}

uses
  StrUtils;

{ THCViewData }

function THCViewData.DoAcceptAction(const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
var
  vItemNo: Integer;
begin
  Result := True;

  if Style.States.Contain(THCState.hosLoading)
    or Style.States.Contain(THCState.hosUndoing)
    or Style.States.Contain(THCState.hosRedoing)
  then
    Exit;

  if AAction = THCAction.actDeleteItem then
  begin
    if Items[AItemNo].StyleNo = THCStyle.Domain then  // �����ʶ
    begin
      if (Items[AItemNo] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // �������ʶ
      begin
        vItemNo := GetDomainAnother(AItemNo);  // ����ʼ
        if vItemNo = SelectInfo.StartItemNo then  // ����ʼ��ѡ����ʼ
          Result := SelectInfo.StartItemOffset = OffsetBefor  // Ҳ��ȫ�ȷ�Χ��
        else
          Result := (vItemNo >= SelectInfo.StartItemNo) and (vItemNo <= SelectInfo.EndItemNo);

        if Result then  // ��ʼҲ��ѡ��ɾ����Χ��
          FDomainStartDeletes.Add(vItemNo);  // ��¼����
      end
      else  // ����ʼ���
        Result := FDomainStartDeletes.IndexOf(AItemNo) >= 0;  // ������ʶ�Ѿ������Ϊ��ɾ��
    end;
  end;

  if Result then
    Result := inherited DoAcceptAction(AItemNo, AOffset, AAction);
end;

procedure THCViewData.DoCaretItemChanged;
begin
  FCaretItemChanged := True;
end;

function THCViewData.CanEdit: Boolean;
begin
  Result := inherited CanEdit;
  if Result and Assigned(FOnCanEdit) then
    Result := FOnCanEdit(Self);
end;

function THCViewData.CheckInsertItemCount(const AStartNo, AEndNo: Integer): Integer;
var
  i, vDelCount, vLevel: Integer;
  vDomainInfo: THCDomainInfo;
begin
  Result := inherited CheckInsertItemCount(AStartNo, AEndNo);

  if Self.Loading then Exit;

  vLevel := -1;
  vDomainInfo := THCDomainInfo.Create;
  try
    GetDomainFrom(AStartNo, 0, vDomainInfo);
    if vDomainInfo.BeginNo >= 0 then
      vLevel := (Items[vDomainInfo.BeginNo] as THCDomainItem).Level;
  finally
    vDomainInfo.Free;
  end;

  for i := AStartNo to AEndNo do
  begin
    if Items[i] is THCDomainItem then  // ���ʶ
    begin
      if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // ����ʼ
        Inc(vLevel);

      Assert(vLevel > -1);
      (Items[i] as THCDomainItem).Level := vLevel;

      if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // �ǽ���
        Dec(vLevel);
    end;
  end;

  // �����������滻��������ǵ����ĶΣ�����ʽ���������
  if Style.States.Contain(hosDomainWholeReplace) then
  begin
    // [ �Ƕ��ף�������������Ƕ���
    if (not Items[AStartNo].ParaFirst) and (AStartNo > 0)  // ���Ƕ��ף�����Data��һ��
      and (Items[AStartNo - 1] is THCDomainItem) and (Items[AStartNo - 1].ParaFirst)
    then
      Items[AStartNo - 1].ParaNo := Items[AStartNo].ParaNo;

    if IsParaLastItem(AEndNo + 1) and (Items[AEndNo + 1] is THCDomainItem) then
      Items[AEndNo + 1].ParaNo := Items[AEndNo].ParaNo;
  end;

  Exit;  // Ŀǰ���ȶ���Ӧ�ò�����ֲ�ƥ���������
  // �����ػ�ճ���ȴ�������Items��ƥ�������ʼ������ʶ��ɾ��
  vDelCount := 0;
  for i := AStartNo to AEndNo do  // ��ǰ������û�в�����ʼ��ʶ����ɾ���������������ʶ
  begin
    if Items[i] is THCDomainItem then  // ���ʶ
    begin
      if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // �ǽ�����˵��û�в����Ӧ����ʼ
      begin
        if i < AEndNo then  // ������󣬺���̳������ʼ����
          Items[i + 1].ParaFirst := Items[i].ParaFirst;

        Items.Delete(i);
        Inc(vDelCount);

        if (i > AStartNo) and (i <= AEndNo - vDelCount) then  // ɾ�����м��
        begin
          if (not Items[i - 1].ParaFirst)
            and (not Items[i].ParaFirst)
            and MergeItemText(Items[i - 1], Items[i])  // ǰ�󶼲��Ƕ��ף����ܺϲ�
          then
          begin
            Items.Delete(i);
            Inc(vDelCount);
          end;
        end;

        Break;
      end
      else  // ����ʼ���ǣ����õ�����
        Break;
    end;
  end;

  for i := AEndNo - vDelCount downto AStartNo do  // �Ӻ���ǰ����û�в��������ʶ����
  begin
    if Items[i] is THCDomainItem then  // ���ʶ
    begin
      if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // ����ʼ��˵��û�в����Ӧ�Ľ���
      begin
        if i < AEndNo - vDelCount then  // ������󣬺���̳������ʼ����
          Items[i + 1].ParaFirst := Items[i].ParaFirst;

        Items.Delete(i);
        Inc(vDelCount);

        if (i > AStartNo) and (i <= AEndNo - vDelCount) then  // ɾ�����м��
        begin
          if (not Items[i - 1].ParaFirst)
            and (not Items[i].ParaFirst)
            and MergeItemText(Items[i - 1], Items[i])  // ǰ�󶼲��Ƕ��ף����ܺϲ�
          then
          begin
            Items.Delete(i);
            Inc(vDelCount);
          end;
        end;

        Break;
      end
      else  // �ǽ������ǣ����õ�����
        Break;
    end;
  end;

  Result := Result - vDelCount;
end;

constructor THCViewData.Create(const AStyle: THCStyle);
begin
  FScript := '';
  FDomainCount := 0;
  FCaretItemChanged := False;
  FDomainStartDeletes := THCIntegerList.Create;
  FHotDomain := THCDomainInfo.Create;
  FHotDomain.Data := Self;
  FActiveDomain := THCDomainInfo.Create;
  FActiveDomain.Data := Self;
  inherited Create(AStyle);
end;

function THCViewData.CreateItemByStyle(const AStyleNo: Integer): THCCustomItem;
begin
  Result := nil;

  if Assigned(FOnCreateItemByStyle) then  // �Զ���������ڴ˴�����
    Result := FOnCreateItemByStyle(Self, AStyleNo);

  if not Assigned(Result) then
    Result := inherited CreateItemByStyle(AStyleNo);
end;

function THCViewData.DeleteActiveDomain: Boolean;
var
  vFirstDrawItemNo, vLastItemNo: Integer;
begin
  Result := False;
  if SelectExists then Exit;

  if FActiveDomain.BeginNo >= 0 then
    Result := DeleteDomain(FActiveDomain)
  else
  if Items[SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
  begin
    Result := (Items[SelectInfo.StartItemNo] as THCCustomRectItem).DeleteActiveDomain;
    if Result then
    begin
      GetFormatRange(vFirstDrawItemNo, vLastItemNo);
      FormatPrepare(vFirstDrawItemNo, vLastItemNo);
      ReFormatData(vFirstDrawItemNo, vLastItemNo);

      Style.UpdateInfoRePaint;
      Style.UpdateInfoReCaret;
    end;
  end;
end;

function THCViewData.DeleteDomain(const ADomain: THCDomainInfo): Boolean;
begin
  Result := DeleteDomainByItemNo(ADomain.BeginNo, ADomain.EndNo);
end;

function THCViewData.DeleteDomainByItemNo(const AStartNo,
  AEndNo: Integer): Boolean;
var
  i, vFirstDrawItemNo, vParaLastItemNo, vDelCount: Integer;
  vBeginPageBreak: Boolean;
  vItem: THCCustomItem;
begin
  Result := False;
  if AStartNo < 0 then Exit;

  Undo_New;

  vFirstDrawItemNo := GetFormatFirstDrawItem(Items[AStartNo].FirstDItemNo);
  vParaLastItemNo := GetParaLastItemNo(AEndNo);
  if Items[AStartNo].ParaFirst then  // ����ʼ�Ƕ���
  begin
    if AEndNo = vParaLastItemNo then  // ������Ƕ�β
    begin
      if AStartNo > 0 then  // ��ɾ���ɾ���Ҫ����һ�����ʼ��ʽ��
        vFirstDrawItemNo := GetFormatFirstDrawItem(Items[AStartNo].FirstDItemNo - 1);
    end
    else  // ��������Ƕ�β����ʼ�Ƕ���
    begin
      UndoAction_ItemParaFirst(AEndNo + 1, 0, True);
      Items[AEndNo + 1].ParaFirst := True;
    end;
  end;

  FormatPrepare(vFirstDrawItemNo, vParaLastItemNo);

  vDelCount := 0;
  vBeginPageBreak := Items[AStartNo].PageBreak;

  for i := AEndNo downto AStartNo do  // ɾ������Χ�ڵ�Item
  begin
    //if CanDeleteItem(i) then  // ����ɾ��
    begin
      UndoAction_DeleteItem(i, 0);
      Items.Delete(i);

      Inc(vDelCount);
    end;
  end;

  FActiveDomain.Clear;

  if AStartNo = 0 then  // ɾ������
  begin
    vItem := CreateDefaultTextItem;
    vItem.ParaFirst := True;
    vItem.PageBreak := vBeginPageBreak;

    Items.Insert(AStartNo, vItem);
    UndoAction_InsertItem(AStartNo, 0);
    Dec(vDelCount);
  end;

  ReFormatData(vFirstDrawItemNo, vParaLastItemNo - vDelCount, -vDelCount);

  Self.InitializeField;
  if AStartNo > Items.Count - 1 then
    ReSetSelectAndCaret(AStartNo - 1)
  else
    ReSetSelectAndCaret(AStartNo, 0);

  Style.UpdateInfoRePaint;
  Style.UpdateInfoReCaret;

  Result := True;
end;

function THCViewData.DeleteSelected: Boolean;
begin
  FDomainStartDeletes.Clear;  // �����ɾ��ʱ��¼ǰ�������Ϣ
  Result := inherited DeleteSelected;
end;

destructor THCViewData.Destroy;
begin
  FreeAndNil(FHotDomain);
  FreeAndNil(FActiveDomain);
  FreeAndNil(FDomainStartDeletes);
  inherited Destroy;
end;

function THCViewData.DoPaintDomainRegion(const AItemNo: Integer): Boolean;
begin
  if AItemNo < 0 then
    Result := False
  else
  if Assigned(FOnPaintDomainRegion) then
    Result := FOnPaintDomainRegion(Self, AItemNo)
  else
    Result := True;
end;

procedure THCViewData.DoRemoveItem(const AItem: THCCustomItem);
begin
  if THCDomainItem.IsBeginMark(AItem) then
    Dec(FDomainCount);

  if Assigned(FHotDomain) then  // Destroyʱ�ͷ���FHotDomain�������ͷ�Itemsʱ����DoRemoveItem
    FHotDomain.Clear;

  inherited DoRemoveItem(AItem);
end;

procedure THCViewData.DoDrawItemPaintAfter(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

  function SelectOffsetAfter_: Boolean;
  begin
    Result := Items[AItemNo].IsSelectComplate;
    if (not Result) and (Items[AItemNo].StyleNo > THCStyle.Null) and (SelectInfo.EndItemNo >= 0) then  // �����ǲ���ѡ�еı߽�
    begin
      if AItemNo = SelectInfo.EndItemNo then
      begin
        if SelectInfo.EndItemOffset = DrawItems[ADrawItemNo].CharOffsetEnd then
          Result := True;
      end
      else
      if AItemNo = SelectInfo.StartItemNo then
        Result := True;
    end;
  end;

  {$REGION ' DrawLineLastMrak ��β�Ļ��з� '}
  procedure DrawLineLastMrak(const ADrawRect: TRect);
  var
    vPt: TPoint;
  begin
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clActiveBorder;

    if APaintInfo.ScaleX <> 1 then
    begin
      SetViewportExtEx(ACanvas.Handle, APaintInfo.WindowWidth, APaintInfo.WindowHeight, @vPt);
      try
        if SelectOffsetAfter_ then
        begin
          ACanvas.Brush.Color := Style.SelColor;
          ACanvas.FillRect(Rect(APaintInfo.GetScaleX(ADrawRect.Right), APaintInfo.GetScaleY(ADrawRect.Top),
            APaintInfo.GetScaleX(ADrawRect.Right + 10), APaintInfo.GetScaleY(ADrawRect.Bottom)));
        end;

        ACanvas.MoveTo(APaintInfo.GetScaleX(AClearRect.Right) + 4, APaintInfo.GetScaleY(AClearRect.Bottom) - 8);
        ACanvas.LineTo(APaintInfo.GetScaleX(AClearRect.Right) + 6, APaintInfo.GetScaleY(AClearRect.Bottom) - 8);
        ACanvas.LineTo(APaintInfo.GetScaleX(AClearRect.Right) + 6, APaintInfo.GetScaleY(AClearRect.Bottom) - 3);

        ACanvas.MoveTo(APaintInfo.GetScaleX(AClearRect.Right),     APaintInfo.GetScaleY(AClearRect.Bottom) - 3);
        ACanvas.LineTo(APaintInfo.GetScaleX(AClearRect.Right) + 6, APaintInfo.GetScaleY(AClearRect.Bottom) - 3);

        ACanvas.MoveTo(APaintInfo.GetScaleX(AClearRect.Right) + 1, APaintInfo.GetScaleY(AClearRect.Bottom) - 4);
        ACanvas.LineTo(APaintInfo.GetScaleX(AClearRect.Right) + 1, APaintInfo.GetScaleY(AClearRect.Bottom) - 1);

        ACanvas.MoveTo(APaintInfo.GetScaleX(AClearRect.Right) + 2, APaintInfo.GetScaleY(AClearRect.Bottom) - 5);
        ACanvas.LineTo(APaintInfo.GetScaleX(AClearRect.Right) + 2, APaintInfo.GetScaleY(AClearRect.Bottom));
      finally
        SetViewportExtEx(ACanvas.Handle, APaintInfo.GetScaleX(APaintInfo.WindowWidth),
          APaintInfo.GetScaleY(APaintInfo.WindowHeight), @vPt);
      end;
    end
    else
    begin
      if SelectOffsetAfter_ then
      begin
        ACanvas.Brush.Color := Style.SelColor;
        ACanvas.FillRect(Rect(ADrawRect.Right, ADrawRect.Top, ADrawRect.Right + 10, ADrawRect.Bottom));
      end;

      ACanvas.MoveTo(AClearRect.Right + 4, AClearRect.Bottom - 8);
      ACanvas.LineTo(AClearRect.Right + 6, AClearRect.Bottom - 8);
      ACanvas.LineTo(AClearRect.Right + 6, AClearRect.Bottom - 3);

      ACanvas.MoveTo(AClearRect.Right,     AClearRect.Bottom - 3);
      ACanvas.LineTo(AClearRect.Right + 6, AClearRect.Bottom - 3);

      ACanvas.MoveTo(AClearRect.Right + 1, AClearRect.Bottom - 4);
      ACanvas.LineTo(AClearRect.Right + 1, AClearRect.Bottom - 1);

      ACanvas.MoveTo(AClearRect.Right + 2, AClearRect.Bottom - 5);
      ACanvas.LineTo(AClearRect.Right + 2, AClearRect.Bottom);
    end;
  end;
  {$ENDREGION}

begin
  inherited DoDrawItemPaintAfter(AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADataDrawLeft,
    ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then
  begin
    if AData.Style.ShowParaLastMark then  // ��ʾ��β�Ļ��з�
    begin
      if (ADrawItemNo < DrawItems.Count - 1) and DrawItems[ADrawItemNo + 1].ParaFirst then
        DrawLineLastMrak(ADrawRect)  // ��β�Ļ��з�
      else
      if ADrawItemNo = DrawItems.Count - 1 then
        DrawLineLastMrak(ADrawRect);  // ��β�Ļ��з�
    end;

    {$IFDEF SHOWDOMAINLEVEL}
    if (AData.Items[AItemNo] is THCDomainItem) then
      DrawDebugInfo(ACanvas, ADrawRect.Left, ADrawRect.Top - 12, IntToStr((AData.Items[AItemNo] as THCDomainItem).Level));
    {$ENDIF}
  end;
end;

procedure THCViewData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADataDrawLeft,
  ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDrawHotDomainBorde, vDrawActiveDomainBorde: Boolean;
  vDliRGN: HRGN;
begin
  inherited DoDrawItemPaintBefor(AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect, ADataDrawLeft,
    ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then  // ƴ����Χ
  begin
    vDrawHotDomainBorde := False;
    vDrawActiveDomainBorde := False;

    if Self.Style.DrawHotDomainRegion and (FHotDomain.BeginNo >= 0) then  // ��Hot��
      vDrawHotDomainBorde := FHotDomain.Contain(AItemNo);

    if Self.Style.DrawActiveDomainRegion and (FActiveDomain.BeginNo >= 0) then  // �м�����
      vDrawActiveDomainBorde := FActiveDomain.Contain(AItemNo);

    if vDrawHotDomainBorde or vDrawActiveDomainBorde then  // ��Hot��򼤻�����
    begin
      if ADrawRect.Left = ADrawRect.Right then
        vDliRGN := CreateRectRgn(ADrawRect.Left, ADrawRect.Top, ADrawRect.Right + 3, ADrawRect.Bottom)
      else
        vDliRGN := CreateRectRgn(ADrawRect.Left, ADrawRect.Top, ADrawRect.Right, ADrawRect.Bottom);

      try
        if vDrawHotDomainBorde then
          CombineRgn(FHotDomainRGN, FHotDomainRGN, vDliRGN, RGN_OR);

        if vDrawActiveDomainBorde then
          CombineRgn(FActiveDomainRGN, FActiveDomainRGN, vDliRGN, RGN_OR);
      finally
        DeleteObject(vDliRGN);
      end;
    end;
  end;
end;

procedure THCViewData.DoInsertItem(const AItem: THCCustomItem);
begin
  if THCDomainItem.IsBeginMark(AItem) then
    Inc(FDomainCount);

  FHotDomain.Clear;
  inherited DoInsertItem(AItem);
end;

function THCViewData.DoInsertTextBefor(const AItemNo, AOffset: Integer; const AText: string): Boolean;
begin
  Result := inherited DoInsertTextBefor(AItemNo, AOffset, AText);
  if Result and Assigned(FOnInsertTextBefor) then
    Result := FOnInsertTextBefor(Self, AItemNo, AOffset, AText);
end;

procedure THCViewData.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited DoLoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 42 then
    HCLoadTextFromStream(AStream, FScript, AFileVersion);
end;

function THCViewData.DoSaveItem(const AItemNo: Integer): Boolean;
var
  vItemNo: Integer;
begin
  Result := inherited DoSaveItem(AItemNo);
  if Result and (Self.Style.States.Contain(THCState.hosCopying)) then  // ���Ʊ���
  begin
    if Items[AItemNo].StyleNo = THCStyle.Domain then  // �����ʶ
    begin
      vItemNo := GetDomainAnother(AItemNo);  // ����ʼ
      Result := (vItemNo >= SelectInfo.StartItemNo) and (vItemNo <= SelectInfo.EndItemNo);
    end;
  end;
end;

function THCViewData.ExecuteScript(const AItemNo: Integer): Boolean;
begin
  Result := False;
end;

function THCViewData.GetDomainAnother(const AItemNo: Integer): Integer;
var
  vDomainItem: THCDomainItem;
  i: Integer;
begin
  Result := -1;

  // ���ⲿ��֤AItemNo��Ӧ����THCDomainItem
  vDomainItem := Self.Items[AItemNo] as THCDomainItem;
  if vDomainItem.MarkType = TMarkType.cmtEnd then  // �ǽ�����ʶ
  begin
    for i := AItemNo - 1 downto 0 do  // ����ʼ��ʶ
    begin
      if Items[i].StyleNo = THCStyle.Domain then
      begin
        if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // ����ʼ��ʶ
        begin
          if (Items[i] as THCDomainItem).Level = vDomainItem.Level then
          begin
            Result := i;
            Break;
          end;
        end;
      end;
    end;
  end
  else  // ����ʼ��ʶ
  begin
    for i := AItemNo + 1 to Self.Items.Count - 1 do  // �ҽ�����ʶ
    begin
      if Items[i].StyleNo = THCStyle.Domain then
      begin
        if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // �ǽ�����ʶ
        begin
          if (Items[i] as THCDomainItem).Level = vDomainItem.Level then
          begin
            Result := i;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure THCViewData.GetDomainFrom(const AItemNo, AOffset: Integer;
  const ADomainInfo: THCDomainInfo; const AIsWantDomain: TRectItemActionEvent = nil);

  function AskIsWantDomain(const AItemNo: Integer): Boolean;
  begin
    if Assigned(AIsWantDomain) then
      Result := AIsWantDomain(Items[AItemNo] as THCCustomRectItem)
    else
      Result := True;
  end;

var
  i, vStartNo, vEndNo, vCount: Integer;
  vLevel: Byte;
  vDomainItem: THCDomainItem;
begin
  ADomainInfo.Clear;

  if (AItemNo < 0) or (AOffset < 0) then Exit;

  { ����ʼ��ʶ }
  vStartNo := AItemNo;
  vEndNo := AItemNo;
  if (Items[AItemNo] is THCDomainItem) and AskIsWantDomain(AItemNo) then  // ��ʼλ�þ���Group
  begin
    vDomainItem := Items[AItemNo] as THCDomainItem;
    if vDomainItem.MarkType = TMarkType.cmtBeg then  // ��ʼλ������ʼ���
    begin
      if AOffset = OffsetAfter then  // ����ں���
      begin
        ADomainInfo.Data := Self;
        ADomainInfo.BeginNo := AItemNo;  // ��ǰ��Ϊ��ʼ��ʶ
        vLevel := vDomainItem.Level;
        vEndNo := AItemNo + 1;
      end
      else  // �����ǰ��
      begin
        if AItemNo > 0 then  // ���ǵ�һ��
          vStartNo := AItemNo - 1  // ��ǰһ����ǰ
        else  // ���ڵ�һ��ǰ��
          Exit;  // ��������
      end;
    end
    else  // ����λ���ǽ������
    begin
      if AOffset = OffsetAfter then  // ����ں���
      begin
        if AItemNo < Items.Count - 1 then  // �������һ��
          vEndNo := AItemNo + 1
        else  // �����һ������
          Exit;  // ��������
      end
      else  // �����ǰ��
      begin
        ADomainInfo.EndNo := AItemNo;
        vStartNo := AItemNo - 1;
      end;
    end;
  end;

  if ADomainInfo.BeginNo < 0 then  // û�ҵ���ʼ
  begin
    vCount := 0;

    if vStartNo < Self.Items.Count div 2 then  // ��ǰ���
    begin
      for i := vStartNo downto 0 do  // ����ǰ����ʼ
      begin
        if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
        begin
          vDomainItem := Items[i] as THCDomainItem;
          if vDomainItem.MarkType = TMarkType.cmtBeg then  // ��ʼ���
          begin
            if vCount > 0 then
              Dec(vCount)
            else
            begin
              ADomainInfo.Data := Self;
              ADomainInfo.BeginNo := i;
              vLevel := vDomainItem.Level;
              Break;
            end;
          end
          else  // �������
            Inc(vCount);  // ��Ƕ��
        end;
      end;

      if (ADomainInfo.BeginNo >= 0) and (ADomainInfo.EndNo < 0) then  // �ҽ�����ʶ
      begin
        for i := vEndNo to Items.Count - 1 do
        begin
          if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
          begin
            vDomainItem := Items[i] as THCDomainItem;
            if vDomainItem.MarkType = TMarkType.cmtEnd then  // �ǽ�β
            begin
              if vDomainItem.Level = vLevel then
              begin
                ADomainInfo.EndNo := i;
                Break;
              end;
            end;
          end;
        end;

        if ADomainInfo.EndNo < 0 then
          raise Exception.Create('�쳣����ȡ�����λ�ó���');
      end;
    end
    else  // �ں���
    begin
      for i := vEndNo to Self.Items.Count - 1 do  // �������ҽ���
      begin
        if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
        begin
          vDomainItem := Items[i] as THCDomainItem;
          if vDomainItem.MarkType = TMarkType.cmtEnd then  // �������
          begin
            if vCount > 0 then
              Dec(vCount)
            else
            begin
              ADomainInfo.EndNo := i;
              vLevel := vDomainItem.Level;
              Break;
            end;
          end
          else
            Inc(vCount);
        end;
      end;

      if (ADomainInfo.EndNo >= 0) and (ADomainInfo.BeginNo < 0) then  // ����ʼ��ʶ
      begin
        for i := vStartNo downto 0 do
        begin
          if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
          begin
            vDomainItem := Items[i] as THCDomainItem;
            if vDomainItem.MarkType = TMarkType.cmtBeg then  // ����ʼ
            begin
              if vDomainItem.Level = vLevel then
              begin
                ADomainInfo.Data := Self;
                ADomainInfo.BeginNo := i;
                Break;
              end;
            end;
          end;
        end;

        if ADomainInfo.BeginNo < 0 then
          raise Exception.Create('�쳣����ȡ����ʼλ�ó���');
      end;
    end;
  end
  else
  if ADomainInfo.EndNo < 0 then // �ҵ���ʼ�ˣ��ҽ���
  begin
    for i := vEndNo to Items.Count - 1 do
    begin
      if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
      begin
        vDomainItem := Items[i] as THCDomainItem;
        if vDomainItem.MarkType = TMarkType.cmtEnd then  // �ǽ�β
        begin
          if vDomainItem.Level = vLevel then
          begin
            ADomainInfo.EndNo := i;
            Break;
          end;
        end;
      end;
    end;

    if ADomainInfo.EndNo < 0 then
      raise Exception.Create('�쳣����ȡ�����λ�ó���');
  end;
end;

procedure THCViewData.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: THCCaretInfo);
var
  vTopData: THCCustomData;
  vRePaint: Boolean;
begin
  inherited GetCaretInfo(AItemNo, AOffset, ACaretInfo);

  vRePaint := False;

  // ��ֵ����Group��Ϣ������� MouseDown
  if Self.SelectInfo.StartItemNo >= 0 then
  begin
    vTopData := GetTopLevelData;
    if vTopData = Self then
    begin
      if Self.Style.DrawActiveDomainRegion and (FActiveDomain.BeginNo >= 0) then  // ԭ������Ϣ(����δͨ��������ƶ����ʱû�����)
        vRePaint := True;

      if FDomainCount > 0 then
        GetDomainFrom(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, FActiveDomain)  // ��ȡ��ǰ��괦ActiveDeGroup��Ϣ
      else
        FActiveDomain.Clear;

      if Self.Style.DrawActiveDomainRegion and (FActiveDomain.BeginNo >= 0) then
        vRePaint := True;
    end;
  end
  else
  if Self.Style.DrawActiveDomainRegion and (FActiveDomain.BeginNo >= 0) then
  begin
    FActiveDomain.Clear;
    vRePaint := True;
  end;

  if vRePaint then
    Style.UpdateInfoRePaint;

  if FCaretItemChanged then
  begin
    FCaretItemChanged := False;
    if Assigned(FOnCaretItemChanged) then
      FOnCaretItemChanged(Self, Items[SelectInfo.StartItemNo]);
  end;
end;

procedure THCViewData.GetCaretInfoCur(var ACaretInfo: THCCaretInfo);
begin
  if Style.UpdateInfo.DragingSelected then
    Self.GetCaretInfo(Self.MouseMoveItemNo, Self.MouseMoveItemOffset, ACaretInfo)
  else
    Self.GetCaretInfo(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, ACaretInfo);
end;

function THCViewData.InsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := inherited InsertItem(AItem);
  if Result then
  begin
    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
    Style.UpdateInfoReScroll;
  end;
end;

procedure THCViewData.InitializeField;
begin
  inherited InitializeField;
  FHotDomain.Clear;
  FActiveDomain.Clear;
end;

function THCViewData.InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
var
  vDomainItem: THCDomainItem;
begin
  Result := False;
  if not CanEdit then Exit;

  Undo_GroupBegin(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  try
    Self.Style.States.Include(hosBatchInsert);  // ���������ֹ����1����ʼ����ȡ���ʱ��������û������
    try
      // ����ͷ
      vDomainItem := CreateDefaultDomainItem as THCDomainItem;
      if Assigned(AMouldDomain) then
        vDomainItem.Assign(AMouldDomain);

      vDomainItem.MarkType := cmtBeg;
      if FActiveDomain.BeginNo >= 0 then
        vDomainItem.Level := (Items[FActiveDomain.BeginNo] as THCDomainItem).Level + 1;

      Result := InsertItem(vDomainItem);

      if Result then  // ����β
      begin
        vDomainItem := CreateDefaultDomainItem as THCDomainItem;
        if Assigned(AMouldDomain) then
          vDomainItem.Assign(AMouldDomain);

        vDomainItem.MarkType := cmtEnd;
        if FActiveDomain.BeginNo >= 0 then
          vDomainItem.Level := (Items[FActiveDomain.BeginNo] as THCDomainItem).Level + 1;

        Result := InsertItem(vDomainItem);
      end;
    finally
      Self.Style.States.Exclude(hosBatchInsert);
    end;
  finally
    Undo_GroupEnd(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  end;

  // ����������������ȡ��ǰλ����Ĺ��ܣ����Զ������ȡһ�µ�ǰλ�������
  ReSetSelectAndCaret(SelectInfo.StartItemNo);
end;

function THCViewData.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem; const AOffsetBefor: Boolean = True): Boolean;
begin
  Result := inherited InsertItem(AIndex, AItem, AOffsetBefor);
  if Result then
  begin
    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
    Style.UpdateInfoReScroll;
  end;
end;

procedure THCViewData.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // ��������Group��Ϣ����ֵ�� GetCaretInfo
  //if Self.Style.DrawActiveDomainRegion and (FActiveDomain.BeginNo >= 0) then
  //  Style.UpdateInfoRePaint;

  //FActiveDomain.Clear;

  inherited MouseDown(Button, Shift, X, Y);

//  if Button = TMouseButton.mbRight then  // �Ҽ��˵�ʱ������ȡ��괦FActiveDomain
//    Style.UpdateInfoReCaret;
end;

procedure THCViewData.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vTopData: THCViewData;
  vRePaint: Boolean;
begin
  vRePaint := Self.Style.DrawHotDomainRegion and (FHotDomain.BeginNo >= 0);
  //FHotDomain.Clear;
  inherited MouseMove(Shift, X, Y);
  //if not Self.MouseMoveRestrain then  // ʵʱ�ж���괦����
  begin
    if FDomainCount > 0 then
      Self.GetDomainFrom(Self.MouseMoveItemNo, Self.MouseMoveItemOffset, FHotDomain)  // ȡHotDeGroup
    else
      FHotDomain.Clear;

    vTopData := Self.GetTopLevelDataAt(X, Y) as THCViewData;
    if (vTopData = Self) or (vTopData.HotDomain.BeginNo < 0) then  // �������� �� ���㲻�����Ҷ���û��HotDeGroup  201711281352
    begin
      if Self.Style.DrawHotDomainRegion and (FHotDomain.BeginNo >= 0) then  // ��FHotDeGroup
        vRePaint := True;
    end;
  end;

  if vRePaint then
    Style.UpdateInfoRePaint;
end;

procedure THCViewData.PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawRight,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom, AVOffset, AFristDItemNo,
  ALastDItemNo: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vOldColor: TColor;
begin
  if not APaintInfo.Print then  // �Ǵ�ӡ���Ƽ���������
  begin
    if Self.Style.DrawHotDomainRegion then
      FHotDomainRGN := CreateRectRgn(0, 0, 0, 0);

    if Self.Style.DrawActiveDomainRegion then
      FActiveDomainRGN := CreateRectRgn(0, 0, 0, 0);
  end;

  inherited PaintData(ADataDrawLeft, ADataDrawTop, ADataDrawRight, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, AVOffset, AFristDItemNo, ALastDItemNo,
    ACanvas, APaintInfo);

  if not APaintInfo.Print then  // �Ǵ�ӡ���Ƽ���������
  begin
    vOldColor := ACanvas.Brush.Color;  // ��Ϊʹ��Brush���Ʊ߿�������Ҫ����ԭ��ɫ
    try
      if Self.Style.DrawHotDomainRegion then
      begin
        if DoPaintDomainRegion(FHotDomain.BeginNo) then
        begin
          ACanvas.Brush.Color := clActiveBorder;
          FrameRgn(ACanvas.Handle, FHotDomainRGN, ACanvas.Brush.Handle, 1, 1);
        end;

        DeleteObject(FHotDomainRGN);
      end;

      if Self.Style.DrawActiveDomainRegion then
      begin
        if DoPaintDomainRegion(FActiveDomain.BeginNo) then
        begin
          ACanvas.Brush.Color := clBlue;
          FrameRgn(ACanvas.Handle, FActiveDomainRGN, ACanvas.Brush.Handle, 1, 1);
        end;

        DeleteObject(FActiveDomainRGN);
      end;
    finally
      ACanvas.Brush.Color := vOldColor;
    end;
  end;
end;

function THCViewData.Replace(const AText: string): Boolean;
begin
//  DeleteSelected;
  InsertText(AText);
end;

procedure THCViewData.ReSetSelectAndCaret(const AItemNo, AOffset: Integer; const ANextWhenMid: Boolean = False);
begin
  inherited ReSetSelectAndCaret(AItemNo, AOffset, ANextWhenMid);
  if not Self.Style.States.Contain(THCState.hosBatchInsert) then
  begin
    if FDomainCount > 0 then
      GetDomainFrom(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, FActiveDomain)  // ��ȡ��ǰ��괦ActiveDeGroup��Ϣ
    else
      FActiveDomain.Clear;
  end;
end;

//procedure THCViewData.SaveDomainToStream(const AStream: TStream; const ADomainItemNo: Integer);
//var
//  vGroupBeg, vGroupEnd: Integer;
//begin
//  vGroupEnd := GetDomainAnother(ADomainItemNo);
//
//  if vGroupEnd > ADomainItemNo then
//    vGroupBeg := ADomainItemNo
//  else
//  begin
//    vGroupBeg := vGroupEnd;
//    vGroupEnd := ADomainItemNo;
//  end;
//
//  _SaveFileFormatAndVersion(AStream);  // �ļ���ʽ�Ͱ汾
//  Self.Style.SaveToStream(AStream);
//  SaveItemToStream(AStream, vGroupBeg + 1, 0, vGroupEnd - 1, GetItemOffsetAfter(vGroupEnd - 1));
//end;

procedure THCViewData.SaveToStream(const AStream: TStream);
begin
  inherited SaveToStream(AStream);
  HCSaveTextToStream(AStream, FScript);
end;

function THCViewData.Search(const AKeyword: string; const AForward,
  AMatchCase: Boolean): Boolean;
var
  vKeyword: string;

  {$REGION ' DoSearchByOffset '}
  function DoSearchByOffset(const AItemNo, AOffset: Integer): Boolean;

    function ReversePos(const SubStr, S: String): Integer;
    var
      i : Integer;
    begin
      Result := 0;

      i := Pos(ReverseString(SubStr), ReverseString(S));
      if i > 0 then
      begin
        i := Length(S) - i - Length(SubStr) + 2;
        Result := i;
      end;
    end;

  var
    vPos, vItemNo: Integer;
    vText, vConcatText, vOverText: string;
  begin
    Result := False;
    if not Self.Items[AItemNo].Visible then Exit;

    if Self.Items[AItemNo].StyleNo < THCStyle.Null then
    begin
      Result := (Self.Items[AItemNo] as THCCustomRectItem).Search(AKeyword, AForward, AMatchCase);
      if Result then
      begin
        Self.SelectInfo.StartItemNo := AItemNo;
        Self.SelectInfo.StartItemOffset := OffsetInner;
        Self.SelectInfo.EndItemNo := -1;
        Self.SelectInfo.EndItemOffset := -1;
      end;
    end
    else
    begin
      if AForward then  // ��ǰ��
      begin
        vText := (Self.Items[AItemNo] as THCTextItem).SubString(1, AOffset);
        if not AMatchCase then  // �����ִ�Сд
          vText := UpperCase(vText);

        vPos := ReversePos(vKeyword, vText);  // һ���ַ�������һ���ַ����������ֵ�λ��(��LastDelimiter�����ִ�Сд)
      end
      else  // �����
      begin
        vText := (Self.Items[AItemNo] as THCTextItem).SubString(AOffset + 1,
          Self.Items[AItemNo].Length - AOffset);
        if not AMatchCase then  // �����ִ�Сд
          vText := UpperCase(vText);

        vPos := Pos(vKeyword, vText);
      end;

      if vPos > 0 then  // ��ǰItem��ƥ��
      begin
        Self.SelectInfo.StartItemNo := AItemNo;

        if AForward then  // ��ǰ��
          Self.SelectInfo.StartItemOffset := vPos - 1
        else  // �����
          Self.SelectInfo.StartItemOffset := AOffset + vPos - 1;

        Self.SelectInfo.EndItemNo := AItemNo;
        Self.SelectInfo.EndItemOffset := Self.SelectInfo.StartItemOffset + Length(vKeyword);

        Result := True;
      end
      else  // û�ҵ�ƥ�䣬������ͬ�����ڵ�TextItem�ϲ������
      if (vText <> '') and (Length(vKeyword) > 1) then
      begin
        if AForward then  // ��ǰ����ͬ������
        begin
          vItemNo := AItemNo;
          vConcatText := vText;
          vOverText := '';

          while (vItemNo > 0)
            and (not Self.Items[vItemNo].ParaFirst)
            and (Self.Items[vItemNo - 1].StyleNo > THCStyle.Null)
          do
          begin
            if not Self.Items[vItemNo - 1].Visible then
            begin
              Dec(vItemNo);
              Continue;
            end;

            if Length(vText) > Length(vKeyword) - 1 then
              vText := RightStr((Self.Items[vItemNo - 1] as THCTextItem).TextEffective, Length(vKeyword) - 1);

            vOverText := vOverText + vText;  // ��¼ƴ���˶��ٸ��ַ�
            vConcatText := vText + vConcatText;  // ƴ�Ӻ���ַ�
            if not AMatchCase then  // �����ִ�Сд
              vConcatText := UpperCase(vConcatText);

            vPos := Pos(vKeyword, vConcatText);
            if vPos > 0 then  // �ҵ���
            begin
              Self.SelectInfo.StartItemNo := vItemNo - 1;
              Self.SelectInfo.StartItemOffset := Self.Items[vItemNo - 1].Length - (Length(vText) - vPos) - 1;

              Self.SelectInfo.EndItemNo := AItemNo;
              Self.SelectInfo.EndItemOffset := vPos + Length(vKeyword) - 1  // �ؼ�������ַ���ƫ��λ��
                - Length(vText);  // ��ȥ��ǰ��Itemռ�Ŀ��

              while vItemNo < AItemNo do  // ��ȥ�м�Item�Ŀ��
              begin
                if Self.Items[vItemNo].Visible then
                  Self.SelectInfo.EndItemOffset := Self.SelectInfo.EndItemOffset - Length((Self.Items[vItemNo] as THCTextItem).TextEffective);

                Inc(vItemNo);
              end;

              Result := True;

              Break;
            end
            else  // ��ǰ���ŵ�û�ҵ�
            begin
              if Length(vOverText) >= Length(vKeyword) - 1 then  // ƴ�ӵĳ����˹ؼ��ֳ��ȣ�˵����ǰ�ı��ͺ����ƴ�Ӻ�û�п�ƥ��
                Break;
            end;

            Dec(vItemNo);
          end;
        end
        else  // �����ͬ������
        begin
          vItemNo := AItemNo;
          vConcatText := vText;
          vOverText := '';

          while (vItemNo < Self.Items.Count - 1)
            and (not Self.Items[vItemNo + 1].ParaFirst)
            and (Self.Items[vItemNo + 1].StyleNo > THCStyle.Null)
          do  // ͬ�κ����TextItem
          begin
            if (not Self.Items[vItemNo + 1].Visible) then
            begin
              Inc(vItemNo);
              Continue;
            end;

            if Length(vText) > Length(vKeyword) - 1 then
              vText := LeftStr((Self.Items[vItemNo + 1] as THCTextItem).TextEffective, Length(vKeyword) - 1);

            vOverText := vOverText + vText;  // ��¼ƴ���˶��ٸ��ַ�
            vConcatText := vConcatText + vText;  // ƴ�Ӻ���ַ�
            if not AMatchCase then  // �����ִ�Сд
              vConcatText := UpperCase(vConcatText);

            vPos := Pos(vKeyword, vConcatText);
            if vPos > 0 then  // �ҵ���
            begin
              Self.SelectInfo.StartItemNo := AItemNo;
              Self.SelectInfo.StartItemOffset := AOffset + vPos - 1;

              Self.SelectInfo.EndItemNo := vItemNo + 1;
              Self.SelectInfo.EndItemOffset := vPos + Length(vKeyword) - 1  // �ؼ�������ַ���ƫ��λ��
                - (Self.Items[AItemNo].Length - AOffset);  // ��ȥ��ǰ��Itemռ�Ŀ��

              while vItemNo >= AItemNo + 1 do  // ��ȥ�м�Item�Ŀ��
              begin
                if Self.Items[vItemNo].Visible then
                  Self.SelectInfo.EndItemOffset := Self.SelectInfo.EndItemOffset - Length((Self.Items[vItemNo] as THCTextItem).TextEffective);

                Dec(vItemNo);
              end;

              Result := True;

              Break;
            end
            else  // ��ǰ���ŵ�û�ҵ�
            begin
              if Length(vOverText) >= Length(vKeyword) - 1 then  // ƴ�ӵĳ����˹ؼ��ֳ��ȣ�˵����ǰ�ı��ͺ����ƴ�Ӻ�û�п�ƥ��
                Break;
            end;

            Inc(vItemNo);
          end;
        end;
      end;
    end;
  end;
  {$ENDREGION}

var
  i, vItemNo, vOffset: Integer;
begin
  Result := False;

  if not AMatchCase then  // �����ִ�Сд
    vKeyword := UpperCase(AKeyword)
  else
    vKeyword := AKeyword;

  if Self.SelectInfo.StartItemNo < 0 then
  begin
    vItemNo := 0;
    vOffset := 0;
  end
  else
  if Self.SelectInfo.EndItemNo >= 0 then  // ��ѡ�и��ݲ��ҷ�����������￪ʼ
  begin
    if AForward then
    begin
      vItemNo := Self.SelectInfo.StartItemNo;
      vOffset := Self.SelectInfo.StartItemOffset;
    end
    else
    begin
      vItemNo := Self.SelectInfo.EndItemNo;
      vOffset := Self.SelectInfo.EndItemOffset;
    end;

    DisSelect;  // ���ԭ����ѡ��״̬
  end
  else
  begin
    vItemNo := Self.SelectInfo.StartItemNo;
    vOffset := Self.SelectInfo.StartItemOffset;
  end;

  Result := DoSearchByOffset(vItemNo, vOffset);

  if not Result then
  begin
    if AForward then  // ��ǰ��
    begin
      for i := vItemNo - 1 downto 0 do
      begin
        if DoSearchByOffset(i, GetItemOffsetAfter(i)) then
        begin
          Result := True;
          Break;
        end;
      end;
    end
    else  // �����
    begin
      for i := vItemNo + 1 to Self.Items.Count - 1 do
      begin
        if DoSearchByOffset(i, 0) then
        begin
          Result := True;
          Break;
        end;
      end;
    end
  end;

  if not Result then  // û�ҵ�
  begin
    if Self.SelectInfo.EndItemNo >= 0 then
    begin
      if not AForward then  // �����
      begin
        vItemNo := Self.SelectInfo.EndItemNo;
        vOffset := Self.SelectInfo.EndItemOffset;
      end
      else
      begin
        vItemNo := Self.SelectInfo.StartItemNo;
        vOffset := Self.SelectInfo.StartItemOffset;
      end;

      DisSelect;  // �������ҵ�ѡ��״̬
      Self.SelectInfo.StartItemNo := vItemNo;
      Self.SelectInfo.StartItemOffset := vOffset;
    end
    else
    if Self.SelectInfo.StartItemNo < 0 then
    begin
      Self.SelectInfo.StartItemNo := vItemNo;
      Self.SelectInfo.StartItemOffset := vOffset;
    end;
  end
  else
  begin
    vItemNo := SelectInfo.StartItemNo;
    vOffset := SelectInfo.StartItemOffset;

    if SelectInfo.EndItemNo >= 0 then
      ReSetSelectAndCaret(SelectInfo.EndItemNo, SelectInfo.EndItemOffset, not aForward)
    else
      ReSetSelectAndCaret(vItemNo, vOffset, not aForward);

    SelectInfo.StartItemNo := vItemNo;
    SelectInfo.StartItemOffset := vOffset;
    MatchItemSelectState;
  end;

  Self.Style.UpdateInfoRePaint;
  Self.Style.UpdateInfoReCaret;
end;

procedure THCViewData.SelectFirstItemBeforWithCaret;
begin
  ReSetSelectAndCaret(0, 0);
end;

procedure THCViewData.SelectItemAfterWithCaret(const AItemNo: Integer);
begin
  ReSetSelectAndCaret(AItemNo);
end;

procedure THCViewData.SelectLastItemAfterWithCaret;
begin
  SelectItemAfterWithCaret(Items.Count - 1);
end;

procedure THCViewData.SetSelectBound(const AStartNo, AStartOffset, AEndNo,
  AEndOffset: Integer; const ASilence: Boolean = True);
var
  vStartNo, vEndNo, vStartOffset, vEndOffset: Integer;
begin
  if AEndNo < 0 then  // ѡ��һ����
  begin
    vStartNo := AStartNo;
    vStartOffset := AStartOffset;
    vEndNo := -1;
    vEndOffset := -1;
  end
  else
  if AEndNo >= AStartNo then  // ��ǰ����ѡ��
  begin
    vStartNo := AStartNo;
    vEndNo := AEndNo;

    if AEndNo = AStartNo then  // ͬһ��Item
    begin
      if AEndOffset >= AStartOffset then  // ����λ������ʼ����
      begin
        vStartOffset := AStartOffset;
        vEndOffset := AEndOffset;
      end
      else  // ����λ������ʼǰ��
      begin
        vStartOffset := AEndOffset;
        vEndOffset := AStartOffset;
      end;
    end
    else  // ����ͬһ��Item
    begin
      vStartOffset := AStartOffset;
      vEndOffset := AEndOffset;
    end;
  end
  else  // AEndNo < AStartNo �Ӻ���ǰѡ��
  begin
    vStartNo := AEndNo;
    vStartOffset := AEndOffset;

    vEndNo := AStartNo;
    vEndOffset := vStartOffset;
  end;

  Self.AdjustSelectRange(vStartNo, vStartOffset, vEndNo, vEndOffset);
  Self.MatchItemSelectState;

  //FSelectSeekNo  �����Ҫȷ�� FSelectSeekNo���˷������ƶ���CustomRichData
  if not ASilence then
  begin
    ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, True);
    Self.Style.UpdateInfoRePaint;
  end
  else  // ȡ������ʽ����ֹInsertText����
  begin
    FCurStyleNo := Items[SelectInfo.StartItemNo].StyleNo;
    FCurParaNo := Items[SelectInfo.StartItemNo].ParaNo;
  end;
end;

procedure THCViewData.TraverseItem(const ATraverse: THCItemTraverse);
var
  i: Integer;
  vDomainInfo: THCDomainNode;
begin
  if ATraverse <> nil then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if ATraverse.Stop then Break;

      if Items[i] is THCDomainItem then
      begin
        if THCDomainItem.IsBeginMark(Items[i]) then
        begin
          vDomainInfo := ATraverse.DomainStack.Peek;
          vDomainInfo := vDomainInfo.AppendChild;
          //GetDomainFrom(i, OffsetAfter, vDomainInfo);
          vDomainInfo.Data := Self;
          vDomainInfo.BeginNo := i;
          ATraverse.DomainStack.Push(vDomainInfo);
        end
        else
        begin
          vDomainInfo := ATraverse.DomainStack.Pop;
          vDomainInfo.EndNo := i;
        end;
      end;

      ATraverse.Process(Self, i, ATraverse.Tag, ATraverse.DomainStack, ATraverse.Stop);
      if Items[i].StyleNo < THCStyle.Null then
        (Items[i] as THCCustomRectItem).TraverseItem(ATraverse);
    end;
  end;
end;

end.
