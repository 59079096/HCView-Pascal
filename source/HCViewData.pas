{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{            支持域管理的文档对象管理单元               }
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

  THCViewData = class(THCViewDevData)  // 具有高亮域和操作Items功能的Data类
  private
    FScript: string;
    FCaretItemChanged: Boolean;
    FDomainStartDeletes: THCIntegerList;  // 仅用于选中删除时，当域起始结束都选中时，删除了结束后标明起始的可删除
    FHotDomain,  // 当前高亮域
    FActiveDomain  // 当前激活域
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
    /// <summary> 是否允许保存该Item </summary>
    function DoSaveItem(const AItemNo: Integer): Boolean; override;
    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    /// <summary> 用于从流加载完Items后，检查不合格的Item并删除 </summary>
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
    /// <summary> 根据传入的域"模具"创建域 </summary>
    /// <param name="AMouldDomain">"模具"调用完此方法后请自行释放</param>
    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
    /// <summary> 获取指定位置所在的域信息 </summary>
    procedure GetDomainFrom(const AItemNo, AOffset: Integer; const ADomainInfo: THCDomainInfo;
      const AIsWantDomain: TRectItemActionEvent = nil);
    /// <summary> 设置选中范围 </summary>
    /// <param name="ASilence">是否"静默"调用，False：响应此选中操作的相关动作(光标位置、界面更新) True：不响应(外部自己处理)</param>
    procedure SetSelectBound(const AStartNo, AStartOffset, AEndNo, AEndOffset: Integer;
      const ASilence: Boolean = True);

    /// <summary> 光标选到指定Item的最后面 </summary>
    procedure SelectItemAfterWithCaret(const AItemNo: Integer);

    /// <summary> 光标选到最后一个Item的最后面 </summary>
    procedure SelectLastItemAfterWithCaret;

    /// <summary> 光标选到第一个Item的最前面 </summary>
    procedure SelectFirstItemBeforWithCaret;

    /// <summary> 获取DomainItem配对的另一个ItemNo </summary>
    /// <param name="AItemNo">当前DomainItem(头或尾)</param>
    function GetDomainAnother(const AItemNo: Integer): Integer;

    /// <summary> 当前位置开始查找指定的内容 </summary>
    /// <param name="AKeyword">要查找的关键字</param>
    /// <param name="AForward">True：向前，False：向后</param>
    /// <param name="AMatchCase">True：区分大小写，False：不区分大小写</param>
    /// <returns>True：找到</returns>
    function Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean;
    function Replace(const AText: string): Boolean;

    procedure GetCaretInfoCur(var ACaretInfo: THCCaretInfo);
    procedure TraverseItem(const ATraverse: THCItemTraverse);

    /// <summary>
    /// 保存域内容到文件流，因为存版本和样式和HCView层面的DataSaveLiteStream重复，暂时又不用此方法，所以没有把DataSaveLiteStream重构到此单元
    /// </summary>
    /// <param name="AStream">流</param>
    /// <param name="ADomainItemNo">域起始或结束ItemNo</param>
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
    if Items[AItemNo].StyleNo = THCStyle.Domain then  // 是域标识
    begin
      if (Items[AItemNo] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 域结束标识
      begin
        vItemNo := GetDomainAnother(AItemNo);  // 找起始
        if vItemNo = SelectInfo.StartItemNo then  // 域起始在选中起始
          Result := SelectInfo.StartItemOffset = OffsetBefor  // 也在全先范围内
        else
          Result := (vItemNo >= SelectInfo.StartItemNo) and (vItemNo <= SelectInfo.EndItemNo);

        if Result then  // 起始也在选中删除范围内
          FDomainStartDeletes.Add(vItemNo);  // 记录下来
      end
      else  // 域起始标记
        Result := FDomainStartDeletes.IndexOf(AItemNo) >= 0;  // 结束标识已经被标记为可删除
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
    if Items[i] is THCDomainItem then  // 域标识
    begin
      if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 是起始
        Inc(vLevel);

      Assert(vLevel > -1);
      (Items[i] as THCDomainItem).Level := vLevel;

      if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 是结束
        Dec(vLevel);
    end;
  end;

  // 域内容整体替换，如果域是单独的段，段样式随插入内容
  if Style.States.Contain(hosDomainWholeReplace) then
  begin
    // [ 是段首，紧接其后面插入非段首
    if (not Items[AStartNo].ParaFirst) and (AStartNo > 0)  // 不是段首，不是Data第一个
      and (Items[AStartNo - 1] is THCDomainItem) and (Items[AStartNo - 1].ParaFirst)
    then
      Items[AStartNo - 1].ParaNo := Items[AStartNo].ParaNo;

    if IsParaLastItem(AEndNo + 1) and (Items[AEndNo + 1] is THCDomainItem) then
      Items[AEndNo + 1].ParaNo := Items[AEndNo].ParaNo;
  end;

  Exit;  // 目前的稳定性应该不会出现不匹配的问题了
  // 检查加载或粘贴等从流插入Items不匹配的域起始结束标识并删除
  vDelCount := 0;
  for i := AStartNo to AEndNo do  // 从前往后找没有插入起始标识的域，删除单独的域结束标识
  begin
    if Items[i] is THCDomainItem then  // 域标识
    begin
      if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 是结束，说明没有插入对应的起始
      begin
        if i < AEndNo then  // 不是最后，后面继承其段起始属性
          Items[i + 1].ParaFirst := Items[i].ParaFirst;

        Items.Delete(i);
        Inc(vDelCount);

        if (i > AStartNo) and (i <= AEndNo - vDelCount) then  // 删除了中间的
        begin
          if (not Items[i - 1].ParaFirst)
            and (not Items[i].ParaFirst)
            and MergeItemText(Items[i - 1], Items[i])  // 前后都不是段首，且能合并
          then
          begin
            Items.Delete(i);
            Inc(vDelCount);
          end;
        end;

        Break;
      end
      else  // 是起始域标记，不用担心了
        Break;
    end;
  end;

  for i := AEndNo - vDelCount downto AStartNo do  // 从后往前，找没有插入结束标识的域
  begin
    if Items[i] is THCDomainItem then  // 域标识
    begin
      if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 是起始，说明没有插入对应的结束
      begin
        if i < AEndNo - vDelCount then  // 不是最后，后面继承其段起始属性
          Items[i + 1].ParaFirst := Items[i].ParaFirst;

        Items.Delete(i);
        Inc(vDelCount);

        if (i > AStartNo) and (i <= AEndNo - vDelCount) then  // 删除了中间的
        begin
          if (not Items[i - 1].ParaFirst)
            and (not Items[i].ParaFirst)
            and MergeItemText(Items[i - 1], Items[i])  // 前后都不是段首，且能合并
          then
          begin
            Items.Delete(i);
            Inc(vDelCount);
          end;
        end;

        Break;
      end
      else  // 是结束域标记，不用担心了
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

  if Assigned(FOnCreateItemByStyle) then  // 自定义的类型在此处解析
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
  if Items[AStartNo].ParaFirst then  // 域起始是段首
  begin
    if AEndNo = vParaLastItemNo then  // 域结束是段尾
    begin
      if AStartNo > 0 then  // 段删除干净了要从上一段最后开始格式化
        vFirstDrawItemNo := GetFormatFirstDrawItem(Items[AStartNo].FirstDItemNo - 1);
    end
    else  // 域结束不是段尾，起始是段首
    begin
      UndoAction_ItemParaFirst(AEndNo + 1, 0, True);
      Items[AEndNo + 1].ParaFirst := True;
    end;
  end;

  FormatPrepare(vFirstDrawItemNo, vParaLastItemNo);

  vDelCount := 0;
  vBeginPageBreak := Items[AStartNo].PageBreak;

  for i := AEndNo downto AStartNo do  // 删除域及域范围内的Item
  begin
    //if CanDeleteItem(i) then  // 允许删除
    begin
      UndoAction_DeleteItem(i, 0);
      Items.Delete(i);

      Inc(vDelCount);
    end;
  end;

  FActiveDomain.Clear;

  if AStartNo = 0 then  // 删除完了
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
  FDomainStartDeletes.Clear;  // 清空域删除时记录前后配对信息
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

  if Assigned(FHotDomain) then  // Destroy时释放了FHotDomain，父类释放Items时触发DoRemoveItem
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
    if (not Result) and (SelectInfo.EndItemNo >= 0) then  // 看看是不是选中的边界
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

  {$REGION ' DrawLineLastMrak 段尾的换行符 '}
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
    if AData.Style.ShowParaLastMark then  // 显示段尾的换行符
    begin
      if (ADrawItemNo < DrawItems.Count - 1) and DrawItems[ADrawItemNo + 1].ParaFirst then
        DrawLineLastMrak(ADrawRect)  // 段尾的换行符
      else
      if ADrawItemNo = DrawItems.Count - 1 then
        DrawLineLastMrak(ADrawRect);  // 段尾的换行符
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

  if not APaintInfo.Print then  // 拼接域范围
  begin
    vDrawHotDomainBorde := False;
    vDrawActiveDomainBorde := False;

    if Self.Style.DrawHotDomainRegion and (FHotDomain.BeginNo >= 0) then  // 有Hot域
      vDrawHotDomainBorde := FHotDomain.Contain(AItemNo);

    if Self.Style.DrawActiveDomainRegion and (FActiveDomain.BeginNo >= 0) then  // 有激活域
      vDrawActiveDomainBorde := FActiveDomain.Contain(AItemNo);

    if vDrawHotDomainBorde or vDrawActiveDomainBorde then  // 在Hot域或激活域中
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
  if Result and (Self.Style.States.Contain(THCState.hosCopying)) then  // 复制保存
  begin
    if Items[AItemNo].StyleNo = THCStyle.Domain then  // 是域标识
    begin
      vItemNo := GetDomainAnother(AItemNo);  // 找起始
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

  // 请外部保证AItemNo对应的是THCDomainItem
  vDomainItem := Self.Items[AItemNo] as THCDomainItem;
  if vDomainItem.MarkType = TMarkType.cmtEnd then  // 是结束标识
  begin
    for i := AItemNo - 1 downto 0 do  // 找起始标识
    begin
      if Items[i].StyleNo = THCStyle.Domain then
      begin
        if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 是起始标识
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
  else  // 是起始标识
  begin
    for i := AItemNo + 1 to Self.Items.Count - 1 do  // 找结束标识
    begin
      if Items[i].StyleNo = THCStyle.Domain then
      begin
        if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 是结束标识
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

  { 找起始标识 }
  vStartNo := AItemNo;
  vEndNo := AItemNo;
  if (Items[AItemNo] is THCDomainItem) and AskIsWantDomain(AItemNo) then  // 起始位置就是Group
  begin
    vDomainItem := Items[AItemNo] as THCDomainItem;
    if vDomainItem.MarkType = TMarkType.cmtBeg then  // 起始位置是起始标记
    begin
      if AOffset = OffsetAfter then  // 光标在后面
      begin
        ADomainInfo.Data := Self;
        ADomainInfo.BeginNo := AItemNo;  // 当前即为起始标识
        vLevel := vDomainItem.Level;
        vEndNo := AItemNo + 1;
      end
      else  // 光标在前面
      begin
        if AItemNo > 0 then  // 不是第一个
          vStartNo := AItemNo - 1  // 从前一个往前
        else  // 是在第一个前面
          Exit;  // 不用找了
      end;
    end
    else  // 查找位置是结束标记
    begin
      if AOffset = OffsetAfter then  // 光标在后面
      begin
        if AItemNo < Items.Count - 1 then  // 不是最后一个
          vEndNo := AItemNo + 1
        else  // 是最后一个后面
          Exit;  // 不用找了
      end
      else  // 光标在前面
      begin
        ADomainInfo.EndNo := AItemNo;
        vStartNo := AItemNo - 1;
      end;
    end;
  end;

  if ADomainInfo.BeginNo < 0 then  // 没找到起始
  begin
    vCount := 0;

    if vStartNo < Self.Items.Count div 2 then  // 在前半程
    begin
      for i := vStartNo downto 0 do  // 先往前找起始
      begin
        if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
        begin
          vDomainItem := Items[i] as THCDomainItem;
          if vDomainItem.MarkType = TMarkType.cmtBeg then  // 起始标记
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
          else  // 结束标记
            Inc(vCount);  // 有嵌套
        end;
      end;

      if (ADomainInfo.BeginNo >= 0) and (ADomainInfo.EndNo < 0) then  // 找结束标识
      begin
        for i := vEndNo to Items.Count - 1 do
        begin
          if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
          begin
            vDomainItem := Items[i] as THCDomainItem;
            if vDomainItem.MarkType = TMarkType.cmtEnd then  // 是结尾
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
          raise Exception.Create('异常：获取域结束位置出错！');
      end;
    end
    else  // 在后半程
    begin
      for i := vEndNo to Self.Items.Count - 1 do  // 先往后找结束
      begin
        if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
        begin
          vDomainItem := Items[i] as THCDomainItem;
          if vDomainItem.MarkType = TMarkType.cmtEnd then  // 结束标记
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

      if (ADomainInfo.EndNo >= 0) and (ADomainInfo.BeginNo < 0) then  // 找起始标识
      begin
        for i := vStartNo downto 0 do
        begin
          if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
          begin
            vDomainItem := Items[i] as THCDomainItem;
            if vDomainItem.MarkType = TMarkType.cmtBeg then  // 是起始
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
          raise Exception.Create('异常：获取域起始位置出错！');
      end;
    end;
  end
  else
  if ADomainInfo.EndNo < 0 then // 找到起始了，找结束
  begin
    for i := vEndNo to Items.Count - 1 do
    begin
      if (Items[i] is THCDomainItem) and AskIsWantDomain(i) then
      begin
        vDomainItem := Items[i] as THCDomainItem;
        if vDomainItem.MarkType = TMarkType.cmtEnd then  // 是结尾
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
      raise Exception.Create('异常：获取域结束位置出错！');
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

  // 赋值激活Group信息，清除在 MouseDown
  if Self.SelectInfo.StartItemNo >= 0 then
  begin
    vTopData := GetTopLevelData;
    if vTopData = Self then
    begin
      if Self.Style.DrawActiveDomainRegion and (FActiveDomain.BeginNo >= 0) then  // 原来有信息(处理未通过鼠标点击移动光标时没有清除)
        vRePaint := True;

      if FDomainCount > 0 then
        GetDomainFrom(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, FActiveDomain)  // 获取当前光标处ActiveDeGroup信息
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
    Self.Style.States.Include(hosBatchInsert);  // 批量插入防止插入1个起始触发取配对时，结束还没插入呢
    try
      // 插入头
      vDomainItem := CreateDefaultDomainItem as THCDomainItem;
      if Assigned(AMouldDomain) then
        vDomainItem.Assign(AMouldDomain);

      vDomainItem.MarkType := cmtBeg;
      if FActiveDomain.BeginNo >= 0 then
        vDomainItem.Level := (Items[FActiveDomain.BeginNo] as THCDomainItem).Level + 1;

      Result := InsertItem(vDomainItem);

      if Result then  // 插入尾
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

  // 因上面批量屏蔽了取当前位置域的功能，所以都插完后，取一下当前位置域情况
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
  // 清除激活的Group信息，赋值在 GetCaretInfo
  //if Self.Style.DrawActiveDomainRegion and (FActiveDomain.BeginNo >= 0) then
  //  Style.UpdateInfoRePaint;

  //FActiveDomain.Clear;

  inherited MouseDown(Button, Shift, X, Y);

//  if Button = TMouseButton.mbRight then  // 右键菜单时，重新取光标处FActiveDomain
//    Style.UpdateInfoReCaret;
end;

procedure THCViewData.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vTopData: THCViewData;
  vRePaint: Boolean;
begin
  vRePaint := Self.Style.DrawHotDomainRegion and (FHotDomain.BeginNo >= 0);
  FHotDomain.Clear;
  inherited MouseMove(Shift, X, Y);
  //if not Self.MouseMoveRestrain then  // 实时判断鼠标处的域
  begin
    if FDomainCount > 0 then
      Self.GetDomainFrom(Self.MouseMoveItemNo, Self.MouseMoveItemOffset, FHotDomain)  // 取HotDeGroup
    else
      FHotDomain.Clear;

    vTopData := Self.GetTopLevelDataAt(X, Y) as THCViewData;
    if (vTopData = Self) or (vTopData.HotDomain.BeginNo < 0) then  // 顶层是我 或 顶层不是我且顶层没有HotDeGroup  201711281352
    begin
      if Self.Style.DrawHotDomainRegion and (FHotDomain.BeginNo >= 0) then  // 有FHotDeGroup
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
  if not APaintInfo.Print then  // 非打印绘制激活数据组
  begin
    if Self.Style.DrawHotDomainRegion then
      FHotDomainRGN := CreateRectRgn(0, 0, 0, 0);

    if Self.Style.DrawActiveDomainRegion then
      FActiveDomainRGN := CreateRectRgn(0, 0, 0, 0);
  end;

  inherited PaintData(ADataDrawLeft, ADataDrawTop, ADataDrawRight, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, AVOffset, AFristDItemNo, ALastDItemNo,
    ACanvas, APaintInfo);

  if not APaintInfo.Print then  // 非打印绘制激活数据组
  begin
    vOldColor := ACanvas.Brush.Color;  // 因为使用Brush绘制边框所以需要缓存原颜色
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
      GetDomainFrom(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, FActiveDomain)  // 获取当前光标处ActiveDeGroup信息
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
//  _SaveFileFormatAndVersion(AStream);  // 文件格式和版本
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
      if AForward then  // 向前找
      begin
        vText := (Self.Items[AItemNo] as THCTextItem).SubString(1, AOffset);
        if not AMatchCase then  // 不区分大小写
          vText := UpperCase(vText);

        vPos := ReversePos(vKeyword, vText);  // 一个字符串在另一个字符串中最后出现的位置(用LastDelimiter不区分大小写)
      end
      else  // 向后找
      begin
        vText := (Self.Items[AItemNo] as THCTextItem).SubString(AOffset + 1,
          Self.Items[AItemNo].Length - AOffset);
        if not AMatchCase then  // 不区分大小写
          vText := UpperCase(vText);

        vPos := Pos(vKeyword, vText);
      end;

      if vPos > 0 then  // 当前Item有匹配
      begin
        Self.SelectInfo.StartItemNo := AItemNo;

        if AForward then  // 向前找
          Self.SelectInfo.StartItemOffset := vPos - 1
        else  // 向后找
          Self.SelectInfo.StartItemOffset := AOffset + vPos - 1;

        Self.SelectInfo.EndItemNo := AItemNo;
        Self.SelectInfo.EndItemOffset := Self.SelectInfo.StartItemOffset + Length(vKeyword);

        Result := True;
      end
      else  // 没找到匹配，尝试在同段相邻的TextItem合并后查找
      if (vText <> '') and (Length(vKeyword) > 1) then
      begin
        if AForward then  // 向前，在同段中找
        begin
          vItemNo := AItemNo;
          vConcatText := vText;
          vOverText := '';

          while (vItemNo > 0)
            and (not Self.Items[vItemNo].ParaFirst)
            and (Self.Items[vItemNo - 1].StyleNo > THCStyle.Null)
          do
          begin
            if Self.Items[vItemNo - 1].Length < Length(vKeyword) then
            begin
              Dec(vItemNo);
              Continue;
            end;

            vText := RightStr(Self.Items[vItemNo - 1].Text, Length(vKeyword) - 1);  // 取后面比关键字少一个字符长度的，以便和当前末尾最后一个拼接
            vOverText := vOverText + vText;  // 记录拼接了多少个字符
            vConcatText := vText + vConcatText;  // 拼接后的字符
            if not AMatchCase then  // 不区分大小写
              vConcatText := UpperCase(vConcatText);

            vPos := Pos(vKeyword, vConcatText);
            if vPos > 0 then  // 找到了
            begin
              Self.SelectInfo.StartItemNo := vItemNo - 1;
              Self.SelectInfo.StartItemOffset := Self.Items[vItemNo - 1].Length - (Length(vText) - vPos) - 1;

              Self.SelectInfo.EndItemNo := AItemNo;
              Self.SelectInfo.EndItemOffset := vPos + Length(vKeyword) - 1  // 关键字最后字符的偏移位置
                - Length(vText);  // 减去最前面Item占的宽度

              while vItemNo < AItemNo do  // 减去中间Item的宽度
              begin
                Self.SelectInfo.EndItemOffset := Self.SelectInfo.EndItemOffset - Self.Items[vItemNo].Length;
                Inc(vItemNo);
              end;

              Result := True;

              Break;
            end
            else  // 当前接着的没找到
            begin
              if Length(vOverText) >= Length(vKeyword) - 1 then  // 拼接的超过了关键字长度，说明当前文本和后面的拼接后没有可匹配
                Break;
            end;

            Dec(vItemNo);
          end;
        end
        else  // 向后，在同段中找
        begin
          vItemNo := AItemNo;
          vConcatText := vText;
          vOverText := '';

          while (vItemNo < Self.Items.Count - 1)
            and (not Self.Items[vItemNo + 1].ParaFirst)
            and (Self.Items[vItemNo + 1].StyleNo > THCStyle.Null)
          do  // 同段后面的TextItem
          begin
            if Self.Items[vItemNo + 1].Length < Length(vKeyword) then
            begin
              Inc(vItemNo);
              Continue;
            end;

            vText := LeftStr(Self.Items[vItemNo + 1].Text, Length(vKeyword) - 1);  // 取后面比关键字少一个字符长度的，以便和当前末尾最后一个拼接
            vOverText := vOverText + vText;  // 记录拼接了多少个字符
            vConcatText := vConcatText + vText;  // 拼接后的字符
            if not AMatchCase then  // 不区分大小写
              vConcatText := UpperCase(vConcatText);

            vPos := Pos(vKeyword, vConcatText);
            if vPos > 0 then  // 找到了
            begin
              Self.SelectInfo.StartItemNo := AItemNo;
              Self.SelectInfo.StartItemOffset := AOffset + vPos - 1;

              Self.SelectInfo.EndItemNo := vItemNo + 1;
              Self.SelectInfo.EndItemOffset := vPos + Length(vKeyword) - 1  // 关键字最后字符的偏移位置
                - (Self.Items[AItemNo].Length - AOffset);  // 减去最前面Item占的宽度

              while vItemNo >= AItemNo + 1 do  // 减去中间Item的宽度
              begin
                Self.SelectInfo.EndItemOffset := Self.SelectInfo.EndItemOffset - Self.Items[vItemNo].Length;
                Dec(vItemNo);
              end;

              Result := True;

              Break;
            end
            else  // 当前接着的没找到
            begin
              if Length(vOverText) >= Length(vKeyword) - 1 then  // 拼接的超过了关键字长度，说明当前文本和后面的拼接后没有可匹配
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

  if not AMatchCase then  // 不区分大小写
    vKeyword := UpperCase(AKeyword)
  else
    vKeyword := AKeyword;

  if Self.SelectInfo.StartItemNo < 0 then
  begin
    vItemNo := 0;
    vOffset := 0;
  end
  else
  if Self.SelectInfo.EndItemNo >= 0 then  // 有选中根据查找方向决定从哪里开始
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

    DisSelect;  // 清除原来的选中状态
  end
  else
  begin
    vItemNo := Self.SelectInfo.StartItemNo;
    vOffset := Self.SelectInfo.StartItemOffset;
  end;

  Result := DoSearchByOffset(vItemNo, vOffset);

  if not Result then
  begin
    if AForward then  // 向前找
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
    else  // 向后找
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

  if not Result then  // 没找到
  begin
    if Self.SelectInfo.EndItemNo >= 0 then
    begin
      if not AForward then  // 向后找
      begin
        vItemNo := Self.SelectInfo.EndItemNo;
        vOffset := Self.SelectInfo.EndItemOffset;
      end
      else
      begin
        vItemNo := Self.SelectInfo.StartItemNo;
        vOffset := Self.SelectInfo.StartItemOffset;
      end;

      DisSelect;  // 清除最后找到选中状态
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
    ReSetSelectAndCaret(SelectInfo.EndItemNo, SelectInfo.EndItemOffset, not aForward);

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
  if AEndNo < 0 then  // 选择一部分
  begin
    vStartNo := AStartNo;
    vStartOffset := AStartOffset;
    vEndNo := -1;
    vEndOffset := -1;
  end
  else
  if AEndNo >= AStartNo then  // 从前往后选择
  begin
    vStartNo := AStartNo;
    vEndNo := AEndNo;

    if AEndNo = AStartNo then  // 同一个Item
    begin
      if AEndOffset >= AStartOffset then  // 结束位置在起始后面
      begin
        vStartOffset := AStartOffset;
        vEndOffset := AEndOffset;
      end
      else  // 结束位置在起始前面
      begin
        vStartOffset := AEndOffset;
        vEndOffset := AStartOffset;
      end;
    end
    else  // 不在同一个Item
    begin
      vStartOffset := AStartOffset;
      vEndOffset := AEndOffset;
    end;
  end
  else  // AEndNo < AStartNo 从后往前选择
  begin
    vStartNo := AEndNo;
    vStartOffset := AEndOffset;

    vEndNo := AStartNo;
    vEndOffset := vStartOffset;
  end;

  Self.AdjustSelectRange(vStartNo, vStartOffset, vEndNo, vEndOffset);
  Self.MatchItemSelectState;

  //FSelectSeekNo  如果需要确定 FSelectSeekNo，此方法得移动到CustomRichData
  if not ASilence then
  begin
    ReSetSelectAndCaret(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, True);
    Self.Style.UpdateInfoRePaint;
  end
  else  // 取最新样式，防止InsertText操作
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
