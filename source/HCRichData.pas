{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{             文档内各类对象高级管理单元                }
{                                                       }
{*******************************************************}

unit HCRichData;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, Generics.Collections, HCCustomData,
  HCCustomRichData, HCItem, HCStyle, HCParaStyle, HCTextStyle, HCTextItem, HCRectItem,
  HCCommon, HCUndoRichData, HCList, HCFloatItem;

type
  TDomain = class
  strict private
    FBeginNo, FEndNo: Integer;
  public
    constructor Create;
    procedure Clear;
    function Contain(const AItemNo: Integer): Boolean;
    property BeginNo: Integer read FBeginNo write FBeginNo;
    property EndNo: Integer read FEndNo write FEndNo;
  end;

  TDrawItemPaintEvent = procedure (const AData: THCCustomData;
    const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
    const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;

  TStyleItemEvent = function (const AStyleNo: Integer): THCCustomItem of object;

  THCRichData = class(THCUndoRichData)  // 富文本数据类，可做为其他显示富文本类的基类
  private
    FDomainStartDeletes: THCIntegerList;  // 仅用于选中删除时，当域起始结束都选中时，删除了结束后标明起始的可删除
    FHotDomain,  // 当前高亮域
    FActiveDomain  // 当前激活域
      : TDomain;
    FHotDomainRGN, FActiveDomainRGN: HRGN;
    FDrawActiveDomainRegion, FDrawHotDomainRegion: Boolean;  // 是否绘制域边框
    FOnDrawItemPaintAfter: TDrawItemPaintEvent;
    FOnCreateItemByStyle: TStyleItemEvent;

    FFloatItems: TObjectList<THCFloatItem>;  // THCItems支持Add时控制暂时不用
    FFloatItemIndex, FMouseDownIndex, FMouseMoveIndex,
    FMouseX, FMouseY
      : Integer;
    function GetFloatItemAt(const X, Y: Integer): Integer;
    function MouseDownFloatItem(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function MouseMoveFloatItem(Shift: TShiftState; X, Y: Integer): Boolean;
    function MouseUpFloatItem(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    procedure PaintFloatItems(const ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

    procedure GetDomainFrom(const AItemNo, AOffset: Integer;
      const ADomain: TDomain);
    function GetActiveDomain: TDomain;
  protected
    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; override;
    function CanDeleteItem(const AItemNo: Integer): Boolean; override;

    /// <summary> 用于从流加载完Items后，检查不合格的Item并删除 </summary>
    function CheckInsertItemCount(const AStartNo, AEndNo: Integer): Integer; override;

    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AStyle: THCStyle); override;
    destructor Destroy; override;

    procedure Clear; override;
    function CreateDefaultDomainItem: THCCustomItem; override;
    function CreateDefaultTextItem: THCCustomItem; override;
    procedure PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure InitializeField; override;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: TCaretInfo); override;
    function DeleteSelected: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function InsertItem(const AItem: THCCustomItem): Boolean; override;
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem;
      const AOffsetBefor: Boolean = True): Boolean; override;

    /// <summary> 插入浮动Item </summary>
    function InsertFloatItem(const AFloatItem: THCFloatItem): Boolean;

    /// <summary> 设置选中范围，仅供外部使用内部不使用 </summary>
    procedure SetSelectBound(const AStartNo, AStartOffset, AEndNo, AEndOffset: Integer);

    /// <summary> 光标选到指定Item的最后面 </summary>
    procedure SelectItemAfterWithCaret(const AItemNo: Integer);

    /// <summary> 光村选到最后一个Item的最后面 </summary>
    procedure SelectLastItemAfterWithCaret;

    /// <summary> 获取DomainItem配对的另一个ItemNo </summary>
    function GetDomainAnother(const AItemNo: Integer): Integer;

    /// <summary> 当前位置开始查找指定的内容 </summary>
    /// <param name="AKeyword">要查找的关键字</param>
    /// <param name="AForward">True：向前，False：向后</param>
    /// <param name="AMatchCase">True：区分大小写，False：不区分大小写</param>
    /// <returns>True：找到</returns>
    function Search(const AKeyword: string; const AForward, AMatchCase: Boolean): Boolean;

    procedure GetCaretInfoCur(var ACaretInfo: TCaretInfo);
    procedure TraverseItem(const ATraverse: TItemTraverse);

    property FloatItems: TObjectList<THCFloatItem> read FFloatItems;
    property HotDomain: TDomain read FHotDomain;
    property ActiveDomain: TDomain read GetActiveDomain;
    property OnDrawItemPaintAfter: TDrawItemPaintEvent read FOnDrawItemPaintAfter write FOnDrawItemPaintAfter;
    property OnCreateItemByStyle: TStyleItemEvent read FOnCreateItemByStyle write FOnCreateItemByStyle;
  end;

implementation

uses
  StrUtils;

{ THCRichData }

function THCRichData.CanDeleteItem(const AItemNo: Integer): Boolean;
var
  vItemNo: Integer;
begin
  Result := inherited CanDeleteItem(AItemNo);
  if Result then
  begin
    if Items[AItemNo].StyleNo = THCStyle.RsDomain then  // 是域标识
    begin
      if (Items[AItemNo] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 域结束标识
      begin
        vItemNo := GetDomainAnother(AItemNo);  // 找起始
        Result := (vItemNo >= SelectInfo.StartItemNo) and (vItemNo <= SelectInfo.EndItemNo);
        if Result then  // 起始也在选中删除范围内
          FDomainStartDeletes.Add(vItemNo);  // 记录下来
      end
      else  // 域起始标记
        Result := FDomainStartDeletes.IndexOf(AItemNo) >= 0;  // 结束标识已经删除了
    end;
  end;
end;

function THCRichData.CheckInsertItemCount(const AStartNo,
  AEndNo: Integer): Integer;
var
  i, vDelCount: Integer;
begin
  Result := inherited CheckInsertItemCount(AStartNo, AEndNo);

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

procedure THCRichData.Clear;
begin
  FFloatItemIndex := -1;
  FMouseDownIndex := -1;
  FMouseMoveIndex := -1;
  FFloatItems.Clear;

  inherited Clear;
end;

constructor THCRichData.Create(const AStyle: THCStyle);
begin
  FFloatItems := TObjectList<THCFloatItem>.Create;
  FFloatItemIndex := -1;
  FMouseDownIndex := -1;
  FMouseMoveIndex := -1;

  FDomainStartDeletes := THCIntegerList.Create;
  FHotDomain := TDomain.Create;
  FActiveDomain := TDomain.Create;
  inherited Create(AStyle);
end;

function THCRichData.CreateDefaultDomainItem: THCCustomItem;
begin
  Result := HCDefaultDomainItemClass.Create(Self);
end;

function THCRichData.CreateDefaultTextItem: THCCustomItem;
begin
  Result := HCDefaultTextItemClass.CreateByText('');  // 必需有参数否则不能调用属性创建
  if Style.CurStyleNo < THCStyle.RsNull then
    Result.StyleNo := 0
  else
    Result.StyleNo := Style.CurStyleNo;

  Result.ParaNo := Style.CurParaNo;
  if Assigned(OnCreateItem) then
    OnCreateItem(Result);
end;

function THCRichData.CreateItemByStyle(const AStyleNo: Integer): THCCustomItem;
begin
  Result := nil;

  if Assigned(FOnCreateItemByStyle) then  // 自定义的类型在此处解析
    Result := FOnCreateItemByStyle(AStyleNo);

  if not Assigned(Result) then
    Result := inherited CreateItemByStyle(AStyleNo);
end;

function THCRichData.DeleteSelected: Boolean;
begin
  FDomainStartDeletes.Clear;  // 清空域删除时记录前后配对信息
  Result := inherited DeleteSelected;
end;

destructor THCRichData.Destroy;
begin
  FHotDomain.Free;
  FActiveDomain.Free;
  FDomainStartDeletes.Free;
  FFloatItems.Free;
  inherited Destroy;
end;

procedure THCRichData.DoDrawItemPaintAfter(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

  {$REGION ' DrawLineLastMrak 段尾的换行符 '}
  procedure DrawLineLastMrak(const ADrawRect: TRect);
  var
    vPt: TPoint;
  begin
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clActiveBorder;

    SetViewportExtEx(ACanvas.Handle, APaintInfo.WindowWidth, APaintInfo.WindowHeight, @vPt);
    try
      ACanvas.MoveTo(APaintInfo.GetScaleX(ADrawRect.Right) + 4,
        APaintInfo.GetScaleY(ADrawRect.Bottom) - 8);
      ACanvas.LineTo(APaintInfo.GetScaleX(ADrawRect.Right) + 6, APaintInfo.GetScaleY(ADrawRect.Bottom) - 8);
      ACanvas.LineTo(APaintInfo.GetScaleX(ADrawRect.Right) + 6, APaintInfo.GetScaleY(ADrawRect.Bottom) - 3);

      ACanvas.MoveTo(APaintInfo.GetScaleX(ADrawRect.Right),     APaintInfo.GetScaleY(ADrawRect.Bottom) - 3);
      ACanvas.LineTo(APaintInfo.GetScaleX(ADrawRect.Right) + 6, APaintInfo.GetScaleY(ADrawRect.Bottom) - 3);

      ACanvas.MoveTo(APaintInfo.GetScaleX(ADrawRect.Right) + 1, APaintInfo.GetScaleY(ADrawRect.Bottom) - 4);
      ACanvas.LineTo(APaintInfo.GetScaleX(ADrawRect.Right) + 1, APaintInfo.GetScaleY(ADrawRect.Bottom) - 1);

      ACanvas.MoveTo(APaintInfo.GetScaleX(ADrawRect.Right) + 2, APaintInfo.GetScaleY(ADrawRect.Bottom) - 5);
      ACanvas.LineTo(APaintInfo.GetScaleX(ADrawRect.Right) + 2, APaintInfo.GetScaleY(ADrawRect.Bottom));
    finally
      SetViewportExtEx(ACanvas.Handle, APaintInfo.GetScaleX(APaintInfo.WindowWidth),
        APaintInfo.GetScaleY(APaintInfo.WindowHeight), @vPt);
    end;
  end;
  {$ENDREGION}

begin
  inherited DoDrawItemPaintAfter(AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then
  begin
    if AData.Style.ShowLineLastMark then
    begin
      if (ADrawItemNo < DrawItems.Count - 1) and DrawItems[ADrawItemNo + 1].ParaFirst then
        DrawLineLastMrak(ADrawRect)  // 段尾的换行符
      else
      if ADrawItemNo = DrawItems.Count - 1 then
        DrawLineLastMrak(ADrawRect);  // 段尾的换行符
    end;
  end;

  if Assigned(FOnDrawItemPaintAfter) then
  begin
    FOnDrawItemPaintAfter(AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
      ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCRichData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDrawHotDomainBorde, vDrawActiveDomainBorde: Boolean;
  vItemNo: Integer;
  vDliRGN: HRGN;
begin
  inherited DoDrawItemPaintBefor(AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then
  begin
    vDrawHotDomainBorde := False;
    vDrawActiveDomainBorde := False;
    vItemNo := DrawItems[ADrawItemNo].ItemNo;

    if FHotDomain.BeginNo >= 0 then  // 有Hot域
      vDrawHotDomainBorde := FHotDomain.Contain(vItemNo);

    if FActiveDomain.BeginNo >= 0 then  // 有激活域
      vDrawActiveDomainBorde := FActiveDomain.Contain(vItemNo);

    if vDrawHotDomainBorde or vDrawActiveDomainBorde then  // 在Hot域或激活域中
    begin
      vDliRGN := CreateRectRgn(ADrawRect.Left, ADrawRect.Top, ADrawRect.Right, ADrawRect.Bottom);
      try
        if (FHotDomain.BeginNo >= 0) and vDrawHotDomainBorde then
          CombineRgn(FHotDomainRGN, FHotDomainRGN, vDliRGN, RGN_OR);
        if (FActiveDomain.BeginNo >= 0) and vDrawActiveDomainBorde then
          CombineRgn(FActiveDomainRGN, FActiveDomainRGN, vDliRGN, RGN_OR);
      finally
        DeleteObject(vDliRGN);
      end;
    end;
  end;
end;

function THCRichData.GetDomainAnother(const AItemNo: Integer): Integer;
var
  vDomain: THCDomainItem;
  i, vIgnore: Integer;
begin
  Result := -1;
  vIgnore := 0;

  // 请外部保证AItemNo对应的是THCDomainItem
  vDomain := Self.Items[AItemNo] as THCDomainItem;
  if vDomain.MarkType = TMarkType.cmtEnd then  // 是结束标识
  begin
    for i := AItemNo - 1 downto 0 do  // 找起始标识
    begin
      if Items[i].StyleNo = THCStyle.RsDomain then
      begin
        if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 是起始标识
        begin
          if vIgnore = 0 then
          begin
            Result := i;
            Break;
          end
          else
            Dec(vIgnore);
        end
        else
          Inc(vIgnore);
      end;
    end;
  end
  else  // 是起始标识
  begin
    for i := AItemNo + 1 to Self.Items.Count - 1 do  // 找结束标识
    begin
      if Items[i].StyleNo = THCStyle.RsDomain then
      begin
        if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 是结束标识
        begin
          if vIgnore = 0 then
          begin
            Result := i;
            Break;
          end
          else
            Dec(vIgnore);
        end
        else
          Inc(vIgnore);
      end;
    end;
  end;
end;

procedure THCRichData.GetDomainFrom(const AItemNo, AOffset: Integer;
  const ADomain: TDomain);
var
  i, vStartNo, vEndNo, vCount: Integer;
begin
  ADomain.Clear;

  if (AItemNo < 0) or (AOffset < 0) then Exit;

  { 找起始标识 }
  vCount := 0;
  // 确定往前找的起始位置
  vStartNo := AItemNo;
  vEndNo := AItemNo;
  if Items[AItemNo] is THCDomainItem then  // 起始位置就是Group
  begin
    if (Items[AItemNo] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 查找位置是起始标记
    begin
      if AOffset = OffsetAfter then  // 光标在后面
      begin
        ADomain.BeginNo := AItemNo;  // 当前即为起始标识
        vEndNo := AItemNo + 1;
      enD
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
        ADomain.EndNo := AItemNo;
        vStartNo := AItemNo - 1;
      end;
    end;
  end;

  if ADomain.BeginNo < 0 then
  begin
    for i := vStartNo downto 0 do  // 找
    begin
      if Items[i] is THCDomainItem then
      begin
        if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 起始标记
        begin
          if vCount <> 0 then  // 有嵌套
            Dec(vCount)
          else
          begin
            ADomain.BeginNo := i;
            Break;
          end;
        end
        else  // 结束标记
          Inc(vCount);  // 有嵌套
      end;
    end;
  end;

  { 找结束标识 }
  if (ADomain.BeginNo >= 0) and (ADomain.EndNo < 0) then
  begin
    vCount := 0;
    for i := vEndNo to Items.Count - 1 do
    begin
      if Items[i] is THCDomainItem then
      begin
        if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 是结尾
        begin
          if vCount <> 0 then
            Dec(vCount)
          else
          begin
            ADomain.EndNo := i;
            Break;
          end;
        end
        else  // 是起始标记
          Inc(vCount);  // 有嵌套
      end;
    end;

    if ADomain.EndNo < 0 then
      raise Exception.Create('异常：获取数据组结束出错！');
  end;
end;

function THCRichData.GetFloatItemAt(const X, Y: Integer): Integer;
var
  i: Integer;
  vFloatItem: THCFloatItem;
begin
  Result := -1;
  for i := 0 to FFloatItems.Count - 1 do
  begin
    vFloatItem := FFloatItems[i];

    if vFloatItem.PtInClient(X - vFloatItem.Left, Y - vFloatItem.Top) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function THCRichData.GetActiveDomain: TDomain;
begin
  Result := nil;
  if FActiveDomain.BeginNo >= 0 then
    Result := FActiveDomain;
end;

procedure THCRichData.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: TCaretInfo);
var
  vTopData: THCCustomRichData;
begin
  if FFloatItemIndex >= 0 then
  begin
    ACaretInfo.Visible := False;
    Exit;
  end;

  inherited GetCaretInfo(AItemNo, AOffset, ACaretInfo);

  // 赋值激活Group信息，清除在 MouseDown
  if Self.SelectInfo.StartItemNo >= 0 then
  begin
    vTopData := GetTopLevelData;
    if vTopData = Self then
    begin
      if FActiveDomain.BeginNo >= 0 then  // 原来有信息(处理未通过鼠标点击移动光标时没有清除)
      begin
        FActiveDomain.Clear;
        FDrawActiveDomainRegion := False;
        Style.UpdateInfoRePaint;
      end;
      // 获取当前光标处ActiveDeGroup信息
      Self.GetDomainFrom(Self.SelectInfo.StartItemNo, Self.SelectInfo.StartItemOffset, FActiveDomain);
      if FActiveDomain.BeginNo >= 0 then
      begin
        FDrawActiveDomainRegion := True;
        Style.UpdateInfoRePaint;
      end;
    end;
  end;
end;

procedure THCRichData.GetCaretInfoCur(var ACaretInfo: TCaretInfo);
begin
  if Style.UpdateInfo.Draging then
    Self.GetCaretInfo(Self.MouseMoveItemNo, Self.MouseMoveItemOffset, ACaretInfo)
  else
    Self.GetCaretInfo(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, ACaretInfo);
end;

function THCRichData.InsertItem(const AItem: THCCustomItem): Boolean;
begin
  Result := inherited InsertItem(AItem);
  if Result then
  begin
    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
    Style.UpdateInfoReScroll;
  end;
end;

procedure THCRichData.InitializeField;
begin
  inherited InitializeField;
  FActiveDomain.Clear;
  FHotDomain.Clear;
end;

function THCRichData.InsertFloatItem(const AFloatItem: THCFloatItem): Boolean;
var
  vTopData: THCRichData;
  vStartNo, vStartOffset, vDrawNo: Integer;
begin
  vTopData := Self.GetTopLevelData as THCRichData;

  // 记录选中起始位置
  vStartNo := vTopData.SelectInfo.StartItemNo;
  vStartOffset := vTopData.SelectInfo.StartItemOffset;

  // 取选中起始处的DrawItem
  vDrawNo := vTopData.GetDrawItemNoByOffset(vStartNo, vStartOffset);

  AFloatItem.Left := vTopData.DrawItems[vDrawNo].Rect.Left
    + vTopData.GetDrawItemOffsetWidth(vDrawNo, vTopData.SelectInfo.StartItemOffset - vTopData.DrawItems[vDrawNo].CharOffs + 1);
  AFloatItem.Top := vTopData.DrawItems[vDrawNo].Rect.Top;

  vTopData.FloatItems.Add(AFloatItem);
  AFloatItem.Active := True;

  Result := True;

  if not vTopData.DisSelect then
    Style.UpdateInfoRePaint;
end;

function THCRichData.InsertItem(const AIndex: Integer;
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

procedure THCRichData.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  // 清除激活的Group信息，赋值在 GetCaretInfo
  if FActiveDomain.BeginNo >= 0 then
    Style.UpdateInfoRePaint;
  FActiveDomain.Clear;
  FDrawActiveDomainRegion := False;

  if not MouseDownFloatItem(Button, Shift, X, Y) then
    inherited MouseDown(Button, Shift, X, Y);

  if Button = TMouseButton.mbRight then  // 右键菜单时，重新取光标处FActiveDomain
    Style.UpdateInfoReCaret;
end;

function THCRichData.MouseDownFloatItem(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := True;

  FMouseDownIndex := GetFloatItemAt(X, Y);
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
    FFloatItems[FFloatItemIndex].MouseDown(Button, Shift,
      X - FFloatItems[FFloatItemIndex].Left, Y - FFloatItems[FFloatItemIndex].Top);
  end;

  if FMouseDownIndex < 0 then
    Result := False
  else
  begin
    FMouseX := X;
    FMouseY := Y;
  end;
end;

procedure THCRichData.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vTopData: THCRichData;
begin
  // 清除 FHotDeGroup 信息
  if FHotDomain.BeginNo >= 0 then
    Style.UpdateInfoRePaint;
  FHotDomain.Clear;
  FDrawHotDomainRegion := False;

  if not MouseMoveFloatItem(Shift, X, Y) then
    inherited MouseMove(Shift, X, Y);

  if not Self.MouseMoveRestrain then  // 在Item上
  begin
    Self.GetDomainFrom(Self.MouseMoveItemNo, Self.MouseMoveItemOffset, FHotDomain);  // 取HotDeGroup
    vTopData := Self.GetTopLevelDataAt(X, Y) as THCRichData;
    if (vTopData = Self) or (not vTopData.FDrawHotDomainRegion) then  // 顶层是我 或 顶层不是我且顶层没有HotDeGroup  201711281352
    begin
      if FHotDomain.BeginNo >= 0 then  // 有FHotDeGroup
      begin
        FDrawHotDomainRegion := True;
        Style.UpdateInfoRePaint;
      end;
    end;
  end;
end;

function THCRichData.MouseMoveFloatItem(Shift: TShiftState; X, Y: Integer): Boolean;
var
  vItemIndex: Integer;
  vFloatItem: THCFloatItem;
begin
  Result := True;

  if (Shift = [ssLeft]) and (FMouseDownIndex >= 0) then  // 按下拖拽
  begin
    vFloatItem := FFloatItems[FMouseDownIndex];
    vFloatItem.MouseMove(Shift, X - vFloatItem.Left, Y - vFloatItem.Top);

    if not vFloatItem.Resizing then
    begin
      vFloatItem.Left := vFloatItem.Left + X - FMouseX;
      vFloatItem.Top := vFloatItem.Top + Y - FMouseY;

      FMouseX := X;
      FMouseY := Y;
    end;

    Style.UpdateInfoRePaint;
  end
  else  // 普通鼠标移动
  begin
    vItemIndex := GetFloatItemAt(X, Y);
    if FMouseMoveIndex <> vItemIndex then
    begin
      if FMouseMoveIndex >= 0 then  // 旧的移出
        FFloatItems[FMouseMoveIndex].MouseLeave;

      FMouseMoveIndex := vItemIndex;
      if FMouseMoveIndex >= 0 then  // 新的移入
        FFloatItems[FMouseMoveIndex].MouseEnter;
    end;

    if vItemIndex >= 0 then
    begin
      vFloatItem := FFloatItems[vItemIndex];
      vFloatItem.MouseMove(Shift, X - vFloatItem.Left, Y - vFloatItem.Top);
    end
    else
      Result := False;
  end;
end;

procedure THCRichData.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if not MouseUpFloatItem(Button, Shift, X, Y) then
    inherited MouseUp(Button, Shift, X, Y);
end;

function THCRichData.MouseUpFloatItem(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer): Boolean;
var
  vFloatItem: THCFloatItem;
begin
  Result := True;

  if FMouseDownIndex >= 0 then
  begin
    vFloatItem := FFloatItems[FMouseDownIndex];
    {if vFloatItem.Resizing then
      Self.Style.UpdateInfoRePaint;}
    vFloatItem.MouseUp(Button, Shift, X - vFloatItem.Left, Y - vFloatItem.Top);
  end
  else
    Result := False;
end;

procedure THCRichData.PaintData(const ADataDrawLeft, ADataDrawTop,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vOldColor: TColor;
begin
  if not APaintInfo.Print then  // 非打印绘制激活数据组
  begin
    if FDrawHotDomainRegion then
      FHotDomainRGN := CreateRectRgn(0, 0, 0, 0);

    if FDrawActiveDomainRegion then
      FActiveDomainRGN := CreateRectRgn(0, 0, 0, 0);
  end;

  inherited PaintData(ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, AVOffset, ACanvas, APaintInfo);

  PaintFloatItems(ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, AVOffset, ACanvas, APaintInfo);

  if not APaintInfo.Print then  // 非打印绘制激活数据组
  begin
    vOldColor := ACanvas.Brush.Color;  // 因为使用Brush绘制边框所以需要缓存原颜色
    try
      if FDrawHotDomainRegion then
      begin
        ACanvas.Brush.Color := clActiveBorder;
        FrameRgn(ACanvas.Handle, FHotDomainRGN, ACanvas.Brush.Handle, 1, 1);
        DeleteObject(FHotDomainRGN);
      end;

      if FDrawActiveDomainRegion then
      begin
        ACanvas.Brush.Color := clBlue;
        FrameRgn(ACanvas.Handle, FActiveDomainRGN, ACanvas.Brush.Handle, 1, 1);
        DeleteObject(FActiveDomainRGN);
      end;
    finally
      ACanvas.Brush.Color := vOldColor;
    end;
  end;
end;

procedure THCRichData.PaintFloatItems(const ADataDrawLeft, ADataDrawTop,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  i: Integer;
  vFloatItem: THCFloatItem;
  vRect: TRect;
begin
  for i := 0 to FFloatItems.Count - 1 do
  begin
    vFloatItem := FFloatItems[i];

    if (vFloatItem.Top + vFloatItem.Height + ADataDrawTop > 0)  // 底部超过页顶端
      and (vFloatItem.Top < ADataDrawBottom)  // 顶部不超过页底端
    then
    begin
      vRect := Bounds(vFloatItem.Left, vFloatItem.Top, vFloatItem.Width, vFloatItem.Height);
      vRect.Offset(ADataDrawLeft, ADataDrawTop - AVOffset);

      vFloatItem.PaintTo(Self.Style, vRect, ADataDrawTop, ADataDrawBottom,
        ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
    end;
  end;
end;

function THCRichData.Search(const AKeyword: string; const AForward,
  AMatchCase: Boolean): Boolean;
var
  vKeyword: string;

  function DoItemSearch(const AItemNo, AOffset: Integer): Boolean;

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
    vPos: Integer;
    vText: string;
  begin
    Result := False;

    if Self.Items[AItemNo].StyleNo < THCStyle.RsNull then
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
        vText := (Self.Items[AItemNo] as THCTextItem).GetTextPart(1, AOffset);
        if not AMatchCase then  // 不区分大小写
          vText := UpperCase(vText);

        vPos := ReversePos(vKeyword, vText);  // 一个字符串在另一个字符串中最后出现的位置(用LastDelimiter不区分大小写)
      end
      else  // 向后找
      begin
        vText := (Self.Items[AItemNo] as THCTextItem).GetTextPart(AOffset + 1,
          Self.Items[AItemNo].Length - AOffset);
        if not AMatchCase then  // 不区分大小写
          vText := UpperCase(vText);

        vPos := Pos(vKeyword, vText);
      end;

      if vPos > 0 then  // 找到了
      begin
        Self.SelectInfo.StartItemNo := AItemNo;

        if AForward then  // 向前找
          Self.SelectInfo.StartItemOffset := vPos - 1
        else  // 向后找
          Self.SelectInfo.StartItemOffset := AOffset + vPos - 1;

        Self.SelectInfo.EndItemNo := AItemNo;
        Self.SelectInfo.EndItemOffset := Self.SelectInfo.StartItemOffset + Length(vKeyword);

        Result := True;
      end;
    end;
  end;

var
  i, vItemNo, vOffset: Integer;
begin
  Result := False;

  if not AMatchCase then  // 不区分大小写
    vKeyword := UpperCase(AKeyword)
  else
    vKeyword := AKeyword;

  if AForward then  // 向前找
  begin
    vItemNo := Self.SelectInfo.StartItemNo;
    vOffset := Self.SelectInfo.StartItemOffset;
  end
  else  // 向后找
  begin
    if Self.SelectInfo.EndItemNo < 0 then
    begin
      vItemNo := Self.SelectInfo.StartItemNo;
      vOffset := Self.SelectInfo.StartItemOffset;
    end
    else
    begin
      vItemNo := Self.SelectInfo.EndItemNo;
      vOffset := Self.SelectInfo.EndItemOffset;
    end;
  end;

  Result := DoItemSearch(vItemNo, vOffset);

  if not Result then
  begin
    if AForward then  // 向前找
    begin
      for i := vItemNo - 1 downto 0 do
      begin
        if DoItemSearch(i, GetItemAfterOffset(i)) then
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
        if DoItemSearch(i, 0) then
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
        Self.SelectInfo.StartItemNo := Self.SelectInfo.EndItemNo;
        Self.SelectInfo.StartItemOffset := Self.SelectInfo.EndItemOffset;
      end;

      Self.SelectInfo.EndItemNo := -1;
      Self.SelectInfo.EndItemOffset := -1;
    end;
  end;

  Self.Style.UpdateInfoRePaint;
  Self.Style.UpdateInfoReCaret;
end;

procedure THCRichData.SelectItemAfterWithCaret(const AItemNo: Integer);
begin
  ReSetSelectAndCaret(AItemNo);
end;

procedure THCRichData.SelectLastItemAfterWithCaret;
begin
  SelectItemAfterWithCaret(Items.Count - 1);
end;

procedure THCRichData.SetSelectBound(const AStartNo, AStartOffset, AEndNo,
  AEndOffset: Integer);
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

  SelectInfo.StartItemNo := AStartNo;
  SelectInfo.StartItemOffset := AStartOffset;

  if (vEndNo < 0)
    or ((vEndNo = vStartNo) and (vEndOffset = vStartOffset))
  then
  begin
    SelectInfo.EndItemNo := -1;
    SelectInfo.EndItemOffset := -1;
  end
  else
  begin
    SelectInfo.EndItemNo := vEndNo;
    SelectInfo.EndItemOffset := vEndOffset;
  end;

  //FSelectSeekNo  如果需要确定 FSelectSeekNo，此方法得移动到CustomRichData
end;

procedure THCRichData.TraverseItem(const ATraverse: TItemTraverse);
var
  i: Integer;
begin
  if ATraverse <> nil then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if ATraverse.Stop then Break;

      ATraverse.Process(Self, i, ATraverse.Tag, ATraverse.Stop);
      if Items[i].StyleNo < THCStyle.RsNull then
        (Items[i] as THCCustomRectItem).TraverseItem(ATraverse);
    end;
  end;
end;

{ TDomain }

procedure TDomain.Clear;
begin
  FBeginNo := -1;
  FEndNo := -1;
end;

function TDomain.Contain(const AItemNo: Integer): Boolean;
begin
  Result := (AItemNo >= FBeginNo) and (AItemNo <= FEndNo);
end;

constructor TDomain.Create;
begin
  Clear;
end;

end.
