{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
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
  HCCommon, HCUndoRichData, HCList;

type
  THCDomainInfo = class(TObject)
  strict private
    FBeginNo, FEndNo: Integer;
  public
    constructor Create;
    procedure Clear;
    /// <summary> 域中是否包含此Item(头、尾也算) </summary>
    function Contain(const AItemNo: Integer): Boolean;
    property BeginNo: Integer read FBeginNo write FBeginNo;
    property EndNo: Integer read FEndNo write FEndNo;
  end;

  THCDataAnnotate = class(TSelectInfo)  // Data批注信息
  private
    FID: Integer;
  public
    procedure Initialize; override;
    procedure CopyRange(const ASrc: TSelectInfo);
    property ID: Integer read FID write FID;
  end;

  THCDataAnnotates = class(TObjectList<THCDataAnnotate>)
  public
    function NewAnnotate: THCDataAnnotate;
  end;

  TStyleItemEvent = function (const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem of object;
  TOnCanEditEvent = function(const Sender: TObject): Boolean of object;
  TDataItemNotifyEvent = procedure(const AData: THCCustomData; const AItem: THCCustomItem) of object;
  TDataAnnotateDrawItemEvent = procedure(const AData: THCCustomData; const ADrawItemNo: Integer; const ADrawRect: TRect) of object;
  TDrawItemPaintContentEvent = procedure(const AData: THCCustomData;
    const ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect; const ADrawText: string;
    const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
    const ACanvas: TCanvas; const APaintInfo: TPaintInfo) of object;

  THCRichData = class(THCUndoRichData)  // 富文本数据类，可做为其他显示富文本类的基类
  private
    FDomainStartDeletes: THCIntegerList;  // 仅用于选中删除时，当域起始结束都选中时，删除了结束后标明起始的可删除
    FHotDomain,  // 当前高亮域
    FActiveDomain  // 当前激活域
      : THCDomainInfo;

    FAnnotates: THCDataAnnotates;
    FHotAnnotate,  // 当前高亮批注
    FActiveAnnotate:  // 当前激活的批注
      Integer;

    FHotDomainRGN, FActiveDomainRGN: HRGN;
    FDrawActiveDomainRegion, FDrawHotDomainRegion: Boolean;  // 是否绘制域边框
    FOnCreateItemByStyle: TStyleItemEvent;
    FOnCanEdit: TOnCanEditEvent;
    FOnInsertItem, FOnRemoveItem: TDataItemNotifyEvent;
    FOnAnnotateDrawItem: TDataAnnotateDrawItemEvent;
    FOnDrawItemPaintContent: TDrawItemPaintContentEvent;

    procedure GetDomainFrom(const AItemNo, AOffset: Integer;
      const ADomainInfo: THCDomainInfo);
    procedure DoInsertItem(const AItem: THCCustomItem);
    procedure DoRemoveItem(const AItem: THCCustomItem);
  protected
    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; override;
    function CanDeleteItem(const AItemNo: Integer): Boolean; override;

    /// <summary> 用于从流加载完Items后，检查不合格的Item并删除 </summary>
    function CheckInsertItemCount(const AStartNo, AEndNo: Integer): Integer; override;

    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure DoDrawItemPaintContent(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADrawText: string;
      const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    procedure DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem); virtual;
    procedure DoDataRemoveItem(const AData: THCCustomData; const AItem: THCCustomItem); virtual;
  public
    constructor Create(const AStyle: THCStyle); override;
    destructor Destroy; override;

    function CreateDefaultDomainItem: THCCustomItem; override;
    function CreateDefaultTextItem: THCCustomItem; override;
    procedure PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure InitializeField; override;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: THCCaretInfo); override;
    function DeleteSelected: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function InsertItem(const AItem: THCCustomItem): Boolean; override;
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem;
      const AOffsetBefor: Boolean = True): Boolean; override;
    function CanEdit: Boolean; override;

    procedure CheckAnnotate(const AHorzOffset, AVertOffset, AFirstDrawItemNo, vLastDrawItemNo,
      AFmtTop, AFmtBottom: Integer);

    /// <summary> 根据传入的域"模具"创建域 </summary>
    /// <param name="AMouldDomain">"模具"调用完此方法后请自行释放</param>
    function InsertDomain(const AMouldDomain: THCDomainItem): Boolean;

    function InsertAnnotate(const ATitle, AText: string): Boolean;

    /// <summary> 设置选中范围，仅供外部使用内部不使用 </summary>
    procedure SetSelectBound(const AStartNo, AStartOffset, AEndNo, AEndOffset: Integer);

    /// <summary> 光标选到指定Item的最后面 </summary>
    procedure SelectItemAfterWithCaret(const AItemNo: Integer);

    /// <summary> 光村选到最后一个Item的最后面 </summary>
    procedure SelectLastItemAfterWithCaret;

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
    procedure TraverseItem(const ATraverse: TItemTraverse);

    property HotDomain: THCDomainInfo read FHotDomain;
    property ActiveDomain: THCDomainInfo read FActiveDomain;
    property HotAnnotate: Integer read FHotAnnotate;
    property ActiveAnnotate: Integer read FActiveAnnotate;
    property OnCreateItemByStyle: TStyleItemEvent read FOnCreateItemByStyle write FOnCreateItemByStyle;
    property OnCanEdit: TOnCanEditEvent read FOnCanEdit write FOnCanEdit;
    property OnInsertItem: TDataItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TDataItemNotifyEvent read FOnRemoveItem write FOnRemoveItem;
    property OnDrawItemPaintContent: TDrawItemPaintContentEvent read FOnDrawItemPaintContent write FOnDrawItemPaintContent;
    property OnAnnotateDrawItem: TDataAnnotateDrawItemEvent read FOnAnnotateDrawItem write FOnAnnotateDrawItem;
  end;

implementation

uses
  StrUtils, Math;

{ THCRichData }

function THCRichData.CanDeleteItem(const AItemNo: Integer): Boolean;
var
  vItemNo: Integer;
begin
  Result := inherited CanDeleteItem(AItemNo);
  if Result then
  begin
    if Items[AItemNo].StyleNo = THCStyle.Domain then  // 是域标识
    begin
      if (Items[AItemNo] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 域结束标识
      begin
        vItemNo := GetDomainAnother(AItemNo);  // 找起始
        Result := (vItemNo >= SelectInfo.StartItemNo) and (vItemNo <= SelectInfo.EndItemNo);
        if Result then  // 起始也在选中删除范围内
          FDomainStartDeletes.Add(vItemNo);  // 记录下来
      end
      else  // 域起始标记
        Result := FDomainStartDeletes.IndexOf(AItemNo) >= 0;  // 结束标识已经被标记为可删除
    end;
  end;
end;

function THCRichData.CanEdit: Boolean;
begin
  Result := inherited CanEdit;
  if Result and Assigned(FOnCanEdit) then
    Result := FOnCanEdit(Self);
end;

procedure THCRichData.CheckAnnotate(const AHorzOffset, AVertOffset,
  AFirstDrawItemNo, vLastDrawItemNo, AFmtTop, AFmtBottom: Integer);
var
  i, vLineSpace: Integer;
  vRectItem: THCCustomRectItem;
  vDrawRect: TRect;
  vAlignHorz: TParaAlignHorz;
begin
  if Assigned(FOnAnnotateDrawItem) then
  begin
    for i := AFirstDrawItemNo to vLastDrawItemNo do
    begin
      vDrawRect := DrawItems[i].Rect;
      if vDrawRect.Top > AFmtBottom then
        Break;

      if GetDrawItemStyle(i) < THCStyle.Null then
      begin
        vRectItem := Items[DrawItems[i].ItemNo] as THCCustomRectItem;

        vLineSpace := GetLineSpace(i);
        InflateRect(vDrawRect, 0, -vLineSpace div 2);  // 除去行间距净Rect，即内容的显示区域

        if vRectItem.JustifySplit then  // 分散占空间
        begin
          vAlignHorz := Style.ParaStyles[vRectItem.ParaNo].AlignHorz;
          if ((vAlignHorz = pahJustify) and (not IsLineLastDrawItem(i)))  // 两端对齐且不是段最后
            or (vAlignHorz = pahScatter)  // 分散对齐
          then
            vDrawRect.Inflate(-(vDrawRect.Width - vRectItem.Width) div 2, 0)
          else
            vDrawRect.Right := vDrawRect.Left + vRectItem.Width;
        end;

        case Style.ParaStyles[vRectItem.ParaNo].AlignVert of  // 垂直对齐方式
          pavCenter: InflateRect(vDrawRect, 0, -(vDrawRect.Height - vRectItem.Height) div 2);
          pavTop: ;
        else
          vDrawRect.Top := vDrawRect.Bottom - vRectItem.Height;
        end;

        vRectItem.CheckAnnotate(vDrawRect.Left + AHorzOffset, vDrawRect.Top + AVertOffset,
          Min(vRectItem.Height, AFmtBottom - vDrawRect.Top));
      end
//      else
//      if DrawItems[i].Rect.Bottom > AFmtTop then
//      begin
//        if GetDrawItemText(i) = '111' then
//        begin
//          vDrawRect.Offset(AHorzOffset, AVertOffset);
//          FOnAnnotateDrawItem(Self, i, vDrawRect);
//        end;
//      end;
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

constructor THCRichData.Create(const AStyle: THCStyle);
begin
  FDomainStartDeletes := THCIntegerList.Create;
  FHotDomain := THCDomainInfo.Create;
  FActiveDomain := THCDomainInfo.Create;
  inherited Create(AStyle);
  FAnnotates := THCDataAnnotates.Create;
  FHotAnnotate := -1;
  FActiveAnnotate := -1;
  Self.Items.OnInsertItem := DoInsertItem;
  Self.Items.OnRemoveItem := DoRemoveItem;
end;

function THCRichData.CreateDefaultDomainItem: THCCustomItem;
begin
  Result := inherited CreateDefaultDomainItem;
  if Assigned(OnCreateItem) then
    OnCreateItem(Result);
end;

function THCRichData.CreateDefaultTextItem: THCCustomItem;
begin
  Result := inherited CreateDefaultTextItem;
  if Assigned(OnCreateItem) then
    OnCreateItem(Result);
end;

function THCRichData.CreateItemByStyle(const AStyleNo: Integer): THCCustomItem;
begin
  Result := nil;

  if Assigned(FOnCreateItemByStyle) then  // 自定义的类型在此处解析
    Result := FOnCreateItemByStyle(Self, AStyleNo);

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
  FAnnotates.Free;
  inherited Destroy;
end;

procedure THCRichData.DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnInsertItem) then
    FOnInsertItem(AData, AItem);
end;

procedure THCRichData.DoDataRemoveItem(const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnRemoveItem) then
    FOnRemoveItem(AData, AItem);
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
    if AData.Style.ShowLineLastMark then  // 显示段尾的换行符
    begin
      if (ADrawItemNo < DrawItems.Count - 1) and DrawItems[ADrawItemNo + 1].ParaFirst then
        DrawLineLastMrak(ADrawRect)  // 段尾的换行符
      else
      if ADrawItemNo = DrawItems.Count - 1 then
        DrawLineLastMrak(ADrawRect);  // 段尾的换行符
    end;
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

  if not APaintInfo.Print then  // 拼接域范围
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

procedure THCRichData.DoDrawItemPaintContent(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  i, vItemNo: Integer;
  //vDrawItem: THCCustomDrawItem;
  vAnnotate: THCDataAnnotate;
begin
//  if ADrawText = '111' then
//   begin
//     ACanvas.Brush.Color := AnnotateBKColor;
//     ACanvas.FillRect(AClearRect);
//   end;
  {vDrawItem := AData.DrawItems[ADrawItemNo];
  vItemNo := vDrawItem.ItemNo;
  for i := 0 to FAnnotates.Count - 1 do
  begin
    vAnnotate := FAnnotates[i];
    if vItemNo = vAnnotate.StartItemNo then
    begin
      //if vDrawItem.CharOffsetEnd then

      Break;
    end
    else
    if vItemNo = vAnnotate.EndItemNo then
    begin
      Break;
    end
    else
    if (vItemNo > vAnnotate.StartItemNo) and (vItemNo < vAnnotate.EndItemNo) then
    begin
      ACanvas.Brush.Color := AnnotateBKColor;
      ACanvas.FillRect(AClearRect);
      Break;
    end;
  end;}

  inherited DoDrawItemPaintContent(AData, ADrawItemNo, ADrawRect, AClearRect,
    ADrawText, ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom,
    ACanvas, APaintInfo);

  if Assigned(FOnDrawItemPaintContent) then
  begin
    FOnDrawItemPaintContent(AData, ADrawItemNo, ADrawRect, AClearRect, ADrawText,
      ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  end;
end;

procedure THCRichData.DoInsertItem(const AItem: THCCustomItem);
begin
  DoDataInsertItem(Self, AItem);
end;

procedure THCRichData.DoRemoveItem(const AItem: THCCustomItem);
begin
  DoDataRemoveItem(Self, AItem);
end;

function THCRichData.GetDomainAnother(const AItemNo: Integer): Integer;
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

procedure THCRichData.GetDomainFrom(const AItemNo, AOffset: Integer;
  const ADomainInfo: THCDomainInfo);
var
  i, vStartNo, vEndNo, vCount: Integer;
  vLevel: Byte;
begin
  ADomainInfo.Clear;

  if (AItemNo < 0) or (AOffset < 0) then Exit;

  { 找起始标识 }
  vStartNo := AItemNo;
  vEndNo := AItemNo;
  if Items[AItemNo]is THCDomainItem then  // 起始位置就是Group
  begin
    if (Items[AItemNo] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 起始位置是起始标记
    begin
      if AOffset = OffsetAfter then  // 光标在后面
      begin
        ADomainInfo.BeginNo := AItemNo;  // 当前即为起始标识
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

  if ADomainInfo.BeginNo < 0 then
  begin
    vCount := 0;

    if vStartNo < Self.Items.Count div 2 then  // 在前半程
    begin
      for i := vStartNo downto 0 do  // 先往前找起始
      begin
        if Items[i] is THCDomainItem then
        begin
          if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 起始标记
          begin
            if vCount > 0 then
              Dec(vCount)
            else
            begin
              ADomainInfo.BeginNo := i;
              vLevel := (Items[i] as THCDomainItem).Level;
              Break;
            end;
          end
          else
            Inc(vCount);
        end;
      end;

      if (ADomainInfo.BeginNo >= 0) and (ADomainInfo.EndNo < 0) then  // 找结束标识
      begin
        for i := vEndNo to Items.Count - 1 do
        begin
          if Items[i] is THCDomainItem then
          begin
            if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 是结尾
            begin
              if (Items[i] as THCDomainItem).Level = vLevel then
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
        if Items[i] is THCDomainItem then
        begin
          if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtEnd then  // 结束标记
          begin
            if vCount > 0 then
              Dec(vCount)
            else
            begin
              ADomainInfo.EndNo := i;
              vLevel := (Items[i] as THCDomainItem).Level;
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
          if Items[i] is THCDomainItem then
          begin
            if (Items[i] as THCDomainItem).MarkType = TMarkType.cmtBeg then  // 是起始
            begin
              if (Items[i] as THCDomainItem).Level = vLevel then
              begin
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
  end;
end;

procedure THCRichData.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: THCCaretInfo);
var
  vTopData: THCCustomRichData;
begin
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

      GetDomainFrom(SelectInfo.StartItemNo, SelectInfo.StartItemOffset, FActiveDomain);  // 获取当前光标处ActiveDeGroup信息

      if FActiveDomain.BeginNo >= 0 then
      begin
        FDrawActiveDomainRegion := True;
        Style.UpdateInfoRePaint;
      end;

      if FActiveAnnotate >= 0 then  // 原来有激活的批注
      begin
        FActiveAnnotate := -1;
        Style.UpdateInfoRePaint;
      end;

      //GetDomainFrom(SelectInfo.StartItemNo, SelectInfo.StartItemOffset,
      //  THCStyle.Annotate, FActiveAnnotate);  // 获取当前光标处批注
      if FActiveAnnotate >= 0 then
        Style.UpdateInfoRePaint;
    end;
  end;
end;

procedure THCRichData.GetCaretInfoCur(var ACaretInfo: THCCaretInfo);
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
  FHotDomain.Clear;
  FActiveDomain.Clear;
  FHotAnnotate := -1;
  FActiveAnnotate := -1;
end;

function THCRichData.InsertDomain(const AMouldDomain: THCDomainItem): Boolean;
var
  vDomainItem: THCDomainItem;
begin
  Result := False;
  if not CanEdit then Exit;

  Undo_GroupBegin(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  try
    // 插入头
    vDomainItem := CreateDefaultDomainItem as THCDomainItem;
    if Assigned(AMouldDomain) then
      vDomainItem.Assign(AMouldDomain);
    vDomainItem.MarkType := cmtBeg;
    if FActiveDomain.BeginNo >= 0 then
      vDomainItem.Level := (Items[FActiveDomain.BeginNo] as THCDomainItem).Level + 1;

    Result := InsertItem(vDomainItem);

    if Result then // 插入尾
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
    Undo_GroupEnd(SelectInfo.StartItemNo, SelectInfo.StartItemOffset);
  end;
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

function THCRichData.InsertAnnotate(const ATitle, AText: string): Boolean;
var
  vBreakItem: THCCustomItem;
  //vAnnotateItem: THCAnnotateItem;
  vSelectStartNo, vSelectStartOffset,
  vSelectEndNo, vSelectEndOffset,
  vFormatFirstItemNo, vFormatLastItemNo, vExtraCount: Integer;
  vAnnotate: THCDataAnnotate;
begin
  Result := False;
  if not CanEdit then Exit;
  if not Self.SelectExists then Exit;

  vAnnotate := FAnnotates.NewAnnotate;
  vAnnotate.CopyRange(SelectInfo);

  Exit;
//  vExtraCount := 0;
//  GetReformatItemRange(vFormatFirstItemNo, vFormatLastItemNo);
//  if vFormatLastItemNo < SelectInfo.EndItemNo then
//     vFormatLastItemNo := GetParaLastItemNo(SelectInfo.EndItemNo);
//  _FormatItemPrepare(vFormatFirstItemNo, vFormatLastItemNo);
//
//  vSelectStartNo := SelectInfo.StartItemNo;
//  vSelectStartOffset := SelectInfo.StartItemOffset;
//  vSelectEndNo := SelectInfo.EndItemNo;
//  vSelectEndOffset := SelectInfo.EndItemOffset;
//
//  Self.DisSelect;
//  Self.InitializeField;
//
//  if vSelectEndOffset < Items[vSelectEndNo].Length then  // 在结束Item中间，用判断在Item最前面吗？
//  begin
//    //UndoAction_DeleteText(vCurItemNo, SelectInfo.StartItemOffset + 1, vsAfter);
//    vBreakItem := Items[vSelectEndNo].BreakByOffset(vSelectEndOffset);  // 后半部分对应的Item
//
//    // 插入后半部分对应的Item
//    Items.Insert(vSelectEndNo + 1, vBreakItem);
//    Inc(vExtraCount);
//    //UndoAction_InsertItem(SelectInfo.EndItemNo, 0);
//
//    vAnnotateItem := THCAnnotateItem.Create(Self);  // 插入批注尾
//    vAnnotateItem.MarkType := cmtEnd;
//    if FActiveAnnotate.BeginNo >= 0 then
//      vAnnotateItem.Level := (Items[FActiveAnnotate.BeginNo] as THCAnnotateItem).Level + 1;
//    vAnnotateItem.Title := ATitle;
//    vAnnotateItem.Text := AText;
//    Items.Insert(vSelectEndNo + 1, vAnnotateItem);
//    Inc(vExtraCount);
//    //UndoAction_InsertItem(SelectInfo.EndItemNo, 0);
//  end
//  else  // 在结束Item最后面
//  begin
//    vAnnotateItem := THCAnnotateItem.Create(Self);  // 插入批注尾
//    vAnnotateItem.MarkType := cmtEnd;
//    if FActiveAnnotate.BeginNo >= 0 then
//      vAnnotateItem.Level := (Items[FActiveAnnotate.BeginNo] as THCAnnotateItem).Level + 1;
//    vAnnotateItem.Title := ATitle;
//    vAnnotateItem.Text := AText;
//    Items.Insert(vSelectEndNo + 1, vAnnotateItem);
//    Inc(vExtraCount);
//  end;
//
//  if vSelectStartOffset > 0 then  // 在起始Item中间，用判断在Item最后面吗？
//  begin
//    //UndoAction_DeleteText(vCurItemNo, SelectInfo.StartItemOffset + 1, vsAfter);
//    vBreakItem := Items[vSelectStartNo].BreakByOffset(vSelectStartOffset);  // 后半部分对应的Item
//
//    // 插入后半部分对应的Item
//    Items.Insert(vSelectStartNo + 1, vBreakItem);
//    Inc(vExtraCount);
//    //UndoAction_InsertItem(SelectInfo.EndItemNo, 0);
//
//    vAnnotateItem := THCAnnotateItem.Create(Self);  // 插入批注头
//    vAnnotateItem.MarkType := cmtBeg;
//    vAnnotateItem.Title := ATitle;
//    vAnnotateItem.Text := AText;
//    Items.Insert(vSelectStartNo + 1, vAnnotateItem);
//    Inc(vExtraCount);
//    //UndoAction_InsertItem(SelectInfo.EndItemNo, 0);
//  end
//  else  // 在起始Item最前面
//  begin
//    vAnnotateItem := THCAnnotateItem.Create(Self);  // 插入批注头
//    vAnnotateItem.MarkType := cmtBeg;
//    vAnnotateItem.Title := ATitle;
//    vAnnotateItem.Text := AText;
//    Items.Insert(vSelectStartNo, vAnnotateItem);
//    Inc(vExtraCount);
//  end;
//
//  _ReFormatData(vFormatFirstItemNo, vFormatLastItemNo + vExtraCount, vExtraCount);
//  ReSetSelectAndCaret(vSelectStartNo);
end;

procedure THCRichData.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  // 清除激活的Group信息，赋值在 GetCaretInfo
  if FActiveDomain.BeginNo >= 0 then
    Style.UpdateInfoRePaint;
  FActiveDomain.Clear;
  FDrawActiveDomainRegion := False;

  inherited MouseDown(Button, Shift, X, Y);

  if Button = TMouseButton.mbRight then  // 右键菜单时，重新取光标处FActiveDomain
    Style.UpdateInfoReCaret;
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
  FHotAnnotate := -1;

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

//    GetDomainFrom(Self.MouseMoveItemNo, Self.MouseMoveItemOffset,
//      THCStyle.Annotate, FHotAnnotate);
    if FHotAnnotate >= 0 then
      Style.UpdateInfoRePaint;
  end;
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

function THCRichData.Replace(const AText: string): Boolean;
begin
//  DeleteSelected;
  InsertText(AText);
end;

function THCRichData.Search(const AKeyword: string; const AForward,
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

  if AForward then  // 向前找，起始位置向前
  begin
    vItemNo := Self.SelectInfo.StartItemNo;
    vOffset := Self.SelectInfo.StartItemOffset;
  end
  else  // 向后找
  begin
    if Self.SelectInfo.EndItemNo < 0 then  // 有选中结束，从选中结束往后
    begin
      vItemNo := Self.SelectInfo.StartItemNo;
      vOffset := Self.SelectInfo.StartItemOffset;
    end
    else  // 没有选中结束，从选中起始往后
    begin
      vItemNo := Self.SelectInfo.EndItemNo;
      vOffset := Self.SelectInfo.EndItemOffset;
    end;
  end;

  Result := DoSearchByOffset(vItemNo, vOffset);

  if not Result then
  begin
    if AForward then  // 向前找
    begin
      for i := vItemNo - 1 downto 0 do
      begin
        if DoSearchByOffset(i, GetItemAfterOffset(i)) then
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
      if Items[i].StyleNo < THCStyle.Null then
        (Items[i] as THCCustomRectItem).TraverseItem(ATraverse);
    end;
  end;
end;

{ THCDomainInfo }

procedure THCDomainInfo.Clear;
begin
  FBeginNo := -1;
  FEndNo := -1;
end;

function THCDomainInfo.Contain(const AItemNo: Integer): Boolean;
begin
  Result := (AItemNo >= FBeginNo) and (AItemNo <= FEndNo);
end;

constructor THCDomainInfo.Create;
begin
  Clear;
end;

{ THCDataAnnotate }

procedure THCDataAnnotate.CopyRange(const ASrc: TSelectInfo);
begin
  Self.StartItemNo := ASrc.StartItemNo;
  Self.StartItemOffset := ASrc.StartItemOffset;
  Self.EndItemNo := ASrc.EndItemNo;
  Self.EndItemOffset := ASrc.EndItemOffset;
end;

procedure THCDataAnnotate.Initialize;
begin
  inherited Initialize;
  FID := -1;
end;

{ THCDataAnnotates }

function THCDataAnnotates.NewAnnotate: THCDataAnnotate;
begin
  Result := THCDataAnnotate.Create;
  Result.ID := Self.Add(Result);
end;

end.
