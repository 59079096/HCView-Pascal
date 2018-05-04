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
  Windows, Classes, Controls, Graphics, HCCustomData, HCCustomRichData, HCItem,
  HCStyle, HCParaStyle, HCTextStyle, HCRectItem, HCCommon, HCDataCommon;

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

  THCRichData = class(THCCustomRichData)  // 富文本数据类，可做为其他显示富文本类的基类
  private
    FHotDomain, FActiveDomain: TDomain;
    FHotDomainRGN, FActiveDomainRGN: HRGN;
    FDrawActiveDomainRegion, FDrawHotDomainRegion: Boolean;

    procedure GetDomainFrom(const AItemNo, AOffset: Integer;
      const ADomain: TDomain);
    function GetActiveDomain: TDomain;
  protected
    function CreateItemByStyle(const AStyleNo: Integer): THCCustomItem; override;
    function CreateDefaultDomainItem: THCCustomItem; override;
    function CreateDefaultTextItem: THCCustomItem; override;
    procedure PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure Clear; override;
    procedure Initialize; override;
    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: TCaretInfo); override;
    function CanDeleteItem(const AItemNo: Integer): Boolean; override;
    //
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const ADrawItemIndex: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    function InsertItem(const AItem: THCCustomItem): Boolean; override;
    function InsertItem(const AIndex: Integer; const AItem: THCCustomItem): Boolean; override;
  public
    constructor Create(const AStyle: THCStyle); override;
    destructor Destroy; override;

    /// <summary> 选到指定Item的最后面 </summary>
    procedure SelectItemAfter(const AItemNo: Integer);

    /// <summary> 选到最后一个Item的最后面 </summary>
    procedure SelectLastItemAfter;

    procedure GetCaretInfoCur(var ACaretInfo: TCaretInfo);
    procedure TraverseItem(const ATraverse: TItemTraverse);
    property HotDomain: TDomain read FHotDomain;
    property ActiveDomain: TDomain read GetActiveDomain;
    //property ShowHotDeGroupRegion: Boolean read FShowHotDeGroupRegion write FShowHotDeGroupRegion; 如果放开则处理201711281352
  end;

implementation

uses
  SysUtils, EmrElementItem, EmrGroupItem; {CreateDefaultTextItem，CreateDefaultDomainItem使用了Emr相关单元}

{ THCRichData }

function THCRichData.CanDeleteItem(const AItemNo: Integer): Boolean;
begin
  Result := Items[AItemNo].StyleNo <> THCStyle.RsDomain;
end;

procedure THCRichData.Clear;
begin
  inherited Clear;
  FHotDomain.Clear;
  FActiveDomain.Clear;
end;

constructor THCRichData.Create(const AStyle: THCStyle);
begin
  FHotDomain := TDomain.Create;
  FActiveDomain := TDomain.Create;
  inherited Create(AStyle);
end;

function THCRichData.CreateDefaultDomainItem: THCCustomItem;
begin
  Result := TDeGroup.Create;
  Result.ParaNo := Style.CurParaNo;
end;

function THCRichData.CreateDefaultTextItem: THCCustomItem;
begin
  Result := TEmrTextItem.CreateByText('');  // 必需有参数否则不能调用属性创建
  Result.StyleNo := Style.CurStyleNo;
  Result.ParaNo := Style.CurParaNo;
  if Assigned(OnCreateItem) then
    OnCreateItem(Result);
end;

function THCRichData.CreateItemByStyle(const AStyleNo: Integer): THCCustomItem;
begin
  // 自定义的类型在此处解析
  Result := inherited CreateItemByStyle(AStyleNo);
end;

destructor THCRichData.Destroy;
begin
  FHotDomain.Free;
  FActiveDomain.Free;
  inherited;
end;

procedure THCRichData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vEmrItem: TEmrTextItem;
  vDrawHotDomainBorde, vDrawActiveDomainBorde: Boolean;
  vItemNo: Integer;
  vDliRGN: HRGN;
begin
  inherited DoDrawItemPaintBefor(AData, ADrawItemIndex, ADrawRect, ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);;
  if not APaintInfo.Print then
  begin
    vDrawHotDomainBorde := False;
    vDrawActiveDomainBorde := False;
    vItemNo := DrawItems[ADrawItemIndex].ItemNo;

    if FHotDomain.BeginNo >= 0 then
      vDrawHotDomainBorde := FHotDomain.Contain(vItemNo);

    if FActiveDomain.BeginNo >= 0 then
      vDrawActiveDomainBorde := FActiveDomain.Contain(vItemNo);

    if vDrawHotDomainBorde or vDrawActiveDomainBorde then
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
      {vRect := ADrawRect;
      //InflateRect(vRect, 0, -GStyle.ParaStyles[GetDrawItemParaStyle(ADrawItemIndex)].LineSpaceHalf);
      ACanvas.Pen.Color := clGreen;
      ACanvas.Pen.Style := psSolid;
      ACanvas.MoveTo(vRect.Left, vRect.Top);
      ACanvas.LineTo(vRect.Right, vRect.Top);
      ACanvas.MoveTo(vRect.Left, vRect.Bottom);
      ACanvas.LineTo(vRect.Right, vRect.Bottom);}
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
  end;
end;

procedure THCRichData.Initialize;
begin
  inherited Initialize;
  FActiveDomain.Clear;
  FHotDomain.Clear;
end;

function THCRichData.InsertItem(const AIndex: Integer;
  const AItem: THCCustomItem): Boolean;
begin
  Result := inherited InsertItem(AIndex, AItem);
  if Result then
  begin
    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
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

  inherited MouseDown(Button, Shift, X, Y);
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

procedure THCRichData.SelectItemAfter(const AItemNo: Integer);
begin
  ReSetSelectAndCaret(AItemNo);
end;

procedure THCRichData.SelectLastItemAfter;
begin
  SelectItemAfter(Items.Count - 1);
end;

procedure THCRichData.TraverseItem(const ATraverse: TItemTraverse);
var
  i: Integer;
begin
  if ATraverse <> nil then
  begin
    for i := Items.Count - 1 downto 0 do  // 倒序遍历，便于删除
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
  Result := (AItemNo > FBeginNo) and (AItemNo < FEndNo);
end;

constructor TDomain.Create;
begin
  Clear;
end;

end.
