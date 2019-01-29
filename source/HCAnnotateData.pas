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
  HCRichData, HCItem, HCStyle, HCParaStyle, HCTextStyle, HCTextItem, HCRectItem,
  HCCommon, HCList;

type
  THCDataAnnotate = class(TSelectInfo)  // Data批注信息
  private
    FID, FStartDrawItemNo, FEndDrawItemNo: Integer;
    FTitle, FText: string;
  public
    procedure Initialize; override;
    procedure CopyRange(const ASrc: TSelectInfo);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
    property ID: Integer read FID write FID;
    property StartDrawItemNo: Integer read FStartDrawItemNo write FStartDrawItemNo;
    property EndDrawItemNo: Integer read FEndDrawItemNo write FEndDrawItemNo;
    property Title: string read FTitle write FTitle;
    property Text: string read FText write FText;
  end;

  THCDataAnnotates = class(TObjectList<THCDataAnnotate>)
  private
    FOnInsertAnnotate, FOnRemoveAnnotate: TNotifyEvent;
  protected
    procedure Notify(const Value: THCDataAnnotate; Action: TCollectionNotification); override;
  public
    procedure DeleteByID(const AID: Integer);
    procedure NewDataAnnotate(const ASelectInfo: TSelectInfo; const ATitle, AText: string);
    property OnInsertAnnotate: TNotifyEvent read FOnInsertAnnotate write FOnInsertAnnotate;
    property OnRemoveAnnotate: TNotifyEvent read FOnRemoveAnnotate write FOnRemoveAnnotate;
  end;

  THCAnnotateMark = (amFirst, amNormal, amLast, amBoth);
  THCDrawItemAnnotate = class(TObject)  // DrawItem注绘制时对应的批信息
  public
    DrawRect: TRect;
    Mark: THCAnnotateMark;
    DataAnnotate: THCDataAnnotate;

    function First: Boolean;
    function Last: Boolean;
  end;

  THCDrawItemAnnotates = class(TObjectList<THCDrawItemAnnotate>)  // 某DrawItem对应的所有批注信息
  public
    procedure NewDrawAnnotate(const ARect: TRect; const AMark: THCAnnotateMark;
      const ADataAnnotate: THCDataAnnotate);
  end;

  TDataDrawItemAnnotateEvent = procedure(const AData: THCCustomData; const ADrawItemNo: Integer;
    const ADrawRect: TRect; const ADataAnnotate: THCDataAnnotate) of object;
  TDataAnnotateEvent = procedure(const AData: THCCustomData; const ADataAnnotate: THCDataAnnotate) of object;
  TDataItemNotifyEvent = procedure(const AData: THCCustomData; const AItem: THCCustomItem) of object;

  THCAnnotateData = class(THCRichData)  // 支持批注功能的Data类
  private
    FDataAnnotates: THCDataAnnotates;
    FHotAnnotate, FActiveAnnotate: THCDataAnnotate;  // 当前高亮批注、当前激活的批注
    FDrawItemAnnotates: THCDrawItemAnnotates;
    FOnDrawItemAnnotate: TDataDrawItemAnnotateEvent;
    FOnInsertAnnotate, FOnRemoveAnnotate: TDataAnnotateEvent;
    FOnInsertItem, FOnRemoveItem: TDataItemNotifyEvent;

    procedure DoInsertItem(const AItem: THCCustomItem);
    procedure DoRemoveItem(const AItem: THCCustomItem);

    /// <summary> 获取指定的DrawItem所属的批注以及在各批注中的区域 </summary>
    /// <param name="ADrawItemNo"></param>
    /// <param name="ACanvas">应用了DrawItem样式的Canvas</param>
    /// <returns></returns>
    function DrawItemOfAnnotate(const ADrawItemNo: Integer;
      const ACanvas: TCanvas; const ADrawRect: TRect): Boolean;

    /// <summary> 指定DrawItem范围内的批注获取各自的DrawItem范围 </summary>
    /// <param name="AFirstDrawItemNo">起始DrawItem</param>
    /// <param name="ALastDrawItemNo">结束DrawItem</param>
    procedure CheckAnnotateRange(const AFirstDrawItemNo, ALastDrawItemNo: Integer);  // 现在在PaintData里处理的，应该在内容变动后就处理好，这样会加快PaintData的效率

    function GetDrawItemFirstDataAnnotateAt(const ADrawItemNo, X, Y: Integer): THCDataAnnotate;
  protected
    procedure DoDataInsertItem(const AData: THCCustomData; const AItem: THCCustomItem); virtual;
    procedure DoDataRemoveItem(const AData: THCCustomData; const AItem: THCCustomItem); virtual;
    procedure DoItemAction(const AItemNo, AOffset: Integer; const AAction: THCItemAction); override;
    procedure DoDrawItemPaintContent(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect, AClearRect: TRect; const ADrawText: string;
      const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure DoInsertAnnotate(Sender: TObject);
    procedure DoRemoveAnnotate(Sender: TObject);
  public
    constructor Create(const AStyle: THCStyle); override;
    destructor Destroy; override;

    procedure GetCaretInfo(const AItemNo, AOffset: Integer; var ACaretInfo: THCCaretInfo); override;
    procedure PaintData(const ADataDrawLeft, ADataDrawTop, ADataDrawBottom,
      ADataScreenTop, ADataScreenBottom, AVOffset, AFirstDItemNo, ALastDItemNo: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure InitializeField; override;
    procedure Clear; override;
    procedure SaveToStream(const AStream: TStream; const AStartItemNo, AStartOffset,
      AEndItemNo, AEndOffset: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;

    function InsertAnnotate(const ATitle, AText: string): Boolean;
    property DataAnnotates: THCDataAnnotates read FDataAnnotates;
    property HotAnnotate: THCDataAnnotate read FHotAnnotate;
    property ActiveAnnotate: THCDataAnnotate read FActiveAnnotate;
    property OnDrawItemAnnotate: TDataDrawItemAnnotateEvent read FOnDrawItemAnnotate write FOnDrawItemAnnotate;
    property OnInsertAnnotate: TDataAnnotateEvent read FOnInsertAnnotate write FOnInsertAnnotate;
    property OnRemoveAnnotate: TDataAnnotateEvent read FOnRemoveAnnotate write FOnRemoveAnnotate;
    property OnInsertItem: TDataItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TDataItemNotifyEvent read FOnRemoveItem write FOnRemoveItem;
  end;

implementation

{ THCAnnotateData }

procedure THCAnnotateData.CheckAnnotateRange(const AFirstDrawItemNo, ALastDrawItemNo: Integer);
var
  i, vFirstNo, vLastNo: Integer;
  vDataAnnotate: THCDataAnnotate;
  vDrawRect: TRect;
  vRectItem: THCCustomRectItem;
begin
  if AFirstDrawItemNo < 0 then Exit;

  vFirstNo := Self.DrawItems[AFirstDrawItemNo].ItemNo;
  vLastNo := Self.DrawItems[ALastDrawItemNo].ItemNo;

  for i := 0 to FDataAnnotates.Count - 1 do
  begin
    vDataAnnotate := FDataAnnotates[i];

    if vDataAnnotate.EndItemNo < vFirstNo then  // 未进入本次查找范围
      Continue;

    if vDataAnnotate.StartItemNo > vLastNo then  // 超出本次查找的范围
      Break;

    vDataAnnotate.StartDrawItemNo :=
      Self.GetDrawItemNoByOffset(vDataAnnotate.StartItemNo, vDataAnnotate.StartItemOffset);
    vDataAnnotate.EndDrawItemNo :=
      Self.GetDrawItemNoByOffset(vDataAnnotate.EndItemNo, vDataAnnotate.EndItemOffset);
    if vDataAnnotate.EndItemOffset = Self.DrawItems[vDataAnnotate.EndDrawItemNo].CharOffs then  // 如果在结束的最前面，按上一个
      vDataAnnotate.EndDrawItemNo := vDataAnnotate.EndDrawItemNo - 1;
  end;

  {for i := AFirstDrawItemNo to ALastDrawItemNo do
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
    else
    if DrawItems[i].Rect.Bottom > AFmtTop then  // DrawItem格式化区域在要判断的格式化区域内
    begin
      if DrawItemBelongAnnotate(i, vDrawRect) then
      begin
        vDrawRect.Offset(AHorzOffset, AVertOffset);
        FOnAnnotateDrawItem(Self, i, vDrawRect);
      end;
    end;
  end; }
end;

procedure THCAnnotateData.Clear;
begin
  FDataAnnotates.Clear;
  inherited Clear;
end;

constructor THCAnnotateData.Create(const AStyle: THCStyle);
begin
  FDataAnnotates := THCDataAnnotates.Create;
  FDataAnnotates.OnInsertAnnotate := DoInsertAnnotate;
  FDataAnnotates.OnRemoveAnnotate := DoRemoveAnnotate;
  FDrawItemAnnotates := THCDrawItemAnnotates.Create;
  inherited Create(AStyle);
  FHotAnnotate := nil;
  FActiveAnnotate := nil;
  Self.Items.OnInsertItem := DoInsertItem;
  Self.Items.OnRemoveItem := DoRemoveItem;
end;

destructor THCAnnotateData.Destroy;
begin
  FDataAnnotates.Free;
  FDrawItemAnnotates.Free;
  inherited Destroy;
end;

procedure THCAnnotateData.DoDataInsertItem(const AData: THCCustomData;
  const AItem: THCCustomItem);
begin
  if Assigned(FOnInsertItem) then
    FOnInsertItem(AData, AItem);
end;

procedure THCAnnotateData.DoDataRemoveItem(const AData: THCCustomData;
  const AItem: THCCustomItem);
begin
  if Assigned(FOnRemoveItem) then
    FOnRemoveItem(AData, AItem);
end;

procedure THCAnnotateData.DoDrawItemPaintContent(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  i: Integer;
  vActive: Boolean;
  vDrawAnnotate: THCDrawItemAnnotate;
begin
  if Assigned(FOnDrawItemAnnotate) and DrawItemOfAnnotate(ADrawItemNo, ACanvas, AClearRect) then  // 当前DrawItem是某批注中的一部分
  begin
    for i := 0 to FDrawItemAnnotates.Count - 1 do  // 此DrawItem所有批注信息
    begin
      vDrawAnnotate := FDrawItemAnnotates[i];

      if not APaintInfo.Print then
      begin
        vActive := vDrawAnnotate.DataAnnotate.Equals(FHotAnnotate) or
          vDrawAnnotate.DataAnnotate.Equals(FActiveAnnotate);

        if vActive then
          ACanvas.Brush.Color := AnnotateBKActiveColor
        else
          ACanvas.Brush.Color := AnnotateBKColor;

        ACanvas.FillRect(vDrawAnnotate.DrawRect);
      end;

      if vDrawAnnotate.First then  // 是批注头 [
      begin
        ACanvas.Pen.Color := clRed;
        ACanvas.MoveTo(vDrawAnnotate.DrawRect.Left + 2, vDrawAnnotate.DrawRect.Top - 2);
        ACanvas.LineTo(vDrawAnnotate.DrawRect.Left, vDrawAnnotate.DrawRect.Top);
        ACanvas.LineTo(vDrawAnnotate.DrawRect.Left, vDrawAnnotate.DrawRect.Bottom);
        ACanvas.LineTo(vDrawAnnotate.DrawRect.Left + 2, vDrawAnnotate.DrawRect.Bottom + 2);
      end;

      if vDrawAnnotate.Last then  // 是批注尾 ]
      begin
        ACanvas.Pen.Color := clRed;
        ACanvas.MoveTo(vDrawAnnotate.DrawRect.Right - 2, vDrawAnnotate.DrawRect.Top - 2);
        ACanvas.LineTo(vDrawAnnotate.DrawRect.Right, vDrawAnnotate.DrawRect.Top);
        ACanvas.LineTo(vDrawAnnotate.DrawRect.Right, vDrawAnnotate.DrawRect.Bottom);
        ACanvas.LineTo(vDrawAnnotate.DrawRect.Right - 2, vDrawAnnotate.DrawRect.Bottom + 2);

        FOnDrawItemAnnotate(AData, ADrawItemNo, vDrawAnnotate.DrawRect, vDrawAnnotate.DataAnnotate);
      end;
    end;
  end;

  inherited DoDrawItemPaintContent(AData, ADrawItemNo, ADrawRect, AClearRect,
    ADrawText, ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom,
    ACanvas, APaintInfo);
end;

procedure THCAnnotateData.DoInsertAnnotate(Sender: TObject);
begin
  Style.UpdateInfoRePaint;
  if Assigned(FOnInsertAnnotate) then
    FOnInsertAnnotate(Self, THCDataAnnotate(Sender));
end;

procedure THCAnnotateData.DoInsertItem(const AItem: THCCustomItem);
begin
  DoDataInsertItem(Self, AItem);
end;

procedure THCAnnotateData.DoItemAction(const AItemNo, AOffset: Integer;
  const AAction: THCItemAction);

  procedure _AnnotateRemove;
  begin

  end;

  {$REGION '插入字符'}
  procedure _AnnotateInsertChar;
  var
    i: Integer;
    vDataAnn: THCDataAnnotate;
  begin
    for i := FDataAnnotates.Count - 1 downto 0 do
    begin
      if FDataAnnotates[i].StartItemNo > AItemNo then  // 变动在此批注之前
        Continue;
      if FDataAnnotates[i].EndItemNo < AItemNo then  // 变动在此批注之后
        Break;

      vDataAnn := FDataAnnotates[i];

      if vDataAnn.StartItemNo = AItemNo then  // 是批注起始Item
      begin
        if vDataAnn.EndItemNo = AItemNo then  // 同时也是批注结束Item(批注在同一个Item里)
        begin
          if AOffset <= vDataAnn.StartItemOffset then  // 在批注起始前面
          begin
            vDataAnn.StartItemOffset := vDataAnn.StartItemOffset + 1;
            vDataAnn.EndItemOffset := vDataAnn.EndItemOffset + 1;
          end
          else  // AOffset > vDataAnn.StartItemOffset
          if AOffset <= vDataAnn.EndItemOffset then  // 在批注中间
            vDataAnn.EndItemOffset := vDataAnn.EndItemOffset + 1;

          if vDataAnn.StartItemOffset = vDataAnn.EndItemOffset then  // 删除没了
            FDataAnnotates.Delete(i);
        end
        else  // 批注起始和结束不是同一个Item
        begin
          if AOffset <= vDataAnn.StartItemOffset then  // 在批注起始前面
            vDataAnn.StartItemOffset := vDataAnn.StartItemOffset + 1;
        end;
      end
      else
      if vDataAnn.EndItemNo = AItemNo then  // 是批注结束Item
      begin
        if AOffset <= vDataAnn.EndItemOffset then  // 在批注中间
          vDataAnn.EndItemOffset := vDataAnn.EndItemOffset + 1;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Backspace键删除了字符'}
  procedure _AnnotateBackChar;
  var
    i: Integer;
    vDataAnn: THCDataAnnotate;
  begin
    for i := FDataAnnotates.Count - 1 downto 0 do
    begin
      if FDataAnnotates[i].StartItemNo > AItemNo then  // 变动在此批注之前
        Continue;
      if FDataAnnotates[i].EndItemNo < AItemNo then  // 变动在此批注之后
        Break;

      vDataAnn := FDataAnnotates[i];

      if vDataAnn.StartItemNo = AItemNo then  // 是批注起始Item
      begin
        if vDataAnn.EndItemNo = AItemNo then  // 同时也是批注结束Item(批注在同一个Item里)
        begin
          if (AOffset > vDataAnn.StartItemOffset)  // 在批注起始后面
            and (AOffset <= vDataAnn.EndItemOffset)  // 在批注中间
          then
            vDataAnn.EndItemOffset := vDataAnn.EndItemOffset - 1
          else  // 在批注所在的Item批注位置前面删除
          begin
            vDataAnn.StartItemOffset := vDataAnn.StartItemOffset - 1;
            vDataAnn.EndItemOffset := vDataAnn.EndItemOffset - 1
          end;

          if vDataAnn.StartItemOffset = vDataAnn.EndItemOffset then  // 删除没了
            FDataAnnotates.Delete(i);
        end
        else  // 批注起始和结束不是同一个Item
        begin
          if AOffset >= vDataAnn.StartItemOffset then  // 在批注起始后面
            vDataAnn.StartItemOffset := vDataAnn.StartItemOffset - 1;
        end;
      end
      else
      if vDataAnn.EndItemNo = AItemNo then  // 是批注结束Item
      begin
        if AOffset <= vDataAnn.EndItemOffset then  // 在批注中间
          vDataAnn.EndItemOffset := vDataAnn.EndItemOffset - 1;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Delete键删除了字符'}
  procedure _AnnotateDeleteChar;
  var
    i: Integer;
    vDataAnn: THCDataAnnotate;
  begin
    for i := FDataAnnotates.Count - 1 downto 0 do
    begin
      if FDataAnnotates[i].StartItemNo > AItemNo then  // 变动在此批注之前
        Continue;
      if FDataAnnotates[i].EndItemNo < AItemNo then  // 变动在此批注之后
        Break;

      vDataAnn := FDataAnnotates[i];

      if vDataAnn.StartItemNo = AItemNo then  // 是批注起始Item
      begin
        if vDataAnn.EndItemNo = AItemNo then  // 同时也是批注结束Item(批注在同一个Item里)
        begin
          if AOffset <= vDataAnn.StartItemOffset then  // 在批注起始前面
          begin
            vDataAnn.StartItemOffset := vDataAnn.StartItemOffset - 1;
            vDataAnn.EndItemOffset := vDataAnn.EndItemOffset - 1;
          end
          else  // AOffset > vDataAnn.StartItemOffset
          if AOffset <= vDataAnn.EndItemOffset then  // 在批注中间
            vDataAnn.EndItemOffset := vDataAnn.EndItemOffset - 1;

          if vDataAnn.StartItemOffset = vDataAnn.EndItemOffset then  // 删除没了
            FDataAnnotates.Delete(i);
        end
        else  // 批注起始和结束不是同一个Item
        begin
          if AOffset <= vDataAnn.StartItemOffset then  // 在批注起始前面
            vDataAnn.StartItemOffset := vDataAnn.StartItemOffset - 1;
        end;
      end
      else
      if vDataAnn.EndItemNo = AItemNo then  // 是批注结束Item
      begin
        if AOffset <= vDataAnn.EndItemOffset then  // 在批注中间
          vDataAnn.EndItemOffset := vDataAnn.EndItemOffset - 1;
      end;
    end;
  end;
  {$ENDREGION}

begin
  case AAction of
    hiaRemove: _AnnotateRemove;
    hiaInsertChar: _AnnotateInsertChar;
    hiaBackDeleteChar: _AnnotateBackChar;
    hiaDeleteChar: _AnnotateDeleteChar;
  end;
end;

procedure THCAnnotateData.DoRemoveAnnotate(Sender: TObject);
begin
  Style.UpdateInfoRePaint;
  if Assigned(FOnRemoveAnnotate) then
    FOnRemoveAnnotate(Self, THCDataAnnotate(Sender));
end;

procedure THCAnnotateData.DoRemoveItem(const AItem: THCCustomItem);
begin
  DoDataRemoveItem(Self, AItem);
end;

function THCAnnotateData.DrawItemOfAnnotate(const ADrawItemNo: Integer;
  const ACanvas: TCanvas; const ADrawRect: TRect): Boolean;
var
  i, vItemNo: Integer;
  vDataAnnotate: THCDataAnnotate;
begin
  Result := False;
  if FDataAnnotates.Count = 0 then Exit;

  vItemNo := Self.DrawItems[ADrawItemNo].ItemNo;
  if vItemNo < FDataAnnotates.First.StartItemNo then Exit;
  if vItemNo > FDataAnnotates.Last.EndItemNo then Exit;

  FDrawItemAnnotates.Clear;
  for i := 0 to FDataAnnotates.Count - 1 do
  begin
    vDataAnnotate := FDataAnnotates[i];

    if vDataAnnotate.EndItemNo < vItemNo then  // 未进入本次查找范围
      Continue;

    if vDataAnnotate.StartItemNo > vItemNo then  // 超出本次查找的范围
      Break;

    if ADrawItemNo = vDataAnnotate.StartDrawItemNo then
    begin
      if ADrawItemNo = vDataAnnotate.EndDrawItemNo then  // 当前DrawItem既是批注起始又是批注结束
      begin
        FDrawItemAnnotates.NewDrawAnnotate(
          Rect(ADrawRect.Left + GetDrawItemOffsetWidth(ADrawItemNo, vDataAnnotate.StartItemOffset - Self.DrawItems[ADrawItemNo].CharOffs + 1, ACanvas),
            ADrawRect.Top,
            ADrawRect.Left + GetDrawItemOffsetWidth(ADrawItemNo, vDataAnnotate.EndItemOffset - Self.DrawItems[ADrawItemNo].CharOffs + 1, ACanvas),
            ADrawRect.Bottom),
          amBoth, vDataAnnotate);
      end
      else  // 仅是批注头
      begin
        FDrawItemAnnotates.NewDrawAnnotate(
          Rect(ADrawRect.Left + GetDrawItemOffsetWidth(ADrawItemNo, vDataAnnotate.StartItemOffset - Self.DrawItems[ADrawItemNo].CharOffs + 1, ACanvas),
            ADrawRect.Top, ADrawRect.Right, ADrawRect.Bottom),
          amFirst, vDataAnnotate);
      end;

      Result := True;
    end
    else
    if ADrawItemNo = vDataAnnotate.EndDrawItemNo then  // 当前DrawItem是批注结束
    begin
      FDrawItemAnnotates.NewDrawAnnotate(
        Rect(ADrawRect.Left, ADrawRect.Top,
          ADrawRect.Left + GetDrawItemOffsetWidth(ADrawItemNo, vDataAnnotate.EndItemOffset - Self.DrawItems[ADrawItemNo].CharOffs + 1, ACanvas),
          ADrawRect.Bottom),
        amLast, vDataAnnotate);

      Result := True;
    end
    else
    if (ADrawItemNo > vDataAnnotate.StartDrawItemNo) and (ADrawItemNo < vDataAnnotate.EndDrawItemNo) then  // 当前DrawItem是批注范围内
    begin
      FDrawItemAnnotates.NewDrawAnnotate(ADrawRect, amNormal, vDataAnnotate);
      Result := True;
    end;
  end;
end;

procedure THCAnnotateData.GetCaretInfo(const AItemNo, AOffset: Integer;
  var ACaretInfo: THCCaretInfo);
var
  vDataAnnotate: THCDataAnnotate;
  vCaretDrawItemNo: Integer;
begin
  inherited GetCaretInfo(AItemNo, AOffset, ACaretInfo);

  if CaretDrawItemNo < 0 then
    vCaretDrawItemNo := GetDrawItemNoByOffset(SelectInfo.StartItemNo, SelectInfo.StartItemOffset)
  else
    vCaretDrawItemNo := CaretDrawItemNo;

  vDataAnnotate := GetDrawItemFirstDataAnnotateAt(vCaretDrawItemNo,
    GetDrawItemOffsetWidth(vCaretDrawItemNo,
      SelectInfo.StartItemOffset - DrawItems[vCaretDrawItemNo].CharOffs + 1),
    DrawItems[vCaretDrawItemNo].Rect.Top + 1);

  if FActiveAnnotate <> vDataAnnotate then
  begin
    FActiveAnnotate := vDataAnnotate;
    Style.UpdateInfoRePaint;
  end;
end;

function THCAnnotateData.GetDrawItemFirstDataAnnotateAt(
  const ADrawItemNo, X, Y: Integer): THCDataAnnotate;
var
  i, vStyleNo: Integer;
  vPt: TPoint;
begin
  Result := nil;

  vStyleNo := GetDrawItemStyle(ADrawItemNo);
  if vStyleNo > THCStyle.Null then
    Style.ApplyTempStyle(vStyleNo);

  if DrawItemOfAnnotate(ADrawItemNo, Style.TempCanvas, DrawItems[ADrawItemNo].Rect) then
  begin
    vPt := Point(X, Y);
    for i := 0 to FDrawItemAnnotates.Count - 1 do
    begin
      if PtInRect(FDrawItemAnnotates[i].DrawRect, vPt) then
      begin
        Result := FDrawItemAnnotates[i].DataAnnotate;
        Break;  // 先只取一个
      end;
    end;
  end;
end;

procedure THCAnnotateData.InitializeField;
begin
  inherited InitializeField;
  FHotAnnotate := nil;
  FActiveAnnotate := nil;
end;

function THCAnnotateData.InsertAnnotate(const ATitle, AText: string): Boolean;
var
  vTopData: THCRichData;
begin
  Result := False;
  if not CanEdit then Exit;
  if not Self.SelectExists then Exit;

  vTopData := GetTopLevelData;
  if (vTopData is THCAnnotateData) and (vTopData <> Self) then
    (vTopData as THCAnnotateData).InsertAnnotate(ATitle, AText)
  else
    FDataAnnotates.NewDataAnnotate(SelectInfo, ATitle, AText);
end;

procedure THCAnnotateData.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vAnnCount: Word;
  i: Integer;
  vAnn: THCDataAnnotate;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 22 then
  begin
    AStream.ReadBuffer(vAnnCount, SizeOf(vAnnCount));
    if vAnnCount > 0 then
    begin
      for i := 0 to vAnnCount - 1 do
      begin
        vAnn := THCDataAnnotate.Create;
        vAnn.LoadFromStream(AStream, AFileVersion);
        FDataAnnotates.Add(vAnn);
      end;
    end;
  end;
end;

procedure THCAnnotateData.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vDataAnnotate: THCDataAnnotate;
begin
  inherited MouseMove(Shift, X, Y);

  vDataAnnotate := GetDrawItemFirstDataAnnotateAt(MouseMoveDrawItemNo, X, Y);

  if FHotAnnotate <> vDataAnnotate then
  begin
    FHotAnnotate := vDataAnnotate;
    Style.UpdateInfoRePaint;
  end;
end;

procedure THCAnnotateData.PaintData(const ADataDrawLeft, ADataDrawTop,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom, AVOffset, AFirstDItemNo,
  ALastDItemNo: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  CheckAnnotateRange(AFirstDItemNo, ALastDItemNo);  // 指定DrawItem范围内的批注获取各自的DrawItem范围
  inherited PaintData(ADataDrawLeft, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, AVOffset, AFirstDItemNo, ALastDItemNo, ACanvas, APaintInfo);
  FDrawItemAnnotates.Clear;
end;

procedure THCAnnotateData.SaveToStream(const AStream: TStream;
  const AStartItemNo, AStartOffset, AEndItemNo, AEndOffset: Integer);
var
  vAnnCount: Word;
  i: Integer;
begin
  inherited SaveToStream(AStream, AStartItemNo, AStartOffset, AEndItemNo, AEndOffset);
  // 存批注
  vAnnCount := FDataAnnotates.Count;
  AStream.WriteBuffer(vAnnCount, SizeOf(vAnnCount));  // 数量
  for i := 0 to vAnnCount - 1 do
    FDataAnnotates[i].SaveToStream(AStream);
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

procedure THCDataAnnotate.LoadFromStream(const AStream: TStream;
  const AFileVersion: Word);
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

  HCLoadTextFromStream(AStream, FTitle);
  HCLoadTextFromStream(AStream, FText);
end;

procedure THCDataAnnotate.SaveToStream(const AStream: TStream);
begin
  AStream.WriteBuffer(Self.StartItemNo, SizeOf(Self.StartItemNo));
  AStream.WriteBuffer(Self.StartItemOffset, SizeOf(Self.StartItemOffset));
  AStream.WriteBuffer(Self.EndItemNo, SizeOf(Self.EndItemNo));
  AStream.WriteBuffer(Self.EndItemOffset, SizeOf(Self.EndItemOffset));
  AStream.WriteBuffer(FID, SizeOf(FID));

  HCSaveTextToStream(AStream, FTitle);
  HCSaveTextToStream(AStream, FText);
end;

{ THCDataAnnotates }

procedure THCDataAnnotates.DeleteByID(const AID: Integer);
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
  begin
    if Items[i].ID = AID then
    begin
      Self.Delete(i);
      Break;
    end;
  end;
end;

procedure THCDataAnnotates.NewDataAnnotate(const ASelectInfo: TSelectInfo;
  const ATitle, AText: string);
var
  vDataAnnotate: THCDataAnnotate;
begin
  vDataAnnotate := THCDataAnnotate.Create;
  vDataAnnotate.CopyRange(ASelectInfo);
  vDataAnnotate.Title := ATitle;
  vDataAnnotate.Text := AText;
  vDataAnnotate.ID := Self.Add(vDataAnnotate);
end;

procedure THCDataAnnotates.Notify(const Value: THCDataAnnotate;
  Action: TCollectionNotification);
begin
  if Action = cnAdded then
  begin
    if Assigned(FOnInsertAnnotate) then
      FOnInsertAnnotate(Value);
  end
  else
  if Action = cnRemoved then
  begin
    if Assigned(FOnRemoveAnnotate) then
      FOnRemoveAnnotate(Value);
  end;

  inherited Notify(Value, Action);
end;

{ THCDrawItemAnnotate }

function THCDrawItemAnnotate.First: Boolean;
begin
  Result := (Mark = amFirst) or (Mark = amBoth);
end;

function THCDrawItemAnnotate.Last: Boolean;
begin
  Result := (Mark = amLast) or (Mark = amBoth);
end;

{ THCDrawItemAnnotates }

procedure THCDrawItemAnnotates.NewDrawAnnotate(const ARect: TRect;
  const AMark: THCAnnotateMark; const ADataAnnotate: THCDataAnnotate);
var
  vDrawItemAnnotate: THCDrawItemAnnotate;
begin
  vDrawItemAnnotate := THCDrawItemAnnotate.Create;
  vDrawItemAnnotate.DrawRect := ARect;
  vDrawItemAnnotate.Mark := AMark;
  vDrawItemAnnotate.DataAnnotate := ADataAnnotate;
  Self.Add(vDrawItemAnnotate);
end;

end.
