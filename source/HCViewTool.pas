unit HCViewTool;

interface

uses
  Windows, Classes, Graphics, SysUtils, Controls, ImgList, Generics.Collections,
  HCView, HCStyle, HCCustomData, HCItem, HCTableItem, HCImageItem, HCToolBar, HCShape;

type
  THCViewTool = class(THCView)
  strict private
    FImages: TCustomImageList;
    FTopData: THCCustomData;
    FActiveItem: THCCustomItem;
    FActiveItemRect: TRect;
    FToolOffset: Integer;
    FHotToolBar: THCToolBar;
    FTableToolBar: THCTableToolBar;
    FImageToolBar: THCImageToolBar;
    FMouseViewPt: TPoint;

    procedure DoImageShapeStructOver(Sender: TObject);

    procedure SetActiveItem(const Value: THCCustomItem);
    procedure SetImages(const Value: TCustomImageList);
    function PtInTableToolBar(const X, Y: Integer): Boolean;
    function PtInImageToolBar(const X, Y: Integer): Boolean;
    procedure DoTableToolBarUpdateView(const ARect: TRect; const ACanvas: TCanvas);
    procedure DoImageToolBarUpdateView(const ARect: TRect; const ACanvas: TCanvas);
    procedure DoImageToolBarControlPaint(const AControl: THCToolBarControl;
      const ALeft, ATop: Integer; const ACanvas: TCanvas);
    function TableMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function TableMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
    function TableMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function TableKeyDown(var Key: Word; Shift: TShiftState): Boolean;
    //
    function ImageMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function ImageMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
    function ImageMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function ImageKeyDown(var Key: Word; Shift: TShiftState): Boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

    procedure DoCaretChange; override;
    procedure DoSectionRemoveItem(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem); override;
    procedure DoSectionDrawItemPaintAfter(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    procedure DoPaintViewBefor(const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    /// <summary> 视图绘制完成 </summary>
    procedure DoPaintViewAfter(const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Images: TCustomImageList read FImages write SetImages;
  end;

implementation

{ THCViewTool }

constructor THCViewTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FToolOffset := -4;
  FActiveItem := nil;
  FHotToolBar := nil;

  FTableToolBar := THCTableToolBar.Create;
  FTableToolBar.OnUpdateView := DoTableToolBarUpdateView;

  FImageToolBar := THCImageToolBar.Create;
  FImageToolBar.OnUpdateView := DoImageToolBarUpdateView;
  FImageToolBar.OnControlPaint := DoImageToolBarControlPaint;
end;

destructor THCViewTool.Destroy;
begin
  FreeAndNil(FTableToolBar);
  FreeAndNil(FImageToolBar);
  inherited Destroy;
end;

procedure THCViewTool.DoTableToolBarUpdateView(const ARect: TRect; const ACanvas: TCanvas);
var
  vRect: TRect;
begin
  if Self.HandleAllocated and Assigned(FTableToolBar) then
  begin
    vRect := ARect;
    OffsetRect(vRect, FTableToolBar.Left, FTableToolBar.Top);
    UpdateView(vRect);
  end;
end;

function THCViewTool.ImageKeyDown(var Key: Word; Shift: TShiftState): Boolean;
var
  vImageItem: THCImageItem;
begin
  Result := False;

  vImageItem := FActiveItem as THCImageItem;
  if vImageItem.ShapeManager.KeyDown(Key, Shift) then
  begin
    Self.UpdateView(Rect(FActiveItemRect.Left, FActiveItemRect.Top - FImageToolBar.Height,
      FActiveItemRect.Right, FActiveItemRect.Bottom));

    Result := True;
  end;
end;

function THCViewTool.ImageMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vImageItem: THCImageItem;
begin
  Result := False;
  if PtInImageToolBar(X, Y) then  // 鼠标在图片编辑工具条上
  begin
    FImageToolBar.MouseDown(Button, Shift, X - FImageToolBar.Left, Y - FImageToolBar.Top);
    if FImageToolBar.ActiveIndex >= 0 then
      (FActiveItem as THCImageItem).ShapeManager.OperStyle := THCShapeStyle(FImageToolBar.ActiveControl.Tag);

    Result := True;
  end
  else
  //if FImageToolBar.ActiveIndex > 0 then  // 鼠标不在图片编辑工具条上，但是点击了除指针外的某个编辑按钮
  begin
    vImageItem := FActiveItem as THCImageItem;
    if PtInRect(Bounds(FActiveItemRect.Left, FActiveItemRect.Top,
                       vImageItem.Width, vImageItem.Height),
                Point(FMouseViewPt.X, FMouseViewPt.Y))
    then  // 在图片上操作
    begin
      if vImageItem.ShapeManager.MouseDown(Button, Shift,
                                           FMouseViewPt.X - FActiveItemRect.Left,
                                           FMouseViewPt.Y - FActiveItemRect.Top)
      then
      begin
        Self.UpdateView(Rect(FActiveItemRect.Left, FActiveItemRect.Top - FImageToolBar.Height,
          FActiveItemRect.Right, FActiveItemRect.Bottom));

        Result := True;
      end;
    end;
  end;
end;

function THCViewTool.ImageMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  vImageItem: THCImageItem;
begin
  Result := False;
  if PtInImageToolBar(X, Y) then  // 在图片工具条上
  begin
    if FHotToolBar <> FImageToolBar then
    begin
      if Assigned(FHotToolBar) then
        FHotToolBar.MouseLeave;

      FHotToolBar := FImageToolBar;
      FHotToolBar.MouseEnter;
    end;
  end
  else
  if FHotToolBar = FImageToolBar then  // 移出图片工具条上
  begin
    FImageToolBar.MouseLeave;
    FHotToolBar := nil;
  end
  else
  //if FImageToolBar.ActiveIndex > 0 then  // 有效的样式、第一个是指针
  begin
    vImageItem := FActiveItem as THCImageItem;
    if PtInRect(Bounds(FActiveItemRect.Left, FActiveItemRect.Top,
                       vImageItem.Width, vImageItem.Height),
                Point(FMouseViewPt.X, FMouseViewPt.Y))
    then  // 在图片上操作
    begin
      if vImageItem.ShapeManager.MouseMove(Shift,
        FMouseViewPt.X - FActiveItemRect.Left,
        FMouseViewPt.Y - FActiveItemRect.Top)
      then
      begin
        Self.UpdateView(Rect(FActiveItemRect.Left, FActiveItemRect.Top - FImageToolBar.Height,
          FActiveItemRect.Right, FActiveItemRect.Bottom));

        if vImageItem.ShapeManager.HotIndex >= 0 then
          Cursor := vImageItem.ShapeManager[vImageItem.ShapeManager.HotIndex].Cursor;

        Result := True;
      end;
    end;
  end;
end;

function THCViewTool.ImageMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vImageItem: THCImageItem;
begin
  Result := False;
  if PtInImageToolBar(X, Y) then
  begin
    FImageToolBar.MouseUp(Button, Shift, X - FImageToolBar.Left, Y - FImageToolBar.Top);
    Result := True;
  end
  else
  //if FImageToolBar.ActiveIndex > 0 then  // 第一个是指针
  begin
    vImageItem := FActiveItem as THCImageItem;
    if PtInRect(Bounds(FActiveItemRect.Left, FActiveItemRect.Top,
                       vImageItem.Width, vImageItem.Height),
                Point(FMouseViewPt.X, FMouseViewPt.Y))
    then  // 在图片上操作
    begin
      if vImageItem.ShapeManager.MouseUp(Button, Shift,
                                         FMouseViewPt.X - FActiveItemRect.Left,
                                         FMouseViewPt.Y - FActiveItemRect.Top)
      then
      begin
        Result := True;
      end;
    end;
  end;
end;

procedure THCViewTool.DoCaretChange;
begin
  inherited DoCaretChange;
  FTableToolBar.Visible := False;
  FImageToolBar.Visible := False;
  if FActiveItem is THCTableItem then
    //(FActiveItem as THCTableItem).ShapeManager.DisActive
  else
  if FActiveItem is THCImageItem then
    (FActiveItem as THCImageItem).ShapeManager.DisActive;
end;

procedure THCViewTool.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if FTableToolBar.Visible or FImageToolBar.Visible then
    Handled := True;

  inherited DoContextPopup(MousePos, Handled);
end;

procedure THCViewTool.DoImageShapeStructOver(Sender: TObject);
begin
  // 构建完成后，图片工具栏恢复到指针按钮
  (FActiveItem as THCImageItem).ShapeManager.OperStyle := THCShapeStyle.hssNone;
  FImageToolBar.ActiveIndex := 0;
end;

procedure THCViewTool.DoImageToolBarControlPaint(const AControl: THCToolBarControl;
  const ALeft, ATop: Integer; const ACanvas: TCanvas);
begin
  if Assigned(FImages) then
    FImages.Draw(ACanvas, ALeft + 4, ATop + 4, AControl.Tag);
end;

procedure THCViewTool.DoImageToolBarUpdateView(const ARect: TRect;
  const ACanvas: TCanvas);
var
  vRect: TRect;
begin
  if Self.HandleAllocated and Assigned(FImageToolBar) then
  begin
    vRect := ARect;
    OffsetRect(vRect, FImageToolBar.Left, FImageToolBar.Top);
    UpdateView(vRect);
//    BitBlt(Self.Canvas.Handle, vRect.Left, vRect.Top, vRect.Right - vRect.Left,
//      vRect.Bottom - vRect.Top, ACanvas.Handle, ARect.Left, ARect.Top, SRCCOPY);
//    InvalidateRect(Self.Handle, ARect, False);
  end;
end;

procedure THCViewTool.DoPaintViewAfter(const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vPt: TPoint;
begin
  inherited DoPaintViewAfter(ACanvas, APaintInfo);

  if Assigned(FActiveItem) and (not Self.ReadOnly) then
  begin
    if FTableToolBar.Visible then
    begin
      if APaintInfo.ScaleX <> 1 then
      begin
        SetViewportExtEx(ACanvas.Handle, APaintInfo.WindowWidth, APaintInfo.WindowHeight, @vPt);
        try
          FTableToolBar.PaintTo(ACanvas, APaintInfo.GetScaleX(FActiveItemRect.Left),
            APaintInfo.GetScaleY(FActiveItemRect.Top) + FToolOffset - FTableToolBar.Height);
        finally
          SetViewportExtEx(ACanvas.Handle, APaintInfo.GetScaleX(APaintInfo.WindowWidth),
            APaintInfo.GetScaleY(APaintInfo.WindowHeight), @vPt);
        end;
      end
      else
        FTableToolBar.PaintTo(ACanvas, FActiveItemRect.Left, FActiveItemRect.Top + FToolOffset - FTableToolBar.Height);
    end
    else
    if FImageToolBar.Visible then
    begin
      if APaintInfo.ScaleX <> 1 then
      begin
        SetViewportExtEx(ACanvas.Handle, APaintInfo.WindowWidth, APaintInfo.WindowHeight, @vPt);
        try
          FImageToolBar.PaintTo(ACanvas, APaintInfo.GetScaleX(FActiveItemRect.Left),
            APaintInfo.GetScaleY(FActiveItemRect.Top) + FToolOffset - FImageToolBar.Height);
        finally
          SetViewportExtEx(ACanvas.Handle, APaintInfo.GetScaleX(APaintInfo.WindowWidth),
            APaintInfo.GetScaleY(APaintInfo.WindowHeight), @vPt);
        end;
      end
      else
        FImageToolBar.PaintTo(ACanvas, FActiveItemRect.Left, FActiveItemRect.Top + FToolOffset - FImageToolBar.Height);
    end;
  end;
end;

procedure THCViewTool.DoPaintViewBefor(const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoPaintViewBefor(ACanvas, APaintInfo);
  FActiveItemRect.Top := -1000;
end;

procedure THCViewTool.DoSectionDrawItemPaintAfter(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
  const ADrawRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom,
  ADataScreenTop, ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  inherited DoSectionDrawItemPaintAfter(Sender, AData, AItemNo, ADrawItemNo,
    ADrawRect, ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if (AData = FTopData) and (AData.Items[AItemNo] = FActiveItem) then
    FActiveItemRect := ADrawRect;
end;

procedure THCViewTool.DoSectionRemoveItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
begin
  inherited DoSectionRemoveItem(Sender, AData, AItem);
  if AItem = FActiveItem then
    FActiveItem := nil;
end;

procedure THCViewTool.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not Self.ReadOnly then
  begin
    if FTableToolBar.Visible and TableKeyDown(Key, Shift) then Exit;
    if FImageToolBar.Visible and ImageKeyDown(Key, Shift) then Exit;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure THCViewTool.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

end;

procedure THCViewTool.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure THCViewTool.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vTopItem: THCCustomItem;
begin
  if not Self.ReadOnly then
  begin
    if FTableToolBar.Visible and TableMouseDown(Button, Shift, X, Y) then Exit;
    if FImageToolBar.Visible and ImageMouseDown(Button, Shift, X, Y) then Exit;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  FTopData := Self.ActiveSectionTopLevelData;
  vTopItem := Self.GetTopLevelItem;
  SetActiveItem(vTopItem);
end;

procedure THCViewTool.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not Self.ReadOnly then
  begin
    FMouseViewPt.X := ZoomOut(X);
    FMouseViewPt.Y := ZoomOut(Y);

    if FTableToolBar.Visible and TableMouseMove(Shift, X, Y) then Exit;
    if FImageToolBar.Visible and ImageMouseMove(Shift, X, Y) then Exit;
  end;

  if Assigned(FHotToolBar) then
    FHotToolBar.MouseMove(Shift, X - FHotToolBar.Left, Y - FHotToolBar.Top)
  else
    inherited MouseMove(Shift, X, Y);
end;

procedure THCViewTool.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if not Self.ReadOnly then
  begin
    if FTableToolBar.Visible and TableMouseUp(Button, Shift, X, Y) then Exit;
    if FImageToolBar.Visible and ImageMouseUp(Button, Shift, X, Y) then Exit;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

function THCViewTool.PtInImageToolBar(const X, Y: Integer): Boolean;
var
  vPt: TPoint;
begin
  Result := False;
  vPt.X := X;
  vPt.Y := Y;
  Result := PtInRect(FImageToolBar.Bound, vPt);
end;

function THCViewTool.PtInTableToolBar(const X, Y: Integer): Boolean;
var
  vPt: TPoint;
begin
  Result := False;
  vPt.X := X;
  vPt.Y := Y;
  Result := PtInRect(FTableToolBar.Bound, vPt);
end;

procedure THCViewTool.SetActiveItem(const Value: THCCustomItem);
var
  vPt: TPoint;
begin
  // MouseDown里会触发重绘，此时ToolBar并未确定显示，处理ToolBar的Visible属性
  // 会重新触发重绘，重绘是通过DoImageToolBarUpdateView(Rect)，需要先计算区域参数
  // 然后触发UpdateView，所以需要提前计算ToolBar的坐标vPt位置

  if FActiveItem <> Value then
  begin
    if FActiveItem is THCTableItem then
      FTableToolBar.Visible := False
    else
    if FActiveItem is THCImageItem then
      FImageToolBar.Visible := False;

    if Assigned(Value) and Value.Active then
    begin
      if Value is THCTableItem then
      begin
        FActiveItem := Value;
        vPt := Self.GetActiveDrawItemViewCoord;
        FTableToolBar.Left := vPt.X;
        FTableToolBar.Top := vPt.Y - FTableToolBar.Height + FToolOffset;
        //FTableToolBar.Visible := True;  暂时没有不显示
      end
      else
      if Value is THCImageItem then
      begin
        FActiveItem := Value;
        (FActiveItem as THCImageItem).ShapeManager.OnStructOver := DoImageShapeStructOver;
        vPt := Self.GetActiveDrawItemViewCoord;
        FImageToolBar.Left := vPt.X;
        FImageToolBar.Top := vPt.Y - FImageToolBar.Height + FToolOffset;
        FImageToolBar.Visible := True;
      end
      else
        FActiveItem := nil;
    end
    else
      FActiveItem := nil;
  end
  else
  if Assigned(FActiveItem) and (not FActiveItem.Active) then
  begin
    if FActiveItem is THCTableItem then
      FTableToolBar.Visible := False
    else
    if FActiveItem is THCImageItem then
      FImageToolBar.Visible := False;

    FActiveItem := nil;
  end;
end;

procedure THCViewTool.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  FTableToolBar.UpdateView;
  FImageToolBar.UpdateView;
end;

function THCViewTool.TableKeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function THCViewTool.TableMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vTableItem: THCTableItem;
begin
  Result := False;
  if PtInTableToolBar(X, Y) then  // 鼠标在表格编辑工具条上
  begin
    FTableToolBar.MouseDown(Button, Shift, X - FTableToolBar.Left, Y - FTableToolBar.Top);
    Result := True;
  end
  else
  //if FTableToolBar.ActiveIndex >= 0 then  // 鼠标不在表格编辑工具条上，但是点击了某个编辑按钮
  begin
    vTableItem := FActiveItem as THCTableItem;
    if PtInRect(Bounds(FActiveItemRect.Left, FActiveItemRect.Top,
                       vTableItem.Width, vTableItem.Height),
                Point(FMouseViewPt.X, FMouseViewPt.Y))
    then  // 在表格上操作
    begin
      Result := True;
    end;
  end;
end;

function THCViewTool.TableMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  vTableItem: THCTableItem;
begin
  Result := False;
  if PtInTableToolBar(X, Y) then
  begin
    if FHotToolBar <> FTableToolBar then
    begin
      if Assigned(FHotToolBar) then
        FHotToolBar.MouseLeave;

      FHotToolBar := FTableToolBar;
      FHotToolBar.MouseEnter;
    end;
  end
  else
  if FHotToolBar = FTableToolBar then
  begin
    FTableToolBar.MouseLeave;
    FHotToolBar := nil;
  end
  else
  //if FTableToolBar.ActiveIndex > 0 then  // 第一个是指针
  begin
    vTableItem := FActiveItem as THCTableItem;
    if PtInRect(Bounds(FActiveItemRect.Left, FActiveItemRect.Top,
                       vTableItem.Width, vTableItem.Height),
                Point(FMouseViewPt.X, FMouseViewPt.Y))
    then  // 在表格上操作
    begin
      Result := True;
    end;
  end;
end;

function THCViewTool.TableMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vTableItem: THCTableItem;
begin
  Result := False;
  if PtInTableToolBar(X, Y) then
  begin
    FTableToolBar.MouseUp(Button, Shift, X - FTableToolBar.Left, Y - FTableToolBar.Top);
    Result := True;
  end
  else
  //if FTableToolBar.ActiveIndex > 0 then  // 第一个是指针
  begin
    vTableItem := FActiveItem as THCTableItem;
    if PtInRect(Bounds(FActiveItemRect.Left, FActiveItemRect.Top,
                       vTableItem.Width, vTableItem.Height),
                Point(FMouseViewPt.X, FMouseViewPt.Y))
    then  // 在表格上操作
    begin
      Result := True;
    end;
  end;
end;

end.
