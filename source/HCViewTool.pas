unit HCViewTool;

interface

uses
  Windows, Classes, Graphics, SysUtils, Controls, ImgList, Generics.Collections,
  Menus, HCView, HCStyle, HCCustomData, HCItem, HCTableItem, HCImageItem, HCToolBar,
  HCShape;

type
  THCViewTool = class(THCView)
  strict private
    FIconList: TObjectList<TBitmap>;
    FToolImageList: TCustomImageList;
    FTopData: THCCustomData;
    FActiveItem: THCCustomItem;
    FActiveItemRect: TRect;
    FToolOffset: Integer;
    FHotToolBar, FCaptureToolBar: THCToolBar;
    FTableToolMenu: TPopupMenu;
    FTableToolBar: THCTableToolBar;
    FImageToolBar: THCImageToolBar;
    FMouseViewPt: TPoint;
    FUseTableTool, FUseImageTool: Boolean;

    FOnTableToolPropertyClick: TNotifyEvent;
    procedure LoadImageList;
    procedure DoTableToolResetRowColClick(Sender: TObject);
    procedure DoTableToolPropertyClick(Sender: TObject);
    procedure DoImageShapeStructOver(Sender: TObject);

    procedure SetUseTableTool(const Value: Boolean);
    procedure SetUseImageTool(const Value: Boolean);

    procedure SetActiveToolItem(const AItem: THCCustomItem);
    procedure CancelActiveToolItem;
    function PtInTableToolBar(const X, Y: Integer): Boolean;
    function PtInImageToolBar(const X, Y: Integer): Boolean;
    procedure DoTableToolBarUpdateView(const ARect: TRect; const ACanvas: TCanvas);
    procedure DoTableToolBarControlPaint(const AControl: THCToolBarControl;
      const ALeft, ATop: Integer; const ACanvas: TCanvas);
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
    procedure DoKillFocus; override;
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
    property UseTableTool: Boolean read FUseTableTool write SetUseTableTool;
    property UseImageTool: Boolean read FUseImageTool write SetUseImageTool;
    property OnTableToolPropertyClick: TNotifyEvent read FOnTableToolPropertyClick write FOnTableToolPropertyClick;
  end;

implementation

{$R HCViewTool.RES}

{ THCViewTool }

procedure THCViewTool.CancelActiveToolItem;
begin
  if FActiveItem is THCImageItem then
  begin
    (FActiveItem as THCImageItem).ShapeManager.DisActive;
    FImageToolBar.Visible := False;
  end
  else
  if FActiveItem is THCTableItem then  // 是表格
    FTableToolBar.Visible := False;

  FActiveItem := nil;
end;

constructor THCViewTool.Create(AOwner: TComponent);
var
  vMenuItem, vResetMenuItem: TMenuItem;
begin
  inherited Create(AOwner);
  FIconList := TObjectList<TBitmap>.Create;
  FToolImageList := TImageList.Create(nil);
  LoadImageList;

  FToolOffset := -4;
  FActiveItem := nil;
  FHotToolBar := nil;
  FUseTableTool := True;
  FUseImageTool := True;

  FTableToolMenu := TPopupMenu.Create(Self);
  vMenuItem := TMenuItem.Create(FTableToolMenu);
  vMenuItem.Caption := '重设行列';
  vResetMenuItem := TMenuItem.Create(FTableToolMenu);
  vResetMenuItem.Caption := '2 x 2';
  vResetMenuItem.Tag := 22;
  vResetMenuItem.OnClick := DoTableToolResetRowColClick;
  vMenuItem.Add(vResetMenuItem);
  FTableToolMenu.Items.Add(vMenuItem);

  vMenuItem := TMenuItem.Create(FTableToolMenu);
  vMenuItem.Caption := '属性';
  vMenuItem.OnClick := DoTableToolPropertyClick;
  FTableToolMenu.Items.Add(vMenuItem);

  FTableToolBar := THCTableToolBar.Create;
  FTableToolBar.OnUpdateView := DoTableToolBarUpdateView;
  FTableToolBar.OnControlPaint := DoTableToolBarControlPaint;

  FImageToolBar := THCImageToolBar.Create;
  FImageToolBar.OnUpdateView := DoImageToolBarUpdateView;
  FImageToolBar.OnControlPaint := DoImageToolBarControlPaint;
end;

destructor THCViewTool.Destroy;
begin
  CancelActiveToolItem;
  FreeAndNil(FTableToolMenu);
  FreeAndNil(FTableToolBar);
  FreeAndNil(FImageToolBar);
  FreeAndNil(FIconList);
  FreeAndNil(FToolImageList);

  inherited Destroy;
end;

procedure THCViewTool.DoTableToolBarControlPaint(
  const AControl: THCToolBarControl; const ALeft, ATop: Integer;
  const ACanvas: TCanvas);
begin
  FToolImageList.Draw(ACanvas, ALeft + 4, ATop + 4, AControl.Tag);
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

procedure THCViewTool.DoTableToolPropertyClick(Sender: TObject);
begin
  if Assigned(FOnTableToolPropertyClick) then
    FOnTableToolPropertyClick(Sender);
end;

procedure THCViewTool.DoTableToolResetRowColClick(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    22: Self.ActiveTableResetRowCol(2, 2);
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
      Cursor := crDefault;
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
    end
    else
      DoImageShapeStructOver(nil);
  end
end;

procedure THCViewTool.DoCaretChange;
begin
  inherited DoCaretChange;

  if not (FActiveItem is THCTableItem) then  // 不是表格
    FTableToolBar.Visible := False;

  if FActiveItem is THCImageItem then
    (FActiveItem as THCImageItem).ShapeManager.DisActive
  else
    FImageToolBar.Visible := False;
end;

procedure THCViewTool.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if FImageToolBar.Visible and (FImageToolBar.ActiveIndex > 0) then
    Handled := True;

  if FTableToolBar.Visible and (FTableToolBar.ActiveIndex >= 0) then
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
  FToolImageList.Draw(ACanvas, ALeft + 4, ATop + 4, AControl.Tag);
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

procedure THCViewTool.DoKillFocus;
begin
  inherited DoKillFocus;
  if Assigned(FTableToolBar) and FTableToolBar.Visible then
    FTableToolBar.UpdateView
  else
  if Assigned(FImageToolBar) and FImageToolBar.Visible then
    FImageToolBar.UpdateView;
end;

procedure THCViewTool.DoPaintViewAfter(const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vPt: TPoint;
begin
  inherited DoPaintViewAfter(ACanvas, APaintInfo);

  if Assigned(FActiveItem) and (not Self.ReadOnly) and Self.Focused then
  begin
    if FTableToolBar.Visible then
    begin
      if APaintInfo.ScaleX <> 1 then
      begin
        SetViewportExtEx(ACanvas.Handle, APaintInfo.WindowWidth, APaintInfo.WindowHeight, @vPt);
        try
          FTableToolBar.PaintTo(ACanvas, APaintInfo.GetScaleX(FActiveItemRect.Left - FTableToolBar.Width + FToolOffset),
            APaintInfo.GetScaleY(FActiveItemRect.Top));// + FToolOffset - FTableToolBar.Height);
        finally
          SetViewportExtEx(ACanvas.Handle, APaintInfo.GetScaleX(APaintInfo.WindowWidth),
            APaintInfo.GetScaleY(APaintInfo.WindowHeight), @vPt);
        end;
      end
      else
      begin
        FTableToolBar.PaintTo(ACanvas, FActiveItemRect.Left - FTableToolBar.Width + FToolOffset,
          FActiveItemRect.Top);  // + FToolOffset - FTableToolBar.Height
      end;
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
    CancelActiveToolItem;
end;

procedure THCViewTool.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not Self.ReadOnly then
  begin
    if FTableToolBar.Visible and TableKeyDown(Key, Shift) then Exit;
    if FImageToolBar.Visible and ImageKeyDown(Key, Shift) then Exit;
  end;

  Self.BeginUpdate;
  try
    inherited KeyDown(Key, Shift);  // 删除Item时会触发ToolBar隐藏，重绘时环境还没有准备好
  finally
    Self.EndUpdate;
  end;
end;

procedure THCViewTool.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

end;

procedure THCViewTool.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure THCViewTool.LoadImageList;
var
  vBmp: TBitmap;
begin
  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'ARROW');  // 箭头0
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'LINE');  // 直线1
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'RECT');  // 矩形2
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'ES');  // 椭圆3
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'DBX');  // 多边形4
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'CUS');  // 自由线条5
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'FH');  // 缝合线6
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'TEXT');  // 文本7
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'COLOR');  // 颜色8
  FToolImageList.AddMasked(vBmp, $FF00FF);

  vBmp := TBitmap.Create;
  FIconList.Add(vBmp);
  vBmp.LoadFromResourceName(HInstance, 'TABLE');  // 表格9
  FToolImageList.AddMasked(vBmp, $FF00FF);
end;

procedure THCViewTool.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vData: THCCustomData;
  vTopItem: THCCustomItem;
begin
  FCaptureToolBar := nil;

  if not Self.ReadOnly then
  begin
    if FUseTableTool and FTableToolBar.Visible and TableMouseDown(Button, Shift, X, Y) then
    begin
      FCaptureToolBar := FTableToolBar;
      Exit;
    end;

    if FUseImageTool and FImageToolBar.Visible and ImageMouseDown(Button, Shift, X, Y) then
    begin
      FCaptureToolBar := FImageToolBar;
      Exit;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  FTopData := Self.ActiveSectionTopLevelData;
  vTopItem := FTopData.GetActiveItem;
  SetActiveToolItem(vTopItem);
  while not Assigned(FActiveItem) do
  begin
    vData := FTopData.GetRootData;
    if vData = FTopData then
      Break;

    FTopData := vData;
    vTopItem := FTopData.GetActiveItem;
    SetActiveToolItem(vTopItem);
  end;
end;

procedure THCViewTool.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not Self.ReadOnly then
  begin
    FMouseViewPt.X := ZoomOut(X);
    FMouseViewPt.Y := ZoomOut(Y);

    if FCaptureToolBar = FTableToolBar then
    begin
      TableMouseMove(Shift, X, Y);
      Exit;
    end
    else
    if FCaptureToolBar = FImageToolBar then
    begin
      ImageMouseMove(Shift, X, Y);
      Exit;
    end;

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
    if FCaptureToolBar = FTableToolBar then
    begin
      FCaptureToolBar := nil;
      TableMouseUp(Button, Shift, X, Y);
      Exit;
    end
    else
    if FCaptureToolBar = FImageToolBar then
    begin
      FCaptureToolBar := nil;
      ImageMouseUp(Button, Shift, X, Y);
      Exit;
    end;

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

procedure THCViewTool.SetActiveToolItem(const AItem: THCCustomItem);
var
  vPt: TPoint;
begin
  // MouseDown里会触发重绘，此时ToolBar并未确定显示，处理ToolBar的Visible属性
  // 会重新触发重绘，重绘是通过DoImageToolBarUpdateView(Rect)，需要先计算区域参数
  // 然后触发UpdateView，所以需要提前计算ToolBar的坐标vPt位置

  if FActiveItem <> AItem then
  begin
    if FUseTableTool and (FActiveItem is THCTableItem) then
      FTableToolBar.Visible := False
    else
    if FUseImageTool and (FActiveItem is THCImageItem) then
      FImageToolBar.Visible := False;

    if Assigned(AItem) and AItem.Active then
    begin
      vPt := Self.GetTopLevelRectDrawItemViewCoord;
      if FUseTableTool and (AItem is THCTableItem) and (FTableToolBar.Controls.Count > 0) then
      begin
        FActiveItem := AItem;

        FTableToolBar.Left := vPt.X - FTableToolBar.Width + FToolOffset;
        FTableToolBar.Top := vPt.Y;// - FTableToolBar.Height + FToolOffset;
        FTableToolBar.Visible := True;  // False 不使用该功能时不显示
      end
      else
      if FUseImageTool and (AItem is THCImageItem) and (FImageToolBar.Controls.Count > 0) then
      begin
        FActiveItem := AItem;
        (FActiveItem as THCImageItem).ShapeManager.OnStructOver := DoImageShapeStructOver;
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
    if FUseTableTool and (FActiveItem is THCTableItem) then
      FTableToolBar.Visible := False
    else
    if FUseImageTool and (FActiveItem is THCImageItem) then
      FImageToolBar.Visible := False;

    FActiveItem := nil;
  end;
end;

procedure THCViewTool.SetUseImageTool(const Value: Boolean);
begin
  if FUseImageTool <> Value then
  begin
    FUseImageTool := Value;
    if not Value then
      FImageToolBar.Visible := False;
  end;
end;

procedure THCViewTool.SetUseTableTool(const Value: Boolean);
begin
  if FUseTableTool <> Value then
  begin
    FUseTableTool := Value;
    if not Value then
      FTableToolBar.Visible := False;
  end;
end;

function THCViewTool.TableKeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function THCViewTool.TableMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
//var
//  vTableItem: THCTableItem;
begin
  Result := False;
  if PtInTableToolBar(X, Y) then  // 鼠标在表格编辑工具条上
  begin
    FTableToolBar.MouseDown(Button, Shift, X - FTableToolBar.Left, Y - FTableToolBar.Top);
    Result := True;
  end;
  {else
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
  end;}
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
      Cursor := crDefault;
    end;
  end
  else
  if FHotToolBar = FTableToolBar then
  begin
    FTableToolBar.MouseLeave;
    FHotToolBar := nil;
  end
  {else
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
  end;}
end;

function THCViewTool.TableMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vPt: TPoint;
begin
  Result := False;
  if PtInTableToolBar(X, Y) then
  begin
    if FTableToolBar.ActiveIndex = 0 then
    begin
      FTableToolBar.ActiveIndex := -1;
      vPt.X := FTableToolBar.Left;
      vPt.Y := FTableToolBar.Top + FTableToolBar.Height;
      vPt := ClientToScreen(vPt);
      FTableToolMenu.Popup(vPt.X, vPt.Y);
    end
    else
    begin
      FTableToolBar.MouseUp(Button, Shift, X - FTableToolBar.Left, Y - FTableToolBar.Top);
      FTableToolBar.ActiveIndex := -1;
    end;

    Result := True;
  end
  else
    FTableToolBar.ActiveIndex := -1;
  {//if FTableToolBar.ActiveIndex > 0 then  // 第一个是指针
  begin
    vTableItem := FActiveItem as THCTableItem;
    if PtInRect(Bounds(FActiveItemRect.Left, FActiveItemRect.Top,
                       vTableItem.Width, vTableItem.Height),
                Point(FMouseViewPt.X, FMouseViewPt.Y))
    then  // 在表格上操作
    begin
      Result := True;
    end;
  end;}
end;

end.
