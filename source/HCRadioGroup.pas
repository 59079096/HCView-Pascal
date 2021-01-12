{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-9-15             }
{                                                       }
{             文档RadioGroup对象实现单元                }
{                                                       }
{*******************************************************}

unit HCRadioGroup;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Generics.Collections, HCItem,
  HCRectItem, HCStyle, HCCustomData, HCCommon, HCXml;

type
  THCRadioStyle = (Radio, CheckBox);

  THCRadioButton = class(TObject)
  private
    FChecked: Boolean;
    FOnSetChecked: TNotifyEvent;
    procedure SetChecked(const Value: Boolean);
  public
    Text, TextValue: string;
    Rect: TRect;
    property Checked: Boolean read FChecked write SetChecked;
    property OnSetChecked: TNotifyEvent read FOnSetChecked write FOnSetChecked;
  end;

  THCRadioGroup = class(THCControlItem)
  private
    FMultSelect: Boolean;
    FItems: TObjectList<THCRadioButton>;
    FRadioStyle: THCRadioStyle;
    FItemHit: Boolean;
    FColumns,  // 大于0的整数时列属性有效，0用于兼容旧版本的不分列
      FBatchCount: Byte;

    FColumnAlign: Boolean;  // 列对齐
    procedure ReLayout;
    function GetItemAt(const X, Y: Integer): Integer;
    procedure SetColumns(const Value: Byte);
    procedure SetColumnAlign(const Value: Boolean);
  protected
    procedure DoSetItemChecked(const AIndex: Integer; const Value: Boolean); virtual;
    procedure DoItemNotify(Sender: TObject; const Item: THCRadioButton; Action: TCollectionNotification);
    procedure DoItemSetChecked(Sender: TObject);
    procedure DoPaintItems(const ACanvas: TCanvas; const ADrawRect: TRect; const APaintInfo: TPaintInfo);
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function GetOffsetAt(const X: Integer): Integer; override;
    function GetText: string; override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure BeginAdd;
    procedure EndAdd;
    procedure AddItem(const AText: string; const ATextValue: string = '';
      const AChecked: Boolean = False);

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property MultSelect: Boolean read FMultSelect write FMultSelect;
    property RadioStyle: THCRadioStyle read FRadioStyle write FRadioStyle;
    property ItemHit: Boolean read FItemHit write FItemHit;
    property Columns: Byte read FColumns write SetColumns;
    property ColumnAlign: Boolean read FColumnAlign write SetColumnAlign;
    property Items: TObjectList<THCRadioButton> read FItems;
  end;

implementation

const
  RadioButtonWidth = 16;

{ THCRadioGroup }

procedure THCRadioGroup.AddItem(const AText: string; const ATextValue: string = '';
  const AChecked: Boolean = False);
var
  vRadioButton: THCRadioButton;
begin
  vRadioButton := THCRadioButton.Create;
  vRadioButton.Checked := AChecked;
  vRadioButton.Text := AText;
  vRadioButton.TextValue := ATextValue;
  FItems.Add(vRadioButton);
end;

procedure THCRadioGroup.Assign(Source: THCCustomItem);
var
  i: Integer;
  vSource: THCRadioGroup;
begin
  inherited Assign(Source);
  vSource := Source as THCRadioGroup;

  FItems.Clear;
  for i := 0 to vSource.Items.Count - 1 do
    AddItem(vSource.Items[i].Text, vSource.Items[i].TextValue, vSource.Items[i].Checked);
end;

procedure THCRadioGroup.BeginAdd;
begin
  Inc(FBatchCount);
end;

constructor THCRadioGroup.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.RadioGroup;
  Width := 100;
  FBatchCount := 0;
  FColumns := 0;  // 0兼容旧版本
  FColumnAlign := True;
  FItemHit := False;
  FItems := TObjectList<THCRadioButton>.Create;
  FItems.OnNotify := DoItemNotify;
  FRadioStyle := THCRadioStyle.Radio;
end;

destructor THCRadioGroup.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure THCRadioGroup.DoItemNotify(Sender: TObject;
  const Item: THCRadioButton; Action: TCollectionNotification);
begin
  if Action = TCollectionNotification.cnAdded then
    Item.OnSetChecked := DoItemSetChecked;

  if Action <> TCollectionNotification.cnExtracted then
    ReLayout;
end;

procedure THCRadioGroup.DoItemSetChecked(Sender: TObject);
var
  i, vIndex: Integer;
begin
  if (not FMultSelect) and THCRadioButton(Sender).Checked then
  begin
    vIndex := FItems.IndexOf(THCRadioButton(Sender));
    for i := 0 to FItems.Count - 1 do
    begin
      if i <> vIndex then
        FItems[i].Checked := False;
    end;
  end;
end;

procedure THCRadioGroup.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  vPaintRegion: HRGN;
  vClipBoxRect: TRect;
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if APaintInfo.Print then

  else
  if Self.IsSelectComplate then
  begin
    ACanvas.Brush.Color := AStyle.SelColor;
    ACanvas.FillRect(ADrawRect);
  end
  else
  if FMouseIn then
  begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FillRect(ADrawRect);
  end;

  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);
  if not AutoSize then
  begin
    GetClipBox(ACanvas.Handle, vClipBoxRect);  // 保存当前的绘图区域

    vPaintRegion := CreateRectRgn(
      APaintInfo.GetScaleX(ADrawRect.Left),
      APaintInfo.GetScaleY(ADrawRect.Top),
      APaintInfo.GetScaleX(ADrawRect.Right),
      APaintInfo.GetScaleY(ADrawRect.Bottom));
    try
      SelectClipRgn(ACanvas.Handle, vPaintRegion);
      DoPaintItems(ACanvas, ADrawRect, APaintInfo);
    finally
      DeleteObject(vPaintRegion);
    end;

    //vPaintRegion := CreateRectRgnIndirect(vClipBoxRect);
    vPaintRegion := CreateRectRgn(
      APaintInfo.GetScaleX(vClipBoxRect.Left),
      APaintInfo.GetScaleY(vClipBoxRect.Top),
      APaintInfo.GetScaleX(vClipBoxRect.Right),
      APaintInfo.GetScaleY(vClipBoxRect.Bottom));
    try
      SelectClipRgn(ACanvas.Handle, vPaintRegion);
    finally
      DeleteObject(vPaintRegion);
    end;
  end
  else
    DoPaintItems(ACanvas, ADrawRect, APaintInfo);
end;

procedure THCRadioGroup.DoPaintItems(const ACanvas: TCanvas; const ADrawRect: TRect; const APaintInfo: TPaintInfo);
var
  i: Integer;
  vPoint: TPoint;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    vPoint.X := FItems[i].Rect.Left;
    vPoint.Y := FItems[i].Rect.Top;
    vPoint.Offset(ADrawRect.Left, ADrawRect.Top);

    if APaintInfo.Print then
    begin
      if FItems[i].Checked then
      begin
        if FRadioStyle = THCRadioStyle.Radio then
        begin
          HCDrawFrameControl(ACanvas, Bounds(vPoint.X, vPoint.Y, RadioButtonWidth, RadioButtonWidth),
            THCControlState.hcsChecked, THCControlStyle.hcyRadio);
        end
        else
        begin
          HCDrawFrameControl(ACanvas, Bounds(vPoint.X, vPoint.Y, RadioButtonWidth, RadioButtonWidth),
            THCControlState.hcsChecked, THCControlStyle.hcyCheck);
        end;
      end
      else
      begin
        if FRadioStyle = THCRadioStyle.Radio then
        begin
          HCDrawFrameControl(ACanvas, Bounds(vPoint.X, vPoint.Y, RadioButtonWidth, RadioButtonWidth),
            THCControlState.hcsCustom, THCControlStyle.hcyRadio);
        end
        else
        begin
          HCDrawFrameControl(ACanvas, Bounds(vPoint.X, vPoint.Y, RadioButtonWidth, RadioButtonWidth),
            THCControlState.hcsCustom, THCControlStyle.hcyCheck);
        end;
      end;

      ACanvas.Brush.Style := bsClear;
    end
    else
    begin
      if FItems[i].Checked then
      begin
        if FRadioStyle = THCRadioStyle.Radio then
        begin
          DrawFrameControl(ACanvas.Handle, Bounds(vPoint.X, vPoint.Y, RadioButtonWidth, RadioButtonWidth),
            DFC_BUTTON, DFCS_CHECKED or DFCS_BUTTONRADIO);
        end
        else
        begin
          DrawFrameControl(ACanvas.Handle, Bounds(vPoint.X, vPoint.Y, RadioButtonWidth, RadioButtonWidth),
            DFC_BUTTON, DFCS_CHECKED or DFCS_BUTTONCHECK);
        end;
      end
      else
      begin
        if FRadioStyle = THCRadioStyle.Radio then
        begin
          DrawFrameControl(ACanvas.Handle, Bounds(vPoint.X, vPoint.Y, RadioButtonWidth, RadioButtonWidth),
            DFC_BUTTON, DFCS_BUTTONRADIO);
        end
        else
        begin
          DrawFrameControl(ACanvas.Handle, Bounds(vPoint.X, vPoint.Y, RadioButtonWidth, RadioButtonWidth),
            DFC_BUTTON, DFCS_BUTTONCHECK);
        end;
      end;
    end;

    ACanvas.TextOut(vPoint.X + RadioButtonWidth, vPoint.Y, FItems[i].Text);
  end;
end;

procedure THCRadioGroup.DoSetItemChecked(const AIndex: Integer; const Value: Boolean);
begin
  FItems[AIndex].Checked := Value;
  Self.DoChange;
end;

procedure THCRadioGroup.EndAdd;
begin
  if FBatchCount > 0 then
    Dec(FBatchCount);

  if FBatchCount = 0 then
    ReLayout;
end;

procedure THCRadioGroup.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
begin
  if Width < FMinWidth then
    Width := FMinWidth;
  if Height < FMinHeight then
    Height := FMinHeight;
end;

function THCRadioGroup.GetItemAt(const X, Y: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FItems.Count - 1 do
  begin
    if FItemHit then  // 文本就命中
    begin
      if PtInRect(FItems[i].Rect, Point(X, Y)) then
      begin
        Result := i;
        Break;
      end;
    end
    else  // 只在box命中
    begin
      if PtInRect(Bounds(FItems[i].Rect.Left, FItems[i].Rect.Top,
        RadioButtonWidth, RadioButtonWidth), Point(x, y))
      then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function THCRadioGroup.GetOffsetAt(const X: Integer): Integer;
begin
  if X <= FPaddingLeft then
    Result := OffsetBefor
  else
  if X >= Width - FPaddingRight then
    Result := OffsetAfter
  else
    Result := OffsetInner;
end;

function THCRadioGroup.GetText: string;
var
  i: Integer;
begin
  Result := inherited GetText;
  for i := 0 to FItems.Count - 1 do
  begin
    if FItems[i].Checked then
    begin
      if Result <> '' then
        Result := Result + '，' + FItems[i].Text
      else
        Result := Result + FItems[i].Text;
    end;
  end;
end;

procedure THCRadioGroup.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  i: Word;
  vS, vText: string;
  vP, vPStart: PChar;
  vBool: Boolean;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  BeginAdd;
  try
    if AFileVersion > 39 then
    begin
      AStream.ReadBuffer(FColumns, SizeOf(FColumns));
      AStream.ReadBuffer(vByte, SizeOf(vByte));
      FMultSelect := Odd(vByte shr 7);
      FItemHit := Odd(vByte shr 6);
      FColumnAlign := Odd(vByte shr 5);
    end;

    // 读Items
    FItems.Clear;
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    if vS <> '' then
    begin
      vP := PChar(vS);

      while vP^ <> #0 do
      begin
        vPStart := vP;
        while not (vP^ in [#0, #10, #13]) do
          Inc(vP);

        SetString(vText, vPStart, vP - vPStart);
        AddItem(vText);

        if vP^ = #13 then
          Inc(vP);
        if vP^ = #10 then
          Inc(vP);
      end;

      if AFileVersion > 35 then
      begin
        i := 0;
        HCLoadTextFromStream(AStream, vS, AFileVersion);
        if vS <> '' then
        begin
          vP := PChar(vS);

          while vP^ <> #0 do
          begin
            vPStart := vP;
            while not (vP^ in [#0, #10, #13]) do
              Inc(vP);

            SetString(vText, vPStart, vP - vPStart);
            Fitems[i].TextValue := vText;
            Inc(i);

            if vP^ = #13 then
              Inc(vP);
            if vP^ = #10 then
              Inc(vP);
          end;
        end;
      end;

      for i := 0 to FItems.Count - 1 do
      begin
        AStream.ReadBuffer(vBool, SizeOf(vBool));
        Fitems[i].Checked := vBool;
      end;
    end;

    if AFileVersion > 33 then
      AStream.ReadBuffer(FRadioStyle, SizeOf(FRadioStyle));
  finally
    EndAdd;
  end;
end;

function THCRadioGroup.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  vIndex: Integer;
begin
  Result := inherited MouseDown(Button, Shift, X, Y);
  if OwnerData.CanEdit and (Button = mbLeft) then
  begin
    vIndex := GetItemAt(X, Y);
    if vIndex >= 0 then
      DoSetItemChecked(vIndex, not FItems[vIndex].Checked);
  end;
end;

function THCRadioGroup.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  GCursor := crDefault;
end;

procedure THCRadioGroup.ParseXml(const ANode: IHCXMLNode);
var
  vList: TStringList;
  i: Integer;
begin
  inherited ParseXml(ANode);

  BeginAdd;
  try
    if ANode.HasAttribute('col') then
      FColumns := ANode.Attributes['col'];

    if ANode.HasAttribute('multsel') then
      FMultSelect := ANode.Attributes['multsel'];

    if ANode.HasAttribute('itemhit') then
      FItemHit := ANode.Attributes['itemhit'];

    if ANode.HasAttribute('colalign') then
      FColumnAlign := ANode.Attributes['colalign'];

    FItems.Clear;
    vList := TStringList.Create;
    try
      // Items文本内容
      vList.DelimitedText := GetXmlRN(ANode.Attributes['item']);
      for i := 0 to vList.Count - 1 do
        AddItem(vList[i]);

      if ANode.HasAttribute('itemvalue') then
      begin
        DelimitedXMLRN(ANode.Attributes['itemvalue'], vList);
        for i := 0 to FItems.Count - 1 do
          FItems[i].TextValue := vList[i];
      end;

      // Items选中状态
      vList.DelimitedText := ANode.Attributes['check'];
      for i := 0 to vList.Count - 1 do
        Fitems[i].Checked := StrToBool(vList[i]);
    finally
      FreeAndNil(vList);
    end;

    FRadioStyle := THCRadioStyle(ANode.Attributes['radiostyle']);
  finally
    EndAdd;
  end;
end;

procedure THCRadioGroup.ReLayout;
var
  vSize: TSize;
  i, vLeft, vTop, vCol, vWMax, vColumnAct: Integer;
begin
  if FBatchCount > 0 then Exit;
  if not Assigned(FItems) then Exit;

  OwnerData.Style.ApplyTempStyle(TextStyleNo);

  vLeft := FPaddingLeft;
  vTop := FPaddingTop;
  if FColumns = 0 then  // 旧版本不分列
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      if FItems[i].Text <> '' then
        vSize := OwnerData.Style.TempCanvas.TextExtent(FItems[i].Text)
      else
        vSize := OwnerData.Style.TempCanvas.TextExtent('H');

      if Self.AutoSize and (vLeft + vSize.cx + RadioButtonWidth > Width) then
      begin
        vLeft := FPaddingLeft;
        vTop := vTop + vSize.cy + FPaddingBottom;
      end;

      FItems[i].Rect := Bounds(vLeft, vTop, RadioButtonWidth + vSize.cx, vSize.cy);
      vLeft := vLeft + RadioButtonWidth + vSize.cx + FPaddingRight;
    end;

    if Self.AutoSize then
      Width := vLeft;

    Height := vTop + vSize.cy + FPaddingBottom;
  end
  else  // 分列
  begin
    vWMax := 0;
    vSize.cy := 0;  // 防止一项也没有
    vCol := 1;
    if FColumns > FItems.Count then
      vColumnAct := FItems.Count
    else
      vColumnAct := FColumns;

    for i := 0 to FItems.Count - 1 do  // 按列排列
    begin
      if FItems[i].Text <> '' then
        vSize := OwnerData.Style.TempCanvas.TextExtent(FItems[i].Text)
      else
        vSize := OwnerData.Style.TempCanvas.TextExtent('H');

      FItems[i].Rect := Bounds(vLeft, vTop, RadioButtonWidth + vSize.cx, vSize.cy);
      vLeft := vLeft + RadioButtonWidth + vSize.cx + FPaddingRight;

      if vCol = vColumnAct then
      begin
        if vLeft > vWMax then
          vWMax := vLeft;

        if i < FItems.Count - 1 then
        begin
          vCol := 1;
          vLeft := FPaddingLeft;
          vTop := vTop + vSize.cy + FPaddingBottom;
        end;
      end
      else
        Inc(vCol);
    end;

    Height := vTop + vSize.cy + FPaddingBottom;

    if FColumnAlign then  // 列对齐
    begin
      for i := 0 to vColumnAct - 2 do  // 每一列最宽的决定下一列的起始
      begin
        // 取第i列最宽的
        vCol := i;  // 第1行第x个Item
        vWMax := FItems[vCol].Rect.Right;
        while vCol + vColumnAct < FItems.Count do
        begin
          vCol := vCol + vColumnAct;
          if vWMax < FItems[vCol].Rect.Right then
            vWMax := FItems[vCol].Rect.Right;
        end;

        vWMax := vWMax + FPaddingRight;
        // 计算第i+1列的起始位置
        vCol := i + 1;
        OffsetRect(FItems[vCol].Rect, vWMax - FItems[vCol].Rect.Left, 0);
        while vCol + vColumnAct < FItems.Count do
        begin
          vCol := vCol + vColumnAct;
          OffsetRect(FItems[vCol].Rect, vWMax - FItems[vCol].Rect.Left, 0);
        end;
      end;

      if AutoSize then  // 最后一列最右侧
      begin
        vCol := vColumnAct - 1;
        vWMax := FItems[vCol].Rect.Right;
        while vCol + vColumnAct < FItems.Count do
        begin
          vCol := vCol + vColumnAct;
          if vWMax < FItems[vCol].Rect.Right then
            vWMax := FItems[vCol].Rect.Right;
        end;

        Width := vWMax + FPaddingRight;
      end;
    end
    else  // 列不对齐
    if AutoSize then
      Width := vWMax;
  end;
end;

procedure THCRadioGroup.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  i: Integer;
  vByte: Byte;
  vTexts, vTextValues: string;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  AStream.WriteBuffer(FColumns, SizeOf(FColumns));
  vByte := 0;
  if FMultSelect then
    vByte := vByte or (1 shl 7);

  if FItemHit then
    vByte := vByte or (1 shl 6);

  if FColumnAlign then
    vByte := vByte or (1 shl 5);

  AStream.WriteBuffer(vByte, SizeOf(vByte));

  // 存Items
  if FItems.Count > 0 then
  begin
    vTexts := FItems[0].Text;
    vTextValues := FItems[0].TextValue;
    for i := 1 to FItems.Count - 1 do
    begin
      vTexts := vTexts + sLineBreak + FItems[i].Text;
      vTextValues := vTextValues + sLineBreak + FItems[i].TextValue;
    end;
  end
  else
  begin
    vTexts := '';
    vTextValues := '';
  end;

  HCSaveTextToStream(AStream, vTexts);
  HCSaveTextToStream(AStream, vTextValues);

  for i := 0 to FItems.Count - 1 do
    AStream.WriteBuffer(FItems[i].Checked, SizeOf(Boolean));

  AStream.WriteBuffer(FRadiostyle, SizeOf(FRadioStyle));
end;

procedure THCRadioGroup.SetColumnAlign(const Value: Boolean);
begin
  if FColumnAlign <> Value then
  begin
    FColumnAlign := Value;
    ReLayout;
  end;
end;

procedure THCRadioGroup.SetColumns(const Value: Byte);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    ReLayout;
  end;
end;

procedure THCRadioGroup.ToXml(const ANode: IHCXMLNode);
var
  vText, vTextValue: string;
  i: Integer;
begin
  inherited ToXml(ANode);

  ANode.Attributes['col'] := FColumns;
  if FMultSelect then
    ANode.Attributes['multsel'] := '1';

  if FItemHit then
    ANode.Attributes['itemhit'] := '1';

  if FColumnAlign then
    ANode.Attributes['colalign'] := '1';

  // 存Items文本内容
  if FItems.Count > 0 then
  begin
    vText := FItems[0].Text;
    vTextValue := FItems[0].TextValue;
    for i := 1 to FItems.Count - 1 do
    begin
      vText := vText + sLineBreak + FItems[i].Text;
      vTextValue := vTextValue + sLineBreak + FItems[i].TextValue;
    end;
  end
  else
  begin
    vText := '';
    vTextValue := '';
  end;

  ANode.Attributes['item'] := vText;
  ANode.Attributes['itemvalue'] := vTextValue;

  // 存Items选中状态
  if FItems.Count > 0 then
  begin
    vText := HCBoolText[FItems[0].Checked];
    for i := 1 to FItems.Count - 1 do
      vText := vText + sLineBreak + HCBoolText[FItems[i].Checked];
  end
  else
    vText := '';

  ANode.Attributes['check'] := vText;
  ANode.Attributes['radiostyle'] := Ord(FRadioStyle);
end;

{ THCRadioButton }

procedure THCRadioButton.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if Assigned(FOnSetChecked) then
      FOnSetChecked(Self);
  end;
end;

end.
