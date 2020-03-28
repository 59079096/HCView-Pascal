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
    Position: TPoint;
    property Checked: Boolean read FChecked write SetChecked;
    property OnSetChecked: TNotifyEvent read FOnSetChecked write FOnSetChecked;
  end;

  THCRadioGroup = class(THCControlItem)
  private
    FMultSelect, FMouseIn: Boolean;
    FItems: TObjectList<THCRadioButton>;
    FRadioStyle: THCRadioStyle;
    FItemHit: Boolean;
    function GetItemAt(const X, Y: Integer): Integer;
  protected
    procedure DoItemNotify(Sender: TObject; const Item: THCRadioButton; Action: TCollectionNotification);
    procedure DoItemSetChecked(Sender: TObject);
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    function GetOffsetAt(const X: Integer): Integer; override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure AddItem(const AText: string; const ATextValue: string = '';
      const AChecked: Boolean = False);

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property MultSelect: Boolean read FMultSelect write FMultSelect;
    property RadioStyle: THCRadioStyle read FRadioStyle write FRadioStyle;
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

constructor THCRadioGroup.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.RadioGroup;
  Width := 100;
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
  i: Integer;
  vPoint: TPoint;
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if FMouseIn then
  begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FillRect(ADrawRect);
  end;

  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);

  for i := 0 to FItems.Count - 1 do
  begin
    vPoint.X := FItems[i].Position.X;
    vPoint.Y := FItems[i].Position.Y;
    vPoint.Offset(ADrawRect.Left, ADrawRect.Top);

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

    ACanvas.TextOut(vPoint.X + RadioButtonWidth, vPoint.Y, FItems[i].Text);
  end;
end;

procedure THCRadioGroup.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vSize: TSize;
  i, vLeft, vTop: Integer;
begin
  Height := FMinHeight;

  ARichData.Style.ApplyTempStyle(TextStyleNo);

  vLeft := FPaddingLeft;
  vTop := FPaddingTop;
  for i := 0 to FItems.Count - 1 do
  begin
    if FItems[i].Text <> '' then
      vSize := ARichData.Style.TempCanvas.TextExtent(FItems[i].Text)
    else
      vSize := ARichData.Style.TempCanvas.TextExtent('H');

    if Self.AutoSize and (vLeft + vSize.cx + RadioButtonWidth > Width) then
    begin
      vLeft := FPaddingLeft;
      vTop := vTop + vSize.cy + FPaddingBottom;
    end;

    FItems[i].Position.X := vLeft;
    FItems[i].Position.Y := vTop;

    vLeft := vLeft + RadioButtonWidth + vSize.cx + FPaddingRight;
  end;

  if Self.AutoSize then
    Width := vLeft;

  Height := vTop + vSize.cy + FPaddingBottom;

  if Width < FMinWidth then
    Width := FMinWidth;
  if Height < FMinHeight then
    Height := FMinHeight;
end;

procedure THCRadioGroup.GetCaretInfo(var ACaretInfo: THCCaretInfo);
begin
  if Self.Active then
    ACaretInfo.Visible := False;
end;

function THCRadioGroup.GetItemAt(const X, Y: Integer): Integer;
var
  i: Integer;
  vSize: TSize;
begin
  Result := -1;

  if FItemHit then
    Self.OwnerData.Style.ApplyTempStyle(TextStyleNo);

  for i := 0 to FItems.Count - 1 do
  begin
    if FItemHit then  // 文本就命中
    begin
      vSize := Self.OwnerData.Style.TempCanvas.TextExtent(FItems[i].Text);
      if PtInRect(Bounds(FItems[i].Position.X, FItems[i].Position.Y,
        RadioButtonWidth + vSize.cx, vSize.cy), Point(X, Y))
      then
      begin
        Result := i;
        Break;
      end;
    end
    else  // 只在box命中
    begin
      if PtInRect(Bounds(FItems[i].Position.X, FItems[i].Position.Y,
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

procedure THCRadioGroup.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  i: Word;
  vS, vText: string;
  vP, vPStart: PChar;
  vBool: Boolean;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  FItems.Clear;
  // 读Items
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
      FItems[vIndex].Checked := not FItems[vIndex].Checked;
  end;
end;

procedure THCRadioGroup.MouseEnter;
begin
  inherited MouseEnter;
  FMouseIn := True;
end;

procedure THCRadioGroup.MouseLeave;
begin
  inherited MouseLeave;
  FMouseIn := False;
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
  FItems.Clear;
  vList := TStringList.Create;
  try
    // Items文本内容
    vList.DelimitedText := ANode.Attributes['item'];
    for i := 0 to vList.Count - 1 do
      AddItem(vList[i]);

    if ANode.HasAttribute('itemvalue') then
    begin
      vList.DelimitedText := ANode.Attributes['itemvalue'];
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
end;

procedure THCRadioGroup.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  i: Integer;
  vTexts, vTextValues: string;
begin
  inherited SaveToStream(AStream, AStart, AEnd);
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

procedure THCRadioGroup.ToXml(const ANode: IHCXMLNode);
var
  vText, vTextValue: string;
  i: Integer;
begin
  inherited ToXml(ANode);

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
