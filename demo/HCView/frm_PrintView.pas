unit frm_PrintView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Printers, HCView, HCSection, HCItem, ComCtrls, Buttons;

type
  TfrmPrintView = class(TForm)
    pnl1: TPanel;
    scrlbrPage: TScrollBar;
    pbPage: TPaintBox;
    btnPrint: TButton;
    pnl2: TPanel;
    pnl3: TPanel;
    edtPageNo: TEdit;
    lblPageCount: TLabel;
    cbbPrinter: TComboBox;
    edtCopies: TEdit;
    cbbPage: TComboBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    ud1: TUpDown;
    edtPrintPageNos: TEdit;
    cbbZoom: TComboBox;
    lbl4: TLabel;
    procedure btnPrintClick(Sender: TObject);
    procedure pbPagePaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure scrlbrPageChange(Sender: TObject);
    procedure pbPageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtPageNoKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure ud1Click(Sender: TObject; Button: TUDBtnType);
    procedure cbbPageChange(Sender: TObject);
    procedure edtCopiesKeyPress(Sender: TObject; var Key: Char);
    procedure edtCopiesExit(Sender: TObject);
    procedure edtPageNoExit(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure cbbZoomChange(Sender: TObject);
  private
    { Private declarations }
    FPageIndex, FDrawLeft, FDrawTop, FDrawWidth, FDrawHeight: Integer;
    FHCView: THCView;
    FBitmap: TBitmap;
    procedure UpdateView;
  public
    { Public declarations }
    procedure SetView(const AHCView: THCView);
  end;

implementation

uses
  CommCtrl, Math, HCCommon, HCUnitConversion;

{$R *.dfm}

var
  FTooltipHandle: Cardinal = 0;
  FToolInfo: TToolInfo;
  FBuffer: array[0..255] of Char;

procedure CreateToolTips(hWnd: Cardinal);
begin
  if FTooltipHandle = 0 then
  begin
    FTooltipHandle := CreateWindowEx(0, 'Tooltips_Class32', nil, TTS_ALWAYSTIP or TTS_BALLOON,
      Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),
      Integer(CW_USEDEFAULT), hWnd, 0, hInstance, nil);

    if FTooltipHandle <> 0 then
    begin
      SetWindowPos(FTooltipHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
      FToolInfo.cbSize := SizeOf(TToolInfo);
      FToolInfo.uFlags := TTF_SUBCLASS or TTF_TRANSPARENT;
      FToolInfo.hInst := hInstance;
    end;
  end;
end;

//BackColor,TextColor�ֱ��Ǳ�����ɫ���ı���ɫ,�����0��ȡĬ��ֵ.
procedure AddToolTip(hwnd: DWORD; lpti: PToolInfo; IconType: Integer;
  Text, Title: PChar; BackColor, TextColor: TColor);
var
  vRect: TRect;
begin
  if (hwnd <> 0) and (GetClientRect(hwnd, vRect)) then
  begin
    lpti.hwnd := hwnd;
    lpti.Rect := vRect;
    lpti.lpszText := Text;
    SendMessage(FTooltipHandle, TTM_ADDTOOL, 0, Integer(lpti));
    FillChar(FBuffer, sizeof(FBuffer), #0);
    lstrcpy(FBuffer, Title);
    if (IconType > 3) or (IconType < 0) then
      IconType := 0;

    if BackColor <> 0 then
      SendMessage(FTooltipHandle, TTM_SETTIPBKCOLOR, BackColor, 0);

    if TextColor <> 0 then
      SendMessage(FTooltipHandle, TTM_SETTIPTEXTCOLOR, TextColor, 0);

    SendMessage(FTooltipHandle, TTM_SETTITLE, IconType, Integer(@FBuffer));
  end;
end;

{ TfrmPrintView }

procedure TfrmPrintView.btnPrintClick(Sender: TObject);
var
  vPages: array of Integer;

  procedure GetPages;
  var
    vLst1, vLst2: TStringList;
    i, j, vMin, vMax: Integer;
  begin
    vLst1 := TStringList.Create;
    vLst1.Delimiter := ',';
    try
      vLst2 := TStringList.Create;
      vLst2.Delimiter := '-';
      try
        vLst1.DelimitedText := edtPrintPageNos.Text;
        for i := 0 to vLst1.Count - 1 do
        begin
          vLst2.DelimitedText := vLst1[i];
          if vLst2.Count = 1 then  // ֻ��һ��ҳ��ֵ
          begin
            SetLength(vPages, Length(vPages) + 1);
            vPages[Length(vPages) - 1] := StrToInt(vLst2[0]) - 1;
          end
          else
          begin
            vMin := StrToInt(vLst2[0]);
            vMax := StrToInt(vLst2[vLst2.Count - 1]);
            if vMin > vMax then
            begin
              vMin := vMin + vMax;
              vMax := vMin - vMax;
              vMin := vMin - vMax;
            end;
            for j := vMin to vMax do
            begin
              SetLength(vPages, Length(vPages) + 1);
              vPages[Length(vPages) - 1] := j - 1;
            end;
          end;
        end;
      finally
        FreeAndNil(vLst2);
      end;
    finally
      FreeAndNil(vLst1);
    end;
  end;

  procedure GetPageRange(var AStar, AEnd: Integer);
  var
    vLst1: TStringList;
  begin
    vLst1 := TStringList.Create;
    try
      vLst1.Delimiter := '-';
      vLst1.DelimitedText := edtPrintPageNos.Text;
      AStar := StrToIntDef(vLst1[0], 1) - 1;
      AEnd := StrToIntDef(vLst1[1], FHCView.PageCount) - 1;
    finally
      FreeAndNil(vLst1);
    end;
  end;

var
  i, vRangeStar, vRangeEnd: Integer;
begin
  case cbbPage.ItemIndex of
    0: FHCView.Print(cbbPrinter.Text, StrToIntDef(edtCopies.Text, 1));  // ȫ��ҳ
    1: FHCView.Print(cbbPrinter.Text, FPageIndex, FPageIndex, StrToIntDef(edtCopies.Text, 1));  // ��ǰҳ
    2:  // ����ҳ
      begin
        if edtPrintPageNos.Text <> '' then  // ָ���˷�Χ
        begin
          GetPageRange(vRangeStar, vRangeEnd);
          for i := vRangeStar to vRangeEnd do
          begin
            if not Odd(i) then  // ż�����������ҳ
            begin
              SetLength(vPages, Length(vPages) + 1);
              vPages[Length(vPages) - 1] := i;
            end;
          end;

          FHCView.Print(cbbPrinter.Text, StrToIntDef(edtCopies.Text, 1), vPages);  // ����ҳ
        end
        else  // ȫ������ҳ
          FHCView.PrintOdd(cbbPrinter.Text);
      end;

    3:  // ż��ҳ
      begin
        if edtPrintPageNos.Text <> '' then  // ָ���˷�Χ
        begin
          GetPageRange(vRangeStar, vRangeEnd);
          for i := vRangeStar to vRangeEnd do
          begin
            if Odd(i) then  // ���������ż��ҳ
            begin
              SetLength(vPages, Length(vPages) + 1);
              vPages[Length(vPages) - 1] := i;
            end;
          end;

          FHCView.Print(cbbPrinter.Text, StrToIntDef(edtCopies.Text, 1), vPages);  // ż��ҳ
        end
        else  // ȫ��ż��ҳ
          FHCView.PrintEven(cbbPrinter.Text);
      end;
    4:
      begin
        GetPages;
        FHCView.Print(cbbPrinter.Text, StrToIntDef(edtCopies.Text, 1), vPages);  // �Զ���ҳ
      end;
  end;
end;

procedure TfrmPrintView.cbbPageChange(Sender: TObject);
begin
  edtPrintPageNos.Visible := False;

  if cbbPage.ItemIndex in [2, 3] then  // 2����ҳ 3ż��ҳ
  begin
    edtPrintPageNos.Clear;
    edtPrintPageNos.Visible := True;

    if cbbPage.ItemIndex = 2 then
      FToolInfo.lpszText := '���ô�ӡָ����Χ�ڵ�����ҳ���� 3-10 ��ӡ��3��5��7��9ҳ' + #13 + '�ձ�ʾ�����Ʒ�Χ'
    else
      FToolInfo.lpszText := '���ô�ӡָ����Χ�ڵ�ż����ҳ���� 2-7 ��ӡ��2��4��6ҳ' + #13 + '�ձ�ʾ�����Ʒ�Χ';

    SendMessage(FTooltipHandle, TTM_SETTOOLINFO, 0, Integer(@FToolInfo));
  end
  else
  if cbbPage.ItemIndex = cbbPage.Items.Count - 1 then // ���һ�����Զ���ҳ��
  begin
    edtPrintPageNos.Clear;
    edtPrintPageNos.Visible := True;

    FToolInfo.lpszText := '��ҳ��Ӣ�Ķ��š�,���ָ��磺1,4,6,8' + #13 +
      '����ҳ��Ӣ�ġ�-���ָ��磺2-5' + #13 + '���Ͽ�ͬʱʹ���磺1,3,5-7,9';

    SendMessage(FTooltipHandle, TTM_SETTOOLINFO, 0, Integer(@FToolInfo));
  end;
end;

procedure TfrmPrintView.cbbZoomChange(Sender: TObject);
begin
  UpdateView;
  pbPage.Invalidate;
end;

procedure TfrmPrintView.edtCopiesExit(Sender: TObject);
begin
  if StrToIntDef(edtCopies.Text, 0) = 0 then
    edtCopies.Text := '1'
  else
    edtCopies.Text := IntToStr(StrToInt(edtCopies.Text));
end;

procedure TfrmPrintView.edtCopiesKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = '0' then
  begin
    if StrToIntDef(edtCopies.Text, 0) = 0 then
      Key := #0;
  end
  else
  begin
    if not (Key in ['1'..'9', #8]) then
      Key := #0;
  end;
end;

procedure TfrmPrintView.edtPageNoExit(Sender: TObject);
var
  vChar: Char;
begin
  vChar := #13;
  edtPageNoKeyPress(Sender, vChar);
end;

procedure TfrmPrintView.edtPageNoKeyPress(Sender: TObject; var Key: Char);
var
  vPageIndex: Integer;
begin
  if Key = #13 then
  begin
    vPageIndex := StrToIntDef(edtPageNo.Text, 1) - 1;

    if vPageIndex < 0 then
      vPageIndex := 0
    else
    if vPageIndex > FHCView.PageCount - 1 then
      vPageIndex := FHCView.PageCount - 1;
      
    edtPageNo.Text := IntToStr(vPageIndex + 1);
    scrlbrPage.Position := vPageIndex;
  end
  else
  if not CharInSet(Key, ['0'..'9', #8]) then
    Key := #0;
end;

procedure TfrmPrintView.FormCreate(Sender: TObject);
begin
  FBitmap := TBitmap.Create;
  FPageIndex := -1;
end;

procedure TfrmPrintView.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBitmap);
end;

procedure TfrmPrintView.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if scrlbrPage.Focused then
  begin
    Handled := True;
    scrlbrPage.Position := scrlbrPage.Position + 1;
  end;
end;

procedure TfrmPrintView.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if scrlbrPage.Focused then
  begin
    Handled := True;
    scrlbrPage.Position := scrlbrPage.Position - 1;
  end;
end;

procedure TfrmPrintView.FormResize(Sender: TObject);
begin
  UpdateView;
  pbPage.Invalidate;
end;

procedure TfrmPrintView.FormShow(Sender: TObject);
begin
  Width := 850;
  Height := 810;
  cbbPrinter.Items.Assign(Printer.Printers);
  cbbPrinter.ItemIndex := Printer.PrinterIndex;

  CreateToolTips(edtPrintPageNos.Handle);
  AddToolTip(edtPrintPageNos.Handle, @FToolInfo, 1, '��ҳ��Ӣ�Ķ��š�,���ָ��磺1,4,6,8' + #13 +
    '����ҳ��Ӣ�ġ�-���ָ��磺2-5' + #13 +
    '���Ͽ�ͬʱʹ���磺1,3,5-7,9', '��ʾ', 0, 0);  //����1���Ը�Ϊ��������������ʾ��ͬ��ͼ��
end;

procedure TfrmPrintView.pbPageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if scrlbrPage.Visible then
    scrlbrPage.SetFocus;
end;

procedure TfrmPrintView.pbPagePaint(Sender: TObject);
begin
  pbPage.Canvas.Brush.Color := pbPage.Color;
  pbPage.Canvas.FillRect(Bounds(0, 0, pbPage.Width, pbPage.Height));
  pbPage.Canvas.Draw(FDrawLeft, FDrawTop, FBitmap);
end;

procedure TfrmPrintView.scrlbrPageChange(Sender: TObject);
begin
  FPageIndex := scrlbrPage.Position;
  edtPageNo.Text := IntToStr(FPageIndex + 1);
  UpdateView;
  pbPage.Invalidate;
end;

procedure TfrmPrintView.SetView(const AHCView: THCView);
begin
  FHCView := AHCView;
  scrlbrPage.Min := 0;
  if FHCView.PageCount > 1 then
    scrlbrPage.Max := FHCView.PageCount - 1
  else
    scrlbrPage.Visible := False;

  FPageIndex := 0;
  edtPageNo.Text := IntToStr(FPageIndex + 1);
  lblPageCount.Caption := '/ ' + IntToStr(FHCView.PageCount);

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin

  end;

  if FTooltipHandle <> 0 then
  begin
    DestroyWindow(FTooltipHandle);
    FTooltipHandle := 0;
  end;
end;

procedure TfrmPrintView.ud1Click(Sender: TObject; Button: TUDBtnType);
var
  vCopies: Integer;
begin
  vCopies := StrToIntDef(edtCopies.Text, 1);
  if Button = TUDBtnType.btNext then  // �ϣ�����
  begin
    Inc(vCopies);
    edtCopies.Text := IntToStr(vCopies);
  end
  else  // ���£�����
  begin
    if vCopies > 1 then
    begin
      Dec(vCopies);
      edtCopies.Text := IntToStr(vCopies);
    end;
  end;
end;

procedure TfrmPrintView.UpdateView;
var
  vSection, vSectionPageIndex: Integer;
  vPaintInfo: TSectionPaintInfo;
  vScaleInfo: TScaleInfo;
  vZoom: Single;
begin
  vZoom := StrToIntDef(cbbZoom.Text, 100) / 100;
  // ����ҳ���ȡ��ʼ�ںͽ�����
  vSection := FHCView.GetSectionPageIndexByPageIndex(FPageIndex, vSectionPageIndex);

  vPaintInfo := TSectionPaintInfo.Create;
  try
    vPaintInfo.Print := True;
    vPaintInfo.SectionIndex := vSection;

    FDrawWidth := Round(FHCView.Sections[vSection].PaperWidthPix * vZoom);
    FDrawHeight := Round(FHCView.Sections[vSection].PaperHeightPix * vZoom);

    FDrawTop := Max((pbPage.Height - FDrawHeight) div 2, 0);
    FDrawLeft := Max((pbPage.Width - FDrawWidth) div 2, 0);

    //vBL := FHCView.Sections[vSection].PageHeightPix / FHCView.Sections[vSection].PaperWidthPix;  // �ݺ��

    vPaintInfo.WindowWidth := FDrawWidth;
    vPaintInfo.WindowHeight := FDrawHeight;

//    if FHCView.Sections[vSection].Page.DataAnnotates.Count > 0 then
//    begin
//      vPaintInfo.Zoom := vZoom * FHCView.Sections[vSection].PaperWidthPix / (FHCView.Sections[vSection].PaperWidthPix + AnnotationWidth);
//      vPaintInfo.ScaleX := vPaintInfo.Zoom;
//      vPaintInfo.ScaleY := vPaintInfo.Zoom;
//    end
//    else
    begin
      vPaintInfo.ScaleX := vZoom;
      vPaintInfo.ScaleY := vZoom;
      vPaintInfo.Zoom := vZoom;
    end;

    FBitmap.SetSize(FDrawWidth, FDrawHeight);
    FBitmap.Canvas.Brush.Color := FHCView.Style.BackgroundColor;
    FBitmap.Canvas.Pen.Color := clBlack;
    FBitmap.Canvas.Rectangle(0, 0, FBitmap.Width, FBitmap.Height);

    vScaleInfo := vPaintInfo.ScaleCanvas(FBitmap.Canvas);
    try
      vPaintInfo.PageIndex := vSectionPageIndex;
      FHCView.Sections[vSection].PaintPaper(vSectionPageIndex, 0, 0,
        FBitmap.Canvas, vPaintInfo);

      {for i := 0 to vPaintInfo.TopItems.Count - 1 do  // ���ƶ���Item
        vPaintInfo.TopItems[i].PaintTop(FBitmap.Canvas, vPaintInfo);}
    finally
      vPaintInfo.RestoreCanvasScale(FBitmap.Canvas, vScaleInfo);
    end;
  finally
    vPaintInfo.Free;
  end;
end;

end.
