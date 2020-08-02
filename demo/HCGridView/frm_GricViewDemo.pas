unit frm_GricViewDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ActnList, ImgList, ExtCtrls, ComCtrls, ToolWin,
  HCGridView, HCTableCell, HCItem, HCCommon, HCCustomData;

type
  TfrmGridViewDemo = class(TForm)
    mmMain: TMainMenu;
    mniN1: TMenuItem;
    mniMerge: TMenuItem;
    mniDeleteCurRow: TMenuItem;
    mniInsertRowTop: TMenuItem;
    mniInsertRowBottom: TMenuItem;
    mniDeleteCurCol: TMenuItem;
    mniN2: TMenuItem;
    mniN7: TMenuItem;
    mniN8: TMenuItem;
    mniInsertColLeft: TMenuItem;
    mniInsertColRight: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    mniN3: TMenuItem;
    mniN4: TMenuItem;
    mniN5: TMenuItem;
    pmGridView: TPopupMenu;
    mniCut: TMenuItem;
    mniN6: TMenuItem;
    mniPaste: TMenuItem;
    mniTable: TMenuItem;
    mni3: TMenuItem;
    mni4: TMenuItem;
    mniN10: TMenuItem;
    mni5: TMenuItem;
    mni6: TMenuItem;
    mniN11: TMenuItem;
    mni7: TMenuItem;
    mniSplitRow: TMenuItem;
    mniN47: TMenuItem;
    mni8: TMenuItem;
    mni9: TMenuItem;
    mniN25: TMenuItem;
    mniBorder: TMenuItem;
    mniTableProperty: TMenuItem;
    mniPara: TMenuItem;
    mniControlItem: TMenuItem;
    mniModAnnotate: TMenuItem;
    mniDelAnnotate: TMenuItem;
    actlst: TActionList;
    actSearch: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    tlbTool: TToolBar;
    btnOpen: TToolButton;
    btnNew: TToolButton;
    btnSave: TToolButton;
    btnprint: TToolButton;
    btn3: TToolButton;
    btnUndo: TToolButton;
    btnRedo: TToolButton;
    cbbZoom: TComboBox;
    btn6: TToolButton;
    cbbFont: TComboBox;
    btn1: TToolButton;
    cbbFontSize: TComboBox;
    cbbFontColor: TColorBox;
    cbbBackColor: TColorBox;
    btnBold: TToolButton;
    btnItalic: TToolButton;
    btnUnderLine: TToolButton;
    btnStrikeOut: TToolButton;
    btnSuperScript: TToolButton;
    btnSubScript: TToolButton;
    btn2: TToolButton;
    btnRightIndent: TToolButton;
    btnLeftIndent: TToolButton;
    btnAlignLeft: TToolButton;
    btnAlignCenter: TToolButton;
    btnAlignRight: TToolButton;
    btnAlignJustify: TToolButton;
    btnAlignScatter: TToolButton;
    btnLineSpace: TToolButton;
    il1: TImageList;
    pmLineSpace: TPopupMenu;
    mniLS100: TMenuItem;
    mniLS115: TMenuItem;
    mniLS150: TMenuItem;
    mniLS200: TMenuItem;
    mniLSFix: TMenuItem;
    mniSave: TMenuItem;
    mniOpen: TMenuItem;
    mniNew: TMenuItem;
    mniN9: TMenuItem;
    mniN12: TMenuItem;
    mnigif1: TMenuItem;
    mniN13: TMenuItem;
    mniN14: TMenuItem;
    mniN15: TMenuItem;
    mniN17: TMenuItem;
    mniN18: TMenuItem;
    mniHC1: TMenuItem;
    mniCheckBox1: TMenuItem;
    mniEdit1: TMenuItem;
    mniCombobox1: TMenuItem;
    mniDateTimePicker1: TMenuItem;
    mniRadioGroup1: TMenuItem;
    mniN19: TMenuItem;
    mniN20: TMenuItem;
    mniN21: TMenuItem;
    mniN22: TMenuItem;
    mniN23: TMenuItem;
    mniExplore: TMenuItem;
    mniN24: TMenuItem;
    mniAlignTopLeft: TMenuItem;
    mniAlignTopCenter: TMenuItem;
    mniAlignTopRight: TMenuItem;
    mniAlignCenterLeft: TMenuItem;
    mniAlignCenterCenter: TMenuItem;
    mniAlignCenterRight: TMenuItem;
    mniAlignBottomLeft: TMenuItem;
    mniAlignBottomCenter: TMenuItem;
    mniAlignBottomRight: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniMergeClick(Sender: TObject);
    procedure mniDeleteCurRowClick(Sender: TObject);
    procedure mniInsertRowTopClick(Sender: TObject);
    procedure mniInsertRowBottomClick(Sender: TObject);
    procedure mniInsertColLeftClick(Sender: TObject);
    procedure mniInsertColRightClick(Sender: TObject);
    procedure mniDeleteCurColClick(Sender: TObject);
    procedure mniN4Click(Sender: TObject);
    procedure mniN5Click(Sender: TObject);
    procedure mniCutClick(Sender: TObject);
    procedure mniN6Click(Sender: TObject);
    procedure mniPasteClick(Sender: TObject);
    procedure mniParaClick(Sender: TObject);
    procedure mniControlItemClick(Sender: TObject);
    procedure cbbFontChange(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure btnAlignLeftClick(Sender: TObject);
    procedure mniLS100Click(Sender: TObject);
    procedure cbbBackColorChange(Sender: TObject);
    procedure cbbFontColorChange(Sender: TObject);
    procedure btnprintClick(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnRedoClick(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure mniOpenClick(Sender: TObject);
    procedure mniSaveClick(Sender: TObject);
    procedure mniExploreClick(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure mniN24Click(Sender: TObject);
    procedure mniN12Click(Sender: TObject);
    procedure mnigif1Click(Sender: TObject);
    procedure mniN14Click(Sender: TObject);
    procedure mniN15Click(Sender: TObject);
    procedure mniN17Click(Sender: TObject);
    procedure mniN18Click(Sender: TObject);
    procedure mniCheckBox1Click(Sender: TObject);
    procedure mniEdit1Click(Sender: TObject);
    procedure mniCombobox1Click(Sender: TObject);
    procedure mniDateTimePicker1Click(Sender: TObject);
    procedure mniRadioGroup1Click(Sender: TObject);
    procedure mniN20Click(Sender: TObject);
    procedure mniN21Click(Sender: TObject);
    procedure mniN22Click(Sender: TObject);
    procedure mniN23Click(Sender: TObject);
    procedure cbbFontSizeChange(Sender: TObject);
    procedure mniBorderClick(Sender: TObject);
    procedure mniTablePropertyClick(Sender: TObject);
    procedure mniAlignTopLeftClick(Sender: TObject);
  private
    { Private declarations }
    FGridView: THCGridView;
    function SaveFile: Boolean;
    procedure DoGridCellPaintBK(const Sender: TObject;
      const ACell: THCTableCell; const ARect: TRect; const ACanvas: TCanvas;
      const APaintInfo: TPaintInfo; var ADrawDefault: Boolean);
    procedure DoComboboxPopupItem(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmGridViewDemo: TfrmGridViewDemo;

implementation

uses
  Printers, HCTextStyle, HCParaStyle, HCViewData, HCFractionItem, HCExpressItem,
  HCSupSubScriptItem, HCCheckBoxItem, HCEditItem, HCComboboxItem, HCDateTimePicker,
  HCRadioGroup, HCBarCodeItem, HCQRCodeItem, HCTextItem, frm_Annotate, frm_TableProperty,
  frm_TableBorderBackColor, frm_PrintView, frm_Paragraph, frm_ControlItemProperty,
  frm_InsertTable, frm_PageSet, HCImageItem, HCRichData;

{$R *.dfm}

procedure TfrmGridViewDemo.btnAlignLeftClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FGridView.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
    1: FGridView.ApplyParaAlignHorz(TParaAlignHorz.pahCenter);
    2: FGridView.ApplyParaAlignHorz(TParaAlignHorz.pahRight);
    3: FGridView.ApplyParaAlignHorz(TParaAlignHorz.pahJustify);  // 两端
    4: FGridView.ApplyParaAlignHorz(TParaAlignHorz.pahScatter);  // 分散
    5: FGridView.ApplyParaLeftIndent;
    6: FGridView.ApplyParaLeftIndent(False);
  end;
end;

procedure TfrmGridViewDemo.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FGridView.ApplyTextStyle(THCFontStyle.tsBold);
    1: FGridView.ApplyTextStyle(THCFontStyle.tsItalic);
    2: FGridView.ApplyTextStyle(THCFontStyle.tsUnderline);
    3: FGridView.ApplyTextStyle(THCFontStyle.tsStrikeOut);
    4: FGridView.ApplyTextStyle(THCFontStyle.tsSuperscript);
    5: FGridView.ApplyTextStyle(THCFontStyle.tsSubscript);
  end;
end;

procedure TfrmGridViewDemo.btnprintClick(Sender: TObject);
var
  vPrintDlg: TPrintDialog;
begin
  vPrintDlg := TPrintDialog.Create(nil);
  try
    if vPrintDlg.Execute then
      FGridView.Print(Printer.Printers[Printer.PrinterIndex]);
  finally
    FreeAndNil(vPrintDlg);
  end;
end;

procedure TfrmGridViewDemo.btnRedoClick(Sender: TObject);
begin
  FGridView.Redo;
end;

procedure TfrmGridViewDemo.btnUndoClick(Sender: TObject);
begin
  FGridView.Undo;
end;

procedure TfrmGridViewDemo.cbbBackColorChange(Sender: TObject);
begin
  FGridView.ApplyTextBackColor(cbbBackColor.Selected);
end;

procedure TfrmGridViewDemo.cbbFontChange(Sender: TObject);
begin
  FGridView.ApplyTextFontName(cbbFont.Text);
  if not FGridView.Focused then
    FGridView.SetFocus;
end;

procedure TfrmGridViewDemo.cbbFontColorChange(Sender: TObject);
begin
  FGridView.ApplyTextColor(cbbFontColor.Selected);
  if not FGridView.Focused then
    FGridView.SetFocus;
end;

procedure TfrmGridViewDemo.cbbFontSizeChange(Sender: TObject);
begin
  FGridView.ApplyTextFontSize(GetFontSize(cbbFontSize.Text));
  if not FGridView.Focused then
    FGridView.SetFocus;
end;

procedure TfrmGridViewDemo.DoComboboxPopupItem(Sender: TObject);
begin
  if Sender is THCComboboxItem then
  begin
    if (Sender as THCComboboxItem).Items.Count > 20 then
      (Sender as THCComboboxItem).Items.Clear;

    (Sender as THCComboboxItem).Items.Add('选项' + IntToStr((Sender as THCComboboxItem).Items.Count - 1));
  end;
end;

procedure TfrmGridViewDemo.DoGridCellPaintBK(const Sender: TObject;
  const ACell: THCTableCell; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo; var ADrawDefault: Boolean);
begin
//  if (ARect.Left + FGridView.HorOffset > 250) then
//  //if (ARect.Top + FGridView.VerOffset > 250) then
//  begin
//    ACanvas.Brush.Color := clYellow;
//    ACanvas.FillRect(ARect);
//  end;
end;

procedure TfrmGridViewDemo.FormCreate(Sender: TObject);
begin
  FGridView := THCGridView.CreateEx(nil, 80, 12);
  //FGridView.OnCellPaintBK := DoGridCellPaintBK;
  FGridView.Align := alClient;
  FGridView.Parent := Self;
  FGridView.PopupMenu := pmGridView;
end;

procedure TfrmGridViewDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGridView);
end;

procedure TfrmGridViewDemo.mniMergeClick(Sender: TObject);
begin
  //FGridView.MergeTableSelectCells;
  FGridView.MergeSelectCells;
end;

procedure TfrmGridViewDemo.mniNewClick(Sender: TObject);
var
  vFrmInsertTable: TfrmInsertTable;
begin
  vFrmInsertTable := TfrmInsertTable.Create(Self);
  try
    vFrmInsertTable.ShowModal;
    if vFrmInsertTable.ModalResult = mrOk then
    begin
      FGridView.ReSetRowCol(StrToInt(vFrmInsertTable.edtRows.Text),
        StrToInt(vFrmInsertTable.edtCols.Text));
    end;
  finally
    FreeAndNil(vFrmInsertTable);
  end;
end;

procedure TfrmGridViewDemo.mniOpenClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
  vExt: string;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '支持的文件|*' + HC_EXT + '; *.xml; *.xlsx|HCGridView (*.hcf)|*' + HC_EXT + '|HCGridView xml (*.xml)|*.xml|Excel 2007 Document (*.xlsx)|*.xlsx';
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
      begin
        Application.ProcessMessages;  // 解决双击打开文件后，触发下层控件的Mousemove，Mouseup事件
        vExt := LowerCase(ExtractFileExt(vOpenDlg.FileName)); // 后缀
        if vExt = HC_EXT then
          FGridView.LoadFromFile(vOpenDlg.FileName)
        else
        if vExt = '.xml' then
          FGridView.LoadFromXml(vOpenDlg.FileName)
        else
          //FGridView.LoadFromDocumentFile(vOpenDlg.FileName, vExt)
      end;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmGridViewDemo.mniN12Click(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
  vImageItem: THCImageItem;
  vTopData: THCRichData;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '图像文件|*.bmp; *.jpg; *.jpeg; *.png|Windows Bitmap|*.bmp|JPEG 文件|*.jpg; *.jpge|可移植网络图形|*.png';
    FGridView.Enabled := False;
    try
      if vOpenDlg.Execute then
      begin
        if vOpenDlg.FileName <> '' then
        begin
          vTopData := FGridView.TopLevelData as THCRichData;
          vImageItem := THCImageItem.Create(vTopData);
          vImageItem.LoadGraphicFile(vOpenDlg.FileName);
          vImageItem.RestrainSize(vTopData.Width, vImageItem.Height);
          Application.ProcessMessages;  // 解决双击打开文件后，触发下层控件的Mousemove，Mouseup事件
          FGridView.InsertItem(vImageItem);
        end;
      end;
    finally
      FGridView.Enabled := True;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmGridViewDemo.mniN14Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vFractionItem: THCFractionItem;
begin
  //vTopData := FGridView.ActiveSectionTopLevelData;
  vTopData := FGridView.TopLevelData;
  if Assigned(vTopData) then
  begin
    vFractionItem := THCFractionItem.Create(vTopData, '12', '2019');
    FGridView.InsertItem(vFractionItem);
  end;
end;

procedure TfrmGridViewDemo.mniN15Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vExpressItem: THCExpressItem;
begin
  //vTopData := FGridView.ActiveSectionTopLevelData;
  vTopData := FGridView.TopLevelData;
  if Assigned(vTopData) then
  begin
    vExpressItem := THCExpressItem.Create(vTopData,
      '12', '5-6', '2017-6-3', '28-30');
    FGridView.InsertItem(vExpressItem);
  end;
end;

procedure TfrmGridViewDemo.mniN17Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vSupSubScriptItem: THCSupSubScriptItem;
begin
  //vTopData := FGridView.ActiveSectionTopLevelData;
  vTopData := FGridView.TopLevelData;
  if Assigned(vTopData) then
  begin
    vSupSubScriptItem := THCSupSubScriptItem.Create(vTopData, '20g', '先煎');
    FGridView.InsertItem(vSupSubScriptItem);
  end;
end;

procedure TfrmGridViewDemo.mniN18Click(Sender: TObject);
begin
  FGridView.InsertLine(1);
end;

procedure TfrmGridViewDemo.mniN20Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vHCBarCode: THCBarCodeItem;
  vS: string;
begin
  vS := InputBox('文本框', '文本', 'HC-' + FormatDateTime('YYYYMMDD', Now));
  //vTopData := FGridView.ActiveSectionTopLevelData;
  vTopData := FGridView.TopLevelData;
  if Assigned(vTopData) then
  begin
    vHCBarCode := THCBarCodeItem.Create(vTopData, vS);
    FGridView.InsertItem(vHCBarCode);
  end;
end;

procedure TfrmGridViewDemo.mniN21Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vQRCode: THCQRCodeItem;
  vS: string;
begin
  vS := InputBox('文本框', '文本', 'HCView使用了DelphiZXingQRCode二维码控件');
  //vTopData := FGridView.ActiveSectionTopLevelData;
  vTopData := FGridView.TopLevelData;
  if Assigned(vTopData) then
  begin
    vQRCode := THCQRCodeItem.Create(vTopData, vS);
    FGridView.InsertItem(vQRCode);
  end;
end;

procedure TfrmGridViewDemo.mniN22Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vTextItem: THCTextItem;
begin
  //vTopData := FGridView.ActiveSectionTopLevelData;
  vTopData := FGridView.TopLevelData;
  if Assigned(vTopData) then
  begin
    vTextItem := vTopData.CreateDefaultTextItem as THCTextItem;
    vTextItem.Text := '打开百度';
    vTextItem.HyperLink := 'www.baidu.com';
    FGridView.InsertItem(vTextItem);
  end;
end;

procedure TfrmGridViewDemo.mniN23Click(Sender: TObject);
var
  vTopData: THCViewData;
  vFrmAnnotate: TfrmAnnotate;
begin
  //vTopData := FGridView.ActiveSectionTopLevelData as THCViewData;
  vTopData := FGridView.TopLevelData as THCViewData;;
  if Assigned(vTopData) then
  begin
    if not vTopData.SelectExists then Exit;

    vFrmAnnotate := TfrmAnnotate.Create(Self);
    try
      vFrmAnnotate.SetAnnotate(nil);
      if vFrmAnnotate.ModalResult = mrOk then
        vTopData.InsertAnnotate(vFrmAnnotate.edtTitle.Text, vFrmAnnotate.mmoText.Text);
    finally
      FreeAndNil(vFrmAnnotate);
    end;
  end;
end;

procedure TfrmGridViewDemo.mniN24Click(Sender: TObject);
var
  vFrmInsertTable: TfrmInsertTable;
begin
  vFrmInsertTable := TfrmInsertTable.Create(Self);
  try
    vFrmInsertTable.ShowModal;
    if vFrmInsertTable.ModalResult = mrOk then
    begin
      FGridView.InsertTable(StrToInt(vFrmInsertTable.edtRows.Text),
        StrToInt(vFrmInsertTable.edtCols.Text));
    end;
  finally
    FreeAndNil(vFrmInsertTable);
  end;
end;

procedure TfrmGridViewDemo.mniParaClick(Sender: TObject);
var
  vFrmParagraph: TfrmParagraph;
begin
  vFrmParagraph := TfrmParagraph.Create(Self);
  try
    vFrmParagraph.SetGridView(FGridView);
  finally
    FreeAndNil(vFrmParagraph);
  end;
end;

procedure TfrmGridViewDemo.mniN4Click(Sender: TObject);
var
  vFrmPrintView: TfrmPrintView;
begin
  vFrmPrintView := TfrmPrintView.Create(Self);
  try
    vFrmPrintView.SetGridView(FGridView);
  finally
    FreeAndNil(vFrmPrintView);
  end;
end;

procedure TfrmGridViewDemo.mniN5Click(Sender: TObject);
begin
  FGridView.Print;
end;

procedure TfrmGridViewDemo.mniN6Click(Sender: TObject);
begin
//  FGridView.Copy;
end;

procedure TfrmGridViewDemo.mniPasteClick(Sender: TObject);
begin
//  FGridView.Paste;
end;

procedure TfrmGridViewDemo.mniRadioGroup1Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vHCRadioGroup: THCRadioGroup;
begin
  //vTopData := FGridView.ActiveSectionTopLevelData;
  vTopData := FGridView.TopLevelData;
  if Assigned(vTopData) then
  begin
    vHCRadioGroup := THCRadioGroup.Create(vTopData);
    vHCRadioGroup.AddItem('选项1');
    vHCRadioGroup.AddItem('选项2');
    vHCRadioGroup.AddItem('选项3');
    FGridView.InsertItem(vHCRadioGroup);
  end;
end;

procedure TfrmGridViewDemo.mniSaveClick(Sender: TObject);
begin
  if SaveFile then
    ShowMessage('保存成功！');
end;

procedure TfrmGridViewDemo.mniTablePropertyClick(Sender: TObject);
var
  vFrmTableProperty: TFrmTableProperty;
begin
  vFrmTableProperty := TFrmTableProperty.Create(Self);
  try
    vFrmTableProperty.SetGridView(FGridView);
  finally
    FreeAndNil(vFrmTableProperty);
  end;
end;

procedure TfrmGridViewDemo.N2Click(Sender: TObject);
var
  vFrmPageSet: TFrmPageSet;
begin
  vFrmPageSet := TFrmPageSet.Create(Self);
  try
    vFrmPageSet.SetGridView(FGridView);
  finally
    FreeAndNil(vFrmPageSet);
  end;
end;

function TfrmGridViewDemo.SaveFile: Boolean;
var
  vDlg: TSaveDialog;
begin
  Result := False;

  if FGridView.FileName <> '' then
  begin
    FGridView.SaveToFile(FGridView.FileName);
    FGridView.IsChanged := False;
    Result := True;
  end
  else
  begin
    vDlg := TSaveDialog.Create(Self);
    try
      vDlg.Filter := 'HCView (*.hcf)|*' + HC_EXT + '|HCView xml (*.xml)|*.xml';//|Word 2007 Document (*.docx)|*.docx';
      vDlg.Execute;
      if vDlg.FileName <> '' then
      begin
        case vDlg.FilterIndex of
          1:
            begin
              if ExtractFileExt(vDlg.FileName) <> HC_EXT then
                vDlg.FileName := vDlg.FileName + HC_EXT;

              FGridView.SaveToFile(vDlg.FileName);
              FGridView.IsChanged := False;
              Result := True;
            end;

          2:
            begin
              if LowerCase(ExtractFileExt(vDlg.FileName)) <> '.xml' then
                vDlg.FileName := vDlg.FileName + '.xml';

              FGridView.SaveToXml(vDlg.FileName, TEncoding.Unicode);
              FGridView.IsChanged := False;
              Result := True;
            end;

//          3:  // .docx
//            begin
//              if LowerCase(ExtractFileExt(vDlg.FileName)) <> HC_EXT_DOCX then
//                vDlg.FileName := vDlg.FileName + HC_EXT_DOCX;
//
//              FGridView.SaveToDocumentFile(vDlg.FileName, HC_EXT_DOCX);
//              FGridView.IsChanged := False;
//              Result := True;
//            end;
        end;
      end;
    finally
      vDlg.Free;
    end;
  end;
end;

procedure TfrmGridViewDemo.mniAlignTopLeftClick(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0: FGridView.ApplyTableCellAlign(THCContentAlign.tcaTopLeft);
    1: FGridView.ApplyTableCellAlign(THCContentAlign.tcaTopCenter);
    2: FGridView.ApplyTableCellAlign(THCContentAlign.tcaTopRight);
    3: FGridView.ApplyTableCellAlign(THCContentAlign.tcaCenterLeft);
    4: FGridView.ApplyTableCellAlign(THCContentAlign.tcaCenterCenter);
    5: FGridView.ApplyTableCellAlign(THCContentAlign.tcaCenterRight);
    6: FGridView.ApplyTableCellAlign(THCContentAlign.tcaBottomLeft);
    7: FGridView.ApplyTableCellAlign(THCContentAlign.tcaBottomCenter);
    8: FGridView.ApplyTableCellAlign(THCContentAlign.tcaBottomRight);
  end;
end;

procedure TfrmGridViewDemo.mniBorderClick(Sender: TObject);
var
  vFrmBorderBackColor: TfrmBorderBackColor;
begin
  vFrmBorderBackColor := TfrmBorderBackColor.Create(Self);
  try
    vFrmBorderBackColor.SetGridView(FGridView);
  finally
    FreeAndNil(vFrmBorderBackColor);
  end;
end;

procedure TfrmGridViewDemo.mniCheckBox1Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vCheckBox: THCCheckBoxItem;
  vS: string;
begin
  vS := '勾选框';
  if InputQuery('勾选框', '文本', vS) then
  begin
    //vTopData := FGridView.ActiveSectionTopLevelData;
    vTopData := FGridView.TopLevelData;
    if Assigned(vTopData) then
    begin
      vCheckBox := THCCheckBoxItem.Create(vTopData, vS, False);
      FGridView.InsertItem(vCheckBox);
    end;
  end;
end;

procedure TfrmGridViewDemo.mniCombobox1Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vCombobox: THCComboboxItem;
  vS: string;
begin
  vS := '默认值';
  if InputQuery('下拉框', '文本内容', vS) then
  begin
     //vTopData := FGridView.ActiveSectionTopLevelData;
    vTopData := FGridView.TopLevelData;
    if Assigned(vTopData) then
    begin
      vCombobox := THCComboboxItem.Create(vTopData, vS);
      vCombobox.Items.Add('选项1');
      vCombobox.Items.Add('选项2');
      vCombobox.Items.Add('选项3');
      vCombobox.OnPopupItem := DoComboboxPopupItem;
      //vCombobox.ItemIndex := 0;
      FGridView.InsertItem(vCombobox);
    end;
  end;
end;

procedure TfrmGridViewDemo.mniControlItemClick(Sender: TObject);
var
  vFrmControlItemProperty: TfrmControlItemProperty;
begin
//  vFrmControlItemProperty := TfrmControlItemProperty.Create(nil);
//  try
//    vFrmControlItemProperty.SetHCView(FHCView);
//  finally
//    FreeAndNil(vFrmControlItemProperty);
//  end;
end;

procedure TfrmGridViewDemo.mniCutClick(Sender: TObject);
begin
//  FGridView.Cut;
end;

procedure TfrmGridViewDemo.mniDateTimePicker1Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vHCDateTimePicker: THCDateTimePicker;
begin
  //vTopData := FGridView.ActiveSectionTopLevelData;
  vTopData := FGridView.TopLevelData;
  if Assigned(vTopData) then
  begin
    vHCDateTimePicker := THCDateTimePicker.Create(vTopData, Now);
    FGridView.InsertItem(vHCDateTimePicker);
  end;
end;

procedure TfrmGridViewDemo.mniDeleteCurColClick(Sender: TObject);
begin
  //FGridView.ActiveTableDeleteCurCol;
  FGridView.DeleteCurCol;
end;

procedure TfrmGridViewDemo.mniDeleteCurRowClick(Sender: TObject);
begin
  //FGridView.ActiveTableDeleteCurRow;
  FGridView.DeleteCurRow;
end;

procedure TfrmGridViewDemo.mniEdit1Click(Sender: TObject);
var
  vTopData: THCCustomData;
  vEdit: THCEditItem;
  vS: string;
begin
  vS := '文本';
  if InputQuery('文本框', '文本内容', vS) then
  begin
    //vTopData := FGridView.ActiveSectionTopLevelData;
    vTopData := FGridView.TopLevelData;
    if Assigned(vTopData) then
    begin
      vEdit := THCEditItem.Create(vTopData, vS);
      FGridView.InsertItem(vEdit);
    end;
  end;
end;

procedure TfrmGridViewDemo.mniExploreClick(Sender: TObject);
var
  vDlg: TSaveDialog;
  vExt: string;
begin
  vDlg := TSaveDialog.Create(Self);
  try
    vDlg.Filter := 'pdf格式|*.pdf' + '|htm格式|*.html';
    vDlg.Execute;
    if vDlg.FileName <> '' then
    begin
      vExt := '';
      case vDlg.FilterIndex of
        1: vExt := '.pdf';
        2: vExt := '.html';
      else
        Exit;
      end;

      if ExtractFileExt(vDlg.FileName) <> vExt then  // 避免重复后缀
        vDlg.FileName := vDlg.FileName + vExt;

      case vDlg.FilterIndex of
        1: FGridView.SaveToPDF(vDlg.FileName);
        2: FGridView.SaveToHtml(vDlg.FileName, False);
      end;
    end;
  finally
    vDlg.Free;
  end;
end;

procedure TfrmGridViewDemo.mnigif1Click(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '图像文件|*.gif';
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
      begin
        Application.ProcessMessages;  // 解决双击打开文件后，触发下层控件的Mousemove，Mouseup事件
        FGridView.InsertGifImage(vOpenDlg.FileName);
      end;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmGridViewDemo.mniInsertColLeftClick(Sender: TObject);
begin
  //FGridView.ActiveTableInsertColBefor(1);
  FGridView.InsertColBefor(1);
end;

procedure TfrmGridViewDemo.mniInsertColRightClick(Sender: TObject);
begin
  //FGridView.ActiveTableInsertColAfter(1);
  FGridView.InsertColAfter(1);
end;

procedure TfrmGridViewDemo.mniInsertRowBottomClick(Sender: TObject);
begin
  //FGridView.ActiveTableInsertRowAfter(1);
  FGridView.InsertRowAfter(1);
end;

procedure TfrmGridViewDemo.mniInsertRowTopClick(Sender: TObject);
begin
  //FGridView.ActiveTableInsertRowBefor(1);
  FGridView.InsertRowBefor(1);
end;

procedure TfrmGridViewDemo.mniLS100Click(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    case (Sender as TMenuItem).Tag of
      0: FGridView.ApplyParaLineSpace(TParaLineSpaceMode.pls100);  // 单倍
      1: FGridView.ApplyParaLineSpace(TParaLineSpaceMode.pls115);  // 1.15倍
      2: FGridView.ApplyParaLineSpace(TParaLineSpaceMode.pls150);  // 1.5倍
      3: FGridView.ApplyParaLineSpace(TParaLineSpaceMode.pls200);  // 双倍
      4: FGridView.ApplyParaLineSpace(TParaLineSpaceMode.plsFix);  // 固定值
    end;
  end;
end;

end.
