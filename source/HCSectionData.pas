{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文档节对象高级管理单元                 }
{                                                       }
{*******************************************************}

unit HCSectionData;

interface

uses
  Windows, Classes, Graphics, SysUtils, Controls, HCCustomRichData, HCCustomData,
  HCPage, HCItem, HCDrawItem, HCCommon, HCStyle, HCParaStyle,
  HCTextStyle, HCRichData;

type
  // 用于文档页眉、页脚、页面Data基类，主要用于处理文档级Data变化时特有的属性或事件
  // 如只读状态切换，页眉、页脚、页面切换时需要通知外部控件以做界面控件状态变化，
  // 而单元格只读切换时不需要
  THCCustomSectionData = class(THCRichData)
  private
    FOnReadOnlySwitch: TNotifyEvent;
  protected
    procedure SetReadOnly(const Value: Boolean); override;
  public
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;
  end;

  THCHeaderData = class(THCCustomSectionData);

  THCFooterData = class(THCCustomSectionData);

  THCPageData = class(THCCustomSectionData)  // 此类中主要处理表格单元格Data不需要而正文需要的属性或事件
  private
    FShowLineActiveMark: Boolean;  // 当前激活的行前显示标识
    FShowUnderLine: Boolean;  // 下划线
    FShowLineNo: Boolean;  // 行号
    function GetPageDataFmtTop(const APageIndex: Integer): Integer;
  protected
    procedure DoDrawItemPaintBefor(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    {$IFDEF DEBUG}
    procedure DoDrawItemPaintAfter(const AData: THCCustomData; const ADrawItemNo: Integer;
      const ADrawRect: TRect; const ADataDrawLeft, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    {$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function InsertStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word): Boolean; override;
  public
    constructor Create(const AStyle: THCStyle); override;

    /// <summary> 从当前位置后分页 </summary>
    function InsertPageBreak: Boolean;

    /// <summary> 插入批注 </summary>
    function InsertAnnotate(const AText: string): Boolean;
    //
    // 保存
    function GetTextStr: string;
    procedure SaveToText(const AFileName: string; const AEncoding: TEncoding);
    procedure SaveToTextStream(const AStream: TStream; const AEncoding: TEncoding);
    // 读取
    procedure LoadFromText(const AFileName: string; const AEncoding: TEncoding);
    procedure LoadFromTextStream(AStream: TStream; AEncoding: TEncoding);
    //
    property ShowLineActiveMark: Boolean read FShowLineActiveMark write FShowLineActiveMark;
    property ShowLineNo: Boolean read FShowLineNo write FShowLineNo;
    property ShowUnderLine: Boolean read FShowUnderLine write FShowUnderLine;
  end;

implementation

{$I HCView.inc}

uses
  Math, HCTextItem, HCRectItem, HCImageItem, HCTableItem, HCPageBreakItem;

{ THCPageData }

constructor THCPageData.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  FShowLineActiveMark := False;
  FShowUnderLine := False;
  FShowLineNo := False;
end;

procedure THCPageData.SaveToStream(const AStream: TStream);
begin
  AStream.WriteBuffer(FShowUnderLine, SizeOf(FShowUnderLine));
  inherited SaveToStream(AStream);
end;

procedure THCPageData.SaveToText(const AFileName: string; const AEncoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToTextStream(Stream, AEncoding);
  finally
    Stream.Free;
  end;
end;

procedure THCPageData.SaveToTextStream(const AStream: TStream; const AEncoding: TEncoding);
var
  Buffer, Preamble: TBytes;
begin
  Buffer := AEncoding.GetBytes(GetTextStr);
  Preamble := AEncoding.GetPreamble;
  if Length(Preamble) > 0 then
    AStream.WriteBuffer(Preamble[0], Length(Preamble));
  AStream.WriteBuffer(Buffer[0], Length(Buffer));
end;

{$IFDEF DEBUG}
procedure THCPageData.DoDrawItemPaintAfter(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited;
  {$IFDEF SHOWITEMNO}
  if ADrawItemNo = Items[DrawItems[ADrawItemNo].ItemNo].FirstDItemNo then  //
  {$ENDIF}
  begin
    {$IFDEF SHOWITEMNO}
    DrawDebugInfo(ACanvas, ADrawRect.Left, ADrawRect.Top - 12, IntToStr(DrawItems[ADrawItemNo].ItemNo));
    {$ENDIF}

    {$IFDEF SHOWDRAWITEMNO}
    DrawDebugInfo(ACanvas, ADrawRect.Left, ADrawRect.Top - 12, IntToStr(ADrawItemNo));
    {$ENDIF}
  end;
end;
{$ENDIF}

procedure THCPageData.DoDrawItemPaintBefor(const AData: THCCustomData;
  const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vTop: Integer;
  vFont: TFont;
  i, vLineNo: Integer;
begin
  inherited DoDrawItemPaintBefor(AData, ADrawItemNo, ADrawRect, ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
  if not APaintInfo.Print then
  begin
    if FShowLineActiveMark then  // 绘制行指示符
    begin
      if ADrawItemNo = GetSelectStartDrawItemNo then  // 是选中的起始DrawItem
      begin
        ACanvas.Pen.Color := clBlue;
        ACanvas.Pen.Style := psSolid;
        vTop := ADrawRect.Top + DrawItems[ADrawItemNo].Height div 2;

        ACanvas.MoveTo(ADataDrawLeft - 10, vTop);
        ACanvas.LineTo(ADataDrawLeft - 11, vTop);

        ACanvas.MoveTo(ADataDrawLeft - 11, vTop - 1);
        ACanvas.LineTo(ADataDrawLeft - 11, vTop + 2);
        ACanvas.MoveTo(ADataDrawLeft - 12, vTop - 2);
        ACanvas.LineTo(ADataDrawLeft - 12, vTop + 3);
        ACanvas.MoveTo(ADataDrawLeft - 13, vTop - 3);
        ACanvas.LineTo(ADataDrawLeft - 13, vTop + 4);
        ACanvas.MoveTo(ADataDrawLeft - 14, vTop - 4);
        ACanvas.LineTo(ADataDrawLeft - 14, vTop + 5);
        ACanvas.MoveTo(ADataDrawLeft - 15, vTop - 2);
        ACanvas.LineTo(ADataDrawLeft - 15, vTop + 3);
        ACanvas.MoveTo(ADataDrawLeft - 16, vTop - 2);
        ACanvas.LineTo(ADataDrawLeft - 16, vTop + 3);
      end;
    end;

    if FShowUnderLine then  // 下划线
    begin
      if DrawItems[ADrawItemNo].LineFirst then
      begin
        ACanvas.Pen.Color := clActiveBorder;
        ACanvas.Pen.Style := psSolid;
        ACanvas.MoveTo(ADataDrawLeft, ADrawRect.Bottom);
        ACanvas.LineTo(ADataDrawLeft + Self.Width, ADrawRect.Bottom);
      end;
    end;

    if FShowLineNo then  // 行号
    begin
      if DrawItems[ADrawItemNo].LineFirst then
      begin
        vLineNo := 0;
        for i := 0 to ADrawItemNo do
        begin
          if DrawItems[i].LineFirst then
            Inc(vLineNo);
        end;

        vFont := TFont.Create;
        try
          vFont.Assign(ACanvas.Font);
          ACanvas.Font.Color := RGB(180, 180, 180);
          ACanvas.Font.Size := 10;
          ACanvas.Font.Style := [];
          ACanvas.Font.Name := 'Courier New';
          //SetTextColor(ACanvas.Handle, RGB(180, 180, 180));
          ACanvas.Brush.Style := bsClear;
          vTop := ADrawRect.Top + (ADrawRect.Bottom - ADrawRect.Top - 16) div 2;
          ACanvas.TextOut(ADataDrawLeft - 50, vTop, IntToStr(vLineNo));
        finally
          ACanvas.Font.Assign(vFont);
          FreeAndNil(vFont);
        end;
      end;
    end;
  end;
end;

function THCPageData.GetTextStr: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Items.Count - 1 do
    Result := Result + Items[i].Text;
end;

function THCPageData.InsertAnnotate(const AText: string): Boolean;
//var
//  vAnnotaeItem: TAnnotaeItem;
begin
  Result := False;
  // 当前选中的内容添加批注，暂时未完成
//  Self.InsertItem(vAnnotaeItem);
end;

function THCPageData.InsertPageBreak: Boolean;
var
  vPageBreak: TPageBreakItem;
  vKey: Word;
begin
  Result := False;

  vPageBreak := TPageBreakItem.Create(Self);
  vPageBreak.ParaFirst := True;
  // 第一个Item分到下一页后，前一页没有任何Item，对编辑有诸多不利，所以在前一页补充一个空Item
  if (SelectInfo.StartItemNo = 0) and (SelectInfo.StartItemOffset = 0) then
  begin
    vKey := VK_RETURN;
    KeyDown(vKey, []);
  end;

  Result := Self.InsertItem(vPageBreak);
end;

function THCPageData.InsertStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word): Boolean;
begin
  // 因为复制粘贴时并不需要FShowUnderLine，为兼容粘贴所以FShowUnderLine在LoadFromStrem时处理
  //AStream.ReadBuffer(FShowUnderLine, SizeOf(FShowUnderLine));
  inherited InsertStream(AStream, AStyle, AFileVersion);
end;

procedure THCPageData.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  AStream.ReadBuffer(FShowUnderLine, SizeOf(FShowUnderLine));
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
end;

function THCPageData.GetPageDataFmtTop(const APageIndex: Integer): Integer;
//var
//  i, vContentHeight: Integer;
begin
//  Result := 0;
//  if APageIndex > 0 then
//  begin
//    vContentHeight := FPageSize.PageHeightPix  // 节页面正文区域高度，即页面除页眉、页脚后净高
//      - FPageSize.PageMarginBottomPix - GetHeaderAreaHeight;
//
//    for i := 0 to APageIndex - 1 do
//      Result := Result + vContentHeight;
//  end;
end;

procedure THCPageData.LoadFromText(const AFileName: string; const AEncoding: TEncoding);
var
  vStream: TStream;
  vFileFormat: string;
begin
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareExclusive);  // 只读打开、不允许其他程序以任何方式打开
  try
    vFileFormat := ExtractFileExt(AFileName);
    vFileFormat := LowerCase(vFileFormat);
    if vFileFormat = '.txt' then
      LoadFromTextStream(vStream, AEncoding);
  finally
    vStream.Free;
  end;
end;

procedure THCPageData.LoadFromTextStream(AStream: TStream; AEncoding: TEncoding);
var
  vSize: Integer;
  vBuffer: TBytes;
  vS: string;
begin
  Clear;
  vSize := AStream.Size - AStream.Position;
  SetLength(vBuffer, vSize);
  AStream.Read(vBuffer[0], vSize);
  vSize := TEncoding.GetBufferEncoding(vBuffer, AEncoding);
  vS := AEncoding.GetString(vBuffer, vSize, Length(vBuffer) - vSize);
  if vS <> '' then
    InsertText(vS);
end;

procedure THCPageData.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vMouseDownItemNo, vMouseDownItemOffset: Integer;
begin
  if FShowLineActiveMark then  // 显示当前编辑行
  begin
    vMouseDownItemNo := Self.MouseDownItemNo;
    vMouseDownItemOffset := Self.MouseDownItemOffset;
    inherited MouseDown(Button, Shift, X, Y);
    if (vMouseDownItemNo <> Self.MouseDownItemNo) or (vMouseDownItemOffset <> Self.MouseDownItemOffset) then
      Style.UpdateInfoRePaint;
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

{ THCCustomSectionData }

procedure THCCustomSectionData.SetReadOnly(const Value: Boolean);
begin
  if Self.ReadOnly <> Value then
  begin
    inherited SetReadOnly(Value);

    if Assigned(FOnReadOnlySwitch) then
      FOnReadOnlySwitch(Self);
  end;
end;

end.
