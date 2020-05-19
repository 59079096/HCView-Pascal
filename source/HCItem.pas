{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文档对象基类实现单元                   }
{                                                       }
{*******************************************************}

unit HCItem;

interface

uses
  Windows, Classes, Controls, Graphics, Generics.Collections, HCCommon, HCStyle,
  HCUndo, HCXml;

type
  TScaleInfo = record
    MapMode: Integer;
    WindowOrg: TSize;
    WindowExt: TSize;
    ViewportOrg: TSize;
    ViewportExt: TSize;
  end;

  THCViewModel = (
    /// <summary> 胶卷视图，显示页眉、页脚 </summary>
    hvmFilm,
    /// <summary> Page视图，显示左右边距，不显示页眉、页脚 </summary>
    hvmPage,
    /// <summary> Text视图，不显示页边距和页眉页脚 </summary>
    hvmEdit
  );

  TItemOptions = set of (ioParaFirst, ioPageBreak);

  TItemSelectState = (issNone, issPart, issComplate);

  THCCustomItemClass = class of THCCustomItem;

  THCCustomItem = class;

  TPaintInfo = class(TObject)  // 绘制时的信息，用于给外部事件增加更多的信息
  private
    FPrint: Boolean;
    FViewModel: THCViewModel;
    FTopItems: TObjectList<THCCustomItem>;
    FWindowWidth, FWindowHeight, FDPI: Integer;
    FScaleX, FScaleY,  // 目标画布和显示器画布dpi比例(打印机dpi和显示器dpi不一致时的缩放比例)
    FZoom  // 视图设置的放大比例
      : Single;
    // 如果要将ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom,
    // 等信息增加到此类中，需要设计表格单元格Data绘制时和页面Data的这几个值不一样
    // 需要不停的修改此类中这几个参数
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ScaleCanvas(const ACanvas: TCanvas): TScaleInfo;
    procedure RestoreCanvasScale(const ACanvas: TCanvas; const AOldInfo: TScaleInfo);
    function GetScaleX(const AValue: Integer): Integer;
    function GetScaleY(const AValue: Integer): Integer;
    procedure DrawNoScaleLine(const ACanvas: TCanvas; const APoints: array of TPoint); overload;
    procedure DrawNoScaleLine(const ACanvas: TCanvas; const AX1, AY1, AX2, AY2: Integer); overload;
    property Print: Boolean read FPrint write FPrint;

    /// <summary> 界面显示模式：页面、Web </summary>
    property ViewModel: THCViewModel read FViewModel write FViewModel;

    /// <summary> 只管理不负责释放 </summary>
    property TopItems: TObjectList<THCCustomItem> read FTopItems;

    /// <summary> 用于绘制的区域高度 </summary>
    property WindowWidth: Integer read FWindowWidth write FWindowWidth;

    /// <summary> 用于绘制的区域宽度 </summary>
    property WindowHeight: Integer read FWindowHeight write FWindowHeight;

    /// <summary> 横向缩放 </summary>
    property ScaleX: Single read FScaleX write FScaleX;

    /// <summary> 纵向缩放 </summary>
    property ScaleY: Single read FScaleY write FScaleY;

    property Zoom: Single read FZoom write FZoom;

    property DPI: Integer read FDPI write FDPI;
  end;

  THCCustomItem = class(TObject)
  strict private
    FParaNo,
    FStyleNo,
    FFirstDItemNo: Integer;
    FActive, FVisible,
    FPrintInvisible  // 打印时不可见
      : Boolean;
    FOptions: TItemOptions;
    FSelectState: TItemSelectState;
  protected
    function GetParaFirst: Boolean;
    procedure SetParaFirst(const Value: Boolean);
    function GetPageBreak: Boolean;
    procedure SetPageBreak(const Value: Boolean);
    function GetSelectComplate: Boolean; virtual;
    function GetSelectPart: Boolean;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    function GetHyperLink: string; virtual;
    procedure SetHyperLink(const Value: string); virtual;
    procedure SetActive(const Value: Boolean); virtual;
    function GetLength: Integer; virtual;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
  public
    constructor Create; virtual;

    procedure Assign(Source: THCCustomItem); virtual;
    /// <summary> 绘制Item的事件 </summary>
    /// <param name="ACanvas"></param>
    /// <param name="ADrawRect">当前DrawItem的区域</param>
    /// <param name="ADataDrawBottom">Item所在的Data本次绘制底部位置</param>
    /// <param name="ADataScreenTop"></param>
    /// <param name="ADataScreenBottom"></param>
    procedure PaintTo(const AStyle: THCStyle; const ADrawRect: TRect;
      const APageDataDrawTop, APageDataDrawBottom, APageDataScreenTop, APageDataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual; final;  // 不可继承

    procedure PaintTop(const ACanvas: TCanvas); virtual;

    /// <summary>
    /// 将2个Item合并为同一个
    /// </summary>
    /// <param name="AItemA">ItemA</param>
    /// <param name="AItemB">ItemB</param>
    /// <returns>True合并成功，否则返回False</returns>
    function CanConcatItems(const AItem: THCCustomItem): Boolean; virtual;

    procedure DisSelect; virtual;
    function CanDrag: Boolean; virtual;
    procedure KillFocus; virtual;
    procedure DblClick(const X, Y: Integer); virtual;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    function GetHint: string; virtual;
    procedure SelectComplate; virtual;
    procedure SelectPart;
    function Selected: Boolean;
    /// <summaryy 在指定的位置是否可接受插入、删除等操作 </summary>
    function AcceptAction(const AOffset: Integer; const ARestrain: Boolean; const AAction: THCAction): Boolean; virtual;
    /// <summary> 从指定位置将当前item分成前后两部分 </summary>
    /// <param name="AOffset">分裂位置</param>
    /// <returns>后半部分对应的Item</returns>
    function BreakByOffset(const AOffset: Integer): THCCustomItem; virtual;
    procedure SaveToStream(const AStream: TStream); overload;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); overload; virtual;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); virtual;
    function ToHtml(const APath: string): string; virtual;
    procedure ToXml(const ANode: IHCXMLNode); virtual;
    procedure ParseXml(const ANode: IHCXMLNode); virtual;

    // 撤销重做相关方法
    procedure Undo(const AUndoAction: THCCustomUndoAction); virtual;
    procedure Redo(const ARedoAction: THCCustomUndoAction); virtual;
    //
    property Options: TItemOptions read FOptions;
    property Text: string read GetText write SetText;
    property Length: Integer read GetLength;
    property ParaFirst: Boolean read GetParaFirst write SetParaFirst;
    property PageBreak: Boolean read GetPageBreak write SetPageBreak;
    property HyperLink: string read GetHyperLink write SetHyperLink;

    property IsSelectComplate: Boolean read GetSelectComplate;
    property IsSelectPart: Boolean read GetSelectPart;

    property StyleNo: Integer read FStyleNo write FStyleNo;
    property ParaNo: Integer read FParaNo write FParaNo;
    property FirstDItemNo: Integer read FFirstDItemNo write FFirstDItemNo;
    property Active: Boolean read FActive write SetActive;
    property Visible: Boolean read FVisible write FVisible;
    property PrintInvisible: Boolean read FPrintInvisible write FPrintInvisible;
  end;

  TItemNotifyEvent = procedure(const AItem: THCCustomItem) of object;

  THCItems = class(TObjectList<THCCustomItem>)
  private
    FOnInsertItem, FOnRemoveItem: TItemNotifyEvent;
  protected
    procedure Notify(const Value: THCCustomItem; Action: TCollectionNotification); override;
  public
    property OnInsertItem: TItemNotifyEvent read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TItemNotifyEvent read FOnRemoveItem write FOnRemoveItem;
  end;

implementation

uses
  SysUtils;

{ THCCustomItem }

function THCCustomItem.CanDrag: Boolean;
begin
  Result := True;
end;

procedure THCCustomItem.Assign(Source: THCCustomItem);
begin
  Self.FStyleNo := Source.StyleNo;
  Self.FParaNo := Source.ParaNo;
  Self.FPrintInvisible := Source.PrintInvisible;
  Self.FOptions := Source.Options;
end;

function THCCustomItem.BreakByOffset(const AOffset: Integer): THCCustomItem;
begin
  // 继承者自己判断能否Break
  Result := THCCustomItemClass(Self.ClassType).Create;
  Result.Assign(Self);
  Result.ParaFirst := False;  // 打断后，后面的肯定不是断首
end;

function THCCustomItem.AcceptAction(const AOffset: Integer; const ARestrain: Boolean;
  const AAction: THCAction): Boolean;
begin
  Result := True;
end;

function THCCustomItem.CanConcatItems(const AItem: THCCustomItem): Boolean;
begin
  // 本方法只支持判断源AItem不是段首，不判断自己是否为段首
  Result := (Self.ClassType = AItem.ClassType)
    and (Self.FStyleNo = AItem.StyleNo)
    //and (not AItem.ParaFirst);  // 源Item不是段首，遇到需要跨段合并的可见201804111209
end;

constructor THCCustomItem.Create;
begin
  FStyleNo := THCStyle.Null;
  FParaNo := THCStyle.Null;
  FFirstDItemNo := -1;
  FOptions := [];
  FSelectState := issNone;
  FVisible := True;
  FActive := False;
  FPrintInvisible := False;
end;

procedure THCCustomItem.DblClick(const X, Y: Integer);
begin
end;

procedure THCCustomItem.DisSelect;
begin
  FSelectState := issNone;  // 处理自己的全选、部分选状态
end;

procedure THCCustomItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
end;

function THCCustomItem.GetHint: string;
begin
  Result := '';
end;

function THCCustomItem.GetHyperLink: string;
begin
  Result := '';
end;

function THCCustomItem.GetLength: Integer;
begin
  Result := 0;
end;

function THCCustomItem.GetPageBreak: Boolean;
begin
  Result := ioPageBreak in FOptions;
end;

function THCCustomItem.GetParaFirst: Boolean;
begin
  Result := ioParaFirst in FOptions;
end;

function THCCustomItem.GetSelectComplate: Boolean;
begin
  Result := FSelectState = issComplate;
end;

function THCCustomItem.GetSelectPart: Boolean;
begin
  Result := FSelectState = issPart;
end;

function THCCustomItem.GetText: string;
begin
  Result := '';
end;

procedure THCCustomItem.KillFocus;
begin
end;

procedure THCCustomItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vParFirst: Boolean;
  vByte: Byte;
begin
  //AStream.ReadBuffer(FStyleNo, SizeOf(FStyleNo));  // 由TCustomData.InsertStream处加载了
  AStream.ReadBuffer(FParaNo, SizeOf(FParaNo));

  if AFileVersion > 25 then
    AStream.ReadBuffer(FOptions, SizeOf(FOptions))
  else
  begin
    AStream.ReadBuffer(vParFirst, SizeOf(vParFirst));
    ParaFirst := vParFirst;
  end;

  if AFileVersion > 33 then
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    FPrintInvisible := Odd(vByte shr 7);
  end;
end;

function THCCustomItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Active := True;
  Result := FActive;
end;

procedure THCCustomItem.MouseEnter;
begin
end;

procedure THCCustomItem.MouseLeave;
begin
end;

function THCCustomItem.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := FActive;
end;

function THCCustomItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := FActive;
end;

procedure THCCustomItem.PaintTo(const AStyle: THCStyle; const ADrawRect: TRect;
  const APageDataDrawTop, APageDataDrawBottom, APageDataScreenTop, APageDataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDCState: THCCanvas;
begin
  if APaintInfo.Print and FPrintInvisible then Exit;

  vDCState := SaveCanvas(ACanvas);
  try
    DoPaint(AStyle, ADrawRect, APageDataDrawTop, APageDataDrawBottom,
      APageDataScreenTop, APageDataScreenBottom, ACanvas, APaintInfo);
  finally
    vDCState.ToCanvas(ACanvas);
    FreeAndNil(vDCState);
  end;
end;

procedure THCCustomItem.PaintTop(const ACanvas: TCanvas);
begin
end;

procedure THCCustomItem.ParseXml(const ANode: IHCXMLNode);
begin
  FStyleNo := ANode.Attributes['sno'];
  FParaNo := ANode.Attributes['pno'];
  Self.ParaFirst := ANode.Attributes['parafirst'];
  Self.PageBreak := ANode.Attributes['pagebreak'];
  if ANode.HasAttribute('printvisible') then
    FPrintInvisible := ANode.Attributes['printvisible']
  else
    FPrintInvisible := False;
end;

procedure THCCustomItem.Redo(const ARedoAction: THCCustomUndoAction);
begin
end;

function THCCustomItem.Selected: Boolean;
begin
  Result := FSelectState <> issNone;
end;

procedure THCCustomItem.SelectComplate;
begin
  FSelectState := issComplate;
end;

procedure THCCustomItem.SelectPart;
begin
  FSelectState := issPart;
end;

procedure THCCustomItem.SetText(const Value: string);
begin
end;

function THCCustomItem.ToHtml(const APath: string): string;
begin
  Result := '';
end;

procedure THCCustomItem.ToXml(const ANode: IHCXMLNode);
begin
  ANode.Attributes['sno'] := FStyleNo;
  ANode.Attributes['pno'] := FParaNo;
  ANode.Attributes['parafirst'] := Self.ParaFirst;
  ANode.Attributes['pagebreak'] := Self.PageBreak;
  if FPrintInvisible then
    ANode.Attributes['printvisible'] := '1';
end;

procedure THCCustomItem.Undo(const AUndoAction: THCCustomUndoAction);
begin
end;

procedure THCCustomItem.SaveToStream(const AStream: TStream);
begin
  SaveToStream(AStream, 0, Self.Length);
end;

procedure THCCustomItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  AStream.WriteBuffer(FStyleNo, SizeOf(FStyleNo));
  AStream.WriteBuffer(FParaNo, SizeOf(FParaNo));
  AStream.WriteBuffer(FOptions, SizeOf(FOptions));

  vByte := 0;
  if FPrintInvisible then
    vByte := vByte or (1 shl 7);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
end;

procedure THCCustomItem.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
    FActive := Value;
end;

procedure THCCustomItem.SetHyperLink(const Value: string);
begin
end;

procedure THCCustomItem.SetPageBreak(const Value: Boolean);
begin
  if Value then
    Include(FOptions, ioPageBreak)
  else
    Exclude(FOptions, ioPageBreak);
end;

procedure THCCustomItem.SetParaFirst(const Value: Boolean);
begin
  if Value then
    Include(FOptions, ioParaFirst)
  else
    Exclude(FOptions, ioParaFirst);
end;

{ THCItems }

procedure THCItems.Notify(const Value: THCCustomItem;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      begin
        if Assigned(FOnInsertItem) then
          FOnInsertItem(Value);
      end;

    cnRemoved:
      begin
        if Assigned(FOnRemoveItem) then
          FOnRemoveItem(Value);
      end;

    cnExtracted: ;
  end;

  inherited Notify(Value, Action);
end;

{ TPaintInfo }

constructor TPaintInfo.Create;
begin
  FTopItems := TObjectList<THCCustomItem>.Create(False);  // 只管理不负责释放
  FScaleX := 1;
  FScaleY := 1;
  FZoom := 1;
  FViewModel := hvmFilm;
end;

destructor TPaintInfo.Destroy;
begin
  FTopItems.Free;
  inherited Destroy;
end;

procedure TPaintInfo.DrawNoScaleLine(const ACanvas: TCanvas; const AX1, AY1,
  AX2, AY2: Integer);
var
  vPt: TPoint;
begin
  SetViewportExtEx(ACanvas.Handle, FWindowWidth, FWindowHeight, @vPt);
  try
    ACanvas.MoveTo(GetScaleX(AX1), GetScaleY(AY1));
    ACanvas.LineTo(GetScaleX(AX2), GetScaleY(AY2));
  finally
    SetViewportExtEx(ACanvas.Handle, Round(FWindowWidth * FScaleX),
      Round(FWindowHeight * FScaleY), @vPt);
  end;
end;

procedure TPaintInfo.DrawNoScaleLine(const ACanvas: TCanvas;
  const APoints: array of TPoint);
var
  vPt: TPoint;
  i: Integer;
begin
  SetViewportExtEx(ACanvas.Handle, FWindowWidth, FWindowHeight, @vPt);
  try
    ACanvas.MoveTo(GetScaleX(APoints[0].X), GetScaleY(APoints[0].Y));
    for i := 1 to Length(APoints) - 1 do
      ACanvas.LineTo(GetScaleX(APoints[i].X), GetScaleY(APoints[i].Y));
  finally
    SetViewportExtEx(ACanvas.Handle, Round(FWindowWidth * FScaleX),
      Round(FWindowHeight * FScaleY), @vPt);
  end;
end;

function TPaintInfo.GetScaleX(const AValue: Integer): Integer;
begin
  Result := Round(AValue * FScaleX);
end;

function TPaintInfo.GetScaleY(const AValue: Integer): Integer;
begin
  Result := Round(AValue * FScaleY);
end;

procedure TPaintInfo.RestoreCanvasScale(const ACanvas: TCanvas;
  const AOldInfo: TScaleInfo);
begin
  if AOldInfo.MapMode = 0 then Exit;

  SetViewportOrgEx(ACanvas.Handle, AOldInfo.ViewportOrg.cx, AOldInfo.ViewportOrg.cy, nil);
  SetViewportExtEx(ACanvas.Handle, AOldInfo.ViewportExt.cx, AOldInfo.ViewportExt.cy, nil);
  SetWindowOrgEx(ACanvas.Handle, AOldInfo.WindowOrg.cx, AOldInfo.WindowOrg.cy, nil);
  SetWindowExtEx(ACanvas.Handle, AOldInfo.WindowExt.cx, AOldInfo.WindowExt.cy, nil);
  SetMapMode(ACanvas.Handle, AOldInfo.MapMode);
end;

function TPaintInfo.ScaleCanvas(const ACanvas: TCanvas): TScaleInfo;
begin
  if (FScaleX = 1) and (FScaleY = 1) then
  begin
    Result.MapMode := 0;
    Exit;
  end;

  Result.MapMode := GetMapMode(ACanvas.Handle);  // 返回映射方式，零则失败
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);  // 逻辑单位转换成具有任意比例轴的任意单位，用SetWindowsEx和SetViewportExtEx函数指定单位、方向和需要的比例
  SetWindowOrgEx(ACanvas.Handle, 0, 0, @Result.WindowOrg);  // 用指定的坐标设置设备环境的窗口原点
  SetWindowExtEx(ACanvas.Handle, FWindowWidth, FWindowHeight, @Result.WindowExt);  // 为设备环境设置窗口的水平的和垂直的范围

  SetViewportOrgEx(ACanvas.Handle, 0, 0, @Result.ViewportOrg);  // 哪个设备点映射到窗口原点(0,0)
  // 用指定的值来设置指定设备环境坐标的X轴、Y轴范围
  SetViewportExtEx(ACanvas.Handle, Round(FWindowWidth * FScaleX),
    Round(FWindowHeight * FScaleY), @Result.ViewportExt);
end;

end.
