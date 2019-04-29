{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                 文档页对象实现单元                    }
{                                                       }
{*******************************************************}

unit HCPage;

interface

uses
  Windows, Classes, HCCommon, HCUnitConversion;

type
  THCPaper = class(TObject)
  private
    FSize: Integer;  // 纸张大小如A4、B5等
    FWidth, FHeight: Single;  // 纸张宽、高（单位mm）
    FWidthPix, FHeightPix: Integer;  // 页面大小
    FMarginTop, FMarginLeft, FMarginRight, FMarginBottom: Single;  // 纸张边距（单位mm）
    FMarginTopPix, FMarginLeftPix, FMarginRightPix, FMarginBottomPix: Integer;  // 页边距
  protected
    procedure SetSize(const Value: Integer);
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetMarginTop(const Value: Single);
    procedure SetMarginLeft(const Value: Single);
    procedure SetMarginRight(const Value: Single);
    procedure SetMarginBottom(const Value: Single);
  public
    constructor Create;
    procedure SaveToStream(const AStream: TStream);
    procedure LoadToStream(const AStream: TStream; const AFileVersion: Word);
    // 纸张
    property Size: Integer read FSize write SetSize;
    property Width: Single read FWidth write SetWidth;
    property Height: Single read FHeight write SetHeight;
    property MarginTop: Single read FMarginTop write SetMarginTop;
    property MarginLeft: Single read FMarginLeft write SetMarginLeft;
    property MarginRight: Single read FMarginRight write SetMarginRight;
    property MarginBottom: Single read FMarginBottom write SetMarginBottom;
    /// <summary> 页宽(含页左右边距) </summary>
    property WidthPix: Integer read FWidthPix;
    /// <summary> 页高(含页眉、页脚) </summary>
    property HeightPix: Integer read FHeightPix;
    property MarginTopPix: Integer read FMarginTopPix;
    property MarginLeftPix: Integer read FMarginLeftPix;
    property MarginRightPix: Integer read FMarginRightPix;
    property MarginBottomPix: Integer read FMarginBottomPix;
  end;

  THCPage = class(TPersistent)
  private
    FStartDrawItemNo,    // 起始item
    FEndDrawItemNo       // 结束item
      : Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property StartDrawItemNo: Integer read FStartDrawItemNo write FStartDrawItemNo;
    property EndDrawItemNo: Integer read FEndDrawItemNo write FEndDrawItemNo;
  end;

  THCPages = class(TList)
  private
    function GetItem(Index: Integer): THCPage;
    procedure SetItem(Index: Integer; const Value: THCPage);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure ClearEx;
    procedure DeleteRange(const AIndex, ACount: Integer);
    property Items[Index: Integer]: THCPage read GetItem write SetItem; default;
  end;

implementation

{ THCPaper }

constructor THCPaper.Create;
begin
  MarginLeft := 25;
  MarginTop := 25;
  MarginRight := 20;
  MarginBottom := 20;
  FSize := DMPAPER_A4;  // 默认A4 210 297
  Width := 210;
  Height := 297;
end;

procedure THCPaper.SetWidth(const Value: Single);
begin
  FWidth := Value;
  FWidthPix := MillimeterToPixX(FWidth);
end;

procedure THCPaper.LoadToStream(const AStream: TStream; const AFileVersion: Word);
var
  vPaperSize: Integer;
  vSize: Single;
  vDataSize: Int64;
begin
  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));

  AStream.ReadBuffer(vPaperSize, SizeOf(vPaperSize));
  FSize := vPaperSize;

  AStream.ReadBuffer(vSize, SizeOf(FWidth));
  Width := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FHeight));
  Height := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FMarginLeft));
  MarginLeft := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FMarginTop));
  MarginTop := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FMarginRight));
  MarginRight := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FMarginBottom));
  MarginBottom := vSize;
end;

procedure THCPaper.SaveToStream(const AStream: TStream);
var
  vBegPos, vEndPos: Int64;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 数据大小占位
  //
  AStream.WriteBuffer(FSize, SizeOf(FSize));
  AStream.WriteBuffer(FWidth, SizeOf(FWidth));
  AStream.WriteBuffer(FHeight, SizeOf(FHeight));
  AStream.WriteBuffer(FMarginLeft, SizeOf(FMarginLeft));
  AStream.WriteBuffer(FMarginTop, SizeOf(FMarginTop));
  AStream.WriteBuffer(FMarginRight, SizeOf(FMarginRight));
  AStream.WriteBuffer(FMarginBottom, SizeOf(FMarginBottom));
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 当前页数据大小
  AStream.Position := vEndPos;
end;

procedure THCPaper.SetHeight(const Value: Single);
begin
  FHeight := Value;
  FHeightPix := MillimeterToPixY(FHeight);
end;

procedure THCPaper.SetMarginBottom(const Value: Single);
begin
  FMarginBottom := Value;
  FMarginBottomPix := MillimeterToPixY(FMarginBottom);
end;

procedure THCPaper.SetMarginLeft(const Value: Single);
begin
  FMarginLeft := Value;
  FMarginLeftPix := MillimeterToPixX(FMarginLeft);
end;

procedure THCPaper.SetMarginRight(const Value: Single);
begin
  FMarginRight := Value;
  FMarginRightPix := MillimeterToPixX(FMarginRight);
end;

procedure THCPaper.SetMarginTop(const Value: Single);
begin
  FMarginTop := Value;
  FMarginTopPix := MillimeterToPixY(FMarginTop);
end;

procedure THCPaper.SetSize(const Value: Integer);
begin
  if FSize <> Value then
    FSize := Value;
end;

{ THCPage }

procedure THCPage.Assign(Source: TPersistent);
begin
  inherited;
  FStartDrawItemNo := (Source as THCPage).StartDrawItemNo;  // 起始item
  FEndDrawItemNo := (Source as THCPage).EndDrawItemNo;  // 结束item
end;

procedure THCPage.Clear;
begin
  FStartDrawItemNo := 0;    // 起始item
  FEndDrawItemNo := 0;      // 结束item
end;

constructor THCPage.Create;
begin
  Clear;
end;

destructor THCPage.Destroy;
begin
  inherited Destroy;
end;

{ THCPages }

procedure THCPages.ClearEx;
begin
  Count := 1;
  Items[0].Clear;
end;

procedure THCPages.DeleteRange(const AIndex, ACount: Integer);
var
  i, vEndIndex: Integer;
begin
  vEndIndex := AIndex + ACount;
  if vEndIndex > Count - 1 then
    vEndIndex := Count - 1;
  for i := vEndIndex downto AIndex do
    Delete(i);
end;

function THCPages.GetItem(Index: Integer): THCPage;
begin
  Result := THCPage(inherited Get(Index));
end;

procedure THCPages.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = TListNotification.lnDeleted then
    THCPage(Ptr).Free;
  inherited;
end;

procedure THCPages.SetItem(Index: Integer; const Value: THCPage);
begin
  inherited Put(Index, Value);
end;

end.
