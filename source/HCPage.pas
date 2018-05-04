{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  Windows, Classes, HCCommon;

type
  THCPageSize = class(TObject)
  private
    FPixelsPerInchX, FPixelsPerInchY: Single;
    FPaperSize: Integer;
    FPaperWidth, FPaperHeight: Single;  // 纸张大小（单位mm）
    FPageWidthPix, FPageHeightPix: Integer;  // 页面大小
    FPaperMarginTop, FPaperMarginLeft, FPaperMarginRight, FPaperMarginBottom: Single;  // 纸张边距（单位mm）
    FPageMarginTopPix, FPageMarginLeftPix, FPageMarginRightPix, FPageMarginBottomPix: Integer;  // 页边距
  protected
    procedure SetPaperSize(const Value: Integer);
    procedure SetPaperWidth(const Value: Single);
    procedure SetPaperHeight(const Value: Single);
    procedure SetPaperMarginTop(const Value: Single);
    procedure SetPaperMarginLeft(const Value: Single);
    procedure SetPaperMarginRight(const Value: Single);
    procedure SetPaperMarginBottom(const Value: Single);
  public
    constructor Create(const APixelsPerInchX, APixelsPerInchY: Single);  // 屏幕1英寸dpi数
    procedure SaveToStream(const AStream: TStream);
    procedure LoadToStream(const AStream: TStream; const AFileVersion: Word);
    // 纸张
    property PaperSize: Integer read FPaperSize write SetPaperSize;
    property PaperWidth: Single read FPaperWidth write SetPaperWidth;
    property PaperHeight: Single read FPaperHeight write SetPaperHeight;
    property PaperMarginTop: Single read FPaperMarginTop write SetPaperMarginTop;
    property PaperMarginLeft: Single read FPaperMarginLeft write SetPaperMarginLeft;
    property PaperMarginRight: Single read FPaperMarginRight write SetPaperMarginRight;
    property PaperMarginBottom: Single read FPaperMarginBottom write SetPaperMarginBottom;
    // 页面
    property PageWidthPix: Integer read FPageWidthPix write FPageWidthPix;
    property PageHeightPix: Integer read FPageHeightPix write FPageHeightPix;
    property PageMarginTopPix: Integer read FPageMarginTopPix write FPageMarginTopPix;
    property PageMarginLeftPix: Integer read FPageMarginLeftPix write FPageMarginLeftPix;
    property PageMarginRightPix: Integer read FPageMarginRightPix write FPageMarginRightPix;
    property PageMarginBottomPix: Integer read FPageMarginBottomPix write FPageMarginBottomPix;
  end;

  PHCPage = ^THCPage;
  THCPage = class(TPersistent)
  public
    StartDrawItemNo,    // 起始item
    EndDrawItemNo       // 结束item
      : Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
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

{ THCPageSize }

constructor THCPageSize.Create(const APixelsPerInchX, APixelsPerInchY: Single);
begin
  FPixelsPerInchX := APixelsPerInchX;
  FPixelsPerInchY := APixelsPerInchY;
  PaperMarginLeft := 30;
  PaperMarginTop := 25;
  PaperMarginRight := 20;
  PaperMarginBottom := 20;
  // 默认A4
  PaperWidth := 210;
  PaperHeight := 297;  // 100
end;

procedure THCPageSize.SetPaperWidth(const Value: Single);
begin
  FPaperWidth := Value;
  FPageWidthPix := Round(FPaperWidth * FPixelsPerInchX);
end;

procedure THCPageSize.LoadToStream(const AStream: TStream; const AFileVersion: Word);
var
  vPaperSize: Integer;
  vSize: Single;
var
  vDataSize: Int64;
begin
  AStream.ReadBuffer(vDataSize, SizeOf(vDataSize));

  AStream.ReadBuffer(vPaperSize, SizeOf(vPaperSize));
  PaperSize := vPaperSize;

  AStream.ReadBuffer(vSize, SizeOf(FPaperWidth));
  PaperWidth := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FPaperHeight));
  PaperHeight := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FPaperMarginLeft));
  PaperMarginLeft := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FPaperMarginTop));
  PaperMarginTop := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FPaperMarginRight));
  PaperMarginRight := vSize;

  AStream.ReadBuffer(vSize, SizeOf(FPaperMarginBottom));
  PaperMarginBottom := vSize;
end;

procedure THCPageSize.SaveToStream(const AStream: TStream);
var
  vBegPos, vEndPos: Int64;
begin
  vBegPos := AStream.Position;
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 数据大小占位
  //
  AStream.WriteBuffer(FPaperSize, SizeOf(FPaperSize));
  AStream.WriteBuffer(FPaperWidth, SizeOf(FPaperWidth));
  AStream.WriteBuffer(FPaperHeight, SizeOf(FPaperHeight));
  AStream.WriteBuffer(FPaperMarginLeft, SizeOf(FPaperMarginLeft));
  AStream.WriteBuffer(FPaperMarginTop, SizeOf(FPaperMarginTop));
  AStream.WriteBuffer(FPaperMarginRight, SizeOf(FPaperMarginRight));
  AStream.WriteBuffer(FPaperMarginBottom, SizeOf(FPaperMarginBottom));
  //
  vEndPos := AStream.Position;
  AStream.Position := vBegPos;
  vBegPos := vEndPos - vBegPos - SizeOf(vBegPos);
  AStream.WriteBuffer(vBegPos, SizeOf(vBegPos));  // 当前页数据大小
  AStream.Position := vEndPos;
end;

procedure THCPageSize.SetPaperHeight(const Value: Single);
begin
  FPaperHeight := Value;
  FPageHeightPix := Round(FPaperHeight * FPixelsPerInchY);
end;

procedure THCPageSize.SetPaperMarginBottom(const Value: Single);
begin
  FPaperMarginBottom := Value;
  FPageMarginBottomPix := Round(FPaperMarginBottom * FPixelsPerInchY);
end;

procedure THCPageSize.SetPaperMarginLeft(const Value: Single);
begin
  FPaperMarginLeft := Value;
  FPageMarginLeftPix := Round(FPaperMarginLeft * FPixelsPerInchX);
end;

procedure THCPageSize.SetPaperMarginRight(const Value: Single);
begin
  FPaperMarginRight := Value;
  FPageMarginRightPix := Round(FPaperMarginRight * FPixelsPerInchX);
end;

procedure THCPageSize.SetPaperMarginTop(const Value: Single);
begin
  FPaperMarginTop := Value;
  FPageMarginTopPix := Round(FPaperMarginTop * FPixelsPerInchY);
end;

procedure THCPageSize.SetPaperSize(const Value: Integer);
begin
  if FPaperSize <> Value then
  begin
    FPaperSize := Value;
    case FPaperSize of
      DMPAPER_A4:
        begin
          PaperWidth := 210;
          PaperHeight := 297;
        end;
    end;
  end;
end;

{ THCPage }

procedure THCPage.Assign(Source: TPersistent);
begin
  inherited;
  StartDrawItemNo := (Source as THCPage).StartDrawItemNo;    // 起始item
  EndDrawItemNo := (Source as THCPage).EndDrawItemNo;      // 结束item
end;

constructor THCPage.Create;
begin
  StartDrawItemNo := -1;    // 起始item
  EndDrawItemNo := -1;      // 结束item
end;

destructor THCPage.Destroy;
begin
  inherited Destroy;
end;

{ THCPages }

procedure THCPages.ClearEx;
begin
  Count := 1;
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
