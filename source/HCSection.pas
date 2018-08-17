{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-8-17             }
{                                                       }
{            文档支持浮动Item节功能实现单元             }
{                                                       }
{*******************************************************}

unit HCSection;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Generics.Collections, HCCustomSection,
  HCStyle, HCFloatItem, HCCommon;

type
  THCSection = class(THCCustomSection)  // 支持浮动Item的Section
  private
    FFloatItems: TObjectList<THCFloatItem>;  // THCItems支持Add时控制暂时不用
    FFloatItemIndex: Integer;

    function GetFloatItemAt(const X, Y: Integer): Integer;
  public
    constructor Create(const AStyle: THCStyle);
    destructor Destroy; override;

    procedure Clear; override;
    procedure GetPageCaretInfo(var ACaretInfo: TCaretInfo); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure PaintPage(const APageIndex, ALeft, ATop: Integer;
      const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo); override;

    /// <summary> 插入浮动Item </summary>
    function InsertFloatItem(const AFloatItem: THCFloatItem): Boolean;
  end;

implementation

{ THCSection }

procedure THCSection.Clear;
begin
  FFloatItems.Clear;
  inherited Clear;
end;

constructor THCSection.Create(const AStyle: THCStyle);
begin
  inherited Create(AStyle);
  FFloatItems := TObjectList<THCFloatItem>.Create;
  FFloatItemIndex := -1;
end;

destructor THCSection.Destroy;
begin
  FFloatItems.Free;
  inherited Destroy;
end;

function THCSection.GetFloatItemAt(const X, Y: Integer): Integer;
var
  i: Integer;
  vFloatItem: THCFloatItem;
  vRect: TRect;
begin
  Result := -1;
  for i := 0 to FFloatItems.Count - 1 do
  begin
    vFloatItem := FFloatItems[i];
    vRect := Bounds(vFloatItem.X, vFloatItem.Y, vFloatItem.Width, vFloatItem.Height);
    if PtInRect(vRect, Point(X, Y)) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure THCSection.GetPageCaretInfo(var ACaretInfo: TCaretInfo);
begin
  if FFloatItemIndex < 0 then
    inherited GetPageCaretInfo(ACaretInfo)
  else
    ACaretInfo.Visible := False;
end;

function THCSection.InsertFloatItem(const AFloatItem: THCFloatItem): Boolean;
begin
  FFloatItems.Add(AFloatItem);
  Result := True;
  Style.UpdateInfoRePaint;
  DoDataChanged(Self);
end;

procedure THCSection.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vItemIndex: Integer;
begin
  vItemIndex := GetFloatItemAt(X, Y - PagePadding);
  if FFloatItemIndex <> vItemIndex then
  begin
    if FFloatItemIndex >= 0 then
      FFloatItems[FFloatItemIndex].Active := False;

    FFloatItemIndex := vItemIndex;

    if FFloatItemIndex >= 0 then
      FFloatItems[FFloatItemIndex].Active := True;

    Style.UpdateInfoRePaint;
    Style.UpdateInfoReCaret;
  end;

  if vItemIndex < 0 then
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure THCSection.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vItemIndex: Integer;
begin
  vItemIndex := GetFloatItemAt(X, Y - PagePadding);
  if vItemIndex < 0 then
    inherited MouseMove(Shift, X, Y)
  else
    GCursor := crDefault;
end;

procedure THCSection.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure THCSection.PaintPage(const APageIndex, ALeft, ATop: Integer;
  const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
var
  i, vPageDrawTop, vPageDrawBottom: Integer;
  vFloatItem: THCFloatItem;
  vRect: TRect;
begin
  inherited PaintPage(APageIndex, ALeft, ATop, ACanvas, APaintInfo);

  vPageDrawTop := ATop;  // 映射到当前页面左上角为原点的起始位置(可为负数)
  vPageDrawBottom := vPageDrawTop + Self.PageHeightPix;  // 页面结束位置(可为负数)
  // 当前页数据能显示出来的区域边界
  //vPageDataScreenTop := Max(vPageDrawTop + vHeaderAreaHeight, 0);
  //vPageDataScreenBottom := Min(vPageDrawBottom - FPageSize.PageMarginBottomPix, vScaleHeight);

  for i := 0 to FFloatItems.Count - 1 do
  begin
    vFloatItem := FFloatItems[i];

    vRect := Bounds(vFloatItem.X, vFloatItem.Y, vFloatItem.Width, vFloatItem.Height);
    vRect.Offset(ALeft, ATop);
    vFloatItem.PaintTo(nil, vRect, vPageDrawTop, vPageDrawBottom, 0, 0, ACanvas, APaintInfo);
    //ACanvas.MoveTo(vFloatItem.X, vFloatItem.Y);
    //ACanvas.LineTo(vFloatItem.X + vFloatItem.Width, vFloatItem.Y + vFloatItem.Height);
  end;
end;

end.
