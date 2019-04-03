{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{            表格单元格内各类对象管理单元               }
{                                                       }
{*******************************************************}

unit HCTableCellData;

interface

uses
  Windows, Classes, Types, HCViewData, HCCustomData, HCStyle, HCCommon, HCStyleMatch;

type
  TGetRootDataEvent = function (): THCCustomData of object;

  THCTableCellData = class(THCViewData)
  private
    FActive,

    // 标识单元格全选状态(全选时点击虽然没在内部Item上也应标识在选中区域)
    // 因CellData如果是EmptyData时，全选并没有SelectEndItem，获取其选中状态时
    // 和未全选一样，所以需要自己记录全选状态
    FCellSelectedAll
      : Boolean;
    FCellHeight: Integer;  // 所属单元格高度(因合并或手动拖高，单元格高度会大于等于其内数据高度)
    FOnGetRootData: TGetRootDataEvent;
    function PointInCellRect(const APt: TPoint): Boolean;
  protected
    function GetHeight: Cardinal; override;

    /// <summary> 取消选中 </summary>
    /// <returns>取消时当前是否有选中，True：有选中；False：无选中</returns>
    function DisSelect: Boolean; override;

    /// <summary> 删除选中 </summary>
    function DeleteSelected: Boolean; override;

    procedure SetActive(const Value: Boolean);
  public
    procedure ApplySelectTextStyle(const AMatchStyle: THCStyleMatch); override;
    procedure ApplySelectParaStyle(const AMatchStyle: THCParaMatch); override;

    /// <summary> 全选 </summary>
    procedure SelectAll; override;

    /// <summary> 坐标是否在AItem的选中区域中 </summary>
    function CoordInSelect(const X, Y, AItemNo, AOffset: Integer;
      const ARestrain: Boolean): Boolean; override;

    /// <summary> 返回指定坐标下的Item和Offset </summary>
    procedure GetItemAt(const X, Y: Integer; var AItemNo, AOffset, ADrawItemNo: Integer;
      var ARestrain: Boolean); override;
    function GetRootData: THCCustomData; override;

    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;

    /// <summary> 选在第一个Item最前面 </summary>
    function SelectFirstItemOffsetBefor: Boolean;

    /// <summary> 选在最后一个Item最后面 </summary>
    function SelectLastItemOffsetAfter: Boolean;

    /// <summary> 选在第一行 </summary>
    function SelectFirstLine: Boolean;

    /// <summary> 选在最后一行 </summary>
    function SelectLastLine: Boolean;

    /// <summary> 清除并返回为处理分页比净高增加的高度(为重新格式化时后面计算偏移用) </summary>
    function ClearFormatExtraHeight: Integer;

    /// <summary> 单元格全选状态(如果是EmptyData时，跨单元格划选时为True) </summary>
    property CellSelectedAll: Boolean read FCellSelectedAll write FCellSelectedAll;

    /// <summary> 所属单元格高度 </summary>
    property CellHeight: Integer read FCellHeight write FCellHeight;
    // 用于表格切换编辑的单元格
    property Active: Boolean read FActive write SetActive;

    property OnGetRootData: TGetRootDataEvent read FOnGetRootData write FOnGetRootData;
  end;

implementation

uses
  HCRectItem, HCItem;

{ THCTableCellData }

procedure THCTableCellData.ApplySelectParaStyle(const AMatchStyle: THCParaMatch);
begin
  if FCellSelectedAll then  // 单元格全选时，应用样式以第一个为准
    CurParaNo := Items[0].ParaNo;

  inherited ApplySelectParaStyle(AMatchStyle);
end;

procedure THCTableCellData.ApplySelectTextStyle(const AMatchStyle: THCStyleMatch);
begin
  if FCellSelectedAll then  // 单元格全选时，应用样式以第一个为准
    CurStyleNo := Items[0].StyleNo;

  inherited ApplySelectTextStyle(AMatchStyle);
end;

function THCTableCellData.ClearFormatExtraHeight: Integer;
var
  i, vFmtOffset, vFormatIncHight: Integer;
begin
  Result := 0;
  vFmtOffset := 0;
  for i := 1 to DrawItems.Count - 1 do
  begin
    if DrawItems[i].LineFirst then
    begin
      if DrawItems[i].Rect.Top <> DrawItems[i - 1].Rect.Bottom then
      begin
        vFmtOffset := DrawItems[i].Rect.Top - DrawItems[i - 1].Rect.Bottom;
        if vFmtOffset > Result then
          Result := vFmtOffset;
      end;
    end;

    OffsetRect(DrawItems[i].Rect, 0, -vFmtOffset);

    if Items[DrawItems[i].ItemNo].StyleNo < THCStyle.Null then  // RectItem如表格，在格式化时有行和行中间的偏移，新格式化时要恢复，由分页函数再处理新格式化后的偏移
    begin
      vFormatIncHight := (Items[DrawItems[i].ItemNo] as THCCustomRectItem).ClearFormatExtraHeight;
      DrawItems[i].Rect.Bottom := DrawItems[i].Rect.Bottom - vFormatIncHight;
    end;
  end;
end;

function THCTableCellData.CoordInSelect(const X, Y, AItemNo,
  AOffset: Integer; const ARestrain: Boolean): Boolean;
begin
  if FCellSelectedAll then
    Result := PointInCellRect(Point(X, Y))
  else
    Result := inherited CoordInSelect(X, Y, AItemNo, AOffset, ARestrain);
end;

function THCTableCellData.DeleteSelected: Boolean;
begin
  Result := inherited DeleteSelected;
  FCellSelectedAll := False;
end;

function THCTableCellData.DisSelect: Boolean;
begin
  Result := inherited DisSelect;
  FCellSelectedAll := False;
end;

function THCTableCellData.GetHeight: Cardinal;
begin
  Result := inherited GetHeight;
  if DrawItems.Count > 0 then
    Result := Result + DrawItems[0].Rect.Top;
end;

procedure THCTableCellData.GetItemAt(const X, Y: Integer; var AItemNo, AOffset,
  ADrawItemNo: Integer; var ARestrain: Boolean);
begin
  inherited GetItemAt(X, Y, AItemNo, AOffset, ADrawItemNo, ARestrain);
  if FCellSelectedAll then
    ARestrain := not PointInCellRect(Point(X, Y))
end;

function THCTableCellData.GetRootData: THCCustomData;
begin
  if Assigned(FOnGetRootData) then
    Result := FOnGetRootData
  else
    Result := inherited GetRootData;
end;

procedure THCTableCellData.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  Self.BeginFormat;
  try
    inherited LoadFromStream(AStream, AStyle, AFileVersion);
  finally
    Self.EndFormat(False);
  end;
end;

function THCTableCellData.PointInCellRect(const APt: TPoint): Boolean;
begin
  Result := PtInRect(Bounds(0, 0, Width, FCellHeight), APt);
end;

procedure THCTableCellData.SelectAll;
begin
  inherited SelectAll;
  FCellSelectedAll := True;
end;

function THCTableCellData.SelectFirstItemOffsetBefor: Boolean;
begin
  Result := False;
  if (not SelectExists) and (SelectInfo.StartItemNo = 0) then
    Result := SelectInfo.StartItemOffset = 0;
end;

function THCTableCellData.SelectFirstLine: Boolean;
begin
  Result := Self.GetParaFirstItemNo(SelectInfo.StartItemNo) = 0;
end;

function THCTableCellData.SelectLastItemOffsetAfter: Boolean;
begin
  Result := False;
  if (not SelectExists) and (SelectInfo.StartItemNo = Self.Items.Count - 1) then  // 最后一个
    Result := SelectInfo.StartItemOffset = Self.GetItemOffsetAfter(SelectInfo.StartItemNo);
end;

function THCTableCellData.SelectLastLine: Boolean;
begin
  Result := Self.GetParaLastItemNo(SelectInfo.StartItemNo) = Self.Items.Count - 1;
end;

procedure THCTableCellData.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
    FActive := Value;

  if not FActive then
  begin
    {if Self.MouseDownItemNo >= 0 then
      Self.Items[Self.MouseDownItemNo].Active := False;}
    Self.DisSelect;
    Self.InitializeField;
    Style.UpdateInfoRePaint;
  end;
end;

end.
