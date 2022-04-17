{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{            ���Ԫ���ڸ���������Ԫ               }
{                                                       }
{*******************************************************}

unit HCTableCellData;

interface

uses
  Windows, Classes, Types, HCViewData, HCCustomData, HCStyle, HCCommon, HCStyleMatch;

type
  TGetRootDataEvent = function (): THCCustomData of object;
  TGetFormatTopFun = function(const ACellData: THCCustomData): Integer of object;

  THCTableCellData = class(THCViewData)
  private
    FActive,

    // ��ʶ��Ԫ��ȫѡ״̬(ȫѡʱ�����Ȼû���ڲ�Item��ҲӦ��ʶ��ѡ������)
    // ��CellData�����EmptyDataʱ��ȫѡ��û��SelectEndItem����ȡ��ѡ��״̬ʱ
    // ��δȫѡһ����������Ҫ�Լ���¼ȫѡ״̬
    FCellSelectedAll
      : Boolean;
    FCellHeight: Integer;  // ������Ԫ��߶�(��ϲ����ֶ��ϸߣ���Ԫ��߶Ȼ���ڵ����������ݸ߶�)
    FOnSetFormatHeightChange: TNotifyEvent;
    FOnGetRootData: TGetRootDataEvent;
    FOnGetFormatTop: TGetFormatTopFun;
    function PointInCellRect(const APt: TPoint): Boolean;
  protected
    function GetHeight: Cardinal; override;

    /// <summary> ȡ��ѡ�� </summary>
    /// <returns>ȡ��ʱ��ǰ�Ƿ���ѡ�У�True����ѡ�У�False����ѡ��</returns>
    function DisSelect: Boolean; override;
    /// <summary> ɾ��ѡ�� </summary>
    function DeleteSelected: Boolean; override;

    procedure DoLoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;

    procedure ReSetSelectAndCaret(const AItemNo, AOffset: Integer;
      const ANextWhenMid: Boolean = False); override;

    procedure ReFormatData(const AFirstDrawItemNo: Integer; const ALastItemNo: Integer = -1;
      const AExtraItemCount: Integer = 0; const AForceClearExtra: Boolean = False); override;

    procedure DoSetFormatHeightChange;
    procedure SetActive(const Value: Boolean);
    function GetFormatTop: Integer;
  public
    procedure ApplySelectTextStyle(const AMatchStyle: THCStyleMatch); override;
    procedure ApplySelectParaStyle(const AMatchStyle: THCParaMatch); override;
    function GetDrawItemFormatTop(const ADrawItemNo: Integer): Integer; override;

    /// <summary> ȫѡ </summary>
    procedure SelectAll; override;

    /// <summary> �����Ƿ���AItem��ѡ�������� </summary>
    function CoordInSelect(const X, Y, AItemNo, AOffset: Integer;
      const ARestrain: Boolean): Boolean; override;

    /// <summary> ����ָ�������µ�Item��Offset </summary>
    procedure GetItemAt(const X, Y: Integer; var AItemNo, AOffset, ADrawItemNo: Integer;
      var ARestrain: Boolean); override;
    function GetRootData: THCCustomData; override;
    procedure SetFormatHeightChange; override;
    /// <summary> ѡ�ڵ�һ��Item��ǰ�� </summary>
    function SelectFirstItemOffsetBefor: Boolean;

    /// <summary> ѡ�����һ��Item����� </summary>
    function SelectLastItemOffsetAfter: Boolean;

    /// <summary> ѡ�ڵ�һ�� </summary>
    function SelectFirstLine: Boolean;

    /// <summary> ѡ�����һ�� </summary>
    function SelectLastLine: Boolean;

    /// <summary> ���������Ϊ�����ҳ�Ⱦ������ӵĸ߶�(Ϊ���¸�ʽ��ʱ�������ƫ����) </summary>
    function ClearFormatExtraHeight: Integer;

    /// <summary> ��Ԫ��ȫѡ״̬(�����EmptyDataʱ���絥Ԫ��ѡʱΪTrue) </summary>
    property CellSelectedAll: Boolean read FCellSelectedAll write FCellSelectedAll;

    /// <summary> ������Ԫ��߶� </summary>
    property CellHeight: Integer read FCellHeight write FCellHeight;
    // ���ڱ���л��༭�ĵ�Ԫ��
    property Active: Boolean read FActive write SetActive;

    property OnGetRootData: TGetRootDataEvent read FOnGetRootData write FOnGetRootData;
    property OnGetFormatTop: TGetFormatTopFun read FOnGetFormatTop write FOnGetFormatTop;
    property OnSetFormatHeightChange: TNotifyEvent read FOnSetFormatHeightChange write FOnSetFormatHeightChange;
  end;

implementation

uses
  HCRectItem, HCItem;

{ THCTableCellData }

procedure THCTableCellData.ApplySelectParaStyle(const AMatchStyle: THCParaMatch);
begin
  if FCellSelectedAll then  // ��Ԫ��ȫѡʱ��Ӧ����ʽ�Ե�һ��Ϊ׼
    CurParaNo := Items[0].ParaNo;

  inherited ApplySelectParaStyle(AMatchStyle);
end;

procedure THCTableCellData.ApplySelectTextStyle(const AMatchStyle: THCStyleMatch);
begin
  if FCellSelectedAll then  // ��Ԫ��ȫѡʱ��Ӧ����ʽ�Ե�һ��Ϊ׼
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

    if Items[DrawItems[i].ItemNo].StyleNo < THCStyle.Null then  // RectItem�����ڸ�ʽ��ʱ���к����м��ƫ�ƣ��¸�ʽ��ʱҪ�ָ����ɷ�ҳ�����ٴ����¸�ʽ�����ƫ��
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

function THCTableCellData.GetDrawItemFormatTop(const ADrawItemNo: Integer): Integer;
begin
  Result := inherited GetDrawItemFormatTop(ADrawItemNo);
  Result := Result + GetFormatTop;
end;

function THCTableCellData.GetFormatTop: Integer;
begin
  if Assigned(FOnGetFormatTop) then
    Result := FOnGetFormatTop(Self)
  else
    Result := 0;
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

procedure THCTableCellData.DoLoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  Self.BeginFormat;
  try
    inherited DoLoadFromStream(AStream, AStyle, AFileVersion);
  finally
    Self.EndFormat(False);
  end;
end;

procedure THCTableCellData.DoSetFormatHeightChange;
begin
  if Assigned(FOnSetFormatHeightChange) then
    FOnSetFormatHeightChange(Self);
end;

function THCTableCellData.PointInCellRect(const APt: TPoint): Boolean;
begin
  Result := PtInRect(Bounds(0, 0, Width, FCellHeight), APt);
end;

procedure THCTableCellData.ReFormatData(const AFirstDrawItemNo, ALastItemNo,
  AExtraItemCount: Integer; const AForceClearExtra: Boolean);
begin
  inherited ReFormatData(AFirstDrawItemNo, ALastItemNo, AExtraItemCount, AForceClearExtra);
  Self.FormatChange := True;

  if Self.FormatHeightChange then
  begin
    SetFormatHeightChange;
    DoFormatDirty;
  end;
end;

procedure THCTableCellData.ReSetSelectAndCaret(const AItemNo, AOffset: Integer;
  const ANextWhenMid: Boolean);
begin
  if FActive then
    inherited ReSetSelectAndCaret(AItemNo, AOffset, ANextWhenMid)
  else
  begin
    Self.SelectInfo.Initialize;
    Self.SelectInfo.StartItemNo := AItemNo;
    Self.SelectInfo.StartItemOffset := AOffset;
    Self.GetActiveDomain;
  end;
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
  if (not SelectExists) and (SelectInfo.StartItemNo = Self.Items.Count - 1) then  // ���һ��
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

procedure THCTableCellData.SetFormatHeightChange;
begin
  Self.FormatChange := False;
  DoSetFormatHeightChange;
end;

end.
