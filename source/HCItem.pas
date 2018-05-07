{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  Windows, Classes, Controls, Graphics, Generics.Collections, HCStyle;

type
  TItemOptions = set of (ioParaFirst, ioSelectPart, ioSelectComplate);

  THCCustomItemClass = class of THCCustomItem;

  THCCustomItem = class;

  TPaintInfo = class(TObject)  // 绘制时的信息，用于给外部事件增加更多的信息
  private
    FPrint: Boolean;
    FTopItems: TObjectList<THCCustomItem>;
  public
    constructor Create;
    destructor Destroy; override;
    property Print: Boolean read FPrint write FPrint;

    /// <summary> 只管理不负责释放 </summary>
    property TopItems: TObjectList<THCCustomItem> read FTopItems;
  end;

  THCCustomItem = class(TObject)
  strict private
    FParaNo,
    FStyleNo,
    FFirstDItemNo: Integer;
    FActive, FVisible: Boolean;
    FOptions: TItemOptions;
  protected
    function GetParaFirst: Boolean;
    procedure SetParaFirst(const Value: Boolean);
    function GetSelectComplate: Boolean; virtual;
    function GetSelectPart: Boolean;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    procedure SetActive(const Value: Boolean); virtual;
    function GetLength: Integer; virtual;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); virtual;
  public
    constructor Create; virtual;

    procedure Assign(Source: THCCustomItem); virtual;
    /// <summary>
    /// 绘制Item的事件
    /// </summary>
    /// <param name="ACanvas"></param>
    /// <param name="ADrawRect">当前DrawItem的区域</param>
    /// <param name="ADataDrawBottom">Item所在的Data本次绘制底部位置</param>
    /// <param name="ADataScreenTop"></param>
    /// <param name="ADataScreenBottom"></param>
    procedure PaintTo(const AStyle: THCStyle; const ADrawRect: TRect;
      const APageDataDrawBottom, APageDataScreenTop, APageDataScreenBottom: Integer;
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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    function GetHint: string; virtual;
    procedure SelectComplate; virtual;
    procedure SelectPart;
    /// <summary> 从指定位置将当前item分成前后两部分 </summary>
    /// <param name="AOffset">分裂位置</param>
    /// <returns>后半部分对应的Item</returns>
    function BreakByOffset(const AOffset: Integer): THCCustomItem; virtual;
    procedure SaveToStream(const AStream: TStream); overload;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); overload; virtual;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); virtual;
    //
    property Options: TItemOptions read FOptions;
    property Text: string read GetText write SetText;
    property Length: Integer read GetLength;
    property ParaFirst: Boolean read GetParaFirst write SetParaFirst;

    property IsSelectComplate: Boolean read GetSelectComplate;
    property IsSelectPart: Boolean read GetSelectPart;

    property StyleNo: Integer read FStyleNo write FStyleNo;
    property ParaNo: Integer read FParaNo write FParaNo;
    property FirstDItemNo: Integer read FFirstDItemNo write FFirstDItemNo;
    property Active: Boolean read FActive write SetActive;
    property Visible: Boolean read FVisible write FVisible;
  end;

  TItemNotifyEvent = procedure(const AItem: THCCustomItem) of object;

  THCItems = class(TObjectList<THCCustomItem>)
  private
    FOnItemInsert: TItemNotifyEvent;
  protected
    procedure Notify(const Value: THCCustomItem; Action: TCollectionNotification); override;
  public
    property OnItemInsert: TItemNotifyEvent read FOnItemInsert write FOnItemInsert;
  end;

implementation

{ THCCustomItem }

function THCCustomItem.CanDrag: Boolean;
begin
  Result := True;
end;

procedure THCCustomItem.Assign(Source: THCCustomItem);
begin
  Self.FParaNo := Source.ParaNo;
  Self.FStyleNo := Source.StyleNo;
  Self.FOptions := Source.Options;
end;

function THCCustomItem.BreakByOffset(const AOffset: Integer): THCCustomItem;
begin
  // 继承者自己判断能否Break
  Result := THCCustomItemClass(Self.ClassType).Create;
  Result.Assign(Self);
  Result.ParaFirst := False;  // 打断后，后面的肯定不是断首
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
  FStyleNo := THCStyle.RsNull;
  FParaNo := THCStyle.RsNull;
  FFirstDItemNo := -1;
  FVisible := True;
end;

procedure THCCustomItem.DblClick(const X, Y: Integer);
begin
end;

procedure THCCustomItem.DisSelect;
begin
  FOptions := Self.Options - [ioSelectPart, ioSelectComplate];  // 处理自己的全选、部分选状态
end;

procedure THCCustomItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
end;

function THCCustomItem.GetHint: string;
begin
  Result := '';
end;

function THCCustomItem.GetLength: Integer;
begin
  Result := 0;
end;

function THCCustomItem.GetParaFirst: Boolean;
begin
  Result := ioParaFirst in FOptions;
end;

function THCCustomItem.GetSelectComplate: Boolean;
begin
  Result := ioSelectComplate in FOptions;
end;

function THCCustomItem.GetSelectPart: Boolean;
begin
  Result := ioSelectPart in FOptions;
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
begin
  //AStream.ReadBuffer(FStyleNo, SizeOf(FStyleNo));  // 由TCustomData.InsertStream处加载了
  AStream.ReadBuffer(FParaNo, SizeOf(FParaNo));
  AStream.ReadBuffer(vParFirst, SizeOf(vParFirst));
  ParaFirst := vParFirst;
end;

procedure THCCustomItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Active := True;
end;

procedure THCCustomItem.MouseEnter;
begin
end;

procedure THCCustomItem.MouseLeave;
begin
end;

procedure THCCustomItem.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure THCCustomItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure THCCustomItem.PaintTo(const AStyle: THCStyle; const ADrawRect: TRect;
  const APageDataDrawBottom, APageDataScreenTop, APageDataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vDCState: Integer;
begin
  vDCState := Windows.SaveDC(ACanvas.Handle);
  try
    DoPaint(AStyle, ADrawRect, APageDataDrawBottom, APageDataScreenTop,
      APageDataScreenBottom, ACanvas, APaintInfo);
  finally
    Windows.RestoreDC(ACanvas.Handle, vDCState);
    ACanvas.Refresh;  // 恢复Pen的修改
  end;
end;

procedure THCCustomItem.PaintTop(const ACanvas: TCanvas);
begin
end;

procedure THCCustomItem.SelectComplate;
begin
  Exclude(FOptions, ioSelectPart);
  Include(FOptions, ioSelectComplate);
end;

procedure THCCustomItem.SelectPart;
begin
  Exclude(FOptions, ioSelectComplate);
  Include(FOptions, ioSelectPart);
end;

procedure THCCustomItem.SetText(const Value: string);
begin
end;

procedure THCCustomItem.SaveToStream(const AStream: TStream);
begin
  SaveToStream(AStream, 0, Self.Length);
end;

procedure THCCustomItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vParFirst: Boolean;
begin
  AStream.WriteBuffer(FStyleNo, SizeOf(FStyleNo));
  AStream.WriteBuffer(FParaNo, SizeOf(FParaNo));

  vParFirst := ParaFirst;
  AStream.WriteBuffer(vParFirst, SizeOf(vParFirst));
end;

procedure THCCustomItem.SetActive(const Value: Boolean);
begin
  FActive := Value;
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
  inherited;
  case Action of
    cnAdded:
      begin
        if Assigned(FOnItemInsert) then
          FOnItemInsert(Value);
      end;

    cnRemoved: ;
    cnExtracted: ;
  end;
end;

{ TPaintInfo }

constructor TPaintInfo.Create;
begin
  FTopItems := TObjectList<THCCustomItem>.Create(False);  // 只管理不负责释放
end;

destructor TPaintInfo.Destroy;
begin
  FTopItems.Free;
  inherited Destroy;
end;

end.
