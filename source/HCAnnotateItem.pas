{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2021-9-9              }
{                                                       }
{              文档批注对象基类实现单元                 }
{                                                       }
{*******************************************************}

unit HCAnnotateItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCCustomData, HCItem, HCRectItem, HCStyle,
  HCCommon, HCXml, Generics.Collections, SysUtils;

type
  THCAnnotateContent = class
    Title, Text: string;
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
    procedure ToXml(const ANode: IHCXMLNode);
    procedure ParseXml(const ANode: IHCXMLNode);
  end;

  THCAnnotateItem = class(THCCustomRectItem)
  private
    FDrawRect: TRect;
    FContent: THCAnnotateContent;
    FReplys: TObjectList<THCAnnotateContent>;
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    ID: Cardinal;
    MarkType: TMarkType;
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    function IsBeginMark: Boolean;
    function IsEndMark: Boolean;
    function GetOffsetAt(const X: Integer): Integer; override;
    function JustifySplit: Boolean; override;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure PaintTop(const ACanvas: TCanvas); override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property Content: THCAnnotateContent read FContent;
    property Replys: TObjectList<THCAnnotateContent> read FReplys;
  end;

implementation

{ THCAnnotateItem }

procedure THCAnnotateItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FContent.Text := (Source as THCAnnotateItem).Content.Text;
  FContent.Title := (Source as THCAnnotateItem).Content.Title;
  ID := (Source as THCAnnotateItem).ID;
end;

constructor THCAnnotateItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := THCStyle.Annotate;
  FContent := THCAnnotateContent.Create;
  FReplys := TObjectList<THCAnnotateContent>.Create;
  ID := 0;
  Width := 0;
  Height := 0;
end;

destructor THCAnnotateItem.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(FReplys);
  inherited Destroy;
end;

procedure THCAnnotateItem.DoPaint(const AStyle: THCStyle;
  const ADrawRect: TRect; const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if not APaintInfo.Print then
  begin
    FDrawRect := ADrawRect;
    APaintInfo.TopItems.Add(Self);
  end;
end;

procedure THCAnnotateItem.FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer);
var
  vItem: THCCustomItem;
begin
  Self.Width := 0;
  Self.Height := ARichData.Style.TextStyles[0].FontHeight - ARichData.Style.LineSpaceMin;

  if MarkType = TMarkType.cmtBeg then
  begin
    if AItemNo < ARichData.Items.Count - 1 then
    begin
      vItem := ARichData.Items[AItemNo + 1];
      if (vItem.StyleNo = Self.StyleNo)
        and ((vItem as THCAnnotateItem).MarkType = TMarkType.cmtEnd)
      then
      begin
        Self.Width := 10;
      end
      else
      if vItem.ParaFirst then
        Self.Width := 10;
    end
    else
      Self.Width := 10;
  end
  else
  begin
    vItem := ARichData.Items[AItemNo - 1];
    if (vItem.StyleNo = Self.StyleNo)
      and ((vItem as THCAnnotateItem).MarkType = TMarkType.cmtBeg)
    then
    begin
      Self.Width := 10;
    end
    else
    if Self.ParaFirst then
      Self.Width := 10;
  end;
end;

function THCAnnotateItem.GetOffsetAt(const X: Integer): Integer;
begin
  if (X >= 0) and (X <= Width) then
  begin
    if MarkType = cmtBeg then
      Result := OffsetAfter
    else
      Result := OffsetBefor;
  end
  else
    Result := inherited GetOffsetAt(X);
end;

function THCAnnotateItem.IsBeginMark: Boolean;
begin
  Result := MarkType = TMarkType.cmtBeg;
end;

function THCAnnotateItem.IsEndMark: Boolean;
begin
  Result := MarkType = TMarkType.cmtEnd;
end;

function THCAnnotateItem.JustifySplit: Boolean;
begin
  Result := False;
end;

procedure THCAnnotateItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  i: Integer;
  vRepCount: Integer;
  vRepContent: THCAnnotateContent;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(ID, SizeOf(ID));
  AStream.ReadBuffer(MarkType, SizeOf(MarkType));
  if MarkType = TMarkType.cmtEnd then
  begin
    FContent.LoadFromStream(AStream, AFileVersion);
    AStream.ReadBuffer(vRepCount, SizeOf(vRepCount));
    for i := 0 to vRepCount - 1 do
    begin
      vRepContent := THCAnnotateContent.Create;
      vRepContent.LoadFromStream(AStream, AFileVersion);
      FReplys.Add(vRepContent);
    end;
  end;
end;

procedure THCAnnotateItem.PaintTop(const ACanvas: TCanvas);
begin
  inherited PaintTop(ACanvas);

  ACanvas.Pen.Width := 1;
  if MarkType = cmtBeg then
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clRed;
    //ACanvas.MoveTo(FDrawRect.Left + 2, FDrawRect.Top - 1);
    ACanvas.MoveTo(FDrawRect.Left, FDrawRect.Top - 1);
    ACanvas.LineTo(FDrawRect.Left, FDrawRect.Bottom + 1);
    //ACanvas.LineTo(FDrawRect.Left + 2, FDrawRect.Bottom + 1);
  end
  else
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clRed;
    //ACanvas.MoveTo(FDrawRect.Right - 2, FDrawRect.Top - 1);
    ACanvas.MoveTo(FDrawRect.Right, FDrawRect.Top - 1);
    ACanvas.LineTo(FDrawRect.Right, FDrawRect.Bottom + 1);
    //ACanvas.LineTo(FDrawRect.Right - 2, FDrawRect.Bottom + 1);
  end;
end;

procedure THCAnnotateItem.ParseXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vRepContent: THCAnnotateContent;
begin
  inherited ParseXml(ANode);
  ID := ANode.Attributes['id'];
  MarkType := TMarkType(ANode.Attributes['mark']);
  if MarkType = TMarkType.cmtEnd then
  begin
    for i := 0 to ANode.ChildNodes.Count - 1 do
    begin
      if ANode.ChildNodes[i].NodeName = 'content' then
         FContent.ParseXml(ANode.ChildNodes[i])
      else
      if ANode.ChildNodes[i].NodeName = 'rp' then
      begin
        vRepContent := THCAnnotateContent.Create;
        vRepContent.ParseXml(ANode.ChildNodes[i]);
      end;
    end;
  end;
end;

procedure THCAnnotateItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  i: Integer;
  vRepCount: Integer;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
  AStream.WriteBuffer(ID, SizeOf(ID));
  AStream.WriteBuffer(MarkType, SizeOf(MarkType));
  if MarkType = TMarkType.cmtEnd then
  begin
    FContent.SaveToStream(AStream);
    vRepCount := FReplys.Count;
    AStream.WriteBuffer(vRepCount, SizeOf(vRepCount));
    for i := 0 to vRepCount - 1 do
      FReplys[i].SaveToStream(AStream);
  end;
end;

procedure THCAnnotateItem.ToXml(const ANode: IHCXMLNode);
var
  i: Integer;
  vNode: IHCXMLNode;
begin
  inherited ToXml(ANode);
  ANode.Attributes['id'] := ID;
  ANode.Attributes['mark'] := Ord(MarkType);
  if MarkType = TMarkType.cmtEnd then
  begin
    vNode := ANode.AddChild('content');
    FContent.ToXml(vNode);

    if FReplys.Count > 0 then
    begin
      vNode := ANode.AddChild('replys');
      for i := 0 to FReplys.Count - 1 do
        FReplys[i].ToXml(vNode.AddChild('rp'));
    end;
  end;
end;

{ THCAnnotateContent }

procedure THCAnnotateContent.LoadFromStream(const AStream: TStream; const AFileVersion: Word);
begin
  HCLoadTextFromStream(AStream, Title, AFileVersion);
  HCLoadTextFromStream(AStream, Text, AFileVersion);
end;

procedure THCAnnotateContent.ParseXml(const ANode: IHCXMLNode);
begin
  Title := ANode.Attributes['title'];
  Text := ANode.Text;
end;

procedure THCAnnotateContent.SaveToStream(const AStream: TStream);
begin
  HCSaveTextToStream(AStream, Title);
  HCSaveTextToStream(AStream, Text);
end;

procedure THCAnnotateContent.ToXml(const ANode: IHCXMLNode);
begin
  ANode.Attributes['title'] := Title;
  ANode.Text := Text;
end;

end.
