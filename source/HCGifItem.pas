{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{           来获取更多的技术交流 2018-5-18              }
{                                                       }
{         文档GifItem(动画图像)对象实现单元             }
{                                                       }
{*******************************************************}

unit HCGifItem;

interface

uses
  Windows, Graphics, Classes, HCStyle, HCItem, HCRectItem, HCCustomData, HCXml,
  {$IFDEF DELPHIXE}Imaging.GIFImg{$ELSE}GIFImg{$ENDIF};

type
  THCGifItem = class(THCAnimateRectItem)
  private
    FDrawRect: TRect;
    FGifImage: TGIFImage;
    procedure DoImageAnimate(Sender: TObject);
    function GetAnimate: Boolean;
    procedure SetAnimate(const Value: Boolean);
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    //
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure LoadFromFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function ToHtml(const APath: string): string; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property Image: TGIFImage read FGifImage;
    property Animate: Boolean read GetAnimate write SetAnimate;
  end;

implementation

uses
  SysUtils;

{ THCGifItem }

procedure THCGifItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FGifImage.Assign((Source as THCGifItem).Image);
end;

constructor THCGifItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  FGifImage := TGIFImage.Create;
  FGifImage.OnChange := DoImageAnimate;
  StyleNo := THCStyle.Gif;
end;

destructor THCGifItem.Destroy;
begin
  FGifImage.Animate := False;
  FreeAndNil(FGifImage);
  inherited;
end;

procedure THCGifItem.DoImageAnimate(Sender: TObject);
begin
  if FGifImage.Animate then
    OwnerData.Style.InvalidateRect(FDrawRect);
end;

procedure THCGifItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  FDrawRect := ADrawRect;
  if APaintInfo.Print then
    ACanvas.Draw(ADrawRect.Left, ADrawRect.Top, FGifImage.Bitmap)
  else
    //ACanvas.StretchDraw(ADrawRect, FGifImage);
    ACanvas.Draw(ADrawRect.Left, ADrawRect.Top, FGifImage);

  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);
end;

function THCGifItem.GetAnimate: Boolean;
begin
  Result := FGifImage.Animate;
end;

function THCGifItem.GetHeight: Integer;
begin
  Result := inherited GetHeight;
  if Result = 0 then
    Result := FGifImage.Height;
end;

function THCGifItem.GetWidth: Integer;
begin
  Result := inherited GetWidth;
  if Result = 0 then
    Result := FGifImage.Width;
end;

procedure THCGifItem.LoadFromFile(const AFileName: string);
begin
  FGifImage.LoadFromFile(AFileName);
  Self.Width := FGifImage.Width;
  Self.Height := FGifImage.Height;
  FGifImage.Animate := True;
end;

procedure THCGifItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vImgSize: Cardinal;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);

  if AFileVersion < 21 then
    FGifImage.LoadFromStream(AStream)
  else
  begin
    AStream.ReadBuffer(vImgSize, SizeOf(vImgSize));
    if vImgSize > 0 then
      FGifImage.LoadFromStream(AStream);  // 会触发OnChange
  end;

  Self.Width := FGifImage.Width;
  Self.Height := FGifImage.Height;
  FGifImage.Animate := True;
end;

procedure THCGifItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  Base64ToGraphic(ANode.Text, FGifImage);
  Self.Width := FGifImage.Width;
  Self.Height := FGifImage.Height;
  FGifImage.Animate := True;
end;

procedure THCGifItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vStream: TMemoryStream;
  vSize: Cardinal;
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  //FGifImage.SaveToStream(AStream);

  vStream := TMemoryStream.Create;  // 兼容其他语言
  try
    FGifImage.SaveToStream(vStream);
    vSize := vStream.Size;
    AStream.WriteBuffer(vSize, SizeOf(vSize));
    AStream.WriteBuffer(vStream.Memory^, vStream.Size);
  finally
    vStream.Free;
  end;
end;

procedure THCGifItem.SetAnimate(const Value: Boolean);
begin
  if FGifImage.Animate <> Value then
    FGifImage.Animate := Value;
end;

function THCGifItem.ToHtml(const APath: string): string;
var
  vFileName: string;
begin
  if APath <> '' then  // 保存到指定的文件夹中
  begin
    if not FileExists(APath + 'images') then
      CreateDir(APath + 'images');
    vFileName := OwnerData.Style.GetHtmlFileTempName + '.gif';
    FGifImage.SaveToFile(APath + 'images\' + vFileName);
    Result := '<img width="' + IntToStr(Width) + '" height="' + IntToStr(Height)
      + '" src="images/' + vFileName + '" alt="THCGifItem" />';
  end
  else
  begin
    Result := '<img width="' + IntToStr(Width) + '" height="' + IntToStr(Height)
      + '" src="data:img/jpg;base64,' + GraphicToBase64(FGifImage) + '" alt="THCGifItem" />';
  end;
end;

procedure THCGifItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Text := GraphicToBase64(FGifImage);
end;

end.
