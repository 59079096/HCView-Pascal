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
  Windows, Graphics, Classes, HCStyle, HCItem, HCRectItem, HCCustomData, Vcl.Imaging.GIFImg;

type
  THCGifItem = class(THCAnimateRectItem)
  private
    FStyle: THCStyle;
    FDrawRect: TRect;
    FImage: TGIFImage;
    procedure DoImageChange(Sender: TObject);
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    //
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    property Image: TGIFImage read FImage;
  end;

implementation

{ THCGifItem }

constructor THCGifItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  FStyle := AOwnerData.Style;
  FImage := TGIFImage.Create;
  FImage.OnChange := DoImageChange;
  StyleNo := THCStyle.Gif;
end;

destructor THCGifItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure THCGifItem.DoImageChange(Sender: TObject);
begin
  Self.Width := FImage.Width;
  Self.Height := FImage.Height;
  FStyle.InvalidateRect(FDrawRect);
end;

procedure THCGifItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  FDrawRect := ADrawRect;
  //ACanvas.StretchDraw(ADrawRect, FImage);
  ACanvas.Draw(ADrawRect.Left, ADrawRect.Top, FImage);
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);
end;

function THCGifItem.GetHeight: Integer;
begin
  Result := inherited GetHeight;
  if Result = 0 then
    Result := FImage.Height;
end;

function THCGifItem.GetWidth: Integer;
begin
  Result := inherited GetWidth;
  if Result = 0 then
    Result := FImage.Width;
end;

procedure THCGifItem.LoadFromFile(const AFileName: string);
begin
  FImage.LoadFromFile(AFileName);
  FImage.Animate := True;
end;

procedure THCGifItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  FImage.LoadFromStream(AStream);
  FImage.Animate := True;
end;

procedure THCGifItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  FImage.SaveToStream(AStream);
end;

end.
