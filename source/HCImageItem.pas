{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{          文档ImageItem(图像)对象实现单元              }
{                                                       }
{*******************************************************}

unit HCImageItem;

interface

uses
  Windows, SysUtils, Graphics, Classes, HCStyle, HCItem, HCRectItem, HCCustomData;

type
  THCImageItem = class(THCResizeRectItem)
  private
    FImage: TBitmap;
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
    procedure PaintTop(const ACanvas: TCanvas); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    /// <summary> 约束到指定大小范围内 </summary>
    procedure RestrainSize(const AWidth, AHeight: Integer); override;
    procedure LoadFromBmpFile(const AFileName: string);

    function ToHtml(const APath: string): string; override;
    function ToXml: string; override;

    /// <summary> 恢复到原始尺寸 </summary>
    procedure RecoverOrigianlSize;
    property Image: TBitmap read FImage;
  end;

implementation

uses
  HCXml;

{ THCImageItem }

procedure THCImageItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FImage.Assign((Source as THCImageItem).Image);
end;

constructor THCImageItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  FImage := TBitmap.Create;
  FImage.OnChange := DoImageChange;
  StyleNo := THCStyle.Image;
end;

destructor THCImageItem.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure THCImageItem.DoImageChange(Sender: TObject);

  {$REGION ' TurnUpDown图像上下翻转(已废弃) '}
  // pf32位时图像数据是按行存放，每行按双字对齐，行按倒序方式存放
  {procedure TurnUpDown;
  var
    i, j, k, m: Integer;
    vRGBP: PByte;
    vRGBArr: array of array of Byte;
  begin
    //GetMem(vRGBArr, FImage.Height * FImage.Width * 3 * SizeOf(Byte));
    SetLength(vRGBArr, FImage.Height, FImage.Width * 3);

    k := 0;
    for i := FImage.Height - 1 downto 0 do
    begin
      vRGBP := FImage.ScanLine[i];

      m := 0;
      for j := 0 to FImage.Width - 1 do
      begin
        vRGBArr[k][m] := vRGBP^;
        Inc(vRGBP);
        Inc(m);
        vRGBArr[k][m] := vRGBP^;
        Inc(vRGBP);
        Inc(m);
        vRGBArr[k][m] := vRGBP^;
        Inc(vRGBP);
        Inc(m);
      end;

      Inc(k);
    end;

    k := 0;
    for i := 0 to FImage.Height - 1 do
    begin
      vRGBP := FImage.ScanLine[i];

      m := 0;
      for j := 0 to FImage.Width - 1 do
      begin
        vRGBP^ := vRGBArr[k][m];
        Inc(vRGBP);
        Inc(m);
        vRGBP^ := vRGBArr[k][m];
        Inc(vRGBP);
        Inc(m);
        vRGBP^ := vRGBArr[k][m];
        Inc(vRGBP);
        Inc(m);
      end;

      Inc(k);
    end;

    SetLength(vRGBArr, 0);
    // FreeMem(PArr);
  end;}
  {$ENDREGION}

begin
  if FImage.PixelFormat <> pf24bit then
  begin
    FImage.PixelFormat := pf24bit;
    {if FImage.PixelFormat = pf32bit then
      //TurnUpDown;
      //FImage.Canvas.CopyMode:= cmSrcCopy;
      FImage.Canvas.CopyRect(FImage.Canvas.ClipRect, FImage.Canvas, Rect(0, FImage.Height, FImage.Width, 0));}
  end;
end;

procedure THCImageItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  ACanvas.StretchDraw(ADrawRect, FImage);

  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);
end;

function THCImageItem.GetHeight: Integer;
begin
  Result := inherited GetHeight;
  if Result = 0 then
    Result := FImage.Height;
end;

function THCImageItem.GetWidth: Integer;
begin
  Result := inherited GetWidth;
  if Result = 0 then
    Result := FImage.Width;
end;

procedure THCImageItem.LoadFromBmpFile(const AFileName: string);
begin
  FImage.LoadFromFile(AFileName);  // 会触发OnChange
  Self.Width := FImage.Width;
  Self.Height := FImage.Height;
end;

procedure THCImageItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vImgSize: Cardinal;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);

  if AFileVersion < 20 then
    FImage.LoadFromStream(AStream)  // 会触发OnChange
  else  // 兼容C#版本
  begin
    AStream.ReadBuffer(vImgSize, SizeOf(vImgSize));
    if vImgSize > 0 then
      FImage.LoadFromStream(AStream);  // 会触发OnChange
  end;
end;

procedure THCImageItem.PaintTop(const ACanvas: TCanvas);
var
  vBlendFunction: TBlendFunction;
begin
  {ACanvas.StretchDraw(Bounds(ResizeRect.Left, ResizeRect.Top, ResizeWidth, ResizeHeight),
    FImage);}
  vBlendFunction.BlendOp := AC_SRC_OVER;
  vBlendFunction.BlendFlags := 0;
  vBlendFunction.AlphaFormat := AC_SRC_OVER;  // 通常为 0，如果源位图为32位真彩色，可为 AC_SRC_ALPHA
  vBlendFunction.SourceConstantAlpha := 128; // 透明度
  Windows.AlphaBlend(ACanvas.Handle,
                     ResizeRect.Left,
                     ResizeRect.Top,
                     ResizeWidth,
                     ResizeHeight,
                     FImage.Canvas.Handle,
                     0,
                     0,
                     FImage.Width,
                     FImage.Height,
                     vBlendFunction
                     );
  inherited PaintTop(ACanvas);
end;

procedure THCImageItem.RecoverOrigianlSize;
begin
  Width := FImage.Width;
  Height := FImage.Height;
end;

procedure THCImageItem.RestrainSize(const AWidth, AHeight: Integer);
var
  vBL: Single;
begin
  if Width > AWidth then
  begin
    vBL := Width / AWidth;
    Width := AWidth;
    Height := Round(Height / vBL);
  end;

  if Height > AHeight then
  begin
    vBL := Height / AHeight;
    Height := AHeight;
    Width := Round(Width / vBL);
  end;
end;

procedure THCImageItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vStream: TMemoryStream;
  vSize: Cardinal;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vStream := TMemoryStream.Create;
  try
    FImage.SaveToStream(vStream);
    vSize := vStream.Size;
    AStream.WriteBuffer(vSize, SizeOf(vSize));
    AStream.WriteBuffer(vStream.Memory^, vStream.Size);
  finally
    vStream.Free;
  end;
end;

function THCImageItem.ToHtml(const APath: string): string;
var
  vFile: string;
begin
  if APath <> '' then  // 保存到指定的文件夹中
  begin
    if not FileExists(APath + 'images') then
      CreateDir(APath + 'images');
    vFile := OwnerData.Style.GetHtmlFileTempName + '.bmp';
    FImage.SaveToFile(APath + 'images\' + vFile);
    Result := '<img width="' + IntToStr(Width) + '" height="' + IntToStr(Height)
      + '" src="images/' + vFile + '" alt="THCImageItem" />';
  end
  else  // 保存为Base64
  begin
    Result := '<img width="' + IntToStr(Width) + '" height="' + IntToStr(Height)
      + '" src="data:img/jpg;base64,' + BitmapToBase64(FImage) + '" alt="THCImageItem" />';
  end;
end;

function THCImageItem.ToXml: string;
begin
  Result := '<image ' + inherited ToXml + '">' + BitmapToBase64(FImage) + '</image>';
end;

end.
