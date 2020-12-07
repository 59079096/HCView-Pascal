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

{$I HCView.inc}

uses
  Windows, SysUtils, Graphics, Classes, HCStyle, HCItem, HCRectItem, HCCustomData,
  HCXml{$IFNDEF BMPIMAGEITEM}, Wincodec{$ENDIF}, HCShape;

type
  THCImageItem = class(THCResizeRectItem)
  private
    FEmpty: Boolean;
    FImage: {$IFDEF BMPIMAGEITEM} TBitmap {$ELSE} TWICImage {$ENDIF};
    FShapeManager: THCShapeManager;
    procedure DoImageChange(Sender: TObject);
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
    procedure PaintTop(const ACanvas: TCanvas); override;
    procedure Clear; override;

    /// <summary> 约束到指定大小范围内 </summary>
    procedure RestrainSize(const AWidth, AHeight: Integer); override;
    procedure LoadGraphicFile(const AFileName: string; const AResize: Boolean = True);
    procedure LoadGraphicStream(const AStream: TStream; const AResize: Boolean = True);
    procedure ImageAssign(const AGraphic: TGraphic);

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function ToHtml(const APath: string): string; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summary> 恢复到原始尺寸 </summary>
    procedure RecoverOrigianlSize;
    property Image: {$IFDEF BMPIMAGEITEM} TBitmap {$ELSE} TWICImage {$ENDIF} read FImage;
    property Empty: Boolean read FEmpty;
    property ShapeManager: THCShapeManager read FShapeManager;
  end;

implementation

{ THCImageItem }

{$IFNDEF BMPIMAGEITEM}
function WICBitmap2Bitmap(const AWICBitmap: IWICBitmap; var ABMP: TBitmap): Boolean;
var
  vWicBitmap: IWICBitmapSource;
  vStride: Cardinal;
  vBuffer: TBytes;
  vBitmapInfo: TBitmapInfo;
  vWidth, vHeight: UInt32;
begin
  Result := False;
  if AWICBitmap = nil then Exit;
  if ABMP = nil then Exit;

  AWICBitmap.GetSize(vWidth, vHeight);
  vStride := vWidth * 4;
  SetLength(vBuffer, vStride * vHeight);

  WICConvertBitmapSource(GUID_WICPixelFormat32bppBGRA, AWICBitmap, vWicBitmap);
  vWicBitmap.CopyPixels(nil, vStride, Length(vBuffer), @vBuffer[0]);

  FillChar(vBitmapInfo, sizeof(vBitmapInfo), 0);
  with vBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(vBitmapInfo);
    biWidth := vWidth;
    biHeight := -vHeight;
    biPlanes := 1;
    biBitCount := 32;
  end;

  with ABMP do
  begin
    PixelFormat := pf32bit;
    SetSize(vWidth, vHeight);
    // DC par not used (ABMP.Canvas.Handle) since Usage = DIB_RGB_COLORS
    SetDIBits(0, Handle, 0, vHeight, @vBuffer[0], vBitmapInfo, DIB_RGB_COLORS);
    AlphaFormat := afDefined;
  end;
  Result := True;
end;

function Bitmap2WICBitmap(const ABMP: TBitmap; var AWICBitmap: IWicBitmap): Boolean;
var
  vPixelFormat: TGUID;
  vBitmapInfo: TBitmapInfo;
  vBuffer: TBytes;
  vWidth, vHeight: Int32;
begin
  Result := False;

  if ABMP.AlphaFormat = afDefined then
    vPixelFormat := GUID_WICPixelFormat32bppBGRA
  else
    vPixelFormat := GUID_WICPixelFormat32bppBGR;

  ABMP.PixelFormat := pf32bit;

  vWidth := ABMP.Width;
  vHeight := ABMP.Height;

  SetLength(vBuffer, vWidth * 4 * vHeight);

  FillChar(vBitmapInfo, sizeof(vBitmapInfo), 0);
  with vBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(vBitmapInfo);
    biWidth := vWidth;
    biHeight := -vHeight;
    biPlanes := 1;
    biBitCount := 32;
  end;
  // Forces evaluation of Bitmap.Handle before Bitmap.Canvas.Handle
  GetDIBits(ABMP.Canvas.Handle, ABMP.Handle, 0, vHeight, @vBuffer[0],
    vBitmapInfo, DIB_RGB_COLORS);

  TWICImage.ImagingFactory.CreateBitmapFromMemory(vWidth, vHeight, vPixelFormat,
    vWidth * 4, Length(vBuffer), @vBuffer[0], AWICBitmap);
end;
{$ENDIF}

procedure THCImageItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FImage.Assign((Source as THCImageItem).Image);
end;

procedure THCImageItem.Clear;
begin
  if Assigned(FImage) then
    FreeAndNil(FImage);

  FImage := {$IFDEF BMPIMAGEITEM} TBitmap {$ELSE} TWICImage {$ENDIF}.Create;
  FImage.OnChange := DoImageChange;
  FEmpty := True;
end;

constructor THCImageItem.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  StyleNo := THCStyle.Image;
  FShapeManager := THCShapeManager.Create;
  Clear;
end;

destructor THCImageItem.Destroy;
begin
  FImage.Free;
  FShapeManager.Free;
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
  {$IFDEF BMPIMAGEITEM}
  if FImage.PixelFormat <> pf24bit then
  begin
    FImage.PixelFormat := pf24bit;
    {if FImage.PixelFormat = pf32bit then
      //TurnUpDown;
      //FImage.Canvas.CopyMode:= cmSrcCopy;
      FImage.Canvas.CopyRect(FImage.Canvas.ClipRect, FImage.Canvas, Rect(0, FImage.Height, FImage.Width, 0));}
  end;
  {$ENDIF}
  FEmpty := FImage.Empty;
end;

procedure THCImageItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vBitmap: TBitmap;
  {$IFDEF BMPIMAGEITEM}
  vStream: TMemoryStream;
  {$ENDIF}
begin
  if APaintInfo.Print then
  begin
    if FEmpty then Exit;

    vBitmap := TBitmap.Create;
    try
      {$IFDEF BMPIMAGEITEM}
      vStream := TMemoryStream.Create;
      try
        FImage.SaveToStream(vStream);
        vStream.Position := 0;
        vBitmap.LoadFromStream(vStream);
      finally
        FreeAndNil(vStream);
      end;
      {$ELSE}
      vBitmap.SetSize(FImage.Width, FImage.Height);
      //WICBitmap2Bitmap(FImage.Handle, vBitmap);  // 这样png的打印出来透明的变成黑色了
      //vBitmap.AlphaFormat := TAlphaFormat.afIgnored;
      vBitmap.Canvas.Draw(0, 0, FImage);
      // 页眉最开始顶部是图片时，打印多页出现第一页丢失图片的问题，增加下面3行可以解决，暂时没找到具体原因
      ACanvas.Pen.Color := clWhite;
      ACanvas.Pen.Width := 1;
      ACanvas.Rectangle(ADrawRect);
      {$ENDIF}
      if (vBitmap.Width <> Width) or (vBitmap.Height <> Height) then
        ACanvas.StretchDraw(ADrawRect, vBitmap)
      else
        ACanvas.Draw(ADrawRect.Left, ADrawRect.Top, vBitmap);
    finally
      FreeAndNil(vBitmap);
    end;
  end
  else  // 非打印
  begin
    if (FImage.Width <> Width) or (FImage.Height <> Height) then
      ACanvas.StretchDraw(ADrawRect, FImage)
    else
      ACanvas.Draw(ADrawRect.Left, ADrawRect.Top, FImage);

    if FEmpty then
    begin
      ACanvas.Pen.Color := clBlack;
      ACanvas.Pen.Width := 1;
      ACanvas.Rectangle(ADrawRect);
    end;
  end;

  FShapeManager.PaintTo(ACanvas, ADrawRect, APaintInfo);

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

procedure THCImageItem.ImageAssign(const AGraphic: TGraphic);
begin
  FImage.Assign(AGraphic);
  FEmpty := False;
end;

procedure THCImageItem.LoadGraphicFile(const AFileName: string; const AResize: Boolean = True);
begin
  FImage.LoadFromFile(AFileName);  // BMPIMAGEITEM会触发OnChange WICImage不会触发
  if AResize then
  begin
    Self.Width := FImage.Width;
    Self.Height := FImage.Height;
  end;

  FEmpty := False;
end;

procedure THCImageItem.LoadGraphicStream(const AStream: TStream; const AResize: Boolean = True);
begin
  FImage.LoadFromStream(AStream);  // BMPIMAGEITEM会触发OnChange WICImage不会触发
  if AResize then
  begin
    Self.Width := FImage.Width;
    Self.Height := FImage.Height;
  end;

  FEmpty := False;
end;

procedure THCImageItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vImgSize: Cardinal;
  {$IFNDEF BMPIMAGEITEM}
  vStream: TMemoryStream;
  {$ENDIF}
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);

  if AFileVersion < 20 then
  begin
    FImage.LoadFromStream(AStream);  // 会触发OnChange
    FEmpty := False;
  end
  else  // 兼容C#版本
  begin
    AStream.ReadBuffer(vImgSize, SizeOf(vImgSize));
    if vImgSize > 0 then
    begin
      {$IFDEF BMPIMAGEITEM}
      FImage.LoadFromStream(AStream);  // 会触发OnChange
      {$ELSE}
      vStream := TMemoryStream.Create;
      try
        vStream.CopyFrom(AStream, vImgSize);
        vStream.Position := 0;
        FImage.LoadFromStream(vStream);
        FEmpty := False;
      finally
        FreeAndNil(vStream);
      end;
      {$ENDIF}
    end
    else
      FEmpty := True;
  end;

  if AFileVersion > 26 then
    FShapeManager.LoadFromStream(AStream);
end;

procedure THCImageItem.PaintTop(const ACanvas: TCanvas);
var
  vBlendFunction: TBlendFunction;
  {$IFNDEF BMPIMAGEITEM}
  vBmp: TBitmap;
  {$ENDIF}
begin
  if Self.Resizing then
  begin
    {$IFNDEF BMPIMAGEITEM}
    vBmp := TBitmap.Create;
    try
      //WICBitmap2Bitmap(FImage.Handle, vBmp);  // 透明的显示背景黑色了
      vBmp.SetSize(FImage.Width, FImage.Height);
      vBmp.Canvas.Draw(0, 0, FImage);
    {$ENDIF}

      vBlendFunction.BlendOp := AC_SRC_OVER;
      vBlendFunction.BlendFlags := 0;
      vBlendFunction.AlphaFormat := AC_SRC_OVER;  // 通常为 0，如果源位图为32位真彩色，可为 AC_SRC_ALPHA
      vBlendFunction.SourceConstantAlpha := 128; // 透明度
      Windows.AlphaBlend(ACanvas.Handle,
                         ResizeRect.Left,
                         ResizeRect.Top,
                         ResizeWidth,
                         ResizeHeight,
                         {$IFDEF BMPIMAGEITEM}
                         FImage.Canvas.Handle,
                         {$ELSE}
                         vBmp.Canvas.Handle,
                         {$ENDIF}
                         0,
                         0,
                         FImage.Width,
                         FImage.Height,
                         vBlendFunction
                         );
    {$IFNDEF BMPIMAGEITEM}
    finally
      FreeAndNil(vBmp);
    end;
    {$ENDIF}
  end;

  inherited PaintTop(ACanvas);
end;

procedure THCImageItem.ParseXml(const ANode: IHCXMLNode);
var
  vNode: IHCXMLNode;
begin
  inherited ParseXml(ANode);
  FEmpty := True;
  vNode := ANode.ChildNodes.FindNode('img');
  if Assigned(vNode) then  // 兼容27之前的文件
  begin
    if vNode.Text <> '' then
    begin
      Base64ToGraphic(vNode.Text, FImage);
      {$IFNDEF BMPIMAGEITEM}
      DoImageChange(Self);
      {$ENDIF}
    end;

    vNode := ANode.ChildNodes.FindNode('shapes');
    FShapeManager.ParseXml(vNode);
  end
  else
  begin
    if vNode.Text <> '' then
    begin
      Base64ToGraphic(ANode.Text, FImage);
      {$IFNDEF BMPIMAGEITEM}
      DoImageChange(Self);
      {$ENDIF}
    end;
  end;
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

procedure THCImageItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vStream: TMemoryStream;
  vSize: Cardinal;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vStream := TMemoryStream.Create;  // 兼容其他语言
  try
    FImage.SaveToStream(vStream);
    vSize := vStream.Size;
    AStream.WriteBuffer(vSize, SizeOf(vSize));
    AStream.WriteBuffer(vStream.Memory^, vStream.Size);
  finally
    vStream.Free;
  end;

  FShapeManager.SaveToStream(AStream);
end;

function THCImageItem.ToHtml(const APath: string): string;
var
  vFile: string;
begin
  if APath <> '' then  // 保存到指定的文件夹中
  begin
    if not DirectoryExists(APath + 'images') then
      CreateDir(APath + 'images');
    vFile := OwnerData.Style.GetHtmlFileTempName + '.bmp';
    FImage.SaveToFile(APath + 'images\' + vFile);
    Result := '<img width="' + IntToStr(Width) + '" height="' + IntToStr(Height)
      + '" src="images/' + vFile + '" alt="THCImageItem" />';
  end
  else  // 保存为Base64
  begin
    Result := '<img width="' + IntToStr(Width) + '" height="' + IntToStr(Height)
      + '" src="data:img/jpg;base64,' + GraphicToBase64(FImage) + '" alt="THCImageItem" />';
  end;
end;

procedure THCImageItem.ToXml(const ANode: IHCXMLNode);
var
  vNode: IHCXMLNode;
begin
  inherited ToXml(ANode);

  vNode := ANode.AddChild('img');
  if not FEmpty then
    vNode.Text := GraphicToBase64(FImage);

  vNode := ANode.AddChild('shapes');
  FShapeManager.ToXml(vNode);
end;

end.
