{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{          �ĵ�ImageItem(ͼ��)����ʵ�ֵ�Ԫ              }
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

    /// <summary> Լ����ָ����С��Χ�� </summary>
    procedure RestrainSize(const AWidth, AHeight: Integer); override;
    procedure LoadGraphicFile(const AFileName: string; const AResize: Boolean = True);
    procedure LoadGraphicStream(const AStream: TStream; const AResize: Boolean = True);
    procedure ImageAssign(const AGraphic: TGraphic);

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure LoadFromBase64(const ABase64: string);
    function ToHtml(const APath: string): string; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    /// <summary> �ָ���ԭʼ�ߴ� </summary>
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
  FEmpty := (Source as THCImageItem).Empty;
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

  {$REGION ' TurnUpDownͼ�����·�ת(�ѷ���) '}
  // pf32λʱͼ�������ǰ��д�ţ�ÿ�а�˫�ֶ��룬�а�����ʽ���
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
      //WICBitmap2Bitmap(FImage.Handle, vBitmap);  // ����png�Ĵ�ӡ����͸���ı�ɺ�ɫ��
      //vBitmap.AlphaFormat := TAlphaFormat.afIgnored;
      vBitmap.Canvas.Draw(0, 0, FImage);
      // ҳü�ʼ������ͼƬʱ����ӡ��ҳ���ֵ�һҳ��ʧͼƬ�����⣬��������3�п��Խ������ʱû�ҵ�����ԭ��
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
  else  // �Ǵ�ӡ
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
  FImage.LoadFromFile(AFileName);  // BMPIMAGEITEM�ᴥ��OnChange WICImage���ᴥ��
  if AResize then
  begin
    Self.Width := FImage.Width;
    Self.Height := FImage.Height;
  end;

  FEmpty := False;
end;

procedure THCImageItem.LoadGraphicStream(const AStream: TStream; const AResize: Boolean = True);
begin
  FImage.LoadFromStream(AStream);  // BMPIMAGEITEM�ᴥ��OnChange WICImage���ᴥ��
  if AResize then
  begin
    Self.Width := FImage.Width;
    Self.Height := FImage.Height;
  end;

  FEmpty := False;
end;

procedure THCImageItem.LoadFromBase64(const ABase64: string);
begin
  Base64ToGraphic(ABase64, FImage);
  {$IFNDEF BMPIMAGEITEM}
  DoImageChange(Self);
  {$ENDIF}
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
    FImage.LoadFromStream(AStream);  // �ᴥ��OnChange
    FEmpty := False;
  end
  else  // ����C#�汾��ͼƬ�ļ�
  begin
    AStream.ReadBuffer(vImgSize, SizeOf(vImgSize));
    if vImgSize > 0 then
    begin
      {$IFDEF BMPIMAGEITEM}
      FImage.LoadFromStream(AStream);  // �ᴥ��OnChange
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
      //WICBitmap2Bitmap(FImage.Handle, vBmp);  // ͸������ʾ������ɫ��
      vBmp.SetSize(FImage.Width, FImage.Height);
      vBmp.Canvas.Draw(0, 0, FImage);
    {$ENDIF}

      vBlendFunction.BlendOp := AC_SRC_OVER;
      vBlendFunction.BlendFlags := 0;
      vBlendFunction.AlphaFormat := AC_SRC_OVER;  // ͨ��Ϊ 0�����ԴλͼΪ32λ���ɫ����Ϊ AC_SRC_ALPHA
      vBlendFunction.SourceConstantAlpha := 128; // ͸����
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
  if Assigned(vNode) then  // ����27֮ǰ���ļ�
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

  vStream := TMemoryStream.Create;  // ������������
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
  if APath <> '' then  // ���浽ָ�����ļ�����
  begin
    if not DirectoryExists(APath + 'images') then
      CreateDir(APath + 'images');
    vFile := OwnerData.Style.GetHtmlFileTempName + '.bmp';
    FImage.SaveToFile(APath + 'images\' + vFile);
    Result := '<img width="' + IntToStr(Width) + '" height="' + IntToStr(Height)
      + '" src="images/' + vFile + '" alt="THCImageItem" />';
  end
  else  // ����ΪBase64
  begin
    Result := '<img width="' + IntToStr(Width) + '" height="' + IntToStr(Height)
      + '" src="data:image/jpg;base64,' + GraphicToBase64(FImage) + '" alt="THCImageItem" />';
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
