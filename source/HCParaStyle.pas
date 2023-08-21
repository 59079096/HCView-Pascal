{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2018-5-4              }
{                                                       }
{                  �ı�����ʽʵ�ֵ�Ԫ                   }
{                                                       }
{*******************************************************}

unit HCParaStyle;

interface

uses
  Classes, Graphics, HCXml;

type
  /// <summary> ��ˮƽ���뷽ʽ���󡢾��С��ҡ����ˡ���ɢ) </summary>
  TParaAlignHorz = (pahLeft, pahCenter, pahRight, pahJustify, pahScatter);

  /// <summary> �δ�ֱ���뷽ʽ���ϡ����С���) </summary>
  TParaAlignVert = (pavTop, pavCenter, pavBottom);

  /// <summary> ����������ʽ </summary>
  TParaFirstLineIndent = (pfiNone, pfiIndented, pfiHanging);

  TParaLineSpaceMode = (pls100, pls115, pls150, pls200, plsMin, plsFix, plsMult);

  THCParaStyle = class(TPersistent)
  strict private
    FLineSpaceMode: TParaLineSpaceMode;
    FLineSpace: Single;

    FFirstIndentPix, // ��������
    FLeftIndentPix,  // ������
    FRightIndentPix  // ������
      : Integer;     // ��λ����

    FFirstIndent,    // ��������
    FLeftIndent,     // ������
    FRightIndent     // ������
      : Single;     // ��λ����

    // �нض�ʱʹ�ôֱ���ʽ
    // False:���ܽضϴ�ǰ����ͬ���͵��ַ�һ��ضϣ��硰���abc������c�ض�ʱ������ab
    // True: Graceful���Žضϣ��жϽضϴ�ǰ��������ͬ���ַ�һ��ضϣ���c�ض�ʱ��abһ��ض�
    FBreakRough: Boolean;

    FBackColor: TColor;
    FAlignHorz: TParaAlignHorz;
    FAlignVert: TParaAlignVert;
  protected
    procedure SetFirstIndent(const Value: Single);
    procedure SetLeftIndent(const Value: Single);
    procedure SetRightIndent(const Value: Single);
  public
    CheckSaveUsed: Boolean;
    TempNo: Integer;
    constructor Create;
    function EqualsEx(const ASource: THCParaStyle): Boolean;
    procedure AssignEx(const ASource: THCParaStyle);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
    function ToCSS: string;
    procedure ToXml(const ANode: IHCXmlNode);
    procedure ParseXml(const ANode: IHCXmlNode);
  published
    property LineSpaceMode: TParaLineSpaceMode read FLineSpaceMode write FLineSpaceMode;
    property LineSpace: Single read FLineSpace write FLineSpace;
    /// <summary> ��������(����) </summary>
    property FirstIndent: Single read FFirstIndent write SetFirstIndent;
    /// <summary> ������(����) </summary>
    property LeftIndent: Single read FLeftIndent write SetLeftIndent;
    /// <summary> ������(����) </summary>
    property RightIndent: Single read FRightIndent write SetRightIndent;
    /// <summary> ��������(����) </summary>
    property FirstIndentPix: Integer read FFirstIndentPix;
    /// <summary> ������(����) </summary>
    property LeftIndentPix: Integer read FLeftIndentPix;
    /// <summary> ������(����) </summary>
    property RightIndentPix: Integer read FRightIndentPix;
    property BackColor: TColor read FBackColor write FBackColor;
    property BreakRough: Boolean read FBreakRough write FBreakRough;
    property AlignHorz: TParaAlignHorz read FAlignHorz write FAlignHorz;
    property AlignVert: TParaAlignVert read FAlignVert write FAlignVert;
  end;

implementation

uses
  SysUtils, HCCommon, HCUnitConversion;

{ THCParaStyle }

procedure THCParaStyle.AssignEx(const ASource: THCParaStyle);
begin
  FLineSpaceMode := ASource.LineSpaceMode;
  FLineSpace := ASource.LineSpace;
  FirstIndent := ASource.FirstIndent;
  LeftIndent := ASource.LeftIndent;
  RightIndent := ASource.RightIndent;
  FBackColor := ASource.BackColor;
  FBreakRough := ASource.BreakRough;
  FAlignHorz := ASource.AlignHorz;
  FAlignVert := ASource.AlignVert;
end;

constructor THCParaStyle.Create;
begin
  FirstIndent := 0;
  LeftIndent := 0;
  RightIndent := 0;

  FLineSpaceMode := TParaLineSpaceMode.pls150;
  FLineSpace := 1;
  FBreakRough := False;
  FBackColor := HCTransparentColor;
  FAlignHorz := TParaAlignHorz.pahJustify;
  FAlignVert := TParaAlignVert.pavCenter;
end;

function THCParaStyle.EqualsEx(const ASource: THCParaStyle): Boolean;
begin
  Result :=
  //(Self.FLineSpace = ASource.LineSpace)
  (FLineSpaceMode = ASource.LineSpaceMode)
  and (FLineSpace = ASource.LineSpace)
  and (FFirstIndent = ASource.FirstIndent)
  and (FLeftIndent = ASource.LeftIndent)
  and (FRightIndent = ASource.RightIndent)
  and (FBackColor = ASource.BackColor)
  and (FBreakRough = ASource.BreakRough)
  and (FAlignHorz = ASource.AlignHorz)
  and (FAlignVert = ASource.AlignVert);
end;

procedure THCParaStyle.LoadFromStream(const AStream: TStream; const AFileVersion: Word);
var
  vLineSpace: Integer;
  vByte: Byte;
begin
  if AFileVersion < 15 then
    AStream.ReadBuffer(vLineSpace, SizeOf(vLineSpace));

  if AFileVersion > 16 then
    AStream.ReadBuffer(FLineSpaceMode, SizeOf(FLineSpaceMode));

  if AFileVersion > 30 then
    AStream.ReadBuffer(FLineSpace, SizeOf(FLineSpace))
  else
  if FLineSpaceMode = TParaLineSpaceMode.plsFix  then  // �ɰ汾ͳһ��12pt����
    FLineSpace := 12;

  if AFileVersion < 22 then
  begin
    AStream.ReadBuffer(FFirstIndentPix, SizeOf(FFirstIndentPix));  // ��������
    AStream.ReadBuffer(FLeftIndentPix, SizeOf(FLeftIndentPix));    // ������
  end
  else
  begin
    AStream.ReadBuffer(FFirstIndent, SizeOf(FFirstIndent));  // ��������
    FFirstIndentPix := MillimeterToPixX(FFirstIndent);
    AStream.ReadBuffer(FLeftIndent, SizeOf(FLeftIndent));    // ������
    FLeftIndentPix := MillimeterToPixX(FLeftIndent);
    AStream.ReadBuffer(FRightIndent, SizeOf(FRightIndent));  // ������
    FRightIndentPix := MillimeterToPixX(FRightIndent);
  end;

  if AFileVersion > 18 then
    HCLoadColorFromStream(AStream, FBackColor)
  else
    AStream.ReadBuffer(FBackColor, SizeOf(FBackColor));

  if AFileVersion > 31 then
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    FBreakRough := Odd(vByte shr 7);
  end;

  AStream.ReadBuffer(FAlignHorz, SizeOf(FAlignHorz));

  if AFileVersion > 17 then
    AStream.ReadBuffer(FAlignVert, SizeOf(FAlignVert));
end;

procedure THCParaStyle.ParseXml(const ANode: IHCXmlNode);

  procedure GetXMLLineSpaceMode_;
  var
    vs: string;
  begin
    vs := ANode.Attributes['spacemode'];
    if vs = '100' then
      FLineSpaceMode := pls100
    else
    if vs = '115' then
      FLineSpaceMode := pls115
    else
    if vs = '150' then
      FLineSpaceMode := pls150
    else
    if vs = '200' then
      FLineSpaceMode := pls200
    else
    if vs = 'min' then
      FLineSpaceMode := plsMin
    else
    if vs = 'mult' then
      FLineSpaceMode := plsMult
    else
    if vs = 'fix' then
      FLineSpaceMode := plsFix;
  end;

  procedure GetXMLHorz_;
  begin
    if ANode.Attributes['horz'] = 'left' then
      FAlignHorz := pahLeft
    else
    if ANode.Attributes['horz'] = 'right' then
      FAlignHorz := pahRight
    else
    if ANode.Attributes['horz'] = 'center' then
      FAlignHorz := pahCenter
    else
    if ANode.Attributes['horz'] = 'justify' then
      FAlignHorz := pahJustify
    else
    if ANode.Attributes['horz'] = 'scatter' then
      FAlignHorz := pahScatter;
  end;

  procedure GetXMLVert_;
  begin
    if ANode.Attributes['vert'] = 'top' then
      FAlignVert := pavTop
    else
    if ANode.Attributes['vert'] = 'center' then
      FAlignVert := pavCenter
    else
    if ANode.Attributes['vert'] = 'bottom' then
      FAlignVert := pavBottom;
  end;

begin
  FirstIndent := ANode.Attributes['firstindent'];
  LeftIndent := ANode.Attributes['leftindent'];
  RightIndent := ANode.Attributes['rightindent'];
  FBackColor := HCRGBStringToColor(ANode.Attributes['bkcolor']);
  FBreakRough := ANode.Attributes['breakrough'];
  GetXMLLineSpaceMode_;
  GetXMLHorz_;
  GetXMLVert_;
end;

procedure THCParaStyle.SaveToStream(const AStream: TStream);
var
  vByte: Byte;
begin
  AStream.WriteBuffer(FLineSpaceMode, SizeOf(FLineSpaceMode));
  AStream.WriteBuffer(FLineSpace, SizeOf(FLineSpace));
  AStream.WriteBuffer(FFirstIndent, SizeOf(FFirstIndent));  // ��������
  AStream.WriteBuffer(FLeftIndent, SizeOf(FLeftIndent));  // ������
  AStream.WriteBuffer(FRightIndent, SizeOf(FRightIndent));  // ������
  HCSaveColorToStream(AStream, FBackColor);

  vByte := 0;
  if FBreakRough then
    vByte := vByte or (1 shl 7);
  AStream.WriteBuffer(vByte, SizeOf(vByte));

  AStream.WriteBuffer(FAlignHorz, SizeOf(FAlignHorz));
  AStream.WriteBuffer(FAlignVert, SizeOf(FAlignVert));
end;

procedure THCParaStyle.SetFirstIndent(const Value: Single);
begin
  if FFirstIndent <> Value then
  begin
    FFirstIndent := Value;
    FFirstIndentPix := MillimeterToPixX(FFirstIndent);
  end;
end;

procedure THCParaStyle.SetLeftIndent(const Value: Single);
begin
  if FLeftIndent <> Value then
  begin
    FLeftIndent := Value;
    FLeftIndentPix := MillimeterToPixX(FLeftIndent);
  end;
end;

procedure THCParaStyle.SetRightIndent(const Value: Single);
begin
  if FRightIndent <> Value then
  begin
    FRightIndent := Value;
    FRightIndentPix := MillimeterToPixX(FRightIndent);
  end;
end;

function THCParaStyle.ToCSS: string;
begin
  Result := ' text-align: ';
  case FAlignHorz of
    pahLeft: Result := Result + 'left';
    pahRight: Result := Result + 'right';
    pahCenter: Result := Result + 'center';
    pahJustify, pahScatter: Result := Result + 'justify';
  end;

  case FLineSpaceMode of
    pls100: Result := Result + '; line-height: 100%';
    pls115: Result := Result + '; line-height: 115%';
    pls150: Result := Result + '; line-height: 150%';
    pls200: Result := Result + '; line-height: 200%';
    plsFix: Result := Result + '; line-height: 10px';
  end;
end;

procedure THCParaStyle.ToXml(const ANode: IHCXmlNode);

  function GetLineSpaceModeXML_: string;
  begin
    case FLineSpaceMode of
      pls100: Result := '100';
      pls115: Result := '115';
      pls150: Result := '150';
      pls200: Result := '200';
      plsMin: Result := 'min';
      plsFix: Result := 'fix';
      plsMult: Result := 'mult';
    else
      Result := '100';
    end;
  end;

  function GetHorzXML_: string;
  begin
    case FAlignHorz of
      pahLeft: Result := 'left';
      pahRight: Result := 'right';
      pahCenter: Result := 'center';
      pahJustify: Result := 'justify';
      pahScatter: Result := 'scatter';
    end;
  end;

  function GetVertXML_: string;
  begin
    case FAlignVert of
      pavTop: Result := 'top';
      pavCenter: Result := 'center';
      pavBottom: Result := 'bottom';
    end;
  end;

begin
  ANode.Attributes['firstindent'] := FFirstIndent;
  ANode.Attributes['leftindent'] := FLeftIndent;
  ANode.Attributes['rightindent'] := FRightIndent;
  ANode.Attributes['bkcolor'] := HCColorToRGBString(FBackColor);
  ANode.Attributes['breakrough'] := FBreakRough;
  ANode.Attributes['spacemode'] := GetLineSpaceModeXML_;
  ANode.Attributes['horz'] := GetHorzXML_;
  ANode.Attributes['vert'] := GetVertXML_;
end;

end.
