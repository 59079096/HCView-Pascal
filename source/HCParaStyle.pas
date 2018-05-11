{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                  文本段样式实现单元                   }
{                                                       }
{*******************************************************}

unit HCParaStyle;

interface

uses
  Classes, Graphics;

type
  /// <summary> 段水平对齐方式：左、右、居中、两端、分散) </summary>
  TParaAlignHorz = (pahLeft, pahRight, pahCenter, pahJustify, pahScatter);

  /// <summary> 段垂直对齐方式：下、居中、上) </summary>
  TParaAlignVert = (pavBottom, pavCenter, pavTop);

  TParaStyle = class(TPersistent)
  strict private
    FLineSpace,  // 行间距
    FLineSpaceHalf,  // 行间距一半
    FFristIndent,// 首行缩进
    FLeftIndent  // 左缩进
      : Integer;
    FBackColor: TColor;
    FAlignHorz: TParaAlignHorz;
    FAlignVert: TParaAlignVert;
  protected
    procedure SetLineSpace(const Value: Integer);
  public
    CheckSaveUsed: Boolean;
    TempNo: Integer;
    constructor Create;
    destructor Destroy; override;
    function EqualsEx(const ASource: TParaStyle): Boolean;
    procedure AssignEx(const ASource: TParaStyle);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word);
  published
    property LineSpace: Integer read FLineSpace write SetLineSpace;
    property LineSpaceHalf: Integer read FLineSpaceHalf;
    property FristIndent: Integer read FFristIndent write FFristIndent;
    property LeftIndent: Integer read FLeftIndent write FLeftIndent;
    property BackColor: TColor read FBackColor write FBackColor;
    property AlignHorz: TParaAlignHorz read FAlignHorz write FAlignHorz;
    property AlignVert: TParaAlignVert read FAlignVert write FAlignVert;
  end;

implementation

{ TParaStyle }

procedure TParaStyle.AssignEx(const ASource: TParaStyle);
begin
  Self.FLineSpace := ASource.LineSpace;
  Self.FLineSpaceHalf := ASource.FLineSpaceHalf;
  Self.FFristIndent := ASource.FristIndent;
  Self.LeftIndent := ASource.LeftIndent;
  Self.FBackColor := ASource.BackColor;
  Self.FAlignHorz := ASource.AlignHorz;
end;

constructor TParaStyle.Create;
begin
  FLineSpace := 8;  // 五号字的高为15，
  FLineSpaceHalf := 4;
  FFristIndent := 0;
  FLeftIndent := 0;
  FBackColor := clSilver;
  FAlignHorz := TParaAlignHorz.pahJustify;
  FAlignVert := TParaAlignVert.pavCenter;
end;

destructor TParaStyle.Destroy;
begin

  inherited;
end;

function TParaStyle.EqualsEx(const ASource: TParaStyle): Boolean;
begin
  Result :=
  (Self.FLineSpace = ASource.LineSpace)
  and (Self.FFristIndent = ASource.FristIndent)
  and (Self.LeftIndent = ASource.LeftIndent)
  and (Self.FBackColor = ASource.BackColor)
  and (Self.FAlignHorz = ASource.AlignHorz)
  and (Self.FAlignVert = ASource.AlignVert)
  and (Self.FLineSpace = ASource.LineSpace);
end;

procedure TParaStyle.LoadFromStream(const AStream: TStream; const AFileVersion: Word);
begin
  AStream.ReadBuffer(FLineSpace, SizeOf(FLineSpace));
  FLineSpaceHalf := FLineSpace div 2;
  AStream.ReadBuffer(FFristIndent, SizeOf(FFristIndent));  // 首行缩进
  AStream.ReadBuffer(FLeftIndent, SizeOf(FLeftIndent));  // 左缩进
  AStream.ReadBuffer(FBackColor, SizeOf(FBackColor));
  AStream.ReadBuffer(FAlignHorz, SizeOf(FAlignHorz));
end;

procedure TParaStyle.SaveToStream(const AStream: TStream);
begin
  AStream.WriteBuffer(FLineSpace, SizeOf(FLineSpace));
  AStream.WriteBuffer(FFristIndent, SizeOf(FFristIndent));  // 首行缩进
  AStream.WriteBuffer(FLeftIndent, SizeOf(FLeftIndent));  // 左缩进
  AStream.WriteBuffer(FBackColor, SizeOf(FBackColor));
  AStream.WriteBuffer(FAlignHorz, SizeOf(FAlignHorz));
end;

procedure TParaStyle.SetLineSpace(const Value: Integer);
begin
  if FLineSpace <> Value then
  begin
    FLineSpace := Value;
    FLineSpaceHalf := Value div 2;
  end;
end;

end.
