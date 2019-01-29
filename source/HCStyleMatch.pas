{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{           文本类的HCItem样式匹配处理单元              }
{                                                       }
{*******************************************************}

unit HCStyleMatch;

interface

uses
  Graphics, HCStyle, HCTextStyle, HCParaStyle;

type
  TOnTextStyle = procedure(const ACurStyleNo: Integer; const AWillStyle: THCTextStyle) of object;

  THCStyleMatch = class(TObject)  // 文本样式匹配类
  private
    FAppend: Boolean;  // 添加还是去掉对应的样式 True添加
    FLock: Boolean;  // 控制由第一个要应用样式的Item决定添加还是去掉样式，比如表格选中多个单元格应用样式时以第一个选中的第一个Item决定
    FOnTextStyle: TOnTextStyle;
  protected
    procedure SetAppend(const Value: Boolean);
    function DoMatchCur(const ATextStyle: THCTextStyle): Boolean; virtual; abstract;
    procedure DoMatchNew(const ATextStyle: THCTextStyle); virtual; abstract;
  public
    constructor Create;
    function GetMatchStyleNo(const AStyle: THCStyle; const ACurStyleNo: Integer): Integer;
    function StyleHasMatch(const AStyle: THCStyle; const ACurStyleNo: Integer): Boolean; virtual;
    property Append: Boolean read FAppend write SetAppend;
    property OnTextStyle: TOnTextStyle read FOnTextStyle write FOnTextStyle;
  end;

  TTextStyleMatch = class(THCStyleMatch)  // 字体样式匹配类
  private
    FFontStyle: THCFontStyle;
  protected
    function DoMatchCur(const ATextStyle: THCTextStyle): Boolean; override;
    procedure DoMatchNew(const ATextStyle: THCTextStyle); override;
  public
    function StyleHasMatch(const AStyle: THCStyle; const ACurStyleNo: Integer): Boolean; override;
    property FontStyle: THCFontStyle read FFontStyle write FFontStyle;
  end;

  TFontNameStyleMatch = class(THCStyleMatch)  // 字体名称匹配类
  private
    FFontName: string;
  protected
    function DoMatchCur(const ATextStyle: THCTextStyle): Boolean; override;
    procedure DoMatchNew(const ATextStyle: THCTextStyle); override;
  public
    property FontName: string read FFontName write FFontName;
  end;

  TFontSizeStyleMatch = class(THCStyleMatch)  // 字体大小匹配类
  private
    FFontSize: Single;
  protected
    function DoMatchCur(const ATextStyle: THCTextStyle): Boolean; override;
    procedure DoMatchNew(const ATextStyle: THCTextStyle); override;
  public
    property FontSize: Single read FFontSize write FFontSize;
  end;

  TColorStyleMatch = class(THCStyleMatch)  // 字体颜色匹配类
  private
    FColor: TColor;
  protected
    function DoMatchCur(const ATextStyle: THCTextStyle): Boolean; override;
    procedure DoMatchNew(const ATextStyle: THCTextStyle); override;
  public
    property Color: TColor read FColor write FColor;
  end;

  TBackColorStyleMatch = class(THCStyleMatch)  // 字体背景色匹配类
  private
    FColor: TColor;
  protected
    function DoMatchCur(const ATextStyle: THCTextStyle): Boolean; override;
    procedure DoMatchNew(const ATextStyle: THCTextStyle); override;
  public
    property Color: TColor read FColor write FColor;
  end;

  THCParaMatch = class(TObject)  // 段样式匹配类
  protected
    function DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean; virtual; abstract;
    procedure DoMatchNewPara(const AParaStyle: THCParaStyle); virtual; abstract;
  public
    function GetMatchParaNo(const AStyle: THCStyle; const ACurParaNo: Integer): Integer;
  end;

  TParaAlignHorzMatch = class(THCParaMatch)  // 段水平对齐匹配类
  private
    FAlign: TParaAlignHorz;
  protected
    function DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean; override;
    procedure DoMatchNewPara(const AParaStyle: THCParaStyle); override;
  public
    property Align: TParaAlignHorz read FAlign write FAlign;
  end;

  TParaAlignVertMatch = class(THCParaMatch)  // 段垂直对齐匹配类
  private
    FAlign: TParaAlignVert;
  protected
    function DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean; override;
    procedure DoMatchNewPara(const AParaStyle: THCParaStyle); override;
  public
    property Align: TParaAlignVert read FAlign write FAlign;
  end;

  TParaLineSpaceMatch = class(THCParaMatch)  // 段行间距匹配类
  private
    FSpaceMode: TParaLineSpaceMode;
  protected
    function DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean; override;
    procedure DoMatchNewPara(const AParaStyle: THCParaStyle); override;
  public
    property SpaceMode: TParaLineSpaceMode read FSpaceMode write FSpaceMode;
  end;

  TParaBackColorMatch = class(THCParaMatch)  // 段背景色匹配类
  private
    FBackColor: TColor;
  protected
    function DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean; override;
    procedure DoMatchNewPara(const AParaStyle: THCParaStyle); override;
  public
    property BackColor: TColor read FBackColor write FBackColor;
  end;

  TParaFirstIndentMatch = class(THCParaMatch)  // 段首行缩进匹配类
  private
    FIndent: Single;
  protected
    function DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean; override;
    procedure DoMatchNewPara(const AParaStyle: THCParaStyle); override;
  public
    property Indent: Single read FIndent write FIndent;
  end;

  TParaLeftIndentMatch = class(THCParaMatch)  // 段左缩进匹配类
  private
    FIndent: Single;
  protected
    function DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean; override;
    procedure DoMatchNewPara(const AParaStyle: THCParaStyle); override;
  public
    property Indent: Single read FIndent write FIndent;
  end;

  TParaRightIndentMatch = class(THCParaMatch)  // 段右缩进匹配类
  private
    FIndent: Single;
  protected
    function DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean; override;
    procedure DoMatchNewPara(const AParaStyle: THCParaStyle); override;
  public
    property Indent: Single read FIndent write FIndent;
  end;

implementation

uses
  HCCommon, HCUnitConversion;

{ TTextStyleMatch }

function TTextStyleMatch.DoMatchCur(const ATextStyle: THCTextStyle): Boolean;
begin
  Result := Append and (FFontStyle in ATextStyle.FontStyles);  // 添加且有，不添加且无时True
end;

procedure TTextStyleMatch.DoMatchNew(const ATextStyle: THCTextStyle);
begin
  if Append then  // 添加
  begin
    // 不能同时为上标和下标
    if FFontStyle = THCFontStyle.tsSuperscript then
      ATextStyle.FontStyles := ATextStyle.FontStyles - [THCFontStyle.tsSubscript]
    else
    if FFontStyle = THCFontStyle.tsSubscript then
      ATextStyle.FontStyles := ATextStyle.FontStyles - [THCFontStyle.tsSuperscript];

    ATextStyle.FontStyles := ATextStyle.FontStyles + [FFontStyle];
  end
  else  // 减去
    ATextStyle.FontStyles := ATextStyle.FontStyles - [FFontStyle]
end;

function TTextStyleMatch.StyleHasMatch(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Boolean;
begin
  Result := FFontStyle in AStyle.TextStyles[ACurStyleNo].FontStyles;
end;

{ THCStyleMatch }

constructor THCStyleMatch.Create;
begin
  FLock := False;
end;

function THCStyleMatch.GetMatchStyleNo(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Integer;
var
  vTextStyle: THCTextStyle;
begin
  Result := THCStyle.Null;
  if DoMatchCur(AStyle.TextStyles[ACurStyleNo]) then
  begin
    Result := ACurStyleNo;
    Exit;
  end;

  vTextStyle := THCTextStyle.Create;
  try
    vTextStyle.AssignEx(AStyle.TextStyles[ACurStyleNo]);  // item当前的样式
    DoMatchNew(vTextStyle);
    if Assigned(FOnTextStyle) then
      FOnTextStyle(ACurStyleNo, vTextStyle);
    Result := AStyle.GetStyleNo(vTextStyle, True);  // 新样式编号
  finally
    vTextStyle.Free;
  end;
end;

procedure THCStyleMatch.SetAppend(const Value: Boolean);
begin
  if (FAppend <> Value) and (not FLock) then
  begin
    FAppend := Value;
    FLock := True;
  end;
end;

function THCStyleMatch.StyleHasMatch(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Boolean;
begin
  Result := False;
end;

{ THCParaMatch }

function THCParaMatch.GetMatchParaNo(const AStyle: THCStyle;
  const ACurParaNo: Integer): Integer;
var
  vParaStyle: THCParaStyle;
begin
  Result := THCStyle.Null;
  if DoMatchCurPara(AStyle.ParaStyles[ACurParaNo]) then
  begin
    Result := ACurParaNo;
    Exit;
  end;

  vParaStyle := THCParaStyle.Create;
  try
    vParaStyle.AssignEx(AStyle.ParaStyles[ACurParaNo]);
    DoMatchNewPara(vParaStyle);
    Result := AStyle.GetParaNo(vParaStyle, True);  // 新段样式
  finally
    vParaStyle.Free;
  end;
end;

{ TParaAlignHorzMatch }

function TParaAlignHorzMatch.DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean;
begin
  Result := AParaStyle.AlignHorz = FAlign;
end;

procedure TParaAlignHorzMatch.DoMatchNewPara(const AParaStyle: THCParaStyle);
begin
  AParaStyle.AlignHorz := FAlign;
end;

{ TParaAlignVertMatch }

function TParaAlignVertMatch.DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean;
begin
  Result := AParaStyle.AlignVert = FAlign;
end;

procedure TParaAlignVertMatch.DoMatchNewPara(const AParaStyle: THCParaStyle);
begin
  AParaStyle.AlignVert := FAlign;
end;

{ TParaLineSpaceMatch }

function TParaLineSpaceMatch.DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean;
begin
  Result := AParaStyle.LineSpaceMode = FSpaceMode;
end;

procedure TParaLineSpaceMatch.DoMatchNewPara(const AParaStyle: THCParaStyle);
begin
  AParaStyle.LineSpaceMode := FSpaceMode;
end;

{ TParaBackColorMatch }

function TParaBackColorMatch.DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean;
begin
  Result := AParaStyle.BackColor = FBackColor;
end;

procedure TParaBackColorMatch.DoMatchNewPara(const AParaStyle: THCParaStyle);
begin
  AParaStyle.BackColor := FBackColor;
end;

{ TParaLeftIndentMatch }

function TParaLeftIndentMatch.DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean;
begin
  Result := AParaStyle.LeftIndent = FIndent;
end;

procedure TParaLeftIndentMatch.DoMatchNewPara(const AParaStyle: THCParaStyle);
begin
  AParaStyle.LeftIndent := FIndent;
end;

{ TFontNameStyleMatch }

function TFontNameStyleMatch.DoMatchCur(const ATextStyle: THCTextStyle): Boolean;
begin
  Result := ATextStyle.Family = FFontName;
end;

procedure TFontNameStyleMatch.DoMatchNew(const ATextStyle: THCTextStyle);
begin
  ATextStyle.Family := FFontName;
end;

{ TFontSizeStyleMatch }

function TFontSizeStyleMatch.DoMatchCur(const ATextStyle: THCTextStyle): Boolean;
begin
  Result := ATextStyle.Size = FFontSize;
end;

procedure TFontSizeStyleMatch.DoMatchNew(const ATextStyle: THCTextStyle);
begin
  ATextStyle.Size := FFontSize;
end;

{ TColorStyleMatch }

function TColorStyleMatch.DoMatchCur(const ATextStyle: THCTextStyle): Boolean;
begin
  Result := ATextStyle.Color = FColor;
end;

procedure TColorStyleMatch.DoMatchNew(const ATextStyle: THCTextStyle);
begin
  ATextStyle.Color := FColor;
end;

{ TBackColorStyleMatch }

function TBackColorStyleMatch.DoMatchCur(const ATextStyle: THCTextStyle): Boolean;
begin
  Result := ATextStyle.BackColor = FColor;
end;

procedure TBackColorStyleMatch.DoMatchNew(const ATextStyle: THCTextStyle);
begin
  ATextStyle.BackColor := FColor;
end;

{ TParaFirstIndentMatch }

function TParaFirstIndentMatch.DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean;
begin
  Result := AParaStyle.FirstIndent = FIndent;
end;

procedure TParaFirstIndentMatch.DoMatchNewPara(const AParaStyle: THCParaStyle);
begin
  AParaStyle.FirstIndent := FIndent
end;

{ TParaRightIndentMatch }

function TParaRightIndentMatch.DoMatchCurPara(const AParaStyle: THCParaStyle): Boolean;
begin
  Result := AParaStyle.RightIndent = FIndent;
end;

procedure TParaRightIndentMatch.DoMatchNewPara(const AParaStyle: THCParaStyle);
begin
  AParaStyle.RightIndent := FIndent;
end;

end.
