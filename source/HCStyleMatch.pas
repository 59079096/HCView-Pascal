{*******************************************************}
{                                                       }
{               HCView V1.0  作者：荆通                 }
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
  TOnTextStyle = procedure(const ACurStyleNo: Integer; var AWillStyle: THCTextStyle) of object;

  TStyleMatch = class  // 文本样式匹配类
  private
    FAppend: Boolean;  // True添加对应样式
    FOnTextStyle: TOnTextStyle;
  public
    function GetMatchStyleNo(const AStyle: THCStyle; const ACurStyleNo: Integer): Integer; virtual; abstract;
    function StyleHasMatch(const AStyle: THCStyle; const ACurStyleNo: Integer): Boolean; virtual;
    property OnTextStyle: TOnTextStyle read FOnTextStyle write FOnTextStyle;
    property Append: Boolean read FAppend write FAppend;
  end;

  TTextStyleMatch = class(TStyleMatch)
  private
    FFontStyle: TFontStyleEx;
  public
    function GetMatchStyleNo(const AStyle: THCStyle; const ACurStyleNo: Integer): Integer; override;
    function StyleHasMatch(const AStyle: THCStyle; const ACurStyleNo: Integer): Boolean; override;
    property FontStyle: TFontStyleEx read FFontStyle write FFontStyle;
  end;

  TFontNameStyleMatch = class(TStyleMatch)
  private
    FFontName: string;
  public
    function GetMatchStyleNo(const AStyle: THCStyle; const ACurStyleNo: Integer): Integer; override;
    property FontName: string read FFontName write FFontName;
  end;

  TFontSizeStyleMatch = class(TStyleMatch)
  private
    FFontSize: Single;
  public
    function GetMatchStyleNo(const AStyle: THCStyle; const ACurStyleNo: Integer): Integer; override;
    property FontSize: Single read FFontSize write FFontSize;
  end;

  TColorStyleMatch = class(TStyleMatch)
  private
    FColor: TColor;
  public
    function GetMatchStyleNo(const AStyle: THCStyle; const ACurStyleNo: Integer): Integer; override;
    property Color: TColor read FColor write FColor;
  end;

  TBackColorStyleMatch = class(TStyleMatch)
  private
    FColor: TColor;
  public
    function GetMatchStyleNo(const AStyle: THCStyle; const ACurStyleNo: Integer): Integer; override;
    property Color: TColor read FColor write FColor;
  end;

  TParaMatch = class  // 段样式匹配类
  private
    FJoin: Boolean;  // 添加对应样式
  public
    function GetMatchParaNo(const AStyle: THCStyle; const ACurParaNo: Integer): Integer; virtual; abstract;
    property Join: Boolean read FJoin write FJoin;
  end;

  TParaAlignHorzMatch = class(TParaMatch)
  private
    FAlign: TParaAlignHorz;
  public
    function GetMatchParaNo(const AStyle: THCStyle; const ACurParaNo: Integer): Integer; override;
    property Align: TParaAlignHorz read FAlign write FAlign;
  end;

  TParaAlignVertMatch = class(TParaMatch)
  private
    FAlign: TParaAlignVert;
  public
    function GetMatchParaNo(const AStyle: THCStyle; const ACurParaNo: Integer): Integer; override;
    property Align: TParaAlignVert read FAlign write FAlign;
  end;

  TParaLineSpaceMatch = class(TParaMatch)
  private
    FSpaceMode: TParaLineSpaceMode;
  public
    function GetMatchParaNo(const AStyle: THCStyle; const ACurParaNo: Integer): Integer; override;
    property SpaceMode: TParaLineSpaceMode read FSpaceMode write FSpaceMode;
  end;

  TParaBackColorMatch = class(TParaMatch)
  private
    FBackColor: TColor;
  public
    function GetMatchParaNo(const AStyle: THCStyle; const ACurParaNo: Integer): Integer; override;
    property BackColor: TColor read FBackColor write FBackColor;
  end;

implementation

uses
  HCCommon;

{ TFontNameStyleMatch }

function TFontNameStyleMatch.GetMatchStyleNo(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Integer;
var
  vTextStyle: THCTextStyle;
begin
  Result := THCStyle.Null;
  if AStyle.TextStyles[ACurStyleNo].Family = FFontName then
  begin
    Result := ACurStyleNo;
    Exit;
  end;

  vTextStyle := THCTextStyle.Create;
  try
    vTextStyle.AssignEx(AStyle.TextStyles[ACurStyleNo]);  // item当前的样式
    vTextStyle.Family := FFontName;
    if Assigned(FOnTextStyle) then
      FOnTextStyle(ACurStyleNo, vTextStyle);
    Result := AStyle.GetStyleNo(vTextStyle, True);  // 新样式编号
  finally
    vTextStyle.Free;
  end;
end;

{ TTextStyleMatch }

function TTextStyleMatch.GetMatchStyleNo(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Integer;
var
  vTextStyle: THCTextStyle;
begin
  Result := THCStyle.Null;
  vTextStyle := THCTextStyle.Create;
  try
    vTextStyle.AssignEx(AStyle.TextStyles[ACurStyleNo]);  // item当前的样式
    if FAppend then  // 添加
    begin
      if not (FFontStyle in vTextStyle.FontStyle) then
      begin
        // 不能同时为上标和下标
        if FFontStyle = TFontStyleEx.tsSuperscript then
          vTextStyle.FontStyle := vTextStyle.FontStyle - [TFontStyleEx.tsSubscript]
        else
        if FFontStyle = TFontStyleEx.tsSubscript then
          vTextStyle.FontStyle := vTextStyle.FontStyle - [TFontStyleEx.tsSuperscript];

        vTextStyle.FontStyle := vTextStyle.FontStyle + [FFontStyle];
      end
      else
        Exit(ACurStyleNo);
    end
    else  // 减去
    begin
      if FFontStyle in vTextStyle.FontStyle then
        vTextStyle.FontStyle := vTextStyle.FontStyle - [FFontStyle]
      else
        Exit(ACurStyleNo);
    end;
    if Assigned(FOnTextStyle) then
      FOnTextStyle(ACurStyleNo, vTextStyle);
    Result := AStyle.GetStyleNo(vTextStyle, True);  // 新样式编号
  finally
    vTextStyle.Free;
  end;
end;

function TTextStyleMatch.StyleHasMatch(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Boolean;
var
  vTextStyle: THCTextStyle;
begin
  Result := False;
  vTextStyle := THCTextStyle.Create;
  try
    vTextStyle.AssignEx(AStyle.TextStyles[ACurStyleNo]);  // item当前的样式
    Result := FFontStyle in vTextStyle.FontStyle;
  finally
    vTextStyle.Free;
  end;
end;

{ TParaAlignHorzMatch }

function TParaAlignHorzMatch.GetMatchParaNo(const AStyle: THCStyle;
  const ACurParaNo: Integer): Integer;
var
  vParaStyle: THCParaStyle;
begin
  Result := THCStyle.Null;
  if AStyle.ParaStyles[ACurParaNo].AlignHorz = FAlign then
  begin
    Result := ACurParaNo;
    Exit;
  end;

  vParaStyle := THCParaStyle.Create;
  try
    vParaStyle.AssignEx(AStyle.ParaStyles[ACurParaNo]);
    vParaStyle.AlignHorz := FAlign;
    Result := AStyle.GetParaNo(vParaStyle, True);  // 新段样式
  finally
    vParaStyle.Free;
  end;
end;

{ TColorStyleMatch }

function TColorStyleMatch.GetMatchStyleNo(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Integer;
var
  vTextStyle: THCTextStyle;
begin
  Result := THCStyle.Null;
  if AStyle.TextStyles[ACurStyleNo].Color = FColor then
  begin
    Result := ACurStyleNo;
    Exit;
  end;

  vTextStyle := THCTextStyle.Create;
  try
    vTextStyle.AssignEx(AStyle.TextStyles[ACurStyleNo]);  // item当前的样式
    vTextStyle.Color := FColor;
    if Assigned(FOnTextStyle) then
      FOnTextStyle(ACurStyleNo, vTextStyle);
    Result := AStyle.GetStyleNo(vTextStyle, True);  // 新样式编号
  finally
    vTextStyle.Free;
  end;
end;

{ TBackColorStyleMatch }

function TBackColorStyleMatch.GetMatchStyleNo(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Integer;
var
  vTextStyle: THCTextStyle;
begin
  Result := THCStyle.Null;
  if AStyle.TextStyles[ACurStyleNo].BackColor = FColor then
  begin
    Result := ACurStyleNo;
    Exit;
  end;

  vTextStyle := THCTextStyle.Create;
  try
    vTextStyle.AssignEx(AStyle.TextStyles[ACurStyleNo]);  // item当前的样式
    vTextStyle.BackColor := FColor;
    if Assigned(FOnTextStyle) then
      FOnTextStyle(ACurStyleNo, vTextStyle);
    Result := AStyle.GetStyleNo(vTextStyle, True);  // 新样式编号
  finally
    vTextStyle.Free;
  end;
end;

{ TFontSizeStyleMatch }

function TFontSizeStyleMatch.GetMatchStyleNo(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Integer;
var
  vTextStyle: THCTextStyle;
begin
  Result := THCStyle.Null;
  if AStyle.TextStyles[ACurStyleNo].Size = FFontSize then
  begin
    Result := ACurStyleNo;
    Exit;
  end;

  vTextStyle := THCTextStyle.Create;
  try
    vTextStyle.AssignEx(AStyle.TextStyles[ACurStyleNo]);  // item当前的样式
    vTextStyle.Size := FFontSize;
    if Assigned(FOnTextStyle) then
      FOnTextStyle(ACurStyleNo, vTextStyle);
    Result := AStyle.GetStyleNo(vTextStyle, True);  // 新样式编号
  finally
    vTextStyle.Free;
  end;
end;

{ TParaAlignVertMatch }

function TParaAlignVertMatch.GetMatchParaNo(const AStyle: THCStyle;
  const ACurParaNo: Integer): Integer;
var
  vParaStyle: THCParaStyle;
begin
  Result := THCStyle.Null;
  if AStyle.ParaStyles[ACurParaNo].AlignVert = FAlign then
  begin
    Result := ACurParaNo;
    Exit;
  end;

  vParaStyle := THCParaStyle.Create;
  try
    vParaStyle.AssignEx(AStyle.ParaStyles[ACurParaNo]);
    vParaStyle.AlignVert := FAlign;
    Result := AStyle.GetParaNo(vParaStyle, True);  // 新段样式
  finally
    vParaStyle.Free;
  end;
end;

{ TStyleMatch }

function TStyleMatch.StyleHasMatch(const AStyle: THCStyle;
  const ACurStyleNo: Integer): Boolean;
begin
  Result := False;
end;

{ TParaLineSpaceMatch }

function TParaLineSpaceMatch.GetMatchParaNo(const AStyle: THCStyle;
  const ACurParaNo: Integer): Integer;
var
  vParaStyle: THCParaStyle;
begin
  Result := THCStyle.Null;
  if AStyle.ParaStyles[ACurParaNo].LineSpaceMode = FSpaceMode then
  begin
    Result := ACurParaNo;
    Exit;
  end;

  vParaStyle := THCParaStyle.Create;
  try
    vParaStyle.AssignEx(AStyle.ParaStyles[ACurParaNo]);
    vParaStyle.LineSpaceMode := FSpaceMode;
    Result := AStyle.GetParaNo(vParaStyle, True);  // 新段样式
  finally
    vParaStyle.Free;
  end;
end;

{ TParaBackColorMatch }

function TParaBackColorMatch.GetMatchParaNo(const AStyle: THCStyle;
  const ACurParaNo: Integer): Integer;
var
  vParaStyle: THCParaStyle;
begin
  Result := THCStyle.Null;
  if AStyle.ParaStyles[ACurParaNo].BackColor = FBackColor then
  begin
    Result := ACurParaNo;
    Exit;
  end;

  vParaStyle := THCParaStyle.Create;
  try
    vParaStyle.AssignEx(AStyle.ParaStyles[ACurParaNo]);
    vParaStyle.BackColor := FBackColor;
    Result := AStyle.GetParaNo(vParaStyle, True);  // 新段样式
  finally
    vParaStyle.Free;
  end;
end;

end.
