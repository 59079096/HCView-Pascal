{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-9-12             }
{                                                       }
{      文档CDateTimePicker(日期时间)对象实现单元        }
{                                                       }
{*******************************************************}

unit HCDateTimePicker;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, HCItem, HCRectItem, HCXml,
  HCStyle, HCCustomData, HCEditItem, HCCommon;

type
  TDateTimeArea = (dtaNone, dtaYear, dtaMonth, dtaDay, dtaHour, dtaMinute,
    dtaSecond, dtaMillisecond);

  THCDateTimePicker = class(THCEditItem)
  private
    FDateTime: TDateTime;
    FFormat: string;
    FAreaRect: TRect;
    FActiveArea: TDateTimeArea;
    FNewYear: string; // 当前输入的年新值
    FJoinKey: Boolean;  // 键盘修改除年外时，记录是否连接输入

    function GetAreaRect(const AArea: TDateTimeArea): TRect;
    function GetAreaAt(const X, Y: Integer): TDateTimeArea;

    procedure SetDateTime(const Value: TDateTime);
    procedure SetInputYear;
    procedure SetFormat(const Value: string);
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SetActive(const Value: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary> 正在其上时内部是否处理指定的Key和Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function InsertText(const AText: string): Boolean; override;
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const ADateTime: TDateTime); virtual;
    //destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property Format: string read FFormat write SetFormat;
    property DateTime: TDateTime read FDateTime write SetDateTime;
  end;

implementation

{$I HCView.inc}

uses
  DateUtils;

{ THCDateTimePicker }

procedure THCDateTimePicker.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FFormat := (Source as THCDateTimePicker).Format;
  FDateTime := (Source as THCDateTimePicker).DateTime;
end;

constructor THCDateTimePicker.Create(const AOwnerData: THCCustomData; const ADateTime: TDateTime);
begin
  FFormat := 'YYYY-MM-DD HH:mm:SS';
  FDateTime := ADateTime;
  inherited Create(AOwnerData, FormatDateTime(FFormat, FDateTime));
  Self.StyleNo := THCStyle.DateTimePicker;
  Width := 80;
  Self.FMargin := 2;
  FActiveArea := dtaNone;
end;

procedure THCDateTimePicker.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vAreaRect: TRect;
begin
  vAreaRect := FAreaRect;
  vAreaRect.Offset(ADrawRect.TopLeft);

  if (FActiveArea <> dtaNone) and (not Self.IsSelectComplate) and (not APaintInfo.Print) then
  begin
    ACanvas.Brush.Color := AStyle.SelColor;
    ACanvas.FillRect(vAreaRect);
  end;

  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if (FActiveArea = dtaYear) and (FNewYear <> '') and (not APaintInfo.Print) then
  begin
    ACanvas.Brush.Color := AStyle.SelColor;
    ACanvas.FillRect(vAreaRect);
    Windows.DrawText(ACanvas.Handle, FNewYear, -1, vAreaRect, DT_RIGHT or DT_SINGLELINE);
  end;
end;

function THCDateTimePicker.GetAreaAt(const X, Y: Integer): TDateTimeArea;
var
  vPt: TPoint;
begin
  vPt := Point(X, Y);

  if PtInRect(GetAreaRect(dtaYear), vPt) then
    Result := dtaYear
  else
  if PtInRect(GetAreaRect(dtaMonth), vPt) then
    Result := dtaMonth
  else
  if PtInRect(GetAreaRect(dtaDay), vPt) then
    Result := dtaDay
  else
  if PtInRect(GetAreaRect(dtaHour), vPt) then
    Result := dtaHour
  else
  if PtInRect(GetAreaRect(dtaMinute), vPt) then
    Result := dtaMinute
  else
  if PtInRect(GetAreaRect(dtaSecond), vPt) then
    Result := dtaSecond
  else
  if PtInRect(GetAreaRect(dtaMillisecond), vPt) then
    Result := dtaMillisecond
  else
    Result := dtaNone;
end;

function THCDateTimePicker.GetAreaRect(const AArea: TDateTimeArea): TRect;
var
  vCanvas: TCanvas;
  vSize: TSize;
  vS: string;
  vCharOffset, vAppendLevel: Integer;

  {$REGION '内部函数'}
  {procedure AppendChars(P: PChar; Count: Integer);
  begin
    Inc(vCharOffset, Count);
  end;}

  function NumberText(Number, Digits: Integer): string;
  const
    Format: array[0..3] of Char = '%.*d';
  var
    vNumBuf: array[0..15] of Char;
    vLen: Integer;
  begin
    vLen := FormatBuf(vNumBuf, System.Length(vNumBuf), Format,
      System.Length(Format), [Digits, Number]);
    SetString(Result, vNumBuf, vLen);
  end;

  procedure AppendFormat(Format: PChar);
  var
    Starter, Token, LastToken: Char;
    DateDecoded, TimeDecoded, Use12HourClock,
    BetweenQuotes: Boolean;
    P: PChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    procedure GetCount;  // 获取连续是Starter的字符有几个
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do
        Inc(Format);
      Count := Format - P + 1;
    end;

    procedure GetDate;  // 分解日期
    begin
      if not DateDecoded then
      begin
        DecodeDate(FDateTime, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

    procedure GetTime;  // 分解时间
    begin
      if not TimeDecoded then
      begin
        DecodeTime(FDateTime, Hour, Min, Sec, MSec);
        TimeDecoded := True;
      end;
    end;

    function ConvertEraString(const Count: Integer) : string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
      P: PChar;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      FormatStr := 'gg';
      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, System.Length(Buffer)) <> 0
      then
      begin
        Result := Buffer;
        if Count = 1 then
        begin
          case SysLocale.PriLangID of
            LANG_JAPANESE:
              Result := Copy(Result, 1, CharToBytelen(Result, 1));
            LANG_CHINESE:
              if (SysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                and (ByteToCharLen(Result, System.Length(Result)) = 4) then
              begin
                P := Buffer + CharToByteIndex(Result, 3) - 1;
                SetString(Result, P, CharToByteLen(P, 2));
              end;
          end;
        end;
      end;
    end;

    function ConvertYearString(const Count: Integer): string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      if Count <= 2 then
        FormatStr := 'yy' // avoid Win95 bug.
      else
        FormatStr := 'yyyy';

      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, System.Length(Buffer)) <> 0
      then
      begin
        Result := Buffer;
        if (Count = 1) and (Result[1] = '0') then
          Result := Copy(Result, 2, System.Length(Result)-1);
      end;
    end;

  begin
    if (Format <> nil) and (vAppendLevel < 2) then
    begin
      Inc(vAppendLevel);
      LastToken := ' ';
      DateDecoded := False;
      TimeDecoded := False;
      Use12HourClock := False;
      while Format^ <> #0 do
      begin
        Starter := Format^;  // 当前字符串第1个字符
        if IsLeadChar(Starter) then  // 本地 MBCS Ansi 字符的集合
        begin
          Format := StrNextChar(Format);
          LastToken := ' ';
          Continue;
        end;
        Format := StrNextChar(Format);
        Token := Starter;
        if Token in ['a'..'z'] then
          Dec(Token, 32);
        if Token in ['A'..'Z'] then
        begin
          if (Token = 'M') and (LastToken = 'H') then
            Token := 'N';
          LastToken := Token;
        end;
        case Token of
          'Y': // 年
            begin
              GetCount;  // 年有几位
              GetDate;   // 分解日期
              if AArea = dtaYear then  // 年起始坐标
              begin
                vS := Copy(Self.Text, 1, vCharOffset);
                Result.Left := FMargin + vCanvas.TextWidth(vS);
              end;

              if Count <= 2 then  // 年数据
              begin
                vS := NumberText(Year mod 100, 2);
                Inc(vCharOffset, System.Length(vS));
              end
              else
              begin
                vS := NumberText(Year, 4);
                Inc(vCharOffset, System.Length(vS));
              end;

              if AArea = dtaYear then  // 年数据所在范围
              begin
                vSize := vCanvas.TextExtent(vS);
                Result.Top := (Height - vSize.cy) div 2;
                Result.Right := Result.Left + vSize.cx;
                Result.Bottom := Result.Top + vSize.cy;
              end;
            end;
          'G':  // 公元
            begin
              GetCount;  // 公元几位显示
              GetDate;
              //AppendString(ConvertEraString(Count));
              Inc(vCharOffset, System.Length(ConvertEraString(Count)));
            end;
          'E':  // 年，只有一个E时表示后2位年
            begin
              GetCount;
              GetDate;

              if AArea = dtaYear then // 年起始坐标
              begin
                vS := Copy(Self.Text, 1, vCharOffset);
                Result.Left := FMargin + vCanvas.TextWidth(vS);
              end;

              // 年数据
              vS := ConvertYearString(Count);
              Inc(vCharOffset, System.Length(vS));
              if AArea = dtaYear then  // 年数据所在范围
              begin
                vSize := vCanvas.TextExtent(vS);
                Result.Top := (Height - vSize.cy) div 2;
                Result.Right := Result.Left + vSize.cx;
                Result.Bottom := Result.Top + vSize.cy;
              end;
            end;
          'M':  // 月
            begin
              GetCount;  // 月份几位显示
              GetDate;

              if AArea = dtaMonth then  // 月起始坐标
              begin
                vS := Copy(Self.Text, 1, vCharOffset);
                Result.Left := FMargin + vCanvas.TextWidth(vS);
              end;

              // 月数据
              case Count of
                1, 2:
                  vS := NumberText(Month, Count);
                3:
                  vS := {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortMonthNames[Month];
              else
                vS := {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongMonthNames[Month];
              end;
              Inc(vCharOffset, System.Length(vS));

              if AArea = dtaMonth then  // 月数据所在范围
              begin
                vSize := vCanvas.TextExtent(vS);
                Result.Top := (Height - vSize.cy) div 2;
                Result.Right := Result.Left + vSize.cx;
                Result.Bottom := Result.Top + vSize.cy;
              end;
            end;
          'D': // 日
            begin
              GetCount;  // 日期几位显示

              if AArea = dtaDay then  // 日起始坐标
              begin
                vS := Copy(Self.Text, 1, vCharOffset);
                Result.Left := FMargin + vCanvas.TextWidth(vS);
              end;

              // 日数据
              case Count of
                1, 2:
                  begin
                    GetDate;
                    vS := NumberText(Day, Count);
                  end;
                3: vS := {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortDayNames[DayOfWeek(FDateTime)];
                4: vS := {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongDayNames[DayOfWeek(FDateTime)];
              end;
              Inc(vCharOffset, System.Length(vS));

              if AArea = dtaDay then  // 日数据所在范围
              begin
                vSize := vCanvas.TextExtent(vS);
                Result.Top := (Height - vSize.cy) div 2;
                Result.Right := Result.Left + vSize.cx;
                Result.Bottom := Result.Top + vSize.cy;
              end;

              {if Count = 5 then
                AppendFormat(Pointer(FormatSettings.ShortDateFormat))
              else
              if Count > 5 then
                AppendFormat(Pointer(FormatSettings.LongDateFormat)); }
            end;
          'H': // 时
            begin
              GetCount;  // 小时几位显示
              GetTime;   // 分散时间
              BetweenQuotes := False;
              P := Format;
              while P^ <> #0 do
              begin
                if IsLeadChar(P^) then
                begin
                  P := StrNextChar(P);
                  Continue;
                end;
                case P^ of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if ( (StrLIComp(P, 'AM/PM', 5) = 0)
                        or (StrLIComp(P, 'A/P',   3) = 0)
                        or (StrLIComp(P, 'AMPM',  4) = 0) ) then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"':
                    BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := Hour;
              if Use12HourClock then
                if H = 0 then
                  H := 12
                else
                if H > 12 then
                  Dec(H, 12);
              if Count > 2 then
                Count := 2;
              //AppendNumber(H, Count);

              if AArea = dtaHour then  // 时起始坐标
              begin
                vS := Copy(Self.Text, 1, vCharOffset);
                Result.Left := FMargin + vCanvas.TextWidth(vS);
              end;

              // 时数据
              vS := NumberText(H, Count);
              Inc(vCharOffset, System.Length(vS));

              if AArea = dtaHour then  // 时数据所在范围
              begin
                vSize := vCanvas.TextExtent(vS);
                Result.Top := (Height - vSize.cy) div 2;
                Result.Right := Result.Left + vSize.cx;
                Result.Bottom := Result.Top + vSize.cy;
              end;
            end;
          'N':  // 分
            begin
              GetCount;
              GetTime;
              if Count > 2 then
                Count := 2;
              //AppendNumber(Min, Count);

              // 分起始坐标
              if AArea = dtaMinute then
              begin
                vS := Copy(Self.Text, 1, vCharOffset);
                Result.Left := FMargin + vCanvas.TextWidth(vS);
              end;

              // 分数据
              vS := NumberText(Min, Count);
              Inc(vCharOffset, System.Length(vS));

              if AArea = dtaMinute then  // 分数据所在范围
              begin
                vSize := vCanvas.TextExtent(vS);
                Result.Top := (Height - vSize.cy) div 2;
                Result.Right := Result.Left + vSize.cx;
                Result.Bottom := Result.Top + vSize.cy;
              end;
            end;
          'S':  // 秒
            begin
              GetCount;
              GetTime;
              if Count > 2 then
                Count := 2;
              //AppendNumber(Sec, Count);

              if AArea = dtaSecond then  // 秒起始坐标
              begin
                vS := Copy(Self.Text, 1, vCharOffset);
                Result.Left := FMargin + vCanvas.TextWidth(vS);
              end;

              // 秒数据
              vS := NumberText(Sec, Count);
              Inc(vCharOffset, System.Length(vS));

              if AArea = dtaSecond then  // 秒数据所在范围
              begin
                vSize := vCanvas.TextExtent(vS);
                Result.Top := (Height - vSize.cy) div 2;
                Result.Right := Result.Left + vSize.cx;
                Result.Bottom := Result.Top + vSize.cy;
              end;
            end;
          'T':  // 时间
            begin
              GetCount;
              if Count = 1 then
                AppendFormat(Pointer({$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortTimeFormat))
              else
                AppendFormat(Pointer({$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongTimeFormat));
            end;
          'Z':  // 毫秒
            begin
              GetCount;
              GetTime;
              if Count > 3 then
                Count := 3;
              //AppendNumber(MSec, Count);

              if AArea = dtaMillisecond then  // 毫秒起始坐标
              begin
                vS := Copy(Self.Text, 1, vCharOffset);
                Result.Left := FMargin + vCanvas.TextWidth(vS);
              end;

              // 毫秒数据
              vS := NumberText(MSec, Count);
              Inc(vCharOffset, System.Length(vS));

              if AArea = dtaMillisecond then  // 毫秒数据所在范围
              begin
                vSize := vCanvas.TextExtent(vS);
                Result.Top := (Height - vSize.cy) div 2;
                Result.Right := Result.Left + vSize.cx;
                Result.Bottom := Result.Top + vSize.cy;
              end;
            end;
          'A':  // 上午、下午
            begin
              GetTime;
              P := Format - 1;
              if StrLIComp(P, 'AM/PM', 5) = 0 then
              begin
                if Hour >= 12 then
                  Inc(P, 3);
                //AppendChars(P, 2);
                Inc(vCharOffset, 2);
                Inc(Format, 4);
                Use12HourClock := TRUE;
              end
              else
              if StrLIComp(P, 'A/P', 3) = 0 then
              begin
                if Hour >= 12 then
                  Inc(P, 2);
                //AppendChars(P, 1);
                Inc(vCharOffset);
                Inc(Format, 2);
                Use12HourClock := TRUE;
              end
              else
              if StrLIComp(P, 'AMPM', 4) = 0 then
              begin
                if Hour < 12 then
                begin
                  //AppendString(TimeAMString);
                  Inc(vCharOffset, System.Length({$IFDEF DELPHIXE}FormatSettings.{$ENDIF}TimeAMString));
                end
                else
                begin
                  //AppendString(TimePMString);
                  Inc(vCharOffset, System.Length({$IFDEF DELPHIXE}FormatSettings.{$ENDIF}TimePMString));
                end;
                Inc(Format, 3);
                Use12HourClock := TRUE;
              end
              else
              if StrLIComp(P, 'AAAA', 4) = 0 then
              begin
                GetDate;
                //AppendString(LongDayNames[DayOfWeek(DateTime)]);
                Inc(vCharOffset, System.Length({$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongDayNames[DayOfWeek(FDateTime)]));
                Inc(Format, 3);
              end
              else
              if StrLIComp(P, 'AAA', 3) = 0 then
              begin
                GetDate;
                //AppendString(ShortDayNames[DayOfWeek(DateTime)]);
                Inc(vCharOffset, System.Length({$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortDayNames[DayOfWeek(FDateTime)]));
                Inc(Format, 2);
              end
              else
              begin
                //AppendChars(@Starter, 1);
                Inc(vCharOffset);
              end;
            end;
          'C':  // 短格式日期时间
            begin
              GetCount;
              AppendFormat(Pointer({$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortDateFormat));
              GetTime;
              if (Hour <> 0) or (Min <> 0) or (Sec <> 0) then
              begin
                //AppendChars(' ', 1);
                Inc(vCharOffset);
                AppendFormat(Pointer({$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongTimeFormat));
              end;
            end;
          '/':  // 日期分隔
            if {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DateSeparator <> #0 then
            begin
              //AppendChars(@DateSeparator, 1);
              Inc(vCharOffset);
            end;
          ':':  // 时间分隔
            if {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}TimeSeparator <> #0 then
            begin
              //AppendChars(@TimeSeparator, 1);
              Inc(vCharOffset);
            end;
          '''', '"':  // 无效字符不做输出?
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
                if IsLeadChar(Format^) then
                  Format := StrNextChar(Format)
                else
                  Inc(Format);
              end;
              //AppendChars(P, Format - P);
              Inc(vCharOffset, Format - P);
              if Format^ <> #0 then
                Inc(Format);
            end;
        else
          begin
            //AppendChars(@Starter, 1);
            Inc(vCharOffset);
          end;
        end;
      end;
      Dec(vAppendLevel);
    end;
  end;
  {$ENDREGION}

begin
  SetRectEmpty(Result);

  if AArea = dtaNone then Exit;

  vCharOffset := 0;
  vAppendLevel := 0;
  vCanvas := THCStyle.CreateStyleCanvas;
  try
    Self.OwnerData.Style.TextStyles[Self.TextStyleNo].ApplyStyle(vCanvas);
    if FFormat <> '' then
      AppendFormat(PChar(FFormat))
    else
      AppendFormat('C');  // C 用短格式显示日期与时间
  finally
    THCStyle.DestroyStyleCanvas(vCanvas);
  end;
end;

procedure THCDateTimePicker.GetCaretInfo(var ACaretInfo: THCCaretInfo);
begin
  ACaretInfo.Visible := False;
end;

function THCDateTimePicker.InsertText(const AText: string): Boolean;
begin
end;

procedure THCDateTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:  // 取消输入的年字符串
      begin
        if FNewYear <> '' then
        begin
          FNewYear := '';
          Self.OwnerData.Style.UpdateInfoRePaint;
        end;
      end;

    VK_RETURN:
      begin
        if FActiveArea = dtaYear then
        begin
          SetInputYear;
          Self.OwnerData.Style.UpdateInfoRePaint;
        end;
      end;

    VK_LEFT:
      begin
        if FActiveArea > dtaNone then
        begin
          if FActiveArea = dtaYear then
            SetInputYear;

          FActiveArea := System.Pred(FActiveArea);
          FAreaRect := GetAreaRect(FActiveArea);
          Self.OwnerData.Style.UpdateInfoRePaint;
        end;
      end;

    VK_RIGHT:
      begin
        if FActiveArea < dtaMillisecond then
        begin
          if FActiveArea = dtaYear then
            SetInputYear;

          FActiveArea := System.Succ(FActiveArea);
          FAreaRect := GetAreaRect(FActiveArea);
          Self.OwnerData.Style.UpdateInfoRePaint;
        end;
      end;
  end;
end;

procedure THCDateTimePicker.KeyPress(var Key: Char);
var
  vNumber, vCount: Word;
  vDateTime: TDateTime;
begin
  if Self.ReadOnly then Exit;

  vDateTime := FDateTime;

  if FActiveArea <> dtaNone then
  begin
    if Key in ['0'..'9'] then
    begin
      case FActiveArea of
        dtaYear:
          begin
            if System.Length(FNewYear) > 3 then
              System.Delete(FNewYear, 1, 1);
            FNewYear := FNewYear + Key;
          end;

        dtaMonth:
          begin;
            vNumber := MonthOf(vDateTime);  // 当前月份
            if vNumber > 9 then  // 当前月份已经是2位数了
            begin
              if Key = '0' then Exit;  // 2位月份再输入0不处理

              vDateTime := RecodeMonth(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end
            else  // 当前月份是1位数字
            if (vNumber = 1) and FJoinKey then  // 当前月份是1月且是连续输入
            begin
              if Key in ['0'..'2'] then  // 10, 11, 12
              begin
                vNumber := vNumber * 10 + StrToInt(Key);
                vDateTime := RecodeMonth(vDateTime, vNumber);  // 直接修改为新键入
              end;
            end
            else  // 不是连续输入，是第1次输入
            begin
              if Key = '0' then Exit;  // 月份第1位是0不处理

              vDateTime := RecodeMonth(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end;
          end;

        dtaDay:
          begin
            vNumber := DayOf(vDateTime);  // 当前日期
            if vNumber > 9 then  // 当前日期已经是2位数了
            begin
              if Key = '0' then Exit;  // 2位日期再输入0不处理

              vDateTime := RecodeDay(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end
            else  // 当前日期是1位数字
            if FJoinKey then  // 是连续输入
            begin
              vNumber := vNumber * 10 + StrToInt(Key);
              vCount := DaysInMonth(vDateTime);
              if vNumber > vCount then
                vNumber := StrToInt(Key);
              vDateTime := RecodeDay(vDateTime, vNumber);  // 直接修改为新键入
            end
            else  // 不是连续输入，是第1次输入
            begin
              if Key = '0' then Exit;  // 月份第1位是0不处理

              vDateTime := RecodeDay(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end;
          end;

        dtaHour:
          begin
            vNumber := HourOf(vDateTime);  // 当前时
            if vNumber > 9 then  // 当前时已经是2位数了
            begin
              if Key = '0' then Exit;  // 2位时再输入0不处理

              vDateTime := RecodeHour(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end
            else  // 当前时是1位数字
            if FJoinKey then  // 当前时是是连续输入
            begin
              vNumber := vNumber * 10 + StrToInt(Key);
              if vNumber > 24 then
                vNumber := StrToInt(Key);
              vDateTime := RecodeHour(vDateTime, vNumber);  // 直接修改为新键入
            end
            else  // 不是连续输入，是第1次输入
            begin
              if Key = '0' then Exit;  // 时第1位是0不处理

              vDateTime := RecodeHour(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end;
          end;

        dtaMinute:
          begin
            vNumber := MinuteOf(vDateTime);  // 当前分
            if vNumber > 9 then  // 当前分已经是2位数了
            begin
              if Key = '0' then Exit;  // 2位时再输入0不处理

              vDateTime := RecodeMinute(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end
            else  // 当前分是1位数字
            if FJoinKey then  // 当前分是是连续输入
            begin
              vNumber := vNumber * 10 + StrToInt(Key);
              if vNumber > 60 then
                vNumber := StrToInt(Key);
              vDateTime := RecodeMinute(vDateTime, vNumber);  // 直接修改为新键入
            end
            else  // 不是连续输入，是第1次输入
            begin
              if Key = '0' then Exit;  // 分第1位是0不处理

              vDateTime := RecodeMinute(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end;
          end;

        dtaSecond:
          begin
            vNumber := SecondOf(vDateTime);  // 当前秒
            if vNumber > 9 then  // 当前秒已经是2位数了
            begin
              if Key = '0' then Exit;  // 2位时再输入0不处理

              vDateTime := RecodeSecond(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end
            else  // 当前秒是1位数字
            if FJoinKey then  // 当前秒是是连续输入
            begin
              vNumber := vNumber * 10 + StrToInt(Key);
              if vNumber > 60 then
                vNumber := StrToInt(Key);
              vDateTime := RecodeSecond(vDateTime, vNumber);  // 直接修改为新键入
            end
            else  // 不是连续输入，是第1次输入
            begin
              if Key = '0' then Exit;  // 秒第1位是0不处理

              vDateTime := RecodeSecond(vDateTime, StrToInt(Key));  // 直接修改为新键入
            end;
          end;

        dtaMillisecond: Exit;
      end;
    end;

    if FActiveArea <> dtaYear then // 除年外，其他的需要实时更新
    begin
      FActiveArea := GetAreaAt(FAreaRect.Left, FAreaRect.Top);
      if FActiveArea <> dtaNone then
        FAreaRect := GetAreaRect(FActiveArea);

      FJoinKey := True;
      SetDateTime(vDateTime);
    end;

    Self.OwnerData.Style.UpdateInfoRePaint;
  end;
end;

procedure THCDateTimePicker.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FFormat);  // 读取Format
  AStream.ReadBuffer(FDateTime, SizeOf(FDateTime));
end;

procedure THCDateTimePicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vArea: TDateTimeArea;
begin
  //inherited MouseDown(Button, Shift, X, Y);
  Self.Active := PtInRect(Rect(0, 0, Width, Height), Point(X, Y));

  vArea := GetAreaAt(X, Y);
  if vArea <> FActiveArea then
  begin
    if FActiveArea = dtaYear then
      SetInputYear;

    FActiveArea := vArea;
    if FActiveArea <> dtaNone then
      FAreaRect := GetAreaRect(FActiveArea);

    Self.OwnerData.Style.UpdateInfoRePaint;
  end;
end;

procedure THCDateTimePicker.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FFormat := ANode.Attributes['format'];
  FDateTime := StrToDateTime(ANode.Attributes['datetime']);
end;

procedure THCDateTimePicker.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FFormat);  // 存Format
  AStream.WriteBuffer(FDateTime, SizeOf(FDateTime));
end;

procedure THCDateTimePicker.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Self.Active then
  begin
    if FActiveArea = dtaYear then
      SetInputYear;

    FActiveArea := TDateTimeArea.dtaNone;
  end;
end;

procedure THCDateTimePicker.SetDateTime(const Value: TDateTime);
begin
  if FDateTime <> Value then
  begin
    FDateTime := Value;
    Self.Text := FormatDateTime(FFormat, FDateTime);
    FAreaRect := GetAreaRect(FActiveArea);
  end;
end;

procedure THCDateTimePicker.SetFormat(const Value: string);
begin
  if FFormat <> Value then
  begin
    FFormat := Value;
    Self.Text := FormatDateTime(FFormat, FDateTime);
    FAreaRect := GetAreaRect(FActiveArea);
  end;
end;

procedure THCDateTimePicker.SetInputYear;

  function GetYear(const AYear: string): Word;

    function Power10(const Sqr: Byte): Cardinal;
    var
      i: Integer;
    begin
      Result := 10;
      for i := 2 to Sqr do
        Result := Result * 10;
    end;

  var
    vYear: Word;
    vPie: Cardinal;
  begin
    Result := YearOf(FDateTime);
    vYear := StrToIntDef(AYear, Result);
    if vYear < Result then
    begin
      vPie := Power10(System.Length(AYear));
      Result := Result div vPie;
      Result := Result * vPie + vYear;
    end
    else
      Result := vYear;
  end;

begin
  if FNewYear <> '' then  // 有输入年，根据输入字符串确定输入的年
  begin
    Self.DateTime := RecodeYear(FDateTime, GetYear(FNewYear));
    FNewYear := '';  // 取消输入的年内容
  end;
end;

procedure THCDateTimePicker.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['format'] := FFormat;
  ANode.Attributes['datetime'] := DateTimeToStr(FDateTime);
end;

function THCDateTimePicker.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := True;
end;

end.
