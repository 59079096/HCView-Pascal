unit HCZLib;

interface

uses
  Windows, Classes, SysUtils, Generics.Defaults, Generics.Collections;

type
  THCZLibCentralDirEntry = packed record
    Sign: Integer;
    Version: Word;
    VersionToExtract: Word;
    Flag: Word;
    Method: Word;
    DateTime: Cardinal;
    CRC32: Integer;
    PackSize: Integer;
    OriginalSize: Integer;
    NameLength: Word;
    ExtraLength: Word;
    CommentLength: Word;
    DiskStart: Word;
    IntAttributes: Word;
    ExtAttributes: Integer;
    RelativeOffset: Integer;
  end;

  THCZLibEndOfDir = packed record
    Sign: Integer;
    DiskNumber: Word;
    NumberOfDiskStart: Word;
    DirStart: Word;
    DirEntryCount: Word;
    DirSize: Integer;
    DirOffset: Integer;
    CommentLength: Word;
  end;

  THCZLibFileEntry = packed record
  private
    function GetDataPosition: Integer;
    function GetSizeForStream: Integer;
  public
    Sign: Integer;
    VersionToExtract: Word;
    Flag: Word;
    Method: Word;
    DateTime: Cardinal;
    CRC32: Cardinal;
    PackSize: Integer;
    OriginalSize: Integer;
    NameLength: Word;
    ExtraLength: Word;
    // 附加的字段
    IsDirectory: Boolean;
    Name: AnsiString;
    RelativeOffset: Integer;

    constructor Create(const AName: AnsiString; const ADateTime: Cardinal; const ACRC32, APackSize, AOriginalSize: Integer);
    class function CheckName(const AName: AnsiString): AnsiString; static;
    procedure LoadFromStream(const AStream: TStream; const AOffset: Integer);
    procedure WriteToStream(const AStream: TStream);
  end;

  THCZLibStreamReaderUnpackMethod = procedure (const APackSize: Integer; const ADestStream: TStream) of object;

  THCZLib = class(TObject)
  private
    FUnpackMethods: TDictionary<Word, THCZLibStreamReaderUnpackMethod>;
    FFiles: TList<THCZLibFileEntry>;
    FFileStream: TStream;
  protected
    procedure UnpackDeflate(const APackSize: Integer; const ADestStream: TStream);
    procedure UnpackNone(const APackSize: Integer; const ADestStream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Decompress(const APackStream: TStream;
      const APackSize: Integer; const ADestStream: TStream); static;
    procedure LoadFromStream(const AStream: TStream);
    procedure Extract(const AEntry: THCZLibFileEntry; const ADestStream: TStream);
    procedure AppendFile(const AFileName: string; const AFileStream: TStream);
    procedure SetFileStream(const AStream: TStream);
    procedure FinishFile;
    property Files: TList<THCZLibFileEntry> read FFiles;
  end;

implementation

uses
  ZLib, Math, AnsiStrings;

const
  HCUnixPathDelim = AnsiChar('/');
  ZLibHeader: Word = $9C78;
  ZLibHeaderSize = 2;
  ZLibFooterSize = 4;

  ZLibFileHeader = $04034B50;
  ZLibEndOfDir = $06054B50;
  ZLibDirFileHeader = $02014B50;

  HCZLIBERROR = 'HCZLib异常：';
  HCZLIBERROR_ENDOFDIR = HCZLIBERROR+ '不正确的结束数据！';
  HCZLIBERROR_DIRFILEHEADER = HCZLIBERROR+ '不正确的路径文件头数据！';
  HCZLIBERROR_FILEHEADER = HCZLIBERROR+ '不正确的文件头数据！';
  HCZLIBERROR_UNSUPPORTCOMPRESS = HCZLIBERROR+ '不支持的压缩方法！';

  HCCRCTABLE: array[0..255] of Cardinal =
   ($00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

function HCCRC32(AData: PByte; ACount: Integer; ACurrentCRC32: Cardinal): Cardinal; overload;
var
  i: Integer;
begin
  Result := ACurrentCRC32;
  for i := 0 to ACount - 1 do
  begin
    Result := (Result shr 8) xor HCCRCTABLE[Byte(Result) xor AData^];
    Inc(AData);
  end;
end;

function HCCRC32(AStream: TStream; const APosition: Int64; ACount: Int64): Cardinal; overload;
var
  vBuffer: PByte;
  vRead: Integer;
  vSavedPosition: Int64;
begin
  vSavedPosition := AStream.Position;
  try
    AStream.Position := APosition;
    vBuffer := AllocMem(MaxWord);
    try
      Result := $FFFFFFFF;
      repeat
        vRead := AStream.Read(vBuffer^, Min(MaxWord, ACount));
        Result := HCCRC32(vBuffer, vRead, Result);
        Dec(ACount, vRead);
      until vRead = 0;
      Result := Result xor $FFFFFFFF;
    finally
      FreeMem(vBuffer);
    end;
  finally
    AStream.Position := vSavedPosition;
  end;
end;

procedure CheckZLibRead(AValue: Boolean; const AMessage: string = '未知！');
begin
  if not AValue then
    raise Exception.Create(HCZLIBERROR + AMessage);
end;

{ THCZLib }

procedure THCZLib.AppendFile(const AFileName: string; const AFileStream: TStream);
var
  vStream: TMemoryStream;
  vSize: Integer;
  vCRC32: Cardinal;
  vPByte: PByte;
  vFileEntry: THCZLibFileEntry;
begin
  AFileStream.Position := 0;
  vCRC32 := HCCRC32(AFileStream, 0, AFileStream.Size);
  {vPByte := AllocMem(AFileStream.Size);
  try
    AFileStream.ReadBuffer(vPByte^, AFileStream.Size);
    vCRC32 := HCCRC32(vPByte, AFileStream.Size, $FFFFFFFF);
    vCRC32 := not vCRC32;
  finally
    FreeMem(vPByte);
  end;}


  vStream := TMemoryStream.Create;
  try
    ZCompressStream(AFileStream, vStream);
//    vCompressStream := TZCompressionStream.Create(vStream);
//    try
//      //vCRC32 := HCCRC32(AFileStream, 0, AFileStream.Size);
//      vCompressStream.Write(AFileStream, AFileStream.Size);
//      //vCompressStream.Write(AFileStream, 0);
//    finally
//      FreeAndNil(vCompressStream);
//    end;
//
    vPByte := vStream.Memory;
    vSize := vStream.Size;
    if vSize > 0 then
    begin
      Inc(vPByte, ZLibHeaderSize);
      Dec(vSize, ZLibFooterSize);
      Dec(vSize, ZLibHeaderSize);
    end;

    vFileEntry := THCZLibFileEntry.Create(AFileName, DateTimeToFileDate(Now), vCRC32, vSize, AFileStream.Size);
    vFileEntry.RelativeOffset := FFileStream.Position;
    vFileEntry.WriteToStream(FFileStream);
    FFileStream.WriteBuffer(vPByte^, vSize);

    FFiles.Add(vFileEntry);
  finally
    FreeAndNil(vStream);
  end;
end;

constructor THCZLib.Create;
begin
  FFiles := TList<THCZLibFileEntry>.Create;
  FUnpackMethods := TDictionary<Word, THCZLibStreamReaderUnpackMethod>.Create;
end;

class procedure THCZLib.Decompress(const APackStream: TStream;
  const APackSize: Integer; const ADestStream: TStream);

  procedure FlushBuffer(var AZStream: TZStreamRec; AOutBuffer: Pointer; AOutBufferSize: Integer);
  var
    vError: Integer;
  begin
    repeat
      AZStream.next_out := Pointer(AOutBuffer);
      AZStream.avail_out := AOutBufferSize;
      vError := inflate(AZStream, Z_FINISH);
      if vError = Z_BUF_ERROR then
        vError := Z_STREAM_END;
      CheckZLibRead(vError >= Z_OK);
      ADestStream.Write(AOutBuffer^, AOutBufferSize - Integer(AZStream.avail_out));
    until (vError = Z_STREAM_END) and (AZStream.avail_out > 0);
  end;

  procedure ProcessBuffer(var AZStream: TZStreamRec;
    AInBuffer, AOutBuffer: Pointer; AInBufferSize, AOutBufferSize: Integer);
  var
    vError: Integer;
  begin
    AZStream.avail_in := AInBufferSize;
    AZStream.next_in := AInBuffer;
    repeat
      AZStream.next_out := AOutBuffer;
      AZStream.avail_out := AOutBufferSize;
      vError := inflate(AZStream, Z_NO_FLUSH);
      CheckZLibRead((vError >= Z_OK) or (vError = Z_BUF_ERROR));
      ADestStream.Write(AOutBuffer^, AOutBufferSize - Integer(AZStream.avail_out));
    until (AZStream.avail_in = 0) and (AZStream.avail_out > 0);
  end;

const
  BufferSize = 32768;

var
  vInBuffer: Pointer;
  vInBufferSize, vPackedSize: Integer;
  vOutBuffer: Pointer;
  AZStream: TZStreamRec;
begin
  if APackSize <= 0 then
    Exit;

  vPackedSize := APackSize;

  vInBuffer := AllocMem(BufferSize);
  try
    vOutBuffer := AllocMem(BufferSize);
    try
      FillChar(AZStream, SizeOf(TZStreamRec), 0);

      CheckZLibRead(InflateInit_(AZStream, ZLIB_VERSION, SizeOf(AZStream)) >= 0);
      try
        ProcessBuffer(AZStream, @ZLibHeader, vOutBuffer, ZLibHeaderSize, BufferSize);
        repeat
          vInBufferSize := Min(vPackedSize, BufferSize);
          if vInBufferSize > 0 then
          begin
            APackStream.ReadBuffer(vInBuffer^, vInBufferSize);
            Dec(vPackedSize, vInBufferSize);
            ProcessBuffer(AZStream, vInBuffer, vOutBuffer, vInBufferSize, BufferSize);
          end;
        until vInBufferSize = 0;
        FlushBuffer(AZStream, vOutBuffer, BufferSize);
      finally
        CheckZLibRead(inflateEnd(AZStream) >= 0);
      end;
    finally
      FreeMem(vOutBuffer);
    end;
  finally
    FreeMem(vInBuffer);
  end
end;

destructor THCZLib.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure THCZLib.Extract(const AEntry: THCZLibFileEntry; const ADestStream: TStream);
var
  vUnpackMethod: THCZLibStreamReaderUnpackMethod;
  vPosition: Int64;
begin
  if not FUnpackMethods.TryGetValue(AEntry.Method, vUnpackMethod) then
    raise Exception.Create(HCZLIBERROR_UNSUPPORTCOMPRESS);

  vPosition := ADestStream.Position;
  FFileStream.Position := AEntry.GetDataPosition;
  vUnpackMethod(AEntry.PackSize, ADestStream);

  if AEntry.CRC32 <> 0 then
  begin
    CheckZLibRead((AEntry.CRC32 = HCCRC32(ADestStream, vPosition, ADestStream.Position - vPosition)),
      AEntry.Name + ' 不完整！');
  end;
end;

procedure THCZLib.FinishFile;
var
  vDirData: THCZLibCentralDirEntry;
  vEndOfDir: THCZLibEndOfDir;
  vFileEntry: THCZLibFileEntry;
  i: Integer;
begin
  FillChar(vEndOfDir, SizeOf(vEndOfDir), 0);
  FillChar(vDirData, SizeOf(vDirData), 0);
  vEndOfDir.Sign := ZLibEndOfDir;
  vEndOfDir.DirOffset := FFileStream.Size;
  vDirData.Sign := ZLibDirFileHeader;
  vDirData.Version := 20;
  vDirData.VersionToExtract := 20;
  vDirData.Method := 8;

  for i := 0 to FFiles.Count - 1 do
  begin
    vFileEntry := Files[i];
    vDirData.CRC32 := vFileEntry.CRC32;
    vDirData.PackSize := vFileEntry.PackSize;
    vDirData.OriginalSize := vFileEntry.OriginalSize;
    vDirData.ExtAttributes := IfThen(vFileEntry.IsDirectory, faDirectory);
    vDirData.DateTime := vFileEntry.DateTime;
    vDirData.NameLength := Length(vFileEntry.Name);
    vDirData.RelativeOffset := vFileEntry.RelativeOffset;
    FFileStream.WriteBuffer(vDirData, SizeOf(vDirData));
    FFileStream.WriteBuffer(vFileEntry.Name[1], vDirData.NameLength);
    Inc(vEndOfDir.DirEntryCount);
  end;
  vEndOfDir.DirStart := vEndOfDir.DirEntryCount;
  vEndOfDir.DirSize := FFileStream.Position - vEndOfDir.DirOffset;
  FFileStream.Write(vEndOfDir, SizeOf(vEndOfDir));
end;

procedure THCZLib.LoadFromStream(const AStream: TStream);
var
  vDir: THCZLibCentralDirEntry;
  vDirs: TList<THCZLibCentralDirEntry>;
  vEndOfDir: THCZLibEndOfDir;
  i: Integer;
  vFileEntry: THCZLibFileEntry;
begin
  FFileStream := AStream;
  FUnpackMethods.Add(0, UnpackNone);
  FUnpackMethods.Add(8, UnpackDeflate);

  AStream.ReadBuffer(vDir, SizeOf(vDir));
  if vDir.Sign = ZLibFileHeader then
  begin
    vDirs := TList<THCZLibCentralDirEntry>.Create;
    try
      AStream.Position := AStream.Size;
      repeat
        AStream.Seek(-SizeOf(THCZLibEndOfDir), soCurrent);
        AStream.ReadBuffer(vEndOfDir, SizeOf(vEndOfDir));
        AStream.Seek(-1, soCurrent);
      until (vEndOfDir.Sign = ZLibEndOfDir) or (AStream.Position < SizeOf(vEndOfDir));
      CheckZLibRead(vEndOfDir.Sign = ZLibEndOfDir, HCZLIBERROR_ENDOFDIR);

      AStream.Position := vEndOfDir.DirOffset;
      vDirs.Capacity := vEndOfDir.DirEntryCount;
      for i := 0 to vEndOfDir.DirEntryCount - 1 do
      begin
        AStream.ReadBuffer(vDir, SizeOf(vDir));
        CheckZLibRead(vDir.Sign = ZLibDirFileHeader, HCZLIBERROR_DIRFILEHEADER);
        vDirs.Add(vDir);
        AStream.Seek(vDir.NameLength + vDir.ExtraLength + vDir.CommentLength, soCurrent);
      end;

      FFiles.Capacity := vDirs.Count;
      for i := 0 to vDirs.Count - 1 do
      begin
        vDir := vDirs[I];
        vFileEntry.LoadFromStream(AStream, vDir.RelativeOffset);
        vFileEntry.IsDirectory := vDir.ExtAttributes and faDirectory = faDirectory;
        if vFileEntry.OriginalSize = 0 then
          vFileEntry.OriginalSize := vDir.OriginalSize;
        if vFileEntry.PackSize = 0 then
          vFileEntry.PackSize := vDir.PackSize;
        CheckZLibRead(vFileEntry.Sign = ZLibFileHeader, HCZLIBERROR_FILEHEADER);
        CheckZLibRead(vDir.OriginalSize = vFileEntry.OriginalSize);
        CheckZLibRead(vDir.PackSize = vFileEntry.PackSize);
        FFiles.Add(vFileEntry);
      end;
    finally
      vDirs.Free;
    end;
  end;
end;

procedure THCZLib.SetFileStream(const AStream: TStream);
begin
  FFileStream := AStream;
end;

procedure THCZLib.UnpackDeflate(const APackSize: Integer; const ADestStream: TStream);
begin
  Decompress(FFileStream, APackSize, ADestStream);
end;

procedure THCZLib.UnpackNone(const APackSize: Integer; const ADestStream: TStream);
begin
  if APackSize > 0 then
    ADestStream.CopyFrom(FFileStream, APackSize);
end;

{ THCZLibFileEntry }

class function THCZLibFileEntry.CheckName(const AName: AnsiString): AnsiString;
begin
  if PathDelim <> HCUnixPathDelim then
    Result := AnsiStrings.StringReplace(AName, PathDelim, HCUnixPathDelim, [rfReplaceAll])
  else
    Result := AName;

  if (Result <> '') and (Result[1] = HCUnixPathDelim) then
    Delete(Result, 1, 1);
end;

constructor THCZLibFileEntry.Create(const AName: AnsiString; const ADateTime: Cardinal;
  const ACRC32, APackSize, AOriginalSize: Integer);
begin
  Sign := ZLibFileHeader;
  VersionToExtract := 20;
  Flag := 0;
  Method := 8;
  DateTime := ADateTime;
  CRC32 := ACRC32;
  PackSize := APackSize;
  OriginalSize := AOriginalSize;
  ExtraLength := 0;
  IsDirectory := False;
  Name := CheckName(AName);
  NameLength := Length(Name);
end;

function THCZLibFileEntry.GetDataPosition: Integer;
begin
  Result := RelativeOffset + GetSizeForStream + NameLength + ExtraLength;
end;

function THCZLibFileEntry.GetSizeForStream: Integer;
begin
  Result := 30;
end;


procedure THCZLibFileEntry.LoadFromStream(const AStream: TStream; const AOffset: Integer);
begin
  AStream.Position := AOffset;
  AStream.ReadBuffer(Self, GetSizeForStream);
  SetLength(Name, NameLength);
  AStream.ReadBuffer(Name[1], NameLength);
  RelativeOffset := AOffset;
  Name := CheckName(Name);
end;


procedure THCZLibFileEntry.WriteToStream(const AStream: TStream);
begin
  AStream.WriteBuffer(Self, GetSizeForStream);
  AStream.WriteBuffer(Name[1], NameLength);
end;

end.


