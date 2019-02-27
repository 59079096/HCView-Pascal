{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2019-1-28             }
{                                                       }
{                第三方文件读取单元                     }
{                                                       }
{*******************************************************}

unit HCDocumentRW;

interface

uses
  Classes, HCView, HCCommon;

type
  THCDocumentReader = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(const AHCView: THCView; const AStream: TStream); virtual; abstract;
    procedure SaveToStream(const AHCView: THCView; const AStream: TStream); virtual; abstract;
  end;

  procedure HCViewLoadFromDocumentFile(const AHCView: THCView; const AFileName: string; const AExt: string);
  procedure HCViewSaveToDocumentFile(const AHCView: THCView; const AFileName: string; const AExt: string);
  procedure HCViewLoadFromDocumentStream(const AHCView: THCView; const AStream: TStream; const AExt: string);
  procedure HCViewSaveDocumentStream(const AHCView: THCView; const AStream: TStream; const AExt: string);

implementation

uses
  SysUtils, HCDocxRW;

const
  HC_EXT_DOCX = '.docx';
  HCS_EXCEPTION_UNSUPPORTFILE = HC_EXCEPTION + '不支持的文件格式！';

procedure HCViewLoadFromDocumentFile(const AHCView: THCView; const AFileName: string; const AExt: string);
var
  vStream: TStream;
  vExt: string;
begin
  AHCView.FileName := AFileName;
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    if AExt = '' then
      vExt := LowerCase(ExtractFileExt(AFileName)) // 后缀
    else
      vExt := AExt;

    HCViewLoadFromDocumentStream(AHCView, vStream, vExt);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure HCViewSaveToDocumentFile(const AHCView: THCView; const AFileName: string; const AExt: string);
var
  vStream: TMemoryStream;
  vExt: string;
begin
  vStream := TMemoryStream.Create;
  try
    if AExt = '' then
      vExt := LowerCase(ExtractFileExt(AFileName)) // 后缀
    else
      vExt := AExt;

    HCViewSaveDocumentStream(AHCView, vStream, vExt);
    vStream.SaveToFile(AFileName);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure HCViewLoadFromDocumentStream(const AHCView: THCView; const AStream: TStream; const AExt: string);
var
  vDocReader: THCDocumentReader;
begin
  if AExt = HC_EXT_DOCX then
    vDocReader := THCDocxReader.Create
  else
    raise Exception.Create(HCS_EXCEPTION_UNSUPPORTFILE);

  AStream.Position := 0;
  vDocReader.LoadFromStream(AHCView, AStream);
end;

procedure HCViewSaveDocumentStream(const AHCView: THCView; const AStream: TStream; const AExt: string);
var
  vDocReader: THCDocumentReader;
begin
  if AExt = HC_EXT_DOCX then
    vDocReader := THCDocxReader.Create
  else
    raise Exception.Create(HCS_EXCEPTION_UNSUPPORTFILE);

  vDocReader.SaveToStream(AHCView, AStream);
end;

{ THCDocumentReader }

constructor THCDocumentReader.Create;
begin

end;

destructor THCDocumentReader.Destroy;
begin

  inherited Destroy;
end;

end.
