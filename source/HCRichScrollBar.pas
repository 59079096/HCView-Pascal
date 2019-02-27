{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                文档高级滚动条实现单元                 }
{                                                       }
{*******************************************************}

unit HCRichScrollBar;

interface

uses
  Windows, Classes, Controls, Graphics, Generics.Collections, HCScrollBar;

type
  TAreaMark = class(TObject)  // 区域标记
  strict private
    FPosition, FHeight: Integer;
  public
    property Position: Integer read FPosition write FPosition;
    property Height: Integer read FHeight write FHeight;
  end;

  TAreaMarks = TObjectList<TAreaMark>;

  THCRichScrollBar = class(THCScrollBar)
  private
    FAreaMarks: TAreaMarks;
  protected
    procedure DoDrawThumBefor(const ACanvas: TCanvas; const AThumRect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure AddAreaPos(const APosition, AHeight: Integer);
  end;

implementation

{ THCRichScrollBar }

procedure THCRichScrollBar.AddAreaPos(const APosition, AHeight: Integer);
var
  vAreaMark: TAreaMark;
begin
  vAreaMark := TAreaMark.Create;
  vAreaMark.Position := APosition;
  vAreaMark.Height := AHeight;

  if not Assigned(FAreaMarks) then
    FAreaMarks := TAreaMarks.Create;

  FAreaMarks.Add(vAreaMark);
end;

constructor THCRichScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor THCRichScrollBar.Destroy;
begin
  if Assigned(FAreaMarks) then
    FAreaMarks.Free;

  inherited Destroy;
end;

procedure THCRichScrollBar.DoDrawThumBefor(const ACanvas: TCanvas;
  const AThumRect: TRect);
var
  i, vDrawTop, vDrawHeight: Integer;
begin
  case Self.Orientation of
    oriHorizontal:
      begin
      end;
    oriVertical:
      begin
        if Assigned(FAreaMarks) then
        begin
          ACanvas.Brush.Color := clBlue;

          for i := 0 to FAreaMarks.Count - 1 do
          begin
            vDrawTop := ButtonSize + Round(FAreaMarks[i].Position * Percent);
            vDrawHeight := Round(FAreaMarks[i].Height * Percent);

            ACanvas.FillRect(Bounds(AThumRect.Left, vDrawTop, AThumRect.Right - AThumRect.Left, vDrawHeight));
          end;
        end;
      end;
  end;
end;

end.
