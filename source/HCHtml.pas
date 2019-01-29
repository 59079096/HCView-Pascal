{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-12-8             }
{                                                       }
{                    html格式处理                       }
{                                                       }
{*******************************************************}

unit HCHtml;

interface

uses
  HCList, HCTextStyle, HCParaStyle, HCRichData;

type
{$IFNDEF MSWINDOWS}
  WideString = String;
{$ENDIF}
  IHtmlElement = interface;
  IHtmlElementList = interface;
  TEnumAttributeNameCallBack = function(AParam: Pointer;
    const AttributeName, AttributeValue: WideString): Boolean; stdcall;
  TFilterElementCallBack = function(AParam: Pointer; AElement: IHtmlElement)
    : Boolean; stdcall;

  IHtmlElement = interface
    ['{8C75239C-8CFA-499F-B115-7CEBEDFB421B}']
    function GetOwner: IHtmlElement; stdcall;
    function GetTagName: WideString; safecall;
    function GetContent: WideString; safecall;
    function GetOrignal: WideString; safecall;
    function GetChildrenCount: Integer; stdcall;
    function GetChildren(Index: Integer): IHtmlElement; stdcall;
    function GetCloseTag: IHtmlElement; stdcall;
    function GetInnerHtml(): WideString; safecall;
    function GetOuterHtml(): WideString; safecall;
    function GetInnerText(): WideString; safecall;

    function GetAttributes(Key: WideString): WideString; safecall;

    function GetSourceLineNum(): Integer; stdcall;
    function GetSourceColNum(): Integer; stdcall;

    // 属性是否存在
    function HasAttribute(AttributeName: WideString): Boolean; stdcall;
    // 查找节点
    { FindElements('Link','type="application/rss+xml"')
      FindElements('','type="application/rss+xml"')
    }
    function FindElements(ATagName: WideString; AAttributes: WideString;
      AOnlyInTopElement: Boolean): IHtmlElementList; stdcall;
    { 用CSS选择器语法查找Element,不支持"伪类"
      CSS Selector Style search,not support Pseudo-classes.

      http://www.w3.org/TR/CSS2/selector.html
    }
    function SimpleCSSSelector(const selector: WideString)
      : IHtmlElementList; stdcall;
    // 枚举属性
    procedure EnumAttributeNames(AParam: Pointer;
      ACallBack: TEnumAttributeNameCallBack); stdcall;

    property TagName: WideString read GetTagName;
    property ChildrenCount: Integer read GetChildrenCount;
    property Children[index: Integer]: IHtmlElement read GetChildren; default;
    property CloseTag: IHtmlElement read GetCloseTag;
    property Content: WideString read GetContent;
    property Orignal: WideString read GetOrignal;
    property Owner: IHtmlElement read GetOwner;
    // 获取元素在源代码中的位置
    property SourceLineNum: Integer read GetSourceLineNum;
    property SourceColNum: Integer read GetSourceColNum;
    //
    property InnerHtml: WideString read GetInnerHtml;
    property OuterHtml: WideString read GetOuterHtml;
    property InnerText: WideString read GetInnerText;

    property Attributes[Key: WideString]: WideString read GetAttributes;
  end;

  IHtmlElementList = interface
    ['{8E1380C6-4263-4BF6-8D10-091A86D8E7D9}']
    function GetCount: Integer; stdcall;
    function GetItems(Index: Integer): IHtmlElement; stdcall;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IHtmlElement read GetItems; default;
  end;

  THCHtmlFormat = class(TObject)
  private
    FVersion: string;  // Version number of the clipboard. Starting version is 0.9.
    FStartHTML: string;  //  Byte count from the beginning of the clipboard to the start of the context, or -1 if no context.
    FEndHTML: string;  //  Byte count from the beginning of the clipboard to the end of the context, or -1 if no context.
    FStartFragment: string;  //  Byte count from the beginning of the clipboard to the start of the fragment.
    FEndFragment: string;  //  Byte count from the beginning of the clipboard to the end of the fragment.
    FStartSelection: string;  //  Byte count from the beginning of the clipboard to the start of the selection.
    FEndSelection: string;  //  Byte count from the beginning of the clipboard to the end of the selection.
    FSourceURL: string;  // This allows handlers of CF_HTML to resolve relative links within a file (such as when CF_HTML text is pasted into a DHTML Edit Control host).
    FTextStyles: THCObjectList;
    FParaStyles: THCObjectList;
    FData: THCRichData;
    procedure Init;
    procedure LoadFromHtml(const AElement: IHtmlElement);
  public
    constructor Create(const AData: THCRichData);
    destructor Destroy; override;
    function Parse(const AHtmlText: string): Boolean;
  end;

function ParserHTML(const Source: WideString): IHtmlElement; stdcall;

implementation

uses
  SysUtils, HCCommon, HCItem; // , generics.Collections;
{$IF NOT Declared(TStringBuilder)}
{$DEFINE CUSTOM_STRINGBUILDER}
{$IFEND}
{$IF Declared(TArray)}
{$DEFINE USE_GENERICS}
{$IFEND}

const
  WhiteSpace = [' ', #13, #10, #9];
  // CSS Attribute Compare Operator
  OperatorChar = ['=', '!', '*', '~', '|', '^', '$'];
  MaxListSize = Maxint div 16;

  // TagProperty
  TpBlock = $01;
  TpInline = $02;
  TpEmpty = $04;
  TpFormatAsInline = $08;
  TpPreserveWhitespace = $10;

  tpInlineOrEmpty = TpInline or TpEmpty;

type
//{$IF NOT Declared(TStringDynArray)}
{$IFDEF USE_GENERICS}
  TStringDynArray = TArray<string>;
{$ELSE}
  TStringDynArray = array of string;
{$ENDIF}
//{$ENDIF}
  TPointerList = array [0 .. MaxListSize - 1] of Pointer;
  PPointerList = ^TPointerList;

  THtmlElement = class;
{$IFNDEF USE_GENERICS}

  { 为了不引入Classes单元,因为在高版本Delphi中Classes单元往往是导致体积膨胀最快的因素. }
  TList = class
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TList;
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
  end;

  THtmlElementList = TList;

  { 没有使用泛型单元的容器,是因为为了可以在低版本Delphi上编译 }
  TIHtmlElementList = class(TInterfacedObject, IHtmlElementList)
  private
    FList: TList;
    function GetItems(Index: Integer): IHtmlElement; stdcall;
    procedure SetItems(Index: Integer; const Value: IHtmlElement);

    function Add(Value: IHtmlElement): Integer;
    procedure Delete(Index: Integer);
    procedure Clear();
    function GetCount: Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(Item: IHtmlElement): Integer;
    property Items[index: Integer]: IHtmlElement read GetItems
      write SetItems; default;
    property Count: Integer read GetCount;
  end;

  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;

  THashItem = record
    Next: PHashItem;
    Key: string;
    Value: string;
  end;

  TStringDictionary = class
  private
    Buckets: array of PHashItem;
  protected
    function Find(const Key: string): PPHashItem;
    function HashOf(const Key: string): Cardinal; virtual;
  public
    constructor Create(Size: Cardinal = 512);
    destructor Destroy; override;
    procedure Add(const Key: string; Value: string);
    procedure AddOrSetValue(const Key: string; Value: string);
    procedure Clear;
    procedure Remove(const Key: string);
    function Modify(const Key: string; Value: string): Boolean;
    function ValueOf(Key: string): string;
    function ContainsKey(const Key: string): Boolean;
    function GetKeys(): TStringDynArray;

    property Values[Key: string]: string read ValueOf; default;
  end;
{$ELSE}

  TStringDictionary = TDictionary<string, string>;
  THtmlElementList = TList<THtmlElement>;
  TIHtmlElementList = TList<IHtmlElement>;
{$ENDIF}
{$IFDEF CUSTOM_STRINGBUILDER}

  { 低版本Delphi中没有TStringBuilder }
  TStringBuilder = class(TObject)
  private
    FBuffMax, FBuffSize, FIndex: Integer;
    FBuffer: array of Char;
    procedure ExpandBuffer(MinSize: Integer);

  public
    constructor Create(ABufferSize: Integer = 4096);
    destructor Destroy; override;
    procedure Append(const Value: string);
    procedure Clear;
    function ToString: string;
  end;
{$ENDIF}

  // CSS
  TAttrOperator = (aoExist, aoEqual, aoNotEqual, aoIncludeWord, aoBeginWord,
    aoBegin, aoEnd, aoContain);

  PAttrSelectorItem = ^TAttrSelectorItem;

  TAttrSelectorItem = record
    Key: string;
    AttrOperator: TAttrOperator;
    Value: string;
  end;

  TSelectorItemRelation = (sirNONE, sirDescendant, sirChildren,
    sirYoungerBrother, sirAllYoungerBrother);

  PCSSSelectorItem = ^TCSSSelectorItem;

  TCSSSelectorItem = record
    Relation: TSelectorItemRelation;
    szTag: string;
    Attributes: array of TAttrSelectorItem;
  end;

  TCSSSelectorItems = array of TCSSSelectorItem;
  PCSSSelectorItems = ^TCSSSelectorItems;
  TCSSSelectorItemGroup = array of TCSSSelectorItems;

  IHtmlElementObj = interface
    ['{04733AF3-E548-42B0-9B81-1CBF037E8D5E}']
    function HtmlElement(): THtmlElement;
  end;

  TAttributeItem = record
    Key, Value: string;
  end;

  TAttributeDynArray = array of TAttributeItem;
{$IF NOT Declared(TStringDictionary)}
  TStringDictionary = TDictionary<string, string>;
{$IFEND}
  TAttributes = TStringDictionary;

  THtmlElement = class(TInterfacedObject, IHtmlElement, IHtmlElementObj)
    FClosed: Boolean;

    //
    FOwner: THtmlElement;
    FCloseTag: IHtmlElement;
    FTagName: string;
    FIsCloseTag: Boolean;
    FContent: string;
    FOrignal: string;

    FAttributes: TAttributes;

    FChildren: TIHtmlElementList;
    FSourceLine: Integer;
    FSourceCol: Integer;

    constructor Create(AOwner: THtmlElement; AText: string;
      ALine, ACol: Integer);
    constructor CreateAsText(AOwner: THtmlElement; AText: string;
      ALine, ACol: Integer);
    constructor CreateAsScript(AOwner: THtmlElement; AText: string;
      ALine, ACol: Integer);
    constructor CreateAsStyle(AOwner: THtmlElement; AText: string;
      ALine, ACol: Integer);
    constructor CreateAsComment(AOwner: THtmlElement; AText: string;
      ALine, ACol: Integer);
    constructor CreateAsTag(AOwner: THtmlElement; AText: string;
      ALine, ACol: Integer);
    constructor CreateAsDocType(AOwner: THtmlElement; AText: string;
      ALine, ACol: Integer);
    destructor Destroy; override;
    procedure _GetHtml(IncludeSelf: Boolean; Sb: TStringBuilder);
    procedure _GetText(IncludeSelf: Boolean; Sb: TStringBuilder);
    function IsTagElement(): Boolean;
    function _FilterElement(const ATagName: WideString;
      const AAttributes: TAttributeDynArray): Boolean;
    procedure _FindElements(AList: TIHtmlElementList;
      const ATagName: WideString; const AAttributes: TAttributeDynArray;
      AOnlyInTopElement: Boolean);
    procedure _SimpleCSSSelector(const ItemGroup: TCSSSelectorItemGroup;
      r: TIHtmlElementList);
    procedure _Select(Item: PCSSSelectorItem; Count: Integer;
      r: TIHtmlElementList; OnlyTopLevel: Boolean = false);
  private
    function HtmlElement(): THtmlElement;
  public
    function GetOwner: IHtmlElement; stdcall;
    function GetTagName: WideString; safecall;
    function GetContent: WideString; safecall;
    function GetOrignal: WideString; safecall;
    function GetChildrenCount: Integer; stdcall;
    function GetChildren(Index: Integer): IHtmlElement; stdcall;
    function GetCloseTag: IHtmlElement; stdcall;
    function GetInnerHtml(): WideString; safecall;
    function GetOuterHtml(): WideString; safecall;
    function GetInnerText(): WideString; safecall;

    function GetAttributes(Key: WideString): WideString; safecall;

    function GetSourceLineNum(): Integer; stdcall;
    function GetSourceColNum(): Integer; stdcall;

    // 属性是否存在
    function HasAttribute(AttributeName: WideString): Boolean; stdcall;
    // 查找节点
    { FindElements('Link','type="application/rss+xml"')
      FindElements('','type="application/rss+xml"')
    }
    function FindElements(ATagName: WideString; AAttributes: WideString;
      AOnlyInTopElement: Boolean): IHtmlElementList; stdcall;
    function SimpleCSSSelector(const selector: WideString)
      : IHtmlElementList; stdcall;
    // 枚举属性
    procedure EnumAttributeNames(AParam: Pointer;
      ACallBack: TEnumAttributeNameCallBack); stdcall;

    property TagName: WideString read GetTagName;
    property ChildrenCount: Integer read GetChildrenCount;
    property Children[index: Integer]: IHtmlElement read GetChildren; default;
    property CloseTag: IHtmlElement read GetCloseTag;
    property Content: WideString read GetContent;
    property Orignal: WideString read GetOrignal;
    property Owner: IHtmlElement read GetOwner;
    // 获取元素在源代码中的位置
    property SourceLineNum: Integer read GetSourceLineNum;
    property SourceColNum: Integer read GetSourceColNum;
    //
    property InnerHtml: WideString read GetInnerHtml;
    property OuterHtml: WideString read GetOuterHtml;
    property InnerText: WideString read GetInnerText;

    property Attributes[Key: WideString]: WideString read GetAttributes;
  end;

  TFNCompareAttr = function(const Item: TAttrSelectorItem;
    E: THtmlElement): Boolean;

{$IFNDEF USE_GENERICS}
  { TList }

destructor TList.Destroy;
begin
  Clear;
end;

function TList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TList.Delete(Index: Integer);
//var
//  Temp: Pointer;
begin
  if (index < 0) or (index >= FCount) then
    Exit;
//  Temp := Items[index];
  Dec(FCount);
  if index < FCount then
    System.Move(FList^[index + 1], FList^[index],
      (FCount - index) * SizeOf(Pointer));
end;

procedure TList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Exit;
  if (Index2 < 0) or (Index2 >= FCount) then
    Exit;
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TList.Expand: TList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TList.First: Pointer;
begin
  Result := Get(0);
end;

function TList.Get(Index: Integer): Pointer;
begin
  if (index < 0) or (index >= FCount) then
    Exit;
  Result := FList^[index];
end;

procedure TList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TList.Insert(Index: Integer; Item: Pointer);
begin
  if (index < 0) or (index > FCount) then
    Exit;
  if FCount = FCapacity then
    Grow;
  if index < FCount then
    System.Move(FList^[index], FList^[index + 1],
      (FCount - index) * SizeOf(Pointer));
  FList^[index] := Item;
  Inc(FCount);
end;

function TList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Exit;
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure TList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if (index < 0) or (index >= FCount) then
    Exit;
  if Item <> FList^[index] then
  begin
    Temp := FList^[index];
    FList^[index] := Item;
  end;
end;

function TList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

procedure TList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Exit;
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));  // 重新申请内容
    FCapacity := NewCapacity;
  end;
end;

procedure TList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Exit;
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

function TList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList^[I] := nil;
    Delete(I);
  end;
end;

{ TStringDictionary }

procedure TStringDictionary.Add(const Key: string; Value: string);
var
  Hash: Integer;
  Bucket: PHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TStringDictionary.AddOrSetValue(const Key: string; Value: string);
begin
  if ContainsKey(Key) then
    Modify(Key, Value)
  else
    Add(Key, Value);
end;

procedure TStringDictionary.Clear;
var
  I: Integer;
  P, N: PHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

function TStringDictionary.ContainsKey(const Key: string): Boolean;
begin
  Result := Find(Key)^ <> nil;
end;

constructor TStringDictionary.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
end;

destructor TStringDictionary.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TStringDictionary.Find(const Key: string): PPHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TStringDictionary.GetKeys: TStringDynArray;
var
  I: Integer;
  Item: PHashItem;
  List: TList;
begin
  List := TList.Create;
  for I := low(Buckets) to high(Buckets) do
    if Buckets[I] <> nil then
    begin
      Item := Buckets[I];
      repeat
        List.Add(Item);
        Item := Item^.Next;
      until Item = nil;
    end;
  SetLength(Result, List.Count);
  for I := 0 to List.Count - 1 do
    Result[I] := PHashItem(List[I])^.Key;
  List.Free;
end;

function TStringDictionary.HashOf(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2)))
      xor Ord(Key[I]);
end;

function TStringDictionary.Modify(const Key: string; Value: string): Boolean;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := false;
end;

procedure TStringDictionary.Remove(const Key: string);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;

function TStringDictionary.ValueOf(Key: string): string;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := '';
end;

{ TIHtmlElementList }

function TIHtmlElementList.Add(Value: IHtmlElement): Integer;
begin
  if Value <> nil then
    Value._AddRef;
  Result := FList.Add(Pointer(Value));
end;

procedure TIHtmlElementList.Clear;
begin
  while Count > 0 do
    Delete(Count - 1);
end;

constructor TIHtmlElementList.Create;
begin
  FList := TList.Create;
end;

procedure TIHtmlElementList.Delete(Index: Integer);
var
  I: IHtmlElement;
begin
  I := IHtmlElement(FList[index]);
  (I as IHtmlElementObj).HtmlElement.FOwner := nil;
  if I <> nil then
    I._Release;
  FList.Delete(index);
end;

destructor TIHtmlElementList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TIHtmlElementList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TIHtmlElementList.GetItems(Index: Integer): IHtmlElement;
begin
  Result := IHtmlElement(FList[index]);
end;

function TIHtmlElementList.IndexOf(Item: IHtmlElement): Integer;
begin
  Result := FList.IndexOf(Pointer(Item));
end;

procedure TIHtmlElementList.SetItems(Index: Integer; const Value: IHtmlElement);
var
  I: IHtmlElement;
begin
  I := IHtmlElement(FList[index]);
  if I <> nil then
    I._Release;
  if Value <> nil then
    Value._AddRef;
  Items[index] := Value;
end;
{$ENDIF}
{$IFDEF CUSTOM_STRINGBUILDER}
{ TStringBuilder }

constructor TStringBuilder.Create(ABufferSize: Integer);
begin
  inherited Create;
  if ABufferSize = 0 then
    FBuffSize := 4096
  else
    FBuffSize := ABufferSize;
  SetLength(FBuffer, FBuffSize);
  FBuffMax := FBuffSize - 1;
  FIndex := 0;
end;

destructor TStringBuilder.Destroy;
begin
  FBuffer := nil;
  inherited Destroy;
end;

procedure TStringBuilder.ExpandBuffer(MinSize: Integer);
begin
  if FBuffSize < MinSize then
    FBuffSize := MinSize;
  FBuffSize := 2 * FBuffSize;
  FBuffMax := FBuffSize - 1;
  SetLength(FBuffer, FBuffSize);
end;

procedure TStringBuilder.Append(const Value: string);
var
  ILen: Integer;
begin
  ILen := Length(Value);
  if (ILen + FIndex) >= FBuffMax then
    ExpandBuffer(ILen + FIndex);
  Move(Value[1], FBuffer[FIndex], SizeOf(Char) * ILen);
  Inc(FIndex, ILen);
end;

function TStringBuilder.ToString: string;
begin
  FBuffer[FIndex] := #0;
  SetLength(Result, FIndex);
  Move(FBuffer[0], Result[1], SizeOf(Char) * FIndex);
end;

procedure TStringBuilder.Clear;
begin
  FIndex := 0;
end;

{$ENDIF}
//

procedure DoError(const Msg: string);
begin
  raise Exception.Create(Msg);

end;

{$IF NOT Declared(CharInSet)}

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
// inline;
begin
  Result := C in CharSet;
end;

{$IFEND}

function TrimStr(ACharSet: TSysCharSet; const Str: string): string;
var
  I, _begin, _End: Integer;
begin
  _begin := -1;
  _End := -1;
  for I := 1 to Length(Str) do
  begin
    if not CharInSet(Str[I], ACharSet) then
    begin
      if _begin < 0 then
        _begin := I;
      _End := I;
    end;
  end;
  if (_begin < 0) or (_End < 0) then
    Result := ''
  else
    Result := Copy(Str, _begin, _End - _begin + 1);

end;

function SplitStr(ACharSet: TSysCharSet; AStr: string): TStringDynArray;
var
  L, I: Integer;
  S: string;
  StrChar: Char;
begin
  Result := nil;
  if Length(AStr) <= 0 then
    Exit;

  I := 1;
  L := 1;
  StrChar := #0;
  while I <= Length(AStr) do
  begin
    if CharInSet(AStr[I], ['''', '"']) then
      if StrChar = #0 then
        StrChar := AStr[1]
      else if StrChar = AStr[1] then
        StrChar := #0;
    if StrChar = #0 then
      if CharInSet(AStr[I], ACharSet) then
      begin
        if I > L then
        begin
          S := Copy(AStr, L, I - L);
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := S;
        end;
        L := I + 1;
      end;
    Inc(I);
  end;
  if (I > L) then
  begin
    S := Copy(AStr, L, I - L);
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := S;
  end;
end;

procedure _SkipBlank(var P: PChar);
begin
  while CharInSet(P^, WhiteSpace) do
    Inc(P);
end;

function _ReadStr(var P: PChar; UntilChars: TSysCharSet): string;
var
  oldP: PChar;
  stringChar: Char;
begin
  _SkipBlank(P);
  oldP := P;
  if CharInSet(P^, ['"', '''']) then
    stringChar := P^
  else
    stringChar := #0;
  Inc(P);
  while True do
  begin
    if stringChar = #0 then
    begin
      if CharInSet(P^, UntilChars) then
        Break;
    end
    else if (P^ = stringChar) then
    begin
      Inc(P);
      Break;
    end;
    Inc(P);
  end;
  SetString(Result, oldP, P - oldP);
  if (stringChar <> #0) and (Length(Result) >= 2) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure _ParserAttrs(P: PChar; var Attrs: TAttributeDynArray);
var
  Key, Value: string;
begin
  SetLength(Attrs, 0);
  while True do
  begin
    _SkipBlank(P);
    if P^ = #0 then
      Break;
    Key := _ReadStr(P, (WhiteSpace + [#0, '=']));
    Value := '';
    _SkipBlank(P);
    if P^ = '=' then
    begin
      Inc(P);
      _SkipBlank(P);
      Value := _ReadStr(P, (WhiteSpace + [#0]));
    end;
    SetLength(Attrs, Length(Attrs) + 1);
    Attrs[Length(Attrs) - 1].Key := Key;
    Attrs[Length(Attrs) - 1].Value := Value;
  end;
end;

procedure _ParserNodeItem(S: string; var ATagName: string;
  var Attrs: TAttributeDynArray);

var
  P: PChar;

begin
  P := PChar(S);
  _SkipBlank(P);
  ATagName := UpperCase(_ReadStr(P, (WhiteSpace + [#0, '/', '>'])));

  _ParserAttrs(P, Attrs);
end;

// ComapreAttr

function _aoExist(const Item: TAttrSelectorItem; E: THtmlElement): Boolean;
begin
  Result := E.FAttributes.ContainsKey(Item.Key);
end;

function _aoEqual(const Item: TAttrSelectorItem; E: THtmlElement): Boolean;
begin
  Result := E.FAttributes.ContainsKey(Item.Key) and
    (E.FAttributes.ValueOf(Item.Key) = Item.Value);
end;

function _aoNotEqual(const Item: TAttrSelectorItem; E: THtmlElement): Boolean;
begin
  Result := E.FAttributes.ContainsKey(Item.Key) and
    (E.FAttributes.ValueOf(Item.Key) <> Item.Value);
end;

function _aoIncludeWord(const Item: TAttrSelectorItem; E: THtmlElement)
  : Boolean;
var
  S: TStringDynArray;
  I: Integer;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  Result := True;
  S := SplitStr(WhiteSpace, E.FAttributes.ValueOf(Item.Key));
  for I := Low(S) to High(S) do
    if S[I] = Item.Value then
      Exit;
  Result := false;
end;

function _aoBeginWord(const Item: TAttrSelectorItem; E: THtmlElement): Boolean;
var
  S: TStringDynArray;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  S := SplitStr((WhiteSpace + ['_', '-']), E.FAttributes.ValueOf(Item.Key));
  Result := (Length(S) > 0) and (S[0] = Item.Value);
end;

function _aoBegin(const Item: TAttrSelectorItem; E: THtmlElement): Boolean;
var
  attr, Value: string;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  attr := E.FAttributes.ValueOf(Item.Key);
  Value := Item.Value;
  Result := (Length(attr) > Length(Value)) and
    (Copy(attr, 1, Length(Value)) = Value);
end;

function _aoEnd(const Item: TAttrSelectorItem; E: THtmlElement): Boolean;
var
  attr, Value: string;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  attr := E.FAttributes.ValueOf(Item.Key);
  Value := Item.Value;
  Result := (Length(attr) > Length(Value)) and
    (Copy(attr, Length(attr) - Length(Value) + 1, Length(attr)) = Value);
end;

function _aoContain(const Item: TAttrSelectorItem; E: THtmlElement): Boolean;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  Result := Pos(Item.Value, E.FAttributes.ValueOf(Item.Key)) > 0;
end;

const
  AttrCompareFuns: array [TAttrOperator] of TFNCompareAttr = (_aoExist,
    _aoEqual, _aoNotEqual, _aoIncludeWord, _aoBeginWord, _aoBegin, _aoEnd,
    _aoContain);

function ConvertEntities(S: string): string; forward;
function ConvertWhiteSpace(S: String): string; forward;
function GetTagProperty(const TagName: string): Word; forward;
function ParserCSSSelector(const Value: string): TCSSSelectorItemGroup; forward;

procedure _ParserHTML(const Source: string; AElementList: THtmlElementList);
var
  BeginLineNum, LineNum, BeginColNum, ColNum: Integer;

  procedure IncSrc(var P: PChar); overload;
  begin
    if P^ = #10 then
    begin
      Inc(LineNum);
      ColNum := 1;
    end
    else
      Inc(ColNum);
    Inc(P);
  end;

  procedure IncSrc(var P: PChar; step: Integer); overload;
  var
    I: Integer;
  begin
    for I := 0 to step - 1 do
      IncSrc(P);
  end;

  procedure SkipBlank(var P: PChar);
  begin
    while CharInSet(P^, WhiteSpace) do
      IncSrc(P);
  end;

  function PeekStr(const P: PChar): string;
  var
    Lp: PChar;
  begin
    Result := '';
    Lp := P;
    while not CharInSet(Lp^, (WhiteSpace + ['/', '>'])) do
      Inc(Lp);
    SetString(Result, P, Lp - P);
  end;

  function IsEndOfTag(P: PChar; TagName: string): Boolean;
  begin
    Result := false;
    Inc(P);
    if P^ <> '/' then
      Exit;
    Inc(P);
    Result := UpperCase(PeekStr(P)) = UpperCase(TagName);
  end;

  function PosCharInTag(var P: PChar; AChar: Char): Boolean;
  var
    StrChar: Char;
  begin
    Result := false;
    StrChar := #0;
    while True do
    begin
      if P^ = #0 then
        Break;
      if P^ = '"' then
      begin
        if StrChar = #0 then
          StrChar := P^
        else
          StrChar := #0;
      end;

      if (P^ = AChar) and (StrChar = #0) then
      begin
        Result := True;
        Break;
      end;
      IncSrc(P);
    end;
  end;

  function ParserStyleData(var P: PChar): string;
  var
    oldP: PChar;
  begin
    oldP := P;
    if Copy(P, 1, 4) = '<!--' then
    begin
      IncSrc(P, 5);
      while True do
      begin
        if P^ = #0 then
          DoError(Format('未完结的Style行:%d;列:%d;', [LineNum, ColNum]))
        else if P^ = '>' then
        begin
          if ((P - 1)^ = '-') and ((P - 2)^ = '-') then
          begin
            IncSrc(P);
            SkipBlank(P);
            Break;
          end;
        end;
        IncSrc(P);
      end;
    end
    else
      while True do
      begin
        case P^ of
          #0:
            begin
              Break;
            end;
          '<':
            begin
              if IsEndOfTag(P, 'style') then
              begin
                Break;
              end;
            end;
        end;
        IncSrc(P);
      end;
    SetString(Result, oldP, P - oldP);
  end;

  function ParserScriptData(var P: PChar): string;
  var
    oldP: PChar;
    stringChar: Char;
    PreIsblique: Boolean;
  begin
    oldP := P;
    stringChar := #0;
    SkipBlank(P);
    if Copy(P, 1, 4) = '<!--' then
    begin
      IncSrc(P, 5);
      while True do
      begin
        if P^ = #0 then
          DoError(Format('未完结的Script行:%d;列:%d;', [LineNum, ColNum]))
        else if P^ = '>' then
        begin
          if ((P - 1)^ = '-') and ((P - 2)^ = '-') then
          begin
            IncSrc(P);
            SkipBlank(P);
            Break;
          end;
        end;
        IncSrc(P);
      end;
    end
    else
    begin
      while True do
      begin
        case P^ of
          #0:
            Break;
          '"', '''': // 字符串
            begin
              stringChar := P^;
              PreIsblique := false;
              IncSrc(P);
              while True do
              begin
                if P^ = #0 then
                  Break;
                if (P^ = stringChar) and (not PreIsblique) then
                  Break;
                if P^ = '\' then
                  PreIsblique := not PreIsblique
                else
                  PreIsblique := false;
                IncSrc(P);
              end;
            end;
          '/': // 注释
            begin
              IncSrc(P);
              case P^ of
                '/': // 行注释
                  begin
                    while True do
                    begin
                      if CharInSet(P^, [#0, #$0A]) then
                      begin
                        Break;
                      end;
                      IncSrc(P);
                    end;
                  end;
                '*': // 块注释
                  begin
                    IncSrc(P);
                    IncSrc(P);
                    while True do
                    begin
                      if P^ = #0 then
                        Break;
                      if (P^ = '/') and (P[-1] = '*') then
                      begin
                        Break;
                      end;
                      IncSrc(P);
                    end;
                  end;
              end;
            end;
          '<':
            begin
              if IsEndOfTag(P, 'script') then
              begin
                Break;
              end;
            end;
        end;
        IncSrc(P);
      end;
    end;
    SetString(Result, oldP, P - oldP);
  end;

var
  ElementType: (EtUnknow, EtTag, EtDocType, EtText, EtComment);
  Tag: THtmlElement;
  oldP, P: PChar;
  Tmp: string;
begin
  LineNum := 1;
  ColNum := 1;
  P := PChar(Source);
  while P^ <> #0 do
  begin
    ElementType := EtUnknow;
    oldP := P;
    BeginLineNum := LineNum;
    BeginColNum := ColNum;
    if P^ = '<' then
    begin
      IncSrc(P);
      if P^ = '!' then // 注释
      begin
        ElementType := EtComment;
        IncSrc(P);
        case P^ of
          '-': // <!--  -->
            begin
              IncSrc(P); // -
              while True do
              begin
                if not PosCharInTag(P, '>') then
                  DoError('LineNum:' + IntToStr(BeginLineNum) + '无法找到Tag结束点:' +
                    Copy(oldP, 1, 100))
                else if (P[-1] = '-') and (P[-2] = '-') then
                begin
                  IncSrc(P);
                  Break;
                end;
                IncSrc(P);
              end;
            end;
          '[': // <![CDATA[.....]]>
            begin
              IncSrc(P); //
              while True do
              begin
                if not PosCharInTag(P, '>') then
                  DoError('LineNum:' + IntToStr(BeginLineNum) + '无法找到Tag结束点:' +
                    Copy(oldP, 1, 100))
                else if (P[-1] = ']') then
                begin
                  IncSrc(P);
                  Break;
                end;
                IncSrc(P);
              end;
            end;
        else // <!.....>
          begin
            if UpperCase(PeekStr(P)) = 'DOCTYPE' then
            begin
              ElementType := EtDocType;
              IncSrc(P); //
              if not PosCharInTag(P, '>') then
                DoError('LineNum:' + IntToStr(BeginLineNum) + '无法找到Tag结束点:' +
                  Copy(oldP, 1, 100))
              else
                IncSrc(P);
            end
            else
            begin
              IncSrc(P); //
              if not PosCharInTag(P, '>') then
                DoError('LineNum:' + IntToStr(BeginLineNum) + '无法找到Tag结束点:' +
                  Copy(oldP, 1, 100))
              else
                IncSrc(P);
            end;
          end;
        end;

      end
      else if P^ = '?' then // <?...?>  XML
      begin
        ElementType := EtComment;
        IncSrc(P); //
        while True do
        begin
          if not PosCharInTag(P, '>') then
            DoError('LineNum:' + IntToStr(BeginLineNum) + '无法找到Tag结束点:' +
              Copy(oldP, 1, 100))
          else if (P[-1] = '?') then
          begin
            IncSrc(P);
            Break;
          end;
          IncSrc(P);
        end;
      end
      else // 正常节点
      begin
        ElementType := EtTag;
        IncSrc(P); //
        if not PosCharInTag(P, '>') then
          DoError('LineNum:' + IntToStr(BeginLineNum) + '无法找到Tag结束点:' +
            Copy(oldP, 1, 100))
        else
          IncSrc(P);
      end;
      SetString(Tmp, oldP, P - oldP);
    end
    else // 文字节点
    begin
      ElementType := EtText;
      while True do
      begin
        if CharInSet(P^, [#0, '<']) then
          Break;
        IncSrc(P);
      end;
      SetString(Tmp, oldP, P - oldP);
    end;

    case ElementType of
      EtUnknow:
        begin
          DoError('LineNum:' + IntToStr(BeginLineNum) + '无法解析的内容:' +
            Copy(oldP, 1, 100));
        end;
      EtDocType:
        begin
          Tag := THtmlElement.CreateAsDocType(nil, Tmp, BeginLineNum,
            BeginColNum);
          AElementList.Add(Tag);
        end;
      EtTag:
        begin
          // OutputDebugString(PChar(Format('LineNum:%d;%d;%s', [BeginLineNum,  BeginColNum, tmp])));
          Tag := THtmlElement.CreateAsTag(nil, Tmp, BeginLineNum, BeginColNum);
          AElementList.Add(Tag);
          //
          if (UpperCase(Tag.FTagName) = 'SCRIPT') and (not Tag.FIsCloseTag) and
            (not Tag.FClosed) then
          begin
            // 读取Script
            BeginLineNum := LineNum;
            BeginColNum := ColNum;
            Tmp := ParserScriptData(P);
            Tag := THtmlElement.CreateAsScript(nil, Tmp, BeginLineNum,
              BeginColNum);
            AElementList.Add(Tag);
          end
          else if (UpperCase(Tag.FTagName) = 'STYLE') and (not Tag.FIsCloseTag)
            and (not Tag.FClosed) then
          begin
            // 读取Style
            BeginLineNum := LineNum;
            BeginColNum := ColNum;
            Tmp := ParserStyleData(P);
            Tag := THtmlElement.CreateAsStyle(nil, Tmp, BeginLineNum,
              BeginColNum);
            AElementList.Add(Tag);
          end;
        end;
      EtText:
        begin
          Tag := THtmlElement.CreateAsText(nil, Tmp, BeginLineNum, BeginColNum);
          Tag.FSourceLine := BeginLineNum;
          Tag.FSourceCol := BeginColNum;
          AElementList.Add(Tag);
        end;
      EtComment:
        begin
          Tag := THtmlElement.CreateAsComment(nil, Tmp, BeginLineNum,
            BeginColNum);
          Tag.FSourceLine := BeginLineNum;
          Tag.FSourceCol := BeginColNum;
          AElementList.Add(Tag);
        end;
    end;

  end;
end;

function BuildTree(ElementList: THtmlElementList): THtmlElement;
var
  I, J: Integer;
  [Weak]
  E: THtmlElement;
  [Weak]
  T: THtmlElement;
  FoundIndex: Integer;
  TagProperty: Word;
begin
  Result := THtmlElement.Create(nil, '', 0, 0);
  Result.FTagName := '#DOCUMENT';
  Result.FClosed := false;
  ElementList.Insert(0, Result);

  I := 1;
  while I < ElementList.Count do
  begin
    E := ElementList[I];
    TagProperty := GetTagProperty(E.FTagName);

    // 空节点,往下找,如果下一个带Tag的节点不是它的关闭节点,那么自动关闭
    {
      if (e.IsTagElement) and (not e.FClosed) and
      ((TagProperty and (tpEmpty)) <> 0) then
      begin
      for J := (I + 1) to ElementList.Count - 1 do
      begin
      T := ElementList[J];
      if T.IsTagElement then
      begin
      if not((T.FTagName = e.FTagName) and (E.FIsCloseTag)) then
      E.FClosed := True;
      Break;
      end;
      end;
      end;
    }
    FoundIndex := -1;
    if E.FIsCloseTag then
    begin
      for J := (I - 1) downto 0 do
      begin
        T := ElementList[J];
        if (not T.FClosed) and (T.FTagName = E.FTagName) and (not T.FIsCloseTag)
        then
        begin
          FoundIndex := J;
          Break;
        end;
      end;
      // 如果往上找,找不到的话这个关闭Tag肯定是无意义的.
      if FoundIndex > 0 then
      begin
        for J := (I - 1) downto FoundIndex do
        begin
          T := ElementList[J];
          T.FClosed := True;
        end;
        THtmlElement(ElementList[FoundIndex]).FCloseTag := E;
      end
      else
      begin
        E.Free;
      end;
      ElementList.Delete(I);
      Continue;
    end
    else
    begin
      for J := (I - 1) downto 0 do
      begin
        T := ElementList[J];
        if not T.FClosed then
        begin
          if ((GetTagProperty(T.FTagName) and TpEmpty) <> 0) then
            T.FClosed := True
          else
          begin
            T.FChildren.Add(E);
            E.FOwner := T;
            Break;
          end;
        end;
      end;
    end;
    Inc(I);
  end;
  Result.FClosed := True;
end;

function ParserHTML(const Source: WideString): IHtmlElement; stdcall;
var
  ElementList: THtmlElementList;
begin
  ElementList := THtmlElementList.Create;
  _ParserHTML(Source, ElementList);
  Result := BuildTree(ElementList);
  ElementList.Free;
end;

{ THtmlElement }

constructor THtmlElement.Create(AOwner: THtmlElement; AText: string;
  ALine, ACol: Integer);
begin
  inherited Create();
  FSourceLine := ALine;
  FSourceCol := ACol;
  FChildren := TIHtmlElementList.Create;
  FOwner := AOwner;
  FOrignal := AText;
  FAttributes := TStringDictionary.Create(32);
end;

constructor THtmlElement.CreateAsComment(AOwner: THtmlElement; AText: string;
  ALine, ACol: Integer);
begin
  Create(AOwner, AText, ALine, ACol);
  FContent := ConvertEntities(AText);
  FTagName := '#COMMENT';
  FClosed := True;
end;

constructor THtmlElement.CreateAsDocType(AOwner: THtmlElement; AText: string;
  ALine, ACol: Integer);
begin
  Create(AOwner, AText, ALine, ACol);
  FContent := ConvertEntities(AText);
  FTagName := '#DOCTYPE';
  FClosed := True;
  if FContent = '' then
    Exit;
  if FContent[1] = '<' then
    Delete(FContent, 1, 1);
  if FContent = '' then
    Exit;
  if FContent[Length(FContent)] = '>' then
    Delete(FContent, Length(FContent), 1);
  FContent := Trim(Copy(Trim(FContent), 9, Length(FContent)));
end;

constructor THtmlElement.CreateAsScript(AOwner: THtmlElement; AText: string;
  ALine, ACol: Integer);
begin
  Create(AOwner, AText, ALine, ACol);
  FContent := ConvertEntities(AText);
  FTagName := '#SCRIPT';
  FClosed := True;
end;

constructor THtmlElement.CreateAsStyle(AOwner: THtmlElement; AText: string;
  ALine, ACol: Integer);
begin
  Create(AOwner, AText, ALine, ACol);
  FContent := ConvertEntities(AText);
  FTagName := '#STYLE';
  FClosed := True;
end;

constructor THtmlElement.CreateAsTag(AOwner: THtmlElement; AText: string;
  ALine, ACol: Integer);
var
  //Strs: TStringDynArray;
  I: Integer;
  Attrs: TAttributeDynArray;
begin

  Create(AOwner, AText, ALine, ACol);
  // TODO 解析TagName和属性
  if AText = '' then
    Exit;
  // 去掉两头的<
  if AText[1] = '<' then
    Delete(AText, 1, 1);

  if AText = '' then
    Exit;
  if AText[Length(AText)] = '>' then
    Delete(AText, Length(AText), 1);
  // 检查是关闭节点,还是单个已经关闭的节点
  if AText = '' then
    Exit;
  FClosed := AText[Length(AText)] = '/';
  FIsCloseTag := AText[1] = '/';

  if FIsCloseTag then
    Delete(AText, 1, 1);
  if FClosed then
    Delete(AText, Length(AText), 1);
  //
  _ParserNodeItem(AText, FTagName, Attrs);
  for I := Low(Attrs) to High(Attrs) do
    FAttributes.AddOrSetValue(LowerCase(Attrs[I].Key),
      ConvertEntities(Attrs[I].Value));
end;

constructor THtmlElement.CreateAsText(AOwner: THtmlElement; AText: string;
  ALine, ACol: Integer);
begin
  Create(AOwner, AText, ALine, ACol);
  FContent := ConvertEntities(AText);
  FTagName := '#TEXT';
  FClosed := True;
end;

destructor THtmlElement.Destroy;
begin
  FChildren.Free;
  FAttributes.Free;
  inherited Destroy;
end;

procedure THtmlElement.EnumAttributeNames(AParam: Pointer;
  ACallBack: TEnumAttributeNameCallBack);
var
  I: Integer;
  Keys: TStringDynArray;
begin
  if not Assigned(ACallBack) then
    Exit;
{$IFDEF USE_GENERICS}
  Keys := FAttributes.Keys.ToArray;
{$ELSE}
  Keys := FAttributes.GetKeys;
{$ENDIF}
  /// Keys := FAttributes.Keys.ToArray;
  for I := low(Keys) to high(Keys) do
    if not ACallBack(AParam, Keys[I], FAttributes[Keys[I]]) then
      Break;
end;

function THtmlElement._FilterElement(const ATagName: WideString;
  const AAttributes: TAttributeDynArray): Boolean;
var
  I: Integer;
begin
  Result := false;
  if (Length(ATagName) = 0) or (SameText(FTagName, ATagName)) then
  begin
    for I := Low(AAttributes) to High(AAttributes) do
      if (not FAttributes.ContainsKey(LowerCase(AAttributes[I].Key))) or
        (AAttributes[I].Value <> FAttributes[LowerCase(AAttributes[I].Key)])
      then
        Exit;
    Result := True;
  end;
end;

procedure THtmlElement._FindElements(AList: TIHtmlElementList;
  const ATagName: WideString; const AAttributes: TAttributeDynArray;
  AOnlyInTopElement: Boolean);
var
  I: Integer;
  C: IHtmlElement;
  E: THtmlElement;
begin
  for I := 0 to FChildren.Count - 1 do
  begin
    C := Children[I];
    E := (C as IHtmlElementObj).HtmlElement;

    if E._FilterElement(ATagName, AAttributes) then
    begin
      AList.Add(E);
    end;
  end;
  if not AOnlyInTopElement then
  begin
    for I := 0 to FChildren.Count - 1 do
    begin
      E := (FChildren[I] as IHtmlElementObj).HtmlElement;
      E._FindElements(AList, ATagName, AAttributes, false);
    end;
  end;
end;

function THtmlElement.FindElements(ATagName: WideString;
  AAttributes: WideString; AOnlyInTopElement: Boolean): IHtmlElementList;
var
  LAttributes: TAttributeDynArray;
  List: TIHtmlElementList;
  P: PChar;
  Attrs: string;
begin
  Attrs := AAttributes;
  P := PChar(Attrs);
  _ParserAttrs(P, LAttributes);
  List := TIHtmlElementList.Create;
  _FindElements(List, ATagName, LAttributes, AOnlyInTopElement);
  Result := List as IHtmlElementList;
end;

function THtmlElement.GetAttributes(Key: WideString): WideString;
begin
  Result := FAttributes[LowerCase(Key)];
end;

function THtmlElement.GetChildren(Index: Integer): IHtmlElement;
begin
  Result := FChildren[index];
end;

function THtmlElement.GetChildrenCount: Integer;
begin
  Result := FChildren.Count;
end;

function THtmlElement.GetCloseTag: IHtmlElement;
begin
  Result := FCloseTag;
end;

function THtmlElement.GetContent: WideString;
begin
  Result := FContent;
end;

function THtmlElement.GetOrignal: WideString;
begin
  Result := FOrignal;
end;

function THtmlElement.GetOuterHtml: WideString;
var
  Sb: TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  _GetHtml(True, Sb);
  Result := Sb.ToString;
  Sb.Free;
end;

function THtmlElement.GetOwner: IHtmlElement;
begin
  Result := FOwner;
end;

function THtmlElement.GetSourceColNum: Integer;
begin
  Result := FSourceCol;
end;

function THtmlElement.GetSourceLineNum: Integer;
begin
  Result := FSourceLine;
end;

function THtmlElement.GetTagName: WideString;
begin
  Result := FTagName;
end;

procedure THtmlElement._GetHtml(IncludeSelf: Boolean; Sb: TStringBuilder);
var
  I: Integer;
  E: THtmlElement;
begin
  if IncludeSelf then
    Sb.Append(FOrignal);

  for I := 0 to FChildren.Count - 1 do
  begin
    E := (FChildren[I] as IHtmlElementObj).HtmlElement;
    E._GetHtml(True, Sb);
  end;
  if IncludeSelf and (FCloseTag <> nil) then
    (FCloseTag as IHtmlElementObj).HtmlElement._GetHtml(True, Sb);
end;

procedure THtmlElement._GetText(IncludeSelf: Boolean; Sb: TStringBuilder);
var
  I: Integer;
  E: THtmlElement;
begin
  if IncludeSelf and (FTagName = '#TEXT') then
  begin
    Sb.Append(FContent);
  end;

  for I := 0 to FChildren.Count - 1 do
  begin
    E := (FChildren[I] as IHtmlElementObj).HtmlElement;
    E._GetText(True, Sb);
  end;
end;

procedure THtmlElement._Select(Item: PCSSSelectorItem; Count: Integer;
  r: TIHtmlElementList; OnlyTopLevel: Boolean);

  function _Filtered(): Boolean;
  var
    I: Integer;
  begin
    Result := false;
    if (Item^.szTag = '') or (Item^.szTag = '*') or (Item^.szTag = FTagName)
    then
    begin
      for I := Low(Item^.Attributes) to High(Item^.Attributes) do
        if not AttrCompareFuns[Item^.Attributes[I].AttrOperator]
          (Item^.Attributes[I], Self) then
          Exit;
      Result := True;
    end;
  end;

var
  f: Boolean;
  I, SelfIndex: Integer;
  PE, E: THtmlElement;
  Next: PCSSSelectorItem;
begin
  f := _Filtered();
  if f then
  begin
    if (Count = 1) then
    begin
      if (r.IndexOf(Self as IHtmlElement) < 0) then
        r.Add(Self as IHtmlElement);
    end
    else if Count > 1 then
    begin
      Next := Item;
      Inc(Next);
      PE := Self.FOwner;
      if PE = nil then
        SelfIndex := -1
      else
        SelfIndex := PE.FChildren.IndexOf(Self as IHtmlElement);

      case Next^.Relation of
        sirDescendant, sirChildren:
          begin
            for I := 0 to FChildren.Count - 1 do
            begin
              E := (FChildren[I] as IHtmlElementObj).HtmlElement;
              E._Select(Next, Count - 1, r, Next^.Relation = sirChildren);
            end;
          end;
        sirAllYoungerBrother, sirYoungerBrother:
          begin
            if (PE <> nil) and (SelfIndex >= 0) then
              for I := (SelfIndex + 1) to PE.FChildren.Count - 1 do
              begin
                E := (PE.FChildren[I] as IHtmlElementObj).HtmlElement;
                if (Length(E.FTagName) = 0) or (E.FTagName[1] <> '#') then
                begin
                  E._Select(Next, Count - 1, r, True);
                  if (Next^.Relation = sirYoungerBrother) then
                    Break;
                end;
              end;
          end;
      end;
    end;
  end;
  if not OnlyTopLevel then
    for I := 0 to FChildren.Count - 1 do
    begin
      E := (FChildren[I] as IHtmlElementObj).HtmlElement;
      E._Select(Item, Count, r);
    end;
end;

procedure THtmlElement._SimpleCSSSelector(const ItemGroup
  : TCSSSelectorItemGroup; r: TIHtmlElementList);
var
  I: Integer;
begin
  for I := Low(ItemGroup) to High(ItemGroup) do
  begin
    _Select(@ItemGroup[I][0], Length(ItemGroup[I]), r);
  end;
end;

function THtmlElement.GetInnerHtml: WideString;
var
  Sb: TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  _GetHtml(false, Sb);
  Result := Sb.ToString;
  Sb.Free;
end;

function THtmlElement.GetInnerText: WideString;
var
  Sb: TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  _GetText(True, Sb);
  Result := Sb.ToString;
  Sb.Free;
end;

function THtmlElement.HasAttribute(AttributeName: WideString): Boolean;
begin
  Result := FAttributes.ContainsKey(LowerCase(AttributeName));
end;

function THtmlElement.HtmlElement: THtmlElement;
begin
  Result := Self;
end;

function THtmlElement.IsTagElement: Boolean;
begin
  Result := (Length(FTagName) > 0) and (FTagName[1] <> '#');
end;

function THtmlElement.SimpleCSSSelector(const selector: WideString)
  : IHtmlElementList;
var
  r: TIHtmlElementList;
begin
  r := TIHtmlElementList.Create;
  _SimpleCSSSelector(ParserCSSSelector(selector), r);
  Result := r as IHtmlElementList;
end;

//
function ParserCSSSelector(const Value: string): TCSSSelectorItemGroup;
var
  LineNum, ColNum: Integer;
  procedure IncSrc(var P: PChar);
  begin
    if P^ = #10 then
    begin
      Inc(LineNum);
      ColNum := 1;
    end
    else
      Inc(ColNum);
    Inc(P);
  end;
  procedure SkipBlank(var P: PChar);
  begin
    while CharInSet(P^, WhiteSpace) do
      IncSrc(P);
  end;

  function ReadStr(var P: PChar; Chars: TSysCharSet): string;
  var
    oldP: PChar;
  begin
    oldP := P;
    while not(CharInSet(P^, Chars)) do
      IncSrc(P);
    SetString(Result, oldP, P - oldP);
  end;

  function AddAttr(var Item: TCSSSelectorItem): PAttrSelectorItem;
  begin
    SetLength(Item.Attributes, Length(Item.Attributes) + 1);
    Result := @Item.Attributes[Length(Item.Attributes) - 1];
  end;

  function ParserAttr(var P: PChar): TAttrSelectorItem;
  var
    oldP: PChar;
    Tmp: string;
    stringChar: Char;
  begin
    IncSrc(P); // [
    Result.Key := '';
    Result.AttrOperator := aoEqual;
    Result.Value := '';
    // Key
    SkipBlank(P);
    oldP := P;
    while not CharInSet(P^, (WhiteSpace + OperatorChar + [']', #0])) do
      IncSrc(P);
    SetString(Result.Key, oldP, P - oldP);
    Result.Key := LowerCase(Result.Key);
    // Operator
    SkipBlank(P);
    oldP := P;
    case P^ of
      '=', '!', '*', '~', '|', '^', '$':
        begin
          IncSrc(P);
          if P^ = '=' then
            IncSrc(P);
        end;
      ']':
        begin
          Result.AttrOperator := aoExist;
          IncSrc(P);
          Exit;
        end;
    else
      begin
        DoError(Format('无法解析CSS Attribute操作符[%d,%d]', [LineNum, ColNum]));
      end;
    end;
    SetString(Tmp, oldP, P - oldP);

    if Length(Tmp) >= 1 then
    begin
      case Tmp[1] of
        '=':
          Result.AttrOperator := aoEqual;
        '!':
          Result.AttrOperator := aoNotEqual;
        '*':
          Result.AttrOperator := aoContain;
        '~':
          Result.AttrOperator := aoIncludeWord;
        '|':
          Result.AttrOperator := aoBeginWord;
        '^':
          Result.AttrOperator := aoBegin;
        '$':
          Result.AttrOperator := aoEnd;
      end;
    end;

    // Value
    SkipBlank(P);
    oldP := P;
    if CharInSet(P^, ['"', '''']) then
      stringChar := P^
    else
      stringChar := #0;
    IncSrc(P);
    while True do
    begin
      if stringChar = #0 then
      begin
        if CharInSet(P^, (WhiteSpace + [#0, ']'])) then
          Break;
      end
      else if (P^ = stringChar) then
      begin
        IncSrc(P);
        Break;
      end;
      IncSrc(P);
    end;
    SetString(Result.Value, oldP, P - oldP);
    if (stringChar <> #0) and (Length(Result.Value) >= 2) then
      Result.Value := Copy(Result.Value, 2, Length(Result.Value) - 2);
    Result.Value := ConvertEntities(Result.Value);
    //
    SkipBlank(P);
    if P^ = ']' then
      IncSrc(P)
    else
      DoError(Format('无法解析Attribute值[%d,%d]', [LineNum, ColNum]));

  end;

  procedure ParserItem(var P: PChar; var Item: TCSSSelectorItem);
  var
    pAttr: PAttrSelectorItem;
  begin
    SkipBlank(P);
    while True do
    begin
      case P^ of
        #0, ',', ' ':
          Break;
        '.': // class
          begin
            IncSrc(P);
            pAttr := AddAttr(Item);
            pAttr^.Key := 'class';
            pAttr^.AttrOperator := aoIncludeWord;
            pAttr^.Value :=
              ReadStr(P, (WhiteSpace + OperatorChar + ['[', ']', '"', '''', ',',
              '.', '#', #0]));
          end;
        '#': // id
          begin
            IncSrc(P);
            pAttr := AddAttr(Item);
            pAttr^.Key := 'id';
            pAttr^.AttrOperator := aoEqual;
            pAttr^.Value :=
              ReadStr(P, (WhiteSpace + OperatorChar + ['[', ']', '"', '''', ',',
              '.', '#', #0]));
          end;
        '[': // attribute
          begin
            pAttr := AddAttr(Item);
            pAttr^ := ParserAttr(P);
          end;
        '/':
          begin
            IncSrc(P);
            if P^ = '*' then // /**/
            begin
              IncSrc(P);
              IncSrc(P);
              while True do
              begin
                if (P^ = '/') and ((P - 1)^ = '*') then
                begin
                  IncSrc(P);
                  Break;
                end;
                IncSrc(P);
              end;
            end;
          end;
      else
        begin
          Item.szTag :=
            UpperCase(ReadStr(P, (WhiteSpace + ['[', ']', '"', '''', ',', '.',
            '#', #0])));
        end;
      end;
    end;
  end;

  function AddItems(var Group: TCSSSelectorItemGroup): PCSSSelectorItems;
  begin
    SetLength(Group, Length(Group) + 1);
    Result := @Group[Length(Group) - 1];
  end;

  function AddItem(var Items: TCSSSelectorItems): PCSSSelectorItem;
  begin
    SetLength(Items, Length(Items) + 1);
    Result := @Items[Length(Items) - 1];
    Result^.Relation := sirNONE;
  end;

var
  P: PChar;
  pitems: PCSSSelectorItems;
  pItem: PCSSSelectorItem;
begin
  P := PChar(Value);
  LineNum := 1;
  ColNum := 1;
  //
  pitems := AddItems(Result);
  pItem := AddItem(pitems^);
  while True do
  begin
    SkipBlank(P);
    ParserItem(P, pItem^);
    SkipBlank(P);
    case P^ of
      ',':
        begin
          IncSrc(P);
          pitems := AddItems(Result);
          pItem := AddItem(pitems^);
        end;
      '>':
        begin
          IncSrc(P);
          pItem := AddItem(pitems^);
          pItem^.Relation := sirChildren;
        end;
      '+':
        begin
          IncSrc(P);
          pItem := AddItem(pitems^);
          pItem^.Relation := sirYoungerBrother;
        end;
      '~':
        begin
          IncSrc(P);
          pItem := AddItem(pitems^);
          pItem^.Relation := sirAllYoungerBrother;
        end;
      #0:
        Break;
    else
      begin
        pItem := AddItem(pitems^);
        pItem^.Relation := sirDescendant;
      end;
    end;

  end;
end;

var
  GEntities: TStringDictionary;

type
  TEntityItem = record
    Key: string;
    Value: WideChar;
  end;

const
  EntityTable: array [0 .. 252 - 1] of TEntityItem = ((Key: '&nbsp;';
    Value: WideChar(160)), (Key: '&iexcl;'; Value: WideChar(161)),
    (Key: '&cent;'; Value: WideChar(162)), (Key: '&pound;';
    Value: WideChar(163)), (Key: '&curren;'; Value: WideChar(164)),
    (Key: '&yen;'; Value: WideChar(165)), (Key: '&brvbar;';
    Value: WideChar(166)), (Key: '&sect;'; Value: WideChar(167)), (Key: '&uml;';
    Value: WideChar(168)), (Key: '&copy;'; Value: WideChar(169)),
    (Key: '&ordf;'; Value: WideChar(170)), (Key: '&laquo;';
    Value: WideChar(171)), (Key: '&not;'; Value: WideChar(172)), (Key: '&shy;';
    Value: WideChar(173)), (Key: '&reg;'; Value: WideChar(174)), (Key: '&macr;';
    Value: WideChar(175)), (Key: '&deg;'; Value: WideChar(176)),
    (Key: '&plusmn;'; Value: WideChar(177)), (Key: '&sup2;';
    Value: WideChar(178)), (Key: '&sup3;'; Value: WideChar(179)),
    (Key: '&acute;'; Value: WideChar(180)), (Key: '&micro;';
    Value: WideChar(181)), (Key: '&para;'; Value: WideChar(182)),
    (Key: '&middot;'; Value: WideChar(183)), (Key: '&cedil;';
    Value: WideChar(184)), (Key: '&sup1;'; Value: WideChar(185)),
    (Key: '&ordm;'; Value: WideChar(186)), (Key: '&raquo;';
    Value: WideChar(187)), (Key: '&frac14;'; Value: WideChar(188)),
    (Key: '&frac12;'; Value: WideChar(189)), (Key: '&frac34;';
    Value: WideChar(190)), (Key: '&iquest;'; Value: WideChar(191)),
    (Key: '&Agrave;'; Value: WideChar(192)), (Key: '&Aacute;';
    Value: WideChar(193)), (Key: '&Acirc;'; Value: WideChar(194)),
    (Key: '&Atilde;'; Value: WideChar(195)), (Key: '&Auml;';
    Value: WideChar(196)), (Key: '&Aring;'; Value: WideChar(197)),
    (Key: '&AElig;'; Value: WideChar(198)), (Key: '&Ccedil;';
    Value: WideChar(199)), (Key: '&Egrave;'; Value: WideChar(200)),
    (Key: '&Eacute;'; Value: WideChar(201)), (Key: '&Ecirc;';
    Value: WideChar(202)), (Key: '&Euml;'; Value: WideChar(203)),
    (Key: '&Igrave;'; Value: WideChar(204)), (Key: '&Iacute;';
    Value: WideChar(205)), (Key: '&Icirc;'; Value: WideChar(206)),
    (Key: '&Iuml;'; Value: WideChar(207)), (Key: '&ETH;'; Value: WideChar(208)),
    (Key: '&Ntilde;'; Value: WideChar(209)), (Key: '&Ograve;';
    Value: WideChar(210)), (Key: '&Oacute;'; Value: WideChar(211)),
    (Key: '&Ocirc;'; Value: WideChar(212)), (Key: '&Otilde;';
    Value: WideChar(213)), (Key: '&Ouml;'; Value: WideChar(214)),
    (Key: '&times;'; Value: WideChar(215)), (Key: '&Oslash;';
    Value: WideChar(216)), (Key: '&Ugrave;'; Value: WideChar(217)),
    (Key: '&Uacute;'; Value: WideChar(218)), (Key: '&Ucirc;';
    Value: WideChar(219)), (Key: '&Uuml;'; Value: WideChar(220)),
    (Key: '&Yacute;'; Value: WideChar(221)), (Key: '&THORN;';
    Value: WideChar(222)), (Key: '&szlig;'; Value: WideChar(223)),
    (Key: '&agrave;'; Value: WideChar(224)), (Key: '&aacute;';
    Value: WideChar(225)), (Key: '&acirc;'; Value: WideChar(226)),
    (Key: '&atilde;'; Value: WideChar(227)), (Key: '&auml;';
    Value: WideChar(228)), (Key: '&aring;'; Value: WideChar(229)),
    (Key: '&aelig;'; Value: WideChar(230)), (Key: '&ccedil;';
    Value: WideChar(231)), (Key: '&egrave;'; Value: WideChar(232)),
    (Key: '&eacute;'; Value: WideChar(233)), (Key: '&ecirc;';
    Value: WideChar(234)), (Key: '&euml;'; Value: WideChar(235)),
    (Key: '&igrave;'; Value: WideChar(236)), (Key: '&iacute;';
    Value: WideChar(237)), (Key: '&icirc;'; Value: WideChar(238)),
    (Key: '&iuml;'; Value: WideChar(239)), (Key: '&eth;'; Value: WideChar(240)),
    (Key: '&ntilde;'; Value: WideChar(241)), (Key: '&ograve;';
    Value: WideChar(242)), (Key: '&oacute;'; Value: WideChar(243)),
    (Key: '&ocirc;'; Value: WideChar(244)), (Key: '&otilde;';
    Value: WideChar(245)), (Key: '&ouml;'; Value: WideChar(246)),
    (Key: '&divide;'; Value: WideChar(247)), (Key: '&oslash;';
    Value: WideChar(248)), (Key: '&ugrave;'; Value: WideChar(249)),
    (Key: '&uacute;'; Value: WideChar(250)), (Key: '&ucirc;';
    Value: WideChar(251)), (Key: '&uuml;'; Value: WideChar(252)),
    (Key: '&yacute;'; Value: WideChar(253)), (Key: '&thorn;';
    Value: WideChar(254)), (Key: '&yuml;'; Value: WideChar(255)),
    (Key: '&fnof;'; Value: WideChar(402)), (Key: '&Alpha;';
    Value: WideChar(913)), (Key: '&Beta;'; Value: WideChar(914)),
    (Key: '&Gamma;'; Value: WideChar(915)), (Key: '&Delta;';
    Value: WideChar(916)), (Key: '&Epsilon;'; Value: WideChar(917)),
    (Key: '&Zeta;'; Value: WideChar(918)), (Key: '&Eta;'; Value: WideChar(919)),
    (Key: '&Theta;'; Value: WideChar(920)), (Key: '&Iota;';
    Value: WideChar(921)), (Key: '&Kappa;'; Value: WideChar(922)),
    (Key: '&Lambda;'; Value: WideChar(923)), (Key: '&Mu;';
    Value: WideChar(924)), (Key: '&Nu;'; Value: WideChar(925)), (Key: '&Xi;';
    Value: WideChar(926)), (Key: '&Omicron;'; Value: WideChar(927)),
    (Key: '&Pi;'; Value: WideChar(928)), (Key: '&Rho;'; Value: WideChar(929)),
    (Key: '&Sigma;'; Value: WideChar(931)), (Key: '&Tau;';
    Value: WideChar(932)), (Key: '&Upsilon;'; Value: WideChar(933)),
    (Key: '&Phi;'; Value: WideChar(934)), (Key: '&Chi;'; Value: WideChar(935)),
    (Key: '&Psi;'; Value: WideChar(936)), (Key: '&Omega;';
    Value: WideChar(937)), (Key: '&alpha;'; Value: WideChar(945)),
    (Key: '&beta;'; Value: WideChar(946)), (Key: '&gamma;';
    Value: WideChar(947)), (Key: '&delta;'; Value: WideChar(948)),
    (Key: '&epsilon;'; Value: WideChar(949)), (Key: '&zeta;';
    Value: WideChar(950)), (Key: '&eta;'; Value: WideChar(951)),
    (Key: '&theta;'; Value: WideChar(952)), (Key: '&iota;';
    Value: WideChar(953)), (Key: '&kappa;'; Value: WideChar(954)),
    (Key: '&lambda;'; Value: WideChar(955)), (Key: '&mu;';
    Value: WideChar(956)), (Key: '&nu;'; Value: WideChar(957)), (Key: '&xi;';
    Value: WideChar(958)), (Key: '&omicron;'; Value: WideChar(959)),
    (Key: '&pi;'; Value: WideChar(960)), (Key: '&rho;'; Value: WideChar(961)),
    (Key: '&sigmaf;'; Value: WideChar(962)), (Key: '&sigma;';
    Value: WideChar(963)), (Key: '&tau;'; Value: WideChar(964)),
    (Key: '&upsilon;'; Value: WideChar(965)), (Key: '&phi;';
    Value: WideChar(966)), (Key: '&chi;'; Value: WideChar(967)), (Key: '&psi;';
    Value: WideChar(968)), (Key: '&omega;'; Value: WideChar(969)),
    (Key: '&thetasym;'; Value: WideChar(977)), (Key: '&upsih;';
    Value: WideChar(978)), (Key: '&piv;'; Value: WideChar(982)), (Key: '&bull;';
    Value: WideChar(8226)), (Key: '&hellip;'; Value: WideChar(8230)),
    (Key: '&prime;'; Value: WideChar(8242)), (Key: '&Prime;';
    Value: WideChar(8243)), (Key: '&oline;'; Value: WideChar(8254)),
    (Key: '&frasl;'; Value: WideChar(8260)), (Key: '&weierp;';
    Value: WideChar(8472)), (Key: '&image;'; Value: WideChar(8465)),
    (Key: '&real;'; Value: WideChar(8476)), (Key: '&trade;';
    Value: WideChar(8482)), (Key: '&alefsym;'; Value: WideChar(8501)),
    (Key: '&larr;'; Value: WideChar(8592)), (Key: '&uarr;';
    Value: WideChar(8593)), (Key: '&rarr;'; Value: WideChar(8594)),
    (Key: '&darr;'; Value: WideChar(8595)), (Key: '&harr;';
    Value: WideChar(8596)), (Key: '&crarr;'; Value: WideChar(8629)),
    (Key: '&lArr;'; Value: WideChar(8656)), (Key: '&uArr;';
    Value: WideChar(8657)), (Key: '&rArr;'; Value: WideChar(8658)),
    (Key: '&dArr;'; Value: WideChar(8659)), (Key: '&hArr;';
    Value: WideChar(8660)), (Key: '&forall;'; Value: WideChar(8704)),
    (Key: '&part;'; Value: WideChar(8706)), (Key: '&exist;';
    Value: WideChar(8707)), (Key: '&empty;'; Value: WideChar(8709)),
    (Key: '&nabla;'; Value: WideChar(8711)), (Key: '&isin;';
    Value: WideChar(8712)), (Key: '&notin;'; Value: WideChar(8713)),
    (Key: '&ni;'; Value: WideChar(8715)), (Key: '&prod;';
    Value: WideChar(8719)), (Key: '&sum;'; Value: WideChar(8721)),
    (Key: '&minus;'; Value: WideChar(8722)), (Key: '&lowast;';
    Value: WideChar(8727)), (Key: '&radic;'; Value: WideChar(8730)),
    (Key: '&prop;'; Value: WideChar(8733)), (Key: '&infin;';
    Value: WideChar(8734)), (Key: '&ang;'; Value: WideChar(8736)),
    (Key: '&and;'; Value: WideChar(8743)), (Key: '&or;'; Value: WideChar(8744)),
    (Key: '&cap;'; Value: WideChar(8745)), (Key: '&cup;';
    Value: WideChar(8746)), (Key: '&int;'; Value: WideChar(8747)),
    (Key: '&there4;'; Value: WideChar(8756)), (Key: '&sim;';
    Value: WideChar(8764)), (Key: '&cong;'; Value: WideChar(8773)),
    (Key: '&asymp;'; Value: WideChar(8776)), (Key: '&ne;';
    Value: WideChar(8800)), (Key: '&equiv;'; Value: WideChar(8801)),
    (Key: '&le;'; Value: WideChar(8804)), (Key: '&ge;'; Value: WideChar(8805)),
    (Key: '&sub;'; Value: WideChar(8834)), (Key: '&sup;';
    Value: WideChar(8835)), (Key: '&nsub;'; Value: WideChar(8836)),
    (Key: '&sube;'; Value: WideChar(8838)), (Key: '&supe;';
    Value: WideChar(8839)), (Key: '&oplus;'; Value: WideChar(8853)),
    (Key: '&otimes;'; Value: WideChar(8855)), (Key: '&perp;';
    Value: WideChar(8869)), (Key: '&sdot;'; Value: WideChar(8901)),
    (Key: '&lceil;'; Value: WideChar(8968)), (Key: '&rceil;';
    Value: WideChar(8969)), (Key: '&lfloor;'; Value: WideChar(8970)),
    (Key: '&rfloor;'; Value: WideChar(8971)), (Key: '&lang;';
    Value: WideChar(9001)), (Key: '&rang;'; Value: WideChar(9002)),
    (Key: '&loz;'; Value: WideChar(9674)), (Key: '&spades;';
    Value: WideChar(9824)), (Key: '&clubs;'; Value: WideChar(9827)),
    (Key: '&hearts;'; Value: WideChar(9829)), (Key: '&diams;';
    Value: WideChar(9830)), (Key: '&quot;'; Value: WideChar(34)), (Key: '&amp;';
    Value: WideChar(38)), (Key: '&lt;'; Value: WideChar(60)), (Key: '&gt;';
    Value: WideChar(62)), (Key: '&OElig;'; Value: WideChar(338)),
    (Key: '&oelig;'; Value: WideChar(339)), (Key: '&Scaron;';
    Value: WideChar(352)), (Key: '&scaron;'; Value: WideChar(353)),
    (Key: '&Yuml;'; Value: WideChar(376)), (Key: '&circ;';
    Value: WideChar(710)), (Key: '&tilde;'; Value: WideChar(732)),
    (Key: '&ensp;'; Value: WideChar(8194)), (Key: '&emsp;';
    Value: WideChar(8195)), (Key: '&thinsp;'; Value: WideChar(8201)),
    (Key: '&zwnj;'; Value: WideChar(8204)), (Key: '&zwj;';
    Value: WideChar(8205)), (Key: '&lrm;'; Value: WideChar(8206)),
    (Key: '&rlm;'; Value: WideChar(8207)), (Key: '&ndash;';
    Value: WideChar(8211)), (Key: '&mdash;'; Value: WideChar(8212)),
    (Key: '&lsquo;'; Value: WideChar(8216)), (Key: '&rsquo;';
    Value: WideChar(8217)), (Key: '&sbquo;'; Value: WideChar(8218)),
    (Key: '&ldquo;'; Value: WideChar(8220)), (Key: '&rdquo;';
    Value: WideChar(8221)), (Key: '&bdquo;'; Value: WideChar(8222)),
    (Key: '&dagger;'; Value: WideChar(8224)), (Key: '&Dagger;';
    Value: WideChar(8225)), (Key: '&permil;'; Value: WideChar(8240)),
    (Key: '&lsaquo;'; Value: WideChar(8249)), (Key: '&rsaquo;';
    Value: WideChar(8250)), (Key: '&euro;'; Value: WideChar(8364)));

function ConvertEntities(S: String): string;
var
  I: Integer;
  T: String;
  r: String;
begin
  I := 1;
  while I < Length(S) do
  begin
    if S[I] = '&' then
    begin
      T := Copy(S, I, 1000);
      T := LowerCase(Copy(T, 1, Pos(';', T)));
      Delete(S, I, Length(T));
      if (Length(T) > 2) and (T[2] = '#') then
        if (Length(T) > 3) and (T[3] = 'x') then
          r := WideChar(StrToIntDef('$' + Copy(T, 4, Length(T) - 4), 33))
        else
          r := WideChar(StrToIntDef(Copy(T, 3, Length(T) - 3), 0))
      else
      begin
        r := '';
        if GEntities.ContainsKey(T) then
          r := GEntities[T]
        else
          r := T;
      end;
      Insert(r, S, I);
    end;
    Inc(I);
  end;
  Result := S;
end;

function ConvertWhiteSpace(S: String): string;
var
  Sb: TStringBuilder;
  I: Integer;
  PreIssWhite, ThisIsWhite: Boolean;
begin
  Sb := TStringBuilder.Create;
  PreIssWhite := false;
  for I := 1 to Length(S) do
  begin
    ThisIsWhite := CharInSet(S[I], WhiteSpace);
    if ThisIsWhite then
    begin
      if not PreIssWhite then
        Sb.Append(S[I]);
      PreIssWhite := True;
    end
    else
    begin
      Sb.Append(S[I]);
      PreIssWhite := false;
    end;
  end;
  Result := Sb.ToString;
  Sb.Free;
end;

const
  BlockTags: array [0 .. 59 - 1] of string = ('HTML', 'HEAD', 'BODY',
    'FRAMESET', 'SCRIPT', 'NOSCRIPT', 'STYLE', 'META', 'LINK', 'TITLE', 'FRAME',
    'NOFRAMES', 'SECTION', 'NAV', 'ASIDE', 'HGROUP', 'HEADER', 'FOOTER', 'P',
    'H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'UL', 'OL', 'PRE', 'DIV', 'BLOCKQUOTE',
    'HR', 'ADDRESS', 'FIGURE', 'FIGCAPTION', 'FORM', 'FIELDSET', 'INS', 'DEL',
    'S', 'DL', 'DT', 'DD', 'LI', 'TABLE', 'CAPTION', 'THEAD', 'TFOOT', 'TBODY',
    'COLGROUP', 'COL', 'TR', 'TH', 'TD', 'VIDEO', 'AUDIO', 'CANVAS', 'DETAILS',
    'MENU', 'PLAINTEXT');
  InlineTags: array [0 .. 56 - 1] of string = ('OBJECT', 'BASE', 'FONT', 'TT',
    'I', 'B', 'U', 'BIG', 'SMALL', 'EM', 'STRONG', 'DFN', 'CODE', 'SAMP', 'KBD',
    'VAR', 'CITE', 'ABBR', 'TIME', 'ACRONYM', 'MARK', 'RUBY', 'RT', 'RP', 'A',
    'IMG', 'BR', 'WBR', 'MAP', 'Q', 'SUB', 'SUP', 'BDO', 'IFRAME', 'EMBED',
    'SPAN', 'INPUT', 'SELECT', 'TEXTAREA', 'LABEL', 'BUTTON', 'OPTGROUP',
    'OPTION', 'LEGEND', 'DATALIST', 'KEYGEN', 'OUTPUT', 'PROGRESS', 'METER',
    'AREA', 'PARAM', 'SOURCE', 'TRACK', 'SUMMARY', 'COMMAND', 'DEVICE');
  EmptyTags: array [0 .. 14 - 1] of string = ('META', 'LINK', 'BASE', 'FRAME',
    'IMG', 'BR', 'WBR', 'EMBED', 'HR', 'INPUT', 'KEYGEN', 'COL', 'COMMAND',
    'DEVICE');
  FormatAsInlineTags: array [0 .. 19 - 1] of string = ('TITLE', 'A', 'P', 'H1',
    'H2', 'H3', 'H4', 'H5', 'H6', 'PRE', 'ADDRESS', 'LI', 'TH', 'TD', 'SCRIPT',
    'STYLE', 'INS', 'DEL', 'S');
  PreserveWhitespaceTags: array [0 .. 4 - 1] of string = ('PRE', 'PLAINTEXT',
    'TITLE', 'TEXTAREA');

var
  GTagProperty: TStringDictionary;

function GetTagProperty(const TagName: string): Word;
var
  Key, S: string;
begin
  Result := 0;
  Key := UpperCase(TagName);
  if GTagProperty.ContainsKey(Key) then
    S := GTagProperty[UpperCase(TagName)]
  else
    Exit;
  if Length(S) <= 0 then
    Result := 0
  else
    Result := Ord(S[1]);
end;

procedure Init();
var
  I: Integer;
  Key, S: string;
begin
  GEntities := TStringDictionary.Create();
  for I := low(EntityTable) to high(EntityTable) do
  begin
    GEntities.Add(EntityTable[I].Key, EntityTable[I].Value);
  end;
  //
  GTagProperty := TStringDictionary.Create;
  for I := low(BlockTags) to high(BlockTags) do
    GTagProperty.AddOrSetValue(BlockTags[I], WideChar(TpBlock));
  for I := low(InlineTags) to high(InlineTags) do
    GTagProperty.AddOrSetValue(InlineTags[I], WideChar(TpInline));

  for I := low(EmptyTags) to high(EmptyTags) do
  begin
    Key := EmptyTags[I];
    if GTagProperty.ContainsKey(Key) then
      S := GTagProperty[Key]
    else
      S := #0;
    S[1] := Char(Ord(S[1]) or TpEmpty);
    GTagProperty.AddOrSetValue(Key, S);
  end;

  for I := low(FormatAsInlineTags) to high(FormatAsInlineTags) do
  begin
    Key := FormatAsInlineTags[I];
    if GTagProperty.ContainsKey(Key) then
      S := GTagProperty[Key]
    else
      S := #0;
    S[1] := Char(Ord(S[1]) or TpFormatAsInline);
    GTagProperty.AddOrSetValue(Key[I], S);
  end;
  for I := low(PreserveWhitespaceTags) to high(PreserveWhitespaceTags) do
  begin
    Key := PreserveWhitespaceTags[I];
    if GTagProperty.ContainsKey(Key) then
      S := GTagProperty[Key]
    else
      S := #0;
    S[1] := Char(Ord(S[1]) or TpPreserveWhitespace);
    GTagProperty.AddOrSetValue(PreserveWhitespaceTags[I], S);
  end;
end;

procedure UnInit();
begin
  GTagProperty.Free;
  GEntities.Free;
end;

{ THCHtmlFormat }

constructor THCHtmlFormat.Create(const AData: THCRichData);
begin
  FData := AData;
end;

destructor THCHtmlFormat.Destroy;
begin
  inherited Destroy;
end;

procedure THCHtmlFormat.Init;
begin
  FVersion := '';
  FStartHTML := '';
  FEndHTML := '';
  FStartFragment := '';
  FEndFragment := '';
  FStartSelection := '';
  FEndSelection := '';
  FSourceURL := '';
end;

procedure GetColorFrom(const AColor: string; var R, G, B: Byte);
var
  i, vPos: Integer;
  vSeq: Byte;
  //vColor: string;
begin
  R := 0;
  G := 0;
  B := 0;
  vSeq := 0;

  if AColor[1] = 'r' then  // rgb颜色
  begin
    //vPos := Pos('(', AColor) + 1;
    //vColor := Copy(AColor, vPos, Pos(')', AColor) - vPos);
    vPos := 1;
    for i := 1 to Length(AColor) do
    begin
      if AColor[i] = '(' then
        vPos := i + 1
      else
      if AColor[i] = ' ' then
        vPos := i + 1
      else
      if (AColor[i] = ',') or (AColor[i] = ')') then
      begin
        if vSeq = 0 then
          R := StrToInt(Trim(Copy(AColor, vPos, i - vPos)))
        else
        if vSeq = 1 then
          G := StrToInt(Trim(Copy(AColor, vPos, i - vPos)))
        else
        if vSeq = 2 then
          B := StrToInt(Trim(Copy(AColor, vPos, i - vPos)));

        vPos := i + 1;
        Inc(vSeq);
      end;
    end;
  end
  else
  if AColor[1] = '#' then  // 16进制颜色
  begin

  end;
end;

procedure THCHtmlFormat.LoadFromHtml(const AElement: IHtmlElement);
var
  vStyleDict: TStringDictionary;

  procedure LoadStyle(const AStyleName, AStyleClass, AStyle: string);
  var
    vPSeek, vPStart, vPm: PChar;
    vKey, vValue: string;
    vMh: Boolean;
    vAttrDict: TStringDictionary;
    vTextStyle: THCTextStyle;
    vR, vG, vB: Byte;
  begin
    vAttrDict := TStringDictionary.Create(32);
    try
      vMh := False;
      vPStart := PChar(AStyle);
      vPSeek := vPStart;
      while vPSeek <> #0 do
      begin
        if vPSeek^ = ';' then
        begin
          if vMh then
          begin
            SetString(vKey, vPStart, vPm - vPStart);
            SetString(vValue, vPm + 1, vPSeek - vPm - 1);
            vAttrDict.Add(Trim(vKey), Trim(vValue));
          end;

          Inc(vPSeek);
          if vPSeek^ = #0 then
            Break;
          vPStart := vPSeek;
          vMh := False;
        end
        else
        if vPSeek^ = ':' then
        begin
          vPm := vPSeek;
          vMh := True;
        end;

        Inc(vPSeek);
      end;

      vTextStyle := THCTextStyle.Create;
      try
        vValue := vAttrDict.ValueOf('font-size');
        if vValue <> '' then

        else  // 空使用默认
        begin
          if AStyleName = 'H1' then
            vTextStyle.Size := GetFontSize('小一')
          else
            vTextStyle.Size := 10;
        end;

        vValue := vAttrDict.ValueOf('color');
        if vValue <> '' then
        begin
          GetColorFrom(vValue, vR, vG, vB);
          vTextStyle.Color := vB shl 16 + vG shl 8 + vR;;
        end;

        vValue := vAttrDict.ValueOf('background-color');
        if vValue <> '' then
        begin
          GetColorFrom(vValue, vR, vG, vB);
          vTextStyle.BackColor := vB shl 16 + vG shl 8 + vR;;
        end;

        vValue := vAttrDict.ValueOf('font-family');
        if vValue <> '' then
          vTextStyle.Family := AnsiDequotedStr(vValue, '"');// vValue.DeQuotedString

        vValue := LowerCase(vAttrDict.ValueOf('font-style'));
        if vValue = 'italic' then
          vTextStyle.FontStyles := vTextStyle.FontStyles + [tsItalic];

        vValue := vAttrDict.ValueOf('font-weight');
        if vValue = 'bold' then
          vTextStyle.FontStyles := vTextStyle.FontStyles + [tsBold];

        vValue := vAttrDict.ValueOf('vertical-align');
        if vValue = 'super' then
          vTextStyle.FontStyles := vTextStyle.FontStyles + [tsSuperscript]
        else
        if vValue = 'super' then
          vTextStyle.FontStyles := vTextStyle.FontStyles + [tsSubscript];

        vStyleDict.Add(AStyleName + AStyleClass, IntToStr(FData.Style.GetStyleNo(vTextStyle, True)));
      finally
        FreeAndNil(vTextStyle);
      end;
    finally
      FreeAndNil(vAttrDict);
    end;
  end;

  procedure LoadStyleTextElement(const AStyleName, AStyleClass, AText: string);
  var
    vStyleNo: Integer;
    vItem: THCCustomItem;
  begin
    vStyleNo := StrToInt(vStyleDict.ValueOf(AStyleName + AStyleClass));
    vItem := FData.CreateDefaultTextItem;
    vItem.Text := AText;
    vItem.StyleNo := vStyleNo;
    vItem.ParaFirst := True;
    FData.InsertItem(vItem);
  end;

var
  i: Integer;
  vTag, vBodyTag: IHtmlElement;
  vElements: IHtmlElementList;
  vStyleClass: string;
begin
  vElements := AElement.FindElements('Html', '', True);
  if vElements.Count > 0 then
  begin
    vElements := vElements[0].FindElements('Body', '', True);
    if vElements.Count > 0 then
    begin
      vBodyTag := vElements[0];

      vStyleDict := TStringDictionary.Create;
      try
        for i := 0 to vBodyTag.ChildrenCount - 1 do
        begin
          vTag := vBodyTag.Children[i];
          if (vTag.TagName = 'H1') or (vTag.TagName = 'P') then
          begin
            vStyleClass := vTag.Attributes['class'];
            if not vStyleDict.ContainsKey(vTag.TagName + vStyleClass) then  // 没有获取过此样式对应的StyleNo
              LoadStyle(vTag.TagName, vStyleClass, vTag.Attributes['style']);
              //vStyles := vBodyTag.SimpleCSSSelector(vTag.TagName);

            LoadStyleTextElement(vTag.TagName, vStyleClass, vTag.InnerText);
          end;
        end;
      finally
        FreeAndNil(vStyleDict);
      end;
    end;
  end;
end;

function THCHtmlFormat.Parse(const AHtmlText: string): Boolean;

  procedure ParseFormatHeader(const AKey, AValue: string);
  begin
    if AKey = 'Version' then
      FVersion := AValue
    else
    if AKey = 'StartHTML' then
      FStartHTML := AValue
    else
    if AKey = 'EndHTML' then
      FEndHTML := AValue
    else
    if AKey = 'StartFragment' then
      FStartFragment := AValue
    else
    if AKey = 'EndFragment' then
      FEndFragment := AValue
    else
    if AKey = 'StartSelection' then
      FStartSelection := AValue
    else
    if AKey = 'EndSelection' then
      FEndSelection := AValue
    else
    if AKey = 'SourceURL' then
      FSourceURL := AValue;
  end;

var
  vKey, vValue: string;
  vPStart, vPSeek: PChar;
  vHtmlElement: IHtmlElement;
begin
  Result := False;

  Self.Init;
  vKey := '';

  vPStart := PChar(AHtmlText);
  vPSeek := vPStart;
  while vPSeek^ <> #0 do   // parse format header
  begin
    if vPSeek^ = #13 then
    begin
      SetString(vValue, vPStart, vPSeek - vPStart);
      Inc(vPSeek);
      if vPSeek^ = #10 then
        Inc(vPSeek);
      vPStart := vPSeek;

      ParseFormatHeader(vKey, vValue);
      vKey := '';

      Continue;
    end
    else
    if vPSeek^ = ':' then
    begin
      if vKey = '' then
      begin
        SetString(vKey, vPStart, vPSeek - vPStart);
        Inc(vPSeek);
        vPStart := vPSeek;
      end;
    end
    else
    if vPSeek^ = '<' then
    begin
      if FVersion = '0.9' then
      begin
        vPStart := vPSeek;
        while vPSeek^ <> #0 do
          Inc(vPseek);

        SetString(vValue, vPStart, vPSeek - vPStart);

        vHtmlElement := ParserHTML(vValue);

        LoadFromHtml(vHtmlElement);

        Result := True;
      end;

      Break;
    end;

    Inc(vPSeek);
  end;
end;

initialization
  Init;

finalization
  UnInit;

end.
