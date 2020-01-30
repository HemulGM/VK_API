// author Marat Shaymardanov, Tomsk 2001, 2013
//
// You can freely use this code in any project
// if sending any postcards with postage stamp to my address:
// Frunze 131/1, 56, Russia, Tomsk, 634021
//
// The buffer for strings.
// The main purpose of the rapid format of long string.
// Features:
// 1. Minimize the handling of the memory manager.
// 2. One-time allocation of the specified size
// 3. The class is not multi-thread safe

unit StrBuffer;

interface

uses
  Classes,
  SysUtils;

const
  MaxBuffSize = Maxint div 16;
  SBufferIndexError = 'Buffer index out of bounds (%d)';
  SBufferCapacityError = 'Buffer capacity out of bounds (%d)';
  SBufferCountError = 'Buffer count out of bounds (%d)';

type

  {
    If you do not have enough space in the string than
    is taken a piece of memory twice the size
    and copies the data in this chunk of memory
  }
  TStrBuffer = class
  private
    FCount: Integer;
    FCapacity: Integer;
    FBuff: PAnsiChar;
  protected
    class procedure Error(const Msg: string; Data: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure Clear;
    procedure Add(const aValue: AnsiString); overload;
    procedure SetCapacity(NewCapacity: Integer);
    function GetText: AnsiString;
    function GetCount: Integer;
  end;

  PSegment = ^TSegment;

  TSegment = record
    Next: PSegment;
    Size: Integer;
    Count: Integer;
    Data: array [0 .. 0] of AnsiChar;
  end;

  { add memory done by segments }
  TSegmentBuffer = class
  private
    FCount: Integer;
    FCapacity: Integer;
    FFirst: PSegment;
    FLast: PSegment;
  protected
    class procedure Error(const Msg: string; Data: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure Clear;
    procedure AddSegment(aSize: Integer);
    procedure Add(const aValue: AnsiChar); overload;
    procedure Add(const aValue: AnsiString); overload;
    procedure Add(const aValue: PAnsiChar; aCnt: Integer); overload;
    function GetText: AnsiString;
    function GetCount: Integer;
    property Text: AnsiString read GetText;
  end;

implementation

{$RANGECHECKS OFF}
{ TStrBuffer }

class procedure TStrBuffer.Error(const Msg: string; Data: Integer);

{ function ReturnAddr: Pointer;  // this is not cross-platform
  asm
  MOV     EAX,[EBP+4]
  end; }

begin
  raise EListError.CreateFmt(Msg, [Data]) { at ReturnAddr };
end;

constructor TStrBuffer.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 0;
  FBuff := nil;
end;

destructor TStrBuffer.Destroy;
begin
  Clear;
  inherited;
end;

procedure TStrBuffer.Clear;
begin
  FCount := 0;
  SetCapacity(0);
end;

procedure TStrBuffer.Add(const aValue: AnsiString);
var
  cnt, delta: Integer;
begin
  cnt := Length(aValue);
  if FCount + cnt > FCapacity then
    begin
      delta := FCapacity div 2;
      if delta < cnt then
        delta := cnt * 2;
      SetCapacity(FCapacity + delta);
    end;
  System.Move(Pointer(aValue)^, PAnsiChar(FBuff + FCount)^, cnt);
  Inc(FCount, cnt);
end;

function TStrBuffer.GetCount: Integer;
begin
  result := FCount;
end;

function TStrBuffer.GetText: AnsiString;
begin
  SetLength(result, FCount);
  System.Move(FBuff^, Pointer(result)^, FCount);
end;

procedure TStrBuffer.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxBuffSize) then
    Error(SBufferCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
    begin
      ReallocMem(FBuff, NewCapacity);
      FCapacity := NewCapacity;
    end;
end;

procedure TStrBuffer.SaveToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FBuff, FCount);
end;

procedure TStrBuffer.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TStrBuffer.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TStrBuffer.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  s: AnsiString;
begin
  Clear;
  Size := Stream.Size - Stream.Position;
  SetString(s, nil, Size);
  Stream.Read(Pointer(s)^, Size);
  Add(s);
end;

{ TSegmentBuffer }

class procedure TSegmentBuffer.Error(const Msg: string; Data: Integer);

  {function ReturnAddr: Pointer;
  asm
    MOV     EAX,[EBP+4]
  end;}

  begin
    raise EListError.CreateFmt(Msg, [Data]);// at ReturnAddr;
  end;

constructor TSegmentBuffer.Create;
begin
  inherited Create;
  FCount := 0;
  FFirst := AllocMem(4096 + SizeOf(TSegment));
  with FFirst^ do
    begin
      Next := nil;
      Size := 4096;
      Count := 0;
    end;
  FLast := FFirst;
end;

destructor TSegmentBuffer.Destroy;
begin
  Clear;
  FreeMem(FFirst, FFirst^.Size);
  inherited Destroy;
end;

procedure TSegmentBuffer.Clear;
var
  p1, p2: PSegment;
begin
  p1 := FFirst;
  while p1 <> FLast do
    begin
      p2 := p1;
      p1 := p1^.Next;
      FreeMem(p2, p2^.Size);
    end;
  FFirst := FLast;
  FFirst^.Count := 0;
  FCount := 0;
end;

procedure TSegmentBuffer.AddSegment(aSize: Integer);
var
  segment: PSegment;
begin
  segment := AllocMem(aSize + SizeOf(TSegment) - SizeOf(AnsiChar));
  with segment^ do
    begin
      Next := nil;
      Size := aSize;
      Count := 0;
    end;
  FLast^.Next := segment;
  FLast := segment;
  Inc(FCapacity, aSize);
end;

function TSegmentBuffer.GetCount: Integer;
begin
  result := FCount;
end;

function TSegmentBuffer.GetText: AnsiString;
var
  p: PAnsiChar;
  segment: PSegment;
  len: Integer;
begin
  SetString(result, nil, FCount);
  p := Pointer(result);
  segment := FFirst;
  while segment <> nil do
    begin
      len := segment^.Count;
      System.Move(segment^.Data, p^, len);
      Inc(p, len);
      segment := segment^.Next;
    end;
end;

procedure TSegmentBuffer.Add(const aValue: PAnsiChar; aCnt: Integer);
var
  p: PAnsiChar;
  tmp: Integer;
begin
  p := aValue;
  // define size of unused memory in current buffer segment
  tmp := FLast^.Size - FLast^.Count;
  // if you do not have enough space in the buffer then copy the "unused" bytes
  // and reduce current segment
  if aCnt > tmp then
    begin
      System.Move(p^, FLast^.Data[FLast^.Count], tmp);
      Inc(FLast^.Count, tmp);
      Inc(FCount, tmp);
      Inc(p, tmp);
      Dec(aCnt, tmp);
      // add another segment of the larger buffer size
      tmp := FLast^.Size;
      if tmp < aCnt then
        tmp := aCnt;
      AddSegment(tmp * 2);
    end;
  if aCnt > 0 then
    begin
      Move(p^, FLast^.Data[FLast^.Count], aCnt);
      Inc(FCount, aCnt);
      Inc(FLast^.Count, aCnt);
    end;
end;

procedure TSegmentBuffer.Add(const aValue: AnsiString);
var
  len: Integer;
begin
  len := Length(aValue);
  if len > 0 then
    Add(@aValue[1], len);
end;

procedure TSegmentBuffer.Add(const aValue: AnsiChar);
begin
  Add(@aValue, 1);
end;

procedure TSegmentBuffer.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSegmentBuffer.SaveToStream(Stream: TStream);
var
  segment: PSegment;
begin
  segment := FFirst;
  while segment <> nil do
    begin
      Stream.WriteBuffer(segment^.Data[0], segment^.Count);
      segment := segment^.Next;
    end;
end;

procedure TSegmentBuffer.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSegmentBuffer.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  s: AnsiString;
begin
  Clear;
  Size := Stream.Size - Stream.Position;
  SetString(s, nil, Size);
  Stream.Read(Pointer(s)^, Size);
  Add(s);
end;

{$RANGECHECKS ON}

end.
