unit VKParams.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm14 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    MemoIn: TMemo;
    ButtonParse: TButton;
    MemoOut: TMemo;
    EditName: TEdit;
    MemoTypes: TMemo;
    procedure ButtonParseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FList: TStringList;
    FListParams: TStringList;
    procedure ParseParam(const Text: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form14: TForm14;

implementation

{$R *.dfm}

procedure TForm14.FormCreate(Sender: TObject);
begin
  FList := TStringList.Create;
  FListParams := TStringList.Create;
end;

procedure TForm14.FormDestroy(Sender: TObject);
begin
  FList.Free;
  FListParams.Free;
end;

procedure TForm14.ParseParam(const Text: string);
var
  Method: string;
  ValueType: string;
  i: Integer;
begin
  ValueType := 'Integer';
  if Pos('text', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('name', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('title', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('desc', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('guid', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('message', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('comment', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('link', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('query', LowerCase(Text)) <> 0 then
    ValueType := 'string';
  if Pos('extended', LowerCase(Text)) <> 0 then
    ValueType := 'Boolean';
  if Text[Text.Length] = 's' then
    ValueType := 'TIds';
  if Text.PadRight(2) = 'ed' then
    ValueType := 'Boolean';
  if Pos('id', LowerCase(Text)) <> 0 then
    ValueType := 'Integer';
  if Pos('privacy', LowerCase(Text)) <> 0 then
    ValueType := 'TArrayOfString';
  ///
  Method := Text;
  Method[1] := UpperCase(Method[1])[1];
  for i := 1 to Method.Length do
  begin
    if Method[i] = '_' then
      Method[i + 1] := UpperCase(Method[i + 1])[1];
  end;
  Method := Method.Replace('_', '');
  FList.Add('function ' + EditName.Text + '.' + Method + '(Value: ' + ValueType + '): Integer;');
  FListParams.Add(Text);
  MemoOut.Lines.Add('  function ' + Method + '(Value: ' + ValueType + '): Integer;');
end;

procedure TForm14.ButtonParseClick(Sender: TObject);
var
  i: Integer;
begin
  FList.Clear;
  FListParams.Clear;
  MemoOut.Lines.Clear;
  MemoOut.Lines.Add(EditName.Text + ' = record');
  MemoOut.Lines.Add('  List: TParams;');
  for i := 0 to MemoIn.Lines.Count - 1 do
  begin
    if not MemoIn.Lines[i].IsEmpty then
      ParseParam(MemoIn.Lines[i].Trim);
  end;
  MemoOut.Lines.Add('end;');

  MemoOut.Lines.Add('');
  MemoOut.Lines.Add('');
  MemoOut.Lines.Add('{ ' + EditName.Text + ' }');
  for i := 0 to FList.Count - 1 do
  begin
    MemoOut.Lines.Add('');
    MemoOut.Lines.Add(FList[i]);
    MemoOut.Lines.Add('begin');
    MemoOut.Lines.Add('  Result := List.Add(''' + FListParams[i] + ''', Value);');
    MemoOut.Lines.Add('end;');
  end;
end;

end.

