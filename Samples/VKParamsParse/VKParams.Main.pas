unit VKParams.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm14 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    MemoIn: TMemo;
    MemoOut: TMemo;
    EditName: TEdit;
    MemoTypes: TMemo;
    MemoTypesOut: TMemo;
    EditType: TEdit;
    Panel1: TPanel;
    ButtonParse: TButton;
    Panel2: TPanel;
    Button1: TButton;
    procedure ButtonParseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FList: TStringList;
    FListParams: TStringList;
    procedure ParseParam(const Text, RecordType: string);
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

procedure TForm14.ParseParam(const Text, RecordType: string);
var
  Method, LowText: string;
  ValueType: string;
  i: Integer;
begin
  LowText := Text.ToLower;
  ValueType := 'Int64';
  if LowText.Contains('text') then
    ValueType := 'string';
  if LowText.Contains('name') then
    ValueType := 'string';
  if LowText.Contains('title') then
    ValueType := 'string';
  if LowText.Contains('desc') then
    ValueType := 'string';
  if LowText.Contains('guid') then
    ValueType := 'string';
  if LowText.Contains('message') then
    ValueType := 'string';
  if LowText.Contains('comment') then
    ValueType := 'string';
  if LowText.Contains('link') then
    ValueType := 'string';
  if LowText.Contains('query') then
    ValueType := 'string';
  if LowText.Contains('extended') then
    ValueType := 'Boolean';
  if LowText.EndsWith('s') then
    ValueType := 'TIds';
  if LowText.PadRight(2) = 'ed' then
    ValueType := 'Boolean';
  if LowText.Contains('id') then
    ValueType := 'Int64';
  if LowText.Contains('privacy') then
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
  FList.Add('function ' + RecordType + '.' + Method + '(Value: ' + ValueType + '): ' + RecordType + ';');
  FListParams.Add(Text);
  MemoOut.Lines.Add('  function ' + Method + '(Value: ' + ValueType + '): ' + RecordType + ';');
end;

procedure TForm14.Button1Click(Sender: TObject);
var
  List: TStringList;
  Items, Item, ArrName, ArrItems: string;
  HelperName, IntercName: string;
begin
  MemoTypesOut.Lines.Clear;
  List := TStringList.Create;
  try
    List.Delimiter := ',';
    List.DelimitedText := StringReplace(MemoTypes.Text, '"', '', [rfReplaceAll]);
    Items := '';
    ArrItems := '';

    for var ListItem in List do
    begin
      Item := ListItem;
      Item[1] := string(Item[1]).ToUpper[1];
      for var i := 1 to Item.Length do
        if (Item[i] = '_') and (i < Item.Length) then
          Item[i + 1] := string(Item[i + 1]).ToUpper[1];
      Item := StringReplace(Item, '_', '', [rfReplaceAll]);

      Items := Items + Item + ', ';
      ArrItems := ArrItems + ListItem.QuotedString + ', ';
    end;
  finally
    List.Free;
  end;
  Items := Items.Trim([',', ' ']);
  Items := EditType.Text + ' = (' + Items + ');';

  ArrItems := ArrItems.Trim([',', ' ']);
  ArrName := EditType.Text;
  ArrName := ArrName.Remove(0, 1);
  ArrItems := ArrName + ': array[' + EditType.Text + '] of string = (' + ArrItems + ');';
  HelperName := EditType.Text + 'Helper';

  MemoTypesOut.Lines.Add(Items);
  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add(HelperName + ' = record helper for ' + EditType.Text);
  MemoTypesOut.Lines.Add('  function ToString: string; inline;');
  MemoTypesOut.Lines.Add('  class function Create(const Value: string): ' + EditType.Text + '; static;');
  MemoTypesOut.Lines.Add('end;');

  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add(ArrItems);

  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add('{ ' + HelperName + ' }');
  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add('class function ' + HelperName + '.Create(const Value: string): ' + EditType.Text + ';');
  MemoTypesOut.Lines.Add('begin');
  MemoTypesOut.Lines.Add('  Result := ' + EditType.Text + '(IndexStr(Value, ' + ArrName + '));');
  MemoTypesOut.Lines.Add('end;');

  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add('function ' + HelperName + '.ToString: string;');
  MemoTypesOut.Lines.Add('begin');
  MemoTypesOut.Lines.Add('  Result := ' + ArrName + '[Self];');
  MemoTypesOut.Lines.Add('end;');

  //Interceprot
  IntercName := string(EditType.Text).Remove(0, 3);
  IntercName := 'T' + IntercName + 'Interceptor';

  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add('---------------------------------------------------');
  MemoTypesOut.Lines.Add(IntercName + ' = class(TEnumInterceptor<' + EditType.Text + '>)');
  MemoTypesOut.Lines.Add('public');
  MemoTypesOut.Lines.Add('  function StringConverter(Data: TObject; Field: string): string; override;');
  MemoTypesOut.Lines.Add('  procedure StringReverter(Data: TObject; Field: string; Arg: string); override;');
  MemoTypesOut.Lines.Add('end;');
  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add('{ ' + IntercName + ' }');
  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add('function ' + IntercName + '.StringConverter(Data: TObject; Field: string): string;');
  MemoTypesOut.Lines.Add('begin');
  MemoTypesOut.Lines.Add('  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<' + EditType.Text + '>.ToString;');
  MemoTypesOut.Lines.Add('end;');
  MemoTypesOut.Lines.Add('');
  MemoTypesOut.Lines.Add('procedure ' + IntercName + '.StringReverter(Data: TObject; Field, Arg: string);');
  MemoTypesOut.Lines.Add('begin');
  MemoTypesOut.Lines.Add('  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(' + EditType.Text + '.Create(Arg)));');
  MemoTypesOut.Lines.Add('end;');
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
      ParseParam(MemoIn.Lines[i].Trim, EditName.Text);
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
    MemoOut.Lines.Add('  List.Add(''' + FListParams[i] + ''', Value);');
    MemoOut.Lines.Add('  Result := Self;');
    MemoOut.Lines.Add('end;');
  end;
end;

end.

