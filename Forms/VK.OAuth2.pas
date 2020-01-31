unit VK.OAuth2;

interface

uses
  Windows, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.OleCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, SHDocVw;

type
  TOAuth2WebFormRedirectEvent = procedure(const AURL: string; var DoCloseWebView: boolean) of object;

  TOAuth2WebFormTitleChangedEvent = procedure(const ATitle: string; var DoCloseWebView: boolean) of object;

  TOAuth2WebFormErrorEvent = procedure(const AURL: string; AStatusCode: Integer; var Cancel: WordBool) of object;

  TFormOAuth2 = class(TForm)
    Browser: TWebBrowser;
    EditAddr: TEdit;
    Shape1: TShape;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BrowserTitleChange(ASender: TObject; const Text: WideString);
    procedure FormCreate(Sender: TObject);
    procedure BrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure BrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags,
      TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure EditAddrChange(Sender: TObject);
    procedure BrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL, Frame,
      StatusCode: OleVariant; var Cancel: WordBool);
  private
    FOnBeforeRedirect: TOAuth2WebFormRedirectEvent;
    FOnAfterRedirect: TOAuth2WebFormRedirectEvent;
    FOnBrowserTitleChanged: TOAuth2WebFormTitleChangedEvent;
    FOnError: TOAuth2WebFormErrorEvent;
    FLastTitle: string;
    FProxyUserName: string;
    FProxyPassword: string;
    FLastURL: string;
    FNeedShow: Boolean;
    FFreeze: Boolean;
  public
    procedure ShowWithURL(const AURL: string); overload;
    procedure ShowWithURL(AParent: TWinControl; const AURL: string); overload;
    procedure SetProxy(Server: string; Port: Integer; UserName: string = ''; Password: string = '');
    property LastTitle: string read FLastTitle;
    property LastURL: string read FLastURL;
    property OnAfterRedirect: TOAuth2WebFormRedirectEvent read FOnAfterRedirect write FOnAfterRedirect;
    property OnBeforeRedirect: TOAuth2WebFormRedirectEvent read FOnBeforeRedirect write FOnBeforeRedirect;
    property OnTitleChanged: TOAuth2WebFormTitleChangedEvent read FOnBrowserTitleChanged write FOnBrowserTitleChanged;
    property OnError: TOAuth2WebFormErrorEvent read FOnError write FOnError;
  end;

var
  FormOAuth2: TFormOAuth2;

procedure DeleteCache(URLContains: string);

procedure FixIE;

implementation

uses
  WinInet, Registry, UrlMon;

{$R *.dfm}

const
  cBase64Codec: array[0..63] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Base64Filler: AnsiChar = '=';

type
  TAByte = array[0..MaxInt - 1] of Byte;

  TPAByte = ^TAByte;

function CalcEncodedSize(InSize: DWord): DWord;
begin
  // no buffers passed along, calculate outbuffer size needed
  Result := (InSize div 3) shl 2;
  if (InSize mod 3) > 0 then
    Inc(Result, 4);
end;

function CalcDecodedSize(const InBuffer; InSize: DWord): DWord;
begin
  Result := 0;
  if InSize = 0 then
    Exit;
  if (InSize mod 4 <> 0) then
    Exit;

  Result := InSize div 4 * 3;
  if (PByte(DWord(InBuffer) + InSize - 2)^ = Ord(Base64Filler)) then
    Dec(Result, 2)
  else if (PByte(DWord(InBuffer) + InSize - 1)^ = Ord(Base64Filler)) then
    Dec(Result);
end;

procedure Base64Encode(const InBuffer; InSize: DWord; var OutBuffer);
var
  X: Integer;
  PIn, POut: TPAByte;
  Acc: Cardinal;
begin
  if InSize > 0 then
  begin
    PIn := @InBuffer;
    POut := @OutBuffer;

    for X := 1 to InSize div 3 do
    begin
      Acc := PIn^[0] shl 16 + PIn^[1] shl 8 + PIn^[2];

      POut^[0] := Byte(cBase64Codec[(Acc shr 18) and $3f]);
      POut^[1] := Byte(cBase64Codec[(Acc shr 12) and $3f]);
      POut^[2] := Byte(cBase64Codec[(Acc shr 6) and $3f]);
      POut^[3] := Byte(cBase64Codec[(Acc) and $3f]);

      Inc(Cardinal(POut), 4);
      Inc(Cardinal(PIn), 3);
    end;
    case InSize mod 3 of
      1:
        begin
          Acc := PIn^[0] shl 16;

          POut^[0] := Byte(cBase64Codec[(Acc shr 18) and $3f]);
          POut^[1] := Byte(cBase64Codec[(Acc shr 12) and $3f]);
          POut^[2] := Byte(Base64Filler);
          POut^[3] := Byte(Base64Filler);
        end;
      2:
        begin
          Acc := PIn^[0] shl 16 + PIn^[1] shl 8;

          POut^[0] := Byte(cBase64Codec[(Acc shr 18) and $3f]);
          POut^[1] := Byte(cBase64Codec[(Acc shr 12) and $3f]);
          POut^[2] := Byte(cBase64Codec[(Acc shr 6) and $3f]);
          POut^[3] := Byte(Base64Filler);
        end;
    end;
  end;
end;

procedure Base64Decode(const InBuffer; InSize: DWord; var OutBuffer);
const
  cBase64Codec: array[0..255] of Byte = (
            $FF, $FF, $FF, $FF, $FF, {005>} $FF, $FF, $FF, $FF, $FF, // 000..009
              $FF, $FF, $FF, $FF, $FF, {015>} $FF, $FF, $FF, $FF, $FF, // 010..019
              $FF, $FF, $FF, $FF, $FF, {025>} $FF, $FF, $FF, $FF, $FF, // 020..029
              $FF, $FF, $FF, $FF, $FF, {035>} $FF, $FF, $FF, $FF, $FF, // 030..039
              $FF, $FF, $FF, $3E, $FF, {045>} $FF, $FF, $3F, $34, $35, // 040..049
              $36, $37, $38, $39, $3A, {055>} $3B, $3C, $3D, $FF, $FF, // 050..059
              $FF, $00, $FF, $FF, $FF, {065>} $00, $01, $02, $03, $04, // 060..069
              $05, $06, $07, $08, $09, {075>} $0A, $0B, $0C, $0D, $0E, // 070..079
              $0F, $10, $11, $12, $13, {085>} $14, $15, $16, $17, $18, // 080..089
              $19, $FF, $FF, $FF, $FF, {095>} $FF, $FF, $1A, $1B, $1C, // 090..099
              $1D, $1E, $1F, $20, $21, {105>} $22, $23, $24, $25, $26, // 100..109
              $27, $28, $29, $2A, $2B, {115>} $2C, $2D, $2E, $2F, $30, // 110..119
              $31, $32, $33, $FF, $FF, {125>} $FF, $FF, $FF, $FF, $FF, // 120..129
              $FF, $FF, $FF, $FF, $FF, {135>} $FF, $FF, $FF, $FF, $FF, // 130..139
              $FF, $FF, $FF, $FF, $FF, {145>} $FF, $FF, $FF, $FF, $FF, // 140..149
              $FF, $FF, $FF, $FF, $FF, {155>} $FF, $FF, $FF, $FF, $FF, // 150..159
              $FF, $FF, $FF, $FF, $FF, {165>} $FF, $FF, $FF, $FF, $FF, // 160..169
              $FF, $FF, $FF, $FF, $FF, {175>} $FF, $FF, $FF, $FF, $FF, // 170..179
              $FF, $FF, $FF, $FF, $FF, {185>} $FF, $FF, $FF, $FF, $FF, // 180..189
              $FF, $FF, $FF, $FF, $FF, {195>} $FF, $FF, $FF, $FF, $FF, // 190..199
              $FF, $FF, $FF, $FF, $FF, {205>} $FF, $FF, $FF, $FF, $FF, // 200..209
              $FF, $FF, $FF, $FF, $FF, {215>} $FF, $FF, $FF, $FF, $FF, // 210..219
              $FF, $FF, $FF, $FF, $FF, {225>} $FF, $FF, $FF, $FF, $FF, // 220..229
              $FF, $FF, $FF, $FF, $FF, {235>} $FF, $FF, $FF, $FF, $FF, // 230..239
              $FF, $FF, $FF, $FF, $FF, {245>} $FF, $FF, $FF, $FF, $FF, // 240..249
              $FF, $FF, $FF, $FF, $FF, {255>} $FF                      // 250..255
              );
var
  X, Y: Integer;
  PIn, POut: TPAByte;
  Acc: dword;
begin
  if (InSize > 0) and (InSize mod 4 = 0) then
  begin
    InSize := InSize shr 2;
    PIn := @InBuffer;
    POut := @OutBuffer;

    for X := 1 to InSize - 1 do
    begin
      Acc := 0;
      Y := -1;

      repeat
        Inc(Y);
        Acc := Acc shl 6;
        Acc := Acc or cBase64Codec[PIn^[Y]];
      until Y = 3;

      POut^[0] := Acc shr 16;
      POut^[1] := Byte(Acc shr 8);
      POut^[2] := Byte(Acc);

      Inc(Cardinal(PIn), 4);
      Inc(Cardinal(POut), 3);
    end;
    Acc := 0;
    Y := -1;

    repeat
      Inc(Y);
      Acc := Acc shl 6;

      if PIn^[Y] = Byte(Base64Filler) then
      begin
        if Y = 3 then
        begin
          POut^[0] := Acc shr 16;
          POut^[1] := Byte(Acc shr 8);
        end
        else
          POut^[0] := Acc shr 10;
        Exit;
      end;

      Acc := Acc or cBase64Codec[PIn^[Y]];
    until Y = 3;

    POut^[0] := Acc shr 16;
    POut^[1] := Byte(Acc shr 8);
    POut^[2] := Byte(Acc);
  end;
end;

procedure Base64EncodeStr(const InText: AnsiString; var OutText: AnsiString);
var
  InSize, OutSize: DWord;
  PIn, POut: Pointer;
begin
  // get size of source
  InSize := Length(InText);
  // calculate size for destination
  OutSize := CalcEncodedSize(InSize);

  // prepare AnsiString length to fit result data
  SetLength(OutText, OutSize);

  if OutSize > 0 then
  begin
    PIn := @InText[1];
    POut := @OutText[1];

    // encode !
    Base64Encode(PIn^, InSize, POut^);
  end;
end;

procedure Base64DecodeStr(const InText: AnsiString; var OutText: AnsiString);
var
  InSize, OutSize: DWord;
  PIn, POut: Pointer;
begin
  // get size of source
  InSize := Length(InText);
  // calculate size for destination
  PIn := @InText[1];
  OutSize := CalcDecodedSize(PIn, InSize);

  // prepare AnsiString length to fit result data
  SetLength(OutText, OutSize);

  if OutSize > 0 then
  begin
    FillChar(OutText[1], OutSize, '.');
    POut := @OutText[1];

    // encode !
    Base64Decode(PIn^, InSize, POut^);
  end;
end;

function Base64EncodeString(const InText: AnsiString): AnsiString;
begin
  Base64EncodeStr(InText, Result);
end;

function Base64DecodeString(const InText: AnsiString): AnsiString;
begin
  Base64DecodeStr(InText, Result);
end;

function Base64EncodeToString(const InBuffer; InSize: DWord): AnsiString;
var
  POut: Pointer;
begin
  SetLength(Result, CalcEncodedSize(InSize));
  POut := @Result[1];
  Base64Encode(InBuffer, InSize, POut^);
end;

procedure TFormOAuth2.SetProxy(Server: string; Port: Integer; UserName: string; Password: string);
var
  PIInfo: PInternetProxyInfo;
begin
  New(PIInfo);
  PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PROXY;
  PIInfo^.lpszProxy := PAnsiChar(AnsiString(Server + ':' + Port.ToString));
  PIInfo^.lpszProxyBypass := '';
  UrlMkSetSessionOption(INTERNET_OPTION_PROXY, PIInfo, SizeOf(Internet_Proxy_Info), 0);
  Dispose(PIInfo);
  FProxyUserName := UserName;
  FProxyPassword := Password;
end;

procedure FixIE;
const
  IEVersion = 11001;
var
  Reg: TRegistry;
begin
  Reg := TRegIniFile.Create(KEY_WRITE);
  Reg.RootKey := HKEY_CURRENT_USER;
  if Reg.OpenKey('SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION', True) then
  begin
    try
      Reg.WriteInteger(ExtractFileName(Application.ExeName), IEVersion);
    except
    end;
  end;
  Reg.CloseKey;
  Reg.Free;
end;

procedure DeleteCache;
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: LongWord;
  dwEntrySize: LongWord;
begin
  dwEntrySize := 0;
  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  GetMem(lpEntryInfo, dwEntrySize);
  if dwEntrySize > 0 then
    lpEntryInfo^.dwStructSize := dwEntrySize;
  hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);
  if hCacheDir <> 0 then
  begin
    repeat
      if (URLContains = '') or (Pos(URLContains, lpEntryInfo^.lpszSourceUrlName) <> 0) then
        DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
      FreeMem(lpEntryInfo, dwEntrySize);
      dwEntrySize := 0;
      FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize);
      GetMem(lpEntryInfo, dwEntrySize);
      if dwEntrySize > 0 then
        lpEntryInfo^.dwStructSize := dwEntrySize;
    until not FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
  end;
  FreeMem(lpEntryInfo, dwEntrySize);
  FindCloseUrlCache(hCacheDir);
end;

procedure TFormOAuth2.EditAddrChange(Sender: TObject);
begin
  if Copy(EditAddr.Text, 1, 6) = ' https' then
    EditAddr.Color := $00D8FFCC
  else
    EditAddr.Color := $0096AFFF;
end;

procedure TFormOAuth2.BrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL,
  Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
var
  LDoCloseForm: boolean;
begin
  if Assigned(FOnBeforeRedirect) then
  begin
    LDoCloseForm := False;

    FOnBeforeRedirect(URL, LDoCloseForm);

    if LDoCloseForm then
    begin
      Cancel := True;
      if Showing then
        if not FFreeze then
          Close;
    end;
  end;
end;

procedure TFormOAuth2.BrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
var
  LDoCloseForm: Boolean;
begin
  FLastURL := VarToStrDef(URL, '');
  EditAddr.Text := ' ' + FLastURL;
  if Showing then
    Repaint;
  if Assigned(FOnAfterRedirect) then
  begin
    LDoCloseForm := False;

    FOnAfterRedirect(FLastURL, LDoCloseForm);

    if LDoCloseForm then
    begin
      FNeedShow := False;
      if Showing then
        if not FFreeze then
          Close;
    end;
  end;
end;

procedure TFormOAuth2.BrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL,
  Frame, StatusCode: OleVariant; var Cancel: WordBool);
begin
  if Assigned(FOnError) then
    FOnError(URL, StatusCode, Cancel);
  Cancel := True;
end;

procedure TFormOAuth2.BrowserTitleChange(ASender: TObject; const Text: WideString);
var
  LCloseForm: boolean;
begin
  if (Text <> FLastTitle) then
  begin
    FLastTitle := Text;

    if Assigned(FOnBrowserTitleChanged) then
    begin
      LCloseForm := False;
      FOnBrowserTitleChanged(FLastTitle, LCloseForm);

      if LCloseForm then
      begin
        FNeedShow := False;
        if Showing then
          if not FFreeze then
            Close;
      end;
    end;
  end;
end;

procedure TFormOAuth2.FormCreate(Sender: TObject);
begin
  FOnAfterRedirect := nil;
  FOnBeforeRedirect := nil;
  FOnBrowserTitleChanged := nil;
  FLastTitle := '';
  FLastURL := '';
  FixIE;
end;

procedure TFormOAuth2.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  begin
    if not FFreeze then
      Close;
  end;
end;

procedure TFormOAuth2.ShowWithURL(AParent: TWinControl; const AURL: string);
var
  TS: Cardinal;
  S: AnsiString;
begin
  if Assigned(AParent) then
  begin
    SetParent(AParent);
    Align := alClient;
    BorderStyle := bsNone;
  end;

  FNeedShow := True;
  if not FProxyUserName.IsEmpty then
  begin
    Base64EncodeStr(FProxyUserName + ':' + FProxyPassword, S);
    Browser.Navigate2(AURL,
      EmptyParam{Flags},
      EmptyParam{TargetFrameName},
      EmptyParam{PostData},
      'Proxy-Authorization: BASIC ' + S + #13#10 +
      'X-StopHandling: 1' + #13#10);
  end
  else
    Browser.Navigate(AURL);
  TS := GetTickCount;
  FFreeze := True;
  while TS + 3000 > GetTickCount do
    Application.ProcessMessages;
  FFreeze := False;

  if FNeedShow then
    Show
  else
    Close;
end;

procedure TFormOAuth2.ShowWithURL(const AURL: string);
var
  TS: Cardinal;
  S: AnsiString;
begin
  SetParent(nil);
  Align := alNone;
  BorderStyle := bsSizeable;

  FNeedShow := True;

  if not FProxyUserName.IsEmpty then
  begin
    Base64EncodeStr(FProxyUserName + ':' + FProxyPassword, S);
    Browser.Navigate2(AURL,
      EmptyParam{Flags},
      EmptyParam{TargetFrameName},
      EmptyParam{PostData},
      'Proxy-Authorization: BASIC ' + S + #13#10 +
      'X-StopHandling: 1' + #13#10);
  end
  else
    Browser.Navigate(AURL);
  TS := GetTickCount;
  FFreeze := True;
  while TS + 3000 > GetTickCount do
    Application.ProcessMessages;
  FFreeze := False;
  if FNeedShow then
    ShowModal
  else
    Close;
end;

end.

