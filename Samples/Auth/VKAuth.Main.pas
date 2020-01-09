unit VKAuth.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VK.API, VK.Components, Vcl.ExtCtrls, VK.Handler,
  Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    VK1: TVK;
    Panel1: TPanel;
    LabelLogin: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Panel2: TPanel;
    Memo1: TMemo;
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure VK1Error(Sender: TObject; Code: Integer; Text: string);
    procedure VK1Login(Sender: TObject);
    procedure VK1Log(Sender: TObject; const Value: string);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  VK.Account.Info, VK.Types, VK.Account.ProfileInfo, VK.Account.ActiveOffers, VK.Account.Counters,
  VK.Account.PushSettings, VK.Structs, VK.Users.Types;

{$R *.dfm}

procedure TFormMain.Button10Click(Sender: TObject);
begin
  if VK1.Account.SetOffline then
    Memo1.Lines.Add('offline')
  else
    Memo1.Lines.Add('Error offline');
end;

procedure TFormMain.Button11Click(Sender: TObject);
begin
  if VK1.Auth.CheckPhone('+79512202849', True) then
    Memo1.Lines.Add('CheckPhone')
  else
    Memo1.Lines.Add('Error CheckPhone');
end;

procedure TFormMain.Button12Click(Sender: TObject);
var
  Users: TUsersClass;
  i: Integer;
begin
  if VK1.Users.Get(Users, '286400863', UserFieldsAll, '') then
  begin
    for i := Low(Users.Items) to High(Users.Items) do
    begin
      Memo1.Lines.Add('about: ' + Users.Items[i].about);
      Memo1.Lines.Add('bdate: ' + Users.Items[i].bdate);
      Memo1.Lines.Add('books: ' + Users.Items[i].books);
      Memo1.Lines.Add('domain: ' + Users.Items[i].domain);
      Memo1.Lines.Add('first_name: ' + Users.Items[i].first_name);
      Memo1.Lines.Add('movies: ' + Users.Items[i].movies);
      Memo1.Lines.Add('------------');
    end;
    Users.Free;
  end;
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  if VK1.Account.Ban(-1) then
    Memo1.Lines.Add('Banned')
  else
    Memo1.Lines.Add('Error banned');
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  if VK1.Account.UnBan(-1) then
    Memo1.Lines.Add('Unbanned')
  else
    Memo1.Lines.Add('Error unbanned');
end;

procedure TFormMain.Button3Click(Sender: TObject);
var
  Offers: TActiveOffers;
  i: Integer;
begin
  if VK1.Account.GetActiveOffers(Offers, 0) then
  begin
    Memo1.Lines.Add('ActiveOffers ' + Offers.count.ToString);
    for i := 0 to Length(Offers.items) - 1 do
    begin
      Memo1.Lines.Add('--');
      Memo1.Lines.Add(Offers.items[i].description);
    end;
    Offers.Free;
  end;
end;

procedure TFormMain.Button4Click(Sender: TObject);
var
  Perm: Int64;
begin
  if VK1.Account.GetAppPermissions(Perm, 58553419) then
    Memo1.Lines.Add(Perm.ToString);
end;

procedure TFormMain.Button5Click(Sender: TObject);
var
  Counters: TCountersClass;
begin
  if VK1.Account.GetCounters(Counters) then
  begin
    Memo1.Lines.Add('messages ' + Counters.messages.ToString);
    Memo1.Lines.Add('notifications ' + Counters.notifications.ToString);
    Counters.Free;
  end;
end;

procedure TFormMain.Button6Click(Sender: TObject);
var
  PushSettings: TPushSettingsClass;
begin
  if VK1.Account.GetPushSettings(PushSettings, '1') then
  begin
    Memo1.Lines.Add('disabled ' + PushSettings.disabled.ToString);
    Memo1.Lines.Add('conversations ' + PushSettings.conversations.count.ToString);
    PushSettings.Free;
  end;
end;

procedure TFormMain.Button7Click(Sender: TObject);
var
  Response: TResponse;
  Info: TProfileInfoData;
begin
  Info.Clear;
  Info.status := 'test1';
  if VK1.Account.SaveProfileInfo(Info, Response) then
    Memo1.Lines.Add(Response.Value);
end;

procedure TFormMain.Button8Click(Sender: TObject);
begin
  VK1.CallMethod('account.getProfileInfo', [],
    procedure(Respone: TResponse)
    begin
      ShowMessage(Respone.Value);
    end);
end;

procedure TFormMain.Button9Click(Sender: TObject);
begin
  if VK1.Account.SetOnline() then
    Memo1.Lines.Add('online')
  else
    Memo1.Lines.Add('Error online');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  VK1.Login;
  VK1.UseServiceKeyOnly := True;
end;

procedure TFormMain.VK1Error(Sender: TObject; Code: Integer; Text: string);
begin
  if (not VK1.IsLogin) and (not VK1.UseServiceKeyOnly) then
  begin
    LabelLogin.Caption := 'login error';
    ShowMessage('Ошибка авторизации');
  end
  else
    ShowMessage('Ошибка: ' + Code.ToString + ' - ' + Text);
  Memo1.Lines.Add('Ошибка: ' + Code.ToString + ' - ' + Text);
end;

procedure TFormMain.VK1Log(Sender: TObject; const Value: string);
begin
  MemoLog.Lines.Add('Log: ' + Value);
end;

procedure TFormMain.VK1Login(Sender: TObject);
var
  Info: TProfileInfoClass;
begin
  LabelLogin.Caption := 'login success';

  if VK1.Account.GetProfileInfo(Info) then
  begin
    Memo1.Lines.Add(Info.country.title);
    Memo1.Lines.Add(Info.relation_partner.first_name);
    Info.Free;
  end;
end;

end.

