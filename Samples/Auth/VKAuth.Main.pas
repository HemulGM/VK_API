unit VKAuth.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VK.API,
  VK.Components, Vcl.ExtCtrls, VK.Handler, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    VK1: TVK;
    LabelLogin: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure VK1Login(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  VK.Account.Info, VK.Account.ProfileInfo;

{$R *.dfm}

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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  VK1.Login(Self);
end;

procedure TFormMain.VK1Login(Sender: TObject);
var
  Info: TProfileInfoClass;
begin
  LabelLogin.Caption := 'login';

  Info := VK1.Account.GetProfileInfo();
  Memo1.Lines.Add(Info.country.title);
  Memo1.Lines.Add(Info.relation_partner.first_name);
  Info.Free;
end;

end.

