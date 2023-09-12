unit Unit5;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, VK.API,
  VK.Components, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts;

type
  TForm5 = class(TForm)
    VK1: TVK;
    Button1: TButton;
    LayoutAuthProc: TLayout;
    Rectangle1: TRectangle;
    AniIndicator1: TAniIndicator;
    ButtonCancelAuth: TButton;
    procedure VK1Auth(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
    procedure Button1Click(Sender: TObject);
    procedure VK1Login(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelAuthClick(Sender: TObject);
  private
    FStopAuth: Boolean;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  VK.ExternalAuth;

{$R *.fmx}

procedure TForm5.Button1Click(Sender: TObject);
begin
  VK1.Login;
end;

procedure TForm5.ButtonCancelAuthClick(Sender: TObject);
begin
  FStopAuth := True;
  ButtonCancelAuth.Enabled := False;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  LayoutAuthProc.Visible := False;
end;

procedure TForm5.VK1Auth(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
begin
  Token := '';
  LayoutAuthProc.Visible := True;
  FStopAuth := False;
  ButtonCancelAuth.Enabled := True;
  GetTokenExternal(Url,
    procedure(var Cancel: Boolean)
    begin
      Cancel := FStopAuth;
    end,
    procedure(const Token: string)
    begin
      LayoutAuthProc.Visible := False;
      if not Token.IsEmpty then
      begin
        VK1.Token := Token;
        VK1.Login;
      end
      else
        ShowMessage('error');
    end);
end;

procedure TForm5.VK1Login(Sender: TObject);
begin
  ShowMessage('success');
end;

end.

