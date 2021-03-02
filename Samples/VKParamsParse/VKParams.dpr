program VKParams;

uses
  Vcl.Forms,
  VKParams.Main in 'VKParams.Main.pas' {Form14};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
