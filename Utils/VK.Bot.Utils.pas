unit VK.Bot.Utils;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.UITypes, System.Variants, System.Classes, System.StrUtils;

type
  TOnInput = reference to procedure(Command: string; var Quit: Boolean);

  Console = class
    class procedure AddLine(Text: string; AColor: ShortInt = 0); overload;
    class procedure AddLineWait(Text: string; AColor: ShortInt = 0); overload;
    class procedure AddLineWait(AItems: array of string; AColor: ShortInt = 0); overload;
    class procedure AddText(Text: string; AColor: ShortInt = 0); overload;
    class procedure AddLine(AItems: array of string; AColor: ShortInt = 0); overload;
    class function Readln: string; overload;
    class function Read: string; overload;
    class procedure Readln(var Str: string); overload;
    class procedure Read(var Str: string); overload;
    class procedure Run(OnCommand: TOnInput);
  end;

const
{$IFDEF MSWINDOWS}
  RED = FOREGROUND_RED;
  GREEN = FOREGROUND_GREEN;
  BLUE = FOREGROUND_BLUE;
{$ELSE}
  RED = 4;
  GREEN = 2;
  BLUE = 1;
{$ENDIF}

implementation

procedure SetColor(AColor: ShortInt);
begin
  {$IFDEF MSWINDOWS}
  if AColor = 0 then
    AColor := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), AColor);
  {$ENDIF}
end;

class procedure Console.AddLine(Text: string; AColor: ShortInt);
begin
  SetColor(AColor);
  System.Writeln(Text);
  SetColor(0);
end;

class procedure Console.AddText(Text: string; AColor: ShortInt);
begin
  SetColor(AColor);
  System.Write(Text);
  SetColor(0);
end;

class function Console.Read: string;
var
  Str: string;
begin
  System.Read(Str);
  Result := Str;
end;

class function Console.Readln: string;
var
  Str: string;
begin
  System.Readln(Str);
  Result := Str;
end;

class procedure Console.AddLine(AItems: array of string; AColor: ShortInt);
var
  Str, AText: string;
begin
  for Str in AItems do
    AText := AText + ', ' + Str;
  Delete(AText, 1, 2);
  AddLine(AText, AColor);
end;

class procedure Console.AddLineWait(AItems: array of string; AColor: ShortInt);
begin
  AddLine(AItems, AColor);
  System.Readln;
end;

class procedure Console.AddLineWait(Text: string; AColor: ShortInt);
begin
  AddLine(Text, AColor);
  System.Readln;
end;

class procedure Console.Read(var Str: string);
begin
  System.Read(Str);
end;

class procedure Console.Readln(var Str: string);
begin
  System.Readln(Str);
end;

class procedure Console.Run(OnCommand: TOnInput);
var
  Input: string;
  Quit: Boolean;
begin
  repeat
    System.Readln(Input);
    Quit := True;
    if Assigned(OnCommand) then
      OnCommand(Input, Quit);
  until Quit;
  AddLine('Завершение ...');
end;

end.

