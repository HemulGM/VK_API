unit VK.Bot.Commands;

interface

uses
  System.SysUtils, System.Variants, System.Classes, VK.Components, VK.GroupEvents, VK.API, VK.Entity.Message,
  VK.Entity.ClientInfo, VK.Types, VK.Entity.Profile, System.Generics.Collections;

type
  TCommandFunc = reference to function(VK: TVK; Message: TVkMessage; ClientInfo: TVkClientInfo): Boolean;

  TTypeOfCommand = (ocEqual, ocContains, ocStartWith, ocCheck, ocAny);

  TCommand = record
    Values: TArray<string>;
    Proc: TCommandFunc;
    Check: TCommandFunc;
    CommandType: TTypeOfCommand;
    CaseSensitive: Boolean;
    class function New(const Values: TArray<string>; Proc: TCommandFunc; CaseSensitive: Boolean = False): TCommand; static;
    class function NewAny(Proc: TCommandFunc): TCommand; static;
    class function NewContains(const Values: TArray<string>; Proc: TCommandFunc; CaseSensitive: Boolean = False): TCommand; static;
    class function NewStartWith(const Values: TArray<string>; Proc: TCommandFunc; CaseSensitive: Boolean = False): TCommand; static;
    class function NewCheck(Check: TCommandFunc; Proc: TCommandFunc): TCommand; static;
  end;

  TCommands = class(TList<TCommand>)
  private
    FVK: TVK;
    procedure SetVK(const Value: TVK);
  public
    procedure HandleMessageGroupEvents(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
    property VK: TVK read FVK write SetVK;
  end;

function RemoveCommand(const Value: string): string;

implementation

uses
  System.Threading;

function RemoveCommand(const Value: string): string;
var
  i: Integer;
begin
  i := Value.IndexOf(' ');
  if i >= 0 then
    Result := Value.Substring(i + 1)
  else
    Result := Value;
end;

{ TCommand }

class function TCommand.New(const Values: TArray<string>; Proc: TCommandFunc; CaseSensitive: Boolean): TCommand;
begin
  Result.Values := Values;
  Result.Proc := Proc;
  Result.CommandType := ocEqual;
  Result.CaseSensitive := CaseSensitive;
end;

class function TCommand.NewAny(Proc: TCommandFunc): TCommand;
begin
  Result.Proc := Proc;
  Result.CommandType := ocAny;
end;

class function TCommand.NewCheck(Check, Proc: TCommandFunc): TCommand;
begin
  Result.Proc := Proc;
  Result.Check := Check;
  Result.CommandType := ocCheck;
end;

class function TCommand.NewContains(const Values: TArray<string>; Proc: TCommandFunc; CaseSensitive: Boolean): TCommand;
begin
  Result.Values := Values;
  Result.Proc := Proc;
  Result.CommandType := ocContains;
  Result.CaseSensitive := CaseSensitive;
end;

class function TCommand.NewStartWith(const Values: TArray<string>; Proc: TCommandFunc; CaseSensitive: Boolean): TCommand;
begin
  Result.Values := Values;
  Result.Proc := Proc;
  Result.CommandType := ocStartWith;
  Result.CaseSensitive := CaseSensitive;
end;

{ TCommands }

procedure TCommands.HandleMessageGroupEvents(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
begin
  TTask.Run(
    procedure
    var
      i: Integer;
      IsOk: Boolean;
      Value: string;
    begin
      IsOk := False;
      try
        for i := 0 to Pred(Count) do
        begin
          case Items[i].CommandType of
            ocEqual, ocContains, ocStartWith:
              begin
                for Value in Items[i].Values do
                begin
                  case Items[i].CommandType of
                    ocEqual:
                      IsOk := string.Compare(Message.Text, Value, Items[i].CaseSensitive) = 0;
                    ocContains:
                      if Items[i].CaseSensitive then
                        IsOk := Message.Text.Contains(Value)
                      else
                        IsOk := Message.Text.ToLower.Contains(Value.ToLower);
                    ocStartWith:
                      IsOk := Message.Text.StartsWith(Value, Items[i].CaseSensitive);
                  end;
                end;
              end;
            ocCheck:
              IsOk := Items[i].Check(FVK, Message, ClientInfo);
            ocAny:
              IsOk := True;
          end;
          if IsOk and Items[i].Proc(FVK, Message, ClientInfo) then
            Break
          else
            IsOk := False;
          if IsOk then
            Break;
        end;
      finally
        Message.Free;
        ClientInfo.Free;
      end;
    end);
end;

procedure TCommands.SetVK(const Value: TVK);
begin
  FVK := Value;
end;

end.

