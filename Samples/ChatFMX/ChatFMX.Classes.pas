unit ChatFMX.Classes;

interface

uses
  VK.Types;

type
  TChatType = (ctChat, ctUser, ctGroup);

  TChatInfo = record
    IsCanWrite: Boolean;
    IsP2P: Boolean;
    OutRead: Int64;
    PeerId: TVkPeerId;
  end;

implementation

end.

