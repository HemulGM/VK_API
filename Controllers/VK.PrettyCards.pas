unit VK.PrettyCards;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types;

type
  { TODO -o������� �������� -c : �������� ������ PrettyCard 26.02.2021 14:10:18 }
  /// <summary>
  /// �������� � ����� ��������, ������� ������������� � ������. � ������ �������� ����� �������� �������� � �������� ��������, �����������, ������. ����� ����� ���������� ��� ���� � ������ � ����� � ��������, ����� �������� ������.
  /// �� ������� ������ �������� �������������� ������ � ������� ��������� ������� (��. wall.postAdsStealth).
  /// </summary>
  TPrettyCardsController = class(TVkController)
  public
    /// <summary>
    /// ������ �������� ��������.
    /// �� ������� ������ �������� �������������� ������ � ������� ��������� ������� (��. ����� wall.postAdsStealth)
    /// </summary>
   { function &Create(var Items: TVkPodcasts): Boolean;   }
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TPrettyCardsController }
  {
function TPrettyCardsController.GetPopular(var Items: TVkPodcasts): Boolean;
begin
  with Handler.Execute('podcasts.getPopular') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPodcasts.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;  }

end.

