unit VK.Entity.Ads;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  Vk.Wrap.Interceptors, VK.Entity.Common, VK.Entity.Common.List,
  VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common.ExtendedList, VK.Types;

type
  TVkAdsAccount = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TAdsAccessRoleInterceptor)]
    FAccess_role: TVkAdsAccessRole;
    FAccount_id: Int64;
    FAccount_name: string;
    FAccount_status: Int64;
    [JsonReflectAttribute(ctString, rtString, TAdsAccountTypeInterceptor)]
    FAccount_type: TVkAdsAccountType;
    FAd_network_allowed_potentially: Boolean;
    FCan_view_budget: Boolean;
    function GetIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);
  public
    /// <summary>
    /// Права пользователя в рекламном кабинете
    /// </summary>
    property AccessRole: TVkAdsAccessRole read FAccess_role write FAccess_role;
    /// <summary>
    /// Идентификатор рекламного кабинета
    /// </summary>
    property AccountId: Int64 read FAccount_id write FAccount_id;
    /// <summary>
    /// Имя рекламного кабинета
    /// </summary>
    property AccountName: string read FAccount_name write FAccount_name;
    /// <summary>
    /// Cтатус рекламного кабинета (1 - активен, 0 - не активен)
    /// </summary>
    property AccountStatus: Int64 read FAccount_status write FAccount_status;
    /// <summary>
    /// Тип рекламного кабинета
    /// </summary>
    property AccountType: TVkAdsAccountType read FAccount_type write FAccount_type;
    property AdNetworkAllowedPotentially: Boolean read FAd_network_allowed_potentially write FAd_network_allowed_potentially;
    /// <summary>
    /// Есть ли возможность просматривать бюджет данного рекламного кабинета
    /// </summary>
    property CanViewBudget: Boolean read FCan_view_budget write FCan_view_budget;
    property IsActive: Boolean read GetIsActive write SetIsActive;
  end;

  TVkAdsAccounts = TVkEntityExtendedList<TVkAdsAccount>;

implementation

uses
  VK.CommonUtils;

{ TVkAdsAccount }

function TVkAdsAccount.GetIsActive: Boolean;
begin
  Result := FAccount_status = 1;
end;

procedure TVkAdsAccount.SetIsActive(const Value: Boolean);
begin
  if Value then
    FAccount_status := 1
  else
    FAccount_status := 0;
end;

end.

