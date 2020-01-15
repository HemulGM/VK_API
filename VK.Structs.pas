unit VK.Structs;

interface

uses
  VK.Types;

type
  TRegisterDeviceData = record
  private
    Fdevice_year: Integer;
    Fsystem_version: string;
    Fsettings: string;
    Fdevice_model: string;
    Fsandbox: string;
    Fdevice_id: string;
    Ftoken: string;
    procedure Setdevice_id(const Value: string);
    procedure Setdevice_model(const Value: string);
    procedure Setdevice_year(const Value: Integer);
    procedure Setsandbox(const Value: string);
    procedure Setsettings(const Value: string);
    procedure Setsystem_version(const Value: string);
    procedure Settoken(const Value: string);
  public
    Fdevice_year_need: Boolean;
    Fsystem_version_need: Boolean;
    Fsettings_need: Boolean;
    Fdevice_model_need: Boolean;
    Fsandbox_need: Boolean;
    Fdevice_id_need: Boolean;
    Ftoken_need: Boolean;
    property token: string read Ftoken write Settoken;
    property device_model: string read Fdevice_model write Setdevice_model;
    property device_year: Integer read Fdevice_year write Setdevice_year;
    property device_id: string read Fdevice_id write Setdevice_id;
    property system_version: string read Fsystem_version write Setsystem_version;
    property settings: string read Fsettings write Setsettings;
    property sandbox: string read Fsandbox write Setsandbox;
    procedure Clear;
  end;

  TProfileInfoData = record
  private
    Fmaiden_name: string;
    Fcancel_request_id: string;
    Fbdate: string;
    Fscreen_name: string;
    Fhome_town: string;
    Ffirst_name: string;
    Frelation_partner_id: string;
    Fstatus: string;
    Fbdate_visibility: string;
    Fcountry_id: string;
    Fsex: string;
    Flast_name: string;
    Frelation: string;
    Fcity_id: string;
    procedure Setbdate(const Value: string);
    procedure Setbdate_visibility(const Value: string);
    procedure Setcancel_request_id(const Value: string);
    procedure Setcity_id(const Value: string);
    procedure Setcountry_id(const Value: string);
    procedure Setfirst_name(const Value: string);
    procedure Sethome_town(const Value: string);
    procedure Setlast_name(const Value: string);
    procedure Setmaiden_name(const Value: string);
    procedure Setrelation(const Value: string);
    procedure Setrelation_partner_id(const Value: string);
    procedure Setscreen_name(const Value: string);
    procedure Setsex(const Value: string);
    procedure Setstatus(const Value: string);
  public
    Fmaiden_name_need: Boolean;
    Fcancel_request_id_need: Boolean;
    Fbdate_need: Boolean;
    Fscreen_name_need: Boolean;
    Fhome_town_need: Boolean;
    Ffirst_name_need: Boolean;
    Frelation_partner_id_need: Boolean;
    Fstatus_need: Boolean;
    Fbdate_visibility_need: Boolean;
    Fcountry_id_need: Boolean;
    Fsex_need: Boolean;
    Flast_name_need: Boolean;
    Frelation_need: Boolean;
    Fcity_id_need: Boolean;
    property first_name: string read Ffirst_name write Setfirst_name;
    property last_name: string read Flast_name write Setlast_name;
    property maiden_name: string read Fmaiden_name write Setmaiden_name;
    property screen_name: string read Fscreen_name write Setscreen_name;
    property cancel_request_id: string read Fcancel_request_id write Setcancel_request_id;
    property sex: string read Fsex write Setsex;
    property relation: string read Frelation write Setrelation;
    property relation_partner_id: string read Frelation_partner_id write Setrelation_partner_id;
    property bdate: string read Fbdate write Setbdate;
    property bdate_visibility: string read Fbdate_visibility write Setbdate_visibility;
    property home_town: string read Fhome_town write Sethome_town;
    property country_id: string read Fcountry_id write Setcountry_id;
    property city_id: string read Fcity_id write Setcity_id;
    property status: string read Fstatus write Setstatus;
    procedure Clear;
  end;

implementation

{ TProfileInfoData }

procedure TProfileInfoData.Clear;
begin
  Fmaiden_name_need := False;
  Fcancel_request_id_need := False;
  Fbdate_need := False;
  Fscreen_name_need := False;
  Fhome_town_need := False;
  Ffirst_name_need := False;
  Frelation_partner_id_need := False;
  Fstatus_need := False;
  Fbdate_visibility_need := False;
  Fcountry_id_need := False;
  Fsex_need := False;
  Flast_name_need := False;
  Frelation_need := False;
  Fcity_id_need := False;
end;

procedure TProfileInfoData.Setbdate(const Value: string);
begin
  Fbdate := Value;
  Fbdate_need := True;
end;

procedure TProfileInfoData.Setbdate_visibility(const Value: string);
begin
  Fbdate_visibility := Value;
  Fbdate_visibility_need := True;
end;

procedure TProfileInfoData.Setcancel_request_id(const Value: string);
begin
  Fcancel_request_id := Value;
  Fcancel_request_id_need := True;
end;

procedure TProfileInfoData.Setcity_id(const Value: string);
begin
  Fcity_id := Value;
  Fcity_id_need := True;
end;

procedure TProfileInfoData.Setcountry_id(const Value: string);
begin
  Fcountry_id := Value;
  Fcountry_id_need := True;
end;

procedure TProfileInfoData.Setfirst_name(const Value: string);
begin
  Ffirst_name := Value;
  Fmaiden_name_need := True;
end;

procedure TProfileInfoData.Sethome_town(const Value: string);
begin
  Fhome_town := Value;
  Fhome_town_need := True;
end;

procedure TProfileInfoData.Setlast_name(const Value: string);
begin
  Flast_name := Value;
  Flast_name_need := True;
end;

procedure TProfileInfoData.Setmaiden_name(const Value: string);
begin
  Fmaiden_name := Value;
  Fmaiden_name_need := True;
end;

procedure TProfileInfoData.Setrelation(const Value: string);
begin
  Frelation := Value;
  Frelation_need := True;
end;

procedure TProfileInfoData.Setrelation_partner_id(const Value: string);
begin
  Frelation_partner_id := Value;
  Frelation_partner_id_need := True;
end;

procedure TProfileInfoData.Setscreen_name(const Value: string);
begin
  Fscreen_name := Value;
  Fscreen_name_need := True;
end;

procedure TProfileInfoData.Setsex(const Value: string);
begin
  Fsex := Value;
  Fsex_need := True;
end;

procedure TProfileInfoData.Setstatus(const Value: string);
begin
  Fstatus := Value;
  Fstatus_need := True;
end;


{ TRegisterDeviceData }

procedure TRegisterDeviceData.Clear;
begin
  Fdevice_year_need := False;
  Fsystem_version_need := False;
  Fsettings_need := False;
  Fdevice_model_need := False;
  Fsandbox_need := False;
  Fdevice_id_need := False;
  Ftoken_need := False;
end;

procedure TRegisterDeviceData.Setdevice_id(const Value: string);
begin
  Fdevice_id := Value;
  Fdevice_id_need := True;
end;

procedure TRegisterDeviceData.Setdevice_model(const Value: string);
begin
  Fdevice_model := Value;
  Fdevice_model_need := True;
end;

procedure TRegisterDeviceData.Setdevice_year(const Value: Integer);
begin
  Fdevice_year := Value;
  Fdevice_year_need := True;
end;

procedure TRegisterDeviceData.Setsandbox(const Value: string);
begin
  Fsandbox := Value;
  Fsandbox_need := True;
end;

procedure TRegisterDeviceData.Setsettings(const Value: string);
begin
  Fsettings := Value;
  Fsettings_need := True;
end;

procedure TRegisterDeviceData.Setsystem_version(const Value: string);
begin
  Fsystem_version := Value;
  Fsystem_version_need := True;
end;

procedure TRegisterDeviceData.Settoken(const Value: string);
begin
  Ftoken := Value;
  Ftoken_need := True;
end;

end.

