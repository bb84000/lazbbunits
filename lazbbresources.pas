{*******************************************************************************
  lazbbResources helper unit
  Load some stuff from RC resources

  bb - sdtp - october 2022
********************************************************************************}
unit lazbbResources;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls,  csvdocument;

  procedure  bbLoadfromResource(var csv: TCSVDocument; ResName: String);
  procedure  bbLoadFromResource(var img: Timage; ResName: String);

const
  RT_RCDATA = MAKEINTRESOURCE(10);

implementation

procedure  bbLoadfromResource (var csv: TCSVDocument; ResName: String);
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(HINSTANCE, ResName, RT_RCDATA);
  try
    csv.LoadFromStream(rs);
  finally
    rs.Free;
  end;
end;

procedure  bbLoadFromResource(var img: Timage; ResName: String);
var
  png : TPortableNetworkgraphic;
begin
  png := TPortableNetworkgraphic.Create;
  try
    png.LoadFromResourceName(HINSTANCE, Resname);
    img.Picture.Assign(png);
  finally
    png.Free;
  end;
end;

end.

