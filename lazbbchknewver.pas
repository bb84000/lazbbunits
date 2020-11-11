{****************************************************************************** }
{ lazbbChkNewVer - Check updates on author site                                 }
{ bb - sdtp - november 2019                                                     }
{*******************************************************************************}

unit lazbbchknewver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, fphttpclient,
  fpopenssl, openssl, extctrls, StdCtrls, lazbbutils, opensslsockets ;



  function VersionToInt (VerStr: String): int64;
  function GetLastVersion (url, prog: string; var errmsg: string): string ;
  function ChkLastVersion (url: string; var errmsg: string): string;



implementation



// Convert version string (a.b.c.d) to int64
// "d" is the lower word, "a" is the higher word.
// Equivalent to d+c*65636+b*65636*65636*a*65636*65636*65636
// returns -1 on error

function  VersionToInt (VerStr: String): int64;
var
  A: TStringArray;
  b: array [0..3] of word;
  i: integer;
begin
  Result:= -1;
  if length(VerStr) > 0 then
  begin
    A:= VerStr.Split('.');
    try
      {$IFDEF ENDIAN_LITTLE}
        for i:= 3 downto 0 do b[i]:= StrToInt(A[3-i]);
      {$ENDIF}
      {$IFDEF ENDIAN_BIG}
         for i:= 0 to 3 do b[i]:= StrToInt(A[i]);
      {$ENDIF}
      result:= int64(b);
    except
      Result:= -1;
    end;
  end;
end;

// Retrieve last update form author site.
// version file is a csv, separator: comma, first field: program name, second field: version (string: 'a.b.c.d')
// Other fields can be ignored
// Returns error message in english

function GetLastVersion (url, prog: string; var errmsg: string): string;
var
  MyHTTPCli: TFPHTTPClient;
  ProgList: TStringList;
  i: integer;
  A: TStringArray;
begin
  result:= '';
  errmsg:= '';
  ProgList:= TStringList.Create;
 { SSL initialization has to be done by hand here }
  InitSSLInterface;
  MyHTTPCli:= TFPHTTPClient.Create(nil);
  try
    MyHTTPCli.IOTimeout:= 5000;
    MyHTTPCli.AllowRedirect:= true;
    ProgList.Text:=MyHTTPCli.get(url);
    if Length(ProgList.Text) > 0 then
    begin
      For i:= 0 to ProgList.Count-1 do
      begin
        A:= ProgList.Strings[i].split(';');
        if CompareText (prog, A[0]) = 0 then
        begin
          result:= A[1];
          break;
        end;
      end;
    end;
  except on e:Exception do
    errmsg:= (e.message);
  end;
  ProgList.Free;
  MyHTTPCli.Free;
end;

// Get new version on Github
// Get latest release page
// extract version value fromn header title


function ChkLastVersion(url: string; var errmsg: string): string;
var
  MyHTTPCli: TFPHTTPClient;
  spage: string;
  stagurl: string;
  titlebeg, titleend: Integer;
  A: TStringArray;
begin
  result:= '';
  { SSL initialization has to be done by hand here }
  InitSSLInterface;
  MyHTTPCli:= TFPHTTPClient.Create(nil);
  try
    MyHTTPCli.IOTimeout:= 5000;
    MyHTTPCli.AllowRedirect:= true;
    MyHTTPCli.AddHeader('User-Agent','Mozilla 5.0 (compatible ct)');
    spage:= MyHTTPCli.Get (url);
    titlebeg:= Pos('<title>', spage);
    titleend:= Pos('</title>', spage);
    stagurl:= Copy(spage, titlebeg+7,titleend-titlebeg-7);
    // Title format example :
    // <title>Release Version 0.7.9.8 - 03/11/2020 ...</title>
    // Split title, version is the third array item
    A:= stagurl.Split(' ');
    result:= A[2];
    MyHTTPCli.Free;
  except
    on e:Exception do
    errmsg:= (e.message);
  end;
end;




end.

