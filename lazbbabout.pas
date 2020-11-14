{****************************************************************************** }
{ lazbbabout - About box for author applications                                }
{ Check new versions functions                                                  }
{ bb - sdtp - november 2019                                                     }
{ Can change width to adapt to program                                           }
{*******************************************************************************}
unit lazbbabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, lclintf, fphttpclient, fpopenssl, openssl, opensslsockets, lazbbutils;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    BtnOK: TBitBtn;
    Image1: TImage;
    LProductName: TLabel;
    LWebSite: TLabel;
    LVersion: TLabel;
    LUpdate: TLabel;
    LCopyright: TLabel;
    PnlDesc: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure LabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LURLClick(Sender: TObject);
  private

  public
    ErrorMessage: String;         // Errormessage passed to main app
    UrlUpdate: String;            // Old version check URL
    UrlWebsite: String;           //
    LastUpdate: TDateTime;
    ChkVerUrl: String;            // New version check theme (Github)
    Version: String;              // New version check
    LastVersion: String;          // New version check
    sLastUpdateSearch: String;    // New version check
    sUpdateAvailable: string;     // New version check
    sNoUpdateAvailable: string;   // New version check
    ProgName: String;
    Checked, NewVersion: Boolean; // New version check
    function ChkNewVersion (url: string=''): string;
  end;
  // Old version check functions
  function VersionToInt (VerStr: String): int64;
  function GetLastVersion (url, prog: string; var errmsg: string): string ;




var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

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


function TAboutBox.ChkNewVersion(url: string=''): string;
var
  MyHTTPCli: TFPHTTPClient;
  spage: string;
  stagurl: string;
  titlebeg, titleend: Integer;
  A: TStringArray;
begin
  result:= '';
  if length(url)=0 then url:= ChkVerUrl;
  { SSL initialization has to be done by hand here }
  InitSSLInterface;
  MyHTTPCli:= TFPHTTPClient.Create(nil);
  try
    MyHTTPCli.IOTimeout:= 5000;
    MyHTTPCli.AllowRedirect:= true;
    MyHTTPCli.AddHeader('User-Agent','Mozilla 5.0 (bb84000 '+ProgName+')');
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
    ErrorMessage:= e.message
  end;
end;

{ TAboutBox }

// Click on Web site URL : launch default browser on author home page
// Click on update URL : launch default browser new version web page

procedure TAboutBox.LURLClick(Sender: TObject);
var
  sendername: string;
  url: string;
begin
  SenderName:= UpperCase(Tcomponent(Sender).Name);
  if SenderName= 'LUPDATE' then
  begin
    if NewVersion then
    begin
      // Link to program binaries is in the help file
      OpenDocument(HelpFile);
      exit;
    end;
    url:= UrlUpdate;
    LastUpdate:= now();
    LUpdate.Hint:= sLastUpdateSearch + ': ' + DateToStr(trunc(LastUpdate));
    // Set url ='' to use Github update scheme instead click go to sdtp version check
    checked:= true;
    If length(url) > 0 then OpenURL(url) else
      LastVersion:= ChkNewVersion(ChkVerUrl);
      if VersionToInt(LastVersion)>VersionToInt(Version) then
      begin
        LUpdate.Caption:= Format(sUpdateAvailable, [LastVersion]);
        NewVersion:= true;
      end else
      begin
        LUpdate.Caption:= sNoUpdateAvailable;
      end;
    end;
  if SenderName= 'LWEBSITE' then
  begin
    url:= UrlWebsite;
    If length(url) > 0 then OpenURL(url);
  end;
end;


procedure TAboutBox.FormActivate(Sender: TObject);
begin
  LWebSite.Caption:= 'Web site: '+UrlWebsite;
end;





// Resize controls when box is resized
procedure TAboutBox.FormChangeBounds(Sender: TObject);
begin
  PnlDesc.left:= 7;
  PnlDesc.Width:= ClientWidth-14;
  LProductName.left:= 0;
  LproductName.Width:= PnlDesc.Width;
  LVersion.left:= 0;
  LVersion.width:= PnlDesc.Width;
  LUpdate.Left:= 0;
  LUpdate.Width:= PnlDesc.Width;
  LCopyright.left:= 0;
  LCopyright.Width:= PnlDesc.Width;
  LWebsite.Left:= 0;
  LWebSite.Width:= PnlDesc.Width;
  BtnOk.Left:= (ClientWidth-BtnOK.Width) div 2;
end;

// Label goes bold when click

procedure TAboutBox.LabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).font.style:= [fsBold];
end;

procedure TAboutBox.LabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.style:= [];
end;

end.

