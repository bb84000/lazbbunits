{******************************************************************************
 lazbbaboutdlg - About box for author applications and update check on GitHub
 Check new versions functions                                                  
 bb - sdtp - february 2022
 Replace lazbbaboutupdate unit:
 - Remove AboutBox creation in application level, creation is done
   in the unit initialization section.
 - No changes in AboutBox use, same parameters as previous one
*******************************************************************************}
unit lazbbaboutdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, lclintf, fphttpclient, fpopenssl, openssl, opensslsockets, lazbbutils;

type

  { TAboutDlg }

  TAboutDlg = class(TForm)
    BtnOK: TBitBtn;
    Image1: TImage;
    LProductName: TLabel;
    LSourceCode: TLabel;
    LWebSite: TLabel;
    LVersion: TLabel;
    LUpdate: TLabel;
    LCopyright: TLabel;
    LProgPage: TLabel;
    PnlDesc: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LURLClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
  private

  public
    ErrorNum: Integer;
    ErrorMessage: String;         // Errormessage passed to main app
    UrlWebsite: String;           // Author Web site
    UrlProgSite: String;          // URL of program page with download link
    UrlSourceCode: String;        // URL of source code if exists
    LastUpdate: TDateTime;
    ChkVerUrl: String;            // New version check theme (Github)
    Version: String;              // Current version
    LastVersion: String;          // Dernière version
    sLastUpdateSearch: String;    // Date de la dernière recherche
    sUpdateAvailable: string;     // Nouvelle version disponible
    sNoUpdateAvailable: string;   // Pas de nouvelle version disponible
    ProgName: String;             // Nom du programme
    Checked, NewVersion: Boolean; // New version check
    function ChkNewVersion (url: string=''): string;
  end;

var
  AboutBox: TAboutDlg;

implementation

{$R *.lfm}

// Public function to get new version on Github
// Get latest release page
// extract version value fromn header title

function TAboutDlg.ChkNewVersion(url: string=''): string;
var
  MyHTTPCli: TFPHTTPClient;
  A: TStringArray;
  sl: TStringList;
  s: string;
  i, p: integer;
begin
  result:= '';
  sl:= TStringList.create();
   if length(url)=0 then url:= ChkVerUrl;
  { SSL initialization has to be done by hand here }
  InitSSLInterface;
  MyHTTPCli:= TFPHTTPClient.Create(nil);
  try
    MyHTTPCli.IOTimeout:= 20000;
    MyHTTPCli.AllowRedirect:= true;
    MyHTTPCli.AddHeader('User-Agent','Mozilla 5.0 (bb84000 '+ProgName+')');
    // Parse last release page to get last version
    // <title>Release Version 0.9.1.7 - 08/04/2021 etc
    sl.text:= MyHTTPCli.Get(url);
    Application.ProcessMessages; //let loading complete
    if sl.Count > 0 then
    begin
      for i:= 0 to sl.Count-1 do
      begin
        p:= pos('<title>', sl.Strings[i]) ;
        if p > 0 then
        begin
          s:=Copy(sl.Strings[i], p+8, 40);
          A:= s.Split(' ');
          break;
        end;
      end;
      // Avoid exception if we dont find the proper line
      if length(A)>=2 then result:= A[2]
      else result:= '';
    end;
  except
    on e:Exception do
       ErrorMessage:= e.message
  end;
  if Assigned(MyHTTPCli) then MyHTTPCli.Free;
  if Assigned(sl) then sl.free;
end;

{ TAboutDlg }

// Click on Web site URL : launch default browser on author home page
// Click on update URL : launch default browser new version web page

procedure TAboutDlg.LURLClick(Sender: TObject);
var
  url: string;
begin
  if TLabel(Sender)= LUpdate then
  begin
    // New version found, click get download page
    if NewVersion then
    begin
      // Link to program binaries is in the help file
      OpenURL(UrlProgSite);
      exit;
    end;
    // No new version at the moment, check it
    //url:= UrlUpdate;
    LastUpdate:= now();
    LUpdate.Hint:= sLastUpdateSearch + ': ' + DateToStr(trunc(LastUpdate));
    checked:= true;
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
  if TLabel(Sender)= LWebSite then
  begin
    url:= UrlWebsite;
    If length(url) > 0 then OpenURL(url);
  end;
  if TLabel(Sender)= LSourceCode then
  begin
    url:= UrlSourceCode;
    If length(url) > 0 then OpenURL(url);
  end;
  if TLabel(Sender)= LProgPage then
  begin
    url:= UrlProgSite;
    If length(url) > 0 then OpenURL(url);
  end;
end;

procedure TAboutDlg.FormActivate(Sender: TObject);
begin
  LWebSite.Hint:= UrlWebsite;
  LProgPage.Hint:= UrlProgSite;
  if length(UrlSourceCode)=0 then
  begin
    LSourceCode.Caption:= '';
    LVersion.Top:= LProductName.Top+25;
    LCopyright.Top:= LVersion.Top+25;
    LUpdate.Top:= LCopyright.Top+25;
    LWebsite.Top:= LUpdate.Top+25;
    LProgPage.Top:= LWebsite.Top+25;
    LSourceCode.visible:= false;
  end else
  begin
    LSourceCode.Hint:= UrlSourceCode;
    LVersion.Top:= LProductName.Top+20;
    LCopyright.Top:= LVersion.Top+20;
    LUpdate.Top:= LCopyright.Top+20;
    LWebsite.Top:= LUpdate.Top+20;
    LProgPage.Top:= LWebsite.Top+20;
    LSourceCode.Top:= LProgPage.Top+20;
    LSourceCode.visible:= true;
  end;
end;

// Resize controls when box is resized
procedure TAboutDlg.FormChangeBounds(Sender: TObject);
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
  LProgPage.Left:= 0;
  LProgPage.Width:= PnlDesc.Width;
  LWebsite.Left:= 0;
  LWebSite.Width:= PnlDesc.Width;
  LSourceCode.Width:= PnlDesc.Width;
  BtnOk.Left:= (ClientWidth-BtnOK.Width) div 2;
end;

procedure TAboutDlg.FormCreate(Sender: TObject);
begin
  inherited;
  UrlSourceCode:='';
end;

// URL Label reactions  : cursor changes to hand when mouse enter and brignt when selected

procedure TAboutDlg.LabelMouseEnter(Sender: TObject);
begin
  if TLabel(Sender).Name='LVersion' then  TLabel(Sender).Cursor:= crHelp
  else TLabel(Sender).Cursor:=  crHandPoint;
end;

procedure TAboutDlg.LabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Cursor:= crDefault;
end;

procedure TAboutDlg.LabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).font.style:= [fsBold];
end;

procedure TAboutDlg.LabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.style:= [];
end;

Initialization

AboutBox:= TAboutDlg.create(nil);

finalization

if Assigned(AboutBox) then AboutBox.Destroy;

end.



