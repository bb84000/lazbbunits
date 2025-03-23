unit lazbbupdatedlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, zipper,
  {$IFDEF WINDOWS}  ShellAPI,
  {$ENDIF}
  ExProgressbar, fphttpclient, opensslsockets;

type
  IProgress = interface
    procedure ProgressNotification(Text: String; MaxProgress,CurrentProgress: int64);
  end;

  { THttpDownloader }

  THttpDownloader = class
  public
    class var
       data: string;
    function download(const url : String; ProgName: String; ProgressMonitor : IProgress) : boolean;
  private
  var
    ProgressMonitor : IProgress;
    procedure DataReceived(Sender: TObject; Const ContentLength, CurrentPos : Int64);
  end;


  { TUpdateBox }

  TUpdateBox = class(TForm, IProgress)
    BtnUpdate: TButton;
    BtnDownload: TButton;
    BtnAbort: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    ProgressbarEx1: TProgressbarEx;
    procedure BtnDownloadClick(Sender: TObject);
    procedure BtnUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    tmpdir: String;
    tmpzip: String;
  public
    ProgName: String;             // Nom du programme
    sNewVer: String;
    ZipInstall: String;           // URL du zip s'installation
    ExeInstall: String;           // Exécutablke installation
    procedure ProgressNotification(thetext: String; MaxProgress,CurrentProgress: int64);
  end;

var
  UpdateBox: TUpdateBox;

implementation

{$R *.lfm}

function THttpDownloader.download(const url : String; ProgName: String; ProgressMonitor : IProgress) : boolean;
var
  client: TFPHTTPClient;
  FSize : int64;
  index : Integer;

begin
  FSize:= 0;
  result:=true;//assume success
  Self.ProgressMonitor:= ProgressMonitor;
  try
    client:= TFPHTTPClient.Create(nil);
    client.AddHeader('User-Agent','Mozilla 5.0 (bb84000 '+ProgName+')');
    client.AllowRedirect:= True;
    try
      client.HTTPMethod('HEAD', url , nil, []);
      FSize := 0;
      for index := 0 to Pred(client.ResponseHeaders.Count) do
      begin
        if LowerCase(client.ResponseHeaders.Names[index]) = 'content-length' then
        begin
          FSize:= StrToInt64(client.ResponseHeaders.ValueFromIndex[index]);
        end;
      end;
      client.OnDataReceived:= @DataReceived;
      data:=client.Get(url);
    except
      on E: Exception do
      begin
        if client.ResponseStatusCode > 399 then
        begin
          ProgressMonitor.ProgressNotification(Format('Status: %d', [client.ResponseStatusCode]) , 0, fsize);//send error message
        end;
        ProgressMonitor.ProgressNotification('Error: ' + E.Message , 0, Fsize);//send error message
        result:=false;
      end;
    end;
  finally
    client.Free;
  end;
end;


procedure THttpDownloader.DataReceived(Sender: TObject; const ContentLength,  CurrentPos: Int64);
begin
  if ContentLength>0 then
    ProgressMonitor.ProgressNotification('' , ContentLength,  CurrentPos);
end;

{ TUpdateBox }

procedure TUpdateBox.FormShow(Sender: TObject);
begin
  Edit1.Text:= sNewVer;
  tmpdir:= GetTempDir;
  BtnUpdate.Enabled:= false;

end;

procedure TUpdateBox.BtnDownloadClick(Sender: TObject);
var
  ZipDownloader: THttpDownloader;
  F: TextFile;
begin
  tmpzip:= tmpdir+ProgName+'.zip';
  try
    ZipDownloader:= THttpDownloader.Create;
    if ZipDownloader.download(ZipInstall, ProgName, self{mainwindow}) then
    begin
      AssignFile(F, tmpzip);
      try
        ReWrite(F);
        Write(F, ZipDownloader.data);
      finally
        CloseFile(F);
      end;
    end;
  except
    Edit1.Text:= 'Erreur de téléchargement';
    BtnUpdate.Enabled:= False;
  end;
  if FileExists(tmpzip) then BtnUpdate.Enabled:= true;
end;

procedure TUpdateBox.BtnUpdateClick(Sender: TObject);
var
   UZip: TUnZipper;
   sei: TShellExecuteInfoA;

begin
  if not FileExists(tmpzip) then
  begin
    BtnUpdate.Enabled:= False;
    close;
  end;
  UZip:= TUnZipper.Create;
  try
    UZip.FileName:= tmpzip;
    UZip.OutputPath:= tmpdir;
    uZip.UnZipFile(tmpzip, ExeInstall);
  finally
    UZip.free;
  end;
  DeleteFile(tmpzip);
  if not FileExists(tmpdir+ExeInstall) then exit;
  {$IFDEF WINDOWS}
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(tmpdir+ExeInstall);
  sei.lpParameters := '';
  sei.nShow := 1; //SW_SHOWNORMAL;
  ShellExecuteExA(@sei);
  {$ENDIF}
end;

procedure TUpdateBox.ProgressNotification(thetext: String; MaxProgress,CurrentProgress: int64);
begin
  if length(theText)>0 then
  else
    ProgressBarEx1.Position:= round(100*CurrentProgress/(maxprogress+1));//+1 to avoid divide by zero
    Application.ProcessMessages;
end;

Initialization

UpdateBox:= TUpdateBox.create(nil);

finalization

if Assigned(UpdateBox) then UpdateBox.Destroy;


end.

