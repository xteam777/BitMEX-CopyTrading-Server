program MCServer;

{$R *.dres}

uses
  FastMM4,
  Vcl.Forms,
  uMain in 'uMain.pas' {fServer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfServer, fServer);
  Application.Run;
end.
