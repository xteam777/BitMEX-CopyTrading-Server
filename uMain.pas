 unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, rtcSystem, rtcInfo, rtcConn, rtcDataSrv, sgcJSON, cHash,
  rtcHttpSrv, Vcl.StdCtrls, Vcl.WinXCtrls, AdvEdit, AdvGrid, rtcSrvModule, rtcFunction,
  sgcWebSocket_Classes, sgcWebSocket_Classes_Indy, sgcWebSocket_Server, System.Net.URLClient,
  System.NetEncoding, sgcWebSocket, sgcWebSocket_Client, System.Net.HTTPClient, System.DateUtils,
  rtcDataCli, rtcCliModule, rtcHttpCli, Vcl.ExtCtrls, SyncObjs, System.IOUtils, rtcLog, StrUtils,
  sgcBase_Classes, sgcTCP_Classes;

type
  TSendStrategyChangeToGateway = procedure(strategy, action: String; koeff: Integer) of Object;
  TUnsubscribeFromStrategy = procedure (strategyId: String) of Object;

  TCheckPingsThread = class(TThread)
  protected
    FSendStrategyChangeToGateway: TSendStrategyChangeToGateway;
    FUnsubscribeFromStrategy: TUnsubscribeFromStrategy;
    procedure Execute; override;
    procedure OnStrategyDisconnect(strategyId: String);
  end;

  PPositionData = ^TPositionData;
  TPositionData = record
    symbol: String;
    leverage: String;
    crossMargin: String;
    currentQty: String;
    avgEntryPrice: String;
    lastPrice: String;
    markPrice: String;
    liquidationPrice: String;
    realisedPnl: String;
    unrealisedPnl: String;
    posInit: String;
    posMargin: String;
  end;

  POrderData = ^TOrderData;
  TOrderData = record
    symbol: String;
    side: String;
    orderQty: String;
    price: String;
    ordType: String;
    execInst: String;
    displayQty: String;
    pegPriceType: String;
    pegOffsetValue: String;
    stopPx: String;
    timeInForce: String;
    orderID: String;
    leavesQty: String;
  end;

  PsgcWSConnection = ^TsgcWSConnection;

  PUserData = ^TUserData;
  TUserData = record
    email: String;
    connectionsList: TStringList;
  end;

  PStrategyData = ^TStrategyData;
  TStrategyData = record
    strategyId: String;
    apiKey: String;
    apiSecret: String;
    lastUsed: TDateTime;
    usersList: TList;
    usersInfo: TRtcInfo;
    ordersList : TList;
    ordersInfo: TRtcInfo;
    ordersGot: Boolean;
    positionsList : TList;
    positionsInfo: TRtcInfo;
    positionsGot: Boolean;
    state: String;
    runDate: TDateTime;
  end;

  TfServer = class(TForm)
    Label1: TLabel;
    eAddress: TAdvEdit;
    tsState: TToggleSwitch;
    WSServer: TsgcWebSocketServer;
    WSClient: TsgcWebSocketClient;
    Client: TRtcHttpClient;
    ClientModule: TRtcClientModule;
    ClientFunctionGroup: TRtcFunctionGroup;
    tReconnect: TTimer;
    rDoLogin: TRtcResult;
    rPing: TRtcResult;
    tPing: TTimer;
    eGateway: TAdvEdit;
    Label2: TLabel;
    Server: TRtcHttpServer;
    ServerModule: TRtcServerModule;
    ServerFunctionGroup: TRtcFunctionGroup;
    rDoLogout: TRtcResult;
    rChangeStrategy: TRtcResult;
    rStrategy_Deactivate: TRtcResult;
    Button1: TButton;
    tRestart: TTimer;
    cbExtendedLog: TCheckBox;
    procedure tsStateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure WSClientMessage(Connection: TsgcWSConnection; const Text: string);
    procedure WSServerMessage(Connection: TsgcWSConnection; const Text: string);
    procedure tReconnectTimer(Sender: TObject);
    procedure ClientConnect(Sender: TRtcConnection);
    procedure ClientDisconnect(Sender: TRtcConnection);
    procedure rDoLoginReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure tPingTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure rChangeStrategyReturn(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure WSServerAuthentication(Connection: TsgcWSConnection; aUser,
      aPassword: string; var Authenticated: Boolean);
    procedure WSServerDisconnect(Connection: TsgcWSConnection; Code: Integer);
    procedure WSServerConnect(Connection: TsgcWSConnection);
    procedure rPingReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure WSClientDisconnect(Connection: TsgcWSConnection; Code: Integer);
    procedure WSClientConnect(Connection: TsgcWSConnection);
    procedure WSClientError(Connection: TsgcWSConnection; const Error: string);
    procedure WSClientException(Connection: TsgcWSConnection; E: Exception);
    procedure tRestartTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    slSymbolSubs: TStringList;
//    function GetConnectionData(strategyId: String; var strategyActive: Boolean): Boolean;
    procedure SubscribeToStrategy(strategyId, apiKey, apiSecret: String);
    procedure UnsubscribeFromStrategy(strategyId: String);
    procedure DoSubscribeError(strategyId, text: String);
    procedure DeleteStragegyItem(strategyId: String);
    procedure DeactivateStragegyOnGateway(strategyId, state: String);
    function RunCall(verb, url, path, data, apiKey, apiSecret: String; needAuth, onlyGetHMAC: Boolean; var nonce, hmacsignature: String): String;
    function UrlEncode2(const S: string): string;
    function GetCurrentTimestamp: Int64;
    function DigitCheckNull(Val: String): String;
    procedure DoLogin;
    procedure DoLogout;
    procedure SendStrategyChangeToGateway(strategy, action: String; koeff: Integer);
    procedure LoadSettings;
    procedure SaveSettings;
    function GetStrategiesRec: TRtcRecord;
    procedure DoStart;
    procedure DoStop;
    function EncodeBase64(str: String): String;
    function DecodeBase64(str: String): String;
    procedure SendOrdersPartial(ConnectionGUID: String);
    procedure SendPositionsPartial(ConnectionGUID: String);
    procedure SendChangesToUsers(strategyId, text: String);
    procedure SaveDlls;
  end;

const
  url = 'https://testnet.bitmex.com';
  wsUrl = 'wss://testnet.bitmex.com/realtimemd';
  noActivityTimeout = 20 * 60 * 1000; //20 минут //Если стратегия никем не используется, отключаем ее
  noSubscribeTimeout = 1 * 60 * 1000; //1 минута //Если с момента подписки прошло столько времени и не получены партиал таблицы, тоже отключаем

var
  fServer: TfServer;
  CS: TCriticalSection;
  tCPThread: TCheckPingsThread;
  strategiesList: TList;
  strategiesInfo: TRtcInfo;
  connectionInfo: TRtcInfo;

implementation

{$R *.dfm}

procedure TCheckPingsThread.Execute;
var
  i: Integer;
begin
  while not Terminated do
  begin
    CS.Acquire;
    try
      i := strategiesList.Count - 1;
      while i >= 0 do
      begin
        if ((PStrategyData(strategiesList[i])^.usersList.Count = 0)
          and (PStrategyData(strategiesList[i])^.lastUsed < IncMillisecond(Now, -noActivityTimeout))) //Стратегия никем не используется
          or (((PStrategyData(strategiesList[i])^.runDate < IncMillisecond(Now, -noSubscribeTimeout))
            and (not PStrategyData(strategiesList[i])^.ordersGot)
            and (not PStrategyData(strategiesList[i])^.positionsGot))) then //Не получены партиал таблицы
        begin
          OnStrategyDisconnect(PStrategyData(strategiesList[i])^.strategyId);

          FUnsubscribeFromStrategy(PStrategyData(strategiesList[i])^.strategyId);
        end;

        i := i - 1;
      end;
    finally
      CS.Release;
    end;

    Sleep(1000);
  end;
end;

procedure TCheckPingsThread.OnStrategyDisconnect(strategyId: String);
begin
  xLog('Strategy disconnected: ' + strategyId);
end;

function TfServer.EncodeBase64(str: String): String;
var
  stream1, stream2: TStringStream;
begin
  stream1 := TStringStream.Create;
  stream2 := TStringStream.Create;
  try
    stream1.WriteString(str);
    stream1.Position := 0;
    TNetEncoding.Base64.Encode(stream1, stream2);
    stream2.Position := 0;
    Result := stream2.ReadString(stream2.Size);
  finally
    try
      FreeAndNil(stream2);
    except
    end;
    try
      FreeAndNil(stream1);
    except
    end;
  end;
end;

function TfServer.DecodeBase64(str: String): String;
var
  stream1, stream2: TStringStream;
begin
  stream1 := TStringStream.Create;
  stream2 := TStringStream.Create;
  try
    stream1.WriteString(str);
    stream1.Position := 0;
    TNetEncoding.Base64.Decode(stream1, stream2);
    stream2.Position := 0;
    Result := stream2.ReadString(stream2.Size);
  finally
    try
      FreeAndNil(stream2);
    except
    end;
    try
      FreeAndNil(stream1);
    except
    end;
  end;
end;

procedure TfServer.FormDestroy(Sender: TObject);
var
  i, j: Integer;
begin
  CS.Acquire;
  try
    tCPThread.Terminate;

    for i := 0 to strategiesList.Count - 1 do
    begin
      for j := 0 to PStrategyData(strategiesList[i])^.usersList.Count - 1 do
        Dispose(PUserData(PStrategyData(strategiesList[i])^.usersList[j]));
      for j := 0 to PStrategyData(strategiesList[i])^.ordersList.Count - 1 do
        Dispose(POrderData(PStrategyData(strategiesList[i])^.ordersList[j]));
      for j := 0 to PStrategyData(strategiesList[i])^.positionsList.Count - 1 do
        Dispose(PPositionData(PStrategyData(strategiesList[i])^.positionsList[j]));

      PStrategyData(strategiesList[i]).usersInfo.Free;
      PStrategyData(strategiesList[i]).ordersInfo.Free;
      PStrategyData(strategiesList[i]).positionsInfo.Free;
      Dispose(PStrategyData(strategiesList[i]));
    end;
    FreeAndNil(strategiesList);
    FreeAndNil(strategiesInfo);
    FreeAndNil(connectionInfo);
    FreeAndNil(slSymbolSubs);
  finally
    CS.Release;
  end;
end;

procedure TfServer.DeleteStragegyItem(strategyId: String);
var
  i, j: Integer;
  pSData: PStrategyData;
  pUData: PUserData;
begin
  CS.Acquire;
  try
    if (strategiesInfo.Child[strategyId] <> nil) then
    begin
      pSData := PStrategyData(strategiesInfo.Child[strategyId].asPtr['data']);

      for i := 0 to pSData^.usersList.Count - 1 do
      begin
        pUData := PUserData(pSData^.usersList[i]);
        for j := 0 to pUData^.connectionsList.Count - 1 do
        begin
          //Чтобы не очищалось в WSServer.OnDisconnect
          connectionInfo.Child[pUData^.connectionsList[j]].asPtr['userData'] := nil;
          connectionInfo.Child[pUData^.connectionsList[j]].asPtr['data'] := nil;

          WSServer.ConnectionsByGUID[pUData^.connectionsList[j]].Disconnect;
          xLog('Do disconnect: ' + pUData^.connectionsList[j]);
        end;
        FreeAndNil(pUData^.connectionsList);

        Dispose(PUserData(pSData^.usersList[i]));
      end;
      pSData^.usersList.Free;
      pSData^.usersInfo.Free;

      for i := 0 to pSData^.ordersList.Count - 1 do
        Dispose(POrderData(pSData^.ordersList[i]));
      pSData^.ordersList.Free;
      pSData^.ordersInfo.Free;

      for i := 0 to pSData^.positionsList.Count - 1 do
        Dispose(PPositionData(pSData^.positionsList[i]));
      pSData^.positionsList.Free;
      pSData^.positionsInfo.Free;

      i := strategiesList.IndexOf(pSData);
      strategiesList.Delete(i);
      strategiesInfo.SetNil(strategyId);
      Dispose(pSData);
    end;
  finally
    CS.Release;
  end;
end;

function TfServer.GetStrategiesRec: TRtcRecord;
var
  i: Integer;
begin
  CS.Acquire;
  try
    Result := TRtcRecord.Create;
    Result.AutoCreate := True;
    for i := 0 to strategiesList.Count - 1 do
      Result.asString[PStrategyData(strategiesList[i])^.strategyId] := PStrategyData(strategiesList[i])^.strategyId;
  finally
    CS.Release;
  end;
end;

procedure TfServer.DoLogin;
begin
  CS.Acquire;
  try
    with ClientModule, Data.NewFunction('Server_DoLogin') do
    begin
      asString['address'] := eAddress.Text;
      asRecord['strategies'] := GetStrategiesRec;
      asInteger['subscriptionsCount'] := WSServer.Count;
      Call(rDoLogin);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.DeactivateStragegyOnGateway(strategyId, state: String);
begin
  CS.Acquire;
  try
    try
      with ClientModule, Data.NewFunction('Strategy_Deactivate') do
      begin
        asString['strategyId'] := strategyId;
        asString['state'] := state;
        Call(rStrategy_Deactivate);
      end;
    except
      on e: Exception do
        xLog('Strategy_Deactivate: ' + e.Message);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.SendStrategyChangeToGateway(strategy, action: String; koeff: Integer);
begin
  CS.Acquire;
  try
    with ClientModule, Data.NewFunction('Server_StatChanges') do
    begin
      asString['address'] := eAddress.Text;
      asString['strategy'] := strategy;
      asString['action'] := action;
      asInteger['subscriptionsCount'] := WSServer.Count + koeff;
      Call(rChangeStrategy);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.LoadSettings;
var
  oJSON: TsgcJSON;
  s: String;
  i, j: Integer;
  val: Extended;
  fFound: Boolean;
begin
  if not FileExists(SysUtils.ChangeFileExt(ParamStr(0), '.cfg')) then
    Exit;

  oJSON := TsgcJSON.Create(nil);
  try
    s := TFile.ReadAllText(SysUtils.ChangeFileExt(ParamStr(0), '.cfg'));
    oJSON.Read(s);

    if (oJSON.Node['Address'] <> nil) then
      eAddress.Text := oJSON.Node['Address'].Value;
    if (oJSON.Node['Gateway'] <> nil) then
      eGateway.Text := oJSON.Node['Gateway'].Value;
    if (oJSON.Node['ExtendedLog'] <> nil) then
      if oJSON.Node['ExtendedLog'].Value = 'True' then
        cbExtendedLog.Checked := True
      else
        cbExtendedLog.Checked := False;
  finally
    FreeAndNil(oJSON)
  end;
end;

procedure TfServer.SaveSettings;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  try
    oJSON.AddPair('Address', eAddress.Text);
    oJSON.AddPair('Gateway', eGateway.Text);
    if cbExtendedLog.Checked then
      oJSON.AddPair('ExtendedLog', 'True')
    else
      oJSON.AddPair('ExtendedLog', 'False');
    TFile.WriteAllText(SysUtils.ChangeFileExt(ParamStr(0), '.cfg'), oJSON.Text);
  finally
    FreeAndNil(oJSON)
  end;
end;

procedure TfServer.rChangeStrategyReturn(Sender: TRtcConnection; Data,
  Result: TRtcValue);
begin
  if Result.isType = rtc_Exception then
    xLog('rChangeStrategyReturn Error: ' + Result.asException)
  else
  if Result.isType <> rtc_Boolean then
    xLog('rChangeStrategyReturn Invalid result type');
end;

procedure TfServer.rDoLoginReturn(Sender: TRtcConnection; Data,
  Result: TRtcValue);
begin
  if Result.isType = rtc_Exception then
    xLog('rDoLoginReturn Error: ' + Result.asException)
  else
  if Result.isType <> rtc_Boolean then
    xLog('rDoLoginReturn Invalid result type');
end;

procedure TfServer.rPingReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
var
  Splitted: TArray<String>;
  i: Integer;
begin
  CS.Acquire;
  try
    if Result.isType = rtc_Exception then
    begin
      xLog(Result.asException);
    end
    else
    if Result.isType = rtc_Record then
    begin
      with Result.asRecord do
      begin
        if asBoolean['needLogin'] then
          DoLogin;
        if (asString['msg'] <> '') then
        begin
          Splitted := asString['msg'].Split([';']);
          for i := 0 to Length(Splitted) - 1 do
            if (strategiesInfo.Child[Splitted[i]] <> nil) then
            begin
              UnsubscribeFromStrategy(Splitted[i]);
              SendStrategyChangeToGateway(Splitted[i], 'Remove', 0);
            end;
        end;
      end;
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.DoLogout;
begin
  CS.Acquire;
  try
    with ClientModule, Data.NewFunction('Server_DoLogout') do
    begin
      asString['address'] := eAddress.Text;
      Call(rDoLogout);
    end;
  finally
    CS.Release;
  end;
end;                                            

procedure TfServer.DoSubscribeError(strategyId, text: String);
begin
  xLog('DoSubscribeError ' + strategyId + ': ' + text);
end;

procedure TfServer.ClientConnect(Sender: TRtcConnection);
begin
  tReconnect.Enabled := False;
  tPing.Enabled := (Client.isConnected and (tsState.State = tssOn));

  DoLogin;

  xLog('Client connected');
end;

procedure TfServer.ClientDisconnect(Sender: TRtcConnection);
begin
  tPing.Enabled := (Client.isConnected and (tsState.State = tssOn));
  tReconnect.Enabled := True;

  xLog('Client disconnected');
end;

procedure TfServer.Button1Click(Sender: TObject);
{const
  path: String = '/realtime';
  verb: String = 'GET';
  data: String = '';
  strategyId: String = 'cc2ee017-c7e6-45f7-a853-9064a81b723d';
  //sstuman@gmail.com
  apiKey: String = 'UNRyH7w0BV7Qj89Ydpt5y2lp';
  apiSecret: String = 'ZzXSbeVJxlQnkiQDVCC_gjg5vfCyzPH2l8XvqyaTHzHXoGPo';
var
  nonce: String;
  hmacsignature: String;
  i: Integer;}
begin
//  WSClient.WriteData('[1, "' + strategyId + '", "' + strategyId + '"]');
//  RunCall(verb, url, path, data, apiKey, apiSecret, True, True, nonce, hmacsignature);
//  WSClient.WriteData('[0, "' + strategyId + '", "' + strategyId + '", {"op": "authKeyExpires", "args":["' + apiKey + '", ' + nonce + ', "' + hmacsignature + '"]}]');
//  for i := 0 to slSymbolSubs.Count - 1 do
//    WSClient.WriteData('[0, "' + strategyId + '", "' + strategyId + '", {"op": "subscribe", "args": "'+ slSymbolSubs[i] + '"}]');
end;

procedure TfServer.Button2Click(Sender: TObject);
//const
//  strategyId: String = 'cc2ee017-c7e6-45f7-a853-9064a81b723d';
begin
//  WSClient.WriteData('[2, "' + strategyId + '", "' + strategyId + '"]');
end;

function TfServer.DigitCheckNull(Val: String): String;
begin
  if Val <> 'null' then
    Result := Val
  else
    Result := '0';
end;

function TfServer.GetCurrentTimestamp: Int64;
begin
  Sleep(10);

  Result := MilliSecondsBetween(Now, EncodeDate(1970, 1, 1));
end;

function TfServer.UrlEncode2(const S: string): string;
var
  I: Integer;
begin
  Result := EmptyStr;
  for i := 1 to Length(S) do
    case S[i] of
    // The NoConversion set contains characters as specificed in RFC 1738 and
    // should not be modified unless the standard changes.
    'A'..'Z', 'a'..'z', '*', '@', '.', '_', '-', '0'..'9',
    '$', '!', '''', '(', ')':
       Result := Result + S[i];
    '—': Result := Result + '%E2%80%94';
    ' ':
{        Result := Result + '+';
    '+':}
        Result := Result + '%20';
   else
     Result := Result + '%' + System.SysUtils.IntToHex(Ord(S[i]), 2);
   end;
end;

function TfServer.RunCall(verb, url, path, data, apiKey, apiSecret: String; needAuth, onlyGetHMAC: Boolean; var nonce, hmacsignature: String): String;
var
  FHTTPClient: THTTPClient;
  LRequest: IHTTPRequest;
  LResponse: IHTTPResponse;
  LHeaders, LHeadersAuth: TNetHeaders;

  sContent: TStringStream;
  i: Integer;
begin
  Result := '';

  if needAuth then
  begin
    nonce := GetCurrentTimestamp.ToString;
    if (verb <> 'GET') then
      hmacsignature := SHA256DigestToHexA(CalcHMAC_SHA256(apiSecret, verb + path + nonce + data))
    else
      hmacsignature := SHA256DigestToHexA(CalcHMAC_SHA256(apiSecret, verb + path + nonce));

    LHeadersAuth := [
      TNetHeader.Create('api-nonce', nonce),
      TNetHeader.Create('api-key', apiKey),
      TNetHeader.Create('api-signature', hmacsignature)];

    if onlyGetHMAC then
      Exit;
  end;

  FHTTPClient := THTTPClient.Create;
  try
    if (verb <> 'GET') then
    begin
      LRequest := FHTTPClient.GetRequest(verb, url + path);

      sContent := TStringStream.Create;
      sContent.WriteString(data);
      sContent.Position := 0;
      LRequest.SourceStream := sContent;
    end
    else
      LRequest := FHTTPClient.GetRequest(verb, url + path);

    LHeaders := [
      TNetHeader.Create('Content-Type', 'application/x-www-form-urlencoded'),
      TNetHeader.Create('Cache-Control', 'max-age=0'),
      TNetHeader.Create('Connection', 'Keep-Alive'),
      TNetHeader.Create('Keep-Alive', '90')];

    if needAuth then
      LResponse := FHTTPClient.Execute(LRequest, nil, LHeaders + LHeadersAuth)
    else
      LResponse := FHTTPClient.Execute(LRequest, nil, LHeaders);

    Result := LResponse.ContentAsString;

//    for i := 0 to Length(LResponse.Headers) - 1 do
//      if LResponse.Headers[i].Name = 'X-RateLimit-Remaining' then
//      begin
//        lRateLimit.Caption := LResponse.Headers[i].Value;
//      end; //При уменьшении ниже 200 оповещение на гейт
  finally
    FreeAndNil(sContent);
    FreeAndNil(FHTTPClient);
  end;
end;

procedure TfServer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  DoStop;

  SaveSettings;

  StopLog;
end;

procedure TfServer.FormCreate(Sender: TObject);
begin
  RTC_LOGS_LIVE_DAYS := 30;

  StartLog;

  SaveDlls;

  tCPThread := TCheckPingsThread.Create(True);
  tCPThread.FreeOnTerminate := True;
  tCPThread.FSendStrategyChangeToGateway := SendStrategyChangeToGateway;
  tCPThread.FUnsubscribeFromStrategy := UnsubscribeFromStrategy;

  slSymbolSubs := TStringList.Create;
  slSymbolSubs.Add('position');
  slSymbolSubs.Add('order');
  slSymbolSubs.Add('execution');
  slSymbolSubs.Add('wallet');

  WSClient.URL := wsUrl;

  strategiesList := TList.Create;
  strategiesInfo := TRtcInfo.Create;
  connectionInfo := TRtcInfo.Create;

  tsState.ThumbColor := RGB(235, 55, 71);

  LoadSettings;
end;

procedure TfServer.tPingTimer(Sender: TObject);
begin
  CS.Acquire;
  try
    with ClientModule, Data.NewFunction('Server_Ping') do
    begin
      asString['address'] := eAddress.Text;
      Call(rPing);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.tReconnectTimer(Sender: TObject);
begin
  if (tsState.State = tssOn) then
    Client.Connect(True);
end;

procedure TfServer.tRestartTimer(Sender: TObject);
begin
  tRestart.Enabled := False;

  WSClient.Active := False;
  WSClient.URL := wsUrl;
  WSClient.Start;
end;

procedure TfServer.tsStateClick(Sender: TObject);
begin
  tsState.Enabled := False;

  if tsState.State = tssOn then
    DoStart
  else
    DoStop;

  tsState.Enabled := True;
end;

procedure TfServer.DoStart;
begin
  if Trim(eAddress.Text) = '' then
  begin
    MessageBox(Handle, 'Address is empty', 'MC Server', MB_OK + MB_ICONERROR);
    tsState.State := tssOff;
    Exit;
  end;
  if Trim(eGateway.Text) = '' then
  begin
    MessageBox(Handle, 'Gateway is empty', 'MC Server', MB_OK + MB_ICONERROR);
    tsState.State := tssOff;
    Exit;
  end;

  try
    WSClient.Active := True;
    WSServer.Active := True;

    Client.ServerAddr := eGateway.Text;
    Client.Connect(True);

    tsState.ThumbColor := RGB(33, 115, 56);

    tReconnect.Enabled := True;
  except
    on E: Exception do
    begin
      xLog('Error: ' + e.Message);

      DoStop;

      Exit;
    end;
  end;

  tPing.Enabled := (Client.isConnected and (tsState.State = tssOn));

  tCPThread.Resume;
end;

procedure TfServer.DoStop;
var
  i: Integer;
begin
  tPing.Enabled := (Client.isConnected and (tsState.State = tssOn));

  tCPThread.Suspend;

  for i := 0 to strategiesList.Count - 1 do
    DeleteStragegyItem(PStrategyData(strategiesList[i])^.strategyId);

  DoLogout;

  tReconnect.Enabled := False;

  try
    WSServer.Active := False;
  finally
  end;
  try
    WSClient.Active := False;
  finally
  end;

  Client.DisconnectNow(False);
  tsState.ThumbColor := RGB(235, 55, 71);
end;

{function TfServer.GetConnectionData(strategyId: String; var strategyActive: Boolean): Boolean;
begin
  CS.Acquire;
  try
    Result := False;

    try
      with ClientModule, Prepare('Client_GetConnectionData') do
      begin
        Param.asString['strategyId'] := strategyId;
        Execute;
        Result := LastResult.asRecord.asBoolean['result'];
        strategyActive := LastResult.asRecord.asBoolean['strategyActive'];
      end;
    except
      on e: Exception do
      begin
        xLog('GetConnectionData: ' + e.Message);
        Result := False;
      end;
    end;
  finally
    CS.Release;
  end;
end;}

procedure TfServer.WSServerAuthentication(Connection: TsgcWSConnection; aUser,
  aPassword: string; var Authenticated: Boolean);
var
  pSData: PStrategyData;
  pUData: PUserData;
  oJSON: TsgcJSON;
  strategyActive: Boolean;
begin
//  aUser - email
//  aPassword - strategyId / apiKey / apiSecret
  CS.Acquire;
  try
    oJSON := TsgcJSON.Create(nil);
    try
      oJSON.Read(DecodeBase64(aPassword));

//      if not GetConnectionData(oJSON.Node['strategyId'].Value, strategyActive) then
//        Exit;
//      if not strategyActive then
//      begin
//        if (strategiesInfo.Child[VarToStr(oJSON.Node['strategyId'].Value)] <> nil) then
//          UnsubscribeFromStrategy(oJSON.Node['strategyId'].Value);
//        Exit;
//      end;

      Authenticated := True;

      //Если стратегия есть то добавляем в нее пользователя
      if (strategiesInfo.Child[VarToStr(oJSON.Node['strategyId'].Value)] <> nil) then
      begin
        pSData := PStrategyData(strategiesInfo.Child[VarToStr(oJSON.Node['strategyId'].Value)].asPtr['data']);
        //Если пользователь уже привязан к этой стратегии, добавляем подключение
        if (pSData^.usersInfo.Child[aUser] <> nil) then
        begin
          pUData := PUserData(pSData^.usersInfo.Child[aUser].asPtr['data']);
          pUData^.connectionsList.Add(Connection.Guid);
        end
        //Если не привязан добавляем пользователя и подключение
        else
        begin
          New(pUData);
          pUData^.email := aUser;
          pUData^.connectionsList := TStringList.Create;
          pUData^.connectionsList.Add(Connection.Guid);

          pSData^.usersList.Add(pUData);
          pSData^.usersInfo.NewChild(aUser).asPtr['data'] := pUData;
        end;
      end
      //Если нет, то создаем, добавляем в нее пользователя и запускаем
      else
      begin
        New(pSData);
        pSData^.strategyId := oJSON.Node['strategyId'].Value;
        pSData^.apiKey := oJSON.Node['apiKey'].Value;
        pSData^.apiSecret := oJSON.Node['apiSecret'].Value;
        pSData^.usersList := TList.Create;
        pSData^.usersInfo := TRtcInfo.Create;
        pSData^.ordersList := TList.Create;
        pSData^.ordersInfo := TRtcInfo.Create;
        pSData^.ordersGot := False;
        pSData^.positionsList := TList.Create;
        pSData^.positionsGot := False;
        pSData^.positionsInfo := TRtcInfo.Create;
        pSData^.state := 'WaitForWelcome';
        pSData^.runDate := Now;

        New(pUData);
        pUData^.email := aUser;
        pUData^.connectionsList := TStringList.Create;
        pUData^.connectionsList.Add(Connection.Guid);

        pSData^.usersList.Add(pUData);
        pSData^.usersInfo.NewChild(aUser).asPtr['data'] := pUData;

        strategiesList.Add(pSData);
        strategiesInfo.NewChild(VarToStr(oJSON.Node['strategyId'].Value)).asPtr['data'] := pSData;

        SubscribeToStrategy(pSData^.strategyId, pSData^.apiKey, pSData^.apiSecret);
      end;

      //Запоминаем данные по соединению для отсылки на клиентов
      if (connectionInfo.Child[Connection.Guid] = nil) then
        with connectionInfo.NewChild(Connection.Guid) do
        begin
          asPtr['data'] := pSData;
          asPtr['userData'] := pUData;
          asBoolean['ordersGot'] := False;
          asBoolean['positionsGot'] := False;
        end;

      SendStrategyChangeToGateway(pSData^.strategyId, 'Users', 0);
    finally
      FreeAndNil(oJSON);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.SubscribeToStrategy(strategyId, apiKey, apiSecret: String);
const
  path: String = '/realtime';
  verb: String = 'GET';
  data: String = '';
var
  nonce: String;
  hmacsignature: String;
  i: Integer;
begin
  CS.Acquire;
  try
    if (strategiesInfo.Child[strategyId] <> nil) then
    begin
      WSClient.WriteData('[1, "' + strategyId + '", "' + strategyId + '"]');
      RunCall(verb, url, path, data, apiKey, apiSecret, True, True, nonce, hmacsignature);
      WSClient.WriteData('[0, "' + strategyId + '", "' + strategyId + '", {"op": "authKeyExpires", "args":["' + apiKey + '", ' + nonce + ', "' + hmacsignature + '"]}]');
//      for i := 0 to slSymbolSubs.Count - 1 do
//        WSClient.WriteData('[0, "' + strategyId + '", "' + strategyId + '", {"op": "subscribe", "args": "'+ slSymbolSubs[i] + '"}]');

      SendStrategyChangeToGateway(strategyId, 'Add', 0);

      xLog('Strategy connected: ' + strategyId);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.UnsubscribeFromStrategy(strategyId: String);
var
  i: Integer;
begin
  CS.Acquire;
  try
    if (strategiesInfo.Child[strategyId] <> nil) then
    begin
      for i := 0 to slSymbolSubs.Count - 1 do
        try
          WSClient.WriteData('[0, "' + strategyId + '", "' + strategyId + '", {"op": "unsubscribe", "args": "'+ slSymbolSubs[i] + '"}]');
        finally
        end;
      try
        WSClient.WriteData('[2, "' + strategyId + '", "' + strategyId + '"]');
      finally
      end;

      try
        DeleteStragegyItem(strategyId);
        SendStrategyChangeToGateway(strategyId, 'Remove', 0);
      finally
      end;
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.WSServerMessage(Connection: TsgcWSConnection;
  const Text: string);
begin
  xLog('WSServerMessage: ' + Text);
end;

procedure TfServer.WSClientConnect(Connection: TsgcWSConnection);
var
  i: Integer;
  pSData: PStrategyData;
begin
  CS.Acquire;
  try
    xLog('WSClientConnect');

    //Если это переподключение, переподписываемчя на стратегии
    for i := 0 to strategiesList.Count - 1 do
    begin
      pSData := PStrategyData(strategiesList[i]);
      pSData^.lastUsed := Now; //Чтобы не отрубилось по пингу
      pSData^.ordersGot := False;
      pSData^.positionsGot := False;
    end;

    for i := 0 to WSServer.Count - 1 do
      if (connectionInfo.Child[WSServer.Connections[i].Guid] <> nil) then
      begin
        connectionInfo.Child[WSServer.Connections[i].Guid].asBoolean['ordersGot'] := False;
        connectionInfo.Child[WSServer.Connections[i].Guid].asBoolean['positionsGot'] := False;
      end;

    for i := 0 to strategiesList.Count - 1 do
      SubscribeToStrategy(pSData^.strategyId, pSData^.apiKey, pSData^.apiSecret);

    tCPThread.Resume;
  finally
    CS.Release;
  end;
end;

procedure TfServer.WSClientDisconnect(Connection: TsgcWSConnection;
  Code: Integer);
begin
  xLog('WSClientDisconnect: ' + IntToStr(Code));

  //Через секунду переподключаем все стратегии
  if tsState.State = tssOn then
    tRestart.Enabled := True;
end;

procedure TfServer.WSClientError(Connection: TsgcWSConnection;
  const Error: string);
begin
  xLog('WSClientError: ' + Error);
end;

procedure TfServer.WSClientException(Connection: TsgcWSConnection;
  E: Exception);
begin
  xLog('WSClientError: ' + E.Message);

  //Через секунду переподключаем все стратегии
  if tsState.State = tssOn then
    tRestart.Enabled := True;
end;

procedure TfServer.WSClientMessage(Connection: TsgcWSConnection;
  const Text: string);
const
  path: String = '/realtime';
  verb: String = 'GET';
  data: String = '';
var
  nonce: String;
  hmacsignature: String;
  oJSON, oJSON2: TsgcJSON;
  i, j: Integer;
  pSData: PStrategyData;
  pOData: POrderData;
  pPData: PPositionData;
  pUData: PUserData;
begin
  if cbExtendedLog.Checked then
    xLog(Text);

  CS.Acquire;
  try
    xLog('WSClientMessage: ' + Text);

    oJSON := TsgcJSON.Create(nil);
    try
      oJSON.Read(Text);

      if (oJSON.Item[0].Value = '2') then //type
      begin
        DeleteStragegyItem(oJSON.Item[1].Value);
        SendStrategyChangeToGateway(oJSON.Item[1].Value, 'Remove', 0);
      end
      else
      if (oJSON.Item[0].Value = '0') then //type
      begin
        if (oJSON.Item[3].Node['status'] <> nil) then  //Всегда если указан статус это ошибка?
        begin
//		    "error":"Access Token expired for subscription: order:*","meta":"note":"Please re-subscribe with a new token to continue receiving messages for this subscription.","status":419
//        "status":"400","error":"Missing API key.","note":""
//        "status":"400","error":"You are already subscribed to this topic: order:*","note":""
//		    "status":401,"error":"Invalid API Key.","meta":,"request":"op":"authKeyExpires","args":["e6DpBZetNRPe2fLEl4PI5aCb",1545474109535,"682f39aa3ce332ea02aa3af50185a6c9c5c991604c53ca1624b469562ca004bf"]
//        "status":401,"error":"Signature not valid.","meta":,"request":"op":"authKeyExpires","args":["8YnyEy_8L6sYp4TtWGiErINJ",1545658340798,"26931e78b9bcd4d77e18242c73c08399ff5745a932aa0e970e850200da2d2c40"]
//		    "status":403,"error":"This key is disabled.","meta":,"request":"op":"authKeyExpires","args":["e6DpBZetNRPe2fLEl4PI5aCb",1545473078869,"205f39ba7f02d8868930d474fd9c9b7c674cf51cbbe1459978384540d57d4b5d"]
//        "status":"419","error":"Access Token expired for subscription: order:*","note":""
//		    "status":429,"error":"Rate limit exceeded, retry in 1 seconds.","meta":"retryAfter":1,"request":"op":"subscribe","args":"orderBook"

          oJSON2 := TsgcJSON.Create(nil);
          try
            oJSON2.AddPair('status', VarToStr(oJSON.Item[3].Node['status'].Value));
            if oJSON.Item[3].Node['error'] <> nil then
              oJSON2.AddPair('error', VarToStr(oJSON.Item[3].Node['error'].Value))
            else
              oJSON2.AddPair('error', '');
            if oJSON.Item[3].Node['note'] <> nil then
              oJSON2.AddPair('note', VarToStr(oJSON.Item[3].Node['note'].Value))
            else
              oJSON2.AddPair('note', '');

            UnsubscribeFromStrategy(oJSON.Item[1].Value);
            DoSubscribeError(oJSON.Item[1].Value, oJSON2.Text);
            DeactivateStragegyOnGateway(oJSON.Item[1].Value, oJSON2.Text);
          finally
            FreeAndNil(oJSON2);
          end;
        end
        else
        if (oJSON.Item[3].Node['info'] <> nil) then
        begin
          if (oJSON.Item[3].Node['info'].Value = 'Welcome to the BitMEX Realtime API.') then
          begin
            if (PStrategyData(strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)]) = nil) then
              Exit;
            if (PStrategyData(strategiesList[i])^.state = 'WaitForWelcome') then
            begin
              RunCall(verb, url, path, data, PStrategyData(strategiesList[i])^.apiKey, PStrategyData(strategiesList[i])^.apiSecret, True, True, nonce, hmacsignature);
              WSClient.WriteData('[0, "' + oJSON.Item[1].Value + '", "' + oJSON.Item[1].Value + '", {"op": "authKeyExpires", "args":["' + PStrategyData(strategiesList[i])^.apiKey + '", ' + nonce + ', "' + hmacsignature + '"]}]');
              PStrategyData(strategiesList[i])^.state := 'Connected';
               for i := 0 to slSymbolSubs.Count - 1 do
                  WSClient.WriteData('[0, "' + oJSON.Item[1].Value + '", "' + oJSON.Item[1].Value + '", {"op": "subscribe", "args": "'+ slSymbolSubs[i] + '"}]');
            end;
          end;
        end
        else
        //Работа с таблицами
        if oJSON.Item[3].Node['table'] <> nil then
        begin
          //Заполнение таблицы мастер ордеров
          if (oJSON.Item[3].Node['table'].Value = 'order') then
          begin
            if (oJSON.Item[3].Node['action'].Value = 'partial') then
            begin
              xLog('OrderPartial Start');

              //Если стратегия отключена по таймауту и пришел партиал
              if (strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)] = nil) then
              begin
                UnsubscribeFromStrategy(oJSON.Item[1].Value);
                Exit;
              end;

              //Очищаем список ордеров
              pSData := PStrategyData(strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)].asPtr['data']);
              i := pSData^.ordersList.Count - 1;
              while i >= 0 do
              begin
                Dispose(POrderData(pSData^.ordersList[i]));
                i := i - 1;
              end;
              pSData^.ordersList.Clear;
              pSData^.ordersInfo.Clear;

              for i := 0 to oJSON.Item[3].Node['data'].Count - 1 do
              begin
                New(pOData);
                pOData^.symbol := oJSON.Item[3].Node['data'].Item[i].Node['symbol'].Value;
                pOData^.side := oJSON.Item[3].Node['data'].Item[i].Node['side'].Value;
                pOData^.orderQty := oJSON.Item[3].Node['data'].Item[i].Node['orderQty'].Value;
                pOData^.price := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['price'].Value);
                pOData^.ordType := oJSON.Item[3].Node['data'].Item[i].Node['ordType'].Value;
                pOData^.execInst := oJSON.Item[3].Node['data'].Item[i].Node['execInst'].Value;
                pOData^.displayQty := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['displayQty'].Value);
                pOData^.pegPriceType := oJSON.Item[3].Node['data'].Item[i].Node['pegPriceType'].Value;
                pOData^.pegOffsetValue := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['pegOffsetValue'].Value);
                pOData^.stopPx := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['stopPx'].Value);
                pOData^.timeInForce := oJSON.Item[3].Node['data'].Item[i].Node['timeInForce'].Value;
                pOData^.orderID := oJSON.Item[3].Node['data'].Item[i].Node['orderID'].Value;
                pOData^.leavesQty := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['leavesQty'].Value);
                pSData^.ordersList.Add(pOData);
                pSData^.ordersInfo.NewChild(pOData^.orderID).asPtr['data'] := pOData;
              end;

              pSData^.ordersGot := True;

              for i := 0 to pSData^.usersList.Count - 1 do
              begin
                pUData := PUserData(pSData^.usersList[i]);
                for j := 0 to pUData^.connectionsList.Count - 1 do
                  SendOrdersPartial(pUData^.connectionsList[j]);

              xLog('OrderPartial Stop');
              end;
            end
            else
            if (oJSON.Item[3].Node['action'].Value = 'insert') then
            begin
              xLog('OrderInsert Start');

              //Если стратегия отключена по таймауту и пришел партиал
              if (strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)] = nil) then
              begin
                UnsubscribeFromStrategy(oJSON.Item[1].Value);
                Exit;
              end;

              for i := 0 to oJSON.Item[3].Node['data'].Count - 1 do
              begin
                //Если ордер по маркету, то отправляем его без внесения в таблицу ордеров
                //Если лимитный то, вносим в таблицу ордеров и отправляем
                if (oJSON.Item[3].Node['data'].Item[i].Node['ordStatus'].Value = 'New') //Любой маркет ордер долден исполняться сразу без занесения в таблицу сравнения
                  and (oJSON.Item[3].Node['data'].Item[i].Node['ordType'].Value = 'Market') then //Закрытие позиции по рынку
  //                    NewOrder(JSONToStrings(oJSON.Item[3].Node['data'].Item[i]), True)
                else //Новый ордер
                begin
                  pSData := PStrategyData(strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)].asPtr['data']);
  //                if pSData^.ordersInfo.Child[VarToStr(oJSON.Item[3].Node['data'].Item[i].Node['orderID'].Value)] <> nil then
                  New(pOData);
                  pOData^.symbol := oJSON.Item[3].Node['data'].Item[i].Node['symbol'].Value;
                  pOData^.side := oJSON.Item[3].Node['data'].Item[i].Node['side'].Value;
                  pOData^.orderQty := oJSON.Item[3].Node['data'].Item[i].Node['orderQty'].Value;
                  pOData^.price := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['price'].Value);
                  pOData^.ordType := oJSON.Item[3].Node['data'].Item[i].Node['ordType'].Value;
                  pOData^.execInst := oJSON.Item[3].Node['data'].Item[i].Node['execInst'].Value;
                  pOData^.displayQty := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['displayQty'].Value);
                  pOData^.pegPriceType := oJSON.Item[3].Node['data'].Item[i].Node['pegPriceType'].Value;
                  pOData^.pegOffsetValue := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['pegOffsetValue'].Value);
                  pOData^.stopPx := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['stopPx'].Value);
                  pOData^.timeInForce := oJSON.Item[3].Node['data'].Item[i].Node['timeInForce'].Value;
                  pOData^.orderID := oJSON.Item[3].Node['data'].Item[i].Node['orderID'].Value;
                  pOData^.leavesQty := oJSON.Item[3].Node['data'].Item[i].Node['leavesQty'].Value; //leavesQty
                  pSData^.ordersList.Add(pOData);
                  pSData^.ordersInfo.NewChild(pOData^.orderID).asPtr['data'] := pOData;
                end;

                SendChangesToUsers(oJSON.Item[1].Value, oJSON.Item[3].Value);
              end;

              xLog('OrderInsert Stop');
            end
            else
            if (oJSON.Item[3].Node['action'].Value = 'update') then
            begin
              xLog('OrderUpdate Start');

              //Если стратегия отключена по таймауту и пришел партиал
              if (strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)] = nil) then
              begin
                UnsubscribeFromStrategy(oJSON.Item[1].Value);
                Exit;
              end;

              for i := 0 to oJSON.Item[3].Node['data'].Count - 1 do
              begin
                pSData := PStrategyData(strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)].asPtr['data']);

                if (pSData^.ordersInfo.Child[VarToStr(oJSON.Item[3].Node['data'].Item[i].Node['orderID'].Value)] = nil) then //Изменения рыночных ордеров
                  Continue;

                pOData := POrderData(pSData^.ordersInfo.Child[VarToStr(oJSON.Item[3].Node['data'].Item[i].Node['orderID'].Value)].asPtr['data']);

                if (oJSON.Item[3].Node['data'].Item[i].Node['ordStatus'] <> nil)
                  and ((oJSON.Item[3].Node['data'].Item[i].Node['ordStatus'].Value = 'Canceled') //Если ордер отменен
                    or (oJSON.Item[3].Node['data'].Item[i].Node['ordStatus'].Value = 'Rejected') //Если ордер отклонен
                    or ((oJSON.Item[3].Node['data'].Item[i].Node['ordStatus'].Value = 'Filled')) and (oJSON.Item[3].Node['data'].Item[i].Node['leavesQty'].Value = '0')) then //Если ордер исполнен
                begin
                  j := pSData^.ordersList.IndexOf(pOData);
                  pSData^.ordersList.Delete(j);
                  pSData^.ordersInfo.SetNil(VarToStr(oJSON.Item[3].Node['data'].Item[i].Node['orderID'].Value));
                  Dispose(pOData);
                end
                else
                begin
                  if oJSON.Item[3].Node['data'].Item[i].Node['side'] <> nil then
                    pOData^.side := oJSON.Item[3].Node['data'].Item[i].Node['side'].Value;
                  if oJSON.Item[3].Node['data'].Item[i].Node['orderQty'] <> nil then
                    pOData^.orderQty := oJSON.Item[3].Node['data'].Item[i].Node['orderQty'].Value;
                  if oJSON.Item[3].Node['data'].Item[i].Node['price'] <> nil then
                    pOData^.price := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['price'].Value);
                  if oJSON.Item[3].Node['data'].Item[i].Node['pegOffsetValue'] <> nil then
                    pOData^.pegOffsetValue := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['pegOffsetValue'].Value);
                  if oJSON.Item[3].Node['data'].Item[i].Node['stopPx'] <> nil then
                    pOData^.stopPx := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['stopPx'].Value);
                  if oJSON.Item[3].Node['data'].Item[i].Node['leavesQty'] <> nil then
                    pOData^.leavesQty := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['leavesQty'].Value);
                end;
              end;

              SendChangesToUsers(oJSON.Item[1].Value, oJSON.Item[3].Value);

              xLog('OrderUpdate Stop');
            end;
          end
          else
          if (oJSON.Item[3].Node['table'].Value = 'position') then
          begin
            if (oJSON.Item[3].Node['action'].Value = 'partial') then
            begin
              xLog('PositionPartial Start');

              //Если стратегия отключена по таймауту и пришел партиал
              if (strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)] = nil) then
              begin
                UnsubscribeFromStrategy(oJSON.Item[1].Value);
                Exit;
              end;

              //Очищаем список позиций
              pSData := PStrategyData(strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)].asPtr['data']);
              i := pSData^.positionsList.Count - 1;
              while i >= 0 do
              begin
                Dispose(PPositionData(pSData^.positionsList[i]));
                i := i - 1;
              end;
              pSData^.positionsList.Clear;
              pSData^.positionsInfo.Clear;

              //Заполняем данные позиций
              for i := 0 to oJSON.Item[3].Node['data'].Count - 1 do
              begin
                New(pPData);
                pPData^.symbol := oJSON.Item[3].Node['data'].Item[i].Node['symbol'].Value;
                pPData^.leverage := oJSON.Item[3].Node['data'].Item[i].Node['leverage'].Value;
                pPData^.crossMargin := oJSON.Item[3].Node['data'].Item[i].Node['crossMargin'].Value;
                pPData^.currentQty := oJSON.Item[3].Node['data'].Item[i].Node['currentQty'].Value;
                pPData^.avgEntryPrice := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['avgEntryPrice'].Value);
                pPData^.lastPrice := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['lastPrice'].Value);
                pPData^.markPrice := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['markPrice'].Value);
                pPData^.liquidationPrice := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['liquidationPrice'].Value);
                pPData^.realisedPnl := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['realisedPnl'].Value);
                pPData^.unrealisedPnl := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['unrealisedPnl'].Value);
                pPData^.posInit := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['posInit'].Value);
                pPData^.posMargin := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['posMargin'].Value);
                pSData^.positionsList.Add(pPData);
                pSData^.positionsInfo.NewChild(pPData^.symbol).asPtr['data'] := pPData;
              end;

              pSData^.positionsGot := True;

              for i := 0 to pSData^.usersList.Count - 1 do
              begin
                pUData := PUserData(pSData^.usersList[i]);
                for j := 0 to pUData^.connectionsList.Count - 1 do
                  SendPositionsPartial(pUData^.connectionsList[j]);
              end;

              xLog('PositionPartial Stop');
            end
            else
            if (oJSON.Item[3].Node['action'].Value = 'update') then
            begin
              xLog('PositionUpdate Start');

              //Если стратегия отключена по таймауту и пришел партиал
              if (strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)] = nil) then
              begin
                UnsubscribeFromStrategy(oJSON.Item[1].Value);
                Exit;
              end;

              for i := 0 to oJSON.Item[3].Node['data'].Count - 1 do
              begin
                if (strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)] = nil) then
                  Continue;

                pSData := PStrategyData(strategiesInfo.Child[VarToStr(oJSON.Item[1].Value)].asPtr['data']);

                if pSData^.positionsInfo.Child[VarToStr(oJSON.Item[3].Node['data'].Item[i].Node['symbol'].Value)] = nil then
                begin
                  xLog('MasterPositionUpdate: Position not found ' + oJSON.Item[3].Node['data'].Item[i].Node['symbol'].Value);
                  Continue;
                end;

                pPData := PPositionData(pSData^.positionsInfo.Child[VarToStr(oJSON.Item[3].Node['data'].Item[i].Node['symbol'].Value)].asPtr['data']);

                if oJSON.Item[3].Node['data'].Item[i].Node['leverage'] <> nil then
                  pPData^.leverage := oJSON.Item[3].Node['data'].Item[i].Node['leverage'].Value;
                if oJSON.Item[3].Node['data'].Item[i].Node['crossMargin'] <> nil then
                  pPData^.crossMargin := oJSON.Item[3].Node['data'].Item[i].Node['crossMargin'].Value;
                if oJSON.Item[3].Node['data'].Item[i].Node['currentQty'] <> nil then
                  pPData^.currentQty := oJSON.Item[3].Node['data'].Item[i].Node['currentQty'].Value;
                if oJSON.Item[3].Node['data'].Item[i].Node['avgEntryPrice'] <> nil then
                  pPData^.avgEntryPrice := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['avgEntryPrice'].Value);
                if oJSON.Item[3].Node['data'].Item[i].Node['lastPrice'] <> nil then
                  pPData^.lastPrice := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['lastPrice'].Value);
                if oJSON.Item[3].Node['data'].Item[i].Node['markPrice'] <> nil then
                  pPData^.markPrice := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['markPrice'].Value);
                if oJSON.Item[3].Node['data'].Item[i].Node['liquidationPrice'] <> nil then
                  pPData^.liquidationPrice := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['liquidationPrice'].Value);
                if oJSON.Item[3].Node['data'].Item[i].Node['realisedPnl'] <> nil then
                  pPData^.realisedPnl := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['realisedPnl'].Value);
                if oJSON.Item[3].Node['data'].Item[i].Node['unrealisedPnl'] <> nil then
                  pPData^.unrealisedPnl := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['unrealisedPnl'].Value);
                if oJSON.Item[3].Node['data'].Item[i].Node['posInit'] <> nil then
                  pPData^.posInit := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['posInit'].Value);
                if oJSON.Item[3].Node['data'].Item[i].Node['posMargin'] <> nil then
                  pPData^.posMargin := DigitCheckNull(oJSON.Item[3].Node['data'].Item[i].Node['posMargin'].Value);
              end;

              SendChangesToUsers(oJSON.Item[1].Value, oJSON.Item[3].Value);

              xLog('PositionUpdate Stop');
            end;
          end
          else
          if (oJSON.Item[3].Node['table'].Value = 'execution')
            or (oJSON.Item[3].Node['table'].Value = 'wallet') then
          begin
            SendChangesToUsers(oJSON.Item[1].Value, oJSON.Item[3].Value);
          end;
        end;
      end;
    finally
      FreeAndNil(oJSON);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.SendOrdersPartial(ConnectionGUID: String);
var
  oJSON: TsgcJSON;
  oArray: IsgcObjectJSON;
  i: Integer;
  pSData: PStrategyData;
  pOData: POrderData;
begin
  CS.Acquire;
  try
    if (connectionInfo.Child[ConnectionGUID] <> nil) //Если соединение прошло аутентификацию
      and (WSServer.ConnectionsByGUID[ConnectionGUID] <> nil) then
    begin
      pSData := PStrategyData(connectionInfo.Child[ConnectionGUID].asPtr['data']);

      if not pSData^.ordersGot then
        Exit;

      if (not connectionInfo.Child[ConnectionGUID].asBoolean['ordersGot']) then
      begin
        oJSON := TsgcJSON.Create(nil);
        try
          oJSON.AddPair('table', 'order');
          oJSON.AddPair('action', 'partial');
          oArray := oJSON.AddArray('data');
          for i := 0 to pSData^.ordersList.Count - 1 do
            with oArray.JSONObject.AddObject(IntToStr(i)).JSONObject do
            begin
              pOData := POrderData(pSData.ordersList[i]);
              AddPair('symbol', pOData^.symbol);
              AddPair('side', pOData^.side);
              AddPair('orderQty', pOData^.orderQty);
              AddPair('price', pOData^.price);
              AddPair('ordType', pOData^.ordType);
              AddPair('execInst', pOData^.execInst);
              AddPair('displayQty', pOData^.displayQty);
              AddPair('pegPriceType', pOData^.pegPriceType);
              AddPair('pegOffsetValue', pOData^.pegOffsetValue);
              AddPair('stopPx', pOData^.stopPx);
              AddPair('timeInForce', pOData^.timeInForce);
              AddPair('orderID', pOData^.orderID);
              AddPair('leavesQty', pOData^.leavesQty);
            end;

          WSServer.WriteData(ConnectionGUID, oJSON.Text);
          connectionInfo.Child[ConnectionGUID].asBoolean['ordersGot'] := True;
        finally
          FreeAndNil(oJSON)
        end;
      end;
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.SendPositionsPartial(ConnectionGUID: String);
var
  oJSON: TsgcJSON;
  oArray: IsgcObjectJSON;
  i: Integer;
  pSData: PStrategyData;
begin
  CS.Acquire;
  try
    if (connectionInfo.Child[ConnectionGUID] <> nil) //Если соединение прошло аутентификацию
      and (WSServer.ConnectionsByGUID[ConnectionGUID] <> nil) then
    begin
      pSData := PStrategyData(connectionInfo.Child[ConnectionGUID].asPtr['data']);

      if not pSData^.positionsGot then
        Exit;

      if (not connectionInfo.Child[ConnectionGUID].asBoolean['positionsGot']) then
      begin
        oJSON := TsgcJSON.Create(nil);
        try
          oJSON.AddPair('table', 'position');
          oJSON.AddPair('action', 'partial');
          oArray := oJSON.AddArray('data');
          for i := 0 to pSData^.positionsList.Count - 1 do
            with oArray.JSONObject.AddObject(IntToStr(i)).JSONObject do
            begin
              AddPair('symbol', PPositionData(pSData.positionsList[i])^.symbol);
              AddPair('leverage', PPositionData(pSData.positionsList[i])^.leverage);
              AddPair('crossMargin', PPositionData(pSData.positionsList[i])^.crossMargin);
              AddPair('currentQty', PPositionData(pSData.positionsList[i])^.currentQty);
              AddPair('avgEntryPrice', PPositionData(pSData.positionsList[i])^.avgEntryPrice);
              AddPair('lastPrice', PPositionData(pSData.positionsList[i])^.lastPrice);
              AddPair('markPrice', PPositionData(pSData.positionsList[i])^.markPrice);
              AddPair('liquidationPrice', PPositionData(pSData.positionsList[i])^.liquidationPrice);
              AddPair('realisedPnl', PPositionData(pSData.positionsList[i])^.realisedPnl);
              AddPair('unrealisedPnl', PPositionData(pSData.positionsList[i])^.unrealisedPnl);
              AddPair('posInit', PPositionData(pSData.positionsList[i])^.posInit);
              AddPair('posMargin', PPositionData(pSData.positionsList[i])^.posMargin);
            end;

          WSServer.WriteData(ConnectionGUID, oJSON.Text);
          connectionInfo.Child[ConnectionGUID].asBoolean['positionsGot'] := True;
        finally
          FreeAndNil(oJSON)
        end;
      end;
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.SendChangesToUsers(strategyId, text: String);
var
  i, j: Integer;
  pSData: PStrategyData;
  pUData: PUserData;
begin
  CS.Acquire;
  try
    if strategiesInfo.Child[strategyId] = nil then
      Exit;

    pSData := PStrategyData(strategiesInfo.Child[strategyId].asPtr['data']);
    if (pSData <> nil) then    
    begin
      if not pSData^.ordersGot then
        Exit;
        
      for i := 0 to pSData^.usersList.Count - 1 do
      begin
        pUData := PUserData(pSData^.usersList[i]);

        for j := 0 to pUData^.connectionsList.Count - 1 do
          if (connectionInfo.Child[pUData^.connectionsList[j]] <> nil) //Если соединение прошло аутентификацию
            and (WSServer.ConnectionsByGUID[pUData^.connectionsList[j]] <> nil) then
          begin
            if connectionInfo.Child[pUData^.connectionsList[j]].asBoolean['ordersGot'] then //Если до этого отправлен партиал
              WSServer.WriteData(pUData^.connectionsList[j], text);
          end;
      end;
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.WSServerConnect(Connection: TsgcWSConnection);
begin
  CS.Acquire;
  try
    try
      xLog('Connected: ' + Connection.Guid);

      if (connectionInfo.Child[Connection.Guid] = nil) then
      begin
        xLog('Do disconnect: ' + Connection.Guid);
        WSServer.ConnectionsByGUID[Connection.Guid].Close
      end;

      SendOrdersPartial(Connection.Guid);
      SendPositionsPartial(Connection.Guid);
    except
      on E: Exception do
        xLog('WSServerConnect Error: ' + E.Message);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.WSServerDisconnect(Connection: TsgcWSConnection;
  Code: Integer);
var
  pSData: PStrategyData;
  pUData: PUserData;
  i: Integer;
begin
  CS.Acquire;
  try
    xLog('Disconnected: ' + Connection.Guid);

    if (connectionInfo.Child[Connection.Guid] <> nil) then
    begin
      pSData := PStrategyData(PStrategyData(connectionInfo.Child[Connection.Guid].asPtr['data']));
      if (pSData <> nil) then
        pSData^.lastUsed := Now;

      pUData := PUserData(connectionInfo.Child[Connection.Guid].asPtr['userData']);
      if (pUData <> nil) then
      begin
        i := pUData^.connectionsList.IndexOf(Connection.Guid);
        if (i <> -1) then
          pUData^.connectionsList.Delete(i);

        //Если у пользователя нет других подключений, удаляем его
        if pUData^.connectionsList.Count = 0 then
        begin
          i := pSData^.usersList.IndexOf(pUData);
          pSData^.usersList.Delete(i);
          pSData^.usersInfo.SetNil(pUData^.email);
          FreeAndNil(pUData^.connectionsList);
          Dispose(pUdata);
        end;
      end;

      connectionInfo.SetNil(Connection.Guid);
      SendStrategyChangeToGateway('', 'Users', 0);
    end;
  finally
    CS.Release;
  end;
end;

procedure TfServer.SaveDlls;
var
  Resource: TResourceStream;
begin
  if (not FileExists(ExtractFilePath(ParamStr(0)) +  'libeay32.dll')) then
    try
      Resource := TResourceStream.Create(HInstance, 'LIBEAY32', RT_RCDATA);
      Resource.SaveToFile(ExtractFilePath(ParamStr(0)) +  'libeay32.dll');
    finally
      Resource.Free;
    end;

  if (not FileExists(ExtractFilePath(ParamStr(0)) +  'ssleay32.dll')) then
    try
      Resource := TResourceStream.Create(HInstance, 'SSLEAY32', RT_RCDATA);
      Resource.SaveToFile(ExtractFilePath(ParamStr(0)) +  'ssleay32.dll');
    finally
      Resource.Free;
    end;

  if (not FileExists(ExtractFilePath(ParamStr(0)) +  'zlib.dll')) then
    try
      Resource := TResourceStream.Create(HInstance, 'ZLIB1', RT_RCDATA);
      Resource.SaveToFile(ExtractFilePath(ParamStr(0)) +  'zlib1.dll');
    finally
      Resource.Free;
    end;
end;

initialization
  CS := TCriticalSection.Create;

finalization
  CS.Free;

end.
