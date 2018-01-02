// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://mmm-experts.com/webservices/MainService.asmx?WSDL
// Encoding : utf-8
// Version  : 1.0
// (6/17/2005 9:42:21 PM - 1.33.2.5)
// ************************************************************************ //

unit uMMMXP_MainService;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Borland types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:string          - "http://www.w3.org/2001/XMLSchema"


  ArrayOfString = array of WideString;          { "http://mmm-experts.com/webservices/" }

  // ************************************************************************ //
  // Namespace : http://mmm-experts.com/webservices/
  // soapAction: http://mmm-experts.com/webservices/%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // binding   : MainServiceSoap
  // service   : MainService
  // port      : MainServiceSoap
  // URL       : http://mmm-experts.com/webservices/MainService.asmx
  // ************************************************************************ //
  MainServiceSoap = interface(IInvokable)
  ['{1459C23B-5907-6A07-360E-B0715EA73BE7}']
    procedure Wakeup; stdcall;
    function  GetLatestVersionOf(const productName: WideString; const version: WideString): ArrayOfString; stdcall;
    function  GetLatestVersionOf2(const productName: WideString; const version: WideString; const userGUID: WideString): ArrayOfString; stdcall;
    procedure UpdateStats(const productName: WideString; const stats: WideString); stdcall;
  end;

function GetMainServiceSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): MainServiceSoap;


implementation

function GetMainServiceSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): MainServiceSoap;
const
  defWSDL = 'http://mmm-experts.com/webservices/MainService.asmx?WSDL';
  defURL  = 'http://mmm-experts.com/webservices/MainService.asmx';
  defSvc  = 'MainService';
  defPrt  = 'MainServiceSoap';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as MainServiceSoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


initialization
  InvRegistry.RegisterInterface(TypeInfo(MainServiceSoap), 'http://mmm-experts.com/webservices/', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(MainServiceSoap), 'http://mmm-experts.com/webservices/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(MainServiceSoap), ioDocument);
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfString), 'http://mmm-experts.com/webservices/', 'ArrayOfString');

end.