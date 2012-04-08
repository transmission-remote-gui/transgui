unit URLListenerOSX;
{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, CocoaAll, InternetConfig, AppleEvents;

type
  THandlerProc = procedure(const url: string);

  { TAppURLHandler }
  
  TAppURLHandler = objcclass(NSObject)
  public
    procedure  getUrlwithReplyEvent(event: NSAppleEventDescriptor; eventReply: NSAppleEventDescriptor); message 'getUrl:withReplyEvent:';
  public
    callBack: THandlerProc;
  end;

procedure RegisterURLHandler(HandlerProc: THandlerProc);
var
  handler : TAppURLHandler;
  eventManager: NSAppleEventManager;
 
implementation

{ TAppURLHandler }

procedure TAppURLHandler.getUrlwithReplyEvent(event: NSAppleEventDescriptor; eventReply: NSAppleEventDescriptor);
var
  url : NSString;
begin
  url:=event.paramDescriptorForKeyword(keyDirectObject).stringValue;
  callBack(url.cString);
end;

procedure RegisterURLHandler(HandlerProc: THandlerProc);
begin
  handler:=TAppURLHandler.alloc.init;
  handler.callBack:=HandlerProc;
  eventManager:=NSAppleEventManager.sharedAppleEventManager;
  eventManager.setEventHandler_andSelector_forEventClass_andEventID(handler,ObjCSelector(handler.getUrlwithReplyEvent), kInternetEventClass,kAEGetURL);
end;

end.
