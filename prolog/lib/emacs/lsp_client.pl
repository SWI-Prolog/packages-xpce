/*  Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025-2026, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(lsp_client,
          [ lsp_buffer_opened/1,     % +Buffer
            ensure_lsp_server/4,     % +Buffer, +File, +Mode, -Sheet
            lsp_token_type/3,        % +LSP, ?TokenTypeNum, ?TokenTypeName
            lsp_client/2,            % ?LSPClientObj, ?Stream
            lsp_mode_module/2,
            lsp_set_document_region/3, % +RegionURI, +DocumentURI, +LineOffset
            lsp_delete_document_region/1 % +RegionURI
          ]).
:- use_module(library(pce)).
:- use_module(library(process)).
:- use_module(library(json_rpc_client)).
:- use_module(library(json_rpc_server)).
:- use_module(library(apply)).
:- use_module(library(broadcast)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(pce_util)).
:- use_module(library(uri)).
:- autoload(library(pprint)).

:- use_module(library(emacs/lsp_registry)).
:- reexport(library(emacs_extend), [lsp_mode_module/2]).

:- meta_predicate
    for_sheet(+, 2).

:- initialization
    ( unlisten(lsp_client),
      listen(lsp_client, pce_emacs(Event), lsp_event(Event))
    ).

/** <module> Basic LSP connection for PceEmacs

This module defines the basic interaction between PceEmacs and
LSP servers.  It is loaded by library(emacs/lsp_registry) when a buffer
is opened in a mode for which an LSP server is registered.  It provides:

  - The classes `lsp_workspace` and `lsp_client`
  - Hooks into `emacs_buffer` to attach zero or more LSP clients
    to a newly opened buffer.
  - Keep the LSPs in sync on changes as well as closing the buffer.

*/

%!  lsp_token_type(+LSP, ?TokenTypeNum, ?TokenTypeName)

lsp_token_type(LSP, TokenTypeNum, TokenTypeName) :-
    token_type(LSP, TokenTypeNum, TokenTypeName).


                /*******************************
                *     CLASS LSP WORKSPACE      *
                *******************************/

:- pce_begin_class(lsp_workspace, object,
                   "Represent a workspace").

:- dynamic
    lsp_workspace/2.                            % Dir, Object

variable(root,	      directory,  get, "Workspace root").
variable(lsp_clients, sheet,     none, "Mode -> chain(LSP client)").

initialise(WS, Root:root=directory) :->
    "Create a workspace from its root"::
    send_super(WS, initialise),
    send(WS, slot, root, Root),
    send(WS, slot, lsp_clients, new(sheet)),
    get(Root, path, FullDir),
    asserta(lsp_workspace(FullDir, WS)).

lookup(_Ctx, Root:directory, WS:lsp_workspace) :<-
    "Lookup existing workspace from directory"::
    get(Root, path, FullDir),
    lsp_workspace(FullDir, WS).

unlink(WS) :->
    retractall(lsp_workspace(_, WS)),
    send_super(WS, unlink).

%!  ensure_lsp_server(+Buffer, +File, +Mode, -Sheet) is semidet.
%
%   True when sheet is a mapping Id->LSPClient for the clients
%   to use for Buffer.

ensure_lsp_server(Buffer, _File, _Mode, Clients) :-
    get(Buffer, attribute, lsp_clients, Clients),
    !.
ensure_lsp_server(Buffer, File, Mode, Clients) :-
    lsp_mode_server(Mode, _),
    !,
    file_workspace(File, Mode, Workspace),
    get(Workspace, lsp_clients, Mode, Clients),
    send(Buffer, attribute, lsp_clients, Clients).

%!  file_workspace(+File, +Mode, -WorkSpace) is det.
%
%   Find or create a workspace for File in Mode.

file_workspace(File, _, WorkSpace) :-
    parent_directory(File, Parent),
    lsp_workspace(Parent, WorkSpace),
    !.
file_workspace(File, Mode, WorkSpace) :-
    project_root(File, Root, [mode(Mode)]),
    new(WorkSpace, lsp_workspace(Root)).

parent_directory(Dir, Dir).
parent_directory(Dir, Parent) :-
    file_directory_name(Dir, Direct),
    Direct \== Dir,
    parent_directory(Direct, Parent).

%   <-lsp_clients
%
%   Associate relevant LSP clients  to  a   mode.  The  LSP  clients are
%   organised in a sheet, mapping the  primary   id  to the client. This
%   allows modes to dispatch certain LSP   services  to specific clients
%   based on the client id.

lsp_clients(WS, Mode:name, Clients:sheet) :<-
    "Get or create LSP clients for mode"::
    get(WS, slot, lsp_clients, Sheet),
    (   get(Sheet, value, Mode, Clients)
    ->  true
    ;   new(Clients, sheet),
        (   lsp_create_client(WS, Mode, Id, Client),
            send(Clients, value, Id, Client),
            send(Sheet, value, Mode, Clients),
            fail
        ;   true
        )
    ).

lsp_create_client(WS, Mode, Id, LSP) :-
    get(WS?root, path, Root),
    lsp_server(Id, Root, Config),
    memberchk(Mode, Config.modes),
    (   get(WS, lsp_client, Id, LSP)
    ->  true
    ;   new(LSP, lsp_client(Id, WS,
                            Config.executable,
                            Config.get(arguments,[]))),
        forall(lsp_configure(LSP, Config), true),
        send(LSP, mode, Mode),
        send(LSP, start)
    ).

lsp_configure(LSP, Config) :-
    send(LSP, slot, config, Config).
lsp_configure(LSP, Config) :-
    send(LSP, slot, change, Config.get(change)).

lsp_client(WS, Id:id=name, Client:lsp_client) :<-
    "Find client from id"::
    get(WS, slot, lsp_clients, ModeSheet),          % Mode -> Sheet
    get_chain(ModeSheet, members, Attributes),
    member(Att, Attributes),
    get(Att, value, ClientSheet),                   % Id -> lsp_client
    get(ClientSheet, value, Id, Client).

:- pce_end_class.



                /*******************************
                *       CLASS LSP CLIENT       *
                *******************************/

%!  lsp_client(?LSPClientObj, ?Stream)

:- dynamic
    lsp_client/2.                               % ?Client, ?Stream

:- at_halt(disconnect_lsps).

disconnect_lsps :-
    forall(retract(lsp_client(LSP, _Stream)),
           send(LSP, free)).

:- pce_begin_class(lsp_client(id), object,
                   "Connect to an LSP server").

variable(id,		name,          get,  "LSP identifier").
variable(mode,          name,	       both, "Mode it was created for").
variable(workspace,	lsp_workspace, get,  "Workspace root").
variable(program,	prolog,	       get,  "LSP excutable").
variable(arguments,	vector,        get,  "LSP excutable arguments").
variable(connection,	prolog*,       get,  "The connecting stream").
variable(initialized,   bool := @off,  get,  "LSP is ready").
variable(pending,	chain*,        get,  "Pending actions").
variable(change,	'1..2' := 2,   both, "How to send changes").
variable(config,	prolog*,       get,  "Dict holding configuration").

initialise(LSP, Id:name, Workspace:workspace=lsp_workspace,
           Program:program=prolog, Argv:arguments=[vector]) :->
    send_super(LSP, initialise),
    send(LSP, slot, id, Id),
    send(LSP, slot, workspace, Workspace),
    send(LSP, slot, program, Program),
    default(Argv, vector, TheArgv),
    send(LSP, slot, arguments, TheArgv).

unlink(LSP) :->
    clean_capabilities(LSP),
    send(LSP, disconnect),
    send_super(LSP, unlink).

start(LSP) :->
    "Connect and initialize"::
    send(LSP, connect),
    send(LSP, init).

connect(LSP) :->
    "Start the LSP server"::
    get(LSP, program, Prog),
    get_object(LSP, arguments, Vector),
    Vector =.. [vector|Argv],
    (   compound(Prog)
    ->  Exe = Prog
    ;   is_absolute_file_name(Prog)
    ->  Exe = Prog
    ;   Exe = path(Prog)
    ),
    process_create(Exe,
                   Argv,
                   [ stdin(pipe(In)),
                     stdout(pipe(Out)),
                     detached(true)
                   ]),
    stream_pair(Stream, Out, In),
    send(LSP, slot, connection, Stream),
    client_thread_alias(LSP, Alias),
    json_full_duplex(Stream,
                     [ header(true),
                       thread_alias(Alias)
                     ]),
    asserta(lsp_client(LSP, Stream)).


client_thread_alias(LSP, Alias) :-
    get(LSP, id, Id),
    get(LSP?workspace?root, path, Root),
    format(atom(Alias), '~w@~w', [Id, Root]).

disconnect(LSP) :->
    "Stop the connection"::
    (   get(LSP, connection, Stream),
        is_stream(Stream)
    ->  retractall(lsp_client(LSP, _)),
        ignore(get(LSP, call, shutdown, _Reply)), % Reply should be `null`
        ignore(send(LSP, notify, exit)),
        close(Stream, [force(true)]),
        send(LSP, slot, connection, @nil)
    ;   true
    ).

call(LSP, Message:message=prolog, Options:options=[prolog],
     Result:prolog) :<-
    "Make a JSON RPC call"::
    default(Options, [], TheOptions),
    (   (   get(LSP, initialized, @on)
        ;   Message = initialize(_)
        )
    ->  true
    ;   debug(lsp(init), "Before initialized: calling ~p", [Message])
    ),
    get(LSP, connection, Stream),
    catch(json_call(Stream, Message, Result,
                    [ header(true)
                    | TheOptions
                    ]),
          Error,
          rpc_error(Message, Error)).

notify(LSP, Message:message=prolog) :->
    "Send a JSON RPC notification"::
    (   (   get(LSP, initialized, @on)
        ;   Message = initialized(_)
        )
    ->  true
    ;   debug(lsp(init), "Before initialized: notifying ~p", [Message])
    ),
    get(LSP, connection, Stream),
    catch(json_notify(Stream, Message,
                      [ header(true)
                      ]),
          Error,
          rpc_error(Message, Error)).

rpc_error(Message, Error) :-
    print_message(error, lsp(rpc_error(Message, Error))),
    fail.

init(LSP) :->
    "Initialize the LSP connection for a directory"::
    get(LSP?workspace?root, path, Dir),
    uri_file_name(URI, Dir),
    current_prolog_flag(pid, PID),
    get(LSP, change, Change), % 2=incremental, 1=full
    get(LSP, call,
        initialize(
            #{ capabilities:
                 #{ workspace:
                      #{ configuration: true
                       },
                    textDocument:
                      #{ synchronization:
                           #{ openClose: true,
                              didSave: true,
                              change: Change
                            },
                         publishDiagnostics:
                           #{ formats: ["plaintext"]
                            },
                         semanticTokens:
                           #{ dynamicRegistration: false,
                              requests:
                                #{ full: true,
                                   range: false
                                 },
                              tokenTypes: [],
                              tokenModifiers: [],
                              formats: ["relative"]
                            }
                       },
                    window:
                      #{ workDoneProgress: true
                       }
                  },
               rootUri: URI,
               processId: PID
             }),
        [ async(initialized(LSP))
        ],
        _Reply),
    debug(lsp(init), 'Sent initialized(~p)', [LSP]).

:- public initialized/2.
initialized(LSP, Result) :-
    debug(lsp(init), 'initialized(~p)', [LSP]),
    send(LSP, initialized, Result).

initialized(LSP, Result:prolog) :->
    "Call back after LSP is ready"::
    (   debugging(lsp(capabilities))
    ->  print_term(Result, [nl(true)])
    ;   true
    ),
    clean_capabilities(LSP),
    catch_with_backtrace(
        register_capabilities(LSP, Result.capabilities),
        E,
        print_message(error, E)),
    send(LSP, notify, initialized(#{})),
    send(LSP, slot, initialized, @on),
    % needs to be called in next event cycle
    new(T, timer(0.1,
                 and(message(LSP, send_pending_actions),
                     message(@receiver, free)))),
    send(T, start, once),
    send(T, lock_object, @on).

:- dynamic
    token_type/3,                         % LSP, TypeId, TypeName
    token_modifier/3,                     % LSP, ModId,  ModName
    capabilities/2.                       % LSP, Dict

clean_capabilities(LSP) :-
    retractall(token_type(LSP,_,_)),
    retractall(token_modifier(LSP,_,_)),
    retractall(capabilities(LSP, _)).

register_capabilities(LSP, Capabilities) :-
    asserta(capabilities(LSP, Capabilities)),
    SemanticTokenProvider = Capabilities.get(semanticTokensProvider),
    !,
    register_token_types(LSP, SemanticTokenProvider.legend).
register_capabilities(_LSP, _Capabilities).

register_token_types(LSP, Legend) :-
    forall(nth0(Id, Legend.tokenTypes, String),
           ( atom_string(TokenType, String),
             assertz(token_type(LSP, Id, TokenType)))),
    forall(nth0(Id, Legend.tokenModifiers, String),
           ( atom_string(TokenModifier, String),
             assertz(token_modifier(LSP, Id, TokenModifier)))).

%   <-modifiers
%
%   Translate the token modifier mask into a list of modifier names.

modifiers(LSP, Mask:mask=int, Modifiers:prolog) :<-
    "Translate a modifier mask to a list of names"::
    token_mask_modifiers(LSP, Mask, 0, Modifiers).

:- det(token_mask_modifiers/4).
token_mask_modifiers(_, 0, _, []) :-
    !.
token_mask_modifiers(LSP, Mask, ModID, List) :-
    Bit is 1<<ModID,
    ModID1 is ModID+1,
    (   Mask /\ Bit =\= 0,
        token_modifier(LSP, ModID, H)
    ->  Mask1 is Mask /\ \Bit,
        List = [H|T],
        token_mask_modifiers(LSP, Mask1, ModID1, T)
    ;   token_mask_modifiers(LSP, Mask,  ModID1, List)
    ).

code_action_kinds(LSP, Kinds:prolog) :<-
    "Get supported codeActionKinds as a list"::
    capabilities(LSP, Dict),
    Provider = Dict.get(codeActionProvider),
    is_dict(Provider),
    Kinds = Provider.get(codeActionKinds).

did_open(LSP, Buffer:emacs_buffer) :->
    "Buffer was opened"::
    (   get(LSP, initialized, @on)
    ->  send(LSP, notify_did_open, Buffer)
    ;   send(LSP, register_pending,
             and(message(LSP, notify_did_open, Buffer),
                 message(Buffer, colourise)))
    ).

register_pending(LSP, Message:code) :->
    "Register an action to be called when LSP is initialized"::
    (   get(LSP, pending, Chain),
        Chain \== @nil
    ->  send(Chain, append, Message)
    ;   send(LSP, slot, pending, chain(Message))
    ).

send_pending_actions(LSP) :->
    "Send pending didOpen()"::
    (   get(LSP, pending, Chain),
        Chain \== @nil
    ->  send(Chain, for_all, message(@arg1, execute)),
        send(LSP, slot, pending, @nil)
    ;   true
    ).

notify_did_open(LSP, Buffer:emacs_buffer) :->
    "Send textDocument/didOpen()"::
    get(Buffer, attribute, lsp_tracking, URI),
    get(Buffer, contents, string(Content)),
    string_length(Content, ContentLength),
    Version = 1,
    send(Buffer, attribute, lsp_version, Version),
    get(Buffer, mode, Mode),
    debug(lsp(file), 'Sending didOpen() for ~p ~p (~D chars)',
          [Buffer, URI, ContentLength]),
    send(LSP, notify,
         'textDocument/didOpen'(
             #{textDocument:
                 #{ uri: URI,
                    languageId: Mode,
                    version: Version,
                    text: Content
                  }
              })).


%   ->lsp_execute_command(+Command) is det.
%
%   Send a request to execute Command. While  this is a JSON RPC request
%   and must have an `id`, it is  normally not answered. The async(true)
%   option ensures we are not waiting for a response.

execute_command(LSP, Command:command=prolog, Args:arguments=prolog) :->
    "Execute a command on the workspace"::
    (   Command == "pce_emacs.edit"
    ->  lsp_apply_edits(Args)
    ;   get(LSP, call,
            'workspace/executeCommand'(
                #{ command: Command,
                   arguments: Args
                 }),
            [async(true)],
            _NoReply)
    ).

:- pce_end_class(lsp_client).


                /*******************************
                *            SERVER            *
                *******************************/

:- json_method
    'textDocument/publishDiagnostics'(
        #{ parameters:
             #{ diagnostics: true
              }
         }),
    'workspace/configuration'(
        #{ parameters:
             #{ items: true
              }
         }) : true,
    'workspace/applyEdit'(
        #{ parameters:
             #{ edit: true
              }
         }) : true,
    'window/workDoneProgress/create'(
        #{ parameters:
             #{ token: true
              }
         }) : true,
    'window/logMessage'(
        #{ parameters:
             #{ message: true,
                type: true
              }
         }),
    'window/showMessage'(
        #{ parameters:
             #{ message: true,
                type: true
              }
         }),
    '$/progress'(
        #{ parameters:
             #{}
         }).

:- dynamic
    document_region/3.		% RegionURI, DocumentURI, LineOffset

%!  lsp_set_document_region(+RegionURI, +DocumentURI, +Fragment)
%
%   Register that we use  RegionURI  to   represent  a  sub-document  of
%   DocumentURI that starts at line LineOffset.  This feature is used to
%   use LSPs that can only process whole documents to get diagnostics on
%   a file region.

lsp_set_document_region(RegionURI, DocumentURI, Fragment) :-
    retractall(document_region(RegionURI, _, _)),
    asserta(document_region(RegionURI, DocumentURI, Fragment)).

%!  lsp_delete_document_region(+RegionURI) is det.
%
%   Unregister the region.

lsp_delete_document_region(RegionURI) :-
    retractall(document_region(RegionURI, _, _)).

%!  lsp_calling(-LSP) is det.
%
%   True when the method is called from LSP.

:- det(lsp_calling/1).
lsp_calling(LSP) :-
    nb_current(json_rpc_stream, Stream),
    lsp_client(LSP, Stream).

%!  uri_buffer(+URI:string, -Buffer:emacs_buffer,
%!             -Region:lsp_region_fragment)
%
%   True when Buffer is the PceEmacs buffer associated with URI.

uri_buffer(URIs, Buffer, Region) :-
    atom_string(RegionURI, URIs),
    document_region(RegionURI, DocumentURI, Region),
    !,
    uri_buffer(DocumentURI, Buffer, _).
uri_buffer(URIs, Buffer, @nil) :-
    uri_file_name(URIs, File),
    get(@emacs, file_buffer, File, Buffer),
    get(Buffer, attribute, lsp_tracking, URI),
    atom_string(URI, URIs).

%!  'textDocument/publishDiagnostics'(+Data)
%
%   Sent after we  send  a  didOpen   or  didChange  notification.  This
%   contains (warning) messages.

'textDocument/publishDiagnostics'(Data) :-
    (   debugging(lsp(diagnostics))
    ->  print_term(Data, [nl(true)])
    ;   true
    ),
    lsp_calling(LSP),
    #{ uri:URI, diagnostics: Diagnostics } :< Data,
    uri_buffer(URI, Buffer, Region),
    !,
    send(Buffer, lsp_publish_diagnostics, LSP, Diagnostics, Region).
'textDocument/publishDiagnostics'(_).

%!  'workspace/configuration'(+Data, -Result) is det.
%
%   Perform dynamic workspace configuration. Data  is   a  list  of JSON
%   objects holding a `section` key. We must   return  a list of objects
%   for each configuration  section.  For   unknown  sections  we return
%   `#{}`.
%
%   @tbd Must be implemented by the mode

'workspace/configuration'(Data, Result) :-
    lsp_calling(LSP),
    debug(lsp(workspace),
          'workspace/configuration(~@) for ~p',
          [print_term(Data, [output(current_output)]), LSP]),
    maplist(lsp_ws_configuration(LSP), Data.items, Result),
    debug(lsp(workspace),
          '--> ~@',
          [print_term(Result, [output(current_output)])]).


lsp_ws_configuration(LSP, Item, Config) :-
    get(LSP, mode, Mode),
    lsp_mode_module(Mode, Module),
    current_predicate(Module:lsp_configuration/2),
    Module:lsp_configuration(Item, Config),
    !.
lsp_ws_configuration(_LSP, _, #{}).

%!  'workspace/applyEdit'(+Data, -Result) is det.
%
%   Act on server initiated workspace edits.

'workspace/applyEdit'(Data, Result) :-
    (   debugging(lsp(edit))
    ->  print_term(Data, [nl(true)])
    ;   true
    ),
    (   catch(lsp_apply_edits(Data.edit), Error, true)
    ->  (   var(Error)
        ->  Result = #{applied: true}
        ;   message_to_string(Error, Msg),
            Result = #{ applied: false,
                        failureReason: Msg
                      }
        )
    ;   Result = #{ applied: false,
                    failureReason: "Failed"
                  }
    ).

%!  lsp_apply_edits(+Data)
%
%   Apply a set of edits.  Data comes in various formats.
%
%     - clangd
%       Sends a dict mapping URIs to change sets for that the specified
%       file.
%     - ltex-ls
%       A list of #{edits: ChangeSet,
%                   textDocument: #{uri: URI, version: Version}}

lsp_apply_edits(Data),
    is_dict(Data),
    dict_pairs(Data.get(changes), _, Pairs),
    maplist(file_change_set, Pairs, ChangePairs) =>
    maplist(apply_edit, ChangePairs).
lsp_apply_edits(Data),
    is_list(Data),
    maplist(file_change_set, Data, ChangePairs) =>
    maplist(apply_edit, ChangePairs).

file_change_set(URI-ChangeSet, URI-ChangeSet) :-
    uri_is_global(URI), is_list(ChangeSet),
    maplist(is_change, ChangeSet),
    !.
file_change_set(Dict, URI-ChangeSet) :-
    is_dict(Dict),
    #{ edits: ChangeSet, textDocument: Document } :< Dict,
    is_dict(Document),
    #{ uri: URI} :< Document.

is_change(Change) :-
    is_dict(Change),
    #{ newText: _, range: Range } :< Change,
    #{ start:_, end:_ } :< Range.

%!  apply_edit(+Pair) is det.
%
%   Pair is `URI-ChangeSet`, representing the changes for URI.

apply_edit(FileURI-Changes) :-
    uri_buffer(FileURI, Buffer, _),
    apply_buffer_changes(Buffer, Changes).
apply_edit(FileURI-Changes) :-
    uri_file_name(FileURI, File),
    new(Buffer, emacs_buffer(File)),
    apply_buffer_changes(Buffer, Changes).

%!  apply_buffer_changes(+Buffer, +Changes) is det.
%
%   Apply a set of edits. Each edit  has a `range` and `newText`. Ranges
%   are against the original document. We turn   the request into a dict
%   in PceEmacs coordinates, sort them and apply them backwards from the
%   end.

apply_buffer_changes(Buffer, Changes) :-
    in_pce_thread_sync(apply_buffer_changes_(Buffer, Changes)).

apply_buffer_changes_(Buffer, Changes) :-
    maplist(change_description(Buffer), Changes, Descriptions),
    sort(offset, >=, Descriptions, Ordered),
    maplist(apply_change(Buffer), Ordered),
    send(Buffer, mark_undo),
    broadcast(pce_emacs(changed(Buffer))),
    new(T, timer(0.1,
                 and(message(Buffer, auto_colourise),
                     message(@receiver, free)))),
    send(T, start, once),
    send(T, lock_object, @on).

change_description(Buffer, Change,
                   #{offset: StartOffset, length: Length, text:String}) :-
    #{ newText: String, range: Range } :< Change,
    #{ start:Start, end:End } :< Range,
    lsp_offset(Start, Buffer, StartOffset),
    lsp_offset(End, Buffer, EndOffset),
    Length is EndOffset-StartOffset.

apply_change(Buffer, #{offset: Start, length: Length, text:String}) :-
    debug(lsp(edit), '~p: ~p[~p] = ~p',
          [Buffer, Start, Length, String]),
    send(Buffer, delete, Start, Length),
    send(Buffer, insert, Start, String).

lsp_offset(#{line:Line, character:Char}, Buffer, Offset) =>
    get(Buffer, lsp_offset, Line, Char, Offset).

%!  'window/workDoneProgress/create'(+Data, -Reply) is det.
%
%   Used by the server to validate we process progress reports.

'window/workDoneProgress/create'(_Data, null).

%!  'window/logMessage'(+Data) is det.

:- det('window/logMessage'/1).
'window/logMessage'(Data) :-
    #{type: Type,		% 1: error, 2: warning, 3:info, 4: log
      message: Message} :< Data,
    message_level(Type, Kind),
    print_message(Kind, lsp(log(Message))).

%!  'window/showMessage'(+Data)
%
%   @tbd Should use ->report to the current PceEmacs window.

:- det('window/showMessage'/1).
'window/showMessage'(Data) :-
    #{type: Type,		% 1: error, 2: warning, 3:info, 4: log
      message: Message} :< Data,
    message_level(Type, Kind),
    print_message(Kind, lsp(show(Message))).

message_level(1, error).
message_level(2, warning).
message_level(3, informational).
message_level(4, debug).

%!  '$/progress'(+Data) is det.

'$/progress'(Data) :-
    (   debugging(lsp(progress))
    ->  print_term(Data, [nl(true)])
    ;   true
    ).

                /*******************************
                *           CONNECT            *
                *******************************/

:- discontiguous
    lsp_event/1.

%!  lsp_buffer_opened(+Buffer) is semidet.
%
%   Connect Buffer to the LSP servers for its mode.  Fails if there are
%   none, or Buffer has no file.

lsp_buffer_opened(Buffer) :-
    get(Buffer, mode, Mode),
    get(Buffer, file, File),
    File \== @nil,
    get(File, path, Path),
    ensure_lsp_server(Buffer, Path, Mode, Clients),
    \+ send(Clients?members, empty),
    uri_file_name(URI, Path),
    debug(lsp(file), 'Opened ~p', [URI]),
    send(Buffer, attribute, lsp_tracking, URI),
    send(Buffer, lsp_changes, @on),
    for_sheet(Clients,
              document_open(Buffer)).

document_open(Buffer, _LSPId, LSP) :-
    send(LSP, did_open, Buffer).

lsp_event(closed(Buffer)) :-
    get(Buffer, attribute, lsp_tracking, URI),
    debug(lsp(file), 'Closed ~p', [URI]),
    get(Buffer, attribute, lsp_clients, Clients),
    for_sheet(Clients,
              document_close(URI)).

document_close(URI, _Id, LSP) :-
    send(LSP, notify,
         'textDocument/didClose'(
             #{textDocument:
                 #{ uri: URI
                  }
              })).

%   changed(+Buffer)
%
%   Triggered on each ->mark_undo when editing  as well as on background
%   changes such as ->reload or workspace edits. LSP defines two levels
%   for handling:
%
%     - 1: send full text
%     - 2: send incremental changes
%
%   The method text_buffer<-lsp_changes returns a  chain of xpce objects
%   describing incremental changes or @nil if there are too many changes
%   for incremental handling. In  the  latter   case  we  send the whole
%   buffer and re-initialize incremental change tracking.
%
%   LSP servers may process incremental changes or   not. If it does not
%   process incremental changes we  should  only   send  the  buffer  on
%   specific actions, e.g., saving or Ctrl-L. Note   that we can ask for
%   <-lsp_changes only once.

lsp_event(changed(Buffer)) :-
    get(Buffer, attribute, lsp_tracking, URI),
    get(Buffer, attribute, lsp_clients, Clients),
    change_levels(Clients, Levels),
    send_changes(Levels, Buffer, URI, Clients).

send_changes(Levels, Buffer, URI, Clients) :-
    memberchk(2, Levels),
    !,
    get(Buffer, lsp_changes, Changes),
    get(Buffer, lsp_increment_version, Version),
    (   Changes == @nil
    ->  get(Buffer, contents, string(Content)),
        JSONChanges = [ #{text: Content} ],
        debug(lsp(change), 'Sending whole buffer for ~p', [Buffer]),
        send(Buffer, lsp_changes, @on)      % re-enable incremental
    ;   chain_list(Changes, ChangeList),
        maplist(to_json, ChangeList, JSONChanges),
        debug(lsp(change), '~p: changes: ~@',
              [ Buffer,
                print_term(JSONChanges, [output(current_output)])
              ])
    ),
    for_sheet(Clients, document_changed(URI, Version, JSONChanges)).
send_changes(_Levels, _Buffer, _URI, _Clients).

%!  change_levels(+Clients:sheet, -Levels:list) is det.

change_levels(Clients, Levels) :-
    new(L, chain),
    send(Clients, for_all, message(L, add, @arg1?value?change)),
    chain_list(L, Levels).

document_changed(URI, Version, JSONChanges, _Id, LSP) :-
    get(LSP, change, 2),
    !,
    send(LSP, notify,
         'textDocument/didChange'(
             #{ textDocument:
                  #{ uri: URI,
                     version: Version
                   },
                contentChanges: JSONChanges
              })).
document_changed(_URI, _Version, _JSONChanges, _Id, _LSP).

to_json(Change, #{ range: #{ start: #{line: SL, character: SP},
                             end: #{line: EL, character: EP}
                           },
                   rangeLength: ULen,
                   text: Text
                 }) :-
    object(Change, text_change(SL, SP, EL, EP, TextObj)),
    get(Change, length, ULen),
    (   TextObj == @nil
    ->  Text = ""
    ;   object(TextObj, string(Text))
    ).

lsp_event(saved(Buffer)) :-
    get(Buffer, attribute, lsp_tracking, URI),
    get(Buffer, attribute, lsp_clients, Clients),
    for_sheet(Clients, document_saved(Buffer, URI)).

document_saved(Buffer, URI, _Id, LSP) :-
    debug(lsp(file), 'Saved ~p', [URI]),
    get(Buffer, contents, string(Content)),
    send(LSP, notify,
         'textDocument/didSave'(
             #{ textDocument:
                  #{ uri: URI
                   },
                text: Content
              })).


                /*******************************
                *     EXTEND EMACS BUFFER      *
                *******************************/

:- emacs_extend_mode(language,
                     []).

class_variable(lsp_roles, sheet*, @nil,
               "Mapping from role to LSP id; @nil: use lsp_server/2").

:- pce_group(lsp).

%   ->lsp_setup()
%
%   Connect to an LSP if possible. Fails  if   no  LSP  can be found. If
%   `role` is @default, try to connect to any LSP.

lsp_setup(M, Role:role=[name]) :->
    "Connect to LSP servers"::
    get(M, lsp_from_role, Role, _LSPId),
    (   get(M, lsp_client, Role, _LSP)
    ->  true
    ;   get(M, text_buffer, Buffer),
        lsp_buffer_opened(Buffer),
        get(M, lsp_client, Role, _LSP)
    ).

lsp_from_role(M, Role:[name], LSPId:name) :<-
    "Get LSP id that serves some role"::
    get(M, class_variable_value, lsp_roles, Sheet),
    (   Sheet \== @nil
    ->  (   Role == @default
        ->  get(Sheet, '_arg', 1, attribute(_Role, LSPId))
        ;   get(Sheet, value, Role, LSPId)
        )
    ;   get(M?text_buffer, mode, Mode),
        lsp_role_server(Mode, Role, LSPId)
    ->  true
    ).

%   <-lsp_client
%
%   Find the LSP client to implement a specific role for this mode.

lsp_client(M, Role:[name], LSP:lsp_client) :<-
    "Get LSP client in some role for this buffer"::
    get(M, text_buffer, TB),
    get(TB, attribute, lsp_clients, Clients),
    (   Role == @default
    ->  get(Clients, '_arg', 1, attribute(_Role, LSP))
    ;   get(M, lsp_from_role, Role, LSPId),
        get(Clients, value, LSPId, LSP)
    ).

lsp_position(M, For:for=[int], Pos:prolog) :<-
    "Get LSP compatible position"::
    get(M, text_buffer, TB),
    get(TB, attribute, lsp_tracking, URI),
    (   For == @default
    ->  get(M, caret, Offset)
    ;   Offset = For
    ),
    get(TB, line_number, Offset, Line1),
    Line is Line1 - 1,
    get(TB, lsp_column, Offset, Col),
    Pos = #{ textDocument: #{ uri: URI },
             position: #{line:Line, character:Col}
           }.

on_symbol(M) :->
    "True if caret is on a symbol"::
    get(M, caret, Caret),
    (   Pos = Caret
    ;   Pos is Caret - 1
    ),
    get(M, character, Pos, Char),
    char_type(Char, alnum),
    !.

%   ->lsp_send_change_full
%
%   Send a level 1 change message  to   all  change  level 1 LSP clients
%   connected to this mode. Fails if there   are  no such clients or the
%   buffer was not changed since our latest message.

lsp_send_change_full(M) :->
    "Send changes to change level 1 clients"::
    get(M, text_buffer, Buffer),
    get(Buffer, attribute, lsp_tracking, URI),
    get(Buffer, attribute, lsp_clients, Clients),
    change_levels(Clients, Levels),
    memberchk(1, Levels),
    get(Buffer, generation, Gen),
    \+ get(Buffer, attribute, lsp_change_full_generation, Gen),
    send(Buffer, attribute, lsp_change_full_generation, Gen),
    get(Buffer, lsp_increment_version, Version),
    get(Buffer, contents, string(Content)),
    for_sheet(Clients, send_change_full(URI, Version, Buffer, Content)).

send_change_full(URI, Version, Buffer, Content, _Id, LSP) :-
    get(LSP, change, 1),
    !,
    debug(lsp(change), 'Sending full change for ~p@~p to ~p',
          [Buffer, Version, LSP]),
    send(LSP, notify,
         'textDocument/didChange'(
             #{ textDocument:
                  #{ uri: URI,
                     version: Version
                   },
                contentChanges: [ #{text: Content} ]
              })).

:- emacs_end_mode.


                /*******************************
                *      BUFFER EXTENSIONS       *
                *******************************/

:- pce_extend_class(emacs_buffer).

lsp_increment_version(Buffer, Version:int) :<-
    "Increment and return current version"::
    get(Buffer, attribute, lsp_version, Version0),
    Version is Version0+1,
    send(Buffer, attribute, lsp_version, Version).

:- pce_end_class.


                /*******************************
                *             UTIL             *
                *******************************/

%!  for_sheet(+Sheet, :Action) is semidet.
%
%   Call call(Action, Name, Value) for each attribute in Sheet.

for_sheet(Sheet, Action) :-
    get(Sheet, '_arity', Count),
    for_sheet_loop(1, Count, Sheet, Action).

for_sheet_loop(I, Count, Sheet, Action) :-
    I =< Count,
    !,
    get(Sheet, '_arg', I, attribute(Name, Value)),
    once(call(Action, Name, Value)),
    I2 is I+1,
    for_sheet_loop(I2, Count, Sheet, Action).
for_sheet_loop(_I, _Count, _Sheet, _Action).


                /*******************************
                *           MESSAGES           *
                *******************************/

:- multifile
    prolog:message//1.

prolog:message(lsp(Msg)) -->
    lsp_message(Msg).

lsp_message(log(Message)) -->
    [ '~s'-[Message] ].
lsp_message(show(Message)) -->
    [ 'TODO: show ~s'-[Message] ].
lsp_message(rpc_error(Message, Error)) -->
    { message_to_string(Error, ErrorMsg) },
    [ '~p: ~s'-[Message, ErrorMsg] ].
