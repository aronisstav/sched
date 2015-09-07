-module(sched_event).

%%%=============================================================================

-export([to_string/1]).

%%%=============================================================================

-export_type([event/0]).

%%%=============================================================================
%%% EXPORTED TYPES
%%%=============================================================================

-

-record(event, {
          actor            :: actor(),
          annotations = [] :: [event_annotation()],
          effects = []     :: [effect()],
          event_info       :: event_info()
         }).

-opaque event() :: #event{}.

%%%=============================================================================
%%% EXPORTED FUNCTIONS
%%%=============================================================================

-spec to_string(event()) -> string().

to_string(Event) ->
  io_lib:format("~p", [Event]).

%%%=============================================================================
%%% INTERNAL TYPES
%%%=============================================================================

-type actor() ::
        {'channel', {pid(), pid()}} |
        {   'port',         port()} |
        {'process',          pid()} |
        {  'timer',    reference()}.

-type event_annotation() ::
        location_ann().

-type effect() ::
        {          'link', [pid_or_port(), pid_or_port()]} |
        {'deliver_signal', pid_or_port()

-type event_info() ::
        {    'bif',     bif_info()} |
        {   'exit',    exit_info()} |
        {'receive', receive_info()}.

%%%-----------------------------------------------------------------------------

-record(bif_info, {
          annotations = [] :: [bif_annotation()],
          bif              :: mfa(),
          result = unknown :: bif_result()
         }).

-type bif_info() :: #bif_info{}.

-type bif_annotation() ::

-type bif_result() ::
        'unknown' |
        {'ok', term()} |
        {'exception', exception()}.

-record(exception, {
          class      :: 'error' | 'exit' | 'throw',
          reason     :: term(),
          stacktrace :: stacktrace()
         }).

-type exception() :: #exception{}.

%%%-----------------------------------------------------------------------------

-record(exit_info, {
          links = []                :: [pid()],
          monitors = []             :: [{reference(), pid()}],
          reason = 'normal'         :: term(),
          name = ?process_name_none :: ?process_name_none | atom(),
          stacktrace = []           :: [term()],
          status = running          :: running | waiting,
          trapping = false          :: boolean()
         }).

%%%-----------------------------------------------------------------------------

-record(receive_info, {
          message            :: {'message', signal('message')} | 'after',
          pattern_fun        :: pattern_fun(),
          timeout = infinity :: timeout(),
          trap_exit          :: boolean()
         }).

-type receive_info() :: #receive_info{}.

-record(signal, {
          data             :: term(),
          id = make_ref()  :: signal_id(),
          type = 'message' :: signal_type()
         }).

-type signal(T) :: #signal{type = T}.

-type pattern_fun() :: fun((term()) -> boolean()).

%%%-----------------------------------------------------------------------------
%%% Annotations
%%%-----------------------------------------------------------------------------

-type location_ann()   :: {'location', location()}.
-type message_id_ann() :: {'message_id', message_id()}.
-type monitor_ann()    :: {'monitor', monitor_info()}.

%%%-----------------------------------------------------------------------------
%%% Miscellaneous types
%%%-----------------------------------------------------------------------------

-type location()    :: term(). %% XXX Specify.
-type signal_id()   :: reference().
-type signal_type() ::
        'demonitor' |
        'exit' |
        'group_leader' |
        'info' |
        'kill' |
        'link' |
        'message' |
        'monitor' |
        'unlink'.
-type pid_or_port() :: pid() | port().
-type stacktrace()  :: list(). %% XXX Specify.
