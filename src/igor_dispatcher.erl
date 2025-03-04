-module(igor_dispatcher).

%% Include files

%% Exported functions

-callback format_unknown_error(Exception) -> Reply when
      Exception :: term(),
      Reply :: binary().

-callback send(State, Service, Packet) -> State when
      Service :: atom(),
      Packet :: iodata().

-callback handler(_State, Service) -> Handler when
      Service :: atom(),
      Handler :: module().

-callback push_context(State, Service, Context) -> {ContextId, State} when
      Service :: atom(),
      Context :: igor_types:context(),
      ContextId :: igor_types:context_id().

-callback pop_context(State, Service, ContextId) -> {Context, State} when
      Service :: atom(),
      ContextId :: igor_types:context_id(),
      Context :: igor_types:context().

%% API

%% Local functions
