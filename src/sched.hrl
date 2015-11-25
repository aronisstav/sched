-include("sched_version.hrl").

-ifdef(DEBUG).
-define(d(T), erlang:display({{?MODULE, ?LINE}, T})).
-else.
-define(d(_), ok).
-endif.
