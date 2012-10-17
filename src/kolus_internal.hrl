-define(INFO_MSG(Msg), error_logger:info_msg(Msg)).
-define(INFO_MSG(Format, Msg), error_logger:info_msg(Format, Msg)).

-define(IDLE_KEY(Key), {c,l,{idle,Key}}).
-define(UNUSED_KEY(Key), {c,l,{unused,Key}}).

-define(LOOKUP_PID(Key), {n,l,{backend_pid,Key}}).
