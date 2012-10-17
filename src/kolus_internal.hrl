-define(INFO_MSG(Msg), error_logger:info_msg(Msg)).
-define(INFO_MSG(Format, Msg), error_logger:info_msg(Format, Msg)).

-define(MANAGER_KEY(Ip, Port), {n,l,{manager,Ip,Port}}).

-define(LOOKUP_TID(Key), {n,l,{backend_tid,Key}}).
-define(LOOKUP_PID(Key), {n,l,{backend_pid,Key}}).
