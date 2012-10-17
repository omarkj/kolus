-record(kolus_backend, {ip :: inet:ip_address(),
			port :: inet:port_number(),
			idle :: pos_integer()|undefined,
			unused :: pos_integer()|undefined,
			manager :: pid()|undefined,
			manager_status :: ets:tid()|undefined
		       }).
