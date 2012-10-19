kolus
=====

Kolus is a library to help you manager your open TCP connections,
manager connections to your various backends as well as help you
balance connections to your backends.

Setup
-----

```
$ make compile
```

Starting Kolus
--------------

```
$ erl -pa ebin -env ERL_LIBS deps -s kolus_app
```

Common Tests
------------

```
$ make test
```

Erlang API
----------

## Backend status

Get the current status of the backends. Keep in mind this information
can change fast, and is considered outdated by `kolus` as soon as the
caller has it.

``` erlang
[kolus_backend()]|[] = kolus:status([backend()|kolus_backend()]|[])
```

### Types:

``` erlang
backend() :: {inet:ip_address(), inet:port_number()}.
kolus_backend() :: #kolus_backend{}.
```

The `#kolus_backend{}` record can be used to match on idle TCP or 
unused TCP connections:

``` erlang
#kolus_backend{ ip :: inet:ip_address(),
	      	port :: inet:port_number(),
	        idle :: pos_integer()|undefined,
	       	unused :: pos_integer()|undefined}.
```

The `idle` and `unused` fields will be `undefined` if the manager has
not been started or has not been looked up. Do not change values of the
`kolus_backend{}`.

## Connecting

Get a socket from the manager, if no socket exists a new one will be 
made and returned to the caller. If no manager exists for this backend 
a new manager will be created and a socket will be  returned from that 
manager.

The Ident is used to identify this connection request to the manager, this
makes it possible to prevent connection request to different idents to go
through the same manager (for example, when a specific Ident is no longer
served via a socket).

``` erlang
{socket, kolus_socket()} = kolus:connect(Ident::any(), backend()|
						       kolus_backend()),
```

The `kolus_socket()` is an opaque type.

## Returning a socket

Return a socket the caller is done using. The socket will be added back to
the manager and made available to other callers.

``` erlang
ok = kolus:return(kolus_socket())
```

## Returning a closed socket

If the manager returns a closed/unusable socket to the caller it can be 
returned using the following function. The socket will be removed from 
the managers list.

``` erlang
ok = kolus:finished(kolus_socket())
```

## Using the `kolus_socket()`

The `kolus_socket()` is an opaque type, you cannot read or write 
directly to it.

### Getting the actual socket

``` erlang
port() = kolus:get_socket(kolus_socket())
```  

### Getting the manager behind the `kolus_socket()`

``` erlang
pid() = kolus:get_manager(kolus_socket())
```  

## Select

You can specify a search function that's run on the list of backends,
this function should return the backend the caller wants to use.

### Very simple "fill first" loadbalancing

``` erlang
fill_first(Backends) ->
	hd(lists:sort(fun(#kolus_backend{idle=A,unused=X},
			   #kolus_backend{idle=B,unused=Y}) ->
			      case A > B of
				  true ->
				      true;
				  false ->
				      case X of
					  0 ->
					      false;
					  _ ->
					      X =< Y
				      end
			      end
		      end, Backends)).
{socket, Socket} = kolus:select(<<"test">>, Backends, fun ff_filter/1).
```

Example
-------

For an example check out [kolus_demo](https://github.com/omarkj/kolus_demo)

License
-------

Check the `LICENSE` file.