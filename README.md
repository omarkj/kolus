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
    [backend_status()]|[] = kolus:status([backend()])
```

### Types:

``` erlang
    backend() :: {inet:ip_address(), inet:port_number()}
    backend_status :: {backend(), pid(), [backend_info()]}.
```

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
    {socket, kolus_socket()} = kolus:connect(Ident::any(), backend()|pid()),
```

If you send in a `pid()` but not a `backend()` it will be treated as a
manager.

The kolus_socket() is an opaque type.

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

License
-------

Check the `LICENSE` file.