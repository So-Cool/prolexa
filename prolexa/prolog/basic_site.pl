:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_open)).
:- use_module(library(ssl)).
:- use_module(library(clpfd)).
:- use_module(prolexa).

%:- initialization http_daemon.

:- http_handler(root(prolexa), prolexa,[]).

% The predicate server(+Port) starts the server. It simply creates a
% number of Prolog threads and then returns to the toplevel, so you can
% (re-)load code, debug, etc.
server(Port) :-
   http_server(http_dispatch, [port(Port)]).
