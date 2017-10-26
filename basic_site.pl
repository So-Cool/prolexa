:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_open)).
:- use_module(library(ssl)).
:- use_module(library(clpfd)).
:- use_module(alexa_mod).

%:- initialization http_daemon.



% Declare a handler, binding an HTTP path to a predicate.
% Here our path is / (the root) and the goal we'll query will be
% say_hi. The third argument is for options
:- http_handler('/', say_hi, []).
:- http_handler('/door', door_page,[]).
:- http_handler(root(alexa), alexa,[]).

% The predicate server(+Port) starts the server. It simply creates a
% number of Prolog threads and then returns to the toplevel, so you can
% (re-)load code, debug, etc.
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

/* The implementation of /. The single argument provides the request
details, which we ignore for now. Our task is to write a CGI-Document:
a number of name: value -pair lines, followed by two newlines, followed
by the document content, The only obligatory header line is the
Content-type: <mime-type> header.
Printing is done with print_html, which takes a list of tokens and
prints them. It attempts to 'reasonably' format html when it recognizes
tags. */

fact('walrus').
fact('elephant').
putp(A,div(A)).

door_page(_Request):-
       make, %refresh the doorbell file
       findall(Time,pressed(Time,_Date),Rest),
       maplist(term_to_atom,Rest,Strings),
       maplist(putp,Strings,Ps),
       reply_html_page(
         title('doorbell'),Ps).

say_hi(_Request) :-
	phrase(
	    my_nonterm,
	    TokenizedHtml,
	    []),
        format('Content-type: text/html~n~n'),
	print_html(TokenizedHtml).

my_nonterm -->
	html([html([head([title('Sam Neaves homepage')]),
		    body([h1('Welcome to Sam Neaves home page'),
			  p("Please take a look at these:"),
                          h2('Publications:'),
                          iframe([width=1400, height=1500,src='http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4962912/'],'something'),
  			  h2('Youtube Channel Playing with Prolog:'),
			  a(href='https://www.youtube.com/channel/UCfWpIHmy5MEx2p9c_GJrE_g', 'Youtube Playing with Prolog'),
			  h2('Remote controlling a robot with Prolog pengines:'),
                          iframe([width=1400, height=1500, src='http://swish.swi-prolog.org/p/embed' + encode(' youtube test.swinb')],'something'),

			  p('This server is powered by swi-prolog and raspberry pi!')
                        ])
                   ])
             ]).

url_test('https://s3.amazonaws.com/echo.api/echo-api-cert-4.pem').


print_header(Request):-
	setup_call_cleanup(
			   open('request.txt', append,Stream,[]),
			   forall(member(M,Request),write_term(Stream,M,[nl(true),quoted(true),fullstop(true)])),
			   close(Stream)
			  ).

