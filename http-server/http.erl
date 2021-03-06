-module(http).
-export([parse_request/1, ok/1, get/1, fnf/0, fnf/1]).

% Parse a request for a page.
parse_request(R0) ->
	{Request, R1} = request_line(R0),
	{Headers, R2} = headers(R1),
	{Body, _} = message_body(R2),
	{Request, Headers, Body}.

% Extract the request information
request_line([$G, $E, $T, 32 | R0]) ->
	{URI, R1} = request_uri(R0),
	{Ver, R2} = http_version(R1),
	[13, 10| R3] = R2,
	{{get, URI, Ver}, R3}.


% Extract and return a URI from the request string
request_uri([32| R0]) ->
	{[], R0};
request_uri([C|R0]) ->
	{Rest, R1} = request_uri(R0),
	{[C|Rest], R1}.

% Extract the http verions information
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
	{v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
	{v10, R0}.

% Consume all headers
headers([13, 10| R0]) ->
	{[], R0};
headers(R0) ->
	{Header, R1} = header(R0),
	{Rest, R2} = headers(R1),
	{[Header|Rest], R2}.

header([13, 10 | R0]) ->
	{[], R0};
header([C | R0]) ->
	{Rest, R1} = header(R0),
	{[C | Rest], R1}.


% Parse the body
message_body(R) ->
	{R, [1]}.




% Response functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ok(Body) ->
	"HTTP/1.1 200 OK\r\n\r\n" ++ Body.

% File not found
fnf() ->
	"HTTP/1.1 404 File not found\r\n\r\n".

% File not found with a body
fnf(Body) ->
	"HTTP/1.1 404 File not found\r\n\r\n" ++ Body.
	

get(URI) ->
	"GET " ++ URI ++ " HTTP/1.1\r\n\r\n".

