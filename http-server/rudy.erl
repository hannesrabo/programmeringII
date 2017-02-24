-module(rudy).
-export([init/1, file_request/1]).


% Initiate the server
init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			
			% Accept an incoming request
			handler(Listen),

			% Close the server
			gen_tcp:close(Listen),
			ok;
		{error, _Error} ->
			error
	end.


% Listen for incoming connections.
handler(Listen) ->

	% Accepting a request from a client
	{ok, Client} = gen_tcp:accept(Listen),
	
	% Parsing the request
	request(Client),

	% Continue parsing
	handler(Listen).


% Parsing request from the clients
request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of
		{ok, Str} ->
			% Parsing the request
			Request = http:parse_request(Str),
			
			% Creating the response
			Response = reply(Request),
			
			% Sending the answer
			gen_tcp:send(Client, Response);
		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,
	gen_tcp:close(Client).

% Format and send the reply to the client.
reply(Request) ->
	% We should load a page from memory here...
	% Reading the requested file.
	{{get, FileName, _HTTPVersion}, _Args, _Message} = Request,
	if
		FileName == [$/]-> 
			binay_file_request("index.html");
		true -> 
			binay_file_request([$. | FileName])
	end.


% This can read practically any file and just converts it to an 
% array before sending it.
binay_file_request(FileName) ->
	%io:format("GET ~p~n", [FileName]),
	case file:read_file(FileName) of
		{ok, Binary} ->
			%Providing the page
			%io:format("200 OK~n"),
			http:ok(binary_to_list(Binary));

		{error, _Reason} ->

		    % Trying to provide 404 page
			case file:read_file("404.html") of
				{ok, Binary} ->
					%io:format("404 Page not available~n"),
					http:fnf(binary_to_list(Binary));
				{error, _Reason} ->
					%io:format("404 Page not available (no page)~n"),
					http:fnf()
			end
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is not very useful as it can't read binary files. 
% Use the binary instead!!
file_request(FileName) ->
	io:format("GET ~p~n", [FileName]),
	case file:open(FileName, [read]) of
		{ok, FileDescriptor} ->
			try read_lines(FileDescriptor) of
				File ->
					io:format("200 OK~n"),
					http:ok(File)
			catch
				_Error ->
					io:format("404 Read error~n"),
					http:fnf()
			after 
				file:close(FileDescriptor)
			end;
		
		{error, _Msg} ->
			io:format("404 File not found~n"),
			http:fnf()
	end.
			
			

read_lines(FileDescriptor) ->
	case io:get_line(FileDescriptor, "") of
		eof -> [];
		Line -> Line ++ read_lines(FileDescriptor)	
	end.
