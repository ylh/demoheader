-module(demoheader).

-export([main/1]).

% bless epp
-define(HEADER,
	?X('demo-protocol', $d, "(version number)", 32, signed-little),
	?X('network-protocol', $n, "(version number)", 32, signed-little),
	?X('server-name', $s, "(IP:port string)", 260, bytes),
	?X('client-name', $c, "(recorder's in-game name)", 260, bytes),
	?X('map-name', $m, "(without .bsp extension)", 260, bytes),
	?X('game-directory', $g, "(tf/, hl2/, etc)", 260, bytes),
	?X('playback-seconds', $S, "(float)", 32, float-little),
	?X('ticks', $t, "(integer)", 32, signed-little),
	?X('frames', $f, "(integer)", 32, signed-little),
	?X('signon-length', $l, "(integer)", 32, signed-little)
).

-define(X(ATOM, CHAR, DESC, _W, _T), F(ATOM, CHAR, DESC)).
main(Args) ->
	F = fun(A, C, D) -> {A, C, atom_to_list(A), undefined, D} end,
	Opts = [
		F(json, $j, "Emit JSON"),
		?HEADER,
		F(help, $h, "Show this help")
	],
	case Args of
		[_|_] ->
			args(Opts, Args);
		_ ->
			usage(Opts),
			halt(1)
	end.
-undef(X).

-define(X(ATOM, _C, _D, _W, _T), ATOM).
is_header(Atom) -> lists:member(Atom, [?HEADER]).
args(Opts, Args) ->
	DU = fun(F, A) ->
		err("error: " ++ F, A),
		usage(Opts),
		halt(1)
	end,
	{Options, NonOptionArgs} = case getopt:parse(Opts, Args) of 
		{error, E} -> DU("~s", [getopt:format_error(Opts, E)]);
		{ok, Res} -> Res
	end,
	lists:member(help, Options) andalso begin usage(Opts), halt(0) end,
	Headers = lists:filter(fun is_header/1, Options),
	Retrieval = case lists:member(json, Options) of
		true -> json_of(case Headers of
			[] -> [?HEADER];
			Else -> Else
		end);
		_ -> case Headers of
			[One] -> fun(#{One := V}) ->
				Fmt = case is_list(V) of true -> "~s"; _ -> "~p" end,
				io_lib:format(Fmt, [V])
			end;
			_ -> DU("without -j, exactly one field must be requested", [])
		end				
	end,
	Parsed = case NonOptionArgs of
		[Single] -> parse(Single);
		_ -> DU("exactly one file must be specified", [])
	end,
	io:format("~s\n", [Retrieval(Parsed)]).
-undef(X).

json_of(Keys) ->
	fun(Map) ->
		F = fun(Key, Acc) ->
			#{Key := Value} = Map,
			[io_lib:format("\"~s\": ~p", [atom_to_list(Key), Value])|Acc]
		end,
		"{\n\t" ++ 
			lists:join(",\n\t", lists:foldl(F, [], lists:reverse(Keys))) ++
		"\n}"
	end.

-define(X(ATOM, _C, _D, WIDTH, TYPE), fun
	({<<Var:WIDTH/TYPE,Rest/binary>>, Map}) -> {Rest, Map#{ATOM => Var}}
end).
parse(Arg) ->
	In = case file:open(Arg, [read, binary]) of
		{ok, I} -> I;
		{error, EE} -> die("~s: ~s", [Arg, file:format_error(EE)])
	end,
	Bin = case file:read(In, 1072) of
		{ok, B} when byte_size(B) =:= 1072 -> B;
		{ok, _} -> die("could not read 1072 bytes (is ~s a demo?)", [Arg]);
		{error, E} -> die("error reading ~s: ~s", [Arg, file:format_error(E)])
	end,
	file:close(In),
	Rem = case Bin of
		<<"HL2DEMO\0", R/binary>> -> R;
		_ -> die("did not see \"HL2DEMO\" at start (is ~s a demo?)", [Arg])
	end,
	{<<>>, Map} = lists:foldl(fun(F, A) -> F(A) end, {Rem, #{}}, [?HEADER]),
	maps:map(fun
		(_, S) when is_binary(S) ->
			% emitting nulls breaks a lot of things, it's kind of funny
			unicode:characters_to_list(case binary:match(S, <<0>>) of
				{Pos, _} -> binary:part(S, 0, Pos);
				_ -> S
			end);
		(_, V) ->
			V
	end, Map).
-undef(X).

-define(X(_A, CHAR, _D, _W, _T), CHAR).
usage(Opts) ->
	err("Usage: ~w -j [-~s] demo.dem", [?MODULE, [?HEADER]]),
	err("       ~w -~s demo.dem\n", [?MODULE, [?HEADER]]),
	err("~ts", [unicode:characters_to_list(getopt:usage_options(Opts))]),
	err("If -j is not given, exactly one header element must be selected "
"(-" ++ [?HEADER] ++ "),\n"
"and it will be printed on standard output. If -j is given, any number of\n"
"header elements may be selected; specifying none is equivalent to specifying\n"
"all of them. Keys are the long forms of their corresponding flags.").
-undef(X).

die(Fmt, Args) ->
	err(Fmt, Args),
	halt(1).
err(Str) ->
	err("~s", [Str]).
err(Fmt, Args) ->
	io:format(standard_error, Fmt ++ "\n", Args).
