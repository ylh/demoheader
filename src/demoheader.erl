-module(demoheader).

-export([main/1]).

% bless epp
-define(FIELDS,
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
	args(Args, [
		F(json, $j, "Emit JSON"),
		?FIELDS,
		F(help, $h, "Show this help")
	]).
-undef(X).

-define(X(ATOM, _C, _D, _W, _T), ATOM).
args(Args, Opts) ->
	U = usage(Opts),
	Args =:= [] andalso die(U, "no arguments given"),
	{OptArgs, ExtraArgs} = case getopt:parse(Opts, Args) of
		{error, E} -> die(U, getopt:format_error(Opts, E));
		{ok, Res} -> Res
	end,
	lists:member(help, OptArgs) andalso (usage(Opts))(0),
	Show = case {
		lists:member(json, OptArgs),
		[Arg || Arg <- OptArgs, lists:member(Arg, [?FIELDS])]
	} of
		{true, []} -> fun(P) -> P end;
		{true, Else} -> fun(P) -> maps:with(Else, P) end;
		{false, [One]} -> fun(P) -> #{One := V} = P, V end;
		{false, _} -> die(U, "without -j, exactly one field must be requested")
	end,
	Parsed = case ExtraArgs of
		[Single] -> parse(Single);
		_ -> die(U, "exactly one file must be specified")
	end,
	io:put_chars([
		jsone:encode(Show(Parsed), [{indent, 1}, {float_format, [short]}]),
		$\n
	]).
-undef(X).

-define(X(ATOM, _C, _D, WIDTH, TYPE), fun
	({<<Var:WIDTH/TYPE, Rem/binary>>, Map}) -> {Rem, Map#{ATOM => Var}}
end).
parse(Arg) ->
	H = fun erlang:halt/1,
	In = case file:open(Arg, [read, binary]) of
		{ok, I} -> I;
		{error, E} -> die(H, "~s: ~s", [Arg, file:format_error(E)])
	end,
	Bin = case file:read(In, 1072) of
		{ok, <<"HL2DEMO\0", Rem:1064/bytes>>} -> Rem;
		{ok, <<_:1072/bytes>>} -> die(H, "no \"HL2DEMO\" at start of file");
		{ok, _} -> die(H, "could not read 1072 bytes");
		{error, EE} -> die(H, "reading ~s: ~s", [Arg, file:format_error(EE)])
	end,
	{<<>>, Map} = lists:foldl(fun(F, A) -> F(A) end, {Bin, #{}}, [?FIELDS]),
	maps:map(fun
		(_, <<S/binary>>) -> [Head|_] = binary:split(S, <<0>>), Head;
		(_, V) -> V
	end, Map).
-undef(X).

-define(X(_A, CHAR, _D, _W, _T), CHAR).
usage(Opts) -> fun(Ret) ->
	err("Usage: ~w -j [-~s] demo.dem", [?MODULE, [?FIELDS]]),
	err("       ~w -~s demo.dem\n", [?MODULE, [?FIELDS]]),
	err(getopt:usage_options(Opts)),
	err("If -j is not given, exactly one header element must be selected "
"(-" ++ [?FIELDS] ++ "),\n"
"and it will be printed on standard output. If -j is given, any number of\n"
"header elements may be selected; specifying none is equivalent to specifying\n"
"all of them. Keys are the long forms of their corresponding flags."),
	halt(Ret)
end.
-undef(X).

die(With, Fmt) -> die(With, Fmt, []).
die(With, Fmt, Args) -> err("error: " ++ Fmt, Args), With(1).
err(Str) -> err("~s", [Str]).
err(Fmt, Args) -> io:format(standard_error, Fmt ++ "\n", Args).
