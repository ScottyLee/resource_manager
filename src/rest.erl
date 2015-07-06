-module (rest).
-export ([response/2]).
-include ("records.hrl").

wait_answer(Req)->
	Pid = whereis(resources),
	receive
		{Pid, MSG} ->  
			Req:respond({MSG#ans.code, MSG#ans.headers, MSG#ans.body})
	end.

process_cmd(Cmd, Data, Req)->
	Command = #cmd{name = Cmd, data = Data},
	Pid = whereis(resources),
	Pid ! {self(), Command},
	wait_answer(Req).

rest_response(Req, 'GET', "/allocate/" ++ UID, _DocRoot) ->
	process_cmd(allocate, UID, Req);

rest_response(Req, 'GET', "deallocate/" ++ RID, _DocRoot) ->
	process_cmd(deallocate, RID, Req);

rest_response(Req, 'GET', "/list", _DocRoot) ->
	process_cmd(list, undefined, Req);

rest_response(Req, 'GET', "/list/" ++ UID, _DocRoot) ->
	process_cmd(list, UID, Req);

rest_response(Req, 'GET', "/reset", _DocRoot) ->
	process_cmd(reset, undefined, Req);

rest_response(Req, 'GET', "/reset/" ++ UID, _DocRoot) ->
	process_cmd(reset, UID, Req);

rest_response(Req, _Method, _Path, _DocRoot) ->
	Ans = #ans{},
    Req:respond({Ans#ans.code, Ans#ans.headers, Ans#ans.body}).

response(Req, DocRoot) ->
    Resp = rest_response(Req, Req:get(method), Req:get(path), DocRoot),
    Resp.