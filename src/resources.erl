-module (resources).
-export ([loop/1, new/0, init/0, perform_cmd/3, allocated/1, deallocated/1]).

-include("records.hrl").
-include("config.hrl").

allocated({?MODULE, Allocated, _Deallocated})->
	Allocated.

deallocated({?MODULE, _Allocated, Deallocated})->
	Deallocated.

make_res_list(Length)->
	case Length > 0 of
		true ->
			[ list_to_binary("res_" ++ integer_to_list(Length))  |  make_res_list(Length - 1) ];
		false -> []
	end.

drop_last(List) ->
	lists:reverse(tl(lists:reverse(List))). % in 17.0 just - lists:droplast(), but in R16B03 - that crap :(

new()->
	new(?CAPACITY).

new(Len)->
	Seq = make_res_list(Len),
	#resources{allocated = dict:new(), deallocated = Seq}.

init()->
	Pid = spawn(resources, loop, [new()]),
    register(resources, Pid).

loop(State)->
    receive
        {From, Msg} ->
            {NewState, Ans} = State:perform_cmd(Msg#cmd.name, Msg#cmd.data),
            From ! {self(), Ans},
            loop(NewState)
    end.

perform_cmd(reset, _, _)->
	Ans = #ans{code = 204, headers = [{"Content-Type", "application/json"}], body = ""},
	{new(), Ans};

% type correction
perform_cmd(allocate, UID, State) when is_list(UID) ->
	perform_cmd(allocate, list_to_binary(UID), State);

perform_cmd(allocate, UID, {?MODULE, Allocated, Deallocated} = State)->
	 case length(Deallocated) > 0 of
	 	true ->
			Resource = lists:last(Deallocated),
			NewDeallocated = drop_last(Deallocated), % in 17.0 just - lists:droplast(), but in R16B03 - that crap :(
			% io:format("~p", [ Deallocated]),
			NewAllocated = dict:store(Resource, UID, Allocated),
			NewState = {?MODULE, NewAllocated, NewDeallocated},
			Ans = #ans{code = 201, body = Resource},
			{NewState, Ans};
	 	false -> 
	 		Ans = #ans{code = 503, body = "Out of resources"},
	 		{State, Ans}
	 end;

% type correction
perform_cmd(deallocate, RID, State) when is_list(RID) ->
	perform_cmd(deallocate, list_to_binary(RID), State);

perform_cmd(deallocate, RID, {?MODULE, Allocated, Deallocated} = State)->
	case dict:is_key(RID, Allocated) of
		true ->
			NewAllocated = dict:erase(RID, Allocated),
			NewDeallocated = [RID | Deallocated],
			NewState = {?MODULE, NewAllocated, NewDeallocated},
			Ans = #ans{code = 204, body = ""},
			{NewState, Ans};
		false -> 	
			Ans = #ans{code = 404, body = "Not allocated"},
			{State, Ans}
	end;
	

perform_cmd(list, undefined , {?MODULE, Allocated, Deallocated} = State)->
	Term = [{allocated, dict:to_list(Allocated)}, {deallocated, Deallocated}],
	List = mochijson2:encode(Term),
	Ans = #ans{code = 200, headers = [{"Content-Type", "application/json"}], body = List},
	{State, Ans};

% type correction
perform_cmd(list, UID, State) when is_list(UID) ->
	perform_cmd(list, list_to_binary(UID), State);

perform_cmd(list, UID , {?MODULE, Allocated, _Deallocated} = State)->
	Fun = fun (Key, Value, AccIn) ->
		case Value == UID of
			true ->	[Key | AccIn];
			false -> AccIn
		end
	end,
	UserAllocated = dict:fold(Fun, [], Allocated),
	Ans = #ans{code = 200, headers = [{"Content-Type", "application/json"}], body = mochijson2:encode(UserAllocated)},
	{State, Ans}.





%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_res_list_test()->
	Res = make_res_list(3),
	Result = [<<"res_3">>,<<"res_2">>,<<"res_1">>],
	?assert(Res =:= Result),
	ok.

reset_test()->
	Res = new(),
	{DState, _} = Res:perform_cmd(deallocate, "res_1"),
	{Result, Ans} = DState:perform_cmd(reset, undefined),	
	?assert(length(Result:deallocated()) =:= ?CAPACITY),
	?assert(length(dict:to_list(Result:allocated())) =:= 0),
	?assert(Ans#ans.code =:= 204),
	?assert(Ans#ans.headers =:= [{"Content-Type","application/json"}]),
	?assert(Ans#ans.body =:= ""),
	ok.

out_of_resources_test()->
	NewState = new(1),
	{State, _} = NewState:perform_cmd(allocate, "bob"),
	{ORState, ORAns} = State:perform_cmd(allocate, "bob2"),
	% % Check http answer code, body and headers
	?assert(ORAns#ans.code =:= 503),
	?assert(ORAns#ans.headers =:= [{"Content-Type", "text/plain"}]),
	?assert(ORAns#ans.body =:= "Out of resources"),	
	?assert(ORState =:= State),
	ok.

drop_last_test()->
	TestData = [<<"A">>, <<"B">>, <<"C">>],
	Result = drop_last(TestData),
	?assert(Result =:= [<<"A">>, <<"B">>]).

cmd_list_test()->
	Res = new(),
	{State, Ans} = Res:perform_cmd(list, undefined),
	?assert(Ans#ans.code =:= 200),
	?assert(Ans#ans.headers =:= [{"Content-Type", "application/json"}]),
	Term = [{allocated, dict:to_list(State#resources.allocated)}, {deallocated, State#resources.deallocated}],
	List = mochijson2:encode(Term),
	?assert(Ans#ans.body =:= List),
	ok.

cmd_list_user_empty_test()->
	Res = new(),
	{State, Ans} = Res:perform_cmd(list, "bob"),
	Res1 = new(),
	% Check http answer code and headers
	?assert(Ans#ans.code =:= 200),
	?assert(Ans#ans.headers =:= [{"Content-Type", "application/json"}]),
	% Check side effect
	?assert(State =:= Res1),

	Term = [],
	List = mochijson2:encode(Term),
	?assert(Ans#ans.body =:= List),
	ok.

cmd_allocate_test()->
	{State, Ans} = allocate("bob"),
	% % Check http answer code, body and headers
	?assert(Ans#ans.code =:= 201),
	?assert(Ans#ans.body =:= <<"res_1">>),
	?assert(Ans#ans.headers =:= [{"Content-Type", "text/plain"}]),
	KvList = dict:to_list(State#resources.allocated),
	% Check allocated resources
	?assert(KvList =:= [{<<"res_1">>,<<"bob">>}]),
	ResultData = [<<"res_12">>,<<"res_11">>,<<"res_10">>,<<"res_9">>,<<"res_8">>,
     <<"res_7">>,<<"res_6">>,<<"res_5">>,<<"res_4">>,<<"res_3">>,<<"res_2">>],
    % Check deallocated resources
    ?assert(State#resources.deallocated =:= ResultData),
	ok.

cmd_deallocate_test()->
	Res = new(),
	{State, _} = Res:perform_cmd(allocate, "bob"),
	{DState, DAns} = State:perform_cmd(deallocate, "res_1"),
	% Check http answer code, body and headers
	?assert(DAns#ans.code =:= 204),
	?assert(DAns#ans.headers =:= [{"Content-Type", "text/plain"}]),
	?assert(DAns#ans.body =:= []),
	{?MODULE, Allocated, Deallocated} = DState,
	% Check allocated resources
	?assert(dict:to_list(Allocated) =:= []),
    % Check deallocated resources	
	TestData = [<<"res_1">>,<<"res_12">>,<<"res_11">>,<<"res_10">>,<<"res_9">>,
     			<<"res_8">>,<<"res_7">>,<<"res_6">>,<<"res_5">>,<<"res_4">>,<<"res_3">>,<<"res_2">>],
	?assert(Deallocated =:= TestData),
	ok.

cmd_deallocate_wrong_test()->
	Res = new(),
	{State, _} = Res:perform_cmd(allocate, "bob"),
	{DStateWrong, DAnsWrong} = State:perform_cmd(deallocate, "CCC"),
	% Check http answer code, body and headers
	?assert(DAnsWrong#ans.code =:= 404),
	?assert(DAnsWrong#ans.headers =:= [{"Content-Type", "text/plain"}]),
	?assert(DAnsWrong#ans.body =:= "Not allocated"),
	?assert(DStateWrong =:= State),	
	ok.

cmd_deallocate_empty_test()->
	Res = new(),
	{DStateWrong, DAnsWrong} = Res:perform_cmd(deallocate, "CCC"),
	% Check http answer code, body and headers
	?assert(DAnsWrong#ans.code =:= 404),
	?assert(DAnsWrong#ans.headers =:= [{"Content-Type", "text/plain"}]),
	?assert(DAnsWrong#ans.body =:= "Not allocated"),
	?assert(DStateWrong =:= Res),	
	ok.

allocate(Name) -> 
	Res = new(),
	Res:perform_cmd(allocate, Name).



-endif.













