-module (resource).
-export ([new/1, capacity/0]).
-include ("config.hrl").
-record (resource, {name = undefined}).



new(Name)->
	#resource{name = Name}.

capacity()->
	?CAPACITY.