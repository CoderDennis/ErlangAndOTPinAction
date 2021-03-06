%%%-------------------------------------------------------------------
%%% Created :  5 Dec 2014 by  <Dennis@DENNIS-V5>
%%%-------------------------------------------------------------------
-module(json_parser).

%% API
-export([parse_document/1]).

%%%===================================================================
%%% API
%%%===================================================================

parse_document(Data) when is_binary(Data) ->
    jp_server:parse_document(Data).
