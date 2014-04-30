%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014 Arjan Scherpenisse
%% Date: 2014-04-29
%%
%% @doc Postgresql pool worker

%% Copyright 2014 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_db_pgsql).
-behaviour(gen_server).

-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([
         squery/2,
         get_parameter/2,
         equery/4
        ]).

-define(TIMEOUT, 5000).
-define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).


-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


squery(Worker, Sql) ->
    gen_server:call(Worker, {squery, Sql}, ?TIMEOUT).

get_parameter(Worker, Parameter) when is_binary(Parameter) ->
    gen_server:call(Worker, {get_parameter, Parameter}, ?TIMEOUT).

equery(Worker, Sql, Parameters, Timeout) ->
    gen_server:call(Worker, {equery, Sql, Parameters}, Timeout).
    

init(Args) ->
    process_flag(trap_exit, true),
    Hostname = proplists:get_value(dbhost, Args),
    Port = proplists:get_value(dbport, Args),
    Database = proplists:get_value(dbdatabase, Args),
    Username = proplists:get_value(dbuser, Args),
    Password = proplists:get_value(dbpassword, Args),
    Schema = proplists:get_value(dbschema, Args),
    
    case pgsql:connect(Hostname, Username, Password,
                       [{database, Database}, {port, Port}]) of
        {ok, Conn} ->
            case pgsql:squery(Conn, "SET search_path TO " ++ Schema) of
                {ok, [], []} ->
                    {ok, #state{conn=Conn}};
                Error -> 
                    pgsql:close(Conn),
                    {stop, Error}
            end;
        {error, _} = E ->
            {stop, E}
    end.

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, decode_reply(pgsql:squery(Conn, Sql)), State};
handle_call({get_parameter, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:get_parameter(Conn, Sql), State};
handle_call({equery, Sql, Params}, _From, #state{conn=Conn}=State) ->
    {reply, decode_reply(pgsql:equery(Conn, Sql, encode_values(Params))), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% These are conversion routines between how z_db expects values and how epgsl expects them.

%% Notable differences:
%% - Input values {term, ...} are term_to_binary encoded and decoded
%% - null <-> undefind
%% - date/datetimes have a floating-point second argument in epgsql, in Zotonic they don't.

encode_values(L) when is_list(L) ->
    lists:map(fun encode_value/1, L).

encode_value(undefined) ->
    null;
encode_value({term, Term}) ->
    B = term_to_binary(Term),
    <<?TERM_MAGIC_NUMBER, B/binary>>;
encode_value(Value) ->
    Value.


decode_reply({ok, Columns, Rows}) ->
    {ok, Columns, lists:map(fun decode_values/1, Rows)};
decode_reply({ok, Nr, Columns, Rows}) ->
    {ok, Nr, Columns, lists:map(fun decode_values/1, Rows)};
decode_reply(R) ->
    R.

decode_values(T) when is_tuple(T) ->
    list_to_tuple(decode_values(tuple_to_list(T)));
decode_values(L) when is_list(L) ->
    lists:map(fun decode_value/1, L).

decode_value({V}) ->
    {decode_value(V)};

decode_value(null) ->
    undefined;
decode_value(<<?TERM_MAGIC_NUMBER, B/binary>>) ->
    binary_to_term(B);
decode_value({H,M,S}) when is_float(S) ->
    {H,M,trunc(S)};
decode_value({{Y,Mm,D},{H,M,S}}) when is_float(S) ->
    {{Y,Mm,D},{H,M,trunc(S)}};
decode_value(V) ->
    V.


