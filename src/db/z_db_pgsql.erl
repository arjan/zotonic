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
    {reply, pgsql:squery(Conn, Sql), State};
handle_call({get_parameter, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:get_parameter(Conn, Sql), State};
handle_call({equery, Sql, Params}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:equery(Conn, Sql, Params), State};
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
