%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014 Arjan Scherpenisse
%% Date: 2014-04-29
%%
%% @doc Database pool wrapper

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

-module(z_db_pool).

-include_lib("zotonic.hrl").
-define(DEFAULT_DB_DRIVER, z_db_pgsql).

-export([
         child_spec/2,
         get_database_options/1,
         db_pool_name/1,
         db_driver/1,
         get_connection/1,
         return_connection/2
        ]).


db_pool_name(Host) ->
    list_to_atom("z_db_pool" ++ [$$ | atom_to_list(Host)]).

db_driver(SiteProps) when is_list(SiteProps) ->
    proplists:get_value(dbdriver, SiteProps, ?DEFAULT_DB_DRIVER);
db_driver(Context=#context{}) ->
    case m_site:get(dbdriver, Context) of
        undefined -> ?DEFAULT_DB_DRIVER;
        Driver -> Driver
    end.


get_database_options(Context) ->
    z_depcache:memo(
      fun() ->
              SiteProps = z_sites_manager:get_site_config(Context#context.host),
              db_opts(SiteProps)
      end,
      {z_db_pool, database_options}, ?HOUR, Context).

%% @doc Optionally add the db pool connection
child_spec(Host, SiteProps) ->
    case proplists:get_value(dbdatabase, SiteProps, atom_to_list(Host)) of
        none -> 
            %% No database connection needed
            [];
        _ ->
            % Add a db pool to the site's processes
            PoolSize    = proplists:get_value(dbpool_size,     SiteProps, 5),
            PoolMax     = proplists:get_value(dbpool_max_overflow,     SiteProps, 20),

            Name = db_pool_name(Host),

            WorkerModule = db_driver(SiteProps),
            WorkerArgs = db_opts(SiteProps),
            
            PoolArgs = [{name, {local, Name}},
                        {worker_module, WorkerModule},
                        {size, PoolSize},
                        {max_overflow, PoolMax}],
            [poolboy:child_spec(Name, PoolArgs, WorkerArgs)]
    end.

db_opts(SiteProps) ->
    [{K, proplists:get_value(K, SiteProps, z_config:get(K))}
     || K <- [dbhost, dbport, dbuser, dbpassword, dbdatabase, dbschema]].

get_connection(#context{db={Pool,_}}) ->
    poolboy:checkout(Pool).

return_connection(Worker, #context{db={Pool,_}}) ->
    poolboy:checkin(Pool, Worker).
