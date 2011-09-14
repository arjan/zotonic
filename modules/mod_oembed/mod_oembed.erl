%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-09-14
%% @doc Enables embedding media from their URL.

%% Copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(mod_oembed).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("OEmbed support").
-mod_description("Add external media in your site by their URL.").
-mod_prio(600).

%% interface functions
-export([
    observe_rsc_update/3,
    observe_media_viewer/2,
    observe_media_stillimage/2,
    event/2
]).

-include_lib("zotonic.hrl").


%% Fantasy mime type to distinguish embeddable html fragments.
-define(OEMBED_MIME, <<"text/html-oembed">>).

%% @doc Check if the update contains video embed information.  If so then update the attached medium item.
%% @spec observe_rsc_update({rsc_update, ResourceId, OldResourceProps}, {Changed, UpdateProps}, Context) -> {NewChanged, NewUpdateProps}
observe_rsc_update({rsc_update, Id, _OldProps}, {Changed, Props}, Context) ->
    case proplists:is_defined(oembed_url, Props) of
        true -> 
            EmbedChanged = case proplists:get_value(oembed_url, Props) of
                Empty when Empty == undefined; Empty == <<>>; Empty == [] ->
                    % Delete the media record iff the media mime type is our mime type
                    case m_media:identify(Id, Context) of
                        {ok, Props} ->
                            case proplists:get_value(mime, Props) of
                                ?OEMBED_MIME -> 
                                    m_media:delete(Id, Context),
                                    true;
                                _ -> 
                                    false
                            end;
                        _ ->
                            false
                    end;
                EmbedUrl ->
                    MediaProps = [
                        {mime, ?OEMBED_MIME},
                        {oembed_url, EmbedUrl}
                    ],

                                   %% FIXME oembed lookup here..

                    case m_media:get(Id, Context) of
                        undefined ->
                            ok = m_media:replace(Id, MediaProps, Context),
                            preview_create(Id, MediaProps, Context),
                            true;
                        OldMediaProps ->
                            case        z_utils:are_equal(proplists:get_value(mime, OldMediaProps), ?OEMBED_MIME)
                                andalso z_utils:are_equal(proplists:get_value(oembed_url, OldMediaProps), EmbedUrl) of
                                true ->
                                    %% Not changed
                                    false; 
                                false -> 
                                    %% Changed, update the medium record
                                    ok = m_media:replace(Id, MediaProps, Context),
                                    preview_create(Id, MediaProps, Context),
                                    true
                            end
                    end
            end,

            Props1 = proplists:delete(oembed_urlProps),
            {Changed or EmbedChanged, Props1};
        false ->
            {Changed, Props}
    end.


%% @doc Return the media viewer for the embedded video (that is, when it is an embedded media).
%% @spec observe_media_viewer(Notification, Context) -> undefined | {ok, Html}
observe_media_viewer({media_viewer, _Id, Props, _Filename, _Options}, _Context) ->
    case proplists:get_value(mime, Props) of
        ?OEMBED_MIME ->
            {ok, "<h1>OEMBED</h1>"};
        _ ->
            undefined
    end.


%% @doc Return the filename of a still image to be used for image tags.
%% @spec observe_media_stillimage(Notification, _Context) -> undefined | {ok, Filename}
observe_media_stillimage({media_stillimage, _Id, Props}, _Context) ->
    case proplists:get_value(mime, Props) of
        ?OEMBED_MIME ->
            {ok, "lib/images/youtube.jpg"};
        _ ->
            undefined
    end.


%% @doc Handle the form submit from the "new media" dialog.  The form is defined in templates/_media_upload_panel.tpl.
%% @spec event(Event, Context1) -> Context2
event({submit, {add_video_embed, EventProps}, _TriggerId, _TargetId}, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    Stay = z_convert:to_bool(proplists:get_value(stay, EventProps, false)),
    EmbedUrl = z_context:get_q_validated("oembed_url", Context),
    ?DEBUG(EmbedUrl),
    case Id of
        %% Create a new page
        undefined ->
            SubjectId = proplists:get_value(subject_id, EventProps),
            Predicate = proplists:get_value(predicate, EventProps, depiction),
            Title   = z_context:get_q_validated("title", Context),
            Props = [
                {title, Title},
                {is_published, true},
                {category, video},
                {oembed_url, EmbedUrl}
            ],
            ?DEBUG(Props),
            F = fun(Ctx) ->
                case m_rsc:insert(Props, Context) of
                    {ok, MediaRscId} ->
                        case SubjectId of
                            undefined -> nop;
                            _ -> m_edge:insert(SubjectId, Predicate, MediaRscId, Ctx)
                        end,
                        {ok, MediaRscId};
                    {error, Error} ->
                        throw({error, Error})
                end
            end,

            case z_db:transaction(F, Context) of
                {ok, MediaId} ->
                    spawn(fun() -> preview_create(MediaId, Props, Context) end),
                    ContextRedirect = case SubjectId of
                        undefined ->
                            case Stay of
                                false -> z_render:wire({redirect, [{dispatch, "admin_edit_rsc"}, {id, MediaId}]}, Context);
                                true -> Context
                            end;
                        _ -> Context
                    end,
                    z_render:wire([{dialog_close, []}, {growl, [{text, "Made the media page."}]} | Actions], ContextRedirect);
                {rollback, {_Error, _Trace}} ->
                    ?ERROR("~p~n~p", [_Error, _Trace]),
                    z_render:growl_error("Could not create the media page.", Context)
            end;
        
        %% Update the current page
        N when is_integer(N) ->
            Props = [
                {category, video},
                {oembed_url, EmbedUrl}
            ],
            case m_rsc:update(Id, Props, Context) of
                {ok, _} ->
                    z_render:wire([{dialog_close, []} | Actions], Context);
                {error, _} ->
                    z_render:growl_error("Could not update the page with the new embed code.", Context)
            end
    end.
    

%%====================================================================
%% support functions
%%====================================================================

%% Fetch or create a preview for the movie
preview_create(MediaId, InsertProps, Context) ->
    ok.
%%preview_youtube(MediaId, InsertProps, z_context:prune_for_async(Context)) end);

% @doc Fetch the preview image of a youtube video. The preview is located at: http://img.youtube.com/vi/[code]/0.jpg
% @todo Make this more robust wrt http errors.
%% preview_youtube(MediaId, InsertProps, Context) ->
%%     case z_convert:to_list(proplists:get_value(video_embed_code, InsertProps)) of
%%         [] -> nop;
%%         Embed ->
%%             case re:run(Embed, "youtube(\-nocookie)?\\.com/v/([^\?\"'&]+)", [{capture,[2],list}]) of
%%                 {match, [Code]} ->
%%                     Url = "http://img.youtube.com/vi/"++Code++"/0.jpg",
%%                     case http:request(Url) of
%%                         {ok, {_StatusLine, _Header, Data}} ->
%%                             %% Received the preview image, move it to a file.
%%                             m_media:save_preview(MediaId, Data, "image/jpeg", Context);
%%                         {error, _Reason} ->
%%                             %% Too bad - no preview available - ignore for now (see todo above)
%%                             nop
%%                     end;
%%                 _ ->
%%                     nop
%%             end
%%     end.
