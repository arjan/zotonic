{# Panel for defining the embed code #}
{% with id.medium as medium %}
{% with medium.mime == "text/html-oembed" as is_oembed %}
<div id="{{ tab }}-oembed">
	<p>Embed a video or other media. Here you can paste embed code from YouTube, Vimeo or other services.</p>

	{% wire id=#form type="submit" postback={add_video_embed predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate="mod_oembed" %}

	<form id="{{ #form }}" method="POST" action="postback">
		<div class="new-media-wrapper">
			{% if not id %}
				<div class="form-item clearfix">
					<label for="{{ #title }}" style="color:white">Media title</label>
					<input type="text" id="{{ #title }}" name="title" value="{{ title|escape }}" />
					{% validate id=#title name="title" type={presence} %}
				</div>
			{% endif %}

			<div class="form-item clearfix">
				<label for="{{ #embed_code }}">Embed URL</label>
				<input id="{{ #embed_code }}" name="oembed_url" value="{{ medium.oembed_url }}" />
				{% validate id=#embed_code name="oembed_url" type={presence} %}
			</div>
	
			<div class="form-item clearfix">
				<button type="submit">{% if id %}Replace{% else %}Make{% endif %} media item</button>
				{% button action={dialog_close} text="Cancel" %}
			</div>
		</div>
	</form>
</div>

{% if is_oembed %}
	{% wire action={script script=["$('#",tabs,"').tabs().tabs('select', '#",#tab,"-oembed')"]} %}
{% endif %}

{% endwith %}
{% endwith %}
