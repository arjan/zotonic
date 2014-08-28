{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing SEO preferences #}

{% block widget_title %}{_ SEO Content _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-seo{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<div class="form-group">
        <div class="checkbox"><label class="control-label" for="custom-slug">
        {_ Page slug _}
    </label>
    <div>
        <input class="checkbox" id="custom-slug" type="checkbox" name="custom_slug" {% if r.custom_slug %}checked="checked"{% endif %} value="1" onclick="$('#custom-slug:checked').val() ? $('#slug').removeAttr('disabled') : $('#slug').attr('disabled', 'disabled');" title="{_ Customize page slug _}" />

        <input type="text" id="slug" name="slug" value="{{ r.slug }}" {% if not r.custom_slug %}disabled="disabled"{% endif %} class="input-xlarge form-control" />
    </div>
</div>

<div class="control-group">
    <div class="controls">
        <div class="checkbox"><label>
            <input id="seo_noindex" type="checkbox" class="do_fieldreplace" name="seo_noindex" {% if r.seo_noindex %}checked="checked"{% endif %} value="1" />
        {_ Ask google to not index this page _}
        </label></div></div>
    </div>            
</div>

<div class="form-group">
    <label class="control-label" for="seo_title">{_ Page title _}</label>
    <div>
        <input class="form-control" type="text" id="seo_title" name="seo_title" value="{{ r.seo_title }}"/>
    </div>
</div>

<div class="form-group">
    <label class="control-label" for="seo_keywords">{_ Page keywords _}</label>
    <div>
        <input class="form-control" type="text" id="seo_keywords" name="seo_keywords" value="{{ r.seo_keywords }}"/>
    </div>
</div>

<div class="form-group">
    <label class="control-label" for="seo_desc">{_ Page description _}</label>
    <div>
        <textarea rows="5" cols="10" id="seo_desc" name="seo_desc" class="seo-desc form-control">{{ r.seo_desc }}</textarea>
    </div>
</div>

{% all catinclude "_admin_edit_content_seo_extra.tpl" r.id %}

{% endwith %}
{% endblock %}
