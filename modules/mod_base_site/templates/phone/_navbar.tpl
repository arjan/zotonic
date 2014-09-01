{# navbar for phone+ #}
<nav class="navbar navbar-inverse navbar-fixed-top">
    <div class="container">
        <div class="navbar-header">
            <!-- .btn-navbar is used as the toggle for collapsed navbar content -->
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target=".nav-collapse">
                <span class="glyphicon glyphicon-bar"></span>
                <span class="glyphicon glyphicon-bar"></span>
                <span class="glyphicon glyphicon-bar"></span>
            </button>

            <a class="navbar-brand" href="/"><img src="/lib/images/logo.png" class="logo" alt="" /> {{ m.config.site.title.value }}</a>
        </div>
        
        <div class="navbar-collapse">
            <form class="navbar-form pull-right" method="get" action="{% url search %}">
                <input type="text" class="search-query form-control" placeholder="Search" name="qs"/>
            </form>
			<div class="pull-right">
                {% include "_navbar_right.tpl"%}
            </div>        
			{% menu menu_id=menu_id id=id maxdepth=2 %}
        </div>
    </div>
</nav>
