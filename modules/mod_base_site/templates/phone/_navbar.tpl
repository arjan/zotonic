{# navbar for phone+ #}
<nav class="navbar navbar-fixed-top">
  <div class="container">
    <div class="container">
        <!-- .btn-navbar is used as the toggle for collapsed navbar content -->
        <a class="btn btn-default navbar-btn" data-toggle="collapse" data-target=".nav-collapse">
          <span class="glyphicon glyphicon-bar"></span>
          <span class="glyphicon glyphicon-bar"></span>
          <span class="glyphicon glyphicon-bar"></span>
        </a>

        <a class="navbar-brand" href="/">
            <span class="zotonic-logo"></span>
            {{ m.config.site.title.value }}
        </a>
        
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
  </div>
</nav>
