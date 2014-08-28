{% extends "page.tpl" %}

{% block below_body %}

{% wire id="contact-form" type="submit" postback={contact} delegate="controller_default_contact" %}
<form id="contact-form" method="post" action="postback" class="form">

	<div class="form-group">
	<label class="control-label" for="name">Name</label>
		<div>
			<input type="text" name="name" id="name" class="col-lg-4 col-md-4 form-control" />
		</div>
	</div>

	<div class="form-group">
	<label class="control-label" for="mail">E-mail</label>
		<div>
			<input type="text" name="mail" id="mail" class="col-lg-4 col-md-4 form-control" />
			{% validate id="mail" type={email} type={presence} %}
		</div>
	</div>

	<div class="form-group">
	<label class="control-label" for="message">Message</label>
		<div>
		<textarea name="message" id="message" cols="60" rows="8" class="col-lg-4 col-md-4 form-control"></textarea>
			{% validate id="message" type={presence} %}
		</div>
	</div>

	<div class="form-group">
		<div>
			<button class="btn btn-primary" type="submit">Send</button>
		</div>
	</div>

</form>

{% endblock %}
