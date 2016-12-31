$(document).on('shiny:connected', function(event) {
    console.log('Connected to the server');

	// send typed text to server
	$('input.typeahead').on('keyup', this, function (event) {
		var typedText = $('input.typeahead').val()
		console.log(typedText);

			Shiny.onInputChange("typedText", typedText);

	});
	
	// receive updated suggestions
	Shiny.addCustomMessageHandler("updateSuggestions",update_suggest);        

});




function update_suggest(data) {
	var el = $("[data-name='" + data.id + "']");

	update_typeahead(data.id, data.local, data.valueKey, data.tokens, data.template, data.limit, data.placeholder);

	el.typeahead('setQuery', el.val()).focus();
} 


