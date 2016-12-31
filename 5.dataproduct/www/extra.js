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
	
// 	console.log(data.local)
// 	$.each(data.local["sentence"], function(k,v){
// 		console.log(v)
// 	});

	update_typeahead(data.id, data.local, data.valueKey, data.tokens, data.template, data.limit, data.placeholder);

	el.typeahead('setQuery', el.val()).focus();
// 	$(".tt-hint").val(data.local["sentence"][0]);
} 


