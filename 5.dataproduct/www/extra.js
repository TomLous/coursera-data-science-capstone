$(document).on('shiny:connected', function(event) {
//   alert('Connected to the server');
	$('input.typeahead').on('keyup', this, function (event) {
	console.log($('input.typeahead').val());
 });
});

