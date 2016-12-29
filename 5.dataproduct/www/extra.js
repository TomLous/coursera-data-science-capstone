$(document).on('shiny:connected', function(event) {
    console.log('Connected to the server');

	// send typed text to server
	$('input.typeahead').on('keyup', this, function (event) {
		var typedText = $('input.typeahead').val()
		console.log(typedText);
		Shiny.onInputChange("typedText", typedText);
	});
	
	// receive updated suggestions
	Shiny.addCustomMessageHandler("updateSuggestions",
        function(suggestions) {
          console.log(suggestions);
        });
        
 
});




function update_suggest(id, dataset, tokens) {


  var el = $("[data-name='" + id + "']");

  // dataset is in the form of object of arrays, need to convert it to an array of objects
  var obj_of_arr = dataset
  // parse an object of arrays into array of object
  var keys = Object.keys(obj_of_arr) // obtain the keys     
  var arr_of_obj = []
  if (typeof obj_of_arr[keys[0]] === "object") {
    var l = obj_of_arr[keys[0]].length
    for (var i = 0; i < l; i++) {
      var tmpobj = {}
      keys.map(function(key) {
        tmpobj[key] = obj_of_arr[key][i]
      })
      if (typeof tokens[i] === "string") {
        tokens[i] = [tokens[i], tmpobj[valueKey]]       
        tmpobj["tokens"] = tokens[i]
      } else {
        tokens[i].push(tmpobj[valueKey])
        tmpobj["tokens"] = tokens[i]
      }
      arr_of_obj.push(tmpobj)
    }
  }  

	old_valueKey = el.valueKey;
	old_template = el.template;
	old_limit = el.limit;

  //remove previous settings
  el.typeahead("destroy")

  // new typeahead
  el.typeahead({
    local: arr_of_obj,
    valueKey: old_valueKey,
    template: old_template,
    engine: Hogan,
    limit: old_limit
  })

  // update the placeholder text
  el.attr("placeholder", placeholder)
} 


