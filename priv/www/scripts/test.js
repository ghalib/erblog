function say(text) {
  alert(text);
}

$(function() {
    $('a.navlink')
      .mouseover(function(event){
	alert(event);
	  });
  });

