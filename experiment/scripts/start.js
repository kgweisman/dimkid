// set up front page slide
showSlide("front");

// $(".slide#front button").click(function() {
// 	showSlide("introduction");
// });

$(".slide#introduction button").click(function() {
	experiment.next();
	window.scrollTo(0, 0);
	showSlide("stage");
});

$('.slide#front button').click(function() {

	// if no subid, prevent progress
	if($('input#subid').val() === "") {
		window.alert("Please enter a subid.");

	} else {
		// record subid in experiment object
		experiment.allData.subid = $('input#subid').val();

		// record or randomly select character
		if ($('input#character').val() === "random") {
			experiment.allData.character = randomElementNR(charList);
		} else if (["beetle","robot"].indexOf($('input#character').val()) === -1) {
			window.alert("Please choose: beetle, robot, or random.");
		} else {
			var chosenChar = $('input#character').val();
			experiment.allData.character = characters[chosenChar];
		}

		// set up introduction slide
		showSlide("introduction");
	}
})

