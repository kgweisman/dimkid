function startStudy() {
	// set up front page slide
	showSlide("front");

	// set up button behaviors
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

			// randomly select character
			experiment.allData.character = randomElementNR(charList);
			
			// set up introduction and stage slides
			$("span#pronounChar").text(experiment.allData.character.pronoun+" "+experiment.allData.character.singular);
			$("span#pluralChar").text(experiment.allData.character.plural);
			$("img#charImage").attr("src", experiment.allData.character.imageSource);
			
			// set image size depending on character
			switch(experiment.allData.character.charName) {
				case "beetle":
				case "bird":
				case "computer":
				case "elephant":
				case "goat":
				case "mouse":
					$('img#charImage').width(270);
					$('img#charImage').height(180);
					break;
				case "doll":
				case "robot":
					$('img#charImage').width(180);
					$('img#charImage').height(270);
					break;
				case "teddy_bear":
					$('img#charImage').width(220);
					$('img#charImage').height(220);
			}

			showSlide("introduction");
			
			// record date and start time
			var date = new Date();
			experiment.allData.dateOfTest = date.getMonth()+1+"/"+date.getDate()+"/"+date.getFullYear();
			experiment.allData.startTime = date.getHours()+":"+date.getMinutes()+":"+date.getSeconds();
			startTimeMS = date.getTime();
		}
	});

}

startStudy();


