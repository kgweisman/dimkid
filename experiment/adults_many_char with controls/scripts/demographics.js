function demographics() {
	// set up button behavior
	$('.slide#demographics button').click(function() {

		// record character wording
		experiment.allData.charWording = $("span#pronounChar").text();

		// record demographic info...
		// text inputs
		experiment.allData.age = $('input#age', '#demographicsForm').val();
		experiment.allData.job = $('input#job', '#demographicsForm').val(); 
		experiment.allData.country = $('input#country', '#demographicsForm').val();

		// // text areas
		experiment.allData.summarize = $('.slide#demographics textarea#summarize').val();
		experiment.allData.comments = $('.slide#demographics textarea#comments').val();

		// multiple choice radios
		experiment.allData.gender = $('input[name=gender]:checked', '#demographicsForm').val();
		experiment.allData.education = $('input[name=education]:checked', '#demographicsForm').val();
		experiment.allData.englishNative = $('input[name=englishNative]:checked', '#demographicsForm').val();

		// multiple answer checkboxes
		$('input[name=ethnicity]:checked', '#demographicsForm').each(function() {
			experiment.allData.ethnicity.push($(this).val());
		});
		$('input[name=religionChild]:checked', '#demographicsForm').each(function() {
			experiment.allData.religionChild.push($(this).val());
		});
		$('input[name=religionNow]:checked', '#demographicsForm').each(function() {
			experiment.allData.religionNow.push($(this).val());
		});

		// record end time and session duration
		var date = new Date();
		experiment.allData.endTime = date.getHours()+":"+date.getMinutes()+":"+date.getSeconds();
		endTimeMS = date.getTime();
		experiment.allData.sessionDuration = (endTimeMS - startTimeMS)/60000;

		// set up end page
		// include "opener." because experiment has been running in new window
		opener.turk.submit(experiment);
		window.scrollTo(0, 0);
		showSlide("end");
	});
};

demographics();

