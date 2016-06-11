function makeExperiment() {
	var date = new Date();

	experiment = {
		// array for making each new trial
		trials: capList,

		// where to store all the data
		allData: {

			// session information
			subid: "",
			dateOfTest: date.getMonth()+1+"/"+date.getDate()+"/"+date.getFullYear(),
			timeOfTest: date.getHours()+":"+date.getMinutes(),
			testingSite: "",
			experimenter: "",
			character: "",

			// randomly assign character (condition)
			// character: randomElementNR(charList),

			// demographic information about participant
			gender: "",
			dateOfBirth: "",
			ethnicity: "",

			// comments
			trialComments: "",
			sessionComments: "",

			// trial by trial data
			trialData: [],
		},

		// show "end" slide when participant has seen all trials
		end: function() {

			// show ending slide	
			showSlide("end");
			
			$('.slide#end button').click(function() { 
				// reload html to return to start slide
				location.reload();
			});

			// export data to csv
			var data = experiment.allData.trialData;
	 
			function DownloadJSON2CSV(objArray) { // code source: http://www.zachhunter.com/2010/11/download-json-to-csv-using-javascript/
			    // get trial-level info
			    var array = typeof objArray != 'object' ? JSON.parse(objArray) : objArray;

			    // add subject-level info
			    for (trial in objArray) {

			    	// auto-filled
			    	objArray[trial].character = experiment.allData.character.charName;
			    	objArray[trial].subid = experiment.allData.subid;
			    	objArray[trial].dateOfTest = experiment.allData.dateOfTest;
			    	objArray[trial].timeOfTest = experiment.allData.timeOfTest;

			    	// manually entered (later)
			    	objArray[trial].testingSite = experiment.allData.testingSite;
			    	objArray[trial].experimenter = experiment.allData.experimenter;
			    	objArray[trial].gender = experiment.allData.gender;
			    	objArray[trial].dateOfBirth = experiment.allData.dateOfBirth;
			    	objArray[trial].ethnicity = experiment.allData.ethnicity;
			    	objArray[trial].trialComments = experiment.allData.trialComments;
			    	objArray[trial].sessionComments = experiment.allData.sessionComments;
			    };

			    // add headers in a hacky way
			    objArray.unshift({
			    	// auto-filled trial-level info
			    	trialNum: "trialNum",
			    	capacity: "capacity",
			    	wording: "wording",
			    	response: "response",
			    	rt: "rt",

			    	// auto-filled subject-level info
			    	character: "character",
			    	subid: "subid",
			    	dateOfTest: "dateOfTest",
			    	timeOfTest: "timeOfTest",

			    	// manually entered (later) subject-level info
			    	testingSite: "testingSite",
			    	experimenter: "experimenter",
			    	gender: "gender",
			    	dateOfBirth: "dateOfBirth",
			    	ethnicity: "ethnicity",
			    	trialComments: "trialComments",
			    	sessionComments: "sessionComments"
			    });

			    // convert to csv
			    var str = '';
			     
			    for (var i = 0; i < array.length; i++) {
			        var line = '';
			        for (var index in array[i]) {
			            if(line != '') line += ','
			         
			            line += array[i][index];
			        }
			 
			        str += line + '\r\n';
			    }
			 
			    if (navigator.appName != 'Microsoft Internet Explorer') {
			        window.open('data:text/csv;charset=utf-8,' + escape(str));
			    } else {
			        var popup = window.open('','csv','');
			        popup.document.body.innerHTML = '<pre>' + str + '</pre>';
			    }          
			}

			DownloadJSON2CSV(data);
		},

		// set up each new trial
		next: function() {
			if (this.trials.length === 0) {
				experiment.end();
			} else { 
				// create place to store data for this trial
				var data = {
					trialNum: (capListLength + 1) - this.trials.length,
					capacity: "",
					wording: "",
					response: "",
					rt: NaN
				};

				// assign capacity
				var chosenCap = randomElementNR(this.trials);
				data.capacity = chosenCap.capName;
				data.wording = chosenCap.wording;

				// display progress bar
				var percentComplete = (data.trialNum - 1)/(capListLength + 1) * 100;
				var percentCompleteRounded = Math.round(percentComplete);
				$("#trial-num").text("trial "+data.trialNum.toString()+" of 78: "+percentCompleteRounded+"% complete");
				$("#stage .progress-bar").attr("aria-valuenow", percentComplete.toString());
				$("#stage .progress-bar").css("width", percentComplete.toString()+"%");

				// set text and images for this trial
				$(".slide#stage #question").text("Do you think "+experiment.allData.character.plural+" can "+data.wording+"?");
				$("#stage #charImage").attr("src", experiment.allData.character.imageSource);

				// show trial
				showSlide("stage");

				// record response and rt
				var startTime = (new Date()).getTime();

				var clickHandler = function(event) {
					var endTime = (new Date()).getTime();
					data.rt = endTime - startTime;
					experiment.allData.trialData.push(data);
				};

				$(".slide#stage button").click(function() { 
					// record response
					data.response = $(this).attr("id");

					// recode response as number
					// XX

					// end trial
					clickHandler();
					$(".slide#stage button").unbind().blur();
					window.scrollTo(0, 0);
					experiment.next();
				})

			}

		}

	}
};

makeExperiment();

