function makeExperiment() {

	// make color palette
	// using http://tools.medialab.sciences-po.fr/iwanthue/
	var bgColors = ["#d1f3c2", "#edddf6", "#d1ffa1", "#d9e3f5", "#c3f094", "#f6dce3", "#37fcea", "#f5ed92", "#64f8f4", "#f4e7a7", "#8bf3f4", "#dff29b", "#c2e9f5", "#a4f6a7", "#fddbd0", "#7bf8d4", "#f2e1bc", "#99f6bb", "#f2e9d6", "#c3f8af", "#a9eef5", "#e0fab6", "#9bf3e1", "#d1eca8", "#d4efe9", "#b3f1bb", "#c4fcf0", "#b3f1bb", "#b6eee2", "#b3f1bb", "#e2eecc", "#9bf3e1", "#b3f1bb", "#9bf3e1", "#c8f4d6", "#9bf3e1", "#a9f2ce", "#9bf3e1", "#9bf3e1", "#9bf3e1"]

	experiment = {
		// array for making each new trial
		trials: capList,

		// where to store all the data
		allData: {

			// session information
			subid: "",
			dateOfTest: "",
			startTime: "",
			endTime: "",
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

			    	var date = new Date();

			    	// auto-filled
			    	objArray[trial].character = experiment.allData.character.charName;
			    	objArray[trial].subid = experiment.allData.subid;
			    	objArray[trial].dateOfTest = experiment.allData.dateOfTest;
			    	objArray[trial].startTime = experiment.allData.startTime;
			    	objArray[trial].endTime = date.getHours()+":"+date.getMinutes()+":"+date.getSeconds();

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
			    	bgColor: "bgColor",
			    	capacity: "capacity",
			    	wording: "wording",
			    	response: "response",
			    	responseNum: "responseNum",
			    	hoverTime: "hoverTime",
			    	rt: "rt",

			    	// auto-filled subject-level info
			    	character: "character",
			    	subid: "subid",
			    	dateOfTest: "dateOfTest",
			    	startTime: "startTime",
			    	endTime: "endTime",

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
					bgColor: "",
					capacity: "",
					wording: "",
					response: "",
					responseNum: NaN,
					hoverTime: 0,
					rt: NaN
				};

				// assign capacity
				var chosenCap = randomElementNR(this.trials);
				data.capacity = chosenCap.capName;
				data.wording = chosenCap.wording;

				// set text for this trial
				$("span#question").text(data.wording);

				// set tooltip definition for this trial
				if (data.trialNum === 1) {
					$('[data-toggle="tooltip"]').tooltip({title: chosenCap.definition, placement: "bottom"}); 
				} else {
				    $('#definition').attr('data-original-title', chosenCap.definition);
				}

				// record tooltip shown events
				var hoverTime = 0;
				$('#definition').on('shown.bs.tooltip', function () {
					var hoverStart = (new Date()).getTime();
					$('#definition').on('hidden.bs.tooltip', function () {
						var hoverEnd = (new Date()).getTime();
						hoverTime = hoverTime + (hoverEnd - hoverStart);
					});
				});

				// display progress bar
				var percentComplete = (data.trialNum - 1)/(capListLength + 1) * 100;
				var percentCompleteRounded = Math.round(percentComplete);
				$("#trial-num").text("trial "+data.trialNum.toString()+" of 78: "+percentCompleteRounded+"% complete");
				$("#stage .progress-bar").attr("aria-valuenow", percentComplete.toString());
				$("#stage .progress-bar").css("width", percentComplete.toString()+"%");

				// change the background color and progress bar color
				data.bgColor = randomElementNR(bgColors);
				document.body.style.backgroundColor = data.bgColor;
				if (data.trialNum !== 1) {
					$(".progress-bar").css("background-color", data.bgColor);
				}

				// show trial
				showSlide("stage");

				// record response and rt
				var trialStart = (new Date()).getTime();

				var clickHandler = function(event) {
					var trialEnd = (new Date()).getTime();
					data.rt = trialEnd - trialStart;
					data.hoverTime = hoverTime;
					experiment.allData.trialData.push(data);
				};

				$(".slide#stage button").click(function() { 
					// record response
					data.response = $(this).attr("id");

					// recode response as number
					switch(data.response) {
						case "no":
							data.responseNum = 0;
							break;
						case "kinda":
							data.responseNum = 0.5;
							break;
						case "yes":
							data.responseNum = 1;
							break;
						default:
							data.responseNum = "NA";
					}

					// end trial
					clickHandler();
					$(".slide#stage button").unbind().blur();
					window.scrollTo(0, 0);
					experiment.next();
				})

				$(".slide#stage button#bail").click(function() { 
					// record response
					data.response = $(this).attr("id");

					// recode response as number
					data.responseNum = "NA";

					// end experiment
					$(".slide#stage button").unbind().blur();
					window.scrollTo(0, 0);
					experiment.end();
				})

			}

		}

	}
};

makeExperiment();

