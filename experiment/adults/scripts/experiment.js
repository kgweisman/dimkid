function makeExperiment() {

	// make color palette
	// using http://tools.medialab.sciences-po.fr/iwanthue/
	var bgColors = ["#94ebda", "#f3c5fa", "#bce98d", "#d5d0fa", "#97eca5", "#ead1ec", "#d1e48e", "#bfd6fa", "#f0d27e", "#3eeaef", "#fdcc93", "#64f8f4", "#fde096", "#79e3f9", "#e3df8e", "#a7dcf9", "#b5eaa1", "#f3d0d9", "#5ef2da", "#f2cdb9", "#8cf2c3", "#fddbd0", "#84eced", "#f0d9a2", "#b4dbe7", "#cae5a1", "#d2dcee", "#aceab4", "#c2e9f5", "#dedfa2", "#a9e7e4", "#f2e1bc", "#9be4c0", "#e3d3af", "#a9f2ce", "#e3dbc8", "#c3e5b5", "#cde8e2", "#d0d9ae", "#bae6c8", "#dee7bc", "#d0e0c8"]

	experiment = {
		// array for making each new trial
		trials: capList,

		// where to store all the data
		allData: {

			// fingerprinting information
			fingerprintData: {},

			// session information
			subid: "",
			dateOfTest: "",
			startTime: "",
			endTime: "",
			sessionDuration: "",
			character: "",
			charWording: "",

			// demographic information about participant
			age: "",
			gender: "",
			job: "",
			education: "",
			ethnicity: [],
			religionChild: [],
			religionNow: [],
			country: "",
			englishNative: "",

			// comments
			comments: "",

			// trial by trial data
			trialData: [],
		},

		// show "end" slide when participant has seen all trials
		end: function() {

			// show demographics slide	
			showSlide("demographics");
			
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
					capWording: "",
					response: "",
					responseNum: NaN,
					hoverTime: 0,
					rt: NaN
				};

				// assign capacity
				var chosenCap = randomElementNR(this.trials);
				data.capacity = chosenCap.capName;
				data.capWording = chosenCap.wording;

				// set text for this trial
				$("span#question").text(data.capWording);

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
				    $('#definition').attr('data-original-title', " ");
					// $('[data-toggle="tooltip"]').tooltip({title: "", placement: "bottom"}); 
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
				});

			}

		}

	}
};

makeExperiment();

