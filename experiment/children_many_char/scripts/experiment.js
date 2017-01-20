function makeExperiment() {

	// make color palette
	// using http://tools.medialab.sciences-po.fr/iwanthue/
	var bgColors = ["#94ebda", "#f3c5fa", "#bce98d", "#d5d0fa", "#97eca5", "#ead1ec", "#d1e48e", "#bfd6fa", "#f0d27e", "#3eeaef", "#fdcc93", "#64f8f4", "#fde096", "#79e3f9", "#e3df8e", "#a7dcf9", "#b5eaa1", "#f3d0d9", "#5ef2da", "#f2cdb9", "#8cf2c3", "#fddbd0", "#84eced", "#f0d9a2", "#b4dbe7", "#cae5a1", "#d2dcee", "#aceab4", "#c2e9f5", "#dedfa2", "#a9e7e4", "#f2e1bc", "#9be4c0", "#e3d3af", "#a9f2ce", "#e3dbc8", "#c3e5b5", "#cde8e2", "#d0d9ae", "#bae6c8", "#dee7bc", "#d0e0c8"]

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
			sessionDuration: "",
			testingSite: "",
			experimenter: "",
			character: "",
			charWording: "",

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
			var dataForExport = experiment.allData.trialData;

			// code source: http://halistechnology.com/2015/05/28/use-javascript-to-export-your-data-as-csv/
			function convertArrayOfObjectsToCSV(args) {
				var result, ctr, keys, columnDelimiter, lineDelimiter, data;

				data = args.data || null;
				if (data == null || !data.length) {
					return null;
				}

		        columnDelimiter = args.columnDelimiter || ',';
		        lineDelimiter = args.lineDelimiter || '\n';

		        keys = Object.keys(data[0]);

		        result = '';
		        result += keys.join(columnDelimiter);
		        result += lineDelimiter;

		        data.forEach(function(item) {
		            ctr = 0;
		            keys.forEach(function(key) {
		                if (ctr > 0) result += columnDelimiter;

		                result += item[key];
		                ctr++;
		            });
		            result += lineDelimiter;
		        });

		        return result;
		    }

		    function downloadCSV(args) {
		        var data, filename, link;

		        // get trial-level info
		        var objArray = experiment.allData.trialData

		        // add subject-level info
			    for (trial in objArray) {

			    	var date = new Date();
			    	var endTimeMS = date.getTime();

			    	// auto-filled
			    	objArray[trial].character = experiment.allData.character.charName;
			    	objArray[trial].charWording = $("span#pronounChar").text();
			    	objArray[trial].subid = experiment.allData.subid;
			    	objArray[trial].dateOfTest = experiment.allData.dateOfTest;
			    	objArray[trial].startTime = experiment.allData.startTime;
			    	objArray[trial].endTime = date.getHours()+":"+date.getMinutes()+":"+date.getSeconds();
			    	objArray[trial].sessionDuration = (endTimeMS - startTimeMS)/60000;

			    	// manually entered (later)
			    	objArray[trial].testingSite = experiment.allData.testingSite;
			    	objArray[trial].experimenter = experiment.allData.experimenter;
			    	objArray[trial].gender = experiment.allData.gender;
			    	objArray[trial].dateOfBirth = experiment.allData.dateOfBirth;
			    	objArray[trial].ethnicity = experiment.allData.ethnicity;
			    	objArray[trial].trialComments = experiment.allData.trialComments;
			    	objArray[trial].sessionComments = experiment.allData.sessionComments;
			    };

		        var csv = convertArrayOfObjectsToCSV({
		            data: objArray
		        });
		        if (csv == null) return;

		        filename = args.filename || 'export.csv';

		        if (!csv.match(/^data:text\/csv/i)) {
		            csv = 'data:text/csv;charset=utf-8,' + csv;
		        }
		        data = encodeURI(csv);

		        link = document.createElement('a');
		        link.setAttribute('href', data);
		        link.setAttribute('download', filename);
		        link.click();
		    }

		    downloadCSV({data: dataForExport, filename: experiment.allData.subid + ".csv" });

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

					// disable all buttons for 250ms
					$(".slide#stage button").prop("disabled", true);
					// var btn = $(this);
    	// 			btn.prop('disabled', true);
    				setTimeout(function(){
    					$(".slide#stage button").prop('disabled', false);
    				}, 250);

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

