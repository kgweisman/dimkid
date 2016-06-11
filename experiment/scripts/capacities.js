function makeCapacities() {

	// make new capacity function
	function addCapacity(capName, wording, definition) {
		function Capacity(capName, wording, definition) {
			this.capName = capName;
			this.wording = wording;
			this.definition = definition;
		};
		newCapacity = new Capacity(capName, wording, definition);
		capacities[newCapacity.capName] = newCapacity;
	};

	// make capacities
	capacities = {};
	// note: capNames taken from ggw-mod2 study (with adult items)
	addCapacity("happy", "feel happy", "like when you're feeling happy");
	addCapacity("depressed", "feel sad", "like when you're feeling sad");
	addCapacity("fear", "feel scared", "like when you're feeling afraid");
	addCapacity("angry", "get angry", "like when you're feeling mad");
	addCapacity("calm", "feel calm", "like when you're feeling relaxed and quiet");
	addCapacity("sounds", "hear sounds", "like when you hear a noise");
	addCapacity("seeing", "see things", "like when you see something");
	addCapacity("temperature", "sense temperatures", "like when you feel warm or cold");
	addCapacity("odors", "smell things", "like when you can smell something");
	addCapacity("depth", "sense whether something is close by or far away", "like when you can tell how far away something is");
	addCapacity("computations", "do math", "like when you add or subtract numbers");
	addCapacity("thoughts", "have thoughts", "like when you're thinking about something");
	addCapacity("reasoning", "figure out how to do things", "like when you're trying to figure something out");
	addCapacity("remembering", "remember things", "like when you remember something that happened before");
	addCapacity("beliefs", "have beliefs", "like when you think something is true");
	addCapacity("hungry", "get hungry", "like when you feel like you need to eat something");
	addCapacity("tired", "feel tired", "like when you feel like you need to go to sleep");
	addCapacity("pain", "feel pain", "like when something hurts");
	addCapacity("nauseated", "feel sick", "like when you feel like you might sneeze or throw up");
	addCapacity("safe", "feel safe", "like when you know that you're okay and you're not in danger");
	addCapacity("love", "feel love", "like when you really really love somebody");
	addCapacity("recognizing", "recognize somebody else", "like when you know who somebody is");
	addCapacity("communicating", "communicate with somebody else", "like when you're talking with somebody");
	addCapacity("guilt", "feel guilty", "like when you feel bad because you did something mean");
	addCapacity("disrespected", "get hurt feelings", "like when you feel bad because somebody did something mean to you");
	addCapacity("free_will", "decide what to do", "like when you choose to do something or not to do it");
	addCapacity("choices", "make choices", "like when you choose between different things");
	addCapacity("self_restraint", "control itself", "like when you're in control of what you're doing");
	addCapacity("intentions", "make plans", "like when you are planning to do something");
	addCapacity("goal", "have goals", "like when you're trying really hard to do something");
	addCapacity("conscious", "be aware of things", "like when you're conscious and you know what's going on");
	addCapacity("self_aware", "be aware of itself", "like when you are thinking about yourself");
	addCapacity("desires", "have desires", "like when you really want something");
	addCapacity("embarrassed", "feel embarrassed", "like when you feel embarrassed about something that happened to you");
	addCapacity("emo_recog", "understand how somebody else is feeling", "like when somebody gets hurts and you know how they're feeling");
	addCapacity("joy", "feel joy", "like when you're feeling really, really, really happy");
	addCapacity("morality", "know what's nice and what's mean", "like when you know what would be nice to do and what would be mean to do");
	addCapacity("personality", "have a personality", "like what makes you you");
	addCapacity("pleasure", "feel pleasure", "like when something feels really good");
	addCapacity("pride", "feel proud", "like when you feel really good about something you did");

	// make list of capacities
	capList = Object.keys(capacities).map(function (key) {return capacities[key]});
	capListLength = Object.keys(capList).length;

};

makeCapacities();

