function makeCharacters() {
	// make new character function
	function addCharacter(charName, pronoun, singular, plural) {
		function Character(charName, pronoun, singular, plural) {
			this.charName = charName;
			this.pronoun = pronoun;
			this.singular = singular;
			this.plural = plural;
			this.imageSource = "images_characters/"+charName+".jpeg";
		};
		newCharacter = new Character(charName, pronoun, singular, plural);
		characters[newCharacter.charName] = newCharacter;
	};

	// make characters
	characters = {};
	addCharacter("computer", "a", "computer", "computers");
	addCharacter("robot", "a", "robot", "robots");
	addCharacter("beetle", "a", "beetle", "beetles");
	addCharacter("bird", "a", "bird", "birds");
	addCharacter("mouse", "a", "mouse", "mice");
	addCharacter("goat", "a", "goat", "goats");
	addCharacter("elephant", "a", "elephant", "elephants");
	addCharacter("doll", "a", "doll", "dolls");
	addCharacter("teddy_bear", "a", "teddy bear", "teddy bears");

	// make list of characters
	charList = Object.keys(characters).map(function (key) {return characters[key]});

};

makeCharacters();

