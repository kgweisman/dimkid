function makeCharacters() {
	// make new character function
	function addCharacter(charName, pronoun, plural) {
		function Character(charName, pronoun, plural) {
			this.charName = charName;
			this.pronoun = pronoun;
			this.plural = plural;
			this.imageSource = "images_characters/"+charName+".jpeg";
		};
		newCharacter = new Character(charName, pronoun, plural);
		characters[newCharacter.charName] = newCharacter;
	};

	// make characters
	characters = {};
	addCharacter("computer", "a", "computers");
	addCharacter("robot", "a", "robots");
	addCharacter("beetle", "a", "beetles");
	addCharacter("bird", "a", "birds");
	addCharacter("mouse", "a", "mice");
	addCharacter("goat", "a", "goats");
	addCharacter("elephant", "a", "elephants");

	// make list of characters
	charList = Object.keys(characters).map(function (key) {return characters[key]});

};

makeCharacters();

