function arrContainsAll(arr, els) {
	var e;
	var i = 0;
	for (; i < els.length; i++) {
		e = els[i];
		if (arr.indexOf(e) == -1) 
			return false;
	}

	return true;
} 

function arrContainsAny(arr, els) {
	var e;
	var i = 0;
	for (; i < els.length; i++) {
		e = els[i];
		if (arr.indexOf(e) > -1) 
			return true;
	}

	return false;
}


function getWordFromWordInfo(wordInfo) {
	return wordInfo.getElementsByClassName("word")[0].textContent;
}

function getElementContent(e) {
	var elClassNames = e.className.split(/\s+/);

	var isClasses = function(classes) {
		return arrContainsAll(elClassNames, classes);	
	};

	if (isClasses(["wordInfo"])) {
		return getWordFromWordInfo(e);	
	} else if (isClasses(["tab"])) {
		return "\t";
	} else if (isClasses(["space"])) {
		return " ";
	} else if (isClasses(["punctuation"])) {
		return e.textContent;
	} else if (e.tagName == "BR") {
		return "\n";
	} else {
		return textFromElement(e);	
	}
}

function isNormal(e) {
	var elClassNames = e.className.split(/\s+/);
	return !arrContainsAny(elClassNames, ["wordInfo", "tab", "space", "punctuation"]);
}

/*returns a string with the textInput content*/
function textFromTextInput() {
	return textFromElement(document.getElementById("textInput"));
}

function textFromElement(e) {
	var childrenCollection = [].slice.call(e.children);
	
	if (childrenCollection.length == 0) {
		return e.textContent;
	}

	return childrenCollection.reduce(function(acc, child) {
		return acc + getElementContent(child);
	}, "").replace(/&nbsp;/, ' ');
}

function createJSONContent() {
		var textValue = textFromTextInput();
			
		var isDaltonianValue = "false";

		if (document.getElementById("daltonian").checked)
			isDaltonianValue = "true";

		var langSelect =
			 document
			.getElementById("languageChoice")
		var selectedLangVal = langSelect.options[langSelect.selectedIndex].value 

		var json = 
			{
				text: textValue,
				isDaltonian: isDaltonianValue,
				selectedLang: selectedLangVal
			};

		return JSON.stringify(json);
}
document.addEventListener("DOMContentLoaded", function(event) { 
	//sends the input text to the server to process it 
	//so to receive a coloured version of the text
	document
	.getElementById("textSendButton")
	.addEventListener("click", function(event) {

		var str_json = createJSONContent();
		var request= new XMLHttpRequest();
		request.open("POST", window.location.href+"result", true);
		request.setRequestHeader("Content-type", "application/json");
		request.onreadystatechange = function() {
			if (this.readyState == XMLHttpRequest.DONE 
				&& this.status == 200) {
				var coloredText = this.response;
				textInput	
				.innerHTML = coloredText;
			}
		};
		request.send(str_json);
	});

});
