document.addEventListener("DOMContentLoaded", function(event) { 
	//sends the input text to the server to process it 
	//so to receive a coloured version of the text
	document
	.getElementById("textSendButton")
	.addEventListener("click", function(event) {
		var textInput = document.getElementById("textInput");
		var textHTML = textInput.innerHTML;
		var textValue = 
			textInput
			.innerText
			.replace(/\d* occurrences/g, '')
			.replace(/\u00A0/g, ' ');//this replaces all non breaking spaces in unicode format into normal white spaces. The non-breaking spaces interfere with the java/scala server parser in such a way that the parser doesnt recognize a nbsp as a character pertaining to the \s character class. Since the server inserts nbsp in the response to the post request if the client sends the response back to the server as an input it will on separating words! This was a headache to fix.
			
		var str_json = JSON.stringify({text: textValue});
		var request= new XMLHttpRequest();
		request.open("POST", window.location.href+"result", true);
		request.setRequestHeader("Content-type", "application/json");
		request.onreadystatechange = function() {
			if (this.readyState == XMLHttpRequest.DONE 
				&& this.status == 200) {
				var coloredText = this.response;
				textInput	
				.innerHTML = coloredText;

				positionObjects();
			}
		};
		console.log(textValue);
		request.send(str_json);
	});

				



});
