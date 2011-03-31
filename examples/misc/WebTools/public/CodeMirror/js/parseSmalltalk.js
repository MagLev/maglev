/* Simple parser for Smalltalk */

var SmalltalkParser = (function() {
	var tokenizeSmalltalk = (function() {
		var identifierTest = /[A-Za-z0-9_]/;
		function normal(source, setState) {
			var char = source.next();
			if (char == ' ' || char == '\t') {
				return 'whitespace';
			}
			if (char == '\n') {
				return 'newline';
			}
			if (/[A-Za-z]/.test(char) || (char == '_' && identifierTest.test(source.peek()))) {
				source.nextWhileMatches(identifierTest);
				if (!source.endOfLine() && source.peek() == ':') {
					source.next();
					return 'st-keyword';
				}
				return 'st-identifier';
			}
			if (char == '$') {
				if (source.endOfLine()) return 'st-error';
				source.next();	
				return 'st-character';
			}
			if (char == '^') {
				return 'st-return';
			}
			else if (char == "'") {
				setState(inString("'", 'st-string'));
				return null;
			}
			else if (char == '"') {
				setState(inString('"', 'st-comment'));
				return null;
			}
			else if (char == '#') {
				if (source.endOfLine()) return 'st-error';
				char = source.next();
				if (char == "'") {
					setState(inString("'", 'st-symbol'));
					return null;
				} else {
					if (/[A-Za-z]/.test(char) || (char == '_' && identifierTest.test(source.peek()))) {
						source.nextWhileMatches(/[A-Za-z0-9_:]/);
						return 'st-symbol';
					}
				}
				return 'st-error';
			}
			return 'st-other';
		}

		function inString(endChar, styleClass) {
			return function(source, setState) {
				while (true) {
					if (source.endOfLine())
						return styleClass;
					if (source.next() == endChar) {
						if (!source.endOfLine() && source.peek() == endChar)
							source.next();
						else
							break;
					}
				}
				setState(normal);
				return styleClass;
			}
		}	
		return function(source, startState) {
		  return tokenizer(source, startState || normal);
		};
	})();
	function parseSmalltalk(source, basecolumn) {
		var tokens = tokenizeSmalltalk(source);	
		var iter = {
			next: function() {
				var token = tokens.next()
				,	style = token.style
				,	content = token.content
				;
				if (style == 'newline') {
					token['indentation'] = function(text, current, direction) {  };
					return token;
				}
				if (style == 'st-identifier') {
					if (content == 'true' || content == 'false') {
						token.style = 'st-boolean';
						return token;
					}
					if (content == 'self') {
						token.style = 'st-self';
						return token;
					}
					if (content == 'super') {
						token.style = 'st-super';
						return token;
					}
					if (content == 'nil') {
						token.style = 'st-nil';
						return token;
					}
					if (content.substring(0,1) == content.substring(0,1).toUpperCase()) {
						token.style = 'st-global';
						return token;
					}
				}
				return token;
			},
			copy: function() {
				var _tokenState = tokens.state;
				return function(source) {
					tokens = tokenizeSmalltalk(source, _tokenState);
					return iter;
				};
			}
		};
		return iter;
	}
	return { make: parseSmalltalk };
})();

if (Editor) {
	Editor.Parser = SmalltalkParser;
}
