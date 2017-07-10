all:
	mkdir -p _client
	mkdir -p _server
	mkdir -p static/js
	eliomc -c -I ../../_build/src/ web.eliom
	js_of_eliom -o static/js/web.js
	ocsigenserver -c web.conf

