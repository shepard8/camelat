all:
	mkdir -p _client
	mkdir -p _server
	mkdir -p static/js
	eliomc -c -I ../../_build/src/ web.eliom
	cp ../../js/camelat.js static/js/camelat.js
	js_of_eliom -o static/js/web.js
	ocsigenserver -c web.conf

clean:
	rm -rf _clean _server static/js/web.js

