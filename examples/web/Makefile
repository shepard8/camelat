run:
	mkdir -p _client
	mkdir -p _server
	mkdir -p static/js
	cp ../../js/camelat.js static/js/camelat.js
	eliomc -c -I ../../_build/default/src/ -I ../../_build/default/eliom/ web.eliom
	js_of_eliom -o static/js/web.js
	ocsigenserver -c web.conf

clean:
	rm -rf _clean _server static/js/web.js

