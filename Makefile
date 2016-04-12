# GZIP=zopfli -c -i1000
GZIP=gzip -k
FONTS=app/semantic/dist/themes/default/assets/fonts

CSSDIR=static/css
JSDIR=static/js

JS=$(JSDIR)/jquery.min.js   \
	 $(JSDIR)/semantic.min.js \
	 $(JSDIR)/search.min.js \
	 $(JSDIR)/dropdown.min.js

CSS=$(CSSDIR)/semantic.min.css \
		$(CSSDIR)/search.min.css \
		$(CSSDIR)/dropdown.min.css

compile: app/static/app.css app/static/app.js

app/static/app.css:
	cat $(CSS) > app/static/app.css
#	$(GZIP) app/static/app.css

app/static/app.js:
	cat $(JS) > app/static/app.js
#	$(GZIP) app/static/app.js

compress:
#	closure-compiler --js app/static/app.js --js_output_file app/static/app.min.js -W QUIET
	mkdir out
	for file in static/icon/*; do $(GZIP) "$$file" > "out/$$file"; done
	for file in $(FONTS)/*; do $(GZIP) "$$file" > "out/$$file"; done

clean:
	rm -f app/static/app.css* app/static/app.js*

