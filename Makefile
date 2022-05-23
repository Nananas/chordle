
all:
	elm make src/Main.elm --output=main.js
	

live:
	elm-live src/Main.elm --hot -- --output=main.js --debug

optimize:
	elm make src/Main.elm --optimize --output=main.js

minify:
	uglifyjs main.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output main.js

archive:
	tar -czf chordle.tar.gz main.js \
	index.html \
	favicon.ico
