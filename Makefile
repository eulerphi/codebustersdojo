default: app.js

app.js:
	elm make src/Main.elm --output build/app.js --debug

clean:
	rm -f build/*.js
	rm -rf elm-stuff

live:
	elm-live src/Main.elm --open -- --output build/app.js --debug

live0:
	elm-live src/Main.elm --open -h 0.0.0.0 -- --output build/app.js --debug

rel:
	elm make src/Main.elm --output build/app.js --optimize

publish: rel
	cp index.html ~/Documents/GitHub/codebustersdojo.github.io/
	cp build/*.js ~/Documents/GitHub/codebustersdojo.github.io/build
	cp css/*.* ~/Documents/GitHub/codebustersdojo.github.io/css