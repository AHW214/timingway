HTML = dist/index.html
ELM = src/Main.elm
JS = src/elm_app.js

$(HTML): $(ELM)
	elm make $(ELM) --output=$(HTML)

js:
	elm make $(ELM) --output $(JS)

clean:
	rm -rf $(HTML)
