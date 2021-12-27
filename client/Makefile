HTML = dist/index.html
ELM = src/Main.elm

$(HTML): $(ELM)
	elm make $(ELM) --output=$(HTML)

clean:
	rm -rf $(HTML)
