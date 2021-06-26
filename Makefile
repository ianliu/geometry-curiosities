pages/eulerline.js: src/EulerLine.elm src/Geom.elm
	elm make --optimize --output=$@ $<


clean:
	rm -f pages/eulerline.js

.PHONY: clean
