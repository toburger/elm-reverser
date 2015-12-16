@echo "off"

elm.exe make .\Main.elm --output .\dist\main.js
uglifyjs.cmd .\dist\main.js --mangle --compress --lint false --verbose false --output .\dist\main.min.js
