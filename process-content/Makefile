build:
	rm -f function.zip
	zip -r function.zip index.js
	aws lambda update-function-code --profile zoomhub --zip-file fileb://function.zip --function-name process-content
