publish:
	rm -f function.zip

	mv node_modules node_modules-macos
	mv node_modules-linux node_modules

	zip -r function.zip index.js node_modules

	mv node_modules node_modules-linux
	mv node_modules-macos node_modules

	aws lambda update-function-code --profile zoomhub --zip-file fileb://function.zip --function-name process-content

install-linux:
	rm -rf node_modules-linux
	mv node_modules node_modules-macos

	npm install
	rm -rf node_modules/sharp
	docker run -v "$PWD":/var/task lambci/lambda:build-nodejs12.x npm install sharp

	mv node_modules node_modules-linux
	mv node_modules-macos node_modules
