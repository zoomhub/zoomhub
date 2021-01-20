publish:
	rm -f function-processContent.zip

	mv node_modules node_modules-macos
	mv node_modules-linux node_modules

	zip -r function-processContent.zip index.js lib node_modules

	mv node_modules node_modules-linux
	mv node_modules-macos node_modules

	aws lambda update-function-code \
		--profile zoomhub \
		--zip-file fileb://function-processContent.zip \
		--function-name processContent

invoke:
	S3_CACHE_BUCKET='cache-development.zoomhub.net' \
		node invoke.js

install-linux:
	rm -rf node_modules-linux
	mv node_modules node_modules-macos

	npm install
	rm -rf node_modules/sharp
	SHARP_IGNORE_GLOBAL_LIBVIPS=1 npm install --arch=x64 --platform=linux sharp

	mv node_modules node_modules-linux
	mv node_modules-macos node_modules
