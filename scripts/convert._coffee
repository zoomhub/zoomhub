#!/usr/bin/env _coffee
#
# Converts content that we have in our database but haven't converted yet.
#
# See these issues:
#
# https://github.com/zoomhub/zoomhub/issues/89
# https://github.com/zoomhub/zoomhub/issues/91
#
# Usage:
#
# </path/to/this/script> <id>
#

assert = require 'assert'
Content = require '../lib/content'
Worker = require '../lib/worker'

echo = console.log
erro = console.error
{exit} = process

[id] = process.argv[2..]
if not id
    erro 'Usage: </path/to/this/script> <id>'
    exit 1

if not content = Content.getById id, _
    erro 'No content with ID:', id
    exit 2

label = "Content with ID #{id} converted! Time taken"
console.time label

Worker.process content, _

console.timeEnd label
