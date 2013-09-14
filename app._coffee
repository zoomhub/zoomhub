express = require 'express'
app = require('streamline-express') express()
config = require './config'

app.use express.logger 'dev'

app.get '/', (req, res, _) ->
    setTimeout _, 500   # mimic async operation
    res.json 'Hello world.'

if module is require.main
    app.listen config.PORT
    console.log """
        Zooming service listening at http://localhost:#{config.PORT}/ ...
    """
else
    module.exports = app
