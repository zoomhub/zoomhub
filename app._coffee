express = require 'express'
app = require('streamline-express') express()

app.use express.logger 'dev'

app.get '/', (req, res, _) ->
    setTimeout _, 500   # mimic async operation
    res.json 'Hello world.'

if module is require.main
    app.listen 5000
    console.log 'Zooming service listening at http://localhost:5000/ ...'
else
    module.exports = app
