app = (require 'supertest')(require '../app')
{expect} = require 'chai'


describe 'UI', ->

    describe 'Homepage', ->

        it 'should return Zoom.it copy', (_) ->
            app.get '/'
                .expect 200
                .expect /Zoom\.it/i
                .end _
