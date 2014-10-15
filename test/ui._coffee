app = require('supertest') require('../app')
expect = require('chai').expect


describe 'UI', ->

    describe 'Homepage', ->

        it 'should return Zoom.it copy', (_) ->
            {text} = app.get('/')
                .expect(200)
                .end _

            expect(text).to.match /Zoom\.it/i

