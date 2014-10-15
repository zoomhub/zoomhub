app = require('supertest') require('../app')
expect = require('chai').expect


@['UI'] =

    'Homepage':

        'should return Zoom.it copy': (_) ->
            {text} = app.get('/')
                .expect(200)
                .end _

            expect(text).to.match /Zoom\.it/i

