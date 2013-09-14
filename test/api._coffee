app = require('supertest') require('../app')
expect = require('chai').expect


@['API'] =

    'GET /':

        'should return a hello world string': (_) ->
            {body} = app.get('/')
                .expect(200)
                .end _

            expect(body).to.match /hello world/i

