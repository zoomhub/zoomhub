## Development

This app is built on [Node.js](http://nodejs.org/), currently v0.10.

```bash
git clone https://github.com/zoomhub/zoomhub.git
cd zoomhub
npm install
```

To start the server, run:

```bash
npm start
```

To run the tests, run:

```bash
npm test
```

The tests are written for [Mocha](http://visionmedia.github.io/mocha/)'s
[`exports` interface](http://visionmedia.github.io/mocha/#exports-interface).
They use [Chai](http://chaijs.com/) for assertions and
[Supertest](https://github.com/visionmedia/supertest) for API requests.

Portions of this code use [CoffeeScript](http://coffeescript.org/) and/or
[Streamline](https://github.com/Sage/streamlinejs) for now, but we're open to
changing that as this project matures and expands.

### Debugging

To debug via [node-inspector](https://github.com/node-inspector/node-inspector),
run:

```
npm run debugger
```

The CoffeeScript and Streamline code will show up as compiled JS currently;
while source maps get generated internally, they don't get outputted to files
currently (which the debugger needs) when you run the code directly.
Bugs are open to fix that!

To support no-hassle debugging, the server runs in Node's `--debug` mode
always right now. We'll figure out a way to turn that on/off down the road.

To set a breakpoint from code, you can just write the `debugger` keyword.


## Administation

### Prerequisites

-   Install [Ansible](http://docs.ansible.com/).
-   Install [pip](https://pypi.python.org/pypi/pip).
-   Copy `deployment/hosts.sample` to `deployment/hosts` and replace
    `localhost` with your server IP.
-   Copy `credentials/userpassword.sample` to
    `deployment/credentials/userpassword` and paste in your password.

### Commands

`zh` is a script for managing ZoomHub. Currently, we support the following
commands:

-   `./zh bootstrap`: Bootstrap server(s); only required once per user.
-   `./zh ping`: Ping production machines.
-   `./zh setup`: Setup server(s).
-   `./zh deploy`: Deploy latest app code from Git.
