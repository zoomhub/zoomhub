var program = require('commander');

exports.loadProgram = function(args, options) {

  options = options || [];

  program
    .version('0.0.1')
    .option('-u, --username [username]', 'Username')
    .option('-p, --password [password]', 'Password')
    .option('-r, --region [region]', 'Region')
    .option('--useInternal', 'Use Local Service Interface');

  options.forEach(function(option) {
    program.option(option);
  });

  program.parse(args);

  if (!program.username && !program.password && !program.region) {
    program.help();
  }

  return {
    program: program,
    cloudOptions: {
      username: program.username,
      password: program.password,
      useInternal: program.useInternal,
      region: program.region
    }
  };
};
