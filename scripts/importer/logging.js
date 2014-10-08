var winston = require('winston');

var logger;

exports.getLogger = function (level) {

  if (logger) {
    return logger;
  }

  logger = new winston.Logger({
    levels: {
      trace: 0,
      debug: 1,
      verbose: 2,
      info: 3,
      warn: 4,
      error: 5
    },
    colors: {
      trace: 'white',
      debug: 'grey',
      verbose: 'cyan',
      info: 'green',
      warn: 'yellow',
      error: 'red'
    },
    transports: [
      new winston.transports.Console({
        level: level,
        prettyPrint: true,
        colorize: true,
        timestamp: true
      })
    ]
  });

  return logger;
}

exports.logFunction = function (message, object) {
  if (object) {
    logger.log(this.event.split('::')[1], message, object);
  }
  else {
    logger.log(this.event.split('::')[1], message);
  }
};