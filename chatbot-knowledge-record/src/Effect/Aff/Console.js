'use strict';

exports.createInterface = function() {
  var readline = require('readline');
  return readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
};

exports.closeInterface = function(rl) {
  return function() {
    rl.close();
  };
};

exports._question = function(rl) {
  return function(text) {
    return function(cb) {
      return function() {
        rl.question(text, function(result) {
          cb(result)();
        });
      };
    };
  };
};
