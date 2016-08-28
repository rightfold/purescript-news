'use strict';

var https = require('https');
var url = require('url');

exports.request = function(url_) {
  return function(success, error) {
    try {
      var options = url.parse(url_);
      options.headers = {'User-Agent': 'purescript-news'};
      var req = https.request(options, function(res) {
        var data = '';
        res.on('data', function(chunk) {
          data += chunk;
        });
        res.on('end', function() {
          success(data);
        });
      });
      req.on('error', function(e) {
        error(e);
      });
      req.end();
    } catch (e) {
      error(e);
    }
  };
};
