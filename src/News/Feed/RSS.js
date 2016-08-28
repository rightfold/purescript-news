'use strict';

var FeedParser = require('feedparser');

exports.parse = function(Left) {
  return function(Right) {
    return function(url) {
      return function(text) {
        try {
          var result = [];

          var parser = new FeedParser({feedurl: url});
          parser.on('readable', function() {
            var item = this.read();
            result.push({
              title: '' + item.title,
              url: '' + item.link,
              time: item.pubDate || new Date(),
            });
          });
          parser.write(text);
          parser.end();

          return Right(result);
        } catch (e) {
          return Left(e);
        }
      };
    };
  };
};
