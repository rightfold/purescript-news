'use strict';

exports.escapeHTML = function(s) {
  return s.replace(/&/g, '&amp;')
          .replace(/</g, '&lt;')
          .replace(/"/g, '&quot;')
          .replace(/'/g, '&#39;');
};
