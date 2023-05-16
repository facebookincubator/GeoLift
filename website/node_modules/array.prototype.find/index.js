'use strict';

var define = require('define-properties');
var callBound = require('call-bind/callBound');
var RequireObjectCoercible = require('es-abstract/2021/RequireObjectCoercible');

var implementation = require('./implementation');
var getPolyfill = require('./polyfill');
var shim = require('./shim');

var $slice = callBound('Array.prototype.slice');

var polyfill = getPolyfill();

var boundFindShim = function find(array, predicate) { // eslint-disable-line no-unused-vars
	RequireObjectCoercible(array);
	var args = $slice(arguments, 1);
	return polyfill.apply(array, args);
};

define(boundFindShim, {
	getPolyfill: getPolyfill,
	implementation: implementation,
	shim: shim
});

module.exports = boundFindShim;
