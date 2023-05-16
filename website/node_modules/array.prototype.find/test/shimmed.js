'use strict';

require('../auto');

var test = require('tape');
var defineProperties = require('define-properties');
var callBind = require('call-bind');

var isEnumerable = Object.prototype.propertyIsEnumerable;
var functionsHaveNames = require('functions-have-names')();
var hasStrictMode = require('has-strict-mode')();

var runTests = require('./tests');

test('shimmed', function (t) {
	t.equal(Array.prototype.find.length, 1, 'Array#find has a length of 1');
	t.test('Function name', { skip: !functionsHaveNames }, function (st) {
		st.equal(Array.prototype.find.name, 'find', 'Array#find has name "find"');
		st.end();
	});

	t.test('enumerability', { skip: !defineProperties.supportsDescriptors }, function (et) {
		et.equal(false, isEnumerable.call(Array.prototype, 'find'), 'Array#find is not enumerable');
		et.end();
	});

	t.test('bad array/this value', { skip: !hasStrictMode }, function (st) {
		st['throws'](function () { return Array.prototype.find.call(undefined); }, TypeError, 'undefined is not an object');
		st['throws'](function () { return Array.prototype.find.call(null); }, TypeError, 'null is not an object');
		st.end();
	});

	runTests(callBind(Array.prototype.find), t);

	t.end();
});
