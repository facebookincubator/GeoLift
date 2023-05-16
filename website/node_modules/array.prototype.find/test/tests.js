'use strict';

var canDistinguishSparseFromUndefined = 0 in [undefined]; // IE 6 - 8 have a bug where this returns false.

var thrower = function () {
	throw new Error('should not reach here');
};

module.exports = function (find, t) {
	var list = [5, 10, 15, 20];

	t.equal(
		find(list, function (item) { return item === 15; }),
		15,
		'find item by predicate'
	);
	t.equal(
		find(list, function (item) { return item === 'a'; }),
		undefined,
		'returns undefined when nothing matches'
	);
	t['throws'](
		function () { find(list); },
		TypeError,
		'throws without callback'
	);

	var context = {};
	var foundIndex = find(list, function (value, index, arr) {
		t.equal(list[index], value);
		t.deepEqual(list, arr);
		t.equal(this, context, 'receiver is as expected');
		return false;
	}, context);
	t.equal(foundIndex, undefined, 'receives all three arguments');

	var arraylike = { 0: 1, 1: 2, 2: 3, length: 3 };
	var found = find(arraylike, function (item) {
		return item === 2;
	});
	t.equal(found, 2, 'works with an array-like object');

	t.equal(
		find({ 0: 1, 1: 2, 2: 3, length: -3 }, thrower),
		undefined,
		'works with an array-like object with negative length'
	);

	t.test('sparse arrays', { skip: !canDistinguishSparseFromUndefined }, function (st) {
		st.test('works with a sparse array', function (s2t) {
			var obj = [1, , undefined]; // eslint-disable-line no-sparse-arrays
			s2t.notOk(1 in obj);
			var seen = [];
			var foundSparse = find(obj, function (item, idx) {
				seen.push([idx, item]);
				return false;
			});
			s2t.equal(foundSparse, undefined);
			s2t.deepEqual(seen, [[0, 1], [1, undefined], [2, undefined]]);

			s2t.end();
		});

		st.test('works with a sparse array-like object', function (s2t) {
			var obj = { 0: 1, 2: undefined, length: 3.2 };
			var seen = [];
			var foundSparse = find(obj, function (item, idx) {
				seen.push([idx, item]);
				return false;
			});
			s2t.equal(foundSparse, undefined);
			s2t.deepEqual(seen, [[0, 1], [1, undefined], [2, undefined]]);

			s2t.end();
		});

		st.end();
	});
};
