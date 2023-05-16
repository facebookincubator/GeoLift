export const version: string;
/**
 * Tests if is is running under a browser.
 * @return {boolean} true if the environment has process, process.version and process.versions.
 */
export function browser(): boolean;
/**
 * Test if 'value' is defined.
 * Alias: def
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is defined, false otherwise.
 */
export function defined(value: unknown): boolean;
/**
 * Test if 'value' is defined.
 * Alias: def
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is defined, false otherwise.
 */
export function def(value: unknown): boolean;
/**
 * Tests if is is running under node.js
 * @return {boolean} true if the environment has process, process.version and process.versions.
 */
export function nodejs(): boolean;
/**
 * Tests if is is running under node.js
 * @return {boolean} true if the environment has process, process.version and process.versions.
 */
export function node(): boolean;
/**
 * Test if 'value' is undefined.
 * Aliases: undef, udef
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is undefined, false otherwise.
 */
export function undefined(value: unknown): boolean;
/**
 * Test if 'value' is undefined.
 * Aliases: undef, udef
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is undefined, false otherwise.
 */
export function udef(value: unknown): boolean;
/**
 * Test if 'value' is undefined.
 * Aliases: undef, udef
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is undefined, false otherwise.
 */
export function undef(value: unknown): boolean;
/**
 * Test if 'value' is an array.
 * Alias: ary, arry
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an array, false otherwise.
 */
export function array(value: unknown): boolean;
/**
 * Test if 'value' is an array.
 * Alias: ary, arry
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an array, false otherwise.
 */
export function arr(value: unknown): boolean;
/**
 * Test if 'value' is an array.
 * Alias: ary, arry
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an array, false otherwise.
 */
export function ary(value: unknown): boolean;
/**
 * Test if 'value' is an array.
 * Alias: ary, arry
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an array, false otherwise.
 */
export function arry(value: unknown): boolean;
/**
 * Test if 'value' is an arraylike object (i.e. it has a length property with a valid value)
 * Aliases: arraylike, arryLike, aryLike
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an arguments object, false otherwise.
 */
export function arrayLike(value: unknown): boolean;
/**
 * Test if 'value' is an arraylike object (i.e. it has a length property with a valid value)
 * Aliases: arraylike, arryLike, aryLike
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an arguments object, false otherwise.
 */
export function arrLike(value: unknown): boolean;
/**
 * Test if 'value' is an arraylike object (i.e. it has a length property with a valid value)
 * Aliases: arraylike, arryLike, aryLike
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an arguments object, false otherwise.
 */
export function arryLike(value: unknown): boolean;
/**
 * Test if 'value' is an arraylike object (i.e. it has a length property with a valid value)
 * Aliases: arraylike, arryLike, aryLike
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an arguments object, false otherwise.
 */
export function aryLike(value: unknown): boolean;
/**
 * Test if 'value' is an arraylike object (i.e. it has a length property with a valid value)
 * Aliases: arraylike, arryLike, aryLike
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an arguments object, false otherwise.
 */
export function arraylike(value: unknown): boolean;
/**
 * Test if 'value' is an arguments object.
 * Alias: args
 * @param {unknown} value value to test
 * @return {boolean} true if 'value' is an arguments object, false otherwise
 */
declare function _arguments(value: unknown): boolean;
/**
 * Test if 'value' is an arguments object.
 * Alias: args
 * @param {unknown} value value to test
 * @return {boolean} true if 'value' is an arguments object, false otherwise
 */
export function args(value: unknown): boolean;
/**
 * Test if 'value' is a boolean.
 * Alias: bool
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a boolean, false otherwise.
 */
export function boolean(value: unknown): boolean;
/**
 * Test if 'value' is a boolean.
 * Alias: bool
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a boolean, false otherwise.
 */
export function bool(value: unknown): boolean;
/**
 * Test if 'value' is an instance of Buffer.
 * Aliases: instOf, instanceof
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an instance of 'constructor'.
 */
export function buffer(value: unknown): boolean;
/**
 * Test if 'value' is an instance of Buffer.
 * Aliases: instOf, instanceof
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an instance of 'constructor'.
 */
export function buff(value: unknown): boolean;
/**
 * Test if 'value' is an instance of Buffer.
 * Aliases: instOf, instanceof
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an instance of 'constructor'.
 */
export function buf(value: unknown): boolean;
/**
 * Test if 'value' is a date.
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a date, false otherwise.
 */
export function date(value: unknown): boolean;
/**
 * Test if 'value' is an error object.
 * Alias: err
 * @param value value to test.
 * @return {boolean} true if 'value' is an error object, false otherwise.
 */
export function error(value: unknown): boolean;
/**
 * Test if 'value' is an error object.
 * Alias: err
 * @param value value to test.
 * @return {boolean} true if 'value' is an error object, false otherwise.
 */
export function err(value: unknown): boolean;
/**
 * Test if 'value' is false.
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is false, false otherwise
 */
declare function _false(value: unknown): boolean;
/**
 * Test if 'value' is a function or async function.
 * Alias: func
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
declare function _function(value: unknown): boolean;
/**
 * Test if 'value' is a function or async function.
 * Alias: func
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
export function fun(value: unknown): boolean;
/**
 * Test if 'value' is a function or async function.
 * Alias: func
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
export function func(value: unknown): boolean;
/**
 * Test if 'value' is an async function using `async () => {}` or `async function () {}`.
 * Alias: func
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
export function asyncFunction(value: unknown): boolean;
/**
 * Test if 'value' is an async function using `async () => {}` or `async function () {}`.
 * Alias: func
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
export function asyncFun(value: unknown): boolean;
/**
 * Test if 'value' is an async function using `async () => {}` or `async function () {}`.
 * Alias: func
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
export function asyncFunc(value: unknown): boolean;
/**
 * Test if 'value' is a synchronous function.
 * Alias: syncFunc
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
export function syncFunction(value: unknown): boolean;
/**
 * Test if 'value' is a synchronous function.
 * Alias: syncFunc
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
export function syncFun(value: unknown): boolean;
/**
 * Test if 'value' is a synchronous function.
 * Alias: syncFunc
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a function, false otherwise.
 */
export function syncFunc(value: unknown): boolean;
/**
 * Test if 'value' is null.
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is null, false otherwise.
 */
declare function _null(value: unknown): boolean;
/**
 * Test is 'value' is either null or undefined.
 * Alias: nullOrUndef
 * @param {unknown} value value to test.
 * @return {boolean} True if value is null or undefined, false otherwise.
 */
export function nullOrUndefined(value: unknown): boolean;
/**
 * Test is 'value' is either null or undefined.
 * Alias: nullOrUndef
 * @param {unknown} value value to test.
 * @return {boolean} True if value is null or undefined, false otherwise.
 */
export function nullOrUndef(value: unknown): boolean;
/**
 * Test if 'value' is a number.
 * Alias: num
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function number(value: unknown): boolean;
/**
 * Test if 'value' is a number.
 * Alias: num
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function num(value: unknown): boolean;
/**
 * Test if 'value' is an object. Note: Arrays, RegExps, Date, Error, etc all return false.
 * Alias: obj
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is an object, false otherwise.
 */
export function object(value: unknown): boolean;
/**
 * Test if 'value' is an object. Note: Arrays, RegExps, Date, Error, etc all return false.
 * Alias: obj
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is an object, false otherwise.
 */
export function obj(value: unknown): boolean;
/**
 * Test if 'value' is a regular expression.
 * Alias: regexp
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a regexp, false otherwise.
 */
export function regExp(value: unknown): boolean;
/**
 * Test if 'value' is a regular expression.
 * Alias: regexp
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a regexp, false otherwise.
 */
export function re(value: unknown): boolean;
/**
 * Test if 'value' is a regular expression.
 * Alias: regexp
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a regexp, false otherwise.
 */
export function regexp(value: unknown): boolean;
/**
 * Test if 'value' is a string.
 * Alias: str
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a string, false otherwise.
 */
export function string(value: unknown): boolean;
/**
 * Test if 'value' is a string.
 * Alias: str
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a string, false otherwise.
 */
export function str(value: unknown): boolean;
/**
 * Test if 'value' is true.
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is true, false otherwise.
 */
declare function _true(value: unknown): boolean;
export function uuid(value: unknown): boolean;
/**
 * Test if 'value' is equal to 'other'. Works for objects and arrays and will do deep comparisions,
 * using recursion.
 * Alias: eq
 * @param {unknown} value value.
 * @param {unknown} other value to compare with.
 * @return {boolean} true if 'value' is equal to 'other', false otherwise
 */
export function equal(value: unknown, other: unknown): boolean;
/**
 * Test if 'value' is equal to 'other'. Works for objects and arrays and will do deep comparisions,
 * using recursion.
 * Alias: eq
 * @param {unknown} value value.
 * @param {unknown} other value to compare with.
 * @return {boolean} true if 'value' is equal to 'other', false otherwise
 */
export function objEquals(value: unknown, other: unknown): boolean;
/**
 * Test if 'value' is equal to 'other'. Works for objects and arrays and will do deep comparisions,
 * using recursion.
 * Alias: eq
 * @param {unknown} value value.
 * @param {unknown} other value to compare with.
 * @return {boolean} true if 'value' is equal to 'other', false otherwise
 */
export function eq(value: unknown, other: unknown): boolean;
/**
 * Test if 'key' in host is an object. To be hosted means host[value] is an object.
 * @param {unknown} value The value to test.
 * @param {unknown} host Host that may contain value.
 * @return {boolean} true if 'value' is hosted by 'host', false otherwise.
 */
export function hosted(value: unknown, host: unknown): boolean;
/**
 * Test if 'value' is an instance of 'constructor'.
 * Aliases: instOf, instanceof
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an instance of 'constructor'.
 */
export function instanceOf(value: unknown, constructor: unknown): boolean;
export const instOf:
  | ((value: unknown, constructor: unknown) => boolean)
  | ((objInst: object, objType: object) => boolean);
/**
 * Test if 'value' is an instance of 'constructor'.
 * Aliases: instOf, instanceof
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an instance of 'constructor'.
 */
declare function _instanceof(value: unknown, constructor: unknown): boolean;
/**
 * Test if 'value' is an instance type objType.
 * Aliases: objInstOf, objectinstanceof, instOf, instanceOf
 * @param {object} objInst an object to testfor type.
 * @param {object} objType an object type to compare.
 * @return {boolean} true if 'value' is an object, false otherwise.
 */
export function objectInstanceOf(objInst: object, objType: object): boolean;
/**
 * Test if 'value' is an instance type objType.
 * Aliases: objInstOf, objectinstanceof, instOf, instanceOf
 * @param {object} objInst an object to testfor type.
 * @param {object} objType an object type to compare.
 * @return {boolean} true if 'value' is an object, false otherwise.
 */
export function objInstOf(objInst: object, objType: object): boolean;
/**
 * Test if 'value' is a type of 'type'.
 * Alias: a
 * @param value value to test.
 * @param {string} type The name of the type.
 * @return {boolean} true if 'value' is an arguments object, false otherwise.
 */
export function type(value: unknown, type: string): boolean;
/**
 * Test if 'value' is a type of 'type'.
 * Alias: a
 * @param value value to test.
 * @param {string} type The name of the type.
 * @return {boolean} true if 'value' is an arguments object, false otherwise.
 */
export function a(value: unknown, type: string): boolean;
/**
 * Test if 'value' is empty. To be empty means to be an array, object or string with nothing contained.
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is empty, false otherwise.
 */
export function empty(value: unknown): boolean;
/**
 * Test if 'value' is an arguments object that is empty.
 * Alias: args
 * @param {unknown} value value to test
 * @return {boolean} true if 'value' is an arguments object with no args, false otherwise
 */
export function emptyArguments(value: unknown): boolean;
/**
 * Test if 'value' is an arguments object that is empty.
 * Alias: args
 * @param {unknown} value value to test
 * @return {boolean} true if 'value' is an arguments object with no args, false otherwise
 */
export function noArgs(value: unknown): boolean;
/**
 * Test if 'value' is an arguments object that is empty.
 * Alias: args
 * @param {unknown} value value to test
 * @return {boolean} true if 'value' is an arguments object with no args, false otherwise
 */
export function emptyArgs(value: unknown): boolean;
/**
 * Test if 'value' is an array containing no entries.
 * Aliases: emptyArry, emptyAry
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is an array with no elemnets.
 */
export function emptyArray(value: unknown): boolean;
/**
 * Test if 'value' is an array containing no entries.
 * Aliases: emptyArry, emptyAry
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is an array with no elemnets.
 */
export function emptyArry(value: unknown): boolean;
/**
 * Test if 'value' is an array containing no entries.
 * Aliases: emptyArry, emptyAry
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is an array with no elemnets.
 */
export function emptyAry(value: unknown): boolean;
/**
 * Test if 'value' is an empty array(like) object.
 * Aliases: arguents.empty, args.empty, ary.empty, arry.empty
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an empty array(like), false otherwise.
 */
export function emptyArrayLike(value: unknown): boolean;
/**
 * Test if 'value' is an empty array(like) object.
 * Aliases: arguents.empty, args.empty, ary.empty, arry.empty
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is an empty array(like), false otherwise.
 */
export function emptyArrLike(value: unknown): boolean;
/**
 * Test if 'value' is an empty string.
 * Alias: emptyStr
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is am empty string, false otherwise.
 */
export function emptyString(value: unknown): boolean;
/**
 * Test if 'value' is an empty string.
 * Alias: emptyStr
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is am empty string, false otherwise.
 */
export function emptyStr(value: unknown): boolean;
/**
 * Test if 'value' is an array containing at least 1 entry.
 * Aliases: nonEmptyArry, nonEmptyAry
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is an array with at least 1 value, false otherwise.
 */
export function nonEmptyArray(value: unknown): boolean;
/**
 * Test if 'value' is an array containing at least 1 entry.
 * Aliases: nonEmptyArry, nonEmptyAry
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is an array with at least 1 value, false otherwise.
 */
export function nonEmptyArr(value: unknown): boolean;
/**
 * Test if 'value' is an array containing at least 1 entry.
 * Aliases: nonEmptyArry, nonEmptyAry
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is an array with at least 1 value, false otherwise.
 */
export function nonEmptyArry(value: unknown): boolean;
/**
 * Test if 'value' is an array containing at least 1 entry.
 * Aliases: nonEmptyArry, nonEmptyAry
 * @param {unknown} value The value to test.
 * @return {boolean} true if 'value' is an array with at least 1 value, false otherwise.
 */
export function nonEmptyAry(value: unknown): boolean;
/**
 * Test if 'value' is an object with properties. Note: Arrays are objects.
 * Alias: nonEmptyObj
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is an object, false otherwise.
 */
export function nonEmptyObject(value: unknown): boolean;
/**
 * Test if 'value' is an object with properties. Note: Arrays are objects.
 * Alias: nonEmptyObj
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is an object, false otherwise.
 */
export function nonEmptyObj(value: unknown): boolean;
/**
 * Test if 'value' is an object with no properties. Note: Arrays are objects.
 * Alias: nonEmptyObj
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is an object, false otherwise.
 */
export function emptyObject(value: unknown): boolean;
/**
 * Test if 'value' is an object with no properties. Note: Arrays are objects.
 * Alias: nonEmptyObj
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is an object, false otherwise.
 */
export function emptyObj(value: unknown): boolean;
/**
 * Test if 'value' is a non-empty string.
 * Alias: nonEmptyStr
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a non-empty string, false otherwise.
 */
export function nonEmptyString(value: unknown): boolean;
/**
 * Test if 'value' is a non-empty string.
 * Alias: nonEmptyStr
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a non-empty string, false otherwise.
 */
export function nonEmptyStr(value: unknown): boolean;
/**
 * Test if 'value' is an even number.
 * @param {Number} value to test.
 * @return {boolean} true if 'value' is an even number, false otherwise.
 */
export function even(value: number): boolean;
/**
 * Test if 'value' is a decimal number.
 * Aliases: decimalNumber, decNum
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a decimal number, false otherwise.
 */
export function decimal(value: unknown): boolean;
/**
 * Test if 'value' is a decimal number.
 * Aliases: decimalNumber, decNum
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a decimal number, false otherwise.
 */
export function dec(value: unknown): boolean;
/**
 * Test if 'value' is a decimal number.
 * Aliases: decimalNumber, decNum
 * @param {unknown} value value to test.
 * @return {boolean} true if 'value' is a decimal number, false otherwise.
 */
export function decNum(value: unknown): boolean;
/**
 * Test if 'value' is an integer.
 * Alias: integer
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is an integer, false otherwise.
 */
export function integer(value: unknown): boolean;
/**
 * Test if 'value' is an integer.
 * Alias: integer
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is an integer, false otherwise.
 */
export function int(value: unknown): boolean;
/**
 * is.nan
 * Test if `value` is not a number.
 *
 * @param {unknown} value value to test
 * @return {boolean} true if `value` is not a number, false otherwise
 * @api public
 */
export function notANumber(value: unknown): boolean;
/**
 * is.nan
 * Test if `value` is not a number.
 *
 * @param {unknown} value value to test
 * @return {boolean} true if `value` is not a number, false otherwise
 * @api public
 */
export function nan(value: unknown): boolean;
/**
 * is.nan
 * Test if `value` is not a number.
 *
 * @param {unknown} value value to test
 * @return {boolean} true if `value` is not a number, false otherwise
 * @api public
 */
export function notANum(value: unknown): boolean;
/**
 * Test if 'value' is an odd number.
 * @param {Number} value to test.
 * @return {boolean} true if 'value' is an odd number, false otherwise.
 */
export function odd(value: number): boolean;
/**
 * Test if 'value' is an odd number.
 * @param {Number} value to test.
 * @return {boolean} true if 'value' is an odd number, false otherwise.
 */
export function oddNumber(value: number): boolean;
/**
 * Test if 'value' is an odd number.
 * @param {Number} value to test.
 * @return {boolean} true if 'value' is an odd number, false otherwise.
 */
export function oddNum(value: number): boolean;
/**
 * Test if 'value' is a positive number.
 * Alias: positiveNum, posNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function positiveNumber(value: unknown): boolean;
/**
 * Test if 'value' is a positive number.
 * Alias: positiveNum, posNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function pos(value: unknown): boolean;
/**
 * Test if 'value' is a positive number.
 * Alias: positiveNum, posNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function positive(value: unknown): boolean;
/**
 * Test if 'value' is a positive number.
 * Alias: positiveNum, posNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function posNum(value: unknown): boolean;
/**
 * Test if 'value' is a positive number.
 * Alias: positiveNum, posNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function positiveNum(value: unknown): boolean;
/**
 * Test if 'value' is a negative number.
 * Aliases: negNum, negativeNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function negativeNumber(value: unknown): boolean;
/**
 * Test if 'value' is a negative number.
 * Aliases: negNum, negativeNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function neg(value: unknown): boolean;
/**
 * Test if 'value' is a negative number.
 * Aliases: negNum, negativeNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function negNum(value: unknown): boolean;
/**
 * Test if 'value' is a negative number.
 * Aliases: negNum, negativeNum
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a number, false otherwise.
 */
export function negativeNum(value: unknown): boolean;
/**
 * Test if 'value' is a negative integer.
 * Aliases: negInt, negativeInteger
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a negative integer, false otherwise.
 */
export function negativeInteger(value: unknown): boolean;
/**
 * Test if 'value' is a negative integer.
 * Aliases: negInt, negativeInteger
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a negative integer, false otherwise.
 */
export function negativeInt(value: unknown): boolean;
/**
 * Test if 'value' is a negative integer.
 * Aliases: negInt, negativeInteger
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a negative integer, false otherwise.
 */
export function negInt(value: unknown): boolean;
/**
 * Test if 'value' is a positive integer.
 * Alias: posInt
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a positive integer, false otherwise.
 */
export function positiveInteger(value: unknown): boolean;
/**
 * Test if 'value' is a positive integer.
 * Alias: posInt
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a positive integer, false otherwise.
 */
export function posInt(value: unknown): boolean;
/**
 * Test if 'value' is a positive integer.
 * Alias: posInt
 * @param {unknown} value to test.
 * @return {boolean} true if 'value' is a positive integer, false otherwise.
 */
export function positiveInt(value: unknown): boolean;
/**
 * Test if 'value' is divisible by 'n'.
 * Alias: divisBy
 * @param {Number} value value to test.
 * @param {Number} n dividend.
 * @return {boolean} true if 'value' is divisible by 'n', false otherwise.
 */
export function divisibleBy(value: number, n: number): boolean;
/**
 * Test if 'value' is divisible by 'n'.
 * Alias: divisBy
 * @param {Number} value value to test.
 * @param {Number} n dividend.
 * @return {boolean} true if 'value' is divisible by 'n', false otherwise.
 */
export function divBy(value: number, n: number): boolean;
/**
 * Test if 'value' is divisible by 'n'.
 * Alias: divisBy
 * @param {Number} value value to test.
 * @param {Number} n dividend.
 * @return {boolean} true if 'value' is divisible by 'n', false otherwise.
 */
export function divisBy(value: number, n: number): boolean;
/**
 * Test if 'value' is greater than or equal to 'other'.
 * Aliases: greaterOrEq, greaterOrEqual
 * @param {Number} value value to test.
 * @param {Number} other value to compare with.
 * @return {boolean} true, if value is greater than or equal to other, false otherwise.
 */
export function greaterOrEqualTo(value: number, other: number): boolean;
/**
 * Test if 'value' is greater than or equal to 'other'.
 * Aliases: greaterOrEq, greaterOrEqual
 * @param {Number} value value to test.
 * @param {Number} other value to compare with.
 * @return {boolean} true, if value is greater than or equal to other, false otherwise.
 */
export function greaterOrEqual(value: number, other: number): boolean;
/**
 * Test if 'value' is greater than or equal to 'other'.
 * Aliases: greaterOrEq, greaterOrEqual
 * @param {Number} value value to test.
 * @param {Number} other value to compare with.
 * @return {boolean} true, if value is greater than or equal to other, false otherwise.
 */
export function ge(value: number, other: number): boolean;
/**
 * Test if 'value' is greater than 'other'.
 * Aliases: greaterThan
 * @param {Number} value value to test.
 * @param {Number} other value to compare with.
 * @return {boolean} true, if value is greater than other, false otherwise.
 */
export function greaterThan(value: number, other: number): boolean;
/**
 * Test if 'value' is greater than 'other'.
 * Aliases: greaterThan
 * @param {Number} value value to test.
 * @param {Number} other value to compare with.
 * @return {boolean} true, if value is greater than other, false otherwise.
 */
export function gt(value: number, other: number): boolean;
/**
 * Test if 'value' is less than or equal to 'other'.
 * Alias: lessThanOrEq, lessThanOrEqual
 * @param {Number} value value to test
 * @param {Number} other value to compare with
 * @return {boolean} true, if 'value' is less than or equal to 'other', false otherwise.
 */
export function lessThanOrEqualTo(value: number, other: number): boolean;
/**
 * Test if 'value' is less than or equal to 'other'.
 * Alias: lessThanOrEq, lessThanOrEqual
 * @param {Number} value value to test
 * @param {Number} other value to compare with
 * @return {boolean} true, if 'value' is less than or equal to 'other', false otherwise.
 */
export function lessThanOrEq(value: number, other: number): boolean;
/**
 * Test if 'value' is less than or equal to 'other'.
 * Alias: lessThanOrEq, lessThanOrEqual
 * @param {Number} value value to test
 * @param {Number} other value to compare with
 * @return {boolean} true, if 'value' is less than or equal to 'other', false otherwise.
 */
export function lessThanOrEqual(value: number, other: number): boolean;
/**
 * Test if 'value' is less than or equal to 'other'.
 * Alias: lessThanOrEq, lessThanOrEqual
 * @param {Number} value value to test
 * @param {Number} other value to compare with
 * @return {boolean} true, if 'value' is less than or equal to 'other', false otherwise.
 */
export function le(value: number, other: number): boolean;
/**
 * Test if 'value' is less than 'other'.
 * Alias: lessThan
 * @param {Number} value value to test
 * @param {Number} other value to compare with
 * @return {boolean} true, if 'value' is less than 'other', false otherwise.
 */
export function lessThan(value: number, other: number): boolean;
/**
 * Test if 'value' is less than 'other'.
 * Alias: lessThan
 * @param {Number} value value to test
 * @param {Number} other value to compare with
 * @return {boolean} true, if 'value' is less than 'other', false otherwise.
 */
export function lt(value: number, other: number): boolean;
/**
 * Test if 'value' is greater than 'others' values.
 * Alias: max
 * @param {Number} value value to test.
 * @param {Array} others values to compare with.
 * @return {boolean} true if 'value' is greater than 'others' values.
 */
export function maximum(value: number, others: unknown[]): boolean;
/**
 * Test if 'value' is greater than 'others' values.
 * Alias: max
 * @param {Number} value value to test.
 * @param {Array} others values to compare with.
 * @return {boolean} true if 'value' is greater than 'others' values.
 */
export function max(value: number, others: unknown[]): boolean;
/**
 * Test if 'value' is less than 'others' values.
 * Alias: min
 * @param {Number} value value to test.
 * @param {Array} others values to compare with.
 * @return {boolean} true if 'value' is less than 'others' values.
 */
export function minimum(value: number, others: unknown[]): boolean;
/**
 * Test if 'value' is less than 'others' values.
 * Alias: min
 * @param {Number} value value to test.
 * @param {Array} others values to compare with.
 * @return {boolean} true if 'value' is less than 'others' values.
 */
export function min(value: number, others: unknown[]): boolean;
/**
 * Test if 'value' is within 'start' and 'finish'.
 * Alias: withIn
 * @param {Number} value value to test.
 * @param {Number} start lower bound.
 * @param {Number} finish upper bound.
 * @return {boolean} true if 'value' is is within 'start' and 'finish', false otherwise.
 */
export function within(value: number, start: number, finish: number): boolean;
/**
 * Test if 'value' is within 'start' and 'finish'.
 * Alias: withIn
 * @param {Number} value value to test.
 * @param {Number} start lower bound.
 * @param {Number} finish upper bound.
 * @return {boolean} true if 'value' is is within 'start' and 'finish', false otherwise.
 */
export function withIn(value: number, start: number, finish: number): boolean;
/**
 * Test if 'value' is within 'precision' decimal places from 'comparitor'.
 * Alias: closish, near.
 * @param {Number} value value to test
 * @param {Number} comparitor value to test 'value' against
 * @param {Number} precision number of decimals to compare floating points, defaults to 2
 * @return {boolean} true if 'value' is within 'precision' decimal places from 'comparitor', false otherwise.
 */
export function prettyClose(
  value: number,
  comparitor: number,
  precision: number
): boolean;
/**
 * Test if 'value' is within 'precision' decimal places from 'comparitor'.
 * Alias: closish, near.
 * @param {Number} value value to test
 * @param {Number} comparitor value to test 'value' against
 * @param {Number} precision number of decimals to compare floating points, defaults to 2
 * @return {boolean} true if 'value' is within 'precision' decimal places from 'comparitor', false otherwise.
 */
export function closish(
  value: number,
  comparitor: number,
  precision: number
): boolean;
/**
 * Test if 'value' is within 'precision' decimal places from 'comparitor'.
 * Alias: closish, near.
 * @param {Number} value value to test
 * @param {Number} comparitor value to test 'value' against
 * @param {Number} precision number of decimals to compare floating points, defaults to 2
 * @return {boolean} true if 'value' is within 'precision' decimal places from 'comparitor', false otherwise.
 */
export function near(
  value: number,
  comparitor: number,
  precision: number
): boolean;
/**
 * Test if a value is a valid DNS address. eg www.stdarg.com is true while
 * 127.0.0.1 is false.
 * @param {unknown} value to test if a DNS address.
 * @return {boolean} true if a DNS address, false otherwise.
 * DNS Address is made up of labels separated by '.'
 * Each label must be between 1 and 63 characters long
 * The entire hostname (including the delimiting dots) has a maximum of 255 characters.
 * Hostname may not contain other characters, such as the underscore character (_)
 * other DNS names may contain the underscore.
 */
export function dnsAddress(value: unknown): boolean;
/**
 * Test if a value is a valid DNS address. eg www.stdarg.com is true while
 * 127.0.0.1 is false.
 * @param {unknown} value to test if a DNS address.
 * @return {boolean} true if a DNS address, false otherwise.
 * DNS Address is made up of labels separated by '.'
 * Each label must be between 1 and 63 characters long
 * The entire hostname (including the delimiting dots) has a maximum of 255 characters.
 * Hostname may not contain other characters, such as the underscore character (_)
 * other DNS names may contain the underscore.
 */
export function dnsAddr(value: unknown): boolean;
/**
 * Test if a value is a valid DNS address. eg www.stdarg.com is true while
 * 127.0.0.1 is false.
 * @param {unknown} value to test if a DNS address.
 * @return {boolean} true if a DNS address, false otherwise.
 * DNS Address is made up of labels separated by '.'
 * Each label must be between 1 and 63 characters long
 * The entire hostname (including the delimiting dots) has a maximum of 255 characters.
 * Hostname may not contain other characters, such as the underscore character (_)
 * other DNS names may contain the underscore.
 */
export function dns(value: unknown): boolean;
/**
 * Test if value is a valid email address.
 * @param {unknown} value to test if an email address.
 * @return {boolean} true if an email address, false otherwise.
 */
export function emailAddress(value: unknown): boolean;
/**
 * Test if value is a valid email address.
 * @param {unknown} value to test if an email address.
 * @return {boolean} true if an email address, false otherwise.
 */
export function email(value: unknown): boolean;
/**
 * Test if value is a valid email address.
 * @param {unknown} value to test if an email address.
 * @return {boolean} true if an email address, false otherwise.
 */
export function emailAddr(value: unknown): boolean;
/**
 * Test if a value is either an IPv4 numeric IP address.
 * The rules are:
 * must be a string
 * length must be 15 characters or less
 * There must be four octets separated by a '.'
 * No octet can be less than 0 or greater than 255.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ipv4Address(value: unknown): boolean;
/**
 * Test if a value is either an IPv4 numeric IP address.
 * The rules are:
 * must be a string
 * length must be 15 characters or less
 * There must be four octets separated by a '.'
 * No octet can be less than 0 or greater than 255.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ipv4(value: unknown): boolean;
/**
 * Test if a value is either an IPv4 numeric IP address.
 * The rules are:
 * must be a string
 * length must be 15 characters or less
 * There must be four octets separated by a '.'
 * No octet can be less than 0 or greater than 255.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ipv4Addr(value: unknown): boolean;
/**
 * Test if a value is either an IPv6 numeric IP address.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ipv6Address(value: unknown): boolean;
/**
 * Test if a value is either an IPv6 numeric IP address.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ipv6(value: unknown): boolean;
/**
 * Test if a value is either an IPv6 numeric IP address.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ipv6Addr(value: unknown): boolean;
/**
 * Test if a value is either an IPv4 or IPv6 numeric IP address.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ipAddress(value: unknown): boolean;
/**
 * Test if a value is either an IPv4 or IPv6 numeric IP address.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ip(value: unknown): boolean;
/**
 * Test if a value is either an IPv4 or IPv6 numeric IP address.
 * @param {unknown} value to test if an ip address.
 * @return {boolean} true if an ip address, false otherwise.
 */
export function ipAddr(value: unknown): boolean;
/**
 * Test is a value is a valid ipv4, ipv6 or DNS name.
 * Aliases: host, hostAddr, hostAddress.
 * @param {unknown} value to test if a host address.
 * @return {boolean} true if a host address, false otherwise.
 */
export function hostAddress(value: unknown): boolean;
/**
 * Test is a value is a valid ipv4, ipv6 or DNS name.
 * Aliases: host, hostAddr, hostAddress.
 * @param {unknown} value to test if a host address.
 * @return {boolean} true if a host address, false otherwise.
 */
export function host(value: unknown): boolean;
/**
 * Test is a value is a valid ipv4, ipv6 or DNS name.
 * Aliases: host, hostAddr, hostAddress.
 * @param {unknown} value to test if a host address.
 * @return {boolean} true if a host address, false otherwise.
 */
export function hostIp(value: unknown): boolean;
/**
 * Test is a value is a valid ipv4, ipv6 or DNS name.
 * Aliases: host, hostAddr, hostAddress.
 * @param {unknown} value to test if a host address.
 * @return {boolean} true if a host address, false otherwise.
 */
export function hostAddr(value: unknown): boolean;
/**
 * Test if a number is a valid TCP port
 * @param {unknown} value to test if its a valid TCP port
 */
export function port(value: unknown): boolean;
/**
 * Test if a number is a valid TCP port in the range 0-1023.
 * Alias: is.sysPort.
 * @param {unknown} value to test if its a valid TCP port
 */
export function systemPort(value: unknown): boolean;
/**
 * Test if a number is a valid TCP port in the range 0-1023.
 * Alias: is.sysPort.
 * @param {unknown} value to test if its a valid TCP port
 */
export function sysPort(value: unknown): boolean;
/**
 * Test if a number is a valid TCP port in the range 1024-65535.
 * @param {unknown} value to test if its a valid TCP port
 */
export function userPort(value: unknown): boolean;
/**
 * Test if a string is a credit card.
 * From http://en.wikipedia.org/wiki/Luhn_algorithm
 * @param {string} value to test if a credit card.
 * @return true if the string is the correct format, false otherwise
 */
export function creditCardNumber(str: string): boolean;
/**
 * Test if a string is a credit card.
 * From http://en.wikipedia.org/wiki/Luhn_algorithm
 * @param {string} value to test if a credit card.
 * @return true if the string is the correct format, false otherwise
 */
export function creditCard(str: string): boolean;
/**
 * Test if a string is a credit card.
 * From http://en.wikipedia.org/wiki/Luhn_algorithm
 * @param {string} value to test if a credit card.
 * @return true if the string is the correct format, false otherwise
 */
export function creditCardNum(str: string): boolean;
/**
 * Test if card number is an American Express card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function americanExpressCardNumber(str: string): boolean;
/**
 * Test if card number is an American Express card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function amexCard(str: string): boolean;
/**
 * Test if card number is an American Express card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function amexCardNum(str: string): boolean;
/**
 * Test if card number is a China UnionPay card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function chinaUnionPayCardNumber(str: string): boolean;
/**
 * Test if card number is a China UnionPay card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function chinaUnion(str: string): boolean;
/**
 * Test if card number is a China UnionPay card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function chinaUnionPayCard(str: string): boolean;
/**
 * Test if card number is a Diner's Club Carte Blance card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClubCarteBlancheCardNumber(str: string): boolean;
/**
 * Test if card number is a Diner's Club Carte Blance card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClubCB(str: string): boolean;
/**
 * Test if card number is a Diner's Club Carte Blance card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClubCarteBlancheCard(str: string): boolean;
/**
 * Test if card number is a Diner's Club International card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClubInternationalCardNumber(str: string): boolean;
/**
 * Test if card number is a Diner's Club International card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClubInt(str: string): boolean;
/**
 * Test if card number is a Diner's Club International card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClubInternationalCard(str: string): boolean;
/**
 * Test if card number is a Diner's Club USA & CA card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClubUSACanadaCardNumber(str: string): boolean;
/**
 * Test if card number is a Diner's Club USA & CA card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClub(str: string): boolean;
/**
 * Test if card number is a Diner's Club USA & CA card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dinersClubUSACanCard(str: string): boolean;
/**
 * Test if card number is a Diner's Club USA/CA card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function discoverCardNumber(str: string): boolean;
/**
 * Test if card number is a Diner's Club USA/CA card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function discover(str: string): boolean;
/**
 * Test if card number is a Diner's Club USA/CA card.
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function discoverCard(str: string): boolean;
/**
 * Test if card number is an InstaPayment card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function instaPaymentCardNumber(str: string): boolean;
/**
 * Test if card number is an InstaPayment card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function instaPayment(str: string): boolean;
/**
 * Test if card number is a JCB card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function jcbCardNumber(str: string): boolean;
/**
 * Test if card number is a JCB card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function jcb(str: string): boolean;
/**
 * Test if card number is a JCB card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function jcbCard(str: string): boolean;
/**
 * Test if card number is a Laser card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function laserCardNumber(str: string): boolean;
/**
 * Test if card number is a Laser card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function laser(str: string): boolean;
/**
 * Test if card number is a Laser card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function laserCard(str: string): boolean;
/**
 * Test if card number is a Maestro card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function maestroCardNumber(str: string): boolean;
/**
 * Test if card number is a Maestro card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function maestro(str: string): boolean;
/**
 * Test if card number is a Maestro card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function maestroCard(str: string): boolean;
/**
 * Test if card number is a Dankort card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dankortCardNumber(str: string): boolean;
/**
 * Test if card number is a Dankort card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dankort(str: string): boolean;
/**
 * Test if card number is a Dankort card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function dankortCard(str: string): boolean;
/**
 * Test if card number is a MasterCard card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function masterCardCardNumber(str: string): boolean;
/**
 * Test if card number is a MasterCard card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function masterCard(str: string): boolean;
/**
 * Test if card number is a MasterCard card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function masterCardCard(str: string): boolean;
/**
 * Test if card number is a Visa card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function visaCardNumber(str: string): boolean;
/**
 * Test if card number is a Visa card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function visa(str: string): boolean;
/**
 * Test if card number is a Visa card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function visaCard(str: string): boolean;
/**
 * Test if card number is a Visa card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function visaElectronCardNumber(str: string): boolean;
/**
 * Test if card number is a Visa card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function visaElectron(str: string): boolean;
/**
 * Test if card number is a Visa card number
 * @param {string} the credit card number string to test.
 * @return true if the string is the correct format, false otherwise
 */
export function visaElectronCard(str: string): boolean;
export function mongoId(id: unknown): boolean;
export function objectId(id: unknown): boolean;
export function objId(id: unknown): boolean;
export function matching(val: unknown, ...args: unknown[]): boolean;
export function match(val: unknown, ...args: unknown[]): boolean;
export function inArgs(val: unknown, ...args: unknown[]): boolean;
/**********************************
 ***Definitely a work in progress***
 **********************************/
/**
 * Test if a string contains a US street address
 * @param {string} the string to search
 * @return true if an address is present, false otherwise
 */
export function streetAddress(str: string): boolean;
/**********************************
 ***Definitely a work in progress***
 **********************************/
/**
 * Test if a string contains a US street address
 * @param {string} the string to search
 * @return true if an address is present, false otherwise
 */
export function street(str: string): boolean;
/**********************************
 ***Definitely a work in progress***
 **********************************/
/**
 * Test if a string contains a US street address
 * @param {string} the string to search
 * @return true if an address is present, false otherwise
 */
export function address(str: string): boolean;
/**
 * Test if a string resembles a US Zip code,
 * no regular expression will be perfect for this,
 * as there are many numbers that aren't valid zip codes
 * @param {string || Number} the string or number literal to test
 * @return true if zipcode like, false otherwise
 */
export function zipCode(str: string): boolean;
/**
 * Test if a string resembles a US Zip code,
 * no regular expression will be perfect for this,
 * as there are many numbers that aren't valid zip codes
 * @param {string || Number} the string or number literal to test
 * @return true if zipcode like, false otherwise
 */
export function zip(str: string): boolean;
/**
 * Test if a string contains a US phone number
 * @param {string} the string to search
 * @return true if str contains a phone number, false otherwise.
 */
export function phoneNumber(str: string): boolean;
/**
 * Test if a string contains a US phone number
 * @param {string} the string to search
 * @return true if str contains a phone number, false otherwise.
 */
export function phone(str: string): boolean;
export function url(val: unknown): unknown;
export function uri(val: unknown): unknown;
export function enumerator(val: unknown, ary: unknown): boolean;
declare function _enum(val: unknown, ary: unknown): boolean;
export function inArray(val: unknown, ary: unknown): boolean;
export {
  _false as false,
  _function as function,
  _null as null,
  _true as true,
  _instanceof as instanceof,
  _enum as enum,
  _arguments as arguments,
};
