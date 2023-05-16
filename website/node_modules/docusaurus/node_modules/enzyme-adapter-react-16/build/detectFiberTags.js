"use strict";

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

var _react = _interopRequireDefault(require("react"));

var _reactDom = _interopRequireDefault(require("react-dom"));

var _enzymeAdapterUtils = require("enzyme-adapter-utils");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { "default": obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function"); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } }); if (superClass) _setPrototypeOf(subClass, superClass); }

function _setPrototypeOf(o, p) { _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) { o.__proto__ = p; return o; }; return _setPrototypeOf(o, p); }

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = _getPrototypeOf(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = _getPrototypeOf(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return _possibleConstructorReturn(this, result); }; }

function _possibleConstructorReturn(self, call) { if (call && (_typeof(call) === "object" || typeof call === "function")) { return call; } return _assertThisInitialized(self); }

function _assertThisInitialized(self) { if (self === void 0) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return self; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Date.prototype.toString.call(Reflect.construct(Date, [], function () {})); return true; } catch (e) { return false; } }

function _getPrototypeOf(o) { _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf(o) { return o.__proto__ || Object.getPrototypeOf(o); }; return _getPrototypeOf(o); }

function getFiber(element) {
  var container = global.document.createElement('div');
  var inst = null;

  var Tester = /*#__PURE__*/function (_React$Component) {
    _inherits(Tester, _React$Component);

    var _super = _createSuper(Tester);

    function Tester() {
      _classCallCheck(this, Tester);

      return _super.apply(this, arguments);
    }

    _createClass(Tester, [{
      key: "render",
      value: function render() {
        inst = this;
        return element;
      }
    }]);

    return Tester;
  }(_react["default"].Component);

  _reactDom["default"].render( /*#__PURE__*/_react["default"].createElement(Tester), container);

  return inst._reactInternalFiber.child;
}

function getLazyFiber(LazyComponent) {
  var container = global.document.createElement('div');
  var inst = null; // eslint-disable-next-line react/prefer-stateless-function

  var Tester = /*#__PURE__*/function (_React$Component2) {
    _inherits(Tester, _React$Component2);

    var _super2 = _createSuper(Tester);

    function Tester() {
      _classCallCheck(this, Tester);

      return _super2.apply(this, arguments);
    }

    _createClass(Tester, [{
      key: "render",
      value: function render() {
        inst = this;
        return /*#__PURE__*/_react["default"].createElement(LazyComponent);
      }
    }]);

    return Tester;
  }(_react["default"].Component); // eslint-disable-next-line react/prefer-stateless-function


  var SuspenseWrapper = /*#__PURE__*/function (_React$Component3) {
    _inherits(SuspenseWrapper, _React$Component3);

    var _super3 = _createSuper(SuspenseWrapper);

    function SuspenseWrapper() {
      _classCallCheck(this, SuspenseWrapper);

      return _super3.apply(this, arguments);
    }

    _createClass(SuspenseWrapper, [{
      key: "render",
      value: function render() {
        return /*#__PURE__*/_react["default"].createElement(_react["default"].Suspense, {
          fallback: false
        }, /*#__PURE__*/_react["default"].createElement(Tester));
      }
    }]);

    return SuspenseWrapper;
  }(_react["default"].Component);

  _reactDom["default"].render( /*#__PURE__*/_react["default"].createElement(SuspenseWrapper), container);

  return inst._reactInternalFiber.child;
}

module.exports = function detectFiberTags() {
  var supportsMode = typeof _react["default"].StrictMode !== 'undefined';
  var supportsContext = typeof _react["default"].createContext !== 'undefined';
  var supportsForwardRef = typeof _react["default"].forwardRef !== 'undefined';
  var supportsMemo = typeof _react["default"].memo !== 'undefined';
  var supportsProfiler = typeof _react["default"].unstable_Profiler !== 'undefined' || typeof _react["default"].Profiler !== 'undefined';
  var supportsSuspense = typeof _react["default"].Suspense !== 'undefined';
  var supportsLazy = typeof _react["default"].lazy !== 'undefined';

  function Fn() {
    return null;
  } // eslint-disable-next-line react/prefer-stateless-function


  var Cls = /*#__PURE__*/function (_React$Component4) {
    _inherits(Cls, _React$Component4);

    var _super4 = _createSuper(Cls);

    function Cls() {
      _classCallCheck(this, Cls);

      return _super4.apply(this, arguments);
    }

    _createClass(Cls, [{
      key: "render",
      value: function render() {
        return null;
      }
    }]);

    return Cls;
  }(_react["default"].Component);

  var Ctx = null;
  var FwdRef = null;
  var LazyComponent = null;

  if (supportsContext) {
    Ctx = /*#__PURE__*/_react["default"].createContext();
  }

  if (supportsForwardRef) {
    // React will warn if we don't have both arguments.
    // eslint-disable-next-line no-unused-vars
    FwdRef = /*#__PURE__*/_react["default"].forwardRef(function (props, ref) {
      return null;
    });
  }

  if (supportsLazy) {
    LazyComponent = /*#__PURE__*/_react["default"].lazy(function () {
      return (0, _enzymeAdapterUtils.fakeDynamicImport)(function () {
        return null;
      });
    });
  }

  return {
    HostRoot: getFiber('test')["return"]["return"].tag,
    // Go two levels above to find the root
    ClassComponent: getFiber( /*#__PURE__*/_react["default"].createElement(Cls)).tag,
    Fragment: getFiber([['nested']]).tag,
    FunctionalComponent: getFiber( /*#__PURE__*/_react["default"].createElement(Fn)).tag,
    MemoSFC: supportsMemo ? getFiber( /*#__PURE__*/_react["default"].createElement( /*#__PURE__*/_react["default"].memo(Fn))).tag : -1,
    MemoClass: supportsMemo ? getFiber( /*#__PURE__*/_react["default"].createElement( /*#__PURE__*/_react["default"].memo(Cls))).tag : -1,
    HostPortal: getFiber( /*#__PURE__*/_reactDom["default"].createPortal(null, global.document.createElement('div'))).tag,
    HostComponent: getFiber( /*#__PURE__*/_react["default"].createElement('span')).tag,
    HostText: getFiber('text').tag,
    Mode: supportsMode ? getFiber( /*#__PURE__*/_react["default"].createElement(_react["default"].StrictMode)).tag : -1,
    ContextConsumer: supportsContext ? getFiber( /*#__PURE__*/_react["default"].createElement(Ctx.Consumer, null, function () {
      return null;
    })).tag : -1,
    ContextProvider: supportsContext ? getFiber( /*#__PURE__*/_react["default"].createElement(Ctx.Provider, {
      value: null
    }, null)).tag : -1,
    ForwardRef: supportsForwardRef ? getFiber( /*#__PURE__*/_react["default"].createElement(FwdRef)).tag : -1,
    Profiler: supportsProfiler ? getFiber( /*#__PURE__*/_react["default"].createElement(_react["default"].Profiler || _react["default"].unstable_Profiler, {
      id: 'mock',
      onRender: function onRender() {}
    })).tag : -1,
    Suspense: supportsSuspense ? getFiber( /*#__PURE__*/_react["default"].createElement(_react["default"].Suspense, {
      fallback: false
    })).tag : -1,
    Lazy: supportsLazy ? getLazyFiber(LazyComponent).tag : -1
  };
};
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9kZXRlY3RGaWJlclRhZ3MuanMiXSwibmFtZXMiOlsiZ2V0RmliZXIiLCJlbGVtZW50IiwiY29udGFpbmVyIiwiZ2xvYmFsIiwiZG9jdW1lbnQiLCJjcmVhdGVFbGVtZW50IiwiaW5zdCIsIlRlc3RlciIsIlJlYWN0IiwiQ29tcG9uZW50IiwiUmVhY3RET00iLCJyZW5kZXIiLCJfcmVhY3RJbnRlcm5hbEZpYmVyIiwiY2hpbGQiLCJnZXRMYXp5RmliZXIiLCJMYXp5Q29tcG9uZW50IiwiU3VzcGVuc2VXcmFwcGVyIiwiU3VzcGVuc2UiLCJmYWxsYmFjayIsIm1vZHVsZSIsImV4cG9ydHMiLCJkZXRlY3RGaWJlclRhZ3MiLCJzdXBwb3J0c01vZGUiLCJTdHJpY3RNb2RlIiwic3VwcG9ydHNDb250ZXh0IiwiY3JlYXRlQ29udGV4dCIsInN1cHBvcnRzRm9yd2FyZFJlZiIsImZvcndhcmRSZWYiLCJzdXBwb3J0c01lbW8iLCJtZW1vIiwic3VwcG9ydHNQcm9maWxlciIsInVuc3RhYmxlX1Byb2ZpbGVyIiwiUHJvZmlsZXIiLCJzdXBwb3J0c1N1c3BlbnNlIiwic3VwcG9ydHNMYXp5IiwibGF6eSIsIkZuIiwiQ2xzIiwiQ3R4IiwiRndkUmVmIiwicHJvcHMiLCJyZWYiLCJIb3N0Um9vdCIsInRhZyIsIkNsYXNzQ29tcG9uZW50IiwiRnJhZ21lbnQiLCJGdW5jdGlvbmFsQ29tcG9uZW50IiwiTWVtb1NGQyIsIk1lbW9DbGFzcyIsIkhvc3RQb3J0YWwiLCJjcmVhdGVQb3J0YWwiLCJIb3N0Q29tcG9uZW50IiwiSG9zdFRleHQiLCJNb2RlIiwiQ29udGV4dENvbnN1bWVyIiwiQ29uc3VtZXIiLCJDb250ZXh0UHJvdmlkZXIiLCJQcm92aWRlciIsInZhbHVlIiwiRm9yd2FyZFJlZiIsImlkIiwib25SZW5kZXIiLCJMYXp5Il0sIm1hcHBpbmdzIjoiOzs7O0FBQUE7O0FBQ0E7O0FBQ0E7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7OztBQUVBLFNBQVNBLFFBQVQs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QXNDSSxHQURaLEdBRTFCLENBQUMsQ0F6QkE7QUEwQkxYLElBQUFBLFFBQVEsRUFBRUYsZ0JBQWdCLEdBQ3RCOUIsUUFBUSxlQUFDUSxrQkFBTUgsYUFBTixDQUFxQkcsa0JBQU13QixRQUFOLElBQWtCeEIsa0JBQU11QixpQkFBN0MsRUFBaUU7QUFBRTZCLE1BQUFBLEVBQUUsRUFBRSxNQUFOO0FBQWNDLE1BQUFBLFFBQWQsc0JBQXlCLENBQUU7QUFBM0IsS0FBakUsQ0FBRCxDQUFSLENBQTBHbEIsR0FEcEYsR0FFdEIsQ0FBQyxDQTVCQTtBQTZCTDFCLElBQUFBLFFBQVEsRUFBRWdCLGdCQUFnQixHQUN0QmpDLFFBQVEsZUFBQ1Esa0JBQU1ILGFBQU4sQ0FBb0JHLGtCQUFNUyxRQUExQixFQUFvQztBQUFFQyxNQUFBQSxRQUFRLEVBQUU7QUFBWixLQUFwQyxDQUFELENBQVIsQ0FBbUV5QixHQUQ3QyxHQUV0QixDQUFDLENBL0JBO0FBZ0NMbUIsSUFBQUEsSUFBSSxFQUFFNUIsWUFBWSxHQUNkcEIsWUFBWSxDQUFDQyxhQUFELENBQVosQ0FBNEI0QixHQURkLEdBRWQsQ0FBQztBQWxDQSxHQUFQO0FBb0NELENBckVEIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IFJlYWN0IGZyb20gJ3JlYWN0JztcbmltcG9ydCBSZWFjdERPTSBmcm9tICdyZWFjdC1kb20nO1xuaW1wb3J0IHsgZmFrZUR5bmFtaWNJbXBvcnQgfSBmcm9tICdlbnp5bWUtYWRhcHRlci11dGlscyc7XG5cbmZ1bmN0aW9uIGdldEZpYmVyKGVsZW1lbnQpIHtcbiAgY29uc3QgY29udGFpbmVyID0gZ2xvYmFsLmRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2RpdicpO1xuICBsZXQgaW5zdCA9IG51bGw7XG4gIGNsYXNzIFRlc3RlciBleHRlbmRzIFJlYWN0LkNvbXBvbmVudCB7XG4gICAgcmVuZGVyKCkge1xuICAgICAgaW5zdCA9IHRoaXM7XG4gICAgICByZXR1cm4gZWxlbWVudDtcbiAgICB9XG4gIH1cbiAgUmVhY3RET00ucmVuZGVyKFJlYWN0LmNyZWF0ZUVsZW1lbnQoVGVzdGVyKSwgY29udGFpbmVyKTtcbiAgcmV0dXJuIGluc3QuX3JlYWN0SW50ZXJuYWxGaWJlci5jaGlsZDtcbn1cblxuZnVuY3Rpb24gZ2V0TGF6eUZpYmVyKExhenlDb21wb25lbnQpIHtcbiAgY29uc3QgY29udGFpbmVyID0gZ2xvYmFsLmRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2RpdicpO1xuICBsZXQgaW5zdCA9IG51bGw7XG4gIC8vIGVzbGludC1kaXNhYmxlLW5leHQtbGluZSByZWFjdC9wcmVmZXItc3RhdGVsZXNzLWZ1bmN0aW9uXG4gIGNsYXNzIFRlc3RlciBleHRlbmRzIFJlYWN0LkNvbXBvbmVudCB7XG4gICAgcmVuZGVyKCkge1xuICAgICAgaW5zdCA9IHRoaXM7XG4gICAgICByZXR1cm4gUmVhY3QuY3JlYXRlRWxlbWVudChMYXp5Q29tcG9uZW50KTtcbiAgICB9XG4gIH1cbiAgLy8gZXNsaW50LWRpc2FibGUtbmV4dC1saW5lIHJlYWN0L3ByZWZlci1zdGF0ZWxlc3MtZnVuY3Rpb25cbiAgY2xhc3MgU3VzcGVuc2VXcmFwcGVyIGV4dGVuZHMgUmVhY3QuQ29tcG9uZW50IHtcbiAgICByZW5kZXIoKSB7XG4gICAgICByZXR1cm4gUmVhY3QuY3JlYXRlRWxlbWVudChcbiAgICAgICAgUmVhY3QuU3VzcGVuc2UsXG4gICAgICAgIHsgZmFsbGJhY2s6IGZhbHNlIH0sXG4gICAgICAgIFJlYWN0LmNyZWF0ZUVsZW1lbnQoVGVzdGVyKSxcbiAgICAgICk7XG4gICAgfVxuICB9XG4gIFJlYWN0RE9NLnJlbmRlcihSZWFjdC5jcmVhdGVFbGVtZW50KFN1c3BlbnNlV3JhcHBlciksIGNvbnRhaW5lcik7XG4gIHJldHVybiBpbnN0Ll9yZWFjdEludGVybmFsRmliZXIuY2hpbGQ7XG59XG5cbm1vZHVsZS5leHBvcnRzID0gZnVuY3Rpb24gZGV0ZWN0RmliZXJUYWdzKCkge1xuICBjb25zdCBzdXBwb3J0c01vZGUgPSB0eXBlb2YgUmVhY3QuU3RyaWN0TW9kZSAhPT0gJ3VuZGVmaW5lZCc7XG4gIGNvbnN0IHN1cHBvcnRzQ29udGV4dCA9IHR5cGVvZiBSZWFjdC5jcmVhdGVDb250ZXh0ICE9PSAndW5kZWZpbmVkJztcbiAgY29uc3Qgc3VwcG9ydHNGb3J3YXJkUmVmID0gdHlwZW9mIFJlYWN0LmZvcndhcmRSZWYgIT09ICd1bmRlZmluZWQnO1xuICBjb25zdCBzdXBwb3J0c01lbW8gPSB0eXBlb2YgUmVhY3QubWVtbyAhPT0gJ3VuZGVmaW5lZCc7XG4gIGNvbnN0IHN1cHBvcnRzUHJvZmlsZXIgPSB0eXBlb2YgUmVhY3QudW5zdGFibGVfUHJvZmlsZXIgIT09ICd1bmRlZmluZWQnIHx8IHR5cGVvZiBSZWFjdC5Qcm9maWxlciAhPT0gJ3VuZGVmaW5lZCc7XG4gIGNvbnN0IHN1cHBvcnRzU3VzcGVuc2UgPSB0eXBlb2YgUmVhY3QuU3VzcGVuc2UgIT09ICd1bmRlZmluZWQnO1xuICBjb25zdCBzdXBwb3J0c0xhenkgPSB0eXBlb2YgUmVhY3QubGF6eSAhPT0gJ3VuZGVmaW5lZCc7XG5cbiAgZnVuY3Rpb24gRm4oKSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cbiAgLy8gZXNsaW50LWRpc2FibGUtbmV4dC1saW5lIHJlYWN0L3ByZWZlci1zdGF0ZWxlc3MtZnVuY3Rpb25cbiAgY2xhc3MgQ2xzIGV4dGVuZHMgUmVhY3QuQ29tcG9uZW50IHtcbiAgICByZW5kZXIoKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gIH1cbiAgbGV0IEN0eCA9IG51bGw7XG4gIGxldCBGd2RSZWYgPSBudWxsO1xuICBsZXQgTGF6eUNvbXBvbmVudCA9IG51bGw7XG4gIGlmIChzdXBwb3J0c0NvbnRleHQpIHtcbiAgICBDdHggPSBSZWFjdC5jcmVhdGVDb250ZXh0KCk7XG4gIH1cbiAgaWYgKHN1cHBvcnRzRm9yd2FyZFJlZikge1xuICAgIC8vIFJlYWN0IHdpbGwgd2FybiBpZiB3ZSBkb24ndCBoYXZlIGJvdGggYXJndW1lbnRzLlxuICAgIC8vIGVzbGludC1kaXNhYmxlLW5leHQtbGluZSBuby11bnVzZWQtdmFyc1xuICAgIEZ3ZFJlZiA9IFJlYWN0LmZvcndhcmRSZWYoKHByb3BzLCByZWYpID0+IG51bGwpO1xuICB9XG4gIGlmIChzdXBwb3J0c0xhenkpIHtcbiAgICBMYXp5Q29tcG9uZW50ID0gUmVhY3QubGF6eSgoKSA9PiBmYWtlRHluYW1pY0ltcG9ydCgoKSA9PiBudWxsKSk7XG4gIH1cblxuICByZXR1cm4ge1xuICAgIEhvc3RSb290OiBnZXRGaWJlcigndGVzdCcpLnJldHVybi5yZXR1cm4udGFnLCAvLyBHbyB0d28gbGV2ZWxzIGFib3ZlIHRvIGZpbmQgdGhlIHJvb3RcbiAgICBDbGFzc0NvbXBvbmVudDogZ2V0RmliZXIoUmVhY3QuY3JlYXRlRWxlbWVudChDbHMpKS50YWcsXG4gICAgRnJhZ21lbnQ6IGdldEZpYmVyKFtbJ25lc3RlZCddXSkudGFnLFxuICAgIEZ1bmN0aW9uYWxDb21wb25lbnQ6IGdldEZpYmVyKFJlYWN0LmNyZWF0ZUVsZW1lbnQoRm4pKS50YWcsXG4gICAgTWVtb1NGQzogc3VwcG9ydHNNZW1vXG4gICAgICA/IGdldEZpYmVyKFJlYWN0LmNyZWF0ZUVsZW1lbnQoUmVhY3QubWVtbyhGbikpKS50YWdcbiAgICAgIDogLTEsXG4gICAgTWVtb0NsYXNzOiBzdXBwb3J0c01lbW9cbiAgICAgID8gZ2V0RmliZXIoUmVhY3QuY3JlYXRlRWxlbWVudChSZWFjdC5tZW1vKENscykpKS50YWdcbiAgICAgIDogLTEsXG4gICAgSG9zdFBvcnRhbDogZ2V0RmliZXIoUmVhY3RET00uY3JlYXRlUG9ydGFsKG51bGwsIGdsb2JhbC5kb2N1bWVudC5jcmVhdGVFbGVtZW50KCdkaXYnKSkpLnRhZyxcbiAgICBIb3N0Q29tcG9uZW50OiBnZXRGaWJlcihSZWFjdC5jcmVhdGVFbGVtZW50KCdzcGFuJykpLnRhZyxcbiAgICBIb3N0VGV4dDogZ2V0RmliZXIoJ3RleHQnKS50YWcsXG4gICAgTW9kZTogc3VwcG9ydHNNb2RlXG4gICAgICA/IGdldEZpYmVyKFJlYWN0LmNyZWF0ZUVsZW1lbnQoUmVhY3QuU3RyaWN0TW9kZSkpLnRhZ1xuICAgICAgOiAtMSxcbiAgICBDb250ZXh0Q29uc3VtZXI6IHN1cHBvcnRzQ29udGV4dFxuICAgICAgPyBnZXRGaWJlcihSZWFjdC5jcmVhdGVFbGVtZW50KEN0eC5Db25zdW1lciwgbnVsbCwgKCkgPT4gbnVsbCkpLnRhZ1xuICAgICAgOiAtMSxcbiAgICBDb250ZXh0UHJvdmlkZXI6IHN1cHBvcnRzQ29udGV4dFxuICAgICAgPyBnZXRGaWJlcihSZWFjdC5jcmVhdGVFbGVtZW50KEN0eC5Qcm92aWRlciwgeyB2YWx1ZTogbnVsbCB9LCBudWxsKSkudGFnXG4gICAgICA6IC0xLFxuICAgIEZvcndhcmRSZWY6IHN1cHBvcnRzRm9yd2FyZFJlZlxuICAgICAgPyBnZXRGaWJlcihSZWFjdC5jcmVhdGVFbGVtZW50KEZ3ZFJlZikpLnRhZ1xuICAgICAgOiAtMSxcbiAgICBQcm9maWxlcjogc3VwcG9ydHNQcm9maWxlclxuICAgICAgPyBnZXRGaWJlcihSZWFjdC5jcmVhdGVFbGVtZW50KChSZWFjdC5Qcm9maWxlciB8fCBSZWFjdC51bnN0YWJsZV9Qcm9maWxlciksIHsgaWQ6ICdtb2NrJywgb25SZW5kZXIoKSB7fSB9KSkudGFnXG4gICAgICA6IC0xLFxuICAgIFN1c3BlbnNlOiBzdXBwb3J0c1N1c3BlbnNlXG4gICAgICA/IGdldEZpYmVyKFJlYWN0LmNyZWF0ZUVsZW1lbnQoUmVhY3QuU3VzcGVuc2UsIHsgZmFsbGJhY2s6IGZhbHNlIH0pKS50YWdcbiAgICAgIDogLTEsXG4gICAgTGF6eTogc3VwcG9ydHNMYXp5XG4gICAgICA/IGdldExhenlGaWJlcihMYXp5Q29tcG9uZW50KS50YWdcbiAgICAgIDogLTEsXG4gIH07XG59O1xuIl19
//# sourceMappingURL=detectFiberTags.js.map