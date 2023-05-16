"use strict";

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports["default"] = createMountWrapper;

var _react = _interopRequireDefault(require("react"));

var _propTypes = _interopRequireDefault(require("prop-types"));

var _airbnbPropTypes = require("airbnb-prop-types");

var _RootFinder = _interopRequireDefault(require("./RootFinder"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { "default": obj }; }

function _extends() { _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; }; return _extends.apply(this, arguments); }

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

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

/* eslint react/forbid-prop-types: 0 */
var stringOrFunction = _propTypes["default"].oneOfType([_propTypes["default"].func, _propTypes["default"].string]);

var makeValidElementType = function makeValidElementType(adapter) {
  if (!adapter) {
    return stringOrFunction;
  }

  function validElementTypeRequired(props, propName) {
    if (!adapter.isValidElementType) {
      for (var _len = arguments.length, args = new Array(_len > 2 ? _len - 2 : 0), _key = 2; _key < _len; _key++) {
        args[_key - 2] = arguments[_key];
      }

      return stringOrFunction.isRequired.apply(stringOrFunction, [props, propName].concat(args));
    }

    var propValue = props[propName]; // eslint-disable-line react/destructuring-assignment

    if (adapter.isValidElementType(propValue)) {
      return null;
    }

    return new TypeError("".concat(propName, " must be a valid element type!"));
  }

  function validElementType(props, propName) {
    var propValue = props[propName];

    if (propValue == null) {
      return null;
    }

    for (var _len2 = arguments.length, args = new Array(_len2 > 2 ? _len2 - 2 : 0), _key2 = 2; _key2 < _len2; _key2++) {
      args[_key2 - 2] = arguments[_key2];
    }

    return validElementTypeRequired.apply(void 0, [props, propName].concat(args));
  }

  validElementType.isRequired = validElementTypeRequired;
  return validElementType;
};
/**
 * This is a utility component to wrap around the nodes we are
 * passing in to `mount()`. Theoretically, you could do everything
 * we are doing without this, but this makes it easier since
 * `renderIntoDocument()` doesn't really pass back a reference to
 * the DOM node it rendered to, so we can't really "re-render" to
 * pass new props in.
 */


function createMountWrapper(node) {
  var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
  var adapter = options.adapter,
      WrappingComponent = options.wrappingComponent;

  var WrapperComponent = /*#__PURE__*/function (_React$Component) {
    _inherits(WrapperComponent, _React$Component);

    var _super = _createSuper(WrapperComponent);

    function WrapperComponent() {
      var _this;

      _classCallCheck(this, WrapperComponent);

      for (var _len3 = arguments.length, args = new Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
        args[_key3] = arguments[_key3];
      }

      _this = _super.call.apply(_super, [this].concat(args));
      var _this$props = _this.props,
          props = _this$props.props,
          wrappingComponentProps = _this$props.wrappingComponentProps,
          context = _this$props.context;
      _this.state = {
        mount: true,
        props: props,
        wrappingComponentProps: wrappingComponentProps,
        context: context
      };
      return _this;
    }

    _createClass(WrapperComponent, [{
      key: "setChildProps",
      value: function setChildProps(newProps, newContext) {
        var callback = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : undefined;
        var _this$state = this.state,
            oldProps = _this$state.props,
            oldContext = _this$state.context;

        var props = _objectSpread(_objectSpread({}, oldProps), newProps);

        var context = _objectSpread(_objectSpread({}, oldContext), newContext);

        this.setState({
          props: props,
          context: context
        }, callback);
      }
    }, {
      key: "setWrappingComponentProps",
      value: function setWrappingComponentProps(props) {
        var callback = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : undefined;
        this.setState({
          wrappingComponentProps: props
        }, callback);
      }
    }, {
      key: "render",
      value: function render() {
        var _this$props2 = this.props,
            Component = _this$props2.Component,
            refProp = _this$props2.refProp;
        var _this$state2 = this.state,
            mount = _this$state2.mount,
            props = _this$state2.props,
            wrappingComponentProps = _this$state2.wrappingComponentProps;
        if (!mount) return null; // eslint-disable-next-line react/jsx-props-no-spreading

        var component = /*#__PURE__*/_react["default"].createElement(Component, _extends({
          ref: refProp
        }, props));

        if (WrappingComponent) {
          return (
            /*#__PURE__*/
            // eslint-disable-next-line react/jsx-props-no-spreading
            _react["default"].createElement(WrappingComponent, wrappingComponentProps, /*#__PURE__*/_react["default"].createElement(_RootFinder["default"], null, component))
          );
        }

        return component;
      }
    }]);

    return WrapperComponent;
  }(_react["default"].Component);

  WrapperComponent.propTypes = {
    Component: makeValidElementType(adapter).isRequired,
    refProp: _propTypes["default"].oneOfType([_propTypes["default"].string, (0, _airbnbPropTypes.ref)()]),
    props: _propTypes["default"].object.isRequired,
    wrappingComponentProps: _propTypes["default"].object,
    context: _propTypes["default"].object
  };
  WrapperComponent.defaultProps = {
    refProp: null,
    context: null,
    wrappingComponentProps: null
  };

  if (options.context && (node.type.contextTypes || options.childContextTypes)) {
    // For full rendering, we are using this wrapper component to provide context if it is
    // specified in both the options AND the child component defines `contextTypes` statically
    // OR the merged context types for all children (the node component or deeper children) are
    // specified in options parameter under childContextTypes.
    // In that case, we define both a `getChildContext()` function and a `childContextTypes` prop.
    var childContextTypes = _objectSpread(_objectSpread({}, node.type.contextTypes), options.childContextTypes);

    WrapperComponent.prototype.getChildContext = function getChildContext() {
      return this.state.context;
    };

    WrapperComponent.childContextTypes = childContextTypes;
  }

  return WrapperComponent;
}
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9jcmVhdGVNb3VudFdyYXBwZXIuanN4Il0sIm5hbWVzIjpbInN0cmluZ09yRnVuY3Rpb24iLCJQcm9wVHlwZXMiLCJvbmVPZlR5cGUiLCJmdW5jIiwic3RyaW5nIiwibWFrZVZhbGlkRWxlbWVudFR5cGUiLCJhZGFwdGVyIiwidmFsaWRFbGVtZW50VHlwZVJlcXVpcmVkIiwicHJvcHMiLCJwcm9wTmFtZSIsImlzVmFsaWRFbGVtZW50VHlwZSIsImFyZ3MiLCJpc1JlcXVpcmVkIiwicHJvcFZhbHVlIiwiVHlwZUVycm9yIiwidmFsaWRFbGVtZW50VHlwZSIsImNyZWF0ZU1vdW50V3JhcHBlciIsIm5vZGUiLCJvcHRpb25zIiwiV3JhcHBpbmdDb21wb25lbnQiLCJ3cmFwcGluZ0NvbXBvbmVudCIsIldyYXBwZXJDb21wb25lbnQiLCJ3cmFwcGluZ0NvbXBvbmVudFByb3BzIiwiY29udGV4dCIsInN0YXRlIiwibW91bnQiLCJuZXdQcm9wcyIsIm5ld0NvbnRleHQiLCJjYWxsYmFjayIsInVuZGVmaW5lZCIsIm9sZFByb3BzIiwib2xkQ29udGV4dCIsInNldFN0YXRlIiwiQ29tcG9uZW50IiwicmVmUHJvcCIsImNvbXBvbmVudCIsIlJlYWN0IiwicHJvcFR5cGVzIiwib2JqZWN0IiwiZGVmYXVsdFByb3BzIiwidHlwZSIsImNvbnRleHRUeXBlcyIsImNoaWxkQ29udGV4dFR5cGVzIiwicHJvdG90eXBlIiwiZ2V0Q2hpbGRDb250ZXh0Il0sIm1hcHBpbmdzIjoiOzs7Ozs7Ozs7QUFBQTs7QUFDQTs7QUFDQTs7QUFDQTs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7QUFFQTtBQUVBLElBQU1BLGdCQUFnQixHQUFHQyxzQkFBVUMsU0FBVixDQUFvQixDQUFDRCxzQkFBVUUsSUFBWCxFQUFpQkYsc0JBQVVHLE1BQTNCLENBQXBCLENBQXpCOztBQUNBLElBQU1DLG9CQUFvQixHQUFHLFNBQXZCQSxvQkFBdUIsQ0FBQ0MsT0FBRCxFQUFhO0FBQ3hD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VXFDO0FBTFEsR0FBN0I7QUFPQWpCLEVBQUFBLGdCQUFnQixDQUFDa0IsWUFBakIsR0FBZ0M7QUFDOUJMLElBQUFBLE9BQU8sRUFBRSxJQURxQjtBQUU5QlgsSUFBQUEsT0FBTyxFQUFFLElBRnFCO0FBRzlCRCxJQUFBQSxzQkFBc0IsRUFBRTtBQUhNLEdBQWhDOztBQU1BLE1BQUlKLE9BQU8sQ0FBQ0ssT0FBUixLQUFvQk4sSUFBSSxDQUFDdUIsSUFBTCxDQUFVQyxZQUFWLElBQTBCdkIsT0FBTyxDQUFDd0IsaUJBQXRELENBQUosRUFBOEU7QUFDNUU7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBLFFBQU1BLGlCQUFpQixtQ0FDbEJ6QixJQUFJLENBQUN1QixJQUFMLENBQVVDLFlBRFEsR0FFbEJ2QixPQUFPLENBQUN3QixpQkFGVSxDQUF2Qjs7QUFLQXJCLElBQUFBLGdCQUFnQixDQUFDc0IsU0FBakIsQ0FBMkJDLGVBQTNCLEdBQTZDLFNBQVNBLGVBQVQsR0FBMkI7QUFDdEUsYUFBTyxLQUFLcEIsS0FBTCxDQUFXRCxPQUFsQjtBQUNELEtBRkQ7O0FBR0FGLElBQUFBLGdCQUFnQixDQUFDcUIsaUJBQWpCLEdBQXFDQSxpQkFBckM7QUFDRDs7QUFDRCxTQUFPckIsZ0JBQVA7QUFDRCIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCBSZWFjdCBmcm9tICdyZWFjdCc7XG5pbXBvcnQgUHJvcFR5cGVzIGZyb20gJ3Byb3AtdHlwZXMnO1xuaW1wb3J0IHsgcmVmIH0gZnJvbSAnYWlyYm5iLXByb3AtdHlwZXMnO1xuaW1wb3J0IFJvb3RGaW5kZXIgZnJvbSAnLi9Sb290RmluZGVyJztcblxuLyogZXNsaW50IHJlYWN0L2ZvcmJpZC1wcm9wLXR5cGVzOiAwICovXG5cbmNvbnN0IHN0cmluZ09yRnVuY3Rpb24gPSBQcm9wVHlwZXMub25lT2ZUeXBlKFtQcm9wVHlwZXMuZnVuYywgUHJvcFR5cGVzLnN0cmluZ10pO1xuY29uc3QgbWFrZVZhbGlkRWxlbWVudFR5cGUgPSAoYWRhcHRlcikgPT4ge1xuICBpZiAoIWFkYXB0ZXIpIHtcbiAgICByZXR1cm4gc3RyaW5nT3JGdW5jdGlvbjtcbiAgfVxuXG4gIGZ1bmN0aW9uIHZhbGlkRWxlbWVudFR5cGVSZXF1aXJlZChwcm9wcywgcHJvcE5hbWUsIC4uLmFyZ3MpIHtcbiAgICBpZiAoIWFkYXB0ZXIuaXNWYWxpZEVsZW1lbnRUeXBlKSB7XG4gICAgICByZXR1cm4gc3RyaW5nT3JGdW5jdGlvbi5pc1JlcXVpcmVkKHByb3BzLCBwcm9wTmFtZSwgLi4uYXJncyk7XG4gICAgfVxuICAgIGNvbnN0IHByb3BWYWx1ZSA9IHByb3BzW3Byb3BOYW1lXTsgLy8gZXNsaW50LWRpc2FibGUtbGluZSByZWFjdC9kZXN0cnVjdHVyaW5nLWFzc2lnbm1lbnRcbiAgICBpZiAoYWRhcHRlci5pc1ZhbGlkRWxlbWVudFR5cGUocHJvcFZhbHVlKSkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIHJldHVybiBuZXcgVHlwZUVycm9yKGAke3Byb3BOYW1lfSBtdXN0IGJlIGEgdmFsaWQgZWxlbWVudCB0eXBlIWApO1xuICB9XG5cbiAgZnVuY3Rpb24gdmFsaWRFbGVtZW50VHlwZShwcm9wcywgcHJvcE5hbWUsIC4uLmFyZ3MpIHtcbiAgICBjb25zdCBwcm9wVmFsdWUgPSBwcm9wc1twcm9wTmFtZV07XG4gICAgaWYgKHByb3BWYWx1ZSA9PSBudWxsKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgcmV0dXJuIHZhbGlkRWxlbWVudFR5cGVSZXF1aXJlZChwcm9wcywgcHJvcE5hbWUsIC4uLmFyZ3MpO1xuICB9XG4gIHZhbGlkRWxlbWVudFR5cGUuaXNSZXF1aXJlZCA9IHZhbGlkRWxlbWVudFR5cGVSZXF1aXJlZDtcblxuICByZXR1cm4gdmFsaWRFbGVtZW50VHlwZTtcbn07XG5cbi8qKlxuICogVGhpcyBpcyBhIHV0aWxpdHkgY29tcG9uZW50IHRvIHdyYXAgYXJvdW5kIHRoZSBub2RlcyB3ZSBhcmVcbiAqIHBhc3NpbmcgaW4gdG8gYG1vdW50KClgLiBUaGVvcmV0aWNhbGx5LCB5b3UgY291bGQgZG8gZXZlcnl0aGluZ1xuICogd2UgYXJlIGRvaW5nIHdpdGhvdXQgdGhpcywgYnV0IHRoaXMgbWFrZXMgaXQgZWFzaWVyIHNpbmNlXG4gKiBgcmVuZGVySW50b0RvY3VtZW50KClgIGRvZXNuJ3QgcmVhbGx5IHBhc3MgYmFjayBhIHJlZmVyZW5jZSB0b1xuICogdGhlIERPTSBub2RlIGl0IHJlbmRlcmVkIHRvLCBzbyB3ZSBjYW4ndCByZWFsbHkgXCJyZS1yZW5kZXJcIiB0b1xuICogcGFzcyBuZXcgcHJvcHMgaW4uXG4gKi9cbmV4cG9ydCBkZWZhdWx0IGZ1bmN0aW9uIGNyZWF0ZU1vdW50V3JhcHBlcihub2RlLCBvcHRpb25zID0ge30pIHtcbiAgY29uc3QgeyBhZGFwdGVyLCB3cmFwcGluZ0NvbXBvbmVudDogV3JhcHBpbmdDb21wb25lbnQgfSA9IG9wdGlvbnM7XG5cbiAgY2xhc3MgV3JhcHBlckNvbXBvbmVudCBleHRlbmRzIFJlYWN0LkNvbXBvbmVudCB7XG4gICAgY29uc3RydWN0b3IoLi4uYXJncykge1xuICAgICAgc3VwZXIoLi4uYXJncyk7XG4gICAgICBjb25zdCB7IHByb3BzLCB3cmFwcGluZ0NvbXBvbmVudFByb3BzLCBjb250ZXh0IH0gPSB0aGlzLnByb3BzO1xuICAgICAgdGhpcy5zdGF0ZSA9IHtcbiAgICAgICAgbW91bnQ6IHRydWUsXG4gICAgICAgIHByb3BzLFxuICAgICAgICB3cmFwcGluZ0NvbXBvbmVudFByb3BzLFxuICAgICAgICBjb250ZXh0LFxuICAgICAgfTtcbiAgICB9XG5cbiAgICBzZXRDaGlsZFByb3BzKG5ld1Byb3BzLCBuZXdDb250ZXh0LCBjYWxsYmFjayA9IHVuZGVmaW5lZCkge1xuICAgICAgY29uc3QgeyBwcm9wczogb2xkUHJvcHMsIGNvbnRleHQ6IG9sZENvbnRleHQgfSA9IHRoaXMuc3RhdGU7XG4gICAgICBjb25zdCBwcm9wcyA9IHsgLi4ub2xkUHJvcHMsIC4uLm5ld1Byb3BzIH07XG4gICAgICBjb25zdCBjb250ZXh0ID0geyAuLi5vbGRDb250ZXh0LCAuLi5uZXdDb250ZXh0IH07XG4gICAgICB0aGlzLnNldFN0YXRlKHsgcHJvcHMsIGNvbnRleHQgfSwgY2FsbGJhY2spO1xuICAgIH1cblxuICAgIHNldFdyYXBwaW5nQ29tcG9uZW50UHJvcHMocHJvcHMsIGNhbGxiYWNrID0gdW5kZWZpbmVkKSB7XG4gICAgICB0aGlzLnNldFN0YXRlKHsgd3JhcHBpbmdDb21wb25lbnRQcm9wczogcHJvcHMgfSwgY2FsbGJhY2spO1xuICAgIH1cblxuICAgIHJlbmRlcigpIHtcbiAgICAgIGNvbnN0IHsgQ29tcG9uZW50LCByZWZQcm9wIH0gPSB0aGlzLnByb3BzO1xuICAgICAgY29uc3QgeyBtb3VudCwgcHJvcHMsIHdyYXBwaW5nQ29tcG9uZW50UHJvcHMgfSA9IHRoaXMuc3RhdGU7XG4gICAgICBpZiAoIW1vdW50KSByZXR1cm4gbnVsbDtcbiAgICAgIC8vIGVzbGludC1kaXNhYmxlLW5leHQtbGluZSByZWFjdC9qc3gtcHJvcHMtbm8tc3ByZWFkaW5nXG4gICAgICBjb25zdCBjb21wb25lbnQgPSA8Q29tcG9uZW50IHJlZj17cmVmUHJvcH0gey4uLnByb3BzfSAvPjtcbiAgICAgIGlmIChXcmFwcGluZ0NvbXBvbmVudCkge1xuICAgICAgICByZXR1cm4gKFxuICAgICAgICAgIC8vIGVzbGludC1kaXNhYmxlLW5leHQtbGluZSByZWFjdC9qc3gtcHJvcHMtbm8tc3ByZWFkaW5nXG4gICAgICAgICAgPFdyYXBwaW5nQ29tcG9uZW50IHsuLi53cmFwcGluZ0NvbXBvbmVudFByb3BzfT5cbiAgICAgICAgICAgIDxSb290RmluZGVyPntjb21wb25lbnR9PC9Sb290RmluZGVyPlxuICAgICAgICAgIDwvV3JhcHBpbmdDb21wb25lbnQ+XG4gICAgICAgICk7XG4gICAgICB9XG4gICAgICByZXR1cm4gY29tcG9uZW50O1xuICAgIH1cbiAgfVxuICBXcmFwcGVyQ29tcG9uZW50LnByb3BUeXBlcyA9IHtcbiAgICBDb21wb25lbnQ6IG1ha2VWYWxpZEVsZW1lbnRUeXBlKGFkYXB0ZXIpLmlzUmVxdWlyZWQsXG4gICAgcmVmUHJvcDogUHJvcFR5cGVzLm9uZU9mVHlwZShbUHJvcFR5cGVzLnN0cmluZywgcmVmKCldKSxcbiAgICBwcm9wczogUHJvcFR5cGVzLm9iamVjdC5pc1JlcXVpcmVkLFxuICAgIHdyYXBwaW5nQ29tcG9uZW50UHJvcHM6IFByb3BUeXBlcy5vYmplY3QsXG4gICAgY29udGV4dDogUHJvcFR5cGVzLm9iamVjdCxcbiAgfTtcbiAgV3JhcHBlckNvbXBvbmVudC5kZWZhdWx0UHJvcHMgPSB7XG4gICAgcmVmUHJvcDogbnVsbCxcbiAgICBjb250ZXh0OiBudWxsLFxuICAgIHdyYXBwaW5nQ29tcG9uZW50UHJvcHM6IG51bGwsXG4gIH07XG5cbiAgaWYgKG9wdGlvbnMuY29udGV4dCAmJiAobm9kZS50eXBlLmNvbnRleHRUeXBlcyB8fCBvcHRpb25zLmNoaWxkQ29udGV4dFR5cGVzKSkge1xuICAgIC8vIEZvciBmdWxsIHJlbmRlcmluZywgd2UgYXJlIHVzaW5nIHRoaXMgd3JhcHBlciBjb21wb25lbnQgdG8gcHJvdmlkZSBjb250ZXh0IGlmIGl0IGlzXG4gICAgLy8gc3BlY2lmaWVkIGluIGJvdGggdGhlIG9wdGlvbnMgQU5EIHRoZSBjaGlsZCBjb21wb25lbnQgZGVmaW5lcyBgY29udGV4dFR5cGVzYCBzdGF0aWNhbGx5XG4gICAgLy8gT1IgdGhlIG1lcmdlZCBjb250ZXh0IHR5cGVzIGZvciBhbGwgY2hpbGRyZW4gKHRoZSBub2RlIGNvbXBvbmVudCBvciBkZWVwZXIgY2hpbGRyZW4pIGFyZVxuICAgIC8vIHNwZWNpZmllZCBpbiBvcHRpb25zIHBhcmFtZXRlciB1bmRlciBjaGlsZENvbnRleHRUeXBlcy5cbiAgICAvLyBJbiB0aGF0IGNhc2UsIHdlIGRlZmluZSBib3RoIGEgYGdldENoaWxkQ29udGV4dCgpYCBmdW5jdGlvbiBhbmQgYSBgY2hpbGRDb250ZXh0VHlwZXNgIHByb3AuXG4gICAgY29uc3QgY2hpbGRDb250ZXh0VHlwZXMgPSB7XG4gICAgICAuLi5ub2RlLnR5cGUuY29udGV4dFR5cGVzLFxuICAgICAgLi4ub3B0aW9ucy5jaGlsZENvbnRleHRUeXBlcyxcbiAgICB9O1xuXG4gICAgV3JhcHBlckNvbXBvbmVudC5wcm90b3R5cGUuZ2V0Q2hpbGRDb250ZXh0ID0gZnVuY3Rpb24gZ2V0Q2hpbGRDb250ZXh0KCkge1xuICAgICAgcmV0dXJuIHRoaXMuc3RhdGUuY29udGV4dDtcbiAgICB9O1xuICAgIFdyYXBwZXJDb21wb25lbnQuY2hpbGRDb250ZXh0VHlwZXMgPSBjaGlsZENvbnRleHRUeXBlcztcbiAgfVxuICByZXR1cm4gV3JhcHBlckNvbXBvbmVudDtcbn1cbiJdfQ==
//# sourceMappingURL=createMountWrapper.js.map