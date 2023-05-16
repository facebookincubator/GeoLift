"use strict";

var _object = _interopRequireDefault(require("object.assign"));

var _react = _interopRequireDefault(require("react"));

var _reactDom = _interopRequireDefault(require("react-dom"));

var _server = _interopRequireDefault(require("react-dom/server"));

var _shallow = _interopRequireDefault(require("react-test-renderer/shallow"));

var _package = require("react-test-renderer/package.json");

var _testUtils = _interopRequireDefault(require("react-dom/test-utils"));

var _semver = _interopRequireDefault(require("semver"));

var _checkPropTypes2 = _interopRequireDefault(require("prop-types/checkPropTypes"));

var _has = _interopRequireDefault(require("has"));

var _reactIs = require("react-is");

var _enzyme = require("enzyme");

var _Utils = require("enzyme/build/Utils");

var _enzymeShallowEqual = _interopRequireDefault(require("enzyme-shallow-equal"));

var _enzymeAdapterUtils = require("enzyme-adapter-utils");

var _findCurrentFiberUsingSlowPath = _interopRequireDefault(require("./findCurrentFiberUsingSlowPath"));

var _detectFiberTags = _interopRequireDefault(require("./detectFiberTags"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { "default": obj }; }

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

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

var is164 = !!_testUtils["default"].Simulate.touchStart; // 16.4+

var is165 = !!_testUtils["default"].Simulate.auxClick; // 16.5+

var is166 = is165 && !_react["default"].unstable_AsyncMode; // 16.6+

var is168 = is166 && typeof _testUtils["default"].act === 'function';

var hasShouldComponentUpdateBug = _semver["default"].satisfies(_package.version, '< 16.8'); // Lazily populated if DOM is available.


var FiberTags = null;

function nodeAndSiblingsArray(nodeWithSibling) {
  var array = [];
  var node = nodeWithSibling;

  while (node != null) {
    array.push(node);
    node = node.sibling;
  }

  return array;
}

function flatten(arr) {
  var result = [];
  var stack = [{
    i: 0,
    array: arr
  }];

  while (stack.length) {
    var n = stack.pop();

    while (n.i < n.array.length) {
      var el = n.array[n.i];
      n.i += 1;

      if (Array.isArray(el)) {
        stack.push(n);
        stack.push({
          i: 0,
          array: el
        });
        break;
      }

      result.push(el);
    }
  }

  return result;
}

function nodeTypeFromType(type) {
  if (type === _reactIs.Portal) {
    return 'portal';
  }

  return (0, _enzymeAdapterUtils.nodeTypeFromType)(type);
}

function isMemo(type) {
  return (0, _enzymeAdapterUtils.compareNodeTypeOf)(type, _reactIs.Memo);
}

function isLazy(type) {
  return (0, _enzymeAdapterUtils.compareNodeTypeOf)(type, _reactIs.Lazy);
}

function unmemoType(type) {
  return isMemo(type) ? type.type : type;
}

function transformSuspense(renderedEl, prerenderEl, _ref) {
  var suspenseFallback = _ref.suspenseFallback;

  if (!(0, _reactIs.isSuspense)(renderedEl)) {
    return renderedEl;
  }

  var children = renderedEl.props.children;

  if (suspenseFallback) {
    var fallback = renderedEl.props.fallback;
    children = replaceLazyWithFallback(children, fallback);
  }

  var _renderedEl$type = renderedEl.type,
      propTypes = _renderedEl$type.propTypes,
      defaultProps = _renderedEl$type.defaultProps,
      contextTypes = _renderedEl$type.contextTypes,
      contextType = _renderedEl$type.contextType,
      childContextTypes = _renderedEl$type.childContextTypes;
  var FakeSuspense = (0, _object["default"])(isStateful(prerenderEl.type) ? /*#__PURE__*/function (_prerenderEl$type) {
    _inherits(FakeSuspense, _prerenderEl$type);

    var _super = _createSuper(FakeSuspense);

    function FakeSuspense() {
      _classCallCheck(this, FakeSuspense);

      return _super.apply(this, arguments);
    }

    _createClass(FakeSuspense, [{
      key: "render",
      value: function render() {
        var type = prerenderEl.type,
            props = prerenderEl.props;
        return /*#__PURE__*/_react["default"].createElement(type, _objectSpread(_objectSpread({}, props), this.props), children);
      }
    }]);

    return FakeSuspense;
  }(prerenderEl.type) : function FakeSuspense(props) {
    // eslint-disable-line prefer-arrow-callback
    return /*#__PURE__*/_react["default"].createElement(renderedEl.type, _objectSpread(_objectSpread({}, renderedEl.props), props), children);
  }, {
    propTypes: propTypes,
    defaultProps: defaultProps,
    contextTypes: contextTypes,
    contextType: contextType,
    childContextTypes: childContextTypes
  });
  return /*#__PURE__*/_react["default"].createElement(FakeSuspense, null, children);
}

function elementToTree(el) {
  if (!(0, _reactIs.isPortal)(el)) {
    return (0, _enzymeAdapterUtils.elementToTree)(el, elementToTree);
  }

  var children = el.children,
      containerInfo = el.containerInfo;
  var props = {
    children: children,
    containerInfo: containerInfo
  };
  return {
    nodeType: 'portal',
    type: _reactIs.Portal,
    props: props,
    key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(el.key),
    ref: el.ref || null,
    instance: null,
    rendered: elementToTree(el.children)
  };
}

function _toTree(vnode) {
  if (vnode == null) {
    return null;
  } // TODO(lmr): I'm not really sure I understand whether or not this is what
  // i should be doing, or if this is a hack for something i'm doing wrong
  // somewhere else. Should talk to sebastian about this perhaps


  var node = (0, _findCurrentFiberUsingSlowPath["default"])(vnode);

  switch (node.tag) {
    case FiberTags.HostRoot:
      return childrenToTree(node.child);

    case FiberTags.HostPortal:
      {
        var containerInfo = node.stateNode.containerInfo,
            children = node.memoizedProps;
        var props = {
          containerInfo: containerInfo,
          children: children
        };
        return {
          nodeType: 'portal',
          type: _reactIs.Portal,
          props: props,
          key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(node.key),
          ref: node.ref,
          instance: null,
          rendered: childrenToTree(node.child)
        };
      }

    case FiberTags.ClassComponent:
      return {
        nodeType: 'class',
        type: node.type,
        props: _objectSpread({}, node.memoizedProps),
        key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(node.key),
        ref: node.ref,
        instance: node.stateNode,
        rendered: childrenToTree(node.child)
      };

    case FiberTags.FunctionalComponent:
      return {
        nodeType: 'function',
        type: node.type,
        props: _objectSpread({}, node.memoizedProps),
        key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(node.key),
        ref: node.ref,
        instance: null,
        rendered: childrenToTree(node.child)
      };

    case FiberTags.MemoClass:
      return {
        nodeType: 'class',
        type: node.elementType.type,
        props: _objectSpread({}, node.memoizedProps),
        key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(node.key),
        ref: node.ref,
        instance: node.stateNode,
        rendered: childrenToTree(node.child.child)
      };

    case FiberTags.MemoSFC:
      {
        var renderedNodes = flatten(nodeAndSiblingsArray(node.child).map(_toTree));

        if (renderedNodes.length === 0) {
          renderedNodes = [node.memoizedProps.children];
        }

        return {
          nodeType: 'function',
          type: node.elementType,
          props: _objectSpread({}, node.memoizedProps),
          key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(node.key),
          ref: node.ref,
          instance: null,
          rendered: renderedNodes
        };
      }

    case FiberTags.HostComponent:
      {
        var _renderedNodes = flatten(nodeAndSiblingsArray(node.child).map(_toTree));

        if (_renderedNodes.length === 0) {
          _renderedNodes = [node.memoizedProps.children];
        }

        return {
          nodeType: 'host',
          type: node.type,
          props: _objectSpread({}, node.memoizedProps),
          key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(node.key),
          ref: node.ref,
          instance: node.stateNode,
          rendered: _renderedNodes
        };
      }

    case FiberTags.HostText:
      return node.memoizedProps;

    case FiberTags.Fragment:
    case FiberTags.Mode:
    case FiberTags.ContextProvider:
    case FiberTags.ContextConsumer:
      return childrenToTree(node.child);

    case FiberTags.Profiler:
    case FiberTags.ForwardRef:
      {
        return {
          nodeType: 'function',
          type: node.type,
          props: _objectSpread({}, node.pendingProps),
          key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(node.key),
          ref: node.ref,
          instance: null,
          rendered: childrenToTree(node.child)
        };
      }

    case FiberTags.Suspense:
      {
        return {
          nodeType: 'function',
          type: _reactIs.Suspense,
          props: _objectSpread({}, node.memoizedProps),
          key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(node.key),
          ref: node.ref,
          instance: null,
          rendered: childrenToTree(node.child)
        };
      }

    case FiberTags.Lazy:
      return childrenToTree(node.child);

    default:
      throw new Error("Enzyme Internal Error: unknown node with tag ".concat(node.tag));
  }
}

function childrenToTree(node) {
  if (!node) {
    return null;
  }

  var children = nodeAndSiblingsArray(node);

  if (children.length === 0) {
    return null;
  }

  if (children.length === 1) {
    return _toTree(children[0]);
  }

  return flatten(children.map(_toTree));
}

function _nodeToHostNode(_node) {
  // NOTE(lmr): node could be a function component
  // which wont have an instance prop, but we can get the
  // host node associated with its return value at that point.
  // Although this breaks down if the return value is an array,
  // as is possible with React 16.
  var node = _node;

  while (node && !Array.isArray(node) && node.instance === null) {
    node = node.rendered;
  } // if the SFC returned null effectively, there is no host node.


  if (!node) {
    return null;
  }

  var mapper = function mapper(item) {
    if (item && item.instance) return _reactDom["default"].findDOMNode(item.instance);
    return null;
  };

  if (Array.isArray(node)) {
    return node.map(mapper);
  }

  if (Array.isArray(node.rendered) && node.nodeType === 'class') {
    return node.rendered.map(mapper);
  }

  return mapper(node);
}

function replaceLazyWithFallback(node, fallback) {
  if (!node) {
    return null;
  }

  if (Array.isArray(node)) {
    return node.map(function (el) {
      return replaceLazyWithFallback(el, fallback);
    });
  }

  if (isLazy(node.type)) {
    return fallback;
  }

  return _objectSpread(_objectSpread({}, node), {}, {
    props: _objectSpread(_objectSpread({}, node.props), {}, {
      children: replaceLazyWithFallback(node.props.children, fallback)
    })
  });
}

var eventOptions = {
  animation: true,
  pointerEvents: is164,
  auxClick: is165
};

function getEmptyStateValue() {
  // this handles a bug in React 16.0 - 16.2
  // see https://github.com/facebook/react/commit/39be83565c65f9c522150e52375167568a2a1459
  // also see https://github.com/facebook/react/pull/11965
  // eslint-disable-next-line react/prefer-stateless-function
  var EmptyState = /*#__PURE__*/function (_React$Component) {
    _inherits(EmptyState, _React$Component);

    var _super2 = _createSuper(EmptyState);

    function EmptyState() {
      _classCallCheck(this, EmptyState);

      return _super2.apply(this, arguments);
    }

    _createClass(EmptyState, [{
      key: "render",
      value: function render() {
        return null;
      }
    }]);

    return EmptyState;
  }(_react["default"].Component);

  var testRenderer = new _shallow["default"]();
  testRenderer.render( /*#__PURE__*/_react["default"].createElement(EmptyState));
  return testRenderer._instance.state;
}

function wrapAct(fn) {
  if (!is168) {
    return fn();
  }

  var returnVal;

  _testUtils["default"].act(function () {
    returnVal = fn();
  });

  return returnVal;
}

function getProviderDefaultValue(Provider) {
  // React stores references to the Provider's defaultValue differently across versions.
  if ('_defaultValue' in Provider._context) {
    return Provider._context._defaultValue;
  }

  if ('_currentValue' in Provider._context) {
    return Provider._context._currentValue;
  }

  throw new Error('Enzyme Internal Error: can’t figure out how to get Provider’s default value');
}

function makeFakeElement(type) {
  return {
    $$typeof: _reactIs.Element,
    type: type
  };
}

function isStateful(Component) {
  return Component.prototype && (Component.prototype.isReactComponent || Array.isArray(Component.__reactAutoBindPairs) // fallback for createClass components
  );
}

var ReactSixteenAdapter = /*#__PURE__*/function (_EnzymeAdapter) {
  _inherits(ReactSixteenAdapter, _EnzymeAdapter);

  var _super3 = _createSuper(ReactSixteenAdapter);

  function ReactSixteenAdapter() {
    var _this;

    _classCallCheck(this, ReactSixteenAdapter);

    _this = _super3.call(this);
    var lifecycles = _this.options.lifecycles;
    _this.options = _objectSpread(_objectSpread({}, _this.options), {}, {
      enableComponentDidUpdateOnSetState: true,
      // TODO: remove, semver-major
      legacyContextMode: 'parent',
      lifecycles: _objectSpread(_objectSpread({}, lifecycles), {}, {
        componentDidUpdate: {
          onSetState: true
        },
        getDerivedStateFromProps: {
          hasShouldComponentUpdateBug: hasShouldComponentUpdateBug
        },
        getSnapshotBeforeUpdate: true,
        setState: {
          skipsComponentDidUpdateOnNullish: true
        },
        getChildContext: {
          calledByRenderer: false
        },
        getDerivedStateFromError: is166
      })
    });
    return _this;
  }

  _createClass(ReactSixteenAdapter, [{
    key: "createMountRenderer",
    value: function createMountRenderer(options) {
      (0, _enzymeAdapterUtils.assertDomAvailable)('mount');

      if ((0, _has["default"])(options, 'suspenseFallback')) {
        throw new TypeError('`suspenseFallback` is not supported by the `mount` renderer');
      }

      if (FiberTags === null) {
        // Requires DOM.
        FiberTags = (0, _detectFiberTags["default"])();
      }

      var attachTo = options.attachTo,
          hydrateIn = options.hydrateIn,
          wrappingComponentProps = options.wrappingComponentProps;
      var domNode = hydrateIn || attachTo || global.document.createElement('div');
      var instance = null;
      var adapter = this;
      return _objectSpread({
        render: function render(el, context, callback) {
          return wrapAct(function () {
            if (instance === null) {
              var type = el.type,
                  props = el.props,
                  ref = el.ref;

              var wrapperProps = _objectSpread({
                Component: type,
                props: props,
                wrappingComponentProps: wrappingComponentProps,
                context: context
              }, ref && {
                refProp: ref
              });

              var ReactWrapperComponent = (0, _enzymeAdapterUtils.createMountWrapper)(el, _objectSpread(_objectSpread({}, options), {}, {
                adapter: adapter
              }));

              var wrappedEl = /*#__PURE__*/_react["default"].createElement(ReactWrapperComponent, wrapperProps);

              instance = hydrateIn ? _reactDom["default"].hydrate(wrappedEl, domNode) : _reactDom["default"].render(wrappedEl, domNode);

              if (typeof callback === 'function') {
                callback();
              }
            } else {
              instance.setChildProps(el.props, context, callback);
            }
          });
        },
        unmount: function unmount() {
          _reactDom["default"].unmountComponentAtNode(domNode);

          instance = null;
        },
        getNode: function getNode() {
          if (!instance) {
            return null;
          }

          return (0, _enzymeAdapterUtils.getNodeFromRootFinder)(adapter.isCustomComponent, _toTree(instance._reactInternalFiber), options);
        },
        simulateError: function simulateError(nodeHierarchy, rootNode, error) {
          var isErrorBoundary = function isErrorBoundary(_ref2) {
            var elInstance = _ref2.instance,
                type = _ref2.type;

            if (is166 && type && type.getDerivedStateFromError) {
              return true;
            }

            return elInstance && elInstance.componentDidCatch;
          };

          var _ref3 = nodeHierarchy.find(isErrorBoundary) || {},
              catchingInstance = _ref3.instance,
              catchingType = _ref3.type;

          (0, _enzymeAdapterUtils.simulateError)(error, catchingInstance, rootNode, nodeHierarchy, nodeTypeFromType, adapter.displayNameOfNode, is166 ? catchingType : undefined);
        },
        simulateEvent: function simulateEvent(node, event, mock) {
          var mappedEvent = (0, _enzymeAdapterUtils.mapNativeEventNames)(event, eventOptions);
          var eventFn = _testUtils["default"].Simulate[mappedEvent];

          if (!eventFn) {
            throw new TypeError("ReactWrapper::simulate() event '".concat(event, "' does not exist"));
          }

          wrapAct(function () {
            eventFn(adapter.nodeToHostNode(node), mock);
          });
        },
        batchedUpdates: function batchedUpdates(fn) {
          return fn(); // return ReactDOM.unstable_batchedUpdates(fn);
        },
        getWrappingComponentRenderer: function getWrappingComponentRenderer() {
          return _objectSpread(_objectSpread({}, this), (0, _enzymeAdapterUtils.getWrappingComponentMountRenderer)({
            toTree: function toTree(inst) {
              return _toTree(inst._reactInternalFiber);
            },
            getMountWrapperInstance: function getMountWrapperInstance() {
              return instance;
            }
          }));
        }
      }, is168 && {
        wrapInvoke: wrapAct
      });
    }
  }, {
    key: "createShallowRenderer",
    value: function createShallowRenderer() {
      var _this2 = this;

      var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
      var adapter = this;
      var renderer = new _shallow["default"]();
      var suspenseFallback = options.suspenseFallback;

      if (typeof suspenseFallback !== 'undefined' && typeof suspenseFallback !== 'boolean') {
        throw TypeError('`options.suspenseFallback` should be boolean or undefined');
      }

      var isDOM = false;
      var cachedNode = null;
      var lastComponent = null;
      var wrappedComponent = null;
      var sentinel = {}; // wrap memo components with a PureComponent, or a class component with sCU

      var wrapPureComponent = function wrapPureComponent(Component, compare) {
        if (!is166) {
          throw new RangeError('this function should not be called in React < 16.6. Please report this!');
        }

        if (lastComponent !== Component) {
          if (isStateful(Component)) {
            wrappedComponent = /*#__PURE__*/function (_Component) {
              _inherits(wrappedComponent, _Component);

              var _super4 = _createSuper(wrappedComponent);

              function wrappedComponent() {
                _classCallCheck(this, wrappedComponent);

                return _super4.apply(this, arguments);
              }

              return wrappedComponent;
            }(Component); // eslint-disable-line react/prefer-stateless-function


            if (compare) {
              wrappedComponent.prototype.shouldComponentUpdate = function (nextProps) {
                return !compare(_this2.props, nextProps);
              };
            } else {
              wrappedComponent.prototype.isPureReactComponent = true;
            }
          } else {
            var memoized = sentinel;
            var prevProps;

            wrappedComponent = function wrappedComponent(props) {
              var shouldUpdate = memoized === sentinel || (compare ? !compare(prevProps, props) : !(0, _enzymeShallowEqual["default"])(prevProps, props));

              if (shouldUpdate) {
                for (var _len = arguments.length, args = new Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
                  args[_key - 1] = arguments[_key];
                }

                memoized = Component.apply(void 0, [_objectSpread(_objectSpread({}, Component.defaultProps), props)].concat(args));
                prevProps = props;
              }

              return memoized;
            };
          }

          (0, _object["default"])(wrappedComponent, Component, {
            displayName: adapter.displayNameOfNode({
              type: Component
            })
          });
          lastComponent = Component;
        }

        return wrappedComponent;
      }; // Wrap functional components on versions prior to 16.5,
      // to avoid inadvertently pass a `this` instance to it.


      var wrapFunctionalComponent = function wrapFunctionalComponent(Component) {
        if (is166 && (0, _has["default"])(Component, 'defaultProps')) {
          if (lastComponent !== Component) {
            wrappedComponent = (0, _object["default"])( // eslint-disable-next-line new-cap
            function (props) {
              for (var _len2 = arguments.length, args = new Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
                args[_key2 - 1] = arguments[_key2];
              }

              return Component.apply(void 0, [_objectSpread(_objectSpread({}, Component.defaultProps), props)].concat(args));
            }, Component, {
              displayName: adapter.displayNameOfNode({
                type: Component
              })
            });
            lastComponent = Component;
          }

          return wrappedComponent;
        }

        if (is165) {
          return Component;
        }

        if (lastComponent !== Component) {
          wrappedComponent = (0, _object["default"])(function () {
            return Component.apply(void 0, arguments);
          }, // eslint-disable-line new-cap
          Component);
          lastComponent = Component;
        }

        return wrappedComponent;
      };

      var renderElement = function renderElement(elConfig) {
        for (var _len3 = arguments.length, rest = new Array(_len3 > 1 ? _len3 - 1 : 0), _key3 = 1; _key3 < _len3; _key3++) {
          rest[_key3 - 1] = arguments[_key3];
        }

        var renderedEl = renderer.render.apply(renderer, [elConfig].concat(rest));
        var typeIsExisted = !!(renderedEl && renderedEl.type);

        if (is166 && typeIsExisted) {
          var clonedEl = transformSuspense(renderedEl, elConfig, {
            suspenseFallback: suspenseFallback
          });
          var elementIsChanged = clonedEl.type !== renderedEl.type;

          if (elementIsChanged) {
            return renderer.render.apply(renderer, [_objectSpread(_objectSpread({}, elConfig), {}, {
              type: clonedEl.type
            })].concat(rest));
          }
        }

        return renderedEl;
      };

      return {
        render: function render(el, unmaskedContext) {
          var _ref4 = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {},
              _ref4$providerValues = _ref4.providerValues,
              providerValues = _ref4$providerValues === void 0 ? new Map() : _ref4$providerValues;

          cachedNode = el;
          /* eslint consistent-return: 0 */

          if (typeof el.type === 'string') {
            isDOM = true;
          } else if ((0, _reactIs.isContextProvider)(el)) {
            providerValues.set(el.type, el.props.value);
            var MockProvider = (0, _object["default"])(function (props) {
              return props.children;
            }, el.type);
            return (0, _enzymeAdapterUtils.withSetStateAllowed)(function () {
              return renderElement(_objectSpread(_objectSpread({}, el), {}, {
                type: MockProvider
              }));
            });
          } else if ((0, _reactIs.isContextConsumer)(el)) {
            var Provider = adapter.getProviderFromConsumer(el.type);
            var value = providerValues.has(Provider) ? providerValues.get(Provider) : getProviderDefaultValue(Provider);
            var MockConsumer = (0, _object["default"])(function (props) {
              return props.children(value);
            }, el.type);
            return (0, _enzymeAdapterUtils.withSetStateAllowed)(function () {
              return renderElement(_objectSpread(_objectSpread({}, el), {}, {
                type: MockConsumer
              }));
            });
          } else {
            isDOM = false;
            var renderedEl = el;

            if (isLazy(renderedEl)) {
              throw TypeError('`React.lazy` is not supported by shallow rendering.');
            }

            renderedEl = transformSuspense(renderedEl, renderedEl, {
              suspenseFallback: suspenseFallback
            });
            var _renderedEl = renderedEl,
                Component = _renderedEl.type;
            var context = (0, _enzymeAdapterUtils.getMaskedContext)(Component.contextTypes, unmaskedContext);

            if (isMemo(el.type)) {
              var _el$type = el.type,
                  InnerComp = _el$type.type,
                  compare = _el$type.compare;
              return (0, _enzymeAdapterUtils.withSetStateAllowed)(function () {
                return renderElement(_objectSpread(_objectSpread({}, el), {}, {
                  type: wrapPureComponent(InnerComp, compare)
                }), context);
              });
            }

            var isComponentStateful = isStateful(Component);

            if (!isComponentStateful && typeof Component === 'function') {
              return (0, _enzymeAdapterUtils.withSetStateAllowed)(function () {
                return renderElement(_objectSpread(_objectSpread({}, renderedEl), {}, {
                  type: wrapFunctionalComponent(Component)
                }), context);
              });
            }

            if (isComponentStateful) {
              if (renderer._instance && el.props === renderer._instance.props && !(0, _enzymeShallowEqual["default"])(context, renderer._instance.context)) {
                var _spyMethod = (0, _enzymeAdapterUtils.spyMethod)(renderer, '_updateClassComponent', function (originalMethod) {
                  return function _updateClassComponent() {
                    var props = renderer._instance.props;

                    var clonedProps = _objectSpread({}, props);

                    renderer._instance.props = clonedProps;

                    for (var _len4 = arguments.length, args = new Array(_len4), _key4 = 0; _key4 < _len4; _key4++) {
                      args[_key4] = arguments[_key4];
                    }

                    var result = originalMethod.apply(renderer, args);
                    renderer._instance.props = props;
                    restore();
                    return result;
                  };
                }),
                    restore = _spyMethod.restore;
              } // fix react bug; see implementation of `getEmptyStateValue`


              var emptyStateValue = getEmptyStateValue();

              if (emptyStateValue) {
                Object.defineProperty(Component.prototype, 'state', {
                  configurable: true,
                  enumerable: true,
                  get: function get() {
                    return null;
                  },
                  set: function set(value) {
                    if (value !== emptyStateValue) {
                      Object.defineProperty(this, 'state', {
                        configurable: true,
                        enumerable: true,
                        value: value,
                        writable: true
                      });
                    }

                    return true;
                  }
                });
              }
            }

            return (0, _enzymeAdapterUtils.withSetStateAllowed)(function () {
              return renderElement(renderedEl, context);
            });
          }
        },
        unmount: function unmount() {
          renderer.unmount();
        },
        getNode: function getNode() {
          if (isDOM) {
            return elementToTree(cachedNode);
          }

          var output = renderer.getRenderOutput();
          return {
            nodeType: nodeTypeFromType(cachedNode.type),
            type: cachedNode.type,
            props: cachedNode.props,
            key: (0, _enzymeAdapterUtils.ensureKeyOrUndefined)(cachedNode.key),
            ref: cachedNode.ref,
            instance: renderer._instance,
            rendered: Array.isArray(output) ? flatten(output).map(function (el) {
              return elementToTree(el);
            }) : elementToTree(output)
          };
        },
        simulateError: function simulateError(nodeHierarchy, rootNode, error) {
          (0, _enzymeAdapterUtils.simulateError)(error, renderer._instance, cachedNode, nodeHierarchy.concat(cachedNode), nodeTypeFromType, adapter.displayNameOfNode, is166 ? cachedNode.type : undefined);
        },
        simulateEvent: function simulateEvent(node, event) {
          for (var _len5 = arguments.length, args = new Array(_len5 > 2 ? _len5 - 2 : 0), _key5 = 2; _key5 < _len5; _key5++) {
            args[_key5 - 2] = arguments[_key5];
          }

          var handler = node.props[(0, _enzymeAdapterUtils.propFromEvent)(event, eventOptions)];

          if (handler) {
            (0, _enzymeAdapterUtils.withSetStateAllowed)(function () {
              // TODO(lmr): create/use synthetic events
              // TODO(lmr): emulate React's event propagation
              // ReactDOM.unstable_batchedUpdates(() => {
              handler.apply(void 0, args); // });
            });
          }
        },
        batchedUpdates: function batchedUpdates(fn) {
          return fn(); // return ReactDOM.unstable_batchedUpdates(fn);
        },
        checkPropTypes: function checkPropTypes(typeSpecs, values, location, hierarchy) {
          return (0, _checkPropTypes2["default"])(typeSpecs, values, location, (0, _enzymeAdapterUtils.displayNameOfNode)(cachedNode), function () {
            return (0, _enzymeAdapterUtils.getComponentStack)(hierarchy.concat([cachedNode]));
          });
        }
      };
    }
  }, {
    key: "createStringRenderer",
    value: function createStringRenderer(options) {
      if ((0, _has["default"])(options, 'suspenseFallback')) {
        throw new TypeError('`suspenseFallback` should not be specified in options of string renderer');
      }

      return {
        render: function render(el, context) {
          if (options.context && (el.type.contextTypes || options.childContextTypes)) {
            var childContextTypes = _objectSpread(_objectSpread({}, el.type.contextTypes || {}), options.childContextTypes);

            var ContextWrapper = (0, _enzymeAdapterUtils.createRenderWrapper)(el, context, childContextTypes);
            return _server["default"].renderToStaticMarkup( /*#__PURE__*/_react["default"].createElement(ContextWrapper));
          }

          return _server["default"].renderToStaticMarkup(el);
        }
      };
    } // Provided a bag of options, return an `EnzymeRenderer`. Some options can be implementation
    // specific, like `attach` etc. for React, but not part of this interface explicitly.
    // eslint-disable-next-line class-methods-use-this

  }, {
    key: "createRenderer",
    value: function createRenderer(options) {
      switch (options.mode) {
        case _enzyme.EnzymeAdapter.MODES.MOUNT:
          return this.createMountRenderer(options);

        case _enzyme.EnzymeAdapter.MODES.SHALLOW:
          return this.createShallowRenderer(options);

        case _enzyme.EnzymeAdapter.MODES.STRING:
          return this.createStringRenderer(options);

        default:
          throw new Error("Enzyme Internal Error: Unrecognized mode: ".concat(options.mode));
      }
    }
  }, {
    key: "wrap",
    value: function wrap(element) {
      return (0, _enzymeAdapterUtils.wrap)(element);
    } // converts an RSTNode to the corresponding JSX Pragma Element. This will be needed
    // in order to implement the `Wrapper.mount()` and `Wrapper.shallow()` methods, but should
    // be pretty straightforward for people to implement.
    // eslint-disable-next-line class-methods-use-this

  }, {
    key: "nodeToElement",
    value: function nodeToElement(node) {
      if (!node || _typeof(node) !== 'object') return null;
      var type = node.type;
      return /*#__PURE__*/_react["default"].createElement(unmemoType(type), (0, _enzymeAdapterUtils.propsWithKeysAndRef)(node));
    } // eslint-disable-next-line class-methods-use-this

  }, {
    key: "matchesElementType",
    value: function matchesElementType(node, matchingType) {
      if (!node) {
        return node;
      }

      var type = node.type;
      return unmemoType(type) === unmemoType(matchingType);
    }
  }, {
    key: "elementToNode",
    value: function elementToNode(element) {
      return elementToTree(element);
    }
  }, {
    key: "nodeToHostNode",
    value: function nodeToHostNode(node) {
      var supportsArray = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

      var nodes = _nodeToHostNode(node);

      if (Array.isArray(nodes) && !supportsArray) {
        return nodes[0];
      }

      return nodes;
    }
  }, {
    key: "displayNameOfNode",
    value: function displayNameOfNode(node) {
      if (!node) return null;
      var type = node.type,
          $$typeof = node.$$typeof;
      var adapter = this;
      var nodeType = type || $$typeof; // newer node types may be undefined, so only test if the nodeType exists

      if (nodeType) {
        switch (nodeType) {
          case (is166 ? _reactIs.ConcurrentMode : _reactIs.AsyncMode) || NaN:
            return is166 ? 'ConcurrentMode' : 'AsyncMode';

          case _reactIs.Fragment || NaN:
            return 'Fragment';

          case _reactIs.StrictMode || NaN:
            return 'StrictMode';

          case _reactIs.Profiler || NaN:
            return 'Profiler';

          case _reactIs.Portal || NaN:
            return 'Portal';

          case _reactIs.Suspense || NaN:
            return 'Suspense';

          default:
        }
      }

      var $$typeofType = type && type.$$typeof;

      switch ($$typeofType) {
        case _reactIs.ContextConsumer || NaN:
          return 'ContextConsumer';

        case _reactIs.ContextProvider || NaN:
          return 'ContextProvider';

        case _reactIs.Memo || NaN:
          {
            var nodeName = (0, _enzymeAdapterUtils.displayNameOfNode)(node);
            return typeof nodeName === 'string' ? nodeName : "Memo(".concat(adapter.displayNameOfNode(type), ")");
          }

        case _reactIs.ForwardRef || NaN:
          {
            if (type.displayName) {
              return type.displayName;
            }

            var name = adapter.displayNameOfNode({
              type: type.render
            });
            return name ? "ForwardRef(".concat(name, ")") : 'ForwardRef';
          }

        case _reactIs.Lazy || NaN:
          {
            return 'lazy';
          }

        default:
          return (0, _enzymeAdapterUtils.displayNameOfNode)(node);
      }
    }
  }, {
    key: "isValidElement",
    value: function isValidElement(element) {
      return (0, _reactIs.isElement)(element);
    }
  }, {
    key: "isValidElementType",
    value: function isValidElementType(object) {
      return !!object && (0, _reactIs.isValidElementType)(object);
    }
  }, {
    key: "isFragment",
    value: function isFragment(fragment) {
      return (0, _Utils.typeOfNode)(fragment) === _reactIs.Fragment;
    }
  }, {
    key: "isCustomComponent",
    value: function isCustomComponent(type) {
      var fakeElement = makeFakeElement(type);
      return !!type && (typeof type === 'function' || (0, _reactIs.isForwardRef)(fakeElement) || (0, _reactIs.isContextProvider)(fakeElement) || (0, _reactIs.isContextConsumer)(fakeElement) || (0, _reactIs.isSuspense)(fakeElement));
    }
  }, {
    key: "isContextConsumer",
    value: function isContextConsumer(type) {
      return !!type && (0, _reactIs.isContextConsumer)(makeFakeElement(type));
    }
  }, {
    key: "isCustomComponentElement",
    value: function isCustomComponentElement(inst) {
      if (!inst || !this.isValidElement(inst)) {
        return false;
      }

      return this.isCustomComponent(inst.type);
    }
  }, {
    key: "getProviderFromConsumer",
    value: function getProviderFromConsumer(Consumer) {
      // React stores references to the Provider on a Consumer differently across versions.
      if (Consumer) {
        var Provider;

        if (Consumer._context) {
          // check this first, to avoid a deprecation warning
          Provider = Consumer._context.Provider;
        } else if (Consumer.Provider) {
          Provider = Consumer.Provider;
        }

        if (Provider) {
          return Provider;
        }
      }

      throw new Error('Enzyme Internal Error: can’t figure out how to get Provider from Consumer');
    }
  }, {
    key: "createElement",
    value: function createElement() {
      return /*#__PURE__*/_react["default"].createElement.apply(_react["default"], arguments);
    }
  }, {
    key: "wrapWithWrappingComponent",
    value: function wrapWithWrappingComponent(node, options) {
      return {
        RootFinder: _enzymeAdapterUtils.RootFinder,
        node: (0, _enzymeAdapterUtils.wrapWithWrappingComponent)(_react["default"].createElement, node, options)
      };
    }
  }]);

  return ReactSixteenAdapter;
}(_enzyme.EnzymeAdapter);

module.exports = ReactSixteenAdapter;
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9SZWFjdFNpeHRlZW5BZGFwdGVyLmpzIl0sIm5hbWVzIjpbImlzMTY0IiwiVGVzdFV0aWxzIiwiU2ltdWxhdGUiLCJ0b3VjaFN0YXJ0IiwiaXMxNjUiLCJhdXhDbGljayIsImlzMTY2IiwiUmVhY3QiLCJ1bnN0YWJsZV9Bc3luY01vZGUiLCJpczE2OCIsImFjdCIsImhhc1Nob3VsZENvbXBvbmVudFVwZGF0ZUJ1ZyIsInNlbXZlciIsInNhdGlzZmllcyIsInRlc3RSZW5kZXJlclZlcnNpb24iLCJGaWJlclRhZ3MiLCJub2RlQW5kU2libGluZ3NBcnJheSIsIm5vZGVXaXRoU2libGluZyIsImFycmF5Iiwibm9kZSIsInB1c2giLCJzaWJsaW5nIiwiZmxhdHRlbiIsImFyciIsInJlc3VsdCIsInN0YWNrIiwiaSIsImxlbmd0aCIsIm4iLCJwb3AiLCJlbCIsIkFycmF5IiwiaXNBcnJheSIsIm5vZGVUeXBlRnJvbVR5cGUiLCJ0eXBlIiwiUG9ydGFsIiwiaXNNZW1vIiwiTWVtbyIsImlzTGF6eSIsIkxhenkiLCJ1bm1lbW9UeXBlIiwidHJhbnNmb3JtU3VzcGVuc2UiLCJyZW5kZXJlZEVsIiwicHJlcmVuZGVyRWwiLCJzdXNwZW5zZUZhbGxiYWNrIiwiY2hpbGRyZW4iLCJwcm9wcyIsImZhbGxiYWNrIiwicmVwbGFjZUxhenlXaXRoRmFsbGJhY2siLCJwcm9wVHlwZXMiLCJkZWZhdWx0UHJvcHMiLCJjb250ZXh0VHlwZXMiLCJjb250ZXh0VHlwZSIsImNoaWxkQ29udGV4dFR5cGVzIiwiRmFrZVN1c3BlbnNlIiwiaXNTdGF0ZWZ1bCIsImNyZWF0ZUVsZW1lbnQiLCJlbGVtZW50VG9UcmVlIiwiY29udGFpbmVySW5mbyIsIm5vZGVUeXBlIiwia2V5IiwicmVmIiwiaW5zdGFuY2UiLCJyZW5kZXJlZCIsInRvVHJlZSIsInZub2RlIiwidGFnIiwiSG9zdFJvb3QiLCJjaGlsZHJlblRvVHJlZSIsImNoaWxkIiwiSG9zdFBvcnRhbCIsInN0YXRlTm9kZSIsIm1lbW9pemVkUHJvcHMiLCJDbGFzc0NvbXBvbmVudCIsIkZ1bmN0aW9uYWxDb21wb25lbnQiLCJNZW1vQ2xhc3MiLCJlbGVtZW50VHlwZSIsIk1lbW9TRkMiLCJyZW5kZXJlZE5vZGVzIiwibWFwIiwiSG9zdENvbXBvbmVudCIsIkhvc3RUZXh0IiwiRnJhZ21lbnQiLCJNb2RlIiwiQ29udGV4dFByb3ZpZGVyIiwiQ29udGV4dENvbnN1bWVyIiwiUHJvZmlsZXIiLCJGb3J3YXJkUmVmIiwicGVuZGluZ1Byb3BzIiwiU3VzcGVuc2UiLCJFcnJvciIsIm5vZGVUb0hvc3ROb2RlIiwiX25vZGUiLCJtYXBwZXIiLCJpdGVtIiwiUmVhY3RET00iLCJmaW5kRE9NTm9kZSIsImV2ZW50T3B0aW9ucyIsImFuaW1hdGlvbiIsInBvaW50ZXJFdmVudHMiLCJnZXRFbXB0eVN0YXRlVmFsdWUiLCJFbXB0eVN0YXRlIiwiQ29tcG9uZW50IiwidGVzdFJlbmRlcmVyIiwiU2hhbGxvd1JlbmRlcmVyIiwicmVuZGVyIiwiX2luc3RhbmNlIiwic3RhdGUiLCJ3cmFwQWN0IiwiZm4iLCJyZXR1cm5WYWwiLCJnZXRQcm92aWRlckRlZmF1bHRWYWx1ZSIsIlByb3ZpZGVyIiwiX2NvbnRleHQiLCJfZGVmYXVsdFZhbHVlIiwiX2N1cnJlbnRWYWx1ZSIsIm1ha2VGYWtlRWxlbWVudCIsIiQkdHlwZW9mIiwiRWxlbWVudCIsInByb3RvdHlwZSIsImlzUmVhY3RDb21wb25lbnQiLCJfX3JlYWN0QXV0b0JpbmRQYWlycyIsIlJlYWN0U2l4dGVlbkFkYXB0ZXIiLCJsaWZlY3ljbGVzIiwib3B0aW9ucyIsImVuYWJsZUNvbXBvbmVudERpZFVwZGF0ZU9uU2V0U3RhdGUiLCJsZWdhY3lDb250ZXh0TW9kZSIsImNvbXBvbmVudERpZFVwZGF0ZSIsIm9uU2V0U3RhdGUiLCJnZXREZXJpdmVkU3RhdGVGcm9tUHJvcHMiLCJnZXRTbmFwc2hvdEJlZm9yZVVwZGF0ZSIsInNldFN0YXRlIiwic2tpcHNDb21wb25lbnREaWRVcGRhdGVPbk51bGxpc2giLCJnZXRDaGlsZENvbnRleHQiLCJjYWxsZWRCeVJlbmRlcmVyIiwiZ2V0RGVyaXZlZFN0YXRlRnJvbUVycm9yIiwiVHlwZUVycm9yIiwiYXR0YWNoVG8iLCJoeWRyYXRlSW4iLCJ3cmFwcGluZ0NvbXBvbmVudFByb3BzIiwiZG9tTm9kZSIsImdsb2JhbCIsImRvY3VtZW50IiwiYWRhcHRlciIsImNvbnRleHQiLCJjYWxsYmFjayIsIndyYXBwZXJQcm9wcyIsInJlZlByb3AiLCJSZWFjdFdyYXBwZXJDb21wb25lbnQiLCJ3cmFwcGVkRWwiLCJoeWRyYXRlIiwic2V0Q2hpbGRQcm9wcyIsInVubW91bnQiLCJ1bm1vdW50Q29tcG9uZW50QXROb2RlIiwiZ2V0Tm9kZSIsImlzQ3VzdG9tQ29tcG9uZW50IiwiX3JlYWN0SW50ZXJuYWxGaWJlciIsInNpbXVsYXRlRXJyb3IiLCJub2RlSGllcmFyY2h5Iiwicm9vdE5vZGUiLCJlcnJvciIsImlzRXJyb3JCb3VuZGFyeSIsImVsSW5zdGFuY2UiLCJjb21wb25lbnREaWRDYXRjaCIsImZpbmQiLCJjYXRjaGluZ0luc3RhbmNlIiwiY2F0Y2hpbmdUeXBlIiwiZGlzcGxheU5hbWVPZk5vZGUiLCJ1bmRlZmluZWQiLCJzaW11bGF0ZUV2ZW50IiwiZXZlbnQiLCJtb2NrIiwibWFwcGVkRXZlbnQiLCJldmVudEZuIiwiYmF0Y2hlZFVwZGF0ZXMiLCJnZXRXcmFwcGluZ0NvbXBvbmVudFJlbmRlcmVyIiwiaW5zdCIsImdldE1vdW50V3JhcHBlckluc3RhbmNlIiwid3JhcEludm9rZSIsInJlbmRlcmVyIiwiaXNET00iLCJjYWNoZWROb2RlIiwibGFzdENvbXBvbmVudCIsIndyYXBwZWRDb21wb25lbnQiLCJzZW50aW5lbCIsIndyYXBQdXJlQ29tcG9uZW50IiwiY29tcGFyZSIsIlJhbmdlRXJyb3IiLCJzaG91bGRDb21wb25lbnRVcGRhdGUiLCJuZXh0UHJvcHMiLCJpc1B1cmVSZWFjdENvbXBvbmVudCIsIm1lbW9pemVkIiwicHJldlByb3BzIiwic2hvdWxkVXBkYXRlIiwiYXJncyIsImRpc3BsYXlOYW1lIiwid3JhcEZ1bmN0aW9uYWxDb21wb25lbnQiLCJyZW5kZXJFbGVtZW50IiwiZWxDb25maWciLCJyZXN0IiwidHlwZUlzRXhpc3RlZCIsImNsb25lZEVsIiwiZWxlbWVudElzQ2hhbmdlZCIsInVubWFza2VkQ29udGV4dCIsInByb3ZpZGVyVmFsdWVzIiwiTWFwIiwic2V0IiwidmFsdWUiLCJNb2NrUHJvdmlkZXIiLCJnZXRQcm92aWRlckZyb21Db25zdW1lciIsImhhcyIsImdldCIsIk1vY2tDb25zdW1lciIsIklubmVyQ29tcCIsImlzQ29tcG9uZW50U3RhdGVmdWwiLCJvcmlnaW5hbE1ldGhvZCIsIl91cGRhdGVDbGFzc0NvbXBvbmVudCIsImNsb25lZFByb3BzIiwiYXBwbHkiLCJyZXN0b3JlIiwiZW1wdHlTdGF0ZVZhbHVlIiwiT2JqZWN0IiwiZGVmaW5lUHJvcGVydHkiLCJjb25maWd1cmFibGUiLCJlbnVtZXJhYmxlIiwid3JpdGFibGUiLCJvdXRwdXQiLCJnZXRSZW5kZXJPdXRwdXQiLCJjb25jYXQiLCJoYW5kbGVyIiwiY2hlY2tQcm9wVHlwZXMiLCJ0eXBlU3BlY3MiLCJ2YWx1ZXMiLCJsb2NhdGlvbiIsImhpZXJhcmNoeSIsIkNvbnRleHRXcmFwcGVyIiwiUmVhY3RET01TZXJ2ZXIiLCJyZW5kZXJUb1N0YXRpY01hcmt1cCIsIm1vZGUiLCJFbnp5bWVBZGFwdGVyIiwiTU9ERVMiLCJNT1VOVCIsImNyZWF0ZU1vdW50UmVuZGVyZXIiLCJTSEFMTE9XIiwiY3JlYXRlU2hhbGxvd1JlbmRlcmVyIiwiU1RSSU5HIiwiY3JlYXRlU3RyaW5nUmVuZGVyZXIiLCJlbGVtZW50IiwibWF0Y2hpbmdUeXBlIiwic3VwcG9ydHNBcnJheSIsIm5vZGVzIiwiQ29uY3VycmVudE1vZGUiLCJBc3luY01vZGUiLCJOYU4iLCJTdHJpY3RNb2RlIiwiJCR0eXBlb2ZUeXBlIiwibm9kZU5hbWUiLCJuYW1lIiwib2JqZWN0IiwiZnJhZ21lbnQiLCJmYWtlRWxlbWVudCIs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QWM7QUFDWixZQUFJdEosUUFBSjs7QUFDQSxZQUFJc0osUUFBUSxDQUFDckosUUFBYixFQUF1QjtBQUFFO0FBQ3BCRCxVQUFBQSxRQURrQixHQUNMc0osUUFBUSxDQUFDckosUUFESixDQUNsQkQsUUFEa0I7QUFFdEIsU0FGRCxNQUVPLElBQUlzSixRQUFRLENBQUN0SixRQUFiLEVBQXVCO0FBQ3pCQSxVQUFBQSxRQUR5QixHQUNac0osUUFEWSxDQUN6QnRKLFFBRHlCO0FBRTdCOztBQUNELFlBQUlBLFFBQUosRUFBYztBQUNaLGlCQUFPQSxRQUFQO0FBQ0Q7QUFDRjs7QUFDRCxZQUFNLElBQUl0QixLQUFKLENBQVUsMkVBQVYsQ0FBTjtBQUNEOzs7b0NBRXNCO0FBQ3JCLDBCQUFPbkYsa0JBQU1pRCxhQUFOLG9DQUFQO0FBQ0Q7Ozs4Q0FFeUJyQyxJLEVBQU15RyxPLEVBQVM7QUFDdkMsYUFBTztBQUNMMkksUUFBQUEsVUFBVSxFQUFWQSw4QkFESztBQUVMcFAsUUFBQUEsSUFBSSxFQUFFLG1EQUEwQlosa0JBQU1pRCxhQUFoQyxFQUErQ3JDLElBQS9DLEVBQXFEeUcsT0FBckQ7QUFGRCxPQUFQO0FBSUQ7Ozs7RUFoakIrQm1ILHFCOztBQW1qQmxDeUIsTUFBTSxDQUFDQyxPQUFQLEdBQWlCL0ksbUJBQWpCIiwic291cmNlc0NvbnRlbnQiOlsiLyogZXNsaW50IG5vLXVzZS1iZWZvcmUtZGVmaW5lOiAwICovXG5pbXBvcnQgUmVhY3QgZnJvbSAncmVhY3QnO1xuaW1wb3J0IFJlYWN0RE9NIGZyb20gJ3JlYWN0LWRvbSc7XG4vLyBlc2xpbnQtZGlzYWJsZS1uZXh0LWxpbmUgaW1wb3J0L25vLXVucmVzb2x2ZWRcbmltcG9ydCBSZWFjdERPTVNlcnZlciBmcm9tICdyZWFjdC1kb20vc2VydmVyJztcbi8vIGVzbGludC1kaXNhYmxlLW5leHQtbGluZSBpbXBvcnQvbm8tdW5yZXNvbHZlZFxuaW1wb3J0IFNoYWxsb3dSZW5kZXJlciBmcm9tICdyZWFjdC10ZXN0LXJlbmRlcmVyL3NoYWxsb3cnO1xuaW1wb3J0IHsgdmVyc2lvbiBhcyB0ZXN0UmVuZGVyZXJWZXJzaW9uIH0gZnJvbSAncmVhY3QtdGVzdC1yZW5kZXJlci9wYWNrYWdlLmpzb24nO1xuLy8gZXNsaW50LWRpc2FibGUtbmV4dC1saW5lIGltcG9ydC9uby11bnJlc29sdmVkXG5pbXBvcnQgVGVzdFV0aWxzIGZyb20gJ3JlYWN0LWRvbS90ZXN0LXV0aWxzJztcbmltcG9ydCBzZW12ZXIgZnJvbSAnc2VtdmVyJztcbmltcG9ydCBjaGVja1Byb3BUeXBlcyBmcm9tICdwcm9wLXR5cGVzL2NoZWNrUHJvcFR5cGVzJztcbmltcG9ydCBoYXMgZnJvbSAnaGFzJztcbmltcG9ydCB7XG4gIEFzeW5jTW9kZSxcbiAgQ29uY3VycmVudE1vZGUsXG4gIENvbnRleHRDb25zdW1lcixcbiAgQ29udGV4dFByb3ZpZGVyLFxuICBFbGVtZW50LFxuICBGb3J3YXJkUmVmLFxuICBGcmFnbWVudCxcbiAgaXNDb250ZXh0Q29uc3VtZXIsXG4gIGlzQ29udGV4dFByb3ZpZGVyLFxuICBpc0VsZW1lbnQsXG4gIGlzRm9yd2FyZFJlZixcbiAgaXNQb3J0YWwsXG4gIGlzU3VzcGVuc2UsXG4gIGlzVmFsaWRFbGVtZW50VHlwZSxcbiAgTGF6eSxcbiAgTWVtbyxcbiAgUG9ydGFsLFxuICBQcm9maWxlcixcbiAgU3RyaWN0TW9kZSxcbiAgU3VzcGVuc2UsXG59IGZyb20gJ3JlYWN0LWlzJztcbmltcG9ydCB7IEVuenltZUFkYXB0ZXIgfSBmcm9tICdlbnp5bWUnO1xuaW1wb3J0IHsgdHlwZU9mTm9kZSB9IGZyb20gJ2VuenltZS9idWlsZC9VdGlscyc7XG5pbXBvcnQgc2hhbGxvd0VxdWFsIGZyb20gJ2VuenltZS1zaGFsbG93LWVxdWFsJztcbmltcG9ydCB7XG4gIGRpc3BsYXlOYW1lT2ZOb2RlLFxuICBlbGVtZW50VG9UcmVlIGFzIHV0aWxFbGVtZW50VG9UcmVlLFxuICBub2RlVHlwZUZyb21UeXBlIGFzIHV0aWxOb2RlVHlwZUZyb21UeXBlLFxuICBtYXBOYXRpdmVFdmVudE5hbWVzLFxuICBwcm9wRnJvbUV2ZW50LFxuICBhc3NlcnREb21BdmFpbGFibGUsXG4gIHdpdGhTZXRTdGF0ZUFsbG93ZWQsXG4gIGNyZWF0ZVJlbmRlcldyYXBwZXIsXG4gIGNyZWF0ZU1vdW50V3JhcHBlcixcbiAgcHJvcHNXaXRoS2V5c0FuZFJlZixcbiAgZW5zdXJlS2V5T3JVbmRlZmluZWQsXG4gIHNpbXVsYXRlRXJyb3IsXG4gIHdyYXAsXG4gIGdldE1hc2tlZENvbnRleHQsXG4gIGdldENvbXBvbmVudFN0YWNrLFxuICBSb290RmluZGVyLFxuICBnZXROb2RlRnJvbVJvb3RGaW5kZXIsXG4gIHdyYXBXaXRoV3JhcHBpbmdDb21wb25lbnQsXG4gIGdldFdyYXBwaW5nQ29tcG9uZW50TW91bnRSZW5kZXJlcixcbiAgY29tcGFyZU5vZGVUeXBlT2YsXG4gIHNweU1ldGhvZCxcbn0gZnJvbSAnZW56eW1lLWFkYXB0ZXItdXRpbHMnO1xuaW1wb3J0IGZpbmRDdXJyZW50RmliZXJVc2luZ1Nsb3dQYXRoIGZyb20gJy4vZmluZEN1cnJlbnRGaWJlclVzaW5nU2xvd1BhdGgnO1xuaW1wb3J0IGRldGVjdEZpYmVyVGFncyBmcm9tICcuL2RldGVjdEZpYmVyVGFncyc7XG5cbmNvbnN0IGlzMTY0ID0gISFUZXN0VXRpbHMuU2ltdWxhdGUudG91Y2hTdGFydDsgLy8gMTYuNCtcbmNvbnN0IGlzMTY1ID0gISFUZXN0VXRpbHMuU2ltdWxhdGUuYXV4Q2xpY2s7IC8vIDE2LjUrXG5jb25zdCBpczE2NiA9IGlzMTY1ICYmICFSZWFjdC51bnN0YWJsZV9Bc3luY01vZGU7IC8vIDE2LjYrXG5jb25zdCBpczE2OCA9IGlzMTY2ICYmIHR5cGVvZiBUZXN0VXRpbHMuYWN0ID09PSAnZnVuY3Rpb24nO1xuXG5jb25zdCBoYXNTaG91bGRDb21wb25lbnRVcGRhdGVCdWcgPSBzZW12ZXIuc2F0aXNmaWVzKHRlc3RSZW5kZXJlclZlcnNpb24sICc8IDE2LjgnKTtcblxuLy8gTGF6aWx5IHBvcHVsYXRlZCBpZiBET00gaXMgYXZhaWxhYmxlLlxubGV0IEZpYmVyVGFncyA9IG51bGw7XG5cbmZ1bmN0aW9uIG5vZGVBbmRTaWJsaW5nc0FycmF5KG5vZGVXaXRoU2libGluZykge1xuICBjb25zdCBhcnJheSA9IFtdO1xuICBsZXQgbm9kZSA9IG5vZGVXaXRoU2libGluZztcbiAgd2hpbGUgKG5vZGUgIT0gbnVsbCkge1xuICAgIGFycmF5LnB1c2gobm9kZSk7XG4gICAgbm9kZSA9IG5vZGUuc2libGluZztcbiAgfVxuICByZXR1cm4gYXJyYXk7XG59XG5cbmZ1bmN0aW9uIGZsYXR0ZW4oYXJyKSB7XG4gIGNvbnN0IHJlc3VsdCA9IFtdO1xuICBjb25zdCBzdGFjayA9IFt7IGk6IDAsIGFycmF5OiBhcnIgfV07XG4gIHdoaWxlIChzdGFjay5sZW5ndGgpIHtcbiAgICBjb25zdCBuID0gc3RhY2sucG9wKCk7XG4gICAgd2hpbGUgKG4uaSA8IG4uYXJyYXkubGVuZ3RoKSB7XG4gICAgICBjb25zdCBlbCA9IG4uYXJyYXlbbi5pXTtcbiAgICAgIG4uaSArPSAxO1xuICAgICAgaWYgKEFycmF5LmlzQXJyYXkoZWwpKSB7XG4gICAgICAgIHN0YWNrLnB1c2gobik7XG4gICAgICAgIHN0YWNrLnB1c2goeyBpOiAwLCBhcnJheTogZWwgfSk7XG4gICAgICAgIGJyZWFrO1xuICAgICAgfVxuICAgICAgcmVzdWx0LnB1c2goZWwpO1xuICAgIH1cbiAgfVxuICByZXR1cm4gcmVzdWx0O1xufVxuXG5mdW5jdGlvbiBub2RlVHlwZUZyb21UeXBlKHR5cGUpIHtcbiAgaWYgKHR5cGUgPT09IFBvcnRhbCkge1xuICAgIHJldHVybiAncG9ydGFsJztcbiAgfVxuXG4gIHJldHVybiB1dGlsTm9kZVR5cGVGcm9tVHlwZSh0eXBlKTtcbn1cblxuZnVuY3Rpb24gaXNNZW1vKHR5cGUpIHtcbiAgcmV0dXJuIGNvbXBhcmVOb2RlVHlwZU9mKHR5cGUsIE1lbW8pO1xufVxuXG5mdW5jdGlvbiBpc0xhenkodHlwZSkge1xuICByZXR1cm4gY29tcGFyZU5vZGVUeXBlT2YodHlwZSwgTGF6eSk7XG59XG5cbmZ1bmN0aW9uIHVubWVtb1R5cGUodHlwZSkge1xuICByZXR1cm4gaXNNZW1vKHR5cGUpID8gdHlwZS50eXBlIDogdHlwZTtcbn1cblxuZnVuY3Rpb24gdHJhbnNmb3JtU3VzcGVuc2UocmVuZGVyZWRFbCwgcHJlcmVuZGVyRWwsIHsgc3VzcGVuc2VGYWxsYmFjayB9KSB7XG4gIGlmICghaXNTdXNwZW5zZShyZW5kZXJlZEVsKSkge1xuICAgIHJldHVybiByZW5kZXJlZEVsO1xuICB9XG5cbiAgbGV0IHsgY2hpbGRyZW4gfSA9IHJlbmRlcmVkRWwucHJvcHM7XG5cbiAgaWYgKHN1c3BlbnNlRmFsbGJhY2spIHtcbiAgICBjb25zdCB7IGZhbGxiYWNrIH0gPSByZW5kZXJlZEVsLnByb3BzO1xuICAgIGNoaWxkcmVuID0gcmVwbGFjZUxhenlXaXRoRmFsbGJhY2soY2hpbGRyZW4sIGZhbGxiYWNrKTtcbiAgfVxuXG4gIGNvbnN0IHtcbiAgICBwcm9wVHlwZXMsXG4gICAgZGVmYXVsdFByb3BzLFxuICAgIGNvbnRleHRUeXBlcyxcbiAgICBjb250ZXh0VHlwZSxcbiAgICBjaGlsZENvbnRleHRUeXBlcyxcbiAgfSA9IHJlbmRlcmVkRWwudHlwZTtcblxuICBjb25zdCBGYWtlU3VzcGVuc2UgPSBPYmplY3QuYXNzaWduKFxuICAgIGlzU3RhdGVmdWwocHJlcmVuZGVyRWwudHlwZSlcbiAgICAgID8gY2xhc3MgRmFrZVN1c3BlbnNlIGV4dGVuZHMgcHJlcmVuZGVyRWwudHlwZSB7XG4gICAgICAgIHJlbmRlcigpIHtcbiAgICAgICAgICBjb25zdCB7IHR5cGUsIHByb3BzIH0gPSBwcmVyZW5kZXJFbDtcbiAgICAgICAgICByZXR1cm4gUmVhY3QuY3JlYXRlRWxlbWVudChcbiAgICAgICAgICAgIHR5cGUsXG4gICAgICAgICAgICB7IC4uLnByb3BzLCAuLi50aGlzLnByb3BzIH0sXG4gICAgICAgICAgICBjaGlsZHJlbixcbiAgICAgICAgICApO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgICA6IGZ1bmN0aW9uIEZha2VTdXNwZW5zZShwcm9wcykgeyAvLyBlc2xpbnQtZGlzYWJsZS1saW5lIHByZWZlci1hcnJvdy1jYWxsYmFja1xuICAgICAgICByZXR1cm4gUmVhY3QuY3JlYXRlRWxlbWVudChcbiAgICAgICAgICByZW5kZXJlZEVsLnR5cGUsXG4gICAgICAgICAgeyAuLi5yZW5kZXJlZEVsLnByb3BzLCAuLi5wcm9wcyB9LFxuICAgICAgICAgIGNoaWxkcmVuLFxuICAgICAgICApO1xuICAgICAgfSxcbiAgICB7XG4gICAgICBwcm9wVHlwZXMsXG4gICAgICBkZWZhdWx0UHJvcHMsXG4gICAgICBjb250ZXh0VHlwZXMsXG4gICAgICBjb250ZXh0VHlwZSxcbiAgICAgIGNoaWxkQ29udGV4dFR5cGVzLFxuICAgIH0sXG4gICk7XG4gIHJldHVybiBSZWFjdC5jcmVhdGVFbGVtZW50KEZha2VTdXNwZW5zZSwgbnVsbCwgY2hpbGRyZW4pO1xufVxuXG5mdW5jdGlvbiBlbGVtZW50VG9UcmVlKGVsKSB7XG4gIGlmICghaXNQb3J0YWwoZWwpKSB7XG4gICAgcmV0dXJuIHV0aWxFbGVtZW50VG9UcmVlKGVsLCBlbGVtZW50VG9UcmVlKTtcbiAgfVxuXG4gIGNvbnN0IHsgY2hpbGRyZW4sIGNvbnRhaW5lckluZm8gfSA9IGVsO1xuICBjb25zdCBwcm9wcyA9IHsgY2hpbGRyZW4sIGNvbnRhaW5lckluZm8gfTtcblxuICByZXR1cm4ge1xuICAgIG5vZGVUeXBlOiAncG9ydGFsJyxcbiAgICB0eXBlOiBQb3J0YWwsXG4gICAgcHJvcHMsXG4gICAga2V5OiBlbnN1cmVLZXlPclVuZGVmaW5lZChlbC5rZXkpLFxuICAgIHJlZjogZWwucmVmIHx8IG51bGwsXG4gICAgaW5zdGFuY2U6IG51bGwsXG4gICAgcmVuZGVyZWQ6IGVsZW1lbnRUb1RyZWUoZWwuY2hpbGRyZW4pLFxuICB9O1xufVxuXG5mdW5jdGlvbiB0b1RyZWUodm5vZGUpIHtcbiAgaWYgKHZub2RlID09IG51bGwpIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuICAvLyBUT0RPKGxtcik6IEknbSBub3QgcmVhbGx5IHN1cmUgSSB1bmRlcnN0YW5kIHdoZXRoZXIgb3Igbm90IHRoaXMgaXMgd2hhdFxuICAvLyBpIHNob3VsZCBiZSBkb2luZywgb3IgaWYgdGhpcyBpcyBhIGhhY2sgZm9yIHNvbWV0aGluZyBpJ20gZG9pbmcgd3JvbmdcbiAgLy8gc29tZXdoZXJlIGVsc2UuIFNob3VsZCB0YWxrIHRvIHNlYmFzdGlhbiBhYm91dCB0aGlzIHBlcmhhcHNcbiAgY29uc3Qgbm9kZSA9IGZpbmRDdXJyZW50RmliZXJVc2luZ1Nsb3dQYXRoKHZub2RlKTtcbiAgc3dpdGNoIChub2RlLnRhZykge1xuICAgIGNhc2UgRmliZXJUYWdzLkhvc3RSb290OlxuICAgICAgcmV0dXJuIGNoaWxkcmVuVG9UcmVlKG5vZGUuY2hpbGQpO1xuICAgIGNhc2UgRmliZXJUYWdzLkhvc3RQb3J0YWw6IHtcbiAgICAgIGNvbnN0IHtcbiAgICAgICAgc3RhdGVOb2RlOiB7IGNvbnRhaW5lckluZm8gfSxcbiAgICAgICAgbWVtb2l6ZWRQcm9wczogY2hpbGRyZW4sXG4gICAgICB9ID0gbm9kZTtcbiAgICAgIGNvbnN0IHByb3BzID0geyBjb250YWluZXJJbmZvLCBjaGlsZHJlbiB9O1xuICAgICAgcmV0dXJuIHtcbiAgICAgICAgbm9kZVR5cGU6ICdwb3J0YWwnLFxuICAgICAgICB0eXBlOiBQb3J0YWwsXG4gICAgICAgIHByb3BzLFxuICAgICAgICBrZXk6IGVuc3VyZUtleU9yVW5kZWZpbmVkKG5vZGUua2V5KSxcbiAgICAgICAgcmVmOiBub2RlLnJlZixcbiAgICAgICAgaW5zdGFuY2U6IG51bGwsXG4gICAgICAgIHJlbmRlcmVkOiBjaGlsZHJlblRvVHJlZShub2RlLmNoaWxkKSxcbiAgICAgIH07XG4gICAgfVxuICAgIGNhc2UgRmliZXJUYWdzLkNsYXNzQ29tcG9uZW50OlxuICAgICAgcmV0dXJuIHtcbiAgICAgICAgbm9kZVR5cGU6ICdjbGFzcycsXG4gICAgICAgIHR5cGU6IG5vZGUudHlwZSxcbiAgICAgICAgcHJvcHM6IHsgLi4ubm9kZS5tZW1vaXplZFByb3BzIH0sXG4gICAgICAgIGtleTogZW5zdXJlS2V5T3JVbmRlZmluZWQobm9kZS5rZXkpLFxuICAgICAgICByZWY6IG5vZGUucmVmLFxuICAgICAgICBpbnN0YW5jZTogbm9kZS5zdGF0ZU5vZGUsXG4gICAgICAgIHJlbmRlcmVkOiBjaGlsZHJlblRvVHJlZShub2RlLmNoaWxkKSxcbiAgICAgIH07XG4gICAgY2FzZSBGaWJlclRhZ3MuRnVuY3Rpb25hbENvbXBvbmVudDpcbiAgICAgIHJldHVybiB7XG4gICAgICAgIG5vZGVUeXBlOiAnZnVuY3Rpb24nLFxuICAgICAgICB0eXBlOiBub2RlLnR5cGUsXG4gICAgICAgIHByb3BzOiB7IC4uLm5vZGUubWVtb2l6ZWRQcm9wcyB9LFxuICAgICAgICBrZXk6IGVuc3VyZUtleU9yVW5kZWZpbmVkKG5vZGUua2V5KSxcbiAgICAgICAgcmVmOiBub2RlLnJlZixcbiAgICAgICAgaW5zdGFuY2U6IG51bGwsXG4gICAgICAgIHJlbmRlcmVkOiBjaGlsZHJlblRvVHJlZShub2RlLmNoaWxkKSxcbiAgICAgIH07XG4gICAgY2FzZSBGaWJlclRhZ3MuTWVtb0NsYXNzOlxuICAgICAgcmV0dXJuIHtcbiAgICAgICAgbm9kZVR5cGU6ICdjbGFzcycsXG4gICAgICAgIHR5cGU6IG5vZGUuZWxlbWVudFR5cGUudHlwZSxcbiAgICAgICAgcHJvcHM6IHsgLi4ubm9kZS5tZW1vaXplZFByb3BzIH0sXG4gICAgICAgIGtleTogZW5zdXJlS2V5T3JVbmRlZmluZWQobm9kZS5rZXkpLFxuICAgICAgICByZWY6IG5vZGUucmVmLFxuICAgICAgICBpbnN0YW5jZTogbm9kZS5zdGF0ZU5vZGUsXG4gICAgICAgIHJlbmRlcmVkOiBjaGlsZHJlblRvVHJlZShub2RlLmNoaWxkLmNoaWxkKSxcbiAgICAgIH07XG4gICAgY2FzZSBGaWJlclRhZ3MuTWVtb1NGQzoge1xuICAgICAgbGV0IHJlbmRlcmVkTm9kZXMgPSBmbGF0dGVuKG5vZGVBbmRTaWJsaW5nc0FycmF5KG5vZGUuY2hpbGQpLm1hcCh0b1RyZWUpKTtcbiAgICAgIGlmIChyZW5kZXJlZE5vZGVzLmxlbmd0aCA9PT0gMCkge1xuICAgICAgICByZW5kZXJlZE5vZGVzID0gW25vZGUubWVtb2l6ZWRQcm9wcy5jaGlsZHJlbl07XG4gICAgICB9XG4gICAgICByZXR1cm4ge1xuICAgICAgICBub2RlVHlwZTogJ2Z1bmN0aW9uJyxcbiAgICAgICAgdHlwZTogbm9kZS5lbGVtZW50VHlwZSxcbiAgICAgICAgcHJvcHM6IHsgLi4ubm9kZS5tZW1vaXplZFByb3BzIH0sXG4gICAgICAgIGtleTogZW5zdXJlS2V5T3JVbmRlZmluZWQobm9kZS5rZXkpLFxuICAgICAgICByZWY6IG5vZGUucmVmLFxuICAgICAgICBpbnN0YW5jZTogbnVsbCxcbiAgICAgICAgcmVuZGVyZWQ6IHJlbmRlcmVkTm9kZXMsXG4gICAgICB9O1xuICAgIH1cbiAgICBjYXNlIEZpYmVyVGFncy5Ib3N0Q29tcG9uZW50OiB7XG4gICAgICBsZXQgcmVuZGVyZWROb2RlcyA9IGZsYXR0ZW4obm9kZUFuZFNpYmxpbmdzQXJyYXkobm9kZS5jaGlsZCkubWFwKHRvVHJlZSkpO1xuICAgICAgaWYgKHJlbmRlcmVkTm9kZXMubGVuZ3RoID09PSAwKSB7XG4gICAgICAgIHJlbmRlcmVkTm9kZXMgPSBbbm9kZS5tZW1vaXplZFByb3BzLmNoaWxkcmVuXTtcbiAgICAgIH1cbiAgICAgIHJldHVybiB7XG4gICAgICAgIG5vZGVUeXBlOiAnaG9zdCcsXG4gICAgICAgIHR5cGU6IG5vZGUudHlwZSxcbiAgICAgICAgcHJvcHM6IHsgLi4ubm9kZS5tZW1vaXplZFByb3BzIH0sXG4gICAgICAgIGtleTogZW5zdXJlS2V5T3JVbmRlZmluZWQobm9kZS5rZXkpLFxuICAgICAgICByZWY6IG5vZGUucmVmLFxuICAgICAgICBpbnN0YW5jZTogbm9kZS5zdGF0ZU5vZGUsXG4gICAgICAgIHJlbmRlcmVkOiByZW5kZXJlZE5vZGVzLFxuICAgICAgfTtcbiAgICB9XG4gICAgY2FzZSBGaWJlclRhZ3MuSG9zdFRleHQ6XG4gICAgICByZXR1cm4gbm9kZS5tZW1vaXplZFByb3BzO1xuICAgIGNhc2UgRmliZXJUYWdzLkZyYWdtZW50OlxuICAgIGNhc2UgRmliZXJUYWdzLk1vZGU6XG4gICAgY2FzZSBGaWJlclRhZ3MuQ29udGV4dFByb3ZpZGVyOlxuICAgIGNhc2UgRmliZXJUYWdzLkNvbnRleHRDb25zdW1lcjpcbiAgICAgIHJldHVybiBjaGlsZHJlblRvVHJlZShub2RlLmNoaWxkKTtcbiAgICBjYXNlIEZpYmVyVGFncy5Qcm9maWxlcjpcbiAgICBjYXNlIEZpYmVyVGFncy5Gb3J3YXJkUmVmOiB7XG4gICAgICByZXR1cm4ge1xuICAgICAgICBub2RlVHlwZTogJ2Z1bmN0aW9uJyxcbiAgICAgICAgdHlwZTogbm9kZS50eXBlLFxuICAgICAgICBwcm9wczogeyAuLi5ub2RlLnBlbmRpbmdQcm9wcyB9LFxuICAgICAgICBrZXk6IGVuc3VyZUtleU9yVW5kZWZpbmVkKG5vZGUua2V5KSxcbiAgICAgICAgcmVmOiBub2RlLnJlZixcbiAgICAgICAgaW5zdGFuY2U6IG51bGwsXG4gICAgICAgIHJlbmRlcmVkOiBjaGlsZHJlblRvVHJlZShub2RlLmNoaWxkKSxcbiAgICAgIH07XG4gICAgfVxuICAgIGNhc2UgRmliZXJUYWdzLlN1c3BlbnNlOiB7XG4gICAgICByZXR1cm4ge1xuICAgICAgICBub2RlVHlwZTogJ2Z1bmN0aW9uJyxcbiAgICAgICAgdHlwZTogU3VzcGVuc2UsXG4gICAgICAgIHByb3BzOiB7IC4uLm5vZGUubWVtb2l6ZWRQcm9wcyB9LFxuICAgICAgICBrZXk6IGVuc3VyZUtleU9yVW5kZWZpbmVkKG5vZGUua2V5KSxcbiAgICAgICAgcmVmOiBub2RlLnJlZixcbiAgICAgICAgaW5zdGFuY2U6IG51bGwsXG4gICAgICAgIHJlbmRlcmVkOiBjaGlsZHJlblRvVHJlZShub2RlLmNoaWxkKSxcbiAgICAgIH07XG4gICAgfVxuICAgIGNhc2UgRmliZXJUYWdzLkxhenk6XG4gICAgICByZXR1cm4gY2hpbGRyZW5Ub1RyZWUobm9kZS5jaGlsZCk7XG4gICAgZGVmYXVsdDpcbiAgICAgIHRocm93IG5ldyBFcnJvcihgRW56eW1lIEludGVybmFsIEVycm9yOiB1bmtub3duIG5vZGUgd2l0aCB0YWcgJHtub2RlLnRhZ31gKTtcbiAgfVxufVxuXG5mdW5jdGlvbiBjaGlsZHJlblRvVHJlZShub2RlKSB7XG4gIGlmICghbm9kZSkge1xuICAgIHJldHVybiBudWxsO1xuICB9XG4gIGNvbnN0IGNoaWxkcmVuID0gbm9kZUFuZFNpYmxpbmdzQXJyYXkobm9kZSk7XG4gIGlmIChjaGlsZHJlbi5sZW5ndGggPT09IDApIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuICBpZiAoY2hpbGRyZW4ubGVuZ3RoID09PSAxKSB7XG4gICAgcmV0dXJuIHRvVHJlZShjaGlsZHJlblswXSk7XG4gIH1cbiAgcmV0dXJuIGZsYXR0ZW4oY2hpbGRyZW4ubWFwKHRvVHJlZSkpO1xufVxuXG5mdW5jdGlvbiBub2RlVG9Ib3N0Tm9kZShfbm9kZSkge1xuICAvLyBOT1RFKGxtcik6IG5vZGUgY291bGQgYmUgYSBmdW5jdGlvbiBjb21wb25lbnRcbiAgLy8gd2hpY2ggd29udCBoYXZlIGFuIGluc3RhbmNlIHByb3AsIGJ1dCB3ZSBjYW4gZ2V0IHRoZVxuICAvLyBob3N0IG5vZGUgYXNzb2NpYXRlZCB3aXRoIGl0cyByZXR1cm4gdmFsdWUgYXQgdGhhdCBwb2ludC5cbiAgLy8gQWx0aG91Z2ggdGhpcyBicmVha3MgZG93biBpZiB0aGUgcmV0dXJuIHZhbHVlIGlzIGFuIGFycmF5LFxuICAvLyBhcyBpcyBwb3NzaWJsZSB3aXRoIFJlYWN0IDE2LlxuICBsZXQgbm9kZSA9IF9ub2RlO1xuICB3aGlsZSAobm9kZSAmJiAhQXJyYXkuaXNBcnJheShub2RlKSAmJiBub2RlLmluc3RhbmNlID09PSBudWxsKSB7XG4gICAgbm9kZSA9IG5vZGUucmVuZGVyZWQ7XG4gIH1cbiAgLy8gaWYgdGhlIFNGQyByZXR1cm5lZCBudWxsIGVmZmVjdGl2ZWx5LCB0aGVyZSBpcyBubyBob3N0IG5vZGUuXG4gIGlmICghbm9kZSkge1xuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgY29uc3QgbWFwcGVyID0gKGl0ZW0pID0+IHtcbiAgICBpZiAoaXRlbSAmJiBpdGVtLmluc3RhbmNlKSByZXR1cm4gUmVhY3RET00uZmluZERPTU5vZGUoaXRlbS5pbnN0YW5jZSk7XG4gICAgcmV0dXJuIG51bGw7XG4gIH07XG4gIGlmIChBcnJheS5pc0FycmF5KG5vZGUpKSB7XG4gICAgcmV0dXJuIG5vZGUubWFwKG1hcHBlcik7XG4gIH1cbiAgaWYgKEFycmF5LmlzQXJyYXkobm9kZS5yZW5kZXJlZCkgJiYgbm9kZS5ub2RlVHlwZSA9PT0gJ2NsYXNzJykge1xuICAgIHJldHVybiBub2RlLnJlbmRlcmVkLm1hcChtYXBwZXIpO1xuICB9XG4gIHJldHVybiBtYXBwZXIobm9kZSk7XG59XG5cbmZ1bmN0aW9uIHJlcGxhY2VMYXp5V2l0aEZhbGxiYWNrKG5vZGUsIGZhbGxiYWNrKSB7XG4gIGlmICghbm9kZSkge1xuICAgIHJldHVybiBudWxsO1xuICB9XG4gIGlmIChBcnJheS5pc0FycmF5KG5vZGUpKSB7XG4gICAgcmV0dXJuIG5vZGUubWFwKChlbCkgPT4gcmVwbGFjZUxhenlXaXRoRmFsbGJhY2soZWwsIGZhbGxiYWNrKSk7XG4gIH1cbiAgaWYgKGlzTGF6eShub2RlLnR5cGUpKSB7XG4gICAgcmV0dXJuIGZhbGxiYWNrO1xuICB9XG4gIHJldHVybiB7XG4gICAgLi4ubm9kZSxcbiAgICBwcm9wczoge1xuICAgICAgLi4ubm9kZS5wcm9wcyxcbiAgICAgIGNoaWxkcmVuOiByZXBsYWNlTGF6eVdpdGhGYWxsYmFjayhub2RlLnByb3BzLmNoaWxkcmVuLCBmYWxsYmFjayksXG4gICAgfSxcbiAgfTtcbn1cblxuY29uc3QgZXZlbnRPcHRpb25zID0ge1xuICBhbmltYXRpb246IHRydWUsXG4gIHBvaW50ZXJFdmVudHM6IGlzMTY0LFxuICBhdXhDbGljazogaXMxNjUsXG59O1xuXG5mdW5jdGlvbiBnZXRFbXB0eVN0YXRlVmFsdWUoKSB7XG4gIC8vIHRoaXMgaGFuZGxlcyBhIGJ1ZyBpbiBSZWFjdCAxNi4wIC0gMTYuMlxuICAvLyBzZWUgaHR0cHM6Ly9naXRodWIuY29tL2ZhY2Vib29rL3JlYWN0L2NvbW1pdC8zOWJlODM1NjVjNjVmOWM1MjIxNTBlNTIzNzUxNjc1NjhhMmExNDU5XG4gIC8vIGFsc28gc2VlIGh0dHBzOi8vZ2l0aHViLmNvbS9mYWNlYm9vay9yZWFjdC9wdWxsLzExOTY1XG5cbiAgLy8gZXNsaW50LWRpc2FibGUtbmV4dC1saW5lIHJlYWN0L3ByZWZlci1zdGF0ZWxlc3MtZnVuY3Rpb25cbiAgY2xhc3MgRW1wdHlTdGF0ZSBleHRlbmRzIFJlYWN0LkNvbXBvbmVudCB7XG4gICAgcmVuZGVyKCkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICB9XG4gIGNvbnN0IHRlc3RSZW5kZXJlciA9IG5ldyBTaGFsbG93UmVuZGVyZXIoKTtcbiAgdGVzdFJlbmRlcmVyLnJlbmRlcihSZWFjdC5jcmVhdGVFbGVtZW50KEVtcHR5U3RhdGUpKTtcbiAgcmV0dXJuIHRlc3RSZW5kZXJlci5faW5zdGFuY2Uuc3RhdGU7XG59XG5cbmZ1bmN0aW9uIHdyYXBBY3QoZm4pIHtcbiAgaWYgKCFpczE2OCkge1xuICAgIHJldHVybiBmbigpO1xuICB9XG4gIGxldCByZXR1cm5WYWw7XG4gIFRlc3RVdGlscy5hY3QoKCkgPT4geyByZXR1cm5WYWwgPSBmbigpOyB9KTtcbiAgcmV0dXJuIHJldHVyblZhbDtcbn1cblxuZnVuY3Rpb24gZ2V0UHJvdmlkZXJEZWZhdWx0VmFsdWUoUHJvdmlkZXIpIHtcbiAgLy8gUmVhY3Qgc3RvcmVzIHJlZmVyZW5jZXMgdG8gdGhlIFByb3ZpZGVyJ3MgZGVmYXVsdFZhbHVlIGRpZmZlcmVudGx5IGFjcm9zcyB2ZXJzaW9ucy5cbiAgaWYgKCdfZGVmYXVsdFZhbHVlJyBpbiBQcm92aWRlci5fY29udGV4dCkge1xuICAgIHJldHVybiBQcm92aWRlci5fY29udGV4dC5fZGVmYXVsdFZhbHVlO1xuICB9XG4gIGlmICgnX2N1cnJlbnRWYWx1ZScgaW4gUHJvdmlkZXIuX2NvbnRleHQpIHtcbiAgICByZXR1cm4gUHJvdmlkZXIuX2NvbnRleHQuX2N1cnJlbnRWYWx1ZTtcbiAgfVxuICB0aHJvdyBuZXcgRXJyb3IoJ0VuenltZSBJbnRlcm5hbCBFcnJvcjogY2Fu4oCZdCBmaWd1cmUgb3V0IGhvdyB0byBnZXQgUHJvdmlkZXLigJlzIGRlZmF1bHQgdmFsdWUnKTtcbn1cblxuZnVuY3Rpb24gbWFrZUZha2VFbGVtZW50KHR5cGUpIHtcbiAgcmV0dXJuIHsgJCR0eXBlb2Y6IEVsZW1lbnQsIHR5cGUgfTtcbn1cblxuZnVuY3Rpb24gaXNTdGF0ZWZ1bChDb21wb25lbnQpIHtcbiAgcmV0dXJuIENvbXBvbmVudC5wcm90b3R5cGUgJiYgKFxuICAgIENvbXBvbmVudC5wcm90b3R5cGUuaXNSZWFjdENvbXBvbmVudFxuICAgIHx8IEFycmF5LmlzQXJyYXkoQ29tcG9uZW50Ll9fcmVhY3RBdXRvQmluZFBhaXJzKSAvLyBmYWxsYmFjayBmb3IgY3JlYXRlQ2xhc3MgY29tcG9uZW50c1xuICApO1xufVxuXG5jbGFzcyBSZWFjdFNpeHRlZW5BZGFwdGVyIGV4dGVuZHMgRW56eW1lQWRhcHRlciB7XG4gIGNvbnN0cnVjdG9yKCkge1xuICAgIHN1cGVyKCk7XG4gICAgY29uc3QgeyBsaWZlY3ljbGVzIH0gPSB0aGlzLm9wdGlvbnM7XG4gICAgdGhpcy5vcHRpb25zID0ge1xuICAgICAgLi4udGhpcy5vcHRpb25zLFxuICAgICAgZW5hYmxlQ29tcG9uZW50RGlkVXBkYXRlT25TZXRTdGF0ZTogdHJ1ZSwgLy8gVE9ETzogcmVtb3ZlLCBzZW12ZXItbWFqb3JcbiAgICAgIGxlZ2FjeUNvbnRleHRNb2RlOiAncGFyZW50JyxcbiAgICAgIGxpZmVjeWNsZXM6IHtcbiAgICAgICAgLi4ubGlmZWN5Y2xlcyxcbiAgICAgICAgY29tcG9uZW50RGlkVXBkYXRlOiB7XG4gICAgICAgICAgb25TZXRTdGF0ZTogdHJ1ZSxcbiAgICAgICAgfSxcbiAgICAgICAgZ2V0RGVyaXZlZFN0YXRlRnJvbVByb3BzOiB7XG4gICAgICAgICAgaGFzU2hvdWxkQ29tcG9uZW50VXBkYXRlQnVnLFxuICAgICAgICB9LFxuICAgICAgICBnZXRTbmFwc2hvdEJlZm9yZVVwZGF0ZTogdHJ1ZSxcbiAgICAgICAgc2V0U3RhdGU6IHtcbiAgICAgICAgICBza2lwc0NvbXBvbmVudERpZFVwZGF0ZU9uTnVsbGlzaDogdHJ1ZSxcbiAgICAgICAgfSxcbiAgICAgICAgZ2V0Q2hpbGRDb250ZXh0OiB7XG4gICAgICAgICAgY2FsbGVkQnlSZW5kZXJlcjogZmFsc2UsXG4gICAgICAgIH0sXG4gICAgICAgIGdldERlcml2ZWRTdGF0ZUZyb21FcnJvcjogaXMxNjYsXG4gICAgICB9LFxuICAgIH07XG4gIH1cblxuICBjcmVhdGVNb3VudFJlbmRlcmVyKG9wdGlvbnMpIHtcbiAgICBhc3NlcnREb21BdmFpbGFibGUoJ21vdW50Jyk7XG4gICAgaWYgKGhhcyhvcHRpb25zLCAnc3VzcGVuc2VGYWxsYmFjaycpKSB7XG4gICAgICB0aHJvdyBuZXcgVHlwZUVycm9yKCdgc3VzcGVuc2VGYWxsYmFja2AgaXMgbm90IHN1cHBvcnRlZCBieSB0aGUgYG1vdW50YCByZW5kZXJlcicpO1xuICAgIH1cbiAgICBpZiAoRmliZXJUYWdzID09PSBudWxsKSB7XG4gICAgICAvLyBSZXF1aXJlcyBET00uXG4gICAgICBGaWJlclRhZ3MgPSBkZXRlY3RGaWJlclRhZ3MoKTtcbiAgICB9XG4gICAgY29uc3QgeyBhdHRhY2hUbywgaHlkcmF0ZUluLCB3cmFwcGluZ0NvbXBvbmVudFByb3BzIH0gPSBvcHRpb25zO1xuICAgIGNvbnN0IGRvbU5vZGUgPSBoeWRyYXRlSW4gfHwgYXR0YWNoVG8gfHwgZ2xvYmFsLmRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2RpdicpO1xuICAgIGxldCBpbnN0YW5jZSA9IG51bGw7XG4gICAgY29uc3QgYWRhcHRlciA9IHRoaXM7XG4gICAgcmV0dXJuIHtcbiAgICAgIHJlbmRlcihlbCwgY29udGV4dCwgY2FsbGJhY2spIHtcbiAgICAgICAgcmV0dXJuIHdyYXBBY3QoKCkgPT4ge1xuICAgICAgICAgIGlmIChpbnN0YW5jZSA9PT0gbnVsbCkge1xuICAgICAgICAgICAgY29uc3QgeyB0eXBlLCBwcm9wcywgcmVmIH0gPSBlbDtcbiAgICAgICAgICAgIGNvbnN0IHdyYXBwZXJQcm9wcyA9IHtcbiAgICAgICAgICAgICAgQ29tcG9uZW50OiB0eXBlLFxuICAgICAgICAgICAgICBwcm9wcyxcbiAgICAgICAgICAgICAgd3JhcHBpbmdDb21wb25lbnRQcm9wcyxcbiAgICAgICAgICAgICAgY29udGV4dCxcbiAgICAgICAgICAgICAgLi4uKHJlZiAmJiB7IHJlZlByb3A6IHJlZiB9KSxcbiAgICAgICAgICAgIH07XG4gICAgICAgICAgICBjb25zdCBSZWFjdFdyYXBwZXJDb21wb25lbnQgPSBjcmVhdGVNb3VudFdyYXBwZXIoZWwsIHsgLi4ub3B0aW9ucywgYWRhcHRlciB9KTtcbiAgICAgICAgICAgIGNvbnN0IHdyYXBwZWRFbCA9IFJlYWN0LmNyZWF0ZUVsZW1lbnQoUmVhY3RXcmFwcGVyQ29tcG9uZW50LCB3cmFwcGVyUHJvcHMpO1xuICAgICAgICAgICAgaW5zdGFuY2UgPSBoeWRyYXRlSW5cbiAgICAgICAgICAgICAgPyBSZWFjdERPTS5oeWRyYXRlKHdyYXBwZWRFbCwgZG9tTm9kZSlcbiAgICAgICAgICAgICAgOiBSZWFjdERPTS5yZW5kZXIod3JhcHBlZEVsLCBkb21Ob2RlKTtcbiAgICAgICAgICAgIGlmICh0eXBlb2YgY2FsbGJhY2sgPT09ICdmdW5jdGlvbicpIHtcbiAgICAgICAgICAgICAgY2FsbGJhY2soKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgaW5zdGFuY2Uuc2V0Q2hpbGRQcm9wcyhlbC5wcm9wcywgY29udGV4dCwgY2FsbGJhY2spO1xuICAgICAgICAgIH1cbiAgICAgICAgfSk7XG4gICAgICB9LFxuICAgICAgdW5tb3VudCgpIHtcbiAgICAgICAgUmVhY3RET00udW5tb3VudENvbXBvbmVudEF0Tm9kZShkb21Ob2RlKTtcbiAgICAgICAgaW5zdGFuY2UgPSBudWxsO1xuICAgICAgfSxcbiAgICAgIGdldE5vZGUoKSB7XG4gICAgICAgIGlmICghaW5zdGFuY2UpIHtcbiAgICAgICAgICByZXR1cm4gbnVsbDtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gZ2V0Tm9kZUZyb21Sb290RmluZGVyKFxuICAgICAgICAgIGFkYXB0ZXIuaXNDdXN0b21Db21wb25lbnQsXG4gICAgICAgICAgdG9UcmVlKGluc3RhbmNlLl9yZWFjdEludGVybmFsRmliZXIpLFxuICAgICAgICAgIG9wdGlvbnMsXG4gICAgICAgICk7XG4gICAgICB9LFxuICAgICAgc2ltdWxhdGVFcnJvcihub2RlSGllcmFyY2h5LCByb290Tm9kZSwgZXJyb3IpIHtcbiAgICAgICAgY29uc3QgaXNFcnJvckJvdW5kYXJ5ID0gKHsgaW5zdGFuY2U6IGVsSW5zdGFuY2UsIHR5cGUgfSkgPT4ge1xuICAgICAgICAgIGlmIChpczE2NiAmJiB0eXBlICYmIHR5cGUuZ2V0RGVyaXZlZFN0YXRlRnJvbUVycm9yKSB7XG4gICAgICAgICAgICByZXR1cm4gdHJ1ZTtcbiAgICAgICAgICB9XG4gICAgICAgICAgcmV0dXJuIGVsSW5zdGFuY2UgJiYgZWxJbnN0YW5jZS5jb21wb25lbnREaWRDYXRjaDtcbiAgICAgICAgfTtcblxuICAgICAgICBjb25zdCB7XG4gICAgICAgICAgaW5zdGFuY2U6IGNhdGNoaW5nSW5zdGFuY2UsXG4gICAgICAgICAgdHlwZTogY2F0Y2hpbmdUeXBlLFxuICAgICAgICB9ID0gbm9kZUhpZXJhcmNoeS5maW5kKGlzRXJyb3JCb3VuZGFyeSkgfHwge307XG5cbiAgICAgICAgc2ltdWxhdGVFcnJvcihcbiAgICAgICAgICBlcnJvcixcbiAgICAgICAgICBjYXRjaGluZ0luc3RhbmNlLFxuICAgICAgICAgIHJvb3ROb2RlLFxuICAgICAgICAgIG5vZGVIaWVyYXJjaHksXG4gICAgICAgICAgbm9kZVR5cGVGcm9tVHlwZSxcbiAgICAgICAgICBhZGFwdGVyLmRpc3BsYXlOYW1lT2ZOb2RlLFxuICAgICAgICAgIGlzMTY2ID8gY2F0Y2hpbmdUeXBlIDogdW5kZWZpbmVkLFxuICAgICAgICApO1xuICAgICAgfSxcbiAgICAgIHNpbXVsYXRlRXZlbnQobm9kZSwgZXZlbnQsIG1vY2spIHtcbiAgICAgICAgY29uc3QgbWFwcGVkRXZlbnQgPSBtYXBOYXRpdmVFdmVudE5hbWVzKGV2ZW50LCBldmVudE9wdGlvbnMpO1xuICAgICAgICBjb25zdCBldmVudEZuID0gVGVzdFV0aWxzLlNpbXVsYXRlW21hcHBlZEV2ZW50XTtcbiAgICAgICAgaWYgKCFldmVudEZuKSB7XG4gICAgICAgICAgdGhyb3cgbmV3IFR5cGVFcnJvcihgUmVhY3RXcmFwcGVyOjpzaW11bGF0ZSgpIGV2ZW50ICcke2V2ZW50fScgZG9lcyBub3QgZXhpc3RgKTtcbiAgICAgICAgfVxuICAgICAgICB3cmFwQWN0KCgpID0+IHtcbiAgICAgICAgICBldmVudEZuKGFkYXB0ZXIubm9kZVRvSG9zdE5vZGUobm9kZSksIG1vY2spO1xuICAgICAgICB9KTtcbiAgICAgIH0sXG4gICAgICBiYXRjaGVkVXBkYXRlcyhmbikge1xuICAgICAgICByZXR1cm4gZm4oKTtcbiAgICAgICAgLy8gcmV0dXJuIFJlYWN0RE9NLnVuc3RhYmxlX2JhdGNoZWRVcGRhdGVzKGZuKTtcbiAgICAgIH0sXG4gICAgICBnZXRXcmFwcGluZ0NvbXBvbmVudFJlbmRlcmVyKCkge1xuICAgICAgICByZXR1cm4ge1xuICAgICAgICAgIC4uLnRoaXMsXG4gICAgICAgICAgLi4uZ2V0V3JhcHBpbmdDb21wb25lbnRNb3VudFJlbmRlcmVyKHtcbiAgICAgICAgICAgIHRvVHJlZTogKGluc3QpID0+IHRvVHJlZShpbnN0Ll9yZWFjdEludGVybmFsRmliZXIpLFxuICAgICAgICAgICAgZ2V0TW91bnRXcmFwcGVySW5zdGFuY2U6ICgpID0+IGluc3RhbmNlLFxuICAgICAgICAgIH0pLFxuICAgICAgICB9O1xuICAgICAgfSxcbiAgICAgIC4uLihpczE2OCAmJiB7IHdyYXBJbnZva2U6IHdyYXBBY3QgfSksXG4gICAgfTtcbiAgfVxuXG4gIGNyZWF0ZVNoYWxsb3dSZW5kZXJlcihvcHRpb25zID0ge30pIHtcbiAgICBjb25zdCBhZGFwdGVyID0gdGhpcztcbiAgICBjb25zdCByZW5kZXJlciA9IG5ldyBTaGFsbG93UmVuZGVyZXIoKTtcbiAgICBjb25zdCB7IHN1c3BlbnNlRmFsbGJhY2sgfSA9IG9wdGlvbnM7XG4gICAgaWYgKHR5cGVvZiBzdXNwZW5zZUZhbGxiYWNrICE9PSAndW5kZWZpbmVkJyAmJiB0eXBlb2Ygc3VzcGVuc2VGYWxsYmFjayAhPT0gJ2Jvb2xlYW4nKSB7XG4gICAgICB0aHJvdyBUeXBlRXJyb3IoJ2BvcHRpb25zLnN1c3BlbnNlRmFsbGJhY2tgIHNob3VsZCBiZSBib29sZWFuIG9yIHVuZGVmaW5lZCcpO1xuICAgIH1cbiAgICBsZXQgaXNET00gPSBmYWxzZTtcbiAgICBsZXQgY2FjaGVkTm9kZSA9IG51bGw7XG5cbiAgICBsZXQgbGFzdENvbXBvbmVudCA9IG51bGw7XG4gICAgbGV0IHdyYXBwZWRDb21wb25lbnQgPSBudWxsO1xuICAgIGNvbnN0IHNlbnRpbmVsID0ge307XG5cbiAgICAvLyB3cmFwIG1lbW8gY29tcG9uZW50cyB3aXRoIGEgUHVyZUNvbXBvbmVudCwgb3IgYSBjbGFzcyBjb21wb25lbnQgd2l0aCBzQ1VcbiAgICBjb25zdCB3cmFwUHVyZUNvbXBvbmVudCA9IChDb21wb25lbnQsIGNvbXBhcmUpID0+IHtcbiAgICAgIGlmICghaXMxNjYpIHtcbiAgICAgICAgdGhyb3cgbmV3IFJhbmdlRXJyb3IoJ3RoaXMgZnVuY3Rpb24gc2hvdWxkIG5vdCBiZSBjYWxsZWQgaW4gUmVhY3QgPCAxNi42LiBQbGVhc2UgcmVwb3J0IHRoaXMhJyk7XG4gICAgICB9XG4gICAgICBpZiAobGFzdENvbXBvbmVudCAhPT0gQ29tcG9uZW50KSB7XG4gICAgICAgIGlmIChpc1N0YXRlZnVsKENvbXBvbmVudCkpIHtcbiAgICAgICAgICB3cmFwcGVkQ29tcG9uZW50ID0gY2xhc3MgZXh0ZW5kcyBDb21wb25lbnQge307IC8vIGVzbGludC1kaXNhYmxlLWxpbmUgcmVhY3QvcHJlZmVyLXN0YXRlbGVzcy1mdW5jdGlvblxuICAgICAgICAgIGlmIChjb21wYXJlKSB7XG4gICAgICAgICAgICB3cmFwcGVkQ29tcG9uZW50LnByb3RvdHlwZS5zaG91bGRDb21wb25lbnRVcGRhdGUgPSAobmV4dFByb3BzKSA9PiAhY29tcGFyZSh0aGlzLnByb3BzLCBuZXh0UHJvcHMpO1xuICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICB3cmFwcGVkQ29tcG9uZW50LnByb3RvdHlwZS5pc1B1cmVSZWFjdENvbXBvbmVudCA9IHRydWU7XG4gICAgICAgICAgfVxuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIGxldCBtZW1vaXplZCA9IHNlbnRpbmVsO1xuICAgICAgICAgIGxldCBwcmV2UHJvcHM7XG4gICAgICAgICAgd3JhcHBlZENvbXBvbmVudCA9IGZ1bmN0aW9uIChwcm9wcywgLi4uYXJncykge1xuICAgICAgICAgICAgY29uc3Qgc2hvdWxkVXBkYXRlID0gbWVtb2l6ZWQgPT09IHNlbnRpbmVsIHx8IChjb21wYXJlXG4gICAgICAgICAgICAgID8gIWNvbXBhcmUocHJldlByb3BzLCBwcm9wcylcbiAgICAgICAgICAgICAgOiAhc2hhbGxvd0VxdWFsKHByZXZQcm9wcywgcHJvcHMpXG4gICAgICAgICAgICApO1xuICAgICAgICAgICAgaWYgKHNob3VsZFVwZGF0ZSkge1xuICAgICAgICAgICAgICBtZW1vaXplZCA9IENvbXBvbmVudCh7IC4uLkNvbXBvbmVudC5kZWZhdWx0UHJvcHMsIC4uLnByb3BzIH0sIC4uLmFyZ3MpO1xuICAgICAgICAgICAgICBwcmV2UHJvcHMgPSBwcm9wcztcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBtZW1vaXplZDtcbiAgICAgICAgICB9O1xuICAgICAgICB9XG4gICAgICAgIE9iamVjdC5hc3NpZ24oXG4gICAgICAgICAgd3JhcHBlZENvbXBvbmVudCxcbiAgICAgICAgICBDb21wb25lbnQsXG4gICAgICAgICAgeyBkaXNwbGF5TmFtZTogYWRhcHRlci5kaXNwbGF5TmFtZU9mTm9kZSh7IHR5cGU6IENvbXBvbmVudCB9KSB9LFxuICAgICAgICApO1xuICAgICAgICBsYXN0Q29tcG9uZW50ID0gQ29tcG9uZW50O1xuICAgICAgfVxuICAgICAgcmV0dXJuIHdyYXBwZWRDb21wb25lbnQ7XG4gICAgfTtcblxuICAgIC8vIFdyYXAgZnVuY3Rpb25hbCBjb21wb25lbnRzIG9uIHZlcnNpb25zIHByaW9yIHRvIDE2LjUsXG4gICAgLy8gdG8gYXZvaWQgaW5hZHZlcnRlbnRseSBwYXNzIGEgYHRoaXNgIGluc3RhbmNlIHRvIGl0LlxuICAgIGNvbnN0IHdyYXBGdW5jdGlvbmFsQ29tcG9uZW50ID0gKENvbXBvbmVudCkgPT4ge1xuICAgICAgaWYgKGlzMTY2ICYmIGhhcyhDb21wb25lbnQsICdkZWZhdWx0UHJvcHMnKSkge1xuICAgICAgICBpZiAobGFzdENvbXBvbmVudCAhPT0gQ29tcG9uZW50KSB7XG4gICAgICAgICAgd3JhcHBlZENvbXBvbmVudCA9IE9iamVjdC5hc3NpZ24oXG4gICAgICAgICAgICAvLyBlc2xpbnQtZGlzYWJsZS1uZXh0LWxpbmUgbmV3LWNhcFxuICAgICAgICAgICAgKHByb3BzLCAuLi5hcmdzKSA9PiBDb21wb25lbnQoeyAuLi5Db21wb25lbnQuZGVmYXVsdFByb3BzLCAuLi5wcm9wcyB9LCAuLi5hcmdzKSxcbiAgICAgICAgICAgIENvbXBvbmVudCxcbiAgICAgICAgICAgIHsgZGlzcGxheU5hbWU6IGFkYXB0ZXIuZGlzcGxheU5hbWVPZk5vZGUoeyB0eXBlOiBDb21wb25lbnQgfSkgfSxcbiAgICAgICAgICApO1xuICAgICAgICAgIGxhc3RDb21wb25lbnQgPSBDb21wb25lbnQ7XG4gICAgICAgIH1cbiAgICAgICAgcmV0dXJuIHdyYXBwZWRDb21wb25lbnQ7XG4gICAgICB9XG4gICAgICBpZiAoaXMxNjUpIHtcbiAgICAgICAgcmV0dXJuIENvbXBvbmVudDtcbiAgICAgIH1cblxuICAgICAgaWYgKGxhc3RDb21wb25lbnQgIT09IENvbXBvbmVudCkge1xuICAgICAgICB3cmFwcGVkQ29tcG9uZW50ID0gT2JqZWN0LmFzc2lnbihcbiAgICAgICAgICAoLi4uYXJncykgPT4gQ29tcG9uZW50KC4uLmFyZ3MpLCAvLyBlc2xpbnQtZGlzYWJsZS1saW5lIG5ldy1jYXBcbiAgICAgICAgICBDb21wb25lbnQsXG4gICAgICAgICk7XG4gICAgICAgIGxhc3RDb21wb25lbnQgPSBDb21wb25lbnQ7XG4gICAgICB9XG4gICAgICByZXR1cm4gd3JhcHBlZENvbXBvbmVudDtcbiAgICB9O1xuXG4gICAgY29uc3QgcmVuZGVyRWxlbWVudCA9IChlbENvbmZpZywgLi4ucmVzdCkgPT4ge1xuICAgICAgY29uc3QgcmVuZGVyZWRFbCA9IHJlbmRlcmVyLnJlbmRlcihlbENvbmZpZywgLi4ucmVzdCk7XG5cbiAgICAgIGNvbnN0IHR5cGVJc0V4aXN0ZWQgPSAhIShyZW5kZXJlZEVsICYmIHJlbmRlcmVkRWwudHlwZSk7XG4gICAgICBpZiAoaXMxNjYgJiYgdHlwZUlzRXhpc3RlZCkge1xuICAgICAgICBjb25zdCBjbG9uZWRFbCA9IHRyYW5zZm9ybVN1c3BlbnNlKHJlbmRlcmVkRWwsIGVsQ29uZmlnLCB7IHN1c3BlbnNlRmFsbGJhY2sgfSk7XG5cbiAgICAgICAgY29uc3QgZWxlbWVudElzQ2hhbmdlZCA9IGNsb25lZEVsLnR5cGUgIT09IHJlbmRlcmVkRWwudHlwZTtcbiAgICAgICAgaWYgKGVsZW1lbnRJc0NoYW5nZWQpIHtcbiAgICAgICAgICByZXR1cm4gcmVuZGVyZXIucmVuZGVyKHsgLi4uZWxDb25maWcsIHR5cGU6IGNsb25lZEVsLnR5cGUgfSwgLi4ucmVzdCk7XG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgcmV0dXJuIHJlbmRlcmVkRWw7XG4gICAgfTtcblxuICAgIHJldHVybiB7XG4gICAgICByZW5kZXIoZWwsIHVubWFza2VkQ29udGV4dCwge1xuICAgICAgICBwcm92aWRlclZhbHVlcyA9IG5ldyBNYXAoKSxcbiAgICAgIH0gPSB7fSkge1xuICAgICAgICBjYWNoZWROb2RlID0gZWw7XG4gICAgICAgIC8qIGVzbGludCBjb25zaXN0ZW50LXJldHVybjogMCAqL1xuICAgICAgICBpZiAodHlwZW9mIGVsLnR5cGUgPT09ICdzdHJpbmcnKSB7XG4gICAgICAgICAgaXNET00gPSB0cnVlO1xuICAgICAgICB9IGVsc2UgaWYgKGlzQ29udGV4dFByb3ZpZGVyKGVsKSkge1xuICAgICAgICAgIHByb3ZpZGVyVmFsdWVzLnNldChlbC50eXBlLCBlbC5wcm9wcy52YWx1ZSk7XG4gICAgICAgICAgY29uc3QgTW9ja1Byb3ZpZGVyID0gT2JqZWN0LmFzc2lnbihcbiAgICAgICAgICAgIChwcm9wcykgPT4gcHJvcHMuY2hpbGRyZW4sXG4gICAgICAgICAgICBlbC50eXBlLFxuICAgICAgICAgICk7XG4gICAgICAgICAgcmV0dXJuIHdpdGhTZXRTdGF0ZUFsbG93ZWQoKCkgPT4gcmVuZGVyRWxlbWVudCh7IC4uLmVsLCB0eXBlOiBNb2NrUHJvdmlkZXIgfSkpO1xuICAgICAgICB9IGVsc2UgaWYgKGlzQ29udGV4dENvbnN1bWVyKGVsKSkge1xuICAgICAgICAgIGNvbnN0IFByb3ZpZGVyID0gYWRhcHRlci5nZXRQcm92aWRlckZyb21Db25zdW1lcihlbC50eXBlKTtcbiAgICAgICAgICBjb25zdCB2YWx1ZSA9IHByb3ZpZGVyVmFsdWVzLmhhcyhQcm92aWRlcilcbiAgICAgICAgICAgID8gcHJvdmlkZXJWYWx1ZXMuZ2V0KFByb3ZpZGVyKVxuICAgICAgICAgICAgOiBnZXRQcm92aWRlckRlZmF1bHRWYWx1ZShQcm92aWRlcik7XG4gICAgICAgICAgY29uc3QgTW9ja0NvbnN1bWVyID0gT2JqZWN0LmFzc2lnbihcbiAgICAgICAgICAgIChwcm9wcykgPT4gcHJvcHMuY2hpbGRyZW4odmFsdWUpLFxuICAgICAgICAgICAgZWwudHlwZSxcbiAgICAgICAgICApO1xuICAgICAgICAgIHJldHVybiB3aXRoU2V0U3RhdGVBbGxvd2VkKCgpID0+IHJlbmRlckVsZW1lbnQoeyAuLi5lbCwgdHlwZTogTW9ja0NvbnN1bWVyIH0pKTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICBpc0RPTSA9IGZhbHNlO1xuICAgICAgICAgIGxldCByZW5kZXJlZEVsID0gZWw7XG4gICAgICAgICAgaWYgKGlzTGF6eShyZW5kZXJlZEVsKSkge1xuICAgICAgICAgICAgdGhyb3cgVHlwZUVycm9yKCdgUmVhY3QubGF6eWAgaXMgbm90IHN1cHBvcnRlZCBieSBzaGFsbG93IHJlbmRlcmluZy4nKTtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICByZW5kZXJlZEVsID0gdHJhbnNmb3JtU3VzcGVuc2UocmVuZGVyZWRFbCwgcmVuZGVyZWRFbCwgeyBzdXNwZW5zZUZhbGxiYWNrIH0pO1xuICAgICAgICAgIGNvbnN0IHsgdHlwZTogQ29tcG9uZW50IH0gPSByZW5kZXJlZEVsO1xuXG4gICAgICAgICAgY29uc3QgY29udGV4dCA9IGdldE1hc2tlZENvbnRleHQoQ29tcG9uZW50LmNvbnRleHRUeXBlcywgdW5tYXNrZWRDb250ZXh0KTtcblxuICAgICAgICAgIGlmIChpc01lbW8oZWwudHlwZSkpIHtcbiAgICAgICAgICAgIGNvbnN0IHsgdHlwZTogSW5uZXJDb21wLCBjb21wYXJlIH0gPSBlbC50eXBlO1xuXG4gICAgICAgICAgICByZXR1cm4gd2l0aFNldFN0YXRlQWxsb3dlZCgoKSA9PiByZW5kZXJFbGVtZW50KFxuICAgICAgICAgICAgICB7IC4uLmVsLCB0eXBlOiB3cmFwUHVyZUNvbXBvbmVudChJbm5lckNvbXAsIGNvbXBhcmUpIH0sXG4gICAgICAgICAgICAgIGNvbnRleHQsXG4gICAgICAgICAgICApKTtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICBjb25zdCBpc0NvbXBvbmVudFN0YXRlZnVsID0gaXNTdGF0ZWZ1bChDb21wb25lbnQpO1xuXG4gICAgICAgICAgaWYgKCFpc0NvbXBvbmVudFN0YXRlZnVsICYmIHR5cGVvZiBDb21wb25lbnQgPT09ICdmdW5jdGlvbicpIHtcbiAgICAgICAgICAgIHJldHVybiB3aXRoU2V0U3RhdGVBbGxvd2VkKCgpID0+IHJlbmRlckVsZW1lbnQoXG4gICAgICAgICAgICAgIHsgLi4ucmVuZGVyZWRFbCwgdHlwZTogd3JhcEZ1bmN0aW9uYWxDb21wb25lbnQoQ29tcG9uZW50KSB9LFxuICAgICAgICAgICAgICBjb250ZXh0LFxuICAgICAgICAgICAgKSk7XG4gICAgICAgICAgfVxuXG4gICAgICAgICAgaWYgKGlzQ29tcG9uZW50U3RhdGVmdWwpIHtcbiAgICAgICAgICAgIGlmIChcbiAgICAgICAgICAgICAgcmVuZGVyZXIuX2luc3RhbmNlXG4gICAgICAgICAgICAgICYmIGVsLnByb3BzID09PSByZW5kZXJlci5faW5zdGFuY2UucHJvcHNcbiAgICAgICAgICAgICAgJiYgIXNoYWxsb3dFcXVhbChjb250ZXh0LCByZW5kZXJlci5faW5zdGFuY2UuY29udGV4dClcbiAgICAgICAgICAgICkge1xuICAgICAgICAgICAgICBjb25zdCB7IHJlc3RvcmUgfSA9IHNweU1ldGhvZChcbiAgICAgICAgICAgICAgICByZW5kZXJlcixcbiAgICAgICAgICAgICAgICAnX3VwZGF0ZUNsYXNzQ29tcG9uZW50JyxcbiAgICAgICAgICAgICAgICAob3JpZ2luYWxNZXRob2QpID0+IGZ1bmN0aW9uIF91cGRhdGVDbGFzc0NvbXBvbmVudCguLi5hcmdzKSB7XG4gICAgICAgICAgICAgICAgICBjb25zdCB7IHByb3BzIH0gPSByZW5kZXJlci5faW5zdGFuY2U7XG4gICAgICAgICAgICAgICAgICBjb25zdCBjbG9uZWRQcm9wcyA9IHsgLi4ucHJvcHMgfTtcbiAgICAgICAgICAgICAgICAgIHJlbmRlcmVyLl9pbnN0YW5jZS5wcm9wcyA9IGNsb25lZFByb3BzO1xuXG4gICAgICAgICAgICAgICAgICBjb25zdCByZXN1bHQgPSBvcmlnaW5hbE1ldGhvZC5hcHBseShyZW5kZXJlciwgYXJncyk7XG5cbiAgICAgICAgICAgICAgICAgIHJlbmRlcmVyLl9pbnN0YW5jZS5wcm9wcyA9IHByb3BzO1xuICAgICAgICAgICAgICAgICAgcmVzdG9yZSgpO1xuXG4gICAgICAgICAgICAgICAgICByZXR1cm4gcmVzdWx0O1xuICAgICAgICAgICAgICAgIH0sXG4gICAgICAgICAgICAgICk7XG4gICAgICAgICAgICB9XG5cbiAgICAgICAgICAgIC8vIGZpeCByZWFjdCBidWc7IHNlZSBpbXBsZW1lbnRhdGlvbiBvZiBgZ2V0RW1wdHlTdGF0ZVZhbHVlYFxuICAgICAgICAgICAgY29uc3QgZW1wdHlTdGF0ZVZhbHVlID0gZ2V0RW1wdHlTdGF0ZVZhbHVlKCk7XG4gICAgICAgICAgICBpZiAoZW1wdHlTdGF0ZVZhbHVlKSB7XG4gICAgICAgICAgICAgIE9iamVjdC5kZWZpbmVQcm9wZXJ0eShDb21wb25lbnQucHJvdG90eXBlLCAnc3RhdGUnLCB7XG4gICAgICAgICAgICAgICAgY29uZmlndXJhYmxlOiB0cnVlLFxuICAgICAgICAgICAgICAgIGVudW1lcmFibGU6IHRydWUsXG4gICAgICAgICAgICAgICAgZ2V0KCkge1xuICAgICAgICAgICAgICAgICAgcmV0dXJuIG51bGw7XG4gICAgICAgICAgICAgICAgfSxcbiAgICAgICAgICAgICAgICBzZXQodmFsdWUpIHtcbiAgICAgICAgICAgICAgICAgIGlmICh2YWx1ZSAhPT0gZW1wdHlTdGF0ZVZhbHVlKSB7XG4gICAgICAgICAgICAgICAgICAgIE9iamVjdC5kZWZpbmVQcm9wZXJ0eSh0aGlzLCAnc3RhdGUnLCB7XG4gICAgICAgICAgICAgICAgICAgICAgY29uZmlndXJhYmxlOiB0cnVlLFxuICAgICAgICAgICAgICAgICAgICAgIGVudW1lcmFibGU6IHRydWUsXG4gICAgICAgICAgICAgICAgICAgICAgdmFsdWUsXG4gICAgICAgICAgICAgICAgICAgICAgd3JpdGFibGU6IHRydWUsXG4gICAgICAgICAgICAgICAgICAgIH0pO1xuICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgcmV0dXJuIHRydWU7XG4gICAgICAgICAgICAgICAgfSxcbiAgICAgICAgICAgICAgfSk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgfVxuICAgICAgICAgIHJldHVybiB3aXRoU2V0U3RhdGVBbGxvd2VkKCgpID0+IHJlbmRlckVsZW1lbnQocmVuZGVyZWRFbCwgY29udGV4dCkpO1xuICAgICAgICB9XG4gICAgICB9LFxuICAgICAgdW5tb3VudCgpIHtcbiAgICAgICAgcmVuZGVyZXIudW5tb3VudCgpO1xuICAgICAgfSxcbiAgICAgIGdldE5vZGUoKSB7XG4gICAgICAgIGlmIChpc0RPTSkge1xuICAgICAgICAgIHJldHVybiBlbGVtZW50VG9UcmVlKGNhY2hlZE5vZGUpO1xuICAgICAgICB9XG4gICAgICAgIGNvbnN0IG91dHB1dCA9IHJlbmRlcmVyLmdldFJlbmRlck91dHB1dCgpO1xuICAgICAgICByZXR1cm4ge1xuICAgICAgICAgIG5vZGVUeXBlOiBub2RlVHlwZUZyb21UeXBlKGNhY2hlZE5vZGUudHlwZSksXG4gICAgICAgICAgdHlwZTogY2FjaGVkTm9kZS50eXBlLFxuICAgICAgICAgIHByb3BzOiBjYWNoZWROb2RlLnByb3BzLFxuICAgICAgICAgIGtleTogZW5zdXJlS2V5T3JVbmRlZmluZWQoY2FjaGVkTm9kZS5rZXkpLFxuICAgICAgICAgIHJlZjogY2FjaGVkTm9kZS5yZWYsXG4gICAgICAgICAgaW5zdGFuY2U6IHJlbmRlcmVyLl9pbnN0YW5jZSxcbiAgICAgICAgICByZW5kZXJlZDogQXJyYXkuaXNBcnJheShvdXRwdXQpXG4gICAgICAgICAgICA/IGZsYXR0ZW4ob3V0cHV0KS5tYXAoKGVsKSA9PiBlbGVtZW50VG9UcmVlKGVsKSlcbiAgICAgICAgICAgIDogZWxlbWVudFRvVHJlZShvdXRwdXQpLFxuICAgICAgICB9O1xuICAgICAgfSxcbiAgICAgIHNpbXVsYXRlRXJyb3Iobm9kZUhpZXJhcmNoeSwgcm9vdE5vZGUsIGVycm9yKSB7XG4gICAgICAgIHNpbXVsYXRlRXJyb3IoXG4gICAgICAgICAgZXJyb3IsXG4gICAgICAgICAgcmVuZGVyZXIuX2luc3RhbmNlLFxuICAgICAgICAgIGNhY2hlZE5vZGUsXG4gICAgICAgICAgbm9kZUhpZXJhcmNoeS5jb25jYXQoY2FjaGVkTm9kZSksXG4gICAgICAgICAgbm9kZVR5cGVGcm9tVHlwZSxcbiAgICAgICAgICBhZGFwdGVyLmRpc3BsYXlOYW1lT2ZOb2RlLFxuICAgICAgICAgIGlzMTY2ID8gY2FjaGVkTm9kZS50eXBlIDogdW5kZWZpbmVkLFxuICAgICAgICApO1xuICAgICAgfSxcbiAgICAgIHNpbXVsYXRlRXZlbnQobm9kZSwgZXZlbnQsIC4uLmFyZ3MpIHtcbiAgICAgICAgY29uc3QgaGFuZGxlciA9IG5vZGUucHJvcHNbcHJvcEZyb21FdmVudChldmVudCwgZXZlbnRPcHRpb25zKV07XG4gICAgICAgIGlmIChoYW5kbGVyKSB7XG4gICAgICAgICAgd2l0aFNldFN0YXRlQWxsb3dlZCgoKSA9PiB7XG4gICAgICAgICAgICAvLyBUT0RPKGxtcik6IGNyZWF0ZS91c2Ugc3ludGhldGljIGV2ZW50c1xuICAgICAgICAgICAgLy8gVE9ETyhsbXIpOiBlbXVsYXRlIFJlYWN0J3MgZXZlbnQgcHJvcGFnYXRpb25cbiAgICAgICAgICAgIC8vIFJlYWN0RE9NLnVuc3RhYmxlX2JhdGNoZWRVcGRhdGVzKCgpID0+IHtcbiAgICAgICAgICAgIGhhbmRsZXIoLi4uYXJncyk7XG4gICAgICAgICAgICAvLyB9KTtcbiAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgICAgfSxcbiAgICAgIGJhdGNoZWRVcGRhdGVzKGZuKSB7XG4gICAgICAgIHJldHVybiBmbigpO1xuICAgICAgICAvLyByZXR1cm4gUmVhY3RET00udW5zdGFibGVfYmF0Y2hlZFVwZGF0ZXMoZm4pO1xuICAgICAgfSxcbiAgICAgIGNoZWNrUHJvcFR5cGVzKHR5cGVTcGVjcywgdmFsdWVzLCBsb2NhdGlvbiwgaGllcmFyY2h5KSB7XG4gICAgICAgIHJldHVybiBjaGVja1Byb3BUeXBlcyhcbiAgICAgICAgICB0eXBlU3BlY3MsXG4gICAgICAgICAgdmFsdWVzLFxuICAgICAgICAgIGxvY2F0aW9uLFxuICAgICAgICAgIGRpc3BsYXlOYW1lT2ZOb2RlKGNhY2hlZE5vZGUpLFxuICAgICAgICAgICgpID0+IGdldENvbXBvbmVudFN0YWNrKGhpZXJhcmNoeS5jb25jYXQoW2NhY2hlZE5vZGVdKSksXG4gICAgICAgICk7XG4gICAgICB9LFxuICAgIH07XG4gIH1cblxuICBjcmVhdGVTdHJpbmdSZW5kZXJlcihvcHRpb25zKSB7XG4gICAgaWYgKGhhcyhvcHRpb25zLCAnc3VzcGVuc2VGYWxsYmFjaycpKSB7XG4gICAgICB0aHJvdyBuZXcgVHlwZUVycm9yKCdgc3VzcGVuc2VGYWxsYmFja2Agc2hvdWxkIG5vdCBiZSBzcGVjaWZpZWQgaW4gb3B0aW9ucyBvZiBzdHJpbmcgcmVuZGVyZXInKTtcbiAgICB9XG4gICAgcmV0dXJuIHtcbiAgICAgIHJlbmRlcihlbCwgY29udGV4dCkge1xuICAgICAgICBpZiAob3B0aW9ucy5jb250ZXh0ICYmIChlbC50eXBlLmNvbnRleHRUeXBlcyB8fCBvcHRpb25zLmNoaWxkQ29udGV4dFR5cGVzKSkge1xuICAgICAgICAgIGNvbnN0IGNoaWxkQ29udGV4dFR5cGVzID0ge1xuICAgICAgICAgICAgLi4uKGVsLnR5cGUuY29udGV4dFR5cGVzIHx8IHt9KSxcbiAgICAgICAgICAgIC4uLm9wdGlvbnMuY2hpbGRDb250ZXh0VHlwZXMsXG4gICAgICAgICAgfTtcbiAgICAgICAgICBjb25zdCBDb250ZXh0V3JhcHBlciA9IGNyZWF0ZVJlbmRlcldyYXBwZXIoZWwsIGNvbnRleHQsIGNoaWxkQ29udGV4dFR5cGVzKTtcbiAgICAgICAgICByZXR1cm4gUmVhY3RET01TZXJ2ZXIucmVuZGVyVG9TdGF0aWNNYXJrdXAoUmVhY3QuY3JlYXRlRWxlbWVudChDb250ZXh0V3JhcHBlcikpO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiBSZWFjdERPTVNlcnZlci5yZW5kZXJUb1N0YXRpY01hcmt1cChlbCk7XG4gICAgICB9LFxuICAgIH07XG4gIH1cblxuICAvLyBQcm92aWRlZCBhIGJhZyBvZiBvcHRpb25zLCByZXR1cm4gYW4gYEVuenltZVJlbmRlcmVyYC4gU29tZSBvcHRpb25zIGNhbiBiZSBpbXBsZW1lbnRhdGlvblxuICAvLyBzcGVjaWZpYywgbGlrZSBgYXR0YWNoYCBldGMuIGZvciBSZWFjdCwgYnV0IG5vdCBwYXJ0IG9mIHRoaXMgaW50ZXJmYWNlIGV4cGxpY2l0bHkuXG4gIC8vIGVzbGludC1kaXNhYmxlLW5leHQtbGluZSBjbGFzcy1tZXRob2RzLXVzZS10aGlzXG4gIGNyZWF0ZVJlbmRlcmVyKG9wdGlvbnMpIHtcbiAgICBzd2l0Y2ggKG9wdGlvbnMubW9kZSkge1xuICAgICAgY2FzZSBFbnp5bWVBZGFwdGVyLk1PREVTLk1PVU5UOiByZXR1cm4gdGhpcy5jcmVhdGVNb3VudFJlbmRlcmVyKG9wdGlvbnMpO1xuICAgICAgY2FzZSBFbnp5bWVBZGFwdGVyLk1PREVTLlNIQUxMT1c6IHJldHVybiB0aGlzLmNyZWF0ZVNoYWxsb3dSZW5kZXJlcihvcHRpb25zKTtcbiAgICAgIGNhc2UgRW56eW1lQWRhcHRlci5NT0RFUy5TVFJJTkc6IHJldHVybiB0aGlzLmNyZWF0ZVN0cmluZ1JlbmRlcmVyKG9wdGlvbnMpO1xuICAgICAgZGVmYXVsdDpcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKGBFbnp5bWUgSW50ZXJuYWwgRXJyb3I6IFVucmVjb2duaXplZCBtb2RlOiAke29wdGlvbnMubW9kZX1gKTtcbiAgICB9XG4gIH1cblxuICB3cmFwKGVsZW1lbnQpIHtcbiAgICByZXR1cm4gd3JhcChlbGVtZW50KTtcbiAgfVxuXG4gIC8vIGNvbnZlcnRzIGFuIFJTVE5vZGUgdG8gdGhlIGNvcnJlc3BvbmRpbmcgSlNYIFByYWdtYSBFbGVtZW50LiBUaGlzIHdpbGwgYmUgbmVlZGVkXG4gIC8vIGluIG9yZGVyIHRvIGltcGxlbWVudCB0aGUgYFdyYXBwZXIubW91bnQoKWAgYW5kIGBXcmFwcGVyLnNoYWxsb3coKWAgbWV0aG9kcywgYnV0IHNob3VsZFxuICAvLyBiZSBwcmV0dHkgc3RyYWlnaHRmb3J3YXJkIGZvciBwZW9wbGUgdG8gaW1wbGVtZW50LlxuICAvLyBlc2xpbnQtZGlzYWJsZS1uZXh0LWxpbmUgY2xhc3MtbWV0aG9kcy11c2UtdGhpc1xuICBub2RlVG9FbGVtZW50KG5vZGUpIHtcbiAgICBpZiAoIW5vZGUgfHwgdHlwZW9mIG5vZGUgIT09ICdvYmplY3QnKSByZXR1cm4gbnVsbDtcbiAgICBjb25zdCB7IHR5cGUgfSA9IG5vZGU7XG4gICAgcmV0dXJuIFJlYWN0LmNyZWF0ZUVsZW1lbnQodW5tZW1vVHlwZSh0eXBlKSwgcHJvcHNXaXRoS2V5c0FuZFJlZihub2RlKSk7XG4gIH1cblxuICAvLyBlc2xpbnQtZGlzYWJsZS1uZXh0LWxpbmUgY2xhc3MtbWV0aG9kcy11c2UtdGhpc1xuICBtYXRjaGVzRWxlbWVudFR5cGUobm9kZSwgbWF0Y2hpbmdUeXBlKSB7XG4gICAgaWYgKCFub2RlKSB7XG4gICAgICByZXR1cm4gbm9kZTtcbiAgICB9XG4gICAgY29uc3QgeyB0eXBlIH0gPSBub2RlO1xuICAgIHJldHVybiB1bm1lbW9UeXBlKHR5cGUpID09PSB1bm1lbW9UeXBlKG1hdGNoaW5nVHlwZSk7XG4gIH1cblxuICBlbGVtZW50VG9Ob2RlKGVsZW1lbnQpIHtcbiAgICByZXR1cm4gZWxlbWVudFRvVHJlZShlbGVtZW50KTtcbiAgfVxuXG4gIG5vZGVUb0hvc3ROb2RlKG5vZGUsIHN1cHBvcnRzQXJyYXkgPSBmYWxzZSkge1xuICAgIGNvbnN0IG5vZGVzID0gbm9kZVRvSG9zdE5vZGUobm9kZSk7XG4gICAgaWYgKEFycmF5LmlzQXJyYXkobm9kZXMpICYmICFzdXBwb3J0c0FycmF5KSB7XG4gICAgICByZXR1cm4gbm9kZXNbMF07XG4gICAgfVxuICAgIHJldHVybiBub2RlcztcbiAgfVxuXG4gIGRpc3BsYXlOYW1lT2ZOb2RlKG5vZGUpIHtcbiAgICBpZiAoIW5vZGUpIHJldHVybiBudWxsO1xuICAgIGNvbnN0IHsgdHlwZSwgJCR0eXBlb2YgfSA9IG5vZGU7XG4gICAgY29uc3QgYWRhcHRlciA9IHRoaXM7XG5cbiAgICBjb25zdCBub2RlVHlwZSA9IHR5cGUgfHwgJCR0eXBlb2Y7XG5cbiAgICAvLyBuZXdlciBub2RlIHR5cGVzIG1heSBiZSB1bmRlZmluZWQsIHNvIG9ubHkgdGVzdCBpZiB0aGUgbm9kZVR5cGUgZXhpc3RzXG4gICAgaWYgKG5vZGVUeXBlKSB7XG4gICAgICBzd2l0Y2ggKG5vZGVUeXBlKSB7XG4gICAgICAgIGNhc2UgKGlzMTY2ID8gQ29uY3VycmVudE1vZGUgOiBBc3luY01vZGUpIHx8IE5hTjogcmV0dXJuIGlzMTY2ID8gJ0NvbmN1cnJlbnRNb2RlJyA6ICdBc3luY01vZGUnO1xuICAgICAgICBjYXNlIEZyYWdtZW50IHx8IE5hTjogcmV0dXJuICdGcmFnbWVudCc7XG4gICAgICAgIGNhc2UgU3RyaWN0TW9kZSB8fCBOYU46IHJldHVybiAnU3RyaWN0TW9kZSc7XG4gICAgICAgIGNhc2UgUHJvZmlsZXIgfHwgTmFOOiByZXR1cm4gJ1Byb2ZpbGVyJztcbiAgICAgICAgY2FzZSBQb3J0YWwgfHwgTmFOOiByZXR1cm4gJ1BvcnRhbCc7XG4gICAgICAgIGNhc2UgU3VzcGVuc2UgfHwgTmFOOiByZXR1cm4gJ1N1c3BlbnNlJztcbiAgICAgICAgZGVmYXVsdDpcbiAgICAgIH1cbiAgICB9XG5cbiAgICBjb25zdCAkJHR5cGVvZlR5cGUgPSB0eXBlICYmIHR5cGUuJCR0eXBlb2Y7XG5cbiAgICBzd2l0Y2ggKCQkdHlwZW9mVHlwZSkge1xuICAgICAgY2FzZSBDb250ZXh0Q29uc3VtZXIgfHwgTmFOOiByZXR1cm4gJ0NvbnRleHRDb25zdW1lcic7XG4gICAgICBjYXNlIENvbnRleHRQcm92aWRlciB8fCBOYU46IHJldHVybiAnQ29udGV4dFByb3ZpZGVyJztcbiAgICAgIGNhc2UgTWVtbyB8fCBOYU46IHtcbiAgICAgICAgY29uc3Qgbm9kZU5hbWUgPSBkaXNwbGF5TmFtZU9mTm9kZShub2RlKTtcbiAgICAgICAgcmV0dXJuIHR5cGVvZiBub2RlTmFtZSA9PT0gJ3N0cmluZycgPyBub2RlTmFtZSA6IGBNZW1vKCR7YWRhcHRlci5kaXNwbGF5TmFtZU9mTm9kZSh0eXBlKX0pYDtcbiAgICAgIH1cbiAgICAgIGNhc2UgRm9yd2FyZFJlZiB8fCBOYU46IHtcbiAgICAgICAgaWYgKHR5cGUuZGlzcGxheU5hbWUpIHtcbiAgICAgICAgICByZXR1cm4gdHlwZS5kaXNwbGF5TmFtZTtcbiAgICAgICAgfVxuICAgICAgICBjb25zdCBuYW1lID0gYWRhcHRlci5kaXNwbGF5TmFtZU9mTm9kZSh7IHR5cGU6IHR5cGUucmVuZGVyIH0pO1xuICAgICAgICByZXR1cm4gbmFtZSA/IGBGb3J3YXJkUmVmKCR7bmFtZX0pYCA6ICdGb3J3YXJkUmVmJztcbiAgICAgIH1cbiAgICAgIGNhc2UgTGF6eSB8fCBOYU46IHtcbiAgICAgICAgcmV0dXJuICdsYXp5JztcbiAgICAgIH1cbiAgICAgIGRlZmF1bHQ6IHJldHVybiBkaXNwbGF5TmFtZU9mTm9kZShub2RlKTtcbiAgICB9XG4gIH1cblxuICBpc1ZhbGlkRWxlbWVudChlbGVtZW50KSB7XG4gICAgcmV0dXJuIGlzRWxlbWVudChlbGVtZW50KTtcbiAgfVxuXG4gIGlzVmFsaWRFbGVtZW50VHlwZShvYmplY3QpIHtcbiAgICByZXR1cm4gISFvYmplY3QgJiYgaXNWYWxpZEVsZW1lbnRUeXBlKG9iamVjdCk7XG4gIH1cblxuICBpc0ZyYWdtZW50KGZyYWdtZW50KSB7XG4gICAgcmV0dXJuIHR5cGVPZk5vZGUoZnJhZ21lbnQpID09PSBGcmFnbWVudDtcbiAgfVxuXG4gIGlzQ3VzdG9tQ29tcG9uZW50KHR5cGUpIHtcbiAgICBjb25zdCBmYWtlRWxlbWVudCA9IG1ha2VGYWtlRWxlbWVudCh0eXBlKTtcbiAgICByZXR1cm4gISF0eXBlICYmIChcbiAgICAgIHR5cGVvZiB0eXBlID09PSAnZnVuY3Rpb24nXG4gICAgICB8fCBpc0ZvcndhcmRSZWYoZmFrZUVsZW1lbnQpXG4gICAgICB8fCBpc0NvbnRleHRQcm92aWRlcihmYWtlRWxlbWVudClcbiAgICAgIHx8IGlzQ29udGV4dENvbnN1bWVyKGZha2VFbGVtZW50KVxuICAgICAgfHwgaXNTdXNwZW5zZShmYWtlRWxlbWVudClcbiAgICApO1xuICB9XG5cbiAgaXNDb250ZXh0Q29uc3VtZXIodHlwZSkge1xuICAgIHJldHVybiAhIXR5cGUgJiYgaXNDb250ZXh0Q29uc3VtZXIobWFrZUZha2VFbGVtZW50KHR5cGUpKTtcbiAgfVxuXG4gIGlzQ3VzdG9tQ29tcG9uZW50RWxlbWVudChpbnN0KSB7XG4gICAgaWYgKCFpbnN0IHx8ICF0aGlzLmlzVmFsaWRFbGVtZW50KGluc3QpKSB7XG4gICAgICByZXR1cm4gZmFsc2U7XG4gICAgfVxuICAgIHJldHVybiB0aGlzLmlzQ3VzdG9tQ29tcG9uZW50KGluc3QudHlwZSk7XG4gIH1cblxuICBnZXRQcm92aWRlckZyb21Db25zdW1lcihDb25zdW1lcikge1xuICAgIC8vIFJlYWN0IHN0b3JlcyByZWZlcmVuY2VzIHRvIHRoZSBQcm92aWRlciBvbiBhIENvbnN1bWVyIGRpZmZlcmVudGx5IGFjcm9zcyB2ZXJzaW9ucy5cbiAgICBpZiAoQ29uc3VtZXIpIHtcbiAgICAgIGxldCBQcm92aWRlcjtcbiAgICAgIGlmIChDb25zdW1lci5fY29udGV4dCkgeyAvLyBjaGVjayB0aGlzIGZpcnN0LCB0byBhdm9pZCBhIGRlcHJlY2F0aW9uIHdhcm5pbmdcbiAgICAgICAgKHsgUHJvdmlkZXIgfSA9IENvbnN1bWVyLl9jb250ZXh0KTtcbiAgICAgIH0gZWxzZSBpZiAoQ29uc3VtZXIuUHJvdmlkZXIpIHtcbiAgICAgICAgKHsgUHJvdmlkZXIgfSA9IENvbnN1bWVyKTtcbiAgICAgIH1cbiAgICAgIGlmIChQcm92aWRlcikge1xuICAgICAgICByZXR1cm4gUHJvdmlkZXI7XG4gICAgICB9XG4gICAgfVxuICAgIHRocm93IG5ldyBFcnJvcignRW56eW1lIEludGVybmFsIEVycm9yOiBjYW7igJl0IGZpZ3VyZSBvdXQgaG93IHRvIGdldCBQcm92aWRlciBmcm9tIENvbnN1bWVyJyk7XG4gIH1cblxuICBjcmVhdGVFbGVtZW50KC4uLmFyZ3MpIHtcbiAgICByZXR1cm4gUmVhY3QuY3JlYXRlRWxlbWVudCguLi5hcmdzKTtcbiAgfVxuXG4gIHdyYXBXaXRoV3JhcHBpbmdDb21wb25lbnQobm9kZSwgb3B0aW9ucykge1xuICAgIHJldHVybiB7XG4gICAgICBSb290RmluZGVyLFxuICAgICAgbm9kZTogd3JhcFdpdGhXcmFwcGluZ0NvbXBvbmVudChSZWFjdC5jcmVhdGVFbGVtZW50LCBub2RlLCBvcHRpb25zKSxcbiAgICB9O1xuICB9XG59XG5cbm1vZHVsZS5leHBvcnRzID0gUmVhY3RTaXh0ZWVuQWRhcHRlcjtcbiJdfQ==
//# sourceMappingURL=ReactSixteenAdapter.js.map