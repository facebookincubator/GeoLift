"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.mapNativeEventNames = mapNativeEventNames;
exports.propFromEvent = propFromEvent;
exports.withSetStateAllowed = withSetStateAllowed;
exports.assertDomAvailable = assertDomAvailable;
exports.displayNameOfNode = displayNameOfNode;
exports.nodeTypeFromType = nodeTypeFromType;
exports.isArrayLike = isArrayLike;
exports.flatten = flatten;
exports.ensureKeyOrUndefined = ensureKeyOrUndefined;
exports.elementToTree = elementToTree;
exports.findElement = findElement;
exports.propsWithKeysAndRef = propsWithKeysAndRef;
exports.getComponentStack = getComponentStack;
exports.simulateError = simulateError;
exports.getMaskedContext = getMaskedContext;
exports.getNodeFromRootFinder = getNodeFromRootFinder;
exports.wrapWithWrappingComponent = wrapWithWrappingComponent;
exports.getWrappingComponentMountRenderer = getWrappingComponentMountRenderer;
exports.fakeDynamicImport = fakeDynamicImport;
exports.compareNodeTypeOf = compareNodeTypeOf;
exports.spyMethod = spyMethod;
exports.spyProperty = spyProperty;
Object.defineProperty(exports, "createMountWrapper", {
  enumerable: true,
  get: function get() {
    return _createMountWrapper["default"];
  }
});
Object.defineProperty(exports, "createRenderWrapper", {
  enumerable: true,
  get: function get() {
    return _createRenderWrapper["default"];
  }
});
Object.defineProperty(exports, "wrap", {
  enumerable: true,
  get: function get() {
    return _wrapWithSimpleWrapper["default"];
  }
});
Object.defineProperty(exports, "RootFinder", {
  enumerable: true,
  get: function get() {
    return _RootFinder["default"];
  }
});

var _functionPrototype = _interopRequireDefault(require("function.prototype.name"));

var _object = _interopRequireDefault(require("object.fromentries"));

var _has = _interopRequireDefault(require("has"));

var _createMountWrapper = _interopRequireDefault(require("./createMountWrapper"));

var _createRenderWrapper = _interopRequireDefault(require("./createRenderWrapper"));

var _wrapWithSimpleWrapper = _interopRequireDefault(require("./wrapWithSimpleWrapper"));

var _RootFinder = _interopRequireDefault(require("./RootFinder"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { "default": obj }; }

function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _iterableToArrayLimit(arr, i) { if (typeof Symbol === "undefined" || !(Symbol.iterator in Object(arr))) return; var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function mapNativeEventNames(event) {
  var _ref = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
      _ref$animation = _ref.animation,
      animation = _ref$animation === void 0 ? false : _ref$animation,
      _ref$pointerEvents = _ref.pointerEvents,
      pointerEvents = _ref$pointerEvents === void 0 ? false : _ref$pointerEvents,
      _ref$auxClick = _ref.auxClick,
      auxClick = _ref$auxClick === void 0 ? false : _ref$auxClick;

  var nativeToReactEventMap = _objectSpread(_objectSpread(_objectSpread({
    compositionend: 'compositionEnd',
    compositionstart: 'compositionStart',
    compositionupdate: 'compositionUpdate',
    keydown: 'keyDown',
    keyup: 'keyUp',
    keypress: 'keyPress',
    contextmenu: 'contextMenu',
    dblclick: 'doubleClick',
    doubleclick: 'doubleClick',
    // kept for legacy. TODO: remove with next major.
    dragend: 'dragEnd',
    dragenter: 'dragEnter',
    dragexist: 'dragExit',
    dragleave: 'dragLeave',
    dragover: 'dragOver',
    dragstart: 'dragStart',
    mousedown: 'mouseDown',
    mousemove: 'mouseMove',
    mouseout: 'mouseOut',
    mouseover: 'mouseOver',
    mouseup: 'mouseUp',
    touchcancel: 'touchCancel',
    touchend: 'touchEnd',
    touchmove: 'touchMove',
    touchstart: 'touchStart',
    canplay: 'canPlay',
    canplaythrough: 'canPlayThrough',
    durationchange: 'durationChange',
    loadeddata: 'loadedData',
    loadedmetadata: 'loadedMetadata',
    loadstart: 'loadStart',
    ratechange: 'rateChange',
    timeupdate: 'timeUpdate',
    volumechange: 'volumeChange',
    beforeinput: 'beforeInput',
    mouseenter: 'mouseEnter',
    mouseleave: 'mouseLeave',
    transitionend: 'transitionEnd'
  }, animation && {
    animationstart: 'animationStart',
    animationiteration: 'animationIteration',
    animationend: 'animationEnd'
  }), pointerEvents && {
    pointerdown: 'pointerDown',
    pointermove: 'pointerMove',
    pointerup: 'pointerUp',
    pointercancel: 'pointerCancel',
    gotpointercapture: 'gotPointerCapture',
    lostpointercapture: 'lostPointerCapture',
    pointerenter: 'pointerEnter',
    pointerleave: 'pointerLeave',
    pointerover: 'pointerOver',
    pointerout: 'pointerOut'
  }), auxClick && {
    auxclick: 'auxClick'
  });

  return nativeToReactEventMap[event] || event;
} // 'click' => 'onClick'
// 'mouseEnter' => 'onMouseEnter'


function propFromEvent(event) {
  var eventOptions = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
  var nativeEvent = mapNativeEventNames(event, eventOptions);
  return "on".concat(nativeEvent[0].toUpperCase()).concat(nativeEvent.slice(1));
}

function withSetStateAllowed(fn) {
  // NOTE(lmr):
  // this is currently here to circumvent a React bug where `setState()` is
  // not allowed without global being defined.
  var cleanup = false;

  if (typeof global.document === 'undefined') {
    cleanup = true;
    global.document = {};
  }

  var result = fn();

  if (cleanup) {
    // This works around a bug in node/jest in that developers aren't able to
    // delete things from global when running in a node vm.
    global.document = undefined;
    delete global.document;
  }

  return result;
}

function assertDomAvailable(feature) {
  if (!global || !global.document || !global.document.createElement) {
    throw new Error("Enzyme's ".concat(feature, " expects a DOM environment to be loaded, but found none"));
  }
}

function displayNameOfNode(node) {
  if (!node) return null;
  var type = node.type;
  if (!type) return null;
  return type.displayName || (typeof type === 'function' ? (0, _functionPrototype["default"])(type) : type.name || type);
}

function nodeTypeFromType(type) {
  if (typeof type === 'string') {
    return 'host';
  }

  if (type && type.prototype && type.prototype.isReactComponent) {
    return 'class';
  }

  return 'function';
}

function getIteratorFn(obj) {
  var iteratorFn = obj && (typeof Symbol === 'function' && _typeof(Symbol.iterator) === 'symbol' && obj[Symbol.iterator] || obj['@@iterator']);

  if (typeof iteratorFn === 'function') {
    return iteratorFn;
  }

  return undefined;
}

function isIterable(obj) {
  return !!getIteratorFn(obj);
}

function isArrayLike(obj) {
  return Array.isArray(obj) || typeof obj !== 'string' && isIterable(obj);
}

function flatten(arrs) {
  // optimize for the most common case
  if (Array.isArray(arrs)) {
    return arrs.reduce(function (flatArrs, item) {
      return flatArrs.concat(isArrayLike(item) ? flatten(item) : item);
    }, []);
  } // fallback for arbitrary iterable children


  var flatArrs = [];
  var iteratorFn = getIteratorFn(arrs);
  var iterator = iteratorFn.call(arrs);
  var step = iterator.next();

  while (!step.done) {
    var item = step.value;
    var flatItem = void 0;

    if (isArrayLike(item)) {
      flatItem = flatten(item);
    } else {
      flatItem = item;
    }

    flatArrs = flatArrs.concat(flatItem);
    step = iterator.next();
  }

  return flatArrs;
}

function ensureKeyOrUndefined(key) {
  return key || (key === '' ? '' : undefined);
}

function elementToTree(el) {
  var recurse = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : elementToTree;

  if (typeof recurse !== 'function' && arguments.length === 3) {
    // special case for backwards compat for `.map(elementToTree)`
    recurse = elementToTree; // eslint-disable-line no-param-reassign
  }

  if (el === null || _typeof(el) !== 'object' || !('type' in el)) {
    return el;
  }

  var type = el.type,
      props = el.props,
      key = el.key,
      ref = el.ref;
  var children = props.children;
  var rendered = null;

  if (isArrayLike(children)) {
    rendered = flatten(children).map(function (x) {
      return recurse(x);
    });
  } else if (typeof children !== 'undefined') {
    rendered = recurse(children);
  }

  var nodeType = nodeTypeFromType(type);

  if (nodeType === 'host' && props.dangerouslySetInnerHTML) {
    if (props.children != null) {
      var error = new Error('Can only set one of `children` or `props.dangerouslySetInnerHTML`.');
      error.name = 'Invariant Violation';
      throw error;
    }
  }

  return {
    nodeType: nodeType,
    type: type,
    props: props,
    key: ensureKeyOrUndefined(key),
    ref: ref,
    instance: null,
    rendered: rendered
  };
}

function mapFind(arraylike, mapper, finder) {
  var found;
  var isFound = Array.prototype.find.call(arraylike, function (item) {
    found = mapper(item);
    return finder(found);
  });
  return isFound ? found : undefined;
}

function findElement(el, predicate) {
  if (el === null || _typeof(el) !== 'object' || !('type' in el)) {
    return undefined;
  }

  if (predicate(el)) {
    return el;
  }

  var rendered = el.rendered;

  if (isArrayLike(rendered)) {
    return mapFind(rendered, function (x) {
      return findElement(x, predicate);
    }, function (x) {
      return typeof x !== 'undefined';
    });
  }

  return findElement(rendered, predicate);
}

function propsWithKeysAndRef(node) {
  if (node.ref !== null || node.key !== null) {
    return _objectSpread(_objectSpread({}, node.props), {}, {
      key: node.key,
      ref: node.ref
    });
  }

  return node.props;
}

function getComponentStack(hierarchy) {
  var getNodeType = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : nodeTypeFromType;
  var getDisplayName = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : displayNameOfNode;
  var tuples = hierarchy.filter(function (node) {
    return node.type !== _RootFinder["default"];
  }).map(function (x) {
    return [getNodeType(x.type), getDisplayName(x)];
  }).concat([['class', 'WrapperComponent']]);
  return tuples.map(function (_ref2, i, arr) {
    var _ref3 = _slicedToArray(_ref2, 2),
        name = _ref3[1];

    var _ref4 = arr.slice(i + 1).find(function (_ref6) {
      var _ref7 = _slicedToArray(_ref6, 1),
          nodeType = _ref7[0];

      return nodeType !== 'host';
    }) || [],
        _ref5 = _slicedToArray(_ref4, 2),
        closestComponent = _ref5[1];

    return "\n    in ".concat(name).concat(closestComponent ? " (created by ".concat(closestComponent, ")") : '');
  }).join('');
}

function simulateError(error, catchingInstance, rootNode, // TODO: remove `rootNode` next semver-major
hierarchy) {
  var getNodeType = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : nodeTypeFromType;
  var getDisplayName = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : displayNameOfNode;
  var catchingType = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : {};
  var instance = catchingInstance || {};
  var componentDidCatch = instance.componentDidCatch;
  var getDerivedStateFromError = catchingType.getDerivedStateFromError;

  if (!componentDidCatch && !getDerivedStateFromError) {
    throw error;
  }

  if (getDerivedStateFromError) {
    var stateUpdate = getDerivedStateFromError.call(catchingType, error);
    instance.setState(stateUpdate);
  }

  if (componentDidCatch) {
    var componentStack = getComponentStack(hierarchy, getNodeType, getDisplayName);
    componentDidCatch.call(instance, error, {
      componentStack: componentStack
    });
  }
}

function getMaskedContext(contextTypes, unmaskedContext) {
  if (!contextTypes || !unmaskedContext) {
    return {};
  }

  return (0, _object["default"])(Object.keys(contextTypes).map(function (key) {
    return [key, unmaskedContext[key]];
  }));
}

function getNodeFromRootFinder(isCustomComponent, tree, options) {
  if (!isCustomComponent(options.wrappingComponent)) {
    return tree.rendered;
  }

  var rootFinder = findElement(tree, function (node) {
    return node.type === _RootFinder["default"];
  });

  if (!rootFinder) {
    throw new Error('`wrappingComponent` must render its children!');
  }

  return rootFinder.rendered;
}

function wrapWithWrappingComponent(createElement, node, options) {
  var wrappingComponent = options.wrappingComponent,
      wrappingComponentProps = options.wrappingComponentProps;

  if (!wrappingComponent) {
    return node;
  }

  return createElement(wrappingComponent, wrappingComponentProps, createElement(_RootFinder["default"], null, node));
}

function getWrappingComponentMountRenderer(_ref8) {
  var toTree = _ref8.toTree,
      getMountWrapperInstance = _ref8.getMountWrapperInstance;
  return {
    getNode: function getNode() {
      var instance = getMountWrapperInstance();
      return instance ? toTree(instance).rendered : null;
    },
    render: function render(el, context, callback) {
      var instance = getMountWrapperInstance();

      if (!instance) {
        throw new Error('The wrapping component may not be updated if the root is unmounted.');
      }

      return instance.setWrappingComponentProps(el.props, callback);
    }
  };
}

function fakeDynamicImport(moduleToImport) {
  return Promise.resolve({
    "default": moduleToImport
  });
}

function compareNodeTypeOf(node, matchingTypeOf) {
  if (!node) {
    return false;
  }

  return node.$$typeof === matchingTypeOf;
} // TODO: when enzyme v3.12.0 is required, delete this


function spyMethod(instance, methodName) {
  var getStub = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : function () {};
  var lastReturnValue;
  var originalMethod = instance[methodName];
  var hasOwn = (0, _has["default"])(instance, methodName);
  var descriptor;

  if (hasOwn) {
    descriptor = Object.getOwnPropertyDescriptor(instance, methodName);
  }

  Object.defineProperty(instance, methodName, {
    configurable: true,
    enumerable: !descriptor || !!descriptor.enumerable,
    value: getStub(originalMethod) || function spied() {
      for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }

      var result = originalMethod.apply(this, args);
      lastReturnValue = result;
      return result;
    }
  });
  return {
    restore: function restore() {
      if (hasOwn) {
        if (descriptor) {
          Object.defineProperty(instance, methodName, descriptor);
        } else {
          /* eslint-disable no-param-reassign */
          instance[methodName] = originalMethod;
          /* eslint-enable no-param-reassign */
        }
      } else {
        /* eslint-disable no-param-reassign */
        delete instance[methodName];
        /* eslint-enable no-param-reassign */
      }
    },
    getLastReturnValue: function getLastReturnValue() {
      return lastReturnValue;
    }
  };
} // TODO: when enzyme v3.12.0 is required, delete this


function spyProperty(instance, propertyName) {
  var handlers = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {};
  var originalValue = instance[propertyName];
  var hasOwn = (0, _has["default"])(instance, propertyName);
  var descriptor;

  if (hasOwn) {
    descriptor = Object.getOwnPropertyDescriptor(instance, propertyName);
  }

  var _wasAssigned = false;
  var holder = originalValue;
  var getV = handlers.get ? function () {
    var value = descriptor && descriptor.get ? descriptor.get.call(instance) : holder;
    return handlers.get.call(instance, value);
  } : function () {
    return holder;
  };
  var set = handlers.set ? function (newValue) {
    _wasAssigned = true;
    var handlerNewValue = handlers.set.call(instance, holder, newValue);
    holder = handlerNewValue;

    if (descriptor && descriptor.set) {
      descriptor.set.call(instance, holder);
    }
  } : function (v) {
    _wasAssigned = true;
    holder = v;
  };
  Object.defineProperty(instance, propertyName, {
    configurable: true,
    enumerable: !descriptor || !!descriptor.enumerable,
    get: getV,
    set: set
  });
  return {
    restore: function restore() {
      if (hasOwn) {
        if (descriptor) {
          Object.defineProperty(instance, propertyName, descriptor);
        } else {
          /* eslint-disable no-param-reassign */
          instance[propertyName] = holder;
          /* eslint-enable no-param-reassign */
        }
      } else {
        /* eslint-disable no-param-reassign */
        delete instance[propertyName];
        /* eslint-enable no-param-reassign */
      }
    },
    wasAssigned: function wasAssigned() {
      return _wasAssigned;
    }
  };
}
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9VdGlscy5qcyJdLCJuYW1lcyI6WyJtYXBOYXRpdmVFdmVudE5hbWVzIiwiZXZlbnQiLCJhbmltYXRpb24iLCJwb2ludGVyRXZlbnRzIiwiYXV4Q2xpY2siLCJuYXRpdmVUb1JlYWN0RXZlbnRNYXAiLCJjb21wb3NpdGlvbmVuZCIsImNvbXBvc2l0aW9uc3RhcnQiLCJjb21wb3NpdGlvbnVwZGF0ZSIsImtleWRvd24iLCJrZXl1cCIsImtleXByZXNzIiwiY29udGV4dG1lbnUiLCJkYmxjbGljayIsImRvdWJsZWNsaWNrIiwiZHJhZ2VuZCIsImRyYWdlbnRlciIsImRyYWdleGlzdCIsImRyYWdsZWF2ZSIsImRyYWdvdmVyIiwiZHJhZ3N0YXJ0IiwibW91c2Vkb3duIiwibW91c2Vtb3ZlIiwibW91c2VvdXQiLCJtb3VzZW92ZXIiLCJtb3VzZXVwIiwidG91Y2hjYW5jZWwiLCJ0b3VjaGVuZCIsInRvdWNobW92ZSIsInRvdWNoc3RhcnQiLCJjYW5wbGF5IiwiY2FucGxheXRocm91Z2giLCJkdXJhdGlvbmNoYW5nZSIsImxvYWRlZGRhdGEiLCJsb2FkZWRtZXRhZGF0YSIsImxvYWRzdGFydCIsInJhdGVjaGFuZ2UiLCJ0aW1ldXBkYXRlIiwidm9sdW1lY2hhbmdlIiwiYmVmb3JlaW5wdXQiLCJtb3VzZWVudGVyIiwibW91c2VsZWF2ZSIsInRyYW5zaXRpb25lbmQiLCJhbmltYXRpb25zdGFydCIsImFuaW1hdGlvbml0ZXJhdGlvbiIsImFuaW1hdGlvbmVuZCIsInBvaW50ZXJkb3duIiwicG9pbnRlcm1vdmUiLCJwb2ludGVydXAiLCJwb2ludGVyY2FuY2VsIiwiZ290cG9pbnRlcmNhcHR1cmUiLCJsb3N0cG9pbnRlcmNhcHR1cmUiLCJwb2ludGVyZW50ZXIiLCJwb2ludGVybGVhdmUiLCJwb2ludGVyb3ZlciIsInBvaW50ZXJvdXQiLCJhdXhjbGljayIsInByb3BGcm9tRXZlbnQiLCJldmVudE9wdGlvbnMiLCJuYXRpdmVFdmVudCIsInRvVXBwZXJDYXNlIiwic2xpY2UiLCJ3aXRoU2V0U3RhdGVBbGxvd2VkIiwiZm4iLCJjbGVhbnVwIiwiZ2xvYmFsIiwiZG9jdW1lbnQiLCJyZXN1bHQiLCJ1bmRlZmluZWQiLCJhc3NlcnREb21BdmFpbGFibGUiLCJmZWF0dXJlIiwiY3JlYXRlRWxlbWVudCIsIkVycm9yIiwiZGlzcGxheU5hbWVPZk5vZGUiLCJub2RlIiwidHlwZSIsImRpc3BsYXlOYW1lIiwibmFtZSIsIm5vZGVUeXBlRnJvbVR5cGUiLCJwcm90b3R5cGUiLCJpc1JlYWN0Q29tcG9uZW50IiwiZ2V0SXRlcmF0b3JGbiIsIm9iaiIsIml0ZXJhdG9yRm4iLCJTeW1ib2wiLCJpdGVyYXRvciIsImlzSXRlcmFibGUiLCJpc0FycmF5TGlrZSIsIkFycmF5IiwiaXNBcnJheSIsImZsYXR0ZW4iLCJhcnJzIiwicmVkdWNlIiwiZmxhdEFycnMiLCJpdGVtIiwiY29uY2F0IiwiY2FsbCIsInN0ZXAiLCJuZXh0IiwiZG9uZSIsInZhbHVlIiwiZmxhdEl0ZW0iLCJlbnN1cmVLZXlPclVuZGVmaW5lZCIsImtleSIsImVsZW1lbnRUb1RyZWUiLCJlbCIsInJlY3Vyc2UiLCJhcmd1bWVudHMiLCJsZW5ndGgiLCJwcm9wcyIsInJlZiIsImNoaWxkcmVuIiwicmVuZGVyZWQiLCJtYXAiLCJ4Iiwibm9kZVR5cGUiLCJkYW5nZXJvdXNseVNldElubmVySFRNTCIsImVycm9yIiwiaW5zdGFuY2UiLCJtYXBGaW5kIiwiYXJyYXlsaWtlIiwibWFwcGVyIiwiZmluZGVyIiwiZm91bmQiLCJpc0ZvdW5kIiwiZmluZCIsImZpbmRFbGVtZW50IiwicHJlZGljYXRlIiwicHJvcHNXaXRoS2V5c0FuZFJlZiIsImdldENvbXBvbmVudFN0YWNrIiwiaGllcmFyY2h5IiwiZ2V0Tm9kZVR5cGUiLCJnZXREaXNwbGF5TmFtZSIsInR1cGxlcyIsImZpbHRlciIsIlJvb3RGaW5kZXIiLCJpIiwiYXJyIiwiY2xvc2VzdENvbXBvbmVudCIsImpvaW4iLCJzaW11bGF0ZUVycm9yIiwiY2F0Y2hpbmdJbnN0YW5jZSIsInJvb3ROb2RlIiwiY2F0Y2hpbmdUeXBlIiwiY29tcG9uZW50RGlkQ2F0Y2giLCJnZXREZXJpdmVkU3RhdGVGcm9tRXJyb3IiLCJzdGF0ZVVwZGF0ZSIsInNldFN0YXRlIiwiY29tcG9uZW50U3RhY2siLCJnZXRNYXNrZWRDb250ZXh0IiwiY29udGV4dFR5cGVzIiwidW5tYXNrZWRDb250ZXh0IiwiT2JqZWN0Iiwia2V5cyIsImdldE5vZGVGcm9tUm9vdEZpbmRlciIsImlzQ3VzdG9tQ29tcG9uZW50IiwidHJlZSIsIm9wdGlvbnMiLCJ3cmFwcGluZ0NvbXBvbmVudCIsInJvb3RGaW5kZXIiLCJ3cmFwV2l0aFdyYXBwaW5nQ29tcG9uZW50Iiwid3JhcHBpbmdDb21wb25lbnRQcm9wcyIsImdldFdyYXBwaW5nQ29tcG9uZW50TW91bnRSZW5kZXJlciIsInRvVHJlZSIsImdldE1vdW50V3JhcHBlckluc3RhbmNlIiwiZ2V0Tm9kZSIsInJlbmRlciIsImNvbnRleHQiLCJjYWxsYmFjayIsInNldFdyYXBwaW5nQ29tcG9uZW50UHJvcHMiLCJmYWtlRHluYW1pY0ltcG9ydCIsIm1vZHVsZVRvSW1wb3J0IiwiUHJvbWlzZSIsInJlc29sdmUiLCJjb21wYXJlTm9kZVR5cGVPZiIsIm1hdGNoaW5nVHlwZU9mIiwiJCR0eXBlb2YiLCJzcHlNZXRob2QiLCJtZXRob2ROYW1lIiwiZ2V0U3R1YiIsImxhc3RSZXR1cm5WYWx1ZSIsIm9yaWdpbmFsTWV0aG9kIiwiaGFzT3duIiwiZGVzY3JpcHRvciIsImdldE93blByb3BlcnR5RGVzY3JpcHRvciIsImRlZmluZVByb3BlcnR5IiwiY29uZmlndXJhYmxlIiwiZW51bWVyYWJsZSIsInNwaWVkIiwiYXJncyIsImFwcGx5IiwicmVzdG9yZSIsImdldExhc3RSZXR1cm5WYWx1ZSIsInNweVByb3BlcnR5IiwicHJvcGVydHlOYW1lIiwiaGFuZGxlcnMiLCJvcmlnaW5hbFZhbHVlIiwid2FzQXNzaWduZWQiLCJob2xkZXIiLCJnZXRWIiwiZ2V0Iiwic2V0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LE1BQU0sQ0FBQ2lDLGNBQVAsQ0FBc0JuRSxRQUF0QixFQUFnQzRFLFlBQWhDLEVBQThDO0FBQzVDUixJQUFBQSxZQUFZLEVBQUUsSUFEOEI7QUFFNUNDLElBQUFBLFVBQVUsRUFBRSxDQUFDSixVQUFELElBQWUsQ0FBQyxDQUFDQSxVQUFVLENBQUNJLFVBRkk7QUFHNUNhLElBQUFBLEdBQUcsRUFBRUQsSUFIdUM7QUFJNUNFLElBQUFBLEdBQUcsRUFBSEE7QUFKNEMsR0FBOUM7QUFPQSxTQUFPO0FBQ0xWLElBQUFBLE9BREsscUJBQ0s7QUFDUixVQUFJVCxNQUFKLEVBQVk7QUFDVixZQUFJQyxVQUFKLEVBQWdCO0FBQ2QvQixVQUFBQSxNQUFNLENBQUNpQyxjQUFQLENBQXNCbkUsUUFBdEIsRUFBZ0M0RSxZQUFoQyxFQUE4Q1gsVUFBOUM7QUFDRCxTQUZELE1BRU87QUFDTDtBQUNBakUsVUFBQUEsUUFBUSxDQUFDNEUsWUFBRCxDQUFSLEdBQXlCSSxNQUF6QjtBQUNBO0FBQ0Q7QUFDRixPQVJELE1BUU87QUFDTDtBQUNBLGVBQU9oRixRQUFRLENBQUM0RSxZQUFELENBQWY7QUFDQTtBQUNEO0FBQ0YsS0FmSTtBQWdCTEcsSUFBQUEsV0FoQksseUJBZ0JTO0FBQ1osYUFBT0EsWUFBUDtBQUNEO0FBbEJJLEdBQVA7QUFvQkQiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgZnVuY3Rpb25OYW1lIGZyb20gJ2Z1bmN0aW9uLnByb3RvdHlwZS5uYW1lJztcbmltcG9ydCBmcm9tRW50cmllcyBmcm9tICdvYmplY3QuZnJvbWVudHJpZXMnO1xuaW1wb3J0IGhhcyBmcm9tICdoYXMnO1xuaW1wb3J0IGNyZWF0ZU1vdW50V3JhcHBlciBmcm9tICcuL2NyZWF0ZU1vdW50V3JhcHBlcic7XG5pbXBvcnQgY3JlYXRlUmVuZGVyV3JhcHBlciBmcm9tICcuL2NyZWF0ZVJlbmRlcldyYXBwZXInO1xuaW1wb3J0IHdyYXAgZnJvbSAnLi93cmFwV2l0aFNpbXBsZVdyYXBwZXInO1xuaW1wb3J0IFJvb3RGaW5kZXIgZnJvbSAnLi9Sb290RmluZGVyJztcblxuZXhwb3J0IHtcbiAgY3JlYXRlTW91bnRXcmFwcGVyLFxuICBjcmVhdGVSZW5kZXJXcmFwcGVyLFxuICB3cmFwLFxuICBSb290RmluZGVyLFxufTtcblxuZXhwb3J0IGZ1bmN0aW9uIG1hcE5hdGl2ZUV2ZW50TmFtZXMoZXZlbnQsIHtcbiAgYW5pbWF0aW9uID0gZmFsc2UsIC8vIHNob3VsZCBiZSB0cnVlIGZvciBSZWFjdCAxNStcbiAgcG9pbnRlckV2ZW50cyA9IGZhbHNlLCAvLyBzaG91bGQgYmUgdHJ1ZSBmb3IgUmVhY3QgMTYuNCtcbiAgYXV4Q2xpY2sgPSBmYWxzZSwgLy8gc2hvdWxkIGJlIHRydWUgZm9yIFJlYWN0IDE2LjUrXG59ID0ge30pIHtcbiAgY29uc3QgbmF0aXZlVG9SZWFjdEV2ZW50TWFwID0ge1xuICAgIGNvbXBvc2l0aW9uZW5kOiAnY29tcG9zaXRpb25FbmQnLFxuICAgIGNvbXBvc2l0aW9uc3RhcnQ6ICdjb21wb3NpdGlvblN0YXJ0JyxcbiAgICBjb21wb3NpdGlvbnVwZGF0ZTogJ2NvbXBvc2l0aW9uVXBkYXRlJyxcbiAgICBrZXlkb3duOiAna2V5RG93bicsXG4gICAga2V5dXA6ICdrZXlVcCcsXG4gICAga2V5cHJlc3M6ICdrZXlQcmVzcycsXG4gICAgY29udGV4dG1lbnU6ICdjb250ZXh0TWVudScsXG4gICAgZGJsY2xpY2s6ICdkb3VibGVDbGljaycsXG4gICAgZG91YmxlY2xpY2s6ICdkb3VibGVDbGljaycsIC8vIGtlcHQgZm9yIGxlZ2FjeS4gVE9ETzogcmVtb3ZlIHdpdGggbmV4dCBtYWpvci5cbiAgICBkcmFnZW5kOiAnZHJhZ0VuZCcsXG4gICAgZHJhZ2VudGVyOiAnZHJhZ0VudGVyJyxcbiAgICBkcmFnZXhpc3Q6ICdkcmFnRXhpdCcsXG4gICAgZHJhZ2xlYXZlOiAnZHJhZ0xlYXZlJyxcbiAgICBkcmFnb3ZlcjogJ2RyYWdPdmVyJyxcbiAgICBkcmFnc3RhcnQ6ICdkcmFnU3RhcnQnLFxuICAgIG1vdXNlZG93bjogJ21vdXNlRG93bicsXG4gICAgbW91c2Vtb3ZlOiAnbW91c2VNb3ZlJyxcbiAgICBtb3VzZW91dDogJ21vdXNlT3V0JyxcbiAgICBtb3VzZW92ZXI6ICdtb3VzZU92ZXInLFxuICAgIG1vdXNldXA6ICdtb3VzZVVwJyxcbiAgICB0b3VjaGNhbmNlbDogJ3RvdWNoQ2FuY2VsJyxcbiAgICB0b3VjaGVuZDogJ3RvdWNoRW5kJyxcbiAgICB0b3VjaG1vdmU6ICd0b3VjaE1vdmUnLFxuICAgIHRvdWNoc3RhcnQ6ICd0b3VjaFN0YXJ0JyxcbiAgICBjYW5wbGF5OiAnY2FuUGxheScsXG4gICAgY2FucGxheXRocm91Z2g6ICdjYW5QbGF5VGhyb3VnaCcsXG4gICAgZHVyYXRpb25jaGFuZ2U6ICdkdXJhdGlvbkNoYW5nZScsXG4gICAgbG9hZGVkZGF0YTogJ2xvYWRlZERhdGEnLFxuICAgIGxvYWRlZG1ldGFkYXRhOiAnbG9hZGVkTWV0YWRhdGEnLFxuICAgIGxvYWRzdGFydDogJ2xvYWRTdGFydCcsXG4gICAgcmF0ZWNoYW5nZTogJ3JhdGVDaGFuZ2UnLFxuICAgIHRpbWV1cGRhdGU6ICd0aW1lVXBkYXRlJyxcbiAgICB2b2x1bWVjaGFuZ2U6ICd2b2x1bWVDaGFuZ2UnLFxuICAgIGJlZm9yZWlucHV0OiAnYmVmb3JlSW5wdXQnLFxuICAgIG1vdXNlZW50ZXI6ICdtb3VzZUVudGVyJyxcbiAgICBtb3VzZWxlYXZlOiAnbW91c2VMZWF2ZScsXG4gICAgdHJhbnNpdGlvbmVuZDogJ3RyYW5zaXRpb25FbmQnLFxuICAgIC4uLihhbmltYXRpb24gJiYge1xuICAgICAgYW5pbWF0aW9uc3RhcnQ6ICdhbmltYXRpb25TdGFydCcsXG4gICAgICBhbmltYXRpb25pdGVyYXRpb246ICdhbmltYXRpb25JdGVyYXRpb24nLFxuICAgICAgYW5pbWF0aW9uZW5kOiAnYW5pbWF0aW9uRW5kJyxcbiAgICB9KSxcbiAgICAuLi4ocG9pbnRlckV2ZW50cyAmJiB7XG4gICAgICBwb2ludGVyZG93bjogJ3BvaW50ZXJEb3duJyxcbiAgICAgIHBvaW50ZXJtb3ZlOiAncG9pbnRlck1vdmUnLFxuICAgICAgcG9pbnRlcnVwOiAncG9pbnRlclVwJyxcbiAgICAgIHBvaW50ZXJjYW5jZWw6ICdwb2ludGVyQ2FuY2VsJyxcbiAgICAgIGdvdHBvaW50ZXJjYXB0dXJlOiAnZ290UG9pbnRlckNhcHR1cmUnLFxuICAgICAgbG9zdHBvaW50ZXJjYXB0dXJlOiAnbG9zdFBvaW50ZXJDYXB0dXJlJyxcbiAgICAgIHBvaW50ZXJlbnRlcjogJ3BvaW50ZXJFbnRlcicsXG4gICAgICBwb2ludGVybGVhdmU6ICdwb2ludGVyTGVhdmUnLFxuICAgICAgcG9pbnRlcm92ZXI6ICdwb2ludGVyT3ZlcicsXG4gICAgICBwb2ludGVyb3V0OiAncG9pbnRlck91dCcsXG4gICAgfSksXG4gICAgLi4uKGF1eENsaWNrICYmIHtcbiAgICAgIGF1eGNsaWNrOiAnYXV4Q2xpY2snLFxuICAgIH0pLFxuICB9O1xuXG4gIHJldHVybiBuYXRpdmVUb1JlYWN0RXZlbnRNYXBbZXZlbnRdIHx8IGV2ZW50O1xufVxuXG4vLyAnY2xpY2snID0+ICdvbkNsaWNrJ1xuLy8gJ21vdXNlRW50ZXInID0+ICdvbk1vdXNlRW50ZXInXG5leHBvcnQgZnVuY3Rpb24gcHJvcEZyb21FdmVudChldmVudCwgZXZlbnRPcHRpb25zID0ge30pIHtcbiAgY29uc3QgbmF0aXZlRXZlbnQgPSBtYXBOYXRpdmVFdmVudE5hbWVzKGV2ZW50LCBldmVudE9wdGlvbnMpO1xuICByZXR1cm4gYG9uJHtuYXRpdmVFdmVudFswXS50b1VwcGVyQ2FzZSgpfSR7bmF0aXZlRXZlbnQuc2xpY2UoMSl9YDtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHdpdGhTZXRTdGF0ZUFsbG93ZWQoZm4pIHtcbiAgLy8gTk9URShsbXIpOlxuICAvLyB0aGlzIGlzIGN1cnJlbnRseSBoZXJlIHRvIGNpcmN1bXZlbnQgYSBSZWFjdCBidWcgd2hlcmUgYHNldFN0YXRlKClgIGlzXG4gIC8vIG5vdCBhbGxvd2VkIHdpdGhvdXQgZ2xvYmFsIGJlaW5nIGRlZmluZWQuXG4gIGxldCBjbGVhbnVwID0gZmFsc2U7XG4gIGlmICh0eXBlb2YgZ2xvYmFsLmRvY3VtZW50ID09PSAndW5kZWZpbmVkJykge1xuICAgIGNsZWFudXAgPSB0cnVlO1xuICAgIGdsb2JhbC5kb2N1bWVudCA9IHt9O1xuICB9XG4gIGNvbnN0IHJlc3VsdCA9IGZuKCk7XG4gIGlmIChjbGVhbnVwKSB7XG4gICAgLy8gVGhpcyB3b3JrcyBhcm91bmQgYSBidWcgaW4gbm9kZS9qZXN0IGluIHRoYXQgZGV2ZWxvcGVycyBhcmVuJ3QgYWJsZSB0b1xuICAgIC8vIGRlbGV0ZSB0aGluZ3MgZnJvbSBnbG9iYWwgd2hlbiBydW5uaW5nIGluIGEgbm9kZSB2bS5cbiAgICBnbG9iYWwuZG9jdW1lbnQgPSB1bmRlZmluZWQ7XG4gICAgZGVsZXRlIGdsb2JhbC5kb2N1bWVudDtcbiAgfVxuICByZXR1cm4gcmVzdWx0O1xufVxuXG5leHBvcnQgZnVuY3Rpb24gYXNzZXJ0RG9tQXZhaWxhYmxlKGZlYXR1cmUpIHtcbiAgaWYgKCFnbG9iYWwgfHwgIWdsb2JhbC5kb2N1bWVudCB8fCAhZ2xvYmFsLmRvY3VtZW50LmNyZWF0ZUVsZW1lbnQpIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoYEVuenltZSdzICR7ZmVhdHVyZX0gZXhwZWN0cyBhIERPTSBlbnZpcm9ubWVudCB0byBiZSBsb2FkZWQsIGJ1dCBmb3VuZCBub25lYCk7XG4gIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGRpc3BsYXlOYW1lT2ZOb2RlKG5vZGUpIHtcbiAgaWYgKCFub2RlKSByZXR1cm4gbnVsbDtcblxuICBjb25zdCB7IHR5cGUgfSA9IG5vZGU7XG5cbiAgaWYgKCF0eXBlKSByZXR1cm4gbnVsbDtcblxuICByZXR1cm4gdHlwZS5kaXNwbGF5TmFtZSB8fCAodHlwZW9mIHR5cGUgPT09ICdmdW5jdGlvbicgPyBmdW5jdGlvbk5hbWUodHlwZSkgOiB0eXBlLm5hbWUgfHwgdHlwZSk7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBub2RlVHlwZUZyb21UeXBlKHR5cGUpIHtcbiAgaWYgKHR5cGVvZiB0eXBlID09PSAnc3RyaW5nJykge1xuICAgIHJldHVybiAnaG9zdCc7XG4gIH1cbiAgaWYgKHR5cGUgJiYgdHlwZS5wcm90b3R5cGUgJiYgdHlwZS5wcm90b3R5cGUuaXNSZWFjdENvbXBvbmVudCkge1xuICAgIHJldHVybiAnY2xhc3MnO1xuICB9XG4gIHJldHVybiAnZnVuY3Rpb24nO1xufVxuXG5mdW5jdGlvbiBnZXRJdGVyYXRvckZuKG9iaikge1xuICBjb25zdCBpdGVyYXRvckZuID0gb2JqICYmIChcbiAgICAodHlwZW9mIFN5bWJvbCA9PT0gJ2Z1bmN0aW9uJyAmJiB0eXBlb2YgU3ltYm9sLml0ZXJhdG9yID09PSAnc3ltYm9sJyAmJiBvYmpbU3ltYm9sLml0ZXJhdG9yXSlcbiAgICB8fCBvYmpbJ0BAaXRlcmF0b3InXVxuICApO1xuXG4gIGlmICh0eXBlb2YgaXRlcmF0b3JGbiA9PT0gJ2Z1bmN0aW9uJykge1xuICAgIHJldHVybiBpdGVyYXRvckZuO1xuICB9XG5cbiAgcmV0dXJuIHVuZGVmaW5lZDtcbn1cblxuZnVuY3Rpb24gaXNJdGVyYWJsZShvYmopIHtcbiAgcmV0dXJuICEhZ2V0SXRlcmF0b3JGbihvYmopO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gaXNBcnJheUxpa2Uob2JqKSB7XG4gIHJldHVybiBBcnJheS5pc0FycmF5KG9iaikgfHwgKHR5cGVvZiBvYmogIT09ICdzdHJpbmcnICYmIGlzSXRlcmFibGUob2JqKSk7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBmbGF0dGVuKGFycnMpIHtcbiAgLy8gb3B0aW1pemUgZm9yIHRoZSBtb3N0IGNvbW1vbiBjYXNlXG4gIGlmIChBcnJheS5pc0FycmF5KGFycnMpKSB7XG4gICAgcmV0dXJuIGFycnMucmVkdWNlKFxuICAgICAgKGZsYXRBcnJzLCBpdGVtKSA9PiBmbGF0QXJycy5jb25jYXQoaXNBcnJheUxpa2UoaXRlbSkgPyBmbGF0dGVuKGl0ZW0pIDogaXRlbSksXG4gICAgICBbXSxcbiAgICApO1xuICB9XG5cbiAgLy8gZmFsbGJhY2sgZm9yIGFyYml0cmFyeSBpdGVyYWJsZSBjaGlsZHJlblxuICBsZXQgZmxhdEFycnMgPSBbXTtcblxuICBjb25zdCBpdGVyYXRvckZuID0gZ2V0SXRlcmF0b3JGbihhcnJzKTtcbiAgY29uc3QgaXRlcmF0b3IgPSBpdGVyYXRvckZuLmNhbGwoYXJycyk7XG5cbiAgbGV0IHN0ZXAgPSBpdGVyYXRvci5uZXh0KCk7XG5cbiAgd2hpbGUgKCFzdGVwLmRvbmUpIHtcbiAgICBjb25zdCBpdGVtID0gc3RlcC52YWx1ZTtcbiAgICBsZXQgZmxhdEl0ZW07XG5cbiAgICBpZiAoaXNBcnJheUxpa2UoaXRlbSkpIHtcbiAgICAgIGZsYXRJdGVtID0gZmxhdHRlbihpdGVtKTtcbiAgICB9IGVsc2Uge1xuICAgICAgZmxhdEl0ZW0gPSBpdGVtO1xuICAgIH1cblxuICAgIGZsYXRBcnJzID0gZmxhdEFycnMuY29uY2F0KGZsYXRJdGVtKTtcblxuICAgIHN0ZXAgPSBpdGVyYXRvci5uZXh0KCk7XG4gIH1cblxuICByZXR1cm4gZmxhdEFycnM7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBlbnN1cmVLZXlPclVuZGVmaW5lZChrZXkpIHtcbiAgcmV0dXJuIGtleSB8fCAoa2V5ID09PSAnJyA/ICcnIDogdW5kZWZpbmVkKTtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGVsZW1lbnRUb1RyZWUoZWwsIHJlY3Vyc2UgPSBlbGVtZW50VG9UcmVlKSB7XG4gIGlmICh0eXBlb2YgcmVjdXJzZSAhPT0gJ2Z1bmN0aW9uJyAmJiBhcmd1bWVudHMubGVuZ3RoID09PSAzKSB7XG4gICAgLy8gc3BlY2lhbCBjYXNlIGZvciBiYWNrd2FyZHMgY29tcGF0IGZvciBgLm1hcChlbGVtZW50VG9UcmVlKWBcbiAgICByZWN1cnNlID0gZWxlbWVudFRvVHJlZTsgLy8gZXNsaW50LWRpc2FibGUtbGluZSBuby1wYXJhbS1yZWFzc2lnblxuICB9XG4gIGlmIChlbCA9PT0gbnVsbCB8fCB0eXBlb2YgZWwgIT09ICdvYmplY3QnIHx8ICEoJ3R5cGUnIGluIGVsKSkge1xuICAgIHJldHVybiBlbDtcbiAgfVxuICBjb25zdCB7XG4gICAgdHlwZSxcbiAgICBwcm9wcyxcbiAgICBrZXksXG4gICAgcmVmLFxuICB9ID0gZWw7XG4gIGNvbnN0IHsgY2hpbGRyZW4gfSA9IHByb3BzO1xuICBsZXQgcmVuZGVyZWQgPSBudWxsO1xuICBpZiAoaXNBcnJheUxpa2UoY2hpbGRyZW4pKSB7XG4gICAgcmVuZGVyZWQgPSBmbGF0dGVuKGNoaWxkcmVuKS5tYXAoKHgpID0+IHJlY3Vyc2UoeCkpO1xuICB9IGVsc2UgaWYgKHR5cGVvZiBjaGlsZHJlbiAhPT0gJ3VuZGVmaW5lZCcpIHtcbiAgICByZW5kZXJlZCA9IHJlY3Vyc2UoY2hpbGRyZW4pO1xuICB9XG5cbiAgY29uc3Qgbm9kZVR5cGUgPSBub2RlVHlwZUZyb21UeXBlKHR5cGUpO1xuXG4gIGlmIChub2RlVHlwZSA9PT0gJ2hvc3QnICYmIHByb3BzLmRhbmdlcm91c2x5U2V0SW5uZXJIVE1MKSB7XG4gICAgaWYgKHByb3BzLmNoaWxkcmVuICE9IG51bGwpIHtcbiAgICAgIGNvbnN0IGVycm9yID0gbmV3IEVycm9yKCdDYW4gb25seSBzZXQgb25lIG9mIGBjaGlsZHJlbmAgb3IgYHByb3BzLmRhbmdlcm91c2x5U2V0SW5uZXJIVE1MYC4nKTtcbiAgICAgIGVycm9yLm5hbWUgPSAnSW52YXJpYW50IFZpb2xhdGlvbic7XG4gICAgICB0aHJvdyBlcnJvcjtcbiAgICB9XG4gIH1cblxuICByZXR1cm4ge1xuICAgIG5vZGVUeXBlLFxuICAgIHR5cGUsXG4gICAgcHJvcHMsXG4gICAga2V5OiBlbnN1cmVLZXlPclVuZGVmaW5lZChrZXkpLFxuICAgIHJlZixcbiAgICBpbnN0YW5jZTogbnVsbCxcbiAgICByZW5kZXJlZCxcbiAgfTtcbn1cblxuZnVuY3Rpb24gbWFwRmluZChhcnJheWxpa2UsIG1hcHBlciwgZmluZGVyKSB7XG4gIGxldCBmb3VuZDtcbiAgY29uc3QgaXNGb3VuZCA9IEFycmF5LnByb3RvdHlwZS5maW5kLmNhbGwoYXJyYXlsaWtlLCAoaXRlbSkgPT4ge1xuICAgIGZvdW5kID0gbWFwcGVyKGl0ZW0pO1xuICAgIHJldHVybiBmaW5kZXIoZm91bmQpO1xuICB9KTtcbiAgcmV0dXJuIGlzRm91bmQgPyBmb3VuZCA6IHVuZGVmaW5lZDtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGZpbmRFbGVtZW50KGVsLCBwcmVkaWNhdGUpIHtcbiAgaWYgKGVsID09PSBudWxsIHx8IHR5cGVvZiBlbCAhPT0gJ29iamVjdCcgfHwgISgndHlwZScgaW4gZWwpKSB7XG4gICAgcmV0dXJuIHVuZGVmaW5lZDtcbiAgfVxuICBpZiAocHJlZGljYXRlKGVsKSkge1xuICAgIHJldHVybiBlbDtcbiAgfVxuICBjb25zdCB7IHJlbmRlcmVkIH0gPSBlbDtcbiAgaWYgKGlzQXJyYXlMaWtlKHJlbmRlcmVkKSkge1xuICAgIHJldHVybiBtYXBGaW5kKHJlbmRlcmVkLCAoeCkgPT4gZmluZEVsZW1lbnQoeCwgcHJlZGljYXRlKSwgKHgpID0+IHR5cGVvZiB4ICE9PSAndW5kZWZpbmVkJyk7XG4gIH1cbiAgcmV0dXJuIGZpbmRFbGVtZW50KHJlbmRlcmVkLCBwcmVkaWNhdGUpO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gcHJvcHNXaXRoS2V5c0FuZFJlZihub2RlKSB7XG4gIGlmIChub2RlLnJlZiAhPT0gbnVsbCB8fCBub2RlLmtleSAhPT0gbnVsbCkge1xuICAgIHJldHVybiB7XG4gICAgICAuLi5ub2RlLnByb3BzLFxuICAgICAga2V5OiBub2RlLmtleSxcbiAgICAgIHJlZjogbm9kZS5yZWYsXG4gICAgfTtcbiAgfVxuICByZXR1cm4gbm9kZS5wcm9wcztcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGdldENvbXBvbmVudFN0YWNrKFxuICBoaWVyYXJjaHksXG4gIGdldE5vZGVUeXBlID0gbm9kZVR5cGVGcm9tVHlwZSxcbiAgZ2V0RGlzcGxheU5hbWUgPSBkaXNwbGF5TmFtZU9mTm9kZSxcbikge1xuICBjb25zdCB0dXBsZXMgPSBoaWVyYXJjaHkuZmlsdGVyKChub2RlKSA9PiBub2RlLnR5cGUgIT09IFJvb3RGaW5kZXIpLm1hcCgoeCkgPT4gW1xuICAgIGdldE5vZGVUeXBlKHgudHlwZSksXG4gICAgZ2V0RGlzcGxheU5hbWUoeCksXG4gIF0pLmNvbmNhdChbW1xuICAgICdjbGFzcycsXG4gICAgJ1dyYXBwZXJDb21wb25lbnQnLFxuICBdXSk7XG5cbiAgcmV0dXJuIHR1cGxlcy5tYXAoKFssIG5hbWVdLCBpLCBhcnIpID0+IHtcbiAgICBjb25zdCBbLCBjbG9zZXN0Q29tcG9uZW50XSA9IGFyci5zbGljZShpICsgMSkuZmluZCgoW25vZGVUeXBlXSkgPT4gbm9kZVR5cGUgIT09ICdob3N0JykgfHwgW107XG4gICAgcmV0dXJuIGBcXG4gICAgaW4gJHtuYW1lfSR7Y2xvc2VzdENvbXBvbmVudCA/IGAgKGNyZWF0ZWQgYnkgJHtjbG9zZXN0Q29tcG9uZW50fSlgIDogJyd9YDtcbiAgfSkuam9pbignJyk7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBzaW11bGF0ZUVycm9yKFxuICBlcnJvcixcbiAgY2F0Y2hpbmdJbnN0YW5jZSxcbiAgcm9vdE5vZGUsIC8vIFRPRE86IHJlbW92ZSBgcm9vdE5vZGVgIG5leHQgc2VtdmVyLW1ham9yXG4gIGhpZXJhcmNoeSxcbiAgZ2V0Tm9kZVR5cGUgPSBub2RlVHlwZUZyb21UeXBlLFxuICBnZXREaXNwbGF5TmFtZSA9IGRpc3BsYXlOYW1lT2ZOb2RlLFxuICBjYXRjaGluZ1R5cGUgPSB7fSxcbikge1xuICBjb25zdCBpbnN0YW5jZSA9IGNhdGNoaW5nSW5zdGFuY2UgfHwge307XG5cbiAgY29uc3QgeyBjb21wb25lbnREaWRDYXRjaCB9ID0gaW5zdGFuY2U7XG5cbiAgY29uc3QgeyBnZXREZXJpdmVkU3RhdGVGcm9tRXJyb3IgfSA9IGNhdGNoaW5nVHlwZTtcblxuICBpZiAoIWNvbXBvbmVudERpZENhdGNoICYmICFnZXREZXJpdmVkU3RhdGVGcm9tRXJyb3IpIHtcbiAgICB0aHJvdyBlcnJvcjtcbiAgfVxuXG4gIGlmIChnZXREZXJpdmVkU3RhdGVGcm9tRXJyb3IpIHtcbiAgICBjb25zdCBzdGF0ZVVwZGF0ZSA9IGdldERlcml2ZWRTdGF0ZUZyb21FcnJvci5jYWxsKGNhdGNoaW5nVHlwZSwgZXJyb3IpO1xuICAgIGluc3RhbmNlLnNldFN0YXRlKHN0YXRlVXBkYXRlKTtcbiAgfVxuXG4gIGlmIChjb21wb25lbnREaWRDYXRjaCkge1xuICAgIGNvbnN0IGNvbXBvbmVudFN0YWNrID0gZ2V0Q29tcG9uZW50U3RhY2soaGllcmFyY2h5LCBnZXROb2RlVHlwZSwgZ2V0RGlzcGxheU5hbWUpO1xuICAgIGNvbXBvbmVudERpZENhdGNoLmNhbGwoaW5zdGFuY2UsIGVycm9yLCB7IGNvbXBvbmVudFN0YWNrIH0pO1xuICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBnZXRNYXNrZWRDb250ZXh0KGNvbnRleHRUeXBlcywgdW5tYXNrZWRDb250ZXh0KSB7XG4gIGlmICghY29udGV4dFR5cGVzIHx8ICF1bm1hc2tlZENvbnRleHQpIHtcbiAgICByZXR1cm4ge307XG4gIH1cbiAgcmV0dXJuIGZyb21FbnRyaWVzKE9iamVjdC5rZXlzKGNvbnRleHRUeXBlcykubWFwKChrZXkpID0+IFtrZXksIHVubWFza2VkQ29udGV4dFtrZXldXSkpO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZ2V0Tm9kZUZyb21Sb290RmluZGVyKGlzQ3VzdG9tQ29tcG9uZW50LCB0cmVlLCBvcHRpb25zKSB7XG4gIGlmICghaXNDdXN0b21Db21wb25lbnQob3B0aW9ucy53cmFwcGluZ0NvbXBvbmVudCkpIHtcbiAgICByZXR1cm4gdHJlZS5yZW5kZXJlZDtcbiAgfVxuICBjb25zdCByb290RmluZGVyID0gZmluZEVsZW1lbnQodHJlZSwgKG5vZGUpID0+IG5vZGUudHlwZSA9PT0gUm9vdEZpbmRlcik7XG4gIGlmICghcm9vdEZpbmRlcikge1xuICAgIHRocm93IG5ldyBFcnJvcignYHdyYXBwaW5nQ29tcG9uZW50YCBtdXN0IHJlbmRlciBpdHMgY2hpbGRyZW4hJyk7XG4gIH1cbiAgcmV0dXJuIHJvb3RGaW5kZXIucmVuZGVyZWQ7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiB3cmFwV2l0aFdyYXBwaW5nQ29tcG9uZW50KGNyZWF0ZUVsZW1lbnQsIG5vZGUsIG9wdGlvbnMpIHtcbiAgY29uc3QgeyB3cmFwcGluZ0NvbXBvbmVudCwgd3JhcHBpbmdDb21wb25lbnRQcm9wcyB9ID0gb3B0aW9ucztcbiAgaWYgKCF3cmFwcGluZ0NvbXBvbmVudCkge1xuICAgIHJldHVybiBub2RlO1xuICB9XG4gIHJldHVybiBjcmVhdGVFbGVtZW50KFxuICAgIHdyYXBwaW5nQ29tcG9uZW50LFxuICAgIHdyYXBwaW5nQ29tcG9uZW50UHJvcHMsXG4gICAgY3JlYXRlRWxlbWVudChSb290RmluZGVyLCBudWxsLCBub2RlKSxcbiAgKTtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGdldFdyYXBwaW5nQ29tcG9uZW50TW91bnRSZW5kZXJlcih7IHRvVHJlZSwgZ2V0TW91bnRXcmFwcGVySW5zdGFuY2UgfSkge1xuICByZXR1cm4ge1xuICAgIGdldE5vZGUoKSB7XG4gICAgICBjb25zdCBpbnN0YW5jZSA9IGdldE1vdW50V3JhcHBlckluc3RhbmNlKCk7XG4gICAgICByZXR1cm4gaW5zdGFuY2UgPyB0b1RyZWUoaW5zdGFuY2UpLnJlbmRlcmVkIDogbnVsbDtcbiAgICB9LFxuICAgIHJlbmRlcihlbCwgY29udGV4dCwgY2FsbGJhY2spIHtcbiAgICAgIGNvbnN0IGluc3RhbmNlID0gZ2V0TW91bnRXcmFwcGVySW5zdGFuY2UoKTtcbiAgICAgIGlmICghaW5zdGFuY2UpIHtcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKCdUaGUgd3JhcHBpbmcgY29tcG9uZW50IG1heSBub3QgYmUgdXBkYXRlZCBpZiB0aGUgcm9vdCBpcyB1bm1vdW50ZWQuJyk7XG4gICAgICB9XG4gICAgICByZXR1cm4gaW5zdGFuY2Uuc2V0V3JhcHBpbmdDb21wb25lbnRQcm9wcyhlbC5wcm9wcywgY2FsbGJhY2spO1xuICAgIH0sXG4gIH07XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBmYWtlRHluYW1pY0ltcG9ydChtb2R1bGVUb0ltcG9ydCkge1xuICByZXR1cm4gUHJvbWlzZS5yZXNvbHZlKHsgZGVmYXVsdDogbW9kdWxlVG9JbXBvcnQgfSk7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBjb21wYXJlTm9kZVR5cGVPZihub2RlLCBtYXRjaGluZ1R5cGVPZikge1xuICBpZiAoIW5vZGUpIHtcbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cbiAgcmV0dXJuIG5vZGUuJCR0eXBlb2YgPT09IG1hdGNoaW5nVHlwZU9mO1xufVxuXG4vLyBUT0RPOiB3aGVuIGVuenltZSB2My4xMi4wIGlzIHJlcXVpcmVkLCBkZWxldGUgdGhpc1xuZXhwb3J0IGZ1bmN0aW9uIHNweU1ldGhvZChpbnN0YW5jZSwgbWV0aG9kTmFtZSwgZ2V0U3R1YiA9ICgpID0+IHt9KSB7XG4gIGxldCBsYXN0UmV0dXJuVmFsdWU7XG4gIGNvbnN0IG9yaWdpbmFsTWV0aG9kID0gaW5zdGFuY2VbbWV0aG9kTmFtZV07XG4gIGNvbnN0IGhhc093biA9IGhhcyhpbnN0YW5jZSwgbWV0aG9kTmFtZSk7XG4gIGxldCBkZXNjcmlwdG9yO1xuICBpZiAoaGFzT3duKSB7XG4gICAgZGVzY3JpcHRvciA9IE9iamVjdC5nZXRPd25Qcm9wZXJ0eURlc2NyaXB0b3IoaW5zdGFuY2UsIG1ldGhvZE5hbWUpO1xuICB9XG4gIE9iamVjdC5kZWZpbmVQcm9wZXJ0eShpbnN0YW5jZSwgbWV0aG9kTmFtZSwge1xuICAgIGNvbmZpZ3VyYWJsZTogdHJ1ZSxcbiAgICBlbnVtZXJhYmxlOiAhZGVzY3JpcHRvciB8fCAhIWRlc2NyaXB0b3IuZW51bWVyYWJsZSxcbiAgICB2YWx1ZTogZ2V0U3R1YihvcmlnaW5hbE1ldGhvZCkgfHwgZnVuY3Rpb24gc3BpZWQoLi4uYXJncykge1xuICAgICAgY29uc3QgcmVzdWx0ID0gb3JpZ2luYWxNZXRob2QuYXBwbHkodGhpcywgYXJncyk7XG4gICAgICBsYXN0UmV0dXJuVmFsdWUgPSByZXN1bHQ7XG4gICAgICByZXR1cm4gcmVzdWx0O1xuICAgIH0sXG4gIH0pO1xuICByZXR1cm4ge1xuICAgIHJlc3RvcmUoKSB7XG4gICAgICBpZiAoaGFzT3duKSB7XG4gICAgICAgIGlmIChkZXNjcmlwdG9yKSB7XG4gICAgICAgICAgT2JqZWN0LmRlZmluZVByb3BlcnR5KGluc3RhbmNlLCBtZXRob2ROYW1lLCBkZXNjcmlwdG9yKTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAvKiBlc2xpbnQtZGlzYWJsZSBuby1wYXJhbS1yZWFzc2lnbiAqL1xuICAgICAgICAgIGluc3RhbmNlW21ldGhvZE5hbWVdID0gb3JpZ2luYWxNZXRob2Q7XG4gICAgICAgICAgLyogZXNsaW50LWVuYWJsZSBuby1wYXJhbS1yZWFzc2lnbiAqL1xuICAgICAgICB9XG4gICAgICB9IGVsc2Uge1xuICAgICAgICAvKiBlc2xpbnQtZGlzYWJsZSBuby1wYXJhbS1yZWFzc2lnbiAqL1xuICAgICAgICBkZWxldGUgaW5zdGFuY2VbbWV0aG9kTmFtZV07XG4gICAgICAgIC8qIGVzbGludC1lbmFibGUgbm8tcGFyYW0tcmVhc3NpZ24gKi9cbiAgICAgIH1cbiAgICB9LFxuICAgIGdldExhc3RSZXR1cm5WYWx1ZSgpIHtcbiAgICAgIHJldHVybiBsYXN0UmV0dXJuVmFsdWU7XG4gICAgfSxcbiAgfTtcbn1cblxuLy8gVE9ETzogd2hlbiBlbnp5bWUgdjMuMTIuMCBpcyByZXF1aXJlZCwgZGVsZXRlIHRoaXNcbmV4cG9ydCBmdW5jdGlvbiBzcHlQcm9wZXJ0eShpbnN0YW5jZSwgcHJvcGVydHlOYW1lLCBoYW5kbGVycyA9IHt9KSB7XG4gIGNvbnN0IG9yaWdpbmFsVmFsdWUgPSBpbnN0YW5jZVtwcm9wZXJ0eU5hbWVdO1xuICBjb25zdCBoYXNPd24gPSBoYXMoaW5zdGFuY2UsIHByb3BlcnR5TmFtZSk7XG4gIGxldCBkZXNjcmlwdG9yO1xuICBpZiAoaGFzT3duKSB7XG4gICAgZGVzY3JpcHRvciA9IE9iamVjdC5nZXRPd25Qcm9wZXJ0eURlc2NyaXB0b3IoaW5zdGFuY2UsIHByb3BlcnR5TmFtZSk7XG4gIH1cbiAgbGV0IHdhc0Fzc2lnbmVkID0gZmFsc2U7XG4gIGxldCBob2xkZXIgPSBvcmlnaW5hbFZhbHVlO1xuICBjb25zdCBnZXRWID0gaGFuZGxlcnMuZ2V0ID8gKCkgPT4ge1xuICAgIGNvbnN0IHZhbHVlID0gZGVzY3JpcHRvciAmJiBkZXNjcmlwdG9yLmdldCA/IGRlc2NyaXB0b3IuZ2V0LmNhbGwoaW5zdGFuY2UpIDogaG9sZGVyO1xuICAgIHJldHVybiBoYW5kbGVycy5nZXQuY2FsbChpbnN0YW5jZSwgdmFsdWUpO1xuICB9IDogKCkgPT4gaG9sZGVyO1xuICBjb25zdCBzZXQgPSBoYW5kbGVycy5zZXQgPyAobmV3VmFsdWUpID0+IHtcbiAgICB3YXNBc3NpZ25lZCA9IHRydWU7XG4gICAgY29uc3QgaGFuZGxlck5ld1ZhbHVlID0gaGFuZGxlcnMuc2V0LmNhbGwoaW5zdGFuY2UsIGhvbGRlciwgbmV3VmFsdWUpO1xuICAgIGhvbGRlciA9IGhhbmRsZXJOZXdWYWx1ZTtcbiAgICBpZiAoZGVzY3JpcHRvciAmJiBkZXNjcmlwdG9yLnNldCkge1xuICAgICAgZGVzY3JpcHRvci5zZXQuY2FsbChpbnN0YW5jZSwgaG9sZGVyKTtcbiAgICB9XG4gIH0gOiAodikgPT4ge1xuICAgIHdhc0Fzc2lnbmVkID0gdHJ1ZTtcbiAgICBob2xkZXIgPSB2O1xuICB9O1xuICBPYmplY3QuZGVmaW5lUHJvcGVydHkoaW5zdGFuY2UsIHByb3BlcnR5TmFtZSwge1xuICAgIGNvbmZpZ3VyYWJsZTogdHJ1ZSxcbiAgICBlbnVtZXJhYmxlOiAhZGVzY3JpcHRvciB8fCAhIWRlc2NyaXB0b3IuZW51bWVyYWJsZSxcbiAgICBnZXQ6IGdldFYsXG4gICAgc2V0LFxuICB9KTtcblxuICByZXR1cm4ge1xuICAgIHJlc3RvcmUoKSB7XG4gICAgICBpZiAoaGFzT3duKSB7XG4gICAgICAgIGlmIChkZXNjcmlwdG9yKSB7XG4gICAgICAgICAgT2JqZWN0LmRlZmluZVByb3BlcnR5KGluc3RhbmNlLCBwcm9wZXJ0eU5hbWUsIGRlc2NyaXB0b3IpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIC8qIGVzbGludC1kaXNhYmxlIG5vLXBhcmFtLXJlYXNzaWduICovXG4gICAgICAgICAgaW5zdGFuY2VbcHJvcGVydHlOYW1lXSA9IGhvbGRlcjtcbiAgICAgICAgICAvKiBlc2xpbnQtZW5hYmxlIG5vLXBhcmFtLXJlYXNzaWduICovXG4gICAgICAgIH1cbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIC8qIGVzbGludC1kaXNhYmxlIG5vLXBhcmFtLXJlYXNzaWduICovXG4gICAgICAgIGRlbGV0ZSBpbnN0YW5jZVtwcm9wZXJ0eU5hbWVdO1xuICAgICAgICAvKiBlc2xpbnQtZW5hYmxlIG5vLXBhcmFtLXJlYXNzaWduICovXG4gICAgICB9XG4gICAgfSxcbiAgICB3YXNBc3NpZ25lZCgpIHtcbiAgICAgIHJldHVybiB3YXNBc3NpZ25lZDtcbiAgICB9LFxuICB9O1xufVxuIl19
//# sourceMappingURL=Utils.js.map