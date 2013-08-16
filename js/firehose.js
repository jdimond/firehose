/* Some Utils */
Array.prototype.clean = function(deleteValue) {
  for (var i = 0; i < this.length; i++) {
    if (this[i] == deleteValue) {
      this.splice(i, 1);
      i--;
    }
  }
  return this;
}
Array.prototype.clone = function() { return this.slice(0); };
Array.prototype.ipush = function(x) {
  var a = this.clone();
  a.push(x);
  return a;
};
String.prototype.toPathArray = function() { return this.split('/').clean(""); }

function deepCompare () {
  var leftChain, rightChain;

  function compare2Objects (x, y) {
    var p;

    // remember that NaN === NaN returns false
    // and isNaN(undefined) returns true
    if (isNaN(x) && isNaN(y) && typeof x === 'number' && typeof y === 'number') {
      return true;
    }

    // Compare primitives and functions.     
    // Check if both arguments link to the same object.
    // Especially useful on step when comparing prototypes
    if (x === y) {
      return true;
    }

    // Works in case when functions are created in constructor.
    // Comparing dates is a common scenario. Another built-ins?
    // We can even handle functions passed across iframes
    if ((typeof x === 'function' && typeof y === 'function') ||
    (x instanceof Date && y instanceof Date) ||
    (x instanceof RegExp && y instanceof RegExp) ||
    (x instanceof String && y instanceof String) ||
    (x instanceof Number && y instanceof Number)) {
      return x.toString() === y.toString();
    }

    // At last checking prototypes as good a we can
    if (!(x instanceof Object && y instanceof Object)) {
      return false;
    }

    if (x.isPrototypeOf(y) || y.isPrototypeOf(x)) {
      return false;
    }

    if (x.constructor !== y.constructor) {
      return false;
    }

    if (x.prototype !== y.prototype) {
      return false;
    }

    // check for infinitive linking loops
    if (leftChain.indexOf(x) > -1 || rightChain.indexOf(y) > -1) {
      return false;
    }

    // Quick checking of one object beeing a subset of another.
    // todo: cache the structure of arguments[0] for performance
    for (p in y) {
      if (y.hasOwnProperty(p) !== x.hasOwnProperty(p)) {
        return false;
      }
      else if (typeof y[p] !== typeof x[p]) {
        return false;
      }
    }

    for (p in x) {
      if (y.hasOwnProperty(p) !== x.hasOwnProperty(p)) {
        return false;
      }
      else if (typeof y[p] !== typeof x[p]) {
        return false;
      }

      switch (typeof (x[p])) {
        case 'object':
        case 'function':

          leftChain.push(x);
          rightChain.push(y);

          if (!compare2Objects (x[p], y[p])) {
            return false;
          }

          leftChain.pop();
          rightChain.pop();
          break;

        default:
          if (x[p] !== y[p]) {
            return false;
          }
          break;
      }
    }

    return true;
  }

  if (arguments.length < 1) {
    return true; //Die silently? Don't know how to handle such case, please help...
    // throw "Need two or more arguments to compare";
  }

  for (var i = 1, l = arguments.length; i < l; i++) {

    leftChain = []; //todo: this can be cached
    rightChain = [];

    if (!compare2Objects(arguments[0], arguments[i])) {
      return false;
    }
  }

  return true;
}

function FireHose(host) {
  this.host = host;

  this.socketQueue = [];
  this.socket = new WebSocket(host)
  this.socket.queue = [];
  this.socket.sendQueue = function (cmd) {
    if (this.queue !== null) {
      this.queue.push(cmd);
    } else {
      this.send(cmd)
    }
  }

  this.currentTransactionId = 0;
  this.transactions = {};

  this.addTransaction = function(path, f, cb) {
    var id = this.currentTransactionId;
    var self = this;
    this.transactions[id] = new function() {
      var curobj = getObject(path, this.state);
      curobj = (typeof curobj === "undefined") ? null : curobj;
      var newobj = f(curobj);
      var msg = {
        "Transaction" : [id, "/"+path.join("/"), curobj, newobj]
      };
      self.socket.sendQueue(JSON.stringify(msg));
      this.cb = cb;
    }
    this.currentTransactionId++;
  }

  this.socket.onopen = function() {
    console.log(this.queue);
    for (var i = 0; i < this.queue.length; i++) {
      this.send(this.queue[i]);
    }
    this.queue = null;
  }

  var LISTENER_TYPES = ['#onvalue', '#child_modified', '#child_added', '#child_removed'];
  var EmptyListener = function() {
    var e = {};
    for (var i = 0; i < LISTENER_TYPES.length; i++) {
      e[LISTENER_TYPES[i]] = [];
    }
    return e;
  }
  this.listeners = new EmptyListener();
  this.state = {};

  var Snapshot = function(name, val) {
    this.name = function() { return (typeof name === "undefined") ? undefined : name; };
    this.val = function() { return (typeof val === "undefined") ? undefined : val; };
    this.child = function(str) {
      var path = str.toPathArray();
      if (path.length == 0) {
        return this;
      } else {
        return new Snapshot(path[path.length-1], getObject(path, this.val()));
      }
    }
    this.hasChild = function(str) {
      var path = str.toPathArray();
      return (typeof getObject(path, this.val()) !== "undefined");
    }
  }

  var setObject = function(path, obj, val) {
    for (var i = 0; i < path.length-1; i++) {
      if (typeof obj[path[i]] !== "object") {
        obj[path[i]] = {};
      }
      obj = obj[path[i]];
    }
    obj[path[path.length-1]] = val;
  }

  var removeObject = function(path, obj) {
    for (var i = 0; i < path.length-1; i++) {
      if (typeof obj[path[i]] !== "object") {
        obj[path[i]] = {};
      }
      obj = obj[path[i]];
    }
    delete obj[path[path.length-1]];
  }

  var getObject = function(path, obj) {
    for (var i = 0; i < path.length; i++) {
      obj = (typeof obj === "object" && obj != null) ? obj[path[i]] : undefined;
    }
    return obj;
  }

  var safeGet = function(obj, prop) {
    if (typeof obj === "object" && obj !== null) {
      return obj[prop];
    } else {
      return undefined;
    }
  }

  this.setData = function(path, val) {
    function notifyListeners(curprop, oldobj, newobj, l) {
      if (typeof l === "undefined") {
        return !deepCompare(oldobj, newobj);
      }
      var changed = false;
      if (typeof newobj === "object") {
        for (var prop in newobj) {
          if (newobj.hasOwnProperty(prop)) {
            var oo = (typeof oldobj === "undefined") ? undefined : oldobj[prop];
            if (typeof oo === "undefined") {
              for (var i = 0; i < l['#child_added'].length; i++) {
                l['#child_added'][i](new Snapshot(prop, newobj[prop]));
              }
              changed = true;
            }
            propChanged = notifyListeners(prop, oo, newobj[prop], l[prop]);
            if (propChanged && typeof oo !== "undefined") {
              for (var i = 0; i < l['#child_modified'].length; i++) {
                l['#child_modified'][i](new Snapshot(prop, newobj[prop]));
              }
            }
            changed = propChanged || changed;
          }
        }
      }
      if (typeof oldobj === "object") {
        for (var prop in oldobj) {
          if (oldobj.hasOwnProperty(prop)) {
            var no = (typeof newobj === "undefined") ? undefined : newobj[prop];
            if (typeof no === "undefined" || no === null) {
              for (var i = 0; i < l['#child_removed'].length; i++) {
                l['#child_removed'][i](new Snapshot(prop, null));
              }
              notifyListeners(prop, oo, newobj[prop], l[prop]);
              changed = true;
            }
          }
        }
      }
      changed = changed || (typeof newobj !== "undefined" &&
        typeof newobj !== "object" &&
        !deepCompare(newobj, oldobj));
        if (changed) {
          for (var i = 0; i < l['#onvalue'].length; i++) {
            l['#onvalue'][i](new Snapshot(curprop, newobj));
          }
        }
        return changed;
    }
    if (val === null) {
      var changed = typeof getObject(path, this.state) !== "undefined";
      var oldo = getObject(path, this.state);
      if (path.length > 0) {
        var l = getObject(path.slice(0,path.length-1), this.listeners);
        if (typeof oldo !== "undefined" && typeof l !== "undefined") {
          for (var j = 0; j < l['#child_removed'].length; j++) {
            l['#child_removed'][j](new Snapshot(path[path.length-1], null));
          }
        }
      }
      var l = getObject(path, this.listeners);
      if (typeof oldo !== "undefined" && typeof l !== "undefined") {
        for (var j = 0; j < l['#onvalue'].length; j++) {
          l['#onvalue'][j](new Snapshot(path[path.length-1], null));
        }
      }
      removeObject(path, this.state, val);
    } else {
      var changed = notifyListeners(path.length > 0 ? path[path.length-1] : null,
        getObject(path, this.state),
        val,
        getObject(path, this.listeners));
        if (path.length > 0) {
          var oldo = getObject(path, this.state);
          var l = getObject(path.slice(0,path.length-1), this.listeners);
          if (typeof oldo === "undefined" && typeof l !== "undefined") {
            for (var j = 0; j < l['#child_added'].length; j++) {
              l['#child_added'][j](new Snapshot(path[path.length-1], val));
            }
          }
        }
        setObject(path, this.state, val);
    }
    if (changed) {
      var l = this.listeners;
      var newo = this.state;
      for (var i = 0; i < path.length && typeof l !== "undefined"; i++) {
        if (typeof newo !== "undefined") {
          for (var j = 0; j < l['#onvalue'].length; j++) {
            l['#onvalue'][j](new Snapshot(path[i-1], newo));
          }
        }
        newo = safeGet(newo, path[i]);
        if (typeof newo !== "undefined") {
          for (var j = 0; j < l['#child_modified'].length; j++) {
            l['#child_modified'][j](new Snapshot(path[i], newo));
          }
        }
        l = l[path[i]];
      }
    }
  }

  this.socket.onmessage = function(fh) {
    return function(msg) {
      var obj = JSON.parse(msg.data);
      if (obj.hasOwnProperty('DataChanged')) {
        var path = obj['DataChanged'][0].toPathArray();
        var val = obj['DataChanged'][1];
        fh.setData(path, val);
      } else {
        if (obj.hasOwnProperty('TransactionSuccessful')) {
          var id = obj['TransactionSuccessful'];
          var success = true;
        } else if (obj.hasOwnProperty('TransactionFailed')) {
          var id = obj['TransactionFailed'];
          var success = false;
        }
        var trans = fh.transactions[id];
        if (typeof trans !== "undefined") {
          trans.cb(false, success);
          delete fh.transactions[id];
        } else {
          console.log("Couldn't find transaction: " + id);
        }
      }
    }
  }(this);

  this.registerListener = function(path, cb, type) {
    var l = this.listeners;
    var found = false;
    for (var i = 0; i < path.length; i++) {
      for (var j = 0; j < LISTENER_TYPES.length; j++) {
        if (l[LISTENER_TYPES[j]].length > 0) {
          found = true;
        }
      }
      if (typeof l[path[i]] !== "object") {
        l[path[i]] = new EmptyListener();
      }
      l = l[path[i]];
    }
    for (var j = 0; j < LISTENER_TYPES.length; j++) {
      if (l[LISTENER_TYPES[j]].length > 0) {
        found = true;
      }
    }
    l[type].push(cb);
    if (!found) {
      var cmd = JSON.stringify({"RegisterListener":"/"+path.join("/")});
      this.socket.sendQueue(cmd);
    } else {
      /* HACK: get the current value */
      var o = getObject(path, this.state);
      o = (typeof o === "undefined") ? null : o;
      cb(new Snapshot(path[path.length-1], o)); // HACK: first register with null
    }
  }

  this.deregisterListener = function(path, cb) {
  }

  this.hose = function(path) {
    return new function(fh) {
      this.fireHose = fh;
      this.path = (Object.prototype.toString.call(path) === '[object Array]') ? path : path.toPathArray();

      this.onValue = function(cb) {
        this.fireHose.registerListener(this.path, cb, "#onvalue");
      }
      this.childAdded = function(cb) {
        this.fireHose.registerListener(this.path, cb, "#child_added");
      }
      this.childRemoved = function(cb) {
        this.fireHose.registerListener(this.path, cb, "#child_removed");
      }
      this.childModified = function(cb) {
        this.fireHose.registerListener(this.path, cb, "#child_modified");
      }
      this.$internalSet = function(path, val) {
        this.fireHose.socket.sendQueue(JSON.stringify({"SetData":["/"+this.path.join("/"),val]}));
        this.fireHose.setData(path, val)
      }
      this.set = function(val) {
        this.$internalSet(this.path, val);
      }
      this.remove = function() {
        this.$internalSet(this.path, null);
      }
      this.push = function(val) {
        var time = new Date().getTime();
        this.$internalSet(this.path.ipush(time), val);
      }
      this.transaction = function(f,cb) {
        this.fireHose.addTransaction(this.path, f, cb);
      }
      this.child = function(child) {
        return fh.hose(this.path.concat(child.toPathArray()));
      }
      this.parent = function() {
        return fh.hose(this.path.slice(0,this.path.length=1));
      }
      this.onDisconnect = function(cb) {
        var self = this;
        window.addEventListener("beforeunload", function(e){
          cb(self);
        }, false);
      }
    }(this);
  }
}
