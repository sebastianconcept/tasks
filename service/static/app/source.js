/* ====================================================================
   |
   |   Amber Smalltalk
   |   http://amber-lang.net
   |
   ======================================================================

   ======================================================================
   |
   | Copyright (c) 2010-2011
   | Nicolas Petton <petton.nicolas@gmail.com>
   |
   | Amber is released under the MIT license
   |
   | Permission is hereby granted, free of charge, to any person obtaining
   | a copy of this software and associated documentation files (the
   | 'Software'), to deal in the Software without restriction, including
   | without limitation the rights to use, copy, modify, merge, publish,
   | distribute, sublicense, and/or sell copies of the Software, and to
   | permit persons to whom the Software is furnished to do so, subject to
   | the following conditions:
   |
   | The above copyright notice and this permission notice shall be
   | included in all copies or substantial portions of the Software.
   |
   | THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
   | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   |
   ==================================================================== */

/* Make sure that console is defined */

if(typeof console === "undefined") {
	this.console = {
		log: function() {},
		warn: function() {},
		info: function() {},
		debug: function() {},
		error: function() {}
	};
}

/* Array extensions */

Array.prototype.addElement = function(el) {
	if(typeof el === 'undefined') { return; }
	if(this.indexOf(el) == -1) {
        this.push(el);
    }
};

Array.prototype.removeElement = function(el) {
    var i = this.indexOf(el);
    if (i !== -1) { this.splice(i, 1); }
};


/* Smalltalk constructors definition */

function SmalltalkObject() {}
function SmalltalkBehavior() {}
function SmalltalkClass() {}
function SmalltalkMetaclass() {
	this.meta = true;
}
function SmalltalkPackage() {}
function SmalltalkMethod() {}
function SmalltalkNil() {}

function SmalltalkOrganizer() {
}

function SmalltalkPackageOrganizer() {
    this.elements = [];
}

function SmalltalkClassOrganizer() {
    this.elements = [];
}

function inherits(child, parent) {
	child.prototype = Object.create(parent.prototype, {
		constructor: { value: child,
			enumerable: false, configurable: true, writable: true }
	});
}

inherits(SmalltalkBehavior, SmalltalkObject);
inherits(SmalltalkClass, SmalltalkBehavior);
inherits(SmalltalkMetaclass, SmalltalkBehavior);
inherits(SmalltalkNil, SmalltalkObject);
inherits(SmalltalkMethod, SmalltalkObject);
inherits(SmalltalkPackage, SmalltalkObject);
inherits(SmalltalkOrganizer, SmalltalkObject);
inherits(SmalltalkPackageOrganizer, SmalltalkOrganizer);
inherits(SmalltalkClassOrganizer, SmalltalkOrganizer);


function Smalltalk() {

	var st = this;

	/* This is the current call context object. While it is publicly available,
	   Use smalltalk.getThisContext() instead which will answer a safe copy of
	   the current context */

	st.thisContext = undefined;

	/* List of all reserved words in JavaScript. They may not be used as variables
	   in Smalltalk. */

	// list of reserved JavaScript keywords as of
	//   http://es5.github.com/#x7.6.1.1
	// and
	//   http://people.mozilla.org/~jorendorff/es6-draft.html#sec-7.6.1
	st.reservedWords = ['break', 'case', 'catch', 'continue', 'debugger',
		'default', 'delete', 'do', 'else', 'finally', 'for', 'function',
		'if', 'in', 'instanceof', 'new', 'return', 'switch', 'this', 'throw',
		'try', 'typeof', 'var', 'void', 'while', 'with',
		// ES5: future use: http://es5.github.com/#x7.6.1.2
		'class', 'const', 'enum', 'export', 'extends', 'import', 'super',
		// ES5: future use in strict mode
		'implements', 'interface', 'let', 'package', 'private', 'protected',
		'public', 'static', 'yield'];

    var initialized = false;

    /* Smalltalk classes */

    var classes = [];
    var wrappedClasses = [];

    /* Method not implemented handlers */

	var dnu = {
		methods: [],
		selectors: [],

		get: function (string) {
			var index = this.selectors.indexOf(string);
			if(index !== -1) {
				return this.methods[index];
			}
			this.selectors.push(string);
			var selector = st.selector(string);
			var method = {jsSelector: selector, fn: this.createHandler(selector)};
			this.methods.push(method);
			return method;
		},

		/* Dnu handler method */

		createHandler: function (selector) {
			return function () {
				var args = Array.prototype.slice.call(arguments);
				return messageNotUnderstood(this, selector, args);
			};
		}
	};

    /* Answer all method selectors based on dnu handlers */

    st.allSelectors = function() {
        return dnu.selectors;
    };

	/* Unique ID number generator */

	var oid = 0;
	st.nextId = function() {
		oid += 1;
		return oid;
	};

	/* We hold all Packages in a separate Object */

	st.packages = {};

	/* Smalltalk package creation. To add a Package, use smalltalk.addPackage() */

	function pkg(spec) {
		var that = new SmalltalkPackage();
		that.pkgName = spec.pkgName;
        that.organization = new SmalltalkPackageOrganizer();
		that.properties = spec.properties || {};
		return that;
	}

	/* Smalltalk class creation. A class is an instance of an automatically
	   created metaclass object. Newly created classes (not their metaclass) 
	   should be added to the smalltalk object, see smalltalk.addClass().
	   Superclass linking is *not* handled here, see smalltalk.init()  */

	function klass(spec) {
		spec = spec || {};
		var meta = metaclass(spec);
		var that = meta.instanceClass;
		that.fn = spec.fn || function() {};
		setupClass(that, spec);

        that.className = spec.className;
        that.wrapped   = spec.wrapped || false;
		meta.className = spec.className + ' class';
		if(spec.superclass) {
			that.superclass = spec.superclass;
			meta.superclass = spec.superclass.klass;
		}
		return that;
	}

	function metaclass(spec) {
		spec = spec || {};
		var that = new SmalltalkMetaclass();
		inherits(
			that.fn = function() {},
			spec.superclass ? spec.superclass.klass.fn : SmalltalkClass
		);
		that.instanceClass = new that.fn();
        setupClass(that);
		return that;
	}

	function setupClass(klass, spec) {
        spec = spec || {};
		klass.iVarNames = spec.iVarNames || [];
		klass.pkg = spec.pkg;

        Object.defineProperty(klass, "toString", {
			value: function() { return 'Smalltalk ' + this.className; },
            enumerable:false, configurable: true, writable: false
		});

		klass.organization          = new SmalltalkClassOrganizer();
        klass.organization.theClass = klass;

		Object.defineProperty(klass, "methods", {
			value: {},
			enumerable: false, configurable: true, writable: true
		});
		wireKlass(klass);
	}

	/* Smalltalk method object. To add a method to a class,
	   use smalltalk.addMethod() */

	st.method = function(spec) {
		var that = new SmalltalkMethod();
		that.selector          = spec.selector;
		that.jsSelector        = spec.jsSelector;
		that.args              = spec.args || {};
		that.category          = spec.category;
		that.source            = spec.source;
		that.messageSends      = spec.messageSends || [];
		that.referencedClasses = spec.referencedClasses || [];
		that.fn                = spec.fn;
		return that;
	};

	/* Initialize a class in its class hierarchy. Handle both classes and
	   metaclasses. */

	st.init = function(klass) {
		st.initClass(klass);
		if(klass.klass && !klass.meta) {
			st.initClass(klass.klass);
		}
	};

    st.initClass = function(klass) {
        if(klass.wrapped) {
            copySuperclass(klass);
        }
        else {
            installSuperclass(klass);
        }

        if(klass === st.Object || klass.wrapped) {
            installDnuHandlers(klass);
        }
    };

	function wireKlass(klass) {
		Object.defineProperty(klass.fn.prototype, "klass", {
			value: klass,
			enumerable: false, configurable: true, writable: true
		});
	}

	function installSuperclass(klass) {
        // only if the klass has not been initialized yet.
		if(klass.fn.prototype._yourself) { return; }

		if(klass.superclass && klass.superclass !== nil) {
            inherits(klass.fn, klass.superclass.fn);
			wireKlass(klass);
			reinstallMethods(klass);
        }
	}

	function copySuperclass(klass, superclass) {
		for (superclass = superclass || klass.superclass;
			 superclass && superclass !== nil;
			 superclass = superclass.superclass) {
			for (var keys = Object.keys(superclass.methods), i = 0; i < keys.length; i++) {
				installMethodIfAbsent(superclass.methods[keys[i]], klass);
			}
		}
	}

	function installMethod(method, klass) {
        Object.defineProperty(klass.fn.prototype, method.jsSelector, {
			value: method.fn,
			enumerable: false, configurable: true, writable: true
		});
	}

	function installMethodIfAbsent(method, klass) {
		if(!klass.fn.prototype[method.jsSelector]) {
			installMethod(method, klass);
		}
	}

	function reinstallMethods(klass) {
        for(var keys = Object.keys(klass.methods), i=0; i<keys.length; i++) {
            installMethod(klass.methods[keys[i]], klass);
		}
	}

	function installDnuHandlers(klass) {
		var m = dnu.methods;
        for(var i=0; i<m.length; i++) {
			installMethodIfAbsent(m[i], klass);
        }
	}

	function installNewDnuHandler(newHandler) {
		installMethodIfAbsent(newHandler, st.Object);
		for(var i = 0; i < wrappedClasses.length; i++) {
			installMethodIfAbsent(newHandler, wrappedClasses[i]);
		}
	}

	/* Answer all registered Packages as Array */
    // TODO: Remove this hack

	st.packages.all = function() {
		var packages = [];
		for(var i in st.packages) {
			if(!st.packages.hasOwnProperty(i) || typeof(st.packages[i]) === "function") continue;
			packages.push(st.packages[i]);
		}
		return packages
	};

	/* Answer all registered Smalltalk classes */
    //TODO: remove the function and make smalltalk.classes an array

	st.classes = function() {
		return classes;
	};

    st.wrappedClasses = function() {
        return wrappedClasses;
    };

	/* Answer the direct subclasses of klass. */

	st.subclasses = function(klass) {
		var subclasses = [];
		var classes = st.classes();
		for(var i=0; i < classes.length; i++) {
			var c = classes[i];
			if(c.fn) {
				//Classes
				if(c.superclass === klass) {
					subclasses.push(c);
				}
				c = c.klass;
				//Metaclasses
				if(c && c.superclass === klass) {
					subclasses.push(c);
				}
			}
		}
		return subclasses;
	};

	/* Create a new class wrapping a JavaScript constructor, and add it to the
	   global smalltalk object. Package is lazily created if it does not exist with given name. */

	st.wrapClassName = function(className, pkgName, fn, superclass, wrapped) {
        if(wrapped !== false) {
            wrapped = true;
        }
		var pkg = st.addPackage(pkgName);
		st[className] = klass({
			className:  className,
			superclass: superclass,
			pkg:        pkg,
			fn:         fn,
            wrapped:    wrapped
		});

        classes.addElement(st[className]);
		if(wrapped) {wrappedClasses.addElement(st[className])}
		pkg.organization.elements.addElement(st[className]);
	};

	/* Create an alias for an existing class */

	st.alias = function(klass, alias) {
		st[alias] = klass;
	};

	/* Add a package to the smalltalk.packages object, creating a new one if needed.
	   If pkgName is null or empty we return nil, which is an allowed package for a class.
	   If package already exists we still update the properties of it. */

	st.addPackage = function(pkgName, properties) {
		if(!pkgName) {return nil;}
		if(!(st.packages[pkgName])) {
			st.packages[pkgName] = pkg({
				pkgName: pkgName,
				properties: properties
			});
		} else {
			if(properties) {
				st.packages[pkgName].properties = properties;
			}
		}
		return st.packages[pkgName];
	};

	/* Add a class to the smalltalk object, creating a new one if needed.
	   A Package is lazily created if it does not exist with given name. */

	st.addClass = function(className, superclass, iVarNames, pkgName) {
		var pkg = st.addPackage(pkgName);
        if (superclass == nil) { superclass = null; }
		if(st[className] && st[className].superclass == superclass) {
			st[className].superclass = superclass;
			st[className].iVarNames = iVarNames;
			st[className].pkg = pkg || st[className].pkg;
		} else {
            if(st[className]) {
                st.removeClass(st[className]);
			}
			st[className] = klass({
				className: className,
				superclass: superclass,
				pkg: pkg,
				iVarNames: iVarNames
			});
		}

        classes.addElement(st[className]);
        pkg.organization.elements.addElement(st[className]);
	};

    st.removeClass = function(klass) {
        klass.pkg.organization.elements.removeElement(klass);
        classes.removeElement(klass);
        delete st[klass.className];
    };

	/* 
     * Add/remove a method to/from a class 
     */

    /* This is a temporary version of addMethod() for backward compatibility */
	st.addMethod = function(method_exJsSelector, klass_exMethod, exKlass) {
        if (typeof method_exJsSelector === "string") { //legacy
            if (method_exJsSelector !== st.selector(klass_exMethod.selector)) {
                console.log("DISCREPANCY: arg, in_method");
                console.log(method_exJsSelector);
                console.log(st.selector(klass_exMethod.selector));
                klass_exMethod.jsSelector = method_exJsSelector;
            }
            return new_addMethod(klass_exMethod, exKlass);
        }

        return new_addMethod(method_exJsSelector, klass_exMethod);
    }

    // later, st.addMethod can be this:
    function new_addMethod(method, klass) {
        if (!(method.jsSelector)) {
            method.jsSelector = st.selector(method.selector);
        }
		installMethod(method, klass);
		klass.methods[method.selector] = method;
		method.methodClass = klass;

        // During the bootstrap, #addCompiledMethod is not used.
        // Therefore we populate the organizer here too
        klass.organization.elements.addElement(method.category);

        for(var i=0; i<method.messageSends.length; i++) {
            var dnuHandler = dnu.get(method.messageSends[i]);
            if(initialized) {
                installNewDnuHandler(dnuHandler);
			}
		}
	};

    st.removeMethod = function(method) {
        var protocol = method.category;
        var klass = method.methodClass;

        delete klass.fn.prototype[st.selector(method.selector)];
	    delete klass.methods[method.selector];

		var selectors = Object.keys(klass.methods);
        // Do *not* delete protocols from here.
        // This is handled by #removeCompiledMethod
    };

	/* Handles unhandled errors during message sends */
    // simply send the message and handle #dnu:

	st.send = function(receiver, selector, args, klass) {
		var method;
		if(receiver == null) {
			receiver = nil;
		}
		method = klass ? klass.fn.prototype[selector] : receiver.klass && receiver[selector];
		if(method) {
            return method.apply(receiver, args);
		} else {
			return messageNotUnderstood(receiver, selector, args);
		}
	}

	st.withContext = function(worker, setup) {
		if(st.thisContext) {
            st.thisContext.pc++;
			return inContext(worker, setup);
		} else {
			try {return inContext(worker, setup)}
			catch(error) {
				if(error.smalltalkError) {
					handleError(error);
                } else {
                    var errorWrapper = st.JavaScriptException._on_(error);
                    try {errorWrapper._signal()} catch(ex) {}
                    errorWrapper._context_(st.getThisContext());
                    handleError(errorWrapper);
                }
				// Reset the context stack in any case
				st.thisContext = undefined;
                // Throw the exception anyway, as we want to stop
                // the execution to avoid infinite loops
				throw error;
			}
		}
	};

	function inContext(worker, setup) {
		var context = pushContext(setup);
		var result = worker(context);
		popContext(context);
		return result;
	}

	/* Handles Smalltalk errors. Triggers the registered ErrorHandler
	   (See the Smalltalk class ErrorHandler and its subclasses */

	function handleError(error) {
        st.ErrorHandler._current()._handleError_(error);
	}

	/* Handles #dnu: *and* JavaScript method calls.
	   if the receiver has no klass, we consider it a JS object (outside of the
	   Amber system). Else assume that the receiver understands #doesNotUnderstand: */

	function messageNotUnderstood(receiver, selector, args) {
		/* Handles JS method calls. */
		if(receiver.klass === undefined || receiver.allowJavaScriptCalls) {
			return callJavaScriptMethod(receiver, selector, args);
		}

		/* Handles not understood messages. Also see the Amber counter-part
		   Object>>doesNotUnderstand: */

		return receiver._doesNotUnderstand_(
			st.Message._new()
				._selector_(st.convertSelector(selector))
				._arguments_(args)
		);
	}

	/* Call a method of a JS object, or answer a property if it exists.
	   Else try wrapping a JSObjectProxy around the receiver.

       If the object property is a function, then call it, except if it starts with
       an uppercase character (we probably want to answer the function itself in this
       case and send it #new from Amber).

	   Converts keyword-based selectors by using the first
	   keyword only, but keeping all message arguments.

	   Example:
	   "self do: aBlock with: anObject" -> "self.do(aBlock, anObject)" */

	function callJavaScriptMethod(receiver, selector, args) {
		var jsSelector = selector._asJavaScriptSelector();
		var jsProperty = receiver[jsSelector];
		if(typeof jsProperty === "function" && !/^[A-Z]/.test(jsSelector)) {
			return jsProperty.apply(receiver, args);
		} else if(jsProperty !== undefined) {
			if(args[0]) {
				receiver[jsSelector] = args[0];
				return nil;
			} else {
				return jsProperty;
			}
		}

		return st.send(st.JSObjectProxy._on_(receiver), selector, args);
	}

	/* Handle thisContext pseudo variable */

	st.getThisContext = function() {
        if(st.thisContext) {
		    st.thisContext.init();
            return st.thisContext;
        } else {
            return nil;
        }
	};

	function pushContext(setup) {
		return st.thisContext = new SmalltalkMethodContext(smalltalk.thisContext, setup);
	}

	function popContext(context) {
		st.thisContext = context.homeContext;
	}

	/* Convert a Smalltalk selector into a JS selector */

    st.selector = function(string) {
        var selector = '_' + string;
	    selector = selector.replace(/:/g, '_');
	    selector = selector.replace(/[\&]/g, '_and');
	    selector = selector.replace(/[\|]/g, '_or');
	    selector = selector.replace(/[+]/g, '_plus');
	    selector = selector.replace(/-/g, '_minus');
	    selector = selector.replace(/[*]/g ,'_star');
	    selector = selector.replace(/[\/]/g ,'_slash');
	    selector = selector.replace(/[\\]/g ,'_backslash');
	    selector = selector.replace(/[\~]/g ,'_tild');
	    selector = selector.replace(/>/g ,'_gt');
	    selector = selector.replace(/</g ,'_lt');
	    selector = selector.replace(/=/g ,'_eq');
	    selector = selector.replace(/,/g ,'_comma');
	    selector = selector.replace(/[@]/g ,'_at');
        return selector
    };

	/* Convert a string to a valid smalltalk selector.
	   if you modify the following functions, also change String>>asSelector
	   accordingly */

	st.convertSelector = function(selector) {
		if(selector.match(/__/)) {
			return convertBinarySelector(selector);
		} else {
			return convertKeywordSelector(selector);
		}
	};

	function convertKeywordSelector(selector) {
		return selector.replace(/^_/, '').replace(/_/g, ':');
	}

	function convertBinarySelector(selector) {
		return selector
			.replace(/^_/, '')
			.replace(/_and/g, '&')
			.replace(/_or/g, '|')
			.replace(/_plus/g, '+')
			.replace(/_minus/g, '-')
			.replace(/_star/g, '*')
			.replace(/_slash/g, '/')
			.replace(/_backslash/g, '\\')
			.replace(/_tild/g, '~')
			.replace(/_gt/g, '>')
			.replace(/_lt/g, '<')
			.replace(/_eq/g, '=')
			.replace(/_comma/g, ',')
			.replace(/_at/g, '@')
	}

	/* Converts a JavaScript object to valid Smalltalk Object */
	st.readJSObject = function(js) {
		var object = js;
		var readObject = (js.constructor === Object);
		var readArray = (js.constructor === Array);

		if(readObject) {
			object = st.Dictionary._new();
		}
		for(var i in js) {
			if(readObject) {
				object._at_put_(i, st.readJSObject(js[i]));
			}
			if(readArray) {
				object[i] = st.readJSObject(js[i]);
			}
		}
		return object;
	};

    /* Boolean assertion */
    st.assert = function(shouldBeBoolean) {
        if ((undefined !== shouldBeBoolean) && (shouldBeBoolean.klass === smalltalk.Boolean)) {
            return shouldBeBoolean == true;
        } else {
            smalltalk.NonBooleanReceiver._new()._object_(shouldBeBoolean)._signal();
        }
    };

    /* Backward compatibility with Amber 0.9.1 */
    st.symbolFor = function(aString) { return aString; }

    /* Smalltalk initialization. Called on page load */

    st.initialize = function() {
		if(initialized) { return; }

		classes.forEach(function(klass) {
            st.init(klass);
        });
        classes.forEach(function(klass) {
            klass._initialize();
        });

        initialized = true;
    };
}

inherits(Smalltalk, SmalltalkObject);

function SmalltalkMethodContext(home, setup) {
	this.homeContext = home;
    this.setup       = setup || function() {};
    this.pc          = 0;
}

// Fallbacks
SmalltalkMethodContext.prototype.locals = {};
SmalltalkMethodContext.prototype.receiver = null;
SmalltalkMethodContext.prototype.selector = null;
SmalltalkMethodContext.prototype.lookupClass = null;

inherits(SmalltalkMethodContext, SmalltalkObject);

SmalltalkMethodContext.prototype.fill = function(receiver, selector, locals, lookupClass) {
    this.receiver    = receiver;
    this.selector    = selector;
    this.locals      = locals || {};
    this.lookupClass = lookupClass;
};

SmalltalkMethodContext.prototype.fillBlock = function(locals, ctx) {
    this.locals        = locals || {};
    this.methodContext = ctx;
};

SmalltalkMethodContext.prototype.init = function() {
	var home = this.homeContext;
	if(home) {home = home.init()}

    this.setup(this);
};

SmalltalkMethodContext.prototype.method = function() {
    var method;
    var lookup = this.lookupClass || this.receiver.klass;
    while(!method && lookup) {
        method = lookup.methods[smalltalk.convertSelector(this.selector)];
        lookup = lookup.superclass
    }
    return method;
};

// TODO: this is just wrong :)
SmalltalkMethodContext.prototype.resume = function() {
    //Brutally set the receiver as thisContext, then re-enter the function
    smalltalk.thisContext = this;
    return this.method.apply(receiver, temps);
};

/* Global Smalltalk objects. */

var nil = new SmalltalkNil();
var smalltalk = new Smalltalk();

if(this.jQuery) {
	this.jQuery.allowJavaScriptCalls = true;
}

/*
 * Answer the smalltalk representation of o.
 * Used in message sends
 */

var _st = function(o) {
	if(o == null) {return nil}
	if(o.klass) {return o}
	return smalltalk.JSObjectProxy._on_(o);
}; 


/***************************************** BOOTSTRAP ******************************************/

smalltalk.wrapClassName("Object", "Kernel-Objects", SmalltalkObject, undefined, false);
smalltalk.wrapClassName("Behavior", "Kernel-Classes", SmalltalkBehavior, smalltalk.Object, false);
smalltalk.wrapClassName("Metaclass", "Kernel-Classes", SmalltalkMetaclass, smalltalk.Behavior, false);
smalltalk.wrapClassName("Class", "Kernel-Classes", SmalltalkClass, smalltalk.Behavior, false);

smalltalk.Object.klass.superclass = smalltalk.Class;


smalltalk.wrapClassName("Smalltalk", "Kernel-Objects", Smalltalk, smalltalk.Object, false);
smalltalk.wrapClassName("Package", "Kernel-Objects", SmalltalkPackage, smalltalk.Object, false);
smalltalk.wrapClassName("CompiledMethod", "Kernel-Methods", SmalltalkMethod, smalltalk.Object, false);
smalltalk.wrapClassName("Organizer", "Kernel-Objects", SmalltalkOrganizer, smalltalk.Object, false);
smalltalk.wrapClassName("PackageOrganizer", "Kernel-Objects", SmalltalkPackageOrganizer, smalltalk.Organizer, false);
smalltalk.wrapClassName("ClassOrganizer", "Kernel-Objects", SmalltalkClassOrganizer, smalltalk.Organizer, false);


smalltalk.wrapClassName("Number", "Kernel-Objects", Number, smalltalk.Object);
smalltalk.wrapClassName("BlockClosure", "Kernel-Methods", Function, smalltalk.Object);
smalltalk.wrapClassName("Boolean", "Kernel-Objects", Boolean, smalltalk.Object);
smalltalk.wrapClassName("Date", "Kernel-Objects", Date, smalltalk.Object);
smalltalk.wrapClassName("UndefinedObject", "Kernel-Objects", SmalltalkNil, smalltalk.Object, false);

smalltalk.addClass("Collection", smalltalk.Object, null, "Kernel-Collections");
smalltalk.addClass("IndexableCollection", smalltalk.Collection, null, "Kernel-Collections");
smalltalk.addClass("SequenceableCollection", smalltalk.IndexableCollection, null, "Kernel-Collections");
smalltalk.addClass("CharacterArray", smalltalk.SequenceableCollection, null, "Kernel-Collections");
smalltalk.wrapClassName("String", "Kernel-Collections", String, smalltalk.CharacterArray);
smalltalk.wrapClassName("Array", "Kernel-Collections", Array, smalltalk.SequenceableCollection);
smalltalk.wrapClassName("RegularExpression", "Kernel-Collections", RegExp, smalltalk.Object);

smalltalk.wrapClassName("Error", "Kernel-Exceptions", Error, smalltalk.Object);
smalltalk.wrapClassName("MethodContext", "Kernel-Methods", SmalltalkMethodContext, smalltalk.Object, false);

/* Alias definitions */

smalltalk.alias(smalltalk.Array, "OrderedCollection");
smalltalk.alias(smalltalk.Date, "Time");
smalltalk.addPackage('Kernel-Objects');
smalltalk.addClass('Object', smalltalk.nil, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "->",
fn: function (anObject){
var self=this;
function $Association(){return smalltalk.Association||(typeof Association=="undefined"?nil:Association)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Association())._key_value_(self,anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"->",{anObject:anObject},smalltalk.Object)})},
messageSends: ["key:value:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self).__eq_eq(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"=",{anObject:anObject},smalltalk.Object)})},
messageSends: ["=="]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "==",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._identityHash()).__eq(_st(anObject)._identityHash());
return $1;
}, function($ctx1) {$ctx1.fill(self,"==",{anObject:anObject},smalltalk.Object)})},
messageSends: ["=", "identityHash"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSON",
fn: function (){
var self=this;
var variables;
function $HashedCollection(){return smalltalk.HashedCollection||(typeof HashedCollection=="undefined"?nil:HashedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $1;
variables=_st($HashedCollection())._new();
_st(_st(_st(self)._class())._allInstanceVariableNames())._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(variables)._at_put_(each,_st(_st(self)._instVarAt_(each))._asJSON());
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$1=variables;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSON",{variables:variables},smalltalk.Object)})},
messageSends: ["new", "do:", "at:put:", "asJSON", "instVarAt:", "allInstanceVariableNames", "class"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSONString",
fn: function (){
var self=this;
function $JSON(){return smalltalk.JSON||(typeof JSON=="undefined"?nil:JSON)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($JSON())._stringify_(_st(self)._asJSON());
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSONString",{},smalltalk.Object)})},
messageSends: ["stringify:", "asJSON"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "asJavascript",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._asString();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJavascript",{},smalltalk.Object)})},
messageSends: ["asString"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "asString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._printString();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asString",{},smalltalk.Object)})},
messageSends: ["printString"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "basicAt:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self[aString];
return self}, function($ctx1) {$ctx1.fill(self,"basicAt:",{aString:aString},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "basicAt:put:",
fn: function (aString,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self[aString] = anObject;
return self}, function($ctx1) {$ctx1.fill(self,"basicAt:put:",{aString:aString,anObject:anObject},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "basicDelete:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
delete self[aString]; return aString;
return self}, function($ctx1) {$ctx1.fill(self,"basicDelete:",{aString:aString},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "basicPerform:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicPerform_withArguments_(aString,[]);
return $1;
}, function($ctx1) {$ctx1.fill(self,"basicPerform:",{aString:aString},smalltalk.Object)})},
messageSends: ["basicPerform:withArguments:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "basicPerform:withArguments:",
fn: function (aString,aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self[aString].apply(self, aCollection);;
return self}, function($ctx1) {$ctx1.fill(self,"basicPerform:withArguments:",{aString:aString,aCollection:aCollection},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "class",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.klass;
return self}, function($ctx1) {$ctx1.fill(self,"class",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "copy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._shallowCopy())._postCopy();
return $1;
}, function($ctx1) {$ctx1.fill(self,"copy",{},smalltalk.Object)})},
messageSends: ["postCopy", "shallowCopy"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "deepCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var copy = self.klass._new();
		for(var i in self) {
		if(/^@.+/.test(i)) {
			copy[i] = self[i]._deepCopy();
		}
		}
		return copy;
	;
return self}, function($ctx1) {$ctx1.fill(self,"deepCopy",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "deprecatedAPI",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(console)._warn_(_st(_st(_st(_st(_st(smalltalk.getThisContext())._home())._asString()).__comma(" is deprecated! (in ")).__comma(_st(_st(_st(smalltalk.getThisContext())._home())._home())._asString())).__comma(")"));
return self}, function($ctx1) {$ctx1.fill(self,"deprecatedAPI",{},smalltalk.Object)})},
messageSends: ["warn:", ",", "asString", "home"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "doesNotUnderstand:",
fn: function (aMessage){
var self=this;
function $MessageNotUnderstood(){return smalltalk.MessageNotUnderstood||(typeof MessageNotUnderstood=="undefined"?nil:MessageNotUnderstood)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st($MessageNotUnderstood())._new();
_st($1)._receiver_(self);
_st($1)._message_(aMessage);
$2=_st($1)._signal();
return self}, function($ctx1) {$ctx1.fill(self,"doesNotUnderstand:",{aMessage:aMessage},smalltalk.Object)})},
messageSends: ["receiver:", "new", "message:", "signal"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "error:",
fn: function (aString){
var self=this;
function $Error(){return smalltalk.Error||(typeof Error=="undefined"?nil:Error)}
return smalltalk.withContext(function($ctx1) { 
_st($Error())._signal_(aString);
return self}, function($ctx1) {$ctx1.fill(self,"error:",{aString:aString},smalltalk.Object)})},
messageSends: ["signal:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "halt",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._error_("Halt encountered");
return self}, function($ctx1) {$ctx1.fill(self,"halt",{},smalltalk.Object)})},
messageSends: ["error:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "identityHash",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

	var hash=self.identityHash;
	if (hash) return hash;
	hash=smalltalk.nextId();
	Object.defineProperty(self, 'identityHash', {value:hash});
	return hash;
	;
return self}, function($ctx1) {$ctx1.fill(self,"identityHash",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNil:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifNil:",{aBlock:aBlock},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNil:ifNotNil:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(anotherBlock)._value_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifNil:ifNotNil:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.Object)})},
messageSends: ["value:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNotNil:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(aBlock)._value_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifNotNil:",{aBlock:aBlock},smalltalk.Object)})},
messageSends: ["value:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNotNil:ifNil:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(aBlock)._value_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifNotNil:ifNil:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.Object)})},
messageSends: ["value:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "instVarAt:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 return self['@'+aString] ;
return self}, function($ctx1) {$ctx1.fill(self,"instVarAt:",{aString:aString},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "instVarAt:put:",
fn: function (aString,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 self['@' + aString] = anObject ;
return self}, function($ctx1) {$ctx1.fill(self,"instVarAt:put:",{aString:aString,anObject:anObject},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isBehavior",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isBehavior",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isBoolean",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isBoolean",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isClass",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isCompiledMethod",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isCompiledMethod",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isImmutable",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isImmutable",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isKindOf:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._isMemberOf_(aClass);
if(smalltalk.assert($2)){
$1=true;
} else {
$1=_st(_st(self)._class())._inheritsFrom_(aClass);
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"isKindOf:",{aClass:aClass},smalltalk.Object)})},
messageSends: ["ifTrue:ifFalse:", "inheritsFrom:", "class", "isMemberOf:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isMemberOf:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class()).__eq(aClass);
return $1;
}, function($ctx1) {$ctx1.fill(self,"isMemberOf:",{aClass:aClass},smalltalk.Object)})},
messageSends: ["=", "class"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isMetaclass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isMetaclass",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isNil",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isNil",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isNumber",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isNumber",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isPackage",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isPackage",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isParseFailure",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isParseFailure",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isString",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "isSymbol",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isSymbol",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "notNil",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._isNil())._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"notNil",{},smalltalk.Object)})},
messageSends: ["not", "isNil"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "perform:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._perform_withArguments_(aString,[]);
return $1;
}, function($ctx1) {$ctx1.fill(self,"perform:",{aString:aString},smalltalk.Object)})},
messageSends: ["perform:withArguments:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "perform:withArguments:",
fn: function (aString,aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return smalltalk.send(self, aString._asSelector(), aCollection);
return self}, function($ctx1) {$ctx1.fill(self,"perform:withArguments:",{aString:aString,aCollection:aCollection},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "postCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"postCopy",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$2;
$1=aStream;
$3=_st(_st(_st(_st(self)._class())._name())._first())._isVowel();
if(smalltalk.assert($3)){
$2="an ";
} else {
$2="a ";
};
_st($1)._nextPutAll_($2);
_st(aStream)._nextPutAll_(_st(_st(self)._class())._name());
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Object)})},
messageSends: ["nextPutAll:", "ifTrue:ifFalse:", "isVowel", "first", "name", "class"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "printString",
fn: function (){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($String())._streamContents_((function(stream){
return smalltalk.withContext(function($ctx2) {
return _st(self)._printOn_(stream);
}, function($ctx2) {$ctx2.fillBlock({stream:stream},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"printString",{},smalltalk.Object)})},
messageSends: ["streamContents:", "printOn:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "putOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aStream)._nextPut_(self);
return self}, function($ctx1) {$ctx1.fill(self,"putOn:",{aStream:aStream},smalltalk.Object)})},
messageSends: ["nextPut:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "respondsTo:",
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._canUnderstand_(aSelector);
return $1;
}, function($ctx1) {$ctx1.fill(self,"respondsTo:",{aSelector:aSelector},smalltalk.Object)})},
messageSends: ["canUnderstand:", "class"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "shallowCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var copy = self.klass._new();
		for(var i in self) {
		if(/^@.+/.test(i)) {
			copy[i] = self[i];
		}
		}
		return copy;
	;
return self}, function($ctx1) {$ctx1.fill(self,"shallowCopy",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "shouldNotImplement",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._error_(_st("This method should not be implemented in ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"shouldNotImplement",{},smalltalk.Object)})},
messageSends: ["error:", ",", "name", "class"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "size",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._error_("Object not indexable");
return self}, function($ctx1) {$ctx1.fill(self,"size",{},smalltalk.Object)})},
messageSends: ["error:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "subclassResponsibility",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._error_("This method is a responsibility of a subclass");
return self}, function($ctx1) {$ctx1.fill(self,"subclassResponsibility",{},smalltalk.Object)})},
messageSends: ["error:"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "test",
fn: function (){
var self=this;
var a;
return smalltalk.withContext(function($ctx1) { 
a=(1);
_st(self)._halt();
return self}, function($ctx1) {$ctx1.fill(self,"test",{a:a},smalltalk.Object)})},
messageSends: ["halt"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "throw:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 throw anObject ;
return self}, function($ctx1) {$ctx1.fill(self,"throw:",{anObject:anObject},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "try:catch:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
try{return aBlock()} catch(e) {return anotherBlock(e)};
return self}, function($ctx1) {$ctx1.fill(self,"try:catch:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "value",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.valueOf();
return self}, function($ctx1) {$ctx1.fill(self,"value",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "yourself",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"yourself",{},smalltalk.Object)})},
messageSends: []}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "~=",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self).__eq(anObject)).__eq(false);
return $1;
}, function($ctx1) {$ctx1.fill(self,"~=",{anObject:anObject},smalltalk.Object)})},
messageSends: ["="]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "~~",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self).__eq_eq(anObject)).__eq(false);
return $1;
}, function($ctx1) {$ctx1.fill(self,"~~",{anObject:anObject},smalltalk.Object)})},
messageSends: ["=", "=="]}),
smalltalk.Object);


smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.Object.klass)})},
messageSends: []}),
smalltalk.Object.klass);


smalltalk.addClass('Boolean', smalltalk.Object, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "&",
fn: function (aBoolean){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(self == true) {
		return aBoolean;
		} else {
		return false;
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"&",{aBoolean:aBoolean},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (aBoolean){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(! aBoolean._isBoolean || ! aBoolean._isBoolean()) {
			return false;
		}
		return Boolean(self == true) == aBoolean
	;
return self}, function($ctx1) {$ctx1.fill(self,"=",{aBoolean:aBoolean},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "==",
fn: function (aBoolean){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self).__eq(aBoolean);
return $1;
}, function($ctx1) {$ctx1.fill(self,"==",{aBoolean:aBoolean},smalltalk.Boolean)})},
messageSends: ["="]}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "and:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self).__eq(true);
$1=_st($2)._ifTrue_ifFalse_(aBlock,(function(){
return smalltalk.withContext(function($ctx2) {
return false;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"and:",{aBlock:aBlock},smalltalk.Boolean)})},
messageSends: ["ifTrue:ifFalse:", "="]}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSON",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSON",{},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "asString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 return self.toString() ;
return self}, function($ctx1) {$ctx1.fill(self,"asString",{},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "deepCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"deepCopy",{},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "ifFalse:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self;
$1=_st($2)._ifTrue_ifFalse_((function(){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}),aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifFalse:",{aBlock:aBlock},smalltalk.Boolean)})},
messageSends: ["ifTrue:ifFalse:"]}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "ifFalse:ifTrue:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self;
$1=_st($2)._ifTrue_ifFalse_(anotherBlock,aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifFalse:ifTrue:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.Boolean)})},
messageSends: ["ifTrue:ifFalse:"]}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "ifTrue:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self;
$1=_st($2)._ifTrue_ifFalse_(aBlock,(function(){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifTrue:",{aBlock:aBlock},smalltalk.Boolean)})},
messageSends: ["ifTrue:ifFalse:"]}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "ifTrue:ifFalse:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(self == true) {
		return aBlock();
		} else {
		return anotherBlock();
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"ifTrue:ifFalse:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "isBoolean",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isBoolean",{},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "isImmutable",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isImmutable",{},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "not",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self).__eq(false);
return $1;
}, function($ctx1) {$ctx1.fill(self,"not",{},smalltalk.Boolean)})},
messageSends: ["="]}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "or:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self).__eq(true);
$1=_st($2)._ifTrue_ifFalse_((function(){
return smalltalk.withContext(function($ctx2) {
return true;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}),aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"or:",{aBlock:aBlock},smalltalk.Boolean)})},
messageSends: ["ifTrue:ifFalse:", "="]}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aStream)._nextPutAll_(_st(self)._asString());
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Boolean)})},
messageSends: ["nextPutAll:", "asString"]}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "shallowCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"shallowCopy",{},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);

smalltalk.addMethod(
smalltalk.method({
selector: "|",
fn: function (aBoolean){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(self == true) {
		return true;
		} else {
		return aBoolean;
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"|",{aBoolean:aBoolean},smalltalk.Boolean)})},
messageSends: []}),
smalltalk.Boolean);



smalltalk.addClass('Date', smalltalk.Object, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "+",
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self + aDate;
return self}, function($ctx1) {$ctx1.fill(self,"+",{aDate:aDate},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "-",
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self - aDate;
return self}, function($ctx1) {$ctx1.fill(self,"-",{aDate:aDate},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "<",
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self < aDate;
return self}, function($ctx1) {$ctx1.fill(self,"<",{aDate:aDate},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "<=",
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self <= aDate;
return self}, function($ctx1) {$ctx1.fill(self,"<=",{aDate:aDate},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: ">",
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self > aDate;
return self}, function($ctx1) {$ctx1.fill(self,">",{aDate:aDate},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: ">=",
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self >= aDate;
return self}, function($ctx1) {$ctx1.fill(self,">=",{aDate:aDate},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "asDateString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.toDateString();
return self}, function($ctx1) {$ctx1.fill(self,"asDateString",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "asLocaleString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.toLocaleString();
return self}, function($ctx1) {$ctx1.fill(self,"asLocaleString",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "asMilliseconds",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._time();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asMilliseconds",{},smalltalk.Date)})},
messageSends: ["time"]}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "asNumber",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._asMilliseconds();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asNumber",{},smalltalk.Date)})},
messageSends: ["asMilliseconds"]}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "asString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.toString();
return self}, function($ctx1) {$ctx1.fill(self,"asString",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "asTimeString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.toTimeString();
return self}, function($ctx1) {$ctx1.fill(self,"asTimeString",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "day",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._dayOfWeek();
return $1;
}, function($ctx1) {$ctx1.fill(self,"day",{},smalltalk.Date)})},
messageSends: ["dayOfWeek"]}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "day:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._dayOfWeek_(aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"day:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: ["dayOfWeek:"]}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "dayOfMonth",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getDate();
return self}, function($ctx1) {$ctx1.fill(self,"dayOfMonth",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "dayOfMonth:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.setDate(aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"dayOfMonth:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "dayOfWeek",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getDay() + 1;
return self}, function($ctx1) {$ctx1.fill(self,"dayOfWeek",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "dayOfWeek:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.setDay(aNumber - 1);
return self}, function($ctx1) {$ctx1.fill(self,"dayOfWeek:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "hours",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getHours();
return self}, function($ctx1) {$ctx1.fill(self,"hours",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "hours:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.setHours(aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"hours:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "milliseconds",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getMilliseconds();
return self}, function($ctx1) {$ctx1.fill(self,"milliseconds",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "milliseconds:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.setMilliseconds(aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"milliseconds:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "minutes",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getMinutes();
return self}, function($ctx1) {$ctx1.fill(self,"minutes",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "minutes:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.setMinutes(aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"minutes:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "month",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getMonth() + 1;
return self}, function($ctx1) {$ctx1.fill(self,"month",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "month:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.setMonth(aNumber - 1);
return self}, function($ctx1) {$ctx1.fill(self,"month:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aStream)._nextPutAll_(_st(self)._asString());
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Date)})},
messageSends: ["nextPutAll:", "asString"]}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "seconds",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getSeconds();
return self}, function($ctx1) {$ctx1.fill(self,"seconds",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "seconds:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.setSeconds(aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"seconds:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "time",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getTime();
return self}, function($ctx1) {$ctx1.fill(self,"time",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "time:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.setTime(aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"time:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "year",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.getFullYear();
return self}, function($ctx1) {$ctx1.fill(self,"year",{},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);

smalltalk.addMethod(
smalltalk.method({
selector: "year:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.setFullYear(aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"year:",{aNumber:aNumber},smalltalk.Date)})},
messageSends: []}),
smalltalk.Date);


smalltalk.addMethod(
smalltalk.method({
selector: "fromMilliseconds:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._new_(aNumber);
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromMilliseconds:",{aNumber:aNumber},smalltalk.Date.klass)})},
messageSends: ["new:"]}),
smalltalk.Date.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromSeconds:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._fromMilliseconds_(_st(aNumber).__star((1000)));
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromSeconds:",{aNumber:aNumber},smalltalk.Date.klass)})},
messageSends: ["fromMilliseconds:", "*"]}),
smalltalk.Date.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromString:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._new_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromString:",{aString:aString},smalltalk.Date.klass)})},
messageSends: ["new:"]}),
smalltalk.Date.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "millisecondsToRun:",
fn: function (aBlock){
var self=this;
var t;
function $Date(){return smalltalk.Date||(typeof Date=="undefined"?nil:Date)}
return smalltalk.withContext(function($ctx1) { 
var $1;
t=_st($Date())._now();
_st(aBlock)._value();
$1=_st(_st($Date())._now()).__minus(t);
return $1;
}, function($ctx1) {$ctx1.fill(self,"millisecondsToRun:",{aBlock:aBlock,t:t},smalltalk.Date.klass)})},
messageSends: ["now", "value", "-"]}),
smalltalk.Date.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "new:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new Date(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"new:",{anObject:anObject},smalltalk.Date.klass)})},
messageSends: []}),
smalltalk.Date.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "now",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._today();
return $1;
}, function($ctx1) {$ctx1.fill(self,"now",{},smalltalk.Date.klass)})},
messageSends: ["today"]}),
smalltalk.Date.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "today",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._new();
return $1;
}, function($ctx1) {$ctx1.fill(self,"today",{},smalltalk.Date.klass)})},
messageSends: ["new"]}),
smalltalk.Date.klass);


smalltalk.addClass('Environment', smalltalk.Object, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "addInstVarNamed:to:",
fn: function (aString,aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(_st(aClass)._instanceVariableNames())._copy();
_st($1)._add_(aString);
$2=_st($1)._yourself();
_st(_st(self)._classBuilder())._addSubclassOf_named_instanceVariableNames_package_(_st(aClass)._superclass(),_st(aClass)._name(),$2,_st(_st(aClass)._package())._name());
return self}, function($ctx1) {$ctx1.fill(self,"addInstVarNamed:to:",{aString:aString,aClass:aClass},smalltalk.HLEnvironment)})},
messageSends: ["addSubclassOf:named:instanceVariableNames:package:", "superclass", "name", "add:", "copy", "instanceVariableNames", "yourself", "package", "classBuilder"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "allSelectors",
fn: function (){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st($Smalltalk())._current())._at_("allSelectors"))._value();
return $1;
}, function($ctx1) {$ctx1.fill(self,"allSelectors",{},smalltalk.Environment)})},
messageSends: ["value", "at:", "current"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "availableClassNames",
fn: function (){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st($Smalltalk())._current())._classes())._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._name();
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"availableClassNames",{},smalltalk.HLEnvironment)})},
messageSends: ["collect:", "name", "classes", "current"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "availablePackageNames",
fn: function (){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st($Smalltalk())._current())._packages())._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._name();
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"availablePackageNames",{},smalltalk.HLEnvironment)})},
messageSends: ["collect:", "name", "packages", "current"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "availableProtocolsFor:",
fn: function (aClass){
var self=this;
var protocols;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
protocols=_st(aClass)._protocols();
$1=_st(aClass)._superclass();
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(protocols)._addAll_(_st(self)._availableProtocolsFor_(_st(aClass)._superclass()));
};
$2=_st(_st(protocols)._asSet())._asArray();
return $2;
}, function($ctx1) {$ctx1.fill(self,"availableProtocolsFor:",{aClass:aClass,protocols:protocols},smalltalk.HLEnvironment)})},
messageSends: ["protocols", "ifNotNil:", "addAll:", "availableProtocolsFor:", "superclass", "asArray", "asSet"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "classBuilder",
fn: function (){
var self=this;
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($ClassBuilder())._new();
return $1;
}, function($ctx1) {$ctx1.fill(self,"classBuilder",{},smalltalk.HLEnvironment)})},
messageSends: ["new"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "classNamed:",
fn: function (aString){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(_st($Smalltalk())._current())._at_(_st(aString)._asSymbol());
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._error_("Invalid class name");
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"classNamed:",{aString:aString},smalltalk.HLEnvironment)})},
messageSends: ["ifNil:", "error:", "at:", "asSymbol", "current"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "classes",
fn: function (){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($Smalltalk())._current())._classes();
return $1;
}, function($ctx1) {$ctx1.fill(self,"classes",{},smalltalk.Environment)})},
messageSends: ["classes", "current"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "commitPackage:",
fn: function (aPackage){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aPackage)._commit();
return self}, function($ctx1) {$ctx1.fill(self,"commitPackage:",{aPackage:aPackage},smalltalk.HLEnvironment)})},
messageSends: ["commit"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "compileClassComment:for:",
fn: function (aString,aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aClass)._comment_(aString);
return self}, function($ctx1) {$ctx1.fill(self,"compileClassComment:for:",{aString:aString,aClass:aClass},smalltalk.HLEnvironment)})},
messageSends: ["comment:"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "compileClassDefinition:",
fn: function (aString){
var self=this;
function $DoIt(){return smalltalk.DoIt||(typeof DoIt=="undefined"?nil:DoIt)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._eval_on_(aString,_st($DoIt())._new());
return self}, function($ctx1) {$ctx1.fill(self,"compileClassDefinition:",{aString:aString},smalltalk.HLEnvironment)})},
messageSends: ["eval:on:", "new"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "compileMethod:for:protocol:",
fn: function (sourceCode,class_,protocol){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(class_)._compile_category_(sourceCode,protocol);
return $1;
}, function($ctx1) {$ctx1.fill(self,"compileMethod:for:protocol:",{sourceCode:sourceCode,class_:class_,protocol:protocol},smalltalk.HLEnvironment)})},
messageSends: ["compile:category:"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "copyClass:to:",
fn: function (aClass,aClassName){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($Smalltalk())._current())._at_(aClassName);
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(self)._error_(_st(_st("A class named ").__comma(aClassName)).__comma(" already exists"));
};
_st(_st($ClassBuilder())._new())._copyClass_named_(aClass,aClassName);
return self}, function($ctx1) {$ctx1.fill(self,"copyClass:to:",{aClass:aClass,aClassName:aClassName},smalltalk.HLEnvironment)})},
messageSends: ["ifNotNil:", "error:", ",", "at:", "current", "copyClass:named:", "new"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "eval:on:",
fn: function (aString,aReceiver){
var self=this;
var compiler;
function $Compiler(){return smalltalk.Compiler||(typeof Compiler=="undefined"?nil:Compiler)}
function $Error(){return smalltalk.Error||(typeof Error=="undefined"?nil:Error)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
var $early={};
try {
compiler=_st($Compiler())._new();
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(compiler)._parseExpression_(aString);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._on_do_($Error(),(function(ex){
return smalltalk.withContext(function($ctx2) {
$1=_st(window)._alert_(_st(ex)._messageText());
throw $early=[$1];
}, function($ctx2) {$ctx2.fillBlock({ex:ex},$ctx1)})}));
$2=_st(compiler)._evaluateExpression_on_(aString,aReceiver);
return $2;
}
catch(e) {if(e===$early)return e[0]; throw e}
}, function($ctx1) {$ctx1.fill(self,"eval:on:",{aString:aString,aReceiver:aReceiver,compiler:compiler},smalltalk.HLEnvironment)})},
messageSends: ["new", "on:do:", "alert:", "messageText", "parseExpression:", "evaluateExpression:on:"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "moveClass:toPackage:",
fn: function (aClass,aPackageName){
var self=this;
var package_;
function $Package(){return smalltalk.Package||(typeof Package=="undefined"?nil:Package)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
package_=_st($Package())._named_(aPackageName);
$1=package_;
if(($receiver = $1) == nil || $receiver == undefined){
_st(self)._error_("Invalid package name");
} else {
$1;
};
$2=_st(package_).__eq_eq(_st(aClass)._package());
if(smalltalk.assert($2)){
$3=self;
return $3;
};
_st(aClass)._package_(package_);
return self}, function($ctx1) {$ctx1.fill(self,"moveClass:toPackage:",{aClass:aClass,aPackageName:aPackageName,package_:package_},smalltalk.HLEnvironment)})},
messageSends: ["named:", "ifNil:", "error:", "ifTrue:", "==", "package", "package:"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "moveMethod:toClass:",
fn: function (aMethod,aClassName){
var self=this;
var destinationClass;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
destinationClass=_st(_st($Smalltalk())._current())._at_(_st(aClassName)._asSymbol());
$1=destinationClass;
if(($receiver = $1) == nil || $receiver == undefined){
_st(self)._error_("Invalid class name");
} else {
$1;
};
$2=_st(destinationClass).__eq_eq(_st(aMethod)._methodClass());
if(smalltalk.assert($2)){
$3=self;
return $3;
};
_st(destinationClass)._compile_category_(_st(aMethod)._source(),_st(aMethod)._protocol());
_st(_st(aMethod)._methodClass())._removeCompiledMethod_(aMethod);
return self}, function($ctx1) {$ctx1.fill(self,"moveMethod:toClass:",{aMethod:aMethod,aClassName:aClassName,destinationClass:destinationClass},smalltalk.Environment)})},
messageSends: ["at:", "asSymbol", "current", "ifNil:", "error:", "ifTrue:", "==", "methodClass", "compile:category:", "source", "protocol", "removeCompiledMethod:"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "moveMethod:toProtocol:",
fn: function (aMethod,aProtocol){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMethod)._category_(aProtocol);
return self}, function($ctx1) {$ctx1.fill(self,"moveMethod:toProtocol:",{aMethod:aMethod,aProtocol:aProtocol},smalltalk.HLEnvironment)})},
messageSends: ["category:"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "packages",
fn: function (){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($Smalltalk())._current())._packages();
return $1;
}, function($ctx1) {$ctx1.fill(self,"packages",{},smalltalk.HLEnvironment)})},
messageSends: ["packages", "current"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "removeClass:",
fn: function (aClass){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
_st(_st($Smalltalk())._current())._removeClass_(aClass);
return self}, function($ctx1) {$ctx1.fill(self,"removeClass:",{aClass:aClass},smalltalk.HLEnvironment)})},
messageSends: ["removeClass:", "current"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "removeMethod:",
fn: function (aMethod){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(aMethod)._methodClass())._removeCompiledMethod_(aMethod);
return self}, function($ctx1) {$ctx1.fill(self,"removeMethod:",{aMethod:aMethod},smalltalk.Environment)})},
messageSends: ["removeCompiledMethod:", "methodClass"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "removeProtocol:from:",
fn: function (aString,aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st(aClass)._methods())._select_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st(each)._protocol()).__eq(aString);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})})))._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(aClass)._removeCompiledMethod_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"removeProtocol:from:",{aString:aString,aClass:aClass},smalltalk.Environment)})},
messageSends: ["do:", "removeCompiledMethod:", "select:", "=", "protocol", "methods"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "renameClass:to:",
fn: function (aClass,aClassName){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($Smalltalk())._current())._at_(aClassName);
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(self)._error_(_st(_st("A class named ").__comma(aClassName)).__comma(" already exists"));
};
_st(_st($ClassBuilder())._new())._renameClass_to_(aClass,aClassName);
return self}, function($ctx1) {$ctx1.fill(self,"renameClass:to:",{aClass:aClass,aClassName:aClassName},smalltalk.HLEnvironment)})},
messageSends: ["ifNotNil:", "error:", ",", "at:", "current", "renameClass:to:", "new"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "renameProtocol:to:in:",
fn: function (aString,anotherString,aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st(aClass)._methods())._select_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st(each)._protocol()).__eq(aString);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})})))._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._protocol_(anotherString);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renameProtocol:to:in:",{aString:aString,anotherString:anotherString,aClass:aClass},smalltalk.Environment)})},
messageSends: ["do:", "protocol:", "select:", "=", "protocol", "methods"]}),
smalltalk.Environment);

smalltalk.addMethod(
smalltalk.method({
selector: "systemAnnouncer",
fn: function (){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st($Smalltalk())._current())._at_("SystemAnnouncer"))._current();
return $1;
}, function($ctx1) {$ctx1.fill(self,"systemAnnouncer",{},smalltalk.Environment)})},
messageSends: ["current", "at:"]}),
smalltalk.Environment);



smalltalk.addClass('JSObjectProxy', smalltalk.Object, ['jsObject'], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "addObjectVariablesTo:",
fn: function (aDictionary){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		for(var i in self['@jsObject']) {
			aDictionary._at_put_(i, self['@jsObject'][i]);
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"addObjectVariablesTo:",{aDictionary:aDictionary},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "at:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self['@jsObject'][aString];
return self}, function($ctx1) {$ctx1.fill(self,"at:",{aString:aString},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (aString,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var obj = self['@jsObject'];
		return aString in obj ? obj[aString] : aBlock();
	;
return self}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{aString:aString,aBlock:aBlock},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifPresent:",
fn: function (aString,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var obj = self['@jsObject'];
		return aString in obj ? aBlock(obj[aString]) : nil;
	;
return self}, function($ctx1) {$ctx1.fill(self,"at:ifPresent:",{aString:aString,aBlock:aBlock},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifPresent:ifAbsent:",
fn: function (aString,aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var obj = self['@jsObject'];
		return aString in obj ? aBlock(obj[aString]) : anotherBlock();
	;
return self}, function($ctx1) {$ctx1.fill(self,"at:ifPresent:ifAbsent:",{aString:aString,aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (aString,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self['@jsObject'][aString] = anObject;
return self}, function($ctx1) {$ctx1.fill(self,"at:put:",{aString:aString,anObject:anObject},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "doesNotUnderstand:",
fn: function (aMessage){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._lookupProperty_(_st(_st(aMessage)._selector())._asJavaScriptSelector());
if(($receiver = $2) == nil || $receiver == undefined){
$1=smalltalk.Object.fn.prototype._doesNotUnderstand_.apply(_st(self), [aMessage]);
} else {
var jsSelector;
jsSelector=$receiver;
$1=_st(self)._forwardMessage_withArguments_(jsSelector,_st(aMessage)._arguments());
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"doesNotUnderstand:",{aMessage:aMessage},smalltalk.JSObjectProxy)})},
messageSends: ["ifNil:ifNotNil:", "doesNotUnderstand:", "forwardMessage:withArguments:", "arguments", "lookupProperty:", "asJavaScriptSelector", "selector"]}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "forwardMessage:withArguments:",
fn: function (aString,anArray){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		return smalltalk.send(self._jsObject(), aString, anArray);
	;
return self}, function($ctx1) {$ctx1.fill(self,"forwardMessage:withArguments:",{aString:aString,anArray:anArray},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "inspectOn:",
fn: function (anInspector){
var self=this;
var variables;
function $Dictionary(){return smalltalk.Dictionary||(typeof Dictionary=="undefined"?nil:Dictionary)}
return smalltalk.withContext(function($ctx1) { 
variables=_st($Dictionary())._new();
_st(variables)._at_put_("#self",_st(self)._jsObject());
_st(anInspector)._setLabel_(_st(self)._printString());
_st(self)._addObjectVariablesTo_(variables);
_st(anInspector)._setVariables_(variables);
return self}, function($ctx1) {$ctx1.fill(self,"inspectOn:",{anInspector:anInspector,variables:variables},smalltalk.JSObjectProxy)})},
messageSends: ["new", "at:put:", "jsObject", "setLabel:", "printString", "addObjectVariablesTo:", "setVariables:"]}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "jsObject",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@jsObject"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"jsObject",{},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "jsObject:",
fn: function (aJSObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@jsObject"]=aJSObject;
return self}, function($ctx1) {$ctx1.fill(self,"jsObject:",{aJSObject:aJSObject},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "keysAndValuesDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var o = self['@jsObject'];
		for(var i in o) {
			aBlock(i, o[i]);
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"keysAndValuesDo:",{aBlock:aBlock},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "lookupProperty:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return aString in self._jsObject() ? aString : nil;
return self}, function($ctx1) {$ctx1.fill(self,"lookupProperty:",{aString:aString},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aStream)._nextPutAll_(_st(_st(self)._jsObject())._toString());
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.JSObjectProxy)})},
messageSends: ["nextPutAll:", "toString", "jsObject"]}),
smalltalk.JSObjectProxy);

smalltalk.addMethod(
smalltalk.method({
selector: "value",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_ifAbsent_("value",(function(){
return smalltalk.withContext(function($ctx2) {
return smalltalk.Object.fn.prototype._value.apply(_st(self), []);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"value",{},smalltalk.JSObjectProxy)})},
messageSends: ["at:ifAbsent:", "value"]}),
smalltalk.JSObjectProxy);


smalltalk.addMethod(
smalltalk.method({
selector: "on:",
fn: function (aJSObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._jsObject_(aJSObject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:",{aJSObject:aJSObject},smalltalk.JSObjectProxy.klass)})},
messageSends: ["jsObject:", "new", "yourself"]}),
smalltalk.JSObjectProxy.klass);


smalltalk.addClass('Number', smalltalk.Object, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "&",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self & aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"&",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "*",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self * aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"*",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "+",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self + aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"+",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "-",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self - aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"-",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "/",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self / aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"/",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "<",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self < aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"<",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "<=",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self <= aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"<=",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(! aNumber._isNumber || ! aNumber._isNumber()) {
			return false;
		}
		return Number(self) == aNumber
	;
return self}, function($ctx1) {$ctx1.fill(self,"=",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: ">",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self > aNumber;
return self}, function($ctx1) {$ctx1.fill(self,">",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: ">=",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self >= aNumber;
return self}, function($ctx1) {$ctx1.fill(self,">=",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "@",
fn: function (aNumber){
var self=this;
function $Point(){return smalltalk.Point||(typeof Point=="undefined"?nil:Point)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Point())._x_y_(self,aNumber);
return $1;
}, function($ctx1) {$ctx1.fill(self,"@",{aNumber:aNumber},smalltalk.Number)})},
messageSends: ["x:y:"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "IsImmutable",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"IsImmutable",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "\x5c\x5c",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self % aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"\x5c\x5c",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "abs",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Math.abs(self);;
return self}, function($ctx1) {$ctx1.fill(self,"abs",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSON",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSON",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "asJavascript",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("(").__comma(_st(self)._printString())).__comma(")");
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJavascript",{},smalltalk.Number)})},
messageSends: [",", "printString"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "asPoint",
fn: function (){
var self=this;
function $Point(){return smalltalk.Point||(typeof Point=="undefined"?nil:Point)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Point())._x_y_(self,self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"asPoint",{},smalltalk.Number)})},
messageSends: ["x:y:"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "asString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 return String(self) ;
return self}, function($ctx1) {$ctx1.fill(self,"asString",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "atRandom",
fn: function (){
var self=this;
function $Random(){return smalltalk.Random||(typeof Random=="undefined"?nil:Random)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(_st(_st($Random())._new())._next()).__star(self))._truncated()).__plus((1));
return $1;
}, function($ctx1) {$ctx1.fill(self,"atRandom",{},smalltalk.Number)})},
messageSends: ["+", "truncated", "*", "next", "new"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "copy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"copy",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "deepCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._copy();
return $1;
}, function($ctx1) {$ctx1.fill(self,"deepCopy",{},smalltalk.Number)})},
messageSends: ["copy"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "even",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((0)).__eq(_st(self).__backslash_backslash((2)));
return $1;
}, function($ctx1) {$ctx1.fill(self,"even",{},smalltalk.Number)})},
messageSends: ["=", "\x5c\x5c"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "identityHash",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._asString()).__comma("n");
return $1;
}, function($ctx1) {$ctx1.fill(self,"identityHash",{},smalltalk.Number)})},
messageSends: [",", "asString"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "isNumber",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isNumber",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "isZero",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self).__eq((0));
return $1;
}, function($ctx1) {$ctx1.fill(self,"isZero",{},smalltalk.Number)})},
messageSends: ["="]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "max:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Math.max(self, aNumber);;
return self}, function($ctx1) {$ctx1.fill(self,"max:",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "min:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Math.min(self, aNumber);;
return self}, function($ctx1) {$ctx1.fill(self,"min:",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "negated",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((0)).__minus(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"negated",{},smalltalk.Number)})},
messageSends: ["-"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "negative",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self).__lt((0));
return $1;
}, function($ctx1) {$ctx1.fill(self,"negative",{},smalltalk.Number)})},
messageSends: ["<"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "odd",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._even())._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"odd",{},smalltalk.Number)})},
messageSends: ["not", "even"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "positive",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self).__gt_eq((0));
return $1;
}, function($ctx1) {$ctx1.fill(self,"positive",{},smalltalk.Number)})},
messageSends: [">="]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aStream)._nextPutAll_(_st(self)._asString());
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Number)})},
messageSends: ["nextPutAll:", "asString"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "printShowingDecimalPlaces:",
fn: function (placesDesired){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.toFixed(placesDesired);
return self}, function($ctx1) {$ctx1.fill(self,"printShowingDecimalPlaces:",{placesDesired:placesDesired},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "rounded",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Math.round(self);;
return self}, function($ctx1) {$ctx1.fill(self,"rounded",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "sqrt",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Math.sqrt(self);
return self}, function($ctx1) {$ctx1.fill(self,"sqrt",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "squared",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self).__star(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"squared",{},smalltalk.Number)})},
messageSends: ["*"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "timesRepeat:",
fn: function (aBlock){
var self=this;
var count;
return smalltalk.withContext(function($ctx1) { 
count=(1);
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(count).__gt(self);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileFalse_((function(){
return smalltalk.withContext(function($ctx2) {
_st(aBlock)._value();
count=_st(count).__plus((1));
return count;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"timesRepeat:",{aBlock:aBlock,count:count},smalltalk.Number)})},
messageSends: ["whileFalse:", "value", "+", ">"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "to:",
fn: function (aNumber){
var self=this;
var array,first,last,count;
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
var $1;
first=_st(self)._truncated();
last=_st(_st(aNumber)._truncated()).__plus((1));
count=(1);
array=_st($Array())._new();
_st(_st(last).__minus(first))._timesRepeat_((function(){
return smalltalk.withContext(function($ctx2) {
_st(array)._at_put_(count,first);
count=_st(count).__plus((1));
count;
first=_st(first).__plus((1));
return first;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$1=array;
return $1;
}, function($ctx1) {$ctx1.fill(self,"to:",{aNumber:aNumber,array:array,first:first,last:last,count:count},smalltalk.Number)})},
messageSends: ["truncated", "+", "new", "timesRepeat:", "at:put:", "-"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "to:by:",
fn: function (stop,step){
var self=this;
var array,value,pos;
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
value=self;
array=_st($Array())._new();
pos=(1);
$1=_st(step).__eq((0));
if(smalltalk.assert($1)){
_st(self)._error_("step must be non-zero");
};
$2=_st(step).__lt((0));
if(smalltalk.assert($2)){
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(value).__gt_eq(stop);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileTrue_((function(){
return smalltalk.withContext(function($ctx2) {
_st(array)._at_put_(pos,value);
pos=_st(pos).__plus((1));
pos;
value=_st(value).__plus(step);
return value;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
} else {
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(value).__lt_eq(stop);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileTrue_((function(){
return smalltalk.withContext(function($ctx2) {
_st(array)._at_put_(pos,value);
pos=_st(pos).__plus((1));
pos;
value=_st(value).__plus(step);
return value;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
};
$3=array;
return $3;
}, function($ctx1) {$ctx1.fill(self,"to:by:",{stop:stop,step:step,array:array,value:value,pos:pos},smalltalk.Number)})},
messageSends: ["new", "ifTrue:", "error:", "=", "ifTrue:ifFalse:", "whileTrue:", "at:put:", "+", ">=", "<=", "<"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "to:by:do:",
fn: function (stop,step,aBlock){
var self=this;
var value;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
value=self;
$1=_st(step).__eq((0));
if(smalltalk.assert($1)){
_st(self)._error_("step must be non-zero");
};
$2=_st(step).__lt((0));
if(smalltalk.assert($2)){
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(value).__gt_eq(stop);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileTrue_((function(){
return smalltalk.withContext(function($ctx2) {
_st(aBlock)._value_(value);
value=_st(value).__plus(step);
return value;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
} else {
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(value).__lt_eq(stop);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileTrue_((function(){
return smalltalk.withContext(function($ctx2) {
_st(aBlock)._value_(value);
value=_st(value).__plus(step);
return value;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
};
return self}, function($ctx1) {$ctx1.fill(self,"to:by:do:",{stop:stop,step:step,aBlock:aBlock,value:value},smalltalk.Number)})},
messageSends: ["ifTrue:", "error:", "=", "ifTrue:ifFalse:", "whileTrue:", "value:", "+", ">=", "<=", "<"]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "to:do:",
fn: function (stop,aBlock){
var self=this;
var nextValue;
return smalltalk.withContext(function($ctx1) { 
nextValue=self;
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(nextValue).__lt_eq(stop);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileTrue_((function(){
return smalltalk.withContext(function($ctx2) {
_st(aBlock)._value_(nextValue);
nextValue=_st(nextValue).__plus((1));
return nextValue;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"to:do:",{stop:stop,aBlock:aBlock,nextValue:nextValue},smalltalk.Number)})},
messageSends: ["whileTrue:", "value:", "+", "<="]}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "truncated",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(self >= 0) {
			return Math.floor(self);
		} else {
			return Math.floor(self * (-1)) * (-1);
		};
	;
return self}, function($ctx1) {$ctx1.fill(self,"truncated",{},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);

smalltalk.addMethod(
smalltalk.method({
selector: "|",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self | aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"|",{aNumber:aNumber},smalltalk.Number)})},
messageSends: []}),
smalltalk.Number);


smalltalk.addMethod(
smalltalk.method({
selector: "pi",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Math.PI;
return self}, function($ctx1) {$ctx1.fill(self,"pi",{},smalltalk.Number.klass)})},
messageSends: []}),
smalltalk.Number.klass);


smalltalk.addClass('Organizer', smalltalk.Object, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "addElement:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.elements.addElement(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"addElement:",{anObject:anObject},smalltalk.Organizer)})},
messageSends: []}),
smalltalk.Organizer);

smalltalk.addMethod(
smalltalk.method({
selector: "elements",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._basicAt_("elements"))._copy();
return $1;
}, function($ctx1) {$ctx1.fill(self,"elements",{},smalltalk.Organizer)})},
messageSends: ["copy", "basicAt:"]}),
smalltalk.Organizer);

smalltalk.addMethod(
smalltalk.method({
selector: "removeElement:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.elements.removeElement(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"removeElement:",{anObject:anObject},smalltalk.Organizer)})},
messageSends: []}),
smalltalk.Organizer);



smalltalk.addClass('ClassOrganizer', smalltalk.Organizer, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "addElement:",
fn: function (aString){
var self=this;
function $ProtocolAdded(){return smalltalk.ProtocolAdded||(typeof ProtocolAdded=="undefined"?nil:ProtocolAdded)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
smalltalk.Organizer.fn.prototype._addElement_.apply(_st(self), [aString]);
$1=_st($ProtocolAdded())._new();
_st($1)._protocol_(aString);
_st($1)._theClass_(_st(self)._theClass());
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
return self}, function($ctx1) {$ctx1.fill(self,"addElement:",{aString:aString},smalltalk.ClassOrganizer)})},
messageSends: ["addElement:", "announce:", "protocol:", "new", "theClass:", "theClass", "yourself", "current"]}),
smalltalk.ClassOrganizer);

smalltalk.addMethod(
smalltalk.method({
selector: "removeElement:",
fn: function (aString){
var self=this;
function $ProtocolRemoved(){return smalltalk.ProtocolRemoved||(typeof ProtocolRemoved=="undefined"?nil:ProtocolRemoved)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
smalltalk.Organizer.fn.prototype._removeElement_.apply(_st(self), [aString]);
$1=_st($ProtocolRemoved())._new();
_st($1)._protocol_(aString);
_st($1)._theClass_(_st(self)._theClass());
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
return self}, function($ctx1) {$ctx1.fill(self,"removeElement:",{aString:aString},smalltalk.ClassOrganizer)})},
messageSends: ["removeElement:", "announce:", "protocol:", "new", "theClass:", "theClass", "yourself", "current"]}),
smalltalk.ClassOrganizer);

smalltalk.addMethod(
smalltalk.method({
selector: "theClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 return self.theClass ;
return self}, function($ctx1) {$ctx1.fill(self,"theClass",{},smalltalk.ClassOrganizer)})},
messageSends: []}),
smalltalk.ClassOrganizer);



smalltalk.addClass('PackageOrganizer', smalltalk.Organizer, [], 'Kernel-Objects');


smalltalk.addClass('Package', smalltalk.Object, ['commitPathJs', 'commitPathSt'], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "classes",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._organization())._elements();
return $1;
}, function($ctx1) {$ctx1.fill(self,"classes",{},smalltalk.Package)})},
messageSends: ["elements", "organization"]}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "commitPathJs",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@commitPathJs"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(_st(self)._class())._defaultCommitPathJs();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"commitPathJs",{},smalltalk.Package)})},
messageSends: ["ifNil:", "defaultCommitPathJs", "class"]}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "commitPathJs:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@commitPathJs"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"commitPathJs:",{aString:aString},smalltalk.Package)})},
messageSends: []}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "commitPathSt",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@commitPathSt"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(_st(self)._class())._defaultCommitPathSt();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"commitPathSt",{},smalltalk.Package)})},
messageSends: ["ifNil:", "defaultCommitPathSt", "class"]}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "commitPathSt:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@commitPathSt"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"commitPathSt:",{aString:aString},smalltalk.Package)})},
messageSends: []}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "isPackage",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isPackage",{},smalltalk.Package)})},
messageSends: []}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "name",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.pkgName;
return self}, function($ctx1) {$ctx1.fill(self,"name",{},smalltalk.Package)})},
messageSends: []}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "name:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.pkgName = aString;
return self}, function($ctx1) {$ctx1.fill(self,"name:",{aString:aString},smalltalk.Package)})},
messageSends: []}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "organization",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_("organization");
return $1;
}, function($ctx1) {$ctx1.fill(self,"organization",{},smalltalk.Package)})},
messageSends: ["basicAt:"]}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
smalltalk.Object.fn.prototype._printOn_.apply(_st(self), [aStream]);
$1=aStream;
_st($1)._nextPutAll_(" (");
_st($1)._nextPutAll_(_st(self)._name());
$2=_st($1)._nextPutAll_(")");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Package)})},
messageSends: ["printOn:", "nextPutAll:", "name"]}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "setupClasses",
fn: function (){
var self=this;
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(self)._classes();
_st($1)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st($ClassBuilder())._new())._setupClass_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$2=_st($1)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._initialize();
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"setupClasses",{},smalltalk.Package)})},
messageSends: ["do:", "setupClass:", "new", "classes", "initialize"]}),
smalltalk.Package);

smalltalk.addMethod(
smalltalk.method({
selector: "sortedClasses",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._sortedClasses_(_st(self)._classes());
return $1;
}, function($ctx1) {$ctx1.fill(self,"sortedClasses",{},smalltalk.Package)})},
messageSends: ["sortedClasses:", "classes", "class"]}),
smalltalk.Package);


smalltalk.Package.klass.iVarNames = ['defaultCommitPathJs','defaultCommitPathSt'];
smalltalk.addMethod(
smalltalk.method({
selector: "commitPathsFromLoader",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var commitPath = typeof amber !== 'undefined' && amber.commitPath;
		if (!commitPath) return;
		if (commitPath.js) self._defaultCommitPathJs_(commitPath.js);
		if (commitPath.st) self._defaultCommitPathSt_(commitPath.st);
	;
return self}, function($ctx1) {$ctx1.fill(self,"commitPathsFromLoader",{},smalltalk.Package.klass)})},
messageSends: []}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "defaultCommitPathJs",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@defaultCommitPathJs"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@defaultCommitPathJs"]="js";
$1=self["@defaultCommitPathJs"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"defaultCommitPathJs",{},smalltalk.Package.klass)})},
messageSends: ["ifNil:"]}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "defaultCommitPathJs:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@defaultCommitPathJs"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"defaultCommitPathJs:",{aString:aString},smalltalk.Package.klass)})},
messageSends: []}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "defaultCommitPathSt",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@defaultCommitPathSt"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@defaultCommitPathSt"]="st";
$1=self["@defaultCommitPathSt"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"defaultCommitPathSt",{},smalltalk.Package.klass)})},
messageSends: ["ifNil:"]}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "defaultCommitPathSt:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@defaultCommitPathSt"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"defaultCommitPathSt:",{aString:aString},smalltalk.Package.klass)})},
messageSends: []}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.klass.fn.prototype._initialize.apply(_st(self), []);
_st(self)._commitPathsFromLoader();
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.Package.klass)})},
messageSends: ["initialize", "commitPathsFromLoader"]}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "load:",
fn: function (aPackageName){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._load_prefix_(aPackageName,_st(_st(self)._defaultCommitPathJs()).__comma("/"));
return self}, function($ctx1) {$ctx1.fill(self,"load:",{aPackageName:aPackageName},smalltalk.Package.klass)})},
messageSends: ["load:prefix:", ",", "defaultCommitPathJs"]}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "load:prefix:",
fn: function (aPackageName,aPrefix){
var self=this;
function $Package(){return smalltalk.Package||(typeof Package=="undefined"?nil:Package)}
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._getScript_onSuccess_(_st(_st(aPrefix).__comma(aPackageName)).__comma(".js"),(function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st($Package())._named_(aPackageName))._setupClasses();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"load:prefix:",{aPackageName:aPackageName,aPrefix:aPrefix},smalltalk.Package.klass)})},
messageSends: ["getScript:onSuccess:", ",", "setupClasses", "named:"]}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "named:",
fn: function (aPackageName){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($Smalltalk())._current())._packageAt_(aPackageName);
return $1;
}, function($ctx1) {$ctx1.fill(self,"named:",{aPackageName:aPackageName},smalltalk.Package.klass)})},
messageSends: ["packageAt:", "current"]}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "named:ifAbsent:",
fn: function (aPackageName,aBlock){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($Smalltalk())._current())._packageAt_ifAbsent_(aPackageName,aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"named:ifAbsent:",{aPackageName:aPackageName,aBlock:aBlock},smalltalk.Package.klass)})},
messageSends: ["packageAt:ifAbsent:", "current"]}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "resetCommitPaths",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@defaultCommitPathJs"]=nil;
self["@defaultCommitPathSt"]=nil;
return self}, function($ctx1) {$ctx1.fill(self,"resetCommitPaths",{},smalltalk.Package.klass)})},
messageSends: []}),
smalltalk.Package.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "sortedClasses:",
fn: function (classes){
var self=this;
var children,others,nodes,expandedClasses;
function $ClassSorterNode(){return smalltalk.ClassSorterNode||(typeof ClassSorterNode=="undefined"?nil:ClassSorterNode)}
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
children=[];
others=[];
_st(classes)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
$1=_st(classes)._includes_(_st(each)._superclass());
if(smalltalk.assert($1)){
return _st(others)._add_(each);
} else {
return _st(children)._add_(each);
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
nodes=_st(children)._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st($ClassSorterNode())._on_classes_level_(each,others,(0));
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
nodes=_st(nodes)._sorted_((function(a,b){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(a)._theClass())._name()).__lt_eq(_st(_st(b)._theClass())._name());
}, function($ctx2) {$ctx2.fillBlock({a:a,b:b},$ctx1)})}));
expandedClasses=_st($Array())._new();
_st(nodes)._do_((function(aNode){
return smalltalk.withContext(function($ctx2) {
return _st(aNode)._traverseClassesWith_(expandedClasses);
}, function($ctx2) {$ctx2.fillBlock({aNode:aNode},$ctx1)})}));
$2=expandedClasses;
return $2;
}, function($ctx1) {$ctx1.fill(self,"sortedClasses:",{classes:classes,children:children,others:others,nodes:nodes,expandedClasses:expandedClasses},smalltalk.Package.klass)})},
messageSends: ["do:", "ifFalse:ifTrue:", "add:", "includes:", "superclass", "collect:", "on:classes:level:", "sorted:", "<=", "name", "theClass", "new", "traverseClassesWith:"]}),
smalltalk.Package.klass);


smalltalk.addClass('Point', smalltalk.Object, ['x', 'y'], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "*",
fn: function (aPoint){
var self=this;
function $Point(){return smalltalk.Point||(typeof Point=="undefined"?nil:Point)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Point())._x_y_(_st(_st(self)._x()).__star(_st(_st(aPoint)._asPoint())._x()),_st(_st(self)._y()).__star(_st(_st(aPoint)._asPoint())._y()));
return $1;
}, function($ctx1) {$ctx1.fill(self,"*",{aPoint:aPoint},smalltalk.Point)})},
messageSends: ["x:y:", "*", "x", "asPoint", "y"]}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "+",
fn: function (aPoint){
var self=this;
function $Point(){return smalltalk.Point||(typeof Point=="undefined"?nil:Point)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Point())._x_y_(_st(_st(self)._x()).__plus(_st(_st(aPoint)._asPoint())._x()),_st(_st(self)._y()).__plus(_st(_st(aPoint)._asPoint())._y()));
return $1;
}, function($ctx1) {$ctx1.fill(self,"+",{aPoint:aPoint},smalltalk.Point)})},
messageSends: ["x:y:", "+", "x", "asPoint", "y"]}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "-",
fn: function (aPoint){
var self=this;
function $Point(){return smalltalk.Point||(typeof Point=="undefined"?nil:Point)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Point())._x_y_(_st(_st(self)._x()).__minus(_st(_st(aPoint)._asPoint())._x()),_st(_st(self)._y()).__minus(_st(_st(aPoint)._asPoint())._y()));
return $1;
}, function($ctx1) {$ctx1.fill(self,"-",{aPoint:aPoint},smalltalk.Point)})},
messageSends: ["x:y:", "-", "x", "asPoint", "y"]}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "/",
fn: function (aPoint){
var self=this;
function $Point(){return smalltalk.Point||(typeof Point=="undefined"?nil:Point)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Point())._x_y_(_st(_st(self)._x()).__slash(_st(_st(aPoint)._asPoint())._x()),_st(_st(self)._y()).__slash(_st(_st(aPoint)._asPoint())._y()));
return $1;
}, function($ctx1) {$ctx1.fill(self,"/",{aPoint:aPoint},smalltalk.Point)})},
messageSends: ["x:y:", "/", "x", "asPoint", "y"]}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (aPoint){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(aPoint)._class()).__eq(_st(self)._class()))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(aPoint)._x()).__eq(_st(self)._x())).__and(_st(_st(aPoint)._y()).__eq(_st(self)._y()));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"=",{aPoint:aPoint},smalltalk.Point)})},
messageSends: ["and:", "&", "=", "y", "x", "class"]}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "asPoint",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asPoint",{},smalltalk.Point)})},
messageSends: []}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self["@x"])._printOn_(aStream);
_st(aStream)._nextPutAll_("@");
$1=_st(_st(self["@y"])._notNil())._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self["@y"])._negative();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(smalltalk.assert($1)){
_st(aStream)._space();
};
_st(self["@y"])._printOn_(aStream);
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Point)})},
messageSends: ["printOn:", "nextPutAll:", "ifTrue:", "space", "and:", "negative", "notNil"]}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "translateBy:",
fn: function (delta){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(delta)._x()).__plus(self["@x"])).__at(_st(_st(delta)._y()).__plus(self["@y"]));
return $1;
}, function($ctx1) {$ctx1.fill(self,"translateBy:",{delta:delta},smalltalk.Point)})},
messageSends: ["@", "+", "y", "x"]}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "x",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@x"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"x",{},smalltalk.Point)})},
messageSends: []}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "x:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@x"]=aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"x:",{aNumber:aNumber},smalltalk.Point)})},
messageSends: []}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "y",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@y"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"y",{},smalltalk.Point)})},
messageSends: []}),
smalltalk.Point);

smalltalk.addMethod(
smalltalk.method({
selector: "y:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@y"]=aNumber;
return self}, function($ctx1) {$ctx1.fill(self,"y:",{aNumber:aNumber},smalltalk.Point)})},
messageSends: []}),
smalltalk.Point);


smalltalk.addMethod(
smalltalk.method({
selector: "x:y:",
fn: function (aNumber,anotherNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._x_(aNumber);
_st($2)._y_(anotherNumber);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"x:y:",{aNumber:aNumber,anotherNumber:anotherNumber},smalltalk.Point.klass)})},
messageSends: ["x:", "new", "y:", "yourself"]}),
smalltalk.Point.klass);


smalltalk.addClass('Random', smalltalk.Object, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "next",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Math.random();
return self}, function($ctx1) {$ctx1.fill(self,"next",{},smalltalk.Random)})},
messageSends: []}),
smalltalk.Random);

smalltalk.addMethod(
smalltalk.method({
selector: "next:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st((1))._to_(anInteger))._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._next();
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"next:",{anInteger:anInteger},smalltalk.Random)})},
messageSends: ["collect:", "next", "to:"]}),
smalltalk.Random);



smalltalk.addClass('Smalltalk', smalltalk.Object, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "asSmalltalkException:",
fn: function (anObject){
var self=this;
function $JavaScriptException(){return smalltalk.JavaScriptException||(typeof JavaScriptException=="undefined"?nil:JavaScriptException)}
function $Error(){return smalltalk.Error||(typeof Error=="undefined"?nil:Error)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(_st(self)._isSmalltalkObject_(anObject))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(anObject)._isKindOf_($Error());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(smalltalk.assert($2)){
$1=anObject;
} else {
$1=_st($JavaScriptException())._on_(anObject);
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"asSmalltalkException:",{anObject:anObject},smalltalk.Smalltalk)})},
messageSends: ["ifTrue:ifFalse:", "on:", "and:", "isKindOf:", "isSmalltalkObject:"]}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "at:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self[aString];
return self}, function($ctx1) {$ctx1.fill(self,"at:",{aString:aString},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "basicParse:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return smalltalk.parser.parse(aString);
return self}, function($ctx1) {$ctx1.fill(self,"basicParse:",{aString:aString},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "classes",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.classes();
return self}, function($ctx1) {$ctx1.fill(self,"classes",{},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "createPackage:",
fn: function (packageName){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return smalltalk.addPackage(packageName);
return self}, function($ctx1) {$ctx1.fill(self,"createPackage:",{packageName:packageName},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "createPackage:properties:",
fn: function (packageName,aDict){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._deprecatedAPI();
$1=_st(aDict)._isEmpty();
if(! smalltalk.assert($1)){
_st(self)._error_("createPackage:properties: called with nonempty properties");
};
$2=_st(self)._createPackage_(packageName);
return $2;
}, function($ctx1) {$ctx1.fill(self,"createPackage:properties:",{packageName:packageName,aDict:aDict},smalltalk.Smalltalk)})},
messageSends: ["deprecatedAPI", "ifFalse:", "error:", "isEmpty", "createPackage:"]}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "deleteClass:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.removeClass(aClass);
return self}, function($ctx1) {$ctx1.fill(self,"deleteClass:",{aClass:aClass},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "deletePackage:",
fn: function (packageName){
var self=this;
return smalltalk.withContext(function($ctx1) { 
delete smalltalk.packages[packageName];
return self}, function($ctx1) {$ctx1.fill(self,"deletePackage:",{packageName:packageName},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "isSmalltalkObject:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return typeof anObject.klass !== 'undefined';
return self}, function($ctx1) {$ctx1.fill(self,"isSmalltalkObject:",{anObject:anObject},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "packageAt:",
fn: function (packageName){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.packages[packageName];
return self}, function($ctx1) {$ctx1.fill(self,"packageAt:",{packageName:packageName},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "packageAt:ifAbsent:",
fn: function (packageName,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._packageAt_(packageName);
$1=_st($2)._ifNil_(aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"packageAt:ifAbsent:",{packageName:packageName,aBlock:aBlock},smalltalk.Smalltalk)})},
messageSends: ["ifNil:", "packageAt:"]}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "packages",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.packages.all();
return self}, function($ctx1) {$ctx1.fill(self,"packages",{},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "parse:",
fn: function (aString){
var self=this;
var result;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self)._try_catch_((function(){
return smalltalk.withContext(function($ctx2) {
result=_st(self)._basicParse_(aString);
return result;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}),(function(ex){
return smalltalk.withContext(function($ctx2) {
return _st(_st(self)._parseError_parsing_(ex,aString))._signal();
}, function($ctx2) {$ctx2.fillBlock({ex:ex},$ctx1)})}));
$1=result;
return $1;
}, function($ctx1) {$ctx1.fill(self,"parse:",{aString:aString,result:result},smalltalk.Smalltalk)})},
messageSends: ["try:catch:", "basicParse:", "signal", "parseError:parsing:"]}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "parseError:parsing:",
fn: function (anException,aString){
var self=this;
function $ParseError(){return smalltalk.ParseError||(typeof ParseError=="undefined"?nil:ParseError)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($ParseError())._new())._messageText_(_st(_st(_st(_st(_st("Parse error on line ").__comma(_st(anException)._basicAt_("line"))).__comma(" column ")).__comma(_st(anException)._basicAt_("column"))).__comma(" : Unexpected character ")).__comma(_st(anException)._basicAt_("found")));
return $1;
}, function($ctx1) {$ctx1.fill(self,"parseError:parsing:",{anException:anException,aString:aString},smalltalk.Smalltalk)})},
messageSends: ["messageText:", ",", "basicAt:", "new"]}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "pseudoVariableNames",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=["self", "super", "nil", "true", "false", "thisContext"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"pseudoVariableNames",{},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "readJSObject:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.readJSObject(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"readJSObject:",{anObject:anObject},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "removeClass:",
fn: function (aClass){
var self=this;
function $ClassRemoved(){return smalltalk.ClassRemoved||(typeof ClassRemoved=="undefined"?nil:ClassRemoved)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
$1=_st(aClass)._isMetaclass();
if(smalltalk.assert($1)){
_st(self)._error_(_st(_st(aClass)._asString()).__comma(" is a Metaclass and cannot be removed!"));
};
_st(self)._deleteClass_(aClass);
$2=_st($ClassRemoved())._new();
_st($2)._theClass_(aClass);
$3=_st($2)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($3);
return self}, function($ctx1) {$ctx1.fill(self,"removeClass:",{aClass:aClass},smalltalk.Smalltalk)})},
messageSends: ["ifTrue:", "error:", ",", "asString", "isMetaclass", "deleteClass:", "announce:", "theClass:", "new", "yourself", "current"]}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "removePackage:",
fn: function (packageName){
var self=this;
var pkg;
return smalltalk.withContext(function($ctx1) { 
pkg=_st(self)._packageAt_ifAbsent_(packageName,(function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._error_(_st("Missing package: ").__comma(packageName));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(_st(pkg)._classes())._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._removeClass_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
_st(self)._deletePackage_(packageName);
return self}, function($ctx1) {$ctx1.fill(self,"removePackage:",{packageName:packageName,pkg:pkg},smalltalk.Smalltalk)})},
messageSends: ["packageAt:ifAbsent:", "error:", ",", "do:", "removeClass:", "classes", "deletePackage:"]}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "renamePackage:to:",
fn: function (packageName,newName){
var self=this;
var pkg;
return smalltalk.withContext(function($ctx1) { 
var $1;
pkg=_st(self)._packageAt_ifAbsent_(packageName,(function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._error_(_st("Missing package: ").__comma(packageName));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$1=_st(self)._packageAt_(newName);
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(self)._error_(_st("Already exists a package called: ").__comma(newName));
};
_st(_st(self)._basicAt_("packages"))._at_put_(newName,pkg);
_st(pkg)._name_(newName);
_st(self)._deletePackage_(packageName);
return self}, function($ctx1) {$ctx1.fill(self,"renamePackage:to:",{packageName:packageName,newName:newName,pkg:pkg},smalltalk.Smalltalk)})},
messageSends: ["packageAt:ifAbsent:", "error:", ",", "ifNotNil:", "packageAt:", "at:put:", "basicAt:", "name:", "deletePackage:"]}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "reservedWords",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.reservedWords;
return self}, function($ctx1) {$ctx1.fill(self,"reservedWords",{},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);

smalltalk.addMethod(
smalltalk.method({
selector: "version",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return "0.10";
}, function($ctx1) {$ctx1.fill(self,"version",{},smalltalk.Smalltalk)})},
messageSends: []}),
smalltalk.Smalltalk);


smalltalk.Smalltalk.klass.iVarNames = ['current'];
smalltalk.addMethod(
smalltalk.method({
selector: "current",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return smalltalk;
return self}, function($ctx1) {$ctx1.fill(self,"current",{},smalltalk.Smalltalk.klass)})},
messageSends: []}),
smalltalk.Smalltalk.klass);


smalltalk.addClass('Timeout', smalltalk.Object, ['rawTimeout'], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "clearInterval",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var interval = self["@rawTimeout"];
		clearInterval(interval);
	;
return self}, function($ctx1) {$ctx1.fill(self,"clearInterval",{},smalltalk.Timeout)})},
messageSends: []}),
smalltalk.Timeout);

smalltalk.addMethod(
smalltalk.method({
selector: "clearTimeout",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var timeout = self["@rawTimeout"];
		clearTimeout(timeout);
	;
return self}, function($ctx1) {$ctx1.fill(self,"clearTimeout",{},smalltalk.Timeout)})},
messageSends: []}),
smalltalk.Timeout);

smalltalk.addMethod(
smalltalk.method({
selector: "rawTimeout:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@rawTimeout"]=anObject;
return self}, function($ctx1) {$ctx1.fill(self,"rawTimeout:",{anObject:anObject},smalltalk.Timeout)})},
messageSends: []}),
smalltalk.Timeout);


smalltalk.addMethod(
smalltalk.method({
selector: "on:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._rawTimeout_(anObject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:",{anObject:anObject},smalltalk.Timeout.klass)})},
messageSends: ["rawTimeout:", "new", "yourself"]}),
smalltalk.Timeout.klass);


smalltalk.addClass('UndefinedObject', smalltalk.Object, [], 'Kernel-Objects');
smalltalk.addMethod(
smalltalk.method({
selector: "asJSON",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=null;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSON",{},smalltalk.UndefinedObject)})},
messageSends: []}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "deepCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"deepCopy",{},smalltalk.UndefinedObject)})},
messageSends: []}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNil:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self;
$1=_st($2)._ifNil_ifNotNil_(aBlock,(function(){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifNil:",{aBlock:aBlock},smalltalk.UndefinedObject)})},
messageSends: ["ifNil:ifNotNil:"]}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNil:ifNotNil:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(aBlock)._value();
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifNil:ifNotNil:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.UndefinedObject)})},
messageSends: ["value"]}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNotNil:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifNotNil:",{aBlock:aBlock},smalltalk.UndefinedObject)})},
messageSends: []}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNotNil:ifNil:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(anotherBlock)._value();
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifNotNil:ifNil:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.UndefinedObject)})},
messageSends: ["value"]}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "isImmutable",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isImmutable",{},smalltalk.UndefinedObject)})},
messageSends: []}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "isNil",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isNil",{},smalltalk.UndefinedObject)})},
messageSends: []}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "notNil",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"notNil",{},smalltalk.UndefinedObject)})},
messageSends: []}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aStream)._nextPutAll_("nil");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.UndefinedObject)})},
messageSends: ["nextPutAll:"]}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "shallowCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"shallowCopy",{},smalltalk.UndefinedObject)})},
messageSends: []}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "subclass:instanceVariableNames:",
fn: function (aString,anotherString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclass_instanceVariableNames_package_(aString,anotherString,nil);
return $1;
}, function($ctx1) {$ctx1.fill(self,"subclass:instanceVariableNames:",{aString:aString,anotherString:anotherString},smalltalk.UndefinedObject)})},
messageSends: ["subclass:instanceVariableNames:package:"]}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "subclass:instanceVariableNames:category:",
fn: function (aString,aString2,aString3){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self)._deprecatedAPI();
$1=_st(self)._subclass_instanceVariableNames_package_(aString,aString2,aString3);
return $1;
}, function($ctx1) {$ctx1.fill(self,"subclass:instanceVariableNames:category:",{aString:aString,aString2:aString2,aString3:aString3},smalltalk.UndefinedObject)})},
messageSends: ["deprecatedAPI", "subclass:instanceVariableNames:package:"]}),
smalltalk.UndefinedObject);

smalltalk.addMethod(
smalltalk.method({
selector: "subclass:instanceVariableNames:package:",
fn: function (aString,aString2,aString3){
var self=this;
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($ClassBuilder())._new())._superclass_subclass_instanceVariableNames_package_(self,_st(aString)._asString(),aString2,aString3);
return $1;
}, function($ctx1) {$ctx1.fill(self,"subclass:instanceVariableNames:package:",{aString:aString,aString2:aString2,aString3:aString3},smalltalk.UndefinedObject)})},
messageSends: ["superclass:subclass:instanceVariableNames:package:", "asString", "new"]}),
smalltalk.UndefinedObject);


smalltalk.addMethod(
smalltalk.method({
selector: "new",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._error_("You cannot create new instances of UndefinedObject. Use nil");
return self}, function($ctx1) {$ctx1.fill(self,"new",{},smalltalk.UndefinedObject.klass)})},
messageSends: ["error:"]}),
smalltalk.UndefinedObject.klass);


smalltalk.addMethod(
smalltalk.method({
selector: "asJavaScriptSelector",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._replace_with_("^([a-zA-Z0-9]*).*$","$1");
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJavaScriptSelector",{},smalltalk.String)})},
messageSends: ["replace:with:"]}),
smalltalk.String);

smalltalk.addPackage('Kernel-Classes');
smalltalk.addClass('Behavior', smalltalk.Object, [], 'Kernel-Classes');
smalltalk.addMethod(
smalltalk.method({
selector: "addCompiledMethod:",
fn: function (aMethod){
var self=this;
var oldMethod,announcement;
function $MethodAdded(){return smalltalk.MethodAdded||(typeof MethodAdded=="undefined"?nil:MethodAdded)}
function $MethodModified(){return smalltalk.MethodModified||(typeof MethodModified=="undefined"?nil:MethodModified)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5,$6;
oldMethod=_st(_st(self)._methodDictionary())._at_ifAbsent_(_st(aMethod)._selector(),(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$1=_st(_st(self)._protocols())._includes_(_st(aMethod)._protocol());
if(! smalltalk.assert($1)){
_st(_st(self)._organization())._addElement_(_st(aMethod)._protocol());
};
_st(self)._basicAddCompiledMethod_(aMethod);
$2=oldMethod;
if(($receiver = $2) == nil || $receiver == undefined){
$3=_st($MethodAdded())._new();
_st($3)._method_(aMethod);
$4=_st($3)._yourself();
announcement=$4;
} else {
$5=_st($MethodModified())._new();
_st($5)._oldMethod_(oldMethod);
_st($5)._method_(aMethod);
$6=_st($5)._yourself();
announcement=$6;
};
_st(_st($SystemAnnouncer())._current())._announce_(announcement);
return self}, function($ctx1) {$ctx1.fill(self,"addCompiledMethod:",{aMethod:aMethod,oldMethod:oldMethod,announcement:announcement},smalltalk.Behavior)})},
messageSends: ["at:ifAbsent:", "selector", "methodDictionary", "ifFalse:", "addElement:", "protocol", "organization", "includes:", "protocols", "basicAddCompiledMethod:", "ifNil:ifNotNil:", "method:", "new", "yourself", "oldMethod:", "announce:", "current"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "allInstanceVariableNames",
fn: function (){
var self=this;
var result;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
result=_st(_st(self)._instanceVariableNames())._copy();
$1=_st(self)._superclass();
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(result)._addAll_(_st(_st(self)._superclass())._allInstanceVariableNames());
};
$2=result;
return $2;
}, function($ctx1) {$ctx1.fill(self,"allInstanceVariableNames",{result:result},smalltalk.Behavior)})},
messageSends: ["copy", "instanceVariableNames", "ifNotNil:", "addAll:", "allInstanceVariableNames", "superclass"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "allSelectors",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$1=_st(_st(self)._allSuperclasses())._inject_into_(_st(self)._selectors(),(function(soFar,aBehavior){
return smalltalk.withContext(function($ctx2) {
$2=soFar;
_st($2)._addAll_(_st(aBehavior)._selectors());
$3=_st($2)._yourself();
return $3;
}, function($ctx2) {$ctx2.fillBlock({soFar:soFar,aBehavior:aBehavior},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"allSelectors",{},smalltalk.Behavior)})},
messageSends: ["inject:into:", "selectors", "addAll:", "yourself", "allSuperclasses"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "allSubclasses",
fn: function (){
var self=this;
var result;
return smalltalk.withContext(function($ctx1) { 
var $1;
result=_st(self)._subclasses();
_st(_st(self)._subclasses())._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(result)._addAll_(_st(each)._allSubclasses());
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$1=result;
return $1;
}, function($ctx1) {$ctx1.fill(self,"allSubclasses",{result:result},smalltalk.Behavior)})},
messageSends: ["subclasses", "do:", "addAll:", "allSubclasses"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "allSubclassesDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._subclasses())._do_((function(each){
return smalltalk.withContext(function($ctx2) {
_st(aBlock)._value_(each);
return _st(each)._allSubclassesDo_(aBlock);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"allSubclassesDo:",{aBlock:aBlock},smalltalk.Behavior)})},
messageSends: ["do:", "value:", "allSubclassesDo:", "subclasses"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "allSuperclasses",
fn: function (){
var self=this;
function $OrderedCollection(){return smalltalk.OrderedCollection||(typeof OrderedCollection=="undefined"?nil:OrderedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$4,$5,$3;
$1=_st(self)._superclass();
if(($receiver = $1) == nil || $receiver == undefined){
$2=[];
return $2;
} else {
$1;
};
$4=_st($OrderedCollection())._with_(_st(self)._superclass());
_st($4)._addAll_(_st(_st(self)._superclass())._allSuperclasses());
$5=_st($4)._yourself();
$3=$5;
return $3;
}, function($ctx1) {$ctx1.fill(self,"allSuperclasses",{},smalltalk.Behavior)})},
messageSends: ["ifNil:", "superclass", "addAll:", "allSuperclasses", "with:", "yourself"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "basicAddCompiledMethod:",
fn: function (aMethod){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.addMethod(aMethod, self);
return self}, function($ctx1) {$ctx1.fill(self,"basicAddCompiledMethod:",{aMethod:aMethod},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "basicNew",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new self.fn();
return self}, function($ctx1) {$ctx1.fill(self,"basicNew",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "basicRemoveCompiledMethod:",
fn: function (aMethod){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		smalltalk.removeMethod(aMethod)
		smalltalk.init(self);
	;
return self}, function($ctx1) {$ctx1.fill(self,"basicRemoveCompiledMethod:",{aMethod:aMethod},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "canUnderstand:",
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(_st(self)._methodDictionary())._keys())._includes_(_st(aSelector)._asString()))._or_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(self)._superclass())._notNil())._and_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st(self)._superclass())._canUnderstand_(aSelector);
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"canUnderstand:",{aSelector:aSelector},smalltalk.Behavior)})},
messageSends: ["or:", "and:", "canUnderstand:", "superclass", "notNil", "includes:", "asString", "keys", "methodDictionary"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "comment",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._basicAt_("comment");
if(($receiver = $2) == nil || $receiver == undefined){
$1="";
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"comment",{},smalltalk.Behavior)})},
messageSends: ["ifNil:", "basicAt:"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "comment:",
fn: function (aString){
var self=this;
function $ClassCommentChanged(){return smalltalk.ClassCommentChanged||(typeof ClassCommentChanged=="undefined"?nil:ClassCommentChanged)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._basicAt_put_("comment",aString);
$1=_st($ClassCommentChanged())._new();
_st($1)._theClass_(self);
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
return self}, function($ctx1) {$ctx1.fill(self,"comment:",{aString:aString},smalltalk.Behavior)})},
messageSends: ["basicAt:put:", "announce:", "theClass:", "new", "yourself", "current"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "commentStamp",
fn: function (){
var self=this;
function $ClassCommentReader(){return smalltalk.ClassCommentReader||(typeof ClassCommentReader=="undefined"?nil:ClassCommentReader)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st($ClassCommentReader())._new();
_st($2)._class_(self);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"commentStamp",{},smalltalk.Behavior)})},
messageSends: ["class:", "new", "yourself"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "commentStamp:prior:",
fn: function (aStamp,prior){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._commentStamp();
return $1;
}, function($ctx1) {$ctx1.fill(self,"commentStamp:prior:",{aStamp:aStamp,prior:prior},smalltalk.Behavior)})},
messageSends: ["commentStamp"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "compile:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._compile_category_(aString,"");
return $1;
}, function($ctx1) {$ctx1.fill(self,"compile:",{aString:aString},smalltalk.Behavior)})},
messageSends: ["compile:category:"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "compile:category:",
fn: function (aString,anotherString){
var self=this;
function $Compiler(){return smalltalk.Compiler||(typeof Compiler=="undefined"?nil:Compiler)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($Compiler())._new())._install_forClass_category_(aString,self,anotherString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"compile:category:",{aString:aString,anotherString:anotherString},smalltalk.Behavior)})},
messageSends: ["install:forClass:category:", "new"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "definition",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return "";
}, function($ctx1) {$ctx1.fill(self,"definition",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "includesBehavior:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self).__eq_eq(aClass))._or_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._inheritsFrom_(aClass);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"includesBehavior:",{aClass:aClass},smalltalk.Behavior)})},
messageSends: ["or:", "inheritsFrom:", "=="]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "includesSelector:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._methodDictionary())._includesKey_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"includesSelector:",{aString:aString},smalltalk.Behavior)})},
messageSends: ["includesKey:", "methodDictionary"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "inheritsFrom:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(aClass)._allSubclasses())._includes_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"inheritsFrom:",{aClass:aClass},smalltalk.Behavior)})},
messageSends: ["includes:", "allSubclasses"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "instanceVariableNames",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.iVarNames;
return self}, function($ctx1) {$ctx1.fill(self,"instanceVariableNames",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "isBehavior",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isBehavior",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "lookupSelector:",
fn: function (selector){
var self=this;
var lookupClass;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
var $early={};
try {
lookupClass=self;
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(lookupClass).__eq(nil);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileFalse_((function(){
return smalltalk.withContext(function($ctx2) {
$1=_st(lookupClass)._includesSelector_(selector);
if(smalltalk.assert($1)){
$2=_st(lookupClass)._methodAt_(selector);
throw $early=[$2];
};
lookupClass=_st(lookupClass)._superclass();
return lookupClass;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return nil;
}
catch(e) {if(e===$early)return e[0]; throw e}
}, function($ctx1) {$ctx1.fill(self,"lookupSelector:",{selector:selector,lookupClass:lookupClass},smalltalk.Behavior)})},
messageSends: ["whileFalse:", "ifTrue:", "methodAt:", "includesSelector:", "superclass", "="]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "methodAt:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._methodDictionary())._at_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"methodAt:",{aString:aString},smalltalk.Behavior)})},
messageSends: ["at:", "methodDictionary"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "methodDictionary",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var dict = smalltalk.HashedCollection._new();
	var methods = self.methods;
	for(var i in methods) {
		if(methods[i].selector) {
			dict._at_put_(methods[i].selector, methods[i]);
		}
	};
	return dict;
return self}, function($ctx1) {$ctx1.fill(self,"methodDictionary",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "methods",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._methodDictionary())._values();
return $1;
}, function($ctx1) {$ctx1.fill(self,"methods",{},smalltalk.Behavior)})},
messageSends: ["values", "methodDictionary"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "methodsFor:",
fn: function (aString){
var self=this;
function $ClassCategoryReader(){return smalltalk.ClassCategoryReader||(typeof ClassCategoryReader=="undefined"?nil:ClassCategoryReader)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st($ClassCategoryReader())._new();
_st($2)._class_category_(self,aString);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"methodsFor:",{aString:aString},smalltalk.Behavior)})},
messageSends: ["class:category:", "new", "yourself"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "methodsFor:stamp:",
fn: function (aString,aStamp){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._methodsFor_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"methodsFor:stamp:",{aString:aString,aStamp:aStamp},smalltalk.Behavior)})},
messageSends: ["methodsFor:"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "methodsInProtocol:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self)._methodDictionary())._values())._select_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st(each)._protocol()).__eq(aString);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"methodsInProtocol:",{aString:aString},smalltalk.Behavior)})},
messageSends: ["select:", "=", "protocol", "values", "methodDictionary"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "name",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.className || nil;
return self}, function($ctx1) {$ctx1.fill(self,"name",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "new",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._basicNew())._initialize();
return $1;
}, function($ctx1) {$ctx1.fill(self,"new",{},smalltalk.Behavior)})},
messageSends: ["initialize", "basicNew"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "organization",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_("organization");
return $1;
}, function($ctx1) {$ctx1.fill(self,"organization",{},smalltalk.Behavior)})},
messageSends: ["basicAt:"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "protocols",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self)._organization())._elements())._sorted();
return $1;
}, function($ctx1) {$ctx1.fill(self,"protocols",{},smalltalk.Behavior)})},
messageSends: ["sorted", "elements", "organization"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "protocolsDo:",
fn: function (aBlock){
var self=this;
var methodsByCategory;
function $HashedCollection(){return smalltalk.HashedCollection||(typeof HashedCollection=="undefined"?nil:HashedCollection)}
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
methodsByCategory=_st($HashedCollection())._new();
_st(_st(_st(self)._methodDictionary())._values())._do_((function(m){
return smalltalk.withContext(function($ctx2) {
return _st(_st(methodsByCategory)._at_ifAbsentPut_(_st(m)._category(),(function(){
return smalltalk.withContext(function($ctx3) {
return _st($Array())._new();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})})))._add_(m);
}, function($ctx2) {$ctx2.fillBlock({m:m},$ctx1)})}));
_st(_st(self)._protocols())._do_((function(category){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_value_(category,_st(methodsByCategory)._at_(category));
}, function($ctx2) {$ctx2.fillBlock({category:category},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"protocolsDo:",{aBlock:aBlock,methodsByCategory:methodsByCategory},smalltalk.Behavior)})},
messageSends: ["new", "do:", "add:", "at:ifAbsentPut:", "category", "values", "methodDictionary", "value:value:", "at:", "protocols"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "prototype",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.fn.prototype;
return self}, function($ctx1) {$ctx1.fill(self,"prototype",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "removeCompiledMethod:",
fn: function (aMethod){
var self=this;
function $MethodRemoved(){return smalltalk.MethodRemoved||(typeof MethodRemoved=="undefined"?nil:MethodRemoved)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._basicRemoveCompiledMethod_(aMethod);
_st(_st(self)._methods())._detect_ifNone_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st(each)._protocol()).__eq(_st(aMethod)._protocol());
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(self)._organization())._removeElement_(_st(aMethod)._protocol());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$1=_st($MethodRemoved())._new();
_st($1)._method_(aMethod);
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
return self}, function($ctx1) {$ctx1.fill(self,"removeCompiledMethod:",{aMethod:aMethod},smalltalk.Behavior)})},
messageSends: ["basicRemoveCompiledMethod:", "detect:ifNone:", "=", "protocol", "removeElement:", "organization", "methods", "announce:", "method:", "new", "yourself", "current"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "selectors",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._methodDictionary())._keys();
return $1;
}, function($ctx1) {$ctx1.fill(self,"selectors",{},smalltalk.Behavior)})},
messageSends: ["keys", "methodDictionary"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "subclasses",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return smalltalk.subclasses(self);
return self}, function($ctx1) {$ctx1.fill(self,"subclasses",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "superclass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.superclass || nil;
return self}, function($ctx1) {$ctx1.fill(self,"superclass",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "theMetaClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._class();
return $1;
}, function($ctx1) {$ctx1.fill(self,"theMetaClass",{},smalltalk.Behavior)})},
messageSends: ["class"]}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "theNonMetaClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"theNonMetaClass",{},smalltalk.Behavior)})},
messageSends: []}),
smalltalk.Behavior);

smalltalk.addMethod(
smalltalk.method({
selector: "withAllSubclasses",
fn: function (){
var self=this;
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st($Array())._with_(self);
_st($2)._addAll_(_st(self)._allSubclasses());
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"withAllSubclasses",{},smalltalk.Behavior)})},
messageSends: ["addAll:", "allSubclasses", "with:", "yourself"]}),
smalltalk.Behavior);



smalltalk.addClass('Class', smalltalk.Behavior, [], 'Kernel-Classes');
smalltalk.addMethod(
smalltalk.method({
selector: "asJavascript",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st("smalltalk.").__comma(_st(self)._name());
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJavascript",{},smalltalk.Class)})},
messageSends: [",", "name"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "category",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._package();
if(($receiver = $2) == nil || $receiver == undefined){
$1="Unclassified";
} else {
$1=_st(_st(self)._package())._name();
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"category",{},smalltalk.Class)})},
messageSends: ["ifNil:ifNotNil:", "name", "package"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "definition",
fn: function (){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$4,$5,$1;
$1=_st($String())._streamContents_((function(stream){
return smalltalk.withContext(function($ctx2) {
$2=stream;
_st($2)._nextPutAll_(_st(_st(self)._superclass())._asString());
_st($2)._nextPutAll_(" subclass: #");
_st($2)._nextPutAll_(_st(self)._name());
_st($2)._nextPutAll_(_st(_st($String())._lf()).__comma(_st($String())._tab()));
$3=_st($2)._nextPutAll_("instanceVariableNames: '");
$3;
_st(_st(self)._instanceVariableNames())._do_separatedBy_((function(each){
return smalltalk.withContext(function($ctx3) {
return _st(stream)._nextPutAll_(each);
}, function($ctx3) {$ctx3.fillBlock({each:each},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx3) {
return _st(stream)._nextPutAll_(" ");
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$4=stream;
_st($4)._nextPutAll_(_st(_st("'").__comma(_st($String())._lf())).__comma(_st($String())._tab()));
_st($4)._nextPutAll_("package: '");
_st($4)._nextPutAll_(_st(self)._category());
$5=_st($4)._nextPutAll_("'");
return $5;
}, function($ctx2) {$ctx2.fillBlock({stream:stream},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"definition",{},smalltalk.Class)})},
messageSends: ["streamContents:", "nextPutAll:", "asString", "superclass", "name", ",", "tab", "lf", "do:separatedBy:", "instanceVariableNames", "category"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "isClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isClass",{},smalltalk.Class)})},
messageSends: []}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "package",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_("pkg");
return $1;
}, function($ctx1) {$ctx1.fill(self,"package",{},smalltalk.Class)})},
messageSends: ["basicAt:"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "package:",
fn: function (aPackage){
var self=this;
var oldPackage;
function $ClassMoved(){return smalltalk.ClassMoved||(typeof ClassMoved=="undefined"?nil:ClassMoved)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
oldPackage=_st(self)._package();
_st(self)._basicAt_put_("pkg",aPackage);
_st(_st(oldPackage)._organization())._removeElement_(self);
_st(_st(aPackage)._organization())._addElement_(self);
$1=_st($ClassMoved())._new();
_st($1)._theClass_(self);
_st($1)._oldPackage_(oldPackage);
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
return self}, function($ctx1) {$ctx1.fill(self,"package:",{aPackage:aPackage,oldPackage:oldPackage},smalltalk.Class)})},
messageSends: ["package", "basicAt:put:", "removeElement:", "organization", "addElement:", "announce:", "theClass:", "new", "oldPackage:", "yourself", "current"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aStream)._nextPutAll_(_st(self)._name());
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Class)})},
messageSends: ["nextPutAll:", "name"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "rename:",
fn: function (aString){
var self=this;
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
_st(_st($ClassBuilder())._new())._renameClass_to_(self,aString);
return self}, function($ctx1) {$ctx1.fill(self,"rename:",{aString:aString},smalltalk.Class)})},
messageSends: ["renameClass:to:", "new"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "subclass:instanceVariableNames:",
fn: function (aString,anotherString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclass_instanceVariableNames_package_(aString,anotherString,nil);
return $1;
}, function($ctx1) {$ctx1.fill(self,"subclass:instanceVariableNames:",{aString:aString,anotherString:anotherString},smalltalk.Class)})},
messageSends: ["subclass:instanceVariableNames:package:"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "subclass:instanceVariableNames:category:",
fn: function (aString,aString2,aString3){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self)._deprecatedAPI();
$1=_st(self)._subclass_instanceVariableNames_package_(aString,aString2,aString3);
return $1;
}, function($ctx1) {$ctx1.fill(self,"subclass:instanceVariableNames:category:",{aString:aString,aString2:aString2,aString3:aString3},smalltalk.Class)})},
messageSends: ["deprecatedAPI", "subclass:instanceVariableNames:package:"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:",
fn: function (aString,aString2,classVars,pools,aString3){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclass_instanceVariableNames_package_(aString,aString2,aString3);
return $1;
}, function($ctx1) {$ctx1.fill(self,"subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:",{aString:aString,aString2:aString2,classVars:classVars,pools:pools,aString3:aString3},smalltalk.Class)})},
messageSends: ["subclass:instanceVariableNames:package:"]}),
smalltalk.Class);

smalltalk.addMethod(
smalltalk.method({
selector: "subclass:instanceVariableNames:package:",
fn: function (aString,aString2,aString3){
var self=this;
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($ClassBuilder())._new())._superclass_subclass_instanceVariableNames_package_(self,_st(aString)._asString(),aString2,aString3);
return $1;
}, function($ctx1) {$ctx1.fill(self,"subclass:instanceVariableNames:package:",{aString:aString,aString2:aString2,aString3:aString3},smalltalk.Class)})},
messageSends: ["superclass:subclass:instanceVariableNames:package:", "asString", "new"]}),
smalltalk.Class);



smalltalk.addClass('Metaclass', smalltalk.Behavior, [], 'Kernel-Classes');
smalltalk.addMethod(
smalltalk.method({
selector: "asJavascript",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("smalltalk.").__comma(_st(_st(self)._instanceClass())._name())).__comma(".klass");
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJavascript",{},smalltalk.Metaclass)})},
messageSends: [",", "name", "instanceClass"]}),
smalltalk.Metaclass);

smalltalk.addMethod(
smalltalk.method({
selector: "definition",
fn: function (){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$1=_st($String())._streamContents_((function(stream){
return smalltalk.withContext(function($ctx2) {
$2=stream;
_st($2)._nextPutAll_(_st(self)._asString());
$3=_st($2)._nextPutAll_(" instanceVariableNames: '");
$3;
_st(_st(self)._instanceVariableNames())._do_separatedBy_((function(each){
return smalltalk.withContext(function($ctx3) {
return _st(stream)._nextPutAll_(each);
}, function($ctx3) {$ctx3.fillBlock({each:each},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx3) {
return _st(stream)._nextPutAll_(" ");
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return _st(stream)._nextPutAll_("'");
}, function($ctx2) {$ctx2.fillBlock({stream:stream},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"definition",{},smalltalk.Metaclass)})},
messageSends: ["streamContents:", "nextPutAll:", "asString", "do:separatedBy:", "instanceVariableNames"]}),
smalltalk.Metaclass);

smalltalk.addMethod(
smalltalk.method({
selector: "instanceClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.instanceClass;
return self}, function($ctx1) {$ctx1.fill(self,"instanceClass",{},smalltalk.Metaclass)})},
messageSends: []}),
smalltalk.Metaclass);

smalltalk.addMethod(
smalltalk.method({
selector: "instanceVariableNames:",
fn: function (aCollection){
var self=this;
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
_st(_st($ClassBuilder())._new())._class_instanceVariableNames_(self,aCollection);
return self}, function($ctx1) {$ctx1.fill(self,"instanceVariableNames:",{aCollection:aCollection},smalltalk.Metaclass)})},
messageSends: ["class:instanceVariableNames:", "new"]}),
smalltalk.Metaclass);

smalltalk.addMethod(
smalltalk.method({
selector: "isMetaclass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isMetaclass",{},smalltalk.Metaclass)})},
messageSends: []}),
smalltalk.Metaclass);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=aStream;
_st($1)._nextPutAll_(_st(_st(self)._instanceClass())._name());
$2=_st($1)._nextPutAll_(" class");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Metaclass)})},
messageSends: ["nextPutAll:", "name", "instanceClass"]}),
smalltalk.Metaclass);

smalltalk.addMethod(
smalltalk.method({
selector: "theMetaClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"theMetaClass",{},smalltalk.Metaclass)})},
messageSends: []}),
smalltalk.Metaclass);

smalltalk.addMethod(
smalltalk.method({
selector: "theNonMetaClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._instanceClass();
return $1;
}, function($ctx1) {$ctx1.fill(self,"theNonMetaClass",{},smalltalk.Metaclass)})},
messageSends: ["instanceClass"]}),
smalltalk.Metaclass);



smalltalk.addClass('ClassBuilder', smalltalk.Object, [], 'Kernel-Classes');
smalltalk.addMethod(
smalltalk.method({
selector: "addSubclassOf:named:instanceVariableNames:package:",
fn: function (aClass,aString,aCollection,packageName){
var self=this;
var theClass;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4;
theClass=_st(_st($Smalltalk())._current())._at_(aString);
$1=theClass;
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
$2=_st(_st(theClass)._superclass()).__eq_eq(aClass);
if(! smalltalk.assert($2)){
$3=_st(self)._migrateClassNamed_superclass_instanceVariableNames_package_(aString,aClass,aCollection,packageName);
return $3;
};
};
$4=_st(self)._basicAddSubclassOf_named_instanceVariableNames_package_(aClass,aString,aCollection,packageName);
return $4;
}, function($ctx1) {$ctx1.fill(self,"addSubclassOf:named:instanceVariableNames:package:",{aClass:aClass,aString:aString,aCollection:aCollection,packageName:packageName,theClass:theClass},smalltalk.ClassBuilder)})},
messageSends: ["at:", "current", "ifNotNil:", "ifFalse:", "migrateClassNamed:superclass:instanceVariableNames:package:", "==", "superclass", "basicAddSubclassOf:named:instanceVariableNames:package:"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "basicAddSubclassOf:named:instanceVariableNames:package:",
fn: function (aClass,aString,aCollection,packageName){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		smalltalk.addClass(aString, aClass, aCollection, packageName);
		return smalltalk[aString]
	;
return self}, function($ctx1) {$ctx1.fill(self,"basicAddSubclassOf:named:instanceVariableNames:package:",{aClass:aClass,aString:aString,aCollection:aCollection,packageName:packageName},smalltalk.ClassBuilder)})},
messageSends: []}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "basicClass:instanceVariableNames:",
fn: function (aClass,aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._basicClass_instanceVariables_(aClass,_st(self)._instanceVariableNamesFor_(aString));
return self}, function($ctx1) {$ctx1.fill(self,"basicClass:instanceVariableNames:",{aClass:aClass,aString:aString},smalltalk.ClassBuilder)})},
messageSends: ["basicClass:instanceVariables:", "instanceVariableNamesFor:"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "basicClass:instanceVariables:",
fn: function (aClass,aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(aClass)._isMetaclass();
if(! smalltalk.assert($1)){
_st(self)._error_(_st(_st(aClass)._name()).__comma(" is not a metaclass"));
};
_st(aClass)._basicAt_put_("iVarNames",aCollection);
return self}, function($ctx1) {$ctx1.fill(self,"basicClass:instanceVariables:",{aClass:aClass,aCollection:aCollection},smalltalk.ClassBuilder)})},
messageSends: ["ifFalse:", "error:", ",", "name", "isMetaclass", "basicAt:put:"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "basicRemoveClass:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.removeClass(aClass);
return self}, function($ctx1) {$ctx1.fill(self,"basicRemoveClass:",{aClass:aClass},smalltalk.ClassBuilder)})},
messageSends: []}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "basicRenameClass:to:",
fn: function (aClass,aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		smalltalk[aString] = aClass;
		delete smalltalk[aClass.className];
		aClass.className = aString;
	;
return self}, function($ctx1) {$ctx1.fill(self,"basicRenameClass:to:",{aClass:aClass,aString:aString},smalltalk.ClassBuilder)})},
messageSends: []}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "basicSwapClassNames:with:",
fn: function (aClass,anotherClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var tmp = aClass.className;
		aClass.className = anotherClass.className;
		anotherClass.className = tmp;
	;
return self}, function($ctx1) {$ctx1.fill(self,"basicSwapClassNames:with:",{aClass:aClass,anotherClass:anotherClass},smalltalk.ClassBuilder)})},
messageSends: []}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "class:instanceVariableNames:",
fn: function (aClass,aString){
var self=this;
function $ClassDefinitionChanged(){return smalltalk.ClassDefinitionChanged||(typeof ClassDefinitionChanged=="undefined"?nil:ClassDefinitionChanged)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._basicClass_instanceVariableNames_(aClass,aString);
_st(self)._setupClass_(aClass);
$1=_st($ClassDefinitionChanged())._new();
_st($1)._theClass_(aClass);
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
return self}, function($ctx1) {$ctx1.fill(self,"class:instanceVariableNames:",{aClass:aClass,aString:aString},smalltalk.ClassBuilder)})},
messageSends: ["basicClass:instanceVariableNames:", "setupClass:", "announce:", "theClass:", "new", "yourself", "current"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "copyClass:named:",
fn: function (aClass,aString){
var self=this;
var newClass;
function $ClassAdded(){return smalltalk.ClassAdded||(typeof ClassAdded=="undefined"?nil:ClassAdded)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
newClass=_st(self)._addSubclassOf_named_instanceVariableNames_package_(_st(aClass)._superclass(),aString,_st(aClass)._instanceVariableNames(),_st(_st(aClass)._package())._name());
_st(self)._copyClass_to_(aClass,newClass);
$1=_st($ClassAdded())._new();
_st($1)._theClass_(newClass);
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
$3=newClass;
return $3;
}, function($ctx1) {$ctx1.fill(self,"copyClass:named:",{aClass:aClass,aString:aString,newClass:newClass},smalltalk.ClassBuilder)})},
messageSends: ["addSubclassOf:named:instanceVariableNames:package:", "superclass", "instanceVariableNames", "name", "package", "copyClass:to:", "announce:", "theClass:", "new", "yourself", "current"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "copyClass:to:",
fn: function (aClass,anotherClass){
var self=this;
function $Compiler(){return smalltalk.Compiler||(typeof Compiler=="undefined"?nil:Compiler)}
return smalltalk.withContext(function($ctx1) { 
_st(anotherClass)._comment_(_st(aClass)._comment());
_st(_st(_st(aClass)._methodDictionary())._values())._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st($Compiler())._new())._install_forClass_category_(_st(each)._source(),anotherClass,_st(each)._category());
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
_st(self)._basicClass_instanceVariables_(_st(anotherClass)._class(),_st(_st(aClass)._class())._instanceVariableNames());
_st(_st(_st(_st(aClass)._class())._methodDictionary())._values())._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st($Compiler())._new())._install_forClass_category_(_st(each)._source(),_st(anotherClass)._class(),_st(each)._category());
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
_st(self)._setupClass_(anotherClass);
return self}, function($ctx1) {$ctx1.fill(self,"copyClass:to:",{aClass:aClass,anotherClass:anotherClass},smalltalk.ClassBuilder)})},
messageSends: ["comment:", "comment", "do:", "install:forClass:category:", "source", "category", "new", "values", "methodDictionary", "basicClass:instanceVariables:", "class", "instanceVariableNames", "setupClass:"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "installMethod:forClass:category:",
fn: function (aCompiledMethod,aBehavior,aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(aCompiledMethod)._category_(aString);
_st(aBehavior)._addCompiledMethod_(aCompiledMethod);
_st(self)._setupClass_(aBehavior);
$1=aCompiledMethod;
return $1;
}, function($ctx1) {$ctx1.fill(self,"installMethod:forClass:category:",{aCompiledMethod:aCompiledMethod,aBehavior:aBehavior,aString:aString},smalltalk.ClassBuilder)})},
messageSends: ["category:", "addCompiledMethod:", "setupClass:"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "instanceVariableNamesFor:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(aString)._tokenize_(" "))._reject_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._isEmpty();
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"instanceVariableNamesFor:",{aString:aString},smalltalk.ClassBuilder)})},
messageSends: ["reject:", "isEmpty", "tokenize:"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "migrateClass:superclass:",
fn: function (aClass,anotherClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(console)._log_(_st(aClass)._name());
_st(self)._migrateClassNamed_superclass_instanceVariableNames_package_(_st(aClass)._name(),anotherClass,_st(aClass)._instanceVariableNames(),_st(_st(aClass)._package())._name());
return self}, function($ctx1) {$ctx1.fill(self,"migrateClass:superclass:",{aClass:aClass,anotherClass:anotherClass},smalltalk.ClassBuilder)})},
messageSends: ["log:", "name", "migrateClassNamed:superclass:instanceVariableNames:package:", "instanceVariableNames", "package"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "migrateClassNamed:superclass:instanceVariableNames:package:",
fn: function (aString,aClass,aCollection,packageName){
var self=this;
var oldClass,newClass,tmp;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
function $Error(){return smalltalk.Error||(typeof Error=="undefined"?nil:Error)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5;
tmp=_st("new*").__comma(aString);
oldClass=_st(_st($Smalltalk())._current())._at_(aString);
newClass=_st(self)._addSubclassOf_named_instanceVariableNames_package_(aClass,tmp,aCollection,packageName);
_st(self)._basicSwapClassNames_with_(oldClass,newClass);
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._copyClass_to_(oldClass,newClass);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._on_do_($Error(),(function(exception){
return smalltalk.withContext(function($ctx2) {
$1=self;
_st($1)._basicSwapClassNames_with_(oldClass,newClass);
$2=_st($1)._basicRemoveClass_(newClass);
$2;
return _st(exception)._signal();
}, function($ctx2) {$ctx2.fillBlock({exception:exception},$ctx1)})}));
$3=self;
_st($3)._rawRenameClass_to_(oldClass,tmp);
$4=_st($3)._rawRenameClass_to_(newClass,aString);
_st(_st(oldClass)._subclasses())._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._migrateClass_superclass_(each,newClass);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
_st(self)._basicRemoveClass_(oldClass);
$5=newClass;
return $5;
}, function($ctx1) {$ctx1.fill(self,"migrateClassNamed:superclass:instanceVariableNames:package:",{aString:aString,aClass:aClass,aCollection:aCollection,packageName:packageName,oldClass:oldClass,newClass:newClass,tmp:tmp},smalltalk.ClassBuilder)})},
messageSends: [",", "at:", "current", "addSubclassOf:named:instanceVariableNames:package:", "basicSwapClassNames:with:", "on:do:", "basicRemoveClass:", "signal", "copyClass:to:", "rawRenameClass:to:", "do:", "migrateClass:superclass:", "subclasses"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "rawRenameClass:to:",
fn: function (aClass,aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		smalltalk[aString] = aClass;
	;
return self}, function($ctx1) {$ctx1.fill(self,"rawRenameClass:to:",{aClass:aClass,aString:aString},smalltalk.ClassBuilder)})},
messageSends: []}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "renameClass:to:",
fn: function (aClass,aString){
var self=this;
function $ClassRenamed(){return smalltalk.ClassRenamed||(typeof ClassRenamed=="undefined"?nil:ClassRenamed)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._basicRenameClass_to_(aClass,aString);
$1=_st($ClassRenamed())._new();
_st($1)._theClass_(aClass);
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
return self}, function($ctx1) {$ctx1.fill(self,"renameClass:to:",{aClass:aClass,aString:aString},smalltalk.ClassBuilder)})},
messageSends: ["basicRenameClass:to:", "announce:", "theClass:", "new", "yourself", "current"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "setupClass:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.init(aClass);;
return self}, function($ctx1) {$ctx1.fill(self,"setupClass:",{aClass:aClass},smalltalk.ClassBuilder)})},
messageSends: []}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "superclass:subclass:",
fn: function (aClass,aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._superclass_subclass_instanceVariableNames_package_(aClass,aString,"",nil);
return $1;
}, function($ctx1) {$ctx1.fill(self,"superclass:subclass:",{aClass:aClass,aString:aString},smalltalk.ClassBuilder)})},
messageSends: ["superclass:subclass:instanceVariableNames:package:"]}),
smalltalk.ClassBuilder);

smalltalk.addMethod(
smalltalk.method({
selector: "superclass:subclass:instanceVariableNames:package:",
fn: function (aClass,aString,aString2,aString3){
var self=this;
var newClass;
function $ClassAdded(){return smalltalk.ClassAdded||(typeof ClassAdded=="undefined"?nil:ClassAdded)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$6,$5,$7,$8,$9;
$1=self;
$2=aClass;
$3=aString;
$4=_st(self)._instanceVariableNamesFor_(aString2);
$6=aString3;
if(($receiver = $6) == nil || $receiver == undefined){
$5="unclassified";
} else {
$5=$6;
};
newClass=_st($1)._addSubclassOf_named_instanceVariableNames_package_($2,$3,$4,$5);
_st(self)._setupClass_(newClass);
$7=_st($ClassAdded())._new();
_st($7)._theClass_(newClass);
$8=_st($7)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($8);
$9=newClass;
return $9;
}, function($ctx1) {$ctx1.fill(self,"superclass:subclass:instanceVariableNames:package:",{aClass:aClass,aString:aString,aString2:aString2,aString3:aString3,newClass:newClass},smalltalk.ClassBuilder)})},
messageSends: ["addSubclassOf:named:instanceVariableNames:package:", "instanceVariableNamesFor:", "ifNil:", "setupClass:", "announce:", "theClass:", "new", "yourself", "current"]}),
smalltalk.ClassBuilder);



smalltalk.addClass('ClassCategoryReader', smalltalk.Object, ['class', 'category'], 'Kernel-Classes');
smalltalk.addMethod(
smalltalk.method({
selector: "class:category:",
fn: function (aClass,aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@class"]=aClass;
self["@category"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"class:category:",{aClass:aClass,aString:aString},smalltalk.ClassCategoryReader)})},
messageSends: []}),
smalltalk.ClassCategoryReader);

smalltalk.addMethod(
smalltalk.method({
selector: "compileMethod:",
fn: function (aString){
var self=this;
function $Compiler(){return smalltalk.Compiler||(typeof Compiler=="undefined"?nil:Compiler)}
return smalltalk.withContext(function($ctx1) { 
_st(_st($Compiler())._new())._install_forClass_category_(aString,self["@class"],self["@category"]);
return self}, function($ctx1) {$ctx1.fill(self,"compileMethod:",{aString:aString},smalltalk.ClassCategoryReader)})},
messageSends: ["install:forClass:category:", "new"]}),
smalltalk.ClassCategoryReader);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.ClassCategoryReader)})},
messageSends: ["initialize"]}),
smalltalk.ClassCategoryReader);

smalltalk.addMethod(
smalltalk.method({
selector: "scanFrom:",
fn: function (aChunkParser){
var self=this;
var chunk;
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
_st((function(){
return smalltalk.withContext(function($ctx2) {
chunk=_st(aChunkParser)._nextChunk();
chunk;
return _st(chunk)._isEmpty();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileFalse_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._compileMethod_(chunk);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(_st($ClassBuilder())._new())._setupClass_(self["@class"]);
return self}, function($ctx1) {$ctx1.fill(self,"scanFrom:",{aChunkParser:aChunkParser,chunk:chunk},smalltalk.ClassCategoryReader)})},
messageSends: ["whileFalse:", "compileMethod:", "nextChunk", "isEmpty", "setupClass:", "new"]}),
smalltalk.ClassCategoryReader);



smalltalk.addClass('ClassCommentReader', smalltalk.Object, ['class'], 'Kernel-Classes');
smalltalk.addMethod(
smalltalk.method({
selector: "class:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@class"]=aClass;
return self}, function($ctx1) {$ctx1.fill(self,"class:",{aClass:aClass},smalltalk.ClassCommentReader)})},
messageSends: []}),
smalltalk.ClassCommentReader);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.ClassCommentReader)})},
messageSends: ["initialize"]}),
smalltalk.ClassCommentReader);

smalltalk.addMethod(
smalltalk.method({
selector: "scanFrom:",
fn: function (aChunkParser){
var self=this;
var chunk;
return smalltalk.withContext(function($ctx1) { 
var $1;
chunk=_st(aChunkParser)._nextChunk();
$1=_st(chunk)._isEmpty();
if(! smalltalk.assert($1)){
_st(self)._setComment_(chunk);
};
return self}, function($ctx1) {$ctx1.fill(self,"scanFrom:",{aChunkParser:aChunkParser,chunk:chunk},smalltalk.ClassCommentReader)})},
messageSends: ["nextChunk", "ifFalse:", "setComment:", "isEmpty"]}),
smalltalk.ClassCommentReader);

smalltalk.addMethod(
smalltalk.method({
selector: "setComment:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@class"])._comment_(aString);
return self}, function($ctx1) {$ctx1.fill(self,"setComment:",{aString:aString},smalltalk.ClassCommentReader)})},
messageSends: ["comment:"]}),
smalltalk.ClassCommentReader);



smalltalk.addClass('ClassSorterNode', smalltalk.Object, ['theClass', 'level', 'nodes'], 'Kernel-Classes');
smalltalk.addMethod(
smalltalk.method({
selector: "getNodesFrom:",
fn: function (aCollection){
var self=this;
var children,others;
function $ClassSorterNode(){return smalltalk.ClassSorterNode||(typeof ClassSorterNode=="undefined"?nil:ClassSorterNode)}
return smalltalk.withContext(function($ctx1) { 
var $1;
children=[];
others=[];
_st(aCollection)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
$1=_st(_st(each)._superclass()).__eq(_st(self)._theClass());
if(smalltalk.assert($1)){
return _st(children)._add_(each);
} else {
return _st(others)._add_(each);
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
self["@nodes"]=_st(children)._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st($ClassSorterNode())._on_classes_level_(each,others,_st(_st(self)._level()).__plus((1)));
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"getNodesFrom:",{aCollection:aCollection,children:children,others:others},smalltalk.ClassSorterNode)})},
messageSends: ["do:", "ifTrue:ifFalse:", "add:", "=", "theClass", "superclass", "collect:", "on:classes:level:", "+", "level"]}),
smalltalk.ClassSorterNode);

smalltalk.addMethod(
smalltalk.method({
selector: "level",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@level"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"level",{},smalltalk.ClassSorterNode)})},
messageSends: []}),
smalltalk.ClassSorterNode);

smalltalk.addMethod(
smalltalk.method({
selector: "level:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@level"]=anInteger;
return self}, function($ctx1) {$ctx1.fill(self,"level:",{anInteger:anInteger},smalltalk.ClassSorterNode)})},
messageSends: []}),
smalltalk.ClassSorterNode);

smalltalk.addMethod(
smalltalk.method({
selector: "nodes",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@nodes"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"nodes",{},smalltalk.ClassSorterNode)})},
messageSends: []}),
smalltalk.ClassSorterNode);

smalltalk.addMethod(
smalltalk.method({
selector: "theClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@theClass"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"theClass",{},smalltalk.ClassSorterNode)})},
messageSends: []}),
smalltalk.ClassSorterNode);

smalltalk.addMethod(
smalltalk.method({
selector: "theClass:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@theClass"]=aClass;
return self}, function($ctx1) {$ctx1.fill(self,"theClass:",{aClass:aClass},smalltalk.ClassSorterNode)})},
messageSends: []}),
smalltalk.ClassSorterNode);

smalltalk.addMethod(
smalltalk.method({
selector: "traverseClassesWith:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aCollection)._add_(_st(self)._theClass());
_st(_st(_st(self)._nodes())._sorted_((function(a,b){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(a)._theClass())._name()).__lt_eq(_st(_st(b)._theClass())._name());
}, function($ctx2) {$ctx2.fillBlock({a:a,b:b},$ctx1)})})))._do_((function(aNode){
return smalltalk.withContext(function($ctx2) {
return _st(aNode)._traverseClassesWith_(aCollection);
}, function($ctx2) {$ctx2.fillBlock({aNode:aNode},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"traverseClassesWith:",{aCollection:aCollection},smalltalk.ClassSorterNode)})},
messageSends: ["add:", "theClass", "do:", "traverseClassesWith:", "sorted:", "<=", "name", "nodes"]}),
smalltalk.ClassSorterNode);


smalltalk.addMethod(
smalltalk.method({
selector: "on:classes:level:",
fn: function (aClass,aCollection,anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._theClass_(aClass);
_st($2)._level_(anInteger);
_st($2)._getNodesFrom_(aCollection);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:classes:level:",{aClass:aClass,aCollection:aCollection,anInteger:anInteger},smalltalk.ClassSorterNode.klass)})},
messageSends: ["theClass:", "new", "level:", "getNodesFrom:", "yourself"]}),
smalltalk.ClassSorterNode.klass);


smalltalk.addPackage('Kernel-Methods');
smalltalk.addClass('BlockClosure', smalltalk.Object, [], 'Kernel-Methods');
smalltalk.addMethod(
smalltalk.method({
selector: "applyTo:arguments:",
fn: function (anObject,aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.apply(anObject, aCollection);
return self}, function($ctx1) {$ctx1.fill(self,"applyTo:arguments:",{anObject:anObject,aCollection:aCollection},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "asCompiledMethod:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return smalltalk.method({selector:aString, fn:self});;
return self}, function($ctx1) {$ctx1.fill(self,"asCompiledMethod:",{aString:aString},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "compiledSource",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.toString();
return self}, function($ctx1) {$ctx1.fill(self,"compiledSource",{},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "currySelf",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		return function () {
			var args = [ this ];
			args.push.apply(args, arguments);
			return self.apply(null, args);
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"currySelf",{},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "ensure:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
try{return self()}finally{aBlock._value()};
return self}, function($ctx1) {$ctx1.fill(self,"ensure:",{aBlock:aBlock},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "fork",
fn: function (){
var self=this;
function $ForkPool(){return smalltalk.ForkPool||(typeof ForkPool=="undefined"?nil:ForkPool)}
return smalltalk.withContext(function($ctx1) { 
_st(_st($ForkPool())._default())._fork_(self);
return self}, function($ctx1) {$ctx1.fill(self,"fork",{},smalltalk.BlockClosure)})},
messageSends: ["fork:", "default"]}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "new",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new self();
return self}, function($ctx1) {$ctx1.fill(self,"new",{},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "newValue:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new self(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"newValue:",{anObject:anObject},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "newValue:value:",
fn: function (anObject,anObject2){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new self(anObject, anObject2);
return self}, function($ctx1) {$ctx1.fill(self,"newValue:value:",{anObject:anObject,anObject2:anObject2},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "newValue:value:value:",
fn: function (anObject,anObject2,anObject3){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new self(anObject, anObject2,anObject3);
return self}, function($ctx1) {$ctx1.fill(self,"newValue:value:value:",{anObject:anObject,anObject2:anObject2,anObject3:anObject3},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "numArgs",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.length;
return self}, function($ctx1) {$ctx1.fill(self,"numArgs",{},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "on:do:",
fn: function (anErrorClass,aBlock){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$1=_st(self)._try_catch_(self,(function(error){
var smalltalkError;
return smalltalk.withContext(function($ctx2) {
smalltalkError=_st(_st($Smalltalk())._current())._asSmalltalkException_(error);
smalltalkError;
$2=_st(smalltalkError)._isKindOf_(anErrorClass);
if(smalltalk.assert($2)){
return _st(aBlock)._value_(smalltalkError);
} else {
return _st(smalltalkError)._signal();
};
}, function($ctx2) {$ctx2.fillBlock({error:error,smalltalkError:smalltalkError},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:do:",{anErrorClass:anErrorClass,aBlock:aBlock},smalltalk.BlockClosure)})},
messageSends: ["try:catch:", "asSmalltalkException:", "current", "ifTrue:ifFalse:", "value:", "signal", "isKindOf:"]}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "timeToRun",
fn: function (){
var self=this;
function $Date(){return smalltalk.Date||(typeof Date=="undefined"?nil:Date)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Date())._millisecondsToRun_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"timeToRun",{},smalltalk.BlockClosure)})},
messageSends: ["millisecondsToRun:"]}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "value",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self();;
return self}, function($ctx1) {$ctx1.fill(self,"value",{},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "value:",
fn: function (anArg){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self(anArg);;
return self}, function($ctx1) {$ctx1.fill(self,"value:",{anArg:anArg},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "value:value:",
fn: function (firstArg,secondArg){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self(firstArg, secondArg);;
return self}, function($ctx1) {$ctx1.fill(self,"value:value:",{firstArg:firstArg,secondArg:secondArg},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "value:value:value:",
fn: function (firstArg,secondArg,thirdArg){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self(firstArg, secondArg, thirdArg);;
return self}, function($ctx1) {$ctx1.fill(self,"value:value:value:",{firstArg:firstArg,secondArg:secondArg,thirdArg:thirdArg},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "valueWithInterval:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var interval = setInterval(self, aNumber);
		return smalltalk.Timeout._on_(interval);
	;
return self}, function($ctx1) {$ctx1.fill(self,"valueWithInterval:",{aNumber:aNumber},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "valueWithPossibleArguments:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.apply(null, aCollection);;
return self}, function($ctx1) {$ctx1.fill(self,"valueWithPossibleArguments:",{aCollection:aCollection},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "valueWithTimeout:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var timeout = setTimeout(self, aNumber);
		return smalltalk.Timeout._on_(timeout);
	;
return self}, function($ctx1) {$ctx1.fill(self,"valueWithTimeout:",{aNumber:aNumber},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "whileFalse",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._whileFalse_((function(){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"whileFalse",{},smalltalk.BlockClosure)})},
messageSends: ["whileFalse:"]}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "whileFalse:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
while(!self()) {aBlock()};
return self}, function($ctx1) {$ctx1.fill(self,"whileFalse:",{aBlock:aBlock},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "whileTrue",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._whileTrue_((function(){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"whileTrue",{},smalltalk.BlockClosure)})},
messageSends: ["whileTrue:"]}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "whileTrue:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
while(self()) {aBlock()};
return self}, function($ctx1) {$ctx1.fill(self,"whileTrue:",{aBlock:aBlock},smalltalk.BlockClosure)})},
messageSends: []}),
smalltalk.BlockClosure);



smalltalk.addClass('CompiledMethod', smalltalk.Object, [], 'Kernel-Methods');
smalltalk.addMethod(
smalltalk.method({
selector: "arguments",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.args || [];
return self}, function($ctx1) {$ctx1.fill(self,"arguments",{},smalltalk.CompiledMethod)})},
messageSends: []}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "category",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._basicAt_("category");
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._defaultCategory();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"category",{},smalltalk.CompiledMethod)})},
messageSends: ["ifNil:", "defaultCategory", "basicAt:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "category:",
fn: function (aString){
var self=this;
var oldProtocol;
function $MethodMoved(){return smalltalk.MethodMoved||(typeof MethodMoved=="undefined"?nil:MethodMoved)}
function $SystemAnnouncer(){return smalltalk.SystemAnnouncer||(typeof SystemAnnouncer=="undefined"?nil:SystemAnnouncer)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
oldProtocol=_st(self)._protocol();
_st(self)._basicAt_put_("category",aString);
$1=_st($MethodMoved())._new();
_st($1)._method_(self);
_st($1)._oldProtocol_(oldProtocol);
$2=_st($1)._yourself();
_st(_st($SystemAnnouncer())._current())._announce_($2);
$3=_st(self)._methodClass();
if(($receiver = $3) == nil || $receiver == undefined){
$3;
} else {
_st(_st(_st(self)._methodClass())._organization())._addElement_(aString);
_st(_st(_st(_st(self)._methodClass())._methods())._select_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st(each)._protocol()).__eq(oldProtocol);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})})))._ifEmpty_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(self)._methodClass())._organization())._removeElement_(oldProtocol);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
};
return self}, function($ctx1) {$ctx1.fill(self,"category:",{aString:aString,oldProtocol:oldProtocol},smalltalk.CompiledMethod)})},
messageSends: ["protocol", "basicAt:put:", "announce:", "method:", "new", "oldProtocol:", "yourself", "current", "ifNotNil:", "addElement:", "organization", "methodClass", "ifEmpty:", "removeElement:", "select:", "=", "methods"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "defaultCategory",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return "as yet unclassified";
}, function($ctx1) {$ctx1.fill(self,"defaultCategory",{},smalltalk.CompiledMethod)})},
messageSends: []}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "fn",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_("fn");
return $1;
}, function($ctx1) {$ctx1.fill(self,"fn",{},smalltalk.CompiledMethod)})},
messageSends: ["basicAt:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "fn:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._basicAt_put_("fn",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"fn:",{aBlock:aBlock},smalltalk.CompiledMethod)})},
messageSends: ["basicAt:put:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "isCompiledMethod",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isCompiledMethod",{},smalltalk.CompiledMethod)})},
messageSends: []}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "isOverridden",
fn: function (){
var self=this;
var selector;
return smalltalk.withContext(function($ctx1) { 
var $1;
var $early={};
try {
selector=_st(self)._selector();
_st(_st(self)._methodClass())._allSubclassesDo_((function(each){
return smalltalk.withContext(function($ctx2) {
$1=_st(each)._includesSelector_(selector);
if(smalltalk.assert($1)){
throw $early=[true];
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return false;
}
catch(e) {if(e===$early)return e[0]; throw e}
}, function($ctx1) {$ctx1.fill(self,"isOverridden",{selector:selector},smalltalk.CompiledMethod)})},
messageSends: ["selector", "allSubclassesDo:", "ifTrue:", "includesSelector:", "methodClass"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "isOverride",
fn: function (){
var self=this;
var superclass;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
superclass=_st(_st(self)._methodClass())._superclass();
$1=superclass;
if(($receiver = $1) == nil || $receiver == undefined){
return false;
} else {
$1;
};
$2=_st(_st(_st(_st(self)._methodClass())._superclass())._lookupSelector_(_st(self)._selector()))._notNil();
return $2;
}, function($ctx1) {$ctx1.fill(self,"isOverride",{superclass:superclass},smalltalk.CompiledMethod)})},
messageSends: ["superclass", "methodClass", "ifNil:", "notNil", "lookupSelector:", "selector"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "messageSends",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_("messageSends");
return $1;
}, function($ctx1) {$ctx1.fill(self,"messageSends",{},smalltalk.CompiledMethod)})},
messageSends: ["basicAt:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "methodClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_("methodClass");
return $1;
}, function($ctx1) {$ctx1.fill(self,"methodClass",{},smalltalk.CompiledMethod)})},
messageSends: ["basicAt:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "protocol",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._category();
return $1;
}, function($ctx1) {$ctx1.fill(self,"protocol",{},smalltalk.CompiledMethod)})},
messageSends: ["category"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "protocol:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._category_(aString);
return self}, function($ctx1) {$ctx1.fill(self,"protocol:",{aString:aString},smalltalk.CompiledMethod)})},
messageSends: ["category:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "referencedClasses",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_("referencedClasses");
return $1;
}, function($ctx1) {$ctx1.fill(self,"referencedClasses",{},smalltalk.CompiledMethod)})},
messageSends: ["basicAt:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "selector",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_("selector");
return $1;
}, function($ctx1) {$ctx1.fill(self,"selector",{},smalltalk.CompiledMethod)})},
messageSends: ["basicAt:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "selector:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._basicAt_put_("selector",aString);
return self}, function($ctx1) {$ctx1.fill(self,"selector:",{aString:aString},smalltalk.CompiledMethod)})},
messageSends: ["basicAt:put:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "source",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._basicAt_("source");
if(($receiver = $2) == nil || $receiver == undefined){
$1="";
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"source",{},smalltalk.CompiledMethod)})},
messageSends: ["ifNil:", "basicAt:"]}),
smalltalk.CompiledMethod);

smalltalk.addMethod(
smalltalk.method({
selector: "source:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._basicAt_put_("source",aString);
return self}, function($ctx1) {$ctx1.fill(self,"source:",{aString:aString},smalltalk.CompiledMethod)})},
messageSends: ["basicAt:put:"]}),
smalltalk.CompiledMethod);



smalltalk.addClass('ForkPool', smalltalk.Object, ['poolSize', 'maxPoolSize', 'queue', 'worker'], 'Kernel-Methods');
smalltalk.addMethod(
smalltalk.method({
selector: "addWorker",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@worker"])._valueWithTimeout_((0));
self["@poolSize"]=_st(self["@poolSize"]).__plus((1));
return self}, function($ctx1) {$ctx1.fill(self,"addWorker",{},smalltalk.ForkPool)})},
messageSends: ["valueWithTimeout:", "+"]}),
smalltalk.ForkPool);

smalltalk.addMethod(
smalltalk.method({
selector: "defaultMaxPoolSize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._defaultMaxPoolSize();
return $1;
}, function($ctx1) {$ctx1.fill(self,"defaultMaxPoolSize",{},smalltalk.ForkPool)})},
messageSends: ["defaultMaxPoolSize", "class"]}),
smalltalk.ForkPool);

smalltalk.addMethod(
smalltalk.method({
selector: "fork:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@poolSize"]).__lt(_st(self)._maxPoolSize());
if(smalltalk.assert($1)){
_st(self)._addWorker();
};
_st(self["@queue"])._nextPut_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"fork:",{aBlock:aBlock},smalltalk.ForkPool)})},
messageSends: ["ifTrue:", "addWorker", "<", "maxPoolSize", "nextPut:"]}),
smalltalk.ForkPool);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
function $Queue(){return smalltalk.Queue||(typeof Queue=="undefined"?nil:Queue)}
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
self["@poolSize"]=(0);
self["@queue"]=_st($Queue())._new();
self["@worker"]=_st(self)._makeWorker();
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.ForkPool)})},
messageSends: ["initialize", "new", "makeWorker"]}),
smalltalk.ForkPool);

smalltalk.addMethod(
smalltalk.method({
selector: "makeWorker",
fn: function (){
var self=this;
var sentinel;
function $Object(){return smalltalk.Object||(typeof Object=="undefined"?nil:Object)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
sentinel=_st($Object())._new();
$1=(function(){
var block;
return smalltalk.withContext(function($ctx2) {
self["@poolSize"]=_st(self["@poolSize"]).__minus((1));
self["@poolSize"];
block=_st(self["@queue"])._nextIfAbsent_((function(){
return smalltalk.withContext(function($ctx3) {
return sentinel;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
block;
$2=_st(block).__eq_eq(sentinel);
if(! smalltalk.assert($2)){
return _st((function(){
return smalltalk.withContext(function($ctx3) {
return _st(block)._value();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}))._ensure_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(self)._addWorker();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
};
}, function($ctx2) {$ctx2.fillBlock({block:block},$ctx1)})});
return $1;
}, function($ctx1) {$ctx1.fill(self,"makeWorker",{sentinel:sentinel},smalltalk.ForkPool)})},
messageSends: ["new", "-", "nextIfAbsent:", "ifFalse:", "ensure:", "addWorker", "value", "=="]}),
smalltalk.ForkPool);

smalltalk.addMethod(
smalltalk.method({
selector: "maxPoolSize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@maxPoolSize"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._defaultMaxPoolSize();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"maxPoolSize",{},smalltalk.ForkPool)})},
messageSends: ["ifNil:", "defaultMaxPoolSize"]}),
smalltalk.ForkPool);

smalltalk.addMethod(
smalltalk.method({
selector: "maxPoolSize:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@maxPoolSize"]=anInteger;
return self}, function($ctx1) {$ctx1.fill(self,"maxPoolSize:",{anInteger:anInteger},smalltalk.ForkPool)})},
messageSends: []}),
smalltalk.ForkPool);


smalltalk.ForkPool.klass.iVarNames = ['default'];
smalltalk.addMethod(
smalltalk.method({
selector: "default",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@default"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@default"]=_st(self)._new();
$1=self["@default"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"default",{},smalltalk.ForkPool.klass)})},
messageSends: ["ifNil:", "new"]}),
smalltalk.ForkPool.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "defaultMaxPoolSize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=(100);
return $1;
}, function($ctx1) {$ctx1.fill(self,"defaultMaxPoolSize",{},smalltalk.ForkPool.klass)})},
messageSends: []}),
smalltalk.ForkPool.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "resetDefault",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@default"]=nil;
return self}, function($ctx1) {$ctx1.fill(self,"resetDefault",{},smalltalk.ForkPool.klass)})},
messageSends: []}),
smalltalk.ForkPool.klass);


smalltalk.addClass('Message', smalltalk.Object, ['selector', 'arguments'], 'Kernel-Methods');
smalltalk.addMethod(
smalltalk.method({
selector: "arguments",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@arguments"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"arguments",{},smalltalk.Message)})},
messageSends: []}),
smalltalk.Message);

smalltalk.addMethod(
smalltalk.method({
selector: "arguments:",
fn: function (anArray){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@arguments"]=anArray;
return self}, function($ctx1) {$ctx1.fill(self,"arguments:",{anArray:anArray},smalltalk.Message)})},
messageSends: []}),
smalltalk.Message);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
smalltalk.Object.fn.prototype._printOn_.apply(_st(self), [aStream]);
$1=aStream;
_st($1)._nextPutAll_("(");
_st($1)._nextPutAll_(_st(self)._selector());
$2=_st($1)._nextPutAll_(")");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Message)})},
messageSends: ["printOn:", "nextPutAll:", "selector"]}),
smalltalk.Message);

smalltalk.addMethod(
smalltalk.method({
selector: "selector",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@selector"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"selector",{},smalltalk.Message)})},
messageSends: []}),
smalltalk.Message);

smalltalk.addMethod(
smalltalk.method({
selector: "selector:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@selector"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"selector:",{aString:aString},smalltalk.Message)})},
messageSends: []}),
smalltalk.Message);

smalltalk.addMethod(
smalltalk.method({
selector: "sendTo:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(anObject)._perform_withArguments_(_st(self)._selector(),_st(self)._arguments());
return $1;
}, function($ctx1) {$ctx1.fill(self,"sendTo:",{anObject:anObject},smalltalk.Message)})},
messageSends: ["perform:withArguments:", "selector", "arguments"]}),
smalltalk.Message);


smalltalk.addMethod(
smalltalk.method({
selector: "selector:arguments:",
fn: function (aString,anArray){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._selector_(aString);
_st($2)._arguments_(anArray);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"selector:arguments:",{aString:aString,anArray:anArray},smalltalk.Message.klass)})},
messageSends: ["selector:", "new", "arguments:", "yourself"]}),
smalltalk.Message.klass);


smalltalk.addClass('MethodContext', smalltalk.Object, [], 'Kernel-Methods');
smalltalk.addMethod(
smalltalk.method({
selector: "asString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._isBlockContext();
if(smalltalk.assert($2)){
$1=_st(_st("a block (in ").__comma(_st(_st(_st(_st(self)._methodContext())._receiver())._class())._printString())).__comma(")");
} else {
$1=_st(_st(_st(_st(_st(self)._receiver())._class())._printString()).__comma(" >> ")).__comma(_st(self)._selector());
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"asString",{},smalltalk.MethodContext)})},
messageSends: ["ifTrue:ifFalse:", ",", "printString", "class", "receiver", "methodContext", "selector", "isBlockContext"]}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "home",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.methodContext || self.homeContext;
return self}, function($ctx1) {$ctx1.fill(self,"home",{},smalltalk.MethodContext)})},
messageSends: []}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "isBlockContext",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._selector())._isNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isBlockContext",{},smalltalk.MethodContext)})},
messageSends: ["isNil", "selector"]}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "locals",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.locals;
return self}, function($ctx1) {$ctx1.fill(self,"locals",{},smalltalk.MethodContext)})},
messageSends: []}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "method",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(_st(self)._methodContext())._receiver())._class())._lookupSelector_(_st(_st(self)._methodContext())._selector());
return $1;
}, function($ctx1) {$ctx1.fill(self,"method",{},smalltalk.MethodContext)})},
messageSends: ["lookupSelector:", "selector", "methodContext", "class", "receiver"]}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "methodContext",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
$1=_st(self)._isBlockContext();
if(! smalltalk.assert($1)){
$2=self;
return $2;
};
$3=_st(self)._home();
return $3;
}, function($ctx1) {$ctx1.fill(self,"methodContext",{},smalltalk.MethodContext)})},
messageSends: ["ifFalse:", "isBlockContext", "home"]}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "outerContext",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.homeContext;
return self}, function($ctx1) {$ctx1.fill(self,"outerContext",{},smalltalk.MethodContext)})},
messageSends: []}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "pc",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.pc;
return self}, function($ctx1) {$ctx1.fill(self,"pc",{},smalltalk.MethodContext)})},
messageSends: []}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
smalltalk.Object.fn.prototype._printOn_.apply(_st(self), [aStream]);
$1=aStream;
_st($1)._nextPutAll_("(");
_st($1)._nextPutAll_(_st(self)._asString());
$2=_st($1)._nextPutAll_(")");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.MethodContext)})},
messageSends: ["printOn:", "nextPutAll:", "asString"]}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "receiver",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.receiver;
return self}, function($ctx1) {$ctx1.fill(self,"receiver",{},smalltalk.MethodContext)})},
messageSends: []}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "selector",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(self.selector) {
			return smalltalk.convertSelector(self.selector);
		} else {
			return nil;
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"selector",{},smalltalk.MethodContext)})},
messageSends: []}),
smalltalk.MethodContext);

smalltalk.addMethod(
smalltalk.method({
selector: "temps",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self)._deprecatedAPI();
$1=_st(self)._locals();
return $1;
}, function($ctx1) {$ctx1.fill(self,"temps",{},smalltalk.MethodContext)})},
messageSends: ["deprecatedAPI", "locals"]}),
smalltalk.MethodContext);



smalltalk.addClass('NativeFunction', smalltalk.Object, [], 'Kernel-Methods');

smalltalk.addMethod(
smalltalk.method({
selector: "constructor:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var native=eval(aString);
		return new native();
	;
return self}, function($ctx1) {$ctx1.fill(self,"constructor:",{aString:aString},smalltalk.NativeFunction.klass)})},
messageSends: []}),
smalltalk.NativeFunction.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "constructor:value:",
fn: function (aString,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var native=eval(aString);
		return new native(anObject);
	;
return self}, function($ctx1) {$ctx1.fill(self,"constructor:value:",{aString:aString,anObject:anObject},smalltalk.NativeFunction.klass)})},
messageSends: []}),
smalltalk.NativeFunction.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "constructor:value:value:",
fn: function (aString,anObject,anObject2){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var native=eval(aString);
		return new native(anObject,anObject2);
	;
return self}, function($ctx1) {$ctx1.fill(self,"constructor:value:value:",{aString:aString,anObject:anObject,anObject2:anObject2},smalltalk.NativeFunction.klass)})},
messageSends: []}),
smalltalk.NativeFunction.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "constructor:value:value:value:",
fn: function (aString,anObject,anObject2,anObject3){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var native=eval(aString);
		return new native(anObject,anObject2, anObject3);
	;
return self}, function($ctx1) {$ctx1.fill(self,"constructor:value:value:value:",{aString:aString,anObject:anObject,anObject2:anObject2,anObject3:anObject3},smalltalk.NativeFunction.klass)})},
messageSends: []}),
smalltalk.NativeFunction.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "exists:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(aString in window) {
			return true
		} else {
			return false
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"exists:",{aString:aString},smalltalk.NativeFunction.klass)})},
messageSends: []}),
smalltalk.NativeFunction.klass);


smalltalk.addPackage('Kernel-Collections');
smalltalk.addClass('Association', smalltalk.Object, ['key', 'value'], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (anAssociation){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self)._class()).__eq(_st(anAssociation)._class()))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(self)._key()).__eq(_st(anAssociation)._key()))._and_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st(self)._value()).__eq(_st(anAssociation)._value());
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"=",{anAssociation:anAssociation},smalltalk.Association)})},
messageSends: ["and:", "=", "value", "key", "class"]}),
smalltalk.Association);

smalltalk.addMethod(
smalltalk.method({
selector: "key",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@key"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"key",{},smalltalk.Association)})},
messageSends: []}),
smalltalk.Association);

smalltalk.addMethod(
smalltalk.method({
selector: "key:",
fn: function (aKey){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@key"]=aKey;
return self}, function($ctx1) {$ctx1.fill(self,"key:",{aKey:aKey},smalltalk.Association)})},
messageSends: []}),
smalltalk.Association);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._key())._printOn_(aStream);
_st(aStream)._nextPutAll_(" -> ");
_st(_st(self)._value())._printOn_(aStream);
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Association)})},
messageSends: ["printOn:", "key", "nextPutAll:", "value"]}),
smalltalk.Association);

smalltalk.addMethod(
smalltalk.method({
selector: "value",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@value"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"value",{},smalltalk.Association)})},
messageSends: []}),
smalltalk.Association);

smalltalk.addMethod(
smalltalk.method({
selector: "value:",
fn: function (aValue){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@value"]=aValue;
return self}, function($ctx1) {$ctx1.fill(self,"value:",{aValue:aValue},smalltalk.Association)})},
messageSends: []}),
smalltalk.Association);


smalltalk.addMethod(
smalltalk.method({
selector: "key:value:",
fn: function (aKey,aValue){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._key_(aKey);
_st($2)._value_(aValue);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"key:value:",{aKey:aKey,aValue:aValue},smalltalk.Association.klass)})},
messageSends: ["key:", "new", "value:", "yourself"]}),
smalltalk.Association.klass);


smalltalk.addClass('Collection', smalltalk.Object, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: ",",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._copy();
_st($2)._addAll_(aCollection);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,",",{aCollection:aCollection},smalltalk.Collection)})},
messageSends: ["addAll:", "copy", "yourself"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "add:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"add:",{anObject:anObject},smalltalk.Collection)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "addAll:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(aCollection)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._add_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$1=aCollection;
return $1;
}, function($ctx1) {$ctx1.fill(self,"addAll:",{aCollection:aCollection},smalltalk.Collection)})},
messageSends: ["do:", "add:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "asArray",
fn: function (){
var self=this;
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Array())._withAll_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"asArray",{},smalltalk.Collection)})},
messageSends: ["withAll:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSON",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._asArray())._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._asJSON();
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSON",{},smalltalk.Collection)})},
messageSends: ["collect:", "asJSON", "asArray"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "asOrderedCollection",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._asArray();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asOrderedCollection",{},smalltalk.Collection)})},
messageSends: ["asArray"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "asSet",
fn: function (){
var self=this;
function $Set(){return smalltalk.Set||(typeof Set=="undefined"?nil:Set)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Set())._withAll_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"asSet",{},smalltalk.Collection)})},
messageSends: ["withAll:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "collect:",
fn: function (aBlock){
var self=this;
var stream;
return smalltalk.withContext(function($ctx1) { 
var $1;
stream=_st(_st(_st(self)._class())._new())._writeStream();
_st(self)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(stream)._nextPut_(_st(aBlock)._value_(each));
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$1=_st(stream)._contents();
return $1;
}, function($ctx1) {$ctx1.fill(self,"collect:",{aBlock:aBlock,stream:stream},smalltalk.Collection)})},
messageSends: ["writeStream", "new", "class", "do:", "nextPut:", "value:", "contents"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "copyWith:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._copy();
_st($2)._add_(anObject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"copyWith:",{anObject:anObject},smalltalk.Collection)})},
messageSends: ["add:", "copy", "yourself"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "copyWithAll:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._copy();
_st($2)._addAll_(aCollection);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"copyWithAll:",{aCollection:aCollection},smalltalk.Collection)})},
messageSends: ["addAll:", "copy", "yourself"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "copyWithoutAll:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._reject_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(aCollection)._includes_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"copyWithoutAll:",{aCollection:aCollection},smalltalk.Collection)})},
messageSends: ["reject:", "includes:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "detect:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._detect_ifNone_(aBlock,(function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._errorNotFound();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"detect:",{aBlock:aBlock},smalltalk.Collection)})},
messageSends: ["detect:ifNone:", "errorNotFound"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "detect:ifNone:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"detect:ifNone:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.Collection)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "do:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"do:",{aBlock:aBlock},smalltalk.Collection)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "do:separatedBy:",
fn: function (aBlock,anotherBlock){
var self=this;
var actionBeforeElement;
return smalltalk.withContext(function($ctx1) { 
actionBeforeElement=(function(){
return smalltalk.withContext(function($ctx2) {
actionBeforeElement=anotherBlock;
return actionBeforeElement;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})});
_st(self)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
_st(actionBeforeElement)._value();
return _st(aBlock)._value_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"do:separatedBy:",{aBlock:aBlock,anotherBlock:anotherBlock,actionBeforeElement:actionBeforeElement},smalltalk.Collection)})},
messageSends: ["do:", "value", "value:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "errorNotFound",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._error_("Object is not in the collection");
return self}, function($ctx1) {$ctx1.fill(self,"errorNotFound",{},smalltalk.Collection)})},
messageSends: ["error:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "ifEmpty:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._isEmpty();
if(smalltalk.assert($2)){
$1=_st(aBlock)._value();
} else {
$1=self;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"ifEmpty:",{aBlock:aBlock},smalltalk.Collection)})},
messageSends: ["ifTrue:ifFalse:", "value", "isEmpty"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "ifNotEmpty:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._notEmpty();
_st($1)._ifTrue_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"ifNotEmpty:",{aBlock:aBlock},smalltalk.Collection)})},
messageSends: ["ifTrue:", "notEmpty"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "includes:",
fn: function (anObject){
var self=this;
var sentinel;
function $Object(){return smalltalk.Object||(typeof Object=="undefined"?nil:Object)}
return smalltalk.withContext(function($ctx1) { 
var $1;
sentinel=_st($Object())._new();
$1=_st(_st(self)._detect_ifNone_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each).__eq(anObject);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx2) {
return sentinel;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))).__tild_eq(sentinel);
return $1;
}, function($ctx1) {$ctx1.fill(self,"includes:",{anObject:anObject,sentinel:sentinel},smalltalk.Collection)})},
messageSends: ["new", "~=", "detect:ifNone:", "="]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "inject:into:",
fn: function (anObject,aBlock){
var self=this;
var result;
return smalltalk.withContext(function($ctx1) { 
var $1;
result=anObject;
_st(self)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
result=_st(aBlock)._value_value_(result,each);
return result;
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$1=result;
return $1;
}, function($ctx1) {$ctx1.fill(self,"inject:into:",{anObject:anObject,aBlock:aBlock,result:result},smalltalk.Collection)})},
messageSends: ["do:", "value:value:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "intersection:",
fn: function (aCollection){
var self=this;
var set,outputSet;
function $Set(){return smalltalk.Set||(typeof Set=="undefined"?nil:Set)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
set=_st(self)._asSet();
outputSet=_st($Set())._new();
_st(aCollection)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
$1=_st(_st(set)._includes_(each))._and_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st(outputSet)._includes_(each))._not();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
if(smalltalk.assert($1)){
return _st(outputSet)._add_(each);
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$2=_st(_st(self)._class())._withAll_(_st(outputSet)._asArray());
return $2;
}, function($ctx1) {$ctx1.fill(self,"intersection:",{aCollection:aCollection,set:set,outputSet:outputSet},smalltalk.Collection)})},
messageSends: ["asSet", "new", "do:", "ifTrue:", "add:", "and:", "not", "includes:", "withAll:", "asArray", "class"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "isEmpty",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._size()).__eq((0));
return $1;
}, function($ctx1) {$ctx1.fill(self,"isEmpty",{},smalltalk.Collection)})},
messageSends: ["=", "size"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "notEmpty",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._isEmpty())._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"notEmpty",{},smalltalk.Collection)})},
messageSends: ["not", "isEmpty"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "occurrencesOf:",
fn: function (anObject){
var self=this;
var tally;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
tally=(0);
_st(self)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
$1=_st(anObject).__eq(each);
if(smalltalk.assert($1)){
tally=_st(tally).__plus((1));
return tally;
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$2=tally;
return $2;
}, function($ctx1) {$ctx1.fill(self,"occurrencesOf:",{anObject:anObject,tally:tally},smalltalk.Collection)})},
messageSends: ["do:", "ifTrue:", "+", "="]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "putOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._putOn_(aStream);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"putOn:",{aStream:aStream},smalltalk.Collection)})},
messageSends: ["do:", "putOn:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "reject:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._select_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st(aBlock)._value_(each)).__eq(false);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"reject:",{aBlock:aBlock},smalltalk.Collection)})},
messageSends: ["select:", "=", "value:"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "remove:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._remove_ifAbsent_(anObject,(function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._errorNotFound();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"remove:",{anObject:anObject},smalltalk.Collection)})},
messageSends: ["remove:ifAbsent:", "errorNotFound"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "remove:ifAbsent:",
fn: function (anObject,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"remove:ifAbsent:",{anObject:anObject,aBlock:aBlock},smalltalk.Collection)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "select:",
fn: function (aBlock){
var self=this;
var stream;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
stream=_st(_st(_st(self)._class())._new())._writeStream();
_st(self)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
$1=_st(aBlock)._value_(each);
if(smalltalk.assert($1)){
return _st(stream)._nextPut_(each);
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$2=_st(stream)._contents();
return $2;
}, function($ctx1) {$ctx1.fill(self,"select:",{aBlock:aBlock,stream:stream},smalltalk.Collection)})},
messageSends: ["writeStream", "new", "class", "do:", "ifTrue:", "nextPut:", "value:", "contents"]}),
smalltalk.Collection);

smalltalk.addMethod(
smalltalk.method({
selector: "size",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"size",{},smalltalk.Collection)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.Collection);


smalltalk.addMethod(
smalltalk.method({
selector: "new:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._new();
return $1;
}, function($ctx1) {$ctx1.fill(self,"new:",{anInteger:anInteger},smalltalk.Collection.klass)})},
messageSends: ["new"]}),
smalltalk.Collection.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "with:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._add_(anObject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"with:",{anObject:anObject},smalltalk.Collection.klass)})},
messageSends: ["add:", "new", "yourself"]}),
smalltalk.Collection.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "with:with:",
fn: function (anObject,anotherObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._add_(anObject);
_st($2)._add_(anotherObject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"with:with:",{anObject:anObject,anotherObject:anotherObject},smalltalk.Collection.klass)})},
messageSends: ["add:", "new", "yourself"]}),
smalltalk.Collection.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "with:with:with:",
fn: function (firstObject,secondObject,thirdObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._add_(firstObject);
_st($2)._add_(secondObject);
_st($2)._add_(thirdObject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"with:with:with:",{firstObject:firstObject,secondObject:secondObject,thirdObject:thirdObject},smalltalk.Collection.klass)})},
messageSends: ["add:", "new", "yourself"]}),
smalltalk.Collection.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "withAll:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._addAll_(aCollection);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"withAll:",{aCollection:aCollection},smalltalk.Collection.klass)})},
messageSends: ["addAll:", "new", "yourself"]}),
smalltalk.Collection.klass);


smalltalk.addClass('IndexableCollection', smalltalk.Collection, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "at:",
fn: function (anIndex){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_ifAbsent_(anIndex,(function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._errorNotFound();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:",{anIndex:anIndex},smalltalk.IndexableCollection)})},
messageSends: ["at:ifAbsent:", "errorNotFound"]}),
smalltalk.IndexableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (anIndex,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassReponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{anIndex:anIndex,aBlock:aBlock},smalltalk.IndexableCollection)})},
messageSends: ["subclassReponsibility"]}),
smalltalk.IndexableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifPresent:",
fn: function (anIndex,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_ifPresent_ifAbsent_(anIndex,aBlock,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:ifPresent:",{anIndex:anIndex,aBlock:aBlock},smalltalk.IndexableCollection)})},
messageSends: ["at:ifPresent:ifAbsent:"]}),
smalltalk.IndexableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifPresent:ifAbsent:",
fn: function (anIndex,aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassReponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"at:ifPresent:ifAbsent:",{anIndex:anIndex,aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.IndexableCollection)})},
messageSends: ["subclassReponsibility"]}),
smalltalk.IndexableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (anIndex,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassReponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"at:put:",{anIndex:anIndex,anObject:anObject},smalltalk.IndexableCollection)})},
messageSends: ["subclassReponsibility"]}),
smalltalk.IndexableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "indexOf:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._indexOf_ifAbsent_(anObject,(function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._errorNotFound();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"indexOf:",{anObject:anObject},smalltalk.IndexableCollection)})},
messageSends: ["indexOf:ifAbsent:", "errorNotFound"]}),
smalltalk.IndexableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "indexOf:ifAbsent:",
fn: function (anObject,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"indexOf:ifAbsent:",{anObject:anObject,aBlock:aBlock},smalltalk.IndexableCollection)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.IndexableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "with:do:",
fn: function (anotherCollection,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._withIndexDo_((function(each,index){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_value_(each,_st(anotherCollection)._at_(index));
}, function($ctx2) {$ctx2.fillBlock({each:each,index:index},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"with:do:",{anotherCollection:anotherCollection,aBlock:aBlock},smalltalk.IndexableCollection)})},
messageSends: ["withIndexDo:", "value:value:", "at:"]}),
smalltalk.IndexableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "withIndexDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassReponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"withIndexDo:",{aBlock:aBlock},smalltalk.IndexableCollection)})},
messageSends: ["subclassReponsibility"]}),
smalltalk.IndexableCollection);



smalltalk.addClass('HashedCollection', smalltalk.IndexableCollection, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: ",",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._shouldNotImplement();
return self}, function($ctx1) {$ctx1.fill(self,",",{aCollection:aCollection},smalltalk.HashedCollection)})},
messageSends: ["shouldNotImplement"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (aHashedCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
$1=_st(_st(self)._class()).__eq(_st(aHashedCollection)._class());
if(! smalltalk.assert($1)){
return false;
};
$2=_st(_st(self)._size()).__eq(_st(aHashedCollection)._size());
if(! smalltalk.assert($2)){
return false;
};
$3=_st(_st(self)._associations()).__eq(_st(aHashedCollection)._associations());
return $3;
}, function($ctx1) {$ctx1.fill(self,"=",{aHashedCollection:aHashedCollection},smalltalk.HashedCollection)})},
messageSends: ["ifFalse:", "=", "class", "size", "associations"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "add:",
fn: function (anAssociation){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_(_st(anAssociation)._key(),_st(anAssociation)._value());
return self}, function($ctx1) {$ctx1.fill(self,"add:",{anAssociation:anAssociation},smalltalk.HashedCollection)})},
messageSends: ["at:put:", "key", "value"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "addAll:",
fn: function (aHashedCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
smalltalk.IndexableCollection.fn.prototype._addAll_.apply(_st(self), [_st(aHashedCollection)._associations()]);
$1=aHashedCollection;
return $1;
}, function($ctx1) {$ctx1.fill(self,"addAll:",{aHashedCollection:aHashedCollection},smalltalk.HashedCollection)})},
messageSends: ["addAll:", "associations"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "asDictionary",
fn: function (){
var self=this;
function $Dictionary(){return smalltalk.Dictionary||(typeof Dictionary=="undefined"?nil:Dictionary)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Dictionary())._fromPairs_(_st(self)._associations());
return $1;
}, function($ctx1) {$ctx1.fill(self,"asDictionary",{},smalltalk.HashedCollection)})},
messageSends: ["fromPairs:", "associations"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSON",
fn: function (){
var self=this;
var c;
return smalltalk.withContext(function($ctx1) { 
var $1;
c=_st(_st(self)._class())._new();
_st(self)._keysAndValuesDo_((function(key,value){
return smalltalk.withContext(function($ctx2) {
return _st(c)._at_put_(key,_st(value)._asJSON());
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
$1=c;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSON",{c:c},smalltalk.HashedCollection)})},
messageSends: ["new", "class", "keysAndValuesDo:", "at:put:", "asJSON"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "associations",
fn: function (){
var self=this;
var associations;
return smalltalk.withContext(function($ctx1) { 
var $1;
associations=[];
_st(self)._associationsDo_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(associations)._add_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$1=associations;
return $1;
}, function($ctx1) {$ctx1.fill(self,"associations",{associations:associations},smalltalk.HashedCollection)})},
messageSends: ["associationsDo:", "add:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "associationsDo:",
fn: function (aBlock){
var self=this;
function $Association(){return smalltalk.Association||(typeof Association=="undefined"?nil:Association)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._keysAndValuesDo_((function(key,value){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_(_st($Association())._key_value_(key,value));
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"associationsDo:",{aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["keysAndValuesDo:", "value:", "key:value:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._includesKey_(aKey);
$1=_st($2)._ifTrue_ifFalse_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._basicAt_(aKey);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}),aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{aKey:aKey,aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["ifTrue:ifFalse:", "basicAt:", "includesKey:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsentPut:",
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_ifAbsent_(aKey,(function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._at_put_(aKey,_st(aBlock)._value());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:ifAbsentPut:",{aKey:aKey,aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["at:ifAbsent:", "at:put:", "value"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifPresent:ifAbsent:",
fn: function (aKey,aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._includesKey_(aKey);
$1=_st($2)._ifTrue_ifFalse_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_(_st(self)._at_(aKey));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}),anotherBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:ifPresent:ifAbsent:",{aKey:aKey,aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.HashedCollection)})},
messageSends: ["ifTrue:ifFalse:", "value:", "at:", "includesKey:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (aKey,aValue){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicAt_put_(aKey,aValue);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:put:",{aKey:aKey,aValue:aValue},smalltalk.HashedCollection)})},
messageSends: ["basicAt:put:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "collect:",
fn: function (aBlock){
var self=this;
var newDict;
return smalltalk.withContext(function($ctx1) { 
var $1;
newDict=_st(_st(self)._class())._new();
_st(self)._keysAndValuesDo_((function(key,value){
return smalltalk.withContext(function($ctx2) {
return _st(newDict)._at_put_(key,_st(aBlock)._value_(value));
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
$1=newDict;
return $1;
}, function($ctx1) {$ctx1.fill(self,"collect:",{aBlock:aBlock,newDict:newDict},smalltalk.HashedCollection)})},
messageSends: ["new", "class", "keysAndValuesDo:", "at:put:", "value:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "deepCopy",
fn: function (){
var self=this;
var copy;
return smalltalk.withContext(function($ctx1) { 
var $1;
copy=_st(_st(self)._class())._new();
_st(self)._keysAndValuesDo_((function(key,value){
return smalltalk.withContext(function($ctx2) {
return _st(copy)._at_put_(key,_st(value)._deepCopy());
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
$1=copy;
return $1;
}, function($ctx1) {$ctx1.fill(self,"deepCopy",{copy:copy},smalltalk.HashedCollection)})},
messageSends: ["new", "class", "keysAndValuesDo:", "at:put:", "deepCopy"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "detect:ifNone:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._values())._detect_ifNone_(aBlock,anotherBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"detect:ifNone:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.HashedCollection)})},
messageSends: ["detect:ifNone:", "values"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "do:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._valuesDo_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"do:",{aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["valuesDo:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "includes:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._values())._includes_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"includes:",{anObject:anObject},smalltalk.HashedCollection)})},
messageSends: ["includes:", "values"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "includesKey:",
fn: function (aKey){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.hasOwnProperty(aKey);
return self}, function($ctx1) {$ctx1.fill(self,"includesKey:",{aKey:aKey},smalltalk.HashedCollection)})},
messageSends: []}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "indexOf:ifAbsent:",
fn: function (anObject,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._keys())._detect_ifNone_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(_st(self)._at_(each)).__eq(anObject);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}),aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"indexOf:ifAbsent:",{anObject:anObject,aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["detect:ifNone:", "=", "at:", "keys"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "keys",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if ('function'===typeof Object.keys) return Object.keys(self);
		var keys = [];
		for(var i in self) {
			if(self.hasOwnProperty(i)) {
				keys.push(i);
			}
		};
		return keys;
	;
return self}, function($ctx1) {$ctx1.fill(self,"keys",{},smalltalk.HashedCollection)})},
messageSends: []}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "keysAndValuesDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._keysDo_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_value_(each,_st(self)._at_(each));
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"keysAndValuesDo:",{aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["keysDo:", "value:value:", "at:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "keysDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._keys())._do_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"keysDo:",{aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["do:", "keys"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.IndexableCollection.fn.prototype._printOn_.apply(_st(self), [aStream]);
_st(aStream)._nextPutAll_(" (");
_st(_st(self)._associations())._do_separatedBy_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._printOn_(aStream);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx2) {
return _st(aStream)._nextPutAll_(" , ");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(aStream)._nextPutAll_(")");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.HashedCollection)})},
messageSends: ["printOn:", "nextPutAll:", "do:separatedBy:", "associations"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "remove:ifAbsent:",
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._removeKey_ifAbsent_(aKey,aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"remove:ifAbsent:",{aKey:aKey,aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["removeKey:ifAbsent:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "removeKey:",
fn: function (aKey){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._remove_(aKey);
return $1;
}, function($ctx1) {$ctx1.fill(self,"removeKey:",{aKey:aKey},smalltalk.HashedCollection)})},
messageSends: ["remove:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "removeKey:ifAbsent:",
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._includesKey_(aKey);
if(smalltalk.assert($2)){
$1=_st(self)._basicDelete_(aKey);
} else {
$1=_st(aBlock)._value();
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"removeKey:ifAbsent:",{aKey:aKey,aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["ifFalse:ifTrue:", "value", "basicDelete:", "includesKey:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "select:",
fn: function (aBlock){
var self=this;
var newDict;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
newDict=_st(_st(self)._class())._new();
_st(self)._keysAndValuesDo_((function(key,value){
return smalltalk.withContext(function($ctx2) {
$1=_st(aBlock)._value_(value);
if(smalltalk.assert($1)){
return _st(newDict)._at_put_(key,value);
};
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
$2=newDict;
return $2;
}, function($ctx1) {$ctx1.fill(self,"select:",{aBlock:aBlock,newDict:newDict},smalltalk.HashedCollection)})},
messageSends: ["new", "class", "keysAndValuesDo:", "ifTrue:", "at:put:", "value:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "shallowCopy",
fn: function (){
var self=this;
var copy;
return smalltalk.withContext(function($ctx1) { 
var $1;
copy=_st(_st(self)._class())._new();
_st(self)._keysAndValuesDo_((function(key,value){
return smalltalk.withContext(function($ctx2) {
return _st(copy)._at_put_(key,value);
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
$1=copy;
return $1;
}, function($ctx1) {$ctx1.fill(self,"shallowCopy",{copy:copy},smalltalk.HashedCollection)})},
messageSends: ["new", "class", "keysAndValuesDo:", "at:put:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "size",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._keys())._size();
return $1;
}, function($ctx1) {$ctx1.fill(self,"size",{},smalltalk.HashedCollection)})},
messageSends: ["size", "keys"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "values",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._keys())._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._at_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"values",{},smalltalk.HashedCollection)})},
messageSends: ["collect:", "at:", "keys"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "valuesDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._keysAndValuesDo_((function(key,value){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_(value);
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"valuesDo:",{aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["keysAndValuesDo:", "value:"]}),
smalltalk.HashedCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "withIndexDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._keysAndValuesDo_((function(key,value){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_value_(value,key);
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"withIndexDo:",{aBlock:aBlock},smalltalk.HashedCollection)})},
messageSends: ["keysAndValuesDo:", "value:value:"]}),
smalltalk.HashedCollection);


smalltalk.addMethod(
smalltalk.method({
selector: "fromPairs:",
fn: function (aCollection){
var self=this;
var dict;
return smalltalk.withContext(function($ctx1) { 
var $1;
dict=_st(self)._new();
_st(aCollection)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(dict)._add_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$1=dict;
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromPairs:",{aCollection:aCollection,dict:dict},smalltalk.HashedCollection.klass)})},
messageSends: ["new", "do:", "add:"]}),
smalltalk.HashedCollection.klass);


smalltalk.addClass('Dictionary', smalltalk.HashedCollection, ['keys', 'values'], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "asHashedCollection",
fn: function (){
var self=this;
function $HashedCollection(){return smalltalk.HashedCollection||(typeof HashedCollection=="undefined"?nil:HashedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($HashedCollection())._fromPairs_(_st(self)._associations());
return $1;
}, function($ctx1) {$ctx1.fill(self,"asHashedCollection",{},smalltalk.Dictionary)})},
messageSends: ["fromPairs:", "associations"]}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSON",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._asHashedCollection())._asJSON();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSON",{},smalltalk.Dictionary)})},
messageSends: ["asJSON", "asHashedCollection"]}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var index = self._positionOfKey_(aKey);
		return index >=0 ? self['@values'][index] : aBlock();
	;
return self}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{aKey:aKey,aBlock:aBlock},smalltalk.Dictionary)})},
messageSends: []}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (aKey,aValue){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var index = self._positionOfKey_(aKey);
		if(index === -1) {
			var keys = self['@keys'];
			index = keys.length;
			keys.push(aKey);
		}

		return self['@values'][index] = aValue;
	;
return self}, function($ctx1) {$ctx1.fill(self,"at:put:",{aKey:aKey,aValue:aValue},smalltalk.Dictionary)})},
messageSends: []}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "includesKey:",
fn: function (aKey){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 return self._positionOfKey_(aKey) >= 0; ;
return self}, function($ctx1) {$ctx1.fill(self,"includesKey:",{aKey:aKey},smalltalk.Dictionary)})},
messageSends: []}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "indexOf:ifAbsent:",
fn: function (anObject,aBlock){
var self=this;
var index;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
index=_st(self["@values"])._indexOf_ifAbsent_(anObject,(function(){
return smalltalk.withContext(function($ctx2) {
return (0);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st(index).__eq((0));
if(smalltalk.assert($2)){
$1=_st(aBlock)._value();
} else {
$1=_st(self["@keys"])._at_(index);
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"indexOf:ifAbsent:",{anObject:anObject,aBlock:aBlock,index:index},smalltalk.Dictionary)})},
messageSends: ["indexOf:ifAbsent:", "ifTrue:ifFalse:", "value", "at:", "="]}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.HashedCollection.fn.prototype._initialize.apply(_st(self), []);
self["@keys"]=[];
self["@values"]=[];
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.Dictionary)})},
messageSends: ["initialize"]}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "keys",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@keys"])._copy();
return $1;
}, function($ctx1) {$ctx1.fill(self,"keys",{},smalltalk.Dictionary)})},
messageSends: ["copy"]}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "keysAndValuesDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@keys"])._with_do_(self["@values"],aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"keysAndValuesDo:",{aBlock:aBlock},smalltalk.Dictionary)})},
messageSends: ["with:do:"]}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "keysDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@keys"])._do_(aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"keysDo:",{aBlock:aBlock},smalltalk.Dictionary)})},
messageSends: ["do:"]}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "positionOfKey:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var keys = self['@keys'];
		for(var i=0;i<keys.length;i++){
			if(keys[i].__eq(anObject)) { return i;}
		}
		return -1;
	;
return self}, function($ctx1) {$ctx1.fill(self,"positionOfKey:",{anObject:anObject},smalltalk.Dictionary)})},
messageSends: []}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "removeKey:ifAbsent:",
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var index = self._positionOfKey_(aKey);
		if(index === -1) {
			return aBlock()
		} else {
			var keys = self['@keys'], values = self['@values'];
			var value = values[index], l = keys.length;
			keys[index] = keys[l-1];
			keys.pop();
			values[index] = values[l-1];
			values.pop();
			return value;
		}
	;
return self}, function($ctx1) {$ctx1.fill(self,"removeKey:ifAbsent:",{aKey:aKey,aBlock:aBlock},smalltalk.Dictionary)})},
messageSends: []}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "values",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@values"])._copy();
return $1;
}, function($ctx1) {$ctx1.fill(self,"values",{},smalltalk.Dictionary)})},
messageSends: ["copy"]}),
smalltalk.Dictionary);

smalltalk.addMethod(
smalltalk.method({
selector: "valuesDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@values"])._do_(aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"valuesDo:",{aBlock:aBlock},smalltalk.Dictionary)})},
messageSends: ["do:"]}),
smalltalk.Dictionary);



smalltalk.addClass('SequenceableCollection', smalltalk.IndexableCollection, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
var $early={};
try {
$1=_st(_st(_st(self)._class()).__eq(_st(aCollection)._class()))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(self)._size()).__eq(_st(aCollection)._size());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(! smalltalk.assert($1)){
return false;
};
_st(self)._withIndexDo_((function(each,i){
return smalltalk.withContext(function($ctx2) {
$2=_st(_st(aCollection)._at_(i)).__eq(each);
if(! smalltalk.assert($2)){
throw $early=[false];
};
}, function($ctx2) {$ctx2.fillBlock({each:each,i:i},$ctx1)})}));
return true;
}
catch(e) {if(e===$early)return e[0]; throw e}
}, function($ctx1) {$ctx1.fill(self,"=",{aCollection:aCollection},smalltalk.SequenceableCollection)})},
messageSends: ["ifFalse:", "and:", "=", "size", "class", "withIndexDo:", "at:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "addLast:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._add_(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"addLast:",{anObject:anObject},smalltalk.SequenceableCollection)})},
messageSends: ["add:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "allButFirst",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._copyFrom_to_((2),_st(self)._size());
return $1;
}, function($ctx1) {$ctx1.fill(self,"allButFirst",{},smalltalk.SequenceableCollection)})},
messageSends: ["copyFrom:to:", "size"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "allButLast",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._copyFrom_to_((1),_st(_st(self)._size()).__minus((1)));
return $1;
}, function($ctx1) {$ctx1.fill(self,"allButLast",{},smalltalk.SequenceableCollection)})},
messageSends: ["copyFrom:to:", "-", "size"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "atRandom",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_(_st(_st(self)._size())._atRandom());
return $1;
}, function($ctx1) {$ctx1.fill(self,"atRandom",{},smalltalk.SequenceableCollection)})},
messageSends: ["at:", "atRandom", "size"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "copyFrom:to:",
fn: function (anIndex,anotherIndex){
var self=this;
var range,newCollection;
return smalltalk.withContext(function($ctx1) { 
var $1;
range=_st(anIndex)._to_(anotherIndex);
newCollection=_st(_st(self)._class())._new_(_st(range)._size());
_st(range)._withIndexDo_((function(each,i){
return smalltalk.withContext(function($ctx2) {
return _st(newCollection)._at_put_(i,_st(self)._at_(each));
}, function($ctx2) {$ctx2.fillBlock({each:each,i:i},$ctx1)})}));
$1=newCollection;
return $1;
}, function($ctx1) {$ctx1.fill(self,"copyFrom:to:",{anIndex:anIndex,anotherIndex:anotherIndex,range:range,newCollection:newCollection},smalltalk.SequenceableCollection)})},
messageSends: ["to:", "new:", "size", "class", "withIndexDo:", "at:put:", "at:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "deepCopy",
fn: function (){
var self=this;
var newCollection;
return smalltalk.withContext(function($ctx1) { 
var $1;
newCollection=_st(_st(self)._class())._new_(_st(self)._size());
_st(self)._withIndexDo_((function(each,index){
return smalltalk.withContext(function($ctx2) {
return _st(newCollection)._at_put_(index,_st(each)._deepCopy());
}, function($ctx2) {$ctx2.fillBlock({each:each,index:index},$ctx1)})}));
$1=newCollection;
return $1;
}, function($ctx1) {$ctx1.fill(self,"deepCopy",{newCollection:newCollection},smalltalk.SequenceableCollection)})},
messageSends: ["new:", "size", "class", "withIndexDo:", "at:put:", "deepCopy"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "detect:ifNone:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		for(var i = 0; i < self.length; i++)
			if(aBlock(self[i]))
				return self[i];
		return anotherBlock();
	;
return self}, function($ctx1) {$ctx1.fill(self,"detect:ifNone:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.SequenceableCollection)})},
messageSends: []}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "do:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
for(var i=0;i<self.length;i++){aBlock(self[i]);};
return self}, function($ctx1) {$ctx1.fill(self,"do:",{aBlock:aBlock},smalltalk.SequenceableCollection)})},
messageSends: []}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "first",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_((1));
return $1;
}, function($ctx1) {$ctx1.fill(self,"first",{},smalltalk.SequenceableCollection)})},
messageSends: ["at:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "first:",
fn: function (n){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._copyFrom_to_((1),n);
return $1;
}, function($ctx1) {$ctx1.fill(self,"first:",{n:n},smalltalk.SequenceableCollection)})},
messageSends: ["copyFrom:to:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "fourth",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_((4));
return $1;
}, function($ctx1) {$ctx1.fill(self,"fourth",{},smalltalk.SequenceableCollection)})},
messageSends: ["at:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "includes:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._indexOf_ifAbsent_(anObject,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})})))._notNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"includes:",{anObject:anObject},smalltalk.SequenceableCollection)})},
messageSends: ["notNil", "indexOf:ifAbsent:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "indexOf:ifAbsent:",
fn: function (anObject,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		for(var i=0;i<self.length;i++) {
			if(self[i].__eq(anObject)) {return i+1}
		};
		return aBlock();
	;
return self}, function($ctx1) {$ctx1.fill(self,"indexOf:ifAbsent:",{anObject:anObject,aBlock:aBlock},smalltalk.SequenceableCollection)})},
messageSends: []}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "indexOf:startingAt:",
fn: function (anObject,start){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._indexOf_startingAt_ifAbsent_(anObject,start,(function(){
return smalltalk.withContext(function($ctx2) {
return (0);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"indexOf:startingAt:",{anObject:anObject,start:start},smalltalk.SequenceableCollection)})},
messageSends: ["indexOf:startingAt:ifAbsent:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "indexOf:startingAt:ifAbsent:",
fn: function (anObject,start,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		for(var i=start-1;i<self.length;i++){
			if(self[i].__eq(anObject)) {return i+1}
		}
		return aBlock();
	;
return self}, function($ctx1) {$ctx1.fill(self,"indexOf:startingAt:ifAbsent:",{anObject:anObject,start:start,aBlock:aBlock},smalltalk.SequenceableCollection)})},
messageSends: []}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "last",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_(_st(self)._size());
return $1;
}, function($ctx1) {$ctx1.fill(self,"last",{},smalltalk.SequenceableCollection)})},
messageSends: ["at:", "size"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "newStream",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._streamClass())._on_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"newStream",{},smalltalk.SequenceableCollection)})},
messageSends: ["on:", "streamClass"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "readStream",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._stream();
return $1;
}, function($ctx1) {$ctx1.fill(self,"readStream",{},smalltalk.SequenceableCollection)})},
messageSends: ["stream"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "removeLast",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._remove_(_st(self)._last());
return self}, function($ctx1) {$ctx1.fill(self,"removeLast",{},smalltalk.SequenceableCollection)})},
messageSends: ["remove:", "last"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "reversed",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"reversed",{},smalltalk.SequenceableCollection)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "second",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_((2));
return $1;
}, function($ctx1) {$ctx1.fill(self,"second",{},smalltalk.SequenceableCollection)})},
messageSends: ["at:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "shallowCopy",
fn: function (){
var self=this;
var newCollection;
return smalltalk.withContext(function($ctx1) { 
var $1;
newCollection=_st(_st(self)._class())._new_(_st(self)._size());
_st(self)._withIndexDo_((function(each,index){
return smalltalk.withContext(function($ctx2) {
return _st(newCollection)._at_put_(index,each);
}, function($ctx2) {$ctx2.fillBlock({each:each,index:index},$ctx1)})}));
$1=newCollection;
return $1;
}, function($ctx1) {$ctx1.fill(self,"shallowCopy",{newCollection:newCollection},smalltalk.SequenceableCollection)})},
messageSends: ["new:", "size", "class", "withIndexDo:", "at:put:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "stream",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._newStream();
return $1;
}, function($ctx1) {$ctx1.fill(self,"stream",{},smalltalk.SequenceableCollection)})},
messageSends: ["newStream"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "streamClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._streamClass();
return $1;
}, function($ctx1) {$ctx1.fill(self,"streamClass",{},smalltalk.SequenceableCollection)})},
messageSends: ["streamClass", "class"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "third",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._at_((3));
return $1;
}, function($ctx1) {$ctx1.fill(self,"third",{},smalltalk.SequenceableCollection)})},
messageSends: ["at:"]}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "with:do:",
fn: function (anotherCollection,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
for(var i=0;i<self.length;i++){aBlock(self[i], anotherCollection[i]);};
return self}, function($ctx1) {$ctx1.fill(self,"with:do:",{anotherCollection:anotherCollection,aBlock:aBlock},smalltalk.SequenceableCollection)})},
messageSends: []}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "withIndexDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
for(var i=0;i<self.length;i++){aBlock(self[i], i+1);};
return self}, function($ctx1) {$ctx1.fill(self,"withIndexDo:",{aBlock:aBlock},smalltalk.SequenceableCollection)})},
messageSends: []}),
smalltalk.SequenceableCollection);

smalltalk.addMethod(
smalltalk.method({
selector: "writeStream",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._stream();
return $1;
}, function($ctx1) {$ctx1.fill(self,"writeStream",{},smalltalk.SequenceableCollection)})},
messageSends: ["stream"]}),
smalltalk.SequenceableCollection);


smalltalk.addMethod(
smalltalk.method({
selector: "streamClass",
fn: function (){
var self=this;
function $Stream(){return smalltalk.Stream||(typeof Stream=="undefined"?nil:Stream)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=$Stream();
return $1;
}, function($ctx1) {$ctx1.fill(self,"streamClass",{},smalltalk.SequenceableCollection.klass)})},
messageSends: []}),
smalltalk.SequenceableCollection.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "streamContents:",
fn: function (aBlock){
var self=this;
var stream;
return smalltalk.withContext(function($ctx1) { 
var $1;
stream=_st(_st(self)._streamClass())._on_(_st(self)._new());
_st(aBlock)._value_(stream);
$1=_st(stream)._contents();
return $1;
}, function($ctx1) {$ctx1.fill(self,"streamContents:",{aBlock:aBlock,stream:stream},smalltalk.SequenceableCollection.klass)})},
messageSends: ["on:", "new", "streamClass", "value:", "contents"]}),
smalltalk.SequenceableCollection.klass);


smalltalk.addClass('Array', smalltalk.SequenceableCollection, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "add:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.push(anObject); return anObject;;
return self}, function($ctx1) {$ctx1.fill(self,"add:",{anObject:anObject},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "asJavascript",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("[").__comma(_st(_st(self)._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._asJavascript();
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})})))._join_(", "))).__comma("]");
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJavascript",{},smalltalk.Array)})},
messageSends: [",", "join:", "collect:", "asJavascript"]}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (anIndex,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if((anIndex < 1) || (self.length < anIndex)) {return aBlock()};
		return self[anIndex - 1];
	;
return self}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{anIndex:anIndex,aBlock:aBlock},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (anIndex,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self[anIndex - 1] = anObject;
return self}, function($ctx1) {$ctx1.fill(self,"at:put:",{anIndex:anIndex,anObject:anObject},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "join:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.join(aString);
return self}, function($ctx1) {$ctx1.fill(self,"join:",{aString:aString},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.SequenceableCollection.fn.prototype._printOn_.apply(_st(self), [aStream]);
_st(aStream)._nextPutAll_(" (");
_st(self)._do_separatedBy_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._printOn_(aStream);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx2) {
return _st(aStream)._nextPutAll_(" ");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(aStream)._nextPutAll_(")");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Array)})},
messageSends: ["printOn:", "nextPutAll:", "do:separatedBy:"]}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "remove:ifAbsent:",
fn: function (anObject,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		for(var i=0;i<self.length;i++) {
			if(self[i] == anObject) {
				self.splice(i,1);
				return self;
			}
		};
		aBlock._value();
	;
return self}, function($ctx1) {$ctx1.fill(self,"remove:ifAbsent:",{anObject:anObject,aBlock:aBlock},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "removeFrom:to:",
fn: function (aNumber,anotherNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.splice(aNumber - 1,anotherNumber - 1);
return self}, function($ctx1) {$ctx1.fill(self,"removeFrom:to:",{aNumber:aNumber,anotherNumber:anotherNumber},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "reversed",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self._copy().reverse();
return self}, function($ctx1) {$ctx1.fill(self,"reversed",{},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "size",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.length;
return self}, function($ctx1) {$ctx1.fill(self,"size",{},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "sort",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._basicPerform_("sort");
return $1;
}, function($ctx1) {$ctx1.fill(self,"sort",{},smalltalk.Array)})},
messageSends: ["basicPerform:"]}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "sort:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		return self.sort(function(a, b) {
			if(aBlock(a,b)) {return -1} else {return 1}
		})
	;
return self}, function($ctx1) {$ctx1.fill(self,"sort:",{aBlock:aBlock},smalltalk.Array)})},
messageSends: []}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "sorted",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._copy())._sort();
return $1;
}, function($ctx1) {$ctx1.fill(self,"sorted",{},smalltalk.Array)})},
messageSends: ["sort", "copy"]}),
smalltalk.Array);

smalltalk.addMethod(
smalltalk.method({
selector: "sorted:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._copy())._sort_(aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"sorted:",{aBlock:aBlock},smalltalk.Array)})},
messageSends: ["sort:", "copy"]}),
smalltalk.Array);


smalltalk.addMethod(
smalltalk.method({
selector: "new:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new Array(anInteger);
return self}, function($ctx1) {$ctx1.fill(self,"new:",{anInteger:anInteger},smalltalk.Array.klass)})},
messageSends: []}),
smalltalk.Array.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "with:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new_((1));
_st($2)._at_put_((1),anObject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"with:",{anObject:anObject},smalltalk.Array.klass)})},
messageSends: ["at:put:", "new:", "yourself"]}),
smalltalk.Array.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "with:with:",
fn: function (anObject,anObject2){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new_((2));
_st($2)._at_put_((1),anObject);
_st($2)._at_put_((2),anObject2);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"with:with:",{anObject:anObject,anObject2:anObject2},smalltalk.Array.klass)})},
messageSends: ["at:put:", "new:", "yourself"]}),
smalltalk.Array.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "with:with:with:",
fn: function (anObject,anObject2,anObject3){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new_((3));
_st($2)._at_put_((1),anObject);
_st($2)._at_put_((2),anObject2);
_st($2)._at_put_((3),anObject3);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"with:with:with:",{anObject:anObject,anObject2:anObject2,anObject3:anObject3},smalltalk.Array.klass)})},
messageSends: ["at:put:", "new:", "yourself"]}),
smalltalk.Array.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "withAll:",
fn: function (aCollection){
var self=this;
var instance,index;
return smalltalk.withContext(function($ctx1) { 
var $1;
index=(1);
instance=_st(self)._new_(_st(aCollection)._size());
_st(aCollection)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
_st(instance)._at_put_(index,each);
index=_st(index).__plus((1));
return index;
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$1=instance;
return $1;
}, function($ctx1) {$ctx1.fill(self,"withAll:",{aCollection:aCollection,instance:instance,index:index},smalltalk.Array.klass)})},
messageSends: ["new:", "size", "do:", "at:put:", "+"]}),
smalltalk.Array.klass);


smalltalk.addClass('CharacterArray', smalltalk.SequenceableCollection, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: ",",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._asString()).__comma(_st(aString)._asString());
return $1;
}, function($ctx1) {$ctx1.fill(self,",",{aString:aString},smalltalk.CharacterArray)})},
messageSends: [",", "asString"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "add:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._errorReadOnly();
return self}, function($ctx1) {$ctx1.fill(self,"add:",{anObject:anObject},smalltalk.CharacterArray)})},
messageSends: ["errorReadOnly"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "asLowercase",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._fromString_(_st(_st(self)._asString())._asLowercase());
return $1;
}, function($ctx1) {$ctx1.fill(self,"asLowercase",{},smalltalk.CharacterArray)})},
messageSends: ["fromString:", "asLowercase", "asString", "class"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "asNumber",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._asString())._asNumber();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asNumber",{},smalltalk.CharacterArray)})},
messageSends: ["asNumber", "asString"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "asString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asString",{},smalltalk.CharacterArray)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "asSymbol",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._asString();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asSymbol",{},smalltalk.CharacterArray)})},
messageSends: ["asString"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "asUppercase",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._fromString_(_st(_st(self)._asString())._asUppercase());
return $1;
}, function($ctx1) {$ctx1.fill(self,"asUppercase",{},smalltalk.CharacterArray)})},
messageSends: ["fromString:", "asUppercase", "asString", "class"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (anIndex,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._errorReadOnly();
return self}, function($ctx1) {$ctx1.fill(self,"at:put:",{anIndex:anIndex,anObject:anObject},smalltalk.CharacterArray)})},
messageSends: ["errorReadOnly"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "errorReadOnly",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._error_("Object is read-only");
return self}, function($ctx1) {$ctx1.fill(self,"errorReadOnly",{},smalltalk.CharacterArray)})},
messageSends: ["error:"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asString())._printOn_(aStream);
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.CharacterArray)})},
messageSends: ["printOn:", "asString"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "putOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aStream)._nextPutString_(self);
return self}, function($ctx1) {$ctx1.fill(self,"putOn:",{aStream:aStream},smalltalk.CharacterArray)})},
messageSends: ["nextPutString:"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "remove:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._errorReadOnly();
return self}, function($ctx1) {$ctx1.fill(self,"remove:",{anObject:anObject},smalltalk.CharacterArray)})},
messageSends: ["errorReadOnly"]}),
smalltalk.CharacterArray);


smalltalk.addMethod(
smalltalk.method({
selector: "fromString:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"fromString:",{aString:aString},smalltalk.CharacterArray.klass)})},
messageSends: ["subclassResponsibility"]}),
smalltalk.CharacterArray.klass);


smalltalk.addClass('String', smalltalk.CharacterArray, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: ",",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self + aString;
return self}, function($ctx1) {$ctx1.fill(self,",",{aString:aString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "<",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return String(self) < aString._asString();
return self}, function($ctx1) {$ctx1.fill(self,"<",{aString:aString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "<=",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return String(self) <= aString._asString();
return self}, function($ctx1) {$ctx1.fill(self,"<=",{aString:aString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(typeof aString === 'undefined') { return false }
		if(!aString._isString || ! aString._isString()) {
			return false;
		}
		return String(self) === String(aString)
	;
return self}, function($ctx1) {$ctx1.fill(self,"=",{aString:aString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "==",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self).__eq(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"==",{aString:aString},smalltalk.String)})},
messageSends: ["="]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: ">",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return String(self) > aString._asString();
return self}, function($ctx1) {$ctx1.fill(self,">",{aString:aString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: ">=",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return String(self) >= aString._asString();
return self}, function($ctx1) {$ctx1.fill(self,">=",{aString:aString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSON",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSON",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asJavascript",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		if(self.search(/^[a-zA-Z0-9_:.$ ]*$/) == -1)
			return "\"" + self.replace(/[\x00-\x1f"\\\x7f-\x9f]/g, function(ch){var c=ch.charCodeAt(0);return "\\x"+("0"+c.toString(16)).slice(-2)}) + "\"";
		else
			return "\"" + self + "\"";
	;
return self}, function($ctx1) {$ctx1.fill(self,"asJavascript",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asLowercase",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.toLowerCase();
return self}, function($ctx1) {$ctx1.fill(self,"asLowercase",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asNumber",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Number(self);
return self}, function($ctx1) {$ctx1.fill(self,"asNumber",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asRegexp",
fn: function (){
var self=this;
function $RegularExpression(){return smalltalk.RegularExpression||(typeof RegularExpression=="undefined"?nil:RegularExpression)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($RegularExpression())._fromString_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"asRegexp",{},smalltalk.String)})},
messageSends: ["fromString:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asSelector",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return smalltalk.selector(self);
return self}, function($ctx1) {$ctx1.fill(self,"asSelector",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asString",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asSymbol",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asSymbol",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asUppercase",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.toUpperCase();
return self}, function($ctx1) {$ctx1.fill(self,"asUppercase",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asciiValue",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.charCodeAt(0);;
return self}, function($ctx1) {$ctx1.fill(self,"asciiValue",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (anIndex,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return String(self).charAt(anIndex - 1) || aBlock();
return self}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{anIndex:anIndex,aBlock:aBlock},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "charCodeAt:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 return self.charCodeAt(anInteger - 1) ;
return self}, function($ctx1) {$ctx1.fill(self,"charCodeAt:",{anInteger:anInteger},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "copyFrom:to:",
fn: function (anIndex,anotherIndex){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.substring(anIndex - 1, anotherIndex);
return self}, function($ctx1) {$ctx1.fill(self,"copyFrom:to:",{anIndex:anIndex,anotherIndex:anotherIndex},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "crlfSanitized",
fn: function (){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._lines())._join_(_st($String())._lf());
return $1;
}, function($ctx1) {$ctx1.fill(self,"crlfSanitized",{},smalltalk.String)})},
messageSends: ["join:", "lf", "lines"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "deepCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._shallowCopy();
return $1;
}, function($ctx1) {$ctx1.fill(self,"deepCopy",{},smalltalk.String)})},
messageSends: ["shallowCopy"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "do:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
for(var i=0;i<self.length;i++){aBlock(self.charAt(i));};
return self}, function($ctx1) {$ctx1.fill(self,"do:",{aBlock:aBlock},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "escaped",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return escape(self);
return self}, function($ctx1) {$ctx1.fill(self,"escaped",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "includesSubString:",
fn: function (subString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 return self.indexOf(subString) != -1 ;
return self}, function($ctx1) {$ctx1.fill(self,"includesSubString:",{subString:subString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "isImmutable",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isImmutable",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "isString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return true;
}, function($ctx1) {$ctx1.fill(self,"isString",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "isVowel",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self)._size()).__eq((1)))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st("aeiou")._includes_(_st(self)._asLowercase());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"isVowel",{},smalltalk.String)})},
messageSends: ["and:", "includes:", "asLowercase", "=", "size"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "join:",
fn: function (aCollection){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($String())._streamContents_((function(stream){
return smalltalk.withContext(function($ctx2) {
return _st(aCollection)._do_separatedBy_((function(each){
return smalltalk.withContext(function($ctx3) {
return _st(stream)._nextPutAll_(_st(each)._asString());
}, function($ctx3) {$ctx3.fillBlock({each:each},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx3) {
return _st(stream)._nextPutAll_(self);
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
}, function($ctx2) {$ctx2.fillBlock({stream:stream},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"join:",{aCollection:aCollection},smalltalk.String)})},
messageSends: ["streamContents:", "do:separatedBy:", "nextPutAll:", "asString"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "lineIndicesDo:",
fn: function (aBlock){
var self=this;
var cr,lf,start,sz,nextLF,nextCR;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4;
var $early={};
try {
start=(1);
sz=_st(self)._size();
cr=_st($String())._cr();
nextCR=_st(self)._indexOf_startingAt_(cr,(1));
lf=_st($String())._lf();
nextLF=_st(self)._indexOf_startingAt_(lf,(1));
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(start).__lt_eq(sz);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileTrue_((function(){
return smalltalk.withContext(function($ctx2) {
$1=_st(_st(nextLF).__eq((0)))._and_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(nextCR).__eq((0));
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
if(smalltalk.assert($1)){
_st(aBlock)._value_value_value_(start,sz,sz);
$2=self;
throw $early=[$2];
};
$3=_st(_st(nextCR).__eq((0)))._or_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st((0)).__lt(nextLF))._and_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(nextLF).__lt(nextCR);
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
if(smalltalk.assert($3)){
_st(aBlock)._value_value_value_(start,_st(nextLF).__minus((1)),nextLF);
start=_st((1)).__plus(nextLF);
start;
nextLF=_st(self)._indexOf_startingAt_(lf,start);
return nextLF;
} else {
$4=_st(_st((1)).__plus(nextCR)).__eq(nextLF);
if(smalltalk.assert($4)){
_st(aBlock)._value_value_value_(start,_st(nextCR).__minus((1)),nextLF);
start=_st((1)).__plus(nextLF);
start;
nextCR=_st(self)._indexOf_startingAt_(cr,start);
nextCR;
nextLF=_st(self)._indexOf_startingAt_(lf,start);
return nextLF;
} else {
_st(aBlock)._value_value_value_(start,_st(nextCR).__minus((1)),nextCR);
start=_st((1)).__plus(nextCR);
start;
nextCR=_st(self)._indexOf_startingAt_(cr,start);
return nextCR;
};
};
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}
catch(e) {if(e===$early)return e[0]; throw e}
}, function($ctx1) {$ctx1.fill(self,"lineIndicesDo:",{aBlock:aBlock,cr:cr,lf:lf,start:start,sz:sz,nextLF:nextLF,nextCR:nextCR},smalltalk.String)})},
messageSends: ["size", "cr", "indexOf:startingAt:", "lf", "whileTrue:", "ifTrue:", "value:value:value:", "and:", "=", "ifTrue:ifFalse:", "-", "+", "or:", "<", "<="]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "lineNumber:",
fn: function (anIndex){
var self=this;
var lineCount;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
var $early={};
try {
lineCount=(0);
_st(self)._lineIndicesDo_((function(start,endWithoutDelimiters,end){
return smalltalk.withContext(function($ctx2) {
lineCount=_st(lineCount).__plus((1));
$1=_st(lineCount).__eq(anIndex);
if(smalltalk.assert($1)){
$2=_st(self)._copyFrom_to_(start,endWithoutDelimiters);
throw $early=[$2];
};
}, function($ctx2) {$ctx2.fillBlock({start:start,endWithoutDelimiters:endWithoutDelimiters,end:end},$ctx1)})}));
return nil;
}
catch(e) {if(e===$early)return e[0]; throw e}
}, function($ctx1) {$ctx1.fill(self,"lineNumber:",{anIndex:anIndex,lineCount:lineCount},smalltalk.String)})},
messageSends: ["lineIndicesDo:", "ifTrue:", "copyFrom:to:", "=", "+"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "lines",
fn: function (){
var self=this;
var lines;
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
var $1;
lines=_st($Array())._new();
_st(self)._linesDo_((function(aLine){
return smalltalk.withContext(function($ctx2) {
return _st(lines)._add_(aLine);
}, function($ctx2) {$ctx2.fillBlock({aLine:aLine},$ctx1)})}));
$1=lines;
return $1;
}, function($ctx1) {$ctx1.fill(self,"lines",{lines:lines},smalltalk.String)})},
messageSends: ["new", "linesDo:", "add:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "linesDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._lineIndicesDo_((function(start,endWithoutDelimiters,end){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_(_st(self)._copyFrom_to_(start,endWithoutDelimiters));
}, function($ctx2) {$ctx2.fillBlock({start:start,endWithoutDelimiters:endWithoutDelimiters,end:end},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"linesDo:",{aBlock:aBlock},smalltalk.String)})},
messageSends: ["lineIndicesDo:", "value:", "copyFrom:to:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "match:",
fn: function (aRegexp){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.search(aRegexp) != -1;
return self}, function($ctx1) {$ctx1.fill(self,"match:",{aRegexp:aRegexp},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "matchesOf:",
fn: function (aRegularExpression){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.match(aRegularExpression);
return self}, function($ctx1) {$ctx1.fill(self,"matchesOf:",{aRegularExpression:aRegularExpression},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "printNl",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
console.log(self);
return self}, function($ctx1) {$ctx1.fill(self,"printNl",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=aStream;
_st($1)._nextPutAll_("'");
_st($1)._nextPutAll_(self);
$2=_st($1)._nextPutAll_("'");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.String)})},
messageSends: ["nextPutAll:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "replace:with:",
fn: function (aString,anotherString){
var self=this;
function $RegularExpression(){return smalltalk.RegularExpression||(typeof RegularExpression=="undefined"?nil:RegularExpression)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._replaceRegexp_with_(_st($RegularExpression())._fromString_flag_(aString,"g"),anotherString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"replace:with:",{aString:aString,anotherString:anotherString},smalltalk.String)})},
messageSends: ["replaceRegexp:with:", "fromString:flag:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "replaceRegexp:with:",
fn: function (aRegexp,aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.replace(aRegexp, aString);
return self}, function($ctx1) {$ctx1.fill(self,"replaceRegexp:with:",{aRegexp:aRegexp,aString:aString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "reversed",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.split("").reverse().join("");
return self}, function($ctx1) {$ctx1.fill(self,"reversed",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "shallowCopy",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._fromString_(self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"shallowCopy",{},smalltalk.String)})},
messageSends: ["fromString:", "class"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "size",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.length;
return self}, function($ctx1) {$ctx1.fill(self,"size",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "tokenize:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.split(aString);
return self}, function($ctx1) {$ctx1.fill(self,"tokenize:",{aString:aString},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "trimBoth",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._trimBoth_("\x5cs");
return $1;
}, function($ctx1) {$ctx1.fill(self,"trimBoth",{},smalltalk.String)})},
messageSends: ["trimBoth:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "trimBoth:",
fn: function (separators){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._trimLeft_(separators))._trimRight_(separators);
return $1;
}, function($ctx1) {$ctx1.fill(self,"trimBoth:",{separators:separators},smalltalk.String)})},
messageSends: ["trimRight:", "trimLeft:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "trimLeft",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._trimLeft_("\x5cs");
return $1;
}, function($ctx1) {$ctx1.fill(self,"trimLeft",{},smalltalk.String)})},
messageSends: ["trimLeft:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "trimLeft:",
fn: function (separators){
var self=this;
function $RegularExpression(){return smalltalk.RegularExpression||(typeof RegularExpression=="undefined"?nil:RegularExpression)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._replaceRegexp_with_(_st($RegularExpression())._fromString_flag_(_st(_st("^[").__comma(separators)).__comma("]+"),"g"),"");
return $1;
}, function($ctx1) {$ctx1.fill(self,"trimLeft:",{separators:separators},smalltalk.String)})},
messageSends: ["replaceRegexp:with:", "fromString:flag:", ","]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "trimRight",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._trimRight_("\x5cs");
return $1;
}, function($ctx1) {$ctx1.fill(self,"trimRight",{},smalltalk.String)})},
messageSends: ["trimRight:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "trimRight:",
fn: function (separators){
var self=this;
function $RegularExpression(){return smalltalk.RegularExpression||(typeof RegularExpression=="undefined"?nil:RegularExpression)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._replaceRegexp_with_(_st($RegularExpression())._fromString_flag_(_st(_st("[").__comma(separators)).__comma("]+$"),"g"),"");
return $1;
}, function($ctx1) {$ctx1.fill(self,"trimRight:",{separators:separators},smalltalk.String)})},
messageSends: ["replaceRegexp:with:", "fromString:flag:", ","]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "unescaped",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return unescape(self);
return self}, function($ctx1) {$ctx1.fill(self,"unescaped",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "withIndexDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
for(var i=0;i<self.length;i++){aBlock(self.charAt(i), i+1);};
return self}, function($ctx1) {$ctx1.fill(self,"withIndexDo:",{aBlock:aBlock},smalltalk.String)})},
messageSends: []}),
smalltalk.String);


smalltalk.addMethod(
smalltalk.method({
selector: "cr",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return '\r';
return self}, function($ctx1) {$ctx1.fill(self,"cr",{},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "crlf",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return '\r\n';
return self}, function($ctx1) {$ctx1.fill(self,"crlf",{},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromCharCode:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return String.fromCharCode(anInteger);
return self}, function($ctx1) {$ctx1.fill(self,"fromCharCode:",{anInteger:anInteger},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromString:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return String(aString);
return self}, function($ctx1) {$ctx1.fill(self,"fromString:",{aString:aString},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "lf",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return '\n';
return self}, function($ctx1) {$ctx1.fill(self,"lf",{},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "random",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return (Math.random()*(22/32)+(10/32)).toString(32).slice(2);;
return self}, function($ctx1) {$ctx1.fill(self,"random",{},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "randomNotIn:",
fn: function (aString){
var self=this;
var result;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st((function(){
return smalltalk.withContext(function($ctx2) {
result=_st(self)._random();
result;
return _st(aString)._includesSubString_(result);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileTrue();
$1=result;
return $1;
}, function($ctx1) {$ctx1.fill(self,"randomNotIn:",{aString:aString,result:result},smalltalk.String.klass)})},
messageSends: ["whileTrue", "random", "includesSubString:"]}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "space",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return ' ';
return self}, function($ctx1) {$ctx1.fill(self,"space",{},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "streamClass",
fn: function (){
var self=this;
function $StringStream(){return smalltalk.StringStream||(typeof StringStream=="undefined"?nil:StringStream)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=$StringStream();
return $1;
}, function($ctx1) {$ctx1.fill(self,"streamClass",{},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "tab",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return '\t';
return self}, function($ctx1) {$ctx1.fill(self,"tab",{},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "value:",
fn: function (aUTFCharCode){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return String.fromCharCode(aUTFCharCode);;
return self}, function($ctx1) {$ctx1.fill(self,"value:",{aUTFCharCode:aUTFCharCode},smalltalk.String.klass)})},
messageSends: []}),
smalltalk.String.klass);


smalltalk.addClass('Set', smalltalk.Collection, ['elements'], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
var $early={};
try {
$1=_st(_st(self)._class()).__eq(_st(aCollection)._class());
if(! smalltalk.assert($1)){
return false;
};
$2=_st(_st(self)._size()).__eq(_st(aCollection)._size());
if(! smalltalk.assert($2)){
return false;
};
_st(self)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
$3=_st(aCollection)._includes_(each);
if(! smalltalk.assert($3)){
throw $early=[false];
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return true;
}
catch(e) {if(e===$early)return e[0]; throw e}
}, function($ctx1) {$ctx1.fill(self,"=",{aCollection:aCollection},smalltalk.Set)})},
messageSends: ["ifFalse:", "=", "class", "size", "do:", "includes:"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "add:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 

		var found;
		for(var i=0; i < self['@elements'].length; i++) {
			if(anObject == self['@elements'][i]) {
				found = true;
				break;
			}
		}
		if(!found) {self['@elements'].push(anObject)}
	;
return self}, function($ctx1) {$ctx1.fill(self,"add:",{anObject:anObject},smalltalk.Set)})},
messageSends: []}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "asArray",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@elements"])._copy();
return $1;
}, function($ctx1) {$ctx1.fill(self,"asArray",{},smalltalk.Set)})},
messageSends: ["copy"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "collect:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._withAll_(_st(self["@elements"])._collect_(aBlock));
return $1;
}, function($ctx1) {$ctx1.fill(self,"collect:",{aBlock:aBlock},smalltalk.Set)})},
messageSends: ["withAll:", "collect:", "class"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "detect:ifNone:",
fn: function (aBlock,anotherBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@elements"])._detect_ifNone_(aBlock,anotherBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"detect:ifNone:",{aBlock:aBlock,anotherBlock:anotherBlock},smalltalk.Set)})},
messageSends: ["detect:ifNone:"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "do:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@elements"])._do_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"do:",{aBlock:aBlock},smalltalk.Set)})},
messageSends: ["do:"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "includes:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@elements"])._includes_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"includes:",{anObject:anObject},smalltalk.Set)})},
messageSends: ["includes:"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.Collection.fn.prototype._initialize.apply(_st(self), []);
self["@elements"]=[];
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.Set)})},
messageSends: ["initialize"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "printOn:",
fn: function (aStream){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.Collection.fn.prototype._printOn_.apply(_st(self), [aStream]);
_st(aStream)._nextPutAll_(" (");
_st(self)._do_separatedBy_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._printOn_(aStream);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx2) {
return _st(aStream)._nextPutAll_(" ");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(aStream)._nextPutAll_(")");
return self}, function($ctx1) {$ctx1.fill(self,"printOn:",{aStream:aStream},smalltalk.Set)})},
messageSends: ["printOn:", "nextPutAll:", "do:separatedBy:"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "remove:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@elements"])._remove_(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"remove:",{anObject:anObject},smalltalk.Set)})},
messageSends: ["remove:"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "select:",
fn: function (aBlock){
var self=this;
var collection;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
collection=_st(_st(self)._class())._new();
_st(self)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
$1=_st(aBlock)._value_(each);
if(smalltalk.assert($1)){
return _st(collection)._add_(each);
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
$2=collection;
return $2;
}, function($ctx1) {$ctx1.fill(self,"select:",{aBlock:aBlock,collection:collection},smalltalk.Set)})},
messageSends: ["new", "class", "do:", "ifTrue:", "add:", "value:"]}),
smalltalk.Set);

smalltalk.addMethod(
smalltalk.method({
selector: "size",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@elements"])._size();
return $1;
}, function($ctx1) {$ctx1.fill(self,"size",{},smalltalk.Set)})},
messageSends: ["size"]}),
smalltalk.Set);



smalltalk.addClass('Queue', smalltalk.Object, ['read', 'readIndex', 'write'], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
function $OrderedCollection(){return smalltalk.OrderedCollection||(typeof OrderedCollection=="undefined"?nil:OrderedCollection)}
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
self["@read"]=_st($OrderedCollection())._new();
self["@write"]=_st($OrderedCollection())._new();
self["@readIndex"]=(1);
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.Queue)})},
messageSends: ["initialize", "new"]}),
smalltalk.Queue);

smalltalk.addMethod(
smalltalk.method({
selector: "next",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._nextIfAbsent_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._error_("Cannot read from empty Queue.");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"next",{},smalltalk.Queue)})},
messageSends: ["nextIfAbsent:", "error:"]}),
smalltalk.Queue);

smalltalk.addMethod(
smalltalk.method({
selector: "nextIfAbsent:",
fn: function (aBlock){
var self=this;
var result;
function $OrderedCollection(){return smalltalk.OrderedCollection||(typeof OrderedCollection=="undefined"?nil:OrderedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4;
var $early={};
try {
result=_st(self["@read"])._at_ifAbsent_(self["@readIndex"],(function(){
return smalltalk.withContext(function($ctx2) {
$1=_st(self["@write"])._isEmpty();
if(smalltalk.assert($1)){
$2=_st(self["@readIndex"]).__gt((1));
if(smalltalk.assert($2)){
self["@read"]=[];
self["@read"];
self["@readIndex"]=(1);
self["@readIndex"];
};
$3=_st(aBlock)._value();
throw $early=[$3];
};
self["@read"]=self["@write"];
self["@read"];
self["@readIndex"]=(1);
self["@readIndex"];
self["@write"]=_st($OrderedCollection())._new();
self["@write"];
return _st(self["@read"])._first();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self["@read"])._at_put_(self["@readIndex"],nil);
self["@readIndex"]=_st(self["@readIndex"]).__plus((1));
$4=result;
return $4;
}
catch(e) {if(e===$early)return e[0]; throw e}
}, function($ctx1) {$ctx1.fill(self,"nextIfAbsent:",{aBlock:aBlock,result:result},smalltalk.Queue)})},
messageSends: ["at:ifAbsent:", "ifTrue:", ">", "value", "isEmpty", "new", "first", "at:put:", "+"]}),
smalltalk.Queue);

smalltalk.addMethod(
smalltalk.method({
selector: "nextPut:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@write"])._add_(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"nextPut:",{anObject:anObject},smalltalk.Queue)})},
messageSends: ["add:"]}),
smalltalk.Queue);



smalltalk.addClass('RegularExpression', smalltalk.Object, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "compile:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.compile(aString);
return self}, function($ctx1) {$ctx1.fill(self,"compile:",{aString:aString},smalltalk.RegularExpression)})},
messageSends: []}),
smalltalk.RegularExpression);

smalltalk.addMethod(
smalltalk.method({
selector: "exec:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.exec(aString) || nil;
return self}, function($ctx1) {$ctx1.fill(self,"exec:",{aString:aString},smalltalk.RegularExpression)})},
messageSends: []}),
smalltalk.RegularExpression);

smalltalk.addMethod(
smalltalk.method({
selector: "test:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.test(aString);
return self}, function($ctx1) {$ctx1.fill(self,"test:",{aString:aString},smalltalk.RegularExpression)})},
messageSends: []}),
smalltalk.RegularExpression);


smalltalk.addMethod(
smalltalk.method({
selector: "fromString:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._fromString_flag_(aString,"");
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromString:",{aString:aString},smalltalk.RegularExpression.klass)})},
messageSends: ["fromString:flag:"]}),
smalltalk.RegularExpression.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromString:flag:",
fn: function (aString,anotherString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new RegExp(aString, anotherString);
return self}, function($ctx1) {$ctx1.fill(self,"fromString:flag:",{aString:aString,anotherString:anotherString},smalltalk.RegularExpression.klass)})},
messageSends: []}),
smalltalk.RegularExpression.klass);


smalltalk.addClass('Stream', smalltalk.Object, ['collection', 'position', 'streamSize'], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "<<",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._write_(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"<<",{anObject:anObject},smalltalk.Stream)})},
messageSends: ["write:"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "atEnd",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._position()).__eq(_st(self)._size());
return $1;
}, function($ctx1) {$ctx1.fill(self,"atEnd",{},smalltalk.Stream)})},
messageSends: ["=", "size", "position"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "atStart",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._position()).__eq((0));
return $1;
}, function($ctx1) {$ctx1.fill(self,"atStart",{},smalltalk.Stream)})},
messageSends: ["=", "position"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "close",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"close",{},smalltalk.Stream)})},
messageSends: []}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "collection",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@collection"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"collection",{},smalltalk.Stream)})},
messageSends: []}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "contents",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._collection())._copyFrom_to_((1),_st(self)._streamSize());
return $1;
}, function($ctx1) {$ctx1.fill(self,"contents",{},smalltalk.Stream)})},
messageSends: ["copyFrom:to:", "streamSize", "collection"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "do:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._atEnd();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._whileFalse_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_(_st(self)._next());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"do:",{aBlock:aBlock},smalltalk.Stream)})},
messageSends: ["whileFalse:", "value:", "next", "atEnd"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "flush",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"flush",{},smalltalk.Stream)})},
messageSends: []}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "isEmpty",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._size()).__eq((0));
return $1;
}, function($ctx1) {$ctx1.fill(self,"isEmpty",{},smalltalk.Stream)})},
messageSends: ["=", "size"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "next",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._atEnd();
if(smalltalk.assert($2)){
$1=nil;
} else {
_st(self)._position_(_st(_st(self)._position()).__plus((1)));
$1=_st(self["@collection"])._at_(_st(self)._position());
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"next",{},smalltalk.Stream)})},
messageSends: ["ifTrue:ifFalse:", "position:", "+", "position", "at:", "atEnd"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "next:",
fn: function (anInteger){
var self=this;
var tempCollection;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
tempCollection=_st(_st(_st(self)._collection())._class())._new();
_st(anInteger)._timesRepeat_((function(){
return smalltalk.withContext(function($ctx2) {
$1=_st(self)._atEnd();
if(! smalltalk.assert($1)){
return _st(tempCollection)._add_(_st(self)._next());
};
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=tempCollection;
return $2;
}, function($ctx1) {$ctx1.fill(self,"next:",{anInteger:anInteger,tempCollection:tempCollection},smalltalk.Stream)})},
messageSends: ["new", "class", "collection", "timesRepeat:", "ifFalse:", "add:", "next", "atEnd"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "nextPut:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._position_(_st(_st(self)._position()).__plus((1)));
_st(_st(self)._collection())._at_put_(_st(self)._position(),anObject);
_st(self)._setStreamSize_(_st(_st(self)._streamSize())._max_(_st(self)._position()));
return self}, function($ctx1) {$ctx1.fill(self,"nextPut:",{anObject:anObject},smalltalk.Stream)})},
messageSends: ["position:", "+", "position", "at:put:", "collection", "setStreamSize:", "max:", "streamSize"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "nextPutAll:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aCollection)._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._nextPut_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"nextPutAll:",{aCollection:aCollection},smalltalk.Stream)})},
messageSends: ["do:", "nextPut:"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "nextPutString:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._nextPut_(aString);
return self}, function($ctx1) {$ctx1.fill(self,"nextPutString:",{aString:aString},smalltalk.Stream)})},
messageSends: ["nextPut:"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "peek",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(self)._atEnd();
if(! smalltalk.assert($2)){
$1=_st(_st(self)._collection())._at_(_st(_st(self)._position()).__plus((1)));
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"peek",{},smalltalk.Stream)})},
messageSends: ["ifFalse:", "at:", "+", "position", "collection", "atEnd"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "position",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@position"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@position"]=(0);
$1=self["@position"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"position",{},smalltalk.Stream)})},
messageSends: ["ifNil:"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "position:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@position"]=anInteger;
return self}, function($ctx1) {$ctx1.fill(self,"position:",{anInteger:anInteger},smalltalk.Stream)})},
messageSends: []}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "reset",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._position_((0));
return self}, function($ctx1) {$ctx1.fill(self,"reset",{},smalltalk.Stream)})},
messageSends: ["position:"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "resetContents",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._reset();
_st(self)._setStreamSize_((0));
return self}, function($ctx1) {$ctx1.fill(self,"resetContents",{},smalltalk.Stream)})},
messageSends: ["reset", "setStreamSize:"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "setCollection:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@collection"]=aCollection;
return self}, function($ctx1) {$ctx1.fill(self,"setCollection:",{aCollection:aCollection},smalltalk.Stream)})},
messageSends: []}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "setStreamSize:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@streamSize"]=anInteger;
return self}, function($ctx1) {$ctx1.fill(self,"setStreamSize:",{anInteger:anInteger},smalltalk.Stream)})},
messageSends: []}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "setToEnd",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._position_(_st(self)._size());
return self}, function($ctx1) {$ctx1.fill(self,"setToEnd",{},smalltalk.Stream)})},
messageSends: ["position:", "size"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "size",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._streamSize();
return $1;
}, function($ctx1) {$ctx1.fill(self,"size",{},smalltalk.Stream)})},
messageSends: ["streamSize"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "skip:",
fn: function (anInteger){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._position_(_st(_st(_st(self)._position()).__plus(anInteger))._min_max_(_st(self)._size(),(0)));
return self}, function($ctx1) {$ctx1.fill(self,"skip:",{anInteger:anInteger},smalltalk.Stream)})},
messageSends: ["position:", "min:max:", "size", "+", "position"]}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "streamSize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@streamSize"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"streamSize",{},smalltalk.Stream)})},
messageSends: []}),
smalltalk.Stream);

smalltalk.addMethod(
smalltalk.method({
selector: "write:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(anObject)._putOn_(self);
return self}, function($ctx1) {$ctx1.fill(self,"write:",{anObject:anObject},smalltalk.Stream)})},
messageSends: ["putOn:"]}),
smalltalk.Stream);


smalltalk.addMethod(
smalltalk.method({
selector: "on:",
fn: function (aCollection){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._setCollection_(aCollection);
_st($2)._setStreamSize_(_st(aCollection)._size());
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:",{aCollection:aCollection},smalltalk.Stream.klass)})},
messageSends: ["setCollection:", "new", "setStreamSize:", "size", "yourself"]}),
smalltalk.Stream.klass);


smalltalk.addClass('StringStream', smalltalk.Stream, [], 'Kernel-Collections');
smalltalk.addMethod(
smalltalk.method({
selector: "cr",
fn: function (){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._nextPutAll_(_st($String())._cr());
return $1;
}, function($ctx1) {$ctx1.fill(self,"cr",{},smalltalk.StringStream)})},
messageSends: ["nextPutAll:", "cr"]}),
smalltalk.StringStream);

smalltalk.addMethod(
smalltalk.method({
selector: "crlf",
fn: function (){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._nextPutAll_(_st($String())._crlf());
return $1;
}, function($ctx1) {$ctx1.fill(self,"crlf",{},smalltalk.StringStream)})},
messageSends: ["nextPutAll:", "crlf"]}),
smalltalk.StringStream);

smalltalk.addMethod(
smalltalk.method({
selector: "lf",
fn: function (){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._nextPutAll_(_st($String())._lf());
return $1;
}, function($ctx1) {$ctx1.fill(self,"lf",{},smalltalk.StringStream)})},
messageSends: ["nextPutAll:", "lf"]}),
smalltalk.StringStream);

smalltalk.addMethod(
smalltalk.method({
selector: "next:",
fn: function (anInteger){
var self=this;
var tempCollection;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
tempCollection=_st(_st(_st(self)._collection())._class())._new();
_st(anInteger)._timesRepeat_((function(){
return smalltalk.withContext(function($ctx2) {
$1=_st(self)._atEnd();
if(! smalltalk.assert($1)){
tempCollection=_st(tempCollection).__comma(_st(self)._next());
return tempCollection;
};
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=tempCollection;
return $2;
}, function($ctx1) {$ctx1.fill(self,"next:",{anInteger:anInteger,tempCollection:tempCollection},smalltalk.StringStream)})},
messageSends: ["new", "class", "collection", "timesRepeat:", "ifFalse:", ",", "next", "atEnd"]}),
smalltalk.StringStream);

smalltalk.addMethod(
smalltalk.method({
selector: "nextPut:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._nextPutAll_(aString);
return self}, function($ctx1) {$ctx1.fill(self,"nextPut:",{aString:aString},smalltalk.StringStream)})},
messageSends: ["nextPutAll:"]}),
smalltalk.StringStream);

smalltalk.addMethod(
smalltalk.method({
selector: "nextPutAll:",
fn: function (aString){
var self=this;
var pre,post;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._atEnd();
if(smalltalk.assert($1)){
_st(self)._setCollection_(_st(_st(self)._collection()).__comma(aString));
} else {
pre=_st(_st(self)._collection())._copyFrom_to_((1),_st(self)._position());
pre;
post=_st(_st(self)._collection())._copyFrom_to_(_st(_st(_st(self)._position()).__plus((1))).__plus(_st(aString)._size()),_st(_st(self)._collection())._size());
post;
_st(self)._setCollection_(_st(_st(pre).__comma(aString)).__comma(post));
};
_st(self)._position_(_st(_st(self)._position()).__plus(_st(aString)._size()));
_st(self)._setStreamSize_(_st(_st(self)._streamSize())._max_(_st(self)._position()));
return self}, function($ctx1) {$ctx1.fill(self,"nextPutAll:",{aString:aString,pre:pre,post:post},smalltalk.StringStream)})},
messageSends: ["ifTrue:ifFalse:", "setCollection:", ",", "collection", "copyFrom:to:", "position", "+", "size", "atEnd", "position:", "setStreamSize:", "max:", "streamSize"]}),
smalltalk.StringStream);

smalltalk.addMethod(
smalltalk.method({
selector: "nextPutString:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._nextPutAll_(aString);
return self}, function($ctx1) {$ctx1.fill(self,"nextPutString:",{aString:aString},smalltalk.StringStream)})},
messageSends: ["nextPutAll:"]}),
smalltalk.StringStream);

smalltalk.addMethod(
smalltalk.method({
selector: "space",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._nextPut_(" ");
return self}, function($ctx1) {$ctx1.fill(self,"space",{},smalltalk.StringStream)})},
messageSends: ["nextPut:"]}),
smalltalk.StringStream);

smalltalk.addMethod(
smalltalk.method({
selector: "tab",
fn: function (){
var self=this;
function $String(){return smalltalk.String||(typeof String=="undefined"?nil:String)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._nextPutAll_(_st($String())._tab());
return $1;
}, function($ctx1) {$ctx1.fill(self,"tab",{},smalltalk.StringStream)})},
messageSends: ["nextPutAll:", "tab"]}),
smalltalk.StringStream);



smalltalk.addPackage('Kernel-Announcements');
smalltalk.addClass('AnnouncementSubscription', smalltalk.Object, ['block', 'announcementClass'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "announcementClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@announcementClass"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"announcementClass",{},smalltalk.AnnouncementSubscription)})},
messageSends: []}),
smalltalk.AnnouncementSubscription);

smalltalk.addMethod(
smalltalk.method({
selector: "announcementClass:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@announcementClass"]=aClass;
return self}, function($ctx1) {$ctx1.fill(self,"announcementClass:",{aClass:aClass},smalltalk.AnnouncementSubscription)})},
messageSends: []}),
smalltalk.AnnouncementSubscription);

smalltalk.addMethod(
smalltalk.method({
selector: "block",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@block"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"block",{},smalltalk.AnnouncementSubscription)})},
messageSends: []}),
smalltalk.AnnouncementSubscription);

smalltalk.addMethod(
smalltalk.method({
selector: "block:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@block"]=aBlock;
return self}, function($ctx1) {$ctx1.fill(self,"block:",{aBlock:aBlock},smalltalk.AnnouncementSubscription)})},
messageSends: []}),
smalltalk.AnnouncementSubscription);

smalltalk.addMethod(
smalltalk.method({
selector: "deliver:",
fn: function (anAnnouncement){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._handlesAnnouncement_(anAnnouncement);
if(smalltalk.assert($1)){
_st(_st(self)._block())._value_(anAnnouncement);
};
return self}, function($ctx1) {$ctx1.fill(self,"deliver:",{anAnnouncement:anAnnouncement},smalltalk.AnnouncementSubscription)})},
messageSends: ["ifTrue:", "value:", "block", "handlesAnnouncement:"]}),
smalltalk.AnnouncementSubscription);

smalltalk.addMethod(
smalltalk.method({
selector: "handlesAnnouncement:",
fn: function (anAnnouncement){
var self=this;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=_st(_st($Smalltalk())._current())._at_(_st(_st(self)._announcementClass())._name());
if(($receiver = $2) == nil || $receiver == undefined){
return false;
} else {
var class_;
class_=$receiver;
$1=_st(class_)._includesBehavior_(_st(_st($Smalltalk())._current())._at_(_st(_st(_st(anAnnouncement)._class())._theNonMetaClass())._name()));
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"handlesAnnouncement:",{anAnnouncement:anAnnouncement},smalltalk.AnnouncementSubscription)})},
messageSends: ["ifNil:ifNotNil:", "includesBehavior:", "at:", "name", "theNonMetaClass", "class", "current", "announcementClass"]}),
smalltalk.AnnouncementSubscription);



smalltalk.addClass('Announcer', smalltalk.Object, ['registry', 'subscriptions'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "announce:",
fn: function (anAnnouncement){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@subscriptions"])._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(each)._deliver_(anAnnouncement);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"announce:",{anAnnouncement:anAnnouncement},smalltalk.Announcer)})},
messageSends: ["do:", "deliver:"]}),
smalltalk.Announcer);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
self["@subscriptions"]=_st($Array())._new();
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.Announcer)})},
messageSends: ["initialize", "new"]}),
smalltalk.Announcer);

smalltalk.addMethod(
smalltalk.method({
selector: "on:do:",
fn: function (aClass,aBlock){
var self=this;
function $AnnouncementSubscription(){return smalltalk.AnnouncementSubscription||(typeof AnnouncementSubscription=="undefined"?nil:AnnouncementSubscription)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st($AnnouncementSubscription())._new();
_st($1)._block_(aBlock);
_st($1)._announcementClass_(aClass);
$2=_st($1)._yourself();
_st(self["@subscriptions"])._add_($2);
return self}, function($ctx1) {$ctx1.fill(self,"on:do:",{aClass:aClass,aBlock:aBlock},smalltalk.Announcer)})},
messageSends: ["add:", "block:", "new", "announcementClass:", "yourself"]}),
smalltalk.Announcer);



smalltalk.addClass('SystemAnnouncer', smalltalk.Announcer, [], 'Kernel-Announcements');

smalltalk.SystemAnnouncer.klass.iVarNames = ['current'];
smalltalk.addMethod(
smalltalk.method({
selector: "current",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@current"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@current"]=smalltalk.Announcer.klass.fn.prototype._new.apply(_st(self), []);
$1=self["@current"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"current",{},smalltalk.SystemAnnouncer.klass)})},
messageSends: ["ifNil:", "new"]}),
smalltalk.SystemAnnouncer.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "new",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._shouldNotImplement();
return self}, function($ctx1) {$ctx1.fill(self,"new",{},smalltalk.SystemAnnouncer.klass)})},
messageSends: ["shouldNotImplement"]}),
smalltalk.SystemAnnouncer.klass);


smalltalk.addClass('SystemAnnouncement', smalltalk.Object, ['theClass'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "theClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@theClass"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"theClass",{},smalltalk.SystemAnnouncement)})},
messageSends: []}),
smalltalk.SystemAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "theClass:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@theClass"]=aClass;
return self}, function($ctx1) {$ctx1.fill(self,"theClass:",{aClass:aClass},smalltalk.SystemAnnouncement)})},
messageSends: []}),
smalltalk.SystemAnnouncement);



smalltalk.addClass('ClassAnnouncement', smalltalk.SystemAnnouncement, ['theClass'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "theClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@theClass"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"theClass",{},smalltalk.ClassAnnouncement)})},
messageSends: []}),
smalltalk.ClassAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "theClass:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@theClass"]=aClass;
return self}, function($ctx1) {$ctx1.fill(self,"theClass:",{aClass:aClass},smalltalk.ClassAnnouncement)})},
messageSends: []}),
smalltalk.ClassAnnouncement);



smalltalk.addClass('ClassAdded', smalltalk.ClassAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('ClassCommentChanged', smalltalk.ClassAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('ClassDefinitionChanged', smalltalk.ClassAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('ClassMoved', smalltalk.ClassAnnouncement, ['oldPackage'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "oldPackage",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@oldPackage"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"oldPackage",{},smalltalk.ClassMoved)})},
messageSends: []}),
smalltalk.ClassMoved);

smalltalk.addMethod(
smalltalk.method({
selector: "oldPackage:",
fn: function (aPackage){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@oldPackage"]=aPackage;
return self}, function($ctx1) {$ctx1.fill(self,"oldPackage:",{aPackage:aPackage},smalltalk.ClassMoved)})},
messageSends: []}),
smalltalk.ClassMoved);



smalltalk.addClass('ClassRemoved', smalltalk.ClassAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('ClassRenamed', smalltalk.ClassAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('MethodAnnouncement', smalltalk.SystemAnnouncement, ['method'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "method",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@method"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"method",{},smalltalk.MethodAnnouncement)})},
messageSends: []}),
smalltalk.MethodAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "method:",
fn: function (aCompiledMethod){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@method"]=aCompiledMethod;
return self}, function($ctx1) {$ctx1.fill(self,"method:",{aCompiledMethod:aCompiledMethod},smalltalk.MethodAnnouncement)})},
messageSends: []}),
smalltalk.MethodAnnouncement);



smalltalk.addClass('MethodAdded', smalltalk.MethodAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('MethodModified', smalltalk.MethodAnnouncement, ['oldMethod'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "oldMethod",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@oldMethod"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"oldMethod",{},smalltalk.MethodModified)})},
messageSends: []}),
smalltalk.MethodModified);

smalltalk.addMethod(
smalltalk.method({
selector: "oldMethod:",
fn: function (aMethod){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@oldMethod"]=aMethod;
return self}, function($ctx1) {$ctx1.fill(self,"oldMethod:",{aMethod:aMethod},smalltalk.MethodModified)})},
messageSends: []}),
smalltalk.MethodModified);



smalltalk.addClass('MethodMoved', smalltalk.MethodAnnouncement, ['oldProtocol'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "oldProtocol",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@oldProtocol"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"oldProtocol",{},smalltalk.MethodMoved)})},
messageSends: []}),
smalltalk.MethodMoved);

smalltalk.addMethod(
smalltalk.method({
selector: "oldProtocol:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@oldProtocol"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"oldProtocol:",{aString:aString},smalltalk.MethodMoved)})},
messageSends: []}),
smalltalk.MethodMoved);



smalltalk.addClass('MethodRemoved', smalltalk.MethodAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('PackageAnnouncement', smalltalk.SystemAnnouncement, ['package'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "package",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@package"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"package",{},smalltalk.PackageAnnouncement)})},
messageSends: []}),
smalltalk.PackageAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "package:",
fn: function (aPackage){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@package"]=aPackage;
return self}, function($ctx1) {$ctx1.fill(self,"package:",{aPackage:aPackage},smalltalk.PackageAnnouncement)})},
messageSends: []}),
smalltalk.PackageAnnouncement);



smalltalk.addClass('PackageAdded', smalltalk.PackageAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('PackageRemoved', smalltalk.PackageAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('ProtocolAnnouncement', smalltalk.SystemAnnouncement, ['theClass', 'protocol'], 'Kernel-Announcements');
smalltalk.addMethod(
smalltalk.method({
selector: "protocol",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@protocol"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"protocol",{},smalltalk.ProtocolAnnouncement)})},
messageSends: []}),
smalltalk.ProtocolAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "protocol:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@protocol"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"protocol:",{aString:aString},smalltalk.ProtocolAnnouncement)})},
messageSends: []}),
smalltalk.ProtocolAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "theClass",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@theClass"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"theClass",{},smalltalk.ProtocolAnnouncement)})},
messageSends: []}),
smalltalk.ProtocolAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "theClass:",
fn: function (aClass){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@theClass"]=aClass;
return self}, function($ctx1) {$ctx1.fill(self,"theClass:",{aClass:aClass},smalltalk.ProtocolAnnouncement)})},
messageSends: []}),
smalltalk.ProtocolAnnouncement);



smalltalk.addClass('ProtocolAdded', smalltalk.ProtocolAnnouncement, [], 'Kernel-Announcements');


smalltalk.addClass('ProtocolRemoved', smalltalk.ProtocolAnnouncement, [], 'Kernel-Announcements');


smalltalk.addPackage('Kernel-Exceptions');
smalltalk.addClass('Error', smalltalk.Object, ['messageText'], 'Kernel-Exceptions');
smalltalk.addMethod(
smalltalk.method({
selector: "context",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.context;
return self}, function($ctx1) {$ctx1.fill(self,"context",{},smalltalk.Error)})},
messageSends: []}),
smalltalk.Error);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._messageText_(_st("Errorclass: ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.Error)})},
messageSends: ["messageText:", ",", "name", "class"]}),
smalltalk.Error);

smalltalk.addMethod(
smalltalk.method({
selector: "isSmalltalkError",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.smalltalkError === true;
return self}, function($ctx1) {$ctx1.fill(self,"isSmalltalkError",{},smalltalk.Error)})},
messageSends: []}),
smalltalk.Error);

smalltalk.addMethod(
smalltalk.method({
selector: "jsStack",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self.stack;
return self}, function($ctx1) {$ctx1.fill(self,"jsStack",{},smalltalk.Error)})},
messageSends: []}),
smalltalk.Error);

smalltalk.addMethod(
smalltalk.method({
selector: "messageText",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@messageText"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"messageText",{},smalltalk.Error)})},
messageSends: []}),
smalltalk.Error);

smalltalk.addMethod(
smalltalk.method({
selector: "messageText:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@messageText"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"messageText:",{aString:aString},smalltalk.Error)})},
messageSends: []}),
smalltalk.Error);

smalltalk.addMethod(
smalltalk.method({
selector: "signal",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.context = smalltalk.getThisContext(); self.smalltalkError = true; throw(self);
return self}, function($ctx1) {$ctx1.fill(self,"signal",{},smalltalk.Error)})},
messageSends: []}),
smalltalk.Error);

smalltalk.addMethod(
smalltalk.method({
selector: "signal:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._messageText_(aString);
_st(self)._signal();
return self}, function($ctx1) {$ctx1.fill(self,"signal:",{aString:aString},smalltalk.Error)})},
messageSends: ["messageText:", "signal"]}),
smalltalk.Error);


smalltalk.addMethod(
smalltalk.method({
selector: "signal",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._signal();
return $1;
}, function($ctx1) {$ctx1.fill(self,"signal",{},smalltalk.Error.klass)})},
messageSends: ["signal", "new"]}),
smalltalk.Error.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "signal:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._signal_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"signal:",{aString:aString},smalltalk.Error.klass)})},
messageSends: ["signal:", "new"]}),
smalltalk.Error.klass);


smalltalk.addClass('JavaScriptException', smalltalk.Error, ['exception'], 'Kernel-Exceptions');
smalltalk.addMethod(
smalltalk.method({
selector: "context:",
fn: function (aMethodContext){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self.context = aMethodContext;
return self}, function($ctx1) {$ctx1.fill(self,"context:",{aMethodContext:aMethodContext},smalltalk.JavaScriptException)})},
messageSends: []}),
smalltalk.JavaScriptException);

smalltalk.addMethod(
smalltalk.method({
selector: "exception",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@exception"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"exception",{},smalltalk.JavaScriptException)})},
messageSends: []}),
smalltalk.JavaScriptException);

smalltalk.addMethod(
smalltalk.method({
selector: "exception:",
fn: function (anException){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@exception"]=anException;
return self}, function($ctx1) {$ctx1.fill(self,"exception:",{anException:anException},smalltalk.JavaScriptException)})},
messageSends: []}),
smalltalk.JavaScriptException);

smalltalk.addMethod(
smalltalk.method({
selector: "messageText",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return 'JavaScript exception: ' + self["@exception"].toString();
return self}, function($ctx1) {$ctx1.fill(self,"messageText",{},smalltalk.JavaScriptException)})},
messageSends: []}),
smalltalk.JavaScriptException);


smalltalk.addMethod(
smalltalk.method({
selector: "on:",
fn: function (anException){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._exception_(anException);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:",{anException:anException},smalltalk.JavaScriptException.klass)})},
messageSends: ["exception:", "new", "yourself"]}),
smalltalk.JavaScriptException.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "on:context:",
fn: function (anException,aMethodContext){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._exception_(anException);
_st($2)._context_(aMethodContext);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:context:",{anException:anException,aMethodContext:aMethodContext},smalltalk.JavaScriptException.klass)})},
messageSends: ["exception:", "new", "context:", "yourself"]}),
smalltalk.JavaScriptException.klass);


smalltalk.addClass('MessageNotUnderstood', smalltalk.Error, ['message', 'receiver'], 'Kernel-Exceptions');
smalltalk.addMethod(
smalltalk.method({
selector: "message",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@message"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"message",{},smalltalk.MessageNotUnderstood)})},
messageSends: []}),
smalltalk.MessageNotUnderstood);

smalltalk.addMethod(
smalltalk.method({
selector: "message:",
fn: function (aMessage){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@message"]=aMessage;
return self}, function($ctx1) {$ctx1.fill(self,"message:",{aMessage:aMessage},smalltalk.MessageNotUnderstood)})},
messageSends: []}),
smalltalk.MessageNotUnderstood);

smalltalk.addMethod(
smalltalk.method({
selector: "messageText",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(_st(self)._receiver())._asString()).__comma(" does not understand #")).__comma(_st(_st(self)._message())._selector());
return $1;
}, function($ctx1) {$ctx1.fill(self,"messageText",{},smalltalk.MessageNotUnderstood)})},
messageSends: [",", "selector", "message", "asString", "receiver"]}),
smalltalk.MessageNotUnderstood);

smalltalk.addMethod(
smalltalk.method({
selector: "receiver",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@receiver"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"receiver",{},smalltalk.MessageNotUnderstood)})},
messageSends: []}),
smalltalk.MessageNotUnderstood);

smalltalk.addMethod(
smalltalk.method({
selector: "receiver:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@receiver"]=anObject;
return self}, function($ctx1) {$ctx1.fill(self,"receiver:",{anObject:anObject},smalltalk.MessageNotUnderstood)})},
messageSends: []}),
smalltalk.MessageNotUnderstood);



smalltalk.addClass('NonBooleanReceiver', smalltalk.Error, ['object'], 'Kernel-Exceptions');
smalltalk.addMethod(
smalltalk.method({
selector: "object",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@object"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"object",{},smalltalk.NonBooleanReceiver)})},
messageSends: []}),
smalltalk.NonBooleanReceiver);

smalltalk.addMethod(
smalltalk.method({
selector: "object:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@object"]=anObject;
return self}, function($ctx1) {$ctx1.fill(self,"object:",{anObject:anObject},smalltalk.NonBooleanReceiver)})},
messageSends: []}),
smalltalk.NonBooleanReceiver);



smalltalk.addClass('ErrorHandler', smalltalk.Object, [], 'Kernel-Exceptions');
smalltalk.addMethod(
smalltalk.method({
selector: "handleError:",
fn: function (anError){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(anError)._context();
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(self)._logErrorContext_(_st(anError)._context());
};
_st(self)._logError_(anError);
return self}, function($ctx1) {$ctx1.fill(self,"handleError:",{anError:anError},smalltalk.ErrorHandler)})},
messageSends: ["ifNotNil:", "logErrorContext:", "context", "logError:"]}),
smalltalk.ErrorHandler);

smalltalk.addMethod(
smalltalk.method({
selector: "log:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(console)._log_(aString);
return self}, function($ctx1) {$ctx1.fill(self,"log:",{aString:aString},smalltalk.ErrorHandler)})},
messageSends: ["log:"]}),
smalltalk.ErrorHandler);

smalltalk.addMethod(
smalltalk.method({
selector: "logContext:",
fn: function (aContext){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(aContext)._home();
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(self)._logContext_(_st(aContext)._home());
};
_st(self)._log_(_st(_st(_st(_st(aContext)._receiver())._asString()).__comma(">>")).__comma(_st(_st(aContext)._selector())._asString()));
return self}, function($ctx1) {$ctx1.fill(self,"logContext:",{aContext:aContext},smalltalk.ErrorHandler)})},
messageSends: ["ifNotNil:", "logContext:", "home", "log:", ",", "asString", "selector", "receiver"]}),
smalltalk.ErrorHandler);

smalltalk.addMethod(
smalltalk.method({
selector: "logError:",
fn: function (anError){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._log_(_st(anError)._messageText());
return self}, function($ctx1) {$ctx1.fill(self,"logError:",{anError:anError},smalltalk.ErrorHandler)})},
messageSends: ["log:", "messageText"]}),
smalltalk.ErrorHandler);

smalltalk.addMethod(
smalltalk.method({
selector: "logErrorContext:",
fn: function (aContext){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=aContext;
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
$2=_st(aContext)._home();
if(($receiver = $2) == nil || $receiver == undefined){
$2;
} else {
_st(self)._logContext_(_st(aContext)._home());
};
};
return self}, function($ctx1) {$ctx1.fill(self,"logErrorContext:",{aContext:aContext},smalltalk.ErrorHandler)})},
messageSends: ["ifNotNil:", "logContext:", "home"]}),
smalltalk.ErrorHandler);


smalltalk.ErrorHandler.klass.iVarNames = ['current'];
smalltalk.addMethod(
smalltalk.method({
selector: "current",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@current"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@current"]=_st(self)._new();
$1=self["@current"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"current",{},smalltalk.ErrorHandler.klass)})},
messageSends: ["ifNil:", "new"]}),
smalltalk.ErrorHandler.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._register();
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.ErrorHandler.klass)})},
messageSends: ["register"]}),
smalltalk.ErrorHandler.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "register",
fn: function (){
var self=this;
function $ErrorHandler(){return smalltalk.ErrorHandler||(typeof ErrorHandler=="undefined"?nil:ErrorHandler)}
return smalltalk.withContext(function($ctx1) { 
_st($ErrorHandler())._setCurrent_(_st(self)._new());
return self}, function($ctx1) {$ctx1.fill(self,"register",{},smalltalk.ErrorHandler.klass)})},
messageSends: ["setCurrent:", "new"]}),
smalltalk.ErrorHandler.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "setCurrent:",
fn: function (anHandler){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@current"]=anHandler;
return self}, function($ctx1) {$ctx1.fill(self,"setCurrent:",{anHandler:anHandler},smalltalk.ErrorHandler.klass)})},
messageSends: []}),
smalltalk.ErrorHandler.klass);


smalltalk.addPackage('Canvas');
smalltalk.addClass('HTMLCanvas', smalltalk.Object, ['root'], 'Canvas');
smalltalk.addMethod(
smalltalk.method({
selector: "a",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("a");
return $1;
}, function($ctx1) {$ctx1.fill(self,"a",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "abbr",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("abbr");
return $1;
}, function($ctx1) {$ctx1.fill(self,"abbr",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "address",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("address");
return $1;
}, function($ctx1) {$ctx1.fill(self,"address",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "area",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("area");
return $1;
}, function($ctx1) {$ctx1.fill(self,"area",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "article",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("article");
return $1;
}, function($ctx1) {$ctx1.fill(self,"article",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "aside",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("aside");
return $1;
}, function($ctx1) {$ctx1.fill(self,"aside",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "audio",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("audio");
return $1;
}, function($ctx1) {$ctx1.fill(self,"audio",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "base",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("base");
return $1;
}, function($ctx1) {$ctx1.fill(self,"base",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "blockquote",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("blockquote");
return $1;
}, function($ctx1) {$ctx1.fill(self,"blockquote",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "body",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("body");
return $1;
}, function($ctx1) {$ctx1.fill(self,"body",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "br",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("br");
return $1;
}, function($ctx1) {$ctx1.fill(self,"br",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "button",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("button");
return $1;
}, function($ctx1) {$ctx1.fill(self,"button",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "canvas",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("canvas");
return $1;
}, function($ctx1) {$ctx1.fill(self,"canvas",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "caption",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("caption");
return $1;
}, function($ctx1) {$ctx1.fill(self,"caption",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "cite",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("cite");
return $1;
}, function($ctx1) {$ctx1.fill(self,"cite",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "code",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("code");
return $1;
}, function($ctx1) {$ctx1.fill(self,"code",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "col",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("col");
return $1;
}, function($ctx1) {$ctx1.fill(self,"col",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "colgroup",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("colgroup");
return $1;
}, function($ctx1) {$ctx1.fill(self,"colgroup",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "command",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("command");
return $1;
}, function($ctx1) {$ctx1.fill(self,"command",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "datalist",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("datalist");
return $1;
}, function($ctx1) {$ctx1.fill(self,"datalist",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "dd",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("dd");
return $1;
}, function($ctx1) {$ctx1.fill(self,"dd",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "del",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("del");
return $1;
}, function($ctx1) {$ctx1.fill(self,"del",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "details",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("details");
return $1;
}, function($ctx1) {$ctx1.fill(self,"details",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "div",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("div");
return $1;
}, function($ctx1) {$ctx1.fill(self,"div",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "div:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._div())._with_(aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"div:",{aBlock:aBlock},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "div"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "dl",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("dl");
return $1;
}, function($ctx1) {$ctx1.fill(self,"dl",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "dt",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("dt");
return $1;
}, function($ctx1) {$ctx1.fill(self,"dt",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "em",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("em");
return $1;
}, function($ctx1) {$ctx1.fill(self,"em",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "embed",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("embed");
return $1;
}, function($ctx1) {$ctx1.fill(self,"embed",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "entity:",
fn: function (aString){
var self=this;
var result;
return smalltalk.withContext(function($ctx1) { 
var $1;
result=_st(_st(_st("<span />")._asJQuery())._html_(_st(_st("&").__comma(aString)).__comma(";")))._text();
$1=_st(_st(result)._size()).__eq((1));
if(! smalltalk.assert($1)){
_st(self)._error_(_st("Not an HTML entity: ").__comma(aString));
};
_st(self)._with_(result);
return self}, function($ctx1) {$ctx1.fill(self,"entity:",{aString:aString,result:result},smalltalk.HTMLCanvas)})},
messageSends: ["text", "html:", ",", "asJQuery", "ifFalse:", "error:", "=", "size", "with:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "fieldset",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("fieldset");
return $1;
}, function($ctx1) {$ctx1.fill(self,"fieldset",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "figcaption",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("figcaption");
return $1;
}, function($ctx1) {$ctx1.fill(self,"figcaption",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "figure",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("figure");
return $1;
}, function($ctx1) {$ctx1.fill(self,"figure",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "footer",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("footer");
return $1;
}, function($ctx1) {$ctx1.fill(self,"footer",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "form",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("form");
return $1;
}, function($ctx1) {$ctx1.fill(self,"form",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h1",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("h1");
return $1;
}, function($ctx1) {$ctx1.fill(self,"h1",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h1:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._h1())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"h1:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "h1"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h2",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("h2");
return $1;
}, function($ctx1) {$ctx1.fill(self,"h2",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h2:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._h2())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"h2:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "h2"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h3",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("h3");
return $1;
}, function($ctx1) {$ctx1.fill(self,"h3",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h3:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._h3())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"h3:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "h3"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h4",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("h4");
return $1;
}, function($ctx1) {$ctx1.fill(self,"h4",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h4:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._h4())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"h4:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "h4"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h5",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("h5");
return $1;
}, function($ctx1) {$ctx1.fill(self,"h5",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h5:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._h5())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"h5:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "h5"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h6",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("h6");
return $1;
}, function($ctx1) {$ctx1.fill(self,"h6",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "h6:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._h6())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"h6:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "h6"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "head",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("head");
return $1;
}, function($ctx1) {$ctx1.fill(self,"head",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "header",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("header");
return $1;
}, function($ctx1) {$ctx1.fill(self,"header",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "hgroup",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("hgroup");
return $1;
}, function($ctx1) {$ctx1.fill(self,"hgroup",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "hr",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("hr");
return $1;
}, function($ctx1) {$ctx1.fill(self,"hr",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "html",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("html");
return $1;
}, function($ctx1) {$ctx1.fill(self,"html",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "iframe",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("iframe");
return $1;
}, function($ctx1) {$ctx1.fill(self,"iframe",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "iframe:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._iframe())._src_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"iframe:",{aString:aString},smalltalk.HTMLCanvas)})},
messageSends: ["src:", "iframe"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "img",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("img");
return $1;
}, function($ctx1) {$ctx1.fill(self,"img",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "img:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._img())._src_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"img:",{aString:aString},smalltalk.HTMLCanvas)})},
messageSends: ["src:", "img"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
function $TagBrush(){return smalltalk.TagBrush||(typeof TagBrush=="undefined"?nil:TagBrush)}
return smalltalk.withContext(function($ctx1) { 
var $1;
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
$1=self["@root"];
if(($receiver = $1) == nil || $receiver == undefined){
self["@root"]=_st($TagBrush())._fromString_canvas_("div",self);
self["@root"];
} else {
$1;
};
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.HTMLCanvas)})},
messageSends: ["initialize", "ifNil:", "fromString:canvas:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeFromJQuery:",
fn: function (aJQuery){
var self=this;
function $TagBrush(){return smalltalk.TagBrush||(typeof TagBrush=="undefined"?nil:TagBrush)}
return smalltalk.withContext(function($ctx1) { 
self["@root"]=_st($TagBrush())._fromJQuery_canvas_(aJQuery,self);
return self}, function($ctx1) {$ctx1.fill(self,"initializeFromJQuery:",{aJQuery:aJQuery},smalltalk.HTMLCanvas)})},
messageSends: ["fromJQuery:canvas:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "input",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("input");
return $1;
}, function($ctx1) {$ctx1.fill(self,"input",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "label",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("label");
return $1;
}, function($ctx1) {$ctx1.fill(self,"label",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "legend",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("legend");
return $1;
}, function($ctx1) {$ctx1.fill(self,"legend",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "li",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("li");
return $1;
}, function($ctx1) {$ctx1.fill(self,"li",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "li:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._li())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"li:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "li"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "link",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("link");
return $1;
}, function($ctx1) {$ctx1.fill(self,"link",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "map",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("map");
return $1;
}, function($ctx1) {$ctx1.fill(self,"map",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "mark",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("mark");
return $1;
}, function($ctx1) {$ctx1.fill(self,"mark",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "menu",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("menu");
return $1;
}, function($ctx1) {$ctx1.fill(self,"menu",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "meta",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("meta");
return $1;
}, function($ctx1) {$ctx1.fill(self,"meta",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "nav",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("nav");
return $1;
}, function($ctx1) {$ctx1.fill(self,"nav",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "newTag:",
fn: function (aString){
var self=this;
function $TagBrush(){return smalltalk.TagBrush||(typeof TagBrush=="undefined"?nil:TagBrush)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($TagBrush())._fromString_canvas_(aString,self);
return $1;
}, function($ctx1) {$ctx1.fill(self,"newTag:",{aString:aString},smalltalk.HTMLCanvas)})},
messageSends: ["fromString:canvas:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "noscript",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("noscript");
return $1;
}, function($ctx1) {$ctx1.fill(self,"noscript",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "object",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("object");
return $1;
}, function($ctx1) {$ctx1.fill(self,"object",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "ol",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("ol");
return $1;
}, function($ctx1) {$ctx1.fill(self,"ol",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "ol:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._ol())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"ol:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "ol"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "optgroup",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("optgroup");
return $1;
}, function($ctx1) {$ctx1.fill(self,"optgroup",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "option",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("option");
return $1;
}, function($ctx1) {$ctx1.fill(self,"option",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "output",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("output");
return $1;
}, function($ctx1) {$ctx1.fill(self,"output",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "p",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("p");
return $1;
}, function($ctx1) {$ctx1.fill(self,"p",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "p:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._p())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"p:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "p"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "param",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("param");
return $1;
}, function($ctx1) {$ctx1.fill(self,"param",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "pre",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("pre");
return $1;
}, function($ctx1) {$ctx1.fill(self,"pre",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "progress",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("progress");
return $1;
}, function($ctx1) {$ctx1.fill(self,"progress",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "root",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@root"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"root",{},smalltalk.HTMLCanvas)})},
messageSends: []}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "root:",
fn: function (aTagBrush){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@root"]=aTagBrush;
return self}, function($ctx1) {$ctx1.fill(self,"root:",{aTagBrush:aTagBrush},smalltalk.HTMLCanvas)})},
messageSends: []}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "script",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("script");
return $1;
}, function($ctx1) {$ctx1.fill(self,"script",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "section",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("section");
return $1;
}, function($ctx1) {$ctx1.fill(self,"section",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "select",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("select");
return $1;
}, function($ctx1) {$ctx1.fill(self,"select",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "small",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("small");
return $1;
}, function($ctx1) {$ctx1.fill(self,"small",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "snippet:",
fn: function (anElement){
var self=this;
var clone,caret;
function $TagBrush(){return smalltalk.TagBrush||(typeof TagBrush=="undefined"?nil:TagBrush)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
clone=_st(_st(anElement)._asJQuery())._clone();
_st(self)._with_(_st($TagBrush())._fromJQuery_canvas_(clone,self));
caret=_st(clone)._find_("[data-snippet=\x22*\x22]");
$1=_st(_st(caret)._toArray())._isEmpty();
if(smalltalk.assert($1)){
caret=clone;
caret;
};
$2=_st($TagBrush())._fromJQuery_canvas_(_st(caret)._removeAttr_("data-snippet"),self);
return $2;
}, function($ctx1) {$ctx1.fill(self,"snippet:",{anElement:anElement,clone:clone,caret:caret},smalltalk.HTMLCanvas)})},
messageSends: ["clone", "asJQuery", "with:", "fromJQuery:canvas:", "find:", "ifTrue:", "isEmpty", "toArray", "removeAttr:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "source",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("source");
return $1;
}, function($ctx1) {$ctx1.fill(self,"source",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "span",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("span");
return $1;
}, function($ctx1) {$ctx1.fill(self,"span",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "span:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._span())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"span:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "span"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "strong",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("strong");
return $1;
}, function($ctx1) {$ctx1.fill(self,"strong",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "strong:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._strong())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"strong:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "strong"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "style",
fn: function (){
var self=this;
function $StyleTag(){return smalltalk.StyleTag||(typeof StyleTag=="undefined"?nil:StyleTag)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@root"])._addBrush_(_st($StyleTag())._canvas_(self));
return $1;
}, function($ctx1) {$ctx1.fill(self,"style",{},smalltalk.HTMLCanvas)})},
messageSends: ["addBrush:", "canvas:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "style:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._style();
_st($2)._with_(aString);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"style:",{aString:aString},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "style", "yourself"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "sub",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("sub");
return $1;
}, function($ctx1) {$ctx1.fill(self,"sub",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "summary",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("summary");
return $1;
}, function($ctx1) {$ctx1.fill(self,"summary",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "sup",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("sup");
return $1;
}, function($ctx1) {$ctx1.fill(self,"sup",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "table",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("table");
return $1;
}, function($ctx1) {$ctx1.fill(self,"table",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "tag:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@root"])._addBrush_(_st(self)._newTag_(aString));
return $1;
}, function($ctx1) {$ctx1.fill(self,"tag:",{aString:aString},smalltalk.HTMLCanvas)})},
messageSends: ["addBrush:", "newTag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "tbody",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("tbody");
return $1;
}, function($ctx1) {$ctx1.fill(self,"tbody",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "td",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("td");
return $1;
}, function($ctx1) {$ctx1.fill(self,"td",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "textarea",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("textarea");
return $1;
}, function($ctx1) {$ctx1.fill(self,"textarea",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "tfoot",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("tfoot");
return $1;
}, function($ctx1) {$ctx1.fill(self,"tfoot",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "th",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("th");
return $1;
}, function($ctx1) {$ctx1.fill(self,"th",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "thead",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("thead");
return $1;
}, function($ctx1) {$ctx1.fill(self,"thead",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "time",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("time");
return $1;
}, function($ctx1) {$ctx1.fill(self,"time",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "title",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("title");
return $1;
}, function($ctx1) {$ctx1.fill(self,"title",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "tr",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("tr");
return $1;
}, function($ctx1) {$ctx1.fill(self,"tr",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "ul",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("ul");
return $1;
}, function($ctx1) {$ctx1.fill(self,"ul",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "ul:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._ul())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"ul:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "ul"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "video",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._tag_("video");
return $1;
}, function($ctx1) {$ctx1.fill(self,"video",{},smalltalk.HTMLCanvas)})},
messageSends: ["tag:"]}),
smalltalk.HTMLCanvas);

smalltalk.addMethod(
smalltalk.method({
selector: "with:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._root())._with_(anObject);
return $1;
}, function($ctx1) {$ctx1.fill(self,"with:",{anObject:anObject},smalltalk.HTMLCanvas)})},
messageSends: ["with:", "root"]}),
smalltalk.HTMLCanvas);


smalltalk.addMethod(
smalltalk.method({
selector: "browserVersion",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(jQuery)._at_("browser"))._version();
return $1;
}, function($ctx1) {$ctx1.fill(self,"browserVersion",{},smalltalk.HTMLCanvas.klass)})},
messageSends: ["version", "at:"]}),
smalltalk.HTMLCanvas.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "isMSIE",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(jQuery)._at_("browser"))._at_("msie"))._notNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isMSIE",{},smalltalk.HTMLCanvas.klass)})},
messageSends: ["notNil", "at:"]}),
smalltalk.HTMLCanvas.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "isMozilla",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(jQuery)._at_("browser"))._at_("mozilla"))._notNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isMozilla",{},smalltalk.HTMLCanvas.klass)})},
messageSends: ["notNil", "at:"]}),
smalltalk.HTMLCanvas.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "isOpera",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(jQuery)._at_("browser"))._at_("opera"))._notNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isOpera",{},smalltalk.HTMLCanvas.klass)})},
messageSends: ["notNil", "at:"]}),
smalltalk.HTMLCanvas.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "isWebkit",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(jQuery)._at_("browser"))._at_("webkit"))._notNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isWebkit",{},smalltalk.HTMLCanvas.klass)})},
messageSends: ["notNil", "at:"]}),
smalltalk.HTMLCanvas.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "onJQuery:",
fn: function (aJQuery){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._basicNew();
_st($2)._initializeFromJQuery_(aJQuery);
_st($2)._initialize();
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"onJQuery:",{aJQuery:aJQuery},smalltalk.HTMLCanvas.klass)})},
messageSends: ["initializeFromJQuery:", "basicNew", "initialize", "yourself"]}),
smalltalk.HTMLCanvas.klass);


smalltalk.addClass('HTMLSnippet', smalltalk.Object, ['snippets'], 'Canvas');
smalltalk.addMethod(
smalltalk.method({
selector: "initializeFromJQuery:",
fn: function (aJQuery){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._snippetsFromJQuery_(aJQuery))._do_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._installSnippetFromJQuery_(_st(each)._asJQuery());
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"initializeFromJQuery:",{aJQuery:aJQuery},smalltalk.HTMLSnippet)})},
messageSends: ["do:", "installSnippetFromJQuery:", "asJQuery", "snippetsFromJQuery:"]}),
smalltalk.HTMLSnippet);

smalltalk.addMethod(
smalltalk.method({
selector: "installSnippetFromJQuery:",
fn: function (element){
var self=this;
var name;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
name=_st(element)._attr_("data-snippet");
$1=_st(name).__eq("*");
if(! smalltalk.assert($1)){
$2=_st(_st("^\x5c*")._asRegexp())._test_(name);
if(smalltalk.assert($2)){
name=_st(name)._allButFirst();
name;
_st(element)._attr_put_("data-snippet","*");
} else {
_st(element)._removeAttr_("data-snippet");
};
_st(self)._snippetAt_install_(name,_st(_st(element)._detach())._get_((0)));
};
return self}, function($ctx1) {$ctx1.fill(self,"installSnippetFromJQuery:",{element:element,name:name},smalltalk.HTMLSnippet)})},
messageSends: ["attr:", "ifFalse:", "ifTrue:ifFalse:", "allButFirst", "attr:put:", "removeAttr:", "test:", "asRegexp", "snippetAt:install:", "get:", "detach", "="]}),
smalltalk.HTMLSnippet);

smalltalk.addMethod(
smalltalk.method({
selector: "snippetAt:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._snippets())._at_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"snippetAt:",{aString:aString},smalltalk.HTMLSnippet)})},
messageSends: ["at:", "snippets"]}),
smalltalk.HTMLSnippet);

smalltalk.addMethod(
smalltalk.method({
selector: "snippetAt:compile:",
fn: function (aString,anElement){
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
function $ClassBuilder(){return smalltalk.ClassBuilder||(typeof ClassBuilder=="undefined"?nil:ClassBuilder)}
return smalltalk.withContext(function($ctx1) { 
_st(_st($ClassBuilder())._new())._installMethod_forClass_category_(_st(_st((function(htmlReceiver){
return smalltalk.withContext(function($ctx2) {
return _st(htmlReceiver)._snippet_(anElement);
}, function($ctx2) {$ctx2.fillBlock({htmlReceiver:htmlReceiver},$ctx1)})}))._currySelf())._asCompiledMethod_(aString),$HTMLCanvas(),"**snippets");
return self}, function($ctx1) {$ctx1.fill(self,"snippetAt:compile:",{aString:aString,anElement:anElement},smalltalk.HTMLSnippet)})},
messageSends: ["installMethod:forClass:category:", "asCompiledMethod:", "currySelf", "snippet:", "new"]}),
smalltalk.HTMLSnippet);

smalltalk.addMethod(
smalltalk.method({
selector: "snippetAt:install:",
fn: function (aString,anElement){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._snippets())._at_put_(aString,anElement);
_st(self)._snippetAt_compile_(aString,anElement);
return self}, function($ctx1) {$ctx1.fill(self,"snippetAt:install:",{aString:aString,anElement:anElement},smalltalk.HTMLSnippet)})},
messageSends: ["at:put:", "snippets", "snippetAt:compile:"]}),
smalltalk.HTMLSnippet);

smalltalk.addMethod(
smalltalk.method({
selector: "snippets",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@snippets"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@snippets"]=smalltalk.HashedCollection._fromPairs_([]);
$1=self["@snippets"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"snippets",{},smalltalk.HTMLSnippet)})},
messageSends: ["ifNil:"]}),
smalltalk.HTMLSnippet);

smalltalk.addMethod(
smalltalk.method({
selector: "snippetsFromJQuery:",
fn: function (aJQuery){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(aJQuery)._find_("[data-snippet]"))._toArray();
return $1;
}, function($ctx1) {$ctx1.fill(self,"snippetsFromJQuery:",{aJQuery:aJQuery},smalltalk.HTMLSnippet)})},
messageSends: ["toArray", "find:"]}),
smalltalk.HTMLSnippet);


smalltalk.HTMLSnippet.klass.iVarNames = ['current'];
smalltalk.addMethod(
smalltalk.method({
selector: "current",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@current"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"current",{},smalltalk.HTMLSnippet.klass)})},
messageSends: []}),
smalltalk.HTMLSnippet.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "ensureCurrent",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
$1=self["@current"];
if(($receiver = $1) == nil || $receiver == undefined){
$2=smalltalk.Object.klass.fn.prototype._new.apply(_st(self), []);
_st($2)._initializeFromJQuery_(_st(document)._asJQuery());
$3=_st($2)._yourself();
self["@current"]=$3;
self["@current"];
} else {
$1;
};
return self}, function($ctx1) {$ctx1.fill(self,"ensureCurrent",{},smalltalk.HTMLSnippet.klass)})},
messageSends: ["ifNil:", "initializeFromJQuery:", "asJQuery", "new", "yourself"]}),
smalltalk.HTMLSnippet.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
smalltalk.Object.klass.fn.prototype._initialize.apply(_st(self), []);
$1=_st(self)._isDOMAvailable();
if(smalltalk.assert($1)){
_st(self)._ensureCurrent();
};
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.HTMLSnippet.klass)})},
messageSends: ["initialize", "ifTrue:", "ensureCurrent", "isDOMAvailable"]}),
smalltalk.HTMLSnippet.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "isDOMAvailable",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
 return typeof document !== 'undefined' ;
return self}, function($ctx1) {$ctx1.fill(self,"isDOMAvailable",{},smalltalk.HTMLSnippet.klass)})},
messageSends: []}),
smalltalk.HTMLSnippet.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "new",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._shouldNotImplement();
return self}, function($ctx1) {$ctx1.fill(self,"new",{},smalltalk.HTMLSnippet.klass)})},
messageSends: ["shouldNotImplement"]}),
smalltalk.HTMLSnippet.klass);


smalltalk.addClass('TagBrush', smalltalk.Object, ['canvas', 'element'], 'Canvas');
smalltalk.addMethod(
smalltalk.method({
selector: "accesskey:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("accesskey",aString);
return self}, function($ctx1) {$ctx1.fill(self,"accesskey:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "action:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("action",aString);
return self}, function($ctx1) {$ctx1.fill(self,"action:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "addBrush:",
fn: function (aTagBrush){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self)._appendChild_(_st(aTagBrush)._element());
$1=aTagBrush;
return $1;
}, function($ctx1) {$ctx1.fill(self,"addBrush:",{aTagBrush:aTagBrush},smalltalk.TagBrush)})},
messageSends: ["appendChild:", "element"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "align:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("align",aString);
return self}, function($ctx1) {$ctx1.fill(self,"align:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "alt:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("alt",aString);
return self}, function($ctx1) {$ctx1.fill(self,"alt:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "append:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(anObject)._appendToBrush_(self);
return self}, function($ctx1) {$ctx1.fill(self,"append:",{anObject:anObject},smalltalk.TagBrush)})},
messageSends: ["appendToBrush:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "appendBlock:",
fn: function (aBlock){
var self=this;
var root;
return smalltalk.withContext(function($ctx1) { 
root=_st(self["@canvas"])._root();
_st(self["@canvas"])._root_(self);
_st(aBlock)._value_(self["@canvas"]);
_st(self["@canvas"])._root_(root);
return self}, function($ctx1) {$ctx1.fill(self,"appendBlock:",{aBlock:aBlock,root:root},smalltalk.TagBrush)})},
messageSends: ["root", "root:", "value:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "appendChild:",
fn: function (anElement){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var element=self['@element'];
	if (null == element.canHaveChildren || element.canHaveChildren) {
		element.appendChild(anElement);
	} else {
		element.text = String(element.text) + anElement.innerHTML;
	} ;
return self}, function($ctx1) {$ctx1.fill(self,"appendChild:",{anElement:anElement},smalltalk.TagBrush)})},
messageSends: []}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "appendString:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._appendChild_(_st(self)._createTextNodeFor_(aString));
return self}, function($ctx1) {$ctx1.fill(self,"appendString:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["appendChild:", "createTextNodeFor:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "appendToBrush:",
fn: function (aTagBrush){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aTagBrush)._addBrush_(self);
return self}, function($ctx1) {$ctx1.fill(self,"appendToBrush:",{aTagBrush:aTagBrush},smalltalk.TagBrush)})},
messageSends: ["addBrush:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "asJQuery",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(window)._jQuery_(_st(self)._element());
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJQuery",{},smalltalk.TagBrush)})},
messageSends: ["jQuery:", "element"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (aString,aValue){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self['@element'].setAttribute(aString, aValue);
return self}, function($ctx1) {$ctx1.fill(self,"at:put:",{aString:aString,aValue:aValue},smalltalk.TagBrush)})},
messageSends: []}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "class:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self['@element'].className = aString;
return self}, function($ctx1) {$ctx1.fill(self,"class:",{aString:aString},smalltalk.TagBrush)})},
messageSends: []}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "cols:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("cols",aString);
return self}, function($ctx1) {$ctx1.fill(self,"cols:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "contenteditable:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("contenteditable",aString);
return self}, function($ctx1) {$ctx1.fill(self,"contenteditable:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "contents:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=self;
_st($1)._empty();
$2=_st($1)._append_(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"contents:",{anObject:anObject},smalltalk.TagBrush)})},
messageSends: ["empty", "append:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "contextmenu:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("contextmenu",aString);
return self}, function($ctx1) {$ctx1.fill(self,"contextmenu:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "createElementFor:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return document.createElement(String(aString));
return self}, function($ctx1) {$ctx1.fill(self,"createElementFor:",{aString:aString},smalltalk.TagBrush)})},
messageSends: []}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "createTextNodeFor:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return document.createTextNode(String(aString));
return self}, function($ctx1) {$ctx1.fill(self,"createTextNodeFor:",{aString:aString},smalltalk.TagBrush)})},
messageSends: []}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "draggable:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("draggable",aString);
return self}, function($ctx1) {$ctx1.fill(self,"draggable:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "element",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@element"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"element",{},smalltalk.TagBrush)})},
messageSends: []}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "empty",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._empty();
return self}, function($ctx1) {$ctx1.fill(self,"empty",{},smalltalk.TagBrush)})},
messageSends: ["empty", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "for:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("for",aString);
return self}, function($ctx1) {$ctx1.fill(self,"for:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "height:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("height",aString);
return self}, function($ctx1) {$ctx1.fill(self,"height:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "hidden",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("hidden","hidden");
return self}, function($ctx1) {$ctx1.fill(self,"hidden",{},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "href:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("href",aString);
return self}, function($ctx1) {$ctx1.fill(self,"href:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "id:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("id",aString);
return self}, function($ctx1) {$ctx1.fill(self,"id:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeFromJQuery:canvas:",
fn: function (aJQuery,aCanvas){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@element"]=_st(aJQuery)._get_((0));
self["@canvas"]=aCanvas;
return self}, function($ctx1) {$ctx1.fill(self,"initializeFromJQuery:canvas:",{aJQuery:aJQuery,aCanvas:aCanvas},smalltalk.TagBrush)})},
messageSends: ["get:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeFromString:canvas:",
fn: function (aString,aCanvas){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@element"]=_st(self)._createElementFor_(aString);
self["@canvas"]=aCanvas;
return self}, function($ctx1) {$ctx1.fill(self,"initializeFromString:canvas:",{aString:aString,aCanvas:aCanvas},smalltalk.TagBrush)})},
messageSends: ["createElementFor:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "media:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("media",aString);
return self}, function($ctx1) {$ctx1.fill(self,"media:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "method:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("method",aString);
return self}, function($ctx1) {$ctx1.fill(self,"method:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "name:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("name",aString);
return self}, function($ctx1) {$ctx1.fill(self,"name:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onBlur:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("blur",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onBlur:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onChange:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("change",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onChange:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onClick:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("click",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onClick:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onDblClick:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("dblclick",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onDblClick:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onFocus:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("focus",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onFocus:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onFocusIn:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("focusin",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onFocusIn:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onFocusOut:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("focusout",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onFocusOut:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onHover:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("hover",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onHover:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyDown:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("keydown",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onKeyDown:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyPress:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("keypress",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onKeyPress:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyUp:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("keyup",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onKeyUp:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onMouseDown:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("mousedown",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onMouseDown:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onMouseEnter:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("mouseenter",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onMouseEnter:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onMouseLeave:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("mouseleave",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onMouseLeave:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onMouseMove:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("mousemove",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onMouseMove:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onMouseOut:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("mouseout",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onMouseOut:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onMouseOver:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("mouseover",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onMouseOver:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onMouseUp:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("mouseup",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onMouseUp:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onSelect:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("select",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onSelect:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onSubmit:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("submit",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onSubmit:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "onUnload:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._asJQuery())._bind_do_("unload",aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"onUnload:",{aBlock:aBlock},smalltalk.TagBrush)})},
messageSends: ["bind:do:", "asJQuery"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "placeholder:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("placeholder",aString);
return self}, function($ctx1) {$ctx1.fill(self,"placeholder:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "rel:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("rel",aString);
return self}, function($ctx1) {$ctx1.fill(self,"rel:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "removeAt:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self['@element'].removeAttribute(aString);
return self}, function($ctx1) {$ctx1.fill(self,"removeAt:",{aString:aString},smalltalk.TagBrush)})},
messageSends: []}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "rows:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("rows",aString);
return self}, function($ctx1) {$ctx1.fill(self,"rows:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "src:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("src",aString);
return self}, function($ctx1) {$ctx1.fill(self,"src:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "style:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("style",aString);
return self}, function($ctx1) {$ctx1.fill(self,"style:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "tabindex:",
fn: function (aNumber){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("tabindex",aNumber);
return self}, function($ctx1) {$ctx1.fill(self,"tabindex:",{aNumber:aNumber},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "target:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("target",aString);
return self}, function($ctx1) {$ctx1.fill(self,"target:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "title:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("title",aString);
return self}, function($ctx1) {$ctx1.fill(self,"title:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "type:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("type",aString);
return self}, function($ctx1) {$ctx1.fill(self,"type:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "valign:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("valign",aString);
return self}, function($ctx1) {$ctx1.fill(self,"valign:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "value:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("value",aString);
return self}, function($ctx1) {$ctx1.fill(self,"value:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "width:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_("width",aString);
return self}, function($ctx1) {$ctx1.fill(self,"width:",{aString:aString},smalltalk.TagBrush)})},
messageSends: ["at:put:"]}),
smalltalk.TagBrush);

smalltalk.addMethod(
smalltalk.method({
selector: "with:",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._append_(anObject);
return self}, function($ctx1) {$ctx1.fill(self,"with:",{anObject:anObject},smalltalk.TagBrush)})},
messageSends: ["append:"]}),
smalltalk.TagBrush);


smalltalk.addMethod(
smalltalk.method({
selector: "fromJQuery:canvas:",
fn: function (aJQuery,aCanvas){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._initializeFromJQuery_canvas_(aJQuery,aCanvas);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromJQuery:canvas:",{aJQuery:aJQuery,aCanvas:aCanvas},smalltalk.TagBrush.klass)})},
messageSends: ["initializeFromJQuery:canvas:", "new", "yourself"]}),
smalltalk.TagBrush.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromString:canvas:",
fn: function (aString,aCanvas){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._initializeFromString_canvas_(aString,aCanvas);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromString:canvas:",{aString:aString,aCanvas:aCanvas},smalltalk.TagBrush.klass)})},
messageSends: ["initializeFromString:canvas:", "new", "yourself"]}),
smalltalk.TagBrush.klass);


smalltalk.addClass('StyleTag', smalltalk.TagBrush, ['canvas', 'element'], 'Canvas');
smalltalk.addMethod(
smalltalk.method({
selector: "with:",
fn: function (aString){
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($HTMLCanvas())._isMSIE();
if(smalltalk.assert($1)){
_st(_st(_st(self)._element())._styleSheet())._cssText_(aString);
} else {
smalltalk.TagBrush.fn.prototype._with_.apply(_st(self), [aString]);
};
return self}, function($ctx1) {$ctx1.fill(self,"with:",{aString:aString},smalltalk.StyleTag)})},
messageSends: ["ifTrue:ifFalse:", "cssText:", "styleSheet", "element", "with:", "isMSIE"]}),
smalltalk.StyleTag);


smalltalk.addMethod(
smalltalk.method({
selector: "canvas:",
fn: function (aCanvas){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._initializeFromString_canvas_("style",aCanvas);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"canvas:",{aCanvas:aCanvas},smalltalk.StyleTag.klass)})},
messageSends: ["initializeFromString:canvas:", "new", "yourself"]}),
smalltalk.StyleTag.klass);


smalltalk.addClass('Widget', smalltalk.Object, [], 'Canvas');
smalltalk.addMethod(
smalltalk.method({
selector: "appendToBrush:",
fn: function (aTagBrush){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._appendToJQuery_(_st(aTagBrush)._asJQuery());
return self}, function($ctx1) {$ctx1.fill(self,"appendToBrush:",{aTagBrush:aTagBrush},smalltalk.Widget)})},
messageSends: ["appendToJQuery:", "asJQuery"]}),
smalltalk.Widget);

smalltalk.addMethod(
smalltalk.method({
selector: "appendToJQuery:",
fn: function (aJQuery){
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._renderOn_(_st($HTMLCanvas())._onJQuery_(aJQuery));
return self}, function($ctx1) {$ctx1.fill(self,"appendToJQuery:",{aJQuery:aJQuery},smalltalk.Widget)})},
messageSends: ["renderOn:", "onJQuery:"]}),
smalltalk.Widget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
fn: function (html){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"renderOn:",{html:html},smalltalk.Widget)})},
messageSends: []}),
smalltalk.Widget);



smalltalk.addMethod(
smalltalk.method({
selector: "appendToBrush:",
fn: function (aTagBrush){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aTagBrush)._append_(_st(self)._asString());
return self}, function($ctx1) {$ctx1.fill(self,"appendToBrush:",{aTagBrush:aTagBrush},smalltalk.Object)})},
messageSends: ["append:", "asString"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "appendToJQuery:",
fn: function (aJQuery){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aJQuery)._append_(_st(self)._asString());
return self}, function($ctx1) {$ctx1.fill(self,"appendToJQuery:",{aJQuery:aJQuery},smalltalk.Object)})},
messageSends: ["append:", "asString"]}),
smalltalk.Object);

smalltalk.addMethod(
smalltalk.method({
selector: "appendToBrush:",
fn: function (aTagBrush){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aTagBrush)._appendBlock_(self);
return self}, function($ctx1) {$ctx1.fill(self,"appendToBrush:",{aTagBrush:aTagBrush},smalltalk.BlockClosure)})},
messageSends: ["appendBlock:"]}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "appendToJQuery:",
fn: function (aJQuery){
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._value_(_st($HTMLCanvas())._onJQuery_(aJQuery));
return self}, function($ctx1) {$ctx1.fill(self,"appendToJQuery:",{aJQuery:aJQuery},smalltalk.BlockClosure)})},
messageSends: ["value:", "onJQuery:"]}),
smalltalk.BlockClosure);

smalltalk.addMethod(
smalltalk.method({
selector: "asSnippet",
fn: function (){
var self=this;
function $HTMLSnippet(){return smalltalk.HTMLSnippet||(typeof HTMLSnippet=="undefined"?nil:HTMLSnippet)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st($HTMLSnippet())._current())._snippetAt_(_st(self)._asString());
return $1;
}, function($ctx1) {$ctx1.fill(self,"asSnippet",{},smalltalk.CharacterArray)})},
messageSends: ["snippetAt:", "asString", "current"]}),
smalltalk.CharacterArray);

smalltalk.addMethod(
smalltalk.method({
selector: "appendToBrush:",
fn: function (aTagBrush){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aTagBrush)._appendString_(self);
return self}, function($ctx1) {$ctx1.fill(self,"appendToBrush:",{aTagBrush:aTagBrush},smalltalk.String)})},
messageSends: ["appendString:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "appendToJQuery:",
fn: function (aJQuery){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aJQuery)._append_(self);
return self}, function($ctx1) {$ctx1.fill(self,"appendToJQuery:",{aJQuery:aJQuery},smalltalk.String)})},
messageSends: ["append:"]}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asJQuery",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return jQuery(String(self));
return self}, function($ctx1) {$ctx1.fill(self,"asJQuery",{},smalltalk.String)})},
messageSends: []}),
smalltalk.String);

smalltalk.addMethod(
smalltalk.method({
selector: "asJQuery",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return jQuery(self['@jsObject']);
return self}, function($ctx1) {$ctx1.fill(self,"asJQuery",{},smalltalk.JSObjectProxy)})},
messageSends: []}),
smalltalk.JSObjectProxy);

smalltalk.addPackage('Mapless');
smalltalk.addClass('MaplessModel', smalltalk.Object, ['data'], 'Mapless');
smalltalk.addMethod(
smalltalk.method({
selector: "=",
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(anObject)._respondsTo_(smalltalk.symbolFor("id")))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(self)._id()).__eq(_st(anObject)._id());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"=",{anObject:anObject},smalltalk.MaplessModel)})},
messageSends: ["and:", "=", "id", "respondsTo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSONString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self)._onAboutToJSON();
$1=_st((smalltalk.JSON || JSON))._stringify_(self["@data"]);
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSONString",{},smalltalk.MaplessModel)})},
messageSends: ["onAboutToJSON", "stringify:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:",
fn: function (aKey){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_(aKey);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:",{aKey:aKey},smalltalk.MaplessModel)})},
messageSends: ["at:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_ifAbsent_(aKey,aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{aKey:aKey,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["at:ifAbsent:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (aKey,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self["@data"])._at_put_(aKey,anObject);
$1=anObject;
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:put:",{aKey:aKey,anObject:anObject},smalltalk.MaplessModel)})},
messageSends: ["at:put:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "create",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._createDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"create",{},smalltalk.MaplessModel)})},
messageSends: ["createDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(self)._path()),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterCreate_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fai").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelCreateError || ModelCreateError))._signal_(_st(_st(_st("Could not create ").__comma(_st(_st(self)._class())._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelCreateError || ModelCreateError))._signal_(_st(_st(_st("Could not create ").__comma(_st(_st(self)._class())._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"createDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["ajax:", "->", "path", "asJSONString", "onAfterCreate:done:", "signal:", ",", "responseText", "name", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOn",
fn: function (){
var self=this;
var selector,expects,object;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4;
selector=smalltalk.symbolFor("createdOn");
expects=(smalltalk.Date || Date);
object=_st(self)._at_(_st(selector)._asString());
$1=object;
if(($receiver = $1) == nil || $receiver == undefined){
return nil;
} else {
$1;
};
$2=_st(object)._isKindOf_(expects);
if(smalltalk.assert($2)){
$3=object;
return $3;
};
$4=_st(self)._at_put_(_st(selector)._asString(),_st(self)._dateAndTimeAt_(selector));
return $4;
}, function($ctx1) {$ctx1.fill(self,"createdOn",{selector:selector,expects:expects,object:object},smalltalk.MaplessModel)})},
messageSends: ["at:", "asString", "ifNil:", "ifTrue:", "isKindOf:", "at:put:", "dateAndTimeAt:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOn:",
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_(smalltalk.symbolFor("createdOn"),aDate);
return self}, function($ctx1) {$ctx1.fill(self,"createdOn:",{aDate:aDate},smalltalk.MaplessModel)})},
messageSends: ["at:put:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "data",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@data"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"data",{},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "dateAndTimeAt:",
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.Date || Date))._fromString_(_st(self)._at_(aSelector));
return $1;
}, function($ctx1) {$ctx1.fill(self,"dateAndTimeAt:",{aSelector:aSelector},smalltalk.MaplessModel)})},
messageSends: ["fromString:", "at:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "delete",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._deleteDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"delete",{},smalltalk.MaplessModel)})},
messageSends: ["deleteDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "deleteDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(_st(self)._path()).__comma("?id=")).__comma(_st(_st(self)._id())._asString())),_st("type").__minus_gt("DELETE"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterDelete_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onDeleteFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onDeleteFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"deleteDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["ajax:", "->", ",", "asString", "id", "path", "asJSONString", "onAfterDelete:done:", "onDeleteFail:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "doesNotUnderstand:",
fn: function (aMessage){
var self=this;
var key,part,subModel,isUndefined,isObject,obj,keys;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$12,$11;
key=_st(_st(aMessage)._selector())._asSymbol();
$1=_st(self)._isUnary_(key);
if(smalltalk.assert($1)){
$2=_st(self["@data"])._isKindOf_((smalltalk.HashedCollection || HashedCollection));
if(smalltalk.assert($2)){
part=_st(self["@data"])._at_ifAbsent_(_st(key)._asString(),(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
part;
} else {
part=_st(self["@data"])._at_(_st(key)._asString());
part;
};
$3=part;
if(($receiver = $3) == nil || $receiver == undefined){
return nil;
} else {
$3;
};
isUndefined=_st(self)._isUndefinedPart_(part);
isUndefined;
$4=isUndefined;
if(smalltalk.assert($4)){
return nil;
};
isObject=_st(self)._isObjectPart_(part);
isObject;
$5=isObject;
if(smalltalk.assert($5)){
obj=_st(self)._newJSObject();
obj;
obj=_st((smalltalk.JSObjectProxy || JSObjectProxy))._on_(obj);
obj;
$6=_st(_st(obj)._keys_(part))._isEmpty();
if(smalltalk.assert($6)){
_st(self["@data"])._at_put_(_st(key)._asString(),nil);
return nil;
};
};
subModel=_st(self)._get_from_("modelClass",part);
subModel;
$7=subModel;
if(($receiver = $7) == nil || $receiver == undefined){
$8=part;
return $8;
} else {
$7;
};
subModel=_st(_st((smalltalk.Smalltalk || Smalltalk))._current())._at_(subModel);
subModel;
$9=subModel;
if(($receiver = $9) == nil || $receiver == undefined){
_st(part)._inspect();
_st(self)._error_(_st(_st("this should have a ").__comma(subModel)).__comma(" modelClass no?"));
} else {
$9;
};
subModel=_st(subModel)._fromReified_(part);
subModel;
_st(self["@data"])._at_put_(_st(key)._asString(),subModel);
$10=subModel;
return $10;
};
$12=_st(_st(self)._isKeyword_(key))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(key)._asString())._occurrencesOf_(":")).__eq((1));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(smalltalk.assert($12)){
key=_st(key)._allButLast();
key;
$11=_st(self["@data"])._at_put_(_st(key)._asString(),_st(_st(aMessage)._arguments())._first());
} else {
$11=smalltalk.Object.fn.prototype._doesNotUnderstand_.apply(_st(self), [aMessage]);
};
return $11;
}, function($ctx1) {$ctx1.fill(self,"doesNotUnderstand:",{aMessage:aMessage,key:key,part:part,subModel:subModel,isUndefined:isUndefined,isObject:isObject,obj:obj,keys:keys},smalltalk.MaplessModel)})},
messageSends: ["asSymbol", "selector", "ifTrue:", "ifTrue:ifFalse:", "at:ifAbsent:", "asString", "at:", "isKindOf:", "ifNil:", "isUndefinedPart:", "isObjectPart:", "newJSObject", "on:", "at:put:", "isEmpty", "keys:", "get:from:", "current", "inspect", "error:", ",", "fromReified:", "isUnary:", "allButLast", "first", "arguments", "doesNotUnderstand:", "and:", "=", "occurrencesOf:", "isKeyword:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "get:from:",
fn: function (anAttribute,aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return aPart[anAttribute];
return self}, function($ctx1) {$ctx1.fill(self,"get:from:",{anAttribute:anAttribute,aPart:aPart},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "id",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_(smalltalk.symbolFor("id"));
return $1;
}, function($ctx1) {$ctx1.fill(self,"id",{},smalltalk.MaplessModel)})},
messageSends: ["at:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "id:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@data"])._at_put_(smalltalk.symbolFor("id"),aString);
return self}, function($ctx1) {$ctx1.fill(self,"id:",{aString:aString},smalltalk.MaplessModel)})},
messageSends: ["at:put:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
function $HashedCollection(){return smalltalk.HashedCollection||(typeof HashedCollection=="undefined"?nil:HashedCollection)}
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
self["@data"]=_st($HashedCollection())._new();
_st(self)._modelClass_(_st(_st(self)._class())._name());
_st(self)._initializeInstanceVersion();
_st(self)._id_(_st(_st(self)._class())._newUUID());
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.MaplessModel)})},
messageSends: ["initialize", "new", "modelClass:", "name", "class", "initializeInstanceVersion", "id:", "newUUID"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeCreatedOn",
fn: function (){
var self=this;
function $Date(){return smalltalk.Date||(typeof Date=="undefined"?nil:Date)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._createdOn_(_st($Date())._now());
return self}, function($ctx1) {$ctx1.fill(self,"initializeCreatedOn",{},smalltalk.MaplessModel)})},
messageSends: ["createdOn:", "now"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeInstanceVersion",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._v_((1));
return self}, function($ctx1) {$ctx1.fill(self,"initializeInstanceVersion",{},smalltalk.MaplessModel)})},
messageSends: ["v:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isKeyword:",
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(aSelector)._asString())._includes_(":");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isKeyword:",{aSelector:aSelector},smalltalk.MaplessModel)})},
messageSends: ["includes:", "asString"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isObjectPart:",
fn: function (aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return typeof part=='object';
return self}, function($ctx1) {$ctx1.fill(self,"isObjectPart:",{aPart:aPart},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isUnary:",
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(aSelector)._asString())._includes_(":"))._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isUnary:",{aSelector:aSelector},smalltalk.MaplessModel)})},
messageSends: ["not", "includes:", "asString"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isUndefinedPart:",
fn: function (aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return aPart=='undefined';
return self}, function($ctx1) {$ctx1.fill(self,"isUndefinedPart:",{aPart:aPart},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "localDelete",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._class())._localDelete_(self);
return self}, function($ctx1) {$ctx1.fill(self,"localDelete",{},smalltalk.MaplessModel)})},
messageSends: ["localDelete:", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._class())._localSave_(self);
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.MaplessModel)})},
messageSends: ["localSave:", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "newJSObject",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Object;
return self}, function($ctx1) {$ctx1.fill(self,"newJSObject",{},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAboutToJSON",
fn: function (){
var self=this;
var obj,keys;
return smalltalk.withContext(function($ctx1) { 
var $1;
obj=_st(self)._newJSObject();
obj=_st((smalltalk.JSObjectProxy || JSObjectProxy))._on_(obj);
keys=_st(obj)._keys_(self["@data"]);
_st(keys)._do_((function(key){
var value;
return smalltalk.withContext(function($ctx2) {
value=_st(self["@data"])._at_(key);
value;
value=_st(self)._perform_(_st(key)._asSymbol());
value;
$1=_st(value)._isKindOf_((smalltalk.MaplessModel || MaplessModel));
if(smalltalk.assert($1)){
_st(value)._onAboutToJSON();
value=_st(value)._data();
value;
};
return _st(self["@data"])._at_put_(key,value);
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"onAboutToJSON",{obj:obj,keys:keys},smalltalk.MaplessModel)})},
messageSends: ["newJSObject", "on:", "keys:", "do:", "at:", "perform:", "asSymbol", "ifTrue:", "onAboutToJSON", "data", "isKindOf:", "at:put:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterCreate:done:",
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@data"]=_st(_st(self)._class())._reify_(x);
_st(self)._announce_(_st((smalltalk.ModelCreated || ModelCreated))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterCreate:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["reify:", "class", "announce:", "for:", "value:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterDelete:done:",
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._announce_(_st((smalltalk.ModelDeleted || ModelDeleted))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterDelete:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["announce:", "for:", "value:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterRefresh:done:",
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._syncWith_(_st(_st(self)._class())._reify_(x));
_st(self)._announce_(_st((smalltalk.ModelRefreshed || ModelRefreshed))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterRefresh:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["syncWith:", "reify:", "class", "announce:", "for:", "value:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterSave:done:",
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._announce_(_st((smalltalk.ModelSaved || ModelSaved))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterSave:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["announce:", "for:", "value:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onDeleteFail:",
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelUpdateError || ModelUpdateError))._signal_(_st("Could not delete ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onDeleteFail:",{x:x},smalltalk.MaplessModel)})},
messageSends: ["signal:", ",", "name", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onRefreshFail:",
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelRefreshError || ModelRefreshError))._signal_(_st("Could not refresh ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onRefreshFail:",{x:x},smalltalk.MaplessModel)})},
messageSends: ["signal:", ",", "name", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onSaveFail:",
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelUpdateError || ModelUpdateError))._signal_(_st("Could not save ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onSaveFail:",{x:x},smalltalk.MaplessModel)})},
messageSends: ["signal:", ",", "name", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "path",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._path();
return $1;
}, function($ctx1) {$ctx1.fill(self,"path",{},smalltalk.MaplessModel)})},
messageSends: ["path", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "refresh",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._refreshDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"refresh",{},smalltalk.MaplessModel)})},
messageSends: ["refreshDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "refreshDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(_st(self)._path()).__comma("?id=")).__comma(_st(_st(self)._id())._asString())),_st("type").__minus_gt("GET"),_st("cache").__minus_gt(false),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterRefresh_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onRefeshFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onRefreshFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"refreshDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["ajax:", "->", ",", "asString", "id", "path", "onAfterRefresh:done:", "onRefeshFail:", "onRefreshFail:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "remoteSaveDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(self)._path()),_st("type").__minus_gt("PUT"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterSave_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onSaveFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onSaveFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"remoteSaveDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["ajax:", "->", "path", "asJSONString", "onAfterSave:done:", "onSaveFail:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "save",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._saveDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"save",{},smalltalk.MaplessModel)})},
messageSends: ["saveDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "saveDo:",
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._remoteSaveDo_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"saveDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["localSave", "remoteSaveDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "subModelAt:",
fn: function (aSelector){
var self=this;
var subModelData,modelClass;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5;
subModelData=_st(self["@data"])._at_(aSelector);
$1=subModelData;
if(($receiver = $1) == nil || $receiver == undefined){
return nil;
} else {
$1;
};
modelClass=_st(subModelData)._at_("modelClass");
$2=modelClass;
if(($receiver = $2) == nil || $receiver == undefined){
return nil;
} else {
$2;
};
modelClass=_st(_st((smalltalk.Smalltalk || Smalltalk))._current())._at_(modelClass);
$3=modelClass;
if(($receiver = $3) == nil || $receiver == undefined){
$4=_st((smalltalk.ModelMetadataError || ModelMetadataError))._signal_(_st(_st("Cannot find ").__comma(_st(aSelector)._asString())).__comma("'s class for this metadata"));
return $4;
} else {
$3;
};
$5=_st(modelClass)._fromReified_(subModelData);
return $5;
}, function($ctx1) {$ctx1.fill(self,"subModelAt:",{aSelector:aSelector,subModelData:subModelData,modelClass:modelClass},smalltalk.MaplessModel)})},
messageSends: ["at:", "ifNil:", "current", "signal:", ",", "asString", "fromReified:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "syncFrom:",
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._syncWith_(_st(_st(self)._class())._reify_(someJson));
return $1;
}, function($ctx1) {$ctx1.fill(self,"syncFrom:",{someJson:someJson},smalltalk.MaplessModel)})},
messageSends: ["syncWith:", "reify:", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "syncWith:",
fn: function (aReifiedJSON){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=aReifiedJSON;
if(($receiver = $1) == nil || $receiver == undefined){
return nil;
} else {
$1;
};
$2=_st(_st(_st(aReifiedJSON)._at_("modelClass"))._isNil())._or_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(aReifiedJSON)._at_("modelClass")).__tild_eq(_st(_st(self)._class())._name());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(smalltalk.assert($2)){
_st(self)._error_("this JSON does not fit here");
};
self["@data"]=aReifiedJSON;
return self}, function($ctx1) {$ctx1.fill(self,"syncWith:",{aReifiedJSON:aReifiedJSON},smalltalk.MaplessModel)})},
messageSends: ["ifNil:", "ifTrue:", "error:", "or:", "~=", "name", "class", "at:", "isNil"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "url",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self)._path()).__comma("/")).__comma(_st(_st(self)._id())._asString());
return $1;
}, function($ctx1) {$ctx1.fill(self,"url",{},smalltalk.MaplessModel)})},
messageSends: [",", "asString", "id", "path"]}),
smalltalk.MaplessModel);


smalltalk.addMethod(
smalltalk.method({
selector: "atId:do:",
fn: function (anId,onDone){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._read_do_(anId,onDone);
return $1;
}, function($ctx1) {$ctx1.fill(self,"atId:do:",{anId:anId,onDone:onDone},smalltalk.MaplessModel.klass)})},
messageSends: ["read:do:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "create",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._createDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"create",{},smalltalk.MaplessModel.klass)})},
messageSends: ["createDo:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "createDo:",
fn: function (aBlock){
var self=this;
var newInstance;
return smalltalk.withContext(function($ctx1) { 
var $1;
newInstance=_st(_st(self)._basicNew())._initialize();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(self)._path()),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(newInstance)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(newInstance)._onAfterCreate_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelCreateError || ModelCreateError))._signal_(_st(_st(_st("Could not create ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelCreateError || ModelCreateError))._signal_(_st(_st(_st("Could not create ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
$1=newInstance;
return $1;
}, function($ctx1) {$ctx1.fill(self,"createDo:",{aBlock:aBlock,newInstance:newInstance},smalltalk.MaplessModel.klass)})},
messageSends: ["initialize", "basicNew", "ajax:", "->", "path", "asJSONString", "onAfterCreate:done:", "signal:", ",", "responseText", "name"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOnFrom:",
fn: function (aReifiedJSON){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.Date || Date))._fromString_(aReifiedJSON);
return $1;
}, function($ctx1) {$ctx1.fill(self,"createdOnFrom:",{aReifiedJSON:aReifiedJSON},smalltalk.MaplessModel.klass)})},
messageSends: ["fromString:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromJson:",
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._syncFrom_(someJson);
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromJson:",{someJson:someJson},smalltalk.MaplessModel.klass)})},
messageSends: ["syncFrom:", "new"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromJsonString:",
fn: function (someJsonString){
var self=this;
var json,modelClass;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
json=_st(self)._reify_(someJsonString);
modelClass=_st(json)._at_ifAbsent_("modelClass",(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st(modelClass)._isNil();
if(smalltalk.assert($2)){
$1=nil;
} else {
$1=_st(_st(_st(_st($Smalltalk())._current())._at_(modelClass))._new())._syncFrom_(someJsonString);
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromJsonString:",{someJsonString:someJsonString,json:json,modelClass:modelClass},smalltalk.MaplessModel.klass)})},
messageSends: ["reify:", "at:ifAbsent:", "ifTrue:ifFalse:", "syncFrom:", "new", "at:", "current", "isNil"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromReified:",
fn: function (aReifiedJSON){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._syncWith_(aReifiedJSON);
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromReified:",{aReifiedJSON:aReifiedJSON},smalltalk.MaplessModel.klass)})},
messageSends: ["syncWith:", "new"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "localDelete:",
fn: function (aMaplessModel){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._localStorage())._removeItem_(_st(aMaplessModel)._id());
return self}, function($ctx1) {$ctx1.fill(self,"localDelete:",{aMaplessModel:aMaplessModel},smalltalk.MaplessModel.klass)})},
messageSends: ["removeItem:", "id", "localStorage"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "localLoadAt:",
fn: function (anId){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._fromJsonString_(_st(_st(window)._localStorage())._getItem_(anId));
return $1;
}, function($ctx1) {$ctx1.fill(self,"localLoadAt:",{anId:anId},smalltalk.MaplessModel.klass)})},
messageSends: ["fromJsonString:", "getItem:", "localStorage"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave:",
fn: function (aMaplessModel){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._localStorage())._setItem_value_(_st(aMaplessModel)._id(),_st(aMaplessModel)._asJSONString());
return self}, function($ctx1) {$ctx1.fill(self,"localSave:",{aMaplessModel:aMaplessModel},smalltalk.MaplessModel.klass)})},
messageSends: ["setItem:value:", "id", "asJSONString", "localStorage"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "manyFromJson:",
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st((smalltalk.JSON || JSON))._parse_(someJson))._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._fromReified_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"manyFromJson:",{someJson:someJson},smalltalk.MaplessModel.klass)})},
messageSends: ["collect:", "fromReified:", "parse:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "newUUID",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._uuidGenerator())._value();
return $1;
}, function($ctx1) {$ctx1.fill(self,"newUUID",{},smalltalk.MaplessModel.klass)})},
messageSends: ["value", "uuidGenerator"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterRead:done:",
fn: function (someJson,aBlock){
var self=this;
var reified;
return smalltalk.withContext(function($ctx1) { 
var $1;
reified=_st(self)._fromJson_(someJson);
_st(aBlock)._value_(reified);
$1=reified;
return $1;
}, function($ctx1) {$ctx1.fill(self,"onAfterRead:done:",{someJson:someJson,aBlock:aBlock,reified:reified},smalltalk.MaplessModel.klass)})},
messageSends: ["fromJson:", "value:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "read:",
fn: function (anId){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._read_do_(anId,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"read:",{anId:anId},smalltalk.MaplessModel.klass)})},
messageSends: ["read:do:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "read:do:",
fn: function (anId,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(_st(self)._path()).__comma("?id=")).__comma(anId)),_st("type").__minus_gt("GET"),_st("cache").__minus_gt(false),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterRead_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelReadError || ModelReadError))._signal_(_st(_st(_st("Could not read ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelReadError || ModelReadError))._signal_(_st(_st(_st("Could not read ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"read:do:",{anId:anId,aBlock:aBlock},smalltalk.MaplessModel.klass)})},
messageSends: ["ajax:", "->", ",", "path", "onAfterRead:done:", "signal:", "responseText", "name"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "reify:",
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.JSON || JSON))._parse_(someJson);
return $1;
}, function($ctx1) {$ctx1.fill(self,"reify:",{someJson:someJson},smalltalk.MaplessModel.klass)})},
messageSends: ["parse:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "uuidGenerator",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return function guid() {
    function _p8(s) {
        var p = (Math.random().toString(16)+"000000000").substr(2,8);
        return s ? "-" + p.substr(0,4) + "-" + p.substr(4,4) : p ;
    }
    return _p8() + _p8(true) + _p8(true) + _p8();
};
return self}, function($ctx1) {$ctx1.fill(self,"uuidGenerator",{},smalltalk.MaplessModel.klass)})},
messageSends: []}),
smalltalk.MaplessModel.klass);


smalltalk.addPackage('SocialAPI');
smalltalk.addClass('AbstractSocialAPI', smalltalk.Object, ['appId'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "appId",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@appId"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"appId",{},smalltalk.AbstractSocialAPI)});},
messageSends: []}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "appId:",
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@appId"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"appId:",{aString:aString},smalltalk.AbstractSocialAPI)});},
messageSends: []}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "hasLoginDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"hasLoginDo:",{aBlock:aBlock},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "load",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"load",{},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "login",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"login",{},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "logout",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"logout",{},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "profileDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"profileDo:",{aBlock:aBlock},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);



smalltalk.addClass('Facebook', smalltalk.AbstractSocialAPI, ['channelUrl'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "basicLoad",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
// Load the SDK asynchronously
  (function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));;
return self}, function($ctx1) {$ctx1.fill(self,"basicLoad",{},smalltalk.Facebook)});},
messageSends: []}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "channelUrl",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@channelUrl"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"channelUrl",{},smalltalk.Facebook)});},
messageSends: []}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "channelUrl:",
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@channelUrl"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"channelUrl:",{aString:aString},smalltalk.Facebook)});},
messageSends: []}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "hasLoginDo:",
fn: function (aBlock) {
var self=this;
function $FB(){return smalltalk.FB||(typeof FB=="undefined"?nil:FB)}
return smalltalk.withContext(function($ctx1) { 
_st($FB())._getLoginStatus_((function(answer){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_(_st(_st(answer)._status()).__eq("connected"));
}, function($ctx2) {$ctx2.fillBlock({answer:answer},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"hasLoginDo:",{aBlock:aBlock},smalltalk.Facebook)});},
messageSends: ["getLoginStatus:", "value:", "=", "status"]}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "load",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=self;
_st($1)._setup();
$2=_st($1)._basicLoad();
return self}, function($ctx1) {$ctx1.fill(self,"load",{},smalltalk.Facebook)});},
messageSends: ["setup", "basicLoad"]}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "setup",
fn: function () {
var self=this;
function $FB(){return smalltalk.FB||(typeof FB=="undefined"?nil:FB)}
return smalltalk.withContext(function($ctx1) { 
_st(window)._at_put_("fbAsyncInit",(function(){
return smalltalk.withContext(function($ctx2) {
return _st($FB())._init_(smalltalk.HashedCollection._fromPairs_([_st("appId").__minus_gt(_st(self)._appId()),_st("channelUrl").__minus_gt(_st(self)._channelUrl()),_st("status").__minus_gt(true),_st("xfbml").__minus_gt(true)]));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"setup",{},smalltalk.Facebook)});},
messageSends: ["at:put:", "init:", "->", "appId", "channelUrl"]}),
smalltalk.Facebook);



smalltalk.addClass('GooglePlus', smalltalk.AbstractSocialAPI, [], 'SocialAPI');


smalltalk.addClass('LinkedIn', smalltalk.AbstractSocialAPI, [], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "renderLoadOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._script();
_st($1)._at_put_("type","text/javascript");
$2=_st($1)._src_("http://platform.linkedin.com/in.js");
_st(_st($2)._asJQuery())._html_(_st(_st("api_key: ").__comma(_st(self)._appId())).__comma("\x0aauthorize: true"));
return self}, function($ctx1) {$ctx1.fill(self,"renderLoadOn:",{html:html},smalltalk.LinkedIn)});},
messageSends: ["html:", ",", "appId", "asJQuery", "at:put:", "script", "src:"]}),
smalltalk.LinkedIn);



smalltalk.addClass('SocialAPI', smalltalk.Object, ['facebook', 'linkedIn', 'googlePlus'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "facebook",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@facebook"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeFacebook();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"facebook",{},smalltalk.SocialAPI)});},
messageSends: ["ifNil:", "initializeFacebook"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "googlePlus",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@googlePlus"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeGooglePlus();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"googlePlus",{},smalltalk.SocialAPI)});},
messageSends: ["ifNil:", "initializeGooglePlus"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeFacebook",
fn: function () {
var self=this;
function $Facebook(){return smalltalk.Facebook||(typeof Facebook=="undefined"?nil:Facebook)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@facebook"]=_st($Facebook())._new();
$1=self["@facebook"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeFacebook",{},smalltalk.SocialAPI)});},
messageSends: ["new"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeGooglePlus",
fn: function () {
var self=this;
function $GooglePlus(){return smalltalk.GooglePlus||(typeof GooglePlus=="undefined"?nil:GooglePlus)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@googlePlus"]=_st($GooglePlus())._new();
$1=self["@googlePlus"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeGooglePlus",{},smalltalk.SocialAPI)});},
messageSends: ["new"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeLinkedIn",
fn: function () {
var self=this;
function $LinkedIn(){return smalltalk.LinkedIn||(typeof LinkedIn=="undefined"?nil:LinkedIn)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@linkedIn"]=_st($LinkedIn())._new();
$1=self["@linkedIn"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeLinkedIn",{},smalltalk.SocialAPI)});},
messageSends: ["new"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "linkedIn",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@linkedIn"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeLinkedIn();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"linkedIn",{},smalltalk.SocialAPI)});},
messageSends: ["ifNil:", "initializeLinkedIn"]}),
smalltalk.SocialAPI);



smalltalk.addPackage('Reactive');
smalltalk.addClass('RAnnouncement', smalltalk.Object, ['subject'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "subject",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@subject"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"subject",{},smalltalk.RAnnouncement)});},
messageSends: []}),
smalltalk.RAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "subject:",
fn: function (anObject) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@subject"]=anObject;
return self}, function($ctx1) {$ctx1.fill(self,"subject:",{anObject:anObject},smalltalk.RAnnouncement)});},
messageSends: []}),
smalltalk.RAnnouncement);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
fn: function (aSubject) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._subject_(aSubject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aSubject:aSubject},smalltalk.RAnnouncement.klass)});},
messageSends: ["subject:", "new", "yourself"]}),
smalltalk.RAnnouncement.klass);


smalltalk.addClass('RTaskChanged', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RTaskRemoved', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RTasksSorted', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RClient', smalltalk.Object, ['socket', 'uri', 'onOpen'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "initializeSocket",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._makeSocketOn_(_st(self)._uri());
_st($2)._onopen_(_st(self)._onOpen());
_st($2)._onclose_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st($2)._onmessage_((function(anEvent){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onMessage_(anEvent);
}, function($ctx2) {$ctx2.fillBlock({anEvent:anEvent},$ctx1)})}));
_st($2)._onerror_((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onError_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}));
$3=_st($2)._yourself();
self["@socket"]=$3;
$1=self["@socket"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeSocket",{},smalltalk.RClient)});},
messageSends: ["onopen:", "onOpen", "makeSocketOn:", "uri", "onclose:", "onmessage:", "onMessage:", "onerror:", "onError:", "yourself"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeURI",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@uri"]=_st(_st("ws://").__comma(_st(_st(window)._location())._hostname())).__comma(":21004/socket");
$1=self["@uri"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeURI",{},smalltalk.RClient)});},
messageSends: [",", "hostname", "location"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "makeSocketOn:",
fn: function (anUri) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new WebSocket(anUri);
return self}, function($ctx1) {$ctx1.fill(self,"makeSocketOn:",{anUri:anUri},smalltalk.RClient)});},
messageSends: []}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onError:",
fn: function (anException) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._halt();
return self}, function($ctx1) {$ctx1.fill(self,"onError:",{anException:anException},smalltalk.RClient)});},
messageSends: ["halt"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onMessage:",
fn: function (anEvent) {
var self=this;
var command;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
command=_st($MaplessModel())._fromJsonString_(_st(anEvent)._data());
_st(command)._react();
return self}, function($ctx1) {$ctx1.fill(self,"onMessage:",{anEvent:anEvent,command:command},smalltalk.RClient)});},
messageSends: ["fromJsonString:", "data", "react"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@onOpen"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@onOpen"]=(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})});
$1=self["@onOpen"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"onOpen",{},smalltalk.RClient)});},
messageSends: ["ifNil:"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@onOpen"]=aBlock;
return self}, function($ctx1) {$ctx1.fill(self,"onOpen:",{aBlock:aBlock},smalltalk.RClient)});},
messageSends: []}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "open",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._initializeSocket();
return self}, function($ctx1) {$ctx1.fill(self,"open",{},smalltalk.RClient)});},
messageSends: ["initializeSocket"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "send:",
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._socket())._send_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"send:",{aString:aString},smalltalk.RClient)});},
messageSends: ["send:", "socket"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "socket",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@socket"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeSocket();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"socket",{},smalltalk.RClient)});},
messageSends: ["ifNil:", "initializeSocket"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "uri",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@uri"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeURI();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"uri",{},smalltalk.RClient)});},
messageSends: ["ifNil:", "initializeURI"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "uri:",
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@uri"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"uri:",{aString:aString},smalltalk.RClient)});},
messageSends: []}),
smalltalk.RClient);



smalltalk.addClass('RModel', smalltalk.MaplessModel, [], 'Reactive');

smalltalk.addMethod(
smalltalk.method({
selector: "path",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return "api";
}, function($ctx1) {$ctx1.fill(self,"path",{},smalltalk.RModel.klass)});},
messageSends: []}),
smalltalk.RModel.klass);


smalltalk.addClass('RCommand', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "api",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._app())._session())._api();
return $1;
}, function($ctx1) {$ctx1.fill(self,"api",{},smalltalk.RCommand)});},
messageSends: ["api", "session", "app"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "execute",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._executeDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"execute",{},smalltalk.RCommand)});},
messageSends: ["executeDo:"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.RModel.fn.prototype._initialize.apply(_st(self), []);
_st(self)._sessionId_(_st(_st(_st(window)._app())._session())._id());
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.RCommand)});},
messageSends: ["initialize", "sessionId:", "id", "session", "app"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "notifyRemote",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._api())._send_(_st(self)._asJSONString());
return self}, function($ctx1) {$ctx1.fill(self,"notifyRemote",{},smalltalk.RCommand)});},
messageSends: ["send:", "asJSONString", "api"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfter:done:",
fn: function (x, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aBlock)._value();
return self}, function($ctx1) {$ctx1.fill(self,"onAfter:done:",{x:x,aBlock:aBlock},smalltalk.RCommand)});},
messageSends: ["value"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "onFail:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._halt();
return self}, function($ctx1) {$ctx1.fill(self,"onFail:",{aBlock:aBlock},smalltalk.RCommand)});},
messageSends: ["halt"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RCommand)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.RCommand);



smalltalk.addClass('RAddList', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/addList")),_st("type").__minus_gt("PUT"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RAddList)});},
messageSends: ["localSave", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RAddList);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._list());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddList)});},
messageSends: ["save:", "list"]}),
smalltalk.RAddList);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._list_(aList);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aList:aList},smalltalk.RAddList.klass)});},
messageSends: ["list:", "new", "yourself"]}),
smalltalk.RAddList.klass);


smalltalk.addClass('RAddTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._updateList_(_st(self)._list());
_st(self)._notifyRemote();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/addTask")),_st("type").__minus_gt("PUT"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RAddTask)});},
messageSends: ["localSave", "updateList:", "list", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddTask)});},
messageSends: ["save:", "task"]}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._addTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RAddTask)});},
messageSends: ["addTask:", "app"]}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "saveAndUpdateList:",
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._updateList_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"saveAndUpdateList:",{aList:aList},smalltalk.RAddTask)});},
messageSends: ["localSave", "updateList:"]}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "updateList:",
fn: function (aList) {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(aList)._addTask_(_st(_st(self)._task())._id());
_st($RStorage())._save_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"updateList:",{aList:aList},smalltalk.RAddTask)});},
messageSends: ["addTask:", "id", "task", "save:"]}),
smalltalk.RAddTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:in:",
fn: function (aTask, aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._task_(aTask);
_st($2)._list_(aList);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:in:",{aTask:aTask,aList:aList},smalltalk.RAddTask.klass)});},
messageSends: ["task:", "new", "list:", "yourself"]}),
smalltalk.RAddTask.klass);


smalltalk.addClass('RAddUser', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/addUser")),_st("type").__minus_gt("PUT"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RAddUser)});},
messageSends: ["localSave", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RAddUser);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._user());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddUser)});},
messageSends: ["save:", "user"]}),
smalltalk.RAddUser);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
fn: function (anUser) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._user_(anUser);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{anUser:anUser},smalltalk.RAddUser.klass)});},
messageSends: ["user:", "new", "yourself"]}),
smalltalk.RAddUser.klass);


smalltalk.addClass('RChangeClients', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._notifyRemote();
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RChangeClients)});},
messageSends: ["notifyRemote"]}),
smalltalk.RChangeClients);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeClients_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeClients)});},
messageSends: ["changeClients:", "app"]}),
smalltalk.RChangeClients);



smalltalk.addClass('RChangeList', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._notifyRemote();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/changeList")),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RChangeList)});},
messageSends: ["localSave", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RChangeList);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._list());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RChangeList)});},
messageSends: ["save:", "list"]}),
smalltalk.RChangeList);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeList_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeList)});},
messageSends: ["changeList:", "app"]}),
smalltalk.RChangeList);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._list_(aList);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aList:aList},smalltalk.RChangeList.klass)});},
messageSends: ["list:", "new", "yourself"]}),
smalltalk.RChangeList.klass);


smalltalk.addClass('RChangeTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._notifyRemote();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/changeTask")),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RChangeTask)});},
messageSends: ["localSave", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RChangeTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RChangeTask)});},
messageSends: ["save:", "task"]}),
smalltalk.RChangeTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeTask)});},
messageSends: ["changeTask:", "app"]}),
smalltalk.RChangeTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._task_(aTask);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aTask:aTask},smalltalk.RChangeTask.klass)});},
messageSends: ["task:", "new", "yourself"]}),
smalltalk.RChangeTask.klass);


smalltalk.addClass('RRemoveTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localRemove();
_st(self)._updateList_(_st(self)._list());
_st(self)._notifyRemote();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/removeTask")),_st("type").__minus_gt("DELETE"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RRemoveTask)});},
messageSends: ["localRemove", "updateList:", "list", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localRemove",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._delete_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localRemove",{},smalltalk.RRemoveTask)});},
messageSends: ["delete:", "task"]}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._removeTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RRemoveTask)});},
messageSends: ["removeTask:", "app"]}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "removeAndUpdateList:",
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localRemove();
_st(self)._updateList_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"removeAndUpdateList:",{aList:aList},smalltalk.RRemoveTask)});},
messageSends: ["localRemove", "updateList:"]}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "updateList:",
fn: function (aList) {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(aList)._removeTask_(_st(_st(self)._task())._id());
_st($RStorage())._save_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"updateList:",{aList:aList},smalltalk.RRemoveTask)});},
messageSends: ["removeTask:", "id", "task", "save:"]}),
smalltalk.RRemoveTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._task_(aTask);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aTask:aTask},smalltalk.RRemoveTask.klass)});},
messageSends: ["task:", "new", "yourself"]}),
smalltalk.RRemoveTask.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "for:list:",
fn: function (aTask, aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._task_(aTask);
_st($2)._list_(aList);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:list:",{aTask:aTask,aList:aList},smalltalk.RRemoveTask.klass)});},
messageSends: ["task:", "new", "list:", "yourself"]}),
smalltalk.RRemoveTask.klass);


smalltalk.addClass('RList', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
fn: function (aRTaskId) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._tasks())._add_(aRTaskId);
return self}, function($ctx1) {$ctx1.fill(self,"addTask:",{aRTaskId:aRTaskId},smalltalk.RList)});},
messageSends: ["add:", "tasks"]}),
smalltalk.RList);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
fn: function (aRTaskId) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._tasks())._remove_ifAbsent_(aRTaskId,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"removeTask:",{aRTaskId:aRTaskId},smalltalk.RList)});},
messageSends: ["remove:ifAbsent:", "tasks"]}),
smalltalk.RList);

smalltalk.addMethod(
smalltalk.method({
selector: "tasks",
fn: function () {
var self=this;
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=smalltalk.RModel.fn.prototype._tasks.apply(_st(self), []);
if(($receiver = $2) == nil || $receiver == undefined){
_st(self)._tasks_(_st($Array())._new());
$1=_st(self)._tasks();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"tasks",{},smalltalk.RList)});},
messageSends: ["ifNil:", "tasks:", "new", "tasks"]}),
smalltalk.RList);


smalltalk.addMethod(
smalltalk.method({
selector: "atId:do:",
fn: function (anId, aBlock) {
var self=this;
function $ModelReadError(){return smalltalk.ModelReadError||(typeof ModelReadError=="undefined"?nil:ModelReadError)}
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(_st(self)._path()).__comma("/getList?id=")).__comma(anId)),_st("type").__minus_gt("GET"),_st("cache").__minus_gt(false),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterRead_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st($ModelReadError())._signal_(_st(_st(_st("Could not read ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st($ModelReadError())._signal_(_st(_st(_st("Could not read ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"atId:do:",{anId:anId,aBlock:aBlock},smalltalk.RList.klass)});},
messageSends: ["ajax:", "->", ",", "path", "onAfterRead:done:", "signal:", "responseText", "name"]}),
smalltalk.RList.klass);


smalltalk.addClass('RProspect', smalltalk.RModel, [], 'Reactive');


smalltalk.addClass('RTask', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.RModel.fn.prototype._initialize.apply(_st(self), []);
_st(self)._isCompleted_(false);
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.RTask)});},
messageSends: ["initialize", "isCompleted:"]}),
smalltalk.RTask);



smalltalk.addClass('RUser', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addList:",
fn: function (aRList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._lists())._add_(aRList);
return self}, function($ctx1) {$ctx1.fill(self,"addList:",{aRList:aRList},smalltalk.RUser)});},
messageSends: ["add:", "lists"]}),
smalltalk.RUser);

smalltalk.addMethod(
smalltalk.method({
selector: "lists",
fn: function () {
var self=this;
function $OrderedCollection(){return smalltalk.OrderedCollection||(typeof OrderedCollection=="undefined"?nil:OrderedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=smalltalk.RModel.fn.prototype._lists.apply(_st(self), []);
if(($receiver = $2) == nil || $receiver == undefined){
_st(self)._lists_(_st($OrderedCollection())._new());
$1=_st(self)._lists();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"lists",{},smalltalk.RUser)});},
messageSends: ["ifNil:", "lists:", "new", "lists"]}),
smalltalk.RUser);



smalltalk.addClass('RSession', smalltalk.Object, ['id', 'user', 'api', 'social'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "api",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@api"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeAPI();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"api",{},smalltalk.RSession)});},
messageSends: ["ifNil:", "initializeAPI"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSONString",
fn: function () {
var self=this;
function $HasedCollection(){return smalltalk.HasedCollection||(typeof HasedCollection=="undefined"?nil:HasedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st($HasedCollection())._new();
_st($2)._at_put_("id",_st(self)._id());
_st($2)._at_put_("userId",_st(_st(self)._user())._id());
$3=_st($2)._asJSONString();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSONString",{},smalltalk.RSession)});},
messageSends: ["at:put:", "id", "new", "user", "asJSONString"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "getUser",
fn: function () {
var self=this;
var anId,guy,newUser,newList;
function $RUser(){return smalltalk.RUser||(typeof RUser=="undefined"?nil:RUser)}
function $RList(){return smalltalk.RList||(typeof RList=="undefined"?nil:RList)}
function $RAddUser(){return smalltalk.RAddUser||(typeof RAddUser=="undefined"?nil:RAddUser)}
function $RAddList(){return smalltalk.RAddList||(typeof RAddList=="undefined"?nil:RAddList)}
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$4,$1;
anId=_st(_st(window)._localStorage())._getItem_("userId");
guy=_st(_st(window)._localStorage())._getItem_(anId);
$2=_st(guy)._isNil();
if(smalltalk.assert($2)){
newUser=_st($RUser())._new();
newUser;
$3=_st($RList())._new();
_st($3)._id_("95faef2d-ffc8-4444-d2c7-3e9bbf97c415");
_st($3)._name_("First list of things to get done");
$4=_st($3)._yourself();
newList=$4;
newList;
_st(newUser)._listId_(_st(newList)._id());
_st(_st(window)._localStorage())._setItem_put_("userId",_st(newUser)._id());
_st(_st($RAddUser())._for_(newUser))._execute();
_st(_st($RAddList())._for_(newList))._execute();
$1=newUser;
} else {
$1=_st($MaplessModel())._fromJsonString_(guy);
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"getUser",{anId:anId,guy:guy,newUser:newUser,newList:newList},smalltalk.RSession)});},
messageSends: ["getItem:", "localStorage", "ifTrue:ifFalse:", "new", "id:", "name:", "yourself", "listId:", "id", "setItem:put:", "execute", "for:", "fromJsonString:", "isNil"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "id",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@id"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeID();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"id",{},smalltalk.RSession)});},
messageSends: ["ifNil:", "initializeID"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeAPI",
fn: function () {
var self=this;
function $RClient(){return smalltalk.RClient||(typeof RClient=="undefined"?nil:RClient)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@api"]=_st($RClient())._new();
$1=self["@api"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeAPI",{},smalltalk.RSession)});},
messageSends: ["new"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeID",
fn: function () {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@id"]=_st($MaplessModel())._newUUID();
$1=self["@id"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeID",{},smalltalk.RSession)});},
messageSends: ["newUUID"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeSocial",
fn: function () {
var self=this;
function $SocialAPI(){return smalltalk.SocialAPI||(typeof SocialAPI=="undefined"?nil:SocialAPI)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@social"]=_st($SocialAPI())._new();
$1=self["@social"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeSocial",{},smalltalk.RSession)});},
messageSends: ["new"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeUser",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@user"]=_st(self)._getUser();
$1=self["@user"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeUser",{},smalltalk.RSession)});},
messageSends: ["getUser"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._initializeID();
$1=_st(_st(self)._social())._facebook();
_st($1)._appId_("318516538288855");
_st($1)._channelUrl_("http://tasks.flowingconcept.com/service/static/facebook/channel.html");
$2=_st($1)._yourself();
_st(_st(_st(self)._social())._linkedIn())._appId_("cekl05jzyc9p");
_st(_st(_st(self)._social())._googlePlus())._appId_("durable-ripsaw-381");
return self}, function($ctx1) {$ctx1.fill(self,"onOpen",{},smalltalk.RSession)});},
messageSends: ["initializeID", "appId:", "facebook", "social", "channelUrl:", "yourself", "linkedIn", "googlePlus"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "social",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@social"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeSocial();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"social",{},smalltalk.RSession)});},
messageSends: ["ifNil:", "initializeSocial"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "user",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@user"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeUser();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"user",{},smalltalk.RSession)});},
messageSends: ["ifNil:", "initializeUser"]}),
smalltalk.RSession);



smalltalk.addClass('RStorage', smalltalk.Object, [], 'Reactive');

smalltalk.addMethod(
smalltalk.method({
selector: "delete:",
fn: function (aMaplessModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._localDelete();
return self}, function($ctx1) {$ctx1.fill(self,"delete:",{aMaplessModel:aMaplessModel},smalltalk.RStorage.klass)});},
messageSends: ["localDelete"]}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "delete:do:",
fn: function (aMaplessModel, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._remoteDeleteDo_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(aMaplessModel)._localDelete();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"delete:do:",{aMaplessModel:aMaplessModel,aBlock:aBlock},smalltalk.RStorage.klass)});},
messageSends: ["remoteDeleteDo:", "localDelete"]}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "save:",
fn: function (aMaplessModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"save:",{aMaplessModel:aMaplessModel},smalltalk.RStorage.klass)});},
messageSends: ["localSave"]}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "save:do:",
fn: function (aMaplessModel, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=aMaplessModel;
_st($1)._localSave();
$2=_st($1)._remoteSaveDo_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"save:do:",{aMaplessModel:aMaplessModel,aBlock:aBlock},smalltalk.RStorage.klass)});},
messageSends: ["localSave", "remoteSaveDo:"]}),
smalltalk.RStorage.klass);


smalltalk.addClass('RWidget', smalltalk.Widget, ['model', 'announcer'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "announce:",
fn: function (anAnnouncement) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._announcer())._announce_(anAnnouncement);
return $1;
}, function($ctx1) {$ctx1.fill(self,"announce:",{anAnnouncement:anAnnouncement},smalltalk.RWidget)});},
messageSends: ["announce:", "announcer"]}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "announcer",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@announcer"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeAnnouncer();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"announcer",{},smalltalk.RWidget)});},
messageSends: ["ifNil:", "initializeAnnouncer"]}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeAnnouncer",
fn: function () {
var self=this;
function $Announcer(){return smalltalk.Announcer||(typeof Announcer=="undefined"?nil:Announcer)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@announcer"]=_st($Announcer())._new();
$1=self["@announcer"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeAnnouncer",{},smalltalk.RWidget)});},
messageSends: ["new"]}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "model",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@model"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"model",{},smalltalk.RWidget)});},
messageSends: []}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "model:",
fn: function (aModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@model"]=aModel;
return self}, function($ctx1) {$ctx1.fill(self,"model:",{aModel:aModel},smalltalk.RWidget)});},
messageSends: []}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "on:do:",
fn: function (anAnnouncement, aReaction) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._announcer())._on_do_(anAnnouncement,aReaction);
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:do:",{anAnnouncement:anAnnouncement,aReaction:aReaction},smalltalk.RWidget)});},
messageSends: ["on:do:", "announcer"]}),
smalltalk.RWidget);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
fn: function (aModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._model_(aModel);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aModel:aModel},smalltalk.RWidget.klass)});},
messageSends: ["model:", "new", "yourself"]}),
smalltalk.RWidget.klass);


smalltalk.addClass('ListWidget', smalltalk.RWidget, ['list', 'input', 'items'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addTask",
fn: function () {
var self=this;
var newTask,newTaskWidget;
function $RTask(){return smalltalk.RTask||(typeof RTask=="undefined"?nil:RTask)}
function $RAddTask(){return smalltalk.RAddTask||(typeof RAddTask=="undefined"?nil:RAddTask)}
function $TaskWidget(){return smalltalk.TaskWidget||(typeof TaskWidget=="undefined"?nil:TaskWidget)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st($RTask())._new();
_st($1)._initializeCreatedOn();
_st($1)._description_(_st(_st(self["@input"])._asJQuery())._val());
$2=_st($1)._yourself();
newTask=$2;
_st(_st($RAddTask())._for_in_(newTask,self["@model"]))._execute();
newTaskWidget=_st($TaskWidget())._for_(newTask);
_st(_st(self)._items())._add_(newTaskWidget);
_st(self)._renderTask_(newTaskWidget);
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"addTask",{newTask:newTask,newTaskWidget:newTaskWidget},smalltalk.ListWidget)});},
messageSends: ["initializeCreatedOn", "new", "description:", "val", "asJQuery", "yourself", "execute", "for:in:", "for:", "add:", "items", "renderTask:", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
fn: function (anAddTask) {
var self=this;
var newTaskWidget;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(anAddTask)._saveAndUpdateList_(self["@model"]);
newTaskWidget=_st(self)._newTaskFor_(_st(anAddTask)._task());
_st(_st(self)._items())._add_(newTaskWidget);
_st(self)._renderTask_(newTaskWidget);
_st(newTaskWidget)._hide();
$1=_st(self)._isAllOrToDo();
if(smalltalk.assert($1)){
_st(newTaskWidget)._show();
};
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"addTask:",{anAddTask:anAddTask,newTaskWidget:newTaskWidget},smalltalk.ListWidget)});},
messageSends: ["saveAndUpdateList:", "newTaskFor:", "task", "add:", "items", "renderTask:", "hide", "ifTrue:", "show", "isAllOrToDo", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeList:",
fn: function (aChageList) {
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._model_(_st(aChageList)._list());
_st(_st(_st(self["@list"])._asJQuery())._children_("li"))._remove();
_st(self)._initializeItems();
_st(self)._renderItemsOn_(_st($HTMLCanvas())._onJQuery_(_st(self["@list"])._asJQuery()));
return self}, function($ctx1) {$ctx1.fill(self,"changeList:",{aChageList:aChageList},smalltalk.ListWidget)});},
messageSends: ["model:", "list", "remove", "children:", "asJQuery", "initializeItems", "renderItemsOn:", "onJQuery:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeTask:",
fn: function (aChangeTask) {
var self=this;
var changedTask;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
changedTask=_st(self)._getTaskWidgetFor_(_st(aChangeTask)._task());
$1=changedTask;
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(changedTask)._changedTask_(_st(aChangeTask)._task());
};
$2=_st(self)._isDone();
if(smalltalk.assert($2)){
_st(changedTask)._showIfDone();
};
$3=_st(self)._isToDo();
if(smalltalk.assert($3)){
_st(changedTask)._showIfToDo();
};
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"changeTask:",{aChangeTask:aChangeTask,changedTask:changedTask},smalltalk.ListWidget)});},
messageSends: ["getTaskWidgetFor:", "task", "ifNotNil:", "changedTask:", "ifTrue:", "showIfDone", "isDone", "showIfToDo", "isToDo", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "currentTaskIds",
fn: function () {
var self=this;
var children,elements;
function $OrderedCollection(){return smalltalk.OrderedCollection||(typeof OrderedCollection=="undefined"?nil:OrderedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $1;
children=_st(_st(self["@list"])._asJQuery())._children();
elements=_st($OrderedCollection())._new();
_st((0))._to_do_(_st(_st(children)._length()).__minus((1)),(function(i){
return smalltalk.withContext(function($ctx2) {
return _st(elements)._add_(_st(_st(children)._at_(i))._id());
}, function($ctx2) {$ctx2.fillBlock({i:i},$ctx1)})}));
$1=elements;
return $1;
}, function($ctx1) {$ctx1.fill(self,"currentTaskIds",{children:children,elements:elements},smalltalk.ListWidget)});},
messageSends: ["children", "asJQuery", "new", "to:do:", "-", "length", "add:", "id", "at:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getAll",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._items();
return $1;
}, function($ctx1) {$ctx1.fill(self,"getAll",{},smalltalk.ListWidget)});},
messageSends: ["items"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getDone",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._getAll())._select_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._isCompleted();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"getDone",{},smalltalk.ListWidget)});},
messageSends: ["select:", "isCompleted", "getAll"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTaskAt:",
fn: function (anId) {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($MaplessModel())._localLoadAt_(anId);
return $1;
}, function($ctx1) {$ctx1.fill(self,"getTaskAt:",{anId:anId},smalltalk.ListWidget)});},
messageSends: ["localLoadAt:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTaskWidgetFor:",
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._items())._detect_ifNone_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(e)._model())._id()).__eq(_st(aTask)._id());
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"getTaskWidgetFor:",{aTask:aTask},smalltalk.ListWidget)});},
messageSends: ["detect:ifNone:", "=", "id", "model", "items"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTasks",
fn: function () {
var self=this;
function $JSON(){return smalltalk.JSON||(typeof JSON=="undefined"?nil:JSON)}
function $RTask(){return smalltalk.RTask||(typeof RTask=="undefined"?nil:RTask)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$1=_st(_st(self["@model"])._tasks())._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
$2=_st(each)._isString();
if(smalltalk.assert($2)){
return _st(self)._getTaskAt_(each);
} else {
return _st($RTask())._fromJsonString_(_st($JSON())._stringify_(each));
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"getTasks",{},smalltalk.ListWidget)});},
messageSends: ["collect:", "ifTrue:ifFalse:", "getTaskAt:", "fromJsonString:", "stringify:", "isString", "tasks"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getToDo",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._getAll())._reject_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._isCompleted();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"getToDo",{},smalltalk.ListWidget)});},
messageSends: ["reject:", "isCompleted", "getAll"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeItems",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@items"]=_st(self)._makeItems();
$1=self["@items"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeItems",{},smalltalk.ListWidget)});},
messageSends: ["makeItems"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isAll",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#all")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isAll",{},smalltalk.ListWidget)});},
messageSends: ["is:", "asJQuery"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isAllOrToDo",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._isAll())._or_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._isToDo();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"isAllOrToDo",{},smalltalk.ListWidget)});},
messageSends: ["or:", "isToDo", "isAll"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isDone",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#done")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isDone",{},smalltalk.ListWidget)});},
messageSends: ["is:", "asJQuery"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isToDo",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#todo")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isToDo",{},smalltalk.ListWidget)});},
messageSends: ["is:", "asJQuery"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "items",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@items"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeItems();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"items",{},smalltalk.ListWidget)});},
messageSends: ["ifNil:", "initializeItems"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "makeItems",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._getTasks())._collect_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(self)._newTaskFor_(e);
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"makeItems",{},smalltalk.ListWidget)});},
messageSends: ["collect:", "newTaskFor:", "getTasks"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "newTaskFor:",
fn: function (aTaskModel) {
var self=this;
function $RTaskChanged(){return smalltalk.RTaskChanged||(typeof RTaskChanged=="undefined"?nil:RTaskChanged)}
function $TaskWidget(){return smalltalk.TaskWidget||(typeof TaskWidget=="undefined"?nil:TaskWidget)}
function $RTaskRemoved(){return smalltalk.RTaskRemoved||(typeof RTaskRemoved=="undefined"?nil:RTaskRemoved)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st($TaskWidget())._for_(aTaskModel);
_st($2)._on_do_($RTaskChanged(),(function(ann){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onTaskChanged_(ann);
}, function($ctx2) {$ctx2.fillBlock({ann:ann},$ctx1)})}));
_st($2)._on_do_($RTaskRemoved(),(function(ann){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onTaskRemove_(ann);
}, function($ctx2) {$ctx2.fillBlock({ann:ann},$ctx1)})}));
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"newTaskFor:",{aTaskModel:aTaskModel},smalltalk.ListWidget)});},
messageSends: ["on:do:", "onTaskChanged:", "for:", "onTaskRemove:", "yourself"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onAdd",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(_st(self["@input"])._asJQuery())._val())._isNil())._or_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(self["@input"])._asJQuery())._val())._isEmpty();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(! smalltalk.assert($1)){
_st(self)._addTask();
_st(_st(self["@input"])._asJQuery())._val_("");
_st(_st(self["@input"])._asJQuery())._focus();
};
return self}, function($ctx1) {$ctx1.fill(self,"onAdd",{},smalltalk.ListWidget)});},
messageSends: ["ifFalse:", "addTask", "val:", "asJQuery", "focus", "or:", "isEmpty", "val", "isNil"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onAllTasks",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._items())._do_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._show();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
_st(self)._unselectTabs();
_st(_st("#all")._asJQuery())._addClass_("selectedTab");
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onAllTasks",{},smalltalk.ListWidget)});},
messageSends: ["do:", "show", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDoneTasks",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._items())._do_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._showIfDone();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
_st(self)._unselectTabs();
_st(_st("#done")._asJQuery())._addClass_("selectedTab");
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onDoneTasks",{},smalltalk.ListWidget)});},
messageSends: ["do:", "showIfDone", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyUp:",
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($1)){
_st(self)._onAdd();
};
return self}, function($ctx1) {$ctx1.fill(self,"onKeyUp:",{e:e},smalltalk.ListWidget)});},
messageSends: ["ifTrue:", "onAdd", "=", "keyCode"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onSorted:",
fn: function (anEvent) {
var self=this;
function $RChangeList(){return smalltalk.RChangeList||(typeof RChangeList=="undefined"?nil:RChangeList)}
function $RTasksSorted(){return smalltalk.RTasksSorted||(typeof RTasksSorted=="undefined"?nil:RTasksSorted)}
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._tasks_(_st(self)._currentTaskIds());
_st(_st($RChangeList())._for_(self["@model"]))._execute();
_st(self)._announce_(_st($RTasksSorted())._for_(self));
return self}, function($ctx1) {$ctx1.fill(self,"onSorted:",{anEvent:anEvent},smalltalk.ListWidget)});},
messageSends: ["tasks:", "currentTaskIds", "execute", "for:", "announce:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskChanged:",
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onTaskChanged:",{ann:ann},smalltalk.ListWidget)});},
messageSends: ["updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskRemove:",
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._removeTask_(_st(_st(_st(ann)._subject())._model())._id());
_st(self["@model"])._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"onTaskRemove:",{ann:ann},smalltalk.ListWidget)});},
messageSends: ["removeTask:", "id", "model", "subject", "localSave"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onToDoTasks",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._items())._do_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._showIfToDo();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
_st(self)._unselectTabs();
_st(_st("#todo")._asJQuery())._addClass_("selectedTab");
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onToDoTasks",{},smalltalk.ListWidget)});},
messageSends: ["do:", "showIfToDo", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
fn: function (aRemoveTask) {
var self=this;
var removedTask;
return smalltalk.withContext(function($ctx1) { 
var $1;
removedTask=_st(self)._getTaskWidgetFor_(_st(aRemoveTask)._task());
_st(aRemoveTask)._removeAndUpdateList_(self["@model"]);
$1=removedTask;
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(removedTask)._remove();
_st(_st(self)._items())._remove_ifAbsent_(removedTask,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
};
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"removeTask:",{aRemoveTask:aRemoveTask,removedTask:removedTask},smalltalk.ListWidget)});},
messageSends: ["getTaskWidgetFor:", "task", "removeAndUpdateList:", "ifNotNil:", "remove", "remove:ifAbsent:", "items", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "render",
fn: function () {
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._renderOn_(_st($HTMLCanvas())._onJQuery_(_st("#wrapper")._asJQuery()));
_st(_st("#wrapper")._asJQuery())._css_val_("min-height","50px");
return self}, function($ctx1) {$ctx1.fill(self,"render",{},smalltalk.ListWidget)});},
messageSends: ["renderOn:", "onJQuery:", "asJQuery", "css:val:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderAddTaskOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$6,$7,$8,$4,$2;
$1=_st(html)._div();
_st($1)._class_("addTaskWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._id_("addTask");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
$5=_st(html)._input();
_st($5)._at_put_("placeholder","Something else to get done?");
_st($5)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onKeyUp_(e);
}, function($ctx4) {$ctx4.fillBlock({e:e},$ctx1)})}));
$6=_st($5)._yourself();
self["@input"]=$6;
self["@input"];
$7=_st(html)._a();
_st($7)._id_("addTaskButton");
_st($7)._onClick_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onAdd();
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$8=_st($7)._with_("+");
return $8;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderAddTaskOn:",{html:html},smalltalk.ListWidget)});},
messageSends: ["class:", "div", "with:", "id:", "at:put:", "input", "onKeyUp:", "yourself", "a", "onClick:", "onAdd"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderItemsOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self["@list"])._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(self)._items())._do_((function(e){
return smalltalk.withContext(function($ctx3) {
_st(e)._renderOn_(html);
$1=_st(self)._isDone();
if(smalltalk.assert($1)){
_st(e)._showIfDone();
};
$2=_st(self)._isToDo();
if(smalltalk.assert($2)){
return _st(e)._showIfToDo();
};
}, function($ctx3) {$ctx3.fillBlock({e:e},$ctx1)})}));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderItemsOn:",{html:html},smalltalk.ListWidget)});},
messageSends: ["with:", "do:", "renderOn:", "ifTrue:", "showIfDone", "isDone", "showIfToDo", "isToDo", "items"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListHeaderOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$5,$6,$2;
$1=_st(html)._div();
_st($1)._id_("listHeader");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._id_("listName");
$4=_st($3)._with_(_st(self["@model"])._name());
$4;
$5=_st(html)._div();
_st($5)._id_("details");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(self)._renderMetricsOn_(html);
return _st(self)._renderTabsOn_(html);
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $6;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderListHeaderOn:",{html:html},smalltalk.ListWidget)});},
messageSends: ["id:", "div", "with:", "name", "renderMetricsOn:", "renderTabsOn:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@list"]=_st(html)._ul();
_st(_st(self["@list"])._asJQuery())._disableSelection();
_st(_st(self["@list"])._asJQuery())._sortable_(smalltalk.HashedCollection._fromPairs_([_st("update").__minus_gt((function(e,ui){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onSorted_(e);
}, function($ctx2) {$ctx2.fillBlock({e:e,ui:ui},$ctx1)})}))]));
_st(self)._renderItemsOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderListOn:",{html:html},smalltalk.ListWidget)});},
messageSends: ["ul", "disableSelection", "asJQuery", "sortable:", "->", "onSorted:", "renderItemsOn:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderMetricsOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$5,$6,$2;
$1=_st(html)._div();
_st($1)._id_("metrics");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._span();
_st($3)._id_("tasks");
$4=_st($3)._with_("x tasks");
$4;
$5=_st(html)._span();
_st($5)._id_("clients");
$6=_st($5)._with_("1 guy (you)");
return $6;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderMetricsOn:",{html:html},smalltalk.ListWidget)});},
messageSends: ["id:", "div", "with:", "span"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._div();
_st($1)._id_("listWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._renderListHeaderOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self)._renderListOn_(html);
_st(self)._renderAddTaskOn_(html);
_st(self)._onAllTasks();
return self}, function($ctx1) {$ctx1.fill(self,"renderOn:",{html:html},smalltalk.ListWidget)});},
messageSends: ["id:", "div", "with:", "renderListHeaderOn:", "renderListOn:", "renderAddTaskOn:", "onAllTasks"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTabsOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5,$6;
$1=_st(html)._div();
_st($1)._id_("done");
_st($1)._class_("tab");
_st($1)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onDoneTasks();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(html)._span_("Done");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$3=_st(html)._div();
_st($3)._id_("todo");
_st($3)._class_("tab");
_st($3)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onToDoTasks();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(html)._span_("To-Do");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$5=_st(html)._div();
_st($5)._id_("all");
_st($5)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAllTasks();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st($5)._class_("tab");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(html)._span_("All");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderTabsOn:",{html:html},smalltalk.ListWidget)});},
messageSends: ["id:", "div", "class:", "onClick:", "onDoneTasks", "with:", "span:", "onToDoTasks", "onAllTasks"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTask:",
fn: function (aTaskWidget) {
var self=this;
var html;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
html=_st($HTMLCanvas())._onJQuery_(_st(self["@list"])._asJQuery());
_st(aTaskWidget)._renderOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderTask:",{aTaskWidget:aTaskWidget,html:html},smalltalk.ListWidget)});},
messageSends: ["onJQuery:", "asJQuery", "renderOn:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "unselectTabs",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st("#details")._asJQuery())._find_(".tab"))._removeClass_("selectedTab");
return self}, function($ctx1) {$ctx1.fill(self,"unselectTabs",{},smalltalk.ListWidget)});},
messageSends: ["removeClass:", "find:", "asJQuery"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateAllTasksMetric",
fn: function () {
var self=this;
var many;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
many=_st(_st(self)._getAll())._size();
$1=_st(many)._isZero();
if(smalltalk.assert($1)){
$2=_st(_st("#tasks")._asJQuery())._text_("Life is too short for no doing it!");
return $2;
};
$3=_st(many).__gt((1));
if(smalltalk.assert($3)){
_st(_st("#tasks")._asJQuery())._text_(_st(_st(many)._asString()).__comma(" tasks total"));
} else {
_st(_st("#tasks")._asJQuery())._text_("only 1 task!");
};
return self}, function($ctx1) {$ctx1.fill(self,"updateAllTasksMetric",{many:many},smalltalk.ListWidget)});},
messageSends: ["size", "getAll", "ifTrue:", "text:", "asJQuery", "isZero", "ifTrue:ifFalse:", ",", "asString", ">"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateDoneTasksMetric",
fn: function () {
var self=this;
var many;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5;
many=_st(_st(self)._getDone())._size();
$1=_st(many)._isZero();
if(smalltalk.assert($1)){
$2=_st(_st("#tasks")._asJQuery())._text_("Come on! let's ship something!");
return $2;
};
$3=_st(_st(_st(self)._getAll())._size()).__eq(many);
if(smalltalk.assert($3)){
$4=_st(_st("#tasks")._asJQuery())._text_(_st(_st(many)._asString()).__comma(" tasks done, OMG it's all done!"));
return $4;
};
$5=_st(many).__gt((1));
if(smalltalk.assert($5)){
_st(_st("#tasks")._asJQuery())._text_(_st(_st(many)._asString()).__comma(" tasks done"));
} else {
_st(_st("#tasks")._asJQuery())._text_("only 1 task done");
};
return self}, function($ctx1) {$ctx1.fill(self,"updateDoneTasksMetric",{many:many},smalltalk.ListWidget)});},
messageSends: ["size", "getDone", "ifTrue:", "text:", "asJQuery", "isZero", ",", "asString", "=", "getAll", "ifTrue:ifFalse:", ">"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTasksMetric",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5,$6;
$1=_st(self)._isDone();
if(smalltalk.assert($1)){
$2=_st(self)._updateDoneTasksMetric();
return $2;
};
$3=_st(self)._isToDo();
if(smalltalk.assert($3)){
$4=_st(self)._updateToDoTasksMetric();
return $4;
};
$5=_st(self)._isAll();
if(smalltalk.assert($5)){
$6=_st(self)._updateAllTasksMetric();
return $6;
};
return self}, function($ctx1) {$ctx1.fill(self,"updateTasksMetric",{},smalltalk.ListWidget)});},
messageSends: ["ifTrue:", "updateDoneTasksMetric", "isDone", "updateToDoTasksMetric", "isToDo", "updateAllTasksMetric", "isAll"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateToDoTasksMetric",
fn: function () {
var self=this;
var many;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
many=_st(_st(self)._getToDo())._size();
$1=_st(many)._isZero();
if(smalltalk.assert($1)){
$2=_st(_st("#tasks")._asJQuery())._text_("Let's do something?");
return $2;
};
$3=_st(many).__gt((1));
if(smalltalk.assert($3)){
_st(_st("#tasks")._asJQuery())._text_(_st(_st(many)._asString()).__comma(" tasks to do"));
} else {
_st(_st("#tasks")._asJQuery())._text_("only 1 task to do");
};
return self}, function($ctx1) {$ctx1.fill(self,"updateToDoTasksMetric",{many:many},smalltalk.ListWidget)});},
messageSends: ["size", "getToDo", "ifTrue:", "text:", "asJQuery", "isZero", "ifTrue:ifFalse:", ",", "asString", ">"]}),
smalltalk.ListWidget);



smalltalk.addClass('ReactiveWidget', smalltalk.RWidget, ['list', 'session'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
fn: function (anAddTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._addTask_(anAddTask);
return self}, function($ctx1) {$ctx1.fill(self,"addTask:",{anAddTask:anAddTask},smalltalk.ReactiveWidget)});},
messageSends: ["addTask:", "list"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeClients:",
fn: function (aChangeClients) {
var self=this;
var clients;
return smalltalk.withContext(function($ctx1) { 
var $1;
clients=_st(aChangeClients)._clients();
$1=_st(clients).__gt((1));
if(smalltalk.assert($1)){
_st(_st("#clients")._asJQuery())._text_(_st(_st(clients)._asString()).__comma(" guys"));
} else {
_st(_st("#clients")._asJQuery())._text_("1 guy (you)");
};
_st(_st("#clients")._asJQuery())._effect_op_duration_("pulsate",smalltalk.HashedCollection._fromPairs_([_st("times").__minus_gt((3))]),(500));
return self}, function($ctx1) {$ctx1.fill(self,"changeClients:",{aChangeClients:aChangeClients,clients:clients},smalltalk.ReactiveWidget)});},
messageSends: ["clients", "ifTrue:ifFalse:", "text:", ",", "asString", "asJQuery", ">", "effect:op:duration:", "->"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeList:",
fn: function (aChageList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._changeList_(aChageList);
return self}, function($ctx1) {$ctx1.fill(self,"changeList:",{aChageList:aChageList},smalltalk.ReactiveWidget)});},
messageSends: ["changeList:", "list"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeTask:",
fn: function (aChangeTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._changeTask_(aChangeTask);
return self}, function($ctx1) {$ctx1.fill(self,"changeTask:",{aChangeTask:aChangeTask},smalltalk.ReactiveWidget)});},
messageSends: ["changeTask:", "list"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getListAt:",
fn: function (anId) {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($MaplessModel())._localLoadAt_(anId);
return $1;
}, function($ctx1) {$ctx1.fill(self,"getListAt:",{anId:anId},smalltalk.ReactiveWidget)});},
messageSends: ["localLoadAt:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "hasTip",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._localStorage())._getItem_("showTipAtStart"))._isNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"hasTip",{},smalltalk.ReactiveWidget)});},
messageSends: ["isNil", "getItem:", "localStorage"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeListDo:",
fn: function (aBlock) {
var self=this;
function $ListWidget(){return smalltalk.ListWidget||(typeof ListWidget=="undefined"?nil:ListWidget)}
function $JSON(){return smalltalk.JSON||(typeof JSON=="undefined"?nil:JSON)}
function $RTask(){return smalltalk.RTask||(typeof RTask=="undefined"?nil:RTask)}
function $RList(){return smalltalk.RList||(typeof RList=="undefined"?nil:RList)}
function $RTasksSorted(){return smalltalk.RTasksSorted||(typeof RTasksSorted=="undefined"?nil:RTasksSorted)}
function $RTaskRemoved(){return smalltalk.RTaskRemoved||(typeof RTaskRemoved=="undefined"?nil:RTaskRemoved)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
self["@list"]=_st($ListWidget())._new();
_st($RList())._atId_do_("95faef2d-ffc8-4444-d2c7-3e9bbf97c415",(function(aList){
return smalltalk.withContext(function($ctx2) {
_st(self["@list"])._model_(aList);
_st(_st(aList)._tasks())._do_((function(each){
return smalltalk.withContext(function($ctx3) {
return _st(_st($RTask())._fromJsonString_(_st($JSON())._stringify_(each)))._localSave();
}, function($ctx3) {$ctx3.fillBlock({each:each},$ctx1)})}));
return _st(aBlock)._value();
}, function($ctx2) {$ctx2.fillBlock({aList:aList},$ctx1)})}));
$2=self["@list"];
_st($2)._on_do_($RTasksSorted(),(function(ann){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onListChanged_(ann);
}, function($ctx2) {$ctx2.fillBlock({ann:ann},$ctx1)})}));
_st($2)._on_do_($RTaskRemoved(),(function(ann){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onTaskRemoved_(ann);
}, function($ctx2) {$ctx2.fillBlock({ann:ann},$ctx1)})}));
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeListDo:",{aBlock:aBlock},smalltalk.ReactiveWidget)});},
messageSends: ["new", "atId:do:", "model:", "do:", "localSave", "fromJsonString:", "stringify:", "tasks", "value", "on:do:", "onListChanged:", "onTaskRemoved:", "yourself"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeSession",
fn: function () {
var self=this;
function $RSession(){return smalltalk.RSession||(typeof RSession=="undefined"?nil:RSession)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@session"]=_st($RSession())._new();
$1=self["@session"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeSession",{},smalltalk.ReactiveWidget)});},
messageSends: ["new"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isLocalhost",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._location())._hostname())._match_("localhost");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isLocalhost",{},smalltalk.ReactiveWidget)});},
messageSends: ["match:", "hostname", "location"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isProduction",
fn: function () {
var self=this;
function $Browser(){return smalltalk.Browser||(typeof Browser=="undefined"?nil:Browser)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Browser())._isNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isProduction",{},smalltalk.ReactiveWidget)});},
messageSends: ["isNil"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isPublished",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._isLocalhost())._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isPublished",{},smalltalk.ReactiveWidget)});},
messageSends: ["not", "isLocalhost"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isSociallyLoggedIn",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isSociallyLoggedIn",{},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "list",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@list"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"list",{},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "listDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@list"];
$1=_st($2)._ifNil_ifNotNil_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._initializeListDo_(aBlock);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}),aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"listDo:",{aBlock:aBlock},smalltalk.ReactiveWidget)});},
messageSends: ["ifNil:ifNotNil:", "initializeListDo:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "loadFacebookSDK",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
(function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));;
return self}, function($ctx1) {$ctx1.fill(self,"loadFacebookSDK",{},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onGotIt",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._localStorage())._setItem_put_("showTipAtStart",false);
return self}, function($ctx1) {$ctx1.fill(self,"onGotIt",{},smalltalk.ReactiveWidget)});},
messageSends: ["setItem:put:", "localStorage"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onListChanged:",
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"onListChanged:",{ann:ann},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNay",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._asJQuery())._scrollTop_((0));
_st(_st("#nayWrapper")._asJQuery())._slideDown_(smalltalk.HashedCollection._fromPairs_([_st("complete").__minus_gt((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st("#feedback")._asJQuery())._find_("textarea"))._focus();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"onNay",{},smalltalk.ReactiveWidget)});},
messageSends: ["scrollTop:", "asJQuery", "slideDown:", "->", "focus", "find:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNayKeyUp:",
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($1)){
_st(_st("#nayWrapper")._asJQuery())._slideUp();
};
$2=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($2)){
_st(self)._onOkNay();
};
return self}, function($ctx1) {$ctx1.fill(self,"onNayKeyUp:",{e:e},smalltalk.ReactiveWidget)});},
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode", "onOkNay"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNayTextAreaKeyUp:",
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($1)){
_st(_st("#nayWrapper")._asJQuery())._slideUp();
};
return self}, function($ctx1) {$ctx1.fill(self,"onNayTextAreaKeyUp:",{e:e},smalltalk.ReactiveWidget)});},
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOkNay",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#nayWrapper")._asJQuery())._slideUp();
_st(self)._sendFeedback();
return self}, function($ctx1) {$ctx1.fill(self,"onOkNay",{},smalltalk.ReactiveWidget)});},
messageSends: ["slideUp", "asJQuery", "sendFeedback"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOkYay",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#yayWrapper")._asJQuery())._slideUp();
_st(self)._sendEmail();
return self}, function($ctx1) {$ctx1.fill(self,"onOkYay",{},smalltalk.ReactiveWidget)});},
messageSends: ["slideUp", "asJQuery", "sendEmail"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#loader")._asJQuery())._remove();
_st(_st(window)._document())._title_("Let's do this!");
_st(window)._at_put_("app",self);
_st(_st(self)._session())._onOpen();
_st(self)._model_(_st(_st(self)._session())._user());
return self}, function($ctx1) {$ctx1.fill(self,"onOpen",{},smalltalk.ReactiveWidget)});},
messageSends: ["remove", "asJQuery", "title:", "document", "at:put:", "onOpen", "session", "model:", "user"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskRemoved:",
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"onTaskRemoved:",{ann:ann},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onYay",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._asJQuery())._scrollTop_((0));
_st(_st("#yayWrapper")._asJQuery())._slideDown_(smalltalk.HashedCollection._fromPairs_([_st("complete").__minus_gt((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#yayEmail")._asJQuery())._focus();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"onYay",{},smalltalk.ReactiveWidget)});},
messageSends: ["scrollTop:", "asJQuery", "slideDown:", "->", "focus"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onYayKeyUp:",
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($1)){
_st(_st("#yayWrapper")._asJQuery())._slideUp();
};
$2=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($2)){
_st(self)._onOkYay();
};
return self}, function($ctx1) {$ctx1.fill(self,"onYayKeyUp:",{e:e},smalltalk.ReactiveWidget)});},
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode", "onOkYay"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "open",
fn: function () {
var self=this;
var html;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
function $RChangeClients(){return smalltalk.RChangeClients||(typeof RChangeClients=="undefined"?nil:RChangeClients)}
return smalltalk.withContext(function($ctx1) { 
html=_st($HTMLCanvas())._onJQuery_(_st("body")._asJQuery());
_st(self)._onOpen();
_st(_st(_st(self)._session())._api())._onOpen_((function(){
return smalltalk.withContext(function($ctx2) {
_st(self)._renderOn_(html);
return _st((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st($RChangeClients())._new())._execute();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}))._valueWithTimeout_((100));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(_st(_st(self)._session())._api())._open();
return self}, function($ctx1) {$ctx1.fill(self,"open",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["onJQuery:", "asJQuery", "onOpen", "onOpen:", "renderOn:", "valueWithTimeout:", "execute", "new", "api", "session", "open"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "openBrowser",
fn: function () {
var self=this;
function $Browser(){return smalltalk.Browser||(typeof Browser=="undefined"?nil:Browser)}
return smalltalk.withContext(function($ctx1) { 
_st($Browser())._open();
return self}, function($ctx1) {$ctx1.fill(self,"openBrowser",{},smalltalk.ReactiveWidget)});},
messageSends: ["open"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
fn: function (aRemoveTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._removeTask_(aRemoveTask);
return self}, function($ctx1) {$ctx1.fill(self,"removeTask:",{aRemoveTask:aRemoveTask},smalltalk.ReactiveWidget)});},
messageSends: ["removeTask:", "list"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderAmberOn:",
fn: function (html) {
var self=this;
var canvas;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
canvas=_st($HTMLCanvas())._onJQuery_(_st("#header")._asJQuery());
$1=_st(canvas)._a();
_st($1)._id_("ide");
_st($1)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._openBrowser();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st($1)._with_("IDE");
return self}, function($ctx1) {$ctx1.fill(self,"renderAmberOn:",{html:html,canvas:canvas},smalltalk.ReactiveWidget)});},
messageSends: ["onJQuery:", "asJQuery", "id:", "a", "onClick:", "openBrowser", "with:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderFacebookLikeOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._div();
_st($1)._class_("fb-like");
_st($1)._at_put_("data-send","true");
_st($1)._at_put_("data-width","450");
_st($1)._at_put_("data-show-faces","true");
$2=_st($1)._yourself();
return self}, function($ctx1) {$ctx1.fill(self,"renderFacebookLikeOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["class:", "div", "at:put:", "yourself"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderFooterOn:",
fn: function (html){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$5,$7,$8,$6,$9,$11,$12,$10,$13,$15,$16,$17,$18,$14,$19,$20,$21,$22,$2;
$1=_st(html)._div();
_st($1)._id_("footer");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
_st(self)._renderSocialLoginOn_(html);
_st(self)._renderLikeOn_(html);
$3=_st(html)._img();
_st($3)._class_("vignette");
$4=_st($3)._src_("static/img/vignette.png");
$4;
$5=_st(html)._p();
_st($5)._class_("bgText");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(html)._span_("Written by ");
$7=_st(html)._a();
_st($7)._href_("http://www.linkedin.com/profile/view?id=46055105");
_st($7)._target_("_blank");
$8=_st($7)._with_("Sebastian Sastre");
return $8;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$6;
$9=_st(html)._p();
_st($9)._class_("bgText");
$10=_st($9)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(html)._span_(" author of ");
$11=_st(html)._a();
_st($11)._class_("bgText");
_st($11)._href_("http://airflowing.com");
_st($11)._target_("_blank");
$12=_st($11)._with_("airflowing");
return $12;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$10;
$13=_st(html)._p();
_st($13)._class_("bgText");
$14=_st($13)._with_((function(){
return smalltalk.withContext(function($ctx3) {
$15=_st(html)._a();
_st($15)._href_("http://amber-lang.net");
_st($15)._target_("_blank");
$16=_st($15)._with_("Amber");
$16;
_st(html)._span_(" frontend | ");
$17=_st(html)._a();
_st($17)._href_("http://www.pharo-project.org/");
_st($17)._target_("_blank");
$18=_st($17)._with_("Pharo");
$18;
return _st(html)._span_(" backend");
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$14;
$19=_st(html)._a();
_st($19)._class_("bgText");
_st($19)._href_("http://flowingconcept.com");
_st($19)._target_("_blank");
$20=_st($19)._with_("flowing");
$20;
$21=_st(html)._p();
_st($21)._class_("bgText");
$22=_st($21)._with_("October 2013");
return $22;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderFooterOn:",{html:html},smalltalk.ReactiveWidget)})},
messageSends: ["id:", "div", "with:", "renderSocialLoginOn:", "renderLikeOn:", "class:", "img", "src:", "p", "span:", "href:", "a", "target:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderHeaderOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
$1=_st(self)._isProduction();
if(smalltalk.assert($1)){
return nil;
};
$2=_st(html)._div();
_st($2)._id_("header");
$3=_st($2)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._renderAmberOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderHeaderOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["ifTrue:", "isProduction", "id:", "div", "with:", "renderAmberOn:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderLikeOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$5,$6,$2;
$1=_st(html)._p();
_st($1)._class_("bgText");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
_st(html)._span_("Like ♥?");
$3=_st(html)._a();
_st($3)._class_("yayNay");
_st($3)._onClick_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(self)._onYay();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$4=_st($3)._with_("Yay!");
$4;
$5=_st(html)._a();
_st($5)._class_("yayNay");
_st($5)._onClick_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(self)._onNay();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$6=_st($5)._with_("Nay!");
return $6;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderLikeOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["class:", "p", "with:", "span:", "a", "onClick:", "onYay", "onNay"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListFooterOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"renderListFooterOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._div();
_st($1)._id_("listLoaderBar");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(html)._img())._src_("static/img/loaderBar.gif");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self)._listDo_((function(){
return smalltalk.withContext(function($ctx2) {
_st(_st("#listLoaderBar")._asJQuery())._remove();
_st(_st("#mainTitle")._asJQuery())._text_("Let's do this!");
_st(_st(self)._list())._render();
return _st(self)._renderListFooterOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderListOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["id:", "div", "with:", "src:", "img", "listDo:", "remove", "asJQuery", "text:", "render", "list", "renderListFooterOn:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderNayOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$7,$8,$6,$9,$11,$12,$10,$13,$14,$15,$16,$4,$2;
$1=_st(html)._div();
_st($1)._id_("nayWrapper");
_st($1)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#Wrapper")._asJQuery())._slideUp();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._id_("nay");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(_st(html)._img())._src_("static/img/ocean.png");
$5=_st(html)._div();
_st($5)._class_("photoCredit");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx4) {
$7=_st(html)._a();
_st($7)._class_("photoCredit");
_st($7)._target_("_blank");
_st($7)._href_("http://www.flickr.com/photos/milanboers/3506659147");
$8=_st($7)._with_("Photo by Milan Boers");
return $8;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$6;
_st(_st(html)._h2())._with_("A drop into the ocean...");
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("...closer to a better world.");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$9=_st(html)._div();
_st($9)._id_("feedback");
$10=_st($9)._with_((function(){
return smalltalk.withContext(function($ctx4) {
_st(_st(html)._label())._with_("Your qualified feedback might just do that:");
$11=_st(html)._textarea();
_st($11)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx5) {
return _st(self)._onNayTextAreaKeyUp_(e);
}, function($ctx5) {$ctx5.fillBlock({e:e},$ctx1)})}));
_st($11)._at_put_("placeholder","What you did or did not like? What do you think would be nice to have next?");
$12=_st($11)._yourself();
return $12;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$10;
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
$13=_st(html)._input();
_st($13)._id_("nayEmail");
_st($13)._at_put_("placeholder","Email for contact? (optional)");
_st($13)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx5) {
return _st(self)._onNayKeyUp_(e);
}, function($ctx5) {$ctx5.fillBlock({e:e},$ctx1)})}));
$14=_st($13)._yourself();
return $14;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$15=_st(html)._a();
_st($15)._class_("dismiss");
_st($15)._onClick_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onOkNay();
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$16=_st($15)._with_("Ok");
return $16;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderNayOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["id:", "div", "onClick:", "slideUp", "asJQuery", "with:", "src:", "img", "class:", "a", "target:", "href:", "h2", "span:", "p", "label", "onKeyUp:", "onNayTextAreaKeyUp:", "textarea", "at:put:", "yourself", "input", "onNayKeyUp:", "onOkNay"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._renderHeaderOn_(html);
_st(self)._renderTipOn_(html);
_st(self)._renderTitleOn_(html);
$1=_st(html)._div();
_st($1)._id_("wrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._renderListOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self)._renderFooterOn_(html);
_st(self)._renderYayOn_(html);
_st(self)._renderNayOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["renderHeaderOn:", "renderTipOn:", "renderTitleOn:", "id:", "div", "with:", "renderListOn:", "renderFooterOn:", "renderYayOn:", "renderNayOn:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderSocialLoginOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4;
$1=_st(self)._isSociallyLoggedIn();
if(smalltalk.assert($1)){
return nil;
};
_st(_st(html)._div())._id_("fb-root");
$2=_st(self)._isPublished();
if(smalltalk.assert($2)){
_st(_st(_st(_st(self)._session())._social())._facebook())._load();
};
$3=_st(html)._script();
_st($3)._at_put_("type","in/Login");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(html)._with_("Hello!");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(_st(_st(_st(self)._session())._social())._linkedIn())._renderLoadOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderSocialLoginOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["ifTrue:", "isSociallyLoggedIn", "id:", "div", "load", "facebook", "social", "session", "isPublished", "at:put:", "script", "with:", "renderLoadOn:", "linkedIn"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTipOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$4,$6,$7,$5,$3;
$1=_st(self)._hasTip();
if(smalltalk.assert($1)){
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#tipWrapper")._asJQuery())._slideDown_(smalltalk.HashedCollection._fromPairs_([_st("complete").__minus_gt(_st((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st("#tipWrapper")._asJQuery())._slideUp();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}))._valueWithTimeout_((8000)))]));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._valueWithTimeout_((5000));
};
$2=_st(html)._div();
_st($2)._id_("tipWrapper");
_st($2)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#tipWrapper")._asJQuery())._slideUp();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$3=_st($2)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$4=_st(html)._div();
_st($4)._id_("tip");
$5=_st($4)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(_st(html)._img())._src_("static/img/manyClients.png");
_st(_st(html)._h2())._with_("Connect more devices for extra fun");
_st(_st(html)._h2())._with_(":D");
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("Try dragging, adding and changing tasks.");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$6=_st(html)._a();
_st($6)._class_("dismiss");
_st($6)._onClick_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onGotIt();
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$7=_st($6)._with_("Ok, got it!");
return $7;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $5;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderTipOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["ifTrue:", "valueWithTimeout:", "slideDown:", "->", "slideUp", "asJQuery", "hasTip", "id:", "div", "onClick:", "with:", "src:", "img", "h2", "span:", "p", "class:", "a", "onGotIt"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTitleOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#mainTitle")._asJQuery())._text_("Let's do...");
return self}, function($ctx1) {$ctx1.fill(self,"renderTitleOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["text:", "asJQuery"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderYayOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$7,$8,$6,$9,$11,$12,$10,$13,$14,$15,$16,$4,$2;
$1=_st(html)._div();
_st($1)._id_("yayWrapper");
_st($1)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#Wrapper")._asJQuery())._slideUp();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._id_("yay");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(_st(html)._img())._src_("static/img/baby.png");
$5=_st(html)._div();
_st($5)._class_("photoCredit");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx4) {
$7=_st(html)._a();
_st($7)._class_("photoCredit");
_st($7)._target_("_blank");
_st($7)._href_("http://www.flickr.com/photos/paparutzi/1062532768");
$8=_st($7)._with_("Photo by Christina Rutz");
return $8;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$6;
_st(_st(html)._h2())._with_("Small...");
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("...is how things start.");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("Are you a doer? This experiment is for something that will help doers and change makers in engaging the right people with their work and make this world a better place to live in.");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("Makes sense?");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
_st(_st(html)._div())._class_("g-plusone");
$9=_st(html)._p();
_st($9)._class_("feedback");
$10=_st($9)._with_((function(){
return smalltalk.withContext(function($ctx4) {
$11=_st(html)._a();
_st($11)._href_("https://docs.google.com/forms/d/17cVwbIWz72C2T4nKrfVGQLP4zf8JGomyp6zJ2_mGspk/viewform");
_st($11)._target_("_blank");
$12=_st($11)._with_("This");
$12;
return _st(html)._span_("3 question poll will also help us help you!");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$10;
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(_st(html)._label())._with_((function(){
return smalltalk.withContext(function($ctx5) {
_st(html)._p_((function(){
return smalltalk.withContext(function($ctx6) {
_st(html)._span_("Want an ");
_st(html)._strong_("early invite");
return _st(html)._span_("?");
}, function($ctx6) {$ctx6.fillBlock({},$ctx1)})}));
$13=_st(html)._input();
_st($13)._id_("yayEmail");
_st($13)._at_put_("placeholder","Enter your email");
_st($13)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx6) {
return _st(self)._onYayKeyUp_(e);
}, function($ctx6) {$ctx6.fillBlock({e:e},$ctx1)})}));
$14=_st($13)._yourself();
return $14;
}, function($ctx5) {$ctx5.fillBlock({},$ctx1)})}));
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$15=_st(html)._a();
_st($15)._class_("dismiss");
_st($15)._onClick_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onOkYay();
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$16=_st($15)._with_("Ok");
return $16;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self)._setGooglePlus();
return self}, function($ctx1) {$ctx1.fill(self,"renderYayOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["id:", "div", "onClick:", "slideUp", "asJQuery", "with:", "src:", "img", "class:", "a", "target:", "href:", "h2", "span:", "p", "p:", "strong:", "input", "at:put:", "onKeyUp:", "onYayKeyUp:", "yourself", "label", "onOkYay", "setGooglePlus"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "sendEmail",
fn: function () {
var self=this;
var email,prospect;
function $RProspect(){return smalltalk.RProspect||(typeof RProspect=="undefined"?nil:RProspect)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
email=_st(_st("#yayEmail")._asJQuery())._val();
$1=_st(email)._isEmpty();
if(smalltalk.assert($1)){
return nil;
};
$2=_st($RProspect())._new();
_st($2)._email_(email);
$3=_st($2)._yourself();
prospect=$3;
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt("api/prospect"),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(prospect)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"sendEmail",{email:email,prospect:prospect},smalltalk.ReactiveWidget)});},
messageSends: ["val", "asJQuery", "ifTrue:", "isEmpty", "email:", "new", "yourself", "ajax:", "->", "asJSONString"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "sendFeedback",
fn: function () {
var self=this;
var feedback,email,prospect;
function $RProspect(){return smalltalk.RProspect||(typeof RProspect=="undefined"?nil:RProspect)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
feedback=_st(_st(_st("#feedback")._asJQuery())._find_("textarea"))._val();
$1=_st(feedback)._isEmpty();
if(smalltalk.assert($1)){
return nil;
};
email=_st(_st("#nayEmail")._asJQuery())._val();
$2=_st($RProspect())._new();
_st($2)._feedback_(feedback);
_st($2)._email_(email);
$3=_st($2)._yourself();
prospect=$3;
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt("api/prospect"),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(prospect)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"sendFeedback",{feedback:feedback,email:email,prospect:prospect},smalltalk.ReactiveWidget)});},
messageSends: ["val", "find:", "asJQuery", "ifTrue:", "isEmpty", "feedback:", "new", "email:", "yourself", "ajax:", "->", "asJSONString"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "session",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@session"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeSession();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"session",{},smalltalk.ReactiveWidget)});},
messageSends: ["ifNil:", "initializeSession"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setFacebookPageLike",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
(function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));;
return self}, function($ctx1) {$ctx1.fill(self,"setFacebookPageLike",{},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setGooglePlus",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
(function() {
    var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
    po.src = 'https://apis.google.com/js/plusone.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
  })();;
return self}, function($ctx1) {$ctx1.fill(self,"setGooglePlus",{},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);


smalltalk.addMethod(
smalltalk.method({
selector: "open",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._open();
return $1;
}, function($ctx1) {$ctx1.fill(self,"open",{},smalltalk.ReactiveWidget.klass)});},
messageSends: ["open", "new"]}),
smalltalk.ReactiveWidget.klass);


smalltalk.addClass('TaskWidget', smalltalk.RWidget, ['listItem', 'view', 'editor'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "changedTask:",
fn: function (aTask) {
var self=this;
function $RChangeTask(){return smalltalk.RChangeTask||(typeof RChangeTask=="undefined"?nil:RChangeTask)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTask_(aTask);
_st(self)._model_(aTask);
_st(_st($RChangeTask())._for_(self["@model"]))._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"changedTask:",{aTask:aTask},smalltalk.TaskWidget)});},
messageSends: ["updateTask:", "model:", "localSave", "for:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "editor",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@editor"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeEditor();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"editor",{},smalltalk.TaskWidget)});},
messageSends: ["ifNil:", "initializeEditor"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "hide",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._hide();
return self}, function($ctx1) {$ctx1.fill(self,"hide",{},smalltalk.TaskWidget)});},
messageSends: ["hide", "asJQuery"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeEditor",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@editor"]=_st(self)._makeEditor();
$1=self["@editor"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeEditor",{},smalltalk.TaskWidget)});},
messageSends: ["makeEditor"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isCompleted",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._is_(":checked");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isCompleted",{},smalltalk.TaskWidget)});},
messageSends: ["is:", "find:", "asJQuery"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "makeEditor",
fn: function () {
var self=this;
var html;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
var $1;
html=_st($HTMLCanvas())._onJQuery_(_st(self["@listItem"])._asJQuery());
$1=_st(self)._renderEditorOn_(html);
return $1;
}, function($ctx1) {$ctx1.fill(self,"makeEditor",{html:html},smalltalk.TaskWidget)});},
messageSends: ["onJQuery:", "asJQuery", "renderEditorOn:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCancel",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st(self)._editor())._asJQuery())._hide();
_st(_st(self["@view"])._asJQuery())._show();
return self}, function($ctx1) {$ctx1.fill(self,"onCancel",{},smalltalk.TaskWidget)});},
messageSends: ["hide", "asJQuery", "editor", "show"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCompletionCheckboxClicked:",
fn: function (anEvent) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(anEvent)._stopPropagation();
_st(self)._setCompletion();
return self}, function($ctx1) {$ctx1.fill(self,"onCompletionCheckboxClicked:",{anEvent:anEvent},smalltalk.TaskWidget)});},
messageSends: ["stopPropagation", "setCompletion"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCompletionClicked:",
fn: function (anEvent) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._is_(":checked");
if(smalltalk.assert($1)){
_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._prop_put_("checked",false);
} else {
_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._attr_put_("checked",true);
};
_st(self)._setCompletion();
return self}, function($ctx1) {$ctx1.fill(self,"onCompletionClicked:",{anEvent:anEvent},smalltalk.TaskWidget)});},
messageSends: ["ifTrue:ifFalse:", "prop:put:", "find:", "asJQuery", "attr:put:", "is:", "setCompletion"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDescriptionClicked",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@view"])._asJQuery())._hide();
_st(_st(_st(_st(self)._editor())._asJQuery())._find_("input"))._val_(_st(self["@model"])._description());
_st(_st(_st(self)._editor())._asJQuery())._show();
_st(_st(_st(_st(self)._editor())._asJQuery())._find_("input"))._focus();
return self}, function($ctx1) {$ctx1.fill(self,"onDescriptionClicked",{},smalltalk.TaskWidget)});},
messageSends: ["hide", "asJQuery", "val:", "description", "find:", "editor", "show", "focus"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDone",
fn: function () {
var self=this;
function $RChangeTask(){return smalltalk.RChangeTask||(typeof RChangeTask=="undefined"?nil:RChangeTask)}
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st(self)._editor())._asJQuery())._hide();
_st(self["@model"])._description_(_st(_st(_st(_st(self)._editor())._asJQuery())._find_("input"))._val());
_st(_st($RChangeTask())._for_(self["@model"]))._execute();
_st(_st(_st(self["@view"])._asJQuery())._find_(".taskDescription"))._text_(_st(self["@model"])._description());
_st(_st(self["@view"])._asJQuery())._show();
_st(_st(self["@view"])._asJQuery())._effect_("highlight");
return self}, function($ctx1) {$ctx1.fill(self,"onDone",{},smalltalk.TaskWidget)});},
messageSends: ["hide", "asJQuery", "editor", "description:", "val", "find:", "execute", "for:", "text:", "description", "show", "effect:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyUp:",
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($1)){
_st(self)._onDone();
};
$2=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($2)){
_st(self)._onCancel();
};
return self}, function($ctx1) {$ctx1.fill(self,"onKeyUp:",{e:e},smalltalk.TaskWidget)});},
messageSends: ["ifTrue:", "onDone", "=", "keyCode", "onCancel"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onRemove:",
fn: function (anEvent) {
var self=this;
function $RRemoveTask(){return smalltalk.RRemoveTask||(typeof RRemoveTask=="undefined"?nil:RRemoveTask)}
function $RTaskRemoved(){return smalltalk.RTaskRemoved||(typeof RTaskRemoved=="undefined"?nil:RTaskRemoved)}
return smalltalk.withContext(function($ctx1) { 
_st(anEvent)._stopPropagation();
_st(self)._remove();
_st(_st($RRemoveTask())._for_list_(self["@model"],_st(_st(_st(window)._app())._list())._model()))._execute();
_st(self)._announce_(_st($RTaskRemoved())._for_(self));
return self}, function($ctx1) {$ctx1.fill(self,"onRemove:",{anEvent:anEvent},smalltalk.TaskWidget)});},
messageSends: ["stopPropagation", "remove", "execute", "for:list:", "model", "list", "app", "announce:", "for:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "remove",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._remove();
_st($RStorage())._delete_(self["@model"]);
return self}, function($ctx1) {$ctx1.fill(self,"remove",{},smalltalk.TaskWidget)});},
messageSends: ["remove", "asJQuery", "delete:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionOldOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$2;
$1=_st(html)._div();
_st($1)._class_("completionWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._input();
_st($3)._class_("completion");
_st($3)._type_("checkbox");
$4=_st($3)._at_put_("name",_st(self["@model"])._id());
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderCompletionOldOn:",{html:html},smalltalk.TaskWidget)});},
messageSends: ["class:", "div", "with:", "input", "type:", "at:put:", "id"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$2;
$1=_st(html)._div();
_st($1)._class_("completionWrapper");
_st($1)._onClick_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onCompletionClicked_(e);
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._input();
_st($3)._onClick_((function(e){
return smalltalk.withContext(function($ctx3) {
return _st(self)._onCompletionCheckboxClicked_(e);
}, function($ctx3) {$ctx3.fillBlock({e:e},$ctx1)})}));
_st($3)._class_("completion");
_st($3)._type_("checkbox");
$4=_st($3)._at_put_("name",_st(self["@model"])._id());
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderCompletionOn:",{html:html},smalltalk.TaskWidget)});},
messageSends: ["class:", "div", "onClick:", "onCompletionClicked:", "with:", "onCompletionCheckboxClicked:", "input", "type:", "at:put:", "id"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionTableOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$2;
$1=_st(html)._table();
_st($1)._class_("completion");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(html)._tr())._with_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st(html)._td())._with_((function(){
return smalltalk.withContext(function($ctx4) {
$3=_st(html)._input();
_st($3)._type_("checkbox");
$4=_st($3)._at_put_("name",_st(self["@model"])._id());
return $4;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderCompletionTableOn:",{html:html},smalltalk.TaskWidget)});},
messageSends: ["class:", "table", "with:", "type:", "input", "at:put:", "id", "td", "tr"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionWrappedOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$2;
$1=_st(html)._div();
_st($1)._class_("completionWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._input();
_st($3)._class_("completion");
_st($3)._type_("checkbox");
$4=_st($3)._at_put_("name",_st(self["@model"])._id());
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderCompletionWrappedOn:",{html:html},smalltalk.TaskWidget)});},
messageSends: ["class:", "div", "with:", "input", "type:", "at:put:", "id"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderEditorOn:",
fn: function (html) {
var self=this;
var container;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$6,$4,$2,$7;
$1=_st(html)._div();
_st($1)._class_("taskEditorWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._class_("taskEditor");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
$5=_st(html)._input();
_st($5)._class_("taskDescription");
_st($5)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onKeyUp_(e);
}, function($ctx4) {$ctx4.fillBlock({e:e},$ctx1)})}));
$6=_st($5)._with_(_st(self["@model"])._description());
return $6;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
container=$2;
$7=container;
return $7;
}, function($ctx1) {$ctx1.fill(self,"renderEditorOn:",{html:html,container:container},smalltalk.TaskWidget)});},
messageSends: ["class:", "div", "with:", "input", "onKeyUp:", "description"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._li();
_st($1)._id_(_st(self["@model"])._id());
_st($1)._class_("task");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
_st(self)._renderCompletionOn_(html);
return _st(self)._renderViewOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
self["@listItem"]=$2;
_st(self)._updateTask_(self["@model"]);
return self}, function($ctx1) {$ctx1.fill(self,"renderOn:",{html:html},smalltalk.TaskWidget)});},
messageSends: ["id:", "id", "li", "class:", "with:", "renderCompletionOn:", "renderViewOn:", "updateTask:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderViewOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$6,$7,$8,$4,$2;
$1=_st(html)._div();
_st($1)._class_("taskViewWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._class_("taskView");
_st($3)._onClick_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(self)._onDescriptionClicked();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
$5=_st(html)._span();
_st($5)._class_("taskDescription");
$6=_st($5)._with_(_st(self["@model"])._description());
$6;
$7=_st(html)._img();
_st($7)._class_("delete");
_st($7)._onClick_((function(e){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onRemove_(e);
}, function($ctx4) {$ctx4.fillBlock({e:e},$ctx1)})}));
$8=_st($7)._src_("static/img/delete.png");
return $8;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
self["@view"]=$2;
return self}, function($ctx1) {$ctx1.fill(self,"renderViewOn:",{html:html},smalltalk.TaskWidget)});},
messageSends: ["class:", "div", "with:", "onClick:", "onDescriptionClicked", "span", "description", "img", "onRemove:", "src:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setCompletion",
fn: function () {
var self=this;
function $RChangeTask(){return smalltalk.RChangeTask||(typeof RChangeTask=="undefined"?nil:RChangeTask)}
function $RTaskChanged(){return smalltalk.RTaskChanged||(typeof RTaskChanged=="undefined"?nil:RTaskChanged)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._isCompleted();
if(smalltalk.assert($1)){
_st(_st(_st(self["@listItem"])._asJQuery())._find_("span.taskDescription"))._addClass_("completed");
} else {
_st(_st(_st(self["@listItem"])._asJQuery())._find_("span.taskDescription"))._removeClass_("completed");
};
_st(self["@model"])._isCompleted_(_st(self)._isCompleted());
_st(_st($RChangeTask())._for_(self["@model"]))._execute();
_st(self)._announce_(_st($RTaskChanged())._for_(self));
return self}, function($ctx1) {$ctx1.fill(self,"setCompletion",{},smalltalk.TaskWidget)});},
messageSends: ["ifTrue:ifFalse:", "addClass:", "find:", "asJQuery", "removeClass:", "isCompleted", "isCompleted:", "execute", "for:", "announce:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "show",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._show();
return self}, function($ctx1) {$ctx1.fill(self,"show",{},smalltalk.TaskWidget)});},
messageSends: ["show", "asJQuery"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "showIfDone",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._isCompleted();
if(smalltalk.assert($1)){
_st(self)._show();
} else {
_st(self)._hide();
};
return self}, function($ctx1) {$ctx1.fill(self,"showIfDone",{},smalltalk.TaskWidget)});},
messageSends: ["ifTrue:ifFalse:", "show", "hide", "isCompleted"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "showIfToDo",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._isCompleted();
if(smalltalk.assert($1)){
_st(self)._hide();
} else {
_st(self)._show();
};
return self}, function($ctx1) {$ctx1.fill(self,"showIfToDo",{},smalltalk.TaskWidget)});},
messageSends: ["ifTrue:ifFalse:", "hide", "show", "isCompleted"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTask:",
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTaskDescription_(aTask);
_st(self)._updateTaskCompletion_(aTask);
return self}, function($ctx1) {$ctx1.fill(self,"updateTask:",{aTask:aTask},smalltalk.TaskWidget)});},
messageSends: ["updateTaskDescription:", "updateTaskCompletion:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTaskCompletion:",
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._attr_put_("checked",_st(aTask)._isCompleted());
$1=_st(aTask)._isCompleted();
if(smalltalk.assert($1)){
_st(_st(_st(self["@listItem"])._asJQuery())._find_("span.taskDescription"))._addClass_("completed");
} else {
_st(_st(_st(self["@listItem"])._asJQuery())._find_("span.taskDescription"))._removeClass_("completed");
};
return self}, function($ctx1) {$ctx1.fill(self,"updateTaskCompletion:",{aTask:aTask},smalltalk.TaskWidget)});},
messageSends: ["attr:put:", "isCompleted", "find:", "asJQuery", "ifTrue:ifFalse:", "addClass:", "removeClass:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTaskDescription:",
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._description_(_st(aTask)._description());
_st(_st(self["@view"])._asJQuery())._show();
_st(_st(self["@view"])._asJQuery())._effect_("highlight");
_st(_st(_st(self["@view"])._asJQuery())._find_(".taskDescription"))._text_(_st(self["@model"])._description());
return self}, function($ctx1) {$ctx1.fill(self,"updateTaskDescription:",{aTask:aTask},smalltalk.TaskWidget)});},
messageSends: ["description:", "description", "show", "asJQuery", "effect:", "text:", "find:"]}),
smalltalk.TaskWidget);



(function () {
    var inBrowser = typeof amber !== "undefined" && typeof amber.load === "function";
    function init() {
        /* Similar to jQuery(document).ready() */

        smalltalk.initialize();
 		smalltalk.ReactiveWidget._open();
 
        if (inBrowser && amber.smalltalkReady) {
            amber.smalltalkReady();
        }
    }

    if (inBrowser) {
        // init is lengthy process done in JavaScript.
        // setTimeout here postpones it, so DOM ready
        // event can occur sooner, thus load process
        // may appear optically faster.
        setTimeout(init, 0);
    } else {
        // In certain configurations, setTimeout is not feasible.
        // It is mainly for `amberc`-produced concatenated
        // node.js programs. There, the actual "main" appears
        // immediately after init, so it must happens synchronously.
        init();
    }
})();