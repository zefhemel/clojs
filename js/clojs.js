/**
 * An initial, inefficient implementation of CloJS's basic data structures
 */

function Cons(head, tail) {
    this.first = function() {
        return head;
    }

    this.rest = function() {
        return tail;
    }

    this.cons = function(newHead) {
        return new Cons(newHead, this);
    }

    this.seq = function() {
        return this;
    }

    this.isEmpty = function() {
        return head == null && tail == null;
    }

    this.equals = function(other) {
        if(this.isEmpty() && other.isEmpty()) {
            return true;
        }
        return head == other.first() && tail.equals(other.rest());
    }

    this.toArray = function() {
        var result = Array();
        result.push(head);
        var current = tail;
        while(current != null) {
            result.push(current.first());
            current = current.rest();
        }
        return result;
    }

    this.toString = function() {
        return "(" + this.toArray().toString() + ")";
    }
}


function Map(obj) {
    function shallowclone(obj) {
        if(obj == null || typeof(obj) != 'object')
            return obj;

        var temp = new Object();

        for(var key in obj)
            temp[key] = obj[key];
        return temp;
    }
    this.assoc = function(k, v) {
        var c = shallowclone(obj);
        c[k] = v;
        return new Map(c);
    }

    this.disassoc = function(k) {
        var c = shallowclone(obj);
        delete c[k];
        return new Map(c);
    }

    this.get = function(k) {
        return obj[k];
    }

    this.seq = function() {
        var c = new Cons(null, null);
        for(var k in obj) {
            c = new Cons(new Vector([k, obj[k]]), c);
        }
        return c;
    }

    this.keys = function() {
        var ks = Array();
        for(var i in obj) {
            ks.push(i);
        }
        return ks;
    }

    this.values = function() {
        var ks = Array();
        for(var i in obj) {
            ks.push(obj[i]);
        }
        return ks;
    }
    
    this.internal = function() {
        return obj;
    }

    this.equals = function(other) {
        if(this.keys().length != other.keys().length) {
            return false;
        }
        var obj2 = other.internal();
        for(var propertyName in obj) {
            if(obj[propertyName] !== obj2[propertyName]) {
                return false;
            }
        }
        return true;
    }

    this.toString = function() {
        var s = '{'
        for(var k in obj) {
            s += k + ": " + obj[k] + ", "
        }
        return s.substring(0, s.length-2) + '}';
    }
}

function Vector(array) {
    this.get = function(idx) {
        return array[idx];
    }

    this.seq = function() {
        var c = new Cons(null, null);
        for(var i = array.length - 1; i >= 0; i--) {
            c = new Cons(array[i], c);
        }
        return c;
    }

    this.toString = function() {
        var s = '['
        for(var i = 0; i < array.length; i++) {
            s += array[i] + ", ";
        }
        return s.substring(0, s.length-2) + ']';
    }
    
    this.internal = function() {
        return array;
    }

    this.equals = function(other) {
        var array2 = other.internal();
        if(array.length != array2.length) {
            return false;
        }
        for(var propertyName in array) {
            if(array[propertyName] !== array2[propertyName]) {
                return false;
            }
        }
        return true;
    }
}

function mapFromSeq(seq) {
    var m = new Object();
    while(!seq.isEmpty()) {
        var f = seq.first();
        m[f.get(0)] = f.get(1);
        seq = seq.rest();
    }
    return new Map(m);
}

function vectorFromSeq(seq) {
    var ar = [];
    while(!seq.isEmpty()) {
        ar.push(seq.first());
        seq = seq.rest();
    }
    return new Vector(ar);
}

String.prototype.seq = function() {
    c = new Cons(null, null);
    for(var i = this.length - 1; i >= 0; i--) {
        c = new Cons(this[i], c);
    }
    return c;
}
String.prototype.equals = function(other) {
    return this == other;
}

Number.prototype.equals = function(other) {
    return this == other;
}

function map(fn, seq) {
    if(!seq.isEmpty()) {
        return new Cons(fn(seq.first()), map(fn, seq.rest()));
    } else {
        return seq;
    }
}
/*
var v = new Vector([1, 2, 3]).seq();
var v2 = new Vector([1, 2, 3]).seq();
print(v.equals(v2));
var m = new Map({"name": "Zef", "age": 26});
var m2 = new Map({"name": "Zef", "age": 26});
print(m.equals(m2));

var m = new Map({"name": "Zef"}).seq().cons(new Vector(["age", 26]));
print(m);

var v = new Vector([1, 2, 3]);
print(v.get(2));
print("Zef".seq())
print(vectorFromSeq(new Vector([1, 2, 3]).seq()))
*/