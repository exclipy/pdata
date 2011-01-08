Persistent Vectors and HashMaps for Haskell
===========================================

One of the prominent features of the [Clojure][1] language are a set of
[immutable data structures][2] with efficient manipulation operations.  One of
the most innovative and important is the persistent hash map based on the
*hash array mapped trie*.

This project is a port of this structure to Haskell, as Data.HashMap.  The
interface has been kept as consistent as possible with Data.Map.

[1]: http://clojure.org/
[2]: http://clojure.org/datatypes


Basic usage
-----------
Here's a demo of what you can do with a HashMap:

    ghci> :m + Data.HashMap
    ghci> empty Data.HashTable.hashString
            -- an empty HashMap (requires a key hash function)
    fromList hashFn []

    ghci> insert "foo" 1 it
    fromList hashFn [("foo",1)]

    ghci> insert "bar" 42 it
    fromList hashFn [("foo",1),("bar",42)]

    ghci> insert "qux" 123 it
    fromList hashFn [("qux",12),("foo",1),("bar",42)]

    ghci> insert "qux" 13 it  -- inserting an existing key overwrites by default
    fromList hashFn [("qux",13),("foo",1),("bar",42)]

    ghci> let a = it
    ghci> a ! "foo"
    1

    ghci> a ! "baz"  -- using (!) is unsafe
    *** Exception: array index out of range: element not in the map

    ghci> Data.HashMap.lookup "bar" a
    Just 42

    ghci> Data.HashMap.lookup "baz" a  -- 'lookup' returns a safe Maybe
    Nothing

    ghci> adjust succ "foo" a  -- apply a function to a value
    fromList hashFn [("qux",13),("foo",2),("bar",42)]

    ghci> Data.HashMap.map succ a  -- apply a function to all values
    fromList hashFn [("qux",14),("foo",2),("bar",43)]

    ghci> keys a
    ["qux","foo","bar"]

    ghci> elems a
    [13,1,42]

    ghci> fromList Data.HashTable.hashString [("a", 1), ("b", 2), ("c", 3)]
    fromList hashFn [("b",2),("c",3),("a",1)]

    ghci> toList it
    [("b",2),("c",3),("a",1)]


Installation
------------

To try it yourself, just do the usual:

    $ runghc Setup.hs configure --user
    $ runghc Setup.hs build
    $ runghc Setup.hs install

Performance
-----------

The single-element operations for the hash map technically runs in logarithmic
time.  However, it is implemented as a 32-ary tree, which means it never exceeds
a depth of 7 nodes, so you can treat them as constant-time operations (for
relatively large constants).

How it works
------------

I wrote this code after reading the following explanatory blog posts on how the
Clojure version works.  They should also provide a decent birds-eye overview of
my Haskell implementation.

* [Understanding Clojure’s PersistentHashMap
  ](http://blog.higher-order.net/2009/09/08/understanding-clojures-persistenthashmap-deftwice/)
* [Assoc and Clojure’s PersistentHashMap: part II
  ](http://blog.higher-order.net/2010/08/16/assoc-and-clojures-persistenthashmap-part-ii/)


To do (help appreciated!)
-------------------------
* Match Data.Map in completeness
* Performance tuning
  * More strictness
  * A more efficient fromList (it currently constructs lots of intermediary
    structures
* Unit tests
* Benchmarks
