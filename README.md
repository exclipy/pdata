Persistent Vectors and HashMaps for Haskell
===========================================

One of the prominent features of the [Clojure][1] language are a set of
[immutable data structures][2] with efficient manipulation operations.  Two of
the most innovative and important are the persistent vector and persistent hash
map.

This project is a port of these structures to Haskell.  The API provides
Data.PVector (the persistent vector) and Data.PHashMap (the persistent hash
map).  The interface for both has been kept as consistent as possible with
Data.Map.

[1]: http://clojure.org/
[2]: http://clojure.org/datatypes


Basic usage
-----------

Here's a demo of what you can do with a PVector:

    ghci> :m + Data.PVector
    ghci> empty  -- the empty pvector
    fromList []

    ghci> append 1 it
    fromList [1]

    ghci> append 42 it
    fromList [1,42]

    ghci> append 13 it
    fromList [1,42,13]

    ghci> let a = it
    ghci> a ! 0  -- indexes are from 0 to n-1
    1

    ghci> a ! 1
    42

    ghci> a ! 2
    13

    ghci> set 1 71 a  -- a new PVector with the element replaced
    fromList [1,71,13]

    ghci> adjust succ 2 a  -- apply a function to a single element
    fromList [1,42,14]

    ghci> Data.PVector.map succ a  -- apply a function to all elements
    fromList [2,43,14]

    ghci> fromList [1..10]  -- convert a list to a PVector
    fromList [1,2,3,4,5,6,7,8,9,10]

    ghci> elems it  -- convert a PVector to a list
    [1,2,3,4,5,6,7,8,9,10]


And here's a demo of the basic functionality of PHashMap:

    ghci> :m + Data.PHashMap
    ghci> empty Data.HashTable.hashString
            -- an empty PHashMap (requires a key hash function)
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

    ghci> Data.PHashMap.lookup "bar" a
    Just 42

    ghci> Data.PHashMap.lookup "baz" a  -- 'lookup' returns a safe Maybe
    Nothing

    ghci> adjust succ "foo" a  -- apply a function to a value
    fromList hashFn [("qux",13),("foo",2),("bar",42)]

    ghci> Data.PHashMap.map succ a  -- apply a function to all values
    fromList hashFn [("qux",14),("foo",2),("bar",43)]

    ghci> keys a
    ["qux","foo","bar"]

    ghci> elems a
    [13,1,42]

    ghci> fromList Data.HashTable.hashString [("a", 1), ("b", 2), ("c", 3)]
    fromList hashFn [("b",2),("c",3),("a",1)]

    ghci> toList it
    [("b",2),("c",3),("a",1)]


How it works
------------

I wrote this code after reading the following explanatory blog posts on how they
work in Clojure.  They should also provide a decent birds-eye overview of my
Haskell implementation.

* [Understanding Clojure’s PersistentVector implementation
  ](http://blog.higher-order.net/2009/02/01/understanding-clojures-persistentvector-implementation/)
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
* Make a PVector-based implementation of IArray (?)
* Unit tests
* Benchmarks
