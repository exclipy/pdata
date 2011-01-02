Persistent Vectors and HashMaps for Haskell
===========================================

One of the prominent features of the [Clojure][1] language are a set of
[immutable data structures][2] with efficient manipulation operations.  Two of
the most innovative and important are the persistent vector and persistent hash
map.  This project is a port of these structures to Haskell.

I wrote this code after reading the following explanatory blog posts on how they
work in Clojure.  They should also provide a decent birds-eye overview of my
Haskell implementation.

*   [Understanding Clojure’s PersistentVector implementation](http://blog.higher-order.net/2009/02/01/understanding-clojures-persistentvector-implementation/)
*   [Understanding Clojure’s PersistentHashMap](http://blog.higher-order.net/2009/09/08/understanding-clojures-persistenthashmap-deftwice/)
*   [Assoc and Clojure’s PersistentHashMap: part II](http://blog.higher-order.net/2010/08/16/assoc-and-clojures-persistenthashmap-part-ii/)


[1]: http://clojure.org/
[2]: http://clojure.org/datatypes
