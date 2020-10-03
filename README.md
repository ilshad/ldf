# LDF - Linked Data Formats for Clojure/ClojureScript

This library aims to provide a bidirectional bridge between 2 worlds:
RDF/Linked Data and EDN/Clojure.

## Features

- Mapping between keyword namespaces and RDF prefixes

	In RDF, the IRI is the identity. In EDN, you can use simple or
	qualified keywords as their internal representations, but only for
	those RDF prefixes where you have reason to do that. In other words,
	IRI prefixes are external, but namespaces for keywords are internal.
	They are not interfering with each other.
	
	In practice, we are not going to convert all the IRIs into keywords,
	and their RDF prefixes into Clojure namespaces, and vice versa.
	Instead:

	- we need only some prefixes (probably, very few of them) to map
	  to the namespaces for qualified keywords.

	- prefixes should not determine how namespaces look like and vice
	  versa.

	- moreover, it is not necessary that an input Turtle document
	  will have prefixes at all. But we still can use qualified keywords
	  by "extracting" part of IRI and creating a namespace for that part.

- Handling terse format for collections

	A well-constructed RDF/Turtle document implies to reduce triples
	into trees with nested predicate lists and object lists. But when
	we work with data, we do not need that. All we need	are	simple
	triples: subject, predicate and object. So the parser flattens
	these trees. On the other hand, the encoder builds the trees for us.

- Handling trees of Blank Node Property Lists

	RDF/Turtle allows Blank Node Property Lists to create nested data
	structures. The parser transforms everything into a flat list of
	triples: subject, predicate and object, creating temporary
	identifiers	for anonymous entities, so we can handle them as
	datalog-like statements. On the other hand, the encoder builds
	such terse trees from our data.

- Isomorphic and bidirectional

	The same syntax and semantics both on frontend and backend,
	both for parsing input documents and producing output documents.

## Status

This work is in progress.

Currently, the development is concentrated around Turtle standard:
https://www.w3.org/TR/turtle and it is already quite accurate.
