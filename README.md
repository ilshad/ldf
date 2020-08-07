# LDF - Linked Data Formats in Clojure/ClojureScript

This library aims to provide a bidirectional bridge between 2 worlds:
RDF/Linked Data and EDN/Clojure.

## Features

- EDN Namespaces and RDF Prefixes

	In RDF/Turtle the IRI is the identity. With this library, you can
	use	simple and qualified keywords as internal identifiers,	but only
	for those URI namespaces where you want to do that. So the prefixes
	in Turtle IRIs are external, but namespaces in EDN keywords are
	internal things and they are not interfering with each other.
	
	In practice, you do not want to convert all the IRIs into keywords,
	and especially, you do not want to convert IRI prefixes into
	namespaces for keywords (and vice versa), but:

	- only some namespaces
	- external prefixes should not determine how internal namespaces
	  look like and vice versa.
	- moreover, it is not necessary that input Turtle document will
	  have prefixes at all.

- Terse format for collections

	Well-constructed RDF/Turtle document implies to reduce triples
	into trees with predicate lists and object lists. But when
	we work with data, we do not need that. All we need are	simple
	triples: subject, predicate, object. So the parser flattens	these
	trees and constructor builds the trees for us.

- Isomorphic and bidirectional

	The same syntax and semantics both on frontend and backend,
	both for parsing input documents and producing output documents.

- Precise coverage of the standard

	If a library covers 99% of the standard, these other uncovered
	1% of the features will constantly arise as issues.

## Status

This work is in progress.

Currently, the development is concentrated around Turtle standard:
https://www.w3.org/TR/turtle and it is already quite accurate.
The main thing left is Blank Nodes, which is TDB. After that,
and after some testing in production, proper documentation will
be added and the first version of the library will be released.
