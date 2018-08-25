[Dataspace/Fabric - Nate Cull](http://natecull.org/wordpress/category/dataspace/)

# Dataspace 0: Those Memex Dreams Again

>Computing in the Internet age has a number of inspiring visions: legendary systems, some of which got built, some of which remained hypothetical, “dream machines”. Among them are: 
> - Vannevar Bush’s Memex (1945).
> - Ted Nelsen’s Xanadu (1960).

[The Curse of Xanadu](https://www.wired.com/1995/06/xanadu/)

[Computer Lib/Dream Machines - Ted Nelson](https://www.amazon.com/Computer-Lib-Dream-Machines-Revised/dp/0914845497/ref=sr_1_2?ie=UTF8&qid=1535213932&sr=8-2&keywords=ted+nelson)

> - J R Licklider’s Intergalactic Computer Network (1963).
> - Douglas Engelbart’s NLS (1968) and Alan Kay’s Dynabook (also 1968).
> - William Gibson’s Cyberspace (1982).

> We currently have an Internet made of vast layers of complexity layered on each other;
> software layers going back to the 1960s at the very latest, built on traditions and workflows originated in the 1950s.
>
> The operating system inside these simulated computers-on-computers  then consists of, essentially, an entire simulated computing department  from the 1950s:
>
> - a bank of clerks operating card punches (text editors  and Interactive Development Environments), 
> - other clerks translating  these punchcards from high-level to low-level languages (compiler  toolchains),
> - machine operators who load the right sets of cards into the machine (operating systems, schedulers, job control systems), 
> - banks of  tape drives (filesystems and databases), 
> - printers (web servers, UIs )…  
> - and a whole bunch of prewritten software card stacks (libraries,  component object systems, open source projects). 
>
> Back around 1980, as interactive systems were starting to become  commonplace and home computers were showing that they were the future,  the idea of Objects started to catch hold, driven by Alan Kay’s  Smalltalk. This seemed to give us the solution: replace the ‘computer’  with the ‘object’, and build everything out of objects. One unified  paradigm to build an entire network from. 
>
> Object -Oriented Programming did become popular – in fact it became so  popular through the 1990s-00s that there were so many different object  systems that almost none of them could communicate. 
> Even as we built the Web, based not on *objects* but on the much older notions of *documents* and *data packets*
>
> The object vision was pretty neat for 1980. Now that we’ve had a few  years to play with it, I think we’ve also found that it has some serious  limitations:
>
> - An object is opaque by design, for safety, simplicity and  compatibility. You can’t look inside an object, there’s no standard  protocol for ‘serialising’ or dumping an object into a standard form,  and because of this you can’t guarantee that you can transfer an object  between systems.
> - An object has side effects. If you send a message to an object,  things change, somewhere in the universe – potentially any object it has  a link to can change. Since an object is opaque by design, those links  can point anywhere. You can’t know what part of the universe just  changed from that message you just sent.
> - An object ‘message send’ is not in fact, usually, an actual message.  It’s generally a function call, which is executed locally and waits,  blocking a thread, until it’s finished. This means the programmer has to  manage all those side effects manually.
> - Not everything in the object world is in fact made out of objects.  Objects are compiled from source code, which generally is stored in  files, not in objects. Objects are made of methods, which (generally)  aren’t themselves objects. And the messages which objects don’t send to  one another aren’t themselves objects.
> - Most seriously: after 27 or so years with the concept, we still  don’t have anything approaching a standard, formal definition of what an  object *is*. Not like ‘function’, or ‘relational database’ for  example, which started with a whole bunch of maths (though we often  ignore most of it and use C and SQL instead, which have *almost* functions and *almost* relations, but not quite). An object seems to be sort of a sociological tendency rather than a mathematical theory.
>
> (The 1990s is littered with distributed object systems that either  failed spectacularly, or got trapped in various tiny niches. See, for  example, IBM’s [System Object Model](https://en.wikipedia.org/wiki/IBM_System_Object_Model); Sun’s [Distributed Objects Everywhere](https://en.wikipedia.org/wiki/Distributed_Objects_Everywhere); and NeXT’s [Portable Distributed Objects](https://en.wikipedia.org/wiki/Portable_Distributed_Objects), which live on kinda-sorta in OSX). 
>
> But there’s one thing that objects do give you: and that’s the idea  that the virtual ‘giant computer’ of the network or Internet is  something like a *space*. 
> A space made out of ‘places’ (objects)  which have ‘directions’ you can ‘travel’ between them (methods, or  fields). 
> You can chain those directions together to make a ‘path’. 
> And  that path tells you what you will ‘find there’: another object,  representing a return value, or collection of values. 

an improved object model (?)

- has a solid mathematical foundation like Relational Algebra to relational database
- loosely typed, as less number of assumptions about the structures of the data as possible, can be later elaborated by users
- has a notion of computation (data transformation) without side effects, pure and self-contained
- able to store large collections of small and irregular pieces of data
- well-defined standard/formats for serialization and deserialization of data for communication between systems

# Dataspace 1: In Search of a Data Model

> The Web is a distributed filesystem – dating back to at least 1969 for Unix – using DNS for the server name, then folders on the server, then finally a file on the server.
> This gives us a file path.

> HTTP augments this with an anchor (the thing after a #), which is a single named node inside an HTML file.
> It then augments this further with query parameters (the things after a ?), which are lists of variables and values, but which don’t relate to a file (what HTTP calls a document) at all.

> Query parameters are only sensible to a HTTP server.
> We want paths that identify a piece of data in a ‘dataspace’, which might be bigger or smaller than a document.
> We need a data model that’s coherent and built for the job of ‘pointing at small pieces of data from anywhere’.
> the core HTTP data model is not good enough is exactly why we have so many web application server frameworks on the servers and Javascript frameworks on the client – to try to reimplement the pieces that weren’t there in the original HTTP/HTML Web vision.

## Option 1: Filesystems
> filesystems end in files.

granularity not small and flexible enough

## Option 2: Object systems
> objects are typed chunks of data that are much smaller than files

> there is no ‘one true object system’ that exists everywhere,
> or even enough places that we can use. 

> the very idea of an object has no formal definition,
> there’s no clear way to transport them or point at them between systems,
> and they just generally feel too complicated and too heavy.

> We want something like objects! But simpler.

## Option 3: Dictionaries and functions
> The kind of information I want to store in a Memex type system turns out to be very similar to logical statements.

> If we’re going to be storing logical statements, though, it’s going to be really really important that we can have dictionaries with keys that are arbitrary dictionaries. 
> JSON will only let you use a text string as a key, not a dictionary itself. Other systems like Lua don’t have this restriction.

> one key with multiple values – is in short is why we can’t use dictionaries
> Functions are dictionaries

dictionary follows function constraint:
- an element in the domain is uniquely mapped to an element in the codomain
- there is no such a function that maps to an empty set as its codomain

> Example
> Individually, they each are sensible dictionaries:
> ```
> {weather: raining}
> {weather: sunny}
> ```

bad example, multiple parallel choices can be described by `List` (product) or `Either`(coproduct)
```
{weather: [raining, sunny]}
{weather: raining | sunny }
```

dictionary is adequate to encode logical statements, even with only `String` allowed in the keys
```
"{A:{is:False}}":{is:True}
```
just need a way to identify logical statements
- strictly equal
- up to isomorphism (an invertible morphism in between)
- adjunctions

when considering CRUD performance, data integrity, and system consistency,
lower implementation details could very likely pollute the mathematical abstraction of equality
e.g. physical addresses, pointers, data races, inconsistency robustness, etc.

the author has Semantic Web's model in mind
I guess he's too harsh to move towards that solution by discrediting the dictionary/function solution without too much thought about isomorphism between

in addition,
maintaining logical consistency in a distributed data system is too computationally intense if not impossible, even for a tiny set of logical rules

and too much conceptual burden for average developers

## Option 4: Relational databases
> All of computing is built on functions? But no.
> Functions are just a special case of a much more fundamental mathematical structure – relations. 

a relation between two types/sets is a hom-set in the category of types/sets which is a set of functions

> Functions are a special case of a special kind of relation: binary relations.

not considering higher-order functions

> You can think of it roughly as a function that can have multiple values.
> for example, square root

> But what computing generally gives us isn’t binary relations.
> Instead we have finitary relations (relations of many places) – and in fact we don’t even have those.

limits/colimits

[Tuple space](https://en.wikipedia.org/wiki/Tuple_space)

> What we have in our SQL database engines is Dr Codd’s relational algebra.
> 1. it’s restricted is that it divides your data into relations (what databases call tables), but each entry (row) in each relation must be the same length,
> meaning relations can really only describe very regular, highly processed and highly structured data.
> 2. Another serious restriction, which isn’t in Codd’s relational algebra itself, but in SQL databases built on it,
> is that you can’t generally nest relations inside relations.

then create separate tables and refer to them
expressiveness is not the issue

the problem is the syntactic noise in the query languages when trying to traverse a deep path

## Option 5: Graphs and triples
> Since the 1990s, a lot of work on storing things like logical statements has been inspired by the Semantic Web idea.
> Led by Tim Berners-Lee
> a way of pointing at small pieces of data.
> The Semantic Web concept is based on graphs with labelled edges, but stored as a large Codd relation
> and the graph is built out of a vast number of triples.
> A triple is a three-place logical statement: `(subject predicate object)`

> Lists aren’t made out of triples. They’re made out of pairs. But we have no way to natively represent a list in triple form.
> To represent a list in triple-based systems you’d need to add an extra thing in the middle, so eg `(a b c)` becomes `(a . (b . (c . ())))`

to me, totally legitimate, the default representation of a linked list in Haskell

the problem is not about syntax
`(a b c)` is not isomorphic to `(a . (b . (c . ())))`

`(a b c)` is a multi-product/limit of `a`, `b`, and `c`
- `(a b c) -> a`
- `(a b c) -> b`
- `(a b c) -> c`

`(a . (b . (c . ()))) ~= a -> (b -> (c -> () ))`

> You might recognise this as good old S-expression dot notation.

> If you’re using RDF (Resource Description Framework) things get even messier because you might then serialise those triples over XML or a half dozen custom syntaxes invented just to solve this problem of ‘how do I serialise triples?’

> But the big problem is that you might want to put your triples – or your sets of triples – somewhere… and that means you might want to have graphs of graphs, or relations containing relations… and although Codd relations can do that, most triple stores, or graph query languages, haven’t quite got there.

nesting relations is not a problem,
actually it's the only one good thing done right by RDF as a graph database

> A third problem which might be so huge it’s not even registering on our radar: triples (or Codd relations generally) don’t have a programming model associated with them. They’re purely a data storage model.
not sure why this is a problem
and in reality, Semantic Web is designed around first-order predicate logic which is the "programming model"
I guess the author doesn't take logic programming as a legit form of programming for general purposes
but it's strictly biased to emphasize it here because the only viable data model tightly connected to "general programming" is Option 2: Object systems.



predicate in Semantic Web is a named polymorphic function that refers to a set of edges
```haskell
predicateA :: forall a b. a -> b
```
in which type `a` and type `b` can be shaped by logical constraints like in dependent type systems

to refer to an edge instance (in order to build higher-order statements/functions), need to use cumbersome reification mechanism
