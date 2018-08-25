[Dataspace/Fabric - Nate Cull](http://natecull.org/wordpress/category/dataspace/)

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
- loosely typed, as less assumption about the structure of the data as possible, can be later elaborated by users
- has a notion of computation (data transformation) without side effects, pure and self-contained
- able to store large collections of small and irregular pieces of data
- well-defined standard/formats for serialization and deserialization of data for communication between systems