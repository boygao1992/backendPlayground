[Clojure for the brave and true - Chapter 10: Clojure Metaphysics: Atoms, Refs, Vars, and Cuddle Zombies](https://www.braveclojure.com/zombie-metaphysics/)

4 basic mutable models

- `Atom`, equivalent to purescript `Ref`
  - dereferencing, `@` = `read`
  - `reset!` = `write`
  - `swap!` = `modify`
- `Ref`
- `Var`
- `Agent`



> ## Atoms
>
> This leads to a different conception of identity. Instead of understanding identity as inherent to a changing object, as in OO metaphysics, Clojure metaphysics construes identity as something we humans impose on **a succession of unchanging values produced by a process over time**. We use names to designate identities. The name Fred is a handy way to refer to a series of individual states F1, F2, F3, and so on. From this viewpoint, there’s no such thing as mutable state. Instead, state means the value of an identity at a point in time.

versioned immutable value

> ### Atoms - Watches
>
> A *watch* is a function that takes four arguments: a key, the reference being watched, its previous state, and its new state. You can register any number of watches with a reference type.

additional logic after each update of the `Atom`

like wrapping an `Atom` in an Observable which captures the Update event and passes it on to children Observables which can then attach more complicated pattern matching logic

> ### Atoms - Validator
>
> *Validators* let you specify what states are allowable for a reference. 
>
> ```clojure
> (def bobby
>   (atom
>    {:cuddle-hunger-level 0 :percent-deteriorated 0}
>     :validator percent-deteriorated-validator))
> (swap! bobby update-in [:percent-deteriorated] + 200)
> ; This throws "Invalid reference state"
> ```

additional logic before each update of the `Atom`

a terrible design to keep using the same set of primitives while additional logic is injected

in FP, this should be lifted into a Monad by corresponding Monad Transformer

> ## Refs: Modeling Sock Transfers
>
> Refs allow you to update the state of multiple identities using transaction semantics.
> These transactions have three features:
>
> - They are *atomic*, meaning that all refs are updated or none of them are.
> - They are *consistent*, meaning that the refs  always appear to have valid states. A sock will always belong to a  dryer or a gnome, but never both or neither.
> - They are *isolated*, meaning that transactions behave as if they executed serially; if two threads are simultaneously running transactions that alter the same ref, one transaction will retry. This is similar to the compare-and-set semantics of atoms.
>
> Clojure uses *software transactional memory (STM)* to implement this behavior. 

optimistic lock with the help of the versioned immutable value

> Here’s how `alter` behaves:
>
> 1. Reach outside the transaction and read the ref’s current state.
> 2. Compare the current state to the state the ref started with within the transaction.
> 3. If the two differ, make the transaction retry.
> 4. Otherwise, commit the altered ref state.
>
> `commute`, on the other hand, behaves like this at commit time:
>
> 1. Reach outside the transaction and read the ref’s current state.
> 2. Run the `commute` function again using the current state.
> 3. Commit the result.

> ```clojure
> (def counter (ref 0))
> (future
>   (dosync
>    (alter counter inc) ; in-transition state : 1
>    (println @counter) ; => 1
>    (Thread/sleep 500)
>    (alter counter inc) ; in-transition state : 2
>    (println @counter))) ; => 2
> (Thread/sleep 250)
> (println @counter) ; => 0, the in-transition state has not been committed yet
> 
> ; result
> 1 |   0ms
> 0 | 250ms
> 2 | 500ms
> ```
>
> 

