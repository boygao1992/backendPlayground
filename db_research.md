# I. Database Model Expressiveness
## I.i Essential Concepts on Information Structure
### Set Theory
**set operator**
- Union
$A \cup B = \{ x: x \in A \lor x \in B \}$
- Intersection
$A \cap B = \{ x: x \in A \land x \in B \}$
- Complement / Set difference
$A \setminus B = \{x: x \in A \land x \notin B \}$
- Cartesian Product
$A \times B = \{ (x,y): x \in A \land y \in B \}$

**ordered pair**
Hausdorff's definition: $(a,b) = \{\{a,1\},\{b,2\}\}$, where $a \nsim 1$ and $b \nsim 2$
Kuratowski definition: $(a,b)_K = \{\{a\},\{a,b\}\}$ (formal definition, defined by set inclusion: $\{a\} \in \{a,b\}$)
Hasse diagram

>**Burali-Forti paradox**, namely that the set of all the ordinals is an ordinal that's bigger than all the ordinals. This works because each ordinal can be defined as the set of all the ordinals that are smaller than it is. (Then an ordinal is less than another ordinal if and only if it is contained in it.)

>von Neumann's **ordinal number**:
$0 = \{\} =\emptyset$
$1 = \{0\} = \{\emptyset\}$
$2 = \{0,1\} = \{\emptyset, \{\emptyset\}\}$
$3 = \{0,1,2\} = \{\emptyset, \{\emptyset\}, \{\emptyset, \{\emptyset\}\}\}$
$\vdots$
$n + 1 = \{0, 1, 2, ..., n\}$

### Lattice (order) Theory
A lattice is a discrete subgroup of a Euclidean vector space, numbers is the theory that occupies itself with lattices.

A *binary relation* $\varrho$ on $A$ is a subset of $A^2$ or $A \times A$: $\{ (a,b) \in A^2 | a \, \varrho \, b \}$
The elements $a,b \in A$ are *in relation* with respect to $\varrho$ if $(a,b) \in \varrho$, for case we shall also use the notation $a \, \varrho \, b$. (Binary relations will be denoted by small Greek letters or by special symbols.)

**Partial Ordering Relation**
Let $P$ be a set, and let $\leq$ be a binary relation on $P$ so that
for all $a, b, c \in P$
1. Reflexivity (Refl)
$a \leq a$
2. Antisymmetry (ASym)
$a \leq b$ and $b \leq a$  imply that $a = b$
3. Transitiveity (Trans)
$a \leq b$ and $b \leq c$ imply that $a \leq c$
4. Comparability / Totality (trichotomy law)
either $a \leq b$ or $b \leq a$

If rule 1,2,3 are fulfilled, then $P$ is a *partial ordered set* (*poset*) and $\leq$ is a *partial ordering* of $P$.
If rule 4 is also fulfilled then $P$ is a *totally ordered set*.

**Equivalence Relation**
Symmetry (Sym)
$(a,b) \in \varepsilon$ implies that $(b,a) \in \varepsilon$
A binary relation $\varepsilon$ on the nonempty set $A$ satisfies (Refl), (Sym), (Trans), is called an *equivalence relation*.
If $\varepsilon$ is an equivalence relation, then the relation $(a,b) \in \varepsilon$ is often denoted as $a \equiv b \, (\bmod \varepsilon)$

**Algebraic Structures**
- Group-like
- Ring-like
- Lattice-like
- Module-like
- Algebre-like

**Group-like structures**
Totality  | Associativity | Identity | Invertibility | Commutativity
--- | --- | --- | --- | ---
Semicategory | Unneeded | Required | Unneeded | Unneeded | Unneeded
Category | Unneeded | Required | Required | Unneeded | Unneeded
Groupoid | Unneeded | Required | Required | Required | Unneeded
Magma | Required | Unneeded | Unneeded | Unneeded | Unneeded
Quasigroup | Required | Unneeded | Unneeded | Required | Unneeded
Loop | Required | Unneeded | Required | Required | Unneeded
Semigroup | Required | Required | Unneeded | Unneeded | Unneeded
Monoid | Required | Required | Required | Unneeded | Unneeded
Group | Required | Required | Required | Required | Unneeded
Abelian group | Required | Required | Required | Required | Required

### Conceptual Data Model
> The logical schema was the way data were represented to conform to the constraints of a particular approach to database management. 
> Logical data models represent the abstract structure of a domain of information.
> A logical data model or logical schema is a data model of a specific problem domain expressed independently of a particular database management product or storage technology (physical data model) but in terms of data structures such as relational tables and columns, object-oriented classes, or XML tags.

- `atom`, `bag`
the `bag` here is mathematical set permitting identical `atom`s
`ordered bag` is also derived from ordered set in algebra
> A multiset is also known as bag, list, or multiple. A multiset is a generalization of the notion of set. The members are allowed to appear more than once.
- `pair`
an `ordered bag` composed of two `atom`s, derived from ordered pair in set theory
- `map`
a `bag` of an arbitrary number of `pair`s

In database realm, Key-Value Pair is an concept derived from `pair`: (key, value)
key-type: `atom`
value-type:
- `atom`
- `bag`
- `pair`
=> `linked list`, equivalent expressiveness as `ordered bag`
- `map`
=> `nested map`

## I.ii Vertical Scaling
Name | Composition | `bag` | `pair` (`ordered bag`)
--- | --- | :---: | :---:
`atom` | undivisable unit | 0 | 0
`bag` | an arbitrary number of `atom`s | 1 | 0
`ordered bag`| introduce ordering | 0 | 1
`pair` | a `ordered bag` of two `atom`s | 0 | 1
`map` | an `unordered bag` of `pair`s | 1 | 1
`ordered map` | a `ordered bag` of `pair`s | 0 | 2
named `map` | an `atom` - `map` `pair`| 1 | 2
`nested map` | alternate `pair` and `bag` | $n$ | $m$

Higher the dimension, more flexible the structure could be.

## I.iii Horizontal Scaling
composition of multiple `nested map`s => `multi nested map`
possibly has internal mappings between `nested map`s

## I.iv Type System
1. `atom` type
  - String
  - Number
2. `bag` type
  - `bag` (default)
  - `ordered bag`
  - `hash bag` (unique `atom`)
  - `ordered hash bag`
3. `map` type
  - `map` (default)
  - `ordered map` (sorted by key/value)
  - `hash map` (unique key/value)
  - `ordered hash map`

# II. Storage and Performance

## Locality of Reference
> Relational database tables have a two-dimensional strucuture but main memory is organized unidimensional, providing memory addresses that start at zero and increase serially to the highest available location.
> The database storage layer has to decide how to map the two-dimensional table strucuture to the linear memory address space.

> Using column-based table layouts enables the use of effcient compression techniques leveraging the high data locality in columns.
> They mainly use the similarity of the data storedin a column. Dictionary encoding can be applied to row-based as well as column-based table layout,
> whereas other techniques like prefx encoding, run-length encoding, cluster encoding or indirect encoding only leverage their full benefts on columnar table layouts.

RDBMS
- row-oriented
- column-oriented
- hybrid table layouts

The data representation has direct correlation with the physical allocation.
Closer two `atom`s in the data representation are, closer their physical addresses are.

column-oriented RDBMS has better performance in column-related operations such as sum, count, min, max, average, sort.
less optimal for CRUD operation on rows

column compression
- the values in the same column are more regular so they are easier to get indexed e.g. Int8Array, same amount of field offsets
- the number of distinct values in column is relatively small
n distinct values can be turned into n seperate bitmaps
> Dictionary encoding decreases memory requirement
> and reduces the amount of data that needs to be transfered between memory and CPU, thereby increasing the performance of query execution.
> - The performance bottlenecks are memory (frequency slow than CPU) and bandwidth.
> The additional load on processor is welcomed.
> - the impact of retrieving the actual values for the encoded columns is rather small.
> When selecting tuples, only the corresponding values defined in the where clause of the query have to be looked up in the dictionary in order to perform the column scan.

column-oriented database should not be confused with column-family database.

## Physical Data Model
Page = Block
Header Page -> Data Page -> Record
Data Page should be able to fit into the Memory
Etx4 Block Size = 4KB ~ 4096KB

> An index in an addtional structure that is derived from the primary data.
Manage addtional strcutures incurs overhead, especially on writes.

> an important trade-off in storage systems: well-chosen indexes speed up read
queries, but every index slows down writes. For this reason, databases don’t usually
index everything by default, but require developers to manually choose indexes.

**Data Structure for Storage**
- Vector (ordered set)
only need one physical address to refer to a sequential block of storage space (only one key-value pair for indexing)
read/update: O(1)
(value) search: O(n)
insert: O(n)
expand if bounded (segmented): O(1) on average
append if unbounded: O(1)
implementation: Log-structured file on disk without in-memory hash map
  ```
db_set () { echo "$1,$2" >> database }
db_get () { grep "^$1," database | sed -e "s/^$1,//" | tail -n 1 }
  ```
- Hash Map (map)
(key) search: O(1)
- SSTables & Log Structured Merge Tree (LSM-tree)
> LSM-trees are typically faster for writes, whereas B-trees are thought to be faster for reads.
Reads are typically slow on LSM-trees because they have to check several different data structures
and SSTables at diffrerent stages of compaction.

- B Tree
B+ Tree (ISAM, an enhanced version)
B* Tree
Fractal Tree / Cache-Oblivious Streaming B-Tree

seperation of Index node and Storage node

**Storage Engine**
- Bitcask
  - Hash Map

  default storage engine in Riak
  > The hash map is kept completely in memory (all the keys fit in the available RAM).
The values can be loaded from disk with just one disk seek.
It that part of data file is already in the filesystem cache, a read doesn't require any disk I/O at all.

- WiredTiger
  - B-Tree
  - LSM Tree
- RocksDB
  - LSM Tree
- PerconaFT (deprecated)
  - Fractal Tree

>A storage engine like Bitcask is well suited to situations where the value for each key
is updated frequently. For example, the key might be the URL of a cat video, and the
value might be the number of times it has been played (incremented every time
someone hits the play button). In this kind of workload, there are a lot of writes, but
there are not too many distinct keys—you have a large number of writes per key, but
it’s feasible to keep all keys in memory.


**Secondary Index**

## Immutability
- Ephemeral
- Persistent

Persistent Data Structure
- Full Persistence
  - Fat Node
  - Node Splitting
- Partially Persistence
  - Fat Node
  - Path Copying
  - Fat Node & Path Copying

e.g. Hitchhiker Tree = Partially Persistent Fractal Tree

Datomic
RocksDB - Persistent Key-Value Store by Facebook

## Concurrent Access Control
Pessimistic concurrency control assumes that conflict is the expected 
condition and we have to guard against it.

### Locking Schemes
A lock is a device that gives one user session access to a resource while keeping or restricting other sessions from that resource.

pessimistic locking
The advantage of pessimistic locking is that changes to the database get made consistently and safely. The primary disadvantage is that this approach isn’t as scalable.
pessimistic locks limit the practical number of simultaneous users that your system can support.

optimistic locking
most viable concurrency control strategy
If you accept the premise that collisions infrequently occur, instead of trying to prevent them you can choose to detect and then resolve them when they do occur.

### No-lock


## Storage Media
- In-memory
- Disk

> Counterintuitively, the performance advantage of in-memory databases is not due to
the fact that they don’t need to read from disk. Even a disk-based storage engine may
never need to read from disk if you have enough memory, because the operating sys‐
tem caches recently used disk blocks in memory anyway. Rather, they can be faster
because they can avoid the overheads of encoding in-memory data structures in a
form that can be written to disk.

# III. Graph Model
concerning the performance and scalability of graph databases the main argument of proponents of this technology is the "join bomb" argument, which states that you can't efficiently store a graph in a relational database since it will require O(log(n)) time to lookup neighboring nodes from the index when crawling the graph. However, this is of course only true for B-tree indexes, whereas hash-based indexing would give you basically the same performance (O(1)) on a graph implemented in a relational database
However, the ad hoc queries that needed to be run against the data were much easier to express as graph traversals than SQL

## Directed Acyclic Graph
> it is possible to find shortest paths and longest paths from a given starting vertex in DAGs in linear time by processing the vertices in a topological order, and calculating the path length for each vertex to be the minimum or maximum length obtained via any of its incoming edges.[

## Model Complexity
## CRUD
Node Deletion
## Graph Algorithms
Traversal / Neighbors
Path Matching
Shortest Path - traverse from both source and target
## Sharding
whether the graph has a natural cluster structure that can be used for sharding

# IV. Database Analytics
## Relational Database
### Relational Model
No. | A | B
---|---|---
1 | 1A | 1B
2 | 2A | 2B

Lossless representations
**Flattened** (set of maps)
Dim-4
{
{row:1,column:A,value:1A},
{row:1,column:B,value:1B},
{row:2,column:A,value:2A},
{row:2,column:B,value:2B}
}

Uniqueness
1. rowA = rowB if rowA.value = rowB.value
2. columnA = columnB if columnA.value = columnB.value

Ordering
1. $1 \leq 2$
2. $A \leq B$

**Row-oriented** (nested map)
Embed row uniqueness by introducing a `map`
model: {row}
Dim-7
{
1:{{column:A,value:1A},{column:B,value:1B}},
2:{{column:A,value:2A},{column:B,value:2B}}
}

Embed row and column ordering by introducing a `sorted set`

{
1:({column:A,value:1A},{column:B,value:1B}),
2:({column:A,value:2A},{column:B,value:2B})
}

Extract schema from the table
schema:
1. row(1,2,...), Dim-2
2. column(A,B), Dim-2

Dim-4
(
(1A, 1B)
(2A, 2B)
)
introduced redundent information about the correlation between the table and the schema

**Column-oriented** (nested map)
{
A:{{row:1,value:1A},{row:2,value:2A}},
B:{{row:1,value:1B},{row:2,value:2B}}
}

columnar model: {column:{row:{value:}}}
{
A:{1:1A, 2:2A},
B:{1:1B, 2:2B}
}

schema:
1. column(A,B)
2. row(1,2,...)

(
(1A,2A)
(1B,2B)
)

the columnar model
- can compress data in place or store a pointer to a list of string
e.g. first_name with a look-up table like {1 = Aaron, 2 = Abe, 3 = Albert, …, 9999 = Zebadiah, …}
then it is possible to utilize a *minimal perfect hashing function* which requires that the set of search values is fixed
- rows have to be retrieved in parallel

### Row-oriented Relational Algebra
**Attribute**
A name-domain `pair`.
The name is distinct within a relation.
The domain defines the set of possible values, usually denoted as type.

**Cell**
Undivisable unit in this model, which contains an `atom` (denoted as value).
The value of a cell is consistent with the attribute type of its column.

**Tuple**
An `sorted set` of cells.
$(a_1, a_2, \dots, a_n) = (b_1, b_2, \dots, b_n)$
iff $a_1 = b_1, a_2 = b_2, \dots, a_n = b_n$

**Relation**
A table with model.
A relation consists of a header (the first row, an `orderd set` of attributes) and a body (an `ordered set` of tuples).
Each row in the body of a relation is a tuple.

**Relation Operator**
Primitive Operators on Relation
- Select ($\sigma$)
commutative
A unary operation written as $\sigma_\varphi RO$ where $\varphi$ is the predicate. The selection retrieves the tuples in $R$, where $\varphi$ holds.
- Project ($\pi$)
A unary operation, written as $\pi_{a_1,a_2,\dots,a_n}RO$, used to slice the relation in a vertical dimension, attributes ($a_1,a_2,\dots,a_n$)
> treat the set of attributes in the header as a basis in linear algebra
project a set of vector (matrix) to a different basis
= project a set of tuple (relation) to a different header
- Cartesian Product ($\times$)
- Union ($\cup$)
A binary operation that create a new relation by appending two relations together.
Require the relations to be union compatiable.
- Difference ($\cap$)
A binary operation that creates a new relation from the tuples which exists in one relation but not in the other.
Require the relations to be union compatiable.
- Rename ($\rho$)
A unary operation that renames an attribute.

Aggregation Functions (on Columns): sum, count, min, max, average.

**Foreign Key**
**Composite Key**
> When a primary key is created from a combination of 2 or more columns, the primary key is called a composite key. Each column may not be unique by itself within the database table but when combined with the other column(s) in the composite key, the combination is unique.
> Any one, none, or all, of the multiple attributes within the compound key can be foreign keys. Indeed, a foreign key may itself be a compound key.

**associative entity**
```
-- This is the junction table.
CREATE TABLE UserPermission (
    UserLogin varchar(50) REFERENCES User (UserLogin),
    PermissionKey varchar(50) REFERENCES Permission (PermissionKey),
    PRIMARY KEY (UserLogin, PermissionKey)
)
```

> A relational database requires the implementation of a base relation (or base table) to resolve many-to-many relationships. This kind of base relation is called an associative table.

> a properly normalized database, the Primary Key should be the only Unique Key in a table.
if need to declare the Composite Key (entity A, entity B) as a Unique Key to avoid duplication, declare it as the Primary Key

**Join** (inner join, $\bowtie$)
$Relation_A: \{ (tuple_1), (tuple_2), ... , (tuple_n) \}$
$Relation_B: \{ (tuple_1), (tuple_2), ... , (tuple_m) \}$
$Relation_A \bowtie Relation_B$
1. Cartesian Product
$$
Relation_A \times Relation_B =
\left\{
\begin{matrix}
	(A.1, B.1) & \dots & (A.1, B.m)\\
	\vdots & & \vdots\\
	(A.n, B.1) & \dots & (A.n, B.m)
\end{matrix}
\right\}
$$
2. Selection
e.g. $A.customer\_id = B.customer\_id$
3. Projection

> Although a join can be defined as a cross-product followed by selections and projections, joins arise much more frequently in practice than plain
cross-products. Further, the result of a cross-product is typically much larger than the result of a join, and it is very important to recognize joins and implement them without materializing the underlying cross-product.

**Natural Join**
> A NATURAL JOIN is a JOIN operation that creates an implicit join clause for you based on the common columns in the two tables being joined. Common columns are columns that have the same name in both tables.
A NATURAL JOIN can be an INNER join, a LEFT OUTER join, or a RIGHT OUTER join. The default is INNER join.

**Outer Join**
similar to inner join but add a $NULL$ tuple to each relation before conduct the cartesian product
$Relation_A: \{ (tuple_1), (tuple_2), ... , (tuple_n), (NULL) \}$
$Relation_B: \{ (tuple_1), (tuple_2), ... , (tuple_m), (NULL) \}$
1. left (outer) join
keep all the $(A.x, B.NULL)$ tuple during selection
2. right (outer) join
keep all the $(A.NULL, B.y)$ tuple during selection
3. full (outer) join
keep all the $(A.x, B.NULL)$ tuple and $(A.NULL, B.y)$ tuple during selection


## Key-Value Store
Redis

Oracle BerkeleyDB
>The architecture is based on a log-based, no-overwrite storage system, enabling high concurrency and speed while providing ACID transactions and record-level locking.

## Column Family Database
Enhanced/Multi-dimensional Table
In RDBMS realm,column family basically allows a table to be the value of a cell.

>It’s worth pointing out that the idea of grouping related data together for locality is
not limited to the document model. For example, Google’s Spanner database offers
the same locality properties in a relational data model, by allowing the schema to
declare that a table’s rows should be interleaved (nested) within a parent table [27].
Oracle allows the same, using a feature called multi-table index cluster tables [28].
The column-family concept in the Bigtable data model (used in Cassandra and
HBase) has a similar purpose of managing locality [29].

### HBase
backed with Hadoop Distributed Filesystem (HDFS)
no schema
LSM-Tree
### Cassandra
schema - CQL, application logic
LSM-Tree

## Document Store

> Comparison to document databases
Document databases reverted back to the hierarchical model in one aspect: storing
nested records (one-to-many relationships, like positions, education, and
contact_info in Figure 2-1) within their parent record rather than in a separate
table.
However, when it comes to representing many-to-one and many-to-many relation‐
ships, relational and document databases are not fundamentally different: in both
cases, the related item is referenced by a unique identifier, which is called a foreign
key in the relational model and a document reference in the document model [9].
That identifier is resolved at read time by using a join or follow-up queries. To date,
document databases have not followed the path of CODASYL.

>The main arguments in favor of the document data model are schema flexibility, bet‐
ter performance due to locality (no need to shard a high-dim tree into low-dim tables), and that for some applications it is closer to the data
structures used by the application. The relational model counters by providing better
support for joins, and many-to-one and many-to-many relationships.

### MongoDB
pluggable architecture for storage engines

### Couchbase

## Graph Database

Graph Layer
- Apache Giraph
>Apache Giraph is an iterative graph processing framework, built on top of Apache Hadoop.
- Apache Tinkerpop
- Google Badwolf (Quad, Temporal RDF)

Query Language
- Gremlin (Apache Tinkerpop)
- AQL (ArangoDB)
- Cypher (Neo4j)
- MQL (Cayley)
- GraphQL
- SPARQL


### ArangoDB
Storage Engine - The current storage engine based on memory mapped files and a new one backed by RocksDB (Key-Value)

### DGraph
Storage Engine - RocksDB (Key-Value)
> Quick point about async: Every write to Dgraph first gets written to a commit log and flushed out to disk. Then it gets applied to the posting list. Periodically, posting lists get merged back to RocksDB. The writes are async. This is okay because even if the machine crashes, the commit logs would still have those writes, and when the posting list is initialized again, it would pick up those writes and apply them back. So, there's no data loss.

### Cayley
pluggable architecture for storage engines
- LevelDB (Key-Value, an ordered mapping from string keys to string values sorted by key)
- Bolt (Key-Value, an embedded database for Go)
- PostgreSQL (Relational Store with JSON support)
- MongoDB for distributed stores (Document Store)
- In-memory, ephemeral

### TitanDB / JanusGraph
Pluggable architecture for storage engines
- Oracle BerkeleyDB (Key-Value)
- Apache Cassandra, Apache HBase (Column-Family)

Graph Layer
- Apache Giraph
- Apache Tinkerpop (Gremlin)

Fulltext Index Engine
- ElasticSearch
- Solr (Lucene)

> storage engine that are based on merging and compacting sorted files are often called LSM storage engines.
> Lucene, an indexing engine for full-text search used by Elasticsearch and Solr, uses a similar method for storing its term dictionary.

# Summary
Our company is a data company but not a database company. We are supposed to use preexistent tools instead of crafting ones by ourselves.
- limited time and resources
- limited experience

We should use the most suitable tools to solve the specific problems in different scenarios.
- specific database has inner optimization for different data access patterns which cannot be achieved in our application layer due to insufficient inner function accessibility
- save time and resources

>effectively performing a client-side join, although this is likely to be slower than a
join performed in the database since it requires additional network round-trips and is
less optimized