=================
Graph file format
=================

SDFkit uses a file format for graphs is based on JSON.
The format consist of options for that apply to the whole graph, a set of
nodes and a set of edges.


Graph options
-------------
Since the JSON format is a key-value format, a graph in JSON is described
as a number of options that apply to the whole graph followed by a list of
nodes and a list of edges. At the highest level, the following keys-values
can be used:

**name**:
   Name of the graph.

   *string* : required

   Not only is the name used purely descriptive, the name is also used for the
   filename during CLaSH code generation and module name.

**clashtypes**:
   CLaSH type definitions to be used in the CLaSH code in the nodes

   *list of "typename", "typdef" pairs* : optional

**nodes**:
   The nodes of the graph.

   *list of node objects* : required

   The formatting of node objects is decribed in the section `Node`_.

**edges**:
   The edges of the graph.

   *list of edge objects* : required

   The formatting of edge objects is decribed in the section `Edge`_.


Node
-----
nodes options


Edge
-----
edge options


Small example
-------------
nodes options