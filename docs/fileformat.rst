.. _sect-graph-file-format:

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
A node is a JSON object with the following attributes:

**name**:
   Name of the node.

   *string* : required

   Not only is the name of the node used for creating edges, it is
   also used  for naming the CLaSH code functions and a lot of
   signals in the generated code.

**pos**:
   Position of the node

   *list of int* : required

   The position of the node is a list of an x and y position: [x,y]
   All nodes are placed in a grid. The vertical resolution is 20 
   units while the horizontal resolution is 50. In SDFkit nodes can
   be moved by dragging and saving the graph.

**function**:
   Function executed in the node.

   *string* : required

   The functionality of a node is implemented as lambda expression. This
   expression is given as a string and is intepreted using the eval() 
   function in python. The format of these lambda function can be found
   in TODO.

**clashcode**:
   CLaSh code of the node.

   *string* : optional

   The python code is not translated autmatically to CLaSh code. Therefore,
   this has to be given as well. This field is optional since the graph can
   be simulated without CLaSh code. The format of the CLaSh can be found
   in TODO.

**color**:
   Color of the node.

   *list of int* : optional

   In order to highlight certain parts of the graph, nodes can be colored.
   The color is given as an rgb list [r,g,b]. Red is expressed as [255, 0, 0].


Edge
-----
Edges are JSON objects connected two nodes and has the following attributes:

**src**:
   Source node.

   *string* : required

**dst**:
   Destination node.

   *string* : required

**resnr**:
   Results number.

   *int* : required

   When a node has several outputs, the node function produces a tuple 
   with several sets of tokens, i.e., a set for every output. In order to
   construct an edge, not only is a node required but also the the output
   number to which the edge is connected. For example, when resnr equals 1
   the edge is connected to the second output (the second element in the
   result tuple).

**argnr**:
   Argument number.

   *int* : required

   Similar to the result number but this number indicates to which input
   an edge is connected. For example, when argnr equals 1 the edge is
   connected to the second input (the second argument of the node function).

**prates**:
   Production rates.

   *list of int* : required

   The production rates of the edge indicates how many tokens are put on
   the edge during a single firing. The production is a list of integers
   but repeating numbers can be expressed more using the following syntax:
   [1, "2*4", 3, "3*2"] becomes: [1, 4, 4, 3, 2, 2, 2]

**crates**:
   Consumption rates.

   *list of int* : required

   The consumption rates of the edge indicates how many tokens are removed from
   the edge during a single firing. The syntax is the same as is used for the 
   production rates.

**tkns**:
   Tokens on the edge.

   *list of anyt type* : required

   The initial set of tokens on the edge before any simulation has been performed.
   An edge without any tokens is indicated using an empty list: [].

**color**:
   Color of the edge.

   *list of int* : optional

   In order to highlight certain parts of the graph, edges can be colored in
   the same way as nodes. The color is given as an rgb list [r,g,b]. Green is 
   therefore expressed as [0, 255, 0].


Python node functions
---------------------
The functions executed in nodes are a python lambda expression with special
structure and naming. Every node function is a string which is interpreted
using python's *eval* function. Node fundtions are expressed in the following
pattern:

.. code-block:: python
   :linenos:

   lambda inputs firecounter, phase: results

Every node function always receives at least two arguemnts, *firecounter* 
and *phase*. *firecounter* indicates how many times the node has fired and
*phase* is a counter to track the phase of the node. Note that *phase* is
only used in CSDF graphs, in all other graphs *phase* remains 0.

For every input of the node, an argument is added before *firecounter*.
When the node has no inputs, only *firecounter* and *phase* are used
as arguments.

The reults of the lambda expressed depends on the number of outputs. The
results are therefore represented as a single list or a tuple with lists.
When a node has zero or one outputs, the lambda returns a list of tokens
or an empty list respectively. For nodes having multiple outputs, every
list of tokens becomes an element in the result tuple.

The following line shows a function for a node that produces data on two
outputs and has no inputs:

.. code-block:: python
   :linenos:

   lambda firecounter, phase: ([firecounter], [firecounter + 1])

Nodes that function as sink have an input and no outputs as is therefore
defined as:

.. code-block:: python
   :linenos:

   lambda d_in, firecounter, phase: []


Node CLaSH code
---------------
TODO: explain that the clash code adheres to a pattern as well.
small note: types are one word -> types are used to generated FIFOs.

