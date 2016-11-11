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

src
dst
resnr
argnr
prates
crates
tkns
color optional




Small example
-------------
nodes options