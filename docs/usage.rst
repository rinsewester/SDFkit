.. _sect-using-sdfkit:

************
Using SDFkit
************

Getting and running
----------
SDFkit is written in python 3 and only requires networkx and PyQt5.
The source can be found on github::

   git clone https://github.com/rinsewester/SDFkit.git
   cd SDFkit/

Using pip we can install the dependencies for SDFkit::

   pip3 install -r requirements.txt

After installing the dependencies, SDFkit can be started using::

   python3 mainwindow.py

Simulating graphs
-----------------
TODO: explain how to simulate graphs, edit tokens, rates and code.

Examples
--------
Batteries are included. A number of example graphs can be found
in the examples. The graphs are categorized according to type in
folders:

- HSDF
   - distinct outputs.json
   - nine counter.json
   - producer consumer.json
- SDF
   - simple graph.json
- CSDF
   - alternating merge.json
   - list splitter.json
   - producer consumer.json
   - sliding window.json
