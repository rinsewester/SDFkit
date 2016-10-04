# SDFkit
An interactive simulation and code generation platform for Synchronous Data Flow (SDF) Graphs

## What is it?
SDFkit contains two parts: a design/simulation environment and code generation.

Cyclo Static Data Flow (CSDF) graphs are a popular model of computation for creating DSP applications.
In the design/simulation environment, CSDF graphs can simulated and altered to optimize performance.
The simulation is cycle accurate when modeling an application for an FPGA.

Actual hardware is created using code generation where the graph is translated to FPGA hardware.
Since a CSDF graph is highly parallel, the parallel nature of an FPGA is exploited resulting in a high performance.

## What does it look like?
A picture is worth a thousand words:
![screenshot](https://raw.githubusercontent.com/rinsewester/SDFkit/master/images/screenshot.png)

## Setup
Run pip to install requirements `pip install -r requirements.txt`
