## Stochastic model of CIN and selection
Population-level karyptypic variation is dependent on 4 factors:<br>
1. Chromosome missegregation rate
2. Positive or negative selection on aneuploid cells
3. The number of cellular divisions that occur
4. The periodicity of chromosomal instability (i.e. how *often* it occurs)

These documents will serve as version control for the agent-based stochastic models of these factors with selection based on normalized gene numbers from each chromosome. All models were developed in NetLogo. 

**CINModelMaster.nlogo3d** is the fundamental framework on which all other files in the ModelingScripts folder are built on.

---
## Description of the model
### Underlying assumptions
1. Chromosomes with more genes "contribute" more to cellular fitness (i.e. a cell with 1 copy of chromosome 1 will have lower fitness than a cell with 1 copy of chromosome 18).
2. The relationship between cellular fitness and ploidy follows a multimodal distribution with local maxima representing balanced, euploid karyotypes and minima representing unbalanced karyotypes. 
3. Cells with a copy number of zero of any chromosome die.
4. Cells whose fitness falls below 0.5 die. 

### General framework
**After initialization, all agents (or "tumor cells") go through the following processes at each time step**
*Note: these are general steps*
#### Initialization
![Initialization](/Images/InitializationProcedure.png)
#### At each time step
**Circle** = system command<br>
**Rectangle** = agent command<br>
**Diamond** = negative agent command<br><br>
![Iteration](/Images/IterationProcedure.png)
