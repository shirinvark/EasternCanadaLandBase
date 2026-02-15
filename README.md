# EasternCanadaLandbase

EasternCanadaLandbase assembles and maintains the spatial landbase used for
planning and analysis in Eastern Canada.

This module gathers prepared spatial inputs (e.g., planning grid, aligned land
cover, protected areas, riparian layers) into a single, consistent landbase
object. It does **not** perform ecological interpretation or management
classification.

## What this module does NOT do

This module intentionally does **not**:

- Classify forested vs. non-forested land  
- Distinguish managed vs. unmanaged forests  
- Define stands or analysis units  
- Perform harvesting simulations  
- Compute AAC (Annual Allowable Cut)  

All classification, yield modeling, and management logic is expected to be
implemented in downstream analytical modules.

## Outputs

The primary output of this module is:

- `sim$Landbase`  
  A structured container that holds the spatial foundation for downstream
  modeling and analysis.

## Role in the Workflow

EasternCanadaLandbase functions as a structural handoff layer between:

- Data preparation modules  
- Analytical / decision modules (e.g., harvesting, AAC, habitat modeling)

The module is intentionally decision-neutral and policy-agnostic.
It provides a stable, reproducible spatial base upon which management
logic can safely operate.
