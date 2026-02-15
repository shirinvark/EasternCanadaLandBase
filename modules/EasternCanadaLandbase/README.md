# EasternCanadaLandbase

EasternCanadaLandbase assembles and maintains the spatial landbase used for
planning and analysis in Eastern Canada.

This module gathers prepared spatial inputs (e.g. planning grid, aligned land
cover, protected areas, riparian layers) into a single, consistent landbase
object. It does **not** perform any ecological or management classification.

In particular, this module does **not**:
- classify forested vs. non-forested land,
- distinguish managed vs. unmanaged forests,
- define stands or analysis units,
- perform harvesting or AAC calculations.

Classification of managed forested stands into analysis units is expected to be
implemented in downstream modules that operate on this landbase.

## Outputs

The main output of this module is:

- `sim$Landbase`: a container holding the spatial foundation for downstream
  analyses.

## Role in the workflow

EasternCanadaLandbase serves as a handoff layer between data preparation modules
and analytical or classification modules (e.g. harvesting, AAC, habitat models).

It is intentionally decision-free and policy-neutral.
