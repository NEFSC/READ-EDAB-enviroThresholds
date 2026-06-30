---
title: "README_archive"
output: html_document
---

# Exploratory Analysis Archive

This folder contains the historical, exploratory scripts used during the development of the V6 Thermal Habitat Risk Indicator. 

**Why are these archived?**
During methodology development, we tested several iterations of the indicator, including:
* Averaging across all four seasons.
* State-trend scoring.
* Alternative climatological baselines (e.g., fixed vs. rolling).

**Conclusion:**
Ultimately, the 4-season average method (and others contained here) was rejected. We found that averaging washed out severe seasonal thermal bottlenecks (e.g., a total collapse of summer habitat masked by an average winter). The final production suite (located in the root `/R` folder) was adopted to isolate the single season of greatest change for each species.