# Inhibition of return and learned value

This project is the repository for all data, scripts, and materials used for the manuscript entitled: "***Inhibition of Return and Learned Value***".

### Project Structure ###

The project is organized into the following main sections:

- **Input**: Where the raw data of Experiment 1 and Experiment 2 are stored.
- **Output**: R objects of the analyses, figures generated from **scripts**, and the rendered manuscript.
- **Scripts**: Includes scripts used to load and preprocess data, and execute the analysis.
- **Materials**: Contains the OpenSesame tasks employed in Experiment 1 and Experiment 2.
- **_extensions**: Quarto extension used to render the manuscript (`ior_lv_manuscript.qmd`) with the *Psicológica* template.

##### Scripts Folder Structure #####

The **scripts** folder is straightforward:

- *analysis_e1.R*: The script used in the **Results** section of *Experiment 1*.
- *analysis_e2.R*: The script used in the **Results** section of *Experiment 2*.
- *analysis_pooled.R*: The script used in the **Pooled analyses of Experiments 1 and 2** section.
- *meta_analysis.R*: The script for the meta-analysis reported in the **General Discussion**.
- *power.R*: The script for the sensitivity power analysis.

All the above .R files implicitly call `source(...)` to run *load_data.R* and *functions.R*. In the former, data are loaded and preprocessed; in the latter, helper functions are loaded into the workspace. `run_all.R` runs all the scripts (cached results in **Output** are used when available).

### License ###

Data, code, and materials are released under a [CC BY 4.0](LICENSE.md) license.

### Links ###

- Preregistration of Experiment 1: <https://osf.io/fa6v9>
- Preregistration of Experiment 2: <https://osf.io/r2yf5>
- Zenodo repository: [ZENODO DOI]
