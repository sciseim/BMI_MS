# BMI_MS
Code and data for the MS: 
Inge Seim, Penny L. Jeffery, Lisa K. Chopin. *Gene expression profiling of The Cancer Genome Atlas supports an inverse association between body mass index (BMI) and major oesophageal tumour subtypes*.

**Summary**:

*`0-copy_gene_exp-into_dirs_sh`*:
- use this to x GDC downloaded data and copy files to the directory ./FPKM-UQ/FPKM
- GDC data was downloaded and copied into the folder FPKM-UQ (downloaded 03.05.2018)
- will obtain *`FPKM-UQ.tar.gz`* (1.62 GB)
-- Please uncompress this to obtain GDC data (FPKM-UQ downloaded 03.05.2018).

*`scripts 1 to 24 are used for various downstream analyses`*:
- `1-parse-gene-exp-data.R`
- `2-load-sample-data.R`
- `3-ensure-geneEXPdf-can-see-SampleInfoDF.R`
- `4-checking-that-FileName-is-the-link.R`
- `5-add-BMI-groups-remove-BMI-outliers.R`
- `6-keep-geneExp-with-sampleinfo-ONLY.R`
- `7-prepare-the-BigDF.R`
- `8-testing-before-looping-through-all.R`
- `9A-select-datasets.R`
- `9B-Spearman-loop.R`
- `10-Table1.R`
- `10B-extract_results_per_dataset.R`
- `11-output-stats-MannWhitneyUtest.R`
- `12-heatmap.R`
- `12B-heatmap-extended-ESCA.R`
- `14-survival-BMI-groups-per-TCGA.R`
- `15-BMI-histograms.R`
- `16-ACvsESCC.R`
- `17A-select-EC-subsets.R`
- `17B-EC-subset-Spearman-loop.R`
- `18-low-vs-high-BMI-groups-Cell-data.R`
- `18-low-vs-high-BMI-groups.R`
- `19-filtering-post-rho-0.3.R`
- `20-ESCA genes boxplot with jitter.R`
