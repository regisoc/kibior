

![](https://github.com/regisoc/kibior/blob/master/inst/logo/kibior.png)

# kibior: easy scientific data handling, searching and sharing with Elasticsearch

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Build Status](https://travis-ci.com/regisoc/kibior.svg?branch=master)](https://travis-ci.com/regisoc/kibior)

Version: `0.1.0`

## TL;DR

| | |
|-|-|
| What  | `kibior` is a R package dedicated to ease the pain of data handling in science, and more notably with biological data. | 
| Where | `kibior` is using `Elasticsearch` as database and search engine. |
| Who   | `kibior` is built for data science and data manipulation, so when any data-related action or need is involved, notably `sharing data`. It mainly targets bioinformaticians, and more broadly, data scientists. |
| When  | Available now from this repository, or [CRAN repository](https://cran.r-project.org/package=kibior). |
| Public instances | Use the `$get_kibio_instance()` method to connect to `Kibio` and access known datasets. See `Kibio datasets` at the end of this document for a complete list. |
| Cite this package | In R session, run `citation("kibior")` |
| Publication | `coming soon`. |



## Main features

This package allows:

- `Pushing`, `pulling`, `joining`, `sharing` and `searching` tabular data between an R session and one or multiple Elasticsearch instances/clusters. 
- `Massive data query and filter` with Elasticsearch engine.
- `Multiple living Elasticsearch connections` to different addresses.
- `Method autocompletion` in proper environments (e.g. R cli, RStudio). 
- `Import and export datasets` from an to files.
- `Server-side execution` for most of operations (i.e. on Elasticsearch instances/clusters).


## How

### Install

```r
# Get from CRAN
install.packages("kibior")

# or get the latest from Github
devtools::install_github("regisoc/kibior")
```

### Run

```r
# load
library(kibior)

# Get a specific instance
kc <- Kibior$new("server_or_address", port)

# Or try something bigger...
kibio <- Kibior$get_kibio_instance()
kibio$list()

```

## Examples 

Here is an extract of some of the features proposed by `KibioR`. 
See `Introduction` vignette for more advanced usage.

### Example: `push` datasets

```r
# Push data (R memory -> Elasticsearch)
dplyr::starwars %>% kc$push("sw")
dplyr::storms %>% kc$push("st")
```

### Example: `pull` datasets

```r
# Pull data with columns selection (Elasticsearch -> R memory)
kc$pull("sw", query = "homeworld:(naboo || tatooine)", 
              columns = c("name", "homeworld", "height", "mass", "species"))
# see vignette for query syntax
```

### Example: `copy` datasets

```r
# Copy dataset (Elasticsearch internal operation)
kc$copy("sw", "sw_copy")
```

### Example: `delete` datasets

```r

# Delete datasets
kc$delete("sw_copy")
```

### Example: `list`, `match` dataset names

```r
# List available datasets
kc$list()

# Search for index names starting with "s"
kc$match("s*")
```

### Example: get `columns` names and list unique `keys` in values

```r
# Get columns of all datasets starting with "s"
kc$columns("s*")

# Get unique values of a column
kc$keys("sw", "homeworld")
```

### Example: some Elasticsearch basic statistical methods 

```r
# Count number of lines in dataset
kc$count("st")

# Count number of lines with query (name of the storm is Anita)
kc$count("st", query = "name:anita")

# Generic stats on two columns
kc$stats("sw", c("height", "mass"))

# Specific descriptive stats with query
kc$avg("sw", c("height", "mass"), query = "homeworld:naboo")
```

### Example: `join`

```r
# Inner join between:
#   1/ a Elasticsearch-based dataset with query ("sw"), 
#   2/ and a in-memory R dataset (dplyr::starwars) 
kc$inner_join("sw", dplyr::starwars, 
              left_query = "hair_color:black",
              left_columns = c("name", "mass", "height"),
              by = "name")
```

## Kibio datasets

Snapshot: `2020-11-19`. Might not be exact. Connect to the Kibio repository to access the current datasets.

| Index name | Source | Dataset | Version | Date (YYYY-MM-DD) | Reference | License |
| ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| pub_biogrid_v4 | BioGRID |  | 3.5.188 | 2020-07-25 | [doi:10.1093/nar/gkj109](https://doi.org/10.1093/nar/gkj109) | MIT: https://biogrid-downloads.nyc3.digitaloceanspaces.com/LICENSE.txt|
| pub_chembl_27_activities | ChEMBL | Activities | 27 | 2020-05 | [doi:10.1093/nar/gkw1074](https://doi.org/10.1093/nar/gkw1074) | CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/ |
| pub_chembl_27_assays | ChEMBL | Assays | 27 | 2020-05 | [doi:10.1093/nar/gkw1074](https://doi.org/10.1093/nar/gkw1074) | CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/ |
| pub_chembl_27_cells | ChEMBL | Cells | 27 | 2020-05 | [doi:10.1093/nar/gkw1074](https://doi.org/10.1093/nar/gkw1074) | CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/ |
| pub_chembl_27_chemreps | ChEMBL | Chemical representations | 27 | 2020-05 | [doi:10.1093/nar/gkw1074](https://doi.org/10.1093/nar/gkw1074) | CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/ |
| pub_chembl_27_compounds | ChEMBL | Compounds | 27 | 2020-05 | [doi:10.1093/nar/gkw1074](https://doi.org/10.1093/nar/gkw1074) | CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/ |
| pub_chembl_27_documents | ChEMBL | Documents | 27 | 2020-05 | [doi:10.1093/nar/gkw1074](https://doi.org/10.1093/nar/gkw1074) | CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/ |
| pub_chembl_27_targets | ChEMBL | Targets | 27 | 2020-05 | [doi:10.1093/nar/gkw1074](https://doi.org/10.1093/nar/gkw1074) | CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/ |
| pub_chembl_27_tissues | ChEMBL | Tissues | 27 | 2020-05 | [doi:10.1093/nar/gkw1074](https://doi.org/10.1093/nar/gkw1074) | CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/ |
| pub_dbsnp | NCBI dbSNP |  | v151, Ch38p13 | 2018-04-27 | [doi:10.1093/nar/29.1.308](https://doi.org/10.1093/nar/29.1.308)  [doi:10.1093/nar/28.1.352](https://doi.org/10.1093/nar/28.1.352)  | NCBI policy: https://www.ncbi.nlm.nih.gov/home/about/policies/|
| pub_disgenet_gene_disease_v6 | DisGeNET | Gene disease association | 7.0 | 2020-05-04 | [doi:10.1093/nar/gkz1021](https://doi.org/10.1093/nar/gkz1021) [doi:10.1093/nar/gkw943](https://doi.org/10.1093/nar/gkw943) [doi:10.1093/database/bav028](https://doi.org/10.1093/database/bav028)  | as-is: https://www.disgenet.org/static/disgenet_ap1/files/downloads/readme.txt|
| pub_disgenet_variant_disease_v6 | DisGeNET | Variant disease association | 7.0 | 2020-05-04 | [doi:10.1093/nar/gkz1021](https://doi.org/10.1093/nar/gkz1021) [doi:10.1093/nar/gkw943](https://doi.org/10.1093/nar/gkw943) [doi:10.1093/database/bav028](https://doi.org/10.1093/database/bav028)  | as-is: https://www.disgenet.org/static/disgenet_ap1/files/downloads/readme.txt|
| pub_drugbank_proteins | DrugBank | Proteins identifiers | 5.1.7 | 2020-07-02 | [doi:10.1093/nar/gkx1037](https://doi.org/10.1093/nar/gkx1037) [doi:10.1093/nar/gkt1068](https://doi.org/10.1093/nar/gkt1068) [doi:10.1093/nar/gkq1126](https://doi.org/10.1093/nar/gkq1126) [doi:10.1093/nar/gkm958](https://doi.org/10.1093/nar/gkm958) [doi:10.1093/nar/gkj067](https://doi.org/10.1093/nar/gkj067) | https://www.drugbank.ca/legal/eula   https://www.drugbank.ca/legal/terms_of_use|
| pub_drugbank | DrugBank |  | 5.1.7 | 2020-07-02 | [doi:10.1093/nar/gkx1037](https://doi.org/10.1093/nar/gkx1037) [doi:10.1093/nar/gkt1068](https://doi.org/10.1093/nar/gkt1068) [doi:10.1093/nar/gkq1126](https://doi.org/10.1093/nar/gkq1126) [doi:10.1093/nar/gkm958](https://doi.org/10.1093/nar/gkm958) [doi:10.1093/nar/gkj067](https://doi.org/10.1093/nar/gkj067) | https://www.drugbank.ca/legal/eula   https://www.drugbank.ca/legal/terms_of_use|
| pub_dsigdb | Drug Signatures Database (DSigDB) |  | 1.0 | 2015-05 | [doi:10.1093/bioinformatics/btv313](https://doi.org/10.1093/bioinformatics/btv313) | freely available for non-commercial use|
| pub_ensembl_homo_sapiens | Ensembl | Stable identifiers for Homo Sapiens | release 100, GRCh38 | 2020-04-29 | [doi:10.1093/nar/gkz966](https://doi.org/10.1093/nar/gkz966) | https://useast.ensembl.org/info/about/legal/disclaimer.html|
| pub_entrez_gene_v2 | NCBI Entrez Gene |  | 2020-04-08 | 2020-06-25 | [doi:10.1093/nar/gkq1237](https://doi.org/10.1093/nar/gkq1237) | https://www.ncbi.nlm.nih.gov/home/about/policies/|
| pub_gencode_v3 | GENCODE |  | Release 35 (GRCh38.p13) | 2020-04-08 | [doi:10.1093/nar/gky955](https://doi.org/10.1093/nar/gky955) [doi:10.1101/gr.135350.111](https://doi.org/10.1101/gr.135350.111)  | https://www.ebi.ac.uk/about/terms-of-use/|
| pub_gtex_v8_metadata | GTEx | Metadata | 8 | 2019-08-26 | [doi:10.1126/science.1262110](https://doi.org/10.1126/science.1262110)  | public: https://www.gtexportal.org/home/datasets, protected raw data: https://gtexportal.org/home/protectedDataAccess|
| pub_gtex_v8 | GTEx | TPM values | 8 | 2019-08-26 | [doi:10.1126/science.1262110](https://doi.org/10.1126/science.1262110)  | public: https://www.gtexportal.org/home/datasets, protected raw data: https://gtexportal.org/home/protectedDataAccess|
| pub_hagr_ageingmap_v2 | HAGR | Ageing Map (Digital Ageing Atlas) | Build 5 | 2014-09-18 | [doi:10.1093/nar/gku843](https://doi.org/10.1093/nar/gku843)  | Use of the Digital Ageing Atlas is free but subject to the conditions of the Human Ageing Genomic Resources: http://genomics.senescence.info/legal.html.|
| pub_hagr_anage_v2 | HAGR | AnAge | Build 14 | 2017-10-14 | [doi:10.1093/nar/gkx1042](https://doi.org/10.1093/nar/gkx1042)  | AnAge is made freely available to everyone under the terms and conditions described in HAGR's license: https://www.genomics.senescence.info/legal.html|
| pub_hagr_drugage_v2 | HAGR | DrugAge | Build 3 | 2019-07-09 | [doi:10.1111/acel.12585](https://doi.org/10.1111/acel.12585)  | Use of the DrugAge is free but subject to the conditions of the Human Ageing Genomic Resources: http://genomics.senescence.info/legal.html.|
| pub_hagr_genage_v2 | HAGR | GenAge | Build 20 | 2020-02-09 | [doi:10.1093/nar/gkx1042](https://doi.org/10.1093/nar/gkx1042)  | GenAge is made freely available to everyone under the terms and conditions described in HAGR's license: https://www.genomics.senescence.info/legal.html.|
| pub_hagr_gendr_v2 | HAGR | GenDR | Build 4 | 2017-06-24 | [doi:10.1371/journal.pgen.1002834](https://doi.org/10.1371/journal.pgen.1002834)  | |
| pub_hagr_longevitymap | HAGR | Longevity Map | Build 3 | 2017-06-24 | [doi:10.1016/j.tig.2013.08.003](https://doi.org/10.1016/j.tig.2013.08.003) | |
| pub_hcop_hsa | HGNC (HCOP) | Human - all ortholog species data | 2020-08-19 | 2020-08-19 | [doi:10.1007/s00335-005-0103-2](https://doi.org/10.1007/s00335-005-0103-2)  [doi:10.1093/bib/bbl030](https://doi.org/10.1093/bib/bbl030)  [doi:10.1093/nar/gkq892](https://doi.org/10.1093/nar/gkq892)  | ftp://ftp.ebi.ac.uk/pub/databases/genenames/README.txt|
| pub_hgnc | HGNC |  | 2020-08-19 | 2020-08-19 | [doi:10.1093/nar/gkw1033](https://doi.org/10.1093/nar/gkw1033)  | ftp://ftp.ebi.ac.uk/pub/databases/genenames/README.txt|
| pub_hmdb_metabolites_v2 | HMDB | Metabolites | 4.0 | 2019-01-16 | [doi:10.1093/nar/gkl923](https://doi.org/10.1093/nar/gkl923) [doi:10.1093/nar/gkn810](https://doi.org/10.1093/nar/gkn810) [doi:10.1093/nar/gks1065](https://doi.org/10.1093/nar/gks1065) [doi:10.1093/nar/gkx1089](https://doi.org/10.1093/nar/gkx1089)  | https://hmdb.ca/downloads 'HMDB is offered to the public as a freely available resource. Use and re-distribution of the data, in whole or in part, for commercial purposes requires explicit permission of the authors and explicit acknowledgment of the source material (HMDB) and the original publication (see below). We ask that users who download significant portions of the database cite the HMDB paper in any resulting publications.'|
| pub_hmdb_proteins_v2 | HMDB | Proteins | 4.0 | 2019-01-14 | [doi:10.1093/nar/gkl923](https://doi.org/10.1093/nar/gkl923) [doi:10.1093/nar/gkn810](https://doi.org/10.1093/nar/gkn810) [doi:10.1093/nar/gks1065](https://doi.org/10.1093/nar/gks1065) [doi:10.1093/nar/gkx1089](https://doi.org/10.1093/nar/gkx1089)  | https://hmdb.ca/downloads 'HMDB is offered to the public as a freely available resource. Use and re-distribution of the data, in whole or in part, for commercial purposes requires explicit permission of the authors and explicit acknowledgment of the source material (HMDB) and the original publication (see below). We ask that users who download significant portions of the database cite the HMDB paper in any resulting publications.'|
| pub_innatedb | InnateDB | Curated genes | 2018-07-05 | 2018-07-05 | [doi:10.1093/nar/gks1147](https://doi.org/10.1093/nar/gks1147)  [doi:10.1186/1752-0509-4-117](https://doi.org/10.1186/1752-0509-4-117)  [doi:10.1038/msb.2008.55](https://doi.org/10.1038/msb.2008.55)  | https://www.innatedb.com/license.jsp (Design Science License)|
| pub_interpro | InterPro | UniProtKB, proteins enriched with names and entries | 81.0 | 2020-08-13 | [doi:10.1093/nar/gky1100](https://doi.org/10.1093/nar/gky1100)  | https://www.ebi.ac.uk/about/terms-of-use/|
| pub_mgi_entrezgene | MGI | Marker associations to entrez gene | 2020-08-17 | 2020-08-17 | [doi:10.1002/0471250953.bi0107s05](https://doi.org/10.1002/0471250953.bi0107s05)  [doi:10.1093/nar/gky1056](https://doi.org/10.1093/nar/gky1056)  [doi:10.1158/0008-5472.CAN-17-0584](https://doi.org/10.1158/0008-5472.CAN-17-0584)  | http://www.informatics.jax.org/mgihome/other/copyright.shtml|
| pub_mirbase_v2 | miRBase |  | 22.1 | 2018-08 | [doi:10.1038/nrg1334](10.1038/nrg1334)  [doi:10.1093/nar/gkt1181](10.1093/nar/gkt1181)  [doi:10.1093/nar/gkq1027](10.1093/nar/gkq1027)  [doi:10.1093/nar/gkm952](10.1093/nar/gkm952)  [doi:10.1093/nar/gkj112](10.1093/nar/gkj112)  [doi:10.1093/nar/gky1141](10.1093/nar/gky1141)  | ftp://mirbase.org/pub/mirbase/22.1/LICENSE|
| pub_mirtarbase_v8 | miRTarBase | all published miRNA target interaction data | 8.0 | 2019-06-30 | [doi:10.1093/nar/gkx1067](https://doi.org/10.1093/nar/gkx1067)  [doi:10.1093/nar/gkz896](https://doi.org/10.1093/nar/gkz896)  | http://mirtarbase.cuhk.edu.cn/cache/download/LICENSE (as is)|
| pub_ncbi_virus_v2 | NCBI virus |  | 2020-08-21 | 2020-08-21 | [doi:10.1093/nar/gkw1065](https://doi.org/10.1093/nar/gkw1065)  | https://www.ncbi.nlm.nih.gov/home/about/policies/|
| pub_protein_atlas_v19_3 | The Human Protein Atlas |  | 19.3 | 2020-03-06 | [doi:10.1126/science.1260419](https://doi.org/10.1126/science.1260419)  [doi:10.1126/science.aal3321](https://doi.org/10.1126/science.aal3321)  [doi:10.1126/science.aan2507](https://doi.org/10.1126/science.aan2507)  | https://www.proteinatlas.org/about/licence|
| pub_pubmed_v4 | NCBI PubMed |  | 2020-08-21 | 2020-08-21 | NCBI PubMed website: https://pubmed.ncbi.nlm.nih.gov/ | https://www.ncbi.nlm.nih.gov/home/about/policies.shtml|
| pub_pubtator_v2_bioconcept | PubTator | Bioconcepts | 2 | 2020-08-21 | [doi:10.1093/nar/gkz389](https://doi.org/10.1093/nar/gkz389)  [doi:10.1093/nar/gkt441](https://doi.org/10.1093/nar/gkt441)  [doi:10.1093/bioinformatics/btv760](https://doi.org/10.1093/bioinformatics/btv760)  [doi:10.1093/bioinformatics/btz070](https://doi.org/10.1093/bioinformatics/btz070)  | https://www.ncbi.nlm.nih.gov/home/about/policies.shtml|
| pub_reactome_73_identifiers | Reactome | External identifiers | 73 | 2020-06-09 | [doi:10.1093/nar/gkz1031](https://doi.org/10.1093/nar/gkz1031) [doi:10.1371/journal.pcbi.1005968](https://doi.org/10.1371/journal.pcbi.1005968) [doi:10.1093/bioinformatics/btx752](https://doi.org/10.1093/bioinformatics/btx752) [doi:10.1093/bioinformatics/btx441](https://doi.org/10.1093/bioinformatics/btx441) [doi:10.1186/s12859-017-1559-2](https://doi.org/10.1186/s12859-017-1559-2) [doi:10.1007/978-1-4939-6783-4_11](https://doi.org/10.1007/978-1-4939-6783-4_11) "reactome_stable_ids.txt", Reactome, 73, https://reactome.org/download-data/ (2020-08-30). | https://reactome.org/license CC0 1.0 https://creativecommons.org/licenses/by/4.0/ |
| pub_reactome_73_mapping | Reactome | Identifiers mapping | 73 | 2020-06-09 | [doi:10.1093/nar/gkz1031](https://doi.org/10.1093/nar/gkz1031) [doi:10.1371/journal.pcbi.1005968](https://doi.org/10.1371/journal.pcbi.1005968) [doi:10.1093/bioinformatics/btx752](https://doi.org/10.1093/bioinformatics/btx752) [doi:10.1093/bioinformatics/btx441](https://doi.org/10.1093/bioinformatics/btx441) [doi:10.1186/s12859-017-1559-2](https://doi.org/10.1186/s12859-017-1559-2) [doi:10.1007/978-1-4939-6783-4_11](https://doi.org/10.1007/978-1-4939-6783-4_11) "*2Reactome_All_levels.txt" and "*2ReactomeReactions.txt", Reactome, 73, https://reactome.org/download-data/ (2020-08-30). | https://reactome.org/license CC0 1.0 https://creativecommons.org/licenses/by/4.0/ |
| pub_reactome_73_pe_mapping | Reactome | Physical entity identifiers mapping | 73 | 2020-06-09 | [doi:10.1093/nar/gkz1031](https://doi.org/10.1093/nar/gkz1031) [doi:10.1371/journal.pcbi.1005968](https://doi.org/10.1371/journal.pcbi.1005968) [doi:10.1093/bioinformatics/btx752](https://doi.org/10.1093/bioinformatics/btx752) [doi:10.1093/bioinformatics/btx441](https://doi.org/10.1093/bioinformatics/btx441) [doi:10.1186/s12859-017-1559-2](https://doi.org/10.1186/s12859-017-1559-2) [doi:10.1007/978-1-4939-6783-4_11](https://doi.org/10.1007/978-1-4939-6783-4_11) "*2Reactome_PE_All_levels.txt" and "*2Reactome_PE_Reactions.txt", Reactome, 73, https://reactome.org/download-data/ (2020-08-30). | https://reactome.org/license CC0 1.0 https://creativecommons.org/licenses/by/4.0/ |
| pub_reactome_73_pathways | Reactome | Pathways | 73 | 2020-06-09 | [doi:10.1093/nar/gkz1031](https://doi.org/10.1093/nar/gkz1031) [doi:10.1371/journal.pcbi.1005968](https://doi.org/10.1371/journal.pcbi.1005968) [doi:10.1093/bioinformatics/btx752](https://doi.org/10.1093/bioinformatics/btx752) [doi:10.1093/bioinformatics/btx441](https://doi.org/10.1093/bioinformatics/btx441) [doi:10.1186/s12859-017](-1559-2https://doi.org/10.1186/s12859-017-1559-2)[ doi:10.1007/978-1-4939-6783-4_11](https://doi.org/10.1007/978-1-4939-6783-4_11) "ReactomePathways.txt", Reactome, 73, https://reactome.org/download-data/ (2020-08-30). | https://reactome.org/license CC0 1.0 https://creativecommons.org/licenses/by/4.0/ |
| pub_reactome_73_ppi | Reactome | Protein-Protein Interaction | 73 | 2020-06-09 | [doi:10.1093/nar/gkz1031](https://doi.org/10.1093/nar/gkz1031) [doi:10.1371/journal.pcbi.1005968](https://doi.org/10.1371/journal.pcbi.1005968) [doi:10.1093/bioinformatics/btx752](https://doi.org/10.1093/bioinformatics/btx752) [doi:10.1093/bioinformatics/btx441](https://doi.org/10.1093/bioinformatics/btx441) [doi:10.1186/s12859-017-1559-2](https://doi.org/10.1186/s12859-017-1559-2) [doi:10.1007/978-1-4939-6783-4_11](https://doi.org/10.1007/978-1-4939-6783-4_11) "reactome.all_species.interactions.tab-delimited.txt", Reactome, 73, https://reactome.org/download-data/ (2020-08-30). | https://reactome.org/license CC0 1.0 https://creativecommons.org/licenses/by/4.0/ |
| pub_sjr_v2 | SJR | SCImago Journal & Country Rank | 2020-08-22 | 2020-08-22 | SCImago, (n.d.). SJR — SCImago Journal & Country Rank [Portal]. 2020/08/22, from http://www.scimagojr.com | Scimago Lab, Copyright 2007-2020. Data Source: Scopus®|
| pub_smpdb_metabolites_v2 | SMPDB | Metabolites | 2.75 | 2018-08-15 | [doi:10.1093/nar/gkp1002](https://doi.org/10.1093/nar/gkp1002)  [doi:10.1093/nar/gkt1067](https://doi.org/10.1093/nar/gkt1067)  | https://smpdb.ca/about#citing 'SMPDB is offered to the public as a freely available resource.'|
| pub_smpdb_pathways_v2 | SMPDB | Pathways | 2.75 | 2018-08-15 | [doi:10.1093/nar/gkp1002](https://doi.org/10.1093/nar/gkp1002)  [doi:10.1093/nar/gkt1067](https://doi.org/10.1093/nar/gkt1067)  | https://smpdb.ca/about#citing 'SMPDB is offered to the public as a freely available resource.'|
| pub_smpdb_proteins_v2 | SMPDB | Proteins | 2.75 | 2018-08-15 | [doi:10.1093/nar/gkp1002](https://doi.org/10.1093/nar/gkp1002)  [doi:10.1093/nar/gkt1067](https://doi.org/10.1093/nar/gkt1067)  | https://smpdb.ca/about#citing 'SMPDB is offered to the public as a freely available resource.'|
| pub_t3db_targets_v2 | T3DB | Targets | 2.0 | 2020-08-22 | [doi:10.1093/nar/gku1004](https://doi.org/10.1093/nar/gku1004)  [doi:10.1093/nar/gkp934](https://doi.org/10.1093/nar/gkp934)  | http://www.t3db.ca/downloads 'T3DB is offered to the public as a freely available resource.'|
| pub_t3db_toxins_v2 | T3DB | Toxins | 2.0 | 2020-08-22 | [doi:10.1093/nar/gku1004](https://doi.org/10.1093/nar/gku1004)  [doi:10.1093/nar/gkp934](https://doi.org/10.1093/nar/gkp934)  | http://www.t3db.ca/downloads 'T3DB is offered to the public as a freely available resource.'|
| pub_tissuenet_v2 | TissueNET2 |  | 2 | 2016-08-02 | [doi:10.1093/nar/gkw1088](https://doi.org/10.1093/nar/gkw1088)  [doi:10.1093/nar/gks1198](https://doi.org/10.1093/nar/gks1198)  [doi:10.1371/journal.pcbi.1003632](https://doi.org/10.1371/journal.pcbi.1003632)  | https://doi.org/10.1093/nar/gkw1088 'The TissueNet database is available for download under the permissive Creative Commons license.'|
| pub_unigene_homo_sapiens | Unigene | Homo Sapiens | 2013-04-25 | 2013-04-25 | [doi:10.1126/science.274.5287.540](https://doi.org/10.1126/science.274.5287.540)  | |
| pub_uniprot_v3 | UniProt | UniProtKB: SwissProt and TrEMBL | 2020-08-12 | 2020-08-12 | [doi:10.1093/nar/gky1049](https://doi.org/10.1093/nar/gky1049) | https://www.uniprot.org/help/license CC-BY-4.0 https://creativecommons.org/licenses/by/4.0 |
| pub_wikipathways_v4 | Wikipathways |  | 2020-08-10 | 2020-08-10 | [doi:10.1093/nar/gkx1064](https://doi.org/10.1093/nar/gkx1064) | https://www.wikipathways.org/index.php/WikiPathways:License_Terms and CC0 1.0 https://creativecommons.org/publicdomain/zero/1.0/ |

