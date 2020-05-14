
# Data informations


- `test.bam.bai` file generated with

```
samtools view -h ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/phase1/data/HG00154/alignment/HG00154.mapped.ILLUMINA.bwa.GBR.low_coverage.20101123.bam 17:7512445-7513455 -O bam > test.bam.bai
```


- `cpg.bed` file downloaded from

```
https://s3.amazonaws.com/bedtools-tutorials/web/cpg.bed
```


- `chr_y.gff3.gz` file downloaded from

```
ftp://ftp.ensembl.org/pub/release-99/gff3/homo_sapiens/Homo_sapiens.GRCh38.99.chromosome.Y.gff3.gz
```


- `dna_human_y.fa.gz` dna file downloaded and subsetted from 

```
ftp://ftp.ensembl.org/pub/release-99/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna_rm.chromosome.Y.fa.gz
```

- `ncrna_mus_musculus.fa.gz` rna file downloaded and subsetted from 

```
ftp://ftp.ensembl.org/pub/release-99/fasta/mus_musculus/ncrna/Mus_musculus.GRCm38.ncrna.fa.gz
```

- `pep_mus_spretus.fa.gz` aa file downloaded and subsetted from 

```
ftp://ftp.ensembl.org/pub/release-99/fasta/mus_spretus/pep/Mus_spretus.SPRET_EiJ_v1.pep.all.fa.gz
```
