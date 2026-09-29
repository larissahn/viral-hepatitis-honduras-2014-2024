# Phylogenetic confirmation of the F1a subgenotype (5 TEG sequences)

*Placement of the only Honduran HBV sequences (U91811–U91815, partial S gene, 1997) within genotype-F diversity, using an expanded reference panel. Citation style: Vancouver. Date: 2026-06-18.*

## Objective

Raise the identity-based call (genotype F) to a supported phylogenetic placement able to resolve **subgenotype** (Central American F1a vs South American F1b), triangulated with web genotyping and the Thr45 marker.

## Panel and method

- **Subgenotype references:** curated set from McNaughton et al. 2020 [1] ("File 3"; 44 genomic sequences spanning A–J and F1–F4 + H; figshare DOI 10.6084/m9.figshare.8851946).
- **Central American F1a anchors:** 4 S-gene sequences from the regional pool (Costa Rica AY090458, AY090459; El Salvador FJ589065, AY090461), a canonical F1a lineage [2,5].
- **Query:** the 5 TEG sequences (U91811–U91815). **Total:** 53 taxa.
- **Alignment:** `DECIPHER::AlignSeqs` [3], trimmed to the S-gene window covered by the TEG (53 taxa × 681 columns; `02_alignments/panel_S_aln.fasta`).
- **Phylogeny:** maximum likelihood in `phangorn::pml_bb` [4], GTR+G(4)+I, ultrafast bootstrap, midpoint-rooted.

## Result

The 5 TEG form a nested group **within the Central American F1a cluster**, sister to the Costa Rica and El Salvador anchors, with **100 % bootstrap** for the TEG + F1a clade; the South American F1b reference (HM585194, Chile) falls outside as a sister branch, and F2–F4 and H are more basal. Topology and support confirm subgenotype **F1a, the autochthonous Central American lineage**.

## Caveat (methodological honesty)

The internal ordering among the five TEG has low support: they are near-identical (same 1997 study), so bootstrap does not resolve their branching order. That is distinct from clade membership: **F1a membership is 100 %**; internal ordering is not claimed. The analysis is in silico, on a partial S gene (681 bp), n = 5, from a single year (1997); it does not replace serological or neutralization data.

## Convergence of evidence

| Method | Result | Source |
|---|---|---|
| S-gene identity vs panel A–J | F (98.2 %); H second (96.8 %) | `context_pipeline.R` |
| geno2pheno[hbv] (sp/SHB-nt/RT) | F1 | `notes/08_*` |
| HBVdb genotyping tool | F | raw not bundled |
| Thr45 marker | Central American F1a signature | Arauz-Ruiz 1997 [2] |
| **ML tree (S gene, expanded panel)** | **TEG + F1a clade, 100 % bootstrap** | this document |

## Files

- `02_alignments/panel_S_aln.fasta` — alignment (53 taxa × 681 col).
- `05_figures/panel_S_ML.nwk` — ML tree (Newick, with support).
- `05_figures/panel_S_ML_tree_circular.pdf` / `.png` — full tree (53 taxa).

---

## References

1. McNaughton AL, Revill PA, Littlejohn M, Matthews PC, Ansari MA. Analysis of genomic-length HBV sequences to determine genotype and subgenotype reference sequences. J Gen Virol. 2020;101(3):271-283. doi:10.1099/jgv.0.001387.
2. Arauz-Ruiz P, Norder H, Visoná KA, Magnius LO. Molecular epidemiology of hepatitis B virus in Central America reflected in the genetic variability of the small S gene. J Infect Dis. 1997;176(4):851-858. doi:10.1086/516507.
3. Wright ES. DECIPHER: harnessing local sequence context to improve protein multiple sequence alignment. BMC Bioinformatics. 2015;16:322. doi:10.1186/s12859-015-0749-z.
4. Schliep KP. phangorn: phylogenetic analysis in R. Bioinformatics. 2011;27(4):592-593. doi:10.1093/bioinformatics/btq706.
5. Devesa M, Pujol FH. Hepatitis B virus genetic diversity in Latin America. Virus Res. 2007;127(2):177-184. doi:10.1016/j.virusres.2007.01.004.
