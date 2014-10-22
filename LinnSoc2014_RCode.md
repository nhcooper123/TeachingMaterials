

```r
install.packages("ape")
```



```r
library(ape)
```



```r
primatedata <- read.table("Primatedata.txt", sep = "\t", header = TRUE)
```



```r
str(primatedata)
```

```
## 'data.frame':	77 obs. of  8 variables:
##  $ Order          : Factor w/ 1 level "Primates": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Family         : Factor w/ 15 levels "Aotidae","Atelidae",..: 2 2 2 14 3 3 3 4 4 4 ...
##  $ Binomial       : Factor w/ 77 levels "Alouatta palliata",..: 5 6 7 8 9 10 11 15 16 17 ...
##  $ AdultBodyMass_g: num  6692 7582 8697 958 558 ...
##  $ GestationLen_d : num  138 226 228 164 154 ...
##  $ HomeRange_km2  : num  2.28 0.73 1.36 0.02 0.32 0.02 0.00212 0.51 0.16 0.24 ...
##  $ MaxLongevity_m : num  336 328 454 304 215 ...
##  $ SocialGroupSize: num  14.5 42 20 2.95 6.85 ...
```



```r
head(primatedata)
```

```
##      Order      Family           Binomial AdultBodyMass_g GestationLen_d
## 1 Primates    Atelidae   Ateles belzebuth          6692.4          138.2
## 2 Primates    Atelidae   Ateles geoffroyi          7582.4          226.4
## 3 Primates    Atelidae    Ateles paniscus          8697.2          228.2
## 4 Primates Pitheciidae  Callicebus moloch           958.1          164.0
## 5 Primates     Cebidae  Callimico goeldii           558.0          154.0
## 6 Primates     Cebidae Callithrix jacchus           290.2          144.0
##   HomeRange_km2 MaxLongevity_m SocialGroupSize
## 1          2.28          336.0           14.50
## 2          0.73          327.6           42.00
## 3          1.36          453.6           20.00
## 4          0.02          303.6            2.95
## 5          0.32          214.8            6.85
## 6          0.02          201.6            8.55
```



```r
names(primatedata)
```

```
## [1] "Order"           "Family"          "Binomial"        "AdultBodyMass_g"
## [5] "GestationLen_d"  "HomeRange_km2"   "MaxLongevity_m"  "SocialGroupSize"
```

This gives you the names of the columns.


```r
primatedata
```



```r
primatetree <- read.nexus("consensusTree_10kTrees_Version2.nex")
```


Let’s examine the tree by typing:


```r
primatetree
```

```
## 
## Phylogenetic tree with 226 tips and 221 internal nodes.
## 
## Tip labels:
## 	Allenopithecus_nigroviridis, Cercopithecus_ascanius, Cercopithecus_cephus, Cercopithecus_cephus_cephus, Cercopithecus_cephus_ngottoensis, Cercopithecus_diana, ...
## 
## Rooted; includes branch lengths.
```

```r
str(primatetree)
```

```
## List of 4
##  $ edge       : int [1:446, 1:2] 227 228 229 230 231 232 233 234 234 235 ...
##  $ edge.length: num [1:446] 4.95 17.69 19.65 8.12 4.82 ...
##  $ Nnode      : int 221
##  $ tip.label  : chr [1:226] "Allenopithecus_nigroviridis" "Cercopithecus_ascanius" "Cercopithecus_cephus" "Cercopithecus_cephus_cephus" ...
##  - attr(*, "class")= chr "phylo"
##  - attr(*, "order")= chr "cladewise"
```



```r
plot(primatetree)
```



```r
plot(primatetree, cex = 0.5)
```



```r
zoom(primatetree, list(grep("Cercopithecus", primatetree$tip.label)), subtree = FALSE)
```



```r
zoom(primatetree, list(grep("Cercopithecus", primatetree$tip.label)), subtree = TRUE)
```



```r
primatetree2 <- drop.tip(primatetree, "Aotus_azarae_infulatus")
str(primatetree2)
```

```
## List of 4
##  $ edge       : int [1:444, 1:2] 226 227 228 229 230 231 232 233 233 234 ...
##  $ edge.length: num [1:444] 4.95 17.69 19.65 8.12 4.82 ...
##  $ Nnode      : int 220
##  $ tip.label  : chr [1:225] "Allenopithecus_nigroviridis" "Cercopithecus_ascanius" "Cercopithecus_cephus" "Cercopithecus_cephus_cephus" ...
##  - attr(*, "class")= chr "phylo"
##  - attr(*, "order")= chr "cladewise"
```



```r
`?`(plot.phylo)
```



```r
par(mfrow = c(1, 1))
plot(primatetree, type = "fan", edge.color = "deeppink", tip.color = "green", 
    cex = 0.5)
```



```r
plot(primatetree)
axisPhylo()
```



```r
is.binary.tree(primatetree)  # we want this to be TRUE
```

```
## [1] FALSE
```

```r
primatetree <- multi2di(primatetree)
```



```r
primatetree.reroot <- root(primatetree, "Saimiri_sciureus")
plot(primatetree.reroot)
```


```r
primatedata$Binomial <- gsub(" ", "_", primatedata$Binomial)
```



```r
row.names(primatedata) <- primatedata$Binomial
```



```r
primatedata2 <- primatedata[, 4:8]
```



```r
match.species <- treedata(primatetree, primatedata2)
```

```
## Warning: The following tips were not found in 'data' and were dropped from 'phy':
## 	Allenopithecus_nigroviridis
## 	Allocebus_trichotis
## 	Alouatta_caraya
## 	Alouatta_sara
## 	Aotus_azarae
## 	Aotus_azarae_infulatus
## 	Aotus_lemurinus_griseimembra
## 	Aotus_nancymaae
## 	Arctocebus_aureus
## 	Arctocebus_calabarensis
## 	Ateles_fusciceps
## 	Ateles_geoffroyi_ornatus
## 	Ateles_geoffroyi_vellerosus
## 	Avahi_laniger
## 	Avahi_occidentalis
## 	Brachyteles_arachnoides
## 	Bunopithecus_hoolock
## 	Callicebus_donacophilus
## 	Callithrix_(Mico)_emiliae
## 	Callithrix_argentata
## 	Callithrix_aurita
## 	Callithrix_geoffroyi
## 	Callithrix_humeralifera
## 	Callithrix_kuhli
## 	Callithrix_penicillata
## 	Cercocebus_agilis
## 	Cercocebus_atys
## 	Cercocebus_torquatus
## 	Cercopithecus_cephus_cephus
## 	Cercopithecus_cephus_ngottoensis
## 	Cercopithecus_diana
## 	Cercopithecus_erythrogaster_erythrogaster
## 	Cercopithecus_erythrotis
## 	Cercopithecus_hamlyni
## 	Cercopithecus_lhoesti
## 	Cercopithecus_lowei
## 	Cercopithecus_mona
## 	Cercopithecus_petaurista
## 	Cercopithecus_preussi
## 	Cercopithecus_solatus
## 	Cercopithecus_wolfi
## 	Cheirogaleus_crossleyi
## 	Chlorocebus_aethiops
## 	Chlorocebus_pygerythrus
## 	Chlorocebus_sabaeus
## 	Chlorocebus_tantalus
## 	Colobus_angolensis
## 	Eulemur_albifrons
## 	Eulemur_albocollaris
## 	Eulemur_collaris
## 	Eulemur_macaco_flavifrons
## 	Eulemur_macaco_macaco
## 	Eulemur_rubriventer
## 	Eulemur_rufus
## 	Eulemur_sanfordi
## 	Euoticus_elegantulus
## 	Galago_gallarum
## 	Galago_zanzibaricus
## 	Gorilla_gorilla_gorilla
## 	Hapalemur_alaotrensis
## 	Hapalemur_aureus
## 	Hapalemur_griseus_griseus
## 	Hapalemur_griseus_meridionalis
## 	Hapalemur_occidentalis
## 	Homo_sapiens
## 	Hylobates_agilis
## 	Hylobates_klossii
## 	Hylobates_moloch
## 	Hylobates_muelleri
## 	Indri_indri
## 	Leontopithecus_chrysomelas
## 	Leontopithecus_chrysopygus
## 	Lepilemur_aeeclis
## 	Lepilemur_ankaranensis
## 	Lepilemur_dorsalis
## 	Lepilemur_edwardsi
## 	Lepilemur_microdon
## 	Lepilemur_mitsinjoensis
## 	Lepilemur_randrianasoli
## 	Lepilemur_ruficaudatus
## 	Lepilemur_sahamalazensis
## 	Lepilemur_seali
## 	Lepilemur_septentrionalis
## 	Lophocebus_aterrimus
## 	Loris_lydekkerianus_grandis
## 	Loris_lydekkerianus_malabaricus
## 	Macaca_arctoides
## 	Macaca_assamensis
## 	Macaca_cyclopis
## 	Macaca_hecki
## 	Macaca_leonina
## 	Macaca_maura
## 	Macaca_nigra
## 	Macaca_nigrescens
## 	Macaca_ochreata
## 	Macaca_ochreata_brunnescens
## 	Macaca_pagensis
## 	Macaca_siberu
## 	Macaca_thibetana
## 	Macaca_tonkeana
## 	Mandrillus_leucophaeus
## 	Microcebus_berthae
## 	Microcebus_bongolavensis
## 	Microcebus_danfossi
## 	Microcebus_griseorufus
## 	Microcebus_jollyae
## 	Microcebus_lehilahytsara
## 	Microcebus_lokobensis
## 	Microcebus_mittermeieri
## 	Microcebus_myoxinus
## 	Microcebus_ravelobensis
## 	Microcebus_sambiranensis
## 	Microcebus_simmonsi
## 	Microcebus_tavaratra
## 	Nomascus_gabriellae
## 	Nomascus_leucogenys
## 	Nycticebus_coucang
## 	Nycticebus_pygmaeus
## 	Pan_troglodytes_schweinfurthii
## 	Pan_troglodytes_troglodytes
## 	Pan_troglodytes_verus
## 	Papio_papio
## 	Piliocolobus_badius
## 	Pongo_abelii
## 	Pongo_pygmaeus_pygmaeus
## 	Presbytis_melalophos
## 	Propithecus_coquereli
## 	Propithecus_diadema
## 	Propithecus_edwardsi
## 	Propithecus_tattersalli
## 	Pygathrix_nemaeus
## 	Rhinopithecus_avunculus
## 	Rhinopithecus_bieti
## 	Rhinopithecus_brelichi
## 	Rhinopithecus_roxellana
## 	Rungwecebus_kipunji
## 	Saguinus_geoffroyi
## 	Saguinus_imperator
## 	Saimiri_boliviensis_boliviensis
## 	Saimiri_oerstedii
## 	Trachypithecus_(Trachypithecus)_auratus
## 	Trachypithecus_(Trachypithecus)_poliocephalus
## 	Trachypithecus_cristatus
## 	Trachypithecus_francoisi
## 	Trachypithecus_johnii
## 	Trachypithecus_phayrei
## 	Trachypithecus_pileatus
## 	Varecia_rubra
## 	Varecia_variegata_variegata
```



```r
mytree <- match.species$phy
mydata <- match.species$data
```



```r
BM <- fitContinuous(mytree, log(mydata[, "AdultBodyMass_g"]), model = c("BM"))
```



```r
BM
```

```
## GEIGER-fitted comparative model of continuous data
##  fitted 'BM' model parameters:
## 	sigsq = 0.028655
## 	z0 = 6.773956
## 
##  model summary:
## 	log-likelihood = -78.096042
## 	AIC = 160.192084
## 	AICc = 160.354246
## 	free parameters = 2
## 
## Convergence diagnostics:
## 	optimization iterations = 100
## 	failed iterations = 0
## 	frequency of best fit = 1.00
## 
##  object summary:
## 	'lik' -- likelihood function
## 	'bnd' -- bounds for likelihood search
## 	'res' -- optimization iteration summary
## 	'opt' -- maximum likelihood parameter estimates
```



```r
OU <- fitContinuous(mytree, log(mydata[, "AdultBodyMass_g"]), model = c("OU"))
```



```r
OU
```

```
## GEIGER-fitted comparative model of continuous data
##  fitted 'OU' model parameters:
## 	alpha = 0.000000
## 	sigsq = 0.028655
## 	z0 = 6.773956
## 
##  model summary:
## 	log-likelihood = -78.096042
## 	AIC = 162.192084
## 	AICc = 162.520851
## 	free parameters = 3
## 
## Convergence diagnostics:
## 	optimization iterations = 100
## 	failed iterations = 0
## 	frequency of best fit = 0.54
## 
##  object summary:
## 	'lik' -- likelihood function
## 	'bnd' -- bounds for likelihood search
## 	'res' -- optimization iteration summary
## 	'opt' -- maximum likelihood parameter estimates
```



```r
EB <- fitContinuous(mytree, log(mydata[, "AdultBodyMass_g"]), model = c("EB"))
```



```r
EB
```

```
## GEIGER-fitted comparative model of continuous data
##  fitted 'EB' model parameters:
## 	a = -0.037692
## 	sigsq = 0.276300
## 	z0 = 6.695068
## 
##  model summary:
## 	log-likelihood = -75.428628
## 	AIC = 156.857257
## 	AICc = 157.186024
## 	free parameters = 3
## 
## Convergence diagnostics:
## 	optimization iterations = 100
## 	failed iterations = 0
## 	frequency of best fit = 0.20
## 
##  object summary:
## 	'lik' -- likelihood function
## 	'bnd' -- bounds for likelihood search
## 	'res' -- optimization iteration summary
## 	'opt' -- maximum likelihood parameter estimates
```

