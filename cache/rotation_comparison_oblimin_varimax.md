# Rotation Comparison: Oblimin (Historical) vs. Varimax (Current)

Source: git commit `aa40dae` (oblimin specification, `cache/01_prepared_data.rds`) versus the current `HEAD` varimax cache. Loadings are columns ordered by substantive factor. Oblimin values are pattern loadings. Loadings below |0.10| are blank.

## Variance Explained and KMO

| Battery | Specification | Factor | Variance Explained | Total | KMO |
|---|---|---|---|---|---|
| Activities | Oblimin | arts / leisure / residual | 0.428 / 0.091 / 0.068 | 0.587 | 0.958 |
| Activities | Varimax | arts / leisure / residual | 0.399 / 0.117 / 0.071 | 0.587 | 0.958 |
| Weak ties | Oblimin | variety / liberal / conservative | 0.201 / 0.239 / 0.173 | 0.612 | 0.938 |
| Weak ties | Varimax | variety / liberal / conservative | 0.201 / 0.229 / 0.182 | 0.612 | 0.938 |
| Strong ties | Oblimin | variety / liberal / conservative | 0.254 / 0.140 / 0.218 | 0.612 | 0.935 |
| Strong ties | Varimax | variety / liberal / conservative | 0.238 / 0.179 / 0.196 | 0.612 | 0.935 |

Oblimin variance figures are psych's approximations for an oblique solution, which are not strictly additive, so per-factor shares should be treated as approximate. The total is the same under both rotations because rotation preserves the total variance captured by the retained components.

## Inter-Factor Correlations (Phi)

**Activities**

Oblimin (Phi):

| | arts | leisure | residual |
|---|---|---|---|
| arts | 1.000 | 0.249 | 0.027 |
| leisure | 0.249 | 1.000 | 0.003 |
| residual | 0.027 | 0.003 | 1.000 |

Varimax (Phi, identity by construction):

| | arts | leisure | residual |
|---|---|---|---|
| arts | 1.000 | 0.000 | 0.000 |
| leisure | 0.000 | 1.000 | 0.000 |
| residual | 0.000 | 0.000 | 1.000 |

**Weak ties**

Oblimin (Phi):

| | variety | liberal | conservative |
|---|---|---|---|
| variety | 1.000 | 0.346 | 0.251 |
| liberal | 0.346 | 1.000 | 0.394 |
| conservative | 0.251 | 0.394 | 1.000 |

Varimax (Phi, identity by construction):

| | liberal | variety | conservative |
|---|---|---|---|
| liberal | 1.000 | 0.000 | 0.000 |
| variety | 0.000 | 1.000 | 0.000 |
| conservative | 0.000 | 0.000 | 1.000 |

**Strong ties**

Oblimin (Phi):

| | variety | liberal | conservative |
|---|---|---|---|
| variety | 1.000 | 0.259 | 0.439 |
| liberal | 0.259 | 1.000 | 0.276 |
| conservative | 0.439 | 0.276 | 1.000 |

Varimax (Phi, identity by construction):

| | variety | conservative | liberal |
|---|---|---|---|
| variety | 1.000 | 0.000 | 0.000 |
| conservative | 0.000 | 1.000 | 0.000 |
| liberal | 0.000 | 0.000 | 1.000 |

## Loadings

### Activity battery (20 items)

| Item | Oblimin: arts | Oblimin: leisure | Oblimin: residual | Varimax: arts | Varimax: leisure | Varimax: residual |
|---|---|---|---|---|---|---|
| museum | 0.80 |  | 0.10 | 0.77 | 0.22 | 0.17 |
| art_gallery | 0.83 |  |  | 0.81 | 0.20 | 0.15 |
| symphony_orchestra_opera | 0.88 | -0.15 |  | 0.84 |  | 0.16 |
| gardening | 0.29 | 0.33 | 0.54 | 0.26 | 0.37 | 0.57 |
| carnival_fair_amusement_park | 0.83 |  |  | 0.81 | 0.15 |  |
| music_concert_festival | 0.82 |  |  | 0.81 | 0.20 |  |
| play_or_musical | 0.84 |  |  | 0.82 | 0.11 |  |
| library | 0.45 | 0.33 |  | 0.44 | 0.42 |  |
| fancy_restaurant | 0.67 | 0.15 | -0.24 | 0.66 | 0.31 | -0.18 |
| fast_food | 0.14 | 0.14 | -0.69 | 0.17 | 0.20 | -0.68 |
| go_for_walk | -0.17 | 0.85 |  | -0.15 | 0.81 |  |
| exercise_or_yoga | 0.28 | 0.49 | -0.27 | 0.30 | 0.56 | -0.23 |
| dance_performance | 0.88 | -0.23 |  | 0.84 |  | 0.11 |
| movie_theater | 0.69 | 0.10 | -0.34 | 0.69 | 0.27 | -0.28 |
| read_novel_poem_or_play | 0.25 | 0.43 |  | 0.25 | 0.48 | 0.11 |
| attend_sports | 0.75 |  | -0.17 | 0.74 | 0.23 | -0.11 |
| home_auto_repair | 0.41 | 0.22 | 0.41 | 0.39 | 0.30 | 0.46 |
| hiking_camping_boating | 0.50 | 0.29 | 0.23 | 0.48 | 0.39 | 0.28 |
| historic_site | 0.71 | 0.10 | 0.16 | 0.68 | 0.25 | 0.22 |
| go_to_festival | 0.77 | 0.14 |  | 0.75 | 0.31 |  |

### Weak-tie battery (18 items)

| Item | Oblimin: variety | Oblimin: liberal | Oblimin: conservative | Varimax: variety | Varimax: liberal | Varimax: conservative |
|---|---|---|---|---|---|---|
| KnowLGBTQ_weak | 0.12 | 0.76 | -0.16 | 0.21 | 0.73 |  |
| KnowLotsaChurch_weak | 0.23 |  | 0.70 | 0.33 | 0.17 | 0.71 |
| KnowLittleChurch_weak |  | 0.62 | 0.26 |  | 0.61 | 0.39 |
| KnowVeryLib_weak |  | 0.78 |  | 0.14 | 0.75 | 0.16 |
| KnowVeryCons_weak | 0.16 | -0.14 | 0.84 | 0.26 |  | 0.80 |
| KnowAsianppl_weak | 0.59 | 0.39 |  | 0.63 | 0.48 | 0.12 |
| KnowHispanixppl_weak | 0.30 | 0.62 |  | 0.38 | 0.64 | 0.13 |
| KnowBlackppl_weak | 0.34 | 0.45 | 0.14 | 0.42 | 0.52 | 0.28 |
| KnowWhiteppl_weak | -0.17 | 0.43 | 0.60 |  | 0.47 | 0.68 |
| Know2ndHome_weak | 0.56 |  | 0.38 | 0.59 | 0.12 | 0.41 |
| KnowBornElsewhere_weak | 0.45 | 0.46 |  | 0.51 | 0.52 | 0.15 |
| KnowMENAppl_weak | 0.76 | 0.16 |  | 0.77 | 0.30 | 0.11 |
| KnowHawaiiPI_weak | 0.85 |  |  | 0.83 | 0.16 |  |
| KnowAmIndianppl_weak | 0.80 |  | 0.12 | 0.79 |  | 0.16 |
| KnowCityppl_weak | 0.25 | 0.56 |  | 0.33 | 0.59 | 0.16 |
| KnowRuralppl_weak |  |  | 0.68 | 0.11 | 0.13 | 0.67 |
| Knowwomen_weak |  | 0.52 | 0.48 | 0.11 | 0.56 | 0.60 |
| Knowmen_weak | -0.14 | 0.61 | 0.44 |  | 0.63 | 0.57 |

### Strong-tie battery (18 items)

| Item | Oblimin: variety | Oblimin: liberal | Oblimin: conservative | Varimax: variety | Varimax: liberal | Varimax: conservative |
|---|---|---|---|---|---|---|
| KnowLGBTQ_strong | 0.27 | 0.69 |  | 0.29 | 0.72 |  |
| KnowLotsaChurch_strong | 0.26 | -0.18 | 0.70 | 0.38 |  | 0.71 |
| KnowLittleChurch_strong |  | 0.49 | 0.45 |  | 0.59 | 0.41 |
| KnowVeryLib_strong | 0.11 | 0.76 |  | 0.17 | 0.81 | 0.11 |
| KnowVeryCons_strong | 0.17 | -0.32 | 0.79 | 0.31 |  | 0.78 |
| KnowAsianppl_strong | 0.67 | 0.22 |  | 0.66 | 0.36 | 0.15 |
| KnowHispanixppl_strong | 0.54 | 0.35 |  | 0.54 | 0.46 | 0.14 |
| KnowBlackppl_strong | 0.52 | 0.21 | 0.21 | 0.55 | 0.37 | 0.30 |
| KnowWhiteppl_strong | -0.13 | 0.21 | 0.80 |  | 0.39 | 0.73 |
| Know2ndHome_strong | 0.60 | -0.14 | 0.29 | 0.63 |  | 0.39 |
| KnowBornElsewhere_strong | 0.62 | 0.26 |  | 0.63 | 0.40 | 0.18 |
| KnowMENAppl_strong | 0.84 |  |  | 0.80 | 0.21 | 0.14 |
| KnowHawaiiPI_strong | 0.87 |  |  | 0.81 | 0.12 |  |
| KnowAmIndianppl_strong | 0.81 | -0.11 |  | 0.78 |  | 0.21 |
| KnowCityppl_strong | 0.27 | 0.45 | 0.22 | 0.33 | 0.56 | 0.26 |
| KnowRuralppl_strong |  |  | 0.69 | 0.12 | 0.15 | 0.65 |
| Knowwomen_strong |  | 0.22 | 0.70 | 0.21 | 0.42 | 0.67 |
| Knowmen_strong |  | 0.40 | 0.65 | 0.15 | 0.56 | 0.61 |

