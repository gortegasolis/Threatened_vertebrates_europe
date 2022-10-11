# Prepare text for topic model
# Source "my_stopwords"
source("my_stopwords.R")

prev_vars <- read.csv("groups.csv") %>% unique()

my_stopwords_split <- my_stopwords %>% split(., 1:50)

# Re-import art_per_class
art_per_class <- readRDS("art_per_class.rds")

# Remove "my_stopwords"
for (x in names(art_per_class)) {
  for (y in my_stopwords_split) {
    art_per_class[[x]]$stm_text <-
      tm::removeWords(x = art_per_class[[x]]$stm_text, y)
  }
}

# Double lemmatization with UDpipe and hunspell
lemma_dict <- c(
  art_per_class$AMPHIBIA$stm_text,
  art_per_class$REPTILIA$stm_text,
  art_per_class$AVES$stm_text,
  art_per_class$MAMMALIA$stm_text
) %>%
  unique() %>%
  make_lemma_dictionary() %>%
  filter(
    !lemma %in% c(
      "crease",
      "lease",
      "pig",
      "vegetate",
      "capture",
      "pend",
      "pa",
      "cord",
      "cuss",
      "fin",
      "dices",
      "it",
      "riv",
      "tensity",
      "duct",
      "generate",
      "album",
      "tress",
      "jury"
    )
  ) %>%
  filter(!token %in% c("recollected", "albumen", "reed", "reeds", "inbreeding", "cites")) %>%
  mutate(lenght = nchar(lemma)) %>%
  filter(!lenght == 1)

for (x in names(art_per_class)) {
  art_per_class[[x]]$stm_text <-
    udpipe(art_per_class[[x]]$stm_text, "english",
      parallel.cores = 20
    ) %>%
    paste.data.frame(., term = "lemma", group = c("doc_id")) %>%
    pull(., "lemma") %>%
    lemmatize_strings(dictionary = lemma_dict)
}

for (x in names(art_per_class)) {
  art_per_class[[x]]$stm_text <-
    gsub(
      x = art_per_class[[x]]$stm_text,
      "\\bpry\\b",
      replacement = "prey"
    ) %>%
    gsub(
      x = ., "\\bnich\\b|\\bnichee\\b|\\bnico\\b",
      replacement = "niche"
    ) %>%
    gsub(
      x = ., "\\bendanger[a-z]+\\b",
      replacement = "endanger"
    ) %>%
    gsub(
      x = ., "\\bendogami[a-z]+\\b|\\binbree[a-z]+\\b|\\binbred\\b",
      replacement = "endogamy"
    ) %>%
    gsub(
      x = ., "\\binjur[a-z]+\\b|\\blesion\\b|\\blesions\\b",
      replacement = "injury"
    ) %>%
    gsub(
      x = .,
      "\\bhybridiz[a-z]+\\b|\\bhybridis[a-z]+\\b",
      replacement = "hybrid"
    ) %>%
    gsub(
      x = .,
      "\\bimplementation\\b",
      replacement = "implement"
    ) %>%
    gsub(
      x = .,
      "\\binfect[a-z]+\\b|\\binfect\\b",
      replacement = "infection"
    ) %>%
    gsub(
      x = ., "\\babundan[a-z]+\\b",
      replacement = "abundance"
    ) %>%
    gsub(
      x = ., "\\bacoustic[a-z]+\\b",
      replacement = "acoustic"
    ) %>%
    gsub(
      x = ., "\\bcompetitiv[a-z]+\\b",
      replacement = "competition"
    ) %>%
    gsub(
      x = .,
      "\\bepidemiolog[a-z]+\\b|\\bepidemic\\b|\\bepizoot[a-z]+\\b",
      replacement = "epidemiology"
    ) %>%
    gsub(
      x = ., "\\bfarmland\\b|\\bfarmer\\b|\\bfarmers\\b",
      replacement = "farm"
    ) %>%
    gsub(
      x = ., "\\bfecal\\b|\\bfaecal\\b|\\bfaeces\\b|\\bfaece\\b",
      replacement = "fece"
    ) %>%
    gsub(
      x = .,
      "\\bgenom[a-z]+\\b|\\bgeneti[a-z]+\\b|\\ballel[a-z]+\\b|\\bgenoty[a-z]+\\b|\\bgenoty\\b|\\bchromosom[a-z]+\\b",
      replacement = "gene"
    ) %>%
    gsub(
      x = ., "\\b[a-z]+virus\\b|\\b[a-z]+viridae\\b|\\b[a-z]+viride\\b|\\b[a-z]+viruse\\b|\\b[a-z]+viruses\\b|\\bviruse\\b|\\bviral\\b|\\bcalicivirida\\b|\\bvirul[a-z]+\\b|\\bvirology\\b|\\b[a-z]+viruses\\b",
      replacement = "virus"
    ) %>%
    gsub(
      x = ., "\\b[a-z]+bacteria\\b|\\b[a-z]+bacterias\\b|\\bbacterial\\b",
      replacement = "bacteria"
    ) %>%
    gsub(
      x = ., "\\bpathogen[a-z]+\\b",
      replacement = "pathogen"
    ) %>%
    gsub(
      x = ., "\\bpathologi[a-z]+\\b",
      replacement = "pathology"
    ) %>%
    gsub(
      x = ., "\\bpolymorph[a-z]+\\b",
      replacement = "polymorphic"
    ) %>%
    gsub(
      x = ., "\\bfeed\\b",
      replacement = "food"
    ) %>%
    gsub(
      x = ., "\\bingestion\\b",
      replacement = "ingest"
    ) %>%
    gsub(
      x = ., "\\breleas[a-z]+\\b",
      replacement = "release"
    ) %>%
    gsub(
      x = ., "\\bmorpho[a-z]+\\b|\\bmorph\\b|\\banatom[a-z]+\\b",
      replacement = "morphology"
    ) %>%
    gsub(
      x = ., "\\blouse\\b|\\bphthirapter[a-z]+\\b|\\bpediculus\\b|\\bpthirus\\b|\\bphilopterid[a-z]+\\b",
      replacement = "lice"
    ) %>%
    gsub(
      x = ., "\\bparasit[a-z]+\\b|\\b[a-z]+parasit[a-z]+\\b",
      replacement = "parasite"
    ) %>%
    gsub(
      x = ., "\\bmigrat[a-z]+\\b|\\bmigran[a-z]+\\b|\\bmigran\\b|\\bemigrat[a-z]+\\b",
      replacement = "migration"
    ) %>%
    gsub(
      x = ., "\\bpoison[a-z]+\\b|\\bvenom\\b|\\bvenoms\\b",
      replacement = "poison"
    ) %>%
    gsub(
      x = ., "\\blekk\\b|\\blekk[a-z]+\\b",
      replacement = "lek"
    ) %>%
    gsub(
      x = ., "\\bantimicrobial\\b|\\bantibio[a-z]+\\b",
      replacement = "antibiotic"
    ) %>%
    gsub(
      x = ., "\\bcoloney\\b|\\bcolonies\\b",
      replacement = "colony"
    ) %>%
    gsub(
      x = ., "\\bintestinal\\b",
      replacement = "intestine"
    ) %>%
    gsub(
      x = ., "\\bmtdna\\b",
      replacement = "dna"
    ) %>%
    gsub(
      x = ., "\\bbehav[a-z]+\\b",
      replacement = "behavior"
    ) %>%
    gsub(
      x = ., "\\binvasiv[a-z]+\\b|\\balien\\b",
      replacement = "invasion"
    ) %>%
    gsub(
      x = ., "\\bmoult[a-z]+\\b|\\bmolt\\b",
      replacement = "moult"
    ) %>%
    gsub(
      x = ., "\\bisotop[a-z]+\\b",
      replacement = "isotope"
    ) %>%
    gsub(
      x = ., "\\bmap[a-z]+\\b",
      replacement = "map"
    ) %>%
    gsub(
      x = ., "\\bcaptiv[a-z]+\\b",
      replacement = "captive"
    ) %>%
    gsub(
      x = ., "\\bcestod[a-z]+\\b",
      replacement = "cestoda"
    ) %>%
    gsub(
      x = ., "\\bbatrachochytrium\\b|\\bchytrid\\b|\\bdendrobatidi\\b|\\bdendrobatidis\\b|\\btdendrobatidis\\b",
      replacement = "chytridiomycosis"
    ) %>%
    gsub(
      x = ., "\\bdemograph[a-z]+\\b",
      replacement = "demography"
    ) %>%
    gsub(
      x = ., "\\bembryo\\b|\\bembryonic\\b",
      replacement = "embryon"
    ) %>%
    gsub(
      x = ., "\\bevoluti[a-z]+\\b|\\bevolv[a-z]+\\b",
      replacement = "evolution"
    ) %>%
    gsub(
      x = ., "\\bmetamorp[a-z]+\\b|\\bpost-metamorph[a-z]+\\b",
      replacement = "metamorphosis"
    ) %>%
    gsub(
      x = ., "\\bpopulati[a-z]+\\b|\\bsubpopulati[a-z]+\\b",
      replacement = "population"
    ) %>%
    gsub(
      x = ., "\\bpredat[a-z]+\\b|\\bdepredat[a-z]+\\b",
      replacement = "predation"
    ) %>%
    gsub(
      x = ., "\\bstress[a-z]+\\b",
      replacement = "stress"
    ) %>%
    gsub(
      x = ., "\\btaxonom[a-z]+\\b|\\blectotyp[a-z]+\\b|\\bholotyp[a-z]+\\b|\\bneotyp[a-z]+\\b",
      replacement = "taxonomy"
    ) %>%
    gsub(
      x = ., "\\bagri\\b|\\bagro\\b|\\bagric[a-z]+\\b",
      replacement = "agroenvironment"
    ) %>%
    gsub(
      x = ., "\\bdimorph[a-z]+\\b",
      replacement = "dimorphism"
    ) %>%
    gsub(
      x = ., "\\bepithel[a-z]+\\b",
      replacement = "epithelium"
    ) %>%
    gsub(
      x = ., "\\bherbivor[a-z]+\\b",
      replacement = "herbivore"
    ) %>%
    gsub(
      x = ., "\\bhormon[a-z]+\\b",
      replacement = "hormonal"
    ) %>%
    gsub(
      x = ., "\\bimmunolog[a-z]+\\b|\\bimmunity\\b",
      replacement = "immune"
    ) %>%
    gsub(
      x = ., "\\bornament[a-z]+\\b",
      replacement = "ornament"
    ) %>%
    gsub(
      x = ., "\\bphylogeog[a-z]+\\b",
      replacement = "phylogeography"
    ) %>%
    gsub(
      x = ., "\\bphylogen[a-z]+\\b",
      replacement = "phylogeny"
    ) %>%
    gsub(
      x = ., "\\bphysio[a-z]+\\b|\\becophysio[a-z]+\\b",
      replacement = "physiology"
    ) %>%
    gsub(
      x = ., "\\btaphonom[a-z]+\\b",
      replacement = "taphonomy"
    ) %>%
    gsub(
      x = ., "\\btrematod[a-z]+\\b|\\bdigenean\\b",
      replacement = "trematoda"
    ) %>%
    gsub(
      x = ., "\\btrichomon[a-z]+\\b",
      replacement = "trichomona"
    ) %>%
    gsub(
      x = ., "\\bcoccid[a-z]+\\b",
      replacement = "coccidiosis"
    ) %>%
    gsub(
      x = ., "\\bdigest[a-z]+\\b|\\bdigest\\b",
      replacement = "digestible"
    ) %>%
    gsub(
      x = ., "\\breprod[a-z]+\\b",
      replacement = "reproduction"
    ) %>%
    gsub(
      x = ., "\\binsectivor[a-z]+\\b",
      replacement = "insectivore"
    ) %>%
    gsub(
      x = ., "\\bixod[a-z]+\\b",
      replacement = "ixodes"
    ) %>%
    gsub(
      x = ., "\\bleishman[a-z]+\\b",
      replacement = "leishmaniasis"
    ) %>%
    gsub(
      x = ., "\\bmyxom[a-z]+\\b|\\btmyxomatosis\\b",
      replacement = "myxomatosis"
    ) %>%
    gsub(
      x = ., "\\bpalae[a-z]+\\b|\\bpaleo[a-z]+\\b|\\bpleistocene\\b|\\bpliocene\\b",
      replacement = "paleoecology"
    ) %>%
    gsub(
      x = ., "\\bpregnan[a-z]+\\b|\\bpregnat[a-z]+\\b",
      replacement = "pregnancy"
    ) %>%
    gsub(
      x = ., "\\btoxoplasm[a-z]+\\b",
      replacement = "toxoplasmosis"
    ) %>%
    gsub(
      x = ., "\\bscaveng[a-z]+\\b",
      replacement = "scavenger"
    ) %>%
    gsub(
      x = ., "\\bmitochon[a-z]+\\b",
      replacement = "mitochondria"
    ) %>%
    gsub(
      x = ., "\\bcarnivor[a-z]+\\b",
      replacement = "carnivore"
    ) %>%
    gsub(
      x = ., "\\bhelminth[a-z]+\\b",
      replacement = "helminth"
    ) %>%
    gsub(
      x = ., "\\bnematod[a-z]+\\b",
      replacement = "nematoda"
    ) %>%
    gsub(
      x = ., "\\bphenotyp[a-z]+\\b",
      replacement = "phenotype"
    ) %>%
    gsub(
      x = ., "\\btoxic[a-z]+\\b|\\becotoxicol[a-z]+\\b|\\bagrotox[a-z]+\\b|\\btoxic\\b",
      replacement = "toxicology"
    ) %>%
    gsub(
      x = ., "\\bpollut[a-z]+\\b|\\bcontamin[a-z]+\\b",
      replacement = "pollution"
    ) %>%
    gsub(
      x = ., "\\btapdol[a-z]+\\b",
      replacement = "tapdole"
    ) %>%
    gsub(
      x = ., "\\bclutch[a-z]+\\b",
      replacement = "clutch"
    ) %>%
    gsub(
      x = ., "\\bpeptid[a-z]+\\b|\\bpeptic\\b",
      replacement = "peptide"
    ) %>%
    gsub(
      x = ., "\\bbiogeog[a-z]+\\b",
      replacement = "biogeography"
    ) %>%
    gsub(
      x = ., "\\bclimat[a-z]+\\b",
      replacement = "climate"
    ) %>%
    gsub(
      x = ., "\\bdistribut[a-z]+\\b",
      replacement = "distribution"
    ) %>%
    gsub(
      x = ., "\\bforag[a-z]+\\b",
      replacement = "foraging"
    ) %>%
    gsub(
      x = ., "\\bdietary\\b",
      replacement = "diet"
    ) %>%
    gsub(
      x = ., "\\bconspecific[a-z]+\\b",
      replacement = "conspecific"
    ) %>%
    gsub(
      x = ., "\\benvironment[a-z]+\\b",
      replacement = "environment"
    ) %>%
    gsub(
      x = ., "\\bthermo[a-z]+\\b|\\bthermal\\b",
      replacement = "thermoregulation"
    ) %>%
    gsub(
      x = ., "\\bsubspecy\\b|\\bsubspecies\\b",
      replacement = "subspecies"
    ) %>%
    gsub(
      x = ., "\\ballopatri[a-z]+\\b|\\ballopatic\\b",
      replacement = "allopatry"
    ) %>%
    gsub(
      x = ., "\\bspecy\\b",
      replacement = "species"
    ) %>%
    gsub(
      x = ., "\\bfrugivor[a-z]+\\b",
      replacement = "frugivore"
    ) %>%
    gsub(
      x = ., "\\bregener[a-z]+\\b",
      replacement = "regeneration"
    ) %>%
    gsub(
      x = ., "\\bdegener[a-z]+\\b",
      replacement = "degeneration"
    ) %>%
    gsub(
      x = ., "\\bgenerate[a-z]+\\b|\\bgenerating\\b",
      replacement = "generate"
    ) %>%
    gsub(
      x = ., "\\bbreed[a-z]+\\b",
      replacement = "breed"
    ) %>%
    gsub(
      x = ., "\\balbuman\\b|\\balbum\\b",
      replacement = "albumen"
    ) %>%
    gsub(
      x = ., "\\btedulis\\b",
      replacement = "edulis"
    ) %>%
    gsub(
      x = ., "\\bcensuse\\b",
      replacement = "census"
    ) %>%
    gsub(
      x = ., "\\bzoonot[a-z]+\\b",
      replacement = "zoonosis"
    ) %>%
    gsub(
      x = ., "\\bcellular\\b",
      replacement = "cell"
    ) %>%
    gsub(
      x = ., "\\bcombination\\b",
      replacement = "combine"
    ) %>%
    gsub(
      x = ., "\\bconsum\\b",
      replacement = "consume"
    ) %>%
    gsub(
      x = ., "\\bdepend[a-z]+\\b",
      replacement = "depend"
    ) %>%
    gsub(
      x = ., "\\bdevelo[a-z]+\\b",
      replacement = "development"
    ) %>%
    gsub(
      x = ., "\\bincreas[a-z]+\\b",
      replacement = "increase"
    ) %>%
    gsub(
      x = ., "\\bdecreas[a-z]+\\b",
      replacement = "decrease"
    ) %>%
    gsub(
      x = ., "\\blarva[a-z]+\\b",
      replacement = "larva"
    ) %>%
    gsub(
      x = ., "\\blocus\\b|\\bmultilocus\\b",
      replacement = "loci"
    ) %>%
    gsub(
      x = ., "\\bmaintai[a-z]+\\b",
      replacement = "maintenance"
    ) %>%
    gsub(
      x = ., "\\bsex[a-z]+\\b",
      replacement = "sex"
    ) %>%
    gsub(
      x = ., "\\btemporary\\b",
      replacement = "temporal"
    ) %>%
    gsub(
      x = ., "\\btransect[a-z]+\\b",
      replacement = "transect"
    ) %>%
    gsub(
      x = ., "\\btranscript[a-z]+\\b",
      replacement = "transcription"
    ) %>%
    gsub(
      x = ., "\\bantipred[a-z]+\\b",
      replacement = "antipredatory"
    ) %>%
    gsub(
      x = ., "\\bdetect[a-z]+\\b",
      replacement = "detect"
    ) %>%
    gsub(
      x = ., "\\blose\\b",
      replacement = "loss"
    ) %>%
    gsub(
      x = ., "\\bobserv[a-z]+\\b|\\bobserv\\b",
      replacement = "observation"
    ) %>%
    gsub(
      x = ., "\\boccur[a-z]+\\b",
      replacement = "occurrence"
    ) %>%
    gsub(
      x = ., "\\bpredict[a-z]+\\b",
      replacement = "predict"
    ) %>%
    gsub(
      x = ., "\\breduct[a-z]+\\b",
      replacement = "reduce"
    ) %>%
    gsub(
      x = ., "\\bseason[a-z]+\\b",
      replacement = "season"
    ) %>%
    gsub(
      x = ., "\\bselect[a-z]+\\b",
      replacement = "select"
    ) %>%
    gsub(
      x = ., "\\bmodel[a-z]+\\b",
      replacement = "model"
    ) %>%
    gsub(
      x = ., "\\breduc[a-z]+\\b",
      replacement = "reduce"
    ) %>%
    gsub(
      x = ., "\\bsurviv[a-z]+\\b",
      replacement = "survive"
    ) %>%
    gsub(
      x = ., "\\bsyntop[a-z]+\\b",
      replacement = "syntopic"
    ) %>%
    gsub(
      x = ., "\\bsympatr[a-z]+\\b",
      replacement = "sympatry"
    ) %>%
    gsub(
      x = ., "\\baccept[a-z]+\\b",
      replacement = "accept"
    ) %>%
    gsub(
      x = ., "\\baccess[a-z]+\\b",
      replacement = "access"
    ) %>%
    gsub(
      x = ., "\\badapt[a-z]+\\b",
      replacement = "adaptation"
    ) %>%
    gsub(
      x = ., "\\baggress[a-z]+\\b",
      replacement = "aggression"
    ) %>%
    gsub(
      x = ., "\\balimentary\\b",
      replacement = "trophic"
    ) %>%
    gsub(
      x = ., "\\basses[a-z]+\\b",
      replacement = "assessment"
    ) %>%
    gsub(
      x = ., "\\bnetwork[a-z]+\\b",
      replacement = "network"
    ) %>%
    gsub(
      x = ., "\\bevaluat[a-z]+\\b",
      replacement = "evaluation"
    ) %>%
    gsub(
      x = ., "\\bavoid[a-z]+\\b",
      replacement = "avoid"
    ) %>%
    gsub(
      x = ., "\\bbentho\\b|\\bbentho[a-z]+\\b",
      replacement = "benthic"
    ) %>%
    gsub(
      x = ., "\\bbiochem[a-z]+\\b",
      replacement = "biochemistry"
    ) %>%
    gsub(
      x = ., "\\bbival[a-z]+\\b",
      replacement = "bivalve"
    ) %>%
    gsub(
      x = ., "\\balimentary\\b",
      replacement = "trophic"
    ) %>%
    gsub(
      x = ., "\\bcarpio\\b|\\bcyprinus\\b",
      replacement = "carp"
    ) %>%
    gsub(
      x = ., "\\bclassif[a-z]+\\b",
      replacement = "classify"
    ) %>%
    gsub(
      x = ., "\\bcloac[a-z]+\\b",
      replacement = "cloacal"
    ) %>%
    gsub(
      x = ., "\\bcoexis[a-z]+\\b",
      replacement = "coexistence"
    ) %>%
    gsub(
      x = ., "\\bcongener[a-z]+\\b",
      replacement = "congener"
    ) %>%
    gsub(
      x = ., "\\bdailly\\b",
      replacement = "daily"
    ) %>%
    gsub(
      x = ., "\\bdepend[a-z]+\\b",
      replacement = "depend"
    ) %>%
    gsub(
      x = ., "\\bdetect[a-z]+\\b",
      replacement = "detect"
    ) %>%
    gsub(
      x = ., "\\bdiagnos[a-z]+\\b|\\btdiagnos[a-z]+\\b",
      replacement = "diagnosis"
    ) %>%
    gsub(
      x = ., "\\bdiverg[a-z]+\\b",
      replacement = "divergent"
    ) %>%
    gsub(
      x = ., "\\bdramatic[a-z]+\\b",
      replacement = "dramatic"
    ) %>%
    gsub(
      x = ., "\\bdrastic[a-z]+\\b",
      replacement = "drastic"
    ) %>%
    gsub(
      x = ., "\\beffectiv[a-z]+\\b",
      replacement = "effective"
    ) %>%
    gsub(
      x = ., "\\benerget[a-z]+\\b",
      replacement = "energetic"
    ) %>%
    gsub(
      x = ., "\\benviron\\b",
      replacement = "environment"
    ) %>%
    gsub(
      x = ., "\\bevalu[a-z]+\\b",
      replacement = "evaluate"
    ) %>%
    gsub(
      x = ., "\\bgeographic[a-z]+\\b",
      replacement = "geographic"
    ) %>%
    gsub(
      x = ., "\\bgeoloc[a-z]+\\b",
      replacement = "geolocation"
    ) %>%
    gsub(
      x = ., "\\blatit[a-z]+\\b",
      replacement = "latitudinal"
    ) %>%
    gsub(
      x = ., "\\blocomot[a-z]+\\b",
      replacement = "locomotor"
    ) %>%
    gsub(
      x = ., "\\bmedic[a-z]+\\b|\\banaesthesi[a-z]+\\b|\\banesthesi[a-z]+\\b|\\bveterinar[a-z]+\\b|\\bclinic[a-z]+\\b|\\bsurgery\\b|\\bsurgical\\b",
      replacement = "medicine"
    ) %>%
    gsub(
      x = ., "\\bmetabol[a-z]+\\b",
      replacement = "metabolism"
    ) %>%
    gsub(
      x = ., "\\bmonitor[a-z]+\\b|\\bmonitor\\b|\\bsurvey\\b|\\bsurvey[a-z]+\\b",
      replacement = "monitoring"
    ) %>%
    gsub(
      x = ., "\\bgadus\\b|\\bmorhua\\b",
      replacement = "cod"
    ) %>%
    gsub(
      x = ., "\\boverlap[a-z]+\\b",
      replacement = "overlap"
    ) %>%
    gsub(
      x = ., "\\boxygan\\b",
      replacement = "oxygen"
    ) %>%
    gsub(
      x = ., "\\bphenolo[a-z]+\\b",
      replacement = "phenology"
    ) %>%
    gsub(
      x = ., "\\bpiscivor[a-z]+\\b",
      replacement = "piscivore"
    ) %>%
    gsub(
      x = ., "\\bprevalen[a-z]+\\b",
      replacement = "prevalence"
    ) %>%
    gsub(
      x = ., "\\bpriorit[a-z]+\\b",
      replacement = "priority"
    ) %>%
    gsub(
      x = ., "\\bsibl\\b",
      replacement = "sibling"
    ) %>%
    gsub(
      x = ., "\\bstochast[a-z]+\\b",
      replacement = "stochastic"
    ) %>%
    gsub(
      x = ., "\\bsustain[a-z]+\\b",
      replacement = "sustainability"
    ) %>%
    gsub(
      x = ., "\\btaxa\\b|\\bclade\\b",
      replacement = "taxon"
    ) %>%
    gsub(
      x = ., "\\bthreat\\b|\\bthreat[a-z]+\\b",
      replacement = "threat"
    ) %>%
    gsub(
      x = ., "\\btopograph[a-z]+\\b",
      replacement = "topography"
    ) %>%
    gsub(
      x = ., "\\btranslocat[a-z]+\\b",
      replacement = "translocation"
    ) %>%
    gsub(
      x = ., "\\brelocat[a-z]+\\b",
      replacement = "relocation"
    ) %>%
    gsub(
      x = ., "\\bwintere\\b|\\bwinte\\b",
      replacement = "winter"
    ) %>%
    gsub(
      x = ., "\\ballomet[a-z]+\\b",
      replacement = "allometry"
    ) %>%
    gsub(
      x = ., "\\banthrop[a-z]+\\b",
      replacement = "anthropogenic"
    ) %>%
    gsub(
      x = ., "\\bantigen[a-z]+\\b",
      replacement = "antigen"
    ) %>%
    gsub(
      x = ., "\\barriv[a-z]+\\b",
      replacement = "arrive"
    ) %>%
    gsub(
      x = ., "\\barter[a-z]+\\b",
      replacement = "artery"
    ) %>%
    gsub(
      x = ., "\\bbias[a-z]+\\b",
      replacement = "bias"
    ) %>%
    gsub(
      x = ., "\\bbioacc[a-z]+\\b",
      replacement = "bioaccumulation"
    ) %>%
    gsub(
      x = ., "\\bborrel[a-z]+\\b|\\bburgdorfer[a-z]+\\b",
      replacement = "borrelia"
    ) %>%
    gsub(
      x = ., "\\bbrucell[a-z]+\\b",
      replacement = "brucella"
    ) %>%
    gsub(
      x = ., "\\bcardiovascular\\b",
      replacement = "cardiac"
    ) %>%
    gsub(
      x = ., "\\bchromosom[a-z]+\\b",
      replacement = "chromosome"
    ) %>%
    gsub(
      x = ., "\\bcrani[a-z]+\\b",
      replacement = "cranial"
    ) %>%
    gsub(
      x = ., "\\bdead\\b",
      replacement = "death"
    ) %>%
    gsub(
      x = ., "\\bdenn[a-z]+\\b",
      replacement = "denn"
    ) %>%
    gsub(
      x = ., "\\bdispers[a-z]+\\b",
      replacement = "dispersal"
    ) %>%
    gsub(
      x = ., "\\bspermiog[a-z]+\\b|\\btspermiog[a-z]+\\b",
      replacement = "spermiogenesis"
    ) %>%
    gsub(
      x = ., "\\bdisrupt[a-z]+\\b",
      replacement = "disrupt"
    ) %>%
    gsub(
      x = ., "\\bendem[a-z]+\\b",
      replacement = "endemism"
    ) %>%
    gsub(
      x = ., "\\benzym[a-z]+\\b",
      replacement = "enzyme"
    ) %>%
    gsub(
      x = ., "\\bepidid[a-z]+\\b",
      replacement = "epididymal"
    ) %>%
    gsub(
      x = ., "\\bextinc[a-z]+\\b",
      replacement = "extinct"
    ) %>%
    gsub(
      x = ., "\\bhealt[a-z]+\\b",
      replacement = "health"
    ) %>%
    gsub(
      x = ., "\\bheteroz[a-z]+\\b",
      replacement = "heterozygosity"
    ) %>%
    gsub(
      x = ., "\\bhistolog[a-z]+\\b",
      replacement = "histology"
    ) %>%
    gsub(
      x = ., "\\bhistopat[a-z]+\\b",
      replacement = "histopathology"
    ) %>%
    gsub(
      x = ., "\\bhistoric[a-z]+\\b",
      replacement = "historical"
    ) %>%
    gsub(
      x = ., "\\bhomoz[a-z]+\\b",
      replacement = "homozygosity"
    ) %>%
    gsub(
      x = ., "\\binfest[a-z]+\\b",
      replacement = "infestation"
    ) %>%
    gsub(
      x = ., "\\bterritor[a-z]+\\b",
      replacement = "territory"
    ) %>%
    gsub(
      x = ., "\\bhabitat[a-z]+\\b",
      replacement = "habitat"
    ) %>%
    gsub(
      x = ., "\\binflamm[a-z]+\\b",
      replacement = "inflammatory"
    ) %>%
    gsub(
      x = ., "\\bimpair[a-z]+\\b",
      replacement = "impair"
    ) %>%
    gsub(
      x = ., "\\binterspec[a-z]+\\b",
      replacement = "interspecific"
    ) %>%
    gsub(
      x = ., "\\bintraspec[a-z]+\\b",
      replacement = "intraspecific"
    ) %>%
    gsub(
      x = ., "\\bmatur[a-z]+\\b",
      replacement = "maturity"
    ) %>%
    gsub(
      x = ., "\\bmicroclimat[a-z]+\\b",
      replacement = "microclimatic"
    ) %>%
    gsub(
      x = ., "\\bmucosa[a-z]+\\b",
      replacement = "mucosa"
    ) %>%
    gsub(
      x = ., "\\bnecrotic\\b",
      replacement = "necrosis"
    ) %>%
    gsub(
      x = ., "\\bneuron[a-z]+\\b",
      replacement = "neuronal"
    ) %>%
    gsub(
      x = ., "\\bnutrit[a-z]+\\b",
      replacement = "nutrition"
    ) %>%
    gsub(
      x = ., "\\boccup[a-z]+\\b",
      replacement = "occupancy"
    ) %>%
    gsub(
      x = ., "\\bocean[a-z]+\\b",
      replacement = "ocean"
    ) %>%
    gsub(
      x = ., "\\bodoran[a-z]+\\b|\\bodou\\b",
      replacement = "odour"
    ) %>%
    gsub(
      x = ., "\\bontog[a-z]+\\b",
      replacement = "ontogeny"
    ) %>%
    gsub(
      x = ., "\\bovar[a-z]+\\b",
      replacement = "ovary"
    ) %>%
    gsub(
      x = ., "\\bprotoz[a-z]+\\b",
      replacement = "protozoa"
    ) %>%
    gsub(
      x = ., "\\briv[a-z]+\\b",
      replacement = "river"
    ) %>%
    gsub(
      x = ., "\\bsarcopt[a-z]+\\b",
      replacement = "sarcoptic"
    ) %>%
    gsub(
      x = ., "\\btsarcocystis\\b",
      replacement = "sarcocystis"
    ) %>%
    gsub(
      x = ., "\\bskelet[a-z]+\\b",
      replacement = "skeleton"
    ) %>%
    gsub(
      x = ., "\\bseropositiv[a-z]+\\b",
      replacement = "seropositive"
    ) %>%
    gsub(
      x = ., "\\bstimul[a-z]+\\b",
      replacement = "stimulus"
    ) %>%
    gsub(
      x = ., "\\btbasis\\b",
      replacement = "basis"
    ) %>%
    gsub(
      x = ., "\\btranscript[a-z]+\\b",
      replacement = "transcript"
    ) %>%
    gsub(
      x = ., "\\btrichostrong[a-z]+\\b",
      replacement = "trichostrongylida"
    ) %>%
    gsub(
      x = ., "\\btumour\\b|\\btumor\\b|\\bcancer[a-z]+\\b|\\bneoplasi[a-z]+\\b|\\bcarcinogen[a-z]+\\b",
      replacement = "cancer"
    ) %>%
    gsub(
      x = ., "\\bvaccin[a-z]+\\b",
      replacement = "vaccine"
    ) %>%
    gsub(
      x = ., "\\boocyt[a-z]+\\b",
      replacement = "oocyte"
    ) %>%
    gsub(
      x = ., "\\bprevitellog[a-z]+\\b",
      replacement = "previtellogenesis"
    ) %>%
    gsub(
      x = ., "\\bvitellog[a-z]+\\b",
      replacement = "vitellogenesis"
    ) %>%
    gsub(
      x = ., "\\bmutual[a-z]+\\b",
      replacement = "mutualism"
    ) %>%
    gsub(
      x = ., "\\bpaedomorph[a-z]+\\b|\\bpedomorph[a-z]+\\b|\\bneoten[a-z]+\\b",
      replacement = "neoteny"
    ) %>%
    gsub(
      x = ., "\\bdeterm[a-z]+\\b",
      replacement = "determination"
    )
}

# Repeat stopwords removal in case lemmatization changed something
for (x in names(art_per_class)) {
  for (y in my_stopwords_split) {
    art_per_class[[x]]$stm_text <-
      tm::removeWords(x = art_per_class[[x]]$stm_text, y)
  }
}

for (x in names(art_per_class)) {
  art_per_class[[x]]$stm_text <-
    tm::removePunctuation(x = art_per_class[[x]]$stm_text) %>%
    tm::removeNumbers(.) %>%
    str_squish()
}

art_df$auto_ID <- tolower(art_df$auto_ID) %>% lemmatize_strings()

# STM pre-processing
to_stm <- mclapply(art_per_class, function(x) {
  x[["word_count"]] <- str_count(x[["stm_text"]], "\\S+")
  x <-
    x %>% filter(word_count > 24) # Remove documents with less than 25 words
  texts <-
    textProcessor(
      x[["stm_text"]],
      metadata = x,
      stem = F,
      ucp = T,
      onlycharacter = T,
      striphtml = T,
    )

  # l.tresh <-
  #   if_else(0.01 * length(texts$documents) < 5,
  #           5,
  #           15)
  u.tresh <- 0.9 * length(texts$documents)

  prep_stm <-
    prepDocuments(
      documents = texts$documents,
      vocab = texts$vocab,
      meta = texts$meta,
      lower.thresh = 10, # l.tresh,
      upper.thresh = u.tresh
    )
  return(prep_stm)
}, mc.cores = 4)
names(to_stm) <- names(art_per_class)


for (x in names(to_stm)) {
  # Create prevalence variables with automatic ids from Web of Science
  to_stm[[x]]$meta <- art_df %>%
    select(my_ID, auto_ID) %>%
    filter(my_ID %in% unique(to_stm[[x]]$meta$my_ID)) %>%
    unnest_tokens(., wos_cat, auto_ID,
      token = "regex", pattern = ";|\\s"
    ) %>%
    mutate(
      wos_cat = str_squish(wos_cat) %>%
        gsub(x = ., pattern = "[[:punct:]]", "") %>%
        gsub(x = ., pattern = "\\&", ""),
      values_from = 1
    ) %>%
    filter(wos_cat %in% prev_vars$wos_cat) %>%
    inner_join(., prev_vars) %>%
    select(my_ID, prev_var, values_from) %>%
    unique() %>%
    group_by(prev_var) %>%
    filter(n() > 4) %>%
    pivot_wider(
      names_from = prev_var,
      values_from = values_from,
      values_fill = 0,
      names_prefix = "prev_"
    ) %>%
    left_join(to_stm[[x]]$meta, ., by = "my_ID") %>%
    mutate(across(starts_with("prev_"), ~ replace_na(.x, 0)))

  to_stm[[x]]$meta <- select(art_df, my_ID, AB) %>%
    group_by(my_ID) %>%
    slice_head(., n = 1) %>%
    left_join(to_stm[[x]]$meta, by = "my_ID", .) %>%
    relocate(., AB, .after = my_ID)
}

saveRDS(to_stm, "to_stm.rds")
# system(paste0("ln ",getwd(),"/to_stm.rds ",getwd(),"/10_networks/to_stm.rds"))

# Search best K number of topics
# jobRunScript("5_search_best_K.R", importEnv = T)

# Run candidate models
jobRunScript("6_run_STM_models.R",
  importEnv = T,
  exportEnv = "R_GlobalEnv"
)
