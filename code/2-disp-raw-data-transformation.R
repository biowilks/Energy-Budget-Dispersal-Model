rm(list=ls())

setwd("~/Energy-Budget-Model/data")

# Load all packages ----------
library(tidyverse)
library(rgbif)

# Import trait datasets ----------
db <- read.csv("DispersalUntransformed.csv")|>
  filter(Statistic == "Maximum")

# Wilman2014 - EltonTraits
eltonbird<-read_tsv('BirdFuncDat.txt')
eltonmam<-read_tsv('MamFuncDat.txt')

# Boettiger2023 - FishBase
fb <-read_tsv('FishBase.txt')
# Boettiger2023 - SeaLifeBase
slb <-read_tsv('SealifeBase.txt')

# Herberstein2022
anidat<-read_tsv('AnimalTraits.txt')



# 1) Data harmonisation ----------
# extract species names
setdiff(c('a', 'b', 'c'), c('a', 'b')) #find the differences between the the two sets

res <- bind_rows(
  db |>
    transmute(species = `Species.ID`, db = 'dispdb') |>
    distinct_all(),
  eltonbird |>
    transmute(species = Scientific, db = 'eltonbirds') |>
    distinct_all(),
  eltonmam |>
    transmute(species = Scientific, db = 'eltonmamm') |>
    distinct_all(),
  fb |>
    transmute(species = Species, db = 'FishBase') |>
    distinct_all(),
  slb |>
    transmute(species = Species, db = 'SeaLifeBase') |>
    distinct_all(),
  anidat |>
    transmute(species = species, db = 'AnimalTraits') |>
    distinct_all()
)

# Harmonisation using rgbif
checklist <- res |>
  mutate(species = gsub('[?]', '', species)) |>
  filter(validUTF8(species))

chunksize <- 1e3 #limit is 1e3
res <- as.list(rep(NA, ceiling( nrow(checklist) / chunksize )))

for (i in seq_along(res)) {
  ### TO DO name_parse, if it fails there are issue with the name, try delete everything after second word.
  message(i)
  start <- chunksize * (i - 1) + 1
  end <- min(start + chunksize - 1, nrow(checklist))
  sp <- checklist[["species"]][start:end] #species to check
  db <- checklist[["db"]][start:end] #database source to append at the end
  first.pass <- name_backbone_checklist(sp) #get taxonomy from gbif
  synonyms <- first.pass |>
    mutate(status = ifelse(is.na(status), "NOT FOUND", status)) |>
    filter(status != 'ACCEPTED') #select only synonyms
  keys <- sapply(synonyms[["canonicalName"]], function(x) {
    if (is.na(x)) {
      return (NA)
    } else {
      return (name_backbone(x)$usageKey)
    }
  }, USE.NAMES = FALSE) #get synonyms keys
  second.pass <- lapply(keys, function(x) {
    if (is.na(x)) {
      column_names <- c('key','scientificName','nubKey','nameKey','taxonID','sourceTaxonKey','kingdom','phylum','order','family','genus','species','kingdomKey','phylumKey','classKey','orderKey','familyKey','genusKey','speciesKey','datasetKey','constituentKey','parentKey','parent','acceptedKey','accepted','basionymKey','basionym','canonicalName','vernacularName','authorship','nameType','rank','origin','taxonomicStatus','nomenclaturalStatus','remarks','numDescendants','lastCrawled','lastInterpreted','issues','synonym','class')
      ans <- as.list(rep(NA, length(column_names)))
      names(ans) <- column_names
      ans <- ans |> as_tibble()
      return (ans)
    } else {
      return (name_usage(x)$data) #deeper search of synonyms
    }
  }) |>
    bind_rows() |>
    mutate(status = taxonomicStatus,
           acceptedUsageKey = key)

  # combine back together

  second.pass.sp <- first.pass |> #species that go to second.pass
    rownames_to_column() |>
    mutate(status = ifelse(is.na(status), "NOT FOUND", status)) |>
    filter(status != 'ACCEPTED') |>
    pull(rowname) |>
    as.integer()

  stopifnot (length(second.pass.sp) == nrow(second.pass))

  res[[i]] <- first.pass |>
    mutate(original = sp, db = db) |>
    filter(status == "ACCEPTED") |>
    bind_rows(second.pass |>
                mutate(original = sp[second.pass.sp], db = db[second.pass.sp])) |>
    transmute(
      db,
      original,
      gbif.binomial = canonicalName,
      gbif.key = usageKey,
      rank,
      status,
      matchType,
      phylum,
      class,
      family,
      genus,
      species,

    )

}


res <- bind_rows(res) #bind chunks

# overview of taxonomy and match type from rgbif
overviewtaxa <- res



# 2) Adding body mass ----------
db <- read.csv("DispersalUntransformed.csv")|>
  filter(Statistic == "Maximum")

# Making the Body.mass column 'NA' if an empty cell
db1 <- db |>
  mutate_at(vars(Body.mass), ~ifelse(. == "", NA, .))

# Left join body mass data by gbif.binomial
birds <- eltonbird |>
  transmute(
    original = Scientific,
    mass = `BodyMass-Value`
  ) |>
  drop_na() |>
  distinct_all() |>
  left_join(res, by = "original") |>
  group_by(gbif.binomial) |>
  summarize(birdmass = mean(mass), .groups = "drop")

mamm <- eltonmam |>
  transmute(
    original = Scientific,
    mass = `BodyMass-Value`
  ) |>
  drop_na() |>
  distinct_all() |>
  left_join(res, by = "original") |>
  group_by(gbif.binomial) |>
  summarize(mammalmass = mean(mass), .groups = "drop")

fish <- fb |>
  transmute(
    original = Species,
    mass = `Weight`
  ) |>
  drop_na() |>
  distinct_all() |>
  left_join(res, by = "original") |>
  group_by(gbif.binomial) |>
  summarize(fishmass = mean(mass), .groups = "drop")

sealife <- slb |>
  transmute(
    original = Species,
    mass = `Weight`
  ) |>
  drop_na() |>
  distinct_all() |>
  left_join(res, by = "original") |>
  group_by(gbif.binomial) |>
  summarize(seamass = mean(mass), .groups = "drop")

anim<-anidat |>
  transmute(
    original = species,
    mass = `body mass`
  ) |>
  drop_na() |>
  distinct_all() |>
  left_join(res, by = "original", relationship = 'many-to-many') |>
  group_by(gbif.binomial) |>
  summarize(animass = mean(mass), .groups = "drop")

# Combine mass data from different sources into a single 'masses' data frame
masses <- bind_rows(
  birds |> select(gbif.binomial, birdmass),
  mamm |> select(gbif.binomial, mammalmass),
  anim |> select(gbif.binomial, animass),
  fish |> select(gbif.binomial, fishmass),
  sealife |> select(gbif.binomial, seamass)
) |>
  mutate(
    Mass = coalesce(birdmass, mammalmass, fishmass, seamass, animass),
    Source = case_when(
      !is.na(birdmass) ~ "Wilman et al 2014",
      !is.na(mammalmass) ~ "Wilman et al 2014",
      !is.na(fishmass) ~ "Boettiger et al. 2023",
      !is.na(seamass) ~ "Boettiger et al. 2023",
      !is.na(animass) ~ "Herberstein et al 2022",
      TRUE ~ NA_character_
    ),
    Unit = case_when(
      !is.na(birdmass) ~ "g",
      !is.na(mammalmass) ~ "g",
      !is.na(animass) ~ "kg",
      !is.na(fishmass) ~ "g",
      !is.na(seamass) ~ "g",
      TRUE ~ NA_character_
    )
    ) |>
  select(-c(birdmass, mammalmass, fishmass, seamass, animass))

# Left join by taxonomy, adding in trait refs to the body mass source column
db2 <- db1 |>
  left_join(res, by = c(Species.ID = "original"), relationship = "many-to-many") |>
  mutate(
    Body.mass.source = case_when (!is.na(Body.mass)&!is.na(Body.mass.ref) ~ MetaRef,
                                  !is.na(Body.mass)&is.na(Body.mass.ref) ~ MetaRef, T ~ "databases")
  )

# Filtering columns
db2 <- db2|>
  select(-gbif.key,-rank,-status,-matchType,-phylum,-genus,-species)|>
  rename('Species.ID.reported' = 'Species.ID')|>
  relocate(gbif.binomial,.before = Movement.type)|>
  relocate(class,.before = Movement.type)|>
  relocate(family,.before = Movement.type)

movement <- db2 |>
  filter(Body.mass.source != "databases") |>
  select(-db,-Body.mass.ref)

traits <- db2 |>
  filter(Body.mass.source == "databases")

# Include missing body mass data from the masses data frame
traits1 <- traits |>
  left_join(masses, by = c("gbif.binomial" = "gbif.binomial"),relationship = "many-to-many") |>
  mutate(
    Body.mass =  Mass,
    Body.mass.source = Source,
    Body.mass.units = Unit) |>
  select(-Source, -Mass, -Unit,-Body.mass.ref,-db)

# Define the priority order for Body mass database sources
source_priority <- c("Wilman et al 2014", "Boettiger et al. 2023", "Herberstein et al 2022")

# Group by Event ID and Body mass source, and select one row based on source priority
traits2 <- traits1 |>
  group_by(`Event.ID`) |>
  arrange(match(Body.mass.source, source_priority)) |>
  slice_head() |>
  ungroup()

# Combine the data frames
db3 <- rbind(movement, traits2)

# Remove duplicates
db4 <- db3 |>
  group_by(`Event.ID`) |>
  slice_head() |>
  ungroup()

# 3) Adding movement mode ----------
# Infer movement modes by class
## find class to search for the next step
unique(db4$class)

# Adding movement type based on class - google/wikipedia search
db5 <- db4 |>
  mutate(`Movement.mode` = case_when(
    class %in% c("Mammalia", "Squamata") ~ "Running",
    class == "Aves" ~ "Flying",
    class %in% c("Amphibia", "Testudines") ~ "Mixed",
    class == c("Elasmobranchii") ~ "Swimming",
    TRUE ~ `Movement.mode`
  )) |>
  mutate(
    `Movement Mode Ref` = ifelse(!is.na(`Movement.mode`), paste("Inferred from Class: ", class), `Movement Mode Ref`)
  )

# Infer movement modes from family
# To find out family names to find movement mode for
namovementdata <- db5 %>%
  filter(is.na(Movement.mode) | Movement.mode == "") %>%
  mutate(`Movement Mode Ref` = NA)

unique(namovementdata$family)

db6 <- db5 |>
  mutate(`Movement.mode` = case_when(
    family %in% c("Cyprinidae","Cottidae","Anostomidae","Poeciliidae","Acipenseridae","Esocidae","Lotidae","Polyodontidae","Centrarchidae",
                       "Percidae","Percichthyidae","Prochilodontidae","Rivulidae","Fundulidae","Moronidae","Ictaluridae","Anguillidae","Pimelodidae",
                       "Salmonidae","Bryconidae","Alestidae","Cichlidae","Bryconidae", "Catostomidae","Nemacheilidae","Doradidae","Serrasalmidae","Lepisosteidae", "Delphinidae") ~ "Swimming",
    family %in% c("Curculionidae", "Lucanidae", "Passalidae","Cerambycidae","Scarabaeidae","Elateridae","Cleridae","Monotomidae","Ciidae", "Tenebrionidae","Scolytidae") ~ "Mixed",
    family %in% c("Syrphidae","Dolichopodidae") ~ "Flying",
    family %in% c("Talpidae") ~ "Digging",
    TRUE ~ `Movement.mode`
  )) |>
  mutate(`Movement Mode Ref` = case_when(
    family %in% c("Cyprinidae","Cottidae","Anostomidae","Poeciliidae","Acipenseridae","Esocidae","Lotidae","Polyodontidae","Centrarchidae",
                       "Percidae","Percichthyidae","Prochilodontidae","Rivulidae","Fundulidae","Moronidae","Ictaluridae","Anguillidae","Pimelodidae",
                       "Salmonidae","Bryconidae","Alestidae","Cichlidae","Bryconidae", "Catostomidae","Nemacheilidae","Doradidae","Serrasalmidae","Lepisosteidae",
                       "Curculionidae", "Lucanidae", "Passalidae","Cerambycidae","Scarabaeidae","Elateridae","Cleridae","Monotomidae","Ciidae", "Tenebrionidae",
                       "Scolytidae","Syrphidae","Dolichopodidae","Delphinidae") ~ paste("Inferred from Family:", family),
    TRUE ~ `Movement Mode Ref`
  ))

# Infer movement modes from species

# Running movement mode:
# bird species (Gallirallus australis, Tribonyx mortierii, Dromaius novaehollandiae, Rhea americana, Rhea pennata, Gallirallus australis,
# Tympanuchus cupido, Tympanuchus pallidicinctus)

# Swimming movement mode:
# All fish species, mammal species (Physeter catodon, Castor canadensis, Castor fiber, Lontra canadensis, Mustela vison),
# bird species (Podiceps cristatus, Cygnus olor, Phalacrocorax pelagicus, Cygnus buccinator)

# Other movement mode:
# Thomomys bottae (digging), Pteromys volans (gliding), Hylobates lar, Trichosurus vulpecula, Phascolarctos cinereus, Alouatta palliata, Sciurus carolinensis, Sciurus niger,
# Macaca sylvanus, Microcebus murinus, Tamiasciurus hudsonicus, Rupicapra rupicapra, Rupicapra pyrenaica (climbing), Ursus maritimus (mixed)

na_movement_data1 <- db6 %>%
  filter(is.na(Movement.mode) | Movement.mode == "") %>%
  mutate(`Movement Mode Ref` = NA)

unique(na_movement_data1$gbif.binomial)

db7 <- db6 |>
  mutate(`Movement.mode` = case_when(
    gbif.binomial %in% c("Abramis brama", "Acipenser fulvescens", "Ambloplites rupestris", "Anablepsoides hartii", "Anguilla australis australis",
                         "Anguilla dieffenbachii", "Anguilla japonica", "Anguilla rostrata", "Barbus barbus", "Barbus haasi", "Brycon orbignyanus",
                         "Catostomus clarkii", "Catostomus insignis", "Catostomus occidentalis", "Chrosomus eos", "Clinostomus funduloides",
                         "Colossoma macropomum", "Cottus bairdii", "Cottus cognatus", "Cottus gobio", "Cottus pollux", "Cottus rhenanus",
                         "Ctenopharyngodon idella", "Cyprinella caerulea", "Cyprinus carpio", "Esox lucius", "Etheostoma flabellare",
                         "Etheostoma fonticola", "Etheostoma nigrum", "Etheostoma podostemone", "Fundulus notatus", "Fundulus olivaceus",
                         "Gobio gobio", "Hemisorubim platyrhynchos", "Hydrocynus vittatus", "Hypophthalmichthys nobilis", "Lavinia exilicauda",
                         "Lefua echigonia", "Lepidomeda aliciae", "Lepisosteus osseus", "Lepomis auritus", "Lepomis cyanellus", "Lepomis megalotis",
                         "Leporinus elongatus", "Leporinus friderici", "Leporinus obtusidens", "Leuciscus idus", "Lota lota", "Maccullochella macquariensis",
                         "Macquaria ambigua", "Micropterus dolomieu", "Micropterus salmoides", "Morone saxatilis", "Mylopharodon conocephalus",
                         "Nocomis leptocephalus", "Notropis lutipinnis", "Oncorhynchus clarkii", "Oncorhynchus kisutch", "Oncorhynchus masou masou",
                         "Oncorhynchus mykiss", "Oncorhynchus tshawytscha", "Oreochromis andersonii", "Oxydoras kneri", "Piaractus mesopotamicus",
                         "Pimelodus albicans", "Pimelodus maculatus", "Pinirampus pirinampu", "Poecilia gillii", "Polyodon spathula",
                         "Prochilodus lineatus", "Prosopium williamsoni", "Pseudoplatystoma corruscans", "Pterodoras granulosus",
                         "Ptychocheilus grandis", "Ptychocheilus lucius", "Ptychocheilus oregonensis", "Pylodictis olivaris", "Rhinichthys atratulus",
                         "Rhinichthys cataractae", "Rutilus rutilus", "Salminus brasiliensis", "Salmo salar", "Salmo trutta", "Salvelinus confluentus",
                         "Salvelinus fontinalis", "Salvelinus leucomaenis leucomaenis", "Salvelinus malma", "Sander", "Sander lucioperca",
                         "Sander vitreus", "Sargochromis giardi", "Scaphirhynchus albus", "Scaphirhynchus platorynchus", "Schizodon borellii",
                         "Semotilus atromaculatus", "Serranochromis altus", "Sorubim lima", "Squalius torgalensis", "Thymallus thymallus",
                         "Tinca tinca", "Xyrauchen texanus", "Zungaro jahu",
                         "Castor canadensis", "Castor fiber", "Lontra canadensis", "Mustela vison", "Physeter catodon",
                         "Podiceps cristatus", "Cygnus olor", "Phalacrocorax pelagicus","Cygnus buccinator") ~ "Swimming",
    gbif.binomial %in% c("Gallirallus australis", "Tribonyx mortierii", "Dromaius novaehollandiae", "Rhea americana","Rhea pennata","Tympanuchus cupido", "Tympanuchus pallidicinctus",
                         "Speotyto cunicularia", "Passerculus sandwichensis", "Zonotrichia leucophrys") ~ "Running",
    gbif.binomial %in% c("Thomomys bottae", "Hylobates lar", "Trichosurus vulpecula", "Phascolarctos cinereus", "Alouatta palliata",
                         "Sciurus carolinensis", "Sciurus niger", "Macaca sylvanus", "Microcebus murinus", "Tamiasciurus hudsonicus", "Pteromys volans",
                         "Rupicapra rupicapra", "Rupicapra pyrenaica","Ursus maritimus") ~ "Other",
    TRUE ~ `Movement.mode`
  )) |>
  mutate(`Movement Mode Ref` = case_when(
    gbif.binomial %in% c("Abramis brama", "Acipenser fulvescens", "Ambloplites rupestris", "Anablepsoides hartii", "Anguilla australis australis",
                         "Anguilla dieffenbachii", "Anguilla japonica", "Anguilla rostrata", "Barbus barbus", "Barbus haasi", "Brycon orbignyanus",
                         "Catostomus clarkii", "Catostomus insignis", "Catostomus occidentalis", "Chrosomus eos", "Clinostomus funduloides",
                         "Colossoma macropomum", "Cottus bairdii", "Cottus cognatus", "Cottus gobio", "Cottus pollux", "Cottus rhenanus",
                         "Ctenopharyngodon idella", "Cyprinella caerulea", "Cyprinus carpio", "Esox lucius", "Etheostoma flabellare",
                         "Etheostoma fonticola", "Etheostoma nigrum", "Etheostoma podostemone", "Fundulus notatus", "Fundulus olivaceus",
                         "Gobio gobio", "Hemisorubim platyrhynchos", "Hydrocynus vittatus", "Hypophthalmichthys nobilis", "Lavinia exilicauda",
                         "Lefua echigonia", "Lepidomeda aliciae", "Lepisosteus osseus", "Lepomis auritus", "Lepomis cyanellus", "Lepomis megalotis",
                         "Leporinus elongatus", "Leporinus friderici", "Leporinus obtusidens", "Leuciscus idus", "Lota lota", "Maccullochella macquariensis",
                         "Macquaria ambigua", "Micropterus dolomieu", "Micropterus salmoides", "Morone saxatilis", "Mylopharodon conocephalus",
                         "Nocomis leptocephalus", "Notropis lutipinnis", "Oncorhynchus clarkii", "Oncorhynchus kisutch", "Oncorhynchus masou masou",
                         "Oncorhynchus mykiss", "Oncorhynchus tshawytscha", "Oreochromis andersonii", "Oxydoras kneri", "Piaractus mesopotamicus",
                         "Pimelodus albicans", "Pimelodus maculatus", "Pinirampus pirinampu", "Poecilia gillii", "Polyodon spathula",
                         "Prochilodus lineatus", "Prosopium williamsoni", "Pseudoplatystoma corruscans", "Pterodoras granulosus",
                         "Ptychocheilus grandis", "Ptychocheilus lucius", "Ptychocheilus oregonensis", "Pylodictis olivaris", "Rhinichthys atratulus",
                         "Rhinichthys cataractae", "Rutilus rutilus", "Salminus brasiliensis", "Salmo salar", "Salmo trutta", "Salvelinus confluentus",
                         "Salvelinus fontinalis", "Salvelinus leucomaenis leucomaenis", "Salvelinus malma", "Sander", "Sander lucioperca",
                         "Sander vitreus", "Sargochromis giardi", "Scaphirhynchus albus", "Scaphirhynchus platorynchus", "Schizodon borellii",
                         "Semotilus atromaculatus", "Serranochromis altus", "Sorubim lima", "Squalius torgalensis", "Thymallus thymallus",
                         "Tinca tinca", "Xyrauchen texanus", "Zungaro jahu","Castor canadensis", "Castor fiber", "Lontra canadensis", "Mustela vison",
                         "Podiceps cristatus", "Cygnus olor", "Phalacrocorax pelagicus","Cygnus buccinator", "Gallirallus australis", "Tribonyx mortierii",
                         "Dromaius novaehollandiae", "Rhea americana","Rhea pennata","Tympanuchus cupido", "Tympanuchus pallidicinctus", "Speotyto cunicularia",
                         "Passerculus sandwichensis", "Zonotrichia leucophrys", "Thomomys bottae", "Hylobates lar", "Trichosurus vulpecula", "Phascolarctos cinereus", "Alouatta palliata",
                         "Sciurus carolinensis", "Sciurus niger", "Macaca sylvanus", "Microcebus murinus", "Tamiasciurus hudsonicus", "Pteromys volans","Physeter catodon",
                         "Rupicapra rupicapra", "Rupicapra pyrenaica","Ursus maritimus") ~ paste("Inferred from Species:", gbif.binomial),
    TRUE ~ `Movement Mode Ref`
  ))

# 4) Filtering data to just include flying birds, running mammals and swimming fish ----------
db_final <- db7 %>%
  filter(!is.na(Body.mass)) %>%
  filter((Movement.mode == "Flying" & !class %in% c("Mammalia", "Insecta")) |
           (Movement.mode == "Running" & !class %in% c("Aves", "Squamata")) |
           (Movement.mode == "Swimming" & !class %in% c("Mammalia", "Aves")))

# 5) Harmonising all the units for distance and body masses ----------
# Excluding rows with km/h and ha in 'Units' column and with NA in 'Value' and 'Body mass' columns
db_final1 <- db_final |>
  filter(Units != "km/h", Units != "ha") |>
  filter(
    !is.na(Value), !is.na(Body.mass))

# Converting units for body mass and distance
# Converting all distance values to meters
db_final2 <- db_final1 |>
  mutate(Value = case_when(Units == "km"  ~ Value * 1000,
                           Units == "miles"  ~ Value * 1609.34,
                           Units == "m"~ Value,
                           TRUE ~ NA)) |>
  mutate(Units = "m")


# Converting all body mass values to grams
db_final3 <- db_final2 |>
  rename(Locomotion.mode = Movement.mode) |>
  rename(Locomotion.mode.source = `Movement Mode Ref`) |>
  mutate(Body.mass = case_when(Body.mass.units == "g" ~ Body.mass / 1000,
                               TRUE ~ Body.mass)) |>
  mutate(Body.mass.units = "kg")


# Save transformed data and taxa overview
setwd("~/Energy-Budget-Model/output")
write_csv(overviewtaxa, "taxonomy_gbif_match.csv")
write_csv(db_final3, "DispersalTransformed.csv")
