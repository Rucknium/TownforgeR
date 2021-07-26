
infl.effects.ls <- TownforgeR::tf_get_influence_effects()

save(infl.effects.ls, file = "data/infl_effects_ls.rda")

seed.word.list.v <- TownforgeR::tf_get_seed_words_list()

save(seed.word.list.v, file = "data/seed_word_list.rda")


building.names.contents <- 
  c("EMPTY"   ,  0, "ROLE_EMPTY",        "Empty",
    "AGR"     ,  1, "ROLE_AGRICULTURAL", "Agricultural",
    "CRAFT"   ,  2, "ROLE_CRAFT",        "Craft",
    "IND"     ,  3, "ROLE_INDUSTRIAL",   "Industrial",
    "COM"     ,  4, "ROLE_COMMERCIAL",   "Commercial",
    "RES1"    ,  5, "ROLE_RESIDENTIAL1", "Basic residential",
    "RES2"    ,  6, "ROLE_RESIDENTIAL2", "Affluent residential",
    "RES3"    ,  7, "ROLE_RESIDENTIAL3", "Luxury residential",
    "MIL"     ,  8, "ROLE_MILITARY",     "Military",
    "CUL"     ,  9, "ROLE_CULTURAL",     "Cultural",
    "STO"     , 10, "ROLE_STONECUTTER",  "Stonecutter",
    "SAW"     , 11, "ROLE_SAWMILL",      "Sawmill",
    "KILN"    , 12, "ROLE_KILN",         "Kiln",
    "SME"     , 13, "ROLE_SMELTER",      "Smelter",
    "WOR"     , 14, "ROLE_WORKFORCE",    "Workforce",
    "ROAD"    , 15, "ROLE_ROAD",         "Road",
    "RESEARCH", 16, "ROLE_RESEARCH",     "Research")

building.names.df <- as.data.frame(matrix(building.names.contents, ncol = 4, byrow = TRUE))
colnames(building.names.df) <- c("abbrev", "id", "role.name", "full.name")
building.names.df <- building.names.df[, c("id", "abbrev", "full.name", "role.name")]
building.names.df$id <- as.numeric(building.names.df$id)

save(building.names.df, file = "data/building_names_df.rda")

building.names.v <- building.names.df$abbrev
names(building.names.v) <- building.names.df$full.name

save(building.names.v, file = "data/building_names_v.rda")

min.size.scale.df <- TownforgeR::tf_get_min_flag_size_data()

save(min.size.scale.df, file = "data/min_size_scale_df.rda")

commodity.id.key <- as.data.frame(matrix(c(
  1,    "Sandstone",
  2,    "Granite",
  3,    "Marble",
  4,    "Pine",
  5,    "Oak",
  6,    "Teak",
  8,    "Runestone",
  256,  "Labour",
  257,  "Firewood",
  1024, "Vegetables",
  1025, "Grain",
  1026, "Meat",
  1027, "Salted meat"
), ncol = 2, byrow = TRUE))
colnames(commodity.id.key) <- c("id", "name")
commodity.id.key$id <- as.numeric(commodity.id.key$id)

save(commodity.id.key, file = "data/commodity_id_key.rda")

commodity.id.key.v <- commodity.id.key$id
names(commodity.id.key.v) <- commodity.id.key$name

save(commodity.id.key.v, file = "data/commodity_id_key_v.rda")


commodities.buildings.produce.df <- as.data.frame(matrix(c(
  1,    "Sandstone",   10, "STO",
  2,    "Granite",     10, "STO",
  3,    "Marble",      10, "STO",
  4,    "Pine",        11, "SAW",
  5,    "Oak",         11, "SAW",
  6,    "Teak",        11, "SAW",
  8,    "Runestone",   NA, NA,
  256,  "Labour",      14, "WOR",
  257,  "Firewood",    NA, NA,
  1024, "Vegetables",  NA, NA,
  1025, "Grain",       NA, NA,
  1026, "Meat",        NA, NA,
  1027, "Salted meat", NA, NA
), ncol = 4, byrow = TRUE))
colnames(commodities.buildings.produce.df) <- c("commodity.id", "commodity.name", "building.id", "building.abbrev")
commodities.buildings.produce.df$commodity.id <- as.numeric(commodities.buildings.produce.df$commodity.id)
commodities.buildings.produce.df$building.id <- as.numeric(commodities.buildings.produce.df$building.id)

save(commodities.buildings.produce.df, file = "data/commodities_buildings_produce_df.rda")


gold.unit.divisor <- 10e+7

save(gold.unit.divisor, file = "data/gold_unit_divisor.rda")

save(infl.effects.ls, seed.word.list.v, building.names.df, building.names.v, min.size.scale.df, 
  commodity.id.key, commodity.id.key.v, commodities.buildings.produce.df,
  gold.unit.divisor, file = "R/sysdata.rda")


# 1 = ag
# 2 = craft
# 5 = basic residential
# 10 = stonecutter
# 11 = sawmill
# 14 = workforce
# 15 = road
# ROLE_EMPTY = 0
# ROLE_AGRICULTURAL = 1
# ROLE_CRAFT = 2
# ROLE_RESIDENTIAL1 = 5
# ROLE_SAWMILL = 11
# ROLE_STONECUTTER = 10
# ROLE_WORKFORCE = 14
