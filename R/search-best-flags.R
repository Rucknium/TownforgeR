#' tf_search_best_flags
#'
#' Brute force search for flags with best production and return on investment
#'
#' @param url.rpc TODO
#' @param building.type TODO
#' @param economic.power TODO
#' @param get.flag.cost TODO
#' @param city TODO
#' @param grid.density.params TODO
#' @param in.shiny TODO
#' @param waitress TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_search_best_flags <- function(url.rpc, building.type, economic.power, get.flag.cost = TRUE, 
  city, grid.density.params = c(10, 10), in.shiny = FALSE, waitress = NULL) {
  # density.params: first element is y (north-south) and second is x (east-west)
  # TODO: make the flag cutouts include inactive flags
  
  #library(Matrix)
  
  #uint32_t city;
  #uint32_t x0;
  #uint32_t y0;
  #uint32_t x1;
  #uint32_t y1;
  #uint8_t role;
  #uint32_t economic_power;
  
  role.id <- building.names.df$id[building.names.df$abbrev == building.type]
  role.name <- building.names.df$role.name[building.names.df$abbrev == building.type]
  
  
  flag.bounds.ls <- TownforgeR::tf_flag_bounds(url.rpc, grid.dim = NULL, 
    coords.origin = NULL) 
  
  cutouts.grid <- flag.bounds.ls$bounds.grid
  
  grid.dim <- dim(cutouts.grid)
  
  infl.grid.disagg  <- TownforgeR::tf_infl_grid(url.rpc, building.type, "bonus", 
    grid.dim = grid.dim, coords.origin = flag.bounds.ls$coords.origin, disaggregated = TRUE)
  
  subject.to.influence <- ( ! "error" %in% names(infl.grid.disagg) )
  
  grid.density.y <- floor(seq(1, grid.dim[1], length.out = grid.density.params[1] + 1))
  grid.density.x <- floor(seq(1, grid.dim[2], length.out = grid.density.params[2] + 1))
  
  candidates.mat <- as.matrix(expand.grid(
    y = grid.density.y[ (-1) * length(grid.density.y)], 
    x = grid.density.x[ (-1) * length(grid.density.x)] ) )
  
  infl.thresholds <- c(0, 1, seq(1.5, 101, 1)) 
  
  candidates.ls <- vector("list", nrow(candidates.mat))
  names(candidates.ls) <- paste0("c(", candidates.mat[, "y"], ", ", candidates.mat[, "x"], ")")
  
  for (i.candidate in seq_along(candidates.ls)) {
    
    cat(i.candidate, " of ", length(candidates.ls), "    ", base::date(), "\n", sep = "")
    
    if (in.shiny) {
      
      #waitress$update(
      #  html = paste0("Expanding flag from candidate grid point ", i.candidate, " of ", length(candidates.ls), ""))
      
      waitress$set(i.candidate / length(candidates.ls) / 1.02)
      
      shiny::setProgress(
        value = i.candidate / length(candidates.ls),
        message = "Searching for best flag placements...",
        detail = paste0("Expanding flag from candidate grid point ", i.candidate, " of ", length(candidates.ls), "")
      )
    }
    
    if (cutouts.grid[as.matrix(candidates.mat[i.candidate, , drop = FALSE]) ] != 0 ) {
      next
    }
    # If the initial point is within another flag, then go to the next initial starting point candidate
    
    init.long <- north.coord <- south.coord <- unname(candidates.mat[i.candidate, 1])
    init.lat  <- east.coord  <- west.coord <- unname(candidates.mat[i.candidate, 2])
    
    min.flag.size <- TownforgeR::tf_min_flag_size(role.name, economic.power)
    north.coord <- north.coord + min.flag.size - 1
    east.coord <- east.coord + min.flag.size - 1
    
    expand.north <- TRUE
    expand.east <- TRUE
    expand.south <- TRUE
    expand.west <- TRUE
    
    candidates.ls[[i.candidate]] <- vector("list", 256 * 256)
    
    while(expand.north || expand.east || expand.south || expand.west) {
      
      if (expand.north) {
        if ( (north.coord - south.coord + 2 ) > 256 ||
            any(dim(cutouts.grid) < c(north.coord + 1, east.coord)) ||
            sum(cutouts.grid[south.coord:(north.coord + 1), west.coord:east.coord]) != 0 ) {
          # if the search runs grows bigger than maximum allowed size, goes into an existing flag or  
          # "goes off the map", then stop expanding in this direction
          expand.north <- FALSE
        } else {
        #  if ((north.coord - south.coord + 1) > 256) {browser()}
          north.coord <- north.coord + 1
          
          candidates.ls[[i.candidate]][[length(candidates.ls[[i.candidate]]) + 1]] <- 
            TownforgeR::tf_expand_search_flag(url.rpc, flag.bounds.ls, infl.grid.disagg, 
              west.coord, south.coord, east.coord, north.coord, init.lat, init.long, role.id, 
              economic.power, city, subject.to.influence, get.flag.cost, infl.thresholds)
        }
      }
      
      if (expand.east) {
        if (  (east.coord - west.coord + 2) > 256 ||
            any(dim(cutouts.grid) < c(north.coord, east.coord + 1)) ||
            sum(cutouts.grid[south.coord:north.coord, west.coord:(east.coord + 1)]) != 0 ) {
          expand.east <- FALSE
        } else {
         # if ((east.coord - west.coord + 1) > 256) {browser()}
          east.coord <- east.coord + 1
          
          candidates.ls[[i.candidate]][[length(candidates.ls[[i.candidate]]) + 1]] <- 
            TownforgeR::tf_expand_search_flag(url.rpc, flag.bounds.ls, infl.grid.disagg, 
              west.coord, south.coord, east.coord, north.coord, init.lat, init.long, role.id, 
              economic.power, city, subject.to.influence, get.flag.cost, infl.thresholds)
        }
      }
      
      if (expand.south) {
        if ( (north.coord - south.coord + 2) > 256 ||
            any( c(south.coord - 1, west.coord) <= 0 ) ||
            sum(cutouts.grid[(south.coord - 1):north.coord, west.coord:east.coord]) != 0 ) {
          # "Going off the map" for southern expansion means going zero or below
          expand.south <- FALSE
        } else {
         # if ((north.coord - south.coord + 1) > 256) {browser()}
          south.coord <- south.coord - 1
          
          candidates.ls[[i.candidate]][[length(candidates.ls[[i.candidate]]) + 1]] <- 
            TownforgeR::tf_expand_search_flag(url.rpc, flag.bounds.ls, infl.grid.disagg, 
              west.coord, south.coord, east.coord, north.coord, init.lat, init.long, role.id, 
              economic.power, city, subject.to.influence, get.flag.cost, infl.thresholds)
        }
      }
      
      if (expand.west) {
        if (  (east.coord - west.coord + 2) > 256 ||
            any( c(south.coord, west.coord - 1) <= 0 ) ||
            sum(cutouts.grid[south.coord:north.coord, (west.coord - 1):east.coord]) != 0 ) {
          expand.west <- FALSE
        } else {
          #if ((east.coord - west.coord + 1) > 256) {browser()}
          west.coord <- west.coord - 1
          
          candidates.ls[[i.candidate]][[length(candidates.ls[[i.candidate]]) + 1]] <- 
            TownforgeR::tf_expand_search_flag(url.rpc, flag.bounds.ls, infl.grid.disagg, 
              west.coord, south.coord, east.coord, north.coord, init.lat, init.long, role.id, 
              economic.power, city, subject.to.influence, get.flag.cost, infl.thresholds)
        }
      }
    }
    candidates.ls[[i.candidate]] <- candidates.ls[[i.candidate]][ lengths(candidates.ls[[i.candidate]]) > 0]
  }
  
  candidates.ls <- unlist(candidates.ls, recursive = FALSE, use.names = FALSE)
  
  candidates.df <- plyr::rbind.fill(candidates.ls)
  # NOTE: "base.production.type_" columns might not be in ascending order of item ID. 
  # That should not matter for later use
  
  candidates.df$area <- with(candidates.df, (y1 - y0 + 1) * (x1 - x0 + 1))
  candidates.df$flag.cost <- candidates.df$flag.cost / gold.unit.divisor
  
  #browser()
  
  for (i in grep("base.production.item_", colnames(candidates.df))) {
    
    item.num <- stringr::str_extract(colnames(candidates.df)[i], "[0-9]+")
    
    candidates.df[, paste0("total.production.item_", item.num)] <- 
      candidates.df[, i] * (1 + 0.05 * candidates.df$infl.boost)
    
    candidates.df[, paste0("total.production.per.area.item_", item.num)] <- 
      candidates.df[, paste0("total.production.item_", item.num)] / candidates.df$area
    
    candidates.df[, paste0("ROI.item_", item.num)] <- 
      candidates.df[, paste0("total.production.item_", item.num)] / candidates.df$flag.cost
  }
  
  list(candidates.df = candidates.df, flag.bounds.ls = flag.bounds.ls, 
    building.type = building.type, economic.power = economic.power, city = city)
}





#' tf_get_best_flag_map
#'
#' Calculates matrix of best flag(s) map
#'
#' @param url.rpc TODO
#' @param candidates.df TODO
#' @param chosen.item.id TODO
#' @param number.of.top.candidates TODO
#' @param display.perimeter TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_get_best_flag_map <-  function(url.rpc, candidates.df, chosen.item.id, 
  number.of.top.candidates, display.perimeter = TRUE) {
  
   #<- 256
   #<- 3
   #<- "WOR"
  # "http://127.0.0.1:28881/json_rpc"
  stopifnot(number.of.top.candidates <= length(c(LETTERS, letters)))
  # can't have more than 2*26 = 52 top candidates since letters are used for labels
  
  candidates.df <- do.call(rbind, by(candidates.df, list(candidates.df$init.lat, candidates.df$init.long), FUN = function(x) {
    x[which.max(x[, paste0("ROI.item_", chosen.item.id)]), , drop = FALSE]
  }) )
  
  candidates.df <- candidates.df[order(candidates.df[, paste0("ROI.item_", chosen.item.id)], decreasing = TRUE), ]
  candidates.df <- candidates.df[seq_len(min(c(number.of.top.candidates, nrow(candidates.df)))), ]
  
  flag.bounds.ls <- tf_flag_bounds(url.rpc, grid.dim = NULL, coords.origin = NULL)
  
  cutouts.grid <- flag.bounds.ls$bounds.grid
  
  for (i in seq_len(nrow(candidates.df))) {
    bounds.grid.tmp <- expand.grid(candidates.df[i, "y0"]:candidates.df[i, "y1"], candidates.df[i, "x0"]:candidates.df[i, "x1"])
    #bounds.grid.tmp <- bounds.grid.tmp[bounds.grid.tmp[, 1] <= grid.dim[1] & bounds.grid.tmp[, 2] <= grid.dim[2] &
    #    bounds.grid.tmp[, 1] > 0L & bounds.grid.tmp[, 2] > 0L,  ]
    # Trim to the grid.dim
    if (nrow(bounds.grid.tmp) == 0) {next}
    cutouts.grid <- cutouts.grid + 2 * Matrix::sparseMatrix(bounds.grid.tmp[, 1], bounds.grid.tmp[, 2], x = 1L, 
      dims = dim(cutouts.grid))
    
    if (display.perimeter) {
      perim.df <- rbind(
        data.frame(y = candidates.df[i, "y0"],                        x = candidates.df[i, "x0"]:candidates.df[i, "x1"]),
        data.frame(y = candidates.df[i, "y0"]:candidates.df[i, "y1"], x = candidates.df[i, "x0"]),
        data.frame(y = candidates.df[i, "y1"],                        x = candidates.df[i, "x0"]:candidates.df[i, "x1"]),
        data.frame(y = candidates.df[i, "y0"]:candidates.df[i, "y1"], x = candidates.df[i, "x1"]) )
      cutouts.grid <- cutouts.grid + 3 * Matrix::sparseMatrix(perim.df[, 1], perim.df[, 2], x = 1L, 
        dims = dim(cutouts.grid))
    }
    
  }
  
  if (display.perimeter) {cutouts.grid@x[cutouts.grid@x > 2]  <- 3 }
  
  label <- c(LETTERS, letters)[seq_len(nrow(candidates.df))]
  
  candidates.df <- cbind(data.frame(label, stringsAsFactors = FALSE), candidates.df)
  
  list(map.mat = cutouts.grid, candidates.df = candidates.df)
  
}



#' tf_expand_search_flag
#'
#' Expands a flag as tf_search_best_flags() searches
#'
#' @param url.rpc TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_expand_search_flag <- function(url.rpc, flag.bounds.ls, infl.grid.disagg, 
  west.coord, south.coord, east.coord, north.coord, init.lat, init.long, role.id, 
  economic.power, city, subject.to.influence, get.flag.cost, infl.thresholds) {
  
  base.production.ls <- TownforgeR::tf_rpc_curl(url.rpc,
    method ="cc_get_production",
    params = list(
      city = city, 
      x0 = flag.bounds.ls$coords.origin[["x"]] + west.coord,
      y0 = flag.bounds.ls$coords.origin[["y"]] + south.coord,
      x1 = flag.bounds.ls$coords.origin[["x"]] + east.coord,
      y1 = flag.bounds.ls$coords.origin[["y"]] + north.coord,
      role = role.id,
      economic_power = economic.power
    ), keep.trying.rpc = TRUE )$result$item_production
  
  base.production.ls <- sapply(base.production.ls, FUN = function(x) {
    ret <- list(x[["amount"]])
    names(ret) <- paste0("base.production.item_", x[["type"]])
    ret
  })
  
  if (subject.to.influence) {
    infl.boost <- sapply(infl.grid.disagg$infl.grid, FUN = function(x) {
      infl.coverage <- x[south.coord:north.coord, west.coord:east.coord]
      (-1) + as.numeric(cut(mean(infl.coverage), breaks = infl.thresholds, right = FALSE) )
      # NOTE: using as.numeric to coerce the factor
    })
  } else {
    infl.boost <- 0
  }
  
  infl.boost <- sum(infl.boost)
  
  if (get.flag.cost) {
    flag.cost <- TownforgeR::tf_rpc_curl(url.rpc,
      method ="cc_get_new_flag_cost",
      params = list(
        city = city, 
        x0 = flag.bounds.ls$coords.origin[["x"]] + west.coord,
        y0 = flag.bounds.ls$coords.origin[["y"]] + south.coord,
        x1 = flag.bounds.ls$coords.origin[["x"]] + east.coord,
        y1 = flag.bounds.ls$coords.origin[["y"]] + north.coord
      ), keep.trying.rpc = TRUE )$result$cost
  } else {
    flag.cost <- NULL
  }
  
  cand.ret <- list(y0 = south.coord, x0 = west.coord, y1 = north.coord, x1 = east.coord, 
    init.lat = init.lat, init.long = init.long,
    infl.boost = unname(infl.boost), flag.cost = flag.cost)
  
  as.data.frame(c(cand.ret, base.production.ls), stringsAsFactors = FALSE)
  
}


#' tf_buy_flags
#'
#' Buys flags
#'
#' @param url.wallet TODO
#' @param flags.to.buy.df TODO
#' @param coords.origin TODO
#' @param city TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_buy_flags <- function(url.wallet, flags.to.buy.df, coords.origin, city) {
  #browser()
  print(flags.to.buy.df)
  print(coords.origin)
  
  for (i in seq_len(nrow(flags.to.buy.df))) {
    TownforgeR::tf_rpc_curl(url.wallet,
      method ="cc_buy_land",
      params = list(
        city = city, 
        x0 = coords.origin[["x"]] + flags.to.buy.df$x0[i],
        y0 = coords.origin[["y"]] + flags.to.buy.df$y0[i],
        x1 = coords.origin[["x"]] + flags.to.buy.df$x1[i],
        y1 = coords.origin[["y"]] + flags.to.buy.df$y1[i]
      ), keep.trying.rpc = TRUE )
  }
}



#' tf_build_buildings
#'
#' Buys flags
#'
#' @param url.townforged TODO
#' @param url.wallet TODO
#' @param flags.to.buy.df TODO
#' @param building.type TODO
#' @param economic.power TODO
#' @param coords.origin TODO
#' @param city TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_build_buildings <- function(url.townforged, url.wallet, flags.to.buy.df, build.tx.hashes.v, building.type, economic.power, coords.origin, city) {
  #browser()
  
  print(flags.to.buy.df)
  print(coords.origin)
  
  flags.ret <- TownforgeR::tf_rpc_curl(url.rpc = url.townforged, method = "cc_get_flags" )$result$flags
  
  max.flag.id <- flags.ret[[length(flags.ret)]]$id
  
  coords.mat <- matrix(NA_real_, nrow = max.flag.id, ncol = 4, 
    dimnames = list(NULL, c("x0", "x1", "y0", "y1")) )
  
  for (i in 1:max.flag.id) {
    if (i == 21 & packageVersion("TownforgeR") == "0.0.15") { next }
    # far away flag in testnet
    ret <- TownforgeR::tf_rpc_curl(method = "cc_get_flag", params = list(id = i), 
      url.rpc = url.townforged, keep.trying.rpc = TRUE)
    if (any(names(ret) == "error") || ret$result$city > 0)  { next }
    coords.mat[i, "x0"] <- ret$result$x0
    coords.mat[i, "x1"] <- ret$result$x1
    coords.mat[i, "y0"] <- ret$result$y0
    coords.mat[i, "y1"] <- ret$result$y1
  }
  
  for (j in seq_len(nrow(flags.to.buy.df))) {
    
    chosen.flag.coords.v <- c(
      x0 = coords.origin[["x"]] + flags.to.buy.df$x0[j],
      y0 = coords.origin[["y"]] + flags.to.buy.df$y0[j],
      x1 = coords.origin[["x"]] + flags.to.buy.df$x1[j],
      y1 = coords.origin[["y"]] + flags.to.buy.df$y1[j]
    )
    
    chosen.flag.coords.mat <- matrix(rep(chosen.flag.coords.v[colnames(coords.mat)], 
      nrow(coords.mat)), ncol = 4, byrow = TRUE)
    
    chosen.flag.id <- which(rowSums(coords.mat == chosen.flag.coords.mat) == 4)
    # All 4 coordinates match
    
    if (length(chosen.flag.id) == 0) { next }
    
    chosen.flag.construction_height <- TownforgeR::tf_rpc_curl(method = "cc_get_flag", 
      params = list(id = chosen.flag.id), url.rpc = url.townforged, keep.trying.rpc = TRUE)$result$construction_height
    
    role.id <- building.names.df$id[building.names.df$abbrev == building.type]
    chosen.name <- ""
    
    tx.hash <- TownforgeR::tf_rpc_curl(url.rpc = url.wallet, method = "cc_building_settings", 
      params = list(flag = chosen.flag.id, role = role.id, economic_power = economic.power, 
        construction_height = chosen.flag.construction_height, name = chosen.name, keep.trying.rpc = TRUE))$result$tx_hash
    
    
    if (length(tx.hash) > 0) {
      flags.to.buy.df <- flags.to.buy.df[ (-1) * j, , drop = FALSE]
      build.tx.hashes.v <- c(build.tx.hashes.v, tx.hash)
    }
    
    
  }
  return(list(flags.to.buy.df = flags.to.buy.df, build.tx.hashes.v = build.tx.hashes.v) )
  # Return all the flags whose transactions were not accepted by the wallet daemon
  # So that they can be tried again.
}

