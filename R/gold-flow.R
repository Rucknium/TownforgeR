
#' tf_gold_flow
#'
#' Network visualization of gold flow
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
#' @import data.table
#' @import visNetwork
tf_gold_flow <- function(url.rpc) {
  
  events <- TownforgeR::tf_rpc_curl(url.rpc = url.rpc, method = "cc_get_game_events", 
    params = list(min_height = 720 * 4 * 42, max_height = 720 * 4 * 49),
    keep.trying.rpc = TRUE)$result$events
  
  event <- sapply(events, FUN = function(x) {y <- x$event; ifelse(length(y) > 0, y, NA)})
  account <- sapply(events, FUN = function(x) {y <- x$account; ifelse(length(y) > 0, y, NA)})
  flag <- sapply(events, FUN = function(x) {y <- x$flag; ifelse(length(y) > 0, y, NA)})
  balance <- sapply(events, FUN = function(x) {y <- x$balance; ifelse(length(y) > 0, y, NA)})
  cmd <- sapply(events, FUN = function(x) {y <- x$cmd; ifelse(length(y) > 0, y, NA)})
  
  events <- data.table::data.table(event, account, flag, balance, cmd)
  
  accounts <- data.table::rbindlist(TownforgeR::tf_rpc_curl(url.rpc = url.rpc,
    method = "cc_get_accounts", params = list(), keep.trying.rpc = TRUE)$result$accounts)
  
  flag.ids <- unique(na.omit(flag))
  flag.names <- vector("integer", length(flag.ids))
  flag.cities <- vector("integer", length(flag.ids))
  
  for (i in seq_along(flag.ids)) {
    flag.info <- TownforgeR::tf_rpc_curl(url.rpc = url.rpc, method = "cc_get_flag",
      params = list(id = flag.ids[i]), keep.trying.rpc = TRUE)$result
    if (length(flag.info) < 1) {next}
    flag.cities[i] <- flag.info$city
    flag.names[i] <- flag.info$name
  }
  
  
  flags <- data.table(id = flag.ids, name = flag.names, city = flag.cities)
  
  cities <- data.table::rbindlist(TownforgeR::tf_rpc_curl(url.rpc = url.rpc,
    method = "cc_get_cities", params = list(), keep.trying.rpc = TRUE)$result$cities)
  
  setnames(cities, "treasury", "id")
  
  treasuries <- accounts[pkey == "0000000000000000000000000000000000000000000000000000000000000000" &
      grepl("treasury", name), ]
  
  treasuries <- merge(treasuries, cities[, .(city_id, id)])
  
  payouts <- events[grepl(" payout ", event) & cmd == 6 & (! is.na(flag)), ]
  taxes <- events[event == "Paid land tax" & cmd == 6 & (! is.na(flag)), ]
  subsidies <- events[grepl("subsidy", event) & cmd == 6 & is.na(flag) & account %in% treasuries$id, ]
  land.purchases <- events[grepl("Bought", event) & cmd == 3 & (! is.na(flag)), ]

  
  payouts.cities <- flags$city[match(payouts$flag, flags$id)]
  
  payouts <- data.table(edge.type = "payout",
    origin.vertex.type = "treasury", destination.vertex.type = "player",
    origin.sub.vertex.type = "treasury", destination.sub.vertex.type = "flag",
    origin.vertex.id = treasuries$id[match(payouts.cities, treasuries$city_id)],
    origin.vertex.name = treasuries$name[match(payouts.cities, treasuries$city_id)],
    origin.sub.vertex.id = treasuries$id[match(payouts.cities, treasuries$city_id)],
    origin.sub.vertex.name = treasuries$name[match(payouts.cities, treasuries$city_id)],
    destination.vertex.id = payouts$account,
    destination.vertex.name = accounts$name[match(payouts$account, accounts$id)],
    destination.sub.vertex.id = payouts$flag,
    destination.sub.vertex.name = flags$id[match(payouts$flag, flags$id)],
    gold = abs(payouts$balance))
  
  taxes.cities <- flags$city[match(taxes$flag, flags$id)]
  
  taxes <- data.table(edge.type = "tax",
    origin.vertex.type = "player", destination.vertex.type = "treasury",
    origin.sub.vertex.type = "flag", destination.sub.vertex.type = "treasury",
    origin.vertex.id = taxes$account,
    origin.vertex.name = accounts$name[match(taxes$account, accounts$id)],
    origin.sub.vertex.id = taxes$flag,
    origin.sub.vertex.name = flags$id[match(taxes$flag, flags$id)],
    destination.vertex.id = treasuries$id[match(taxes.cities, treasuries$city_id)],
    destination.vertex.name = treasuries$name[match(taxes.cities, treasuries$city_id)],
    destination.sub.vertex.id = treasuries$id[match(taxes.cities, treasuries$city_id)],
    destination.sub.vertex.name = treasuries$name[match(taxes.cities, treasuries$city_id)],
    gold = abs(taxes$balance))
  
  
  subsidies <- data.table(edge.type = "subsidy",
    origin.vertex.type = "coinbase", destination.vertex.type = "treasury",
    origin.sub.vertex.type = "coinbase", destination.sub.vertex.type = "treasury",
    origin.vertex.id = 9999999,
    origin.vertex.name = "coinbase",
    origin.sub.vertex.id = 9999999,
    origin.sub.vertex.name = "coinbase",
    destination.vertex.id = subsidies$account,
    destination.vertex.name = treasuries$name[match(subsidies$account, treasuries$id)],
    destination.sub.vertex.id = subsidies$account,
    destination.sub.vertex.name = treasuries$name[match(subsidies$account, treasuries$id)],
    gold = abs(subsidies$balance))
  
  
  land.purchases.cities <- flags$city[match(land.purchases$flag, flags$id)]
  
  land.purchases <- data.table(edge.type = "land_purchase",
    origin.vertex.type = "player", destination.vertex.type = "treasury",
    origin.sub.vertex.type = "flag", destination.sub.vertex.type = "treasury",
    origin.vertex.id = land.purchases$account,
    origin.vertex.name = accounts$name[match(land.purchases$account, accounts$id)],
    origin.sub.vertex.id = land.purchases$flag,
    origin.sub.vertex.name = flags$id[match(land.purchases$flag, flags$id)],
    destination.vertex.id = treasuries$id[match(land.purchases.cities, treasuries$city_id)],
    destination.vertex.name = treasuries$name[match(land.purchases.cities, treasuries$city_id)],
    destination.sub.vertex.id = treasuries$id[match(land.purchases.cities, treasuries$city_id)],
    destination.sub.vertex.name = treasuries$name[match(land.purchases.cities, treasuries$city_id)],
    gold = abs(land.purchases$balance))
  
  
  all.events <- rbind(payouts, taxes, subsidies, land.purchases)
  
  
  nodes <- data.frame(
    id = unique(c(all.events$origin.vertex.id, all.events$destination.vertex.id)),
    label = unique(c(all.events$origin.vertex.name, all.events$destination.vertex.name)))
  
  nodes$icon.code[nodes$id %in% treasuries$id] <- "f2af" # f15a"
  nodes$icon.code[nodes$id == 9999999] <- "f26f" # f0ed"
  nodes$icon.code[is.na(nodes$icon.code)] <- "f213" #f007"
  # https://astronautweb.co/snippet/font-awesome/
  
  # https://ionic.io/ionicons
  # or:
  # https://fontawesome.com/
  
  nodes$shape <- "icon"
  nodes$icon.face <- "Ionicons"
  nodes$icon.color <- "black"
  nodes$physics <- FALSE
  
  
  edges <- all.events[, 
    .(gold = sum(gold)/1e+11, arrows = "to", color = ""),
    by = .(origin.vertex.id, destination.vertex.id, edge.type)] # , origin.sub.vertex.id
  edges[edge.type == "payout", color := "green"]
  edges[edge.type == "tax", color := "red"]
  edges[edge.type == "subsidy", color := "gold"]
  edges[edge.type == "land_purchase", color := "blue"]
  edges$edge.type <- NULL
  setnames(edges, c("from", "to", "value", "arrows", "color"))
  
  
  visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = list(from = 1, to = 1), 
      algorithm = "hierarchical", hover = TRUE)) %>% 
    visNetwork::addIonicons()
  
}
