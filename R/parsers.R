###
#
# Parsers and other useful functions for extracting, manipulating, and structuring data from RPC
#
###

#' tf_parse_accounts
#'
#' Parse existing user accounts and some key info into an R data.frame
#'
#' @param ids TODO
#' @param get_accounts TODO
#' @param get_account TODO
#' @param fields_plain TODO
#' @param fields_nests TODO
#' @param collapse_fun TODO
#' @param truncate_char TODO
#' @param url.rpc TODO
#' @param ... TODO
#'
#' @return A data.frame corresponding to queried plain and nested fields over desired ids
#'
#' @examples
#' tf_parse_accounts()
#'
#' @export
tf_parse_accounts <- function(
	# Custom vector of account ids to query data for
	ids, 
	# RPC call for querying a list of accounts
	get_accounts = 'cc_get_accounts', 
	# RPC call for querying a single account and its relevant data
	get_account  = 'cc_get_account',  
	# Vector of account parameters that contain only a single value to retrieve
	fields_plain = c('name','public_key','moose_killed','bears_killed','prestige','description','balance'),	
	# Vector of account parameters that contain nested lists and require further parsing
	fields_nests = c('flags','item_balances','badges'),	
	# A function that parses a nested list into a single character string
	collapse_fun = TownforgeR:::parseNest,
	# If collapsed_fun produces a character count above truncation limit, cut it and add '...' at the end
	truncate_char = 40,
  url.rpc = "http://127.0.0.1:18881/json_rpc",
	# Additional parameters
	...
){
	# No custom account id list, extracting all data
	if(missing(ids)){
		# First extract existing user account ids using 'cc_get_accounts'		
		ids <- unlist(lapply(TownforgeR::tf_rpc_curl(
		  url.rpc = url.rpc, method=get_accounts)$result$accounts, FUN=function(x) x$id))
	# Invalid class for custom ids
	}else if(!class(ids) %in% c("character", "numeric")){
		stop(paste("Invalid class in 'ids':", class(ids)))
	# Cast ids to numeric just in case
	}else{
		ids <- as.numeric(ids)
	}
	# l(ist)apply through the obtained metadata and row-bind them into a data.frame
	do.call("rbind", 
		# Loop over individual account ids and extract a row of data for each based on desired fields
		lapply(ids, FUN=function(id) {
			tmp <- TownforgeR::tf_rpc_curl(url.rpc = url.rpc, method=get_account, params=list("id"=id))$result
			tmp_plain <- as.data.frame(tmp[fields_plain])
			tmp_nests <- do.call("cbind", lapply(tmp[fields_nests], FUN=collapse_fun))
			names(tmp_nests) <- fields_nests
			tmp_nests <- as.data.frame(t(apply(tmp_nests, MARGIN=2, FUN=function(str) { ifelse(nchar(str) > truncate_char, paste0(substring(str, 1, truncate_char-3), "..."), str) })))
			colnames(tmp_nests) <- fields_nests
			cbind(id = id, tmp_plain, tmp_nests)
		})
	)
}

#' tf_parse_network
#'
#' Creates verbatim output string that describes Townforge network status
#' 
#' @param get_info TODO
#' @param url.rpc TODO
#'
#' @export
tf_parse_network <- function(
	# RPC command for getting Townforge network info
	get_info = 'get_info',
  url.rpc = "http://127.0.0.1:18881/json_rpc"
){
	# Line change
	cat("\n")
	info <- TownforgeR::tf_rpc_curl(url.rpc = url.rpc, method=get_info)$result
	# Omit id and jsonrpc
	info <- info[-c(1:2)]
	# Parse rest into suitable fields
	cat(paste(paste(names(info), ":", info), collapse="\n"))
	# Line change
	cat("\n")
}

#' tf_parse_items
#'
#' Description
#'
#' @export
tf_parse_items <- function(

){

}

#' tf_parse_markets
#'
#' Description
#'
#' @param get_order_book TODO
#' @param url.rpc TODO
#'
#' @export
tf_parse_markets <- function(
  # RPC command for extracting market orders in the tx pool
  get_order_book = "cc_get_order_book",
  url.rpc = "http://127.0.0.1:18881/json_rpc"
){
  markets <- TownforgeR::tf_rpc_curl(url.rpc = url.rpc, method=get_order_book, 
    params=list("bids"=TRUE, "offers"=TRUE), nonce.as.string = TRUE)$result
  markets <- do.call("rbind", 
    lapply(
      unlist(markets[c("bids", "offers")], recursive = FALSE),
      FUN = as.data.frame
    )
  )
  markets
}

#' tf_parse_nfts
#'
#' Description
#'
#' @param get_custom_items TODO
#' @param truncate_char TODO
#' @param url.rpc TODO
#'
#' @export
tf_parse_nfts <- function(
	# RPC command for extracting custom items
	get_custom_items = "cc_get_custom_items",
	# Character count to truncate strings to avoid extremely lengthy columns
	truncate_char = 80,
  url.rpc = "http://127.0.0.1:18881/json_rpc"
){
	nfts <- do.call("rbind", 
		lapply(
			TownforgeR::tf_rpc_curl(url.rpc = url.rpc, method=get_custom_items)$result$items, 
			FUN=function(z) { unlist(z)[c("id","name","creator","amount","creation_height","name","pdesc","gold","hash","ipfs_multihash","is_group")] }
		)
	)
	nfts <- as.data.frame(
		apply(
			nfts, MARGIN=2, 
			FUN=function(str) 
			{ 
				ifelse(nchar(str) > truncate_char, paste0(substring(str, 1, truncate_char-3), "..."), str) 
			}
		)
	)
	nfts	
}

#' tf_parse_nft_png
#'
#' Description
#'
#' @param item_hash TODO
#' @param storage_blocks TODO
#'
#' @export
tf_parse_nft_png <- function(
	item_hash,
	# Expected block storage count to save image at TF's IPFS rather than ipfs.io; after certain time query on ipfs.io, before try TF's ipfs
	# By default NFT data is contained for 2.5 days on TF's IPFS at the time of writing this
	storage_blocks = 2.5*24*60 
){
	png <- RCurl::getBinaryURL(paste0("https://ipfs.townforge.net/ipfs/", item_hash), httpheader = "image/png", ssl.verifypeer = FALSE)
	writeBin(png, con = tf <- tempfile(fileext = ".png"))
	shell.exec(tf)	
}


###
#
# R Shiny related parsers
#
###


#' tf_shiny_nft_png
#'
#' Description
#'
#' @param item_hash TODO
#'
#' @export
tf_shiny_nft_png <- function(
	item_hash = "",
	# Expected block storage count to save image at TF's IPFS rather than ipfs.io; after certain time query on ipfs.io, before try TF's ipfs
	# By default NFT data is contained for 2.5 days on TF's IPFS at the time of writing this
	storage_blocks = 2.5*24*60 
){
	paste0('<img src="https://ipfs.townforge.net/ipfs/',item_hash,'">')
}

#' tf_shiny_item_info
#'
#' Description
#'
#' @param item_hash TODO
#'
#' @export
tf_shiny_item_info <- function(
	item_hash
){
	
}

#' tf_parse_cities
#'
#' Description
#'
#' @param url.rpc TODO
#'
#' @export
tf_parse_cities <- function(url.rpc) {
  cities.df <- do.call(rbind,
    lapply(TownforgeR::tf_rpc_curl(url.rpc, 
      method ="cc_get_cities", params = list())$result$cities,
      as.data.frame) )
  cities.v <- cities.df$city_id
  names(cities.v) <- cities.df$name
  list(cities.df = cities.df, cities.v = cities.v)
}
