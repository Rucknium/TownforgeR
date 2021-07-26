#' TownforgeR function for curl'ing RPC calls
#'
#' Description
#'
#' @param url.rpc TODO
#' @param method One of: '
#' @param	params TODO
#' @param	num.as.string TODO
#' @param	nonce.as.string TODO
#' @param	verbatim.replace TODO
#' @param keep.trying.rpc TODO
#' @param ... TODO
#'
#' @details A list of RPC commands compatible with Townforge (v0.27.0.7 at the time of writing this) is available via Python code in https://git.townforge.net/townforge/townforge/src/branch/cc/utils/python-rpc/framework/daemon.py
#'
#' @export
tf_rpc_curl <- function(
	# URL for TF RPC connection
  url.rpc = "http://127.0.0.1:18881/json_rpc", 
	# Premade JSON template; method and params
	# From daemon.py functions:
	# - getblocktemplate(self, address, prev_block = "", game_account_key = "", client = ""):
	# - add_aux_pow(self, blocktemplate_blob, aux_pow, client = ""):
	# - send_raw_transaction(self, tx_as_hex, do_not_relay = False, do_sanity_checks = True, client = ""):
	# - submitblock(self, block):
	# - getblock(self, hash = '', height = 0, fill_pow_hash = False, include_blob = True, include_json = True, client = ""):
	# - getlastblockheader(self, client = ""):
	# - getblockheaderbyhash(self, hash = "", hashes = [], client = ""):
	# - getblockheaderbyheight(self, height, client = ""): 
	# - getblockheadersrange(self, start_height, end_height, fill_pow_hash = False, client = ""):
	# - get_connections(self, client = ""):
	# - get_info(self, client = ""):
	# - hard_fork_info(self, client = ""):
	# - generateblocks(self, address, blocks=1, prev_block = "", starting_nonce = 0):
	# - get_height(self, client = ""):
	# - pop_blocks(self, nblocks = 1):
	# - start_mining(self, miner_address, threads_count = 0, do_background_mining = False, ignore_battery = False):
	# - stop_mining(self):
	# - mining_status(self):
	# - get_transaction_pool(self, client = ""):
	# - get_transaction_pool_hashes(self, client = ""):
	# - get_transaction_pool_stats(self, client = ""):
	# - flush_txpool(self, txids = []):
	# - get_version(self):
	# - get_bans(self):
	# - set_bans(self, bans = []):
	# - banned(self, address = ''):
	# - set_bootstrap_daemon(self, address, username = '', password = ''):
	# - get_public_nodes(self, gray = False, white = True):
	# - get_transactions(self, txs_hashes = [], decode_as_json = False, prune = False, split = False, get_cc_data = False, client = ""):
	# - get_outs(self, outputs = [], get_txid = False, client = ""):
	# - get_coinbase_tx_sum(self, height, count, client = ""):
	# - get_output_distribution(self, amounts = [], from_height = 0, to_height = 0, cumulative = False, binary = False, compress = False, client = ""):
	# - get_output_histogram(self, amounts = [], min_count = 0, max_count = 0, unlocked = False, recent_cutoff = 0, client = ""):
	# - set_log_level(self, level):
	# - set_log_categories(self, categories = ''):
	# - get_alt_blocks_hashes(self, client = ""):
	# - get_alternate_chains(self, client = ""):
	# - get_fee_estimate(self, grace_blocks = 0):
	# - is_key_image_spent(self, key_images = [], client = ""):
	# - save_bc(self):
	# - get_peer_list(self):
	# - set_log_hash_rate(self, visible):
	# - stop_daemon(self):
	# - get_net_stats(self):
	# - get_limit(self):
	# - set_limit(self, limit_down, limit_up):
	# - out_peers(self, out_peers):
	# - in_peers(self, in_peers):
	# - update(self, command, path = None):
	# - get_block_count(self):
	# - get_block_hash(self, height):
	# - relay_tx(self, txids = [], client = ""):
	# - sync_info(self, client = ""):
	# - get_txpool_backlog(self, client = ""):
	# - prune_blockchain(self, check = False):
	# - flush_cache(self, bad_txs = False):
	# - sync_txpool(self):
	# - rpc_access_info(self, client):
	# - rpc_access_submit_nonce(self, client, nonce, cookie):
	# - rpc_access_pay(self, client, paying_for, payment):
	# - rpc_access_tracking(self, clear = False):
	# - rpc_access_data(self):
	# - rpc_access_account(self, client, delta_balance = 0):
	## Townforge specific commands (legacy prefix "Crypto City" ~ CC)	
	# - cc_lookup_account(self, address):
	# - cc_get_account(self, id):
	# - cc_get_city(self, id):
	# - cc_get_flag(self, id, get_packed_tiles = False, get_unpacked_tiles = False):
	# - cc_find_flag(self, city, x, y):
	# - cc_get_new_flag_cost(self, city, x0, y0, x1, y1):
	# - cc_get_order_book(self, bids, offers, type = [], id = [], calculate_matchable = True):
	# - cc_get_nonces_mined(self, nonces):
	# - cc_get_shares(self, city = 0):
	# - cc_get_last_update_events(self, height = None):
	# - cc_get_game_events(self, min_height = 0, max_height = 0, account = 0, flag = 0, item = 0, cmd = 0):
	# - cc_get_discoveries(self, account = 0):
	# - cc_get_cities(self):
	# - cc_get_special_events(self, city, all = False):
	# - cc_get_custom_items(self, ids = []):
	# - cc_get_accounts(self):
	# - cc_get_flags(self):
	# - cc_get_badge(self, id):
	# - cc_get_badge_totals(self):
	# - cc_get_attributes(self):
	# - cc_get_bonuses(self, accounts, research = False, firefighting = False):
	# - cc_get_level_increases(self):
	# - cc_is_invitation_used(self, invitation):
	# - cc_get_flag_resizing_cost(self, city, x0_0, y0_0, x1_0, y1_0, x0_1, y0_1, x1_1, y1_1):
	# - cc_get_service_fee(self, service_price, x0, y0, x1, y1):
	# - cc_get_new_city_cost(self)
	# - cc_are_discoveries_enabled(self, account, discovery):
	# - cc_get_item_count(self, id):
	# - cc_get_calendar(self, height = None):
	# - cc_get_temperature(self, city = 0):
	# - cc_is_nonce_used(self, nonce):
	# - cc_get_used_nonces(self):
	# - cc_get_new_nonces(self, num_nonces):
	# - cc_get_scripts(self, first_script = 0, last_script = 0xffffffff, include_blob = False, include_requirements = False, account = 0, owner = 0, city = 0)
	# - cc_get_script(self, script):
	# - cc_get_script_state(self, account, script, state, city):
	# - cc_get_script_variables(self):
	# - cc_get_script_variable(self, name):
	# - cc_get_foreclosures(self):
	# - cc_get_runestones(self, locations):
	# - cc_get_blob(self, hash):
	# - cc_get_blob_info(self, hashes):
	# - cc_get_blobs(self):
	# - cc_get_auctions(self):
	# - cc_get_stats(self):
	method = "get_block_count",
	params = list(),
  userpwd = "",
  num.as.string = FALSE,
  nonce.as.string = FALSE,
  verbatim.replace = NULL,
  keep.trying.rpc = FALSE,
	# Additional parameters
	...
){
  
  if (length(verbatim.replace) > 0 ) {
    
    json.ret <- RJSONIO::toJSON(
      list(
        jsonrpc = "2.0", 
        id = "0", 
        method = method,
        params = params
      ), digits = 50 # fixed: https://stackoverflow.com/questions/10820638/disable-scientific-notation-when-converting-to-json
    )
    
    for ( i in seq_along(verbatim.replace)) {
      json.ret <- sub(paste0("\"<VERBATIMREPLACE", i, ">\""), verbatim.replace[i],  json.ret, fixed = TRUE)
    }
    
  } else {
    
    json.ret <- RJSONIO::toJSON(
      list(
        jsonrpc = "2.0", 
        id = "0", 
        method = method,
        params = params
      ), digits = 50
    )
  }
  
  rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
    .opts = list(
      userpwd = userpwd,
      postfields = json.ret,
      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
      # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
    )
  ), error = function(e) {NULL})
  
  if (keep.trying.rpc && length(rcp.ret) == 0) {
    while (length(rcp.ret) == 0) {
      rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
        .opts = list(
          userpwd = userpwd,
          postfields = json.ret,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
          # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
        )
      ), error = function(e) {NULL})
    }
  }
  
  if (is.null(rcp.ret)) {
    stop("Cannot connect to townforged. Is townforged running?")
  }
  
  if (num.as.string) {
    rcp.ret <- gsub("(: )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }
  
  if (nonce.as.string & ! num.as.string) {
    rcp.ret <- gsub("(\"nonce\": )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }
  
	RJSONIO::fromJSON(rcp.ret) # , simplify = FALSE
}
