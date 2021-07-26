

#' tf_server_wallet_restore
#'
#' Create a wallet file from seed words via townforge-wallet-cli. Expected to be used on a server
#'
#' @param wallet.restore.username TODO
#' @param wallet.restore.password TODO
#' @param wallet.restore.seed.words TODO
#' @param wallet.restore.seed.offset.passphrase TODO
#' @param wallet.directory TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import stringr
tf_server_wallet_restore <- function(wallet.restore.username, wallet.restore.password, wallet.restore.seed.words, 
  wallet.restore.seed.offset.passphrase, wallet.directory) {
  
  stopifnot(gsub("[0-9A-Za-z]", "", wallet.restore.username) != 0) # stop if not alphanumeric
  stopifnot(nchar(wallet.restore.username) <= 30)
  wallet.files <- list.files(wallet.directory)
  stopifnot( ! any(wallet.restore.username %in% wallet.files)) # any() for safety in case somehow we get a vector
  
  wallet.restore.seed.words <- tolower(wallet.restore.seed.words)
  
  wallet.restore.seed.words <- stringr::str_extract_all(wallet.restore.seed.words, 
    paste0("(", paste0(seed.word.list.v, collapse = ")|("), ")") )[[1]]
  stopifnot(length(wallet.restore.seed.words) == 25)
  # seed.word.list.v is https://raw.githubusercontent.com/monero-project/monero/master/src/mnemonics/english.h
  
  wallet.restore.seed.words <- paste0(wallet.restore.seed.words, collapse = " ")
  
  system(paste0("townforge-wallet-cli --generate-new-wallet=", 
    wallet.directory, "/", wallet.restore.username, " --testnet --restore-deterministic-wallet"),
    wait = TRUE,
    input = c(
      wallet.restore.seed.words, # "Specify Electrum seed:"
      wallet.restore.seed.offset.passphrase, # "Enter seed offset passphrase, empty if none:"
      wallet.restore.password, # "Enter a new password for the wallet:"
      # For some reason when invoked from R, the CLI does not ask to confirm password, so only need this once
      "0", # "Restore from specific blockchain height (optional, default 0), or alternatively from specific date (YYYY-MM-DD):"
      "No", # "The daemon is not set up to background mine....Do you want to do it now? (Y/Yes/N/No):"
      "exit" # exit after sync
    ))
  # See also https://github.com/monero-project/monero/issues/3131
  # Hmm I guess I could have just done restore_deterministic_wallet in the wallet RPC instead of using input:
  # https://www.getmonero.org/resources/developer-guides/wallet-rpc.html#restore_deterministic_wallet
  
  return(invisible(NULL))
}




#' tf_server_wallet_load
#'
#' Load a wallet with new townforge-wallet-rpc instance
#'
#' @param wallet.username TODO
#' @param wallet.password TODO
#' @param wallet.directory TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import stringr
#' @import readr
tf_server_wallet_load <- function(wallet.username, wallet.password, wallet.directory) {
  
  stopifnot(gsub("[0-9A-Za-z]", "", wallet.username) != 0) # stop if not alphanumeric
  wallet.files <- list.files(wallet.directory)
  stopifnot( any(wallet.username %in% wallet.files)) # any() for safety in case somehow we get a vector
  
  wallet.rpc.password <- paste0(sample(c(LETTERS, letters), 10, replace = TRUE), collapse = TRUE)
  # TODO: Maybe have a more cryptographically secure RNG. This password is not critical, however
  
  ss.output <- system("ss -tunlp | grep townforge-wallet-rpc", intern = TRUE)
  # https://linuxize.com/post/check-listening-ports-linux/
  ss.output.df <- as.data.frame(readr::read_fwf(ss.output, readr::fwf_empty(ss.output, 
    col_names = c("Netid", "State", "Recv.Q", "Send.Q", "Local.Address.Port", "Peer.Address.Port", "Process"))))
  
  ports.listened.to <- gsub(".+:", "", ss.output.df$Local.Address.Port)
  candidate.ports <- 63079:(63079 + 100)
  candidate.ports <- setdiff(candidate.ports, ports.listened.to)
  stopifnot(length(candidate.ports) > 5) # Need at least safety margin of 5 ports available. TODO: re-consider these limits
  wallet.rpc.bind.port <- sample(candidate.ports, size = 1)
  # choose a random port from the set
  
  system(paste0("townforge-wallet-rpc --wallet-file ",
    wallet.directory, "/", wallet.username, " --testnet --daemon-port 28881 --prompt-for-password --rpc-bind-port ",
    wallet.rpc.bind.port, "--rpc-login TownforgeR:", wallet.rpc.password),
    input = "wallet.password"
  )
  
  list(wallet.rpc.bind.port = wallet.rpc.bind.port, wallet.rpc.password = wallet.rpc.password)
  
}

