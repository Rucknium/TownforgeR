# Server definition for the TownforgeR web interface
# cheat-sheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

#' Shiny server side
#'
#' Description
serverTF <- function(input, output, session){
  
  waitress <- waiter::Waitress$new(theme = "overlay-percent", min = 0, max = 1)
  waiter <- waiter::Waiter$new()
  
  url.townforged <- shiny::getShinyOption("url.townforged", stop("townforged RPC URL not set!"))
  # Grabs url.rpc argument from shinyTF()
  # Thanks to https://stackoverflow.com/questions/49470474/saving-r-shiny-app-as-a-function-with-arguments-passed-to-the-shiny-app
  
  light <- bslib::bs_theme(bootswatch = "minty")
  dark <- bslib::bs_theme(bg = "black", fg = "white", primary = "purple")
  shiny::observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) {dark} else {light}
  ))
  # https://rstudio.github.io/bslib/articles/bslib.html#dynamic-theming
  
  
  session.vars <- shiny::reactiveValues(
    wallet_rpc_port = "",
    best.flag.candidates.ls = NULL,
    cities = TownforgeR::tf_parse_cities(url.townforged)
  )
  
  
  
  
  # Need to load select options on the run
  shiny::updateSelectInput(session, "item_inspect",
    choices = TownforgeR:::formatNFTs(url.rpc = url.townforged)
  )
  
  output$pars <- shiny::renderUI({
    if(input$command %in% c("cc_get_account")){
      shiny::textInput("id", "id", value = 0)
    }else if(input$command %in% c("cc_get_discoveries")){
      shiny::textInput("account", "account", value = 0)
    }else if(input$command %in% c("cc_get_shares","cc_get_temperature")){
      shiny::textInput("city", "city", value = 0)
    }else if(input$command %in% c("cc_find_flag")){
      shiny::textInput("city", "city", value = 0); shiny::textInput("x", "x", value = 0); shiny::textInput("y", "y", value = 0)
    }
  })
  output$verb <- shiny::renderPrint({
    TownforgeR::tf_rpc_curl(
      url.rpc = url.townforged,
      method = input$command,
      params = TownforgeR:::pruneList(
        list(
          id = as.numeric(input$id),
          account = as.numeric(input$account),
          city = as.numeric(input$city),
          x = as.numeric(input$x),
          y = as.numeric(input$y)
        )
      )
    )
  })
  output$accountsDT <- DT::renderDataTable({
    TownforgeR::tf_parse_accounts(url.rpc = url.townforged)
  })
  output$marketsDT <- DT::renderDataTable({
    TownforgeR::tf_parse_markets(url.rpc = url.townforged)
  })
  output$nftsDT <- DT::renderDataTable({
    TownforgeR::tf_parse_nfts(url.rpc = url.townforged)
  })
  output$network <- shiny::renderPrint({
    TownforgeR::tf_parse_network(url.rpc = url.townforged)
  })
  output$inspect_item <- shiny::renderText({
    items <- TownforgeR::tf_parse_nfts(url.rpc = url.townforged)
    items <- items[which(paste(items$id, ":", items$name) == input$item_inspect),]
    ret <- ifelse(items$ipfs_multihash=="",
      # Not an IPFS NFT, return ordinary HTML formatting
      #input$item_inspect,
      paste(paste(names(items), items, sep=": "), collapse="<br>"),
      # Try render an NFT representation in addition to just item information
      paste(paste(paste(names(items), items, sep=": "), collapse="<br>"),
        TownforgeR::tf_shiny_nft_png(items$ipfs_multihash), sep="<br><br>")
    )
    ret
  })
  
  
  
  shiny::observeEvent(input$port_submit_button, {
    
    session.vars$wallet_rpc_port <- input$port_wallet_rpc
    
    wallet_balance <- TownforgeR::tf_rpc_curl(url.rpc = paste0("http://127.0.0.1:", session.vars$wallet_rpc_port, "/json_rpc"),
      method ="get_balance")
    
    #print(wallet_balance)
    
    output$wallet_balance_text <-
      shiny::renderText(paste0( "Wallet balance: ", prettyNum(wallet_balance$result$balance / gold.unit.divisor, big.mark = ","),
        "<br>In-game account balance: ",  prettyNum(wallet_balance$result$cc_balance / gold.unit.divisor, big.mark = ",") ) )
    
    output$wallet_init_disappears <- shiny::reactive(TRUE)
    
  })
  
  shiny::observeEvent(input$wallet_pw_submit_button, {
    
    session.vars$wallet_rpc_port <- "63079"
    
    system_command <- paste0("\"", input$wallet_rpc_path, "\" --wallet-file \"", input$wallet_path,
      "\" --testnet --rpc-bind-port 63079 --daemon-port ", input$port_townforged,
      " --password \"", input$wallet_pw, "\" --disable-rpc-login" )
    
    system(system_command, wait = FALSE)
    
    shiny::withProgress(message = "Starting townforge-wallet-rpc...", Sys.sleep(10))
    # Wait for townforge-wallet-rpc to boot up
    
    wallet_balance <- TownforgeR::tf_rpc_curl(url.rpc = paste0("http://127.0.0.1:", session.vars$wallet_rpc_port, "/json_rpc"),
      method ="get_balance")
    
    output$wallet_balance_text <-
      shiny::renderText(paste0( "Total balance: ", prettyNum(wallet_balance$result$balance / gold.unit.divisor, big.mark = ","),
        "<br>In-game account balance: ",  prettyNum(wallet_balance$result$cc_balance / gold.unit.divisor, big.mark = ",") ) )
    
    output$wallet_init_disappears <- shiny::reactive(TRUE)
    
    
  })
  
  
  output$wallet_init_disappears <- shiny::reactive(FALSE)
  
  shiny::outputOptions(output, "wallet_init_disappears", suspendWhenHidden = FALSE)
  # See https://shinydata.wordpress.com/2015/02/02/a-few-things-i-learned-about-shiny-and-reactive-programming/
  
  shiny::observeEvent(input$deposit_submit_button, {
    
    deposit_result <- TownforgeR::tf_rpc_curl(url.rpc = paste0("http://127.0.0.1:", session.vars$wallet_rpc_port, "/json_rpc"),
      method = "cc_deposit", params = list(amount = input$deposit_amount * gold.unit.divisor))
    # FIXED: TODO: cc_deposit won't accept scientific notation, it seems, so "large" deposit amounts fail if not
    # formatted with formatC(). Need a general fix to this. maybe in tf_rpc_curl()
    
    
    output$deposit_tx_hash <- shiny::renderText(paste0("Transaction hash: ", deposit_result$result$tx_hash_list) )
    
  })
  
  shiny::observeEvent(input$depth_chart_button, {
    
    output$depth_chart <- shiny::renderPlot({
      
      order.book <- TownforgeR::tf_parse_markets(url.rpc = url.townforged)
      order.book <- order.book[order.book$id == 1, ]
      order.book$price <- order.book$price / gold.unit.divisor
      order.book <- order.book[order(order.book$price), ]
      bids.book <- order.book[order.book$bid, ]
      offers.book <- order.book[ ! order.book$bid, ]
      
      bids.book.steps <- sum(bids.book$amount) - c(0, cumsum(bids.book$amount))
      
      offers.book.steps <- c(0, cumsum(offers.book$amount))
      
      plot(0, 0,  type = "n", yaxs = "i",
        sub = paste0("Price spread: ", round(min(offers.book$price) - max(bids.book$price), digits = 5)),
        xlab = "Price", ylab = "Depth",
        main = "Sandstone order book",
        xlim = range(order.book$price),
        ylim = c(0, 1.1 * max(bids.book.steps, offers.book.steps)))
      
      plot(stepfun(bids.book$price, bids.book.steps, right = TRUE), col = "green", 
        lwd = 2, do.points = FALSE, add = TRUE)
      
      plot(stepfun(offers.book$price, offers.book.steps, right = TRUE), col = "red",
        lwd = 2, do.points = FALSE,  add = TRUE)
      
    })
  })
  
  shiny::observeEvent(input$map_button, {
    
    output$map_chart <- shiny::renderPlot({
      
      flags.ret <- TownforgeR::tf_rpc_curl(url.rpc = url.townforged, method = "cc_get_flags")$result$flags
      max.flag.id <- flags.ret[[length(flags.ret)]]$id
      
      coords.mat <- matrix(NA_real_, nrow = max.flag.id, ncol = 4, dimnames = list(NULL, c("x0", "x1", "y0", "y1")) )
      owner.id <- vector(mode = "numeric", length = max.flag.id)
      
      for (i in 1:max.flag.id) {
        if (i == 21 & packageVersion("TownforgeR") == "0.0.15") { next }
        # far away flag in testnet
        ret <- TownforgeR::tf_rpc_curl(url.rpc = url.townforged, method = "cc_get_flag", params = list(id = i))
        if (any(names(ret) == "error") || ret$result$city > 0)  { next }
        coords.mat[i, "x0"] <- ret$result$x0
        coords.mat[i, "x1"] <- ret$result$x1
        coords.mat[i, "y0"] <- ret$result$y0
        coords.mat[i, "y1"] <- ret$result$y1
        owner.id[i] <- ret$result$owner
      }
      
      owner.id <- owner.id[complete.cases(coords.mat)]
      coords.mat <- coords.mat[complete.cases(coords.mat), ]
      
      owner.df <- data.frame(owner.id = owner.id, owner.name = NA, stringsAsFactors = FALSE)
      owner.df <- unique(owner.df)
      
      for (i in unique(owner.id)) {
        ret <- TownforgeR::tf_rpc_curl(url.rpc = url.townforged, method = "cc_get_account", params = list(id = i))
        if (any(names(ret) == "error")) { next }
        owner.df$owner.name[owner.df$owner.id == i] <- ret$result$name
      }
      
      plot(0, 0, xlim = range(coords.mat[, c("x0", "x1")]), 
        ylim = range(coords.mat[, c("y0", "y1")]),
        main = "Flag map, by owner", asp = 1)
      rect(coords.mat[, "x0"], coords.mat[, "y0"], coords.mat[, "x1"], coords.mat[, "y1"], col = owner.id)
      legend("topright", legend = owner.df$owner.name, fill = owner.df$owner.id) #, horiz = TRUE)
      
    })
  })
  
  shiny::observeEvent(input$influence_button, {
    
    output$influence_chart <- shiny::renderPlot({
      shiny::withProgress(message = "Calculating influence...", {
        isolate(TownforgeR::tf_plot_influence(url.rpc = url.townforged, 
          input$building_type, input$effect_type, input$cut_out_flags) )
      })
    })
  })
  
  observe({
    shiny::updateSelectInput(session, "optimize_flag_chosen_item_id", 
      choices = commodity.id.key.v[commodity.id.key.v %in% 
          commodities.buildings.produce.df$commodity.id[ 
            commodities.buildings.produce.df$building.abbrev %in% input$optimize_flag_building_type] ])
  })
  
  observe({
    shiny::updateSelectInput(session, "optimize_flag_city", 
      choices = session.vars$cities$cities.v)
  })
  
  shiny::observeEvent(input$optimize_flag_button, {
    
    waitress$start(h3("Calculating flag production..."))
    
    # url <- "http://127.0.0.1:28881/json_rpc"
    chosen.item.id <- as.numeric(input$optimize_flag_chosen_item_id)
    number.of.top.candidates <- input$optimize_flag_number_of_top_candidates
    building.type <- input$optimize_flag_building_type
    economic.power <- as.numeric(input$optimize_flag_economic_power)
    city <- as.numeric(input$optimize_flag_city)
    # "http://127.0.0.1:28881/json_rpc"
    print(building.type)
    print(economic.power)
    print(number.of.top.candidates)
    print(chosen.item.id)
    
    session.vars$best.flag.candidates.ls <- shiny::withProgress(message = "Searching for best flag placements...", {
      TownforgeR::tf_search_best_flags(url.rpc = url.townforged, 
        building.type = building.type, economic.power = economic.power, 
        get.flag.cost = TRUE, city = city, grid.density.params = rep(input$optimize_flag_grid_density, 2), 
        in.shiny = TRUE, waitress = waitress)
    })
    
    candidates.df <- session.vars$best.flag.candidates.ls$candidates.df
    
    #print(str(candidates.df))
    
    best.flag.map.ls <- TownforgeR::tf_get_best_flag_map(url.rpc = url.townforged, 
      candidates.df, chosen.item.id, number.of.top.candidates, display.perimeter = TRUE)
    
    session.vars$best.flag.candidates.ls$candidates.df <- best.flag.map.ls$candidates.df
    
    #cat("\n mmmmmmmm \n")
    #print(str(best.flag.map.ls))
    
    output$optimize_flag_chart <- shiny::renderPlot({
      
      best.flag.map.mat <- as.matrix(Matrix::t(best.flag.map.ls$map.mat))
      best.flag.map.mat[best.flag.map.mat == 0] <- NA
      best.flag.map.mat.dim <- dim(best.flag.map.mat)
      # See for why must transpose:
      # https://stackoverflow.com/a/66453734
      
      par(mar = c(1, 1, 1, 1) + 0.1)
      # c(bottom, left, top, right)
      
      image(best.flag.map.mat, 
        col = c(1, 2, 3),
        xlim = c(0, max(best.flag.map.mat.dim)/best.flag.map.mat.dim[1]),
        ylim = c(0, max(best.flag.map.mat.dim)/best.flag.map.mat.dim[2]),
        axes = F,
        useRaster = TRUE)
      
      text(
        best.flag.map.ls$candidates.df$x0/best.flag.map.mat.dim[1], 
        best.flag.map.ls$candidates.df$y0/best.flag.map.mat.dim[2], 
        labels = c(LETTERS, letters)[seq_len(nrow(best.flag.map.ls$candidates.df))],
        cex = 2, xpd = NA) # font = 2 is bold font
      
      par(mar = c(5, 4, 4, 2) + 0.1)
      # Back to default, just in case
      
    })
    
    output$optimize_flag_table <- DT::renderDataTable({
      best.flag.map.ls$candidates.df},
      rownames = FALSE,
      extensions = c("Buttons", "ColReorder"), 
      options = list(dom = "Bfrtip", buttons = I("colvis"), colReorder = list(realtime = FALSE)) )
    # https://rstudio.github.io/DT/extensions.html
    
    # https://shiny.rstudio.com/articles/dynamic-ui.html
    output$optimize_flag_buy_flag_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::checkboxGroupInput("optimize_flag_buy_flag_input", "Choose flag(s) to buy and build upon", 
          choices = best.flag.map.ls$candidates.df$label, inline = TRUE),
        shiny::actionButton("buy_optimized_flag_button", "Buy and build selected flag(s)"),
        shiny::verbatimTextOutput("optimize_flag_buy_tx_hash")
      )
    })
    
    waitress$close() 
    
  })
  
  
  shiny::observeEvent(input$buy_optimized_flag_button, {
    
    if (session.vars$wallet_rpc_port == "") {
      stop("TownforgeR not connected to wallet.")
    }
    
    waiter$show()
    
    #  session.vars$best.flag.candidates.ls$candidates.df
    #  session.vars$best.flag.candidates.ls$flag.bounds.ls$coords.origin
    # list(candidates.df = candidates.df, flag.bounds.ls = flag.bounds.ls)
    cat("\n\nBUYING...\n\n")
    
    
    flags.to.buy.df <- session.vars$best.flag.candidates.ls$candidates.df
    print(flags.to.buy.df$label)
    print(input$optimize_flag_buy_flag_input)
    flags.to.buy.df <- flags.to.buy.df[flags.to.buy.df$label %in% input$optimize_flag_buy_flag_input, , drop = FALSE]
    print(flags.to.buy.df)
    # stop()
    
    TownforgeR::tf_buy_flags(url.wallet = paste0("http://127.0.0.1:", session.vars$wallet_rpc_port, "/json_rpc"), 
      flags.to.buy.df = flags.to.buy.df, 
      coords.origin = session.vars$best.flag.candidates.ls$flag.bounds.ls$coords.origin,
      city = session.vars$best.flag.candidates.ls$city)
    cat("\n\nBOUGHT...?\n\n")
    
    build.tx.hashes.v <- c()
    
    while(nrow(flags.to.buy.df) > 0) {
      # print(flags.to.buy.df)
      #browser()
      tf.build.buildings.ret <- TownforgeR::tf_build_buildings(url.townforged = url.townforged, 
        url.wallet = paste0("http://127.0.0.1:", session.vars$wallet_rpc_port, "/json_rpc"),
        flags.to.buy.df = flags.to.buy.df, 
        build.tx.hashes.v = build.tx.hashes.v, 
        building.type = session.vars$best.flag.candidates.ls$building.type, 
        economic.power = session.vars$best.flag.candidates.ls$economic.power, 
        coords.origin = session.vars$best.flag.candidates.ls$flag.bounds.ls$coords.origin,
        city = session.vars$best.flag.candidates.ls$city)
      
      flags.to.buy.df <- tf.build.buildings.ret$flags.to.buy.df
      build.tx.hashes.v <- tf.build.buildings.ret$build.tx.hashes.v
      
      Sys.sleep(10)
    }
    
    output$optimize_flag_buy_tx_hash <- shiny::renderText( 
      c("Sucessfully purchased flag(s) and built building(s)!",
      paste0("Building transaction hash: https://explorer.townforge.net/tx/", build.tx.hashes.v) )
    )
      
      waiter$hide()
      
  })
    
}
