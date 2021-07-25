# UI definition for the TownforgeR web interface
# cheat-sheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

#' Shiny UI for browser
#'
#' Description

load("R/sysdata.rda")
# A little hacky, but it's the only way I can get package to build.

uiTF <- shiny::navbarPage(paste("TownforgeR", gsub("`|´", "", packageVersion("TownforgeR"))),
  theme = bslib::bs_theme(bootswatch = "minty"),
  # For more info on themes, see https://shiny.rstudio.com/articles/themes.html 
  # Raw command panel
  shiny::tabPanel("Raw commands",
    shiny::sidebarLayout(
      #textInput("method", "Selected TF RPC method name"),
      shiny::sidebarPanel(
        shiny::checkboxInput("dark_mode", "Dark mode"),
        shiny::selectInput("command", label="Select a TF RPC command",
          choices = c(
            "get_block_count", # Get current blockchain height
            #"mining_status", # Get daemon mining status
            "cc_get_account",
            "cc_get_accounts",
            "cc_get_cities",
            "cc_get_flag",
            "cc_get_flags",
            "cc_get_shares",
            "cc_get_last_update_events",
            "cc_get_discoveries",
            "cc_get_temperature",
            "cc_find_flag"
          )
        ),
        shiny::uiOutput("pars")
      ),
      shiny::mainPanel(
        shiny::verbatimTextOutput("verb")
      )
    )
  ),
  # Accounts panel
  shiny::tabPanel("Accounts",
    DT::dataTableOutput("accountsDT")
  ),
  # Markets panel
  shiny::tabPanel("Markets",
    DT::dataTableOutput("marketsDT")
  ),
  # NFTs panel
  shiny::tabPanel("NFTs",
    DT::dataTableOutput("nftsDT")
  ),
  # Network panel
  shiny::tabPanel("Network",
    shiny::verbatimTextOutput("network")
  ),
  # Inspect panel
  shiny::tabPanel("Inspect",
    shiny::tabsetPanel(type = "tabs",
      shiny::tabPanel("NFTs",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::selectInput("item_inspect",
              label = "Select item",
              choices = c("")
              #choices = TownforgeR:::formatNFTs()
            )
          ),
          shiny::mainPanel(
            shiny::htmlOutput("inspect_item")
          )
        )
      ),
      shiny::tabPanel("Blocks",
        "foo"
      )
    )
  ),
  shiny::tabPanel("Wallet",
    shiny::fluidRow(
      shiny::column(12,
        shiny::conditionalPanel(
          condition = "output.wallet_init_disappears != true",
          shiny::h2("WARNING: Use of wallet operations may be insecure. Code has not been reviewed for security issues "),
          shiny::br(),
          shiny::h5("To use wallet operations, townforge-wallet-rpc must be started. Shiny can start it or you can start it yourself. RPC login must be disabled."),
          shiny::h5("A suggested way to start townforge-wallet-rpc is:"),
          shiny::h5("/file/path/to/townforge-wallet-rpc --wallet-file /file/path/to/wallet.keys --testnet --rpc-bind-port <desired port for townforge-wallet-rpc (enter in form below)> --daemon-port <townforged port> --prompt-for-password --disable-rpc-login"),
          
          shiny::radioButtons("wallet_startup_choice",
            "How do you want to use the wallet operations?",
            c("I have already started townforge-wallet-rpc myself" = "port",
              "I want Shiny to start townforge-wallet-rpc for me" = "password"),
            selected = character(0), width = "100%"),
          shiny::conditionalPanel(
            condition = "input.wallet_startup_choice == 'port'",
            shiny::textInput(
              "port_wallet_rpc", label = "Enter port number of townforge-wallet-rpc", value = "", width = NULL,
              placeholder = "e.g. 63079"
            ),
            shiny::actionButton("port_submit_button", "Submit")
          ),
          shiny::conditionalPanel(
            condition = "input.wallet_startup_choice == 'password'",
            shiny::textInput(
              "wallet_rpc_path", label = "File path of townforge-wallet-rpc", value = "", width = NULL,
              placeholder = "/file/path/to/townforge-wallet-rpc"
            ),
            shiny::textInput(
              "port_townforged", label = "Enter port number of townforged", value = "", width = NULL,
              placeholder = "e.g. 18881"
            ),
            shiny::textInput(
              "wallet_path", label = "File path of wallet file", value = "", width = NULL,
              placeholder = "/file/path/to/wallet.keys"
            ),
            shiny::textInput(
              "wallet_pw", label = "Wallet password", value = "", width = NULL,
              placeholder = "Leave blank if your wallet has no password"
            ),
            shiny::actionButton("wallet_pw_submit_button", "Submit")
          )
        ),
        
        shiny::conditionalPanel(
          condition = "output.wallet_init_disappears == true",
          shiny::h4("Deposit to your game account"),
          shiny::br(),
          shiny::htmlOutput("wallet_balance_text"),
          shiny::br(),
          shiny::numericInput("deposit_amount", label = "Deposit amount:", value = 0, min = 0),
          
          shiny::actionButton("deposit_submit_button", "Submit"),
          
          shiny::verbatimTextOutput("deposit_tx_hash")
        )
      )
    )
  ),
  shiny::tabPanel("Order Book",
    shiny::actionButton("depth_chart_button", "Show sandstone order depth chart"),
    shiny::plotOutput("depth_chart")
  ),
  shiny::tabPanel("Map",
    shiny::actionButton("map_button", "Show map"),
    shiny::plotOutput("map_chart", height = "1000px")
  ),
  shiny::tabPanel("Influence", # building.type, effect.type, cut.out.flags 
    shiny::h5("For more info, see the Influence section of https://townforge.net/manual/"),
    shiny::selectInput("building_type", "Building type influenced:", 
      choices = building.names.v),
    shiny::selectInput("effect_type", "Effect:", 
      choices = c(bonus = "bonus", need = "need", penalty = "penalty")),
    shiny::selectInput("cut_out_flags", "Cut out existing flags:", 
      choices = c(Yes = TRUE, No = FALSE)),
    shiny::actionButton("influence_button", "Show map"),
    shiny::plotOutput("influence_chart", height = "1000px")
  ),
  shiny::tabPanel("Optimize Flag", # building.type, effect.type, cut.out.flags 
    waiter::use_waitress(),
    waiter::use_waiter(),
    shiny::selectInput("optimize_flag_building_type", "Building type:", 
      choices = building.names.v[building.names.v %in% 
          commodities.buildings.produce.df$building.abbrev[! is.na(commodities.buildings.produce.df$building.abbrev)] ] ),
    shiny::selectInput("optimize_flag_chosen_item_id", "Target commodity to maximize return on investment (ROI):", 
      choices = NULL),  
    shiny::sliderInput("optimize_flag_economic_power", "Intended economic power of building:",
      min = 100, max = 300, value = 100, step = 10),
      # TODO: Is it increments of 10 from 100 to 300?
    #shiny::selectInput("optimize_flag_economic_power", "Intended economic power of building:", 
    #  choices = seq(100, 300, by = 10)), # TODO: Is it increments of 10 from 100 to 300?
    shiny::sliderInput("optimize_flag_number_of_top_candidates", "Number of top candidate flags to display:",
      min = 1, max = length(c(LETTERS, letters)), value = 5, step = 1),
    shiny::selectInput("optimize_flag_city", "Town:", choices = NULL),
    shiny::sliderInput("optimize_flag_grid_density", "Density of grid search points (3 = about 5 minutes search time; 10 ≈ 30 minutes):",
      min = 2, max = 10, value = 3, step = 1),
    shiny::actionButton("optimize_flag_button", "Search for best flag placements"),
    shiny::br(),
    shiny::br(),
    shiny::uiOutput("optimize_flag_buy_flag_ui"),
    shiny::br(),
    shiny::br(),
    DT::dataTableOutput("optimize_flag_table"),
    shiny::plotOutput("optimize_flag_chart", height = "1000px")
  )
)
