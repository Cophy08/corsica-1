### SCRAPE NEXT DAY ###
# Last edit: Manny (2017-06-01)


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); require(doMC)
require(Kmisc); require(RSQLite); require(glmnet); require(rvest); require(survival)
require(caret)
load("/srv/shiny-server/modules/user_functions.RData")
load("/srv/shiny-server/modules/dryscrape.RData")
load("/srv/shiny-server/modules/stats.RData")
load("/srv/shiny-server/models/xg_model.RData")
load("/srv/shiny-server/models/adjustments_model.RData")


## Query database
# Select season
games <- paste0("20150",
                ds.all_games[c(1:665)],
                sep = ""
                )

# Connect to database
conn <- dbConnect(SQLite(), "~/corsica_data/raw.sqlite")

# Write query
pbp_query <- dbSendQuery(conn,
                         paste("SELECT * FROM pbp WHERE game_id IN (",
                               paste(games,
                                     collapse = ", "
                                     ),
                               ")",
                               sep = ""
                               )
                         )

# Fetch PBP
pbp <- fetch(pbp_query, -1)

# Disconnect
dbDisconnect(conn)

# Enhance PBP
pbp <- st.pbp_enhance(pbp)


## Compile Team Stats
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_team, game_strength_state) %>%
    st.sum_team("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_team, game_strength_state) %>%
    st.sum_team("Away")
) %>%
  data.frame() ->
  team_stats_full

## Compile Skater Stats
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, game_strength_state) %>%
    rename(player = home_on_1) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, game_strength_state) %>%
    rename(player = home_on_2) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, game_strength_state) %>%
    rename(player = home_on_3) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_4, game_strength_state) %>%
    rename(player = home_on_4) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_5, game_strength_state) %>%
    rename(player = home_on_5) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_6, game_strength_state) %>%
    rename(player = home_on_6) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, game_strength_state) %>%
    rename(player = away_on_1) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, game_strength_state) %>%
    rename(player = away_on_2) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, game_strength_state) %>%
    rename(player = away_on_3) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_4, game_strength_state) %>%
    rename(player = away_on_4) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_5, game_strength_state) %>%
    rename(player = away_on_5) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_6, game_strength_state) %>%
    rename(player = away_on_6) %>%
    st.sum_skater("Away")
) %>%
  data.frame() %>%
  group_by(season, session, game_id, game_date, player, game_strength_state, venue) %>%
  summarise_each(funs(sum), -c(GP)) %>%
  data.frame() %>%
  merge(team_stats_full %>%
          rename(tTOI = TOI, 
                 tCF = CF, 
                 tCA = CA, 
                 tFF = FF, 
                 tFA = FA, 
                 tSF = SF, 
                 tSA = SA, 
                 tGF = GF, 
                 tGA = GA, 
                 txGF = xGF, 
                 txGA = xGA, 
                 tACF = ACF, 
                 tACA = ACA, 
                 tAFF = AFF, 
                 tAFA = AFA, 
                 tASF = ASF, 
                 tASA = ASA, 
                 tAGF = AGF, 
                 tAGA = AGA, 
                 tAxGF = AxGF, 
                 tAxGA = AxGA,
                 tOZS = OZS, 
                 tDZS = DZS, 
                 tNZS = NZS
          ) %>%
          select(team, 
                 season, 
                 game_id, 
                 venue, 
                 game_strength_state,
                 tTOI:tNZS
          ) %>%
          data.frame(),
        by.x = c("season", "game_id", "venue", "team", "game_strength_state"),
        by.y = c("season", "game_id", "venue", "team", "game_strength_state")
  ) %>%
  data.frame() ->
  skater_stats_full

## Compile Goalie Stats
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_goalie, game_strength_state) %>%
    st.sum_goalie("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_goalie, game_strength_state) %>%
    st.sum_goalie("Away")
) %>%
  data.frame() ->
  goalie_stats_full


## Compile Combos Stats
# 2 players
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_2, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_2),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_3, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_3),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_4, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_3, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_3),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_4, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, home_on_4, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_3, player_2 = home_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_3, player_2 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_3, player_2 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_4, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_4, player_2 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_4, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_4, player_2 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_5, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_5, player_2 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_2, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_2),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_3, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_3),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_4, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_3, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_3),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_4, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, away_on_4, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_3, player_2 = away_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_3, player_2 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_3, player_2 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_4, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_4, player_2 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_4, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_4, player_2 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_5, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_5, player_2 = away_on_6)
) %>%
  data.frame() %>%
  filter(!is.na(player_1),
         !is.na(player_2)
  ) %>%
  group_by(player_1, player_2) %>%
  mutate(combo_code = st.combo_code(first(player_1), first(player_2), "0000000")) %>%
  group_by(season, session, game_id, game_date, combo_code, game_strength_state, venue) %>%
  summarise_each(funs(sum), -c(GP, player_1, player_2)) %>%
  merge(team_stats_full %>%
          rename(tTOI = TOI, 
                 tCF = CF, 
                 tCA = CA, 
                 tFF = FF, 
                 tFA = FA, 
                 tSF = SF, 
                 tSA = SA, 
                 tGF = GF, 
                 tGA = GA, 
                 txGF = xGF, 
                 txGA = xGA, 
                 tACF = ACF, 
                 tACA = ACA, 
                 tAFF = AFF, 
                 tAFA = AFA, 
                 tASF = ASF, 
                 tASA = ASA, 
                 tAGF = AGF, 
                 tAGA = AGA, 
                 tAxGF = AxGF, 
                 tAxGA = AxGA,
                 tOZS = OZS, 
                 tDZS = DZS, 
                 tNZS = NZS
                 ) %>%
          select(team, 
                 season, 
                 game_id, 
                 venue, 
                 game_strength_state,
                 tTOI:tNZS
                 ) %>%
          data.frame(),
        by.x = c("season", "game_id", "venue", "game_strength_state"),
        by.y = c("season", "game_id", "venue", "game_strength_state")
        ) %>%
  data.frame() ->
  combo_stats_2

# 3 players
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_2, home_on_3, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_2, player_3 = home_on_3),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_2, home_on_4, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_2, player_3 = home_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_2, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_2, player_3 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_2, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_2, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_3, home_on_4, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_3, player_3 = home_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_3, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_3, player_3 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_3, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_3, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_4, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_4, player_3 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_4, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_4, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, home_on_5, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_1, player_2 = home_on_5, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_3, home_on_4, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_3, player_3 = home_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_3, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_3, player_3 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_3, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_3, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_4, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_4, player_3 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_4, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_4, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, home_on_5, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_2, player_2 = home_on_5, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, home_on_4, home_on_5, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_3, player_2 = home_on_4, player_3 = home_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, home_on_4, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_3, player_2 = home_on_4, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, home_on_5, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_3, player_2 = home_on_5, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_4, home_on_5, home_on_6, game_strength_state) %>%
    st.sum_team("Home") %>%
    rename(player_1 = home_on_4, player_2 = home_on_5, player_3 = home_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_2, away_on_3, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_2, player_3 = away_on_3),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_2, away_on_4, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_2, player_3 = away_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_2, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_2, player_3 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_2, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_2, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_3, away_on_4, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_3, player_3 = away_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_3, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_3, player_3 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_3, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_3, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_4, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_4, player_3 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_4, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_4, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, away_on_5, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_1, player_2 = away_on_5, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_3, away_on_4, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_3, player_3 = away_on_4),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_3, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_3, player_3 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_3, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_3, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_4, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_4, player_3 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_4, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_4, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, away_on_5, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_2, player_2 = away_on_5, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, away_on_4, away_on_5, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_3, player_2 = away_on_4, player_3 = away_on_5),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, away_on_4, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_3, player_2 = away_on_4, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, away_on_5, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_3, player_2 = away_on_5, player_3 = away_on_6),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_4, away_on_5, away_on_6, game_strength_state) %>%
    st.sum_team("Away") %>%
    rename(player_1 = away_on_4, player_2 = away_on_5, player_3 = away_on_6)
) %>%
  data.frame() %>%
  filter(!is.na(player_1),
         !is.na(player_2),
         !is.na(player_3)
  ) %>%
  group_by(player_1, player_2, player_3) %>%
  mutate(combo_code = st.combo_code(first(player_1), first(player_2), first(player_3))) %>%
  group_by(season, session, game_id, game_date, combo_code, game_strength_state, venue) %>%
  summarise_each(funs(sum), -c(GP, player_1, player_2, player_3)) %>%
  merge(team_stats_full %>%
          rename(tTOI = TOI, 
                 tCF = CF, 
                 tCA = CA, 
                 tFF = FF, 
                 tFA = FA, 
                 tSF = SF, 
                 tSA = SA, 
                 tGF = GF, 
                 tGA = GA, 
                 txGF = xGF, 
                 txGA = xGA, 
                 tACF = ACF, 
                 tACA = ACA, 
                 tAFF = AFF, 
                 tAFA = AFA, 
                 tASF = ASF, 
                 tASA = ASA, 
                 tAGF = AGF, 
                 tAGA = AGA, 
                 tAxGF = AxGF, 
                 tAxGA = AxGA,
                 tOZS = OZS, 
                 tDZS = DZS, 
                 tNZS = NZS
                 ) %>%
          select(team, 
                 season, 
                 game_id, 
                 venue, 
                 game_strength_state,
                 tTOI:tNZS
                 ) %>%
          data.frame(),
        by.x = c("season", "game_id", "venue", "game_strength_state"),
        by.y = c("season", "game_id", "venue", "game_strength_state")
        ) %>%
  data.frame() ->
  combo_stats_3


## Write to database (Corsica)
# Connect
conn <- dbConnect(SQLite(), "~/corsica_data/corsica.sqlite")

# Check for duplicates
pbp_contains <- sqliteQuickColumn(conn,
                                  "pbp",
                                  "game_id"
                                  )

pbp %>%
  filter(game_id %in% unique(pbp_contains) == FALSE) %>%
  data.frame() ->
  pbp

team_contains <- sqliteQuickColumn(conn,
                                   "team_stats",
                                   "game_id"
                                   )

team_stats_full %>%
  filter(game_id %in% unique(team_contains) == FALSE) %>%
  data.frame() ->
  team_stats_full

skater_contains <- sqliteQuickColumn(conn,
                                     "skater_stats",
                                     "game_id"
                                     )

skater_stats_full %>%
  filter(game_id %in% unique(skater_contains) == FALSE) %>%
  data.frame() ->
  skater_stats_full

goalie_contains <- sqliteQuickColumn(conn,
                                     "goalie_stats",
                                     "game_id"
                                     )

goalie_stats_full %>%
  filter(game_id %in% unique(goalie_contains) == FALSE) %>%
  data.frame() ->
  goalie_stats_full

combo2_contains <- sqliteQuickColumn(conn,
                                     "combo2_stats",
                                     "game_id"
                                     )

combo_stats_2 %>%
  filter(game_id %in% unique(combo2_contains) == FALSE) %>%
  data.frame() ->
  combo_stats_2

combo3_contains <- sqliteQuickColumn(conn,
                                     "combo3_stats",
                                     "game_id"
                                     )

combo_stats_3 %>%
  filter(game_id %in% unique(combo3_contains) == FALSE) %>%
  data.frame() ->
  combo_stats_3

# Write
dbWriteTable(conn,
             "pbp",
             pbp,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "team_stats",
             team_stats_full,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "skater_stats",
             skater_stats_full,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "goalie_stats",
             goalie_stats_full,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "combo2_stats",
             combo_stats_2,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "combo3_stats",
             combo_stats_3,
             overwrite = FALSE,
             append = TRUE
             )

# Disconnect
dbDisconnect(conn)

