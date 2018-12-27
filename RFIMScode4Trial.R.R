install.packages('tidyverse')

library("tidyverse")

install.packages("magrittr")
library("assertr")
library("dplyr")
library("magrittr")
library("tidyr")
library("readxl")
library("stringr")
library("writexl")


# User inputs
input_file_name <- "~\\Personal\\Trial_Test\\input\\rfims.xlsx"
output_file_name <- "~\\Personal\\Trial_Test\\output\\output1_rfims.xlsx"

# Input tabs as Tibbles
input_file_name %>% 
  excel_sheets %>% 
  lapply( function( name ) assign( name,
                                   read_excel( input_file_name, sheet = name ),
                                   envir = .GlobalEnv ) )
#------------------#
#     Functions 1  #
#------------------#

system_name <- function( subsystem_name ) subsystem_name %>% str_split("_") %>% first %>% first

#------------------#
#     Functions 2  #
#------------------#

calculate_aggregate_m.dash.cm <- function( Data.Tib , parent_name) Data.Tib %>%  
  mutate( lambda = 1 / MTBM.CM, cm_total_repair_time = lambda * CM.Repair.Time.Hours )  %>%  
  group_by( !! enquo(parent_name) ) %>%   
  summarize( numerator = sum( cm_total_repair_time ), denominator = sum( lambda ) ) %>%   
  mutate( Mdash.CM = numerator / denominator ) %>%  
  select( -numerator, -denominator )

calculate_aggregate_m.dash.pm <- function( Data.Tib , parent_name) Data.Tib %>%  
  mutate( freq = 1 / MTBM.PM, pm_total_repair_time = freq * PM.Repair.Time.Hours )  %>%   
  group_by( !! enquo(parent_name) ) %>%   
  summarize( numerator = sum( pm_total_repair_time ), denominator = sum( freq ) ) %>%   
  mutate( Mdash.PM = numerator / denominator ) %>%  
  select( -numerator, -denominator )

calculate_m.dash <- function( Data.Tib ) Data.Tib %>%  
  mutate( M.DASH = mapply( function(x,y,z,w) (x*z+y*w)/(x+y),                           
                           x = lambda,                           
                           y = freq,                           
                           z = Mdash.CM,                           
                           w = Mdash.PM ))

calculate_aggregate_logistic.cm <- function( Data.Tib , parent_name) Data.Tib %>%  
  mutate( lambda = 1 / MTBM.CM, cm_mldt = lambda * CM.MLDT )  %>%  
  group_by( !! enquo(parent_name) ) %>%   
  summarize( numerator = sum( cm_mldt ), denominator = sum( lambda ) ) %>%   
  mutate( Logistic.CM = numerator / denominator ) %>%  
  select( -numerator, -denominator )

calculate_aggregate_logistic.pm <- function( Data.Tib , parent_name) Data.Tib %>%  
  mutate( freq = 1 / MTBM.PM, pm_mldt = freq * PM.MLDT )  %>%   
  group_by( !! enquo(parent_name) ) %>%   
  summarize( numerator = sum( pm_mldt ), denominator = sum( freq ) ) %>%   
  mutate( Logistic.PM = numerator / denominator ) %>%  
  select( -numerator, -denominator )

calculate_logistic_delay_time <- function( Data.Tib ) Data.Tib %>%  
  mutate( MLDT = mapply( function(x,y,z,w) (x*z+y*w)/(x+y),                         
                         x = lambda,                         
                         y = freq,                         
                         z = Logistic.CM,                         
                         w = Logistic.PM )) 

calculate_aggregate_admin.cm <- function( Data.Tib , parent_name) Data.Tib %>%  
  mutate( lambda = 1 / MTBM.CM, cm_mtwa = lambda * CM.MTWA )  %>%  
  group_by( !! enquo(parent_name) ) %>%   
  summarize( numerator = sum( cm_mtwa ), denominator = sum( lambda ) ) %>%   
  mutate( Admin.CM = numerator / denominator ) %>%  
  select( -numerator, -denominator )

calculate_aggregate_admin.pm <- function( Data.Tib , parent_name) Data.Tib %>%  
  mutate( freq = 1 / MTBM.PM, pm_mtwa = freq * PM.MTWA )  %>%   
  group_by( !! enquo(parent_name) ) %>%   
  summarize( numerator = sum( pm_mtwa ), denominator = sum( freq ) ) %>%   
  mutate( Admin.PM = numerator / denominator ) %>%  
  select( -numerator, -denominator )

calculate_aggregate_a.o <- function( Data.Tib, parent_name) Data.Tib %>%
  mutate( Unit.A.o = MTBM.CM/( MTBM.CM + CM.Repair.Time.Hours + CM.MLDT + CM.MTWA ) ) %>%
  mutate( A.o.With.Redundancy = mapply( function(x,y,z) account_for_redundancy(x,y,z),                                    
                                        x = Server.Count - How.Many.Up,                                    
                                        y = Server.Count,                                    
                                        z = 1 - Unit.A.o )) %>%
  group_by( !! enquo(parent_name) ) %>%
  summarize( Composite.A.o = prod(A.o.With.Redundancy))

calculate_aggregate_mtbm.cm <- function( Data.Tib ) Data.Tib %>%
  mutate( Composite.MTBM.CM = Composite.A.o * (Mdash.CM + Logistic.CM + Admin.CM)/(1 - Composite.A.o))

calculate_aggregate_mtbm.cm.no_repair <- function( Data.Tib ) Data.Tib %>%
  mutate( MTBM.CM = mapply( function( lambda, n, q) 1/( lambda/sum( 1/( ( n-q ):n ) ) ),
                            lambda = 1/MTBM.CM, 
                            n = Server.Count, 
                            q = Server.Count - How.Many.Up ) ) %>%
  mutate( How.Many.Up = 1, Server.Count = 1 )

calculate_admin_wait_time <- function( Data.Tib ) Data.Tib %>%  
  mutate( MTWA = mapply( function(x,y,z,w) (x*z+y*w)/(x+y),                         
                         x = lambda,                         
                         y = freq,                         
                         z = Admin.CM,                         
                         w = Admin.PM )) %>%  
  mutate( MDT = mapply( function(x,y,z) x+y+z,                        
                        x = M.DASH,                        
                        y = MLDT,                        
                        z = MTWA )) %>%  
  mutate( MTBM = mapply( function(x,y) (x+y)^-1,                         
                         x = lambda,                         
                         y = freq)) 

calculate_predicted.avail <- function( Data.Tib ) Data.Tib %>%  
  mutate( Predicted.Avail = mapply( function(x,y) x/(x+y),                                    
                                    x = MTBM,                                    
                                    y = MDT )) %>%  
  mutate( Predicted.Avail = mapply( function(x,y,z) account_for_redundancy(x,y,z),                                    
                                    x = Server.Count - How.Many.Up,                                    
                                    y = Server.Count,                                    
                                    z = 1 - Predicted.Avail ))

account_for_redundancy <- function( num_spares, total_num_servers, prob_of_failure) sum(dbinom(0:num_spares,                                                                                                
                                                                                               total_num_servers,                                                                                                
                                                                                               prob_of_failure)) %>%  
  min( . , .9999999 )

calculate_predicted.mtbm <- function( Data.Tib ) Data.Tib %>%  
  mutate( Predicted.MTBM = mapply ( function(x,y) x*y/(1-x),                                    
                                    x = Predicted.Avail,                                    
                                    y = MDT))
#------------------#
#     Functions 3  #
#------------------#

calculate_fci <- function( Data.Tib ) Data.Tib %>%  
  mutate( Global.Error = sum(1 - Data.Tib$Predicted.Avail )) %>%  
  mutate( FCI = mapply( function(x,y) (1-x)/(y),                        
                        x = Predicted.Avail,                        
                        y = Global.Error )) %>%  
  select( -Global.Error )



calculate_avail_budget <- function( Data.Tib ) { Data.Tib %<>%     
    mutate( Avail.Budget = mapply( function(x) 1/log(x),                                    
                                   x = FCI))  
  Data.Tib %<>%    
    mutate( Avail.Denom = sum( Data.Tib$Avail.Budget ) ) %>%    
    mutate( Avail.Budget = mapply( function(x,y) x/y,                                   
                                   x = Avail.Budget,                                   
                                   y = Avail.Denom)) %>%    
    select( -Avail.Denom) 
  
  return(Data.Tib) }

calculate_avail_targets <- function( Data.Tib ) Data.Tib %>%  
  mutate( Avail.Req.For.95.Percent = mapply ( function(x,y) 1-(1-x)*y,                                              
                                              x = 0.95,                                              
                                              y = Avail.Budget)) %>%  
  mutate( Avail.Req.For.99.Percent = mapply ( function(x,y) 1-(1-x)*y,                                              
                                              x = 0.99,                                               
                                              y = Avail.Budget))

calculate_mtbm_targets <- function( Data.Tib ) Data.Tib %>%  
  mutate ( MTBM.Req.For.95.Percent = mapply ( function(x,y) x*y/(1-x),                                              
                                              x = Avail.Req.For.95.Percent,                                              
                                              y = MDT)) %>%  
  mutate ( MTBM.Req.For.99.Percent = mapply ( function(x,y) x*y/(1-x),                                              
                                              x = Avail.Req.For.99.Percent,                                              
                                              y = MDT)) %>%  
  mutate ( MTBM.Req.For.95.Percent = ifelse( test = MTBM.Req.For.95.Percent > Predicted.MTBM,                                             
                                             yes  = MTBM,                                             
                                             no   = ifelse( test = MTBM.Req.For.95.Percent * 1.5 < Predicted.MTBM,                                                            
                                                            yes  = MTBM.Req.For.95.Percent * 1.5,                                                            
                                                            no   = MTBM.Req.For.95.Percent ))) %>%  
  mutate ( MTBM.Req.For.99.Percent = ifelse( test = MTBM.Req.For.99.Percent > Predicted.MTBM,                                             
                                             yes  = MTBM,                                             
                                             no   = ifelse( test = MTBM.Req.For.99.Percent * 1.5 < Predicted.MTBM,                                                            
                                                            yes  = MTBM.Req.For.99.Percent * 1.5,                                                            
                                                            no   = MTBM.Req.For.99.Percent )))


calculate_mdt_targets <- function( Data.Tib ) Data.Tib %>%  
  mutate ( MDT.Req.For.95.Percent = mapply( function(x,y) y*(1-x)/x,                                            
                                            x = Avail.Req.For.95.Percent,                                            
                                            y = MTBM.Req.For.95.Percent)) %>%  
  mutate ( MDT.Req.For.99.Percent = mapply( function(x,y) y*(1-x)/x,                                            
                                            x = Avail.Req.For.99.Percent,                                            
                                            y = MTBM.Req.For.99.Percent))



gen_mdt_req_tib <- function( Data.Tib, block_name, parent_name, mtbm_req_name, mdt_req_name ) {  
  block <- enquo( block_name )  
  parent <- enquo( parent_name )  
  mtbm_req <- enquo( mtbm_req_name )  
  mdt_req <- enquo( mdt_req_name )  
  Data.Tib %>%    
    group_by( !! block, !! parent ) %>%     
    mutate( MTBM = !! mtbm_req,            
            MDT = !! mdt_req ) %>%     
    calculate_aggregate_mdt( !! parent ) %>%     
    rename( !! quo_name(mdt_req) := MDT )  
}



gen_dd_tib <- function( Data.Tib, block, parent, grandparent ) Data.Tib %>%  
  gen_mdt_req_tib( !! enquo( block ),                   
                   !! enquo( parent ),                   
                   MTBM.Req.For.95.Percent,                   
                   MDT.Req.For.95.Percent ) %>%  
  left_join( Data.Tib %>%               
               gen_mdt_req_tib( !! enquo( block ),                                
                                !! enquo( parent),                                
                                MTBM.Req.For.99.Percent,                                
                                MDT.Req.For.99.Percent),             
             by = quo_name( enquo(parent) ) )   %>%  
  left_join( Data.Tib %>%                
               group_by( !! enquo( parent ),                         
                         !! enquo( grandparent ) )  %>%               
               gen_avail_parameters_for_req,             
             by =  quo_name( enquo( parent ) ) ) %>%  
  gen_mtbm_parameters_for_req



gen_avail_parameters_for_req <- function(Data.Tib) Data.Tib %>%  
  summarize( Avail.Req.For.95.Percent = prod(Avail.Req.For.95.Percent),             
             Avail.Req.For.99.Percent = prod(Avail.Req.For.99.Percent))



gen_mtbm_parameters_for_req <- function(Data.Tib) Data.Tib %>%  
  mutate ( MTBM.Req.For.95.Percent = mapply ( function(x,y) x*y/(1-x),                                              
                                              x = Avail.Req.For.95.Percent,                                              
                                              y = MDT.Req.For.95.Percent ),           
           MTBM.Req.For.99.Percent = mapply ( function(x,y) x*y/(1-x),                                              
                                              x = Avail.Req.For.99.Percent,                                              
                                              y = MDT.Req.For.99.Percent ))


#------------------#
#   Main Argument  #
#------------------#

# Create a System Mapping Tib
System.Mapping <- Sheet4 %>%
  select( Block, SDD.Unit = Parent ) %>% 
  mutate( Block = SDD.Unit ) %>% 
  mutate( SDD.Unit = mapply(function(x) system_name(x), x = Block )) %>% unique

#--------------------#
# Synthesize metrics #
# for blocks with    #
# sub-components     #
#--------------------#

#  Map tib to parent for rollup
Rollup.Tib <- Sheet1 %>%
  inner_join( Sheet2, by ="Block" ) 

# Create block metrics for sector
# which assumes redundany without repair
Redun.W.o.Repair.Tib <- Rollup.Tib %>%
  filter( Parent == 'sector' ) %>%
  calculate_aggregate_mtbm.cm.no_repair
# Now add these back to the Rollup tib
Rollup.Tib %<>%
  filter( Parent != 'sector') %>%
  bind_rows( Redun.W.o.Repair.Tib )

# Calculate aggregate metrics for roll up, set them as individual block metrics
Rollup.Tib %<>%
  mutate( freq = How.Many.Up/MTBM.PM) %>% 
  group_by( Parent ) %>% 
  summarize( freq= sum(freq)) %>% 
  left_join( Rollup.Tib %>% calculate_aggregate_m.dash.cm(Parent), by = "Parent") %>%
  left_join( Rollup.Tib %>% calculate_aggregate_m.dash.pm(Parent), by = "Parent") %>% 
  left_join( Rollup.Tib %>% calculate_aggregate_logistic.cm(Parent), by = "Parent") %>% 
  left_join( Rollup.Tib %>% calculate_aggregate_logistic.pm(Parent), by = "Parent") %>%
  left_join( Rollup.Tib %>% calculate_aggregate_admin.cm(Parent), by = "Parent") %>%
  left_join( Rollup.Tib %>% calculate_aggregate_admin.pm(Parent), by = "Parent") %>%
  left_join( Rollup.Tib %>% calculate_aggregate_a.o(Parent), by = "Parent") %>%
  calculate_aggregate_mtbm.cm() %>%
  mutate( MTBM.PM = 1/freq,
          Part.Refresh.Period.Days = 30,
          MTBF.Source = 'calculated') %>%
  rename( Block = Parent,
          MTBM.CM = Composite.MTBM.CM,
          CM.Repair.Time.Hours = Mdash.CM, 
          PM.Repair.Time.Hours = Mdash.PM,
          CM.MLDT = Logistic.CM,
          PM.MLDT = Logistic.PM,
          CM.MTWA = Admin.CM,
          PM.MTWA = Admin.PM) %>%
  select( -freq, -Composite.A.o )

# Add roll up calculations to block
Sheet1 %<>%
  bind_rows( Rollup.Tib )

# Repeat the process for the second level of roll up
Rollup.2.Tib <- Sheet1 %>%
  inner_join( Sheet3, by ="Block" ) 

# Calculate aggregate metrics for roll up, set them as individual block metrics
Rollup.2.Tib %<>%
  mutate( freq = How.Many.Up/MTBM.PM) %>% 
  group_by( Parent ) %>% 
  summarize( freq= sum(freq)) %>% 
  left_join( Rollup.2.Tib %>% calculate_aggregate_m.dash.cm(Parent), by = "Parent") %>%
  left_join( Rollup.2.Tib %>% calculate_aggregate_m.dash.pm(Parent), by = "Parent") %>%
  left_join( Rollup.2.Tib %>% calculate_aggregate_logistic.cm(Parent), by = "Parent") %>% 
  left_join( Rollup.2.Tib %>% calculate_aggregate_logistic.pm(Parent), by = "Parent") %>%
  left_join( Rollup.2.Tib %>% calculate_aggregate_admin.cm(Parent), by = "Parent") %>%
  left_join( Rollup.2.Tib %>% calculate_aggregate_admin.pm(Parent), by = "Parent") %>%
  left_join( Rollup.2.Tib %>% calculate_aggregate_a.o(Parent), by = "Parent") %>%
  calculate_aggregate_mtbm.cm() %>%
  mutate( MTBM.PM = 1/freq,
          Part.Refresh.Period.Days = 30,
          MTBF.Source = 'calculated') %>%
  rename( Block = Parent,
          MTBM.CM = Composite.MTBM.CM,
          CM.Repair.Time.Hours = Mdash.CM, 
          PM.Repair.Time.Hours = Mdash.PM,
          CM.MLDT = Logistic.CM,
          PM.MLDT = Logistic.PM,
          CM.MTWA = Admin.CM,
          PM.MTWA = Admin.PM) %>%
  select( -freq, -Composite.A.o )

# Add new synthesized blocks to the original block list
Sheet1 %<>%
  bind_rows( Rollup.2.Tib )

#--------------------#
# Perform roll up to #
# generate req. tibs #
# e.g. FDD, SSDD and #
# SDD tibs           #
#--------------------#

# First generate the FDD Tib
LRU.Tib <- Sheet1 %>%
  inner_join( Sheet4, by = "Block") %>%
  inner_join( System.Mapping, by = c("Parent" = "Block")) %>%
  unique %>%
  rename( SSDD.Unit = Parent ) %>%
  mutate( SLR.Unit = "RFIMS") %>%
  tibble::rowid_to_column( "ID")

Pool.Tib <- LRU.Tib %>% 
  filter( Server.Count > 2)

Pool.Tib %<>% 
  mutate( freq = How.Many.Up/MTBM.PM) %>% 
  mutate( Block = mapply( function(x) paste(x,"pool",sep="_"), x = Block )) %>%
  group_by( ID, Block, SSDD.Unit, SDD.Unit, SLR.Unit ) %>% 
  summarize( freq= sum(freq)) %>% 
  left_join( Pool.Tib %>% calculate_aggregate_m.dash.cm(ID), by = "ID") %>%
  left_join( Pool.Tib %>% calculate_aggregate_m.dash.pm(ID), by = "ID") %>% 
  left_join( Pool.Tib %>% calculate_aggregate_logistic.cm(ID), by = "ID") %>% 
  left_join( Pool.Tib %>% calculate_aggregate_logistic.pm(ID), by = "ID") %>%
  left_join( Pool.Tib %>% calculate_aggregate_admin.cm(ID), by = "ID") %>%
  left_join( Pool.Tib %>% calculate_aggregate_admin.pm(ID), by = "ID") %>%
  left_join( Pool.Tib %>% calculate_aggregate_a.o(ID), by = "ID") %>%
  calculate_aggregate_mtbm.cm() %>%
  mutate( MTBM.PM = 1/freq ) %>%
  rename( MTBM.CM = Composite.MTBM.CM,
          CM.Repair.Time.Hours = Mdash.CM, 
          PM.Repair.Time.Hours = Mdash.PM,
          CM.MLDT = Logistic.CM,
          PM.MLDT = Logistic.PM,
          CM.MTWA = Admin.CM,
          PM.MTWA = Admin.PM) %>%
  select( -freq, -Composite.A.o )

FDD.Tib <- LRU.Tib %>% 
  filter( Server.Count == 1) %>% 
  bind_rows( Pool.Tib) %>% 
  mutate( How.Many.Up = 1, Server.Count = 1 ) %>%
  select( -Part.Refresh.Period.Days, -MTBF.Source ) 

# Generate the columns for the FDD.Tib requirement calc inputs
FDD.Tib %<>% 
  mutate( lambda = How.Many.Up/MTBM.CM, freq = How.Many.Up/MTBM.PM) %>% 
  group_by( ID, Block, SSDD.Unit, SDD.Unit, SLR.Unit, How.Many.Up, Server.Count ) %>% 
  summarize( lambda = sum(lambda), freq= sum(freq)) %>% 
  mutate( mtbm = ( lambda + freq)^-1 ) %>% 
  left_join( FDD.Tib %>% calculate_aggregate_m.dash.cm(ID), by = "ID") %>%
  left_join( FDD.Tib %>% calculate_aggregate_m.dash.pm(ID), by = "ID") %>%
  left_join( FDD.Tib %>% calculate_aggregate_logistic.cm(ID), by = "ID") %>% 
  left_join( FDD.Tib %>% calculate_aggregate_logistic.pm(ID), by = "ID") %>%
  left_join( FDD.Tib %>% calculate_aggregate_admin.cm(ID), by = "ID") %>%
  left_join( FDD.Tib %>% calculate_aggregate_admin.pm(ID), by = "ID")

# Calculate the req for the FDD Tib
FDD.Tib %<>%
  calculate_m.dash() %>%
  calculate_logistic_delay_time() %>%
  calculate_admin_wait_time() %>% 
  calculate_predicted.avail() %>%
  calculate_predicted.mtbm() %>%
  calculate_fci() %>%
  calculate_avail_budget() %>%
  calculate_avail_targets() %>%
  calculate_mtbm_targets() %>% 
  calculate_mdt_targets() %>%
  ungroup 


# Generate tne SSDD Tib
SSDD.Tib <- FDD.Tib %>%
  mutate( MTBM.CM = 1/lambda,
          MTBM.PM = 1/freq ) %>%
  rename( CM.Repair.Time.Hours = Mdash.CM, 
          PM.Repair.Time.Hours = Mdash.PM,
          CM.MLDT = Logistic.CM,
          PM.MLDT = Logistic.PM,
          CM.MTWA = Admin.CM,
          PM.MTWA = Admin.PM)

# Generate input columns for SSDD.Tib requirements
SSDD.Tib %<>%
  mutate( lambda = How.Many.Up/MTBM.CM, freq = How.Many.Up/MTBM.PM) %>% 
  group_by( SSDD.Unit, SDD.Unit, SLR.Unit, How.Many.Up, Server.Count  ) %>% 
  summarize( lambda = sum(lambda), freq= sum(freq)) %>% 
  mutate( mtbm = ( lambda + freq)^-1 ) %>% 
  left_join( SSDD.Tib %>% calculate_aggregate_m.dash.cm(SSDD.Unit), by = "SSDD.Unit") %>%
  left_join( SSDD.Tib %>% calculate_aggregate_m.dash.pm(SSDD.Unit), by = "SSDD.Unit") %>%
  left_join( SSDD.Tib %>% calculate_aggregate_logistic.cm(SSDD.Unit), by = "SSDD.Unit") %>% 
  left_join( SSDD.Tib %>% calculate_aggregate_logistic.pm(SSDD.Unit), by = "SSDD.Unit") %>%
  left_join( SSDD.Tib %>% calculate_aggregate_admin.cm(SSDD.Unit), by = "SSDD.Unit") %>%
  left_join( SSDD.Tib %>% calculate_aggregate_admin.pm(SSDD.Unit), by = "SSDD.Unit") %>%
  left_join( FDD.Tib %>%
               group_by( SSDD.Unit ) %>%
               summarise( Avail.Req.For.95.Percent = prod( Avail.Req.For.95.Percent ),
                          Avail.Req.For.99.Percent = prod( Avail.Req.For.99.Percent ) ),
             by = 'SSDD.Unit' )

# Calculate Analytics on SSDD.Tib
SSDD.Tib %<>%
  calculate_m.dash() %>%
  calculate_logistic_delay_time() %>%
  calculate_admin_wait_time() %>% 
  calculate_predicted.avail()%>%
  calculate_predicted.mtbm() %>%
  calculate_fci() %>%
  calculate_mtbm_targets() %>% 
  calculate_mdt_targets() %>%
  ungroup 


# Generate an SDD Tib
SDD.Tib <- SSDD.Tib %>%
  mutate( MTBM.CM = 1/lambda,
          MTBM.PM = 1/freq ) %>%
  rename( CM.Repair.Time.Hours = Mdash.CM, 
          PM.Repair.Time.Hours = Mdash.PM,
          CM.MLDT = Logistic.CM,
          PM.MLDT = Logistic.PM,
          CM.MTWA = Admin.CM,
          PM.MTWA = Admin.PM)

# Generate input columns for SDD.Tib Requirements
SDD.Tib %<>%
  group_by( SDD.Unit, SLR.Unit, How.Many.Up, Server.Count ) %>%
  summarize( lambda = sum(lambda), freq= sum(freq)) %>% 
  mutate( mtbm = ( lambda + freq)^-1 ) %>% 
  left_join( SDD.Tib %>% calculate_aggregate_m.dash.cm(SDD.Unit), by = "SDD.Unit") %>%
  left_join( SDD.Tib %>% calculate_aggregate_m.dash.pm(SDD.Unit), by = "SDD.Unit") %>%
  left_join( SDD.Tib %>% calculate_aggregate_logistic.cm(SDD.Unit), by = "SDD.Unit") %>% 
  left_join( SDD.Tib %>% calculate_aggregate_logistic.pm(SDD.Unit), by = "SDD.Unit") %>%
  left_join( SDD.Tib %>% calculate_aggregate_admin.cm(SDD.Unit), by = "SDD.Unit") %>%
  left_join( SDD.Tib %>% calculate_aggregate_admin.pm(SDD.Unit), by = "SDD.Unit") %>%
  left_join( SSDD.Tib %>%
               group_by( SDD.Unit ) %>%
               summarise( Avail.Req.For.95.Percent = prod( Avail.Req.For.95.Percent ),
                          Avail.Req.For.99.Percent = prod( Avail.Req.For.99.Percent ) ),
             by = 'SDD.Unit' )

# Calculate Analytics on SDD.Tib
SDD.Tib %<>%
  calculate_m.dash() %>%
  calculate_logistic_delay_time() %>%
  calculate_admin_wait_time() %>% 
  calculate_predicted.avail() %>%
  calculate_predicted.mtbm() %>%
  calculate_fci() %>%
  calculate_avail_budget() %>%
  calculate_mtbm_targets() %>% 
  calculate_mdt_targets() %>%
  select( -lambda, -freq, -mtbm, -Server.Count ) %>%
  ungroup %>%
  select( -How.Many.Up )

FDD.Tib %<>%
  select( Block, SSDD.Unit, SDD.Unit, SLR.Unit, M.DASH, MLDT, MTWA, MDT, MTBM, FCI, Avail.Budget,
          Avail.Req.For.95.Percent, MTBM.Req.For.95.Percent, MDT.Req.For.95.Percent,
          Avail.Req.For.99.Percent, MTBM.Req.For.95.Percent, MDT.Req.For.99.Percent )

SSDD.Tib %<>%
  select( SSDD.Unit, SDD.Unit, SLR.Unit, M.DASH, MLDT, MTWA, MDT, MTBM, FCI,
          Avail.Req.For.95.Percent, MTBM.Req.For.95.Percent, MDT.Req.For.95.Percent,
          Avail.Req.For.99.Percent, MTBM.Req.For.99.Percent, MDT.Req.For.99.Percent )

SDD.Tib %<>%
  select( SDD.Unit, SLR.Unit, M.DASH, MLDT, MTWA, MDT, MTBM, FCI,
          Avail.Req.For.95.Percent, MTBM.Req.For.95.Percent, MDT.Req.For.95.Percent,
          Avail.Req.For.99.Percent, MTBM.Req.For.99.Percent, MDT.Req.For.99.Percent )

# Write results to file
list( FDD = FDD.Tib, SSDD = SSDD.Tib, SDD = SDD.Tib ) %>%
  write_xlsx( output_file_name )

# Validate Roll-Up
# FDD.Tib %>% group_by( SSDD.Unit ) %>% summarise( Avail.Budget = sum(Avail.Budget), N5 = prod( Avail.Req.For.95.Percent ), N9 = prod( Avail.Req.For.99.Percent) ) %>% calculate_avail_targets() %>% mutate( N5.Error = Avail.Req.For.95.Percent - N5, N9.Error = Avail.Req.For.99.Percent - N9)



