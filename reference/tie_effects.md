# tie_effects

Overview of statistics in the tie-oriented model, see Details.

## Usage

``` r
tie_effects(directed = NULL, endogenous = NULL)
```

## Arguments

- directed:

  logical value. The function outputs all statistics in the tie-oriented
  model for directed events if true, or all statistics in the
  tie-oriented model for undirected events if false.

- endogenous:

  logical value. The function outputs all endogenous statistics in the
  tie-oriented model if true, or all exogenous statistics if false

## Value

Returns a list of available effects and their corresponding statistics.

## Details

Overview of statistics in the tie-oriented model.

Baseline:

- [`baseline`](https://tilburgnetworkgroup.github.io/remstats/reference/baseline.md)

Exogenous statistics:

- [`send()`](https://tilburgnetworkgroup.github.io/remstats/reference/send.md)

- [`receive()`](https://tilburgnetworkgroup.github.io/remstats/reference/receive.md)

- [`tie()`](https://tilburgnetworkgroup.github.io/remstats/reference/tie.md)

- [`same()`](https://tilburgnetworkgroup.github.io/remstats/reference/same.md)

- [`difference()`](https://tilburgnetworkgroup.github.io/remstats/reference/difference.md)

- [`average()`](https://tilburgnetworkgroup.github.io/remstats/reference/average.md)

- [`minimum()`](https://tilburgnetworkgroup.github.io/remstats/reference/minimum.md)

- [`maximum()`](https://tilburgnetworkgroup.github.io/remstats/reference/maximum.md)

- [`event()`](https://tilburgnetworkgroup.github.io/remstats/reference/event.md)

- [`userStat()`](https://tilburgnetworkgroup.github.io/remstats/reference/userStat.md)

Endogenous statistics:

- [`indegreeSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/indegreeSender.md)

- [`indegreeReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/indegreeReceiver.md)

- [`outdegreeSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/outdegreeSender.md)

- [`outdegreeReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/outdegreeReceiver.md)

- [`totaldegreeSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeSender.md)

- [`totaldegreeReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeReceiver.md)

- [`totaldegreeDyad()`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeDyad.md)

- [`degreeMin()`](https://tilburgnetworkgroup.github.io/remstats/reference/degreeMin.md)

- [`degreeMax()`](https://tilburgnetworkgroup.github.io/remstats/reference/degreeMax.md)

- [`degreeDiff()`](https://tilburgnetworkgroup.github.io/remstats/reference/degreeDiff.md)

- [`inertia()`](https://tilburgnetworkgroup.github.io/remstats/reference/inertia.md)

- [`reciprocity()`](https://tilburgnetworkgroup.github.io/remstats/reference/reciprocity.md)

- [`otp()`](https://tilburgnetworkgroup.github.io/remstats/reference/otp.md)

- [`itp()`](https://tilburgnetworkgroup.github.io/remstats/reference/itp.md)

- [`osp()`](https://tilburgnetworkgroup.github.io/remstats/reference/osp.md)

- [`isp()`](https://tilburgnetworkgroup.github.io/remstats/reference/isp.md)

- [`sp()`](https://tilburgnetworkgroup.github.io/remstats/reference/sp.md)

- [`psABBA()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBA.md)

- [`psABBY()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBY.md)

- [`psABXA()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXA.md)

- [`psABXB()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXB.md)

- [`psABXY()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXY.md)

- [`psABAY()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABAY.md)

- [`psABAB()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABAB.md)

- [`rrankSend()`](https://tilburgnetworkgroup.github.io/remstats/reference/rrankSend.md)

- [`rrankReceive()`](https://tilburgnetworkgroup.github.io/remstats/reference/rrankReceive.md)

- [`recencySendSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencySendSender.md)

- [`recencySendReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencySendReceiver.md)

- [`recencyReceiveSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyReceiveSender.md)

- [`recencyReceiveReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyReceiveReceiver.md)

- [`recencyContinue()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyContinue.md)

- [`FEtype()`](https://tilburgnetworkgroup.github.io/remstats/reference/FEtype.md)

## Examples

``` r
# List of available effects 
tie_effects()
#>  [1] "baseline"               "FEtype"                 "send"                  
#>  [4] "receive"                "tie"                    "same"                  
#>  [7] "difference"             "average"                "minimum"               
#> [10] "maximum"                "event"                  "inertia"               
#> [13] "reciprocity"            "indegreeSender"         "indegreeReceiver"      
#> [16] "outdegreeSender"        "outdegreeReceiver"      "totaldegreeSender"     
#> [19] "totaldegreeReceiver"    "totaldegreeDyad"        "degreeMin"             
#> [22] "degreeMax"              "degreeDiff"             "otp"                   
#> [25] "itp"                    "osp"                    "isp"                   
#> [28] "sp"                     "psABBA"                 "psABBY"                
#> [31] "psABXA"                 "psABXB"                 "psABXY"                
#> [34] "psABAY"                 "psABAB"                 "rrankSend"             
#> [37] "rrankReceive"           "recencyContinue"        "recencySendSender"     
#> [40] "recencySendReceiver"    "recencyReceiveSender"   "recencyReceiveReceiver"
#> [43] "userStat"              

# List of available effects for undirected networks
tie_effects(directed = FALSE)
#>  [1] "baseline"        "FEtype"          "tie"             "same"           
#>  [5] "difference"      "average"         "minimum"         "maximum"        
#>  [9] "event"           "inertia"         "totaldegreeDyad" "degreeMin"      
#> [13] "degreeMax"       "degreeDiff"      "sp"              "psABAY"         
#> [17] "psABAB"          "recencyContinue" "userStat"       

# List of available endogenous effects for undirected networks
tie_effects(directed = FALSE, endogenous = TRUE)
#>  [1] "baseline"        "FEtype"          "inertia"         "totaldegreeDyad"
#>  [5] "degreeMin"       "degreeMax"       "degreeDiff"      "sp"             
#>  [9] "psABAY"          "psABAB"          "recencyContinue"
```
