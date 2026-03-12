# actor_effects

Overview of statistics in the actor-oriented model, see Details.

## Usage

``` r
actor_effects(step = NULL)
```

## Arguments

- step:

  outputs all statistics in the sender activity step (if \`step =
  sender\`) or receiver choice step (if \`step = receiver\`).

## Value

Returns a list of available effects and their corresponding statistics
based on the specified \`step\` (sender or receiver).

## Details

Overview of statistics in the actor-oriented model.

A list of available effects and their corresponding statistics for the
*sender activity rate* step:

- [`baseline()`](https://tilburgnetworkgroup.github.io/remstats/reference/baseline.md)

- [`send()`](https://tilburgnetworkgroup.github.io/remstats/reference/send.md)

- [`indegreeSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/indegreeSender.md)

- [`outdegreeSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/outdegreeSender.md)

- [`totaldegreeSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeSender.md)

- [`recencySendSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencySendSender.md)

- [`recencyReceiveSender()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyReceiveSender.md)

- [`psABA()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABA.md)

- [`psABB()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABB.md)

- [`psABX()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABX.md)

A list of available effects and their corresponding statistics for the
*receiver choice* step:

- [`receive()`](https://tilburgnetworkgroup.github.io/remstats/reference/receive.md)

- [`tie()`](https://tilburgnetworkgroup.github.io/remstats/reference/tie.md)

- [`same()`](https://tilburgnetworkgroup.github.io/remstats/reference/same.md)

- [`difference()`](https://tilburgnetworkgroup.github.io/remstats/reference/difference.md)

- [`average()`](https://tilburgnetworkgroup.github.io/remstats/reference/average.md)

- [`indegreeReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/indegreeReceiver.md)

- [`outdegreeReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/outdegreeReceiver.md)

- [`totaldegreeReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeReceiver.md)

- [`inertia()`](https://tilburgnetworkgroup.github.io/remstats/reference/inertia.md)

- [`reciprocity()`](https://tilburgnetworkgroup.github.io/remstats/reference/reciprocity.md)

- [`otp()`](https://tilburgnetworkgroup.github.io/remstats/reference/otp.md)

- [`itp()`](https://tilburgnetworkgroup.github.io/remstats/reference/itp.md)

- [`osp()`](https://tilburgnetworkgroup.github.io/remstats/reference/osp.md)

- [`isp()`](https://tilburgnetworkgroup.github.io/remstats/reference/isp.md)

- [`rrankSend()`](https://tilburgnetworkgroup.github.io/remstats/reference/rrankSend.md)

- [`rrankReceive()`](https://tilburgnetworkgroup.github.io/remstats/reference/rrankReceive.md)

- [`recencySendReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencySendReceiver.md)

- [`recencyReceiveReceiver()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyReceiveReceiver.md)

- [`recencyContinue()`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyContinue.md)

- [`psABAB()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABAB.md)

- [`psABBA()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBA.md)

- [`psABXA()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXA.md)

- [`psABXB()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXB.md)

- [`psABAY()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABAY.md)

- [`psABBY()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBY.md)

- [`psABXY()`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXY.md)

## Examples

``` r
# List of available effects for both the sender and receiver step
actor_effects()
#> $sender
#>  [1] "baseline"             "send"                 "indegreeSender"      
#>  [4] "outdegreeSender"      "totaldegreeSender"    "recencySendSender"   
#>  [7] "recencyReceiveSender" "psABA"                "psABB"               
#> [10] "psABX"                "userStat"            
#> 
#> $receiver
#>  [1] "receive"                "same"                   "difference"            
#>  [4] "average"                "tie"                    "inertia"               
#>  [7] "reciprocity"            "indegreeReceiver"       "outdegreeReceiver"     
#> [10] "totaldegreeReceiver"    "otp"                    "itp"                   
#> [13] "osp"                    "isp"                    "rrankSend"             
#> [16] "rrankReceive"           "recencySendReceiver"    "recencyReceiveReceiver"
#> [19] "recencyContinue"        "psABBA"                 "psABAB"                
#> [22] "psABXB"                 "psABXA"                 "psABAY"                
#> [25] "psABBY"                 "psABXY"                 "userStat"              
#> 

# List of available effects for the sender step
actor_effects(step = "sender")
#>  [1] "baseline"             "send"                 "indegreeSender"      
#>  [4] "outdegreeSender"      "totaldegreeSender"    "recencySendSender"   
#>  [7] "recencyReceiveSender" "psABA"                "psABB"               
#> [10] "psABX"                "userStat"            

# List of available effects for the receiver step
actor_effects(step = "receiver")
#>  [1] "receive"                "same"                   "difference"            
#>  [4] "average"                "tie"                    "inertia"               
#>  [7] "reciprocity"            "indegreeReceiver"       "outdegreeReceiver"     
#> [10] "totaldegreeReceiver"    "otp"                    "itp"                   
#> [13] "osp"                    "isp"                    "rrankSend"             
#> [16] "rrankReceive"           "recencySendReceiver"    "recencyReceiveReceiver"
#> [19] "recencyContinue"        "psABBA"                 "psABAB"                
#> [22] "psABXB"                 "psABXA"                 "psABAY"                
#> [25] "psABBY"                 "psABXY"                 "userStat"              
```
