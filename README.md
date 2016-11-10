## CH/OTP Test Task

### Algorithm

When nodes are started they detect each other and form a directed 
ring graph. Each node generate messages and remember them in its cache 
and send to its right neighbour(next node on ring). When node receives 
message from left node (previous node on ring) it checks such message 
exist in its cache, if not then retranslate(pass) further to next node, 
otherwise do not pass it further. When some additional node starts all 
nodes recompute its right neighbour and if it is changed then all cached 
messages passed to the right neighbour (next node).
 
### Node Architecture

Node have some specific internal processes residing in its dedicated module:
 * Peer Monitor -- discovers nodes and computes right neighbour (next node) 
   and sends it to Message Retranslator process (see bellow)
 * Message Generator -- generates random messages and passes them to Message 
   Retranslator process (see bellow)
 * Message Retranslator -- retranslates(passes) messages to next node if 
   they are not in cache. Sends newly generated messages (received from 
   Message Generator) and caches them. When new next neighbour received 
   from Peer Monitor it sends all cached messages to it.
     
### TODO

There is a problem with findPeers in a PeerMonitor, it does not discover 
node down, hence come additional ping-pong protocol/mechanism required to 
accurately detect nodes down event.

### Run Nodes

Currently nodes started manually from shell:

```
$ stack exec chtt -- --host "127.0.0.1" --port "1234" --send-for 30 --wait-for 5 --with-seed 0
$ stack exec chtt -- --host "127.0.0.1" --port "1235" --send-for 30 --wait-for 5 --with-seed 0
$ stack exec chtt -- --host "127.0.0.1" --port "1236" --send-for 30 --wait-for 5 --with-seed 0
$ stack exec chtt -- --host "127.0.0.1" --port "1237" --send-for 30 --wait-for 5 --with-seed 0
```

It is required to clarify `2.3 Cluster configuration` how to launch 
cluster in a more automatic fashion.